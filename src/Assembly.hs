{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-} -- Keep patterns for convenience

module Assembly (
    -- Re-export Core Types/State/Monad needed by users
    module Assembly.Core,

    -- Higher-level functions
    runAssembler,
    generateBinary, -- Might hide later
    formatHexBytes, -- Might hide later
    -- eDSL Instruction Aliases are now exported via Assembly.Core

    -- List Helpers (Keep these here as they use eDSL aliases and macros from Core)
    asc,
    createList,
    listAdd,
    listForEach,
    listCopy,
    stringAsList,
) where

-- Import necessary components from Core
import Assembly.Core
-- Other necessary imports for this module's functions
import Control.Monad.State.Strict (execState)
import Data.Word (Word8, Word16)
import Data.Int (Int8)
import Data.Bits ((.&.), shiftR)
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Data.Char (ord)
import Data.Maybe (fromMaybe, mapMaybe) -- Keep mapMaybe if needed by generateBinary
import Numeric (showHex)
import qualified Data.Foldable as String -- For stringAsList length

-- Types, State, Monad, Core Primitives, and eDSL Aliases are now in Assembly.Core

               
-- --- List Helpers --- (These use functions now exported by Assembly.Core)
asc :: Char -> Word8
asc = fromIntegral . fromEnum

createList :: AddressRef -> Asm ()
createList addr = do
    lda $ Imm 0x00
    sta $ OpAbs addr -- Store length 0 at the start of the list area

listAdd :: AddressRef -> Word8 -> Asm ()
listAdd l element = do
  let listLenAddr = OpAbs l
  lda listLenAddr -- Get current length
  tax             -- Put length in X (acts as index for the new element)
  inx             -- Increment index (new length)
  stx listLenAddr -- Store new length
  lda $ Imm element -- Load the element to add
  sta $ OpAbsX l  -- Store element at list_base + X (index = old_length + 1)

listForEach :: AddressRef -> (Operand -> Asm ()) -> Asm () -- Pass Operand for flexibility
listForEach l action = do
    let listLenAddr = OpAbs l
    ldx $ Imm 0x00         -- Start index at 0
    loopLabel <- makeUniqueLabel () -- Uses makeUniqueLabel from Core (via Macros import)
    endLabel  <- makeUniqueLabel ()
    l_ loopLabel           -- Uses l_ from Core
    cpx listLenAddr        -- Uses cpx alias defined above
    beq endLabel           -- Uses beq from Core
    inx                    -- Uses inx alias defined above
    let currentElementAddr = OpAbsX l -- Address is base + X
    action currentElementAddr -- Perform action with the *address* of the element
    jmp $ AbsLabel loopLabel -- Uses jmp alias defined above
    l_ endLabel

listCopy :: AddressRef -> AddressRef -> Asm ()
listCopy src dst = do
    let srcLenAddr = OpAbs src
    let dstLenAddr = OpAbs dst
    -- Initialize destination length to 0
    lda $ Imm 0            -- Uses lda alias
    sta dstLenAddr         -- Uses sta alias
    -- Loop using listForEach structure
    ldx $ Imm 0x00         -- Uses ldx alias
    copyLoopLabel <- makeUniqueLabel () -- Uses makeUniqueLabel from Core
    endCopyLabel  <- makeUniqueLabel ()
    l_ copyLoopLabel       -- Uses l_ from Core
    cpx srcLenAddr         -- Uses cpx alias
    beq endCopyLabel       -- Uses beq from Core
    inx                    -- Uses inx alias
    lda $ OpAbsX src       -- Uses lda alias
    sta $ OpAbsX dst       -- Uses sta alias
    txa                    -- Uses txa alias
    sta dstLenAddr         -- Uses sta alias
    jmp $ AbsLabel copyLoopLabel -- Uses jmp alias
    l_ endCopyLabel        -- Uses l_ from Core

stringAsList :: String -> Asm ()
stringAsList s = do
    let bytes = map (fromIntegral . ord) s
    db [fromIntegral $ String.length s] -- Store length byte first - Use qualified String.length
    db bytes                         -- Uses db from Core

-- --- Binary Generation (Pass 2) --- Remains in this module

-- This function still returns Either for branch offset calculation

unknownLbl l = "Pass 2 Error: Unknown label in branch '" ++ l ++ "'"
branchRange l pc o = "Pass 2 Error: Branch target out of range for '" ++ l ++ "' at PC " ++ hx pc ++ " (offset=" ++ show o ++ ")"


generateBinary :: AsmState -> Either String [Word8]
generateBinary finalState = foldl' processInstruction (Right []) (reverse $ asmCode finalState)
  where labels = asmLabels finalState
        calculateOffset :: ProgramCounter -> Label -> Either String Int8
        calculateOffset pc targetLabel =
            case Map.lookup targetLabel labels of
                Nothing -> Left $ unknownLbl targetLabel
                Just targetAddr ->
                    let offset = fromIntegral targetAddr - fromIntegral (pc + 2) -- Offset relative to the *next* instruction
                    in if offset >= -128 && offset <= 127
                    then Right (fromIntegral offset)
                    else Left (branchRange targetLabel pc offset)
        processInstruction :: Either String [Word8] -> (ProgramCounter, SymbolicInstruction) -> Either String [Word8]
        processInstruction (Left err) _ = Left err
        processInstruction (Right currentBytes) (pc, instruction) =
            case instructionBytes pc instruction of
                Left err      -> Left err
                Right newBytes -> Right (currentBytes ++ newBytes)

        instructionBytes :: ProgramCounter -> SymbolicInstruction -> Either String [Word8]
        instructionBytes _ (SLabelDef _) = Right []
        instructionBytes _ (SBytes bs) = Right bs
        instructionBytes _ (SWords ws) = Right $ concatMap wordToBytesLE ws
        instructionBytes pc (SBranch bm targetLabel) =
            calculateOffset pc targetLabel >>= \offset -> Right [branchOpcode bm, fromIntegral (offset :: Int8)]
        instructionBytes _ (SIns m Nothing) = maybe (Left $ errImp m) Right (impliedOpcode m)
        instructionBytes _ (SIns m (Just op)) = maybe (Left $ errComb m op) Right (operandOpcode m op finalState)

        errImp m = "Pass 2 Error: " ++ show m ++ " requires an operand or is not a known implied instruction."
        errComb m op = "Pass 2 Error: " ++ show m ++ " with operand " ++ show op ++ " is not a known combination."

-- --- Assembler Runner ---
runAssembler :: ProgramCounter -> Asm () -> Either String ([Word8], Map.Map Label ProgramCounter)
runAssembler startAddr asmAction =
    let finalState = execState (unAsm asmAction) (initialAsmState startAddr)
    in case generateBinary finalState of
        Left err   -> Left err
        Right code -> Right (code, asmLabels finalState)

-- --- Formatting Utility ---
formatHexBytes :: Word16 -> [Word8] -> String
formatHexBytes startAddr bytes = unlines $ formatLines 16 (zip [startAddr..] bytes)
  where
    formatLines :: Int -> [(Word16, Word8)] -> [String]
    formatLines _ [] = []
    formatLines n addrBytes =
        let (lineBytes, remainingBytes) = splitAt n addrBytes
            lineAddr = fst $ head lineBytes
            hexValues = unwords $ map (byteToHex . snd) lineBytes
            asciiChars = map (byteToAscii . snd) lineBytes
        in (addrToHex lineAddr ++ ": " ++ padRight (n * 3 - 1) hexValues ++ " ;" ++ asciiChars) : formatLines n remainingBytes

    padRight :: Int -> String -> String
    padRight len str = str ++ replicate (len - length str) ' '

    byteToHex :: Word8 -> String
    byteToHex b = let s = showHex b "" in if length s == 1 then '0':s else s

    addrToHex :: Word16 -> String
    addrToHex a = let s = showHex a "" in replicate (4 - length s) '0' ++ s

    byteToAscii :: Word8 -> Char
    byteToAscii b = let c = toEnum (fromIntegral b) in if b >= 32 && b <= 126 then c else '.'
