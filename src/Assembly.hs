
module Assembly (
    -- Re-export Core Types/State/Monad needed by users
    module Assembly.Core,

    -- Higher-level functions
    runAssembler,
    generateBinary,
    formatHexBytes,

    -- List Helpers
    asc,
    listCreate,
    listCreate_,
    listAdd,
    listForEach,
    listCopy,
    listFromString
) where

import Assembly.Core -- Teraz importuje wszystko, w tym nowe typy i tabelę
import Control.Monad.State.Strict (execState)
import Data.Word (Word8, Word16)
import Data.Int (Int8)
import Data.Bits ((.&.), shiftR)
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Data.Char (ord)
import Data.Maybe (fromMaybe, mapMaybe)
import Numeric (showHex)
import qualified Data.Foldable as String -- Alias dla String.length

-- --- List Helpers (bez zmian, ale używają zaktualizowanych aliasów z Core) ---
asc :: Char -> Word8
asc = fromIntegral . fromEnum


-- Listy są indeksowande od 1! 
-- Listy są przechowywane w pamięci jako:
-- [length, element1, element2, ..., elementN]
-- length - długość listy
-- element1 - pierwszy element listy..
-- wynika z tego że:
--   listBase + 0 to długość listy
--   listBase + 1 to pierwszy element
--   listBase + 2 to drugi element
--   ...

listCreate_ :: AddressRef -> Asm ()
listCreate_ addr = do
    lda $ Imm 0x00
    sta $ OpAbs addr -- Initializes list in absolute memory

-- Modified listCreate to return AddressRef within the Asm monad
listCreate :: String -> Asm AddressRef
listCreate s = do
    let sAddr = AddrLabel s
    listCreate_ sAddr
    return sAddr

listAdd :: AddressRef -> Word8 -> Asm ()
listAdd l element = do
  let listBaseAbs = OpAbs l
  let listBaseAbsX = OpAbsX l
  lda listBaseAbs
  tax -- długość -> X
  inx -- X++
  stx listBaseAbs -- X -> długość
  lda $ Imm element
  sta listBaseAbsX -- Zapisz element na list_base + X

listForEach :: AddressRef -> (Operand -> Asm ()) -> Asm ()
listForEach l action = do
    let listLenAddr = OpAbs l
    ldx $ Imm 0x00         -- Start index at 0
    loopLabel <- makeUniqueLabel () -- Uses makeUniqueLabel from Core (via Macros import)
    endLabel  <- makeUniqueLabel ()
    l_ loopLabel             -- Uses l_ from Core
    cpx listLenAddr          -- Uses cpx alias defined above
    beq endLabel             -- Uses beq from Core, jumps to endLabel if X == length, for empty list X == length == 0!
    inx                      -- X indexed from 1!
    action $ OpAbsX l        -- Perform action with the *address* of the element
    jmp $ AbsLabel loopLabel -- Uses jmp alias defined above
    l_ endLabel


listCopy :: AddressRef -> AddressRef -> Asm ()
listCopy src dst = do
    let srcAbs = OpAbs src
    let dstAbs = OpAbs dst
    lda $ Imm 0  -- Inicjalizuj długość listy docelowej
    sta dstAbs
    -- Pętla kopiująca
    ldx $ Imm 0
    copyLoopLabel <- makeUniqueLabel ()
    endCopyLabel  <- makeUniqueLabel ()
    l_ copyLoopLabel
    -- lda srcAbs          -- Pobierz długość źródła
    -- cmp $ OpZPX (AddrLit16 0) -- Porównaj z X (używamy ZPX jako hack do porównania z X, wymaga to aby $00 nie był używany lub można użyć CPX)
    cpx srcAbs
    beq endCopyLabel    -- Jeśli X == długość źródła, koniec
    inx                 -- Zwiększ X (nowa potencjalna długość)
    lda $ OpAbsX src         -- Załaduj bajt ze źródła[X]
    sta $ OpAbsX dst         -- Zapisz bajt w docelowym[X]
    txa                 -- Przenieś X do A
    sta dstAbs          -- Zapisz nową długość w docelowym
    jmp $ AbsLabel copyLoopLabel
    l_ endCopyLabel


listFromString :: String -> Asm ()
listFromString s = do
    let bytes = map (fromIntegral . ord) s
    db [fromIntegral $ String.length s] -- Długość
    db bytes                         -- Bajty

-- --- Binary Generation (Pass 2) --- ZAKTUALIZOWANA

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
                    let offset = fromIntegral targetAddr - fromIntegral (pc + 2)
                    in if offset >= -128 && offset <= 127
                       then Right (fromIntegral offset)
                       else Left (branchRange targetLabel pc offset)

        processInstruction :: Either String [Word8] -> (ProgramCounter, SymbolicInstruction) -> Either String [Word8]
        processInstruction (Left err) _ = Left err
        processInstruction (Right currentBytes) (pc, instruction) =
            case instructionBytes pc instruction of
                Left err       -> Left err
                Right newBytes -> Right (currentBytes ++ newBytes)

        -- ZAKTUALIZOWANA: Używa generateInstructionBytes z Core
        instructionBytes :: ProgramCounter -> SymbolicInstruction -> Either String [Word8]
        instructionBytes _ (SLabelDef _) = Right []
        instructionBytes _ (SBytes bs) = Right bs
        instructionBytes _ (SWords ws) = Right $ concatMap wordToBytesLE ws
        instructionBytes pc (SBranch bm targetLabel) =
            calculateOffset pc targetLabel >>= \offset -> Right [branchOpcode bm, fromIntegral (offset :: Int8)]
        instructionBytes _ (SIns m maybeOp) =
            -- Używamy teraz funkcji pomocniczej zdefiniowanej w Core.hs
            -- która korzysta z instructionTable
            generateInstructionBytes m maybeOp finalState


-- --- Assembler Runner (bez zmian) ---
runAssembler :: ProgramCounter -> Asm () -> Either String ([Word8], Map.Map Label ProgramCounter)
runAssembler startAddr asmAction =
    let finalState = execState (unAsm asmAction) (initialAsmState startAddr)
    in case generateBinary finalState of
        Left err   -> Left err
        Right code -> Right (code, asmLabels finalState)

-- --- Formatting Utility (bez zmian) ---
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