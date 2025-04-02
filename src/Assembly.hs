{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Assembly (
    -- Types
    Address,
    Label,
    ProgramCounter,
    AddressRef(..),
    Operand(..),
    pattern Imm,
    pattern AbsLit,
    pattern AbsLabel,
    pattern AbsXLit,
    pattern AbsXLabel,
    pattern ZPLabel,
    zpLit,
    BranchMnemonic(..),
    Mnemonic(..),
    SymbolicInstruction(..),
    AsmState(..),
    initialAsmState,
    Asm(..),
    runAssembler,

    -- eDSL Functions
    l_,
    db,
    dw,
    string,
    lda, sta, ldx, ldy, jmp, inx, rts, adc, sbc, tax, tay, stx, sty, cmp, cpx, cpy, txa, tya, txs, tsx,
    bne, beq, bcs, bcc, bmi, bpl, bvs, bvc,

    -- Macros
    ifNotEqThen,
    ifEqThen,
    ifCarryThen,
    ifNotCarryThen,
    whileEqDo,
    whileNotEqDo,
    whileCarryDo,
    whileNotCarryDo,

    -- List Helpers
    asc,
    createList,
    listAdd,
    listForEach,
    listCopy,
    stringAsList,

    -- Binary Generation (Internal Helpers - potentially hide later)
    generateBinary,
    formatHexBytes -- Keep formatting here for now
) where

import Control.Monad.State.Strict
import Control.Monad (when)
import Data.Word
import Data.Int (Int8)
import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Data.Char (ord)
import Data.Maybe (fromMaybe, mapMaybe)
import Numeric (showHex)
import GHC.Arr (cmpArray) -- Used implicitly? Keep for now.
import GHC.Read (list) -- Used implicitly? Keep for now.
import qualified Data.Foldable as String -- For stringAsList length

-- --- Types ---
type Address = Word16
type Label = String
type ProgramCounter = Word16
data AddressRef = AddrLit16 Word16 | AddrLabel Label deriving (Show, Eq)
data Operand = OpImm Word8 | OpAbs AddressRef | OpAbsX AddressRef | OpAbsY AddressRef | OpZP AddressRef | OpZPX AddressRef | OpZPY AddressRef | OpIndX AddressRef | OpIndY AddressRef deriving (Show, Eq)
pattern Imm v = OpImm v; pattern AbsLit v = OpAbs (AddrLit16 v); pattern AbsLabel l = OpAbs (AddrLabel l); pattern AbsXLit v = OpAbsX (AddrLit16 v); pattern AbsXLabel l = OpAbsX (AddrLabel l); pattern ZPLabel l = OpZP (AddrLabel l)
zpLit :: Word8 -> Operand; zpLit v = OpZP (AddrLit16 (fromIntegral v))

data BranchMnemonic = BNE | BEQ | BCS | BCC | BMI | BPL | BVS | BVC deriving (Show, Eq, Ord, Enum, Bounded)

data Mnemonic = LDA | STA | LDX | LDY | JMP | INX | RTS | ADC | SBC | TAX | TAY | STX | STY | CMP | CPX | CPY |
                TXA | TYA | TXS | TSX
                deriving (Show, Eq, Ord, Enum, Bounded)

data SymbolicInstruction = S_LabelDef Label | S_Ins Mnemonic (Maybe Operand) | S_Branch BranchMnemonic Label | S_Bytes [Word8] | S_Words [Word16] deriving (Show, Eq)

-- --- Assembler State ---
data AsmState = AsmState
  { asmPC            :: ProgramCounter
  , asmLabels        :: Map.Map Label ProgramCounter
  , asmCode          :: [(ProgramCounter, SymbolicInstruction)]
  , asmMacroCounter  :: Int
  } deriving (Show)

initialAsmState :: ProgramCounter -> AsmState
initialAsmState startAddr = AsmState startAddr Map.empty [] 0

-- --- Assembler Monad ---
newtype Asm a = Asm { unAsm :: State AsmState a }
  deriving (Functor, Applicative, Monad, MonadState AsmState)

-- --- Helper Functions in Monad ---

getInstructionSize :: Mnemonic -> Maybe Operand -> Either String Word16
getInstructionSize m (Just op) = case (m, op) of
    (LDA, OpImm _)  -> Right 2; (LDA, OpZP _)   -> Right 2; (LDA, OpZPX _)  -> Right 2
    (LDA, OpAbs _)  -> Right 3; (LDA, OpAbsX _) -> Right 3; (LDA, OpAbsY _) -> Right 3
    (LDA, OpIndX _) -> Right 2; (LDA, OpIndY _) -> Right 2
    (STA, OpZP _)   -> Right 2; (STA, OpZPX _)  -> Right 2
    (STA, OpAbs _)  -> Right 3; (STA, OpAbsX _) -> Right 3; (STA, OpAbsY _) -> Right 3
    (STA, OpIndX _) -> Right 2; (STA, OpIndY _) -> Right 2
    (LDX, OpImm _)  -> Right 2; (LDX, OpZP _)   -> Right 2; (LDX, OpZPX _)  -> Right 2 -- Było ZPY, corrected to ZPX? Check 6502 docs. Assuming ZPX for now.
    (LDX, OpAbs _)  -> Right 3; (LDX, OpAbsY _) -> Right 3
    (LDY, OpImm _)  -> Right 2; (LDY, OpZP _)   -> Right 2; (LDY, OpZPX _)  -> Right 2 -- Było ZPY, corrected to ZPX? Check 6502 docs. Assuming ZPX for now.
    (LDY, OpAbs _)  -> Right 3; (LDY, OpAbsX _) -> Right 3
    (STX, OpZP _)   -> Right 2; (STX, OpZPY _)  -> Right 2; (STX, OpAbs _)  -> Right 3
    (STY, OpZP _)   -> Right 2; (STY, OpZPX _)  -> Right 2; (STY, OpAbs _)  -> Right 3
    (CPX, OpImm _)  -> Right 2; (CPX, OpZP _)   -> Right 2; (CPX, OpAbs _)  -> Right 3
    (CPY, OpImm _)  -> Right 2; (CPY, OpZP _)   -> Right 2; (CPY, OpAbs _)  -> Right 3
    (CMP, OpImm _)  -> Right 2; (CMP, OpZP _)   -> Right 2; (CMP, OpZPX _)  -> Right 2
    (CMP, OpAbs _)  -> Right 3; (CMP, OpAbsX _) -> Right 3; (CMP, OpAbsY _) -> Right 3
    (CMP, OpIndX _) -> Right 2; (CMP, OpIndY _) -> Right 2
    (ADC, OpImm _)  -> Right 2; (ADC, OpZP _)   -> Right 2; (ADC, OpZPX _)  -> Right 2
    (ADC, OpAbs _)  -> Right 3; (ADC, OpAbsX _) -> Right 3; (ADC, OpAbsY _) -> Right 3
    (ADC, OpIndX _) -> Right 2; (ADC, OpIndY _) -> Right 2
    (SBC, OpImm _)  -> Right 2; (SBC, OpZP _)   -> Right 2; (SBC, OpZPX _)  -> Right 2
    (SBC, OpAbs _)  -> Right 3; (SBC, OpAbsX _) -> Right 3; (SBC, OpAbsY _) -> Right 3
    (SBC, OpIndX _) -> Right 2; (SBC, OpIndY _) -> Right 2
    (JMP, OpAbs _)  -> Right 3
    -- TODO: Add other instructions (JMP Indirect, etc.)
    (_, _) -> Left $ "Unsupported/TODO addressing mode " ++ show op ++ " for " ++ show m
getInstructionSize m Nothing = case m of
    (INX) -> Right 1; (RTS) -> Right 1; (TAX) -> Right 1; (TAY) -> Right 1
    (TXA) -> Right 1; (TYA) -> Right 1; (TXS) -> Right 1; (TSX) -> Right 1
    -- TODO: Add other implied instructions (INY, DEX, DEY, CLC, SEC, CLI, SEI, CLV, NOP, BRK, PHP, PLP, PHA, PLA)
    _     -> Left $ "Mnemonic " ++ show m ++ " requires an operand or is not a known implied instruction (or is TODO)."

emitGeneric :: SymbolicInstruction -> Either String Word16 -> Asm ()
emitGeneric _ (Left err) = error $ "Assembly Error (emitGeneric): " ++ err -- Consider using Either in Asm or a dedicated error type
emitGeneric instruction (Right size) = do
  pc <- gets asmPC
  modify' $ \s -> s { asmCode = (pc, instruction) : asmCode s, asmPC = pc + size }

emitIns :: Mnemonic -> Operand -> Asm (); emitIns m op = emitGeneric (S_Ins m (Just op)) (getInstructionSize m (Just op))
emitImplied :: Mnemonic -> Asm (); emitImplied m = emitGeneric (S_Ins m Nothing) (getInstructionSize m Nothing)
emitBranch :: BranchMnemonic -> Label -> Asm (); emitBranch m l = emitGeneric (S_Branch m l) (Right 2)

l_ :: Label -> Asm ()
l_ lbl = do pc <- gets asmPC; labels <- gets asmLabels
            when (Map.member lbl labels) $ error $ "Label redefined: " ++ lbl -- Consider Either
            modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
            emitGeneric (S_LabelDef lbl) (Right 0)

db :: [Word8] -> Asm (); db bs = let size = fromIntegral $ length bs in when (size > 0) $ emitGeneric (S_Bytes bs) (Right size)
dw :: [Word16] -> Asm (); dw ws = let size = fromIntegral (length ws) * 2 in when (size > 0) $ emitGeneric (S_Words ws) (Right size)
string :: String -> Asm (); string str = let bytes = map (fromIntegral . ord) str; size = fromIntegral $ length bytes in when (size > 0) $ emitGeneric (S_Bytes bytes) (Right size)

makeUniqueLabel :: () -> Asm Label
makeUniqueLabel _ = do
    count <- gets asmMacroCounter
    modify' $ \s -> s { asmMacroCounter = count + 1 }
    return $ "_lbl_" ++ show count

-- --- eDSL Functions ---
lda :: Operand -> Asm (); lda = emitIns LDA
sta :: Operand -> Asm (); sta = emitIns STA
ldx :: Operand -> Asm (); ldx = emitIns LDX
ldy :: Operand -> Asm (); ldy = emitIns LDY
jmp :: Operand -> Asm (); jmp = emitIns JMP
inx :: Asm (); inx = emitImplied INX
rts :: Asm (); rts = emitImplied RTS
adc :: Operand -> Asm (); adc = emitIns ADC
sbc :: Operand -> Asm (); sbc = emitIns SBC
tax :: Asm (); tax = emitImplied TAX
tay :: Asm (); tay = emitImplied TAY
txa :: Asm (); txa = emitImplied TXA
tya :: Asm (); tya = emitImplied TYA
txs :: Asm (); txs = emitImplied TXS
tsx :: Asm (); tsx = emitImplied TSX
stx :: Operand -> Asm (); stx = emitIns STX
sty :: Operand -> Asm (); sty = emitIns STY
cmp :: Operand -> Asm (); cmp = emitIns CMP
cpx :: Operand -> Asm (); cpx = emitIns CPX
cpy :: Operand -> Asm (); cpy = emitIns CPY

bne :: Label -> Asm (); bne = emitBranch BNE
beq :: Label -> Asm (); beq = emitBranch BEQ
bcs :: Label -> Asm (); bcs = emitBranch BCS
bcc :: Label -> Asm (); bcc = emitBranch BCC
bmi :: Label -> Asm (); bmi = emitBranch BMI
bpl :: Label -> Asm (); bpl = emitBranch BPL
bvs :: Label -> Asm (); bvs = emitBranch BVS
bvc :: Label -> Asm (); bvc = emitBranch BVC

-- --- Macros ---
ifNotEqThen :: Asm () -> Asm ()
ifNotEqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel
    thenBlock
    l_ endLabel

ifEqThen :: Asm () -> Asm ()
ifEqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bne endLabel -- Corrected: branch if NOT equal to skip
    thenBlock
    l_ endLabel

ifCarryThen :: Asm () -> Asm ()
ifCarryThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcc endLabel
    thenBlock
    l_ endLabel

ifNotCarryThen :: Asm () -> Asm ()
ifNotCarryThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcs endLabel
    thenBlock
    l_ endLabel

whileEqDo :: Asm () -> Asm () -> Asm ()
whileEqDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bne endLabel -- Branch if Not Equal (exit condition)
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileNotEqDo :: Asm () -> Asm () -> Asm ()
whileNotEqDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    beq endLabel -- Branch if Equal (exit condition)
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileCarryDo :: Asm () -> Asm () -> Asm ()
whileCarryDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcc endLabel -- Branch if Carry Clear (exit condition)
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileNotCarryDo :: Asm () -> Asm () -> Asm ()
whileNotCarryDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcs endLabel -- Branch if Carry Set (exit condition)
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

-- --- List Helpers ---
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
    loopLabel <- makeUniqueLabel ()
    endLabel  <- makeUniqueLabel ()
    l_ loopLabel
    cpx listLenAddr        -- Compare index (X) with length
    beq endLabel           -- If equal, we're done
    inx                    -- Increment index for 1-based access (element 1 is at base+1)
    let currentElementAddr = OpAbsX l -- Address is base + X
    action currentElementAddr -- Perform action with the *address* of the element
    jmp $ AbsLabel loopLabel
    l_ endLabel

listCopy :: AddressRef -> AddressRef -> Asm ()
listCopy src dst = do
    let srcLenAddr = OpAbs src
    let dstLenAddr = OpAbs dst
    -- Initialize destination length to 0
    lda $ Imm 0
    sta dstLenAddr
    -- Loop using listForEach structure
    ldx $ Imm 0x00
    copyLoopLabel <- makeUniqueLabel ()
    endCopyLabel  <- makeUniqueLabel ()
    l_ copyLoopLabel
    cpx srcLenAddr         -- Compare index (X) with source length
    beq endCopyLabel       -- If equal, done copying elements
    inx                    -- Increment index (1-based for element access)
    lda $ OpAbsX src       -- Load element from src[X]
    sta $ OpAbsX dst       -- Store element to dst[X]
    txa                    -- Get current index (which is the new length)
    sta dstLenAddr         -- Update destination length
    jmp $ AbsLabel copyLoopLabel
    l_ endCopyLabel

stringAsList :: String -> Asm ()
stringAsList s = do
    let bytes = map (fromIntegral . ord) s
    db [fromIntegral $ length bytes] -- Store length byte first
    db bytes                         -- Store character bytes

-- --- Binary Generation (Pass 2) ---
generateBinary :: AsmState -> Either String [Word8]
generateBinary finalState = foldl' processInstruction (Right []) (reverse $ asmCode finalState)
  where labels = asmLabels finalState
        processInstruction :: Either String [Word8] -> (ProgramCounter, SymbolicInstruction) -> Either String [Word8]
        processInstruction (Left err) _ = Left err
        processInstruction (Right currentBytes) (pc, instruction) =
            case instructionBytes pc instruction of
                Left err      -> Left err
                Right newBytes -> Right (currentBytes ++ newBytes)

        instructionBytes :: ProgramCounter -> SymbolicInstruction -> Either String [Word8]
        instructionBytes _ (S_LabelDef _) = Right []
        instructionBytes _ (S_Bytes bs) = Right bs
        instructionBytes _ (S_Words ws) = Right $ concatMap wordToBytesLE ws
        instructionBytes pc (S_Branch bm targetLabel) =
            calculateOffset pc targetLabel >>= \offset -> Right [branchOpcode bm, fromIntegral (offset :: Int8)]
        instructionBytes _ (S_Ins m Nothing) = maybe (Left $ errImp m) Right (impliedOpcode m)
        instructionBytes _ (S_Ins m (Just op)) = maybe (Left $ errComb m op) Right (operandOpcode m op)

        errImp m = "Pass 2 Error: " ++ show m ++ " requires an operand or is not a known implied instruction."
        errComb m op = "Pass 2 Error: Unsupported instruction/operand combination: " ++ show m ++ " " ++ show op

        impliedOpcode :: Mnemonic -> Maybe [Word8]
        impliedOpcode = \case INX -> Just [0xE8]; RTS -> Just [0x60]; TAX -> Just [0xAA]; TAY -> Just [0xA8]
                              TXA -> Just [0x8A]; TYA -> Just [0x98]; TXS -> Just [0x9A]; TSX -> Just [0xBA]; _ -> Nothing -- TODO: Add others

        branchOpcode :: BranchMnemonic -> Word8
        branchOpcode = \case BNE -> 0xD0; BEQ -> 0xF0; BCS -> 0xB0; BCC -> 0x90; BMI -> 0x30; BPL -> 0x10; BVS -> 0x70; BVC -> 0x50

        operandOpcode :: Mnemonic -> Operand -> Maybe [Word8]
        operandOpcode m o = resolveAddressMaybe (getOperandAddressRef o) >>= \resolvedAddr ->
            case (m, o) of
                (LDA, OpImm v)  -> Just [0xA9, v]
                (LDA, OpZP _)   -> Just [0xA5, lo resolvedAddr]
                (LDA, OpZPX _)  -> Just [0xB5, lo resolvedAddr]
                (LDA, OpAbs _)  -> Just [0xAD, lo resolvedAddr, hi resolvedAddr]
                (LDA, OpAbsX _) -> Just [0xBD, lo resolvedAddr, hi resolvedAddr]
                (LDA, OpAbsY _) -> Just [0xB9, lo resolvedAddr, hi resolvedAddr]
                (LDA, OpIndX _) -> Just [0xA1, lo resolvedAddr]
                (LDA, OpIndY _) -> Just [0xB1, lo resolvedAddr]

                (STA, OpZP _)   -> Just [0x85, lo resolvedAddr]
                (STA, OpZPX _)  -> Just [0x95, lo resolvedAddr]
                (STA, OpAbs _)  -> Just [0x8D, lo resolvedAddr, hi resolvedAddr]
                (STA, OpAbsX _) -> Just [0x9D, lo resolvedAddr, hi resolvedAddr]
                (STA, OpAbsY _) -> Just [0x99, lo resolvedAddr, hi resolvedAddr]
                (STA, OpIndX _) -> Just [0x81, lo resolvedAddr]
                (STA, OpIndY _) -> Just [0x91, lo resolvedAddr]

                (LDX, OpImm v)  -> Just [0xA2, v]
                (LDX, OpZP _)   -> Just [0xA6, lo resolvedAddr]
                (LDX, OpZPY _)  -> Just [0xB6, lo resolvedAddr] -- ZPY exists for LDX
                (LDX, OpAbs _)  -> Just [0xAE, lo resolvedAddr, hi resolvedAddr]
                (LDX, OpAbsY _) -> Just [0xBE, lo resolvedAddr, hi resolvedAddr] -- AbsY exists for LDX

                (LDY, OpImm v)  -> Just [0xA0, v]
                (LDY, OpZP _)   -> Just [0xA4, lo resolvedAddr]
                (LDY, OpZPX _)  -> Just [0xB4, lo resolvedAddr] -- ZPX exists for LDY
                (LDY, OpAbs _)  -> Just [0xAC, lo resolvedAddr, hi resolvedAddr]
                (LDY, OpAbsX _) -> Just [0xBC, lo resolvedAddr, hi resolvedAddr] -- AbsX exists for LDY

                (STX, OpZP _)   -> Just [0x86, lo resolvedAddr]
                (STX, OpZPY _)  -> Just [0x96, lo resolvedAddr]
                (STX, OpAbs _)  -> Just [0x8E, lo resolvedAddr, hi resolvedAddr]

                (STY, OpZP _)   -> Just [0x84, lo resolvedAddr]
                (STY, OpZPX _)  -> Just [0x94, lo resolvedAddr]
                (STY, OpAbs _)  -> Just [0x8C, lo resolvedAddr, hi resolvedAddr]

                (CMP, OpImm v)  -> Just [0xC9, v]
                (CMP, OpZP _)   -> Just [0xC5, lo resolvedAddr]
                (CMP, OpZPX _)  -> Just [0xD5, lo resolvedAddr]
                (CMP, OpAbs _)  -> Just [0xCD, lo resolvedAddr, hi resolvedAddr]
                (CMP, OpAbsX _) -> Just [0xDD, lo resolvedAddr, hi resolvedAddr]
                (CMP, OpAbsY _) -> Just [0xD9, lo resolvedAddr, hi resolvedAddr]
                (CMP, OpIndX _) -> Just [0xC1, lo resolvedAddr]
                (CMP, OpIndY _) -> Just [0xD1, lo resolvedAddr]

                (CPX, OpImm v)  -> Just [0xE0, v]
                (CPX, OpZP _)   -> Just [0xE4, lo resolvedAddr]
                (CPX, OpAbs _)  -> Just [0xEC, lo resolvedAddr, hi resolvedAddr]

                (CPY, OpImm v)  -> Just [0xC0, v]
                (CPY, OpZP _)   -> Just [0xC4, lo resolvedAddr]
                (CPY, OpAbs _)  -> Just [0xCC, lo resolvedAddr, hi resolvedAddr]

                (ADC, OpImm v)  -> Just [0x69, v]
                (ADC, OpZP _)   -> Just [0x65, lo resolvedAddr]
                (ADC, OpZPX _)  -> Just [0x75, lo resolvedAddr]
                (ADC, OpAbs _)  -> Just [0x6D, lo resolvedAddr, hi resolvedAddr]
                (ADC, OpAbsX _) -> Just [0x7D, lo resolvedAddr, hi resolvedAddr]
                (ADC, OpAbsY _) -> Just [0x79, lo resolvedAddr, hi resolvedAddr]
                (ADC, OpIndX _) -> Just [0x61, lo resolvedAddr]
                (ADC, OpIndY _) -> Just [0x71, lo resolvedAddr]

                (SBC, OpImm v)  -> Just [0xE9, v]
                (SBC, OpZP _)   -> Just [0xE5, lo resolvedAddr]
                (SBC, OpZPX _)  -> Just [0xF5, lo resolvedAddr]
                (SBC, OpAbs _)  -> Just [0xED, lo resolvedAddr, hi resolvedAddr]
                (SBC, OpAbsX _) -> Just [0xFD, lo resolvedAddr, hi resolvedAddr]
                (SBC, OpAbsY _) -> Just [0xF9, lo resolvedAddr, hi resolvedAddr]
                (SBC, OpIndX _) -> Just [0xE1, lo resolvedAddr]
                (SBC, OpIndY _) -> Just [0xF1, lo resolvedAddr]

                (JMP, OpAbs _)  -> Just [0x4C, lo resolvedAddr, hi resolvedAddr]
                -- TODO: JMP Indirect [0x6C, lo, hi]

                _ -> Nothing -- Combination not supported or requires specific handling

        getOperandAddressRef :: Operand -> Maybe AddressRef
        getOperandAddressRef = \case
            OpImm _ -> Nothing -- Immediate doesn't have an address ref
            OpAbs r -> Just r; OpAbsX r -> Just r; OpAbsY r -> Just r
            OpZP r -> Just r; OpZPX r -> Just r; OpZPY r -> Just r
            OpIndX r -> Just r; OpIndY r -> Just r

        resolveAddressMaybe :: Maybe AddressRef -> Maybe Word16
        resolveAddressMaybe Nothing = Just 0 -- For Imm case, value doesn't matter here
        resolveAddressMaybe (Just (AddrLit16 l)) = Just l
        resolveAddressMaybe (Just (AddrLabel l)) = Map.lookup l labels

        -- This function still returns Either for branch offset calculation
        calculateOffset :: ProgramCounter -> Label -> Either String Int8
        calculateOffset pc targetLabel =
            case Map.lookup targetLabel labels of
                Nothing -> Left $ unknownLbl targetLabel
                Just targetAddr ->
                    let offset = fromIntegral targetAddr - fromIntegral (pc + 2) -- Offset relative to the *next* instruction
                    in if offset >= -128 && offset <= 127
                       then Right (fromIntegral offset)
                       else Left (branchRange targetLabel pc offset)
        unknownLbl l = "Pass 2 Error: Unknown label in branch '" ++ l ++ "'"
        branchRange l pc o = "Pass 2 Error: Branch target out of range for '" ++ l ++ "' at PC " ++ hx pc ++ " (offset=" ++ show o ++ ")"

        lo :: Word16 -> Word8; lo = fromIntegral . (.&. 0xFF)
        hi :: Word16 -> Word8; hi w = fromIntegral (w `shiftR` 8)
        hx :: Word16 -> String; hx a = showHex a ""
        wordToBytesLE :: Word16 -> [Word8]; wordToBytesLE w = [lo w, hi w]

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
