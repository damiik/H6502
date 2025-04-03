{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Assembly.Core (
    -- Core Types
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

    -- Core State & Monad
    AsmState(..),
    initialAsmState,
    Asm(..), -- Export the type constructor and field name (unAsm)

    -- Core Primitives
    getInstructionSize,
    emitGeneric,
    emitIns,
    emitImplied,
    emitBranch,
    l_,
    db,
    dw,
    string,
    makeUniqueLabel,

    -- Opcode-related functions
    impliedOpcode,
    branchOpcode,
    operandOpcode,
    getOperandAddressRef,

    -- Miscellaneous
    lo,
    hi,
    hx,
    wordToBytesLE,

    -- eDSL Instruction Aliases
    lda, sta, ldx, ldy, jmp, inx, adc, sbc, tax, tay, stx, sty, cmp, cpx, cpy, txa, tya, txs, tsx,
    bne, beq, bcs, bcc, bmi, bpl, bvs, bvc, jsr, rts,
) where

import Control.Monad.State.Strict
import Control.Monad (when)
import Data.Word
import Data.Int (Int8)
import Data.Bits ((.&.), shiftR)
-- import Data.Bits -- Not used directly here
import qualified Data.Map.Strict as Map
-- import Data.Foldable (foldl') -- Not used directly here
import Data.Char (ord)
-- import Data.Maybe (fromMaybe) -- Not used directly here
import Numeric (showHex) -- Not used directly here

-- --- Types ---
type Address = Word16
type Label = String
type ProgramCounter = Word16
data AddressRef = AddrLit16 Word16 | AddrLabel Label deriving (Show, Eq)
data Operand = OpImm Word8 | OpAbs AddressRef | OpAbsX AddressRef | OpAbsY AddressRef | OpZP AddressRef | OpZPX AddressRef | OpZPY AddressRef | OpIndX AddressRef | OpIndY AddressRef | OpInd AddressRef deriving (Show, Eq)

-- Pattern Synonyms with Signatures
pattern Imm :: Word8 -> Operand
pattern Imm v = OpImm v

pattern Ind :: AddressRef -> Operand  -- Indirect addressing mode opcode ($nnnn)
pattern Ind r = OpInd r

pattern AbsLit :: Word16 -> Operand  -- Absolute addressing mode opcode $nnnn 
pattern AbsLit v = OpAbs (AddrLit16 v)

pattern AbsLabel :: Label -> Operand
pattern AbsLabel l = OpAbs (AddrLabel l)

pattern AbsXLit :: Word16 -> Operand
pattern AbsXLit v = OpAbsX (AddrLit16 v)

pattern AbsXLabel :: Label -> Operand
pattern AbsXLabel l = OpAbsX (AddrLabel l)

pattern ZPLabel :: Label -> Operand
pattern ZPLabel l = OpZP (AddrLabel l)

zpLit :: Word8 -> Operand; zpLit v = OpZP (AddrLit16 (fromIntegral v))

data BranchMnemonic = BNE | BEQ | BCS | BCC | BMI | BPL | BVS | BVC deriving (Show, Eq, Ord, Enum, Bounded)

data Mnemonic = LDA | STA | LDX | LDY | JMP | INX | RTS | ADC | SBC | TAX | TAY | STX | STY | CMP | CPX | CPY |
                TXA | TYA | TXS | TSX | JSR
                deriving (Show, Eq, Ord, Enum, Bounded)

data SymbolicInstruction = SLabelDef Label | SIns Mnemonic (Maybe Operand) | SBranch BranchMnemonic Label | SBytes [Word8] | SWords [Word16] deriving (Show, Eq)

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

-- --- Core Primitive Functions ---
resolveAddressMaybe :: Maybe AddressRef -> AsmState  -> Maybe Word16
resolveAddressMaybe Nothing _ = Just 0 -- For Imm case, value doesn't matter here
resolveAddressMaybe (Just (AddrLit16 l)) _ = Just l
resolveAddressMaybe (Just (AddrLabel l)) asmState = Map.lookup l labels
    where labels = asmLabels asmState

getInstructionSize :: Mnemonic -> Maybe Operand -> Either String Word16
getInstructionSize m (Just op) = case (m, op) of
    (LDA, OpImm _)  -> Right 2; (LDA, OpZP _)   -> Right 2; (LDA, OpZPX _)  -> Right 2
    (LDA, OpAbs _)  -> Right 3; (LDA, OpAbsX _) -> Right 3; (LDA, OpAbsY _) -> Right 3
    (LDA, OpIndX _) -> Right 2; (LDA, OpIndY _) -> Right 2
    (STA, OpZP _)   -> Right 2; (STA, OpZPX _)  -> Right 2
    (STA, OpAbs _)  -> Right 3; (STA, OpAbsX _) -> Right 3; (STA, OpAbsY _) -> Right 3
    (STA, OpIndX _) -> Right 2; (STA, OpIndY _) -> Right 2
    (LDX, OpImm _)  -> Right 2; (LDX, OpZP _)   -> Right 2; (LDX, OpZPX _)  -> Right 2
    (LDX, OpAbs _)  -> Right 3; (LDX, OpAbsY _) -> Right 3
    (LDY, OpImm _)  -> Right 2; (LDY, OpZP _)   -> Right 2; (LDY, OpZPX _)  -> Right 2
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
    (JMP, OpInd _) -> Right 3
    (JSR, OpAbs _)  -> Right 3
    (_, _) -> Left $ "Unsupported/TODO addressing mode " ++ show op ++ " for " ++ show m

getInstructionSize m Nothing = case m of
    (INX) -> Right 1; (RTS) -> Right 1; (TAX) -> Right 1; (TAY) -> Right 1
    (TXA) -> Right 1; (TYA) -> Right 1; (TXS) -> Right 1; (TSX) -> Right 1
    _     -> Left $ "Mnemonic " ++ show m ++ " requires an operand or is not a known implied instruction."


emitGeneric :: SymbolicInstruction -> Either String Word16 -> Asm ()
emitGeneric _ (Left err) = error $ "Assembly Error (emitGeneric): " ++ err
emitGeneric instruction (Right size) = do
  pc <- gets asmPC
  modify' $ \s -> s { asmCode = (pc, instruction) : asmCode s, asmPC = pc + size }

emitIns :: Mnemonic -> Operand -> Asm ()
emitIns m op = emitGeneric (SIns m (Just op)) (getInstructionSize m (Just op))

emitImplied :: Mnemonic -> Asm ()
emitImplied m = emitGeneric (SIns m Nothing) (getInstructionSize m Nothing)

emitBranch :: BranchMnemonic -> Label -> Asm ()
emitBranch m l = emitGeneric (SBranch m l) (Right 2)

l_ :: Label -> Asm ()
l_ lbl = do pc <- gets asmPC; labels <- gets asmLabels
            when (Map.member lbl labels) $ error $ "Label redefined: " ++ lbl
            modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
            emitGeneric (SLabelDef lbl) (Right 0)

db :: [Word8] -> Asm (); db bs = let size = fromIntegral $ length bs in when (size > 0) $ emitGeneric (SBytes bs) (Right size)
dw :: [Word16] -> Asm (); dw ws = let size = fromIntegral (length ws) * 2 in when (size > 0) $ emitGeneric (SWords ws) (Right size)
string :: String -> Asm (); string str = let bytes = map (fromIntegral . ord) str; size = fromIntegral $ length bytes in when (size > 0) $ emitGeneric (SBytes bytes) (Right size)

makeUniqueLabel :: () -> Asm Label
makeUniqueLabel _ = do
    count <- gets asmMacroCounter
    modify' $ \s -> s { asmMacroCounter = count + 1 }
    return $ "_lbl_" ++ show count

-- --- eDSL Instruction Aliases (using Core primitives) ---
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
jsr :: Operand -> Asm (); jsr = emitIns JSR

impliedOpcode :: Mnemonic -> Maybe [Word8]
impliedOpcode = \case INX -> Just [0xE8]; RTS -> Just [0x60]; TAX -> Just [0xAA]; TAY -> Just [0xA8]
                      TXA -> Just [0x8A]; TYA -> Just [0x98]; TXS -> Just [0x9A]; TSX -> Just [0xBA]; _ -> Nothing

branchOpcode :: BranchMnemonic -> Word8
branchOpcode = \case BNE -> 0xD0; BEQ -> 0xF0; BCS -> 0xB0; BCC -> 0x90; BMI -> 0x30; BPL -> 0x10; BVS -> 0x70; BVC -> 0x50

operandOpcode :: Mnemonic -> Operand -> AsmState -> Maybe [Word8]
operandOpcode m o asmState = resolveAddressMaybe (getOperandAddressRef o) asmState >>= \resolvedAddr ->
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
        (JMP, OpInd _)  -> Just [0x6C, lo resolvedAddr, hi resolvedAddr]
        (JSR, OpAbs _)  -> Just [0x20, lo resolvedAddr, hi resolvedAddr]
        _ -> Nothing -- Combination not supported or requires specific handling

getOperandAddressRef :: Operand -> Maybe AddressRef
getOperandAddressRef = \case
    OpImm _ -> Nothing -- Immediate doesn't have an address ref
    OpAbs r -> Just r; OpAbsX r -> Just r; OpAbsY r -> Just r
    OpZP r -> Just r; OpZPX r -> Just r; OpZPY r -> Just r
    OpIndX r -> Just r; OpIndY r -> Just r


lo :: Word16 -> Word8; lo = fromIntegral . (.&. 0xFF)
hi :: Word16 -> Word8; hi w = fromIntegral (w `shiftR` 8)
hx :: Word16 -> String; hx a = showHex a ""
wordToBytesLE :: Word16 -> [Word8]; wordToBytesLE w = [lo w, hi w]
