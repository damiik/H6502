{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Assembly.Core (
    -- Core Types
    Address,
    Label,
    ProgramCounter,
    LabelExpression(..), -- New type for label arithmetic
    AddressRef(..),
    AddressingMode(..),
    Operand(..),
    pattern Imm,
    pattern AbsLit,
    pattern AbsAddress,
    pattern AbsLabel,
    pattern AbsXLit,
    pattern AbsXLabel,
    pattern ZPLabel,
    zpLit,
    pattern Ind,
    pattern IndX,
    pattern IndY,
    pattern ZPX,
    pattern ZPY,
    pattern AbsYLabel,
    pattern AbsYLit,
    pattern ImmLsbLabel, -- Export new pattern synonym
    pattern ImmMsbLabel, -- Export new pattern synonym
    BranchMnemonic(..),
    Mnemonic(..),
    SymbolicInstruction(..),
    Directive(..), -- NEW: Represents directives like ORG

    -- Core State & Monad
    AsmState(..),
    initialAsmState,
    Asm(..),

    -- Core Primitives (niektóre zostaną zmodyfikowane)
    getInstructionInfo, -- NOWA FUNKCJA ZAMIAST getInstructionSize/operandOpcode
    getInstructionSize, -- ZOSTANIE ZMODYFIKOWANA/USUNIĘTA
    emitGeneric,
    emitIns,
    emitImplied, -- MOŻE BYĆ ZMODYFIKOWANY
    emitAccumulator, -- NOWY
    emitBranch,
    l_,
    if_,
    while_,
    doWhile_,
    db, -- Renamed
    dw, -- Renamed
    string, -- Renamed
    immChar,
    stringPETSCI, -- NEW: PETSCII string function
    stringC64, -- NEW: PETSCII string function for screen codes
    org, -- Renamed & Export the org directive function
    makeUniqueLabel,
    makeLabelWithPrefix,
    generateInstructionBytes,

    -- Opcode-related functions (teraz oparte na tabeli)
    instructionTable, -- NOWA TABELA
    getOperandAddressingMode, -- NOWA FUNKCJA POMOCNICZA
    resolveAddressMaybe, -- ISTNIEJĄCA
    getOperandAddressRef, -- ISTNIEJĄCA
    branchOpcode, -- NOWA FUNKCJA POMOCNICZA

    -- Miscellaneous
    lsb, msb,
    hx,
    asc,
    addr2word16,
    evalLabelExpr, -- NEW EXPORT
    wordToBytesLE, -- Added export
    (.+) , -- Renamed operator for AddressRef
    (.-), -- Renamed operator for AddressRef
    parens,            -- function for wrapping AddressRef in parentheses

    -- New typeclass for arithmetic expressions and its methods
    ArithExpr(add, sub),

    -- eDSL Instruction Aliases
    lda, sta, ldx, ldy, jmp, inx, adc, sbc, tax, tay, stx, sty,
    cmp, cpx, cpy, txa, tya, txs, tsx, bne, beq, bcs, bcc, bmi,
    bpl, bvs, bvc, jsr, rts, ora, asl, php, clc, and, bit, rol,
    ror, plp, sec, rti, eor, lsr, pha, cli, pla, sei, dey, clv,
    iny, dex, cld, nop, sed, inc, dec, brk,

    -- conditions
    Conditions(..),
    pattern AccIsNonZero,
    pattern AccIsZero,
    pattern AccIsPositive,
    pattern AccIsNegative,
) where

import Prelude hiding (and, or) -- Hiding only 'and', 'or'
import qualified Prelude as P (and, or)  -- Use P.(+) and P.(-) for standard numeric ops
import Control.Monad.State.Strict
import Control.Monad (when)
import Data.Word
import Data.Int (Int8)
import Data.Bits ((.&.), shiftR)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) -- Jawny import dla Map
import Data.Maybe (fromMaybe) -- Potrzebne dla lookupów
import Data.Char (ord, chr, isDigit, isAsciiUpper, isAsciiLower)
import Numeric (showHex)
import Data.List (foldl') -- Potrzebne do budowania tabeli

import Assembly.Branch (BranchMnemonic(..)) -- Importujemy BranchMnemonic z osobnego modułu
import Assembly.Instructions6502 (getModeSize, instructionData, Mnemonic(..), AddressingMode(..))

-- --- Types ---
type Address = Word16
type Label = String
type ProgramCounter = Word16

-- New type for label arithmetic expressions
data LabelExpression
  = LabelRef Label
  | LabelAdd LabelExpression Word16
  | LabelSub LabelExpression Word16
  | LabelParen LabelExpression
  deriving (Show, Eq, Ord)

-- Updated AddressRef to include label expressions
data AddressRef
  = AddrLit16 Word16
  | AddrLit8 Word8
  | AddrLabel Label
  | AddrLabelExpr LabelExpression
  deriving (Show, Eq, Ord)

-- Renamed operators for AddressRef
infixl 6 .+
(.+):: AddressRef -> Word16 -> AddressRef
a .+ i  =
    case a of
        AddrLit16 v -> AddrLit16 (v + i) -- Use P.(+) for Word16 addition
        AddrLit8 v -> AddrLit8 (fromIntegral (fromIntegral v + i)) -- Use P.(+) for Word16 addition
        AddrLabel l -> AddrLabelExpr (LabelAdd (LabelRef l) i)
        AddrLabelExpr expr -> AddrLabelExpr (LabelAdd expr i)

infixl 6 .-
(.-) :: AddressRef -> Word16 -> AddressRef
a .- i =
  case a of
    AddrLit16 v -> AddrLit16 (v - i) -- Use P.(-) for Word16 subtraction
    AddrLit8 v -> AddrLit8 (fromIntegral (fromIntegral v - i)) -- Use P.(-) for Word16 subtraction
    AddrLabel l -> AddrLabelExpr (LabelSub (LabelRef l) i)
    AddrLabelExpr expr -> AddrLabelExpr (LabelSub expr i)

-- New typeclass for arithmetic expressions
class ArithExpr a where
  add :: a -> Word16 -> a
  sub :: a -> Word16 -> a

-- Instance using the renamed operators
instance ArithExpr AddressRef where
  add = (.+)
  sub = (.-)

-- Updated Operand to match AddressingMode
data Operand
    = OpImm Word8          -- Immediate
    | OpZP AddressRef      -- ZeroPage
    | OpZPX AddressRef     -- ZeroPageX
    | OpZPY AddressRef     -- ZeroPageY
    | OpAbs AddressRef     -- Absolute
    | OpAbsX AddressRef    -- AbsoluteX
    | OpAbsY AddressRef    -- AbsoluteY
    | OpInd AddressRef     -- Indirect (tylko JMP)
    | OpIndX AddressRef    -- IndirectX ($NN,X)
    | OpIndY AddressRef    -- IndirectY ($NN),Y
    -- NEW: Operands for immediate LSB/MSB of a label's address
    | OpImmLsbLabel Label  -- Immediate LSB of label address, e.g., LDA #<label
    | OpImmMsbLabel Label  -- Immediate MSB of label address, e.g., LDA #>label
    -- Brak konstruktora dla Implicit, Accumulator, Relative - są obsługiwane inaczej
    deriving (Show, Eq)

-- Pattern Synonyms (aktualizacja i dodanie nowych)
pattern Imm :: Word8 -> Operand; pattern Imm v = OpImm v
-- pattern ImmChar :: Char -> Operand; pattern ImmChar v = OpImm (fromIntegral (ord v))
pattern AbsLit :: Word16 -> Operand; pattern AbsLit v = OpAbs (AddrLit16 v)
pattern AbsAddress :: AddressRef -> Operand; pattern AbsAddress v = OpAbs v
pattern AbsLabel :: Label -> Operand; pattern AbsLabel l = OpAbs (AddrLabel l)
pattern AbsXLit :: Word16 -> Operand; pattern AbsXLit v = OpAbsX (AddrLit16 v)
pattern AbsXLabel :: Label -> Operand; pattern AbsXLabel l = OpAbsX (AddrLabel l)
pattern AbsYLit :: Word16 -> Operand; pattern AbsYLit v = OpAbsY (AddrLit16 v)
pattern AbsYLabel :: Label -> Operand; pattern AbsYLabel l = OpAbsY (AddrLabel l)
pattern ZPLabel :: Label -> Operand; pattern ZPLabel l = OpZP (AddrLabel l)
zpLit :: Word8 -> Operand; zpLit v = OpZP (AddrLit16 (fromIntegral v))
pattern ZPX :: AddressRef -> Operand; pattern ZPX r = OpZPX r
pattern ZPY :: AddressRef -> Operand; pattern ZPY r = OpZPY r
pattern Ind :: AddressRef -> Operand; pattern Ind r = OpInd r
pattern IndX :: AddressRef -> Operand; pattern IndX r = OpIndX r
pattern IndY :: AddressRef -> Operand; pattern IndY r = OpIndY r
-- NEW: Pattern synonyms for immediate LSB/MSB operands
pattern ImmLsbLabel :: Label -> Operand; pattern ImmLsbLabel l = OpImmLsbLabel l
pattern ImmMsbLabel :: Label -> Operand; pattern ImmMsbLabel l = OpImmMsbLabel l

-- NEW: Represents directives that don't directly generate code but affect assembly
newtype Directive = DOrg Address deriving (Show, Eq) -- HLint: Use newtype

-- SymbolicInstruction (updated to include directives)
data SymbolicInstruction
    = SLabelDef Label
    | SIns Mnemonic (Maybe Operand)
    | SBranch BranchMnemonic Label
    | SBytes [Word8]
    | SWords [Word16]
    | SDirective Directive -- NEW: Add directives here
    deriving (Show, Eq)


-- --- Assembler State (bez zmian) ---
data AsmState = AsmState
  { asmPC            :: ProgramCounter
  , asmLabels        :: Map.Map Label ProgramCounter
  , asmCode          :: [(ProgramCounter, SymbolicInstruction)]
  , asmMacroCounter  :: Int
  } deriving (Show)

initialAsmState :: ProgramCounter -> AsmState
initialAsmState startAddr = AsmState startAddr Map.empty [] 0

-- --- Assembler Monad (bez zmian) ---
newtype Asm a = Asm { unAsm :: State AsmState a }
  deriving (Functor, Applicative, Monad, MonadState AsmState)

-- --- Tabela Instrukcji ---

-- Typ dla wpisu w tabeli: (Opcode, Rozmiar w bajtach)
type OpcodeEntry = (Word8, Word8)

-- Główna tabela instrukcji: Mapa z Mnemonic na mapę z AddressingMode na OpcodeEntry
instructionTable :: Map Mnemonic (Map AddressingMode OpcodeEntry)
instructionTable = buildInstructionTable instructionData

-- Funkcja pomocnicza do konwersji (przeniesiona/utworzona tutaj)
branchMnemonicToMnemonic :: BranchMnemonic -> Mnemonic
branchMnemonicToMnemonic = \case
    Assembly.Branch.B_BNE -> BNE;
    Assembly.Branch.B_BEQ -> BEQ;
    Assembly.Branch.B_BCS -> BCS;
    Assembly.Branch.B_BCC -> BCC;
    Assembly.Branch.B_BMI -> BMI;
    Assembly.Branch.B_BPL -> BPL;
    Assembly.Branch.B_BVS -> BVS;
    Assembly.Branch.B_BVC -> BVC;
    -- Te konstruktory po prawej stronie odnoszą się teraz do typu Mnemonic z Instructions6502

-- Funkcja do pobierania opkodu (przeniesiona i zmodyfikowana z Core.hs)
-- Używa teraz instructionTable z Core.hs
branchOpcode :: BranchMnemonic -> Word8
branchOpcode bm =
    let mnemonic = branchMnemonicToMnemonic bm
    in case Map.lookup mnemonic instructionTable >>= Map.lookup Relative of
        Just (opcode, _size) -> opcode
        Nothing -> error $ "Internal error: Opcode for branch " ++ show bm ++ " not found in instruction table."


-- Funkcja budująca główną tabelę instrukcji
buildInstructionTable :: [(Mnemonic, AddressingMode, Word8)] -> Map Mnemonic (Map AddressingMode OpcodeEntry)
buildInstructionTable = foldl' insertInstruction Map.empty
  where
    insertInstruction table (mnemonic, mode, opcode) =
      let size = getModeSize mode
          entry = (opcode, size)
          modeMap = Map.singleton mode entry
      in Map.insertWith Map.union mnemonic modeMap table

-- NOWA: Funkcja pomocnicza do mapowania Operandu na AddressingMode
-- Dla Implicit/Accumulator potrzebujemy mnemonika
getOperandAddressingMode :: Mnemonic -> Maybe Operand -> Either String AddressingMode
getOperandAddressingMode _ (Just (OpImm _))   = Right Immediate
getOperandAddressingMode _ (Just (OpZP _))    = Right ZeroPage
getOperandAddressingMode _ (Just (OpZPX _))   = Right ZeroPageX
getOperandAddressingMode _ (Just (OpZPY _))   = Right ZeroPageY
getOperandAddressingMode _ (Just (OpAbs _))   = Right Absolute
getOperandAddressingMode _ (Just (OpAbsX _))  = Right AbsoluteX
getOperandAddressingMode _ (Just (OpAbsY _))  = Right AbsoluteY
getOperandAddressingMode _ (Just (OpInd _))   = Right Indirect
getOperandAddressingMode _ (Just (OpIndX _))  = Right IndirectX
getOperandAddressingMode _ (Just (OpIndY _))  = Right IndirectY
-- NEW: Handle LSB/MSB label operands as Immediate mode
getOperandAddressingMode _ (Just (OpImmLsbLabel _)) = Right Immediate
getOperandAddressingMode _ (Just (OpImmMsbLabel _)) = Right Immediate
getOperandAddressingMode m Nothing = case m of
    -- Sprawdzanie czy mnemonic obsługuje tryb Accumulator
    ASL -> Right Accumulator
    LSR -> Right Accumulator
    ROL -> Right Accumulator
    ROR -> Right Accumulator
    -- W przeciwnym razie zakładamy Implicit (lub błąd, jeśli nie ma wpisu)
    _   -> Right Implicit

-- NOWA: Funkcja do pobierania informacji o instrukcji (opcode i rozmiar)
getInstructionInfo :: Mnemonic -> Maybe Operand -> Either String OpcodeEntry
getInstructionInfo m maybeOp = do
    mode <- getOperandAddressingMode m maybeOp
    case Map.lookup m instructionTable >>= Map.lookup mode of
        Just entry -> Right entry
        Nothing    -> Left $ "Unsupported addressing mode '" ++ show mode ++ "' for mnemonic '" ++ show m ++ "'"

-- --- Core Primitive Functions (MODYFIKACJE) ---

-- resolveAddressMaybe (updated to handle AddrLabelExpr and LabelParen)
resolveAddressMaybe :: Maybe AddressRef -> AsmState -> Maybe Word16
resolveAddressMaybe Nothing _ = Just 0 -- For Imm, value doesn't matter
resolveAddressMaybe (Just (AddrLit16 v)) _ = Just v
resolveAddressMaybe (Just (AddrLit8 v)) _ = Just (fromIntegral v) -- Convert to Word16
resolveAddressMaybe (Just (AddrLabel l)) s = Map.lookup l (asmLabels s)
resolveAddressMaybe (Just (AddrLabelExpr expr)) s = resolveLabelExpr expr (asmLabels s)
  where
    resolveLabelExpr :: LabelExpression -> Map Label ProgramCounter -> Maybe Word16
    resolveLabelExpr (LabelRef l) labels = Map.lookup l labels
    resolveLabelExpr (LabelAdd subExpr offset) labels = (+) offset <$> resolveLabelExpr subExpr labels -- Use P.(+)
    resolveLabelExpr (LabelSub subExpr offset) labels = subtract offset <$> resolveLabelExpr subExpr labels -- Using subtract for correct order
    resolveLabelExpr (LabelParen subExpr) labels = resolveLabelExpr subExpr labels -- Handle LabelParen

-- getInstructionSize (now uses getInstructionInfo)
getInstructionSize :: Mnemonic -> Maybe Operand -> Either String Word16
getInstructionSize m maybeOp = fmap (fromIntegral . snd) (getInstructionInfo m maybeOp)

-- emitGeneric (Use P.(+) for PC update)
emitGeneric :: SymbolicInstruction -> Either String Word16 -> Asm ()
emitGeneric _ (Left err) = error $ "Assembly Error (emitGeneric): " ++ err -- TODO: Lepsza obsługa błędów
emitGeneric instruction (Right size) = do
  pc <- gets asmPC
  modify' $ \s -> s { asmCode = (pc, instruction) : asmCode s, asmPC = pc + size }

-- emitIns (bez zmian, ale używa zaktualizowanego getInstructionSize)
emitIns :: Mnemonic -> Operand -> Asm ()
emitIns m op = emitGeneric (SIns m (Just op)) (getInstructionSize m (Just op))

-- emitImplied (używa teraz SIns m Nothing)
emitImplied :: Mnemonic -> Asm ()
emitImplied m = emitGeneric (SIns m Nothing) (getInstructionSize m Nothing)

-- NOWA: emitAccumulator (dla jasności, chociaż technicznie to też SIns m Nothing)
emitAccumulator :: Mnemonic -> Asm ()
emitAccumulator m = emitGeneric (SIns m Nothing) (getInstructionSize m Nothing)

-- emitBranch (bez zmian)
emitBranch :: BranchMnemonic -> Label -> Asm ()
emitBranch m l = emitGeneric (SBranch m l) (Right 2)

-- NEW: org directive function
-- Sets the program counter to a specific address.
org :: Address -> Asm ()
org addr = do
    pc <- gets asmPC -- Get current PC for the record, though it doesn't generate bytes
    -- Add the directive instruction to the code list
    modify' $ \s -> s { asmCode = (pc, SDirective (DOrg addr)) : asmCode s }
    -- Set the program counter to the new address
    modify' $ \s -> s { asmPC = addr }

-- l_ (bez zmian)
l_ :: Label -> Asm ()
l_ lbl = do pc <- gets asmPC; labels <- gets asmLabels
            when (Map.member lbl labels) $ error $ "Label redefined: " ++ lbl -- TODO: Lepsza obsługa błędów
            modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
            emitGeneric (SLabelDef lbl) (Right 0)

-- db, dw, string (Renamed)
db :: [Word8] -> Asm (); db bs = let size = fromIntegral $ length bs in when (size > 0) $ emitGeneric (SBytes bs) (Right size)
dw :: [Word16] -> Asm (); dw ws = let size = fromIntegral (length ws) * 2 in when (size > 0) $ emitGeneric (SWords ws) (Right size)
string :: String -> Asm (); string str = let bytes = map (fromIntegral . ord) str; size = fromIntegral $ length bytes in when (size > 0) $ emitGeneric (SBytes bytes) (Right size)

immChar :: Char -> Operand; immChar v = OpImm (fromIntegral (ord v))

-- NEW: stringPETSCI function for PETSCII encoding
stringPETSCI :: String -> Asm ()
stringPETSCI str =
  let
    -- Simple PETSCII conversion (basic characters)
    petscii :: Char -> Word8
    petscii c = case c of
      c' | isAsciiUpper c' -> fromIntegral (ord c' + 128) -- Uppercase letters
        | isAsciiLower c' -> fromIntegral (ord c' - 32)  -- Lowercase to uppercase PETSCII
      ' '      -> 32                         -- Space
      _        -> fromIntegral (ord c)       -- Other characters (basic assumption)
    bytes = map petscii str
    size = fromIntegral $ length bytes
  in when (size > 0) $ emitGeneric (SBytes bytes) (Right size)

stringC64 :: String -> Asm ()
stringC64  str =
  let
    -- Simple PETSCII conversion (basic characters)
    asciiToScreencode :: Char -> Word8
    asciiToScreencode c
      | isAsciiUpper c = fromIntegral (ord c - 64)          -- A-Z: 65-90 -> 1-26
      | isAsciiLower c = fromIntegral (ord c - 96)          -- a-z: 97-122 -> 1-26 (mapowane na wielkie)
      | isDigit c      = fromIntegral (ord c - 48 + 48)     -- 0-9: 48-57 -> 48-57
      | c == ' '             =  32                    -- Spacja: 32
      | c == '@'             =  0                     -- @: 0
      | c == '$'             =  36                    -- $: 36
      | otherwise            =  63                    -- Nieobsługiwane znaki: ? (63)
    bytes = map asciiToScreencode str
    size = fromIntegral $ length bytes
  in when (size > 0) $ emitGeneric (SBytes bytes) (Right size)


-- makeUniqueLabel_ (Use P.(+) for counter update)
makeUniqueLabel_ :: String -> Asm Label
makeUniqueLabel_ prefix = do
    count <- gets asmMacroCounter
    modify' $ \s -> s { asmMacroCounter = count + 1 }
    return $ "_lbl_" ++ prefix ++ "_" ++ show count

makeUniqueLabel :: () -> Asm Label
makeUniqueLabel _ = makeUniqueLabel_ ""

makeLabelWithPrefix :: String -> Asm Label
makeLabelWithPrefix = makeUniqueLabel_


-- --- Opcode-related functions (USUNIĘTE/ZASTĄPIONE) ---
-- impliedOpcode, operandOpcode zostają zastąpione przez logikę w generateBinary używającą instructionTable

-- Funkcja pomocnicza dla generateBinary (może być w Assembly.hs)
-- generateInstructionBytes (updated to handle OpImmLsbLabel and OpImmMsbLabel)
generateInstructionBytes :: Mnemonic -> Maybe Operand -> AsmState -> Either String [Word8]
generateInstructionBytes m maybeOp asmState = do
    (opcode, size) <- getInstructionInfo m maybeOp

    -- Handle specific operand types
    case maybeOp of
        Just (OpImmLsbLabel l) ->
            case Map.lookup l (asmLabels asmState) of
                Just addr -> Right [opcode, lsb addr]
                Nothing   -> Left $ "Failed to resolve label '" ++ l ++ "' for LSB operand"
        Just (OpImmMsbLabel l) ->
            case Map.lookup l (asmLabels asmState) of
                Just addr -> Right [opcode, msb addr]
                Nothing   -> Left $ "Failed to resolve label '" ++ l ++ "' for MSB operand"
        Just (OpImm v) -> Right [opcode, v] -- Standard Immediate
        _ -> do -- Handle address-based operands
            addressRef <- Right $ getOperandAddressRef maybeOp
            resolvedAddrMaybe <- Right $ resolveAddressMaybe addressRef asmState
            case resolvedAddrMaybe of
                Nothing | size > 1 && maybeOp /= Nothing ->
                    Left $ "Failed to resolve address for " ++ show m ++ " " ++ show maybeOp
                _ -> let resolvedAddr = fromMaybe 0 resolvedAddrMaybe
                     in Right $ case size of
                         1 -> [opcode] -- Implicit or Accumulator
                         2 -> [opcode, lsb resolvedAddr] -- ZP, ZPX, ZPY, IndX, IndY, Relative
                         3 -> [opcode, lsb resolvedAddr, msb resolvedAddr] -- Abs, AbsX, AbsY, Ind
                         _ -> error "Internal error: Invalid instruction size derived"

-- getOperandAddressRef (no changes needed, extracts AddressRef)
getOperandAddressRef :: Maybe Operand -> Maybe AddressRef
getOperandAddressRef = \case
    Just (OpZP r)   -> Just r
    Just (OpZPX r)  -> Just r
    Just (OpZPY r)  -> Just r
    Just (OpAbs r)  -> Just r
    Just (OpAbsX r) -> Just r
    Just (OpAbsY r) -> Just r
    Just (OpInd r)  -> Just r
    Just (OpIndX r) -> Just r
    Just (OpIndY r) -> Just r
    _               -> Nothing -- Dla Imm, Implicit, Accumulator


-- --- Miscellaneous ---
lsb :: Word16 -> Word8; lsb = fromIntegral . (.&. 0xFF)
msb :: Word16 -> Word8; msb w = fromIntegral (w `shiftR` 8)
hx :: Word16 -> String; hx a = showHex a ""
wordToBytesLE :: Word16 -> [Word8]; wordToBytesLE w = [msb w, msb w]

-- Top-level function for testing
evalLabelExpr :: LabelExpression -> Word16
evalLabelExpr = \case
  LabelRef l -> error $ "Compile-time error: Cannot get value of unresolved label '" ++ l ++ "' within expression"
  LabelParen expr -> evalLabelExpr expr
  LabelAdd subExpr offset -> evalLabelExpr subExpr + offset -- Use P.(+)
  LabelSub subExpr offset -> evalLabelExpr subExpr - offset -- Use P.(-)

-- addrVal using evalLabelExpr
addr2word16 :: AddressRef -> Word16
addr2word16 = \case
  AddrLit16 v -> v
  AddrLit8 v -> fromIntegral v
  AddrLabel l -> -- resolveAddressMaybe (AddrLabel l)
    -- TODO: Handle this case properly
    error $ "Compile-time error!!: Cannot get value of unresolved label '" ++ l ++ "'"
  AddrLabelExpr expr -> evalLabelExpr expr

addr2word8 :: AddressRef -> Word8
addr2word8 = \case
  AddrLit16 v -> fromIntegral v
  AddrLit8 v -> v
  AddrLabel l -> error $ "Compile-time error!!: Cannot get value of unresolved label '" ++ l ++ "'"
  AddrLabelExpr expr -> fromIntegral $ evalLabelExpr expr


-- parens function definition
parens :: AddressRef -> AddressRef
parens (AddrLabelExpr e) = AddrLabelExpr (LabelParen e)
parens addr = addr

-- Character to ASCII code
asc :: Char -> Word8
asc = fromIntegral . fromEnum

-- --- eDSL Instruction Aliases ---
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
bne :: Label -> Asm (); bne = emitBranch B_BNE
beq :: Label -> Asm (); beq = emitBranch B_BEQ
bcs :: Label -> Asm (); bcs = emitBranch B_BCS
bcc :: Label -> Asm (); bcc = emitBranch B_BCC
bmi :: Label -> Asm (); bmi = emitBranch B_BMI
bpl :: Label -> Asm (); bpl = emitBranch B_BPL
bvs :: Label -> Asm (); bvs = emitBranch B_BVS
bvc :: Label -> Asm (); bvc = emitBranch B_BVC
jsr :: Operand -> Asm (); jsr = emitIns JSR
ora :: Operand -> Asm (); ora = emitIns ORA
asl :: Maybe Operand -> Asm ()
asl Nothing   = emitAccumulator ASL
asl (Just op) = emitIns ASL op
php :: Asm (); php = emitImplied PHP
clc :: Asm (); clc = emitImplied CLC
and :: Operand -> Asm (); and = emitIns AND
bit :: Operand -> Asm (); bit = emitIns BIT
rol :: Maybe Operand -> Asm ()
rol Nothing   = emitAccumulator ROL
rol (Just op) = emitIns ROL op
plp :: Asm (); plp = emitImplied PLP
sec :: Asm (); sec = emitImplied SEC
rti :: Asm (); rti = emitImplied RTI
eor :: Operand -> Asm (); eor = emitIns EOR
lsr :: Maybe Operand -> Asm ()
lsr Nothing   = emitAccumulator LSR
lsr (Just op) = emitIns LSR op
pha :: Asm (); pha = emitImplied PHA
cli :: Asm (); cli = emitImplied CLI
pla :: Asm (); pla = emitImplied PLA
sei :: Asm (); sei = emitImplied SEI
dey :: Asm (); dey = emitImplied DEY
clv :: Asm (); clv = emitImplied CLV
iny :: Asm (); iny = emitImplied INY
dex :: Asm (); dex = emitImplied DEX
cld :: Asm (); cld = emitImplied CLD
nop :: Asm (); nop = emitImplied NOP
sed :: Asm (); sed = emitImplied SED
inc :: Operand -> Asm (); inc = emitIns INC
dec :: Operand -> Asm (); dec = emitIns DEC
brk :: Asm (); brk = emitImplied BRK
ror :: Maybe Operand -> Asm ()
ror Nothing   = emitAccumulator ROR
ror (Just op) = emitIns ROR op


-- Typ danych reprezentujący warunki flag procesora
data Conditions =
    IsNonZero   -- Z=0 (BNE) - Wynik ostatniej operacji nie był zerem
  | IsZero      -- Z=1 (BEQ) - Wynik ostatniej operacji był zerem
  | IsCarryClear      -- C=0 (BCC) - Nie było przeniesienia
  | IsCarrySet        -- C=1 (BCS) - Było przeniesienie
  | IsNegative  -- N=1 (BMI) - Wynik był ujemny (najstarszy bit ustawiony)
  | IsPositive  -- N=0 (BPL) - Wynik był dodatni lub zerowy (najstarszy bit wyczyszczony)
  | IsOverflowClear   -- V=0 (BVC) - Nie było nadmiaru w operacji arytmetycznej ze znakiem
  | IsOverflowSet     -- V=1 (BVS) - Był nadmiar w operacji arytmetycznej ze znakiem
  deriving (Eq, Show)

-- Funkcja generująca odpowiedni skok warunkowy
branchOnCondition :: Conditions -> Label -> Asm ()
branchOnCondition IsNonZero target = emitBranch B_BNE target
branchOnCondition IsZero    target = emitBranch B_BEQ target
branchOnCondition IsCarryClear      target = emitBranch B_BCC target
branchOnCondition IsCarrySet        target = emitBranch B_BCS target
branchOnCondition IsNegative  target = emitBranch B_BMI target
branchOnCondition IsPositive  target = emitBranch B_BPL target
branchOnCondition IsOverflowClear   target = emitBranch B_BVC target
branchOnCondition IsOverflowSet     target = emitBranch B_BVS target

-- Funkcja odwracająca warunek  
invert :: Conditions -> Conditions
invert IsNonZero = IsZero
invert IsZero    = IsNonZero
invert IsCarryClear      = IsCarrySet
invert IsCarrySet        = IsCarryClear
invert IsNegative  = IsPositive
invert IsPositive  = IsNegative
invert IsOverflowClear   = IsOverflowSet
invert IsOverflowSet     = IsOverflowClear

-- Opcjonalne synonimy wzorców dla czytelności (syntax sugar)
pattern AccIsZero      :: Conditions
pattern AccIsZero      = IsZero

pattern AccIsNonZero   :: Conditions
pattern AccIsNonZero   = IsNonZero

pattern AccIsPositive  :: Conditions
pattern AccIsPositive  = IsPositive

pattern AccIsNegative  :: Conditions
pattern AccIsNegative  = IsNegative


-- Wykonuje blok kodu, jeśli podany warunek jest PRAWDZIWY
-- (Zakłada, że flagi zostały ustawione *przed* wywołaniem if_)
if_ :: Conditions -> Asm () -> Asm ()
if_ condition asmBlock = do
    skipLabel <- makeUniqueLabel ()
    -- Wykonaj skok WARUNKOWY ZA blok, jeśli warunek jest FAŁSZYWY
    branchOnCondition (invert condition) skipLabel
    -- Wykonaj blok kodu, jeśli warunek jest PRAWIDŁOWY (nie skoczono)
    asmBlock
    l_ skipLabel -- Etykieta końca bloku if

-- Zaktualizowane makro WHILE
-- Wykonuje blok kodu, dopóki warunek jest PRAWDZIWY
-- (Zakłada, że flagi są ustawiane *przed* sprawdzeniem warunku na początku pętli)
while_ :: Conditions -> Asm () -> Asm ()
while_ condition asmBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel   <- makeUniqueLabel ()
    l_ startLabel
    -- Sprawdź warunek: skocz na koniec, jeśli FAŁSZYWY
    branchOnCondition (invert condition) endLabel
    -- Wykonaj ciało pętli, jeśli warunek PRAWDZIWY
    asmBlock
    jmp $ AbsLabel startLabel -- Wróć na początek, aby ponownie sprawdzić warunek
    l_ endLabel

-- Zaktualizowane makro DO-WHILE
-- Wykonuje blok kodu RAZ, a następnie powtarza, dopóki warunek jest PRAWDZIWY
-- (Zakłada, że flagi są ustawiane *wewnątrz* bloku, tuż przed końcem iteracji)
doWhile_ :: Conditions -> Asm () -> Asm ()
doWhile_ condition asmBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    -- Wykonaj ciało pętli
    asmBlock
    -- Sprawdź warunek na końcu: skocz na początek, jeśli PRAWIDZIWY
    branchOnCondition condition startLabel
    -- W przeciwnym razie (warunek FAŁSZYWY), wypadnij z pętli
