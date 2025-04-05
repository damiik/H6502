{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Assembly.Core (
    -- Core Types
    Address,
    Label,
    ProgramCounter,
    AddressRef(..),
    AddressingMode(..), -- NOWY TYP
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
    BranchMnemonic(..),
    Mnemonic(..), -- ZAKTUALIZOWANY
    SymbolicInstruction(..),

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
    db,
    dw,
    string,
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
    lo,
    hi,
    hx,
    wordToBytesLE,

    -- eDSL Instruction Aliases 
    lda, sta, ldx, ldy, jmp, inx, adc, sbc, tax, tay, stx, sty, 
    cmp, cpx, cpy, txa, tya, txs, tsx, bne, beq, bcs, bcc, bmi, 
    bpl, bvs, bvc, jsr, rts, ora, asl, php, clc, and, bit, rol, 
    ror, plp, sec, rti, eor, lsr, pha, cli, pla, sei, dey, clv, 
    iny, dex, cld, nop, sed, inc, dec
) where


import Prelude hiding (and, or) -- remove conflict with opcode names
import Control.Monad.State.Strict
import Control.Monad (when)
import Data.Word
import Data.Int (Int8)
import Data.Bits ((.&.), shiftR)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) -- Jawny import dla Map
import Data.Maybe (mapMaybe, fromMaybe) -- Potrzebne dla lookupów
import Data.Char (ord)
import Numeric (showHex)
import Data.List (foldl') -- Potrzebne do budowania tabeli

import Assembly.Branch (BranchMnemonic(..)) -- Importujemy BranchMnemonic z osobnego modułu
import Assembly.Instructions6502 (getModeSize, instructionData, Mnemonic(..), AddressingMode(..)) -- Importujemy dane instrukcji z osobnego modułu

-- --- Types ---
type Address = Word16
type Label = String
type ProgramCounter = Word16
data AddressRef = AddrLit16 Word16 | AddrLabel Label deriving (Show, Eq, Ord) -- Dodano Ord


-- ZAKTUALIZOWANY Operand, aby pasował do AddressingMode
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
    -- Brak konstruktora dla Implicit, Accumulator, Relative - są obsługiwane inaczej
    deriving (Show, Eq)

-- Pattern Synonyms (aktualizacja i dodanie nowych)
pattern Imm :: Word8 -> Operand; pattern Imm v = OpImm v
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

-- SymbolicInstruction (bez zmian na razie)
data SymbolicInstruction = SLabelDef Label | SIns Mnemonic (Maybe Operand) | SBranch BranchMnemonic Label | SBytes [Word8] | SWords [Word16] deriving (Show, Eq)

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

-- resolveAddressMaybe (bez zmian)
resolveAddressMaybe :: Maybe AddressRef -> AsmState -> Maybe Word16
resolveAddressMaybe Nothing _ = Just 0 -- Dla Imm, wartość nie ma znaczenia
resolveAddressMaybe (Just (AddrLit16 l)) _ = Just l
resolveAddressMaybe (Just (AddrLabel l)) asmState = Map.lookup l labels
    where labels = asmLabels asmState

-- getInstructionSize (teraz używa getInstructionInfo)
getInstructionSize :: Mnemonic -> Maybe Operand -> Either String Word16
getInstructionSize m maybeOp = fmap (fromIntegral . snd) (getInstructionInfo m maybeOp)

-- emitGeneric (bez zmian)
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

-- l_ (bez zmian)
l_ :: Label -> Asm ()
l_ lbl = do pc <- gets asmPC; labels <- gets asmLabels
            when (Map.member lbl labels) $ error $ "Label redefined: " ++ lbl -- TODO: Lepsza obsługa błędów
            modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
            emitGeneric (SLabelDef lbl) (Right 0)

-- db, dw, string, makeUniqueLabel (bez zmian)
db :: [Word8] -> Asm (); db bs = let size = fromIntegral $ length bs in when (size > 0) $ emitGeneric (SBytes bs) (Right size)
dw :: [Word16] -> Asm (); dw ws = let size = fromIntegral (length ws) * 2 in when (size > 0) $ emitGeneric (SWords ws) (Right size)
string :: String -> Asm (); string str = let bytes = map (fromIntegral . ord) str; size = fromIntegral $ length bytes in when (size > 0) $ emitGeneric (SBytes bytes) (Right size)

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
generateInstructionBytes :: Mnemonic -> Maybe Operand -> AsmState -> Either String [Word8]
generateInstructionBytes m maybeOp asmState = do
    (opcode, size) <- getInstructionInfo m maybeOp
    addressRef <- Right $ getOperandAddressRef maybeOp -- Pobierz AddressRef, jeśli istnieje
    resolvedAddrMaybe <- Right $ resolveAddressMaybe addressRef asmState

    case resolvedAddrMaybe of
        Nothing | size > 1 && maybeOp /= Nothing -> Left $ "Failed to resolve address for " ++ show m ++ " " ++ show maybeOp -- Błąd jeśli adres jest potrzebny a nie da się go rozwiązać
        _ -> let resolvedAddr = fromMaybe 0 resolvedAddrMaybe -- Domyślnie 0 jeśli niepotrzebny (np. Imm) lub błąd
                 operandVal = case maybeOp of Just (OpImm v) -> v; _ -> 0 -- Pobierz wartość dla Immediate
             in Right $ case size of
                 1 -> [opcode]
                 2 -> case fromMaybe Implicit (either (const Nothing) Just (getOperandAddressingMode m maybeOp)) of -- Potrzebujemy trybu, aby odróżnić Imm od innych 2-bajtowych
                          Immediate -> [opcode, operandVal]
                          _         -> [opcode, lo resolvedAddr] -- ZP, ZPX, ZPY, IndX, IndY
                 3 -> [opcode, lo resolvedAddr, hi resolvedAddr] -- Abs, AbsX, AbsY, Ind
                 _ -> error "Internal error: Invalid instruction size derived" -- Nie powinno się zdarzyć

-- getOperandAddressRef (aktualizacja dla nowych Op*)
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

-- branchOpcode (przeniesione)
-- branchOpcode :: BranchMnemonic -> Word8
-- branchOpcode = \case BNE -> 0xD0; BEQ -> 0xF0; BCS -> 0xB0; BCC -> 0x90; BMI -> 0x30; BPL -> 0x10; BVS -> 0x70; BVC -> 0x50


-- --- Miscellaneous (bez zmian) ---
lo :: Word16 -> Word8; lo = fromIntegral . (.&. 0xFF)
hi :: Word16 -> Word8; hi w = fromIntegral (w `shiftR` 8)
hx :: Word16 -> String; hx a = showHex a ""
wordToBytesLE :: Word16 -> [Word8]; wordToBytesLE w = [lo w, hi w]

-- --- eDSL Instruction Aliases (TRZEBA DODAĆ NOWE) ---

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


-- Uwaga: Trzeba dodać aliasy dla wszystkich mnemoników z `Mnemonic`
