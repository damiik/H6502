{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-} -- Użyjemy dla ładniejszych literałów

import Control.Monad.State.Strict
import Control.Monad (when)
import Data.Word
import Data.Int (Int8)
import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Data.Char (ord)
import Data.Maybe (fromMaybe) -- Do obsługi Maybe Operand
import Data.Functor.Contravariant (Op(Op))
import Numeric (showHex)

-- --- Typy pomocnicze ---

type Address = Word16
type Label = String
type ProgramCounter = Word16

-- Adres może być literałem 16-bit lub etykietą
data AddressRef = AddrLit16 Word16 | AddrLabel Label deriving (Show, Eq)

-- <<< NOWY TYP OPERAND >>>
-- Reprezentuje różne tryby adresowania
data Operand
  = OpImm Word8          -- #$nn
  | OpAbs AddressRef     -- $nnnn | label
  | OpAbsX AddressRef    -- $nnnn,X | label,X
  | OpAbsY AddressRef    -- $nnnn,Y | label,Y
  | OpZP AddressRef      -- $nn | label (musi być w ZP)
  | OpZPX AddressRef     -- $nn,X | label,X (musi być w ZP)
  | OpZPY AddressRef     -- $nn,Y | label,Y (musi być w ZP) 
  | OpIndX AddressRef    -- ($nn,X) | (label,X) (musi być w ZP)
  | OpIndY AddressRef    -- ($nn),Y | (label),Y (musi być w ZP)
  -- Akumulator (dla ROL A, LSR A itp. - nieużywane w przykładzie)
  -- OpAcc
  deriving (Show, Eq)

-- Użyjemy synonimów wzorców dla wygody użycia literałów w kodzie Asm
pattern Imm :: Word8 -> Operand
pattern Imm v = OpImm v

pattern AbsLit :: Word16 -> Operand
pattern AbsLit v = OpAbs (AddrLit16 v)

pattern AbsLabel :: Label -> Operand
pattern AbsLabel l = OpAbs (AddrLabel l)

pattern AbsXLit :: Word16 -> Operand
pattern AbsXLit v = OpAbsX (AddrLit16 v)

pattern AbsXLabel :: Label -> Operand
pattern AbsXLabel l = OpAbsX (AddrLabel l)

-- pattern ZPLit :: Word8 -> Operand
-- pattern ZPLit v = OpZP (AddrLit16 (fromIntegral v))

pattern ZPLabel :: Label -> Operand
pattern ZPLabel l = OpZP (AddrLabel l)

-- Zamiast pattern ZPLit
zpLit :: Word8 -> Operand
zpLit v = OpZP (AddrLit16 (fromIntegral v))


-- Można dodać więcej patternów dla ZPX, AbsY, IndX, IndY

-- --- Mnemoniki ---
data Mnemonic = LDA | STA | LDX | LDY | JMP | INX | RTS | ADC | SBC | 
                TAX | TAY | STX | STY | CMP | CPX | CPY | TXA | TYA | TXS | TSX 
  deriving (Show, Eq, Ord, Enum, Bounded) -- Ord potrzebne dla Map

data BranchMnemonic = BNE | BEQ | BCS | BCC | BMI | BPL | BVS | BVC
  deriving (Show, Eq, Ord, Enum, Bounded)

-- <<< ZMIENIONY SymbolicInstruction >>>
data SymbolicInstruction
  = S_LabelDef Label
  | S_Ins Mnemonic (Maybe Operand) -- Generyczna instrukcja (Maybe dla implikowanych)
  | S_Branch BranchMnemonic Label  -- Skoki warunkowe
  | S_Bytes [Word8]
  | S_Words [Word16]
  deriving (Show, Eq)

-- --- Stan Asemblera (bez zmian) ---
data AsmState = AsmState
  { asmPC     :: ProgramCounter
  , asmLabels :: Map.Map Label ProgramCounter
  , asmCode   :: [(ProgramCounter, SymbolicInstruction)]
  } deriving (Show)

initialAsmState :: ProgramCounter -> AsmState
initialAsmState startAddr = AsmState startAddr Map.empty []

-- --- Monada Asemblera (bez zmian) ---
newtype Asm a = Asm { unAsm :: State AsmState a }
  deriving (Functor, Applicative, Monad, MonadState AsmState)

-- --- Funkcje pomocnicze w monadzie ---

-- Funkcja pomocnicza do określania rozmiaru instrukcji
-- W przyszłości można to zastąpić mapą lub bardziej zaawansowaną logiką
getInstructionSize :: Mnemonic -> Maybe Operand -> Either String Word16
getInstructionSize m (Just op) = case (m, op) of
    (LDA, OpImm _)  -> Right 2; (LDA, OpZP _)   -> Right 2; (LDA, OpZPX _)  -> Right 2
    (LDA, OpAbs _)  -> Right 3; (LDA, OpAbsX _) -> Right 3; (LDA, OpAbsY _) -> Right 3
    (LDA, OpIndX _) -> Right 2; (LDA, OpIndY _) -> Right 2

    (STA, OpZP _)   -> Right 2; (STA, OpZPX _)  -> Right 2
    (STA, OpAbs _)  -> Right 3; (STA, OpAbsX _) -> Right 3; (STA, OpAbsY _) -> Right 3
    (STA, OpIndX _) -> Right 2; (STA, OpIndY _) -> Right 2

    (LDX, OpImm _)  -> Right 2; (LDX, OpZP _)   -> Right 2; (LDX, OpZPX _)  -> Right 2 -- Poprawka ZP -> ZPY
    (LDX, OpAbs _)  -> Right 3; (LDX, OpAbsY _) -> Right 3

    (LDY, OpImm _)  -> Right 2; (LDY, OpZP _)   -> Right 2; --(LDY, OpZPY _)  -> Right 2 -- Poprawka ZP -> ZPX
    (LDY, OpAbs _)  -> Right 3; (LDY, OpAbsX _) -> Right 3

    (JMP, OpAbs _)  -> Right 3


    (STX, _) -> case op of OpZP _ -> Right 2; OpAbs _ -> Right 3; _ -> Left "Invalid STX operand"
    (STY, _) -> case op of OpZP _ -> Right 2; OpZPX _ -> Right 2; OpAbs _ -> Right 3; _ -> Left "Invalid STY operand"
    (CPX, _) -> case op of OpImm _ -> Right 2; OpZP _ -> Right 2; OpAbs _ -> Right 3; _ -> Left "Invalid CPX operand"
    (CPY, _) -> case op of OpImm _ -> Right 2; OpZP _ -> Right 2; OpAbs _ -> Right 3; _ -> Left "Invalid CPY operand"

    -- TODO: JMP Indirect ($nnnn) - wymaga dodania OpInd do Operand
    -- (JMP, OpInd _) -> Right 3

    -- Inne mnemoniki...
    (_, _) -> Left $ "Unsupported addressing mode " ++ show op ++ " for " ++ show m

getInstructionSize m Nothing = case m of -- Tryby implikowane
    (INX) -> Right 1
    (RTS) -> Right 1
    (TAX) -> Right 1
    (TAY) -> Right 1
    (TXA) -> Right 1
    (TYA) -> Right 1
    (TXS) -> Right 1
    (TSX) -> Right 1
    -- Inne implikowane...
    _     -> Left $ "Mnemonic " ++ show m ++ " requires an operand."

emitGeneric :: SymbolicInstruction -> Either String Word16 -> Asm ()
emitGeneric _ (Left err) = error $ "Assembly Error: " ++ err -- Lepsza obsługa błędów potrzebna
emitGeneric instruction (Right size) = do
  pc <- gets asmPC
  modify' $ \s -> s { asmCode = (pc, instruction) : asmCode s, asmPC = pc + size }

-- Funkcja pomocnicza do emitowania instrukcji z operandem
emitIns :: Mnemonic -> Operand -> Asm ()
emitIns mnemonic operand = emitGeneric (S_Ins mnemonic (Just operand)) (getInstructionSize mnemonic (Just operand))

-- Funkcja pomocnicza do emitowania instrukcji implikowanych
emitImplied :: Mnemonic -> Asm ()
emitImplied mnemonic = emitGeneric (S_Ins mnemonic Nothing) (getInstructionSize mnemonic Nothing)

-- Funkcja pomocnicza do emitowania skoków warunkowych
emitBranch :: BranchMnemonic -> Label -> Asm ()
emitBranch mnemonic label = emitGeneric (S_Branch mnemonic label) (Right 2) -- Skoki są zawsze 2 bajty

-- Etykiety i dane bez zmian
l_ :: Label -> Asm ()
l_ lbl = do pc <- gets asmPC; labels <- gets asmLabels
            when (Map.member lbl labels) $ error $ "Label redefined: " ++ lbl
            modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
            emitGeneric (S_LabelDef lbl) (Right 0)

db :: [Word8] -> Asm ()
db bs = let size = fromIntegral $ length bs in when (size > 0) $ emitGeneric (S_Bytes bs) (Right size)

dw :: [Word16] -> Asm ()
dw ws = let size = fromIntegral (length ws) * 2 in when (size > 0) $ emitGeneric (S_Words ws) (Right size)

string :: String -> Asm ()
string str = let bytes = map (fromIntegral . ord) str; size = fromIntegral $ length bytes
             in when (size > 0) $ emitGeneric (S_Bytes bytes) (Right size)

-- --- Funkcje eDSL (teraz bardziej generyczne) ---

lda :: Operand -> Asm ()
lda = emitIns LDA

sta :: Operand -> Asm ()
sta = emitIns STA

ldx :: Operand -> Asm ()
ldx = emitIns LDX

ldy :: Operand -> Asm ()
ldy = emitIns LDY

jmp :: Operand -> Asm () -- Na razie obsługuje tylko Abs
jmp = emitIns JMP

inx :: Asm ()
inx = emitImplied INX

rts :: Asm ()
rts = emitImplied RTS

-- Skoki warunkowe
bne :: Label -> Asm ()
bne = emitBranch BNE

beq :: Label -> Asm ()
beq = emitBranch BEQ

tax :: Asm () -- <<< DODANO funkcję tax
tax = emitImplied TAX

tay :: Asm () -- <<< DODANO funkcję tay
tay = emitImplied TAY


txa :: Asm () -- <<< DODANO funkcję txa
txa = emitImplied TXA

tya :: Asm () -- <<< DODANO funkcję tya
tya = emitImplied TYA

txs :: Asm () -- <<< DODANO funkcję txs
txs = emitImplied TXS

tsx :: Asm () -- <<< DODANO funkcję tyx
tsx = emitImplied TSX

stx :: Operand -> Asm () -- <<< DODANO funkcję stx
stx = emitIns STX

sty :: Operand -> Asm () -- <<< DODANO funkcję sty
sty = emitIns STY

cpx :: Operand -> Asm () -- <<< DODANO funkcję cpx
cpx = emitIns CPX

cpy :: Operand -> Asm () -- <<< DODANO funkcję cpy
cpy = emitIns CPY

-- Można dodać resztę: bcs, bcc, bmi, bpl, bvs, bvc

-- --- Generowanie Kodu Binarnego (Pass 2) ---

generateBinary :: AsmState -> Either String [Word8]
generateBinary finalState =
  foldl' processInstruction (Right []) (reverse $ asmCode finalState)
  where
    labels = asmLabels finalState

    processInstruction :: Either String [Word8] -> (ProgramCounter, SymbolicInstruction) -> Either String [Word8]
    processInstruction (Left err) _ = Left err
    processInstruction (Right currentBytes) (pc, instruction) =
      (currentBytes ++) <$> instructionBytes pc instruction

    instructionBytes :: ProgramCounter -> SymbolicInstruction -> Either String [Word8]
    instructionBytes _  (S_LabelDef _) = Right []
    instructionBytes _  (S_Bytes bs)   = Right bs
    instructionBytes _  (S_Words ws)   = Right $ concatMap wordToBytesLE ws

    instructionBytes pc (S_Branch branchMnemonic targetLabel) = do
        offset <- calculateOffset pc targetLabel
        let opcode = case branchMnemonic of BNE -> 0xD0; BEQ -> 0xF0; BCS -> 0xB0; BCC -> 0x90; BMI -> 0x30; BPL -> 0x10; BVS -> 0x70; BVC -> 0x50
        Right [opcode, fromIntegral offset]

    -- Dopasowanie dla instrukcji implikowanych
    instructionBytes _ (S_Ins INX Nothing) = Right [0xE8]
    instructionBytes _ (S_Ins RTS Nothing) = Right [0x60]
    instructionBytes _ (S_Ins TAX Nothing) = Right [0xAA] -- <<< DODANO opcode TAX
    instructionBytes _ (S_Ins TAY Nothing) = Right [0xA8] -- <<< DODANO opcode TAY
    instructionBytes _ (S_Ins TXA Nothing) = Right [0x8A] -- <<< DODANO opcode TXA
    instructionBytes _ (S_Ins TYA Nothing) = Right [0x98] -- <<< DODANO opcode TYA
    instructionBytes _ (S_Ins TXS Nothing) = Right [0x9A] -- <<< DODANO opcode TXS
    instructionBytes _ (S_Ins TSX Nothing) = Right [0xBA] -- <<< DODANO opcode TSX
    instructionBytes _ (S_Ins mnemonic Nothing) = Left $ "Pass 2 Error: " ++ show mnemonic ++ " requires an operand."
    -- Inne implikowane...

    -- Dopasowanie dla instrukcji z operandami
    instructionBytes _  (S_Ins mnemonic (Just operand)) =
      case (mnemonic, operand) of
          -- LDA
          (LDA, OpImm val)   -> Right [0xA9, val]
          (LDA, OpZP ref)    -> resolveZP ref >>= \addr8 -> Right [0xA5, addr8]
          (LDA, OpZPX ref)   -> resolveZP ref >>= \addr8 -> Right [0xB5, addr8]
          (LDA, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0xAD, loByte addr16, hiByte addr16]
          (LDA, OpAbsX ref)  -> resolveAbs ref >>= \addr16 -> Right [0xBD, loByte addr16, hiByte addr16]
          (LDA, OpAbsY ref)  -> resolveAbs ref >>= \addr16 -> Right [0xB9, loByte addr16, hiByte addr16]
          (LDA, OpIndX ref)  -> resolveZP ref >>= \addr8 -> Right [0xA1, addr8]
          (LDA, OpIndY ref)  -> resolveZP ref >>= \addr8 -> Right [0xB1, addr8]
          -- STA
          (STA, OpZP ref)    -> resolveZP ref >>= \addr8 -> Right [0x85, addr8]
          (STA, OpZPX ref)   -> resolveZP ref >>= \addr8 -> Right [0x95, addr8]
          (STA, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0x8D, loByte addr16, hiByte addr16]
          (STA, OpAbsX ref)  -> resolveAbs ref >>= \addr16 -> Right [0x9D, loByte addr16, hiByte addr16]
          (STA, OpAbsY ref)  -> resolveAbs ref >>= \addr16 -> Right [0x99, loByte addr16, hiByte addr16]
          (STA, OpIndX ref)  -> resolveZP ref >>= \addr8 -> Right [0x81, addr8]
          (STA, OpIndY ref)  -> resolveZP ref >>= \addr8 -> Right [0x91, addr8]
          -- LDX
          (LDX, OpImm val)   -> Right [0xA2, val]
          (LDX, OpZP ref)    -> resolveZP ref >>= \addr8 -> Right [0xA6, addr8]
          (LDX, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0xAE, loByte addr16, hiByte addr16]
          -- LDY
          (LDY, OpImm val)   -> Right [0xA0, val]
          (LDY, OpZP ref)    -> resolveZP ref >>= \addr8 -> Right [0xA4, addr8]
          (LDY, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0xAC, loByte addr16, hiByte addr16]
          -- JMP
          (JMP, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0x4C, loByte addr16, hiByte addr16]
          -- STX
          (STX, OpZP ref)    -> resolveZP ref >>= \addr8 -> Right [0x86, addr8]
          (STX, OpZPY ref)   -> resolveZP ref >>= \addr8 -> Right [0x96, addr8] -- Używa OpZPY
          (STX, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0x8E, loByte addr16, hiByte addr16]
          -- STY
          (STY, OpZP ref)    -> resolveZP ref >>= \addr8 -> Right [0x84, addr8]
          (STY, OpZPX ref)   -> resolveZP ref >>= \addr8 -> Right [0x94, addr8] -- Używa OpZPX
          (STY, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0x8C, loByte addr16, hiByte addr16]
 
          (CPX, OpImm val)   -> Right [0xE0, val]
          (CPX, OpZP ref)    -> resolveZP ref >>= \addr8 -> Right [0xE4, addr8]
          (CPX, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0xEC, loByte addr16, hiByte addr16]
          -- CPY
          (CPY, OpImm val)   -> Right [0xC0, val]
          (CPY, OpZP ref)    -> resolveZP ref >>= \addr8 -> Right [0xC4, addr8]
          (CPY, OpAbs ref)   -> resolveAbs ref >>= \addr16 -> Right [0xCC, loByte addr16, hiByte addr16]
 
 
          (_, _) -> Left $ "Pass 2 Error: Unsupported instruction/operand combination: " ++ show mnemonic ++ " " ++ show operand


    -- Funkcja pomocnicza do konwersji Word16 na [Word8] (Little Endian)
    wordToBytesLE :: Word16 -> [Word8]
    wordToBytesLE w = [loByte w, hiByte w]

    -- Rozwiązuje adres i sprawdza, czy pasuje do strony zerowej
    resolveZP :: AddressRef -> Either String Word8
    resolveZP ref = do
        addr16 <- resolveAddress ref
        if addr16 <= 0xFF then
            Right (fromIntegral addr16)
        else
            Left $ "Pass 2 Error: Address " ++ formatRef ref ++ " ($" ++ showHex addr16 "" ++ ") does not fit in zero page."

    -- Rozwiązuje adres 16-bitowy
    resolveAbs :: AddressRef -> Either String Word16
    resolveAbs = resolveAddress

    -- Ogólna funkcja rozwiązująca adres (czy to literał czy etykieta)
    resolveAddress :: AddressRef -> Either String Word16
    resolveAddress (AddrLit16 lit) = Right lit
    resolveAddress (AddrLabel lbl) =
      case Map.lookup lbl labels of
        Just addr -> Right addr
        Nothing -> Left $ "Pass 2 Error: Unknown label '" ++ lbl ++ "'"

    -- Formatowanie referencji do adresu dla komunikatów błędów
    formatRef :: AddressRef -> String
    formatRef (AddrLit16 lit) = "$" ++ showHex lit ""
    formatRef (AddrLabel lbl) = "'" ++ lbl ++ "'"


    calculateOffset :: ProgramCounter -> Label -> Either String Int8
    calculateOffset currentPC targetLabel = do
        targetAddr <- case Map.lookup targetLabel labels of
                          Just addr -> Right addr
                          Nothing -> Left $ "Pass 2 Error: Unknown label in branch '" ++ targetLabel ++ "'"
        let offset = fromIntegral targetAddr - fromIntegral (currentPC + 2)
        if offset >= -128 && offset <= 127 then
            Right (fromIntegral offset :: Int8)
        else
            Left $ "Pass 2 Error: Branch target out of range for '" ++ targetLabel ++ "' at PC " ++ showHex currentPC "" ++ " (offset=" ++ show offset ++ ")"

    loByte :: Word16 -> Word8
    loByte = fromIntegral . (.&. 0xFF)

    hiByte :: Word16 -> Word8
    hiByte w = fromIntegral (w `shiftR` 8)


-- --- Uruchomienie Asemblera (bez zmian) ---

runAssembler :: ProgramCounter -> Asm () -> Either String ([Word8], Map.Map Label ProgramCounter)
runAssembler startAddr asmAction =
  let finalState = execState (unAsm asmAction) (initialAsmState startAddr)
  in case generateBinary finalState of
      Left err -> Left err
      Right binaryCode -> Right (binaryCode, asmLabels finalState)

-- --- Przykład Użycia (z nową składnią) ---

asc :: Char -> Word8
asc c = fromIntegral (fromEnum c)

createList :: AddressRef -> Asm ()
createList addr = do
    lda $ Imm 0x00
    sta $ OpAbs addr

listAdd :: AddressRef -> Word8 -> Asm ()
listAdd list element = do
  let list' = OpAbs list
  let element' = Imm element

  lda list'   -- list length -> A
  tax                -- A -> X
  inx                -- x + 1 -> x
  lda element'  -- Get the integer value of the element
  sta $ OpAbsX list  -- Store the element at the calculated index
  stx list'   -- Update the list length

listCopy :: AddressRef -> AddressRef -> Asm ()
listCopy src dst = do
    let src' = OpAbs src
    let dst' = OpAbs dst

    ldx $ Imm 0x00
    txa -- x -> a, set the list length to 0
    sta dst'
    l_ "copyList"
    cpx src'
    beq "endCopyList"
    inx
    lda $ OpAbsX src
    sta $ OpAbsX dst  -- Store the element at the calculated index
    txa
    sta dst' -- update the list length
    jmp $ AbsLabel "copyList"
    l_ "endCopyList"
    -- zapisz długość listy

mySimpleProgram :: Asm ()
mySimpleProgram = do
    l_ "start"
    lda $ zpLit 0x10
    sta $ AbsLit 0x0200
    
    let myList = AddrLit16 0x00c0
    let myList2 = AddrLit16 0x00d0
    let myList3 = AddrLit16 0x00e0

    createList myList
    createList myList2
    createList myList3

    listAdd myList 0x01
    listAdd myList 0x02
    listAdd myList 0x03
    listAdd myList 0x04
    
    listAdd myList2 $ asc 'a'
    listAdd myList2 $ asc 'b'
    listAdd myList2 $ asc 'c'

    listCopy myList myList3

    l_ "loop"
    inx -- Instrukcja implikowana
    -- Przykład użycia etykiety w trybie AbsX
    sta $ AbsXLabel "values"
    bne "loop"

    lda $ Imm 0xAA
    -- Przykład JMP do etykiety
    jmp $ AbsLabel "end_routine"

    rts -- Instrukcja implikowana

    l_ "message"
    -- Użycie strony zerowej z literałem
    ldx $ zpLit 0xFF -- Przykładowe użycie ZP
    string $ "Hello!" ++ ['\0']
    db [0x0D, 0x00]

    l_ "values"
    dw [0x1234, 0xABCD, 0xFFFF, 0x0000]
    db [1, 2, 3, 4, 5]

    l_ "end_routine"
    lda $ Imm 0x55
    sta $ AbsLit 0x0201
    l_ "createList"
    lda $ Imm 0x00
    sta $ AbsLit 0x0300
    rts

main :: IO ()
main = do
    let startAddress = 0x0000
    putStrLn $ "Assembling program starting at: $" ++ showHex startAddress ""
    case runAssembler startAddress mySimpleProgram of
      Left err -> putStrLn $ "Assembly failed: " ++ err
      Right (byteCode, labels) -> do
        putStrLn "\n--- Assembly Successful! ---"
        putStrLn "\nLabels Defined:"
        mapM_ (\(lbl, addr) -> putStrLn $ "  " ++ lbl ++ ": $" ++ showHex addr "") (Map.toList labels)

        putStrLn "\nGenerated ByteCode:"
        putStrLn $ formatHexBytes startAddress byteCode

-- Formatowanie bez zmian...
formatHexBytes :: Word16 -> [Word8] -> String
formatHexBytes startAddr bytes = unlines $ formatLines 16 $ zip [startAddr..] bytes
  where formatLines _ [] = []; formatLines n addrBytes = let (lineBytes, remainingBytes) = splitAt n addrBytes; lineAddr = fst $ head lineBytes; hexValues = unwords $ map (byteToHex . snd) lineBytes in (addrToHex lineAddr ++ ": " ++ hexValues) : formatLines n remainingBytes; byteToHex b = let s = showHex b "" in if length s == 1 then '0':s else s; addrToHex a = let s = showHex a "" in replicate (4 - length s) '0' ++ s