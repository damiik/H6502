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
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import GHC.Arr (cmpArray)
import GHC.Read (list)
import qualified Data.Foldable as String
-- Usunięto niepotrzebny import Op z Contravariant
-- import Data.Functor.Contravariant (Op(Op))

-- --- Typy pomocnicze (bez zmian) ---
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

-- --- Stan Asemblera (dodano licznik makr) ---
data AsmState = AsmState
  { asmPC            :: ProgramCounter
  , asmLabels        :: Map.Map Label ProgramCounter
  , asmCode          :: [(ProgramCounter, SymbolicInstruction)]
  , asmMacroCounter  :: Int -- <<< NOWY LICZNIK DLA UNIKALNYCH ETYKIET MAKRA
  } deriving (Show)

initialAsmState :: ProgramCounter -> AsmState
initialAsmState startAddr = AsmState startAddr Map.empty [] 0 -- <<< Inicjalizacja licznika

-- --- Monada Asemblera (bez zmian) ---
newtype Asm a = Asm { unAsm :: State AsmState a }
  deriving (Functor, Applicative, Monad, MonadState AsmState)

-- --- Funkcje pomocnicze w monadzie ---

-- getInstructionSize, emitGeneric, emitIns, emitImplied, emitBranch (bez zmian)
getInstructionSize :: Mnemonic -> Maybe Operand -> Either String Word16
getInstructionSize m (Just op) = case (m, op) of
    (LDA, OpImm _)  -> Right 2; (LDA, OpZP _)   -> Right 2; (LDA, OpZPX _)  -> Right 2
    (LDA, OpAbs _)  -> Right 3; (LDA, OpAbsX _) -> Right 3; (LDA, OpAbsY _) -> Right 3
    (LDA, OpIndX _) -> Right 2; (LDA, OpIndY _) -> Right 2
    (STA, OpZP _)   -> Right 2; (STA, OpZPX _)  -> Right 2
    (STA, OpAbs _)  -> Right 3; (STA, OpAbsX _) -> Right 3; (STA, OpAbsY _) -> Right 3
    (STA, OpIndX _) -> Right 2; (STA, OpIndY _) -> Right 2
    (LDX, OpImm _)  -> Right 2; (LDX, OpZP _)   -> Right 2; (LDX, OpZPX _)  -> Right 2 -- Było ZPY
    (LDX, OpAbs _)  -> Right 3; (LDX, OpAbsY _) -> Right 3
    (LDY, OpImm _)  -> Right 2; (LDY, OpZP _)   -> Right 2; (LDY, OpZPX _)  -> Right 2 -- Było ZPY
    (LDY, OpAbs _)  -> Right 3; (LDY, OpAbsX _) -> Right 3
    (STX, OpZP _)   -> Right 2; (STX, OpZPY _)  -> Right 2; (STX, OpAbs _)  -> Right 3
    (STY, OpZP _)   -> Right 2; (STY, OpZPX _)  -> Right 2; (STY, OpAbs _)  -> Right 3
    (CPX, OpImm _)  -> Right 2; (CPX, OpZP _)   -> Right 2; (CPX, OpAbs _)  -> Right 3
    (CPY, OpImm _)  -> Right 2; (CPY, OpZP _)   -> Right 2; (CPY, OpAbs _)  -> Right 3
    (JMP, OpAbs _)  -> Right 3
    (_, _) -> Left $ "Unsupported/TODO addressing mode " ++ show op ++ " for " ++ show m
getInstructionSize m Nothing = case m of
    (INX) -> Right 1; (RTS) -> Right 1; (TAX) -> Right 1; (TAY) -> Right 1
    (TXA) -> Right 1; (TYA) -> Right 1; (TXS) -> Right 1; (TSX) -> Right 1
    _     -> Left $ "Mnemonic " ++ show m ++ " requires an operand or is TODO (implied)."

emitGeneric :: SymbolicInstruction -> Either String Word16 -> Asm ()
emitGeneric _ (Left err) = error $ "Assembly Error (emitGeneric): " ++ err
emitGeneric instruction (Right size) = do
  pc <- gets asmPC
  modify' $ \s -> s { asmCode = (pc, instruction) : asmCode s, asmPC = pc + size }
emitIns :: Mnemonic -> Operand -> Asm (); emitIns m op = emitGeneric (S_Ins m (Just op)) (getInstructionSize m (Just op))
emitImplied :: Mnemonic -> Asm (); emitImplied m = emitGeneric (S_Ins m Nothing) (getInstructionSize m Nothing)
emitBranch :: BranchMnemonic -> Label -> Asm (); emitBranch m l = emitGeneric (S_Branch m l) (Right 2)

l_ :: Label -> Asm ()
l_ lbl = do pc <- gets asmPC; labels <- gets asmLabels
            when (Map.member lbl labels) $ error $ "Label redefined: " ++ lbl
            modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
            emitGeneric (S_LabelDef lbl) (Right 0)

db :: [Word8] -> Asm (); db bs = let size = fromIntegral $ length bs in when (size > 0) $ emitGeneric (S_Bytes bs) (Right size)
dw :: [Word16] -> Asm (); dw ws = let size = fromIntegral (length ws) * 2 in when (size > 0) $ emitGeneric (S_Words ws) (Right size)
string :: String -> Asm (); string str = let bytes = map (fromIntegral . ord) str; size = fromIntegral $ length bytes in when (size > 0) $ emitGeneric (S_Bytes bytes) (Right size)

-- <<< NOWA FUNKCJA POMOCNICZA DO GENEROWANIA ETYKIET >>>
-- Generuje unikalną etykietę na podstawie nazwy bazowej i licznika makr
makeUniqueLabel :: () -> Asm Label
makeUniqueLabel _ = do
    count <- gets asmMacroCounter
    -- Zwiększamy licznik globalnie 
    modify' $ \s -> s { asmMacroCounter = count + 1 }
    -- Создаем уникальное имя
    return $ "_lbl_" ++ show count

-- --- Funkcje eDSL (bez zmian, oprócz makr) ---
lda :: Operand -> Asm (); lda = emitIns LDA
sta :: Operand -> Asm (); sta = emitIns STA
ldx :: Operand -> Asm (); ldx = emitIns LDX
ldy :: Operand -> Asm (); ldy = emitIns LDY
jmp :: Operand -> Asm (); jmp = emitIns JMP
inx :: Asm (); inx = emitImplied INX
rts :: Asm (); rts = emitImplied RTS; 
bne :: Label -> Asm (); bne = emitBranch BNE
beq :: Label -> Asm (); beq = emitBranch BEQ 
bcs :: Label -> Asm (); bcs = emitBranch BCS
bcc :: Label -> Asm (); bcc = emitBranch BCC
tax :: Asm (); tax = emitImplied TAX
tay :: Asm (); tay = emitImplied TAY 
txa :: Asm (); txa = emitImplied TXA
tya :: Asm (); tya = emitImplied TYA
txs :: Asm (); txs = emitImplied TXS
tsx :: Asm (); tsx = emitImplied TSX
stx :: Operand -> Asm (); stx = emitIns STX
sty :: Operand -> Asm (); sty = emitIns STY
cpx :: Operand -> Asm (); cpx = emitIns CPX

-- Generowanie Kodu Binarnego (Pass 2 - bez zmian w logice, tylko dodane opcody) ---
generateBinary :: AsmState -> Either String [Word8]
generateBinary finalState = foldl' processInstruction (Right []) (reverse $ asmCode finalState)
  where labels = asmLabels finalState
        processInstruction (Left err) _ = Left err; processInstruction (Right cur) (pc, i) = (cur ++) <$> instructionBytes pc i
        instructionBytes _ (S_LabelDef _) = Right []; instructionBytes _ (S_Bytes bs) = Right bs; instructionBytes _ (S_Words ws) = Right $ concatMap wordToBytesLE ws
        instructionBytes pc (S_Branch bm tl) = calculateOffset pc tl >>= \off -> Right [branchOpcode bm, fromIntegral off]
        instructionBytes _ (S_Ins m Nothing) = maybe (Left $ errImp m) Right (impliedOpcode m)
        instructionBytes _ (S_Ins m (Just op)) = maybe (Left $ errComb m op) Right (operandOpcode m op)

        errImp m = "Pass 2 Error: " ++ show m ++ " requires an operand or is not a known implied instruction."
        errComb m op = "Pass 2 Error: Unsupported instruction/operand combination: " ++ show m ++ " " ++ show op

        impliedOpcode :: Mnemonic -> Maybe [Word8]
        impliedOpcode = \case INX -> Just [0xE8]; RTS -> Just [0x60]; TAX -> Just [0xAA]; TAY -> Just [0xA8]
                              TXA -> Just [0x8A]; TYA -> Just [0x98]; TXS -> Just [0x9A]; TSX -> Just [0xBA]; _ -> Nothing

        branchOpcode = \case BNE -> 0xD0; BEQ -> 0xF0; BCS -> 0xB0; BCC -> 0x90; BMI -> 0x30; BPL -> 0x10; BVS -> 0x70; BVC -> 0x50

        operandOpcode :: Mnemonic -> Operand -> Maybe [Word8]
        operandOpcode m o = case (m, o) of
            (LDA, OpImm v)  -> Just [0xA9, v]; (LDA, OpZP r)   -> resolveZP r >>= \a -> Just [0xA5, a]; (LDA, OpZPX r)  -> resolveZP r >>= \a -> Just [0xB5, a]
            (LDA, OpAbs r)  -> resolveAbs r >>= \a -> Just [0xAD, lo a, hi a]; (LDA, OpAbsX r) -> resolveAbs r >>= \a -> Just [0xBD, lo a, hi a]; (LDA, OpAbsY r) -> resolveAbs r >>= \a -> Just [0xB9, lo a, hi a]
            (LDA, OpIndX r) -> resolveZP r >>= \a -> Just [0xA1, a]; (LDA, OpIndY r) -> resolveZP r >>= \a -> Just [0xB1, a]
            (STA, OpZP r)   -> resolveZP r >>= \a -> Just [0x85, a]; (STA, OpZPX r)  -> resolveZP r >>= \a -> Just [0x95, a]
            (STA, OpAbs r)  -> resolveAbs r >>= \a -> Just [0x8D, lo a, hi a]; (STA, OpAbsX r) -> resolveAbs r >>= \a -> Just [0x9D, lo a, hi a]; (STA, OpAbsY r) -> resolveAbs r >>= \a -> Just [0x99, lo a, hi a]
            (STA, OpIndX r) -> resolveZP r >>= \a -> Just [0x81, a]; (STA, OpIndY r) -> resolveZP r >>= \a -> Just [0x91, a]
            (LDX, OpImm v)  -> Just [0xA2, v]; (LDX, OpZP r)   -> resolveZP r >>= \a -> Just [0xA6, a]; -- (LDX, OpZPY r) -> resolveZP r >>= \a -> Just [0xB6, a] -- Requires OpZPY
            (LDX, OpAbs r)  -> resolveAbs r >>= \a -> Just [0xAE, lo a, hi a]; -- (LDX, OpAbsY r)-> resolveAbs r >>= \a -> Just [0xBE, lo a, hi a] -- Requires OpAbsY
            (LDY, OpImm v)  -> Just [0xA0, v]; (LDY, OpZP r)   -> resolveZP r >>= \a -> Just [0xA4, a]; -- (LDY, OpZPX r) -> resolveZP r >>= \a -> Just [0xB4, a] -- Requires OpZPX
            (LDY, OpAbs r)  -> resolveAbs r >>= \a -> Just [0xAC, lo a, hi a]; -- (LDY, OpAbsX r)-> resolveAbs r >>= \a -> Just [0xBC, lo a, hi a] -- Requires OpAbsX
            (STX, OpZP r)   -> resolveZP r >>= \a -> Just [0x86, a]; (STX, OpZPY r)  -> resolveZP r >>= \a -> Just [0x96, a]; (STX, OpAbs r) -> resolveAbs r >>= \a -> Just [0x8E, lo a, hi a]
            (STY, OpZP r)   -> resolveZP r >>= \a -> Just [0x84, a]; (STY, OpZPX r)  -> resolveZP r >>= \a -> Just [0x94, a]; (STY, OpAbs r) -> resolveAbs r >>= \a -> Just [0x8C, lo a, hi a]
            (CPX, OpImm v)  -> Just [0xE0, v]; (CPX, OpZP r)   -> resolveZP r >>= \a -> Just [0xE4, a]; (CPX, OpAbs r) -> resolveAbs r >>= \a -> Just [0xEC, lo a, hi a]
            (CPY, OpImm v)  -> Just [0xC0, v]; (CPY, OpZP r)   -> resolveZP r >>= \a -> Just [0xC4, a]; (CPY, OpAbs r) -> resolveAbs r >>= \a -> Just [0xCC, lo a, hi a]
            (JMP, OpAbs r)  -> resolveAbs r >>= \a -> Just [0x4C, lo a, hi a]
            _ -> Nothing -- Kombinacja nieobsługiwana

        -- Poniższe funkcje są teraz używane wewnątrz operandOpcode i muszą zwracać Maybe zamiast Either
        resolveZP :: AddressRef -> Maybe Word8; resolveZP r = resolveAddress r >>= \a -> if a <= 0xFF then Just (fromIntegral a) else Nothing
        resolveAbs :: AddressRef -> Maybe Word16; resolveAbs = resolveAddress
        resolveAddress :: AddressRef -> Maybe Word16
        resolveAddress (AddrLit16 l) = Just l
        resolveAddress (AddrLabel l) = Map.lookup l labels

        -- Ta funkcja nadal zwraca Either, bo jest używana bezpośrednio w `instructionBytes` dla S_Branch
        calculateOffset :: ProgramCounter -> Label -> Either String Int8
        calculateOffset pc tl = do targetAddr <- maybe (Left $ unknownLbl tl) Right (Map.lookup tl labels)
                                   let offset = fromIntegral targetAddr - fromIntegral (pc + 2)
                                   if offset >= -128 && offset <= 127 then Right (fromIntegral offset) else Left (branchRange tl pc offset)
        unknownLbl l = "Pass 2 Error: Unknown label in branch '" ++ l ++ "'"
        branchRange l pc o = "Pass 2 Error: Branch target out of range for '" ++ l ++ "' at PC " ++ hx pc ++ " (offset=" ++ show o ++ ")"

        lo = loByte; hi = hiByte; hx a = showHex a ""
        wordToBytesLE w = [lo w, hi w]; loByte = fromIntegral . (.&. 0xFF); hiByte w = fromIntegral (w `shiftR` 8)



-- --- ZMODYFIKOWANE Funkcje-Makra ---

ifNotEqThen :: Asm () -> Asm ()
ifNotEqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel                 -- Jeśli równe, pomiń blok
    thenBlock                    -- Wykonaj blok "then"
    l_ endLabel                  -- Koniec bloku warunkowego


ifEqThen :: Asm () -> Asm ()
ifEqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel                 -- Jeśli równe, pomiń blok
    thenBlock                    -- Wykonaj blok "then"
    l_ endLabel                  -- Koniec bloku warunkowego

ifCarryThen :: Asm () -> Asm ()
ifCarryThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcc endLabel                 -- Jeśli zero, pomiń blok
    thenBlock                    -- Wykonaj blok "then"
    l_ endLabel                  -- Koniec bloku warunkowego

ifNotCarryThen :: Asm () -> Asm ()
ifNotCarryThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcs endLabel                 -- Jeśli nie zero, pomiń blok
    thenBlock                    -- Wykonaj blok "then"
    l_ endLabel                  -- Koniec bloku warunkowego

whileEqDo :: Asm () -> Asm () -> Asm ()
whileEqDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bne endLabel                 -- Jeśli równe, zakończ pętlę
    doBlock                      -- Wykonaj blok "do"
    jmp $ AbsLabel startLabel     -- Skocz do początku pętli
    l_ endLabel                  -- Koniec pętli

whileNotEqDo :: Asm () -> Asm () -> Asm ()
whileNotEqDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    beq endLabel                 -- Jeśli różne, zakończ pętlę
    doBlock                      -- Wykonaj blok "do"
    jmp $ AbsLabel startLabel     -- Skocz do początku pętli
    l_ endLabel                  -- Koniec pętli

whileCarryDo :: Asm () -> Asm () -> Asm ()
whileCarryDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcc endLabel                 -- Jeśli zero, zakończ pętlę
    doBlock                      -- Wykonaj blok "do"
    jmp $ AbsLabel startLabel     -- Skocz do początku pętli
    l_ endLabel                  -- Koniec pętli  

whileNotCarryDo :: Asm () -> Asm () -> Asm ()
whileNotCarryDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcs endLabel                 -- Jeśli nie zero, zakończ pętlę
    doBlock                      -- Wykonaj blok "do"
    jmp $ AbsLabel startLabel     -- Skocz do początku pętli
    l_ endLabel                  -- Koniec pętli



asc :: Char -> Word8
asc c = fromIntegral (fromEnum c)

-- W createList nie ma etykiet, więc bez zmian
createList :: AddressRef -> Asm ()
createList addr = do
    lda $ Imm 0x00
    sta $ OpAbs addr

-- W listAdd nie ma etykiet, więc bez zmian
listAdd :: AddressRef -> Word8 -> Asm ()
listAdd l element = do
  let list' = OpAbs l -- Zmienna przechowująca długość listy
  lda list'
  tax
  inx
  lda $ Imm element
  sta $ OpAbsX l  -- Używa X, który jest długością+1
  stx list' -- Zapisz nową długość


listForEach :: AddressRef -> (Word8 -> Asm ()) -> Asm ()
listForEach l action = do
    let accumulatedValue = 0
    ldx $ Imm 0x00
    whileNotEqDo (cpx $ OpAbs l) $ do
        inx
        lda $ OpAbsX l -- Odczytaj element[X] z listy
        action $ fromIntegral (accumulatedValue + 1) -- Przykład użycia akumulowanej wartości

-- --- Zmodyfikowana funkcja listCopy ---
listCopy :: AddressRef -> AddressRef -> Asm ()
listCopy src dst = do
    listForEach src $ \_ -> do -- x is the index, a is the element
        sta $ OpAbsX dst
        txa
        sta $ OpAbs dst -- Zapisz nową długość listy docelowej pod adresem absolutnym dst


stringAsList :: String -> Asm ()
stringAsList s = do
    db [fromIntegral $ String.length s] -- Dodaj długość łańcucha
    let str = map (fromIntegral . ord) s
    db str
    --db [0x00] -- Dodaj terminator 0x00 na końcu

-- listCopy używa teraz makeUniqueLabel
-- listCopy :: AddressRef -> AddressRef -> Asm ()
-- listCopy src dst = do
--     -- <<< Generuj unikalne etykiety PRZED ich użyciem >>>
--     copyLoopLabel <- makeUniqueLabel()
--     endCopyLabel  <- makeUniqueLabel()

--     let src' = OpAbs src
--     let dst' = OpAbs dst

--     ldx $ Imm 0x00
--     txa
--     sta dst' -- Zeruj długość listy docelowej
--     l_ copyLoopLabel     -- <<< Użyj unikalnej etykiety pętli
--     cpx src'             -- Porównaj X z długością listy źródłowej
--     beq endCopyLabel     -- <<< Skocz do unikalnej etykiety końca
--     inx                  -- X = X + 1 (indeks do odczytu/zapisu)
--     lda $ OpAbsX src     -- Odczytaj element[X] ze źródła
--     sta $ OpAbsX dst     -- Zapisz element[X] do celu
--     txa                  -- A = X (aktualna długość)
--     sta dst'             -- Zapisz nową długość listy docelowej
--     jmp $ AbsLabel copyLoopLabel -- <<< Skocz do unikalnej etykiety pętli
--     l_ endCopyLabel      -- <<< Zdefiniuj unikalną etykietę końca





-- listCopy :: AddressRef -> AddressRef -> Asm ()
-- listCopy src dst = do

--     let src' = OpAbs src
--     let dst' = OpAbs dst
--     ldx $ Imm 0x00         -- X = 0 (indeks startowy)
--     txa                    -- A = X
--     sta dst'               -- Zeruj długość listy docelowej
--     whileNotEqDo (cpx src') $ do  -- Warunek: porównaj X z długością src'
--         inx                       -- X = X + 1 (indeks do odczytu/zapisu)
--         lda $ OpAbsX src          -- Odczytaj element[X] ze źródła
--         sta $ OpAbsX dst          -- Zapisz element[X] do celu
--         txa                       -- A = X (aktualna długość)
--         sta dst'                  -- Zapisz nową długość listy docelowej



-- --- Uruchomienie Asemblera (bez zmian) ---
runAssembler :: ProgramCounter -> Asm () -> Either String ([Word8], Map.Map Label ProgramCounter)
runAssembler startAddr asmAction = let finalState = execState (unAsm asmAction) (initialAsmState startAddr)
                                   in generateBinary finalState >>= \code -> Right (code, asmLabels finalState)


-- --- Przykład Użycia (z wielokrotnym użyciem listCopy) ---

mySimpleProgram :: Asm ()
mySimpleProgram = do
    l_ "start"
    -- Inicjalizacja list
    -- let myList1 = AddrLabel "lista1"
    let myList2 = AddrLabel "lista2"
    let myList3 = AddrLabel "lista3" -- Obszar na listę 3
    let myList4 = AddrLabel "lista4" -- Obszar na listę 4

    -- createList myList1
    createList myList2
    createList myList3
    createList myList4

    -- Dodaj elementy do list
    -- listAdd myList1 (asc 'H')
    -- listAdd myList1 (asc 'i')
    -- listAdd myList1 (asc '!')

    listAdd myList2 0x10
    listAdd myList2 0x20
    listAdd myList2 0x30
    listAdd myList2 0x40

    -- Kopiuj listy wielokrotnie
    listCopy (AddrLabel "lista1") myList3   -- Pierwsze wywołanie listCopy
    listCopy myList2 myList4  -- Drugie wywołanie listCopy

    -- Inny kod...
    lda $ Imm 0x00
    l_ "forever"
    jmp $ AbsLabel "forever"

    l_ "lista1"
    stringAsList "Hello World!"

    l_ "lista2"
    db [0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] -- Padding  

    l_ "lista3"
    db [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] -- Padding

    l_ "lista4"
    db [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] -- Padding

     

main :: IO ()
main = do
    let startAddress = 0x8000 -- Typowy adres startowy dla programów BASIC/ML
    putStrLn $ "Assembling program starting at: $" ++ showHex startAddress ""
    case runAssembler startAddress mySimpleProgram of
      Left err -> putStrLn $ "Assembly failed: \n" ++ err
      Right (byteCode, labels) -> do
        putStrLn "\n--- Assembly Successful! ---"
        putStrLn "\nLabels Defined:"
        mapM_ (\(lbl, addr) -> putStrLn $ "  " ++ lbl ++ "= $" ++ showHex addr "") (Map.toList labels)
        -- Wypiszmy tylko pierwsze ~128 bajtów dla zwięzłości
        putStrLn $ "\nGenerated ByteCode (first " ++ show (min 128 (length byteCode)) ++ " bytes):"
        putStrLn $ formatHexBytes startAddress byteCode -- Usunięto `take 128`

-- Formatowanie bez zmian...
formatHexBytes :: Word16 -> [Word8] -> String
formatHexBytes startAddr bytes = unlines $ formatLines 16 $ zip [startAddr..] bytes
  where formatLines _ [] = []; formatLines n addrBytes = let (lineBytes, remainingBytes) = splitAt n addrBytes; lineAddr = fst $ head lineBytes; hexValues = unwords $ map (byteToHex . snd) lineBytes in (addrToHex lineAddr ++ ": " ++ hexValues) : formatLines n remainingBytes; byteToHex b = let s = showHex b "" in if length s == 1 then '0':s else s; addrToHex a = let s = showHex a "" in replicate (4 - length s) '0' ++ s