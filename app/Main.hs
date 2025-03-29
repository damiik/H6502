{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-} -- Dodane dla case w generateBinary

import Control.Monad.State.Strict
import Control.Monad (when) 
import Data.Word
import Data.Int (Int8) -- Potrzebne dla offsetów
import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl') -- Bardziej ścisła wersja foldl
import Numeric (showHex) -- Do wyświetlania

-- --- Typy pomocnicze ---

type Address = Word16
type Label = String
type ProgramCounter = Word16

-- Odwołanie do adresu - może być literałem lub etykietą
data AddressRef = AddrLiteral Word16 | AddrLabel Label deriving (Show, Eq)

-- Symboliczna reprezentacja instrukcji (przed rozwiązaniem etykiet)
data SymbolicInstruction
  = S_LabelDef Label           -- Tylko marker dla Pass 1 i listingu
  | S_LDA_Imm Word8            -- LDA #$nn      (Op: $A9, Size: 2)
  | S_STA_Abs AddressRef       -- STA $nnnn     (Op: $8D, Size: 3)
  | S_STA_Abs_X AddressRef     -- STA $nnnn,X   (Op: $9D, Size: 3)
  | S_JMP_Abs AddressRef       -- JMP $nnnn     (Op: $4C, Size: 3)
  | S_BNE Label                -- BNE label     (Op: $D0, Size: 2)
  | S_INX                      -- INX           (Op: $E8, Size: 1)
  | S_RTS                      -- RTS           (Op: $60, Size: 1)
  deriving (Show, Eq)

-- --- Stan Asemblera ---
data AsmState = AsmState
  { asmPC     :: ProgramCounter
  , asmLabels :: Map.Map Label ProgramCounter
  , asmCode   :: [(ProgramCounter, SymbolicInstruction)] -- Budowane w odwrotnej kolejności
  } deriving (Show)

initialAsmState :: ProgramCounter -> AsmState
initialAsmState startAddr = AsmState startAddr Map.empty []

-- --- Monada Asemblera ---
newtype Asm a = Asm { unAsm :: State AsmState a }
  deriving (Functor, Applicative, Monad, MonadState AsmState)

-- --- Funkcje pomocnicze w monadzie ---

-- Zgłoszenie błędu (bardzo proste - zatrzymuje obliczenia w Either)
-- Można by użyć ExceptT dla lepszej obsługi błędów
-- throwAsmError :: String -> Asm a -- W tej prostej wersji nie mamy jak zgłosić błędu w monadzie

-- "Emituj" symboliczną instrukcję (dodaj do listy i zaktualizuj PC)
emit :: SymbolicInstruction -> Word16 -> Asm ()
emit instruction size = do
  pc <- gets asmPC
  -- Dodajemy na początek listy dla efektywności
  modify' $ \s -> s { asmCode = (pc, instruction) : asmCode s, asmPC = pc + size }

-- Zdefiniuj etykietę w bieżącym miejscu
defineLabel :: Label -> Asm ()
defineLabel lbl = do
  pc <- gets asmPC
  labels <- gets asmLabels
  when (Map.member lbl labels) $ do
    -- W tej prostej monadzie nie możemy łatwo zgłosić błędu,
    -- można by użyć error lub bardziej zaawansowanej monady
    error $ "Label redefined: " ++ lbl -- Zatrzymanie programu
  modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
  -- Emitujemy też marker, żeby był w liście kodu
  emit (S_LabelDef lbl) 0 -- Etykieta nie zajmuje miejsca w kodzie binarnym

-- --- Funkcje eDSL dla Instrukcji 6502 ---

lda_imm :: Word8 -> Asm ()
lda_imm val = emit (S_LDA_Imm val) 2

sta_abs :: AddressRef -> Asm ()
sta_abs addrRef = emit (S_STA_Abs addrRef) 3

sta_abs_x :: AddressRef -> Asm ()
sta_abs_x addrRef = emit (S_STA_Abs_X addrRef) 3

jmp_abs :: AddressRef -> Asm ()
jmp_abs addrRef = emit (S_JMP_Abs addrRef) 3

bne :: Label -> Asm ()
bne targetLabel = emit (S_BNE targetLabel) 2

inx :: Asm ()
inx = emit S_INX 1

rts :: Asm ()
rts = emit S_RTS 1

-- --- Generowanie Kodu Binarnego (Pass 2 - funkcja czysta) ---

generateBinary :: AsmState -> Either String [Word8]
generateBinary finalState =
  -- Przetwarzamy listę w poprawnej kolejności (odwracamy listę zbudowaną przez :)
  -- Używamy foldl' i odwróconej listy dla akumulacji `Either`
  foldl' processInstruction (Right []) (reverse $ asmCode finalState)
  where
    labels = asmLabels finalState

    processInstruction :: Either String [Word8] -> (ProgramCounter, SymbolicInstruction) -> Either String [Word8]
    processInstruction (Left err) _ = Left err -- Propaguj błąd
    processInstruction (Right currentBytes) (pc, instruction) =
      (currentBytes ++) <$> instructionBytes pc instruction -- Dołącz bajty nowej instrukcji

    --Dołączamy nowe bajty NA POCZĄTEK akumulatora za pomocą `flip (++)` lub jawnie
    -- processInstruction (Right accumulatedBytesReversed) (pc, instruction) = do
    --   newBytes <- instructionBytes pc instruction
    --   Right (newBytes ++ accumulatedBytesReversed) -- Nowe bajty na początku
    --   -- Right $ newBytes ++ accumulatedBytesReversed -- Bardziej jawne

    instructionBytes :: ProgramCounter -> SymbolicInstruction -> Either String [Word8]
    instructionBytes _  (S_LabelDef _) = Right [] -- Etykiety nie generują bajtów
    instructionBytes _  (S_LDA_Imm val) = Right [0xA9, val]
    instructionBytes _  (S_STA_Abs addrRef) = resolveAddress addrRef >>= \addr -> Right [0x8D, loByte addr, hiByte addr]
    instructionBytes _  (S_STA_Abs_X addrRef) = resolveAddress addrRef >>= \addr -> Right [0x9D, loByte addr, hiByte addr]
    instructionBytes _  (S_JMP_Abs addrRef) = resolveAddress addrRef >>= \addr -> Right [0x4C, loByte addr, hiByte addr]
    instructionBytes pc (S_BNE targetLabel) = calculateOffset pc targetLabel >>= \offset -> Right [0xD0, fromIntegral offset] -- Opcode BNE = $D0
    instructionBytes _  S_INX = Right [0xE8]
    instructionBytes _  S_RTS = Right [0x60]

    resolveAddress :: AddressRef -> Either String Word16
    resolveAddress (AddrLiteral lit) = Right lit
    resolveAddress (AddrLabel lbl) =
      case Map.lookup lbl labels of
        Just addr -> Right addr
        Nothing -> Left $ "Pass 2 Error: Unknown label '" ++ lbl ++ "'"

    calculateOffset :: ProgramCounter -> Label -> Either String Int8
    calculateOffset currentPC targetLabel = do
      targetAddr <- case Map.lookup targetLabel labels of
                      Just addr -> Right addr
                      Nothing -> Left $ "Pass 2 Error: Unknown label in branch '" ++ targetLabel ++ "'"
      -- Offset jest względem adresu *następnej* instrukcji (PC + rozmiar branch)
      let offset = fromIntegral targetAddr - fromIntegral (currentPC + 2)
      if offset >= -128 && offset <= 127 then
          Right (fromIntegral offset :: Int8)
      else
          Left $ "Pass 2 Error: Branch target out of range for '" ++ targetLabel ++ "' at PC " ++ showHex currentPC "" ++ " (offset=" ++ show offset ++ ")"

    loByte :: Word16 -> Word8
    loByte = fromIntegral . (.&. 0xFF)

    hiByte :: Word16 -> Word8
    hiByte w = fromIntegral (w `shiftR` 8)


-- --- Uruchomienie Asemblera ---

runAssembler :: ProgramCounter -> Asm () -> Either String ([Word8], Map.Map Label ProgramCounter)
runAssembler startAddr asmAction =
  let finalState = execState (unAsm asmAction) (initialAsmState startAddr)
  in case generateBinary finalState of
      Left err -> Left err
      Right binaryCode -> Right (binaryCode, asmLabels finalState)

-- --- Przykład Użycia ---

mySimpleProgram :: Asm ()
mySimpleProgram = do
    defineLabel "start"
    lda_imm 0x10
    sta_abs (AddrLiteral 0x0020) -- Zapisz do pamięci $0200

    defineLabel "loop"
    inx
    sta_abs_x (AddrLabel "data_area") -- Zapisz A do data_area + X
    bne "loop"                 -- Wróć do loop jeśli flaga Z=0 (będzie pętla przez jakiś czas)

    lda_imm 0xAA
    jmp_abs (AddrLabel "end_routine") -- Skocz do etykiety zdefiniowanej później

    rts                        -- Ten RTS nigdy nie zostanie osiągnięty

    defineLabel "data_area"    -- Etykieta bez kodu (adres dla danych)
    -- Można by dodać dyrektywy jak .byte, .word itp.

    defineLabel "end_routine"
    lda_imm 0x55
    sta_abs (AddrLiteral 0x0021)
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

-- Funkcja pomocnicza do ładnego wyświetlania hex
formatHexBytes :: Word16 -> [Word8] -> String
formatHexBytes startAddr bytes = unlines $ formatLines 16 $ zip [startAddr..] bytes
  where
    formatLines :: Int -> [(Word16, Word8)] -> [String]
    formatLines _ [] = []
    formatLines n addrBytes =
      let (lineBytes, remainingBytes) = splitAt n addrBytes
          lineAddr = fst $ head lineBytes
          hexValues = unwords $ map (byteToHex . snd) lineBytes
      in (addrToHex lineAddr ++ ": " ++ hexValues) : formatLines n remainingBytes

    byteToHex :: Word8 -> String
    byteToHex b = let s = showHex b "" in if length s == 1 then '0':s else s

    addrToHex :: Word16 -> String
    addrToHex a = let s = showHex a "" in replicate (4 - length s) '0' ++ s