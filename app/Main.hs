{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.State.Strict
import Control.Monad (when)
import Data.Word
import Data.Int (Int8)
import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Numeric (showHex)
import Data.Char (ord) -- <<< Potrzebne do konwersji String na bajty

-- --- Typy pomocnicze ---

type Address = Word16
type Label = String
type ProgramCounter = Word16

data AddressRef = AddrLiteral Word16 | AddrLabel Label deriving (Show, Eq)

-- Symboliczna reprezentacja instrukcji i danych
data SymbolicInstruction
  = S_LabelDef Label
  | S_LDA_Imm Word8
  | S_STA_Abs AddressRef
  | S_STA_Abs_X AddressRef
  | S_JMP_Abs AddressRef
  | S_BNE Label
  | S_INX
  | S_RTS
  -- <<< NOWE DYREKTYWY DANYCH >>>
  | S_Bytes [Word8]   -- .byte b1, b2, ...
  | S_Words [Word16]  -- .word w1, w2, ... (little-endian)
  | S_String String   -- .string "..." (jako bajty ASCII)
  deriving (Show, Eq)

-- --- Stan Asemblera ---
data AsmState = AsmState
  { asmPC     :: ProgramCounter
  , asmLabels :: Map.Map Label ProgramCounter
  , asmCode   :: [(ProgramCounter, SymbolicInstruction)]
  } deriving (Show)

initialAsmState :: ProgramCounter -> AsmState
initialAsmState startAddr = AsmState startAddr Map.empty []

-- --- Monada Asemblera ---
newtype Asm a = Asm { unAsm :: State AsmState a }
  deriving (Functor, Applicative, Monad, MonadState AsmState)

-- --- Funkcje pomocnicze w monadzie ---

emit :: SymbolicInstruction -> Word16 -> Asm ()
emit instruction size = do
  pc <- gets asmPC
  modify' $ \s -> s { asmCode = (pc, instruction) : asmCode s, asmPC = pc + size }

l_ :: Label -> Asm ()
l_ lbl = do
  pc <- gets asmPC
  labels <- gets asmLabels
  when (Map.member lbl labels) $
    error $ "Label redefined: " ++ lbl
  modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
  emit (S_LabelDef lbl) 0

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

-- <<< NOWE FUNKCJE eDSL DLA DYREKTYW DANYCH >>>

-- Dyrektywa .byte b1, b2, ...
db :: [Word8] -> Asm ()
db bs = do
    let size = fromIntegral $ length bs -- Rozmiar w bajtach
    when (size > 0) $ emit (S_Bytes bs) size

-- Dyrektywa .word w1, w2, ... (little-endian)
dw :: [Word16] -> Asm ()
dw ws = do
    let size = fromIntegral (length ws) * 2 -- Każde słowo to 2 bajty
    when (size > 0) $ emit (S_Words ws) size

-- Dyrektywa .string "..." (ASCII)
string :: String -> Asm ()
string str = do
    -- Konwertujemy String na listę Word8 (ASCII)
    let bytes = map (fromIntegral . ord) str
    let size = fromIntegral $ length bytes -- Każdy znak to 1 bajt
    when (size > 0) $ emit (S_Bytes bytes) size -- Możemy reużyć S_Bytes!
    -- Alternatywnie, moglibyśmy trzymać S_String i konwertować w Pass 2,
    -- ale konwersja tutaj jest równie dobra i upraszcza Pass 2.

-- --- Generowanie Kodu Binarnego (Pass 2 - funkcja czysta) ---

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
    instructionBytes _  (S_LDA_Imm val) = Right [0xA9, val]
    instructionBytes _  (S_STA_Abs addrRef) = resolveAddress addrRef >>= \addr -> Right [0x8D, loByte addr, hiByte addr]
    instructionBytes _  (S_STA_Abs_X addrRef) = resolveAddress addrRef >>= \addr -> Right [0x9D, loByte addr, hiByte addr]
    instructionBytes _  (S_JMP_Abs addrRef) = resolveAddress addrRef >>= \addr -> Right [0x4C, loByte addr, hiByte addr]
    instructionBytes pc (S_BNE targetLabel) = calculateOffset pc targetLabel >>= \offset -> Right [0xD0, fromIntegral offset]
    instructionBytes _  S_INX = Right [0xE8]
    instructionBytes _  S_RTS = Right [0x60]
    -- <<< OBSŁUGA NOWYCH DYREKTYW W PASS 2 >>>
    instructionBytes _  (S_Bytes bs) = Right bs -- Po prostu zwróć listę bajtów
    instructionBytes _  (S_Words ws) = Right $ concatMap wordToBytesLE ws -- Konwertuj każde słowo na [lo, hi] i połącz
    instructionBytes _  (S_String _) = error "S_String should have been converted to S_Bytes in emitString"
                                      -- Powyższy error nie powinien wystąpić, jeśli emitString działa poprawnie.
                                      -- Jeśli zdecydowalibyśmy się trzymać S_String do Pass 2, obsługa byłaby tutaj:
                                      -- Right $ map (fromIntegral . ord) str

    -- Funkcja pomocnicza do konwersji Word16 na [Word8] (Little Endian)
    wordToBytesLE :: Word16 -> [Word8]
    wordToBytesLE w = [loByte w, hiByte w]

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

-- --- Przykład Użycia (z nowymi dyrektywami) ---

mySimpleProgram :: Asm ()
mySimpleProgram = do
    l_ "start"
    lda_imm 0x10
    sta_abs $ AddrLiteral 0x0200 -- Użyjmy sensownego adresu strony zerowej lub RAM

    l_ "loop"
    inx
    -- Przykład użycia adresu danych: Załóżmy, że chcemy wyzerować obszar 'values'
    -- To nie jest najbardziej efektywny kod 6502, ale pokazuje koncepcję
    sta_abs_x $ AddrLabel "values"
    bne "loop"                 -- Pętla (bez warunku wyjścia, dla uproszczenia)

    lda_imm 0xAA
    jmp_abs $ AddrLabel "end_routine"

    rts                        -- Niedostępny kod

    -- <<< Przykładowe użycie dyrektyw danych >>>
    l_ "message"
    string $ "Hello Haskell+6502 World!" ++ ['\0'] -- String z NUL terminator
    -- Alternatywnie, można użyć db, ale trzeba by ręcznie dodać terminator:
    db [0x0D, 0x00] -- CR + NUL terminator 

    l_ "values"
    dw [ -- Kilka wartości 16-bitowych
        0x1234, 0xABCD, 0xFFFF, 0x0000
      ]

    db [ -- Kilka bajtów
        1, 2, 3, 4, 5
      ]

    l_ "end_routine"
    lda_imm 0x55
    sta_abs $ AddrLiteral 0x0201
    rts

main :: IO ()
main = do
    let startAddress = 0x0000 -- Sensowny adres startowy dla C64/NES itp.
    putStrLn $ "Assembling program starting at: $" ++ showHex startAddress ""
    case runAssembler startAddress mySimpleProgram of
      Left err -> putStrLn $ "Assembly failed: " ++ err
      Right (byteCode, labels) -> do
        putStrLn "\n--- Assembly Successful! ---"
        putStrLn "\nLabels Defined:"
        mapM_ (\(lbl, addr) -> putStrLn $ "  " ++ lbl ++ ": $" ++ showHex addr "") (Map.toList labels)

        putStrLn "\nGenerated ByteCode:"
        putStrLn $ formatHexBytes startAddress byteCode

-- Formatowanie bez zmian
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