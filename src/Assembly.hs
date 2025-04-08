
module Assembly (
    -- Re-export Core Types/State/Monad needed by users
    module Assembly.Core,

    -- Higher-level functions
    runAssembler,
    generateBinary,
    formatHexBytes,

    -- Re-export List helpers
    module Assembly.List
) where


import Prelude 
import Assembly.Core (hx, Asm, ProgramCounter, Label, initialAsmState, unAsm, AsmState(..), SymbolicInstruction(..), branchOpcode, wordToBytesLE, generateInstructionBytes)
import Assembly.List
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
