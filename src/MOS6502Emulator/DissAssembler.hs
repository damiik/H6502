module MOS6502Emulator.DissAssembler (
    disassembleInstructionPure
  , disassembleInstructionsPure -- Export disassembleInstructions
  , InstructionInfo(..)
  , opcodeMap   -- Export opcodeMap  
  , formatHex8
  , formatHex16
  , unwords
) where

import Data.Word (Word8, Word16)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric (showHex)
-- import Control.Monad.State (gets) -- To get parts of the state -- REMOVED
import Control.Lens (view) -- Import view
import Data.Bits (shiftL, (.|.), testBit) -- Import bitwise operators and testBit
import Data.Int (Int16) -- Import Int16
import MOS6502Emulator.Core (fetchByteMemPure, toWord, Machine(..)) -- Import full Machine and pure fetch
import MOS6502Emulator.Lenses (labelMap) -- Import the labelMap lens
import Assembly.Instructions6502 (Mnemonic(..), AddressingMode(..), instructionData, getModeSize)
import Data.List (unwords) -- Explicitly import unwords

-- | Formats a Word8 as a two-character hexadecimal string, padding with a leading zero if necessary.
formatHex8 :: Word8 -> String
formatHex8 b =
  let hexStr = showHex b ""
  in if length hexStr < 2 then "0" ++ hexStr else hexStr

-- | Formats a Word16 as a four-character hexadecimal string, padding with leading zeros if necessary.
formatHex16 :: Word16 -> String
formatHex16 w =
  let hexStr = showHex w ""
  in replicate (4 - length hexStr) '0' ++ hexStr



-- | Data structure to hold instruction details for disassembly
data InstructionInfo = InstructionInfo
    { mnemonic :: Mnemonic
    , addressingMode :: AddressingMode
    , size :: Word8
    } deriving (Show)

-- | Lookup table from opcode to InstructionInfo
opcodeMap :: Map Word8 InstructionInfo
opcodeMap = Map.fromList $ map (\(m, am, op) -> (op, InstructionInfo m am (getModeSize am))) instructionData

-- | Disassemble a single instruction at the given program counter (pure version).
disassembleInstructionPure :: Word16 -> Machine -> (String, Word8)
disassembleInstructionPure pc machine =
    let opcode = fetchByteMemPure pc machine
    in case Map.lookup opcode opcodeMap of
        Nothing -> ("Unknown opcode: $" ++ formatHex8 opcode, 1)
        Just info ->
            let lblMap = view labelMap machine -- Get the labelMap from the Machine state
                operands = fetchOperandsPure pc (size info) machine
                bytes = opcode : operands
                byteString = Data.List.unwords $ map formatHex8 bytes
                paddedByteString = take 10 (byteString ++ replicate 10 ' ') -- Pad for alignment
                operandString = formatOperandPure pc info operands lblMap -- Pass labelMap
                addressString = "\x1b[34m($\x1b[33m" ++ formatHex16 pc ++ "\x1b[34m): \x1b[0m"
                instructionString = addressString  ++ paddedByteString ++ " \x1b[36m\x1b[1m" ++ show (mnemonic info) ++ "\x1b[0m " ++ operandString
            in (instructionString, size info)

-- | Fetch the operand bytes for an instruction (pure version).
fetchOperandsPure :: Word16 -> Word8 -> Machine -> [Word8]
fetchOperandsPure _ 1 _ = [] -- Implicit and Accumulator modes have no operands
fetchOperandsPure pc 2 machine =
    let operand1 = fetchByteMemPure (pc + 1) machine
    in [operand1]
fetchOperandsPure pc 3 machine =
    let operand1 = fetchByteMemPure (pc + 1) machine
        operand2 = fetchByteMemPure (pc + 2) machine
    in [operand1, operand2]
fetchOperandsPure _ _ _ = [] -- Should not happen with valid instruction sizes

-- | Format the operand string based on the addressing mode (pure version).
formatOperandPure :: Word16 -> InstructionInfo -> [Word8] -> Map.Map Word16 String -> String
formatOperandPure pc info operands lblMap =
    let formatAddress addr =
            case Map.lookup addr lblMap of
                Just lbl -> "\x1b[32m" ++ lbl ++ "\x1b[0m \x1b[34m($\x1b[33m" ++ formatHex16 addr ++ "\x1b[34m)\x1b[0m"
                Nothing  -> "\x1b[34m$\x1b[33m" ++ formatHex16 addr ++ "\x1b[0m"
    in case addressingMode info of
        Implicit    -> ""
        Accumulator -> "\x1b[35mA\x1b[0m"
        Immediate   -> case operands of
                           [operand] -> "\x1b[34m#$\x1b[33m" ++ (formatHex8 operand) ++ "\x1b[0m"
                           _         -> "Invalid operands for Immediate mode"
        ZeroPage    -> case operands of
                           [operand] -> formatAddress (fromIntegral operand)
                           _         -> "Invalid operands for ZeroPage mode"
        ZeroPageX   -> case operands of
                           [operand] -> formatAddress (fromIntegral operand) ++ "\x1b[34m,X\x1b[0m"
                           _         -> "Invalid operands for ZeroPageX mode"
        ZeroPageY   -> case operands of
                           [operand] -> formatAddress (fromIntegral operand) ++ "\x1b[34m,Y\x1b[0m"
                           _         -> "Invalid operands for ZeroPageY mode"
        Absolute    -> case operands of
                           [lo, hi] -> let addr = mkWord lo hi in formatAddress addr
                           _        -> "Invalid operands for Absolute mode"
        AbsoluteX   -> case operands of
                           [lo, hi] -> let addr = mkWord lo hi in formatAddress addr ++ "\x1b[34m,X\x1b[0m"
                           _        -> "Invalid operands for AbsoluteX mode"
        AbsoluteY   -> case operands of
                           [lo, hi] -> let addr = mkWord lo hi in formatAddress addr ++ "\x1b[34m,Y\x1b[0m"
                           _        -> "Invalid operands for AbsoluteY mode"
        Indirect    -> case operands of -- Only for JMP
                           [lo, hi] -> let addr = mkWord lo hi in "\x1b[34m(" ++ formatAddress addr ++ "\x1b[34m)\x1b[0m"
                           _        -> "Invalid operands for Indirect mode"
        IndirectX   -> case operands of
                           [operand] -> "\x1b[34m(" ++ formatAddress (fromIntegral operand) ++ "\x1b[34m,X)\x1b[0m"
                           _         -> "Invalid operands for IndirectX mode"
        IndirectY   -> case operands of
                           [operand] -> "\x1b[34m(" ++ formatAddress (fromIntegral operand) ++ "\x1b[34m),Y\x1b[0m"
                           _         -> "Invalid operands for IndirectY mode"
        Relative    -> case operands of
                           [offsetByte] ->
                               let offset :: Int16
                                   offset = if testBit offsetByte 7
                                            then fromIntegral offsetByte - 256 -- Calculate two's complement for negative
                                            else fromIntegral offsetByte
                               in formatAddress (pc + 2 + fromIntegral offset)
                           _            -> "Invalid operands for Relative mode"

-- Helper to create a Word16 from low and high bytes
mkWord :: Word8 -> Word8 -> Word16
mkWord lo hi = toWord lo .|. (toWord hi `shiftL` 8)

-- | Helper function to disassemble multiple instructions and return them as a list of strings (pure version).
disassembleInstructionsPure :: Word16 -> Int -> Machine -> ([String], Word16) -- Return (disassembled lines, address after last instruction)
disassembleInstructionsPure currentPC 0 _ = ([], currentPC)
disassembleInstructionsPure currentPC remaining machine =
    let lblMap = view labelMap machine -- Get the labelMap from the Machine state using the lens
        -- Disassemble the current instruction
        (disassembled, instLen) = disassembleInstructionPure currentPC machine
        nextPC = currentPC + (fromIntegral instLen)
        (restOfLines, finalPC) = disassembleInstructionsPure nextPC (remaining - 1) machine
        currentLine = case Map.lookup currentPC lblMap of
                        Just lbl -> "\n\x1b[32m" ++ lbl ++ ":\x1b[0m" ++ disassembled
                        Nothing  -> disassembled
    in (currentLine : restOfLines, finalPC)
