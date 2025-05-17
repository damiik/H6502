
module MOS6502Emulator.DissAssembler (
    disassembleInstruction
  , disassembleInstructions -- Export disassembleInstructions
) where

import Data.Word (Word8, Word16)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric (showHex)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets) -- To get parts of the state
import Data.Bits (shiftL, (.|.), testBit) -- Import bitwise operators and testBit
import Data.Int (Int16) -- Import Int16
import MOS6502Emulator.Machine (FDX, fetchByteMem, fetchWordMem, toWord, Machine(labelMap)) -- Import labelMap
import Assembly.Instructions6502 (Mnemonic(..), AddressingMode(..), instructionData, getModeSize)
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

-- | Disassemble a single instruction at the given program counter
disassembleInstruction :: Word16 -> FDX (String, Word8)
disassembleInstruction pc = do
    opcode <- fetchByteMem pc
    case Map.lookup opcode opcodeMap of
        Nothing -> return ("Unknown opcode: $" ++ formatHex8 opcode, 1)
        Just info -> do
            lblMap <- gets labelMap -- Get the labelMap from the Machine state
            operands <- fetchOperands pc (size info)
            let bytes = opcode : operands
            let byteString = unwords $ map formatHex8 bytes
            let paddedByteString = take 10 (byteString ++ replicate 10 ' ') -- Pad for alignment
            operandString <- formatOperand pc info operands lblMap -- Pass labelMap
            let addressString = "\x1b[34m($\x1b[33m" ++ formatHex16 pc ++ "\x1b[34m): \x1b[0m"
            let instructionString = addressString  ++ paddedByteString ++ " \x1b[36m\x1b[1m" ++ show (mnemonic info) ++ "\x1b[0m " ++ operandString
            return (instructionString, size info)

-- | Fetch the operand bytes for an instruction
fetchOperands :: Word16 -> Word8 -> FDX [Word8]
fetchOperands _ 1 = return [] -- Implicit and Accumulator modes have no operands
fetchOperands pc 2 = do
    operand1 <- fetchByteMem (pc + 1)
    return [operand1]
fetchOperands pc 3 = do
    operand1 <- fetchByteMem (pc + 1)
    operand2 <- fetchByteMem (pc + 2)
    return [operand1, operand2]
fetchOperands _ _ = return [] -- Should not happen with valid instruction sizes

-- | Format the operand string based on the addressing mode
formatOperand :: Word16 -> InstructionInfo -> [Word8] -> Map.Map Word16 String -> FDX String
formatOperand pc info operands lblMap =
    let formatAddress addr =
            case Map.lookup addr lblMap of
                Just lbl -> "\x1b[32m" ++ lbl ++ "\x1b[0m \x1b[34m($\x1b[33m" ++ formatHex16 addr ++ "\x1b[34m)\x1b[0m"
                Nothing  -> "\x1b[34m$\x1b[33m" ++ formatHex16 addr ++ "\x1b[0m"
    in case addressingMode info of
        Implicit    -> return ""
        Accumulator -> return "\x1b[35mA\x1b[0m"
        Immediate   -> case operands of
                           [operand] -> return $ "\x1b[34m#$\x1b[33m" ++ (formatHex8 operand) ++ "\x1b[0m"
                           _         -> return "Invalid operands for Immediate mode"
        ZeroPage    -> case operands of
                           [operand] -> return $ formatAddress (fromIntegral operand)
                           _         -> return "Invalid operands for ZeroPage mode"
        ZeroPageX   -> case operands of
                           [operand] -> return $ formatAddress (fromIntegral operand) ++ "\x1b[34m,X\x1b[0m"
                           _         -> return "Invalid operands for ZeroPageX mode"
        ZeroPageY   -> case operands of
                           [operand] -> return $ formatAddress (fromIntegral operand) ++ "\x1b[34m,Y\x1b[0m"
                           _         -> return "Invalid operands for ZeroPageY mode"
        Absolute    -> case operands of
                           [lo, hi] -> let addr = mkWord lo hi in return $ formatAddress addr
                           _        -> return "Invalid operands for Absolute mode"
        AbsoluteX   -> case operands of
                           [lo, hi] -> let addr = mkWord lo hi in return $ formatAddress addr ++ "\x1b[34m,X\x1b[0m"
                           _        -> return "Invalid operands for AbsoluteX mode"
        AbsoluteY   -> case operands of
                           [lo, hi] -> let addr = mkWord lo hi in return $ formatAddress addr ++ "\x1b[34m,Y\x1b[0m"
                           _        -> return "Invalid operands for AbsoluteY mode"
        Indirect    -> case operands of -- Only for JMP
                           [lo, hi] -> let addr = mkWord lo hi in return $ "\x1b[34m(" ++ formatAddress addr ++ "\x1b[34m)\x1b[0m"
                           _        -> return "Invalid operands for Indirect mode"
        IndirectX   -> case operands of
                           [operand] -> return $ "\x1b[34m(" ++ formatAddress (fromIntegral operand) ++ "\x1b[34m,X)\x1b[0m"
                           _         -> return "Invalid operands for IndirectX mode"
        IndirectY   -> case operands of
                           [operand] -> return $ "\x1b[34m(" ++ formatAddress (fromIntegral operand) ++ "\x1b[34m),Y\x1b[0m"
                           _         -> return "Invalid operands for IndirectY mode"
        Relative    -> case operands of
                           [offsetByte] -> do
                               let offset :: Int16
                                   offset = if testBit offsetByte 7
                                            then fromIntegral offsetByte - 256 -- Calculate two's complement for negative
                                            else fromIntegral offsetByte
                               let targetAddr = pc + 2 + fromIntegral offset
                               return $ formatAddress targetAddr
                           _            -> return "Invalid operands for Relative mode"

-- Helper to create a Word16 from low and high bytes
mkWord :: Word8 -> Word8 -> Word16
mkWord lo hi = toWord lo .|. (toWord hi `shiftL` 8)

-- | Helper function to disassemble multiple instructions and print them.
disassembleInstructions :: Word16 -> Int -> FDX Word16 -- Return the address after the last disassembled instruction
disassembleInstructions currentPC 0 = return currentPC
disassembleInstructions currentPC remaining = do
    machine <- gets id -- Get the entire Machine state
    let lblMap = labelMap machine
    -- Check if the currentPC has a label and print it
    case Map.lookup currentPC lblMap of
        Just lbl -> liftIO $ putStrLn $ "\n\x1b[32m" ++ lbl ++ ":\x1b[0m" -- Print label on a new line if it exists
        Nothing  -> return ()

    -- Disassemble the current instruction
    (disassembled, instLen) <- disassembleInstruction currentPC
    liftIO $ putStrLn disassembled
    let nextPC = currentPC + (fromIntegral instLen)
    disassembleInstructions nextPC (remaining - 1)
