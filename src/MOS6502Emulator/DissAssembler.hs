{-# LANGUAGE LambdaCase #-}

module MOS6502Emulator.DissAssembler (
    disassembleInstruction
) where

import Data.Word (Word8, Word16)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric (showHex)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (shiftL, (.|.), testBit) -- Import bitwise operators and testBit
import Data.Int (Int16) -- Import Int16

import MOS6502Emulator.Machine (FDX, fetchByteMem, fetchWordMem, toWord)
import Assembly.Instructions6502 (Mnemonic(..), AddressingMode(..), instructionData, getModeSize)

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
        Nothing -> return ("Unknown opcode: $" ++ showHex opcode "", 1)
        Just info -> do
            operands <- fetchOperands pc (size info)
            let bytes = opcode : operands
            let byteString = unwords $ map (`showHex` "") bytes
            let paddedByteString = take 10 (byteString ++ replicate 10 ' ') -- Pad for alignment
            operandString <- formatOperand pc info operands
            let addressString = "\x1b[34m($\x1b[33m" ++ showHex pc "" ++ "\x1b[34m): \x1b[0m"
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
formatOperand :: Word16 -> InstructionInfo -> [Word8] -> FDX String
formatOperand pc info operands =
    case addressingMode info of
        Implicit    -> return ""
        Accumulator -> return "\x1b[35mA\x1b[0m"
        Immediate   -> case operands of
                           [operand] -> return $ "\x1b[34m#$\x1b[33m" ++ (showHex operand "") ++ "\x1b[0m"
                           _         -> return "Invalid operands for Immediate mode"
        ZeroPage    -> case operands of
                           [operand] -> return $ "\x1b[34m$\x1b[33m" ++ (showHex operand "") ++ "\x1b[0m"
                           _         -> return "Invalid operands for ZeroPage mode"
        ZeroPageX   -> case operands of
                           [operand] -> return $ "\x1b[34m$\x1b[33m" ++ showHex operand "" ++ "\x1b[34m,X\x1b[0m"
                           _         -> return "Invalid operands for ZeroPageX mode"
        ZeroPageY   -> case operands of
                           [operand] -> return $ "\x1b[34m$\x1b[33m" ++ showHex operand "" ++ "\x1b[34m,Y\x1b[0m"
                           _         -> return "Invalid operands for ZeroPageY mode"
        Absolute    -> case operands of
                           [lo, hi] -> do
                               let addr = mkWord lo hi
                               return $ "\x1b[34m$\x1b[33m" ++ (showHex addr "") ++ "\x1b[0m"
                           _        -> return "Invalid operands for Absolute mode"
        AbsoluteX   -> case operands of
                           [lo, hi] -> do
                               let addr = mkWord lo hi
                               return $ "\x1b[34m$\x1b[33m" ++ showHex addr "" ++ "\x1b[34m,X\x1b[0m"
                           _        -> return "Invalid operands for AbsoluteX mode"
        AbsoluteY   -> case operands of
                           [lo, hi] -> do
                               let addr = mkWord lo hi
                               return $ "\x1b[34m$\x1b[33m" ++ showHex addr "" ++ "\x1b[34m,Y\x1b[0m"
                           _        -> return "Invalid operands for AbsoluteY mode"
        Indirect    -> case operands of -- Only for JMP
                           [lo, hi] -> do
                               let addr = mkWord lo hi
                               return $ "\x1b[34m($\x1b[33m" ++ showHex addr "" ++ "\x1b[34m)\x1b[0m"
                           _        -> return "Invalid operands for Indirect mode"
        IndirectX   -> case operands of
                           [operand] -> return $ "\x1b[34m($\x1b[33m" ++ showHex operand "" ++ "\x1b[34m,X)\x1b[0m"
                           _         -> return "Invalid operands for IndirectX mode"
        IndirectY   -> case operands of
                           [operand] -> return $ "\x1b[34m($\x1b[33m" ++ showHex operand "" ++ "\x1b[34m),Y\x1b[0m"
                           _         -> return "Invalid operands for IndirectY mode"
        Relative    -> case operands of
                           [offsetByte] -> do
                               let offset :: Int16
                                   offset = if testBit offsetByte 7
                                            then fromIntegral offsetByte - 256 -- Calculate two's complement for negative
                                            else fromIntegral offsetByte
                               -- Standard 6502 relative branch calculation: (address of opcode + 2) + signed_offset
                               let targetAddr = pc + 2 + fromIntegral offset
                               return $ "\x1b[34m$\x1b[33m" ++ (showHex targetAddr "") ++ "\x1b[0m"
                           _            -> return "Invalid operands for Relative mode"

-- Helper to create a Word16 from low and high bytes
mkWord :: Word8 -> Word8 -> Word16
mkWord lo hi = toWord lo .|. (toWord hi `shiftL` 8)
