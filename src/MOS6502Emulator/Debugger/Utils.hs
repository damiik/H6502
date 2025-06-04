module MOS6502Emulator.Debugger.Utils
  ( parseHexWord
  , parseHexByte
  , setPC_
  , getRegisters
  , logMemoryRange
  , logRegisters
  , formatStatusFlags
  ) where

import Numeric (readHex, showHex)
import Data.Word (Word16, Word8)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad.State (get, modify)
import Text.Printf (printf)
import Data.List (stripPrefix, unwords)
import Data.Bits (testBit)

import MOS6502Emulator.Core (FDX, fetchByteMem)
import MOS6502Emulator.Machine (Machine(mRegs, mMem)) -- Added Machine to import list
import MOS6502Emulator.Registers (Registers(rPC, rAC, rX, rY, rSP, rSR))
import MOS6502Emulator.DissAssembler (formatHex8, formatHex16)

-- Helper to parse a hex string into a Word16.
parseHexWord :: String -> Maybe Word16
parseHexWord s = case readHex (stripHexPrefix s) of
    [(val, "")] -> Just val
    _           -> Nothing
  where
    stripHexPrefix :: String -> String
    stripHexPrefix str = fromMaybe str (stripPrefix "0x" str)

-- Helper to parse a hex string into a Word8.
parseHexByte :: String -> Maybe Word8
parseHexByte s = case readHex (stripHexPrefix s) of
    [(val, "")] -> Just val
    _           -> Nothing
  where
    stripHexPrefix :: String -> String
    stripHexPrefix str = fromMaybe str (stripPrefix "0x" str)

-- | Sets the Program Counter (PC) in the machine state.
setPC_ :: Word16 -> FDX ()
setPC_ addr = modify (\m -> m { mRegs = (mRegs m) { rPC = addr } })

-- | Gets the current register values from the machine state.
getRegisters :: FDX Registers
getRegisters = do
  machine <- get
  return (mRegs machine)


formatStatusFlags :: Word8 -> String
formatStatusFlags sr =
  let getFlagBit r b = if testBit r b then '*' else ' '
  in [getFlagBit sr 7, 'N', ' ', getFlagBit sr 6, 'V', '-', getFlagBit sr 4, 'B', ' ', getFlagBit sr 3, 'D', ' ', getFlagBit sr 2, 'I', ' ', getFlagBit sr 1, 'Z', ' ', getFlagBit sr 0, 'C']


-- | Logs the current register values as a list of strings.
logRegisters :: Registers -> [String]
logRegisters reg =
  let sr = rSR reg
      formatBinary8 b = p1 ++ " " ++ p2
            where
              p1 =  [if testBit b i then '1' else '0' | i <- [7,6..4]]
              p2 =  [if testBit b i then '1' else '0' | i <- [3,2..0]]
  in [ "--------------------------------------------"
     , "\x1b[35m\x1b[1mPC\x1b[0m\x1b[35m: $" ++ (formatHex16 (rPC reg)) ++ "\x1b[0m"
     , "\x1b[33m\x1b[1mAC\x1b[0m\x1b[33m: $" ++ formatHex8 (rAC reg) ++ " [" ++ formatBinary8 (rAC reg) ++ "] ( " ++ show (rAC reg) ++ " )\x1b[0m"
     , "\x1b[32m\x1b[1m X\x1b[0m\x1b[32m: $" ++ formatHex8 (rX reg) ++ " [\x1b[32m" ++ formatBinary8 (rX reg) ++ "\x1b[32m] ( " ++ show (rX reg) ++ " )\x1b[0m"
     , "\x1b[32m\x1b[1m Y\x1b[0m\x1b[32m: $" ++ formatHex8 (rY reg) ++ " [\x1b[32m" ++ formatBinary8 (rY reg) ++ "\x1b[32m] ( " ++ show (rY reg) ++ " )\x1b[0m"
     , "\x1b[35m\x1b[1mSP\x1b[0m\x1b[35m: $" ++ (formatHex8 (rSP reg)) ++ "\x1b[0m"
     , "\x1b[35m\x1b[1mSR\x1b[0m\x1b[35m:     [" ++ formatBinary8 (rSR reg)  ++ "]  " ++ formatStatusFlags sr ++ "\n        *NV-B DIZC*\x1b[0m"
     ]


-- | Logs the memory range as a list of strings.
logMemoryRange :: Word16 -> Word16 -> Maybe String -> FDX [String]
logMemoryRange start end name = do
  bytes <- sequence [fetchByteMem addr | addr <- [start..end]]
  return [
    (case name of
        Just n -> n ++ " [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = "
        Nothing -> "MEM [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = ") ++
    unwords (map formatHex8 bytes)
    ]
