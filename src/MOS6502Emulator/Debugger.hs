module MOS6502Emulator.Debugger (logRegisters, logMemoryRange) where

import Data.Word (Word16, Word8)
import Numeric (showHex)
import Control.Monad.IO.Class (liftIO)
import MOS6502Emulator.Machine(Machine(), FDX, getMemory, fetchByteMem)
import MOS6502Emulator.Registers
import MOS6502Emulator.Memory (Memory())


logRegisters :: Registers -> FDX ()
logRegisters reg = liftIO $ do
  putStrLn $ "\x1b[35m\x1b[1mPC\x1b[0m\x1b[35m: $" ++ (showHex (rPC reg) "") ++ "\x1b[0m"
  putStrLn $ "\x1b[33m\x1b[1mAC\x1b[0m\x1b[33m: $" ++ showHex (rAC reg) "" ++ " ( " ++ show (rAC reg) ++ " )\x1b[0m"
  putStrLn $ "\x1b[32m\x1b[1m X\x1b[0m\x1b[32m: $" ++ showHex (rX reg) "" ++ " ( " ++ show (rX reg) ++ " )\x1b[0m"
  putStrLn $ "\x1b[32m\x1b[1m Y\x1b[0m\x1b[32m: $" ++ showHex (rY reg) "" ++ " ( " ++ show (rY reg) ++ " )\x1b[0m"
  putStrLn $ "\x1b[35m\x1b[1mSP\x1b[0m\x1b[35m: $" ++ (showHex (rSP reg) "") ++ "\x1b[0m"
  putStrLn $ "\x1b[35m\x1b[1mSR\x1b[0m\x1b[35m: $" ++ (showHex (rSR reg) "") ++ "\x1b[0m"

logMemoryRange :: Word16 -> Word16 -> FDX ()
logMemoryRange start end = do
  bytes <- sequence [fetchByteMem addr | addr <- [start..end]]
  liftIO $ putStrLn $
    "MEM [$" ++ showHex start "" ++ " - $" ++ showHex end "" ++ "] = " ++
    unwords (map (`showHex` "") bytes)
