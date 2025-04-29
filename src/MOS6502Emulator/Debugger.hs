module MOS6502Emulator.Debugger (logRegisters, logMemoryRange) where

import Data.Word (Word16, Word8)
import Numeric (showHex)
import Control.Monad.IO.Class (liftIO)
import MOS6502Emulator.Machine(Machine(), FDX, getMemory, fetchByteMem)
import MOS6502Emulator.Registers
import MOS6502Emulator.Memory (Memory())


logRegisters :: Registers -> FDX ()
logRegisters reg = liftIO $ do
  putStrLn $ "PC: $" ++ showHex (rPC reg) ""
  putStrLn $ "AC: $" ++ showHex (rAC reg) "" ++ " (" ++ show (rAC reg) ++ ")"
  putStrLn $ " X: $" ++ showHex (rX reg) "" ++ " (" ++ show (rX reg) ++ ")"
  putStrLn $ " Y: $" ++ showHex (rY reg) "" ++ " (" ++ show (rY reg) ++ ")"
  putStrLn $ "SP: $" ++ showHex (rSP reg) ""
  putStrLn $ "SR: $" ++ showHex (rSR reg) ""

logMemoryRange :: Word16 -> Word16 -> FDX ()
logMemoryRange start end = do
  bytes <- sequence [fetchByteMem addr | addr <- [start..end]]
  liftIO $ putStrLn $
    "MEM [$" ++ showHex start "" ++ " - $" ++ showHex end "" ++ "] = " ++
    unwords (map (`showHex` "") bytes)
