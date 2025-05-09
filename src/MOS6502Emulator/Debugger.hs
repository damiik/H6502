module MOS6502Emulator.Debugger (logRegisters, logMemoryRange) where

import Data.Word (Word16, Word8)
import Numeric (showHex)
import Control.Monad.IO.Class (liftIO)
import MOS6502Emulator.Machine(Machine(), FDX, getMemory, fetchByteMem)
import MOS6502Emulator.Registers
import MOS6502Emulator.Memory (Memory())
import Data.Bits -- Import Data.Bits

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

logRegisters :: Registers -> FDX ()
logRegisters reg = liftIO $ do
  let sr = rSR reg
  let getFlagBit r b = if testBit r b then '1' else '0'

  let statusString = (if testBit sr 7 then "* Negative " else []) ++
                     (if testBit sr 6 then "* Overflow " else []) ++
                     (if testBit sr 4 then "* Break " else []) ++
                     (if testBit sr 3 then "* Decimal " else []) ++
                     (if testBit sr 2 then "* Interrupt " else []) ++
                     (if testBit sr 1 then "* Zero (Equal)" else []) ++
                     (if testBit sr 0 then "* Carry "  else [])
  -- let statusString =
  --       "\x1b[35mN-\x1b[33m" ++ [getFlagBit 7] ++
  --       "\x1b[35m V-\x1b[33m" ++ [getFlagBit 6] ++
  --       "\x1b[35m B-\x1b[33m" ++ [getFlagBit 4] ++
  --       "\x1b[35m D-\x1b[33m" ++ [getFlagBit 3] ++
  --       "\x1b[35m I-\x1b[33m" ++ [getFlagBit 2] ++
  --       "\x1b[35m Z-\x1b[33m" ++ [getFlagBit 1] ++
  --       "\x1b[35m C-\x1b[33m" ++ [getFlagBit 0]

  let formatBinary8 b = p1 ++ " " ++ p2
        where
          p1 =  [getFlagBit b i | i <- [7,6..4]]
          p2 =  [getFlagBit b i | i <- [3,2..0]]
  putStrLn "--------------------------------------------"
  putStrLn $ "\x1b[35m\x1b[1mPC\x1b[0m\x1b[35m: $" ++ (formatHex16 (rPC reg)) ++ "\x1b[0m"
  putStrLn $ "\x1b[33m\x1b[1mAC\x1b[0m\x1b[33m: $" ++ formatHex8 (rAC reg) ++ " [" ++ formatBinary8 (rAC reg) ++ "] ( " ++ show (rAC reg) ++ " )\x1b[0m"
  putStrLn $ "\x1b[32m\x1b[1m X\x1b[0m\x1b[32m: $" ++ formatHex8 (rX reg) ++ " [\x1b[32m" ++ formatBinary8 (rX reg) ++ "\x1b[32m] ( " ++ show (rX reg) ++ " )\x1b[0m"
  putStrLn $ "\x1b[32m\x1b[1m Y\x1b[0m\x1b[32m: $" ++ formatHex8 (rY reg) ++ " [\x1b[32m" ++ formatBinary8 (rY reg) ++ "\x1b[32m] ( " ++ show (rY reg) ++ " )\x1b[0m"
  putStrLn $ "\x1b[35m\x1b[1mSP\x1b[0m\x1b[35m: $" ++ (formatHex8 (rSP reg)) ++ "\x1b[0m"
  putStrLn $ "\x1b[35m\x1b[1mSR\x1b[0m\x1b[35m:     [" ++ formatBinary8 (rSR reg)  ++ "]  " ++ statusString ++ "\n        *NV-B DIZC*\x1b[0m" -- Updated SR output

logMemoryRange :: Word16 -> Word16 -> Maybe String -> FDX ()
logMemoryRange start end name = do
  bytes <- sequence [fetchByteMem addr | addr <- [start..end]]
  liftIO $ putStrLn $
    (case name of
        Just n -> n ++ " [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = "
        Nothing -> "MEM [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = ") ++
    unwords (map formatHex8 bytes)
