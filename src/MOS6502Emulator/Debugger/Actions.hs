{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger.Actions
  ( executeStepAndRender
  , handlePostInstructionChecks
  , logRegisters -- Export logRegisters
  , logMemoryRange -- Export logMemoryRange
  ) where

import Control.Monad.State (get, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified System.Console.ANSI as ANSI
import Data.Word (Word8, Word16) -- Added for Word8, Word16
import Data.Bits (testBit) -- Added for testBit

import MOS6502Emulator.Core (FDX, fetchByteMem) -- Added fetchByteMem
import MOS6502Emulator.Machine (fdxSingleCycle, Machine(..))
import MOS6502Emulator.Registers (Registers(rPC, rAC, rX, rY, rSP, rSR)) -- Added all register fields
import MOS6502Emulator.DissAssembler (disassembleInstruction, formatHex8, formatHex16) -- Added formatHex8, formatHex16
import MOS6502Emulator.Display (renderScreen, putOutput)

-- | Executes a single instruction cycle, clears the screen, renders the updated screen, and handles post-instruction checks.
executeStepAndRender :: FDX ()
executeStepAndRender = do
  continue <- fdxSingleCycle -- Execute one instruction
  nextMachineState <- get -- Get state after instruction execution
  let regOutput = logRegisters (mRegs nextMachineState) -- Capture register output
  liftIO ANSI.clearScreen -- Aggressive clear after step
  liftIO $ ANSI.setCursorPosition 0 0 -- Reset cursor
  renderScreen nextMachineState regOutput -- Render the screen after stepping, passing register output
  handlePostInstructionChecks -- Handle tracing and halting checks

-- | Handles post-instruction checks: halting and tracing.
-- This function should NOT re-enter the debugger loop or call runLoop.
handlePostInstructionChecks :: FDX ()
handlePostInstructionChecks = do
  nextMachineState <- get -- Get the updated state after the instruction
  if halted nextMachineState
    then do
      liftIO $ putStrLn "\nMachine halted. Entering debugger."
      modify (\m -> m { debuggerActive = True }) -- Activate debugger
    else do
      -- Log registers and memory trace blocks if tracing is enabled and debugger is not active
      when (enableTrace nextMachineState && not (debuggerActive nextMachineState)) $ do
          let currentPC_after = rPC (mRegs nextMachineState) -- Get PC after execution
          disassembled <- disassembleInstruction currentPC_after -- Use PC after execution
          putOutput "" -- Use console output instead of direct print
          putOutput (fst disassembled) -- Use console output instead of direct print
          let regOutput = logRegisters (mRegs nextMachineState) -- Capture register output
          mapM_ putOutput regOutput -- Use console output instead of direct print
          -- Log all memory trace blocks
          mapM_ (\(start, end, name) -> do
                   memOutput <- logMemoryRange start end name -- Capture memory trace output
                   mapM_ putOutput memOutput) (memoryTraceBlocks nextMachineState) -- Use console output

-- | Logs the current register values as a list of strings.
logRegisters :: Registers -> [String]
logRegisters reg =
  let sr = rSR reg
      formatBinary8 b = p1 ++ " " ++ p2
            where
              p1 =  [if testBit b i then '1' else '0' | i <- [7,6..4]]
              p2 =  [if testBit b i then '1' else '0' | i <- [3,2..0]]
  in [ "--------------------------------------------"
     , "\x1b[35m\x1b[1mPC\x1b[0m\x1b[35m: $" ++ formatHex16 (rPC reg) ++ "\x1b[0m"
     , "\x1b[33m\x1b[1mAC\x1b[0m\x1b[33m: $" ++ formatHex8 (rAC reg) ++ " [" ++ formatBinary8 (rAC reg) ++ "] ( " ++ show (rAC reg) ++ " )\x1b[0m"
     , "\x1b[32m\x1b[1m X\x1b[0m\x1b[32m: $" ++ formatHex8 (rX reg) ++ " [\x1b[32m" ++ formatBinary8 (rX reg) ++ "\x1b[32m] ( " ++ show (rX reg) ++ " )\x1b[0m"
     , "\x1b[32m\x1b[1m Y\x1b[0m\x1b[32m: $" ++ formatHex8 (rY reg) ++ " [\x1b[32m" ++ formatBinary8 (rY reg) ++ "\x1b[32m] ( " ++ show (rY reg) ++ " )\x1b[0m"
     , "\x1b[35m\x1b[1mSP\x1b[0m\x1b[35m: $" ++ formatHex8 (rSP reg) ++ "\x1b[0m"
     , "\x1b[35m\x1b[1mSR\x1b[0m\x1b[35m: $" ++ formatHex8 (rSR reg) ++ formatStatusFlags sr ++ "\n        *NV-B DIZC*\x1b[0m"
     ]

formatStatusFlags :: Word8 -> String
formatStatusFlags sr =
  let getFlagBit r b = if testBit r b then '*' else ' '
  in [' ', getFlagBit sr 7, 'N', ' ', getFlagBit sr 6, 'V', ' ', '-', '-', getFlagBit sr 4, 'B', ' ', getFlagBit sr 3, 'D', ' ', getFlagBit sr 2, 'I', ' ', getFlagBit sr 1, 'Z', ' ', getFlagBit sr 0, 'C']

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
