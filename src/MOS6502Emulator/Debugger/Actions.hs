{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger.Actions
  ( executeStepAndRender
  , handlePostInstructionChecks
  , logRegisters -- Export logRegisters
  , logMemoryRangePure -- Export logMemoryRangePure
  ) where

import Control.Monad.State (get, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified System.Console.ANSI as ANSI
import Data.Word (Word8, Word16) -- Added for Word8, Word16
import Data.Bits (testBit) -- Added for testBit
import Control.Lens -- Import Control.Lens
import MOS6502Emulator.Lenses -- Import our custom lenses
import qualified MOS6502Emulator.Lenses as L -- Import all lenses qualified

import MOS6502Emulator.Core (FDX, fetchByteMemPure, Machine(..)) -- Added fetchByteMemPure
import MOS6502Emulator.Machine (fdxSingleCycle)
import MOS6502Emulator.Registers (Registers(_rPC, _rAC, _rX, _rY, _rSP, _rSR)) -- Added all register fields
import MOS6502Emulator.DissAssembler (disassembleInstructionPure, formatHex8, formatHex16) -- Added formatHex8, formatHex16
import MOS6502Emulator.Display (renderScreen, putOutput)

-- | Executes a single instruction cycle, clears the screen, renders the updated screen, and handles post-instruction checks.
executeStepAndRender :: FDX ()
executeStepAndRender = do
  _ <- fdxSingleCycle -- Execute one instruction
  
  -- Get the current machine state after the step
  currentMachine <- get 

  -- Capture register output (logRegisters expects Registers, use mRegs gives FDX Registers)
  regs <- use mRegs
  let regOutput = logRegisters regs 

  -- Capture memory trace output (logMemoryRange returns FDX [String], so mapM over it)
  memBlocks <- use memoryTraceBlocks
  let memTraceOutputList = map (\(start, end, name) -> logMemoryRangePure start end name currentMachine) memBlocks
  let memTraceOutput = concat memTraceOutputList
  liftIO ANSI.clearScreen -- Aggressive clear after step
  liftIO $ ANSI.setCursorPosition 0 0 -- Reset cursor

  -- Get disassembly lines
  currentPC_after <- use (mRegs . rPC) -- Get PC after execution
  let (disassembledLines, _) = disassembleInstructionPure currentPC_after currentMachine

  -- Render the screen with disassembly, registers, and memory traces
  -- Disassembly is placed at the top of the right column content.
  renderScreen currentMachine (disassembledLines : regOutput ++ memTraceOutput)

  -- Now, handlePostInstructionChecks only needs to manage halting and trace enable.
  handlePostInstructionChecks

-- | Handles post-instruction checks: halting and tracing.
-- This function should NOT re-enter the debugger loop or call runLoop.
handlePostInstructionChecks :: FDX ()
handlePostInstructionChecks = do
  haltedState <- use halted
  if haltedState
    then do
      liftIO $ putStrLn "\nMachine halted. Entering debugger."
      debuggerActive .= True -- Activate debugger
    else do
      -- Log registers if tracing is enabled and debugger is not active
      enableTraceState <- use enableTrace
      debuggerActiveState <- use debuggerActive
      when (enableTraceState && not debuggerActiveState) $ do
          currentMachine <- get
          currentPC_after <- use (mRegs . rPC) -- Get PC after execution
          let (disassembled, _) = disassembleInstructionPure currentPC_after currentMachine -- Use PC after execution
          -- Removed logging to putOutput here. Only state updates.
          return ()

-- | Logs the current register values as a list of strings.
logRegisters :: Registers -> [String]
logRegisters reg =
  let sr = _rSR reg
      formatBinary8 b = p1 ++ " " ++ p2
            where
              p1 =  [if testBit b i then '1' else '0' | i <- [7,6..4]]
              p2 =  [if testBit b i then '1' else '0' | i <- [3,2..0]]
  in [ "--------------------------------------------"
     , "\x1b[35m\x1b[1mPC\x1b[0m\x1b[35m: $" ++ formatHex16 (_rPC reg) ++ "\x1b[0m"
     , "\x1b[33m\x1b[1mAC\x1b[0m\x1b[33m: $" ++ formatHex8 (_rAC reg) ++ " [" ++ formatBinary8 (_rAC reg) ++ "] ( " ++ show (_rAC reg) ++ " )\x1b[0m"
     , "\x1b[32m\x1b[1m X\x1b[0m\x1b[32m: $" ++ formatHex8 (_rX reg) ++ " [\x1b[32m" ++ formatBinary8 (_rX reg) ++ "\x1b[32m] ( " ++ show (_rX reg) ++ " )\x1b[0m"
     , "\x1b[32m\x1b[1m Y\x1b[0m\x1b[32m: $" ++ formatHex8 (_rY reg) ++ " [\x1b[32m" ++ formatBinary8 (_rY reg) ++ "\x1b[32m] ( " ++ show (_rY reg) ++ " )\x1b[0m"
     , "\x1b[35m\x1b[1mSP\x1b[0m\x1b[35m: $" ++ formatHex8 (_rSP reg) ++ "\x1b[0m"
     , "\x1b[35m\x1b[1mSR\x1b[0m\x1b[35m: $" ++ formatHex8 (_rSR reg) ++ formatStatusFlags sr ++ "\n        *NV-B DIZC*\x1b[0m"
     ]

formatStatusFlags :: Word8 -> String
formatStatusFlags sr =
  let getFlagBit r b = if testBit r b then '*' else ' '
  in [' ', getFlagBit sr 7, 'N', ' ', getFlagBit sr 6, 'V', ' ', '-', '-', getFlagBit sr 4, 'B', ' ', getFlagBit sr 3, 'D', ' ', getFlagBit sr 2, 'I', ' ', getFlagBit sr 1, 'Z', ' ', getFlagBit sr 0, 'C']

-- | Logs the memory range as a list of strings (pure version).
logMemoryRangePure :: Word16 -> Word16 -> Maybe String -> Machine -> [String]
logMemoryRangePure start end name machine =
  let bytes = [fetchByteMemPure addr machine | addr <- [start..end]]
  in [
    (case name of
        Just n -> n ++ " [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = "
        Nothing -> "MEM [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = ") ++
    unwords (map formatHex8 bytes)
    ]
