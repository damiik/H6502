{-# LANGUAGE LambdaCase #-} -- Added for \case syntax
module MOS6502Emulator
  ( runEmulator
  , runTest
  , runDebugger
  , newMachine
  , setupMachine
  , Machine(..)
  , Memory
  , Registers
  , instructionCount -- Export instructionCount
  ) where

import Control.Exception (try, catch, IOException) -- Added for exception handling
import Control.Monad.State (get, modify, put, gets, runStateT) -- Import runStateT
import Control.Monad (when, unless, void) -- Import the 'when', 'unless', and 'void' functions
import Control.Monad.IO.Class (liftIO)
import Numeric (showHex, readHex) -- Import showHex and readHex
import Text.Printf
import System.IO (hSetEcho, hFlush, stdout, hSetBuffering, readFile, writeFile, BufferMode(NoBuffering, LineBuffering), stdin, hReady, getChar) -- Import hSetBuffering, BufferMode, stdin, hReady, getChar, LineBuffering
import System.IO.Error (isEOFError) -- Import isEOFError
import Data.Word ( Word8, Word16 )
import Data.List (stripPrefix, cycle, take) -- Import stripPrefix, cycle, take
import Data.Bits (Bits, (.&.)) -- Import Bits for status register manipulation if needed
import qualified Data.Map.Strict as Map -- For Map.empty

import MOS6502Emulator.Machine (fdxSingleCycle, loadSymbolFile, setPC_, setAC_, setX_, setY_, setSR_, setSP_, writeByteMem_) -- Import fdxSingleCycle from Core
import MOS6502Emulator.Core (Machine(..), FDX(..), getRegisters, instructionCount, cycleCount, setRegisters, getMemory, setMemory, fetchByteMem, fetchWordMem, writeByteMem, mkWord, toWord, DebuggerMode(..)) -- Import DebuggerMode from Machine
import MOS6502Emulator.Instructions
import MOS6502Emulator.Memory
import MOS6502Emulator.Registers
import qualified MOS6502Emulator.Debugger as D
-- import qualified MOS6502Emulator.Debugger.VimMode as V
import MOS6502Emulator.Debugger (handleCommand, handleBreak, handleMemTrace, saveDebuggerState, loadDebuggerState, DebuggerAction(..)) -- Import handleCommand, handleBreak, handleMemTrace, saveDebuggerState, loadDebuggerState, and interactiveLoopHelper, and DebuggerAction
import MOS6502Emulator.DissAssembler (disassembleInstruction) -- Import disassembleInstruction
import MOS6502Emulator.Debugger.Console (DebuggerConsoleState, initialConsoleState, putOutput, renderScreen) -- Import renderScreen
import MOS6502Emulator.Debugger.VimModeCore (initialVimState, VimState(..)) -- Import Motion, Action, and ViewMode
import qualified MOS6502Emulator.Debugger.VimModeEnhanced as V
-- import MOS6502Emulator.Debugger.VimModeEnhanced (handleVimKey) -- Removed as it's no longer exported

-- | Initializes a new 6502 machine state
-- | Initializes a new 6502 machine state
newMachine :: IO Machine
newMachine = do
  mem <- memory  -- 64KB of memory initialized by MOS6502Emulator.Memory
  let regs = mkRegisters
  return Machine { mRegs = regs, mMem = mem, halted = False, instructionCount = 0, cycleCount = 0, enableTrace = True, traceMemoryStart = 0x0000, traceMemoryEnd = 0x00FF, breakpoints = [], debuggerActive = False, memoryTraceBlocks = [], lastDisassembledAddr = 0x0000, labelMap = Map.empty, debugLogPath = Nothing, debuggerMode = CommandMode, pcHistory = [], storedAddresses = Map.empty, redoHistory = [], vimState = initialVimState, mConsoleState = initialConsoleState} -- Initialize new fields, including debuggerMode, pcHistory, storedAddresses, redoHistory, and mConsoleState

-- | The main fetch-decode-execute loop. Runs `fdxSingleCycle` repeatedly until emulation stops.
runFDXLoop :: FDX ()
runFDXLoop = do
  continue <- fdxSingleCycle -- This will now refer to the imported fdxSingleCycle
  when continue runFDXLoop

-- | Runs the emulator until the machine is halted
-- | Runs the emulator with the given machine state and starting PC
runEmulator :: Word16 -> Machine -> IO ((), Machine)
runEmulator startPC initialMachine = do
  let machineWithStartPC = initialMachine { mRegs = (mRegs initialMachine) { rPC = startPC } }
  runMachine machineWithStartPC


-- | The inner loop that manages debugger activation and instruction execution.
runLoop :: FDX ()
runLoop = do
  machine <- get
  if halted machine
    then return () -- Stop if machine is halted
    else do
      if debuggerActive machine
        then do
          -- Determine which debugger loop to run and capture the action
          action <- case debuggerMode machine of
            CommandMode -> do
              action <- D.interactiveLoopHelper
              case action of
                SwitchToVimMode -> do
                  put (machine { debuggerMode = VimMode })
                  return action
                _ -> return action
            VimMode -> do
              liftIO $ putStrLn $ "DEBUG: runLoop (VimMode) - halted before V.interactiveLoopHelper: " ++ show (halted machine)
              -- V.interactiveLoopHelper already handles putting the updated machine state
              (dbgAction, _) <- V.interactiveLoopHelper -- Discard newVimState as it's already put
              liftIO $ putStrLn $ "DEBUG: runLoop (VimMode) - dbgAction from V.interactiveLoopHelper: " ++ show dbgAction
              return dbgAction
            VimCommandMode -> do
              liftIO $ putStrLn $ "DEBUG: runLoop (VimCommandMode) - halted before V.interactiveLoopHelper: " ++ show (halted machine)
              -- V.interactiveLoopHelper already handles putting the updated machine state
              (dbgAction, _) <- V.interactiveLoopHelper -- Discard newVimState as it's already put
              liftIO $ putStrLn $ "DEBUG: runLoop (VimCommandMode) - dbgAction from V.interactiveLoopHelper: " ++ show dbgAction
              return dbgAction
        --else do
          -- Handle the action returned by the debugger loop
          case action of
            ContinueLoop _ -> runLoop -- Continue the main runLoop
            ExecuteStep _  -> do
              continue <- fdxSingleCycle -- Execute one instruction
              nextMachineState <- get -- Get state after instruction execution
              -- Check if the machine is halted after executing the instruction
              if halted nextMachineState
                then do
                  liftIO $ putStrLn "\nMachine halted. Entering debugger."
                  put (nextMachineState { debuggerActive = True }) -- Activate debugger
                  handlePostInstructionChecks -- Call helper
                else if not continue
                  then return () -- Stop if fdxSingleCycle returns False
                  else handlePostInstructionChecks -- Call helper
            ExitDebugger   -> do
              modify (\m -> m { debuggerActive = False }) -- Exit debugger mode
              runLoop -- Continue the main runLoop
            QuitEmulator   -> modify (\m -> m { halted = True }) -- Halt the emulator
            SwitchToCommandMode -> do
              modify (\m -> m { debuggerMode = CommandMode }) -- Switch to CommandMode
              runLoop -- Continue the main runLoop
            SwitchToVimMode -> do
              modify (\m -> m { debuggerMode = VimMode }) -- Switch to VimMode
              machineAfterModeChange <- get -- Get the machine state after mode change
              renderScreen machineAfterModeChange -- Render the screen immediately
              runLoop -- Continue the main runLoop
            NoAction       -> runLoop -- Simply continue the main runLoop, as vimState is already updated
        else do
          continue <- fdxSingleCycle -- Execute one instruction
          nextMachineState <- get -- Get state after instruction execution
          -- Check if the machine is halted after executing the instruction
          if halted nextMachineState
            then do
              liftIO $ putStrLn "\nMachine halted. Entering debugger."
              put (nextMachineState { debuggerActive = True }) -- Activate debugger
              handlePostInstructionChecks -- Call helper
            else if not continue
              then return () -- Stop if fdxSingleCycle returns False (not halted, but some other stop condition)
              else handlePostInstructionChecks -- Call helper

-- -- | The inner loop that manages debugger activation and instruction execution.
-- runLoop :: FDX ()
-- runLoop = do
--   machine <- get
--   if halted machine
--     then return () -- Stop if machine is halted
--     else do
--       if debuggerActive machine
--         then do
--           -- liftIO $ putStrLn "\nEntering interactive debugger."
--           -- Determine which debugger loop to run and capture the action
--           action <- case debuggerMode machine of
--             CommandMode -> D.interactiveLoopHelper initialConsoleState
--             VimMode     -> do

--               V.interactiveLoopHelper initialConsoleState
          
--           -- Handle the action returned by the debugger loop
--           case action of
--             ContinueLoop _ -> runLoop -- Continue the main runLoop
--             ExecuteStep _  -> do
--               continue <- fdxSingleCycle -- Execute one instruction
--               nextMachineState <- get -- Get state after instruction execution
--               -- Check if the machine is halted after executing the instruction
--               if halted nextMachineState
--                 then do
--                   liftIO $ putStrLn "\nMachine halted. Entering debugger."
--                   put (nextMachineState { debuggerActive = True }) -- Activate debugger
--                   handlePostInstructionChecks -- Call helper
--                 else if not continue
--                   then return () -- Stop if fdxSingleCycle returns False
--                   else handlePostInstructionChecks -- Call helper
--             ExitDebugger   -> do
--               modify (\m -> m { debuggerActive = False }) -- Exit debugger mode
--               runLoop -- Continue the main runLoop
--             QuitEmulator   -> modify (\m -> m { halted = True }) -- Halt the emulator
--             SwitchToCommandMode -> do
--               modify (\m -> m { debuggerMode = CommandMode }) -- Switch to CommandMode
--               runLoop -- Continue the main runLoop
--             SwitchToVimMode -> do
--               modify (\m -> m { debuggerMode = VimMode }) -- Switch to VimMode
--               runLoop -- Continue the main runLoop
--             NoAction       -> runLoop -- Continue the main runLoop
--         else do
--           continue <- fdxSingleCycle -- Execute one instruction
--           nextMachineState <- get -- Get state after instruction execution
--           -- Check if the machine is halted after executing the instruction
--           if halted nextMachineState
--             then do
--               liftIO $ putStrLn "\nMachine halted. Entering debugger."
--               put (nextMachineState { debuggerActive = True }) -- Activate debugger
--               handlePostInstructionChecks -- Call helper
--             else if not continue
--               then return () -- Stop if fdxSingleCycle returns False (not halted, but some other stop condition)
--               else handlePostInstructionChecks -- Call helper

-- | Handles post-instruction checks: halting, tracing, and breakpoints.
handlePostInstructionChecks :: FDX ()
handlePostInstructionChecks = do
  nextMachineState <- get -- Get the updated state after the instruction
  if halted nextMachineState
    then do
      -- Machine was halted, debugger is already active from the calling branch
      case debuggerMode nextMachineState of
        CommandMode -> void D.interactiveLoopHelper -- Enter CommandMode loop, discard result
        VimMode     -> void V.interactiveLoopHelper -- Enter VimMode loop, discard result
        VimCommandMode -> void V.interactiveLoopHelper -- Enter VimCommandMode loop, discard result
    else do
      -- Log registers and memory trace blocks if tracing is enabled
      when (enableTrace nextMachineState) $ do
          let currentPC_after = rPC (mRegs nextMachineState) -- Get PC after execution
          disassembled <- disassembleInstruction currentPC_after -- Use PC after execution
          putOutput "" -- Use console output instead of direct print
          putOutput (fst disassembled) -- Use console output instead of direct print
          let regOutput = D.logRegisters (mRegs nextMachineState) -- Capture register output
          mapM_ putOutput regOutput -- Use console output instead of direct print
          -- Log all memory trace blocks
          mapM_ (\(start, end, name) -> do
                   memOutput <- D.logMemoryRange start end name -- Capture memory trace output
                   mapM_ putOutput memOutput) (memoryTraceBlocks nextMachineState) -- Use console output

      -- Check for breakpoints after executing the instruction
      let currentPC = rPC (mRegs nextMachineState)
      if currentPC `elem` breakpoints nextMachineState
        then do
          liftIO $ putStrLn $ "\nBreakpoint hit at $" ++ showHex currentPC ""
          put (nextMachineState { debuggerActive = True }) -- Activate debugger
          -- Determine which debugger loop to enter based on the current mode
          case debuggerMode nextMachineState of
            CommandMode -> void D.interactiveLoopHelper -- Enter CommandMode loop, discard result
            VimMode     -> void V.interactiveLoopHelper -- Enter VimMode loop, discard result
        else
          runLoop -- Continue the main runLoop (execute next instruction)


-- | Runs the FDX monad, handling debugger state and the main execution loop.
runMachine :: Machine -> IO ((), Machine)
runMachine initialMachine = do
  liftIO $ putStrLn $ "Initial PC in runMachine: $" ++ showHex (rPC (mRegs initialMachine)) ""
  (result, finalMachine) <- runStateT (unFDX runLoop) initialMachine
  _ <- runStateT (unFDX (saveDebuggerState finalMachine)) finalMachine
  return (result, finalMachine)


-- | Sets up the initial state of the machine, including registers and memory.
-- It also loads debugger state and symbol files if paths are provided.
-- Note: This function no longer sets the PC, as it's handled by runEmulator
setupMachine :: Machine -> [(Word16, Word8)] -> Maybe FilePath -> Maybe FilePath -> IO Machine
setupMachine initialMachine memoryWrites maybeSymPath maybeDebugLogPath = do
    mem <- foldr (\(addr, val) acc -> acc >>= \m -> writeByte addr val m >> return m) (return $ mMem initialMachine) memoryWrites
    let machineWithMem = initialMachine { mMem = mem, debugLogPath = maybeDebugLogPath } -- Set debugLogPath here

    -- ath is provided
    (loadedBreakpoints, loadedMemBlocks) <- case maybeDebugLogPath of
        Just logPath -> fst <$> runStateT (unFDX (loadDebuggerState logPath)) initialMachine
        Nothing -> return ([], [])

    let machineWithLoadedState = machineWithMem { breakpoints = loadedBreakpoints, memoryTraceBlocks = loadedMemBlocks }

    -- Load symbol file if path is provided
    case maybeSymPath of
      Just symPath -> snd <$> runStateT (unFDX $ loadSymbolFile symPath) machineWithLoadedState
      Nothing -> return machineWithLoadedState

-- | Runs an emulation test with the given start address, load address, and bytecode.
runTest :: Word16 -> Word16 -> [Word8] -> IO ()
runTest startAddress actualLoadAddress byteCode = do
  putStrLn $ "Running emulation test starting at $" ++ showHex startAddress ""
  putStrLn $ "Loading bytecode at $" ++ showHex actualLoadAddress ""
  initialMachine <- newMachine

  -- Prepare memory writes - load bytecode at its actual load address
  let memoryWrites = zip [actualLoadAddress..] byteCode

  -- Setup the machine (PC is set by runEmulator now)
  -- Pass Nothing for symbol file path and debug log path in runTest
  setupMachine initialMachine memoryWrites Nothing Nothing >>= \setupResult -> do
    putStrLn "Emulator machine setup complete."

    -- Run the emulator with the specified start address
    (_, finalMachine) <- runEmulator startAddress setupResult

    putStrLn "\n--- Emulation Finished ---"
    putStrLn $ "Final Registers: " ++ show (mRegs finalMachine)
    putStrLn $ "Instructions Executed: " ++ show (instructionCount finalMachine)


-- | Initializes the emulator with the given starting address and bytecode, then enters interactive debugger mode.
-- It also loads symbol files if a path is provided.
runDebugger :: Word16 -> Word16 -> [Word8] -> Maybe FilePath -> IO Machine
runDebugger startAddress actualLoadAddress byteCode maybeSymPath = do
  putStrLn $ "Initializing debugger with code starting at $" ++ showHex startAddress ""
  putStrLn $ "Loading bytecode at $" ++ showHex actualLoadAddress ""
  initialMachine <- newMachine

  -- Prepare memory writes - load bytecode at its actual load address
  let memoryWrites = zip [actualLoadAddress..] byteCode

  -- Setup the machine, including loading symbols and debugger state
  setupMachine initialMachine memoryWrites maybeSymPath (Just "debugger_state.log") >>= \setupResult -> do -- Hardcode default log path
    putStrLn "Emulator machine setup complete."

    -- Set the starting PC and enter interactive debugger loop
    let machineWithStartPC = setupResult { 
          mRegs = (mRegs setupResult) { rPC = startAddress }, 
          debuggerActive = True,
          vimState = (vimState setupResult) { vsCursor = startAddress, vsViewStart = startAddress } -- Initialize vsCursor and vsViewStart
        }
    putStrLn "\nEntering interactive debugger."
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False


    (_, finalMachine) <- runStateT (unFDX runLoop) machineWithStartPC -- Start the main runLoop
    return finalMachine

-- Removed: -- | The main interactive debugger loop.
-- Removed: interactiveDebuggerLoop :: FDX ()
-- Removed: interactiveDebuggerLoop = interactiveLoopHelper "" -- Start with no last command

-- | Helper function for the interactive debugger loop, handling command input and execution.
-- Removed: -- | Helper function for the interactive debugger loop, handling command input and execution.
-- Removed: interactiveLoopHelper :: String -> FDX ()


-- Removed:
