module MOS6502Emulator
  ( runEmulator
  , runTest
  , runDebugger
  , newMachine
  , setupMachine
  , Machine(..)
  , Memory
  , Registers
  -- , instructionCount -- Export instructionCount
  ) where

import Control.Monad.State (get, modify, put, runStateT) -- Import runStateT
import Control.Monad (when, unless) -- Import the 'when', 'unless', and 'void' functions
import Control.Monad.IO.Class (liftIO)
import Numeric (showHex) -- Removed readHex as it's not used here
import System.IO (hSetEcho, hSetBuffering, BufferMode(NoBuffering), stdin, hReady) -- Import hReady and getChar
import Data.Word ( Word8, Word16 )
import qualified Data.Map.Strict as Map -- For Map.empty

import MOS6502Emulator.Machine (loadSymbolFile, fdxSingleCycle)
import MOS6502Emulator.Core (Machine(..), FDX(..), instructionCount, cycleCount, DebuggerMode(..))
import MOS6502Emulator.Memory
import MOS6502Emulator.Registers
import qualified MOS6502Emulator.Debugger as D
import MOS6502Emulator.Debugger (saveDebuggerState, loadDebuggerState, DebuggerAction(..))
import MOS6502Emulator.Debugger.Actions (executeStepAndRender) -- Import logMemoryRange from Actions
import MOS6502Emulator.Debugger.Console (initialConsoleState) -- Only import initialConsoleState
import MOS6502Emulator.Display (renderScreen) -- Import renderScreen from Display
import MOS6502Emulator.Debugger.VimMode.Core (initialVimState, VimState(..))
import qualified MOS6502Emulator.Debugger.VimMode.Enhanced as V
import qualified System.Console.ANSI as ANSI -- Import for clearScreen and setCursorPosition

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
          -- Debugger is active, enter the interactive loop based on mode
          (action, updatedMachineState) <- case debuggerMode machine of
            CommandMode -> D.interactiveLoopHelper
            VimMode -> V.interactiveLoopHelper
            VimCommandMode -> V.interactiveLoopHelper

          put updatedMachineState -- Update the machine state with the one returned by the helper

          -- Handle the action returned by the debugger loop
          case action of
            ContinueLoop _ -> runLoop -- Stay in the debugger loop
            ExecuteStep _  -> do
              executeStepAndRender -- Use the new unified function
              runLoop -- Continue the main runLoop (which will re-evaluate debuggerActive)
            ExitDebugger   -> do
              modify (\m -> m { debuggerActive = False }) -- Exit debugger mode
              runLoop -- Continue the main runLoop
            QuitEmulator   -> modify (\m -> m { halted = True }) -- Halt the emulator
            SwitchToCommandMode -> do
              -- The mode is already updated in updatedMachineState, just continue loop
              runLoop
            SwitchToVimMode -> do
              -- The mode is already updated in updatedMachineState, just continue loop
              machineAfterModeChange <- get -- Get the machine state after mode change
              _ <- renderScreen machineAfterModeChange []-- Render the screen immediately
              runLoop -- Continue the main runLoop
            SwitchToVimCommandMode -> do
              -- The mode is already updated in updatedMachineState, just continue loop
              runLoop
            NoAction       -> runLoop -- Simply continue the main runLoop
        else do -- if not debuggerActive machine
          machineBeforeStep <- get
          let currentPC = rPC (mRegs machineBeforeStep)
          if currentPC `elem` breakpoints machineBeforeStep
            then do
              liftIO $ putStrLn $ "\nBreakpoint hit at $" ++ showHex currentPC ""
              put (machineBeforeStep { debuggerActive = True }) -- Activate debugger
              liftIO $ putStrLn "Debugger activated due to breakpoint. Entering debugger loop."
              updatedMachine <- get
              renderScreen updatedMachine [] -- Explicitly render the screen to ensure UI is visible
              liftIO clearInputBuffer -- Clear input buffer on debugger re-entry
              runLoop -- Re-enter runLoop, which will now go into the debuggerActive branch
            else do
              executeStepAndRender -- Use the new unified function
              unless (halted machineBeforeStep) runLoop -- Continue if not halted


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

    -- Clear screen before starting the debugger loop
    liftIO ANSI.clearScreen
    liftIO $ ANSI.setCursorPosition 0 0
    -- Clear input buffer before starting the debugger loop
    clearInputBuffer

    (_, finalMachine) <- runStateT (unFDX runLoop) machineWithStartPC -- Start the main runLoop
    return finalMachine

-- Helper function to clear the input buffer
clearInputBuffer :: IO ()
clearInputBuffer = do
  ready <- hReady stdin
  when ready $ do
    _ <- getChar
    clearInputBuffer
