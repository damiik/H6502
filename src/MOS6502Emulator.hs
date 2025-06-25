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

import Control.Monad.State (get, modify, put, runStateT)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Numeric (showHex)
import System.IO (hSetEcho, hSetBuffering, BufferMode(NoBuffering), stdin, hReady)
import Data.Word ( Word8, Word16 )
import qualified Data.Map.Strict as Map
import Control.Lens
import MOS6502Emulator.Lenses as L

import MOS6502Emulator.Machine (loadSymbolFile, fdxSingleCycle)
import MOS6502Emulator.Core (Machine(..), FDX(..), _instructionCount, _cycleCount, DebuggerMode(..))
import MOS6502Emulator.Memory
import MOS6502Emulator.Registers
import qualified MOS6502Emulator.Debugger as D
import MOS6502Emulator.Debugger (saveDebuggerState, loadDebuggerState, DebuggerAction(..))
import MOS6502Emulator.Debugger.Actions (executeStepAndRender)
import MOS6502Emulator.Debugger.Console (initialConsoleState)
import MOS6502Emulator.Display (renderScreen)
import MOS6502Emulator.Debugger.VimMode.Core (initialVimState, VimState(..))
import qualified MOS6502Emulator.Debugger.VimMode.Enhanced as V
import qualified System.Console.ANSI as ANSI

-- | Initializes a new 6502 machine state
-- | Initializes a new 6502 machine state
newMachine :: IO Machine
newMachine = do
  mem <- memory  -- 64KB of memory initialized by MOS6502Emulator.Memory
  let regs = mkRegisters
  return Machine { _mRegs = regs, _mMem = mem, _halted = False, _instructionCount = 0, _cycleCount = 0, _enableTrace = True, _traceMemoryStart = 0x0000, _traceMemoryEnd = 0x00FF, _breakpoints = [], _debuggerActive = False, _memoryTraceBlocks = [], _lastDisassembledAddr = 0x0000, _labelMap = Map.empty, _debugLogPath = Nothing, _debuggerMode = CommandMode, _pcHistory = [], _storedAddresses = Map.empty, _redoHistory = [], _vimState = initialVimState, _mConsoleState = initialConsoleState} -- Initialize new fields, including debuggerMode, pcHistory, storedAddresses, redoHistory, and mConsoleState

-- | The main fetch-decode-execute loop. Runs `fdxSingleCycle` repeatedly until emulation stops.
runFDXLoop :: FDX ()
runFDXLoop = do
  continue <- fdxSingleCycle -- This will now refer to the imported fdxSingleCycle
  when continue runFDXLoop

-- | Runs the emulator until the machine is halted
-- | Runs the emulator with the given machine state and starting PC
runEmulator :: Word16 -> Machine -> IO ((), Machine)
runEmulator startPC initialMachine = do
  let machineWithStartPC = initialMachine { _mRegs = (_mRegs initialMachine) { _rPC = startPC } }
  runMachine machineWithStartPC


-- | The inner loop that manages debugger activation and instruction execution.
runLoop :: FDX ()
runLoop = do
  machine <- get
  if _halted machine
    then return () -- Stop if machine is halted
    else do
      if _debuggerActive machine
        then do
          -- Debugger is active, enter the interactive loop based on mode
          (action, updatedMachineState) <- case _debuggerMode machine of
            CommandMode -> D.interactiveLoopHelper
            VimMode -> V.interactiveLoopHelper
            VimCommandMode -> V.interactiveLoopHelper

          put updatedMachineState -- Update the machine state with the one returned by the helper

          -- Handle the action returned by the debugger loop
          case action of
            ExecuteStepAction -> do
              executeStepAndRender -- Use the new unified function
              runLoop -- Continue the main runLoop (which will re-evaluate _debuggerActive)
            ExitDebuggerAction -> do
              modify (\m -> m { _debuggerActive = False }) -- Exit debugger mode
              runLoop -- Continue the main runLoop
            QuitEmulatorAction -> modify (\m -> m { _halted = True }) -- Halt the emulator
            SetDebuggerModeAction _ -> do
              -- The mode is already updated in updatedMachineState, just continue loop
              runLoop
            RenderScreenAction -> do
              renderScreen updatedMachineState (view L.outputLines (view L.mConsoleState updatedMachineState))
              runLoop
            UpdateConsoleOutputAction _ _ -> do
              -- Console output is already updated in updatedMachineState, just continue loop
              runLoop
            NoDebuggerAction -> runLoop -- Simply continue the main runLoop
        else do -- if not _debuggerActive machine
          machineBeforeStep <- get
          let currentPC = _rPC (_mRegs machineBeforeStep)
          if currentPC `elem` _breakpoints machineBeforeStep
            then do
              liftIO $ putStrLn $ "\nBreakpoint hit at $" ++ showHex currentPC ""
              put (machineBeforeStep { _debuggerActive = True }) -- Activate debugger
              liftIO $ putStrLn "Debugger activated due to breakpoint. Entering debugger loop."
              updatedMachine <- get
              renderScreen updatedMachine [] -- Explicitly render the screen to ensure UI is visible
              liftIO clearInputBuffer -- Clear input buffer on debugger re-entry
              runLoop -- Re-enter runLoop, which will now go into the _debuggerActive branch
            else do
              executeStepAndRender -- Use the new unified function
              unless (_halted machineBeforeStep) runLoop -- Continue if not halted


-- | Runs the FDX monad, handling debugger state and the main execution loop.
runMachine :: Machine -> IO ((), Machine)
runMachine initialMachine = do
  liftIO $ putStrLn $ "Initial PC in runMachine: $" ++ showHex (_rPC (_mRegs initialMachine)) ""
  (result, finalMachine) <- runStateT (unFDX runLoop) initialMachine
  _ <- runStateT (unFDX (saveDebuggerState finalMachine)) finalMachine
  return (result, finalMachine)


-- | Sets up the initial state of the machine, including registers and memory.
-- It also loads debugger state and symbol files if paths are provided.
-- Note: This function no longer sets the PC, as it's handled by runEmulator
setupMachine :: Machine -> [(Word16, Word8)] -> Maybe FilePath -> Maybe FilePath -> IO Machine
setupMachine initialMachine memoryWrites maybeSymPath maybeDebugLogPath = do
    mem <- foldr (\(addr, val) acc -> acc >>= \m -> writeByte addr val m >> return m) (return $ _mMem initialMachine) memoryWrites
    let machineWithMem = initialMachine { _mMem = mem, _debugLogPath = maybeDebugLogPath } -- Set _debugLogPath here

    -- ath is provided
    (loadedBreakpoints, loadedMemBlocks) <- case maybeDebugLogPath of
        Just logPath -> fst <$> runStateT (unFDX (loadDebuggerState logPath)) initialMachine
        Nothing -> return ([], [])

    let machineWithLoadedState = machineWithMem { _breakpoints = loadedBreakpoints, _memoryTraceBlocks = loadedMemBlocks }

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
    putStrLn $ "Final Registers: " ++ show (_mRegs finalMachine)
    putStrLn $ "Instructions Executed: " ++ show (_instructionCount finalMachine)


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
          _mRegs = (_mRegs setupResult) { _rPC = startAddress }, 
          _debuggerActive = True,
          _vimState = (_vimState setupResult) { vsCursor = startAddress, vsViewStart = startAddress } -- Initialize vsCursor and vsViewStart
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
