{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger
  ( DebuggerAction(..) -- Export DebuggerAction
  , saveDebuggerState
  , loadDebuggerState
  , interactiveLoopHelper -- Exporting for CommandMode
  ) where
import Numeric ( readHex)
import Control.Monad.State (put, get, runStateT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word16)
import Control.Exception (IOException, displayException, try, finally)
import Text.Printf (printf)
import System.IO (hFlush, stdout,  stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering, LineBuffering))
import qualified System.Console.ANSI as ANSI
import Control.Lens
import MOS6502Emulator.Lenses as L

import MOS6502Emulator.Core (FDX, Machine(..), unFDX)
import MOS6502Emulator.Machine (fdxSingleCycle)
import MOS6502Emulator.Debugger.Core ( DebuggerConsoleState(..), DebuggerAction(..), DebuggerMode(..), DebuggerState(..), DebuggerInput(..))
import MOS6502Emulator.Debugger.Commands (handleCommandPure)
import MOS6502Emulator.Debugger.Utils (parseDebuggerCommand, parseHexWord)
import MOS6502Emulator.Display (renderScreen, putOutput, getKey, termHeight)
import MOS6502Emulator.Debugger.Actions (logRegisters, logMemoryRangePure, handlePostInstructionChecks)
import MOS6502Emulator.Debugger.VimMode.HandleKey (handleVimNormalModeKey) -- Import handleVimNormalModeKey
import MOS6502Emulator.Debugger.VimMode.Core (VimState(..)) -- Import VimState for vsInCommandMode and vsCommandBuffer

-- | Helper to read a command line, optionally leaving the newline in the buffer.
-- This function now accepts an `initialInput` string, which is useful when a key
-- has already been consumed (e.g., by `getKey` for help scrolling) but needs
-- to be part of the subsequent command input.
-- This resolves the stalling issue where a character was consumed by `getKey`
-- and then `readCommand` (now `readCommandWithInitialInput`) would wait for
-- a newline that never arrived, causing the debugger to hang.
readCommandWithInitialInput :: String -> FDX (String, Bool) -- Returns (command, shouldConsumeNewline)
readCommandWithInitialInput initialInput = do
  liftIO $ hSetEcho stdin False -- Turn off echo for raw input
  machine <- get -- Get the current machine state
  (cmd, consumeNewline) <- liftIO $ Control.Exception.finally (fst <$> runStateT (unFDX (readChars initialInput)) machine) (hSetEcho stdin True)
  return (cmd, consumeNewline)
  where
    readChars :: String -> FDX (String, Bool)
    readChars currentInput = do
      c <- liftIO getKey
      liftIO $ putChar c -- Echo the character back to the console
      liftIO $ hFlush stdout
      if c == '\n'
        then do
          -- If the command is "v", we don't consume the newline here.
          -- The VimMode's interactiveLoopHelper will consume it.
          let shouldConsumeNewline = (currentInput /= "v")
          return (currentInput, shouldConsumeNewline)
        else readChars (currentInput ++ [c])
-- | Helper function for the interactive debugger loop, handling command input and execution.
interactiveLoopHelper :: FDX (DebuggerAction, Machine) -- Changed signature to return Machine
interactiveLoopHelper = do
  -- Ensure terminal settings are correct for debugger interaction
  liftIO $ hSetBuffering stdin NoBuffering
  liftIO $ hSetEcho stdin False
  
  -- Use finally to ensure terminal settings are restored and screen is cleared on exit
  machine <- get -- Get the current machine state
  (action, finalMachineState) <- liftIO $ Control.Exception.finally (runStateT (unFDX (interactiveLoopHelperInternal (DebuggerWaitingForInput CommandMode))) machine) (hSetEcho stdin True >> hSetBuffering stdin LineBuffering >> ANSI.clearScreen >> ANSI.setCursorPosition 0 0)
  return (action, finalMachineState)

-- | Pure transition function for the debugger state machine.
nextState :: DebuggerState -> Machine -> DebuggerInput -> (DebuggerState, Machine, [DebuggerAction])
nextState currentState machine input =
  case currentState of
    DebuggerWaitingForInput currentMode ->
      case input of
        CommandInput cmdStr ->
          let parsedCommand = parseDebuggerCommand cmdStr
              (machineAfterCommand, outputLines, debuggerAction) = handleCommandPure machine parsedCommand
              -- Determine the next debugger state based on the actions
              nextDebuggerState = case debuggerAction of
                    QuitEmulatorAction -> DebuggerQuittingEmulator
                    ExitDebuggerAction -> DebuggerExitingLoop
                    SetDebuggerModeAction newMode -> DebuggerWaitingForInput newMode
                    _ -> if currentMode == VimCommandMode then DebuggerWaitingForInput VimMode else DebuggerWaitingForInput currentMode -- Always return to VimMode from VimCommandMode
              actions = [UpdateConsoleOutputAction cmdStr outputLines, RenderScreenAction, debuggerAction]
              -- If coming from VimCommandMode and returning to VimMode, clear the command buffer and inCommandMode flag
              finalMachine = machineAfterCommand
                             & L.vimState .~ (if currentMode == VimCommandMode && nextDebuggerState == DebuggerWaitingForInput VimMode
                                              then (view L.vimState machineAfterCommand) { vsInCommandMode = False, vsCommandBuffer = "" }
                                              else (view L.vimState machineAfterCommand))
                             & L.debuggerMode .~ (case nextDebuggerState of DebuggerWaitingForInput newMode -> newMode; _ -> currentMode) -- Explicitly set debuggerMode
          in (nextDebuggerState, finalMachine, actions)
        KeyInput key -> -- This case is for help scrolling or direct key input in VimMode
          case currentMode of
            VimMode ->
              -- This case should ideally not be reached if VimKeyProcessed is used
              -- but as a fallback, it can indicate an unhandled key in VimMode
              (currentState, machine, [NoDebuggerAction])
            _ -> -- For CommandMode, if a single key is pressed, treat it as part of a command
              let newConsoleState = (_mConsoleState machine) { _inputBuffer = _inputBuffer (_mConsoleState machine) ++ [key] }
                  newMachine = machine & L.mConsoleState .~ newConsoleState
              in (currentState, newMachine, [NoDebuggerAction])
        VimKeyProcessed (debuggerAction, outputLines, vimState, consoleState, debuggerMode) ->
          let nextDebuggerState = case debuggerAction of
                QuitEmulatorAction -> DebuggerQuittingEmulator
                ExitDebuggerAction -> DebuggerExitingLoop
                SetDebuggerModeAction newMode -> DebuggerWaitingForInput newMode
                _ -> DebuggerWaitingForInput VimMode -- Stay in VimMode
              actions = [UpdateConsoleOutputAction "" outputLines, RenderScreenAction, debuggerAction] -- Command string is empty for single key
              newMachine = machine & L.vimState .~ vimState & L.mConsoleState .~ consoleState & L.debuggerMode .~ debuggerMode
          in (nextDebuggerState, newMachine, actions)

    DebuggerDisplayingHelp currentMode scrollPos ->
      case input of
        KeyInput '\n' ->
          let helpTextLines = _helpLines (_mConsoleState machine)
              availableContentHeight = termHeight - 2
              newScrollPos = scrollPos + availableContentHeight
          in if newScrollPos >= length helpTextLines
            then -- Reached end of help, clear help state and return to waiting for input
              let newConsoleState = (_mConsoleState machine) { _helpLines = [], _helpScrollPos = 0, _inputBuffer = "", _cursorPosition = 0 }
                  newMachine = machine & L.mConsoleState .~ newConsoleState
              in (DebuggerWaitingForInput currentMode, newMachine, [RenderScreenAction])
            else -- Scroll to next page of help
              let newConsoleState = (_mConsoleState machine) { _helpScrollPos = newScrollPos, _inputBuffer = "", _cursorPosition = 0 }
                  newMachine = machine & L.mConsoleState .~ newConsoleState
              in (DebuggerDisplayingHelp currentMode newScrollPos, newMachine, [RenderScreenAction])
        KeyInput key ->
          -- If help is displayed but not Enter, clear help and process as normal command
          let newConsoleState = (_mConsoleState machine) { _helpLines = [], _helpScrollPos = 0 }
              machineWithClearedHelp = machine & L.mConsoleState .~ newConsoleState
              -- Prepend the consumed key to the input buffer for the next command.
              initialInput = [key]
          in nextState (DebuggerWaitingForInput currentMode) machineWithClearedHelp (CommandInput initialInput) -- Treat as command input

    _ -> (currentState, machine, [NoDebuggerAction]) -- Fallback for other states or unexpected inputs

interactiveLoopHelperInternal :: DebuggerState -> FDX DebuggerAction
interactiveLoopHelperInternal debuggerState = do
  machine <- get
  if _halted machine
    then return QuitEmulatorAction -- Machine halted, return QuitEmulator action
    else do
      let currentConsoleState = _mConsoleState machine
      let consoleStateWithPrompt = currentConsoleState { _inputBuffer = "> ", _cursorPosition = 2 }
      L.mConsoleState .= consoleStateWithPrompt -- Update machine's console state

      -- Render the screen initially based on the current machine state
      -- renderScreen machine (view L.outputLines (view L.mConsoleState machine))

      (nextDebuggerState, finalMachineState, actions) <- case debuggerState of
        DebuggerWaitingForInput currentMode -> do
          (inputEvent, newMachineState) <- case currentMode of
            VimMode -> do
              key <- liftIO getKey
              ((debuggerAction, outputLines, vimState, consoleState, debuggerMode), updatedMachine) <- liftIO $ runStateT (unFDX (handleVimNormalModeKey key (view L.vimState machine) (view L.mConsoleState machine) (view L.debuggerMode machine))) machine
              return (VimKeyProcessed (debuggerAction, outputLines, vimState, consoleState, debuggerMode), updatedMachine)
            _ -> do
              (commandToExecute, _) <- readCommandWithInitialInput ""
              return (CommandInput commandToExecute, machine) -- No machine state change for other modes here
          
          -- Apply the machine state change from VimMode key handling before calling nextState
          put newMachineState
          
          let (newState, finalMachineAfterInput, newActions) = nextState debuggerState newMachineState inputEvent
          return (newState, finalMachineAfterInput, newActions)
        DebuggerDisplayingHelp currentMode scrollPos -> do
          key <- liftIO getKey -- Read a single key
          let (newState, newMachine, newActions) = nextState debuggerState machine (KeyInput key)
          return (newState, newMachine, newActions)
        _ -> return (debuggerState, machine, [NoDebuggerAction]) -- Should not happen if state machine is well-defined

      put finalMachineState -- Update the machine state in the FDX monad

      -- Execute actions returned by nextState
      mapM_ executeDebuggerAction actions

      -- Continue the loop based on the next debugger state
      case nextDebuggerState of
        DebuggerWaitingForInput _ -> interactiveLoopHelperInternal nextDebuggerState
        DebuggerDisplayingHelp _ _ -> interactiveLoopHelperInternal nextDebuggerState
        DebuggerExitingLoop -> return ExitDebuggerAction
        DebuggerQuittingEmulator -> return QuitEmulatorAction

-- | Executes a DebuggerAction, performing side effects.
executeDebuggerAction :: DebuggerAction -> FDX ()
executeDebuggerAction action = do
  machine <- get
  case action of
    ExecuteStepAction -> do
      _ <- fdxSingleCycle -- Execute one instruction
      currentMachine <- get
      -- Capture register output (logRegisters expects Registers, use mRegs gives FDX Registers)
      regs <- use L.mRegs
      let regOutput = logRegisters regs
      -- Capture memory trace output (logMemoryRange returns FDX [String], so mapM over it)
      memBlocks <- use L.memoryTraceBlocks
      let memTraceOutputList = map (\(start, end, name) -> logMemoryRangePure start end name currentMachine) memBlocks
      let memTraceOutput = concat memTraceOutputList
      liftIO ANSI.clearScreen -- Aggressive clear after step
      liftIO $ ANSI.setCursorPosition 0 0 -- Reset cursor
      renderScreen currentMachine (regOutput ++ memTraceOutput)
      handlePostInstructionChecks -- Handle tracing and halting checks
    ExitDebuggerAction -> L.debuggerActive .= False
    QuitEmulatorAction -> L.halted .= True
    RenderScreenAction -> renderScreen machine (view L.outputLines (view L.mConsoleState machine))
    UpdateConsoleOutputAction cmd output -> do
      let updatedConsoleState = (view L.mConsoleState machine) { _outputLines = (view L.outputLines (view L.mConsoleState machine)) ++ ["> " ++ cmd] ++ output, _inputBuffer = "", _cursorPosition = 0, _lastCommand = cmd }
      L.mConsoleState .= updatedConsoleState
    SetDebuggerModeAction newMode -> do
      L.debuggerMode .= newMode
      -- Clear help lines when changing debugger mode
      L.mConsoleState . L.helpLines .= []
      L.mConsoleState . L.helpScrollPos .= 0
    NoDebuggerAction -> return ()

-- | Saves the current debugger state (breakpoints and memory trace blocks) to a file.
saveDebuggerState :: Machine -> FDX () -- Changed to FDX
saveDebuggerState machine = case _debugLogPath machine of
    Just filePath -> do
        let content = "breakpoints: " ++ unwords [printf "%04X" bp | bp <- _breakpoints machine] ++ "\n" ++
                      "memory_trace_blocks: " ++ unlines (map (\(start, end, name) ->
                                                                    printf "%04X %04X" start end ++
                                                                    case name of
                                                                        Just n -> " " ++ n
                                                                        Nothing -> "")
                                                                   (_memoryTraceBlocks machine))
        result <- liftIO (try (writeFile filePath content) :: IO (Either IOException ()))
        case result of
            Left e -> do
                putOutput $ "Error saving debugger state: " ++ displayException e
                return ()
            Right _ -> do
                putOutput $ "Debugger state saved to: " ++ filePath
                return ()
    Nothing -> return ()

-- | Loads debugger state (breakpoints and memory trace blocks) from a file.
loadDebuggerState :: FilePath -> FDX ([Word16], [(Word16, Word16, Maybe String)]) -- Changed to FDX
loadDebuggerState filePath = do
    fileContentOrError <- liftIO $ try (readFile filePath)
    case fileContentOrError of
        Left e -> do
            putOutput $ "Error loading debugger state: " ++ show (e :: IOException)
            return ([], [])
        Right fileContent -> do
            let ls = lines fileContent
            let breakpoints = parseBreakpoints ls
            let memBlocks = parseMemBlocks ls
            putOutput $ "Loaded debugger state from: " ++ filePath
            return (breakpoints, memBlocks)
  where
    parseBreakpoints :: [String] -> [Word16]
    parseBreakpoints = concatMap (parseWords . drop 1 . words) . filter (("breakpoints:" ==) . head . words)
    
    parseMemBlocks :: [String] -> [(Word16, Word16, Maybe String)]
    parseMemBlocks = concatMap (parseBlock . drop 1 . words) . filter (("memory_trace_blocks:" ==) . head . words)
    
    parseWords :: [String] -> [Word16]
    parseWords = mapMaybe (fmap fst . listToMaybe . readHex)
    
    parseBlock :: [String] -> [(Word16, Word16, Maybe String)]
    parseBlock [] = []
    parseBlock (startStr:endStr:rest) =
        case (MOS6502Emulator.Debugger.Utils.parseHexWord startStr, MOS6502Emulator.Debugger.Utils.parseHexWord endStr) of
            (Just startAddr, Just endAddr) ->
                let name = if null rest then Nothing else Just (unwords rest)
                in [(startAddr, endAddr, name)]
            _ -> [] -- Invalid address format or not enough arguments
    parseBlock _ = [] -- Invalid block format (e.g., only one address)
