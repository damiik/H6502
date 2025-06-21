{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger
  ( DebuggerAction(..) -- Export DebuggerAction
  , saveDebuggerState
  , loadDebuggerState
  , interactiveLoopHelper -- Exporting for CommandMode
  ) where
import Numeric ( readHex)
import Control.Monad.State (put, get, modify, runStateT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word16)
import Control.Exception (IOException, displayException, try, finally)
import Text.Printf (printf)
import System.IO (hFlush, stdout,  stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering, LineBuffering))

import MOS6502Emulator.Core
import MOS6502Emulator.Machine
import MOS6502Emulator.Debugger.Core ( DebuggerConsoleState(..), DebuggerAction(..)) -- Import from Debugger.Core
import MOS6502Emulator.Debugger.Commands
import MOS6502Emulator.Debugger.Utils() -- Removed logRegisters
import MOS6502Emulator.Display (renderScreen, putOutput, getKey, termHeight) -- Import from Display
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
  
  -- Use finally to ensure terminal settings are restored on exit
  machine <- get -- Get the current machine state
  (action, finalMachineState) <- liftIO $ Control.Exception.finally (runStateT (unFDX interactiveLoopHelperInternal) machine) (hSetEcho stdin True >> hSetBuffering stdin LineBuffering)
  return (action, finalMachineState)

interactiveLoopHelperInternal :: FDX DebuggerAction
interactiveLoopHelperInternal = do
  machine <- get
  if _halted machine
    then return QuitEmulator -- Machine halted, return QuitEmulator action
    else do
      let currentConsoleState = _mConsoleState machine
      let consoleStateWithPrompt = currentConsoleState { _inputBuffer = "> ", _cursorPosition = 2 }
      modify (\m -> m { _mConsoleState = consoleStateWithPrompt }) -- Update machine's console state

      -- Check if help is currently displayed and if Enter is pressed
      let helpTextLines = _helpLines (_mConsoleState machine)
      let helpTextScrollPos = _helpScrollPos (_mConsoleState machine)

      if not (null helpTextLines) then do
        key <- liftIO getKey -- Read a single key
        if key == '\n' then do
          let availableContentHeight = termHeight - 2
          let newScrollPos = helpTextScrollPos + availableContentHeight
          if newScrollPos >= length helpTextLines then do
            -- Reached end of help, clear help state
            modify (\m -> m { _mConsoleState = (_mConsoleState m) { _helpLines = [], _helpScrollPos = 0, _inputBuffer = "", _cursorPosition = 0 } })
          else do
            -- Scroll to next page of help
            modify (\m -> m { _mConsoleState = (_mConsoleState m) { _helpScrollPos = newScrollPos, _inputBuffer = "", _cursorPosition = 0 } })

          updatedMachine <- get -- Get the updated machine state
          renderScreen updatedMachine (take availableContentHeight $ drop newScrollPos helpTextLines) -- Re-render with new help scroll position
          interactiveLoopHelperInternal -- Continue the loop
        else do
          -- If help is displayed but not Enter, clear help and process as normal command
          modify (\m -> m { _mConsoleState = (_mConsoleState m) { _helpLines = [], _helpScrollPos = 0 } })
          -- Prepend the consumed key to the input buffer for readCommandWithInitialInput.
          -- This ensures that if a key was pressed while help was active (and it wasn't Enter),
          -- it is correctly processed as the start of the next command, preventing a stall.
          let initialInput = [key]
          -- Now proceed with command input, passing the initial input
          (commandToExecute, consumeNewline) <- readCommandWithInitialInput initialInput
          (action, output) <- handleCommand commandToExecute
          machineAfterCommand <- get
          let updatedConsoleState = (_mConsoleState machineAfterCommand) { _outputLines = _outputLines (_mConsoleState machineAfterCommand) ++ ["> " ++ commandToExecute] ++ output, _inputBuffer = "", _cursorPosition = 0, _lastCommand = commandToExecute }
          let updatedMachine = machineAfterCommand { _mConsoleState = updatedConsoleState }
          put updatedMachine
          case action of
            ContinueLoop _ -> do
              renderScreen updatedMachine (_outputLines updatedConsoleState)
              interactiveLoopHelperInternal
            NoAction -> do
              renderScreen updatedMachine (_outputLines updatedConsoleState)
              interactiveLoopHelperInternal
            ExecuteStep _  -> do
              renderScreen updatedMachine (_outputLines updatedConsoleState) -- Render the output from handleCommand immediately
              return action -- Let the main runLoop handle the execution and rendering
            ExitDebugger   -> modify (\m -> m { _debuggerActive = False }) >> return action
            QuitEmulator   -> modify (\m -> m { _halted = True }) >> return action
            SwitchToVimMode -> do
              modify (\m -> m { _debuggerMode = VimMode }) >> return action
            SwitchToCommandMode -> do
              modify (\m -> m { _debuggerMode = CommandMode })
              interactiveLoopHelperInternal
      else do
        -- No help being displayed, proceed with normal command input.
        -- In this case, there's no pre-consumed key, so we pass an empty initial input.
        (commandToExecute, consumeNewline) <- readCommandWithInitialInput ""
        (action, output) <- handleCommand commandToExecute
        machineAfterCommand <- get
        let updatedConsoleState = (_mConsoleState machineAfterCommand) { _outputLines = _outputLines (_mConsoleState machineAfterCommand) ++ ["> " ++ commandToExecute] ++ output, _inputBuffer = "", _cursorPosition = 0, _lastCommand = commandToExecute }
        let updatedMachine = machineAfterCommand { _mConsoleState = updatedConsoleState }
        put updatedMachine
        case action of
          ContinueLoop _ -> do
            renderScreen updatedMachine (_outputLines updatedConsoleState)
            interactiveLoopHelperInternal
          NoAction -> do
            renderScreen updatedMachine (_outputLines updatedConsoleState)
            interactiveLoopHelperInternal
          ExecuteStep _  -> do
            renderScreen updatedMachine (_outputLines updatedConsoleState) -- Render the output from handleCommand immediately
            return action -- Let the main runLoop handle the execution and rendering
          ExitDebugger   -> modify (\m -> m { _debuggerActive = False }) >> return action
          QuitEmulator   -> modify (\m -> m { _halted = True }) >> return action
          SwitchToVimMode -> do
            modify (\m -> m { _debuggerMode = VimMode }) >> return action
          SwitchToCommandMode -> interactiveLoopHelperInternal

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
        case (parseHexWord startStr, parseHexWord endStr) of
            (Just startAddr, Just endAddr) ->
                let name = if null rest then Nothing else Just (unwords rest)
                in [(startAddr, endAddr, name)]
            _ -> [] -- Invalid address format or not enough arguments
    parseBlock _ = [] -- Invalid block format (e.g., only one address)
