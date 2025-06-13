{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger
  ( DebuggerAction(..) -- Export DebuggerAction
  , logRegisters -- Keep exporting for now, though its usage might change
  , saveDebuggerState
  , loadDebuggerState
  , interactiveLoopHelper -- Exporting for CommandMode
  , isExecuteStep -- Export isExecuteStep
  ) where
import Numeric ( readHex)
import Control.Monad.State (put, get, modify, runStateT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word16)
import Control.Exception (IOException, displayException, try, finally)
import Text.Printf (printf)
import System.IO (hFlush, stdout,  stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering, LineBuffering))
import Control.Monad (unless) -- Added for conditional rendering
import Data.List (isInfixOf) -- Added for substring checking

import MOS6502Emulator.Core
import MOS6502Emulator.Machine
import MOS6502Emulator.Debugger.Core ( DebuggerConsoleState(..), DebuggerAction(..)) -- Import from Debugger.Core
import MOS6502Emulator.Debugger.Commands
import MOS6502Emulator.Debugger.Console (renderScreen, putOutput,  getKey, termHeight) -- Import console I/O functions, added putString and termHeight
import MOS6502Emulator.Debugger.Utils(logRegisters)
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
interactiveLoopHelper :: FDX DebuggerAction -- Changed signature
interactiveLoopHelper = do
  -- Ensure terminal settings are correct for debugger interaction
  liftIO $ hSetBuffering stdin NoBuffering
  liftIO $ hSetEcho stdin False
  
  -- Use finally to ensure terminal settings are restored on exit
  machine <- get -- Get the current machine state
  (action, _) <- liftIO $ Control.Exception.finally (runStateT (unFDX interactiveLoopHelperInternal) machine) (hSetEcho stdin True >> hSetBuffering stdin LineBuffering)
  return action

interactiveLoopHelperInternal :: FDX DebuggerAction
interactiveLoopHelperInternal = do
  machine <- get
  if halted machine
    then return QuitEmulator -- Machine halted, return QuitEmulator action
    else do
      let currentConsoleState = mConsoleState machine
      let consoleStateWithPrompt = currentConsoleState { inputBuffer = "> ", cursorPosition = 2 }
      modify (\m -> m { mConsoleState = consoleStateWithPrompt }) -- Update machine's console state

      -- Check if help is currently displayed and if Enter is pressed
      let helpTextLines = helpLines (mConsoleState machine)
      let helpTextScrollPos = helpScrollPos (mConsoleState machine)

      if not (null helpTextLines) then do
        key <- liftIO getKey -- Read a single key
        if key == '\n' then do
          let availableContentHeight = termHeight - 2
          let newScrollPos = helpTextScrollPos + availableContentHeight
          if newScrollPos >= length helpTextLines then do
            -- Reached end of help, clear help state
            modify (\m -> m { mConsoleState = (mConsoleState m) { helpLines = [], helpScrollPos = 0, inputBuffer = "", cursorPosition = 0 } })
          else do
            -- Scroll to next page of help
            modify (\m -> m { mConsoleState = (mConsoleState m) { helpScrollPos = newScrollPos, inputBuffer = "", cursorPosition = 0 } })

          updatedMachine <- get -- Get the updated machine state
          renderScreen updatedMachine -- Re-render with new help scroll position
          interactiveLoopHelperInternal -- Continue the loop
        else do
          -- If help is displayed but not Enter, clear help and process as normal command
          modify (\m -> m { mConsoleState = (mConsoleState m) { helpLines = [], helpScrollPos = 0 } })
          -- Prepend the consumed key to the input buffer for readCommandWithInitialInput.
          -- This ensures that if a key was pressed while help was active (and it wasn't Enter),
          -- it is correctly processed as the start of the next command, preventing a stall.
          let initialInput = [key]
          -- Now proceed with command input, passing the initial input
          (commandToExecute, consumeNewline) <- readCommandWithInitialInput initialInput
          (action, output) <- handleCommand commandToExecute
          let filteredOutput = if isExecuteStep action
                               then filterOutRegisterLines output -- Filter output for step actions
                               else output
          machineAfterCommand <- get
          let updatedMachine = machineAfterCommand { mConsoleState = (mConsoleState machineAfterCommand) { outputLines = outputLines (mConsoleState machineAfterCommand) ++ ["> " ++ commandToExecute] ++ filteredOutput, inputBuffer = "", cursorPosition = 0, lastCommand = commandToExecute } }
          put updatedMachine
          -- Conditional rendering: only render if not executing a step
          unless (isExecuteStep action) $ renderScreen updatedMachine
          case action of
            ContinueLoop _ -> interactiveLoopHelperInternal
            ExecuteStep _  -> return action
            ExitDebugger                 -> modify (\m -> m { debuggerActive = False }) >> return action
            QuitEmulator                 -> modify (\m -> m { halted = True }) >> return action
            NoAction                     -> interactiveLoopHelperInternal
            SwitchToVimMode              -> do
              modify (\m -> m { debuggerMode = VimMode }) >> return action
            SwitchToCommandMode          -> interactiveLoopHelperInternal
      else do
        -- No help being displayed, proceed with normal command input.
        -- In this case, there's no pre-consumed key, so we pass an empty initial input.
        (commandToExecute, consumeNewline) <- readCommandWithInitialInput ""
        (action, output) <- handleCommand commandToExecute
        let filteredOutput = if isExecuteStep action
                               then filterOutRegisterLines output -- Filter output for step actions
                               else output
        machineAfterCommand <- get
        let updatedMachine = machineAfterCommand { mConsoleState = (mConsoleState machineAfterCommand) { outputLines = outputLines (mConsoleState machineAfterCommand) ++ ["> " ++ commandToExecute] ++ filteredOutput, inputBuffer = "", cursorPosition = 0, lastCommand = commandToExecute } }
        put updatedMachine
        -- Conditional rendering: only render if not executing a step
        unless (isExecuteStep action) $ renderScreen updatedMachine
        case action of
          ContinueLoop _ -> interactiveLoopHelperInternal
          ExecuteStep _  -> return action
          ExitDebugger                 -> modify (\m -> m { debuggerActive = False }) >> return action
          QuitEmulator                 -> modify (\m -> m { halted = True }) >> return action
          NoAction                     -> interactiveLoopHelperInternal
          SwitchToVimMode              -> do
            modify (\m -> m { debuggerMode = VimMode }) >> return action
          SwitchToCommandMode          -> interactiveLoopHelperInternal

-- Helper function to check if an action is ExecuteStep
isExecuteStep :: DebuggerAction -> Bool
isExecuteStep (ExecuteStep _) = True
isExecuteStep _ = False

-- Helper function to filter out register-related lines from command output
filterOutRegisterLines :: [String] -> [String]
filterOutRegisterLines = filter (\s -> not (
    "PC: $" `isInfixOf` s ||
    "AC: $" `isInfixOf` s ||
    "X: $" `isInfixOf` s ||
    "Y: $" `isInfixOf` s ||
    "SP: $" `isInfixOf` s ||
    "SR: " `isInfixOf` s ||
    "a [$" `isInfixOf` s || -- For memory trace blocks
    "Registers: (will be updated after step)" `isInfixOf` s
  ))



-- | Saves the current debugger state (breakpoints and memory trace blocks) to a file.
saveDebuggerState :: Machine -> FDX () -- Changed to FDX
saveDebuggerState machine = case debugLogPath machine of
    Just filePath -> do
        let content = "breakpoints: " ++ unwords [printf "%04X" bp | bp <- breakpoints machine] ++ "\n" ++
                      "memory_trace_blocks: " ++ unlines (map (\(start, end, name) ->
                                                                    printf "%04X %04X" start end ++
                                                                    case name of
                                                                        Just n -> " " ++ n
                                                                        Nothing -> "")
                                                                   (memoryTraceBlocks machine))
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
