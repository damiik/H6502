module MOS6502Emulator.Debugger.VimMode.Enhanced (
    interactiveLoopHelper
    , renderVimScreen
) where


import Data.Word
import qualified Data.Map as Map
import Numeric (showHex)
import System.IO (hFlush, stdout, stdin, hSetEcho, hReady)
import Data.List (stripPrefix) -- Added for stripPrefix

import Control.Monad.State (get, put, MonadIO (liftIO), modify) -- Added modify
import Control.Monad.Trans.State (runStateT)
import MOS6502Emulator.Core (Machine(..), FDX, fetchByteMem, unFDX, mConsoleState) -- Removed getRegisters, parseHexWord
import MOS6502Emulator.Registers (Registers(..), rPC) 
import MOS6502Emulator.Debugger.VimMode.Core
import MOS6502Emulator.Debugger.VimMode.HandleKey (handleVimNormalModeKey) -- Changed import
import MOS6502Emulator.Debugger.Console(getKey, getInput, termHeight, termWidth, putOutput, putString, printTwoColumns) -- Updated import
import MOS6502Emulator.Debugger.Core (DebuggerAction(..), DebuggerConsoleState(..), DebuggerMode(..)) -- New import, ensuring DebuggerAction is here
import MOS6502Emulator.DissAssembler(disassembleInstructions, formatHex16, formatHex8, unwords)
import MOS6502Emulator.Debugger.Commands (handleBreak, handleMemTrace, handleSetPC, handleSetReg8, handleFill, handleDisassemble, handleCommand) -- Moved handle* imports
import MOS6502Emulator.Debugger (logRegisters, isExecuteStep) -- logRegisters remains in Debugger.hs, import isExecuteStep
import MOS6502Emulator.Debugger.Utils (parseHexWord, parseHexByte, setPC_, getRegisters) -- Import from Debugger.Utils
import qualified System.Console.ANSI as ANSI
import Control.Monad (void, unless) -- Added void and unless

-- | Parse a hex string to a byte value
-- parseHexByte :: String -> Maybe Word8
-- parseHexByte s = case readHex s of
--     [(n, "")] -> if n <= 0xFF then Just (fromIntegral n) else Nothing
--     _ -> Nothing



-- | Handle breakpoint commands (similar to handleBreakVim from VimMode.hs)
handleBreakVim :: VimState -> FDX (DebuggerAction, [String], VimState)
handleBreakVim vimState = do
  putString "\nBreakpoints (a: add, d: delete):" -- Changed to putString
  key <- liftIO getKey
  case key of
    'a' -> do
      putString "Add Breakpoint (address):" -- Changed to putString
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleBreak args ""
      return (action, ["Breakpoints (a: add, d: delete): " ++ [key], "Add Breakpoint (address): " ++ input] ++ output, vimState { vsMessage = last output })
    'd' -> do
      putString "Delete Breakpoint (address):" -- Changed to putString
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleBreak args ""
      return (action, ["Breakpoints (a: add, d: delete): " ++ [key], "Delete Breakpoint (address): " ++ input] ++ output, vimState { vsMessage = last output })
    _ -> return (NoAction, ["Invalid Breakpoint command"], vimState { vsMessage = "Invalid Breakpoint command" })

-- | Handle memory trace commands
handleMemTraceVim :: VimState -> FDX (DebuggerAction, [String], VimState)
handleMemTraceVim vimState = do
  putString "\nMemory Trace (a: add, d: delete):" -- Changed to putString
  key <- liftIO getKey
  case key of
    'a' -> do
      putString "Add Memory Trace Block (start end [name]): " -- Changed to putString
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleMemTrace args ""
      return (action, ["Memory Trace (a: add, d: delete): " ++ [key], "Add Memory Trace Block (start end [name]): " ++ input] ++ output, vimState { vsMessage = last output })
    'd' -> do
      putString "Delete Memory Trace Block (start end [name]): " -- Changed to putString
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleMemTrace args ""
      return (action, ["Memory Trace (a: add, d: delete): " ++ [key], "Delete Memory Trace Block (start end [name]): " ++ input] ++ output, vimState { vsMessage = last output })
    _ -> return (NoAction, ["Invalid Memory Trace command"], vimState { vsMessage = "Invalid Memory Trace command" })

-- | Handle stored address commands
handleAddressVim :: VimState -> FDX (DebuggerAction, [String], VimState)
handleAddressVim vimState = do
  putString "\nStored Addresses (s: store, g: goto):" -- Changed to putString
  key <- liftIO getKey
  case key of
    's' -> do
      putString "Store Current PC (key):" -- Changed to putString
      keyChar <- liftIO getKey
      machine <- get
      let currentPC = rPC (mRegs machine)
      put (machine { storedAddresses = Map.insert keyChar currentPC (storedAddresses machine) })
      return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "Stored PC $" ++ showHex currentPC "" ++ " at key '" ++ [keyChar] ++ "'"], vimState { vsMessage = "Stored PC at '" ++ [keyChar] ++ "'" })
    'g' -> do
      putString "Goto Stored Address (key):" -- Changed to putString
      keyChar <- liftIO getKey
      machine <- get
      case Map.lookup keyChar (storedAddresses machine) of
        Just addr -> do
          put (machine { mRegs = (mRegs machine) { rPC = addr } })
          (disassembledOutput, _) <- disassembleInstructions addr 1
          return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "PC set to $" ++ showHex addr ""] ++ disassembledOutput, vimState { vsCursor = addr, vsViewStart = addr, vsMessage = "Moved to $" ++ showHex addr "" })
        Nothing -> return (NoAction, ["No address stored at key '" ++ [keyChar] ++ "'"], vimState { vsMessage = "No address stored at '" ++ [keyChar] ++ "'" })
    _ -> return (NoAction, ["Invalid Stored Address command"], vimState { vsMessage = "Invalid Stored Address command" })

-- | Main dispatcher for Vim key presses.
handleVimKey :: Char -> Machine -> VimState -> FDX (DebuggerAction, [String], VimState, DebuggerConsoleState, DebuggerMode)
handleVimKey key machine vimState = 
  if key == '\n' && vsInCommandMode vimState || vsInCommandMode vimState
    then handleVimCommandModeKey key machine vimState
    else do
      let vimState' = vimState { vsMessage = "" }
      (action, output, newVimState, updatedConsoleState, updatedDebuggerMode) <- handleVimNormalModeKey key vimState' (mConsoleState machine) (debuggerMode machine)
      return (action, output, newVimState, updatedConsoleState, updatedDebuggerMode)

-- | Handles key presses when in Vim Command Mode (after ':').
handleVimCommandModeKey :: Char -> Machine -> VimState -> FDX (DebuggerAction, [String], VimState, DebuggerConsoleState, DebuggerMode)
handleVimCommandModeKey key machine vimState = do
  let currentCommandBuffer = vsCommandBuffer vimState
  let currentConsoleState = mConsoleState machine
  let currentDebuggerMode = debuggerMode machine
  let availableContentHeight = termHeight - 2 -- Space for two columns and status line

  case key of
    '\n' -> do -- Enter key
      let helpTextLines = helpLines currentConsoleState
      let helpTextScrollPos = helpScrollPos currentConsoleState

      if not (null helpTextLines) then do
        -- If help is being displayed, scroll it
        let newScrollPos = helpTextScrollPos + availableContentHeight
        if newScrollPos >= length helpTextLines then do
          -- Reached end of help, clear help state
          let newConsoleState = currentConsoleState {
                                  outputLines = outputLines currentConsoleState ++ helpTextLines, -- Add help to output
                                  helpLines = [],
                                  helpScrollPos = 0,
                                  vimCommandInputBuffer = "", inputBuffer = "" }
          let newVimState = vimState { vsInCommandMode = False, vsCommandBuffer = "" }
          return (NoAction, [], newVimState, newConsoleState, VimMode)
        else do
          -- Scroll to next page of help
          let newConsoleState = currentConsoleState { helpScrollPos = newScrollPos, vimCommandInputBuffer = "", inputBuffer = "" }
          return (NoAction, [], vimState, newConsoleState, currentDebuggerMode)
      else do
        -- No help being displayed, execute command
        liftIO $ hSetEcho stdin False -- Disable echo after command execution
        let commandToExecute = drop 1 currentCommandBuffer -- Remove leading ':'
        (action, output) <- handleCommand commandToExecute
        -- Get the machine state *after* handleCommand has potentially modified it
        machineAfterCommand <- get 

        let newVimState = vimState { vsInCommandMode = False, vsCommandBuffer = "" }
        -- Use machineAfterCommand to get the latest console state
        let newConsoleState = (mConsoleState machineAfterCommand) { vimCommandInputBuffer = "", inputBuffer = "" }
        let newDebuggerMode = VimMode -- Switch back to VimMode
        return (action, output, newVimState, newConsoleState, newDebuggerMode)
    '\DEL' -> do -- Backspace key
      if length currentCommandBuffer > 1
        then do
          let newCommandBuffer = init currentCommandBuffer
          let newConsoleState = currentConsoleState { vimCommandInputBuffer = newCommandBuffer }
          return (NoAction, [], vimState { vsCommandBuffer = newCommandBuffer }, newConsoleState, currentDebuggerMode)
        else do -- Only ':' is left, exit command mode
          let newVimState = vimState { vsInCommandMode = False, vsCommandBuffer = "" }
          let newConsoleState = currentConsoleState { vimCommandInputBuffer = "", inputBuffer = "" }
          let newDebuggerMode = VimMode -- Exit command mode, go back to VimMode
          return (NoAction, [], newVimState, newConsoleState, newDebuggerMode)
    _ -> do -- Any other character
      let newCommandBuffer = currentCommandBuffer ++ [key]
      let newConsoleState = currentConsoleState { vimCommandInputBuffer = newCommandBuffer }
      -- The updateVimCommandLine function needs the current machine state for rendering,
      -- but we are not modifying the machine state here, only returning new console state.
      -- So, we pass the current machine state to updateVimCommandLine.
      updateVimCommandLine machine newCommandBuffer
      return (NoAction, [], vimState { vsCommandBuffer = newCommandBuffer }, newConsoleState, currentDebuggerMode)

interactiveLoopHelper :: FDX (DebuggerAction, VimState)
interactiveLoopHelper = do
  machine <- get
  if halted machine
    then return (QuitEmulator, vimState machine)
    else do
      let currentVimState = vimState machine
      key <- liftIO getKey
      (action, output, newVimState, newConsoleState, newDebuggerMode) <- handleVimKey key machine currentVimState

      -- Determine if helpLines should be cleared
      let shouldClearHelpNow =
            not (null (helpLines newConsoleState)) && -- Help is currently displayed
            key /= '\n' &&                            -- The key pressed was not for scrolling help
            newDebuggerMode == VimMode                -- We are in VimMode (not VimCommandMode where :h is entered)

      let consoleStateToUse =
            if shouldClearHelpNow
            then newConsoleState { helpLines = [], helpScrollPos = 0 }
            else newConsoleState
 
      -- Get the machine state *after* handleVimKey (which may have called handleCommand and modified global state)
      machineAfterHandleKey <- get
    
      -- Apply the specific components returned from handleVimKey to the latest machine state
      let finalMachineState = machineAfterHandleKey {
                mConsoleState = consoleStateToUse { outputLines = outputLines consoleStateToUse ++ output },
                vimState = newVimState,
                debuggerMode = newDebuggerMode
              }
    
      put finalMachineState -- Update the global state with the fully merged state
    
      -- Use the final state for rendering
      let updatedMachine = finalMachineState
      unless (isExecuteStep action) $ do -- Only render if not executing a step
        if vsInCommandMode newVimState
          then updateVimCommandLine updatedMachine (vsCommandBuffer newVimState)
          else renderVimScreen updatedMachine newVimState

      case action of
        ContinueLoop _ -> interactiveLoopHelper
        ExecuteStep _ -> do
          machineAfterExecution <- get
          return (action, newVimState { vsCursor = rPC (mRegs machineAfterExecution), vsViewStart = rPC (mRegs machineAfterExecution) })
        ExitDebugger -> return (action, newVimState)
        QuitEmulator -> return (action, newVimState)
        NoAction -> interactiveLoopHelper
        SwitchToCommandMode -> return (action, newVimState)
        SwitchToVimMode -> do
          return (action, newVimState)
        SwitchToVimCommandMode -> do
          -- Update the debuggerMode immediately
          modify (\m -> m { debuggerMode = VimCommandMode })
          -- Get the updated machine state for rendering
          updatedMachine <- get
          -- Render the screen to show the mode change and prompt
          renderVimScreen updatedMachine newVimState
          interactiveLoopHelper


-- | Render screen with vim-specific cursor and status
renderVimScreen :: Machine -> VimState -> FDX ()
renderVimScreen machine vimState = do
  liftIO ANSI.hideCursor
  liftIO ANSI.clearScreen
  liftIO $ ANSI.setCursorPosition 0 0

  let consoleState = mConsoleState machine
  let availableContentHeight = termHeight - 2 -- Space for two columns and status line
  let maxOutputLines = availableContentHeight
  
  let helpTextLines = helpLines consoleState
  let helpTextScrollPos = helpScrollPos consoleState

  let rightColumnContent = if null helpTextLines
                           then reverse $ take maxOutputLines $ reverse (outputLines consoleState)
                           else take maxOutputLines $ drop helpTextScrollPos helpTextLines

  let linesPerPage = availableContentHeight -- Use available content height for main view
  let cursorPos = vsCursor vimState
  let viewStart = vsViewStart vimState

  -- Adjust viewStart to keep cursor in view
  let adjustedViewStart = case vsViewMode vimState of
        CodeView -> if cursorPos < viewStart
                    then max 0 (cursorPos - fromIntegral (linesPerPage `div` 2))
                    else if cursorPos >= viewStart + fromIntegral linesPerPage -- Adjusted for full screen height
                          then cursorPos - fromIntegral (linesPerPage - 1) -- Adjusted to ensure cursor is on last line
                          else viewStart
        _ -> if cursorPos < viewStart
              then max 0 (cursorPos - fromIntegral (linesPerPage * 16 `div` 2))
              else if cursorPos >= viewStart + fromIntegral (linesPerPage * 16)
                   then cursorPos - fromIntegral (linesPerPage * 8)
                   else viewStart
  
  -- Generate left column content based on view mode
  leftColumnLines <- case vsViewMode vimState of
    CodeView -> do
      ((disassembledLines, _), _) <- liftIO $ runStateT (unFDX $ disassembleInstructions adjustedViewStart linesPerPage) machine
      return $ zipWith (\line addr ->
                       if addr == cursorPos
                       then "\x1b[7m" ++ line ++ "\x1b[0m" -- Highlight cursor line
                       else line
                     ) disassembledLines [adjustedViewStart ..]

    MemoryView -> do
      let startAddr = adjustedViewStart
      let endAddr = min 0xFFFF (startAddr + fromIntegral (linesPerPage * 16 - 1))
      bytes <- liftIO $ runStateT (unFDX $ mapM fetchByteMem [startAddr .. endAddr]) machine
      let byteLines = chunkBytes (fst bytes) 16
      return $ zipWith (\addr line -> formatHex16 addr ++ ": " ++ unwords (map formatHex8 line)) [startAddr, startAddr + 16 ..] byteLines
      
    RegisterView -> do
      regs <- liftIO $ runStateT (unFDX getRegisters) machine
      return $ logRegisters (fst regs)

    StackView -> do
      let sp = rSP (mRegs machine)
      let stackStart = fromIntegral sp + 0x0100
      let stackEnd = min 0x01FF (stackStart + fromIntegral (linesPerPage * 16 - 1))
      bytes <- liftIO $ runStateT (unFDX $ mapM fetchByteMem [stackStart .. stackEnd]) machine
      let byteLines = chunkBytes (fst bytes) 16
      return $ zipWith (\addr line -> formatHex16 addr ++ ": " ++ unwords (map formatHex8 line)) [stackStart, stackStart + 16 ..] byteLines

  -- Print two columns
  liftIO $ printTwoColumns termWidth leftColumnLines rightColumnContent

  -- Status line
  liftIO $ ANSI.setCursorPosition (termHeight - 2) 0
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  let modeDisplay = case debuggerMode machine of -- Use debuggerMode from machine
        CommandMode -> " COMMAND "
        VimMode -> " VIM "
        VimCommandMode -> " VIM COMMAND " -- New mode display
  liftIO $ putStr modeDisplay
  let regs = mRegs machine
  let regDisplay = "A=" ++ showHex (rAC regs) "" ++ " X=" ++ showHex (rX regs) "" ++ " Y=" ++ showHex (rY regs) "" ++ " PC=" ++ showHex (rPC regs) ""
  liftIO $ putStr regDisplay
  let cursorDisplay = " $" ++ showHex cursorPos ""
  liftIO $ putStr cursorDisplay
  let countDisplay = case vsCount vimState of
        Just n -> " [" ++ show n ++ "]"
        Nothing -> ""
  liftIO $ putStr countDisplay
  let operatorDisplay = case vsOperator vimState of

        Just op -> 
          case op of
            DeleteOp -> " d"
            ChangeOp -> " c"
            YankOp -> " y"
          -- Just op -> 
        Nothing -> ""
  liftIO $ putStr operatorDisplay
  let spacerLength = max 0 (termWidth - length modeDisplay - length regDisplay - length cursorDisplay - length countDisplay - length operatorDisplay)
  liftIO $ putStr $ replicate spacerLength ' '
  liftIO $ ANSI.setSGR [ANSI.Reset]
  liftIO $ hFlush stdout

  -- Command/message line
  liftIO $ ANSI.setCursorPosition (termHeight - 1) 0
  liftIO $ ANSI.setSGR [ANSI.Reset]
  case debuggerMode machine of
    VimCommandMode -> do
      -- Display colon prompt and user input without double colon
      liftIO $ putStr (":" ++ vimCommandInputBuffer consoleState)
      liftIO $ ANSI.setCursorPosition (termHeight - 1) (length (vimCommandInputBuffer consoleState) + 1)
    _ -> do
      liftIO $ putStr (vsMessage vimState)
      liftIO $ ANSI.setCursorPosition (termHeight - 1) (length (vsMessage vimState))
  liftIO $ hFlush stdout
  liftIO ANSI.showCursor

-- | Chunk a list into sublists of given size
chunkBytes :: [Word8] -> Int -> [[Word8]]
chunkBytes [] _ = []
chunkBytes xs n = take n xs : chunkBytes (drop n xs) n

-- | Update only the command line during Vim command mode input
updateVimCommandLine :: Machine -> String -> FDX ()
updateVimCommandLine machine cmdBuffer = do
  let lineNum = termHeight - 1
  liftIO $ ANSI.setCursorPosition lineNum 0
  liftIO ANSI.clearLine
  liftIO $ putStr cmdBuffer  -- cmdBuffer already contains the colon
  liftIO $ ANSI.setCursorPosition lineNum (length cmdBuffer)
  liftIO $ hFlush stdout
