module MOS6502Emulator.Debugger.VimMode.Enhanced (
    interactiveLoopHelper
    , renderVimScreen
) where


import Data.Word
import qualified Data.Map as Map
import Numeric (showHex)
import System.IO (hFlush, stdout, stdin, hSetEcho, hReady, hSetBuffering, BufferMode(NoBuffering, LineBuffering))
import Data.List (stripPrefix) -- Added for stripPrefix
import Control.Lens -- Import Control.Lens
import MOS6502Emulator.Lenses -- Import our custom lenses

import Control.Monad.State (get, put, MonadIO (liftIO), modify) -- Added modify
import Control.Monad.Trans.State (runStateT)
import Control.Exception (finally) -- Import finally
import MOS6502Emulator.Core (Machine(..), FDX, fetchByteMem, unFDX, _mConsoleState) -- Removed getRegisters, parseHexWord
import MOS6502Emulator.Registers (Registers(..), _rPC) 
import MOS6502Emulator.Debugger.VimMode.Core
import MOS6502Emulator.Debugger.VimMode.HandleKey (handleVimNormalModeKey) -- Changed import
import MOS6502Emulator.Display(getKey, getInput, termHeight, termWidth, putOutput, putString, printTwoColumns, renderScreen) -- Updated import
import MOS6502Emulator.Debugger.Core (DebuggerAction(..), DebuggerConsoleState(..), DebuggerMode(..)) -- New import, ensuring DebuggerAction is here
import MOS6502Emulator.DissAssembler(disassembleInstructions, formatHex16, formatHex8, unwords)
import MOS6502Emulator.Debugger.Commands (handleBreak, handleMemTrace, handleSetPC, handleSetReg8, handleFill, handleDisassemble, handleCommand) -- Moved handle* imports
import MOS6502Emulator.Debugger.Actions (executeStepAndRender, logRegisters, logMemoryRange) -- Import executeStepAndRender and logging functions
import MOS6502Emulator.Debugger.Utils (parseHexWord, parseHexByte, getRegisters) -- Import from Debugger.Utils
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
      currentPC <- use (mRegs . rPC) -- Use lens to get PC
      storedAddresses %= Map.insert keyChar currentPC -- Use lens to update storedAddresses
      return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "Stored PC $" ++ showHex currentPC "" ++ " at key '" ++ [keyChar] ++ "'"], vimState { vsMessage = "Stored PC at '" ++ [keyChar] ++ "'" })
    'g' -> do
      putString "Goto Stored Address (key):" -- Changed to putString
      keyChar <- liftIO getKey
      -- Use lens to lookup stored address
      maybeAddr <- use (storedAddresses . to (Map.lookup keyChar))
      case maybeAddr of
        Just addr -> do
          mRegs . rPC .= addr -- Use lens to set PC
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
      (action, output, newVimState, updatedConsoleState, updatedDebuggerMode) <- handleVimNormalModeKey key vimState' (_mConsoleState machine) (_debuggerMode machine)
      return (action, output, newVimState, updatedConsoleState, updatedDebuggerMode)

-- | Handles key presses when in Vim Command Mode (after ':').
handleVimCommandModeKey :: Char -> Machine -> VimState -> FDX (DebuggerAction, [String], VimState, DebuggerConsoleState, DebuggerMode)
handleVimCommandModeKey key machine vimState = do
  let currentCommandBuffer = vsCommandBuffer vimState
  let currentConsoleState = _mConsoleState machine
  let currentDebuggerMode = _debuggerMode machine
  let availableContentHeight = termHeight - 2 -- Space for two columns and status line

  case key of
    '\n' -> do -- Enter key
      let helpTextLines = _helpLines currentConsoleState
      let helpTextScrollPos = _helpScrollPos currentConsoleState

      if not (null helpTextLines) then do
        -- If help is being displayed, scroll it
        let newScrollPos = helpTextScrollPos + availableContentHeight
        if newScrollPos >= length helpTextLines then do
          -- Reached end of help, clear help state
          let newConsoleState = currentConsoleState {
                                  _outputLines = _outputLines currentConsoleState ++ helpTextLines, -- Add help to output
                                  _helpLines = [],
                                  _helpScrollPos = 0,
                                  _vimCommandInputBuffer = "", _inputBuffer = "" }
          let newVimState = vimState { vsInCommandMode = False, vsCommandBuffer = "" }
          return (NoAction, [], newVimState, newConsoleState, VimMode)
        else do
          -- Scroll to next page of help
          let newConsoleState = currentConsoleState { _helpScrollPos = newScrollPos, _vimCommandInputBuffer = "", _inputBuffer = "" }
          return (NoAction, [], vimState, newConsoleState, currentDebuggerMode)
      else do
        -- No help being displayed, execute command
        liftIO $ hSetEcho stdin False -- Disable echo after command execution
        let commandToExecute = drop 1 currentCommandBuffer -- Remove leading ':'
        (actionFromCommand, output) <- handleCommand commandToExecute
        -- Get the machine state *after* handleCommand has potentially modified it
        machineAfterCommand <- get 

        let newVimState = vimState { vsInCommandMode = False, vsCommandBuffer = "" }
        -- Use machineAfterCommand to get the latest console state
        let newConsoleState = (_mConsoleState machineAfterCommand) { _vimCommandInputBuffer = "", _inputBuffer = "" }
        let newDebuggerMode = VimMode -- Switch back to VimMode

        -- If handleCommand executed a step, it already rendered. Return NoAction to prevent double rendering.
        return (actionFromCommand, output, newVimState, newConsoleState, newDebuggerMode)
    '\DEL' -> do -- Backspace key
      if length currentCommandBuffer > 1
        then do
          let newCommandBuffer = init currentCommandBuffer
          let newConsoleState = currentConsoleState { _vimCommandInputBuffer = newCommandBuffer }
          return (NoAction, [], vimState { vsCommandBuffer = newCommandBuffer }, newConsoleState, currentDebuggerMode)
        else do -- Only ':' is left, exit command mode
          let newVimState = vimState { vsInCommandMode = False, vsCommandBuffer = "" }
          let newConsoleState = currentConsoleState { _vimCommandInputBuffer = "", _inputBuffer = "" }
          let newDebuggerMode = VimMode -- Exit command mode, go back to VimMode
          return (NoAction, [], newVimState, newConsoleState, newDebuggerMode)
    _ -> do -- Any other character
      let newCommandBuffer = currentCommandBuffer ++ [key]
      let newConsoleState = currentConsoleState { _vimCommandInputBuffer = newCommandBuffer }
      -- The updateVimCommandLine function needs the current machine state for rendering,
      -- but we are not modifying the machine state here, only returning new console state.
      -- So, we pass the current machine state to updateVimCommandLine.
      updateVimCommandLine machine newCommandBuffer
      return (NoAction, [], vimState { vsCommandBuffer = newCommandBuffer }, newConsoleState, currentDebuggerMode)

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
  haltedState <- use halted
  if haltedState
    then return QuitEmulator
    else do
      currentVimState <- use vimState
      key <- liftIO getKey
      currentMachine <- get -- Get machine state for handleVimKey
      (action, output, newVimState, newConsoleState, newDebuggerMode) <- handleVimKey key currentMachine currentVimState

      -- Determine if helpLines should be cleared
      let shouldClearHelpNow =
            not (null (_helpLines newConsoleState)) && -- Help is currently displayed
            key /= '\n' &&                            -- The key pressed was not for scrolling help
            newDebuggerMode == VimMode                -- We are in VimMode (not VimCommandMode where :h is entered)

      let consoleStateToUse =
            if shouldClearHelpNow
            then newConsoleState { _helpLines = [], _helpScrollPos = 0 }
            else newConsoleState
 
      -- Apply the specific components returned from handleVimKey to the global state using lenses
      mConsoleState .= consoleStateToUse { _outputLines = _outputLines consoleStateToUse ++ output }
      vimState .= newVimState
      debuggerMode .= newDebuggerMode
    
      -- Use the current state for rendering
      updatedMachine <- get
      if vsInCommandMode newVimState
        then updateVimCommandLine updatedMachine (vsCommandBuffer newVimState)
        else renderVimScreen updatedMachine newVimState

      case action of
        ContinueLoop _ -> interactiveLoopHelperInternal
        ExecuteStep _ -> do
          -- Let the main runLoop handle the execution and rendering.
          -- Update vimState cursor for the next render.
          -- machineAfterExecution <- get -- No longer needed, state is updated by lenses
          return action
        ExitDebugger -> return action
        QuitEmulator -> return action
        NoAction -> interactiveLoopHelperInternal
        SwitchToCommandMode -> do
          debuggerMode .= CommandMode -- Update the debuggerMode immediately to CommandMode
          updatedMachine <- get -- Get the updated machine state for rendering
          renderVimScreen updatedMachine newVimState
          return action
        SwitchToVimMode -> do
          return action
        SwitchToVimCommandMode -> do
          debuggerMode .= VimCommandMode -- Update the debuggerMode immediately
          updatedMachine <- get -- Get the updated machine state for rendering
          renderVimScreen updatedMachine newVimState
          interactiveLoopHelperInternal


-- | Render screen with vim-specific cursor and status
renderVimScreen :: Machine -> VimState -> FDX ()
renderVimScreen machine vimState = do
  let consoleState = _mConsoleState machine
  let availableContentHeight = termHeight - 2
  let maxOutputLines = availableContentHeight
  
  let helpTextLines = _helpLines consoleState
  let helpTextScrollPos = _helpScrollPos consoleState

  actualRightColumnContent <-
    if not (null helpTextLines) then
      return $ take maxOutputLines $ drop helpTextScrollPos helpTextLines
    else case vsViewMode vimState of
      CodeView -> return $ reverse $ take maxOutputLines $ reverse (_outputLines consoleState)
      MemoryView -> do
        let startAddr = vsViewStart vimState
        let endAddr = min 0xFFFF (startAddr + fromIntegral (maxOutputLines * 16 - 1))
        bytes <- liftIO $ runStateT (unFDX $ mapM fetchByteMem [startAddr .. endAddr]) machine
        let byteLines = chunkBytes (fst bytes) 16
        return $ zipWith (\addr line -> formatHex16 addr ++ ": " ++ unwords (map formatHex8 line)) [startAddr, startAddr + 16 ..] byteLines
      RegisterView -> do
        regs <- liftIO $ runStateT (unFDX getRegisters) machine
        return $ logRegisters (fst regs)
      StackView -> do
        let sp = _rSP (_mRegs machine)
        let stackStart = fromIntegral sp + 0x0100
        let stackEnd = min 0x01FF (stackStart + fromIntegral (maxOutputLines * 16 - 1))
        bytes <- liftIO $ runStateT (unFDX $ mapM fetchByteMem [stackStart .. stackEnd]) machine
        let byteLines = chunkBytes (fst bytes) 16
        return $ zipWith (\addr line -> formatHex16 addr ++ ": " ++ unwords (map formatHex8 line)) [stackStart, stackStart + 16 ..] byteLines

  renderScreen machine actualRightColumnContent

  -- Status line
  liftIO $ ANSI.setCursorPosition (termHeight - 2) 0
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  let modeDisplay = case _debuggerMode machine of -- Use debuggerMode from machine
        CommandMode -> " COMMAND "
        VimMode -> " VIM "
        VimCommandMode -> " VIM COMMAND " -- New mode display
  liftIO $ putStr modeDisplay
  let regs = _mRegs machine
  let regDisplay = "A=" ++ showHex (_rAC regs) "" ++ " X=" ++ showHex (_rX regs) "" ++ " Y=" ++ showHex (_rY regs) "" ++ " PC=" ++ showHex (_rPC regs) ""
  liftIO $ putStr regDisplay
  let cursorDisplay = " $" ++ showHex (vsCursor vimState) ""
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
  case _debuggerMode machine of
    VimCommandMode -> do
      -- Display colon prompt and user input without double colon
      liftIO $ putStr (":" ++ _vimCommandInputBuffer consoleState)
      liftIO $ ANSI.setCursorPosition (termHeight - 1) (length (_vimCommandInputBuffer consoleState) + 1)
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
