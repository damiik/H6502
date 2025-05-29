module MOS6502Emulator.Debugger.VimModeHandleKey
  ( handleVimKey
  ) where
  
import Control.Monad.State (StateT, get, put, modify, liftIO, runStateT)
import System.IO (hFlush, stdout, stdin, hSetEcho)
import Data.Word (Word8, Word16)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Numeric (showHex)

import MOS6502Emulator.Core(Machine(..), FDX, fetchByteMem, getRegisters, parseHexWord, unFDX, mRegs, mConsoleState, parseHexByte) -- Removed DebuggerAction, added parseHexByte
import MOS6502Emulator.Machine (setPC_)
import MOS6502Emulator.Debugger.Types (DebuggerAction(..), DebuggerConsoleState(..)) -- Ensure DebuggerAction is imported from here
import MOS6502Emulator.Registers(Registers(..))
import MOS6502Emulator.DissAssembler(disassembleInstructions, disassembleInstruction, InstructionInfo(..), opcodeMap)
import MOS6502Emulator.Debugger(handleMemTrace, handleBreak, handleDisassemble, handleSetPC, handleSetReg8, handleFill, logRegisters )
import MOS6502Emulator.Debugger.VimModeCore ( VimState(..), Motion(..), Action(..), ViewMode(..), RepeatableCommand(..), parseCount)
import MOS6502Emulator.Debugger.VimModeExecute (executeMotion, executeAction)
import MOS6502Emulator.Debugger.Console(getKey, getInput, termHeight, termWidth, putOutput, putString)
import qualified System.Console.ANSI as ANSI

handleVimKey :: Char -> VimState -> FDX (DebuggerAction, [String], VimState)
handleVimKey key vimState = do
  machine <- get
  let currentPos = vsCursor vimState
  let count = fromMaybe 1 (vsCount vimState)
  
  case (vsOperator vimState, key) of
    -- Operator-pending mode
    (Just op, 'j') | op `elem` ["d", "c", "y"] -> do
      newPos <- executeMotion (NextInstruction count) currentPos
      let action = case op of
            "d" -> Delete (NextInstruction count)
            "c" -> Change (NextInstruction count)
            "y" -> Yank (NextInstruction count)
            _ -> error "Invalid operator" -- Shouldn't happen
      (newPos', output) <- executeAction action currentPos vimState
      let newYankBuffer = if op == "y"
            then Map.insert (vsRegister vimState) (maybe [] id (Map.lookup '"' (vsYankBuffer vimState))) (vsYankBuffer vimState)
            else vsYankBuffer vimState
      return (NoAction, output, vimState { vsCursor = newPos', vsCount = Nothing, vsOperator = Nothing, vsYankBuffer = newYankBuffer, vsLastChange = Just (RepeatAction action) })
    
    (Just op, 'k') | op `elem` ["d", "c", "y"] -> do
      newPos <- executeMotion (PrevInstruction count) currentPos
      let action = case op of
            "d" -> Delete (PrevInstruction count)
            "c" -> Change (PrevInstruction count)
            "y" -> Yank (PrevInstruction count)
            _ -> error "Invalid operator"
      (newPos', output) <- executeAction action currentPos vimState
      let newYankBuffer = if op == "y"
            then Map.insert (vsRegister vimState) (maybe [] id (Map.lookup '"' (vsYankBuffer vimState))) (vsYankBuffer vimState)
            else vsYankBuffer vimState
      return (NoAction, output, vimState { vsCursor = newPos', vsCount = Nothing, vsOperator = Nothing, vsYankBuffer = newYankBuffer, vsLastChange = Just (RepeatAction action) })
    
    -- Escape - clear pending operations or switch to CommandMode
    (Nothing, '\x1b') -> return (SwitchToCommandMode, ["Switching to Command Mode"], vimState { vsOperator = Nothing, vsCount = Nothing })
    (Just _, '\x1b') -> return (NoAction, [""], vimState { vsOperator = Nothing, vsCount = Nothing })
    
    -- Numbers - build count prefix
    (Nothing, c) | isDigit c -> do
      let currentCount = fromMaybe 0 (vsCount vimState)
      let newCount = currentCount * 10 + (read [c])
      return (NoAction, [""], vimState { vsCount = Just newCount })
    
    -- Operator commands
    (Nothing, c) | c `elem` ['d', 'c', 'y'] -> do
      return (NoAction, [""], vimState { vsOperator = Just [c] })

    -- Repeat last change
    (Nothing, '.') -> case vsLastChange vimState of
      Just (RepeatAction act) -> do
        (newPos, output) <- executeAction act currentPos vimState
        return (NoAction, output, vimState { vsCursor = newPos, vsMessage = head output })
      Just (RepeatMotion mot) -> do
        newPos <- executeMotion mot currentPos
        let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 3
              then newPos - fromIntegral ((termHeight - 3) * 2)
              else vsViewStart vimState
        return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing })
      Just RepeatStep -> do
        return (ExecuteStep "step", [], vimState { vsCount = Nothing, vsMessage = "Stepped one instruction" })
      Nothing -> return (NoAction, ["No previous change to repeat"], vimState { vsMessage = "No previous change to repeat" })
    
    -- Movement commands
    (Nothing, 'j') -> do
      putString "Naciśnięto 'j'" -- Debugowanie, changed to putString
      let motion = NextInstruction count
      newPos <- executeMotion motion currentPos
      let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 3
            then newPos - fromIntegral ((termHeight - 3) * 2)
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'k') -> do
      let motion = PrevInstruction count
      newPos <- executeMotion motion currentPos
      let newViewStart = if newPos < vsViewStart vimState
            then max 0 (newPos - fromIntegral (termHeight - 3))
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'h') -> do
      let motion = PrevByte count
      newPos <- executeMotion motion currentPos
      let newViewStart = if newPos < vsViewStart vimState
            then max 0 (newPos - fromIntegral (termHeight - 3) * 16)
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'l') -> do
      let motion = NextByte count
      newPos <- executeMotion motion currentPos
      let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 16
            then newPos - fromIntegral ((termHeight - 3) * 8)
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'w') -> do
      let motion = WordForward count
      newPos <- executeMotion motion currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'b') -> do
      let motion = WordBackward count
      newPos <- executeMotion motion currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'G') -> do
      case vsCount vimState of
        Just addr -> do
          let newPos = fromIntegral addr
          let motion = GotoAddress newPos
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
        Nothing -> do
          let newPos = rPC (mRegs machine)
          let motion = GotoPC
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'g') -> do
      nextKey <- liftIO getKey
      case nextKey of
        'g' -> do
          let newPos = rPC (mRegs machine)
          let motion = GotoPC
          return (NoAction, ["Goto PC"], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
        _ -> return (NoAction, ["Invalid g command"], vimState { vsCount = Nothing })
    
    (Nothing, 'H') -> do
      let motion = TopOfPage
      newPos <- executeMotion motion currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'M') -> do
      let motion = MiddlePage
      newPos <- executeMotion motion currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    (Nothing, 'L') -> do
      let motion = EndOfPage
      newPos <- executeMotion motion currentPos
      let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 16
            then newPos - fromIntegral ((termHeight - 3) * 8)
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) })
    
    -- Action commands
    (Nothing, 'r') -> do
      putString "Replace with hex byte: " -- Changed to putString
      liftIO $ hSetEcho stdin True
      hexStr <- liftIO getInput
      liftIO $ hSetEcho stdin False
      case parseHexByte hexStr of
        Just newByte -> do
          let action = Set newByte
          (newPos, output) <- executeAction action currentPos vimState
          return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) })
        Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" })
    
    (Nothing, '+') -> do
      let action = Increment count
      (newPos, output) <- executeAction action currentPos vimState
      return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) })
    
    (Nothing, '-') -> do
      let action = Decrement count
      (newPos, output) <- executeAction action currentPos vimState
      return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) })
    
    (Nothing, '~') -> do
      putString "Bit number (0-7): " -- Changed to putString
      liftIO $ hSetEcho stdin True
      bitStr <- liftIO getInput
      liftIO $ hSetEcho stdin False
      case parseCount bitStr of
        Just bit -> do
          let action = ToggleBit bit
          (newPos, output) <- executeAction action currentPos vimState
          return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) })
        Nothing -> return (NoAction, ["Invalid bit number"], vimState { vsCount = Nothing, vsMessage = "Invalid bit number" })
    
    (Nothing, 'B') -> do
      let action = AddBreakpoint
      (newPos, output) <- executeAction action currentPos vimState
      return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) })
    
    (Nothing, '\r') -> do
      let action = ExecuteToHere
      (newPos, output) <- executeAction action currentPos vimState
      return (ExecuteStep "execute-to-here", output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) })
    
    -- Find commands
    (Nothing, 'f') -> do
      putString "Find byte (hex): " -- Changed to putString
      liftIO $ hSetEcho stdin True
      hexStr <- liftIO getInput
      liftIO $ hSetEcho stdin False
      case parseHexByte hexStr of
        Just targetByte -> do
          newPos <- executeMotion (FindByte targetByte True) currentPos
          let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, True), vsCount = Nothing, vsMessage = "Found byte at $" ++ showHex newPos "" }
          return (NoAction, ["Found byte at $" ++ showHex newPos ""], newState)
        Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" })
    
    (Nothing, 'F') -> do
      putString "Find byte backward (hex): " -- Changed to putString
      liftIO $ hSetEcho stdin True
      hexStr <- liftIO getInput
      liftIO $ hSetEcho stdin False
      case parseHexByte hexStr of
        Just targetByte -> do
          newPos <- executeMotion (FindByte targetByte False) currentPos
          let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, False), vsCount = Nothing, vsMessage = "Found byte at $" ++ showHex newPos "" }
          return (NoAction, ["Found byte at $" ++ showHex newPos ""], newState)
        Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" })
    
    (Nothing, 't') -> do
      putString "Till byte (hex): " -- Changed to putString
      liftIO $ hSetEcho stdin True
      hexStr <- liftIO getInput
      liftIO $ hSetEcho stdin False
      case parseHexByte hexStr of
        Just targetByte -> do
          newPos <- executeMotion (TillByte targetByte True) currentPos
          let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, True), vsCount = Nothing, vsMessage = "Moved to $" ++ showHex newPos "" }
          return (NoAction, ["Moved to $" ++ showHex newPos ""], newState)
        Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" })
    
    (Nothing, 'T') -> do
      putString "Till byte backward (hex): " -- Changed to putString
      liftIO $ hSetEcho stdin True
      hexStr <- liftIO getInput
      liftIO $ hSetEcho stdin False
      case parseHexByte hexStr of
        Just targetByte -> do
          newPos <- executeMotion (TillByte targetByte False) currentPos
          let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, False), vsCount = Nothing, vsMessage = "Moved to $" ++ showHex newPos "" }
          return (NoAction, ["Moved to $" ++ showHex newPos ""], newState)
        Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" })
    
    (Nothing, ';') -> do
      case vsLastFind vimState of
        Just (byte, forward) -> do
          newPos <- executeMotion (FindByte byte forward) currentPos
          return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = "Repeated find to $" ++ showHex newPos "" })
        Nothing -> return (NoAction, ["No previous find"], vimState { vsCount = Nothing, vsMessage = "No previous find" })
    
    (Nothing, ',') -> do
      case vsLastFind vimState of
        Just (byte, forward) -> do
          newPos <- executeMotion (FindByte byte (not forward)) currentPos
          return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = "Repeated find to $" ++ showHex newPos "" })
        Nothing -> return (NoAction, ["No previous find"], vimState { vsCount = Nothing, vsMessage = "No previous find" })
    
    -- View mode switching
    (Nothing, 'v') -> do
      let newViewMode = case vsViewMode vimState of
            CodeView -> MemoryView
            MemoryView -> RegisterView  
            RegisterView -> StackView
            StackView -> CodeView
      return (NoAction, ["Switched to " ++ show newViewMode], vimState { vsViewMode = newViewMode, vsMessage = "Switched to " ++ show newViewMode })
    
    -- Step execution
    (Nothing, 's') -> return (ExecuteStep [key], [], vimState { vsCount = Nothing, vsMessage = "Stepped one instruction", vsLastChange = Just RepeatStep })
    
    -- Continue execution
    (Nothing, 'c') -> return (ExecuteStep "continue", [], vimState { vsCount = Nothing, vsMessage = "Continuing execution" })
    
    -- CommandMode commands integration
    (Nothing, 'b') -> handleBreakVim vimState
    (Nothing, 'm') -> handleMemTraceVim vimState
    (Nothing, 'a') -> handleAddressVim vimState
    
    -- Marks
    (Nothing, 'm') -> do
      putString "Mark key: " -- Changed to putString
      keyChar <- liftIO getKey
      return (NoAction, ["Set mark '" ++ [keyChar] ++ "' at $" ++ showHex currentPos ""], vimState { vsMarks = Map.insert keyChar currentPos (vsMarks vimState), vsMessage = "Set mark '" ++ [keyChar] ++ "'" })
    
    (Nothing, '\'') -> do
      putString "Goto mark: " -- Changed to putString
      keyChar <- liftIO getKey
      case Map.lookup keyChar (vsMarks vimState) of
        Just addr -> return (NoAction, ["Moved to mark '" ++ [keyChar] ++ "' at $" ++ showHex addr ""], vimState { vsCursor = addr, vsViewStart = addr, vsMessage = "Moved to mark '" ++ [keyChar] ++ "'" })
        Nothing -> return (NoAction, ["No mark '" ++ [keyChar] ++ "'"], vimState { vsMessage = "No mark '" ++ [keyChar] ++ "'" })
    
    -- Legacy commands
    (Nothing, 'q') -> return (QuitEmulator, [], vimState { vsMessage = "Quitting emulator" })
    (Nothing, 'x') -> return (ExitDebugger, [], vimState { vsMessage = "Exiting debugger" })
    
    -- Colon commands
    (Nothing, ':') -> do
      putString ":" -- Changed to putString
      liftIO $ hSetEcho stdin True
      command <- liftIO getInput
      liftIO $ hSetEcho stdin False
      handleColonCommand command vimState
    
    -- Default
    _ -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped" })

-- | Handle breakpoint commands (similar to handleBreakVim from VimMode.hs)
handleBreakVim :: VimState -> FDX (DebuggerAction, [String], VimState)
handleBreakVim vimState = do
  putString "\nBreakpoints (a: add, d: delete):" -- Changed to putString
  key <- liftIO getKey
  let currentPos = vsCursor vimState
  case key of
    'a' -> do
      let action = AddBreakpoint
      (newPos, output) <- executeAction action currentPos vimState
      return (NoAction, ("Breakpoints (a: add, d: delete): " ++ [key]) : output, vimState { vsCursor = newPos, vsMessage = head output, vsLastChange = Just (RepeatAction action) })
    'd' -> do
      let action = RemoveBreakpoint
      (newPos, output) <- executeAction action currentPos vimState
      return (NoAction, ("Breakpoints (a: add, d: delete): " ++ [key]) : output, vimState { vsCursor = newPos, vsMessage = head output, vsLastChange = Just (RepeatAction action) })
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


-- | Handle colon commands with enhanced CommandMode integration
handleColonCommand :: String -> VimState -> FDX (DebuggerAction, [String], VimState)
handleColonCommand command vimState = do
  let args = words command
  case args of
    ["q"] -> return (QuitEmulator, [], vimState { vsMessage = "Quitting emulator" })
    ["x"] -> return (ExitDebugger, [], vimState { vsMessage = "Exiting debugger" })
    ["reg"] -> do
      regs <- getRegisters
      let output = logRegisters regs
      return (NoAction, output, vimState { vsMessage = "Displayed registers" })
    ["regs"] -> do
      regs <- getRegisters
      let output = logRegisters regs
      return (NoAction, output, vimState { vsMessage = "Displayed registers" })
    ["goto", addrStr] -> do
      case parseHexWord addrStr of
        Just addr -> do
          setPC_ addr
          return (NoAction, ["PC set to $" ++ showHex addr ""], vimState { vsCursor = addr, vsViewStart = addr, vsMessage = "PC set to $" ++ showHex addr "" })
        Nothing -> return (NoAction, ["Invalid address"], vimState { vsMessage = "Invalid address" })
    ["set", "pc", addrStr] -> do
      (action, output) <- handleSetPC addrStr ""
      machineAfterSetPC <- get -- Get the machine state after setPC_
      return (action, output, vimState { vsCursor = rPC (mRegs machineAfterSetPC), vsViewStart = rPC (mRegs machineAfterSetPC), vsMessage = head output })
    ["ra", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rAC = val }) valStr "Accumulator" ""
      return (action, output, vimState { vsMessage = head output })
    ["rx", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rX = val }) valStr "X Register" ""
      return (action, output, vimState { vsMessage = head output })
    ["ry", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rY = val }) valStr "Y Register" ""
      return (action, output, vimState { vsMessage = head output })
    ["rsp", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rSP = val }) valStr "Stack Pointer" ""
      return (action, output, vimState { vsMessage = head output })
    ["rsr", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rSR = val }) valStr "Status Register" ""
      return (action, output, vimState { vsMessage = head output })
    ["fill", startAddrStr, endAddrStr] -> do
      (action, output) <- handleFill [startAddrStr,  endAddrStr] ""
      return (action, output, vimState { vsMessage = head output })
    ["fill", startAddrStr, endAddrStr, byteStr] -> do
      (action, output) <- handleFill (startAddrStr : endAddrStr : [byteStr]) ""
      return (action, output, vimState { vsMessage = head output })
    ["fill", startAddrStr, endAddrStr, byteStr1, byteStr2] -> do
      (action, output) <- handleFill (startAddrStr : endAddrStr : [byteStr1, byteStr2]) ""
      return (action, output, vimState { vsMessage = head output })
    ["break"] -> do
      (action, output) <- handleBreak [] ""
      return (action, output, vimState { vsMessage = last output })
    ["bk"] -> do
      (action, output) <- handleBreak [] ""
      return (action, output, vimState { vsMessage = last output })
    ["break", addrStr] -> do
      (action, output) <- handleBreak [addrStr] ""
      return (action, output, vimState { vsMessage = last output })
    ["bk", addrStr] -> do
      (action, output) <- handleBreak [addrStr] ""
      return (action, output, vimState { vsMessage = last output })
    ["mem"] -> do
      (action, output) <- handleMemTrace [] ""
      return (action, output, vimState { vsMessage = last output })
    ["m"] -> do
      (action, output) <- handleMemTrace [] ""
      return (action, output, vimState { vsMessage = last output })
    ["mem", startAddrStr, endAddrStr] -> do
      (action, output) <- handleMemTrace [startAddrStr, endAddrStr] ""
      return (action, output, vimState { vsMessage = last output })
    ["mem", startAddrStr, endAddrStr, name] -> do
      (action, output) <- handleMemTrace [startAddrStr, endAddrStr, name] ""
      return (action, output, vimState { vsMessage = last output })
    -- ["trace"] -> do
    --   let newTraceState = not (enableTrace machine)
    --   put (machine { enableTrace = newTraceState })
    --   let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
    --   return (NoAction, output, vimState { vsMessage = head output })
    -- ["t"] -> do
    --   let newTraceState = not (enableTrace machine)
    --   put (machine { enableTrace = newTraceState })
    --   let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
    --   return (NoAction, output, vimState { vsMessage = head output })
    ["d"] -> do
      (action, output) <- handleDisassemble [] ""
      return (action, output, vimState { vsMessage = last output })
    ["d", addrStr] -> do
      (action, output) <- handleDisassemble [addrStr] ""
      return (action, output, vimState { vsMessage = last output })
    _ -> return (NoAction, ["Unknown command: " ++ command], vimState { vsCount = Nothing, vsMessage = "Unknown command: " ++ command })

interactiveLoopHelper :: FDX (DebuggerAction, VimState) -- Changed signature
-- | Enhanced interactive loop with vim state management
-- interactiveLoopHelper :: DebuggerConsoleState -> FDX DebuggerAction
interactiveLoopHelper = do -- Removed consoleState argument
  machine <- get
  if halted machine
    then return (QuitEmulator, vimState machine) -- Return QuitEmulator action if halted
    else do
      -- Use the vimState from the machine for the current iteration.
      -- This ensures that the state persists across loop iterations.
      let currentVimState = vimState machine 
      renderVimScreen machine currentVimState -- Removed consoleState argument
      key <- liftIO getKey
      (action, output, newVimState) <- handleVimKey key currentVimState
      
      -- Update console output lines in the machine's console state
      modify (\m -> m { mConsoleState = (mConsoleState m) { outputLines = outputLines (mConsoleState m) ++ output } })
      
      -- Always update the machine's vimState with the newVimState before the next iteration
      put (machine { vimState = newVimState }) 

      case action of
        ContinueLoop _ -> interactiveLoopHelper
        ExecuteStep _ -> do
          machineAfterExecution <- get -- Get the machine state *after* the step has been executed by the main loop
          return (action, newVimState { vsCursor = rPC (mRegs machineAfterExecution), vsViewStart = rPC (mRegs machineAfterExecution) })
        ExitDebugger -> return (action, newVimState)  
        QuitEmulator -> return (action, newVimState)
        -- NoAction -> do
        --       machine' <- get
        --       liftIO $ putStrLn $ "newVimState.vsCursor przed put: " ++ show (vsCursor newVimState)
        --       put (machine' { vimState = newVimState })
        --       machine'' <- get
        --       liftIO $ putStrLn $ "vsCursor po put: " ++ show (vsCursor $ vimState machine'')
        --       interactiveLoopHelper updatedConsoleState
        NoAction -> do
                -- Debug prints (optional, can be removed after verification)
                liftIO $ putStrLn $ "newVimState.vsCursor przed put: " ++ show (vsCursor newVimState)
                liftIO $ putStrLn $ "vsCursor po put: " ++ show (vsCursor newVimState)
                -- Renderowanie po zaktualizowaniu stanu
                renderVimScreen machine newVimState -- Removed consoleState argument
                interactiveLoopHelper


        SwitchToCommandMode -> return (action, newVimState)
        SwitchToVimMode -> interactiveLoopHelper


-- | Render screen with vim-specific cursor and status
renderVimScreen :: Machine -> VimState -> FDX () -- Changed signature
renderVimScreen machine vimState = do -- Removed consoleState argument
  liftIO $ putStrLn $ "Renderowanie z vsCursor: " ++ show (vsCursor vimState)
  liftIO ANSI.hideCursor
  liftIO ANSI.clearScreen
  liftIO $ ANSI.setCursorPosition 0 0

  let linesPerPage = termHeight - 3 -- Reserve 2 lines for status, 1 for message
  let cursorPos = vsCursor vimState
  let viewStart = vsViewStart vimState

  -- Adjust viewStart to keep cursor in view
  let adjustedViewStart = case vsViewMode vimState of
        CodeView -> if cursorPos < viewStart
                    then max 0 (cursorPos - fromIntegral (linesPerPage `div` 2))
                    else if cursorPos >= viewStart + fromIntegral (linesPerPage * 3)
                         then cursorPos - fromIntegral (linesPerPage * 2)
                         else viewStart
        _ -> if cursorPos < viewStart
             then max 0 (cursorPos - fromIntegral (linesPerPage * 16 `div` 2))
             else if cursorPos >= viewStart + fromIntegral (linesPerPage * 16)
                  then cursorPos - fromIntegral (linesPerPage * 8)
                  else viewStart

  -- Render content based on view mode
  case vsViewMode vimState of
    CodeView -> do
      ((disassembledLines, _), _) <- liftIO $ runStateT (unFDX $ disassembleInstructions adjustedViewStart linesPerPage) machine
      let linesWithCursor = zipWith (\line addr -> 
                      if addr == cursorPos
                      then "\x1b[7m" ++ line ++ "\x1b[0m" -- Highlight cursor line
                      else line
                    ) disassembledLines [adjustedViewStart ..]
      -- Print lines with cursor highlighting
      liftIO $ mapM_ (\(line, row) -> do
        ANSI.setCursorPosition row 0
        putStr line
        hFlush stdout) (zip linesWithCursor [0..])

    MemoryView -> do
      let startAddr = adjustedViewStart
      let endAddr = min 0xFFFF (startAddr + fromIntegral (linesPerPage * 16 - 1))
      bytes <- liftIO $ runStateT (unFDX $ mapM fetchByteMem [startAddr .. endAddr]) machine
      let byteLines = chunkBytes (fst bytes) 16
      let linesWithAddr = zipWith (\addr line -> formatHex16 addr ++ ": " ++ unwords (map formatHex8 line)) [startAddr, startAddr + 16 ..] byteLines
      let linesWithCursor = zipWith (\line addr -> if cursorPos >= addr && cursorPos < addr + 16
            then let offset = fromIntegral (cursorPos - addr) * 3
                 in take offset line ++ "\x1b[7m" ++ take 2 (drop offset line) ++ "\x1b[0m" ++ drop (offset + 2) line
            else line) linesWithAddr [startAddr, startAddr + 16 ..]
      liftIO $ mapM_ (\(line, row) -> do
        ANSI.setCursorPosition row 0
        putStr line
        hFlush stdout) (zip linesWithCursor [0..])

    RegisterView -> do
      regs <- liftIO $ runStateT (unFDX getRegisters) machine
      let output = logRegisters (fst regs)
      liftIO $ mapM_ (\(line, row) -> do
        ANSI.setCursorPosition row 0
        putStr line
        hFlush stdout) (zip output [0..])

    StackView -> do
      let sp = rSP (mRegs machine)
      let stackStart = fromIntegral sp + 0x0100
      let stackEnd = min 0x01FF (stackStart + fromIntegral (linesPerPage * 16 - 1))
      bytes <- liftIO $ runStateT (unFDX $ mapM fetchByteMem [stackStart .. stackEnd]) machine
      let byteLines = chunkBytes (fst bytes) 16
      let linesWithAddr = zipWith (\addr line -> formatHex16 addr ++ ": " ++ unwords (map formatHex8 line)) [stackStart, stackStart + 16 ..] byteLines
      let linesWithCursor = zipWith (\line addr -> if cursorPos >= addr && cursorPos < addr + 16
            then let offset = fromIntegral (cursorPos - addr) * 3
                 in take offset line ++ "\x1b[7m" ++ take 2 (drop offset line) ++ "\x1b[0m" ++ drop (offset + 2) line
            else line) linesWithAddr [stackStart, stackStart + 16 ..]
      liftIO $ mapM_ (\(line, row) -> do
        ANSI.setCursorPosition row 0
        putStr line
        hFlush stdout) (zip linesWithCursor [0..])

  -- Status line
  liftIO $ ANSI.setCursorPosition (termHeight - 2) 0
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  let modeDisplay = case vsViewMode vimState of
        CodeView -> " CODE "
        MemoryView -> " MEMORY "
        RegisterView -> " REGISTERS "
        StackView -> " STACK "
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
        Just op -> " " ++ op
        Nothing -> ""
  liftIO $ putStr operatorDisplay
  let spacerLength = max 0 (termWidth - length modeDisplay - length regDisplay - length cursorDisplay - length countDisplay - length operatorDisplay)
  liftIO $ putStr $ replicate spacerLength ' '
  liftIO $ ANSI.setSGR [ANSI.Reset]
  liftIO $ hFlush stdout

  -- Message line
  liftIO $ ANSI.setCursorPosition (termHeight - 1) 0
  liftIO $ putStr (vsMessage vimState)
  liftIO $ hFlush stdout
  liftIO ANSI.showCursor

-- | Helper to format Word8 as two-character hex
formatHex8 :: Word8 -> String
formatHex8 b = let hexStr = showHex b "" in if length hexStr < 2 then "0" ++ hexStr else hexStr

-- | Helper to format Word16 as four-character hex
formatHex16 :: Word16 -> String
formatHex16 w = let hexStr = showHex w "" in replicate (4 - length hexStr) '0' ++ hexStr

-- | Chunk a list into sublists of given size
chunkBytes :: [Word8] -> Int -> [[Word8]]
chunkBytes [] _ = []
chunkBytes xs n = take n xs : chunkBytes (drop n xs) n
