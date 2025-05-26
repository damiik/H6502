module MOS6502Emulator.Debugger.VimModeHandleKey
  ( handleVimKey
  ) where
  
import Control.Monad.State (StateT, get, put, modify, liftIO)
import System.IO (hFlush, stdout, stdin, hSetEcho)
import Data.Word (Word16)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Numeric (showHex)

import MOS6502Emulator.Core(Machine(..), DebuggerAction(..), FDX, mRegs, getRegisters, parseHexByte, parseHexWord)
import MOS6502Emulator.Machine (setPC_)
-- import MOS6502Emulator.Registers (Registers(..), rPC)
-- import MOS6502Emulator.Debugger.Console (getKey, getInput, termHeight, termWidth) 
import MOS6502Emulator.Registers(Registers(..))
import MOS6502Emulator.DissAssembler(disassembleInstructions, disassembleInstruction, InstructionInfo(..), opcodeMap)
import MOS6502Emulator.Debugger(handleMemTrace, handleBreak, handleDisassemble, handleSetPC, handleSetReg8, handleFill, logRegisters )
import MOS6502Emulator.Debugger.VimModeCore ( VimState(..), Motion(..), Action(..), ViewMode(..), parseCount)-- | Enhanced vim key handler with composition support
import MOS6502Emulator.Debugger.VimModeExecute (executeMotion, executeAction)
import MOS6502Emulator.Debugger.Console(getKey, getInput, termHeight)

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
      (newPos', output) <- executeAction action currentPos
      let newYankBuffer = if op == "y"
            then Map.insert (vsRegister vimState) (maybe [] id (Map.lookup '"' (vsYankBuffer vimState))) (vsYankBuffer vimState)
            else vsYankBuffer vimState
      return (NoAction, output, vimState { vsCursor = newPos', vsCount = Nothing, vsOperator = Nothing, vsYankBuffer = newYankBuffer })
    
    (Just op, 'k') | op `elem` ["d", "c", "y"] -> do
      newPos <- executeMotion (PrevInstruction count) currentPos
      let action = case op of
            "d" -> Delete (PrevInstruction count)
            "c" -> Change (PrevInstruction count)
            "y" -> Yank (PrevInstruction count)
            _ -> error "Invalid operator"
      (newPos', output) <- executeAction action currentPos
      let newYankBuffer = if op == "y"
            then Map.insert (vsRegister vimState) (maybe [] id (Map.lookup '"' (vsYankBuffer vimState))) (vsYankBuffer vimState)
            else vsYankBuffer vimState
      return (NoAction, output, vimState { vsCursor = newPos', vsCount = Nothing, vsOperator = Nothing, vsYankBuffer = newYankBuffer })
    
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
    
    -- Movement commands
    (Nothing, 'j') -> do
      newPos <- executeMotion (NextInstruction count) currentPos
      let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 3
            then newPos - fromIntegral ((termHeight - 3) * 2)
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing })
    
    (Nothing, 'k') -> do
      newPos <- executeMotion (PrevInstruction count) currentPos
      let newViewStart = if newPos < vsViewStart vimState
            then max 0 (newPos - fromIntegral (termHeight - 3))
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing })
    
    (Nothing, 'h') -> do
      newPos <- executeMotion (PrevByte count) currentPos
      let newViewStart = if newPos < vsViewStart vimState
            then max 0 (newPos - fromIntegral (termHeight - 3) * 16)
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing })
    
    (Nothing, 'l') -> do
      newPos <- executeMotion (NextByte count) currentPos
      let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 16
            then newPos - fromIntegral ((termHeight - 3) * 8)
            else vsViewStart vimState
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing })
    
    (Nothing, 'w') -> do
      newPos <- executeMotion (WordForward count) currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing })
    
    (Nothing, 'b') -> do
      newPos <- executeMotion (WordBackward count) currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing })
    
    (Nothing, 'G') -> do
      case vsCount vimState of
        Just addr -> do
          let newPos = fromIntegral addr
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing })
        Nothing -> do
          let newPos = rPC (mRegs machine)
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing })
    
    (Nothing, 'g') -> do
      nextKey <- liftIO getKey
      case nextKey of
        'g' -> do
          let newPos = rPC (mRegs machine)
          return (NoAction, ["Goto PC"], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing })
        _ -> return (NoAction, ["Invalid g command"], vimState { vsCount = Nothing })
    
    (Nothing, 'H') -> do
      newPos <- executeMotion TopOfPage currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing })
    
    (Nothing, 'M') -> do
      newPos <- executeMotion MiddlePage currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing })
    
    (Nothing, 'L') -> do
      newPos <- executeMotion EndOfPage currentPos
      return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing })
    
    -- Action commands
    (Nothing, 'r') -> do
      liftIO $ putStr "Replace with hex byte: " >> hFlush stdout
      liftIO $ hSetEcho stdin True
      hexStr <- liftIO getInput
      liftIO $ hSetEcho stdin False
      case parseHexByte hexStr of
        Just newByte -> do
          (newPos, output) <- executeAction (Set newByte) currentPos
          return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output })
        Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" })
    
    (Nothing, '+') -> do
      (newPos, output) <- executeAction (Increment count) currentPos
      return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output })
    
    (Nothing, '-') -> do
      (newPos, output) <- executeAction (Decrement count) currentPos
      return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output })
    
    (Nothing, '~') -> do
      liftIO $ putStr "Bit number (0-7): " >> hFlush stdout
      liftIO $ hSetEcho stdin True
      bitStr <- liftIO getInput
      liftIO $ hSetEcho stdin False
      case parseCount bitStr of
        Just bit -> do
          (newPos, output) <- executeAction (ToggleBit bit) currentPos
          return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output })
        Nothing -> return (NoAction, ["Invalid bit number"], vimState { vsCount = Nothing, vsMessage = "Invalid bit number" })
    
    (Nothing, 'B') -> do
      (newPos, output) <- executeAction AddBreakpoint currentPos
      return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output })
    
    (Nothing, '\r') -> do
      (newPos, output) <- executeAction ExecuteToHere currentPos
      return (ExecuteStep "execute-to-here", output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output })
    
    -- Find commands
    (Nothing, 'f') -> do
      liftIO $ putStr "Find byte (hex): " >> hFlush stdout
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
      liftIO $ putStr "Find byte backward (hex): " >> hFlush stdout
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
      liftIO $ putStr "Till byte (hex): " >> hFlush stdout
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
      liftIO $ putStr "Till byte backward (hex): " >> hFlush stdout
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
    (Nothing, 's') -> return (ExecuteStep [key], [], vimState { vsCount = Nothing, vsMessage = "Stepped one instruction" })
    
    -- Continue execution
    (Nothing, 'c') -> return (ExecuteStep "continue", [], vimState { vsCount = Nothing, vsMessage = "Continuing execution" })
    
    -- CommandMode commands integration
    (Nothing, 'b') -> handleBreakVim vimState
    (Nothing, 'm') -> handleMemTraceVim vimState
    (Nothing, 'a') -> handleAddressVim vimState
    
    -- Marks
    (Nothing, 'm') -> do
      liftIO $ putStr "Mark key: " >> hFlush stdout
      keyChar <- liftIO getKey
      return (NoAction, ["Set mark '" ++ [keyChar] ++ "' at $" ++ showHex currentPos ""], vimState { vsMarks = Map.insert keyChar currentPos (vsMarks vimState), vsMessage = "Set mark '" ++ [keyChar] ++ "'" })
    
    (Nothing, '\'') -> do
      liftIO $ putStr "Goto mark: " >> hFlush stdout
      keyChar <- liftIO getKey
      case Map.lookup keyChar (vsMarks vimState) of
        Just addr -> return (NoAction, ["Moved to mark '" ++ [keyChar] ++ "' at $" ++ showHex addr ""], vimState { vsCursor = addr, vsViewStart = addr, vsMessage = "Moved to mark '" ++ [keyChar] ++ "'" })
        Nothing -> return (NoAction, ["No mark '" ++ [keyChar] ++ "'"], vimState { vsMessage = "No mark '" ++ [keyChar] ++ "'" })
    
    -- Legacy commands
    (Nothing, 'q') -> return (QuitEmulator, [], vimState { vsMessage = "Quitting emulator" })
    (Nothing, 'x') -> return (ExitDebugger, [], vimState { vsMessage = "Exiting debugger" })
    
    -- Colon commands
    (Nothing, ':') -> do
      liftIO $ putStr ":" >> hFlush stdout
      liftIO $ hSetEcho stdin True
      command <- liftIO getInput
      liftIO $ hSetEcho stdin False
      handleColonCommand command vimState
    
    -- Default
    _ -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped" })

-- | Handle breakpoint commands (similar to handleBreakVim from VimMode.hs)
handleBreakVim :: VimState -> FDX (DebuggerAction, [String], VimState)
handleBreakVim vimState = do
  liftIO $ putStr "\nBreakpoints (a: add, d: delete):" >> hFlush stdout
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStr "Add Breakpoint (address):" >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleBreak args ""
      return (action, ["Breakpoints (a: add, d: delete): " ++ [key], "Add Breakpoint (address): " ++ input] ++ output, vimState { vsMessage = last output })
    'd' -> do
      liftIO $ putStr "Delete Breakpoint (address):" >> hFlush stdout
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
  liftIO $ putStr "\nMemory Trace (a: add, d: delete):" >> hFlush stdout
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStr "Add Memory Trace Block (start end [name]): " >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleMemTrace args ""
      return (action, ["Memory Trace (a: add, d: delete): " ++ [key], "Add Memory Trace Block (start end [name]): " ++ input] ++ output, vimState { vsMessage = last output })
    'd' -> do
      liftIO $ putStr "Delete Memory Trace Block (start end [name]): " >> hFlush stdout
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
  liftIO $ putStr "\nStored Addresses (s: store, g: goto):" >> hFlush stdout
  key <- liftIO getKey
  case key of
    's' -> do
      liftIO $ putStr "Store Current PC (key):" >> hFlush stdout
      keyChar <- liftIO getKey
      machine <- get
      let currentPC = rPC (mRegs machine)
      put (machine { storedAddresses = Map.insert keyChar currentPC (storedAddresses machine) })
      return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "Stored PC $" ++ showHex currentPC "" ++ " at key '" ++ [keyChar] ++ "'"], vimState { vsMessage = "Stored PC at '" ++ [keyChar] ++ "'" })
    'g' -> do
      liftIO $ putStr "Goto Stored Address (key):" >> hFlush stdout
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
      return (action, output, vimState { vsCursor = vsCursor vimState, vsMessage = head output })
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
      (action, output) <- handleFill [startAddrStr, endAddrStr ] ""
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
    _ -> return (NoAction, ["Unknown command: " ++ command], vimState { vsMessage = "Unknown command: " ++ command })
