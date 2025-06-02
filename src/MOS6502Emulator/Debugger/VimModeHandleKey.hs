module MOS6502Emulator.Debugger.VimModeHandleKey
  ( handleVimNormalModeKey -- Renamed and exported
  ) where
  
import Control.Monad.State (StateT, get, put, modify, liftIO, runStateT)
import System.IO (hFlush, stdout, stdin, hSetEcho)
import Data.Word (Word8, Word16)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Numeric (showHex)

import MOS6502Emulator.Core(Machine(..), FDX, fetchByteMem, writeByteMem, getRegisters, parseHexWord, unFDX, mRegs, mConsoleState, parseHexByte, AddressMode (Y)) -- Added writeByte
import MOS6502Emulator.Machine (setPC_)
import MOS6502Emulator.Memory(writeByte)
import MOS6502Emulator.Debugger.Types (DebuggerAction(..), DebuggerConsoleState(..)) -- Ensure DebuggerAction is imported from here
import MOS6502Emulator.Registers(Registers(..))
import MOS6502Emulator.DissAssembler(disassembleInstructions, disassembleInstruction, InstructionInfo(..), opcodeMap)
import MOS6502Emulator.Debugger(handleMemTrace, handleBreak, handleDisassemble, handleSetPC, handleSetReg8, handleFill, logRegisters )
import MOS6502Emulator.Debugger.VimModeCore ( VimState(..), Motion(..), Action(..), ViewMode(..), RepeatableCommand(..), OperatorType(..), VisualType(..))
import MOS6502Emulator.Debugger.VimModeExecute (executeMotion, executeAction)
import MOS6502Emulator.Debugger.Console(getKey, getInput, termHeight, termWidth, putOutput, putString)
import qualified System.Console.ANSI as ANSI

-- | Parse count prefix (like 5dd, 10j)
parseCount :: String -> Maybe Int
parseCount s = if all isDigit s && not (null s) 
               then Just (read s) 
               else Nothing

-- TODO: Add real actions
-- | Handle keys in visual mode
handleVisualKey :: Char -> VimState -> FDX (DebuggerAction, [String], VimState)
handleVisualKey key vimState = do
    let start = vsVisualStart vimState
    let end = vsCursor vimState
    let (minAddr, maxAddr) = if start <= end then (start, end) else (end, start)
    
    case key of
        -- Switch visual types
        'v' -> return (NoAction, [], vimState { vsVisualType = CharVisual })
        'V' -> return (NoAction, [], vimState { vsVisualType = LineVisual })
        '\x1b' -> return (NoAction, [], vimState { vsInVisualMode = False })  -- Exit visual mode
        
        -- Operations
        'y' -> do
            let bytes = case vsVisualType vimState of
                        LineVisual -> [minAddr..maxAddr]
                        _ -> [minAddr..maxAddr]
            content <- mapM fetchByteMem bytes
            let newYank = Map.insert (vsRegister vimState) content (vsYankBuffer vimState)
            return (NoAction, ["Yanked " ++ show (length content) ++ " bytes"],
                    vimState { vsYankBuffer = newYank, vsInVisualMode = False })
                    
        'd' -> do
            let bytes = case vsVisualType vimState of
                        LineVisual -> [minAddr..maxAddr]
                        _ -> [minAddr..maxAddr]
            mapM_ (`writeByteMem` 0) bytes  -- Delete by writing 0s
            return (NoAction, ["Deleted " ++ show (length bytes) ++ " bytes"],
                    vimState { vsInVisualMode = False })
                    
        'c' -> do
            putString "Change with hex byte: "
            liftIO $ hSetEcho stdin True
            hexStr <- liftIO getInput
            liftIO $ hSetEcho stdin False
            case parseHexByte hexStr of
                Just byte -> do
                    let bytes = case vsVisualType vimState of
                                LineVisual -> [minAddr..maxAddr]
                                _ -> [minAddr..maxAddr]
                    mapM_ (`writeByteMem` byte) bytes
                    return (NoAction, ["Changed " ++ show (length bytes) ++ " bytes to " ++ hexStr],
                            vimState { vsInVisualMode = False })
                Nothing -> return (NoAction, ["Invalid hex byte"], vimState)
        
        -- Move cursor in visual mode
        'j' -> do
            newPos <- executeMotion (NextInstruction 1) (vsCursor vimState)
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos })
        'k' -> do
            newPos <- executeMotion (PrevInstruction 1) (vsCursor vimState)
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos })
        'h' -> do
            newPos <- executeMotion (PrevByte 1) (vsCursor vimState)
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos })
        'l' -> do
            newPos <- executeMotion (NextByte 1) (vsCursor vimState)
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos })
        
        -- Show disassembly of selection
        'D' -> do
            let disassembleAddr addr = do
                    (instr, _) <- disassembleInstruction addr
                    return $ "  " ++ showHex addr ": " ++ instr
            output <- mapM disassembleAddr [minAddr..maxAddr]
            return (NoAction, "Disassembled selection:":output, vimState)
        
        _   -> return (NoAction, ["Key '" ++ [key] ++ "' not supported in visual mode"], vimState)

handleVimNormalModeKey :: Char -> VimState -> FDX (DebuggerAction, [String], VimState)
handleVimNormalModeKey key vimState = do
  machine <- get
  let currentPos = vsCursor vimState
  let count = fromMaybe 1 (vsCount vimState)
  
  -- Handle visual mode first
  if vsInVisualMode vimState
    then handleVisualKey key vimState
    else
      case (vsOperator vimState, key) of
    -- Operator-pending mode
        (Just op, motionKey) | motionKey `elem` ['j','k','h','l','w','b','G','g','H','M','L'] -> do
            motion <- case motionKey of
              'j' -> return $ NextInstruction count
              'k' -> return $ PrevInstruction count
              'h' -> return $ PrevByte count
              'l' -> return $ NextByte count
              'w' -> return $ WordForward count
              'b' -> return $ WordBackward count
              'G' -> return $ GotoAddressMotion $ fromIntegral $ fromMaybe (fromIntegral $ rPC (mRegs machine)) (vsCount vimState)
              'g' -> return GotoPC
              'H' -> return TopOfPage
              'M' -> return MiddlePage
              'L' -> return EndOfPage
              _   -> error "Unhandled motion"
            
            let action = case op of
                            DeleteOp -> Delete motion
                            ChangeOp -> Change motion
                            YankOp   -> Yank motion
            (newPos', output) <- executeAction action currentPos vimState
            let newYankBuffer = if op == YankOp
                then Map.insert (vsRegister vimState) (maybe [] id (Map.lookup '"' (vsYankBuffer vimState))) (vsYankBuffer vimState)
                else vsYankBuffer vimState
            return (NoAction, output, vimState { vsCursor = newPos', vsCount = Nothing, vsOperator = Nothing, vsYankBuffer = newYankBuffer, vsLastChange = Just (RepeatAction action) })
    
        (Just op, 'k') | op `elem` [DeleteOp, ChangeOp, YankOp] -> do
              newPos <- executeMotion (PrevInstruction count) currentPos
              let action = case op of
                    DeleteOp -> Delete (PrevInstruction count)
                    ChangeOp -> Change (PrevInstruction count)
                    YankOp -> Yank (PrevInstruction count)
                    _ -> error "Invalid operator"
              (newPos', output) <- executeAction action currentPos vimState
              let newYankBuffer = if op == YankOp
                    then Map.insert (vsRegister vimState) (maybe [] id (Map.lookup '"' (vsYankBuffer vimState))) (vsYankBuffer vimState)
                    else vsYankBuffer vimState
              return (NoAction, output, vimState { vsCursor = newPos', vsCount = Nothing, vsOperator = Nothing, vsYankBuffer = newYankBuffer, vsLastChange = Just (RepeatAction action) })
    
        -- Escape - clear pending operations or switch to CommandMode
        (Nothing, '\x1b') -> return (SwitchToCommandMode, ["Switching to Command Mode"], vimState { vsOperator = Nothing, vsCount = Nothing })
        (Just _, '\x1b') -> return (NoAction, [""], vimState { vsOperator = Nothing, vsCount = Nothing })
        
        -- Handle newline as a no-op
        (Nothing, '\n') -> return (NoAction, [""], vimState { vsCount = Nothing, vsMessage = "" })

        -- Numbers - build count prefix
        (Nothing, c) | isDigit c -> do
          let currentCount = fromMaybe 0 (vsCount vimState)
          let newCount = currentCount * 10 + (read [c])
          return (NoAction, [""], vimState { vsCount = Just newCount })
    
    -- Operator commands
        (Nothing, 'v') ->
          -- Enter visual mode
          return (NoAction, ["Entered visual mode"], vimState {
            vsInVisualMode = True,
            vsVisualStart = currentPos
          })
          
        (Nothing, c) | c `elem` ['d','c','y','w','l','b'] -> do
          case c of
            'd' -> return (NoAction, [""], vimState { vsOperator = Just DeleteOp })
            'c' -> return (NoAction, [""], vimState { vsOperator = Just ChangeOp })
            'y' -> return (NoAction, [""], vimState { vsOperator = Just YankOp })
            'w' -> do  -- Yank/delete word object
                let motion = WordObject (fromMaybe 1 (vsCount vimState))
                let action = case vsOperator vimState of
                              Just DeleteOp -> Delete motion
                              Just ChangeOp -> Change motion
                              Just YankOp   -> Yank motion
                              _ -> Move motion
                (newPos, output) <- executeAction action currentPos vimState
                return (NoAction, output, vimState { vsCursor = newPos, vsOperator = Nothing })
            'l' -> do  -- Yank/delete line object
                let motion = LineObject (fromMaybe 1 (vsCount vimState))
                let action = case vsOperator vimState of
                              Just DeleteOp -> Delete motion
                              Just ChangeOp -> Change motion
                              Just YankOp   -> Yank motion
                              _ -> Move motion
                (newPos, output) <- executeAction action currentPos vimState
                return (NoAction, output, vimState { vsCursor = newPos, vsOperator = Nothing })
            'b' -> do  -- Yank/delete bracket object
                let motion = BracketObject
                let action = case vsOperator vimState of
                              Just DeleteOp -> Delete motion
                              Just ChangeOp -> Change motion
                              Just YankOp   -> Yank motion
                              _ -> Move motion
                (newPos, output) <- executeAction action currentPos vimState
                return (NoAction, output, vimState { vsCursor = newPos, vsOperator = Nothing })
            _ -> return (NoAction, [""], vimState)

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
            return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion mot) })
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
              let motion = GotoAddressMotion newPos
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
        
        -- Colon commands (handled by VimModeEnhanced now)
        (Nothing, ':') -> do
          -- Removed: liftIO $ hSetEcho stdin True -- Echo is now handled by manual rendering
          return (SwitchToVimCommandMode, [], vimState { vsInCommandMode = True, vsCommandBuffer = ":", vsMessage = "" })
    
    -- Default
        _ -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped" })
