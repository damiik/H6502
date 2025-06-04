module MOS6502Emulator.Debugger.VimMode.HandleKey
  ( handleVimNormalModeKey -- Renamed and exported
  ) where
  
import Control.Monad.State (StateT, get, put, modify, liftIO, runStateT)
import System.IO (hFlush, stdout, stdin, hSetEcho)
import Data.Word (Word8, Word16)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Numeric (showHex)

import MOS6502Emulator.Core(Machine(..), FDX, fetchByteMem, writeByteMem, unFDX, mRegs, mConsoleState, AddressMode (Y)) -- Removed parseHexWord, parseHexByte, getRegisters
import MOS6502Emulator.Machine (setPC_) -- setPC_ is now in Debugger.Utils, but it's also exported from Machine.hs, so this import is fine.
import MOS6502Emulator.Memory(writeByte)
import MOS6502Emulator.Debugger.Core (DebuggerAction(..), DebuggerConsoleState(..), DebuggerMode(..), parseCount) -- Ensure DebuggerAction and DebuggerMode are imported from here
import MOS6502Emulator.Registers(Registers(..))
import MOS6502Emulator.DissAssembler(disassembleInstructions, disassembleInstruction, InstructionInfo(..), opcodeMap)
import MOS6502Emulator.Debugger.Commands(handleMemTrace, handleBreak, handleDisassemble, handleSetPC, handleSetReg8, handleFill) -- Moved handle* imports
import MOS6502Emulator.Debugger(logRegisters) -- logRegisters remains in Debugger.hs
import MOS6502Emulator.Debugger.VimMode.Core ( VimState(..), Motion(..), Action(..), ViewMode(..), RepeatableCommand(..), OperatorType(..), VisualType(..))
import MOS6502Emulator.Debugger.VimMode.Execute (executeMotion, executeAction)
import MOS6502Emulator.Debugger.Console(getKey, getInput, termHeight, termWidth, putOutput, putString)
import qualified System.Console.ANSI as ANSI
import MOS6502Emulator.Debugger.Utils (parseHexWord, parseHexByte, getRegisters) -- Import from Debugger.Utils


-- TODO: Add real actions
-- | Handle keys in visual mode
handleVisualKey :: Char -> VimState -> FDX (DebuggerAction, [String], VimState, DebuggerConsoleState, DebuggerMode)
handleVisualKey key vimState = do
    machine <- get -- Get machine state to access console and mode
    let currentConsoleState = mConsoleState machine
    let currentDebuggerMode = debuggerMode machine -- Use accessor here
    let start = vsVisualStart vimState
    let end = vsCursor vimState
    let (minAddr, maxAddr) = if start <= end then (start, end) else (end, start)
    
    case key of
        -- Switch visual types
        'v' -> return (NoAction, [], vimState { vsVisualType = CharVisual }, currentConsoleState, currentDebuggerMode)
        'V' -> return (NoAction, [], vimState { vsVisualType = LineVisual }, currentConsoleState, currentDebuggerMode)
        '\x1b' -> return (NoAction, [], vimState { vsInVisualMode = False }, currentConsoleState, currentDebuggerMode)  -- Exit visual mode
        
        -- Operations
        'y' -> do
            let bytes = case vsVisualType vimState of
                        LineVisual -> [minAddr..maxAddr]
                        _ -> [minAddr..maxAddr]
            content <- mapM fetchByteMem bytes
            let newYank = Map.insert (vsRegister vimState) content (vsYankBuffer vimState)
            return (NoAction, ["Yanked " ++ show (length content) ++ " bytes"],
                    vimState { vsYankBuffer = newYank, vsInVisualMode = False }, currentConsoleState, currentDebuggerMode)
                    
        'd' -> do
            let bytes = case vsVisualType vimState of
                        LineVisual -> [minAddr..maxAddr]
                        _ -> [minAddr..maxAddr]
            mapM_ (`writeByteMem` 0) bytes  -- Delete by writing 0s
            return (NoAction, ["Deleted " ++ show (length bytes) ++ " bytes"],
                    vimState { vsInVisualMode = False }, currentConsoleState, currentDebuggerMode)
                    
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
                            vimState { vsInVisualMode = False }, currentConsoleState, currentDebuggerMode)
                Nothing -> return (NoAction, ["Invalid hex byte"], vimState, currentConsoleState, currentDebuggerMode)
        
        -- Move cursor in visual mode
        'j' -> do
            newPos <- executeMotion (NextInstruction 1) (vsCursor vimState)
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos }, currentConsoleState, currentDebuggerMode)
        'k' -> do
            newPos <- executeMotion (PrevInstruction 1) (vsCursor vimState)
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos }, currentConsoleState, currentDebuggerMode)
        'h' -> do
            newPos <- executeMotion (PrevByte 1) (vsCursor vimState)
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos }, currentConsoleState, currentDebuggerMode)
        'l' -> do
            newPos <- executeMotion (NextByte 1) (vsCursor vimState)
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos }, currentConsoleState, currentDebuggerMode)
        
        -- Show disassembly of selection
        'D' -> do
            let disassembleAddr addr = do
                    (instr, _) <- disassembleInstruction addr
                    return $ "  " ++ showHex addr ": " ++ instr
            output <- mapM disassembleAddr [minAddr..maxAddr]
            return (NoAction, "Disassembled selection:":output, vimState, currentConsoleState, currentDebuggerMode)
        
        _   -> return (NoAction, ["Key '" ++ [key] ++ "' not supported in visual mode"], vimState, currentConsoleState, currentDebuggerMode)

handleVimNormalModeKey :: Char -> VimState -> DebuggerConsoleState -> DebuggerMode -> FDX (DebuggerAction, [String], VimState, DebuggerConsoleState, DebuggerMode)
handleVimNormalModeKey key vimState debuggerConsoleState initialDebuggerMode = do
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
            return (NoAction, output, vimState { vsCursor = newPos', vsCount = Nothing, vsOperator = Nothing, vsYankBuffer = newYankBuffer, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
    
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
              return (NoAction, output, vimState { vsCursor = newPos', vsCount = Nothing, vsOperator = Nothing, vsYankBuffer = newYankBuffer, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
    
        -- Escape - clear pending operations or switch to CommandMode
        (Nothing, '\x1b') -> return (SwitchToCommandMode, ["Switching to Command Mode"], vimState { vsOperator = Nothing, vsCount = Nothing }, debuggerConsoleState, initialDebuggerMode)
        (Just _, '\x1b') -> return (NoAction, [""], vimState { vsOperator = Nothing, vsCount = Nothing },  debuggerConsoleState, initialDebuggerMode)
        
        -- Handle newline for help scrolling or as a no-op
        (Nothing, '\n') -> do
          machine <- get -- Get the current machine state
          let currentConsoleState = mConsoleState machine
          let helpTextLines = helpLines currentConsoleState
          let helpTextScrollPos = helpScrollPos currentConsoleState
          let availableContentHeight = termHeight - 2 -- Space for two columns and status line

          if not (null helpTextLines) then do
            let newScrollPos = helpTextScrollPos + availableContentHeight
            if newScrollPos >= length helpTextLines then do
              -- Reached end of help, clear help state
              modify (\m -> m { mConsoleState = (mConsoleState m) {
                                  outputLines = outputLines (mConsoleState m) ++ helpTextLines, -- Add help to output
                                  helpLines = [],
                                  helpScrollPos = 0
                                }
                              })
              machineAfterModify <- get -- Get the updated machine state
              let updatedConsoleState = mConsoleState machineAfterModify
              let updatedDebuggerMode = debuggerMode machineAfterModify
              return (NoAction, [], vimState { vsCount = Nothing, vsMessage = "" }, updatedConsoleState, updatedDebuggerMode )
            else do
              -- Scroll to next page of help
              modify (\m -> m { mConsoleState = (mConsoleState m) { helpScrollPos = newScrollPos } })
              machineAfterModify <- get -- Get the updated machine state
              let updatedConsoleState = mConsoleState machineAfterModify
              let updatedDebuggerMode = debuggerMode machineAfterModify
              return (NoAction, [], vimState { vsCount = Nothing, vsMessage = "" }, updatedConsoleState, updatedDebuggerMode )
          else do
            -- No help being displayed, act as no-op
            return (NoAction, [""], vimState { vsCount = Nothing, vsMessage = "" }, currentConsoleState, initialDebuggerMode )

        -- Numbers - build count prefix
        (Nothing, c) | isDigit c -> do
          let currentCount = fromMaybe 0 (vsCount vimState)
          let newCount = currentCount * 10 + (read [c])
          return (NoAction, [""], vimState { vsCount = Just newCount }, debuggerConsoleState, initialDebuggerMode)
    
    -- Operator commands
        (Nothing, 'v') ->
          -- Enter visual mode
          return (NoAction, ["Entered visual mode"], vimState {
            vsInVisualMode = True,
            vsVisualStart = currentPos
          }, debuggerConsoleState, initialDebuggerMode)
          
        (Nothing, c) | c `elem` ['d','c','y','w','l','b'] -> do
          case c of
            'd' -> return (NoAction, [""], vimState { vsOperator = Just DeleteOp }, debuggerConsoleState, initialDebuggerMode)
            'c' -> return (NoAction, [""], vimState { vsOperator = Just ChangeOp }, debuggerConsoleState, initialDebuggerMode)
            'y' -> return (NoAction, [""], vimState { vsOperator = Just YankOp }, debuggerConsoleState, initialDebuggerMode)
            'w' -> do  -- Yank/delete word object
                let motion = WordObject (fromMaybe 1 (vsCount vimState))
                let action = case vsOperator vimState of
                              Just DeleteOp -> Delete motion
                              Just ChangeOp -> Change motion
                              Just YankOp   -> Yank motion
                              _ -> Move motion
                (newPos, output) <- executeAction action currentPos vimState
                return (NoAction, output, vimState { vsCursor = newPos, vsOperator = Nothing }, debuggerConsoleState, initialDebuggerMode)
            'l' -> do  -- Yank/delete line object
                let motion = LineObject (fromMaybe 1 (vsCount vimState))
                let action = case vsOperator vimState of
                              Just DeleteOp -> Delete motion
                              Just ChangeOp -> Change motion
                              Just YankOp   -> Yank motion
                              _ -> Move motion
                (newPos, output) <- executeAction action currentPos vimState
                return (NoAction, output, vimState { vsCursor = newPos, vsOperator = Nothing }, debuggerConsoleState, initialDebuggerMode)
            'b' -> do  -- Yank/delete bracket object
                let motion = BracketObject
                let action = case vsOperator vimState of
                              Just DeleteOp -> Delete motion
                              Just ChangeOp -> Change motion
                              Just YankOp   -> Yank motion
                              _ -> Move motion
                (newPos, output) <- executeAction action currentPos vimState
                return (NoAction, output, vimState { vsCursor = newPos, vsOperator = Nothing }, debuggerConsoleState, initialDebuggerMode)
            _ -> return (NoAction, [""], vimState, debuggerConsoleState, initialDebuggerMode)

        -- Repeat last change
        (Nothing, '.') -> case vsLastChange vimState of
          Just (RepeatAction act) -> do
            (newPos, output) <- executeAction act currentPos vimState
            return (NoAction, output, vimState { vsCursor = newPos, vsMessage = head output }, debuggerConsoleState, initialDebuggerMode)
          Just (RepeatMotion mot) -> do
            newPos <- executeMotion mot currentPos
            let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 3
                  then newPos - fromIntegral ((termHeight - 3) * 2)
                  else vsViewStart vimState
            return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion mot) }, debuggerConsoleState, initialDebuggerMode)
          Just RepeatStep -> do
            return (ExecuteStep "step", [], vimState { vsCount = Nothing, vsMessage = "Stepped one instruction" }, debuggerConsoleState, initialDebuggerMode)
          Nothing -> return (NoAction, ["No previous change to repeat"], vimState { vsMessage = "No previous change to repeat" }, debuggerConsoleState, initialDebuggerMode)
    
        -- Movement commands
        (Nothing, 'j') -> do
          putString "Naciśnięto 'j'" -- Debugowanie, changed to putString
          let motion = NextInstruction count
          newPos <- executeMotion motion currentPos
          let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 3
                then newPos - fromIntegral ((termHeight - 3) * 2)
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'k') -> do
          let motion = PrevInstruction count
          newPos <- executeMotion motion currentPos
          let newViewStart = if newPos < vsViewStart vimState
                then max 0 (newPos - fromIntegral (termHeight - 3))
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'h') -> do
          let motion = PrevByte count
          newPos <- executeMotion motion currentPos
          let newViewStart = if newPos < vsViewStart vimState
                then max 0 (newPos - fromIntegral (termHeight - 3) * 16)
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'l') -> do
          let motion = NextByte count
          newPos <- executeMotion motion currentPos
          let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 16
                then newPos - fromIntegral ((termHeight - 3) * 8)
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'w') -> do
          let motion = WordForward count
          newPos <- executeMotion motion currentPos
          return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'b') -> do
          let motion = WordBackward count
          newPos <- executeMotion motion currentPos
          return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'G') -> do
          case vsCount vimState of
            Just addr -> do
              let newPos = fromIntegral addr
              let motion = GotoAddressMotion newPos
              return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
            Nothing -> do
              let newPos = rPC (mRegs machine)
              let motion = GotoPC
              return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'g') -> do
          nextKey <- liftIO getKey
          case nextKey of
            'g' -> do
              let newPos = rPC (mRegs machine)
              let motion = GotoPC
              return (NoAction, ["Goto PC"], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
            _ -> return (NoAction, ["Invalid g command"], vimState { vsCount = Nothing }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'H') -> do
          let motion = TopOfPage
          newPos <- executeMotion motion currentPos
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode  )
        
        (Nothing, 'M') -> do
          let motion = MiddlePage
          newPos <- executeMotion motion currentPos
          return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'L') -> do
          let motion = EndOfPage
          newPos <- executeMotion motion currentPos
          let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 16
                then newPos - fromIntegral ((termHeight - 3) * 8)
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
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
              return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
            Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, '+') -> do
          let action = Increment count
          (newPos, output) <- executeAction action currentPos vimState
          return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, '-') -> do
          let action = Decrement count
          (newPos, output) <- executeAction action currentPos vimState
          return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, '~') -> do
          putString "Bit number (0-7): " -- Changed to putString
          liftIO $ hSetEcho stdin True
          bitStr <- liftIO getInput
          liftIO $ hSetEcho stdin False
          case parseCount bitStr of
            Just bit -> do
              let action = ToggleBit bit
              (newPos, output) <- executeAction action currentPos vimState
              return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
            Nothing -> return (NoAction, ["Invalid bit number"], vimState { vsCount = Nothing, vsMessage = "Invalid bit number" }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'B') -> do
          let action = AddBreakpoint
          (newPos, output) <- executeAction action currentPos vimState
          return (NoAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, '\r') -> do
          let action = ExecuteToHere
          (newPos, output) <- executeAction action currentPos vimState
          return (ExecuteStep "execute-to-here", output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
        
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
              return (NoAction, ["Found byte at $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
            Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'F') -> do
          putString "Find byte backward (hex): " -- Changed to putString
          liftIO $ hSetEcho stdin True
          hexStr <- liftIO getInput
          liftIO $ hSetEcho stdin False
          case parseHexByte hexStr of
            Just targetByte -> do
              newPos <- executeMotion (FindByte targetByte False) currentPos
              let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, False), vsCount = Nothing, vsMessage = "Found byte at $" ++ showHex newPos "" }
              return (NoAction, ["Found byte at $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
            Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 't') -> do
          putString "Till byte (hex): " -- Changed to putString
          liftIO $ hSetEcho stdin True
          hexStr <- liftIO getInput
          liftIO $ hSetEcho stdin False
          case parseHexByte hexStr of
            Just targetByte -> do
              newPos <- executeMotion (TillByte targetByte True) currentPos
              let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, True), vsCount = Nothing, vsMessage = "Moved to $" ++ showHex newPos "" }
              return (NoAction, ["Moved to $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
            Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'T') -> do
          putString "Till byte backward (hex): " -- Changed to putString
          liftIO $ hSetEcho stdin True
          hexStr <- liftIO getInput
          liftIO $ hSetEcho stdin False
          case parseHexByte hexStr of
            Just targetByte -> do
              newPos <- executeMotion (TillByte targetByte False) currentPos
              let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, False), vsCount = Nothing, vsMessage = "Moved to $" ++ showHex newPos "" }
              return (NoAction, ["Moved to $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
            Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, ';') -> do
          case vsLastFind vimState of
            Just (byte, forward) -> do
              newPos <- executeMotion (FindByte byte forward) currentPos
              return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = "Repeated find to $" ++ showHex newPos "" }, debuggerConsoleState, initialDebuggerMode )
            Nothing -> return (NoAction, ["No previous find"], vimState { vsCount = Nothing, vsMessage = "No previous find" }, debuggerConsoleState, initialDebuggerMode )
        
        (Nothing, ',') -> do
          case vsLastFind vimState of
            Just (byte, forward) -> do
              newPos <- executeMotion (FindByte byte (not forward)) currentPos
              return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = "Repeated find to $" ++ showHex newPos "" }, debuggerConsoleState, initialDebuggerMode )
            Nothing -> return (NoAction, ["No previous find"], vimState { vsCount = Nothing, vsMessage = "No previous find" }, debuggerConsoleState, initialDebuggerMode )
      
    -- View mode switching
        (Nothing, 'v') -> do
          let newViewMode = case vsViewMode vimState of
                CodeView -> MemoryView
                MemoryView -> RegisterView
                RegisterView -> StackView
                StackView -> CodeView
          return (NoAction, ["Switched to " ++ show newViewMode], vimState { vsViewMode = newViewMode, vsMessage = "Switched to " ++ show newViewMode }, debuggerConsoleState, initialDebuggerMode )
    
        -- Step execution
        (Nothing, 's') -> return (ExecuteStep [key], [], vimState { vsCount = Nothing, vsMessage = "Stepped one instruction", vsLastChange = Just RepeatStep }, debuggerConsoleState, initialDebuggerMode )
        
        -- Continue execution
        (Nothing, 'c') -> return (ExecuteStep "continue", [], vimState { vsCount = Nothing, vsMessage = "Continuing execution" }, debuggerConsoleState, initialDebuggerMode )
        
        -- Marks
        (Nothing, 'm') -> do
          putString "Mark key: " -- Changed to putString
          keyChar <- liftIO getKey
          return (NoAction, ["Set mark '" ++ [keyChar] ++ "' at $" ++ showHex currentPos ""], vimState { vsMarks = Map.insert keyChar currentPos (vsMarks vimState), vsMessage = "Set mark '" ++ [keyChar] ++ "'" }, debuggerConsoleState, initialDebuggerMode )
        
        (Nothing, '\'') -> do
          putString "Goto mark: " -- Changed to putString
          keyChar <- liftIO getKey
          case Map.lookup keyChar (vsMarks vimState) of
            Just addr -> return (NoAction, ["Moved to mark '" ++ [keyChar] ++ "' at $" ++ showHex addr ""], vimState { vsCursor = addr, vsViewStart = addr, vsMessage = "Moved to mark '" ++ [keyChar] ++ "'" }, debuggerConsoleState, initialDebuggerMode )
            Nothing -> return (NoAction, ["No mark '" ++ [keyChar] ++ "'"], vimState { vsMessage = "No mark '" ++ [keyChar] ++ "'" }, debuggerConsoleState, initialDebuggerMode )
        
        -- Legacy commands
        (Nothing, 'q') -> return (QuitEmulator, [], vimState { vsMessage = "Quitting emulator" }, debuggerConsoleState, initialDebuggerMode )
        (Nothing, 'x') -> return (ExitDebugger, [], vimState { vsMessage = "Exiting debugger" }, debuggerConsoleState, initialDebuggerMode )
        
        -- Colon commands (handled by VimModeEnhanced now)
        (Nothing, ':') -> do
          -- Removed: liftIO $ hSetEcho stdin True -- Echo is now handled by manual rendering
          return (SwitchToVimCommandMode, [], vimState { vsInCommandMode = True, vsCommandBuffer = ":", vsMessage = "" }, debuggerConsoleState, initialDebuggerMode )
    
    -- Default
        _ -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped" }, debuggerConsoleState, initialDebuggerMode)
