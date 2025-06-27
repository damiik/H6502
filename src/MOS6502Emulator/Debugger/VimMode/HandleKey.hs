module MOS6502Emulator.Debugger.VimMode.HandleKey ( handleVimNormalModeKey ) where
import Control.Monad.State (get, modify, liftIO)
import Control.Lens -- Import Control.Lens for lens operators
import MOS6502Emulator.Lenses -- Import custom lenses
import System.IO (stdin, hSetEcho)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Numeric (showHex)

import MOS6502Emulator.Core(Machine(..), FDX,  _mRegs, _mConsoleState) -- Removed parseHexWord, parseHexByte, getRegisters
import MOS6502Emulator.Debugger.Core (DebuggerAction(..), DebuggerConsoleState(..), DebuggerMode(..), parseCount) -- Ensure DebuggerAction and DebuggerMode are imported from here
import MOS6502Emulator.Registers(Registers(..))
import MOS6502Emulator.Debugger.VimMode.Core ( VimState(..), Motion(..), Action(..), ViewMode(..), RepeatableCommand(..), OperatorType(..), VisualType(..), ObjectModifier(..), TextObjectType(..), CommandState(..))
import MOS6502Emulator.Debugger.VimMode.Execute (executeMotion, executeAction)
import MOS6502Emulator.Debugger.VimMode.HandleVisualKey (handleVisualKey)
import MOS6502Emulator.Debugger.Console(renderScreen, getKey, getInput, putString, termHeight)
import MOS6502Emulator.Debugger.Utils (parseHexByte) -- Import from Debugger.Utils
import MOS6502Emulator.Debugger.VimMode.CommandParser (parseVimCommand)


handleVimNormalModeKey :: Char -> VimState -> DebuggerConsoleState -> DebuggerMode -> FDX (DebuggerAction, [String], VimState, DebuggerConsoleState, DebuggerMode)
handleVimNormalModeKey key vimState debuggerConsoleState initialDebuggerMode = do
  machine <- get
  let currentPos = vsCursor vimState
  let count = fromMaybe 1 (vsCount vimState)
  
  -- Handle visual mode first
  if vsInVisualMode vimState
    then handleVisualKey key vimState
    else
      case vsOperator vimState of
        Just op -> -- Operator is pending, handle motion/object
          case key of
            '\x1b' -> return (NoDebuggerAction, [""], vimState { vsOperator = Nothing, vsCount = Nothing },  debuggerConsoleState, initialDebuggerMode)
            'i' -> return (NoDebuggerAction, [""], vimState {vsCommandState = Object op Inner}, debuggerConsoleState, initialDebuggerMode)
            'a' -> return (NoDebuggerAction, [""], vimState {vsCommandState = Object op Outer}, debuggerConsoleState, initialDebuggerMode)
            -- Handle motions directly after operator
            'j' -> handleMotion op (NextInstruction count)
            'k' -> handleMotion op (PrevInstruction count)
            'h' -> handleMotion op (PrevByte count)
            'l' -> handleMotion op (NextByte count)
            'w' -> handleMotion op (WordForward count)
            'b' -> handleMotion op (WordBackward count)
            'G' -> handleMotion op (GotoAddressMotion $ fromIntegral $ fromMaybe (fromIntegral $ _rPC (_mRegs machine)) (vsCount vimState))
            'g' -> do
              nextKey <- liftIO getKey
              case nextKey of
                'g' -> handleMotion op GotoPC
                _ -> return (NoDebuggerAction, ["Invalid g command for operator"], vimState { vsCount = Nothing, vsMessage = "Invalid g command for operator" }, debuggerConsoleState, initialDebuggerMode)
            'H' -> handleMotion op TopOfPage
            'M' -> handleMotion op MiddlePage
            'L' -> handleMotion op EndOfPage
            -- Handle text objects after operator
            -- Note: 'w', 'l', 'b', '"' are also motions, so this needs careful ordering or separate handling
            -- For now, assuming text objects are distinct from simple motions
            _ -> return (NoDebuggerAction, ["Key '" ++ [key] ++ "' not mapped for operator"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped for operator" }, debuggerConsoleState, initialDebuggerMode)
          where
            handleMotion op' motion = do
              let action = case op' of
                            DeleteOp -> Delete motion
                            ChangeOp -> Change motion
                            YankOp   -> Yank motion
              (dbgAction, newPos', output) <- executeAction action currentPos vimState
              let newYankBuffer = if op' == YankOp
                  then Map.insert (vsRegister vimState) (maybe [] id (Map.lookup '"' (vsYankBuffer vimState))) (vsYankBuffer vimState)
                  else vsYankBuffer vimState
              return (dbgAction, output, vimState {vsCursor = newPos', vsCount = Nothing, vsYankBuffer = newYankBuffer, vsLastChange = Just (RepeatAction action), vsOperator = Nothing}, debuggerConsoleState, initialDebuggerMode)
            -- Removed handleObjectMotion as it was only used by the problematic 'p' case.
            -- If text objects are to be implemented, they should be added with proper TextObjectType constructors.

        Nothing -> -- No operator pending, handle normal commands
          case key of
            '\x1b' -> return (SetDebuggerModeAction CommandMode, ["Switching to Command Mode"], vimState { vsOperator = Nothing, vsCount = Nothing }, debuggerConsoleState, initialDebuggerMode)
            '\n' -> do -- Handle newline for help scrolling or as a no-op
              machine' <- get -- Get the current machine state
              let currentConsoleState = _mConsoleState machine'
              let helpTextLines = _helpLines currentConsoleState
              let helpTextScrollPos = _helpScrollPos currentConsoleState
              let availableContentHeight = termHeight - 2 -- Space for two columns and status line

              if not (null helpTextLines) then do
                let newScrollPos = helpTextScrollPos + availableContentHeight
                if newScrollPos >= length helpTextLines then do
                  let updatedConsoleState = currentConsoleState {
                    _outputLines = _outputLines currentConsoleState ++ helpTextLines,
                    _helpLines = [],
                    _helpScrollPos = 0
                  }
                  modify (\m -> m { _mConsoleState = updatedConsoleState })
                  machineAfterModify <- get
                  let finalConsoleState = _mConsoleState machineAfterModify
                  let updatedDebuggerMode = _debuggerMode machineAfterModify
                  return (NoDebuggerAction, [], vimState { vsCount = Nothing, vsMessage = "" }, finalConsoleState, updatedDebuggerMode )
                else do
                  let updatedConsoleState = currentConsoleState { _helpScrollPos = newScrollPos }
                  modify (\m -> m { _mConsoleState = updatedConsoleState })
                  machineAfterModify <- get
                  let finalConsoleState = _mConsoleState machineAfterModify
                  let updatedDebuggerMode = _debuggerMode machineAfterModify
                  return (NoDebuggerAction, [], vimState { vsCount = Nothing, vsMessage = "" }, finalConsoleState, updatedDebuggerMode )
              else do
                return (NoDebuggerAction, [""], vimState { vsCount = Nothing, vsMessage = "" }, currentConsoleState, initialDebuggerMode )

            c | isDigit c -> do
              let currentCount = fromMaybe 0 (vsCount vimState)
              let newCount = currentCount * 10 + (read [c])
              return (NoDebuggerAction, [""], vimState { vsCount = Just newCount }, debuggerConsoleState, initialDebuggerMode)
        
            '.' -> case vsLastChange vimState of
              Just (RepeatAction act) -> do
                (dbgAction, newPos, output) <- executeAction act currentPos vimState
                return (dbgAction, output, vimState { vsCursor = newPos, vsMessage = head output }, debuggerConsoleState, initialDebuggerMode)
              Just (RepeatMotion mot) -> do
                newPos <- executeMotion mot currentPos vimState
                let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 3
                      then newPos - fromIntegral ((termHeight - 3) * 2)
                      else vsViewStart vimState
                return (NoDebuggerAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion mot) }, debuggerConsoleState, initialDebuggerMode)
              Just RepeatStep -> do
                return (ExecuteStepAction, [], vimState { vsCount = Nothing, vsMessage = "Stepped one instruction" }, debuggerConsoleState, initialDebuggerMode)
              Nothing -> return (NoDebuggerAction, ["No previous change to repeat"], vimState { vsMessage = "No previous change to repeat" }, debuggerConsoleState, initialDebuggerMode)
        
            'j' -> handleMotion (NextInstruction count)
            'k' -> handleMotion (PrevInstruction count)
            'h' -> handleMotion (PrevByte count)
            'l' -> handleMotion (NextByte count)
            'w' -> handleMotion (WordForward count)
            'b' -> handleMotion (WordBackward count)
            'G' -> handleMotion (GotoAddressMotion $ fromIntegral $ fromMaybe (fromIntegral $ _rPC (_mRegs machine)) (vsCount vimState))
            'g' -> do
              nextKey <- liftIO getKey
              case nextKey of
                'g' -> handleMotion GotoPC
                _ -> return (NoDebuggerAction, ["Invalid g command"], vimState { vsCount = Nothing }, debuggerConsoleState, initialDebuggerMode)
            'H' -> handleMotion TopOfPage
            'M' -> handleMotion MiddlePage
            'L' -> handleMotion EndOfPage
            'r' -> do
              putString "Replace with hex byte: "
              liftIO $ hSetEcho stdin True
              hexStr <- liftIO getInput
              liftIO $ hSetEcho stdin False
              case parseHexByte hexStr of
                Just newByte -> handleAction (Set newByte)
                Nothing -> return (NoDebuggerAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
            '+' -> handleAction (Increment count)
            '-' -> handleAction (Decrement count)
            '~' -> do
              putString "Bit number (0-7): "
              liftIO $ hSetEcho stdin True
              bitStr <- liftIO getInput
              liftIO $ hSetEcho stdin False
              case parseCount bitStr of
                Just bit -> handleAction (ToggleBit bit)
                Nothing -> return (NoDebuggerAction, ["Invalid bit number"], vimState { vsCount = Nothing, vsMessage = "Invalid bit number" }, debuggerConsoleState, initialDebuggerMode)
            'B' -> handleAction AddBreakpoint
            '\r' -> do
              let action = ExecuteToHere
              (dbgAction, newPos, output) <- executeAction action currentPos vimState
              renderScreen machine []
              return (dbgAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
            'f' -> do
              putString "Find byte (hex): "
              liftIO $ hSetEcho stdin True
              hexStr <- liftIO getInput
              liftIO $ hSetEcho stdin False
              case parseHexByte hexStr of
                Just targetByte -> do
                  newPos <- executeMotion (FindByte targetByte True) currentPos vimState
                  let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, True), vsCount = Nothing, vsMessage = "Found byte at $" ++ showHex newPos "" }
                  return (NoDebuggerAction, ["Found byte at $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
                Nothing -> return (NoDebuggerAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
            'F' -> do
              putString "Find byte backward (hex): "
              liftIO $ hSetEcho stdin True
              hexStr <- liftIO getInput
              liftIO $ hSetEcho stdin False
              case parseHexByte hexStr of
                Just targetByte -> do
                  newPos <- executeMotion (FindByte targetByte False) currentPos vimState
                  let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, False), vsCount = Nothing, vsMessage = "Found byte at $" ++ showHex newPos "" }
                  return (NoDebuggerAction, ["Found byte at $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
                Nothing -> return (NoDebuggerAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
            't' -> do
              putString "Till byte (hex): "
              liftIO $ hSetEcho stdin True
              hexStr <- liftIO getInput
              liftIO $ hSetEcho stdin False
              case parseHexByte hexStr of
                Just targetByte -> do
                  newPos <- executeMotion (TillByte targetByte True) currentPos vimState
                  let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, True), vsCount = Nothing, vsMessage = "Moved to $" ++ showHex newPos "" }
                  return (NoDebuggerAction, ["Moved to $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
                Nothing -> return (NoDebuggerAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
            'T' -> do
              putString "Till byte backward (hex): "
              liftIO $ hSetEcho stdin True
              hexStr <- liftIO getInput
              liftIO $ hSetEcho stdin False
              case parseHexByte hexStr of
                Just targetByte -> do
                  newPos <- executeMotion (TillByte targetByte False) currentPos vimState
                  let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, False), vsCount = Nothing, vsMessage = "Moved to $" ++ showHex newPos "" }
                  return (NoDebuggerAction, ["Moved to $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
                Nothing -> return (NoDebuggerAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
            ';' -> do
              case vsLastFind vimState of
                Just (byte, forward) -> do
                  newPos <- executeMotion (FindByte byte forward) currentPos vimState
                  return (NoDebuggerAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = "Repeated find to $" ++ showHex newPos "" }, debuggerConsoleState, initialDebuggerMode )
                Nothing -> return (NoDebuggerAction, ["No previous find"], vimState { vsCount = Nothing, vsMessage = "No previous find" }, debuggerConsoleState, initialDebuggerMode )
            ',' -> do
              case vsLastFind vimState of
                Just (byte, forward) -> do
                  newPos <- executeMotion (FindByte byte (not forward)) currentPos vimState
                  return (NoDebuggerAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = "Repeated find to $" ++ showHex newPos "" }, debuggerConsoleState, initialDebuggerMode )
                Nothing -> return (NoDebuggerAction, ["No previous find"], vimState { vsCount = Nothing, vsMessage = "No previous find" }, debuggerConsoleState, initialDebuggerMode )
          
            'v' -> do
              return (NoDebuggerAction, ["Entered visual mode"], vimState { vsInVisualMode = True, vsVisualStart = currentPos}, debuggerConsoleState, initialDebuggerMode)
        
            's' -> do
              return (ExecuteStepAction, [], vimState { vsCount = Nothing, vsMessage = "Stepped one instruction", vsLastChange = Just RepeatStep }, debuggerConsoleState, initialDebuggerMode )
            
            'c' -> return (ExecuteStepAction, [], vimState { vsCount = Nothing, vsMessage = "Continuing execution" }, debuggerConsoleState, initialDebuggerMode )
            
            'm' -> do
              putString "Mark key: "
              keyChar <- liftIO getKey
              return (NoDebuggerAction, ["Set mark '" ++ [keyChar] ++ "' at $" ++ showHex currentPos ""], vimState { vsMarks = Map.insert keyChar currentPos (vsMarks vimState), vsMessage = "Set mark '" ++ [keyChar] ++ "'" }, debuggerConsoleState, initialDebuggerMode )
            
            '\'' -> do
              putString "Goto mark: "
              keyChar <- liftIO getKey
              case Map.lookup keyChar (vsMarks vimState) of
                Just addr -> return (NoDebuggerAction, ["Moved to mark '" ++ [keyChar] ++ "' at $" ++ showHex addr ""], vimState { vsCursor = addr, vsViewStart = addr, vsMessage = "Moved to mark '" ++ [keyChar] ++ "'" }, debuggerConsoleState, initialDebuggerMode )
                Nothing -> return (NoDebuggerAction, ["No mark '" ++ [keyChar] ++ "'"], vimState { vsMessage = "No mark '" ++ [keyChar] ++ "'" }, debuggerConsoleState, initialDebuggerMode )
            
            'q' -> return (QuitEmulatorAction, [], vimState { vsMessage = "Quitting emulator" }, debuggerConsoleState, initialDebuggerMode )
            'x' -> return (ExitDebuggerAction, [], vimState { vsMessage = "Exiting debugger" }, debuggerConsoleState, initialDebuggerMode )
            
            ':' -> do
              return (SetDebuggerModeAction VimCommandMode, [], vimState { vsInCommandMode = True, vsCommandBuffer = ":", vsMessage = "" }, debuggerConsoleState, initialDebuggerMode )
        
            -- Operators
            'd' -> return (NoDebuggerAction, [""], vimState { vsOperator = Just DeleteOp}, debuggerConsoleState, initialDebuggerMode)
            'c' -> return (NoDebuggerAction, [""], vimState { vsOperator = Just ChangeOp}, debuggerConsoleState, initialDebuggerMode)
            'y' -> return (NoDebuggerAction, [""], vimState { vsOperator = Just YankOp }, debuggerConsoleState, initialDebuggerMode)

            -- Register selection
            '"' -> return (NoDebuggerAction, [""], vimState { vsCommandState = VimPendingCompose }, debuggerConsoleState, initialDebuggerMode)

            _ -> return (NoDebuggerAction, ["Key '" ++ [key] ++ "' not mapped"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped" }, debuggerConsoleState, initialDebuggerMode)
          where
            handleMotion motion = do
              newPos <- executeMotion motion currentPos vimState
              let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 3
                    then newPos - fromIntegral ((termHeight - 3) * 2)
                    else vsViewStart vimState
              return (NoDebuggerAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
            handleAction action = do
              (dbgAction, newPos, output) <- executeAction action currentPos vimState
              return (dbgAction, output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
