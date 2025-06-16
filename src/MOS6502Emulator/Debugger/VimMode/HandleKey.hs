module MOS6502Emulator.Debugger.VimMode.HandleKey ( handleVimNormalModeKey ) where
import Control.Monad.State (get, modify, liftIO)
import System.IO (stdin, hSetEcho)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Numeric (showHex)

import MOS6502Emulator.Core(Machine(..), FDX,  mRegs, mConsoleState) -- Removed parseHexWord, parseHexByte, getRegisters
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
      case (vsOperator vimState, key) of

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
    

        -- Repeat last change
        (Nothing, '.') -> case vsLastChange vimState of
          Just (RepeatAction act) -> do
            (newPos, output) <- executeAction act currentPos vimState
            return (NoAction, output, vimState { vsCursor = newPos, vsMessage = head output }, debuggerConsoleState, initialDebuggerMode)
          Just (RepeatMotion mot) -> do
            newPos <- executeMotion mot currentPos vimState
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
          newPos <- executeMotion motion currentPos vimState
          let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 3
                then newPos - fromIntegral ((termHeight - 3) * 2)
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'k') -> do
          let motion = PrevInstruction count
          newPos <- executeMotion motion currentPos vimState
          let newViewStart = if newPos < vsViewStart vimState
                then max 0 (newPos - fromIntegral (termHeight - 3))
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'h') -> do
          let motion = PrevByte count
          newPos <- executeMotion motion currentPos vimState
          let newViewStart = if newPos < vsViewStart vimState
                then max 0 (newPos - fromIntegral (termHeight - 3) * 16)
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'l') -> do
          let motion = NextByte count
          newPos <- executeMotion motion currentPos vimState
          let newViewStart = if newPos >= vsViewStart vimState + fromIntegral (termHeight - 3) * 16
                then newPos - fromIntegral ((termHeight - 3) * 8)
                else vsViewStart vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newViewStart, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'w') -> do
          let motion = WordForward count
          newPos <- executeMotion motion currentPos vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'b') -> do
          let motion = WordBackward count
          newPos <- executeMotion motion currentPos vimState
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
          newPos <- executeMotion motion currentPos vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsViewStart = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode  )
        
        (Nothing, 'M') -> do
          let motion = MiddlePage
          newPos <- executeMotion motion currentPos vimState
          return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsLastChange = Just (RepeatMotion motion) }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, 'L') -> do
          let motion = EndOfPage
          newPos <- executeMotion motion currentPos vimState
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
          renderScreen machine []
          return (ExecuteStep "execute-to-here", output, vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = head output, vsLastChange = Just (RepeatAction action) }, debuggerConsoleState, initialDebuggerMode)
        
        -- Find commands
        (Nothing, 'f') -> do
          putString "Find byte (hex): " -- Changed to putString
          liftIO $ hSetEcho stdin True
          hexStr <- liftIO getInput
          liftIO $ hSetEcho stdin False
          case parseHexByte hexStr of
            Just targetByte -> do
              newPos <- executeMotion (FindByte targetByte True) currentPos vimState
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
              newPos <- executeMotion (FindByte targetByte False) currentPos vimState
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
              newPos <- executeMotion (TillByte targetByte True) currentPos vimState
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
              newPos <- executeMotion (TillByte targetByte False) currentPos vimState
              let newState = vimState { vsCursor = newPos, vsLastFind = Just (targetByte, False), vsCount = Nothing, vsMessage = "Moved to $" ++ showHex newPos "" }
              return (NoAction, ["Moved to $" ++ showHex newPos ""], newState, debuggerConsoleState, initialDebuggerMode)
            Nothing -> return (NoAction, ["Invalid hex byte"], vimState { vsCount = Nothing, vsMessage = "Invalid hex byte" }, debuggerConsoleState, initialDebuggerMode)
        
        (Nothing, ';') -> do
          case vsLastFind vimState of
            Just (byte, forward) -> do
              newPos <- executeMotion (FindByte byte forward) currentPos vimState
              return (NoAction, [""], vimState { vsCursor = newPos, vsCount = Nothing, vsMessage = "Repeated find to $" ++ showHex newPos "" }, debuggerConsoleState, initialDebuggerMode )
            Nothing -> return (NoAction, ["No previous find"], vimState { vsCount = Nothing, vsMessage = "No previous find" }, debuggerConsoleState, initialDebuggerMode )
        
        (Nothing, ',') -> do
          case vsLastFind vimState of
            Just (byte, forward) -> do
              newPos <- executeMotion (FindByte byte (not forward)) currentPos vimState
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
        (Nothing, 's') -> do
          -- Removed renderScreen here, it will be called after fdxSingleCycle in runLoop
          return (ExecuteStep [key], [], vimState { vsCount = Nothing, vsMessage = "Stepped one instruction", vsLastChange = Just RepeatStep }, debuggerConsoleState, initialDebuggerMode )
        
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
    --    _ -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped" }, debuggerConsoleState, initialDebuggerMode)
        -- Command composition state machine
        _ -> case vsCommandState vimState of
            NoCommand ->            -- NoCommand state: handle operators, visual mode, etc.
                case key of
                    -- Visual mode
                    'v' -> return (NoAction, ["Entered visual mode"], vimState { vsInVisualMode = True, vsVisualStart = currentPos}, debuggerConsoleState, initialDebuggerMode)
                    -- Operators
                    'd' -> return (NoAction, [""], vimState { vsCommandState = Operator DeleteOp}, debuggerConsoleState, initialDebuggerMode)
                    'c' -> return (NoAction, [""], vimState { vsCommandState = Operator ChangeOp}, debuggerConsoleState, initialDebuggerMode)
                    'y' -> return (NoAction, [""], vimState { vsCommandState = Operator YankOp }, debuggerConsoleState, initialDebuggerMode)
                    _ -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped" }, debuggerConsoleState, initialDebuggerMode)
            
            Operator op ->            -- Operator state: handle modifiers (i,a) or motions
                case key of
                    'i' -> return (NoAction, [""], vimState {vsCommandState = Object op Inner}, debuggerConsoleState, initialDebuggerMode)
                    'a' -> return (NoAction, [""], vimState {vsCommandState = Object op Outer}, debuggerConsoleState, initialDebuggerMode)
                    _   -> do
                        -- Handle motion directly
                        let count' = fromMaybe 1 (vsCount vimState) -- Use count' to avoid shadowing
                        let maybeMotion = case key of
                                          'j' -> Just $ NextInstruction count'
                                          'k' -> Just $ PrevInstruction count'
                                          'h' -> Just $ PrevByte count'
                                          'l' -> Just $ NextByte count'
                                          'w' -> Just $ WordForward count'
                                          'b' -> Just $ WordBackward count'
                                          'G' -> Just $ GotoAddressMotion $ fromIntegral $ fromMaybe (fromIntegral $ rPC (mRegs machine)) (vsCount vimState)
                                          'g' -> Just GotoPC
                                          'H' -> Just TopOfPage
                                          'M' -> Just MiddlePage
                                          'L' -> Just EndOfPage
                                          _   -> Nothing
                        
                        case maybeMotion of
                          Just motion -> handleMotion motion
                          Nothing -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped for motion"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped for motion" }, debuggerConsoleState, initialDebuggerMode)
                where
                  handleMotion motion = do
                    let action = case op of
                                  DeleteOp -> Delete motion
                                  ChangeOp -> Change motion
                                  YankOp   -> Yank motion
                    (newPos', output) <- executeAction action currentPos vimState
                    let newYankBuffer = if op == YankOp
                        then Map.insert (vsRegister vimState) (maybe [] id (Map.lookup '"' (vsYankBuffer vimState))) (vsYankBuffer vimState)
                        else vsYankBuffer vimState
                    return (NoAction, output, vimState {vsCursor = newPos', vsCount = Nothing, vsYankBuffer = newYankBuffer, vsLastChange = Just (RepeatAction action), vsCommandState = NoCommand}, debuggerConsoleState, initialDebuggerMode)
            
            Object op mod -> do            -- Object state: handle text objects
                let n = fromMaybe 1 (vsCount vimState)
                let maybeMotion = case key of
                                'w' -> Just $ TextObject mod Word n
                                'l' -> Just $ TextObject mod Line n
                                'b' -> Just $ TextObject mod Bracket n
                                '"' -> Just $ TextObject mod Quote n
                                _   -> Nothing
                
                case maybeMotion of
                  Just motion -> handleObjectMotion motion
                  Nothing -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped for object"], vimState { vsCount = Nothing, vsMessage = "Key '" ++ [key] ++ "' not mapped for object" }, debuggerConsoleState, initialDebuggerMode)
                where
                  handleObjectMotion motion = do
                    let action = case op of
                                  DeleteOp -> Delete motion
                                  ChangeOp -> Change motion
                                  YankOp   -> Yank motion
                    (newPos, output) <- executeAction action currentPos vimState
                    return (NoAction, output, vimState {
                        vsCursor = newPos,
                        vsCount = Nothing,
                        vsLastChange = Just (RepeatAction action),
                        vsCommandState = NoCommand
                    }, debuggerConsoleState, initialDebuggerMode)            

            CommandModeV -> case key of
                '\x1b' ->  -- Escape
                          return (NoAction, [""], vimState {
                              vsCommandState = NoCommand,
                              vsCommandBuffer = "",
                              vsInCommandMode = False -- Reset in command mode flag
                          }, debuggerConsoleState, initialDebuggerMode)
                '\n' -> do  -- Enter
                    let cmdStr = vsCommandBuffer vimState
                    let parsedCmd = parseVimCommand cmdStr
                    (newPos, output) <- executeAction (ColonCommand parsedCmd) currentPos vimState
                    let newState = vimState { vsCursor = newPos, vsCommandState = NoCommand, vsCommandBuffer = "", vsInCommandMode = False, vsCount = Nothing, vsLastChange = Nothing }
                    return (NoAction, output, newState, debuggerConsoleState, initialDebuggerMode)
                _ -> return (NoAction, [""], vimState {vsCommandBuffer = vsCommandBuffer vimState ++ [key]}, debuggerConsoleState, initialDebuggerMode) -- Add char to buffer
            VimPendingCompose ->
                if key == '\x1b'  -- Escape
                then return (NoAction, ["Register selection cancelled"], vimState {
                    vsCommandState = NoCommand,
                    vsMessage = "Register selection cancelled"
                }, debuggerConsoleState, initialDebuggerMode)
                else return (NoAction, [""], vimState {
                    vsRegister = key,
                    vsCommandState = NoCommand,
                    vsMessage = "Set register '" ++ [key] ++ "'"
                }, debuggerConsoleState, initialDebuggerMode)
            
            _ -> return (NoAction, ["Key '" ++ [key] ++ "' not mapped"], vimState { vsCommandState = NoCommand, vsCommandBuffer = "" }, debuggerConsoleState, initialDebuggerMode)
