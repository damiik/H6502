module MOS6502Emulator.Debugger.VimMode.HandleVisualKey ( handleVisualKey ) where
import Control.Monad.State (get, modify, liftIO)
import System.IO (stdin, hSetEcho)
import qualified Data.Map as Map
import Numeric (showHex)

import MOS6502Emulator.Core(Machine(..), FDX, fetchByteMem, writeByteMem, _mRegs, _mConsoleState) -- Removed parseHexWord, parseHexByte, getRegisters
import MOS6502Emulator.Debugger.Core (DebuggerAction(..), DebuggerConsoleState(..), DebuggerMode(..), parseCount) -- Ensure DebuggerAction and DebuggerMode are imported from here
import MOS6502Emulator.DissAssembler(disassembleInstructionPure)
import MOS6502Emulator.Debugger.VimMode.Core ( VimState(..), Motion(..), Action(..), ViewMode(..), RepeatableCommand(..), OperatorType(..), VisualType(..), ObjectModifier(..), TextObjectType(..), CommandState(..))
import MOS6502Emulator.Debugger.VimMode.Execute (executeMotion, executeAction)
import MOS6502Emulator.Debugger.Console(getKey, getInput, putString, termHeight)
import MOS6502Emulator.Debugger.Utils (parseHexByte) -- Import from Debugger.Utils


-- TODO: Add real actions
-- | Handle keys in visual mode
handleVisualKey :: Char -> VimState -> FDX (DebuggerAction, [String], VimState, DebuggerConsoleState, DebuggerMode)
handleVisualKey key vimState = do
    machine <- get -- Get machine state to access console and mode
    let currentConsoleState = _mConsoleState machine
    let currentDebuggerMode = _debuggerMode machine -- Use accessor here
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
            newPos <- executeMotion (NextInstruction 1) (vsCursor vimState) vimState
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos }, currentConsoleState, currentDebuggerMode)
        'k' -> do
            newPos <- executeMotion (PrevInstruction 1) (vsCursor vimState) vimState
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos }, currentConsoleState, currentDebuggerMode)
        'h' -> do
            newPos <- executeMotion (PrevByte 1) (vsCursor vimState) vimState
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos }, currentConsoleState, currentDebuggerMode)
        'l' -> do
            newPos <- executeMotion (NextByte 1) (vsCursor vimState) vimState
            return (NoAction, [""], vimState { vsCursor = newPos, vsVisualEnd = newPos }, currentConsoleState, currentDebuggerMode)
        
        -- Show disassembly of selection
        'D' -> do
            let disassembleAddr addr =
                    let (instr, _) = disassembleInstructionPure addr machine
                    in "  " ++ showHex addr ": " ++ instr
            let output = map disassembleAddr [minAddr..maxAddr]
            return (NoAction, "Disassembled selection:":output, vimState, currentConsoleState, currentDebuggerMode)
        
        _   -> return (NoAction, ["Key '" ++ [key] ++ "' not supported in visual mode"], vimState, currentConsoleState, currentDebuggerMode)
