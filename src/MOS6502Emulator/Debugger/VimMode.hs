{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger.VimMode
  (
  ) where

import Numeric (showHex)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (put, get, modify)
import System.IO (hFlush, stdout, hSetEcho, stdin)
import qualified Data.Map.Strict as Map

import MOS6502Emulator.Registers
import MOS6502Emulator.Debugger (handleBreak, handleMemTrace, DebuggerAction) -- Import necessary functions from Debugger
import MOS6502Emulator.Debugger.Console (getKey, getInput, putOutput, putString, renderScreen, DebuggerConsoleState(..), initialConsoleState) -- Import I/O functions from Console, renderScreen, DebuggerConsoleState, and initialConsoleState

import MOS6502Emulator.Debugger.VimModeHandleKey (handleVimKey)
import MOS6502Emulator.Core (FDX, Machine (..), DebuggerAction (..))
import MOS6502Emulator.DissAssembler (disassembleInstructions)
-- | Handles 'm' sub-commands in VimMode.
handleMemTraceVim :: FDX (DebuggerAction, [String])
handleMemTraceVim = do
  -- Prompt for input, but don't print it directly to outputLines yet
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
      return (action, ["Memory Trace (a: add, d: delete): " ++ [key], "Add Memory Trace Block (start end [name]): " ++ input] ++ output)
    'd' -> do
      liftIO $ putStr "Delete Memory Trace Block (start end [name]): " >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleMemTrace args ""
      return (action, ["Memory Trace (a: add, d: delete): " ++ [key], "Delete Memory Trace Block (start end [name]): " ++ input] ++ output)
    _ -> do
      return (NoAction, ["Memory Trace (a: add, d: delete): " ++ [key], "Invalid Memory Trace command. Press 'm' to view blocks."])

-- | Handles 'b' sub-commands in VimMode.
handleBreakVim :: FDX (DebuggerAction, [String])
handleBreakVim = do
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
      return (action, ["Breakpoints (a: add, d: delete): " ++ [key], "Add Breakpoint (address): " ++ input] ++ output)
    'd' -> do
      liftIO $ putStr "Delete Breakpoint (address):" >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleBreak args ""
      return (action, ["Breakpoints (a: add, d: delete): " ++ [key], "Delete Breakpoint (address): " ++ input] ++ output)
    _ -> do
      return (NoAction, ["Breakpoints (a: add, d: delete): " ++ [key], "Invalid Breakpoint command."])

-- | Handles 'a' sub-commands in VimMode (Stored Addresses).
handleAddressVim :: FDX (DebuggerAction, [String])
handleAddressVim = do
  liftIO $ putStr "\nStored Addresses (s: store, g: goto):" >> hFlush stdout
  key <- liftIO getKey
  case key of
    's' -> do
      liftIO $ putStr "Store Current PC (key):" >> hFlush stdout
      keyChar <- liftIO getKey
      machine <- get
      let currentPC = rPC (mRegs machine)
      modify (\m -> m { storedAddresses = Map.insert keyChar currentPC (storedAddresses m) })
      return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "Stored PC $" ++ showHex currentPC "" ++ " at key '" ++ [keyChar] ++ "'"])
    'g' -> do
      liftIO $ putStr "Goto Stored Address (key):" >> hFlush stdout
      keyChar <- liftIO getKey
      machine <- get
      case Map.lookup keyChar (storedAddresses machine) of
        Just addr -> do
          put (machine { mRegs = (mRegs machine) { rPC = addr } })
          (disassembledOutput, _) <- disassembleInstructions addr 1
          return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "PC set to $" ++ showHex addr ""] ++ disassembledOutput)
        Nothing -> do
          return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "No address stored at key '" ++ [keyChar] ++ "'"])
    _ -> do
      return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "Invalid Stored Address command."])

-- | Handles key presses in VimMode.
-- handleVimKey :: Char -> FDX (DebuggerAction, [String])
-- handleVimKey key = do
--   case key of
--     'q' -> return (QuitEmulator, [])
--     'x' -> return (ExitDebugger, [])
--     'b' -> handleBreakVim
--     'm' -> handleMemTraceVim
--     'a' -> handleAddressVim
--     'j' -> return (ExecuteStep [key], [])
--     'z' -> return (ExecuteStep [key], []) -- Treat 'z' as a step for now
--     '\x1b' -> do
--       return (SwitchToCommandMode, ["Switching to Command Mode."])
--     _   -> return (NoAction, []) -- Placeholder for other Vim mode commands

-- | Helper function for the interactive debugger loop in VimMode.
-- interactiveLoopHelper :: DebuggerConsoleState -> FDX DebuggerAction
-- interactiveLoopHelper consoleState = do
--   machine <- get
--   if halted machine
--     then return QuitEmulator -- Return QuitEmulator if machine is halted
--     else do
--       liftIO $ renderScreen machine consoleState -- Render the screen
--       key <- liftIO getKey -- Read a single key
--       (action, output) <- handleVimKey key -- Handle the key press and get output

--       -- Update console state with the new output lines
--       let currentConsoleStateWithOutput = consoleState { outputLines = outputLines consoleState ++ output }

--       -- Decide what to do based on the action
--       case action of
--         ContinueLoop nextLastCommand -> interactiveLoopHelper (currentConsoleStateWithOutput { lastCommand = nextLastCommand, inputBuffer = "", cursorPosition = 0 }) -- Continue the loop
--         ExecuteStep  nextLastCommand -> return (ExecuteStep nextLastCommand) -- Return ExecuteStep to the main loop
--         ExitDebugger                 -> return ExitDebugger -- Return ExitDebugger
--         QuitEmulator                 -> return QuitEmulator -- Return QuitEmulator
--         NoAction                     -> interactiveLoopHelper (currentConsoleStateWithOutput { inputBuffer = "", cursorPosition = 0 }) -- Continue the loop with the updated state
--         SwitchToCommandMode          -> return SwitchToCommandMode -- Return SwitchToCommandMode
--         SwitchToVimMode              -> interactiveLoopHelper (currentConsoleStateWithOutput { inputBuffer = "", cursorPosition = 0 }) -- Stay in VimMode
