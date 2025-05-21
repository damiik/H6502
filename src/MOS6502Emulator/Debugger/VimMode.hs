{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger.VimMode
  ( getKey
  , handleVimKey
  , interactiveLoopHelper -- Export interactiveLoopHelper for use in MOS6502Emulator.hs
  ) where

import Data.Word (Word16, Word8)
import Numeric (showHex, readHex)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Monad.State (put, get, modify, MonadTrans (lift))
import System.IO (hFlush, stdout, hSetEcho, hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, hReady, getChar)
import Data.List (stripPrefix, cycle, take)
import Data.Maybe (mapMaybe, listToMaybe, isNothing)
import Data.Bits (Bits, (.&.))
import qualified Data.Map.Strict as Map
import System.IO.Error (isEOFError)
import Control.Exception (catch)

import MOS6502Emulator.Registers
import MOS6502Emulator.Memory (Memory(), writeByte)
import MOS6502Emulator.Machine (Machine(), DebuggerMode(..), FDX, writeByteMem, setPC_, getMemory, fetchByteMem, debugLogPath, breakpoints, memoryTraceBlocks, mRegs, storedAddresses, labelMap, lastDisassembledAddr, mMem, enableTrace, debuggerActive, halted, traceMemoryStart, traceMemoryEnd, debuggerMode, DebuggerAction(..), getRegisters)
import MOS6502Emulator.DissAssembler (disassembleInstruction, disassembleInstructions)
import MOS6502Emulator.Debugger (handleBreak, handleMemTrace, parseHexWord) -- Import necessary functions from Debugger
import MOS6502Emulator.Debugger.Console (getKey, getInput, putOutput, putString, renderScreen, DebuggerConsoleState(..), initialConsoleState) -- Import I/O functions from Console, renderScreen, DebuggerConsoleState, and initialConsoleState

-- | Handles 'm' sub-commands in VimMode.
handleMemTraceVim :: FDX DebuggerAction
handleMemTraceVim = do
  liftIO $ putStr "\nMemory Trace (a: add, d: delete):"
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStr "Add Memory Trace Block (start end [name]): "
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, _) <- handleMemTrace args "" -- Capture action and discard output
      return action
    'd' -> do
      liftIO $ putStr "Delete Memory Trace Block (start end [name]): " >> hFlush stdout
      input <- liftIO getLine
      let args = words input
      (action, _) <- handleMemTrace args "" -- Capture action and discard output
      return action
    _ -> do
      liftIO $ putStr "Invalid Memory Trace command. Press 'm' to view blocks."
      return NoAction

-- | Handles 'b' sub-commands in VimMode.
handleBreakVim :: FDX DebuggerAction
handleBreakVim = do
  liftIO $ putOutput "\nBreakpoints (a: add, d: delete):"
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putOutput "Add Breakpoint (address):"
      input <- liftIO getInput
      let args = words input
      (action, _) <- handleBreak args "" -- Capture action and discard output
      return action
    'd' -> do
      liftIO $ putOutput "Delete Breakpoint (address):"
      input <- liftIO getInput
      let args = words input
      (action, _) <- handleBreak args "" -- Capture action and discard output
      return action
    _ -> do
      liftIO $ putOutput "Invalid Breakpoint command."
      return NoAction

-- | Handles 'a' sub-commands in VimMode (Stored Addresses).
handleAddressVim :: FDX DebuggerAction
handleAddressVim = do
  liftIO $ putOutput "\nStored Addresses (s: store, g: goto):"
  key <- liftIO getKey
  case key of
    's' -> do
      liftIO $ putOutput "Store Current PC (key):"
      keyChar <- liftIO getKey
      machine <- get
      let currentPC = rPC (mRegs machine)
      modify (\m -> m { storedAddresses = Map.insert keyChar currentPC (storedAddresses m) })
      liftIO $ putOutput $ "Stored PC $" ++ showHex currentPC "" ++ " at key '" ++ [keyChar] ++ "'"
      return NoAction
    'g' -> do
      liftIO $ putOutput "Goto Stored Address (key):"
      keyChar <- liftIO getKey
      machine <- get
      case Map.lookup keyChar (storedAddresses machine) of
        Just addr -> do
          put (machine { mRegs = (mRegs machine) { rPC = addr } })
          liftIO $ putOutput $ "PC set to $" ++ showHex addr ""
          -- Re-disassemble at the new PC
          disassembleInstructions addr 1
          return NoAction
        Nothing -> do
          liftIO $ putOutput ("No address stored at key '" ++ [keyChar] ++ "'")
          return NoAction
    _ -> do
      liftIO $ putOutput "Invalid Stored Address command."
      return NoAction

-- | Handles key presses in VimMode.
handleVimKey :: Char -> FDX DebuggerAction
handleVimKey key = do
  -- liftIO $ putStrLn $ "Vim mode key pressed: " ++ [key]
  case key of
    'q' -> return QuitEmulator
    'x' -> return ExitDebugger
    'b' -> handleBreakVim
    'm' -> handleMemTraceVim
    'a' -> handleAddressVim
    'j' -> return (ExecuteStep [key])
    'z' -> return (ExecuteStep [key]) -- Treat 'z' as a step for now
    '\x1b' -> do
      liftIO $ putOutput "Switching to Command Mode."
      return SwitchToCommandMode
    _   -> return NoAction -- Placeholder for other Vim mode commands

-- | Helper function for the interactive debugger loop in VimMode.
interactiveLoopHelper :: DebuggerConsoleState -> FDX DebuggerAction
interactiveLoopHelper consoleState = do
  machine <- get
  if halted machine
    then return QuitEmulator -- Return QuitEmulator if machine is halted
    else do
      liftIO $ renderScreen machine consoleState -- Render the screen
      key <- liftIO getKey -- Read a single key
      action <- handleVimKey key -- Handle the key press

      -- Update console state based on the action and key press
      let newConsoleState = case action of
                              ContinueLoop _ -> consoleState -- ContinueLoop doesn't change console state directly here
                              ExecuteStep _  -> consoleState -- ExecuteStep doesn't change console state directly here
                              ExitDebugger   -> consoleState
                              QuitEmulator   -> consoleState
                              NoAction       -> consoleState
                              SwitchToCommandMode -> consoleState
                              SwitchToVimMode -> consoleState
      -- In Vim mode, we might want to add the key pressed to output for feedback, or clear output on certain keys.
      -- For now, let's just clear output lines on any key press to keep it simple and re-render.
      let updatedConsoleState = newConsoleState { outputLines = [], inputBuffer = "", cursorPosition = 0 } -- Clear console output on key press

      case action of
        ContinueLoop nextLastCommand -> interactiveLoopHelper (updatedConsoleState { lastCommand = nextLastCommand }) -- Continue the loop
        ExecuteStep  nextLastCommand -> return (ExecuteStep nextLastCommand) -- Return ExecuteStep to the main loop
        ExitDebugger                 -> return ExitDebugger -- Return ExitDebugger
        QuitEmulator                 -> return QuitEmulator -- Return QuitEmulator
        NoAction                     -> interactiveLoopHelper updatedConsoleState -- Continue the loop with the updated state
        SwitchToCommandMode          -> return SwitchToCommandMode -- Return SwitchToCommandMode
        SwitchToVimMode              -> interactiveLoopHelper updatedConsoleState -- Stay in VimMode
