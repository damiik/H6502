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
import MOS6502Emulator.Machine (Machine(), DebuggerMode(..), FDX, getMemory, fetchByteMem, debugLogPath, breakpoints, memoryTraceBlocks, mRegs, storedAddresses, labelMap, lastDisassembledAddr, mMem, enableTrace, debuggerActive, halted, traceMemoryStart, traceMemoryEnd, debuggerMode, DebuggerAction(..), getRegisters)
import MOS6502Emulator.Machine (writeByteMem, setPC_)
import MOS6502Emulator.DissAssembler (disassembleInstruction)
import Control.Monad.State (put, get, modify)
import MOS6502Emulator.Registers
import MOS6502Emulator.Memory (Memory(), writeByte)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, hReady, getChar)
import Data.List (stripPrefix, cycle, take)
import Data.Maybe (mapMaybe, listToMaybe, isNothing)
import Data.Bits (Bits, (.&.))
import qualified Data.Map.Strict as Map
import System.IO.Error (isEOFError)
import Control.Exception (catch)
import MOS6502Emulator.Debugger (handleBreak, handleMemTrace, parseHexWord) -- Import necessary functions from Debugger
import MOS6502Emulator.DissAssembler (disassembleInstructions) -- Import disassembleInstructions from DissAssembler

-- | Reads a single character from stdin without buffering.
getKey :: IO Char
getKey = catch
  (do
    hSetBuffering stdin NoBuffering
    c <- getChar
    return c
  )
  (\e -> if isEOFError e then return '\n' else ioError e) -- Handle EOF (e.g., Ctrl+D)

-- | Handles 'm' sub-commands in VimMode.
handleMemTraceVim :: FDX DebuggerAction
handleMemTraceVim = do
  liftIO $ putStrLn "\nMemory Trace (a: add, d: delete):"
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStr "Add Memory Trace Block (start end [name]): " >> hFlush stdout
      input <- liftIO getLine
      let args = words input
      handleMemTrace args "" -- handleMemTrace will perform the action and return a DebuggerAction
    'd' -> do
      liftIO $ putStr "Delete Memory Trace Block (start end [name]): " >> hFlush stdout
      input <- liftIO getLine
      let args = words input
      handleMemTrace args "" -- handleMemTrace will perform the action and return a DebuggerAction
    _ -> do
      liftIO $ putStrLn "Invalid Memory Trace command. Press 'm' to view blocks."
      return NoAction

-- | Handles 'b' sub-commands in VimMode.
handleBreakVim :: FDX DebuggerAction
handleBreakVim = do
  liftIO $ putStrLn "\nBreakpoints (a: add, d: delete):"
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStrLn "Add Breakpoint (address):"
      input <- liftIO getLine
      let args = words input
      handleBreak args "" -- handleBreak will perform the action and return a DebuggerAction
    'd' -> do
      liftIO $ putStrLn "Delete Breakpoint (address):"
      input <- liftIO getLine
      let args = words input
      handleBreak args "" -- handleBreak will perform the action and return a DebuggerAction
    _ -> do
      liftIO $ putStrLn "Invalid Breakpoint command."
      return NoAction

-- | Handles 'a' sub-commands in VimMode (Stored Addresses).
handleAddressVim :: FDX DebuggerAction
handleAddressVim = do
  liftIO $ putStrLn "\nStored Addresses (s: store, g: goto):"
  key <- liftIO getKey
  case key of
    's' -> do
      liftIO $ putStrLn "Store Current PC (key):"
      keyChar <- liftIO getKey
      machine <- get
      let currentPC = rPC (mRegs machine)
      modify (\m -> m { storedAddresses = Map.insert keyChar currentPC (storedAddresses m) })
      liftIO $ putStrLn $ "Stored PC $" ++ showHex currentPC "" ++ " at key '" ++ [keyChar] ++ "'"
      return NoAction
    'g' -> do
      liftIO $ putStrLn "Goto Stored Address (key):"
      keyChar <- liftIO getKey
      machine <- get
      case Map.lookup keyChar (storedAddresses machine) of
        Just addr -> do
          put (machine { mRegs = (mRegs machine) { rPC = addr } })
          liftIO $ putStrLn $ "PC set to $" ++ showHex addr ""
          -- Re-disassemble at the new PC
          disassembleInstructions addr 1
          return NoAction
        Nothing -> do
          liftIO $ putStrLn ("No address stored at key '" ++ [keyChar] ++ "'")
          return NoAction
    _ -> do
      liftIO $ putStrLn "Invalid Stored Address command."
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
      liftIO $ putStrLn "Switching to Command Mode."
      return SwitchToCommandMode
    _   -> return NoAction -- Placeholder for other Vim mode commands

-- | Helper function for the interactive debugger loop in VimMode.
interactiveLoopHelper :: String -> FDX DebuggerAction
interactiveLoopHelper lastCommand = do -- lastCommand is not used in Vim mode
  machine <- get
  if halted machine
    then return QuitEmulator -- Return QuitEmulator if machine is halted
    else do
      liftIO $ putStr "* " >> hFlush stdout
      key <- liftIO getKey
      action <- handleVimKey key
      case action of
        ContinueLoop nextLastCommand -> interactiveLoopHelper nextLastCommand -- Continue the loop
        ExecuteStep  nextLastCommand -> return (ExecuteStep nextLastCommand) -- Return ExecuteStep to the main loop
        ExitDebugger                 -> return ExitDebugger -- Return ExitDebugger
        QuitEmulator                 -> return QuitEmulator -- Return QuitEmulator
        NoAction                     -> interactiveLoopHelper lastCommand -- Continue the loop with the same last command
        SwitchToCommandMode          -> return SwitchToCommandMode -- Return SwitchToCommandMode
        SwitchToVimMode              -> interactiveLoopHelper lastCommand -- Stay in VimMode
