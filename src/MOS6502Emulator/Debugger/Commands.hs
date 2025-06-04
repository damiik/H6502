{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger.Commands
  ( handleBreak
  , handleMemTrace
  , handleFill
  , handleSetReg8
  , handleSetPC
  , handleDisassemble
  , handleCommand
  ) where

import Numeric (showHex, readHex)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Monad.State (put, get, modify)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word16, Word8)
import Data.List (stripPrefix, cycle, take)
import Data.Bits (Bits, (.&.), testBit)
import qualified Data.Map.Strict as Map
import System.IO.Error (isEOFError)
import Control.Exception (SomeException, IOException, displayException, try, catch)
import qualified Data.Map as Map
import Text.Printf (printf)
import qualified Data.Char as Char
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, hReady, readFile, writeFile, hSetEcho)

import MOS6502Emulator.Core (FDX, fetchByteMem, halted, enableTrace, breakpoints, memoryTraceBlocks, debugLogPath, traceMemoryStart, traceMemoryEnd, lastDisassembledAddr) -- Explicitly import what's needed from Core
import MOS6502Emulator.Machine (Machine(..)) -- Import Machine type
import MOS6502Emulator.DissAssembler (disassembleInstructions, formatHex8, formatHex16)
import MOS6502Emulator.Registers (Registers(..)) -- Import Registers type and its fields
import MOS6502Emulator.Memory (Memory(), writeByte)
import MOS6502Emulator.Debugger.Console (renderScreen, getInput, putOutput, putString, getKey, termHeight)
import MOS6502Emulator.Debugger.Core (DebuggerConsoleState(..), initialConsoleState, DebuggerAction(..), DebuggerMode(..))
import MOS6502Emulator.Debugger.VimMode.Core (vimModeHelp)
import System.Console.ANSI (clearScreen)
import qualified System.Console.ANSI as ANSI

import MOS6502Emulator.Debugger.Utils (parseHexWord, parseHexByte, setPC_, getRegisters, logMemoryRange, logRegisters) -- Import from Debugger.Utils

-- | Handles breakpoint commands in the debugger.
handleBreak :: [String] -> String -> FDX (DebuggerAction, [String])
handleBreak args lastCommand = do
  machine <- get
  case args of
    [] -> do -- List breakpoints
      let output = "Current breakpoints:" : map (\bp -> "  $" ++ showHex bp "") (breakpoints machine)
      return (ContinueLoop lastCommand, output)
    [addrStr] -> do -- Add or remove breakpoint
      case parseHexWord addrStr of
        Just addr -> do
          let currentBreakpoints = breakpoints machine
          if addr `elem` currentBreakpoints
            then do
              let newBreakpoints = filter (/= addr) currentBreakpoints
              put (machine { breakpoints = newBreakpoints })
              let output = ["Breakpoint removed at $" ++ showHex addr ""]
              return (ContinueLoop lastCommand, output)
            else do
              let newBreakpoints = addr : currentBreakpoints
              put (machine { breakpoints = newBreakpoints })
              let output = ["Breakpoint added at $" ++ showHex addr ""]
              return (ContinueLoop lastCommand, output)
        Nothing -> do
          let output = ["Invalid address format for breakpoint. Use hex (e.g., bk 0x0400)."]
          return (ContinueLoop lastCommand, output)
    _ -> do -- Too many arguments
      let output = ["Invalid use of breakpoint command. Use 'bk' or 'break' to list, or 'bk <address>' to add/remove."]
      return (ContinueLoop lastCommand, output)

-- | Handles memory trace commands in the debugger.
handleMemTrace :: [String] -> String -> FDX (DebuggerAction, [String])
handleMemTrace args lastCommand = do
  machine <- get
  case args of
    [] -> do -- List memory trace blocks
      let output = "Current memory trace blocks:" : map (\(start, end, name) ->
                                                            "  $" ++ showHex start "" ++ " - $" ++ showHex end "" ++
                                                            case name of
                                                                Just n -> " (" ++ n ++ ")"
                                                                Nothing -> "")
                                                         (memoryTraceBlocks machine)
      return (ContinueLoop lastCommand, output)
    [startAddrStr, endAddrStr] -> do -- Add or remove memory trace block without name
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) -> do
          let currentBlocks = memoryTraceBlocks machine
          let newBlock = (startAddr, endAddr, Nothing)
          if newBlock `elem` currentBlocks
            then do
              let newBlocks = filter (/= newBlock) currentBlocks
              put (machine { memoryTraceBlocks = newBlocks })
              let newBlocks = filter (/= newBlock) currentBlocks
              put (machine { memoryTraceBlocks = newBlocks })
              let output = ["Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
              return (ContinueLoop lastCommand, output)
            else do
              let newBlocks = newBlock : currentBlocks
              put (machine { memoryTraceBlocks = newBlocks })
              let output = ["Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
              return (ContinueLoop lastCommand, output)
        _ -> do
          let output = ["Invalid address format for memory trace block. Use hex (e.g., mem 0x0200 0x0300)."]
          return (ContinueLoop lastCommand, output)
    startAddrStr:endAddrStr:nameWords -> do -- Add or remove memory trace block with name
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) -> do
          let currentBlocks = memoryTraceBlocks machine
          let name = unwords nameWords
          let newBlock = (startAddr, endAddr, Just name)
          if newBlock `elem` currentBlocks
            then do
              let newBlocks = filter (/= newBlock) currentBlocks
              put (machine { memoryTraceBlocks = newBlocks })
              let output = ["Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"]
              return (ContinueLoop lastCommand, output)
            else do
              let newBlocks = newBlock : currentBlocks
              put (machine { memoryTraceBlocks = newBlocks })
              let output = ["Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"]
              return (ContinueLoop lastCommand, output)
        _ -> do
          let output = ["Invalid address format for memory trace block. Use hex (e.g., mem 0x0200 0x0300 MyRegion)."]
          return (ContinueLoop lastCommand, output)
    _ -> do -- Incorrect number of arguments
      let output = ["Invalid use of memory trace command. Use 'mem' or 'm' to list, 'mem <start> <end>' to add/remove without name, or 'mem <start> <end> <name>' to add/remove with name."]
      return (ContinueLoop lastCommand, output)

-- | Handles the fill memory command in the debugger.
handleFill :: [String] -> String -> FDX (DebuggerAction, [String])
handleFill args lastCommand = do
  case args of
    startAddrStr : endAddrStr : byteStrs -> do
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) -> do
          let byteValues = mapMaybe parseHexByte byteStrs
          if null byteValues
            then do
              let output = ["No valid byte values provided or parse error."]
              return (ContinueLoop lastCommand, output)
            else if startAddr > endAddr
            then do
              let output = ["Start address cannot be greater than end address."]
              return (ContinueLoop lastCommand, output)
            else do
              let addressRange = [startAddr .. endAddr]
              let fillBytes = take (length addressRange) (Data.List.cycle byteValues)
              machine <- get
              let mem = mMem machine
              -- Perform writes within FDX using liftIO
              liftIO $ mapM_ (\(addr, val) -> writeByte addr val mem) (zip addressRange fillBytes)
              -- Memory is modified in-place, no need to 'put' the machine state back just for this
              let output = ["Memory filled from $" ++ showHex startAddr "" ++ " to $" ++ showHex endAddr ""]
              return (ContinueLoop lastCommand, output)
        _ -> do
          let output = ["Invalid address format for fill command. Use hex (e.g., fill 0200 0300 ff 00)."]
          return (ContinueLoop lastCommand, output)
    _ -> do
      let output = ["Invalid use of fill command. Use 'fill <start> <end> <byte1> [byte2...]"]
      return (ContinueLoop lastCommand, output)

-- | Generic handler for setting 8-bit registers in the debugger.
handleSetReg8 :: (Registers -> Word8 -> Registers) -> String -> String -> String -> FDX (DebuggerAction, [String])
handleSetReg8 regSetter valStr regName lastCommand = do
    case parseHexByte valStr of
        Just val -> do
            modify (\m -> m { mRegs = regSetter (mRegs m) val })
            let output = [regName ++ " set to $" ++ showHex val ""]
            return (ContinueLoop lastCommand, output)
        Nothing -> do
            let output = ["Invalid hex value for " ++ regName ++ "."]
            return (ContinueLoop lastCommand, output)

-- | Specific handler for setting the 16-bit PC in the debugger.
handleSetPC :: String -> String -> FDX (DebuggerAction, [String])
handleSetPC valStr lastCommand = do
    case parseHexWord valStr of
        Just val -> do
            setPC_ val -- Assuming setPC updates the state directly
            let output = ["PC set to $" ++ showHex val ""]
            return (ContinueLoop lastCommand, output)
        Nothing -> do
            let output = ["Invalid hex value for PC."]
            return (ContinueLoop lastCommand, output)

-- | Handler for disassembling instructions in the debugger.
handleDisassemble :: [String] -> String -> FDX (DebuggerAction, [String])
handleDisassemble args lastCommand = do
    machine <- get
    let startAddr = case args of
                      [addrStr] -> case parseHexWord addrStr of
                                     Just addr -> addr
                                     Nothing -> lastDisassembledAddr machine -- Use last disassembled address on invalid input
                      _         -> lastDisassembledAddr machine -- Use last disassembled address if no argument
    (disassembledOutput, finalAddr) <- disassembleInstructions startAddr 32
    modify (\m -> m { lastDisassembledAddr = finalAddr }) -- Update last disassembled address
    let output = ("Disassembling 32 instructions starting at $" ++ showHex startAddr "") : disassembledOutput
    return (ContinueLoop lastCommand, output)

-- | Handles commands in CommandMode.
handleCommand :: String -> FDX (DebuggerAction, [String])
handleCommand commandToExecute = do
  machine <- get
  let handleStep :: FDX (DebuggerAction, [String])
      handleStep = return (ExecuteStep commandToExecute, [])
  let handleRegs :: FDX (DebuggerAction, [String])
      handleRegs = do
        regs <- getRegisters
        return (NoAction, logRegisters regs)

  let handleTrace :: FDX (DebuggerAction, [String])
      handleTrace = do
        let newTraceState = not (enableTrace machine)
        put (machine { enableTrace = newTraceState })
        let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
        return (NoAction, output)

  let handleGoto :: String -> FDX (DebuggerAction, [String])
      handleGoto addrStr = do
        currentMachine <- get
        if null addrStr then do
          let output = ["Address required for goto command."]
          return (NoAction, output)
        else case parseHexWord addrStr of
          Just addr -> do
            put (currentMachine { mRegs = (mRegs currentMachine) { rPC = addr } })
            let output = ["PC set to $" ++ showHex addr ""]
            return (NoAction, output)
          Nothing -> do
            let output = ["Invalid address format."]
            return (NoAction, output)

  let handleHelp :: FDX (DebuggerAction, [String])
      handleHelp = do
        let standardCommands =
              [ "Available commands:"
              , "  step  / z:              execute one instruction cycle"
              , "  regs  / r:              show current register values"
              , "  mem   / m [addr] [end]: add/remove memory range to display"
              , "  break / bk:             add/remove breakpoint to the list"
              , "  quit  / q:              quit program"
              , "  exit  / e:              exit interactive mode"
              , "  trace / t:              toggle instruction tracing"
              , "  goto  / g <addr>:       set program counter to address"
              , "  fill  / f <start> <end> <byte1> [byte2...]: fill memory range with bytes"
              , "  ra <val>:              set Accumulator to hex value"
              , "  rx <val>:              set X register to hex value"
              , "  ry <val>:              set Y register to hex value"
              , "  rsp <val>:             set Stack Pointer to hex value"
              , "  rsr <val>:             set Status Register to hex value"
              , "  rpc <val>:             set Program Counter to hex value"
              , "  d:                     disassemble 32 instructions from current PC"
              ]
        let fullHelpText = standardCommands ++ [""] ++ lines vimModeHelp
        modify (\m -> m { mConsoleState = (mConsoleState m) { helpLines = fullHelpText, helpScrollPos = 0 } })
        putOutput "Displaying help. Press Enter to scroll."
        return (NoAction, [])

  case words commandToExecute of
    ["help"] -> handleHelp
    ["h"] -> handleHelp
    ["goto", addrStr] -> handleGoto addrStr
    ["g", addrStr] -> handleGoto addrStr
    "fill":args -> handleFill args commandToExecute
    "f":args -> handleFill args commandToExecute -- Alias for fill
    ["step"] -> handleStep
    ["z"] -> handleStep
    ["regs"] -> handleRegs
    ["r"] -> handleRegs
    "mem":args -> handleMemTrace args commandToExecute
    "m":args -> handleMemTrace args commandToExecute -- Alias for mem
    ["log"] -> do
      outputLinesFromLog <- mapM (\(start, end, name) -> logMemoryRange start end name) (memoryTraceBlocks machine)
      let outputLinesFromLog' = concat outputLinesFromLog
      return (NoAction, "Memory trace blocks:" : outputLinesFromLog')
    ["x"] -> do
      modify (\m -> m { debuggerActive = False }) -- Exit debugger mode
      let output = ["Exiting debugger. Continuing execution."]
      return (ExitDebugger, output)
    "bk":args -> handleBreak args commandToExecute
    "break":args -> handleBreak args commandToExecute -- Alias for bk
    ["q"] -> return (QuitEmulator, []) -- Set halted flag
    ["quit"] -> return (QuitEmulator, []) -- Set halted flag
    ["trace"] -> handleTrace
    ["t"] -> handleTrace
    ["addr-range", startAddrStr, endAddrStr] -> do
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) -> do
          put (machine { traceMemoryStart = startAddr, traceMemoryEnd = endAddr })
          let output = ["Memory trace range set to $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
          return (NoAction, output)
        _ -> do
          let output = ["Invalid address format. Use hex (e.g., addr-range 0200 0300)."]
          return (NoAction, output)
    -- Register setting commands
    ["ra", valStr] -> handleSetReg8 (\r val -> r { rAC = val }) valStr "Accumulator" commandToExecute
    ["rx", valStr] -> handleSetReg8 (\r val -> r { rX = val }) valStr "X Register" commandToExecute
    ["ry", valStr] -> handleSetReg8 (\r val -> r { rY = val }) valStr "Y Register" commandToExecute
    ["rsp", valStr] -> handleSetReg8 (\r val -> r { rSP = val }) valStr "Stack Pointer" commandToExecute
    ["rsr", valStr] -> handleSetReg8 (\r val -> r { rSR = val }) valStr "Status Register" commandToExecute
    ["rpc", valStr] -> handleSetPC valStr commandToExecute
    "d":args -> handleDisassemble args commandToExecute
    ["v"] -> return (SwitchToVimMode, [])
    _      -> do
      let output = ["Invalid command."]
      return (NoAction, output)
