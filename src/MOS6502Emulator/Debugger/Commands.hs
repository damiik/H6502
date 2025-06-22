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
import Control.Monad.State (put, get, modify)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word16, Word8)
import Data.List (cycle, delete, sort)
import Control.Lens

import MOS6502Emulator.Core (FDX, fetchByteMem, _halted, _enableTrace, _breakpoints, _memoryTraceBlocks, _debugLogPath, _traceMemoryStart, _traceMemoryEnd, _lastDisassembledAddr) -- Explicitly import what's needed from Core
import MOS6502Emulator.Machine (Machine(..), setPC_) -- Import Machine type
import MOS6502Emulator.DissAssembler (disassembleInstructions, formatHex8, formatHex16)
import MOS6502Emulator.Memory (writeByte)
import MOS6502Emulator.Debugger.Console (renderScreen, getInput, putOutput, putString, getKey, termHeight)
import MOS6502Emulator.Debugger.Core (DebuggerCommand(..),  DebuggerAction(..))
import MOS6502Emulator.Debugger.VimMode.Core (vimModeHelp)
import MOS6502Emulator.Debugger.Actions (executeStepAndRender, logRegisters, logMemoryRange) -- Import the new unified step function and logging functions

import MOS6502Emulator.Debugger.Utils (parseHexWord, parseHexByte, parseDebuggerCommand) -- Import from Debugger.Utils
import MOS6502Emulator.Lenses

-- | Handles breakpoint commands in the debugger.
handleBreak :: [String] -> String -> FDX (DebuggerAction, [String])
handleBreak args lastCommand = do
  case args of
    [] -> do -- List breakpoints
      currentBreakpoints <- use breakpoints
      let output = "Current breakpoints:" : map (\bp -> "  $" ++ showHex bp "") currentBreakpoints
      return (ContinueLoop lastCommand, output)
    [addrStr] -> do -- Add or remove breakpoint
      case parseHexWord addrStr of
        Just addr -> do
          isSet <- use (breakpoints . to (elem addr))
          if isSet
            then do
              breakpoints %= delete addr -- Remove the breakpoint  
              let output = ["Breakpoint removed at $" ++ showHex addr ""]
              return (ContinueLoop lastCommand, output)
            else do
              breakpoints %= sort . (addr:)  -- Update the breakpoints in the machine state
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
  case args of
    [] -> do -- List memory trace blocks
      currentMemoryTraceBlocks <- use memoryTraceBlocks
      let output = "Current memory trace blocks:" : map (\(start, end, name) ->
                                                            "  $" ++ showHex start "" ++ " - $" ++ showHex end "" ++
                                                            case name of
                                                                Just n -> " (" ++ n ++ ")"
                                                                Nothing -> "")
                                                         currentMemoryTraceBlocks
      return (ContinueLoop lastCommand, output)
    [startAddrStr, endAddrStr] -> do -- Add or remove memory trace block without name
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) -> do
          currentBlocks <- use memoryTraceBlocks
          let newBlock = (startAddr, endAddr, Nothing)
          if newBlock `elem` currentBlocks
            then do
              memoryTraceBlocks %= filter (/= newBlock)
              let output = ["Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
              return (ContinueLoop lastCommand, output)
            else do
              memoryTraceBlocks %= (newBlock :)
              let output = ["Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
              return (ContinueLoop lastCommand, output)
        _ -> do
          let output = ["Invalid address format for memory trace block. Use hex (e.g., mem 0x0200 0x0300)."]
          return (ContinueLoop lastCommand, output)
    startAddrStr:endAddrStr:nameWords -> do -- Add or remove memory trace block with name
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) -> do
          currentBlocks <- use memoryTraceBlocks
          let name = unwords nameWords
          let newBlock = (startAddr, endAddr, Just name)
          if newBlock `elem` currentBlocks
            then do
              memoryTraceBlocks %= filter (/= newBlock)
              let output = ["Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"]
              return (ContinueLoop lastCommand, output)
            else do
              memoryTraceBlocks %= (newBlock :)
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
              let mem = _mMem machine
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
handleSetReg8 :: ASetter' Machine Word8 -> String -> String -> String -> FDX (DebuggerAction, [String])
handleSetReg8 regLens valStr regName lastCommand = do
    case parseHexByte valStr of
        Just val -> do
            regLens .= val
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
    startAddr' <- use lastDisassembledAddr
    let startAddr = case args of
                      [addrStr] -> case parseHexWord addrStr of
                                     Just addr -> addr
                                     Nothing -> startAddr' -- Use last disassembled address on invalid input
                      _         -> startAddr' -- Use last disassembled address if no argument
    (disassembledOutput, finalAddr) <- disassembleInstructions startAddr 32
    lastDisassembledAddr .= finalAddr -- Update last disassembled address
    let output = ("Disassembling 32 instructions starting at $" ++ showHex startAddr "") : disassembledOutput
    return (ContinueLoop lastCommand, output)

-- | Handles commands in CommandMode.
handleCommand :: String -> FDX (DebuggerAction, [String])
handleCommand commandToExecute = do
  let parsedCommand = parseDebuggerCommand commandToExecute
  let handleStep :: FDX (DebuggerAction, [String])
      handleStep = do
        memOutput <- mapM (\(start, end, name) -> logMemoryRange start end name) =<< use memoryTraceBlocks
        let output = concat memOutput
        return (ExecuteStep "", output) -- Return ExecuteStep to signal rendering is handled
  let handleRegs :: FDX (DebuggerAction, [String])
      handleRegs = do
        regs <- use mRegs
        return (NoAction, logRegisters regs)
  let handleTrace :: FDX (DebuggerAction, [String])
      handleTrace = do
        enableTrace %= not
        newTraceState <- use enableTrace
        let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
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
        mConsoleState . helpLines .= fullHelpText
        mConsoleState . helpScrollPos .= 0
        putOutput "Displaying help. Press Enter to scroll."
        return (NoAction, [])
  let handleAddrRange :: Word16 -> Word16 -> FDX (DebuggerAction, [String])
      handleAddrRange startAddr endAddr = do
        traceMemoryStart .= startAddr
        traceMemoryEnd .= endAddr
        let output = ["Memory trace range set to $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
        return (NoAction, output)
  let handleLog :: FDX (DebuggerAction, [String])
      handleLog = do
        outputLinesFromLog <- mapM (\(start, end, name) -> logMemoryRange start end name) =<< use memoryTraceBlocks
        let outputLinesFromLog' = concat outputLinesFromLog
        return (NoAction, "Memory trace blocks:" : outputLinesFromLog')
  let handleVimMode :: FDX (DebuggerAction, [String])
      handleVimMode = return (SwitchToVimMode, [])
  case parsedCommand of
    Step -> handleStep
    Break addr -> do
      case addr of
        Nothing -> do
          currentBreakpoints <- use breakpoints
          let output = "Current breakpoints:" : map (\bp -> "  $" ++ showHex bp "") currentBreakpoints
          return (ContinueLoop commandToExecute, output)
        Just bpAddr -> do
          isSet <- use (breakpoints . to (elem bpAddr))
          if isSet
            then do
              breakpoints %= delete bpAddr -- Remove the breakpoint
              let output = ["Breakpoint removed at $" ++ showHex bpAddr ""]
              return (ContinueLoop commandToExecute, output)
            else do
              breakpoints %= sort . (bpAddr:) -- Add the breakpoint
              let output = ["Breakpoint added at $" ++ showHex bpAddr ""]
              return (ContinueLoop commandToExecute, output)
    MemTrace traceInfo -> do
      case traceInfo of
        Nothing -> do
          currentMemoryTraceBlocks <- use memoryTraceBlocks
          let output = "Current memory trace blocks:" : map (\(start, end, name) ->
                                                                "  $" ++ showHex start "" ++ " - $" ++ showHex end "" ++
                                                                case name of
                                                                    Just n -> " (" ++ n ++ ")"
                                                                    Nothing -> "")
                                                             currentMemoryTraceBlocks
          return (ContinueLoop commandToExecute, output)
        Just (startAddr, endAddr, maybeName) -> do
          currentBlocks <- use memoryTraceBlocks
          let newBlock = (startAddr, endAddr, maybeName)
          if newBlock `elem` currentBlocks
            then do
              memoryTraceBlocks %= filter (/= newBlock)
              let output = ["Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ maybe "" (\n -> " (" ++ n ++ ")") maybeName]
              return (ContinueLoop commandToExecute, output)
            else do
              memoryTraceBlocks %= (newBlock :)
              let output = ["Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ maybe "" (\n -> " (" ++ n ++ ")") maybeName]
              return (ContinueLoop commandToExecute, output)
    Fill startAddr endAddr bytes -> do
      if null bytes
        then do
          let output = ["No valid byte values provided or parse error."]
          return (ContinueLoop commandToExecute, output)
        else if startAddr > endAddr
        then do
          let output = ["Start address cannot be greater than end address."]
          return (ContinueLoop commandToExecute, output)
        else do
          let addressRange = [startAddr .. endAddr]
          let fillBytes = take (length addressRange) (cycle bytes)
          machine <- get
          let mem = _mMem machine
          liftIO $ mapM_ (\(addr, val) -> writeByte addr val mem) (zip addressRange fillBytes)
          let output = ["Memory filled from $" ++ showHex startAddr "" ++ " to $" ++ showHex endAddr ""]
          return (ContinueLoop commandToExecute, output)
    SetReg8 regName val -> do
      let regLens = case regName of
                      "Accumulator" -> mRegs . rAC
                      "X Register" -> mRegs . rX
                      "Y Register" -> mRegs . rY
                      "Stack Pointer" -> mRegs . rSP
                      "Status Register" -> mRegs . rSR
                      _ -> error "Invalid register name"
      regLens .= val
      let output = [regName ++ " set to $" ++ showHex val ""]
      return (ContinueLoop commandToExecute, output)
    SetPC val -> do
      setPC_ val
      let output = ["PC set to $" ++ showHex val ""]
      return (ContinueLoop commandToExecute, output)
    Disassemble maybeAddr -> do
      startAddr' <- use lastDisassembledAddr
      let startAddr = case maybeAddr of
                        Just addr -> addr
                        Nothing -> startAddr'
      (disassembledOutput, finalAddr) <- disassembleInstructions startAddr 32
      lastDisassembledAddr .= finalAddr
      let output = ("Disassembling 32 instructions starting at $" ++ showHex startAddr "") : disassembledOutput
      return (ContinueLoop commandToExecute, output)
    Regs -> handleRegs
    Trace -> handleTrace
    Goto addr -> do
      mRegs . rPC .= addr
      let output = ["PC set to $" ++ showHex addr ""]
      return (NoAction, output)
    Quit -> do
      halted .= True
      return (QuitEmulator, [])
    Exit -> do
      debuggerActive .= False
      let output = ["Exiting debugger. Continuing execution."]
      return (ExitDebugger, output)
    Unknown cmdStr -> do
      case words cmdStr of
        ["help"] -> handleHelp
        ["h"] -> handleHelp
        ["log"] -> handleLog
        ["addr-range", startAddrStr, endAddrStr] -> do
          case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
            (Just startAddr, Just endAddr) -> handleAddrRange startAddr endAddr
            _ -> do
              let output = ["Invalid address format. Use hex (e.g., addr-range 0200 0300)."]
              return (NoAction, output)
        ["v"] -> handleVimMode
        _ -> do
          let output = ["Invalid command."]
          return (NoAction, output)
