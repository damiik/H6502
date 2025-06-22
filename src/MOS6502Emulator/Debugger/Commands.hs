{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger.Commands
  ( handleCommandPure
  ) where

import Numeric (showHex)
import Data.Maybe (mapMaybe)
import Data.Word (Word16, Word8)
import Data.List (cycle, delete, sort)
import Control.Lens

import MOS6502Emulator.Core (fetchByteMemPure, writeByteMemPure, Machine(..))
import MOS6502Emulator.Machine (setPC_)
import MOS6502Emulator.DissAssembler (disassembleInstructionsPure, formatHex8, formatHex16)
import MOS6502Emulator.Debugger.Core (DebuggerCommand(..), DebuggerAction(..), DebuggerConsoleState(..))
import MOS6502Emulator.Debugger.VimMode.Core (vimModeHelp)
import MOS6502Emulator.Debugger.Actions (logRegisters, logMemoryRangePure)

import MOS6502Emulator.Debugger.Utils (parseHexWord, parseHexByte, parseDebuggerCommand)
import MOS6502Emulator.Lenses

-- | Handles breakpoint commands in the debugger (pure version).
handleBreakPure :: Machine -> [String] -> (Machine, [String], DebuggerAction)
handleBreakPure machine args =
  case args of
    [] -> -- List breakpoints
      let currentBreakpoints = view breakpoints machine
          output = "Current breakpoints:" : map (\bp -> "  $" ++ showHex bp "") currentBreakpoints
      in (machine, output, ContinueLoop "")
    [addrStr] -> -- Add or remove breakpoint
      case parseHexWord addrStr of
        Just addr ->
          let isSet = elem addr (view breakpoints machine)
          in if isSet
            then -- Remove the breakpoint
              let newMachine = machine & breakpoints %~ delete addr
                  output = ["Breakpoint removed at $" ++ showHex addr ""]
              in (newMachine, output, ContinueLoop "")
            else -- Add the breakpoint
              let newMachine = machine & breakpoints %~ sort . (addr:)
                  output = ["Breakpoint added at $" ++ showHex addr ""]
              in (newMachine, output, ContinueLoop "")
        Nothing ->
          let output = ["Invalid address format for breakpoint. Use hex (e.g., bk 0x0400)."]
          in (machine, output, ContinueLoop "")
    _ -> -- Too many arguments
      let output = ["Invalid use of breakpoint command. Use 'bk' or 'break' to list, or 'bk <address>' to add/remove."]
      in (machine, output, ContinueLoop "")

-- | Handles memory trace commands in the debugger (pure version).
handleMemTracePure :: Machine -> [String] -> (Machine, [String], DebuggerAction)
handleMemTracePure machine args =
  case args of
    [] -> -- List memory trace blocks
      let currentMemoryTraceBlocks = view memoryTraceBlocks machine
          output = "Current memory trace blocks:" : map (\(start, end, name) ->
                                                            "  $" ++ showHex start "" ++ " - $" ++ showHex end "" ++
                                                            case name of
                                                                Just n -> " (" ++ n ++ ")"
                                                                Nothing -> "")
                                                         currentMemoryTraceBlocks
      in (machine, output, ContinueLoop "")
    [startAddrStr, endAddrStr] -> -- Add or remove memory trace block without name
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) ->
          let currentBlocks = view memoryTraceBlocks machine
              newBlock = (startAddr, endAddr, Nothing)
          in if newBlock `elem` currentBlocks
            then
              let newMachine = machine & memoryTraceBlocks %~ filter (/= newBlock)
                  output = ["Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
              in (newMachine, output, ContinueLoop "")
            else
              let newMachine = machine & memoryTraceBlocks %~ (newBlock :)
                  output = ["Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
              in (newMachine, output, ContinueLoop "")
        _ ->
          let output = ["Invalid address format for memory trace block. Use hex (e.g., mem 0x0200 0x0300)."]
          in (machine, output, ContinueLoop "")
    startAddrStr:endAddrStr:nameWords -> -- Add or remove memory trace block with name
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) ->
          let currentBlocks = view memoryTraceBlocks machine
              name = unwords nameWords
              newBlock = (startAddr, endAddr, Just name)
          in if newBlock `elem` currentBlocks
            then
              let newMachine = machine & memoryTraceBlocks %~ filter (/= newBlock)
                  output = ["Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"]
              in (newMachine, output, ContinueLoop "")
            else
              let newMachine = machine & memoryTraceBlocks %~ (newBlock :)
                  output = ["Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"]
              in (newMachine, output, ContinueLoop "")
        _ ->
          let output = ["Invalid address format for memory trace block. Use hex (e.g., mem 0x0200 0x0300 MyRegion)."]
          in (machine, output, ContinueLoop "")
    _ -> -- Incorrect number of arguments
      let output = ["Invalid use of memory trace command. Use 'mem' or 'm' to list, 'mem <start> <end>' to add/remove without name, or 'mem <start> <end> <name>' to add/remove with name."]
      in (machine, output, ContinueLoop "")

-- | Handles the fill memory command in the debugger (pure version).
handleFillPure :: Machine -> [String] -> (Machine, [String], DebuggerAction)
handleFillPure machine args =
  case args of
    startAddrStr : endAddrStr : byteStrs ->
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) ->
          let byteValues = mapMaybe parseHexByte byteStrs
          in if null byteValues
            then
              let output = ["No valid byte values provided or parse error."]
              in (machine, output, ContinueLoop "")
            else if startAddr > endAddr
            then
              let output = ["Start address cannot be greater than end address."]
              in (machine, output, ContinueLoop "")
            else
              let addressRange = [startAddr .. endAddr]
                  fillBytes = take (length addressRange) (Data.List.cycle byteValues)
                  newMachine = foldl (\m (addr, val) -> writeByteMemPure addr val m) machine (zip addressRange fillBytes)
                  output = ["Memory filled from $" ++ showHex startAddr "" ++ " to $" ++ showHex endAddr ""]
              in (newMachine, output, ContinueLoop "")
        _ ->
          let output = ["Invalid address format for fill command. Use hex (e.g., fill 0200 0300 ff 00)."]
          in (machine, output, ContinueLoop "")
    _ ->
      let output = ["Invalid use of fill command. Use 'fill <start> <end> <byte1> [byte2...]"]
      in (machine, output, ContinueLoop "")

-- | Generic handler for setting 8-bit registers in the debugger (pure version).
handleSetReg8Pure :: ASetter' Machine Word8 -> String -> String -> Machine -> (Machine, [String], DebuggerAction)
handleSetReg8Pure regLens valStr regName machine =
    case parseHexByte valStr of
        Just val ->
            let newMachine = machine & regLens .~ val
                output = [regName ++ " set to $" ++ showHex val ""]
            in (newMachine, output, ContinueLoop "")
        Nothing ->
            let output = ["Invalid hex value for " ++ regName ++ "."]
            in (machine, output, ContinueLoop "")

-- | Specific handler for setting the 16-bit PC in the debugger (pure version).
handleSetPCPure :: String -> Machine -> (Machine, [String], DebuggerAction)
handleSetPCPure valStr machine =
    case parseHexWord valStr of
        Just val ->
            let newMachine = setPC_ val machine -- Assuming setPC_ updates the state directly and is pure
                output = ["PC set to $" ++ showHex val ""]
            in (newMachine, output, ContinueLoop "")
        Nothing ->
            let output = ["Invalid hex value for PC."]
            in (machine, output, ContinueLoop "")

-- | Handler for disassembling instructions in the debugger (pure version).
handleDisassemblePure :: Machine -> [String] -> (Machine, [String], DebuggerAction)
handleDisassemblePure machine args =
    let startAddr' = view lastDisassembledAddr machine
        startAddr = case args of
                      [addrStr] -> case parseHexWord addrStr of
                                     Just addr -> addr
                                     Nothing -> startAddr' -- Use last disassembled address on invalid input
                      _         -> startAddr' -- Use last disassembled address if no argument
        (disassembledOutput, finalAddr) = disassembleInstructionsPure startAddr 32 machine
        newMachine = machine & lastDisassembledAddr .~ finalAddr -- Update last disassembled address
        output = ("Disassembling 32 instructions starting at $" ++ showHex startAddr "") : disassembledOutput
    in (newMachine, output, ContinueLoop "")

-- | Handles commands in CommandMode (pure version).
handleCommandPure :: Machine -> DebuggerCommand -> (Machine, [String], DebuggerAction)
handleCommandPure machine parsedCommand =
  let handleStepPure :: Machine -> (Machine, [String], DebuggerAction)
      handleStepPure m =
        let memOutput = map (\(start, end, name) -> logMemoryRangePure start end name m) (view memoryTraceBlocks m)
            output = concat memOutput
        in (m, output, ExecuteStep "") -- Return ExecuteStep to signal rendering is handled
      handleRegsPure :: Machine -> (Machine, [String], DebuggerAction)
      handleRegsPure m =
        let regs = view mRegs m
        in (m, logRegisters regs, NoAction)
      handleTracePure :: Machine -> (Machine, [String], DebuggerAction)
      handleTracePure m =
        let newTraceState = not (view enableTrace m)
            newMachine = m & enableTrace .~ newTraceState
            output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
        in (newMachine, output, NoAction)
      handleHelpPure :: Machine -> (Machine, [String], DebuggerAction)
      handleHelpPure m =
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
            fullHelpText = standardCommands ++ [""] ++ lines vimModeHelp
            newConsoleState = (view mConsoleState m) { _helpLines = fullHelpText, _helpScrollPos = 0 }
            newMachine = m & mConsoleState .~ newConsoleState
        in (newMachine, ["Displaying help. Press Enter to scroll."], NoAction)
      handleAddrRangePure :: Word16 -> Word16 -> Machine -> (Machine, [String], DebuggerAction)
      handleAddrRangePure startAddr endAddr m =
        let newMachine = m & traceMemoryStart .~ startAddr & traceMemoryEnd .~ endAddr
            output = ["Memory trace range set to $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""]
        in (newMachine, output, NoAction)
      handleLogPure :: Machine -> (Machine, [String], DebuggerAction)
      handleLogPure m =
        let outputLinesFromLog = map (\(start, end, name) -> logMemoryRangePure start end name m) (view memoryTraceBlocks m)
            outputLinesFromLog' = concat outputLinesFromLog
        in (m, "Memory trace blocks:" : outputLinesFromLog', NoAction)
      handleVimModePure :: Machine -> (Machine, [String], DebuggerAction)
      handleVimModePure m = (m, [], SwitchToVimMode)
  in case parsedCommand of
    Step -> handleStepPure machine
    Break addr ->
      case addr of
        Nothing ->
          let currentBreakpoints = view breakpoints machine
              output = "Current breakpoints:" : map (\bp -> "  $" ++ showHex bp "") currentBreakpoints
          in (machine, output, ContinueLoop "")
        Just bpAddr ->
          let isSet = elem bpAddr (view breakpoints machine)
          in if isSet
            then
              let newMachine = machine & breakpoints %~ delete bpAddr
                  output = ["Breakpoint removed at $" ++ showHex bpAddr ""]
              in (newMachine, output, ContinueLoop "")
            else
              let newMachine = machine & breakpoints %~ sort . (bpAddr:)
                  output = ["Breakpoint added at $" ++ showHex bpAddr ""]
              in (newMachine, output, ContinueLoop "")
    MemTrace traceInfo ->
      case traceInfo of
        Nothing ->
          let currentMemoryTraceBlocks = view memoryTraceBlocks machine
              output = "Current memory trace blocks:" : map (\(start, end, name) ->
                                                                "  $" ++ showHex start "" ++ " - $" ++ showHex end "" ++
                                                                case name of
                                                                    Just n -> " (" ++ n ++ ")"
                                                                    Nothing -> "")
                                                             currentMemoryTraceBlocks
          in (machine, output, ContinueLoop "")
        Just (startAddr, endAddr, maybeName) ->
          let currentBlocks = view memoryTraceBlocks machine
              newBlock = (startAddr, endAddr, maybeName)
          in if newBlock `elem` currentBlocks
            then
              let newMachine = machine & memoryTraceBlocks %~ filter (/= newBlock)
                  output = ["Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ maybe "" (\n -> " (" ++ n ++ ")") maybeName]
              in (newMachine, output, ContinueLoop "")
            else
              let newMachine = machine & memoryTraceBlocks %~ (newBlock :)
                  output = ["Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ maybe "" (\n -> " (" ++ n ++ ")") maybeName]
              in (newMachine, output, ContinueLoop "")
    Fill startAddr endAddr bytes ->
      if null bytes
        then (machine, ["No valid byte values provided or parse error."], ContinueLoop "")
        else if startAddr > endAddr
        then (machine, ["Start address cannot be greater than end address."], ContinueLoop "")
        else
          let addressRange = [startAddr .. endAddr]
              fillBytes = take (length addressRange) (cycle bytes)
              newMachine = foldl (\m (addr, val) -> writeByteMemPure addr val m) machine (zip addressRange fillBytes)
              output = ["Memory filled from $" ++ showHex startAddr "" ++ " to $" ++ showHex endAddr ""]
          in (newMachine, output, ContinueLoop "")
    SetReg8 regName val ->
      let regLens = case regName of
                      "Accumulator" -> mRegs . rAC
                      "X Register" -> mRegs . rX
                      "Y Register" -> mRegs . rY
                      "Stack Pointer" -> mRegs . rSP
                      "Status Register" -> mRegs . rSR
                      _ -> error "Invalid register name"
          newMachine = machine & regLens .~ val
          output = [regName ++ " set to $" ++ showHex val ""]
      in (newMachine, output, ContinueLoop "")
    SetPC val ->
      let newMachine = setPC_ val machine
          output = ["PC set to $" ++ showHex val ""]
      in (newMachine, output, ContinueLoop "")
    Disassemble maybeAddr ->
      let startAddr' = view lastDisassembledAddr machine
          startAddr = case maybeAddr of
                        Just addr -> addr
                        Nothing -> startAddr'
          (disassembledOutput, finalAddr) = disassembleInstructionsPure startAddr 32 machine
          newMachine = machine & lastDisassembledAddr .~ finalAddr
          output = ("Disassembling 32 instructions starting at $" ++ showHex startAddr "") : disassembledOutput
      in (newMachine, output, ContinueLoop "")
    Regs -> handleRegsPure machine
    Trace -> handleTracePure machine
    Goto addr ->
      let newMachine = machine & mRegs . rPC .~ addr
          output = ["PC set to $" ++ showHex addr ""]
      in (newMachine, output, NoAction)
    Quit ->
      let newMachine = machine & halted .~ True
      in (newMachine, [], QuitEmulator)
    Exit ->
      let newMachine = machine & debuggerActive .~ False
          output = ["Exiting debugger. Continuing execution."]
      in (newMachine, output, ExitDebugger)
    Unknown cmdStr ->
      case words cmdStr of
        ["help"] -> handleHelpPure machine
        ["h"] -> handleHelpPure machine
        ["log"] -> handleLogPure machine
        ["addr-range", startAddrStr, endAddrStr] ->
          case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
            (Just startAddr, Just endAddr) -> handleAddrRangePure startAddr endAddr machine
            _ -> (machine, ["Invalid address format. Use hex (e.g., addr-range 0200 0300)."], NoAction)
        ["v"] -> handleVimModePure machine
        _ -> (machine, ["Invalid command."], NoAction)
