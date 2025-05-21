{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger
  ( DebuggerAction(..) -- Export DebuggerAction
  , logRegisters -- Keep exporting for now, though its usage might change
  , logMemoryRange -- Keep exporting for now, though its usage might change
  , saveDebuggerState
  , loadDebuggerState
  , formatStatusFlags
  , handleBreak -- Exporting for use in MOS6502Emulator
  , handleMemTrace -- Exporting for use in MOS6502Emulator
  , handleCommand -- Exporting for use in MOS6502Emulator
  , handleFill -- Exporting for CommandMode
  , handleSetReg8 -- Exporting for CommandMode
  , handleSetPC -- Exporting for CommandMode
  , handleDisassemble -- Exporting for CommandMode
  , interactiveLoopHelper -- Exporting for CommandMode
  , parseHexByte -- Exporting for CommandMode
  , parseHexWord -- Exporting for CommandMode
  ) where

import Numeric (showHex, readHex)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Monad.State (put, get, modify)
import Data.Maybe (mapMaybe, listToMaybe, isNothing)
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

import MOS6502Emulator.Machine (writeByteMem, setPC_, Machine(), DebuggerMode(..), FDX, getMemory, fetchByteMem, debugLogPath, breakpoints, memoryTraceBlocks, mRegs, storedAddresses, labelMap, lastDisassembledAddr, mMem, enableTrace, debuggerActive, halted, traceMemoryStart, traceMemoryEnd, debuggerMode, DebuggerAction(..), getRegisters)
import MOS6502Emulator.DissAssembler (disassembleInstruction)
import MOS6502Emulator.Registers
import MOS6502Emulator.Memory (Memory(), writeByte)
import MOS6502Emulator.Debugger.Console (renderScreen, getInput, putOutput, getKey) -- Import console I/O functions

import System.Console.ANSI (clearScreen) -- Import clearScreen
import MOS6502Emulator.Debugger.Console (DebuggerConsoleState(..), initialConsoleState, renderScreen, getKey) -- Import DebuggerConsoleState, initialConsoleState, and getKey
import qualified System.Console.ANSI as ANSI -- Import qualified System.Console.ANSI

-- | Helper function for the interactive debugger loop, handling command input and execution.
interactiveLoopHelper :: DebuggerConsoleState -> FDX DebuggerAction
interactiveLoopHelper initialConsoleState = do
  machine <- get
  if halted machine
    then return QuitEmulator -- Machine halted, return QuitEmulator action
    else do
      liftIO $ renderScreen machine initialConsoleState -- Render the screen initially

      -- Temporarily enable echo for command input and use getLine
      liftIO $ hSetEcho stdin True
      commandToExecute <- liftIO getLine -- Read the full command input line
      -- Disable echo again
      liftIO $ hSetEcho stdin False

      (action, output) <- handleCommand commandToExecute -- Process the command

      -- Update console state after command execution
      -- Note: inputBuffer and cursorPosition are not used with getLine approach for main command input
      let newOutputLines = outputLines initialConsoleState ++ ["> " ++ commandToExecute] ++ output -- Add command and captured output
      let updatedConsoleState = initialConsoleState { outputLines = newOutputLines, inputBuffer = "", cursorPosition = 0, lastCommand = commandToExecute }

      liftIO ANSI.clearScreen -- Clear screen before next render

      case action of
        ContinueLoop _ -> interactiveLoopHelper updatedConsoleState -- Continue the loop with updated state
        ExecuteStep _  -> return action -- Return the ExecuteStep action
        ExitDebugger                 -> put (machine { debuggerActive = False }) >> return action
        QuitEmulator                 -> put (machine { halted = True }) >> return action
        NoAction                     -> interactiveLoopHelper updatedConsoleState -- Continue the loop with updated state
        SwitchToVimMode              -> modify (\m -> m { debuggerMode = VimMode }) >> return action
        SwitchToCommandMode          -> interactiveLoopHelper updatedConsoleState -- Stay in CommandMode and continue loop




-- | Formats a Word8 as a two-character hexadecimal string, padding with a leading zero if necessary.
formatHex8 :: Word8 -> String
formatHex8 b =
  let hexStr = showHex b ""
  in if length hexStr < 2 then '0' : hexStr else hexStr

-- | Formats a Word16 as a four-character hexadecimal string, padding with leading zeros if necessary.
formatHex16 :: Word16 -> String
formatHex16 w =
  let hexStr = showHex w ""
  in replicate (4 - length hexStr) '0' ++ hexStr

-- | Formats the status flags of the status register (SR) into a human-readable string.
formatStatusFlags :: Word8 -> String
formatStatusFlags sr =
  let getFlagBit r b = if testBit r b then '*' else ' '
  in [getFlagBit sr 7, 'N', ' ',
      getFlagBit sr 6, 'V', '-',
      getFlagBit sr 4, 'B', ' ',
      getFlagBit sr 3, 'D', ' ',
      getFlagBit sr 2, 'I', ' ',
      getFlagBit sr 1, 'Z', ' ',
      getFlagBit sr 0, 'C']

-- | Logs the current register values as a list of strings.
logRegisters :: Registers -> [String]
logRegisters reg =
  let sr = rSR reg
      formatBinary8 b = p1 ++ " " ++ p2
            where
              p1 =  [if testBit b i then '1' else '0' | i <- [7,6..4]]
              p2 =  [if testBit b i then '1' else '0' | i <- [3,2..0]]
  in [ "--------------------------------------------"
     , "\x1b[35m\x1b[1mPC\x1b[0m\x1b[35m: $" ++ (formatHex16 (rPC reg)) ++ "\x1b[0m"
     , "\x1b[33m\x1b[1mAC\x1b[0m\x1b[33m: $" ++ formatHex8 (rAC reg) ++ " [" ++ formatBinary8 (rAC reg) ++ "] ( " ++ show (rAC reg) ++ " )\x1b[0m"
     , "\x1b[32m\x1b[1m X\x1b[0m\x1b[32m: $" ++ formatHex8 (rX reg) ++ " [\x1b[32m" ++ formatBinary8 (rX reg) ++ "\x1b[32m] ( " ++ show (rX reg) ++ " )\x1b[0m"
     , "\x1b[32m\x1b[1m Y\x1b[0m\x1b[32m: $" ++ formatHex8 (rY reg) ++ " [\x1b[32m" ++ formatBinary8 (rY reg) ++ "\x1b[32m] ( " ++ show (rY reg) ++ " )\x1b[0m"
     , "\x1b[35m\x1b[1mSP\x1b[0m\x1b[35m: $" ++ (formatHex8 (rSP reg)) ++ "\x1b[0m"
     , "\x1b[35m\x1b[1mSR\x1b[0m\x1b[35m:     [" ++ formatBinary8 (rSR reg)  ++ "]  " ++ formatStatusFlags sr ++ "\n        *NV-B DIZC*\x1b[0m"
     ]

-- | Logs the memory range as a list of strings.
logMemoryRange :: Word16 -> Word16 -> Maybe String -> FDX [String]
logMemoryRange start end name = do
  bytes <- sequence [fetchByteMem addr | addr <- [start..end]]
  return [
    (case name of
        Just n -> n ++ " [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = "
        Nothing -> "MEM [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = ") ++
    unwords (map formatHex8 bytes)
    ]

-- | Saves the current debugger state (breakpoints and memory trace blocks) to a file.
saveDebuggerState :: Machine -> IO ()
saveDebuggerState machine = case debugLogPath machine of
    Just filePath -> liftIO $ do
        let breakpointsStr = "breakpoints: " ++ unwords [printf "%04X" bp | bp <- breakpoints machine]
        let memBlocksStr = "memory_trace_blocks: " ++ unlines (map (\(start, end, name) ->
                                                                    printf "%04X %04X" start end ++
                                                                    case name of
                                                                        Just n -> " " ++ n
                                                                        Nothing -> "")
                                                                   (memoryTraceBlocks machine))
        try (writeFile filePath (breakpointsStr ++ "\n" ++ memBlocksStr)) >>= \case
            Left e -> putOutput $ "Error saving debugger state: " ++ show (e :: IOException)
            Right _ -> putOutput $ "Debugger state saved to: " ++ filePath
    Nothing -> return ()

-- | Loads debugger state (breakpoints and memory trace blocks) from a file.
loadDebuggerState :: FilePath -> IO ([Word16], [(Word16, Word16, Maybe String)])
loadDebuggerState filePath = liftIO $ do
    fileContentOrError <- try (readFile filePath)
    case fileContentOrError of
        Left e -> do
            putOutput $ "Error loading debugger state: " ++ show (e :: IOException)
            return ([], [])
        Right fileContent -> do
            let ls = lines fileContent
            let breakpoints = parseBreakpoints ls
            let memBlocks = parseMemBlocks ls
            putOutput $ "Loaded debugger state from: " ++ filePath
            return (breakpoints, memBlocks)
  where
    parseBreakpoints :: [String] -> [Word16]
    parseBreakpoints = concatMap (parseWords . drop 1 . words) . filter (("breakpoints:" ==) . head . words)
    
    parseMemBlocks :: [String] -> [(Word16, Word16, Maybe String)]
    parseMemBlocks = concatMap (parseBlock . drop 1 . words) . filter (("memory_trace_blocks:" ==) . head . words)
    
    parseWords :: [String] -> [Word16]
    parseWords = mapMaybe (fmap fst . listToMaybe . readHex)
    
    parseBlock :: [String] -> [(Word16, Word16, Maybe String)]
    parseBlock [] = []
    parseBlock (startStr:endStr:rest) =
        case (parseHexWord startStr, parseHexWord endStr) of
            (Just startAddr, Just endAddr) ->
                let name = if null rest then Nothing else Just (unwords rest)
                in [(startAddr, endAddr, name)]
            _ -> [] -- Invalid address format or not enough arguments
    parseBlock _ = [] -- Invalid block format (e.g., only one address)


-- | Handles breakpoint commands in the debugger.
handleBreak :: [String] -> String -> FDX (DebuggerAction, [String])
handleBreak args lastCommand = do
  machine <- get
  case args of
    [] -> do -- List breakpoints
      let output = "Current breakpoints:" : map (\bp -> "  $" ++ showHex bp "") (breakpoints machine)
      return (ContinueLoop lastCommand, output)
    [addrStr] -> do -- Add or remove breakpoint
      case readHex addrStr of
        [(addr, "")] -> do
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
        _ -> do
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
      case (readHex startAddrStr, readHex endAddrStr) of
        ([(startAddr, "")], [(endAddr, "")]) -> do
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
-- | Helper function to safely parse a hex string to Maybe Word8.
parseHexByte :: String -> Maybe Word8
parseHexByte s = case readHex s of
  [(val, "")] | val >= 0 && val <= 255 -> Just (fromInteger val) -- Ensure value fits in Word8
  _           -> Nothing

-- | Helper function to safely parse a hex string to Maybe Word16.
parseHexWord :: String -> Maybe Word16
parseHexWord s = case readHex s of
  [(val, "")] | val >= 0 && val <= 65535 -> Just (fromInteger val) -- Ensure value fits in Word16
  _           -> Nothing

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
    let outputLines = ["Disassembling 32 instructions starting at $" ++ showHex startAddr ""]
    finalAddr <- disassembleInstructions startAddr 32 -- disassembleInstructions still prints directly for now
    modify (\m -> m { lastDisassembledAddr = finalAddr }) -- Update last disassembled address
    -- Note: disassembleInstructions currently prints directly. We would need to capture its output
    -- if we want to add it to the console state's outputLines. For now, we'll just return the initial message.
    return (ContinueLoop lastCommand, outputLines)
-- | Helper function to disassemble multiple instructions and print them.
disassembleInstructions :: Word16 -> Int -> FDX Word16 -- Return the address after the last disassembled instruction
disassembleInstructions currentPC 0 = return currentPC
disassembleInstructions currentPC remaining = do
    machine <- get
    let lblMap = labelMap machine
    -- Check if the currentPC has a label and print it
    case Map.lookup currentPC lblMap of
        Just lbl -> liftIO $ putOutput $ "\n\x1b[32m" ++ lbl ++ ":\x1b[0m" -- Print label on a new line if it exists
        Nothing  -> return ()

    -- Disassemble the current instruction
    (disassembled, instLen) <- disassembleInstruction currentPC
    liftIO $ putOutput disassembled
    let nextPC = currentPC + (fromIntegral instLen)
    disassembleInstructions nextPC (remaining - 1)

-- | Handles commands in CommandMode.
handleCommand :: String -> FDX (DebuggerAction, [String])
handleCommand commandToExecute = do
  machine <- get
  let handleStep :: FDX (DebuggerAction, [String])
      handleStep = return (ExecuteStep commandToExecute, []) -- Step doesn't produce immediate output
  let handleRegs :: FDX (DebuggerAction, [String])
      handleRegs = do
        -- logRegisters =<< getRegisters -- logRegisters prints directly
        regs <- getRegisters
        let sr = rSR regs
        let formatBinary8 b = p1 ++ " " ++ p2
              where
                p1 =  [if testBit b i then '1' else '0' | i <- [7,6..4]]
                p2 =  [if testBit b i then '1' else '0' | i <- [3,2..0]]
        let output =
              [ "--------------------------------------------"
              , "\x1b[35m\x1b[1mPC\x1b[0m\x1b[35m: $" ++ (formatHex16 (rPC regs)) ++ "\x1b[0m"
              , "\x1b[33m\x1b[1mAC\x1b[0m\x1b[33m: $" ++ formatHex8 (rAC regs) ++ " [" ++ formatBinary8 (rAC regs) ++ "] ( " ++ show (rAC regs) ++ " )\x1b[0m"
              , "\x1b[32m\x1b[1m X\x1b[0m\x1b[32m: $" ++ formatHex8 (rX regs) ++ " [\x1b[32m" ++ formatBinary8 (rX regs) ++ "\x1b[32m] ( " ++ show (rX regs) ++ " )\x1b[0m"
              , "\x1b[32m\x1b[1m Y\x1b[0m\x1b[32m: $" ++ formatHex8 (rY regs) ++ " [\x1b[32m" ++ formatBinary8 (rY regs) ++ "\x1b[32m] ( " ++ show (rY regs) ++ " )\x1b[0m"
              , "\x1b[35m\x1b[1mSP\x1b[0m\x1b[35m: $" ++ (formatHex8 (rSP regs)) ++ "\x1b[0m"
              , "\x1b[35m\x1b[1mSR\x1b[0m\x1b[35m:     [" ++ formatBinary8 (rSR regs)  ++ "]  " ++ formatStatusFlags sr ++ "\n        *NV-B DIZC*\x1b[0m"
              ]
        return (NoAction, output)

  let handleTrace :: FDX (DebuggerAction, [String])
      handleTrace = do
        let newTraceState = not (enableTrace machine)
        put (machine { enableTrace = newTraceState })
        let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
        return (NoAction, output)

  let handleGoto :: String -> FDX (DebuggerAction, [String])
      handleGoto addrStr = do
        machine <- get
        if null addrStr then do
          let output = ["Address required for goto command."]
          return (NoAction, output)
        else case readHex addrStr of
          [(addr, "")] -> do
            put (machine { mRegs = (mRegs machine) { rPC = addr } })
            let output = ["PC set to $" ++ showHex addr ""]
            return (NoAction, output)
          _ -> do
            let output = ["Invalid address format."]
            return (NoAction, output)

  let handleHelp :: FDX (DebuggerAction, [String])
      handleHelp = do
        let output = ["Available commands:\n\
\step  / z:              execute one instruction cycle\n\
\regs  / r:              show current register values\n\
\mem   / m [addr] [end]: add/remove memory range to dispaly\n\
\break / bk:             add/remove breakpoint to the list\n\
\quit  / q:              quit program\n\
\exit  / e:              exit interactive mode\n\
\trace / t:              toggle instruction tracing\n\
\goto  / g <addr>:       set program counter to address\n\
\fill  / f <start> <end> <byte1> [byte2...]: fill memory range with bytes\n\
\ra <val>:              set Accumulator to hex value\n\
\rx <val>:              set X register to hex value\n\
\ry <val>:              set Y register to hex value\n\
\rsp <val>:             set Stack Pointer to hex value\n\
\rsr <val>:             set Status Register to hex value\n\
\rpc <val>:             set Program Counter to hex value\n\
\d:                     disassemble 32 instructions from current PC"]
        return (NoAction, output)

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
      -- logMemoryRange prints directly, need to refactor
      mapM_ (\(start, end, name) -> logMemoryRange start end name) (memoryTraceBlocks machine)
      return (NoAction, ["Memory trace blocks logged (output above)."]) -- Placeholder output
    ["x"] -> do
      put (machine { debuggerActive = False }) -- Exit debugger mode
      let output = ["Exiting debugger. Continuing execution."]
      return (ExitDebugger, output)
    "bk":args -> handleBreak args commandToExecute
    "break":args -> handleBreak args commandToExecute -- Alias for bk
    ["q"] -> return (QuitEmulator, []) -- Set halted flag
    ["quit"] -> return (QuitEmulator, []) -- Set halted flag
    ["trace"] -> handleTrace
    ["t"] -> handleTrace
    ["addr-range", startAddrStr, endAddrStr] -> do
      case (readHex startAddrStr :: [(Word16, String)], readHex endAddrStr :: [(Word16, String)]) of
        ([(startAddr, "")], [(endAddr, "")]) -> do
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
