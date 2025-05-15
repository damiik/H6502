{-# LANGUAGE LambdaCase #-}
module MOS6502Emulator.Debugger (logRegisters, logMemoryRange, saveDebuggerState, loadDebuggerState, formatStatusFlags) where

import Data.Word (Word16, Word8)
import Numeric (showHex, readHex)
import Control.Monad.IO.Class (liftIO)
import MOS6502Emulator.Machine (Machine(), FDX, getMemory, fetchByteMem, debugLogPath, breakpoints, memoryTraceBlocks, mRegs, storedAddresses, labelMap, lastDisassembledAddr, mMem)
import MOS6502Emulator.Machine (writeByteMem, setPC_)
import MOS6502Emulator.DissAssembler (disassembleInstruction)
import MOS6502Emulator.Machine (interactiveLoopHelper)
import Control.Monad.State (put, get, modify)
import MOS6502Emulator.Registers
import MOS6502Emulator.Memory (Memory(), writeByte)
import Data.Bits
import System.IO.Error (isEOFError)
import Control.Exception (SomeException, IOException, displayException, try, catch)
import qualified Data.Map as Map
import Text.Printf (printf)
import qualified Data.Char as Char
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(NoBuffering))
import Data.Maybe (mapMaybe, listToMaybe, isNothing)
import Data.List (cycle)

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

logRegisters :: Registers -> FDX ()
logRegisters reg = liftIO $ do
  let sr = rSR reg
  let formatBinary8 b = p1 ++ " " ++ p2
        where
          p1 =  [if testBit b i then '1' else '0' | i <- [7,6..4]]
          p2 =  [if testBit b i then '1' else '0' | i <- [3,2..0]]
  liftIO $ do
    putStrLn "--------------------------------------------"
    putStrLn $ "\x1b[35m\x1b[1mPC\x1b[0m\x1b[35m: $" ++ (formatHex16 (rPC reg)) ++ "\x1b[0m"
    putStrLn $ "\x1b[33m\x1b[1mAC\x1b[0m\x1b[33m: $" ++ formatHex8 (rAC reg) ++ " [" ++ formatBinary8 (rAC reg) ++ "] ( " ++ show (rAC reg) ++ " )\x1b[0m"
    putStrLn $ "\x1b[32m\x1b[1m X\x1b[0m\x1b[32m: $" ++ formatHex8 (rX reg) ++ " [\x1b[32m" ++ formatBinary8 (rX reg) ++ "\x1b[32m] ( " ++ show (rX reg) ++ " )\x1b[0m"
    putStrLn $ "\x1b[32m\x1b[1m Y\x1b[0m\x1b[32m: $" ++ formatHex8 (rY reg) ++ " [\x1b[32m" ++ formatBinary8 (rY reg) ++ "\x1b[32m] ( " ++ show (rY reg) ++ " )\x1b[0m"
    putStrLn $ "\x1b[35m\x1b[1mSP\x1b[0m\x1b[35m: $" ++ (formatHex8 (rSP reg)) ++ "\x1b[0m"
    putStrLn $ "\x1b[35m\x1b[1mSR\x1b[0m\x1b[35m:     [" ++ formatBinary8 (rSR reg)  ++ "]  " ++ formatStatusFlags sr ++ "\n        *NV-B DIZC*\x1b[0m"

logMemoryRange :: Word16 -> Word16 -> Maybe String -> FDX ()
logMemoryRange start end name = do
  bytes <- sequence [fetchByteMem addr | addr <- [start..end]]
  liftIO $ putStrLn $
    (case name of
        Just n -> n ++ " [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = "
        Nothing -> "MEM [$" ++ formatHex16 start ++ " - $" ++ formatHex16 end ++ "] = ") ++
    unwords (map formatHex8 bytes)

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
            Left e -> putStrLn $ "Error saving debugger state: " ++ show (e :: IOException)
            Right _ -> putStrLn $ "Debugger state saved to: " ++ filePath
    Nothing -> return ()

-- | Loads debugger state (breakpoints and memory trace blocks) from a file.
loadDebuggerState :: FilePath -> IO ([Word16], [(Word16, Word16, Maybe String)])
loadDebuggerState filePath = liftIO $ do
    fileContentOrError <- try (readFile filePath)
    case fileContentOrError of
        Left e -> do
            putStrLn $ "Error loading debugger state: " ++ show (e :: IOException)
            return ([], [])
        Right fileContent -> do
            let ls = lines fileContent
            let breakpoints = parseBreakpoints ls
            let memBlocks = parseMemBlocks ls
            putStrLn $ "Loaded debugger state from: " ++ filePath
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
        case (readHex startStr, readHex endStr) of
            ([(startAddr, "")], [(endAddr, "")]) ->
                let name = if null rest then Nothing else Just (unwords rest)
                in [(startAddr, endAddr, name)]
            _ -> []
    parseBlock _ = []

-- | Handles 'm' sub-commands in VimMode.
handleMemTraceVim :: FDX ()
handleMemTraceVim = do
  liftIO $ putStrLn "\nMemory Trace (a: add, d: delete):"
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStr "Add Memory Trace Block (start end [name]): " >> hFlush stdout
      input <- liftIO getLine
      let args = words input
      case args of
        [startAddrStr, endAddrStr] -> do
          case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
            (Just startAddr, Just endAddr) -> do
              modify (\m -> m { memoryTraceBlocks = (startAddr, endAddr, Nothing) : memoryTraceBlocks m })
              liftIO $ putStrLn $ "Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""
            _ -> liftIO $ putStrLn "Invalid address format."
        startAddrStr:endAddrStr:nameWords -> do
          case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
            (Just startAddr, Just endAddr) -> do
              let name = unwords nameWords
              modify (\m -> m { memoryTraceBlocks = (startAddr, endAddr, Just name) : memoryTraceBlocks m })
              liftIO $ putStrLn $ "Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"
            _ -> liftIO $ putStrLn "Invalid address format."
        _ -> liftIO $ putStrLn "Invalid arguments for adding memory trace block."
    'd' -> do
      liftIO $ putStr "Delete Memory Trace Block (start end [name]): " >> hFlush stdout
      input <- liftIO getLine
      let args = words input
      case args of
        [startAddrStr, endAddrStr] -> do
          case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
            (Just startAddr, Just endAddr) -> do
              modify (\m -> m { memoryTraceBlocks = filter (\(s, e, n) -> not (s == startAddr && e == endAddr && isNothing n)) (memoryTraceBlocks m) })
              liftIO $ putStrLn $ "Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""
            _ -> liftIO $ putStrLn "Invalid address format."
        startAddrStr:endAddrStr:nameWords -> do
          case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
            (Just startAddr, Just endAddr) -> do
              let name = unwords nameWords
              modify (\m -> m { memoryTraceBlocks = filter (\(s, e, n) -> not (s == startAddr && e == endAddr && Just name == n)) (memoryTraceBlocks m) })
              liftIO $ putStrLn $ "Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"
            _ -> liftIO $ putStrLn "Invalid address format."
        _ -> liftIO $ putStrLn "Invalid arguments for deleting memory trace block."
    _ -> liftIO $ putStrLn "Invalid Memory Trace command. Press 'm' to view blocks."

-- | Handles 'b' sub-commands in VimMode.
handleBreakVim :: FDX ()
handleBreakVim = do
  liftIO $ putStrLn "\nBreakpoints (a: add, d: delete):"
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStrLn "Add Breakpoint (address):"
      input <- liftIO getLine
      let args = words input
      handleBreak args "" -- Reuse handleBreak logic
    'd' -> do
      liftIO $ putStrLn "Delete Breakpoint (address):"
      input <- liftIO getLine
      let args = words input
      handleBreak args "" -- Reuse handleBreak logic
    _ -> liftIO $ putStrLn "Invalid Breakpoint command."

-- | Handles 'a' sub-commands in VimMode (Stored Addresses).
handleAddressVim :: FDX ()
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
    'g' -> do
      liftIO $ putStrLn "Goto Stored Address (key):"
      keyChar <- liftIO getKey
      machine <- get
      case Map.lookup keyChar (storedAddresses machine) of
        Just addr -> do
          put (machine { mRegs = (mRegs machine) { rPC = addr } })
          liftIO $ putStrLn $ "PC set to $" ++ showHex addr ""
          -- Re-disassemble at the new PC
          finalAddr <- disassembleInstructions addr 1
          modify (\m -> m { lastDisassembledAddr = finalAddr })
        Nothing -> liftIO $ putStrLn ("No address stored at key '" ++ [keyChar] ++ "'")
    _ -> liftIO $ putStrLn "Invalid Stored Address command."


-- | Prompts the user for input in the debugger.
prompt :: String -> IO String
prompt msg = liftIO $ do
  putStr msg
  hFlush stdout
  getLine

-- | Reads a single character from stdin without buffering.
getKey :: IO Char
getKey = catch
  (do
    hSetBuffering stdin NoBuffering
    c <- getChar
    return c
  )
  (\e -> if isEOFError e then return '\n' else ioError e) -- Handle EOF (e.g., Ctrl+D)

-- | Handles breakpoint commands in the debugger.
handleBreak :: [String] -> String -> FDX ()
handleBreak args lastCommand = do
  machine <- get
  case args of
    [] -> do -- List breakpoints
      liftIO $ putStrLn "Current breakpoints:"
      mapM_ (\bp -> liftIO $ putStrLn $ "  $" ++ showHex bp "") (breakpoints machine)
      interactiveLoopHelper lastCommand
    [addrStr] -> do -- Add or remove breakpoint
      case readHex addrStr of
        [(addr, "")] -> do
          let currentBreakpoints = breakpoints machine
          if addr `elem` currentBreakpoints
            then do
              let newBreakpoints = filter (/= addr) currentBreakpoints
              put (machine { breakpoints = newBreakpoints })
              liftIO $ putStrLn $ "Breakpoint removed at $" ++ showHex addr ""
              interactiveLoopHelper lastCommand
            else do
              let newBreakpoints = addr : currentBreakpoints
              put (machine { breakpoints = newBreakpoints })
              liftIO $ putStrLn $ "Breakpoint added at $" ++ showHex addr ""
              interactiveLoopHelper lastCommand
        _ -> do
          liftIO $ putStrLn "Invalid address format for breakpoint. Use hex (e.g., bk 0x0400)."
          interactiveLoopHelper lastCommand
    _ -> do -- Too many arguments
      liftIO $ putStrLn "Invalid use of breakpoint command. Use 'bk' or 'break' to list, or 'bk <address>' to add/remove."
      interactiveLoopHelper lastCommand

-- | Handles memory trace commands in the debugger.
handleMemTrace :: [String] -> String -> FDX ()
handleMemTrace args lastCommand = do
  machine <- get
  case args of
    [] -> do -- List memory trace blocks
      liftIO $ putStrLn "Current memory trace blocks:"
      mapM_ (\(start, end, name) ->
              liftIO $ putStrLn $ "  $" ++ showHex start "" ++ " - $" ++ showHex end "" ++
                                   case name of
                                       Just n -> " (" ++ n ++ ")"
                                       Nothing -> "")
            (memoryTraceBlocks machine)
      interactiveLoopHelper lastCommand
    [startAddrStr, endAddrStr] -> do -- Add or remove memory trace block without name
      case (readHex startAddrStr, readHex endAddrStr) of
        ([(startAddr, "")], [(endAddr, "")]) -> do
          let currentBlocks = memoryTraceBlocks machine
          let newBlock = (startAddr, endAddr, Nothing)
          if newBlock `elem` currentBlocks
            then do
              let newBlocks = filter (/= newBlock) currentBlocks
              put (machine { memoryTraceBlocks = newBlocks })
              liftIO $ putStrLn $ "Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""
              interactiveLoopHelper lastCommand
            else do
              let newBlocks = newBlock : currentBlocks
              put (machine { memoryTraceBlocks = newBlocks })
              liftIO $ putStrLn $ "Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""
              interactiveLoopHelper lastCommand
        _ -> do
          liftIO $ putStrLn "Invalid address format for memory trace block. Use hex (e.g., mem 0x0200 0x0300)."
          interactiveLoopHelper lastCommand
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
              liftIO $ putStrLn $ "Memory trace block removed: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"
              interactiveLoopHelper lastCommand
            else do
              let newBlocks = newBlock : currentBlocks
              put (machine { memoryTraceBlocks = newBlocks })
              liftIO $ putStrLn $ "Memory trace block added: $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr "" ++ " (" ++ name ++ ")"
              interactiveLoopHelper lastCommand
        _ -> do
          liftIO $ putStrLn "Invalid address format for memory trace block. Use hex (e.g., mem 0x0200 0x0300 MyRegion)."
          interactiveLoopHelper lastCommand
    _ -> do -- Incorrect number of arguments
      liftIO $ putStrLn "Invalid use of memory trace command. Use 'mem' or 'm' to list, 'mem <start> <end>' to add/remove without name, or 'mem <start> <end> <name>' to add/remove with name."
      interactiveLoopHelper lastCommand

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
handleFill :: [String] -> String -> FDX ()
handleFill args lastCommand = do
  case args of
    startAddrStr : endAddrStr : byteStrs -> do
      case (parseHexWord startAddrStr, parseHexWord endAddrStr) of
        (Just startAddr, Just endAddr) -> do
          let byteValues = mapMaybe parseHexByte byteStrs
          if null byteValues
            then do
              liftIO $ putStrLn "No valid byte values provided or parse error."
              interactiveLoopHelper lastCommand
            else if startAddr > endAddr
            then do
              liftIO $ putStrLn "Start address cannot be greater than end address."
              interactiveLoopHelper lastCommand
            else do
              let addressRange = [startAddr .. endAddr]
              let fillBytes = take (length addressRange) (Data.List.cycle byteValues)
              machine <- get
              let mem = mMem machine
              -- Perform writes within FDX using liftIO
              liftIO $ mapM_ (\(addr, val) -> writeByte addr val mem) (zip addressRange fillBytes)
              -- Memory is modified in-place, no need to 'put' the machine state back just for this
              liftIO $ putStrLn $ "Memory filled from $" ++ showHex startAddr "" ++ " to $" ++ showHex endAddr ""
              interactiveLoopHelper lastCommand
        _ -> do
          liftIO $ putStrLn "Invalid address format for fill command. Use hex (e.g., fill 0200 0300 ff 00)."
          interactiveLoopHelper lastCommand
    _ -> do
      liftIO $ putStrLn "Invalid use of fill command. Use 'fill <start> <end> <byte1> [byte2...]'"
      interactiveLoopHelper lastCommand

-- | Generic handler for setting 8-bit registers in the debugger.
handleSetReg8 :: (Registers -> Word8 -> Registers) -> String -> String -> String -> FDX ()
handleSetReg8 regSetter valStr regName lastCommand = do
    case parseHexByte valStr of
        Just val -> do
            modify (\m -> m { mRegs = regSetter (mRegs m) val })
            liftIO $ putStrLn $ regName ++ " set to $" ++ showHex val ""
            interactiveLoopHelper lastCommand
        Nothing -> do
            liftIO $ putStrLn $ "Invalid hex value for " ++ regName ++ "."
            interactiveLoopHelper lastCommand

-- | Specific handler for setting the 16-bit PC in the debugger.
handleSetPC :: String -> String -> FDX ()
handleSetPC valStr lastCommand = do
    case parseHexWord valStr of
        Just val -> do
            setPC_ val -- Assuming setPC updates the state directly
            liftIO $ putStrLn $ "PC set to $" ++ showHex val ""
            interactiveLoopHelper lastCommand
        Nothing -> do
            liftIO $ putStrLn "Invalid hex value for PC."
            interactiveLoopHelper lastCommand

-- | Handler for disassembling instructions in the debugger.
handleDisassemble :: [String] -> String -> FDX ()
handleDisassemble args lastCommand = do
    machine <- get
    let startAddr = case args of
                      [addrStr] -> case parseHexWord addrStr of
                                     Just addr -> addr
                                     Nothing -> lastDisassembledAddr machine -- Use last disassembled address on invalid input
                      _         -> lastDisassembledAddr machine -- Use last disassembled address if no argument
    liftIO $ putStrLn $ "Disassembling 32 instructions starting at $" ++ showHex startAddr ""
    finalAddr <- disassembleInstructions startAddr 32
    modify (\m -> m { lastDisassembledAddr = finalAddr }) -- Update last disassembled address
    interactiveLoopHelper lastCommand

-- | Helper function to disassemble multiple instructions and print them.
disassembleInstructions :: Word16 -> Int -> FDX Word16 -- Return the address after the last disassembled instruction
disassembleInstructions currentPC 0 = return currentPC
disassembleInstructions currentPC remaining = do
    machine <- get
    let lblMap = labelMap machine
    -- Check if the currentPC has a label and print it
    case Map.lookup currentPC lblMap of
        Just lbl -> liftIO $ putStrLn $ "\n\x1b[32m" ++ lbl ++ ":\x1b[0m" -- Print label on a new line if it exists
        Nothing  -> return ()

    -- Disassemble the current instruction
    (disassembled, instLen) <- disassembleInstruction currentPC
    liftIO $ putStrLn disassembled
    let nextPC = currentPC + (fromIntegral instLen)
    disassembleInstructions nextPC (remaining - 1)
