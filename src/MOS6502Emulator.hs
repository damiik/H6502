{-# LANGUAGE LambdaCase #-} -- Added for \case syntax
module MOS6502Emulator
  ( runEmulator
  , runTest
  , runDebugger
  , newMachine
  , setupMachine
  , Machine(..)
  , Memory
  , Registers
  , instructionCount -- Export instructionCount
  , saveDebuggerState -- Export save function
  , loadDebuggerState -- Export load function
  ) where

import MOS6502Emulator.Machine
import MOS6502Emulator.Instructions
import MOS6502Emulator.Memory
import MOS6502Emulator.Registers
import MOS6502Emulator.Debugger
import MOS6502Emulator.DissAssembler (disassembleInstruction) -- Import disassembleInstruction
import Control.Monad.State (get, modify, put, gets, runStateT) -- Import runStateT
import Control.Monad (when, unless) -- Import the 'when' function
import Control.Monad.IO.Class (liftIO)
import Data.Word ( Word8, Word16 )
import Numeric (showHex, readHex) -- Import showHex and readHex
import System.IO (hFlush, stdout)
import Data.List (stripPrefix, cycle, take) -- Import stripPrefix, cycle, take
import Data.Maybe (mapMaybe, listToMaybe) -- Import mapMaybe and listToMaybe
import Data.Bits (Bits, (.&.)) -- Import Bits for status register manipulation if needed
import qualified Data.Map.Strict as Map -- For Map.empty
import System.IO (readFile, writeFile) -- Added for file operations
import Control.Exception (try, IOException) -- Added for exception handling


fdxSingleCycle :: FDX Bool -- Returns True if emulation should continue, False if halted
fdxSingleCycle = do
  -- liftIO $ putStrLn ""
  machineState <- get
  -- liftIO $ putStrLn $ "Current PC at start of fdxSingleCycle: $" ++ showHex (rPC (mRegs machineState)) ""
  if halted machineState
    then return False -- Machine is halted, stop emulation
    else do
      pc <- getReg rPC  -- Get current PC
      let currentPC = pc -- Store PC before incrementing
      b <- fetchByteMem pc -- Fetch opcode byte at PC
      setPC (pc + 1)   -- Move PC to next byte (like a real 6502)
      modify (\s -> s { instructionCount = instructionCount s + 1 })
      execute b
      when (enableTrace machineState) $ do
        disassembled <- disassembleInstruction currentPC -- Use the stored PC
        liftIO $ putStrLn ""
        liftIO $ putStrLn (fst disassembled)
        logRegisters =<< getRegisters
        -- Log all memory trace blocks
        mapM_ (\(start, end, name) -> logMemoryRange start end name) (memoryTraceBlocks machineState)
      gets (not . halted)


-- | Initializes a new 6502 machine state
newMachine :: IO Machine
newMachine = do
  mem <- memory  -- 64KB of memory initialized by MOS6502Emulator.Memory
  let regs = mkRegisters
  return Machine { mRegs = regs, mMem = mem, halted = False, instructionCount = 0, cycleCount = 0, enableTrace = True, traceMemoryStart = 0x0000, traceMemoryEnd = 0x00FF, breakpoints = [], debuggerActive = False, memoryTraceBlocks = [], lastDisassembledAddr = 0x0000, labelMap = Map.empty, debugLogPath = Nothing } -- Initialize new fields, including labelMap and debugLogPath

-- | The main fetch-decode-execute loop
runFDXLoop :: FDX ()
runFDXLoop = do
  continue <- fdxSingleCycle
  when continue runFDXLoop

-- | Runs the emulator until the machine is halted
-- | Runs the emulator with the given machine state and starting PC
runEmulator :: Word16 -> Machine -> IO ((), Machine)
runEmulator startPC initialMachine = do
  let machineWithStartPC = initialMachine { mRegs = (mRegs initialMachine) { rPC = startPC } }
  runMachine runFDXLoop machineWithStartPC

-- Run the FDX monad, handling debugger state
runMachine :: FDX () -> Machine -> IO ((), Machine)
runMachine debuggerLoop initialMachine = do
  liftIO $ putStrLn $ "Initial PC in runMachine: $" ++ showHex (rPC (mRegs initialMachine)) ""
  (result, finalMachine) <- runStateT (unFDX $ runLoop debuggerLoop) initialMachine
  saveDebuggerState finalMachine -- Save debugger state on exit
  return (result, finalMachine)
  where
    runLoop :: FDX () -> FDX ()
    runLoop debuggerLoopAction = do
      machine <- get
      if halted machine
        then return () -- Stop if machine is halted
        else if debuggerActive machine
          then do
            liftIO $ putStrLn "\nEntering interactive debugger."
            debuggerLoopAction -- Run the interactive debugger loop
            nextMachineState <- get -- Get state after debugger loop
            runLoop debuggerLoopAction -- Continue the main runLoop
          else do
            continue <- fdxSingleCycle -- Execute one instruction
            nextMachineState <- get -- Get state after instruction execution
            if not continue
              then return () -- Stop if fdxSingleCycle returns False (halted)
              else do
                -- Check for breakpoints after executing the instruction
                let currentPC = rPC (mRegs nextMachineState)
                if currentPC `elem` breakpoints nextMachineState
                  then do
                    liftIO $ putStrLn $ "\nBreakpoint hit at $" ++ showHex currentPC ""
                    put (nextMachineState { debuggerActive = True }) -- Activate debugger
                    runLoop debuggerLoopAction -- Continue the main runLoop (will enter debugger next)
                  else
                    runLoop debuggerLoopAction -- Continue the main runLoop (execute next instruction)

-- | Sets up the initial state of the machine, including registers and memory
-- Note: This function no longer sets the PC, as it's handled by runEmulator
setupMachine :: Machine -> [(Word16, Word8)] -> Maybe FilePath -> Maybe FilePath -> IO Machine
setupMachine initialMachine memoryWrites maybeSymPath maybeDebugLogPath = do
  mem <- foldr (\(addr, val) acc -> acc >>= \m -> writeByte addr val m >> return m) (return $ mMem initialMachine) memoryWrites
  let machineWithMem = initialMachine { mMem = mem, debugLogPath = maybeDebugLogPath } -- Set debugLogPath here

  -- Load debugger state if path is provided
  (loadedBreakpoints, loadedMemBlocks) <- case maybeDebugLogPath of
    Just logPath -> loadDebuggerState logPath
    Nothing -> return ([], [])

  let machineWithLoadedState = machineWithMem { breakpoints = loadedBreakpoints, memoryTraceBlocks = loadedMemBlocks }

  -- Load symbol file if path is provided
  case maybeSymPath of
    Just symPath -> snd <$> runStateT (unFDX $ loadSymbolFile symPath) machineWithLoadedState
    Nothing -> return machineWithLoadedState


runTest :: Word16 -> Word16 -> [Word8] -> IO ()
runTest startAddress actualLoadAddress byteCode = do
  putStrLn $ "Running emulation test starting at $" ++ showHex startAddress ""
  putStrLn $ "Loading bytecode at $" ++ showHex actualLoadAddress ""
  initialMachine <- newMachine

  -- Prepare memory writes - load bytecode at its actual load address
  let memoryWrites = zip [actualLoadAddress..] byteCode

  -- Setup the machine (PC is set by runEmulator now)
  -- Pass Nothing for symbol file path and debug log path in runTest
  setupMachine initialMachine memoryWrites Nothing Nothing >>= \setupResult -> do
    putStrLn "Emulator machine setup complete."

    -- Run the emulator with the specified start address
    (_, finalMachine) <- runEmulator startAddress setupResult

    putStrLn "\n--- Emulation Finished ---"
    putStrLn $ "Final Registers: " ++ show (mRegs finalMachine)
    putStrLn $ "Instructions Executed: " ++ show (instructionCount finalMachine)


-- | Initializes the emulator with the given starting address and bytecode, then enters interactive debugger mode
runDebugger :: Word16 -> Word16 -> [Word8] -> Maybe FilePath -> IO ()
runDebugger startAddress actualLoadAddress byteCode maybeSymPath = do
  putStrLn $ "Initializing debugger with code starting at $" ++ showHex startAddress ""
  putStrLn $ "Loading bytecode at $" ++ showHex actualLoadAddress ""
  initialMachine <- newMachine

  -- Prepare memory writes - load bytecode at its actual load address
  let memoryWrites = zip [actualLoadAddress..] byteCode

  -- Setup the machine, including loading symbols and debugger state
  setupMachine initialMachine memoryWrites maybeSymPath (Just "debugger_state.log") >>= \setupResult -> do -- Hardcode default log path
    putStrLn "Emulator machine setup complete."

    -- Set the starting PC and enter interactive debugger loop
    let machineWithStartPC = setupResult { mRegs = (mRegs setupResult) { rPC = startAddress }, debuggerActive = True } -- Set debuggerActive to True
    putStrLn "\nEntering interactive debugger."
    _ <- runMachine (interactiveLoopHelper "") machineWithStartPC
    return ()


interactiveDebuggerLoop :: FDX ()
interactiveDebuggerLoop = interactiveLoopHelper "" -- Start with no last command

interactiveLoopHelper :: String -> FDX ()
interactiveLoopHelper lastCommand = do
  machine <- get
  unless (halted machine) $ do
    cmd <- liftIO $ prompt "> "
    let commandToExecute = if null cmd then lastCommand else cmd
    let handleStep = fdxSingleCycle >> interactiveLoopHelper commandToExecute
    let handleRegs = (logRegisters =<< getRegisters) >> interactiveLoopHelper commandToExecute
    let handleTrace = do
          let newTraceState = not (enableTrace machine)
          put (machine { enableTrace = newTraceState })
          liftIO $ putStrLn $ "Tracing " ++ if newTraceState then "enabled." else "disabled."
          interactiveLoopHelper commandToExecute

    let handleGoto addrStr = do
          machine <- get
          if null addrStr then do
            liftIO $ putStrLn "Address required for goto command."
            interactiveLoopHelper commandToExecute
          else case readHex addrStr of
            [(addr, "")] -> do
              put (machine { mRegs = (mRegs machine) { rPC = addr } })
              liftIO $ putStrLn $ "PC set to $" ++ showHex addr ""
              interactiveLoopHelper commandToExecute
            _ -> do
              liftIO $ putStrLn "Invalid address format."
              interactiveLoopHelper commandToExecute

    let handleHelp = liftIO $ putStrLn "Available commands:\n\
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
\d:                     disassemble 32 instructions from current PC"
    case words commandToExecute of
      ["help"] -> handleHelp >> interactiveLoopHelper commandToExecute
      ["h"] -> handleHelp >> interactiveLoopHelper commandToExecute
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
        -- Need to get the specific memory trace block to log, or just log all of them?
        -- Based on current behavior, it logs the single range stored in traceMemoryStart/End.
        -- Let's assume 'log' without args logs all named/unnamed blocks.
        -- If they want to log the single range, they can use 'mem <start> <end>'.
        mapM_ (\(start, end, name) -> logMemoryRange start end name) (memoryTraceBlocks machine)
        interactiveLoopHelper commandToExecute
      ["x"] -> do
        put (machine { debuggerActive = False }) -- Exit debugger mode
        liftIO $ putStrLn "Exiting debugger. Continuing execution."
        -- The runLoop in runMachine will now continue with fdxSingleCycle
      "bk":args -> handleBreak args commandToExecute
      "break":args -> handleBreak args commandToExecute -- Alias for bk
      ["log"] -> do
        -- This seems like a duplicate 'log' command case. Removing this one.
        interactiveLoopHelper commandToExecute -- Just continue the loop
      ["q"] -> modify (\m -> m { halted = True }) -- Set halted flag
      ["quit"] -> modify (\m -> m { halted = True }) -- Set halted flag
      ["trace"] -> handleTrace
      ["t"] -> handleTrace
      ["addr-range", startAddrStr, endAddrStr] -> do
        case (readHex startAddrStr :: [(Word16, String)], readHex endAddrStr :: [(Word16, String)]) of
          ([(startAddr, "")], [(endAddr, "")]) -> do
            put (machine { traceMemoryStart = startAddr, traceMemoryEnd = endAddr })
            liftIO $ putStrLn $ "Memory trace range set to $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""
            interactiveLoopHelper commandToExecute
          _ -> do
            liftIO $ putStrLn "Invalid address format. Use hex (e.g., addr-range 0200 0300)."
            interactiveLoopHelper lastCommand
      -- Register setting commands
      ["ra", valStr] -> handleSetReg8 (\r val -> r { rAC = val }) valStr "Accumulator" commandToExecute
      ["rx", valStr] -> handleSetReg8 (\r val -> r { rX = val }) valStr "X Register" commandToExecute
      ["ry", valStr] -> handleSetReg8 (\r val -> r { rY = val }) valStr "Y Register" commandToExecute
      ["rsp", valStr] -> handleSetReg8 (\r val -> r { rSP = val }) valStr "Stack Pointer" commandToExecute
      ["rsr", valStr] -> handleSetReg8 (\r val -> r { rSR = val }) valStr "Status Register" commandToExecute
      ["rpc", valStr] -> handleSetPC valStr commandToExecute
      "d":args -> handleDisassemble args commandToExecute
      _      -> do
        liftIO $ putStrLn "Invalid command."
        interactiveLoopHelper lastCommand -- Don't update last command on invalid input

prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine

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

-- Helper function to safely parse a hex string to Maybe Word8
parseHexByte :: String -> Maybe Word8
parseHexByte s = case readHex s of
  [(val, "")] | val >= 0 && val <= 255 -> Just (fromInteger val) -- Ensure value fits in Word8
  _           -> Nothing

-- Helper function to safely parse a hex string to Maybe Word16
parseHexWord :: String -> Maybe Word16
parseHexWord s = case readHex s of
  [(val, "")] | val >= 0 && val <= 65535 -> Just (fromInteger val) -- Ensure value fits in Word16
  _           -> Nothing

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

-- Generic handler for setting 8-bit registers
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

-- Specific handler for setting the 16-bit PC
handleSetPC :: String -> String -> FDX ()
handleSetPC valStr lastCommand = do
    case parseHexWord valStr of
        Just val -> do
            setPC val -- Assuming setPC updates the state directly
            liftIO $ putStrLn $ "PC set to $" ++ showHex val ""
            interactiveLoopHelper lastCommand
        Nothing -> do
            liftIO $ putStrLn "Invalid hex value for PC."
            interactiveLoopHelper lastCommand

-- Handler for disassembling 32 instructions
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

-- Helper function to disassemble multiple instructions
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


-- | Saves the current debugger state (breakpoints and memory trace blocks) to a file
saveDebuggerState :: Machine -> IO ()
saveDebuggerState machine =
    case debugLogPath machine of
        Just filePath -> liftIO $ do
            let breakpointsStr = "breakpoints: " ++ unwords (map (\bp -> showHex bp "") (breakpoints machine))
            let memBlocksStr = "memory_trace_blocks: " ++ unlines (map (\(start, end, name) ->
                                                                        showHex start "" ++ " " ++ showHex end "" ++
                                                                        case name of
                                                                            Just n -> " " ++ n
                                                                            Nothing -> "")
                                                                   (memoryTraceBlocks machine))
            try (writeFile filePath (breakpointsStr ++ "\n" ++ memBlocksStr)) >>= \case
                Left e -> putStrLn $ "Error saving debugger state: " ++ show (e :: IOException)
                Right _ -> putStrLn $ "Debugger state saved to: " ++ filePath
        Nothing -> return () -- No debug log path specified


-- | Loads debugger state (breakpoints and memory trace blocks) from a file
loadDebuggerState :: FilePath -> IO ([Word16], [(Word16, Word16, Maybe String)])
loadDebuggerState filePath = liftIO $ do
    fileContentOrError <- try (readFile filePath)
    case fileContentOrError of
        Left e -> do
            putStrLn $ "Error loading debugger state: " ++ show (e :: IOException)
            return ([], []) -- Return empty lists on error
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
        case (parseHexWord startStr, parseHexWord endStr) of
            (Just startAddr, Just endAddr) ->
                let name = if null rest then Nothing else Just (unwords rest)
                in (startAddr, endAddr, name) : parseBlock [] -- Process rest as part of the name, handle next block on a new line
            _                              -> parseBlock rest -- Skip invalid block and continue parsing
    parseBlock _ = [] -- Invalid block format (e.g., only one address)
