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
  , DebuggerMode(..) -- Export DebuggerMode
  ) where

import MOS6502Emulator.Machine (Machine(..), FDX(..), getRegisters, instructionCount, cycleCount, setRegisters, getMemory, setMemory, fetchByteMem, fetchWordMem, writeByteMem, mkWord, toWord, loadSymbolFile, setPC_, setAC_, setX_, setY_, setSR_, setSP_, writeByteMem_, DebuggerMode(..)) -- Import DebuggerMode from Machine
import MOS6502Emulator.Instructions
import MOS6502Emulator.Memory
import MOS6502Emulator.Registers
import qualified MOS6502Emulator.Debugger as D
import MOS6502Emulator.DissAssembler (disassembleInstruction) -- Import disassembleInstruction
import Control.Monad.State (get, modify, put, gets, runStateT) -- Import runStateT
import Control.Monad (when, unless, void) -- Import the 'when', 'unless', and 'void' functions
import Control.Monad.IO.Class (liftIO)
import Data.Word ( Word8, Word16 )
import Numeric (showHex, readHex) -- Import showHex and readHex
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, hReady, getChar) -- Import hSetBuffering, BufferMode, stdin, hReady, getChar, LineBuffering
import Data.List (stripPrefix, cycle, take) -- Import stripPrefix, cycle, take
import Data.Maybe (mapMaybe, listToMaybe, isNothing) -- Import mapMaybe, listToMaybe, and isNothing
import Data.Bits (Bits, (.&.)) -- Import Bits for status register manipulation if needed
import qualified Data.Map.Strict as Map -- For Map.empty
import System.IO (readFile, writeFile) -- Added for file operations
import Control.Exception (try, IOException) -- Added for exception handling
import System.IO.Error (isEOFError) -- Import isEOFError
import Control.Exception (catch) -- Import catch
import Text.Printf

-- | Performs a single fetch-decode-execute cycle of the 6502 emulator.
-- Returns `True` if emulation should continue, `False` if halted.
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
        D.logRegisters =<< getRegisters
        -- Log all memory trace blocks
        mapM_ (\(start, end, name) -> D.logMemoryRange start end name) (memoryTraceBlocks machineState)
      gets (not . halted)


-- | Initializes a new 6502 machine state
-- | Initializes a new 6502 machine state
newMachine :: IO Machine
newMachine = do
  mem <- memory  -- 64KB of memory initialized by MOS6502Emulator.Memory
  let regs = mkRegisters
  return Machine { mRegs = regs, mMem = mem, halted = False, instructionCount = 0, cycleCount = 0, enableTrace = True, traceMemoryStart = 0x0000, traceMemoryEnd = 0x00FF, breakpoints = [], debuggerActive = False, memoryTraceBlocks = [], lastDisassembledAddr = 0x0000, labelMap = Map.empty, debugLogPath = Nothing, debuggerMode = CommandMode, pcHistory = [], storedAddresses = Map.empty, redoHistory = [] } -- Initialize new fields, including debuggerMode, pcHistory, storedAddresses, and redoHistory

-- | The main fetch-decode-execute loop. Runs `fdxSingleCycle` repeatedly until emulation stops.
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

-- | Runs the FDX monad, handling debugger state and the main execution loop.
runMachine :: FDX () -> Machine -> IO ((), Machine)
runMachine debuggerLoop initialMachine = do
  liftIO $ putStrLn $ "Initial PC in runMachine: $" ++ showHex (rPC (mRegs initialMachine)) ""
  (result, finalMachine) <- runStateT (unFDX $ runLoop debuggerLoop) initialMachine
  saveDebuggerState finalMachine -- Save debugger state on exit
  return (result, finalMachine)
  where
    -- | The inner loop that manages debugger activation and instruction execution.
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
            -- Check if the machine is halted after executing the instruction
            if halted nextMachineState
              then do
                liftIO $ putStrLn "\nMachine halted. Entering debugger."
                put (nextMachineState { debuggerActive = True }) -- Activate debugger
                runLoop debuggerLoopAction -- Continue the main runLoop (will enter debugger next)
              else if not continue
                then return () -- Stop if fdxSingleCycle returns False (not halted, but some other stop condition)
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

-- | Sets up the initial state of the machine, including registers and memory.
-- It also loads debugger state and symbol files if paths are provided.
-- Note: This function no longer sets the PC, as it's handled by runEmulator
setupMachine :: Machine -> [(Word16, Word8)] -> Maybe FilePath -> Maybe FilePath -> IO Machine
setupMachine initialMachine memoryWrites maybeSymPath maybeDebugLogPath = do
    mem <- foldr (\(addr, val) acc -> acc >>= \m -> writeByte addr val m >> return m) (return $ mMem initialMachine) memoryWrites
    let machineWithMem = initialMachine { mMem = mem, debugLogPath = maybeDebugLogPath } -- Set debugLogPath here

    -- Load debugger state if p:220
    -- ath is provided
    (loadedBreakpoints, loadedMemBlocks) <- case maybeDebugLogPath of
      Just logPath -> loadDebuggerState logPath
      Nothing -> return ([], [])

    let machineWithLoadedState = machineWithMem { breakpoints = loadedBreakpoints, memoryTraceBlocks = loadedMemBlocks }

    -- Load symbol file if path is provided
    case maybeSymPath of
      Just symPath -> snd <$> runStateT (unFDX $ loadSymbolFile symPath) machineWithLoadedState
      Nothing -> return machineWithLoadedState

-- | Runs an emulation test with the given start address, load address, and bytecode.
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


-- | Initializes the emulator with the given starting address and bytecode, then enters interactive debugger mode.
-- It also loads symbol files if a path is provided.
runDebugger :: Word16 -> Word16 -> [Word8] -> Maybe FilePath -> IO Machine
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
    (_, finalMachine) <- runMachine (interactiveLoopHelper "") machineWithStartPC
    return finalMachine

-- | The main interactive debugger loop.
interactiveDebuggerLoop :: FDX ()
interactiveDebuggerLoop = interactiveLoopHelper "" -- Start with no last command

-- | Helper function for the interactive debugger loop, handling command input and execution.
interactiveLoopHelper :: String -> FDX ()
interactiveLoopHelper lastCommand = do
  machine <- get
  unless (halted machine) $ do
    case debuggerMode machine of
      CommandMode -> do
        cmd <- liftIO $ prompt "> "
        let commandToExecute = if null cmd then lastCommand else cmd
        handleCommand commandToExecute
        interactiveLoopHelper commandToExecute
      VimMode -> do
        liftIO $ putStr "> " >> hFlush stdout
        key <- liftIO getKey
        handleVimKey key
        interactiveLoopHelper lastCommand -- In Vim mode, last command is less relevant

-- | Handles commands in CommandMode.
handleCommand :: String -> FDX ()
handleCommand commandToExecute = do
  machine <- get
  let handleStep :: FDX ()
      handleStep = void fdxSingleCycle -- Use void to ignore the Bool result
  let handleRegs :: FDX ()
      handleRegs = D.logRegisters =<< getRegisters
  let handleTrace :: FDX ()
      handleTrace = do
        let newTraceState = not (enableTrace machine)
        put (machine { enableTrace = newTraceState })
        liftIO $ putStrLn $ "Tracing " ++ if newTraceState then "enabled." else "disabled."

  let handleGoto :: String -> FDX ()
      handleGoto addrStr = do
        machine <- get
        if null addrStr then do
          liftIO $ putStrLn "Address required for goto command."
        else case readHex addrStr of
          [(addr, "")] -> do
            put (machine { mRegs = (mRegs machine) { rPC = addr } })
            liftIO $ putStrLn $ "PC set to $" ++ showHex addr ""
          _ -> do
            liftIO $ putStrLn "Invalid address format."

  let handleHelp :: FDX ()
      handleHelp = liftIO $ putStrLn "Available commands:\n\
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
    ["help"] -> handleHelp
    ["h"] -> handleHelp
    ["goto", addrStr] -> handleGoto addrStr
    ["g", addrStr] -> handleGoto addrStr
    "fill":args -> handleFill args commandToExecute -- Pass lastCommand for recursive calls if needed, though fill doesn't use it
    "f":args -> handleFill args commandToExecute -- Alias for fill
    ["step"] -> handleStep
    ["z"] -> handleStep
    ["regs"] -> handleRegs
    ["r"] -> handleRegs
    "mem":args -> handleMemTrace args commandToExecute -- Pass lastCommand
    "m":args -> handleMemTrace args commandToExecute -- Alias for mem
    ["log"] -> do
      mapM_ (\(start, end, name) -> D.logMemoryRange start end name) (memoryTraceBlocks machine)
    ["x"] -> do
      put (machine { debuggerActive = False }) -- Exit debugger mode
      liftIO $ putStrLn "Exiting debugger. Continuing execution."
    "bk":args -> handleBreak args commandToExecute -- Pass lastCommand
    "break":args -> handleBreak args commandToExecute -- Alias for bk
    ["q"] -> modify (\m -> m { halted = True }) -- Set halted flag
    ["quit"] -> modify (\m -> m { halted = True }) -- Set halted flag
    ["trace"] -> handleTrace
    ["t"] -> handleTrace
    ["addr-range", startAddrStr, endAddrStr] -> do
      case (readHex startAddrStr :: [(Word16, String)], readHex endAddrStr :: [(Word16, String)]) of
        ([(startAddr, "")], [(endAddr, "")]) -> do
          put (machine { traceMemoryStart = startAddr, traceMemoryEnd = endAddr })
          liftIO $ putStrLn $ "Memory trace range set to $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""
        _ -> do
          liftIO $ putStrLn "Invalid address format. Use hex (e.g., addr-range 0200 0300)."
    -- Register setting commands
    ["ra", valStr] -> handleSetReg8 (\r val -> r { rAC = val }) valStr "Accumulator" commandToExecute -- Pass lastCommand
    ["rx", valStr] -> handleSetReg8 (\r val -> r { rX = val }) valStr "X Register" commandToExecute -- Pass lastCommand
    ["ry", valStr] -> handleSetReg8 (\r val -> r { rY = val }) valStr "Y Register" commandToExecute -- Pass lastCommand
    ["rsp", valStr] -> handleSetReg8 (\r val -> r { rSP = val }) valStr "Stack Pointer" commandToExecute -- Pass lastCommand
    ["rsr", valStr] -> handleSetReg8 (\r val -> r { rSR = val }) valStr "Status Register" commandToExecute -- Pass lastCommand
    ["rpc", valStr] -> handleSetPC valStr commandToExecute -- Pass lastCommand
    "d":args -> handleDisassemble args commandToExecute -- Pass lastCommand
    ["v"] ->       put (machine { debuggerMode = VimMode })
    _      -> do
      liftIO $ putStrLn "Invalid command."

-- | Handles key presses in VimMode.
handleVimKey :: Char -> FDX ()
handleVimKey key = do
  machine <- get
  case key of
    '\ESC' -> do
      liftIO $ putStrLn "\nExiting Vim mode."
      liftIO $ hSetBuffering stdin LineBuffering -- Restore line buffering
      put (machine { debuggerMode = CommandMode })
    'y' -> do
      liftIO $ putStrLn "\nStep forward..."
      -- Save current PC to history before stepping
      modify (\m -> m { pcHistory = rPC (mRegs m) : pcHistory m })
      _ <- fdxSingleCycle -- Execute one instruction and capture result
      return () -- Result already handled by fdxSingleCycle
    'k' -> do
      liftIO $ putStrLn "\nStep backward..."
      case pcHistory machine of
        [] -> liftIO $ putStrLn "No PC history to step back."
        (prevPC:restHistory) -> do
          put (machine { mRegs = (mRegs machine) { rPC = prevPC }, pcHistory = restHistory })
          liftIO $ putStrLn $ "PC set to $" ++ showHex prevPC ""
          -- Re-disassemble at the new PC
          finalAddr <- disassembleInstructions prevPC 1
          modify (\m -> m { lastDisassembledAddr = finalAddr })
          -- Add current PC to redo history
          modify (\m -> m { redoHistory = rPC (mRegs m) : redoHistory m })
    'r' -> do
      liftIO $ putStrLn "\nRegisters:"
      D.logRegisters =<< getRegisters
      -- Show flag bits in human-readable format
      let sr = rSR $ mRegs machine
      liftIO $ putStrLn $ "Flags: " ++ D.formatStatusFlags sr
    'm' -> do
      liftIO $ putStrLn "\n--- Memory Trace Blocks ---"
      if null (memoryTraceBlocks machine)
        then liftIO $ putStrLn "No memory trace blocks defined."
        else mapM_ (\(start, end, name) ->
                      liftIO $ putStrLn $ "  $" ++ showHex start "" ++ " - $" ++ showHex end "" ++
                                           case name of
                                               Just n -> " (" ++ n ++ ")"
                                               Nothing -> "")
                    (memoryTraceBlocks machine)
      liftIO $ putStrLn "---------------------------"
      liftIO $ putStrLn "  m + a: Add block, m + d: Delete block, m + e: Edit block" -- Updated help
    'b' -> do
      liftIO $ putStrLn "\n--- Breakpoints ---"
      if null (breakpoints machine)
        then liftIO $ putStrLn "No breakpoints defined."
        else mapM_ (\bp -> liftIO $ putStrLn $ "  $" ++ showHex bp "") (breakpoints machine)
      liftIO $ putStrLn "-------------------"
      -- TODO: Add sub-commands for adding/removing breakpoints (e.g., 'ba' to add, 'bd' to delete)
    'a' -> do
      liftIO $ putStrLn "\n--- Stored Addresses ---"
      if Map.null (storedAddresses machine)
        then liftIO $ putStrLn "No stored addresses defined."
        else mapM_ (\(key, addr) -> liftIO $ putStrLn $ "  '" ++ [key] ++ ": $" ++ showHex addr "") (Map.toList (storedAddresses machine))
      liftIO $ putStrLn "----------------------"
      liftIO $ putStrLn "  a + s: Store address, a + g: Goto address" -- Add help for sub-commands
      -- TODO: Implement stored addresses commands (e.g., 'as' to store, 'ag' to goto)
    'q' -> do
      liftIO $ putStrLn "\nQuitting..."
      modify (\m -> m { halted = True }) -- Set halted flag
    'm' -> handleMemTraceVim -- Delegate to a new handler for 'm' sub-commands
    'b' -> handleBreakVim -- Delegate to a new handler for 'b' sub-commands
    'a' -> handleAddressVim -- Delegate to a new handler for 'a' sub-commands
    _ -> do
      liftIO $ putStrLn $ "\nUnknown Vim key: " ++ [key]
      liftIO $ putStrLn "Available keys: y (step), k (back), r (regs), m (mem), b (break), a (addr), q (quit), ESC (cmd mode)"

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
        Nothing -> liftIO $ putStrLn $ "No address stored at key '" ++ [keyChar] ++ "'"
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
            setPC val -- Assuming setPC updates the state directly
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


-- | Saves the current debugger state (breakpoints and memory trace blocks) to a file.
-- | Saves the current debugger state (breakpoints and memory trace blocks) to a file.
-- | Saves the current debugger state (breakpoints and memory trace blocks) to a file.
-- | Saves the current debugger state (breakpoints and memory trace blocks) to a file.
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
    Nothing -> return () -- No debug log path specified


-- | Loads debugger state (breakpoints and memory trace blocks) from a file.
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
    -- | Parses breakpoint addresses from lines of text.
    parseBreakpoints :: [String] -> [Word16]
    parseBreakpoints = concatMap (parseWords . drop 1 . words) . filter (("breakpoints:" ==) . head . words)

    -- | Parses memory trace block definitions from lines of text.
    parseMemBlocks :: [String] -> [(Word16, Word16, Maybe String)]
    parseMemBlocks = concatMap (parseBlock . drop 1 . words) . filter (("memory_trace_blocks:" ==) . head . words)

    -- | Parses hexadecimal words from a list of strings.
    parseWords :: [String] -> [Word16]
    parseWords = mapMaybe (fmap fst . listToMaybe . readHex)

    -- | Parses a single memory trace block definition from a list of strings.
    parseBlock :: [String] -> [(Word16, Word16, Maybe String)]
    parseBlock [] = []
    parseBlock (startStr:endStr:rest) =
        case (parseHexWord startStr, parseHexWord endStr) of
            (Just startAddr, Just endAddr) ->
                let name = if null rest then Nothing else Just (unwords rest)
                in (startAddr, endAddr, name) : parseBlock [] -- Process rest as part of the name, handle next block on a new line
            _                              -> parseBlock rest -- Skip invalid block and continue parsing
    parseBlock _ = [] -- Invalid block format (e.g., only one address)
