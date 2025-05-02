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
import Data.List (stripPrefix) -- Import stripPrefix


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
        liftIO $ putStrLn disassembled
        logRegisters =<< getRegisters
        -- Log all memory trace blocks
        mapM_ (uncurry logMemoryRange) (memoryTraceBlocks machineState)
      gets (not . halted)


-- | Initializes a new 6502 machine state
newMachine :: IO Machine
newMachine = do
  mem <- memory  -- 64KB of memory initialized by MOS6502Emulator.Memory
  let regs = mkRegisters
  return Machine { mRegs = regs, mMem = mem, halted = False, instructionCount = 0, cycleCount = 0, enableTrace = True, traceMemoryStart = 0x0000, traceMemoryEnd = 0x00FF, breakpoints = [], debuggerActive = False, memoryTraceBlocks = [] } -- Initialize new fields

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
setupMachine :: Machine -> [(Word16, Word8)] -> IO Machine
setupMachine machine memoryWrites = do
  mem <- foldr (\(addr, val) acc -> acc >>= \m -> writeByte addr val m >> return m) (return $ mMem machine) memoryWrites
  return machine { mMem = mem }


runTest :: Word16 -> Word16 -> [Word8] -> IO ()
runTest startAddress actualLoadAddress byteCode = do
  putStrLn $ "Running emulation test starting at $" ++ showHex startAddress ""
  putStrLn $ "Loading bytecode at $" ++ showHex actualLoadAddress ""
  initialMachine <- newMachine

  -- Prepare memory writes - load bytecode at its actual load address
  let memoryWrites = zip [actualLoadAddress..] byteCode

  -- Setup the machine (PC is set by runEmulator now)
  setupMachine initialMachine memoryWrites >>= \setupResult -> do
    putStrLn "Emulator machine setup complete."

    -- Run the emulator with the specified start address
    (_, finalMachine) <- runEmulator startAddress setupResult

    putStrLn "\n--- Emulation Finished ---"
    putStrLn $ "Final Registers: " ++ show (mRegs finalMachine)
    putStrLn $ "Instructions Executed: " ++ show (instructionCount finalMachine)


-- | Initializes the emulator with the given starting address and bytecode, then enters interactive debugger mode
runDebugger :: Word16 -> Word16 -> [Word8] -> IO ()
runDebugger startAddress actualLoadAddress byteCode = do
  putStrLn $ "Initializing debugger with code starting at $" ++ showHex startAddress ""
  putStrLn $ "Loading bytecode at $" ++ showHex actualLoadAddress ""
  initialMachine <- newMachine

  -- Prepare memory writes - load bytecode at its actual load address
  let memoryWrites = zip [actualLoadAddress..] byteCode

  -- Setup the machine
  setupMachine initialMachine memoryWrites >>= \setupResult -> do
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
\goto  / g <addr>:       set program counter to address"
    case words commandToExecute of
      ["help"] -> handleHelp >> interactiveLoopHelper commandToExecute
      ["h"] -> handleHelp >> interactiveLoopHelper commandToExecute
      ["goto", addrStr] -> handleGoto addrStr
      ["g", addrStr] -> handleGoto addrStr
      ["step"] -> handleStep
      ["z"] -> handleStep
      ["regs"] -> handleRegs
      ["r"] -> handleRegs
      "mem":args -> handleMemTrace args commandToExecute
      "m":args -> handleMemTrace args commandToExecute -- Alias for mem
      ["log"] -> do
        logMemoryRange (traceMemoryStart machine) (traceMemoryEnd machine)
        interactiveLoopHelper commandToExecute
      ["x"] -> do
        put (machine { debuggerActive = False }) -- Exit debugger mode
        liftIO $ putStrLn "Exiting debugger. Continuing execution."
        -- The runLoop in runMachine will now continue with fdxSingleCycle
      "bk":args -> handleBreak args commandToExecute
      "break":args -> handleBreak args commandToExecute -- Alias for bk
      ["log"] -> do
        logMemoryRange (traceMemoryStart machine) (traceMemoryEnd machine)
        interactiveLoopHelper commandToExecute
      ["q"] -> return ()
      ["quit"] -> return ()
      ["trace"] -> handleTrace
      ["t"] -> handleTrace
      ["addr-range", startAddrStr, endAddrStr] -> do
        case (readHex startAddrStr :: [(Word16, String)], readHex endAddrStr :: [(Word16, String)]) of
          ([(startAddr, "")], [(endAddr, "")]) -> do
            put (machine { traceMemoryStart = startAddr, traceMemoryEnd = endAddr })
            liftIO $ putStrLn $ "Memory trace range set to $" ++ showHex startAddr "" ++ " - $" ++ showHex endAddr ""
            interactiveLoopHelper commandToExecute
          _ -> do
            liftIO $ putStrLn "Invalid address format. Use hex (e.g., addr-range 0x0200 0x0300)."
            interactiveLoopHelper lastCommand
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
      mapM_ (\(start, end) -> liftIO $ putStrLn $ "  $" ++ showHex start "" ++ " - $" ++ showHex end "") (memoryTraceBlocks machine)
      interactiveLoopHelper lastCommand
    [startAddrStr, endAddrStr] -> do -- Add or remove memory trace block
      case (readHex startAddrStr, readHex endAddrStr) of
        ([(startAddr, "")], [(endAddr, "")]) -> do
          let currentBlocks = memoryTraceBlocks machine
          let newBlock = (startAddr, endAddr)
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
    _ -> do -- Incorrect number of arguments
      liftIO $ putStrLn "Invalid use of memory trace command. Use 'mem' or 'm' to list, or 'mem <start> <end>' to add/remove."
      interactiveLoopHelper lastCommand
