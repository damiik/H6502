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
import Control.Monad.State (get, modify, put) -- Import get, modify, and put
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
  when (enableTrace machineState) $ do
    logRegisters =<< getRegisters
    logMemoryRange (traceMemoryStart machineState) (traceMemoryEnd machineState)
  -- liftIO $ putStrLn $ "Current PC at start of fdxSingleCycle: $" ++ showHex (rPC (mRegs machineState)) ""
  if halted machineState
    then return False -- Machine is halted, stop emulation
    else do
      -- liftIO $ putStrLn "Fetching instruction..."
      pc <- getReg rPC  -- Get current PC
      b <- fetchByteMem pc -- Fetch opcode byte at PC
      setPC (pc + 1)   -- Move PC to next byte (like a real 6502)
      modify (\s -> s { instructionCount = instructionCount s + 1 })
      execute b
      machineState' <- get
      return (not (halted machineState'))


-- | Initializes a new 6502 machine state
newMachine :: IO Machine
newMachine = do
  mem <- memory  -- 64KB of memory initialized by MOS6502Emulator.Memory
  let regs = mkRegisters
  return Machine { mRegs = regs, mMem = mem, halted = False, instructionCount = 0, cycleCount = 0, enableTrace = False, traceMemoryStart = 0x0000, traceMemoryEnd = 0x00FF }

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
    let machineWithStartPC = setupResult { mRegs = (mRegs setupResult) { rPC = startAddress } }
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
    let handleMem addrStr addrEnd = do
          if null addrStr && null addrEnd then
            do 
            logMemoryRange (traceMemoryStart machine) (traceMemoryEnd machine)
            interactiveLoopHelper commandToExecute
          else
            case (readHex addrStr, readHex addrEnd) of
              ([(addrStr', "")], [(addrEnd', "")]) -> do
                put (machine { traceMemoryStart = addrStr', traceMemoryEnd = addrEnd'})
                logMemoryRange addrStr' addrEnd'
                interactiveLoopHelper commandToExecute
              ([(addrStr', "")], _) -> do
                put (machine { traceMemoryStart = addrStr', traceMemoryEnd = addrStr' + 15})
                logMemoryRange addrStr' (addrStr' + 15)
                interactiveLoopHelper commandToExecute
              _ -> do
                liftIO $ putStrLn "Invalid address format."
                interactiveLoopHelper commandToExecute
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
\step/s: Execute one instruction cycle\n\
\regs/r: Show current register values\n\
\mem/m [addr] [end]: Show memory range (default: current trace range)\n\
\log: Log current memory range\n\
\q/quit: Exit debugger\n\
\trace: Toggle instruction tracing\n\
\addr-range <start> <end>: Set memory trace range\n\
\goto/g <addr>: Set program counter to address"
    case words commandToExecute of
      ["help"] -> handleHelp >> interactiveLoopHelper commandToExecute
      ["h"] -> handleHelp >> interactiveLoopHelper commandToExecute
      ["goto", addrStr] -> handleGoto addrStr
      ["g", addrStr] -> handleGoto addrStr
      ["step"] -> handleStep
      ["z"] -> handleStep
      ["regs"] -> handleRegs
      ["r"] -> handleRegs
      ["mem", addrStr, addrEnd] -> handleMem addrStr addrEnd
      ["m", addrStr, addrEnd] -> handleMem addrStr addrEnd
      ["mem", addrStr] -> handleMem addrStr "" 
      ["m", addrStr] -> handleMem addrStr "" 
      ["mem"]  -> handleMem "" "" 
      ["m"] -> handleMem "" "" 
      ["log"] -> do
        logMemoryRange (traceMemoryStart machine) (traceMemoryEnd machine)
        interactiveLoopHelper commandToExecute
      ["q"] -> return ()
      ["quit"] -> return ()
      ["trace"] -> handleTrace
      ["t"] -> handleTrace
      ["addr-range", startAddrStr, endAddrStr] -> do
        case (readHex startAddrStr, readHex endAddrStr) of
          ([(startAddr, "")], [(endAddr, "")]) -> do
            put (machine { traceMemoryStart = fromIntegral startAddr, traceMemoryEnd = fromIntegral endAddr })
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
