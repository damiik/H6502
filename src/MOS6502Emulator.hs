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
import Control.Monad.State (get, modify) -- Import get and modify
import Control.Monad (when, unless) -- Import the 'when' function
import Control.Monad.IO.Class (liftIO)
import Data.Word ( Word8, Word16 )
import Numeric (showHex)
import System.IO (hFlush, stdout)


fdxSingleCycle :: FDX Bool -- Returns True if emulation should continue, False if halted
fdxSingleCycle = do
  -- liftIO $ putStrLn ""
  machineState <- get
  when (enableTrace machineState) $ do
    logRegisters =<< getRegisters
    logMemoryRange 0xC000 0xC010 -- Or any other range  
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
  return Machine { mRegs = regs, mMem = mem, halted = False, instructionCount = 0, cycleCount = 0, enableTrace = False }

-- | The main fetch-decode-execute loop
runFDXLoop :: FDX ()
runFDXLoop = do
  continue <- fdxSingleCycle
  when continue runFDXLoop

-- | Runs the emulator until the machine is halted
runUntilHalted :: FDX ()
runUntilHalted = do
  continue <- fdxSingleCycle
  when continue runUntilHalted

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


-- | Runs the emulator with the given starting address and bytecode, then enters debugger mode
runDebugger :: Word16 -> Word16 -> [Word8] -> IO ()
runDebugger startAddress actualLoadAddress byteCode = do
  putStrLn $ "Initializing debugger with code starting at $" ++ showHex startAddress ""
  putStrLn $ "Loading bytecode at $" ++ showHex actualLoadAddress ""
  initialMachine <- newMachine

  -- Prepare memory writes - load bytecode at its actual load address
  let memoryWrites = zip [actualLoadAddress..] byteCode

  -- Setup the machine
  setupMachine initialMachine memoryWrites >>= \setupResult -> do
    putStrLn "Emulator machine setup complete. Running emulation until BRK..."

    -- Run the emulator until halted
    (_, machineAfterEmulation) <- runMachine runUntilHalted setupResult { mRegs = (mRegs setupResult) { rPC = startAddress } }

    putStrLn "\n--- Emulation Halted ---"
    putStrLn $ "Final Registers: " ++ show (mRegs machineAfterEmulation)
    putStrLn $ "Instructions Executed: " ++ show (instructionCount machineAfterEmulation)

    putStrLn "\nEntering interactive debugger."
    -- Enter interactive debugger loop with the final machine state
    _ <- runMachine interactiveDebuggerLoop machineAfterEmulation
    return ()


interactiveDebuggerLoop :: FDX ()
interactiveDebuggerLoop = do
  machine <- get
  unless (halted machine) $ do
    cmd <- liftIO $ prompt "> "
    case cmd of
      "step" -> fdxSingleCycle >> interactiveDebuggerLoop
      "regs" -> (logRegisters =<< getRegisters) >> interactiveDebuggerLoop
      "mem"  -> logMemoryRange 0x0000 0x00FF >> interactiveDebuggerLoop
      "quit" -> return ()
      _      -> interactiveDebuggerLoop

prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine
