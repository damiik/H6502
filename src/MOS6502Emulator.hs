module MOS6502Emulator
  ( runEmulator
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

import Control.Monad (when) -- Import the 'when' function

import Data.Word ( Word8, Word16 )

-- | Initializes a new 6502 machine state
newMachine :: IO Machine
newMachine = do
  mem <- memory  -- 64KB of memory initialized by MOS6502Emulator.Memory
  let regs = mkRegisters
  return Machine { mRegs = regs, mMem = mem, halted = False, instructionCount = 0, cycleCount = 0 }

-- | The main fetch-decode-execute loop
runFDXLoop :: FDX ()
runFDXLoop = do
  continue <- fdxSingleCycle
  when continue runFDXLoop

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

-- TODO: Add a function to load a program into memory