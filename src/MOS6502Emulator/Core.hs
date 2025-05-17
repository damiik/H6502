-- | Defines core emulator functions that are not specific to the debugger or machine state.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
module MOS6502Emulator.Core where

import MOS6502Emulator.Machine (FDX, Machine(..), getRegisters, instructionCount, cycleCount, setRegisters, getMemory, setMemory, fetchByteMem, fetchWordMem, writeByteMem, mkWord, toWord, loadSymbolFile, setPC_, setAC_, setX_, setY_, setSR_, setSP_, writeByteMem_, DebuggerMode(..))
import MOS6502Emulator.Instructions (execute)
import MOS6502Emulator.DissAssembler (disassembleInstruction)
import Control.Monad (when)
import Control.Monad.State (get, modify', gets)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word8, Word16)
import Data.Functor ((<&>))
import Numeric (showHex)
import MOS6502Emulator.Registers (rPC)

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
    pc <- fmap rPC getRegisters  -- Get current PC
    -- pc <- getRegisters >>= return . rPC  -- Get current PC
    let currentPC = pc -- Store PC before incrementing
    b <- fetchByteMem pc -- Fetch opcode byte at PC
    setPC_ (pc + 1)   -- Move PC to next byte (like a real 6502)
    modify' (\s -> s { instructionCount = instructionCount s + 1 })
    execute b
    when (enableTrace machineState) $ do
      disassembled <- disassembleInstruction currentPC -- Use the stored PC
      liftIO $ putStrLn ""
      liftIO $ putStrLn (fst disassembled)
    gets (not . halted)
