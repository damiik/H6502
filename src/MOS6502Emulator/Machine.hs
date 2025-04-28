{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MOS6502Emulator.Machine
(Machine(..)
 ,AddressMode(..)
 ,runMachine
 ,halted
,FDX
,getRegisters
,instructionCount
,cycleCount
,setRegisters
,getMemory
,setMemory
) where

-- import MonadLib
-- import MonadLib.Derive

import MOS6502Emulator.Memory (Memory)
import MOS6502Emulator.Registers (Registers, rPC) -- Import rPC
import Control.Monad.Trans.Class (lift)  -- Import lift
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, put)
import Numeric (showHex) -- Import showHex

data Machine = Machine
  { mRegs   :: !Registers
  , mMem    :: !Memory
  , halted         :: !Bool
  , instructionCount :: !Int
  , cycleCount     :: !Int
  }

-- | FDX is fetch-decode-execute
newtype FDX a = FDX { unFDX :: StateT Machine IO a }
  deriving (Functor, Monad, Applicative)

-- runMachine :: FDX a -> Machine -> IO (a, Machine)
-- runMachine f m = runStateT m (unFDX f)

-- Run the FDX monad
runMachine :: FDX a -> Machine -> IO (a, Machine)
runMachine f initialMachine = do
  liftIO $ putStrLn $ "Initial PC in runMachine: $" ++ showHex (rPC (mRegs initialMachine)) ""
  runStateT (unFDX f) initialMachine

-- isoFDX :: Iso (StateT Machine IO) FDX
-- isoFDX = Iso FDX unFDX

-- instance StateM FDX Machine where
--   get = derive_get isoFDX
--   set = derive_set isoFDX
-- MonadState instance for FDX

-- instance MonadState Machine FDX where
--   get = FDX $ lift get
--   put m = FDX $ lift (put m)

instance MonadState Machine FDX where
  get = FDX get
  put m = FDX (put m)

instance MonadIO FDX where
  liftIO = FDX . liftIO

getRegisters :: FDX Registers
getRegisters = do
  m <- get
  return (mRegs m)

setRegisters :: Registers -> FDX ()
setRegisters rs = do
  m <- get
  put ( m { mRegs = rs } )

getMemory :: FDX Memory
getMemory = do
  m <- get
  return (mMem m)

setMemory :: Memory -> FDX ()
setMemory mem = do
  m <- get
  put (m { mMem = mem })

data AddressMode = 
  Immediate
  | Zeropage
  | ZeropageX
  | ZeropageY
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | IndirectX
  | IndirectY
  | Accumulator
  | X
  | Y
  | SP