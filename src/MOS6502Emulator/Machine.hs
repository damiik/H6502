{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}

module MOS6502Emulator.Machine
(Machine(..)
 ,AddressMode(..)
 ,runMachine

,FDX (..)

,getRegisters
,instructionCount
,cycleCount
,setRegisters
,getMemory
,setMemory
,fetchByteMem
,fetchWordMem
,writeByteMem
,mkWord
,toWord

) where

-- import MonadLib
-- import MonadLib.Derive

import MOS6502Emulator.Memory (Memory)
import qualified MOS6502Emulator.Memory as Mem
import MOS6502Emulator.Registers (Registers, rPC) -- Import rPC
import Control.Monad.Trans.Class (lift)  -- Import lift
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, put)
import Numeric (showHex) -- Import showHex
import Data.Word (Word8, Word16)
import Data.Bits (shiftL)


-- In this context, "Word" in an identifier name
-- means a machine word on the 6502 which is 16 bits.
-- Not to be confused with the Haskell Word type
mkWord :: Word8  -- ^ low byte
       -> Word8  -- ^ high byte
       -> Word16
mkWord !lb !hb = (hw `shiftL` 8) + lw
  where
  lw = toWord lb
  hw = toWord hb

toWord :: Word8 -> Word16
toWord = fromIntegral



data Machine = Machine
  { mRegs            :: Registers
  , mMem             :: Memory
  , halted           :: Bool
  , instructionCount :: Int
  , cycleCount       :: Int
  , enableTrace      :: Bool
  , traceMemoryStart :: Word16
  , traceMemoryEnd   :: Word16
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


-- | Fetches a byte from the provided
-- address.
fetchByteMem :: Word16 -> FDX Word8
fetchByteMem addr = do
  mem <- getMemory
  Mem.fetchByte addr mem

-- | Fetches a word located at an address
-- stored in the zero page. That means
-- we only need an 8bit address, but we
-- also read address+1
fetchWordMem :: Word8 -> FDX Word16
fetchWordMem addr = do
  mem <- getMemory
  lo  <- Mem.fetchByte (toWord addr)     mem
  hi  <- Mem.fetchByte (toWord (addr+1)) mem
  return $! mkWord lo hi

writeByteMem :: Word16 -> Word8 -> FDX ()
writeByteMem addr b = do
  mem <- getMemory
  Mem.writeByte addr b mem
