-- | Defines the memory model for the MOS 6502 emulator.
{-# LANGUAGE FlexibleContexts #-}
module MOS6502Emulator.Memory where

import Prelude hiding ( replicate, read )
import Data.Vector.Unboxed.Mutable (MVector, replicate, read, write)
import Control.Monad.Primitive (PrimState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word

-- | Represents the 6502 memory as a mutable vector of Word8.
type Memory = MVector (PrimState IO) Word8

-- | The total size of the 6502 address space (64KB).
addressSize :: Int
addressSize = 2^(16::Int)

-- | Creates a new memory instance, initialized to zeros.
memory :: IO Memory
memory = replicate addressSize 0

-- | Fetches a byte from memory at the given address.
fetchByte :: MonadIO m => Word16 -> Memory -> m Word8
fetchByte addr m = liftIO $ read m (fromIntegral addr)

-- | Writes a byte to memory at the given address.
writeByte :: MonadIO m => Word16 -> Word8 -> Memory -> m ()
writeByte addr b m = liftIO $ write m (fromIntegral addr) b
