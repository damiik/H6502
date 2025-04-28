{-# LANGUAGE FlexibleContexts #-}
module MOS6502Emulator.Memory where

import Prelude hiding ( replicate, read )
import Data.Vector.Unboxed.Mutable (MVector, replicate, read, write)
import Control.Monad.Primitive (PrimState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word


type Memory = MVector (PrimState IO) Word8

addressSize :: Int
addressSize = 2^(16::Int)

memory :: IO Memory
memory = replicate addressSize 0

fetchByte :: MonadIO m => Word16 -> Memory -> m Word8
fetchByte addr m = liftIO $ read m (fromIntegral addr)

writeByte :: MonadIO m => Word16 -> Word8 -> Memory -> m ()
writeByte addr b m = liftIO $ write m (fromIntegral addr) b