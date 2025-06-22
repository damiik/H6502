-- | Defines core emulator functions that are not specific to the debugger or machine state.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveTraversable#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MOS6502Emulator.Core (Machine (..), FDX (..), getRegisters, setRegisters, getMemory, setMemory, fetchByteMem, fetchWordMem, writeByteMem, fetchByteMemPure, fetchWordMemPure, writeByteMemPure, mkWord, toWord, parseHexByte, parseHexWord, DebuggerMode (..), AddressMode (..)) where

import Control.Monad.Trans.State (StateT)
import Control.Monad.State(MonadState, get, put)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map -- Added for labelMap
import Data.Word (Word8, Word16)
import Data.Bits (shiftL)

import MOS6502Emulator.Registers (Registers(..))
import qualified MOS6502Emulator.Memory as Mem
import MOS6502Emulator.Debugger.VimMode.Core (VimState) -- Import VimState type
import MOS6502Emulator.Debugger.Core (DebuggerConsoleState, initialConsoleState, DebuggerMode(..), DebuggerAction(..)) -- Import from new Types module
import Numeric (readHex)
import Control.Lens (view)

-- | Represents the addressing modes of the 6502.
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

-- | Creates a 16-bit Word from a low byte and a high byte.
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

-- | Converts a Word8 to a Word16.
toWord :: Word8 -> Word16
toWord = fromIntegral

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


-- | Represents the state of the MOS 6502 machine.
data Machine = Machine
  { _mRegs            :: Registers -- ^ The machine's registers.
  , _mMem             :: Mem.Memory -- ^ The machine's memory.
  , _halted           :: Bool -- ^ Indicates if the machine is halted.
  , _instructionCount :: Int -- ^ The number of instructions executed.
  , _cycleCount       :: Int -- ^ The number of cycles executed.
  , _enableTrace      :: Bool -- ^ Indicates if instruction tracing is enabled.
  , _traceMemoryStart :: Word16 -- ^ The start address for memory tracing (for backward compatibility or single range).
  , _traceMemoryEnd   :: Word16   -- ^ The end address for memory tracing (for backward compatibility or single range).
  , _breakpoints      :: [Word16] -- ^ A list of breakpoint addresses.
  , _debuggerActive   :: Bool     -- ^ Indicates if the debugger is active.
  , _memoryTraceBlocks :: [(Word16, Word16, Maybe String)] -- ^ A list of memory ranges to trace, with optional names.
  , _lastDisassembledAddr :: Word16 -- ^ The address of the last disassembled instruction.
  , _labelMap             :: Map.Map Word16 String -- ^ A map from addresses to labels.
  , _debugLogPath         :: Maybe FilePath -- ^ The path for debugger state persistence.
  , _debuggerMode         :: DebuggerMode -- ^ The current mode of the debugger.
  , _pcHistory            :: [Word16] -- ^ History of the Program Counter for stepping back.
  , _redoHistory          :: [Word16] -- ^ History of the Program Counter for stepping forward (after stepping back).
  , _storedAddresses      :: Map.Map Char Word16 -- ^ A map of named stored addresses.
  , _vimState             :: VimState -- New field
  , _mConsoleState        :: DebuggerConsoleState -- ^ The state of the debugger console.
  }

newtype FDX a = FDX { unFDX :: StateT Machine IO a }
  deriving (Functor, Monad, Applicative)

-- | MonadState instance for the FDX monad.
instance MonadState Machine FDX where
  get = FDX get
  put m = FDX (put m)

-- | MonadIO instance for the FDX monad.
instance MonadIO FDX where
  liftIO = FDX . liftIO


-- | Fetches a byte from the provided address in memory.
fetchByteMem :: Word16 -> FDX Word8
fetchByteMem addr = do
  mem <- getMemory
  Mem.fetchByte addr mem

-- | Fetches a word (16 bits) located at an address
-- stored in the zero page. That means
-- we only need an 8bit address, but we
-- also read address+1
fetchWordMem :: Word8 -> FDX Word16
fetchWordMem addr = do
  mem <- getMemory
  lo  <- Mem.fetchByte (toWord addr)     mem
  hi  <- Mem.fetchByte (toWord (addr+1)) mem
  return $! mkWord lo hi

-- | Writes a byte to the provided address in memory.
writeByteMem :: Word16 -> Word8 -> FDX ()
writeByteMem addr b = do
  mem <- getMemory
  Mem.writeByte addr b mem

-- | Fetches a byte from the provided address in memory (pure version).
fetchByteMemPure :: Word16 -> Machine -> Word8
fetchByteMemPure addr machine = Mem.fetchBytePure addr (_mMem machine)

-- | Fetches a word (16 bits) located at an address
-- stored in the zero page (pure version).
fetchWordMemPure :: Word8 -> Machine -> Word16
fetchWordMemPure addr machine =
  let mem = _mMem machine
      lo  = Mem.fetchBytePure (toWord addr) mem
      hi  = Mem.fetchBytePure (toWord (addr+1)) mem
  in mkWord lo hi

-- | Writes a byte to the provided address in memory (pure version).
writeByteMemPure :: Word16 -> Word8 -> Machine -> Machine
writeByteMemPure addr b machine =
  machine { _mMem = Mem.writeBytePure addr b (_mMem machine) }


-- | Gets the current memory state.
getMemory :: FDX Mem.Memory
getMemory = _mMem <$> get

-- | Sets the current memory state.
setMemory :: Mem.Memory -> FDX ()
setMemory mem = do
  m <- get
  put (m { _mMem = mem })

-- | Gets the current register state.
getRegisters :: FDX Registers
getRegisters = do
  m <- get
  return (_mRegs m)

-- | Sets the current register state.
setRegisters :: Registers -> FDX ()
setRegisters rs = do
  m <- get
  put ( m { _mRegs = rs } )
