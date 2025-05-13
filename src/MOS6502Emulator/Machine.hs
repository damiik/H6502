-- | Defines the core types and state for the MOS 6502 emulator machine.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}

module MOS6502Emulator.Machine
(Machine(..)
 ,AddressMode(..)

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
,loadSymbolFile -- Export new function
) where

-- import MonadLib
-- import MonadLib.Derive

import MOS6502Emulator.Memory (Memory)
import qualified MOS6502Emulator.Memory as Mem
import MOS6502Emulator.Registers (Registers, rPC) -- Import rPC
import Control.Monad.Trans.Class (lift)  -- Import lift
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, put, modify') -- Added modify'
import Numeric (showHex, readHex) -- Import showHex and readHex
import Data.Word (Word8, Word16)
import Data.Bits (shiftL)
import qualified Data.Map.Strict as Map -- Added for labelMap
import System.IO (readFile) -- For reading the symbol file
import Control.Exception (try, IOException) -- For error handling


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


-- | Represents the state of the MOS 6502 machine.
data Machine = Machine
  { mRegs            :: Registers -- ^ The machine's registers.
  , mMem             :: Memory -- ^ The machine's memory.
  , halted           :: Bool -- ^ Indicates if the machine is halted.
  , instructionCount :: Int -- ^ The number of instructions executed.
  , cycleCount       :: Int -- ^ The number of cycles executed.
  , enableTrace      :: Bool -- ^ Indicates if instruction tracing is enabled.
  , traceMemoryStart :: Word16 -- ^ The start address for memory tracing (for backward compatibility or single range).
  , traceMemoryEnd   :: Word16   -- ^ The end address for memory tracing (for backward compatibility or single range).
  , breakpoints      :: [Word16] -- ^ A list of breakpoint addresses.
  , debuggerActive   :: Bool     -- ^ Indicates if the debugger is active.
  , memoryTraceBlocks :: [(Word16, Word16, Maybe String)] -- ^ A list of memory ranges to trace, with optional names.
  , lastDisassembledAddr :: Word16 -- ^ The address of the last disassembled instruction.
  , labelMap             :: Map.Map Word16 String -- ^ A map from addresses to labels.
  , debugLogPath         :: Maybe FilePath -- ^ The path for debugger state persistence.
  }

-- | Represents the fetch-decode-execute monad for the emulator.
newtype FDX a = FDX { unFDX :: StateT Machine IO a }
  deriving (Functor, Monad, Applicative)

-- runMachine :: FDX a -> Machine -> IO (a, Machine)
-- runMachine f m = runStateT m (unFDX f)

-- isoFDX :: Iso (StateT Machine IO) FDX
-- isoFDX = Iso FDX unFDX

-- instance StateM FDX Machine where
--   get = derive_get isoFDX
--   set = derive_set isoFDX
-- MonadState instance for FDX

-- | MonadState instance for the FDX monad.
instance MonadState Machine FDX where
  get = FDX get
  put m = FDX (put m)

-- | MonadIO instance for the FDX monad.
instance MonadIO FDX where
  liftIO = FDX . liftIO

-- | Gets the current register state.
getRegisters :: FDX Registers
getRegisters = do
  m <- get
  return (mRegs m)

-- | Sets the current register state.
setRegisters :: Registers -> FDX ()
setRegisters rs = do
  m <- get
  put ( m { mRegs = rs } )

-- | Gets the current memory state.
getMemory :: FDX Memory
getMemory = do
  m <- get
  return (mMem m)

-- | Sets the current memory state.
setMemory :: Memory -> FDX ()
setMemory mem = do
  m <- get
  put (m { mMem = mem })

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

-- | Loads a symbol file into the Machine's labelMap.
-- The file format is expected to be lines of "address label".
loadSymbolFile :: FilePath -> FDX ()
loadSymbolFile filePath = do
    liftIO $ putStrLn $ "Attempting to load symbol file: " ++ filePath -- Debug message
    fileContentOrError <- liftIO $ try (System.IO.readFile filePath)
    case fileContentOrError of
        Left e -> liftIO $ putStrLn $ "Error loading symbol file: " ++ show (e :: IOException)
        Right fileContent -> do
            liftIO $ putStrLn "Symbol file read successfully. Parsing..." -- Debug message
            let ls = lines fileContent
            let parsedLabels = foldr parseLine Map.empty ls
            modify' $ \m -> m { labelMap = parsedLabels }
            liftIO $ putStrLn $ "Parsed " ++ show (Map.size parsedLabels) ++ " labels." -- Debug message
  where
    -- | Parses a single line from the symbol file into an address-label pair.
    parseLine :: String -> Map.Map Word16 String -> Map.Map Word16 String
    parseLine line acc =
        case words line of
            (addrStr:nameRest) | not (null nameRest) ->
                case readHex addrStr of
                    [(addr, "")] -> Map.insert addr (unwords nameRest) acc
                    _            -> acc -- Failed to parse address
            _ -> acc -- Line doesn't fit format
