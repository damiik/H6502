{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines the core types and state for the MOS 6502 emulator machine.
module MOS6502Emulator.Machine
( Machine(..) -- Export Machine type and its constructors/fields
, DebuggerAction(ContinueLoop, ExecuteStep, ExitDebugger, QuitEmulator, NoAction, SwitchToVimMode, SwitchToCommandMode) -- Export DebuggerAction constructors
,rPC -- Export rPC field from Registers (needed in VimMode via Machine)
,mRegs -- Export mRegs field from Machine
,pcHistory -- Export pcHistory field from Machine
,redoHistory -- Export redoHistory field from Machine
,breakpoints -- Export breakpoints field from Machine
,memoryTraceBlocks -- Export memoryTraceBlocks field from Machine
,storedAddresses -- Export storedAddresses field from Machine
,lastDisassembledAddr -- Export lastDisassembledAddr field from Machine
,debuggerMode -- Export debuggerMode field from Machine
,fdxSingleCycle
,instructionCount
,loadSymbolFile
,setPC_
,setAC_
,setX_
,setY_
,setSR_
,setSP_
,writeByteMem_
) where

-- import MonadLib
-- import MonadLib.Derive

import Control.Monad.Trans.Class (lift)  -- Import lift
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.State (MonadState, get, gets, put, modify') -- Added modify'
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Numeric (showHex, readHex) -- Import showHex and readHex
import Data.Word (Word8, Word16)
import qualified Data.Map.Strict as Map -- Added for labelMap
import System.IO (readFile) -- For reading the symbol file
import Control.Exception (try, IOException) -- For error handling
import Control.Lens -- Import Control.Lens
import MOS6502Emulator.Lenses -- Import our custom lenses
import qualified MOS6502Emulator.Lenses as L -- Import all lenses qualified

import MOS6502Emulator.Instructions (execute, setSP)
import qualified MOS6502Emulator.Memory as Mem
import MOS6502Emulator.Registers (_rPC) -- Import rPC
import MOS6502Emulator.Core
import MOS6502Emulator.Debugger.Core (DebuggerAction(..)) -- Import DebuggerAction
import MOS6502Emulator.DissAssembler


-- runMachine :: FDX a -> Machine -> IO (a, Machine)
-- runMachine f m = runStateT m (unFDX f)

-- isoFDX :: Iso (StateT Machine IO) FDX
-- isoFDX = Iso FDX unFDX

-- instance StateM FDX Machine where
--   get = derive_get isoFDX
--   set = derive_set isoFDX
-- MonadState instance for FDX


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
            modify' $ \m -> m { _labelMap = parsedLabels }
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



-- | Sets the Program Counter register.
setPC_ :: Word16 -> FDX ()
setPC_ val = modify' (L.mRegs . L.rPC .~ val)

-- | Sets the Accumulator register.
setAC_ :: MonadState Machine m => Word8 -> m ()
setAC_ val = mRegs . rAC .= val

-- | Sets the X register.
setX_ :: MonadState Machine m => Word8 -> m ()
setX_ val = mRegs . rX .= val

-- | Sets the Y register.
setY_ :: MonadState Machine m => Word8 -> m ()
setY_ val = mRegs . rY .= val

-- | Sets the Status Register.
setSR_ :: MonadState Machine m => Word8 -> m ()
setSR_ val = mRegs . rSR .= val


-- | Sets the Stack Pointer register.
setSP_ :: MonadState Machine m => Word8 -> m () 
setSP_ val = mRegs . rSP .= val
-- | Fetches a byte from memory at the specified address.

-- | Writes a byte to the provided address in memory.
-- This is a direct state modification function, distinct from instruction-based writes.
writeByteMem_ :: Word16 -> Word8 -> FDX ()
writeByteMem_ addr b = modify' $ \m -> m { _mMem = Mem.writeBytePure addr b (_mMem m) }

-- | Performs a single fetch-decode-execute cycle of the 6502 emulator.
-- Returns `True` if emulation should continue, `False` if halted.
fdxSingleCycle :: FDX Bool -- Returns True if emulation should continue, False if halted
fdxSingleCycle = do
  -- liftIO $ putStrLn ""
  machineState <- get
  -- liftIO $ putStrLn $ "Current PC at start of fdxSingleCycle: $" ++ showHex (rPC (mRegs machineState)) ""
  if _halted machineState
    then return False -- Machine is halted, stop emulation
    else do
    pc <- fmap _rPC getRegisters  -- Get current PC
    -- pc <- getRegisters >>= return . rPC  -- Get current PC
    let currentPC = pc -- Store PC before incrementing
    b <- fetchByteMem pc -- Fetch opcode byte at PC
    setPC_ (pc + 1)   -- Move PC to next byte (like a real 6502)
    modify' (\s -> s { _instructionCount = _instructionCount s + 1 })
    execute b
    when (_enableTrace machineState) $ do
      disassembled <- disassembleInstruction currentPC -- Use the stored PC
      liftIO $ putStrLn ""
      liftIO $ putStrLn (fst disassembled)
    gets (not . _halted)
