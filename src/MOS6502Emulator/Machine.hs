{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Defines the core types and state for the MOS 6502 emulator machine.
module MOS6502Emulator.Machine
(

 DebuggerAction(ContinueLoop, ExecuteStep, ExitDebugger, QuitEmulator, NoAction, SwitchToVimMode, SwitchToCommandMode) -- Export DebuggerAction constructors
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
import Data.Bits (shiftL)
import qualified Data.Map.Strict as Map -- Added for labelMap
import System.IO (readFile) -- For reading the symbol file
import Control.Exception (try, IOException) -- For error handling

import MOS6502Emulator.Instructions (execute)
import MOS6502Emulator.Memory (Memory)
import qualified MOS6502Emulator.Memory as Mem
import MOS6502Emulator.Registers (Registers, rPC, rAC, rSP, rSR, rX, rY ) -- Import rPC
import MOS6502Emulator.Core
import MOS6502Emulator.Debugger.VimModeCore (VimState) -- Import VimState type
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



-- | Sets the Program Counter register.
setPC_ :: Word16 -> FDX ()
setPC_ val = modify' $ \m -> m { mRegs = (mRegs m) { rPC = val } }

-- | Sets the Accumulator register.
setAC_ :: Word8 -> FDX ()
setAC_ val = modify' $ \m -> m { mRegs = (mRegs m) { rAC = val } }

-- | Sets the X register.
setX_ :: Word8 -> FDX ()
setX_ val = modify' $ \m -> m { mRegs = (mRegs m) { rX = val } }

-- | Sets the Y register.
setY_ :: Word8 -> FDX ()
setY_ val = modify' $ \m -> m { mRegs = (mRegs m) { rY = val } }

-- | Sets the Status Register.
setSR_ :: Word8 -> FDX ()
setSR_ val = modify' $ \m -> m { mRegs = (mRegs m) { rSR = val } }

-- | Sets the Stack Pointer register.
setSP_ :: Word8 -> FDX ()
setSP_ val = modify' $ \m -> m { mRegs = (mRegs m) { rSP = val } }

-- | Writes a byte to the provided address in memory.
-- This is a direct state modification function, distinct from instruction-based writes.
writeByteMem_ :: Word16 -> Word8 -> FDX ()
writeByteMem_ addr b = modify' $ \m -> m { mMem = Mem.writeBytePure addr b (mMem m) }

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