{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines the core types and state for the MOS 6502 emulator machine.
module MOS6502Emulator.Machine
( Machine(..) -- Export Machine type and its constructors/fields
, DebuggerAction(ExecuteStepAction, ExitDebuggerAction, QuitEmulatorAction, RenderScreenAction, UpdateConsoleOutputAction, SetDebuggerModeAction, NoDebuggerAction) -- Export DebuggerAction constructors
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
import MOS6502Emulator.DissAssembler (disassembleInstructionPure)


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
            L.labelMap .= parsedLabels
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
setPC_ :: Word16 -> Machine -> Machine
setPC_ val machine = machine & L.mRegs . L.rPC .~ val

-- | Sets the Accumulator register.
setAC_ :: Word8 -> Machine -> Machine
setAC_ val machine = machine & L.mRegs . L.rAC .~ val

-- | Sets the X register.
setX_ :: Word8 -> Machine -> Machine
setX_ val machine = machine & L.mRegs . L.rX .~ val

-- | Sets the Y register.
setY_ :: Word8 -> Machine -> Machine
setY_ val machine = machine & L.mRegs . L.rY .~ val

-- | Sets the Status Register.
setSR_ :: Word8 -> Machine -> Machine
setSR_ val machine = machine & L.mRegs . L.rSR .~ val


-- | Sets the Stack Pointer register.
setSP_ :: Word8 -> Machine -> Machine
setSP_ val machine = machine & L.mRegs . L.rSP .~ val

-- | Writes a byte to the provided address in memory.
-- This is a direct state modification function, distinct from instruction-based writes.
writeByteMem_ :: Word16 -> Word8 -> Machine -> Machine
writeByteMem_ addr b machine = machine & L.mMem %~ Mem.writeBytePure addr b

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
    _ <- modify' (setPC_ (pc + 1))   -- Move PC to next byte (like a real 6502)
    L.instructionCount %= (+ 1)
    execute b
    when (_enableTrace machineState) $ do
      let (disassembled, _) = disassembleInstructionPure currentPC machineState -- Use the stored PC and pass machineState
      liftIO $ putStrLn ""
      liftIO $ putStrLn disassembled
    L.lastDisassembledAddr .= currentPC -- Update last disassembled address using lens
    gets (not . _halted)
