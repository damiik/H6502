module Assembly.TestHelper (runAssemblyTest, runAssemblyDebugTest) where

import Test.Hspec
import Assembly.Core
import Assembly (runAssembler)
import MOS6502Emulator (runEmulator, runDebugger, newMachine, setupMachine, Machine(..), Memory, Registers) -- Import emulator functions and types
import MOS6502Emulator.Memory (fetchByte) -- Import fetchByte from Memory module
import MOS6502Emulator.Registers (Registers(..), SRFlag(..), lookupSRFlag)
import Data.Word (Word8, Word16)
import Data.ByteString (ByteString)
import Data.Map.Strict as Map
import Assembly.Core (ProgramCounter, Label) -- Import ProgramCounter and Label

-- Helper function to assemble Asm, run in emulator, and return final state and label map
runAssemblyTest :: Word16 -> Asm () -> IO (Machine, Map.Map Label ProgramCounter)
runAssemblyTest initialPC asmBlock = do
  case runAssembler initialPC asmBlock of
    Left err -> fail $ "Assembly failed: " ++ err
    Right (actualLoadAddress, byteCode, labelMap) -> do
      initialMachine <- newMachine
      let memoryWrites = zip [actualLoadAddress..] byteCode
      setupMachine initialMachine memoryWrites Nothing Nothing >>= \setupResult -> do
        (_, finalMachine) <- runEmulator actualLoadAddress setupResult
        return (finalMachine, labelMap)

-- Helper function to assemble Asm, run in debugger, and return final state
runAssemblyDebugTest :: Word16 -> Asm () -> IO (Machine, Map.Map Label ProgramCounter)
runAssemblyDebugTest initialPC asmBlock = do
  case runAssembler initialPC asmBlock of
    Left err -> fail $ "Assembly failed: " ++ err
    Right (actualLoadAddress, byteCode, labelMap) -> do
      -- runDebugger takes startAddress, actualLoadAddress, byteCode, maybeSymPath
      finalMachine <- runDebugger initialPC actualLoadAddress byteCode Nothing
      return (finalMachine, labelMap)

