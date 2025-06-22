module MOS6502Emulator.Debugger.Utils
  ( parseHexWord
  , parseHexByte
  , getRegisters
  , formatStatusFlags
  , parseDebuggerCommand
  ) where

import Numeric (readHex, showHex)
import Data.Word (Word16, Word8)
import Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import Control.Monad.State (get, modify)
import Data.List (stripPrefix)
import Data.Bits (testBit)

import MOS6502Emulator.Core (FDX)
import MOS6502Emulator.Debugger.Core (DebuggerCommand(..))
import MOS6502Emulator.Machine (Machine(_mRegs))
import MOS6502Emulator.Registers (Registers(..))
import MOS6502Emulator.DissAssembler (formatHex8, formatHex16)

-- Helper to parse a hex string into a Word16.
parseHexWord :: String -> Maybe Word16
parseHexWord s = case readHex (stripHexPrefix s) of
    [(val, "")] -> Just val
    _           -> Nothing
  where
    stripHexPrefix :: String -> String
    stripHexPrefix str = fromMaybe str (stripPrefix "0x" str)

-- Helper to parse a hex string into a Word8.
parseHexByte :: String -> Maybe Word8
parseHexByte s = case readHex (stripHexPrefix s) of
    [(val, "")] -> Just val
    _           -> Nothing
  where
    stripHexPrefix :: String -> String
    stripHexPrefix str = fromMaybe str (stripPrefix "0x" str)


-- | Gets the current register values from the machine state.
getRegisters :: FDX Registers
getRegisters = do
  machine <- get
  return (_mRegs machine)

formatStatusFlags :: Word8 -> String
formatStatusFlags sr =
  let getFlagBit r b = if testBit r b then '*' else ' '
  in [' ', getFlagBit sr 7, 'N', ' ', getFlagBit sr 6, 'V', ' ', '-', '-', getFlagBit sr 4, 'B', ' ', getFlagBit sr 3, 'D', ' ', getFlagBit sr 2, 'I', ' ', getFlagBit sr 1, 'Z', ' ', getFlagBit sr 0, 'C']

-- | Parse user input string into a DebuggerCommand
parseDebuggerCommand :: String -> DebuggerCommand
parseDebuggerCommand input = 
  let tokens = words input
  in case tokens of
       ["step"] -> Step
       ["z"] -> Step
       ["bk"] -> Break Nothing
       ["break"] -> Break Nothing
       ["bk", addrStr] -> Break (parseHexWord addrStr)
       ["break", addrStr] -> Break (parseHexWord addrStr)
       ["mem"] -> MemTrace Nothing
       ["m"] -> MemTrace Nothing
       ["mem", startStr, endStr] -> 
         case (parseHexWord startStr, parseHexWord endStr) of
           (Just start, Just end) -> MemTrace (Just (start, end, Nothing))
           _ -> Unknown input
       ["m", startStr, endStr] -> 
         case (parseHexWord startStr, parseHexWord endStr) of
           (Just start, Just end) -> MemTrace (Just (start, end, Nothing))
           _ -> Unknown input
       ("mem":startStr:endStr:nameWords) -> 
         case (parseHexWord startStr, parseHexWord endStr) of
           (Just start, Just end) -> MemTrace (Just (start, end, Just (unwords nameWords)))
           _ -> Unknown input
       ("m":startStr:endStr:nameWords) -> 
         case (parseHexWord startStr, parseHexWord endStr) of
           (Just start, Just end) -> MemTrace (Just (start, end, Just (unwords nameWords)))
           _ -> Unknown input
       ("fill":startStr:endStr:byteStrs) ->
         case (parseHexWord startStr, parseHexWord endStr) of
           (Just start, Just end) -> Fill start end (mapMaybe parseHexByte byteStrs)
           _ -> Unknown input
       ("f":startStr:endStr:byteStrs) ->
         case (parseHexWord startStr, parseHexWord endStr) of
           (Just start, Just end) -> Fill start end (mapMaybe parseHexByte byteStrs)
           _ -> Unknown input
       ["ra", valStr] -> 
         case parseHexByte valStr of
           Just val -> SetReg8 "Accumulator" val
           Nothing -> Unknown input
       ["rx", valStr] -> 
         case parseHexByte valStr of
           Just val -> SetReg8 "X Register" val
           Nothing -> Unknown input
       ["ry", valStr] -> 
         case parseHexByte valStr of
           Just val -> SetReg8 "Y Register" val
           Nothing -> Unknown input
       ["rsp", valStr] -> 
         case parseHexByte valStr of
           Just val -> SetReg8 "Stack Pointer" val
           Nothing -> Unknown input
       ["rsr", valStr] -> 
         case parseHexByte valStr of
           Just val -> SetReg8 "Status Register" val
           Nothing -> Unknown input
       ["rpc", valStr] -> 
         case parseHexWord valStr of
           Just val -> SetPC val
           Nothing -> Unknown input
       ["d"] -> Disassemble Nothing
       ["d", addrStr] -> Disassemble (parseHexWord addrStr)
       ["regs"] -> Regs
       ["r"] -> Regs
       ["trace"] -> Trace
       ["t"] -> Trace
       ["goto", addrStr] -> 
         case parseHexWord addrStr of
           Just addr -> Goto addr
           Nothing -> Unknown input
       ["g", addrStr] -> 
         case parseHexWord addrStr of
           Just addr -> Goto addr
           Nothing -> Unknown input
       ["quit"] -> Quit
       ["q"] -> Quit
       ["exit"] -> Exit
       ["x"] -> Exit
       _ -> Unknown input