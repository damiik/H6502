module MOS6502Emulator.Debugger.Utils
  ( parseHexWord
  , parseHexByte
  , getRegisters
  , formatStatusFlags
  ) where

import Numeric (readHex, showHex)
import Data.Word (Word16, Word8)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad.State (get, modify)
import Data.List (stripPrefix)
import Data.Bits (testBit)

import MOS6502Emulator.Core (FDX)
import MOS6502Emulator.Machine (Machine(_mRegs))
import MOS6502Emulator.Registers (Registers(_rPC, _rAC, _rX, _rY, _rSP, _rSR))
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
