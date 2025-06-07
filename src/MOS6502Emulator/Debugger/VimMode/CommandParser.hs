module MOS6502Emulator.Debugger.VimMode.CommandParser ( parseVimCommand ) where

import Data.Word (Word8, Word16)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Numeric (readHex)
import Data.List (stripPrefix, isPrefixOf)
import MOS6502Emulator.Debugger.VimMode.Core (VimCommand(..))
import MOS6502Emulator.Debugger.Utils (parseHexWord, parseHexByte)
import Text.Read (readMaybe)

parseVimCommand :: String -> VimCommand
parseVimCommand cmdStr =
    let tokens = words (dropWhile (==':') cmdStr) -- Remove leading ':'
    in case tokens of
        ["break"] -> VBreak Nothing
        ["break", addrStr] -> VBreak (parseHexWord addrStr)
        ["watch"] -> VWatch Nothing
        ["watch", rangeStr] ->
            case span (/='-') rangeStr of
                (startStr, '-' : endStr) ->
                    case (parseHexWord startStr, parseHexWord endStr) of
                        (Just startAddr, Just endAddr) -> VWatch (Just (startAddr, endAddr))
                        _ -> VUnknown cmdStr
                _ -> VUnknown cmdStr
        ["step"] -> VStep (Just 1)
        ["step", countStr] -> VStep (readMaybe countStr)
        ["regs"] -> VRegs
        ["disas"] -> VDisas Nothing Nothing
        ["disas", startStr] -> VDisas (parseHexWord startStr) Nothing
        ["disas", startStr, endStr] -> VDisas (parseHexWord startStr) (parseHexWord endStr)
        ("fill":startStr:endStr:byteStrs) ->
            case (parseHexWord startStr, parseHexWord endStr) of
                (Just startAddr, Just endAddr) ->
                    VFill startAddr endAddr (mapMaybe parseHexByte byteStrs)
                _ -> VUnknown cmdStr
        ["ra", valStr] -> VSetReg8 'A' (fromMaybe 0 (parseHexByte valStr))
        ["rx", valStr] -> VSetReg8 'X' (fromMaybe 0 (parseHexByte valStr))
        ["ry", valStr] -> VSetReg8 'Y' (fromMaybe 0 (parseHexByte valStr))
        ["rsp", valStr] -> VSetReg8 'S' (fromMaybe 0 (parseHexByte valStr))
        ["rsr", valStr] -> VSetReg8 'P' (fromMaybe 0 (parseHexByte valStr))
        ["rpc", valStr] -> VSetPC (fromMaybe 0 (parseHexWord valStr))
        ["quit"] -> VQuit
        ["q"] -> VQuit
        ["exit"] -> VExit
        ["x"] -> VExit
        ["trace"] -> VTrace
        ["t"] -> VTrace
        _ -> VUnknown cmdStr