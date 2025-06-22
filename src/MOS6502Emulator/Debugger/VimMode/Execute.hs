module MOS6502Emulator.Debugger.VimMode.Execute
    ( executeAction
    , executeMotion
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Word (Word8, Word16)
import Data.Bits (testBit, clearBit, setBit)
import Data.List (findIndex)
import Data.Maybe (mapMaybe)
import Numeric (showHex)
import Control.Monad (foldM)
import Control.Monad.State (get, put)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout, stdin, hSetEcho)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

import MOS6502Emulator.Debugger.Commands (handleBreak, handleMemTrace, handleFill, handleSetReg8, handleSetPC, handleDisassemble) -- Explicit imports
import MOS6502Emulator.Debugger.Utils (getRegisters, parseHexByte) -- For VRegs and getRegisters
import MOS6502Emulator.Debugger.Console (getInput, termHeight)
import MOS6502Emulator.Debugger.VimMode.Core(Action(..), Motion(..), ViewMode(..), VimState (..), VimCommand(..))
import qualified MOS6502Emulator.Debugger.VimMode.Core as VM
import MOS6502Emulator.Core (FDX, fetchByteMem, writeByteMem) 
import MOS6502Emulator.DissAssembler (disassembleInstruction, InstructionInfo(..), disassembleInstructions, opcodeMap, formatHex16)
import Control.Lens
import MOS6502Emulator.Lenses
import MOS6502Emulator.Registers (_rAC, _rX, _rY, _rSP, _rSR, _rPC) -- Import for register setters
import MOS6502Emulator.Machine
import MOS6502Emulator.Debugger.Actions (logRegisters) -- Import executeStepAndRender and logging functions

executeVimCommand :: VimCommand -> FDX (DebuggerAction, [String])
executeVimCommand cmd = do
  case cmd of
    VBreak addr -> handleBreak (maybe [] (\a -> [formatHex16 a]) addr) ""
    VWatch (Just (startAddr, endAddr)) -> handleMemTrace [formatHex16 startAddr, formatHex16 endAddr] ""
    VWatch Nothing -> handleMemTrace [] "" -- List trace blocks
    VStep count -> return (ExecuteStep (show (fromMaybe 1 count)), [])
    VRegs -> do
      regs <- getRegisters
      return (NoAction, logRegisters regs)
    VDisas start end -> handleDisassemble (mapMaybe (fmap formatHex16) [start, end]) ""
    VFill start end bytes -> handleFill (map formatHex16 [start, end] ++ map (printf "%02X") bytes) ""
    VSetReg8 regChar val ->
        let regLens = case regChar of
                          'A' -> mRegs . rAC
                          'X' -> mRegs . rX
                          'Y' -> mRegs . rY
                          'S' -> mRegs . rSP
                          'P' -> mRegs . rSR
                          _   -> error "Invalid register character" -- Should not happen with proper parsing
        in handleSetReg8 regLens (printf "%02X" val) [regChar] ""
    VSetPC addr -> handleSetPC (formatHex16 addr) ""
    VQuit -> return (QuitEmulator, [])
    VExit -> return (ExitDebugger, [])
    VTrace -> do
      enableTrace %= not
      newTraceState <- use enableTrace
      let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
      return (NoAction, output)
    VUnknown cmdStr -> return (NoAction, ["Unknown command: " ++ cmdStr])

-- Update executeAction to handle ColonCommand
executeAction :: Action -> Word16 -> VimState -> FDX (Word16, [String])
executeAction action currentPos vimState = do
  case action of
    Set newByte -> do
      writeByteMem currentPos newByte
      return (currentPos, ["Set $" ++ showHex currentPos "" ++ " = $" ++ showHex newByte ""])
    
    Increment n -> do
      currentByte <- fetchByteMem currentPos
      let newByte = currentByte + fromIntegral n
      writeByteMem currentPos newByte
      return (currentPos, ["Incremented $" ++ showHex currentPos "" ++ " by " ++ show n])
    
    Decrement n -> do
      currentByte <- fetchByteMem currentPos
      let newByte = currentByte - fromIntegral n
      writeByteMem currentPos newByte
      return (currentPos, ["Decremented $" ++ showHex currentPos "" ++ " by " ++ show n])
    
    ToggleBit bit -> do
      if bit < 0 || bit > 7
        then return (currentPos, ["Invalid bit number (0-7)"])
        else do
          currentByte <- fetchByteMem currentPos
          let newByte = if testBit currentByte bit then clearBit currentByte bit else setBit currentByte bit
          writeByteMem currentPos newByte
          return (currentPos, ["Toggled bit " ++ show bit ++ " at $" ++ showHex currentPos ""])
    
    AddBreakpoint -> do
      breakpoints %= (currentPos :)
      return (currentPos, ["Breakpoint added at $" ++ showHex currentPos ""])
    
    RemoveBreakpoint -> do
      breakpoints %= filter (/= currentPos)
      return (currentPos, ["Breakpoint removed at $" ++ showHex currentPos ""])
    
    Delete motion -> do
      endPos <- executeMotion motion currentPos vimState
      let start = min currentPos endPos
      let end = max currentPos endPos
      mapM_ (`writeByteMem` 0) [start .. end]
      return (currentPos, ["Zeroed memory from $" ++ showHex start "" ++ " to $" ++ showHex end ""])
    
    Change motion -> do
      endPos <- executeMotion motion currentPos vimState
      let start = min currentPos endPos
      _ <- liftIO $ putStr "Enter bytes (hex): " >> hFlush stdout
      liftIO $ putStr "Enter bytes (hex): " >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let byteValues = mapMaybe parseHexByte (words input)
      if null byteValues
        then return (currentPos, ["No valid bytes provided"])
        else do
          let fillBytes = take (fromIntegral (endPos - start + 1)) (cycle byteValues)
          mapM_ (uncurry writeByteMem) (zip [start .. endPos] fillBytes)
          return (currentPos, ["Changed memory from $" ++ showHex start "" ++ " to $" ++ showHex endPos ""])
    
    Yank motion -> do
      endPos <- executeMotion motion currentPos vimState
      let start = min currentPos endPos
      let end = max currentPos endPos
      bytes <- mapM fetchByteMem [start .. end]
      -- The original code did not store yanked bytes in VimState.
      -- If this needs to be a state update, the function signature needs to change.
      return (currentPos, ["Yanked " ++ show (length bytes) ++ " bytes from $" ++ showHex start "" ++ " to $" ++ showHex end ""])
    
    Paste forward -> do
      let yankBuffer = Map.lookup (vsRegister vimState) (vsYankBuffer vimState)
      case yankBuffer of
        Nothing -> return (currentPos, ["No data in yank buffer"])
        Just bytes -> do
          let pastePos = if forward then currentPos + 1 else currentPos
          mapM_ (\(offset, val) -> writeByteMem (pastePos + fromIntegral offset) val) (zip [0..] bytes)
          return (currentPos, ["Pasted " ++ show (length bytes) ++ " bytes at $" ++ showHex pastePos ""])
    
    ExecuteToHere -> do
      breakpoints %= (currentPos :)
      return (currentPos, ["Temporary breakpoint added at $" ++ showHex currentPos "", "Continuing execution"])
    
    ColonCommand vimCmd -> do
        (dbgAction, output) <- executeVimCommand vimCmd
        -- Need to handle dbgAction potentially changing the debugger mode or state
        -- For now, just pass it through or convert to NoAction if it means staying in VimMode
        return (currentPos, output)

    _ -> return (currentPos, ["Unknown action"])

-- | Execute a motion and return new cursor position
executeMotion :: Motion -> Word16 -> VimState -> FDX Word16
executeMotion motion currentPos vimState = do
  let maxAddr = 0xFFFF -- Maximum address for 6502
  case motion of
    NextInstruction n -> do
      foldM (\pos _ -> do
          if pos >= maxAddr then return pos
          else do
            (_, instLen) <- disassembleInstruction pos
            return (pos + fromIntegral instLen)
        ) currentPos [1..n]  
    PrevInstruction n -> do
      -- Search backward to find instruction boundaries
      let searchBack pos count = if count <= 0 || pos == 0
            then return pos
            else do
              -- Try addresses slightly before to find a valid instruction start
              let candidates = [pos - fromIntegral i | i <- [1..3]] -- Instructions are 1-3 bytes
              validPos <- findValidInstructionStart candidates
              case validPos of
                Just prevPos -> searchBack prevPos (count - 1)
                Nothing -> return pos -- No valid instruction found, stay put
      searchBack currentPos n
    
    NextByte n -> return $ min maxAddr (currentPos + fromIntegral n)
    PrevByte n -> return $ max 0 (currentPos - fromIntegral n)
    
    GotoAddressMotion addr -> return $ min maxAddr addr
    GotoPC -> use (mRegs . rPC)
    
    WordForward n -> executeMotion (NextInstruction n) currentPos vimState
    WordBackward n -> executeMotion (PrevInstruction n) currentPos vimState
    
    EndOfPage -> do
      let linesPerPage = termHeight - 3 -- Reserve space for status and input
      case vsViewMode vimState of
        CodeView -> do
          -- Disassemble forward to find the address after linesPerPage instructions
          (_, finalAddr) <- disassembleInstructions currentPos linesPerPage
          return $ max 0 (finalAddr - 1)
        _ -> return $ min maxAddr (currentPos + fromIntegral (linesPerPage * 16)) -- 16 bytes per line in MemoryView
    
    TopOfPage -> return $ vsViewStart vimState
    
    MiddlePage -> do
      let linesPerPage = termHeight - 3
      let halfPage = linesPerPage `div` 2
      case vsViewMode vimState of
        CodeView -> do
          (_, finalAddr) <- disassembleInstructions (vsViewStart vimState) halfPage
          return $ max 0 (finalAddr - 1)
        _ -> return $ min maxAddr (vsViewStart vimState + fromIntegral (halfPage * 16))
    
    FindByte byte forward -> findByteInMemory currentPos byte forward
    
    TillByte byte forward -> do
      newPos <- findByteInMemory currentPos byte forward
      return $ if forward then max 0 (newPos - 1) else min maxAddr (newPos + 1)
    
    RepeatFind forward -> do
        case vsLastFind vimState of
          Just (byte, _) -> findByteInMemory currentPos byte forward
          Nothing -> return currentPos
    
    TextObject mod objType n -> do
        case vsViewMode vimState of
            CodeView -> do
              let currentPC = vsCursor vimState
              case objType of
                VM.Word -> do
                  precedingInsts <- disassembleInstructionsFromBeginning currentPC opcodeMap
                  let startOfCurrentInst = case reverse precedingInsts of
                                             ((addr, _, _):_) -> addr
                                             _ -> currentPC

                  (_, endAddr) <- disassembleInstructions startOfCurrentInst n
                  return (endAddr - 1)
                VM.Line -> do
                  precedingInsts <- disassembleInstructionsFromBeginning currentPC opcodeMap
                  let startOfCurrentInst = case reverse precedingInsts of
                                             ((addr, _, _):_) -> addr
                                             _ -> currentPC

                  (_, endAddr) <- disassembleInstructions startOfCurrentInst n
                  return (endAddr - 1)

                VM.Bracket -> do
                  (instStr, _) <- disassembleInstruction currentPC
                  let openBracketIndex = findIndex (`elem` "([") instStr
                  let closeBracketIndex = findIndex (`elem` ")]") instStr
                  case (openBracketIndex, closeBracketIndex) of
                    (Just open, Just close) | open < close ->
                      let startByte = currentPC + fromIntegral (open `div` 3)
                          endByte = currentPC + fromIntegral (close `div` 3)
                      in return endByte
                    _ -> return currentPC
                VM.Quote -> do
                  (instStr, _) <- disassembleInstruction currentPC
                  let openQuoteIndex = findIndex (=='"') instStr
                  let closeQuoteIndex = findIndex (=='"') (drop (fromMaybe 0 openQuoteIndex + 1) instStr)
                  case (openQuoteIndex, fmap (+ (fromMaybe 0 openQuoteIndex + 1)) closeQuoteIndex) of
                    (Just open, Just close) | open < close ->
                      let startByte = currentPC + fromIntegral (open `div` 3)
                          endByte = currentPC + fromIntegral (close `div` 3)
                      in return endByte
                    _ -> return currentPC
            MemoryView -> do
              case objType of
                VM.Word -> return $ min maxAddr (currentPos + fromIntegral (n * 2))
                VM.Line -> return $ min maxAddr (currentPos + fromIntegral (n * 16))
                VM.Bracket -> return currentPos
                VM.Quote -> return currentPos
            _ -> return currentPos
    _ -> return currentPos

-- Helper to find instruction start (needed for Word/Line)
disassembleInstructionsFromBeginning :: Word16 -> Map Word8 InstructionInfo -> FDX [(Word16, String, Int)]
disassembleInstructionsFromBeginning startAddr opMap = go startAddr []
  where
    go addr acc
      | addr > 0xFFFF = return $ reverse acc
      | otherwise = do
          byte <- fetchByteMem addr
          case Map.lookup byte opMap of
            Just instInfo -> do
              let nextAddr = addr + fromIntegral (size instInfo)
              go nextAddr ((addr, "", fromIntegral (size instInfo)) : acc)
            Nothing -> return $ reverse acc


-- | Find the first valid instruction start among candidate addresses
findValidInstructionStart :: [Word16] -> FDX (Maybe Word16)
findValidInstructionStart [] = return Nothing
findValidInstructionStart (addr:rest) = do
  opcode <- fetchByteMem addr
  case Map.lookup opcode (opcodeMap :: Map Word8 InstructionInfo) of -- Assume opcodeMap from DissAssembler.hs
    Just _ -> return (Just addr)
    Nothing -> findValidInstructionStart rest

-- | Find a byte in memory starting from position
findByteInMemory :: Word16 -> Word8 -> Bool -> FDX Word16
findByteInMemory startPos targetByte forward = do
  let maxAddr = 0xFFFF
  let searchRange = if forward
                    then [startPos + 1 .. maxAddr]
                    else reverse [0 .. startPos - 1]
  findFirst searchRange
  where
    findFirst [] = return startPos -- Not found
    findFirst (addr:rest) = do
      byte <- fetchByteMem addr
      if byte == targetByte
        then return addr
        else findFirst rest
