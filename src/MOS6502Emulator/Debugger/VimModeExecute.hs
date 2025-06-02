module MOS6502Emulator.Debugger.VimModeExecute
    ( executeAction
    , executeMotion
    ) where

import qualified Data.Map as Map
import Data.Word (Word8, Word16)
import Data.Bits (testBit, clearBit, setBit)
import Data.Maybe (mapMaybe)
import Numeric (showHex)
import Control.Monad.State (get, put)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout, stdin, hSetEcho)
import MOS6502Emulator.Core (Machine(..),FDX, fetchByteMem, writeByteMem, parseHexByte)
import MOS6502Emulator.Debugger.VimModeCore(Action(..), Motion(..), ViewMode(..), VimState (..), initialVimState)
import MOS6502Emulator.Debugger.Console (getInput, termHeight)
import Control.Monad (foldM)
import MOS6502Emulator.DissAssembler (disassembleInstruction, InstructionInfo, disassembleInstructions, opcodeMap)
import Data.Map (Map)
import MOS6502Emulator.Machine

-- | Execute an action with a motion
executeAction :: Action -> Word16 -> VimState -> FDX (Word16, [String])
executeAction action currentPos vimState = do
  machine <- get
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
      let newBreakpoints = currentPos : breakpoints machine
      put (machine { breakpoints = newBreakpoints })
      return (currentPos, ["Breakpoint added at $" ++ showHex currentPos ""])
    
    RemoveBreakpoint -> do
      let newBreakpoints = filter (/= currentPos) (breakpoints machine)
      put (machine { breakpoints = newBreakpoints })
      return (currentPos, ["Breakpoint removed at $" ++ showHex currentPos ""])
    
    Delete motion -> do
      endPos <- executeMotion motion currentPos
      let start = min currentPos endPos
      let end = max currentPos endPos
      mapM_ (`writeByteMem` 0) [start .. end]
      return (currentPos, ["Zeroed memory from $" ++ showHex start "" ++ " to $" ++ showHex end ""])
    
    Change motion -> do
      endPos <- executeMotion motion currentPos
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
      endPos <- executeMotion motion currentPos
      let start = min currentPos endPos
      let end = max currentPos endPos
      bytes <- mapM fetchByteMem [start .. end]
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
      let newBreakpoints = currentPos : breakpoints machine
      put (machine { breakpoints = newBreakpoints })
      return (currentPos, ["Temporary breakpoint added at $" ++ showHex currentPos "", "Continuing execution"])


-- | Execute a motion and return new cursor position
executeMotion :: Motion -> Word16 -> FDX Word16
executeMotion motion currentPos = do
  machine <- get
  let maxAddr = 0xFFFF -- Maximum address for 6502
  case motion of
    -- NextInstruction n -> do
    --   liftIO $ putStrLn "Nowa pozycja.. "
    -- NextInstruction n -> do
    --   foldM (\pos _ -> do
    --       if pos >= maxAddr then return pos
    --       else do
    --         (_, instLen) <- disassembleInstruction pos
    --         let newPos = pos + fromIntegral instLen
    --         liftIO $ putStrLn $ "Nowa pozycja: " ++ show newPos ++ " maxAddr: " ++ show maxAddr
    --         return newPos
    --     ) currentPos [1..n]
    NextInstruction n -> do
      foldM (\pos _ -> do
          if pos >= maxAddr then return pos
          else do
            (_, instLen) <- disassembleInstruction pos
            let newPos = pos + fromIntegral instLen
            liftIO $ putStrLn $ "Nowa pozycja: " ++ show newPos ++ " maxAddr: " ++ show maxAddr
            return newPos
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
    GotoPC -> return $ rPC (mRegs machine)
    
    WordForward n -> executeMotion (NextInstruction n) currentPos
    WordBackward n -> executeMotion (PrevInstruction n) currentPos
    
    EndOfPage -> do
      let linesPerPage = termHeight - 3 -- Reserve space for status and input
      case vsViewMode initialVimState of -- Use initialVimState for view mode; ideally, this would use vimState
        CodeView -> do
          -- Disassemble forward to find the address after linesPerPage instructions
          (lines, finalAddr) <- disassembleInstructions currentPos linesPerPage
          return $ max 0 (finalAddr - 1)
        _ -> return $ min maxAddr (currentPos + fromIntegral (linesPerPage * 16)) -- 16 bytes per line in MemoryView
    
    TopOfPage -> return $ vsViewStart initialVimState -- Ideally, use vimState
    
    MiddlePage -> do
      let linesPerPage = termHeight - 3
      let halfPage = linesPerPage `div` 2
      case vsViewMode initialVimState of
        CodeView -> do
          (lines, finalAddr) <- disassembleInstructions (vsViewStart initialVimState) halfPage
          return $ max 0 (finalAddr - 1)
        _ -> return $ min maxAddr (vsViewStart initialVimState + fromIntegral (halfPage * 16))
    
    FindByte byte forward -> findByteInMemory currentPos byte forward
    
    TillByte byte forward -> do
      newPos <- findByteInMemory currentPos byte forward
      return $ if forward then max 0 (newPos - 1) else min maxAddr (newPos + 1)
    
    RepeatFind forward -> do
        machine <- get
        case vsLastFind (vimState machine) of
          Just (byte, _) -> findByteInMemory currentPos byte forward
          Nothing -> return currentPos


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
