module MOS6502Emulator.Debugger.Console
  ( renderScreen
  , getKey
  , getInput
  , putOutput
  , putString
  , termHeight
  , termWidth
  , DebuggerConsoleState
  , initialConsoleState
  , printTwoColumns -- Export printTwoColumns
  ) where

import qualified System.Console.ANSI as ANSI
import Numeric (showHex)
import Data.Word (Word16) -- Import Word16
import MOS6502Emulator.Core (Machine(..), FDX(..), mRegs, mConsoleState) -- Updated import from Core, removed debuggerMode as it's from Types
import MOS6502Emulator.Debugger.Core (DebuggerConsoleState(..), initialConsoleState, DebuggerMode(..)) -- New import from Types, exporting constructors and fields
import MOS6502Emulator.Registers (rAC, rX, rY, rPC) -- Import register fields
import MOS6502Emulator.DissAssembler (disassembleInstruction) -- Import disassembleInstruction
import Control.Monad.State (runStateT, get, modify) -- Import runStateT, get, modify
import Control.Monad.IO.Class (liftIO) -- Import liftIO
import System.IO (hFlush, stdout) -- Import hFlush and stdout
import Data.List (findIndex, splitAt, foldl')
import Control.Monad (forM_, unless)

-- | Strips ANSI escape codes from a string to calculate its visual length.
stripAnsiCodes :: String -> String
stripAnsiCodes [] = []
stripAnsiCodes ('\ESC':'[':xs) = 
  let dropped = dropWhile (\c -> c /= 'm' && c /= 'K' && c /= 'G' && c /= 'H' && c /= 'f' && c /= 'J') xs
      remaining = if null dropped then [] else tail dropped
  in stripAnsiCodes remaining
stripAnsiCodes (x:xs) = x : stripAnsiCodes xs

-- Placeholder for terminal dimensions, replace with actual values or functions to get them
termWidth :: Int
termWidth = 120 -- Reverting to 120 as per user's initial request for total screen width

termHeight :: Int
termHeight = 24

getKey :: IO Char
getKey = getChar

-- | Prints two columns of text separated by a vertical bar.
printTwoColumns :: Int -> [String] -> [String] -> IO ()
printTwoColumns _ leftLines rightLines = do -- 'width' parameter is now ignored as widths are fixed
  let leftColumnWidth = 60   -- Accommodates disassembly lines
      rightColumnWidth = 59  -- Allows full command descriptions (60+1+59=120)
      maxLines = max (length leftLines) (length rightLines)

      -- Helper function to wrap text to a specified width
      wrapText :: Int -> String -> [String]
      wrapText _ "" = [""]
      wrapText width s =
        if length cleanStr <= width
        then [s]
        else let (first, rest) = splitAt width cleanStr
                 -- Find last space to break at word boundary
                 lastSpace = findLastSpace first
                 (before, after) = if lastSpace > 0
                                   then splitAt lastSpace cleanStr
                                   else (cleanStr, "")
                 -- Re-add ANSI codes to wrapped segments
                 wrappedBefore = addAnsiCodes s before
                 wrappedAfter = drop (length before) s
             in [wrappedBefore] <> wrapText width wrappedAfter
        where
          cleanStr = stripAnsiCodes s
          findLastSpace str =
            case findIndex (==' ') (reverse str) of
              Just pos -> length str - pos - 1
              Nothing  -> 0
          addAnsiCodes original stripped =
            -- Simple implementation: preserve ANSI codes by reapplying to each segment
            -- For better implementation we'd need full ANSI state machine
            if null original then original
            else take (length stripped) original

      padString s targetWidth =
        let visualLen = length (stripAnsiCodes s)
            paddingNeeded = targetWidth - visualLen
        in if paddingNeeded > 0
           then s ++ replicate paddingNeeded ' '
           else s  -- Rely on wrapping to prevent overflow

  forM_ [0..maxLines-1] $ \i -> do
    let l = if fromIntegral i < length leftLines then leftLines !! i else ""
        r = if fromIntegral i < length rightLines then rightLines !! i else ""
    
    let paddedL = padString l leftColumnWidth
    let paddedR = padString r rightColumnWidth

    ANSI.setCursorPosition (fromIntegral i) 0
    putStr $ paddedL ++ "|" ++ paddedR
    hFlush stdout -- Flush after each line to ensure immediate display

-- Render the debugger screen with status line and input line
renderScreen :: Machine -> FDX () -- Changed signature
renderScreen machine = do
  liftIO ANSI.hideCursor
  liftIO ANSI.saveCursor
  liftIO ANSI.clearScreen
  liftIO $ ANSI.setCursorPosition 0 0 -- Keep $ for setCursorPosition as it takes two args

  let consoleState = mConsoleState machine -- Get console state from machine

  let availableContentHeight = termHeight - 2 -- Space for two columns and status line
  let maxOutputLines = availableContentHeight -- All available height for output

  let currentOutputLines = outputLines consoleState
  let helpTextLines = helpLines consoleState
  let helpTextScrollPos = helpScrollPos consoleState

  let rightColumnContent = if null helpTextLines
                           then reverse $ take maxOutputLines $ reverse currentOutputLines
                           else take maxOutputLines $ drop helpTextScrollPos helpTextLines
  
  -- Display disassembled code (left column)
  let currentPC = rPC (mRegs machine)
  ((disassembledLines, _), _) <- liftIO $ runStateT (unFDX $ disassembleLines availableContentHeight currentPC) machine
  
  -- Print two columns
  liftIO $ printTwoColumns termWidth disassembledLines rightColumnContent

  -- Status line (with background color)
  liftIO $ ANSI.setCursorPosition (termHeight - 2) 0
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  let modeDisplay = case debuggerMode machine of
        CommandMode -> " COMMAND "
        VimMode -> " VIM "
        VimCommandMode -> " VIM COMMAND "
  liftIO $ putStr modeDisplay -- Keep $ for putStr as it takes one arg

  -- Debugger status (registers)
  let regs = mRegs machine
  let regDisplay = "A=" ++ showHex (rAC regs) "" ++ " X=" ++ showHex (rX regs) "" ++ " Y=" ++ showHex (rY regs) "" ++ " PC=" ++ showHex (rPC regs) ""
  liftIO $ putStr regDisplay -- Keep $ for putStr as it takes one arg

  -- Placeholder for other status info (filename, modified, position)
  let fileSection = " [No Name] " ++ " "
  liftIO $ putStr fileSection -- Keep $ for putStr as it takes one arg

  -- Middle spacer
  let spacerLength = max 0 (termWidth - length modeDisplay - length regDisplay - length fileSection)
  liftIO $ putStr $ replicate spacerLength ' ' -- Keep $ for putStr as it takes one arg
  liftIO $ hFlush stdout

  -- Command/message line (with normal background)
  liftIO $ ANSI.setCursorPosition (termHeight - 1) 0
  liftIO $ ANSI.setSGR [ANSI.Reset]
  let commandLineContent = case debuggerMode machine of
        VimCommandMode -> ":" ++ inputBuffer consoleState
        _ -> inputBuffer consoleState
  liftIO $ putStr commandLineContent
  liftIO $ hFlush stdout

  -- Position cursor at the end of the input buffer
  let bufferLength = length (inputBuffer consoleState)
  let cursorCol = case debuggerMode machine of
        VimCommandMode -> 1 + bufferLength
        _ -> bufferLength
  liftIO $ ANSI.setCursorPosition (termHeight - 1) cursorCol
  liftIO ANSI.showCursor
  liftIO $ hFlush stdout

-- Helper function to disassemble a given number of lines and return them as a list of strings.
disassembleLines :: Int -> Word16 -> FDX ([String], Word16) -- Return (disassembled lines, address after last instruction)
disassembleLines count currentPC
  | count <= 0 = return ([], currentPC)
  | otherwise = do
      (disassembled, instLen) <- disassembleInstruction currentPC
      let nextPC = currentPC + fromIntegral instLen
      (restOfLines, finalPC) <- disassembleLines (count - 1) nextPC
      return (disassembled : restOfLines, finalPC)

-- Basic input function (will be replaced by direct key handling)
getInput :: IO String
getInput = getLine

-- | Adds a string to the console output lines.
putOutput :: String -> FDX ()
putOutput s = modify (\m -> m { mConsoleState = (mConsoleState m) { outputLines = outputLines (mConsoleState m) ++ [s] } })

-- | Adds a string to the console output lines (without newline).
putString :: String -> FDX ()
putString s = modify (\m -> m { mConsoleState = (mConsoleState m) { outputLines = outputLines (mConsoleState m) ++ [s] } })
