module MOS6502Emulator.Display
  ( renderScreen
  , getKey
  , getInput
  , putOutput
  , putString
  , termHeight
  , termWidth
  , printTwoColumns
  ) where

import qualified System.Console.ANSI as ANSI
import Numeric (showHex)
import Data.Word (Word16, Word8)
import MOS6502Emulator.Core (FDX(..), unFDX)
import MOS6502Emulator.Machine (Machine(..))
import MOS6502Emulator.Debugger.Core (DebuggerConsoleState(..), DebuggerMode(..))
import MOS6502Emulator.Registers (_rAC, _rX, _rY, _rPC)
import MOS6502Emulator.DissAssembler (disassembleInstructionPure)
import Control.Monad.State (runStateT, modify, gets)
import Control.Lens
import MOS6502Emulator.Lenses as L
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import Data.List (findIndex)
import Control.Monad (forM_)
import Control.Exception (try, IOException)

-- Define the maximum number of lines to keep in the console output buffer.
maxConsoleOutputLines :: Int
maxConsoleOutputLines = termHeight - 2

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
termWidth = 120 -- total screen width

termHeight :: Int
termHeight = 24

getKey :: IO Char
getKey = getChar

-- Basic input function (will be replaced by direct key handling)
getInput :: IO String
getInput = getLine

-- | Prints two columns of text separated by a vertical bar.
printTwoColumns :: Int -> [String] -> [String] -> IO ()
printTwoColumns _ leftLines rightLines = do -- 'width' parameter is now ignored as widths are fixed
  -- Cap both leftLines and rightLines to prevent hangs
  let cappedLeftLines = take 100 leftLines
  let cappedRightLines = take 100 rightLines
  let leftLen = length cappedLeftLines
  let rightLen = length cappedRightLines
  let maxLines = max leftLen rightLen
  
  let leftColumnWidth = 60   -- Accommodates disassembly lines
      rightColumnWidth = 59  -- Allows full command descriptions (60+1+59=120)

      -- Helper function to wrap text to a specified width
      wrapText :: Int -> String -> [String]
      wrapText _ "" = [""]
      wrapText width s =
        if length cleanStr <= width
        then [s]
        else let (first, _) = splitAt width cleanStr
                 -- Find last space to break at word boundary
                 lastSpace = findLastSpace first
                 (before, _) = if lastSpace > 0
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
    let l = if i < length cappedLeftLines then cappedLeftLines !! i else ""
        r = if i < length cappedRightLines then cappedRightLines !! i else ""
    
    let paddedL = padString l leftColumnWidth
    let paddedR = padString r rightColumnWidth

    ANSI.setCursorPosition i 0
    putStr $ paddedL ++ "|" ++ paddedR
    hFlush stdout -- Flush after each line to ensure immediate display

-- Render the debugger screen with status line and input line
renderScreen :: Machine -> [String] -> FDX ()
renderScreen machine rightColumnContent = do
  liftIO ANSI.hideCursor
  liftIO ANSI.saveCursor
  liftIO ANSI.clearScreen
  liftIO $ ANSI.setCursorPosition 0 0

  let consoleState = _mConsoleState machine
  let availableContentHeight = termHeight - 2
  let maxOutputLines = availableContentHeight

  let currentPC = _rPC (_mRegs machine)
  let (disassembledLines, _) = disassembleLinesPure availableContentHeight currentPC machine
  
  result <- liftIO $ try (printTwoColumns termWidth disassembledLines rightColumnContent) :: FDX (Either IOException ())
  case result of
    Left err -> liftIO $ putStrLn $ "Error in printTwoColumns: " ++ show err
    Right _ -> return ()

  -- Status line (with background color)
  liftIO $ ANSI.setCursorPosition (termHeight - 2) 0
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  let modeDisplay = case _debuggerMode machine of
        CommandMode -> " COMMAND "
        VimMode -> " VIM "
        VimCommandMode -> " VIM COMMAND "
  liftIO $ putStr modeDisplay -- Keep $ for putStr as it takes one arg

  -- Debugger status (registers)
  let regs = _mRegs machine
  let regDisplay = "A=" ++ showHex (_rAC regs) "" ++ " X=" ++ showHex (_rX regs) "" ++ " Y=" ++ showHex (_rY regs) "" ++ " PC=" ++ showHex (_rPC regs) ""
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
  let commandLineContent = case _debuggerMode machine of
        VimCommandMode -> ":" ++ _inputBuffer consoleState
        _ -> _inputBuffer consoleState
  liftIO $ putStr commandLineContent
  liftIO $ hFlush stdout

  -- Position cursor at the end of the input buffer
  let bufferLength = length (_inputBuffer consoleState)
  let cursorCol = case _debuggerMode machine of
        VimCommandMode -> 1 + bufferLength
        _ -> bufferLength
  liftIO $ ANSI.setCursorPosition (termHeight - 1) cursorCol
  liftIO ANSI.showCursor
  liftIO $ hFlush stdout

-- Helper function to disassemble a given number of lines and return them as a list of strings (pure version).
disassembleLinesPure :: Int -> Word16 -> Machine -> ([String], Word16) -- Return (disassembled lines, address after last instruction)
disassembleLinesPure count currentPC machine
  | count <= 0 = ([], currentPC)
  | otherwise =
      let (disassembled, instLen) = disassembleInstructionPure currentPC machine
          nextPC = currentPC + fromIntegral instLen
          (restOfLines, finalPC) = disassembleLinesPure (count - 1) nextPC machine
      in (disassembled : restOfLines, finalPC)

-- | Adds a string to the console output lines.
-- The `take maxConsoleOutputLines` ensures that the `outputLines` list
-- never exceeds the defined maximum size, effectively implementing a circular
-- buffer for console output. This is crucial for preventing memory leaks
-- caused by an ever-growing list of past outputs.
putOutput :: String -> FDX ()
putOutput s = do
  currentConsoleState <- gets _mConsoleState
  let updatedOutputLines = take maxConsoleOutputLines (_outputLines currentConsoleState ++ [s])
  L.mConsoleState . L.outputLines .= updatedOutputLines

-- | Adds a string to the console output lines (without newline).
-- Similar to `putOutput`, this function also caps the size of `outputLines`
-- to prevent memory accumulation.
putString :: String -> FDX ()
putString s = do
  currentConsoleState <- gets _mConsoleState
  let updatedOutputLines = take maxConsoleOutputLines (_outputLines currentConsoleState ++ [s])
  L.mConsoleState . L.outputLines .= updatedOutputLines
