module MOS6502Emulator.Debugger.Console
  ( DebuggerConsoleState(..) -- Export the new data type
  , initialConsoleState -- Export the initial state
  , renderScreen
  , getKey
  , getInput
  , putOutput
  , putString
  , termHeight
  , termWidth
  ) where

import qualified System.Console.ANSI as ANSI
import Numeric (showHex)
import Data.Word (Word16) -- Import Word16
import MOS6502Emulator.Core (Machine(..), DebuggerMode(..), mRegs, debuggerMode, FDX(..)) -- Import Machine, DebuggerMode, and FDX
import MOS6502Emulator.Registers (rAC, rX, rY, rPC) -- Import register fields
import MOS6502Emulator.DissAssembler (disassembleInstruction) -- Import disassembleInstruction
import Control.Monad.State (runStateT) -- Import runStateT
import Control.Monad.IO.Class (liftIO) -- Import liftIO
import System.IO (hFlush, stdout) -- Import hFlush and stdout

-- | Represents the state of the debugger console.
data DebuggerConsoleState = DebuggerConsoleState
  { outputLines :: [String] -- Lines of output to display
  , inputBuffer :: String -- Current input buffer
  , cursorPosition :: Int -- Cursor position in the input buffer
  , lastCommand :: String -- The last executed command
  } deriving (Show)

-- | The initial state of the debugger console.
initialConsoleState :: DebuggerConsoleState
initialConsoleState = DebuggerConsoleState
  { outputLines = []
  , inputBuffer = ""
  , cursorPosition = 0
  , lastCommand = ""
  }

-- Placeholder for terminal dimensions, replace with actual values or functions to get them
termWidth :: Int
termWidth = 80

termHeight :: Int
termHeight = 24

getKey :: IO Char
getKey = getChar

-- Render the debugger screen with status line and input line
renderScreen :: Machine -> DebuggerConsoleState -> IO ()
renderScreen machine consoleState = do
  ANSI.hideCursor
  ANSI.saveCursor
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0

  let availableContentHeight = termHeight - 2 -- Space for disassembled code and output lines
  let maxOutputLines = availableContentHeight - 1 -- Reserve 1 line for the input prompt

  let truncatedOutputLines = reverse $ take maxOutputLines $ reverse (outputLines consoleState)
  let remainingHeightForDisassembly = availableContentHeight - length truncatedOutputLines

  -- Display disassembled code
  let currentPC = rPC (mRegs machine)
  ((disassembledLines, _), _) <- liftIO $ runStateT (unFDX $ disassembleLines remainingHeightForDisassembly currentPC) machine
  
  -- Print disassembled lines with explicit cursor positioning
  mapM_ (\(line, row) -> do
    ANSI.setCursorPosition row 0
    putStr line
    hFlush stdout) (zip disassembledLines [0..])

  -- Print output lines with explicit cursor positioning
  mapM_ (\(line, row) -> do
    ANSI.setCursorPosition (length disassembledLines + row) 0
    putStr line
    hFlush stdout) (zip truncatedOutputLines [0..])

  -- Status line (with background color)
  ANSI.setCursorPosition (termHeight - 2) 0
  ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  let modeDisplay = case debuggerMode machine of
        CommandMode -> " COMMAND "
        VimMode -> " VIM "
  putStr modeDisplay

  -- Debugger status (registers)
  let regs = mRegs machine
  let regDisplay = "A=" ++ showHex (rAC regs) "" ++ " X=" ++ showHex (rX regs) "" ++ " Y=" ++ showHex (rY regs) "" ++ " PC=" ++ showHex (rPC regs) ""
  putStr regDisplay

  -- Placeholder for other status info (filename, modified, position)
  let fileSection = " [No Name] " ++ " "
  putStr fileSection

  -- Middle spacer
  let spacerLength = max 0 (termWidth - length modeDisplay - length regDisplay - length fileSection)
  putStr $ replicate spacerLength ' '
  hFlush stdout

  -- Command/message line (with normal background)
  ANSI.setCursorPosition (termHeight - 1) 0
  ANSI.setSGR [ANSI.Reset]
  putStr (inputBuffer consoleState)
  hFlush stdout

  -- Position cursor at the end of the input buffer
  ANSI.setCursorPosition (termHeight - 1) (length (inputBuffer consoleState))
  ANSI.showCursor
  hFlush stdout

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

-- Basic output function (will be replaced by updating console state)
putOutput :: String -> IO ()
putOutput = putStrLn

putString :: String -> IO ()
putString = putStr
