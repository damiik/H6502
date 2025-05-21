module MOS6502Emulator.Debugger.Console
  ( DebuggerConsoleState(..) -- Export the new data type
  , initialConsoleState -- Export the initial state
  , renderScreen
  , getKey
  , getInput
  , putOutput
  , putString
  ) where

import qualified System.Console.ANSI as ANSI
import Numeric (showHex)
import Data.Word (Word16) -- Import Word16
import MOS6502Emulator.Machine (Machine(..), DebuggerMode(..), mRegs, debuggerMode, FDX(..)) -- Import Machine, DebuggerMode, and FDX
import MOS6502Emulator.Registers (rAC, rX, rY, rPC) -- Import register fields
import MOS6502Emulator.DissAssembler (disassembleInstruction) -- Import disassembleInstruction
import Control.Monad.State (runStateT) -- Import runStateT
import Control.Monad.IO.Class (liftIO) -- Import liftIO

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
  -- Save cursor position to restore later
  ANSI.saveCursor
  -- Clear and redraw everything
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0

  -- Display disassembled code
  let currentPC = rPC (mRegs machine)
  (disassembledLines, _) <- liftIO $ runStateT (unFDX $ disassembleLines (termHeight - 2 - length (outputLines consoleState) - 1) currentPC) machine -- Adjust height for output lines and input line
  mapM_ putStrLn disassembledLines

  -- Display output lines
  mapM_ putStrLn (outputLines consoleState)

  -- Status line (with background color)
  ANSI.setCursorPosition (termHeight - 2) 0
  -- Mode indicator with different colors based on mode
  case debuggerMode machine of
    CommandMode -> ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
    VimMode -> ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Blue, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
  let modeDisplay = case debuggerMode machine of
        CommandMode -> " COMMAND "
        VimMode -> " VIM "
  putStr modeDisplay

  -- Debugger status (registers)
  let regs = mRegs machine
  let regDisplay = "A=" ++ showHex (rAC regs) "" ++ " X=" ++ showHex (rX regs) "" ++ " Y=" ++ showHex (rY regs) "" ++ " PC=" ++ showHex (rPC regs) ""
  putStr regDisplay

  -- Placeholder for other status info (filename, modified, position)
  let fileSection = " [No Name] " ++ " " -- Replace with actual file info from Machine
  putStr fileSection

  -- Middle spacer
  let spacerLength = max 0 (termWidth - length modeDisplay - length regDisplay - length fileSection)
  putStr $ replicate spacerLength ' '

  -- Command/message line (with normal background)
  ANSI.setCursorPosition (termHeight - 1) 0
  ANSI.setSGR [ANSI.Reset] -- Reset colors for the message line

  -- Display input buffer
  putStr (inputBuffer consoleState)

  -- Position cursor at the end of the input buffer
  ANSI.setCursorPosition (termHeight - 1) (length (inputBuffer consoleState))
  ANSI.showCursor

-- Helper function to disassemble a given number of lines
disassembleLines :: Int -> Word16 -> FDX [String]
disassembleLines count currentPC
  | count <= 0 = return []
  | otherwise = do
      (disassembled, instLen) <- disassembleInstruction currentPC
      let nextPC = currentPC + (fromIntegral instLen)
      restOfLines <- disassembleLines (count - 1) nextPC
      return (disassembled : restOfLines)

-- Basic input function (will be replaced by direct key handling)
getInput :: IO String
getInput = getLine

-- Basic output function (will be replaced by updating console state)
putOutput :: String -> IO ()
putOutput = putStrLn

putString :: String -> IO ()
putString = putStr
