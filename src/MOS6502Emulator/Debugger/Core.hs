module MOS6502Emulator.Debugger.Core
  ( DebuggerMode(..)
  , DebuggerAction(..)
  , DebuggerConsoleState(..)
  , initialConsoleState
  , parseCount
  ) where

import Data.Word (Word8, Word16) -- Import Word16
import Data.Char (isDigit)
import Data.Bits (testBit)
import Numeric (showHex)
-- | Data type to represent the debugger mode
data DebuggerMode = CommandMode | VimMode | VimCommandMode deriving (Show, Eq)

-- | Data type to represent actions the debugger can take.
data DebuggerAction = ContinueLoop String | ExecuteStep String | ExitDebugger | QuitEmulator | NoAction | SwitchToVimMode | SwitchToCommandMode | SwitchToVimCommandMode deriving (Show, Eq)

-- | Represents the state of the debugger console.
data DebuggerConsoleState = DebuggerConsoleState
  { _outputLines :: [String] -- Lines of output to display (right column)
  , _inputBuffer :: String -- Current input buffer
  , _cursorPosition :: Int -- Cursor position in the input buffer
  , _lastCommand :: String -- The last executed command
  , _vimCommandInputBuffer :: String -- New field for Vim command mode input
  , _helpLines :: [String] -- Added: Stores lines of help text for pagination
  , _helpScrollPos :: Int  -- Added: Current scroll position for help text
  } deriving (Show)

-- | The initial state of the debugger console.
initialConsoleState :: DebuggerConsoleState
initialConsoleState = DebuggerConsoleState
  { _outputLines = []
  , _inputBuffer = ""
  , _cursorPosition = 0
  , _lastCommand = ""
  , _vimCommandInputBuffer = "" -- Initialize new field
  , _helpLines = [] -- Initialize helpLines as empty
  , _helpScrollPos = 0  -- Initialize helpScrollPos to 0
  }


-- | Parse count prefix (like 5dd, 10j)
parseCount :: String -> Maybe Int
parseCount s = if all isDigit s && not (null s) 
               then Just (read s) 
               else Nothing
