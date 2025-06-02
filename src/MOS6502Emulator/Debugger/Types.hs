module MOS6502Emulator.Debugger.Types
  ( DebuggerMode(..)
  , DebuggerAction(..)
  , DebuggerConsoleState(..)
  , initialConsoleState
  ) where

import Data.Word (Word16) -- Import Word16

-- | Data type to represent the debugger mode
data DebuggerMode = CommandMode | VimMode | VimCommandMode deriving (Show, Eq)

-- | Data type to represent actions the debugger can take.
data DebuggerAction = ContinueLoop String | ExecuteStep String | ExitDebugger | QuitEmulator | NoAction | SwitchToVimMode | SwitchToCommandMode | SwitchToVimCommandMode deriving (Show, Eq)

-- | Represents the state of the debugger console.
data DebuggerConsoleState = DebuggerConsoleState
  { outputLines :: [String] -- Lines of output to display (right column)
  , inputBuffer :: String -- Current input buffer
  , cursorPosition :: Int -- Cursor position in the input buffer
  , lastCommand :: String -- The last executed command
  , vimCommandInputBuffer :: String -- New field for Vim command mode input
  , helpLines :: [String] -- Added: Stores lines of help text for pagination
  , helpScrollPos :: Int  -- Added: Current scroll position for help text
  } deriving (Show)

-- | The initial state of the debugger console.
initialConsoleState :: DebuggerConsoleState
initialConsoleState = DebuggerConsoleState
  { outputLines = []
  , inputBuffer = ""
  , cursorPosition = 0
  , lastCommand = ""
  , vimCommandInputBuffer = "" -- Initialize new field
  , helpLines = [] -- Initialize helpLines as empty
  , helpScrollPos = 0  -- Initialize helpScrollPos to 0
  }
