module MOS6502Emulator.Debugger.Core
  ( DebuggerMode(..)
  , DebuggerState(..)
  , DebuggerInput(..)
  , DebuggerAction(..)
  , DebuggerCommand(..)
  , DebuggerConsoleState(..)
  , initialConsoleState
  , parseCount
  ) where

import Data.Word (Word8, Word16) -- Import Word16
import Data.Char (isDigit)
import Data.Bits (testBit)
import Numeric (showHex)
import MOS6502Emulator.Debugger.VimMode.Core (VimState) -- Import VimState
-- | Data type to represent the debugger mode
data DebuggerMode = CommandMode | VimMode | VimCommandMode deriving (Show, Eq)

-- | Data type to represent the overall state of the debugger's control flow.
data DebuggerState
  = DebuggerWaitingForInput DebuggerMode -- Waiting for user command in a specific mode
  | DebuggerDisplayingHelp DebuggerMode Int -- Displaying help, with current mode and scroll position
  | DebuggerExitingLoop -- The debugger is exiting its interactive loop
  | DebuggerQuittingEmulator -- The emulator is quitting entirely
  deriving (Show, Eq)

-- | Data type to represent user input or internal events for the debugger state machine.
data DebuggerInput
  = CommandInput String -- A full command string entered by the user
  | KeyInput Char       -- A single key press (e.g., for help scrolling)
  | VimKeyProcessed (DebuggerAction, [String], VimState, DebuggerConsoleState, DebuggerMode) -- Result of processing a VimMode key
  deriving (Show)

-- | Data type to represent actions the debugger can take (side effects).
data DebuggerAction
  = ExecuteStepAction -- Signal to execute one CPU step
  | ExitDebuggerAction -- Signal to exit the debugger loop
  | QuitEmulatorAction -- Signal to quit the entire emulator
  | RenderScreenAction -- Signal to re-render the screen
  | UpdateConsoleOutputAction String [String] -- Signal to update console output with command and its result
  | SetDebuggerModeAction DebuggerMode -- Signal to change the debugger mode
  | NoDebuggerAction -- No specific action to perform
  deriving (Show, Eq)

-- | Algebraic Data Type for debugger commands
data DebuggerCommand
  = Step
  | Break (Maybe Word16)
  | MemTrace (Maybe (Word16, Word16, Maybe String))
  | Fill Word16 Word16 [Word8]
  | SetReg8 String Word8
  | SetPC Word16
  | Disassemble (Maybe Word16)
  | Regs
  | Trace
  | Goto Word16
  | Quit
  | Exit
  | Unknown String
  deriving (Show, Eq)


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
