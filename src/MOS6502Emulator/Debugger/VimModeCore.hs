module MOS6502Emulator.Debugger.VimModeCore
  ( Motion(..)
  , Action(..)
  , ViewMode(..)
  , RepeatableCommand(..) 
  , VimState(..)
  , initialVimState
  , vimModeHelp
  ) where
import Data.Word

import qualified Data.Map as Map

-- | Commands that can be repeated with '.'
data RepeatableCommand =
    RepeatAction Action
  | RepeatMotion Motion
  | RepeatStep
  deriving (Show, Eq)

data Motion = Up | Down | Left | Right | PageUp | PageDown | ToStart | ToEnd
            | NextInstruction Int | PrevInstruction Int | NextByte Int | PrevByte Int
            | GotoAddressMotion Word16 | GotoPC | WordForward Int | WordBackward Int
            | EndOfPage | TopOfPage | MiddlePage | FindByte Word8 Bool | TillByte Word8 Bool | RepeatFind Bool
  deriving (Eq, Show)

data Action = Move Motion | SetView ViewMode | EnterCommandMode | ExecuteCommand | Step | ToggleBreakpoint | ToggleMemTrace | StoreAddress | GotoAddress | SearchForward | SearchBackward | RepeatSearch | RepeatSearchReverse
            | Set Word8 | Increment Int | Decrement Int | ToggleBit Int
            | AddBreakpoint | RemoveBreakpoint | Delete Motion | Change Motion
            | Yank Motion | Paste Bool | ExecuteToHere
  deriving (Eq, Show)

data ViewMode = CodeView | MemoryView | RegisterView | StackView
  deriving (Eq, Show, Enum, Bounded)

data VimState = VimState
  { vsCursor :: Word16
  , vsViewStart :: Word16
  , vsViewMode :: ViewMode
  , vsCount :: Maybe Int
  , vsOperator :: Maybe String
  , vsMotion :: Maybe Motion
  , vsMessage :: String
  , vsInCommandMode :: Bool
  , vsCommandBuffer :: String
  , vsRegister :: Char
  , vsYankBuffer :: Map.Map Char [Word8]
  , vsLastFind :: Maybe (Word8, Bool)
  , vsLastChange :: Maybe RepeatableCommand
  , vsMarks :: Map.Map Char Word16
  } deriving (Eq, Show)

initialVimState :: VimState
initialVimState = VimState
  { vsCursor = 0
  , vsViewStart = 0
  , vsViewMode = CodeView
  , vsCount = Nothing
  , vsOperator = Nothing
  , vsMotion = Nothing
  , vsMessage = ""
  , vsInCommandMode = False
  , vsCommandBuffer = ""
  , vsRegister = '"'  -- Default to unnamed register
  , vsYankBuffer = Map.empty
  , vsLastFind = Nothing
  , vsLastChange = Nothing
  , vsMarks = Map.empty
  }

-- | VimMode command documentation
vimModeHelp :: String
vimModeHelp = unlines
  [ "Vim Mode Commands:"
  , "  Navigation:"
  , "    j/k       Move cursor down/up"
  , "    h/l       Move left/right in memory view"
  , "    Ctrl+u/d  Page up/down"
  , "    G/gg      Go to end/start"
  , "    :         Enter command mode"
  , "    /?        Search forward/backward"
  , "    n/N       Repeat search forward/backward"
  , "  Views:"
  , "    r         View registers"
  , "    m         View memory"
  , "    s         View stack"
  , "    v         Cycle views (Code, Memory, Registers, Stack)"
  , "  Execution:"
  , "    z         Step instruction"
  , "    Enter     Execute command"
  , "  Debugger:"
  , "    b         Breakpoints menu"
  , "    t         Memory traces menu"
  , "    a         Stored addresses menu"
  , "  Command Mode (after ':'):"
  , "    Same as debugger commands (step, regs, mem, etc.)"
  ]