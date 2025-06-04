module MOS6502Emulator.Debugger.VimMode.Core
  ( Motion(..)
  , Action(..)
  , OperatorType(..)
  , VisualType(..)
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
            | WordObject Int  -- New text object: word
            | LineObject Int  -- New text object: line
            | BracketObject   -- New text object: bracket pair
  deriving (Eq, Show)

-- Visual mode types
data VisualType = CharVisual | LineVisual | BlockVisual
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
  , vsOperator :: Maybe OperatorType  -- Represents pending operator (d, c, y)
  , vsMotion :: Maybe Motion
  , vsMessage :: String
  , vsInCommandMode :: Bool
  , vsInVisualMode :: Bool
  , vsVisualType :: VisualType  -- Added: CharVisual, LineVisual or BlockVisual
  , vsVisualStart :: Word16  -- Start of visual selection
  , vsVisualEnd :: Word16    -- Added: End of visual selection
  , vsCommandBuffer :: String
  , vsRegister :: Char
  , vsYankBuffer :: Map.Map Char [Word8]
  , vsLastFind :: Maybe (Word8, Bool)
  , vsLastChange :: Maybe RepeatableCommand
  , vsMarks :: Map.Map Char Word16
  } deriving (Eq, Show)

-- | Operator types for operator-pending mode
data OperatorType = DeleteOp | ChangeOp | YankOp deriving (Eq, Show)

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
  , vsInVisualMode = False
  , vsVisualType = CharVisual  -- Added: default visual type
  , vsVisualStart = 0
  , vsVisualEnd = 0            -- Added
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
  , ""
  , "  Navigation:"
  , "    h/j/k/l   Move cursor (left/down/up/right)"
  , "    w/b       Word forward/backward (code view)"
  , "    e         End of word (code view)"
  , "    0/$       Start/end of line (code view)"
  , "    gg/G      Start/end of view"
  , "    H/M/L     Top/middle/bottom of page"
  , "    Ctrl+u/d  Scroll page up/down"
  , "    gp        Go to Program Counter (PC)"
  , "    g<addr>   Go to address (e.g., g1234)"
  , "    f/F<byte> Find byte forward/backward on line"
  , "    t/T<byte> Till byte forward/backward on line"
  , "    ;/ ,      Repeat last find/till"
  , "    '<char>   Go to mark <char>"
  , ""
  , "  Editing/Manipulation:"
  , "    r<byte>   Replace byte under cursor"
  , "    d<motion> Delete with motion (e.g., dw, d$, dG)"
  , "    c<motion> Change with motion (e.g., cw, c$, cG)"
  , "    y<motion> Yank with motion (e.g., yw, y$, yG)"
  , "    p/P       Paste after/before cursor"
  , "    x         Delete character under cursor"
  , "    ~         Toggle bit under cursor"
  , "    Ctrl+a/x  Increment/Decrement byte"
  , "    .         Repeat last change"
  , "    \"<reg>   Specify register for next op"
  , ""
  , "  Visual Mode:"
  , "    v         Char visual mode"
  , "    V         Line visual mode"
  , "    Ctrl+v    Block visual mode"
  , "    d/c/y     Delete/Change/Yank selection"
  , ""
  , "  Views:"
  , "    c/m/r/s   View code/memory/registers/stack"
  , "    v         Cycle views"
  , ""
  , "  Execution:"
  , "    z         Step instruction"
  , "    gh        Execute until cursor"
  , ""
  , "  Debugger Features:"
  , "    b         Toggle breakpoint"
  , "    t         Toggle memory trace"
  , "    a         Store address"
  , "    :         Enter command mode"
  , "              (e.g., :step, :regs, :break <addr>)"
  , "              (e.g., :trace <addr>, :goto <addr>)"
  , "    /         Search forward"
  , "              (e.g., /<byte_pattern> or /<instr>)"
  , "    ?         Search backward"
  , "    n/N       Repeat search forward/backward"
  , "    m<char>   Set mark <char>"
  , ""
  , "  General:"
  , "    <count>   Repeat command (e.g., 5j, d2w)"
  , "    Enter     Execute command in command mode"
  ]
