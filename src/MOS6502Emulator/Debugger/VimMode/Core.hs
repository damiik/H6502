module MOS6502Emulator.Debugger.VimMode.Core
  ( Motion(..)
  , Action(..)
  , OperatorType(..)
  , VisualType(..)
  , ViewMode(..)
  , ObjectModifier(..)
  , TextObjectType(..)
  , CommandState(..)
  , RepeatableCommand(..)
  , VimState(..)
  , VimCommand(..)
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

-- | Object modifier for text objects
data ObjectModifier = Inner | Outer deriving (Eq, Show)

-- | Text object types
data TextObjectType = Word | Line | Bracket | Quote deriving (Eq, Show)

-- | Command composition state
data CommandState =
    NoCommand
  | Operator OperatorType
  | Object OperatorType ObjectModifier
  | VimPendingCompose  -- New state for verb-object composition
  | CommandModeV
  deriving (Eq, Show)

-- Add to MOS6502Emulator.Debugger.VimMode.Core.hs
data VimCommand =
    VBreak (Maybe Word16) -- :break [addr]
  | VWatch (Maybe (Word16, Word16)) -- :watch [start-end]
  | VStep (Maybe Int) -- :step [count]
  | VRegs -- :regs
  | VDisas (Maybe Word16) (Maybe Word16) -- :disas [start [end]]
  | VFill Word16 Word16 [Word8] -- :fill start end byte1 [byte2...]
  | VSetReg8 Char Word8 -- :ra val, :rx val, :ry val, :rsp val, :rsr val
  | VSetPC Word16 -- :rpc val
  | VQuit -- :quit / :q
  | VExit -- :exit / :x
  | VTrace -- :trace / :t
  | VUnknown String -- For unparsed or invalid commands
  deriving (Eq, Show)

data Motion = Up | Down | Left | Right | PageUp | PageDown | ToStart | ToEnd
            | NextInstruction Int | PrevInstruction Int | NextByte Int | PrevByte Int
            | GotoAddressMotion Word16 | GotoPC | WordForward Int | WordBackward Int
            | EndOfPage | TopOfPage | MiddlePage | FindByte Word8 Bool | TillByte Word8 Bool | RepeatFind Bool
            | TextObject ObjectModifier TextObjectType Int  -- Unified text object with modifier and count
  deriving (Eq, Show)

-- Visual mode types
data VisualType = CharVisual | LineVisual | BlockVisual
  deriving (Eq, Show)

-- Update Action data type to include ExecuteVimCommand
data Action = Move Motion | SetView ViewMode | EnterCommandMode | ExecuteCommand | Step | ToggleBreakpoint | ToggleMemTrace | StoreAddress | GotoAddress | SearchForward | SearchBackward | RepeatSearch | RepeatSearchReverse
            | Set Word8 | Increment Int | Decrement Int | ToggleBit Int
            | AddBreakpoint | RemoveBreakpoint | Delete Motion | Change Motion
            | Yank Motion | Paste Bool | ExecuteToHere
            | ColonCommand VimCommand -- New action to execute parsed Vim commands
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
  , vsCommandState :: CommandState  -- Track command composition state
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
  , vsCommandState = NoCommand  -- Initialize to NoCommand
  }

-- | VimMode command documentation
vimModeHelp :: String
vimModeHelp = unlines
  [ "Vim Mode Commands:"
  , ""
  , "  Navigation:"
  , "    h/j/k/l   Move cursor (left/down/up/right)"
  , "    w/b       Word forward/backward (code view, by instruction)"
  , "    e         End of word (code view)"
  , "    0/$       Start/end of line (code view)"
  , "    gg/G      Start/end of view"
  , "    H/M/L     Top/middle/bottom of page"
  , "    Ctrl+u/d  Scroll page up/down"
  , "    gp        Go to Program Counter (PC)"
  , "    g<addr>   Go to address (e.g., g1234)"
  , "    f/F<byte> Find byte forward/backward"
  , "    t/T<byte> Till byte forward/backward"
  , "    ;/ ,      Repeat last find/till"
  , "    '<char>   Go to mark <char>"
  , ""
  , "  Editing/Manipulation:"
  , "    r<byte>   Replace byte under cursor"
  , "    d<motion> Delete with motion (e.g., dw, d$, dG, diw, caw)"
  , "    c<motion> Change with motion (e.g., cw, c$, cG, ciw, caw)"
  , "    y<motion> Yank with motion (e.g., yw, y$, yG, yiw, yaw)"
  , "    p/P       Paste after/before cursor"
  , "    x         Delete character under cursor"
  , "    ~         Toggle bit under cursor"
  , "    + / -     Increment/Decrement byte"
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
  , "    s         Step instruction"
  , "    <CR>      Execute until cursor (Enter key)"
  , ""
  , "  Debugger Features (Colon Commands):"
  , "    :break [<addr>]       Set/list/remove breakpoint at <addr>"
  , "    :watch [<start>-<end>]  Trace memory range"
  , "    :step [<count>]       Execute <count> instructions (default 1)"
  , "    :regs                 Show current register values"
  , "    :disas [<start> [<end>]] Disassemble memory range (default 32 from last PC)"
  , "    :fill <start> <end> <byte1> [...] Fill memory range with bytes"
  , "    :ra <val>             Set Accumulator to hex value"
  , "    :rx <val>             Set X register to hex value"
  , "    :ry <val>             Set Y register to hex value"
  , "    :rsp <val>            Set Stack Pointer to hex value"
  , "    :rsr <val>            Set Status Register to hex value"
  , "    :rpc <val>            Set Program Counter to hex value"
  , "    :quit / :q            Quit emulator"
  , "    :exit / :x            Exit debugger (return to emulator)"
  , "    :trace / :t           Toggle instruction tracing"
  , "    :m<char>              Set mark <char>"
  , ""
  , "  General:"
  , "    <count>   Repeat command (e.g., 5j, d2w)"
  , "    Escape    Cancel pending command/mode"
  , "    .         Repeat last change"
  ]
