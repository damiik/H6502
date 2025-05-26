-- | Core functionality for Vim-style navigation in the debugger.
module MOS6502Emulator.Debugger.VimModeCore
    ( VimState(..),
      Motion(..)
    , Action(..)
    , ViewMode(..)
    , initialVimState
    , parseCount
    ) where


import Data.Word
import qualified Data.Map as Map
import Data.Char (isDigit)





-- | Vim-style motions for navigating memory/code
data Motion = 
    NextInstruction Int    -- j, 5j
  | PrevInstruction Int    -- k, 5k  
  | NextByte Int          -- l, 5l
  | PrevByte Int          -- h, 5h
  | GotoAddress Word16    -- G with address
  | GotoPC                -- gg - goto current PC
  | WordForward Int       -- w - next instruction boundary
  | WordBackward Int      -- b - prev instruction boundary
  | EndOfPage            -- L - end of visible memory
  | TopOfPage            -- H - top of visible memory
  | MiddlePage           -- M - middle of visible memory
  | FindByte Word8 Bool  -- f/F - find byte forward/backward
  | TillByte Word8 Bool  -- t/T - till byte forward/backward
  | RepeatFind Bool      -- ;/, - repeat last find
  deriving (Show, Eq)

-- | Vim-style actions that can be composed with motions
data Action =
    Delete Motion         -- d<motion> - delete/zero memory range
  | Change Motion         -- c<motion> - change memory range  
  | Yank Motion          -- y<motion> - copy memory range
  | Paste Bool           -- p/P - paste after/before
  | Set Word8            -- r<byte> - set byte at cursor
  | Increment Int        -- <C-a> - increment byte
  | Decrement Int        -- <C-x> - decrement byte
  | ToggleBit Int        -- ~<bit> - toggle bit
  | AddBreakpoint        -- B - add breakpoint at cursor
  | RemoveBreakpoint     -- <C-B> - remove breakpoint
  | ExecuteToHere       -- <Enter> - execute to cursor
  deriving (Show, Eq)

-- | Extended vim state for debugger context
data VimState = VimState
  { vsCursor :: Word16           -- Current memory address cursor
  , vsViewStart :: Word16        -- Top of current view window
  , vsViewMode :: ViewMode       -- What we're currently viewing
  , vsLastMotion :: Maybe Motion -- For repeating motions
  , vsLastFind :: Maybe (Word8, Bool) -- Last find command (byte, forward?)
  , vsCount :: Maybe Int         -- Pending count prefix (5dd, etc)
  , vsOperator :: Maybe String   -- Pending operator (d, c, y)
  , vsRegister :: Char           -- Current register for yank/paste
  , vsYankBuffer :: Map.Map Char [Word8] -- Named registers
  , vsMarks :: Map.Map Char Word16 -- Named marks (ma, 'a)
  , vsCommandBuffer :: String    -- For : commands
  , vsSearchBuffer :: String     -- For / searches
  , vsMessage :: String          -- Status message
  } deriving (Show)

data ViewMode = 
    CodeView     -- Disassembly view (default)
  | MemoryView   -- Raw memory hex view  
  | RegisterView -- Register detail view
  | StackView    -- Stack view
  deriving (Show, Eq)

-- | Initial Vim state
initialVimState :: VimState
initialVimState = VimState
  { vsCursor = 0x0000
  , vsViewStart = 0x0000  
  , vsViewMode = CodeView
  , vsLastMotion = Nothing
  , vsLastFind = Nothing
  , vsCount = Nothing
  , vsOperator = Nothing
  , vsRegister = '"'  -- Default register
  , vsYankBuffer = Map.empty
  , vsMarks = Map.empty
  , vsCommandBuffer = ""
  , vsSearchBuffer = ""
  , vsMessage = ""
  }

-- | Parse count prefix (like 5dd, 10j)
parseCount :: String -> Maybe Int
parseCount s = if all isDigit s && not (null s) 
               then Just (read s) 
               else Nothing



