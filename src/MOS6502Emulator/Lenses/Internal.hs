{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MOS6502Emulator.Lenses.Internal where

import Control.Lens
import MOS6502Emulator.Core (Machine(..))
import MOS6502Emulator.Debugger.Core (DebuggerConsoleState(..))
import MOS6502Emulator.Registers (Registers(..))
import Data.Word (Word16, Word8)
import qualified Data.Map.Strict as Map

-- Generate lenses using default rules
makeLenses ''Machine
makeLenses ''Registers
makeLenses ''DebuggerConsoleState
