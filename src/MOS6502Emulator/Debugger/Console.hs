module MOS6502Emulator.Debugger.Console
  ( DebuggerConsoleState(..)
  , initialConsoleState
  -- Re-export from MOS6502Emulator.Display
  , renderScreen
  , getKey
  , getInput
  , putOutput
  , putString
  , termHeight
  , termWidth
  , printTwoColumns
  ) where

import MOS6502Emulator.Debugger.Core (DebuggerConsoleState(..), initialConsoleState)
import MOS6502Emulator.Display (renderScreen, getKey, getInput, putOutput, putString, termHeight, termWidth, printTwoColumns)
