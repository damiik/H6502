-- | Module defining 6502 conditional branch mnemonics.
--
-- In this module, conditional branch mnemonics are defined and
-- helper functions for handling branches. The 'emitBranch' function is
-- re-exported by the main 'Assembly' module for API users.
module Assembly.Branch (
    BranchMnemonic(..),
) where

import Data.Word (Word8)
import qualified Data.Map.Strict as Map
-- Imports from Assembly.Core.Types are commented out because:
-- * Word8 needed for opcodes
-- * Map.Strict used in future implementations
-- * Mnemonics and instruction tables are now in Core.hs

-- | Type of 6502 conditional branch mnemonics.
--
-- Constructors correspond to standard 6502 mnemonics:
-- [B_BNE] BNE - Branch if Not Equal (Z=0)
-- [B_BEQ] BEQ - Branch if EQual (Z=1)
-- [B_BCS] BCS - Branch if Carry Set (C=1)
-- [B_BCC] BCC - Branch if Carry Clear (C=0)
-- [B_BMI] BMI - Branch if MInus (N=1)
-- [B_BPL] BPL - Branch if PLus (N=0)
-- [B_BVS] BVS - Branch if oVerflow Set (V=1)
-- [B_BVC] BVC - Branch if oVerflow Clear (V=0)
data BranchMnemonic = B_BNE | B_BEQ | B_BCS | B_BCC | B_BMI | B_BPL | B_BVS | B_BVC
    deriving (Show, Eq, Ord, Enum, Bounded) -- Deriving instances for all BranchMnemonic
