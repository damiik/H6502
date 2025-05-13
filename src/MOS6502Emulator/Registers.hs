-- | Defines the MOS 6502 CPU registers and status flags.
{-# LANGUAGE StrictData #-}

module MOS6502Emulator.Registers (
  Registers(..)
  , SRFlag(..)
  , lookupSRFlag
  , setSRFlag
  , clearSRFlag
  , mkRegisters
) where

import Data.Word
import Data.Bits
import Data.Maybe ( catMaybes )

-- | Represents the MOS 6502 CPU registers.
-- http://www.masswerk.at/6502/6502_instruction_set.html
data Registers = Registers
  { rPC :: Word16 -- ^ Program Counter
  , rAC :: Word8  -- ^ Accumulator
  , rX  :: Word8  -- ^ X register
  , rY  :: Word8  -- ^ Y register
  , rSR :: Word8  -- ^ Status register [NV-BDIZC]
  , rSP :: Word8  -- ^ Stack pointer
  } deriving (Read, Show, Eq, Ord)

-- | Represents the individual flags in the Status Register.
data SRFlag = Carry      -- ^ bit 0
            | Zero       -- ^ bit 1
            | Interrupt  -- ^ bit 2
            | Decimal    -- ^ bit 3
            | Break      -- ^ bit 4
            | Ignored    -- ^ bit 5
            | Overflow   -- ^ bit 6
            | Negative   -- ^ bit 7
  deriving (Read, Show, Eq, Ord, Enum)

-- | Constructs the registers with initial values (PC=0, AC=0, X=0, Y=0, SR=0, SP=0xff).
mkRegisters :: Registers
mkRegisters = Registers
  { rPC = 0x00
  , rAC = 0x00
  , rX  = 0x00
  , rY  = 0x00
  , rSR = 0x00
  , rSP = 0xff
  }

-- | A list of all status register flags.
allSRFlags :: [SRFlag]
allSRFlags = [Carry .. Negative]

-- | Looks up the value of a particular flag by name.
-- Returns `Just flag` if the flag is set, `Nothing` otherwise.
lookupSRFlag :: Registers -> SRFlag -> Maybe SRFlag
lookupSRFlag (Registers { rSR = sr }) f
  | testBit sr (fromEnum f) = Just f
  | otherwise               = Nothing

-- | Returns a list of all currently set status register flags.
-- The return type is morally `Data.Set.Set SRFlag`.
getSRFlags :: Registers -> [SRFlag]
getSRFlags rs =
  catMaybes (zipWith lookupSRFlag (repeat rs) allSRFlags)

-- | Applies a bit transformation function at the specified status register bit.
atSRFlag :: (Word8 -> Int -> Word8) -> Registers -> SRFlag -> Registers
atSRFlag f rs@(Registers { rSR = sr }) flag =
  rs { rSR = f sr (fromEnum flag) }

-- | Clears a specific status register flag.
clearSRFlag :: Registers -> SRFlag -> Registers
clearSRFlag = atSRFlag clearBit

-- | Sets a specific status register flag.
setSRFlag :: Registers -> SRFlag -> Registers
setSRFlag = atSRFlag setBit

-- | Complements (toggles) a specific status register flag.
complementSRFlag :: Registers -> SRFlag -> Registers
complementSRFlag = atSRFlag complementBit

-----------------------------------------------------------------
-- With the exception of clearSRFlags these are overkill

-- | Applies a function at every bit in the status register.
atSRFlags :: (Word8 -> Int -> Word8) -> Registers -> Registers
atSRFlags f rs@(Registers { rSR = sr }) =
  rs { rSR = foldl f sr (map fromEnum allSRFlags) }

-- | Clears all the bits in the status register.
clearSRFlags :: Registers -> Registers
clearSRFlags rs = rs { rSR = 0 }

-- | Sets every bit in the status register.
setSRFlags :: Registers -> Registers
setSRFlags = atSRFlags setBit

-- | Complements (toggles) every bit in the status register.
complementSRFlags :: Registers -> Registers
complementSRFlags = atSRFlags complementBit
