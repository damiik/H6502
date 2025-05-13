-- | Provides higher-level macros for common arithmetic and logic operations.
{-# LANGUAGE PatternSynonyms #-}
module Assembly.ArithmeticLogic (
    -- Arithmetic/Logic Macros
    cmp'r, and'r, cmp'y, cmp'x,
    adc'rb, add'rb, sub'rb, sub'br,
    and'rb, cmp'rz,
    decsum, binsum
) where

import Prelude hiding (and) -- Hide the 'and' function from Prelude
import Data.Word (Word8, Word16)

import Assembly.Core
    ( Asm,
      Operand(..),
      pattern A_,
      AddressRef(AddrLit8), -- Correctly import AddrLit8 as a constructor of AddressRef
      Conditions(IsNonZero) -- Only IsNonZero is used by doWhile_ with tya
      )
import Assembly.EDSLInstr (
    lda, sta, cmp, cpx, cpy, sec, sbc, clc, txa, tya,
    and, -- Assembly 'and' instruction
    doWhile_, -- Needed for decsum/binsum
    sed, cld, rts, -- Needed for decsum/binsum
    dey, adc, -- Added dey, adc
    IY(IY), -- Explicitly import the IY constructor
    (#) -- Import # operator
    )

-- --- Arithmetic and Logical Macros ---

-- | Compares the value at the given address with a literal byte value.
cmp'r :: AddressRef -> Word8 -> Asm()
cmp'r address value = do
    lda# value
    cmp address

-- | Performs a logical AND between the value at the given address and a literal byte value, storing the result in the accumulator.
and'r :: AddressRef -> Word8 -> Asm()
and'r address value = do
    lda# value
    and address

-- | Compares the value in the Y register with a literal byte value.
cmp'y :: Word8  -> Asm()
cmp'y value = do
    tya
    cmp# value

-- | Compares the value in the X register with a literal byte value.
cmp'x :: Word8  -> Asm()
cmp'x value = do
    txa
    cmp# value

-- | Adds a literal byte value to the value at the given address with carry, storing the result back at the address.
-- adding value to op with carry (op = op + value + carry)
adc'rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
adc'rb op value = do
    lda# value
    adc op
    sta op

-- | Adds a literal byte value to the value at the given address, storing the result back at the address.
-- adding value to op (op = op + value)
add'rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
add'rb op value = do
    lda# value
    clc
    adc op
    sta op

-- | Performs a logical AND between a literal byte value and the value at the given address, storing the result back at the address.
-- adding value to op (op = op + value)
and'rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
and'rb op value = do
    lda# value
    and op
    sta op

-- | Compares the value at the first address with the value at the second address.
cmp'rz :: AddressRef -> AddressRef -> Asm()
cmp'rz op1 op2 = do
    lda op1
    cmp op2

-- | Subtracts the value at the given address from a literal byte value, storing the result in the accumulator.
-- substract op from value! (A = value - op)
sub'br:: Word8 -> AddressRef -> Asm() -- op -= value :: Word8
sub'br value op = do
    lda# value
    sec
    sbc op

-- | Subtracts a literal byte value from the value at the given address, storing the result back at the address.
-- substract value from op (op = op - value)
sub'rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
sub'rb op value = do
    lda op
    sec
    sbc# value
    sta op

-- | Performs a multi-digit decimal (BCD) addition.
-- Assumes numbers are stored in memory starting at 0x40 and 0x42, with length in Y.
decsum :: Asm ()
decsum = do
        sed                              -- decimal mode
        clc                              -- clear carry
        doWhile_ IsNonZero $ do          -- while Y != 0
            dey                          -- Y--
            lda (IY (AddrLit8 0x40)) -- Get 2 decimal digits from string 1
            adc (IY (AddrLit8 0x42)) -- Add pair of digits from string 2
            sta (IY (AddrLit8 0x40)) -- Store result in string 1
            tya                          -- Y -> A (set Zero flag if Y=0)
        cld                              -- Back to binary arithmetic mode
        rts


-- | Performs a multi-digit binary addition.
-- Address of number 1 at 0040:0041.
-- Address of number 2 at 0042:0043.
-- Length of numbers (in bytes) in Index Register Y. Numbers arranged starting with most significant digits.
-- Reslult: Sum replaces number with starting address in memory locations 0040 and 0041.
binsum :: Asm ()
binsum = do
        clc                              -- clear carry
        doWhile_ IsNonZero $ do          -- while Y != 0
            dey                          -- Y--
            lda (IY (AddrLit8 0x40)) -- Get 2 decimal digits from string 1
            adc (IY (AddrLit8 0x42)) -- Add pair of digits from string 2
            sta (IY (AddrLit8 0x40)) -- Store result in string 1
            tya                          -- Y -> A (set Zero flag if Y=0)
        rts
