{-# LANGUAGE PatternSynonyms, BinaryLiterals, FlexibleInstances #-}
module C64.HelpersC64 (
    -- C64 Hardware Interaction Macros
    waitRaster,
    vicWaitLine,
    vicWaitBottom,
    vicWaitTop,
    fillScreen,
    configureVectors,
    printChar,
    printColorChar,
    printByte,
    hundreds2Petscii,
    tens2Petscii,
    macrosLib,
    -- and'r, cmp'r, -- Removed from export as they are local helpers

    -- Internal Helpers
    makeUniqueLabel, -- Keeping these here as requested, but they might be better in a shared module
    makeLabelWithPrefix,
    skipNext2B
) where

import Control.Monad (when)
import Data.Word (Word8, Word16)
import Data.Bits((.&.), shiftR) -- Needed for bitwise ops in vicWaitLine
import Data.Char (ord, isDigit, isAsciiUpper, isAsciiLower) -- Needed for PETSCII conversion
import Prelude (($), (*), fromIntegral, error) -- Explicitly import $, *, and fromIntegral
import qualified Prelude as P ((+), (*)) -- Needed for printByte address calculation (qualified *)

import Assembly.Core
    ( Asm,
      Operand(OpAbsX, OpAbsY, OpZPX), -- Correctly import specific Operand constructors
      pattern A_, -- Needed for swapB (kept here as it was originally)
      AddressRef(AddrLabel, AddrLit16, AddrLit8, AddrLabelExpr), -- Need AddrLabel, AddrLit16, AddrLit8, and AddrLabelExpr
      (.+), -- Needed for address arithmetic
      lsb, msb, -- Needed for configureVectors, printByte
      Address, -- Needed for fillMemory, printColorChar, printByte
      Label, -- Needed for configureVectors, printChar, printByte, fillMemory, l_
      makeUniqueLabel, makeLabelWithPrefix, l_, -- Label generation and definition
      Conditions(IsNonZero, IsZero, IsCarry), -- Needed for while_/doWhile_ and hundreds/tens2Petscii conditions
      LabelExpression(LabelRef), -- Needed for configureVectors
      db -- Added db to Assembly.Core import
      )
import Assembly.EDSLInstr (
    lda, sta, cmp, and, -- Basic instructions
    ldx, ldy, cpx, -- Instructions with immediate for loops/counters (removed #)
    inx, iny, dex, -- Increments/Decrements
    while_, doWhile_, -- Control flow macros
    jsr, rts, -- Subroutine calls and return
    (#), -- Immediate addressing operator
    asl, rol, -- Bitwise operations
    IY(IY), -- Indexed addressing mode constructor
    X(X), -- Indexed addressing mode constructor
    Y(Y), -- Indexed addressing mode constructor
    OpGenerator(toOperand), -- Import OpGenerator class
    (#<), (#>), -- Import #< and #> operators
    tay, sec, sbc, inc, adc, clc -- Added missing instructions
    )

import Assembly.Memory

import C64 (vicRaster, screenRam, colorRam, vicControl1) -- C64-specific addresses
-- Import staRb from Assembly.Memory if it's used by hundreds2Petscii and tens2Petscii
-- import Assembly.Memory (staRb)


-- --- C64 Hardware Interaction Macros ---

vicWaitBottom :: Asm ()
vicWaitBottom = do
    while_ IsZero $ do
        and'r vicControl1 0x80 -- Renamed and_r

vicWaitTop :: Asm ()
vicWaitTop = do
    while_ IsNonZero $ do
        and'r vicControl1 0x80 -- Renamed and_r

-- Need and'r helper defined here or imported
and'r :: AddressRef -> Word8 -> Asm()
and'r address value = do
    lda# value
    and address


waitRaster :: Asm()
waitRaster = do
    -- vicWaitTop -- The original commented this out
    vicWaitBottom -- Waiting for bottom of raster

-- On a PAL C64 (common in Europe/Australia), the raster beam counts from line 0 up to line 311.
-- On an NTSC C64 (common in North America/Japan), it counts from line 0 up to line 262.
vicWaitLine :: Word16 -> Asm()
vicWaitLine line = do
    l_ "vicWaitLine"
    let lineLsb = lsb line
    let msbBitFlag = lsb ((line `shiftR` 1) .&. 0x80) -- "compile time" precalculate Word8 msb bit 9 of line (Word16)
    doWhile_ IsNonZero $ do         -- Repeat the whole process if the 9th bit check fails.
        doWhile_ IsNonZero $ do
            cmp'r vicRaster lineLsb -- Wait until VIC raster register ($D012) matches the target line. -- Renamed cmp_r
                                    -- $D012 == line (Z=1 from last CMP). Now check the 9th bit (bit 7 of $D011)
        lda vicControl1             -- Load VIC control register 1 ($D011)
        and# 0x80                   -- Isolate bit 7. Sets Z flag (Z=1 if bit 7 is 0).
        cmp# msbBitFlag                -- The outer doWhile_ IsNonZero will loop if Z=0 (bit 7 is 1).

-- Need cmp'r helper defined here or imported
cmp'r :: AddressRef -> Word8 -> Asm()
cmp'r address value = do
    lda# value
    cmp address


fillScreen :: AddressRef  -> Word8 -> Asm ()
fillScreen screenAddr fillB = do
    lda# fillB
    ldx# 250
    while_ IsNonZero $ do -- This should probably be doWhile_ as it checks after dex
        dex
        sta (X screenAddr)
        sta (X (screenAddr .+ 250))
        sta (X (screenAddr .+ 500))
        sta (X (screenAddr .+ 750))

-- The OpGenerator instance for X AddressRef is already defined in Assembly.EDSLInstr, do not redefine it here.
-- instance OpGenerator (X AddressRef) where
--     toOperand (X addr) =
--         case addr of
--             AddrLit8 v -> OpZPX (AddrLit8 v)
--             _          -> OpAbsX addr


-- Need OpGenerator class imported from Assembly.EDSLInstr or Assembly.Core (likely EDSLInstr)
-- class OpGenerator a where -- Already imported from EDSLInstr
--    toOperand :: a -> Operand


-- Updated configureVectors to accept AddressRef and use symbolic LSB/MSB loading
configureVectors :: AddressRef -> Asm ()
configureVectors addrRef = do
    -- Extract the label name. Raise error if it's not a label reference.
    let labelName = case addrRef of
            AddrLabel l -> l
            AddrLabelExpr (LabelRef l) -> l -- Need LabelExpression and LabelRef
            _ -> error "configureVectors requires a label reference (AddrLabel or AddrLabelExpr (LabelRef ...))"

    -- IRQ Vector -> Use symbolic LSB/MSB operands
    -- lda $ ImmLsbLabel labelName -- Old syntax
    -- sta $ AddrLit16 0x0314
    -- lda $ ImmMsbLabel labelName -- Old syntax
    -- sta $ AddrLit16 0x0315

    -- NMI Vector -> Use symbolic LSB/MSB operands
    -- $0318/$0319 is the RAM NMI vector, mirroring $FFFA/$FFFB.
    -- lda#  lsb (resolveAddressMaybe labelName) -- Old syntax
    lda #< labelName -- Using the #< operator
    sta (AddrLit16 0x0318)
    lda #> labelName -- Using the #> operator
    sta (AddrLit16 0x0319)

    -- lda# 0xFF -- Commented out in original
    -- sta$ AddrLit16 0xFFFE
    -- sta$ AddrLit16 0xFFFF

    -- BRK Vector -> Use symbolic LSB/MSB operands
    -- lda $ ImmLsbLabel labelName -- Old syntax
    -- sta $ AddrLit16 0x0316
    -- lda $ ImmMsbLabel labelName -- Old syntax
    -- sta $ AddrLit16 0x0317

-- Need LabelExpression and LabelRef from Assembly.Core
-- Need #< and #> from Assembly.EDSLInstr

printChar :: Word16 -> Word8 -> Asm()
printChar textPos color = do
    -- Assuming accumulator holds the character to print
    sta (X (screenRam .+ textPos))    -- Store the character at the screen memory location
    lda# color
    sta (X (colorRam .+ textPos))    -- Store the color at the screen color memory location

-- Need X constructor/OpGenerator instance, screenRam, colorRam from C64, and .+ from Assembly.Core

printColorChar :: Word16 -> Address -> Asm()
printColorChar textPos colorMap = do
    -- Assuming accumulator holds the character to print
    sta (X (screenRam .+ textPos))
    tay -- Need tay
    lda (Y colorMap) -- Need Y constructor/OpGenerator instance
    sta (X (colorRam .+ textPos))

-- Need X, Y constructors/OpGenerator instances, screenRam, colorRam from C64, .+ from Assembly.Core, and tay instruction

macrosLib = do
    l_ "hundreds2Petscii" -- Need l_ from Assembly.Core
    hundreds2Petscii
    l_ "tens2Petscii"
    tens2Petscii

-- returns petscii character of hundreds of value from accumulator
-- returns rest of value in 0xf8
-- Assumes accumulator holds the value on entry
hundreds2Petscii :: Asm ()
hundreds2Petscii = do
    let count = AddrLit8 0xf7 -- Need AddrLit8 from Assembly.Core
    let rest = AddrLit8 0xf8

    -- sta rest -- Commented out in original? No, it's not. It's missing here.
    -- Need sta instruction

    -- staRb count 0 -- Assuming staRb is available in Assembly.Memory
    sta'rb count 0 -- Use direct instruction instead of cross-module macro for now

    lda rest -- Need lda instruction
    sec -- Need sec instruction
    sbc# 100 -- Need sbc instruction and # operator
    while_ IsCarry $ do -- Need while_ and IsCarry from Assembly.Core/EDSLInstr
        inc count -- Need inc instruction
        sbc# 100
    adc# 100 -- undo last dec -- Need adc instruction
    sta rest -- Need sta instruction
    lda count -- Need lda instruction
    clc -- Need clc instruction
    adc# 0x30        -- Convert the count to ASCII
    rts -- Need rts instruction

-- Need lda, sta, sec, sbc, #, while_, IsCarry, inc, adc, clc, rts, AddrLit8, Conditions(IsCarry)

-- return petscii character of tens of accumulator, (max value of accumulator have to be < 99)
-- return rest of value in 0xf8
-- Assumes accumulator holds the value on entry
tens2Petscii :: Asm ()
tens2Petscii = do
    let count = AddrLit8 0xf7
    let rest = AddrLit8 0xf8

    sta'rb count 0 -- Use direct instruction instead of cross-module macro for now(AddrLit8 0xf7) -- Use direct instruction instead of cross-module macro for now
    lda rest -- Need lda instruction
    sec -- Need sec instruction
    sbc# 10 -- Need sbc instruction and # operator
    while_ IsCarry $ do -- Need while_ and IsCarry
        inc count -- Need inc instruction
        sbc# 10
    adc# 10 -- undo last dec -- Need adc instruction
    sta rest -- Need sta instruction
    lda count -- Need lda instruction
    clc -- Need clc instruction
    adc# 0x30        -- Convert the count to ASCII
    rts -- Need rts instruction

-- Need lda, sta, sec, sbc, #, while_, IsCarry, inc, adc, clc, rts, AddrLit8, Conditions(IsCarry)

--- prints decimal byte at x y coords
--- (needs *macrosLib*)
-- Assumes value to print is in accumulator on entry
printByte :: Word16 -> Word16 -> Address -> Asm()
printByte x y color = do
    let rest = AddrLit8 0xf8 -- Need AddrLit8
    let screenAddress = y * 0x40 P.+ x -- Need P.* and P.+ from qualified Prelude
    jsr "hundreds2Petscii" -- Need jsr
    ldx# 0 -- Need ldx#
    printColorChar screenAddress color  --Print a character from the hellotext string at the specified position
    jsr "tens2Petscii"
    ldx# 1
    printColorChar screenAddress color  --Print a character from the hellotext string at the specified position

    lda rest -- Need lda
    clc -- Need clc
    adc# 0x30 -- Need adc#
    ldx# 2 -- Need ldx#
    printColorChar screenAddress color  --Print a character from the hellotext string at the specified position
    rts -- Need rts

-- Need AddrLit8, P.*, P.+, jsr, ldx#, printColorChar, lda, clc, adc#, rts

-- --- Internal Helpers / Utilities ---

-- makeUniqueLabel and makeLabelWithPrefix are imported from Assembly.Core

skipNext2B :: Asm ()
skipNext2B = do
    db [0x2C] -- Instruction BIT $address (direct addr. mode) -- Need db from Assembly.Core

-- Need db from Assembly.Core