{-# LANGUAGE PatternSynonyms, BinaryLiterals #-}
module C64.SerialPort (
    -- Serial Port Macros
    readCia2PortB,
    writeCia2PortA,
    initSerialPort,
    serialIRQ,
    setIRQVector
) where

import Data.Word (Word8, Word16)
import Data.Bits((.&.), (.|.), shiftL, shiftR, testBit, complement, xor, (.<<.), (.>>.))
import Assembly.Core
    ( l_,
      makeUniqueLabel,
      makeLabelWithPrefix,
      zpLit,
      addr2word16,
      lsb,
      msb,
      Label,
      Address,
      AddressRef(AddrLabel, AddrLit16, AddrLit8, AddrLabelExpr),
      Asm,
      Operand(..), -- Import all constructors
      pattern Imm, -- Import the pattern synonym
      pattern AbsLabel,
      pattern ZPAddr,
      pattern ImmLsbLabel, -- Import the new pattern synonym
      pattern ImmMsbLabel, -- Import the new pattern synonym
      (.+), -- Import renamed operator
      (.-), -- Import renamed operator
      parens, -- Import parens if needed by macros (though unlikely)
      ArithExpr(add, sub), -- Import the class methods if needed directly (unlikely)
      evalLabelExpr,
      resolveAddressMaybe,
      LabelExpression(LabelRef),
      pattern IsNonZero,
      pattern IsCarry,
      Conditions(..), -- Ensure Conditions type is imported if needed by macros like while_
      db
      )
import Assembly.EDSLInstr
import Prelude hiding((+), (-), and, or) -- Keep hiding Prelude's + and - if P.(+) is used elsewhere
import qualified Prelude as P ((+), (-))
import C64 (cia2DataPortA, cia2DataPortB, cia2TimerALow, cia2TimerAHigh, cia2InterruptControl)

-- Reads a byte from CIA2 Data Port B ($DD01)
readCia2PortB :: Asm ()
readCia2PortB = do
    lda cia2DataPortB -- Load the byte from the port into the accumulator

-- Writes the byte in the accumulator to CIA2 Data Port A ($DD00)
writeCia2PortA :: Asm ()
writeCia2PortA = do
    sta cia2DataPortA -- Store the byte from the accumulator to the port

-- Initializes CIA2 ports and Timer A for basic serial communication (RS232) at ~300 baud (PAL)
initSerialPort :: Asm ()
initSerialPort = do
    -- Configure CIA2 Port A DDR ($DD02)
    -- Set Bit 2 (TXD) as output (1), others as input (0) for simplicity
    lda# 0b00000100 -- Only Bit 2 is 1
    sta $ AddrLit16 0xdd02 -- CIA2 DDRA

    -- Configure CIA2 Port B DDR ($DD03)
    -- Set Bit 0 (RXD) as input (0), Bit 1 (RTS) and Bit 2 (DTR) as output (1)
    lda# 0b00000110 -- Bit 1 and Bit 2 are 1
    sta $ AddrLit16 0xdd03 -- CIA2 DDRB

    -- Optional: Set initial states for output lines (RTS, DTR, TXD)
    -- For simplicity, let's just ensure TXD is high initially (idle state)
    lda cia2DataPortA
    ora# 0b00000100 -- Set Bit 2 (TXD)
    sta cia2DataPortA

    -- Optional: Set initial states for RTS and DTR (typically high)
    lda cia2DataPortB
    ora# 0b00000110 -- Set Bit 1 (RTS) and Bit 2 (DTR)
    sta cia2DataPortB

    -- Configure CIA2 Timer A ($DD04/$DD05) for ~300 baud (PAL)
    -- Load latch value (205 = $00CD)
    lda# 0xCD -- LSB
    sta cia2TimerALow
    lda# 0x00 -- MSB
    sta cia2TimerAHigh

    -- Configure CIA2 Control Register A ($DD0E)
    -- Bits:
    -- 0: Start/Stop (1=Start)
    -- 1: Input (0=System Clock)
    -- 2: Force Load (1=Load Latch)
    -- 3: Output Mode (0=Normal)
    -- 4: Run Mode (1=Continuous)
    -- 5: PB6 Toggle (1=Toggle PB6 on underflow)
    -- 6: PC2 Count (1=Count pulses on PC2)
    -- Okay, let's try again. To load the latch and start continuous interrupts:
    -- Load Latch (Bit 2 = 1), Start (Bit 0 = 1), Continuous (Bit 4 = 1).
    -- Value: 0b00010101 = $15

    -- Let's use $15 to load and start the timer in continuous mode for interrupts.
    lda# 0b00010101 -- Start (1), Force Load (1), Continuous (1)
    sta $ AddrLit16 0xdd0e -- CIA2 Control Register A

    -- Enable CIA2 Timer A interrupt ($DD0D)
    -- Bit 0: Timer A interrupt enable
    -- Bit 7: Interrupt enable (1 to set, 0 to clear)
    lda# 0b10000001 -- Set (Bit 7), Enable Timer A (Bit 0)
    sta cia2InterruptControl

-- Create a basic serial IRQ handler macro
serialIRQ :: Asm ()
serialIRQ = do
    -- Save registers
    pha -- Push A
    txa -- Transfer X to A
    pha -- Push X
    tya -- Transfer Y to A
    pha -- Push Y

    -- Acknowledge CIA2 interrupt
    -- Reading $DD0D acknowledges the interrupt
    lda cia2InterruptControl
    -- We might need to check which interrupt occurred if multiple are enabled,
    -- but for now, assume it's the Timer A interrupt.

    -- *** Serial Communication Logic Goes Here ***
    -- This is where the bit-banging for reading/writing would be implemented.
    -- This is complex and depends on the baud rate and protocol.
    -- For now, we'll leave this as a placeholder.

    -- Restore registers
    pla -- Pull Y
    tay -- Transfer A to Y
    pla -- Pull X
    tax -- Transfer A to X
    pla -- Pull A

    -- Return from interrupt
    rti

-- Macro to set the IRQ vector
setIRQVector :: Label -> Asm ()
setIRQVector handlerLabel = do
    -- The IRQ vector is at $FFFE/$FFFF
    -- We need to write the LSB to $FFFE and the MSB to $FFFF
    -- Use symbolic operands for the handler address
    lda #< handlerLabel
    sta $ AddrLit16 0xfffe
    lda #> handlerLabel
    sta $ AddrLit16 0xffff

    -- We also need to clear the I flag in the processor status register to enable IRQs
    cli -- Clear Interrupt Disable flag
