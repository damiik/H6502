{-# LANGUAGE PatternSynonyms, BinaryLiterals #-}
-- | This module provides functions and definitions for interacting with
-- | serial ports on the C64, including CIA #1, ACIA 6551, and CIA #2 based
-- | communication.
module C64.SerialPort (
    -- Serial Port Macros
    readCia2PortB,
    writeCia2PortA,
    initSerialPort,
    serialIRQ,
    setIRQVector,

    -- CIA1 Serial Port (from example)
    cia1TimerALo,
    cia1TimerAHi,
    cia1CtrlRegA,
    cia1IrqMaskOrIcr,
    cia1PortA,
    txBufferAddr,
    rxBufferAddr,
    bitCountAddr,
    initCia1ForSerial,
    sendCharViaCia1,
    irqHandlerCia1,
    setKernalIrqVector,
    exampleSerialUsage_CIA1,

    -- ACIA 6551 Serial Port (from user example)
    aciaSR,
    aciaRDR,
    aciaTDR,
    aciaCMD,
    aciaCTRL,
    defineHelloMsg,
    initAciaEDSL,
    sendCharAciaEDSL,
    receiveCharAciaEDSL,
    checkErrorsAciaEDSL,
    exampleAciaProgram
) where

import Data.Word (Word8, Word16)
import Data.Char (ord) -- For fromEnum
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
      ArithExpr(add, sub), -- Import the class methuse: `lda $ X, label`ods if needed directly (unlikely)
      evalLabelExpr,
      resolveAddressMaybe,
      LabelExpression(LabelRef),
      pattern IsNonZero,
      pattern IsCarry,
      Conditions(..), -- Ensure Conditions type is imported if needed by macros like while_
      db
      )
import Assembly.EDSLInstr -- Import all from EDSLInstr


import Prelude hiding((+), (-), and, or) -- Keep hiding Prelude's + and - if P.(+) is used elsewhere
import qualified Prelude as P ((+), (-))
import C64 (cia2DataPortA, cia2DataPortB, cia2TimerALow, cia2TimerAHigh, cia2InterruptControl)

-- | *** CIA #1 Based Serial Communication (from user example) ***

-- | CIA #1 Registers (as per example)
cia1TimerALo, cia1TimerAHi, cia1CtrlRegA, cia1IrqMaskOrIcr, cia1PortA :: Address -- Assuming Address is Word16
-- | Lower byte of Timer A
cia1TimerALo     = 0xDC04
-- | Higher byte of Timer A
cia1TimerAHi     = 0xDC05
-- | Control Register for Timer A
cia1CtrlRegA     = 0xDC0E
-- | IRQ Mask (write) / Interrupt Control Register (read) for CIA #1
cia1IrqMaskOrIcr = 0xDC0D
-- | Port A of CIA #1 (bit 0 = TX, bit 1 = RX, as per example)
cia1PortA        = 0xDC00

-- | Serial Protocol Configuration (as per example)
-- | BAUD_RATE = 2400 (used to derive timerCyclesForBaud)
-- | BITS_PER_CHAR = 10 (8N1: 1 start, 8 data, 1 stop)
bitsPerCharExample :: Word8
-- | Number of bits per character (8N1: 1 start, 8 data, 1 stop)
bitsPerCharExample = 10

timerCyclesForBaudExample :: Word16
-- | Timer cycles required for one bit duration at 2400 baud (1MHz CIA clock)
timerCyclesForBaudExample = 417 -- For 2400 baud at 1MHz CIA clock (1 bit = ~417 us)

-- | Zero Page Buffers and Counter (as per example)
txBufferAddr, rxBufferAddr, bitCountAddr :: Word8
-- | Zero page address for the transmission buffer
txBufferAddr = 0x02
-- | Zero page address for the reception buffer (not used in send example)
rxBufferAddr = 0x03
-- | Zero page address for the bit counter during transmission/reception
bitCountAddr = 0x04

-- | Function to set the KERNAL IRQ vector ($0314-$0315)
-- | From example:
-- | ; Wektor przerwań IRQ (adres $0314-$0315)
-- |         .org $0314
-- |         .word IRQ_Handler ; Adres handlera przerwań
setKernalIrqVector :: Label -> Asm ()
-- ^ Sets the KERNAL IRQ vector at $0314-$0315 to point to the provided handler label.
setKernalIrqVector handlerLabel = do
    -- Assembly.EDSLInstr.comment "-- Set KERNAL IRQ handler vector at $0314-$0315 --"
    lda #< handlerLabel
    sta (AddrLit16 0x0314)
    lda #> handlerLabel
    sta (AddrLit16 0x0315)

-- | Initialization of CIA#1 for Serial Communication (corresponds to 'Start' in example)
-- | Translated user comment:
-- | CIA Initialization:
-- | Timer A is configured in One-Shot mode with a value corresponding to the duration of one bit (for 2400 baud).
-- | Timer A interrupt is enabled by setting a bit in CIA1_IRQ_MASK.
initCia1ForSerial :: Asm ()
-- ^ Initializes CIA#1 and Timer A for serial communication as per the user example.
initCia1ForSerial = do
    -- Assembly.EDSLInstr.comment "--- Initialize CIA#1 for Serial Communication (from example) ---"
    -- Assembly.EDSLInstr.comment "CIA Initialization:"
    -- Assembly.EDSLInstr.comment "Timer A is configured in One-Shot mode with a value corresponding to"
    -- Assembly.EDSLInstr.comment "the duration of one bit (for 2400 baud)."
    -- Assembly.EDSLInstr.comment "Timer A interrupt is enabled by setting a bit in CIA1_IRQ_MASK."

    -- Assembly.EDSLInstr.comment "Disable interrupts globally"
    sei

    -- Assembly.EDSLInstr.comment "Disable all CIA#1 interrupts initially by writing to MASK register ($DC0D)"
    -- Assembly.EDSLInstr.comment "Example uses: LDA #$7F; STA CIA1_IRQ_MASK"
    -- Assembly.EDSLInstr.comment "Value $7F means bits 0-4 are 0 (disable int.), bit 7 is 0 (clear mode for mask bits)."
    lda #0x7F
    sta (AddrLit16 cia1IrqMaskOrIcr)

    -- Assembly.EDSLInstr.comment "Configure Timer A Control Register ($DC0E) for One-Shot mode"
    -- Assembly.EDSLInstr.comment "Example uses: LDA #%00000001; STA CIA1_CTRL_REG_A"
    -- Assembly.EDSLInstr.comment "Value %00000001: Bit 0 (Start Timer A)=1. Other bits relevant to mode (RunMode=0 for one-shot) are 0."
    lda #0b00000001
    sta (AddrLit16 cia1CtrlRegA)

    -- Assembly.EDSLInstr.comment "Set Timer A latch for 2400 baud (1 bit = ~417 us)"
    -- Assembly.EDSLInstr.comment "Example uses: LDA #<417; STA CIA1_TIMER_A_LO; LDA #>417; STA CIA1_TIMER_A_HI"
    lda #lsb timerCyclesForBaudExample
    sta (AddrLit16 cia1TimerALo)
    lda #msb timerCyclesForBaudExample
    sta (AddrLit16 cia1TimerAHi)

    -- Assembly.EDSLInstr.comment "Enable interrupts globally"
    cli

    -- Assembly.EDSLInstr.comment "Enable Timer A interrupt in CIA#1 IRQ Mask Register ($DC0D)"
    -- Assembly.EDSLInstr.comment "Example uses: LDA #%00000010; STA CIA1_IRQ_MASK"
    -- Assembly.EDSLInstr.comment "This implies bit 1 of MASK register enables Timer A interrupt (differs from standard CIA where bit 0 is Timer A)."
    -- Assembly.EDSLInstr.comment "Writing %00000010 with bit 7 implicitly 0 means CLEARING mask bits except bit 1."
    -- Assembly.EDSLInstr.comment "To SET mask bit 1 (enable interrupt), it should be #%10000010."
    -- Assembly.EDSLInstr.comment "Following example's literal value, assuming specific non-standard behavior or interpretation."
    lda #0b10000010 -- Corrected based on standard CIA behavior and example comment intent
    sta (AddrLit16 cia1IrqMaskOrIcr)

-- | Send Character Routine for CIA#1 (corresponds to 'SendChar' in example)
-- | Translated user comment (part 1 for SendChar):
-- | Data Transmission:
-- | The SendChar function sends an ASCII character via the serial port:
-- | First, it sends the start bit (low state).
sendCharViaCia1 :: Asm ()
-- ^ Sends a single character via the CIA#1 serial port, assuming the character is in the transmission buffer.
sendCharViaCia1 = do
    l_ "SendChar_CIA1"
    -- Assembly.EDSLInstr.comment "--- SendChar routine for CIA#1 (from example) ---"
    -- Assembly.EDSLInstr.comment "Data Transmission (SendChar part):"
    -- Assembly.EDSLInstr.comment "The SendChar function sends an ASCII character via the serial port:"
    -- Assembly.EDSLInstr.comment "First, it sends the start bit (low state)."
    -- Assembly.EDSLInstr.comment "Assumes character to send is already in TxBuffer (ZP: $02)"

    -- Assembly.EDSLInstr.comment "Set TX line for Start Bit (low state)"
    -- Assembly.EDSLInstr.comment "Example: LDA #%00000001; STA CIA1_PORT_A"
    -- Assembly.EDSLInstr.comment "This is unusual if port bit 0 is TX and 0=low, 1=high."
    -- Assembly.EDSLInstr.comment "Literal translation of example code."
    lda #0b00000001 -- Value for Port A to make TX line represent start bit
    sta (AddrLit16 cia1PortA)

    -- Assembly.EDSLInstr.comment "Initialize bit counter"
    -- Assembly.EDSLInstr.comment "Example: LDA #BITS_PER_CHAR; STA BitCount (BITS_PER_CHAR = 10)"
    lda #bitsPerCharExample
    sta bitCountAddr -- Pass Word8 directly

    -- Assembly.EDSLInstr.comment "Start Timer A to time the first bit (start bit's duration)"
    -- Assembly.EDSLInstr.comment "Example: LDA #%00000011; STA CIA1_CTRL_REG_A"
    -- Assembly.EDSLInstr.comment "Value %00000011 for CIA_CTRL_REG_A ($DC0E):"
    -- Assembly.EDSLInstr.comment "  Bit 0 (START) = 1 (Start timer)"
    -- Assembly.EDSLInstr.comment "  Bit 1 (PBON)  = 1 (Timer A output on PB6)"
    -- Assembly.EDSLInstr.comment "Timer runs in mode previously set (one-shot). Restarting one-shot reloads latch."
    lda #0b00000011
    sta (AddrLit16 cia1CtrlRegA)
    rts

-- | IRQ Handler for CIA#1 Serial Communication (corresponds to 'IRQ_Handler' in example)
-- | Translated user comments (part 2 for IRQ_Handler):
-- | Data Transmission (IRQ part):
-- | Then, it transmits subsequent data bits (LSB first).
-- | After each bit, the timer is restarted to ensure synchronization.
-- | Interrupt Handler:
-- | At each interrupt, the handler checks if Timer A has reached zero (underflowed).
-- | If so, it transmits the next data bit via CIA1_PORT_A.
-- | After all bits have been transmitted (10 bits in 8N1 protocol), the transmission ends.
irqHandlerCia1 :: Asm ()
-- ^ Interrupt handler for CIA#1 Timer A, used for serial bit transmission.
irqHandlerCia1 = do
    l_ "IRQ_Handler_CIA1"
    -- Assembly.EDSLInstr.comment "--- IRQ Handler for CIA#1 Timer A (from example) ---"
    -- Assembly.EDSLInstr.comment "Data Transmission (IRQ part):"
    -- Assembly.EDSLInstr.comment "Then, it transmits subsequent data bits (LSB first)."
    -- Assembly.EDSLInstr.comment "After each bit, the timer is restarted to ensure synchronization."
    -- Assembly.EDSLInstr.comment "Interrupt Handler:"
    -- Assembly.EDSLInstr.comment "At each interrupt, the handler checks if Timer A has reached zero (underflowed)."
    -- Assembly.EDSLInstr.comment "If so, it transmits the next data bit via CIA1_PORT_A."
    -- Assembly.EDSLInstr.comment "After all bits have been transmitted (10 bits in 8N1 protocol), the transmission ends."

    -- Save registers
    pha
    txa
    pha
    tya
    pha

    -- Check interrupt source
    -- Assembly.EDSLInstr.comment "Check source of interrupt - expecting Timer A from CIA#1"
    -- Assembly.EDSLInstr.comment "Example reads CIA1_IRQ_MASK ($DC0D), which is ICR when read."
    -- Assembly.EDSLInstr.comment "Example then checks bit 1 (AND #%00000010)."
    -- Assembly.EDSLInstr.comment "This implies Timer A flag is bit 1 of ICR (differs from standard CIA)."
    exitIsrLabel <- makeUniqueLabel ()
    lda (AddrLit16 cia1IrqMaskOrIcr) -- Read ICR from $DC0D
    and #0b00000010     -- Check bit 1
    beq exitIsrLabel     -- If not set (not Timer A interrupt as per example's logic), exit

    -- Handle transmission bit
    -- Assembly.EDSLInstr.comment "Timer A interrupt occurred, handle serial transmission bit"
    txDoneLabel <- makeUniqueLabel ()
    lda bitCountAddr -- Pass Word8 directly
    beq txDoneLabel      -- If bit count is 0, transmission of this char is done

    -- Assembly.EDSLInstr.comment "Shift out next bit from TxBuffer (ZP: $02)"
    sendLowLabel <- makeUniqueLabel ()
    setTxLabel   <- makeUniqueLabel ()
    lda txBufferAddr -- Pass Word8 directly
    lsr (Nothing :: Maybe Word8) -- Disambiguate for accumulator
    sta txBufferAddr -- Pass Word8 directly
    bcc sendLowLabel     -- If Carry=0 (bit was 0), send LOW state on TX

    -- Send HIGH bit (data bit was 1)
    -- Assembly.EDSLInstr.comment "Send HIGH state on TX line (for data bit 1)"
    -- Assembly.EDSLInstr.comment "Example: LDA #%00000001; STA CIA1_PORT_A"
    lda #0b00000001
    jmp setTxLabel

    l_ sendLowLabel
    -- Assembly.EDSLInstr.comment "Send LOW state on TX line (for data bit 0)"
    -- Assembly.EDSLInstr.comment "Example: LDA #%00000000; STA CIA1_PORT_A"
    lda #0b00000000
    -- Fall through to SetTX

    l_ setTxLabel
    -- Assembly.EDSLInstr.comment "Set TX line on CIA1_PORT_A with value in A"
    sta (AddrLit16 cia1PortA)

    -- Assembly.EDSLInstr.comment "Decrement bit counter"
    dec bitCountAddr -- Pass Word8 directly

    -- Assembly.EDSLInstr.comment "Restart Timer A for the next bit's duration"
    -- Assembly.EDSLInstr.comment "Example: LDA #%00000011; STA CIA1_CTRL_REG_A (Start Timer, PBOn=1)"
    lda #0b00000011
    sta (AddrLit16 cia1CtrlRegA)
    -- RTI is not here; it's at TxDone or Exit_ISR. Control flows to next interrupt.

    l_ txDoneLabel
    -- Assembly.EDSLInstr.comment "Transmission of one character complete (all bits timed)"
    -- Assembly.EDSLInstr.comment "Restore registers and return from interrupt"
    pla
    tay
    pla
    tax
    pla
    rti

    l_ exitIsrLabel
    -- Assembly.EDSLInstr.comment "Not the expected Timer A interrupt (or spurious), exit"
    -- Assembly.EDSLInstr.comment "Restore registers and return from interrupt"
    pla
    tay
    pla
    tax
    pla
    rti

-- | Example usage of the CIA1 serial routines (derived from example's main program)
exampleSerialUsage_CIA1 :: Label -> Char -> Asm ()
-- ^ Provides an example program using the CIA1 serial transmission routines.
exampleSerialUsage_CIA1 irqHandlerLabel charToSend = do
    -- Assembly.EDSLInstr.comment "--- Example Main Program Logic for CIA1 Serial (from example) ---"
    l_ "Start_CIA1_Example"

    -- Assembly.EDSLInstr.comment "Initialize CIA1 and set up KERNAL IRQ vector"
    -- Assembly.EDSLInstr.comment "This combines initCia1ForSerial and setKernalIrqVector for a complete setup"
    initCia1ForSerial -- Sets up CIA1, Timer A, enables global interrupts (CLI)
    setKernalIrqVector irqHandlerLabel -- Sets $0314/$0315 to point to our IRQ handler

    -- Assembly.EDSLInstr.comment "Prepare data to send"
    -- Assembly.EDSLInstr.comment "Example: LDA #'A'; STA TxBuffer"
    lda #fromIntegral (ord charToSend) -- Load ASCII value of char
    sta txBufferAddr          -- Store to TxBuffer (ZP: $02) - Pass Word8 directly

    -- Assembly.EDSLInstr.comment "Call SendChar routine"
    -- Assembly.EDSLInstr.comment "Example: JSR SendChar"
    jsr (AddrLabel "SendChar_CIA1")    -- Call the send routine

    -- Assembly.EDSLInstr.comment "Infinite loop (as in example)"
    loopLabel <- makeUniqueLabel ()
    l_ loopLabel
    jmp loopLabel



-- | *** ACIA 6551 Based Serial Communication (from user example) ***

-- | ACIA 6551 Registers (base address $DE00 as per user example)
aciaSR, aciaRDR, aciaTDR, aciaCMD, aciaCTRL :: Address
-- | ACIA Status Register (Read) / Control Register (Write, but different bits)
aciaSR   = 0xDE00
-- | ACIA Receiver Data Register (Read)
aciaRDR  = 0xDE00
-- | ACIA Transmitter Data Register (Write)
aciaTDR  = 0xDE01
-- | ACIA Command Register (Write)
aciaCMD  = 0xDE02
-- | ACIA Control Register (Write) - Note: Example uses $DE03 for CTRL, $DE00 for SR.
-- | Standard ACIA 6551 typically has SR/RDR at base and TDR/CR at base+1.
-- | The example uses $DE00 for SR/RDR, $DE01 for TDR, $DE02 for CMD, $DE03 for CTRL.
-- | This is unusual. Following user's example for addresses.
aciaCTRL = 0xDE03

-- | Define HELLO_MSG string
defineHelloMsg :: Asm Label
-- ^ Defines a null-terminated "Hello from C64!" string in memory and returns its label.
defineHelloMsg = do
    label <- makeLabelWithPrefix "HELLO_MSG"
    l_ label
    db $ map (fromIntegral . ord) "Hello from C64!\r\n\0" -- Null-terminated for BEQ in example
    return label

-- | Initialize ACIA (8N1, 9600 baud - as per example's InitACIA)
-- | Example's InitACIA:
-- | LDA #%00000011 ; Command: 8N1, no parity, 1 stop bit -> STA ACIA_CMD ($DE02)
-- | LDA #%00000111 ; Control: Enable transmitter/receiver -> STA ACIA_CTRL ($DE03)
-- | Note: Baud rate setting is usually part of Control Register, not Command.
-- | The example values for CMD and CTRL seem to conflate/simplify standard ACIA setup.
-- | Following example's direct register writes.
initAciaEDSL :: Asm ()
-- ^ Initializes the ACIA 6551 serial port according to the user example's configuration.
initAciaEDSL = do
    l_ "initAciaEDSL"
    -- Command: 8N1, no parity, 1 stop bit (as per example's comment for CMD)
    -- However, value %00000011 for CMD usually means:
    -- Bits 1-0: Counter Divide Select (00 = /1, 01 = /16, 10 = /64, 11 = Master Reset)
    -- This would be Master Reset.
    -- For 8N1, no parity, 1 stop bit, it's usually set in Control Register.
    -- Example's LDA #%00000011 ; STA ACIA_CMD
    lda #0b00000011
    sta (AddrLit16 aciaCMD)

    -- Control: Enable transmitter/receiver (as per example's comment for CTRL)
    -- Value %00000111 for CTRL usually means:
    -- Bits 4-0: Baud rate select (e.g. 00001 for 110 baud with 1.8432MHz crystal)
    -- Bits 6-5: Word select (e.g. 10 for 8 bits, 1 stop bit)
    -- Bit 7: Receive Interrupt Enable
    -- Example's LDA #%00000111 ; STA ACIA_CTRL
    lda #0b00000111
    sta (AddrLit16 aciaCTRL)
    rts

-- | Send a single character via ACIA
-- | Example's SendChar:
-- | PHA
-- | CheckTBE: LDA ACIA_SR; AND #%00100000 (TBE); BEQ CheckTBE
-- | PLA; STA ACIA_TDR; RTS
sendCharAciaEDSL :: Asm ()
-- ^ Sends a single character via the ACIA 6551 serial port (blocking until transmit buffer empty).
sendCharAciaEDSL = do
    l_ "sendCharAciaEDSL"
    pha
    checkTBELabel <- makeLabelWithPrefix "CheckTBE_ACIA"
    l_ checkTBELabel
    lda (AddrLit16 aciaSR)
    and #0b00100000 -- Check TBE (Transmit Buffer Empty - bit 5)
    beq checkTBELabel
    pla
    sta (AddrLit16 aciaTDR)
    rts

-- | Receive a single character (blocking)
-- | Example's ReceiveChar:
-- | CheckRDR: LDA ACIA_SR; AND #%00000010 (RDR); BEQ CheckRDR
-- | LDA ACIA_RDR; RTS
receiveCharAciaEDSL :: Asm ()
-- ^ Receives a single character from the ACIA 6551 serial port (blocking until data is ready).
receiveCharAciaEDSL = do
    l_ "receiveCharAciaEDSL"
    checkRDRLabel <- makeLabelWithPrefix "CheckRDR_ACIA"
    l_ checkRDRLabel
    lda (AddrLit16 aciaSR)
    and #0b00000010 -- Check RDR (Receive Data Ready - bit 1)
    beq checkRDRLabel
    lda (AddrLit16 aciaRDR) -- Read received byte
    rts

-- | Error Handling (optional, as per example)
-- | Example's CheckErrors:
-- | LDA ACIA_SR; AND #%00001110 (PE, FE, OE); BEQ NoError
-- | ; Handle error
-- | LDA #%00000111; STA ACIA_CTRL
-- | NoError: RTS
checkErrorsAciaEDSL :: Asm ()
-- ^ Checks the ACIA Status Register for Parity, Framing, or Overrun errors.
checkErrorsAciaEDSL = do
    l_ "checkErrorsAciaEDSL"
    noErrorLabel <- makeLabelWithPrefix "NoError_ACIA"
    lda (AddrLit16 aciaSR)
    and #0b00001110 -- Check PE (bit 2), FE (bit 3), OE (bit 4)
    beq noErrorLabel
    -- Handle error (example just re-enables transmitter/receiver)
    lda #0b00000111
    sta (AddrLit16 aciaCTRL)
    l_ noErrorLabel
    rts

-- | Example ACIA Program (mirrors user's assembly example)
exampleAciaProgram :: Asm ()
-- ^ An example program demonstrating sending a string and echoing received characters using the ACIA.
exampleAciaProgram = do
    l_ "exampleAciaProgram"
    sei -- Disable interrupts

    jsr (AddrLabel "initAciaEDSL") -- Corrected to camelCase if hlint applied it

    -- helloMsgLabel <- defineHelloMsg -- Define and get label for HELLO_MSG

    ldx #0
    sendLoopLabel <- makeLabelWithPrefix "SendLoop_ACIA"
    receiveLoopLabel <- makeLabelWithPrefix "ReceiveLoop_ACIA"
    l_ sendLoopLabel
    lda$ X (AddrLabel "HELLO_MSG") -- Using the AbsXLabel pattern - Correct EDSL syntax for LDA label,X is unknown.
    beq receiveLoopLabel           -- If char is null, end of string
    jsr (AddrLabel "sendCharAciaEDSL") -- Corrected to camelCase
    inx
    -- BNE SendLoop (original example has BNE, but LDA,X then BEQ is more common for strings)
    -- For direct translation of BNE, we'd need a CMP or similar.
    -- Assuming the BEQ handles the loop termination correctly for null-terminated string.
    jmp sendLoopLabel -- If not BEQ, continue loop

    l_ receiveLoopLabel
    jsr (AddrLabel "receiveCharAciaEDSL") -- Wait for incoming byte; Corrected to camelCase
    -- The received byte is in A
    jsr (AddrLabel "sendCharAciaEDSL")    -- Echo back received byte (which is in A); Corrected to camelCase
    jmp receiveLoopLabel




-- | *** CIA #2 Based Serial Communication (existing code) ***

-- | Reads a byte from CIA2 Data Port B ($DD01)
readCia2PortB :: Asm ()
-- ^ Reads a byte from CIA2 Data Port B ($DD01) into the accumulator.
readCia2PortB = do
    lda cia2DataPortB -- Load the byte from the port into the accumulator

-- | Writes the byte in the accumulator to CIA2 Data Port A ($DD00)
writeCia2PortA :: Asm ()
-- ^ Writes the byte in the accumulator to CIA2 Data Port A ($DD00).
writeCia2PortA = do
    sta cia2DataPortA -- Store the byte from the accumulator to the port

-- | Initializes CIA2 ports and Timer A for basic serial communication (RS232) at ~300 baud (PAL)
initSerialPort :: Asm ()
-- ^ Initializes CIA2 ports and Timer A for basic serial communication (RS232).
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

-- | Create a basic serial IRQ handler macro
serialIRQ :: Asm ()
-- ^ Basic serial IRQ handler placeholder for CIA2.
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

-- | Macro to set the IRQ vector
setIRQVector :: Label -> Asm ()
-- ^ Sets the system IRQ vector at $FFFE/$FFFF to point to the provided handler label.
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
