{-# LANGUAGE BinaryLiterals    #-}
module C64 (
    screenRam, colorRam, 
    vicMemoryControl,vicRaster,
    vicBorderColor, vicBackgroundColor, vicBackgroundColor1, vicBackgroundColor2, vicBackgroundColor3,
    cia1DataPortA, cia1InterruptControl, cia2InterruptControl,
    cia2DataPortA, cia2DataPortB, cia2TimerALow, cia2TimerAHigh,
    kernalClrscr, kernalGetin,
    keyCursorDown, keyCursorRight,
    keyCursorUp, keyCursorLeft,
    vicControl1, vicControl2,
    vicSprite0X, vicSprite0Y,
    vicSprite1X, vicSprite1Y,
    vicSprite2X, vicSprite2Y,
    vicSprite3X, vicSprite3Y,
    vicSprite4X, vicSprite4Y,
    vicSprite5X, vicSprite5Y,
    vicSprite6X, vicSprite6Y,
    vicSprite7X, vicSprite7Y,
    vicSpriteMSBX,  vicSpriteEnable,
    vicSpritePriority, vicSpriteMulticolor, vicSpriteDoubleWidth, vicSpriteDoubleHeight, -- Added Sprite properties
    vicSpriteSpriteColision, vicSpriteBackColision, -- Added Sprite collision registers
    vicSpriteMc0, vicSpriteMc1, -- Added Sprite multicolor registers
    vicSprite0Color, vicSprite1Color, vicSprite2Color, vicSprite3Color, -- Added Sprite color registers
    vicSprite4Color, vicSprite5Color, vicSprite6Color, vicSprite7Color,
    _BLACK, _WHITE, _RED, _CYAN, _PURPLE,
    _GREEN, _BLUE, _YELLOW, _ORANGE, _BROWN,
    _PINK, _DARK_GREY, _GREY, _LIGHT_GREEN,
    _LIGHT_BLUE, _LIGHT_GREY,
    scanKeyboard,
    _KEY_SPACE,
    _KEY_RETURN,
    _KEY_CURSOR_DOWN,
    _KEY_CURSOR_RIGHT,
    _KEY_A,
    _KEY_B,
    _KEY_C,
    _KEY_D,
    _KEY_E,
    _KEY_F,
    _KEY_G,
    _KEY_H,
    _KEY_I,
    _KEY_J,
    _KEY_K,
    _KEY_L,
    _KEY_M,
    _KEY_N,
    _KEY_O,
    _KEY_P,
    _KEY_Q,
    _KEY_R,
    _KEY_S,
    _KEY_T,
    _KEY_U,
    _KEY_V,
    _KEY_W,
    _KEY_X,
    _KEY_Y,
    _KEY_Z,
    _KEY_0,
    _KEY_1,
    _KEY_2,
    _KEY_3,
    _KEY_4,
    _KEY_5,
    _KEY_6,
    _KEY_7,
    _KEY_8,
    _KEY_9,
    _KEY_PLUS,
    _KEY_MINUS,
    _KEY_POUND,
    _KEY_ASTERISK,
    _KEY_AT,
    _KEY_COLON,
    _KEY_SEMICOLON,
    _KEY_COMMA,
    _KEY_PERIOD,
    _KEY_EQUALS,
    _KEY_L_SHIFT,
    _KEY_R_SHIFT,
    _KEY_UP_ARROW,
    _KEY_F1,
    _KEY_F3,
    _KEY_F5,
    _KEY_F7,



) where
import Prelude hiding (and) -- Hide 'and' from Prelude
import Control.Monad (replicateM_)
import Data.Word
import Assembly.Core
import Assembly.EDSLInstr
-- Removed cyclic import of C64.SerialPort




-- | Address of the screen RAM.
screenRam :: AddressRef
screenRam = AddrLit16 0x0400

-- | Address of the color RAM.
colorRam :: AddressRef
colorRam = AddrLit16 0xd800


-- | Address of CIA1 Data Port A (Joystick Port 2, unused).
cia1DataPortA :: AddressRef
cia1DataPortA = AddrLit16 0xdc00 -- Joystick Port 2 (unused)
-- | Address of CIA1 Data Port B (Joystick Port 2, unused).
cia1DataPortB :: AddressRef
cia1DataPortB = AddrLit16 0xdc01 -- Joystick Port 2 (unused)

-- | Address of CIA1 Interrupt Control Register.
cia1InterruptControl :: AddressRef
cia1InterruptControl = AddrLit16 0xdc0d
-- | Address of CIA2 Interrupt Control Register.
cia2InterruptControl :: AddressRef
cia2InterruptControl = AddrLit16 0xdd0d


-- Port A, serial bus access. Bits:

-- Bits #0-#1: VIC bank. Values:
-- %00, 0: Bank #3, $C000-$FFFF, 49152-65535.
-- %01, 1: Bank #2, $8000-$BFFF, 32768-49151.
-- %10, 2: Bank #1, $4000-$7FFF, 16384-32767.
-- %11, 3: Bank #0, $0000-$3FFF, 0-16383.

-- Bit #2: RS232 TXD line, output bit.
-- Bit #3: Serial bus ATN OUT; 0 = High; 1 = Low.
-- Bit #4: Serial bus CLOCK OUT; 0 = High; 1 = Low.
-- Bit #5: Serial bus DATA OUT; 0 = High; 1 = Low.
-- Bit #6: Serial bus CLOCK IN; 0 = Low; 1 = High.
-- Bit #7: Serial bus DATA IN; 0 = Low; 1 = High.
-- | Address of CIA2 Data Port A.
cia2DataPortA :: AddressRef
cia2DataPortA = AddrLit16 0xdd00


-- Port B, RS232 access. Read bits:
-- Bit #0: RS232 RXD line, input bit.
-- Bit #3: RS232 RI line.
-- Bit #4: RS232 DCD line.
-- Bit #5: User port H pin.
-- Bit #6: RS232 CTS line; 1 = Sender is ready to send.
-- Bit #7: RS232 DSR line; 1 = Receiver is ready to receive.

-- Write bits:
-- Bit #1: RS232 RTS line. 1 = Sender is ready to send.
-- Bit #2: RS232 DTR line. 1 = Receiver is ready to receive.
-- Bit #3: RS232 RI line.
-- Bit #4: RS232 DCD line.
-- Bit #5: User port H pin.
-- | Address of CIA2 Data Port B.
cia2DataPortB :: AddressRef
cia2DataPortB = AddrLit16 0xdd01

-- | Address of CIA2 Timer A Low byte.
cia2TimerALow :: AddressRef
cia2TimerALow = AddrLit16 0xdd04
-- | Address of CIA2 Timer A High byte.
cia2TimerAHigh :: AddressRef
cia2TimerAHigh = AddrLit16 0xdd05


-- KERNAL Routines
-- | Address of the KERNAL routine to clear the screen.
kernalClrscr :: AddressRef
kernalClrscr = AddrLit16 0xE544
-- | Address of the KERNAL routine to read a character from the keyboard (blocking).
kernalGetin :: AddressRef
kernalGetin = AddrLit16 0xFFE4 -- Reads a character from the keyboard (blocking)

-- PETSCII codes for cursor keys
-- | PETSCII code for the Cursor Down key.
keyCursorDown :: Word8;  keyCursorDown  = 17
-- | PETSCII code for the Cursor Right key.
keyCursorRight :: Word8;  keyCursorRight = 29
-- | PETSCII code for the Cursor Up key.
keyCursorUp    :: Word8; keyCursorUp    = 145
-- | PETSCII code for the Cursor Left key.
keyCursorLeft  :: Word8; keyCursorLeft  = 157



-- The colors of the C64
-- | Black color.
_BLACK :: Word8; _BLACK = 0x00
-- | White color.
_WHITE :: Word8; _WHITE = 0x01
-- | Red color.
_RED :: Word8; _RED = 0x02
-- | Cyan color.
_CYAN :: Word8; _CYAN = 0x03
-- | Purple color.
_PURPLE :: Word8; _PURPLE = 0x04
-- | Green color.
_GREEN :: Word8; _GREEN = 0x05
-- | Blue color.
_BLUE :: Word8; _BLUE = 0x06
-- | Yellow color.
_YELLOW :: Word8; _YELLOW = 0x07
-- | Orange color.
_ORANGE :: Word8; _ORANGE = 0x08
-- | Brown color.
_BROWN :: Word8; _BROWN = 0x09
-- | Pink color.
_PINK :: Word8; _PINK = 0x0a
-- | Light Red color (same as Pink).
_LIGHT_RED :: Word8; _LIGHT_RED = 0x0a
-- | Dark Grey color.
_DARK_GREY :: Word8; _DARK_GREY= 0x0b
-- | Grey color.
_GREY :: Word8; _GREY = 0x0c
-- | Light Green color.
_LIGHT_GREEN :: Word8; _LIGHT_GREEN = 0x0d
-- | Light Blue color.
_LIGHT_BLUE :: Word8; _LIGHT_BLUE = 0x0e
-- | Light Grey color.
_LIGHT_GREY :: Word8; _LIGHT_GREY = 0x0f



-- --- Codes for $CB / $C5 ---
_KEY_INST_DEL     =   0 :: Word8        -- (Ins/Del)
_KEY_RETURN       =   1 :: Word8        --; (Enter)
_KEY_CURSOR_RIGHT =   2 :: Word8        --; ⇔ (Left/Right)
_KEY_F7           =   3 :: Word8
_KEY_F1           =   4 :: Word8
_KEY_F3           =   5 :: Word8
_KEY_F5           =   6 :: Word8
_KEY_CURSOR_DOWN  =   7 :: Word8        --; ⇑ (Up/Down)
_KEY_3            =   8 :: Word8
_KEY_W            =   9 :: Word8
_KEY_A            =  10 :: Word8
_KEY_4            =  11 :: Word8
_KEY_Z            =  12 :: Word8
_KEY_S            =  13 :: Word8
_KEY_E            =  14 :: Word8
_KEY_L_SHIFT      =  15 :: Word8    --; (left Shift, CapsLock)
_KEY_5            =  16 :: Word8
_KEY_R            =  17 :: Word8
_KEY_D            =  18 :: Word8
_KEY_6            =  19 :: Word8
_KEY_C            =  20 :: Word8
_KEY_F            =  21 :: Word8
_KEY_T            =  22 :: Word8
_KEY_X            =  23 :: Word8
_KEY_7            =  24 :: Word8
_KEY_Y            =  25 :: Word8
_KEY_G            =  26 :: Word8
_KEY_8            =  27 :: Word8
_KEY_B            =  28 :: Word8
_KEY_H            =  29 :: Word8
_KEY_U            =  30 :: Word8
_KEY_V            =  31 :: Word8
_KEY_9            =  32 :: Word8
_KEY_I            =  33 :: Word8
_KEY_J            =  34 :: Word8
_KEY_0            =  35 :: Word8
_KEY_M            =  36 :: Word8
_KEY_K            =  37 :: Word8
_KEY_O            =  38 :: Word8
_KEY_N            =  39 :: Word8
_KEY_PLUS         =  40 :: Word8     --; + (Shift + "=" )
_KEY_P            =  41 :: Word8
_KEY_L            =  42 :: Word8
_KEY_MINUS        =  43 :: Word8     --; -
_KEY_PERIOD       =  44 :: Word8     --; .
_KEY_COLON        =  45 :: Word8     --; : (Shift + ";")
_KEY_AT           =  46 :: Word8     --; @ (Shift + "2")
_KEY_COMMA        =  47 :: Word8     --; ,
_KEY_POUND        =  48 :: Word8     --; £ (Backslash \)
_KEY_ASTERISK     =  49 :: Word8     --; * (Shift + "8" )
_KEY_SEMICOLON    =  50 :: Word8     --; ;
_KEY_CLR_HOME     =  51 :: Word8     -- (Home)
_KEY_R_SHIFT      =  52 :: Word8     --; (right Shift)
_KEY_EQUALS       =  53 :: Word8     --; =
_KEY_UP_ARROW     =  54 :: Word8     --; ↑ (PgDn)
_KEY_QUESTION     =  55 :: Word8     --; ?
_KEY_1            =  56 :: Word8
_KEY_ARROW_LEFT   =  57 :: Word8     --; ← (End)
_KEY_2            =  59 :: Word8
_KEY_SPACE        =  60 :: Word8
_KEY_Q            =  62 :: Word8
_KEY_RUNSTOP      =  63 :: Word8
-- _KEY_SHIFTLOCK    =   8 :: Word8
-- _KEY_CTRL         =   2 :: Word8

-- --- Special definitions (from table, for readability)
_KEY_NONE         = 64 :: Word8              --; no key





-- CIA#1 Register Addresses
cia1PortA = AddrLit16 0xDC00 -- Port A (Keyboard columns - write)
cia1PortB = AddrLit16 0xDC01 -- Port B (Keyboard rows - read)
cia1DDRA  = AddrLit16 0xDC02 -- Direction register for Port A
cia1DDRB  = AddrLit16 0xDC03 -- Direction register for Port B


-- | Scans the C64 keyboard matrix.
scanKeyboard :: Asm()
scanKeyboard = do

    let dataPortB = AddrLit8 0xf7  -- ZPAddr makes operand directly from Word8
    let scanKeycode = AddrLit8 0xf8

    --; set columns (Port A) as output and rows (Port B) as input
    lda# 0xff
    sta cia1DDRA
    lda# 0x00
    sta cia1DDRB

    lda# 0x40
    sta scanKeycode -- default keycode

    ldy# 0                         --; Y = 0 (column index)
    clc
    while_ IsNonCarry $ do              --; Y < 8
        lda $ Y "colMaskData"   --; colMaskData[Y] -> A
        sta cia1DataPortA       --; cia1DataPortA = mask 11111110, for column 0
        nop                             --; wait for signals to settle
        nop
        lda cia1DataPortB       --; cia1DataPortB -> A (keys in column Y)
        sta dataPortB                   --; cia1DataPortB -> dataPortB (for testing each bit/row in loop)
        ldx# 0x00                  --; X = 0  (row index)
        clc
        while_ IsNonCarry $  do         --; X < 8

            lda dataPortB
            and# 0x01 -- Use 'and' directly from EDSLInstr
            if_ IsZero $ do             --; found keycode = Y*8 + X
                tya
                asl A_
                asl A_
                asl A_             --; column * 8
                stx scanKeycode
                clc
                adc scanKeycode         --; column * 8 + row
                sta scanKeycode
                rts                     --; break from while_ by setting Y=8 i X=8 or return

            lda dataPortB
            lsr A_
            sta dataPortB    -- ;  rotating right dataPortB, testing bits 0-1-2-3-4-5-6-6-7 of dataPortB
            inx          -- ; next row index
            cpx# 0x08

        iny
        cpy# 0x08
    rts
    l_ "colMaskData"
    db [0b11111110,
        0b11111101,
        0b11111011,
        0b11110111,
        0b11101111,
        0b11011111,
        0b10111111,
        0b01111111]



    -- --; MODIFIERS = 0 at start
    -- LDA #0
    -- STA SCAN_MODIFIERS

    -- --; --- SHIFT LEFT: column 1 ($BF), row 6 (bit 6) ---
    -- LDA #$BF
    -- STA DC00
    -- LDA DC01
    -- AND #$40
    -- if_ IsZero $ do
    --     LDA SCAN_MODIFIERS
    --     ORA #1     ; bit 0 = SHIFT
    --     STA SCAN_MODIFIERS

    -- --; --- SHIFT RIGHT: column 2 ($DF), row 0 (bit 7) ---
    -- LDA #$DF
    -- STA DC00
    -- LDA DC01
    -- AND #$80
    -- if_ IsZero $ do
    --     LDA SCAN_MODIFIERS
    --     ORA #1
    --     STA SCAN_MODIFIERS


    -- --; --- COMMODORE: column 7 ($FE), row 1 (bit 6) ---
    -- LDA #$FE
    -- STA DC00
    -- LDA DC01
    -- AND #$40
    -- if_ IsZero $ do
    --     LDA SCAN_MODIFIERS
    --     ORA #2
    --     STA SCAN_MODIFIERS

    -- ; --- CTRL: column 5 ($F7), row 0 (bit 7) ---
    -- LDA #$F7
    -- STA DC00
    -- LDA DC01
    -- AND #$80
    -- if_ IsZero  $ do
    --     LDA SCAN_MODIFIERS
    --     ORA #4
    --     STA SCAN_MODIFIERS

    -- rts

-- Data needed for the procedure
-- | Defines the data needed for the keyboard scanning procedure.
defineKeyboardData :: Asm ()
defineKeyboardData = do
    -- 8-byte array for keyboard matrix state (initialized with zeros or arbitrarily)
    l_ "keyMatrixStateData"
    replicateM_ 8 $ db [0x00 :: Word8] -- 8 bytes for keyMatrixState

    -- Array of values for selecting keyboard rows (bit 0 for row 0, etc.)
    -- Value $FE (%11111110) selects row 0
    -- Value $FD (%11111101) selects row 1
    -- ...
    -- Value $7F (%01111111) selects row 7
    l_ "rowSelectValuesData"
    db [0x7F, 0xBF, 0xDF, 0xEF, 0xF7, 0xFB, 0xFD, 0xFE]

-- Somewhere in the main code, data needs to be defined:
-- mainAssembly :: Asm ()
-- mainAssembly = do
--    org 0xC000
--    jmp (AbsLabel "start")
--    scanKeys -- Procedure definition
--    defineKeyboardData -- Data definition
--    l_ "start"
--    -- ... rest of the program ...
--    jsr "scanKeys" -- Scanning call
    -- ... checking the state in keyMatrixState ...



-- | Address of the sprite pointers in memory.
_SPRITE_POINTERS                     = 0x07f8 -- 2040

--; ----------------------------------------------------------
--; VIC-II Video Display
--; $D000-$D3FF, 53248-54271
--; ----------------------------------------------------------

--; Sprite horizontal and vertical position registers
-- | Address of Sprite 0 X position register.
vicSprite0X :: AddressRef; vicSprite0X = AddrLit16 0xd000 -- 53248
-- | Address of Sprite 0 Y position register.
vicSprite0Y :: AddressRef; vicSprite0Y = AddrLit16 0xd001 -- 53249
-- | Address of Sprite 1 X position register.
vicSprite1X :: AddressRef; vicSprite1X = AddrLit16 0xd002 -- 53250
-- | Address of Sprite 1 Y position register.
vicSprite1Y :: AddressRef; vicSprite1Y = AddrLit16 0xd003 -- 53251
-- | Address of Sprite 2 X position register.
vicSprite2X :: AddressRef; vicSprite2X = AddrLit16 0xd004 -- 53252
-- | Address of Sprite 2 Y position register.
vicSprite2Y :: AddressRef; vicSprite2Y = AddrLit16 0xd005 -- 53253
-- | Address of Sprite 3 X position register.
vicSprite3X :: AddressRef; vicSprite3X = AddrLit16 0xd006 -- 53254
-- | Address of Sprite 3 Y position register.
vicSprite3Y :: AddressRef; vicSprite3Y = AddrLit16 0xd007 -- 53255
-- | Address of Sprite 4 X position register.
vicSprite4X :: AddressRef; vicSprite4X = AddrLit16 0xd008 -- 53256
-- | Address of Sprite 4 Y position register.
vicSprite4Y :: AddressRef; vicSprite4Y = AddrLit16 0xd009 -- 53257
-- | Address of Sprite 5 X position register.
vicSprite5X :: AddressRef; vicSprite5X = AddrLit16 0xd00a -- 53258
-- | Address of Sprite 5 Y position register.
vicSprite5Y :: AddressRef; vicSprite5Y = AddrLit16 0xd00b -- 53259
-- | Address of Sprite 6 X position register.
vicSprite6X :: AddressRef; vicSprite6X = AddrLit16 0xd00c -- 53260
-- | Address of Sprite 6 Y position register.
vicSprite6Y :: AddressRef; vicSprite6Y = AddrLit16 0xd00d -- 53261
-- | Address of Sprite 7 X position register.
vicSprite7X :: AddressRef; vicSprite7X = AddrLit16 0xd00e -- 53262
-- | Address of Sprite 7 Y position register.
vicSprite7Y :: AddressRef; vicSprite7Y = AddrLit16 0xd00f -- 53263

--; Most significant bit of sprites 0-7 horizontal positions, bit #x: Sprite #x (X-coord. bit #8 of spirte #x)
-- | Address of the Most Significant Bit (MSB) of sprite X positions.
vicSpriteMSBX :: AddressRef; vicSpriteMSBX =  AddrLit16 0xd010 -- 53264


-- 0xD011 Control Register #1
-- Bits #0-#2: Vertical raster scroll.
-- Bit#3: Screen height; 0 = 24 rows; 1 = 25 rows.
--              |  Display window height   | First line  | Last line
--          ----+--------------------------+-------------+----------
--            0 | 24 text lines/192 pixels |   55 (0x37)  | 246 (0xf6)
--            1 | 25 text lines/200 pixels |   51 (0x33)  | 250 (0xfa)
-- Bit #4: 0 = Screen off, complete screen is covered by border; 1 = Screen on, normal screen contents are visible.
-- Bit #5: 0 = Text mode; 1 = Bitmap mode.
-- Bit #6: 1 = Extended background mode on.
-- Bit #7:  Read: Current raster line (bit #8).
--         Write: Raster line to generate interrupt at (bit #8).
-- | Address of VIC Control Register #1.
vicControl1 :: AddressRef
vicControl1 = AddrLit16 0xd011

-- Read: Current raster line (bits #0-#7).
-- Write: Raster line to generate interrupt at (bits #0-#7).
-- | Address of the VIC Raster register.
vicRaster :: AddressRef
vicRaster = AddrLit16 0xd012
--; Sprite enable register
-- | Address of the Sprite Enable register.
vicSpriteEnable :: AddressRef; vicSpriteEnable = AddrLit16 0xd015 -- 53269


-- Bits #0-#2: Horizontal raster scroll.
-- Bit #3: Screen width; 0 = 38 columns; 1 = 40 columns.
--               |   Display window width   | First X coo. | Last X coo.
--           ----+--------------------------+--------------+------------
--             0 | 38 characters/304 pixels |   31 (0x1f)   |  334 (0x14e)
--             1 | 40 characters/320 pixels |   24 (0x18)   |  343 (0x157)
-- Bit #4: 1 = Multicolor mode on.
-- | Address of VIC Control Register #2.
vicControl2 :: AddressRef
vicControl2 = AddrLit16 0xd016


-- 0xD018 VIC-II base addresses
-- - Bit#0: not used
-- Bits #1-#3: In text mode, pointer to character memory (bits #11-#13), relative to VIC bank, memory address $DD00. Values:
-- CB Address Bits 11-13 of the Character Set (*2048)
-- %000, 0: $0000-$07FF, 0-2047.
-- %001, 1: $0800-$0FFF, 2048-4095.
-- %010, 2: $1000-$17FF, 4096-6143.
-- %011, 3: $1800-$1FFF, 6144-8191.
-- %100, 4: $2000-$27FF, 8192-10239.
-- %101, 5: $2800-$2FFF, 10240-12287.
-- %110, 6: $3000-$37FF, 12288-14335.
-- %111, 7: $3800-$3FFF, 14336-16383.

-- Values %010 and %011 in VIC bank #0 and #2 select Character ROM instead.
-- In bitmap mode, pointer to bitmap memory (bit #13), relative to VIC bank, memory address $DD00. Values:
-- %0xx, 0: $0000-$1FFF, 0-8191.
-- %1xx, 4: $2000-$3FFF, 8192-16383.

-- Bits #4-#7: Pointer to screen memory (bits #10-#13), relative to VIC bank, memory address $DD00. Values:
-- VM Address Bits 10-13 of the Screen RAM (*1024)
-- %0000, 0: $0000-$03FF, 0-1023.
-- %0001, 1: $0400-$07FF, 1024-2047.
-- %0010, 2: $0800-$0BFF, 2048-3071.
-- %0011, 3: $0C00-$0FFF, 3072-4095.
-- %0100, 4: $1000-$13FF, 4096-5119.
-- %0101, 5: $1400-$17FF, 5120-6143.
-- %0110, 6: $1800-$1BFF, 6144-7167.
-- %0111, 7: $1C00-$1FFF, 7168-8191.
-- %1000, 8: $2000-$23FF, 8192-9215.
-- %1001, 9: $2400-$27FF, 9216-10239.
-- %1010, 10: $2800-$2BFF, 10240-11263.
-- %1011, 11: $2C00-$2FFF, 11264-12287.
-- %1100, 12: $3000-$33FF, 13312-14335.
-- %1101, 13: $3400-$37FF, 13312-14335.
-- %1110, 14: $3800-$3BFF, 14336-15359.
-- %1111, 15: $3C00-$3FFF, 15360-16383.
-- | Address of VIC Memory Control Register.
vicMemoryControl :: AddressRef
vicMemoryControl = AddrLit16 0xd018 -- Bity 7-4: Screen Base, Bity 3-1: Charset Base


-- Bit #0: 1 = Current raster line is equal to the raster line to generate interrupt at.
-- Bit #1: 1 = Sprite-background collision occurred.
-- Bit #2: 1 = Sprite-sprite collision occurred.
-- Bit #3: 1 = Light pen signal arrived.
-- Bit #7: 1 = An event (or more events), that may generate an interrupt, occurred and it has not been (not all of them have been) acknowledged yet.
-- Write bits:
-- Bit #0: 1 = Acknowledge raster interrupt.
-- Bit #1: 1 = Acknowledge sprite-background collision interrupt.
-- Bit #2: 1 = Acknowledge sprite-sprite collision interrupt.
-- Bit #3: 1 = Acknowledge light pen interrupt.
-- | Address of VIC Interrupt Status Register.
vicInterruptStatus :: AddressRef
vicInterruptStatus = AddrLit16 0xd019 -- Bit 7: 1 = IRQ occurred; 0 = No IRQ occurred.


-- Bit #0: 1 = Raster interrupt enabled.
-- Bit #1: 1 = Sprite-background collision interrupt enabled.
-- Bit #2: 1 = Sprite-sprite collision interrupt enabled.
-- Bit #3: 1 = Light pen interrupt enabled.
-- | Address of VIC Interrupt Enable Register.
vicInterruptEnable :: AddressRef
vicInterruptEnable = AddrLit16 0xd01a -- Bit 7: 1 = Enable raster IRQ; 0 = Disable raster IRQ.

-- | Address of the Sprite Priority register.
vicSpritePriority :: AddressRef
vicSpritePriority = AddrLit16 0xd01b -- Bit #x: 0 = Sprite #x is drawn in front of screen contents; 1 = Sprite #x is behind screen contents.
-- | Address of the Sprite Multicolor register.
vicSpriteMulticolor :: AddressRef
vicSpriteMulticolor = AddrLit16 0xd01c -- Bit #x: 1 = Sprite #x is multicolored.
-- | Address of the Sprite Double Width register.
vicSpriteDoubleWidth :: AddressRef
vicSpriteDoubleWidth = AddrLit16 0xd01d  --Bit #x: 1 = Sprite #x is stretched to double width.
-- | Address of the Sprite Double Height register.
vicSpriteDoubleHeight :: AddressRef
vicSpriteDoubleHeight = AddrLit16 0xd017 -- Bit #x: 1 = Sprite #x is stretched to double height.

-- | Address of the Sprite-Sprite Collision register.
vicSpriteSpriteColision :: AddressRef
vicSpriteSpriteColision = AddrLit16 0xd01e
-- | Address of the Sprite-Background Collision register.
vicSpriteBackColision :: AddressRef
vicSpriteBackColision = AddrLit16 0xd01f

-- | Address of the Border Color register.
vicBorderColor :: AddressRef
vicBorderColor = AddrLit16 0xd020
-- | Address of the Background Color register.
vicBackgroundColor :: AddressRef
vicBackgroundColor = AddrLit16 0xd021
-- | Address of Background Color 1 register.
vicBackgroundColor1 :: AddressRef
vicBackgroundColor1 = AddrLit16 0xd022
-- | Address of Background Color 2 register.
vicBackgroundColor2 :: AddressRef
vicBackgroundColor2 = AddrLit16 0xd023
-- New comment added
-- | Address of Background Color 3 register.
vicBackgroundColor3 :: AddressRef
vicBackgroundColor3 = AddrLit16 0xd024


--; Sprite Multicolor
-- | Address of Sprite Multicolor Register 0.
vicSpriteMc0 :: AddressRef; vicSpriteMc0 =  AddrLit16 0xd025 -- 53285
-- | Address of Sprite Multicolor Register 1.
vicSpriteMc1 :: AddressRef; vicSpriteMc1 =  AddrLit16 0xd026 -- 53286

--; Sprite color registers
-- | Address of Sprite 0 Color register.
vicSprite0Color :: AddressRef; vicSprite0Color = AddrLit16 0xd027 -- 53287
-- | Address of Sprite 1 Color register.
vicSprite1Color :: AddressRef; vicSprite1Color = AddrLit16 0xd028 -- 53288
-- | Address of Sprite 2 Color register.
vicSprite2Color :: AddressRef; vicSprite2Color = AddrLit16 0xd029 -- 53289
-- | Address of Sprite 3 Color register.
vicSprite3Color :: AddressRef; vicSprite3Color = AddrLit16 0xd02a -- 53290
-- | Address of Sprite 4 Color register.
vicSprite4Color :: AddressRef; vicSprite4Color = AddrLit16 0xd02b -- 53291
-- | Address of Sprite 5 Color register.
vicSprite5Color :: AddressRef; vicSprite5Color = AddrLit16 0xd02c -- 53292
-- | Address of Sprite 6 Color register.
vicSprite6Color :: AddressRef; vicSprite6Color = AddrLit16 0xd02d -- 53293
-- | Address of Sprite 7 Color register.
vicSprite7Color :: AddressRef; vicSprite7Color = AddrLit16 0xd02e -- 53294


-- Decimal         Hex           Uses
-- 0     - 827     0000 - 033B   Operating system
-- 828   - 1019    033C - 03FB   Cassette buffer
-- 1024  - 2023    0400 - 07FF   Screen memory
-- 2040  - 2047    07F8 - 07FF   Sprite data pointers
-- 2048  - 40959   0800 - 9FFF   BASIC area
-- 40960 - 49151   A000 - BFFF   BASIC ROM / 8k RAM
-- 49152 - 53247   C000 - CFFF   4k RAM
-- 53248 - 54271   D000 - D3FF   VIC chip (sprites, video display)
-- 54272 - 55295   D400 – D7FF   SID chip (sound)
-- 55296 - 56319   D800 – DBFF   Colour memory
-- 56320 - 57343   DC00 – DEFF   Input/output etc.
-- 57344 - 65535   E000 - FFFF   KERNAL ROM / 8k RAM
