{-# LANGUAGE BinaryLiterals    #-}
module C64 (
    screenRam, colorRam, 
    vicBankSelect, 
    vicMemoryControl,
    vicBorderColor, vicBackgroundColor,
    vicRaster, cia1DataPortA,
    kernalClrscr, kernalGetin,
    keyCursorDown, keyCursorRight,
    keyCursorUp, keyCursorLeft,
    control1, control2, memory,
    color0, color1, color2, color3,
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
    _SPRITE_0_COLOR_REGISTER,
    _SPRITE_1_COLOR_REGISTER,
    _SPRITE_2_COLOR_REGISTER,
    _SPRITE_3_COLOR_REGISTER,
    _SPRITE_4_COLOR_REGISTER,
    _SPRITE_5_COLOR_REGISTER,
    _SPRITE_6_COLOR_REGISTER,
    _SPRITE_7_COLOR_REGISTER,
    _SPRITE_MULTICOLOR_REGISTERS,
    _SPRITE_ENABLE_REGISTER,
    _SPRITE_MC0,
    _SPRITE_MC1,
    _SP0X,
    _SP0Y,
    _SP1X,
    _SP1Y,
    _SP2X,
    _SP2Y,
    _SP3X,
    _SP3Y,
    _SP4X,
    _SP4Y,
    _SP5X,
    _SP5Y,
    _SP6X,
    _SP6Y,
    _SP7X,
    _SP7Y,




) where
import Prelude hiding (and) -- Hide 'and' from Prelude
import Control.Monad (replicateM_)
import Data.Word
import Assembly.Core
import Assembly.EDSLInstr 



-- C64 Constants
color0 = AddrLit16 0xd020
color1 = AddrLit16 0xd021
color2 = AddrLit16 0xd022
color3 = AddrLit16 0xd023


screenRam :: AddressRef
screenRam = AddrLit16 0x0400

colorRam :: AddressRef
colorRam = AddrLit16 0xd800



-- 0xD011 Control Register #1
-- - Bit#0-#2: YSCROLL Screen Soft Scroll Vertical
-- - Bit#3: RSEL Switch betweem 25 or 24 visible rows
--          RSEL|  Display window height   | First line  | Last line
--          ----+--------------------------+-------------+----------
--            0 | 24 text lines/192 pixels |   55 (0x37)  | 246 (0xf6)
--            1 | 25 text lines/200 pixels |   51 (0x33)  | 250 (0xfa)
-- - Bit#4: DEN Switch VIC-II output on/off
-- - Bit#5: BMM Turn Bitmap Mode on/off
-- - Bit#6: ECM Turn Extended Color Mode on/off
-- - Bit#7: RST8 9th Bit for 0xD012 Rasterline counter
-- Initial Value: %10011011
control1 = AddrLit16 0xd011
-- 0xD012 RASTER Raster counter

-- 0xD016 Control register 2
-- -  Bit#0-#2: XSCROLL Screen Soft Scroll Horizontal
-- -  Bit#3: CSEL Switch betweem 40 or 38 visible columns
--           CSEL|   Display window width   | First X coo. | Last X coo.
--           ----+--------------------------+--------------+------------
--             0 | 38 characters/304 pixels |   31 (0x1f)   |  334 (0x14e)
--             1 | 40 characters/320 pixels |   24 (0x18)   |  343 (0x157)
-- -  Bit#4: MCM Turn Multicolor Mode on/off
-- -  Bit#5-#7: not used
-- Initial Value: %00001000
control2 = AddrLit16 0xd016 


-- 0xD018 VIC-II base addresses
-- - Bit#0: not used
-- - Bit#1-#3: CB Address Bits 11-13 of the Character Set (*2048)
-- - Bit#4-#7: VM Address Bits 10-13 of the Screen RAM (*1024)
-- Initial Value: %00010100
memory = AddrLit16 0xd018



vicBankSelect      = AddrLit16 0xDD00
vicMemoryControl   = AddrLit16 0xD018 -- Bity 7-4: Screen Base, Bity 3-1: Charset Base
vicBorderColor     = AddrLit16 0xD020
vicBackgroundColor = AddrLit16 0xD021
vicRaster          = AddrLit16 0xD012
cia1DataPortA      = AddrLit16 0xDC00 -- Joystick Port 2 (nieużywane)
cia1DataPortB      = AddrLit16 0xDC01 -- Joystick Port 2 (nieużywane)

-- Rutyny KERNAL
kernalClrscr       = AddrLit16 0xE544
kernalGetin        = AddrLit16 0xFFE4 -- Odczytuje znak z klawiatury (blokujące)

-- Kody PETSCII dla klawiszy kursorów
keyCursorDown :: Word8;  keyCursorDown  = 17
keyCursorRight :: Word8;  keyCursorRight = 29
keyCursorUp    :: Word8; keyCursorUp    = 145
keyCursorLeft  :: Word8; keyCursorLeft  = 157



-- The colors of the C64
_BLACK = 0x00 :: Word8
_WHITE = 0x01 :: Word8
_RED = 0x02 :: Word8
_CYAN = 0x03 :: Word8
_PURPLE = 0x04 :: Word8
_GREEN = 0x05 :: Word8
_BLUE = 0x06 :: Word8
_YELLOW = 0x07 :: Word8
_ORANGE = 0x08 :: Word8
_BROWN = 0x09 :: Word8
_PINK = 0x0a :: Word8 -- _LT_RED
_DARK_GREY= 0x0b :: Word8
_GREY = 0x0c :: Word8
_LIGHT_GREEN = 0x0d :: Word8
_LIGHT_BLUE = 0x0e :: Word8
_LIGHT_GREY = 0x0f :: Word8



; --- Kody dla $CB / $C5 ---
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

--; --- Definicje specjalne (z tabeli, dla czytelności)
_KEY_NONE         = 64 :: Word8              --; brak klawisza





-- Adresy rejestrów CIA#1
cia1PortA = AddrLit16 0xDC00 -- Port A (Kolumny klawiatury - zapis)
cia1PortB = AddrLit16 0xDC01 -- Port B (Wiersze klawiatury - odczyt)
cia1DDRA  = AddrLit16 0xDC02 -- Rejestr kierunku dla Portu A
cia1DDRB  = AddrLit16 0xDC03 -- Rejestr kierunku dla Portu B


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
            sta dataPortB    -- ;  rotating right dataPortB, testing bits 0-1-2-3-4-5-6-7 of dataPortB
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



    -- --; MODIFIERS = 0 na start
    -- LDA #0
    -- STA SCAN_MODIFIERS

    -- --; --- SHIFT LEFT: kolumna 1 ($BF), wiersz 6 (bit 6) ---
    -- LDA #$BF
    -- STA DC00
    -- LDA DC01
    -- AND #$40
    -- if_ IsZero $ do
    --     LDA SCAN_MODIFIERS
    --     ORA #1     ; bit 0 = SHIFT
    --     STA SCAN_MODIFIERS

    -- --; --- SHIFT RIGHT: kolumna 2 ($DF), wiersz 0 (bit 7) ---
    -- LDA #$DF
    -- STA DC00
    -- LDA DC01
    -- AND #$80
    -- if_ IsZero $ do
    --     LDA SCAN_MODIFIERS
    --     ORA #1
    --     STA SCAN_MODIFIERS
    

    -- --; --- COMMODORE: kolumna 7 ($FE), wiersz 1 (bit 6) ---
    -- LDA #$FE
    -- STA DC00
    -- LDA DC01
    -- AND #$40
    -- if_ IsZero $ do
    --     LDA SCAN_MODIFIERS
    --     ORA #2
    --     STA SCAN_MODIFIERS

    -- ; --- CTRL: kolumna 5 ($F7), wiersz 0 (bit 7) ---
    -- LDA #$F7
    -- STA DC00
    -- LDA DC01
    -- AND #$80
    -- if_ IsZero  $ do
    --     LDA SCAN_MODIFIERS
    --     ORA #4
    --     STA SCAN_MODIFIERS

    -- rts

-- Dane potrzebne procedurze
defineKeyboardData :: Asm ()
defineKeyboardData = do
    -- Tablica 8 bajtów na stan macierzy klawiatury (inicjowana zerami lub dowolnie)
    l_ "keyMatrixStateData"
    replicateM_ 8 $ db [0x00 :: Word8] -- 8 bytes for keyMatrixState

    -- Tablica wartości do wyboru wierszy klawiatury (bit 0 dla wiersza 0 itd.)
    -- Wartość $FE (%11111110) wybiera wiersz 0
    -- Wartość $FD (%11111101) wybiera wiersz 1
    -- ...
    -- Wartość $7F (%01111111) wybiera wiersz 7
    l_ "rowSelectValuesData"
    db [0x7F, 0xBF, 0xDF, 0xEF, 0xF7, 0xFB, 0xFD, 0xFE]

-- Gdzieś w kodzie głównym trzeba zdefiniować dane:
-- mainAssembly :: Asm ()
-- mainAssembly = do
--    org 0xC000
--    jmp (AbsLabel "start")
--    scanKeys -- Definicja procedury
--    defineKeyboardData -- Definicja danych
--    l_ "start"
--    -- ... reszta programu ...
--    jsr "scanKeys" -- Wywołanie skanowania
    -- ... sprawdzenie stanu w keyMatrixState ...



_SPRITE_POINTERS                     = 0x07f8 -- 2040

--; ----------------------------------------------------------
--; VIC-II Video Display
--; $D000-$D3FF, 53248-54271
--; ----------------------------------------------------------

--; Sprite horizontal and vertical position registers
_SP0X                                = 0xd000 -- 53248
_SP0Y                                = 0xd001 -- 53249
_SP1X                                = 0xd002 -- 53250
_SP1Y                                = 0xd003 -- 53251
_SP2X                                = 0xd004 -- 53252
_SP2Y                                = 0xd005 -- 53253
_SP3X                                = 0xd006 -- 53254
_SP3Y                                = 0xd007 -- 53255
_SP4X                                = 0xd008 -- 53256
_SP4Y                                = 0xd009 -- 53257
_SP5X                                = 0xd00A -- 53258
_SP5Y                                = 0xd00B -- 53259
_SP6X                                = 0xd00C -- 53260
_SP6Y                                = 0xd00D -- 53261
_SP7X                                = 0xd00E -- 53262
_SP7Y                                = 0xd00F -- 53263

--; Most significant bits of sprites 0-7 horizontal positions
_MSIGX                               = 0xd010 -- 53264

--; Sprite enable register
_SPRITE_ENABLE_REGISTER              = 0xd015 -- 53269

--; Multicolor registers
_SPRITE_MULTICOLOR_REGISTERS         = 0xd01c -- 53276

--; Sprite Multicolor
_SPRITE_MC0                          = 0xd025 -- 53285
_SPRITE_MC1                          = 0xd026 -- 53286

--; Sprite color registers
_SPRITE_0_COLOR_REGISTER             = 0xd027 -- 53287
_SPRITE_1_COLOR_REGISTER             = 0xd028 -- 53288
_SPRITE_2_COLOR_REGISTER             = 0xd029 -- 53289
_SPRITE_3_COLOR_REGISTER             = 0xd02a -- 53290
_SPRITE_4_COLOR_REGISTER             = 0xd02b -- 53291
_SPRITE_5_COLOR_REGISTER             = 0xd02c -- 53292
_SPRITE_6_COLOR_REGISTER             = 0xd02d -- 53293
_SPRITE_7_COLOR_REGISTER             = 0xd02e -- 53294
