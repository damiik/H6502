{-# LANGUAGE OverloadedStrings #-}
module C64.HorizontalBars (horizontalBars) where

import Assembly.Core hiding (and) -- Hide Core's and if Prelude's is needed elsewhere, or qualify Core's usage
import qualified Assembly.Core as C (and) -- Import qualified if needed
import Assembly(Asm)
import Data.Word (Word8, Word16) -- Added Word16
import Prelude hiding (and) -- Hide Prelude's and

-- Constants
screenRam :: Word16
screenRam = 0x0400

colorRam :: Word16
colorRam = 0xd800

rasterLine :: Word16
rasterLine = 0xd012

borderColor :: Word16
borderColor = 0xd020

backgroundColor :: Word16
backgroundColor = 0xd021

-- Zero Page Variables (using fixed addresses from assembly)
scrollOffset :: Operand
scrollOffset = OpAbs $ AddrLit16 0x02

delayCounter :: Operand
delayCounter = OpAbs $ AddrLit16 0x03
basicLoader :: Int -> Asm()
basicLoader addr  = -- BASIC loader, don't use with --c64 option (direct BASIC output)
    let progAddress :: [Word8]; progAddress = map asc $ " " ++ show addr
    in db $ [
    0x00,                  -- Padding, BASIC loader starts at 0x0801
    0x0c, 0x08,            -- Next line $080c
    0x10, 0x00,            -- BASIC line number
    0x9e                   -- SYS token
    ]
    ++ progAddress ++      -- Address in PETSCII sequence
    [
    0x00,                  -- Terminator 0
    0x00, 0x00             -- End of BASIC program
    ]


-- The main assembly program
horizontalBars :: Asm ()
horizontalBars = do
    -- org 0x0800 -- Ustawienie adresu początkowego dla BASIC loadera
    -- basicLoader 0x1000 -- program starts at $1000   
    -- db $ replicate 0x7f2 (0x00 :: Word8) -- Padding to fill the rest of the BASIC loader

    org 0x1000 -- Ustawienie adresu początkowego dla programu
    l_ "start"
    sei                -- Disable interrupts

    lda $ Imm 0x00     -- Black border color
    sta $ OpAbs $ AddrLit16 borderColor
    lda $ Imm 0x00     -- Black background color
    sta $ OpAbs $ AddrLit16 backgroundColor

    -- Initialize zero page variables (optional, depending on environment)
    lda $ Imm 0x00
    sta scrollOffset
    lda $ Imm 3 -- Initial delay
    sta delayCounter
    jsr $ AbsLabel "fill_screen" -- Fill screen with initial bars

    l_ "main_loop"
    jsr $ AbsLabel "wait_raster" -- Sync with raster

    dec delayCounter   -- Animation delay
    bne "main_loop"    -- Loop if delay counter not zero

    lda $ Imm 3        -- Reset delay counter
    sta delayCounter

    jsr $ AbsLabel "scroll_colors" -- Scroll bar colors
    jmp $ AbsLabel "main_loop"

    -- Subroutine: Fill Screen with Bars
    l_ "fill_screen"
    ldx $ Imm 0        -- Fill entire color RAM (implicitly 1000 bytes, but loop handles 256)
    l_ "fill_loop"
    txa                -- Use index as color base
    C.and $ Imm 0x0f     -- Limit to 16 colors (0-15) -- Use qualified C.and
    sta $ OpAbsX $ AddrLit16 colorRam      -- First quarter (0-249)
    sta $ OpAbsX $ AddrLit16 (colorRam + 250) -- Second quarter (250-499)
    sta $ OpAbsX $ AddrLit16 (colorRam + 500) -- Third quarter (500-749)
    sta $ OpAbsX $ AddrLit16 (colorRam + 750) -- Fourth quarter (750-999)

    lda $ immChar '*'   -- Space character code
    sta $ OpAbsX $ AddrLit16 screenRam
    sta $ OpAbsX $ AddrLit16 (screenRam + 250)
    sta $ OpAbsX $ AddrLit16 (screenRam + 500)
    sta $ OpAbsX $ AddrLit16 (screenRam + 750)

    inx
    bne "fill_loop"    -- Repeat for 256 bytes (X wraps around)
    rts

    -- Subroutine: Scroll Colors
    l_ "scroll_colors"
    inc scrollOffset   -- Increment scroll offset
    lda scrollOffset
    C.and $ Imm 0x0f     -- Limit to 16 positions (0-15) -- Use qualified C.and

    tax                -- Store current scroll offset in X
    ldy $ Imm 0        -- Index for color RAM and scroll_table
    l_ "scroll_loop"
    txa                -- Get base offset (from X)
    clc
    adc $ AbsYLabel "scroll_table" -- Add offset from table based on Y
    C.and $ Imm 0x0f     -- Limit to 16 colors -- Use qualified C.and
    sta $ OpAbsY $ AddrLit16 colorRam -- Write color to COLOR_RAM[y]

    iny
    bne "scroll_loop"  -- Process 256 bytes
    rts

    -- Subroutine: Wait for Raster Line
    l_ "wait_raster"
    lda $ Imm 100      -- Wait for line ~100
    l_ "wait"
    cmp $ OpAbs $ AddrLit16 rasterLine
    bne "wait"
    rts

    -- Data: Scroll Table
    l_ "scroll_table"
    db scrollTableData

    -- org 0x1ffe --vector reset
    -- db $ replicate 3727 (0x00 :: Word8) -- Reset vector to $0000
    -- db [0x00, 0x10] -- Reset vector to $0800
-- Helper for scroll table data
scrollTableData :: [Word8]
scrollTableData = concat $ replicate 2 $ concat
    [ replicate 8 0,  replicate 8 1
    , replicate 8 2,  replicate 8 3
    , replicate 8 4,  replicate 8 5
    , replicate 8 6,  replicate 8 7
    , replicate 8 8,  replicate 8 9
    , replicate 8 10, replicate 8 11
    , replicate 8 12, replicate 8 13
    , replicate 8 14, replicate 8 15
    ] 


