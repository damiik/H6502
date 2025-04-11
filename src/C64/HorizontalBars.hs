{-# LANGUAGE OverloadedStrings #-}
module C64.HorizontalBars (horizontalBars) where

import Assembly.Core hiding (and) -- Hide Core's and if Prelude's is needed elsewhere, or qualify Core's usage
import qualified Assembly.Core as C (and) -- Import qualified if needed
import Assembly(Asm)
import Assembly.Macros
import Data.Word (Word8, Word16) -- Added Word16
import Prelude hiding (and) -- Hide Prelude's and
import C64
import Control.Monad



basicLoader :: Int -> Asm()
basicLoader addr = -- BASIC loader, don't use with --c64 option (direct BASIC output)
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


-- Constants

-- Zero Page Variables (using fixed addresses from assembly)
scrollOffset = AddrLit16 0x02
lastColor = AddrLit16 0x03
delayCounter = AddrLit16 0x04


-- [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
-- ...,...,
-- ...,15..., 
-- 0...,1...
-- ..., ...,
-- ,...,15...]
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

scrollColors :: AddressRef -> Asm ()
scrollColors addr = do
    inc $ OpAbs scrollOffset   -- Increment scroll offset
    lda $ OpAbs scrollOffset
    C.and $ Imm 0x0f     -- Limit to 16 positions (0-15) -- Use qualified C.and
    tax                  -- Store current scroll offset in Y
    ldy $ Imm 0x00
    doWhile_ IsNonZero $ do
        lda $ AbsXLabel "colors_list" -- Get color from list, for x indexed by y
        sta $ OpAbsY addr -- Write color to COLOR_RAM[y]
        iny
        cmpY 80


-- The main assembly programB
horizontalBars :: Asm ()
horizontalBars = do
    org 0x0800 -- Ustawienie adresu początkowego dla BASIC loadera
    basicLoader 0x1000 -- program starts at $1000   
    db $ replicate 0x7f2 (0x00 :: Word8) -- Padding to fill the rest of the BASIC loader
    
    org 0x1000 -- Ustawienie adresu początkowego dla programu
    l_ "start"
    sei                -- Disable interrupts
    staRb borderColor _DARK_GREY
    staRb backgroundColor _DARK_GREY
    -- Initialize zero page variables
    staRb scrollOffset 0
    staRb lastColor 0
    jsr $ AbsLabel "fill_screen" -- Fill screen with initial bars

    l_ "main_loop"

    staRb delayCounter 80
    doWhile_ IsNonZero $ do
         jsr $ AbsLabel "delay" -- Delay loop
         dec $ OpAbs delayCounter 

    waitRaster -- Sync with raster
    scrollColors colorRam
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0028)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0050)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0078)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x00a0)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x00c8)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x00f0)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0118)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0140)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0168)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0190)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x01b8)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x01e0)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0208)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0230)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0258)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0280)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x02a8)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x02d0)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x02f8)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0320)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0348)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0370)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x0398)
    scrollColors $ AddrLit16 ( addr2word16 colorRam + 0x03c0)

    inc $ OpAbs lastColor
    lda $ OpAbs lastColor
    C.and $ Imm 0x0f     -- Limit to 16 positions (0-15) -- Use qualified C.and
    sta $ OpAbs lastColor
    sta $ OpAbs scrollOffset 
    jmp $ AbsLabel "main_loop"


    -- Subroutine: Fill Screen with Bars
    l_ "fill_screen"
    ldx $ Imm 0        -- Fill entire color RAM (implicitly 1000 bytes, but loop handles 256)
    doWhileNz (inx) $ do
        txa                -- Use index as color base
        replicateM_ 6 $ ror Nothing -- Rotate right 6 times
        C.and $ Imm 0x0f     -- Limit to 16 colors (0-15) -- Use qualified C.and
        sta $ OpAbsX $ AddrLit16 (addr2word16 colorRam)      -- First quarter (0-249)
        sta $ OpAbsX $ AddrLit16 (addr2word16 colorRam + 250) -- Second quarter (250-499)
        sta $ OpAbsX $ AddrLit16 (addr2word16 colorRam + 500) -- Third quarter (500-749)
        sta $ OpAbsX $ AddrLit16 (addr2word16 colorRam + 750) -- Fourth quarter (750-999)

        lda $ Imm 0xA0 --immChar '='   -- Space character code
        sta $ OpAbsX $ AddrLit16 (addr2word16 screenRam)
        sta $ OpAbsX $ AddrLit16 (addr2word16 screenRam + 250)
        sta $ OpAbsX $ AddrLit16 (addr2word16 screenRam + 500)
        sta $ OpAbsX $ AddrLit16 (addr2word16 screenRam + 750)
    rts

    l_ "delay"
    replicateM_ 400 $ C.and $ Imm 0xef
    rts
    l_ "colors_list"
    db [_LIGHT_BLUE, _BLUE, _CYAN, _GREEN,
        _LIGHT_GREEN, _YELLOW, _ORANGE, _PINK, 
        _PURPLE, _PINK, _ORANGE, _YELLOW, 
        _LIGHT_GREEN, _GREEN, _CYAN, _BLUE ] -- Color data

    replicateM_ 20 $ db [0x00 :: Word8] -- Padding to fill the rest of the program
    -- Data: Scroll Table
    l_ "scroll_table"
    db scrollTableData
