module C64 (
    screenRam, colorRam,
    rasterLine, borderColor, backgroundColor,
    control1, control2, memory,
    color0, color1, color2, color3,
    _BLACK, _WHITE, _RED, _CYAN, _PURPLE,
    _GREEN, _BLUE, _YELLOW, _ORANGE, _BROWN,
    _PINK, _DARK_GREY, _GREY, _LIGHT_GREEN,
    _LIGHT_BLUE, _LIGHT_GREY
) where

import Data.Word
import Assembly.Core


-- C64 Constants
color0 = AddrLit16 0xd020
color1 = AddrLit16 0xd021
color2 = AddrLit16 0xd022
color3 = AddrLit16 0xd023


screenRam :: AddressRef
screenRam = AddrLit16 0x0400

colorRam :: AddressRef
colorRam = AddrLit16 0xd800

rasterLine :: AddressRef
rasterLine = AddrLit16 0xd012

borderColor :: AddressRef
borderColor = AddrLit16 0xd020

backgroundColor :: AddressRef
backgroundColor = AddrLit16 0xd021

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
raster = AddrLit16 0xd012 

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
