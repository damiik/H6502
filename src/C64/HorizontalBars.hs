{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals    #-}
module C64.HorizontalBars (horizontalBars) where

import Assembly.Core hiding (and) -- Hide Core's and if Prelude's is needed elsewhere, or qualify Core's usage
import qualified Assembly.Core as C (and) -- Import qualified if needed
import Assembly(Asm)
import Assembly.Macros
import Data.Word (Word8, Word16) -- Added Word16
import Prelude hiding (and) -- Hide Prelude's and
import Data.Bits ((.|.), (.<<.), (.>>.), (.&.), shiftL, shiftR)
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
-- Stałe C64

-- cia1DataPortA      = Addr 0xDC00 -- Joystick Port 2 (nieużywane)

-- Rutyny KERNAL
-- kernalClrscr       = Addr 0xE544
-- kernalGetin        = Addr 0xFFE4 -- Odczytuje znak z klawiatury (blokujące)
-- Rozmiary
screenWidthChars :: Num a => a
screenWidthChars   = 40
screenHeightChars :: Num a => a
screenHeightChars  = 25

-- Mapa tła (większa niż ekran)
mapWidthChars :: Word16
mapWidthChars      = 64 -- Używamy potęgi 2 dla łatwiejszych obliczeń offsetu
mapHeightChars :: Word16
mapHeightChars     = 50

mapTotalSize :: Word16
mapTotalSize       = mapWidthChars * mapHeightChars

baseCodeAddr :: Address
baseCodeAddr       = 0x1000 -- Adres kodu programu

charsetRam :: AddressRef
charsetRam = AddrLit16 0x2000 -- Gdzie umieścimy nasz zestaw znaków (musi być w granicach banku VIC)

charsetSize :: Word16
charsetSize = fromIntegral $ length udgData

-- Adresy danych mapy (muszą być gdzieś w pamięci CPU)
largeMapDataAddr :: Address
largeMapDataAddr   = 0x2000 + charsetSize --addr2word16 charsetRam + (fromIntegral (length udgData)) -- Adres danych mapy                              ---------> USTAWIĆ RĘCZNIE !!!
largeMapColorAddr :: Address
largeMapColorAddr  = largeMapDataAddr + mapTotalSize -- Kolory bezpośrednio po danych mapy

-- Free zero page RAM includes locations 2-6 and $FB-$FE (251-254). 
-- Assuming certain ML math calls and RS-232 communications aren't used, $F7-$FE (247-254) is free.
zpBase1 :: Word8
zpBase1 = 0x02      -- $02-$05
-- Zero Page Variables (using fixed addresses from assembly)
scrollOffset :: AddressRef; scrollOffset = AddrLit8 zpBase1 -- 1
lastColor :: AddressRef; lastColor = AddrLit8 zpBase1 .+ 0x01 --1
delayCounter :: AddressRef; delayCounter = AddrLit8  zpBase1 .+ 0x02 -- 1

zpBase2 :: Word8    
zpBase2 = 0x90      -- $90-$9F
tmp16R1 :: AddressRef; tmp16R1 = AddrLit8 zpBase2 -- 2
tmp16R2 :: AddressRef; tmp16R2 = AddrLit8 zpBase2 .+ 0x02 -- 2
tmp16R3 :: AddressRef; tmp16R3 = AddrLit8 zpBase2 .+ 0x04 -- 2
tmp16R4 :: AddressRef; tmp16R4 = AddrLit8 zpBase2 .+ 0x06 -- 2
tmp16R5 :: AddressRef; tmp16R5 = AddrLit8 zpBase2 .+ 0x08 -- 2
tmp16R6 :: AddressRef; tmp16R6 = AddrLit8 zpBase2 .+ 0x0A -- 2
tmp16R7 :: AddressRef; tmp16R7 = AddrLit8 zpBase2 .+ 0x0C -- 2
tmp16R8 :: AddressRef; tmp16R8 = AddrLit8 zpBase2 .+ 0x0E -- 2


zpBase3 :: Word8
zpBase3 = 0xA0   -- $A0-$AF
scrollX :: AddressRef; scrollX = AddrLit8 zpBase3  -- 1
scrollY :: AddressRef; scrollY = AddrLit8  zpBase3 .+ 0x01 -- 1
mapSrcPtr :: AddressRef; mapSrcPtr = AddrLit8 zpBase3 .+ 0x03 -- 2
mapColorSrcPtr :: AddressRef; mapColorSrcPtr = AddrLit8  zpBase3 .+ 0x05 -- 2
screenDstPtr :: AddressRef; screenDstPtr = AddrLit8 zpBase3 .+ 0x07 -- 2
colorDstPtr :: AddressRef; colorDstPtr = AddrLit8 zpBase3 .+ 0x09 -- 2

-- temporary variables on zero page
zpBase4 :: Word8
zpBase4 = 0xF7      -- $F7-$FE
tmp16R0 :: AddressRef; tmp16R0 = AddrLit8 zpBase4  -- 2
rowCounter :: AddressRef; rowCounter = AddrLit8 zpBase4 .+ 0x02 -- 1
srcTemp :: AddressRef; srcTemp = AddrLit8 zpBase4 .+ 0x04 -- 2
dstTemp :: AddressRef; dstTemp = AddrLit8 zpBase4 .+ 0x06 -- 2


-- --- Definicja zestawu znaków (UDG) ---
-- Znak 0
udgData :: [Word8]
udgData = [ 0b00000000  -- 0x81
           , 0b00000000  -- 0x42
           , 0b00000000  -- 0x24
           , 0b00000000  -- 0x18
           , 0b00000000  -- 0x24
           , 0b00000000  -- 0x42
           , 0b00000000  -- 0x81
           , 0b00000000  -- 0x00
           ] ++

           [ 0b00011000 -- 0x81 -- 1
           , 0b00011000 -- 0x42
           , 0b00111100 -- 0x24 
           , 0b01111110 -- 0x18
           , 0b11111111 -- 0x18
           , 0b11111111 -- 0x24
           , 0b01100110 -- 0x42
           , 0b01100110 -- 0x81
           ] ++
           
           [ 0b00110000 -- 0x81 --2
           , 0b11111000 -- 0x42
           , 0b11111100 -- 0x24 
           , 0b00111111 -- 0x18
           , 0b00111111 -- 0x18
           , 0b11111100 -- 0x24
           , 0b11111000 -- 0x42
           , 0b00110000 -- 0x81
           ] ++

           [ 0b00000000 -- 0x81 --3
           , 0b00001110 -- 0x42
           , 0b00011110 -- 0x24 
           , 0b00111111 -- 0x18
           , 0b01111111 -- 0x18
           , 0b01111110 -- 0x24
           , 0b01100110 -- 0x42
           , 0b01100110 -- 0x81
           ] ++

           [ 0b00000000 -- 0x81 --4
           , 0b00000000 -- 0x42
           , 0b00000000 -- 0x24 
           , 0b00001000 -- 0x18
           , 0b00111110 -- 0x18
           , 0b00001000 -- 0x24
           , 0b00000000 -- 0x42
           , 0b00000000 -- 0x81
           ] ++

           [ 0b00000000 -- 0x81 --5
           , 0b00000000 -- 0x42
           , 0b00000100 -- 0x24 
           , 0b00000000 -- 0x18
           , 0b01100000 -- 0x18
           , 0b01100000 -- 0x24
           , 0b00000000 -- 0x42
           , 0b00010000 -- 0x81
           ] ++


           [ 0b00000000 -- 0x81 --6
           , 0b00000000 -- 0x42
           , 0b00000000 -- 0x24 
           , 0b00001000 -- 0x18
           , 0b00111110 -- 0x18
           , 0b00001000 -- 0x24
           , 0b00000000 -- 0x42
           , 0b00000000 -- 0x81
           ] ++

           [ 0b00000000 -- 0x81 --7
           , 0b00000000 -- 0x42
           , 0b00000100 -- 0x24 
           , 0b00000000 -- 0x18
           , 0b01100000 -- 0x18
           , 0b01100000 -- 0x24
           , 0b00000000 -- 0x42
           , 0b00010000 -- 0x81
           ] ++

           [ 0b11111111 -- 0x81 --8
           , 0b11111111 -- 0x42
           , 0b10111101 -- 0x24 
           , 0b11100111 -- 0x18
           , 0b11100111 -- 0x18
           , 0b10111101 -- 0x24
           , 0b11111111 -- 0x42
           , 0b11111111 -- 0x81
           ]

-- --- Definicja dużej mapy ---
-- Tworzymy mapę 64x50 wypełnioną naprzemiennie znakami 0 i 1
largeMapPattern :: [Word8]
largeMapPattern =  

        [8, 8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [8, 8, 8, 0, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [8, 8, 8, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [8, 8, 8, 0, 1, 0, 0, 3, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [8, 8, 8, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [8, 8, 8, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 2, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ++ 
        [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]  

      

largeMapColors :: [Word8]
largeMapColors = take (fromIntegral mapTotalSize) $ cycle [_CYAN, _YELLOW, _GREEN, _PURPLE] -- Kolory dla znaków  green, purple] -- Kolory dla znaków



-- [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
-- ...,...,
-- ...,15..., 
-- 0...,1...
-- ..., ...,
-- ,...,15...]
-- Helper for scroll table data
-- scrollTableData :: [Word8]
-- scrollTableData = concat $ replicate 2 $ concat
--     [ replicate 8 0,  replicate 8 1
--     , replicate 8 2,  replicate 8 3
--     , replicate 8 4,  replicate 8 5
--     , replicate 8 6,  replicate 8 7
--     , replicate 8 8,  replicate 8 9
--     , replicate 8 10, replicate 8 11
--     , replicate 8 12, replicate 8 13
--     , replicate 8 14, replicate 8 15
--     ] 

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
        cmp_y 80

initGame :: Asm ()
initGame = do
    -- Ustaw bank VIC
    lda $ OpAbs vicBankSelect
    C.and $ Imm 0b11111100
    ora $ Imm 0b00000011
    sta $ OpAbs vicBankSelect

    -- Skonfiguruj VIC dla pamięci ekranu i zestawu znaków
    -- Ekran @ $0400 => Bity 7-4 = %0001 (adres / 1024 = 1)
    -- Bit 0 = 0 (standard hires character mode)
    -- Całość: %00011000 = $18
    lda $ Imm (0b00010000 .|. (lsb ((addr2word16 charsetRam) .>>. 10)))
    -- lda $ Imm 0x18
    sta $ OpAbs vicMemoryControl -- vicMemoryControl = Addr 0xD018


    -- Skopiuj dane UDG
    -- copyBlock charsetRam (AddrLabel "udgDataSource") (fromIntegral $ length udgData0 + length udgData1)

    -- Wyczyść ekran
    jsr $ OpAbs kernalClrscr

    -- Ustaw kolory
    lda $ Imm  _BLACK
    sta $ OpAbs vicBorderColor
    sta $ OpAbs vicBackgroundColor

    -- Zainicjuj pozycję przewijania
    lda $ Imm 0
    sta $ OpZP scrollX
    sta $ OpZP scrollY

    -- Skopiuj początkowy widok mapy
    jsr $ AbsLabel "copyVisibleMap"

    cli

-- The main assembly programB
horizontalBars :: Asm ()
horizontalBars = do
    org 0x0800 -- Ustawienie adresu początkowego dla BASIC loadera
    basicLoader  $ fromIntegral baseCodeAddr -- program starts at $1000   
    -- db $ replicate (0x7f2 + fromIntegral baseCodeAddr - 0x1000)  (0x00 :: Word8) -- Padding to fill the rest of the BASIC loader
    
    org baseCodeAddr -- Ustawienie adresu początkowego dla programu
    l_ "start"
    sei                -- Disable interrupts
    
    sta_rb vicBorderColor _DARK_GREY
    sta_rb vicBackgroundColor _DARK_GREY
    -- Initialize zero page variables
    sta_rb scrollOffset 0
    sta_rb lastColor 0

    --let addr = addr2word16 $ AddrLabelExpr $ LabelRef "dummyVector"
    -- let addr = addr2word16 $ AddrLabelExpr $ LabelRef "dummyVector"

    initGame
    -- configureVectors (AddrLabelExpr $ LabelRef "dummyVector")

    l_ "main_loop"

    -- jsr $ OpAbs kernalGetin -- Wynik (PETSCII) w Akumulatorze
    -- Jeśli A = 0, żaden klawisz nie wciśnięty (GETIN czeka)

    -- Sprawdź kody klawiszy i zaktualizuj scrollX/Y używając if_
    -- Sprawdź Kursor W GÓRĘ (PETSCII 145)
    -- cmp $ OpImm keyCursorUp
    -- if_ AccIsZero $ do          -- Jeśli (A == keyCursorUp)
    --     lda $ OpZP scrollY             -- Wczytaj Y
    --     if_ AccIsNonZero $ do   -- Jeśli (Y != 0)
    --         dec $ OpZP scrollY         --   Zmniejsz Y

    -- -- Sprawdź Kursor W DÓŁ (PETSCII 17)
    -- cmp $ OpImm keyCursorDown
    -- if_ AccIsZero $ do          -- Jeśli (A == keyCursorDown)
    --     lda  $ OpZP scrollY             -- Wczytaj Y
    --     cmp $ OpImm (fromIntegral mapHeightChars - screenHeightChars) -- Porównaj z max Y
    --     if_ IsCarryClear $ do   -- Jeśli (Y < max_Y), CMP nie ustawiło Carry
    --         inc  $ OpZP scrollY         --   Zwiększ Y

    -- -- Sprawdź Kursor W LEWO (PETSCII 157)
    -- cmp $ OpImm keyCursorLeft
    -- if_ AccIsZero $ do          -- Jeśli (A == keyCursorLeft)
    --     lda $ OpZP scrollX             -- Wczytaj X
    --     if_ AccIsNonZero $ do   -- Jeśli (X != 0)
    --         dec  $ OpZP scrollX         --   Zmniejsz X

    -- -- Sprawdź Kursor W PRAWO (PETSCII 29)
    -- cmp $ OpImm keyCursorRight
    -- if_ AccIsZero $ do          -- Jeśli (A == keyCursorRight)
    --     lda $ OpZP scrollX             -- Wczytaj X
    --     cmp $ OpImm (fromIntegral mapWidthChars - screenWidthChars) -- Porównaj z max X
    --     if_ IsCarryClear $ do   -- Jeśli (X < max_X), CMP nie ustawiło Carry
    --         inc  $ OpZP scrollX         --   Zwiększ X
    waitRaster -- Sync with raster

    sta_rb delayCounter 80
    doWhile_ IsNonZero $ do
         jsr $ AbsLabel "delay" -- Delay loop
         dec $ OpAbs delayCounter 

    jsr $ AbsLabel "copyVisibleMap" -- Fill screen with initial bars
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



    -- jsr $ AbsLabel "copyVisibleMap" -- Fill screen with initial bars
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



    -- *** Podprogram Kopiowania Widocznego Fragmentu Mapy ***
    l_ "copyVisibleMap"

    let temp = OpZP tmp16R1
    let temp' = OpZP (tmp16R1 .+ 1)

    -- Ustaw wskaźniki docelowe
    sta_rw screenDstPtr (addr2word16 screenRam)
    sta_rw colorDstPtr (addr2word16 colorRam)

    -- Oblicz adres początkowy w dużej mapie: largeMapData + scrollY * mapWidth + scrollX
    lda $ OpZP scrollY
    sta temp
    lda $ Imm 0
    sta temp'

    -- Mnożenie * 64 (przesunięcie w lewo o 6) - użycie pętli doWhile_
    ldx $ Imm 6
    doWhile_ IsNonZero $ do  -- Pętla wykonuje się, dopóki X > 0
        asl $ Just temp
        rol $ Just temp'
        dex                 -- dex ustawia flagę Z, gdy X osiągnie 0

    -- Dodaj scrollX
    clc
    lda $ OpZP scrollX
    adc temp
    sta temp -- temp16 = offset(getZPAddr temp16)
    lda $ Imm 0
    adc temp'
    sta temp' -- temp16 = offset

    -- Ustaw wskaźniki źródłowe
    sta_zw mapSrcPtr largeMapDataAddr
    add_zzw mapSrcPtr tmp16R1 

    sta_zw mapColorSrcPtr largeMapColorAddr
    add_zzw mapColorSrcPtr tmp16R1

    -- Kopiowanie wiersz po wierszu - użycie pętli doWhile_
    lda $ Imm screenHeightChars  -- set Z flag
    sta $ OpZP rowCounter
    doWhile_ IsNonZero $ do -- Pętla wykonuje się, dopóki rowCounter > 0

        -- Kopiowanie kolumn - użycie pętli doWhile_
        ldy $ Imm 0
        clc                            -- Ustaw Carry na 0
        doWhile_ IsCarryClear $ do     -- Pętla wykonuje się, dopóki Y < screenWidthChars

            -- Kopiuj znak
            lda $ IndY mapSrcPtr       -- Czytaj z mapy źródłowej [mapSrcPtr],Y
            sta $ IndY screenDstPtr    -- Pisz do pamięci ekranu [screenDstPtr],Y
            -- Kopiuj kolor
            lda $ IndY mapColorSrcPtr  -- Czytaj z mapy kolorów [mapColorSrcPtr],Y
            sta $ IndY colorDstPtr     -- Pisz do pamięci kolorów [colorDstPtr],Y
            iny
            cpy $ Imm screenWidthChars -- Ustawia Carry, gdy Y >= screenWidthChars

        -- Przesuń wskaźniki źródłowe *mapSrcPtr* i *mapColorSrcPtr* o 1 wiersz
        sta_zw tmp16R1 mapWidthChars    -- temp16 = mapWidthChars (64)
        add_zzw mapSrcPtr tmp16R1       -- mapSrcPtr = mapSrcPtr + mapWidthChars
        add_zzw mapColorSrcPtr tmp16R1  -- mapColorSrcPtr = mapColorSrcPtr + mapWidthChars

        -- Przesuń wskaźniki *screenDstPtr* i *colorDstPtr* na początek następnego wiersza ekranu
        sta_zw tmp16R1 screenWidthChars  -- temp16 = screenWidthChars (40)
        add_zzw screenDstPtr tmp16R1    -- screenDstPtr = screenDstPtr + screenWidthChars
        add_zzw colorDstPtr tmp16R1     -- colorDstPtr = colorDstPtr + screenWidthChars

        -- Zmniejsz licznik wierszy
        dec $ OpZP rowCounter -- dec ustawia flagę Z, gdy licznik osiągnie 0
    rts

    --org 0x1800
    l_ "dummyVector"
    rti
 

    l_ "colors_list"
    db [_LIGHT_BLUE, _BLUE, _CYAN, _GREEN,
        _LIGHT_GREEN, _YELLOW, _ORANGE, _PINK, 
        _PURPLE, _PINK, _ORANGE, _YELLOW, 
        _LIGHT_GREEN, _GREEN, _CYAN, _BLUE ] -- Color data

    -- replicateM_ 0x94A $ db [0x00 :: Word8] -- Padding to fill the rest of the program

    -- character set data, starting from $2000
    org $ addr2word16 charsetRam

    -- *** Dane ***
    l_ "udgDataSource"
    db udgData

    l_ "largeMapData"
    db largeMapPattern

    l_ "largeMapColorData"
    db largeMapColors
