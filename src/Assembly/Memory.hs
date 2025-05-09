{-# LANGUAGE PatternSynonyms #-}
module Assembly.Memory (
    -- Memory Operations
    addAto16bit,
    sta'rb, sta'rw, sta'rrw,
    adc'rrw, add'rrw,

    -- Block Memory Operations
    copyBlock,
    fillMemory,
    swap'b
) where

import Control.Monad (when) -- Added for db in Assembly.Core (used indirectly by l_)
import Data.Word (Word8, Word16)
import Data.Int (Int) -- Import Int
import Prelude (($), (*), fromIntegral) -- Explicitly import $, *, and fromIntegral operators
import qualified Prelude as P ((+), fromIntegral) -- Needed for fillMemory address calculation and ldx#

import Assembly.Core
    ( Asm,
      Operand(..),
      pattern A_, -- Added A_
      AddressRef(AddrLit16, AddrLit8, AddrLabel), -- Added AddrLabel
      (.+), -- Needed for address arithmetic
      lsb, msb, -- Needed for staRw
      Address, -- Needed for fillMemory
      Label, -- Needed for fillMemory
      makeLabelWithPrefix, -- Added makeLabelWithPrefix for fillMemory
      l_, -- Added l_
      Conditions(IsNonZero) -- Added IsNonZero
      )
import Assembly.EDSLInstr (
    lda, sta, clc, adc, sec, sbc, -- Basic arithmetic and memory access
    ldx, cpx, inx, -- Needed for fillMemory and copyBlock loops
    dey, -- Needed for copyBlock (index)
    while_, doWhile_, -- Needed for copyBlock and fillMemory
    inc, -- Needed for fillMemory
    (#), -- Explicitly import # operator
    asl, rol, -- Added asl and rol for swapB
    tax, ldy, iny, dex, rts, -- Added missing instructions
    IY(IY), -- Explicitly import the IY constructor
    Y(Y)
    )


-- zmienne lokalne na stronie zerowej
-- te adresy mogą się pokrywać z innmi zmiennymi lokalnymi
srcTemp :: AddressRef
srcTemp = AddrLit16 0x12 -- 2
dstTemp :: AddressRef
dstTemp = AddrLit16 0x14 -- 2

-- --- Memory Operations ---

-- Macro for 16-bit addition: Adds the 8-bit value in A to the 16-bit value at accAddr (low byte) and accAddr .+ 1 (high byte)
-- Uses ZP $00 as temporary storage for A. WARNING: Ensure ZP $00 is safe to use.
addAto16bit :: AddressRef -> Asm ()
addAto16bit accAddr = do
    let tempOperandAddr = AddrLit8 0x00 -- Use ZP $00 for temporary storage
    -- Store A (operand) temporarily
    sta tempOperandAddr          -- Store A (operand) temporarily
    -- Add low byte
    lda accAddr          -- Load low byte of accumulator
    clc                          -- Clear carry
    adc tempOperandAddr          -- Add operand from temp storage
    sta accAddr          -- Store result in low byte
    -- Add high byte with carry
    lda (accAddr .+ 1)    -- Load high byte of accumulator (using .+ operator)
    adc# 0                  -- Add 0 + carry
    sta (accAddr .+ 1)    -- Store result in high byte (using .+ operator)

sta'rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
sta'rb op value = do
    lda# value
    sta op

sta'rw :: AddressRef -> Word16 -> Asm() -- op <- value :: Word16
sta'rw op value = do
    lda# lsb value -- Lower byte
    sta op
    lda# msb value-- Upper byte
    sta (op .+ 1) -- Store upper byte in next address

adc'rrw :: AddressRef -> AddressRef -> Asm() -- op1 <- op1 + op2 + carry
adc'rrw op1 op2 = do
    lda op1
    adc op2
    sta op1
    lda (op1 .+ 1)
    adc (op2 .+ 1)
    sta (op1 .+ 1)

add'rrw :: AddressRef -> AddressRef -> Asm() -- op1 <- op1 + op2
add'rrw op1 op2 = do
    clc
    lda op1
    adc op2
    sta op1
    lda (op1 .+ 1)
    adc (op2 .+ 1)
    sta (op1 .+ 1)

sta'rrw :: AddressRef -> AddressRef -> Asm() -- op1 <- op2
sta'rrw op1 op2 = do
    lda op2
    sta op1
    lda (op2 .+ 1)
    sta (op1 .+ 1)

-- --- Block Memory Operations ---

-- | Kopiuje blok pamięci używając pętli while_.
-- | Używa rejestrów A, X, Y.
-- | UWAGA: Kopiuje maksymalnie 256 bajtów (licznik 8-bitowy w X).
-- | Parametry:
-- |   dest: Adres docelowy
-- |   src: Adres źródłowy
-- |   count: Liczba bajtów do skopiowania
copyBlock :: AddressRef -- Cel
            -> AddressRef -- Źródło
            -> Word8 -- Liczba bajtów (max 256)
            -> Asm ()
copyBlock dest src count = do

    -- Inicjalizacja wskaźników (tak jak poprzednio)
    sta'rrw srcTemp src   -- srcTemp = src
    sta'rrw dstTemp dest  -- dstTemp = dest

    -- Inicjalizacja licznika (X) i indeksu (Y)
    lda# count
    tax         -- Przenieś liczbę bajtów do X. WAŻNE: TAX ustawia flagę Z!
                -- Jeśli count=0, Z=1. Jeśli count!=0, Z=0.
    ldy# 0      -- Inicjalizuj indeks Y

    -- Pętla WHILE: kontynuuj, dopóki X jest RÓŻNY od zera (Z=0)
    -- Makro while_ sprawdza warunek *przed* wykonaniem bloku 'do'
    -- Flaga Z jest ustawiona przez TAX przed pierwszym sprawdzeniem
    -- oraz przez DEX na końcu każdej iteracji dla następnego sprawdzenia
    while_ IsNonZero $ do
        -- Ciało pętli:
        lda (IY srcTemp)  -- Załaduj bajt ze źródła
        sta (IY dstTemp)   -- Zapisz bajt do celu
        iny                           -- Zwiększ indeks

        -- Zmniejsz licznik X - to ustawi flagę Z dla sprawdzenia
        -- na początku *następnej* iteracji przez makro while_
        dex

    -- Koniec pętli (gdy warunek ResultIsNonZero przestanie być prawdziwy,
    -- czyli gdy DEX ustawi flagę Z (X=0))
    -- Nie jest potrzebna etykieta końca, makro while_ zarządza skokami


swap'b :: Asm ()
swap'b = do  -- 0xAB -> 0xBA

    asl A_
    adc# 0x80
    rol A_
    asl A_
    adc#  0x80
    rol A_


fillMemory:: AddressRef -> Word8 -> Int -> Asm ()
fillMemory memAddr value sizeKb = do

    sfmd <- makeLabelWithPrefix "sfmd"
    ldx# fromIntegral (4 * sizeKb)  -- 256 * 4 bytes
    ldy# 0 -- 0..255 counter
    cpx# 0
    while_ IsNonZero $ do
        doWhile_ IsNonZero $ do
            lda# value
            l_  sfmd
            sta (Y memAddr) -- MSB of *memAddr* will be incremented when Y==0, LSB of *memAddr* is 0! (is replaced by Y index)
            iny
        inc (AddrLabel sfmd .+ 2)  -- this will increment *STA* instruction second byte operand (at "sfmd")
        dex
    rts