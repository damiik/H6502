module C64.HelloWorld (helloWorld) where

import Assembly.Core
import Assembly.EDSLInstr
import Assembly(Asm)
import Assembly.Macros

startSequence = [0x0c, 0x08, 0xb5, 0x07, 0x9e, 0x20, 0x32, 0x30, 0x36, 0x32, 0x00, 0x00, 0x00]

helloWorld :: Asm ()
helloWorld = do
    org 0x0801 -- Ustawienie adresu początkowego
    db startSequence-- Initialization bytes
    lda# 0x00  -- Wyczyść ekran
    sta $ AddrLit16 0xD020       -- Ustaw kolor tła
    sta $ AddrLit16 0xD021       -- Ustaw kolor ramki

    ldx# 0x00   -- Licznik/index
    lda $ X "text" -- Załaduj pierwszy znak z tekstu
    while_ IsNonZero $ do
        sta $ X (AddrLit16 0x0400) -- Zapisz znak na ekranie (pozycja 1024)    
        inx
        lda $ X "text"
    rts         -- Powrót do BASIC

    l_ "text"
    stringC64 "HELLO WORLD OF DARYO_PL2"  -- Tekst do wyświetlenia + terminator 0
    db [0x00] -- Terminator 0
