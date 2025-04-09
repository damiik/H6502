module C64.HelloWorld (helloWorld) where

import Assembly.Core
import Assembly(Asm)

startSequence = [0x0c, 0x08, 0xb5, 0x07, 0x9e, 0x20, 0x32, 0x30, 0x36, 0x32, 0x00, 0x00, 0x00]

helloWorld :: Asm ()
helloWorld = do
    org 0x0801 -- Ustawienie adresu początkowego
    db startSequence-- Initialization bytes
    lda $ Imm 0x00  -- Wyczyść ekran
    sta $ OpAbs $ AddrLit16 0xD020       -- Ustaw kolor tła
    sta $ OpAbs $ AddrLit16 0xD021       -- Ustaw kolor ramki

    ldx $ Imm 0x00   -- Licznik/index
    l_ "loop"    
    lda $ AbsXLabel "text"    -- Załaduj kolejny znak
    beq "end"     -- Jeśli zero (koniec ciągu), zakończ
    sta $ OpAbsX $ AddrLit16 0x0400 -- Zapisz znak na ekranie (pozycja 1024)
    inx           --Zwiększ licznik
    jmp $ AbsLabel "loop" --   ; Powtórz pętlę

    l_ "end"
    rts         -- Powrót do BASIC

    l_ "text"
    stringC64 "HELLO WORLD OF DARYO_PL2"  -- Tekst do wyświetlenia + terminator 0
    db [0x00] -- Terminator 0
