start:
    lda #0x10  ;-- Wczytaj 0x10 do rejestru A, "#"-immediate mode, not zero page (opcode: A9)
    sta 0x0200 ;-- Użyjmy sensownego adresu strony zerowej lub RAM

loop:
    inx
    ;-- Przykład użycia adresu danych: Załóżmy, że chcemy wyzerować obszar 'values'
    ;-- To nie jest najbardziej efektywny kod 6502, ale pokazuje koncepcję
    sta values,x
    bne loop ;-- Pętla

    lda 0xAA
    jmp end_routine

    rts            ;  -- Niedostępny kod

message:
    .text "Hello Haskell+6502 World!"
    dat: .byte 0x0D, 0x00 ;-- CR + NUL terminator

values:
    .word 0x1234, 0xABCD, 0xFFFF ; -- Kilka wartości 16-bitowych
    .byte 1, 2, 3, 4, 5         ;-- Kilka bajtów

end_routine:
    lda 0x55
    sta 0x0201
    rts