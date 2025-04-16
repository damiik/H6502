{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE BinaryLiterals    #-}
module ScrollingMap where

import Assembly.Core
import Assembly.Macros
import qualified Data.Map as Map
import Data.Word (Word8, Word16)

-- Stałe C64
vicBankSelect      = Addr 0xDD00
vicMemoryControl   = Addr 0xD018 -- Bity 7-4: Screen Base, Bity 3-1: Charset Base
vicBorderColor     = Addr 0xD020
vicBackgroundColor = Addr 0xD021
vicRaster          = Addr 0xD012
-- cia1DataPortA      = Addr 0xDC00 -- Joystick Port 2 (nieużywane)

-- Rutyny KERNAL
kernalClrscr       = Addr 0xE544
kernalGetin        = Addr 0xFFE4 -- Odczytuje znak z klawiatury (blokujące)

-- Kody PETSCII dla klawiszy kursorów
keyCursorDown  = 17
keyCursorRight = 29
keyCursorUp    = 145
keyCursorLeft  = 157

-- Adresy pamięci programu
charsetTargetBase :: Address
charsetTargetBase  = 0x2000 -- Gdzie umieścimy nasz zestaw znaków (musi być w granicach banku VIC)

screenRamBase :: Address
screenRamBase      = 0x0400 -- Standardowy adres pamięci ekranu

colorRamBase :: Address
colorRamBase       = 0xD800 -- Standardowy adres pamięci kolorów

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

-- Adresy danych mapy (muszą być gdzieś w pamięci CPU)
largeMapDataAddr :: Address
largeMapDataAddr   = 0xA000
largeMapColorAddr :: Address
largeMapColorAddr  = largeMapDataAddr + fromIntegral mapTotalSize -- Kolory bezpośrednio po danych mapy

-- Stałe kolorów
black   = 0
white   = 1
red     = 2
cyan    = 3
purple  = 4
green   = 5
blue    = 6
yellow  = 7

-- Zmienne w stronie zerowej
scrollX :: AddressRef
scrollX = ZPLabel "scrollX" -- Bieżąca pozycja X w dużej mapie (górny lewy róg ekranu)

scrollY :: AddressRef
scrollY = ZPLabel "scrollY" -- Bieżąca pozycja Y w dużej mapie (górny lewy róg ekranu)

mapSrcPtr :: AddressRef
mapSrcPtr = ZPLabel "mapSrcPtr" -- Wskaźnik ZP (2 bajty) do danych źródłowych mapy

mapColorSrcPtr :: AddressRef
mapColorSrcPtr = ZPLabel "mapColorSrcPtr" -- Wskaźnik ZP (2 bajty) do danych źródłowych kolorów

screenDstPtr :: AddressRef
screenDstPtr = ZPLabel "screenDstPtr" -- Wskaźnik ZP (2 bajty) do pamięci ekranu (cel)

colorDstPtr :: AddressRef
colorDstPtr = ZPLabel "colorDstPtr" -- Wskaźnik ZP (2 bajty) do pamięci kolorów (cel)

temp16 :: AddressRef
temp16 = ZPLabel "temp16"     -- Tymczasowy 16-bitowy wskaźnik/wartość

rowCounter :: AddressRef
rowCounter = ZPLabel "rowCounter" -- Licznik wierszy dla pętli kopiowania

-- --- Definicja zestawu znaków (UDG) ---
-- Prosty znak: ukośna linia i kropka
-- Znak 0
udgData :: [Word8]
udgData = [ 0b10000001
          , 0b01000010
          , 0b00100100
          , 0b00011000
          , 0b00100100
          , 0b01000010
          , 0b10000001
          , 0b00000000
          ]

-- Znak 1: Odwrócona linia
udgData1 :: [Word8]
udgData1 = [ 0b10000001
           , 0b01000010
           , 0b00100100
           , 0b00011000
           , 0b00011000
           , 0b00100100
           , 0b01000010
           , 0b10000001
           ]

-- --- Definicja dużej mapy ---
-- Tworzymy mapę 64x50 wypełnioną naprzemiennie znakami 0 i 1
largeMapPattern :: [Word8]
largeMapPattern = take (fromIntegral mapTotalSize) $ cycle [0, 1]

largeMapColors :: [Word8]
largeMapColors = take (fromIntegral mapTotalSize) $ cycle [cyan, yellow, green, purple] -- Kolory dla znaków

-- --- Główny program Asemblera ---
scrollingMapKeyboard :: Asm
scrollingMapKeyboard = do
    org 0xC000 -- Adres startowy programu

    -- Inicjalizacja
    jmp (AbsLabel "init")

    -- *** Podprogram Kopiowania Widocznego Fragmentu Mapy ***
    l_ "copyVisibleMap"

    -- Ustaw wskaźniki docelowe
    load16 (Imm screenRamBase) screenDstPtr
    load16 (Imm colorRamBase) colorDstPtr

    -- Oblicz adres początkowy w dużej mapie: largeMapData + scrollY * mapWidth + scrollX
    lda scrollY
    sta (getZPAddr temp16)
    lda Imm 0
    sta (getZPAddr temp16 + 1)

    -- Mnożenie * 64 (przesunięcie w lewo o 6) - użycie pętli doWhile_
    ldx #6
    doWhile_ AccIsNonZero $ do  -- Pętla wykonuje się, dopóki X > 0
        asl (getZPAddr temp16)
        rol (getZPAddr temp16 + 1)
        dex                 -- dex ustawia flagę Z, gdy X osiągnie 0

    -- Dodaj scrollX
    clc
    lda scrollX
    adc (getZPAddr temp16)
    sta (getZPAddr temp16)
    lda #0
    adc (getZPAddr temp16 + 1)
    sta (getZPAddr temp16 + 1) -- temp16 = offset

    -- Ustaw wskaźniki źródłowe
    load16 (Imm largeMapDataAddr) mapSrcPtr
    add16 mapSrcPtr temp16 mapSrcPtr
    load16 (Imm largeMapColorAddr) mapColorSrcPtr
    add16 mapColorSrcPtr temp16 mapColorSrcPtr

    -- Kopiowanie wiersz po wierszu - użycie pętli doWhile_
    lda #screenHeightChars
    sta rowCounter
    doWhile_ AccIsNonZero $ do -- Pętla wykonuje się, dopóki rowCounter > 0
        -- Kopiowanie kolumn - użycie pętli doWhile_
        ldy #0
        doWhile_ IsCarryClear $ do -- Pętla wykonuje się, dopóki Y < screenWidthChars
            -- Kopiuj znak
            lda [mapSrcPtr],Y
            sta [screenDstPtr],Y
            -- Kopiuj kolor
            lda [mapColorSrcPtr],Y
            sta [colorDstPtr],Y
            iny
            cpy #screenWidthChars -- Ustawia Carry, gdy Y >= screenWidthChars

        -- Przesuń wskaźniki na początek następnego wiersza
        load16Imm mapWidthChars temp16
        add16 mapSrcPtr temp16 mapSrcPtr
        add16 mapColorSrcPtr temp16 mapColorSrcPtr
        load16Imm screenWidthChars temp16
        add16 screenDstPtr temp16 screenDstPtr
        add16 colorDstPtr temp16 colorDstPtr

        -- Zmniejsz licznik wierszy
        dec rowCounter -- dec ustawia flagę Z, gdy licznik osiągnie 0

    rts

    -- *** Główna Pętla Programu ***
    l_ "mainLoop"
    jsr "waitForRaster" -- Czekaj na ramkę

    -- Odczytaj klawisz (blokująco)
    jsr kernalGetin -- Wynik (PETSCII) w Akumulatorze
    -- Jeśli A = 0, żaden klawisz nie wciśnięty (GETIN czeka)

    -- Sprawdź kody klawiszy i zaktualizuj scrollX/Y używając if_
    -- Sprawdź Kursor W GÓRĘ (PETSCII 145)
    cmp #keyCursorUp
    if_ AccIsZero $ do          -- Jeśli (A == keyCursorUp)
        lda scrollY             -- Wczytaj Y
        if_ AccIsNonZero $ do   -- Jeśli (Y != 0)
            dec scrollY         --   Zmniejsz Y

    -- Sprawdź Kursor W DÓŁ (PETSCII 17)
    cmp #keyCursorDown
    if_ AccIsZero $ do          -- Jeśli (A == keyCursorDown)
        lda scrollY             -- Wczytaj Y
        cmp #(fromIntegral mapHeightChars - fromIntegral screenHeightChars) -- Porównaj z max Y
        if_ IsCarryClear $ do   -- Jeśli (Y < max_Y), CMP nie ustawiło Carry
            inc scrollY         --   Zwiększ Y

    -- Sprawdź Kursor W LEWO (PETSCII 157)
    cmp #keyCursorLeft
    if_ AccIsZero $ do          -- Jeśli (A == keyCursorLeft)
        lda scrollX             -- Wczytaj X
        if_ AccIsNonZero $ do   -- Jeśli (X != 0)
            dec scrollX         --   Zmniejsz X

    -- Sprawdź Kursor W PRAWO (PETSCII 29)
    cmp #keyCursorRight
    if_ AccIsZero $ do          -- Jeśli (A == keyCursorRight)
        lda scrollX             -- Wczytaj X
        cmp #(fromIntegral mapWidthChars - fromIntegral screenWidthChars) -- Porównaj z max X
        if_ IsCarryClear $ do   -- Jeśli (X < max_X), CMP nie ustawiło Carry
            inc scrollX         --   Zwiększ X

    -- Po sprawdzeniu klawiszy, przerysuj mapę (jeśli coś się zmieniło lub nie)
    -- Dla uproszczenia rysujemy zawsze po wciśnięciu klawisza
    jsr "copyVisibleMap"

    -- Pętla nieskończona
    jmp "mainLoop"

    -- *** Inicjalizacja ***
    l_ "init"
    sei

    -- Ustaw bank VIC
    lda vicBankSelect
    and #%11111100
    ora #%00000011
    sta vicBankSelect

    -- Skonfiguruj VIC dla pamięci ekranu i zestawu znaków
    lda #0x18
    sta vicMemoryControl

    -- Skopiuj dane UDG
    copyBlock (AbsLabel "udgDataSource") (AbsLit charsetTargetBase) (Imm (fromIntegral $ length udgData + length udgData1))

    -- Wyczyść ekran
    jsr kernalClrscr

    -- Ustaw kolory
    lda #black
    sta vicBorderColor
    sta vicBackgroundColor

    -- Zainicjuj pozycję przewijania
    lda #0
    sta scrollX
    sta scrollY

    -- Skopiuj początkowy widok mapy
    jsr "copyVisibleMap"

    cli
    jmp "mainLoop"

    -- *** Dane ***
    l_ "udgDataSource"
    db udgData
    db udgData1

    l_ "largeMapData"
    db largeMapPattern

    l_ "largeMapColorData"
    db largeMapColors


    -- *** Rezerwacja miejsca w stronie zerowej ***
    reserveZPBytes "scrollX" 1
    reserveZPBytes "scrollY" 1
    reserveZPBytes "mapSrcPtr" 2
    reserveZPBytes "mapColorSrcPtr" 2
    reserveZPBytes "screenDstPtr" 2
    reserveZPBytes "colorDstPtr" 2
    reserveZPBytes "temp16" 2
    reserveZPBytes "rowCounter" 1

    -- Dołącz makra (jeśli nie są w osobnym module kompilowanym razem)
    -- Zakładamy, że są dostępne przez import Assembly.Macros
    -- waitForRaster -- używane
    -- load16, store16, add16 -- używane
    -- copyBlock -- używane
    -- if_, doWhile_ -- używane
