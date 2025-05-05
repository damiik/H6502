{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals    #-}
module C64.HorizontalBars (horizontalBars) where

import Prelude hiding (and) -- Hide Prelude's and
import Assembly.Core
import Assembly.EDSLInstr
import Assembly(Asm)
import Assembly.Macros
import Data.Word (Word8, Word16) -- Added Word16
import Data.Bits ((.|.), (.<<.), (.>>.), (.&.), shiftL, shiftR)
import C64
import Control.Monad
import System.IO (readFile)
import Data.Char (isHexDigit)
import Numeric (readHex)
import Data.List (dropWhile, isPrefixOf, takeWhile, words)
import Data.Maybe (mapMaybe)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO) -- For loading data at top level
import Control.Exception (evaluate) -- For forcing evaluation
import Control.DeepSeq (rnf) -- For deep evaluation



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
mapHeightChars     = 64

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
charMapColorAddr :: Address
charMapColorAddr  = largeMapDataAddr + mapTotalSize -- Kolory bezpośrednio po danych mapy

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

-- mapScrPtrAbs :: AddressRef; mapScrPtrAbs = AddrLit16 (fromIntegral zpBase3) .+ 0x0B -- 2
-- mapColPtrAbs :: AddressRef; mapColPtrAbs = AddrLit16 (fromIntegral zpBase3) .+ 0x0D -- 2
-- screenDstPtrAbs :: AddressRef; screenDstPtrAbs = AddrLit16 (fromIntegral zpBase3) .+ 0x0F -- 2
-- colorDstPtrAbs :: AddressRef; colorDstPtrAbs = AddrLit16 (fromIntegral zpBase3) .+ 0x11 -- 2

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
udgData = unsafePerformIO $ loadCharsetDataFromFile "c64/world3.asm"


loadColorDataFromFile :: FilePath -> IO [Word8]
loadColorDataFromFile filePath = do
    content <- readFile filePath
    let fileLines = lines content
    -- Find lines after "map_data" label, ignoring comments and empty lines
    let mapDataLines = dropWhile (not . isPrefixOf "charset_attrib_data") fileLines
    -- Take lines starting with ".byte" until another label or empty line
    let byteLines = takeWhile (isPrefixOf ".byte") $ drop 2 mapDataLines -- drop the "map_data" line itself
    -- Extract hex values from byte lines
    System.IO.hFlush System.IO.stdout -- Force flush
    let hexValues = concatMap extractHexValues byteLines
    putStrLn $ "Color data loaded from " ++ filePath
    evaluate $ rnf hexValues -- Force full evaluation
    return hexValues
-- --- Function to load charset data from file ---
loadCharsetDataFromFile :: FilePath -> IO [Word8]
loadCharsetDataFromFile filePath = do
    content <- readFile filePath
    let fileLines = lines content
    -- Find lines after "charset_data" label, ignoring comments and empty lines
    let charsetDataLines = dropWhile (not . isPrefixOf "charset_data") fileLines
    -- Take lines starting with ".byte" until another label or empty line
    let byteLines = takeWhile (isPrefixOf ".byte") $ drop 2 charsetDataLines -- drop the "charset_data" line itself
    -- Extract hex values from byte lines
    System.IO.hFlush System.IO.stdout -- Force flush
    putStrLn $ "Charset data loaded from " ++ filePath
    let hexValues = concatMap extractHexValues byteLines
    evaluate $ rnf hexValues -- Force full evaluation
    return hexValues

-- --- Function to load map data from file ---
loadMapDataFromFile :: FilePath -> IO [Word8]
loadMapDataFromFile filePath = do
    content <- readFile filePath
    let fileLines = lines content
    -- Find lines after "map_data" label, ignoring comments and empty lines
    let mapDataLines = dropWhile (not . isPrefixOf "map_data") fileLines
    -- Take lines starting with ".byte" until another label or empty line
    let byteLines = takeWhile (isPrefixOf ".byte") $ drop 2 mapDataLines -- drop the "map_data" line itself
    -- Extract hex values from byte lines
    System.IO.hFlush System.IO.stdout -- Force flush
    let hexValues = concatMap extractHexValues byteLines
    putStrLn $ "Map data loaded from " ++ filePath
    evaluate $ rnf hexValues -- Force full evaluation
    return hexValues

-- Helper to extract hex values from a single ".byte" line, handling commas
extractHexValues :: String -> [Word8]
extractHexValues line
    | ".byte" `isPrefixOf` line = unsafePerformIO $ do
        let values = mapMaybe parseHexValue $ splitBytes $ drop 6 line -- drop ".byte "
        putStrLn $ "db:" ++ show values
        System.IO.hFlush System.IO.stdout -- Force flush the output buffer
        return values
    | otherwise = []
    where
      splitBytes = words . map (\c -> if c == ',' then ' ' else c) -- Replace commas with spaces before splitting

-- Helper to parse a single hex value like "$FF"
parseHexValue :: String -> Maybe Word8
parseHexValue s = case s of
    ('$':rest) -> case readHex rest of
                      [(val, "")] -> Just val
                      _           -> Nothing -- Parsing failed or leftover chars
    _          -> Nothing -- Not a hex value starting with $


-- --- Definicja dużej mapy (ładowana z pliku) ---
largeMapPattern :: [Word8]
largeMapPattern = unsafePerformIO $ loadMapDataFromFile "c64/world3.asm"


charMapColors :: [Word8]
charMapColors = unsafePerformIO $ loadColorDataFromFile "c64/world3.asm"    
-- largeMapColors = take (fromIntegral mapTotalSize) $ cycle [_CYAN, _YELLOW, _GREEN, _PURPLE] -- Kolory dla znaków  green, purple] -- Kolory dla znaków


scrollColors :: AddressRef -> Asm ()
scrollColors addr = do
    inc scrollOffset   -- Increment scroll offset
    lda scrollOffset
    and # 0x0f    -- Limit to 16 positions (0-15) -- Use EDSL 'and'
    tax                  -- Store current scroll offset in Y
    ldy # 0x00
    doWhile_ IsNonZero $ do
        lda $  X ("colors_list"::String) -- Get color from list, for x indexed by y
        sta (Y addr) -- Write color to COLOR_RAM[y]
        iny
        cmp_y 80


initGame :: Asm ()
initGame = do

    -- Initialize zero page variables
    sta_rb scrollOffset 0
    sta_rb lastColor 0

    -- Ustaw Bank #3 dla VIC, $C000-$FFFF,
    lda cia2DataPortA
    and# 0b11111100 -- clear bits 0 and 1
    ora# 0b00000011 -- set bits 0 and 1 to 1
    sta cia2DataPortA

    -- Bank out Basic and Kernal ROM
    lda$ AddrLit8 0x01
    and# 0b11111000    -- crear bits 0,1,2
    ora# 0b00000101    -- set bits 0,2
    sta$ AddrLit8 0x01

    -- Skonfiguruj VIC dla pamięci ekranu i zestawu znaków
    -- Ekran @ $0400 => Bity 7-4 = %0001 (adres / 1024 = 1)
    -- Bit 0 = 0 (standard hires character mode)
    -- Całość: %00011000 = $18
    lda # (0b00010000 .|. (lsb ((addr2word16 charsetRam) .>>. 10)))
    sta vicMemoryControl -- vicMemoryControl = Addr 0xD018

    -- set multicolor
    lda vicControl2      -- Read current value
    ora# 0b00010000 -- Set bit 4 ($10 hex)
    sta vicControl2      --Write back

    sta_rb vicBackgroundColor1 _LIGHT_BLUE     -- Ustaw Background Color 1 (np. dla bitów 01 znaku)
    sta_rb vicBackgroundColor2 _BLUE     -- Ustaw Background Color 2 (np. dla bitów 10 znaku)

    -- Skopiuj dane UDG
    -- copyBlock charsetRam (AddrLabel "udgDataSource") (fromIntegral $ length udgData0 + length udgData1)

    -- Wyczyść ekran
    --jsr kernalClrsc

    -- Ustaw kolory
    sta_rb vicBorderColor _DARK_GREY
    sta_rb vicBackgroundColor _BLACK

    lda# 0x7f   -- disable CIA IRQ
    sta cia1InterruptControl
    sta cia2InterruptControl

    -- Zainicjuj pozycję przewijania
    lda # 0
    sta scrollX
    sta scrollY

    -- Skopiuj początkowy widok mapy
    jsr ("copyVisibleMap"::String)

    cli

--     * = $fff0 "IRQ Indirect vector"
-- IRQ_Indirect:
-- 	.label IRQ_LSB = $fff1
-- 	.label IRQ_MSB = $fff2
-- 	jmp $BEEF


-- The main assembly programB
horizontalBars :: Asm ()
horizontalBars = do
    org 0x0800 -- Ustawienie adresu początkowego dla BASIC loadera
    basicLoader  $ fromIntegral baseCodeAddr -- program starts at $1000   
    -- db $ replicate (0x7f2 + fromIntegral baseCodeAddr - 0x1000)  (0x00 :: Word8) -- Padding to fill the rest of the BASIC loader
    
    org baseCodeAddr -- Ustawienie adresu początkowego dla programu
    l_ "start"
    sei                -- Disable interrupts


    --let addr = addr2word16 $ AddrLabelExpr $ LabelRef "dummyVector"
    -- let addr = addr2word16 $ AddrLabelExpr $ LabelRef "dummyVector"

    initGame
    configureVectors (AddrLabelExpr $ LabelRef "dummyVector")


    l_ "main_loop"



    let helloText = ("helloText"::String)
    -- brk

    ldx # 0x00 -- Initialize x register to -1
    lda $ X helloText   -- set zero flag
    while_ IsNonZero $ do
        printChar 10 _BLACK  --Print a character from the hellotext string at the specified position
        inx
        lda $ X helloText   -- set zero flag

    l_ "scan_keyboard"
    jsr ("scanKeyboard"::Label)
    let scanKeycode = AddrLit8 0xf8 -- AddLit8 makes ZP address operand directly from Word8
    sta scanKeycode
    cmp# _KEY_S
    if_ IsZero $ do
        add_rb scrollY 1

    lda scanKeycode
    cmp# _KEY_D
    if_ IsZero $ do
        add_rb scrollX 1

    lda scanKeycode
    cmp# _KEY_W
    if_ IsZero $ do 
        sub_rb scrollY 1

    lda scanKeycode
    cmp# _KEY_A
    if_ IsZero $ do
        sub_rb scrollX 1

    lda scanKeycode
    jsr ("printByte"::Label)
    
    -- let count = ZPAddr 0xf7  -- ZPAddr makes operand directly from Word8
    -- let rest = ZPAddr 0xf8

    -- sta_ob count 0
    -- lda scanKeycode

    -- sec
    -- sbc (Imm 100)
    -- while_ IsCarry $ do
    --     inc count
    --     sbc (Imm 100)
    -- adc (Imm 100) -- undo last dec
    -- sta rest

    -- ldx (Imm 0)
    -- lda count 
    -- clc
    -- adc (Imm 0x30)        -- Convert the count to ASCII 
    -- printChar 10 _YELLOW  --Print a character from the hellotext string at the specified position

    -- sta_ob count 0
    -- lda rest
    -- sec
    -- sbc (Imm 10)
    -- while_ IsCarry $ do
    --     inc count
    --     sbc (Imm 10)
    --     sta rest
    -- clc
    -- adc (Imm 10) -- undo last dec
    -- sta rest

    -- ldx (Imm 1)
    -- lda count 
    -- clc
    -- adc (Imm 0x30)       
    -- printChar 10 _PINK  --Print a character from the hellotext string at the specified position


    -- ldx (Imm 2)
    -- lda rest   
    -- clc
    -- adc (Imm 0x30)  
    -- printChar 10 _LIGHT_BLUE  --Print a character from the hellotext string at the specified position


    -- cmp (Imm _KEY_SPACE)
    -- beq "main_loop"

    -- jmp ("scan_keyboard"::Label)
    -- macrosLib

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
    -- waitRaster -- Sync with raster
    brk
    vicWaitLine 255
    -- jsr ("scrollColors"::Label) -- Scroll colors

    -- sta_rb delayCounter 10
    -- doWhile_ IsNonZero $ do
    --      jsr ("delay"::Label) -- Delay loop
    --      dec delayCounter 

    jsr ("copyVisibleMap"::Label) -- Fill screen with initial bars
    -- scrollColors colorRam
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0028)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0050)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0078)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x00a0)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x00c8)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x00f0)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0118)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0140)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0168)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0190)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x01b8)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x01e0)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0208)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0230)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0258)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0280)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x02a8)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x02d0)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x02f8)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0320)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0348)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0370)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x0398)
    -- scrollColors $ AddrLit16 (addr2word16 colorRam + 0x03c0)

    -- inc lastColor
    -- lda lastColor
    -- and# 0x0f     -- Limit to 16 positions (0-15) -- Use EDSL 'and'
    -- sta lastColor
    -- sta scrollOffset

    -- jsr $ AbsLabel "copyVisibleMap" -- Fill screen with initial bars
    jmp ("main_loop"::Label)
    macrosLib


    -- Subroutine: Fill Screen with Bars
    l_ "fill_screen"
    ldx# 0        -- Fill entire color RAM (implicitly 1000 bytes, but loop handles 256)
    doWhileNz (inx) $ do
        txa                -- Use index as color base
        replicateM_ 6 $ ror A_     -- Rotate right 6 times
        and# 0x0f     -- Limit to 16 colors (0-15) -- Use EDSL 'and'
        sta $ X (addr2word16 colorRam)      -- First quarter (0-249)
        sta $ X (addr2word16 colorRam + 250) -- Second quarter (250-499)
        sta $ X (addr2word16 colorRam + 500) -- Third quarter (500-749)
        sta $ X (addr2word16 colorRam + 750) -- Fourth quarter (750-999)

        lda# 0xA0 --immChar '='   -- Space character code
        sta $ X (addr2word16 screenRam)
        sta $ X (addr2word16 screenRam + 250)
        sta $ X (addr2word16 screenRam + 500)
        sta $ X (addr2word16 screenRam + 750)
    rts

    l_ "delay"
    replicateM_ 200 $ and# 0xef -- Use EDSL 'and'
    rts



    -- Copy visible part of worldmap procedure
    -- *scrollX* and *scrollY* are used for calculate starting worldmap address
    l_ "copyVisibleMap"

    let temp = tmp16R1
    let temp' = tmp16R1 .+ 1

    -- Ustaw wskaźniki docelowe
    sta_rw screenDstPtr (addr2word16 screenRam)
    sta_rw colorDstPtr (addr2word16 colorRam)

    -- Oblicz adres początkowy w dużej mapie: largeMapData + scrollY * mapWidth + scrollX
    lda scrollY
    sta temp
    lda# 0
    sta temp'

    -- Mnożenie * 64 (przesunięcie w lewo o 6) - użycie pętli doWhile_
    ldx# 6
    doWhile_ IsNonZero $ do  -- Pętla wykonuje się, dopóki X > 0
        asl $ Just temp
        rol $ Just temp'
        dex                 -- dex ustawia flagę Z, gdy X osiągnie 0

    -- Dodaj scrollX
    clc
    lda scrollX
    adc temp
    sta temp -- temp16 = offset(getZPAddr temp16)
    lda# 0
    adc temp'
    sta temp' -- temp16 = offset

    -- Ustaw wskaźniki źródłowe
    sta_rw mapSrcPtr largeMapDataAddr
    add_rrw mapSrcPtr tmp16R1 

    sta_rw mapColorSrcPtr charMapColorAddr
    add_rrw mapColorSrcPtr tmp16R1

    -- Kopiowanie wiersz po wierszu - użycie pętli doWhile_
    lda# screenHeightChars  -- set Z flag
    sta rowCounter
    doWhile_ IsNonZero $ do -- Pętla wykonuje się, dopóki rowCounter > 0
        -- Kopiowanie kolumn - użycie pętli doWhile_
        ldy# 0
        clc                             -- Ustaw Carry na 0
        doWhile_ IsNonCarry $ do        -- Pętla wykonuje się, dopóki Y < screenWidthChars
            -- copy char
            lda $ IY mapSrcPtr          -- Czytaj z mapy źródłowej [mapSrcPtr],Y
            sta $ IY screenDstPtr       -- Pisz do pamięci ekranu [screenDstPtr],Y

            -- copy color
            tax                         -- x = current character (index) 
            lda $ X charMapColorAddr    -- get color from char color map
            sta $ IY colorDstPtr        -- Pisz do pamięci kolorów [colorDstPtr],Y
            -- lda $ IY mapColorSrcPtr     -- Czytaj z mapy kolorów [mapColorSrcPtr],Y
            -- sta $ IY colorDstPtr        -- Pisz do pamięci kolorów [colorDstPtr],Y
            iny
            cpy# screenWidthChars       -- Ustawia Carry, gdy Y >= screenWidthChars

        -- Przesuń wskaźniki źródłowe *mapSrcPtr* i *mapColorSrcPtr* o 1 wiersz
        sta_rw tmp16R1 mapWidthChars    -- temp16 = mapWidthChars (64)
        add_rrw mapSrcPtr tmp16R1       -- mapSrcPtr = mapSrcPtr + mapWidthChars
        -- add_rrw mapColorSrcPtr tmp16R1  -- mapColorSrcPtr = mapColorSrcPtr + mapWidthChars

        -- Przesuń wskaźniki *screenDstPtr* i *colorDstPtr* na początek następnego wiersza ekranu
        sta_rw tmp16R1 screenWidthChars -- temp16 = screenWidthChars (40)
        add_rrw screenDstPtr tmp16R1    -- screenDstPtr = screenDstPtr + screenWidthChars
        add_rrw colorDstPtr tmp16R1     -- colorDstPtr = colorDstPtr + screenWidthChars

        -- Zmniejsz licznik wierszy
        dec rowCounter                  -- dec ustawia flagę Z, gdy licznik osiągnie 0
    rts

    l_ "scanKeyboard"
    scanKeyboard

    l_ "printByte"
    printByte 1 15 charMapColorAddr

    --org 0x1800
    l_ "dummyVector"
    rti

    l_ "colors_list"
    db [_LIGHT_BLUE, _BLUE, _CYAN, _GREEN,
        _LIGHT_GREEN, _YELLOW, _ORANGE, _PINK, 
        _PURPLE, _PINK, _ORANGE, _YELLOW, 
        _LIGHT_GREEN, _GREEN, _CYAN, _BLUE ] -- Color data

    l_ "helloText"
    stringC64 "Hello, World!"; db [0x00]

    -- replicateM_ 0x94A $ db [0x00 :: Word8] -- Padding to fill the rest of the program

    -- character set data, starting from $2000
    org $ addr2word16 charsetRam

    -- *** Dane ***
    l_ "udgDataSource"
    db udgData

    l_ "largeMapData"
    db largeMapPattern

    l_ "charMapColorData"
    db charMapColors
