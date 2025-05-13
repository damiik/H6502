-- | Implements a scrolling horizontal bars effect on the Commodore 64.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE FlexibleContexts #-}

module C64.HorizontalBars (horizontalBars) where

import Prelude hiding (and)
import Prelude (($), (*), fromIntegral, error, show)
import qualified Prelude as P ((+), (*))

import Control.Monad (replicateM_, when)
import Data.Word (Word8, Word16)
import Data.Bits ((.|.), (.>>.))

import Assembly.Core
import Assembly.EDSLInstr
import Assembly.ArithmeticLogic
import Assembly.Memory
import C64.HelpersC64

import C64 (vicRaster, screenRam, colorRam, vicControl1, vicMemoryControl, vicControl2, cia2DataPortA, cia1InterruptControl, cia2InterruptControl, vicBackgroundColor1, vicBackgroundColor2, vicBorderColor, vicBackgroundColor, _BLACK, _WHITE, _RED, _CYAN, _PURPLE, _GREEN, _BLUE, _YELLOW, _ORANGE, _BROWN, _LIGHT_GREY, _GREY, _LIGHT_GREEN, _LIGHT_BLUE, _PINK, _KEY_S, _KEY_D, _KEY_W, _KEY_A, scanKeyboard)
import C64.SerialPort (initAciaEDSL, sendCharAciaEDSL, receiveCharAciaEDSL, exampleAciaProgram, checkErrorsAciaEDSL)

import System.IO (readFile, hFlush, stdout)
import Data.Char (isHexDigit, ord)
import Numeric (readHex)
import Data.List (dropWhile, isPrefixOf, takeWhile, words)
import Data.Maybe (mapMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate)
import Control.DeepSeq (rnf)

-- | Generates a BASIC loader that SYSs to the given address.
basicLoader :: Int -> Asm ()
basicLoader addr =
    let progAddress = map (fromIntegral . ord) $ " " ++ show addr
    in db $
        [ 0x00
        , 0x0c, 0x08
        , 0x10, 0x00
        , 0x9e
        ]
        ++ progAddress ++
        [ 0x00
        , 0x00, 0x00
        ]

-- | Number of characters that fit on one screen row.
screenWidthChars :: Num a => a
screenWidthChars = 40

-- | Number of characters that fit on one screen column.
screenHeightChars :: Num a => a
screenHeightChars = 25

-- | Width of the background map in characters.
mapWidthChars :: Word16
mapWidthChars = 64

-- | Height of the background map in characters.
mapHeightChars :: Word16
mapHeightChars = 64

-- | Total size of the background map in bytes.
mapTotalSize :: Word16
mapTotalSize = mapWidthChars * mapHeightChars

-- | Base address for the program code.
baseCodeAddr :: Address
baseCodeAddr = 0x1000

-- | Address of the character set in RAM.
charsetRam :: AddressRef
charsetRam = AddrLit16 0x2000

-- | Size of the character set in bytes.
charsetSize :: Word16
charsetSize = fromIntegral $ length udgData

-- | Address of the large map data in memory.
largeMapDataAddr :: Address
largeMapDataAddr = 0x2000 + charsetSize

-- | Address of the character map color data in memory.
charMapColorAddr :: Address
charMapColorAddr = largeMapDataAddr + mapTotalSize

-- | Base address for zero page variables (block 1).
zpBase1 :: Word8
zpBase1 = 0x02

-- | Scroll offset variable (zero page).
scrollOffset :: AddressRef
scrollOffset = AddrLit8 zpBase1

-- | Last color variable (zero page).
lastColor :: AddressRef
lastColor = AddrLit8 zpBase1 .+ 0x01

-- | Delay counter variable (zero page).
delayCounter :: AddressRef
delayCounter = AddrLit8 zpBase1 .+ 0x02

-- | Base address for zero page variables (block 2).
zpBase2 :: Word8
zpBase2 = 0x90

-- | Temporary 16-bit register 1.
tmp16R1 :: AddressRef
tmp16R1 = AddrLit8 zpBase2

-- | Temporary 16-bit register 2.
tmp16R2 :: AddressRef
tmp16R2 = AddrLit8 zpBase2 .+ 0x02

-- | Temporary 16-bit register 3.
tmp16R3 :: AddressRef
tmp16R3 = AddrLit8 zpBase2 .+ 0x04

-- | Temporary 16-bit register 4.
tmp16R4 :: AddressRef
tmp16R4 = AddrLit8 zpBase2 .+ 0x06

-- | Temporary 16-bit register 5.
tmp16R5 :: AddressRef
tmp16R5 = AddrLit8 zpBase2 .+ 0x08

-- | Temporary 16-bit register 6.
tmp16R6 :: AddressRef
tmp16R6 = AddrLit8 zpBase2 .+ 0x0A

-- | Temporary 16-bit register 7.
tmp16R7 :: AddressRef
tmp16R7 = AddrLit8 zpBase2 .+ 0x0C

-- | Temporary 16-bit register 8.
tmp16R8 :: AddressRef
tmp16R8 = AddrLit8 zpBase2 .+ 0x0E

-- | Base address for zero page variables (block 3).
zpBase3 :: Word8
zpBase3 = 0xA0

-- | Scroll X offset variable.
scrollX :: AddressRef
scrollX = AddrLit8 zpBase3

-- | Scroll Y offset variable.
scrollY :: AddressRef
scrollY = AddrLit8 zpBase3 .+ 0x01

-- | Map source pointer variable.
mapSrcPtr :: AddressRef
mapSrcPtr = AddrLit8 zpBase3 .+ 0x03

-- | Map color source pointer variable.
mapColorSrcPtr :: AddressRef
mapColorSrcPtr = AddrLit8 zpBase3 .+ 0x05

-- | Screen destination pointer variable.
screenDstPtr :: AddressRef
screenDstPtr = AddrLit8 zpBase3 .+ 0x07

-- | Color destination pointer variable.
colorDstPtr :: AddressRef
colorDstPtr = AddrLit8 zpBase3 .+ 0x09

-- | Base address for zero page variables (block 4).
zpBase4 :: Word8
zpBase4 = 0xF7

-- | Temporary 16-bit register 0.
tmp16R0 :: AddressRef
tmp16R0 = AddrLit8 zpBase4

-- | Row counter variable.
rowCounter :: AddressRef
rowCounter = AddrLit8 zpBase4 .+ 0x02

-- | Temporary byte address.
tmpByteAddr :: Word8
tmpByteAddr = zpBase4 P.+ 0x08

-- | Temporary byte variable.
tmpByte :: AddressRef
tmpByte = AddrLit8 tmpByteAddr

-- | User-defined graphics data loaded from file.
udgData :: [Word8]
udgData = unsafePerformIO $ loadCharsetDataFromFile "c64/world3.asm"

-- | Loads color data from a file.
loadColorDataFromFile :: FilePath -> IO [Word8]
loadColorDataFromFile filePath = do
    content <- readFile filePath
    let fileLines = lines content
        mapDataLines = dropWhile (not . isPrefixOf "charset_attrib_data") fileLines
        byteLines = takeWhile (isPrefixOf ".byte") $ drop 2 mapDataLines
        hexValues = concatMap extractHexValues byteLines
    hFlush stdout
    putStrLn $ "Color data loaded from " ++ filePath
    evaluate $ rnf hexValues
    return hexValues

-- | Loads charset data from a file.
loadCharsetDataFromFile :: FilePath -> IO [Word8]
loadCharsetDataFromFile filePath = do
    content <- readFile filePath
    let fileLines = lines content
        charsetDataLines = dropWhile (not . isPrefixOf "charset_data") fileLines
        byteLines = takeWhile (isPrefixOf ".byte") $ drop 2 charsetDataLines
        hexValues = concatMap extractHexValues byteLines
    hFlush stdout
    putStrLn $ "Charset data loaded from " ++ filePath
    evaluate $ rnf hexValues
    return hexValues

-- | Loads map data from a file.
loadMapDataFromFile :: FilePath -> IO [Word8]
loadMapDataFromFile filePath = do
    content <- readFile filePath
    let fileLines = lines content
        mapDataLines = dropWhile (not . isPrefixOf "map_data") fileLines
        byteLines = takeWhile (isPrefixOf ".byte") $ drop 2 mapDataLines
        hexValues = concatMap extractHexValues byteLines
    hFlush stdout
    putStrLn $ "Map data loaded from " ++ filePath
    evaluate $ rnf hexValues
    return hexValues

-- | Helper to extract hex values from a single ".byte" line.
extractHexValues :: String -> [Word8]
extractHexValues line
    | ".byte" `isPrefixOf` line =
        let values = mapMaybe parseHexValue $ splitBytes $ drop 6 line
        in values
    | otherwise = []
  where
    splitBytes = words . map (\c -> if c == ',' then ' ' else c)

-- | Helper to parse a single hex value like \"$FF\".
parseHexValue :: String -> Maybe Word8
parseHexValue s = case s of
    ('$':rest) -> case readHex rest of
        [(val, "")] -> Just val
        _           -> Nothing
    _          -> Nothing

-- | Large map pattern data loaded from file.
largeMapPattern :: [Word8]
largeMapPattern = unsafePerformIO $ loadMapDataFromFile "c64/world3.asm"

-- | Character map colors data loaded from file.
charMapColors :: [Word8]
charMapColors = unsafePerformIO $ loadColorDataFromFile "c64/world3.asm"

-- | Scrolls the colors in the specified memory region.
scrollColors :: AddressRef -> Asm ()
scrollColors addr = do
    inc scrollOffset
    lda scrollOffset
    and # 0x0f
    tax
    ldy # 0x00
    doWhile_ IsNonZero $ do
        lda $ X ("colors_list"::String)
        sta (Y addr)
        iny
        cmp'y 80

-- | Initializes the game state and hardware.
initGame :: Asm ()
initGame = do
    sta'rb scrollOffset 0
    sta'rb lastColor 0

    lda cia2DataPortA
    and# 0b11111100
    ora# 0b00000011
    sta cia2DataPortA

    lda$ AddrLit8 0x01
    and# 0b11111000
    ora# 0b00000101
    sta$ AddrLit8 0x01

    lda # (0b00010000 .|. (lsb ((addr2word16 charsetRam) .>>. 10)))
    sta vicMemoryControl

    lda vicControl2
    ora# 0b00010000
    sta vicControl2

    sta'rb vicBackgroundColor1 _LIGHT_BLUE
    sta'rb vicBackgroundColor2 _BLUE

    sta'rb vicBorderColor _GREY
    sta'rb vicBackgroundColor _BLACK

    lda# 0x7f
    sta cia1InterruptControl
    sta cia2InterruptControl

    lda # 0
    sta scrollX
    sta scrollY

    jsr ("copyVisibleMap"::String)
    cli

-- | The main assembly program for horizontal bars effect.
horizontalBars :: Asm ()
horizontalBars = do
    org 0x0800
    basicLoader $ fromIntegral baseCodeAddr

    org baseCodeAddr
    l_ "start"
    sei

    initGame
    configureVectors (AddrLabelExpr $ LabelRef "dummyVector")

    -- brk
    l_ "main_loop"

    let helloText = ("helloText"::Label)

    ldx # 0x00
    lda $ X helloText
    while_ IsNonZero $ do
        printChar 10 _BLACK
        inx
        lda $ X helloText

    l_ "scan_keyboard"
    jsr ("scanKeyboard"::Label)
    let scanKeycode = AddrLit8 0xf8
    sta scanKeycode
    cmp# _KEY_S
    if_ IsZero $ add'rb scrollY 1

    lda scanKeycode
    cmp# _KEY_D
    if_ IsZero $ sub'rb scrollX 1

    lda scanKeycode
    cmp# _KEY_W
    if_ IsZero $ sub'rb scrollY 1

    lda scanKeycode
    cmp# _KEY_A
    if_ IsZero $ add'rb scrollX 1

    lda scanKeycode
    jsr ("printByte"::Label)

    vicWaitLine 255

    jsr ("copyVisibleMap"::Label)
    jmp ("main_loop"::Label)

    macrosLib
    initAciaEDSL
    sendCharAciaEDSL
    receiveCharAciaEDSL
    checkErrorsAciaEDSL
    exampleAciaProgram

    l_ "HELLO_MSG"
    db $ map (fromIntegral . ord) "Hello from C64!\r\n\0"

    -- | Subroutine: Fill screen with bars.
    l_ "fill_screen"
    ldx# 0
    doWhile_ IsNonZero $ do
        txa
        replicateM_ 6 $ ror A_
        and# 0x0f
        sta$ X (addr2word16 colorRam)
        sta$ X (addr2word16 colorRam + 250)
        sta$ X (addr2word16 colorRam + 500)
        sta$ X (addr2word16 colorRam + 750)

        lda# 0xA0
        sta$ X (addr2word16 screenRam)
        sta$ X (addr2word16 screenRam + 250)
        sta$ X (addr2word16 screenRam + 500)
        sta$ X (addr2word16 screenRam + 750)
        inx
    rts

    l_ "delay"
    ldx# 0xFF
    doWhile_ IsNonZero $ do
        dex
        ldy# 0xFF
        doWhile_ IsNonZero $ do dey
    rts

    -- | Subroutine: Copy visible map portion to screen.
    l_ "copyVisibleMap"
    let temp16 = tmp16R1
    let temp16_msb = tmp16R1 .+ 1

    sta'rw screenDstPtr (addr2word16 screenRam)
    sta'rw colorDstPtr (addr2word16 colorRam)

    lda scrollY
    sta temp16
    lda# 0
    sta temp16_msb

    ldx# 6
    doWhile_ IsNonZero $ do
        asl $ Just temp16
        rol $ Just temp16_msb
        dex

    clc
    lda scrollX
    adc temp16
    sta temp16
    lda# 0
    adc temp16_msb
    sta temp16_msb

    sta'rw mapSrcPtr largeMapDataAddr
    add'rrw mapSrcPtr tmp16R1

    sta'rw mapColorSrcPtr charMapColorAddr
    add'rrw mapColorSrcPtr tmp16R1

    lda# screenHeightChars
    sta rowCounter
    doWhile_ IsNonZero $ do
        ldy# 0
        clc
        doWhile_ IsNonCarry $ do
            lda $ IY mapSrcPtr
            sta $ IY screenDstPtr

            tax
            lda $ X charMapColorAddr
            sta $ IY colorDstPtr

            iny
            cpy# screenWidthChars
        -- Advance source pointers *mapSrcPtr* and *mapColorSrcPtr* by 1 row (mapWidthChars)
        sta'rw tmp16R1 mapWidthChars -- temp16 = mapWidthChars (64)
        add'rrw mapSrcPtr tmp16R1       -- mapSrcPtr = mapSrcPtr + mapWidthChars
        -- add'rrw mapColorSrcPtr tmp16R1  -- mapColorSrcPtr = mapColorSrcPtr + mapWidthChars

        -- Advance destination pointers *screenDstPtr* and *colorDstPtr* to the start of the next screen row (screenWidthChars)
        sta'rw tmp16R1 screenWidthChars -- temp16 = screenWidthChars (40)
        add'rrw screenDstPtr tmp16R1    -- screenDstPtr = screenDstPtr + screenWidthChars
        add'rrw colorDstPtr tmp16R1     -- colorDstPtr = colorDstPtr + screenWidthChars

        -- Decrement row counter
        dec rowCounter                  -- Decrement rowCounter, sets Z flag when it reaches 0
    rts


    l_ "scanKeyboard"
    scanKeyboard
    -- Dummy IRQ vector for configureVectors
    l_ "printByte"
    printByte 1 15 charMapColorAddr
    l_ "dummyVector"
    rti

    l_ "colors_list" -- Data for scrollColors (example)
    db [ _BLACK, _WHITE, _RED, _CYAN, _PURPLE, _GREEN, _BLUE, _YELLOW, _ORANGE, _BROWN, _GREY, _GREY, _LIGHT_GREEN, _LIGHT_BLUE, _PINK ]
    l_ "helloText"
    stringC64 "Hello, World!"; db [0x00]


    -- character set data, starting from $2000
    org $ addr2word16 charsetRam
    -- Data sections
    l_ "udgDataSource"
    db udgData -- User Defined Graphics data

    l_ "largeMapDataSource"
    db largeMapPattern -- Large map data

    l_ "charMapColorSource"
    db charMapColors -- Character map color data
