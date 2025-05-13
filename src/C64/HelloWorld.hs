-- | Defines a simple "Hello, World!" program for the Commodore 64.
module C64.HelloWorld (helloWorld) where

import Assembly.Core
import Assembly.EDSLInstr
import Assembly(Asm)
import Assembly.Macros

-- | The BASIC loader sequence for C64.
startSequence = [0x0c, 0x08, 0xb5, 0x07, 0x9e, 0x20, 0x32, 0x30, 0x36, 0x32, 0x00, 0x00, 0x00]

-- | Generates the "Hello, World!" program.
helloWorld :: Asm ()
helloWorld = do
    org 0x0801 -- Set the starting address
    db startSequence-- Initialization bytes
    lda# 0x00  -- Clear the screen
    sta $ AddrLit16 0xD020       -- Set background color
    sta $ AddrLit16 0xD021       -- Set border color

    ldx# 0x00   -- Counter/index
    lda $ X "text" -- Load the first character from the text
    while_ IsNonZero $ do
        sta $ X (AddrLit16 0x0400) -- Write character to the screen (position 1024)
        inx
        lda $ X "text"
    rts         -- Return to BASIC

    l_ "text"
    stringC64 "HELLO WORLD OF DARYO_PL2"  -- Text to display + terminator 0
    db [0x00] -- Terminator 0
