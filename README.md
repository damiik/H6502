# H6502
MOS6502 assembly encoded in Haskell EDSL for compile C64 programs in format of .prg


# Usage (Ubuntu Linux):
To use this project, you need to have *GHC* and *stack* installed and set up. You can then create a Haskell file with C64 assembly in /C64 directory, set this file for assembling in *app/main.hs* file (now `C64/HorizontalBars.hs` is used as an example), then compile & run project using the following commands:


* To compile & run the program in *VICE* emulator, just run the following commands in your terminal from your project directory (with  `./c64` directory for the result):

```bash
stack run -- --output ./c64/result.prg && cd c64 && /usr/bin/x64sc result.prg && cd ..
```

* To compile & debug the program in embeded emulator/debugger, you can use the following command:
```bash
stack run -- --debug-address 4096 -s result.lab
```


# Example: Hello World
```haskell

import Assembly.Core
import Assembly.EDSLInstr
import Assembly(Asm)
import Assembly.Macros
import C64

-- | The BASIC loader sequence for C64.
startSequence = [0x0c, 0x08, 0xb5, 0x07, 0x9e, 0x20, 0x32, 0x30, 0x36, 0x32, 0x00, 0x00, 0x00]

-- | Generates the "Hello, World!" program.
helloWorld :: Asm ()
helloWorld = do
    org 0x0801                     -- Set the starting address
    db startSequence               -- Initialization bytes

    lda# _BLACK                    -- Clear the screen
    sta $ vicBorderColor           -- Set border color
    sta $ vicBackgroundColor       -- Set background color

    ldx# 0x00                      -- Counter/index
    lda $ X "text"                 -- Load the first character from the text (set zero flag)
    while_ IsNonZero $ do
        sta $ X screenRam          -- Write character to the screen (position= 1024 + X index)
        inx                        -- Increment X index
        lda $ X "text"             -- Load next character (set zero flag)

    rts                            -- Return to BASIC

    l_ "text"                      -- Define a label for the text
    stringC64 "HELLO WORLD FROM DARYO_PL"; db [0x00]  -- Text to display with null terminator


```

# *while_* macro example

In example above the `while_` macro creates a loop that continues until the condition is false. In this case, it checks if the accumulator is non-zero (with predefined condition `IsNonZero`), which allows the program to print characters until it reaches the *null* terminator, you can use also other predefined macros like `doWhile_`, `if_` etc., or you can also define your own program control macros like that.

`while_` example is one of powerful constructs that allows you to write loops in a more readable way, similar to high-level languages, just using haskell *do block* syntax. It abstracts away the low-level details of setting up loop branch labels, making your assembly code cleaner and easier to understand. 

Definition of `while_` macro is as follows:

```haskell  
while_ :: Conditions -> Asm () -> Asm ()
while_ condition asmBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel   <- makeUniqueLabel ()
    l_ startLabel
    -- Check condition: jump to end if FALSE.
    branchOnCondition (invert condition) endLabel
    -- Execute loop body if condition TRUE.
    asmBlock
    jmp startLabel -- Return to the beginning to check the condition again.
    l_ endLabel
``` 