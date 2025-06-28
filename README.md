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
    lda $ X "text"                 -- Load the first character from the text (sets zero flag)
    while_ IsNonZero $ do
        sta $ X screenRam          -- Write character to the screen (position= 1024 + X index)
        inx                        -- Increment X index
        lda $ X "text"             -- Load next character (sets zero flag -> sets exit loop condition)

    rts                            -- Return to BASIC

    l_ "text"                      -- Define a label for the text
    stringC64 "HELLO WORLD FROM DARYO_PL!"; db [0x00]  -- Text to display with null terminator


```

# *while_* macro example

In example above the `while_` macro creates a loop that continues until the condition is true. In this case, it checks if the accumulator is non-zero (with predefined condition `IsNonZero`), which allows the program to print characters until it reaches the *null* terminator. You can use also other predefined macros like `doWhile_`, `if_` etc., or define your own program control macros like that.

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


# Testing
This project includes a set of tests to ensure the correctness of the assembly macros and instructions. The
* To compile & test, you can use the following command (this will run the tests defined in the `test` directory and will use embedded emulator for testing):
```bash
stack test
```

In the `test/Assembly/Spec.hs` file, you can find the main test suite that runs all the tests defined in the `test/Assembly` directory. For example:
* In the `test/Assembly/ListSpec.hs` file there is an example of test sequence for the lists, and you can add your own tests there. 
* In the `test/Assembly/ControlFlowSpec.hs` file there are some tests for the control flow macros, such as `if_`, `while_`, `forX`, etc.

# Example: Testing *forX* macro:
```haskell
it "should iterate over a range and execute the block for each value (forEachRange)" $ do
    let testProgram = do
        org 0x0900 -- Start assembly at 0x0900

        -- Initialize memory locations to a non-zero value to ensure they are overwritten
        let testAddr = AddrLit8 0x50
        lda # 0xFF
        sta testAddr
        sta$ testAddr .+ 1
        sta$ testAddr .+ 2
        sta$ testAddr .+ 3
        sta$ testAddr .+ 4

        forX 0 4 $ do  
            txa
            sta$ X testAddr -- Store the value of X at address testAddr (0x50) + X
            


    (finalMachine, _) <- runAssemblyTest 0x0900 testProgram

    -- Assert the values in memory locations 0x50 to 0x54
    fetchByte 0x50 (_mMem finalMachine) `shouldReturn` 0x00 -- X=0
    fetchByte 0x51 (_mMem finalMachine) `shouldReturn` 0x01 -- X=1
    fetchByte 0x52 (_mMem finalMachine) `shouldReturn` 0x02 -- X=2
    fetchByte 0x53 (_mMem finalMachine) `shouldReturn` 0x03 -- X=3
    fetchByte 0x54 (_mMem finalMachine) `shouldReturn` 0xff -- X=4 (excluded)
    -- Assert the final value in the Accumulator
    _rAC (_mRegs finalMachine) `shouldBe` 0x03
```

Function `forX` is a macro that iterates over a range of values, setting the X register to each value in the range and executing the block of code for each value. In this example, it stores the value of X at memory locations starting from 0x50. 
Compiled MOS6502 assembly program bytes block is stored in *testProgram* variable, which is then run in the emulator by the *runAssemblyTest* function.
Result of execution of MOS6502 code (emulation) is a tuple containing the final state of the machine and the output of the assembly program. The test then checks that the values in memory locations 0x50 to 0x54 are set correctly according to the values of X during the iterations.
Instead of using *runAssemblyTest* function, you can also use *debugAssemblyTest* function to run the program in the embedded emulator with debugging capabilities, which allows you to step through the code and inspect the state of the machine at each step.
Debugger uses *VICE* debugger commands, so you can use the same commands as in VICE to control the execution of the program, inspect memory, registers, etc.
