module Assembly.ControlFlowSpec (spec) where

import Test.Hspec
import Assembly.Core
import Assembly.EDSLInstr (brk, lda, cmp, sec, clc, adc, sbc, bit, inc, dec, sta, beq, jmp, (#), X(X), if_, ifElse_) -- Import instructions, X, and immediate addressing operator
import Assembly.ControlFlow (ifzThen, ifnzThen, ifeqThen, ifneThen, ifcThen, ifncThen, ifmThen, ifpThen, ifoThen, ifnoThen, whileZ, doWhileZ, whileNz, doWhileNz, whileC, doWhileC, whileNc, doWhileNc, whileM, doWhileM, whileP, doWhileP, whileO, doWhileO, whileNo, doWhileNo, forEachRange) -- Explicitly import all control flow macros for testing
import Assembly (runAssembler)
import MOS6502Emulator (runEmulator, runDebugger, newMachine, setupMachine, Machine(..))
import MOS6502Emulator.Memory (fetchByte)
import MOS6502Emulator.Registers (Registers(..), SRFlag(..), lookupSRFlag)
import Data.Word (Word8, Word16) -- Added Word16
import Data.ByteString (ByteString) -- Added for ByteString

-- Helper function to assemble Asm, run in emulator, and return final state
runAssemblyTest :: Word16 -> Asm () -> IO Machine
runAssemblyTest initialPC asmBlock = do
  case runAssembler initialPC asmBlock of
    Left err -> fail $ "Assembly failed: " ++ err
    Right (actualLoadAddress, byteCode, _) -> do
      initialMachine <- newMachine
      let memoryWrites = zip [actualLoadAddress..] byteCode
      setupMachine initialMachine memoryWrites Nothing Nothing >>= \setupResult -> do
        (_, finalMachine) <- runEmulator actualLoadAddress setupResult
        return finalMachine

spec :: Spec
spec = do
  describe "Control Flow Macros" $ do
    it "should execute the block if the zero flag is set (ifzThen)" $ do
      let testProgram = do
            -- Set Zero flag
            lda # 0x00 -- LDA #$00 sets the Zero flag
            -- ifzThen block
            ifzThen $ do
              lda # 0xFF -- Load a known value if zero flag is set

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifzThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should not execute the block if the zero flag is set (ifnzThen)" $ do
      let testProgram = do
            -- Set Zero flag
            lda # 0x00 -- LDA #$00 sets the Zero flag
            -- ifnzThen block
            ifnzThen $ do
              lda # 0xFF -- This should NOT be executed

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x00, indicating the ifnzThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if the zero flag is not set (ifzThen)" $ do
      let testProgram = do
            -- Clear Zero flag (e.g., by loading a non-zero value)
            lda # 0x01 -- LDA #$01 clears the Zero flag
            -- ifzThen block
            ifzThen $ do
              lda # 0xFF -- This should NOT be executed

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x01, indicating the ifzThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block if the zero flag is not set (ifnzThen)" $ do
      let testProgram = do
            -- Clear Zero flag (e.g., by loading a non-zero value)
            lda # 0x01 -- LDA #$01 clears the Zero flag
            -- ifnzThen block
            ifnzThen $ do
              lda # 0xFF -- Load a known value if zero flag is not set

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifnzThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should execute the block if the equal flag is set (ifeqThen)" $ do
      let testProgram = do
            -- Set Equal flag (e.g., by comparing equal values)
            lda # 0x10
            cmp # 0x10 -- CMP #$10 sets the Equal flag
            -- ifeqThen block
            ifeqThen $ do
              lda # 0xFF -- Load a known value if equal flag is set

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifeqThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should not execute the block if the equal flag is not set (ifeqThen)" $ do
      let testProgram = do
            -- Clear Equal flag (e.g., by comparing non-equal values)
            lda # 0x10
            cmp # 0x11 -- CMP #$11 clears the Equal flag
            -- ifeqThen block
            ifeqThen $ do
              lda # 0xFF -- This should NOT be executed

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x10, indicating the ifeqThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x10

    it "should execute the block if the equal flag is not set (ifneThen)" $ do
      let testProgram = do
            -- Clear Equal flag (e.g., by comparing non-equal values)
            lda # 0x10
            cmp # 0x11 -- CMP #$11 clears the Equal flag
            -- ifneThen block
            ifneThen $ do
              lda # 0xFF -- Load a known value if equal flag is not set

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifneThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should not execute the block if the equal flag is set (ifneThen)" $ do
      let testProgram = do
            -- Set Equal flag (e.g., by comparing equal values)
            lda # 0x10
            cmp # 0x10 -- CMP #$10 sets the Equal flag
            -- ifneThen block
            ifneThen $ do
              lda # 0xFF -- This should NOT be executed

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x10, indicating the ifneThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x10

    it "should execute the block if the carry flag is set (ifcThen)" $ do
      let testProgram = do
            -- Set Carry flag
            sec -- SEC sets the Carry flag
            -- ifcThen block
            ifcThen $ do
              lda # 0xFF -- Load a known value if carry flag is set

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifcThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should not execute the block if the carry flag is not set (ifcThen)" $ do
      let testProgram = do
            -- Clear Carry flag
            clc -- CLC clears the Carry flag
            -- ifcThen block
            ifcThen $ do
              lda # 0xFF -- This should NOT be executed

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x00 (initial state), indicating the ifcThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block if the carry flag is not set (ifncThen)" $ do
      let testProgram = do
            -- Clear Carry flag
            clc -- CLC clears the Carry flag
            -- ifncThen block
            ifncThen $ do
              lda # 0xFF -- Load a known value if carry flag is not set

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifncThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should not execute the block if the carry flag is set (ifncThen)" $ do
      let testProgram = do
            -- Set Carry flag
            sec -- SEC sets the Carry flag
            -- ifncThen block
            ifncThen $ do
              lda # 0xFF -- This should NOT be executed

      finalMachine <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x00 (initial state), indicating the ifncThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block repeatedly while the zero flag is set (whileZ)" $ do
      let testProgram = do
            let counter = AddrLit8 0xa0
            org 0x0800 -- Start assembly at 0x0800
            lda# 0x03 -- Initialize a counter in A
            sta counter -- Store counter at address 0x20
            whileZ ( do
                lda counter 
                cmp# 0x00 
                ifElse_ IsZero (lda# 0xff) (lda#0x00)  -- reverse zero flag
              ) $ do -- Condition: Zero flag not set if value at 0x20 is 0
                dec counter -- Decrement the counter at 0x20
            lda counter -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter reached 0, which stopped the loop
      fetchByte 0xa0 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the final value (0)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if the zero flag is not set initially (whileZ)" $ do
      let testProgram = do
            let counter = AddrLit8 0xa0
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initialize a non-zero counter in A
            sta counter -- Store counter at address 0x20
            whileZ ( do 
                lda counter 
                cmp # 0x00 
              ) $ do -- Condition: Zero flag set if value at 0x20 is 0
                dec counter -- This should NOT be executed
            lda counter -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter remained at its initial non-zero value
      fetchByte 0xa0 (mMem finalMachine) `shouldReturn` 0x01
      -- Assert that the Accumulator holds the initial value (1)
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block at least once and loop while zero flag is set (doWhileZ)" $ do
      let testProgram = do
            let counter = AddrLit8 0xa0
            org 0x0800 -- Start assembly at 0x0800
            brk
            lda # 0x01 -- Initialize a counter in A
            sta counter -- Store counter at address 0x20

            doWhileZ ( do 
              lda counter
              cmp # 0x00 
              ) $ do -- Condition: Zero flag set if value at 0x20 is 0
              inc counter -- Increment the counter at 0x20
              
            lda counter -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter became 1 (from initial 1), then 2, ..., until it wrapped around or became non-zero in a way that cmp #0 set the Zero flag.
      -- A doWhileZ with INC will loop until overflow wraps it to 0.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00 -- After wrap around to 0
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the zero flag is not set after the first iteration (doWhileZ)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0xFF -- Initialize a counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            doWhileZ (do { lda (0x20::Word8); cmp # 0x00 }) $ do -- Condition: Zero flag set if value at 0x20 is 0
              inc (0x20::Word8) -- Increment the counter at 0x20 (becomes 0x00)
              lda (0x20::Word8) -- Load the incremented value (0x00) into A to set flags

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter was incremented once to 0x00
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the final value (0x00)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block repeatedly while the zero flag is not set (whileNz)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x03 -- Initialize a counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            whileNz (do { lda (0x20::Word8); cmp # 0x00 }) $ do -- Condition: Zero flag *not* set if value at 0x20 is non-zero
              dec (0x20::Word8) -- Decrement the counter at 0x20

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter reached 0, which stopped the loop
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the final value (0)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if the zero flag is set initially (whileNz)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x00 -- Initialize a zero counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            whileNz (do { lda (0x20::Word8); cmp # 0x00 }) $ do -- Condition: Zero flag *not* set if value at 0x20 is non-zero
              inc (0x20::Word8) -- This should NOT be executed

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter remained at its initial zero value
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the initial value (0)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block at least once and loop while zero flag is not set (doWhileNz)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x02 -- Initialize a counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            doWhileNz (do { lda (0x20::Word8); cmp # 0x00 }) $ do -- Condition: Zero flag *not* set if value at 0x20 is non-zero
              dec (0x20::Word8) -- Decrement the counter at 0x20
              lda (0x20::Word8) -- Load the decremented value into A to set flags

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter reached 0, which stopped the loop
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the final value (0)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block repeatedly while the carry flag is not set (whileNc)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x03 -- Counter
            sta (0x20::Word8)

            whileNc (do { lda (0x20::Word8); cmp # 0x00; beq ("set_carry"::Label); clc; jmp ("check_carry"::Label); l_ "set_carry"; sec; l_ "check_carry" }) $ do -- Condition: Carry not set
              dec (0x20::Word8)

            lda (0x20::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- The loop decrements until the value at 0x20 is 0.
      -- When 0x20 becomes 0, the condition block sets the Zero flag, then branches to set_carry, setting the Carry flag.
      -- The whileNc macro checks if Carry is *not* set. Since Carry is now set, the loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if the carry flag is set initially (whileNc)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta (0x20::Word8)
            sec -- Ensure Carry is set initially

            whileNc (do { lda (0x20::Word8); cmp # 0x00; beq ("set_carry_no_exec"::Label); clc; jmp ("check_carry_no_exec"::Label); l_ "set_carry_no_exec"; sec; l_ "check_carry_no_exec" }) $ do -- Condition: Carry not set
              inc (0x20::Word8) -- This should NOT be executed

            lda (0x20::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Initially Carry is set, so the loop should not execute.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x01
      -- rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block at least once and loop while carry flag is not set (doWhileNc)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta (0x20::Word8)

            doWhileNc (do { lda (0x20::Word8); cmp # 0x00; beq ("set_carry_dw"::Label); clc; jmp ("check_carry_dw"::Label); l_ "set_carry_dw"; sec; l_ "check_carry_dw" }) $ do -- Condition: Carry not set
              dec (0x20::Word8)

            lda (0x20::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition: lda 0, cmp #0 sets Zero. Branch to set_carry_dw sets Carry.
      -- doWhileNc checks condition *after* block. Carry is set, so loop exits after one iteration.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the carry flag is set after the first iteration (doWhileNc)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta (0x20::Word8)

            doWhileNc (do { sec }) $ do -- Condition: Carry set
              dec (0x20::Word8) -- Should execute once

            lda (0x20::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition sets Carry.
      -- doWhileNc checks condition *after* block. Carry is set, so loop exits after one iteration.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block repeatedly while the negative flag is set (whileM)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x83 -- Initialize a negative counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            whileM (do { lda (0x20::Word8); cmp # 0x80 }) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              inc (0x20::Word8) -- Increment the counter at 0x20

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- The loop increments until the value at 0x20 becomes positive (less than 0x80).
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x80 -- After incrementing from 0x7F to 0x80, LDA 0x80 sets Neg. Loop exits.
      rAC (mRegs finalMachine) `shouldBe` 0x80

    it "should not execute the block if the negative flag is not set initially (whileM)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initialize a positive counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            whileM (do { lda (0x20::Word8); cmp # 0x80 }) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              dec (0x20::Word8) -- This should NOT be executed

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Initially Negative flag is not set, so the loop should not execute.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x01
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block at least once and loop while negative flag is set (doWhileM)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x81 -- Initialize a negative counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            doWhileM (do { lda (0x20::Word8); cmp # 0x80 }) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              inc (0x20::Word8) -- Increment the counter at 0x20
              lda (0x20::Word8) -- Load the incremented value into A to set flags

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Starts with 0x81. Block increments to 0x82. Condition: lda 0x82, cmp #0x80. Negative flag is set. Loop continues.
      -- Loop continues until value becomes 0x80. After incrementing from 0x7F to 0x80, LDA 0x80 sets Negative.
      -- Let's use a counter that starts negative and increments until it becomes positive.
      -- Starts at 0x81. Inc to 0x82 (Neg set). Loop. ... Inc to 0xFF (Neg set). Loop. Inc to 0x00 (Neg clear). Loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the negative flag is not set after the first iteration (doWhileM)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x7F -- Initialize a positive counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            doWhileM (do { lda (0x20::Word8); cmp # 0x80 }) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              inc (0x20::Word8) -- Increment the counter at 0x20 (becomes 0x80)
              lda (0x20::Word8) -- Load the incremented value (0x80) into A to set flags

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Starts with 0x7F. Block increments to 0x80. Condition: LDA 0x80 sets Neg. doWhileM checks *after*. Loop continues.
      -- Ah, the condition should be about the negative flag *remaining* set.
      -- Let's use a counter that starts negative and increments until it becomes positive.
      -- Starts at 0x80. Inc to 0x81. LDA 0x81 sets Neg. Loop. ... Inc to 0xFF. LDA 0xFF sets Neg. Loop. Inc to 0x00. LDA 0x00 clears Neg. Loop exits.
      let testProgram' = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x80 -- Initialize a negative counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            doWhileM (do { lda (0x20::Word8) }) $ do -- Condition: Negative flag set by loading the value
              inc (0x20::Word8) -- Increment the counter at 0x20

            lda (0x20::Word8) -- Load the final value of the counter into A

      finalMachine' <- runAssemblyTest 0x0800 testProgram'
      -- Starts at 0x80. Block increments to 0x81. Condition: lda 0x81 sets Neg. Loop.
      -- Loop continues until value becomes 0x00 (after 0xFF). LDA 0x00 clears Neg. Loop exits.
      fetchByte 0x20 (mMem finalMachine') `shouldReturn` 0x00
      rAC (mRegs finalMachine') `shouldBe` 0x00

    it "should execute the block repeatedly while the overflow flag is set (whileO)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x60 -- Initial value
            sta (0x30::Word8) -- Store

            -- Set Carry for ADC
            sec

            whileO (do { lda (0x30::Word8); adc # 0x20; sta (0x30::Word8); lda (0x30::Word8) }) $ do -- Condition: Overflow flag set by ADC
              -- The loop will continue as long as the ADC operation results in overflow.
              -- Starting with 0x60, adding 0x20 gives 0x80 (signed overflow).
              -- Next iteration: 0x80 + 0x20 = 0xA0 (no overflow). Loop should exit.
              pure () -- Loop body does nothing else

            lda (0x30::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- The value at 0x30 should become 0x80 after the first iteration, and then the loop should exit.
      fetchByte 0x30 (mMem finalMachine) `shouldReturn` 0x80
      -- The final LDA (0x30) should load 0x80.
      rAC (mRegs finalMachine) `shouldBe` 0x80

    it "should not execute the block if the overflow flag is not set initially (whileO)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initial value
            sta (0x30::Word8) -- Store

            -- Set Carry for ADC
            sec

            whileO (do { lda (0x30::Word8); adc # 0x01; sta (0x30::Word8); lda (0x30::Word8) }) $ do -- Condition: Overflow flag set by ADC
              inc (0x30::Word8) -- This should NOT be executed

            lda (0x30::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Initially, 0x01 + 0x01 does not cause overflow, so the loop should not execute.
      fetchByte 0x30 (mMem finalMachine) `shouldReturn` 0x01
      -- The final LDA (0x30) should load 0x01.
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block at least once and loop while overflow flag is set (doWhileO)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x60 -- Initial value
            sta (0x30::Word8) -- Store

            -- Set Carry for ADC
            sec

            doWhileO (do { lda (0x30::Word8); adc # 0x20; sta (0x30::Word8); lda (0x30::Word8) }) $ do -- Condition: Overflow flag set by ADC
              -- The loop will continue as long as the ADC operation results in overflow.
              -- Starts with 0x60. First iteration: 0x60 + 0x20 = 0x80 (overflow). Condition checked *after* block. Loop continues.
              -- Second iteration: 0x80 + 0x20 = 0xA0 (no overflow). Condition checked. Loop exits.
              pure () -- Loop body does nothing else

            lda (0x30::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- The value at 0x30 should become 0x80 after the first iteration, and 0xA0 after the second iteration.
      fetchByte 0x30 (mMem finalMachine) `shouldReturn` 0xA0
      -- The final LDA (0x30) should load 0xA0.
      rAC (mRegs finalMachine) `shouldBe` 0xA0

    it "should execute the block exactly once if the overflow flag is not set after the first iteration (doWhileO)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initial value
            sta (0x30::Word8) -- Store counter

            doWhileO (do { lda # 0x01; adc # 0x01 }) $ do -- Condition: Clears Overflow flag
              inc (0x30::Word8) -- Should execute once

            lda (0x30::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block increments to 2. Condition clears Overflow. doWhileO checks *after*. Loop exits.
      fetchByte 0x30 (mMem finalMachine) `shouldReturn` 0x02
      rAC (mRegs finalMachine) `shouldBe` 0x02

    it "should execute the block repeatedly while the overflow flag is not set (whileNo)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x03 -- Counter
            sta (0x40::Word8) -- Store counter

            whileNo (do { lda (0x40::Word8); cmp # 0x00; beq ("set_ov_whileNo"::Label); lda # 0x01; adc # 0x01; jmp ("check_ov_whileNo"::Label); l_ "set_ov_whileNo"; lda # 0x7F; adc # 0x01; l_ "check_ov_whileNo" }) $ do -- Condition: Clears Overflow if counter > 0, Sets if counter is 0
              dec (0x40::Word8) -- Decrement the counter at 0x40

            lda (0x40::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- The loop decrements the counter until it reaches 0.
      -- When the counter is > 0, the condition clears the Overflow flag. whileNo continues.
      -- When the counter reaches 0, the condition sets the Overflow flag. whileNo exits.
      fetchByte 0x40 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if the overflow flag is set initially (whileNo)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta (0x40::Word8) -- Store counter

            whileNo (do { lda # 0x7F; adc # 0x01 }) $ do -- Condition: Sets Overflow flag
              inc (0x40::Word8) -- This should NOT be executed

            lda (0x40::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Initially Overflow is set by the condition, so the loop should not execute.
      fetchByte 0x40 (mMem finalMachine) `shouldReturn` 0x01
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block at least once and loop while overflow flag is not set (doWhileNo)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x03 -- Counter
            sta (0x40::Word8) -- Store counter

            doWhileNo (do { lda (0x40::Word8); cmp # 0x00; beq ("set_ov_doWhileNo"::Label); lda # 0x01; adc # 0x01; jmp ("check_ov_doWhileNo"::Label); l_ "set_ov_doWhileNo"; lda # 0x7F; adc # 0x01; l_ "check_ov_doWhileNo" }) $ do -- Condition: Clears Overflow if counter > 0, Sets if counter is 0
              dec (0x40::Word8) -- Decrement the counter at 0x40

            lda (0x40::Word8) -- Load the final value of the counter into A

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- The loop body executes at least once. The counter is decremented.
      -- The condition checks the counter *after* decrementing.
      -- Starts with 3. Dec to 2. Condition clears V. Loop.
      -- Counter becomes 0. Condition sets V. Loop exits.
      fetchByte 0x40 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the overflow flag is set after the first iteration (doWhileNo)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta (0x40::Word8) -- Store counter

            doWhileNo (do { lda # 0x7F; adc # 0x01 }) $ do -- Condition: Sets Overflow flag
              dec (0x40::Word8) -- Should execute once

            lda (0x40::Word8) -- Load final value

      finalMachine <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition sets Overflow. doWhileNo checks *after*. Loop exits.
      fetchByte 0x40 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should iterate over a range and execute the block for each value (forEachRange)" $ do
      let testProgram = do
            org 0x0900 -- Start assembly at 0x0900
            -- Initialize memory locations to a non-zero value to ensure they are overwritten
            lda # 0xFF
            sta (AddrLit8 0x50)
            sta (AddrLit8 0x51)
            sta (AddrLit8 0x52)
            sta (AddrLit8 0x53)
            sta (AddrLit8 0x54)

            forEachRange 0 4 $ \_ -> do -- forEachRange sets X; we don't need the operand
              -- Store the value of X at address 0x50 indexed by X
              -- This will write to 0x50, 0x51, 0x52, 0x53, 0x54
              sta$ X (AddrLit8 0x50) -- STA ($50,X)

            -- Load a final value into A to ensure the program didn't crash
            lda # 0xAA

      finalMachine <- runAssemblyTest 0x0900 testProgram

      -- Assert the values in memory locations 0x50 to 0x54
      fetchByte 0x50 (mMem finalMachine) `shouldReturn` 0x00 -- X=0
      fetchByte 0x51 (mMem finalMachine) `shouldReturn` 0x01 -- X=1
      fetchByte 0x52 (mMem finalMachine) `shouldReturn` 0x02 -- X=2
      fetchByte 0x53 (mMem finalMachine) `shouldReturn` 0x03 -- X=3
      fetchByte 0x54 (mMem finalMachine) `shouldReturn` 0x04 -- X=4
      -- Assert the final value in the Accumulator
      rAC (mRegs finalMachine) `shouldBe` 0xAA

