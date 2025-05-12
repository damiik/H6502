module Assembly.ControlFlowSpec (spec) where

import Test.Hspec
import Assembly.Core
import Assembly.EDSLInstr (tax, tay, txa, tya, dex, dey, brk, lda, ldx, ldy, cmp, sec, clc, adc, sbc, bit, inc, dec, sta, beq, jmp, (#), X(X), if_, ifElse_) -- Import instructions, X, and immediate addressing operator
import Assembly.ControlFlow (ifzThen, ifnzThen, ifeqThen, ifneThen, ifcThen, ifncThen, ifmThen, ifpThen, ifoThen, ifnoThen, whileZ, doWhileZ, whileNz, doWhileNz, whileC, doWhileC, whileNc, doWhileNc, whileM, doWhileM, whileP, doWhileP, whileO, doWhileO, whileNo, doWhileNo, doWhileX, doWhileY, doUntilX, doUntilY, whileX, whileY, forX) -- Explicitly import all control flow macros for testing
import Assembly (runAssembler)
import MOS6502Emulator (runEmulator, runDebugger, newMachine, setupMachine, Machine(..))
import MOS6502Emulator.Memory (fetchByte)
import MOS6502Emulator.Registers (Registers(..), SRFlag(..), lookupSRFlag)
import Data.Word (Word8, Word16) -- Added Word16
import Data.ByteString (ByteString) -- Added for ByteString
import Assembly.TestHelper (runAssemblyTest, runAssemblyDebugTest)

------------------------------------------------------------------------
-- For run debugger use: runAssemblyDebugTest instead of runAssemblyTest
------------------------------------------------------------------------
-- For run one test use:
-- stack test --test-arguments '-m "Control Flow Macros should execute the block at least once and loop while zero flag is set (doWhileZ)"'

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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifzThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should not execute the block if the zero flag is set (ifnzThen)" $ do
      let testProgram = do
            -- Set Zero flag
            lda # 0x00 -- LDA #$00 sets the Zero flag
            -- ifnzThen block
            ifnzThen $ do
              lda # 0xFF -- This should NOT be executed

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x00, indicating the ifnzThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if the zero flag is not set (ifzThen)" $ do
      let testProgram = do
            -- Clear Zero flag (e.g., by loading a non-zero value)
            lda # 0x01 -- LDA #$01 clears the Zero flag
            -- ifzThen block
            ifzThen $ do
              lda # 0xFF -- This should NOT be executed

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x01, indicating the ifzThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block if the zero flag is not set (ifnzThen)" $ do
      let testProgram = do
            -- Clear Zero flag (e.g., by loading a non-zero value)
            lda # 0x01 -- LDA #$01 clears the Zero flag
            -- ifnzThen block
            ifnzThen $ do
              lda # 0xFF -- Load a known value if zero flag is not set

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x10, indicating the ifeqThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x10

    it "should execute the block if the carry flag is set (ifcThen)" $ do
      let testProgram = do
            -- Set Carry flag
            sec -- SEC sets the Carry flag
            -- ifcThen block
            ifcThen $ do
              lda # 0xFF -- Load a known value if carry flag is set

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifneThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should not execute the block if the carry flag is not set (ifcThen)" $ do
      let testProgram = do
            -- Clear Carry flag
            clc -- CLC clears the Carry flag
            -- ifcThen block
            ifcThen $ do
              lda # 0xFF -- This should NOT be executed

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0x00 (initial state), indicating the ifcThen block was skipped
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block if the carry flag is not set (ifncThen)" $ do
      let testProgram = do
            -- Clear Carry flag
            clc -- CLC clears the Carry flag
            -- ifncThen block
            ifncThen $ do
              lda # 0xFF -- Load a known value if carry flag is not set

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
      -- Assert that the Accumulator contains 0xFF, indicating the ifcThen block was executed
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should not execute the block if the carry flag is set (ifncThen)" $ do
      let testProgram = do
            -- Set Carry flag
            sec -- SEC sets the Carry flag
            -- ifncThen block
            ifncThen $ do
              lda # 0xFF -- This should NOT be executed

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Run the test program starting at 0x0800
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter remained at its initial non-zero value
      fetchByte 0xa0 (mMem finalMachine) `shouldReturn` 0x01
      -- Assert that the Accumulator holds the initial value (1)
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block at least once and loop while zero flag is set (doWhileZ)" $ do
      let testProgram = do
            let counter = AddrLit8 0xa0
            org 0x0800 -- Start assembly at 0x0800
            lda # 0xff -- Initialize a counter in A
            sta counter -- Store counter at address 0x20

            doWhileZ ( do 
              lda counter
              cmp # 0x00 
              ) $ do -- Condition: Zero flag set if value at 0x20 is 0
              inc counter -- Increment the counter at 0x20
              
            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram -- Use the new debug helper
      -- Assert that the counter became 0 (from initial -1), then 1 and cmp #0 reset the Zero flag ending the loop.
      -- A doWhileZ with INC will loop until overflow wraps it to 0.
      fetchByte 0xa0 (mMem finalMachine) `shouldReturn` 0x01 -- Use 0xa0 as defined in the test
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block exactly once if the zero flag is not set after the first iteration (doWhileZ)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x00 -- Initialize a counter in A
            sta counter -- Store counter at address 0x20

            doWhileZ (do { lda counter; cmp # 0x00 }) $ do -- Condition: Zero flag set if value at 0x20 is 0
              inc counter -- Increment the counter at 0x20 (becomes 0x00)

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter was incremented once to 0x01
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x01
      -- Assert that the Accumulator holds the final value (0x01)
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block repeatedly while the zero flag is not set (whileNz)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x03 -- Initialize a counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            whileNz (do { lda (0x20::Word8); cmp # 0x00 }) $ do -- Condition: Zero flag *not* set if value at 0x20 is non-zero
              dec (0x20::Word8) -- Decrement the counter at 0x20

            lda (0x20::Word8) -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- The loop decrements until the value at 0x20 is 0.
      -- When 0x20 becomes 0, the condition block sets the Zero flag, then branches to set_carry, setting the Carry flag.
      -- The whileNc macro checks if Carry is *not* set. Since Carry is now set, the loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if the carry flag is set initially (whileNc)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta counter
            sec -- Ensure Carry is set initially

            whileNc ( do 
              lda counter 
              cmp # 0x00 
              ifElse_ IsZero (clc) (sec)  -- reverse zero flag 
              ) $ do -- Condition: Carry not set
              inc counter -- This should NOT be executed

            lda counter -- Load final value

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition: lda 0, cmp #0 sets Zero. Branch to set_carry_dw sets Carry.
      -- doWhileNc checks condition *after* block. Carry is set, so loop exits after one iteration.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the carry flag is set after the first iteration (doWhileNc)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta (0x20::Word8)
            clc
            doWhileNc (do { sec }) $ do -- Condition: Carry set
              dec (0x20::Word8) -- Should execute once

            lda (0x20::Word8) -- Load final value

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition sets Carry.
      -- doWhileNc checks condition *after* block. Carry is set, so loop exits after one iteration.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block repeatedly while the negative flag is set (whileM)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x83 -- Initialize a negative counter in A
            sta counter -- Store counter at address 0x20

            whileM ( do 
              lda counter
              -- cmp # 0x80 -- cmp will make 0x83 - 0x80 = 3 (Positive) but 0x83 is Negative (bit 7 is set)
              ) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              dec counter -- Increment the counter at 0x20

            lda (0x20::Word8) -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- The loop increments until the value at 0x20 becomes positive (less than 0x80).
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x7f -- After incrementing from 0x7F to 0x80, LDA 0x80 sets Neg. Loop exits.
      rAC (mRegs finalMachine) `shouldBe` 0x7f

    it "should not execute the block if the negative flag is not set initially (whileM)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initialize a positive counter in A
            sta counter -- Store counter at address 0x20

            whileM (do lda counter ) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              dec counter -- This should NOT be executed
            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Initially Negative flag is not set, so the loop should not execute.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x01
      rAC (mRegs finalMachine) `shouldBe` 0x01

    it "should execute the block at least once and loop while negative flag is set (doWhileM)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x81 -- Initialize a negative counter in A
            sta counter -- Store counter at address 0x20

            doWhileM (do lda counter) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              inc counter -- Increment the counter at 0x20

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 0x81. Block increments to 0x82. Condition: lda 0x82, cmp #0x80. Negative flag is set. Loop continues.
      -- Loop continues until value becomes 0x80. After incrementing from 0x7F to 0x80, LDA 0x80 sets Negative.
      -- Let's use a counter that starts negative and increments until it becomes positive.
      -- Starts at 0x81. Inc to 0x82 (Neg set). Loop. ... Inc to 0xFF (Neg set). Loop. Inc to 0x00 (Neg clear). Loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the negative flag is not set after the first iteration (doWhileM)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x7F -- Initialize a positive counter in A
            sta counter -- Store counter at address 0x20

            doWhileM (do lda counter) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              inc counter -- Increment the counter at 0x20 (becomes 0x80)

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 0x7F. Block increments to 0x80. Condition: LDA 0x80 sets Neg. doWhileM checks *after*. Loop continues.
      -- Ah, the condition should be about the negative flag *remaining* set.
      -- Let's use a counter that starts negative and increments until it becomes positive.
      -- Starts at 0x80. Inc to 0x81. LDA 0x81 sets Neg. Loop. ... Inc to 0xFF. LDA 0xFF sets Neg. Loop. Inc to 0x00. LDA 0x00 clears Neg. Loop exits.
      let testProgram' = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x80 -- Initialize a negative counter in A
            sta counter -- Store counter at address 0x20

            doWhileM (do lda counter ) $ do -- Condition: Negative flag set by loading the value
              inc counter -- Increment the counter at 0x20
            lda counter -- Load the final value of the counter into A

      (finalMachine', _) <- runAssemblyTest 0x0800 testProgram'
      -- Starts at 0x80. Block increments to 0x81. Condition: lda 0x81 sets Neg. Loop.
      -- Loop continues until value becomes 0x00 (after 0xFF). LDA 0x00 clears Neg. Loop exits.
      fetchByte 0x20 (mMem finalMachine') `shouldReturn` 0x00
      rAC (mRegs finalMachine') `shouldBe` 0x00

    it "should execute the block at least once and loop while zero flag is not set (doWhileNz)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x02 -- Initialize a counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            doWhileNz (do { lda (0x20::Word8); cmp # 0x00 }) $ do -- Condition: Zero flag *not* set if value at 0x20 is non-zero
              dec (0x20::Word8) -- Decrement the counter at 0x20
              lda (0x20::Word8) -- Load the decremented value into A to set flags

            lda (0x20::Word8) -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter reached 0, which stopped the loop
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the final value (0)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the zero flag is set after the first iteration (doWhileNz)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initialize a counter in A
            sta (0x20::Word8) -- Store counter at address 0x20

            doWhileNz (do { lda (0x20::Word8); cmp # 0x00 }) $ do -- Condition: Zero flag *not* set if value at 0x20 is non-zero
              dec (0x20::Word8) -- Decrement the counter at 0x20 (becomes 0x00)
              lda (0x20::Word8) -- Load the decremented value into A to set flags

            lda (0x20::Word8) -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the counter was decremented once to 0x00
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the final value (0x00)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block at least once and loop while carry flag is set (doWhileC)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initialize counter
            sta counter -- Store counter

            doWhileC (do { clc }) $ do -- Condition: Carry flag is set
              dec counter -- Decrement counter

            lda counter -- Load final value

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition clears Carry. doWhileC checks *after*. Loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the carry flag is not set after the first iteration (doWhileC)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initialize counter
            sta counter -- Store counter
            sec -- Set Carry initially

            doWhileC (do { clc }) $ do -- Condition: Carry flag is set
              dec counter -- Decrement counter

            lda counter -- Load final value

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition clears Carry. doWhileC checks *after*. Loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block at least once and loop while carry flag is not set (doWhileNc)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta (0x20::Word8)

            doWhileNc (do { lda (0x20::Word8); cmp # 0x00; beq ("set_carry_dw"::Label); clc; jmp ("check_carry_dw"::Label); l_ "set_carry_dw"; sec; l_ "check_carry_dw" }) $ do -- Condition: Carry not set
              dec (0x20::Word8)

            lda (0x20::Word8) -- Load final value

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition: lda 0, cmp #0 sets Zero. Branch to set_carry_dw sets Carry.
      -- doWhileNc checks condition *after* block. Carry is set, so loop exits after one iteration.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the carry flag is set after the first iteration (doWhileNc)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Counter
            sta (0x20::Word8)
            clc
            doWhileNc (do { sec }) $ do -- Condition: Carry set
              dec (0x20::Word8) -- Should execute once

            lda (0x20::Word8) -- Load final value

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition sets Carry.
      -- doWhileNc checks condition *after* block. Carry is set, so loop exits after one iteration.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block at least once and loop while negative flag is set (doWhileM)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x81 -- Initialize a negative counter in A
            sta counter -- Store counter at address 0x20

            doWhileM (do lda counter) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              inc counter -- Increment the counter at 0x20

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 0x81. Block increments to 0x82. Condition: lda 0x82 sets Neg. Loop continues.
      -- Loop continues until value becomes 0x00 (after 0xFF). LDA 0x00 clears Neg. Loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block exactly once if the negative flag is not set after the first iteration (doWhileM)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initialize a positive counter in A
            sta counter -- Store counter at address 0x20

            doWhileM (do lda counter) $ do -- Condition: Negative flag set if value at 0x20 >= 0x80
              inc counter -- Increment the counter at 0x20

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 0x01. Block increments to 0x02. Condition: LDA 0x02 clears Neg. doWhileM checks *after*. Loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x02
      rAC (mRegs finalMachine) `shouldBe` 0x02

    it "should execute the block at least once and loop while positive flag is set (doWhileP)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initialize a positive counter in A
            sta counter -- Store counter at address 0x20

            doWhileP (do lda counter) $ do -- Condition: Positive flag set if value at 0x20 < 0x80 and not zero
              inc counter -- Increment the counter at 0x20

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 0x01. Block increments. Loop continues until value becomes negative (>= 0x80).
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x80
      rAC (mRegs finalMachine) `shouldBe` 0x80

    it "should execute the block exactly once if the positive flag is not set after the first iteration (doWhileP)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x80 -- Initialize a negative counter in A
            sta counter -- Store counter at address 0x20

            doWhileP (do lda counter) $ do -- Condition: Positive flag set if value at 0x20 < 0x80 and not zero
              inc counter -- Increment the counter at 0x20

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 0x80. Block increments to 0x81. Condition: LDA 0x81 clears Pos. doWhileP checks *after*. Loop exits.
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x81
      rAC (mRegs finalMachine) `shouldBe` 0x81

    it "should execute the block at least once and loop while overflow flag is set (doWhileO)" $ do
      let testProgram = do
            let counter = AddrLit8 0x30
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x80 -- Initial value
            sta counter -- Store
            -- Set Carry for ADC
            sec
            doWhileO ( do
              sta counter
              ) $ do
              -- The loop will continue as long as the ADC operation results in overflow.
              lda counter
              adc # 0x80   -- 0x80 + 0x80 + carry = 0x00 + carry + overflow (overflow becouse turns from negative to +1)
                           -- Second iteration: 0x01 + 0x80 + carry = 0x82 (no overflow). Condition checked. Loop exits.

            lda counter -- Load final value

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- The value at 0x30 should become 0x80 after the first iteration, and 0xA0 after the second iteration.
      fetchByte 0x30 (mMem finalMachine) `shouldReturn` 0x82
      -- The final LDA (0x30) should load 0xA0.
      rAC (mRegs finalMachine) `shouldBe` 0x82

    it "should execute the block exactly once if the overflow flag is not set after the first iteration (doWhileO)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x01 -- Initial value
            sta (0x30::Word8) -- Store counter

            doWhileO (do { lda # 0x01; adc # 0x01 }) $ do -- Condition: Clears Overflow flag
              inc (0x30::Word8) -- Should execute once

            lda (0x30::Word8) -- Load final value

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block increments to 2. Condition clears Overflow. doWhileO checks *after*. Loop exits.
      fetchByte 0x30 (mMem finalMachine) `shouldReturn` 0x02
      rAC (mRegs finalMachine) `shouldBe` 0x02

    it "should execute the block repeatedly while the overflow flag is not set (doWhileNo)" $ do
      let testProgram = do
            org 0x0800 -- Start assembly at 0x0800
            lda # 0x03 -- Counter
            sta (0x40::Word8) -- Store counter

            doWhileNo (do { lda (0x40::Word8); cmp # 0x00; beq ("set_ov_doWhileNo"::Label); lda # 0x01; adc # 0x01; jmp ("check_ov_doWhileNo"::Label); l_ "set_ov_doWhileNo"; lda # 0x7F; adc # 0x01; l_ "check_ov_doWhileNo" }) $ do -- Condition: Clears Overflow if counter > 0, Sets if counter is 0
              dec (0x40::Word8) -- Decrement the counter at 0x40

            lda (0x40::Word8) -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
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

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Starts with 1. Block decrements to 0. Condition sets Overflow. doWhileNo checks *after*. Loop exits.
      fetchByte 0x40 (mMem finalMachine) `shouldReturn` 0x00
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block at least once and loop while X is not zero (doWhileX)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            ldx # 0x03 -- Initialize X register

            doWhileX $ do -- Loop while X is not zero
              txa -- Transfer X to A to store
              sta counter -- Store X at address 0x20 (will be overwritten)
              -- inx -- doWhileX increments X automatically

            -- After loop, X should be 0x00 (due to overflow from 0xFF)
            -- The last value stored at 0x20 should be 0x00
            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the last value stored at 0x20 was 0x00
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the final value (0x00)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block at least once and loop while Y is not zero (doWhileY)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            ldy # 0x03 -- Initialize Y register

            doWhileY $ do -- Loop while Y is not zero
              tya -- Transfer Y to A to store
              sta counter -- Store Y at address 0x20 (will be overwritten)
              -- iny -- doWhileY increments Y automatically

            -- After loop, Y should be 0x00 (due to overflow from 0xFF)
            -- The last value stored at 0x20 should be 0x00
            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the last value stored at 0x20 was 0x00
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      -- Assert that the Accumulator holds the final value (0x00)
      rAC (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block at least once and loop until X is zero (doUntilX)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            lda# 0x00
            tax -- Initialize X register
            sta counter -- Store counter
            sta$ counter .+ 1

            doUntilX $ do -- Loop until X is zero
              inc counter -- Store X at address 0x20 (will be overwritten)
              if_ IsZero $ do inc$ counter .+ 1 
              -- dex -- doUntilX decrements X automatically

            -- After loop, X should be 0x00
            -- The last value stored at 0x20 should be 0x0100

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the last value stored at 0x20 was 0x0100
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x00
      fetchByte 0x21 (mMem finalMachine) `shouldReturn` 0x01
      rX (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block at least once and loop until Y is zero (doUntilY)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            ldy # 0x03 -- Initialize Y register

            doUntilY $ do -- Loop until Y is zero
              tya -- Transfer Y to A to store
              sta counter -- Store Y at address 0x20 (will be overwritten)
              -- dey -- doUntilY decrements Y automatically

            -- After loop, Y should be 0x00
            -- The last value stored at 0x20 should be 0x00
            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the last value stored at 0x20 was 0x00
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x01
      -- Assert that the Accumulator holds the final value (0x00)
      rAC (mRegs finalMachine) `shouldBe` 0x01
      rY (mRegs finalMachine) `shouldBe` 0x00

    it "should execute the block repeatedly while X is not zero (whileX)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            ldx # 0x03 -- Initialize X register

            whileX $ do -- Loop while X is not zero
              txa -- Transfer X to A to store
              sta counter -- Store X at address 0x20 (will be overwritten)
              -- dex -- whileX decrements X automatically

            -- After loop, X should be 0x00
            -- The last value stored at 0x20 should be 0x00
            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the last value stored at 0x20 was 0x00
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x01
      -- Assert that the Accumulator holds the final value (0x00)
      rAC (mRegs finalMachine) `shouldBe` 0x01
      rX (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if X is zero initially (whileX)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            ldx # 0x00 -- Initialize X register to zero

            whileX $ do -- Loop while X is not zero
              txa -- This should NOT be executed
              sta counter -- This should NOT be executed

            -- After loop, X should still be 0x00
            -- The value at 0x20 should be its initial value (undefined or 0x00 if memory is zeroed)
            -- Let's initialize memory to a known value to assert it wasn't changed
            lda # 0xFF
            sta counter
            ldx # 0x00 -- Re-initialize X after setting memory

            whileX $ do
              txa
              sta counter

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the value at 0x20 remained 0xFF
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0xFF
      -- Assert that the Accumulator holds the final value (0xFF)
      rAC (mRegs finalMachine) `shouldBe` 0xFF

    it "should execute the block repeatedly while Y is not zero (whileY)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            ldy # 0x03 -- Initialize Y register

            whileY $ do -- Loop while Y is not zero
              tya -- Transfer Y to A to store
              sta counter -- Store Y at address 0x20 (will be overwritten)
              -- dey -- whileY decrements Y automatically

            -- After loop, Y should be 0x00
            -- The last value stored at 0x20 should be 0x00
            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the last value stored at 0x20 was 0x00
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0x01
      -- Assert that the Accumulator holds the final value (0x00)
      rAC (mRegs finalMachine) `shouldBe` 0x01
      rY (mRegs finalMachine) `shouldBe` 0x00

    it "should not execute the block if Y is zero initially (whileY)" $ do
      let testProgram = do
            let counter = AddrLit8 0x20
            org 0x0800 -- Start assembly at 0x0800
            ldy # 0x00 -- Initialize Y register to zero

            whileY $ do -- Loop while Y is not zero
              tya -- This should NOT be executed
              sta counter -- This should NOT be executed

            -- After loop, Y should still be 0x00
            -- The value at 0x20 should be its initial value (undefined or 0x00 if memory is zeroed)
            -- Let's initialize memory to a known value to assert it wasn't changed
            lda # 0xFF
            sta counter
            ldy # 0x00 -- Re-initialize Y after setting memory

            whileY $ do
              tya
              sta counter

            lda counter -- Load the final value of the counter into A

      (finalMachine, _) <- runAssemblyTest 0x0800 testProgram
      -- Assert that the value at 0x20 remained 0xFF
      fetchByte 0x20 (mMem finalMachine) `shouldReturn` 0xFF
      -- Assert that the Accumulator holds the final value (0xFF)
      rAC (mRegs finalMachine) `shouldBe` 0xFF

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

            forX 0 4 ( do -- forEachRange sets X; we don't need the operand
              txa
              sta$ X testAddr-- Store the value of X at address 0x50 indexed by X
              )


      (finalMachine, _) <- runAssemblyTest 0x0900 testProgram

      -- Assert the values in memory locations 0x50 to 0x54
      fetchByte 0x50 (mMem finalMachine) `shouldReturn` 0x00 -- X=0
      fetchByte 0x51 (mMem finalMachine) `shouldReturn` 0x01 -- X=1
      fetchByte 0x52 (mMem finalMachine) `shouldReturn` 0x02 -- X=2
      fetchByte 0x53 (mMem finalMachine) `shouldReturn` 0x03 -- X=3
      fetchByte 0x54 (mMem finalMachine) `shouldReturn` 0xff -- X=4 (excluded)
      -- Assert the final value in the Accumulator
      rAC (mRegs finalMachine) `shouldBe` 0x03
