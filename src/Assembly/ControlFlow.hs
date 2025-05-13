-- | Provides higher-level macros for common control flow patterns.
{-# LANGUAGE PatternSynonyms, BinaryLiterals #-}
module Assembly.ControlFlow (
    -- Control Flow Macros
    ifnzThen, ifzThen, ifneThen, ifeqThen, ifcThen, ifncThen,
    ifmThen, ifpThen, ifoThen, ifnoThen,
    whileZ, doWhileZ,
    whileNz, doWhileNz,
    whileC, doWhileC,
    whileNc, doWhileNc,
    whileM, doWhileM,
    whileP, doWhileP,
    whileO, doWhileO,
    whileNo, doWhileNo,
    whileEq, doWhileEq,
    whileNe, doWhileNe,
    doWhileX,
    doWhileY,
    doUntilX,
    doUntilY,
    whileX, whileY,
    forX, forY,

    -- Case/Switch Statement Macros
    caseOf, caseOfA, caseOfX, caseOfY, caseOfAddr, caseOfZP, caseOfMultiBytes,
    -- Example usages (can be moved to tests later if preferred)
    customCaseExample, checkSpecialPositions,

    -- Helper (potentially hide later) -- Keeping makeUniqueLabel here for now as it's used directly
    makeUniqueLabel, makeLabelWithPrefix
) where

import Control.Monad (mapM, mapM_, sequence_)
import Prelude (($), return, zipWith) -- Explicitly import $, return, and zipWith

import Data.Word (Word8, Word16)

import Assembly.Core
    ( l_,
      makeUniqueLabel, makeLabelWithPrefix, -- Keep makeUniqueLabel and makeLabelWithPrefix here
      Address,
      AddressRef(AddrLabel, AddrLit16, AddrLit8, AddrLabelExpr),
      Asm,
      Operand(..), -- Import all constructors
      pattern Imm, pattern AbsLabel, pattern ZPAddr,
      pattern ImmLsbLabel, pattern ImmMsbLabel, pattern A_,
      (.+), (.-), parens,
      ArithExpr(add, sub), evalLabelExpr, resolveAddressMaybe,
      LabelExpression(LabelRef),
      Conditions(IsNonZero, IsCarry), -- Import specific patterns or just Conditions
      db, -- Used in caseOfMultiBytes example setup
      Label -- Add Label to the import list
      )
import Assembly.EDSLInstr (
      dey, dex,
      beq, bne, bcc, bcs, bpl, bmi, bvc, bvs, -- Branches
      jmp, jsr, rts, -- Jumps and Returns
      inx, iny, -- Increments/Decrements used in loops and other places
      cmp, cpx, cpy, lda, sta, ldy, ldx, -- Instructions with addressing modes (removed #)
      (#) -- Explicitly import the immediate addressing operator
      )
import qualified Prelude as P ((+), (-)) -- Used in caseOfMultiBytes example and others

-- Zero Page Temporary Storage (Move this to a common file if needed by other macro files)
-- For now, keep it here as it was in the original Macros.hs
srcTemp = AddrLit16 0x12 -- 2
dstTemp = AddrLit16 0x14 -- 2

-- --- Control Flow ---

-- --- Conditions ---
-- | Executes the `thenBlock` if the Zero flag is not set (result is non-zero).
ifnzThen :: Asm () -> Asm ()
ifnzThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Zero flag is set (result is zero).
ifzThen :: Asm () -> Asm ()
ifzThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bne endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Zero flag is not set (result is not equal).
ifneThen :: Asm () -> Asm ()
ifneThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Zero flag is set (result is equal).
ifeqThen :: Asm () -> Asm ()
ifeqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bne endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Carry flag is set.
ifcThen :: Asm () -> Asm ()
ifcThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcc endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Carry flag is not set.
ifncThen :: Asm () -> Asm ()
ifncThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcs endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Negative flag is set (result is minus).
ifmThen :: Asm () -> Asm ()
ifmThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bpl endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Negative flag is not set (result is plus).
ifpThen :: Asm () -> Asm ()
ifpThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bmi endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Overflow flag is set.
ifoThen :: Asm () -> Asm ()
ifoThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bvc endLabel
    thenBlock
    l_ endLabel

-- | Executes the `thenBlock` if the Overflow flag is not set.
ifnoThen :: Asm () -> Asm ()
ifnoThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bvs endLabel
    thenBlock
    l_ endLabel

-- --- Loops ---
-- | Loops `X` times with index from `X-1` down to 0. Skips if X is 0.
-- make loop x times with indexes 255..1, ( if x = 0 then skip loop )
whileX :: Asm () -> Asm ()
whileX doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    beq endLabel -- If X is 0 initially, skip loop
    doBlock
    dex
    bne startLabel -- Loop if X is not 0 after decrement
    l_ endLabel

-- | Loops `Y` times with index from `Y-1` down to 0. Skips if Y is 0.
-- make loop y times with indexes 255..1, ( if y = 0 then skip loop )
whileY :: Asm () -> Asm ()
whileY doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    beq endLabel -- If Y is 0 initially, skip loop
    doBlock
    dey
    bne startLabel -- Loop if Y is not 0 after decrement
    l_ endLabel

-- TESTING FIRST!

-- | Loops `X+1` times with index from `X` down to 0. Executes once if X is 0.
-- make loop x times + 1 with indexes 255..0, ( if x = 0 then make loop once)
doWhileX :: Asm () -> Asm ()
doWhileX doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()  -- We need an end label now
    l_ startLabel
    doBlock
    cpx #0  -- Compare X with 0 to check if it's zero
    beq endLabel  -- If X *was* 0 before decrementing, end the loop
    dex
    jmp startLabel  -- Otherwise, decrement and loop back
    l_ endLabel

-- | Loops `Y+1` times with index from `Y` down to 0. Executes once if Y is 0.
-- make loop y times + 1 with indexes 255..0, if y = 0 then make loop once)
doWhileY :: Asm () -> Asm ()
doWhileY doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    cpy #0
    beq endLabel
    dey
    jmp startLabel
    l_ endLabel

-- | Loops `X` times (or 256 times if X is 0 initially) with index from `X-1` down to 0.
-- make loop x times ( if x = 0 then make loop 256 times, with indexes 0,255..1)
doUntilX :: Asm () -> Asm ()
doUntilX doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    dex
    bne startLabel

-- | Loops `Y` times (or 256 times if Y is 0 initially) with index from `Y-1` down to 0.
-- make loop y times ( if y = 0 then make loop 256 times, with indexes 0,255..1)
doUntilY :: Asm () -> Asm ()
doUntilY doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    dey
    bne startLabel




-- TESTING FIRST!
-- | Executes `doBlock` while `conditionBlock` results in Zero flag being set.
whileEq :: Asm () -> Asm () -> Asm ()
whileEq conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bne endLabel
    doBlock
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Zero flag is set.
doWhileEq :: Asm () -> Asm () -> Asm ()
doWhileEq conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    beq startLabel

-- TESTING FIRST!
-- | Executes `doBlock` while `conditionBlock` results in Zero flag being clear.
whileNe :: Asm () -> Asm () -> Asm ()
whileNe conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    beq endLabel
    doBlock
    --loop
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Zero flag is clear.
doWhileNe :: Asm () -> Asm () -> Asm ()
doWhileNe conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bne startLabel

-- | Alias for `whileEq`. Executes `doBlock` while `conditionBlock` results in Zero flag being set.
whileZ :: Asm () -> Asm () -> Asm ()
whileZ = whileEq

-- | Alias for `doWhileEq`. Executes `doBlock` then checks `conditionBlock`. Repeats if Zero flag is set.
doWhileZ :: Asm () -> Asm () -> Asm ()
doWhileZ = doWhileEq

-- TESTING FIRST!
-- | Executes `doBlock` while `conditionBlock` results in Zero flag being clear.
whileNz :: Asm () -> Asm () -> Asm ()
whileNz conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock -- Check condition first
    beq endLabel -- Branch to end if Zero (condition is false for non-zero)
    doBlock
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Zero flag is clear.
doWhileNz :: Asm () -> Asm () -> Asm ()
doWhileNz conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bne startLabel

-- | Executes `doBlock` while `conditionBlock` results in Carry flag being set.
whileC :: Asm () -> Asm () -> Asm ()
whileC conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcc endLabel
    doBlock
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Carry flag is set.
doWhileC :: Asm () -> Asm () -> Asm ()
doWhileC conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bcs startLabel

-- | Executes `doBlock` while `conditionBlock` results in Carry flag being clear.
whileNc :: Asm () -> Asm () -> Asm ()
whileNc conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcs endLabel
    doBlock
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Carry flag is clear.
doWhileNc :: Asm () -> Asm () -> Asm ()
doWhileNc conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bcc startLabel

-- | Executes `doBlock` while `conditionBlock` results in Negative flag being set.
whileM :: Asm () -> Asm () -> Asm ()
whileM conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bpl endLabel
    doBlock
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Negative flag is set.
doWhileM :: Asm () -> Asm () -> Asm ()
doWhileM conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bmi startLabel

-- TESTING FIRST!
-- | Executes `doBlock` while `conditionBlock` results in Negative flag being clear.
whileP :: Asm () -> Asm () -> Asm ()
whileP conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bmi endLabel
    doBlock
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Negative flag is clear.
doWhileP :: Asm () -> Asm () -> Asm ()
doWhileP conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bpl startLabel

-- | Executes `doBlock` while `conditionBlock` results in Overflow flag being set.
whileO :: Asm () -> Asm () -> Asm ()
whileO conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bvc endLabel
    doBlock
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Overflow flag is set.
doWhileO :: Asm () -> Asm () -> Asm ()
doWhileO conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bvs startLabel


-- | Executes `doBlock` while `conditionBlock` results in Overflow flag being clear.
whileNo :: Asm () -> Asm () -> Asm ()
whileNo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bvs endLabel
    doBlock
    jmp startLabel
    l_ endLabel

-- TESTING LAST!
-- | Executes `doBlock` then checks `conditionBlock`. Repeats if Overflow flag is clear.
doWhileNo :: Asm () -> Asm () -> Asm ()
doWhileNo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bvc startLabel


-- --- Iterations ---
-- | Loops with X register from `start` to `end-1`.
-- forX start end action -> loops for X = *start* to (*end* - 1)
forX :: Word8 -> Word8 -> Asm () -> Asm ()
forX start end action = do
    ldx# start
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx# end
    beq endLabel
    action
    inx
    jmp startLabel
    l_ endLabel

-- | Loops with Y register from `start` to `end-1`.
-- forY start end action -> loops for Y = *start* to (*end* - 1)
forY :: Word8 -> Word8 -> Asm () -> Asm ()
forY start end action = do
    ldy# start
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpy# end
    beq endLabel
    action
    inx
    jmp startLabel
    l_ endLabel
-- --- Switches ---
-- | Generic case/switch statement. `compareWith` is a function that takes a value and performs the comparison.
caseOf :: (Word8 -> Asm ()) -> [(Word8, Asm ())] -> Asm ()
caseOf compareWith cases = do
    endLabel <- makeUniqueLabel ()
    casesWithLabels <- mapM (\(val, action) -> do
        caseLabel <- makeUniqueLabel ()
        return (val, caseLabel, action)
      ) cases
    mapM_ (\(val, caseLabel, _) -> do
        compareWith val
        beq caseLabel
      ) casesWithLabels
    jmp endLabel -- Default case: jump to end if no match
    mapM_ (\(_, caseLabel, action) -> do
        l_ caseLabel
        action
        jmp endLabel -- Jump to end after executing case action
      ) casesWithLabels
    l_ endLabel

-- | Case/switch statement comparing with the Accumulator.
caseOfA :: [(Word8, Asm ())] -> Asm ()
caseOfA = caseOf (cmp #)

-- | Case/switch statement comparing with the X register.
caseOfX :: [(Word8, Asm ())] -> Asm ()
caseOfX = caseOf (cpx #)

-- | Case/switch statement comparing with the Y register.
caseOfY :: [(Word8, Asm ())] -> Asm ()
caseOfY = caseOf (cpy #)

-- | Case/switch statement comparing the Accumulator with a value from an address.
caseOfAddr :: AddressRef -> [(Word8, Asm ())] -> Asm ()
caseOfAddr addr = caseOf (\val -> do
    lda addr
    cmp# val)

-- | Case/switch statement comparing the Accumulator with a value from a zero-page address.
caseOfZP :: AddressRef -> [(Word8, Asm ())] -> Asm ()
caseOfZP addr = caseOf (\val -> do
    lda addr
    cmp# val)

-- | Case/switch statement comparing multiple bytes from specified addresses.
caseOfMultiBytes :: [(AddressRef, Word8)] -> [([(Word8, Word8)], Asm ())] -> Asm ()
caseOfMultiBytes addrBytes cases = do
    endLabel <- makeUniqueLabel ()
    defaultLabel <- makeUniqueLabel () -- Label for default case

    casesWithLabels <- mapM (\(vals, action) -> do
        caseLabel <- makeUniqueLabel ()
        matchLabel <- makeUniqueLabel ()
        noMatchLabel <- makeUniqueLabel () -- Potentially redundant now
        return (vals, caseLabel, matchLabel, noMatchLabel, action)
      ) cases

    mapM_ (\(vals, caseLabel, matchLabel, _, _) -> do
        l_ matchLabel -- Start check for this case
        -- Compare each byte pair
        sequence_ $ zipWith (\(addr, _) (val, _) -> do
            lda addr
            cmp# val
            bne defaultLabel -- If any byte doesn't match, jump to default/next check
          ) addrBytes vals
        -- If all bytes matched:
        jmp caseLabel -- Jump to the action label for this case
      ) casesWithLabels

    -- Default case (if none of the above matched)
    l_ defaultLabel
    jmp endLabel -- Jump to the very end

    -- Define action blocks
    mapM_ (\(_, caseLabel, _, _, action) -> do
        l_ caseLabel
        action
        jmp endLabel -- Jump to end after action
      ) casesWithLabels
    l_ endLabel -- Final end label

-- | Example usage of `caseOf` for enemy health.
-- Example usage (remains unchanged, illustrative only)
customCaseExample :: Asm ()
customCaseExample = do
    endCase <- makeUniqueLabel () -- Use a more descriptive name
    caseOf (\val -> do
        lda ("enemy_health"::Label)
        cmp# val
        -- bcs jumps if A >= val (carry set). We want A <= val.
        -- So, compare and branch if *not* less than (A >= val).
        bcs endCase -- If health >= val, skip this case
        -- If we are here, health < val. For <=, we need to handle == separately
        -- A simpler way for <= might be needed depending on exact 6502 flags
        -- For this example, let's assume the logic aims for "less than"
        -- If exact <= is needed, the compare logic needs adjustment.
        ) [
            (11, do  -- Case: enemy_health < 11 (i.e., <= 10)
                jsr ("enemy_critical_animation"::Label)),
            (51, do  -- Case: enemy_health < 51 (i.e., <= 50)
                 -- Important: This case comes *after* the <= 10 case.
                 -- If health is <= 10, it will match the first case and jump out.
                 -- This block only runs if health is 11 <= health <= 50.
                jsr ("enemy_wounded_animation"::Label))

        ]
    l_ endCase


-- | Example usage of `caseOfMultiBytes` for checking player position.
-- Example: Check player position (16-bit x and y coordinates)
checkSpecialPositions :: Asm ()
checkSpecialPositions = do
    caseOfMultiBytes [(AddrLabel "player_x_low", 1), (AddrLabel "player_x_high", 1), (AddrLabel "player_y_low", 1), (AddrLabel "player_y_high", 1)] [
        -- Position (100, 150) = ($64, $00, $96, $00) assuming little-endian
        ([(0x64, 1), (0x00, 1), (0x96, 1), (0x00, 1)], do
            jsr ("trigger_secret_passage"::Label)),

        -- Position (200, 50) = ($C8, $00, $32, $00)
        ([(0xC8, 1), (0x00, 1), (0x32, 1), (0x00, 1)], do
            jsr ("spawn_treasure_chest"::Label)),

        -- Position (150, 150) = ($96, 0x00, 0x96, 0x00) -- Corrected 0x00 to 1
        ([(0x96, 1), (0x00, 1), (0x96, 1), (0x00, 1)], do
            jsr ("activate_teleporter"::Label))
        ]
