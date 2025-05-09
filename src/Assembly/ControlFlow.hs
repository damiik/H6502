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
    forEachRange,

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
      beq, bne, bcc, bcs, bpl, bmi, bvc, bvs, -- Branches
      jmp, jsr, rts, -- Jumps and Returns
      inx, iny, -- Increments/Decrements used in loops and other places
      cmp, cpx, cpy, lda, sta, ldx, -- Instructions with addressing modes (removed #)
      (#) -- Explicitly import the immediate addressing operator
      )
import qualified Prelude as P ((+), (-)) -- Used in caseOfMultiBytes example and others

-- Zero Page Temporary Storage (Move this to a common file if needed by other macro files)
-- For now, keep it here as it was in the original Macros.hs
srcTemp = AddrLit16 0x12 -- 2
dstTemp = AddrLit16 0x14 -- 2

-- --- Control Flow ---

-- --- Warunki ---
ifnzThen :: Asm () -> Asm ()
ifnzThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel
    thenBlock
    l_ endLabel

ifzThen :: Asm () -> Asm ()
ifzThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bne endLabel
    thenBlock
    l_ endLabel

ifneThen :: Asm () -> Asm ()
ifneThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel
    thenBlock
    l_ endLabel

ifeqThen :: Asm () -> Asm ()
ifeqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bne endLabel
    thenBlock
    l_ endLabel

ifcThen :: Asm () -> Asm ()
ifcThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcc endLabel
    thenBlock
    l_ endLabel

ifncThen :: Asm () -> Asm ()
ifncThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcs endLabel
    thenBlock
    l_ endLabel

ifmThen :: Asm () -> Asm ()
ifmThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bpl endLabel
    thenBlock
    l_ endLabel

ifpThen :: Asm () -> Asm ()
ifpThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bmi endLabel
    thenBlock
    l_ endLabel

ifoThen :: Asm () -> Asm ()
ifoThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bvc endLabel
    thenBlock
    l_ endLabel

ifnoThen :: Asm () -> Asm ()
ifnoThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bvs endLabel
    thenBlock
    l_ endLabel

-- --- Pętle  ---
doWhileX :: Asm () -> Asm ()
doWhileX doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    inx
    bne startLabel

doWhileY :: Asm () -> Asm ()
doWhileY doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    iny
    bne startLabel

-- TESTING FIRST!
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
doWhileEq :: Asm () -> Asm () -> Asm ()
doWhileEq conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    beq startLabel

-- TESTING FIRST!
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
doWhileNe :: Asm () -> Asm () -> Asm ()
doWhileNe conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bne startLabel

whileZ :: Asm () -> Asm () -> Asm ()
whileZ = whileEq

doWhileZ :: Asm () -> Asm () -> Asm ()
doWhileZ = doWhileEq

-- TESTING FIRST!
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
doWhileNz :: Asm () -> Asm () -> Asm ()
doWhileNz conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bne startLabel


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
doWhileC :: Asm () -> Asm () -> Asm ()
doWhileC conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bcs startLabel

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
doWhileNc :: Asm () -> Asm () -> Asm ()
doWhileNc conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bcc startLabel

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
doWhileM :: Asm () -> Asm () -> Asm ()
doWhileM conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bmi startLabel

-- TESTING FIRST!
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
doWhileP :: Asm () -> Asm () -> Asm ()
doWhileP conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bpl startLabel


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
doWhileO :: Asm () -> Asm () -> Asm ()
doWhileO conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bvs startLabel



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
doWhileNo :: Asm () -> Asm () -> Asm ()
doWhileNo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    doBlock
    conditionBlock
    bvc startLabel


-- --- Iteracje ---
forEachRange :: Word8 -> Word8 -> (Operand -> Asm ()) -> Asm ()
forEachRange start end action = do
    ldx# start
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx# end
    beq endLabel
    action (OpAbsX (AddrLit16 0)) -- Note: OpAbsX operand might need adjustment based on use case
    inx
    jmp startLabel
    l_ endLabel

-- --- Przełączniki ---
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

caseOfA :: [(Word8, Asm ())] -> Asm ()
caseOfA = caseOf (cmp #)

caseOfX :: [(Word8, Asm ())] -> Asm ()
caseOfX = caseOf (cpx #)

caseOfY :: [(Word8, Asm ())] -> Asm ()
caseOfY = caseOf (cpy #)

caseOfAddr :: AddressRef -> [(Word8, Asm ())] -> Asm ()
caseOfAddr addr = caseOf (\val -> do
    lda addr
    cmp# val)

caseOfZP :: AddressRef -> [(Word8, Asm ())] -> Asm ()
caseOfZP addr = caseOf (\val -> do
    lda addr
    cmp# val)

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