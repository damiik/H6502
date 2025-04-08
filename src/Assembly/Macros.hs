{-# LANGUAGE PatternSynonyms #-}
module Assembly.Macros (
    -- Macros
    ifNotEqThen, ifEqThen, ifCarryThen, ifNotCarryThen,
    ifMinusThen, ifPlusThen, ifOverflowThen, ifNotOverflowThen,
    whileEqDo, whileNotEqDo, whileCarryDo, whileNotCarryDo,
    whileMinusDo, whilePlusDo, whileOverflowDo, whileNotOverflowDo,
    forEachRange,
    caseOf, caseOfA, caseOfX, caseOfY, caseOfAddr, caseOfZP, caseOfMultiBytes,
    addAto16bit, -- Add 8-bit value in A to a 16-bit memory location
    -- Helper (potentially hide later)
    makeUniqueLabel
) where

--import Assembly.List (forEach, forEachListWithIndex, mapListInPlace, mapListToNew, filterList, foldList, filterMoreThan, sumList)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word8)
import Assembly.Core
    ( adc,
      bcc,
      bcs,
      beq,
      bmi,
      bne,
      bpl,
      bvc,
      bvs,
      clc,
      cmp,
      cpx,
      cpy,
      inx,
      jmp,
      jsr,
      l_,
      lda,
      ldx,
      makeUniqueLabel,
      sta,
      zpLit,
      AddressRef(AddrLabel, AddrLit16),
      Asm,
      Operand(..), -- Import all constructors
      pattern Imm, -- Import the pattern synonym
      pattern AbsLabel,
      (.+), -- Import renamed operator
      (.-), -- Import renamed operator
      parens, -- Import parens if needed by macros (though unlikely)
      ArithExpr(add, sub) -- Import the class methods if needed directly (unlikely)
      )
import Prelude hiding((+), (-), and, or) -- Keep hiding Prelude's + and - if P.(+) is used elsewhere
import qualified Prelude as P ((+), (-))

-- --- Warunki (bez zmian) ---
ifNotEqThen :: Asm () -> Asm ()
ifNotEqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel
    thenBlock
    l_ endLabel

ifEqThen :: Asm () -> Asm ()
ifEqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bne endLabel
    thenBlock
    l_ endLabel

ifCarryThen :: Asm () -> Asm ()
ifCarryThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcc endLabel
    thenBlock
    l_ endLabel

ifNotCarryThen :: Asm () -> Asm ()
ifNotCarryThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bcs endLabel
    thenBlock
    l_ endLabel

ifMinusThen :: Asm () -> Asm ()
ifMinusThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bpl endLabel
    thenBlock
    l_ endLabel

ifPlusThen :: Asm () -> Asm ()
ifPlusThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bmi endLabel
    thenBlock
    l_ endLabel

ifOverflowThen :: Asm () -> Asm ()
ifOverflowThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bvc endLabel
    thenBlock
    l_ endLabel

ifNotOverflowThen :: Asm () -> Asm ()
ifNotOverflowThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bvs endLabel
    thenBlock
    l_ endLabel

-- --- Pętle (bez zmian) ---
whileEqDo :: Asm () -> Asm () -> Asm ()
whileEqDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bne endLabel
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileNotEqDo :: Asm () -> Asm () -> Asm ()
whileNotEqDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    --let loop = do jmp $ AbsLabel startLabel

    l_ startLabel
    conditionBlock
    beq endLabel
    doBlock
    --loop
    jmp $ AbsLabel startLabel
    l_ endLabel

whileCarryDo :: Asm () -> Asm () -> Asm ()
whileCarryDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcc endLabel
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileNotCarryDo :: Asm () -> Asm () -> Asm ()
whileNotCarryDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcs endLabel
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileMinusDo :: Asm () -> Asm () -> Asm ()
whileMinusDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bpl endLabel
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whilePlusDo :: Asm () -> Asm () -> Asm ()
whilePlusDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bmi endLabel
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileOverflowDo :: Asm () -> Asm () -> Asm ()
whileOverflowDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bvc endLabel
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileNotOverflowDo :: Asm () -> Asm () -> Asm ()
whileNotOverflowDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bvs endLabel
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

-- Macro for 16-bit addition: Adds the 8-bit value in A to the 16-bit value at accAddr (low byte) and accAddr .+ 1 (high byte)
-- Uses ZP $00 as temporary storage for A. WARNING: Ensure ZP $00 is safe to use.
addAto16bit :: AddressRef -> Asm ()
addAto16bit accAddr = do
    let tempOperandAddr = zpLit 0x00 -- Use ZP $00 for temporary storage
    -- Store A (operand) temporarily
    sta tempOperandAddr          -- Store A (operand) temporarily
    -- Add low byte
    lda $ OpAbs accAddr          -- Load low byte of accumulator
    clc                          -- Clear carry
    adc tempOperandAddr          -- Add operand from temp storage
    sta $ OpAbs accAddr          -- Store result in low byte
    -- Add high byte with carry
    lda $ OpAbs (accAddr .+ 1)    -- Load high byte of accumulator (using .+ operator)
    adc $ Imm 0                  -- Add 0 + carry
    sta $ OpAbs (accAddr .+ 1)    -- Store result in high byte (using .+ operator)

-- --- Iteracje (bez zmian) ---
forEachRange :: Word8 -> Word8 -> (Operand -> Asm ()) -> Asm ()
forEachRange start end action = do
    ldx $ Imm start
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ Imm end
    beq endLabel
    action (OpAbsX (AddrLit16 0)) -- Note: OpAbsX operand might need adjustment based on use case
    inx
    jmp $ AbsLabel startLabel
    l_ endLabel

-- --- Przełączniki (bez zmian) ---
caseOf :: (Word8 -> Asm ()) -> [(Word8, Asm ())] -> Asm ()
caseOf compareWith cases = do
    endLabel <- makeUniqueLabel ()
    casesWithLabels <- mapM (\(val, action) -> do
        caseLabel <- makeUniqueLabel ()
        return (val, caseLabel, action)
      ) cases
    mapM_ (\(val, caseLabel, _) -> do -- Removed unused 'action' from this mapM_
        compareWith val
        beq caseLabel
      ) casesWithLabels
    jmp $ AbsLabel endLabel -- Default case: jump to end if no match
    mapM_ (\(_, caseLabel, action) -> do
        l_ caseLabel
        action
        jmp $ AbsLabel endLabel -- Jump to end after executing case action
      ) casesWithLabels
    l_ endLabel

caseOfA :: [(Word8, Asm ())] -> Asm ()
caseOfA = caseOf (cmp . Imm)

caseOfX :: [(Word8, Asm ())] -> Asm ()
caseOfX = caseOf (cpx . Imm)

caseOfY :: [(Word8, Asm ())] -> Asm ()
caseOfY = caseOf (cpy . Imm)

caseOfAddr :: AddressRef -> [(Word8, Asm ())] -> Asm ()
caseOfAddr addr = caseOf (\val -> do
    lda $ OpAbs addr
    cmp $ Imm val)

caseOfZP :: AddressRef -> [(Word8, Asm ())] -> Asm ()
caseOfZP addr = caseOf (\val -> do
    lda $ OpZP addr
    cmp $ Imm val)

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
            lda $ OpAbs addr
            cmp $ Imm val
            bne defaultLabel -- If any byte doesn't match, jump to default/next check
          ) addrBytes vals
        -- If all bytes matched:
        jmp $ AbsLabel caseLabel -- Jump to the action label for this case
      ) casesWithLabels

    -- Default case (if none of the above matched)
    l_ defaultLabel
    jmp $ AbsLabel endLabel -- Jump to the very end

    -- Define action blocks
    mapM_ (\(_, caseLabel, _, _, action) -> do
        l_ caseLabel
        action
        jmp $ AbsLabel endLabel -- Jump to end after action
      ) casesWithLabels

    l_ endLabel -- Final end label

-- Example usage (remains unchanged, illustrative only)
customCaseExample :: Asm ()
customCaseExample = do
    endCase <- makeUniqueLabel () -- Use a more descriptive name
    caseOf (\val -> do
        lda $ AbsLabel "enemy_health"
        cmp $ Imm val
        -- bcs jumps if A >= val (carry set). We want A <= val.
        -- So, compare and branch if *not* less than (A >= val).
        bcs endCase -- If health >= val, skip this case
        -- If we are here, health < val. For <=, we need to handle == separately
        -- A simpler way for <= might be needed depending on exact 6502 flags
        -- For this example, let's assume the logic aims for "less than"
        -- If exact <= is needed, the compare logic needs adjustment.
        ) [
            (11, do  -- Case: enemy_health < 11 (i.e., <= 10)
                jsr $ AbsLabel "enemy_critical_animation"),
            (51, do  -- Case: enemy_health < 51 (i.e., <= 50)
                 -- Important: This case comes *after* the <= 10 case.
                 -- If health is <= 10, it will match the first case and jump out.
                 -- This block only runs if health is 11 <= health <= 50.
                jsr $ AbsLabel "enemy_wounded_animation")

        ]
    l_ endCase


-- Example: Check player position (16-bit x and y coordinates)
checkSpecialPositions :: Asm ()
checkSpecialPositions = do
    caseOfMultiBytes [(AddrLabel "player_x_low", 1), (AddrLabel "player_x_high", 1), (AddrLabel "player_y_low", 1), (AddrLabel "player_y_high", 1)] [
        -- Position (100, 150) = ($64, $00, $96, $00) assuming little-endian
        ([(0x64, 1), (0x00, 1), (0x96, 1), (0x00, 1)], do
            jsr $ AbsLabel "trigger_secret_passage"),

        -- Position (200, 50) = ($C8, $00, $32, $00)
        ([(0xC8, 1), (0x00, 1), (0x32, 1), (0x00, 1)], do
            jsr $ AbsLabel "spawn_treasure_chest"),

        -- Position (150, 150) = ($96, $00, $96, $00)
        ([(0x96, 1), (0x00, 1), (0x96, 1), (0x00, 1)], do
            jsr $ AbsLabel "activate_teleporter")
        ]
