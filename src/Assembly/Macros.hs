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
      (+),
      addrVal
      )

import Prelude hiding((+))

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

-- Macro for 16-bit addition: Adds the 8-bit value in A to the 16-bit value at (accLowAddr, accHighAddr)
-- Requires caller to provide both addresses.
-- Uses ZP $00 as temporary storage for A. WARNING: Ensure ZP $00 is safe to use.
-- addAto16bit :: AddressRef -> AddressRef -> Asm ()
-- addAto16bit accLowAddr accHighAddr = do
--     let tempOperandAddr = zpLit 0x00 -- Use ZP $00 for temporary storage

--     sta tempOperandAddr          -- Store A (operand) temporarily

--     -- Add low byte
--     lda $ OpAbs accLowAddr       -- Load low byte of accumulator
--     clc                          -- Clear carry
--     adc tempOperandAddr          -- Add operand from temp storage
--     sta $ OpAbs accLowAddr       -- Store result in low byte

--     -- Add high byte with carry
--     lda $ OpAbs accHighAddr      -- Load high byte of accumulator
--     adc $ Imm 0                  -- Add 0 + carry
--     sta $ OpAbs accHighAddr      -- Store result in high byte

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
    lda $ OpAbs $ accAddr + 1    -- Load high byte of accumulator
    adc $ Imm 0                  -- Add 0 + carry
    sta $ OpAbs $ accAddr + 1    -- Store result in high byte

-- --- Iteracje (bez zmian) ---
forEachRange :: Word8 -> Word8 -> (Operand -> Asm ()) -> Asm ()
forEachRange start end action = do
    ldx $ Imm start
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ Imm end
    beq endLabel
    action (OpAbsX (AddrLit16 0))
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
    mapM_ (\(val, caseLabel, action) -> do
        compareWith val
        beq caseLabel
      ) casesWithLabels
    jmp $ AbsLabel endLabel
    mapM_ (\(_, caseLabel, action) -> do
        l_ caseLabel
        action
        jmp $ AbsLabel endLabel
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
    casesWithLabels <- mapM (\(vals, action) -> do
        caseLabel <- makeUniqueLabel ()
        matchLabel <- makeUniqueLabel ()
        noMatchLabel <- makeUniqueLabel ()
        return (vals, caseLabel, matchLabel, noMatchLabel, action)
      ) cases
    mapM_ (\(vals, caseLabel, matchLabel, noMatchLabel, _) -> do
        l_ matchLabel
        let addrVals = zip addrBytes vals
        sequence_ $ zipWith (\(addr, _) (val, _) -> do
            lda $ OpAbs addr
            cmp $ Imm val
            bne noMatchLabel
          ) addrBytes vals
        jmp $ AbsLabel caseLabel
        l_ noMatchLabel
      ) casesWithLabels
    jmp $ AbsLabel endLabel
    mapM_ (\(_, caseLabel, _, _, action) -> do
        l_ caseLabel
        action
        jmp $ AbsLabel endLabel
      ) casesWithLabels
    l_ endLabel
-- caseOfList

-- Example with custom comparison
customCaseExample :: Asm ()
customCaseExample = do
    notEqual <- makeUniqueLabel ()
    caseOf (\val -> do
        -- Custom comparison logic
        lda $ AbsLabel "enemy_health"
        cmp $ Imm val
        bcs notEqual  -- Branch if >= (carry set)
        lda $ Imm 0xFF              -- Force equality flag for <=
        cmp $ Imm 0xFF
        ) [
            (50, do  -- Case: enemy_health <= 50
                jsr $ AbsLabel "enemy_wounded_animation"),
            
            (10, do  -- Case: enemy_health <= 10
                jsr $ AbsLabel "enemy_critical_animation")
        ]
    l_ notEqual  -- Label for when no case matches

--Example: Check player position (16-bit x and y coordinates)
checkSpecialPositions :: Asm ()
checkSpecialPositions = do
    caseOfMultiBytes [(AddrLabel "player_x", 2), (AddrLabel "player_y", 2)] [
        ([(100, 0), (150, 0)], do  -- Position (100, 150)
            jsr $ AbsLabel "trigger_secret_passage"),
        
        ([(200, 0), (50, 0)], do   -- Position (200, 50)
            jsr $ AbsLabel "spawn_treasure_chest"),
        
        ([(150, 0), (150, 0)], do  -- Position (150, 150)
            jsr $ AbsLabel "activate_teleporter")
        ]
