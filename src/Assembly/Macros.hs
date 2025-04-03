{-# LANGUAGE LambdaCase #-}

module Assembly.Macros (
    -- Macros
    ifNotEqThen,
    ifEqThen,
    ifCarryThen,
    ifNotCarryThen,
    whileEqDo,
    whileNotEqDo,
    whileCarryDo,
    whileNotCarryDo,
    -- Helper (potentially hide later)
    makeUniqueLabel,
    caseOf,
    caseOfA,
    caseOfX,
    caseOfY,
    caseOfAddr,
    caseOfZP,

) where

import Data.Word (Word8)
--import Control.Monad.State.Strict (MonadState, gets, modify')
-- Fallback to wildcard import to try and resolve persistent AbsLabel import error
import Assembly.Core -- Wildcard import of Assembly.Core
-- import Assembly.Core ( Asm Label AddressRef(AddrLabel) Operand(OpAbs) AbsLabel AsmState(asmMacroCounter) )
-- import Assembly.Core ( l_, beq, bne, bcc, bcs, jmp, makeUniqueLabel ) -- Import specific functions needed from Core

-- makeUniqueLabel is now imported from Assembly.Core

-- --- Macros ---
ifNotEqThen :: Asm () -> Asm ()
ifNotEqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    beq endLabel
    thenBlock
    l_ endLabel

ifEqThen :: Asm () -> Asm ()
ifEqThen thenBlock = do
    endLabel <- makeUniqueLabel ()
    bne endLabel -- Corrected: branch if NOT equal to skip
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

whileEqDo :: Asm () -> Asm () -> Asm ()
whileEqDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bne endLabel -- Branch if Not Equal (exit condition)
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileNotEqDo :: Asm () -> Asm () -> Asm ()
whileNotEqDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    beq endLabel -- Branch if Equal (exit condition)
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileCarryDo :: Asm () -> Asm () -> Asm ()
whileCarryDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcc endLabel -- Branch if Carry Clear (exit condition)
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel

whileNotCarryDo :: Asm () -> Asm () -> Asm ()
whileNotCarryDo conditionBlock doBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    conditionBlock
    bcs endLabel -- Branch if Carry Set (exit condition)
    doBlock
    jmp $ AbsLabel startLabel
    l_ endLabel


-- -- General case macro that takes a loading function and a list of (value, action) pairs
-- caseOf :: (Word8 -> Asm ()) -> [(Word8, Asm ())] -> Asm ()
-- caseOf loadValue cases = do
--     endLabel <- makeUniqueLabel ()
--     casesWithLabels <- mapM (\(val, action) -> do
--         caseLabel <- makeUniqueLabel ()
--         return (val, caseLabel, action)
--       ) cases
    
--     -- Generate code for each case
--     mapM_ (\(val, caseLabel, action) -> do
--         loadValue val    -- Load the case value to compare against
--         cmp $ Imm val    -- Compare with the current value
--         beq caseLabel    -- Branch if equal to this case's label
--       ) casesWithLabels
    
--     -- Jump to end if no case matches
--     jmp $ AbsLabel endLabel
    
--     -- Generate the case blocks
--     mapM_ (\(_, caseLabel, action) -> do
--         l_ caseLabel
--         action
--         jmp $ AbsLabel endLabel
--       ) casesWithLabels
    
--     -- End label
--     l_ endLabel

-- -- Specialized case macro for the A register (most common use case)
-- caseOfA :: [(Word8, Asm ())] -> Asm ()
-- caseOfA = caseOf (\_ -> return ()) -- No need to load anything, A already has the value


-- General case macro that takes a comparison function and a list of (value, action) pairs
caseOf :: (Word8 -> Asm ()) -> [(Word8, Asm ())] -> Asm ()
caseOf compareWith cases = do
    endLabel <- makeUniqueLabel ()
    casesWithLabels <- mapM (\(val, action) -> do
        caseLabel <- makeUniqueLabel ()
        return (val, caseLabel, action)
      ) cases
    
    -- Generate code for each case
    mapM_ (\(val, caseLabel, action) -> do
        compareWith val   -- Run the comparison function with this value
        beq caseLabel     -- Branch if equal to this case's label
      ) casesWithLabels
    
    -- Jump to end if no case matches
    jmp $ AbsLabel endLabel
    
    -- Generate the case blocks
    mapM_ (\(_, caseLabel, action) -> do
        l_ caseLabel
        action
        jmp $ AbsLabel endLabel
      ) casesWithLabels
    
    -- End label
    l_ endLabel

-- Case for A register (most common use case)
caseOfA :: [(Word8, Asm ())] -> Asm ()
caseOfA = caseOf (cmp . Imm)

-- Case for X register
caseOfX :: [(Word8, Asm ())] -> Asm ()
caseOfX = caseOf (cpx . Imm)

-- Case for Y register
caseOfY :: [(Word8, Asm ())] -> Asm ()
caseOfY = caseOf (cpy . Imm)

-- Case for comparing a memory address
caseOfAddr :: AddressRef -> [(Word8, Asm ())] -> Asm ()
caseOfAddr addr = caseOf (\val -> do
    lda $ OpAbs addr
    cmp $ Imm val)

-- Case for comparing a zero page address
caseOfZP :: AddressRef -> [(Word8, Asm ())] -> Asm ()
caseOfZP addr = caseOf (\val -> do
    lda $ OpZP addr
    cmp $ Imm val)

-- Case for comparing multiple bytes (e.g., for 16-bit values)
-- addrBytes: A list of tuples where each tuple contains:
--  * AddressRef - The memory address where a byte is stored.
--  * Word8 - Number used to indicate how many bytes are compared. The number of elements in this list determines how many bytes are compared.
caseOfMultiBytes :: [(AddressRef, Word8)] -> [([(Word8, Word8)], Asm ())] -> Asm ()
caseOfMultiBytes addrBytes cases = do
    endLabel <- makeUniqueLabel ()
    casesWithLabels <- mapM (\(vals, action) -> do
        caseLabel <- makeUniqueLabel ()
        matchLabel <- makeUniqueLabel ()
        noMatchLabel <- makeUniqueLabel ()
        return (vals, caseLabel, matchLabel, noMatchLabel, action)
      ) cases
    
    -- Generate comparison code for each case
    mapM_ (\(vals, caseLabel, matchLabel, noMatchLabel, _) -> do
        l_ matchLabel
        -- Compare each byte in sequence
        let addrVals = zip addrBytes vals
        sequence_ $ zipWith (\(addr, expectedVal) (val, _) -> do
            lda $ OpAbs addr
            cmp $ Imm val
            bne noMatchLabel
          ) addrBytes vals
        
        -- If we get here, all bytes matched
        jmp $ AbsLabel caseLabel
        
        -- No match, try next case
        l_ noMatchLabel
      ) casesWithLabels
    
    -- Jump to end if no case matches
    jmp $ AbsLabel endLabel
    
    -- Generate the case blocks
    mapM_ (\(_, caseLabel, _, _, action) -> do
        l_ caseLabel
        action
        jmp $ AbsLabel endLabel
      ) casesWithLabels
    
    -- End label
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

