
module Assembly.Macros (
    -- Macros
    ifNotEqThen, ifEqThen, ifCarryThen, ifNotCarryThen,
    ifMinusThen, ifPlusThen, ifOverflowThen, ifNotOverflowThen,
    whileEqDo, whileNotEqDo, whileCarryDo, whileNotCarryDo,
    whileMinusDo, whilePlusDo, whileOverflowDo, whileNotOverflowDo,
    forEachRange, forEachListWithIndex,
    mapListInPlace, mapListToNew,
    filterList, foldList,
    filterMoreThan, sumList,
    caseOf, caseOfA, caseOfX, caseOfY, caseOfAddr, caseOfZP, caseOfMultiBytes,
    -- Helper (potentially hide later)
    makeUniqueLabel
) where

import Data.Word (Word8)
import Assembly.Core

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
    l_ startLabel
    conditionBlock
    beq endLabel
    doBlock
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

forEachListWithIndex :: AddressRef -> (Operand -> Word8 -> Asm ()) -> Asm ()
forEachListWithIndex listBase action = do
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ OpAbs listBase
    beq endLabel
    action (OpAbsX listBase) (fromIntegral $ fromEnum 'X')
    inx
    jmp $ AbsLabel startLabel
    l_ endLabel

-- --- Mapowanie (bez zmian) ---
mapListInPlace :: AddressRef -> (Operand -> Asm ()) -> Asm ()
mapListInPlace listBase transform = do
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ OpAbs listBase
    beq endLabel
    lda $ OpAbsX listBase
    transform (OpAbsX listBase)
    sta $ OpAbsX listBase
    inx
    jmp $ AbsLabel startLabel
    l_ endLabel

mapListToNew :: AddressRef -> AddressRef -> (Operand -> Asm ()) -> Asm ()
mapListToNew srcList dstList transform = do
    lda $ Imm 0x00
    sta $ OpAbs dstList
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ OpAbs srcList
    beq endLabel
    lda $ OpAbsX srcList
    transform (OpAbsX srcList)
    sta $ OpAbsX dstList
    inx
    txa
    sta $ OpAbs dstList
    jmp $ AbsLabel startLabel
    l_ endLabel

-- Przykład użycia filterList: wybierz wartości > 20 z myList1 do myList3
filterMoreThan :: AddressRef -> AddressRef -> Word8 -> Asm ()
filterMoreThan l1 l2 v = do
    filterList l1 l2 $ \e skipLabel -> do
        cmp $ Imm v
        bcc skipLabel -- Pomiń, jeśli mniejsze (carry clear)
        beq skipLabel -- Pomiń, jeśli równe (zero set)
        lda e                 -- Przywróć wartość do A, jeśli warunek spełniony

-- --- Nowe makra: Filter i Fold ---
filterList :: AddressRef -> AddressRef -> (Operand -> Label -> Asm ()) -> Asm ()
filterList srcList dstList predicate = do
    lda $ Imm 0x00
    sta $ OpAbs dstList -- Zainicjuj długość nowej listy
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    skipLabel <- makeLabelWithPrefix "skip_filterList"
    l_ startLabel
    cpx $ OpAbs srcList -- Porównaj X z długością źródłowej listy
    beq endLabel
    inx  -- inkrementuj X, aby przejść do następnego lub pierwszego elementu (indexowanie od 1!)
    lda $ OpAbsX srcList -- Załaduj element
    predicate (OpAbsX srcList) skipLabel -- Wykonaj predykat z etykietą pomijania
    ldy $ OpAbs dstList -- Załaduj bieżącą długość docelowej listy do Y
    iny -- Zwiększ Y, aby uzyskać nowy indeks następnego lub pierwszego elementu (indexowanie od 1!)
    sta $ OpAbsY dstList -- Zapisz element w nowej liście (używając Y jako indeksu)
    tya                 --    Y -> A
    sta $ OpAbs dstList -- Zaktualizuj długość nowej listy
    l_ skipLabel
    jmp $ AbsLabel startLabel
    l_ endLabel


-- Przykład użycia foldList: oblicz sumę elementów w myList2
sumList :: AddressRef -> AddressRef -> Asm ()
sumList myList2 result = do
    foldList myList2 0 $ \acc elem -> do
        clc
        adc elem -- Dodaj element do akumulatora
    sta $ AbsAddress result -- Zapisz wynik    

foldList :: AddressRef -> Word8 -> (Word8 -> Operand -> Asm ()) -> Asm ()
foldList listBase initialValue combine = do
    lda $ Imm initialValue -- Zainicjuj akumulator wartością początkową
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ OpAbs listBase -- Porównaj X z długością listy
    beq endLabel
    combine (fromIntegral $ fromEnum 'A') (OpAbsX listBase) -- Połącz bieżący akumulator z elementem
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

