{-# LANGUAGE PatternSynonyms #-}
module Assembly.Macros (
    -- Macros
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
    
    --whileAddr, doWhileAddr, whileZP, doWhileZP,
    forEachRange, 
    caseOf, caseOfA, caseOfX, caseOfY, caseOfAddr, caseOfZP, caseOfMultiBytes,
    addAto16bit, -- Add 8-bit value in A to a 16-bit memory location
    -- Helper (potentially hide later)
    makeUniqueLabel,
    waitRaster,
    vicWaitLine,
    cmp_r, cmp_y, cmp_x, 
    adc_rb, add_rb, sub_rb, sub_br,
    sta_rb, sta_rw, 
    adc_rrw, add_rrw, sta_rrw, 
    copyBlock,
    fillScreen,
    decsum, binsum,
    configureVectors,
    printChar,
    printByte,
    skipNext2B,
    fillMemory,
    macrosLib
) where

--import Assembly.List (forEach, forEachListWithIndex, mapListInPlace, mapListToNew, filterList, foldList, filterMoreThan, sumList)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word8, Word16)
import Data.Bits((.&.), (.|.), shiftL, shiftR, testBit, complement, xor, (.<<.), (.>>.))
import Assembly.Core
    ( l_,
      makeUniqueLabel,
      makeLabelWithPrefix,
      zpLit,
      addr2word16,
      lsb,
      msb,
      Label,
      Address,
      AddressRef(AddrLabel, AddrLit16, AddrLit8, AddrLabelExpr),
      Asm,
      Operand(..), -- Import all constructors
      pattern Imm, -- Import the pattern synonym
      pattern AbsLabel,
      pattern ZPAddr,
      pattern ImmLsbLabel, -- Import the new pattern synonym
      pattern ImmMsbLabel, -- Import the new pattern synonym
      (.+), -- Import renamed operator
      (.-), -- Import renamed operator
      parens, -- Import parens if needed by macros (though unlikely)
      ArithExpr(add, sub), -- Import the class methods if needed directly (unlikely)
      evalLabelExpr,
      resolveAddressMaybe,
      LabelExpression(LabelRef),
      pattern IsNonZero,
      pattern IsCarry,
      Conditions(..), -- Ensure Conditions type is imported if needed by macros like while_
      db
      )
import Assembly.EDSLInstr 
import Prelude hiding((+), (-), and, or) -- Keep hiding Prelude's + and - if P.(+) is used elsewhere
import qualified Prelude as P ((+), (-))
import C64 (vicRaster, screenRam, colorRam, vicControl1)


-- zmienne lokalne na stronie zerowej 
-- te adresy mogą się pokrywać z innmi zmiennymi lokalnymi
srcTemp = AddrLit16 0x12 -- 2
dstTemp = AddrLit16 0x14 -- 2



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
    beq endLabel
    doBlock
    conditionBlock
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



-- Macro for 16-bit addition: Adds the 8-bit value in A to the 16-bit value at accAddr (low byte) and accAddr .+ 1 (high byte)
-- Uses ZP $00 as temporary storage for A. WARNING: Ensure ZP $00 is safe to use.
addAto16bit :: AddressRef -> Asm ()
addAto16bit accAddr = do
    let tempOperandAddr = AddrLit8 0x00 -- Use ZP $00 for temporary storage
    -- Store A (operand) temporarily
    sta tempOperandAddr          -- Store A (operand) temporarily
    -- Add low byte
    lda accAddr          -- Load low byte of accumulator
    clc                          -- Clear carry
    adc tempOperandAddr          -- Add operand from temp storage
    sta accAddr          -- Store result in low byte
    -- Add high byte with carry
    lda (accAddr .+ 1)    -- Load high byte of accumulator (using .+ operator)
    adc# 0                  -- Add 0 + carry
    sta (accAddr .+ 1)    -- Store result in high byte (using .+ operator)

-- --- Iteracje (bez zmian) ---
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

        -- Position (150, 150) = ($96, $00, $96, $00)
        ([(0x96, 1), (0x00, 1), (0x96, 1), (0x00, 1)], do
            jsr ("activate_teleporter"::Label))
        ]



{-# HLINT ignore "Use camelCase" #-}
cmp_r :: AddressRef -> Word8 -> Asm()
cmp_r address value = do
    lda# value
    cmp address

and_r :: AddressRef -> Word8 -> Asm()
and_r address value = do
    lda# value
    and address


cmp_y :: Word8  -> Asm()
cmp_y value = do
    tya
    cmp# value

cmp_x :: Word8  -> Asm()
cmp_x value = do
    txa
    cmp# value

sta_rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
sta_rb op value = do
    lda# value
    sta op

-- adding value to op with carry (op = op + value + carry)
adc_rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
adc_rb op value = do
    lda# value
    adc op
    sta op

-- adding value to op (op = op + value)
add_rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
add_rb op value = do
    lda# value
    clc
    adc op
    sta op


-- adding value to op (op = op + value)
and_rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
and_rb op value = do
    lda# value
    and op
    sta op

cmp_rz :: AddressRef -> AddressRef -> Asm()
cmp_rz op1 op2 = do
    lda op1
    cmp op2

-- substract op from value! (A = value - op)
sub_br:: Word8 -> AddressRef -> Asm() -- op -= value :: Word8
sub_br value op = do
    lda# value
    sec
    sbc op

-- substract value from op (op = op - value)
sub_rb :: AddressRef -> Word8 -> Asm() -- op <- value :: Word8
sub_rb op value = do
    lda op
    sec
    sbc# value
    sta op

sta_rw :: AddressRef -> Word16 -> Asm() -- op <- value :: Word16
sta_rw op value = do
    -- let nextAddr = case op of 
    --         AddrLit8 addr -> AddrLit8 $ addr P.+ 1 -- Increment the address by 1 op P.+ 1
    --         AddrLit16 addr -> AddrLit16 $ addr P.+ 1 -- Increment the address by 1
    --         _ -> error "Unsupported address type"   

    lda# lsb value -- Lower byte
    sta op
    lda# msb value-- Upper byte
    sta (op .+ 1) -- Store upper byte in next address    

adc_rrw :: AddressRef -> AddressRef -> Asm() -- op1 <- op1 + op2 + carry
adc_rrw op1 op2 = do
    lda op1
    adc op2
    sta op1
    lda (op1 .+ 1)
    adc (op2 .+ 1)
    sta (op1 .+ 1)

add_rrw :: AddressRef -> AddressRef -> Asm() -- op1 <- op1 + op2
add_rrw op1 op2 = do
    clc
    lda op1
    adc op2
    sta op1
    lda (op1 .+ 1)
    adc (op2 .+ 1)
    sta (op1 .+ 1)

-- sta_zw :: AddressRef -> Word16 -> Asm() -- op1 <- op2
-- sta_zw op1 value = do
--     lda# lsb value
--     sta op1
--     lda# msb value
--     sta (op1 .+ 1)

-- add_zrw :: AddressRef -> AddressRef -> Asm() -- op1 <- op1 + op2
-- add_zrw op1 op2 = do
--     lda op1
--     clc
--     adc op2
--     sta op1
--     lda (op1 .+ 1)
--     adc (op2 .+ 1)
--     sta (op1 .+ 1)

sta_rrw :: AddressRef -> AddressRef -> Asm() -- op1 <- op2
sta_rrw op1 op2 = do
    lda op2
    sta op1
    lda (op2 .+ 1)
    sta (op1 .+ 1)

-- add_zzw :: AddressRef -> AddressRef -> Asm() -- op1 <- op1 + op2
-- add_zzw op1 op2 = do
--     lda op1
--     clc
--     adc op2
--     sta op1
--     lda (op1 .+ 1)
--     adc (op2 .+ 1)
--     sta (op1 .+ 1)

-- | Kopiuje blok pamięci używając pętli while_.
-- | Używa rejestrów A, X, Y.
-- | UWAGA: Kopiuje maksymalnie 256 bajtów (licznik 8-bitowy w X).
-- | Parametry:
-- |   dest: Adres docelowy
-- |   src: Adres źródłowy
-- |   count: Liczba bajtów do skopiowania
copyBlock :: AddressRef -- Cel
            -> AddressRef -- Źródło
            -> Word8 -- Liczba bajtów (max 256)
            -> Asm ()
copyBlock dest src count = do

    -- Inicjalizacja wskaźników (tak jak poprzednio)
    sta_rrw srcTemp src   -- srcTemp = src
    sta_rrw dstTemp dest  -- dstTemp = dest

    -- Inicjalizacja licznika (X) i indeksu (Y)
    lda# count
    tax         -- Przenieś liczbę bajtów do X. WAŻNE: TAX ustawia flagę Z!
                -- Jeśli count=0, Z=1. Jeśli count!=0, Z=0.
    ldy# 0      -- Inicjalizuj indeks Y

    -- Pętla WHILE: kontynuuj, dopóki X jest RÓŻNY od zera (Z=0)
    -- Makro while_ sprawdza warunek *przed* wykonaniem bloku 'do'
    -- Flaga Z jest ustawiona przez TAX przed pierwszym sprawdzeniem
    -- oraz przez DEX na końcu każdej iteracji dla następnego sprawdzenia
    while_ IsNonZero $ do
        -- Ciało pętli:
        lda $ IY srcTemp  -- Załaduj bajt ze źródła
        sta $ IY dstTemp   -- Zapisz bajt do celu
        iny                           -- Zwiększ indeks

        -- Zmniejsz licznik X - to ustawi flagę Z dla sprawdzenia
        -- na początku *następnej* iteracji przez makro while_
        dex

    -- Koniec pętli (gdy warunek ResultIsNonZero przestanie być prawdziwy,
    -- czyli gdy DEX ustawi flagę Z (X=0))
    -- Nie jest potrzebna etykieta końca, makro while_ zarządza skokami


-- waitRaster :: Asm ()
-- waitRaster = do 
--     while_ IsNonZero $ do
--         cmp_r vicRaster 250  
vicWaitBottom :: Asm ()
vicWaitBottom = do
    while_ IsZero $ do
        and_r vicControl1 0x80


vicWaitTop :: Asm ()
vicWaitTop = do
    while_ IsNonZero $ do
        and_r vicControl1 0x80


waitRaster :: Asm()
waitRaster = do
    -- vicWaitTop
    vicWaitBottom

-- On a PAL C64 (common in Europe/Australia), the raster beam counts from line 0 up to line 311.
-- On an NTSC C64 (common in North America/Japan), it counts from line 0 up to line 262.
vicWaitLine :: Word16 -> Asm()
vicWaitLine line = do

    let lineLsb = lsb line
    let msbBitFlag = lsb ((line `shiftR` 1) .&. 0x80) -- "compile time" precalculate Word8 msb bit 9 of line (Word16)
    doWhile_ IsNonZero $ do         -- Repeat the whole process if the 9th bit check fails.
        doWhile_ IsNonZero $ do
            cmp_r vicRaster lineLsb -- Wait until VIC raster register ($D012) matches the target line.
                                    -- $D012 == line (Z=1 from last CMP). Now check the 9th bit (bit 7 of $D011)
        lda vicControl1             -- Load VIC control register 1 ($D011)
        and# 0x80                   -- Isolate bit 7. Sets Z flag (Z=1 if bit 7 is 0).
        cmp# msbBitFlag                -- The outer doWhile_ IsNonZero will loop if Z=0 (bit 7 is 1).


fillScreen :: AddressRef  -> Word8 -> Asm ()
fillScreen screenAddr fillB = do
    lda# fillB
    ldx# 250
    while_ IsNonZero $ do
        dex
        sta $ X screenAddr
        sta $ X (screenAddr .+ 250)
        sta $ X (screenAddr .+ 500)
        sta $ X (screenAddr .+ 750)


decsum = do  
        sed                              -- decimal mode
        clc                              -- clear carry
        doWhile_ IsNonZero $ do          -- while Y != 0
            dey                          -- Y--
            lda $ IY (AddrLit8 0x40) -- Get 2 decimal digits from string 1
            adc $ IY (AddrLit8 0x42) -- Add pair of digits from string 2
            sta $ IY (AddrLit8 0x40) -- Store result in string 1
            tya                          -- Y -> A (set Zero flag if Y=0)
        cld                              -- Back to binary arithmetic mode
        rts


-- Multidigit binary addition (bcd)
-- Address of number 1 at 0040:0041. 
-- Address of number 2 at 0042:0043. 
-- Length of numbers (in bytes) in Index Register Y. Numbers arranged starting with most significant digits.
-- Reslult: Sum replaces number with starting address in memory locations 0040 and 0041.
binsum :: Asm ()
binsum = do  
        clc                              -- clear carry
        doWhile_ IsNonZero $ do          -- while Y != 0
            dey                          -- Y--
            lda $ IY (AddrLit8 0x40) -- Get 2 decimal digits from string 1
            adc $ IY (AddrLit8 0x42) -- Add pair of digits from string 2
            sta $ IY (AddrLit8 0x40) -- Store result in string 1
            tya                          -- Y -> A (set Zero flag if Y=0)
        rts

-- Updated configureVectors to accept AddressRef and use symbolic LSB/MSB loading
configureVectors :: AddressRef -> Asm ()
configureVectors addrRef = do
    -- Extract the label name. Raise error if it's not a label reference.
    let labelName = case addrRef of
            AddrLabel l -> l
            AddrLabelExpr (LabelRef l) -> l
            _ -> error "configureVectors requires a label reference (AddrLabel or AddrLabelExpr (LabelRef ...))"

    -- IRQ Vector -> Use symbolic LSB/MSB operands
    -- lda $ ImmLsbLabel labelName
    -- sta $ AddrLit16 0x0314
    -- lda $ ImmMsbLabel labelName
    -- sta $ AddrLit16 0x0315
    -- let addr = resolveAddressMaybe $ Just addrRef
    --NMI Vector -> Use symbolic LSB/MSB operands
    -- $0318/$0319 is the RAM NMI vector, mirroring $FFFA/$FFFB.
    -- lda#  lsb (resolveAddressMaybe labelName)
    lda #< labelName
    sta $ AddrLit16 0x0318
    lda #> labelName
    sta $ AddrLit16 0x0319

    -- lda# 0xFF
    -- sta$ AddrLit16 0xFFFE
    -- sta$ AddrLit16 0xFFFF

    -- BRK Vector -> Use symbolic LSB/MSB operands
    -- lda $ ImmLsbLabel labelName
    -- sta $ AddrLit16 0x0316
    -- lda $ ImmMsbLabel labelName
    -- sta $ AddrLit16 0x0317

printChar :: Word16 -> Word8 -> Asm()
printChar textPos color = do
    
    sta $ X (screenRam .+ textPos)    -- Store the character at the screen memory location
    lda# color
    sta $ X (colorRam .+ textPos)    -- Store the color at the screen color memory location

printColorChar :: Word16 -> Address -> Asm()
printColorChar textPos colorMap = do
    sta$ X (screenRam .+ textPos)
    tay 
    lda (Y colorMap)
    sta $ X (colorRam .+ textPos)



macrosLib = do
    l_ "hundreds2Petscii"
    hundreds2Petscii
    l_ "tens2Petscii"
    tens2Petscii

-- returns petscii character of hundreds of value from accumulator
-- returns rest of value in 0xf8
hundreds2Petscii = do 

    let count = AddrLit8 0xf7
    let rest = AddrLit8 0xf8

    -- sta rest
    sta_rb count 0
    lda rest
    sec
    sbc# 100
    while_ IsCarry $ do
        inc count
        sbc# 100
    adc# 100 -- undo last dec
    sta rest
    lda count 
    clc
    adc# 0x30        -- Convert the count to ASCII     
    rts

-- return petscii character of tens of accumulator, (max value of accumulator have to be < 99)
-- return rest of value in 0xf8

tens2Petscii = do 

    let count = AddrLit8 0xf7
    let rest = AddrLit8 0xf8
    
    -- sta rest
    sta_rb count 0
    lda rest
    sec
    sbc# 10
    while_ IsCarry $ do
        inc count
        sbc# 10
    adc# 10 -- undo last dec
    sta rest
    lda count 
    clc
    adc# 0x30        -- Convert the count to ASCII     
    rts



--- prints decimal byte at x y coords
--- (needs *macrosLib*)
printByte :: Word16 -> Word16 -> Address -> Asm()
printByte x y color = do

    let rest = AddrLit8 0xf8
    let screenAddress = y * 0x40 P.+ x
    jsr "hundreds2Petscii"

    ldx# 0
    printColorChar screenAddress color  --Print a character from the hellotext string at the specified position
    jsr "tens2Petscii"
    ldx# 1
    printColorChar screenAddress color  --Print a character from the hellotext string at the specified position

    lda rest 
    clc
    adc# 0x30
    ldx# 2
    printColorChar screenAddress color  --Print a character from the hellotext string at the specified position
    rts

skipNext2B = do
    db [0x2C] -- Instruction BIT $address (direct addr. mode)

fillMemory:: AddressRef -> Word8 -> Int -> Asm ()
fillMemory memAddr value sizeKb = do

    sfmd <- makeLabelWithPrefix "sfmd"
    ldx# fromIntegral (4 * sizeKb)  -- 256 * 4 bytes
    ldy# 0 -- 0..255 counter
    cpx# 0
    while_ IsNonZero $ do
        doWhile_ IsNonZero $ do
            lda# value
            l_  sfmd
            sta$ Y memAddr -- MSB of *memAddr* will be incremented when Y==0, LSB of *memAddr* is 0! (is replaced by Y index)
            iny
        inc$ AddrLabel sfmd .+ 2  -- this will increment *STA* instruction second byte operand (at "sfmd")
        dex
    rts


