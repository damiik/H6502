module Assembly.List (
    -- List operations
    iterateWithIndexList,
    mapInPlaceList,
    mapToNewList,
    filterList,
    foldList,
    filterMoreThanList,
    sumList,

    -- List helpers
    createList,
    createList_,
    addToList,
    copyList,
    createListFromString,
    iterateList
) where

import Data.Word (Word8, Word16)
import Data.Char (ord)
import qualified Data.Foldable as String
import Assembly.Core hiding (LabelAdd, lda, sta, ldx, ldy, jmp, inx, adc, sbc, tax, tay, stx, sty, cmp, cpx, cpy, txa, tya, txs, tsx, bne, beq, bcs, bcc, bmi, bpl, bvs, bvc, jsr, rts, ora, asl, php, clc, and, bit, rol, ror, plp, sec, rti, eor, lsr, pha, cli, pla, sei, dey, clv, iny, dex, cld, nop, sed, inc, dec, brk) -- Hide Core's LabelAdd and remove aliases
import qualified Assembly.Core as C (LabelExpression(LabelAdd), (.+)) -- Import renamed operator (.+)
import Assembly.EDSLInstr 
import Assembly.Macros (addAto16bit) -- Import the updated macro
import Prelude

-- List creation
createList_ :: AddressRef -> Asm ()
createList_ addr = do
    lda# 0x00
    sta addr


createList :: String -> Asm AddressRef
createList s = do
    let sAddr = AddrLabel s
    createList_ sAddr
    return sAddr


copyList :: AddressRef -> AddressRef -> Asm ()
copyList src dst = do
    -- let srcAbs = OpAbs src
    -- let dstAbs = OpAbs dst
    lda# 0  -- Inicjalizuj długość listy docelowej
    sta dst
    -- Pętla kopiująca
    ldx# 0
    copyLoopLabel <- makeUniqueLabel ()
    endCopyLabel  <- makeUniqueLabel ()
    l_ copyLoopLabel
    -- lda srcAbs          -- Pobierz długość źródła
    -- cmp $ OpZPX (AddrLit16 0) -- Porównaj z X (używamy ZPX jako hack do porównania z X, wymaga to aby $00 nie był używany lub można użyć CPX)
    cpx src
    beq endCopyLabel    -- Jeśli X == długość źródła, koniec
    inx                 -- Zwiększ X (nowa potencjalna długość)
    lda $ X src         -- Załaduj bajt ze źródła[X]
    sta $ X dst         -- Zapisz bajt w docelowym[X]
    txa                 -- Przenieś X do A
    sta dst          -- Zapisz nową długość w docelowym
    jmp copyLoopLabel
    l_ endCopyLabel


createListFromString :: String -> Asm ()
createListFromString s = do
    let bytes = map (fromIntegral . ord) s
    db [fromIntegral $ String.length s] -- Length
    db bytes                         -- Bytes

-- List operations
addToList :: AddressRef -> Word8 -> Asm ()
addToList l element = do
  let listBaseAbs = OpAbs l
  let listBaseAbsX = OpAbsX l
  lda l --listBaseAbs
  tax
  inx
  stx l --listBaseAbs
  lda# element
  sta $ X l --listBaseAbsX


iterateList :: AddressRef -> (Operand -> Asm ()) -> Asm ()
iterateList l action = do
    let listLenAddr = OpAbs l
    ldx# 0x00         -- Start index at 0
    loopLabel <- makeUniqueLabel () -- Uses makeUniqueLabel from Core (via Macros import)
    endLabel  <- makeUniqueLabel ()
    l_ loopLabel             -- Uses l_ from Core
    cpx l --listLenAddr          -- Uses cpx alias defined above
    beq endLabel             -- Uses beq from Core, jumps to endLabel if X == length, for empty list X == length == 0!
    inx                      -- X indexed from 1!
    action $ OpAbsX l        -- Perform action with the *address* of the element
    jmp loopLabel -- Uses jmp alias defined above
    l_ endLabel


-- List iteration with index
iterateWithIndexList :: AddressRef -> (Operand -> Word8 -> Asm ()) -> Asm ()
iterateWithIndexList listBase action = do
    ldx# 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx listBase
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    action (OpAbsX listBase) (fromIntegral $ fromEnum 'X') -- Passing 'X' doesn't make sense here, should pass index
    jmp startLabel
    l_ endLabel

-- Map to new list
mapToNewList :: AddressRef -> AddressRef -> (Operand -> Asm ()) -> Asm ()
mapToNewList srcList dstList transform = do
    lda# 0x00
    sta dstList
    ldx# 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx srcList
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    lda $ X srcList
    transform (OpAbsX srcList) -- transform should modify A
    sta $ X dstList
    txa -- Update length
    sta dstList
    jmp startLabel
    l_ endLabel

-- In-place list transformation
mapInPlaceList :: AddressRef -> (Operand -> Asm ()) -> Asm ()
mapInPlaceList listBase transform = do
    ldx# 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx listBase
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    lda $ X listBase
    transform (OpAbsX listBase) -- transform should modify A
    sta $ X listBase
    jmp startLabel
    l_ endLabel


-- Filter list elements
filterList :: AddressRef -> AddressRef -> (Operand -> Label -> Asm ()) -> Asm ()
filterList srcList dstList predicate = do
    lda# 0x00
    sta dstList -- Initialize destination length
    ldx# 0x00      -- Source index
    ldy# 0x00      -- Destination index
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    skipLabel <- makeLabelWithPrefix "skip_filterList"
    l_ startLabel
    cpx srcList
    beq endLabel
    inx  -- Increment source index (1-based access)
    lda $ X srcList -- Load source element
    -- Predicate should branch to skipLabel if element should NOT be included
    predicate (OpAbsX srcList) skipLabel
    -- Element should be included:
    iny                 -- Increment destination index
    sta $ Y dstList -- Store element at destination[Y]
    tya                 -- Get new destination length (Y)
    sta dstList -- Update destination length byte
    l_ skipLabel        -- Label to jump to if element is skipped
    jmp startLabel
    l_ endLabel

-- Filter elements greater than value
filterMoreThanList :: AddressRef -> AddressRef -> Word8 -> Asm ()
filterMoreThanList l1 l2 v = do
    filterList l1 l2 $ \_ skipLabel -> do -- The first argument (operand) is implicitly A here
        cmp# v
        -- If A < v (carry clear) or A == v (zero set), skip.
        bcc skipLabel
        beq skipLabel
        -- If A > v (carry set, zero clear), continue (don't jump to skipLabel)

-- Sum list elements into a 16-bit result stored at resultAddr (low byte) and resultAddr.+1 (high byte)
sumList :: AddressRef -> AddressRef -> Asm ()
sumList listBase resultAddr = do

    -- Initialize 16-bit result to 0
    lda# 0
    sta resultAddr
    sta (resultAddr C..+ 1) -- Use qualified renamed operator C.(.+)

    -- Loop through the list
    ldx# 0x00         -- Start index at 0
    loopLabel <- makeUniqueLabel ()
    endLabel  <- makeUniqueLabel ()

    l_ loopLabel
    cpx listBase     -- Compare index with list length
    beq endLabel             -- Exit if index == length

    inx                      -- Increment index (accessing element X)
    lda $ X listBase    -- Load list element into A

    -- Add element (in A) to the 16-bit result using the single-parameter macro
    addAto16bit resultAddr

    jmp loopLabel -- Continue loop
    l_ endLabel

-- Fold/reduce list (8-bit accumulator)
foldList :: AddressRef -> Word8 -> (AddressRef -> Operand -> Asm ()) -> Asm ()
foldList listBase initialValue combine = do
    lda# initialValue
    -- Store initial accumulator value somewhere safe if needed, e.g., ZP
    let tempAcc = (AddrLit8 0x01) -- Example: Use ZP $01 for accumulator
    sta tempAcc

    ldx# 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx listBase
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    -- Combine current accumulator (from tempAcc) with list element (OpAbsX listBase)
    -- The result should typically end up back in A (or tempAcc)
    combine tempAcc (OpAbsX listBase)
    -- Assuming result is in A after combine, store it back
    sta tempAcc
    jmp startLabel
    l_ endLabel
    -- After loop, final result is in tempAcc, load it if needed
    lda tempAcc
