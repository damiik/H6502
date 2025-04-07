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
import Assembly.Core hiding (LabelAdd) -- Hide Core's LabelAdd if needed
import qualified Assembly.Core as Core (LabelExpression(LabelAdd)) -- Use qualified import if needed
import Assembly.Macros (addAto16bit) -- Import the updated macro
import Prelude hiding((+))


-- List creation
createList_ :: AddressRef -> Asm ()
createList_ addr = do
    lda $ Imm 0x00
    sta $ OpAbs addr


createList :: String -> Asm AddressRef
createList s = do
    let sAddr = AddrLabel s
    createList_ sAddr
    return sAddr


copyList :: AddressRef -> AddressRef -> Asm ()
copyList src dst = do
    let srcAbs = OpAbs src
    let dstAbs = OpAbs dst
    lda $ Imm 0  -- Inicjalizuj długość listy docelowej
    sta dstAbs
    -- Pętla kopiująca
    ldx $ Imm 0
    copyLoopLabel <- makeUniqueLabel ()
    endCopyLabel  <- makeUniqueLabel ()
    l_ copyLoopLabel
    -- lda srcAbs          -- Pobierz długość źródła
    -- cmp $ OpZPX (AddrLit16 0) -- Porównaj z X (używamy ZPX jako hack do porównania z X, wymaga to aby $00 nie był używany lub można użyć CPX)
    cpx srcAbs
    beq endCopyLabel    -- Jeśli X == długość źródła, koniec
    inx                 -- Zwiększ X (nowa potencjalna długość)
    lda $ OpAbsX src         -- Załaduj bajt ze źródła[X]
    sta $ OpAbsX dst         -- Zapisz bajt w docelowym[X]
    txa                 -- Przenieś X do A
    sta dstAbs          -- Zapisz nową długość w docelowym
    jmp $ AbsLabel copyLoopLabel
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
  lda listBaseAbs
  tax
  inx
  stx listBaseAbs
  lda $ Imm element
  sta listBaseAbsX


iterateList :: AddressRef -> (Operand -> Asm ()) -> Asm ()
iterateList l action = do
    let listLenAddr = OpAbs l
    ldx $ Imm 0x00         -- Start index at 0
    loopLabel <- makeUniqueLabel () -- Uses makeUniqueLabel from Core (via Macros import)
    endLabel  <- makeUniqueLabel ()
    l_ loopLabel             -- Uses l_ from Core
    cpx listLenAddr          -- Uses cpx alias defined above
    beq endLabel             -- Uses beq from Core, jumps to endLabel if X == length, for empty list X == length == 0!
    inx                      -- X indexed from 1!
    action $ OpAbsX l        -- Perform action with the *address* of the element
    jmp $ AbsLabel loopLabel -- Uses jmp alias defined above
    l_ endLabel


-- List iteration with index
iterateWithIndexList :: AddressRef -> (Operand -> Word8 -> Asm ()) -> Asm ()
iterateWithIndexList listBase action = do
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ OpAbs listBase
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    action (OpAbsX listBase) (fromIntegral $ fromEnum 'X')
    jmp $ AbsLabel startLabel
    l_ endLabel

-- Map to new list
mapToNewList :: AddressRef -> AddressRef -> (Operand -> Asm ()) -> Asm ()
mapToNewList srcList dstList transform = do
    lda $ Imm 0x00
    sta $ OpAbs dstList
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ OpAbs srcList
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    lda $ OpAbsX srcList
    transform (OpAbsX srcList)
    sta $ OpAbsX dstList
    txa
    sta $ OpAbs dstList
    jmp $ AbsLabel startLabel
    l_ endLabel

-- In-place list transformation
mapInPlaceList :: AddressRef -> (Operand -> Asm ()) -> Asm ()
mapInPlaceList listBase transform = do
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ OpAbs listBase
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    lda $ OpAbsX listBase
    transform (OpAbsX listBase)
    sta $ OpAbsX listBase
    jmp $ AbsLabel startLabel
    l_ endLabel


-- Filter list elements
filterList :: AddressRef -> AddressRef -> (Operand -> Label -> Asm ()) -> Asm ()
filterList srcList dstList predicate = do
    lda $ Imm 0x00
    sta $ OpAbs dstList
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    skipLabel <- makeLabelWithPrefix "skip_filterList"
    l_ startLabel
    cpx $ OpAbs srcList
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    lda $ OpAbsX srcList
    predicate (OpAbsX srcList) skipLabel
    ldy $ OpAbs dstList
    iny
    sta $ OpAbsY dstList
    tya
    sta $ OpAbs dstList
    l_ skipLabel
    jmp $ AbsLabel startLabel
    l_ endLabel

-- Filter elements greater than value
filterMoreThanList :: AddressRef -> AddressRef -> Word8 -> Asm ()
filterMoreThanList l1 l2 v = do
    filterList l1 l2 $ \e skipLabel -> do
        cmp $ Imm v
        bcc skipLabel
        beq skipLabel
        lda e

-- Sum list elements into a 16-bit result stored at resultAddr (low byte) and resultAddr+1 (high byte)
sumList :: AddressRef -> AddressRef -> Asm ()
sumList listBase resultAddr = do

    -- Initialize 16-bit result to 0
    lda $ Imm 0
    sta $ OpAbs resultAddr
    sta $ OpAbs $ resultAddr + 1 -- Store 0 in the high byte address

    -- Loop through the list
    ldx $ Imm 0x00         -- Start index at 0
    loopLabel <- makeUniqueLabel ()
    endLabel  <- makeUniqueLabel ()

    l_ loopLabel
    cpx $ OpAbs listBase     -- Compare index with list length
    beq endLabel             -- Exit if index == length

    inx                      -- Increment index (accessing element X)
    lda $ OpAbsX listBase    -- Load list element into A

    -- Add element (in A) to the 16-bit result using the single-parameter macro
    addAto16bit resultAddr

    jmp $ AbsLabel loopLabel -- Continue loop
    l_ endLabel

-- Fold/reduce list (8-bit accumulator)
foldList :: AddressRef -> Word8 -> (Word8 -> Operand -> Asm ()) -> Asm ()
foldList listBase initialValue combine = do
    lda $ Imm initialValue
    ldx $ Imm 0x00
    startLabel <- makeUniqueLabel ()
    endLabel <- makeUniqueLabel ()
    l_ startLabel
    cpx $ OpAbs listBase
    beq endLabel
    inx  -- increment X before first access (indexing from 1)
    combine (fromIntegral $ fromEnum 'A') (OpAbsX listBase)
    jmp $ AbsLabel startLabel
    l_ endLabel
