{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Assembly.EDSLInstr (

    -- OpGenerator Class
    OpGenerator(toOperand),
    -- A(..),
    X(..),
    Y(..),

    I(),
    IX(..),
    IY(..),
    IM(..),
    (#),
    (#>),
    (#<),

    -- eDSL Instruction Aliases
    lda, sta, ldx, ldy, jmp, inx, adc, sbc, tax, tay, stx, sty,
    cmp, cpx, cpy, txa, tya, txs, tsx, bne, beq, bcs, bcc, bmi,
    bpl, bvs, bvc, jsr, rts, ora, asl, php, clc, and, bit, rol,
    ror, plp, sec, rti, eor, lsr, pha, cli, pla, sei, dey, clv,
    iny, dex, cld, nop, sed, inc, dec, brk,

    -- eDSL Control Flow Macros
    if_,
    while_,
    doWhile_,
) where

import Data.Word (Word8, Word16)
import Data.String
import Prelude hiding (and) -- Hide 'and' from Prelude
import Assembly.Core (Asm, Operand(..), pattern A_, Label, emitIns, emitImplied, emitAccumulator, emitBranch, Conditions, branchOnCondition, makeUniqueLabel, invert,l_, pattern AbsLabel, pattern AddrLabel, pattern AddrLit8, pattern AddrLit16, AddressRef(..), pattern LsbImm, pattern MsbImm, Label, LabelExpression)
import Assembly.Instructions6502 (Mnemonic(..))
import Assembly.Branch (BranchMnemonic(..)) -- Import BranchMnemonic constructors

-- OpGenerator Class and Instances
class OpGenerator a where
    toOperand :: a -> Operand



-- OpGenerator instances for Word8 and Word16 absoulte addressing mode
-- -------------------------------------------------------------------
instance OpGenerator Word8 where
    toOperand v = OpZP (AddrLit8 v) 

instance OpGenerator Word16 where
    toOperand v = OpAbs (AddrLit16 v)

instance OpGenerator String where
    toOperand l = OpAbs (AddrLabel l)

--TODO: missing AddrLabelExpr with zero page.
instance OpGenerator AddressRef where
    toOperand addr =  case addr of
        AddrLit8 v -> OpZP (AddrLit8 v)       -- Immediate addressing
        AddrLit16 v -> OpAbs (AddrLit16 v)       -- Absolute addressing
        AddrLabel l -> OpAbs (AddrLabel l) -- Absolute addressing with label
        AddrLabelExpr expr -> OpAbs (AddrLabelExpr expr) -- Absolute addressing with label expression


-- Define newtype wrappers for LSB/MSB immediate mode
-- -------------------------------------------------------
newtype LsbLabel a = LsbLabel a
newtype MsbLabel a = MsbLabel a

-- Add OpGenerator instances for these wrappers targeting Labels
instance OpGenerator (LsbLabel Label) where
    toOperand (LsbLabel l) = OpLsbImm l -- Use the new constructor (or pattern LsbImm l)

instance OpGenerator (MsbLabel Label) where
    toOperand (MsbLabel l) = OpMsbImm l -- Use the new constructor (or pattern MsbImm l)

infixl 6 #>
(#>):: (MsbLabel Label -> Asm ()) -> Label -> Asm ()
ins #> val  = ins (MsbLabel val)
infixl 6 #<
(#<):: (LsbLabel Label -> Asm ()) -> Label -> Asm ()
ins #< val  = ins (LsbLabel val)


-- Define newtype wrappers for immediate intent
-- -----------------------------------------------------------------------------
newtype IM a = IM a
instance OpGenerator (IM Word8) where
    toOperand (IM a) = OpImm a

-- (#) :: Word8 -> IM Word8
-- (#)  = IM-- Use the toOperand function from OpGenerator

infixl 6 #
(#):: (IM Word8 -> Asm ()) -> Word8 -> Asm ()
ins # val  = ins (IM val)

-- newtype A a  = Maybe a
-- instance OpGenerator (A ()) where
--     toOperand A = OpNull -- OpGenerator constructors for addressing modes


-- nieużywane
-- newtype Z a = Z a
-- instance OpGenerator (Z Word8) where
--     toOperand (Z v) = OpZP (AddrLit8 v)

-- newtype ZX a = ZX a
-- instance OpGenerator (ZX Word8) where
--     toOperand (ZX addr) = OpZPX (AddrLit8 addr)

-- newtype ZY a = ZY a
-- instance OpGenerator (ZY Word8) where
--     toOperand (ZY addr) = OpZPY (AddrLit8 addr)


-- define newtype wrappers for absolute X and Y indexed addressing modes
------------------------------------------------------------------------
newtype X a = X a
instance OpGenerator (X Word8) where
    toOperand (X addr) = OpAbsX (AddrLit8 addr)

instance OpGenerator (X Word16) where
    toOperand (X addr) = OpAbsX (AddrLit16 addr)

instance OpGenerator (X String) where
    toOperand (X addr) = OpAbsX (AddrLabel addr)

--TODO: missing AddrLabelExpr with zero page addressing
instance OpGenerator (X AddressRef) where
    toOperand (X addr) = 
        case addr of
            AddrLit16 _ -> OpAbsX addr
            AddrLabel _ -> OpAbsX addr
            AddrLit8 _ -> OpZPX addr
            AddrLabelExpr e -> OpAbsX (AddrLabelExpr e)


newtype Y a = Y a
instance OpGenerator (Y Word8) where
    toOperand (Y addr) = OpZPY (AddrLit8 addr)

instance OpGenerator (Y Word16) where
    toOperand (Y addr) = OpAbsY (AddrLit16 addr)

instance OpGenerator (Y String) where
    toOperand (Y addr) = OpAbsY (AddrLabel addr)

instance OpGenerator (Y AddressRef) where  -- TODO: uzupełnić wszystkie instancje AddressRef
    toOperand (Y addr) = 
        case addr of
            AddrLit16 _ -> OpAbsY addr
            AddrLabel _ -> OpAbsY addr
            AddrLabelExpr _ -> OpAbsY addr
            AddrLit8 _ -> OpZPY addr


-- Indirect addressing modes
-- ----------------------------------------------------------------
newtype I a = I a

instance OpGenerator (I Word8) where
    toOperand (I addr) = OpInd (AddrLit8 addr)
instance OpGenerator (I Word16) where
    toOperand (I addr) = OpInd (AddrLit16 addr)

instance OpGenerator (I String) where
    toOperand (I addr) = OpInd (AddrLabel addr)

instance OpGenerator (I AddressRef) where
    toOperand (I addr) = case addr of
        AddrLit16 _ -> OpInd addr
        AddrLabel _ -> OpInd addr
        AddrLabelExpr _ -> OpInd addr
        AddrLit8 _ -> OpInd addr


newtype IX a = IX a
instance OpGenerator (IX Word8) where
    toOperand (IX addr) = OpIndX (AddrLit8 addr)

instance OpGenerator (IX Word16) where
    toOperand (IX addr) = OpIndX (AddrLit16 addr)

instance OpGenerator (IX AddressRef) where
    toOperand (IX addr) = case addr of
        AddrLit16 _ -> OpIndX addr
        AddrLabel _ -> OpIndX addr
        AddrLabelExpr _ -> OpIndX addr
        AddrLit8 _ -> OpIndX addr

instance OpGenerator (IX String) where
    toOperand (IX addr) = OpIndX (AddrLabel addr)


newtype IY a = IY a
instance OpGenerator (IY Word8) where
    toOperand (IY addr) = OpIndY (AddrLit8 addr)

instance OpGenerator (IY Word16) where
    toOperand (IY addr) = OpIndY (AddrLit16 addr)

instance OpGenerator (IY AddressRef) where
    toOperand (IY addr) = case addr of
        AddrLit16 _ -> OpIndY addr
        AddrLabel _ -> OpIndY addr
        AddrLabelExpr _ -> OpIndY addr
        AddrLit8 _ -> OpIndY addr


instance OpGenerator (IY String) where
    toOperand (IY addr) = OpIndY (AddrLabel addr)


-- --- eDSL Instruction Aliases ---
lda :: OpGenerator a => a -> Asm (); lda op = emitIns LDA (toOperand op)
-- lda :: OpGenerator b => (a -> b) -> a -> Asm (); lda mode addr = emitIns LDA (toOperand (mode addr))
sta :: OpGenerator a => a -> Asm (); sta op = emitIns STA (toOperand op)
ldx :: OpGenerator a => a -> Asm (); ldx op = emitIns LDX (toOperand op)
ldy :: OpGenerator a => a -> Asm (); ldy op = emitIns LDY (toOperand op)
jmp :: OpGenerator a => a -> Asm (); jmp op = emitIns JMP (toOperand op)
-- jmp :: AddressRef -> Asm (); jmp addr = emitIns JMP (OpAbs addr)

jsr :: OpGenerator a => a -> Asm (); jsr op = emitIns JSR (toOperand op)
-- jsr :: AddressRef -> Asm (); jsr addr = emitIns JSR (OpAbs addr)
-- inc :: OpGenerator a => a -> Asm (); inc op = emitIns INC (toOperand op)
-- dec :: OpGenerator a => a -> Asm (); dec op = emitIns DEC (toOperand op)
inx :: Asm (); inx = emitImplied INX
rts :: Asm (); rts = emitImplied RTS
adc :: OpGenerator a => a -> Asm (); adc op = emitIns ADC (toOperand op)
sbc :: OpGenerator a => a -> Asm (); sbc op = emitIns SBC (toOperand op)
tax :: Asm (); tax = emitImplied TAX
tay :: Asm (); tay = emitImplied TAY
txa :: Asm (); txa = emitImplied TXA
tya :: Asm (); tya = emitImplied TYA
txs :: Asm (); txs = emitImplied TXS
tsx :: Asm (); tsx = emitImplied TSX
stx :: OpGenerator a => a -> Asm (); stx op = emitIns STX (toOperand op)
sty :: OpGenerator a => a -> Asm (); sty op = emitIns STY (toOperand op)
cmp :: OpGenerator a => a -> Asm (); cmp op = emitIns CMP (toOperand op)
cpx :: OpGenerator a => a -> Asm (); cpx op = emitIns CPX (toOperand op)
cpy :: OpGenerator a => a -> Asm (); cpy op = emitIns CPY (toOperand op)
bne :: Label -> Asm (); bne = emitBranch B_BNE
beq :: Label -> Asm (); beq = emitBranch B_BEQ
bcs :: Label -> Asm (); bcs = emitBranch B_BCS
bcc :: Label -> Asm (); bcc = emitBranch B_BCC
bmi :: Label -> Asm (); bmi = emitBranch B_BMI
bpl :: Label -> Asm (); bpl = emitBranch B_BPL
bvs :: Label -> Asm (); bvs = emitBranch B_BVS
bvc :: Label -> Asm (); bvc = emitBranch B_BVC
ora :: OpGenerator a => a -> Asm (); ora op = emitIns ORA (toOperand op)
asl :: OpGenerator a => Maybe a -> Asm ()
asl Nothing   = emitAccumulator ASL
asl (Just op) = emitIns ASL (toOperand op)
php :: Asm (); php = emitImplied PHP
clc :: Asm (); clc = emitImplied CLC
and :: OpGenerator a => a -> Asm (); and op = emitIns AND (toOperand op)
bit :: OpGenerator a => a -> Asm (); bit op = emitIns BIT (toOperand op)
rol :: OpGenerator a => Maybe a -> Asm ()
rol Nothing   = emitAccumulator ROL
rol (Just op) = emitIns ROL (toOperand op)
plp :: Asm (); plp = emitImplied PLP
sec :: Asm (); sec = emitImplied SEC
rti :: Asm (); rti = emitImplied RTI
eor :: OpGenerator a => a -> Asm (); eor op = emitIns EOR (toOperand op)
lsr :: OpGenerator a => Maybe a -> Asm ()
lsr Nothing   = emitAccumulator LSR
lsr (Just op) = emitIns LSR (toOperand op)
pha :: Asm (); pha = emitImplied PHA
cli :: Asm (); cli = emitImplied CLI
pla :: Asm (); pla = emitImplied PLA
sei :: Asm (); sei = emitImplied SEI
dey :: Asm (); dey = emitImplied DEY
clv :: Asm (); clv = emitImplied CLV
iny :: Asm (); iny = emitImplied INY
dex :: Asm (); dex = emitImplied DEX
cld :: Asm (); cld = emitImplied CLD
nop :: Asm (); nop = emitImplied NOP
sed :: Asm (); sed = emitImplied SED
inc :: OpGenerator a => a -> Asm (); inc op = emitIns INC (toOperand op)
dec :: OpGenerator a => a -> Asm (); dec op = emitIns DEC (toOperand op)
brk :: Asm (); brk = emitImplied BRK
ror :: OpGenerator a => Maybe a -> Asm ()
ror Nothing   = emitAccumulator ROR
ror (Just op) = emitIns ROR (toOperand op)



-- Wykonuje blok kodu, jeśli podany warunek jest PRAWDZIWY
-- (Zakłada, że flagi zostały ustawione *przed* wywołaniem if_)
if_ :: Conditions -> Asm () -> Asm ()
if_ condition asmBlock = do
    skipLabel <- makeUniqueLabel ()
    -- Wykonaj skok WARUNKOWY ZA blok, jeśli warunek jest FAŁSZYWY
    branchOnCondition (invert condition) skipLabel
    -- Wykonaj blok kodu, jeśli warunek jest PRAWIDŁOWY (nie skoczono)
    asmBlock
    l_ skipLabel -- Etykieta końca bloku if

-- Zaktualizowane makro WHILE
-- Wykonuje blok kodu, dopóki warunek jest PRAWDZIWY
-- (Zakłada, że flagi są ustawiane *przed* sprawdzeniem warunku na początku pętli)
while_ :: Conditions -> Asm () -> Asm ()
while_ condition asmBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel   <- makeUniqueLabel ()
    l_ startLabel
    -- Sprawdź warunek: skocz na koniec, jeśli FAŁSZYWY
    branchOnCondition (invert condition) endLabel
    -- Wykonaj ciało pętli, jeśli warunek PRAWDZIWY
    asmBlock
    jmp startLabel -- Wróć na początek, aby ponownie sprawdzić warunek
    l_ endLabel

-- Zaktualizowane makro DO-WHILE
-- Wykonuje blok kodu RAZ, a następnie powtarza, dopóki warunek jest PRAWDZIWY
-- (Zakłada, że flagi są ustawiane *wewnątrz* bloku, tuż przed końcem iteracji)
doWhile_ :: Conditions -> Asm () -> Asm ()
doWhile_ condition asmBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    -- Wykonaj ciało pętli
    asmBlock
    -- Sprawdź warunek na końcu: skocz na początek, jeśli PRAWIDZIWY
    branchOnCondition condition startLabel
    -- W przeciwnym razie (warunek FAŁSZYWY), wypadnij z pętli
