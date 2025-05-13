-- | Provides an EDSL (Embedded Domain Specific Language) for writing 6502 assembly instructions
-- and control flow macros.
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Assembly.EDSLInstr (

    -- OpGenerator Class
    OpGenerator(toOperand),
    -- A(..), -- Accumulator addressing (implicit)
    X(..),    -- Indexed X addressing wrapper
    Y(..),    -- Indexed Y addressing wrapper

    I(..),    -- Indirect addressing wrapper
    IX(..),   -- Indexed Indirect X addressing wrapper
    IY(..),   -- Indirect Indexed Y addressing wrapper
    IM(..),   -- Immediate addressing wrapper
    (#),      -- Operator for immediate addressing
    (#>),     -- Operator for immediate MSB of label
    (#<),     -- Operator for immediate LSB of label

    -- eDSL Instruction Aliases
    lda, sta, ldx, ldy, jmp, inx, adc, sbc, tax, tay, stx, sty,
    cmp, cpx, cpy, txa, tya, txs, tsx, bne, beq, bcs, bcc, bmi,
    bpl, bvs, bvc, jsr, rts, ora, asl, php, clc, and, bit, rol,
    ror, plp, sec, rti, eor, lsr, pha, cli, pla, sei, dey, clv,
    iny, dex, cld, nop, sed, inc, dec, brk,

    -- eDSL Control Flow Macros
    if_,
    ifElse_,
    while_,
    doWhile_,
) where

import Data.Word (Word8, Word16)
import Data.String
import Prelude hiding (and) -- Hide 'and' from Prelude
import Assembly.Core (Asm, Operand(..), pattern A_, Label, emitIns, emitImplied, emitAccumulator, emitBranch, Conditions, branchOnCondition, makeUniqueLabel, invert,l_, pattern AbsLabel, pattern AddrLabel, pattern AddrLit8, pattern AddrLit16, AddressRef(..), pattern LsbImm, pattern MsbImm, Label, LabelExpression)
import Assembly.Instructions6502 (Mnemonic(..))
import Assembly.Branch (BranchMnemonic(..)) -- Import BranchMnemonic constructors

-- | Typeclass for types that can be converted to an `Operand`.
class OpGenerator a where
    toOperand :: a -> Operand



-- OpGenerator instances for Word8 and Word16 absolute addressing mode
-- -------------------------------------------------------------------
-- | Converts a `Word8` to a Zero Page operand.
instance OpGenerator Word8 where
    toOperand v = OpZP (AddrLit8 v)

-- | Converts a `Word16` to an Absolute operand.
instance OpGenerator Word16 where
    toOperand v = OpAbs (AddrLit16 v)

-- | Converts a `String` (label) to an Absolute operand.
instance OpGenerator String where
    toOperand l = OpAbs (AddrLabel l)

--TODO: missing AddrLabelExpr with zero page.
-- | Converts an `AddressRef` to an appropriate `Operand` based on its constructor.
instance OpGenerator AddressRef where
    toOperand addr =  case addr of
        AddrLit8 v -> OpZP (AddrLit8 v)       -- Zero Page addressing
        AddrLit16 v -> OpAbs (AddrLit16 v)       -- Absolute addressing
        AddrLabel l -> OpAbs (AddrLabel l) -- Absolute addressing with label
        AddrLabelExpr expr -> OpAbs (AddrLabelExpr expr) -- Absolute addressing with label expression


-- Define newtype wrappers for LSB/MSB immediate mode
-- -------------------------------------------------------
-- | Newtype wrapper for the LSB of a label's address in immediate mode.
newtype LsbLabel a = LsbLabel a
-- | Newtype wrapper for the MSB of a label's address in immediate mode.
newtype MsbLabel a = MsbLabel a

-- Add OpGenerator instances for these wrappers targeting Labels
-- | Converts `LsbLabel Label` to an `OpLsbImm` operand.
instance OpGenerator (LsbLabel Label) where
    toOperand (LsbLabel l) = OpLsbImm l -- Use the new constructor (or pattern LsbImm l)

-- | Converts `MsbLabel Label` to an `OpMsbImm` operand.
instance OpGenerator (MsbLabel Label) where
    toOperand (MsbLabel l) = OpMsbImm l -- Use the new constructor (or pattern MsbImm l)

-- | Infix operator for applying an instruction with the MSB of a label as an immediate operand.
infixl 6 #>
(#>):: (MsbLabel Label -> Asm ()) -> Label -> Asm ()
ins #> val  = ins (MsbLabel val)
-- | Infix operator for applying an instruction with the LSB of a label as an immediate operand.
infixl 6 #<
(#<):: (LsbLabel Label -> Asm ()) -> Label -> Asm ()
ins #< val  = ins (LsbLabel val)


-- Define newtype wrappers for immediate intent
-- -----------------------------------------------------------------------------
-- | Newtype wrapper to signify an immediate operand.
newtype IM a = IM a
-- | Converts `IM Word8` to an `OpImm` operand.
instance OpGenerator (IM Word8) where
    toOperand (IM a) = OpImm a

-- (#) :: Word8 -> IM Word8
-- (#)  = IM-- Use the toOperand function from OpGenerator

-- | Infix operator for applying an instruction with an immediate `Word8` operand.
infixl 6 #
(#):: (IM Word8 -> Asm ()) -> Word8 -> Asm ()
ins # val  = ins (IM val)

-- newtype A a  = Maybe a
-- instance OpGenerator (A ()) where
--     toOperand A = OpNull -- OpGenerator constructors for addressing modes


-- unused
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
-- | Newtype wrapper for X-indexed addressing.
newtype X a = X a
-- | Converts `X Word8` to an Absolute,X operand (using the Word8 as an address).
instance OpGenerator (X Word8) where
    toOperand (X addr) = OpAbsX (AddrLit8 addr)

-- | Converts `X Word16` to an Absolute,X operand.
instance OpGenerator (X Word16) where
    toOperand (X addr) = OpAbsX (AddrLit16 addr)

-- | Converts `X String` (label) to an Absolute,X operand.
instance OpGenerator (X String) where
    toOperand (X addr) = OpAbsX (AddrLabel addr)

--TODO: missing AddrLabelExpr with zero page addressing
-- | Converts `X AddressRef` to an appropriate X-indexed operand.
instance OpGenerator (X AddressRef) where
    toOperand (X addr) =
        case addr of
            AddrLit16 _ -> OpAbsX addr
            AddrLabel _ -> OpAbsX addr
            AddrLit8 _ -> OpZPX addr
            AddrLabelExpr e -> OpAbsX (AddrLabelExpr e)

-- | Newtype wrapper for Y-indexed addressing.
newtype Y a = Y a
-- | Converts `Y Word8` to a Zero Page,Y operand.
instance OpGenerator (Y Word8) where
    toOperand (Y addr) = OpZPY (AddrLit8 addr)

-- | Converts `Y Word16` to an Absolute,Y operand.
instance OpGenerator (Y Word16) where
    toOperand (Y addr) = OpAbsY (AddrLit16 addr)

-- | Converts `Y String` (label) to an Absolute,Y operand.
instance OpGenerator (Y String) where
    toOperand (Y addr) = OpAbsY (AddrLabel addr)

-- | Converts `Y AddressRef` to an appropriate Y-indexed operand.
instance OpGenerator (Y AddressRef) where  -- TODO: complete all AddressRef instances
    toOperand (Y addr) =
        case addr of
            AddrLit16 _ -> OpAbsY addr
            AddrLabel _ -> OpAbsY addr
            AddrLabelExpr _ -> OpAbsY addr
            AddrLit8 _ -> OpZPY addr


-- Indirect addressing modes
-- ----------------------------------------------------------------
-- | Newtype wrapper for indirect addressing.
newtype I a = I a

-- | Converts `I Word8` to an Indirect operand (using Word8 as address).
instance OpGenerator (I Word8) where
    toOperand (I addr) = OpInd (AddrLit8 addr)
-- | Converts `I Word16` to an Indirect operand.
instance OpGenerator (I Word16) where
    toOperand (I addr) = OpInd (AddrLit16 addr)

-- | Converts `I String` (label) to an Indirect operand.
instance OpGenerator (I String) where
    toOperand (I addr) = OpInd (AddrLabel addr)

-- | Converts `I AddressRef` to an Indirect operand.
instance OpGenerator (I AddressRef) where
    toOperand (I addr) = case addr of
        AddrLit16 _ -> OpInd addr
        AddrLabel _ -> OpInd addr
        AddrLabelExpr _ -> OpInd addr
        AddrLit8 _ -> OpInd addr

-- | Newtype wrapper for indexed indirect X addressing.
newtype IX a = IX a
-- | Converts `IX Word8` to an Indexed Indirect X operand.
instance OpGenerator (IX Word8) where
    toOperand (IX addr) = OpIndX (AddrLit8 addr)

-- | Converts `IX Word16` to an Indexed Indirect X operand.
instance OpGenerator (IX Word16) where
    toOperand (IX addr) = OpIndX (AddrLit16 addr)

-- | Converts `IX AddressRef` to an Indexed Indirect X operand.
instance OpGenerator (IX AddressRef) where
    toOperand (IX addr) = case addr of
        AddrLit16 _ -> OpIndX addr
        AddrLabel _ -> OpIndX addr
        AddrLabelExpr _ -> OpIndX addr
        AddrLit8 _ -> OpIndX addr

-- | Converts `IX String` (label) to an Indexed Indirect X operand.
instance OpGenerator (IX String) where
    toOperand (IX addr) = OpIndX (AddrLabel addr)

-- | Newtype wrapper for indirect indexed Y addressing.
newtype IY a = IY a
-- | Converts `IY Word8` to an Indirect Indexed Y operand.
instance OpGenerator (IY Word8) where
    toOperand (IY addr) = OpIndY (AddrLit8 addr)

-- | Converts `IY Word16` to an Indirect Indexed Y operand.
instance OpGenerator (IY Word16) where
    toOperand (IY addr) = OpIndY (AddrLit16 addr)

-- | Converts `IY AddressRef` to an Indirect Indexed Y operand.
instance OpGenerator (IY AddressRef) where
    toOperand (IY addr) = case addr of
        AddrLit16 _ -> OpIndY addr
        AddrLabel _ -> OpIndY addr
        AddrLabelExpr _ -> OpIndY addr
        AddrLit8 _ -> OpIndY addr

-- | Converts `IY String` (label) to an Indirect Indexed Y operand.
instance OpGenerator (IY String) where
    toOperand (IY addr) = OpIndY (AddrLabel addr)


-- --- eDSL Instruction Aliases ---
lda :: OpGenerator a => a -> Asm (); lda op = emitIns LDA (toOperand op)
sta :: OpGenerator a => a -> Asm (); sta op = emitIns STA (toOperand op)
ldx :: OpGenerator a => a -> Asm (); ldx op = emitIns LDX (toOperand op)
ldy :: OpGenerator a => a -> Asm (); ldy op = emitIns LDY (toOperand op)
jmp :: OpGenerator a => a -> Asm (); jmp op = emitIns JMP (toOperand op)
jsr :: OpGenerator a => a -> Asm (); jsr op = emitIns JSR (toOperand op)
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



-- | Executes a block of code if the given condition is TRUE.
-- (Assumes flags were set *before* calling if_).
if_ :: Conditions -> Asm () -> Asm ()
if_ condition asmBlock = do
    skipLabel <- makeUniqueLabel ()
    -- Perform a CONDITIONAL jump PAST the block if the condition is FALSE.
    branchOnCondition (invert condition) skipLabel
    -- Execute the code block if the condition is TRUE (no jump occurred).
    asmBlock
    l_ skipLabel -- End label of the if block.

-- | Executes `asmBlock` if the condition is TRUE, otherwise executes `elseBlock`.
ifElse_ :: Conditions -> Asm () -> Asm () -> Asm ()
ifElse_ condition asmBlock elseBlock = do
    skipLabel <- makeUniqueLabel ()
    elseLabel <- makeUniqueLabel ()
    -- Perform a CONDITIONAL jump to the else block if the condition is FALSE.
    branchOnCondition (invert condition) elseLabel
    -- Execute the 'then' block if the condition is TRUE.
    asmBlock
    jmp skipLabel -- Skip the else block.
    l_ elseLabel
    elseBlock
    l_ skipLabel -- End label of the if-else block.

-- | Updated WHILE macro.
-- Executes a block of code as long as the condition is TRUE.
-- (Assumes flags are set *before* checking the condition at the beginning of the loop).
while_ :: Conditions -> Asm () -> Asm ()
while_ condition asmBlock = do
    startLabel <- makeUniqueLabel ()
    endLabel   <- makeUniqueLabel ()
    l_ startLabel
    -- Check condition: jump to end if FALSE.
    branchOnCondition (invert condition) endLabel
    -- Execute loop body if condition TRUE.
    asmBlock
    jmp startLabel -- Return to the beginning to check the condition again.
    l_ endLabel

-- | Updated DO-WHILE macro.
-- Executes a block of code ONCE, then repeats as long as the condition is TRUE.
-- (Assumes flags are set *inside* the block, just before the end of the iteration).
doWhile_ :: Conditions -> Asm () -> Asm ()
doWhile_ condition asmBlock = do
    startLabel <- makeUniqueLabel ()
    l_ startLabel
    -- Execute loop body.
    asmBlock
    -- Check condition at the end: jump to the beginning if TRUE.
    branchOnCondition condition startLabel
    -- Otherwise (condition FALSE), fall out of the loop.
