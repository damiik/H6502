{-# LANGUAGE PatternSynonyms #-}

module Assembly.EDSLInstr (

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

import Prelude hiding (and) -- Hide 'and' from Prelude
import Assembly.Core (Asm, Operand, Label, emitIns, emitImplied, emitAccumulator, emitBranch, Conditions, branchOnCondition, makeUniqueLabel, invert,l_, pattern AbsLabel)
import Assembly.Instructions6502 (Mnemonic(..))
import Assembly.Branch (BranchMnemonic(..)) -- Import BranchMnemonic constructors

-- --- eDSL Instruction Aliases ---
lda :: Operand -> Asm (); lda = emitIns LDA
sta :: Operand -> Asm (); sta = emitIns STA
ldx :: Operand -> Asm (); ldx = emitIns LDX
ldy :: Operand -> Asm (); ldy = emitIns LDY
jmp :: Operand -> Asm (); jmp = emitIns JMP
inx :: Asm (); inx = emitImplied INX
rts :: Asm (); rts = emitImplied RTS
adc :: Operand -> Asm (); adc = emitIns ADC
sbc :: Operand -> Asm (); sbc = emitIns SBC
tax :: Asm (); tax = emitImplied TAX
tay :: Asm (); tay = emitImplied TAY
txa :: Asm (); txa = emitImplied TXA
tya :: Asm (); tya = emitImplied TYA
txs :: Asm (); txs = emitImplied TXS
tsx :: Asm (); tsx = emitImplied TSX
stx :: Operand -> Asm (); stx = emitIns STX
sty :: Operand -> Asm (); sty = emitIns STY
cmp :: Operand -> Asm (); cmp = emitIns CMP
cpx :: Operand -> Asm (); cpx = emitIns CPX
cpy :: Operand -> Asm (); cpy = emitIns CPY
bne :: Label -> Asm (); bne = emitBranch B_BNE
beq :: Label -> Asm (); beq = emitBranch B_BEQ
bcs :: Label -> Asm (); bcs = emitBranch B_BCS
bcc :: Label -> Asm (); bcc = emitBranch B_BCC
bmi :: Label -> Asm (); bmi = emitBranch B_BMI
bpl :: Label -> Asm (); bpl = emitBranch B_BPL
bvs :: Label -> Asm (); bvs = emitBranch B_BVS
bvc :: Label -> Asm (); bvc = emitBranch B_BVC
jsr :: Operand -> Asm (); jsr = emitIns JSR
ora :: Operand -> Asm (); ora = emitIns ORA
asl :: Maybe Operand -> Asm ()
asl Nothing   = emitAccumulator ASL
asl (Just op) = emitIns ASL op
php :: Asm (); php = emitImplied PHP
clc :: Asm (); clc = emitImplied CLC
and :: Operand -> Asm (); and = emitIns AND
bit :: Operand -> Asm (); bit = emitIns BIT
rol :: Maybe Operand -> Asm ()
rol Nothing   = emitAccumulator ROL
rol (Just op) = emitIns ROL op
plp :: Asm (); plp = emitImplied PLP
sec :: Asm (); sec = emitImplied SEC
rti :: Asm (); rti = emitImplied RTI
eor :: Operand -> Asm (); eor = emitIns EOR
lsr :: Maybe Operand -> Asm ()
lsr Nothing   = emitAccumulator LSR
lsr (Just op) = emitIns LSR op
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
inc :: Operand -> Asm (); inc = emitIns INC
dec :: Operand -> Asm (); dec = emitIns DEC
brk :: Asm (); brk = emitImplied BRK
ror :: Maybe Operand -> Asm ()
ror Nothing   = emitAccumulator ROR
ror (Just op) = emitIns ROR op



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
    jmp $ AbsLabel startLabel -- Wróć na początek, aby ponownie sprawdzić warunek
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
