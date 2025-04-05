{-# LANGUAGE LambdaCase #-}


module Assembly.Instructions6502 (
    AddressingMode(..),
    Mnemonic(..),
    instructionData,
    getModeSize,
) where
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map



-- NOWY: Enum dla wszystkich trybów adresowania
data AddressingMode
    = Implicit
    | Accumulator
    | Immediate
    | ZeroPage
    | ZeroPageX
    | ZeroPageY
    | Absolute
    | AbsoluteX
    | AbsoluteY
    | Indirect
    | IndirectX -- (zp,X)
    | IndirectY -- (zp),Y
    | Relative -- Specjalny dla skoków
    deriving (Show, Eq, Ord, Enum, Bounded)

-- Funkcja pomocnicza do obliczania rozmiaru na podstawie trybu adresowania
getModeSize :: AddressingMode -> Word8
getModeSize = \case
    Implicit      -> 1
    Accumulator   -> 1
    Immediate     -> 2
    ZeroPage      -> 2
    ZeroPageX     -> 2
    ZeroPageY     -> 2
    Absolute      -> 3
    AbsoluteX     -> 3
    AbsoluteY     -> 3
    Indirect      -> 3 -- Tylko JMP
    IndirectX     -> 2
    IndirectY     -> 2
    Relative      -> 2 -- Skoki warunkowe


-- ZAKTUALIZOWANY: Enum dla wszystkich mnemoników 6502
data Mnemonic
    = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI | BNE | BPL | BRK | BVC | BVS | CLC
    | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR | INC | INX | INY | JMP
    | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL | ROR | RTI
    | RTS | SBC | SEC | SED | SEI | STA | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
    deriving (Show, Eq, Ord, Enum, Bounded)


-- Surowe dane instrukcji: Lista (Mnemonic, AddressingMode, Opcode) - rozmiar zostanie dodany
-- Na podstawie listy OCaml, przekonwertowane na Hex i z odpowiednimi typami Haskell
instructionData :: [(Mnemonic, AddressingMode, Word8)]
instructionData = [
    (BRK, Implicit, 0x00),
    (ORA, IndirectX, 0x01),
    (ORA, ZeroPage, 0x05),
    (ASL, ZeroPage, 0x06),
    (PHP, Implicit, 0x08),
    (ORA, Immediate, 0x09),
    (ASL, Accumulator, 0x0A),
    (ORA, Absolute, 0x0D),
    (ASL, Absolute, 0x0E),
    (BPL, Relative, 0x10),
    (ORA, IndirectY, 0x11),
    (ORA, ZeroPageX, 0x15),
    (ASL, ZeroPageX, 0x16),
    (CLC, Implicit, 0x18),
    (ORA, AbsoluteY, 0x19),
    (ORA, AbsoluteX, 0x1D),
    (ASL, AbsoluteX, 0x1E),
    (JSR, Absolute, 0x20),
    (AND, IndirectX, 0x21),
    (BIT, ZeroPage, 0x24),
    (AND, ZeroPage, 0x25),
    (ROL, ZeroPage, 0x26),
    (PLP, Implicit, 0x28),
    (AND, Immediate, 0x29),
    (ROL, Accumulator, 0x2A),
    (BIT, Absolute, 0x2C),
    (AND, Absolute, 0x2D),
    (ROL, Absolute, 0x2E),
    (BMI, Relative, 0x30),
    (AND, IndirectY, 0x31),
    (AND, ZeroPageX, 0x35),
    (ROL, ZeroPageX, 0x36),
    (SEC, Implicit, 0x38),
    (AND, AbsoluteY, 0x39),
    (AND, AbsoluteX, 0x3D),
    (ROL, AbsoluteX, 0x3E),
    (RTI, Implicit, 0x40),
    (EOR, IndirectX, 0x41),
    (EOR, ZeroPage, 0x45),
    (LSR, ZeroPage, 0x46),
    (PHA, Implicit, 0x48),
    (EOR, Immediate, 0x49),
    (LSR, Accumulator, 0x4A),
    (JMP, Absolute, 0x4C),
    (EOR, Absolute, 0x4D),
    (LSR, Absolute, 0x4E),
    (BVC, Relative, 0x50),
    (EOR, IndirectY, 0x51),
    (EOR, ZeroPageX, 0x55),
    (LSR, ZeroPageX, 0x56),
    (CLI, Implicit, 0x58),
    (EOR, AbsoluteY, 0x59),
    (EOR, AbsoluteX, 0x5D),
    (LSR, AbsoluteX, 0x5E),
    (RTS, Implicit, 0x60),
    (ADC, IndirectX, 0x61),
    (ADC, ZeroPage, 0x65),
    (ROR, ZeroPage, 0x66),
    (PLA, Implicit, 0x68),
    (ADC, Immediate, 0x69),
    (ROR, Accumulator, 0x6A),
    (JMP, Indirect, 0x6C),
    (ADC, Absolute, 0x6D),
    (ROR, Absolute, 0x6E),
    (BVS, Relative, 0x70),
    (ADC, IndirectY, 0x71),
    (ADC, ZeroPageX, 0x75),
    (ROR, ZeroPageX, 0x76),
    (SEI, Implicit, 0x78),
    (ADC, AbsoluteY, 0x79),
    (ADC, AbsoluteX, 0x7D),
    (ROR, AbsoluteX, 0x7E),
    (STA, IndirectX, 0x81),
    (STY, ZeroPage, 0x84),
    (STA, ZeroPage, 0x85),
    (STX, ZeroPage, 0x86),
    (DEY, Implicit, 0x88),
    (TXA, Implicit, 0x8A),
    (STY, Absolute, 0x8C),
    (STA, Absolute, 0x8D),
    (STX, Absolute, 0x8E),
    (BCC, Relative, 0x90),
    (STA, IndirectY, 0x91),
    (STY, ZeroPageX, 0x94),
    (STA, ZeroPageX, 0x95),
    (STX, ZeroPageY, 0x96),
    (TYA, Implicit, 0x98),
    (STA, AbsoluteY, 0x99),
    (TXS, Implicit, 0x9A),
    (STA, AbsoluteX, 0x9D),
    (LDY, Immediate, 0xA0),
    (LDA, IndirectX, 0xA1),
    (LDX, Immediate, 0xA2),
    (LDY, ZeroPage, 0xA4),
    (LDA, ZeroPage, 0xA5),
    (LDX, ZeroPage, 0xA6),
    (TAY, Implicit, 0xA8),
    (LDA, Immediate, 0xA9),
    (TAX, Implicit, 0xAA),
    (LDY, Absolute, 0xAC),
    (LDA, Absolute, 0xAD),
    (LDX, Absolute, 0xAE),
    (BCS, Relative, 0xB0),
    (LDA, IndirectY, 0xB1),
    (LDY, ZeroPageX, 0xB4),
    (LDA, ZeroPageX, 0xB5),
    (LDX, ZeroPageY, 0xB6),
    (CLV, Implicit, 0xB8),
    (LDA, AbsoluteY, 0xB9),
    (TSX, Implicit, 0xBA),
    (LDY, AbsoluteX, 0xBC),
    (LDA, AbsoluteX, 0xBD),
    (LDX, AbsoluteY, 0xBE),
    (CPY, Immediate, 0xC0),
    (CMP, IndirectX, 0xC1),
    (CPY, ZeroPage, 0xC4),
    (CMP, ZeroPage, 0xC5),
    (DEC, ZeroPage, 0xC6),
    (INY, Implicit, 0xC8),
    (CMP, Immediate, 0xC9),
    (DEX, Implicit, 0xCA),
    (CPY, Absolute, 0xCC),
    (CMP, Absolute, 0xCD),
    (DEC, Absolute, 0xCE),
    (BNE, Relative, 0xD0),
    (CMP, IndirectY, 0xD1),
    (CMP, ZeroPageX, 0xD5),
    (DEC, ZeroPageX, 0xD6),
    (CLD, Implicit, 0xD8),
    (CMP, AbsoluteY, 0xD9),
    (CMP, AbsoluteX, 0xDD),
    (DEC, AbsoluteX, 0xDE),
    (CPX, Immediate, 0xE0),
    (SBC, IndirectX, 0xE1),
    (CPX, ZeroPage, 0xE4),
    (SBC, ZeroPage, 0xE5),
    (INC, ZeroPage, 0xE6),
    (INX, Implicit, 0xE8),
    (SBC, Immediate, 0xE9),
    (NOP, Implicit, 0xEA),
    (CPX, Absolute, 0xEC),
    (SBC, Absolute, 0xED),
    (INC, Absolute, 0xEE),
    (BEQ, Relative, 0xF0),
    (SBC, IndirectY, 0xF1),
    (SBC, ZeroPageX, 0xF5),
    (INC, ZeroPageX, 0xF6),
    (SED, Implicit, 0xF8),
    (SBC, AbsoluteY, 0xF9),
    (SBC, AbsoluteX, 0xFD),
    (INC, AbsoluteX, 0xFE)
  ]
