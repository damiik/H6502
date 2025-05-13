-- | Defines the core data types, state, monad, and primitive functions for the 6502 assembler.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Assembly.Core (
    -- Core Types
    Address,
    Label,
    ProgramCounter,
    LabelExpression(..), -- New type for label arithmetic
    AddressRef(..),
    AddressingMode(..),
    Operand(..),
    pattern A_,
    pattern Imm,
    pattern AbsLit,
    pattern AbsAddress,
    pattern AbsLabel,
    pattern AbsXLit,
    pattern AbsXLabel,
    pattern ZPLabel,
    pattern ZPAddr,
    zpLit,
    pattern Ind,
    pattern IndX,
    pattern IndY,
    pattern ZPX,
    pattern ZPY,
    pattern AbsYLabel,
    pattern AbsYLit,
    pattern ImmLsbLabel, -- Export new pattern synonym
    pattern ImmMsbLabel, -- Export new pattern synonym
    pattern LsbImm,
    pattern MsbImm,
    BranchMnemonic(..),
    Mnemonic(..),
    SymbolicInstruction(..),
    Directive(..), -- NEW: Represents directives like ORG

    -- Core State & Monad
    AsmState(..),
    initialAsmState,
    Asm(..),

    -- Core Primitives (some will be modified)
    getInstructionInfo, -- NEW FUNCTION INSTEAD OF getInstructionSize/operandOpcode
    getInstructionSize, -- WILL BE MODIFIED/REMOVED
    emitGeneric,
    emitIns,
    emitImplied, -- MAY BE MODIFIED
    emitAccumulator, -- NEW
    emitBranch,
    l_,

    db, -- Renamed
    dw, -- Renamed
    string, -- Renamed
    immChar,
    stringC64, -- NEW: PETSCII string function for screen codes
    org, -- Renamed & Export the org directive function
    makeUniqueLabel,
    makeLabelWithPrefix,
    generateInstructionBytes,
    invert,

    -- Opcode-related functions (now table-based)
    instructionTable, -- NEW TABLE
    getOperandAddressingMode, -- NEW HELPER FUNCTION
    resolveAddressMaybe, -- EXISTING
    getOperandAddressRef, -- EXISTING
    branchOpcode, -- NEW HELPER FUNCTION

    -- Miscellaneous
    lsb, msb,
    hx,
    asc,
    addr2word16,
    evalLabelExpr, -- NEW EXPORT
    wordToBytesLE, -- Added export
    (.+) , -- Renamed operator for AddressRef
    (.-), -- Renamed operator for AddressRef
    parens,            -- function for wrapping AddressRef in parentheses

    -- New typeclass for arithmetic expressions and its methods
    ArithExpr(add, sub),

    -- conditions
    Conditions(..),
    pattern AccIsNonZero,
    pattern AccIsZero,
    pattern AccIsPositive,
    pattern AccIsNegative,
    branchOnCondition,

) where

import Prelude hiding (and, or) -- Hiding only 'and', 'or'
import qualified Prelude as P (and, or)  -- Use P.(+) and P.(-) for standard numeric ops
import Control.Monad.State.Strict
import Control.Monad (when)
import Data.Word
import Data.Int (Int8)
import Data.Bits ((.&.), shiftR)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) -- Explicit import for Map
import Data.Maybe (fromMaybe) -- Needed for lookups
import Data.Char (ord, chr, isDigit, isAsciiUpper, isAsciiLower)
import Numeric (showHex)
import Data.List (foldl') -- Needed for building the table

import Assembly.Branch (BranchMnemonic(..)) -- Importing BranchMnemonic from a separate module
import Assembly.Instructions6502 (getModeSize, instructionData, Mnemonic(..), AddressingMode(..))

-- --- Types ---
-- | Represents a 16-bit memory address.
type Address = Word16
-- | Represents a label in assembly code.
type Label = String
-- | Represents the program counter.
type ProgramCounter = Word16

-- | Represents an expression involving labels for address calculation.
data LabelExpression
  = LabelRef Label -- ^ A direct reference to a label.
  | LabelAdd LabelExpression Word16 -- ^ Addition of an offset to a label expression.
  | LabelSub LabelExpression Word16 -- ^ Subtraction of an offset from a label expression.
  | LabelParen LabelExpression -- ^ Parenthesized label expression.
  deriving (Show, Eq, Ord)

-- | Represents a reference to an address, which can be a literal, a label, or a label expression.
data AddressRef
  = AddrLit16 Word16 -- ^ A 16-bit literal address.
  | AddrLit8 Word8   -- ^ An 8-bit literal address (typically for zero-page).
  | AddrLabel Label  -- ^ A reference to a label.
  | AddrLabelExpr LabelExpression -- ^ A reference to a label expression.
  deriving (Show, Eq, Ord)

-- | Operator for adding an offset to an `AddressRef`.
infixl 6 .+
(.+):: AddressRef -> Word16 -> AddressRef
a .+ i  =
    case a of
        AddrLit16 v -> AddrLit16 (v + i) -- Use P.(+) for Word16 addition
        AddrLit8 v -> AddrLit8 (fromIntegral (fromIntegral v + i)) -- Use P.(+) for Word16 addition
        AddrLabel l -> AddrLabelExpr (LabelAdd (LabelRef l) i)
        AddrLabelExpr expr -> AddrLabelExpr (LabelAdd expr i)

-- | Operator for subtracting an offset from an `AddressRef`.
infixl 6 .-
(.-) :: AddressRef -> Word16 -> AddressRef
a .- i =
  case a of
    AddrLit16 v -> AddrLit16 (v - i) -- Use P.(-) for Word16 subtraction
    AddrLit8 v -> AddrLit8 (fromIntegral (fromIntegral v - i)) -- Use P.(-) for Word16 subtraction
    AddrLabel l -> AddrLabelExpr (LabelSub (LabelRef l) i)
    AddrLabelExpr expr -> AddrLabelExpr (LabelSub expr i)

-- | Typeclass for types that support arithmetic operations with Word16 offsets.
class ArithExpr a where
  add :: a -> Word16 -> a
  sub :: a -> Word16 -> a

-- | Instance of `ArithExpr` for `AddressRef`.
instance ArithExpr AddressRef where
  add = (.+)
  sub = (.-)

-- | Represents the operand of a 6502 instruction.
data Operand
    = OpImm Word8          -- ^ Immediate addressing: `#$value`.
    | OpZP AddressRef      -- ^ Zero Page addressing: `$address`.
    | OpZPX AddressRef     -- ^ Zero Page,X addressing: `$address,X`.
    | OpZPY AddressRef     -- ^ Zero Page,Y addressing: `$address,Y`.
    | OpAbs AddressRef     -- ^ Absolute addressing: `$address`.
    | OpAbsX AddressRef    -- ^ Absolute,X addressing: `$address,X`.
    | OpAbsY AddressRef    -- ^ Absolute,Y addressing: `$address,Y`.
    | OpInd AddressRef     -- ^ Indirect addressing (only for JMP): `($address)`.
    | OpIndX AddressRef    -- ^ Indexed Indirect addressing: `($address,X)`.
    | OpIndY AddressRef    -- ^ Indirect Indexed addressing: `($address),Y`.
    -- NEW: Operands for immediate LSB/MSB of a label's address
    | OpImmLsbLabel Label  -- ^ Immediate LSB of label address, e.g., `LDA #<label`.
    | OpImmMsbLabel Label  -- ^ Immediate MSB of label address, e.g., `LDA #>label`.
    -- NEW: Replace OpImmLsbLabel/OpImmMsbLabel with these:
    | OpLsbImm Label       -- ^ Immediate LSB of label address, e.g., `LDA #<label`.
    | OpMsbImm Label       -- ^ Immediate MSB of label address, e.g., `LDA #>label`.

    | OpNull               -- ^ Represents no operand (for implied/accumulator instructions).
    -- No constructor for Implicit, Accumulator, Relative - they are handled differently.
    deriving (Show, Eq)

-- | Pattern synonym for accumulator addressing (represents no explicit operand).
pattern A_ :: Maybe Word8; pattern A_ = Nothing -- Dummy value, as A is implicit
-- | Pattern synonym for immediate operand.
pattern Imm :: Word8 -> Operand; pattern Imm v = OpImm v
-- | Pattern synonym for absolute literal operand.
pattern AbsLit :: Word16 -> Operand; pattern AbsLit v = OpAbs (AddrLit16 v)
-- | Pattern synonym for absolute address reference operand.
pattern AbsAddress :: AddressRef -> Operand; pattern AbsAddress v = OpAbs v
-- | Pattern synonym for absolute label operand.
pattern AbsLabel :: Label -> Operand; pattern AbsLabel l = OpAbs (AddrLabel l)
-- | Pattern synonym for absolute,X literal operand.
pattern AbsXLit :: Word16 -> Operand; pattern AbsXLit v = OpAbsX (AddrLit16 v)
-- | Pattern synonym for absolute,X label operand.
pattern AbsXLabel :: Label -> Operand; pattern AbsXLabel l = OpAbsX (AddrLabel l)
-- | Pattern synonym for absolute,Y literal operand.
pattern AbsYLit :: Word16 -> Operand; pattern AbsYLit v = OpAbsY (AddrLit16 v)
-- | Pattern synonym for absolute,Y label operand.
pattern AbsYLabel :: Label -> Operand; pattern AbsYLabel l = OpAbsY (AddrLabel l)
-- | Pattern synonym for zero-page label operand.
pattern ZPLabel :: Label -> Operand; pattern ZPLabel l = OpZP (AddrLabel l)
-- | Pattern synonym for zero-page address literal operand.
pattern ZPAddr :: Word8 -> Operand; pattern ZPAddr l = OpZP (AddrLit8 l)
-- | Smart constructor for zero-page literal operand.
zpLit :: Word8 -> Operand; zpLit v = OpZP (AddrLit16 (fromIntegral v))
-- | Pattern synonym for zero-page,X address reference operand.
pattern ZPX :: AddressRef -> Operand; pattern ZPX r = OpZPX r
-- | Pattern synonym for zero-page,Y address reference operand.
pattern ZPY :: AddressRef -> Operand; pattern ZPY r = OpZPY r
-- | Pattern synonym for indirect address reference operand.
pattern Ind :: AddressRef -> Operand; pattern Ind r = OpInd r
-- | Pattern synonym for indexed indirect address reference operand.
pattern IndX :: AddressRef -> Operand; pattern IndX r = OpIndX r
-- | Pattern synonym for indirect indexed address reference operand.
pattern IndY :: AddressRef -> Operand; pattern IndY r = OpIndY r
-- | Pattern synonym for immediate LSB of label operand.
pattern ImmLsbLabel :: Label -> Operand; pattern ImmLsbLabel l = OpImmLsbLabel l
-- | Pattern synonym for immediate MSB of label operand.
pattern ImmMsbLabel :: Label -> Operand; pattern ImmMsbLabel l = OpImmMsbLabel l
-- | Pattern synonym for immediate LSB of label operand (alternative).
pattern LsbImm :: Label -> Operand; pattern LsbImm l = OpLsbImm l
-- | Pattern synonym for immediate MSB of label operand (alternative).
pattern MsbImm :: Label -> Operand; pattern MsbImm l = OpMsbImm l

-- | Represents assembler directives like ORG.
newtype Directive = DOrg Address deriving (Show, Eq) -- HLint: Use newtype

-- | Represents a single symbolic instruction or directive in the assembly code.
data SymbolicInstruction
    = SLabelDef Label -- ^ Definition of a label.
    | SIns Mnemonic (Maybe Operand) -- ^ A 6502 instruction with an optional operand.
    | SBranch BranchMnemonic Label -- ^ A branch instruction to a label.
    | SBytes [Word8] -- ^ A sequence of bytes to be emitted.
    | SWords [Word16] -- ^ A sequence of words (16-bit) to be emitted.
    | SDirective Directive -- ^ An assembler directive.
    deriving (Show, Eq)


-- --- Assembler State (no changes) ---
-- | Represents the state of the assembler.
data AsmState = AsmState
  { asmPC            :: ProgramCounter -- ^ Current program counter.
  , asmLabels        :: Map.Map Label ProgramCounter -- ^ Map of labels to their addresses.
  , asmCode          :: [(ProgramCounter, SymbolicInstruction)] -- ^ List of assembled instructions and their PCs.
  , asmMacroCounter  :: Int -- ^ Counter for generating unique labels in macros.
  } deriving (Show)

-- | Creates an initial assembler state with the given starting address.
initialAsmState :: ProgramCounter -> AsmState
initialAsmState startAddr = AsmState startAddr Map.empty [] 0

-- --- Assembler Monad (no changes) ---
-- | The assembler monad, a state monad transformer over IO.
newtype Asm a = Asm { unAsm :: State AsmState a }
  deriving (Functor, Applicative, Monad, MonadState AsmState)

-- --- Instruction Table ---

-- | Type for an entry in the instruction table: (Opcode, Size in bytes).
type OpcodeEntry = (Word8, Word8)

-- | Main instruction table: Map from Mnemonic to a map from AddressingMode to OpcodeEntry.
instructionTable :: Map Mnemonic (Map AddressingMode OpcodeEntry)
instructionTable = buildInstructionTable instructionData

-- | Helper function to convert `BranchMnemonic` to `Mnemonic`.
-- (moved/created here)
branchMnemonicToMnemonic :: BranchMnemonic -> Mnemonic
branchMnemonicToMnemonic = \case
    Assembly.Branch.B_BNE -> BNE;
    Assembly.Branch.B_BEQ -> BEQ;
    Assembly.Branch.B_BCS -> BCS;
    Assembly.Branch.B_BCC -> BCC;
    Assembly.Branch.B_BMI -> BMI;
    Assembly.Branch.B_BPL -> BPL;
    Assembly.Branch.B_BVS -> BVS;
    Assembly.Branch.B_BVC -> BVC;
    -- These constructors on the right side now refer to the Mnemonic type from Instructions6502

-- | Function for getting the opcode for a branch instruction.
-- (moved and modified from Core.hs)
-- Now uses instructionTable from Core.hs.
branchOpcode :: BranchMnemonic -> Word8
branchOpcode bm =
    let mnemonic = branchMnemonicToMnemonic bm
    in case Map.lookup mnemonic instructionTable >>= Map.lookup Relative of
        Just (opcode, _size) -> opcode
        Nothing -> error $ "Internal error: Opcode for branch " ++ show bm ++ " not found in instruction table."


-- | Function for building the main instruction table from a list of instruction data.
buildInstructionTable :: [(Mnemonic, AddressingMode, Word8)] -> Map Mnemonic (Map AddressingMode OpcodeEntry)
buildInstructionTable = foldl' insertInstruction Map.empty
  where
    insertInstruction table (mnemonic, mode, opcode) =
      let size = getModeSize mode
          entry = (opcode, size)
          modeMap = Map.singleton mode entry
      in Map.insertWith Map.union mnemonic modeMap table

-- | NEW: Helper function for mapping an `Operand` to its corresponding `AddressingMode`.
-- For Implicit/Accumulator, the mnemonic is needed to determine the mode.
getOperandAddressingMode :: Mnemonic -> Maybe Operand -> Either String AddressingMode
getOperandAddressingMode _ (Just (OpImm _))   = Right Immediate
getOperandAddressingMode _ (Just (OpZP _))    = Right ZeroPage
getOperandAddressingMode _ (Just (OpZPX _))   = Right ZeroPageX
getOperandAddressingMode _ (Just (OpZPY _))   = Right ZeroPageY
getOperandAddressingMode _ (Just (OpAbs _))   = Right Absolute
getOperandAddressingMode _ (Just (OpAbsX _))  = Right AbsoluteX
getOperandAddressingMode _ (Just (OpAbsY _))  = Right AbsoluteY
getOperandAddressingMode _ (Just (OpInd _))   = Right Indirect
getOperandAddressingMode _ (Just (OpIndX _))  = Right IndirectX
getOperandAddressingMode _ (Just (OpIndY _))  = Right IndirectY
-- NEW: Handle LSB/MSB label operands as Immediate mode
getOperandAddressingMode _ (Just (OpImmLsbLabel _)) = Right Immediate
getOperandAddressingMode _ (Just (OpImmMsbLabel _)) = Right Immediate
getOperandAddressingMode _ (Just (OpLsbImm _))= Right Immediate -- NEW: Treat as Immediate
getOperandAddressingMode _ (Just (OpMsbImm _))= Right Immediate -- NEW: Treat as Immediate
getOperandAddressingMode m (Just (OpNull)) = case m of
    -- Checking if mnemonic supports Accumulator mode
    ASL -> Right Accumulator
    LSR -> Right Accumulator
    ROL -> Right Accumulator
    ROR -> Right Accumulator
    -- Add other mnemonics that support Accumulator mode
    _ -> Left $ "Mnemonic " ++ show m ++ " does not support Accumulator mode"
getOperandAddressingMode m Nothing = case m of
    -- Checking if mnemonic supports Accumulator mode
    ASL -> Right Accumulator
    LSR -> Right Accumulator
    ROL -> Right Accumulator
    ROR -> Right Accumulator
    -- Otherwise, we assume Implicit (or error if there is no entry)
    _   -> Right Implicit

-- | NEW: Function for getting instruction information (opcode and size).
getInstructionInfo :: Mnemonic -> Maybe Operand -> Either String OpcodeEntry
getInstructionInfo m maybeOp = do
    mode <- getOperandAddressingMode m maybeOp
    case Map.lookup m instructionTable >>= Map.lookup mode of
        Just entry -> Right entry
        Nothing    -> Left $ "Unsupported addressing mode '" ++ show mode ++ "' for mnemonic '" ++ show m ++ "'"

-- --- Core Primitive Functions (MODIFICATIONS) ---

-- | Resolves an `AddressRef` to a `Word16` address, if possible.
-- (updated to handle AddrLabelExpr and LabelParen)
resolveAddressMaybe :: Maybe AddressRef -> AsmState -> Maybe Word16
resolveAddressMaybe Nothing _ = Just 0 -- For Imm, value doesn't matter
resolveAddressMaybe (Just (AddrLit16 v)) _ = Just v
resolveAddressMaybe (Just (AddrLit8 v)) _ = Just (fromIntegral v) -- Convert to Word16
resolveAddressMaybe (Just (AddrLabel l)) s = Map.lookup l (asmLabels s)
resolveAddressMaybe (Just (AddrLabelExpr expr)) s = resolveLabelExpr expr (asmLabels s)
  where
    resolveLabelExpr :: LabelExpression -> Map Label ProgramCounter -> Maybe Word16
    resolveLabelExpr (LabelRef l) labels = Map.lookup l labels
    resolveLabelExpr (LabelAdd subExpr offset) labels = (+) offset <$> resolveLabelExpr subExpr labels -- Use P.(+)
    resolveLabelExpr (LabelSub subExpr offset) labels = subtract offset <$> resolveLabelExpr subExpr labels -- Using subtract for correct order
    resolveLabelExpr (LabelParen subExpr) labels = resolveLabelExpr subExpr labels -- Handle LabelParen

-- | Gets the size of an instruction in bytes.
-- (now uses getInstructionInfo)
getInstructionSize :: Mnemonic -> Maybe Operand -> Either String Word16
getInstructionSize m maybeOp = fmap (fromIntegral . snd) (getInstructionInfo m maybeOp)

-- | Emits a generic symbolic instruction and updates the program counter.
-- (Use P.(+) for PC update)
emitGeneric :: SymbolicInstruction -> Either String Word16 -> Asm ()
emitGeneric _ (Left err) = error $ "Assembly Error (emitGeneric): " ++ err -- TODO: Better error handling
emitGeneric instruction (Right size) = do
  pc <- gets asmPC
  modify' $ \s -> s { asmCode = (pc, instruction) : asmCode s, asmPC = pc + size }

-- | Emits an instruction with an operand.
-- (no changes, but uses updated getInstructionSize)
emitIns :: Mnemonic -> Operand -> Asm ()
emitIns m op = emitGeneric (SIns m (Just op)) (getInstructionSize m (Just op))

-- | Emits an implied instruction (no operand).
-- (now uses SIns m Nothing)
emitImplied :: Mnemonic -> Asm ()
emitImplied m = emitGeneric (SIns m Nothing) (getInstructionSize m Nothing)

-- | NEW: Emits an accumulator instruction (no explicit operand).
-- (for clarity, although technically it's also SIns m Nothing)
emitAccumulator :: Mnemonic -> Asm ()
emitAccumulator m = emitGeneric (SIns m Nothing) (getInstructionSize m Nothing)

-- | Emits a branch instruction.
-- (no changes)
emitBranch :: BranchMnemonic -> Label -> Asm ()
emitBranch m l = emitGeneric (SBranch m l) (Right 2)

-- | NEW: ORG directive function.
-- Sets the program counter to a specific address.
org :: Address -> Asm ()
org addr = do
    pc <- gets asmPC -- Get current PC for the record, though it doesn't generate bytes
    -- Add the directive instruction to the code list
    modify' $ \s -> s { asmCode = (pc, SDirective (DOrg addr)) : asmCode s }
    -- Set the program counter to the new address
    modify' $ \s -> s { asmPC = addr }

-- | Defines a label at the current program counter.
-- (no changes)
l_ :: Label -> Asm ()
l_ lbl = do pc <- gets asmPC; labels <- gets asmLabels
            when (Map.member lbl labels) $ error $ "Label redefined: " ++ lbl -- TODO: Better error handling
            modify' $ \s -> s { asmLabels = Map.insert lbl pc labels }
            emitGeneric (SLabelDef lbl) (Right 0)

-- | Emits a sequence of bytes. (Renamed)
db :: [Word8] -> Asm (); db bs = let size = fromIntegral $ length bs in when (size > 0) $ emitGeneric (SBytes bs) (Right size)
-- | Emits a sequence of words (16-bit). (Renamed)
dw :: [Word16] -> Asm (); dw ws = let size = fromIntegral (length ws) * 2 in when (size > 0) $ emitGeneric (SWords ws) (Right size)
-- | Emits a string as a sequence of ASCII bytes. (Renamed)
string :: String -> Asm (); string str = let bytes = map (fromIntegral . ord) str; size = fromIntegral $ length bytes in when (size > 0) $ emitGeneric (SBytes bytes) (Right size)

-- | Creates an immediate operand from a character (its ASCII value).
immChar :: Char -> Operand; immChar v = OpImm (fromIntegral (ord v))

-- | Emits a string as a sequence of C64 screen codes.
stringC64 :: String -> Asm ()
stringC64  str =
  let
    -- Simple PETSCII conversion (basic characters)
    asciiToScreencode :: Char -> Word8
    asciiToScreencode c
      | isAsciiUpper c = fromIntegral (ord c - 64)          -- A-Z: 65-90 -> 1-26
      | isAsciiLower c = fromIntegral (ord c - 96)          -- a-z: 97-122 -> 1-26 (mapped to uppercase)
      | isDigit c      = fromIntegral (ord c - 48 + 48)     -- 0-9: 48-57 -> 48-57
      | c == ' '             =  32                    -- Space: 32
      | c == '@'             =  0                     -- @: 0
      | c == '$'             =  36                    -- $: 36
      | c == ','             =  44                    -- ,: 44
      | c == '.'             =  46                    -- .: 46
      | c == ':'             =  58                    -- :: 58
      | c == ';'             =  59                    -- ;: 59
      | c == '='             =  61                    -- =: 61
      | c == '?'             =  63                    -- ?: 63
      | c == '!'             =  33                    -- !: 33
      | c == '#'             =  35                    -- #: 35
      | c == '\0'            =  0                     -- Null character
      | otherwise            =  63                    -- Unsupported characters: ? (63)
    bytes = map asciiToScreencode str
    size = fromIntegral $ length bytes
  in when (size > 0) $ emitGeneric (SBytes bytes) (Right size)


-- | Generates a unique label with a given prefix.
-- (Use P.(+) for counter update)
makeUniqueLabel_ :: String -> Asm Label
makeUniqueLabel_ prefix = do
    count <- gets asmMacroCounter
    modify' $ \s -> s { asmMacroCounter = count + 1 }
    return $ "_lbl_" ++ prefix ++ "_" ++ show count

-- | Generates a unique label with no prefix.
makeUniqueLabel :: () -> Asm Label
makeUniqueLabel _ = makeUniqueLabel_ ""

-- | Generates a unique label with the given prefix.
makeLabelWithPrefix :: String -> Asm Label
makeLabelWithPrefix = makeUniqueLabel_


-- --- Opcode-related functions (DELETED/REPLACED) ---
-- impliedOpcode, operandOpcode are replaced by logic in generateBinary using instructionTable

-- | Helper function for `generateBinary` (can be in Assembly.hs).
-- Generates the byte sequence for a given instruction.
-- (updated to handle OpImmLsbLabel and OpImmMsbLabel)
generateInstructionBytes :: Mnemonic -> Maybe Operand -> AsmState -> Either String [Word8]
generateInstructionBytes m maybeOp asmState = do
    (opcode, size) <- getInstructionInfo m maybeOp
    -- Helper to get the immediate opcode for the given mnemonic
    let getImmOpcode = case Map.lookup m instructionTable >>= Map.lookup Immediate of
                           Just (op, _) -> Right op
                           Nothing -> Left $ "Mnemonic '" ++ show m ++ "' does not support Immediate addressing mode (needed for LSB/MSB)."

    -- Handle specific operand types
    case maybeOp of

        Just (OpLsbImm l) -> do -- NEW CASE
            immOpcode <- getImmOpcode
            case Map.lookup l (asmLabels asmState) of
                Just addr -> Right [immOpcode, lsb addr]
                Nothing   -> Left $ "Failed to resolve label '" ++ l ++ "' for LSB immediate operand"
        Just (OpMsbImm l) -> do -- NEW CASE
            immOpcode <- getImmOpcode
            case Map.lookup l (asmLabels asmState) of
                Just addr -> Right [immOpcode, msb addr]
                Nothing   -> Left $ "Failed to resolve label '" ++ l ++ "' for MSB immediate operand"


        Just (OpImmLsbLabel l) ->
            case Map.lookup l (asmLabels asmState) of
                Just addr -> Right [opcode, lsb addr]
                Nothing   -> Left $ "Failed to resolve label '" ++ l ++ "' for LSB operand"
        Just (OpImmMsbLabel l) ->
            case Map.lookup l (asmLabels asmState) of
                Just addr -> Right [opcode, msb addr]
                Nothing   -> Left $ "Failed to resolve label '" ++ l ++ "' for MSB operand"
        Just (OpImm v) -> Right [opcode, v] -- Standard Immediate

        _ -> do -- Handle other address-based operands and implied/accumulator
            --(opcode, size) <- getInstructionInfo m maybeOp -- Get opcode and size for non-immediate LSB/MSB
            -- Handle Implicit/Accumulator (size 1)
            --when (size == 1) $ Right [opcode]

            -- Handle address-based operands (size 2 or 3)
            addressRef <- Right $ getOperandAddressRef maybeOp
            resolvedAddrMaybe <- Right $ resolveAddressMaybe addressRef asmState
            case resolvedAddrMaybe of
                Nothing | size > 1 && maybeOp /= Nothing ->
                    Left $ "Failed to resolve address for " ++ show m ++ " " ++ show maybeOp
                _ -> let resolvedAddr = fromMaybe 0 resolvedAddrMaybe
                     in Right $ case size of
                         1 -> [opcode] -- Implicit or Accumulator
                         2 -> [opcode, lsb resolvedAddr] -- ZP, ZPX, ZPY, IndX, IndY, Relative
                         3 -> [opcode, lsb resolvedAddr, msb resolvedAddr] -- Abs, AbsX, AbsY, Ind
                         _ -> error "Internal error: Invalid instruction size derived"

-- | Extracts the `AddressRef` from an `Operand`, if present.
-- (no changes needed, extracts AddressRef)
getOperandAddressRef :: Maybe Operand -> Maybe AddressRef
getOperandAddressRef = \case
    Just (OpZP r)   -> Just r
    Just (OpZPX r)  -> Just r
    Just (OpZPY r)  -> Just r
    Just (OpAbs r)  -> Just r
    Just (OpAbsX r) -> Just r
    Just (OpAbsY r) -> Just r
    Just (OpInd r)  -> Just r
    Just (OpIndX r) -> Just r
    Just (OpIndY r) -> Just r
    _               -> Nothing -- For Imm, Implicit, Accumulator


-- --- Miscellaneous ---
-- | Extracts the low byte of a 16-bit word.
lsb :: Word16 -> Word8; lsb = fromIntegral . (.&. 0xFF)
-- | Extracts the high byte of a 16-bit word.
msb :: Word16 -> Word8; msb w = fromIntegral (w `shiftR` 8)
-- | Formats a 16-bit word as a hexadecimal string.
hx :: Word16 -> String; hx a = showHex a ""
-- | Converts a 16-bit word to a list of two bytes (little-endian).
wordToBytesLE :: Word16 -> [Word8]; wordToBytesLE w = [msb w, msb w]

-- | Evaluates a `LabelExpression` to a `Word16` address.
-- This is a top-level function for testing and should not be used during assembly if labels are unresolved.
evalLabelExpr :: LabelExpression -> Word16
evalLabelExpr = \case
  LabelRef l -> error $ "Compile-time error: Cannot get value of unresolved label '" ++ l ++ "' within expression"
  LabelParen expr -> evalLabelExpr expr
  LabelAdd subExpr offset -> evalLabelExpr subExpr + offset -- Use P.(+)
  LabelSub subExpr offset -> evalLabelExpr subExpr - offset -- Use P.(-)

-- | Converts an `AddressRef` to a `Word16` address.
-- Uses `evalLabelExpr` for label expressions, which can error if labels are unresolved.
addr2word16 :: AddressRef -> Word16
addr2word16 = \case
  AddrLit16 v -> v
  AddrLit8 v -> fromIntegral v
  AddrLabel l -> -- resolveAddressMaybe (AddrLabel l)
    -- TODO: Handle this case properly
    error $ "Compile-time error!!: Cannot get value of unresolved label '" ++ l ++ "'"
  AddrLabelExpr expr -> evalLabelExpr expr

-- | Converts an `AddressRef` to a `Word8` address (truncating if necessary).
addr2word8 :: AddressRef -> Word8
addr2word8 = \case
  AddrLit16 v -> fromIntegral v
  AddrLit8 v -> v
  AddrLabel l -> error $ "Compile-time error!!: Cannot get value of unresolved label '" ++ l ++ "'"
  AddrLabelExpr expr -> fromIntegral $ evalLabelExpr expr


-- | Wraps an `AddressRef` in parentheses if it's a `LabelExpression`.
parens :: AddressRef -> AddressRef
parens (AddrLabelExpr e) = AddrLabelExpr (LabelParen e)
parens addr = addr

-- | Converts a character to its ASCII code.
asc :: Char -> Word8
asc = fromIntegral . fromEnum

-- | Data type representing processor flag conditions for branching.
data Conditions =
    IsNonZero   -- ^ Z=0 (BNE) - Result of the last operation was not zero.
  | IsZero      -- ^ Z=1 (BEQ) - Result of the last operation was zero.
  | IsNonCarry  -- ^ C=0 (BCC) - There was no carry.
  | IsCarry     -- ^ C=1 (BCS) - There was a carry.
  | IsNegative  -- ^ N=1 (BMI) - Result was negative (most significant bit set).
  | IsPositive  -- ^ N=0 (BPL) - Result was positive or zero (most significant bit cleared).
  | IsNonOverflow -- ^ V=0 (BVC) - There was no overflow in signed arithmetic operation.
  | IsOverflow  -- ^ V=1 (BVS) - There was an overflow in signed arithmetic operation.
  deriving (Eq, Show)

-- | Function generating the appropriate conditional branch instruction.
branchOnCondition :: Conditions -> Label -> Asm ()
branchOnCondition IsNonZero target = emitBranch B_BNE target
branchOnCondition IsZero    target = emitBranch B_BEQ target
branchOnCondition IsNonCarry     target = emitBranch B_BCC target
branchOnCondition IsCarry        target = emitBranch B_BCS target
branchOnCondition IsNegative  target = emitBranch B_BMI target
branchOnCondition IsPositive  target = emitBranch B_BPL target
branchOnCondition IsNonOverflow   target = emitBranch B_BVC target
branchOnCondition IsOverflow     target = emitBranch B_BVS target

-- | Function inverting the condition.
invert :: Conditions -> Conditions
invert IsNonZero     = IsZero
invert IsZero        = IsNonZero
invert IsNonCarry    = IsCarry
invert IsCarry       = IsNonCarry
invert IsNegative    = IsPositive
invert IsPositive    = IsNegative
invert IsNonOverflow = IsOverflow
invert IsOverflow    = IsNonOverflow

-- | Optional pattern synonyms for readability (syntax sugar).
pattern AccIsZero      :: Conditions
pattern AccIsZero      = IsZero

pattern AccIsNonZero   :: Conditions
pattern AccIsNonZero   = IsNonZero

pattern AccIsPositive  :: Conditions
pattern AccIsPositive  = IsPositive

pattern AccIsNegative  :: Conditions
pattern AccIsNegative  = IsNegative
