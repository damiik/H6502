-- | Defines the 6502 instruction set and their execution logic within the emulator.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module MOS6502Emulator.Instructions where

import qualified MOS6502Emulator.Memory as Mem
import MOS6502Emulator.Registers hiding (_rPC, _rAC, _rX, _rY, _rSR, _rSP) -- Hide old record accessors
import MOS6502Emulator.Core
import Control.Lens -- Import Control.Lens
import qualified MOS6502Emulator.Lenses as L -- Import all lenses qualified

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Control.Monad.State ( MonadState, modify, modify', gets ) -- Import MonadState, modify, modify', and gets
import Control.Monad.IO.Class (liftIO) -- Import liftIO

import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe ( isJust )
import Numeric (showHex) -- Import showHex for debugging output

import MOS6502Emulator.DissAssembler (disassembleInstruction, formatHex16, formatHex8) -- Import disassembler


-- | Gets a specific register value from the machine state.
getReg :: MonadState Machine m => Getter Registers a -> m a
getReg l = gets (view (L.mRegs . l))

-- | Sets the Program Counter (PC) register.
setPC :: Word16 -> FDX ()
setPC !pc = modify' (L.mRegs . L.rPC .~ pc)

-- | Sets the Accumulator (AC) register.
setAC :: Word8 -> FDX ()
setAC !ac = modify' (L.mRegs . L.rAC .~ ac)

-- | Sets the X index register.
setX :: Word8 -> FDX ()
setX !x = modify' (L.mRegs . L.rX .~ x)

-- | Sets the Y index register.
setY :: Word8 -> FDX ()
setY !y = modify' (L.mRegs . L.rY .~ y)

-- | Sets the Status Register (SR).
setSR :: Word8 -> FDX ()
setSR !sr = modify' (L.mRegs . L.rSR .~ sr)

-- | Sets the Stack Pointer (SP) register.
setSP :: Word8 -> FDX ()
setSP !sp = modify' (L.mRegs . L.rSP .~ sp)

-- | Executes the ADC (Add with Carry) instruction.
addAC :: AddressMode -> FDX ()
addAC mode = do
  b <- fetchOperand mode
  ac_val <- getReg L.rAC
  c <- isFlagSet Carry
  let result =  (fromIntegral ac_val) + (fromIntegral b) + (if c then 1 else 0) :: Word16
  setFlag Carry (result > 0xFF) -- 9-bit overflow
  setFlag Zero (result .&. 0xFF == 0)
  setFlag Negative (testBit result 7)
  setFlag Overflow (not (testBit (ac_val `xor` b) 7) && testBit (ac_val `xor` (fromIntegral result)) 7) -- Overflow logic: (A^b7) && ~(ac^b7)
  setAC (fromIntegral result)

-- | Executes the SBC (Subtract with Borrow) instruction.
sbc :: AddressMode -> FDX ()
sbc mode = modifyOperand Accumulator $ \ac_val -> do
  b <- fetchOperand mode
  c <- isFlagSet Carry
  let result = (fromIntegral ac_val) - (fromIntegral b) - (if c then 0 else 1) :: Word16
  setFlag Carry (result < 0x00 || result > 0xFF) -- Carry is set if no borrow occurs (result >= 0)
  setFlag Zero (result .&. 0xFF == 0)
  setFlag Negative (testBit result 7)
  setFlag Overflow (testBit (ac_val `xor` b) 7 && testBit (ac_val `xor` (fromIntegral result)) 7) -- Overflow logic: (A^b7) && (ac^b7)
  setAC (fromIntegral result)
  return $! fromIntegral result

-- | Executes the AND (Logical AND) instruction.
andAC :: AddressMode -> FDX ()
andAC mode = do
  b <- fetchOperand mode
  modifyOperand Accumulator $ \ac_val -> do
    let result = ac_val .&. b
    setFlag Zero (result == 0)
    setFlag Negative (testBit result 7)
    return result

-- | Executes the ASL (Arithmetic Shift Left) instruction.
asl :: AddressMode -> FDX ()
asl mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 7)
  let result = shiftL b 1
  setFlag Zero (result .&. 0xFF == 0)
  setFlag Negative (testBit result 7)
  return result

-- | Executes the LSR (Logical Shift Right) instruction.
lsr :: AddressMode -> FDX ()
lsr mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 0)
  setFlag Negative False
  let result = shiftR b 1
  setFlag Zero (result .&. 0xFF == 0)
  return result

-- | Executes the DEC (Decrement Memory) instruction.
dec :: AddressMode -> FDX ()
dec mode = modifyOperand mode $ \b -> do
  let result = b - 1
  setFlag Negative (testBit result 7)
  setFlag Zero (result .&. 0xFF == 0)
  return result

-- | Executes the EOR (Exclusive OR) instruction.
eor :: AddressMode -> FDX ()
eor mode = modifyOperand Accumulator $ \ac_val -> do
  b <- fetchOperand mode
  let result = ac_val `xor` b
  setFlag Negative (testBit result 7)
  setFlag Zero (result == 0)
  return result

-- | Executes the ORA (Logical Inclusive OR) instruction.
ora :: AddressMode -> FDX ()
ora mode = modifyOperand Accumulator $ \ac_val -> do
  b <- fetchOperand mode
  let result = ac_val .|. b
  setFlag Negative (testBit result 7)
  setFlag Zero (result == 0)
  return result

-- | Executes the ROL (Rotate Left) instruction.
rol :: AddressMode -> FDX ()
rol mode = modifyOperand mode $ \m -> do
  inputCarry <- isFlagSet Carry
  let outputCarry = testBit m 7
  let result = (m `shiftL` 1) .|. (if inputCarry then 0x01 else 0x00)
  setFlag Negative (testBit result 7)
  setFlag Zero (result .&. 0xFF == 0)
  setFlag Carry outputCarry
  return result

-- | Executes the ROR (Rotate Right) instruction.
ror :: AddressMode -> FDX ()
ror mode = modifyOperand mode $ \m -> do
  inputCarry <- isFlagSet Carry
  let outputCarry = testBit m 0
  let result = (m `shiftR` 1) .|. (if inputCarry then 0x80 else 0x00)
  setFlag Negative (testBit result 7)
  setFlag Zero (result .&. 0xFF == 0)
  setFlag Carry outputCarry
  return result

-- | Executes the INC (Increment Memory) instruction.
inc :: AddressMode -> FDX ()
inc mode = modifyOperand mode $ \b -> do
  let result = b + 1
  setFlag Negative (testBit result 7)
  setFlag Zero (result .&. 0xFF == 0)
  return result

-- | Represents the types of JMP instructions.
data JMPType = AbsoluteJmp
             | IndirectJmp

-- | Executes the JMP (Jump) instruction.
jmp :: JMPType -> FDX ()
jmp AbsoluteJmp = do
  aLsb <- fetchAndIncPC
  aMsb <- fetchAndIncPC
  setPC $ mkWord aLsb aMsb

jmp IndirectJmp = do
  aLsb <- fetchAndIncPC
  aMsb <- fetchAndIncPC
  let addr = mkWord aLsb aMsb
  pcl  <- fetchByteMem addr
  pch  <- fetchByteMem (addr + 1)
  setPC (mkWord pcl pch)

-- | Executes the JSR (Jump to Subroutine) instruction.
jsr :: FDX ()
jsr = do
  aLsb <- fetchAndIncPC
  aMsb <- fetchAndIncPC
  let targetAddress = mkWord aLsb aMsb
  liftIO $ putStrLn $ "Jsr address: " ++ (formatHex16 targetAddress)

  currentPC <- getReg L.rPC

  -- Return address is already PC+2 after two fetchAndIncPC calls but stored return address
  -- must be decremented because RTS will increment it: RTS -> pull PC, PC+1 -> PC
  let returnAddress = currentPC - 1
  liftIO $ putStrLn $ "Jsr return address: " ++ (formatHex16 returnAddress)

  pushWord returnAddress -- Push the correct return address (PC+2)
  setPC targetAddress -- Set PC to the target address

-- | Executes a Load instruction (LDA, LDX, LDY).
load :: AddressMode -> AddressMode -> FDX ()
load src dest = modifyOperand dest $ \_ -> do
  m <- fetchOperand src
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

-- | Executes a Store instruction (STA, STX, STY).
store :: AddressMode -> AddressMode -> FDX ()
store src dest = modifyOperand dest (const (fetchOperand src))

-- | Executes a Branch instruction based on a test condition.
branchOn :: FDX Bool -> FDX ()
branchOn test = do
  b <- fetchAndIncPC
  liftIO $ putStrLn $ "***branchOn operand: " ++ (formatHex8 b)
  c  <- test
  curPC <- getReg L.rPC
  let offset = fromIntegral b :: Int8 -- make it signed
  when c (setPC $ curPC + fromIntegral offset)

-- | Pulls a byte from the stack, incrementing the stack pointer.
pull :: FDX Word8
pull = do
  sp_val <- getReg L.rSP
  let hi  = 0x0100 :: Word16
      sp' = hi + toWord sp_val + 1
  b <- fetchByteMem sp'
  setSP (sp_val + 1)
  return b

-- | Pushes a byte onto the stack, decrementing the stack pointer.
pushByte :: Word8 -> FDX ()
pushByte b = do
  sp_val <- getReg L.rSP
  let hi   = 0x0100 :: Word16
      addr = hi + toWord sp_val
  writeByteMem addr b
  setSP (sp_val - 1)

-- | Pushes the value of a register onto the stack.
pushReg :: Getter Registers Word8 -> FDX ()
pushReg l = do
  r_val  <- getReg l
  sp_val <- getReg L.rSP
  let hi   = 0x0100 :: Word16
      addr = hi + toWord sp_val
  writeByteMem addr r_val
  setSP (sp_val - 1)

-- | Pushes the current Program Counter (PC) onto the stack (high byte then low byte).
pushPC :: FDX ()
pushPC = do
  pc_val <- getReg L.rPC
  let pcl  = fromIntegral (pc_val .&. 0x00FF)
      pch  = fromIntegral (pc_val `shiftR` 8 .&. 0xFF) :: Word8 -- Correctly get high byte as Word8
  pushByte pch -- Push high byte
  pushByte pcl -- Push low byte

-- | Pushes a 16-bit word onto the stack (high byte then low byte).
pushWord :: Word16 -> FDX ()
pushWord w = do
  let highByte = fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8
      lowByte  = fromIntegral (w .&. 0xFF) :: Word8
  pushByte highByte -- Push high byte
  pushByte lowByte  -- Push low byte

-- | Executes a Compare instruction (CMP, CPX, CPY).
cmp :: Getter Registers Word8 -> AddressMode -> FDX ()
cmp l mode = do
  b <- fetchOperand mode
  r_val <- getReg l
  setFlag Carry (r_val >= b)
  setFlag Zero (r_val == b)
  setFlag Negative (testBit (r_val - b) 7)

-- | Executes the BIT (Test Bits) instruction.
testBits :: AddressMode -> FDX ()
testBits mode = do
  b  <- fetchOperand mode
  ac <- getReg L.rAC
  let m7 = testBit b 7
      m6 = testBit b 6
  setFlag Negative m7
  setFlag Overflow m6
  setFlag Zero (ac .&. b == 0)

-- | Checks if a specific Status Register flag is set.
isFlagSet :: SRFlag -> FDX Bool
isFlagSet f = do
  regs_val <- gets (view L.mRegs)
  return (isJust (lookupSRFlag regs_val f))

-- | Sets or clears a specific Status Register flag.
setFlag :: SRFlag -> Bool -> FDX ()
setFlag f b = do
  modify' (L.mRegs . L.rSR %~ (\sr_val -> 
    let sr_int = (fromIntegral sr_val :: Int)
        result = if b 
                then setBit sr_int (fromEnum f) 
                else clearBit sr_int (fromEnum f)
    in (fromIntegral result :: Word8)))

-- fetchByteAtPC :: FDX Word8
-- fetchByteAtPC = do
--   pc <- getReg rPC
--   -- Fetch the byte at the current PC, but don't increment the PC here.
--   -- The PC increment is handled in fdxSingleCycle after fetching the opcode,
--   -- and by fetchOperand and fetchWordAtPC for operands.
--   fetchByteMem pc

-- fetchByteAfterPC :: FDX Word8
-- fetchByteAfterPC = do
--   pc <- getReg rPC
--   -- Fetch the byte at the current PC, but don't increment the PC here.
--   -- The PC increment is handled in fdxSingleCycle after fetching the opcode,
--   -- and by fetchOperand and fetchWordAtPC for operands.
--   fetchByteMem (pc + 1)

-- fetchWordAtPC :: FDX Word16
-- fetchWordAtPC = do
--   pc <- getReg rPC
--   low <- fetchByteMem pc -- Fetch low byte at current PC
--   high <- fetchByteMem (pc + 1) -- Fetch high byte at current PC + 1
--   -- REMOVE PC increment here
--   return $! mkWord low high



-- Original fetchByteAtPC - replaced inline in fdxSingleCycle
-- fetchByteAtPC :: FDX Word8
-- fetchByteAtPC = do
--   pc  <- getReg rPC
--   setPC $! 1+pc
--   fetchByteMem pc


-- | Fetches the next byte from memory at PC, incrementing PC.
fetchAndIncPC :: FDX Word8
fetchAndIncPC = do
  pc_val <- getReg L.rPC
  b <- fetchByteMem pc_val
  setPC (pc_val + 1)
  -- The instructionCount and cycleCount fields need to be updated.
  -- Since they are now lenses, use the %~ operator for modification.
  modify' (L.instructionCount +~ 1)
  -- cycleCount will be handled by the specific instruction execution logic if applicable.
  return b

-- | Fetches the next two bytes from memory at PC (little-endian), incrementing PC twice.
fetchWordAndIncPC :: FDX Word16
fetchWordAndIncPC = do
  lo <- fetchAndIncPC
  hi <- fetchAndIncPC
  return $ mkWord lo hi

-- | Fetches the operand for the given addressing mode.
fetchOperand :: AddressMode -> FDX Word8
fetchOperand Immediate = fetchAndIncPC
fetchOperand Zeropage  = fetchAndIncPC >>= fetchByteMem . toWord
fetchOperand ZeropageX = do
  addrByte <- fetchAndIncPC
  x <- getReg L.rX
  fetchByteMem (toWord (addrByte + x))
fetchOperand ZeropageY = do
  addrByte <- fetchAndIncPC
  y <- getReg L.rY
  fetchByteMem (toWord (addrByte + y))
fetchOperand Absolute  = fetchWordAndIncPC >>= fetchByteMem
fetchOperand AbsoluteX = do
  w <- fetchWordAndIncPC
  x <- getReg L.rX
  fetchByteMem (w + toWord x)
fetchOperand AbsoluteY = do
  w <- fetchWordAndIncPC
  y <- getReg L.rY
  fetchByteMem (w + toWord y)
fetchOperand IndirectX = do
  b    <- fetchAndIncPC
  x <- getReg L.rX
  addr <- fetchWordMem (b + x)
  fetchByteMem addr
fetchOperand IndirectY = do
  b    <- fetchAndIncPC
  addr <- fetchWordMem b
  y <- getReg L.rY
  fetchByteMem (addr + toWord y)
fetchOperand Accumulator = getReg L.rAC
fetchOperand X           = getReg L.rX
fetchOperand Y           = getReg L.rY
fetchOperand SP          = getReg L.rSP

-- | Modifies the operand at the given addressing mode using the provided function.
-- Takes an AddressMode (like Accumulator, Zeropage, Absolute, etc.) and a function.
-- Then write value returned by function back to the correct location (memory or a register) based on the AddressMode.
-- Function (Word8 -> FDX Word8)
--  * takes the current value of the operand (fetched according to the AddressMode) as input.
--  * perform the desired operation (like shifting) and update status flags.
--  * must return the new value for the operand within the FDX monad.
modifyOperand :: AddressMode -> (Word8 -> FDX Word8) -> FDX ()
modifyOperand Immediate _ = return () -- Cannot write to immediate

modifyOperand Zeropage op = do
  zpAddr <- fetchAndIncPC
  v <- fetchByteMem (toWord zpAddr)
  v' <- op v
  writeByteMem (toWord zpAddr) v'

modifyOperand ZeropageX op = do
  zpAddr <- fetchAndIncPC
  x <- getReg L.rX
  let addr = toWord (zpAddr + x)
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

modifyOperand ZeropageY op = do
  zpAddr <- fetchAndIncPC
  y <- getReg L.rY
  let addr = toWord (zpAddr + y)
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

modifyOperand Absolute op = do
  addr <- fetchWordAndIncPC
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

modifyOperand AbsoluteX op = do
  base <- fetchWordAndIncPC
  x <- getReg L.rX
  let addr = base + toWord x
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

modifyOperand AbsoluteY op = do
  base <- fetchWordAndIncPC
  y <- getReg L.rY
  let addr = base + toWord y
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

modifyOperand IndirectX op = do
  zpAddr <- fetchAndIncPC
  x <- getReg L.rX
  addr <- fetchWordMem (zpAddr + x)
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

-- TODO: address incremented with carry
modifyOperand IndirectY op = do
  zpAddr <- fetchAndIncPC
  y <- getReg L.rY
  base <- fetchWordMem zpAddr
  let addr = base + toWord y
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

-- this does not set the status register flags
-- for the accumulator, that's is up to the caller of
-- modifyOperand
modifyOperand Accumulator op = do
  ac_val  <- getReg L.rAC
  ac'_val <- op ac_val
  setAC ac'_val
modifyOperand X           op = do
  x_val  <- getReg L.rX
  x'_val <- op x_val
  setX x'_val
modifyOperand Y           op = do
  y_val  <- getReg L.rY
  y'_val <- op y_val
  setY y'_val
modifyOperand SP          op = do
  sp_val  <- getReg L.rSP
  sp'_val <- op sp_val
  setSP sp'_val

-- | Executes a single 6502 instruction based on the provided opcode.
-- opcodes without WDC (Western Design Center) W65C02 Extensions
execute :: Word8 -> FDX ()
execute opc = do
  case opc of

    -- N Z C I D V
    -- + + + - - +
    0x69 -> addAC Immediate    -- ADC, Add Memory to Accumulator with Carry    -- A + M + C -> A, C
    0x65 -> addAC Zeropage
    0x75 -> addAC ZeropageX
    0x6D -> addAC Absolute
    0x7D -> addAC AbsoluteX
    0x79 -> addAC AbsoluteY
    0x61 -> addAC IndirectX
    0x71 -> addAC IndirectY

    -- N Z C I D V
    -- + + - - - -
    0x29 -> andAC Immediate    -- AND, AND Memory with Accumulator    -- A AND M -> A
    0x25 -> andAC Zeropage
    0x35 -> andAC ZeropageX
    0x2D -> andAC Absolute
    0x3D -> andAC AbsoluteX
    0x39 -> andAC AbsoluteY
    0x21 -> andAC IndirectX
    0x31 -> andAC IndirectY

    -- N Z C I D V
    -- + + + - - -
    0x0A -> asl Accumulator    -- ASL, Shift Left One Bit (Memory of Accumulator)    -- C <- [76543210] <- 0
    0x06 -> asl Zeropage
    0x16 -> asl ZeropageX
    0x0E -> asl Absolute
    0x1E -> asl AbsoluteX

    -- Branch on Carry Clear
    0x90 -> branchOn (not <$> isFlagSet Carry)    -- BCC, Branch on Carry Clear
    -- Branch On Carry Set
    0xB0 -> branchOn (isFlagSet Carry)    -- BCS, Branch On Carry Set
    -- Branch on Result Zero
    0xF0 -> branchOn (isFlagSet Zero)    -- BEQ, Branch on Result Zero

    --  N Z C I D V
    -- M7 + - - - M6
    -- Test Bits in Memory with Accumulator
    0x24 -> testBits Zeropage    -- BIT, Test Bits in Memory with Accumulator   -- A AND M, M7 -> N, M6 -> V
    0x2C -> testBits Absolute

    -- Branch on Result Minus
    0x30 -> branchOn (isFlagSet Negative)    -- BMI, Branch on Result Minus
    -- Branch on Result not Zero
    0xD0 -> branchOn (not <$> isFlagSet Zero)    -- BNE, Branch on Result not Zero
    -- Branch on Resutl Plus
    0x10 -> branchOn (not <$> isFlagSet Negative)    -- BPL, Branch on Resutl Plus


    -- BRK, Force Break
    0x00 -> do
      _ <- fetchOperand Immediate -- BRK consumes one byte operand (not used)
      pushPC -- Push PC+2 (or PC+1 depending on implementation detail, but +2 is common)
      pushReg L.rSR -- Push status register
      setFlag Interrupt True -- Set interrupt disable flag
      -- Load new PC from IRQ/BRK vector (0xFFFE/0xFFFF)
      ll <- fetchByteMem 0xFFFE
      hh <- fetchByteMem 0xFFFF
      let pc = mkWord ll hh
      setPC pc
      -- Signal that the machine should halt
      modify (L.halted .~ True)

    -- Break on Overflow Clear
    0x50 -> branchOn (not <$> isFlagSet Overflow)    -- BVC, Break on Overflow Clear
    -- Branch on Overflow Set
    0x70 -> branchOn (isFlagSet Overflow)    -- BVS, Branch on Overflow Set
    --  addAC IndirectY

    -- N Z C I D V
    -- - - 0 - - -
    -- CLC, Clear Carry flag
    0x18 -> setFlag Carry False    -- CLC, Clear Carry flag    -- 0 -> C

    -- N Z C I D V
    -- - - - - 0 -
    -- CLD, Clear Decimal Mode
    0xD8 -> setFlag Decimal False    -- CLD, Clear Decimal Mode    -- 0 -> D

    -- N Z C I D V
    -- - - - 0 - -
    -- CLI, Clear Interrupt Disable Bit
    0x58 -> setFlag Interrupt False    -- CLI, Clear Interrupt Disable Bit    -- 0 -> I

    -- N Z C I D V
    -- - - - - - 0
    -- CLV, Clear Overflow Flag
    0xB8 -> setFlag Overflow False    -- CLV, Clear Overflow Flag    -- 0 -> V

    -- N Z C I D V
    -- + + + - - -
    -- CMP, Compare Memory with Accumulator
    0xC9 -> cmp L.rAC Immediate    -- CMP, Compare Memory with Accumulator    -- A - M
    0xC5 -> cmp L.rAC Zeropage
    0xD5 -> cmp L.rAC ZeropageX
    0xCD -> cmp L.rAC Absolute
    0xDD -> cmp L.rAC AbsoluteX
    0xD9 -> cmp L.rAC AbsoluteY
    0xC1 -> cmp L.rAC IndirectX
    0xD1 -> cmp L.rAC IndirectY
    -- CPX, Compare Memory and Index X
    0xE0 -> cmp L.rX Immediate    -- CPX, Compare Memory and Index X    -- X - M
    0xE4 -> cmp L.rX Zeropage
    0xEC -> cmp L.rX Absolute
    -- CPY, Compare Memory and Index Y
    0xC0 -> cmp L.rY Immediate    -- CPY, Compare Memory and Index Y    -- Y - M
    0xC4 -> cmp L.rY Zeropage
    0xCC -> cmp L.rY Absolute

    -- N Z C I D V
    -- + + - - - -
    -- DEC, Decrement Memory by One
    0xC6 -> dec Zeropage    -- DEC, Decrement Memory by One    -- M - 1 -> M
    0xD6 -> dec ZeropageX
    0xCE -> dec Absolute
    0xDE -> dec AbsoluteX

    -- N Z C I D V
    -- + + - - - -
    -- DEX, Decrement Index X by One
    0xCA -> dec X    -- DEX, Decrement Index X by One    -- X - 1 -> X
    -- DEY, Decrement Index Y by One
    0x88 -> dec Y    -- DEY, Decrement Index Y by One    -- Y - 1 -> Y

    -- N Z C I D V
    -- + + - - - -
    -- EOR, Exclusive-OR Memory with Accumulator
    0x49 -> eor Immediate    -- EOR, Exclusive-OR Memory with Accumulator    -- A EOR M -> A
    0x45 -> eor Zeropage
    0x55 -> eor ZeropageX
    0x4D -> eor Absolute
    0x5D -> eor AbsoluteX
    0x59 -> eor AbsoluteY
    0x41 -> eor IndirectX
    0x51 -> eor IndirectY

    -- N Z C I D V
    -- + + - - - -
    -- INC, Increment Memory by One
    0xE6 -> inc Zeropage    -- INC, Increment Memory by One    -- M + 1 -> M
    0xF6 -> inc ZeropageX
    0xEE -> inc Absolute
    0xFE -> inc AbsoluteX

    -- N Z C I D V
    -- + + - - - -
    -- INX, Increment Index X by One
    0xE8 -> inc X    -- INX, Increment Index X by One    -- X + 1 -> X
    -- INY, Increment Index Y by One
    0xC8 -> inc Y    -- INY, Increment Index Y by One    -- Y + 1 -> Y

    -- JMP, Jump to New Location
    0x4C -> jmp AbsoluteJmp    -- JMP, Jump to New Location   -- (PC + 1) -> PCL, (PC + 2) -> PCH
    0x6C -> jmp IndirectJmp

    -- JSR, Jump to Subroutine
    0x20 -> jsr    -- JSR, Jump to New Location Saving Return Address   -- push (PC + 2), (PC + 1) -> PCL, (PC + 2) -> PCH

    -- N Z C I D V
    -- + + - - - -
    -- LDA, Load Accumulator with Memory
    0xA9 -> load Immediate Accumulator    -- LDA, Load Accumulator with Memory    -- M -> A
    0xA5 -> load Zeropage  Accumulator
    0xB5 -> load ZeropageX Accumulator
    0xAD -> load Absolute  Accumulator
    0xBD -> load AbsoluteX Accumulator
    0xB9 -> load AbsoluteY Accumulator
    0xA1 -> load IndirectX Accumulator
    0xB1 -> load IndirectY Accumulator
    -- LDX, Load Index X with Memory
    0xA2 -> load Immediate X    -- LDX, Load Index X with Memory    -- M -> X
    0xA6 -> load Zeropage  X
    0xB6 -> load ZeropageY X
    0xAE -> load Absolute  X
    0xBE -> load AbsoluteY X
    -- LDY, Load Index Y with Memory
    0xA0 -> load Immediate Y    -- LDY, Load Index Y with Memory    -- M -> Y
    0xA4 -> load Zeropage  Y
    0xB4 -> load ZeropageX Y
    0xAC -> load Absolute  Y
    0xBC -> load AbsoluteX Y

    -- N Z C I D V
    -- - + + - - -
    -- LSR, Shift One Bit Right (Memory or Accumulator)
    0x4A -> lsr Accumulator    -- LSR, Shift One Bit Right (Memory or Accumulator)    -- 0 -> [76543210] -> C
    0x46 -> lsr Zeropage
    0x56 -> lsr ZeropageX
    0x4E -> lsr Absolute
    0x5E -> lsr AbsoluteX

    -- NOP, No Operation
    -- TODO: I believe this still needs to fetch the PC
    -- to get the cycle count right and not loop
    0xEA -> return ()    -- NOP, No Operation

    -- N Z C I D V
    -- + + - - - -
    -- ORA, OR Memory with Accumulator
    0x09 -> ora Immediate    -- ORA, OR Memory with Accumulator    -- A OR M -> A
    0x05 -> ora Zeropage
    0x15 -> ora ZeropageX
    0x0D -> ora Absolute
    0x1D -> ora AbsoluteX
    0x19 -> ora AbsoluteY
    0x01 -> ora IndirectX
    0x11 -> ora IndirectY

    -- PHA, Push Accumulator on Stack
    0x48 -> pushReg L.rAC    -- PHA, Push Accumulator on Stack    -- push A
    -- PHP, Push Processor Status on Stack
    0x08 -> pushReg L.rSR    -- PHP, Push Processor Status on Stack    -- push SR

    -- N Z C I D V
    -- + + - - - -
    -- PLA, Pull Accumulator from Stack
    0x68 -> do    -- PLA, Pull Accumulator from Stack    -- pull A
      b <- pull
      setFlag Negative (testBit b 7)
      setFlag Zero     (b == 0)
      setAC b

    -- PLP, Pull Processor Status from Stack
    0x28 -> do    -- PLP, Pull Processor Status from Stack    -- pull SR
      b <- pull
      setSR b

    -- N Z C I D V
    -- + + + - - -
    -- ROL, Rotate One Bit Left (Memory or Accumulator)
    0x2A -> rol Accumulator    -- ROL, Rotate One Bit Left (Memory or Accumulator)    -- C <- [76543210] <- C
    0x26 -> rol Zeropage
    0x36 -> rol ZeropageX
    0x2E -> rol Absolute
    0x3E -> rol AbsoluteX
    -- ROR, Rotate One Bit Right (Memory or Accumulator)
    0x6A -> ror Accumulator    -- ROR, Rotate One Bit Right (Memory or Accumulator)    -- C -> [76543210] -> C
    0x66 -> ror Zeropage
    0x76 -> ror ZeropageX
    0x6E -> ror Absolute
    0x7E -> ror AbsoluteX

    -- RTI, Return from Interrupt
    0x40 -> do    -- RTI, Return from Interrupt    -- pull SR, pull PC
      sr  <- pull
      setSR sr
      pcl <- pull
      pch <- pull
      setPC (mkWord pcl pch)

    -- RTS, Return from Subroutine
    0x60 -> do    -- RTS, Return from Subroutine    -- pull PC, PC + 1 -> PC
      pcl <- pull
      pch <- pull
      setPC ((mkWord pcl pch) + 1)

    -- N Z C I D V
    -- + + + - - +
    -- SBC, Subtract Memory from Accumulator with Borrow
    0xE9 -> sbc Immediate    -- SBC, Subtract Memory from Accumulator with Borrow    -- A - M - C -> A
    0xE5 -> sbc Zeropage
    0xF5 -> sbc ZeropageX
    0xED -> sbc Absolute
    0xFD -> sbc AbsoluteX
    0xF9 -> sbc AbsoluteY
    0xE1 -> sbc IndirectX
    0xF1 -> sbc IndirectY

    -- SEC, Set Carry Flag
    0x38 -> setFlag Carry True        -- SEC, Set Carry Flag       -- 1 -> C
    -- SED, Set Decimal Flag
    0xF8 -> setFlag Decimal True      -- SED, Set Decimal Flag     -- 1 -> D
    -- SEI, Set Interrupt Disable Status
    0x78 -> setFlag Interrupt True    -- SEI, Set Interrupt Disable Status    -- 1 -> I

    -- STA, Store Accumulator in Memory
    0x85 -> store Accumulator Zeropage    -- STA, Store Accumulator in Memory    -- A -> M
    0x95 -> store Accumulator ZeropageX
    0x8D -> store Accumulator Absolute
    0x9D -> store Accumulator AbsoluteX
    0x99 -> store Accumulator AbsoluteY
    0x81 -> store Accumulator IndirectX
    0x91 -> store Accumulator IndirectY
    -- STX, Store Index X in Memory
    0x86 -> store X Zeropage    -- STX, Store Index X in Memory    -- X -> M
    0x96 -> store X ZeropageY
    0x8E -> store X Absolute
    -- STY, Store Index Y in Memory
    0x84 -> store Y Zeropage    -- STY, Store Index Y in Memory    -- Y -> M
    0x94 -> store Y ZeropageX
    0x8C -> store Y Absolute

    -- N Z C I D V
    -- + + - - - -
    -- TAX, Transfer Accumulator to Index X
    0xAA -> load Accumulator X    -- TAX, Transfer Accumulator to Index X    -- A -> X
    -- TAY, Transfer Accumulator to Index Y
    0xA8 -> load Accumulator Y    -- TAY, Transfer Accumulator to Index Yodes are nop    -- A -> Y
    -- TSX, Transfer Stack Pointer to Index x
    0xBA -> load SP X             -- TSX, Transfer Stack Pointer to Index x    -- SP -> X
    -- TXA, Transfer Index X to Accumulator
    0x8A -> load X Accumulator    -- TXA, Transfer Index X to Accumulator    -- X -> A
    -- TXS, Transfer Index X to Stack Register
    0x9A -> load X SP    -- TXS, Transfer Index X to Stack Register    -- X -> SP
    -- TYA -> Transfer Index Y to Accumulator
    0x98 -> load Y Accumulator    -- TYA -> Transfer Index Y to Accumulator    -- Y -> A

    -- TODO: all unimplemented opc
    -- the correct thing would be to check
    -- their cycle counts
    _ -> do return () -- Instruction size in bytes
