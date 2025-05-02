{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
module MOS6502Emulator.Instructions where

import qualified MOS6502Emulator.Memory as Mem
import MOS6502Emulator.Registers
import MOS6502Emulator.Machine

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Control.Monad.State ( modify, modify') -- Import get and modify
import Control.Monad.IO.Class (liftIO) -- Import liftIO

import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe ( isJust )
import Numeric (showHex) -- Import showHex for debugging output

import MOS6502Emulator.DissAssembler (disassembleInstruction) -- Import disassembler

getReg :: (Registers -> a) -> FDX a
getReg f = f <$> getRegisters

-- setPC is a function that takes a Word16 and returns an action within the FDX monad. The () indicates the action doesn't produce a meaningful result value, but it performs some effect (like modifying state).
-- modify' works inside any monad m that is also an instance of MonadState managing state s. It takes a function (s -> s) that transforms the state. It returns an action m () within that specific monad m
-- within the body of setPC, the specific type of modify' becomes (Machine -> Machine) -> FDX ().
setPC :: Word16 -> FDX ()
setPC !pc = modify' $ \m -> m { mRegs = (mRegs m) { rPC = pc } } --new value for *mRegs* field is created by taking the original registers (mRegs m) and updating their *rPC* field
setAC :: Word8 -> FDX ()
setAC !ac = modify' $ \m -> m { mRegs = (mRegs m) { rAC = ac } }
setX :: Word8 -> FDX ()
setX !x = modify' $ \m -> m { mRegs = (mRegs m) { rX = x } }
setY :: Word8 -> FDX ()
setY !y = modify' $ \m -> m { mRegs = (mRegs m) { rY = y } }
setSR :: Word8 -> FDX ()
setSR !sr = modify' $ \m -> m { mRegs = (mRegs m) { rSR = sr } }
setSP :: Word8 -> FDX ()
setSP !sp = modify' $ \m -> m { mRegs = (mRegs m) { rSP = sp } }

addAC :: AddressMode -> FDX ()
addAC mode = do
  b <- fetchOperand mode
  ac <- getReg rAC
  c <- isFlagSet Carry
  let result =  (fromIntegral ac) + (fromIntegral b) + (if c then 1 else 0) :: Word16
  setFlag Carry (result > 0xFF) -- 9-bit overflow
  setFlag Zero ((result .&. 0xFF) == 0)
  setFlag Negative (testBit result 7)
  setFlag Overflow (not (testBit (ac `xor` b) 7) && testBit (ac `xor` (fromIntegral result)) 7) -- Overflow logic: (A^b7) && ~(ac^b7)
  setAC (fromIntegral result)


sbc :: AddressMode -> FDX ()
sbc mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  c <- isFlagSet Carry
  let result = (fromIntegral ac) - (fromIntegral b) - (if c then 0 else 1) :: Word16
  setFlag Carry (result > 0xFF) -- 9-bit overflow
  setFlag Zero ((result .&. 0xFF) == 0)
  setFlag Negative (testBit result 7)
  setFlag Overflow (testBit (ac `xor` b) 7 && testBit (ac `xor` (fromIntegral result)) 7) -- Overflow logic: (A^b7) && (ac^b7)
  setAC (fromIntegral result)   
  return $! fromIntegral result

andAC :: AddressMode -> FDX ()
andAC mode = do
  b <- fetchOperand mode
  modifyOperand Accumulator $ \ac -> do
    let result = ac .&. b
    setFlag Zero ((result .&. 0xFF) == 0)
    setFlag Negative (testBit result 7)
    return result

asl :: AddressMode -> FDX ()
asl mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 7)
  let result = shiftL b 1
  setFlag Zero ((result .&. 0xFF) == 0)
  setFlag Negative (testBit result 7)
  return result

lsr :: AddressMode -> FDX ()
lsr mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 0)
  setFlag Negative False
  let result = shiftR b 1
  setFlag Zero ((result .&. 0xFF) == 0)
  return result

dec :: AddressMode -> FDX ()
dec mode = modifyOperand mode $ \b -> do
  let result = b - 1
  setFlag Negative (testBit result 7)
  setFlag Zero ((result .&. 0xFF) == 0)
  return result

eor :: AddressMode -> FDX ()
eor mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  let result = ac `xor` b
  setFlag Negative (testBit result 7)
  setFlag Zero ((result .&. 0xFF) == 0)
  return result

ora :: AddressMode -> FDX ()
ora mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  let result = ac .|. b
  setFlag Negative (testBit result 7)
  setFlag Zero ((result .&. 0xFF) == 0)
  return result

rol :: AddressMode -> FDX ()
rol mode = modifyOperand mode $ \m -> do
  inputCarry <- isFlagSet Carry
  let outputCarry = testBit m 7
  let result = (m `shiftL` 1) .|. (if inputCarry then 0x01 else 0x00)
  setFlag Negative (testBit result 7)
  setFlag Zero ((result .&. 0xFF) == 0)
  setFlag Carry outputCarry
  return result

ror :: AddressMode -> FDX ()
ror mode = modifyOperand mode $ \m -> do
  inputCarry <- isFlagSet Carry 
  let outputCarry = testBit m 0
  let result = (m `shiftR` 1) .|. (if inputCarry then 0x80 else 0x00)
  setFlag Negative (testBit result 7)
  setFlag Zero ((result .&. 0xFF) == 0)
  setFlag Carry outputCarry
  return result

inc :: AddressMode -> FDX ()
inc mode = modifyOperand mode $ \b -> do
  let result = b + 1
  setFlag Negative (testBit result 7)
  setFlag Zero ((result .&. 0xFF) == 0)
  return result

data JMPType = AbsoluteJmp
             | IndirectJmp

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

jsr :: FDX ()
jsr = do
  aLsb <- fetchAndIncPC
  aMsb <- fetchAndIncPC
  let targetAddress = mkWord aLsb aMsb
  liftIO $ putStrLn $ "Jsr address: " ++ (showHex targetAddress "")

  currentPC <- getReg rPC

  -- Return address is already PC+2 after two fetchAndIncPC calls but stored return address 
  -- must be decremented because RTS will increment it: RTS -> pull PC, PC+1 -> PC
  let returnAddress = currentPC - 1
  liftIO $ putStrLn $ "Jsr return address: " ++ (showHex returnAddress "")

  pushWord returnAddress -- Push the correct return address (PC+2)
  setPC targetAddress -- Set PC to the target address

load :: AddressMode -> AddressMode -> FDX ()
load src dest = modifyOperand dest $ \_ -> do
  m <- fetchOperand src
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

store :: AddressMode -> AddressMode -> FDX ()
store src dest = modifyOperand dest (const (fetchOperand src))

branchOn :: FDX Bool -> FDX ()
branchOn test = do
  b <- fetchAndIncPC
  liftIO $ putStrLn $ "***branchOn operand: " ++ (showHex b "")
  c  <- test
  curPC <- getReg rPC
  let offset = fromIntegral b :: Int8 -- make it signed
  when c (setPC $ curPC + fromIntegral offset)


pull :: FDX Word8
pull = do
  sp <- getReg rSP
  let hi  = 0x0100 :: Word16
      sp' = hi + toWord sp + 1
  b <- fetchByteMem sp'
  setSP $! sp + 1
  return b

pushByte :: Word8 -> FDX ()
pushByte b = do
  sp <- getReg rSP
  let hi   = 0x0100 :: Word16
      addr = hi + toWord sp
  writeByteMem addr b
  setSP $! sp - 1

pushReg :: (Registers -> Word8) -> FDX ()
pushReg s = do
  r  <- getReg s
  sp <- getReg rSP
  let hi   = 0x0100 :: Word16
      addr = hi + toWord sp
  writeByteMem addr r
  setSP $! sp - 1

pushPC :: FDX ()
pushPC = do
  pc <- getReg rPC
  let pcl  = fromIntegral (pc .&. 0x00FF)
      pch  = fromIntegral (pc `shiftR` 8 .&. 0xFF) :: Word8 -- Correctly get high byte as Word8
  pushByte pch -- Push high byte
  pushByte pcl -- Push low byte

pushWord :: Word16 -> FDX ()
pushWord w = do
  let highByte = fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8
      lowByte  = fromIntegral (w .&. 0xFF) :: Word8
  pushByte highByte -- Push high byte
  pushByte lowByte  -- Push low byte

cmp :: (Registers -> Word8) -> AddressMode -> FDX ()
cmp s mode = do
  b <- fetchOperand mode
  r <- getReg s
  setFlag Carry (r >= b)
  setFlag Zero  (r == b)
  setFlag Negative (testBit (r - b) 7)

testBits :: AddressMode -> FDX ()
testBits mode = do
  b  <- fetchOperand mode
  ac <- getReg rAC
  let m7 = testBit b 7
      m6 = testBit b 6
  setFlag Negative m7
  setFlag Overflow m6
  setFlag Zero (ac .&. b == 0)

isFlagSet :: SRFlag -> FDX Bool
isFlagSet f = do
  rs <- getRegisters
  return (isJust (lookupSRFlag rs f))

setFlag :: SRFlag -> Bool -> FDX ()
setFlag f b = do
  rs <- getRegisters
  setRegisters $! setFlagBit rs b
 where
 setFlagBit rs True  = setSRFlag rs f
 setFlagBit rs False = clearSRFlag rs f

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


-- | Fetches the next byte from memory at PC, incrementing PC
fetchAndIncPC :: FDX Word8
fetchAndIncPC = do
  pc <- getReg rPC
  b <- fetchByteMem pc
  setPC (pc + 1)
  return b

-- | Fetches the next two bytes from memory at PC (little-endian), incrementing PC twice
fetchWordAndIncPC :: FDX Word16
fetchWordAndIncPC = do
  lo <- fetchAndIncPC
  hi <- fetchAndIncPC
  return $ mkWord lo hi

fetchOperand :: AddressMode -> FDX Word8
fetchOperand Immediate = fetchAndIncPC
fetchOperand Zeropage  = fetchAndIncPC >>= fetchByteMem . toWord
fetchOperand ZeropageX = do
  addrByte <- fetchAndIncPC
  x <- getReg rX
  fetchByteMem (toWord (addrByte + x))
fetchOperand ZeropageY = do
  addrByte <- fetchAndIncPC
  y <- getReg rY
  fetchByteMem (toWord (addrByte + y))
fetchOperand Absolute  = fetchWordAndIncPC >>= fetchByteMem
fetchOperand AbsoluteX = do
  w <- fetchWordAndIncPC
  x <- getReg rX
  fetchByteMem (w + toWord x)
fetchOperand AbsoluteY = do
  w <- fetchWordAndIncPC
  y <- getReg rY
  fetchByteMem (w + toWord y)
fetchOperand IndirectX = do
  b    <- fetchAndIncPC
  x    <- getReg rX
  addr <- fetchWordMem (b + x)
  fetchByteMem addr
fetchOperand IndirectY = do
  b    <- fetchAndIncPC
  addr <- fetchWordMem b
  y    <- getReg rY
  fetchByteMem (addr + toWord y)
fetchOperand Accumulator = getReg rAC
fetchOperand X           = getReg rX
fetchOperand Y           = getReg rY
fetchOperand SP          = getReg rSP

-- modifyOperand :: AddressMode -> (Word8 -> FDX Word8) -> FDX ()
--Takes an AddressMode (like Accumulator, Zeropage, Absolute, etc.) and a function.
--Then write value returned by function back to the correct location (memory or a register) based on the AddressMode.
--Function (Word8 -> FDX Word8) 
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
  x <- getReg rX
  let addr = toWord (zpAddr + x)
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

modifyOperand ZeropageY op = do
  zpAddr <- fetchAndIncPC
  y <- getReg rY
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
  x <- getReg rX
  let addr = base + toWord x
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

modifyOperand AbsoluteY op = do
  base <- fetchWordAndIncPC
  y <- getReg rY
  let addr = base + toWord y
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

modifyOperand IndirectX op = do
  zpAddr <- fetchAndIncPC
  x <- getReg rX
  addr <- fetchWordMem (zpAddr + x)
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

-- TODO: address incremented with carry
modifyOperand IndirectY op = do
  zpAddr <- fetchAndIncPC
  y <- getReg rY
  base <- fetchWordMem zpAddr
  let addr = base + toWord y
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'

-- this does not set the status register flags
-- for the accumulator, that's is up to the caller of
-- modifyOperand
modifyOperand Accumulator op = do
  ac  <- getReg rAC
  ac' <- op ac
  setAC ac'
modifyOperand X           op = do
  x  <- getReg rX
  x' <- op x
  setX x'
modifyOperand Y           op = do
  y  <- getReg rY
  y' <- op y
  setY y'
modifyOperand SP          op = do
  sp  <- getReg rSP
  sp' <- op sp
  setSP sp'

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

    0x90 -> branchOn (not <$> isFlagSet Carry)    -- BCC, Branch on Carry Clear
    0xB0 -> branchOn (isFlagSet Carry)    -- BCS, Branch On Carry Set
    0xF0 -> branchOn (isFlagSet Zero)    -- BEQ, Branch on Result Zero

    --  N Z C I D V
    -- M7 + - - - M6
    0x24 -> testBits Zeropage    -- BIT, Test Bits in Memory with Accumulator   -- A AND M, M7 -> N, M6 -> V
    0x2C -> testBits Absolute

    0x30 -> branchOn (isFlagSet Negative)    -- BMI, Branch on Result Minus
    0xD0 -> branchOn (not <$> isFlagSet Zero)    -- BNE, Branch on Result not Zero
    0x10 -> branchOn (not <$> isFlagSet Negative)    -- BPL, Branch on Resutl Plus


    -- BRK, Force Break
    0x00 -> do
      _ <- fetchOperand Immediate -- BRK consumes one byte operand (not used)
      pushPC -- Push PC+2 (or PC+1 depending on implementation detail, but +2 is common)
      pushReg rSR -- Push status register
      setFlag Interrupt True -- Set interrupt disable flag
      -- Load new PC from IRQ/BRK vector (0xFFFE/0xFFFF)
      ll <- fetchByteMem 0xFFFE
      hh <- fetchByteMem 0xFFFF
      let pc = mkWord ll hh
      setPC pc
      -- Signal that the machine should halt
      modify (\s -> s { halted = True })

    0x50 -> branchOn (not <$> isFlagSet Overflow)    -- BVC, Break on Overflow Clear
    0x70 -> branchOn (isFlagSet Overflow)    -- BVS, Branch on Overflow Set
    --  addAC IndirectY

    -- N Z C I D V
    -- - - 0 - - -
    0x18 -> setFlag Carry False    -- CLC, Clear Carry flag    -- 0 -> C

    -- N Z C I D V
    -- - - - - 0 -
    0xD8 -> setFlag Decimal False    -- CLD, Clear Decimal Mode    -- 0 -> D

    -- N Z C I D V
    -- - - - 0 - -
    0x58 -> setFlag Interrupt False    -- CLI, Clear Interrupt Disable Bit    -- 0 -> I

    -- N Z C I D V
    -- - - - - - 0
    0xB8 -> setFlag Overflow False    -- CLV, Clear Overflow Flag    -- 0 -> V

    -- N Z C I D V
    -- + + + - - -
    0xC9 -> cmp rAC Immediate    -- CMP, Compare Memory with Accumulator    -- A - M
    0xC5 -> cmp rAC Zeropage
    0xD5 -> cmp rAC ZeropageX
    0xCD -> cmp rAC Absolute
    0xDD -> cmp rAC AbsoluteX
    0xD9 -> cmp rAC AbsoluteY
    0xC1 -> cmp rAC IndirectX
    0xD1 -> cmp rAC IndirectY
    0xE0 -> cmp rX Immediate    -- CPX, Compare Memory and Index X    -- X - M
    0xE4 -> cmp rX Zeropage
    0xEC -> cmp rX Absolute
    0xC0 -> cmp rY Immediate    -- CPY, Compare Memory and Index Y    -- Y - M
    0xC4 -> cmp rY Zeropage
    0xCC -> cmp rY Absolute

    -- N Z C I D V
    -- + + - - - -
    0xC6 -> dec Zeropage    -- DEC, Decrement Memory by One    -- M - 1 -> M
    0xD6 -> dec ZeropageX
    0xCE -> dec Absolute
    0xDE -> dec AbsoluteX

    -- N Z C I D V
    -- + + - - - -
    0xCA -> dec X    -- DEX, Decrement Index X by One    -- X - 1 -> X
    0x88 -> dec Y    -- DEY, Decrement Index Y by One    -- Y - 1 -> Y

    -- N Z C I D V
    -- + + - - - -
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
    0xE6 -> inc Zeropage    -- INC, Increment Memory by One    -- M + 1 -> M
    0xF6 -> inc ZeropageX
    0xEE -> inc Absolute
    0xFE -> inc AbsoluteX

    -- N Z C I D V
    -- + + - - - -
    0xE8 -> inc X    -- INX, Increment Index X by One    -- X + 1 -> X
    0xC8 -> inc Y    -- INY, Increment Index Y by One    -- Y + 1 -> Y

    0x4C -> jmp AbsoluteJmp    -- JMP, Jump to New Location   -- (PC + 1) -> PCL, (PC + 2) -> PCH
    0x6C -> jmp IndirectJmp

    0x20 -> jsr    -- JSR, Jump to New Location Saving Return Address   -- push (PC + 2), (PC + 1) -> PCL, (PC + 2) -> PCH

    -- N Z C I D V
    -- + + - - - -
    0xA9 -> load Immediate Accumulator    -- LDA, Load Accumulator with Memory    -- M -> A
    0xA5 -> load Zeropage  Accumulator
    0xB5 -> load ZeropageX Accumulator
    0xAD -> load Absolute  Accumulator
    0xBD -> load AbsoluteX Accumulator
    0xB9 -> load AbsoluteY Accumulator
    0xA1 -> load IndirectX Accumulator
    0xB1 -> load IndirectY Accumulator
    0xA2 -> load Immediate X    -- LDX, Load Index X with Memory    -- M -> X
    0xA6 -> load Zeropage  X
    0xB6 -> load ZeropageY X
    0xAE -> load Absolute  X
    0xBE -> load AbsoluteY X
    0xA0 -> load Immediate Y    -- LDY, Load Index Y with Memory    -- M -> Y
    0xA4 -> load Zeropage  Y
    0xB4 -> load ZeropageX Y
    0xAC -> load Absolute  Y
    0xBC -> load AbsoluteX Y

    -- N Z C I D V
    -- - + + - - -
    0x4A -> lsr Accumulator    -- LSR, Shift One Bit Right (Memory or Accumulator)    -- 0 -> [76543210] -> C
    0x46 -> lsr Zeropage
    0x56 -> lsr ZeropageX
    0x4E -> lsr Absolute
    0x5E -> lsr AbsoluteX

    -- TODO: I believe this still needs to fetch the PC
    -- to get the cycle count right and not loop
    0xEA -> return ()    -- NOP, No Operation

    -- N Z C I D V
    -- + + - - - -
    0x09 -> ora Immediate    -- ORA, OR Memory with Accumulator    -- A OR M -> A
    0x05 -> ora Zeropage
    0x15 -> ora ZeropageX
    0x0D -> ora Absolute
    0x1D -> ora AbsoluteX
    0x19 -> ora AbsoluteY
    0x01 -> ora IndirectX
    0x11 -> ora IndirectY

    0x48 -> pushReg rAC    -- PHA, Push Accumulator on Stack    -- push A
    0x08 -> pushReg rSR    -- PHP, Push Processor Status on Stack    -- push SR

    -- N Z C I D V
    -- + + - - - -
    0x68 -> do    -- PLA, Pull Accumulator from Stack    -- pull A
      b <- pull
      setFlag Negative (testBit b 7)
      setFlag Zero     (b == 0)
      setAC b


    0x28 -> do    -- PLP, Pull Processor Status from Stack    -- pull SR
      b <- pull
      setSR b

    -- N Z C I D V
    -- + + + - - -
    0x2A -> rol Accumulator    -- ROL, Rotate One Bit Left (Memory or Accumulator)    -- C <- [76543210] <- C
    0x26 -> rol Zeropage
    0x36 -> rol ZeropageX
    0x2E -> rol Absolute
    0x3E -> rol AbsoluteX
    0x6A -> ror Accumulator    -- ROR, Rotate One Bit Right (Memory or Accumulator)    -- C -> [76543210] -> C
    0x66 -> ror Zeropage
    0x76 -> ror ZeropageX
    0x6E -> ror Absolute
    0x7E -> ror AbsoluteX

    0x40 -> do    -- RTI, Return from Interrupt    -- pull SR, pull PC
      sr  <- pull
      setSR sr
      pcl <- pull
      pch <- pull
      setPC (mkWord pcl pch)

    0x60 -> do    -- RTS, Return from Subroutine    -- pull PC, PC + 1 -> PC
      pcl <- pull
      pch <- pull
      setPC ((mkWord pcl pch) + 1)

    -- N Z C I D V
    -- + + + - - +
    0xE9 -> sbc Immediate    -- SBC, Subtract Memory from Accumulator with Borrow    -- A - M - C -> A
    0xE5 -> sbc Zeropage
    0xF5 -> sbc ZeropageX
    0xED -> sbc Absolute
    0xFD -> sbc AbsoluteX
    0xF9 -> sbc AbsoluteY
    0xE1 -> sbc IndirectX
    0xF1 -> sbc IndirectY

    0x38 -> setFlag Carry True        -- SEC, Set Carry Flag       -- 1 -> C
    0xF8 -> setFlag Decimal True      -- SED, Set Decimal Flag     -- 1 -> D
    0x78 -> setFlag Interrupt True    -- SEI, Set Interrupt Disable Status    -- 1 -> I

    0x85 -> store Accumulator Zeropage    -- STA, Store Accumulator in Memory    -- A -> M
    0x95 -> store Accumulator ZeropageX
    0x8D -> store Accumulator Absolute
    0x9D -> store Accumulator AbsoluteX
    0x99 -> store Accumulator AbsoluteY
    0x81 -> store Accumulator IndirectX
    0x91 -> store Accumulator IndirectY
    0x86 -> store X Zeropage    -- STX, Store Index X in Memory    -- X -> M
    0x96 -> store X ZeropageY
    0x8E -> store X Absolute
    0x84 -> store Y Zeropage    -- STY, Store Index Y in Memory    -- Y -> M
    0x94 -> store Y ZeropageX
    0x8C -> store Y Absolute

    -- N Z C I D V
    -- + + - - - -
    0xAA -> load Accumulator X    -- TAX, Transfer Accumulator to Index X    -- A -> X
    0xA8 -> load Accumulator Y    -- TAY, Transfer Accumulator to Index Yodes are nop    -- A -> Y
    0xBA -> load SP X             -- TSX, Transfer Stack Pointer to Index x    -- SP -> X
    0x8A -> load X Accumulator    -- TXA, Transfer Index X to Accumulator    -- X -> A
    0x9A -> load X SP    -- TXS, Transfer Index X to Stack Register    -- X -> SP
    0x98 -> load Y Accumulator    -- TYA -> Transfer Index Y to Accumulator    -- Y -> A

    -- TODO: all unimplemented opc    
    -- the correct thing would be to check
    -- their cycle counts
    _ -> do return () -- Instruction size in bytes
