{-# LANGUAGE BangPatterns #-}
module MOS6502Emulator.Instructions where

import qualified MOS6502Emulator.Memory as Mem
import MOS6502Emulator.Registers
import MOS6502Emulator.Machine

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Control.Monad.State (get, modify) -- Import get and modify
import Control.Monad.IO.Class (liftIO) -- Import liftIO

import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe ( isJust )
import Numeric (showHex) -- Import showHex for debugging output

getReg :: (Registers -> a) -> FDX a
getReg f = do
  rs <- getRegisters
  return $! f rs

setPC :: Word16 -> FDX ()
setPC !pc = do
  rs <- getRegisters
  setRegisters $! rs { rPC = pc }

setAC :: Word8 -> FDX ()
setAC !w = do
  rs <- getRegisters
  setRegisters $! rs { rAC = w }

setSP :: Word8 -> FDX ()
setSP !w = do
  rs <- getRegisters
  setRegisters $! rs { rSP = w }

setSR :: Word8 -> FDX ()
setSR !w = do
  rs <- getRegisters
  setRegisters $! rs { rSR = w }

setX :: Word8 -> FDX ()
setX !w = do
  rs <- getRegisters
  setRegisters $! rs { rX = w }

setY :: Word8 -> FDX ()
setY !w = do
  rs <- getRegisters
  setRegisters $! rs { rY = w }

addAC :: AddressMode -> FDX ()
-- TODO: update status register
-- TODO: add carry bit
addAC mode = do
  b <- fetchOperand mode
  modifyOperand Accumulator $ \ac -> do
    return $! ac + b

sbc :: AddressMode -> FDX ()
sbc mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  c <- isFlagSet Carry
  -- TODO: update status register
  return $! ac - b - (toBit c)
 where
 toBit True  = 1
 toBit False = 0

andAC :: AddressMode -> FDX ()
-- TODO: update status register
andAC mode = do
  b <- fetchOperand mode
  modifyOperand Accumulator $ \ac -> do
    return $! ac .&. b

asl :: AddressMode -> FDX ()
-- TODO: update status register
asl mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 7)
  return $! (b `shiftL` 1)

lsr :: AddressMode -> FDX ()
-- TODO: update status register
lsr mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 7)
  -- TODO: is this the right shiftR?
  return $! (b `shiftR` 1)

dec :: AddressMode -> FDX ()
dec mode = modifyOperand mode $ \b -> do
  let m = b - 1
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

eor :: AddressMode -> FDX ()
eor mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  let m = ac `xor` b
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

ora :: AddressMode -> FDX ()
ora mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  let m = ac .|. b
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

rol :: AddressMode -> FDX ()
rol mode = modifyOperand mode $ \m -> do
  let m' = m `rotateL` 1
  -- TODO: update status registers and check
  -- that this is the right rotate
  return m'

ror :: AddressMode -> FDX ()
ror mode = modifyOperand mode $ \m -> do
  let m' = m `rotateR` 1
  -- TODO: update status registers and check
  -- that this is the right rotate
  return m'

inc :: AddressMode -> FDX ()
inc mode = modifyOperand mode $ \b -> do
  let m = b + 1
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

data JMPType = AbsoluteJmp
             | IndirectJmp

jmp :: JMPType -> FDX ()
-- TODO: no idea if either of these are correct
jmp AbsoluteJmp = fetchWordAtPC >>= setPC
jmp IndirectJmp = do
  addr <- fetchWordAtPC
  pcl  <- fetchByteMem addr
  pch  <- fetchByteMem (addr + 1)
  setPC (mkWord pcl pch)

-- TODO: I think this is right
jsr :: FDX ()
jsr = do
  currentPC <- getReg rPC
  setPC $ currentPC + 1
  targetAddress <- fetchWordAtPC

  liftIO $ putStrLn $ "Jsr address: " ++ (showHex targetAddress "")
  liftIO $ putStrLn $ "Jsr return address: " ++ (showHex (currentPC + 3) "")

  pushWord $ currentPC + 3 -- Push the return address
  setPC (targetAddress - 3) -- Set PC to the target address - 3, after *jsr* PC is incremented by 3

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
  currentPC <- getReg rPC
  -- setPC $ currentPC + 1
  b <- fetchByteMem (currentPC + 1)
  -- b <- fetchByteAtPC
  liftIO $ putStrLn $ "***branchOn operand: " ++ (showHex b "")
  c  <- test
  let offset = fromIntegral b :: Int8 -- make it signed
  when c (setPC $ currentPC + fromIntegral offset)


  

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

-- | Fetches a byte from the provided
-- address.
fetchByteMem :: Word16 -> FDX Word8
fetchByteMem addr = do
  mem <- getMemory
  Mem.fetchByte addr mem

-- | Fetches a word located at an address
-- stored in the zero page. That means
-- we only need an 8bit address, but we
-- also read address+1
fetchWordMem :: Word8 -> FDX Word16
fetchWordMem addr = do
  mem <- getMemory
  lo  <- Mem.fetchByte (toWord addr)     mem
  hi  <- Mem.fetchByte (toWord (addr+1)) mem
  return $! mkWord lo hi

writeByteMem :: Word16 -> Word8 -> FDX ()
writeByteMem addr b = do
  mem <- getMemory
  Mem.writeByte addr b mem

fetchByteAtPC :: FDX Word8
fetchByteAtPC = do
  pc <- getReg rPC
  -- Fetch the byte at the current PC, but don't increment the PC here.
  -- The PC increment is handled in fdxSingleCycle after fetching the opcode,
  -- and by fetchOperand and fetchWordAtPC for operands.
  fetchByteMem pc

fetchByteAfterPC :: FDX Word8
fetchByteAfterPC = do
  pc <- getReg rPC
  -- Fetch the byte at the current PC, but don't increment the PC here.
  -- The PC increment is handled in fdxSingleCycle after fetching the opcode,
  -- and by fetchOperand and fetchWordAtPC for operands.
  fetchByteMem (pc + 1)

fetchWordAtPC :: FDX Word16
fetchWordAtPC = do
  pc <- getReg rPC
  low <- fetchByteMem pc -- Fetch low byte at current PC
  high <- fetchByteMem (pc + 1) -- Fetch high byte at current PC + 1
  -- REMOVE PC increment here
  return $! mkWord low high

-- In this context, "Word" in an identifier name
-- means a machine word on the 6502 which is 16 bits.
-- Not to be confused with the Haskell Word type
mkWord :: Word8  -- ^ low byte
       -> Word8  -- ^ high byte
       -> Word16
mkWord !lb !hb = (hw `shiftL` 8) + lw
  where
  lw = toWord lb
  hw = toWord hb

toWord :: Word8 -> Word16
toWord = fromIntegral

fdxSingleCycle :: FDX Bool -- Returns True if emulation should continue, False if halted
fdxSingleCycle = do
  -- liftIO $ putStrLn ""
  machineState <- get
  -- liftIO $ putStrLn $ "Current PC at start of fdxSingleCycle: $" ++ showHex (rPC (mRegs machineState)) ""
  if halted machineState
    then return False -- Machine is halted, stop emulation
    else do
      -- liftIO $ putStrLn "Fetching instruction..."
      pc <- getReg rPC  -- Get current PC
      b <- fetchByteMem pc -- Fetch byte at current PC
      -- REMOVE setPC (pc + 1) here

      -- Increment instruction count
      modify (\s -> s { instructionCount = instructionCount s + 1 })
      -- liftIO $ putStrLn $ "Executing opcode: " ++ show b
      -- TODO: Add cycle count increment based on opcode execution
      instructionSize <- execute b -- execute now returns instruction size
      -- liftIO $ putStrLn "Instruction executed."
      -- Increment PC by the instruction size after execution
      pcAfterExecution <- getReg rPC -- Get PC after execute might have changed it (e.g. JMP, JSR)
      setPC (pcAfterExecution + fromIntegral instructionSize)

      machineState' <- get
      return (not (halted machineState')) -- Continue if not halted after execution

-- Original fetchByteAtPC - replaced inline in fdxSingleCycle
-- fetchByteAtPC :: FDX Word8
-- fetchByteAtPC = do
--   pc  <- getReg rPC
--   setPC $! 1+pc
--   fetchByteMem pc


fetchOperand :: AddressMode -> FDX Word8
-- Immediate mode fetches the byte at the current PC and increments the PC.
-- This is handled by fdxSingleCycle for the opcode fetch.
-- For immediate operands after the opcode, we need a way to fetch the *next* byte.
-- Let's create a helper for fetching the next byte and incrementing PC.
fetchOperand Immediate = do
    pc <- getReg rPC
    b <- fetchByteMem pc
    -- REMOVE setPC (pc + 1)
    return b

fetchOperand Zeropage  = do
  addrByte <- fetchByteAtPC
  -- REMOVE setPC (pc + 1)
  fetchByteMem (toWord addrByte)
fetchOperand ZeropageX = do
  addrByte <- fetchByteAtPC
  -- REMOVE setPC (pc + 1)
  x <- getReg rX
  fetchByteMem (toWord (addrByte + x)) -- stay on the zeropage but add x
fetchOperand ZeropageY = do
  addrByte <- fetchByteAtPC
  -- REMOVE setPC (pc + 1)
  y <- getReg rY
  fetchByteMem (toWord (addrByte + y)) -- stay on the zeropage but add y
fetchOperand Absolute  = fetchWordAtPC >>= fetchByteMem
fetchOperand AbsoluteX = do
  w <- fetchWordAtPC
  x <- getReg rX
  fetchByteMem (w + (toWord x))
fetchOperand AbsoluteY = do
  w <- fetchWordAtPC
  y <- getReg rY
  fetchByteMem (w + (toWord y))
fetchOperand IndirectX = do
  b    <- fetchOperand Immediate
  x    <- getReg rX
  addr <- fetchWordMem (b + x) -- zeropage index plus x
  fetchByteMem addr
fetchOperand IndirectY = do
  -- In this case, we add the value in Y to the address pointed
  -- to by the zeropage address, and then fetch the byte there.
  b    <- fetchOperand Immediate
  addr <- fetchWordMem b
  y    <- getReg rY
  fetchByteMem (addr + toWord y)
fetchOperand Accumulator = getReg rAC
fetchOperand X           = getReg rX
fetchOperand Y           = getReg rY
fetchOperand SP          = getReg rSP

modifyOperand :: AddressMode -> (Word8 -> FDX Word8) -> FDX ()
modifyOperand Immediate _ = return () -- TODO: should this be an error?
modifyOperand Zeropage op = do
  b  <- fetchByteAtPC
  b' <- op b
  writeByteMem (toWord b) b'
modifyOperand ZeropageX op = do
  b <- fetchByteAtPC
  x <- getReg rX
  let addr = toWord (b + x) -- stay on the zeropage but add x
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'
modifyOperand ZeropageY op = do
  b <- fetchByteAtPC
  y <- getReg rY
  let addr = toWord (b + y) -- stay on the zeropage but add y
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'
modifyOperand Absolute  op = do
  addr <- fetchWordAtPC
  b  <- fetchByteMem addr
  b' <- op b
  writeByteMem addr b'
modifyOperand AbsoluteX op = do
  w <- fetchWordAtPC
  x <- getReg rX
  -- TODO: what does it mean to increment the address with carry?
  -- I think it means that you convert x to 16 bit and then add
  let addr = w + toWord x
  b  <- fetchByteMem addr
  b' <- op b
  writeByteMem addr b'
modifyOperand AbsoluteY op = do
  w <- fetchWordAtPC
  y <- getReg rY
  -- TODO: what does it mean to increment the address with carry?
  -- I think it means that you convert y to 16 bit and then add
  let addr = w + toWord y
  b  <- fetchByteMem addr
  b' <- op b
  writeByteMem addr b' -- Corrected typo here (was v')
modifyOperand IndirectX op = do
  b <- fetchByteAtPC
  x <- getReg rX
  let zeroPageAddr = b + x -- zeropage indexed by b + x
  addr <- fetchWordMem zeroPageAddr
  v    <- fetchByteMem addr
  v'   <- op v
  writeByteMem addr v'
modifyOperand IndirectY op = do
  b <- fetchByteAtPC
  y <- getReg rY
  v1 <- fetchWordMem b -- zeropage indexed by b, add y to result
  let addr = v1 + toWord y
  v  <- fetchByteMem addr
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

execute :: Word8 -> FDX Int
execute opc = do
  pc' <- getReg rPC
  liftIO $ putStrLn $ "Executing opcode $" ++ showHex opc "" ++ " at PC $" ++ showHex pc' ""
  case opc of
    -- ADC, Add Memory to Accumulator with Carry
    -- A + M + C -> A, C
    -- N Z C I D V
    -- + + + - - +
    --
    0x69 -> do
      addAC Immediate
      return 2 -- Instruction size in bytes
    0x65 -> do
      addAC Zeropage
      return 2 -- Instruction size in bytes
    0x75 -> do
      addAC ZeropageX
      return 2 -- Instruction size in bytes
    0x6D -> do
      addAC Absolute
      return 3 -- Instruction size in bytes
    0x7D -> do
      addAC AbsoluteX
      return 3 -- Instruction size in bytes
    0x79 -> do
      addAC AbsoluteY
      return 3 -- Instruction size in bytes
    0x61 -> do
      addAC IndirectX
      return 2 -- Instruction size in bytes
    0x71 -> do
      addAC IndirectY
      return 2 -- Instruction size in bytes
    -- AND, AND Memory with Accumulator
    -- A AND M -> A
    -- N Z C I D V
    -- + + - - - -
    0x29 -> do
      andAC Immediate
      return 2 -- Instruction size in bytes

    0x25 -> do
      andAC Zeropage
      return 2 -- Instruction size in bytes
    0x35 -> do
      andAC ZeropageX
      return 2 -- Instruction size in bytes

    0x2D -> do
      andAC Absolute
      return 3 -- Instruction size in bytes

    0x3D -> do
      andAC AbsoluteX
      return 3 -- Instruction size in bytes

    0x39 -> do
      andAC AbsoluteY
      return 3 -- Instruction size in bytes

    0x21 -> do
      andAC IndirectX
      return 2 -- Instruction size in bytes

    0x31 -> do
      andAC IndirectY
      return 2 -- Instruction size in bytes

    -- ASL, Shift Left One Bit (Memory of Accumulator)
    -- C <- [76543210] <- 0
    -- N Z C I D V
    -- + + + - - -
    0x0A -> do
      asl Accumulator
      return 1 -- Instruction size in bytes

    0x06 -> do
      asl Zeropage
      return 2 -- Instruction size in bytes

    0x16 -> do
      asl ZeropageX
      return 2 -- Instruction size in bytes

    0x0E -> do
      asl Absolute
      return 3 -- Instruction size in bytes

    0x1E -> do
      asl AbsoluteX
      return 3 -- Instruction size in bytes

    -- BCC, Branch on Carry Clear
    -- branch if C = 0
    -- N Z C I D V
    -- - - - - - -
    0x90 -> do
      branchOn (not <$> isFlagSet Carry)
      return 2   -- Instruction size in bytes

    -- BCS, Branch On Carry Set
    -- branch if C = 1
    -- N Z C I D V
    -- - - - - - -
    0xB0 -> do
      branchOn (isFlagSet Carry)
      return 2 -- Instruction size in bytes

    -- BEQ, Branch on Result Zero
    -- branch if Z = 1
    -- N Z C I D V
    -- - - - - - -
    0xF0 -> do
      branchOn (isFlagSet Zero)
      return 2 -- Instruction size in bytes

    -- BIT, Test Bits in Memory with Accumulator
    -- A AND M, M7 -> N, M6 -> V
    --  N Z C I D V
    -- M7 + - - - M6
    -- TODO: wikibooks listed a 3rd op code for BIT
    -- why is that?
    0x24 -> do
      testBits Zeropage
      return 2 -- Instruction size in bytes

    0x2C -> do
      testBits Absolute
      return 3 -- Instruction size in bytes

    -- BMI, Branch on Result Minus
    -- branch if N = 1
    -- N Z C I D V
    -- - - - - - -
    0x30 -> do
      branchOn (isFlagSet Negative)
      return 2 -- Instruction size in bytes

    -- BNE, Branch on Result not Zero
    -- branch if Z = 0
    -- N Z C I D V
    -- - - - - - -
    0xD0 -> do
      branchOn (not <$> isFlagSet Zero)
      return 2 -- Instruction size in bytes


    -- BPL, Branch on Resutl Plus
    -- branch if N = 0
    -- N Z C I D V
    -- - - - - - -
    0x10 -> do
      branchOn (not <$> isFlagSet Negative)
      return 2 -- Instruction size in bytes


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
      return 1 -- Instruction size in bytes
    -- BVC, Break on Overflow Clear
    -- branch if V = 0
    -- N Z C I D V
    -- - - - - - -
    0x50 -> do
      branchOn (not <$> isFlagSet Overflow)
      return 2 -- Instruction size in bytes

    -- BVS, Branch on Overflow Set
    -- branch if V = 1
    -- N Z C I D V
    -- - - - - - -
    0x70 -> do
      branchOn (isFlagSet Overflow)
      return 2 -- Instruction size in bytes

    -- CLC, Clear Carry flag
    -- 0 -> C
    -- N Z C I D V
    -- - - 0 - - -
    0x18 -> do
      setFlag Carry False
      return 1 -- Instruction size in bytes

    -- CLD, Clear Decimal Mode
    -- 0 -> D
    -- N Z C I D V
    -- - - - - 0 -
    0xD8 -> do
      setFlag Decimal False
      return 1 -- Instruction size in bytes

    -- CLI, Clear Interrupt Disable Bit
    -- 0 -> I
    -- N Z C I D V
    -- - - - 0 - -
    0x58 -> do
      setFlag Interrupt False
      return 1 -- Instruction size in bytes

    -- CLV, Clear Overflow Flag
    -- 0 -> V
    -- N Z C I D V
    -- - - - - - 0
    0xB8 -> do
      setFlag Overflow False
      return 1 -- Instruction size in bytes

    -- CMP, Compare Memory with Accumulator
    -- A - M
    -- N Z C I D V
    -- + + + - - -
    0xC9 -> do
      cmp rAC Immediate
      return 2 -- Instruction size in bytes

    0xC5 -> do
      cmp rAC Zeropage
      return 2 -- Instruction size in bytes

    0xD5 -> do
      cmp rAC ZeropageX
      return 2 -- Instruction size in bytes

    0xCD -> do
      cmp rAC Absolute
      return 3 -- Instruction size in bytes

    0xDD -> do
      cmp rAC AbsoluteX
      return 3 -- Instruction size in bytes

    0xD9 -> do
      cmp rAC AbsoluteY
      return 3 -- Instruction size in bytes

    0xC1 -> do
      cmp rAC IndirectX
      return 2 -- Instruction size in bytes

    0xD1 -> do
      cmp rAC IndirectY
      return 2 -- Instruction size in bytes

    -- CPX, Compare Memory and Index X
    -- X - M
    -- N Z C I D V
    -- + + + - - -
    0xE0 -> do
      cmp rX Immediate
      return 2 -- Instruction size in bytes

    0xE4 -> do
      cmp rX Zeropage
      return 2 -- Instruction size in bytes

    0xEC -> do
      cmp rX Absolute
      return 3 -- Instruction size in bytes

    -- CPY, Compare Memory and Index Y
    -- Y - M
    -- N Z C I D V
    -- + + + - - -
    0xC0 -> do
      cmp rY Immediate
      return 2 -- Instruction size in bytes

    0xC4 -> do
      cmp rY Zeropage
      return 2 -- Instruction size in bytes

    0xCC -> do
      cmp rY Absolute
      return 3 -- Instruction size in bytes

    -- DEC, Decrement Memory by One
    -- M - 1 -> M
    -- N Z C I D V
    -- + + - - - -
    0xC6 -> do
      dec Zeropage
      return 2 -- Instruction size in bytes

    0xD6 -> do
      dec ZeropageX
      return 2 -- Instruction size in bytes

    0xCE -> do
      dec Absolute
      return 3 -- Instruction size in bytes

    0xDE -> do
      dec AbsoluteX
      return 3 -- Instruction size in bytes

    -- DEX, Decrement Index X by One
    -- X - 1 -> X
    -- N Z C I D V
    -- + + - - - -
    0xCA -> do
      dec X
      return 1 -- Instruction size in bytes

    -- DEY, Decrement Index Y by One
    -- Y - 1 -> Y
    -- N Z C I D V
    -- + + - - - -
    0x88 -> do
      dec Y
      return 1 -- Instruction size in bytes

    -- EOR, Exclusive-OR Memory with Accumulator
    -- A EOR M -> A
    -- N Z C I D V
    -- + + - - - -
    0x49 -> do
      eor Immediate
      return 2 -- Instruction size in bytes

    0x45 -> do
      eor Zeropage
      return 2 -- Instruction size in bytes
    0x55 -> do
      eor ZeropageX
      return 2 -- Instruction size in bytes

    0x4D -> do
      eor Absolute
      return 3 -- Instruction size in bytes

    0x5D -> do
      eor AbsoluteX
      return 3 -- Instruction size in bytes

    0x59 -> do
      eor AbsoluteY
      return 3 -- Instruction size in bytes

    0x41 -> do
      eor IndirectX
      return 2 -- Instruction size in bytes

    0x51 -> do
      eor IndirectY
      return 2 -- Instruction size in bytes

    -- INC, Increment Memory by One
    -- M + 1 -> M
    -- N Z C I D V
    -- + + - - - -
    0xE6 -> do
      inc Zeropage
      return 2 -- Instruction size in bytes

    0xF6 -> do
      inc ZeropageX
      return 2 -- Instruction size in bytes

    0xEE -> do
      inc Absolute
      return 3 -- Instruction size in bytes

    0xFE -> do
      inc AbsoluteX
      return 3 -- Instruction size in bytes

    -- INX, Increment Index X by One
    -- X + 1 -> X
    -- N Z C I D V
    -- + + - - - -
    0xE8 -> do
      inc X
      return 1 -- Instruction size in bytes

    -- INY, Increment Index Y by One
    -- Y + 1 -> Y
    -- N Z C I D V
    -- + + - - - -
    0xC8 -> do
      inc Y
      return 1 -- Instruction size in bytes

    -- JMP, Jump to New Location
    -- (PC + 1) -> PCL
    -- (PC + 2) -> PCH
    -- N Z C I D V
    -- - - - - - -
    0x4C -> do
      jmp AbsoluteJmp
      return 3 -- Instruction size in bytes

    0x6C -> do
      jmp IndirectJmp
      return 3 -- Instruction size in bytes

    -- JSR, Jump to New Location Saving Return Address
    -- push (PC + 2)
    -- (PC + 1) -> PCL
    -- (PC + 2) -> PCH
    -- N Z C I D V
    -- - - - - - -
    0x20 -> do
      jsr
      return 3 -- Instruction size in bytes

    -- LDA, Load Accumulator with Memory
    -- M -> A
    -- N Z C I D V
    -- + + - - - -
    0xA9 -> do
      load Immediate Accumulator
      return 2 -- Instruction size in bytes

    0xA5 -> do
      load Zeropage  Accumulator
      return 2 -- Instruction size in bytes

    0xB5 -> do
      load ZeropageX Accumulator
      return 2 -- Instruction size in bytes

    0xAD -> do
      load Absolute  Accumulator
      return 3 -- Instruction size in bytes

    0xBD -> do
      load AbsoluteX Accumulator
      return 3 -- Instruction size in bytes

    0xB9 -> do
      load AbsoluteY Accumulator
      return 3 -- Instruction size in bytes

    0xA1 -> do
      load IndirectX Accumulator
      return 2 -- Instruction size in bytes

    0xB1 -> do
      load IndirectY Accumulator
      return 2 -- Instruction size in bytes

    -- LDX, Load Index X with Memory
    -- M -> X
    -- N Z C I D V
    -- + + - - - -
    0xA2 -> do
      load Immediate X
      return 2 -- Instruction size in bytes

    0xA6 -> do
      load Zeropage  X
      return 2 -- Instruction size in bytes

    0xB6 -> do
      load ZeropageY X
      return 2 -- Instruction size in bytes

    0xAE -> do
      load Absolute  X
      return 3 -- Instruction size in bytes

    0xBE -> do
      load AbsoluteY X
      return 3 -- Instruction size in bytes

    -- LDY, Load Index Y with Memory
    -- M -> Y
    -- N Z C I D V
    -- + + - - - -
    0xA0 -> do
      load Immediate Y
      return 2 -- Instruction size in bytes

    0xA4 -> do
      load Zeropage  Y
      return 2 -- Instruction size in bytes

    0xB4 -> do
      load ZeropageX Y
      return 2 -- Instruction size in bytes

    0xAC -> do
      load Absolute  Y
      return 3 -- Instruction size in bytes

    0xBC -> do
      load AbsoluteX Y
      return 3 -- Instruction size in bytes

    -- LSR, Shift One Bit Right (Memory or Accumulator)
    -- 0 -> [76543210] -> C
    -- N Z C I D V
    -- - + + - - -
    0x4A -> do
      lsr Accumulator
      return 1 -- Instruction size in bytes

    0x46 -> do
      lsr Zeropage
      return 2 -- Instruction size in bytes

    0x56 -> do
      lsr ZeropageX
      return 2 -- Instruction size in bytes

    0x4E -> do
      lsr Absolute
      return 3 -- Instruction size in bytes

    0x5E -> do
      lsr AbsoluteX
      return 3 -- Instruction size in bytes

    -- NOP, No Operation
    -- TODO: I believe this still needs to fetch the PC
    -- to get the cycle count right and not loop
    0xEA -> do
      return 1 -- Instruction size in bytes

    -- ORA, OR Memory with Accumulator
    -- A OR M -> A
    -- N Z C I D V
    -- + + - - - -
    0x09 -> do
      ora Immediate
      return 2 -- Instruction size in bytes

    0x05 -> do
      ora Zeropage
      return 2 -- Instruction size in bytes
    0x15 -> do
      ora ZeropageX
      return 2 -- Instruction size in bytes

    0x0D -> do
      ora Absolute
      return 3 -- Instruction size in bytes

    0x1D -> do
      ora AbsoluteX
      return 3 -- Instruction size in bytes

    0x19 -> do
      ora AbsoluteY
      return 3 -- Instruction size in bytes

    0x01 -> do
      ora IndirectX
      return 2 -- Instruction size in bytes

    0x11 -> do
      ora IndirectY
      return 2 -- Instruction size in bytes

    -- PHA, Push Accumulator on Stack
    -- push A
    -- N V C I D V
    -- - - - - - -
    0x48 -> do
      pushReg rAC
      return 1 -- Instruction size in bytes
  -- +p
    -- PHP, Push Processor Status on Stack
    -- push SR
    -- N Z C I D V
    -- + + + + + +
    0x08 -> do
      pushReg rSR -- +p
      return 1 -- Instruction size in bytes

    -- PLA, Pull Accumulator from Stack
    -- pull A
    -- N Z C I D V
    -- + + - - - -
    0x68 -> do
      b <- pull
      setFlag Negative (testBit b 7)
      setFlag Zero     (b == 0)
      setAC b
      return 1 -- Instruction size in bytes
    -- PLP, Pull Processor Status from Stack
    -- pull SR
    -- N Z C I D V
    -- + + + + + +
    0x28 -> do
      b <- pull
      setFlag Negative (testBit b 7)
      setFlag Zero     (b == 0)
      setSR b
      return 1 -- Instruction size in bytes
    -- ROL, Rotate One Bit Left (Memory or Accumulator)
    -- C <- [76543210] <- C
    -- N Z C I D V
    -- + + + - - -
    0x2A -> do
      rol Accumulator
      return 1 -- Instruction size in bytes

    0x26 -> do
      rol Zeropage
      return 2 -- Instruction size in bytes

    0x36 -> do
      rol ZeropageX
      return 2 -- Instruction size in bytes

    0x2E -> do
      rol Absolute
      return 3 -- Instruction size in bytes

    0x3E -> do
      rol AbsoluteX
      return 3 -- Instruction size in bytes
  -- +p
    -- ROR, Rotate One Bit Right (Memory or Accumulator)
    -- C -> [76543210] -> C
    -- N Z C I D V
    -- + + + - - -
    0x6A -> do
      ror Accumulator
      return 1 -- Instruction size in bytes

    0x66 -> do
      ror Zeropage
      return 2 -- Instruction size in bytes

    0x76 -> do
      ror ZeropageX
      return 2 -- Instruction size in bytes

    0x6E -> do
      ror Absolute
      return 3 -- Instruction size in bytes

    0x7E -> do
      ror AbsoluteX
      return 3 -- Instruction size in bytes

    -- RTI, Return from Interrupt
    -- pull SR, pull PC
    -- N Z C I D V
    -- - - - - - -
    0x40 -> do
    -- TODO: is the PC set correctly at the end?
      sr  <- pull
      setSR sr
      pch <- pull
      pcl <- pull
      setPC (mkWord pcl pch)
      return 1 -- Instruction size in bytes
    -- RTS, Return from Subroutine
    -- pull PC, PC + 1 -> PC
    -- N Z C I D V
    -- - - - - - -
    0x60 -> do
    -- TODO: is the PC set correctly at the end?
      pch <- pull
      pcl <- pull
      setPC ((mkWord pcl pch) + 1)
      return 1 -- Instruction size in bytes
    -- SBC, Subtract Memory from Accumulator with Borrow
    -- A - M - C -> A
    -- N Z C I D V
    -- + + + - - +
    0xE9 -> do
      sbc Immediate
      return 2 -- Instruction size in bytes

    0xE5 -> do
      sbc Zeropage
      return 2 -- Instruction size in bytes

    0xF5 -> do
      sbc ZeropageX
      return 2 -- Instruction size in bytes

    0xED -> do
      sbc Absolute
      return 3 -- Instruction size in bytes

    0xFD -> do
      sbc AbsoluteX
      return 3 -- Instruction size in bytes

    0xF9 -> do
      sbc AbsoluteY
      return 3 -- Instruction size in bytes

    0xE1 -> do
      sbc IndirectX
      return 2 -- Instruction size in bytes

    0xF1 -> do
      sbc IndirectY
      return 2 -- Instruction size in bytes

    -- SEC, Set Carry Flag
    -- 1 -> C
    -- N Z C I D V
    -- - - 1 - - -
    0x38 -> do
      setFlag Carry True
      return 1 -- Instruction size in bytes

    -- SED, Set Decimal Flag
    -- 1 -> D
    -- N Z C I D V
    -- - - - - 1 -
    0xF8 -> do
      setFlag Decimal True
      return 1 -- Instruction size in bytes

    -- SEI, Set Interrupt Disable Status
    -- 1 -> I
    -- N Z C I D V
    -- - - - 1 - -
    0x78 -> do
      setFlag Interrupt True
      return 1 -- Instruction size in bytes
    -- STA, Store Accumulator in Memory
    -- A -> M
    -- N Z C I D V
    -- - - - - - -
    0x85 -> do
      store Accumulator Zeropage
      return 2 -- Instruction size in bytes

    0x95 -> do
      store Accumulator ZeropageX
      return 2 -- Instruction size in bytes

    0x8D -> do
      store Accumulator Absolute
      return 3 -- Instruction size in bytes

    0x9D -> do
      store Accumulator AbsoluteX
      return 3 -- Instruction size in bytes

    0x99 -> do
      store Accumulator AbsoluteY
      return 3 -- Instruction size in bytes

    0x81 -> do
      store Accumulator IndirectX
      return 2 -- Instruction size in bytes

    0x91 -> do
      store Accumulator IndirectY
      return 2 -- Instruction size in bytes

    -- STX, Store Index X in Memory
    -- X -> M
    -- N Z C I D V
    -- - - - - - -
    0x86 -> do
      store X Zeropage
      return 2 -- Instruction size in bytes

    0x96 -> do
      store X ZeropageY
      return 2 -- Instruction size in bytes

    0x8E -> do
      store X Absolute
      return 3 -- Instruction size in bytes

    -- STY, Store Index Y in Memory
    -- Y -> M
    -- N Z C I D V
    -- - - - - - -
    0x84 -> do
      store Y Zeropage
      return 2 -- Instruction size in bytes

    0x94 -> do
      store Y ZeropageX
      return 2 -- Instruction size in bytes

    0x8C -> do
      store Y Absolute
      return 3 -- Instruction size in bytes

    -- TAX, Transfer Accumulator to Index X
    -- A -> X
    -- N Z C I D V
    -- + + - - - -
    0xAA -> do
      load Accumulator X
      return 1 -- Instruction size in bytes

    -- TAY, Transfer Accumulator to Index Y
    -- A -> Y
    -- N Z C I D V
    -- + + - - - -
    0xA8 -> do
      load Accumulator Y
      return 1 -- Instruction size in bytes

    -- TSX, Transfer Stack Pointer to Index x
    -- SP -> X
    -- N Z C I D V
    -- + + - - - -
    0xBA -> do
      load SP X
      return 1 -- Instruction size in bytes

    -- TXA, Transfer Index X to Accumulator
    -- X -> A
    -- N Z C I D V
    -- + + - - - -
    0x8A -> do
      load X Accumulator
      return 1 -- Instruction size in bytes

    -- TXS, Transfer Index X to Stack Register
    -- X -> SP
    -- N Z C I D V
    0x9A -> do
      load X SP
      return 1 -- Instruction size in bytes

    -- TYA -> Transfer Index Y to Accumulator
    -- Y -> A
    -- N Z C I D V
    0x98 -> do
      load Y Accumulator
      return 1 -- Instruction size in bytes

    -- TODO: all unimplemented opcodes are nop
    -- the correct thing would be to check
    -- their cycle counts
    _ -> do
      return 1 -- Instruction size in bytes
