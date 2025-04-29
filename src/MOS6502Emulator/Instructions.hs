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

-- addAC :: AddressMode -> FDX ()
-- TODO: update status register
-- -- TODO: add carry bit
-- addAC mode = do
--   b <- fetchOperand mode
--   modifyOperand Accumulator $ \ac -> do
--     return $! ac + b

addAC :: AddressMode -> FDX ()
addAC mode = do
  b <- fetchOperand mode
  ac <- getReg rAC
  c <- isFlagSet Carry
  let result = ac + b + (if c then 1 else 0)
  setFlag Carry (result > 0xFF) -- 9-bit overflow
  setFlag Zero (result == 0)
  setFlag Negative (testBit result 7)
  -- Overflow logic: (A^b7) && ~(ac^b7)
  setFlag Overflow (not (testBit (ac `xor` b) 7) && testBit (ac `xor` result) 7)
  setAC (fromIntegral result)


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
  liftIO $ putStrLn $ "Jsr return address: " ++ (showHex currentPC "")

  pushWord currentPC -- Push the return address
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
      b <- fetchByteMem pc -- Fetch opcode byte at PC
      setPC (pc + 1)   -- Move PC to next byte (like a real 6502)
      modify (\s -> s { instructionCount = instructionCount s + 1 })
      execute b
      machineState' <- get
      return (not (halted machineState'))

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

execute :: Word8 -> FDX ()
execute opc = do
  pc' <- getReg rPC
  liftIO $ putStrLn $ "Executing opcode $" ++ showHex opc "" ++ " at PC $" ++ showHex pc' ""
  case opc of
    -- ADC, Add Memory to Accumulator with Carry
    -- A + M + C -> A, C
    -- N Z C I D V
    -- + + + - - +
    --
    0x69 -> do addAC Immediate
    0x65 -> do
      addAC Zeropage
    0x75 -> do
      addAC ZeropageX
    0x6D -> do
      addAC Absolute
    0x7D -> do
      addAC AbsoluteX
    0x79 -> do
      addAC AbsoluteY
    0x61 -> do
      addAC IndirectX
    0x71 -> do
      addAC IndirectY

    -- AND, AND Memory with Accumulator
    -- A AND M -> A
    -- N Z C I D V
    -- + + - - - -
    0x29 -> do
      andAC Immediate

    0x25 -> do
      andAC Zeropage

    0x35 -> do
      andAC ZeropageX

    0x2D -> do
      andAC Absolute

    0x3D -> do
      andAC AbsoluteX

    0x39 -> do
      andAC AbsoluteY

    0x21 -> do
      andAC IndirectX

    0x31 -> do
      andAC IndirectY

    -- ASL, Shift Left One Bit (Memory of Accumulator)
    -- C <- [76543210] <- 0
    -- N Z C I D V
    -- + + + - - -
    0x0A -> do
      asl Accumulator

    0x06 -> do
      asl Zeropage

    0x16 -> do
      asl ZeropageX

    0x0E -> do
      asl Absolute

    0x1E -> do
      asl AbsoluteX

    -- BCC, Branch on Carry Clear
    -- branch if C = 0
    -- N Z C I D V
    -- - - - - - -
    0x90 -> do
      branchOn (not <$> isFlagSet Carry)

    -- BCS, Branch On Carry Set
    -- branch if C = 1
    -- N Z C I D V
    -- - - - - - -
    0xB0 -> do
      branchOn (isFlagSet Carry)

    -- BEQ, Branch on Result Zero
    -- branch if Z = 1
    -- N Z C I D V
    -- - - - - - -
    0xF0 -> do
      branchOn (isFlagSet Zero)

    -- BIT, Test Bits in Memory with Accumulator
    -- A AND M, M7 -> N, M6 -> V
    --  N Z C I D V
    -- M7 + - - - M6
    -- TODO: wikibooks listed a 3rd op code for BIT
    -- why is that?
    0x24 -> do
      testBits Zeropage

    0x2C -> do
      testBits Absolute

    -- BMI, Branch on Result Minus
    -- branch if N = 1
    -- N Z C I D V
    -- - - - - - -
    0x30 -> do
      branchOn (isFlagSet Negative)

    -- BNE, Branch on Result not Zero
    -- branch if Z = 0
    -- N Z C I D V
    -- - - - - - -
    0xD0 -> do
      branchOn (not <$> isFlagSet Zero)


    -- BPL, Branch on Resutl Plus
    -- branch if N = 0
    -- N Z C I D V
    -- - - - - - -
    0x10 -> do
      branchOn (not <$> isFlagSet Negative)


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

    -- BVC, Break on Overflow Clear
    -- branch if V = 0
    -- N Z C I D V
    -- - - - - - -
    0x50 -> do
      branchOn (not <$> isFlagSet Overflow)

    -- BVS, Branch on Overflow Set
    -- branch if V = 1
    -- N Z C I D V
    -- - - - - - -
    0x70 -> do
      branchOn (isFlagSet Overflow)

    -- CLC, Clear Carry flag
    -- 0 -> C
    -- N Z C I D V
    -- - - 0 - - -
      addAC IndirectY
    0x18 -> do
      setFlag Carry False

    -- CLD, Clear Decimal Mode
    -- 0 -> D
    -- N Z C I D V
    -- - - - - 0 -
    0xD8 -> do
      setFlag Decimal False

    -- CLI, Clear Interrupt Disable Bit
    -- 0 -> I
    -- N Z C I D V
    -- - - - 0 - -
    0x58 -> do
      setFlag Interrupt False

    -- CLV, Clear Overflow Flag
    -- 0 -> V
    -- N Z C I D V
    -- - - - - - 0
    0xB8 -> do
      setFlag Overflow False

    -- CMP, Compare Memory with Accumulator
    -- A - M
    -- N Z C I D V
    -- + + + - - -
    0xC9 -> do
      cmp rAC Immediate

    0xC5 -> do
      cmp rAC Zeropage

    0xD5 -> do
      cmp rAC ZeropageX

    0xCD -> do
      cmp rAC Absolute

    0xDD -> do
      cmp rAC AbsoluteX

    0xD9 -> do
      cmp rAC AbsoluteY

    0xC1 -> do
      cmp rAC IndirectX

    0xD1 -> do
      cmp rAC IndirectY

    -- CPX, Compare Memory and Index X
    -- X - M
    -- N Z C I D V
    -- + + + - - -
    0xE0 -> do
      cmp rX Immediate

    0xE4 -> do
      cmp rX Zeropage

    0xEC -> do
      cmp rX Absolute

    -- CPY, Compare Memory and Index Y
    -- Y - M
    -- N Z C I D V
    -- + + + - - -
    0xC0 -> do
      cmp rY Immediate

    0xC4 -> do
      cmp rY Zeropage

    0xCC -> do
      cmp rY Absolute

    -- DEC, Decrement Memory by One
    -- M - 1 -> M
    -- N Z C I D V
    -- + + - - - -
    0xC6 -> do
      dec Zeropage

    0xD6 -> do
      dec ZeropageX

    0xCE -> do
      dec Absolute

    0xDE -> do
      dec AbsoluteX

    -- DEX, Decrement Index X by One
    -- X - 1 -> X
    -- N Z C I D V
    -- + + - - - -
    0xCA -> do
      dec X

    -- DEY, Decrement Index Y by One
    -- Y - 1 -> Y
    -- N Z C I D V
    -- + + - - - -
    0x88 -> do
      dec Y

    -- EOR, Exclusive-OR Memory with Accumulator
    -- A EOR M -> A
    -- N Z C I D V
    -- + + - - - -
    0x49 -> do
      eor Immediate

    0x45 -> do
      eor Zeropage

    0x55 -> do
      eor ZeropageX

    0x4D -> do
      eor Absolute

    0x5D -> do
      eor AbsoluteX

    0x59 -> do
      eor AbsoluteY

    0x41 -> do
      eor IndirectX

    0x51 -> do
      eor IndirectY

    -- INC, Increment Memory by One
    -- M + 1 -> M
    -- N Z C I D V
    -- + + - - - -
    0xE6 -> do
      inc Zeropage

    0xF6 -> do
      inc ZeropageX

    0xEE -> do
      inc Absolute

    0xFE -> do
      inc AbsoluteX

    -- INX, Increment Index X by One
    -- X + 1 -> X
    -- N Z C I D V
    -- + + - - - -
    0xE8 -> do
      inc X

    -- INY, Increment Index Y by One
    -- Y + 1 -> Y
    -- N Z C I D V
    -- + + - - - -
    0xC8 -> do
      inc Y

    -- JMP, Jump to New Location
    -- (PC + 1) -> PCL
    -- (PC + 2) -> PCH
    -- N Z C I D V
    -- - - - - - -
    0x4C -> do
      jmp AbsoluteJmp

    0x6C -> do
      jmp IndirectJmp

    -- JSR, Jump to New Location Saving Return Address
    -- push (PC + 2)
    -- (PC + 1) -> PCL
    -- (PC + 2) -> PCH
    -- N Z C I D V
    -- - - - - - -
    0x20 -> do
      jsr

    -- LDA, Load Accumulator with Memory
    -- M -> A
    -- N Z C I D V
    -- + + - - - -
    0xA9 -> do
      load Immediate Accumulator

    0xA5 -> do
      load Zeropage  Accumulator

    0xB5 -> do
      load ZeropageX Accumulator

    0xAD -> do
      load Absolute  Accumulator

    0xBD -> do
      load AbsoluteX Accumulator

    0xB9 -> do
      load AbsoluteY Accumulator

    0xA1 -> do
      load IndirectX Accumulator

    0xB1 -> do
      load IndirectY Accumulator

    -- LDX, Load Index X with Memory
    -- M -> X
    -- N Z C I D V
    -- + + - - - -
    0xA2 -> do
      load Immediate X

    0xA6 -> do
      load Zeropage  X

    0xB6 -> do
      load ZeropageY X

    0xAE -> do
      load Absolute  X

    0xBE -> do
      load AbsoluteY X

    -- LDY, Load Index Y with Memory
    -- M -> Y
    -- N Z C I D V
    -- + + - - - -
    0xA0 -> do
      load Immediate Y

    0xA4 -> do
      load Zeropage  Y

    0xB4 -> do
      load ZeropageX Y

    0xAC -> do
      load Absolute  Y

    0xBC -> do
      load AbsoluteX Y

    -- LSR, Shift One Bit Right (Memory or Accumulator)
    -- 0 -> [76543210] -> C
    -- N Z C I D V
    -- - + + - - -
    0x4A -> do
      lsr Accumulator

    0x46 -> do
      lsr Zeropage

    0x56 -> do
      lsr ZeropageX

    0x4E -> do
      lsr Absolute

    0x5E -> do
      lsr AbsoluteX

    -- NOP, No Operation
    -- TODO: I believe this still needs to fetch the PC
    -- to get the cycle count right and not loop
    0xEA -> return ()

    -- ORA, OR Memory with Accumulator
    -- A OR M -> A
    -- N Z C I D V
    -- + + - - - -
    0x09 -> do
      ora Immediate

    0x05 -> do
      ora Zeropage
      
    0x15 -> do
      ora ZeropageX

    0x0D -> do
      ora Absolute

    0x1D -> do
      ora AbsoluteX

    0x19 -> do
      ora AbsoluteY

    0x01 -> do
      ora IndirectX

    0x11 -> do
      ora IndirectY

    -- PHA, Push Accumulator on Stack
    -- push A
    -- N V C I D V
    -- - - - - - -
    0x48 -> do
      pushReg rAC
  -- +p
    -- PHP, Push Processor Status on Stack
    -- push SR
    -- N Z C I D V
    -- + + + + + +
    0x08 -> do
      pushReg rSR -- +p

    -- PLA, Pull Accumulator from Stack
    -- pull A
    -- N Z C I D V
    -- + + - - - -
    0x68 -> do
      b <- pull
      setFlag Negative (testBit b 7)
      setFlag Zero     (b == 0)
      setAC b

    -- PLP, Pull Processor Status from Stack
    -- pull SR
    -- N Z C I D V
    -- + + + + + +
    0x28 -> do
      b <- pull
      setFlag Negative (testBit b 7)
      setFlag Zero     (b == 0)
      setSR b

    -- ROL, Rotate One Bit Left (Memory or Accumulator)
    -- C <- [76543210] <- C
    -- N Z C I D V
    -- + + + - - -
    0x2A -> do
      rol Accumulator

    0x26 -> do
      rol Zeropage

    0x36 -> do
      rol ZeropageX

    0x2E -> do
      rol Absolute

    0x3E -> do
      rol AbsoluteX

    -- ROR, Rotate One Bit Right (Memory or Accumulator)
    -- C -> [76543210] -> C
    -- N Z C I D V
    -- + + + - - -
    0x6A -> do
      ror Accumulator

    0x66 -> do
      ror Zeropage

    0x76 -> do
      ror ZeropageX

    0x6E -> do
      ror Absolute

    0x7E -> do
      ror AbsoluteX

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

    -- RTS, Return from Subroutine
    -- pull PC, PC + 1 -> PC
    -- N Z C I D V
    -- - - - - - -
    0x60 -> do
    -- TODO: is the PC set correctly at the end?
      pch <- pull
      pcl <- pull
      setPC ((mkWord pcl pch) + 1)
      
    -- SBC, Subtract Memory from Accumulator with Borrow
    -- A - M - C -> A
    -- N Z C I D V
    -- + + + - - +
    0xE9 -> do
      sbc Immediate

    0xE5 -> do
      sbc Zeropage

    0xF5 -> do
      sbc ZeropageX

    0xED -> do
      sbc Absolute

    0xFD -> do
      sbc AbsoluteX

    0xF9 -> do
      sbc AbsoluteY

    0xE1 -> do
      sbc IndirectX

    0xF1 -> do
      sbc IndirectY

    -- SEC, Set Carry Flag
    -- 1 -> C
    -- N Z C I D V
    -- - - 1 - - -
    0x38 -> do
      setFlag Carry True

    -- SED, Set Decimal Flag
    -- 1 -> D
    -- N Z C I D V
    -- - - - - 1 -
    0xF8 -> do
      setFlag Decimal True

    -- SEI, Set Interrupt Disable Status
    -- 1 -> I
    -- N Z C I D V
    -- - - - 1 - -
    0x78 -> do
      setFlag Interrupt True

    -- STA, Store Accumulator in Memory
    -- A -> M
    -- N Z C I D V
    -- - - - - - -
    0x85 -> do
      store Accumulator Zeropage

    0x95 -> do
      store Accumulator ZeropageX

    0x8D -> do
      store Accumulator Absolute

    0x9D -> do
      store Accumulator AbsoluteX

    0x99 -> do
      store Accumulator AbsoluteY

    0x81 -> do
      store Accumulator IndirectX

    0x91 -> do
      store Accumulator IndirectY

    -- STX, Store Index X in Memory
    -- X -> M
    -- N Z C I D V
    -- - - - - - -
    0x86 -> do
      store X Zeropage

    0x96 -> do
      store X ZeropageY

    0x8E -> do
      store X Absolute

    -- STY, Store Index Y in Memory
    -- Y -> M
    -- N Z C I D V
    -- - - - - - -
    0x84 -> do
      store Y Zeropage

    0x94 -> do
      store Y ZeropageX

    0x8C -> do
      store Y Absolute

    -- TAX, Transfer Accumulator to Index X
    -- A -> X
    -- N Z C I D V
    -- + + - - - -
    0xAA -> do
      load Accumulator X

    -- TAY, Transfer Accumulator to Index Y
    -- A -> Y
    -- N Z C I D V
    -- + + - - - -
    0xA8 -> do
      load Accumulator Y

    -- TSX, Transfer Stack Pointer to Index x
    -- SP -> X
    -- N Z C I D V
    -- + + - - - -
    0xBA -> do
      load SP X

    -- TXA, Transfer Index X to Accumulator
    -- X -> A
    -- N Z C I D V
    -- + + - - - -
    0x8A -> do
      load X Accumulator

    -- TXS, Transfer Index X to Stack Register
    -- X -> SP
    -- N Z C I D V
    0x9A -> do
      load X SP

    -- TYA -> Transfer Index Y to Accumulator
    -- Y -> A
    -- N Z C I D V
    0x98 -> do
      load Y Accumulator

    -- TODO: all unimplemented opcodes are nop
    -- the correct thing would be to check
    -- their cycle counts
    _ -> do
      return () -- Instruction size in bytes
