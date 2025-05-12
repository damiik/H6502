module Assembly.ListSpec (spec) where

import Test.Hspec
import Assembly.List
import Assembly.Core
import Assembly.EDSLInstr
import Assembly.Macros
import Data.Word
import Assembly (runAssembler) -- Import runAssembler
import MOS6502Emulator (runEmulator, newMachine, setupMachine, Machine(..), Memory, Registers) -- Import emulator functions and types
import Data.Char (ord) -- Import ord
import MOS6502Emulator.Memory (fetchByte) -- Import fetchByte from Memory module
import qualified Data.Map.Strict as Map -- Import Map
import Assembly.TestHelper (runAssemblyTest, runAssemblyDebugTest)

spec :: Spec
spec = do
  describe "Assembly.List" $ do
    describe "createList_" $ do
      it "should initialize a list at the given address with length 0" $ do
        let listAddr = 0x0200 -- Example address for the list
        let asm = do
              org 0x0100 -- Assemble at 0x0100
              createList_ (AddrLit16 listAddr)
              -- Add a halt instruction to stop emulation
              brk

        (finalMachine, _) <- runAssemblyTest 0x0100 asm
        -- Verify that the byte at listAddr is 0
        byteAtListAddr <- fetchByte listAddr (mMem finalMachine)
        byteAtListAddr `shouldBe` 0x00

    describe "createList" $ do
      it "should create a list with a label and initialize its length to 0" $ do
        let listLabel = "myLabeledList"
        let asm = do
              org 0x0100 -- Assemble at 0x0100
              listAddrRef <- createList listLabel
              -- Add a halt instruction to stop emulation
              brk

        (finalMachine, labelMap) <- runAssemblyTest 0x0100 asm
        -- Find the address of the label in the assembled code
        case Map.lookup listLabel labelMap of
          Nothing -> expectationFailure $ "Label '" ++ listLabel ++ "' not found in label map"
          Just listAddr -> do
            -- Verify that the byte at listAddr is 0
            byteAtListAddr <- fetchByte listAddr (mMem finalMachine)
            byteAtListAddr `shouldBe` 0x00

    describe "copyList" $ do
      it "should copy elements from a source list to a destination list" $ do
        let srcListAddr = 0x0200
        let dstListAddr = 0x0300
        let sourceData = [1, 2, 3, 4, 5]
        let asm = do
              org 0x0100 -- Assemble at 0x0100
              -- Create source list
              jmp  ("codeStart"::Label) -- Jump over data
              -- Create source list
              l_ "sourceListData" -- Define label for source data
              db [fromIntegral (length sourceData)] -- Length byte
              db sourceData -- Data bytes
              -- Define label for the start of executable code
              l_ "codeStart"
              let srcListRef = AddrLabel "sourceListData" -- Use label reference
              let dstListRef = AddrLit16 dstListAddr -- Keep as literal for now
              -- Copy list
              copyList srcListRef dstListRef
              -- Add a halt instruction to stop emulation
              brk

        (finalMachine, _) <- runAssemblyTest 0x0100 asm
        -- Verify destination list length
        dstListLength <- fetchByte dstListAddr (mMem finalMachine)
        dstListLength `shouldBe` fromIntegral (length sourceData)

        -- Verify destination list content
        let expectedBytes = fromIntegral (length sourceData) : sourceData
        actualBytes <- mapM (\offset -> fetchByte (dstListAddr + fromIntegral offset) (mMem finalMachine)) [0..length sourceData]
        actualBytes `shouldBe` expectedBytes

    describe "createListFromString" $ do
      it "should create a list from a string with the correct length and bytes" $ do
        let testString = "HELLO"
        let listLabel = "test-list"::Label
        let expectedBytes = fromIntegral (length testString) : map (fromIntegral . ord) testString
        let asm = do
              codeStart <- makeUniqueLabel ()
              org 0x0100 -- Assemble at 0x0100
              -- createListFromString directly emits bytes, so we need to make sure it's placed at listAddr
              -- This function doesn't take an AddressRef, it just emits db.
              -- We need to adjust the test approach or the function's usage.
              -- Looking at the function definition: createListFromString :: String -> Asm ()
              -- It just does `db [length] db bytes`. This means it emits at the current PC.
              -- So, we need to set the PC to listAddr before calling it.
              jmp codeStart
              l_ listLabel
              
              createListFromString testString
              l_ codeStart
              brk

        (finalMachine, labelMap) <- runAssemblyTest 0x0100 asm -- Run from 0x0100 where BRK is
        -- Verify list content

        case Map.lookup listLabel labelMap of
          Nothing -> expectationFailure $ "Label '" ++ listLabel ++ "' not found in label map"
          Just listAddr -> do
            -- Verify that the byte at listAddr is 0

            listLen <- fetchByte listAddr (mMem finalMachine)
            actualBytes <- mapM (\offset -> fetchByte (listAddr + fromIntegral offset) (mMem finalMachine)) [0..length expectedBytes - 1]
            actualBytes `shouldBe` expectedBytes

    describe "addToList" $ do
      it "should add an element to the end of the list and update the length" $ do
        let testListAddr = 0x0200
        let initialData = [10, 20, 30]
        let elementToAdd = 40
        let expectedLength = fromIntegral (length initialData + 1)
        let expectedBytes =  [expectedLength] ++ initialData ++ [elementToAdd] -- Expected bytes before addToList

        let asm = do
              org 0x0100 -- Assemble at 0x0100
              jmp ("program-start"::Label)

              -- Create initial list
              org testListAddr
              db [fromIntegral (length initialData)] -- Length byte
              db initialData -- Data bytes
              let listRef = AddrLit16 testListAddr
              -- Add element to list
              l_ "program-start"
              addToList listRef elementToAdd
              -- Add a halt instruction to stop emulation
              brk

        (finalMachine, _) <- runAssemblyTest 0x0100 asm
        -- Verify final list length
        finalListLength <- fetchByte testListAddr (mMem finalMachine)
        finalListLength `shouldBe` expectedLength

        -- Verify final list content
        -- Need to fetch bytes up to the new length
        actualBytes <- mapM (\offset -> fetchByte (testListAddr + fromIntegral offset) (mMem finalMachine)) [0..expectedLength]
        actualBytes `shouldBe` expectedBytes

    describe "iterateList" $ do
      it "should iterate over the list elements and apply the action" $ do
        let testListAddr = 0x0200
        let testData = [1, 2, 3]
        let resultAddr = 0x0400 -- Address to store the sum of elements

        let action :: Operand -> Asm ()
            action elementAddr = do
              -- Load the element value into A
              emitIns LDA elementAddr
              -- Add A to the value at resultAddr
              clc
              adc (AddrLit16 resultAddr)
              sta (AddrLit16 resultAddr)

        let asm = do
              org 0x0100 -- Assemble at 0x0100
              jmp ("program-start"::Label)

              -- Create list
              org testListAddr
              db [fromIntegral (length testData)] -- Length byte
              db testData -- Data bytes
              
              let listRef = AddrLit16 testListAddr
              -- Initialize resultAddr to 0
              l_ "program-start"
              lda# 0
              sta (AddrLit16 resultAddr)
              -- Iterate and apply action
              iterateList listRef action
              -- Add a halt instruction to stop emulation
              brk

        (finalMachine, _) <- runAssemblyTest 0x0100 asm
        -- Verify the sum stored at resultAddr
        finalSum <- fetchByte resultAddr (mMem finalMachine)
        finalSum `shouldBe` fromIntegral (sum testData)

    describe "iterateWithIndexList" $ do
      it "should iterate over the list elements with their indices and apply the action" $ do
        let listAddr = 0x0200
        let testData = [10, 20, 30]
        -- We will store the element value + its index at a new memory location
        let resultBaseAddr = 0x0400

        let action :: Operand -> Word8 -> Asm ()
            action elementAddr _ = do
              txa  -- load index (x) -> A
              -- Load the element value into A
              clc
              emitIns ADC elementAddr
              -- Store the result at resultBaseAddr + index
              sta$ X (AddrLit16 resultBaseAddr) -- Store at base + index
              txa -- load index (x) -> A again, this time for store list length
              sta$ AddrLit16 resultBaseAddr

        let asm = do
              org 0x0100 -- Assemble at 0x0100
              jmp ("program-start"::Label)
              
              -- Create list
              org listAddr  
              db [fromIntegral (length testData)] -- Length byte
              db testData -- Data bytes
              let listRef = AddrLit16 listAddr
              
              -- Iterate with index and apply action
              l_ "program-start"  
              iterateWithIndexList listRef action
              -- Add a halt instruction to stop emulation
              brk

        (finalMachine, _) <- runAssemblyDebugTest 0x0100 asm
        -- Verify the results stored in memory
        let expectedResults = fromIntegral (length testData)  : (zipWith (\val idx -> val + idx) testData [1..])
        actualResults <- mapM (\idx -> fetchByte (resultBaseAddr + fromIntegral idx) (mMem finalMachine)) [0..length testData]
        actualResults `shouldBe` map fromIntegral expectedResults

    describe "mapToNewList" $ do
      it "should apply a transformation to each element and store the result in a new list"
        pending -- Need to implement test using runAssemblyTest

    describe "mapInPlaceList" $ do
      it "should apply a transformation to each element in place"
        pending -- Need to implement test using runAssemblyTest

    describe "filterList" $ do
      it "should filter elements based on a predicate and store the result in a new list"
        pending -- Need to implement test using runAssemblyTest

    describe "filterMoreThanList" $ do
      it "should filter elements greater than a given value"
        pending -- Need to implement test using runAssemblyTest

    describe "sumList" $ do
      it "should calculate the sum of list elements and store it in a 16-bit result"
        pending -- Need to implement test using runAssemblyTest

    describe "foldList" $ do
      it "should fold/reduce the list elements using an initial value and a combine function"
        pending -- Need to implement test using runAssemblyTest
