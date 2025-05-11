import Assembly.Core hiding ( LabelExpression(..), ArithExpr(add, sub), org) -- Hide Core's renamed operators, LabelAdd, and org if needed
import Assembly.EDSLInstr
import qualified Assembly.Core as C ( ArithExpr(add, sub), LabelExpression(LabelAdd)) -- Import only needed qualified names
import Assembly(Asm, runAssembler, generateBinary, formatHexBytes, formatBasic, org) -- Import org and formatBasic from Assembly
import Assembly.List(createList, createList_, createListFromString, addToList, copyList, filterMoreThanList, sumList) -- Import from Assembly.List instead
import Assembly.Macros(addAto16bit)  -- Import addAto16bit
import qualified Data.Map.Strict as Map
import Numeric (showHex)
import C64.Examples (horizontalBars)
import Data.Word (Word16, Word8)
import Data.ByteString as BS (writeFile, toStrict)
import Data.Binary.Put (runPut, putWord16le, putWord8)
import System.FilePath (takeExtension)
import Prelude -- Explicitly import Prelude to qualify (+)
import Options.Applicative
import MOS6502Emulator (Machine(..), newMachine, setupMachine, runDebugger, instructionCount)
import MOS6502Emulator.Registers (mkRegisters, rPC)

-- stack run -- --output ./c64/result.prg && cd c64 && /usr/bin/x64sc result.prg && cd ..
-- *brk* instruction to stop
-- stack run -- --debug-address 4096 -s result.lab

-- --- Command Line Options ---
data Options = Options
  { c64BasicOutput :: Bool
  , outputFile :: Maybe FilePath
  , debugAddress :: Maybe Word16
  , symbolOutputFile :: Maybe FilePath -- Added for symbol file output
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
     ( long "c64"
     <> help "Output in Commodore 64 BASIC format"
     )
  <*> optional (strOption
     ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output file name for binary data"
      ))
 <*> optional (option auto
       ( long "debug-address"
       <> metavar "ADDRESS"
       <> help "Run assembled code in emulator starting at ADDRESS"
       ))
 <*> optional (strOption -- Added for symbol file output
       ( long "symbol-output"
       <> short 's'
       <> metavar "SYM_FILE"
       <> help "Output symbol file name"
       ))

options :: ParserInfo Options
options = info (optionsParser <**> helper)
           ( fullDesc
          <> progDesc "Assemble 6502 code and output in hex or C64 BASIC"
         <> header "H6502 Assembler" )


-- --- Przykład Użycia ---

mySimpleProgram01 :: Asm ()
mySimpleProgram01 = do

    org 0xc000
    jmp "start" -- Skok do początku programu

    l_ "lista2"
    db [0x00] -- Length of the list
    db [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] -- Padding for elements

    l_ "lista3"
    db [0x00] -- Length of the list
    db [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] -- Padding for elements

    l_ "lista4"
    db [0x00] -- Length of the list
    db [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] -- Padding for elements

    l_ "lista5"
    db [0x00] -- Length of the list
    db [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] -- Padding for elements

    l_ "lista1"
    createListFromString "Hello World!"

    l_ "sumResult"
    db [0x00, 0x00] -- Wynik sumowania


    l_ "start"

    -- Test addAto16bit
    lda# 0x05       -- Load value 5 into A
    sta $ AddrLabel "test16bit"  -- Store low byte
    lda# 0x00
    sta $ AddrLabel "test16bit" .+ 1  -- Store high byte (0x0005) - Use C.add
    lda# 0x03       -- Load value 3 into A
    addAto16bit $ AddrLabel "test16bit"  -- Add 3 to 0x0005, result should be 0x0008

    -- Expected result: test16bit = 0x08, test16bit+1 = 0x00
    -- Test label arithmetic with parentheses
    l_ "label_arith_test"
    -- Use standard (Prelude.+) for numeric arithmetic within the offset
    lda $ AddrLabel "label_arith_test" .+ (2 - (2*3))
    sta $ AddrLabel "label_arith_result"

    jmp "l_cont"  -- Skok do testu

    l_ "test16bit"
    db [0x00, 0x00]  -- 16-bit storage for test

    l_ "label_arith_result"
    db [0x00]  -- Storage for label arithmetic result
    db [0x00, 0x00]  -- 16-bit storage for test

    l_ "l_cont"

    let myList2 = AddrLabel "lista2"  -- Obszar na listę 2
    let sumResult = AddrLabel "sumResult" -- Obszar na wynik sumowania
    createList_ (myList2) -- Inicjalizuj listę 2
    myList3 <- createList "lista3" -- Obszar na listę 3
    myList4 <- createList "lista4" -- Obszar na listę 4
    myList5 <- createList "lista5" -- Obszar na listę 5

    -- Dodaj elementy do list
    -- addToList myList1 (asc 'H')
    -- addToList myList1 (asc 'i')
    -- addToList myList1 (asc '!')

    addToList myList2 0x10
    addToList myList2 0x20
    addToList myList2 0x30
    addToList myList2 0x40
    addToList myList2 0x50
    addToList myList2 0x60

    -- Kopiuj listy wielokrotnie
    copyList (AddrLabel "lista1") myList3   -- Pierwsze wywołanie copyList
    copyList myList2 myList4  -- Drugie wywołanie copyList

    filterMoreThanList myList2 myList5 0x20 -- Przykład użycia filterMoreThanList
    sumList myList5 sumResult

    -- Inny kod...
    lda# 0x00
    l_ "loop_forever"
    jmp "loop_forever"





main :: IO ()
main = do
    opts <- execParser options
    let initialStartAddress = 0x800 -- Default start address if no ORG is used (BASIC loader -1)
    putStrLn $ "Attempting assembly (default start: $" ++ showHex initialStartAddress "" ++ ")"
    case runAssembler initialStartAddress horizontalBars of -- Using horizontalBars for now as an example program
      Left err -> putStrLn $ "Assembly failed: \n" ++ err
      Right (actualStartAddress, byteCode, labels) -> do
        putStrLn "\n--- Assembly Successful! ---"
        putStrLn $ "Actual Start Address: $" ++ showHex actualStartAddress ""
        putStrLn "\nLabels Defined:"
        mapM_ (\(lbl, addr) -> putStrLn $ "  " ++ lbl ++ "= $" ++ showHex addr "") (Map.toList labels)

        -- Write symbol file if specified
        case symbolOutputFile opts of
          Just symPath -> do
            let symbolData = unlines $ map (\(name, addr) -> showHex addr "" ++ " " ++ name) (Map.toList labels)
            Prelude.writeFile symPath symbolData
            putStrLn $ "\nSymbols written to: " ++ symPath
          Nothing -> return ()
        
        case debugAddress opts of
          Just addr -> do
            _ <- runDebugger addr actualStartAddress byteCode (symbolOutputFile opts) -- If testAddress is provided, run the test with the specified start address and actual load address, passing symbol file path
            return () -- Explicitly return IO () after running the debugger
          Nothing -> do -- Otherwise, proceed with file output
            let output = if c64BasicOutput opts
                            then formatBasic actualStartAddress byteCode
                            else formatHexBytes actualStartAddress byteCode
            
            case outputFile opts of
              Nothing -> do
                putStrLn $ "\nGenerated ByteCode (" ++ show (length byteCode) ++ " bytes):"
                putStrLn output
              Just filePath -> do
                if takeExtension filePath == ".prg"
                  then do
                    let prgData = BS.toStrict $ runPut $ do
                          putWord16le (fromIntegral actualStartAddress)
                          mapM_ putWord8 byteCode
                    BS.writeFile filePath prgData
                    putStrLn $ "\nBytecode written to PRG file: " ++ filePath
                  else do
                    Prelude.writeFile filePath output
                    putStrLn $ "\nBytecode written to: " ++ filePath
