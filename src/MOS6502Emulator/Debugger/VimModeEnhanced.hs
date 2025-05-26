module MOS6502Emulator.Debugger.VimModeEnhanced (
    interactiveLoopHelper
    , renderVimScreen
    , handleVimKey
) where


import Data.Word
import qualified Data.Map as Map
import Numeric (showHex)
import System.IO (hFlush, stdout, stdin, hSetEcho)

import Control.Monad.State (get, put, MonadIO (liftIO))
import Control.Monad.Trans.State (runStateT)
import MOS6502Emulator.Core (Machine(..), FDX, fetchByteMem, DebuggerAction (..), getRegisters, parseHexWord, unFDX)
import MOS6502Emulator.Registers (Registers(..), rPC) 
import MOS6502Emulator.Debugger.VimModeCore
import MOS6502Emulator.Debugger.VimModeHandleKey
import MOS6502Emulator.Debugger.Console(getKey, getInput, termHeight, termWidth, DebuggerConsoleState (..))
import MOS6502Emulator.DissAssembler(disassembleInstructions)
import MOS6502Emulator.Debugger (handleBreak, handleMemTrace, logRegisters, handleSetPC, handleSetReg8, handleFill, handleDisassemble)
import MOS6502Emulator.Machine (setPC_)
import qualified System.Console.ANSI as ANSI

-- | Parse a hex string to a byte value
-- parseHexByte :: String -> Maybe Word8
-- parseHexByte s = case readHex s of
--     [(n, "")] -> if n <= 0xFF then Just (fromIntegral n) else Nothing
--     _ -> Nothing



-- | Handle breakpoint commands (similar to handleBreakVim from VimMode.hs)
handleBreakVim :: VimState -> FDX (DebuggerAction, [String], VimState)
handleBreakVim vimState = do
  liftIO $ putStr "\nBreakpoints (a: add, d: delete):" >> hFlush stdout
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStr "Add Breakpoint (address):" >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleBreak args ""
      return (action, ["Breakpoints (a: add, d: delete): " ++ [key], "Add Breakpoint (address): " ++ input] ++ output, vimState { vsMessage = last output })
    'd' -> do
      liftIO $ putStr "Delete Breakpoint (address):" >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleBreak args ""
      return (action, ["Breakpoints (a: add, d: delete): " ++ [key], "Delete Breakpoint (address): " ++ input] ++ output, vimState { vsMessage = last output })
    _ -> return (NoAction, ["Invalid Breakpoint command"], vimState { vsMessage = "Invalid Breakpoint command" })

-- | Handle memory trace commands
handleMemTraceVim :: VimState -> FDX (DebuggerAction, [String], VimState)
handleMemTraceVim vimState = do
  liftIO $ putStr "\nMemory Trace (a: add, d: delete):" >> hFlush stdout
  key <- liftIO getKey
  case key of
    'a' -> do
      liftIO $ putStr "Add Memory Trace Block (start end [name]): " >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleMemTrace args ""
      return (action, ["Memory Trace (a: add, d: delete): " ++ [key], "Add Memory Trace Block (start end [name]): " ++ input] ++ output, vimState { vsMessage = last output })
    'd' -> do
      liftIO $ putStr "Delete Memory Trace Block (start end [name]): " >> hFlush stdout
      liftIO $ hSetEcho stdin True
      input <- liftIO getInput
      liftIO $ hSetEcho stdin False
      let args = words input
      (action, output) <- handleMemTrace args ""
      return (action, ["Memory Trace (a: add, d: delete): " ++ [key], "Delete Memory Trace Block (start end [name]): " ++ input] ++ output, vimState { vsMessage = last output })
    _ -> return (NoAction, ["Invalid Memory Trace command"], vimState { vsMessage = "Invalid Memory Trace command" })

-- | Handle stored address commands
handleAddressVim :: VimState -> FDX (DebuggerAction, [String], VimState)
handleAddressVim vimState = do
  liftIO $ putStr "\nStored Addresses (s: store, g: goto):" >> hFlush stdout
  key <- liftIO getKey
  case key of
    's' -> do
      liftIO $ putStr "Store Current PC (key):" >> hFlush stdout
      keyChar <- liftIO getKey
      machine <- get
      let currentPC = rPC (mRegs machine)
      put (machine { storedAddresses = Map.insert keyChar currentPC (storedAddresses machine) })
      return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "Stored PC $" ++ showHex currentPC "" ++ " at key '" ++ [keyChar] ++ "'"], vimState { vsMessage = "Stored PC at '" ++ [keyChar] ++ "'" })
    'g' -> do
      liftIO $ putStr "Goto Stored Address (key):" >> hFlush stdout
      keyChar <- liftIO getKey
      machine <- get
      case Map.lookup keyChar (storedAddresses machine) of
        Just addr -> do
          put (machine { mRegs = (mRegs machine) { rPC = addr } })
          (disassembledOutput, _) <- disassembleInstructions addr 1
          return (NoAction, ["Stored Addresses (s: store, g: goto): " ++ [key], "PC set to $" ++ showHex addr ""] ++ disassembledOutput, vimState { vsCursor = addr, vsViewStart = addr, vsMessage = "Moved to $" ++ showHex addr "" })
        Nothing -> return (NoAction, ["No address stored at key '" ++ [keyChar] ++ "'"], vimState { vsMessage = "No address stored at '" ++ [keyChar] ++ "'" })
    _ -> return (NoAction, ["Invalid Stored Address command"], vimState { vsMessage = "Invalid Stored Address command" })

-- | Handle colon commands with enhanced CommandMode integration
handleColonCommand :: String -> VimState -> FDX (DebuggerAction, [String], VimState)
handleColonCommand command vimState = do
  let args = words command
  machine <- get
  case args of
    ["q"] -> return (QuitEmulator, [], vimState { vsMessage = "Quitting emulator" })
    ["x"] -> return (ExitDebugger, [], vimState { vsMessage = "Exiting debugger" })
    ["reg"] -> do
      regs <- getRegisters
      let output = logRegisters regs
      return (NoAction, output, vimState { vsMessage = "Displayed registers" })
    ["regs"] -> do
      regs <- getRegisters
      let output = logRegisters regs
      return (NoAction, output, vimState { vsMessage = "Displayed registers" })
    ["goto", addrStr] -> do
      case parseHexWord addrStr of
        Just addr -> do
          setPC_ addr
          return (NoAction, ["PC set to $" ++ showHex addr ""], vimState { vsCursor = addr, vsViewStart = addr, vsMessage = "PC set to $" ++ showHex addr "" })
        Nothing -> return (NoAction, ["Invalid address"], vimState { vsMessage = "Invalid address" })
    ["set", "pc", addrStr] -> do
      (action, output) <- handleSetPC addrStr ""
      return (action, output, vimState { vsCursor = vsCursor vimState, vsMessage = head output })
    ["ra", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rAC = val }) valStr "Accumulator" ""
      return (action, output, vimState { vsMessage = head output })
    ["rx", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rX = val }) valStr "X Register" ""
      return (action, output, vimState { vsMessage = head output })
    ["ry", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rY = val }) valStr "Y Register" ""
      return (action, output, vimState { vsMessage = head output })
    ["rsp", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rSP = val }) valStr "Stack Pointer" ""
      return (action, output, vimState { vsMessage = head output })
    ["rsr", valStr] -> do
      (action, output) <- handleSetReg8 (\r val -> r { rSR = val }) valStr "Status Register" ""
      return (action, output, vimState { vsMessage = head output })
    ["fill", startAddrStr, endAddrStr] -> do
      (action, output) <- handleFill [startAddrStr,  endAddrStr] ""
      return (action, output, vimState { vsMessage = head output })
    ["fill", startAddrStr, endAddrStr, byteStr] -> do
      (action, output) <- handleFill (startAddrStr : endAddrStr : [byteStr]) ""
      return (action, output, vimState { vsMessage = head output })
    ["fill", startAddrStr, endAddrStr, byteStr1, byteStr2] -> do
      (action, output) <- handleFill (startAddrStr : endAddrStr : [byteStr1, byteStr2]) ""
      return (action, output, vimState { vsMessage = head output })
    ["break"] -> do
      (action, output) <- handleBreak [] ""
      return (action, output, vimState { vsMessage = last output })
    ["bk"] -> do
      (action, output) <- handleBreak [] ""
      return (action, output, vimState { vsMessage = last output })
    ["break", addrStr] -> do
      (action, output) <- handleBreak [addrStr] ""
      return (action, output, vimState { vsMessage = last output })
    ["bk", addrStr] -> do
      (action, output) <- handleBreak [addrStr] ""
      return (action, output, vimState { vsMessage = last output })
    ["mem"] -> do
      (action, output) <- handleMemTrace [] ""
      return (action, output, vimState { vsMessage = last output })
    ["m"] -> do
      (action, output) <- handleMemTrace [] ""
      return (action, output, vimState { vsMessage = last output })
    ["mem", startAddrStr, endAddrStr] -> do
      (action, output) <- handleMemTrace [startAddrStr, endAddrStr] ""
      return (action, output, vimState { vsMessage = last output })
    ["mem", startAddrStr, endAddrStr, name] -> do
      (action, output) <- handleMemTrace [startAddrStr, endAddrStr, name] ""
      return (action, output, vimState { vsMessage = last output })
    ["trace"] -> do
      let newTraceState = not (enableTrace machine)
      put (machine { enableTrace = newTraceState })
      let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
      return (NoAction, output, vimState { vsMessage = head output })
    ["t"] -> do
      let newTraceState = not (enableTrace machine)
      put (machine { enableTrace = newTraceState })
      let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
      return (NoAction, output, vimState { vsMessage = head output })
    ["d"] -> do
      (action, output) <- handleDisassemble [] ""
      return (action, output, vimState { vsMessage = last output })
    ["d", addrStr] -> do
      (action, output) <- handleDisassemble [addrStr] ""
      return (action, output, vimState { vsMessage = last output })
    _ -> return (NoAction, ["Unknown command: " ++ command], vimState { vsMessage = "Unknown command: " ++ command })

interactiveLoopHelper :: DebuggerConsoleState -> FDX (DebuggerAction, VimState)
-- | Enhanced interactive loop with vim state management
-- interactiveLoopHelper :: DebuggerConsoleState -> FDX DebuggerAction
interactiveLoopHelper consoleState = do
  machine <- get
  if halted machine
    then return (QuitEmulator, vimState machine) -- Return QuitEmulator action if halted
    else do
      -- Initialize vim state with current PC (ideally, load from Machine)
      let vimState = initialVimState { vsCursor = rPC (mRegs machine), vsViewStart = rPC (mRegs machine) }
      liftIO $ renderVimScreen machine consoleState vimState
      key <- liftIO getKey
      (action, output, newVimState) <- handleVimKey key vimState
      let updatedConsoleState = consoleState { outputLines = outputLines consoleState ++ output }
      case action of
        ContinueLoop _ -> interactiveLoopHelper updatedConsoleState
        ExecuteStep _ -> return (action, newVimState)
        ExitDebugger -> return (action, newVimState)  
        QuitEmulator -> return (action, newVimState)
        NoAction -> interactiveLoopHelper updatedConsoleState
        SwitchToCommandMode -> return (action, newVimState)
        SwitchToVimMode -> interactiveLoopHelper updatedConsoleState


-- | Render screen with vim-specific cursor and status
renderVimScreen :: Machine -> DebuggerConsoleState -> VimState -> IO ()
renderVimScreen machine consoleState vimState = do
  ANSI.hideCursor
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0

  let linesPerPage = termHeight - 3 -- Reserve 2 lines for status, 1 for message
  let cursorPos = vsCursor vimState
  let viewStart = vsViewStart vimState

  -- Adjust viewStart to keep cursor in view
  let adjustedViewStart = case vsViewMode vimState of
        CodeView -> if cursorPos < viewStart
                    then max 0 (cursorPos - fromIntegral (linesPerPage `div` 2))
                    else if cursorPos >= viewStart + fromIntegral (linesPerPage * 3)
                         then cursorPos - fromIntegral (linesPerPage * 2)
                         else viewStart
        _ -> if cursorPos < viewStart
             then max 0 (cursorPos - fromIntegral (linesPerPage * 16 `div` 2))
             else if cursorPos >= viewStart + fromIntegral (linesPerPage * 16)
                  then cursorPos - fromIntegral (linesPerPage * 8)
                  else viewStart

  -- Render content based on view mode
  case vsViewMode vimState of
    CodeView -> do
      ((disassembledLines, _), _) <- runStateT (unFDX $ disassembleInstructions adjustedViewStart linesPerPage) machine
      let linesWithCursor = zipWith (\line addr -> 
                      if addr == cursorPos
                      then "\x1b[7m" ++ line ++ "\x1b[0m" -- Highlight cursor line
                      else line
                    ) disassembledLines [adjustedViewStart ..]
      -- Print lines with cursor highlighting
      mapM_ (\(line, row) -> do
        ANSI.setCursorPosition row 0
        putStr line
        hFlush stdout) (zip linesWithCursor [0..])

    MemoryView -> do
      let startAddr = adjustedViewStart
      let endAddr = min 0xFFFF (startAddr + fromIntegral (linesPerPage * 16 - 1))
      bytes <- runStateT (unFDX $ mapM fetchByteMem [startAddr .. endAddr]) machine
      let byteLines = chunkBytes (fst bytes) 16
      let linesWithAddr = zipWith (\addr line -> formatHex16 addr ++ ": " ++ unwords (map formatHex8 line)) [startAddr, startAddr + 16 ..] byteLines
      let linesWithCursor = zipWith (\line addr -> if cursorPos >= addr && cursorPos < addr + 16
            then let offset = fromIntegral (cursorPos - addr) * 3
                 in take offset line ++ "\x1b[7m" ++ take 2 (drop offset line) ++ "\x1b[0m" ++ drop (offset + 2) line
            else line) linesWithAddr [startAddr, startAddr + 16 ..]
      mapM_ (\(line, row) -> do
        ANSI.setCursorPosition row 0
        putStr line
        hFlush stdout) (zip linesWithCursor [0..])

    RegisterView -> do
      regs <- runStateT (unFDX getRegisters) machine
      let output = logRegisters (fst regs)
      mapM_ (\(line, row) -> do
        ANSI.setCursorPosition row 0
        putStr line
        hFlush stdout) (zip output [0..])

    StackView -> do
      let sp = rSP (mRegs machine)
      let stackStart = fromIntegral sp + 0x0100
      let stackEnd = min 0x01FF (stackStart + fromIntegral (linesPerPage * 16 - 1))
      bytes <- runStateT (unFDX $ mapM fetchByteMem [stackStart .. stackEnd]) machine
      let byteLines = chunkBytes (fst bytes) 16
      let linesWithAddr = zipWith (\addr line -> formatHex16 addr ++ ": " ++ unwords (map formatHex8 line)) [stackStart, stackStart + 16 ..] byteLines
      let linesWithCursor = zipWith (\line addr -> if cursorPos >= addr && cursorPos < addr + 16
            then let offset = fromIntegral (cursorPos - addr) * 3
                 in take offset line ++ "\x1b[7m" ++ take 2 (drop offset line) ++ "\x1b[0m" ++ drop (offset + 2) line
            else line) linesWithAddr [stackStart, stackStart + 16 ..]
      mapM_ (\(line, row) -> do
        ANSI.setCursorPosition row 0
        putStr line
        hFlush stdout) (zip linesWithCursor [0..])

  -- Status line
  ANSI.setCursorPosition (termHeight - 2) 0
  ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  let modeDisplay = case vsViewMode vimState of
        CodeView -> " CODE "
        MemoryView -> " MEMORY "
        RegisterView -> " REGISTERS "
        StackView -> " STACK "
  putStr modeDisplay
  let regs = mRegs machine
  let regDisplay = "A=" ++ showHex (rAC regs) "" ++ " X=" ++ showHex (rX regs) "" ++ " Y=" ++ showHex (rY regs) "" ++ " PC=" ++ showHex (rPC regs) ""
  putStr regDisplay
  let cursorDisplay = " $" ++ showHex cursorPos ""
  putStr cursorDisplay
  let countDisplay = case vsCount vimState of
        Just n -> " [" ++ show n ++ "]"
        Nothing -> ""
  putStr countDisplay
  let operatorDisplay = case vsOperator vimState of
        Just op -> " " ++ op
        Nothing -> ""
  putStr operatorDisplay
  let spacerLength = max 0 (termWidth - length modeDisplay - length regDisplay - length cursorDisplay - length countDisplay - length operatorDisplay)
  putStr $ replicate spacerLength ' '
  ANSI.setSGR [ANSI.Reset]
  hFlush stdout

  -- Message line
  ANSI.setCursorPosition (termHeight - 1) 0
  putStr (vsMessage vimState)
  hFlush stdout
  ANSI.showCursor

-- | Helper to format Word8 as two-character hex
formatHex8 :: Word8 -> String
formatHex8 b = let hexStr = showHex b "" in if length hexStr < 2 then "0" ++ hexStr else hexStr

-- | Helper to format Word16 as four-character hex
formatHex16 :: Word16 -> String
formatHex16 w = let hexStr = showHex w "" in replicate (4 - length hexStr) '0' ++ hexStr

-- | Chunk a list into sublists of given size
chunkBytes :: [Word8] -> Int -> [[Word8]]
chunkBytes [] _ = []
chunkBytes xs n = take n xs : chunkBytes (drop n xs) n