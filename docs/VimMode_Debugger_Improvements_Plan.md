# VimMode Debugger Improvements Plan

This plan outlines the architectural improvements for `VimMode` in the Haskell debugger, focusing on composable commands and integrating VICE-inspired colon commands.

## Current Architecture Overview:

The debugger utilizes two primary interaction modes: `CommandMode` and `VimMode`.

*   **`CommandMode` (`src/MOS6502Emulator/Debugger/Commands.hs`):** This mode handles a variety of text-based commands such as `step`, `regs`, `break`, `mem`, `fill`, and register manipulation. The `handleCommand` function serves as the central dispatcher for these commands.
*   **`VimMode` (`src/MOS6502Emulator/Debugger/VimMode/HandleKey.hs`, `src/MOS6502Emulator/Debugger/VimMode/Execute.hs`, `src/MOS6502Emulator/Debugger/VimMode/Core.hs`):** This mode is designed to mimic Vim's modal editing paradigm, offering normal mode operations, composable commands, and a `VimCommandMode` (for colon commands).
    *   `src/MOS6502Emulator/Debugger/VimMode/Core.hs` defines the core data structures like `VimState`, `Motion`, `Action`, and `CommandState`, which are crucial for managing the modal behavior and command composition.
    *   `src/MOS6502Emulator/Debugger/VimMode/HandleKey.hs` is responsible for processing key presses in `VimMode`'s normal and operator-pending states. It correctly implements the state machine for composable commands (e.g., `diw`) and handles various direct actions and movements. It also initiates `VimCommandMode` when `:` is pressed.
    *   `src/MOS6502Emulator/Debugger/VimMode/Execute.hs` contains the logic for executing `Action`s (like `Set`, `Increment`, `Delete`, `Change`, `Yank`) and `Motion`s (like `NextInstruction`, `PrevByte`, `WordForward`).

## Areas for Improvement and Alignment with "Vim Language" Philosophy:

1.  **Colon Command Unification:** There's a functional overlap between the existing `CommandMode` commands and the intended `VimMode` colon commands (e.g., `:break`, `:step`, `:regs`). The current `VimCommandMode` handling in `HandleKey.hs` is rudimentary.
2.  **Completing VICE-inspired Commands:** While some commands like `:break` and `:watch` are noted, the full set of VICE debugger commands (`:step`, `:regs`, `:disas`) needs explicit implementation and integration into `VimCommandMode`.
3.  **Text Object Granularity:** The `TextObject` motion type is defined, but its execution for `Word`, `Line`, `Bracket`, and `Quote` needs to be robustly implemented in `executeMotion` to accurately define the affected memory or instruction ranges for composable operations like `d`, `c`, `y`.

## Proposed Architectural Improvements:

The core idea is to enhance `VimMode` by:

1.  **Centralizing Command Handling:** Unifying the parsing and execution of colon commands so that `VimCommandMode` can leverage the existing robust command handlers from `src/MOS6502Emulator/Debugger/Commands.hs`.
2.  **Expanding Composable Actions:** Ensuring text objects are fully functional and precise for memory and CPU state manipulation.

## Detailed Plan:

**Phase 1: Deep Dive and Command Unification**

1.  **Define `VimCommand` Data Type:**
    *   **Goal:** Create a new structured data type to represent parsed Vim colon commands with their arguments.
    *   **Proposed Code Change (in `src/MOS6502Emulator/Debugger/VimMode/Core.hs`):**
        ```haskell
        -- Add to MOS6502Emulator.Debugger.VimMode.Core.hs
        data VimCommand =
            VBreak (Maybe Word16) -- :break [addr]
          | VWatch (Maybe (Word16, Word16)) -- :watch [start-end]
          | VStep (Maybe Int) -- :step [count]
          | VRegs -- :regs
          | VDisas (Maybe Word16) (Maybe Word16) -- :disas [start [end]]
          | VFill Word16 Word16 [Word8] -- :fill start end byte1 [byte2...]
          | VSetReg8 Char Word8 -- :ra val, :rx val, :ry val, :rsp val, :rsr val
          | VSetPC Word16 -- :rpc val
          | VQuit -- :quit / :q
          | VExit -- :exit / :x
          | VTrace -- :trace / :t
          | VUnknown String -- For unparsed or invalid commands
          deriving (Eq, Show)

        -- Update Action data type to include ExecuteVimCommand
        data Action = Move Motion | SetView ViewMode | EnterCommandMode | ExecuteCommand | Step | ToggleBreakpoint | ToggleMemTrace | StoreAddress | GotoAddress | SearchForward | SearchBackward | RepeatSearch | RepeatSearchReverse
                    | Set Word8 | Increment Int | Decrement Int | ToggleBit Int
                    | AddBreakpoint | RemoveBreakpoint | Delete Motion | Change Motion
                    | Yank Motion | Paste Bool | ExecuteToHere
                    | ColonCommand VimCommand -- New action to execute parsed Vim commands
          deriving (Eq, Show)
        ```
    *   **Reasoning:** This centralizes the definition of all colon commands, making parsing and execution more structured and maintainable. `ColonCommand VimCommand` in `Action` allows the `executeAction` function to handle these commands.

2.  **Create `parseVimCommand` Function:**
    *   **Goal:** Implement a parser to convert raw colon command strings into the structured `VimCommand` data type.
    *   **Proposed New Module (`src/MOS6502Emulator/Debugger/VimMode/CommandParser.hs`):**
        ```haskell
        module MOS6502Emulator.Debugger.VimMode.CommandParser ( parseVimCommand ) where

        import Data.Word (Word8, Word16)
        import Data.Maybe (listToMaybe, mapMaybe)
        import Numeric (readHex)
        import Data.List (stripPrefix, isPrefixOf)
        import MOS6502Emulator.Debugger.VimMode.Core (VimCommand(..))
        import MOS6502Emulator.Debugger.Utils (parseHexWord, parseHexByte)

        parseVimCommand :: String -> VimCommand
        parseVimCommand cmdStr =
            let tokens = words (dropWhile (==':') cmdStr) -- Remove leading ':'
            in case tokens of
                ["break"] -> VBreak Nothing
                ["break", addrStr] -> VBreak (parseHexWord addrStr)
                ["watch"] -> VWatch Nothing
                ["watch", rangeStr] ->
                    case span (/='-') rangeStr of
                        (startStr, '-' : endStr) ->
                            case (parseHexWord startStr, parseHexWord endStr) of
                                (Just startAddr, Just endAddr) -> VWatch (Just (startAddr, endAddr))
                                _ -> VUnknown cmdStr
                        _ -> VUnknown cmdStr
                ["step"] -> VStep (Just 1)
                ["step", countStr] -> VStep (fmap read (listToMaybe (words countStr))) -- Assuming count is a decimal int
                ["regs"] -> VRegs
                ["disas"] -> VDisas Nothing Nothing
                ["disas", startStr] -> VDisas (parseHexWord startStr) Nothing
                ["disas", startStr, endStr] -> VDisas (parseHexWord startStr) (parseHexWord endStr)
                ("fill":startStr:endStr:byteStrs) ->
                    case (parseHexWord startStr, parseHexWord endStr) of
                        (Just startAddr, Just endAddr) ->
                            VFill startAddr endAddr (mapMaybe parseHexByte byteStrs)
                        _ -> VUnknown cmdStr
                ["ra", valStr] -> VSetReg8 'A' <$> parseHexByte valStr `orElse` VUnknown cmdStr
                ["rx", valStr] -> VSetReg8 'X' <$> parseHexByte valStr `orElse` VUnknown cmdStr
                ["ry", valStr] -> VSetReg8 'Y' <$> parseHexByte valStr `orElse` VUnknown cmdStr
                ["rsp", valStr] -> VSetReg8 'S' <$> parseHexByte valStr `orElse` VUnknown cmdStr
                ["rsr", valStr] -> VSetReg8 'P' <$> parseHexByte valStr `orElse` VUnknown cmdStr -- 'P' for processor status register
                ["rpc", valStr] -> VSetPC <$> parseHexWord valStr `orElse` VUnknown cmdStr
                ["quit"] -> VQuit
                ["q"] -> VQuit
                ["exit"] -> VExit
                ["x"] -> VExit
                ["trace"] -> VTrace
                ["t"] -> VTrace
                _ -> VUnknown cmdStr
            where
              orElse :: Maybe a -> b -> Either b a
              orElse (Just x) _ = Right x
              orElse Nothing y = Left y
              (Right x) `orElse` _ = x -- For proper chaining
              (Left y) `orElse` z = z -- For proper chaining
        ```
    *   **Reasoning:** This module encapsulates the parsing logic, keeping `HandleKey.hs` focused on state transitions and `Execute.hs` on execution. It leverages `parseHexWord` and `parseHexByte` for consistency.

3.  **Refactor `handleCommand` into `executeVimCommand`:**
    *   **Goal:** Create a new function that takes a parsed `VimCommand` and executes it, leveraging the existing handlers in `src/MOS6502Emulator/Debugger/Commands.hs`.
    *   **Proposed Code Change (in `src/MOS6502Emulator/Debugger/VimMode/Execute.hs`):**
        ```haskell
        -- Add to MOS6502Emulator.Debugger.VimMode.Execute.hs
        import MOS6502Emulator.Debugger.VimMode.CommandParser (parseVimCommand) -- New import
        import MOS6502Emulator.Debugger.Commands (handleBreak, handleMemTrace, handleFill, handleSetReg8, handleSetPC, handleDisassemble) -- Explicit imports
        import MOS6502Emulator.Debugger.Utils (logRegisters) -- For VRegs

        executeVimCommand :: VimCommand -> FDX (DebuggerAction, [String])
        executeVimCommand cmd = do
          machine <- get
          case cmd of
            VBreak addr -> handleBreak (maybe [] (return . formatHex16) addr) ""
            VWatch (Just (startAddr, endAddr)) -> handleMemTrace [formatHex16 startAddr, formatHex16 endAddr] ""
            VWatch Nothing -> handleMemTrace [] "" -- List trace blocks
            VStep count -> return (ExecuteStep (show (fromMaybe 1 count)), [])
            VRegs -> do
              regs <- getRegisters
              return (NoAction, logRegisters regs)
            VDisas start end -> handleDisassemble (mapMaybe (fmap formatHex16) [start, end]) ""
            VFill start end bytes -> handleFill (map formatHex16 [start, end] ++ map (printf "%02X") bytes) ""
            VSetReg8 regChar val ->
                let regSetter = case regChar of
                                  'A' -> (\r v -> r { rAC = v })
                                  'X' -> (\r v -> r { rX = v })
                                  'Y' -> (\r v -> r { rY = v })
                                  'S' -> (\r v -> r { rSP = v })
                                  'P' -> (\r v -> r { rSR = v })
                                  _   -> id -- Should not happen due to parser
                in handleSetReg8 regSetter (printf "%02X" val) [regChar] ""
            VSetPC addr -> handleSetPC (formatHex16 addr) ""
            VQuit -> return (QuitEmulator, [])
            VExit -> return (ExitDebugger, [])
            VTrace -> do
              let newTraceState = not (enableTrace machine)
              put (machine { enableTrace = newTraceState })
              let output = ["Tracing " ++ if newTraceState then "enabled." else "disabled."]
              return (NoAction, output)
            VUnknown cmdStr -> return (NoAction, ["Unknown command: " ++ cmdStr])

        -- Update executeAction to handle ColonCommand
        executeAction :: Action -> Word16 -> VimState -> FDX (Word16, [String])
        executeAction action currentPos vimState = do
          machine <- get
          case action of
            -- Existing cases...
            ColonCommand vimCmd -> do
                (dbgAction, output) <- executeVimCommand vimCmd
                -- Need to handle dbgAction potentially changing the debugger mode or state
                -- For now, just pass it through or convert to NoAction if it means staying in VimMode
                return (currentPos, output)
            -- ... other actions
        ```
    *   **Reasoning:** This creates a clear separation of concerns, with `executeVimCommand` acting as an adapter between the structured `VimCommand` and the existing string-based command handlers.

4.  **Integrate Parser and Executor into `HandleKey.hs`:**
    *   **Goal:** Connect the colon command input in `VimCommandMode` to the new parsing and execution logic.
    *   **Proposed Code Change (in `src/MOS6502Emulator/Debugger/VimMode/HandleKey.hs`):**
        ```haskell
        -- Modify MOS6502Emulator.Debugger.VimMode.HandleKey.hs
        -- Import new modules
        import MOS6502Emulator.Debugger.VimMode.CommandParser (parseVimCommand)

        -- Inside handleVimNormalModeKey, CommandModeV case:
            CommandModeV ->
                case key of
                    '\x1b' ->  -- Escape
                          return (NoAction, [""], vimState {
                              vsCommandState = NoCommand,
                              vsCommandBuffer = "",
                              vsInCommandMode = False -- Reset in command mode flag
                          }, debuggerConsoleState, initialDebuggerMode)
                    '\n' -> do  -- Enter
                        let cmdStr = vsCommandBuffer vimState
                        let parsedCmd = parseVimCommand cmdStr
                        (dbgAction, output) <- executeAction (ColonCommand parsedCmd) currentPos vimState -- Use executeAction
                        let newState = vimState { vsCommandState = NoCommand, vsCommandBuffer = "", vsInCommandMode = False }
                        return (dbgAction, output, newState, debuggerConsoleState, initialDebuggerMode)
                    _ -> return (NoAction, [""], vimState {vsCommandBuffer = vsCommandBuffer vimState ++ [key]}, debuggerConsoleState, initialDebuggerMode)
        ```
    *   **Reasoning:** This updates the `VimCommandMode` state machine to correctly parse the command buffer and execute it using the new `executeVimCommand` via `executeAction`. It also properly resets the `vsInCommandMode` flag.

**Phase 2: Enhancing Command Functionality and Integration**

5.  **Refine `Execute.hs` for Text Objects:**
    *   **Goal:** Ensure that the `TextObject` motion accurately identifies the start and end addresses for `Delete`, `Change`, and `Yank` operations.
    *   **Proposed Code Change (in `src/MOS6502Emulator/Debugger/VimMode/Execute.hs`):**
        ```haskell
        -- Modify MOS6502Emulator.Debugger.VimMode.Execute.hs
        -- Inside executeMotion, TextObject case:
        TextObject mod objType n -> do
          case vsViewMode vimState of -- Assume vimState is available or passed down
            CodeView -> do
              currentMachine <- get
              let currentPC = vsCursor vimState -- Use vimState cursor
              case objType of
                Word -> do
                  -- For word, interpret as instruction. n is the count of instructions.
                  -- Find the start of the current instruction
                  let (precedingInsts, _) = disassembleInstructionsFromBeginning currentPC (opcodeMap :: Map Word8 InstructionInfo)
                  let startOfCurrentInst = case reverse precedingInsts of
                                             ((addr, _, _):_) -> addr
                                             _ -> currentPC -- Fallback

                  -- Find the nth instruction's end address
                  (_, endAddr) <- disassembleInstructions startOfCurrentInst n
                  return (endAddr - 1) -- Return end of the nth instruction
                Line -> do
                  -- For line, interpret as instruction. n is the count of instructions.
                  -- Similar to Word, but consider the full instruction as a "line"
                  let (precedingInsts, _) = disassembleInstructionsFromBeginning currentPC (opcodeMap :: Map Word8 InstructionInfo)
                  let startOfCurrentInst = case reverse precedingInsts of
                                             ((addr, _, _):_) -> addr
                                             _ -> currentPC

                  (_, endAddr) <- disassembleInstructions startOfCurrentInst n
                  return (endAddr - 1)

                Bracket -> do
                  -- Find matching parentheses/brackets within the current instruction's operands.
                  -- This is a more complex parsing task, might need to iterate through the disassembled
                  -- string to find '(', ')', '[', ']'
                  -- For simplicity initially, let's assume it operates on the current instruction.
                  (instStr, instLen) <- disassembleInstruction currentPC
                  let openBracketIndex = findIndex (flip elem "([") instStr
                  let closeBracketIndex = findIndex (flip elem ")]") instStr
                  case (openBracketIndex, closeBracketIndex) of
                    (Just open, Just close) | open < close ->
                      let startByte = currentPC + fromIntegral (open `div` 3) -- Rough estimate of byte offset
                          endByte = currentPC + fromIntegral (close `div` 3)
                      in return endByte
                    _ -> return currentPC -- No brackets found in current instruction, or invalid
                Quote -> do
                  -- Similar to Bracket, but for quotes "..."
                  (instStr, instLen) <- disassembleInstruction currentPC
                  let openQuoteIndex = findIndex (=='"') instStr
                  let closeQuoteIndex = findIndex (=='"') (drop (fromMaybe 0 openQuoteIndex + 1) instStr)
                  case (openQuoteIndex, fmap (+ (fromMaybe 0 openQuoteIndex + 1)) closeQuoteIndex) of
                    (Just open, Just close) | open < close ->
                      let startByte = currentPC + fromIntegral (open `div` 3)
                          endByte = currentPC + fromIntegral (close `div` 3)
                      in return endByte
                    _ -> return currentPC
            MemoryView -> do
              -- For MemoryView, operations are byte-based.
              -- 'Word' and 'Line' can be interpreted as fixed blocks of bytes or as address ranges.
              case objType of
                Word -> return $ min maxAddr (currentPos + fromIntegral (n * 2)) -- Example: 2 bytes per "word"
                Line -> return $ min maxAddr (currentPos + fromIntegral (n * 16)) -- Example: 16 bytes per "line"
                Bracket -> return currentPos -- Not directly applicable, return current pos
                Quote -> return currentPos -- Not directly applicable, return current pos

        -- Helper to find instruction start (needed for Word/Line)
        disassembleInstructionsFromBeginning :: Word16 -> Map Word8 InstructionInfo -> [(Word16, String, Int)]
        disassembleInstructionsFromBeginning startAddr opMap = go startAddr []
          where
            go addr acc
              | addr > 0xFFFF = reverse acc
              | otherwise = case Map.lookup (machineMem (initialMachine { mMem = undefined }) addr) opMap of -- Simplified for plan, actual fetchByteMem
                  Just (opcode, instLen) ->
                    let nextAddr = addr + fromIntegral instLen
                    in go nextAddr ((addr, "", instLen) : acc) -- Store addr, empty string (for now), length
                  Nothing -> reverse acc -- Stop if invalid opcode (or if out of bounds)
        ```
    *   **Reasoning:** This provides a concrete approach to implementing `TextObject` motions, distinguishing between `CodeView` (instruction-based) and `MemoryView` (byte-based) operations. The `Bracket` and `Quote` operations are simplified for now, focusing on their appearance in disassembled instructions.

6.  **Update `vimModeHelp`:**
    *   **Goal:** Provide accurate and comprehensive documentation for all new and enhanced `VimMode` commands.
    *   **Proposed Code Change (in `src/MOS6502Emulator/Debugger/VimMode/Core.hs`):**
        ```haskell
        -- Modify MOS6502Emulator.Debugger.VimMode.Core.hs
        vimModeHelp :: String
        vimModeHelp = unlines
          [ "Vim Mode Commands:"
          , ""
          , "  Navigation:"
          , "    h/j/k/l   Move cursor (left/down/up/right)"
          , "    w/b       Word forward/backward (code view, by instruction)"
          , "    e         End of word (code view)"
          , "    0/$       Start/end of line (code view)"
          , "    gg/G      Start/end of view"
          , "    H/M/L     Top/middle/bottom of page"
          , "    Ctrl+u/d  Scroll page up/down"
          , "    gp        Go to Program Counter (PC)"
          , "    g<addr>   Go to address (e.g., g1234)"
          , "    f/F<byte> Find byte forward/backward"
          , "    t/T<byte> Till byte forward/backward"
          , "    ;/ ,      Repeat last find/till"
          , "    '<char>   Go to mark <char>"
          , ""
          , "  Editing/Manipulation:"
          , "    r<byte>   Replace byte under cursor"
          , "    d<motion> Delete with motion (e.g., dw, d$, dG, diw, caw)"
          , "    c<motion> Change with motion (e.g., cw, c$, cG, ciw, caw)"
          , "    y<motion> Yank with motion (e.g., yw, y$, yG, yiw, yaw)"
          , "    p/P       Paste after/before cursor"
          , "    x         Delete character under cursor"
          , "    ~         Toggle bit under cursor"
          , "    + / -     Increment/Decrement byte"
          , "    .         Repeat last change"
          , "    \"<reg>   Specify register for next op"
          , ""
          , "  Visual Mode:"
          , "    v         Char visual mode"
          , "    V         Line visual mode"
          , "    Ctrl+v    Block visual mode"
          , "    d/c/y     Delete/Change/Yank selection"
          , ""
          , "  Views:"
          , "    c/m/r/s   View code/memory/registers/stack"
          , "    v         Cycle views"
          , ""
          , "  Execution:"
          , "    s         Step instruction"
          , "    <CR>      Execute until cursor (Enter key)"
          , ""
          , "  Debugger Features (Colon Commands):"
          , "    :break [<addr>]       Set/list/remove breakpoint at <addr>"
          , "    :watch [<start>-<end>]  Trace memory range"
          , "    :step [<count>]       Execute <count> instructions (default 1)"
          , "    :regs                 Show current register values"
          , "    :disas [<start> [<end>]] Disassemble memory range (default 32 from last PC)"
          , "    :fill <start> <end> <byte1> [...] Fill memory range with bytes"
          , "    :ra <val>             Set Accumulator to hex value"
          , "    :rx <val>             Set X register to hex value"
          , "    :ry <val>             Set Y register to hex value"
          , "    :rsp <val>            Set Stack Pointer to hex value"
          , "    :rsr <val>            Set Status Register to hex value"
          , "    :rpc <val>            Set Program Counter to hex value"
          , "    :quit / :q            Quit emulator"
          , "    :exit / :x            Exit debugger (return to emulator)"
          , "    :trace / :t           Toggle instruction tracing"
          , "    :m<char>              Set mark <char>"
          , ""
          , "  General:"
          , "    <count>   Repeat command (e.g., 5j, d2w)"
          , "    Escape    Cancel pending command/mode"
          , "    .         Repeat last change"
          ]
        ```
    *   **Reasoning:** This provides up-to-date and comprehensive documentation for the user, reflecting the new capabilities and clarifying existing ones.

7.  **Review `Debugger.hs` `interactiveLoopHelper`:**
    *   **Goal:** Ensure clean separation of concerns between `Debugger.hs` and `VimMode`'s input loop.
    *   **Proposed Review/Consideration (in `src/MOS6502Emulator/Debugger.hs`):**
        *   The `interactiveLoopHelper` in `Debugger.hs` currently handles `CommandMode` and explicitly switches to `VimMode` on 'v'. It also contains `readCommand` and direct command parsing.
        *   With `VimMode` having its own `VimCommandMode` and `executeVimCommand`, the `Debugger.hs` `handleCommand` will primarily be used for the legacy `CommandMode` interactions.
        *   No immediate code change is proposed for `Debugger.hs` in this planning phase, but it's important to be aware that once `VimMode`'s colon commands are fully implemented, the `Debugger.hs` `handleCommand` might be further simplified or `CommandMode` could be deprecated in favor of `VimMode`'s comprehensive colon command support. The `readCommand` function can likely remain as it serves the `CommandMode` which might still be desired as a fallback or alternative.

## Mermaid Diagrams:

```mermaid
graph TD
    subgraph Debugger Interaction Flow
        A[User Input Key] --> B{Current Debugger Mode?};
        B -- CommandMode --> C[interactiveLoopHelper];
        C --> D[readCommand];
        D --> E[handleCommand (Debugger.Commands.hs)];
        E --> F{Execute DebuggerAction};
    end

    subgraph VimMode Command Flow
        B -- VimMode --> G[handleVimNormalModeKey (HandleKey.hs)];
        G -- Key is ':' --> H[Switch to VimCommandMode (vsInCommandMode = True)];
        H --> I[Collect Command Buffer (vsCommandBuffer)];
        I -- Enter Pressed --> J[parseVimCommand (NEW CommandParser.hs)];
        J -- Parsed VimCommand --> K[executeVimCommand (Execute.hs)];
        K --> E;
        K -- Invalid Command --> L[Display Error Message];
    end

    subgraph VimMode Composable Action Flow
        G -- Key is Motion/Action --> M[executeMotion / executeAction (Execute.hs)];
        M --> F;
    end
```

```mermaid
stateDiagram-v2
    direction LR
    [*] --> NormalMode
    NormalMode --> OperatorPending: d, c, y
    NormalMode --> CommandModeV: :
    NormalMode --> VisualMode: v
    NormalMode --> ExecuteNormalAction: (j, k, r, +, -, ~, B, etc.)
    NormalMode --> CountPending: Digit
    NormalMode --> VimPendingCompose: "

    CountPending --> NormalMode: Non-digit key (applies count to next action)
    CountPending --> CountPending: Digit

    OperatorPending --> ObjectPending: i, a
    OperatorPending --> ExecuteActionWithMotion: Motion key (e.g., dd, dw)

    ObjectPending --> ExecuteActionWithTextObject: Object key (w, l, b, ")

    VisualMode --> ExecuteVisualAction: d, c, y (on selection)
    VisualMode --> NormalMode: Escape

    CommandModeV --> NormalMode: Escape
    CommandModeV --> ParseAndExecuteVimCommand: Enter (on command buffer)
    CommandModeV --> CommandModeV: Any other key (append to buffer)

    VimPendingCompose --> NormalMode: Escape (cancel)
    VimPendingCompose --> NormalMode: Any key (set register)

    ExecuteNormalAction --> NormalMode
    ExecuteActionWithMotion --> NormalMode
    ExecuteActionWithTextObject --> NormalMode
    ParseAndExecuteVimCommand --> NormalMode
    ExecuteVisualAction --> NormalMode