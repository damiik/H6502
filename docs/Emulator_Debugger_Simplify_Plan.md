### Current Architecture Analysis

#### Debugger Commands I/O

- __Input Handling__: The debugger operates in an interactive loop defined in `Debugger.hs` with `interactiveLoopHelper`. It reads user input character by character using `readCommandWithInitialInput`, which handles raw terminal input without echo, ensuring direct control over command entry. Commands are processed in `handleCommand` (from `Debugger/Commands.hs`), which parses input strings and dispatches to specific handlers like `handleBreak` for breakpoints or `handleStep` for instruction stepping.
- __Output Management__: Output is managed through `renderScreen` (from `Display.hs`), which updates the terminal display with disassembled code on the left and console output (including command results and register states) on the right. The `DebuggerConsoleState` in `Debugger/Core.hs` maintains output lines, input buffer, and cursor position, updated after each command execution.
- __Mode Switching__: The debugger supports multiple modes (`CommandMode`, `VimMode`, `VimCommandMode`) defined in `Debugger/Core.hs`. Mode transitions (e.g., switching to Vim mode with the 'v' command) are handled within the interactive loop, altering how input is interpreted and processed.

#### State Management

- __CPU Registers__: Stored in the `Registers` data type within the `Machine` record (from `Machine.hs`). The `FDX` monad (a `StateT Machine IO`) facilitates state updates using functions like `setPC_`, `setAC_`, etc., which directly modify the `mRegs` field.
- __Console State__: Managed via `DebuggerConsoleState`, which tracks output lines, input buffer, and last command. This state is updated after each command to reflect the latest interaction.
- __Breakpoints and Memory Trace Areas__: Stored as lists in `Machine` (`breakpoints` and `memoryTraceBlocks`). Commands like `handleBreak` and `handleMemTrace` modify these lists directly via `put` or `modify` operations in the `FDX` monad.
- __History of CPU Steps__: Tracked via `pcHistory` and `redoHistory` in `Machine`, allowing navigation through past program counter values, though the provided code doesn't fully show how these are updated or used (likely in Vim mode navigation).
- __Overall State__: The `Machine` data type encapsulates all emulator state, including debugger-specific fields. State updates are imperative, using `modify` or `put` to alter the `Machine` record in place within the `FDX` monad.

#### Current Style

The code heavily relies on imperative state manipulation within the `StateT` monad, with frequent use of `get`, `put`, and `modify`. Command handling involves side effects (e.g., terminal I/O) mixed with state updates, which is typical for Haskell's monadic style but can obscure the logic flow and make the code harder to reason about.

### Simplification Using Functional Programming Paradigms

To simplify the debugger code, I propose leveraging functional programming principles such as immutability, referential transparency, and declarative programming. Here are the key areas for improvement and the proposed approach:

1. __State Management with Immutable Data Structures and Lenses__

   - __Current Issue__: The frequent use of `modify` and `put` for state updates is imperative and can lead to complex state mutation logic that's hard to track.
   - __Proposed Solution__: Use lenses (from the `lens` library) to provide a more declarative way to update nested fields in the `Machine` and `DebuggerConsoleState` records. Lenses allow for composable, focused updates to immutable data structures, reducing the risk of unintended side effects.
     - Example: Instead of `modify (\m -> m { mRegs = (mRegs m) { rPC = addr } })`, use a lens like `mRegs . rPC .~ addr` to update the program counter.
   - __Benefit__: This approach makes state updates more readable and less error-prone, aligning with functional purity by treating state as a series of transformations rather than mutations.

2. __Separate Pure Logic from I/O__

   - __Current Issue__: Command handling mixes pure logic (e.g., parsing input, deciding actions) with I/O operations (e.g., rendering the screen, reading input).
   - __Proposed Solution__: Refactor the code to separate pure functions for command parsing and state transformation from I/O actions. Define pure functions that take the current `Machine` state and command input, returning a new state and a list of actions to perform (e.g., render, output text).
     - Example: A pure `handleCommandPure :: Machine -> String -> (Machine, [String], DebuggerAction)` could compute the new state and outputs, while an I/O wrapper executes rendering or input reading.
   - __Benefit__: This separation enhances testability (pure functions can be tested without I/O) and clarity, adhering to functional programming's emphasis on referential transparency.

3. __Use Algebraic Data Types for Command Representation__

   - __Current Issue__: Commands are parsed as strings with pattern matching in `handleCommand`, leading to verbose and error-prone code.
   - __Proposed Solution__: Define an ADT for commands, such as `data DebuggerCommand = Step | Break Word16 | MemTrace Word16 Word16 (Maybe String) | ...`, and write a parser to convert input strings to this type. Then, handle commands with a single `executeCommand :: DebuggerCommand -> Machine -> (Machine, [String], DebuggerAction)`.
   - __Benefit__: This approach reduces parsing errors, makes command handling more type-safe, and centralizes logic, aligning with functional programming's use of strong typing.

4. __Simplify Interactive Loop with a State Machine__

   - __Current Issue__: The `interactiveLoopHelperInternal` function in `Debugger.hs` is complex due to nested conditionals for mode handling and help text scrolling.
   - __Proposed Solution__: Model the debugger as a state machine with explicit states (e.g., `WaitingForInput`, `DisplayingHelp`, `ExecutingCommand`) and transitions driven by user input or actions. Use a pure function to compute the next state and actions, then execute I/O based on those actions.
   - __Benefit__: This declarative state machine approach reduces complexity and makes the control flow easier to reason about, a common functional programming technique.

5. __Optimize Console Output with Circular Buffers__

   - __Current Issue__: The `outputLines` list in `DebuggerConsoleState` is capped using `take maxConsoleOutputLines`, which is a start but still involves list concatenation (`++`), an O(n) operation.
   - __Proposed Solution__: Use a more efficient data structure like a `Seq` from `Data.Sequence` for output lines, which supports efficient appending and truncation, maintaining functional purity while improving performance.
   - __Benefit__: This leverages functional data structures to handle state more efficiently without sacrificing immutability.

6. __Refactor History Management__

   - __Current Issue__: `pcHistory` and `redoHistory` management isn't fully visible in the provided code, but likely involves imperative list updates.
   - __Proposed Solution__: Treat history as an immutable stack or zipper data structure, where stepping back or forward is a pure transformation of the history state. Define pure functions to update history on each step.
   - __Benefit__: This aligns with functional programming by avoiding mutable state and making history navigation predictable and reversible.

### Proposed Plan for Implementation

1. __Introduce Lenses for State Updates__

   - Add the `lens` library to the project dependencies.
   - Define lenses for `Machine` and nested records like `Registers` and `DebuggerConsoleState`.
   - Replace `modify` calls with lens-based updates in command handlers and state transitions.

2. __Refactor Command Handling__

   - Define a `DebuggerCommand` ADT and write a parser for user input.
   - Implement a pure `executeCommand` function to handle state transitions and output generation.
   - Wrap I/O operations in a thin layer around the pure logic for rendering and input handling.

3. __Model Debugger as a State Machine__

   - Define a `DebuggerState` ADT for different modes and sub-states (e.g., help display).
   - Write a pure transition function `nextState :: DebuggerState -> Input -> (DebuggerState, [Action])`.
   - Implement the interactive loop to execute actions (I/O) based on state transitions.

4. __Optimize Data Structures__

   - Replace `outputLines` list with `Data.Sequence.Seq` for efficient output management.
   - If needed, implement a zipper or similar structure for `pcHistory` and `redoHistory`.

5. __Document and Test__

   - Add Haddock documentation for new ADTs and functions to maintain clarity.
   - Write unit tests for pure functions (e.g., command parsing, state transitions) using a framework like HUnit or QuickCheck.

### Conclusion

The current debugger implementation, while functional in the monadic sense, can be significantly simplified by adopting more declarative and pure functional programming techniques. By using lenses, separating pure logic from I/O, defining ADTs for commands, and modeling the debugger as a state machine, the codebase can become more maintainable, testable, and aligned with Haskell's functional paradigm.


### Incremental Update Plan for Emulator/Debugger

To update the debugger module incrementally, I've broken down the refactoring process into simple, manageable steps. Each step focuses on a specific aspect of the codebase, allowing for testing and validation before moving to the next. This approach minimizes the risk of introducing bugs and ensures the code remains functional at each stage.

#### Step 1: Introduce Lenses for State Updates

- __Objective__: Replace imperative state modifications with declarative lens-based updates for better readability and maintainability.

- __Actions__:

  1. Add the `lens` library to the project dependencies in `H6502.cabal` or `package.yaml`.
  2. Define lenses for the `Machine`, `Registers`, and `DebuggerConsoleState` data types in a new utility module, e.g., `src/MOS6502Emulator/Lenses.hs`.
  3. Update a single, simple state modification function (e.g., `setPC_` in `Machine.hs`) to use lenses instead of direct record updates. For example, change `modify (\m -> m { mRegs = (mRegs m) { rPC = val } })` to `modify (mRegs . rPC .~ val)` using the lens.

- __Validation__: Test the emulator to ensure that setting the program counter still works correctly. Run a simple test case or use the debugger to verify the PC updates as expected.

- __Why Incremental__: This step focuses on one state update function, limiting the scope of change and allowing you to verify the lens approach before applying it broadly.

#### Step 2: Apply Lenses to Remaining State Updates

- __Objective__: Extend the use of lenses to all state update functions in the codebase.

- __Actions__:

  1. Update other register setters (`setAC_`, `setX_`, etc.) in `Machine.hs` to use lenses.
  2. Refactor state updates in `Debugger/Commands.hs` (e.g., in `handleBreak` or `handleMemTrace`) to use lenses for modifying `breakpoints` and `memoryTraceBlocks`.

- __Validation__: Test debugger commands like setting breakpoints and memory trace blocks to ensure state updates are correct. Run the emulator with tracing enabled to confirm no regressions.

- __Why Incremental__: This builds on Step 1, applying the same pattern to other state updates, ensuring consistency while still keeping changes focused and testable.

#### Step 3: Define a Debugger Command ADT

- __Objective__: Replace string-based command parsing with a type-safe algebraic data type (ADT) for commands.

- __Actions__:

  1. Create a new module, e.g., `src/MOS6502Emulator/Debugger/Types.hs`, to define a `DebuggerCommand` ADT like `data DebuggerCommand = Step | Break Word16 | MemTrace Word16 Word16 (Maybe String) | Quit | ...`.
  2. Write a parser function in the same module to convert user input strings to `DebuggerCommand` values, handling the current command formats (e.g., "step", "bk 0x0400").
  3. Update `handleCommand` in `Debugger/Commands.hs` to accept a `DebuggerCommand` instead of a raw string, initially just for a subset of commands (e.g., `Step`, `Quit`).

- __Validation__: Test the debugger with basic commands (e.g., "step", "quit") to ensure they are parsed and executed correctly. Verify that unhandled commands still fall back to the existing string-based logic.

- __Why Incremental__: This step introduces the ADT for a limited set of commands, allowing you to test the type-safe approach without rewriting all command handling at once.

#### Step 4: Extend Command ADT to All Commands

- __Objective__: Fully transition command handling to use the `DebuggerCommand` ADT.

- __Actions__:

  1. Extend the parser to handle all debugger commands as defined in the ADT.
  2. Refactor the remaining logic in `handleCommand` to process all commands via the ADT, removing the old string pattern matching.

- __Validation__: Run comprehensive tests on all debugger commands (breakpoints, memory tracing, register setting, etc.) to ensure full functionality. Use the debugger interactively to confirm behavior.

- __Why Incremental__: This builds on Step 3, completing the transition to type-safe command handling after validating the initial subset, reducing the risk of breaking functionality.

#### Step 5: Separate Pure Logic from I/O in Command Handling

- __Objective__: Isolate pure state transformation logic from I/O operations for better testability and clarity.

- __Actions__:

  1. Refactor `handleCommand` in `Debugger/Commands.hs` into a pure function `handleCommandPure :: Machine -> DebuggerCommand -> (Machine, [String], DebuggerAction)` that computes the new state and outputs without performing I/O.
  2. Create a thin I/O wrapper in `Debugger.hs` (within `interactiveLoopHelperInternal`) to call `handleCommandPure` and then perform rendering or output based on the returned `[String]` and `DebuggerAction`.

- __Validation__: Test the debugger to ensure commands produce the same output and state changes as before. Write a simple unit test for `handleCommandPure` with a mock `Machine` state to verify logic without I/O.

- __Why Incremental__: This step focuses on refactoring command handling while maintaining the existing interactive loop structure, ensuring the separation of concerns is correct before broader changes.

#### Step 6: Model Debugger as a State Machine

- __Objective__: Simplify the interactive loop by modeling the debugger as a state machine with explicit states and transitions.

- __Actions__:

  1. Define a `DebuggerState` ADT in `Debugger/Types.hs` to represent states like `WaitingForInput`, `DisplayingHelp`, etc.
  2. Write a pure transition function `nextState :: DebuggerState -> Machine -> Input -> (DebuggerState, Machine, [Action])` where `Input` is user input or events, and `Action` includes rendering or output tasks.
  3. Update `interactiveLoopHelperInternal` in `Debugger.hs` to use this state machine, executing I/O based on returned actions.

- __Validation__: Test the debugger's control flow, especially mode switching (e.g., to Vim mode) and help display, to ensure transitions are smooth and state is maintained correctly.

- __Why Incremental__: This step focuses on the control flow of the debugger loop, building on previous refactors to command handling, ensuring the state machine integrates with existing logic.

#### Step 7: Optimize Data Structures for Output and History

- __Objective__: Improve performance and maintainability by using efficient functional data structures.

- __Actions__:

  1. Replace the `outputLines` list in `DebuggerConsoleState` with `Data.Sequence.Seq` for efficient appending and truncation, updating related functions in `Debugger/Console.hs` and `Display.hs`.
  2. If applicable (based on full history management code), refactor `pcHistory` and `redoHistory` to use a zipper or similar structure for efficient navigation, updating related logic in `Machine.hs` or Vim mode modules.

- __Validation__: Test the debugger's output display to ensure it handles large amounts of output without performance degradation. Verify history navigation (if implemented) works as expected.

- __Why Incremental__: This step targets specific data structures, allowing performance improvements to be validated independently of logic changes.

#### Step 8: Documentation and Final Testing

- __Objective__: Ensure the refactored code is well-documented and thoroughly tested.

- __Actions__:

  1. Add Haddock documentation to new types and functions in `Debugger/Types.hs` and other modified modules.
  2. Write unit tests for pure functions (e.g., `handleCommandPure`, `nextState`) using HUnit or QuickCheck, placing them in the `test/` directory.
  3. Perform a full integration test of the emulator and debugger with a sample program to confirm all features work together.

- __Validation__: Review documentation for clarity and completeness. Run all tests to ensure no regressions in functionality.

- __Why Incremental__: This final step focuses on quality assurance, ensuring the refactored code is maintainable and reliable before considering the update complete.

### Control and Rollback Strategy

- __Version Control__: Before starting each step, commit the current code state to a version control system (e.g., Git) with a descriptive message (e.g., "Before Step 1: Introducing Lenses"). This allows rollback if a step introduces issues.
- __Testing After Each Step__: Run a predefined set of test cases or interactively use the debugger after each step to catch issues early. If a step fails validation, revert to the previous commit and adjust the approach.
- __Incremental Scope__: Each step is designed to be small enough that changes can be easily understood and debugged if necessary, maintaining control over the codebase.


