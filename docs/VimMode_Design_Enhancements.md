# VimMode Design Enhancements

## 1. Composable Commands

### Grammar
`[count][operator][modifier][object]`

### Reference Table
| Element     | Options          | Description                     |
|-------------|------------------|---------------------------------|
| Operator    | d, c, y         | Delete/Change/Yank              |
| Modifier    | i, a             | Inner/Outer object              |
| Object      | w, l, b, "      | Word/Line/Bracket/Quote         |

## 2. Colon Commands

### VICE Command Equivalences
| Command         | Example             | Action                                  |
|-----------------|---------------------|-----------------------------------------|
| :break          | :break $1000       | Set breakpoint at 0x1000               |
| :watch          | :watch 1000-1FFF   | Trace memory range                      |
| :step           | :step 5            | Execute 5 instructions                 |
| :regs           | :regs              | Show register values                    |
| :disas          | :disas $1000 $1010 | Disassemble memory range                |

## 3. State Transition Diagram
```mermaid
stateDiagram-v2
    [*] --> NormalMode
    NormalMode --> OperatorPending: d/c/y
    OperatorPending --> ObjectPending: i/a
    ObjectPending --> Execute: Object key
    OperatorPending --> Execute: Motion key
    NormalMode --> CommandMode: :
    CommandMode --> Execute: Enter command
    Execute --> NormalMode
```

## 4. Implemented Features

### Command Composition
- Operators: d (delete), c (change), y (yank)
- Modifiers: i (inner), a (outer)
- Objects: w (word), l (line), b (bracket)
- Examples: `diw` (delete inner word), `caw` (change a word)

### Colon Commands
- `:break $addr` - Set breakpoint at specified address
- `:watch $start-$end` - Trace memory range
- `: ` - Starts command input
- Escape cancels command mode
- Enter executes command

### State Handling
- `NoCommand` state handles normal operations
- `Operator` state waits for modifier or motion
- `Object` state selects text object type
- `VimPendingCompose` state for register selection
- `CommandModeV` handles command input

## 5. Example Usage
```text
Normal mode:
  c i w  # Change inner word
  :break $1000 # Set breakpoint
  :watch 1000-1FFF # Trace memory range

Command mode:
  :           # Enters command mode
  break $1000 # Types command
  Enter       # Executes :break command
```

## 6. Command Reference Tables

### Text Object Commands
| Command | Description                  |
|---------|------------------------------|
| diw     | Delete inner word            |
| daw     | Delete a word (with space)   |
| ci"     | Change inside quotes         |
| yi[     | Yank inside brackets         |

### Colon Commands
| Command         | Parameters      | Description                    |
|-----------------|-----------------|--------------------------------|
| :break          | $addr           | Set breakpoint at address      |
| :watch          | $start-$end     | Trace memory range             |
| :step           | [count]         | Execute instructions           |
| :regs           |                 | Show register values           |
| :disas          | [$start [$end]] | Disassemble memory range       |

### State Transitions
| State              | Key     | Next State        |
|--------------------|---------|-------------------|
| NoCommand          | d/c/y   | Operator          |
| Operator           | i/a     | Object            |
| Operator           | motion  | Execute action    |
| Object             | object  | Execute action    |
| NoCommand          | :       | CommandModeV      |
| CommandModeV       | Escape  | NoCommand         |
| CommandModeV       | Enter   | Execute command   |
| NoCommand          | "       | VimPendingCompose |
| VimPendingCompose  | key     | NoCommand         |

```haskell
-- CommandState with VimPendingCompose
data CommandState =
    NoCommand
  | Operator OperatorType
  | Object OperatorType ObjectModifier
  | VimPendingCompose  -- For register selection
  | CommandModeV
  deriving (Eq, Show)
```

### File: `src/MOS6502Emulator/Debugger/VimMode/HandleKey.hs`
- Implemented command composition state machine
- Added colon command handling
- Added VimPendingCompose state for register selection

### File: `src/MOS6502Emulator/Debugger/VimMode/Execute.hs`
- Implemented colon command execution
- Added support for :break and :watch commands