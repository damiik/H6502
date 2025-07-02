# Debugger Flowchart
This flowchart illustrates the main components and flow of the MOS6502 Emulator's debugger, including

```mermaid
graph TD
    subgraph Emulator Initialization src/MOS6502Emulator.hs
        EI_Start[Start runDebugger] --> EI_Init[Initialize Machine newMachine]
        EI_Init --> EI_Setup[Setup Machine setupMachine]
        EI_Setup --> EI_Load[Load Symbols & Debugger State]
        EI_Load --> EI_SetPC[Set Starting PC & Debugger Active]
        EI_SetPC --> EI_RunLoop[Enter runLoop]
    end

    subgraph Main Execution Loop src/MOS6502Emulator.hs
        MEL_CheckHalt{Halted?} -->|Yes| MEL_Halt[Stop]
        MEL_CheckHalt -->|No| MEL_CheckDebug{Debugger Active?}
        MEL_CheckDebug -->|Yes| MEL_Interactive[Interactive Loop by Mode]
        MEL_CheckDebug -->|No| MEL_CheckBP{Breakpoint Hit?}
        MEL_CheckBP -->|Yes| MEL_ActivateDebug[Activate Debugger]
        MEL_CheckBP -->|No| MEL_Execute[executeStepAndRender]
        MEL_Execute --> MEL_CheckHalt
        MEL_ActivateDebug --> MEL_Interactive
    end

    subgraph Interactive Debugger Loop src/MOS6502Emulator/Debugger.hs
        IDL_Mode{Debugger Mode?} -->|CommandMode| IDL_Command(Get User Input CommandMode)
        IDL_Mode -->|VimMode| IDL_Vim(Handle Vim Key Input)
        IDL_Mode -->|VimCommandMode| IDL_VimCmd(Handle Vim Command Input)
        IDL_Command --> IDL_Parse[Parse Command]
        IDL_Parse --> IDL_Handle[handleCommandPure]
        IDL_Handle --> IDL_Action{DebuggerAction?}
        IDL_Vim --> IDL_VimAction{DebuggerAction?}
        IDL_VimCmd --> IDL_VimCmdAction{DebuggerAction?}
        IDL_Action -->|ExecuteStep| IDL_Step[executeStepAndRender]
        IDL_Action -->|ExitDebugger| IDL_ExitDebug[Deactivate Debugger]
        IDL_Action -->|QuitEmulator| IDL_Quit[Set Halted True]
        IDL_Action -->|SetMode| IDL_SetMode[Update Debugger Mode]
        IDL_Action -->|Render| IDL_Render[renderScreen]
        IDL_Action -->|UpdateConsole| IDL_UpdateConsole[Update Console Output]
        IDL_VimAction -->|ExecuteStep| IDL_Step
        IDL_VimAction -->|ExitDebugger| IDL_ExitDebug
        IDL_VimAction -->|QuitEmulator| IDL_Quit
        IDL_VimAction -->|SetMode| IDL_SetMode
        IDL_VimAction -->|Render| IDL_Render
        IDL_VimCmdAction -->|ExecuteStep| IDL_Step
        IDL_VimCmdAction -->|ExitDebugger| IDL_ExitDebug
        IDL_VimCmdAction -->|QuitEmulator| IDL_Quit
        IDL_VimCmdAction -->|SetMode| IDL_SetMode
        IDL_VimCmdAction -->|Render| IDL_Render
    end

    subgraph Command Handling src/MOS6502Emulator/Debugger/Commands.hs
        CH_Handle[handleCommandPure] --> CH_Command{Command Type}
        CH_Command -->|Step| CH_Step[Execute Step Action]
        CH_Command -->|Break| CH_Break[Add/Remove Breakpoint]
        CH_Command -->|MemTrace| CH_MemTrace[Add/Remove Memory Trace]
        CH_Command -->|Fill| CH_Fill[Fill Memory Range]
        CH_Command -->|SetReg| CH_SetReg[Set Register Value]
        CH_Command -->|Disassemble| CH_Disassemble[Disassemble Instructions]
        CH_Command -->|Quit| CH_Quit[Quit Emulator Action]
        CH_Command -->|Exit| CH_Exit[Exit Debugger Action]
        CH_Command -->|ModeChange| CH_Mode[Set Debugger Mode Action]
        CH_Command -->|Help| CH_Help[Display Help Action]
    end

    subgraph Step Execution & Rendering src/MOS6502Emulator/Debugger/Actions.hs
        SER_Execute[executeStepAndRender] --> SER_Cycle[fdxSingleCycle]
        SER_Cycle --> SER_Update[Update Machine State]
        SER_Update --> SER_LogReg[Log Registers]
        SER_Update --> SER_LogMem[Log Memory Traces]
        SER_Update --> SER_Disassemble[Disassemble Current Instruction]
        SER_LogReg --> SER_RenderPrep[Prepare Output Lines]
        SER_LogMem --> SER_RenderPrep
        SER_Disassemble --> SER_RenderPrep
        SER_RenderPrep --> SER_Render[renderScreen]
        SER_Render --> SER_Check[handlePostInstructionChecks]
        SER_Check --> SER_HaltCheck{Halted?}
        SER_HaltCheck -->|Yes| SER_Activate[Activate Debugger]
    end

    subgraph Display src/MOS6502Emulator/Display.hs
        D_Render[renderScreen] --> D_Output[Console Output]
    end

    subgraph State
        S_Machine[Machine State]
        S_Console[DebuggerConsoleState]
    end

    EI_RunLoop --> MEL_CheckHalt
    MEL_Interactive --> IDL_Mode
    IDL_Step --> SER_Execute
    IDL_Render --> D_Render
    IDL_UpdateConsole -- Updates --> S_Console
    IDL_Handle -- Reads/Updates --> S_Machine
    IDL_Handle -- Reads/Updates --> S_Console
    CH_Handle -- Reads/Updates --> S_Machine
    CH_Handle -- Reads/Updates --> S_Console
    SER_Execute -- Reads/Updates --> S_Machine
    SER_Render -- Renders to --> D_Render

    style EI_Start fill:#f9f, stroke:#333, stroke-width:2px
    style MEL_Interactive fill:#bbf, stroke:#333, stroke-width:2px
    style IDL_Mode fill:#77b, stroke:#333, stroke-width:2px
    style CH_Command fill:#77b, stroke:#333, stroke-width:2px
    style SER_Execute fill:#f9f, stroke:#333, stroke-width:2px
    style D_Render fill:#bbf, stroke:#333, stroke-width:2px
```


# Debugger Data Flow (Vim Mode)
This diagram details the data flow and control logic for the debugger's Vim mode, focusing on how user input is processed, how state is managed, and how the screen is rendered.

```mermaid
graph TD
    subgraph User Input & Main Loop ["src/MOS6502Emulator/Debugger/VimMode/Enhanced.hs"]
        Start[interactiveLoopHelper] --> SetupTerminal["hSetBuffering, hSetEcho"]
        SetupTerminal --> Loop(interactiveLoopHelperInternal)
        Loop -- Checks --> IsHalted{Machine Halted?}
        IsHalted -->|Yes| QuitAction[Return QuitEmulatorAction]
        IsHalted -->|No| GetKey[liftIO getKey]
        GetKey -- key --> HandleKey
        HandleKey -- "(action, output, newState)" --> UpdateState[Update Machine State (vimState, mConsoleState, debuggerMode)]
        UpdateState --> Render[Render Screen]
        Render --> HandleAction{DebuggerAction?}
        HandleAction -->|ExecuteStepAction| StepAction[Return ExecuteStepAction to outer loop]
        HandleAction -->|ExitDebuggerAction| ExitAction[Return ExitDebuggerAction]
        HandleAction -->|QuitEmulatorAction| QuitAction
        HandleAction -->|NoDebuggerAction, ...| Loop
        Loop --> RestoreTerminal["finally: Restore Terminal"]
    end

    subgraph Key Handling Dispatch ["src/MOS6502Emulator/Debugger/VimMode/Enhanced.hs"]
        HandleKey[handleVimKey] -- based on vimState --> KeyDispatch{Mode?}
        KeyDispatch -->|VimCommandMode| HVC_Key[handleVimCommandModeKey]
        KeyDispatch -->|VimMode| HVN_Key[handleVimNormalModeKey]
    end

    subgraph Vim Normal Mode ["src/MOS6502Emulator/Debugger/VimMode/HandleKey.hs"]
        HVN_Key[handleVimNormalModeKey] --> IsVisual{In Visual Mode?}
        IsVisual -->|Yes| HVV_Key[handleVisualKey]
        IsVisual -->|No| IsOperatorPending{Operator Pending?}
        IsOperatorPending -->|Yes| HandleOpMotion[Handle Motion for Operator (d,c,y)]
        HandleOpMotion --> EA_Execute[executeAction]
        IsOperatorPending -->|No| HandleNormalCmd[Handle Normal Command]
        HandleNormalCmd -- "e.g., j,k,w,b" --> EM_Execute[executeMotion]
        HandleNormalCmd -- "e.g., r,+,B" --> EA_Execute[executeAction]
        HandleNormalCmd -- "e.g., d,c,y" --> SetOperator[Set vsOperator]
        HandleNormalCmd -- ":" --> SetModeVimCmd[Set Mode to VimCommandMode]
        SetOperator --> HVN_Key
    end

    subgraph Vim Command Mode ["src/MOS6502Emulator/Debugger/VimMode/Enhanced.hs"]
        HVC_Key[handleVimCommandModeKey] --> KeyType{Key?}
        KeyType -->|'\n' (Enter)| ExecVimCmd[Execute Command]
        ExecVimCmd --> PDC[parseDebuggerCommand]
        PDC --> HCP[handleCommandPure]
        HCP -- "(newMachine, output, action)" --> HVC_Key
        KeyType -->|'\DEL' (Backspace)| UpdateBuffer[Update Command Buffer]
        KeyType -->|'char'| UpdateBuffer
        UpdateBuffer --> HVC_Key
    end

    subgraph Action & Motion Execution ["src/MOS6502Emulator/Debugger/VimMode/Execute.hs"]
        EM_Execute[executeMotion] -- Reads --> S_Machine[Machine State]
        EM_Execute -- "Returns new cursor pos" --> HVN_Key
        EA_Execute[executeAction] -- Reads/Writes --> S_Machine
        EA_Execute -- Can call --> EVC_Execute[executeVimCommand]
        EVC_Execute --> HCP
        EA_Execute -- "Returns (action, pos, output)" --> HVN_Key
    end
    
    subgraph Command Handling ["src/MOS6502Emulator/Debugger/Commands.hs"]
        HCP[handleCommandPure] -- Reads/Updates --> S_Machine
        HCP -- Returns --> Result["(Machine, [String], DebuggerAction)"]
    end

    subgraph Rendering ["src/MOS6502Emulator/Debugger/VimMode/Enhanced.hs"]
        Render --> RVS[renderVimScreen]
        RVS --> ViewMode{vsViewMode?}
        ViewMode -->|CodeView| GetDisassembly[Get Disassembly]
        ViewMode -->|MemoryView| GetMemory[Fetch Memory Bytes]
        ViewMode -->|RegisterView| GetRegisters[logRegisters]
        ViewMode -->|StackView| GetStack[Fetch Stack Bytes]
        GetDisassembly --> CallRenderScreen
        GetMemory --> CallRenderScreen
        GetRegisters --> CallRenderScreen
        GetStack --> CallRenderScreen
        CallRenderScreen[renderScreen] --> RenderStatus[Render Status & Command Line]
        RenderStatus --> ConsoleOutput[Output to Console]
    end

    subgraph State
        S_Machine["Machine State\\n(Registers, Memory, Breakpoints, vimState, mConsoleState, etc.)"]
    end

    HandleKey -- Reads --> S_Machine
    HCP -- Reads/Writes --> S_Machine
    EM_Execute -- Reads --> S_Machine
    EA_Execute -- Reads/Writes --> S_Machine
    RVS -- Reads --> S_Machine

    style Start fill:#f9f, stroke:#333, stroke-width:2px
    style Loop fill:#bbf, stroke:#333, stroke-width:2px
    style HandleKey fill:#77b, stroke:#333, stroke-width:2px
    style RVS fill:#f9f, stroke:#333, stroke-width:2px
    style ConsoleOutput fill:#bbf, stroke:#333, stroke-width:2px
```