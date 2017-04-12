			RELEASE NOTES FOR CEF32 V2.1

Changes in V2.1 from V2.0
    General:
        o CEF32 and all components reflect V2.6 of the CEF specification.
        o New utility: CEF32 Raster Font Editor.
        o Memory domains now supported.
	o Additional support routines added to CEF_Util.
        o New program: Macro11 command-line assembler for PDP11s.
    User Interface:
	o Reopen menus handled better.
        o Load Component dialog enhanced.
        o CPU popup menu supports copy/paste.
        o Fixed exceptions when closing editor tabs.
        o Disassembly tab can no longer be closed with menu shortcut.
        o Memory tab scrollbar now operates correctly for 64-bit address ranges.
    Master assembler
        o Fixed handling of DIFFERENT, IDENTICAL, DIF, and IDN conditionals.
        o New directive: .SBTTL
        o New directive: .PSECT
        o .RAD50 is a new synonym for .RADIX50
    Components, fonts, and emulators:
        o New DEC LA30 font (la30.c).
        o New DEC LA36 font (la36.c).
	o New DEC LA36 compressed font (la36c.c).
	o New DEC LK01 (DECWriter) keyboard (LK01_KB.dll).
	o New DEC LK02 (DECWriter II) keyboard (LK02_KB.dll).
	o New Printer component (LA36.dll).
        o New terminal component for SVM (SVMScreen.dll).
        o New fast Static RAM memory component (SRAM.dll).
        o New 32-bit address static RAM (Blockram.dll).
        o New 64-bit file-mapped ROM (svm_Code_RAM.dll).
	o New GigaByte microcode engine component (GB_ucode.dll).
	o New SVM32 component (CEFSVM.dll).
	o Logging now works in video terminals (VT52.dll).
	o Fixed input queue problems that could cause hangs or lose characters
	  (VT52.dll).
	o Fixed menu oddness in video terminals (VT52.dll).
	o Properly handle reversing video in video terminals (VT52.dll).
        o Increased PDP-11 CPU performance (pdp11.dll)
	o Serial I/O devices now handle mismatched data transfer rates.
        o Generic memory component now supports full 64-bit address range
          (cefmemory.dll).


			RELEASE NOTES FOR CEF32 V2.0

Changes in V2.0 from V1.9:
    General:
        o CEF32 and all components reflect V2.5 of the CEF specification.
        o Fix abend caused by setting memory watchpoints.
        o Fix painting issues on data displays.
    User Interface:
        o Added Media Manager.
        o Localization files supported.
    Components and emulators:
        o Fix various errors that could happen in PDP11 disassembly (pdp11.dll).
        o New Rockwell AIM 65 keyboard (AIM65_KB.dll).


			RELEASE NOTES FOR CEF32 V1.9

Changes in V1.9 from V1.8:
    General:
        o CEF32 and all components reflect V2.4 of the CEF specification.
        o TUI_Interface no longer sends signals to originating component.
        o TUI_Interface now calls Set_Signal along with Signal_Change_Notice.
    User Interface:
        o CPU status pane doesn't say "halted" when executing.
        o Fixed problem with disassembly scrollbar sometimes not scrolling up.
    Components and emulators:
        o Fix disassembly of EIS instructions in PDP-11 (pdp11.dll).
        o PDP-11 disassembly of relative addressing now shows absolute address
          (pdp11.dll).
        o PDP-11 assembler handles operand expressions better (pdp11.dll).
        o PDP-11 no longer halts on RESET instruction (pdp11.dll).
        o PDP-11 handles signals better (pdp11.dll).
        o PDP-11 MOVB to register now sign-extends (pdp11.dll).
        o PDP-11 Autodecrement mode works properly (pdp11.dll).
        o PDP-11 traps push PC/PS onto stack in proper order (pdp11.dll).
        o Fix PDP-11 flag handling on execution of various instructions
          (pdp11.dll).
        o PDP-11 MUL and ASHC instructions no longer invert destination
          registers (pdp11.dll).
        o DL11W.dll component renamed to KW11L.dll.
        o Added RESET button to UNIBUS front panel (unibus.dll).
        o UNIBUS Switch Register support is now optional (unibus.dll).
        o Fix problems with addressing mode changes in UNIBUS (unibus.dll).
        o All UNIBUS components prefix signal names with "UNIBUS_".
        o VT52 has menu option to clear screen: Edit | Clear Screen (vt52.dll).
        o Fix VT52 problem ignoring incoming serial data (vt52.dll).
        o MITS 88-SIO now operates within spec (SIO.dll).
        o Fix problems with Altair system not propogating reads/writes
          (Altair.dll).


			RELEASE NOTES FOR CEF32 V1.8

Changes in V1.8 from V1.7:
    General:
        o CEF32 and all components reflect V2.3 of the CEF specification.
        o Master clock provided and used.
        o All preferences are persisted and restored.
        o Fix abend when adding watches before any CPU is loaded.
        o Corrected inconsistencies in clock time.  Now all times are in
          nanosecond increments.
    User Interface:
        o Combined assembly status and error dialogs into a single dialog.
        o Fixed problems with numbers sometimes displaying off by one.
        o Watch values now show in the specified base.
        o Address spaces now show in memory view popup.
        o UI properly handles sparse overlapping memory devices.
        o Fix abend in File | Save As when used in Disassembly.
        o File | Save disabled when in Disassembly.
        o Disassembly works properly for instructions that use relative
          addresses.
    Master Assembler:
        o Fixed problem with arithmetic errors in expressions with integer
          values larger than 65536.
        o Fixed problem with comments following DW directives.
        o ~ and NOT operators supported in expressions.
        o =: is equivalent to =
        o ==: is equivalent to ==
    Components and emulators:
        o The VT05 is declining and will not be distributed after this release
          (vt05.dll).  It is replaced by the VT52 (vt52.dll).
        o 64K RAM can now occupy any contiguous range of memory addresses
          (s64k_ram.dll).
        o New DEC KE11-A Extended Arithmetic Element (EAE) component (EAE.dll).
        o New Line clock for DEC DL11-W (DL11W.dll).
        o New UNIBUS component with PDP-11/70 front-panel (unibus.dll).
        o New Intel Intellec8 microcomputer emulator (intellec8.cef).
        o New Intel Intellec8 front-panel (intellec8.dll).
        o New 4 Mb static RAM component (s4m_ram.dll)
        o Intel 8008 assembler handles DAD and DEF directives (i8008.dll).
        o Intel 8008 CPU handles Segment_Size properly (i8008.dll).
        o PDP-11 emulator emulates a PDP-11/34 (pdp11.cef).
        o PDP-11 emulator uses UNIBUS component (pdp11.cef).
        o Fixed PDP-11 bug using immediate absolute addresses in immediate mode
          (pdp11.dll).
        o Fixed PDP-11 disassembly bugs for SUB, BMI, BHI, and BLOS (pdp11.dll).
        o PDP-11 assembler recognizes ', !, and ^ notations (pdp11.dll).
        o PDP-11 assembler supports expressions following % (pdp11.dll).
        o Fixed various other PDP-11 assembly bugs (pdp11.dll).
        o PDP-11 operates as a PDP-11/34 by default (pdp11.dll).
        o DL11 can now operate in DEC DL11-W mode (DL11.dll).
        o DL11 properly responds to requests for vector (DL11.dll).
        o VT52 keyboard generates correct names for blank keys (vt52_kb.dll).
        o Fixed dead keys on VT52 keyboard (vt52_kb.dll).


			RELEASE NOTES FOR CEF32 V1.7

Changes in V1.7 from V1.6:
    General:
        o CEF32 and all components reflect V2.2 of the CEF specification.
        o Deposits and examines are done using bit counts instead of byte
          counts.
        o Loaded components have both inputs and outputs connected to default
          memory automatically.
        o Program hardened against faulty components.
    User Interface:
        o Dialog for changing registers whose length is not a multiple of 8 now
          more accurately restricts the range of valid values.
        o Changing registers whose length is less than 8 bits now works.
        O CPU status pane doesn't reset its position when it is updated.
        o When changing data in the CPU status pane, only the numeric value
          is shown in the dialog.
        o When a CPU component is loaded, the memory display now defaults to the
          default base of the CPU and the disassembly tab updates properly.
        o Disassembly pane shows data properly in all bases.
        o Disassembly pane scrollbar positions correctly.
        o Fixed various other problems with the Disassembly pane.
        o Pressing the UP arrow key when the immediate mode edit box is focused
          will load the edit box with the last immediate mode command.
        o Delete All execution breakpoints from dialog now works.
        o Memory pane popup menu supports "Pattern" to allow writing patterns
          of data to memory.
        o Modify Memory dialog has been reworked.
        o New "Show Errors" item on Assemble menu shows last error list.
        o New "Emulator ports" item on Components menu shows all published
          emulator ports.
        o File | Close and Assemble | Assemble are only enabled when a tab with
          source is focused.
    Master Assembler:
        o ASCII, ASCIC, and ASCIZ directives properly handle text quoted with
          characters other than " and '.
        o Fixed potential parsing problems in directive processing.
        o "$=x" construct is equivalent to "ORG X".
        o RADIX directive supports mnuemonics for base number.
        o BASE directive is now a synonym for RADIX.
        o New directive: .DC
        o New directive: .SYMBOL
        o New directive: .XREF
    Components and emulators:
        o New Fairchild 3850 CPU component with 3853 SMI (F3850.dll).
        o New emulator for Fairchild 3850/3853 CPU with 64 Kb RAM (f8.cef).
        o New PDP-11 CPU component (PDP11.dll).
        o New DEC DL11 serial interface component (DL11.dll).
        O New emulator for PDP-11/20 CPU with 56 Kb RAM, DL11, and VT05 system
          console (pdp11.cef).
        o New Intel 8008 CPU component (I8008.dll).
        o New Intel Imm8-60 serial I/O board component (imm860.dll).
        o Screen components can now use threading (Genscreen.dll, VT05.dll, and
          VDM1.dll).
        o VT05 screen copy to clipboard now includes new line at end of each row
          (VT05.dll).
        o VT05 setup allows setting caption (VT05.dll).


			RELEASE NOTES FOR CEF32 V1.6

Changes in V1.6 from V1.5:
    General:
        o CEF32 and all components reflect V2.1 of the CEF specification.
        o CEF32.exe now shows CPU output signal states below registers.
        o Fixed abends related to editing in tabs.
        o Fixed abend related to resizing application.
        o Fixed abend when attempting to assemble the disassembly.
        o Fixed horizontal scrolling in editing tabs.
        o Various other minor UI fixes.
    Master Assembler:
        o Expression evaluation recognizes symbol names that begin with "$" or
          ".".
        o Back-patching of multibyte data is done correctly for big-endian CPUs.
        o Fixed processing of .ASCII, .ASCIC, .ASCIZ, and .DEFM directives.
        o Fixed abend related to macro processing.
    Components:
        o New RCA CDP1802 CPU component (RCA1802.dll).
        o New Cosmac Elf system component (Cosmac_Elf.dll).
        o VT05 now can connect to emulator ports via port lists (VT05.dll).
        o Fixed abend on startup of Altair system component (Altair.dll).
        o Added "Run" button to Altair front-panel (Altair.dll).
        o Z80 register examine no longer converts by-reference Size parameter
          from bits to bytes (Z80.dll).


			RELEASE NOTES FOR CEF32 V1.5

Changes in V1.5 from V1.4:
    General:
        o CEF32 and all components now reflect V2.0 of the CEF specification.
        o Fix a couple of syntax errors that kept certain components from
          compiling.
        o Fix abend when expanding size of disassembly window when using
          CEF_Memory.
        o Fix abends related to terminating components.  
    Components:
        o New DEC VT52 keyboard component (VT52_KB).
        o New DEC LK201 keyboard component (LK201_KB).
        o New DEC LA120 (DecWriter III) keyboard component (LA120_KB).
        o New Teletype Model 33 keyboard component (ASR33_KB).
        o New TRS-80 Model I keyboard component (TRS80_1_KB).
        o New Apple II keyboard component (Apple_2_KB).
        o Fixed problem with Z80 triggering register read watchpoints when it
          should not have.


			RELEASE NOTES FOR CEF32 V1.4

Changes in V1.4 from V1.3:
    General
        o All code supports Version 1.7 of the CEF specification.
    User Interface:
        o Fixed CEF32 abend when a component calls UI.Terminate.
    Components:
        o New Altair 8800 system component (Altair).
        o New emulator for MITS Altair 8800 system (Altair.cef).
        o New DEC VT100 keyboard component (VT100_KB).
        o New 108-key PC keyboard component (PC108_KB).
        o Fixed various abends when terminating visual components.
        o Handle multiple instances of visual components better.
        o VT05 Can now send files and log received data to files.
        o VT05 now emulates a glass teletype, a DEC VT05 terminal, and a DEC
          VT50 terminal.
        o VT05 can now change which keyboard component is used, and whether a
          keyboard is shown at all.
        o Added EEOS and EEOL keys to VT05 keyboard (VT05_KB).
        o SOL20 serial port can now receive data.
        o SOL20 has option to hide keyboard.
        o Prevent ringing on signals in Serial cable (Serial).
        o VDM1 now has control port (default C8 hex).


Changes in V1.3 from V1.2:
    Components:
        o New video terminal component (VT05).
        o New DEC VT05 keyboard component (VT05_KB).
        o New CEF_Util.dll implementing CEF Utilities Specification.
        o New Processor Technology SOL-20 system component (SOL20).
        o New Processor Technology SOL-20 emulator (SOL20.cef).
        o VDM1 component now uses CEF_Util.
        o Added top/bottom margin to VDM1 component.


Changes in V1.2 from V1.1:
    User Interface:
        o Memory dump now works
        o Memory load/dump support *.ent files
        o Register modification dialog now accepts lowercase
        o Fixed problem with "Goto Current Address" failing
        o Fixed editor focus problems
        o Watches can now be set on symbolic addresses
    Assembler:
        o ORG directive now works
        o $ is now a synonym for .
        o Better handle constants
        o Allow address symbol definition without requiring ":"
        o SHR, SHL, AND, OR, and XOR are now valid numeric operators
        o Notifies user if attempting to assemble into ROM
    Components:
        o Z80.DLL now supports Set Up commands: 8080, 8085, and Z80
        o Fixed several bugs in Z80.DLL emulation
        o Z80.DLL assembler now allows expressions for immediate data
        o Fixed various problems with forward references in Z80.DLL assembler
        o S64K_RAM.DLL now properly loads data in response to Set Up LOAD
        o New S-100 VDM-1 emulator component (VDM1.dll)
        o New Serial cable component (Serial.dll).
        o Fixed bug in GenScreen that misplaced characters on the screen
        o Improved performance of GenScreen.

