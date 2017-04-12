{$N+}
{
        Program Name : Z80CPU
        Package Name : Z80
        Purpose      : 8080/8085/Z80 CPU (CEF component) emulator
        Institution  :
        Date Written : 9-Aug-2001
        Written By   : Alan Conroy
        Version      : 1.1

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

          23-Mar-2006   EAC         Added Set_Up.
          10-Jan-2008   EAC         Support specification V2.2.    

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	  This unit implements a Zilog Z80B CPU emulator as a CEF component.
        This component can also be configured as an Intel 8080 or 8085.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Z80CPU ;

interface

uses { Borland... }
     Classes, { TList }

     { CEF... }
     _CEF, // TCEF_Logger
     CEF, { TBase_CPU }
     _CEFUtil, // TCEF_Watchpoint

     { Z80... }
     Z80ASM, { TMode }

     { Other... }
     _DebugIn, { TDebug_Interface }
     CommonUt, { TInteger_List }
     _Streams, // TCOM_Stream
     _UE ; // tUnified_Exception

type Register_Set = array[ 0..1, 0..3 ] of integer ;

const SF_Predefined = 1 ; { Predefined symbol }

const Z80Err_Facility = 42 ;
      Z80Err_Success = 0 ;
      Z80Err_Invalid_Component = 1 ;
      Z80Err_Already_Connected = 2 ;
      Z80Err_Component_Not_Found = 3 ;
      Z80Err_No_Cache = 4 ;
      Z80Err_Invalid_Register = 5 ;
      Z80_No_Breakpoint = 6 ;
      Z80_Breakpoint_Exists = 7 ;
      Z80_Invalid_Address = 8 ;
      Z80_Invalid_Operation = 9 ;
      Z80_Invalid_Instruction = 10 ;
      Z80_IO_Trap = 11 ;
      Z80_NMI_Only_In_Z80 = 12 ;
      Z80_Nested_NMI = 13 ;

type TZ80_Profiler = class( TBase_Profiler )
                         private // Instance data...
                             // Profile data...
                             _Clock : integer ;
                             _Instruction_Count : integer ;
                             Port_Outputs : array[ 0..255 ] of integer ;
                             Port_Inputs : array[ 0..255 ] of integer ;
                             Execution_Addresses : array[ 0..$FFFF ] of integer ;

                             // 0-FF = normal instructions
                             // 100-1FF = Z80 ED extension
                             // 200-2FF = Z80 CB extension
                             // 300-3FF = Z80 DD extension
                             // 400-4FF = Z80 FD extension
                             // 500-5FF = Z80 DDCB extension
                             // 600-6FF = Z80 FDCB extension
                             Instructions : array[ 0..$6FF ] of integer ;

                             // Report data...
                             Outputs : TStringList ;
                             Inputs : TStringList ;
                             Addresses : TStringList ;
                             Instruction_Lines : TStringList ;

                             // Temps used for returning PChars...
                             Temp_Domain_Name : string ;
                             Temp_Report_Line : string ;

                             // Other data...
                             Dirty : boolean ;
                             _Mode : TMode ;

                         public // API...
                             procedure Generate_Report ;
                             procedure Increment( Domain, Index : integer ) ;
                             procedure Increment_Clock( Count : integer ) ;
                             procedure Set_Mode( Mode : TMode ) ;

                         public // Overrides...
                             procedure Clear( Domain : integer ) ; override ;

                             function Domain_Name( Index : integer ) : PChar ;
                                 override ;

                             function Report_Line( Domain, Index : integer ) : PChar ;
                                 override ;
                     end ; // TZ80_Profiler

type TZ80_CPU = class ;

     TZ80 = class( TBase_Component )
                private // Instance data...
                    Inputs : TList ;
                    Outputs : TList ;
                    _Tag : longint ;
                    _Parent : TComponent ;
                    _Logger : TCEF_Logger ;

                    Temp_Signal_Name : string ;
                    Temp_Get_Exception_Description : string ;
                    Temp_Get_State_Name : string ;

                public // Public instance data...
                    _CPU : TZ80_CPU ;
                
                public // API...
                    _Serial_Number : integer ;

                    function CPU : TCPU ; override ;

                    function Facility_Code : longint ; override ;

                    function Initialize( UI : TUI_Interface ) : TUnified_Exception ;  override ;

                    function Terminate : TUnified_Exception ; override ;

                    function Serial_Number : integer ; override ;

                    function Child_Component( Index : longint ) : TComponent ; override ;

                    function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                        Access : longint ) : TUnified_Exception ; override ;

                    function Component_Type : longint ; override ;

                    function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                    function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                    function Debugger : TDebug_Interface ; override ;

                    function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                        Memory : boolean ) : TUnified_Exception ; override ;

                    function Disconnect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                    function Disconnect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                    function Examine( Address : int64 ; var Size : longint ;
                        Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

                    function Get_Access_Mode( Address : int64 ;
                        Memory : boolean ) : longint ; override ;

                    function Get_Profiling : boolean ; override ;

                    function Get_Read_Latency : longint ; override ;

                    function Get_Write_Latency : longint ; override ;

                    function Input_Component( Index : longint ) : TComponent ; override ;

                    function Name : PChar ; override ;

                    function Output_Component( Index : longint ) : TComponent ; override ;

                    function Read( Address : int64 ; Size : longint ;
                        IO_Type : longint ) : boolean ; override ;

                    function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                    function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                    function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                    function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                    function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                        Typ : longint ) : TUnified_Exception ; override ;

                    procedure Set_Profiling( _On, Children : boolean ) ;  override ;

                    procedure Set_Read_Latency( Value : longint ) ; override ;

                    function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                        Access : longint ) : TUnified_Exception ; override ;

                    procedure Set_Write_Latency( Value : longint ) ;  override ;

                    procedure Show_Status ; override ;

                    function Support_Feature( ID : integer ) : boolean ;
                        override ; stdcall ;

                    procedure Wake ; override ;

                    function Write( Address : int64 ; Value, Size : longint ;
                        IO_Type : longint ) : TUnified_Exception ; override ;

                    function Write_String( Address : int64 ; Value : PChar ;
                        Size : longint ; IO_Type : longint ) : TUnified_Exception ; override ;

                    procedure Reset ; override ;

                    procedure Set_Signal( Name : PChar ; State : boolean ) ; override ;

                    function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                        override ;

                    procedure Set_Up( P : PChar ) ; override ;

                    function Signal_Count : longint ; override ;

                    function Signal_Name( Index : longint ) : PChar ; override ;

                    function Signal_Index( Name : PChar ) : integer ; override ;

                    function Signal_Out( Index : longint ) : boolean ; override ;

                    function Signal_Active_Low( Index : longint ) : boolean ; override ;

                    function Get_State_Name( Index : longint ) : PChar ; override ;

                    function Get_Exception_Description( Index : longint ) : PChar ;
                       override ;

                    procedure Set_Tag( Value : longint ) ; override ;

                    function Get_Tag : longint ; override ;

                    function Get_Parent : TComponent ; override ;

                    procedure Set_Parent( Component : TComponent ) ; override ;

                    function Profiler : TProfiler ; override ;

                    function Get_Trace : boolean ; override ;

                    procedure Set_Trace( Value : boolean ) ; override ;

                    function Get_Logger : TCEF_Logger ; override ;

                    procedure Set_Logger( Value : TCEF_Logger ) ; override ;
            end ; // TZ80


     TZ80_CPU = class( TBase_CPU )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private
            Parent : TZ80 ;

            Overflow : Boolean ;
            { True if P/V flag is treated as overflow, false if parity }
            _I : integer ; { I register }
            IFF2 : boolean ; { Interrupt flag 2 }
            _IM : byte ; { IM register }
            In_NMI : boolean ; { True if NMI/TRAP with no RETN yet }
            Interrupts : boolean ; { True if interrupts enabled }
            _IX, _IY : integer ; { Index registers }
            _R : integer ; { R register }
            _Register : Register_Set ; { General registers }
            Register_Set0, Register_Set1 : Integer ;
            { Which register set is current ( Register_Set0 for AF, and Register_Set1
              for B-L) }
            _SP : integer ; { Stack pointer }
            _PC : word ; { Current PC }
            _UI : TUI_Interface ;
            _Speed : integer ; // KHz
            Temp_Register_Name : string ;
            Temp_Register_Description : string ;
            Temp_Log_Trace : string ;
            Temp_Signal_Exception : string ;
            _Halted : boolean ; // True if last instruction was a halt
            _Register_Watchpoints : array[ 0..30 ] of integer ; // Access mode for registers
            _Profiling : boolean ; // True if profiling
            _Memory_Watchpoints : TCEF_Watchpoint_Manager ;
            _Port_Watchpoints : array[ 0..255 ] of integer ;
            _Breakpoints : TInteger_List ;
            Memory_Data_Latch : byte ; // Last byte sent to us
            _Run_Stream : TCOM_Stream ;
            _Stream_PC_Offset : integer ;
            Serial_Input_State : boolean ;
            Serial_Output_State : boolean ;
            Interrupt_Instruction : boolean ; // True if instruction from bus due to interrupt
            RST5_Enabled : boolean ;
            RST6_Enabled : boolean ;
            RST7_Enabled : boolean ;
            RST7_Pending : boolean ;
            RST5_State : boolean ;
            RST6_State : boolean ;
            RST7_State : boolean ;
            Blocked : boolean ;
            Stopping : boolean ;
            _Profiler : TZ80_Profiler ;
            _Trace : boolean ; // True to trace execution
            Segments : TInteger_List ;
            _Logger : TCEF_Logger ;
            _RTS : TRun_Time_System ;
            _RTS_Flags : longint ;

        private // Internal utility routines...
            procedure Push( X : Integer ) ;
            function Pop : Integer ;
            procedure Push_PC ; { Push PC to stack }
            procedure Do_NMI ;
            procedure Do_Interrupt ;
            procedure Do_Wait ;
            function ByteRead( Address : Integer ) : Char ; { Return data at Address }
            function Byte_Read( Address : integer ) : integer ;
            function Word_Read( Address : integer ) : integer ;
            procedure ByteWrite( Address, Value : Integer ) ; { Write to memory }
            procedure MemWrite( Address : Integer ; Value : String ) ; { Write to memory }
            procedure Increment_Clock( Count : integer ) ;
            function Bus_Read( Address : Integer ; IO_Type : longint ) : Char ; { Return data at Address }
            function Bus_Examine( Address : Integer ) : Char ; { Return data at memory Address }
            procedure Execute( Single_Step, Into : boolean ) ;
            procedure Output( Port, Value : integer ) ;
            function Input( Port : integer ) : integer ;
            procedure Send_Signal( const Name : string ; Value : boolean ) ;
            procedure Clear_Watchpoints ;
            procedure State_Change_Notice( Index : integer ; State : boolean ) ;
            procedure Log_Trace( const Description : string ) ;
            procedure Signal_Exception( const Description : string ;
                Index : longint ) ;
            procedure Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
                Memory, Internal, Port : boolean ) ;
            function Instruction_At( Address : integer ) : string ;
            function Lo_Register( I1, I2 : integer ) : byte ;
            function Hi_Register( I1, I2 : integer ) : byte ;

            function Get_A : integer ;
            procedure Set_A( Value : integer ) ;
            function Get_C : integer ;
            procedure Set_C( Value : integer ) ;
            function Get_BC : integer ;
            procedure Set_BC( Value : integer ) ;
            function Get_HL : integer ;
            procedure Set_HL( Value : integer ) ;
            function Get_I : integer ;
            procedure Set_I( Value : integer ) ;
            function Get_IM : byte ;
            procedure Set_IM( Value : byte ) ;
            function Get_IX : integer ;
            procedure Set_IX( Value : integer ) ;
            function Get_IY : integer ;
            procedure Set_IY( Value : integer ) ;
            function Get_PC : word ;
            procedure Set_PC( Value : word ) ;
            function Get_R : integer ;
            procedure Set_R( Value : integer ) ;
            function Get_SP : integer ;
            procedure Set_SP( Value : integer ) ;

            function Get_Register( _Set, Index : integer ) : integer ;
            procedure Set_Register( _set, Index : integer ; Value : integer ) ;

            property Register_A : integer
                read Get_A
                write Set_A ;
            property Register_C : integer
                read Get_C
                write Set_C ;
            property Register_BC : integer
                read Get_BC
                write Set_BC ;
            property Register_HL : integer
                read Get_HL
                write Set_HL ;
            property IM : byte
                read Get_IM
                write Set_IM ;
            property IX : integer
                read Get_IX
                write Set_IX ;
            property IY : integer
                read Get_IY
                write Set_IY ;
            property I : integer
                read Get_I
                write Set_I ;
            property PC : word
                read Get_PC
                write Set_PC ;
            property R : integer
                read Get_R
                write Set_R ;
            property SP : integer
                read Get_SP
                write Set_SP ;
            property Register[ _Set, Index : integer ] : integer
                read Get_Register
                write Set_Register ;

        public // API...
            Base : integer ;
            Mode : TMode ;

            function Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;
                override ;

            function Cancel_Breakpoint( Address : int64 ;
                Space : integer ; Physical : boolean ) : TUnified_Exception ; override ;

            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ;

            function Disassemble( Address : int64 ; Base, Size : longint ;
                Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Get_Clock_Speed : longint ; override ;

            procedure Halt ; override ;

            function Halted : boolean ; override ;

            procedure Run ; override ;

            procedure Run_From_Stream( Stream : TCOM_Stream ) ; override ;

            function Set_Breakpoint( Address : int64 ; Space : integer ;
                Physical : boolean ) : TUnified_Exception ; override ;

            procedure Set_Clock_Speed( Value : longint ) ; override ;

            procedure Step( Into : boolean ) ; override ;

            function Translate( Space : integer ; Address : int64 ) : int64 ;
                override ;

            function Default_Base : integer ; override ;

            function Get_Low_Memory : int64 ; override ;

            function Get_High_Memory : int64 ; override ;

            function Get_Low_Virtual_Memory( Space : integer ) : int64 ; override ;

            function Get_High_Virtual_Memory( Space : integer ) : int64 ; override ;

            function Get_Low_Port : int64 ; override ;

            function Get_High_Port : int64 ; override ;

            function Support_Virtual_Address : boolean ; override ;

            function Segment_Size( Index : integer ) : integer ; override ;

            function Register_Name( Index : integer ) : PChar ; override ;

            function Register_Size( Index : integer ) : integer ; override ;

            function Register_Description( Index : integer ) : PChar ;
                override ;

            procedure Restart ; override ;

            function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

            function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

            function Clear_Internal_Watchpoint( Address : int64 ;
                Memory : boolean ; Access : integer ) : TUnified_Exception ; override ;

            function Set_Internal_Watchpoint( Address : int64 ;
                Memory : boolean ; Access : integer ) : TUnified_Exception ; override ;

            function Get_Current_Address( Space : integer ; Physical : boolean ) : int64 ;
                override ;

            procedure Set_Current_Address(Space : integer ; Physical : boolean ;
                Value : int64 ) ; override ;

            procedure Stop ; override ;

            function Top_Of_Stack( Index : integer ) : int64 ; override ;

            function Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;
                override ;
    end ; // TZ80_CPU


implementation

uses { Borland... }
     SysUtils, { Allocmem }

     { Z80 }
     Z80Util,

     { Other... }
     _ASCII, { CR }
     CVT, { Cvtb }
     HTML, { TXML_Parser }
     Num1s, { num1 }
     Parse, // TString_Parser
     SStreams, // TCOM_String_Stream
     Standard ; // Bit_Values

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ; external 'CEF_Util.dll' ;

const AF = 0 ;
      BC = 1 ;
      DE = 2 ;
      HL = 3 ;

// Profiler domains...
const Domain_Port_Outputs = 0 ;
const Domain_Port_Inputs = 1 ;
const Domain_Execution_Addresses = 2 ;
const Domain_Instructions = 3 ;
const Domain_Other = 4 ;
  const Domain_Other_Instruction_Count = 0 ;
  const Domain_Other_Clock = 1 ;


// CPU states...
      // All modes...
const State_Port_Input = 0 ;
const State_Port_Output = 1 ;
const State_Interrupt = 2 ;
      // Z80 states...
const State_NMI = 3 ;
      // 8085 states...
const State_RST5 = 4 ;
const State_RST6 = 5 ;
const State_RST7 = 6 ;

{ Utility routines... }

function Number_Format( Base : integer ; Value : String ; Size : integer ) : String ;
{ Return a number with proper number of leading zeroes.  Size is bytes }

var A : Integer ;

begin
    A := Size * 8 ;
    if( Base = 8 ) then
    begin
        A := A div 3 + 1 ;
    end else
    begin
        A := A div 4 ; { Determine number of digits }
    end ;
    if( Base = 10 ) then
    begin
        A := A + 1 ;
    end ;
    while( Length( Value ) < A ) do
    begin
        Value := '0' + Value ; { Pad with zeroes }
    end ;
    Number_Format := Value ; { Return result }
end ; { Numer_Format }



function Instruction_Name( I : integer ; Mode : TMode ) : string ;

begin
    Result := inttostr( I ) ;
    case I of
        0 : Result := 'NOP' ;
        $27 : Result := 'DAA' ;
        $80..$87 : Result := 'ADD' ;
        $88..$8F : Result := 'ADC' ;
        $90..$97 : Result := 'SUB' ;
        $C1 : Result := 'POP BC' ;
        $C5 : Result := 'PUSH BC' ;
        $C7 : Result := 'RST 0' ;
        $C9 : Result := 'RET' ;
        $CD : Result := 'CALL' ;
        $CF : Result := 'RST 1' ;
        $D1 : Result := 'POP DE' ;
        $D3 : Result := 'OUT' ;
        $D5 : Result := 'PUSH DE' ;
        $D7 : Result := 'RST 2' ;
        $DB : Result := 'IN' ;
        $DF : Result := 'RST 3' ;
        $E1 : Result := 'POP HL' ;
        $E5 : Result := 'PUSH HL' ;
        $E7 : Result := 'RST 4' ;
        $EF : Result := 'RST 5' ;
        $F1 : Result := 'POP PSW' ;
        $F3 : Result := 'DI' ;
        $F5 : Result := 'PUSH PSW' ;
        $F7 : Result := 'RST 6' ;
        $FB : Result := 'EI' ;
        $FF : Result := 'RST 7' ;
    end ;
    if( Mode = Z80 ) then
    begin
        case I of
            1 : Result := 'LD BC' ;
            2 : Result := 'LD (BC),A' ;
            3 : Result := 'INC BC' ;
            4 : Result := 'INC B' ;
            5 : Result := 'DEC B' ;
            6 : Result := 'LD B' ;
            7 : Result := 'RLCA' ;
            8 : Result := 'EX AF,AF''' ;
            9 : Result := 'ADD HL,BC' ;
            $A: Result := 'LD A,(BC)' ;
            $B : Result := 'DEC BC' ;
            $C : Result := 'INC C' ;
            $D : Result := 'DEC C' ;
            $E : Result := 'LD C' ;
            $F : Result := 'RRCA' ;
            $10 : Result := 'DJNZ' ;
            $11 : Result := 'LD DE' ;
            $12 : Result := 'LD (DE),A' ;
            $13 : Result := 'INC DE' ;
            $14 : Result := 'INC D' ;
            $15 : Result := 'DEC D' ;
            $16 : Result := 'LD D' ;
            $17 : Result := 'RLA' ;
            $18 : Result := 'JR' ;
            $19 : Result := 'ADD HL,DE' ;
            $1A : Result := 'LD A,(DE)' ;
            $1B : Result := 'DEC DE' ;
            $1C : Result := 'INC E' ;
            $1D : Result := 'DEC E' ;
            $1E : Result := 'LD E' ;
            $1F : Result := 'RRA' ;
            $20 : Result := 'JR NZ' ;
            $21 : Result := 'LD HL' ;
            $22 : Result := 'LD (),HL' ;
            $23 : Result := 'INC HL' ;
            $24 : Result := 'INC H' ;
            $25 : Result := 'DEC H' ;
            $26 : Result := 'LD H' ;
            $28 : Result := 'JR Z' ;
            $29 : Result := 'ADD HL,HL' ;
            $2A : Result := 'LD HL,()' ;
            $2B : Result := 'DEC HL' ;
            $2C : Result := 'INC L' ;
            $2D : Result := 'DEC L' ;
            $2E : Result := 'LD L' ;
            $2F : Result := 'CPL' ;
            $30 : Result := 'JR NC' ;
            $31 : Result := 'LD SP' ;
            $32 : Result := 'LD (),A' ;
            $33 : Result := 'INC SP' ;
            $34 : Result := 'INC M' ;
            $35 : Result := 'DEC M' ;
            $36 : Result := 'LD (HL)' ;
            $37 : Result := 'SCF' ;
            $38 : Result := 'JR C' ;
            $39 : Result := 'ADD HL,SP' ;
            $3A : Result := 'LD A,()' ;
            $3B : Result := 'DEC SP' ;
            $3C : Result := 'INC A' ;
            $3D : Result := 'DEC A' ;
            $3E : Result := 'LD A' ;
            $3F : Result := 'CCF' ;
            $40..$75 : Result := 'LD' ;
            $76 : Result := 'HALT' ;
            $77..$7F : Result := 'LD' ;
            $98..$9F : Result := 'SBC' ;
            $A0..$A7 : Result := 'AND' ;
            $A8..$AF : Result := 'XOR' ;
            $B0..$B7 : Result := 'OR' ;
            $B8..$BF : Result := 'CP' ;
            $C0 : Result := 'RET NZ' ;
            $C2 : Result := 'JP NZ' ;
            $C3 : Result := 'JP' ;
            $C4 : Result := 'CALL NZ' ;
            $C6 : Result := 'ADD A' ;
            $C8 : Result := 'RET Z' ;
            $CA : Result := 'JP Z' ;
            $CB : Result := 'CB extension' ;
            $CC : Result := 'CALL Z' ;
            $CE : Result := 'ADC A' ;
            $D0 : Result := 'RET NC' ;
            $D2 : Result := 'JP NC' ;
            $D4 : Result := 'CALL NC' ;
            $D6 : Result := 'SUB' ;
            $D8 : Result := 'RET C' ;
            $D9 : Result := 'EXX' ;
            $DA : Result := 'JP C' ;
            $DC : Result := 'CALL C' ;
            $DD : Result := 'DD extension' ;
            $DE : Result := 'SBC A' ;
            $E0 : Result := 'RET PO' ;
            $E2 : Result := 'JP PO' ;
            $E3 : Result := 'EX (SP),HL' ;
            $E5 : Result := 'CALL PO' ;
            $E6 : Result := 'AND' ;
            $E8 : Result := 'RET PE' ;
            $E9 : Result := 'JP (HL)' ;
            $EA : Result := 'JP PE' ;
            $EB : Result := 'EX DE,HL' ;
            $EC : Result := 'CALL PE' ;
            $ED : Result := 'ED extension' ;
            $EE : Result := 'XOR' ;
            $F0 : Result := 'RET P' ;
            $F2 : Result := 'JP P' ;
            $F4 : Result := 'CALL P' ;
            $F6 : Result := 'OR' ;
            $F8 : Result := 'RET M' ;
            $F9 : Result := 'LD SP,HL' ;
            $FA : Result := 'JP M' ;
            $FC : Result := 'CALL M' ;
            $FD : Result := 'FD extension' ;
            $FE : Result := 'CP' ;
            $140 : Result := 'IN B,(C)' ;
            $142 : Result := 'SBC HL,BC' ;
            $141 : Result := 'OUT (C),B' ;
            $143 : Result := 'LD' ;
            $144 : Result := 'NEG' ;
            $145 : Result := 'RETN' ;
            $146 : Result := 'IM0' ;
            $147 : Result := 'LD I,A' ;
            $148 : Result := 'IN C,(C)' ;
            $149 : Result := 'OUT (C),C' ;
            $14A : Result := 'ADC HL,BC' ;
            $14B : Result := 'LD BC' ;
            $14D : Result := 'RETI' ;
            $14F : Result := 'LD R,A' ;
            $150 : Result := 'IN D,(C)' ;
            $151 : Result := 'OUT (C),D' ;
            $152 : Result := 'SBC HL,DE' ;
            $153 : Result := 'LD' ;
            $156 : Result := 'IM1' ;
            $157 : Result := 'LD A,I' ;
            $158 : Result := 'IN E,(C)' ;
            $159 : Result := 'OUT (C),E' ;
            $15A : Result := 'ADC HL,DE' ;
            $15B : Result := 'LD DE' ;
            $15E : Result := 'IM2' ;
            $15F : Result := 'LD A,R' ;
            $160 : Result := 'IN H,(C)' ;
            $161 : Result := 'OUT (C),H' ;
            $162 : Result := 'SBC HL,HL' ;
            $163 : Result := 'LD' ;
            $167 : Result := 'RRD' ;
            $168 : Result := 'IN L,(C)' ;
            $169 : Result := 'OUT (C),L' ;
            $16A : Result := 'ADC HL,HL' ;
            $16B : Result := 'LD HL' ;
            $16F : Result := 'RLD' ;
            $172 : Result := 'SBC HL,SP' ;
            $173 : Result := 'LD' ;
            $178 : Result := 'IN A,(C)' ;
            $179 : Result := 'OUT (C),A' ;
            $17A : Result := 'ADC HL,SP' ;
            $17B : Result := 'LD SP' ;
            $1A0 : Result := 'LDI' ;
            $1A1 : Result := 'CPI' ;
            $1A2 : Result := 'INI' ;
            $1A3 : Result := 'OUTI' ;
            $1A8 : Result := 'LDD' ;
            $1A9 : Result := 'CPD' ;
            $1AA : Result := 'IND' ;
            $1AB : Result := 'OUTD' ;
            $1B0 : Result := 'LDIR' ;
            $1B1 : Result := 'CPIR' ;
            $1B2 : Result := 'INIR' ;
            $1B3 : Result := 'OTIR' ;
            $1B8 : Result := 'LDDR' ;
            $1B9 : Result := 'CPDR' ;
            $1BA : Result := 'INDR' ;
            $1BB : Result := 'OTDR' ;
            $200..$207 : Result := 'RLC' ;
            $208..$20D : Result := 'RRC' ;
            $20F : Result := 'RRC A' ;
            $210..$215 : Result := 'RL' ;
            $217 : Result := 'RL A' ;
            $21F : Result := 'RR A' ;
            $218..$21D : Result := 'RR' ;
            $220..$225 : Result := 'SLA' ;
            $227 : Result := 'SLA A' ;
            $228..$22D : Result := 'SRA' ;
            $22F : Result := 'SRA A' ;
            $238..$23D : Result := 'SRL' ;
            $23F : Result := 'SRL A' ;
            $240..$27F : Result := 'BIT' ;
            $280..$2BF : Result := 'RES' ;
            $2C0..$2FF : Result := 'SET' ;
            $309 : Result := 'ADD index' ;
            $321 : Result := 'LD index' ;
            $322 : Result := 'LD ,(index)' ;
            $323 : Result := 'INC index' ;
            $32A : Result := 'LD index' ;
            $32B : Result := 'DEC index' ;
            $334 : Result := 'INC index' ;
            $335 : Result := 'DEC index' ;
            $336 : Result := 'LD (index)' ;
            $346 : Result := 'LD ,(index)' ;
            $370 : Result := 'LD (index)' ;
            $386 : Result := 'ADD A,index' ;
            $38E : Result := 'ADC A,index' ;
            $396 : Result := 'SUB (index)' ;
            $39E : Result := 'SBC A,(index)' ;
            $3A6 : Result := 'AND index' ;
            $3AE : Result := 'XOR (index)' ;
            $3B6 : Result := 'OR (index)' ;
            $3BE : Result := 'CP index' ;
            $3CB : Result := 'DDCB extension' ;
            $3E1 : Result := 'POP index' ;
            $3E3 : Result := 'EX (SP,index' ;
            $3E5 : Result := 'PUSH index' ;
            $3E9 : Result := 'JP (index)' ;
            $3F9 : Result := 'LD SP' ;
            $409 : Result := 'ADD index' ;
            $421 : Result := 'LD index' ;
            $422 : Result := 'LD ,(index)' ;
            $423 : Result := 'INC index' ;
            $42A : Result := 'LD index' ;
            $42B : Result := 'DEC index' ;
            $434 : Result := 'INC index' ;
            $435 : Result := 'DEC index' ;
            $436 : Result := 'LD (index)' ;
            $446 : Result := 'LD ,(index)' ;
            $470 : Result := 'LD (index)' ;
            $486 : Result := 'ADD A,index' ;
            $48E : Result := 'ADC A,index' ;
            $496 : Result := 'SUB (index)' ;
            $49E : Result := 'SBC A,(index)' ;
            $4A6 : Result := 'AND index' ;
            $4AE : Result := 'XOR (index)' ;
            $4B6 : Result := 'OR (index)' ;
            $4BE : Result := 'CP index' ;
            $4CB : Result := 'FDCB extension' ;
            $4E1 : Result := 'POP index' ;
            $4E3 : Result := 'EX (SP,index' ;
            $4E5 : Result := 'PUSH index' ;
            $4E9 : Result := 'JP (index)' ;
            $4F9 : Result := 'LD SP' ;
            $506 : Result := 'RLC (index)' ;
            $50E : Result := 'RRC (index)' ;
            $516 : Result := 'RL (index)' ;
            $51E : Result := 'RR (index)' ;
            $526 : Result := 'SLA (index)' ;
            $52E : Result := 'SRC (index)' ;
            $53E : Result := 'SRL (index)' ;
            $546 : Result := 'BIT 0,(index)' ;
            $54E : Result := 'BIT 1,(index)' ;
            $556 : Result := 'BIT 2,(index)' ;
            $55E : Result := 'BIT 3,(index)' ;
            $566 : Result := 'BIT 4,(index)' ;
            $56E : Result := 'BIT 5,(index)' ;
            $576 : Result := 'BIT 6,(index)' ;
            $57E : Result := 'BIT 7,(index)' ;
            $589 : Result := 'RES 0,(index)' ;
            $58E : Result := 'RES 1,(index)' ;
            $596 : Result := 'RES 2,(index)' ;
            $59E : Result := 'RES 3,(index)' ;
            $5A6 : Result := 'RES 4,(index)' ;
            $5AE : Result := 'RES 5,(index)' ;
            $5B6 : Result := 'RES 6,(index)' ;
            $5BE : Result := 'RES 7,(index)' ;
            $5C6 : Result := 'SET 0,(index)' ;
            $5CE : Result := 'SET 1,(index)' ;
            $5D6 : Result := 'SET 2,(index)' ;
            $5DE : Result := 'SET 3,(index)' ;
            $5E6 : Result := 'SET 4,(index)' ;
            $5EE : Result := 'SET 5,(index)' ;
            $5F6 : Result := 'SET 6,(index)' ;
            $5FE : Result := 'SET 7,(index)' ;
            $606 : Result := 'RLC (index)' ;
            $60E : Result := 'RRC (index)' ;
            $616 : Result := 'RL (index)' ;
            $61E : Result := 'RR (index)' ;
            $626 : Result := 'SLA (index)' ;
            $62E : Result := 'SRC (index)' ;
            $63E : Result := 'SRL (index)' ;
            $646 : Result := 'BIT 0,(index)' ;
            $64E : Result := 'BIT 1,(index)' ;
            $656 : Result := 'BIT 2,(index)' ;
            $65E : Result := 'BIT 3,(index)' ;
            $666 : Result := 'BIT 4,(index)' ;
            $66E : Result := 'BIT 5,(index)' ;
            $676 : Result := 'BIT 6,(index)' ;
            $67E : Result := 'BIT 7,(index)' ;
            $689 : Result := 'RES 0,(index)' ;
            $68E : Result := 'RES 1,(index)' ;
            $696 : Result := 'RES 2,(index)' ;
            $69E : Result := 'RES 3,(index)' ;
            $6A6 : Result := 'RES 4,(index)' ;
            $6AE : Result := 'RES 5,(index)' ;
            $6B6 : Result := 'RES 6,(index)' ;
            $6BE : Result := 'RES 7,(index)' ;
            $6C6 : Result := 'SET 0,(index)' ;
            $6CE : Result := 'SET 1,(index)' ;
            $6D6 : Result := 'SET 2,(index)' ;
            $6DE : Result := 'SET 3,(index)' ;
            $6E6 : Result := 'SET 4,(index)' ;
            $6EE : Result := 'SET 5,(index)' ;
            $6F6 : Result := 'SET 6,(index)' ;
            $6FE : Result := 'SET 7,(index)' ;
        end ;
    end else
    begin
        case I of
            1 : Result := 'LXI B' ;
            2 : Result := 'STAX B' ;
            3 : Result := 'INX B' ;
            4 : Result := 'INR B' ;
            5 : Result := 'DCR B' ;
            6 : Result := 'MVI B' ;
            7 : Result := 'RLC' ;
            9 : Result := 'DAD BC' ;
            $A: Result := 'LDAX B' ;
            $B : Result := 'DCX B' ;
            $C : Result := 'INR C' ;
            $D : Result := 'DCR C' ;
            $E : Result := 'MVI C' ;
            $F : Result := 'RRC' ;
            $11 : Result := 'LXI D' ;
            $12 : Result := 'STAX D' ;
            $13 : Result := 'INX D' ;
            $14 : Result := 'INR D' ;
            $15 : Result := 'DCR D' ;
            $16 : Result := 'MVI D' ;
            $17 : Result := 'RAL' ;
            $19 : Result := 'DAD DE' ;
            $1A : Result := 'LDAX D' ;
            $1B : Result := 'DCX D' ;
            $1C : Result := 'INR E' ;
            $1D : Result := 'DCR E' ;
            $1E : Result := 'MVI E' ;
            $1F : Result := 'RAR' ;
            $21 : Result := 'LXI H' ;
            $22 : Result := 'SHLD' ;
            $23 : Result := 'INX H' ;
            $24 : Result := 'INR H' ;
            $25 : Result := 'DCR H' ;
            $26 : Result := 'MVI H' ;
            $29 : Result := 'DAD HL' ;
            $2B : Result := 'DCX H' ;
            $2A : Result := 'LHLD' ;
            $2C : Result := 'INR L' ;
            $2D : Result := 'DCR L' ;
            $2E : Result := 'MVI L' ;
            $2F : Result := 'CMA' ;
            $31 : Result := 'LXI SP' ;
            $32 : Result := 'STA' ;
            $33 : Result := 'INX SP' ;
            $34 : Result := 'INR M' ;
            $35 : Result := 'DCR M' ;
            $36 : Result := 'MVI M' ;
            $37 : Result := 'STC' ;
            $39 : Result := 'DAD SP' ;
            $3A : Result := 'LDA' ;
            $3B : Result := 'DCX SP' ;
            $3C : Result := 'INR A' ;
            $3D : Result := 'DCR A' ;
            $3E : Result := 'MVI A' ;
            $3F : Result := 'CMC' ;
            $40..$75 : Result := 'MOV' ;
            $76 : Result := 'HLT' ;
            $77..$7F : Result := 'MOV' ;
            $98..$9F : Result := 'SBB' ;
            $A0..$A7 : Result := 'ANA' ;
            $A8..$AF : Result := 'XRA' ;
            $B0..$B7 : Result := 'ORA' ;
            $B8..$BF : Result := 'CMP' ;
            $C0 : Result := 'RNZ' ;
            $C2 : Result := 'JNZ' ;
            $C3 : Result := 'JMP' ;
            $C4 : Result := 'CNZ' ;
            $C6 : Result := 'ADI' ;
            $C8 : Result := 'RZ' ;
            $CA : Result := 'JZ' ;
            $CC : Result := 'CZ' ;
            $CE : Result := 'ACI' ;
            $D0 : Result := 'RNC' ;
            $D2 : Result := 'JNC' ;
            $D4 : Result := 'CNC' ;
            $D6 : Result := 'SUI' ;
            $D8 : Result := 'RC' ;
            $DA : Result := 'JC' ;
            $DC : Result := 'CC' ;
            $DE : Result := 'SBC A' ;
            $E0 : Result := 'RPO' ;
            $E2 : Result := 'JPO' ;
            $E3 : Result := 'XTHL' ;
            $E5 : Result := 'CPO' ;
            $E6 : Result := 'ANI' ;
            $E8 : Result := 'RPE' ;
            $EA : Result := 'JPE' ;
            $EB : Result := 'XCHG' ;
            $EC : Result := 'CPE' ;
            $EE : Result := 'XRI' ;
            $E9 : Result := 'PCHL' ;
            $F0 : Result := 'RP' ;
            $F2 : Result := 'JP' ;
            $F4 : Result := 'CP' ;
            $F6 : Result := 'ORI' ;
            $F8 : Result := 'RM' ;
            $F9 : Result := 'SPHL' ;
            $FA : Result := 'JM' ;
            $FC : Result := 'CM' ;
            $FE : Result := 'CPI' ;
        end ;
        if( Mode = M8085 ) then
        begin
            if( I = $20 ) then
            begin
                Result := 'RIM' ;
                Exit ;
            end else
            if( I = $30 ) then
            begin
                Result := 'SIM' ;
                Exit ;
            end ;
        end ;
    end ;
end ; // Instruction_Name


// TZ80_Profiler methods...

// API...

procedure TZ80_Profiler.Generate_Report ;

var Base : integer ;
    Loop : integer ;

begin
    // Clear dirty flag and set up...
    Dirty := False ;
    if( Outputs <> nil ) then
    begin
        Outputs.Clear ;
        Inputs.Clear ;
        Addresses.Clear ;
        Instruction_Lines.Clear ;
    end else
    begin
        Outputs := TStringList.Create ;
        Inputs := TStringList.Create ;
        Addresses := TStringList.Create ;
        Instruction_Lines := TStringList.Create ;
    end ;
    if( _Mode = Z80 ) then
    begin
        Base := 16 ;
    end else
    begin
        Base := 8 ;
    end ;

    // Generate port output report
    for Loop := 0 to 255 do
    begin
        if( Port_Outputs[ Loop ] <> 0 ) then
        begin
            Outputs.Add( cvtb( 10, Base, inttostr( Loop ) ) + ': ' + inttostr( Port_Outputs[ Loop ] ) ) ;
        end ;
    end ;

    // Generate port input report
    for Loop := 0 to 255 do
    begin
        if( Port_Inputs[ Loop ] <> 0 ) then
        begin
            Inputs.Add( cvtb( 10, Base, inttostr( Loop ) ) + ': ' + inttostr( Port_Inputs[ Loop ] ) ) ;
        end ;
    end ;

    // Generate execution address report
    for Loop := 0 to 65535 do
    begin
        if( Execution_Addresses[ Loop ] <> 0 ) then
        begin
            Addresses.Add( cvtb( 10, Base, inttostr( Loop ) ) + ': ' + inttostr( Execution_Addresses[ Loop ] ) ) ;
        end ;
    end ;

    // Generate instruction report
    for Loop := 0 to $6FF do
    begin
        if( Instructions[ Loop ] <> 0 ) then
        begin
            Instruction_Lines.Add( Instruction_Name( Loop, _Mode ) + ': ' + inttostr( Instructions[ Loop ] ) ) ;
        end ;
    end ;
    Instruction_Lines.Sort ;
end ; // TZ80_Profiler.Generate_Report


procedure TZ80_Profiler.Increment( Domain, Index : integer ) ;

begin
    Dirty := True ;
    case Domain of
        Domain_Port_Outputs: inc( Port_Outputs[ Index ] ) ;
        Domain_Port_Inputs: inc( Port_Inputs[ Index ] ) ;
        Domain_Execution_Addresses: inc( Execution_Addresses[ Index ] ) ;
        Domain_Instructions: inc( Instructions[ Index ] ) ;
        Domain_Other: if( Index = Domain_Other_Instruction_Count ) then
                      begin
                          inc( _Instruction_Count ) ;
                      end ;
    end ;
end ;


procedure TZ80_Profiler.Increment_Clock( Count : integer ) ;

begin
    Dirty := True ;
    _Clock := _Clock + Count ;
end ;


procedure TZ80_Profiler.Set_Mode( Mode : TMode ) ;

begin
    _Mode := Mode ;
end ;


// Overrides...

procedure TZ80_Profiler.Clear( Domain : integer ) ;

begin
    if( Domain = -1 ) then
    begin
        Clear( Domain_Port_Outputs ) ;
        Clear( Domain_Port_Inputs ) ;
        Clear( Domain_Execution_Addresses ) ;
        Clear( Domain_Instructions ) ;
        Clear( Domain_Other ) ;
        exit ;
    end ;

    case Domain of
        Domain_Port_Outputs :
            begin
                fillchar( Port_Outputs, sizeof( Port_Outputs ), 0 ) ;
                if( Outputs <> nil ) then
                begin
                    Outputs.Clear ;
                end ;
            end ;
        Domain_Port_Inputs :
            begin
                fillchar( Port_Inputs, sizeof( Port_Outputs ), 0 ) ;
                if( Inputs <> nil ) then
                begin
                    Inputs.Clear ;
                end ;
            end ;
        Domain_Execution_Addresses :
            begin
                fillchar( Execution_Addresses, sizeof( Execution_Addresses ), 0 ) ;
                if( Addresses <> nil ) then
                begin
                    Addresses.Clear ;
                end ;
            end ;
        Domain_Instructions :
            begin
                fillchar( Instructions, sizeof( Instructions ), 0 ) ;
                if( Instruction_Lines <> nil ) then
                begin
                    Instruction_Lines.Clear ;
                end ;
            end ;
        Domain_Other :
            begin
                _Clock := 0 ;
                _Instruction_Count := 0 ;
            end ;
    end ; // case Domain
end ; // TZ80_Profiler.Clear


function TZ80_Profiler.Domain_Name( Index : integer ) : PChar ;

begin
    case Index of
        Domain_Port_Outputs: Temp_Domain_Name := 'Port output' ;
        Domain_Port_Inputs: Temp_Domain_Name := 'Port input' ;
        Domain_Execution_Addresses: Temp_Domain_Name := 'Executed addresses' ;
        Domain_Instructions: Temp_Domain_Name := 'Instructions' ;
        Domain_Other: Temp_Domain_Name := 'Other information' ;
        else
            begin
                Result := nil ;
                exit ;
            end ;
    end ;
    Result := PChar( Temp_Domain_Name ) ;
end ;


function TZ80_Profiler.Report_Line( Domain, Index : integer ) : PChar ;

begin
    if( Dirty ) then
    begin
        Generate_Report ;
    end ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        case Domain of
            Domain_Port_Outputs:
                if( Index < Outputs.Count ) then
                begin
                    Temp_Report_Line := Outputs[ Index ] ;
                end else
                begin
                    exit ;
                end ;
            Domain_Port_Inputs:
                if( Index < Inputs.Count ) then
                begin
                    Temp_Report_Line := Inputs[ Index ] ;
                end else
                begin
                    exit ;
                end ;
            Domain_Execution_Addresses:
                if( Index < Addresses.Count ) then
                begin
                    Temp_Report_Line := Addresses[ Index ] ;
                end else
                begin
                    exit ;
                end ;
            Domain_Instructions:
                if( Index < Instruction_Lines.Count ) then
                begin
                    Temp_Report_Line := Instruction_Lines[ Index ] ;
                end else
                begin
                    exit ;
                end ;
            Domain_Other:
                case Index of
                    Domain_Other_Instruction_Count : Temp_Report_Line := 'Instructions executed: ' + inttostr( _Instruction_Count ) ;
                    Domain_Other_Clock : Temp_Report_Line := 'Clock cycles: ' + inttostr( _Clock ) ;
                    else exit ;
                end
            else exit ;
        end ;
    end ;
    Result := PChar( Temp_Report_Line ) ;
end ; // TZ80_Profiler.Report_Line



function Register_To_Breakpoint( _Set, Index : integer ) : integer ;

begin
    if( _Set = 0 ) then
    begin
        Result := Index + 2 ;
    end else
    begin
        Result := Index + 14 ;
    end ;
end ;


function Get_Mask( Value : Integer ) : String ; { Return bits names }

var A : String ;
    B : string[ 3 ] ;

begin
    A := '' ;
    B := 'P/V' ;
    if( ( Value and 128 ) = 128 ) then
    begin
        A := 'S ' ;
    end else
    begin
        A := 'NS ' ;
    end ;
    if( ( Value and 64 ) = 64 ) then
    begin
        A := A + 'Z ' ;
    end else
    begin
        A := A + 'NZ ' ;
    end ;
    A := A + '0 ' ;
    if ( Value and 16 ) = 16 then
    begin
        A := A + 'H ' ;
    end else
    begin
        A := A + 'NH ' ;
    end ;
    A := A + '0 ' ;
    if( ( Value and 4 ) = 4 ) then
    begin
        A := A + B + ' ' ;
    end else
    begin
        A := A + 'N' + B + ' ' ;
    end ;
    if ( Value and 2 ) = 2 then
    begin
        A := A + '1 ' ;
    end else
    begin
        A := A + '0 ' ;
    end ;
    if( ( Value and 1 ) = 1 ) then
    begin
        A := A + 'C' ;
    end else
    begin
        A := A + 'NC' ;
    end ;
    Get_Mask := A ; { Return value }
end ; // Get_Mask


// TZ80_CPU methods...

// Constructors and destructors...

constructor TZ80_CPU.Create ;

begin
    inherited Create ;

    _Memory_Watchpoints := Get_Watchpoint_Manager ;
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
    _Breakpoints := TInteger_List.Create ;
    _Speed := 2000 ; // 2 MHz
    Base := Default_Base ;
    Segments := TInteger_List.Create ;
    Restart ; // Do power-on reset
end ;


destructor TZ80_CPU.Destroy ;

begin
    Clear_Watchpoints ;
    _Memory_Watchpoints.Terminate ;
    _Memory_Watchpoints := nil ;
    _Breakpoints.Free ;
    _Breakpoints := nil ;
    Segments.Free ;
    Segments := nil ;

    inherited Destroy ;
end ;


// API...

procedure TZ80_CPU.Push( X : Integer ) ;

begin
    Sp := _Sp - 1 ;
    ByteWrite( Sp, X ) ;
end ;


function TZ80_CPU.Pop : Integer ;

begin
    Pop := Byte_Read( SP ) ;
    SP := _SP + 1 ;
end ;


procedure TZ80_CPU.Push_PC ; { Push PC to stack }

begin
    Push( Hi( Pc ) ) ;
    Push( Lo( Pc ) ) ;
end ;


procedure TZ80_CPU.State_Change_Notice( Index : integer ; State : boolean ) ;

begin
    _UI.State_Change_Notice( Parent, Index, State ) ;

    case Index of
        State_Interrupt : Log_Trace( 'Process interrupt' ) ;
        State_NMI : Log_Trace( 'Process non-maskable interrupt' ) ;
        State_RST5 : Log_Trace( 'Process RST5.5 interrupt' ) ;
        State_RST6 : Log_Trace( 'Process RST6.5 interrupt' ) ;
        State_RST7 : Log_Trace( 'Process RST7.5 interrupt' ) ;
    end ;
end ;


procedure TZ80_CPU.Log_Trace( const Description : string ) ;

begin
    if( _Trace ) then
    begin
        Temp_Log_Trace := Description ;
        _UI.Log_Trace( Parent, PChar( Temp_Log_Trace ) ) ;
    end ;
end ;


procedure TZ80_CPU.Signal_Exception( const Description : string ; Index : longint ) ;

begin
    Temp_Signal_Exception := Description ;
    _UI.Signal_Exception( Parent, PChar( Temp_Signal_Exception ), Index ) ;
end ;


procedure TZ80_CPU.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
    Memory, Internal, Port : boolean ) ;

begin
    _UI.Watchpoint_Notice( Address, Access, Tag, Parent, Memory, Internal, Port ) ;
end ;


function TZ80_CPU.Instruction_At( Address : integer ) : string ;

var Stream : TCOM_String_Stream ;

begin
    Stream := TCOM_String_Stream.Create ;
    Disassemble( Address, Base, 1, Stream ) ;
    Result := string( Stream.As_String ) ;
    Stream.Detach ;
end ;


procedure TZ80_CPU.Do_NMI ;

begin
    _Halted := False ;
    if( In_NMI ) then
    begin
        Parent.Set_Error( Z80_Nested_NMI ) ;
        Signal_Exception( '', 1 ) ; // Nested NMI
     end else
     begin
 	Push_PC ;
 	IFF2 := Interrupts ;
	Interrupts := False ;
	PC := $66 ;
	In_NMI := True ;
        State_Change_Notice( State_NMI, True ) ;
    end ;
end ;


procedure TZ80_CPU.Do_Interrupt ;

begin
    if( not Interrupts ) then // Interrupts disabled
    begin
        exit ;
    end ;
    _Halted := False ;
    Interrupt_Instruction := True ;
    State_Change_Notice( State_Interrupt, True ) ;
end ;


procedure TZ80_CPU.Do_Wait ;

begin
    try
        _UI.Idle( Parent ) ;
    except
    end ;
end ;


function TZ80_CPU.Bus_Examine( Address : Integer ) : Char ; { Return data at memory Address }

var Component : TComponent ;
    Loop, Size : integer ;
    UEC : TUnified_Exception ;

begin
    for Loop := 0 to Parent.Inputs.Count - 1 do
    begin
        Size := 8 ;
        Component := TComponent( Parent.Inputs[ Loop ] ) ;
        UEC := Component.Examine( Address, Size, @Result, True ) ;
        if( UEC = nil ) then
        begin
            exit ;
        end ;
    end ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Size := 8 ;
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        UEC := Component.Examine( Address, Size, @Result, True ) ;
        if( UEC = nil ) then
        begin
            exit ;
        end ;
    end ;
    Result := #255 ; // No component responded to this address
end ;


function TZ80_CPU.Bus_Read( Address : Integer ; IO_Type : longint ) : Char ; { Return data at Address }

var Component : TComponent ;
    Loop : integer ;

begin
    Result := #0 ;
    Memory_Data_Latch := 255 ; // Default if nothing responds
    try
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Address, 8, IO_Type ) ) then
            begin
                exit ;
            end ;
        end ;
    finally
        Bus_Read := char( Memory_Data_Latch ) ;
    end ;
end ;


function TZ80_CPU.ByteRead( Address : Integer ) : Char ; { Return data at Address }

begin
    Result := Bus_Read( Address and $FFFF, IO_Type_Memory ) ;
end ;


function TZ80_CPU.Byte_Read( Address : integer ) : integer ;
{ Read a byte from the specified address }

begin
    Byte_Read := ord( ByteRead( Address ) ) ;
end ;


function TZ80_CPU.Word_Read( Address : integer ) : integer ;
{ Read a word from the specified address }

var X : integer ;

begin
    X := Byte_Read( Address ) ;
    X := X or ( swap( Byte_Read( Address + 1 ) ) and $FF00 ) ;
    Word_Read := X ;
end ;


procedure TZ80_CPU.ByteWrite( Address, Value : Integer ) ; { Write to memory }

var Component : TComponent ;
    Loop : integer ;
    UEC : TUnified_Exception ;

begin
    Address := Address and $FFFF ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        UEC := Component.Write( Address, Value, 8, IO_Type_Memory ) ;
        if( UEC <> nil ) then
        begin
            exit ;
        end ;
    end ;
end ;


procedure TZ80_CPU.MemWrite( Address : Integer ; Value : String ) ; { Write to memory }

var A : Integer ;

begin
    for A := 1 to Length( Value ) do
    begin
        ByteWrite( Address, Ord( Value[ A ] ) ) ; { Write a byte }
        Address := Address + 1 ; { Increment address }
    end
end ;


procedure TZ80_CPU.Increment_Clock( Count : integer ) ;

var R : extended ;
    W : int64 ; // Amount of time to wait (in picoseconds)

begin
    if( _Profiling ) then
    begin
        TZ80_Profiler( Parent.Profiler ).Increment_Clock( Count ) ;
    end ;
    try
        if( _UI.Clock <> nil ) then
        begin
            R := 1 ;
            R := R / _Speed ; // Cycle time, in seconds
            R := R * Count ;
            R := int( R * 1000000000.0 ) ; // Convert from seconds to nanoseconds
            W := trunc( R ) ;
            Blocked := True ;
            _UI.Clock.Block( Parent, W ) ;
        end ;
    except
    end ;
end ;


procedure TZ80_CPU.Send_Signal( const Name : string ; Value : boolean ) ;

var Component : TComponent ;
    Index, Loop : integer ;

begin
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        Component.Set_Signal( PChar( Name ), Value ) ;
    end ;
    case Mode of
        M8080 : Index := 0 ;
        M8085 :
            if( Name = 'INTR' ) then
            begin
                Index := 0 ;
            end else
            if( Name = 'RST5.5' ) then
            begin
                Index := 1 ;
            end else
            if( Name = 'RST6.5' ) then
            begin
                Index := 2 ;
            end else
            if( Name = 'RST7.5' ) then
            begin
                Index := 3 ;
            end else
            if( Name = 'TRAP' ) then
            begin
                Index := 4 ;
            end else
            if( Name = 'SID' ) then
            begin
                Index := 5 ;
            end else
            if( Name = 'SOD' ) then
            begin
                Index := 6 ;
            end ;
        Z80 :
            if( Name = 'NMI' ) then
            begin
                Index := 0 ;
            end else
            begin
                Index := 1 ;
            end ;
    end ;
    if( _Logger <> nil ) then
    begin
        _Logger.Log( Parent, PChar( Name + ' = ' + inttostr( ord( Value ) ) ), -1, True, LT_Sent_Signal ) ;
    end ;
    _UI.Signal_Change_Notice( Parent, Index, Value <> Parent.Signal_Active_Low( Index ) ) ;
end ; // TZ80_CPU.Send_Signal


procedure TZ80_CPU.Clear_Watchpoints ;

begin
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
end ;


function TZ80_CPU.Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;

begin
    Result := TZ80_Assembler.Create ;
    TZ80_Assembler( Result ).CPU := self ;
    TZ80_Assembler( Result ).Segments := Segments ;
    Result.Initialize( Master ) ;
    TZ80_Assembler( Result ).Base := Base ;
    TZ80_Assembler( Result ).Mode := Mode ;
end ;


function TZ80_CPU.Cancel_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

var Index : integer ;

begin
    Result := Parent.Set_Error( Z80_No_Breakpoint ) ; // Assume failure
    if( ( Address < 0 ) or ( Address > $FFFF ) ) then
    begin
        exit ;
    end ;
    Index := _Breakpoints.Indexof( Address ) ;
    if( Index = -1 ) then
    begin
        exit ;
    end ;
    _Breakpoints.Delete( Index ) ;
    Result := Parent.Set_Error( 0 ) ;
end ;


function TZ80_CPU.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := _Memory_Watchpoints.Clear_Watchpoint( Address, Access ) ;
    end else
    begin
        Result := Parent.Set_Error( 0 ) ;
        if( ( Address < Get_Low_Port ) or ( Address > Get_High_Port ) ) then
        begin
            Result := Parent.Set_Error( Z80_Invalid_Address ) ;
            exit ;
        end ;
        _Port_Watchpoints[ Address ] :=
            _Port_Watchpoints[ Address ] and not( Access ) ;
    end ; // if( Memory )
end ; // TZ80_CPU.Clear_Watchpoint


function TZ80_CPU.Disassemble( Address : int64 ; Base, Size : longint ;
    Stream : TCOM_Stream ) : TUnified_Exception ;

    function _Disassemble : string ;

    label Ee ;

    var A : Integer ;
        Bb, Cc : String ;
        DText, Instruction : string ;
        Temp : integer ;
        TPC : integer ;

        function Base_Suffix( Base : integer ) : string ;

        begin
            case Base of
                16 : Result := 'H' ;
                10 : Result := '.' ;
                2 : Result := 'B' ;
                8 : Result := 'Q' ;
                else Result := '' ;
            end ;
        end ;


        function Fetch : Integer ; { Fetch next byte }

        var A : Integer ;
            Temp : string ;

        begin
            A := ord( Bus_Examine( Tpc ) ) ;
            Tpc := Tpc + 1 ;
            Size := Size - 1 ;
            Temp := Cvtb( 10, Base, Num1( A ) ) ;
            if( ( length( Temp ) = 1 ) or ( pos( Temp[ 1 ], '0123456789' ) = 0 ) ) then
            begin
                Instruction := Instruction + '0' ;
            end ;
            Temp := Temp + Base_Suffix( Base ) ;
            Instruction := Instruction + Temp + ' ' ;
            Fetch := A ;
            DText := DText + Chr( A ) ;
        end ;


        function FetchWord : Integer ; { Fetch next byte }

        var A, B : Integer ;
            Temp : string ;

        begin
            A := Ord( Bus_Examine( Tpc ) ) ;
            B := Ord( Bus_Examine( Tpc + 1 ) ) ;
            Size := Size - 2 ;
            DText := DText + Chr( A ) + Chr( B ) ;
            A := A or Swap( B ) ;
            Temp := Cvtb( 10, Base, Num1( Cvtif( A ) ) ) ;
            while( length( Temp ) < 4 ) do
            begin
                Temp := '0' + Temp ;
            end ;
            if( pos( Temp[ 1 ], '0123456789' ) = 0 ) then
            begin
                Instruction := Instruction + '0' ;
            end ;
            Temp := Temp + Base_Suffix( Base ) ;
            Instruction := Instruction + Temp + ' ' ;
            FetchWord := A ;
            Tpc := Tpc + 2 ;
        end ;


        function Cvis( Value, Size : longint ) : string ;

        begin
            if( Value < 0 ) then
            begin
                Result := Num1( Value ) + '.' ;
                exit ;
            end ;
            Result := Cvtb( 10, Base, Num1( Value ) ) ;
            while( length( Result ) < Size ) do
            begin
                Result := '0' + Result ;
            end ;
            if( pos( Result[ 1 ], '0123456789' ) = 0 ) then
            begin
                Result := '0' + Result ;
            end ;
            Result := Result + Base_Suffix( Base ) ;
        end ;


        function Er( X, Y : Integer ) : String ;
        { Extract register from X ((X div Y) and 7) and return string }

        var A : Integer ;

        begin
            A := ( ( X div Y ) and 7 ) + 1 ;
            if( ( A = 7 ) and ( Mode = Z80 ) ) then
            begin
                Er := '(HL)' ;
            end else
            begin
                Er := Copy( 'BCDEHLMA', A, 1 ) ;
            end ;
        end ;


        function Er2( X, Y : Integer ) : String ;
        { Extract register from X ((X div Y) and 3) and return appropriate string }

        var A, Z : Integer ;

        begin
            A := ( ( X div Y ) and 3 ) + 1 ;
            if( ( Mode = Z80 ) or ( A = 4 ) ) then
            begin
                Z := 2 ;
            end else
            begin
                Z := 1 ;
            end ;
            Er2 := Copy( 'BCDEHLSP', A * 2 - 1, Z )
        end ;


        function Eqq( X, Y : Integer ) : String ;
        { Extract register from X ((X div Y) and 3) and return appropriate string }

        var A, Z : Integer ;

        begin
            A := ( ( X div Y ) and 3 ) + 1 ;
            if( Mode = Z80 ) then
            begin
                Z := 2 ;
            end else
            begin
                Z := 1 ;
            end ;
            if( ( Mode <> Z80 ) and ( A = 4 ) ) then
            begin
                Eqq := 'PSW' ;
            end else
            begin
                Eqq := Copy( 'BCDEHLAF', A * 2 - 1, Z ) ;
            end ;
        end ;

    var AA : string ;

    begin
        Tpc := Address ;
        Instruction := '' ;
        DText := '' ;
        A := Fetch ;
        if( A = 0 ) then { NOP }
        begin
            _Disassemble := 'NOP' ;
            Exit ;
        end ;
        if( A = $Db ) then { IN }
        begin
            Aa := Cvis( Fetch, 1 ) ;
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'IN   A,(' + Aa + ')' ;
            end else
            begin
                _Disassemble := 'IN   ' + Aa ;
            end ;
            Exit ;
        end ;
        if( A = $D3 ) then { OUT }
        begin
            Aa := Cvis( Fetch, 1 ) ;
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'OUT  (' + Aa + '),A' ;
            end else
            begin
                _Disassemble := 'OUT  ' + Aa ;
            end ; 
            Exit ;
        end ;
        if( A = $76 ) then { HALT }
        begin
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'HALT' ;
            end else 
            begin
                _Disassemble := 'HLT' ;
            end ;
            Exit ;
        end ;
        if( A = $Fb ) then { EI }
        begin
            _Disassemble := 'EI' ;
            Exit ;
        end ;
        if( A = $F3 ) then { DI }
        begin
            _Disassemble := 'DI' ;
            Exit ;
        end ;
        if( ( A and $C7 ) = $C7 ) then { RST }
        begin
            Aa := Cvis( ( A div 8 ) and 7, 2 ) ;
            _Disassemble := 'RST  ' + Aa ;
            Exit ;
        end ;
        if( A = $3F ) then { CCF or CMC }
        begin
            if( Mode = Z80 ) then _Disassemble := 'CCF' else _Disassemble := 'CMC' ;
            Exit ;
        end ;
        if( A = $37 ) then { SCF or STC }
        begin
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'SCF' ;
            end else 
            begin
                _Disassemble := 'STC' ;
            end ;
            Exit ;
        end ;
        if( ( A and $C7 ) = 4 ) then { INC or INR }
        begin
            Aa := Er( A, 8 ) ;
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'INC  ' + Aa ;
            end else 
            begin
                _Disassemble := 'INR  ' + Aa ;
            end ;
            Exit ;
        end ;
        if( ( A and $C7 ) = 5 ) then { DEC or DCR }
        begin
            Aa := Er( A, 8 ) ;
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'DEC  ' + Aa ;
            end else 
            begin
                _Disassemble := 'DCR  ' + Aa ;
            end ;
            Exit ;
        end ;
        if( A = $2F ) then { CPL or CMA }
        begin
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'CPL' ;
            end else 
            begin
                _Disassemble := 'CMA' ;
            end ;   
            Exit ;
        end ;
        if( A = $27 ) then { DAA }
        begin
            _Disassemble := 'DAA' ;
            Exit ;
        end ;
        if( ( A and $CF ) = $C5 ) then { PUSH }
        begin
            Aa := Eqq( A, 16 ) ;
            if( Mode = Z80 ) then _Disassemble := 'PUSH ' + Aa else
                _Disassemble := 'PUSH ' + Aa ;
            Exit ;
        end ;
        if( ( A and $CF ) = $C1 ) then {POP}
        begin
            Aa := Eqq( A, 16 ) ;
            if( Mode = Z80 ) then _Disassemble := 'POP  ' + Aa else
                _Disassemble := 'POP  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $Cf ) = 9 ) then {DAD or ADD HL,ss}
        begin
            Aa := Er2( A, 16 ) ;
            if( Mode = Z80 ) then _Disassemble := 'ADD  HL,' + Aa else
                _Disassemble := 'DAD  ' + Aa ;
            Exit ;
        end ;
        if ( A and $Cf ) = 3 then {INX or INC ss}
        begin
            Aa := Er2( A, 16 ) ;
            if( Mode = Z80 ) then _Disassemble := 'INC  ' + Aa else
                _Disassemble := 'INX  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $Cf ) = $B ) then { DCX or DEC ss }
        begin
            Aa := Er2( A, 16 ) ;
            if( Mode = Z80 ) then _Disassemble := 'DEC  ' + Aa else _Disassemble := 'DCX  ' + Aa ;
            Exit ;
        end ;
        if( A = $Eb ) then { XCHG or EX DE,HL }
        begin
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'EX   DE,HL' ;
            end else 
            begin
                _Disassemble := 'XCHG' ;
            end ;
            Exit ;
        end ;
        if( A = $E3 ) then { XTHL or EX (SP),HL }
        begin
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'EX   (SP),HL' ;
            end else 
            begin
                _Disassemble := 'XTHL' ;
            end ;
            Exit ;
        end ;
        if( A = $F9 ) then { SPHL or LD SP,HL }
        begin
            if( Mode = Z80 ) then _Disassemble := 'LD   SP,HL' else _Disassemble := 'SPHL' ;
            Exit ;
        end ;
        if( A = 7 ) then { RLC or RLCA }
        begin
            if( Mode = Z80 ) then _Disassemble := 'RLCA' else _Disassemble := 'RLC' ;
            Exit ;
        end ;
        if( A = $F ) then { RRC or RRCA }
        begin
            if( Mode = Z80 ) then _Disassemble := 'RRCA' else _Disassemble := 'RRC' ;
            Exit ;
        end ;
        if( A = $17 ) then { RAL or RLA }
        begin
            if( Mode = Z80 ) then _Disassemble := 'RLA' else _Disassemble := 'RAL' ;
            Exit ;
        end ;
        if( A = $1F ) then { RAR or RRA }
        begin
            if( Mode = Z80 ) then _Disassemble := 'RRA' else _Disassemble := 'RAR' ;
            Exit ;
        end ;
        if( ( A and $C0 ) = $40 ) then { MOV ddd,sss or LD r,r' }
        begin
            Aa := Er( A, 8 ) + ',' + Er( A, 1 ) ;
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'LD   ' + Aa ;
            end else
            begin
                _Disassemble := 'MOV  ' + Aa ;
            end ;
            Exit ;
        end ;
        if( ( A and $EF ) = 2 ) then { STAX or LD (BC),A/LD (DE),A }
        begin
            Aa := Er2( A, 16 ) ;
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'LD   (' + Aa + '),A' ;
            end else
            begin
                _Disassemble := 'STAX ' + Aa ;
            end ; 
            Exit ;
        end ;
        if( ( A and $EF ) = $A ) then { LDAX or LD A,(BC)/LD A,(DE) }
        begin
            Aa := Er2( A, 16 ) ;
            if( Mode = Z80 ) then
            begin
                _Disassemble := 'LD   A,(' + Aa + ')' ;
            end else
            begin
                _Disassemble := 'LDAX ' + Aa ;
            end ;
            Exit ;
        end ;
        if( ( A and $F8 ) = $80 ) then {ADD r or ADD A,r}
        begin
            Aa := Er( A, 1 ) ;
            if( Mode = Z80 ) then _Disassemble := 'ADD  A,' + Aa else
                _Disassemble := 'ADD  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $F8 ) = $88 ) then {ADC r or ADC A,r}
        begin
            Aa := Er( A, 1 ) ;
            if( Mode = Z80 ) then _Disassemble := 'ADC  A,' + Aa else
                _Disassemble := 'ADC  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $F8 ) = $90 ) then {SUB r}
        begin
            Aa := Er( A, 1 ) ;
            _Disassemble := 'SUB  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $F8 ) = $98 ) then {SBB r or SBC A,r}
        begin
            Aa := Er( A, 1 ) ;
            if( Mode = Z80 ) then _Disassemble := 'SBC  A,' + Aa else
                _Disassemble := 'SBB  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $F8 ) = $A0 ) then {ANA r or AND r}
        begin
            Aa := Er( A, 1 ) ;
            if( Mode = Z80 ) then _Disassemble := 'AND  ' + Aa else _Disassemble := 'ANA  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $F8 ) = $A8 ) then {XRA r or XOR r}
        begin
            Aa := Er( A, 1 ) ;
            if( Mode = Z80 ) then _Disassemble := 'XOR  ' + Aa else _Disassemble := 'XRA  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $F8 ) = $B0 ) then {ORA r or OR r}
        begin
            Aa := Er( A, 1 ) ;
            if( Mode = Z80 ) then _Disassemble := 'OR   ' + Aa else _Disassemble := 'ORA  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $F8 ) = $B8 ) then {CMP r or CP r}
        begin
            Aa := Er( A, 1 ) ;
            if( Mode = Z80 ) then _Disassemble := 'CP   ' + Aa else _Disassemble := 'CMP  ' + Aa ;
            Exit ;
        end ;
        if( A = $32 ) then {STA n or LD (n),A}
        begin
            Aa := Cvis( trunc( Cvtif( FetchWord ) ), 4 ) ; {Get address}
            if( Mode = Z80 ) then _Disassemble := 'LD   (' + Aa + '),A' else
                _Disassemble := 'STA  ' + Aa ;
            Exit ;
        end ;
        if( A = $3A ) then {LDA n or LD A,(n)}
        begin
            Aa := Cvis( trunc( Cvtif( FetchWord ) ), 4 ) ; {Get address}
            if( Mode = Z80 ) then _Disassemble := 'LD   A,(' + Aa + ')' else
                _Disassemble := 'LDA  ' + Aa ;
            Exit ;
        end ;
        if( A = $22 ) then {SHLD n or LD (n),HL}
        begin
            Aa := Cvis( trunc( Cvtif( FetchWord ) ), 4 ) ; {Get address}
            if( Mode = Z80 ) then _Disassemble := 'LD   (' + Aa + '),HL' else
                _Disassemble := 'SHLD ' + Aa ;
            Exit ;
        end ;
        if( A = $2A ) then {LHLD n or LD HL,(n)}
        begin
            Aa := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ; {Get address}
            if( Mode = Z80 ) then _Disassemble := 'LD   HL,(' + Aa + ')' else
                _Disassemble := 'LHLD ' + Aa ;
            Exit ;
        end ;
        if( ( A and $Cf ) = 1 ) then {LXI rp or LD rp,n}
        begin
            Aa := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ; {Get address}
            Bb := Er2( A, 16 ) ;
            if( Mode = Z80 ) then _Disassemble := 'LD   ' + Bb + ',' + Aa else
                _Disassemble := 'LXI  ' + Bb + ',' + Aa ;
            Exit ;
        end ;
        if( ( A and $C7 ) = 6 ) then {MVI r,n or LD r,n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            Bb := Er( A, 8 ) ;
            if( Mode = Z80 ) then _Disassemble := 'LD   ' + Bb + ',' + Aa else
                _Disassemble := 'MVI  ' + Bb + ',' + Aa ;
            Exit
        end ;
        if( A = $C6 ) then {ADI n or ADD A,n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            if( Mode = Z80 ) then _Disassemble := 'ADD  A,' + Aa else
                _Disassemble := 'ADI  ' + Aa ;
            Exit ;
        end ;
        if( A = $CE ) then {ACI n or ADC A,n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            if( Mode = Z80 ) then _Disassemble := 'ADC  A,' + Aa else
                _Disassemble := 'ACI  ' + Aa ;
            Exit ;
        end ;
        if( A = $D6 ) then {SUI n or SUB n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            if( Mode = Z80 ) then _Disassemble := 'SUB  ' + Aa else _Disassemble := 'SUI  ' + Aa ;
            Exit ;
        end ;
        if( A = $DE ) then {SBI n or SBC A,n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            if( Mode = Z80 ) then _Disassemble := 'SBC  A,' + Aa else
                _Disassemble := 'SBI  ' + Aa ;
            Exit ;
        end ;
        if( A = $E6 ) then {ANI n or AND n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            if( Mode = Z80 ) then _Disassemble := 'AND  ' + Aa else _Disassemble := 'ANI  ' + Aa ;
            Exit ;
        end ;
        if( A = $Ee ) then {XRI n or XOR n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            if( Mode = Z80 ) then _Disassemble := 'XOR  ' + Aa else _Disassemble := 'XRI  ' + Aa ;
            Exit ;
        end ;
        if( A = $F6 ) then {ORI n or OR n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            if( Mode = Z80 ) then _Disassemble := 'OR   ' + Aa else _Disassemble := 'ORI  ' + Aa ;
            Exit ;
        end ;
        if( A = $FE ) then {CPI n or CP n}
        begin
            Aa := Cvis( Fetch, 2 ) ;
            if( Mode = Z80 ) then _Disassemble := 'CP   ' + Aa else _Disassemble := 'CPI  ' + Aa ;
            Exit ;
        end ;
        if( A = $E9 ) then {PCHL or JP (HL)}
        begin
            if( Mode = Z80 ) then _Disassemble := 'JP   (HL)' else _Disassemble := 'PCHL' ;
            Exit ;
        end ;
        if( A = $C3 ) then { JMP n or JP n }
        begin
            Aa := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
            if( Mode = Z80 ) then _Disassemble := 'JP   ' + Aa else _Disassemble := 'JMP  ' + Aa ;
            Exit ;
        end ;
        if( ( A and $C7 ) = $C2 ) then {Jcc n or JP cc,n}
        begin
            Bb := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
            A := ( A div 8 ) and 7 ;
            case A of
                 0 : Aa := 'NZ' ;
                 1 : Aa := 'Z' ;
                 2 : Aa := 'NC' ;
                 3 : Aa := 'C' ;
                 4 : Aa := 'PO' ;
                 5 : Aa := 'PE' ;
                 6 : Aa := 'P' ;
                 7 : Aa := 'M'
            end ;
            if( Mode = Z80 ) then _Disassemble := 'JP   ' + Aa + ',' + Bb else
               begin
                   if( Length( Aa ) = 1 ) then Aa := Aa + ' ' ;
                   Aa := 'J' + Aa ;
                   _Disassemble := Aa + '  ' + Bb
               end ;
            Exit;
        end ;
        if( A = $Cd ) then { CALL n }
        begin
            Aa := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
            _Disassemble := 'CALL ' + Aa ;
            Exit;
        end ;
        if( ( A and $C7 ) = $C4 ) then {Ccc n or CALL cc,n}
        begin
            Bb := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
            A := ( A div 8 ) and 7 ;
            case A of
                 0 : Aa := 'NZ' ;
                 1 : Aa := 'Z' ;
                 2 : Aa := 'NC' ;
                 3 : Aa := 'C' ;
                 4 : Aa := 'PO' ;
                 5 : Aa := 'PE' ;
                 6 : Aa := 'P' ;
                 7 : Aa := 'M'
            end ;
            if( Mode = Z80 ) then _Disassemble := 'CALL ' + Aa + ',' + Bb else
            begin
                if Length( Aa ) = 1 then Aa := Aa + ' ' ;
                Aa := 'C' + Aa ;
                _Disassemble := Aa + '  ' + Bb
            end ;
            Exit ;
        end ;
        if( A = $C9 ) then {RET}
        begin
            _Disassemble := 'RET' ;
            Exit
        end ;
        if ( A and $C7 ) = $C0 then {Rc or RET c}
        begin
            A := ( A div 8 ) and 7 ;
            case A of
                 0 : Aa := 'NZ' ;
                 1 : Aa := 'Z' ;
                 2 : Aa := 'NC' ;
                 3 : Aa := 'C' ;
                 4 : Aa := 'PO' ;
                 5 : Aa := 'PE' ;
                 6 : Aa := 'P' ;
                 7 : Aa := 'M'
            end ;
            if( Mode = Z80 ) then _Disassemble := 'RET  ' + Aa else _Disassemble := 'R' + Aa ;
            Exit ;
        end ;
        if( Mode = M8080 ) then goto Ee ; {Not an 8080 op-code}
        if( Mode = M8085 ) then
        begin
            if( A = $20 ) then {RIM}
            begin
                _Disassemble := 'RIM' ;
                Exit ;
            end ;
            if( A = $30 ) then {SIM}
            begin
                _Disassemble := 'SIM' ;
                Exit ;
            end ;
            goto Ee ; {Not an 8085 op-code}
        end ;
        if( A = 8 ) then {EX AF,AF'}
        begin
            _Disassemble := 'EX   AF,AF''' ;
            Exit ;
        end ;
        if( A = $D9 ) then {EXX}
        begin
            _Disassemble := 'EXX' ;
            Exit ;
        end ;
        if( A = $18 ) then {JR n}
        begin
            Temp := Fetch + 2 ;
            if( ( Base = 10 ) and ( Temp > 127 ) ) then
            begin
                Temp := $FFFFFF00 or Temp ;
            end ;
            Bb := Cvis( Temp, 1 ) ; {Get offset}
            _Disassemble := 'JR   ' + Bb ;
            Exit ;
        end ;
        if( ( A and $E7 ) = $20 ) then {JR}
        begin
            Temp := Fetch + 2 ;
            if( ( Base = 10 ) and ( Temp > 127 ) ) then
            begin
                Temp := $FFFFFF00 or Temp ;
            end ;
            Bb := Cvis( Temp, 1 ) ; {Get offset}
            A := ( A div 8 ) and 3 ;
            case A of
                 0 : Aa := 'NZ' ;
                 1 : Aa := 'Z' ;
                 2 : Aa := 'NC' ;
                 3 : Aa := 'C'
            end ;
            _Disassemble := 'JR   ' + Aa + ',' + Bb ;
            Exit ;
        end ;
        if( A = $10 ) then {DJNZ n}
        begin
            Bb := Cvis( Fetch + 2, 2 ) ;
            _Disassemble := 'DJNZ ' + Bb ;
            Exit ;
        end ;
        if( A = $ED ) then {Extension $ED}
        begin
            A := Fetch ; {Get op-code}
            if( _Profiling ) then
            begin
                TZ80_Profiler( Parent.Profiler ).Increment( Domain_Instructions, $100 or A ) ;
            end ;
            if( ( A and $C7 ) = $40 ) then
            begin
                Aa := Er( A, 8 ) ;
                _Disassemble := 'IN   ' + Aa + ',(C)' ;
                Exit ;
            end ;
            if( A = $Ba ) then
            begin
                _Disassemble := 'INDR' ;
                Exit
            end ;
            if A = $A2 then
            begin
                _Disassemble := 'INI' ;
                Exit
            end ;
            if A = $B2 then
            begin
                _Disassemble := 'INIR' ;
                Exit
            end ;
            if A = $BB then
            begin
                _Disassemble := 'OTDR' ;
                Exit
            end ;
            if A = $B3 then
            begin
                _Disassemble := 'OTIR' ;
                Exit
            end ;
            if A = $A3 then
            begin
                _Disassemble := 'OUTI' ;
                Exit
            end ;
            if A = $AA then
            begin
                _Disassemble := 'IND' ;
                Exit
            end ;
            if( A = $A3 ) then
            begin
                _Disassemble := 'OUTI' ;
                Exit ;
            end ;
            if( A = $AB ) then
            begin
                _Disassemble := 'OUTD' ;
                Exit ;
            end ;
            if( ( A and $C7 ) = $41 ) then
            begin
                Aa := Er( A, 8 ) ;
                _Disassemble := 'OUT  (C),' + Aa ;
                Exit ;
            end ;
            if( A = $46 ) then {IM 0}
            begin
                _Disassemble := 'IM0' ;
                Exit ;
            end ;
            if( A = $56 ) then {IM 1}
            begin
                _Disassemble := 'IM1' ;
                Exit ;
            end ;
            if( A = $5E ) then {IM 2}
            begin
                _Disassemble := 'IM2' ;
                Exit ;
            end ;
            if( A = $57 ) then {LD A,I}
            begin
                _Disassemble := 'LD   A,I' ;
                Exit ;
            end ;
            if( A = $5F ) then {LD A,R}
            begin
                _Disassemble := 'LD   A,R' ;
                Exit ;
            end ;
            if( A = $47 ) then {LD I,A}
            begin
                _Disassemble := 'LD   I,A' ;
                Exit ;
            end ;
            if( ( A and $CF ) = $4F ) then {LD R,A}
            begin
                _Disassemble := 'LD   R,A' ;
                Exit ;
            end ;
            if( ( A and $CF ) = $4B ) then {LD dd,(nn)}
            begin
                Bb := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
                Aa := Er( A, 16 ) ;
                _Disassemble := 'LD   ' + Aa + ',(' + Bb + ')' ;
                Exit;
            end ;
            if( A = $43 ) then {LD (nn),dd}
            begin
                Bb := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
                Aa := Er( A, 16 ) ;
                _Disassemble := 'LD   (' + Bb + '),' + Aa ;
                Exit ;
            end ;
            if( A = $44 ) then {NEG}
            begin
                _Disassemble := 'NEG' ;
                Exit ;
            end ;
            if( A = $4D ) then {RETI}
            begin
                _Disassemble := 'RETI' ;
                Exit ;
            end ;
            if( A = $45 ) then {RETN}
            begin
                _Disassemble := 'RETN' ;
                Exit ;
            end ;
            if( A = $6F ) then {RLD}
            begin
                _Disassemble := 'RLD' ;
                Exit ;
            end ;
            if( A = $67 ) then {RRD}
            begin
                _Disassemble := 'RRD' ;
                Exit ;
            end ;
            if( ( A and $CF ) = $4A ) then {ADC HL,ss}
            begin
                Aa := Er2( A, 16 ) ;
                _Disassemble := 'ADC  HL,' + Aa ;
                Exit ;
            end ;
            if( ( A and $CF ) = $42 ) then {SBC HL,ss}
            begin
                Aa := Er2( A, 16 ) ;
                _Disassemble := 'SBC  HL,' + Aa ;
                Exit ;
            end ;
            if( A = $A9 ) then {CPD}
            begin
                _Disassemble := 'CPD' ;
                Exit ;
            end ;
            if( A = $B9 ) then {CPDR}
            begin
                _Disassemble := 'CPDR' ;
                Exit ;
            end ;
            if( A = $A1 ) then {CPI}
            begin
                _Disassemble := 'CPI' ;
                Exit ;
            end ;
            if( A = $B1 ) then {CPIR}
            begin
                _Disassemble := 'CPIR' ;
                Exit ;
            end ;
            if( A = $A8 ) then {LDD}
            begin
                _Disassemble := 'LDD' ;
                Exit ;
            end ;
            if( A = $B8 ) then {LDDR}
            begin
                _Disassemble := 'LDDR' ;
                Exit ;
            end ;
            if( A = $A0 ) then {LDI}
            begin
                _Disassemble := 'LDI' ;
                Exit ;
            end ;
            if( A = $B0 ) then {LDIR}
            begin
                _Disassemble := 'LDIR' ;
                Exit ;
            end ;
            goto Ee ;
        end ;
        if( A = $CB ) then {Extension $CB}
        begin
            A := Fetch ; {Get op-code}
            if( _Profiling ) then
            begin
                TZ80_Profiler( Parent.Profiler ).Increment( Domain_Instructions, $200 or A ) ;
            end ;
            if ( A and $C0 ) = $40 then {BIT b,r}
            begin
                Bb := Cvis( ( A div 8 ) and 7, 2 ) ; {bit}
                Aa := Er( A, 1 ) ;
                _Disassemble := 'BIT  ' + Bb + ',' + Aa ;
                Exit ;
            end ;
            if( ( A and $C0 ) = $80 ) then {RES b,r}
            begin
                Bb := Cvis( ( A div 8 ) and 7, 2 ) ; {bit}
                Aa := Er( A, 1 ) ;
                _Disassemble := 'RES  ' + Bb + ',' + Aa ;
                Exit ;
            end ;
            if( ( A and $F8 ) = $10 ) then {RL r}
            begin
                _Disassemble := 'RL   ' + Er( A, 1 ) ;
                Exit ;
            end ;
            if( ( A and $F8 ) = 0 ) then {RLC r}
            begin
                _Disassemble := 'RLC  ' + Er( A, 1 ) ;
                Exit ;
            end ;
            if( ( A and $F8 ) = $18 ) then {RR r}
            begin
                _Disassemble := 'RR   ' + Er( A, 1 ) ;
                Exit ;
            end ;
            if( ( A and $F8 ) = $08 ) then {RRC r}
            begin
                _Disassemble := 'RRC  ' + Er( A, 1 ) ;
                Exit ;
            end ;
            if( ( A and $C0 ) = $C0 ) then {SET b,r}
            begin
                Bb := Cvis( ( A div 8 ) and 7, 2 ) ; {bit}
                Aa := Er( A, 1 ) ;
                _Disassemble := 'SET  ' + Bb + ',' + Aa ;
                Exit ;
            end ;
            if( ( A and $F8 ) = $20 ) then {SLA r}
            begin
                _Disassemble := 'SLA  ' + Er( A, 1 ) ;
                Exit ;
            end ;
            if( ( A and $F8 ) = $28 ) then {SRA r}
            begin
                _Disassemble := 'SRA  ' + Er( A, 1 ) ;
                Exit ;
            end ;
            if( ( A and $F8 ) = $38 ) then {SRL r}
            begin
                _Disassemble := 'SRL  ' + Er( A, 1 ) ;
                Exit ;
            end ;
            goto Ee ;
        end ;
        if( ( A = $DD ) or ( A = $FD ) ) then {Extensions $DD and $FD (IX and IY)}
        begin
            if( A = $DD ) then
            begin
                Aa := 'IX' ;
                if( _Profiling ) then
                begin
                    TZ80_Profiler( Parent.Profiler ).Increment( Domain_Instructions, $300 or A ) ;
                end ;
            end else
            begin
                Aa := 'IY' ;
                if( _Profiling ) then
                begin
                    TZ80_Profiler( Parent.Profiler ).Increment( Domain_Instructions, $400 or A ) ;
                end ;
            end ;
            A := Fetch ;
            if( A = $8E ) then {ADC A,(IZ+d)}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                _Disassemble := 'ADC  A,(' + Aa + '+' + Bb + ')' ;
                Exit ;
            end ;
            if( A = $86 ) then {ADD A,(IZ+d)}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                _Disassemble := 'ADD  A,(' + Aa + '+' + Bb + ')' ;
                Exit ;
            end ;
            if( A = 9 ) then {ADD IZ,rr}
            begin
                A := ( ( A div 16 ) and 3 ) + 1 ;
                case A of
                     1..2 : Bb := Copy( 'BCDE', A * 2 - 1, 2 ) ;
                        3 : Bb := Aa ;
                        4 : Bb := 'SP'
                end ;
                _Disassemble := 'ADD  ' + Aa + ',' + Bb ;
                Exit ;
            end ;
            if( A = $A6 ) then {AND (IZ+d)}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                _Disassemble := 'AND  (' + Aa + '+' + Bb + ')' ;
                Exit ;
            end ;
            if( A = $BE ) then {CP (IZ+d)}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                _Disassemble := 'CP   (' + Aa + '+' + Bb + ')' ;
                Exit
            end ;
            if A = $35 then {DEC (IZ+d)}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                _Disassemble := 'DEC  (' + Aa + '+' + Bb + ')' ;
                Exit
            end ;
            if A = $2B then {DEC IZ}
            begin
                _Disassemble := 'DEC  ' + Aa ;
                Exit
            end ;
            if A = $E3 then {EX (SP),IZ}
            begin
                _Disassemble := 'EX   (SP),' + Aa ;
                Exit
            end ;
            if A = $34 then {INC (IZ+d)}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                _Disassemble := 'INC  (' + Aa + '+' + Bb + ')' ;
                Exit
            end ;
            if A = $23 then {INC IZ}
            begin
                _Disassemble := 'INC  ' + Aa ;
                Exit
            end ;
            if A = $E9 then {JP (IZ)}
            begin
                _Disassemble := 'JP   (' + Aa + ')' ;
                Exit
            end ;
            if( A = $2A ) then {LD IZ,(n)}
            begin
                Bb := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
                _Disassemble := 'LD   ' + Aa + ',(' + Bb + ')' ;
                Exit ;
            end ;
            if( A = $21 ) then {LD IZ,n}
            begin
                Bb := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
                _Disassemble := 'LD   ' + Aa + ',' + Bb ;
                Exit ;
            end ;
            if( A = $36 ) then {LD (IZ+d),n}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                Cc := Cvis( Fetch, 2 ) ; {Get data byte}
                _Disassemble := 'LD   (' + Aa + '+' + Bb + '),' + Cc ;
                Exit ;
            end ;
            if( A = $70 ) then {LD (IZ+d),r}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                Cc := Er( A, 1 ) ; {Get data byte}
                _Disassemble := 'LD   (' + Aa + '+' + Bb + '),' + Cc ;
                Exit ;
            end ;
            if( A = $22 ) then {LD (n),IZ}
            begin
                Bb := Cvis( trunc( CvtIF( FetchWord ) ), 4 ) ;
                _Disassemble := 'LD   (' + Bb + '),' + Aa ;
                Exit ;
            end ;
            if( A = $46 ) then {LD r,(IZ+d)}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                Cc := Er( A, 8 ) ;
                _Disassemble := 'LD   ' + Cc + ',(' + Aa + '+' + Bb + ')' ;
                Exit ;
            end ;
            if( A = $F9 ) then {LD SP,IZ}
            begin
                _Disassemble := 'LD   SP,' + Aa ;
                Exit ;
            end ;
            if( A = $B6 ) then {OR (IZ+d)}
            begin
                Bb := Cvis( Fetch, 2 ) ;
                _Disassemble := 'OR   (' + Aa + '+' + Bb + ')' ;
                Exit ;
            end ;
            if( A = $E1 ) then {POP IZ}
            begin
                _Disassemble := 'POP  ' + Aa ;
                Exit ;
            end ;
            if( A = $E5 ) then {PUSH IZ}
            begin
                _Disassemble := 'PUSH ' + Aa ;
                Exit ;
            end ;
            if( A = $Cb ) then {Combination extensions $DD $CB and $FD $CB}
            begin
                if( Aa = 'IX' ) then
                begin
                    if( _Profiling ) then
                    begin
                        TZ80_Profiler( Parent.Profiler ).Increment( Domain_Instructions, $500 or A ) ;
                    end ;
                end else
                begin
                    if( _Profiling ) then
                    begin
                        TZ80_Profiler( Parent.Profiler ).Increment( Domain_Instructions, $600 or A ) ;
                    end ;
                end ;
                Aa := '(' + Aa + '+' + Cvis( Fetch, 2 ) + ')' ; {Get (IZ+d)}
                A := Fetch ;
                if( ( A and $C7 ) = $46 ) then {BIT b,(IZ+d)}
                begin
                    Bb := Cvis( ( A div 8 ) and 7, 2 ) ; {bit}
                    _Disassemble := 'BIT  ' + Bb + ',' + Aa ;
                    Exit ;
                end ;
                if( ( A and $C7 ) = $86 ) then {RES b,(IZ+d)}
                begin
                    Bb := Cvis( ( A div 8 ) and 7, 2 ) ; {bit}
                    _Disassemble := 'RES  ' + Bb + ',' + Aa ;
                    Exit ;
                end ;
                if( A = $16 ) then {RL (IZ+d)}
                begin
                    _Disassemble := 'RL   (' + Aa ;
                    Exit ;
                end ;
                if( A = $06 ) then {RLC (IZ+d)}
                begin
                    _Disassemble := 'RLC  ' + Aa ;
                    Exit ;
                end ;
                if( A = $1E ) then {RR (IZ+d)}
                begin
                    _Disassemble := 'RR   ' + Aa ;
                    Exit ;
                end ;
                if( A = $0E ) then {RRC (IZ+d)}
                begin
                    _Disassemble := 'RRC  ' + Aa ;
                    Exit ;
                end ;
                if( ( A and $C7 ) = $C6 ) then {SET b,(IZ+d)}
                begin
                    Bb := Cvis( ( A div 8 ) and 7, 2 ) ; {bit}
                    _Disassemble := 'SET  ' + Bb + ',(' + Aa + ')' ;
                    Exit ;
                end ;
                if( A = $26 ) then {SLA (IZ+d)}
                begin
                    _Disassemble := 'SLA  ' + Aa ;
                    Exit ;
                end ;
                if( A = $2E ) then {SRA (IZ+d)}
                begin
                    _Disassemble := 'SRA  ' + Aa ;
                    Exit ;
                end ;
                if( A = $3E ) then {SRL (IZ+d)}
                begin
                    _Disassemble := 'SRL  ' + Aa ;
                    Exit ;
                end ;
                goto Ee ;
            end ;
            if( A = $9E ) then {SBC A,(IZ+d)}
            begin
                _Disassemble := 'SBC  A,' + Aa ;
                Exit ;
            end ;
            if( A = $96 ) then {SUB (IZ+d)}
            begin
                _Disassemble := 'SUB  ' + Aa ;
                Exit ;
            end ;
            if( A = $Ae ) then {XOR (IZ+d)}
            begin
                _Disassemble := 'XOR  ' + Aa ;
                Exit ;
            end ;
        end ;
Ee:
        _Disassemble := 'DB ' + Cvis( A, 2 ) ;
    end ; { TZ80_CPU.Disassemble._Disassemble }

var S : string ;

begin
    Result := Set_Error( 0 ) ;
    if( Base = 0 ) then
    begin
        Base := Default_Base ;
    end ;
    while( Size > 0 ) do
    begin
        if( length( S ) > 0 ) then
        begin
            S := S + CR ;
        end ;
        S := S + _Disassemble ;
    end ;
    Stream.Write( PChar( S )[ 0 ], length( S ) ) ;
end ; { TZ80_CPU.Disassemble }


function TZ80_CPU.Get_Clock_Speed : longint ;

begin
    Result := _Speed ;
end ;


procedure TZ80_CPU.Halt ;

begin
    _Halted := True ;
end ;


function TZ80_CPU.Halted : boolean ;

begin
    Result := _Halted ;
end ;


procedure TZ80_CPU.Run_From_Stream( Stream : TCOM_Stream ) ;

var Saved_PC : integer ;

begin
    Saved_PC := _PC ;
    _Stream_PC_Offset := _PC ;
    _Run_Stream := Stream ;
    Execute( False, False ) ;
    _PC := Saved_PC ;
    _Run_Stream := nil ;
end ;


procedure TZ80_CPU.Run ;

begin
    _Halted := False ;
    Execute( False, False ) ;
end ;


procedure TZ80_CPU.Execute( Single_Step, Into : boolean ) ;

var Flags_Changed : boolean ; // Set to true if any flags change value
    Flags_Read : boolean ; // True if any flags are examined

    procedure _Set_Flag( Value : integer ) ;

    var Old : integer ;

    begin
        Old := _Register[ Register_Set0, AF ] and $FF00 ;
        _Register[ Register_Set0, AF ] :=
            _Register[ Register_Set0, AF ] or Value ;
        if( Old <> _Register[ Register_Set0, AF ] and $FF00 ) then
        begin
            Flags_Changed := True ;
        end ;
    end ;


    procedure _Reset_Flag( Value : integer ) ;

    var Old : integer ;

    begin
        Old := _Register[ Register_Set0, AF ] and $FF00 ;
        _Register[ Register_Set0, AF ] :=
            _Register[ Register_Set0, AF ] and ( not Value ) ;
        if( Old <> _Register[ Register_Set0, AF ] and $FF00 ) then
        begin
            Flags_Changed := True ;
        end ;
    end ;


    function _Is_Flag_Set( Value : integer ) : boolean ;

    begin
        Result := ( ( Register[ Register_Set0, AF ] and Value ) <> 0 ) ;
        Flags_Read := True ;
    end ;


    procedure Set_C ; {Set carry flag}

    begin
        _Set_Flag( 256 ) ;
    end ;


    procedure Reset_C ;

    begin
        _Reset_Flag( 256 ) ;
    end ;


    function Is_C_Set : boolean ;

    begin
       Result :=_Is_Flag_Set( 256 ) ;
    end ;


    procedure Set_C2 ; { Set auxillary carry flag (H) }

    begin
        _Set_Flag( 4096 ) ;
    end ;


    procedure Reset_C2 ;

    begin
        _Reset_Flag( 4096 ) ;
    end ;


    function Is_C2_Set : boolean ;

    begin
       Result :=_Is_Flag_Set( 4096 ) ;
    end ;


    procedure Set_D ; { Set double carry flag (8085) }

    begin
        _Set_Flag( 8192 ) ;
    end ;


    procedure Reset_D ;

    begin
        _Reset_Flag( 8192 ) ;
    end ;


    function Is_D_Set : boolean ;

    begin
       Result :=_Is_Flag_Set( 8192 ) ;
    end ;


    procedure Set_N ; {Set subtraction flag}

    begin
        _Set_Flag( 512 ) ;
    end ;


    procedure Reset_N ;

    begin
        _Reset_Flag( 512 ) ;
    end ;


    function Is_N_Set : boolean ;

    begin
       Result :=_Is_Flag_Set( 512 ) ;
    end ;


    procedure Set_P ;

    begin
        _Set_Flag( 1024 ) ;
    end ;


    procedure Reset_P ;

    begin
        _Reset_Flag( 1024 ) ;
    end ;


    function Is_P_Set : boolean ;

    begin
        Result :=_Is_Flag_Set( 1024 ) ;
    end ;


    procedure Set_Z ; { Set zero flag }

    begin
        _Set_Flag( 16384 ) ;
    end ;


    procedure Reset_Z ;

    begin
        _Reset_Flag( 16384 ) ;
    end ;


    function Is_Z_Set : boolean ;

    begin
       Result :=_Is_Flag_Set( 16384 ) ;
    end ;


    function Fetch : Integer ; { Fetch next byte }

    var Ch : char ;
        Size : longint ;

    begin
        if( Interrupt_Instruction ) then
        begin
            Ch := Bus_Read( 0, 2 ) ;
            Fetch := ord( Ch ) ;
        end else
        begin
            if( _Run_Stream <> nil ) then
            begin
                if( ( PC < _Stream_PC_Offset ) or ( _Run_Stream.At_End ) ) then
                begin
                    _Run_Stream := nil ; // Something made us jump out of the stream
                end else
                begin
                    Size := 1 ;
                    _Run_Stream.Read( Ch, Size ) ;
                    Fetch := ord( Ch ) ;
                    exit ;
                end ;
            end ;
            Fetch := ord( ByteRead( PC ) ) ;
            PC := PC + 1 ;
            R := R + 1 ;
        end ;
    end ;


    function Fetch_Word : integer ; { Fetch a 2-byte word }

    var X : integer ;

    begin
        X := Fetch ;
        X := X or swap( Fetch ) ;
        Fetch_Word := X ;
    end ;


    function Extract_Register( X, Y : integer ) : integer ;
    { Extract register from X ((X div Y) and 7) and return appropriate byte }

    var A, B : Integer ;

    begin
        A := ( ( X div Y ) and 7 ) + 2 ;
        B := A div 2 ;
        if( A = 9 ) then { A }
        begin
            Extract_Register := Lo( _Register[ Register_Set0, AF ] ) ;
            if( _Run_Stream = nil ) then
            begin
                if( ( _Register_Watchpoints[ 6 ] and Access_Read ) <> 0 ) then
                begin
                    Watchpoint_Notice( 6, Access_Read, 0, False, True, False ) ;
                end ;
            end ;
        end else
        begin
            if( A = 8 ) then { (HL) }
            begin
                Extract_Register := Byte_Read( Register[ Register_Set1, HL ] ) ;
            end else
            begin
                if( Odd( A ) ) then
                begin
                    Extract_Register := Lo_Register( Register_Set1, B ) ;
                    if( _Run_Stream = nil ) then
                    begin
                        if( ( _Register_Watchpoints[ 8 + ( B - 1 ) * 2 ] and Access_Read ) <> 0 ) then
                        begin
                            Watchpoint_Notice( 8 + ( B - 1 ) * 2, Access_Read, 0, False, True, False ) ;
                        end ;
                    end ;
                end else
                begin
                    Extract_Register := Hi_Register( Register_Set1, B ) ;
                    if( _Run_Stream = nil ) then
                    begin
                        if( ( _Register_Watchpoints[ 9 + ( B - 1 ) * 2 ] and Access_Read ) <> 0 ) then
                        begin
                            Watchpoint_Notice( 9 + ( B - 1 ) * 2, Access_Read, 0, False, True, False ) ;
                        end ;
                    end ;
                end ;
            end ;
        end ;
    end ; { Extract_Register }


    procedure Set_Register( X, Y, Z : Integer ) ;
    { Extract register from X ((X div Y) and 7) and set appropriate byte to z }

    var A, B : integer ;

    begin
        A := ( ( X div Y ) and 7 ) + 2 ;
        B := A div 2 ;
        Z := Z and 255 ;
        if A = 9 then Register[ Register_Set0, AF ] := Swap( Hi_Register( Register_Set0, AF ) ) or Z else
           if A = 8 then ByteWrite( Register[ Register_Set1, HL ], Z ) else
              if Odd( A ) then Register[ Register_Set1, B ] := Swap( Hi( _Register[ Register_Set1, B ] ) ) or Z else
                 Register[ Register_Set1, B ] := Swap( Z ) or Lo( _Register[ Register_Set1, B ] )
    end ;


    procedure Flags( X : Integer ) ; { Adjust P, S, and Z flags }

    var A, B, C : Integer ;

    begin
        _Register[ Register_Set0, AF ] := _Register[ Register_Set0, AF ] and $3FFF ;
        Flags_Changed := True ;
        { Clear S and Z Flags }
        X := X and 255 ;
        if( X = 0 ) then
        begin
            Set_Z ;
        end ;
        if( X > 127 ) then
        begin
            _Register[ Register_Set0, AF ] :=
                _Register[ Register_Set0, AF ] or Swap( 128 ) ; { Set S }
        end ;

        // Handle Parity flag...
        if Overflow then
        begin
            Exit ; { Don't modify P/V if overflow mode }
        end ;
        _Register[ Register_Set0, AF ] :=
            _Register[ Register_Set0, AF ] and $FBFF ; {Clear P/V flag}
        A := 0 ;
        B := 1 ;
        for C := 0 to 7 do
        begin
            if( ( X and B ) = B ) then
            begin
                A := A + 1 ;
            end ;
            B := B + B ;
        end ;
        if( ( A and 1 ) = 0 ) then
        begin
            Set_P ; // Even parity
        end ;
    end ; // .Flags


    procedure Add( A, B : Integer ) ; { Add A+B to accumulator }

    begin
        _Register[ Register_Set0, AF ] :=
            _Register[ Register_Set0, AF ] and ( not 4352 ) ;
        Flags_Changed := True ;
        if ( A and 15 ) + ( Lo( _Register[ Register_Set0, AF ] ) and 15 ) + B > 15 then Set_C2 ;
        if Overflow then if ( ( A and 127 ) + ( Lo( _Register[ Register_Set0, AF ] ) and 127 ) + B > 127 ) xor
            ( ( A and 255 ) + ( _Register[ Register_Set0, AF ] and 255 ) + B > 255 ) then
                Set_P else
                Reset_P ;
        A := A + Lo( _Register[ Register_Set0, AF ] ) + B ;
        if A > 255 then
        begin
            Set_C ;
            A := A and 255 ;
        end ;
        _Register[ Register_Set0, AF ] := Swap( Hi( _Register[ Register_Set0, AF ] ) ) or A ;
        Flags( A )
    end ; // Add


    procedure Sub( A, B : Integer ) ; { Subtract A+B from accumulator }

    begin
        A := -( A + B ) and 255 ;
        if( A = 0 ) then if ( B = 1 ) then
        begin
            A := 256 ; { Subtraction of 0 }
        end else
        begin
            _Register[ Register_Set0, AF ] :=
                _Register[ Register_Set0, AF ] and ( not 4352 ) ;
            Flags( Lo( _Register[ Register_Set0, AF ] ) ) ;
            exit ;
        end ;
        _Register[ Register_Set0, AF ] := _Register[ Register_Set0, AF ] or 4352 ;
        if ( A and 15 ) + ( Lo( _Register[ Register_Set0, AF ] ) and 15 ) > 15 then
            Reset_C2 ;
        if Overflow then if ( ( A and 127 ) + ( Lo( _Register[ Register_Set0, AF ] ) and 127 ) > 127 ) xor
            ( ( A and 255 ) + ( _Register[ Register_Set0, AF ] and 255 ) > 255 ) then
                Set_P else
                Reset_P ;
        A := A + Lo( _Register[ Register_Set0, AF ] ) ;
        if( A > 255 ) then
        begin
            Reset_C ;
            A := A and 255 ;
        end ;
        _Register[ Register_Set0, AF ] :=
            swap( Hi( _Register[ Register_Set0, AF ] ) ) or A ;
        Flags( A ) ;
    end ; // Sub


    procedure Logical_Flags ; {Reset C and N}

    begin
        Reset_C ;
        if Mode = Z80 then
        begin
            Reset_N ;
        end ;
    end ;


    procedure Ccmp( A : Integer ) ; {Compare A with accumulator}

    begin
        A := -A and 255 ;
        if A = 0 then {Compare with 0}
        begin
            Register[ Register_Set0, AF ] := _Register[ Register_Set0, AF ] and ( not 4352 ) ;
            Flags( Lo( _Register[ Register_Set0, AF ] ) ) ;
            Exit
        end ;
        _Register[ Register_Set0, AF ] := _Register[ Register_Set0, AF ] or 4352 ;
        if ( A and 15 ) + ( Lo( _Register[ Register_Set0, AF ] ) and 15 ) > 15 then
            Reset_C2 ;
        if Overflow then if ( ( A and 127 ) + ( Lo( _Register[ Register_Set0, AF ] ) and 127 ) > 127 ) xor
            ( ( A and 255 ) + ( _Register[ Register_Set0, AF ] and 255 ) > 255 ) then
                Set_P else
                Reset_P ;
        A := A + Lo( _Register[ Register_Set0, AF ] ) ;
        if A > 255 then
        begin
            Reset_C ;
            A := A and 255
        end ;
        Flags( A ) ;
    end ;


    function Execute_Z80( A : integer ) : boolean ;

        function Er2( X, Y : Integer ) : Integer ;
        {Extract register from X ((X DIV Y) AND 3) and return appropriate word}

        var A : Integer ;

        begin
            A := ( ( X div Y ) and 3 ) + 1 ;
            if A = 4 then Er2 := Sp else Er2 := Register[ Register_Set1, A ]
        end ;


        procedure Carry16( X, Y, Z : Real ) ; {Set carry if a carry out of 16 bits}

        begin
            if X < 0 then X := 65536.0 + X ;
            if Y < 0 then Y := 65536.0 + Y ;
            if Z < 0 then Z := 65536.0 + Z ;
            if( X + Y + Z > 65535.0 ) then Set_C ;
        end ;


        procedure Flags16( X : Integer ) ; {Adjust P, S, and Z flags}

        var A, B, C : Integer ;

        begin
            Flags_Changed := True ;
            _Register[ Register_Set0, AF ] := _Register[ Register_Set0, AF ] and $13Ff ; {Clear P, S, and Z}
            if( X = 0 ) then
            begin
                Set_Z ;
            end ;
            if( X < 0 ) then
            begin
                _Register[ Register_Set0, AF ] := _Register[ Register_Set0, AF ] or Swap( 128 ) ; {Set S}
            end ;
            A := 0 ;
            B := 1 ;
            for C := 0 to 16 do
            begin
                if ( X and B ) = B then
                begin
                    A := A + 1 ;
                end ;
                B := B + B
            end ;
            if ( A and 1 ) = 0 then
                Set_P ;
        end ;


        procedure Cmp( A : Integer ) ; {Compare A with accumulator, block style}

        begin
            A := A and 255 ;
            _Register[ Register_Set0, AF ] :=
                _Register[ Register_Set0, AF ] or 4096 or Swap( 128 ) ; { S and H }
            if ( A and 15 ) + ( Lo( _Register[ Register_Set0, AF ] ) and 15 ) > 15 then
                Reset_C2 ;
            Flags_Changed := True ;
        end ;


    var AA : string ;
        B, C, Index, Iz : integer ;
        Temp : Real ; {Dummy variable}

    label CPDR, CPIR, INDR, INIR, Unknown_OpCode, Lddr, Ldir ;

    begin
        Execute_Z80 := True ; { Assume success }
        if( A = 8 ) then { EX AF,AF' }
        begin
            Increment_Clock( 4 ) ;
            Register_Set0 := ( Register_Set0 + 1 ) and 1 ;
            Exit ;
        end ;
        if( A = $D9 ) then { EXX }
        begin
            Increment_Clock( 4 ) ;
            Register_Set1 := ( Register_Set1 + 1 ) and 1 ;
            Exit ;
        end ;
        if( A = $18 ) then { JR n }
        begin
            Increment_Clock( 12 ) ;
            B := Fetch ; { Get offset }
            if( B > 127 ) then
            begin
                B := B or Swap( 255 ) ; { Sign extend }
            end ;
            PC := PC + B ; {Jump}
            Exit ;
        end ;
        if( ( A and $E7 ) = $20 ) then { JR cc, n }
        begin
            Increment_Clock( 7 ) ;
            B := Fetch ; {Get offset}
            A := ( A div 8 ) and 3 ;
            C := _Register[ Register_Set0, AF ] ;
            Flags_Read := True ;
            case A of
                 0 : {JR NZ,n}
                     if ( C and 16384 ) = 16384 then Exit ;
                 1 : {JR Z,n}
                     if ( C and 16384 ) = 0 then Exit ;
                 2 : {JR NC,n}
                     if ( C and 256 ) = 256 then Exit ;
                 3 : {JR C,n}
                     if ( C and 256 ) = 0 then Exit
            end ;
            Increment_Clock( 5 ) ;
            if B > 127 then B := B or Swap( 255 ) ; {Sign extend}
            Pc := Pc + B ; {Jump}
            Exit ;
        end ;
        if( A = $10 ) then {DJNZ n}
        begin
            Increment_Clock( 8 ) ;
            C := Fetch ;
            if C > 127 then C := C or Swap( 255 ) ;
            B := Hi( _Register[ Register_Set1, BC ] ) - 1 ;
            Register[ Register_Set1, BC ] :=
                Swap( B ) or Lo_Register( Register_Set1, BC ) ;
            if( B <> 0 ) then
            begin
                PC := PC + C ;
                Increment_Clock( 5 ) ;
            end ;
            Exit ;
        end ;
        if( A = $ED ) then { Extension $ED }
        begin
            A := Fetch ; { Get op-code }
            if( ( A and $C7 ) = $41 ) then { OUT (C),r }
            begin
                Increment_Clock( 12 ) ;
                B := Extract_Register( A, 8 ) ;
                Output( Register_BC, B ) ;
                exit ;
            end ;
            if( ( A and $C7 ) = $40 ) then { IN r,(C) }
            begin
                Increment_Clock( 12 ) ;
                B := Extract_Register( A, 8 ) ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags_Changed := True ;
                Register[ Register_Set1, B ] := Input( Register_BC ) ;
                Flags( B ) ;
                exit ;
            end ;
            if( ( A = $B2 ) or ( A = $B3 ) ) then { INIR or OTIR }
            begin
                Set_N ;
                Set_Z ;
INIR:
                if( A = $B3 ) then { OTIR }
                begin
                    Output( Register_C, Byte_Read( Register_HL ) ) ;
                end else
                begin
                    B := Input( Register_C ) ;
                    Bytewrite( Register_HL, B ) ;
                end ;
                Register[ Register_Set1, BC ] :=
                    Swap( ( Hi( _Register[ Register_Set1, BC ] ) - 1 ) and 255 ) or
                    Lo( _Register[ Register_Set1, BC ] ) ;
                Register[ Register_Set1, HL ] := _Register[ Register_Set1, HL ] + 1 ;
                if( hi( _Register[ Register_Set1, BC ] ) <> 0 ) then
                begin
                    Increment_Clock( 16 ) ;
                    PC := PC - 2 ; { Loop }
                end else
                begin
                    Increment_Clock( 21 ) ;
                end ;
                Exit ;
            end ;
            if ( A = $A2 ) or ( A = $A3 ) then { INI or OUTI }
            begin
                Increment_Clock( 16 ) ;
                Set_N ;
                if( A = $A3 ) then
                begin
                    Output( Register_BC, Byte_Read( Register_HL) ) ;
                end else
                begin
                    B := Input( Register_C ) ;
                    Bytewrite( Register_HL, B ) ;
                end ;
                Register[ Register_Set1, BC ] :=
                    swap( ( Hi( _Register[ Register_Set1, BC ] ) - 1 ) and 255 ) or
                    Lo( _Register[ Register_Set1, BC ] ) ;
                Reset_Z ;
                if Hi( _Register[ Register_Set1, BC ] ) = 0 then
                begin
                    Set_Z ;
                end ;
                Register[ Register_Set1, HL ] :=
                    _Register[ Register_Set1, HL ] + 1 ;
                exit ;
            end ;
            if( ( A = $BB ) or ( A = $BA ) ) then { OTDR or INDR }
            begin
                Set_N ;
                Set_Z ;
INDR:
                if( A = $BB ) then { OTDR }
                begin
                    Output( Register_BC, Byte_Read( Register_HL ) ) ;
                end else
                begin
                    B := Input( Register_C ) ;
                    Bytewrite( Register_HL, B ) ;
                end ;
                Register[ Register_Set1, HL ] :=
                    _Register[ Register_Set1, HL ] - 1 ;
                Register[ Register_Set1, BC ] :=
                    Swap( ( Hi( _Register[ Register_Set1, BC ] ) - 1 ) and 255 ) or
                    Lo( _Register[ Register_Set1, BC ] ) ;
                if( Hi( _Register[ Register_Set1, BC ] ) <> 0 ) then { B <> 0 }
                begin
                    Increment_Clock( 21 ) ;
                    PC := PC - 2 ; { Loop }
                end else
                begin
                    Increment_Clock( 16 ) ;
                end ;
                exit ;
            end ;
            if( ( A = $AB ) or ( A = $AA ) ) then {IND or OUTD}
            begin
                Increment_Clock( 16 ) ;
                Set_N ;
                Register[ Register_Set1, BC ] :=
                    Swap( ( Hi( _Register[ Register_Set1, BC ] ) - 1 ) and 255 ) or
                    Lo( _Register[ Register_Set1, BC ] ) ;
                if( A = $AB ) then
                begin
                    Output( Register_BC, Byte_Read( Register_HL ) ) ;
                end else
                begin
                    B := Input( Register_C ) ;
                    Bytewrite( Register_HL, B ) ;
                end ;
                Register[ Register_Set1, HL ] :=
                    _Register[ Register_Set1, HL ] - 1 ;
                Reset_Z ;
                if Hi( _Register[ Register_Set1, BC ] ) = 0 then
                begin
                    Set_Z ;
                end ;
                exit ;
            end ;
            if( A = $46 ) then { IM 0 }
            begin
                Increment_Clock( 8 ) ;
                Im := 0 ;
                Exit ;
            end ;
            if( A = $56 ) then { IM 1 }
            begin
                Increment_Clock( 8 ) ;
                Im := 1 ;
                Exit ;
            end ;
            if( A = $5E ) then { IM 2 }
            begin
                Increment_Clock( 8 ) ;
                Im := 2 ;
                Exit ;
            end ;
            if( A = $57 ) then { LD A,I }
            begin
                Increment_Clock( 9 ) ;
                Register[ Register_Set0, AF ] :=
                    I or Swap( Hi( _Register[ Register_Set0, AF ] ) ) ;
                if( Interrupts ) then
                begin
                    Set_P ;
                end ;
                exit ;
            end ;
            if( A = $5F ) then { LD A,R }
            begin
                Increment_Clock( 9 ) ;
                Register[ Register_Set0, AF ] :=
                    R or Swap( Hi( _Register[ Register_Set0, AF ] ) ) ;
                Flags( R ) ;
                if Interrupts then
                begin
                    Set_P ;
                end ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and $EDFF ; { Clear H and N flags }
                exit ;
            end ;
            if( A = $47 ) then { LD I,A }
            begin
                Increment_Clock( 9 ) ;
                I := Get_A ;
                exit ;
            end ;
            if( A = $4F ) then { LD R,A }
            begin
                Increment_Clock( 9 ) ;
                R := Get_A ;
                exit ;
            end ;
            if( ( A and $CF ) = $4B ) then { LD dd,(nn) }
            begin
                Increment_Clock( 20 ) ;
                C := Fetch_Word ;
                C := Word_Read( C ) ;
                A := ( ( A div 16 ) and 3 ) + 1 ;
                if( A = 4 ) then
                begin
                    SP := C ;
                end else
                begin
                    Register[ Register_Set1, A ] := C ;
                end ;
                Exit ;
            end ;
            if( ( A and $CF ) = $43 ) then { LD (nn),dd }
            begin
                Increment_Clock( 20 ) ;
                C := Fetch_Word ;
                A := ( ( A div 16 ) and 3 ) + 1 ;
                if( A = 4 ) then
                begin
                    B := Sp ;
                end else
                begin
                    B := Register[ Register_Set1, A ] ;
                end ;
                MemWrite( C, chr( B ) + chr( swap( B ) ) ) ;
                exit ;
            end ;
            if( A = $44 ) then { NEG }
            begin
                Increment_Clock( 4 ) ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 1280 ) ;
                Flags_Changed := True ;
                if Lo( _Register[ Register_Set0, AF ] ) = $80 then
                begin
                    Set_C2 ;
                end ;
                if Lo( _Register[ Register_Set0, AF ] ) = 0 then
                begin
                    Set_C ;
                end ;
                Set_A( lo( -lo( _Register[ Register_Set0, AF ] ) ) ) ;
                Set_N ;
                exit ;
            end ;
            if( A = $4D ) then { RETI }
            begin
                if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                begin
                    if( _RTS.Return ) then
                    begin
                        exit ;
                    end ;
                end ;
                Increment_Clock( 14 ) ;
                Pc := Pop ;
                PC := PC or swap( Pop ) ;
                exit ;
            end ;
            if( A = $45 ) then { RETN }
            begin
                if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                begin
                    if( _RTS.Return ) then
                    begin
                        exit ;
                    end ;
                end ;
                Increment_Clock( 14 ) ;
                Pc := Pop ;
                PC := PC or swap( Pop ) ;
                Interrupts := IFF2 ;
                In_NMI := False ;
                State_Change_Notice( State_NMI, False ) ;
                exit ;
            end ;
            if( A = $6F ) then { RLD }
            begin
                Increment_Clock( 18 ) ;
                A := Byte_Read( Register[ Register_Set1, HL ] ) ; { Get (HL) }
                B := A div 16 ; { Get upper 4 bits }
                A := ( ( A * 16 ) and 255 ) or ( Register[ Register_Set0, AF ] and 15 ) ;
                { New value for (HL) }
                Set_A( ( Register[ Register_Set0, AF ] and ( not 15 ) ) or B ) ; { Fix A }
                ByteWrite( Register[ Register_Set1, HL ], A ) ; { Fix (HL) }
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags( _Register[ Register_Set0, AF ] and $FF ) ;
                exit ;
            end ;
            if( A = $67 ) then { RRD }
            begin
                Increment_Clock( 18 ) ;
                A := Byte_Read( Register[ Register_Set1, HL ] ) ; { Get (HL) }
                B := A and 15 ; { Get low 4 bits }
                A := ( A div 16 ) or ( ( Register[ Register_Set0, AF ] and 15 ) * 16 ) ;
                { New value for (HL) }
                _Register[ Register_Set0, AF ] :=
                    ( _Register[ Register_Set0, AF ] and ( not 15 ) ) or B ; { Fix A }
                ByteWrite( _Register[ Register_Set1, HL ], A ) ; { Fix (HL) }
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags( _Register[ Register_Set0, AF ] and $FF ) ;
                exit ;
            end ;
            if( ( A and $CF ) = $4A ) then { ADC HL,ss }
            begin
                Increment_Clock( 15 ) ;
                B := Er2( A, 16 ) ; {Get value to add to HL}
                if ( Is_C_Set ) then A := 1 else A := 0 ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                if ( B and 4091 ) + ( _Register[ Register_Set1, HL ] and 4091 ) + A > 4091 then Set_C2 ;
                Carry16( A, B, _Register[ Register_Set1, HL ] ) ;
                B := A + _Register[ Register_Set1, HL ] + B ;
                Register[ Register_Set1, HL ] := B ;
                Flags16( B ) ;
                Reset_N ;
                Exit
            end ;
            if( ( A and $CF ) = $42 ) then { SBC HL,ss }
            begin
                Increment_Clock( 15 ) ;
                A := Er2( A, 16 ) ; { Get value to subtract from HL }
                if( Is_C_Set ) then
                begin
                    B := 1 ;
                end else
                begin
                    B := 0 ;
                end ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags_Changed := True ;
                A := -( A + B ) ; { Value plus carry in 2's complement }
                if( ( -A and 4091 ) + ( _Register[ Register_Set1, HL ] and 4091 ) > 4091 ) then
                begin
                    Set_C2 ;
                end ;
                if( ( A = 0 ) and ( B = 1 ) ) then { If a carry occured }
                begin
                    Temp := 32768.0 ;
                end else
                begin
                    Temp := A ;
                end ;
                Carry16( Temp, _Register[ Register_Set1, HL ], 0 ) ;
                B := _Register[ Register_Set1, HL ] + A ;
                Register[ Register_Set1, HL ] := B ;
                Flags16( B ) ;
                Set_N ;
                exit ;
            end ;
            if( A = $A9 ) then { CPD }
            begin
                Increment_Clock( 16 ) ;
                B := Byte_Read( _Register[ Register_Set1, HL ] ) ; { (HL) }
                A := -B and 511 ; {Get and complement byte for subtraction}
                Set_N ;
                Cmp( A ) ; { Set H and S flags }
                B := Byte_Read( Register[ Register_Set1, HL ] ) ; { (HL) }
                Register[ Register_Set1, HL ] := _Register[ Register_Set1, HL ] - 1 ;
                Register[ Register_Set1, BC ] := _Register[ Register_Set1, BC ] - 1 ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 17408 ) ;
                if( _Register[ Register_Set1, BC ] <> 0 ) then
                    Set_P ;
                Flags_Changed := True ;
                if( B = Lo( _Register[ Register_Set0, AF ] ) ) then
                begin
                    Set_Z ;
                end ;
                exit ;
            end ;
            if( A = $B9 ) then { CPDR }
            begin
                Increment_Clock( 16 ) ;
                Set_N ;
                B := Byte_Read( Register[ Register_Set1, HL ] ) ;
                A := -B and 511 ; {Get and complement byte for subtraction}
CPDR:
                Cmp( A ) ;
                Register[ Register_Set1, HL ] := _Register[ Register_Set1, HL ] - 1 ;
                Register[ Register_Set1, BC ] := _Register[ Register_Set1, BC ] - 1 ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 17408 ) ;
                Flags_Changed := True ;
                B := Byte_Read( Register[ Register_Set1, HL ] ) ;
                if _Register[ Register_Set1, BC ] <> 0 then
                    Set_P ;
                if B = Lo( _Register[ Register_Set0, AF ] ) then
                begin
                    Set_Z ;
                end ;
                if(
                    ( _Register[ Register_Set1, BC ] <> 0 )
                    and
                    ( Lo( _Register[ Register_Set0, AF ] ) <> B )
                  ) then
                begin
                    Increment_Clock( 5 ) ;
                    PC := PC - 2 ; { Loop }
                end ;
                exit ;
            end ;
            if( A = $A1 ) then { CPI }
            begin
                Increment_Clock( 16 ) ;
                B := Byte_Read( _Register[ Register_Set1, HL ] ) ;
                A := -B and 511 ; {Get and complement byte for subtraction}
                Set_N ;
                Cmp( A ) ;
                B := Byte_Read( Register[ Register_Set1, HL ] ) ;
                Register[ Register_Set1, HL ] := _Register[ Register_Set1, HL ] + 1 ;
                Register[ Register_Set1, BC ] := _Register[ Register_Set1, BC ] - 1 ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 17408 ) ;
                if _Register[ Register_Set1, BC ] <> 0 then
                    Set_P ;
                Flags_Changed := True ;
                if B = Lo( _Register[ Register_Set0, AF ] ) then
                begin
                    Set_Z ;
                end ;
                exit ;
            end ;
            if( A = $B1 ) then { CPIR }
            begin
                Set_N ;
                B := Byte_Read( Register[ Register_Set1, HL ] ) ;
Cpir:
                Increment_Clock( 16 ) ;
                A := -B and 511 ; {Get and complement byte for subtraction}
                Cmp( A ) ;
                Register[ Register_Set1, HL ] := _Register[ Register_Set1, HL ] + 1 ;
                B := Byte_Read( _Register[ Register_Set1, HL ] ) ;
                Register[ Register_Set1, BC ] := _Register[ Register_Set1, BC ] - 1 ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 17408 ) ;
                Flags_Changed := True ;
                if _Register[ Register_Set1, BC ] <> 0 then
                    Set_P ;
                if B = Lo( _Register[ Register_Set0, AF ] ) then
                begin
                    Set_Z ;
                end ;
                if(
                    ( _Register[ Register_Set1, BC ] <> 0 )
                    and
                    ( Lo( _Register[ Register_Set0, AF ] ) <> B )
                  ) then
                begin
                    Increment_Clock( 5 ) ;
                    PC := PC - 2 ; { Loop }
                end ;
                Exit ;
            end ;
            if( A = $A8 ) then { LDD }
            begin
                Increment_Clock( 16 ) ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 5632 ) ;
                ByteWrite( Register[ Register_Set1, DE ], Byte_Read( Register[ Register_Set1, HL ] ) ) ;
                for A := 1 to 3 do
                begin
                    Register[ Register_Set1, A ] :=
                        _Register[ Register_Set1, A ] - 1 ;
                end ;
                if( _Register[ Register_Set1, BC ] <> 0 ) then
                begin
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] or $400 ;
                end ;
                Flags_Changed := True ;
                exit ;
            end ;
            if( A = $B8 ) then { LDDR }
            begin
LDDR:
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 5632 ) ;
                Flags_Changed := True ;
                ByteWrite( Register[ Register_Set1, DE ], Byte_Read( Register[ Register_Set1, HL ] ) ) ;
                for A := 1 to 3 do
                begin
                    Register[ Register_Set1, A ] :=
                        _Register[ Register_Set1, A ] - 1 ;
                end ;
                if( _Register[ Register_Set1, BC ] <> 0 ) then
                begin
                    Increment_Clock( 21 ) ;
                    PC := PC - 2 ; { Loop }
                end else
                begin
                    Increment_Clock( 16 ) ;
                end ;
                exit ;
            end ;
            if( A = $A0 ) then { LDI }
            begin
                Increment_Clock( 16 ) ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 5632 ) ;
                ByteWrite( Register[ Register_Set1, DE ], Byte_Read( Register[ Register_Set1, HL ] ) ) ;
                Register[ Register_Set1, BC ] :=
                    Register[ Register_Set1, BC ] - 1 ;
                for A := 2 to 3 do
                begin
                    Register[ Register_Set1, A ] :=
                        _Register[ Register_Set1, A ] + 1 ;
                end ;
                if _Register[ Register_Set1, BC ] <> 0 then
                begin
                    Set_P ;
                end ;
                Flags_Changed := True ;
                exit ;
            end ;
            if( A = $B0 ) then { LDIR }
            begin
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 5632 ) ;
                Flags_Changed := True ;
LDIR:
                ByteWrite( Register[ Register_Set1, DE ], Byte_Read( Register[ Register_Set1, HL ] ) ) ;
                Register[ Register_Set1, BC ] :=
                    _Register[ Register_Set1, BC ] - 1 ;
                for A := 2 to 3 do
                begin
                    Register[ Register_Set1, A ] :=
                        _Register[ Register_Set1, A ] + 1 ;
                end ;
                if( _Register[ Register_Set1, BC ] <> 0 ) then
                begin
                    Increment_Clock( 21 ) ;
                    PC := PC - 2 ; { Loop }
                end else
                begin
                    Increment_Clock( 16 ) ;
                end ;
                exit ;
            end ;
            goto Unknown_OpCode ; { Error }
        end ;
        if( A = $CB ) then { Extension $CB }
        begin
            A := Fetch ; {Get op-code}
            if( ( A and $C0 ) = $40 ) then { BIT b,r or BIT b,(HL) }
            begin
                Increment_Clock( 8 ) ;
                if( ( A and 7 ) = 6 ) then
                begin
                    Increment_Clock( 4 ) ;
                end ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 16896 ) ; { Clear N & Z }
                Flags_Changed := True ;
                Set_C2 ;
                C := Bit_Values[ ( A div 8 ) and 7 ] ; {bit}
                A := Extract_Register( A, 1 ) ;
                if( ( A and C ) <> 0 ) then { Bit is set }
                begin
                    Set_Z ;
                end ;
                exit ;
            end ;
            if( ( A and $C0 ) = $80 ) then { RES b,r }
            begin
                if( ( A and 7 ) = 6 ) then { RES b,(HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Bit_Values[ ( A div 8 ) and 7 ] ; { Bit }
                Set_Register( A, 1, Extract_Register( A, 1 ) and ( not B ) ) ;
                exit ;
            end ;
            if( ( A and $F8 ) = $10 ) then { RL r/RL (HL) }
            begin
                if( ( A and 7 ) = 6 ) then { RL (HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Extract_Register( A, 1 ) * 2 ;
                if( Is_C_Set ) then
                begin
                    B := B or 1 ;
                end ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                if( B > 255 ) then
                begin
                    Set_C ;
                end ;
                Set_Register( A, 1, B ) ;
                Flags( B ) ;
                exit ;
            end ;
            if( ( A and $F8 ) = 0 ) then { RLC r/RLC (HL) }
            begin
                if( ( A and 7 ) = 6 ) then { RLC (HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Extract_Register( A, 1 ) * 2 ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                if B > 255 then
                begin
                    B := B or 1 ;
                    Set_C ;
                end ;
                Set_Register( A, 1, B ) ;
                Flags( B ) ;
                exit ;
            end ;
            if( ( A and $F8 ) = $18 ) then { RR r/RR (HL) }
            begin
                if( ( A and 7 ) = 6 ) then { RR (HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Extract_Register( A, 1 ) ;
                if( Is_C_Set ) then
                begin
                    B := B or 256 ;
                end ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                if( ( B and 1 ) = 1 ) then
                begin
                    Set_C ;
                end ;
                B := B div 2 ;
                Set_Register( A, 1, B ) ;
                Flags( B ) ;
                exit ;
            end ;
            if( ( A and $F8 ) = $08 ) then { RRC r/RRC (HL) }
            begin
                if( ( A and 7 ) = 6 ) then { RRC (HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Extract_Register( A, 1 ) ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                if ( B and 1 ) = 1 then
                begin
                    B := B or 256 ;
                    Set_C
                end ;
                B := B div 2 ;
                Set_Register( A, 1, B ) ;
                Flags( B ) ;
                Exit ;
            end ;
            if( ( A and $C0 ) = $C0 ) then { SET b,r }
            begin
                if( ( A and 7 ) = 6 ) then { SET b,(HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Bit_Values[ ( A div 8 ) and 7 ] ; { Bit }
                Set_Register( A, 1, Extract_Register( A, 1 ) or B ) ;
                exit ;
            end ;
            if( ( A and $F8 ) = $20 ) then { SLA r }
            begin
                if( ( A and 7 ) = 6 ) then { SLA (HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Extract_Register( A, 1 ) * 2 ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                if( B > 255 ) then
                begin
                    Set_C ;
                end ;
                Set_Register( A, 1, B ) ;
                Flags( B ) ;
                exit ;
            end ;
            if( ( A and $F8 ) = $28 ) then { SRA r }
            begin
                if( ( A and 7 ) = 6 ) then { SRA (HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Extract_Register( A, 1 ) ;
                if( ( B and 128 ) = 128 ) then
                begin
                    B := B or 256 ;
                end ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                if( ( B and 1 ) = 1 ) then
                begin
                    Set_C ;
                end ;
                B := B div 2 ;
                Set_Register( A, 1, B ) ;
                Flags( B ) ;
                exit ;
            end ;
            if( ( A and $F8 ) = $38 ) then { SRL r }
            begin
                if( ( A and 7 ) = 6 ) then { SRL (HL) }
                begin
                    Increment_Clock( 15 ) ;
                end else
                begin
                    Increment_Clock( 8 ) ;
                end ;
                B := Extract_Register( A, 1 ) ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                if( ( B and 1 ) = 1 ) then
                begin
                    Set_C ;
                end ;
                B := B div 2 ;
                Set_Register( A, 1, B ) ;
                Flags( B ) ;
                exit ;
            end ;
            goto Unknown_OpCode ; { Error }
        end ;
        if( ( A = $DD ) or ( A = $FD ) ) then {Extensions $DD and $FD}
        begin
            if( A = $DD ) then {IX reference}
            begin
                Index := 0 ;
                IZ := IX ;
            end else {IY reference}
            begin
                Index := 1 ;
                IZ := IY ;
            end ;
            A := Fetch ;
            if( A = $8E ) then { ADC A,(IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                Overflow := True ;
                A := Fetch ; {Get D}
                A := Byte_Read( A + Iz ) ;
                if ( Is_C_Set ) then C := 1 else C := 0 ;
                Add( A, C ) ;
                Exit
            end ;
            if A = $86 then { ADD A,(IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                Overflow := True ;
                A := Fetch ; {Get D}
                A := Byte_Read( A + Iz ) ;
                Add( A, 0 ) ;
                Exit
            end ;
            if ( A and $CF ) = 9 then { ADD IZ,rr }
            begin
                Increment_Clock( 15 ) ;
                Overflow := True ;
                A := ( ( A div 16 ) and 3 ) + 1 ;
                case A of
                     1..2 : B := Register[ Register_Set1, A ] ;
                        3 : if Index = 0 then B := Ix else B := Iy ;
                     else B := Sp ;
                end ;
                if Index = 0 then A := Ix else A := Iy ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                Flags_Changed := True ;
                if ( A and 4091 ) + ( B and 4091 ) > 4091 then
                begin
                    Set_C2 ;
                end ;
                Carry16( A, B, 0 ) ;
                A := A + B ;
                if Index = 0 then Ix := A else Iy := A ;
                Exit ;
            end ;
            if( A = $A6 ) then { AND (IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                A := Fetch ; {Get D}
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 768 ) ;
                Flags_Changed := True ;
                Set_C2 ;
                B := Byte_Read( Iz + A ) and Lo( _Register[ Register_Set0, AF ] ) ;
                Set_A( B ) ;
                Flags( B ) ;
                Exit
            end ;
            if( A = $BE ) then { CP (IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                Overflow := True ;
                A := Fetch ; {Get D}
                A := Byte_Read( Iz + A ) ;
                Ccmp( A ) ;
                Exit ;
            end ;
            if( A = $35 ) then { DEC (IZ+d) }
            begin
                Increment_Clock( 23 ) ;
                Overflow := True ;
                A := Fetch ; {Get D}
                B := Iz + A ;
                A := Byte_Read( B ) ;
                Reset_N ;
                Dec( A ) ;
                MemWrite( B, Chr( A ) ) ;
                Exit ;
            end ;
            if( A = $2B ) then { DEC IZ }
            begin
                Increment_Clock( 10 ) ;
                Iz := Iz - 1 ;
                if Index = 0 then Ix := Iz else Iy := Iz ;
                Exit ;
            end ;
            if( A = $E3 ) then { EX (SP),IZ }
            begin
                Increment_Clock( 23 ) ;
                A := Swap( Byte_Read( Sp + 1 ) ) or Byte_Read( Sp ) ;
                if Index = 0 then Ix := A else Iy := A ;
                ByteWrite( Sp, Lo( Iz ) ) ;
                ByteWrite( Sp + 1, Hi( Iz ) ) ;
                Exit ;
            end ;
            if( A = $34 ) then { INC (IZ+d) }
            begin
                Increment_Clock( 23 ) ;
                Overflow := True ;
                A := Fetch ; {Get D}
                B := Iz + A ;
                A := Byte_Read( B ) ;
                Reset_N ;
                Inc( A ) ;
                MemWrite( B, Chr( A ) ) ;
                Exit ;
            end ;
            if( A = $23 ) then { INC IZ }
            begin
                Increment_Clock( 10 ) ;
                inc( IZ ) ;
                if( Index = 0 ) then
                begin
                    IX := IZ ;
                end else
                begin
                    IY := IZ ;
                end ;
                Exit ;
            end ;
            if( A = $E9 ) then { JP (IZ) }
            begin
                Increment_Clock( 8 ) ;
                Pc := Iz ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
                Exit ;
            end ;
            if( A = $2A ) then { LD IZ,(n) }
            begin
                Increment_Clock( 20 ) ;
                A := Fetch_Word ;
                B := Word_Read( A ) ;
                if( Index = 0 ) then
                begin
                    Ix := B ;
                end else
                begin
                    Iy := B ;
                end ;
                exit ;
            end ;
            if( A = $21 ) then { LD IZ,n }
            begin
                Increment_Clock( 14 ) ;
                A := Fetch_Word ;
                if( Index = 0 ) then
                begin
                    IX := A ;
                end else
                begin
                    IY := A ;
                end ;
                exit ;
            end ;
            if( A = $36 ) then { LD (IZ+d),n }
            begin
                Increment_Clock( 19 ) ;
                A := Fetch ; { Get D }
                Aa := Chr( Fetch ) ; { Get data byte }
                A := Iz + A ;
                MemWrite( A, Aa ) ; { Write it to memory }
                Exit ;
            end ;
            if( ( A and $F8 ) = $70 ) then { LD (IZ+d),r }
            begin
                Increment_Clock( 19 ) ;
                B := Extract_Register( A, 1 ) ; { Get data byte }
                A := Fetch ; { Get D }
                A := Iz + A ;
                MemWrite( A, chr( B ) ) ; { Write it to memory }
                exit ;
            end ;
            if( A = $22 ) then { LD (n),IZ }
            begin
                Increment_Clock( 20 ) ;
                A := Fetch_Word ;
                MemWrite( A, Chr( Lo( Iz ) ) + Chr( Hi( Iz ) ) ) ;
                exit ;
            end ;
            if( ( A and $C7 ) = $46 ) then { LD r,(IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                C := Fetch ; { Get D }
                B := Byte_Read( C + IZ ) ;
                Set_Register( A, 8, B ) ;
                Exit ;
            end ;
            if( A = $F9 ) then { LD SP,IZ }
            begin
                Increment_Clock( 10 ) ;
                SP := IZ ;
                exit ;
            end ;
            if( A = $B6 ) then { OR (IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                A := Fetch ; { Get D }
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 768 ) ;
                Flags_Changed := True ;
                Set_C2 ;
                B := Byte_Read( Iz + A ) or Lo( _Register[ Register_Set0, AF ] ) ;
                Set_A( B ) ;
                Flags( B ) ;
                exit ;
            end ;
            if( A = $E1 ) then { POP IZ }
            begin
                Increment_Clock( 14 ) ;
                if( Index = 0 ) then
                begin
                    Ix := Pop ;
                    Ix := IX or swap( Pop ) ;
                end else
                begin
                    Iy := Pop ;
                    Iy := IY or swap( Pop ) ;
                end ;
                exit ;
            end ;
            if( A = $E5 ) then { PUSH IZ }
            begin
                Increment_Clock( 15 ) ;
                Push( Hi( Iz ) ) ;
                Push( Lo( Iz ) ) ;
                exit ;
            end ;
            if( A = $CB ) then { Combination extensions $DD $CB and $FD $CB }
            begin
                IZ := IZ + Fetch ; { Get (IZ+d) }
                A := Fetch ;
                if( ( A and $C7 ) = $46 ) then { BIT b,(IZ+d) }
                begin
                    Increment_Clock( 20 ) ;
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 16896 ) ;
                        { Clear N & Z }
                    Flags_Changed := True ;
                    Set_C2 ;
                    C := Bit_Values[ ( A div 8 ) and 7 ] ; {bit}
                    A := Byte_Read( Iz ) ;
                    if( ( A and C ) <> 0 ) then
                    begin
                        Set_Z ;
                    end ;
                    exit ;
                end ;
                if( ( A and $C7 ) = $86 ) then { RES b,(IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Bit_Values[ ( A div 8 ) and 7 ] ; { Bit }
                    MemWrite( Iz, Chr( Byte_Read( Iz ) and ( not B ) ) ) ;
                    exit ;
                end ;
                if( A = $16 ) then { RL (IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Byte_Read( Iz ) * 2 ;
                    if( Is_C_Set ) then
                    begin
                        B := B or 1 ;
                    end ;
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                    if( B > 255 ) then
                    begin
                        Set_C ;
                    end ;
                    MemWrite( Iz, Chr( B ) ) ;
                    Flags( B ) ;
                    exit ;
                end ;
                if( A = $06 ) then { RLC (IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Byte_Read( IZ ) * 2 ;
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                    if( B > 255 ) then
                    begin
                        B := B or 1 ;
                        Set_C ;
                    end ;
                    MemWrite( IZ, chr( B ) ) ;
                    Flags( B ) ;
                    exit ;
                end ;
                if( A = $1E ) then { RR (IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Byte_Read( IZ ) ;
                    if( Is_C_Set ) then
                    begin
                        B := B or 256 ;
                    end ;
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                    if( ( B and 1 ) = 1 ) then
                    begin
                        Set_C ;
                    end ;
                    B := B div 2 ;
                    MemWrite( IZ, chr( B ) ) ;
                    Flags( B ) ;
                    exit ;
                end ;
                if( A = $0E ) then { RRC (IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Byte_Read( IZ ) ;
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                    if( ( B and 1 ) = 1 ) then
                    begin
                        B := B or 256 ;
                        Set_C ;
                    end ;
                    B := B div 2 ;
                    MemWrite( IZ, Chr( B ) ) ;
                    Flags( B ) ;
                    exit ;
                end ;
                if( ( A and $C7 ) = $C6 ) then { SET b,(IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Bit_Values[ ( A div 8 ) and 7 ] ; { Bit }
                    C := Byte_Read( IZ ) or B ;
                    MemWrite( IZ, chr( C ) ) ;
                    exit ;
                end ;
                if( A = $26 ) then { SLA (IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Byte_Read( IZ ) * 2 ;
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                    if( B > 255 ) then
                    begin
                        Set_C ;
                    end ;
                    MemWrite( IZ, chr( B ) ) ;
                    Flags( B ) ;
                    exit ;
                end ;
                if( A = $2E ) then { SRA (IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Byte_Read( IZ ) ;
                    if( ( B and 128 ) = 128 ) then
                    begin
                        B := B or 256 ;
                    end ;
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                    if( ( B and 1 ) = 1 ) then
                    begin
                        Set_C ;
                    end ;
                    B := B div 2 ;
                    ByteWrite( IZ, B ) ;
                    Flags( B ) ;
                    exit ;
                end ;
                if( A = $3E ) then { SRL (IZ+d) }
                begin
                    Increment_Clock( 23 ) ;
                    B := Byte_Read( IZ ) ;
                    _Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 4864 ) ;
                    if( ( B and 1 ) = 1 ) then
                    begin
                        Set_C ;
                    end ;
                    B := B div 2 ;
                    ByteWrite( IZ, B ) ;
                    Flags( B ) ;
                    exit ;
                end ;
                goto Unknown_OpCode ; { Error }
            end ;
            if( A = $9E ) then { SBC A,(IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                Overflow := True ;
                if( Is_C_Set ) then
                begin
                    B := 1 ;
                end else
                begin
                    B := 0 ;
                end ;
                A := Byte_Read( IZ ) ;
                Sub( A, B ) ;
                exit ;
            end ;
            if( A = $96 ) then { SUB (IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                A := Byte_Read( IZ ) ;
                Sub( A, 0 ) ;
                exit ;
            end ;
            if( A = $AE ) then { XOR (IZ+d) }
            begin
                Increment_Clock( 19 ) ;
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 768 ) ;
                Set_C2 ;
                B := Byte_Read( IZ ) xor Lo( _Register[ Register_Set0, AF ] ) ;
                Set_A( B ) ;
                Flags( B ) ;
                exit ;
            end
        end ;
    Unknown_OpCode:
        Execute_Z80 := False ; { Failure }
    end ; { Execute_Z80 }


label Unknown_OpCode ;

var A, A1, B, C, E : integer ;
    Did_Interrupt_Fetch : boolean ;
    Lng : longint ;
    Count : integer ;
    Nest_Level : integer ;

begin // TZ80_CPU.Execute
    // Setup...
    Stopping := False ;
    Count := 0 ;
    Nest_Level := 0 ;
    Did_Interrupt_Fetch := False ;
    Flags_Changed := False ;
    Flags_Read := False ;

    // Execution loop
    while( True ) do
    begin
        if( Flags_Changed ) then
        begin
            Flags_Changed := False ;
            if( _Run_Stream = nil ) then
            begin
                if( ( _Register_Watchpoints[ 2 ] and Access_Write ) <> 0 ) then
                begin
                    Watchpoint_Notice( 2, Access_Write, 0, False, True, False )
                end else
                if( ( _Register_Watchpoints[ 7 ] and Access_Write ) <> 0 ) then
                begin
                    Watchpoint_Notice( 7, Access_Write, 0, False, True, False )
                end ;
            end ;
        end ;
        if( Flags_Read ) then
        begin
            Flags_Read := False ;
            if( _Run_Stream = nil ) then
            begin
                if( ( _Register_Watchpoints[ 2 ] and Access_Read ) <> 0 ) then
                begin
                    Watchpoint_Notice( 2, Access_Read, 0, False, True, False )
                end else
                if( ( _Register_Watchpoints[ 7 ] and Access_Read ) <> 0 ) then
                begin
                    Watchpoint_Notice( 7, Access_Read, 0, False, True, False )
                end ;
            end ;
        end ;
        if( Stopping ) then
        begin
            Stopping := False ;
            exit ;
        end ;
        if( Blocked ) then
        begin
            Do_Wait ;
            continue ;
        end ;
        if( Did_Interrupt_Fetch ) then
        begin
            Interrupt_Instruction := False ; // No longer fetching an instruction
        end ;
        if( Nest_Level = 0 ) then
        begin
            inc( Count ) ;
        end ;
        if( Single_Step and ( Count > 1 ) ) then
        begin
            exit ;
        end ;

        if( not Interrupt_Instruction ) then
        begin
            // Exit if we are done
            if( _Run_Stream = nil ) then
            begin
                if( _Halted ) then
                begin
                    exit ;
                end ;
                if( RST7_Enabled and RST7_Pending ) then
                begin
                    RST7_Pending := False ;
                    Push_PC ;
                    Interrupts := False ;
                    PC := $3C ;
                end ;
            end else
            begin
                if( _Run_Stream.At_End ) then
                begin
                    exit ;
                end ;
            end ;
        end ;

        if( ( Count > 1 ) and ( _Run_Stream = nil ) ) then
        begin
            if( _Breakpoints.Indexof( _PC ) <> -1 ) then
            begin
                _UI.Breakpoint_Notice( _PC, True, 0, Parent ) ;
                if( Stopping ) then
                begin
                    exit ;
                end ;
            end ;
        end ;
        if( _Profiling ) then
        begin
            TZ80_Profiler( Parent.Profiler ).Increment( Domain_Execution_Addresses, PC ) ;
            TZ80_Profiler( Parent.Profiler ).Increment( Domain_Other, Domain_Other_Instruction_Count ) ;
        end ;
        Log_Trace( 'Executing instruction at address ' + cvtb( 10, Base, inttostr( PC ) ) + ': ' + Instruction_At( PC ) ) ;
        A := Fetch ;
        if( Interrupt_Instruction ) then
        begin
            Did_Interrupt_Fetch := True ;
        end ;

        if( _Profiling ) then
        begin
            TZ80_Profiler( Parent.Profiler ).Increment( Domain_Instructions, A ) ;
        end ;
        if( _Logger <> nil ) then
        begin
            _Logger.Update( Parent, PC, A ) ;
        end ;
        if( A = 0 ) then
        begin
            Increment_Clock( 4 ) ;
            continue ; { NOP }
        end ;

        if( Mode = M8080 ) then { Handle undocumented/unintended unused opcodes }
        begin
            if( A = $CB ) then
            begin
                Signal_Exception( '', 0 ) ; // Invalid instruction
                A := $C3 ;
            end ;
        end ;
        if( ( A = $DB ) or ( A = $D3 ) ) then { IN and OUT }
        begin
            Increment_Clock( 11 ) ;
            A1 := Fetch ; { Get port number }
            if( A = $D3 ) then { OUT }
            begin
                Output( A1, Register_A ) ;
            end else
            begin
                Register_A := Input( A1 ) ;
            end ;
            continue ;
        end ;
        if( A = $76 ) then { HALT }
        begin
            Increment_Clock( 4 ) ;
            _Halted := True ;
            continue ;
        end ;
        if( A = $FB ) then { EI }
        begin
            Increment_Clock( 4 ) ;
            Interrupts := True ;
            continue ;
        end ;
        if( A = $F3 ) then {DI}
        begin
            Increment_Clock( 4 ) ;
            Interrupts := False ;
            continue ;
        end ;
        if( ( A and $C7 ) = $C7 ) then { RST }
        begin
            Increment_Clock( 11 ) ;
            Push_PC ;
            PC := A and ( not $C7 ) ;
            _Run_Stream := nil ;
            if( not Into ) then
            begin
                inc( Nest_Level ) ;
            end ;
            continue ;
        end ;
        Overflow := False ;
        if( A = $3F ) then { CCF or CMC }
        begin
            Increment_Clock( 4 ) ;
            A := ( Hi( _Register[ Register_Set0, AF ] ) and 254 ) or ( ( not Hi( _Register[ Register_Set0, AF ] ) ) and 1 ) ;
            _Register[ Register_Set0, AF ] :=
                Swap( A ) or Lo( _Register[ Register_Set0, AF ] ) ;
            Flags_Changed := True ;
            continue ;
        end ;
        if( A = $37 ) then { SCF or STC }
        begin
            Increment_Clock( 4 ) ;
            if( Mode = Z80 ) then
            begin
                { Reset H and N }
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags_Changed := True ;
            end ;
            Set_C ;
            continue ;
        end ;
        if( ( A and $C7 ) = 4 ) then { INC or INR }
        begin
            Increment_Clock( 4 ) ;
            if( A = $34 ) then { INC (HL) }
            begin
                Increment_Clock( 7 ) ;
            end ;
            if( Mode = Z80 ) then
            begin
                Reset_N ;
                Overflow := True ;
            end ;
            B := Extract_Register( A, 8 ) ;
            if( A <> $34 ) then
            begin
                if( ( B and 15 ) = 15 ) then
                begin
                    Set_C2 ;
                end else
                begin
                    Reset_C2 ;
                end ;
            end ;
            Inc( B ) ;
            if( A <> $34 ) then
            begin
                Flags( B ) ;
            end ;
            Set_Register( A, 8, B ) ;
            continue ;
        end ;
        if( ( A and $C7 ) = 5 ) then { DEC or DCR }
        begin
            Increment_Clock( 4 ) ;
            if( Mode = Z80 ) then
            begin
                Set_N ;
                Overflow := True
            end ;
            B := Extract_Register( A, 8 ) ;
            if( ( ( A div 8 ) and 7 ) = 6 ) then { DEC (HL) }
            begin
                Increment_Clock( 7 ) ;
            end ;
            if( A <> $34 ) then
            begin
                if( ( B and 15 ) = 0 ) then // Half-borrow
                begin
                    Set_C2 ;
                end else
                begin
                    Reset_C2 ;
                end ;
            end ;
            Dec( B ) ;
            if( ( ( A div 8 ) and 7 ) <> 6 ) then
            begin
                Flags( B ) ;
            end ;
            Set_Register( A, 8, B ) ;
            continue ;
        end ;
        if( A = $2F ) then { CPL or CMA }
        begin
            Increment_Clock( 4 ) ;
            A := not Lo( _Register[ Register_Set0, AF ] ) ;
            _Register[ Register_Set0, AF ] :=
                Swap( Hi( _Register[ Register_Set0, AF ] ) ) or ( A and 255 ) ;
            if( Mode = Z80 ) then
            begin
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] or 4608 ; { Set H and N }
            end ;
            Flags_Changed := True ;
            continue ;
        end ;
        if( A = $27 ) then { DAA }
        begin
            Increment_Clock( 4 ) ;
            A := Lo( _Register[ Register_Set0, AF ] ) ;
            if( not Is_N_Set ) then { N = 0 }
            begin
                if( not Is_C_Set ) then { C = 0 }
                begin
                    if( Is_C2_Set ) then { H = 0 }
                    begin
                        if( ( A and $F ) > 9 ) then
                        begin
                            if( A > $8F ) then
                            begin
                                A := A + $66 ;
                                Set_C ;
                            end else
                            begin
                                A := A + 6 ;
                            end ;
                        end else
                        begin
                            if( A > $9F ) then
                            begin
                                A := A + $60 ;
                                Set_C ;
                            end ;
                        end ;
                    end else
                    begin
                        if( A > $9F ) then
                        begin
                            A := A + $66 ;
                            Set_C ;
                        end else
                        begin
                            A := A + 6 ;
                        end ;
                    end ;
                end else
                begin
                    if( Is_C2_Set ) then { H = 0 }
                    begin
                        if( A and $F > 9 ) then
                        begin
                            A := A + $66 ;
                        end else
                        begin
                            A := A + $60 ;
                        end ;
                    end else
                    begin
                        A := A + $66 ;
                    end ;
                end ;
            end else
            begin
                if( not Is_C_Set ) then { C = 0 }
                begin
                    if( Is_C2_Set ) then { H <> 0 }
                    begin
                        A := A + $FA ;
                    end ;
                end else
                begin
                    if( not Is_C2_Set ) then { H = 0 }
                    begin
                        A := A + $A0 ;
                    end else
                    begin
                        A := A + $9A ;
                    end ;
                end ;
            end ;
            A := A and $FF ;
            Flags( A ) ;
            _Register[ Register_Set0, AF ] :=
                ( _Register[ Register_Set0, AF ] and $FF00 ) or A ;
            Flags_Changed := True ;
            continue ;
        end ;
        if( ( A and $CF ) = $C5 ) then { PUSH rr }
        begin
            Increment_Clock( 11 ) ;
            A := ( ( ( A div 16 ) and 3 ) + 1 ) and 3 ;
            if( A = 0 ) then
            begin
                B := Register_Set0 ;
                Push( Lo_Register( B, A ) ) ;
                Push( Hi_Register( B, A ) ) ;
            end else
            begin
                B := Register_Set1 ;
                Push( Hi_Register( B, A ) ) ;
                Push( Lo_Register( B, A ) ) ;
            end ;
            continue ;
        end ;
        if( ( A and $CF ) = $C1 ) then { POP rr }
        begin
            Increment_Clock( 10 ) ;
            A := ( ( ( A div 16 ) and 3 ) + 1 ) and 3 ;
            if( A = 0 ) then { AF }
            begin
                B := Register_Set0 ;
            end else
            begin
                B := Register_Set1 ;
            end ;
            Register[ B, A ] := Pop ;
            Register[ B, A ] := Register[ B, A ] or swap( Pop ) ;
            if( A = 0 ) then { Internally, our A is hi and F is lo }
            begin
                Register[ B, A ] := swap( Register[ B, A ] ) ;
            end ;
            continue ;
        end ;
        if ( A and $CF ) = 9 then { DAD or ADD HL,ss }
        begin
            Increment_Clock( 11 ) ;
            if Mode = Z80 then Reset_N ;
            A := ( ( ( A div 16 ) and 3 ) + 1 ) and 3 ;
            B := Register_Set1 ;
            if A = 0 then B := Register_Set0 ;
            Register[ B, 3 ] := Register[ B, A ] + Register[ B, 3 ] ;
            if Cvtif( Register[ B, A ] ) + Cvtif( Register[ B, 3 ] ) > 65535.0 then
            begin
                case Mode of
                    M8085: Set_D ;
                    Z80: Set_C ;
                end ;
            end else
            begin
                case Mode of
                    M8085: Reset_D ;
                    Z80: Reset_C ;
                end ;
            end ;
            continue ;
        end ;
        if ( A and $CF ) = 3 then { INX or INC ss }
        begin
            Increment_Clock( 6 ) ;
            A := ( ( A shr 4 ) and 3 ) + 1 ; { Register pair to increment }
            if( A = 4 ) then
            begin
                SP := SP + 1 ;
            end else
            begin
                Register[ Register_Set1, A ] :=
                    Register[ Register_Set1, A ] + 1 ;
                if( Mode = M8085 ) then
                begin
                    Reset_D ;
                    if( Register[ Register_Set1, A ] = 0 ) then
                    begin
                        Set_D ;
                    end ;
                end ;
            end ;
            continue ;
        end ;
        if( ( A and $CF ) = $B ) then { DCX or DEC ss }
        begin
            Increment_Clock( 6 ) ;
            A := ( A shr 4 ) and 3 + 1 ;
            if( A = 4 ) then
            begin
                SP := SP - 1 ;
            end else
            begin
                Register[ Register_Set1, A ] :=
                    Register[ Register_Set1, A ] - 1 ;
                if( Mode = M8085 ) then
                begin
                    Set_D ;
                    if( Register[ Register_Set1, A ] = $FFFF ) then
                    begin
                        Reset_D ;
                    end ;
                end ;
            end ;
            continue ;
        end ;
        if( A = $EB ) then { XCHG or EX DE,HL }
        begin
            Increment_Clock( 4 ) ;
            A := Register[ Register_Set1, DE ] ;
            Register[ Register_Set1, DE ] := Register[ Register_Set1, HL ] ;
            Register[ Register_Set1, HL ] := A ;
            continue ;
        end ;
        if( A = $E3 ) then { XTHL or EX (SP),HL }
        begin
            Increment_Clock( 19 ) ;
            A := Lo_Register( Register_Set1, HL ) ;
            B := Hi_Register( Register_Set1, HL ) ;
            Register[ Register_Set1, HL ] := Swap( Byte_Read( Sp + 1 ) ) or Byte_Read( Sp ) ;
            ByteWrite( Sp, A ) ;
            ByteWrite( Sp + 1, B ) ;
            continue ;
        end ;
        if( A = $F9 ) then { SPHL or LD SP,HL }
        begin
            Increment_Clock( 6 ) ;
            Sp := Register[ Register_Set1, HL ] ;
            continue ;
        end ;
        if( A = 7 ) then { RLC or RLCA }
        begin
            Increment_Clock( 4 ) ;
            if( Mode = Z80 ) then
            begin
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags_Changed := True ;
            end ;
            Reset_C ;
            A := Lo( _Register[ Register_Set0, AF ] ) ;
            if( A > 127 ) then
            begin
                A := A * 2 + 1 ;
            end else
            begin
                A := A * 2 ;
            end ;
            Register[ Register_Set0, AF ] :=
                swap( Hi( _Register[ Register_Set0, AF ] ) ) or A ;
            continue ;
        end ;
        if( A = $F ) then { RRC or RRCA }
        begin
            Increment_Clock( 4 ) ;
            if( Mode = Z80 ) then
            begin
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags_Changed := True ;
            end ;
            Reset_C ;
            A := lo( _Register[ Register_Set0, AF ] ) ;
            if( ( A and 1 ) = 1 ) then
            begin
                Set_C ;
                A := ( A div 2 ) or 128 ;
            end else
            begin
                A := A div 2 ;
            end ;
            Register[ Register_Set0, AF ] :=
                swap( hi( _Register[ Register_Set0, AF ] ) ) or A ;
            continue ;
        end ;
        if( A = $17 ) then { RAL or RLA }
        begin
            Increment_Clock( 4 ) ;
            if( Mode = Z80 ) then
            begin
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags_Changed := True ;
            end ;
            A := Lo( _Register[ Register_Set0, AF ] ) ;
            B := 0 ;
            if( Is_C_Set ) then
            begin
                B := 1 ;
            end ;
            Reset_C ;
            Register[ Register_Set0, AF ] :=
                swap( Hi( _Register[ Register_Set0, AF ] ) ) or A * 2 or B ;
            continue ;
        end ;
        if( A = $1F ) then { RAR or RRA }
        begin
            Increment_Clock( 4 ) ;
            if( Mode = Z80 ) then
            begin
                _Register[ Register_Set0, AF ] :=
                    _Register[ Register_Set0, AF ] and ( not 4608 ) ;
                Flags_Changed := True ;
            end ;
            A := _Register[ Register_Set0, AF ] and 511 ;
            if( ( A and 1 ) = 1 ) then
            begin
                Set_C ;
            end else
            begin
                Reset_C ;
            end ;
            Set_A( A div 2 ) ;
            continue ;
        end ;
        if( ( A and $C0 ) = $40 ) then { MOV ddd,sss or LD r,r' }
        begin
            Increment_Clock( 4 ) ;
            B := ( A and 7 ) + 2 ;
            A := ( ( A div 8 ) and 7 ) + 2 ;
            case B of
                 9 : E := Get_A ;
                 8 : begin
                         E := Byte_Read( Register[ Register_Set1, HL ] ) ;
                     end ;
                else if Odd( B ) then
                     begin
                         E := Lo_Register( Register_Set1, B div 2 ) ;
                     end else
                     begin
                         E := Hi_Register( Register_Set1, B div 2 ) ;
                     end ;
            end ;
            case A of
                 9 : Set_A( E ) ;
                 8 : begin
                         Increment_Clock( 3 ) ;
                         MemWrite( _Register[ Register_Set1, HL ], Chr( E ) ) ;
                     end ;
                else
                if Odd( A )
                    then
                        Register[ Register_Set1, A div 2 ] :=
                            Swap( Hi( _Register[ Register_Set1, A div 2 ] ) ) or E
                    else
                        Register[ Register_Set1, A div 2 ] :=
                            Swap( E ) or Lo( _Register[ Register_Set1, A div 2 ] )
            end ;
            continue ;
        end ;
        if( ( A and $EF ) = 2 ) then { STAX or LD (BC),A or LD (DE),A }
        begin
            Increment_Clock( 7 ) ;
            if( ( A and $10 ) = $10 ) then
            begin
                A := Register[ Register_Set1, DE ] ;
            end else
            begin
                A := Register[ Register_Set1, BC ] ;
            end ;
            MemWrite( A, Chr( Register[ Register_Set0, AF ] ) ) ;
            continue ;
        end ;
        if( ( A and $EF ) = $A ) then { LDAX or LD A,(BC)/LD A,(DE) }
        begin
            Increment_Clock( 7 ) ;
            if( ( A and $10 ) = $10 ) then
            begin
                A := Byte_Read( Register[ Register_Set1, DE ] ) ;
            end else
            begin
                A := Byte_Read( Register[ Register_Set1, BC ] ) ;
            end ;
            Register[ Register_Set0, AF ] :=
                swap( Hi_Register( Register_Set0, AF ) ) or A ;
            continue ;
        end ;
        if( ( A and $F8 ) = $80 ) then { ADD r or ADD A,r }
        begin
            Increment_Clock( 4 ) ;
            if Mode = Z80 then
            begin
                Reset_N ;
                Overflow := True ;
            end ;
            if( ( A and 7 ) = 6 ) then { ADD A,(HL) }
            begin
                Increment_Clock( 3 ) ;
            end ;
            A := Extract_Register( A, 1 ) ;
            Add( A, 0 ) ;
            continue ;
        end ;
        if( ( A and $F8 ) = $88 ) then { ADC r or ADC A,r or ADC A,(HL) }
        begin
            Increment_Clock( 4 ) ;
            if( Mode = Z80 ) then
            begin
                Reset_N ;
                Overflow := True ;
            end ;
            if( ( A and 7 ) = 6 ) then { ADC A,(HL) }
            begin
                Increment_Clock( 3 ) ;
            end ;
            A := Extract_Register( A, 1 ) ;
            if ( Is_C_Set ) then
            begin
                C := 1 ;
            end else
            begin
                C := 0 ;
            end ;
            Add( A, C ) ;
            continue ;
        end ;
        if( ( A and $F8 ) = $90 ) then { SUB r }
        begin
            if( ( A and 7 ) = 6 ) then
            begin
                Increment_Clock( 7 ) ;
            end else
            begin
                Increment_Clock( 4 ) ;
            end ;
            if( Mode = Z80 ) then
            begin
                Set_N ;
                Overflow := True ;
            end ;
            A := Extract_Register( A, 1 ) ; { Get byte for subtraction }
            Sub( A, 0 ) ;
            continue ;
        end ;
        if( ( A and $F8 ) = $98 ) then { SBB r or SBC A,r }
        begin
            if( ( A and 7 ) = 6 ) then { SBC A,(HL) }
            begin
                Increment_Clock( 7 ) ;
            end else
            begin
                Increment_Clock( 4 ) ;
            end ;
            if( Mode = Z80 ) then
            begin
                Set_N ;
                Overflow := True ;
            end ;
            A := Extract_Register( A, 1 ) ;
            if( Is_C_Set ) then
            begin
                C := 1 ;
            end else
            begin
                C := 0 ;
            end ;
            Sub( A, C ) ;
            continue ;
        end ;
        if( ( A and $F8 ) = $A0 ) then { ANA r or AND r }
        begin
            Increment_Clock( 4 ) ;
            if( ( A and 7 ) = 6 ) then { AND (HL) }
            begin
                Increment_Clock( 3 ) ;
            end ;
            A := Extract_Register( A, 1 ) ;
            Logical_Flags ;
            A := A and Lo( _Register[ Register_Set0, AF ] ) ;
            Register[ Register_Set0, AF ] := Swap( Hi( _Register[ Register_Set0, AF ] ) ) or A ;
            Flags( A ) ;
            continue ;
        end ;
        if( ( A and $F8 ) = $A8 ) then { XRA r or XOR r }
        begin
            if( ( A and 7 ) = 6 ) then { XOR (HL) }
            begin
                Increment_Clock( 7 ) ;
            end else
            begin
                Increment_Clock( 4 ) ;
            end ;
            A := Extract_Register( A, 1 ) ;
            Logical_Flags ;
            if( Mode = Z80 ) then
            begin
                Reset_C2 ;
            end ;
            A := A xor Lo( _Register[ Register_Set0, AF ] ) ;
            Register[ Register_Set0, AF ] :=
                swap( Hi( _Register[ Register_Set0, AF ] ) ) or A ;
            Flags( A ) ;
            continue ;
        end ;
        if( ( A and $F8 ) = $B0 ) then { ORA r or OR r }
        begin
            if( ( A and 7 ) = 6 ) then { OR (HL) }
            begin
                Increment_Clock( 7 ) ;
            end else
            begin
                Increment_Clock( 4 ) ;
            end ;
            A := Extract_Register( A, 1 ) ;
            Logical_Flags ;
            if( Mode = Z80 ) then
            begin
                Reset_C2 ;
            end ;
            A := A or lo( _Register[ Register_Set0, AF ] ) ;
            Register[ Register_Set0, AF ] :=
                swap( hi( _Register[ Register_Set0, AF ] ) ) or A ;
            Flags( A ) ;
            continue ;
        end ;
        if( ( A and $F8 ) = $B8 ) then { CMP r or CP r }
        begin
            Increment_Clock( 4 ) ;
            A := Extract_Register( A, 1 ) ;
            if( A = 6 ) then { CP (HL) }
            begin
                Increment_Clock( 3 ) ;
            end ;
            if( Mode = Z80 ) then
            begin
                Set_N ;
                Overflow := True
            end ;
            Ccmp( A ) ;
            continue ;
        end ;
        if( A = $32 ) then { STA n or LD (n),A }
        begin
            Increment_Clock( 13 ) ;
            A := Fetch_Word ; { Get immediate address }
            MemWrite( A, Chr( Lo_Register( Register_Set0, AF ) ) ) ;
            continue ;
        end ;
        if( A = $3A ) then { LDA n or LD A,(n) }
        begin
            Increment_Clock( 13 ) ;
            A := Fetch_Word ; { Get immediate address }
            Register[ Register_Set0, AF ] :=
                Byte_Read( A ) or swap( Hi( _Register[ Register_Set0, AF ] ) ) ;
            continue ;
        end ;
        if( A = $22 ) then { SHLD n or LD (n),HL }
        begin
            Increment_Clock( 16 ) ;
            A := Fetch_Word ; { Get address }
            MemWrite( A, chr( Register[ Register_Set1, HL ] ) + chr( Hi( _Register[ Register_Set1, HL ] ) ) ) ;
            continue ;
        end ;
        if( A = $2A ) then { LHLD n or LD HL,(n) }
        begin
            Increment_Clock( 16 ) ;
            A := Fetch_Word ; { Get address }
            Register[ Register_Set1, HL ] := Word_Read( A ) ;
            continue ;
        end ;
        if( ( A and $CF ) = 1 ) then { LXI rp or LD rp,n }
        begin
            Increment_Clock( 10 ) ;
            A := ( ( A div 16 ) and 3 ) + 1 ;
            B := Fetch_Word ; { Get data }
            if A = 4 then Sp := B else Register[ Register_Set1, A ] := B ;
            continue ;
        end ;
        if( ( A and $C7 ) = 6 ) then { MVI r or LD r,n }
        begin
            Increment_Clock( 7 ) ;
            if( A = $36 ) then { LD (HL), n }
            begin
                Increment_Clock( 3 ) ;
            end ;
            Set_Register( A, 8, Fetch ) ;
            continue ;
        end ;
        if A = $C6 then { ADI n or ADD A,n }
        begin
            Increment_Clock( 7 ) ;
            A := Fetch ; {Get data}
            if Mode = Z80 then
            begin
                Reset_N ;
                Overflow := True
            end ;
            Add( A, 0 ) ;
            continue ;
        end ;
        if( A = $CE ) then { ACI n or ADC A,n }
        begin
            Increment_Clock( 7 ) ;
            A := Fetch ;
            if Mode = Z80 then
            begin
                Reset_N ;
                Overflow := True
            end ;
            if ( Is_C_Set ) then C := 1 else C := 0 ;
            Add( A, C ) ;
            continue ;
        end ;
        if( A = $D6 ) then { SUI n or SUB n }
        begin
            Increment_Clock( 7 ) ;
            A := Fetch ; { Get byte to subtract }
            if( Mode = Z80 ) then
            begin
                Set_N ;
                Overflow := True ;
            end ;
            Sub( A, 0 ) ;
            continue ;
        end ;
        if( A = $DE ) then { SBI n or SBC A,n }
        begin
            Increment_ClocK( 7 ) ;
            A := Fetch ;
            if( Mode = Z80 ) then
            begin
                Set_N ;
                Overflow := True ;
            end ;
            if( Is_C_Set ) then
            begin
                C := 1 ;
            end else
            begin
                C := 0 ;
            end ;
            Sub( A, C ) ;
            continue ;
        end ;
        if( A = $E6 ) then { ANI n or AND n }
        begin
            Increment_Clock( 7 ) ;
            A := Fetch ;
            Logical_Flags ;
            if Mode = Z80 then Set_C2 ;
            A := A and Lo( _Register[ Register_Set0, AF ] ) ;
            Register[ Register_Set0, AF ] := Swap( Hi( _Register[ Register_Set0, AF ] ) ) or A ;
            Flags( A ) ;
            continue ;
        end ;
        if( A = $EE ) then { XRI n or XOR n }
        begin
            Increment_Clock( 7 ) ;
            A := Fetch ;
            Logical_Flags ;
            if( Mode = Z80 ) then
            begin
                Reset_C2 ;
            end ;
            A := A xor Lo( _Register[ Register_Set0, AF ] ) ;
            Register[ Register_Set0, AF ] :=
                swap( Hi( _Register[ Register_Set0, AF ] ) ) or A ;
            Flags( A ) ;
            continue ;
        end ;
        if( A = $F6 ) then { ORI n or OR n }
        begin
            Increment_Clock( 7 ) ;
            A := Fetch ;
            Logical_Flags ;
            if( Mode = Z80 ) then
            begin
                Reset_C2 ;
            end ;
            A := A or lo( _Register[ Register_Set0, AF ] ) ;
            Register[ Register_Set0, AF ] :=
                swap( hi( _Register[ Register_Set0, AF ] ) ) or A ;
            Flags( A ) ;
            continue ;
        end ;
        if( A = $FE ) then { CPI n or CP n }
        begin
            Increment_Clock( 7 ) ;
            A := Fetch ;
            if Mode = Z80 then
            begin
                Set_N ;
                Overflow := True
            end ;
            Ccmp( A ) ;
            continue ;
        end ;
        if( A = $E9 ) then { PCHL or JP (HL) }
        begin
            Increment_Clock( 4 ) ;
            _Run_Stream := nil ;
            Pc := Register[ Register_Set1, HL ] ;
            continue ;
        end ;
        if( A = $C3 ) then { JMP n or JP n }
        begin
            Increment_Clock( 10 ) ;
            _Run_Stream := nil ;
            Pc := Fetch_Word ;
            continue ;
        end ;
        if( ( A and $C7 ) = $C2 ) then { JCC n or JP cc,n }
        begin
            Increment_Clock( 10 ) ;
            B := Fetch_Word ;
            A := ( A div 8 ) and 7 ;
            C := Register[ Register_Set0, AF ] ;
            case A of
                 0 : {JNZ n or JP NZ,n}
                     if ( C and 16384 ) = 16384 then continue ;
                 1 : {JZ n or JP Z,n}
                     if ( C and 16384 ) = 0 then continue ;
                 2 : {JNC n or JP NC,n}
                     if ( C and 256 ) = 256 then continue ;
                 3 : {JC n or JP C,n}
                     if ( C and 256 ) = 0 then continue ;
                 4 : {JPO n or JP PO,n}
                     if ( C and 1024 ) = 1024 then continue ;
                 5 : {JPE n or JP PE,n}
                     if ( C and 1024 ) = 0 then continue ;
                 6 : {JP n or JP P,n}
                     if ( C and 32768 ) = 32768 then continue ;
                 7 : {JM n or JP M,n}
                     if ( C and 32768 ) = 0 then continue ;
            end ;
            _Run_Stream := nil ;
            PC := B ; { Do jump }
            continue ;
        end ;
        if( A = $CD ) then { CALL n }
        begin
            Increment_Clock( 17 ) ;
            A := Fetch_Word ;
            Push_PC ;
            _Run_Stream := nil ;
            Pc := A ;
            if( not Into ) then
            begin
                inc( Nest_Level ) ;
            end ;
            continue ;
        end ;
        if( ( A and $C7 ) = $C4 ) then { Ccc n or CALL cc,n }
        begin
            Increment_Clock( 10 ) ;
            B := Fetch_Word ;
            A := ( A div 8 ) and 7 ;
            C := Register[ Register_Set0, AF ] ;
            if( ( A = 0 ) or ( A = 2 ) ) then
            begin
                Increment_Clock( 7 ) ;
            end ;
            case A of
                 0 : {CNZ n or CALL NZ,n}
                     if ( C and 16384 ) = 16384 then continue ;
                 1 : {CZ n or CALL Z,n}
                     if ( C and 16384 ) = 0 then continue ;
                 2 : {CNC n or CALL NC,n}
                     if ( C and 256 ) = 256 then continue ;
                 3 : {CC n or CALL C,n}
                     if ( C and 256 ) = 0 then continue ;
                 4 : {CPO n or CALL PO,n}
                     if ( C and 1024 ) = 1024 then continue ;
                 5 : {CPE n or CALL PE,n}
                     if ( C and 1024 ) = 0 then continue ;
                 6 : {CP n or CALL P,n}
                     if ( C and 32768 ) = 32768 then continue ;
                 7 : {CM n or CALL M,n}
                     if ( C and 32768 ) = 0 then continue ;
            end ;
            Push_PC ;
            _Run_Stream := nil ;
            Pc := B ;
            if( not Into ) then
            begin
                inc( Nest_Level ) ;
            end ;
            continue ;
        end ;
        if( A = $C9 ) then { RET }
        begin
            Increment_Clock( 10 ) ;
            _Run_Stream := nil ;
            PC := Pop ;
            PC := PC or swap( Pop ) ;
            if( not Into ) then
            begin
                dec( Nest_Level ) ;
            end ;
            continue ;
        end ;
        if( ( A and $C7 ) = $C0 ) then { RC or RET c }
        begin
            Increment_Clock( 5 ) ;
            A := ( A div 8 ) and 7 ;
            C := Register[ Register_Set0, AF ] ;
            case A of
                 0 : { RNZ or RET NZ }
                     if ( C and 16384 ) = 16384 then continue ;
                 1 : { RZ or RET Z }
                     if ( C and 16384 ) = 0 then continue ;
                 2 : { RNC or RET NC }
                     if ( C and 256 ) = 256 then continue ;
                 3 : { RC or RET C }
                     if ( C and 256 ) = 0 then continue ;
                 4 : { RPO or RET PO }
                     if ( C and 1024 ) = 1024 then continue ;
                 5 : { RPE or RET PE }
                     if ( C and 1024 ) = 0 then continue ;
                 6 : { RP or RET P }
                     if ( C and 32768 ) = 32768 then continue ;
                 7 : { RM or RET M }
                     if ( C and 32768 ) = 0 then continue ;
            end ;
            Increment_Clock( 6 ) ;
            _Run_Stream := nil ;
            Pc := Pop ;
            PC := PC or swap( Pop ) ;
            if( not Into ) then
            begin
                dec( Nest_Level ) ;
            end ;
            continue ;
        end ;
        if( Mode = M8080 ) then
        begin
            goto Unknown_OpCode ; { Not an 8080 op-code }
        end ;
        if( Mode = M8085 ) then
        begin
            if( ( A = $20 ) or ( A = $30 ) ) then { RIM and SIM }
            begin
                Increment_Clock( 1 ) ;
                if( A = $20 ) then { RIM }
                begin
                    A := 0 ;
                    if( RST5_Enabled ) then
                    begin
                        A := A or 1 ;
                    end ;
                    if( RST6_Enabled ) then
                    begin
                        A := A or 2 ;
                    end ;
                    if( RST7_Enabled ) then
                    begin
                        A := A or 4 ;
                    end ;
                    if( Interrupts ) then
                    begin
                        A := A or 8 ;
                    end ;
                    if( RST5_State ) then
                    begin
                        A := A or 16 ;
                    end ;
                    if( RST6_State ) then
                    begin
                        A := A or 32 ;
                    end ;
                    if( RST7_Pending ) then
                    begin
                        A := A or 64 ;
                    end ;
                    if( Serial_Input_State ) then
                    begin
                        A := A OR 128 ;
                    end ;
                    Register_A := A ;
                end else
                begin
                    if( ( Register_A and 8 ) = 8 ) then // Mask Set Enable
                    begin
                        RST5_Enabled := ( ( Register_A and 1 ) <> 0 ) ;
                        RST6_Enabled := ( ( Register_A and 2 ) <> 0 ) ;
                        RST7_Enabled := ( ( Register_A and 4 ) <> 0 ) ;
                    end ;
                    if( ( Register_A and 16 ) <> 0 ) then
                    begin
                        RST7_Pending := False ;
                    end ;
                    if( ( Register_A and 64 ) <> 0 ) then
                    begin
                        Serial_Output_State := ( ( Register_A and 128 ) <> 0 ) ;
                        Send_Signal( 'SOD', Serial_Output_State ) ;
                    end ;
                end ;
                continue ;
            end ;
            if( A = $08 ) then { DSUB B }
            begin
                Increment_Clock( 1 ) ;
                Lng := Register[ Register_Set0, HL ] ;
                Lng := Lng - Register[ Register_Set0, BC ] ;
                if( _Register[ Register_Set0, HL ] > _Register[ Register_Set0, BC ] ) then
                begin
                    Set_D ;
                end else
                begin
                    Reset_D ;
                end ;
                Register[ Register_Set0, HL ] := Lng ;
                continue ;
            end ;
            if( A = $10 ) then { RHR }
            begin
                Increment_Clock( 1 ) ;
                if( ( _Register[ Register_Set0, HL ] and 1 ) = 1 ) then
                begin
                    Set_C ;
                end else
                begin
                    Reset_C ;
                end ;
                Register[ Register_Set0, HL ] :=
                    _Register[ Register_Set0, HL ] shr 1 ;
                continue ;
            end ;
            if( A = $18 ) then { RDL }
            begin
                Increment_Clock( 1 ) ;
                A := ( _Register[ Register_Set0, AF ] shr 8 ) and 1 ; { Carry bit }
                if( _Register[ Register_Set0, DE ] < 0 ) then
                begin
                    Set_C ;
                    Set_D ;
                end else
                begin
                    Register[ Register_Set0, AF ] :=
                        _Register[ Register_Set0, AF ] and ( not 8193 ) ;
                    Flags_Changed := True ;
                end ;
                Register[ Register_Set0, DE ] :=
                    ( _Register[ Register_Set0, DE ] shl 1 ) or A ;
                continue ;
            end ;
            if( A = $28 ) then { DMOV D,H }
            begin
                Increment_Clock( 1 ) ;
                Register[ Register_Set0, DE ] :=
                    Register[ Register_Set0, HL ] + Fetch ;
                continue ;
            end ;
            if( A = $38 ) then { DMOV D, SP }
            begin
                Increment_Clock( 1 ) ;
                Register[ Register_Set0, DE ] := SP + Fetch ;
                continue ;
            end ;
            if( A = $CB ) then { RSTV }
            begin
                Increment_Clock( 1 ) ;
                if( Is_D_Set ) then
                begin
                    _Run_Stream := nil ;
                    Push_PC ;
                    PC := $40 ;
                    if( not Into ) then
                    begin
                        inc( Nest_Level ) ;
                    end ;
                end ;
                continue ;
            end ;
            if( A = $D9 ) then { SHLX }
            begin
                Increment_Clock( 1 ) ;
                ByteWrite( _Register[ Register_Set0, DE ],
                    lo( _Register[ Register_Set0, HL ] ) ) ;
                ByteWrite( Register[ Register_Set0, DE ] + 1,
                    Hi_Register( Register_Set0, HL ) ) ;
                continue ;
            end ;
            if( A = $DD ) then { JND }
            begin
                Increment_Clock( 10 ) ;
                B := Fetch_Word ;
                if( Is_D_Set ) then
                begin
                    continue ;
                end ;
                _Run_Stream := nil ;
                PC := B ; { Do jump }
                continue ;
            end ;
            if( A = $ED ) then { LHLX }
            begin
                Increment_Clock( 1 ) ;
                Register[ Register_Set0, HL ] :=
                    Byte_Read( Register[ Register_Set0, DE ] ) or
                    swap( Byte_Read( _Register[ Register_Set0, DE ] + 1 ) ) ;
                continue ;
            end ;
            if( A = $FD ) then { JD }
            begin
                Increment_Clock( 10 ) ;
                B := Fetch_Word ;
                if( Is_D_Set ) then
                begin
                    continue ;
                end ;
                _Run_Stream := nil ;
                PC := B ; { Do jump }
                continue ;
            end ;
            goto Unknown_OpCode ; { Not an 8085 op-code }
        end ;

        if( Execute_Z80( A ) ) then
        begin
            continue ;
        end ;

Unknown_OpCode:
        Signal_Exception( '', 0 ) ; // Invalid instruction
    end ; // while( True )
end ; // TZ80_CPU.Execute


procedure TZ80_CPU.Output( Port, Value : integer ) ;

var Component : TComponent ;
    Loop : integer ;

begin
    try
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
            Component.Write( Port, Value, 8, IO_Type_IO ) ;
        end ;
    finally
        if( _Run_Stream = nil ) then
        begin
            if( ( _Port_Watchpoints[ Port ] and Access_Output ) <> 0 ) then
            begin
                Watchpoint_Notice( Port, Access_Output, 0, False, False, True ) ;
            end ;
            State_Change_Notice( State_Port_Output, True ) ;
            if( _Profiling ) then
            begin
                TZ80_Profiler( Parent.Profiler ).Increment( Domain_Port_Outputs, Port ) ;
            end ;
            Log_Trace( 'Output ' + inttostr( Value ) + '. to port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Input( Port : integer ) : integer ;

var Component : TComponent ;
    Loop : integer ;

begin
    Result := 0 ;
    try
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Port, 8, IO_Type_IO ) ) then
            begin
                exit ;
            end ;
        end ;
    finally
        Result := Memory_Data_Latch ;
        if( _Run_Stream = nil ) then
        begin
            if( ( _Port_Watchpoints[ Port ] and Access_Input ) <> 0 ) then
            begin
                Watchpoint_Notice( Port, Access_Input, 0, False, False, True ) ;
            end ;
            State_Change_Notice( State_Port_Input, True ) ;
            if( _Profiling ) then
            begin
                TZ80_Profiler( Parent.Profiler ).Increment( Domain_Port_Inputs, Port ) ;
            end ;
            Log_Trace( 'Input ' + inttostr( Result ) + '. from port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Lo_Register( I1, I2 : integer ) : byte ;

var _I : integer ;

begin
    Result := Lo( _Register[ I1, I2 ] ) ;
    if( _Run_Stream = nil ) then
    begin
        _I := 6 + I2 * 2 ;
        if( I1 = 1 ) then
        begin
            _I := _I + 12 ;
        end ;
        if( ( _Register_Watchpoints[ _I ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( _I, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Hi_Register( I1, I2 : integer ) : byte ;

var _I : integer ;

begin
    Result := Hi( _Register[ I1, I2 ] ) ;
    if( _Run_Stream = nil ) then
    begin
        _I := 6 + I2 * 2 + 1 ;
        if( I1 = 1 ) then
        begin
            _I := _I + 12 ;
        end ;
        if( ( _Register_Watchpoints[ _I ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( _I, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_A : integer ;

begin
    Result := Lo( _Register[ Register_Set0, AF ] ) ;
    if( Register_Set1 = 0 ) then
    begin
        _I := 6 ;
    end else
    begin
        _I := 18 ;
    end ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ _I ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( _I, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_A( Value : integer ) ;

var _I : integer ;

begin
    if( Register_Set1 = 0 ) then
    begin
        _I := 6 ;
    end else
    begin
        _I := 18 ;
    end ;
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( _I, Value ) ;
    end ;
    _Register[ Register_Set0, AF ] :=
        ( ( _Register[ Register_Set0, AF ] and ( not $FF ) ) or Value ) and $FF ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ _I ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( _I, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_C : integer ;

var _I : integer ;

begin
    Result := Lo( _Register[ Register_Set1, BC ] ) ;
    if( Register_Set1 = 0 ) then
    begin
        _I := 9 ;
    end else
    begin
        _I := 21 ;
    end ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ _I ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( _I, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_C( Value : integer ) ;

var _I : integer ;

begin
    if( Register_Set1 = 0 ) then
    begin
        _I := 9 ;
    end else
    begin
        _I := 21 ;
    end ;
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( _I, Value ) ;
    end ;
    _Register[ Register_Set1, BC ] :=
        ( ( _Register[ Register_Set1, BC ] and ( not $FF ) ) or Value ) and $FF ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ _I ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( _I, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_BC : integer ;

begin
    Result := Register[ Register_Set1, BC ] ;
end ;


procedure TZ80_CPU.Set_BC( Value : integer ) ;

begin
    Register[ Register_Set1, BC ] := Value ;
end ;


function TZ80_CPU.Get_HL : integer ;

begin
    Result := Register[ Register_Set1, HL ] ;
end ;


procedure TZ80_CPU.Set_HL( Value : integer ) ;

begin
    Register[ Register_Set1, HL ] := Value ;
end ;


function TZ80_CPU.Get_I : integer ;

begin
    Result := _I ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 26 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 26, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_I( Value : integer ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 26, Value ) ;
    end ;
    _I := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 26 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 26, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_IM : byte ;

begin
    Result := _IM ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 28 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 28, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_IM( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 28, Value ) ;
    end ;
    _IM := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 28 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 28, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_IX : integer ;

begin
    Result := _IX ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 29 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 29, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_IX( Value : integer ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 29, Value ) ;
    end ;
    _IX := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 29 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 29, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_IY : integer ;

begin
    Result := _IY ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 30 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 30, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_IY( Value : integer ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 30, Value ) ;
    end ;
    _IY := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 30 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 30, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_PC : word ;

begin
    Result := _PC ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 0 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 0, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_PC( Value : word ) ;

begin
    _PC := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 0 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 0, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_R : integer ;

begin
    Result := _R ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 27 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 27, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_R( Value : integer ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 27, Value ) ;
    end ;
    _R := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 27 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 27, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_SP : integer ;

begin
    Result := _SP ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 1 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 1, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_SP( Value : integer ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 1, Value ) ;
    end ;
    _SP := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 1 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 1, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Get_Register( _Set, Index : integer ) : integer ;

var I : integer ;

begin
    Result := _Register[ _Set, Index ] ;
    I := Register_To_Breakpoint( _Set, Index ) ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ I ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( I, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TZ80_CPU.Set_Register( _Set, Index : integer ; Value : integer ) ;

var _I : integer ;
    Old : integer ;

begin
    Old := _Register[ _Set, Index ] ;
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( Index, Value ) ;
    end ;
    _Register[ _Set, Index ] := Value and $FFFF ;
    if( ( _Run_Stream = nil ) and ( Old <> Value  ) ) then
    begin
        _I := Register_To_Breakpoint( _Set, Index ) ;
        if( ( _Register_Watchpoints[ _I ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( _I, Access_Write, 0, False, True, False ) ;
        end ;
        if( lo( Value ) <> lo( Old ) ) then
        begin
            _I := 6 + Index * 2 ;
            if( _Set = 1 ) then
            begin
                _I := _I + 12 ;
            end ;
            if( ( _Register_Watchpoints[ _I ] and Access_Write ) <> 0 ) then
            begin
                Watchpoint_Notice( _I, Access_Write, 0, False, True, False ) ;
            end ;
        end ;
        if( hi( Value ) <> hi( Old ) ) then
        begin
            _I := 6 + Index * 2 + 1 ;
            if( _Set = 1 ) then
            begin
                _I := _I + 12 ;
            end ;
            if( ( _Register_Watchpoints[ _I ] and Access_Write ) <> 0 ) then
            begin
                Watchpoint_Notice( _I, Access_Write, 0, False, True, False ) ;
            end ;
        end ;
    end ;
end ; // TZ80_CPU.Set_Register


function TZ80_CPU.Set_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

begin
    if( ( Address < 0 ) or ( Address > $FFFF ) ) then
    begin
        Result := Parent.Set_Error( Z80_Invalid_Address ) ;
        exit ;
    end ;
    if( _Breakpoints.Indexof( Address ) <> -1 ) then
    begin
        Result := Parent.Set_Error( Z80_Breakpoint_Exists ) ;
        exit ;
    end ;
    _Breakpoints.Add( Address ) ;
    Result := Parent.Set_Error( 0 ) ;
end ;


procedure TZ80_CPU.Set_Clock_Speed( Value : longint ) ;

begin
    _Speed := Value ;
end ;


procedure TZ80_CPU.Step( Into : boolean ) ;

begin
    _Halted := False ;
    Execute( True, Into ) ;
end ;


function TZ80_CPU.Translate( Space : integer ; Address : int64 ) : int64 ;

begin
    Translate := Address ;
end ;


function TZ80_CPU.Default_Base : integer ;

begin
    Result := 16 ;
end ;


function TZ80_CPU.Get_Low_Memory : int64 ;

begin
    Result := 0 ;
end ;


function TZ80_CPU.Get_High_Memory : int64 ;

begin
    Result := 65535 ;
end ;


function TZ80_CPU.Get_Low_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TZ80_CPU.Get_High_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TZ80_CPU.Get_Low_Port : int64 ;

begin
    Result := 0 ;
end ;


function TZ80_CPU.Get_High_Port : int64 ;

begin
    Result := 255 ;
end ;


function TZ80_CPU.Support_Virtual_Address : boolean ;

begin
    Result := False ;
end ;


function TZ80_CPU.Segment_Size( Index : integer ) : integer ;

begin
    if( ( Index < 0 ) or ( Index >= Segments.Count ) ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := Segments[ Index ] ;
    end ;
end ;


function TZ80_CPU.Register_Name( Index : integer ) : PChar ;

begin
    case Index of
        0 : Temp_Register_Name := 'PC' ;
        1 : Temp_Register_Name := 'SP' ;
        2 : Temp_Register_Name := 'AF' ;
        3 : Temp_Register_Name := 'BC' ;
        4 : Temp_Register_Name := 'DE' ;
        5 : Temp_Register_Name := 'HL' ;
        6 : Temp_Register_Name := 'A' ;
        7 : Temp_Register_Name := 'F' ;
        8 : Temp_Register_Name := 'B' ;
        9 : Temp_Register_Name := 'C' ;
        10 : Temp_Register_Name := 'D' ;
        11 : Temp_Register_Name := 'E' ;
        12 : Temp_Register_Name := 'H' ;
        13 : Temp_Register_Name := 'L' ;
        14 : Temp_Register_Name := 'AF'#39 ;
        15 : Temp_Register_Name := 'BC'#39 ;
        16 : Temp_Register_Name := 'DE'#39 ;
        17 : Temp_Register_Name := 'HL'#39 ;
        18 : Temp_Register_Name := 'A'#39 ;
        19 : Temp_Register_Name := 'F'#39 ;
        20 : Temp_Register_Name := 'B'#39 ;
        21 : Temp_Register_Name := 'C'#39 ;
        22 : Temp_Register_Name := 'D'#39 ;
        23 : Temp_Register_Name := 'E'#39 ;
        24 : Temp_Register_Name := 'H'#39 ;
        25 : Temp_Register_Name := 'L'#39 ;
        26 : Temp_Register_Name := 'I' ;
        27 : Temp_Register_Name := 'R' ;
        28 : Temp_Register_Name := 'IM' ;
        29 : Temp_Register_Name := 'IX' ;
        30 : Temp_Register_Name := 'IY' ;
        else Temp_Register_Name := '' ; // Invalid index
    end ;
    Result := PChar( Temp_Register_Name ) ;
end ;


function TZ80_CPU.Register_Size( Index : integer ) : integer ;

begin
    case Index of
        0 : Result := 16 ; // PC
        1 : Result := 16 ; // SP
        2 : Result := 16 ; // AF
        3 : Result := 16 ; // BC
        4 : Result := 16 ; // DE
        5 : Result := 16 ; // HL
        6 : Result := 8 ; // A
        7 : Result := 8 ; // F
        8 : Result := 8 ; // B
        9 : Result := 8 ; // C
        10 : Result := 8 ; // D
        11 : Result := 8 ; // E
        12 : Result := 8 ; // H
        13 : Result := 8 ; // L
        14 : Result := 16 ; // AF'
        15 : Result := 16 ; // BC'
        16 : Result := 16 ; // DE'
        17 : Result := 16 ; // HL'
        18 : Result := 8 ; // A'
        19 : Result := 8 ; // F'
        20 : Result := 8 ; // B'
        21 : Result := 8 ; // C'
        22 : Result := 8 ; // D'
        23 : Result := 8 ; // E'
        24 : Result := 8 ; // H'
        25 : Result := 8 ; // L'
        26 : Result := 16 ; // I
        27 : Result := 16 ; // R
        28 : Result := 8 ; // IM
        29 : Result := 16 ; // IX
        30 : Result := 16 ; // IY
        else Result := 0 ; // Invalid index
    end ;
end ;


function TZ80_CPU.Register_Description( Index : integer ) : PChar ;

begin
    Temp_Register_Description := '' ;
    if( Index = 7 ) then
    begin
        Temp_Register_Description := Get_Mask( hi( _Register[ Register_Set0, AF ] ) ) ;
    end else
    if( ( Index = 19 ) and ( Mode = Z80 ) ) then
    begin
        Temp_Register_Description := Get_Mask( hi( _Register[ Register_Set1, AF ] ) ) ;
    end ;
    Result := PChar( Temp_Register_Description ) ;
end ;


function TZ80_CPU.Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := Parent.Set_Error( Z80Err_No_Cache ) ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 30 ) ) then
        begin
            Result := Parent.Set_Error( Z80Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] := _Register_Watchpoints[ Address ] or Access ;
            Result := Parent.Set_Error( 0 ) ;
        end ;
    end ;
end ;


function TZ80_CPU.Top_Of_Stack( Index : integer ) : int64 ;

begin
    Top_Of_Stack := SP ;
end ;


function TZ80_CPU.Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;

begin
    _RTS := RTS ;
    _RTS_Flags := Flags ;
    Result := nil ;
end ;


function TZ80_CPU.Get_Current_Address( Space : integer ; Physical : boolean ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := PC ;
    end else
    begin
        Result := 0 ;
    end ;
end ;


procedure TZ80_CPU.Set_Current_Address( Space : integer ; Physical : boolean ; Value : int64 ) ;

begin
    if( Space = 0 ) then
    begin
        _PC := Value ;
    end ;
end ;


procedure TZ80_CPU.Stop ;

begin
    Stopping := True ;
end ;


procedure TZ80_CPU.Restart ;

begin
    _Register[ 0, 0 ] := 512 ;
    _Register[ 1, 0 ] := 512 ;
    _PC := 0 ;
    _R := 0 ;
    _I := 0 ;
    Interrupts := False ;
    _IM := 0 ;
    In_NMI := False ;
    _Run_Stream := nil ;
    RST5_Enabled := True ;
    RST6_Enabled := True ;
    RST7_Enabled := True ;
    RST7_Pending := False ;
    RST5_State := False ;
    RST6_State := False ;
    RST7_State := False ;
end ;


function TZ80_CPU.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var Dummy : integer ;
    Loop, Loop1 : integer ;
    Parser, Watchpoint_Parser : TXML_Parser ;
    S : string ;

begin
    Result := Set_Error( 0 ) ;

    // Setup default state...
    Overflow := False ;
    IFF2 := False ;
    In_NMI := False ;
    Interrupts := False ;
    _Halted := False ;
    _Profiling := False ;
    Serial_Input_State := False ;
    Serial_Output_State := False ;
    Interrupt_Instruction := False ;
    RST5_Enabled := False ;
    RST6_Enabled := False ;
    RST7_Enabled := False ;
    RST7_Pending := False ;
    RST5_State := False ;
    RST6_State := False ;
    RST7_State := False ;
    fillchar( _Register_Watchpoints, sizeof( _Register_Watchpoints ), 0 ) ;
    _Breakpoints.Clear ;
    Clear_Watchpoints ;

    // Parse the image...
    Parser := TXML_Parser.Create ;
    Dummy := Stream.Size ;
    setlength( S, Dummy ) ;
    Stream.Read( S[ 1 ], Dummy ) ;
    Parser.Set_Source( PChar( S ) ) ;
    while( not Parser.EOF ) do
    begin
        S := uppercase( Parser.Token ) ;
        if( copy( S, 1, 1 ) = '<' ) then
        begin
            if( S = '<BASE>' ) then
            begin
                S := Parser.Get_Section( 'base' ) ;
                try
                    Base := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<TRACE/>' ) then
            begin
                _Trace := True ;
            end else
            if( S = '<OVERFLOW/>' ) then
            begin
                Overflow := True ;
            end else
            if( S = '<I>' ) then
            begin
                S := Parser.Get_Section( 'i' ) ;
                try
                    _I := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<IFF2/>' ) then
            begin
                IFF2 := True ;
            end else
            if( S = '<IM>' ) then
            begin
                S := Parser.Get_Section( 'im' ) ;
                try
                    IM := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<IN_NMI/>' ) then
            begin
                In_NMI := True ;
            end else
            if( S = '<INTERRUPTS/>' ) then
            begin
                Interrupts := True ;
            end else
            if( S = '<IX>' ) then
            begin
                S := Parser.Get_Section( 'ix' ) ;
                try
                    _IX := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<IY>' ) then
            begin
                S := Parser.Get_Section( 'iy' ) ;
                try
                    _IY := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<MODE>' ) then
            begin
                try
                    Mode := TMode( strtoint( S ) ) ;
                except
                end ;
            end else
            if( S = '<R>' ) then
            begin
                S := Parser.Get_Section( 'r' ) ;
                try
                    _R := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<SP>' ) then
            begin
                S := Parser.Get_Section( 'sp' ) ;
                try
                    _SP := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<PC>' ) then
            begin
                S := Parser.Get_Section( 'pc' ) ;
                try
                    _PC := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<SPEED>' ) then
            begin
                S := Parser.Get_Section( 'speed' ) ;
                try
                    _Speed := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<HALTED/>' ) then
            begin
                _Halted := True ;
            end else
            if( S = '<PROFILING/>' ) then
            begin
                _Profiling := True ;
            end else
            if( S = '<REGISTER_SET0>' ) then
            begin
                S := Parser.Get_Section( 'Register_Set0' ) ;
                try
                    Register_Set0 := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<REGISTER_SET1>' ) then
            begin
                S := Parser.Get_Section( 'Register_Set1' ) ;
                try
                    Register_Set1 := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<SERIAL_INPUT_STATE/>' ) then
            begin
                Serial_Input_State := True ;
            end else
            if( S = '<SERIAL_OUTPUT_STATE/>' ) then
            begin
                Serial_Output_State := True ;
            end else
            if( S = '<INTERRUPT_INSTRUCTION/>' ) then
            begin
                Interrupt_Instruction := True ;
            end else
            if( S = '<RST5_ENABLED/>' ) then
            begin
                RST5_Enabled := True ;
            end else
            if( S = '<RST6_ENABLED/>' ) then
            begin
                RST6_Enabled := True ;
            end else
            if( S = '<RST7_ENABLED/>' ) then
            begin
                RST7_Enabled := True ;
            end else
            if( S = '<RST7_PENDING/>' ) then
            begin
                RST7_Pending := True ;
            end else
            if( S = '<RST5_STATE/>' ) then
            begin
                RST5_State := True ;
            end else
            if( S = '<RST6_STATE/>' ) then
            begin
                RST6_State := True ;
            end else
            if( S = '<RST7_STATE/>' ) then
            begin
                RST7_State := True ;
            end else
            if( S = '<REGISTERS>' ) then
            begin
                S := Parser.Get_Section( 'Registers' ) ;
                S := copy( S, 2, length( S ) ) ; // Trim first bar
                Loop := 0 ;
                Loop1 := 0 ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( '|', S ) ;
                    try
                        _Register[ Loop, Loop1 ] :=
                            strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop1 ) ;
                    if( Loop1 = 4 ) then
                    begin
                        Loop1 := 0 ;
                        inc( Loop ) ;
                        if( Loop > 1 ) then
                        begin
                            break ;
                        end ;
                    end ;
                end ;
            end else
            if( S = '<BREAKPOINTS>' ) then
            begin
                S := Parser.Get_Section( 'Breakpoints' ) ;
                _Breakpoints.DeSerialize( S ) ;
            end else
            if( S = '<REGISTER_WATCHPOINTS>' ) then
            begin
                S := Parser.Get_Section( 'Register_Watchpoints' ) ;
                S := copy( S, 2, length( S ) ) ; // Trim first bar
                Loop := 0 ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( '|', S ) ;
                    try
                        _Register_Watchpoints[ Loop ] := strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop ) ;
                    if( Loop > 30 ) then
                    begin
                        break ;
                    end ;
                end ;
            end else
            if( S = '<MEMORY_WATCHPOINTS>' ) then
            begin
                _Memory_Watchpoints.Deserialize( PChar( Parser.Get_Section( 'Memory_watchpoints' ) ) ) ;
            end else
            if( S = '<PORT_WATCHPOINTS>' ) then
            begin
                Watchpoint_Parser := TXML_Parser.Create ;
                try
                    S := Parser.Get_Section( 'Memory_watchpoints' ) ;
                    Loop := 0 ;
                    while( length( S ) > 0 ) do
                    begin
                        Dummy := pos( '|', S + '|' ) ;
                        try
                            _Port_Watchpoints[ Loop ] :=
                                strtoint( copy( S, 1, Dummy - 1 ) ) ;
                        except
                        end ;
                        inc( Loop ) ;
                        S := copy( S, Dummy + 1, length( S ) ) ;
                    end ;
                finally
                    Watchpoint_Parser.Free ;
                end ;
            end else
            begin
                if( copy( S, length( S ) - 1, 2 ) <> '/>' ) then
                begin
                    Parser.Get_Section( PChar( S ) ) ;
                end ;
            end ;
        end ; // if( copy( S, 1, 1 ) = '<' )
    end ; // while( not Parser.EOF )
    Parser.Free ;
end ; // TZ80_CPU.Restore_State


function TZ80_CPU.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

    procedure Output( S : string ) ;

    begin
        Stream.Write( S[ 1 ], length( S ) ) ;
    end ;

var Loop, Loop1 : integer ;

begin
    Result := Set_Error( 0 ) ;
    Output( '<base>' + inttostr( Base ) + '</base>' ) ;
    if( Overflow ) then
    begin
        Output( '<overflow/>' ) ;
    end ;
    if( _Trace ) then
    begin
        Output( '<trace/>' ) ;
    end ;
    Output( '<i>' + inttostr( I ) + '</i>' ) ;
    if( IFF2 ) then
    begin
        Output( '<IFF2/>' ) ;
    end ;
    Output( '<im>' + inttostr( IM ) + '</im>' ) ;
    if( In_NMI ) then
    begin
        Output( '<In_NMI/>' ) ;
    end ;
    if( Interrupts ) then
    begin
        Output( '<Interrupts/>' ) ;
    end ;
    Output( '<ix>' + inttostr( IX ) + '</ix>' ) ;
    Output( '<iy>' + inttostr( IY ) + '</iy>' ) ;
    Output( '<mode>' + inttostr( integer( Mode ) ) + '</mode>' ) ;
    Output( '<r>' + inttostr( R ) + '</r>' ) ;
    Output( '<sp>' + inttostr( SP ) + '</sp>' ) ;
    Output( '<pc>' + inttostr( PC ) + '</pc>' ) ;
    Output( '<speed>' + inttostr( _Speed ) + '</speed>' ) ;
    if( _Halted ) then
    begin
        Output( '<Halted/>' ) ;
    end ;
    if( _Profiling ) then
    begin
        Output( '<Profiling/>' ) ;
    end ;
    Output( '<Register_Set0>' + inttostr( Register_Set0 ) + '</Register_Set0>' ) ;
    Output( '<Register_Set1>' + inttostr( Register_Set1 ) + '</Register_Set1>' ) ;
    if( Serial_Input_State ) then
    begin
        Output( '<Serial_Input_State/>' ) ;
    end ;
    if( Serial_Output_State ) then
    begin
        Output( '<Serial_Output_State/>' ) ;
    end ;
    if( Interrupt_Instruction ) then
    begin
        Output( '<Interrupt_Instruction/>' ) ;
    end ;
    if( RST5_Enabled ) then
    begin
        Output( '<RST5_Enabled/>' ) ;
    end ;
    if( RST6_Enabled ) then
    begin
        Output( '<RST6_Enabled/>' ) ;
    end ;
    if( RST7_Enabled ) then
    begin
        Output( '<RST7_Enabled/>' ) ;
    end ;
    if( RST7_Pending ) then
    begin
        Output( '<RST7_Pending/>' ) ;
    end ;
    if( RST5_State ) then
    begin
        Output( '<RST5_State/>' ) ;
    end ;
    if( RST6_State ) then
    begin
        Output( '<RST6_State/>' ) ;
    end ;
    if( RST7_State ) then
    begin
        Output( '<RST7_State/>' ) ;
    end ;

    Output( '<Registers>' ) ;
    for Loop := 0 to 1 do
    begin
        for Loop1 := 0 to 3 do
        begin
            Output( '|' + inttostr( _Register[ Loop, Loop1 ] ) ) ;
        end ;
    end ;
    Output( '</Registers>' ) ;

    Output( '<Breakpoints>' + _Breakpoints.Serialize + '</Breakpoints>' ) ;

    Output( '<Register_Watchpoints>' ) ;
    for Loop := 0 to 30 do
    begin
        Output( '|' + inttostr( _Register_Watchpoints[ Loop ] ) ) ;
    end ;
    Output( '</Register_Watchpoints>' ) ;

    Output( '<Memory_watchpoints>' ) ;
    Output( '<watchpoint>' + string( _Memory_Watchpoints.Serialize ) ) ;
    Output( '</Memory_watchpoints>' ) ;

    Output( '<Port_watchpoints>' ) ;
    for Loop := 0 to 255 do
    begin
        Output( inttostr( _Port_Watchpoints[ Loop ] ) + '|' ) ;
    end ;
    Output( '</Port_watchpoints>' ) ;
end ; // TZ80_CPU.Save_State


function TZ80_CPU.Clear_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := Parent.Set_Error( Z80Err_No_Cache ) ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 30 ) ) then
        begin
            Result := Parent.Set_Error( Z80Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] := _Register_Watchpoints[ Address ] and ( not Access_None ) ;
            Result := Parent.Set_Error( 0 ) ;
        end ;
    end ;
end ;



// TZ80 methods...

{ API... }

function TZ80.CPU : TCPU ;

begin
    Result := _CPU ;
end ;


function TZ80.Facility_Code : longint ;

begin
    Result := Z80Err_Facility ;
end ;


function TZ80.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU := TZ80_CPU.Create ;
    _CPU._UI := UI ;
    _CPU.Parent := self ;
    Inputs := TList.Create ;
    Outputs := TList.Create ;
end ;


function TZ80.Terminate : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    if( _CPU._UI <> nil ) then
    begin
        _CPU._UI.Termination_Notice( self ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    _CPU.Free ;
    _CPU := nil ;
end ;


function TZ80.Serial_Number : integer ;

begin
    Result := _Serial_Number ;
end ;


function TZ80.Child_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TZ80.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Clear_Watchpoints ;
end ; // TZ80.Clear_Watchpoint


function TZ80.Component_Type : longint ;

begin
    Result := Component_Type_CPU ; // CPU
end ;


function TZ80.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( Z80Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Inputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( Z80Err_Already_Connected ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Result := Set_Error( Z80Err_Success ) ;
end ;


function TZ80.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( Z80Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Outputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( Z80Err_Already_Connected ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Result := Set_Error( Z80Err_Success ) ;
end ;


function TZ80.Debugger : TDebug_Interface ;

begin
    Result := nil ; // TODO
end ;


function TZ80.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var V : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        Result := Set_Error( Z80Err_No_Cache ) ;
    end else
    begin
        if( Address > 30 ) then
        begin
            Set_Error( Z80Err_Invalid_Register ) ;
            exit ;
        end ;
        Set_Error( 0 ) ;
        if( Size = 0 ) then
        begin
            exit ;
        end ;
        if( Size > CPU.Register_Size( Address ) ) then
        begin
            Size := CPU.Register_Size( Address ) ;
        end ;
        Size := ( Size + 7 ) div 8 ; // Number of bytes
        V := 0 ;
        move( Buffer^, V, Size ) ;
        case Address of
            0 : _CPU._PC := V ;
            1 : _CPU._SP := V ;
            2 : _CPU._Register[ 0, AF ] := V ;
            3 : _CPU._Register[ 0, BC ] := V ;
            4 : _CPU._Register[ 0, DE ] := V ;
            5 : _CPU._Register[ 0, HL ] := V ;
            6 : _CPU._Register[ 0, AF ] := ( _CPU._Register[ 0, AF ] and $FF ) or ( swap( V ) and $FF00 ) ;
            7 : _CPU._Register[ 0, AF ] := ( _CPU._Register[ 0, AF ] and $FF00 ) or V ;
            8 : _CPU._Register[ 0, BC ] := ( _CPU._Register[ 0, BC ] and $FF ) or ( swap( V ) and $FF00 ) ;
            9 : _CPU._Register[ 0, BC ] := ( _CPU._Register[ 0, BC ] and $FF00 ) or V ;
            10 : _CPU._Register[ 0, DE ] := ( _CPU._Register[ 0, DE ] and $FF ) or ( swap( V ) and $FF00 ) ;
            11 : _CPU._Register[ 0, DE ] := ( _CPU._Register[ 0, DE ] and $FF00 ) or V ;
            12 : _CPU._Register[ 0, HL ] := ( _CPU._Register[ 0, HL ] and $FF ) or ( swap( V ) and $FF00 ) ;
            13 : _CPU._Register[ 0, HL ] := ( _CPU._Register[ 0, HL ] and $FF00 ) or V ;
            14 : _CPU._Register[ 1, AF ] := V ;
            15 : _CPU._Register[ 1, BC ] := V ;
            16 : _CPU._Register[ 1, DE ] := V ;
            17 : _CPU._Register[ 1, HL ] := V ;
            18 : _CPU._Register[ 1, AF ] := ( _CPU._Register[ 1, AF ] and $FF ) or ( swap( V ) and $FF00 ) ;
            19 : _CPU._Register[ 1, AF ] := ( _CPU._Register[ 1, AF ] and $FF00 ) or V ;
            20 : _CPU._Register[ 1, BC ] := ( _CPU._Register[ 1, BC ] and $FF ) or ( swap( V ) and $FF00 ) ;
            21 : _CPU._Register[ 1, BC ] := ( _CPU._Register[ 1, BC ] and $FF00 ) or V ;
            22 : _CPU._Register[ 1, DE ] := ( _CPU._Register[ 1, DE ] and $FF ) or ( swap( V ) and $FF00 ) ;
            23 : _CPU._Register[ 1, DE ] := ( _CPU._Register[ 1, DE ] and $FF00 ) or V ;
            24 : _CPU._Register[ 1, HL ] := ( _CPU._Register[ 1, HL ] and $FF ) or ( swap( V ) and $FF00 ) ;
            25 : _CPU._Register[ 1, HL ] := ( _CPU._Register[ 1, HL ] and $FF00 ) or V ;
            26 : _CPU._I := V ;
            27 : _CPU._R := V ;
            28 : _CPU._IM := V ;
            29 : _CPU._IX := V ;
            30 : _CPU._IY := V ;
        end ; // case Address
    end ; // if( Memory )
end ; // TZ80.Deposit


function TZ80.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Result := Set_Error( Z80Err_Component_Not_Found ) ;
    end else
    begin
	Result := Set_Error( Z80Err_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TZ80.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Result := Set_Error( Z80Err_Component_Not_Found ) ;
    end else
    begin
	Result := Set_Error( Z80Err_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TZ80.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Size, V : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        Result := Set_Error( Z80Err_No_Cache ) ;
    end else
    begin
        if( Address > 30 ) then
        begin
            Result := Set_Error( Z80Err_Invalid_Register ) ;
            exit ;
        end ;
        Set_Error( 0 ) ;
        if( Size = 0 ) then
        begin
            exit ;
        end ;
        if( Size > CPU.Register_Size( Address ) ) then
        begin
            Size := CPU.Register_Size( Address ) ;
        end ;
        case Address of
            0 : V := _CPU._PC ;
            1 : V := _CPU._SP ;
            2 : V := _CPU._Register[ 0, AF ] ;
            3 : V := _CPU._Register[ 0, BC ] ;
            4 : V := _CPU._Register[ 0, DE ] ;
            5 : V := _CPU._Register[ 0, HL ] ;
            6 : V := lo( _CPU._Register[ 0, AF ] ) ;
            7 : V := hi( _CPU._Register[ 0, AF ] ) ;
            8 : V := hi( _CPU._Register[ 0, BC ] ) ;
            9 : V := lo( _CPU._Register[ 0, BC ] ) ;
            10 : V := hi( _CPU._Register[ 0, DE ] ) ;
            11 : V := lo( _CPU._Register[ 0, DE ] ) ;
            12 : V := hi( _CPU._Register[ 0, HL ] ) ;
            13 : V := lo( _CPU._Register[ 0, HL ] ) ;
            14 : V := _CPU._Register[ 1, AF ] ;
            15 : V := _CPU._Register[ 1, BC ] ;
            16 : V := _CPU._Register[ 1, DE ] ;
            17 : V := _CPU._Register[ 1, HL ] ;
            18 : V := lo( _CPU._Register[ 1, AF ] ) ;
            19 : V := hi( _CPU._Register[ 1, AF ] ) ;
            20 : V := hi( _CPU._Register[ 1, BC ] ) ;
            21 : V := lo( _CPU._Register[ 1, BC ] ) ;
            22 : V := hi( _CPU._Register[ 1, DE ] ) ;
            23 : V := lo( _CPU._Register[ 1, DE ] ) ;
            24 : V := hi( _CPU._Register[ 1, HL ] ) ;
            25 : V := lo( _CPU._Register[ 1, HL ] ) ;
            26 : V := _CPU._I ;
            27 : V := _CPU._R ;
            28 : V := _CPU._IM ;
            29 : V := _CPU._IX ;
            30 : V := _CPU._IY ;
        end ; // case Address
        _Size := ( Size + 7 ) div 8 ; // Number of bytes
        move( V, Buffer^, _Size ) ;
    end ; // if( Memory )
end ; // TZ80.Examine


function TZ80.Get_Access_Mode( Address : int64 ; Memory : boolean ) : longint ;

begin
    if( Memory ) then
    begin
        Result := Access_None ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 30 ) ) then
        begin
            Result := Access_None ;
        end else
        begin
            Result := Access_All ;
        end ;
    end ;
end ;


function TZ80.Get_Profiling : boolean ;

begin
    Result := _CPU._Profiling ;
end ;


function TZ80.Get_Read_Latency : longint ;

begin
    Result := 0 ;
end ;


function TZ80.Get_Write_Latency : longint ;

begin
    Result := 0 ;
end ;


function TZ80.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
        Result := nil ;
        Set_Error( Z80Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Inputs[ Index ] ;
end ;


const Z80_Name : string = 'CEF Z80' ;

function TZ80.Name : PChar ;

begin
    Result := PChar( Z80_Name ) ;
end ;


function TZ80.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Outputs.Count ) ) then
    begin
        Result := nil ;
        Set_Error( Z80Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Outputs[ Index ] ;
end ;


function TZ80.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ; // Doens't apply to CPUs
end ;


function TZ80.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
end ;


function TZ80.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Restore_State( Stream ) ;
end ;


function TZ80.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
end ;


function TZ80.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Save_State( Stream ) ;
end ;


function TZ80.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( Z80_Invalid_Operation ) ;
end ;


procedure TZ80.Set_Profiling( _On, Children : boolean ) ;

begin
    _CPU._Profiling := _On ;
end ;


procedure TZ80.Set_Read_Latency( Value : longint ) ;

begin
    // Do nothing - we have no read latency
end ;


function TZ80.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    if( Access = 0 ) then // No effect
    begin
        exit ;
    end ;
    if( Memory ) then
    begin
        _CPU._Memory_Watchpoints.Create_Watchpoint( Address, Access ) ;
    end else
    begin
        if( ( Address < CPU.Get_Low_Port ) or ( Address > CPU.Get_High_Port ) ) then
        begin
            Result := Set_Error( Z80_Invalid_Address ) ;
            exit ;
        end ;
        _CPU._Port_Watchpoints[ Address ] := _CPU._Port_Watchpoints[ Address ] or Access ;
    end ;
end ; // TZ80.Set_Watchpoint


procedure TZ80.Set_Write_Latency( Value : longint ) ;

begin
    // Intentionally left blank - no latency
end ;


procedure TZ80.Show_Status ;

    function Show( X, Size : Integer ) : String ; { Return formatted number X }

    begin
	Show := Number_Format( _CPU.Base, Cvtb( 10, _CPU.Base, num1( X ) ), Size )
    end ;


var Index : integer ;
    S : string ;

    procedure Output( S : string ) ;

    begin
        _CPU._UI.Log_Status( PChar( S ), Index ) ;
        S := '' ;
        inc( Index ) ;
    end ;


begin
    Index := 0 ;
    case _CPU.Mode of
        M8080 : Output( 'Intel 8080' ) ;
        M8085 : Output( 'Intel 8085' ) ;
        else Output( 'Zilog Z80' ) ;
    end ;
    S := 'PC=' + Show( _CPU._Pc, 2 ) + '  SP=' + Show( _CPU._Sp, 2 ) + '  Interrupts=' ;
    if _CPU.Interrupts then
    begin
        S := S + ' enabled' ;
    end else
    begin
        S := S + ' disabled' ;
    end ;
    S := S + '  IM=' + num1( _CPU._Im ) ;
    Output( S ) ;
    if( _CPU.Register_Set1 = 0 ) then S := S + '*' ;
    S := S + 'A=' + Show( Lo( _CPU._Register[ 0, AF ] ), 1 ) ;
    S := S + '    F=' + Show( Hi( _CPU._Register[ 0, AF ] ), 1 ) +
        ' (' + Get_Mask( Hi( _CPU._Register[ 0, AF ] ) ) + ')' ;
    Output( S ) ;
    if( _CPU.Register_Set1 = 0 ) then S := S + '*' ;
    S := S + 'B=' + Show( Hi( _CPU._Register[ 0, BC ] ), 1 ) ;
    S := S + '    C=' + Show( Lo( _CPU._Register[ 0, BC ] ), 1 ) ;
    S := S + '     BC=' + Show( _CPU._Register[ 0, BC ], 2 ) ;
    if( _CPU.Register_Set1 = 0 ) then S := S + '*' ;
    S := S + 'D=' + Show( Hi( _CPU._Register[ 0, DE ] ), 1 ) ;
    S := S + '    E=' + Show( Lo( _CPU._Register[ 0, DE ] ), 1 ) ;
    S := S + '     DE=' + Show( _CPU._Register[ 0, DE ], 2 ) ;
    if( _CPU.Register_Set1 = 0 ) then S := S + '*' ;
    S := S + 'H=' + Show( Hi( _CPU._Register[ 0, HL ] ), 1 ) ;
    S := S + '    L=' + Show( Lo( _CPU._Register[ 0, HL ] ), 1 ) ;
    S := S + '     HL=' + Show( _CPU._Register[ 0, HL ], 2 ) ;
    Output( S ) ;
    if( _CPU.Register_Set1 = 0 ) then S := S + '*' ;
    S := S + 'A''=' + Show( Lo( _CPU._Register[ 1, AF ] ), 1 ) ;
    S := S + '   F''=' + Show( Hi( _CPU._Register[ 1, AF ] ), 1 ) +
        ' (' + Get_Mask( Hi( _CPU._Register[ 1, AF ] ) ) + ')' ;
    Output( S ) ;
    if( _CPU.Register_Set1 = 1 ) then S := S + '*' ;
    S := S + 'B''=' + Show( Hi( _CPU._Register[ 1, BC ] ), 1 ) ;
    S := S + '   C''=' + Show( Lo( _CPU._Register[ 1, BC ] ), 1 ) ;
    S := S + '    BC''=' + Show( _CPU._Register[ 1, BC ], 2 ) ;
    S := S + '    (BC'')=' + Show( _CPU.Byte_Read( _CPU._Register[ 1, BC ] ), 2 ) ;
    Output( S ) ;
    if( _CPU.Register_Set1 = 1 ) then S := S + '*' ;
    S := S + 'D''=' + Show( Hi( _CPU._Register[ 1, DE ] ), 1 ) ;
    S := S + '   E''=' + Show( Lo( _CPU._Register[ 1, DE ] ), 1 ) ;
    S := S + '    DE''=' + Show( _CPU._Register[ 1, DE ], 2 ) ;
    if( _CPU.Register_Set1 = 1 ) then S := S + '*' ;
    S := S + 'H''=' + Show( Hi( _CPU._Register[ 1, HL ] ), 1 ) ;
    S := S + '   L''=' + Show( Lo( _CPU._Register[ 1, HL ] ), 1 ) ;
    S := S + '    HL''=' + Show( _CPU._Register[ 1, HL ], 2 ) ;
    S := S + '  I=' + Show( _CPU._I, 1 ) + '    R=' + Show( _CPU._R, 1 ) ;
    S := S + 'IX=' + Show( _CPU._IX, 2 ) ;
    S := S + '  IY=' + Show( _CPU._IY, 2 ) ;
    Output( S ) ;
end ; // TZ80.Show_Status


function TZ80.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


procedure TZ80.Wake ;

begin
    _CPU.Blocked := False ;
end ;


function TZ80.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : integer ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := Value ;
end ;


function TZ80.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := ord( Value[ ( ( Size + 7 ) div 8 ) - 1 ] ) ;
end ;


procedure TZ80.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = '8080' ) then
        begin
            _CPU.Mode := M8080 ;
        end else
        if( S = '8085' ) then
        begin
            _CPU.Mode := M8085 ;
        end else
        if( S = 'Z80' ) then
        begin
            _CPU.Mode := Z80 ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ;


procedure TZ80.Reset ;

begin
    CPU.Restart ;
end ;


procedure TZ80.Set_Signal( Name : PChar ; State : boolean ) ;

var Temp : string ;

begin
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    case _CPU.Mode of
        M8080 :
            if( _CPU.Interrupts and ( ( Temp = 'INT' ) or ( Temp = 'INTR' ) ) ) then
            begin
                if( _Logger <> nil ) then
                begin
                    _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                end ;
                _CPU.Do_Interrupt ;
            end ;
        M8085 :
            begin
                if( ( Temp = 'INTR' ) or ( Temp = 'INT' ) ) then
                begin
                    if( _CPU.Interrupts ) then
                    begin
                        if( _Logger <> nil ) then
                        begin
                            _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                        end ;
                        _CPU.Do_Interrupt ;
                    end ;
                end else
                if( Temp = 'RST5.5' ) then
                begin
                    if( _Logger <> nil ) then
                    begin
                        _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                    end ;
                    _CPU.RST5_State := State ;
                    if( State ) then
                    begin
                        if( _CPU.RST5_Enabled ) then
                        begin
                            _CPU.Push_PC ;
                            _CPU.Interrupts := False ;
                            _CPU.PC := $2C ;
                            _CPU.State_Change_Notice( State_RST5, True ) ;
                        end ;
                    end ;
                end else
                if( Temp = 'RST6.5' ) then
                begin
                    if( _Logger <> nil ) then
                    begin
                        _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                    end ;
                    _CPU.RST6_State := State ;
                    if( State ) then
                    begin
                        if( _CPU.RST6_Enabled ) then
                        begin
                            _CPU.Push_PC ;
                            _CPU.Interrupts := False ;
                            _CPU.PC := $34 ;
                            _CPU.State_Change_Notice( State_RST6, True ) ;
                        end ;
                    end ;
                end else
                if( Temp = 'RST7.5' ) then
                begin
                    if( _Logger <> nil ) then
                    begin
                        _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                    end ;
                    if( State <> _CPU.RST7_State ) then // Signal is edge sensitive - only do something if it changes
                    begin
                        _CPU.RST7_State := State ;
                        if( State ) then
                        begin
                            if( _CPU.RST7_Enabled ) then
                            begin
                                _CPU.Push_PC ;
                                _CPU.Interrupts := False ;
                                _CPU.PC := $3C ;
                            end else
                            begin
                                _CPU.RST7_Pending := True ;
                            end ;
                            _CPU.State_Change_Notice( State_RST7, True ) ;
                        end ;
                    end ;
                end else
                if( Temp = 'TRAP' ) then
                begin
                    if( State ) then
                    begin
                        if( _Logger <> nil ) then
                        begin
                            _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                        end ;
                        _CPU.Do_NMI ;
                    end ;
                end else
                if( Temp = 'SID' ) then
                begin
                    _CPU.Serial_Input_State := State ;
                    if( _Logger <> nil ) then
                    begin
                        _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                    end ;
                    _CPU._UI.Signal_Change_Notice( self, 5, State ) ;
                end else
                begin
                    exit ;
                end ;
            end ;
        Z80 :
            begin
		        if( Temp = 'NMI' ) then
                begin
                    if( not State ) then
                    begin
                        if( _Logger <> nil ) then
                        begin
                            _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                        end ;
                        _CPU.Do_NMI ;
                    end ;
                end else
                if( Temp = 'INT' ) then
                begin
                    if( not State and _CPU.Interrupts ) then
                    begin
                        if( _Logger <> nil ) then
                        begin
                            _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
                        end ;
                        _CPU.Do_Interrupt ;
                    end ;
                end ;
            end ;
    end ; // case Mode
end ; // TZ80.Set_Signal


function TZ80.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

var Temp : string ;

begin
    Result := False ;
    if( _CPU.Mode = M8085 ) then
    begin
        Temp := string( Name ) ;
        Temp := uppercase( Name ) ;
        if( Temp = 'SOD' ) then
        begin
            Result := _CPU.Serial_Output_State ;
        end ;
    end ;
end ;


function TZ80.Signal_Count : longint ;

begin
    case _CPU.Mode of
        M8080 : Result := 1 ;
        M8085 : Result := 7 ;
        else {Z80} Result := 2 ;
    end ;
end ;


function TZ80.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := '' ;
    case _CPU.Mode of
        M8080 :
            case Index of
                0 : Temp_Signal_Name := 'INT' ;
            end ;
        M8085 :
            case Index of
                0 : Temp_Signal_Name := 'INTR' ;
                1 : Temp_Signal_Name := 'RST5.5' ;
                2 : Temp_Signal_Name := 'RST6.5' ;
                3 : Temp_Signal_Name := 'RST7.5' ;
                4 : Temp_Signal_Name := 'TRAP' ; // NMI
                5 : Temp_Signal_Name := 'SID' ;
                6 : Temp_Signal_Name := 'SOD' ;
            end ;
        Z80 :
            case Index of
		0 : Temp_Signal_Name := 'NMI' ; // Active low
                1 : Temp_Signal_Name := 'INT' ; // Active low
            end ;
    end ;
    if( Temp_Signal_Name = '' ) then
    begin
        Result := nil ;
    end else
    begin
        Result := PChar( Temp_Signal_Name ) ;
    end ;
end ;


function TZ80.Signal_Index( Name : PChar ) : integer ;

var S : string ;

begin
    S := string( Name ) ;
    Result := -1 ;
    case _CPU.Mode of
        M8080 :
            if( ( S = 'INT' ) or ( S = 'INTR' ) ) then
            begin
                Result := 0 ;
            end ;
        M8085 :
            if( ( S = 'INT' ) or ( S = 'INTR' ) ) then
            begin
                Result := 0 ;
            end else
            if( S = 'RST5.5' ) then
            begin
                Result := 1 ;
            end else
            if( S = 'RST6.5' ) then
            begin
                Result := 2 ;
            end else
            if( S = 'RST7.5' ) then
            begin
                Result := 3 ;
            end else
            if( S = 'TRAP' ) then
            begin
                Result := 4 ;
            end else
            if( S = 'SID' ) then
            begin
                Result := 5 ;
            end else
            if( S = 'SOD' ) then
            begin
                Result := 6 ;
            end ;
        Z80 :
            if( S = 'NMI' ) then
            begin
                Result := 0 ;
            end ;
    end ;
end ;


function TZ80.Signal_Out( Index : longint ) : boolean ;

begin
    if( ( _CPU.Mode = M8085 ) and ( Index = 6 ) ) then
    begin
        Result := True ; // SOD
    end else
    begin
        Result := False ;
    end ;
end ;


function TZ80.Signal_Active_Low( Index : longint ) : boolean ;

begin
    if( _CPU.Mode = Z80 ) then
    begin
        Result := True ;
    end else
    begin
        Result := False ;
    end ;
end ;


function TZ80.Get_State_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( ( Index >= 0 ) and ( Index <= 6 ) ) then
    begin
        case Index of
            State_Port_Input : Temp_Get_State_Name := 'Port input' ;
            State_Port_Output : Temp_Get_State_Name := 'Port output' ;
            State_Interrupt : Temp_Get_State_Name := 'Interrupt' ;
            State_NMI : Temp_Get_State_Name := 'NMI' ;
            State_RST5 : Temp_Get_State_Name := 'RST5' ;
            State_RST6 : Temp_Get_State_Name := 'RST6' ;
            State_RST7 : Temp_Get_State_Name := 'RST7' ;
        end ;
        Result := PChar( Temp_Get_State_Name ) ;
    end ;
end ;


function TZ80.Get_Exception_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Temp_Get_Exception_Description := 'Invalid instruction' ;
                Result := PChar( Temp_Get_Exception_Description ) ;
            end ;
        1 : begin
                Temp_Get_Exception_Description := 'Nested NMI' ;
                Result := PChar( Temp_Get_Exception_Description ) ;
            end ;
    end ;
end ;


procedure TZ80.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TZ80.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TZ80.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TZ80.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function TZ80.Profiler : TProfiler ;

begin
    if( _CPU._Profiler = nil ) then
    begin
        _CPU._Profiler := TZ80_Profiler.Create ;
        _CPU._Profiler.Set_Mode( _CPU.Mode ) ;
    end ;
    Result := _CPU._Profiler ;
end ;


function TZ80.Get_Trace : boolean ;

begin
    Result := _CPU._Trace ;
end ;


procedure TZ80.Set_Trace( Value : boolean ) ;

begin
    _CPU._Trace := Value ;
end ;


function TZ80.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TZ80.Set_Logger( Value : TCEF_Logger ) ;

begin
    if( _Logger <> nil ) then
    begin
        _Logger.Detach ;
    end ;
    _Logger := Value ;
    if( Value <> nil ) then
    begin
        _Logger.Attach ;
    end ;
    _CPU._Logger := Value ;
end ;


end.

