{$N+}
{
        Program Name : Gigabyte_uCode_CPU
        Package Name : Gigabyte_uCode
        Purpose      : Gigabyte micro-code (CEF component) emulator
        Institution  :
        Date Written : 4-Dec-2011
        Written By   : Alan Conroy
        Version      : 2.0

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON                         

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This unit implements a Gigabyte micro-code emulator as a CEF
        component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Gigabyte_uCode_CPU ;

interface

uses { Borland... }
     Classes, { TList }

     { CEF... }
     _CEF, // TCEF_Logger
     CEF, { TBase_CPU }
     _CEFUtil, // TCEF_Watchpoint

     { Gigabyte_uCode... }
     Gigabyte_uCode_ASM, { TMode }

     { Other... }
     _DebugIn, { TDebug_Interface }
     CommonUt, { TInteger_List }
     _Streams, // TCOM_Stream
     _UE ; // TUnified_Exception

type Register_Set = array[ 0..255 ] of longint ;

const SF_Predefined = 1 ; { Predefined symbol }

const Gigabyte_uCodeErr_Facility = 42 ;
      Gigabyte_uCodeErr_Success = 0 ;
      Gigabyte_uCodeErr_Invalid_Component = 1 ;
      Gigabyte_uCodeErr_Already_Connected = 2 ;
      Gigabyte_uCodeErr_Component_Not_Found = 3 ;
      Gigabyte_uCodeErr_No_Cache = 4 ;
      Gigabyte_uCodeErr_Invalid_Register = 5 ;
      Gigabyte_uCode_No_Breakpoint = 6 ;
      Gigabyte_uCode_Breakpoint_Exists = 7 ;
      Gigabyte_uCode_Invalid_Address = 8 ;
      Gigabyte_uCode_Invalid_Operation = 9 ;
      Gigabyte_uCode_Invalid_Instruction = 10 ;

      Latch_DA = 0 ;
      Latch_PC = 1 ;
      Latch_SP = 2 ;
      Latch_DA1 = 3 ;

type TGigabyte_uCode_Profiler = class( TBase_Profiler )
                         private // Instance data...
                             // Profile data...
                             _Clock : integer ;
                             _Instruction_Count : integer ;
                             Port_Outputs : array[ 0..255 ] of integer ;
                             Port_Inputs : array[ 0..255 ] of integer ;
                             Execution_Addresses : array[ 0..$FFFFF ] of integer ;

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

                         public // API...
                             procedure Generate_Report ;
                             procedure Increment( Domain, Index : integer ) ;
                             procedure Increment_Clock( Count : integer ) ;

                         public // Overrides...
                             procedure Clear( Domain : integer ) ; override ;

                             function Domain_Name( Index : integer ) : PChar ;
                                 override ;

                             function Report_Line( Domain, Index : integer ) : PChar ;
                                 override ;
                     end ; // TGigabyte_uCode_Profiler

type TGigabyte_uCode_CPU = class ;

     TGigabyte_uCode = class( TBase_Component )
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
                    _CPU : TGigabyte_uCode_CPU ;

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
            end ; // TGigabyte_uCode


     TLock_Mode = ( LM_Unlocked,
                    LM_Locked,
                    LM_Pending_Lock
                  ) ;

     TGigabyte_uCode_CPU = class( TBase_CPU )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private
            Parent : TGigabyte_uCode ;

            _UI : TUI_Interface ;
            _Speed : integer ; // KHz
            Temp_Register_Name : string ;
            Temp_Register_Description : string ;
            Temp_Log_Trace : string ;
            _Halted : boolean ; // True if last instruction was a halt
            _Register_Watchpoints : array[ 0..30 ] of integer ; // Access mode for registers
            _Profiling : boolean ; // True if profiling
            _Memory_Watchpoints : TCEF_Watchpoint_Manager ;
            _Port_Watchpoints : array[ 0..255 ] of integer ;
            _Breakpoints : TInteger_List ;
            Memory_Data_Latch : int64 ; // Last data sent to us
            _Run_Stream : TCOM_Stream ;
            _Stream_PC_Offset : integer ;
            Blocked : boolean ;
            Paused : boolean ;
            Stopping : boolean ;
            _Profiler : TGigabyte_uCode_Profiler ;
            _Trace : boolean ; // True to trace execution
            Segments : TInteger_List ;
            Lock_Mode : TLock_Mode ;
            Pending_Interrupt : boolean ;
            _Latched_Address : integer ; // See Latch_*
            ROM : TComponent ; // Microcode
            _Logger : TCEF_Logger ;
            Temp_Address_Representation : string ;

        private // Registers...
            _Registers : Register_Set ; { General registers }
            _SP : int64 ; { Stack pointer }
            _PC : int64 ; { PC }
            _IP : int64 ;
            _Temp : int64 ;
            _Status : byte ;
            _ALUA : int64 ;
            _ALUB : int64 ;
            _ALU : int64 ; // ALU output
            _RC : word ; // ROM instruction pointer
            _DA : int64 ;
            _DA1 : int64 ;
            _T_TS, _T_S, _T_M, _T_H, _T_DW, _T_D, _T_MO : byte ;
            _T_Y : word ;
            _Instruction : longint ;
            _Bus : longint ;
            _Resume : word ;

        private // Internal utility routines...
            function Interrupts : boolean ; // True if interrupts enabled

            function Latched_Address : int64 ; { Return latched Address }
            procedure Interrupt ;
            procedure Do_Interrupt ;
            procedure Do_Wait ;
            function ROM_Read( Address : word ) : int64 ; { Return data at ROM address }
            procedure Increment_Clock( Count : integer ) ;
            function Bus_Read( Address : int64 ; Size, IO_Type : longint ) : int64 ; { Return data at Address }
            function Bus_Examine( Address : Integer ) : int64 ; { Return data at memory Address }
            procedure Bus_Write( Address : int64 ; Size : integer ; Value : int64 ; IO_Type : longint ) ; { Write to memory }
            procedure Execute( Single_Step, Into : boolean ) ;
            procedure Output( Port, Value : integer ; W : boolean ) ;
            procedure Send_Signal( const Name : string ; Value : boolean ) ;
            procedure Clear_Watchpoints ;
            procedure State_Change_Notice( Index : integer ; State : boolean ) ;
            procedure Log_Trace( const Description : string ) ;
            procedure Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
                Memory, Internal, Port : boolean ) ;
            function Instruction_At( Address : integer ) : string ;

            function Get_SP : int64 ;
            procedure Set_SP( Value : int64 ) ;
            function Get_PC : int64 ;
            procedure Set_PC( Value : int64 ) ;
            function Get_IP : int64 ;
            procedure Set_IP( Value : int64 ) ;
            function Get_Temp : int64 ;
            procedure Set_Temp( Value : int64 ) ;
            function Get_Status : byte ;
            procedure Set_Status( Value : byte ) ;
            function Get_ALUA : int64 ;
            procedure Set_ALUA( Value : int64 ) ;
            function Get_ALUB : int64 ;
            procedure Set_ALUB( Value : int64 ) ;
            function Get_ALU : int64 ;
            procedure Set_ALU( Value : int64 ) ;
            function Get_RC : word ;
            procedure Set_RC( Value : word ) ;
            function Get_DA : int64 ;
            procedure Set_DA( Value : int64 ) ;
            function Get_DA1 : int64 ;
            procedure Set_DA1( Value : int64 ) ;
            function Get_T_TS : byte ;
            procedure Set_T_TS( Value : byte ) ;
            function Get_T_S : byte ;
            procedure Set_T_S( Value : byte ) ;
            function Get_T_M : byte ;
            procedure Set_T_M( Value : byte ) ;
            function Get_T_H : byte ;
            procedure Set_T_H( Value : byte ) ;
            function Get_T_DW : byte ;
            procedure Set_T_DW( Value : byte ) ;
            function Get_T_D : byte ;
            procedure Set_T_D( Value : byte ) ;
            function Get_T_MO : byte ;
            procedure Set_T_MO( Value : byte ) ;
            function Get_T_Y : word ;
            procedure Set_T_Y( Value : word ) ;
            function Get_Instruction : longint ;
            procedure Set_Instruction( Value : longint ) ;
            function Get_X : byte ;
            procedure Set_X( Value : byte ) ;
            function Get_Y : byte ;
            procedure Set_Y( Value : byte ) ;
            function Get_Z : byte ;
            procedure Set_Z( Value : byte ) ;
            function Get_Bus : longint ;
            procedure Set_Bus( Value : longint ) ;
            function Get_Registers( Index : integer ) : longint ;
            procedure Set_Registers( Index : integer ; Value : longint ) ;
            function Get_Resume : word ;
            procedure Set_Resume( Value : word ) ;
            function Get__X : byte ;
            procedure Set__X( Value : byte ) ;
            function Get__Y : byte ;
            procedure Set__Y( Value : byte ) ;
            function Get__Z : byte ;
            procedure Set__Z( Value : byte ) ;

            property SP : int64
                read Get_SP
                write Set_SP ;
            property PC : int64
                read Get_PC
                write Set_PC ;
            property IP : int64
                read Get_IP
                write Set_IP ;
            property Temp : int64
                read Get_Temp
                write Set_Temp ;
            property Status : byte
                read Get_Status
                write Set_Status ;
            property ALUA : int64
                read Get_ALUA
                write Set_ALUA ;
            property ALUB : int64
                read Get_ALUB
                write Set_ALUB ;
            property ALU : int64
                read Get_ALU
                write Set_ALU ;
            property RC : word
                read Get_RC
                write Set_RC ;
            property DA : int64
                read Get_DA
                write Set_DA ;
            property DA1 : int64
                read Get_DA1
                write Set_DA1 ;
            property T_TS : byte
                read Get_T_TS
                write Set_T_TS ;
            property T_S : byte
                read Get_T_S
                write Set_T_S ;
            property T_M : byte
                read Get_T_M
                write Set_T_M ;
            property T_H : byte
                read Get_T_H
                write Set_T_H ;
            property T_DW : byte
                read Get_T_DW
                write Set_T_DW ;
            property T_D : byte
                read Get_T_D
                write Set_T_D ;
            property T_MO : byte
                read Get_T_MO
                write Set_T_MO ;
            property T_Y : word
                read Get_T_Y
                write Set_T_Y ;
            property Instruction : longint
                read Get_Instruction
                write Set_Instruction ;
            property X : byte
                read Get_X
                write Set_X ;
            property Y : byte
                read Get_Y
                write Set_Y ;
            property Z : byte
                read Get_Z
                write Set_Z ;
            property _X : byte
                read Get__X
                write Set__X ;
            property _Y : byte
                read Get__Y
                write Set__Y ;
            property _Z : byte
                read Get__Z
                write Set__Z ;
            property Bus : longint
                read Get_Bus
                write Set_Bus ;
            property Registers[ Index : integer ] : longint
                read Get_Registers
                write Set_Registers ;
            property Resume : word
                read Get_Resume
                write Set_Resume ;

        public // API...
            Base : integer ;

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

            function Register_Description( Index : longint ) : PChar ;
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

            function Get_Target_Memory : TComponent ; override ;

            function Address_Representation_Ex( C : TComponent ;
                Base : integer ; Address : int64 ) : PChar ; override ;
    end ; // TGigabyte_uCode_CPU


const // Status byte masks
      SBF_CF = 1 ; // Carry flag
      SBF_IF = 2 ; // Interrupt flag (0 = enabled)
      SBF_ZF = 4 ; // Zero flag
      SBF_Mode = $18 ; // Mode (ring)
      SBF_HI = $20 ; // Hardware Interrupt

const // Register indexes
      RI_SP = 0 ;
      RI_PC = 1 ;
      RI_IP = 2 ;
      RI_Temp = 3 ;
      RI_Status = 4 ;
      RI_ALUA = 5 ;
      RI_ALUB = 6 ;
      RI_ALU = 7 ;
      RI_RC = 8 ;
      RI_DA = 9 ;
      RI_DA1 = 10 ;
      RI_Instruction = 11 ;
      RI_X = 12 ;
      RI_Y = 13 ;
      RI_Z = 14 ;
      RI_Bus = 15 ;
      RI_Resume = 16 ;
      RI_T_TS = 17 ;
      RI_T_S = 18 ;
      RI_T_M = 19 ;
      RI_T_H = 20 ;
      RI_T_DW = 21 ;
      RI_T_D = 22 ;
      RI_T_MO = 23 ;
      RI_T_Y = 24 ;
      RI_Register_First = 25 ;
      RI_Register_Last = RI_Register_First + 255 ;

implementation

uses { Borland... }
     SysUtils, { Allocmem }

     { Gigabyte_uCode }
     Gigabyte_uCode_Util,

     { Other... }
     _ASCII, { CR }
     CVT, { Cvtb }
     HTML, { TXML_Parser }
     Num1s, { num1 }
     Parse, // TString_Parser
     SStreams, // TCOM_String_Stream
     Standard ; // Bit_Values

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ; external 'CEF_Util.dll' ;

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



function Instruction_Name( I : integer ) : string ;

begin
    Result := inttostr( I ) ;
    case I of
        0..5 : Result := 'ADD' ;
        6, $E, $16, $1E : Result := 'PUSH' ;
        7, $F, $17, $1F : Result := 'POP' ;
        $08..$0D : Result := 'OR' ;
        $10..$15 : Result := 'ADC' ;
        $18..$1D : Result := 'SBB' ;
        $20..$25 : Result := 'AND' ;
        $27 : Result := 'DAA' ;
        $28..$2D : Result := 'SUB' ;
        $2F : Result := 'DAS' ;
        $30..$35 : Result := 'XOR' ;
        $37 : Result := 'AAA' ;
        $38..$3D : Result := 'CMP' ;
        $3F : Result := 'AAS' ;
        $40..$47 : Result := 'INC' ;
        $48..$4F : Result := 'DEC' ;
        $50..$57 : Result := 'PUSH' ;
        $58..$5F : Result := 'POP' ;
        $70 : Result := 'JO' ;
        $71 : Result := 'JNO' ;
        $72 : Result := 'JB' ;
        $73 : Result := 'JNB' ;
        $74 : Result := 'JE' ;
        $75 : Result := 'JNE' ;
        $76 : Result := 'JBE' ;
        $77 : Result := 'JA' ;
        $78 : Result := 'JS' ;
        $79 : Result := 'JNS' ;
        $7A : Result := 'JP' ;
        $7B : Result := 'JNP' ;
        $7C : Result := 'JL' ;
        $7D : Result := 'JGE' ;
        $7E : Result := 'JLE' ;
        $7F : Result := 'JG' ;
        $080..$083 : Result := 'ADD' ;
        $180..$181 : Result := 'OR' ;
        $280..$283 : Result := 'ADC' ;
        $380..$383 : Result := 'SBB' ;
        $480..$481 : Result := 'AND' ;
        $580..$583 : Result := 'SUB' ;
        $680..$681 : Result := 'XOR' ;
        $780..$783 : Result := 'CMP' ;
        $84..$85 : Result := 'TEST' ;
        $86..$87 : Result := 'XCHG' ;
        $88..$08C : Result := 'MOV' ;
        $8D : Result := 'LEA' ;
        $08E : Result := 'MOV' ;
        $08F : Result := 'POP' ;
        $90..$97 : Result := 'XCHG' ;
        $98 : Result := 'CBW' ;
        $99 : Result := 'CWD' ;
        $9A : Result := 'CALL' ;
        $9C : Result := 'PUSHF' ;
        $9D : Result := 'POPF' ;
        $9E : Result := 'SAHF' ;
        $9F : Result := 'LAHF' ;
        $A0..$A3 : Result := 'MOV' ;
        $A4..$A5 : Result := 'MOVS' ;
        $A6..$A7 : Result := 'CMPS' ;
        $AC..$AD : Result := 'LODS' ;
        $AA..$AB : Result := 'STOS' ;
        $AE..$AF : Result := 'SCAS' ;
        $B0..$BF : Result := 'MOV' ;
        $C4 : Result := 'LES' ;
        $C5 : Result := 'LDS' ;
        $0C6..$0C7 : Result := 'MOV' ;
        $0D0..$0D3 : Result := 'ROL' ;
        $1D0..$1D3 : Result := 'ROR' ;
        $2D0..$2D3 : Result := 'RCL' ;
        $3D0..$3D3 : Result := 'RCR' ;
        $4D0..$4D3 : Result := 'SHL' ;
        $5D0..$5D3 : Result := 'SHR' ;
        $7D0..$7D3 : Result := 'SAR' ;
        $9B : Result := 'WAIT' ;
        $A8..$A9 : Result := 'TEST' ;
        $C2..$C3 : Result := 'RET' ;
        $CA..$CB : Result := 'RET' ;
        $CC : Result := 'INT3' ;
        $CD : Result := 'INT' ;
        $CE : Result := 'INTO' ;
        $CF : Result := 'IRET' ;
        $D4 : Result := 'AAM' ;
        $D5 : Result := 'AAD' ;
        $D7 : Result := 'XLAT' ;
        $D8..$DF : Result := 'ESC' ;
        $E0 : Result := 'LOOPNE' ;
        $E1 : Result := 'LOOPE' ;
        $E2 : Result := 'LOOP' ;
        $E3 : Result := 'JCXZ' ;
        $E4..$E5 : Result := 'IN' ;
        $E6..$E7 : Result := 'OUT' ;
        $E8 : Result := 'CALL' ;
        $E9 : Result := 'JMP' ;
        $EA..$EB : Result := 'JMP' ;
        $ED : Result := 'IN' ;
        $EE..$EF : Result := 'OUT' ;
        $F0 : Result := 'LOCK' ;
        $F2..$F3 : Result := 'REP' ;
        $F4 : Result := 'HLT' ;
        $F5 : Result := 'CMC' ;
        $0F6..$0F7 : Result := 'TEST' ;
        $2F6..$2F7 : Result := 'NOT' ;
        $3F6..$3F7 : Result := 'NEG' ;
        $4F6..$4F7 : Result := 'MUL' ;
        $5F6..$5F7 : Result := 'IMUL' ;
        $6F6..$6F7 : Result := 'DIV' ;
        $7F6..$7F7 : Result := 'IDIV' ;
        $F8 : Result := 'CLC' ;
        $F9 : Result := 'STC' ;
        $FC : Result := 'CLD' ;
        $FD : Result := 'STD' ;
        $FA : Result := 'CLI' ;
        $FB : Result := 'STI' ;
        $0FE..$0FF : Result := 'INC' ;
        $1FE..$1FF : Result := 'DEC' ;
        $2FF : Result := 'CALL' ;
        $3FF : Result := 'CALL' ;
        $4FF : Result := 'JMP' ;
        $5FF : Result := 'JMP' ;
        $6FF : Result := 'PUSH' ;
    end ;
end ; // Instruction_Name


// TGigabyte_uCode_Profiler methods...

// API...

procedure TGigabyte_uCode_Profiler.Generate_Report ;

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
    Base := 8 ;

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
    for Loop := 0 to $7FF do
    begin
(*
        if( Instructions[ Loop ] <> 0 ) then
        begin
            Instruction_Lines.Add( Instruction_Name( Loop, _Mode ) + ': ' + inttostr( Instructions[ Loop ] ) ) ;
        end ;
*)
    end ;
    Instruction_Lines.Sort ;
end ; // TGigabyte_uCode_Profiler.Generate_Report


procedure TGigabyte_uCode_Profiler.Increment( Domain, Index : integer ) ;

begin
    Dirty := True ;
    case Domain of
        Domain_Port_Outputs: inc( Port_Outputs[ Index ] ) ;
        Domain_Port_Inputs: inc( Port_Inputs[ Index ] ) ;
        Domain_Execution_Addresses: inc( Execution_Addresses[ Index ] ) ;
        Domain_Instructions: ;
        Domain_Other: if( Index = Domain_Other_Instruction_Count ) then
                      begin
                          inc( _Instruction_Count ) ;
                      end ;
    end ;
end ;


procedure TGigabyte_uCode_Profiler.Increment_Clock( Count : integer ) ;

begin
    Dirty := True ;
    _Clock := _Clock + Count ;
end ;


// Overrides...

procedure TGigabyte_uCode_Profiler.Clear( Domain : integer ) ;

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
                //fillchar( Instructions, sizeof( Instructions ), 0 ) ;
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
end ; // TGigabyte_uCode_Profiler.Clear


function TGigabyte_uCode_Profiler.Domain_Name( Index : integer ) : PChar ;

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


function TGigabyte_uCode_Profiler.Report_Line( Domain, Index : integer ) : PChar ;

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
end ; // TGigabyte_uCode_Profiler.Report_Line



function Get_Mask( Value : Integer ) : String ; { Return bits names }

var A : string ;

    procedure Add_Flag( Value : integer ; N : char ) ;

    begin
        if( Value = 0 ) then
        begin
            A := A + 'N' + N ;
        end else
        begin
            A := A + N ;
        end ;
        A := A + ' ' ;
    end ;

begin
    A := '' ;
    Add_Flag( Value and SBF_HI, 'H' ) ;
    Add_Flag( Value and SBF_ZF, 'Z' ) ;
    Add_Flag( ( not Value ) and SBF_IF, 'I' ) ;
    Add_Flag( Value and SBF_CF, 'C' ) ;
    A := A + ' ring ' + inttostr( ( Value and SBF_Mode ) shr 3 ) ;
    Get_Mask := A ; { Return value }
end ; // Get_Mask


// TGigabyte_uCode_CPU methods...

// Constructors and destructors...

constructor TGigabyte_uCode_CPU.Create ;

begin
    inherited Create ;

    _Memory_Watchpoints := Get_Watchpoint_Manager ;
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
    _Breakpoints := TInteger_List.Create ;
    _Speed := 100000 ; // 1 GHz
    Base := Default_Base ;
    Segments := TInteger_List.Create ;
    Restart ; // Do power-on reset
end ;


destructor TGigabyte_uCode_CPU.Destroy ;

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

procedure TGigabyte_uCode_CPU.State_Change_Notice( Index : integer ; State : boolean ) ;

begin
    _UI.State_Change_Notice( Parent, Index, State ) ;

    case Index of
        State_Interrupt : Log_Trace( 'Process interrupt' ) ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Log_Trace( const Description : string ) ;

begin
    if( _Trace ) then
    begin
        Temp_Log_Trace := Description ;
        _UI.Log_Trace( Parent, PChar( Temp_Log_Trace ) ) ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
    Memory, Internal, Port : boolean ) ;

begin
    _UI.Watchpoint_Notice( Address, Access, Tag, Parent, Memory, Internal, Port ) ;
end ;


function TGigabyte_uCode_CPU.Instruction_At( Address : integer ) : string ;

var Stream : TCOM_String_Stream ;

begin
    Stream := TCOM_String_Stream.Create ;
    Disassemble( Address, Base, 1, Stream ) ;
    Result := string( Stream.As_String ) ;
    Stream.Detach ;
end ;


function TGigabyte_uCode_CPU.Interrupts : boolean ;

begin
    Result := ( _Status and SBF_IF ) <> 0 ;
end ;


procedure TGigabyte_uCode_CPU.Interrupt ;

begin
    Pending_Interrupt := True ;
end ;


procedure TGigabyte_uCode_CPU.Do_Interrupt ;

begin
    if( not Interrupts ) then // Interrupts disabled
    begin
        exit ;
    end ;
    Interrupt ;
end ;


procedure TGigabyte_uCode_CPU.Do_Wait ;

begin
    try
        _UI.Idle( Parent ) ;
    except
    end ;
end ;


function TGigabyte_uCode_CPU.ROM_Read( Address : word ) : int64 ; { Return data at ROM address }

var Size : integer ;

begin
    Result := 0 ;
    Memory_Data_Latch := 0 ; // Default if nothing responds
    Size := 40 ;
    Address := Address * 8 ;
    if( ROM = nil ) then
    begin
        Result := 0 ;
        exit ;
    end ;
    if( ROM.Read( Address, Size, IO_Type_Memory ) ) then
    begin
        exit ;
    end ;
    ROM_Read := Memory_Data_Latch ;
end ;


function TGigabyte_uCode_CPU.Bus_Examine( Address : Integer ) : int64 ; { Return data at ROM Address }

var Size : integer ;
    UEC : TUnified_Exception ;

begin
    Size := 40 ;
    Address := Address * 5 ;
    if( ROM = nil ) then
    begin
        Result := 0 ;
        exit ;
    end ;
    UEC := ROM.Examine( Address, Size, @Result, True ) ;
    if( UEC = nil ) then
    begin
        exit ;
    end ;
end ;


function TGigabyte_uCode_CPU.Bus_Read( Address : int64 ; Size, IO_Type : longint ) : int64 ; { Return data at Address }

var Component : TComponent ;
    Loop : integer ;

begin
    Result := 0 ;
    Memory_Data_Latch := -1 ; // Default if nothing responds
    try
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Address, Size, IO_Type ) ) then
            begin
                exit ;
            end ;
        end ;
        if( IO_Type = IO_Type_IO ) then
        begin
            if( _Run_Stream = nil ) then
            begin
                if( ( _Port_Watchpoints[ Address ] and Access_Input ) <> 0 ) then
                begin
                    Watchpoint_Notice( Address, Access_Input, 0, False, False, True ) ;
                end ;
                State_Change_Notice( State_Port_Input, True ) ;
                if( _Profiling ) then
                begin
                    TGigabyte_uCode_Profiler( Parent.Profiler ).Increment( Domain_Port_Inputs, Address ) ;
                end ;
                Log_Trace( 'Input ' + inttostr( Memory_Data_Latch ) + '. from port ' + inttostr( Address ) + '.' ) ;
            end ;
        end ;
    finally
        Bus_Read := Memory_Data_Latch ;
    end ;
end ;


function TGigabyte_uCode_CPU.Latched_Address : int64 ; { Return latched Address }

begin
    case _Latched_Address of
      Latch_PC : Result := _PC ;
      Latch_SP : Result := _SP ;
      Latch_DA1 : Result := _DA1 ;
      else Result := _DA ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Bus_Write( Address : int64 ; Size : integer ; Value : int64 ; IO_Type : longint ) ; { Write to memory }

var Component : TComponent ;
    Loop : integer ;
    UEC : TUnified_Exception ;

begin
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        UEC := Component.Write( Latched_Address, Value, Size, IO_Type) ;
        if( UEC <> nil ) then
        begin
            exit ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Increment_Clock( Count : integer ) ;

var R : extended ;
    W : int64 ; // Amount of time to wait (in picoseconds)

begin
    if( _Profiling ) then
    begin
        TGigabyte_uCode_Profiler( Parent.Profiler ).Increment_Clock( Count ) ;
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


procedure TGigabyte_uCode_CPU.Send_Signal( const Name : string ; Value : boolean ) ;

var Component : TComponent ;
    Index, Loop : integer ;

begin
    Index := -1 ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        Component.Set_Signal( PChar( Name ), Value ) ;
    end ;
    if( Name = 'INTA' ) then
    begin
        Index := 2 ;
    end else
    if( Name = 'LOCK' ) then
    begin
        Index := 4 ;
    end ;
    if( Index <> -1 ) then
    begin
        if( _Logger <> nil ) then
        begin
            _Logger.Log( Parent, PChar( Name + ' = ' + inttostr( ord( Value ) ) ), -1, True, LT_Sent_Signal ) ;
        end ;
        _UI.Signal_Change_Notice( Parent, Index, Value <> Parent.Signal_Active_Low( Index ) ) ;
    end ;
end ; // TGigabyte_uCode_CPU.Send_Signal


procedure TGigabyte_uCode_CPU.Clear_Watchpoints ;

begin
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
end ;


function TGigabyte_uCode_CPU.Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;

begin
    Result := TGigabyte_uCode_Assembler.Create ;
    TGigabyte_uCode_Assembler( Result ).CPU := self ;
    Result.Initialize( Master ) ;
    TGigabyte_uCode_Assembler( Result ).Base := Base ;
end ;


function TGigabyte_uCode_CPU.Cancel_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

var Index : integer ;

begin
    Result := Parent.Set_Error( Gigabyte_uCode_No_Breakpoint ) ; // Assume failure
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


function TGigabyte_uCode_CPU.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := _Memory_Watchpoints.Clear_Watchpoint( Address, Access ) ;
    end else
    begin
        Result := Parent.Set_Error( 0 ) ;
        _Port_Watchpoints[ Address ] :=
            _Port_Watchpoints[ Address ] and not( Access ) ;
    end ; // if( Memory )
end ; // TGigabyte_uCode_CPU.Clear_Watchpoint


function TGigabyte_uCode_CPU.Disassemble( Address : int64 ; Base, Size : longint ;
    Stream : TCOM_Stream ) : TUnified_Exception ;

    function _Disassemble : string ;

    label Ee ;

    var DText, Instruction : string ;
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


        function Fetch : int64 ; { Fetch next instruction }

        var A : int64 ;
            Temp : string ;

        begin
            A := Bus_Examine( Tpc ) ;
            Temp := Cvtb( 10, Base, Num1( A ) ) ;
            while( length( Temp ) < 8 ) do
            begin
                Temp := '0' + Temp ;
            end ;
            if( pos( Temp[ 1 ], '0123456789' ) = 0 ) then
            begin
                Instruction := Instruction + '0' ;
            end ;
            Temp := Temp + Base_Suffix( Base ) ;
            Instruction := Instruction + Temp + ' ' ;
            Fetch := A ;
            Tpc := Tpc + 1 ;
        end ;


        function Cvis( Value, Size : int64 ) : string ;

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


        function Source( Dst : boolean ; A : int64 ; B : word ) : string ;

        begin
            case A and ROM_Source_Destination of
                ROM_Source_Register_X : Result := 'X' ;
                ROM_Source_Register_Y : Result := 'Y' ;
                ROM_Source_Register_Z : Result := 'Z' ;
                ROM_Source_Register_Status : Result := 'SB' ;
                ROM_Source_Data : Result := cvis( B, 4 ) ;
                ROM_Source_Register_TS : Result := 'T(TS)' ;
                ROM_Source_Register_S : Result := 'T(S)' ;
                ROM_Source_Register_M : Result := 'T(M)' ;
                ROM_Source_Register_H : Result := 'T(H)' ;
                ROM_Source_Register_DW : Result := 'T(DW)' ;
                ROM_Source_Register_MO : Result := 'T(MO)' ;
                ROM_Source_Register_TD : Result := 'T(D)' ;
                ROM_Source_Register_TY : Result := 'T(Y)' ;
                ROM_Source_ALUA_Lo : if( Dst ) then
                                     begin
                                         Result := 'ALUAL' ;
                                     end else
                                     begin
                                         Result := 'ALUL' ;
                                     end ;
                ROM_Source_ALUA_Hi : if( Dst ) then
                                     begin
                                         Result := 'ALUAH' ;
                                     end else
                                     begin
                                         Result := 'ALUH' ;
                                     end ;
                ROM_Source_ALUB_Lo : Result := 'ALUBL' ;
                ROM_Source_ALUB_Hi : Result := 'ALUBH' ;
                ROM_Source_PC_Lo : Result := 'PCL' ;
                ROM_Source_PC_Hi : Result := 'PCH' ;
                ROM_Source_SP_Lo : Result := 'SPL' ;
                ROM_Source_SP_Hi : Result := 'SPH' ;
                ROM_Source_DA_Lo : Result := 'DAL' ;
                ROM_Source_DA_Hi : Result := 'DAH' ;
                ROM_Source_IP_Lo : Result := 'IPL' ;
                ROM_Source_IP_Hi : Result := 'IPH' ;
                ROM_Source_Temp_Lo : Result := 'TEMPL' ;
                ROM_Source_Temp_Hi : Result := 'TEMPH' ;
                ROM_Source_DA1_Lo : Result := 'DA1L' ;
                ROM_Source_DA1_Hi : Result := 'DA1H' ;
                else Result := '?' ;
            end ;
        end ;


        function ALU_Op( A : int64 ) : string ;

        begin
            case A of
                ALU_Complement : Result := 'COMPLEMENT' ;
                ALU_Increment : Result := 'INCREMENT' ;
                ALU_Decrement : Result := 'DECREMENT' ;
                ALU_AND : Result := 'AND' ;
                ALU_NAND : Result := 'NAND' ;
                ALU_OR : Result := 'OR' ;
                ALU_NOR : Result := 'NOR' ;
                ALU_XOR : Result := 'XOR' ;
                ALU_XNOR : Result := 'XNOR' ;
                ALU_ADD : Result := 'ADD' ;
                ALU_ADDC : Result := 'ADDC' ;
                ALU_SUB : Result := 'SUBTRACT' ;
                ALU_SUBB : Result := 'SUBTRACTB' ;
                ALU_Multiply : Result := 'MULTIPLY' ;
                ALU_DAA : Result := 'DAA' ;
                ALU_DAS : Result := 'DAS' ;
                ALU_SHL : Result := 'SHL' ;
                ALU_SHLC : Result := 'SHLC' ;
                ALU_SHR : Result := 'SHR' ;
                ALU_SHRC : Result := 'SHRC' ;
                else Result := '?' ;
            end ;
        end ;


        function IO_Size( A : int64 ) : string ;

        begin
            case A and ROM_Size of
                ROM_16_Bit : Result := '16' ;
                ROM_8_Bit : Result := '8' ;
            end ;
        end ;


    var A : int64 ;
        AA : string ;
        B : int64 ;

        procedure Add( const S : string ) ;

        begin
            if( AA <> '' ) then
            begin
                AA := AA + ' / ' ;
            end ;
            AA := AA + S ;
        end ;


    begin
        Tpc := Address div 5 ;
        Instruction := '' ;
        DText := '' ;
        A := Fetch ;
        AA := '' ;
        B := ( A shr ROM_Data_Shift ) and $FFFF ;
        case A and ROM_Internal_Operation of
            ROM_Internal_To_Bus : AA := Source( False, A, B ) + ' > BUS' ;
            ROM_Internal_From_Bus : AA := 'BUS > ' + Source( True, A, 0 ) ;
            ROM_Internal_Clear_Carry : AA := 'CC' ;
            ROM_Internal_Jump : AA := 'JUMP ' + cvis( B, 4 ) ;
            ROM_Internal_Halt : AA := 'HALT' ;
            ROM_Internal_Enable_Interrupts : AA := 'EI' ;
            ROM_Internal_Disable_Interrupts : AA := 'DI' ;
            ROM_Internal_ALU_Op : AA := ALU_Op( B ) ;
            ROM_Internal_Set_Bit_In_Z : AA := 'SBBZ' ;
            ROM_Internal_Clear_Bit_In_Z : AA := 'CBBZ' ;
            ROM_Internal_If_Bit_In_Z : AA := 'IFZS' ;
            ROM_Internal_If_Not_Bit_In_Z : AA :='IFZC' ;
            ROM_Internal_If_temp_Zero : AA := 'IFZ' ;
            ROM_Internal_If_Bit_In_Status : AA := 'IFSS' ;
            ROM_Internal_If_Not_Bit_In_Status : AA := 'IFSC' ;
            ROM_Internal_If_Bit_In_Temp : AA := 'IFTS' ;
            ROM_Internal_If_Not_Bit_In_Temp : AA := 'IFTC' ;
            ROM_Internal_If_Temp_Not_Zero : AA := 'IFNZ' ;
            ROM_Internal_Clear_Temp_Bit : AA := 'CT' ;
            ROM_Internal_Load_Resume_Register : AA := 'RESUME' ;
            ROM_Internal_If_Not_Kernel : AA := 'IFNK' ;
            ROM_Internal_Kernel_Check : AA := 'CHECK' ;
            ROM_Internal_Clear_Clock : AA := 'CLEAR CLOCK' ;
            ROM_Internal_Set_Temp_Bit : AA := 'ST' ;
        end ;
        case A and ROM_External_Operation of
            ROM_External_Read_Memory : Add( 'MREAD' + IO_Size( A ) ) ;
            ROM_External_Write_Memory : Add( 'MWRITE' + IO_Size( A ) ) ;
            ROM_External_Input : Add( 'INPUT' + IO_Size( A ) ) ;
            ROM_External_Output : Add( 'OUTPUT' + IO_Size( A ) ) ;
            ROM_External_Lock_Bus : Add( 'LOCK' ) ;
            ROM_External_Latch_SP : Add( 'LATCH SP' ) ;
            ROM_External_Latch_PC : Add( 'LATCH PC' ) ;
            ROM_External_Unlatch : Add( 'UNLATCH' ) ;
            ROM_External_Latch_DA1 : Add( 'LATCH DA1' ) ;
            ROM_External_Unlock_Bus : Add( 'UNLOCK' ) ;
            ROM_External_Set_INTA : Add( 'SINTA' ) ;
            ROM_External_Clear_INTA : Add( 'CINTA' ) ;
        end ;
        if( ( A and ROM_Inc_DA1 ) <> 0 ) then
        begin
            Add( '+DA1' ) ;
        end ;
        if( ( A and ROM_Inc_PC ) <> 0 ) then
        begin
            Add( '+PC' ) ;
        end ;
        if( ( A and ROM_Inc_DA ) <> 0 ) then
        begin
            Add( '+DA' ) ;
        end ;
        if( ( A and ROM_Inc_SP ) <> 0 ) then
        begin
            Add( '+SP' ) ;
        end ;
        if( ( A and ROM_Dec_SP ) <> 0 ) then
        begin
            Add( '-SP' ) ;
        end ;
        if( ( A and ROM_Dec_PC ) <> 0 ) then
        begin
            Add( '-PC' ) ;
        end ;
        if( ( A and ROM_Dec_Temp ) <> 0 ) then
        begin
            Add( '-Temp' ) ;
        end ;
        if( ( A and ROM_End ) <> 0 ) then
        begin
            Add( 'END' ) ;
        end ;

        if( AA = '' ) then
        begin
            AA := 'NOP' ;
        end ;

        _Disassemble := AA ;
    end ; { TGigabyte_uCode_CPU.Disassemble._Disassemble }

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
        Size := Size - 5 ;
    end ;
    Stream.Write( PChar( S )[ 0 ], length( S ) ) ;
end ; { TGigabyte_uCode_CPU.Disassemble }


function TGigabyte_uCode_CPU.Get_Clock_Speed : longint ;

begin
    Result := _Speed ;
end ;


procedure TGigabyte_uCode_CPU.Halt ;

begin
    _Halted := True ;
end ;


function TGigabyte_uCode_CPU.Halted : boolean ;

begin
    Result := _Halted ;
end ;


procedure TGigabyte_uCode_CPU.Run_From_Stream( Stream : TCOM_Stream ) ;

begin
    _Stream_PC_Offset := _RC ;
    _Run_Stream := Stream ;
    Execute( False, False ) ;
    _Run_Stream := nil ;
end ;


procedure TGigabyte_uCode_CPU.Run ;

begin
    _Halted := False ;
    Execute( False, False ) ;
end ;


procedure TGigabyte_uCode_CPU.Execute( Single_Step, Into : boolean ) ;

var Flags_Changed : boolean ; // Set to true if any flags change value
    Flags_Read : boolean ; // True if any flags are examined
    Operation : int64 ; // Current operation

    procedure _Set_Flag( Value : integer ) ;

    var Old : integer ;

    begin
        Old := _Status ;
        _Status := _Status or Value ;
        if( Old <> _Status ) then
        begin
            Flags_Changed := True ;
        end ;
    end ;


    procedure _Reset_Flag( Value : integer ) ;

    var Old : integer ;

    begin
        Old := _Status ;
        _Status := _Status and ( not Value ) ;
        if( Old <> _Status ) then
        begin
            Flags_Changed := True ;
        end ;
    end ;


    function Is_Flag_Set( Value : integer ) : boolean ;

    begin
        Result := ( ( _Status and Value ) <> 0 ) ;
        Flags_Read := True ;
    end ;


    procedure Set_C ; {Set carry flag}

    begin
        _Set_Flag( SBF_CF ) ;
    end ;


    procedure Reset_C ;

    begin
        _Reset_Flag( SBF_CF ) ;
    end ;


    function Is_C_Set : boolean ;

    begin
       Result := Is_Flag_Set( 1 ) ;
    end ;


    procedure Set_Z ; { Set zero flag }

    begin
        _Set_Flag( SBF_ZF ) ;
    end ;


    procedure Reset_Z ;

    begin
        _Reset_Flag( SBF_ZF ) ;
    end ;


    function Is_Z_Set : boolean ;

    begin
       Result := Is_Flag_Set( SBF_ZF ) ;
    end ;


    procedure Set_I ; { Set IE flag }

    begin
        _Set_Flag( SBF_IF ) ;
    end ;


    procedure Reset_I ;

    begin
        _Reset_Flag( SBF_IF ) ;
    end ;


    function Is_I_Set : boolean ;

    begin
       Result := Is_Flag_Set( SBF_IF ) ;
    end ;


    procedure Set_H ;

    begin
        _Set_Flag( SBF_HI ) ;
    end ;


    procedure Reset_H ;

    begin
        _Reset_Flag( SBF_HI ) ;
    end ;


    function Is_H_Set : boolean ;

    begin
       Result := Is_Flag_Set( SBF_HI ) ;
    end ;


    function Fetch : int64 ; { Fetch next operation }

    var Ch : char ;
        Size : longint ;
        I : integer ;
        R : int64 ;

    begin
        Result := 0 ;
        if( _Run_Stream <> nil ) then
        begin
            for I := 0 to 4 do
            begin
                if( ( _RC >= _Stream_PC_Offset ) and ( not _Run_Stream.At_End ) ) then
                begin
                    Size := 1 ;
                    _Run_Stream.Read( Ch, Size ) ;
                end ;
                R := ord( Ch ) ;
                R := R shl ( I * 8 ) ;
                Result := Result or R ;
            end ;
        end else
        begin
            Result := Bus_Examine( _RC ) ;
            RC := _RC + 1 ;
        end ;
    end ;


    procedure Add( _Carry : boolean ; ALUA, ALUB : int64 ) ;

    var Carry, High_Bits : integer ;

    begin
        Carry := 0 ;
        if( _Carry ) then
        begin
            Carry := _Status and 1 ;
        end ;
        _Status := _Status and not ( SBF_CF or SBF_ZF ) ;
        Flags_Changed := True ;
        High_Bits := 0 ;
        if( ALUA < 0 ) then
        begin
            inc( High_Bits ) ;
        end ;
        if( ALUB < 0 ) then
        begin
            inc( High_Bits ) ;
        end ;
        if( ( ALUA and $7FFFFFFFFFFFFFFF ) + ( ALUB and $7FFFFFFFFFFFFFFF ) + Carry < 0 ) then
        begin
            inc( High_Bits ) ;
        end ;
        ALU := ALUA + ALUB + Carry ;
        if( High_Bits > 1 ) then
        begin
            Set_C ;
        end ;
        if( ALU = 0 ) then
        begin
            Set_Z ;
        end ;
    end ;


    procedure Sub( _Borrow : boolean ; ALUA, ALUB : integer ) ;

    var Borrow : integer ;

    begin
        _Status := _Status and not SBF_CF ;
        if( ALUA < ALUB ) then
        begin
            _Status := _Status or SBF_CF ;
        end ;
        if( _Borrow ) then
        begin
            Borrow := _Status and 1 ;
            ALU := ALUA - ALUB - Borrow ;
        end else
        begin
            ALU := ALUA - ALUB ;
        end ;
    end ;


    procedure DAA ;

    var Carry : integer ;
        A, B : int64 ;
        S : integer ;
        _R, _A, _B : int64 ;

    begin
        Carry := _Status and 1 ;
        _Status := _Status and not SBF_CF ;
        S := 0 ;
        _R := 0 ;
        _A := _ALUA ;
        _B := _ALUB ;
        while( ( _A > 0 ) or ( _B > 0 ) or ( Carry > 0 ) ) do
        begin
            A := _A and $F ;
            B := _B and $F ;
            A := A + B + Carry ;
            Carry := 0 ;
            while( A > 9 ) do
            begin
                Carry := 1 ;
                A := ( A + 6 ) and $F ;
            end ;
            A := A shl S ;
            S := S + 4 ;
            _R := _R or A ;
            _A := _A shr 4 ;
            _B := _B shr 4 ;
        end ;
        ALU := _R ;
        _Status := _Status or Carry ;
    end ;


    procedure DAS ;

    var Carry : integer ;
        Dummy : integer ;
        A, B : int64 ;
        S : string ;
        _A, _B : int64 ;

    begin
        A := _ALUA ;
        B := _ALUB ;
        S := '' ;
        while( A > 0 ) do
        begin
            _A := A and $F ;
            A := A shr 4 ;
            if( _A > 9 ) then
            begin
                _A := 9 ;
            end ;
            S := inttostr( _A ) + S ;
        end ;
        A := strtoint( S ) ;
        S := '' ;
        while( B > 0 ) do
        begin
            _B := B and $F ;
            B := B shr 4 ;
            if( _B > 9 ) then
            begin
                _B := 9 ;
            end ;
            S := inttostr( _B ) + S ;
        end ;
        B := strtoint( S ) ;
        Carry := 0 ;
        if( A < B ) then
        begin
            Carry := 1 ;
        end ;
        A := A - B ;
        if( A < 0 ) then
        begin
            A := -A ;
        end ;
        S := inttostr( A ) ;
        A := 0 ;
        for Dummy := 1 to length( S ) do
        begin
            A := A or ( ord( S[ Dummy ] ) - 48 ) ;
            if( Dummy < length( S ) ) then
            begin
                A := A shl 4 ;
            end ;
        end ;
        ALU := A ;
        _Status := _Status or Carry ;
    end ;


    procedure ALU_Op ;

    var C : integer ;

    begin
        case ( Operation and ROM_Data ) shr ROM_Data_Shift of
            ALU_Complement : ALU := not ALUA ;
            ALU_Increment : Add( False, ALUA, 1 ) ;
            ALU_Decrement : Sub( False, ALUA, 1 ) ;
            ALU_AND : ALU := ALUA and ALUB ;
            ALU_NAND : ALU := not ( ALUA and ALUB ) ;
            ALU_OR : ALU := ALUA or ALUB ;
            ALU_NOR : ALU := not ( ALUA or ALUB ) ;
            ALU_XOR : ALU := ALUA xor ALUB  ;
            ALU_XNOR : ALU := not ( ALUA xor ALUB ) ;
            ALU_ADD : Add( False, ALUA, ALUB ) ;
            ALU_ADDC : Add( True, ALUA, ALUB ) ;
            ALU_SUB : Sub( False, ALUA, ALUB ) ;
            ALU_SUBB : Sub( True, ALUA, ALUB ) ;
            ALU_Multiply : ALU := ALUA * ALUB ;
            ALU_DAA : DAA ;
            ALU_DAS : DAS ;
            ALU_SHL : ALU := ALUA shl 1 ;
            ALU_SHLC :
                begin
                    C := _Status and 1 ;
                    _Status := _Status and not ( SBF_CF or SBF_ZF ) ;
                    if( ALUA < 0 ) then
                    begin
                        Set_C ;
                    end ;
                    ALU := ( ALUA shl 1 ) or C ;
                    if( ALU = 0 ) then
                    begin
                        Set_Z ;
                    end ;
                end ;
            ALU_SHR : ALU := ALUA shr 1 ;
            ALU_SHRC :
                begin
                    C := _Status and 1 ;
                    _Status := _Status and not ( SBF_CF or SBF_ZF ) ;
                    if( ( ALUA and 1 ) = 1 ) then
                    begin
                        Set_C ;
                    end ;
                    ALU := ( ALUA shr 1 ) ;
                    if( C = 1 ) then
                    begin
                        ALU := ALU or $8000000000000000 ;
                    end ;
                    if( ALU = 0 ) then
                    begin
                        Set_Z ;
                    end ;
                end ;
        end ;
    end ; // ALU_Op


    function Get_Source : longint ;

    begin
        case Operation and ROM_Source_Destination of
            ROM_Source_Register_X : Result := _Registers[ X ] ;
            ROM_Source_Register_Y : Result := _Registers[ Y ] ;
            ROM_Source_Register_Z : Result := _Registers[ Z ] ;
            ROM_Source_Register_Status : Result := _Status ;
            ROM_Source_Data : Result := ( Operation shr ROM_Data_Shift ) and $FFFF ;
            ROM_Source_Register_TS : Result := T_TS ;
            ROM_Source_Register_S : Result := T_S ;
            ROM_Source_Register_M : Result := T_M ;
            ROM_Source_Register_H : Result := T_H ;
            ROM_Source_Register_DW : Result := T_DW ;
            ROM_Source_Register_MO : Result := T_MO ;
            ROM_Source_Register_TD : Result := T_D ;
            ROM_Source_Register_TY : Result := T_Y ;
            ROM_Source_ALUA_Lo : Result := ALU and $FFFFFFFF ;
            ROM_Source_ALUA_Hi : Result := ALU shr 32 ;
            ROM_Source_PC_Lo : Result := PC and $FFFFFFFF ;
            ROM_Source_PC_Hi : Result := PC shr 32 ;
            ROM_Source_SP_Lo : Result := SP and $FFFFFFFF ;
            ROM_Source_SP_Hi : Result := SP shr 32 ;
            ROM_Source_DA_Lo : Result := DA and $FFFFFFFF ;
            ROM_Source_DA_Hi : Result := DA shr 32 ;
            ROM_Source_IP_Lo : Result := IP and $FFFFFFFF ;
            ROM_Source_IP_Hi : Result := IP shr 32 ;
            ROM_Source_Temp_Lo : Result := Temp and $FFFFFFFF ;
            ROM_Source_Temp_Hi : Result := Temp shr 32 ;
            ROM_Source_DA1_Lo : Result := DA1 and $FFFFFFFF ;
            ROM_Source_DA1_Hi : Result := DA1 shr 32 ;
            else Result := 0 ;
        end ;
    end ;


    procedure Set_Destination ;

    var I : int64 ;

    begin
        case Operation and ROM_Source_Destination of
            ROM_Source_Register_X : Registers[ X ] := Bus ;
            ROM_Source_Register_Y : Registers[ Y ] := Bus ;
            ROM_Source_Register_Z : Registers[ Z ] := Bus ;
            ROM_Source_Register_Status : Status := Bus and $F ;
            ROM_Source_Register_TS : T_TS := Bus and $F ;
            ROM_Source_Register_S : T_S := Bus and $FF ;
            ROM_Source_Register_M : T_M := Bus and $FF ;
            ROM_Source_Register_H : T_H := Bus and $FF ;
            ROM_Source_Register_DW : T_DW := Bus and $7 ;
            ROM_Source_Register_MO : T_MO := Bus and $F ;
            ROM_Source_Register_TD : T_D := Bus and $F ;
            ROM_Source_Register_TY : T_Y := Bus and $FFFF ;
            ROM_Source_ALUA_Lo : ALUA := ( _ALUA and $FFFFFFFF00000000 ) or Bus ;
            ROM_Source_ALUA_Hi :
                begin
                    I := Bus ;
                    I := I shl 32 ;
                    ALUA := ( _ALUA and $FFFFFFFF ) or I ;
                end ;
            ROM_Source_ALUB_Lo : ALUB := ( _ALUB and $FFFFFFFF00000000 ) or Bus ;
            ROM_Source_ALUB_Hi :
                begin
                    I := Bus ;
                    I := I shl 32 ;
                    ALUB := ( _ALUB and $FFFFFFFF ) or I ;
                end ;
            ROM_Source_PC_Lo : PC := ( _PC and $FFFFFFFF00000000 ) or Bus ;
            ROM_Source_PC_Hi :
                begin
                    I := Bus ;
                    I := I shl 32 ;
                    PC := ( _PC and $FFFFFFFF ) or I ;
                end ;
            ROM_Source_SP_Lo : SP := ( _SP and $FFFFFFFF00000000 ) or Bus ;
            ROM_Source_SP_Hi :
                begin
                    I := Bus ;
                    I := I shl 32 ;
                    SP := ( _SP and $FFFFFFFF ) or I ;
                end ;
            ROM_Source_DA_Lo : DA := ( _DA and $FFFFFFFF00000000 ) or Bus ;
            ROM_Source_DA_Hi :
                begin
                    I := Bus ;
                    I := I shl 32 ;
                    DA := ( _DA and $FFFFFFFF ) or I ;
                end ;
            ROM_Source_IP_Lo : IP := ( _IP and $FFFFFFFF00000000 ) or Bus ;
            ROM_Source_IP_Hi :
                begin
                    I := Bus ;
                    I := I shl 32 ;
                    IP := ( _IP and $FFFFFFFF ) or I ;
                end ;
            ROM_Source_Temp_Lo : Temp := ( _Temp and $FFFFFFFF00000000 ) or Bus ;
            ROM_Source_Temp_Hi :
                begin
                    I := Bus ;
                    I := I shl 32 ;
                    Temp := ( _Temp and $FFFFFFFF ) or I ;
                end ;
            ROM_Source_DA1_Lo : DA1 := ( _DA1 and $FFFFFFFF00000000 ) or Bus ;
            ROM_Source_DA1_Hi :
                begin
                    I := Bus ;
                    I := I shl 32 ;
                    DA1 := ( _DA1 and $FFFFFFFF ) or I ;
                end ;
        end ;
    end ;


    procedure Do_If( B : boolean ) ;

    begin
        if( B ) then
        begin
            RC := _RC + 2 ;
        end ;
    end ;


    function IO_Size : integer ;

    begin
        case Operation and ROM_Size of
            ROM_16_Bit : Result := 16 ;
            ROM_8_Bit : Result := 8 ;
            else Result := 32 ;
        end ;
    end ;


var B : integer ;
    Original_PC : longint ;

begin // TGigabyte_uCode_CPU.Execute
    // Setup...
    if( Paused ) then
    begin
        Do_Wait ;
        exit ;
    end ;
    if( _Halted ) then
    begin
        exit ;
    end ;
    Stopping := False ;
    Flags_Changed := False ;
    Flags_Read := False ;

    // Execution loop
    while( True ) do
    begin
        if( Blocked ) then
        begin
            Do_Wait ;
            continue ;
        end ;
        if( Flags_Changed ) then
        begin
            Flags_Changed := False ;
        end ;
        if( Flags_Read ) then
        begin
            Flags_Read := False ;
        end ;
        if( Stopping ) then
        begin
            Stopping := False ;
            exit ;
        end ;
        if( Lock_Mode = LM_Pending_Lock ) then
        begin
            Lock_Mode := LM_Locked ;
            Send_Signal( 'LOCK', True ) ;
        end else
        if( Lock_Mode = LM_Locked ) then
        begin
            Lock_Mode := LM_Unlocked ;
            Send_Signal( 'LOCK', False ) ;
        end ;
        if( Single_Step ) then
        begin
            exit ;
        end ;

        // Exit if we are done
        if( _Run_Stream <> nil ) then
        begin
            if( _Run_Stream.At_End ) then
            begin
                exit ;
            end ;
        end ;

        if( _Run_Stream = nil ) then
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
            TGigabyte_uCode_Profiler( Parent.Profiler ).Increment( Domain_Execution_Addresses, PC ) ;
            TGigabyte_uCode_Profiler( Parent.Profiler ).Increment( Domain_Other, Domain_Other_Instruction_Count ) ;
        end ;
        Log_Trace( 'Executing instruction at address ' +
            cvtb( 10, Base, inttostr( _RC ) ) + ': ' + Instruction_At( _RC ) ) ;
        Original_PC := _RC ;
        Operation := Fetch ;
        if( _Logger <> nil ) then
        begin
            _Logger.Update( Parent, Original_PC, Operation ) ;
        end ;

        if( _Profiling ) then
        begin
            TGigabyte_uCode_Profiler( Parent.Profiler ).Increment( Domain_Instructions, Operation ) ;
        end ;

        B := ( Operation shr ROM_Data_Shift ) and $FFFF ;
        case Operation and ROM_Internal_Operation of
            ROM_Internal_To_Bus : Bus := Get_Source ;
            ROM_Internal_From_Bus : Set_Destination ;
            ROM_Internal_Clear_Carry : Reset_C ;
            ROM_Internal_Jump : RC := B ;
            ROM_Internal_Halt : _Halted := True ;
            ROM_Internal_Enable_Interrupts : Reset_I ;
            ROM_Internal_Disable_Interrupts : Set_I ;
            ROM_Internal_ALU_Op : ALU_Op ;
            ROM_Internal_Set_Bit_In_Z : Registers[ Z ] := _Registers[ Z ] or Bit_Values[ Bus and 31 ] ;
            ROM_Internal_Clear_Bit_In_Z : Registers[ Z ] := _Registers[ Z ] and not Bit_Values[ Bus and 31 ] ;
            ROM_Internal_If_Bit_In_Z : Do_If( ( _Registers[ Z ] and Bit_Values[ Bus and 31 ] ) <> 0 ) ;
            ROM_Internal_If_Not_Bit_In_Z : Do_If( ( _Registers[ Z ] and Bit_Values[ Bus and 31 ] ) = 0 ) ;
            ROM_Internal_If_temp_Zero : Do_If( _Temp = 0 ) ;
            ROM_Internal_If_Bit_In_Status : Do_If( ( _Status and Bit_Values[ Bus and 7 ] ) <> 0 ) ;
            ROM_Internal_If_Not_Bit_In_Status : Do_If( ( _Status and Bit_Values[ Bus and 7 ] ) = 0 ) ;
            ROM_Internal_If_Bit_In_Temp : Do_If( ( _Temp and Bit_Values[ Bus and 31 ] ) <> 0 ) ;
            ROM_Internal_If_Not_Bit_In_Temp : Do_If( ( _Temp and Bit_Values[ Bus and 31 ] ) = 0 ) ;
            ROM_Internal_If_Temp_Not_Zero : Do_If( _Temp <> 0 ) ;
            ROM_Internal_Clear_Temp_Bit : Temp := _Temp and not Bit_Values[ Bus and 31 ] ;
            ROM_Internal_Load_Resume_Register : Resume := _RC ;
            ROM_Internal_If_Not_Kernel : Do_If( ( _Status and SBF_Mode ) <> 0 ) ;
            ROM_Internal_Set_Temp_Bit : Temp := _Temp or Bit_Values[ Bus and 31 ] ;
            ROM_Internal_Kernel_Check :
                if( ( _Status and SBF_Mode ) <> 0 ) then
                begin
                    _RC := ord( ROM_Read( $FFF8 ) ) ; // PINT address
                    RC := _RC shl 8 or ord( ROM_Read( $FFF9 ) ) ;
                end ;
            ROM_Internal_Clear_Clock :
                begin
                    T_TS := 0 ;
                    T_S := 0 ;
                    T_M := 0 ;
                    T_H := 0 ;
                    T_DW := 0 ;
                    T_D := 0 ;
                    T_MO := 0 ;
                    T_Y := 0 ;
                end ;
        end ;
        case Operation and ROM_External_Operation of
            ROM_External_Read_Memory : Bus := Bus_Read( Latched_Address, IO_Size, IO_Type_Memory ) ;
            ROM_External_Write_Memory : Bus_Write( Latched_Address, IO_Size, Bus, IO_Type_Memory ) ;
            ROM_External_Input : Bus := Bus_Read( Latched_Address, IO_Size, IO_Type_IO ) ;
            ROM_External_Output : Bus_Write( Latched_Address, IO_Size, Bus, IO_Type_IO ) ;
            ROM_External_Lock_Bus : Send_Signal( 'LOCK', True ) ;
            ROM_External_Latch_SP : _Latched_Address := Latch_SP ;
            ROM_External_Latch_PC : _Latched_Address := Latch_PC ;
            ROM_External_Unlatch : _Latched_Address := Latch_DA ;
            ROM_External_Latch_DA1 : _Latched_Address := Latch_DA1 ;
            ROM_External_Unlock_Bus : Send_Signal( 'LOCK', False ) ;
            ROM_External_Set_INTA : Send_Signal( 'INTA', True ) ;
            ROM_External_Clear_INTA : Send_Signal( 'INTA', False ) ;
        end ;
        if( ( Operation and ROM_Inc_DA1 ) <> 0 ) then
        begin
            DA1 := _DA1 + 2 ;
        end ;
        if( ( Operation and ROM_Inc_PC ) <> 0 ) then
        begin
            PC := _PC + 2 ;
        end ;
        if( ( Operation and ROM_Inc_DA ) <> 0 ) then
        begin
            DA := _DA + 2 ;
        end ;
        if( ( Operation and ROM_Inc_SP ) <> 0 ) then
        begin
            SP := _SP + 2 ;
        end ;
        if( ( Operation and ROM_Dec_SP ) <> 0 ) then
        begin
            SP := _SP - 2 ;
        end ;
        if( ( Operation and ROM_Dec_PC ) <> 0 ) then
        begin
            PC := _PC - 2 ;
        end ;
        if( ( Operation and ROM_Dec_Temp ) <> 0 ) then
        begin
            Temp := _Temp - 1 ;
        end ;
        if( _Run_Stream = nil ) then
        begin
            inc( _RC ) ;
        end ;
        if( ( Operation and ROM_End ) <> 0 ) then
        begin
            if( _Halted ) then
            begin
                exit ;
            end ;
        end ;
    end ; // while( True )
end ; // TGigabyte_uCode_CPU.Execute


procedure TGigabyte_uCode_CPU.Output( Port, Value : integer ; W : boolean ) ;

var Component : TComponent ;
    Loop : integer ;
    S : string ;
    Size : integer ;
    UEC : TUnified_Exception ;

begin
    if( W ) then
    begin
        Size := 16 ;
        S := 'word' ;
    end else
    begin
        Size := 8 ;
        S := 'byte' ;
    end ;
    try
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
            UEC := Component.Write( Port, Value, Size, IO_Type_IO ) ;
            if( UEC <> nil ) then
            begin
                exit ;
            end ;
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
                TGigabyte_uCode_Profiler( Parent.Profiler ).Increment( Domain_Port_Outputs, Port ) ;
            end ;
            Log_Trace( 'Output ' + inttostr( Value ) + ' (' + S + '). to port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ; // TGigabyte_uCode_CPU.Output


function TGigabyte_uCode_CPU.Get_PC : int64 ;

begin
    Result := _PC ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_PC ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_PC, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_PC( Value : int64 ) ;

begin
    _PC := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_PC ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_PC, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;



function TGigabyte_uCode_CPU.Get_SP : int64 ;

begin
    Result := _SP ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_SP ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_SP, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_SP( Value : int64 ) ;

begin
    _SP := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_SP ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_SP, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_IP : int64 ;

begin
    Result := _IP ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_IP ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_IP, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_IP( Value : int64 ) ;

begin
    _IP := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_IP ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_IP, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Temp : int64 ;

begin
    Result := _Temp ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Temp ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Temp, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Temp( Value : int64 ) ;

begin
    _Temp := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Temp ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Temp, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Status : byte ;

begin
    Result := _Status ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Status ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Status, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Status( Value : byte ) ;

begin
    _Status := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Status ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Status, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_ALUA : int64 ;

begin
    Result := _ALUA ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_ALUA ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_ALUA, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_ALUA( Value : int64 ) ;

begin
    _ALUA := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_ALUA ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_ALUA, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_ALUB : int64 ;

begin
    Result := _ALUB ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_ALUB ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_ALUB, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_ALUB( Value : int64 ) ;

begin
    _ALUB := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_ALUB ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_ALUB, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_ALU : int64 ;

begin
    Result := _ALU ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_ALU ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_ALU, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_ALU( Value : int64 ) ;

begin
    _ALU := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_ALU ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_ALU, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_RC : word ;

begin
    Result := _RC ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_RC ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_RC, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_RC( Value : word ) ;

begin
    _RC := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_RC ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_RC, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_DA : int64 ;

begin
    Result := _DA ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_DA ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_DA, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_DA( Value : int64 ) ;

begin
    _DA := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_DA ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_DA, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_DA1 : int64 ;

begin
    Result := _DA1 ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_DA1 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_DA1, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_DA1( Value : int64 ) ;

begin
    _DA1 := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_DA1 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_DA1, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_T_TS : byte ;

begin
    Result := _T_TS ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_TS ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_TS, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_T_TS( Value : byte ) ;

begin
    _T_TS := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_TS ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_TS, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_T_S : byte ;

begin
    Result := _T_S ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_S ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_S, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_T_S( Value : byte ) ;

begin
    _T_S := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_S ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_S, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_T_M : byte ;

begin
    Result := _T_M ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_M ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_M, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_T_M( Value : byte ) ;

begin
    _T_M := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_M ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_M, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_T_H : byte ;

begin
    Result := _T_H ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_H ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_H, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_T_H( Value : byte ) ;

begin
    _T_H := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_H ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_H, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_T_DW : byte ;

begin
    Result := _T_DW ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_DW ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_DW, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_T_DW( Value : byte ) ;

begin
    _T_DW := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_DW ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_DW, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_T_D : byte ;

begin
    Result := _T_D ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_D ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_D, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_T_D( Value : byte ) ;

begin
    _T_D := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_D ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_D, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_T_MO : byte ;

begin
    Result := _T_MO ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_MO ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_MO, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_T_MO( Value : byte ) ;

begin
    _T_MO := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_MO ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_MO, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_T_Y : word ;

begin
    Result := _T_Y ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_Y ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_Y, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_T_Y( Value : word ) ;

begin
    _T_Y := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_T_Y ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_T_Y, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Instruction : longint ;

begin
    Result := _Instruction ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Instruction ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Instruction, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Instruction( Value : longint ) ;

begin
    _Instruction := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Instruction ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Instruction, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_X : byte ;

begin
    Result := _X ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_X ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_X, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_X( Value : byte ) ;

begin
    _X := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_X ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_X, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Y : byte ;

begin
    Result := _Y ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Y ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Y, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Y( Value : byte ) ;

begin
    _Y := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Y ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Y, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Z : byte ;

begin
    Result := _Z ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Z ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Z, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Z( Value : byte ) ;

begin
    _Z := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Z ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Z, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Bus : longint ;

begin
    Result := _Bus ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Bus ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Bus, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Bus( Value : longint ) ;

begin
    _Bus := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Bus ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Bus, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Registers( Index : integer ) : longint ;

begin
    Result := _Registers[ Index - RI_Register_First ] ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ Index ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Registers( Index : integer ; Value : longint ) ;

begin
    _Registers[ Index ] := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ Index - RI_Register_First ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Resume : word ;

begin
    Result := _Resume ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Resume ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Resume, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Resume( Value : word ) ;

begin
    _Resume := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ RI_Resume ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( RI_Resume, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get__X : byte ;

begin
    Result := ( _Instruction shr 16 ) and 255 ;
end ;


procedure TGigabyte_uCode_CPU.Set__X( Value : byte ) ;

begin
    _Instruction := ( _Instruction and ( not $FF0000 ) ) or ( Value shl 16 ) ;
end ;


function TGigabyte_uCode_CPU.Get__Y : byte ;

begin
    Result := ( _Instruction shr 8 ) and 255 ;
end ;


procedure TGigabyte_uCode_CPU.Set__Y( Value : byte ) ;

begin
    _Instruction := ( _Instruction and ( not $FF00 ) ) or ( Value shl 8 ) ;
end ;


function TGigabyte_uCode_CPU.Get__Z : byte ;

begin
    Result := _Instruction and 255 ;
end ;


procedure TGigabyte_uCode_CPU.Set__Z( Value : byte ) ;

begin
    _Instruction := ( _Instruction and ( not 255 ) ) or Value ;
end ;


function TGigabyte_uCode_CPU.Set_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

begin
    if( ( Address < 0 ) or ( Address > Get_High_Memory ) ) then
    begin
        Result := Parent.Set_Error( Gigabyte_uCode_Invalid_Address ) ;
        exit ;
    end ;
    if( _Breakpoints.Indexof( Address ) <> -1 ) then
    begin
        Result := Parent.Set_Error( Gigabyte_uCode_Breakpoint_Exists ) ;
        exit ;
    end ;
    _Breakpoints.Add( Address ) ;
    Result := Parent.Set_Error( 0 ) ;
end ;


procedure TGigabyte_uCode_CPU.Set_Clock_Speed( Value : longint ) ;

begin
    _Speed := Value ;
end ;


procedure TGigabyte_uCode_CPU.Step( Into : boolean ) ;

begin
    _Halted := False ;
    Execute( True, Into ) ;
end ;


function TGigabyte_uCode_CPU.Translate( Space : integer ; Address : int64 ) : int64 ;

begin
    Translate := Address ;
end ;


function TGigabyte_uCode_CPU.Default_Base : integer ;

begin
    Result := 10 ;
end ;


function TGigabyte_uCode_CPU.Get_Low_Memory : int64 ;

begin
    Result := 0 ;
end ;


function TGigabyte_uCode_CPU.Get_High_Memory : int64 ;

begin
    Result := $7FFFFFFFFFFFFFFF ;
end ;


function TGigabyte_uCode_CPU.Get_Low_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TGigabyte_uCode_CPU.Get_High_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TGigabyte_uCode_CPU.Get_Low_Port : int64 ;

begin
    Result := 0 ;
end ;


function TGigabyte_uCode_CPU.Get_High_Port : int64 ;

begin
    Result := $7FFFFFFFFFFFFFFF ;
end ;


function TGigabyte_uCode_CPU.Support_Virtual_Address : boolean ;

begin
    Result := False ;
end ;


function TGigabyte_uCode_CPU.Segment_Size( Index : integer ) : integer ;

begin
    if( ( Index < 0 ) or ( Index >= Segments.Count ) ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := Segments[ Index ] ;
    end ;
end ;


function TGigabyte_uCode_CPU.Register_Name( Index : integer ) : PChar ;

begin
    case Index of
        RI_SP : Temp_Register_Name := 'SP' ;
        RI_PC : Temp_Register_Name := 'PC' ;
        RI_IP : Temp_Register_Name := 'IP' ;
        RI_Temp : Temp_Register_Name := 'Temp' ;
        RI_Status : Temp_Register_Name := 'SB' ;
        RI_ALUA : Temp_Register_Name := 'ALUA' ;
        RI_ALUB : Temp_Register_Name := 'ALUB' ;
        RI_ALU : Temp_Register_Name := 'ALU' ;
        RI_RC : Temp_Register_Name := 'RC' ;
        RI_DA : Temp_Register_Name := 'DA' ;
        RI_DA1 : Temp_Register_Name := 'DA1' ;
        RI_T_TS : Temp_Register_Name := 'T(TS)' ;
        RI_T_S : Temp_Register_Name := 'T(S)' ;
        RI_T_M : Temp_Register_Name := 'T(M)' ;
        RI_T_H : Temp_Register_Name := 'T(H)' ;
        RI_T_DW : Temp_Register_Name := 'T(DW)' ;
        RI_T_D : Temp_Register_Name := 'T(D)' ;
        RI_T_MO : Temp_Register_Name := 'T(MO)' ;
        RI_T_Y : Temp_Register_Name := 'T(Y)' ;
        RI_Instruction : Temp_Register_Name := 'Instruction' ;
        RI_X : Temp_Register_Name := 'X' ;
        RI_Y : Temp_Register_Name := 'Y' ;
        RI_Z : Temp_Register_Name := 'Z' ;
        RI_Bus : Temp_Register_Name := 'Bus' ;
        RI_Resume : Temp_Register_Name := 'Resume' ;
        RI_Register_First..RI_Register_Last : Temp_Register_Name := 'R' + inttostr( Index - RI_Register_First ) ;
        else Temp_Register_Name := '' ; // Invalid index
    end ;
    Result := PChar( Temp_Register_Name ) ;
end ;


function TGigabyte_uCode_CPU.Register_Size( Index : integer ) : integer ;

begin
    case Index of
        RI_SP : Result := 64 ;
        RI_PC : Result := 64 ;
        RI_IP : Result := 64 ;
        RI_Temp : Result := 64 ;
        RI_Status : Result := 8 ;
        RI_ALUA : Result := 64 ;
        RI_ALUB : Result := 64 ;
        RI_ALU : Result := 64 ;
        RI_RC : Result := 16 ;
        RI_DA : Result := 64 ;
        RI_DA1 : Result := 64 ;
        RI_T_TS : Result := 4 ;
        RI_T_S : Result := 8 ;
        RI_T_M : Result := 8 ;
        RI_T_H : Result := 8 ;
        RI_T_DW : Result := 3 ;
        RI_T_D : Result := 8 ;
        RI_T_MO : Result := 4 ;
        RI_T_Y : Result := 16 ;
        RI_Instruction : Result := 32 ;
        RI_X : Result := 8 ;
        RI_Y : Result := 8 ;
        RI_Z : Result := 8 ;
        RI_Bus : Result := 32 ;
        RI_Resume : Result := 16 ;
        RI_Register_First..RI_Register_Last : Result := 32 ;
        else Result := 0 ; // Invalid index
    end ;
end ;


function TGigabyte_uCode_CPU.Register_Description( Index : longint ) : PChar ;

begin
    Temp_Register_Description := '' ;
    if( Index = RI_Status ) then
    begin
        Temp_Register_Description := Get_Mask( _Status ) ;
    end ;
    Result := PChar( Temp_Register_Description ) ;
end ;


function TGigabyte_uCode_CPU.Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := Parent.Set_Error( Gigabyte_uCodeErr_No_Cache ) ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 21 ) ) then
        begin
            Result := Parent.Set_Error( Gigabyte_uCodeErr_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] := _Register_Watchpoints[ Address ] or Access ;
            Result := Parent.Set_Error( 0 ) ;
        end ;
    end ;
end ;


function TGigabyte_uCode_CPU.Top_Of_Stack( Index : integer ) : int64 ;

begin
    Top_Of_Stack := 0 ; // Microcode engine has no stack
end ;


function TGigabyte_uCode_CPU.Get_Target_Memory : TComponent ;

begin
    Result := ROM ;
end ;


function TGigabyte_uCode_CPU.Address_Representation_Ex( C : TComponent ;
    Base : integer ; Address : int64 ) : PChar ;

begin
    if( ( C <> nil ) and ( C = ROM ) ) then
    begin
        Address := Address div 5 ;
        Temp_Address_Representation := cvtb( 10, Base, inttostr( Address ) ) ;
        Result := PChar( Temp_Address_Representation ) ;
    end else
    begin
        Result := inherited Address_Representation_Ex( C, Base, Address ) ;
    end ;
end ;


function TGigabyte_uCode_CPU.Get_Current_Address( Space : integer ; Physical : boolean ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := _RC ;
    end else
    begin
        Result := 0 ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Set_Current_Address( Space : integer ; Physical : boolean ; Value : int64 ) ;

begin
    if( Space = 0 ) then
    begin
        _RC := Value ;
    end ;
end ;


procedure TGigabyte_uCode_CPU.Stop ;

begin
    Stopping := True ;
end ;


procedure TGigabyte_uCode_CPU.Restart ;

begin
    _Halted := False ;
    _RC := 0 ;
end ;


function TGigabyte_uCode_CPU.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var Dummy : integer ;
    Loop, Loop1 : integer ;
    Parser, Watchpoint_Parser : TXML_Parser ;
    S : string ;

begin
    Result := Set_Error( 0 ) ;

    // Setup default state...
    _Halted := False ;
    _Profiling := False ;
    fillchar( _Register_Watchpoints, sizeof( _Register_Watchpoints ), 0 ) ;
    _Breakpoints.Clear ;
    Clear_Watchpoints ;
    Stopping := False ;

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
            if( S = '<REGISTERS>' ) then
            begin
                S := Parser.Get_Section( 'Registers' ) ;
                S := copy( S, 2, length( S ) ) ; // Trim first bar
                Loop1 := 0 ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( '|', S ) ;
                    try
                        _Registers[ Loop1 ] :=
                            strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop1 ) ;
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
            if( S = '<STOPPING>' ) then
            begin
                Stopping := True ;
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
end ; // TGigabyte_uCode_CPU.Restore_State


function TGigabyte_uCode_CPU.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

    procedure Output( S : string ) ;

    begin
        Stream.Write( S[ 1 ], length( S ) ) ;
    end ;

var Loop, Loop1 : integer ;

begin
    Result := Set_Error( 0 ) ;
    Output( '<base>' + inttostr( Base ) + '</base>' ) ;
    if( _Trace ) then
    begin
        Output( '<trace/>' ) ;
    end ;
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

    Output( '<Registers>' ) ;
    for Loop1 := 0 to 255 do
    begin
        Output( '|' + inttostr( _Registers[ Loop1 ] ) ) ;
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
    if( Stopping ) then
    begin
        Output( '<stopping>' ) ;
    end ;
end ; // TGigabyte_uCode_CPU.Save_State


function TGigabyte_uCode_CPU.Clear_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := Parent.Set_Error( Gigabyte_uCodeErr_No_Cache ) ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 21 ) ) then
        begin
            Result := Parent.Set_Error( Gigabyte_uCodeErr_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] := _Register_Watchpoints[ Address ] and ( not Access_None ) ;
            Result := Parent.Set_Error( 0 ) ;
        end ;
    end ;
end ;



// TGigabyte_uCode methods...

{ API... }

function TGigabyte_uCode.CPU : TCPU ;

begin
    Result := _CPU ;
end ;


function TGigabyte_uCode.Facility_Code : longint ;

begin
    Result := Gigabyte_uCodeErr_Facility ;
end ;


function TGigabyte_uCode.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU := TGigabyte_uCode_CPU.Create ;
    _CPU._UI := UI ;
    _CPU.ROM := UI.Load_Component( 'S4M_RAM' ) ;
    _CPU.Parent := self ;
    Inputs := TList.Create ;
    Outputs := TList.Create ;
end ;


function TGigabyte_uCode.Terminate : TUnified_Exception ;

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


function TGigabyte_uCode.Serial_Number : integer ;

begin
    Result := _Serial_Number ;
end ;


function TGigabyte_uCode.Child_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TGigabyte_uCode.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Clear_Watchpoints ;
end ; // TGigabyte_uCode.Clear_Watchpoint


function TGigabyte_uCode.Component_Type : longint ;

begin
    Result := Component_Type_CPU ; // CPU
end ;


function TGigabyte_uCode.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( Gigabyte_uCodeErr_Invalid_Component ) ;
        exit ;
    end ;
    if( ( _CPU.ROM = nil ) and ( Component.Component_Type = Component_Type_Memory ) ) then
    begin
        _CPU.ROM := Component ;
    end ;
    if( Inputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( Gigabyte_uCodeErr_Already_Connected ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Result := Set_Error( Gigabyte_uCodeErr_Success ) ;
end ;


function TGigabyte_uCode.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( Gigabyte_uCodeErr_Invalid_Component ) ;
        exit ;
    end ;
    if( ( _CPU.ROM = nil ) and ( Component.Component_Type = Component_Type_Memory ) ) then
    begin
        _CPU.ROM := Component ;
    end ;
    if( Outputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( Gigabyte_uCodeErr_Already_Connected ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Result := Set_Error( Gigabyte_uCodeErr_Success ) ;
end ;


function TGigabyte_uCode.Debugger : TDebug_Interface ;

begin
    Result := nil ; // TODO
end ;


function TGigabyte_uCode.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var V : int64 ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        Result := Set_Error( Gigabyte_uCodeErr_No_Cache ) ;
    end else
    begin
        if( Address > RI_Register_Last ) then
        begin
            Result := Set_Error( Gigabyte_uCodeErr_Invalid_Register ) ;
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
        Size := Size + 7 ;
        Size := Size shr 3 ; // Number of bytes
        V := 0 ;
        move( Buffer^, V, Size ) ;
        case Address of
            RI_SP : _CPU._SP := V ;
            RI_PC : _CPU._PC := V ;
            RI_IP : _CPU._IP := V ;
            RI_Temp : _CPU._Temp := V ;
            RI_Status : _CPU._Status := V ;
            RI_ALUA : _CPU._ALUA := V ;
            RI_ALUB : _CPU._ALUB := V ;
            RI_ALU : _CPU._ALU := V ;
            RI_RC : _CPU._RC := V ;
            RI_DA : _CPU._DA := V ;
            RI_DA1 : _CPU._DA1 := V ;
            RI_T_TS : _CPU._T_TS := V ;
            RI_T_S : _CPU._T_S := V ;
            RI_T_M : _CPU._T_M := V ;
            RI_T_H : _CPU._T_H := V ;
            RI_T_DW : _CPU._T_DW := V ;
            RI_T_D : _CPU._T_D := V ;
            RI_T_MO : _CPU._T_MO := V ;
            RI_T_Y : _CPU._T_Y := V ;
            RI_Instruction : _CPU._Instruction := V ;
            RI_X : _CPU._X := V ;
            RI_Y : _CPU._Y := V ;
            RI_Z : _CPU._Z := V ;
            RI_Bus : _CPU._Bus := V ;
            RI_Resume : _CPU._Resume := V ;
            RI_Register_First..RI_Register_Last : _CPU._Registers[ Address - RI_Register_First ] := V ;
        end ; // case Address
    end ; // if( Memory )
end ; // TGigabyte_uCode.Deposit


function TGigabyte_uCode.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Result := Set_Error( Gigabyte_uCodeErr_Component_Not_Found ) ;
    end else
    begin
	    Result := Set_Error( Gigabyte_uCodeErr_Success ) ;
	    Inputs.Remove( Component ) ;
    end ;
end ;


function TGigabyte_uCode.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Result := Set_Error( Gigabyte_uCodeErr_Component_Not_Found ) ;
    end else
    begin
	    Result := Set_Error( Gigabyte_uCodeErr_Success ) ;
	    Outputs.Remove( Component ) ;
    end ;
end ;


function TGigabyte_uCode.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Size : integer ;
    V : int64 ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        Result := Set_Error( Gigabyte_uCodeErr_No_Cache ) ;
    end else
    begin
        if( Address > RI_Register_Last ) then
        begin
            Result := Set_Error( Gigabyte_uCodeErr_Invalid_Register ) ;
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
            RI_SP : V := _CPU._SP ;
            RI_PC : V := _CPU._PC ;
            RI_IP : V := _CPU._IP ;
            RI_Temp : V := _CPU._Temp ;
            RI_Status : V := _CPU._Status ;
            RI_ALUA : V := _CPU._ALUA ;
            RI_ALUB : V := _CPU._ALUB ;
            RI_ALU : V := _CPU._ALU ;
            RI_RC : V := _CPU._RC ;
            RI_DA : V := _CPU._DA ;
            RI_DA1 : V := _CPU._DA1 ;
            RI_T_TS : V := _CPU._T_TS ;
            RI_T_S : V := _CPU._T_S ;
            RI_T_M : V := _CPU._T_M ;
            RI_T_H : V := _CPU._T_H ;
            RI_T_DW : V := _CPU._T_DW ;
            RI_T_D : V := _CPU._T_D ;
            RI_T_MO : V := _CPU._T_MO ;
            RI_T_Y : V := _CPU._T_Y ;
            RI_Instruction : V := _CPU._Instruction ;
            RI_X : V := _CPU._X ;
            RI_Y : V := _CPU._Y ;
            RI_Z : V := _CPU._Z ;
            RI_Bus : V := _CPU._Bus ;
            RI_Resume : V := _CPU._Resume ;
            RI_Register_First..RI_Register_Last : V := _CPU._Registers[ Address - RI_Register_First ] ;
        end ; // case Address
        _Size := ( Size + 7 ) div 8 ; // Number of bytes
        move( V, Buffer^, _Size ) ;
    end ; // if( Memory )
end ; // TGigabyte_uCode.Examine


function TGigabyte_uCode.Get_Access_Mode( Address : int64 ; Memory : boolean ) : longint ;

begin
    if( Memory ) then
    begin
        Result := Access_None ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 21 ) ) then
        begin
            Result := Access_None ;
        end else
        begin
            Result := Access_All ;
        end ;
    end ;
end ;


function TGigabyte_uCode.Get_Profiling : boolean ;

begin
    Result := _CPU._Profiling ;
end ;


function TGigabyte_uCode.Get_Read_Latency : longint ;

begin
    Result := 0 ;
end ;


function TGigabyte_uCode.Get_Write_Latency : longint ;

begin
    Result := 0 ;
end ;


function TGigabyte_uCode.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
        Result := nil ;
        Set_Error( Gigabyte_uCodeErr_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Inputs[ Index ] ;
end ;


const Gigabyte_uCode_Name : string = 'CEF Gigabyte_uCode' ;

function TGigabyte_uCode.Name : PChar ;

begin
    Result := PChar( Gigabyte_uCode_Name ) ;
end ;


function TGigabyte_uCode.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Outputs.Count ) ) then
    begin
        Result := nil ;
        Set_Error( Gigabyte_uCodeErr_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Outputs[ Index ] ;
end ;


function TGigabyte_uCode.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ; // Doens't apply to CPUs
end ;


function TGigabyte_uCode.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
end ;


function TGigabyte_uCode.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Restore_State( Stream ) ;
end ;


function TGigabyte_uCode.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
end ;


function TGigabyte_uCode.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Save_State( Stream ) ;
end ;


function TGigabyte_uCode.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( Gigabyte_uCode_Invalid_Operation ) ;
end ;


procedure TGigabyte_uCode.Set_Profiling( _On, Children : boolean ) ;

begin
    _CPU._Profiling := _On ;
end ;


procedure TGigabyte_uCode.Set_Read_Latency( Value : longint ) ;

begin
    // Do nothing - we have no read latency
end ;


function TGigabyte_uCode.Set_Watchpoint( Address : int64 ; Memory : boolean ;
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
            Result := Set_Error( Gigabyte_uCode_Invalid_Address ) ;
            exit ;
        end ;
        _CPU._Port_Watchpoints[ Address ] := _CPU._Port_Watchpoints[ Address ] or Access ;
    end ;
end ; // TGigabyte_uCode.Set_Watchpoint


procedure TGigabyte_uCode.Set_Write_Latency( Value : longint ) ;

begin
    // Intentionally left blank - no latency
end ;


procedure TGigabyte_uCode.Show_Status ;

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


var Dummy : integer ;

begin
    Index := 0 ;
    S := 'RC=' + Show( _CPU._RC, 2 ) + '  Resume=' + Show( _CPU._RC, 2 ) ;
    Output( S ) ;
    S := 'PC=' + Show( _CPU._PC, 8 ) ;
    S := S + '    PCL=' + Show( _CPU._PC and $FFFFFFFF, 4 ) ;
    S := S + '    PCH=' + Show( _CPU._PC shr 32, 4 ) ;
    Output( S ) ;
    S := 'SP=' + Show( _CPU._Sp, 8 ) ;
    S := S + '    SPL=' + Show( _CPU._SP and $FFFFFFFF, 4 ) ;
    S := S + '    SPH=' + Show( _CPU._SP shr 32, 4 ) ;
    Output( S ) ;
    S := 'SB=' + Show( _CPU._Status, 1 ) + ' (' + Get_Mask( _CPU._Status ) + ')' ;
    Output( S ) ;
    S := 'Instruction=' + Show( _CPU._Instruction, 4 ) ;
    S := S + '    X=' + Show( _CPU._X, 1 ) ;
    S := S + '    Y=' + Show( _CPU._Y, 1 ) ;
    S := S + '    Z=' + Show( _CPU._Z, 1 ) ;
    Output( S ) ;
    S := 'IP=' + Show( _CPU._IP, 8 ) ;
    S := S + '    IPL=' + Show( _CPU._IP and $FFFFFFFF, 4 ) ;
    S := S + '    IPH=' + Show( _CPU._IP shr 32, 4 ) ;
    Output( S ) ;
    S := 'Temp=' + Show( _CPU._Temp, 8 ) ;
    S := S + '    TempL=' + Show( _CPU._Temp and $FFFFFFFF, 4 ) ;
    S := S + '    TempH=' + Show( _CPU._Temp shr 32, 4 ) ;
    Output( S ) ;
    S := 'ALUA=' + Show( _CPU._ALUA, 8 ) ;
    S := S + '    ALUAL=' + Show( _CPU._ALUA and $FFFFFFFF, 4 ) ;
    S := S + '    ALUAH=' + Show( _CPU._ALUA shr 32, 4 ) ;
    Output( S ) ;
    S := 'ALUB=' + Show( _CPU._ALUA, 8 ) ;
    S := S + '    ALUBL=' + Show( _CPU._ALUB and $FFFFFFFF, 4 ) ;
    S := S + '    ALUBH=' + Show( _CPU._ALUB shr 32, 4 ) ;
    Output( S ) ;
    S := 'ALU=' + Show( _CPU._ALU, 8 ) ;
    S := S + '    ALUL=' + Show( _CPU._ALU and $FFFFFFFF, 4 ) ;
    S := S + '    ALUH=' + Show( _CPU._ALU shr 32, 4 ) ;
    Output( S ) ;
    S := 'DA=' + Show( _CPU._DA, 8 ) ;
    S := S + '    DAL=' + Show( _CPU._DA and $FFFFFFFF, 4 ) ;
    S := S + '    DAH=' + Show( _CPU._DA shr 32, 4 ) ;
    Output( S ) ;
    S := 'DA1=' + Show( _CPU._DA1, 8 ) ;
    S := S + '    DA1L=' + Show( _CPU._DA1 and $FFFFFFFF, 4 ) ;
    S := S + '    DA1H=' + Show( _CPU._DA1 shr 32, 4 ) ;
    Output( S ) ;
    S := 'Bus=' + Show( _CPU._Bus, 8 ) ;
    Output( S ) ;
    S := 'T(TS)=' + Show( _CPU._T_TS, 1 ) + '  T(S)=' + Show( _CPU._T_S, 1 ) ;
    Output( S ) ;
    S := 'T(M)=' + Show( _CPU._T_M, 1 ) + '   T(H)=' + Show( _CPU._T_H, 1 ) ;
    Output( S ) ;
    S := 'T(D)=' + Show( _CPU._T_D, 1 ) + '   T(MO)=' + Show( _CPU._T_MO, 1 ) ;
    Output( S ) ;
    S := 'T(DW)=' + Show( _CPU._T_DW, 1 ) + '  T(Y)=' + Show( _CPU._T_Y, 2 ) ;
    Output( S ) ;
    for Dummy := 0 to 255 do
    begin
        S := 'R' + inttostr( Dummy ) + '=' + Show( _CPU._Registers[ Dummy ], 4 ) ;
        Output( S ) ;
    end ;
end ; // TGigabyte_uCode.Show_Status


function TGigabyte_uCode.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


procedure TGigabyte_uCode.Wake ;

begin
    _CPU.Blocked := False ;
end ;


function TGigabyte_uCode.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : integer ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := Value ;
end ;


function TGigabyte_uCode.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := ord( Value[ ( ( Size + 7 ) div 8 ) - 1 ] ) ;
end ;


procedure TGigabyte_uCode.Set_Up( P : PChar ) ;

var
Parser : TString_Parser ;
    S : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'ROM' ) then
        begin
            S := Parser.Token ;
            if( S = '=' ) then
            begin
                S := Parser.Token ;
            end ;
            _CPU.ROM.Set_Up( PChar( 'LOAD ' + S ) ) ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ;


procedure TGigabyte_uCode.Reset ;

begin
    CPU.Restart ;
end ;


procedure TGigabyte_uCode.Set_Signal( Name : PChar ; State : boolean ) ;

var Temp : string ;

begin
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    if( _CPU.Interrupts and ( Temp = 'INT' ) ) then
    begin
        if( _Logger <> nil ) then
        begin
            _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
        end ;
        _CPU.Do_Interrupt ;
    end ;
end ; // TGigabyte_uCode.Set_Signal


function TGigabyte_uCode.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

begin
    Result := False ;
end ;


function TGigabyte_uCode.Signal_Count : longint ;

begin
    Result := 5 ;
end ;


function TGigabyte_uCode.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := '' ;
            case Index of
                0 : Temp_Signal_Name := 'INT' ;
                1 : Temp_Signal_Name := 'SINT' ;
                2 : Temp_Signal_Name := 'INTA' ;
                3 : Temp_Signal_Name := 'RESET' ;
                4 : Temp_Signal_Name := 'LOCK' ;
            end ;
    if( Temp_Signal_Name = '' ) then
    begin
        Result := nil ;
    end else
    begin
        Result := PChar( Temp_Signal_Name ) ;
    end ;
end ;


function TGigabyte_uCode.Signal_Index( Name : PChar ) : integer ;

var S : string ;

begin
    S := string( Name ) ;
    if( S = 'INT' ) then
    begin
        Result := 0 ;
        exit ;
    end ;
    if( S = 'NMI' ) then
    begin
        Result := 1 ;
        exit ;
    end ;
    if( S = 'INTA' ) then
    begin
        Result := 2 ;
        exit ;
    end ;
    if( S = 'TEST' ) then
    begin
        Result := 3 ;
        exit ;
    end ;
    if( S = 'LOCK' ) then
    begin
        Result := 4 ;
        exit ;
    end ;
    Result := -1 ;
end ;


function TGigabyte_uCode.Signal_Out( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TGigabyte_uCode.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := Index < 3 ;
end ;


function TGigabyte_uCode.Get_State_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( ( Index >= 0 ) and ( Index <= 6 ) ) then
    begin
        case Index of
            State_Port_Input : Temp_Get_State_Name := 'Port input' ;
            State_Port_Output : Temp_Get_State_Name := 'Port output' ;
            State_Interrupt : Temp_Get_State_Name := 'Interrupt' ;
        end ;
        Result := PChar( Temp_Get_State_Name ) ;
    end ;
end ;


function TGigabyte_uCode.Get_Exception_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Temp_Get_Exception_Description := 'Invalid instruction' ;
                Result := PChar( Temp_Get_Exception_Description ) ;
            end ;
    end ;
end ;


procedure TGigabyte_uCode.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TGigabyte_uCode.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TGigabyte_uCode.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TGigabyte_uCode.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function TGigabyte_uCode.Profiler : TProfiler ;

begin
    if( _CPU._Profiler = nil ) then
    begin
        _CPU._Profiler := TGigabyte_uCode_Profiler.Create ;
    end ;
    Result := _CPU._Profiler ;
end ;


function TGigabyte_uCode.Get_Trace : boolean ;

begin
    Result := _CPU._Trace ;
end ;


procedure TGigabyte_uCode.Set_Trace( Value : boolean ) ;

begin
    _CPU._Trace := Value ;
end ;


function TGigabyte_uCode.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TGigabyte_uCode.Set_Logger( Value : TCEF_Logger ) ;

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

