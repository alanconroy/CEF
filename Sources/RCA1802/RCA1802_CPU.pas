{$N+}
{
        Program Name : RCA1802
        Package Name : CEF
        Purpose      : RCA CDP1802B CPU (CEF component)
        Institution  :
        Date Written : 9-Jan-2007
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

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	  This unit implements an RCA CDP1802 CPU emulator as a CEF component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit RCA1802_CPU ;

interface

uses { Borland... }
     Classes, { TList }

     { CEF... }
     _CEF, // TCEF_Logger
     CEF, { TBase_Profiler }
     _CEFUtil, // TCEF_Watchpoint

     { RCA 1802... }
     RCA1802_ASM, { TMode }

     { Other... }
     _DebugIn, { TDebug_Interface }
     CommonUt, { TInteger_List }
     _Streams, // TCOM_Stream
     _UE ; // Tunified_Exception

type TRegister_Set = array[ 0..15 ] of word ;

const SF_Predefined = 1 ; { Predefined symbol }

const RCA1802Err_Facility = 42 ;
      RCA1802Err_Success = 0 ;
      RCA1802Err_Invalid_Component = 1 ;
      RCA1802Err_Already_Connected = 2 ;
      RCA1802Err_Component_Not_Found = 3 ;
      RCA1802Err_No_Cache = 4 ;
      RCA1802Err_Invalid_Register = 5 ;
      RCA1802_No_Breakpoint = 6 ;
      RCA1802_Breakpoint_Exists = 7 ;
      RCA1802_Invalid_Address = 8 ;
      RCA1802_Invalid_Operation = 9 ;
      RCA1802_Invalid_Instruction = 10 ;
      RCA1802_IO_Trap = 11 ;
      RCA1802_NMI_Only_In_RCA1802 = 12 ;
      RCA1802_Nested_NMI = 13 ;

type TRCA1802_Profiler = class( TBase_Profiler )
                         private // Instance data...
                             // Profile data...
                             _Clock : integer ;
                             _Instruction_Count : integer ;
                             Port_Outputs : array[ 1..7 ] of integer ;
                             Port_Inputs : array[ 1..7 ] of integer ;
                             Execution_Addresses : array[ 0..$FFFF ] of integer ;
                             Instructions : array[ 0..511 ] of integer ;

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
                             Mode : TMode ;

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
                     end ; // TRCA1802_Profiler

type TRCA1802_CPU = class;
     TRCA1802 = class( TBase_Component )
                    private // Instance data...
                        _CPU : TRCA1802_CPU ;
                        Inputs : TList ;
                        Outputs : TList ;
                        _Parent : TComponent ;
                        _Tag : longint ;
                        _Logger : TCEF_Logger ;
                        Temp_Signal_Name : string ;
                        Temp_Get_State_Name : string ;
                        Temp_Get_Exception_Description : string ;

                    public // API...
                        _Serial_Number : integer ;

                        function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                            override ;

                        function Terminate : TUnified_Exception ; override ;

                        function Serial_Number : integer ; override ;

                        function Child_Component( Index : longint ) : TComponent ;
                            override ;

                        function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                            Access : longint ) : TUnified_Exception ; override ;

                        function Component_Type : longint ; override ;

                        function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                            override ;

                        function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                            override ;

                        function CPU : TCPU ; override ;

                        function Debugger : TDebug_Interface ; override ;

                        function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                            Memory : boolean ) : TUnified_Exception ; override ;

                        function Disconnect_Input( Component : TComponent ) : TUnified_Exception ;
                            override ;

                        function Disconnect_Output( Component : TComponent ) : TUnified_Exception ;
                            override ;

                        function Examine( Address : int64 ; var Size : longint ;
                            Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;
                            override ;

                        function Facility_Code : longint ; override ;

                        function Get_Access_Mode( Address : int64 ;
                            Memory : boolean ) : longint ; override ;

                        function Get_Profiling : boolean ; override ;

                        function Get_Read_Latency : longint ; override ;

                        function Get_Write_Latency : longint ; override ;

                        function Input_Component( Index : longint ) : TComponent ;
                            override ;

                        function Name : PChar ; override ;

                        function Output_Component( Index : longint ) : TComponent ;
                            override ;

                        function Read( Address : int64 ; Size : longint ;
                            IO_Type : longint ) : boolean ; override ;

                        function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                            override ;

                        function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                            override ;

                        function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                            override ;

                        function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                            override ;

                        function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                            Typ : longint ) : TUnified_Exception ; override ;

                        procedure Set_Profiling( _On, Children : boolean ) ;
                            override ;

                        procedure Set_Read_Latency( Value : longint ) ;
                            override ;

                        function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                            Access : longint ) : TUnified_Exception ; override ;

                        procedure Set_Write_Latency( Value : longint ) ;
                            override ;

                        procedure Show_Status ; override ;

                        function Support_Feature( ID : integer ) : boolean ;
                            override ; stdcall ;

                        procedure Wake ; override ;

                        function Write( Address : int64 ; Value, Size : longint ;
                            IO_Type : longint ) : TUnified_Exception ; override ;

                        function Write_String( Address : int64 ; Value : PChar ;
                            Size : longint ; IO_Type : longint ) : TUnified_Exception ;
                            override ;

                        procedure Set_Up( P : PChar ) ; override ;

                        procedure Reset ; override ;

                        procedure Set_Signal( Name : PChar ; State : boolean ) ;
                            override ;

                        function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                            override ;

                        function Signal_Count : longint ; override ;

                        function Signal_Name( Index : longint ) : PChar ;
                            override ;

                        function Signal_Out( Index : longint ) : boolean ;
                            override ;

                        function Signal_Active_Low( Index : longint ) : boolean ;
                            override ;

                        function Get_State_Name( Index : longint ) : PChar ;
                            override ;

                        function Get_Exception_Description( Index : longint ) : PChar ;
                           override ;

                        procedure Set_Tag( Value : longint ) ; override ;

                        function Get_Tag : longint ; override ;

                        function Get_Parent : TComponent ; override ;

                        procedure Set_Parent( Component : TComponent ) ;
                            override ;

                        function Profiler : TProfiler ; override ;

                        function Get_Trace : boolean ; override ;

                        procedure Set_Trace( Value : boolean ) ; override ;

                        function Signal_Index( Name : PChar ) : integer ;
                            override ;

                        function Get_Logger : TCEF_Logger ; override ;

                        procedure Set_Logger( Value : TCEF_Logger ) ; override ;
                end ; // TRCA1802

     TCounter_Mode = ( CM_Stopped, // Stopped
                       CM_EC1, // Event Counter 1
                       CM_EC2, // Event Counter 2
                       CM_Timer, // Timer
                       CM_PDM1, // Pulse Duration Measurement 1
                       CM_PDM2 // Pulse Duration Measurement 2
                     ) ;

     TRCA1802_CPU = class( TBase_CPU )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private // Generic CPU support...
            Parent : TRCA1802 ;
            _UI : TUI_Interface ;
            _Speed : integer ; // KHz
            _Register_Watchpoints : array[ 0..20 ] of integer ; // Access mode for registers
            _Profiling : boolean ; // True if profiling
            _Memory_Watchpoints : TCEF_Watchpoint_Manager ;
            _Port_Watchpoints : array[ 1..7 ] of integer ;
            _Breakpoints : TInteger_List ;
            Memory_Data_Latch : byte ; // Last byte sent to us
            _Run_Stream : TCOM_Stream ;
            _Stream_PC_Offset : integer ;
            _Profiler : TRCA1802_Profiler ;
            _Trace : boolean ; // True to trace execution
            Temp_Register_Name : string ;
            Temp_Register_Description : string ;
            Temp_Log_Trace : string ;
            Temp_Signal_Exception : string ;
            _RTS : TRun_Time_System ;
            _RTS_Flags : longint ;

        private // Processor internals...
            _Register_D : byte ;
            _Register_DF : boolean ;
            _Register_PX : byte ;
            _Register_T : byte ;
            _Register_IN : byte ;
            CH : byte ; // Counter jam value
            Counter : byte ;
            Prescalar : byte ;
            Counter_Mode : TCounter_Mode ;
            ETQ : boolean ; // Enable Toggle Q
            Q_FF : boolean ; // True if Q flipflop on
            IE : boolean ; // True if Master Interrupts enabled
            CIE : boolean ; // True if Counter Interrupts enabled
            XIE : boolean ; // True if External Interrupts enabled
            CI : boolean ; // True if pending Counter Interrupt
            _Register : TRegister_Set ; { General registers }
            _Halted : boolean ; // True if last instruction was a halt
            _Idle : boolean ; // True if in idle state
            EF1, EF2, EF3, EF4 : boolean ;
            Blocked : boolean ;
            Stopping : boolean ;
            Pending_DMA_In : boolean ;
            Pending_DMA_Out : boolean ;
            Pending_Interrupt : boolean ; // Pending external interrupt
            _Logger : TCEF_Logger ;

            function Get__PC : Word ;
            procedure Set__PC( Value : word ) ;
            function Get__Register_X : byte ;
            procedure Set__Register_X( Value : byte ) ;

            property _PC : word
                read Get__PC
                write Set__PC ;

            property _Register_X : byte
                read Get__Register_X
                write Set__Register_X ;

        private // Internal utility routines...
            procedure Decrement_Counter ;
            procedure Do_Wait ;
            function ByteRead( Address : Integer ) : Char ; { Return data at Address }
            function Byte_Read( Address : integer ) : integer ;
            function Word_Read( Address : integer ) : integer ;
            procedure ByteWrite( Address, Value : Integer ) ; { Write to memory }
            procedure Increment_Clock( Count : integer ) ;
            function Bus_Read( Address : Integer ; IO_Type : longint ) : Char ; { Return data at Address }
            function Bus_Examine( Address : Integer ) : Char ; { Return data at memory Address }
            procedure Execute( Single_Step, Into : boolean ) ;
            procedure Output( Port, Value : integer ) ;
            procedure DMA_Output( Value : integer ) ;
            function Input( Port : integer ) : integer ;
            function DMA_Input : integer ;
            procedure Send_Signal( const Name : string ; Value : boolean ) ;
            procedure Clear_Watchpoints ;
            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ;
            function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;
            function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;
            procedure State_Change_Notice( Index : integer ; State : boolean ) ;
            procedure Log_Trace( const Description : string ) ;
            procedure Signal_Exception( const Description : string ;
                Index : longint ) ;
            procedure Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
                Memory, Internal, Port : boolean ) ;
            function Instruction_At( Address : integer ) : string ;

            function Get_D : byte ;
            procedure Set_D( Value : byte ) ;
            function Get_DF : boolean ;
            procedure Set_DF( Value : boolean ) ;
            function Get_IN : byte ;
            procedure Set_IN( Value : byte ) ;
            function Get_PX : byte ;
            procedure Set_PX( Value : byte ) ;
            function Get_PC : word ;
            procedure Set_PC( Value : word ) ;
            function Get_T : byte ;
            procedure Set_T( Value : byte ) ;
            function Get_X : byte ;
            procedure Set_X( Value : byte ) ;

            function Get_Register( Index : integer ) : integer ;
            procedure Set_Register( Index : integer ; Value : integer ) ;

            property PC : word
                read Get_PC
                write Set_PC ;
            property Register_D : byte
                read Get_D
                write Set_D ;
            property Register_DF : boolean
                read Get_DF
                write Set_DF ;
            property Register_IN : byte
                read Get_IN
                write Set_IN ;
            property Register_PX : byte
                read Get_PX
                write Set_PX ;
            property Register_T : byte
                read Get_T
                write Set_T ;
            property Register_X : byte
                read Get_X
                write Set_X ;
            property Register[ Index : integer ] : integer
                read Get_Register
                write Set_Register ;

        public // API...
            Base : integer ;
            Mode : TMode ;

            function Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;
                override ;

            function Cancel_Breakpoint( Address : int64 ;
                Space : integer ; Physical : boolean ) : TUnified_Exception ; override ;

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

            function Register_Name( Index : integer ) : PChar ; override ;

            function Register_Size( Index : integer ) : integer ; override ;

            function Register_Description( Index : integer ) : PChar ;
                override ;

            procedure Restart ; override ;

            function Page_Size : longint ; override ;

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

            function Big_Endian : boolean ; override ;

            function Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;
                override ;
     end ; // TRCA1802

implementation

uses { Borland... }
     SysUtils, { Allocmem }

     { RCA 1802... }
     RCA1802_Util,

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
      // 1802 modes...
const State_DMA_IN = 3 ;
const State_DMA_OUT = 4 ;
const State_IDLE = 5 ;


const IO_Type_DMA = 2 ;


// Utility routines...

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
        0 : Result := 'IDL' ;
        1..15 : Result := 'LDN' ;
        $10..$1F : Result := 'INC' ;
        $20..$2F : Result := 'DEC' ;
        $30 : Result := 'BR' ;
        $31 : Result := 'BQ' ;
        $32 : Result := 'BZ' ;
        $33 : Result := 'BDF' ;
        $34 : Result := 'B1' ;
        $35 : Result := 'B2' ;
        $36 : Result := 'B3' ;
        $37 : Result := 'B4' ;
        $38 : Result := 'NBR' ;
        $39 : Result := 'BNQ' ;
        $3A : Result := 'BNZ' ;
        $3B : Result := 'BNF' ;
        $3C : Result := 'BN1' ;
        $3D : Result := 'BN2' ;
        $3E : Result := 'BN3' ;
        $3F : Result := 'BN4' ;
        $40..$4F : Result := 'LDA' ;
        $50..$5F : Result := 'STR' ;
        $60 : Result := 'IRX' ;
        $61..$67 : Result := 'OUT' ;
        $69..$6F : Result := 'IN' ;
        $70 : Result := 'RET' ;
        $71 : Result := 'DIS' ;
        $72 : Result := 'LDXA' ;
        $73 : Result := 'STXD' ;
        $74 : Result := 'ADC' ;
        $75 : Result := 'SDB' ;
        $76 : Result := 'SHRC' ;
        $77 : Result := 'SMB' ;
        $78 : Result := 'SAV' ;
        $79 : Result := 'MARK' ;
        $7A : Result := 'REQ' ;
        $7B : Result := 'SEQ' ;
        $7C : Result := 'ADCI' ;
        $7D : Result := 'SDBI' ;
        $7E : Result := 'SHLC' ;
        $7F : Result := 'SMBI' ;
        $80..$8F : Result := 'GLO' ;
        $90..$9F : Result := 'GHI' ;
        $A0..$AF : Result := 'PLO' ;
        $B0..$BF : Result := 'PHI' ;
        $C0 : Result := 'LBR' ;
        $C1 : Result := 'LBQ' ;
        $C2 : Result := 'LBZ' ;
        $C3 : Result := 'LBDF' ;
        $C4 : Result := 'NOP' ;
        $C5 : Result := 'LSNQ' ;
        $C6 : Result := 'LSNZ' ;
        $C7 : Result := 'LSNF' ;
        $C8 : Result := 'NLBR' ;
        $C9 : Result := 'LBNQ' ;
        $CA : Result := 'LBNZ' ;
        $CB : Result := 'LBNF' ;
        $CC : Result := 'LSIE' ;
        $CD : Result := 'LSQ' ;
        $CE : Result := 'LSZ' ;
        $CF : Result := 'LSDF' ;
        $D0..$DF : Result := 'SEP' ;
        $E0..$EF : Result := 'SEX' ;
        $F0 : Result := 'LDX' ;
        $F1 : Result := 'OR' ;
        $F2 : Result := 'AND' ;
        $F3 : Result := 'XOR' ;
        $F4 : Result := 'ADD' ;
        $F5 : Result := 'SD' ;
        $F6 : Result := 'SHR' ;
        $F7 : Result := 'SM' ;
        $F8 : Result := 'LDI' ;
        $F9 : Result := 'ORI' ;
        $FA : Result := 'ANI' ;
        $FB : Result := 'XRI' ;
        $FC : Result := 'ADI' ;
        $FD : Result := 'SDI' ;
        $FE : Result := 'SHL' ;
        $FF : Result := 'SMI' ;

        // 1806 instructions...
        256..511 :
            begin
                case I div 256 of
                    0 : Result := 'STPC' ;
                    1 : Result := 'DTC' ;
                    2 : Result := 'SPM2' ;
                    3 : Result := 'SCM2' ;
                    4 : Result := 'SPM1' ;
                    5 : Result := 'SCM1' ;
                    6 : Result := 'LDC' ;
                    7 : Result := 'STM' ;
                    8 : Result := 'GEC' ;
                    9 : Result := 'ETQ' ;
                    $A : Result := 'XIE' ;
                    $B : Result := 'XID' ;
                    $C : Result := 'CIE' ;
                    $D : Result := 'CID' ;
                    $20..$2F : Result := 'DBNZ' ;
                    $3E : Result := 'BCI' ;
                    $3F : Result := 'BXI' ;
                    $60..$6F : Result := 'RLXA' ;
                    $74 : Result := 'DADC' ;
                    $76 : Result := 'DSAV' ;
                    $77 : Result := 'DSMB' ;
                    $7C : Result := 'DACI' ;
                    $7F : Result := 'DSBI' ;
                    $80..$8F : Result := 'SCAL' ;
                    $90..$9F : Result := 'SRET' ;
                    $A0..$AF : Result := 'RSXD' ;
                    $B0..$BF : Result := 'RNX' ;
                    $C0..$CF : Result := 'RLDI' ;
                    $F4 : Result := 'DADD' ;
                    $F7 : Result := 'DSM' ;
                    $FC : Result := 'DADI' ;
                    $FF : Result := 'DSMI' ;
                end ;
            end ;
    end ;
end ; // Instruction_Name


// TRCA1802_Profiler methods...

// API...

procedure TRCA1802_Profiler.Generate_Report ;

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
    Base := 16 ;

    // Generate port output report
    for Loop := 1 to 7 do
    begin
        if( Port_Outputs[ Loop ] <> 0 ) then
        begin
            Outputs.Add( cvtb( 10, Base, inttostr( Loop ) ) + ': ' + inttostr( Port_Outputs[ Loop ] ) ) ;
        end ;
    end ;

    // Generate port input report
    for Loop := 1 to 7 do
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
    for Loop := 0 to 255 do
    begin
        if( Instructions[ Loop ] <> 0 ) then
        begin
            Instruction_Lines.Add( Instruction_Name( Loop ) + ': ' + inttostr( Instructions[ Loop ] ) ) ;
        end ;
    end ;
    if( Mode = M_1806 ) then
    begin
        for Loop := 0 to 255 do
        begin
            if( Instructions[ Loop + 256 ] <> 0 ) then
            begin
                Instruction_Lines.Add( Instruction_Name( Loop + 256 ) + ': ' + inttostr( Instructions[ Loop ] ) ) ;
            end ;
        end ;
    end ;
    Instruction_Lines.Sort ;
end ; // TRCA1802_Profiler.Generate_Report


procedure TRCA1802_Profiler.Increment( Domain, Index : integer ) ;

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


procedure TRCA1802_Profiler.Increment_Clock( Count : integer ) ;

begin
    Dirty := True ;
    _Clock := _Clock + Count ;
end ;


// Overrides...

procedure TRCA1802_Profiler.Clear( Domain : integer ) ;

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
end ; // TRCA1802_Profiler.Clear


function TRCA1802_Profiler.Domain_Name( Index : integer ) : PChar ;

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


function TRCA1802_Profiler.Report_Line( Domain, Index : integer ) : PChar ;

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
end ; // TRCA1802_Profiler.Report_Line



// TRCA1802_CPU methods...

// Constructors and destructors...

constructor TRCA1802_CPU.Create ;

begin
    inherited Create ;

    _Memory_Watchpoints := Get_Watchpoint_Manager ;
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
    _Breakpoints := TInteger_List.Create ;
    _Speed := 5000 ; // 5 MHz
    Base := Default_Base ;
end ;


destructor TRCA1802_CPU.Destroy ;

begin
    Clear_Watchpoints ;
    _Memory_Watchpoints.Terminate ;
    _Memory_Watchpoints := nil ;
    _Breakpoints.Free ;
    _Breakpoints := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TRCA1802_CPU.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := _Memory_Watchpoints.Clear_Watchpoint( Address, Access ) ;
    end else
    begin
        Result := Set_Error( 0 ) ;
        if( ( Address < Get_Low_Port ) or ( Address > Get_High_Port ) ) then
        begin
            Result := Set_Error( RCA1802_Invalid_Address ) ;
            exit ;
        end ;
        _Port_Watchpoints[ Address ] :=
            _Port_Watchpoints[ Address ] and not( Access ) ;
    end ; // if( Memory )
end ; // TRCA1802_CPU.Clear_Watchpoint


procedure TRCA1802_CPU.State_Change_Notice( Index : integer ; State : boolean ) ;

begin
    _UI.State_Change_Notice( Parent, Index, State ) ;

    case Index of
        State_Interrupt : Log_Trace( 'Process interrupt' ) ;
        State_DMA_IN : Log_Trace( 'Process DMA IN' ) ;
        State_DMA_OUT : Log_Trace( 'Process DMA OUT' ) ;
        State_IDLE : log_Trace( 'Process IDLE' ) ;
    end ;
end ;


procedure TRCA1802_CPU.Log_Trace( const Description : string ) ;

begin
    if( _Trace ) then
    begin
        Temp_Log_Trace := Description ;
        _UI.Log_Trace( Parent, PChar( Temp_Log_Trace ) ) ;
    end ;
end ;


procedure TRCA1802_CPU.Signal_Exception( const Description : string ; Index : longint ) ;

begin
    Temp_Signal_Exception := Description ;
    _UI.Signal_Exception( Parent, PChar( Temp_Signal_Exception ), Index ) ;
end ;


procedure TRCA1802_CPU.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
    Memory, Internal, Port : boolean ) ;

begin
    _UI.Watchpoint_Notice( Address, Access, Tag, Parent, Memory, Internal, Port ) ;
end ;


function TRCA1802_CPU.Instruction_At( Address : integer ) : string ;

var Stream : TCOM_String_Stream ;

begin
    Stream := TCOM_String_Stream.Create ;
    Disassemble( Address, Base, 1, Stream ) ;
    Result := string( Stream.As_String ) ;
    Stream.Detach ;
end ;


function TRCA1802_CPU.Get__PC : Word ;

begin
    Result := _Register[ _Register_PX and 15 ] ;
end ;


procedure TRCA1802_CPU.Set__PC( Value : word ) ;

begin
    _Register[ _Register_PX and 15 ] := Value ;
end ;


function TRCA1802_CPU.Get__Register_X : byte ;

begin
    Result := _Register_PX shr 4 ;
end ;


procedure TRCA1802_CPU.Set__Register_X( Value : byte ) ;

begin
    _Register_PX := ( _Register_PX and $F ) or ( Value shl 4 ) ;
end ;


function Simple_BCD_Add( Value1, Value2 : longint ) : longint ;

var BCD1, BCD2, BCDR : array[ 0..3 ] of byte ;
    Loop : integer ;
    Work : longint ;

begin
    fillchar( BCDR, 4, 0 ) ;
    move( Value1, BCD1, 4 ) ;
    move( Value2, BCD2, 4 ) ;
    Work := 0 ;
    for Loop := 0 to 3 do
    begin
        Work := ( BCD1[ Loop ] and $F ) + ( BCD2[ Loop ] and $F ) + Work ;
        if( ( Work and $F ) > 9 ) then
        begin
            Work := Work + 6 ;
        end ;
        BCDR[ Loop ] := Work and $F ;
        Work := Work shr 4 ;

        Work := ( BCD1[ Loop ] shr 4 ) + ( BCD2[ Loop ] shr 4 ) + Work ;
        if( ( Work and $F ) > 9 ) then
        begin
            Work := Work + 6 ;
        end ;
        BCDR[ Loop ] := BCDR[ Loop ] or ( ( Work and $F ) shl 4 ) ;
        Work := Work shr 4 ;
    end ;
    move( BCDR, Result, 4 ) ;
end ;


function Simple_BCD_Subtract( Value1, Value2 : longint ;
    var Borrow : boolean ) : longint ; // Value1 - Value2 - Borrow

var BCD1, BCD2, BCDR : array[ 0..3 ] of byte ;
    _Borrow : byte ;
    Inverted : boolean ;
    Loop : integer ;
    Work : longint ;

begin
    fillchar( BCDR, 4, 0 ) ;
    _Borrow := ord( Borrow ) ;
    if( Value1 >= Value2 + _Borrow ) then
    begin
        move( Value1, BCD1, 4 ) ;
        move( Value2, BCD2, 4 ) ;
        Inverted := False ;
    end else
    begin
        move( Value2, BCD1, 4 ) ;
        move( Value1, BCD2, 4 ) ;
        Inverted := True ;
    end ;
    for Loop := 0 to 3 do
    begin
        if( BCD1[ Loop ] and $F >= ( BCD2[ Loop ] and $F ) + _Borrow ) then
        begin
            Work := ( BCD1[ Loop ] and $F ) - ( BCD2[ Loop ] and $F ) - _Borrow ;
            _Borrow := 0 ;
        end else
        begin
            Work := ( BCD1[ Loop ] and $F ) + 10 - ( BCD2[ Loop ] and $F ) - _Borrow ;
            _Borrow := 1 ;
        end ;
        BCDR[ Loop ] := Work and $F ;

        if( BCD1[ Loop ] shr 4 >= ( BCD2[ Loop ] shr 4 ) + _Borrow ) then
        begin
            Work := ( BCD1[ Loop ] shr 4 ) - ( BCD2[ Loop ] shr 4 ) - _Borrow ;
            _Borrow := 0 ;
        end else
        begin
            Work := ( BCD1[ Loop ] shr 4 ) + 10 - ( BCD2[ Loop ] shr 4 ) - _Borrow ;
            _Borrow := 1 ;
        end ;
        BCDR[ Loop ] := BCDR[ Loop ] or ( Work shl 4 ) ;
    end ;
    move( BCDR, Result, 4 ) ;
    if( Inverted ) then // Need to make it a 10's complement
    begin
        Result := $9999 - Result + 1 ;
    end ;
    Borrow := not Inverted ;
end ; // Simple_BCD_Subtract


procedure TRCA1802_CPU.Decrement_Counter ;

begin
    dec( Counter ) ;
    if( Counter = 1 ) then
    begin
        Counter := CH ;
        if( ETQ ) then
        begin
            Q_FF := not Q_FF ;
            Send_Signal( 'Q', Q_FF ) ;
        end ;
        CI := True ;
    end ;
end ;


procedure TRCA1802_CPU.Do_Wait ;

begin
    try
        _UI.Idle( Parent ) ;
    except
    end ;
end ;


function TRCA1802_CPU.Bus_Examine( Address : Integer ) : Char ; { Return data at memory Address }

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


function TRCA1802_CPU.Bus_Read( Address : Integer ; IO_Type : longint ) : Char ; { Return data at Address }

var Component : TComponent ;
    Loop : integer ;

begin
    Memory_Data_Latch := 255 ; // Default if nothing responds
    try
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Address, 8, IO_Type ) ) then
            begin
                Bus_Read := char( Memory_Data_Latch ) ;
                exit ;
            end ;
        end ;
    finally
        Bus_Read := char( Memory_Data_Latch ) ;
    end ;
end ;


function TRCA1802_CPU.ByteRead( Address : Integer ) : Char ; { Return data at Address }

begin
    Result := Bus_Read( Address and $FFFF, IO_Type_Memory ) ;
end ;


function TRCA1802_CPU.Byte_Read( Address : integer ) : integer ;
{ Read a byte from the specified address }

begin
    Byte_Read := ord( ByteRead( Address ) ) ;
end ;


function TRCA1802_CPU.Word_Read( Address : integer ) : integer ;
{ Read a word from the specified address }

var X : integer ;

begin
    X := Byte_Read( Address ) ;
    X := X or ( swap( Byte_Read( Address + 1 ) ) and $FF00 ) ;
    Word_Read := X ;
end ;


procedure TRCA1802_CPU.ByteWrite( Address, Value : Integer ) ; { Write to memory }

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


procedure TRCA1802_CPU.Increment_Clock( Count : integer ) ;

var R : extended ;
    W : int64 ; // Amount of time to wait (in picoseconds)

begin
    if( _Profiling ) then
    begin
        TRCA1802_Profiler( Parent.Profiler ).Increment_Clock( Count ) ;
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
    if( Mode = M_1806 ) then
    begin
        dec( Prescalar ) ;
        if( Prescalar = 0 ) then
        begin
            if( Counter_Mode = CM_Timer ) then
            begin
                Decrement_Counter ;
            end ;
        end else
        begin
            Prescalar := Prescalar and 31 ;
        end ;
        if( ( Counter_Mode = CM_PDM1 ) and ( not EF1 ) ) then
        begin
            Decrement_Counter ;
        end else
        if( ( Counter_Mode = CM_PDM2 ) and ( not EF2 ) ) then
        begin
            Decrement_Counter ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Clear_Watchpoints ;

begin
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
end ;


function TRCA1802_CPU.Register_Description( Index : integer ) : PChar ;

begin
    Temp_Register_Description := '' ;
    if( ( _Register_PX and 15 ) = Index ) then
    begin
        Temp_Register_Description := '(PC)' ;
    end ;
    if( ( _Register_PX shr 4 ) = Index ) then
    begin
        if( Temp_Register_Description <> '' ) then
        begin
            Temp_Register_Description := Temp_Register_Description + ', ' ;
        end ;
        Temp_Register_Description := Temp_Register_Description + '(X)' ;
    end ;
    Result := PChar( Temp_Register_Description ) ;
end ;


function TRCA1802_CPU.Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;

begin
    Result := TRCA1802_Assembler.Create ;
    TRCA1802_Assembler( Result ).CPU := self ;
    Result.Initialize( Master ) ;
    TRCA1802_Assembler( Result ).Base := Base ;
    TRCA1802_Assembler( Result ).Mode := Mode ;
end ;


function TRCA1802_CPU.Cancel_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

var Index : integer ;

begin
    Result := Set_Error( RCA1802_No_Breakpoint ) ; // Assume failure
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
    Result := Set_Error( 0 ) ;
end ;


function TRCA1802_CPU.Get_Clock_Speed : longint ;

begin
    Result := _Speed ;
end ;


procedure TRCA1802_CPU.Halt ;

begin
    _Halted := True ;
end ;


function TRCA1802_CPU.Halted : boolean ;

begin
    Result := _Halted ;
end ;


procedure TRCA1802_CPU.Run_From_Stream( Stream : TCOM_Stream ) ;

begin
    _Run_Stream := Stream ;
    Execute( False, False ) ;
    _Run_Stream := nil ;
end ;


procedure TRCA1802_CPU.Run ;

begin
    _Halted := False ;
    Execute( False, False ) ;
end ;


function TRCA1802_CPU.Set_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

begin
    if( ( Address < 0 ) or ( Address > $FFFF ) ) then
    begin
        Result := Set_Error( RCA1802_Invalid_Address ) ;
        exit ;
    end ;
    if( _Breakpoints.Indexof( Address ) <> -1 ) then
    begin
        Result := Set_Error( RCA1802_Breakpoint_Exists ) ;
        exit ;
    end ;
    _Breakpoints.Add( Address ) ;
    Result := Set_Error( 0 ) ;
end ;


procedure TRCA1802_CPU.Set_Clock_Speed( Value : longint ) ;

begin
    _Speed := Value ;
end ;


procedure TRCA1802_CPU.Step( Into : boolean ) ;

begin
    _Halted := False ;
    Execute( True, Into ) ;
end ;


function TRCA1802_CPU.Page_Size : longint ;

begin
    Result := 256 ;
end ;


function TRCA1802_CPU.Clear_Internal_Watchpoint( Address : int64 ;
    Memory : boolean ; Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( RCA1802Err_No_Cache ) ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 30 ) ) then
        begin
            Result := Set_Error( RCA1802Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] := _Register_Watchpoints[ Address ] and ( not Access_None ) ;
            Result := Set_Error( 0 ) ;
        end ;
    end ;
end ;


function TRCA1802_CPU.Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( RCA1802Err_No_Cache ) ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 30 ) ) then
        begin
            Result := Set_Error( RCA1802Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] := _Register_Watchpoints[ Address ] or Access ;
            Result := Set_Error( 0 ) ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Stop ;

begin
    Stopping := True ;
end ;


procedure TRCA1802_CPU.Send_Signal( const Name : string ; Value : boolean ) ;

var Component : TComponent ;
    Index, Loop : integer ;

begin
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        Component.Set_Signal( PChar( Name ), Value ) ;
    end ;
    if( Name = 'INT' ) then
    begin
        Index := 0 ;
    end else
    if( Name = 'DMA_IN' ) then
    begin
        Index := 1 ;
    end else
    if( Name = 'DMA_OUT' ) then
    begin
        Index := 2 ;
    end else
    if( Name = 'Q' ) then
    begin
        Index := 3 ;
    end else
    begin
        exit ;
    end ;
    if( _Logger <> nil ) then
    begin
        _Logger.Log( Parent, PChar( Name + ' = ' + inttostr( ord( Value ) ) ), -1, True, LT_Sent_Signal ) ;
    end ;
    _UI.Signal_Change_Notice( Parent, Index, Value <> Parent.Signal_Active_Low( Index ) ) ;
end ; // TRCA1802_CPU.Send_Signal


function TRCA1802_CPU.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var Dummy : integer ;
    Loop, Loop1 : integer ;
    Parser, Watchpoint_Parser : TXML_Parser ;
    S : string ;

begin
    // Setup default state...
    Result := Set_Error( 0 ) ;
    Q_FF := False ;
    IE := False ;
    CI := False ;
    CIE := False ;
    XIE := False ;
    ETQ := False ;
    CH := 0 ;
    _Halted := False ;
    _Profiling := False ;
    Pending_DMA_In := False ;
    Pending_DMA_In := False ;
    Pending_Interrupt := False ;
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
            if( S = '<INTERRUPTS/>' ) then
            begin
                IE := True ;
            end else
            if( S = '<CIE/>' ) then
            begin
                CIE := True ;
            end else
            if( S = '<XIE/>' ) then
            begin
                XIE := True ;
            end else
            if( S = '<CI/>' ) then
            begin
                CI := True ;
            end else
            if( S = '<CH>' ) then
            begin
                S := Parser.Get_Section( 'ch' ) ;
                try
                    CH := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<COUNTERMODE>' ) then
            begin
                S := Parser.Get_Section( 'countermode' ) ;
                try
                    Counter_Mode := TCounter_Mode( strtoint( S ) ) ;
                except
                end ;
            end else
            if( S = '<ETQ>' ) then
            begin
                ETQ := True ;
            end else
            if( S = '<PRESCALAR>' ) then
            begin
                S := Parser.Get_Section( 'prescalar' ) ;
                try
                    Prescalar := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<COUNTER>' ) then
            begin
                S := Parser.Get_Section( 'counter' ) ;
                try
                    Counter := strtoint( S ) ;
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
            if( S = '<MODE>' ) then
            begin
                try
                    Mode := TMode( strtoint( S ) ) ;
                except
                end ;
            end else
            if( S = '<PENDING_DMA_IN/>' ) then
            begin
                Pending_DMA_In := True ;
            end ;
            if( S = '<PENDING_DMA_OUT/>' ) then
            begin
                Pending_DMA_Out := True ;
            end ;
            if( S = '<PENDING_INTEERUPT/>' ) then
            begin
                Pending_Interrupt := True ;
            end ;
            if( S = '<HALTED/>' ) then
            begin
                _Halted := True ;
            end else
            if( S = '<PROFILING/>' ) then
            begin
                _Profiling := True ;
            end else
            if( S = '<Q/>' ) then
            begin
                Q_FF := True ;
            end else
            if( S = '<DF/>' ) then
            begin
                _Register_DF := True ;
            end else
            if( S = '<IN>' ) then
            begin
                S := Parser.Get_Section( 'in' ) ;
                try
                    _Register_IN := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<PX>' ) then
            begin
                S := Parser.Get_Section( 'px' ) ;
                try
                    _Register_PX := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<T>' ) then
            begin
                S := Parser.Get_Section( 't' ) ;
                try
                    _Register_T := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<D>' ) then
            begin
                S := Parser.Get_Section( 'd' ) ;
                try
                    _Register_D := strtoint( S ) ;
                except
                end ;
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
                        _Register[ Loop1 ] := strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop1 ) ;
                    if( Loop1 > 15 ) then // Too many values
                    begin
                        break ;
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
end ; // TRCA1802_CPU.Restore_State


function TRCA1802_CPU.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

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
    if( IE ) then
    begin
        Output( '<Interrupts/>' ) ;
    end ;
    if( CIE ) then
    begin
        Output( '<CIE/>' ) ;
    end ;
    if( XIE ) then
    begin
        Output( '<XIE/>' ) ;
    end ;
    Output( '<ch>' + inttostr( CH ) + '</ch>' ) ;
    if( CI ) then
    begin
        Output( '<CI/>' ) ;
    end ;
    if( ETQ ) then
    begin
        Output( '<ETQ/>' ) ;
    end ;
    Output( '<countermode>' + inttostr( ord( Counter_Mode ) ) + '</countermode>' ) ;
    Output( '<counter>' + inttostr( Counter ) + '</counter>' ) ;
    Output( '<prescalar>' + inttostr( Prescalar ) + '</prescalar>' ) ;
    if( Q_FF ) then
    begin
        Output( '<Q/>' ) ;
    end ;
    if( _Register_DF ) then
    begin
        Output( '<DF/>' ) ;
    end ;
    Output( '<d>' + inttostr( _Register_D ) + '</d>' ) ;
    Output( '<t>' + inttostr( _Register_T ) + '</t>' ) ;
    Output( '<px>' + inttostr( _Register_PX ) + '</px>' ) ;
    Output( '<in>' + inttostr( _Register_IN ) + '</in>' ) ;
    Output( '<speed>' + inttostr( _Speed ) + '</speed>' ) ;
    Output( '<mode>' + inttostr( ord( Mode ) ) + '</mode>' ) ;
    if( Pending_DMA_In ) then
    begin
        Output( '<pending_dma_in/>' ) ;
    end ;
    if( Pending_DMA_In ) then
    begin
        Output( '<pending_dma_out/>' ) ;
    end ;
    if( Pending_Interrupt ) then
    begin
        Output( '<pending_interrupt/>' ) ;
    end ;
    if( _Halted ) then
    begin
        Output( '<Halted/>' ) ;
    end ;
    if( _Profiling ) then
    begin
        Output( '<Profiling/>' ) ;
    end ;

    Output( '<Registers>' ) ;
    for Loop1 := 0 to 15 do
    begin
        Output( '|' + inttostr( _Register[ Loop1 ] ) ) ;
    end ;
    Output( '</Registers>' ) ;

    Output( '<Breakpoints>' + _Breakpoints.Serialize + '</Breakpoints>' ) ;

    Output( '<Register_Watchpoints>' ) ;
    for Loop := 0 to 20 do
    begin
        Output( '|' + inttostr( _Register_Watchpoints[ Loop ] ) ) ;
    end ;
    Output( '</Register_Watchpoints>' ) ;

    Output( '<Memory_watchpoints>' ) ;
    Output( '<watchpoint>' + string( _Memory_Watchpoints.Serialize ) ) ;
    Output( '</Memory_watchpoints>' ) ;

    Output( '<Port_watchpoints>' ) ;
    for Loop := Get_Low_Port to Get_High_Port do
    begin
        Output( inttostr( _Port_Watchpoints[ Loop ] ) + '|' ) ;
    end ;
    Output( '</Port_watchpoints>' ) ;
end ; // TRCA1802_CPU.Save_State


function TRCA1802_CPU.Disassemble( Address : int64 ; Base, Size : longint ;
    Stream : TCOM_Stream ) : TUnified_Exception ;

    function _Disassemble : string ;

    label Ee ;

    var A : Integer ;
        DText, Instruction : string ;
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


        function FetchWord : Integer ; { Fetch next word }

        var A, B, C : Integer ;
            Temp : string ;

        begin
            A := Ord( Bus_Examine( Tpc ) ) ;
            B := Ord( Bus_Examine( Tpc + 1 ) ) ;
            Size := Size - 2 ;
            DText := DText + Chr( A ) + Chr( B ) ;
            C := B or Swap( A ) ;
            Temp := Cvtb( 10, Base, Num1( Cvtif( C ) ) ) ;
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
            FetchWord := B or swap( A ) ;
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


    begin
        Tpc := Address ;
        Instruction := '' ;
        DText := '' ;
        A := Fetch ;
        if( ( Mode = M_1806 ) and ( A = $68 ) ) then
        begin
            A := Fetch ;
            case A of
                0 : Result := 'STPC' ;
                1 : Result := 'DTC' ;
                2 : Result := 'SPM2' ;
                3 : Result := 'SCM2' ;
                4 : Result := 'SPM1' ;
                5 : Result := 'SCM1' ;
                6 : Result := 'LDC' ;
                7 : Result := 'STM' ;
                8 : Result := 'GEC' ;
                9 : Result := 'ETQ' ;
                $A : Result := 'XIE' ;
                $B : Result := 'XID' ;
                $C : Result := 'CIE' ;
                $D : Result := 'CID' ;
                $20..$2F : Result := 'DBNZ R' + inttostr( A and 15 ) + ',' + CVIS( FetchWord, 4 ) ;
                $3E : Result := 'BCI ' + CVIS( Fetch, 2 ) ;
                $3F : Result := 'BXI ' + CVIS( Fetch, 2 ) ;
                $60..$6F : Result := 'RLXA R' + inttostr( A and 15 ) ;
                $74 : Result := 'DADC' ;
                $76 : Result := 'DSAV' ;
                $77 : Result := 'DSMB' ;
                $7C : Result := 'DACI ' + CVIS( Fetch, 2 ) ;
                $7F : Result := 'DSBI ' + CVIS( Fetch, 2 ) ;
                $80..$8F : Result := 'SCAL R' + inttostr( A and 15 ) ;
                $90..$9F : Result := 'SRET R' + inttostr( A and 15 ) ;
                $A0..$AF : Result := 'RSXD R' + inttostr( A and 15 ) ;
                $B0..$BF : Result := 'RNX R' + inttostr( A and 15 ) ;
                $C0..$CF : Result := 'RLDI R' + inttostr( A and 15 ) + ',' + CVIS( Fetch, 2 ) ;
                $F4 : Result := 'DADD' ;
                $F7 : Result := 'DSM' ;
                $FC : Result := 'DADI ' + CVIS( Fetch, 2 ) ;
                $FF : Result := 'DSMI ' + CVIS( Fetch, 2 ) ;
                else _Disassemble := 'DB ' + Cvis( $68, 2 ) + Cvis( A, 2 ) ;
            end ;
        end else
        case A of
            0 : Result := 'IDL' ;
            1..15 : Result := 'LDN R' + inttostr( A and 15 ) ;
            $10..$1F : Result := 'INC R' + inttostr( A and 15 ) ;
            $20..$2F : Result := 'DEC R' + inttostr( A and 15 ) ;
            $30 : Result := 'BR ' + CVIS( Fetch, 2 ) ;
            $31 : Result := 'BQ ' + CVIS( Fetch, 2 ) ;
            $32 : Result := 'BZ ' + CVIS( Fetch, 2 ) ;
            $33 : Result := 'BDF ' + CVIS( Fetch, 2 ) ;
            $34 : Result := 'B1 ' + CVIS( Fetch, 2 ) ;
            $35 : Result := 'B2 ' + CVIS( Fetch, 2 ) ;
            $36 : Result := 'B3 ' + CVIS( Fetch, 2 ) ;
            $37 : Result := 'B4 ' + CVIS( Fetch, 2 ) ;
            $38 : Result := 'NBR' ;
            $39 : Result := 'BNQ ' + CVIS( Fetch, 2 ) ;
            $3A : Result := 'BNZ ' + CVIS( Fetch, 2 ) ;
            $3B : Result := 'BNF ' + CVIS( Fetch, 2 ) ;
            $3C : Result := 'BN1 ' + CVIS( Fetch, 2 ) ;
            $3D : Result := 'BN2 ' + CVIS( Fetch, 2 ) ;
            $3E : Result := 'BN3 ' + CVIS( Fetch, 2 ) ;
            $3F : Result := 'BN4 ' + CVIS( Fetch, 2 ) ;
            $40..$4F : Result := 'LDA R' + inttostr( A and 15 ) ;
            $50..$5F : Result := 'STR R' + inttostr( A and 15 ) ;
            $60 : Result := 'IRX' ;
            $61..$67 : Result := 'OUT R' + inttostr( A and 7 ) ;
            $69..$6F : Result := 'IN R' + inttostr( A and 7 ) ;
            $70 : Result := 'RET' ;
            $71 : Result := 'DIS' ;
            $72 : Result := 'LDXA' ;
            $73 : Result := 'STXD' ;
            $74 : Result := 'ADC' ;
            $75 : Result := 'SDB' ;
            $76 : Result := 'SHRC' ;
            $77 : Result := 'SMB' ;
            $78 : Result := 'SAV' ;
            $79 : Result := 'MARK' ;
            $7A : Result := 'REQ' ;
            $7B : Result := 'SEQ' ;
            $7C : Result := 'ADCI ' + CVIS( Fetch, 2 ) ;
            $7D : Result := 'SDBI ' + CVIS( Fetch, 2 ) ;
            $7E : Result := 'SHLC' ;
            $7F : Result := 'SMBI ' + CVIS( Fetch, 2 ) ;
            $80..$8F : Result := 'GLO R' + inttostr( A and 15 ) ;
            $90..$9F : Result := 'GHI R' + inttostr( A and 15 ) ;
            $A0..$AF : Result := 'PLO R' + inttostr( A and 15 ) ;
            $B0..$BF : Result := 'PHI R' + inttostr( A and 15 ) ;
            $C0 : Result := 'LBR ' + CVIS( FetchWord, 4 ) ;
            $C1 : Result := 'LBQ ' + CVIS( FetchWord, 4 ) ;
            $C2 : Result := 'LBZ ' + CVIS( FetchWord, 4 ) ;
            $C3 : Result := 'LBDF ' + CVIS( FetchWord, 4 ) ;
            $C4 : Result := 'NOP' ;
            $C5 : Result := 'LSNQ ' + CVIS( FetchWord, 4 ) ;
            $C6 : Result := 'LSNZ ' + CVIS( FetchWord, 4 ) ;
            $C7 : Result := 'LSNF ' + CVIS( FetchWord, 4 ) ;
            $C8 : Result := 'NLBR' ;
            $C9 : Result := 'LBNQ ' + CVIS( FetchWord, 4 ) ;
            $CA : Result := 'LBNZ ' + CVIS( FetchWord, 4 ) ;
            $CB : Result := 'LBNF ' + CVIS( FetchWord, 4 ) ;
            $CC : Result := 'LSIE ' + CVIS( FetchWord, 4 ) ;
            $CD : Result := 'LSQ ' + CVIS( FetchWord, 4 ) ;
            $CE : Result := 'LSZ ' + CVIS( FetchWord, 4 ) ;
            $CF : Result := 'LSDF ' + CVIS( FetchWord, 4 ) ;
            $D0..$DF : Result := 'SEP R' + inttostr( A and 15 ) ;
            $E0..$EF : Result := 'SEX R' + inttostr( A and 15 ) ;
            $F0 : Result := 'LDX' ;
            $F1 : Result := 'OR' ;
            $F2 : Result := 'AND' ;
            $F3 : Result := 'XOR' ;
            $F4 : Result := 'ADD' ;
            $F5 : Result := 'SD' ;
            $F6 : Result := 'SHR' ;
            $F7 : Result := 'SM' ;
            $F8 : Result := 'LDI ' + CVIS( Fetch, 2 ) ;
            $F9 : Result := 'ORI ' + CVIS( Fetch, 2 ) ;
            $FA : Result := 'ANI ' + CVIS( Fetch, 2 ) ;
            $FB : Result := 'XRI ' + CVIS( Fetch, 2 ) ;
            $FC : Result := 'ADI ' + CVIS( Fetch, 2 ) ;
            $FD : Result := 'SDI ' + CVIS( Fetch, 2 ) ;
            $FE : Result := 'SHL' ;
            $FF : Result := 'SMI ' + CVIS( Fetch, 2 ) ;
            else _Disassemble := 'DB ' + Cvis( A, 2 ) ;
        end ; // case A of
    end ; { TRCA1802_CPU.Disassemble._Disassemble }

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
end ; { TRCA1802_CPU.Disassemble }


procedure TRCA1802_CPU.Execute( Single_Step, Into : boolean ) ;

    function Fetch : Integer ; { Fetch next byte }

    var Ch : char ;
        Size : longint ;

    begin
        if( _Run_Stream <> nil ) then
        begin
            if( ( _PC < _Stream_PC_Offset ) or ( _Run_Stream.At_End ) ) then
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
        PC := _PC + 1 ;
    end ;


    function Fetch_Word : integer ; { Fetch a 2-byte word }

    var X : integer ;

    begin
        X := Fetch ;
        X := Fetch or swap( X ) ;
        Fetch_Word := X ;
        Increment_Clock( 1 ) ; // Extra byte read is another machine cycle
    end ;


    function Execute_RCA1802( A : integer ) : boolean ;

    var B : integer ;
        H : boolean ;

    begin
        Execute_RCA1802 := True ; { Assume success }
        Increment_Clock( 2 ) ; // All instructions take at least 2 machine cycles
        Register_IN := A ;
        if( ( A = $68 ) and ( Mode = M_1806 ) ) then
        begin
            A := Fetch ;
            Register_IN := A ;
            if( _Profiling ) then
            begin
                TRCA1802_Profiler( Parent.Profiler ).Increment( Domain_Instructions, A ) ;
            end ;
            Increment_Clock( 1 ) ; // Extra byte read is another machine cycle
            if( A = 0 ) then // STPC - Stop Counter
            begin
                Counter_Mode := CM_Stopped ;
                Prescalar := 0 ;
                exit ;
            end ;
            if( A = 1 ) then // DTC - Decrement timer/counter
            begin
                Decrement_Counter ;
                exit ;
            end ;
            if( A = 2 ) then // SPM2 - Set pulse width mode 2 and start
            begin
                Counter_Mode := CM_PDM2 ;
                Prescalar := 0 ;
                exit ;
            end ;
            if( A = 3 ) then // SCM2 - Set counter mode 2 and start
            begin
                Counter_Mode := CM_EC2 ;
                Prescalar := 0 ;
                exit ;
            end ;
            if( A = 4 ) then // SPM1 - Set pulse width mode 1 and start
            begin
                Counter_Mode := CM_PDM1 ;
                Prescalar := 0 ;
                exit ;
            end ;
            if( A = 5 ) then // SCM1 - Set counter mode 1 and start
            begin
                Counter_Mode := CM_EC1 ;
                Prescalar := 0 ;
                exit ;
            end ;
            if( A = 6 ) then // LDC - Load counter
            begin
                if( Counter_Mode = CM_Stopped ) then
                begin
                    CI := False ;
                    Counter := _Register_D ;
                    ETQ := False ;
                end ;
                CH := Register_D ;
                exit ;
            end ;
            if( A = 7 ) then // STM - Set Timer mode and start
            begin
                Counter_Mode := CM_Timer ;
                Counter := Prescalar ;
                exit ;
            end ;
            if( A = 8 ) then // GEC - Get Counter
            begin
                Register_D := Counter ;
                exit ;
            end ;
            if( A = 9 ) then // ETQ - Enable toggle Q
            begin
                ETQ := True ;
                exit ;
            end ;
            if( A = $A ) then // XIE - External interrupt enable
            begin
                XIE := True ;
                exit ;
            end ;
            if( A = $B ) then // XID - External interrupt disable
            begin
                XIE := False ;
                exit ;
            end ;
            if( A = $C ) then // CIE - Counter interrupt enable
            begin
                CIE := True ;
                exit ;
            end ;
            if( A = $D ) then // CID - Counter interrupt disable
            begin
                CIE := False ;
                exit ;
            end ;
            if( A = $3E ) then // BCI - Short branch on counter interrupt
            begin
                if( CI ) then
                begin
                    CI := False ;
                    ETQ := False ;
                    PC := ( _PC and $FF ) or Fetch ;
                    if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                    begin
                        _RTS.Jumped ;
                    end ;
                end else
                begin
                    PC := _PC + 1 ;
                end ;
                exit ;
            end ;
            if( A = $3F ) then // BXI - Short branch on external interrupt
            begin
                if( Pending_Interrupt ) then
                begin
                    Pending_Interrupt := False ;
                    PC := ( _PC and $FF ) or Fetch ;
                    if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                    begin
                        _RTS.Jumped ;
                    end ;
                end else
                begin
                    PC := _PC + 1 ;
                end ;
                exit ;
            end ;
            if( A = $74 ) then // DADC - Decimal Add with Carry
            begin
                A := Byte_Read( _Register[ _Register_X ] ) ;
                B := Simple_BCD_Add( _Register_D, A ) ;
                B := Simple_BCD_Add( B, ord( _Register_DF ) ) ; // Add-in the carry
                _Register_DF := ( B > $FF ) ;
                _Register_D := B and $FF ;
                exit ;
            end ;
            if( A = $76 ) then // DSAV - Save T, D, DF
            begin
                A := _Register[ _Register_X ] ;
                ByteWrite( A - 1, _Register_T ) ;
                ByteWrite( A - 2, _Register_D ) ;

                // Right-shift D with carry
                if( _Register_DF ) then
                begin
                    B := 128 ;
                end else
                begin
                    B := 0 ;
                end ;
                Register_DF := ( ( _Register_D and 1 ) <> 0 ) ;
                _Register_D := _Register_D shr 1 ;
                Register_D := _Register_D or B ;

                ByteWrite( A - 3, _Register_D ) ;
                Register[ _Register_X ] := A - 3 ;
                exit ;
            end ;
            if( A = $77 ) then // DSMB - Decimal subtract Memory with Borrow
            begin
                A := Byte_Read( _Register[ _Register_X ] ) ;
                B := Simple_BCD_Subtract( _Register_D, A, _Register_DF ) ;
                _Register_D := B and $FF ;
                exit ;
            end ;
            if( A = $F4 ) then // DADD - Decimal Add
            begin
                A := Byte_Read( _Register[ _Register_X ] ) ;
                B := Simple_BCD_Add( _Register_D, A ) ;
                _Register_DF := ( B > $FF ) ;
                _Register_D := B and $FF ;
                exit ;
            end ;
            if( A = $F7 ) then // DSM - Decimal Subtract memory
            begin
                A := Byte_Read( _Register[ _Register_X ] ) ;
                H := False ;
                B := Simple_BCD_Subtract( _Register_D, A, H ) ;
                _Register_DF := H ;
                _Register_D := B and $FF ;
                exit ;
            end ;
            if( A = $7C ) then // DACI - Decimal add with carry, immediate
            begin
                A := Fetch ;
                B := Simple_BCD_Add( _Register_D, A ) ;
                B := Simple_BCD_Add( B, ord( _Register_DF ) ) ;
                _Register_DF := ( B > $FF ) ;
                _Register_D := B and $FF ;
                exit ;
            end ;
            if( A = $7F ) then // DSBI - Decimal Subtract Memory with Borrow Immediate
            begin
                A := Fetch ;
                B := Simple_BCD_Subtract( _Register_D, A, _Register_DF ) ;
                Register_D := B and $FF ;
                exit ;
            end ;
            if( A = $FC ) then // DADI - Decimal Add Immediate
            begin
                A := Fetch ;
                B := Simple_BCD_Add( _Register_D, A ) ;
                _Register_DF := ( B > $FF ) ;
                _Register_D := B and $FF ;
                exit ;
            end ;
            if( A = $FF ) then // DSMI - Decimal subtract memory, immediate
            begin
                A := Fetch ;
                H := False ;
                B := Simple_BCD_Subtract( _Register_D, A, H ) ;
                _Register_DF := H ;
                _Register_D := B and $FF ;
                exit ;
            end ;
            if( ( A and $F0 ) = $20 ) then // DBNZ - Decrement register N and long branch is not equal 0
            begin
                A := A and 15 ;
                Register[ A ] := _Register[ A ] - 1 ;
                if( _Register[ A ] <> 0 ) then
                begin
                    PC := Fetch_Word ;
                end else
                begin
                    _PC := _PC + 2 ;
                end ;
                exit ;
            end ;
            if( ( A and $F0 ) = $60 ) then // RLXA - Register Load via X and advance
            begin
                Register[ A and 15 ] :=
                    swap( ord( ByteRead( _Register[ _Register_X ] ) ) )
                    or
                    ord( ByteRead( _Register[ _Register_X ] + 1 ) ) ;
                Register[ _Register_X ] := _Register[ _Register_X ] + 2 ;
                exit ;
            end ;
            if( ( A and $F0 ) = $80 ) then // SCAL - Standard call
            begin
                if( ( _RTS_Flags and RTS_Want_Calls ) <> 0 ) then
                begin
                    if( _RTS.Call( swap( ord( ByteRead( _Register[ A and 15 ] ) ) )
                      or
                      ord( ByteRead( _Register[ A and 15 ] + 1 ) ) ) ) then
                    begin
                        exit ;
                    end ;
                end ;
                B := _Register[ A and 15 ] ;
                ByteWrite( _Register[ _Register_X ], B and $FF ) ;
                ByteWrite( _Register[ _Register_X ] - 1, swap( B ) and $FF ) ;
                Register[ _Register_X ] := _Register[ _Register_X ] - 2 ;
                Register[ A and 15 ] := _PC ;

                PC := swap( ord( ByteRead( _Register[ A and 15 ] ) ) )
                      or
                      ord( ByteRead( _Register[ A and 15 ] + 1 ) ) ;
                Register[ A and 15 ] := _Register[ A and 15 ] + 2 ;
                exit ;
            end ;
            if( ( A and $F0 ) = $90 ) then // SRET - Standard return
            begin
                if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                begin
                    if( _RTS.Return ) then
                    begin
                        exit ;
                    end ;
                end ;
                PC := _Register[ A and 15 ] ;
                Register[ A and 15 ] :=
                    swap( ord( ByteRead( _Register[ _Register_X ] + 1 ) ) )
                    or
                    ord( ByteRead( _Register[ _Register_X ] + 2 ) ) ;
                Register[ _Register_X ] := _Register[ _Register_X ] + 2 ;
                exit ;
            end ;
            if( ( A and $F0 ) = $A0 ) then // RSXD - Register store via X and decrement
            begin
                A := A and 15 ;
                ByteWrite( _Register[ _Register_X ], _Register[ A ] and $FF ) ;
                ByteWrite( _Register[ _Register_X ] - 1, ( _Register[ A ] shr 8 ) and $FF ) ;
                Register[ Register_X ] := _Register[ _Register_X ] - 2 ;
                exit ;
            end ;
            if( ( A and $F0 ) = $B0 ) then // RNX - Register N to register X copy
            begin
                Register[ _Register_X ] := Register[ A and 15 ] ;
                exit ;
            end ;
            if( ( A and $F0 ) = $C0 ) then // RLDI - Register Load Immediate
            begin
                Register[ A and 15 ] := Fetch_Word ;
                exit ;
            end ;
            A := $6800 or A ;
        end ;

        if( A = 0 ) then { IDL }
        begin
            _Idle := True ;
            State_Change_Notice( State_Idle, True ) ;
            Exit ;
        end ;
        if( ( A >= 1 ) and ( A <= 15 ) ) then // LDN
        begin
            Register_D := ord( ByteRead( _Register[ A and 15 ] ) ) ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $10 ) then // INC
        begin
            Register[ A and 15 ] := _Register[ A and 15 ] + 1 ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $20 ) then // DEC
        begin
            Register[ A and 15 ] := _Register[ A and 15 ] - 1 ;
            Exit ;
        end ;
        if( A = $30 ) then // BR
        begin
            PC := ( _PC and ( not $FF ) ) or Fetch ;
            if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
            begin
                _RTS.Jumped ;
            end ;
            Exit ;
        end ;
        if( A = $31 ) then // BQ
        begin
            if( Q_FF ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $32 ) then // BZ
        begin
            if( _Register_D <> 0 ) then
            begin
                Fetch ;
            end else
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end ;
            Exit ;
        end ;
        if( A = $33 ) then // BDF
        begin
            if( _Register_DF ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $34 ) then // B1
        begin
            if( EF1 ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $35 ) then // B2
        begin
            if( EF2 ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $36 ) then // B3
        begin
            if( EF3 ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $37 ) then // B4
        begin
            if( EF4 ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $38 ) then // NBR
        begin
            PC := _PC + 1 ;
            if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
            begin
                _RTS.Jumped ;
            end ;
            Exit ;
        end ;
        if( A = $39 ) then // BNQ
        begin
            if( not Q_FF ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $3A ) then // BNZ
        begin
            if( _Register_D = 0 ) then
            begin
                Fetch ;
            end else
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end ;
            Exit ;
        end ;
        if( A = $3B ) then // BNF
        begin
            if( Register_DF ) then
            begin
               Fetch ;
            end else
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end ;
            Exit ;
        end ;
        if( A = $3C ) then // BN1
        begin
            if( not EF1 ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $3D ) then // BN2
        begin
            if( not EF2 ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $3E ) then // BN3
        begin
            if( not EF2 ) then
            begin
                PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( A = $3F ) then // BN4
        begin
            if( not EF4 ) then
            begin
               PC := ( _PC and ( not $FF ) ) or Fetch ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
            end else
            begin
               Fetch ;
            end ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $40 ) then // LDA
        begin
            B := _Register[ A and 15 ] ;
            Register_D := ord( ByteRead( B ) ) ;
            Register[ A and 15 ] := B + 1 ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $50 ) then // STR
        begin
            ByteWrite( _Register[ A and 15 ], Register_D ) ;
            Exit ;
        end ;
        if( A = $60 ) then // IRX
        begin
            Register[ _Register_X  ] := _Register[ _Register_X ] + 1 ;
            Exit ;
        end ;
        if( ( A >= $61 ) and ( A <= $67 ) ) then // OUT
        begin
            Output( A and 7, Byte_Read( _Register[ _Register_X ] ) ) ;
            Register[ _Register_X ] := _Register[ _Register_X ] + 1 ;
            Exit ;
        end ;
        if( ( A >= $69 ) and ( A <= $6F ) ) then // IN
        begin
            Register_D := Input( A and 7 ) ;
            ByteWrite( _Register[ _Register_X ], _Register_D ) ;
            Exit ;
        end ;
        if( A = $70 ) then // RET
        begin
            if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
            begin
                if( _RTS.Return ) then
                begin
                    exit ;
                end ;
            end ;
            B := _Register_X ;
            Register_PX := Byte_Read( _Register[ B ] ) ;
            Register[ B ] := _Register[ B ] + 1 ;
            IE := True ;
            Exit ;
        end ;
        if( A = $71 ) then // DIS
        begin
            B := _Register_X ;
            Register_PX := Byte_Read( _Register[ B ] ) ;
            Register[ B ] := _Register[ B ] + 1 ;
            IE := False ;
            Exit ;
        end ;
        if( A = $72 ) then // LDXA
        begin
            B := _Register[ Register_X ] ;
            Register_D := ord( Byte_Read( B ) ) ;
            Register[ _Register_X ] := B + 1 ;
            Exit ;
        end ;
        if( A = $73 ) then // STXD
        begin
            B := _Register[ Register_X ] ;
            ByteWrite( B, Register_D ) ;
            Register[ _Register_X ] := B - 1 ;
            Exit ;
        end ;
        if( A = $74 ) then // ADC
        begin
            B := _Register_D + Byte_Read( _Register[ _Register_X ] ) ;
            if( _Register_DF ) then
            begin
                inc( B ) ;
            end ;
            Register_DF := ( B > $FF ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $75 ) then // SDB
        begin
            B := Byte_Read( _Register[ _Register_X ] ) - Register_D ;
            if( not _Register_DF ) then
            begin
                dec( B ) ;
            end ;
            Register_DF := ( B >= 0 ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $76 ) then // SHRC
        begin
            if( _Register_DF ) then
            begin
                B := 128 ;
            end else
            begin
                B := 0 ;
            end ;
            Register_DF := ( ( _Register_D and 1 ) <> 0 ) ;
            _Register_D := _Register_D shr 1 ;
            Register_D := _Register_D or B ;
            Exit ;
        end ;
        if( A = $77 ) then // SMB
        begin
            B := _Register_D - Byte_Read( _Register[ _Register_X ] ) ;
            if( not _Register_DF ) then
            begin
                dec( B ) ;
            end ;
            Register_DF := ( B >= 0 ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $78 ) then // SAV
        begin
            ByteWrite( _Register[ _Register_X ], _Register_T ) ;
            Exit ;
        end ;
        if( A = $79 ) then // MARK
        begin
            Register_T := _Register_PX ;
            ByteWrite( _Register[ 2 ], _Register_PX ) ;
            Register_PX := ( ( _Register_PX shl 4 ) or ( _Register_PX and $F ) ) and $FF ;
            Register[ 2 ] := _Register[ 2 ] - 1 ;
            Exit ;
        end ;
        if( A = $7A ) then // REQ
        begin
            if( Q_FF ) then
            begin
                Q_FF := False ;
                Send_Signal( 'Q', False ) ;
            end ;
            Exit ;
        end ;
        if( A = $7B ) then // SEQ
        begin
            if( not Q_FF ) then
            begin
                Q_FF := True ;
                Send_Signal( 'Q', True ) ;
            end ;
            Exit ;
        end ;
        if( A = $7C ) then // ADCI
        begin
            B := _Register_D + Fetch ;
            if( _Register_DF ) then
            begin
                inc( B ) ;
            end ;
            Register_DF := ( B > $FF ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $7D ) then // SDBI
        begin
            B := Fetch - Register_D ;
            if( not _Register_DF ) then
            begin
                dec( B ) ;
            end ;
            Register_DF := ( B >= 0 ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $7E ) then // SHLC
        begin
            if( _Register_DF ) then
            begin
                B := 1 ;
            end else
            begin
                B := 0 ;
            end ;
            Register_DF := ( ( _Register_D and 128 ) <> 0 ) ;
            _Register_D := _Register_D shl 1 ;
            Register_D := _Register_D or B ;
            Exit ;
        end ;
        if( A = $7F ) then // SMBI
        begin
            B := _Register_D - Fetch ;
            if( not _Register_DF ) then
            begin
                dec( B ) ;
            end ;
            Register_DF := ( B >= 0 ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $80 ) then // GLO
        begin
            Register_D := Register[ A and 15 ] and 255 ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $90 ) then // GHI
        begin
            Register_D := Register[ A and 15 ] shr 8 ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $A0 ) then // PLO
        begin
            B := _Register[ A and 15 ] and ( not $FF ) ;
            B := B or Register_D ;
            Register[ A and 15 ] := B ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $B0 ) then // PHI
        begin
            B := Register_D ;
            B := ( B shl 8 ) or _Register[ A and 15 ] and $FF ;
            Register[ A and 15 ] := B ;
            Exit ;
        end ;
        if( A = $C0 ) then // LBR
        begin
            PC := Fetch_Word ;
            Exit ;
        end ;
        if( A = $C1 ) then // LBQ
        begin
            if( Q_FF ) then
            begin
                PC := Fetch_Word ;
            end else
            begin
                Fetch_Word ;
            end ;
            Exit ;
        end ;
        if( A = $C2 ) then // LBZ
        begin
            if( _Register_D = 0 ) then
            begin
                PC := Fetch_Word ;
            end else
            begin
                Fetch_Word ;
            end ;
            Exit ;
        end ;
        if( A = $C3 ) then // LBDF
        begin
            if( _Register_DF ) then
            begin
                PC := Fetch_Word ;
            end else
            begin
                Fetch_Word ;
            end ;
            Exit ;
        end ;
        if( A = $C4 ) then // NOP
        begin
            Exit ;
        end ;
        if( A = $C5 ) then // LSNQ
        begin
            if( not Q_FF ) then
            begin
                PC := _PC + 2 ;
            end ;
            Exit ;
        end ;
        if( A = $C6 ) then // LSNZ
        begin
            if( _Register_D <> 0 ) then
            begin
                PC := _PC + 2 ;
            end ;
            Exit ;
        end ;
        if( A = $C7 ) then // LSNF
        begin
            if( not _Register_DF ) then
            begin
                PC := _PC + 2 ;
            end ;
            Exit ;
        end ;
        if( A = $C8 ) then // NLBR
        begin
            PC := _PC + 2 ;
            Exit ;
        end ;
        if( A = $C9 ) then // LBNQ
        begin
            if( not Q_FF ) then
            begin
                PC := Fetch_Word ;
            end else
            begin
                Fetch_Word ;
            end ;
            Exit ;
        end ;
        if( A = $CA ) then // LBNZ
        begin
            if( _Register_D <> 0 ) then
            begin
                PC := Fetch_Word ;
            end else
            begin
                Fetch_Word ;
            end ;
            Exit ;
        end ;
        if( A = $CB ) then // LBNF
        begin
            if( not _Register_DF ) then
            begin
                PC := Fetch_Word ;
            end else
            begin
                Fetch_Word ;
            end ;
            Exit ;
        end ;
        if( A = $CC ) then // LSIE
        begin
            if( IE ) then
            begin
                PC := _PC + 2 ;
            end ;
            Exit ;
        end ;
        if( A = $CD ) then // LSQ
        begin
            if( Q_FF ) then
            begin
                PC := _PC + 2 ;
            end ;
            Exit ;
        end ;
        if( A = $CE ) then // LSZ
        begin
            if( _Register_D = 0 ) then
            begin
                PC := _PC + 2 ;
            end ;
            Exit ;
        end ;
        if( A = $CF ) then // LSDF
        begin
            if( _Register_DF ) then
            begin
                PC := _PC + 2 ;
            end ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $D0 ) then // SEP
        begin
            Register_PX := ( _Register_PX and ( not $F ) ) or ( A and 15 ) ;
            Exit ;
        end ;
        if( ( A and $FF0 ) = $E0 ) then // SEX
        begin
            Register_PX := ( _Register_PX and $F ) or ( ( A shl 4 ) and $F0 ) ;
            Exit ;
        end ;
        if( A = $F0 ) then // LDX
        begin
            Register_D := ord( ByteRead( _Register[ _Register_X ] ) ) ;
            Exit ;
        end ;
        if( A = $F1 ) then // OR
        begin
            Register_D := _Register_D or Byte_Read( _Register[ _Register_X ] ) ;
            Exit ;
        end ;
        if( A = $F2 ) then // AND
        begin
            Register_D := _Register_D and Byte_Read( _Register[ _Register_X ] ) ;
            Exit ;
        end ;
        if( A = $F3 ) then // XOR
        begin
            Register_D := _Register_D xor Byte_Read( _Register[ _Register_X ] ) ;
            Exit ;
        end ;
        if( A = $F4 ) then // ADD
        begin
            B := _Register_D + Byte_Read( _Register[ _Register_X ] ) ;
            Register_DF := ( B > $FF ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $F5 ) then // SD
        begin
            B := Byte_Read( _Register[ _Register_X ] ) - Register_D ;
            Register_DF := ( B >= 0 ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $F6 ) then // SHR
        begin
            Register_DF := ( ( _Register_D and 1 ) <> 0 ) ;
            Register_D := _Register_D shr 1 ;
            Exit ;
        end ;
        if( A = $F7 ) then // SM
        begin
            B := Register_D - Byte_Read( _Register[ _Register_X ] ) ;
            Register_DF := ( B >= 0 ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $F8 ) then // LDI
        begin
            Register_D := Fetch ;
            Exit ;
        end ;
        if( A = $F9 ) then // ORI
        begin
            Register_D := _Register_D or Fetch ;
            Exit ;
        end ;
        if( A = $FA ) then // ANI
        begin
            Register_D := _Register_D and Fetch ;
            Exit ;
        end ;
        if( A = $FB ) then // XRI
        begin
            Register_D :=_Register_D xor Fetch ;
            Exit ;
        end ;
        if( A = $FC ) then // ADI
        begin
            B := _Register_D + Fetch ;
            Register_DF := ( B > $FF ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $FD ) then // SDI
        begin
            B := Fetch - _Register_D ;
            Register_DF := ( B >= 0 ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        if( A = $FE ) then // SHL
        begin
            Register_DF := ( ( _Register_D and 128 ) <> 0 ) ;
            Register_D := _Register_D shl 1 ;
            Exit ;
        end ;
        if( A = $FF ) then // SMI
        begin
            B := Register_D - Fetch ;
            Register_DF := ( B >= 0 ) ;
            Register_D := B and $FF ;
            Exit ;
        end ;
        Execute_RCA1802 := False ; { Failure }
    end ;

var A : integer ;
    Count : integer ;
    Nest_Level : integer ;
    Original_PC : longint ;

begin // TRCA1802_CPU.Execute
    // Setup...
    Stopping := False ;
    Count := 0 ;
    Nest_Level := 0 ;

    // Execution loop
    while( True ) do
    begin
        if( Stopping ) then
        begin
            Stopping := False ;
            exit ;
        end ;
        if( Blocked ) then // Waiting for system to catch up to our clock time
        begin
            Do_Wait ;
            continue ;
        end ;
        if( _Run_Stream = nil ) then // Not in immediate mode
        begin
            if( Pending_DMA_In ) then
            begin
                _Idle := False ;
                _Halted := False ;
                Pending_DMA_In := False ;
                ByteWrite( Register[ 0 ], DMA_Input ) ;
            end ;
            if( Pending_DMA_Out ) then
            begin
                _Idle := False ;
                _Halted := False ;
                Pending_DMA_Out := False ;
                DMA_Output( Byte_Read( Register[ 0 ] ) ) ;
            end ;
            if( IE ) then // Interrupts are enabled
            begin
                if( Pending_Interrupt ) then
                begin
                    Pending_Interrupt := False ;
                    _Idle := False ;
                    IE := False ;
                    _Register_T := _Register_PX ;
                    _Register_PX := $21 ;
                    _Halted := False ;
                    State_Change_Notice( State_Interrupt, True ) ;
                end else
                if( CI and ( Mode = M_1806 ) ) then
                begin
                    _Idle := False ;
                    IE := False ;
                    _Register_T := _Register_PX ;
                    _Register_PX := $21 ;
                    _Halted := False ;
                    State_Change_Notice( State_Interrupt, True ) ;
                end ;
            end ;
            if( _Idle ) then
            begin
                Do_Wait ;
                continue ;
            end ;
        end ;
        if( Nest_Level = 0 ) then
        begin
            inc( Count ) ;
        end ;
        if( Single_Step and ( Count > 1 ) ) then
        begin
            exit ;
        end ;

        // Exit if we are done
        if( _Run_Stream = nil ) then
        begin
            if( _Halted ) then
            begin
                exit ;
            end ;
        end else
        begin
            if( _Run_Stream.At_End ) then
            begin
                exit ;
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
            TRCA1802_Profiler( Parent.Profiler ).Increment( Domain_Execution_Addresses, PC ) ;
            TRCA1802_Profiler( Parent.Profiler ).Increment( Domain_Other, Domain_Other_Instruction_Count ) ;
        end ;
        Log_Trace( 'Executing instruction at address ' + cvtb( 10, Base, inttostr( PC ) ) + ': ' + Instruction_At( PC ) ) ;
        Original_PC := PC ;
        A := Fetch ;

        if( _Profiling ) then
        begin
            TRCA1802_Profiler( Parent.Profiler ).Increment( Domain_Instructions, A ) ;
        end ;
        if( _Logger <> nil ) then
        begin
            _Logger.Update( Parent, Original_PC, A ) ;
        end ;

        if( Execute_RCA1802( A ) ) then
        begin
            continue ;
        end ;

        Signal_Exception( '', 0 ) ; // Invalid instruction
    end ; // while( True )
end ; // TRCA1802_CPU.Execute


procedure TRCA1802_CPU.Output( Port, Value : integer ) ;

var Component : TComponent ;
    Loop : integer ;
    UEC : TUnified_Exception ;

begin
    try
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
            UEC := Component.Write( Port, Value, 8, IO_Type_IO ) ;
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
                TRCA1802_Profiler( Parent.Profiler ).Increment( Domain_Port_Outputs, Port ) ;
            end ;
            Log_Trace( 'Output ' + inttostr( Value ) + '. to port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.DMA_Output( Value : integer ) ;

var Component : TComponent ;
    Loop : integer ;
    UEC : TUnified_Exception ;

begin
    try
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
            UEC := Component.Write( 0, Value, 8, IO_Type_DMA ) ;
            if( UEC <> nil ) then
            begin
                exit ;
            end ;
        end ;
    finally
    end ;
end ;


function TRCA1802_CPU.Input( Port : integer ) : integer ;

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
                TRCA1802_Profiler( Parent.Profiler ).Increment( Domain_Port_Inputs, Port ) ;
            end ;
            Log_Trace( 'Input ' + inttostr( Result ) + '. from port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ;


function TRCA1802_CPU.DMA_Input : integer ;

var Component : TComponent ;
    Loop : integer ;

begin
    for Loop := 0 to Parent.Inputs.Count - 1 do
    begin
        Component := TComponent( Parent.Inputs[ Loop ] ) ;
        if( Component.Read( 0, 8, IO_Type_DMA ) ) then
        begin
            Result := Memory_Data_Latch ;
            exit ;
        end ;
    end ;
    Result := Memory_Data_Latch ;
end ;


function TRCA1802_CPU.Get_Register( Index : integer ) : integer ;

begin
    Result := _Register[ Index ] ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ Index ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Set_Register( Index : integer ; Value : integer ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( Index, Value ) ;
    end ;
    _Register[ Index ] := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ Index ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TRCA1802_CPU.Get_PC : word ;

begin
    Result := Register[ Register_PX and 15 ] ;
end ;


procedure TRCA1802_CPU.Set_PC( Value : word ) ;

begin
    Register[ Register_PX and 15 ] := Value ;
end ;


function TRCA1802_CPU.Translate( Space : integer ; Address : int64 ) : int64 ;

begin
    Translate := Address ;
end ;


function TRCA1802_CPU.Default_Base : integer ;

begin
    Result := 16 ;
end ;


function TRCA1802_CPU.Get_D : byte ;

begin
    Result := _Register_D ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 16 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 16, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Set_D( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 16, Value ) ;
    end ;
    _Register_D := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 16 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 16, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TRCA1802_CPU.Get_IN : byte ;

begin
    Result := _Register_IN ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 19 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 19, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Set_IN( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 19, Value ) ;
    end ;
    _Register_IN := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 19 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 19, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TRCA1802_CPU.Get_PX : byte ;

begin
    Result := _Register_PX ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 17 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 17, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Set_PX( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 17, Value ) ;
    end ;
    _Register_PX := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 17 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 17, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TRCA1802_CPU.Get_T : byte ;

begin
    Result := _Register_T ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 18 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 18, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Set_T( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 18, Value ) ;
    end ;
    _Register_T := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 18 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 18, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TRCA1802_CPU.Get_X : byte ;

begin
    Result := Register_PX shr 4 ;
end ;


procedure TRCA1802_CPU.Set_X( Value : byte ) ;

begin
    Register_PX := ( _Register_PX and $F ) or ( Value shl 4 ) ;
end ;


function TRCA1802_CPU.Get_DF : boolean ;

begin
    Result := _Register_DF ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 16 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 16, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Set_DF( Value : boolean ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := boolean( _RTS.Register_Change( 16, ord( Value ) ) ) ;
    end ;
    _Register_DF := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 16 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 16, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TRCA1802_CPU.Get_Low_Memory : int64 ;

begin
    Result := 0 ;
end ;


function TRCA1802_CPU.Get_High_Memory : int64 ;

begin
    Result := 65535 ;
end ;


function TRCA1802_CPU.Get_Low_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TRCA1802_CPU.Get_High_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TRCA1802_CPU.Get_Low_Port : int64 ;

begin
    Result := 1 ;
end ;


function TRCA1802_CPU.Get_High_Port : int64 ;

begin
    Result := 7 ;
end ;


function TRCA1802_CPU.Support_Virtual_Address : boolean ;

begin
    Result := False ;
end ;


function TRCA1802_CPU.Register_Name( Index : integer ) : PChar ;

begin
    Temp_Register_Name := '' ; // Invalid index
    case Index of
        0..15 : Temp_Register_Name := 'R' + inttostr( Index ) ;
        16 : Temp_Register_Name := 'D' ;
        17 : Temp_Register_Name := 'PX' ;
        18 : Temp_Register_Name := 'T' ;
        19 : Temp_Register_Name := 'IN' ;
        20 : Temp_Register_Name := 'DF' ;
        21 : if( Mode = M_1802 ) then
             begin
                 Temp_Register_Name := 'IE' ;
             end else
             begin
                 Temp_Register_Name := 'MIE' ;
             end ;
    end ;
    if( Mode = M_1806 ) then
    begin
        case Index of
            22 : Temp_Register_Name := 'CH' ;
            23 : Temp_Register_Name := 'Counter' ;
            24 : Temp_Register_Name := 'ETQ' ;
            25 : Temp_Register_Name := 'CIE' ;
            26 : Temp_Register_Name := 'XIE' ;
            27 : Temp_Register_Name := 'CI' ;
        end ;
    end ;
    Result := PChar( Temp_Register_Name ) ;
end ;


function TRCA1802_CPU.Register_Size( Index : integer ) : integer ;

begin
    Result := 0 ;
    case Index of
        0..15 : Result := 16 ; // Registers
        16..19 : Result := 8 ; // D, PX, T, IN
        20..21 : Result := 1 ; // DF, IE
    end ;
    if( Mode = M_1806 ) then
    begin
        case Index of
            22..23 : Result := 8 ; // CH and Counter
            24..27 : Result := 1 ; // EYQ, CIE, XIE, CI
        end ;
    end ;
end ;


procedure TRCA1802_CPU.Restart ;

begin
    Mode := M_1802 ;
    Q_FF := False ;
    Register_IN := 0 ;
    Register_PX := 0 ;
    IE := True ;
    XIE := True ;
    CIE := True ;
    CI := False ;
    Pending_Interrupt := False ;
    _Register[ 0 ] := 0 ;
    Counter_Mode := CM_Stopped ;
    Prescalar := 0 ;

    _Run_Stream := nil ;
end ;


function TRCA1802_CPU.Top_Of_Stack( Index : integer ) : int64 ;

begin
    Top_Of_Stack := 0 ;
end ;


function TRCA1802_CPU.Big_Endian : boolean ;

begin
    Result := True ;
end ;


function TRCA1802_CPU.Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;

begin
    _RTS := RTS ;
    _RTS_Flags := Flags ;
    Result := nil ;
end ;


function TRCA1802_CPU.Get_Current_Address( Space : integer ;
    Physical : boolean ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := PC ;
    end else
    begin
        Result := 0 ;
    end ;
end ;


procedure TRCA1802_CPU.Set_Current_Address( Space : integer ; Physical : boolean ;
    Value : int64 ) ;

begin
    if( Space = 0 ) then
    begin
        _PC := Value ;
    end ;
end ;



// TRCA1802 methods...

{ Internal utility routines... }

// API...

// Generic CPU support routines...

function TRCA1802.Facility_Code : longint ;

begin
    Result := RCA1802Err_Facility ;
end ;


function TRCA1802.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU := TRCA1802_CPU.Create ;
    _CPU._UI := UI ;
    _CPU.Parent := self ;
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    CPU.Restart ; // Do power-on reset
end ;


function TRCA1802.Terminate : TUnified_Exception ;

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
    CPU.Free ;
end ;


function TRCA1802.Serial_Number : integer ;

begin
    Result := _Serial_Number ;
end ;


function TRCA1802.Child_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TRCA1802.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Clear_Watchpoint( Address, Memory, Access ) ;
end ;


function TRCA1802.Component_Type : longint ;

begin
    Result := Component_Type_CPU ; // CPU
end ;


function TRCA1802.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := _CPU.Set_Error( RCA1802Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Inputs.Indexof( Component ) <> -1 ) then
    begin
        Result := _CPU.Set_Error( RCA1802Err_Already_Connected ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Result := _CPU.Set_Error( RCA1802Err_Success ) ;
end ;


function TRCA1802.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := _CPU.Set_Error( RCA1802Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Outputs.Indexof( Component ) <> -1 ) then
    begin
        Result := _CPU.Set_Error( RCA1802Err_Already_Connected ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Result := _CPU.Set_Error( RCA1802Err_Success ) ;
end ;


function TRCA1802.CPU : TCPU ;

begin
    Result := _CPU ;
end ;


function TRCA1802.Debugger : TDebug_Interface ;

begin
    Result := nil ; // TODO
end ;


function TRCA1802.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Result := _CPU.Set_Error( RCA1802Err_Component_Not_Found ) ;
    end else
    begin
	    Result := _CPU.Set_Error( RCA1802Err_Success ) ;
	    Inputs.Remove( Component ) ;
    end ;
end ;


function TRCA1802.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Result := _CPU.Set_Error( RCA1802Err_Component_Not_Found ) ;
    end else
    begin
	    Result := _CPU.Set_Error( RCA1802Err_Success ) ;
	    Outputs.Remove( Component ) ;
    end ;
end ;


function TRCA1802.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var V : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        _CPU.Set_Error( RCA1802Err_No_Cache ) ;
    end else
    begin
        if( ( Address > 21 ) and ( _CPU.Mode = M_1802 ) ) then
        begin
            _CPU.Set_Error( RCA1802Err_Invalid_Register ) ;
            exit ;
        end ;
        if( Address > 27 ) then
        begin
            _CPU.Set_Error( RCA1802Err_Invalid_Register ) ;
            exit ;
        end ;
        _CPU.Set_Error( 0 ) ;

        if( Size = 0 ) then
        begin
            exit ;
        end ;
        if( Size > _CPU.Register_Size( Address ) ) then
        begin
            Size := _CPU.Register_Size( Address ) ;
        end ;
        Size := ( Size + 7 ) div 8 ; // Number of bytes
        V := 0 ;
        move( Buffer^, V, Size ) ;
        case Address of
            0..15 : _CPU._Register[ Address ] := V ;
            16 : _CPU._Register_D := V ;
            17 : _CPU._Register_PX := V ;
            18 : _CPU._Register_T := V ;
            19 : _CPU._Register_IN := V ;
            20 : _CPU._Register_DF := ( ( V and 1 ) <> 0 ) ;
            21 : _CPU.IE := ( ( V and 1 ) <> 0 ) ;
            22 : _CPU.CH := V ;
            23 : _CPU.Counter := V ;
            24 : _CPU.ETQ := ( ( V and 1 ) <> 0 ) ;
            25 : _CPU.CIE := ( ( V and 1 ) <> 0 ) ;
            26 : _CPU.XIE := ( ( V and 1 ) <> 0 ) ;
            27 : _CPU.CI := ( ( V and 1 ) <> 0 ) ;
        end ; // case Address
    end ; // if( Memory )
end ; // TRCA1802.Deposit


function TRCA1802.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Size, V : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        _CPU.Set_Error( RCA1802Err_No_Cache ) ;
    end else
    begin
        if( ( Address > 21 ) and ( _CPU.Mode = M_1802 ) ) then
        begin
            _CPU.Set_Error( RCA1802Err_Invalid_Register ) ;
            exit ;
        end ;
        if( Address > 27 ) then
        begin
            _CPU.Set_Error( RCA1802Err_Invalid_Register ) ;
            exit ;
        end ;
        _CPU.Set_Error( 0 ) ;

        if( Size = 0 ) then
        begin
            exit ;
        end ;
        if( Size > _CPU.Register_Size( Address ) ) then
        begin
            Size := _CPU.Register_Size( Address ) ;
        end ;
        case Address of
            0..15 : V := _CPU._Register[ Address ] ;
            16 : V := _CPU._Register_D ;
            17 : V := _CPU._Register_PX ;
            18 : V := _CPU._Register_T ;
            19 : V := _CPU._Register_IN ;
            20 : if( _CPU._Register_DF ) then
                 begin
                     V := 1 ;
                 end else
                 begin
                     V := 0 ;
                 end ;
            21 : V := ord( _CPU.IE ) ;
            22 : V := _CPU.CH ;
            23 : V := _CPU.Counter ;
            24 : V := ord( _CPU.ETQ ) ;
            25 : V := ord( _CPU.CIE ) ;
            26 : V := ord( _CPU.XIE ) ;
            27 : V := ord( _CPU.CI ) ;
        end ; // case Address
        _Size := ( Size + 7 ) div 8 ; // Number of bytes
        move( V, Buffer^, _Size ) ;
    end ; // if( Memory )
end ; // TRCA1802.Examine


function TRCA1802.Get_Access_Mode( Address : int64 ; Memory : boolean ) : longint ;

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


function TRCA1802.Get_Profiling : boolean ;

begin
    Result := _CPU._Profiling ;
end ;


function TRCA1802.Get_Read_Latency : longint ;

begin
    Result := 0 ;
end ;


function TRCA1802.Get_Write_Latency : longint ;

begin
    Result := 0 ;
end ;


function TRCA1802.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
        Result := nil ;
        _CPU.Set_Error( RCA1802Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Inputs[ Index ] ;
end ;


const RCA1802_Name : string = 'RCA CDP1802' ;

function TRCA1802.Name : PChar ;

begin
    Result := PChar( RCA1802_Name ) ;
end ;


function TRCA1802.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Outputs.Count ) ) then
    begin
        Result := nil ;
        _CPU.Set_Error( RCA1802Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Outputs[ Index ] ;
end ;


function TRCA1802.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ; // Doens't apply to CPUs
end ;


function TRCA1802.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
end ;


function TRCA1802.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
end ;


function TRCA1802.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( RCA1802_Invalid_Operation ) ;
end ;


procedure TRCA1802.Set_Profiling( _On, Children : boolean ) ;

begin
    _CPU._Profiling := _On ;
end ;


procedure TRCA1802.Set_Read_Latency( Value : longint ) ;

begin
    // Do nothing - we have no read latency
end ;


function TRCA1802.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    if( Access = 0 ) then // No effect
    begin
        exit ;
    end ;
    if( Memory ) then
    begin
        _CPU._Memory_Watchpoints.Create_Watchpoint( Address, Access ) ;
    end else
    begin
        if( ( Address < _CPU.Get_Low_Port ) or ( Address > _CPU.Get_High_Port ) ) then
        begin
            Result := _CPU.Set_Error( RCA1802_Invalid_Address ) ;
            exit ;
        end ;
        _CPU._Port_Watchpoints[ Address ] := _CPU._Port_Watchpoints[ Address ] or Access ;
    end ;
end ; // TRCA1802.Set_Watchpoint


procedure TRCA1802.Set_Write_Latency( Value : longint ) ;

begin
    // Intentionally left blank - no latency
end ;


function TRCA1802.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


procedure TRCA1802.Wake ;

begin
    _CPU.Blocked := False ;
end ;


function TRCA1802.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : integer ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := Value ;
end ;


function TRCA1802.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := ord( Value[ ( ( Size + 7 ) div 8 ) - 1 ] ) ;
end ;


procedure TRCA1802.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = '1802' ) then
        begin
            _CPU.Mode := M_1802 ;
        end else
        if( S = '1806' ) then
        begin
            _CPU.Mode := M_1806 ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ;


procedure TRCA1802.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TRCA1802.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TRCA1802.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TRCA1802.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function TRCA1802.Profiler : TProfiler ;

begin
    if( _CPU._Profiler = nil ) then
    begin
        _CPU._Profiler := TRCA1802_Profiler.Create ;
    end ;
    Result := _CPU._Profiler ;
end ;


function TRCA1802.Get_Trace : boolean ;

begin
    Result := _CPU._Trace ;
end ;


procedure TRCA1802.Set_Trace( Value : boolean ) ;

begin
    _CPU._Trace := Value ;
end ;


function TRCA1802.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Restore_State( Stream ) ;
end ;


function TRCA1802.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Save_State( Stream ) ;
end ;


procedure TRCA1802.Show_Status ;

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


var Loop : integer ;

begin
    Index := 0 ;
    case _CPU.Mode of
        M_1802 : Output( 'RCA CDP1802' ) ;
        M_1806 : Output( 'RCA CDP1806' ) ;
    end ;
    if( _CPU.Mode = M_1802 ) then
    begin
        S := 'IE=' ;
    end else
    begin
        S := 'MIE=' ;
    end ;
    if( _CPU.IE ) then
    begin
        S := S + ' enabled' ;
    end else
    begin
        S := S + ' disabled' ;
    end ;
    Output( S ) ;
    S := 'D=' + Show( _CPU._Register_D, 1 ) ;
    Output( S ) ;
    for Loop := 0 to 15 do
    begin
        S := S + 'R' + inttostr( Loop ) + '=' + Show( _CPU._Register[ Loop ], 2 ) ;
        if( ( _CPU._Register_PX and 15 ) = Loop ) then
        begin
            S := S + ' (PC)' ;
        end ;
        if( ( _CPU._Register_PX shr 4 ) = Loop ) then
        begin
            S := S + ' (X)' ;
        end ;
        Output( S ) ;
    end ;
    S := 'PX=' + Show( _CPU._Register_PX, 1 ) ;
    Output( S ) ;
    S := 'T=' + Show( _CPU._Register_T, 1 ) ;
    Output( S ) ;
    S := 'IN=' + Show( _CPU._Register_IN, 1 ) ;
    Output( S ) ;
    if( _CPU.Mode = M_1806 ) then
    begin
        S := 'CH=' + Show( _CPU.CH, 1 ) ;
        Output( S ) ;
        S := 'Counter = ' ;
        case _CPU.Counter_Mode of
            CM_Stopped : S := S + 'Stopped' ;
            CM_EC1 : S := S + 'EC1' ;
            CM_EC2 : S := S + 'EC2' ;
            CM_Timer : S := S + 'Timer' ;
            CM_PDM1 : S := S + 'PDM1' ;
            CM_PDM2 : S := S + 'PDM2' ;
        end ;
        Output( S ) ;
        S := 'CIE=' ;
        if( _CPU.CIE ) then
        begin
            S := S + ' enabled' ;
        end else
        begin
            S := S + ' disabled' ;
        end ;
        Output( S ) ;
        S := 'XIE=' ;
        if( _CPU.XIE ) then
        begin
            S := S + ' enabled' ;
        end else
        begin
            S := S + ' disabled' ;
        end ;
        Output( S ) ;
    end ;
end ; // TRCA1802.Show_Status


procedure TRCA1802.Reset ;

begin
    _CPU.Restart ;
end ;


procedure TRCA1802.Set_Signal( Name : PChar ; State : boolean ) ;

var Temp : string ;

begin
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    if( Temp = 'INT' ) then
    begin
        if( State and _CPU.IE ) then
        begin
            _CPU.Pending_Interrupt := True ;
        end ;
    end else
    if( Temp = 'DMA_IN' ) then
    begin
        if( State ) then
        begin
            _CPU.Pending_DMA_In := True ;
        end ;
    end else
    if( Temp = 'DMA_OUT' ) then
    begin
        if( State ) then
        begin
            _CPU.Pending_DMA_Out := True ;
        end ;
    end else
    if( Temp = 'EF1' ) then
    begin
        if( _CPU.EF1 <> State ) then
        begin
            _CPU.EF1 := State ;
            if( _CPU.Mode = M_1806 ) then
            begin
                if( ( _CPU.Counter_Mode = CM_EC1 ) and ( not State ) ) then
                begin
                    _CPU.Decrement_Counter ;
                end else
                if( ( _CPU.Counter_Mode = CM_PDM1 ) and State ) then
                begin
                    _CPU.Counter_Mode := CM_Stopped ;
                    _CPU.CI := True ;
                end ;
            end ;
        end ;
    end else
    if( Temp = 'EF2' ) then
    begin
        if( _CPU.EF2 <> State ) then
        begin
            _CPU.EF2 := State ;
            if( _CPU.Mode = M_1806 ) then
            begin
                if( ( _CPU.Counter_Mode = CM_EC2 ) and ( not State ) ) then
                begin
                    _CPU.Decrement_Counter ;
                end else
                if( ( _CPU.Counter_Mode = CM_PDM2 ) and State ) then
                begin
                    _CPU.Counter_Mode := CM_Stopped ;
                    _CPU.CI := True ;
                end ;
            end ;
        end ;
    end else
    if( Temp = 'EF3' ) then
    begin
        _CPU.EF3 := State ;
    end else
    if( Temp = 'EF4' ) then
    begin
        _CPU.EF4 := State ;
    end else
    begin
        exit ;
    end ;
    if( _Logger <> nil ) then
    begin
        _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
    end ;
end ; // TRCA1802.Set_Signal


function TRCA1802.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

var Temp : string ;

begin
    Result := False ;
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    if( Temp = 'Q' ) then
    begin
        State := _CPU.Q_FF ;
        Result := True ;
    end ;
end ;


function TRCA1802.Signal_Count : longint ;

begin
    Result := 8 ;
end ;


function TRCA1802.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := '' ;
    case Index of
        0 : Temp_Signal_Name := 'INT' ;
        1 : Temp_Signal_Name := 'DMA_IN' ;
        2 : Temp_Signal_Name := 'DMA_OUT' ;
        3 : Temp_Signal_Name := 'Q' ;
        4 : Temp_Signal_Name := 'EF1' ;
        5 : Temp_Signal_Name := 'EF2' ;
        6 : Temp_Signal_Name := 'EF3' ;
        7 : Temp_Signal_Name := 'EF4' ;
    end ;
    if( Temp_Signal_Name = '' ) then
    begin
        Result := nil ;
    end else
    begin
        Result := PChar( Temp_Signal_Name ) ;
    end ;
end ;


function TRCA1802.Signal_Out( Index : longint ) : boolean ;

begin
    if( Index = 3 ) then
    begin
        Result := True ; // Q
    end else
    begin
        Result := False ;
    end ;
end ;


function TRCA1802.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := True ;
end ;


function TRCA1802.Get_State_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( ( Index >= 0 ) and ( Index <= 5 ) ) then
    begin
        case Index of
            State_Port_Input : Temp_Get_State_Name := 'Port input' ;
            State_Port_Output : Temp_Get_State_Name := 'Port output' ;
            State_Interrupt : Temp_Get_State_Name := 'Interrupt' ;
            State_DMA_IN : Temp_Get_State_Name := 'DMA In' ;
            State_DMA_OUT : Temp_Get_State_Name := 'DMA Out' ;
            State_IDLE : Temp_Get_State_Name := 'Idle' ;
        end ;
        Result := PChar( Temp_Get_State_Name ) ;
    end ;
end ;


function TRCA1802.Get_Exception_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Temp_Get_Exception_Description := 'Invalid instruction' ;
                Result := PChar( Temp_Get_Exception_Description ) ;
            end ;
    end ;
end ;


function TRCA1802.Signal_Index( Name : PChar ) : integer ;

var S : string ;

begin
    S := string( Name ) ;
    if( S = 'INT' ) then
    begin
        Result := 0 ;
    end else
    if( S = 'DMA_IN' ) then
    begin
        Result := 1 ;
    end else
    if( S = 'DMA_OUT' ) then
    begin
        Result := 2 ;
    end else
    if( S = 'Q' ) then
    begin
        Result := 3 ;
    end else
    if( S = 'EF1' ) then
    begin
        Result := 4 ;
    end else
    if( S = 'EF2' ) then
    begin
        Result := 5 ;
    end else
    if( S = 'EF3' ) then
    begin
        Result := 6 ;
    end else
    if( S = 'EF4' ) then
    begin
        Result := 7 ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TRCA1802.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TRCA1802.Set_Logger( Value : TCEF_Logger ) ;

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

