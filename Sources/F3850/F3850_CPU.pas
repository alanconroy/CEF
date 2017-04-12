{$N+}
{
        Program Name : F3850
        Package Name : CEF
        Purpose      : Fairchild 3850 CPU and 3853 SMI (CEF components)
        Institution  :
        Date Written : 9-Jul-2007
        Written By   : Alan Conroy
        Version      : 1.0

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *    DATE        BY          REASON                         *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	  This unit implements a Fairchild 3850 CPU emulator and 3853 Static
        Memory Interface emulator as a single CEF CPU component.  Note that the
        lack of DMA support in the 3853 emulation is a non-issue in CEF32 since
        no two components can actually access the RAM simultaneously.  Thus, a
        2854 DMA controller component could be used with this component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit F3850_CPU ;

interface

uses { Borland... }
     Classes, { TList }

     { CEF... }
     _CEF, // TUI_Logger
     CEF, { TBase_CPU }
     _CEFUtil, // TCEF_Watchpoint

     { Other... }
     _DebugIn, { TDebug_Interface }
     CommonUt, { TInteger_List }
     _Streams, // TCOM_Stream
     _UE ; // tUnified_Exception

const SF_Predefined = 1 ; { Predefined symbol }

const F3850Err_Facility = -1 ;
      F3850Err_Success = 0 ;
      F3850Err_Invalid_Component = 1 ;
      F3850Err_Already_Connected = 2 ;
      F3850Err_Component_Not_Found = 3 ;
      F3850Err_No_Cache = 4 ;
      F3850Err_Invalid_Register = 5 ;
      F3850_No_Breakpoint = 6 ;
      F3850_Breakpoint_Exists = 7 ;
      F3850_Invalid_Address = 8 ;
      F3850_Invalid_Operation = 9 ;
      F3850_Invalid_Instruction = 10 ;
      F3850_IO_Trap = 11 ;

type TF3850_Profiler = class( TBase_Profiler )
                         private // Instance data...
                             // Profile data...
                             _Clock : integer ;
                             _Instruction_Count : integer ;
                             Port_Outputs : array[ 0..255 ] of integer ;
                             Port_Inputs : array[ 0..255 ] of integer ;
                             Execution_Addresses : array[ 0..$FFFF ] of integer ;
                             Instructions : array[ 0..255 ] of integer ;

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
                     end ; // TF3850_Profiler

const Max_Register = 77 ;

type TF3850_CPU = class;
     TF3850 = class( TBase_Component )
                    private // Instance data...
                        _CPU : TF3850_CPU ;
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
                end ; // TF3850

     TF3850_CPU = class( TBase_CPU )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private // Generic CPU support...
            Parent : TF3850 ;
            _UI : TUI_Interface ;
            _Speed : integer ; // KHz
            _Register_Watchpoints : array[ 0..Max_Register ] of integer ; // Access mode for registers
            _Profiling : boolean ; // True if profiling
            _Memory_Watchpoints : TCEF_Watchpoint_Manager ;
            _Port_Watchpoints : array[ 1..7 ] of integer ;
            _Breakpoints : TInteger_List ;
            Memory_Data_Latch : byte ; // Last byte sent to us
            _Run_Stream : TCOM_Stream ;
            _Stream_PC_Offset : integer ;
            _Profiler : TF3850_Profiler ;
            _Trace : boolean ; // True to trace execution
            Blocked : boolean ;
            Segments : TInteger_List ;
            _Logger : TCEF_Logger ;
            _RTS : TRun_Time_System ;
            _RTS_Flags : longint ;

            Temp_Register_Name : string ;
            Temp_Register_Description : string ;
            Temp_Log_Trace : string ;
            Temp_Signal_Exception : string ;

        private // Processor internals...
            _Register : array[ 0..63 ] of byte ;
            _Accumulator : byte ;
            _ISAR : byte ;
            _W : byte ; // Status (flags)
            _PC : word ; // PC0
            _Stack : word ; // PC1
            _DC0, _DC1 : word ; // Data Counter registers
            Interrupt_Privilege : boolean ;

            _Halted : boolean ; // True if last instruction was a halt
            Pending_Interrupt : boolean ; // Pending external interrupt

        private // 3853 data...
            IO_Port_Select : byte ;
            Timer : integer ;
            Interrupt_Mode : byte ;
            Timer_Interrupt : boolean ; // True if interrupt was generated by timer
            ADLO, ADHI : byte ; // Interrupt address registers

        private // Signal states...
            EXTRES, INTREQ : boolean ;

        private // Internal utility routines...
            procedure Do_Wait ;
            function ByteRead( Address : Integer ) : Char ; { Return data at Address }
            function Byte_Read( Address : integer ) : integer ;
            function Word_Read( Address : integer ) : integer ;
            procedure ByteWrite( Address, Value : Integer ) ; { Write to memory }
            procedure Set_Flags( Value : byte ; O, C : boolean ) ;
            procedure Increment_Clock( Count : integer ) ;
            function Bus_Read( Address : Integer ; IO_Type : longint ) : Char ; { Return data at Address }
            function Bus_Examine( Address : Integer ) : Char ; { Return data at memory Address }
            function Resolve_Register( A : byte ) : byte ;
            procedure Execute( Single_Step, Into : boolean ) ;
            procedure Output( Port, Value : integer ) ;
            function Input( Port : integer ) : integer ;
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

            function Get_Accumulator : byte ;
            procedure Set_Accumulator( Value : byte ) ;
            function Get_PC : word ;
            procedure Set_PC( Value : word ) ;
            function Get_W : byte ;
            procedure Set_W( Value : byte ) ;
            function Get_ISAR : byte ;
            procedure Set_ISAR( Value : byte ) ;
            function Get_Stack : word ;
            procedure Set_Stack( Value : word ) ;
            function Get_DC0 : word ;
            procedure Set_DC0( Value : word ) ;
            function Get_DC1 : word ;
            procedure Set_DC1( Value : word ) ;
            function Get_H : word ;
            procedure Set_H( Value : word ) ;
            function Get_J : byte ;
            procedure Set_J( Value : byte ) ;
            function Get_K : word ;
            procedure Set_K( Value : word ) ;
            function Get_L : word ;
            procedure Set_L( Value : word ) ;
            function Get_Register( Index : byte ) : byte ;
            procedure Set_Register( Index : byte ; Value : byte ) ;

            property Accumulator : byte
                read Get_Accumulator
                write Set_Accumulator ;

            property DC0 : word
                read Get_DC0
                write Set_DC0 ;
            property DC1 : word
                read Get_DC1
                write Set_DC1 ;
            property PC : word
                read Get_PC
                write Set_PC ;
            property ISAR : byte
                read Get_ISAR
                write Set_ISAR ;
            property Register[ Index : byte ] : byte
                read Get_Register
                write Set_Register ;
            property Stack : word
                read Get_Stack
                write Set_Stack ;
            property W : byte
                read Get_W
                write Set_W ;
            property H : word
                read Get_H
                write Set_H ;
            property J : byte
                read Get_J
                write Set_J ;
            property K : word
                read Get_K
                write Set_K ;
            property L : word
                read Get_L
                write Set_L ;
            property Q : word // Synonym for L
                read Get_L
                write Set_L ;

        public // API...
            Base : integer ;

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

            function Get_Low_Virtual_Memory( Space : integer ) : int64 ;
                override ;

            function Get_High_Virtual_Memory( Space : integer ) : int64 ;
                override ;

            function Get_Low_Port : int64 ; override ;

            function Get_High_Port : int64 ; override ;

            function Support_Virtual_Address : boolean ; override ;

            function Segment_Size( Index : integer ) : integer ; override ;

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
                override ; stdcall ;
     end ; // TF3850

implementation

uses { Borland... }
     SysUtils, { Allocmem }

     { Other... }
     _ASCII, { CR }
     CVT, { Cvtb }
     HTML, { TXML_Parser }
     Num1s, { num1 }
     Parse, // TString_Parser
     SStreams, // TCOM_String_Stream
     Standard, // Bit_Values

     { Fairchild 3850... }
     F3850_ASM, // TF3850_Assembler
     F3850_Util ;
     

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
const State_Port_Input = 0 ;
const State_Port_Output = 1 ;
const State_Interrupt = 2 ;
CONST State_Idle = 3 ;

const IO_Type_Interrupt = 2 ;




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
        0..$B : Result := 'LR' ;
        $C : Result := 'PK' ;
        $D..$11: Result := 'LR' ;
        $12 : Result := 'SR' ;
        $13 : Result := 'SL' ;
        $14 : Result := 'SR' ;
        $15 : Result := 'SL' ;
        $16 : Result := 'LM' ;
        $17 : Result := 'ST' ;
        $18 : Result := 'COM' ;
        $19 : Result := 'LNK' ;
        $1A : Result := 'DI' ;
        $1B : Result := 'EI' ;
        $1C : Result := 'POP' ;
        $1D..$1E : Result := 'LR' ;
        $1F : Result := 'INC' ;
        $20 : Result := 'LI' ;
        $21 : Result := 'NI' ;
        $22 : Result := 'OI' ;
        $23 : Result := 'XI' ;
        $24 : Result := 'AI' ;
        $25 : Result := 'CI' ;
        $26 : Result := 'IN' ;
        $27 : Result := 'OUT' ;
        $28 : Result := 'PI' ;
        $29 : Result := 'JMP' ;
        $2A : Result := 'DCI' ;
        $2B : Result := 'NOP' ;
        $2C : Result := 'XDC' ;
        $30..$3E : Result := 'DS' ;
        $40..$4E : Result := 'LR' ;
        $50..$5E : Result := 'LR' ;
        $60..$67 : Result := 'LISU' ;
        $68..$6F : Result := 'LISL' ;
        $70 : Result := 'CLR' ;
        $71..$7F : Result := 'LIS' ;
        $80 : Result := 'BT' ;
        $81 : Result := 'BP' ;
        $82 : Result := 'BC' ;
        $83 : Result := 'BT' ;
        $84 : Result := 'BZ' ;
        $85..$87 : Result := 'BT' ;
        $88 : Result := 'AM' ;
        $89 : Result := 'AMD' ;
        $8A : Result := 'NM' ;
        $8B : Result := 'OM' ;
        $8C : Result := 'XM' ;
        $8D : Result := 'CM' ;
        $8E : Result := 'ADC' ;
        $8F : Result := 'BRZ' ;
        $90 : Result := 'BR' ;
        $91 : Result := 'BM' ;
        $92 : Result := 'BNC' ;
        $93 : Result := 'BF' ;
        $94 : Result := 'BNZ' ;
        $95..$97 : Result := 'BF' ;
        $98 : Result := 'BNO' ;
        $99..$9F : Result := 'BF' ;
        $A0..$AF : Result := 'INS' ;
        $B0..$BF : Result := 'OUTS' ;
        $C0..$CE : Result := 'AS' ;
        $D0..$DE : Result := 'ASD' ;
        $E0..$EE : Result := 'XS' ;
        $F0..$FE : Result := 'NS' ;
    end ;
end ; // Instruction_Name


// TF3850_Profiler methods...

// API...

procedure TF3850_Profiler.Generate_Report ;

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
    for Loop := 0 to 255 do
    begin
        if( Instructions[ Loop ] <> 0 ) then
        begin
            Instruction_Lines.Add( Instruction_Name( Loop ) + ': ' + inttostr( Instructions[ Loop ] ) ) ;
        end ;
    end ;
    Instruction_Lines.Sort ;
end ; // TF3850_Profiler.Generate_Report


procedure TF3850_Profiler.Increment( Domain, Index : integer ) ;

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


procedure TF3850_Profiler.Increment_Clock( Count : integer ) ;

begin
    Dirty := True ;
    _Clock := _Clock + Count ;
end ;


// Overrides...

procedure TF3850_Profiler.Clear( Domain : integer ) ;

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
end ; // TF3850_Profiler.Clear


function TF3850_Profiler.Domain_Name( Index : integer ) : PChar ;

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


function TF3850_Profiler.Report_Line( Domain, Index : integer ) : PChar ;

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
end ; // TF3850_Profiler.Report_Line



// TF3850_CPU methods...

// Constructors and destructors...

constructor TF3850_CPU.Create ;

begin
    inherited Create ;

    _Memory_Watchpoints := Get_Watchpoint_Manager ;
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
    _Breakpoints := TInteger_List.Create ;
    _Speed := 2000 ; // 2 MHz
    IO_Port_Select := 3 ;
    Base := Default_Base ;
    Segments := TInteger_List.Create ;
end ;


destructor TF3850_CPU.Destroy ;

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


// Internal utility routines...

function TF3850_CPU.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
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
            Result := Set_Error( F3850_Invalid_Address ) ;
            exit ;
        end ;
        _Port_Watchpoints[ Address ] :=
            _Port_Watchpoints[ Address ] and not( Access ) ;
    end ; // if( Memory )
end ; // TF3850_CPU.Clear_Watchpoint


procedure TF3850_CPU.State_Change_Notice( Index : integer ; State : boolean ) ;

begin
    _UI.State_Change_Notice( Parent, Index, State ) ;

    case Index of
        State_Interrupt : Log_Trace( 'Process interrupt' ) ;
    end ;
end ;


procedure TF3850_CPU.Log_Trace( const Description : string ) ;

begin
    if( _Trace ) then
    begin
        Temp_Log_Trace := Description ;
        _UI.Log_Trace( Parent, PChar( Temp_Log_Trace ) ) ;
    end ;
end ;


procedure TF3850_CPU.Signal_Exception( const Description : string ; Index : longint ) ;

begin
    Temp_Signal_Exception := Description ;
    _UI.Signal_Exception( Parent, PChar( Temp_Signal_Exception ), Index ) ;
end ;


procedure TF3850_CPU.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
    Memory, Internal, Port : boolean ) ;

begin
    _UI.Watchpoint_Notice( Address, Access, Tag, Parent, Memory, Internal, Port ) ;
end ;


function TF3850_CPU.Instruction_At( Address : integer ) : string ;

var Stream : TCOM_String_Stream ;

begin
    Stream := TCOM_String_Stream.Create ;
    Disassemble( Address, Base, 1, Stream ) ;
    Result := string( Stream.As_String ) ;
    Stream.Detach ;
end ;


procedure TF3850_CPU.Do_Wait ;

begin
    try
        _UI.Idle( Parent ) ;
    except
    end ;
end ;


function TF3850_CPU.Bus_Examine( Address : Integer ) : Char ; { Return data at memory Address }

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


function TF3850_CPU.Bus_Read( Address : Integer ; IO_Type : longint ) : Char ; { Return data at Address }

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
        
        // If no memory connected to inputs, try outputs...
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
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


function TF3850_CPU.ByteRead( Address : Integer ) : Char ; { Return data at Address }

begin
    Result := Bus_Read( Address and $FFFF, IO_Type_Memory ) ;
end ;


function TF3850_CPU.Byte_Read( Address : integer ) : integer ;
{ Read a byte from the specified address }

begin
    Byte_Read := ord( ByteRead( Address ) ) ;
end ;


function TF3850_CPU.Word_Read( Address : integer ) : integer ;
{ Read a word from the specified address }

var X : integer ;

begin
    X := Byte_Read( Address ) ;
    X := X or ( swap( Byte_Read( Address + 1 ) ) and $FF00 ) ;
    Word_Read := X ;
end ;


procedure TF3850_CPU.ByteWrite( Address, Value : Integer ) ; { Write to memory }

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


procedure TF3850_CPU.Increment_Clock( Count : integer ) ;

var R : extended ;
    W : int64 ; // Amount of time to wait (in picoseconds)

begin
    if( _Profiling ) then
    begin
        TF3850_Profiler( Parent.Profiler ).Increment_Clock( Count ) ;
    end ;
    try
        if( _UI.Clock <> nil ) then
        begin
            R := 1 ;
            R := R / _Speed ; // Cycle time, in seconds
            R := R * Count ;
            R := int( R * 1000000000.0 ) ; // Convert from seconds to nanoseconds
            W := trunc( R / 2 ) ; // Our measurements are in .5 cycles, so divide by 2
            Blocked := True ;
            _UI.Clock.Block( Parent, W ) ;
        end ;
    except
    end ;

    if( ( Interrupt_Mode = 3 ) and ( Timer >= 0 ) ) then
    begin
        inc( Count ) ; // Round up
        Count := Count div 2 ; // Convert to actual clock count
        Timer := Timer - Count ;
        if( Timer <= 0 ) then
        begin
            Timer := 7905 ;
            Pending_Interrupt := True ;
            Timer_Interrupt := True ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Clear_Watchpoints ;

begin
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
end ;


function TF3850_CPU.Register_Description( Index : integer ) : PChar ;

    procedure Check( Bit : integer ; const Flag : string ) ;

    begin
        if( ( _W and Bit ) = Bit ) then
        begin
            Temp_Register_Description := Temp_Register_Description + Flag ;
        end else
        begin
            Temp_Register_Description := Temp_Register_Description + 'N' + Flag ;
        end ;
        Temp_Register_Description := Temp_Register_Description + ' ' ;
    end ;

begin
    Temp_Register_Description := '' ;
    if( Index = 66 ) then // W
    begin
        Check( 16, 'ICB' ) ;
        Check( 8, 'O' ) ;
        Check( 4, 'Z' ) ;
        Check( 2, 'C' ) ;
        Check( 1, 'S' ) ;
    end ;
    Result := PChar( Temp_Register_Description ) ;
end ;


function TF3850_CPU.Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;

begin
    Result := TF3850_Assembler.Create ;
    TF3850_Assembler( Result ).CPU := self ;
    TF3850_Assembler( Result ).Segments := Segments ;
    Result.Initialize( Master ) ;
    TF3850_Assembler( Result ).Base := Base ;
end ;


function TF3850_CPU.Cancel_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

var Index : integer ;

begin
    Result := Set_Error( F3850_No_Breakpoint ) ; // Assume failure
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


function TF3850_CPU.Get_Clock_Speed : longint ;

begin
    Result := _Speed ;
end ;


procedure TF3850_CPU.Halt ;

begin
    _Halted := True ;
end ;


function TF3850_CPU.Halted : boolean ;

begin
    Result := _Halted ;
end ;


procedure TF3850_CPU.Run_From_Stream( Stream : TCOM_Stream ) ;

begin
    _Run_Stream := Stream ;
    Execute( False, False ) ;
    _Run_Stream := nil ;
end ;


procedure TF3850_CPU.Run ;

begin
    _Halted := False ;
    Execute( False, False ) ;
end ;


function TF3850_CPU.Set_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

begin
    if( ( Address < 0 ) or ( Address > $FFFF ) ) then
    begin
        Result := Set_Error( F3850_Invalid_Address ) ;
        exit ;
    end ;
    if( _Breakpoints.Indexof( Address ) <> -1 ) then
    begin
        Result := Set_Error( F3850_Breakpoint_Exists ) ;
        exit ;
    end ;
    _Breakpoints.Add( Address ) ;
    Result := Set_Error( 0 ) ;
end ;


procedure TF3850_CPU.Set_Clock_Speed( Value : longint ) ;

begin
    _Speed := Value ;
end ;


procedure TF3850_CPU.Step( Into : boolean ) ;

begin
    _Halted := False ;
    Execute( True, Into ) ;
end ;


function TF3850_CPU.Page_Size : longint ;

begin
    Result := 256 ;
end ;


function TF3850_CPU.Clear_Internal_Watchpoint( Address : int64 ;
    Memory : boolean ; Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( F3850Err_No_Cache ) ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > Max_Register ) ) then
        begin
            Result := Set_Error( F3850Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] :=
                _Register_Watchpoints[ Address ] and ( not Access_None ) ;
            Result := Set_Error( 0 ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( F3850Err_No_Cache ) ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > Max_Register ) ) then
        begin
            Result := Set_Error( F3850Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] :=
                _Register_Watchpoints[ Address ] or Access ;
            Result := Set_Error( 0 ) ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Stop ;

begin
    Halt ;
end ;


procedure TF3850_CPU.Send_Signal( const Name : string ; Value : boolean ) ;

var Component : TComponent ;
    Index, Loop : integer ;

begin
    if( _Logger <> nil ) then
    begin
        _Logger.Log( Parent, PChar( Name + ' = ' + inttostr( ord( Value ) ) ), -1, True, LT_Sent_Signal ) ;
    end ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        Component.Set_Signal( PChar( Name ), Value ) ;
    end ;
    if( Name = 'ICB' ) then
    begin
        Index := 0 ;
    end else
    begin
        exit ;
    end ;
    _UI.Signal_Change_Notice( Parent, Index, Value <> Parent.Signal_Active_Low( Index ) ) ;
end ; // TF3850_CPU.Send_Signal


function TF3850_CPU.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var Dummy : integer ;
    Loop, Loop1 : integer ;
    Parser, Watchpoint_Parser : TXML_Parser ;
    S : string ;

begin
    // Setup default state...
    Result := Set_Error( 0 ) ;
    _Halted := False ;
    _Profiling := False ;
    Pending_Interrupt := False ;
    fillchar( _Register_Watchpoints, sizeof( _Register_Watchpoints ), 0 ) ;
    _Breakpoints.Clear ;
    Clear_Watchpoints ;
    Interrupt_Privilege := False ;
    Timer_Interrupt := False ;
    
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
            if( S = '<SPEED>' ) then
            begin
                S := Parser.Get_Section( 'speed' ) ;
                try
                    _Speed := strtoint( S ) ;
                except
                end ;
            end else
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
            if( S = '<W>' ) then
            begin
                S := Parser.Get_Section( 'w' ) ;
                try
                    _W := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<IO_PORT>' ) then
            begin
                S := Parser.Get_Section( 'io_port' ) ;
                try
                    IO_Port_Select := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<TIMER>' ) then
            begin
                S := Parser.Get_Section( 'timer' ) ;
                try
                    Timer := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<TIMER_INTERRUPT/>' ) then
            begin
                Timer_Interrupt := True ;
            end else
            if( S = '<IM>' ) then
            begin
                S := Parser.Get_Section( 'im' ) ;
                try
                    Interrupt_Mode := strtoint( S ) ;
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
                    if( Loop1 > 63 ) then // Too many values
                    begin
                        break ;
                    end ;
                end ;
            end else
            if( S = '<ACCUMULATOR>' ) then
            begin
                S := Parser.Get_Section( 'accumulator' ) ;
                try
                    _Accumulator := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<ISAR>' ) then
            begin
                S := Parser.Get_Section( 'isar' ) ;
                try
                    _ISAR := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<IP/>' ) then
            begin
                Interrupt_Privilege := True ;
            end else
            if( S = '<DC0>' ) then
            begin
                S := Parser.Get_Section( 'dc0' ) ;
                try
                    _DC0 := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<DC1>' ) then
            begin
                S := Parser.Get_Section( 'dc1' ) ;
                try
                    _DC1 := strtoint( S ) ;
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
            if( S = '<STACK>' ) then
            begin
                S := Parser.Get_Section( 'stack' ) ;
                try
                    _Stack := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<ADLO>' ) then
            begin
                S := Parser.Get_Section( 'adlo' ) ;
                try
                    ADLO := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<ADHI>' ) then
            begin
                S := Parser.Get_Section( 'adhi' ) ;
                try
                    ADHI := strtoint( S ) ;
                except
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
end ; // TF3850_CPU.Restore_State


function TF3850_CPU.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

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
    Output( '<speed>' + inttostr( _Speed ) + '</speed>' ) ;
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
    if( Interrupt_Privilege ) then
    begin
        Output( '<ip/>' ) ;
    end ;

    Output( '<Registers>' ) ;
    for Loop1 := 0 to 63 do
    begin
        Output( '|' + inttostr( _Register[ Loop1 ] ) ) ;
    end ;
    Output( '</Registers>' ) ;
    Output( '<accumulator>' + inttostr( _Accumulator ) + '</accumulator>' ) ;
    Output( '<isar>' + inttostr( _ISAR ) + '</isar>' ) ;
    Output( '<w>' + inttostr( _W ) + '</w>' ) ;
    Output( '<pc>' + inttostr( _PC ) + '</pc>' ) ;
    Output( '<dc0>' + inttostr( _DC0 ) + '</dc0>' ) ;
    Output( '<dc1>' + inttostr( _DC1 ) + '</dc1>' ) ;
    Output( '<stack>' + inttostr( _Stack ) + '</stack>' ) ;
    Output( '<Breakpoints>' + _Breakpoints.Serialize + '</Breakpoints>' ) ;
    Output( '<io_port>' + inttostr( IO_Port_Select ) + '</io_port>' ) ;
    Output( '<timer>' + inttostr( Timer ) + '</timer>' ) ;
    Output( '<adlo>' + inttostr( ADLO ) + '</adlo>' ) ;
    Output( '<adhi>' + inttostr( ADHI ) + '</adhi>' ) ;
    if( Timer_Interrupt ) then
    begin
        Output( '<timer_interrupt/>' ) ;
    end ;
    Output( '<im>' + inttostr( Interrupt_Mode ) + '</mi>' ) ;
    Output( '<Register_Watchpoints>' ) ;
    for Loop := 0 to Max_Register do
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
end ; // TF3850_CPU.Save_State


function Register_Name( A : integer ) : string ;

begin
    case A of
        0..11 : Result := 'r' + inttostr( A ) ;
        12 : Result := 'S' ;
        13 : Result := 'I' ;
        14 : Result := 'D' ;
    end ;
end ;


function Simple_BCD_Add( Value1, Value2 : longint ) : longint ;

var C, IC : boolean ;

begin
    Result := Value1 + Value2 ;
    C := Result > 255 ;
    IC := ( ( Value1 and 15 ) + ( Value2 and 15 ) ) > 15 ;
    Result := Result and 255 ;
    if( not C ) then
    begin
        Result := Result + $A0 ;
    end ;
    if( not IC ) then
    begin
        Result := ( Result and $F0 ) or ( ( Result + $A ) and $F ) ;
    end ;
    if( C ) then // Add carry back in
    begin
        Result := Result or $100 ;
    end ;
end ;


function TF3850_CPU.Disassemble( Address : int64 ; Base, Size : longint ;
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
            C := A or Swap( B ) ;
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
            FetchWord := A or swap( B ) ;
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
        case A of
            0 : Result := 'LR A,KU' ;
            1 : Result := 'LR A,KL' ;
            2 : Result := 'LR A,QU' ;
            3 : Result := 'LR A,QL' ;
            4 : Result := 'LR KU,A' ;
            5 : Result := 'LR KL,A' ;
            6 : Result := 'LR QU,A' ;
            7 : Result := 'LR QL,A' ;
            8 : Result := 'LR K,P' ;
            9 : Result := 'LR P,K' ;
            $A : Result := 'LR A,IS' ;
            $B : Result := 'LR IS,A' ;
            $C : Result := 'PK' ;
            $D : Result := 'LR P0,Q' ;
            $E : Result := 'LR Q,DC' ;
            $F : Result := 'LR DC,Q' ;
            $10 : Result := 'LR DC,H' ;
            $11 : Result := 'LR H, DC' ;
            $12 : Result := 'SR 1' ;
            $13 : Result := 'SL 1' ;
            $14 : Result := 'SR 4' ;
            $15 : Result := 'SL 4' ;
            $16 : Result := 'LM' ;
            $17 : Result := 'ST' ;
            $18 : Result := 'COM' ;
            $19 : Result := 'LNK' ;
            $1A : Result := 'DI' ;
            $1B : Result := 'EI' ;
            $1C : Result := 'POP' ;
            $1D : Result := 'LR W,J' ;
            $1E : Result := 'LR J,W' ;
            $1F : Result := 'INC' ;
            $20 : Result := 'LI ' + CVIS( Fetch, 2 ) ;
            $21 : Result := 'NI ' + CVIS( Fetch, 2 ) ;
            $22 : Result := 'OI ' + CVIS( Fetch, 2 ) ;
            $23 : Result := 'XI ' + CVIS( Fetch, 2 ) ;
            $24 : Result := 'AI ' + CVIS( Fetch, 2 ) ;
            $25 : Result := 'CI ' + CVIS( Fetch, 2 ) ;
            $26 : Result := 'IN ' + CVIS( Fetch, 2 ) ;
            $27 : Result := 'OUT ' + CVIS( Fetch, 2 ) ;
            $28 : Result := 'PI ' + CVIS( FetchWord, 4 ) ;
            $29 : Result := 'JMP ' + CVIS( FetchWord, 4 ) ;
            $2A : Result := 'DCI ' + CVIS( FetchWord, 4 ) ;
            $2B : Result := 'NOP' ;
            $2C : Result := 'XDC' ;
            $30..$3E : Result := 'DS ' + Register_Name( A and 15 ) ;
            $40..$4E : Result := 'LR A, ' + Register_Name( A and 15 ) ;
            $50..$5E : Result := 'LR ' + Register_Name( A and 15 ) + ',A' ;
            $60..$67 : Result := 'LISU ' + inttostr( A and 7 ) ;
            $68..$6F : Result := 'LISL ' + inttostr( A and 7 ) ;
            $70 : Result := 'CLR' ;
            $71..$7F : Result := 'LIS ' + inttostr( A and 15 ) ;
            $80 : Result := 'BT ' + CVIS( Fetch, 2 ) ;
            $81 : Result := 'BP ' + CVIS( Fetch, 2 ) ;
            $82 : Result := 'BC ' + CVIS( Fetch, 2 ) ;
            $83 : Result := 'BT ' + CVIS( Fetch, 2 ) ;
            $84 : Result := 'BZ ' + CVIS( Fetch, 2 ) ;
            $85..$87 : Result := 'BT ' + inttostr( A and 15 ) + ',' + CVIS( Fetch, 2 ) ;
            $88 : Result := 'AM' ;
            $89 : Result := 'AMD' ;
            $8A : Result := 'NM' ;
            $8B : Result := 'OM' ;
            $8C : Result := 'XM' ;
            $8D : Result := 'CM' ;
            $8E : Result := 'ADC' ;
            $8F : Result := 'BRZ ' + CVIS( Fetch, 2 ) ;
            $90 : Result := 'BR ' + CVIS( Fetch, 2 ) ;
            $91 : Result := 'BM ' + CVIS( Fetch, 2 ) ;
            $92 : Result := 'BNC ' + CVIS( Fetch, 2 ) ;
            $93 : Result := 'BF ' + CVIS( Fetch, 2 ) ;
            $94 : Result := 'BNZ ' + CVIS( Fetch, 2 ) ;
            $95..$97 : Result := 'BF ' + inttostr( A and 15 ) + ',' + CVIS( Fetch, 2 ) ;
            $98 : Result := 'BNO ' + CVIS( Fetch, 2 ) ;
            $99..$9F : Result := 'BF ' + inttostr( A and 15 ) + ',' + CVIS( Fetch, 2 ) ;
            $A0..$AF : Result := 'INS ' + inttostr( A and 15 ) ;
            $B0..$BF : Result := 'OUTS ' + inttostr( A and 15 ) ;
            $C0..$CE : Result := 'AS ' + Register_Name( A and 15 ) ;
            $D0..$DE : Result := 'ASD ' + Register_Name( A and 15 ) ;
            $E0..$EE : Result := 'XS ' + Register_Name( A and 15 ) ;
            $F0..$FE : Result := 'NS ' + Register_Name( A and 15 ) ;
            else _Disassemble := 'DB ' + Cvis( A, 2 ) ;
        end ; // case A of
    end ; { TF3850_CPU.Disassemble._Disassemble }

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
end ; { TF3850_CPU.Disassemble }


procedure TF3850_CPU.Set_Flags( Value : byte ; O, C : boolean ) ;

var B : integer ;

begin
    B := 0 ;
    if( C ) then
    begin
       B := B or 2 ; // Carry
    end ;
    if( ( O or C ) and ( O <> C ) ) then
    begin
       B := B or 8 ; // Overflow
    end ;
    if( Value = 0 ) then
    begin
        B := B or 4 ; // Zero
    end ;
    if( Value < 128 ) then
    begin
        B := B or 1 ; // Sign
    end ;
    if( _W > 15 ) then
    begin
        B := B or 16 ; // preserve ICB
    end ;
    W := B ;
end ;


function TF3850_CPU.Resolve_Register( A : byte ) : byte ;

begin
    case A of
        0..11 : Result := A ;
        12 : Result := ISAR ;
        13 : begin
                 Result := ISAR ;
                 ISAR := ( Result and not 7 ) or ( ( Result + 1 ) and 7 ) ;
             end ;
        else begin
                 Result := ISAR ;
                 ISAR := ( Result and not 7 ) or ( ( Result - 1 ) and 7 ) ;
             end ;
    end ;
end ;


procedure TF3850_CPU.Execute( Single_Step, Into : boolean ) ;

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
        X := swap( Fetch ) or X ;
        Fetch_Word := X ;
        Increment_Clock( 1 ) ; // Extra byte read is another machine cycle
    end ;


    function Execute_F3850( A : integer ) : boolean ;

    var B, C, F : integer ;

    begin
        Execute_F3850 := True ; { Assume success }
        case A of
            0..7 : Increment_Clock( 2 ) ;
            8..9 : Increment_Clock( 8 ) ;
            $A..$B : Increment_Clock( 2 ) ;
            $C..$11 : Increment_Clock( 8 ) ;
            $12..$15 : Increment_Clock( 2 ) ;
            $16..$17 : Increment_Clock( 5 ) ;
            $18..$1B : Increment_Clock( 2 ) ;
            $1C..$1D : Increment_Clock( 4 ) ;
            $1E..$1F : Increment_Clock( 2 ) ;
            $20..$25 : Increment_Clock( 5 ) ;
            $26..$27 : Increment_Clock( 8 ) ;
            $28 : Increment_Clock( 13 ) ;
            $29 : Increment_Clock( 11 ) ;
            $2A : Increment_Clock( 12 ) ;
            $2B : Increment_Clock( 2 ) ;
            $2C : Increment_Clock( 4 ) ;
            $30..$3E : Increment_Clock( 3 ) ;
            $40..$4E : Increment_Clock( 2 ) ;
            $50..$5E : Increment_Clock( 2 ) ;
            $60..$7F : Increment_Clock( 2 ) ;
            $88..$8E : Increment_Clock( 5 ) ;
            $90 : Increment_Clock( 7 ) ;
            $C0..$CE : Increment_Clock( 2 ) ;
            $D0..$DE : Increment_Clock( 4 ) ;
            $E0..$EE : Increment_Clock( 2 ) ;
            $F0..$FE : Increment_Clock( 2 ) ;
        end ;

        Interrupt_Privilege := False ;
        if( A = 0 ) then { LR A,KU }
        begin
            Accumulator := Register[ 12 ] ;
            Exit ;
        end ;
        if( A = 1 ) then { LR A,KL }
        begin
            Accumulator := Register[ 13 ] ;
            Exit ;
        end ;
        if( A = 2 ) then { LR A,QU }
        begin
            Accumulator := Register[ 14 ] ;
            Exit ;
        end ;
        if( A = 3 ) then { LR A,QL }
        begin
            Accumulator := Register[ 15 ] ;
            Exit ;
        end ;
        if( A = 4 ) then { LR KU,A }
        begin
            Register[ 12 ] := Accumulator ;
            Exit ;
        end ;
        if( A = 5 ) then { LR KL,A }
        begin
            Register[ 13 ] := Accumulator ;
            Exit ;
        end ;
        if( A = 6 ) then { LR QU,A }
        begin
            Register[ 14 ] := Accumulator ;
            Exit ;
        end ;
        if( A = 7 ) then { LR QL,A }
        begin
            Register[ 15 ] := Accumulator ;
            Exit ;
        end ;
        if( A = 8 ) then { LR K,P }
        begin
            B := Stack ;
            Register[ 12 ] := B shr 8 ;
            Register[ 13 ] := B and 255 ;
            Exit ;
        end ;
        if( A = 9 ) then { LR P,K }
        begin
            Stack := ( Register[ 12 ] shl 8 ) or Register[ 13 ] ;
            Exit ;
        end ;
        if( A = $A ) then { LR A,IS }
        begin
            Accumulator := ISAR ;
            Exit ;
        end ;
        if( A = $B ) then { LR IS,A }
        begin
            ISAR := Accumulator ;
            Exit ;
        end ;
        if( A = $C ) then // PK
        begin
            if( ( _RTS_Flags and RTS_Want_Calls ) <> 0 ) then
            begin
                if( _RTS.Call( ( Register[ 12 ] shl 8 ) or Register[ 13 ] ) ) then
                begin
                    exit ;
                end ;
            end ;
            Stack := PC ;
            PC := ( Register[ 12 ] shl 8 ) or Register[ 13 ] ;
            Interrupt_Privilege := True ;
            Exit ;
        end ;
        if( A = $D ) then // LR P0,Q
        begin
            PC := ( Register[ 14 ] shl 8 ) or Register[ 15 ] ;
            Exit ;
        end ;
        if( A = $E ) then // LR Q,DC
        begin
            B := DC0 ;
            Register[ 14 ] := B shr 8 ;
            Register[ 15 ] := B and 255 ;
            Exit ;
        end ;
        if( A = $F ) then // LR DC,Q
        begin
            DC0 := ( Register[ 14 ] shl 8 ) or Register[ 15 ] ;
            Exit ;
        end ;
        if( A = $10 ) then // LR DC,H
        begin
            DC0 := ( Register[ 10 ] shl 8 ) or Register[ 11 ] ;
            Exit ;
        end ;
        if( A = $11 ) then // LR H, DC
        begin
            B := DC0 ;
            Register[ 10 ] := B shr 8 ;
            Register[ 11 ] := B and 255 ;
            Exit ;
        end ;
        if( A = $12 ) then // SR 1
        begin
            Accumulator := _Accumulator shr 1 ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( A = $13 ) then // SL 1
        begin
            Accumulator := _Accumulator shl 1 ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( A = $14 ) then // SR 4
        begin
            Accumulator := _Accumulator shr 4 ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( A = $15 ) then // SL 4
        begin
            Accumulator := _Accumulator shl 4 ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( A = $16 ) then // LM
        begin
            Accumulator := Byte_Read( DC0 ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( A = $17 ) then // ST
        begin
            ByteWrite( DC0, Accumulator ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( A = $18 ) then // COM
        begin
            Accumulator := not _Accumulator ;
            Set_Flags( _Accumulator, False, False ) ;
            DC0 := _DC0 + Accumulator ;
            Exit ;
        end ;
        if( A = $19 ) then // LNK
        begin
            if( ( _W and 2 ) = 0 ) then // Carry not set
            begin
                Set_Flags( _Accumulator, False, False ) ; // Clear flags
            end else
            begin
                C := _Accumulator ;
                B := C + 1 ; // Add carry
                _Accumulator := B ;
                Set_Flags( _Accumulator, ( B and 64 ) <> ( C and 64 ), B > 255 ) ;
            end ;
            Exit ;
        end ;
        if( A = $1A ) then // DI
        begin
            W := _W and 15 ;
            Exit ;
        end ;
        if( A = $1B ) then // EI
        begin
            Interrupt_Privilege := True ;
            W := _W or 16 ;
            Exit ;
        end ;
        if( A = $1C ) then // POP
        begin
            Interrupt_Privilege := True ;
            PC := Stack ;
            Exit ;
        end ;
        if( A = $1D ) then // LR W,J
        begin
            Interrupt_Privilege := True ;
            W := Register[ 9 ] ;
            Exit ;
        end ;
        if( A = $1E ) then // LR J,W
        begin
            Register[ 9 ] := W ;
            Exit ;
        end ;
        if( A = $1F ) then // INC
        begin
            C := _Accumulator ;
            B := C + 1 ;
            _Accumulator := B ;
            Set_Flags( _Accumulator, ( B and 64 ) <> ( C and 64 ), B > 255 ) ;
            Exit ;
        end ;
        if( A = $20 ) then // LI
        begin
            Accumulator := Fetch ;
            Exit ;
        end ;
        if( A = $21 ) then // NI
        begin
            Accumulator := _Accumulator and Fetch ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( A = $22 ) then // OI
        begin
            Accumulator := _Accumulator or Fetch ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( A = $23 ) then // XI
        begin
            Accumulator := _Accumulator xor Fetch ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( A = $24 ) then // AI
        begin
            C := _Accumulator ;
            B := C + Fetch ;
            Accumulator := B ;
            Set_Flags( _Accumulator, ( B and 64 ) <> ( C and 64 ), B > 255 ) ;
            Exit ;
        end ;
        if( A = $25 ) then // CI
        begin
            C := ( ( not _Accumulator ) + 1 ) and 255 ;
            B := C + Fetch ;
            Set_Flags( B, ( B and 64 ) <> ( C and 64 ), B > 255 ) ;
            Exit ;
        end ;
        if( A = $26 ) then // IN
        begin
            Accumulator := Input( Fetch ) ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( A = $27 ) then // OUT
        begin
            Output( Fetch, Accumulator ) ;
            Exit ;
        end ;
        if( A = $28 ) then // PI
        begin
            if( ( _RTS_Flags and RTS_Want_Calls ) <> 0 ) then
            begin
                if( _RTS.Call( ( Register[ 12 ] shl 8 ) or Register[ 13 ] ) ) then
                begin
                    exit ;
                end ;
            end ;
            Interrupt_Privilege := True ;
            Stack := PC ;
            PC := Fetch_Word ;
            Accumulator := PC shr 8 ;
            Exit ;
        end ;
        if( A = $29 ) then // JMP
        begin
            Interrupt_Privilege := True ;
            B := Fetch_Word ;
            PC := B ;
            if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
            begin
                _RTS.Jumped ;
            end ;
            Exit ;
        end ;
        if( A = $2A ) then // DCI
        begin
            DC0 := Fetch_Word ;
            Exit ;
        end ;
        if( A = $2B ) then // NOP
        begin
            Exit ;
        end ;
        if( A = $2C ) then // XDC
        begin
            B := _DC0 ;
            DC0 := _DC1 ;
            DC1 := B ;
            Exit ;
        end ;
        if( ( A >= $30 ) and ( A <= $3E ) ) then // DS r
        begin
            B := Resolve_Register( A and 15 ) ;
            C := _Register[ B ] ;
            F := C - 1 ;
            Register[ B ] := F ;
            Set_Flags( F and 255, ( F and 64 ) <> ( C and 64 ), F > 255 ) ;
            Exit ;
        end ;
        if( ( A >= $40 ) and ( A <= $4E ) ) then // LR A, r
        begin
            B := Resolve_Register( A and 15 ) ;
            Accumulator := Register[ B ] ;
            Exit ;
        end ;
        if( ( A >= $50 ) and ( A <= $5E ) ) then // LR r, A
        begin
            B := Resolve_Register( A and 15 ) ;
            Register[ B ] := Accumulator ;
            Exit ;
        end ;
        if( ( A >= $60 ) and ( A <= $67 ) ) then // LISU
        begin
            ISAR := ( _ISAR and 7 ) or ( ( A and 7 ) shl 3 ) ;
            Exit ;
        end ;
        if( ( A >= $68 ) and ( A <= $6F ) ) then // LISL
        begin
            ISAR := ( _ISAR and ( not 7 ) ) or ( A and 7 ) ;
            Exit ;
        end ;
        if( ( A and $F0 ) = $70 ) then // CLR/LIS
        begin
            Accumulator := A and 15 ;
            Exit ;
        end ;
        if( ( A >= $80 ) and ( A <= $87 ) ) then // BT (BP/BC/BZ)
        begin
            B := Fetch ;
            if( B > 127 ) then
            begin
                B := B or ( not 255 ) ; // Sign-extend
            end ;
            if( _Run_Stream = nil ) then // Not in immediate mode
            begin
                B := B - 2 ;
            end ;
            if( ( _W and A and 7 ) <> 0 ) then
            begin
                PC := _PC + B ;
                Increment_Clock( 7 ) ;
            end else
            begin
                Increment_Clock( 6 ) ;
            end ;
            Exit ;
        end ;
        if( A = $88 ) then // AM
        begin
            C := _Accumulator ;
            B := C + Byte_Read( DC0 ) ;
            Accumulator := B ;
            Set_Flags( _Accumulator, ( B and 64 ) <> ( C and 64 ), B > 255 ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( A = $89 ) then // AMD
        begin
            B := Simple_BCD_Add( _Accumulator, Byte_Read( DC0 ) ) ;
            Accumulator := B ;
            Set_Flags( _Accumulator, False, B > 255 ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( A = $8A ) then // NM
        begin
            Accumulator := _Accumulator and Byte_Read( DC0 ) ;
            Set_Flags( _Accumulator, False, False ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( A = $8B ) then // OM
        begin
            Accumulator := _Accumulator or Byte_Read( DC0 ) ;
            Set_Flags( _Accumulator, False, False ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( A = $8C ) then // XM
        begin
            Accumulator := _Accumulator xor Byte_Read( DC0 ) ;
            Set_Flags( _Accumulator, False, False ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( A = $8D ) then // CM
        begin
            C := ( ( not _Accumulator ) + 1 ) and 255 ;
            B := C + Byte_Read( DC0 ) ;
            Set_Flags( B, ( B and 64 ) <> ( C and 64 ), B > 255 ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( A = $8E ) then // ADC
        begin
            DC0 := _DC0 + Accumulator ;
            Exit ;
        end ;
        if( A = $8F ) then // BRZ (B7)
        begin
            B := Fetch ;
            if( B > 127 ) then
            begin
                B := B or ( not 255 ) ; // Sign-extend
            end ;
            if( ( _ISAR and 7 ) <> 7 ) then
            begin
                if( _Run_Stream = nil ) then // Not in immediate mode
                begin
                    B := B - 2 ;
                end ;
                PC := _PC + B ;
                Increment_Clock( 5 ) ;
            end else
            begin
                Increment_Clock( 4 ) ;
            end ;
            Exit ;
        end ;
        if( A = $90 ) then // BR
        begin
            B := Fetch ;
            if( B > 127 ) then
            begin
                B := B or ( not 255 ) ; // Sign-extend
            end ;
            if( _Run_Stream = nil ) then // Not in immediate mode
            begin
                B := B - 2 ;
            end ;
            if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
            begin
                _RTS.Jumped ;
            end ;
            PC := _PC + B ;
            Exit ;
        end ;
        if( ( A >= $91 ) and ( A <= $9F ) ) then // BF (BM/BNC/BNS/BNO)
        begin
            B := Fetch ;
            if( B > 127 ) then
            begin
                B := B or ( not 255 ) ; // Sign-extend
            end ;
            if( ( _W and A and 15 ) = 0 ) then
            begin
                if( _Run_Stream = nil ) then // Not in immediate mode
                begin
                    B := B - 2 ;
                end ;
                if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                begin
                    _RTS.Jumped ;
                end ;
                PC := _PC + B ;
                Increment_Clock( 7 ) ;
            end else
            begin
                Increment_Clock( 6 ) ;
            end ;
            Exit ;
        end ;
        if( ( A and $F0 ) = $A0 ) then // INS
        begin
            Accumulator := Input( A and 15 ) ;
            Set_Flags( _Accumulator, False, False ) ;
            if( ( A and 15 ) < 2 ) then
            begin
                Increment_Clock( 4 ) ;
            end else
            begin
                Increment_Clock( 8 ) ;
            end ;   
            Exit ;
        end ;
        if( ( A and $F0 ) = $B0 ) then // OUTS
        begin
            Interrupt_Privilege := True ;
            Output( A and 15, Accumulator ) ;
            if( ( A and 15 ) < 2 ) then
            begin
                Increment_Clock( 4 ) ;
            end else
            begin
                Increment_Clock( 8 ) ;
            end ;
            Exit ;
        end ;
        if( ( A >= $C0 ) and ( A <= $CE ) ) then // AS r
        begin
            C := _Accumulator ;
            B := Resolve_Register( A and 15 ) ;
            B := C + Register[ B ] ;
            Accumulator := B ;
            Set_Flags( _Accumulator, ( B and 64 ) <> ( C and 64 ), B > 255 ) ;
            Exit ;
        end ;
        if( ( A >= $D0 ) and ( A <= $DE ) ) then // ASD r
        begin
            B := Resolve_Register( A and 15 ) ;
            B := Simple_BCD_Add( _Accumulator, Register[ B ] ) ;
            Accumulator := B ;
            Set_Flags( _Accumulator, False, B > 255 ) ;
            DC0 := _DC0 + 1 ;
            Exit ;
        end ;
        if( ( A >= $E0 ) and ( A <= $EE ) ) then // XS r
        begin
            B := Resolve_Register( A and 15 ) ;
            Accumulator := _Accumulator xor Register[ B ] ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        if( ( A >= $F0 ) and ( A <= $FE ) ) then // NS r
        begin
            B := Resolve_Register( A and 15 ) ;
            Accumulator := _Accumulator and Register[ B ] ;
            Set_Flags( _Accumulator, False, False ) ;
            Exit ;
        end ;
        Execute_F3850 := False ; { Failure }
    end ;

var A : integer ;
    Count : integer ;
    Nest_Level : integer ;
    Original_PC : longint ;

begin // TF3850_CPU.Execute
    // Setup...
    Count := 0 ;
    Nest_Level := 0 ;

    // Execution loop
    while( True ) do
    begin
        if( Blocked ) then // Waiting for system to catch up to our clock time
        begin
            Do_Wait ;
            continue ;
        end ;
        if( _Run_Stream = nil ) then // Not in immediate mode
        begin
            if( ( ( W and 16 ) <> 0 ) and ( ( Interrupt_Mode and 1 ) = 1 ) ) then // Interrupts are enabled
            begin
                if( Pending_Interrupt and ( not Interrupt_Privilege ) ) then
                begin
                    W := _W and 15 ; // Clear ICB
                    Pending_Interrupt := False ;
                    Stack := PC ;
                    if( Timer_Interrupt ) then
                    begin
                        Timer_Interrupt := False ;
                        A := ( ( ADHI shl 8 ) or ADLO ) and ( not 128 ) ;
                    end else
                    begin
                        A := ( ADHI shl 8 ) or ADLO or 128 ;
                    end ;
                    if( ( _RTS_Flags and RTS_Want_Interrupts ) <> 0 ) then
                    begin
                        if( _RTS.Trap( A ) ) then
                        begin
                            exit ;
                        end ;
                    end ;
                    PC := A ;
                end ;
            end ;
            if( EXTRES ) then // So long as this is asserted- we are idle
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
            end ;
        end ;
        if( _Profiling ) then
        begin
            TF3850_Profiler( Parent.Profiler ).Increment( Domain_Execution_Addresses, PC ) ;
            TF3850_Profiler( Parent.Profiler ).Increment( Domain_Other, Domain_Other_Instruction_Count ) ;
        end ;
        Log_Trace( 'Executing instruction at address ' + cvtb( 10, Base, inttostr( PC ) ) + ': ' + Instruction_At( PC ) ) ;
        Original_PC := PC ;
        A := Fetch ;
        if( _Logger <> nil ) then
        begin
            _Logger.Update( Parent, Original_PC, A ) ;
        end ;

        if( _Profiling ) then
        begin
            TF3850_Profiler( Parent.Profiler ).Increment( Domain_Instructions, A ) ;
        end ;

        if( Execute_F3850( A ) ) then
        begin
            continue ;
        end ;

        Signal_Exception( '', 0 ) ; // Invalid instruction
    end ; // while( True )
end ; // TF3850_CPU.Execute


procedure TF3850_CPU.Output( Port, Value : integer ) ;

var Component : TComponent ;
    Loop : integer ;
    UEC : TUnified_Exception ;

begin
    try
        if( ( Port and $FC ) = ( IO_Port_Select shl 2 ) ) then // 3853 access
        begin
            case ( Port and 3 ) of
                0 : ADLO := Value ;
                1 : ADHI := Value ;
                2 : // Interrrupt control
                    begin
                        Interrupt_Mode := Value and 3 ;
                        Pending_Interrupt := False ;
                    end ;
                3 : // Timer
                    begin
                        if( Value = 255 ) then // disabling
                        begin
                            Timer := -1 ;
                        end else
                        begin
                            Timer := Translate_Timer[ Value and 255 ] * 31 ;
                        end ;
                        if( Pending_Interrupt and Timer_Interrupt ) then
                        begin
                            Pending_Interrupt := False ;
                            Timer_Interrupt := False ;
                        end ;
                    end ;
            end ; // case
        end else
        begin
            for Loop := 0 to Parent.Outputs.Count - 1 do
            begin
                Component := TComponent( Parent.Outputs[ Loop ] ) ;
                UEC := Component.Write( Port, Value, 8, IO_Type_IO ) ;
                if( UEC <> nil ) then
                begin
                    exit ;
                end ;
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
                TF3850_Profiler( Parent.Profiler ).Increment( Domain_Port_Outputs, Port ) ;
            end ;
            Log_Trace( 'Output ' + inttostr( Value ) + '. to port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Input( Port : integer ) : integer ;

var Component : TComponent ;
    Loop : integer ;

begin
    Result := 0 ;
    try
        if( ( Port and $FC ) = ( IO_Port_Select shl 2 ) ) then // 3853 access
        begin
            case ( Port and 3 ) of
                0 : Memory_Data_Latch := ADLO ;
                1 : Memory_Data_Latch := ADHI ;
                else Memory_Data_Latch := 255 ;// Cannot read from this port
            end ; // case
        end else
        begin
            Memory_Data_Latch := 255 ;
            for Loop := 0 to Parent.Inputs.Count - 1 do
            begin
                Component := TComponent( Parent.Inputs[ Loop ] ) ;
                if( Component.Read( Port, 8, IO_Type_IO ) ) then
                begin
                    exit ;
                end ;
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
                TF3850_Profiler( Parent.Profiler ).Increment( Domain_Port_Inputs, Port ) ;
            end ;
            Log_Trace( 'Input ' + inttostr( Result ) + '. from port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Get_Register( Index : byte ) : byte ;

    procedure Check_Watch( Index : integer ) ;

    begin
        if( ( _Register_Watchpoints[ Index ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Read, 0, False, True, False ) ;
        end ;
    end ;

begin
    Result := _Register[ Index ] ;
    if( _Run_Stream = nil ) then
    begin
        Check_Watch( Index ) ;

        // Handle synonym registers...
        case Index of
            9 : Check_Watch( 71 ) ;
            10..11 : Check_Watch( 70 ) ;
            12..13 : Check_Watch( 72 ) ;
            14..15 :
                begin
                    Check_Watch( 73 ) ;
                    Check_Watch( 74 ) ;
                end ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Set_Register( Index : byte ; Value : byte ) ;

    procedure Check_Watch( Index : integer ) ;

    begin
        if( ( _Register_Watchpoints[ Index ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Write, 0, False, True, False ) ;
        end ;
    end ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( Index, Value ) ;
    end ;
    _Register[ Index ] := Value ;
    if( _Run_Stream = nil ) then
    begin
        Check_Watch( Index ) ;

        // Handle synonym registers...
        case Index of
            9 : Check_Watch( 71 ) ;
            10..11 : Check_Watch( 70 ) ;
            12..13 : Check_Watch( 72 ) ;
            14..15 :
                begin
                    Check_Watch( 73 ) ;
                    Check_Watch( 74 ) ;
                end ;
        end ;
    end ;
end ;


function TF3850_CPU.Get_PC : word ;

begin
    Result := _PC ;
end ;


procedure TF3850_CPU.Set_PC( Value : word ) ;

begin
    _PC := Value ;
end ;


function TF3850_CPU.Get_Accumulator : byte ;

begin
    Result := _Accumulator ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 77 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 77, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Set_Accumulator( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 77, Value ) ;
    end ;
    _Accumulator := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 77 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 77, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Get_W : byte ;

begin
    Result := _W ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 66 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 66, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Set_W( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 66, Value ) ;
    end ;
    if( ( _W and 16 ) <> ( Value and 16 ) ) then // Changing ICB
    begin
        Send_Signal( 'ICB', ( Value and 16 ) = 0 ) ;
    end ;
    _W := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 66 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 66, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Get_ISAR : byte ;

begin
    Result := _ISAR ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 65 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 65, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Set_ISAR( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 65, Value ) ;
    end ;
    _ISAR := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 65 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 65, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Get_Stack : word ;

begin
    Result := _Stack ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 67 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 67, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Set_Stack( Value : word ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 67, Value ) ;
    end ;
    _Stack := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 67 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 67, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Get_DC0 : word ;

begin
    Result := _DC0 ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 68 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 68, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Set_DC0( Value : word ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 68, Value ) ;
    end ;
    _DC0 := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 68 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 68, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Get_DC1 : word ;

begin
    Result := _DC1 ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 69 ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( 69, Access_Read, 0, False, True, False ) ;
        end ;
    end ;
end ;


procedure TF3850_CPU.Set_DC1( Value : word ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 69, Value ) ;
    end ;
    _DC1 := Value ;
    if( _Run_Stream = nil ) then
    begin
        if( ( _Register_Watchpoints[ 69 ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( 69, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ;


function TF3850_CPU.Get_H : word ;

begin
    Result := _Register[ 10 ] shl 8 or _Register[ 11 ] ;
end ;


procedure TF3850_CPU.Set_H( Value : word ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 10, Value ) ;
    end ;
    _Register[ 10 ] := Value shr 8 ;
    _Register[ 11 ] := Value and 15 ;
end ;


function TF3850_CPU.Get_J : byte ;

begin
    Result := _Register[ 9 ] ;
end ;


procedure TF3850_CPU.Set_J( Value : byte ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 9, Value ) ;
    end ;
    _Register[ 9 ] := Value ;
end ;


function TF3850_CPU.Get_K : word ;

begin
    Result := _Register[ 12 ] shl 8 or _Register[ 13 ] ;
end ;


procedure TF3850_CPU.Set_K( Value : word ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 12, Value ) ;
    end ;
    _Register[ 12 ] := Value shr 8 ;
    _Register[ 13 ] := Value and 15 ;
end ;


function TF3850_CPU.Get_L : word ;

begin
    Result := _Register[ 14 ] shl 8 or _Register[ 15 ] ;
end ;


procedure TF3850_CPU.Set_L( Value : word ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 14, Value ) ;
    end ;
    _Register[ 14 ] := Value shr 8 ;
    _Register[ 15 ] := Value and 15 ;
end ;


function TF3850_CPU.Translate( Space : integer ; Address : int64 ) : int64 ;

begin
    Translate := Address ;
end ;


function TF3850_CPU.Default_Base : integer ;

begin
    Result := 16 ;
end ;


function TF3850_CPU.Get_Low_Memory : int64 ;

begin
    Result := 0 ;
end ;


function TF3850_CPU.Get_High_Memory : int64 ;

begin
    Result := 65535 ;
end ;


function TF3850_CPU.Get_Low_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TF3850_CPU.Get_High_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TF3850_CPU.Get_Low_Port : int64 ;

begin
    Result := 1 ;
end ;


function TF3850_CPU.Get_High_Port : int64 ;

begin
    Result := 255 ;
end ;


function TF3850_CPU.Support_Virtual_Address : boolean ;

begin
    Result := False ;
end ;


function TF3850_CPU.Segment_Size( Index : integer ) : integer ;

begin
    if( ( Index < 0 ) or ( Index >= Segments.Count ) ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := Segments[ Index ] ;
    end ;
end ;


function TF3850_CPU.Register_Name( Index : integer ) : PChar ;

begin
    Temp_Register_Name := '' ; // Invalid index
    case Index of
        0..63 : Temp_Register_Name := 'R' + inttostr( Index ) ;
        64 : Temp_Register_Name := 'PC0' ;
        65 : Temp_Register_Name := 'ISAR' ;
        66 : Temp_Register_Name := 'W' ;
        67 : Temp_Register_Name := 'PC1' ;
        68 : Temp_Register_Name := 'DC0' ;
        69 : Temp_Register_Name := 'DC1' ;
        70 : Temp_Register_Name := 'H' ;
        71 : Temp_Register_Name := 'J' ;
        72 : Temp_Register_Name := 'K' ;
        73 : Temp_Register_Name := 'L' ;
        74 : Temp_Register_Name := 'Q' ;
        75 : Temp_Register_Name := 'Timer' ;
        76 : Temp_Register_Name := 'Interrupt mode' ;
        77 : Temp_Register_Name := 'A' ;
    end ;
    Result := PChar( Temp_Register_Name ) ;
end ;


function TF3850_CPU.Register_Size( Index : integer ) : integer ;

begin
    Result := 0 ;
    case Index of
        0..63 : Result := 8 ; // Registers
        64 : Result := 16 ; // PC
        65 : Result := 6 ; // ISAR
        66 : Result := 5 ; // W
        67..70 : Result := 16 ; // PC1, DC0, DC1, H
        71 : Result := 8 ; // J
        72..74 : Result := 16 ; // K, L, Q
        75 : Result := 8 ; // Timer
        76 : Result := 2 ; // Interrupt mode
        77 : Result := 8 ; // A
    end ;
end ;


procedure TF3850_CPU.Restart ;

begin
    _W := _W and 15 ;
    Pending_Interrupt := False ;
    _Stack := _PC ;
    _PC := 0 ;

    _Run_Stream := nil ;
end ;


function TF3850_CPU.Top_Of_Stack( Index : integer ) : int64 ;

begin
    Top_Of_Stack := 0 ;
end ;


function TF3850_CPU.Big_Endian : boolean ;

begin
    Result := True ;
end ;


function TF3850_CPU.Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;

begin
    _RTS := RTS ;
    _RTS_Flags := Flags ;
    Result := nil ;
end ;


function TF3850_CPU.Get_Current_Address( Space : integer ;
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


procedure TF3850_CPU.Set_Current_Address( Space : integer ; Physical : boolean ;
    Value : int64 ) ;

begin
    if( Space = 0 ) then
    begin
        _PC := Value ;
    end ;
end ;



// TF3850 methods...

{ Internal utility routines... }

// API...

// Generic CPU support routines...

function TF3850.Facility_Code : longint ;

begin
    Result := F3850Err_Facility ;
end ;


function TF3850.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU := TF3850_CPU.Create ;
    _CPU._UI := UI ;
    _CPU.Parent := self ;
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    CPU.Restart ; // Do power-on reset
end ;


function TF3850.Terminate : TUnified_Exception ;

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


function TF3850.Serial_Number : integer ;

begin
    Result := _Serial_Number ;
end ;


function TF3850.Child_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TF3850.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Clear_Watchpoint( Address, Memory, Access ) ;
end ;


function TF3850.Component_Type : longint ;

begin
    Result := Component_Type_CPU ; // CPU
end ;


function TF3850.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := _CPU.Set_Error( F3850Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Inputs.Indexof( Component ) <> -1 ) then
    begin
        Result := _CPU.Set_Error( F3850Err_Already_Connected ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Result := _CPU.Set_Error( F3850Err_Success ) ;
end ;


function TF3850.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := _CPU.Set_Error( F3850Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Outputs.Indexof( Component ) <> -1 ) then
    begin
        Result := _CPU.Set_Error( F3850Err_Already_Connected ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Result := _CPU.Set_Error( F3850Err_Success ) ;
end ;


function TF3850.CPU : TCPU ;

begin
    Result := _CPU ;
end ;


function TF3850.Debugger : TDebug_Interface ;

begin
    Result := nil ; // TODO
end ;


function TF3850.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Result := _CPU.Set_Error( F3850Err_Component_Not_Found ) ;
    end else
    begin
	    Result := _CPU.Set_Error( F3850Err_Success ) ;
	    Inputs.Remove( Component ) ;
    end ;
end ;


function TF3850.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Result := _CPU.Set_Error( F3850Err_Component_Not_Found ) ;
    end else
    begin
	    Result := _CPU.Set_Error( F3850Err_Success ) ;
	    Outputs.Remove( Component ) ;
    end ;
end ;


function TF3850.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var V : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        _CPU.Set_Error( F3850Err_No_Cache ) ;
    end else
    begin
        if( Address > Max_Register ) then
        begin
            Result := Set_Error( F3850Err_Invalid_Register ) ;
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
            0..63 : _CPU._Register[ Address ] := V ;
            64 : _CPU._PC := V ;
            65 : _CPU._ISAR := V ;
            66 : _CPU._W := V ;
            67 : _CPU._Stack := V ;
            68 : _CPU._DC0 := V ;
            69 : _CPU._DC1 := V ;
            70 : _CPU.H := V ;
            71 : _CPU.J := V ;
            72 : _CPU.K := V ;
            73 : _CPU.L := V ;
            74 : _CPU.Q := V ;
            75 : _CPU.Timer := V ;
            76 : _CPU.Interrupt_Mode := V and 3 ;
            77 : _CPU.Accumulator := V ;
        end ; // case Address
    end ; // if( Memory )
end ; // TF3850.Deposit


function TF3850.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var Len, V : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        _CPU.Set_Error( F3850Err_No_Cache ) ;
    end else
    begin
        if( Address > Max_Register ) then
        begin
            Result := Set_Error( F3850Err_Invalid_Register ) ;
            exit ;
        end ;
        _CPU.Set_Error( 0 ) ;

        if( Size = 0 ) then
        begin
            exit ;
        end ;
        Len := _CPU.Register_Size( Address ) ;
        if( Size > Len ) then
        begin
            Size := Len ;
        end ;
        case Address of
            0..63 : V := _CPU._Register[ Address ] ;
            64 : V := _CPU._PC ;
            65 : V := _CPU._ISAR ;
            66 : V := _CPU._W ;
            67 : V := _CPU._Stack ;
            68 : V := _CPU._DC0 ;
            69 : V := _CPU._DC1 ;
            70 : V := _CPU.H ;
            71 : V := _CPU.J ;
            72 : V := _CPU.K ;
            73 : V := _CPU.L ;
            74 : V := _CPU.Q ;
            75 : V := _CPU.Timer ;
            76 : V := _CPU.Interrupt_Mode ;
            77 : V := _CPU._Accumulator ;
        end ; // case Address
        move( V, Buffer^, ( Size + 7 ) div 8 ) ;
    end ; // if( Memory )
end ; // TF3850.Examine


function TF3850.Get_Access_Mode( Address : int64 ; Memory : boolean ) : longint ;

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


function TF3850.Get_Profiling : boolean ;

begin
    Result := _CPU._Profiling ;
end ;


function TF3850.Get_Read_Latency : longint ;

begin
    Result := 0 ;
end ;


function TF3850.Get_Write_Latency : longint ;

begin
    Result := 0 ;
end ;


function TF3850.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
        Result := nil ;
        _CPU.Set_Error( F3850Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Inputs[ Index ] ;
end ;


const F3850_Name : string = 'Fairchild 3850/3853' ;

function TF3850.Name : PChar ;

begin
    Result := PChar( F3850_Name ) ;
end ;


function TF3850.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Outputs.Count ) ) then
    begin
        Result := nil ;
        _CPU.Set_Error( F3850Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Outputs[ Index ] ;
end ;


function TF3850.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ; // Doens't apply to CPUs
end ;


function TF3850.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
end ;


function TF3850.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
end ;


function TF3850.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( F3850_Invalid_Operation ) ;
end ;


procedure TF3850.Set_Profiling( _On, Children : boolean ) ;

begin
    _CPU._Profiling := _On ;
end ;


procedure TF3850.Set_Read_Latency( Value : longint ) ;

begin
    // Do nothing - we have no read latency
end ;


function TF3850.Set_Watchpoint( Address : int64 ; Memory : boolean ;
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
            Result := _CPU.Set_Error( F3850_Invalid_Address ) ;
            exit ;
        end ;
        _CPU._Port_Watchpoints[ Address ] := _CPU._Port_Watchpoints[ Address ] or Access ;
    end ;
end ; // TF3850.Set_Watchpoint


procedure TF3850.Set_Write_Latency( Value : longint ) ;

begin
    // Intentionally left blank - no latency
end ;


function TF3850.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


procedure TF3850.Wake ;

begin
    _CPU.Blocked := False ;
end ;


function TF3850.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : integer ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := Value ;
end ;


function TF3850.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := ord( Value[ ( ( Size + 7 ) div 8 ) - 1 ] ) ;
end ;


function Convert_Value( S : string ) : integer ;

begin
    Result := 0 ;
    S := Edit( S, -1 ) ;
    if( copy( S, length( S ), 1 ) = '.' ) then
    begin
        S := copy( S, 1, length( S ) - 1 ) ; // A decimal value
    end else
    begin
        S := CVTB( 16, 10, S ) ; // Convert from hexadecimal
    end ;
    try
        Result := strtoint( S ) ;
    except
    end ;
end ;


procedure TF3850.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'IO_PORT_SELECT' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                _CPU.IO_Port_Select := Convert_Value( S ) ;
            except
            end ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ;


procedure TF3850.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TF3850.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TF3850.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TF3850.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function TF3850.Profiler : TProfiler ;

begin
    if( _CPU._Profiler = nil ) then
    begin
        _CPU._Profiler := TF3850_Profiler.Create ;
    end ;
    Result := _CPU._Profiler ;
end ;


function TF3850.Get_Trace : boolean ;

begin
    Result := _CPU._Trace ;
end ;


procedure TF3850.Set_Trace( Value : boolean ) ;

begin
    _CPU._Trace := Value ;
end ;


function TF3850.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Restore_State( Stream ) ;
end ;


function TF3850.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Save_State( Stream ) ;
end ;


procedure TF3850.Show_Status ;

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
    Output( 'F3850/3853' ) ;
    S := 'A=' + Show( _CPU._Accumulator, 1 ) ;
    Output( S ) ;
    S := 'PC=' + Show( _CPU._PC, 2 ) ;
    Output( S ) ;
    for Loop := 0 to 63 do
    begin
        S := 'R' + inttostr( Loop ) + '=' + Show( _CPU._Register[ Loop ], 1 ) ;
        if( Loop = _CPU._ISAR ) then
        begin
            S := S + ' *' ;
        end ;
        Output( S ) ;
    end ;
    S := 'ISAR=' + Show( _CPU._ISAR, 1 ) ;
    Output( S ) ;
    S := 'W=' + Show( _CPU._W, 1 ) ;
    Output( S ) ;
    S := 'PC1=' + Show( _CPU._Stack, 2 ) ;
    Output( S ) ;
    S := 'DC0=' + Show( _CPU._DC0, 2 ) ;
    Output( S ) ;
    S := 'DC1=' + Show( _CPU._DC1, 2 ) ;
    Output( S ) ;
    S := 'H=' + Show( _CPU.H, 2 ) ;
    Output( S ) ;
    S := 'J=' + Show( _CPU.J, 1 ) ;
    Output( S ) ;
    S := 'K=' + Show( _CPU.K, 2 ) ;
    Output( S ) ;
    S := 'L=' + Show( _CPU.L, 2 ) ;
    Output( S ) ;
    S := 'Q=' + Show( _CPU.Q, 2 ) ;
    Output( S ) ;
    S := 'Timer=' + Show( _CPU.Timer, 1 ) ;
    Output( S ) ;
    S := 'IM=' + Show( _CPU.Interrupt_Mode, 1 ) ;
    Output( S ) ;
end ; // TF3850.Show_Status


procedure TF3850.Reset ;

begin
    _CPU.Restart ;
end ;


procedure TF3850.Set_Signal( Name : PChar ; State : boolean ) ;

var Temp : string ;

begin
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    if( Temp = 'INTREQ' ) then
    begin
        if( _CPU.INTREQ <> State ) then
        begin
            if( _Logger <> nil ) then
            begin
                _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
            end ;
            _CPU.INTREQ := State ;
            if( not State ) then
            begin
                _CPU.Pending_Interrupt := True ;
            end ;
        end ;
    end else
    if( Temp = 'EXTRES' ) then
    begin
        if( _CPU.EXTRES <> State ) then
        begin
            if( _Logger <> nil ) then
            begin
                _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
            end ;
            _CPU.EXTRES := State ;
            if( not State ) then
            begin
                _CPU.Restart ;
            end ;
        end ;
    end else
    begin
        exit ;
    end ;
end ; // TF3850.Set_Signal


function TF3850.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

var Temp : string ;

begin
    Result := False ;
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    if( Temp = 'ICB' ) then
    begin
        State := ( _CPU.W and 16 ) > 0 ;
        Result := True ;
    end ;
    if( Temp = 'INTREG' ) then
    begin
        State := _CPU.INTREQ ;
        Result := True ;
    end ;
    if( Temp = 'EXTRES' ) then
    begin
        State := _CPU.EXTRES ;
        Result := True ;
    end ;
end ;


function TF3850.Signal_Count : longint ;

begin
    Result := 3 ;
end ;


function TF3850.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := '' ;
    case Index of
        0 : Temp_Signal_Name := 'ICB' ;
        1 : Temp_Signal_Name := 'EXTRES' ;
        2 : Temp_Signal_Name := 'INTREQ' ;
    end ;
    if( Temp_Signal_Name = '' ) then
    begin
        Result := nil ;
    end else
    begin
        Result := PChar( Temp_Signal_Name ) ;
    end ;
end ;


function TF3850.Signal_Out( Index : longint ) : boolean ;

begin
    if( Index = 0 ) then
    begin
        Result := True ; // ICB
    end else
    begin
        Result := False ;
    end ;
end ;


function TF3850.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := True ;
end ;


function TF3850.Get_State_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( ( Index >= 0 ) and ( Index <= 5 ) ) then
    begin
        case Index of
            State_Port_Input : Temp_Get_State_Name := 'Port input' ;
            State_Port_Output : Temp_Get_State_Name := 'Port output' ;
            State_Interrupt : Temp_Get_State_Name := 'Interrupt' ;
            State_Idle : Temp_Get_State_Name := 'Idle' ;
        end ;
        Result := PChar( Temp_Get_State_Name ) ;
    end ;
end ;


function TF3850.Get_Exception_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Temp_Get_Exception_Description := 'Invalid instruction' ;
                Result := PChar( Temp_Get_Exception_Description ) ;
            end ;
    end ;
end ;


function TF3850.Signal_Index( Name : PChar ) : integer ;

var S : string ;

begin
    S := string( Name ) ;
    if( S = 'INT' ) then
    begin
        Result := 0 ;
    end else
    if( S = 'EXTRES' ) then
    begin
        Result := 1 ;
    end else
    if( S = 'INTREQ' ) then
    begin
        Result := 1 ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TF3850.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TF3850.Set_Logger( Value : TCEF_Logger ) ;

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
    _CPU._Logger := _Logger ;
end ;



end.

