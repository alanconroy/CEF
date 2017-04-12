{$N+}
{
        Program Name : I8008CPU
        Package Name : I8008
        Purpose      : Intel 8008 CPU (CEF component) emulator
        Institution  :
        Date Written : 9-Aug-2007
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

        This unit implements an Intel 8008B CPU emulator as a CEF component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit I8008CPU ;

interface

uses { Borland... }
     Classes, { TList }

     { CEF... }
     _CEF, // TCEF_Logger
     CEF, { TBase_CPU }
     _CEFUtil, // TCEF_Watchpoint

     { I8008... }
     I8008ASM,

     { Other... }
     _DebugIn, { TDebug_Interface }
     CommonUt, { TInteger_List }
     _Streams, // TCOM_Stream
     _UE ; // tUnified_Exception

const SF_Predefined = 1 ; { Predefined symbol }

const I8008Err_Facility = 42 ;
      I8008Err_Success = 0 ;
      I8008Err_Invalid_Component = 1 ;
      I8008Err_Already_Connected = 2 ;
      I8008Err_Component_Not_Found = 3 ;
      I8008Err_Invalid_Stack_Address = 4 ;
      I8008Err_Invalid_Register = 5 ;
      I8008_No_Breakpoint = 6 ;
      I8008_Breakpoint_Exists = 7 ;
      I8008_Invalid_Address = 8 ;
      I8008_Invalid_Operation = 9 ;
      I8008_Invalid_Instruction = 10 ;
      I8008_IO_Trap = 11 ;

type TI8008_Profiler = class( TBase_Profiler )
                         private // Instance data...
                             // Profile data...
                             _Clock : integer ;
                             _Instruction_Count : integer ;
                             Port_Outputs : array[ 0..31 ] of integer ;
                             Port_Inputs : array[ 0..7 ] of integer ;
                             Execution_Addresses : array[ 0..16383 ] of integer ;

                             Instructions : array[ 0..$FF ] of integer ;

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
                     end ; // TI8008_Profiler

type TI8008_CPU = class ;

     TI8008 = class( TBase_Component )
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
                    _CPU : TI8008_CPU ;
                
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

                    procedure Set_Signal( Name : PChar ; State : boolean ) ;
                        override ;

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
            end ; // TI8008


     TI8008_CPU = class( TBase_CPU )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private
            Parent : TI8008 ;

            Interrupts : boolean ; { True if interrupts enabled }
            _Register : array[ 0..7 ] of byte ; { General registers: AFBCDEHL }
            _SP : byte ; { Stack pointer }
            _PC : word ; { Current PC }
            _UI : TUI_Interface ;
            _Speed : integer ; // KHz
            Temp_Register_Name : string ;
            Temp_Register_Description : string ;
            Temp_Log_Trace : string ;
            Temp_Signal_Exception : string ;
            _Halted : boolean ; // True if last instruction was a halt
            _Register_Watchpoints : array[ 0..10 ] of integer ; // Access mode for registers
            _Stack_Watchpoints : array[ 0..13 ] of integer ; // Access mode for stack
            _Profiling : boolean ; // True if profiling
            _Memory_Watchpoints : TCEF_Watchpoint_Manager ;
            _Port_Watchpoints : array[ 0..31 ] of integer ;
            _Breakpoints : TInteger_List ;
            Memory_Data_Latch : byte ; // Last byte sent to us
            _Run_Stream : TCOM_Stream ;
            _Stream_PC_Offset : integer ;
            Interrupt_Instruction : boolean ; // True if instruction from bus due to interrupt
            Blocked : boolean ;
            Stopping : boolean ;
            _Profiler : TI8008_Profiler ;
            _Trace : boolean ; // True to trace execution
            _Stack : array[ 0..6 ] of word ;
            Segments : TInteger_List ;
            _Logger : TCEF_Logger ;
            _RTS : TRun_Time_System ;
            _RTS_Flags : longint ;

        private // Internal utility routines...
            procedure Push( X : Integer ) ;
            function Pop : Integer ;
            procedure Do_Interrupt ;
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
            function Input( Port : integer ) : integer ;
            procedure Clear_Watchpoints ;
            procedure State_Change_Notice( Index : integer ; State : boolean ) ;
            procedure Log_Trace( const Description : string ) ;
            procedure Signal_Exception( const Description : string ;
                Index : longint ) ;
            procedure Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
                Memory, Internal, Port : boolean ) ;
            function Instruction_At( Address : integer ) : string ;

            function Get_HL : integer ;
            procedure Set_HL( Value : integer ) ;
            function Get_PC : word ;
            procedure Set_PC( Value : word ) ;
            function Get_SP : integer ;
            procedure Set_SP( Value : integer ) ;

            function Get_Register( Index : integer ) : integer ;
            procedure Set_Register( Index : integer ; Value : integer ) ;

            property Register_HL : integer
                read Get_HL
                write Set_HL ;
            property PC : word
                read Get_PC
                write Set_PC ;
            property SP : integer
                read Get_SP
                write Set_SP ;
            property Register[ Index : integer ] : integer
                read Get_Register
                write Set_Register ;

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

            function Get_Low_Input_Port( Space : integer ) : int64 ; override ;

            function Get_High_Input_Port( Space : integer ) : int64 ; override ;

            function Get_Low_Output_Port( Space : integer ) : int64 ; override ;

            function Get_High_Output_Port( Space : integer ) : int64 ; override ;

            function Get_Stack_Interface( Space : integer ) : TCEF_Stack_Interface ;
                override ;

            function Segment_Size( Index : integer ) : integer ; override ;

            function Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;
                override ;
    end ; // TI8008_CPU


implementation

uses { Borland... }
     SysUtils, { Allocmem }

     { I8008 }
     I8008Util,

     { Other... }
     _ASCII, { CR }
     CVT, { Cvtb }
     HTML, { TXML_Parser }
     Num1s, { num1 }
     Parse, // TString_Parser
     SStreams, // TCOM_String_Stream
     Standard, // Bit_Values
     UE ; // Create_Simple_UE

type TI8008_Stack_Interface = class( TBase_CEF_Stack_Interface )
                                  private // Instance data
                                      CPU : TI8008_CPU ;

                                  public
                                      constructor Create( C : TI8008_CPU ) ;

                                      procedure Terminate ; override ;

                                      function Low_Bound : int64 ; override ;

                                      function High_Bound : int64 ; override ;

                                      function Item_Size : longint ; override ;

                                      function Value( Index : int64 ) : int64 ;
                                          override ;
                              end ;

constructor TI8008_Stack_Interface.Create( C : TI8008_CPU ) ;

begin
    inherited Create ;

    CPU := C ;
end ;


procedure TI8008_Stack_Interface.Terminate ;

begin
    Free ;
end ;


function TI8008_Stack_Interface.Low_Bound : int64 ;

begin
    Result := 0 ;
end ;


function TI8008_Stack_Interface.High_Bound : int64 ;

begin
    Result := 6 ;
end ;


function TI8008_Stack_Interface.Item_Size : longint ;

begin
    Result := 14 ;
end ;


function TI8008_Stack_Interface.Value( Index : int64 ) : int64 ;

begin
    if( ( Index < Low_Bound ) or ( Index > High_Bound ) ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := CPU._Stack[ Index ] ;
    end ;
end ;



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
        0..1 : Result := 'HLT' ;
        2 : Result := 'RLC' ;
        4 : Result := 'ADI' ;
        8 : Result := 'INB' ;
        $A : Result := 'RRC' ;
        $C : Result := 'ACI' ;
        $10 : Result := 'INC' ;
        $11 : Result := 'DCC' ;
        $12 : Result := 'RAL' ;
        $14 : Result := 'SUI' ;
        $18 : Result := 'IND' ;
        $19 : Result := 'DCD' ;
        $1A : Result := 'RAR' ;
        $1C : Result := 'SBI' ;
        $20 : Result := 'INE' ;
        $21 : Result := 'DCE' ;
        $24 : Result := 'NDI' ;
        $28 : Result := 'INH' ;
        $29 : Result := 'DCH' ;
        $2C : Result := 'XRI' ;
        $30 : Result := 'INL' ;
        $31 : Result := 'DCL' ;
        $34 : Result := 'ORI' ;
        $3C : Result := 'CPI' ;
        $80..$87 : Result := 'ADr' ;
        $88..$8F : Result := 'ACr' ;
        $90..$97 : Result := 'SUr' ;
        $98..$9F : Result := 'SBr' ;
        $A0..$A7 : Result := 'NDr' ;
        $A8..$AF : Result := 'XRr' ;
        $B0..$B7 : Result := 'ORr' ;
        $B8..$BF : Result := 'CPr' ;
        $C0..$CF : Result := 'Lrr' ;
        $FF : Result := 'HLT' ;
        else
            case I and $C7 of
                5 : Result := 'RST' ;
                6 : Result := 'LrI' ;
                7 : Result := 'RET' ;
                $44 : Result := 'JMP' ;
                $46 : Result := 'CAL' ;
                else
                    case I and $E7 of
                        3 : Result := 'RFc' ;
                        $2C : Result := 'RTc' ;
                        $40 : Result := 'JFc' ;
                        $42 : Result := 'CFc' ;
                        $60 : Result := 'JTc' ;
                        $62 : Result := 'CTc' ;
                        else
                            if( ( I and $F1 ) = $41 ) then
                            begin
                                Result := 'INP' ;
                            end else
                            if( ( I and $C1 ) = $41 ) then
                            begin
                                Result := 'OUT' ;
                            end ;
                    end ;
            end ;
    end ;
end ; // Instruction_Name


// TI8008_Profiler methods...

// API...

procedure TI8008_Profiler.Generate_Report ;

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
    for Loop := 0 to 31 do
    begin
        if( Port_Outputs[ Loop ] <> 0 ) then
        begin
            Outputs.Add( cvtb( 10, Base, inttostr( Loop ) ) + ': ' + inttostr( Port_Outputs[ Loop ] ) ) ;
        end ;
    end ;

    // Generate port input report
    for Loop := 0 to 7 do
    begin
        if( Port_Inputs[ Loop ] <> 0 ) then
        begin
            Inputs.Add( cvtb( 10, Base, inttostr( Loop ) ) + ': ' + inttostr( Port_Inputs[ Loop ] ) ) ;
        end ;
    end ;

    // Generate execution address report
    for Loop := 0 to 16383 do
    begin
        if( Execution_Addresses[ Loop ] <> 0 ) then
        begin
            Addresses.Add( cvtb( 10, Base, inttostr( Loop ) ) + ': ' + inttostr( Execution_Addresses[ Loop ] ) ) ;
        end ;
    end ;

    // Generate instruction report
    for Loop := 0 to $FF do
    begin
        if( Instructions[ Loop ] <> 0 ) then
        begin
            Instruction_Lines.Add( Instruction_Name( Loop ) + ': ' + inttostr( Instructions[ Loop ] ) ) ;
        end ;
    end ;
    Instruction_Lines.Sort ;
end ; // TI8008_Profiler.Generate_Report


procedure TI8008_Profiler.Increment( Domain, Index : integer ) ;

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


procedure TI8008_Profiler.Increment_Clock( Count : integer ) ;

begin
    Dirty := True ;
    _Clock := _Clock + Count ;
end ;


// Overrides...

procedure TI8008_Profiler.Clear( Domain : integer ) ;

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
end ; // TI8008_Profiler.Clear


function TI8008_Profiler.Domain_Name( Index : integer ) : PChar ;

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


function TI8008_Profiler.Report_Line( Domain, Index : integer ) : PChar ;

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
end ; // TI8008_Profiler.Report_Line



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

begin
    A := '' ;
    if( ( Value and 1 ) = 1 ) then
    begin
        A := 'C ' ;
    end else
    begin
        A := 'NC ' ;
    end ;
    if( ( Value and 2 ) = 2 ) then
    begin
        A := A + 'Z ' ;
    end else
    begin
        A := A + 'NZ ' ;
    end ;
    if ( Value and 4 ) = 4 then
    begin
        A := A + 'S ' ;
    end else
    begin
        A := A + 'NS ' ;
    end ;
    if( ( Value and 8 ) = 8 ) then
    begin
        A := A + 'P' ;
    end else
    begin
        A := A + 'NP' ;
    end ;
    Get_Mask := A ; { Return value }
end ; // Get_Mask


// TI8008_CPU methods...

// Constructors and destructors...

constructor TI8008_CPU.Create ;

begin
    inherited Create ;

    _Memory_Watchpoints := Get_Watchpoint_Manager ;
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
    _Breakpoints := TInteger_List.Create ;
    _Speed := 800 ; // 800 KHz
    Base := Default_Base ;
    Segments := TInteger_List.Create ;
    Restart ; // Do power-on reset
end ;


destructor TI8008_CPU.Destroy ;

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

procedure TI8008_CPU.Push( X : Integer ) ;

begin
    Sp := ( _Sp - 1 ) and 7 ;
    if( SP > 6 ) then
    begin
        SP := 6 ;
    end ;
    _Stack[ SP ] := X and 16383 ;
end ;


function TI8008_CPU.Pop : Integer ;

begin
    Result := _Stack[ SP ] ;
    SP := _SP + 1 ;
    if( SP > 6 ) then
    begin
        SP := 0 ;
    end ;
end ;


procedure TI8008_CPU.State_Change_Notice( Index : integer ; State : boolean ) ;

begin
    _UI.State_Change_Notice( Parent, Index, State ) ;

    case Index of
        State_Interrupt : Log_Trace( 'Process interrupt' ) ;
    end ;
end ;


procedure TI8008_CPU.Log_Trace( const Description : string ) ;

begin
    if( _Trace ) then
    begin
        Temp_Log_Trace := Description ;
        _UI.Log_Trace( Parent, PChar( Temp_Log_Trace ) ) ;
    end ;
end ;


procedure TI8008_CPU.Signal_Exception( const Description : string ; Index : longint ) ;

begin
    Temp_Signal_Exception := Description ;
    _UI.Signal_Exception( Parent, PChar( Temp_Signal_Exception ), Index ) ;
end ;


procedure TI8008_CPU.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
    Memory, Internal, Port : boolean ) ;

begin
    _UI.Watchpoint_Notice( Address, Access, Tag, Parent, Memory, Internal, Port ) ;
end ;


function TI8008_CPU.Instruction_At( Address : integer ) : string ;

var Stream : TCOM_String_Stream ;

begin
    Stream := TCOM_String_Stream.Create ;
    Disassemble( Address, Base, 1, Stream ) ;
    Result := string( Stream.As_String ) ;
    Stream.Detach ;
end ;


procedure TI8008_CPU.Do_Interrupt ;

begin
    if( not Interrupts ) then // Interrupts disabled
    begin
        exit ;
    end ;
    _Halted := False ;
    Interrupt_Instruction := True ;
    State_Change_Notice( State_Interrupt, True ) ;
end ;


procedure TI8008_CPU.Do_Wait ;

begin
    try
        _UI.Idle( Parent ) ;
    except
    end ;
end ;


function TI8008_CPU.Bus_Examine( Address : Integer ) : Char ; { Return data at memory Address }

var Component : TComponent ;
    Loop, Size : integer ;
    UEC : TUnified_Exception ;

begin
    for Loop := 0 to Parent.Inputs.Count - 1 do
    begin
        Size := 1 ;
        Component := TComponent( Parent.Inputs[ Loop ] ) ;
        UEC := Component.Examine( Address, Size, @Result, True ) ;
        if( UEC = nil ) then
        begin
            exit ;
        end ;
    end ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Size := 1 ;
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        UEC := Component.Examine( Address, Size, @Result, True ) ;
        if( UEC = nil ) then
        begin
            exit ;
        end ;
    end ;
    Result := #255 ; // No component responded to this address
end ;


function TI8008_CPU.Bus_Read( Address : Integer ; IO_Type : longint ) : Char ; { Return data at Address }

var Component : TComponent ;
    Loop : integer ;

begin
    Memory_Data_Latch := 255 ; // Default if nothing responds
    try
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Address, 1, IO_Type ) ) then
            begin
            end ;
        end ;
    finally
        Bus_Read := char( Memory_Data_Latch ) ;
    end ;
end ;


function TI8008_CPU.ByteRead( Address : Integer ) : Char ; { Return data at Address }

begin
    Result := Bus_Read( Address and 16383, IO_Type_Memory ) ;
end ;


function TI8008_CPU.Byte_Read( Address : integer ) : integer ;
{ Read a byte from the specified address }

begin
    Byte_Read := ord( ByteRead( Address ) ) ;
end ;


function TI8008_CPU.Word_Read( Address : integer ) : integer ;
{ Read a word from the specified address }

var X : integer ;

begin
    X := Byte_Read( Address ) ;
    X := X or ( swap( Byte_Read( Address + 1 ) ) and $FF00 ) ;
    Word_Read := X ;
end ;


procedure TI8008_CPU.ByteWrite( Address, Value : Integer ) ; { Write to memory }

var Component : TComponent ;
    Loop : integer ;
    UEC : TUnified_Exception ;

begin
    Address := Address and 16383 ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        UEC := Component.Write( Address, Value, 1, IO_Type_Memory ) ;
        if( UEC <> nil ) then
        begin
            exit ;
        end ;
    end ;
end ;


procedure TI8008_CPU.Increment_Clock( Count : integer ) ;

var R : extended ;
    W : int64 ; // Amount of time to wait (in picoseconds)

begin
    if( _Profiling ) then
    begin
        TI8008_Profiler( Parent.Profiler ).Increment_Clock( Count ) ;
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


procedure TI8008_CPU.Clear_Watchpoints ;

begin
    fillchar( _Port_Watchpoints, sizeof( _Port_Watchpoints ), 0 ) ;
end ;


function TI8008_CPU.Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;

begin
    Result := TI8008_Assembler.Create ;
    TI8008_Assembler( Result ).CPU := self ;
    TI8008_Assembler( Result ).Segments := Segments ;
    Result.Initialize( Master ) ;
    TI8008_Assembler( Result ).Base := Base ;
end ;


function TI8008_CPU.Cancel_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

var Index : integer ;

begin
    Result := Parent.Set_Error( I8008_No_Breakpoint ) ; // Assume failure
    if( ( Address < 0 ) or ( Address > 16383 ) ) then
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


function TI8008_CPU.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
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
            Result := Parent.Set_Error( I8008_Invalid_Address ) ;
            exit ;
        end ;
        _Port_Watchpoints[ Address ] :=
            _Port_Watchpoints[ Address ] and not( Access ) ;
    end ; // if( Memory )
end ; // TI8008_CPU.Clear_Watchpoint


function TI8008_CPU.Disassemble( Address : int64 ; Base, Size : longint ;
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
            Er := Copy( 'ABCDEHLM', A, 1 ) ;
        end ;


        function Ef( X, Y : Integer ) : String ;
        { Extract flag from X ((X div Y) and 3) and return string }

        var A : Integer ;

        begin
            A := ( ( X div Y ) and 3 ) + 1 ;
            Ef := Copy( 'CZSP', A, 1 ) ;
        end ;


    var AA : string ;

    begin
        Tpc := Address ;
        Instruction := '' ;
        DText := '' ;
        A := Fetch ;
        case A of
            0..1, 255 :
                begin
                    _Disassemble := 'HLT' ;
                    exit ;
                end ;
            2 :
                begin
                    _Disassemble := 'RLC' ;
                    exit ;
                end ;
            4 :
                begin
                    Aa := Cvis( Fetch, 2 ) ;
                    _Disassemble := 'ADI ' + AA ;
                    exit ;
                end ;
            8 :
                begin
                    _Disassemble := 'INB' ;
                    exit ;
                end ;
            9 :
                begin
                    _Disassemble := 'DCB' ;
                    exit ;
                end ;
            $A :
                begin
                    _Disassemble := 'RRC' ;
                    exit ;
                end ;
            $C :
                begin
                    Aa := Cvis( Fetch, 2 ) ;
                    _Disassemble := 'ACI ' + AA ;
                    exit ;
                end ;
            $10 :
                begin
                    _Disassemble := 'INC' ;
                    exit ;
                end ;
            $11 :
                begin
                    _Disassemble := 'DCC' ;
                    exit ;
                end ;
            $12 :
                begin
                    _Disassemble := 'RAL' ;
                    exit ;
                end ;
            $14 :
                begin
                    Aa := Cvis( Fetch, 2 ) ;
                    _Disassemble := 'SUI ' + AA ;
                    exit ;
                end ;
            $18 :
                begin
                    _Disassemble := 'IND' ;
                    exit ;
                end ;
            $19 :
                begin
                    _Disassemble := 'DCD' ;
                    exit ;
                end ;
            $1A :
                begin
                    _Disassemble := 'RAR' ;
                    exit ;
                end ;
            $1C :
                begin
                    Aa := Cvis( Fetch, 2 ) ;
                    _Disassemble := 'SBI ' + AA ;
                    exit ;
                end ;
            $20 :
                begin
                    _Disassemble := 'INE' ;
                    exit ;
                end ;
            $21 :
                begin
                    _Disassemble := 'DCE' ;
                    exit ;
                end ;
            $24 :
                begin
                    Aa := Cvis( Fetch, 2 ) ;
                    _Disassemble := 'NDI ' + AA ;
                    exit ;
                end ;
            $28 :
                begin
                    _Disassemble := 'INH' ;
                    exit ;
                end ;
            $29 :
                begin
                    _Disassemble := 'DCH' ;
                    exit ;
                end ;
            $2C :
                begin
                    Aa := Cvis( Fetch, 2 ) ;
                    _Disassemble := 'XRI ' + AA ;
                    exit ;
                end ;
            $30 :
                begin
                    _Disassemble := 'INL' ;
                    exit ;
                end ;
            $31 :
                begin
                    _Disassemble := 'DCL' ;
                    exit ;
                end ;
            $34 :
                begin
                    Aa := Cvis( Fetch, 2 ) ;
                    _Disassemble := 'ORI ' + AA ;
                    exit ;
                end ;
            $3C :
                begin
                    Aa := Cvis( Fetch, 2 ) ;
                    _Disassemble := 'CPI ' + AA ;
                    exit ;
                end ;
            $80..$87 :
                begin
                    _Disassemble := 'AD' + Er( A, 1 ) ;
                    exit ;
                end ;
            $88..$8F :
                begin
                    _Disassemble := 'AC' + Er( A, 1 ) ;
                    exit ;
                end ;
            $90..$97 :
                begin
                    _Disassemble := 'SU' + Er( A, 1 ) ;
                    exit ;
                end ;
            $98..$9F :
                begin
                    _Disassemble := 'SB' + Er( A, 1 ) ;
                    exit ;
                end ;
            $A0..$A7 :
                begin
                    _Disassemble := 'ND' + Er( A, 1 ) ;
                    exit ;
                end ;
            $A8..$AF :
                begin
                    _Disassemble := 'XR' + Er( A, 1 ) ;
                    exit ;
                end ;
            $B0..$B7 :
                begin
                    _Disassemble := 'OR' + Er( A, 1 ) ;
                    exit ;
                end ;
            $B8..$BF :
                begin
                    _Disassemble := 'CP' + Er( A, 1 ) ;
                    exit ;
                end ;
            $C0..$CF : // Lrr
                begin
                    _Disassemble := 'L' + Er( A, 8 ) + Er( A, 1 ) ;
                    exit ;
                end ;
            else
                case A and $C7 of
                    5 : // RST
                        begin
                            _Disassemble := 'RST ' + inttostr( ( A shr 3 ) and 7 ) ;
                            exit ;
                        end ;
                    6 : // LrI
                        begin
                            Aa := Cvis( Fetch, 2 ) ;
                            _Disassemble := 'L' + Er( A, 8 ) + 'I ' + AA ;
                            exit ;
                        end ;
                    7 :
                        begin
                            _Disassemble := 'RET' ;
                            exit ;
                        end ;
                    $44 :
                        begin
                            _Disassemble := 'JMP ' +
                                Cvis( trunc( Cvtif( FetchWord ) ) and 16383, 4 ) ;
                            exit ;
                        end ;
                    $46 :
                        begin
                            _Disassemble := 'CAL ' +
                                Cvis( trunc( Cvtif( FetchWord ) ) and 16383, 4 ) ;
                            exit ;
                        end ;
                    else
                        case A and $E7 of
                            3 :
                                begin
                                    _Disassemble := 'RF' + Ef( A, 8 ) ;
                                    exit ;
                                end ;
                            $2C :
                                begin
                                    _Disassemble := 'RT' + Ef( A, 8 ) ;
                                    exit ;
                                end ;
                            $40 :
                                begin
                                    _Disassemble := 'JF' + Ef( A, 8 ) + ' ' +
                                        Cvis( trunc( Cvtif( FetchWord ) ) and 16383, 4 ) ;
                                    exit ;
                                end ;
                            $42 :
                                begin
                                    _Disassemble := 'CF' + Ef( A, 8 ) + ' ' +
                                        Cvis( trunc( Cvtif( FetchWord ) ) and 16383, 4 ) ;
                                    exit ;
                                end ;
                            $60 :
                                begin
                                    _Disassemble := 'JT' + Ef( A, 8 ) + ' ' +
                                        Cvis( trunc( Cvtif( FetchWord ) ) and 16383, 4 ) ;
                                    exit ;
                                end ;
                            $62 :
                                begin
                                    _Disassemble := 'CT' + Ef( A, 8 ) + ' ' +
                                        Cvis( trunc( Cvtif( FetchWord ) ) and 16383, 4 ) ;
                                    exit ;
                                end ;
                            else
                                if( ( A and $F1 ) = $41 ) then
                                begin
                                    _Disassemble := 'INP ' + inttostr( ( A div 2 ) and 7 ) ;
                                    exit ;
                                end else
                                if( ( A and $C1 ) = $41 ) then
                                begin
                                    _Disassemble := 'OUT ' + inttostr( ( A div 2 ) and 7 ) ;
                                    exit ;
                                end ;
                        end ;
                end ;
        end ; // case A of
Ee:
        _Disassemble := 'DB ' + Cvis( A, 2 ) ;
    end ; { TI8008_CPU.Disassemble._Disassemble }

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
end ; { TI8008_CPU.Disassemble }


function TI8008_CPU.Get_Clock_Speed : longint ;

begin
    Result := _Speed ;
end ;


procedure TI8008_CPU.Halt ;

begin
    _Halted := True ;
end ;


function TI8008_CPU.Halted : boolean ;

begin
    Result := _Halted ;
end ;


procedure TI8008_CPU.Run_From_Stream( Stream : TCOM_Stream ) ;

begin
    _Run_Stream := Stream ;
    Execute( False, False ) ;
    _Run_Stream := nil ;
end ;


procedure TI8008_CPU.Run ;

begin
    _Halted := False ;
    Execute( False, False ) ;
end ;


procedure TI8008_CPU.Execute( Single_Step, Into : boolean ) ;

var Flags_Changed : boolean ; // Set to true if any flags change value
    Flags_Read : boolean ; // True if any flags are examined

    procedure _Set_Flag( Value : integer ) ;

    var Old : integer ;

    begin
        Old := _Register[ 1 ] ;
        _Register[ 1 ] := _Register[ 1 ] or Value ;
        if( Old <> _Register[ 1 ] ) then
        begin
            Flags_Changed := True ;
        end ;
    end ;


    procedure _Reset_Flag( Value : integer ) ;

    var Old : integer ;

    begin
        Old := _Register[ 1 ] ;
        _Register[ 1 ] := _Register[ 1 ] and ( not Value ) ;
        if( Old <> _Register[ 1 ] ) then
        begin
            Flags_Changed := True ;
        end ;
    end ;


    function _Is_Flag_Set( Value : integer ) : boolean ;

    begin
        Result := ( ( Register[ 1 ] and Value ) <> 0 ) ;
        Flags_Read := True ;
    end ;


    procedure Set_C ; {Set carry flag}

    begin
        _Set_Flag( 1 ) ;
    end ;


    procedure Reset_C ;

    begin
        _Reset_Flag( 1 ) ;
    end ;


    function Is_C_Set : boolean ;

    begin
       Result :=_Is_Flag_Set( 1 ) ;
    end ;


    procedure Set_Z ; { Set zero flag }

    begin
        _Set_Flag( 2 ) ;
    end ;


    procedure Reset_Z ;

    begin
        _Reset_Flag( 2 ) ;
    end ;


    function Is_Z_Set : boolean ;

    begin
       Result :=_Is_Flag_Set( 2 ) ;
    end ;


    procedure Set_S ;

    begin
        _Set_Flag( 4 ) ;
    end ;


    procedure Reset_S ;

    begin
        _Reset_Flag( 4 ) ;
    end ;


    function Is_S_Set : boolean ;

    begin
        Result :=_Is_Flag_Set( 4 ) ;
    end ;


    procedure Set_P ;

    begin
        _Set_Flag( 8 ) ;
    end ;


    procedure Reset_P ;

    begin
        _Reset_Flag( 8 ) ;
    end ;


    function Is_P_Set : boolean ;

    begin
        Result :=_Is_Flag_Set( 8 ) ;
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
        end ;
    end ; // .Fetch


    function Fetch_Word : integer ; { Fetch a 2-byte word }

    var X : integer ;

    begin
        X := Fetch ;
        X := X or swap( Fetch ) ;
        Fetch_Word := X ;
    end ;


    function Extract_Register( X, Y : integer ) : integer ;
    { Extract register from X ((X div Y) and 7) and return appropriate byte }

    var A : Integer ;

    begin
        A := ( ( X div Y ) and 7 ) ;
        if( A > 0 ) then
        begin
            A := A + 1 ;
        end ;
        if( A > 7 ) then // M
        begin
            Extract_Register := Byte_Read( Register_HL ) ;
        end else
        begin
            Extract_Register := _Register[ A ] ;
            if( _Run_Stream = nil ) then
            begin
                if( ( _Register_Watchpoints[ A ] and Access_Read ) <> 0 ) then
                begin
                    Watchpoint_Notice( A, Access_Read, 0, False, True, False ) ;
                end ;
            end ;
        end ;
    end ; { Extract_Register }


    procedure Flags( X : Integer ) ; { Adjust P, S, and Z flags }

    var A, B, C : Integer ;

    begin
        _Register[ 1 ] := _Register[ 1 ] and 1 ; // Preserve carry
        Flags_Changed := True ;
        X := X and 255 ;
        if( X = 0 ) then
        begin
            Set_Z ;
        end ;
        if( X > 127 ) then
        begin
            Set_S ;
        end ;

        // Handle Parity flag...
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
        if( ( A and 1 ) = 0 ) then // Even
        begin
            Set_P ; // Even parity
        end ;
    end ; // .Flags


    procedure Add( A, B : Integer ) ; { Add A+B to accumulator }

    begin
        _Register[ 1 ] := 0 ;
        Flags_Changed := True ;
        A := A + _Register[ 0 ] + B ;
        if( A > 255 ) then
        begin
            Set_C ;
            A := A and 255 ;
        end ;
        _Register[ 0 ] := A and 255 ;
        Flags( A )
    end ; // Add


    procedure Sub( A, B : Integer ) ;
    { Subtract A+B from accumulator. }

    begin
        A := -( A + B ) and 255 ;
        if( A = 0 ) then if ( B = 1 ) then
        begin
            A := 256 ; { Subtraction of 0 }
        end else
        begin
            Reset_C ;
            Flags( _Register[ 0 ] ) ;
            exit ;
        end ;
        A := A + _Register[ 0 ] ;
        if( A > 255 ) then // Underflow
        begin
            Reset_C ;
        end else
        begin
            Set_C ;
        end ;
        _Register[ 0 ] := A and 255 ;
        Flags( A ) ;
    end ; // Sub


    procedure Logical_Flags ; {Reset C}

    begin
        Reset_C ;
    end ;


    procedure Ccmp( A : Integer ) ; {Compare A with accumulator}

    begin
        A := - A and 255 ;
        if A = 0 then {Compare with 0}
        begin
            Register[ 1 ] := _Register[ 1 ] and ( not 4352 ) ;
            Flags( _Register[ 1 ] ) ;
            Exit
        end ;
        _Register[ 1 ] := _Register[ 1 ] or 4352 ;
        if(
            ( ( A and 127 ) + ( _Register[ 1 ] and 127 ) > 127 )
            xor
            ( ( A and 255 ) + ( _Register[ 1 ] and 255 ) > 255 )
          ) then
        begin
            Set_P ;
        end else
        begin
            Reset_P ;
        end ;
        A := A + _Register[ 1 ] ;
        if A > 255 then
        begin
            Reset_C ;
            A := A and 255
        end ;
        Flags( A ) ;
    end ;


    function Eff( X, Y : Integer ) : boolean ;
    { Extract flag from X ((X div Y) and 3) and return boolean }

    var A, Mask : Integer ;

    begin
        A := ( ( X div Y ) and 3 ) ;
        case A of
            0 : Mask := 1 ;
            1 : Mask := 2 ;
            2 : Mask := 4 ;
            else Mask := 8 ;
        end ;
        Eff := ( ( _Register[ 1 ] and Mask ) <> 0 ) ;
    end ;


label Unknown_OpCode ;

var A, A1, B, C : integer ;
    Did_Interrupt_Fetch : boolean ;
    Count : integer ;
    Nest_Level : integer ;
    Original_PC : longint ;
    
begin // TI8008_CPU.Execute
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
                    Watchpoint_Notice( 2, Access_Write, 0, False, True, False ) ;
                end else
                if( ( _Register_Watchpoints[ 7 ] and Access_Write ) <> 0 ) then
                begin
                    Watchpoint_Notice( 7, Access_Write, 0, False, True, False ) ;
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
                    Watchpoint_Notice( 2, Access_Read, 0, False, True, False ) ;
                end else
                if( ( _Register_Watchpoints[ 7 ] and Access_Read ) <> 0 ) then
                begin
                    Watchpoint_Notice( 7, Access_Read, 0, False, True, False ) ;
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
            TI8008_Profiler( Parent.Profiler ).Increment( Domain_Execution_Addresses, PC ) ;
            TI8008_Profiler( Parent.Profiler ).Increment( Domain_Other, Domain_Other_Instruction_Count ) ;
        end ;
        Original_PC := PC ;
        Log_Trace( 'Executing instruction at address ' + cvtb( 10, Base, inttostr( PC ) ) + ': ' + Instruction_At( PC ) ) ;
        A := Fetch ;
        if( _Logger <> nil ) then
        begin
            _Logger.Update( Parent, Original_PC, A ) ;
        end ;
        if( Interrupt_Instruction ) then
        begin
            Did_Interrupt_Fetch := True ;
        end ;

        if( _Profiling ) then
        begin
            TI8008_Profiler( Parent.Profiler ).Increment( Domain_Instructions, A ) ;
        end ;

        case A of
            0..1, 255 : // HLT
                begin
                    Increment_Clock( 4 ) ;
                    _Halted := True ;
                    continue ;
                end ;
            2 : // RLC
                begin
                    Increment_Clock( 5 ) ;
                    C := Register[ 0 ] shr 7 ;
                    Register[ 0 ] := ( _Register[ 0 ] shl 1 ) ;
                    Register[ 1 ] := ( _Register[ 1 ] and $FE ) or C ; // Set carry flag
                    continue ;
                end ;
            4 : // ADI
                begin
                    Increment_Clock( 8 ) ;
                    A := Fetch ; // Get data
                    Add( A, 0 ) ;
                    continue ;
                end ;
            8 : // INB
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 2 ] + 1 ;
                    Register[ 2 ] := C ;
                    Flags( C ) ;
                    continue ;
                end ;
            9 : // DCB
                begin
                    Increment_Clock( 5 ) ;
                    Register[ 2 ] := _Register[ 2 ] - 1 ;
                    Flags( _Register[ 2 ] ) ;
                    continue ;
                end ;
            $A : // RRC
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 0 ] and 1 ;
                    Register[ 0 ] := ( _Register[ 0 ] shr 1 ) ;
                    Register[ 1 ] := ( _Register[ 1 ] and $FE ) or C ; // Set carry flag
                    continue ;
                end ;
            $C : // ACI
                begin
                    Increment_Clock( 8 ) ;
                    A := Fetch ; // Get data
                    Add( A, _Register[ 1 ] and 1 ) ;
                    continue ;
                end ;
            $10 : // INC
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 3 ] + 1 ;
                    Register[ 3 ] := C ;
                    Flags( C ) ;
                    continue ;
                end ;
            $11 : // DCC
                begin
                    Increment_Clock( 5 ) ;
                    Register[ 3 ] := _Register[ 3 ] - 1 ;
                    Flags( _Register[ 3 ] ) ;
                    continue ;
                end ;
            $12 : // RAL
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 0 ] shr 7 ;
                    Register[ 0 ] := ( _Register[ 0 ] shl 1 ) or ( Register[ 1 ] and 1 ) ;
                    Register[ 1 ] := ( _Register[ 1 ] and $FE ) or C ; // Set carry flag
                    continue ;
                end ;
            $14 : // SUI
                begin
                    Increment_Clock( 8 ) ;
                    A := Fetch ; // Get data
                    Sub( A, 0 ) ;
                    continue ;
                end ;
            $18 : // IND
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 4 ] + 1 ;
                    Register[ 4 ] := C ;
                    Flags( C ) ;
                    continue ;
                end ;
            $19 : // DCD
                begin
                    Increment_Clock( 5 ) ;
                    Register[ 4 ] := _Register[ 4 ] - 1 ;
                    Flags( _Register[ 4 ] ) ;
                    continue ;
                end ;
            $1A : // RAR
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 0 ] and 1 ;
                    Register[ 0 ] := ( _Register[ 0 ] shr 1 ) or ( Register[ 1 ] shl 7 ) ;
                    Register[ 1 ] := ( _Register[ 1 ] and $FE ) or C ; // Set carry flag
                    continue ;
                end ;
            $1C : // SBI
                begin
                    Increment_Clock( 8 ) ;
                    A := Fetch ; // Get data
                    Sub( A, _Register[ 1 ] and 1 ) ;
                    continue ;
                end ;
            $20 : // INE
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 5 ] + 1 ;
                    Register[ 5 ] := C ;
                    Flags( C ) ;
                    continue ;
                end ;
            $21 : // DCE
                begin
                    Increment_Clock( 5 ) ;
                    Register[ 5 ] := _Register[ 5 ] - 1 ;
                    Flags( _Register[ 5 ] ) ;
                    continue ;
                end ;
            $24 : // NDI
                begin
                    Increment_Clock( 8 ) ;
                    A := Fetch ; // Get data
                    Register[ 0 ] := _Register[ 0 ] and A ;
                    continue ;
                end ;
            $28 : // INH
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 6 ] + 1 ;
                    Register[ 6 ] := C ;
                    Flags( C ) ;
                    continue ;
                end ;
            $29 : // DCH
                begin
                    Increment_Clock( 5 ) ;
                    Register[ 6 ] := _Register[ 6 ] - 1 ;
                    Flags( _Register[ 6 ] ) ;
                    continue ;
                end ;
            $2C : // XRI
                begin
                    Increment_Clock( 8 ) ;
                    A := Fetch ; // Get data
                    Register[ 0 ] := _Register[ 0 ] xor A ;
                    continue ;
                end ;
            $30 : // INL
                begin
                    Increment_Clock( 5 ) ;
                    C := _Register[ 7 ] + 1 ;
                    Register[ 7 ] := C ;
                    Flags( C ) ;
                    continue ;
                end ;
            $31 : // DCL
                begin
                    Increment_Clock( 5 ) ;
                    Register[ 7 ] := _Register[ 7 ] - 1 ;
                    Flags( _Register[ 7 ] ) ;
                    continue ;
                end ;
            $34 : // ORI
                begin
                    Increment_Clock( 8 ) ;
                    A := Fetch ; // Get data
                    Register[ 0 ] := _Register[ 0 ] or A ;
                    continue ;
                end ;
            $3C : // CPI
                begin
                    Increment_Clock( 8 ) ;
                    A := Fetch ; // Get data
                    if( A = _Register[ 0 ] ) then
                    begin
                        Set_Z ;
                    end else
                    begin
                        Reset_Z ;
                    end ;
                    if( A > _Register[ 0 ] ) then
                    begin
                        Set_C ;
                    end else
                    begin
                        Reset_C ;
                    end ;
                    continue ;
                end ;
            $80..$87 : // ADr
                begin
                    C := A and 7 ; // Get register
                    A := Extract_Register( A, 1 ) ;
                    if( C = 7 ) then // Memory
                    begin
                        Increment_Clock( 8 ) ;
                    end else
                    begin
                        Increment_Clock( 5 ) ;
                    end ;
                    A := A + _Register[ 0 ] ;
                    Register[ 0 ] := A and 255 ;
                    Flags( A ) ;
                    if( A > 255 ) then // Carry
                    begin
                        Set_C ;
                    end else
                    begin
                        Reset_C ;
                    end ;
                    continue ;
                end ;
            $88..$8F : // ACr
                begin
                    C := A and 7 ; // Get register
                    A := Extract_Register( A, 1 ) ;
                    if( C = 7 ) then // Memory
                    begin
                        Increment_Clock( 8 ) ;
                    end else
                    begin
                        Increment_Clock( 5 ) ;
                    end ;
                    A := A + _Register[ 0 ] + ( _Register[ 1 ] and 1 ) ;
                    Register[ 0 ] := A and 255 ;
                    Flags( A ) ;
                    if( A > 255 ) then // Carry
                    begin
                        Set_C ;
                    end else
                    begin
                        Reset_C ;
                    end ;
                    continue ;
                end ;
            $90..$97 : // SUr
                begin
                    C := A and 7 ; // Get register
                    A := Extract_Register( A, 1 ) ;
                    if( C = 7 ) then // M
                    begin
                        Increment_Clock( 8 ) ;
                    end else
                    begin
                        Increment_Clock( 5 ) ;
                    end ;
                    Sub( A, 0 ) ;
                    continue ;
                end ;
            $98..$9F : // SBr
                begin
                    C := A and 7 ; // Get register
                    A := Extract_Register( A, 1 ) ;
                    if( C = 8 ) then // M
                    begin
                        Increment_Clock( 8 ) ;
                    end else
                    begin
                        Increment_Clock( 5 ) ;
                    end ;
                    Sub( A, _Register[ 1 ] and 1 ) ;
                    continue ;
                end ;
            $A0..$A7 : // NDr
                begin
                    C := A and 7 ; // Get register
                    A := Extract_Register( A, 1 ) ;
                    if( C = 7 ) then // M
                    begin
                        Increment_Clock( 8 ) ;
                    end else
                    begin
                        Increment_Clock( 5 ) ;
                    end ;
                    Register[ 0 ] := _Register[ 0 ] and A ;
                    continue ;
                end ;
            $A8..$AF : // XRr
                begin
                    C := A and 7 ; // Get register
                    A := Extract_Register( A, 1 ) ;
                    if( C = 7 ) then // M
                    begin
                        Increment_Clock( 8 ) ;
                    end else
                    begin
                        Increment_Clock( 5 ) ;
                    end ;
                    Register[ 0 ] := _Register[ 0 ] xor A ;
                    continue ;
                end ;
            $B0..$B7 : // ORr
                begin
                    C := A and 7 ; // Get register
                    A := Extract_Register( A, 1 ) ;
                    if( C = 7 ) then // M
                    begin
                        Increment_Clock( 8 ) ;
                    end else
                    begin
                        Increment_Clock( 5 ) ;
                    end ;
                    Register[ 0 ] := _Register[ 0 ] or A ;
                    continue ;
                end ;
            $B8..$BF : // CPr
                begin
                    C := A and 7 ; // Get register
                    A := Extract_Register( A, 1 ) ;
                    if( C = 7 ) then // M
                    begin
                        Increment_Clock( 8 ) ;
                    end else
                    begin
                        Increment_Clock( 5 ) ;
                    end ;
                    if( A = _Register[ 0 ] ) then
                    begin
                        Set_Z ;
                    end else
                    begin
                        Reset_Z ;
                    end ;
                    if( A > _Register[ 0 ] ) then
                    begin
                        Set_C ;
                    end else
                    begin
                        Reset_C ;
                    end ;
                    continue ;
                end ;
            $C0..$FE : // Lrr
                begin
                    A1 := ( A shr 3 ) and 7 ; // Get destination register
                    if( A1 > 0 ) then
                    begin
                        inc( A1 ) ;
                    end ;
                    C := A and 7 ; // Get source register
                    A := Extract_Register( C, 1 ) ; // Get source
                    if( A1 = 8 ) then // Destination = M
                    begin
                        Increment_Clock( 7 ) ;
                        ByteWrite( Register_HL and 16383, A ) ;
                    end else
                    begin
                        if( C = 7 ) then // Source = M
                        begin
                            Increment_Clock( 8 ) ;
                        end else
                        begin
                            Increment_Clock( 5 ) ;
                        end ;
                        Register[ A1 ] := A ;
                    end ;
                    continue ;
                end ;
            else
                case A and $C7 of
                    6 : // LrI
                        begin
                            B := Fetch ;
                            C := ( A shr 3 ) and 7 ; // Get register
                            if( C > 0 ) then
                            begin
                                C := C + 1 ; // Skip flag register
                            end ;
                            if( C = 8 ) then // M
                            begin
                                Increment_Clock( 9 ) ;
                                ByteWrite( Register_HL and 16383, B ) ;
                            end else
                            begin
                                Increment_Clock( 8 ) ;
                                Register[ C ] := B ;
                            end ;
                            continue ;
                        end ;
                    5 : // RST
                        begin
                            if( ( _RTS_Flags and RTS_Want_Interrupts ) <> 0 ) then
                            begin
                                if( _RTS.Trap( A and $38 ) ) then
                                begin
                                    continue ;
                                end ;
                            end ;
                            Increment_Clock( 5 ) ;
                            Push( _PC ) ;
                            PC := A and $38 ;
                            continue ;
                        end ;
                    7 : // RET
                        begin
                            if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                            begin
                                if( _RTS.Return ) then
                                begin
                                    continue ;
                                end ;
                            end ;
                            Increment_Clock( 5 ) ;
                            PC := Pop ;
                            continue ;
                        end ;
                    $44 : // JMP
                        begin
                            Increment_Clock( 11 ) ;
                            PC := Fetch_Word and 16383 ;
                            if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                            begin
                                _RTS.Jumped ;
                            end ;
                            continue ;
                        end ;
                    $46 : // CAL
                        begin
                            A := Fetch_Word and 16383 ;
                            if( ( _RTS_Flags and RTS_Want_Calls ) <> 0 ) then
                            begin
                                if( _RTS.Call( A ) ) then
                                begin
                                    continue ;
                                end ;
                            end ;
                            Increment_Clock( 11 ) ;
                            Push( _PC ) ;
                            PC := A ;
                            continue ;
                        end ;
                    else
                        case A and $E7 of
                            3 : // RFc
                                begin
                                    if( not Eff( A, 8 ) ) then
                                    begin
                                        A := Pop ;
                                        if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                                        begin
                                            if( _RTS.Return ) then
                                            begin
                                                continue ;
                                            end ;
                                        end ;
                                        PC := A ;
                                        Increment_Clock( 5 ) ;
                                    end else
                                    begin
                                        Increment_Clock( 3 ) ;
                                    end ;
                                    continue ;
                                end ;
                            $23 : // RTc
                                begin
                                    if( Eff( A, 8 ) ) then
                                    begin
                                        A := Pop ;
                                        if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                                        begin
                                            if( _RTS.Return ) then
                                            begin
                                                continue ;
                                            end ;
                                        end ;
                                        PC := A ;
                                        Increment_Clock( 5 ) ;
                                    end else
                                    begin
                                        Increment_Clock( 3 ) ;
                                    end ;
                                    continue ;
                                end ;
                            $40 : // JFc
                                begin
                                    C := Fetch_Word and 16383 ;
                                    if( not Eff( A, 8 ) ) then
                                    begin
                                        PC := C ;
                                        Increment_Clock( 11 ) ;
                                        if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                                        begin
                                            _RTS.Jumped ;
                                        end ;
                                    end else
                                    begin
                                        Increment_Clock( 9 ) ;
                                    end ;
                                    continue ;
                                end ;
                            $42 : // CFc
                                begin
                                    C := Fetch_Word and 16383 ;
                                    if( not Eff( A, 8 ) ) then
                                    begin
                                        if( ( _RTS_Flags and RTS_Want_Calls ) <> 0 ) then
                                        begin
                                            if( _RTS.Call( C ) ) then
                                            begin
                                                continue ;
                                            end ;
                                        end ;
                                        Push( _PC ) ;
                                        PC := C ;
                                        Increment_Clock( 11 ) ;
                                    end else
                                    begin
                                        Increment_Clock( 9 ) ;
                                    end ;
                                    continue ;
                                end ;
                            $60 : // JTc
                                begin
                                    C := Fetch_Word and 16383 ;
                                    if( Eff( A, 8 ) ) then
                                    begin
                                        PC := C ;
                                        Increment_Clock( 11 ) ;
                                        if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                                        begin
                                            _RTS.Jumped ;
                                        end ;
                                    end else
                                    begin
                                        Increment_Clock( 9 ) ;
                                    end ;
                                    continue ;
                                end ;
                            $62 : // CTc
                                begin
                                    C := Fetch_Word and 16383 ;
                                    if( Eff( A, 8 ) ) then
                                    begin
                                        if( ( _RTS_Flags and RTS_Want_Calls ) <> 0 ) then
                                        begin
                                            if( _RTS.Call( C ) ) then
                                            begin
                                                continue ;
                                            end ;
                                        end ;
                                        Push( _PC ) ;
                                        PC := C ;
                                        Increment_Clock( 11 ) ;
                                    end else
                                    begin
                                        Increment_Clock( 9 ) ;
                                    end ;
                                    continue ;
                                end ;
                            else
                                if( ( A and $F1 ) = $41 ) then // INP
                                begin
                                    Increment_Clock( 8 ) ;
                                    Register[ 0 ] := Input( ( A shr 1 ) and 7 ) ;
                                    continue ;
                                end else
                                if( ( A and $C1 ) = $41 ) then // OUT
                                begin
                                    Increment_Clock( 6 ) ;
                                    Output( ( A shr 1 ) and 31, _Register[ 0 ] ) ;
                                    continue ;
                                end ;
                        end ;
                end ;
        end ; // case A of

Unknown_OpCode:
        Signal_Exception( '', 0 ) ; // Invalid instruction
    end ; // while( True )
end ; // TI8008_CPU.Execute


procedure TI8008_CPU.Output( Port, Value : integer ) ;

var Component : TComponent ;
    Loop : integer ;

begin
    Last_Error := nil ;
    try
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
            Last_Error := Component.Write( Port, Value, 1, IO_Type_IO ) ;
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
                TI8008_Profiler( Parent.Profiler ).Increment( Domain_Port_Outputs, Port ) ;
            end ;
            Log_Trace( 'Output ' + inttostr( Value ) + '. to port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ;


function TI8008_CPU.Input( Port : integer ) : integer ;

var Component : TComponent ;
    Loop : integer ;

begin
    try
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Port, 1, IO_Type_IO ) ) then
            begin
                Result := Memory_Data_Latch ; // To prevent compiler warning
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
                TI8008_Profiler( Parent.Profiler ).Increment( Domain_Port_Inputs, Port ) ;
            end ;
            Log_Trace( 'Input ' + inttostr( Result ) + '. from port ' + inttostr( Port ) + '.' ) ;
        end ;
    end ;
end ;


function TI8008_CPU.Get_HL : integer ;

begin
    Result := Register[ 6 ] ;
    Result := ( Result shl 8 ) or Register[ 7 ] ;
end ;


procedure TI8008_CPU.Set_HL( Value : integer ) ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( 6, Value ) ;
    end ;
    Register[ 6 ] := Value shr 8 ;
    Register[ 7 ] := Value and 255 ;
end ;


function TI8008_CPU.Get_PC : word ;

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


procedure TI8008_CPU.Set_PC( Value : word ) ;

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


function TI8008_CPU.Get_SP : integer ;

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


procedure TI8008_CPU.Set_SP( Value : integer ) ;

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


function TI8008_CPU.Get_Register( Index : integer ) : integer ;

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


procedure TI8008_CPU.Set_Register( Index : integer ; Value : integer ) ;

var Old : integer ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( Index, Value ) ;
    end ;
    Old := _Register[ Index ] ;
    _Register[ Index ] := Value and $FF ;
    if( ( _Run_Stream = nil ) and ( Old <> Value  ) ) then
    begin
        if( ( _Register_Watchpoints[ Index ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Write, 0, False, True, False ) ;
        end ;
    end ;
end ; // TI8008_CPU.Set_Register


function TI8008_CPU.Set_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

begin
    if( ( Address < 0 ) or ( Address > 16383 ) ) then
    begin
        Result := Parent.Set_Error( I8008_Invalid_Address ) ;
        exit ;
    end ;
    if( _Breakpoints.Indexof( Address ) <> -1 ) then
    begin
        Result := Parent.Set_Error( I8008_Breakpoint_Exists ) ;
        exit ;
    end ;
    _Breakpoints.Add( Address ) ;
    Result := Parent.Set_Error( 0 ) ;
end ;


procedure TI8008_CPU.Set_Clock_Speed( Value : longint ) ;

begin
    _Speed := Value ;
end ;


procedure TI8008_CPU.Step( Into : boolean ) ;

begin
    _Halted := False ;
    Execute( True, Into ) ;
end ;


function TI8008_CPU.Translate( Space : integer ; Address : int64 ) : int64 ;

begin
    Translate := Address ;
end ;


function TI8008_CPU.Default_Base : integer ;

begin
    Result := 8 ;
end ;


function TI8008_CPU.Get_Low_Memory : int64 ;

begin
    Result := 0 ;
end ;


function TI8008_CPU.Get_High_Memory : int64 ;

begin
    Result := 16383 ;
end ;


function TI8008_CPU.Get_Low_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TI8008_CPU.Get_High_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TI8008_CPU.Get_Low_Port : int64 ;

begin
    Result := 0 ;
end ;


function TI8008_CPU.Get_High_Port : int64 ;

begin
    Result := 31 ;
end ;


function TI8008_CPU.Support_Virtual_Address : boolean ;

begin
    Result := False ;
end ;


function TI8008_CPU.Register_Name( Index : integer ) : PChar ;

begin
    case Index of
        0 : Temp_Register_Name := 'PC' ;
        1 : Temp_Register_Name := 'SP' ;
        2 : Temp_Register_Name := 'A' ;
        3 : Temp_Register_Name := 'F' ;
        4 : Temp_Register_Name := 'B' ;
        5 : Temp_Register_Name := 'C' ;
        6 : Temp_Register_Name := 'D' ;
        7 : Temp_Register_Name := 'E' ;
        8 : Temp_Register_Name := 'H' ;
        9 : Temp_Register_Name := 'L' ;
        10 : Temp_Register_Name := 'M' ; // HL
        else Temp_Register_Name := '' ; // Invalid index
    end ;
    Result := PChar( Temp_Register_Name ) ;
end ;


function TI8008_CPU.Register_Size( Index : integer ) : integer ;

begin
    case Index of
        0 : Result := 14 ; // PC
        1 : Result := 3 ; // SP
        2 : Result := 8 ; // A
        3 : Result := 8 ; // F
        4 : Result := 8 ; // B
        5 : Result := 8 ; // C
        6 : Result := 8 ; // D
        7 : Result := 8 ; // E
        8 : Result := 8 ; // H
        9 : Result := 8 ; // L
        10 : Result := 16 ; // M
        else Result := 0 ; // Invalid index
    end ;
end ;


function TI8008_CPU.Register_Description( Index : integer ) : PChar ;

begin
    Temp_Register_Description := '' ;
    if( Index = 3 ) then
    begin
        Temp_Register_Description := Get_Mask( _Register[ 1 ] ) ;
    end ;
    Result := PChar( Temp_Register_Description ) ;
end ;


function TI8008_CPU.Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        if( Address > 13 ) then
        begin
            Result := Parent.Set_Error( I8008Err_Invalid_Stack_Address ) ;
        end else
        begin
            _Stack_Watchpoints[ Address ] := _Stack_Watchpoints[ Address ] or Access ;
            Result := Parent.Set_Error( 0 ) ;
        end ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 10 ) ) then
        begin
            Result := Parent.Set_Error( I8008Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] := _Register_Watchpoints[ Address ] or Access ;
            Result := Parent.Set_Error( 0 ) ;
        end ;
    end ;
end ;


function TI8008_CPU.Top_Of_Stack( Index : integer ) : int64 ;

begin
    Top_Of_Stack := SP ;
end ;


function TI8008_CPU.Get_Low_Input_Port( Space : integer ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TI8008_CPU.Get_High_Input_Port( Space : integer ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := 7 ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TI8008_CPU.Get_Low_Output_Port( Space : integer ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TI8008_CPU.Get_High_Output_Port( Space : integer ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := 31 ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TI8008_CPU.Get_Stack_Interface( Space : integer ) : TCEF_Stack_Interface ;

begin
    if( Space = 0 ) then
    begin
        Result := TI8008_Stack_Interface.Create( self ) ;
    end else
    begin
        Result := nil ;
    end ;
end ;


function TI8008_CPU.Segment_Size( Index : integer ) : integer ;

begin
    if( ( Index < 0 ) or ( Index >= Segments.Count ) ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := Segments[ Index ] ;
    end ;
end ;


function TI8008_CPU.Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;

begin
    _RTS := RTS ;
    _RTS_Flags := Flags ;
    Result := nil ;
end ;


function TI8008_CPU.Get_Current_Address( Space : integer ; Physical : boolean ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := PC ;
    end else
    begin
        Result := 0 ;
    end ;
end ;


procedure TI8008_CPU.Set_Current_Address( Space : integer ; Physical : boolean ;
    Value : int64 ) ;

begin
    if( Space = 0 ) then
    begin
        _PC := Value and 16383 ;
    end ;
end ;


procedure TI8008_CPU.Stop ;

begin
    Stopping := True ;
end ;


procedure TI8008_CPU.Restart ;

begin
    _Halted := True ;

    _PC := 0 ;
    fillchar( _Stack, sizeof( _Stack ), 0 ) ;
    fillchar( _Register, sizeof( _Register ), 0 ) ;
    Interrupts := False ;
    _Run_Stream := nil ;
end ;


function TI8008_CPU.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var Dummy : integer ;
    Loop, Loop1 : integer ;
    Parser, Watchpoint_Parser : TXML_Parser ;
    S : string ;

begin
    // Setup default state...
    Result := Set_Error( 0 ) ;
    Interrupts := False ;
    _Halted := False ;
    _Profiling := False ;
    Interrupt_Instruction := False ;
    fillchar( _Register_Watchpoints, sizeof( _Register_Watchpoints ), 0 ) ;
    fillchar( _Stack_Watchpoints, sizeof( _Stack_Watchpoints ), 0 ) ;
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
                Interrupts := True ;
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
            if( S = '<INTERRUPT_INSTRUCTION/>' ) then
            begin
                Interrupt_Instruction := True ;
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
                        _Register[ Loop1 ] :=
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
            if( S = '<STACK>' ) then
            begin
                S := Parser.Get_Section( 'stack' ) ;
                S := copy( S, 2, length( S ) ) ; // Trim first bar
                Loop := 0 ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( '|', S ) ;
                    try
                        _Stack[ Loop ] :=
                            strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop ) ;
                    if( Loop > 13 ) then
                    begin
                        break ;
                    end ;
                end ;
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
                    if( Loop > 10 ) then
                    begin
                        break ;
                    end ;
                end ;
            end else
            if( S = '<STACK_WATCHPOINTS>' ) then
            begin
                S := Parser.Get_Section( 'Stack_Watchpoints' ) ;
                S := copy( S, 2, length( S ) ) ; // Trim first bar
                Loop := 0 ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( '|', S ) ;
                    try
                        _Stack_Watchpoints[ Loop ] := strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop ) ;
                    if( Loop > 13 ) then
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
end ; // TI8008_CPU.Restore_State


function TI8008_CPU.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

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
    if( Interrupts ) then
    begin
        Output( '<Interrupts/>' ) ;
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
    if( Interrupt_Instruction ) then
    begin
        Output( '<Interrupt_Instruction/>' ) ;
    end ;

    Output( '<Registers>' ) ;
    for Loop1 := 0 to 7 do
    begin
        Output( '|' + inttostr( _Register[ Loop1 ] ) ) ;
    end ;
    Output( '</Registers>' ) ;

    Output( '<Breakpoints>' + _Breakpoints.Serialize + '</Breakpoints>' ) ;

    Output( '<Register_Watchpoints>' ) ;
    for Loop := 0 to 10 do
    begin
        Output( '|' + inttostr( _Register_Watchpoints[ Loop ] ) ) ;
    end ;
    Output( '</Register_Watchpoints>' ) ;

    Output( '<Stack>' ) ;
    for Loop := 0 to 13 do
    begin
        Output( '|' + inttostr( _Stack[ Loop ] ) ) ;
    end ;
    Output( '</Stack>' ) ;

    Output( '<Stack_Watchpoints>' ) ;
    for Loop := 0 to 13 do
    begin
        Output( '|' + inttostr( _Stack_Watchpoints[ Loop ] ) ) ;
    end ;
    Output( '</Stack_Watchpoints>' ) ;

    Output( '<Memory_watchpoints>' ) ;
    Output( '<watchpoint>' + string( _Memory_Watchpoints.Serialize ) ) ;
    Output( '</Memory_watchpoints>' ) ;

    Output( '<Port_watchpoints>' ) ;
    for Loop := 0 to 31 do
    begin
        Output( inttostr( _Port_Watchpoints[ Loop ] ) + '|' ) ;
    end ;
    Output( '</Port_watchpoints>' ) ;
end ; // TI8008_CPU.Save_State


function TI8008_CPU.Clear_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        if( ( Address < 0 ) or ( Address > 13 ) ) then
        begin
            Result := Parent.Set_Error( I8008Err_Invalid_Stack_Address ) ;
        end else
        begin
            _Stack_Watchpoints[ Address ] :=
                _Stack_Watchpoints[ Address ] and ( not Access_None ) ;
            Result := Parent.Set_Error( 0 ) ;
        end ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 10 ) ) then
        begin
            Result := Parent.Set_Error( I8008Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] :=
                _Register_Watchpoints[ Address ] and ( not Access_None ) ;
            Result := Parent.Set_Error( 0 ) ;
        end ;
    end ;
end ;



// TI8008 methods...

{ API... }

function TI8008.CPU : TCPU ;

begin
    Result := _CPU ;
end ;


function TI8008.Facility_Code : longint ;

begin
    Result := I8008Err_Facility ;
end ;


function TI8008.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU := TI8008_CPU.Create ;
    _CPU._UI := UI ;
    _CPU.Parent := self ;
    Inputs := TList.Create ;
    Outputs := TList.Create ;
end ;


function TI8008.Terminate : TUnified_Exception ;

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


function TI8008.Serial_Number : integer ;

begin
    Result := _Serial_Number ;
end ;


function TI8008.Child_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TI8008.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Clear_Watchpoints ;
end ; // TI8008.Clear_Watchpoint


function TI8008.Component_Type : longint ;

begin
    Result := Component_Type_CPU ; // CPU
end ;


function TI8008.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( I8008Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Inputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( I8008Err_Already_Connected ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Result := Set_Error( I8008Err_Success ) ;
end ;


function TI8008.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( I8008Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Outputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( I8008Err_Already_Connected ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Result := Set_Error( I8008Err_Success ) ;
end ;


function TI8008.Debugger : TDebug_Interface ;

begin
    Result := nil ; // TODO
end ;


function TI8008.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var V : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        Size := ( Size + 7 ) div 8 ; // Number of bytes
        V := 0 ;
        while( Size > 0 ) do
        begin
            if( ( Address < 0 ) or ( Address > 13 ) ) then
            begin
                Result := Set_Error( I8008Err_Invalid_Stack_Address ) ;
                exit ;
            end ;
            _CPU._Stack[ Address ] := ord( PChar( Buffer )[ V ] ) ;
            inc( V ) ;
            dec( Size ) ;
            inc( Address ) ;
        end ;
    end else
    begin
        if( Address > 10 ) then
        begin
            Result := Set_Error( I8008Err_Invalid_Register ) ;
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
            0 : _CPU._PC := V and 16383 ;
            1 : _CPU._SP := V and 7 ;
            2 : _CPU._Register[ 0 ] := V ;
            3 : _CPU._Register[ 1 ] := V ;
            4 : _CPU._Register[ 2 ] := V ;
            5 : _CPU._Register[ 3 ] := V ;
            6 : _CPU._Register[ 4 ] := V ;
            7 : _CPU._Register[ 5 ] := V ;
            8 : _CPU._Register[ 6 ] := V ;
            9 : _CPU._Register[ 7 ] := V ;
            10 : begin
                     _CPU._Register[ 6 ] := V ;
                     _CPU._Register[ 7 ] := V shr 8 ;
                 end ;
        end ; // case Address
    end ; // if( Memory )
end ; // TI8008.Deposit


function TI8008.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Result := Set_Error( I8008Err_Component_Not_Found ) ;
    end else
    begin
	Result := Set_Error( I8008Err_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TI8008.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Result := Set_Error( I8008Err_Component_Not_Found ) ;
    end else
    begin
	Result := Set_Error( I8008Err_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TI8008.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var V, _Size : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        Size := ( Size + 7 ) div 8 ; // Number of bytes
        V := 0 ;
        while( Size > 0 ) do
        begin
            if( ( Address < 0 ) or ( Address > 13 ) ) then
            begin
                Result := Set_Error( I8008Err_Invalid_Stack_Address ) ;
                exit ;
            end ;
            PChar( Buffer )[ V ] := chr( _CPU._Stack[ Address ] ) ;
            inc( V ) ;
            dec( Size ) ;
            inc( Address ) ;
        end ;
    end else
    begin
        if( Address > 10 ) then
        begin
            Result := Set_Error( I8008Err_Invalid_Register ) ;
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
            2 : V := _CPU._Register[ 0 ] ;
            3 : V := _CPU._Register[ 1 ] ;
            4 : V := _CPU._Register[ 2 ] ;
            5 : V := _CPU._Register[ 3 ] ;
            6 : V := _CPU._Register[ 4 ] ;
            7 : V := _CPU._Register[ 5 ] ;
            8 : V := _CPU._Register[ 6 ] ;
            9 : V := _CPU._Register[ 7 ] ;
            10 : V := _CPU._Register[ 6 ] or ( _CPU._Register[ 7 ] shl 8 ) ;
        end ; // case Address
        _Size := ( Size + 7 ) div 8 ; // Number of bytes
        move( V, Buffer^, _Size ) ;
    end ; // if( Memory )
end ; // TI8008.Examine


function TI8008.Get_Access_Mode( Address : int64 ; Memory : boolean ) : longint ;

begin
    if( Memory ) then
    begin
        Result := Access_None ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 10 ) ) then
        begin
            Result := Access_None ;
        end else
        begin
            Result := Access_All ;
        end ;
    end ;
end ;


function TI8008.Get_Profiling : boolean ;

begin
    Result := _CPU._Profiling ;
end ;


function TI8008.Get_Read_Latency : longint ;

begin
    Result := 0 ;
end ;


function TI8008.Get_Write_Latency : longint ;

begin
    Result := 0 ;
end ;


function TI8008.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
        Result := nil ;
        Set_Error( I8008Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Inputs[ Index ] ;
end ;


const I8008_Name : string = 'Intel 8008' ;

function TI8008.Name : PChar ;

begin
    Result := PChar( I8008_Name ) ;
end ;


function TI8008.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Outputs.Count ) ) then
    begin
        Result := nil ;
        Set_Error( I8008Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Outputs[ Index ] ;
end ;


function TI8008.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ; // Doesn't apply to CPUs
end ;


function TI8008.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
end ;


function TI8008.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Restore_State( Stream ) ;
end ;


function TI8008.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
end ;


function TI8008.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Save_State( Stream ) ;
end ;


function TI8008.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( I8008_Invalid_Operation ) ;
end ;


procedure TI8008.Set_Profiling( _On, Children : boolean ) ;

begin
    _CPU._Profiling := _On ;
end ;


procedure TI8008.Set_Read_Latency( Value : longint ) ;

begin
    // Do nothing - we have no read latency
end ;


function TI8008.Set_Watchpoint( Address : int64 ; Memory : boolean ;
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
            Result := Set_Error( I8008_Invalid_Address ) ;
            exit ;
        end ;
        _CPU._Port_Watchpoints[ Address ] := _CPU._Port_Watchpoints[ Address ] or Access ;
    end ;
end ; // TI8008.Set_Watchpoint


procedure TI8008.Set_Write_Latency( Value : longint ) ;

begin
    // Intentionally left blank - no latency
end ;


procedure TI8008.Show_Status ;

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
    Output( 'Intel 8008' ) ;
    S := 'PC=' + Show( _CPU._Pc, 2 ) + '  SP=' + Show( _CPU._Sp, 2 ) + '  Interrupts=' ;
    if _CPU.Interrupts then
    begin
        S := S + ' enabled' ;
    end else
    begin
        S := S + ' disabled' ;
    end ;
    Output( S ) ;
    S := S + 'A=' + Show( Lo( _CPU._Register[ 0 ] ), 1 ) ;
    S := S + '    F=' + Show( Hi( _CPU._Register[ 0 ] ), 1 ) +
        ' (' + Get_Mask( Hi( _CPU._Register[ 0 ] ) ) + ')' ;
    Output( S ) ;
    S := S + 'B=' + Show( Hi( _CPU._Register[ 1 ] ), 1 ) ;
    S := S + '    C=' + Show( Lo( _CPU._Register[ 2 ] ), 1 ) ;
    S := S + 'D=' + Show( Hi( _CPU._Register[ 3 ] ), 1 ) ;
    S := S + '    E=' + Show( Lo( _CPU._Register[ 4 ] ), 1 ) ;
    S := S + 'H=' + Show( Hi( _CPU._Register[ 5 ] ), 1 ) ;
    S := S + '    L=' + Show( Lo( _CPU._Register[ 6 ] ), 1 ) ;
    S := S + '    M=' + Show( _CPU._Register[ 5 ] or swap( _CPU._Register[ 6 ] ), 2 ) ;
    Output( S ) ;
end ; // TI8008.Show_Status


function TI8008.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


procedure TI8008.Wake ;

begin
    _CPU.Blocked := False ;
end ;


function TI8008.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : integer ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := Value ;
end ;


function TI8008.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := ord( Value[ Size - 1 ] ) ;
end ;


procedure TI8008.Set_Up( P : PChar ) ;

begin
end ;


procedure TI8008.Reset ;

begin
    CPU.Restart ;
end ;


procedure TI8008.Set_Signal( Name : PChar ; State : boolean ) ;

var Temp : string ;

begin
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    if( _CPU.Interrupts and ( Temp = 'INT' ) ) then
    begin
        if( _Logger <> nil ) then
        begin
            _Logger.Log( self, 'INT', -1, False, LT_Received_Signal ) ;
        end ;
        _CPU.Do_Interrupt ;
    end ;
end ; // TI8008.Set_Signal


function TI8008.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

begin
    Result := False ;
end ;


function TI8008.Signal_Count : longint ;

begin
    Result := 1 ;
end ;


function TI8008.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := '' ;
    case Index of
        0 : Temp_Signal_Name := 'INT' ;
    end ;
    if( Temp_Signal_Name = '' ) then
    begin
        Result := nil ;
    end else
    begin
        Result := PChar( Temp_Signal_Name ) ;
    end ;
end ;


function TI8008.Signal_Index( Name : PChar ) : integer ;

var S : string ;

begin
    S := string( Name ) ;
    Result := -1 ;
end ;


function TI8008.Signal_Out( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TI8008.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TI8008.Get_State_Name( Index : longint ) : PChar ;

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


function TI8008.Get_Exception_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Temp_Get_Exception_Description := 'Invalid instruction' ;
                Result := PChar( Temp_Get_Exception_Description ) ;
            end ;
    end ;
end ;


procedure TI8008.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TI8008.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TI8008.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TI8008.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function TI8008.Profiler : TProfiler ;

begin
    if( _CPU._Profiler = nil ) then
    begin
        _CPU._Profiler := TI8008_Profiler.Create ;
    end ;
    Result := _CPU._Profiler ;
end ;


function TI8008.Get_Trace : boolean ;

begin
    Result := _CPU._Trace ;
end ;


procedure TI8008.Set_Trace( Value : boolean ) ;

begin
    _CPU._Trace := Value ;
end ;


function TI8008.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TI8008.Set_Logger( Value : TCEF_Logger ) ;

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

