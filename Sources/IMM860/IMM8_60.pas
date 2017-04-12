{$N+}
{
        Program Name : IMM860
        Package Name : CEF
        Purpose      : Intel IMM8-60 component for CEF
        Institution  : 
        Date Written : 5-Jan-2008
        Written By   : Alan Conroy
        Version      : 1.0

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

        This unit implements an Intel Imm8-60 serial interface.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan


        Conditionals:
            TEST    Causes test to run upon unit initialization.
}

unit IMM8_60 ;

interface

{$I EDEFINES.INC}

uses // Borland...
     Windows, // LoadLibrary
     Classes,

     // C&C...
     Compatib,
     Collect, { TCollection }
     _DebugIn, // TDebug_Interface
     DebugInt, // TText_Debugger
     _Streams, // TCOM_Stream
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TUI_Interface
     CEF, { TBase_Cable }
     _CEFUtil ; // TCEF_Watchpoint

const CEFIMM860_Facility = -1 ;
      CEFIMM860Err_Success = 0 ;
      CEFIMM860Err_Invalid_Range = 1 ;
      CEFIMM860Err_Component_Not_Found = 2 ;
      CEFIMM860Err_No_Matching_Watchpoint = 3 ;
      CEFIMM860Err_Access_Violation = 4 ;
      CEFIMM860Err_Address_Out_Of_Range = 5 ;
      CEFIMM860Err_Invalid_Component = 6 ;
      CEFIMM860Err_Memory_Exhausted = 7 ; { Couldn't allocate any more memory }

type TIMM860 = class ;

     TReceiver = class( TBase_Cable )
                     private
                         Cable : TComponent ;
                         IO : TIMM860 ;
                         Parent : TComponent ;

                     public
                         procedure Receive( Source : TComponent ; Speed : int64 ;
                             Value, Data_Size, Stop_Bits : longint ) ;
                             override ;

                         procedure Send_Data( Value : longint ) ;
                 end ;

     TIMM860_IO = class;

     TIMM860 = class( TBase_Component )
        private { Instance data... }
            Access_Mode : integer ;
            Inputs, Outputs : TList ;
            Profiling : boolean ; { True if profiling accesses }
            _UI : TUI_Interface ;
            Watchpoint_List : TCEF_Watchpoint_Manager ;
            _Tag : longint ;
            _Parent : TComponent ;
            _Cable : TComponent ; // What we are connected to
            Cable_End : TReceiver ; // Our cable end
            IO : TIMM860_IO ;
            Check_Parity : boolean ;
            Temp_Signal_Name : string ;
            _Logger : TCEF_Logger ;

        private // Serial line attributes...
            _Size : integer ;
            _Transmit, _Receive : integer ;
            _Stop_Bits : integer ;

        private // Registers...
            RBUF : byte ; // Input data - port 0
            RCSR : byte ; // Input status - port 1
                          // and 1 = Data available
                          // and 2 = Overrun error
                          // and 4 = Transmit buffer empty
                          // and 8 = Framming error
                          // and 16 = Parity error (inhibited)
                          // and 32 = Data available (tape reader)
                          // and 64 = Punch ready
                          // and 128 = unused
            XBUF : byte ; // Output data - port 8 (10 octal)
            XCSR : byte ; // Output control - port 9 (11 octal)
                          // and 1 = Reader ADV-1
                          // and 2 = Punch command
                          // and 4 = Reader command
                          // and 8 = Data Out Enabl
                          // and 16 = Data In
                          // and 32 = Data Out
                          // and 64 = R (else W)
                          // and 128 = RA (else WA)

        protected // Property handlers...
            function Translate_Error( Code : longint ) : string ; override ;

        private // Internal utility routines...
            function Default_Input : TComponent ;
            function Default_Output : TComponent ;
            procedure Receive( Source : TComponent ;
                Speed, Value, Data_Size, Stop_Bits : integer ) ;
            function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

	    public { Public instance data... }
            _Serial_Number : integer ;

        public { API... }
            function Facility_Code : longint ; override ;

            function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                override ;

            function Terminate : TUnified_Exception ; override ;

            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; override ;

            function Component_Type : longint ; override ;

            function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                override ;

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

            function Get_Access_Mode( Address : int64 ;
                Memory : boolean ) : longint ; override ;

            function Get_Profiling : boolean ; override ;

            function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                override ;

            function Input_Component( Index : longint ) : TComponent ; override ;

            function Cable : TCable ; override ;

            function Memory : TMemory ; override ;

            function Name : PChar ; override ;

            function Output_Component( Index : longint ) : TComponent ;
                override ;

            function Read( Address : int64 ; Size : longint ;
                IO_Type : longint ) : boolean ; override ;

            function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                Typ : longint ) : TUnified_Exception ; override ;

            procedure Set_Signal( Name : PChar ; State : boolean ) ; override ;

            procedure Set_Profiling( _On, Children : boolean ) ;  override ;

            function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; override ;

            procedure Show_Status ; override ;

            function Signal_Count : longint ; override ;

            function Signal_Name( Index : longint ) : PChar ; override ;

            function Signal_Out( Index : longint ) : boolean ; override ;

            function Signal_Active_Low( Index : longint ) : boolean ; override ;

            function Signal_Index( Name : PChar ) : integer ; override ;

            function Write( Address : int64 ; Value, Size : longint ;
                IO_Type : longint ) : TUnified_Exception ; override ;

            procedure Set_Tag( Value : longint ) ; override ;

            function Get_Tag : longint ; override ;

            function Get_Parent : TComponent ; override ;

            procedure Set_Parent( Component : TComponent ) ; override ;

            procedure Set_Up( P : PChar ) ; override ;

            procedure Child_Notification( Child : TComponent ;
                var Notice : longint ; var Params : int64 ) ; override ;
            function Get_Port( Index : longint ) : TComponent ; override ;
            function Get_Port_Connection( Index : longint ) : TComponent ;
                override ;
            function Get_Logger : TCEF_Logger ; override ;
            procedure Set_Logger( Value : TCEF_Logger ) ; override ;
     end ;

     TIMM860_IO = class( TBase_Memory )
                     public // Constructors...
                         constructor Create ;

                     private // Instance data...
                         Parent : TIMM860 ;
                          _Low : int64 ; // Lowest port
                          _Send : integer ; // Transmit baud rate
                          _Receive : integer ; // Receive baud rate
                          
                     public // API...
                         function Facility_Code : longint ; override ;

                         function Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;
                             override ;

                         procedure Get_Address_Range( var Low, High : int64 ) ;
                             override ;
                  end ;

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
{$IFDEF Test}
     Dialogs,
{$ENDIF}
     Sysutils, // Allocmem

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     UE, // Create_Simple_UE
     Num1s,
     Parse ; // TString_Parser

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ;
    external 'CEF_Util.dll' ;


function TIMM860.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        CEFIMM860Err_Success: _Error := 'Success' ;
        CEFIMM860Err_Invalid_Range: _Error := 'Invalid range' ;
        CEFIMM860Err_Component_Not_Found: _Error := 'Component not found' ;
        CEFIMM860Err_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        CEFIMM860Err_Access_Violation: _Error := 'Access violation' ;
        CEFIMM860Err_Address_Out_Of_Range: _Error := 'Address out of range' ;
        CEFIMM860Err_Invalid_Component: _Error := 'Invalid component' ;
        CEFIMM860Err_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { Translate_Error }


// TReceiver methods...

procedure TReceiver.Receive( Source : TComponent ; Speed : int64 ;
    Value, Data_Size, Stop_Bits : longint ) ;

begin
    IO.Receive( Source, Speed, Value, Data_Size, Stop_Bits ) ;
end ;


procedure TReceiver.Send_Data( Value : longint ) ;

begin
    if( Cable <> nil ) then
    begin
        Cable.Cable.Receive( Parent, IO._Transmit, Value, IO._Size, IO._Stop_Bits ) ;
    end ;
end ;



{ TCEF_IMM860_Debugger methods... }

type TCEF_IMM860_Debugger = class( TText_Debugger )
                                private
                                    _IO : TIMM860 ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property IO : TIMM860
                                                 read _IO
                                                 write _IO ;
                            end ;

function TCEF_IMM860_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

    function Pointer_To_String( P : pointer ) : string ;

    begin
        if( P = nil ) then
        begin
            Result := 'nil' ;
        end else
        begin
            Result := Num1( longint( P ) ) ;
        end ;
    end ;


    function Boolean_To_String( B : boolean ) : string ;

    begin
        if( B ) then
        begin
            Result := 'True' ;
        end else
        begin
            Result := 'False' ;
        end ;
    end ;


    function Access_Mode_To_String( AM : integer ) : string ;

    begin
        if( ( AM and Access_All ) = Access_All ) then
        begin
            Result := 'Access_All' ;
            exit ;
        end ;
        if( ( AM and Access_RW ) = Access_RW ) then
        begin
            Result := 'Access_RW' ;
            exit ;
        end ;
        if( AM = 0 ) then
        begin
            Result := 'Access_None' ;
            exit ;
        end ;
        if( ( AM and Access_Read ) = Access_Read ) then
        begin
            Result := 'Access_Read' ;
        end ;
        if( ( AM and Access_Write ) = Access_Write ) then
        begin
            if( Result <> '' ) then
            begin
                Result := Result + ', ' ;
            end ;
            Result := Result + 'Access_Write' ;
        end ;
        if( ( AM and Access_Execute ) = Access_Execute ) then
        begin
            if( Result <> '' ) then
            begin
                Result := Result + ', ' ;
            end ;
            Result := Result + 'Access_Execute' ;
        end ;
        if( Result = '' ) then
        begin
            Result := '* unknown *' ;
        end ;
    end ;


var I : TText_Debugger ;
    I1 : TDebug_Interface ;

begin
    I := TText_Debugger.Create ;
    case Ordinal of
        0 : I.Title := PChar( 'Access_Mode = ' + Access_Mode_To_String( IO.Access_Mode ) ) ;
        1 : begin
                if( IO.Default_Input = nil ) then
                begin
                    I.Title := 'Default_Input = nil' ;
                end else
                begin
                    I1 := IO.Default_Input.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Input = ' + Pointer_To_String( IO.Default_Input ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Input = ' + Pointer_To_String( IO.Default_Input ) ) ;
                end ;
            end ;
        2 : begin
                if( IO.Default_Output = nil ) then
                begin
                    I.Title := 'Default_Output = nil' ;
                end else
                begin
                    I1 := IO.Default_Output.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Output = ' + Pointer_To_String( IO.Default_Output ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Output = ' + Pointer_To_String( IO.Default_Output ) ) ;
                end ;
            end ;
        3 : I.Title := PChar( '_Low = ' + Num1( IO.IO._Low ) ) ;
        4 : I.Title := PChar( 'Profiling = ' + Boolean_To_String( IO.Profiling ) ) ;
        5 : I.Title := PChar( '_Serial_Number = ' + Num1( IO._Serial_Number ) ) ;
        6 : begin
                if( IO._UI = nil ) then
                begin
                    I.Title := '_UI = nil' ;
                end else
                begin
                    I1 := IO._UI.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( '_UI = ' + Pointer_To_String( IO._UI ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( '_UI = ' + Pointer_To_String( IO._UI ) ) ;
                end ;
            end ;
        7 : I.Title := Pchar( 'Watchpoint_List = ' + Pointer_To_String( pointer( IO.Watchpoint_List ) ) ) ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TCEF_IMM860_Debugger.Count : longint ;

begin
    Result := 9 ;
end ;


{ TIMM860 methods... }

function TIMM860.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TIMM860.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


procedure TIMM860.Receive( Source : TComponent ;
    Speed, Value, Data_Size, Stop_Bits : integer ) ;

var B : boolean ;

begin
    Value := Value and 255 ;
    RCSR := ( RCSR and 33 ) or 4 or 64 ; // Reset all but data-available and trasmit/punch ready
    if( ( RCSR and 1 ) = 1 ) then // Already had data
    begin
        RCSR := RCSR or 2 ; // Overrun
    end ;
    B := False ;
    if( _Cable <> nil ) then
    begin
        _Cable.Get_Signal( 'BREAK', B ) ;
        if( B ) then
        begin
            RCSR := RCSR or 8 ; // Framing error
        end ;
    end ;
    if(
        ( ( Speed <> IO._Receive ) and ( Speed <> 0 ) and ( _Receive <> 0 ) )
        or
        ( ( Data_Size <> _Size ) and ( Data_Size <> 0 ) and ( _Size <> 0 ) )
        or
        ( ( Stop_Bits <> _Stop_Bits ) and ( Stop_Bits <> 0 ) and ( _Stop_Bits <> 0 ) )
        or
        B
      ) then
    begin
        Value := Value or 8 ; // Frame error
    end ;
    RCSR := RCSR or 1 ; // Data valid
    RBUF := Value ;
end ; // TIMM860.Receive


function TIMM860.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_List.Watchpoint_At( Address ) ;
end ;


{ API... }

function TIMM860.Facility_Code : longint ;

begin
    Facility_Code := CEFIMM860_Facility ;
end ;


function TIMM860.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    Profiling := False ;
    _UI := UI ;
    Watchpoint_List := Get_Watchpoint_Manager ;
    Access_Mode := Access_RW or Access_Execute ;
    Check_Parity := False ;
    IO := TIMM860_IO.Create ;
    Cable_End := TReceiver.Create ;
    Cable_End.IO := self ;
    Cable_End.Parent := self ;

    Initialize := Set_Error( CEFIMM860Err_Success ) ;
end ; { TIMM860.Initialize }


function TIMM860.Terminate : TUnified_Exception ;

begin
    if( _UI <> nil ) then
    begin
        _UI.Termination_Notice( self ) ;
    end ;
    Watchpoint_List.Terminate ;
    Watchpoint_List := nil ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Cable_End.Free ;
    Cable_End := nil ;
    IO.Free ;
    IO := nil ;
    Terminate := Set_Error( CEFIMM860Err_Success ) ;
end ; { TIMM860.Terminate }



function TIMM860.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Result := nil ;
    Last_Error := nil ;
    if( not Memory ) then
    begin
        Result := Watchpoint_List.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( CEFIMM860Err_No_Matching_Watchpoint ) ;
    end ;
end ;


function TIMM860.Component_Type : longint ;

begin
    Component_Type := Component_Type_Memory ;
end ;


function TIMM860.Connect_Input( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := Set_Error( CEFIMM860Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Component.Cable <> nil ) then
    begin
        _Cable := Component ;
        Cable_End.Cable := Component ;
    end else
    begin
        Inputs.Add( Component ) ;
    end ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        P := integer( Component ) ;
        Parent.Child_Notification( self, Code, P ) ;
    end ;
    Connect_Input := Set_Error( CEFIMM860Err_Success ) ;
end ;


function TIMM860.Connect_Output( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := Set_Error( CEFIMM860Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Component.Cable <> nil ) then
    begin
        _Cable := Component ;
        Cable_End.Cable := Component ;
    end else
    begin
        Outputs.Add( Component ) ;
    end ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        P := integer( Component ) ;
        Parent.Child_Notification( self, Code, P ) ;
    end ;
    Connect_Output := Set_Error( CEFIMM860Err_Success ) ;
end ;


function TIMM860.Debugger : TDebug_Interface ;

begin
    Result := TCEF_IMM860_Debugger.Create ;
    TCEF_IMM860_Debugger( Result ).IO := Self ;
end ;


function TIMM860.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Value : word ;

begin
    if( Memory or ( Size = 0 ) ) then
    begin
        Deposit := Set_Error( CEFIMM860Err_Address_Out_Of_Range ) ;
        exit ;
    end ;
    if( ( IO._Low + 8 <> Address ) and ( IO._Low + 9 <> Address ) ) then
    begin
        Deposit := Set_Error( CEFIMM860Err_Address_Out_Of_Range ) ;
        exit;
    end ;
    _Buffer := PChar( Buffer ) ;
    Value := ord( _Buffer[ 0 ] ) ;
    Address := Address - IO._Low ;
    try
        case Address of
            9 : // XCSR
                begin
                    XCSR := Value ;
                    if( _Cable <> nil ) then
                end ;
            8 : // XBUF
                begin
                    XBUF := Value ;
                end ;
        end ; // Case Address of
    except
    end ;
    Deposit := Set_Error( CEFIMM860Err_Success ) ;
end ; { TIMM860.Deposit }


function TIMM860.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
        Disconnect_Input := Set_Error( CEFIMM860Err_Component_Not_Found ) ;
    end else
    begin
	    Disconnect_Input := Set_Error( CEFIMM860Err_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            P := integer( Component ) ;
            Parent.Child_Notification( self, Code, P ) ;
        end ;
	    Inputs.Remove( Component ) ;
    end ;
end ;


function TIMM860.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
        Disconnect_Output := Set_Error( CEFIMM860Err_Component_Not_Found ) ;
    end else
    begin
    	Disconnect_Output := Set_Error( CEFIMM860Err_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            P := integer( Component ) ;
            Parent.Child_Notification( self, Code, P ) ;
        end ;
	    Outputs.Remove( Component ) ;
    end ;
end ;


function TIMM860.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Value : integer ;

begin
    if( Memory or ( Size = 0 ) ) then
    begin
        Examine := Set_Error( CEFIMM860Err_Address_Out_Of_Range ) ;
        exit ;
    end ;
    if( ( IO._Low <> Address ) and ( IO._Low + 1 <> Address ) ) then
    begin
        Examine := Set_Error( CEFIMM860Err_Address_Out_Of_Range ) ;
        exit;
    end ;
    fillchar( Buffer^, 1, 255 ) ;
    _Buffer := Buffer ;
    Address := Address - IO._Low ;

    // Retrieve data...
    try
        case Address of
            0 : Value := RBUF ;
            1 : // RCSR
                begin
                    Value := RCSR ;
                end ;
        end ; // Case Address of
        _Buffer[ 0 ] := char( Value ) ;
    except
    end ;
    Examine := Set_Error( CEFIMM860Err_Success ) ;
end ; { TIMM860.Examine }


function TIMM860.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    if( ( not Memory ) and ( IO._Low <= Address ) and ( IO._Low + 7 >= Address ) ) then
    begin
        Get_Access_Mode := Access_Mode ;
    end else
    begin
        Get_Access_Mode := Access_None ;
    end ;
end ;


function TIMM860.Get_Profiling : boolean ;

begin
    Get_Profiling := Profiling ;
end ;


function TIMM860.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

begin
    Result := False ;
end ;


function TIMM860.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	    Input_Component := nil ;
    end else
    begin
	    Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


function TIMM860.Cable : TCable ;

begin
    Result := Cable_End ;
end ;


function TIMM860.Memory : TMemory ;

begin
    Result := IO ;
end ;


const _Name : PChar = 'IMM860'#0 ;

function TIMM860.Name : PChar ;

begin
    Name := _Name ;
end ;


function TIMM860.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	    Output_Component := nil ;
    end else
    begin
	    Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


function TIMM860.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

var Buffer : word ;
    Loop : integer ;
    Watch : TCEF_Watchpoint ;

begin
    Read := False ;
    if( ( IO_Type <> IO_Type_IO ) or ( Size = 0 ) ) then
    begin
        exit ;
    end ;
    if( ( IO._Low <> Address ) and ( IO._Low + 1 <> Address ) ) then
    begin
        exit ;
    end ;
    if( ( Access_Mode and Access_Read ) = 0 ) then
    begin
        exit ;
    end ;
    Read := True ;
    Buffer := 0 ;
    Examine( Address, Size, PChar( @Buffer ), False ) ;
    if( _Logger <> nil ) then
    begin
        _Logger.Log( self, PChar( cvtb( 10, _Logger.Data_Radix, inttostr( Buffer ) ) ), -1, False, LT_Input ) ;
    end ;
    case Address - IO._Low of
        0 : // RBUF
            begin
                RCSR := RCSR and ( not 1 ) ; // Clear data available
            end ;
    end ; // Case Address of
    if( Default_Output <> nil ) then
    begin
        for Loop := 0 to Outputs.Count - 1 do
        begin
            TComponent( Outputs[ Loop ] ).Write_String( Address, PChar( @Buffer ), Size, IO_Type_Memory ) ;
        end ;
    end ;
    if( Default_Input <> nil ) then
    begin
        for Loop := 0 to Inputs.Count - 1 do
        begin
            try
                TComponent( Inputs[ Loop ] ).Write_String( Address, PChar( @Buffer ), Size, IO_Type_Memory ) ;
            except
            end ;
        end ;
    end ;
    Watch := Watchpoint_At( Address ) ;
    if( Watch <> nil ) then
    begin
        if( ( Watch.Access and Access_Read ) <> 0 ) then
        begin
            _UI.Watchpoint_Notice( Address, Access_Read, 0, TComponent( self ), True, False, False ) ;
        end ;
    end ;
    if( Profiling ) then
    begin
         //~~~
    end ;
end ; // TIMM860.Read


function TIMM860.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

var Loop, High : int64 ;
    S : longint ;
    Value : byte ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    S := sizeof( Loop ) ;
    Stream.Read( Loop, S ) ;
    Stream.Read( High, S ) ;
    S := 1 ;
    while( Loop <= High ) do
    begin
	Stream.Read( Value, S ) ;
        if( S = 0 ) then
        begin
            exit ;
        end ;
	Deposit( Loop, 1, @Value, False ) ;
	Loop := Loop + 1 ;
    end ;
end ;


function TIMM860.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var S : longint ;

begin
    Result := nil ;
    Last_Error := nil ;
    S := sizeof( IO._Low ) ;
    Stream.Read( IO._Low, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    S := sizeof( Profiling ) ;
    Stream.Read( Profiling, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
end ;


function TIMM860.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

var Loop : int64 ;
    High : int64 ;
    S : longint ;
    Value : integer ;

begin { TIMM860.Save_Contents }
    fillchar( Result, sizeof( Result ), 0 ) ;

    { Write contents... }
    Loop := 0 ;
    Stream.Write( Loop, sizeof( Loop ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        Last_Error := Result ;
        exit ;
    end ;
    Stream.Write( High, sizeof( High ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        Last_Error := Result ;
        exit ;
    end ;
    S := 1 ;
    while( Loop <= High ) do
    begin
        Examine( Loop, S, @Value, True ) ;
	    Stream.Write( Value, 1 ) ;
        Result := Stream.Last_Error ;
        if( Result <> nil ) then
        begin
            Last_Error := Result ;
            exit ;
        end ;
	Loop := Loop + 1 ;
    end ;
end ; { TIMM860.Save_Contents }


function TIMM860.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Stream.Write( IO._Low, sizeof( IO._Low ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        Last_Error := Result ;
        exit ;
    end ;
    Stream.Write( Profiling, sizeof( Profiling ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        Last_Error := Result ;
        exit ;
    end ;
end ;


function TIMM860.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    if( ( not Memory ) and ( IO._Low <= Low ) and ( IO._Low + 7 >= High ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := Set_Error( CEFIMM860Err_Success ) ;
end ;


procedure TIMM860.Set_Signal( Name : PChar ; State : boolean ) ;

begin
end ;


procedure TIMM860.Set_Profiling( _On, Children : boolean ) ;

begin
    Profiling := _On ;
end ;


function TIMM860.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Watchpoint := Set_Error( CEFIMM860Err_Success ) ;
    if( ( not Memory ) and ( IO._Low <= Address ) and ( IO._Low + 7 >= Address ) ) then
    begin
        Watchpoint_List.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TIMM860.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TIMM860.Signal_Count : longint ;

begin
    Result := 1 ;
end ;


function TIMM860.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := '' ;
    Result := PChar( Temp_Signal_Name ) ;
end ;


function TIMM860.Signal_Out( Index : longint ) : boolean ;

begin
    Result := ( Index = 0 ) ;
end ;


function TIMM860.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TIMM860.Signal_Index( Name : PChar ) : integer ;

begin
    Result := - 1 ;
end ;


function TIMM860.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var Watch : TCEF_Watchpoint ;

begin
    if( ( IO_Type <> IO_Type_IO ) or ( Size = 0 ) ) then
    begin
        Write := Set_Error( CEFIMM860Err_Address_Out_Of_Range ) ;
        exit ;
    end ;
    if(
        ( Address <> IO._Low + 8 )
        and
        ( Address <> IO._Low + 9 )
      ) then
    begin
        Write := Set_Error( CEFIMM860Err_Address_Out_Of_Range ) ;
        exit ;
    end ;
    if( ( Access_Mode and Access_Write ) = 0 ) then
    begin
        Write := Set_Error( CEFIMM860Err_Access_Violation ) ;
        exit ;
    end ;
    if( Profiling ) then
    begin
        //~~~
    end ;
    Watch := Watchpoint_At( Address ) ;
    if( Watch <> nil ) then
    begin
        if( ( Watch.Access and Access_Write ) <> 0 ) then
        begin
            _UI.Watchpoint_Notice( Address, Access_Write, 0, TComponent( self ), True, False, False ) ;
        end ;
    end ;
    Write := Deposit( Address, Size, @Value, False ) ;
    case Address - IO._Low of
        8 : // XBUF
            begin
                if( _Cable <> nil ) then
                begin
                    _Cable.Cable.Receive( self, _Transmit, Value and 255, _Size, _Stop_Bits ) ;
                    if( _Logger <> nil ) then
                    begin
                        _Logger.Log( self, PChar( cvtb( 10, _Logger.Data_Radix, inttostr( Value ) ) ), -1, False, LT_Output ) ;
                    end ;
                end ;
            end ;
        9 : // XCSR
    end ;
end ;


constructor TIMM860_IO.Create ;

begin
    inherited Create ;

    _Low := 0 ; 
end ;


function TIMM860_IO.Facility_Code : longint ;

begin
    Result := CEFIMM860_Facility ;
end ;


function TIMM860_IO.Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;

begin
    if( ( Low < 0 ) or ( Low > High ) ) then
    begin
	    Set_Address_Range := Parent.Set_Error( CEFIMM860Err_Invalid_Range ) ;
	    exit ;
    end ;
    Set_Address_Range := Parent.Set_Error( CEFIMM860Err_Success ) ;
    _Low := Low ;
end ;


procedure TIMM860_IO.Get_Address_Range( var Low, High : int64 ) ;

begin
    Low := _Low ;
    High := _Low + 7 ;
end ;


procedure TIMM860.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TIMM860.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TIMM860.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TIMM860.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
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


procedure TIMM860.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S, S1 : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'ADDRESS' ) then
        begin
            S := uppercase( Parser.Token ) ;
            S1 := Parser.Token ;
            if( S1 = '.' ) then
            begin
                S := S + '.' ;
            end else
            begin
                Parser.Put_Token( S1 ) ;
            end ;
            IO._Low := Convert_Value( S ) ;
        end else
        if( S = 'PARITY' ) then
        begin
            Check_Parity := True ;
        end else
        if( S = 'NOPARITY' ) then
        begin
            Check_Parity := False ;
        end else
        if( S = 'SEND' ) then
        begin
            S := uppercase( Parser.Token ) ;
            IO._Send := Convert_Value( S ) ;
        end else
        if( S = 'RECEIVE' ) then
        begin
            S := uppercase( Parser.Token ) ;
            IO._Receive := Convert_Value( S ) ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ; // TIMM860.Set_Up


procedure TIMM860.Child_Notification( Child : TComponent ;
    var Notice : longint ; var Params : int64 ) ;

var Speed : int64 ;
    Value, Data_Size, Stop_Bits : integer ;

begin
    if( ( Notice = Child_Notice_Receive_Data ) and ( Child = _Cable ) and ( _Cable <> nil ) ) then
    begin
        _Cable.Cable.Get_Data( Speed, Value, Data_Size, Stop_Bits ) ;
        Receive( _Cable, Speed, Value, Data_Size, Stop_Bits ) ;
    end else
    if( Notice = Child_Notice_Connect ) then
    begin
        if( ( Child = _Cable ) and ( _Cable <> nil ) ) then
        begin
            _Cable := pointer( Params ) ;
            Cable_End.Cable := _Cable ;
        end ;
    end else
    if( Notice = Child_Notice_Disconnect ) then
    begin
        if( ( Child = _Cable ) and ( _Cable <> nil ) ) then
        begin
            _Cable := nil ;
            Cable_End.Cable := nil ;
        end ;
    end ;
end ;


function TIMM860.Get_Port( Index : longint ) : TComponent ;

begin
    Result := nil ;
    case Index of
        0 : Result := _Cable ;
       else exit ;
    end ;
end ;


function TIMM860.Get_Port_Connection( Index : longint ) : TComponent ;

begin
    Result := nil ;
    case Index of
        0 : Result := _Cable ;
        else exit ;
    end ;
end ;


function TIMM860.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TIMM860.Set_Logger( Value : TCEF_Logger ) ;

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
end ;


{$IFDEF Test}

type TTest_UI = class( TUI_Interface )
                   private
                       Profile_Address : int64 ;
                       Profile_Access : integer ;
                       Watch_Address : int64 ;
                       Watch_Access : integer ;

                   public
                       Profile_Triggered : boolean ;
                       Watch_Triggered : boolean ;

                   public
                       procedure Block( Component : TComponent ; Blocked : boolean ) ;
                           override ;

                       function Breakpoint_Notice( Address : int64 ; Physical : boolean ;
                           CPU : TCPU ) : boolean ; override ;

                       function Clock : TMaster_Clock ; override ;

                       procedure Log_Error( Text : PChar ; Severity : longint ) ;
                           override ;

                       procedure Log_Simulated_Error( Text : PChar ;
                           Severity : longint ) ; override ;

                       procedure Log_Status( Text : PChar ; Index : longint ) ;
                           override ;

                       procedure Profile_Notice( Address : int64 ; Access : longint ;
                           Tag, Time : longint ; Component : TComponent ) ; override ;

                       function Version : integer ; override ;

                       procedure Watchpoint_Notice( Address : int64 ;
                           Access, Tag : longint ; Component : TComponent ) ;
                           override ;

                           
                       procedure Allow_Profile( Address : int64 ; Access : integer ) ;

                       procedure Allow_Watch( Address : int64 ; Access : integer ) ;
                end ;

    TTest_Output_Component = class( TComponent )
                              public
                                Enabled : boolean ;
                                _Address : int64 ;
                                _Value : longint ;
                                _Size : longint ;

                              public
                                function Facility_Code : longint ; override ;

                                function Initialize( UI : TUI_Interface ) : TUnified_Exception ;  override ;

                                function Terminate : TUnified_Exception ; override ;

                                function Child_Component( Index : longint ) : TComponent ; override ;

                                function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                function Component_Type : longint ; override ;

                                function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

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
                                    Memory : boolean ) : boolean ; override ;

                                function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Set_Access_Mode( Low, High : int64 ;
                                    Memory : boolean ; Typ : longint ) : TUnified_Exception ;
                                    override ;

                                procedure Set_Profiling( _On, Children : boolean ) ; override ;

                                procedure Set_Read_Latency( Value : longint ) ; override ;

                                function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                procedure Set_Write_Latency( Value : longint ) ;  override ;

                                procedure Show_Status ; override ;

                                function Version : integer ; override ;

                                procedure Wake ; override ;

                                function Write( Address : int64 ; Value, Size : longint ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; Memory : boolean ) : TUnified_Exception ; override ;
                             end ;

    TTest_Input_Component = class( TComponent )
                              public
                                Enabled : boolean ;
                                _Address : int64 ;
                                _Value : longint ;
                                _Size : longint ;

                              public
                                function Facility_Code : longint ; override ;

                                function Initialize( UI : TUI_Interface ) : TUnified_Exception ;  override ;

                                function Terminate : TUnified_Exception ; override ;

                                function Child_Component( Index : longint ) : TComponent ; override ;

                                function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                function Component_Type : longint ; override ;

                                function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

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
                                    Memory : boolean ) : boolean ; override ;

                                function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Set_Access_Mode( Low, High : int64 ;
                                    Memory : boolean ; Typ : longint ) : TUnified_Exception ;
                                    override ;

                                procedure Set_Profiling( _On, Children : boolean ) ; override ;

                                procedure Set_Read_Latency( Value : longint ) ; override ;

                                function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                procedure Set_Write_Latency( Value : longint ) ;  override ;

                                procedure Show_Status ; override ;

                                function Version : integer ; override ;

                                procedure Wake ; override ;

                                function Write( Address : int64 ; Value, Size : longint ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; Memory : boolean ) : TUnified_Exception ; override ;
                             end ;

procedure TTest_UI.Block( Component : TComponent ; Blocked : boolean ) ;

begin
end ;


function TTest_UI.Breakpoint_Notice( Address : int64 ; Physical : boolean ;
   CPU : TCPU ) : boolean ;

begin
    ShowMessage( 'Breakpoint notice' ) ;
end ;


function TTest_UI.Clock : TMaster_Clock ;

begin
    Result := Test_Clock ;
end ;


procedure TTest_UI.Log_Error( Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TTest_UI.Log_Simulated_Error( Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TTest_UI.Log_Status( Text : PChar ; Index : longint ) ;

begin
end ;


procedure TTest_UI.Profile_Notice( Address : int64 ; Access : longint ;
   Tag, Time : longint ; Component : TComponent ) ;

begin
    if( ( Address <> Profile_Address ) or ( Access <> Profile_Access ) ) then
    begin
        ShowMessage( 'Invalid profile notice' ) ;
    end ;
    Profile_Triggered := True ;
end ;


function TTest_UI.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_UI.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
   Component : TComponent ) ;

begin
    if( ( Address <> Watch_Address ) or ( Access <> Watch_Access ) ) then
    begin
        ShowMessage( 'Invalid watchpoint notice' ) ;
    end ;
    Watch_Triggered := True ;
end ;


procedure TTest_UI.Allow_Profile( Address : int64 ; Access : integer ) ;

begin
    Profile_Address := Address ;
    Profile_Access := Access ;
end ;


procedure TTest_UI.Allow_Watch( Address : int64 ; Access : integer ) ;

begin
    Watch_Address := Address ;
    Watch_Access := Access ;
end ;


function TTest_Output_Component.Facility_Code : longint ;

begin
    Result := 0 ;
end ;


function TTest_Output_Component.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Terminate : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Child_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Child_Component' ) ;
end ;


function TTest_Output_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Clear_Watch' ) ;
end ;


function TTest_Output_Component.Component_Type : longint ;

begin
    ShowMessage( 'Call to component.Component_Type' ) ;
end ;


function TTest_Output_Component.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Input' ) ;
end ;


function TTest_Output_Component.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Output' ) ;
end ;


function TTest_Output_Component.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Deposit' ) ;
end ;


function TTest_Output_Component.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Input' ) ;
end ;


function TTest_Output_Component.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Output' ) ;
end ;


function TTest_Output_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Examine' ) ;
end ;


function TTest_Output_Component.Get_Access_Mode( Address : int64 ;
    Memory : boolean ) : longint ;

begin
    ShowMessage( 'Call to component.Get_Access_Mode' ) ;
end ;


function TTest_Output_Component.Get_Profiling : boolean ;

begin
    ShowMessage( 'Call to component.Get_Profiling' ) ;
end ;


function TTest_Output_Component.Get_Read_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Read_Latency' ) ;
end ;


function TTest_Output_Component.Get_Write_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Write_Latency' ) ;
end ;


function TTest_Output_Component.Input_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Input_Component' ) ;
end ;


function TTest_Output_Component.Name : PChar ;

begin
    ShowMessage( 'Call to component.Name' ) ;
end ;


function TTest_Output_Component.Output_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Output_Component' ) ;
end ;


function TTest_Output_Component.Read( Address : int64 ; Size : longint ;
    Memory : boolean ) : boolean ;

begin
end ;


function TTest_Output_Component.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Set_Access_Mode( Low, High : int64 ;
    Memory : boolean ; Typ : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Access_Mode' ) ;
end ;


procedure TTest_Output_Component.Set_Profiling( _On, Children : boolean ) ;

begin
    ShowMessage( 'Call to component.Set_Profiling' ) ;
end ;


procedure TTest_Output_Component.Set_Read_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Read_Latency' ) ;
end ;


function TTest_Output_Component.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Watchpoint' ) ;
end ;


procedure TTest_Output_Component.Set_Write_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Write_Latency' ) ;
end ;


procedure TTest_Output_Component.Show_Status ;

begin
    ShowMessage( 'Call to component.Show_Status' ) ;
end ;


function TTest_Output_Component.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_Output_Component.Wake ;

begin
    ShowMessage( 'Call to component.Wake' ) ;
end ;


function TTest_Output_Component.Write( Address : int64 ; Value, Size : longint ;
    Memory : boolean ) : TUnified_Exception ;

begin
    if( Enabled and Memory ) then
    begin
        _Address := Address ;
        _Size := Size ;
        _Value := Value ;
    end else
    begin
        ShowMessage( 'Unexpected Write to output component' ) ;
    end ;
end ;


function TTest_Output_Component.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; Memory : boolean ) : TUnified_Exception ;

begin
    Write( Address, ord( Value[ 0 ] ), Size, Memory ) ;
end ;


function TTest_Input_Component.Facility_Code : longint ;

begin
    Result := 0 ;
end ;



function TTest_Input_Component.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Terminate : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Child_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Child_Component' ) ;
end ;


function TTest_Input_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Clear_Watch' ) ;
end ;


function TTest_Input_Component.Component_Type : longint ;

begin
    ShowMessage( 'Call to component.Component_Type' ) ;
end ;


function TTest_Input_Component.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Input' ) ;
end ;


function TTest_Input_Component.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Output' ) ;
end ;


function TTest_Input_Component.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Deposit' ) ;
end ;


function TTest_Input_Component.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Input' ) ;
end ;


function TTest_Input_Component.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Output' ) ;
end ;


function TTest_Input_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Examine' ) ;
end ;


function TTest_Input_Component.Get_Access_Mode( Address : int64 ;
    Memory : boolean ) : longint ;

begin
    ShowMessage( 'Call to component.Get_Access_Mode' ) ;
end ;


function TTest_Input_Component.Get_Profiling : boolean ;

begin
    ShowMessage( 'Call to component.Get_Profiling' ) ;
end ;


function TTest_Input_Component.Get_Read_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Read_Latency' ) ;
end ;


function TTest_Input_Component.Get_Write_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Write_Latency' ) ;
end ;


function TTest_Input_Component.Input_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Input_Component' ) ;
end ;


function TTest_Input_Component.Name : PChar ;

begin
    ShowMessage( 'Call to component.Name' ) ;
end ;


function TTest_Input_Component.Output_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Output_Component' ) ;
end ;


function TTest_Input_Component.Read( Address : int64 ; Size : longint ;
    Memory : boolean ) : boolean ;

begin
end ;


function TTest_Input_Component.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Set_Access_Mode( Low, High : int64 ;
    Memory : boolean ; Typ : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Access_Mode' ) ;
end ;


procedure TTest_Input_Component.Set_Profiling( _On, Children : boolean ) ;

begin
    ShowMessage( 'Call to component.Set_Profiling' ) ;
end ;


procedure TTest_Input_Component.Set_Read_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Read_Latency' ) ;
end ;


function TTest_Input_Component.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Watchpoint' ) ;
end ;


procedure TTest_Input_Component.Set_Write_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Write_Latency' ) ;
end ;


procedure TTest_Input_Component.Show_Status ;

begin
    ShowMessage( 'Call to component.Show_Status' ) ;
end ;


function TTest_Input_Component.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_Input_Component.Wake ;

begin
    ShowMessage( 'Call to component.Wake' ) ;
end ;


function TTest_Input_Component.Write( Address : int64 ; Value, Size : longint ;
    Memory : boolean ) : TUnified_Exception ;

begin
    if( Enabled and Memory ) then
    begin
        _Address := Address ;
        _Size := Size ;
        _Value := Value ;
    end else
    begin
        ShowMessage( 'Unexpected Write to output component' ) ;
    end ;
end ;


function TTest_Input_Component.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Write_String' ) ;
end ;


type TTest_Streamer = class( TCOM_Stream )
        public
            S : TMemoryStream ;

            constructor Create ;
            function At_End : boolean ; override ;
            function Facility_Code : longint ; override ;
            function Read( var Buffer ; var Size : longint ) : TUnified_Exception ;
                override ;
            function Read_Line( var Buffer ; var Size : longint ) : TUnified_Exception ;
                override ;
            function Seek( Position : longint ) : TUnified_Exception ; override ;
            function Write( var Buffer ; size : longint ) : TUnified_Exception ; override ;
            function Write_Line( Buffer : PChar ) : TUnified_Exception ; override ;
      end ;

constructor TTest_Streamer.Create ;

begin
    inherited Create ;

    S := TMemoryStream.Create ;
end ;


function TTest_Streamer.At_End : boolean ;

begin
    Result := ( S.Position >= S.Size ) ;
end ;


function TTest_Streamer.Facility_Code : longint ;

begin
    Result := -1 ;
end ;


function TTest_Streamer.Read( var Buffer ; var Size : longint ) : TUnified_Exception ;

begin
    S.ReadBuffer( Buffer, Size ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Read_Line( var Buffer ; var Size : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to Read_Line' ) ;
end ;


function TTest_Streamer.Seek( Position : longint ) : TUnified_Exception ;

begin
    S.Seek( 0, soFromBeginning ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Write( var Buffer ; size : longint ) : TUnified_Exception ;

begin
    S.WriteBuffer( Buffer, Size ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Write_Line( Buffer : PChar ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to Write_Line' ) ;
end ;


procedure Test_Unit ;

var Address, Count, Temp : int64 ;
    E : TUnified_Exception ;
    Test : TIMM860 ;
    Test_UI : TTest_UI ;
    Test_Output_Component : TTest_Output_Component ;
    Test_Input_Component : TTest_Input_Component ;
    Test_Stream : TTest_Streamer ;

    procedure Check_E ;

    begin
        if( E.Code <> 0 ) then
        begin
            ShowMessage( 'Error: ' + inttostr( E.Code ) ) ;
        end ;
    end ;

var Dummy, Size, Work : integer ;

begin
    { Coverage test... }

    { Setup... }
    Test := TIMM860.Create ;
    Test_UI := TTest_UI.Create ;
    Test_UI.Allow_Profile( -1, 0 ) ;
    Test_Output_Component := TTest_Output_Component.Create ;
    Test_Output_Component.Enabled := False ;
    Test_Input_Component := TTest_Input_Component.Create ;
    Test_Input_Component.Enabled := False ;
    Test_Stream := TTest_Streamer.Create ;
    E := Test.Initialize( Test_UI ) ;
    Check_E ;

    { Check for invalid conditions... }
    if( Test.Output_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Input_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;
    E := Test.Connect_Output( Test_Output_Component ) ;
    Check_E ;
    E := Test.Connect_Input( Test_Input_Component ) ;
    Check_E ;
    E := Test.Deposit( 0, 8, @Dummy, False ) ;
    Check_E ;
    Size := 8 ;
    E := Test.Examine( 0, Size, @Work, False ) ;
    if( E.COde = 0 ) then
    begin
        ShowMessage( 'No error on non-memory' ) ;
    end ;
    Work := 0 ;
    E := Test.Examine( 0, Size, @Work, True ) ;
    Check_E ;
    if( Work <> 255 ) then
    begin
        ShowMessage( 'Invalid examine address' ) ;
    end ;
    Test.Save_Contents( Test_Stream ) ;
    Test_Stream.Seek( 0 ) ;

    { Test reads/writes... }
    for Dummy := 0 to 255 do
    begin
        E := Test.Deposit( Dummy * 2, 8, @Dummy, True ) ;
        Check_E ;
    end ;
    Size := 8 ;
    E := Test.Examine( 1024, Size, @Work, False ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on accessing non-memory' ) ;
    end ;
    for Dummy := 0 to 255 do
    begin
        Size := 8 ;
        Work := 0 ;
        E := Test.Examine( Dummy * 2, Size, @Work, True ) ;
        Check_E ;
        if( Size <> 8 ) then
        begin
            ShowMessage( 'Illegal size returned from Examine' ) ;
        end ;
        if( Work <> Dummy ) then
        begin
            ShowMessage( 'Examine doesn''t match deposit' ) ;
        end ;
    end ;

    { Misc other tests... }
    if( Test.Get_Access_Mode( 0, False ) <> Access_None ) then
    begin
        ShowMessage( 'Invalid access mode returned' ) ;
    end ;
    if( Test.Name <> 'CEF Generic memory' ) then
    begin
        ShowMessage( 'Invalid name returned' ) ;
    end ;
    E := Test.Set_Address_Range( 0, 255 ) ;
    Check_E ;
    E := Test.Set_Address_Range( 255, 0 ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on Invalid set_address_range' ) ;
    end ;
    E := Test.Examine( 1024, Size, @Work, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on accessing memory out of range' ) ;
    end ;
    if( Test.Get_Profiling = True ) then
    begin
        ShowMessage( 'Bad profiling return' ) ;
    end ;
    Test.Set_Profiling( True, True ) ;
    if( Test.Get_Profiling <> True ) then
    begin
        ShowMessage( 'Bad profiling return' ) ;
    end ;
    if( Test.Get_Read_Latency <> 0 ) then
    begin
        ShowMessage( 'Invalid default read latency' ) ;
    end ;
    if( Test.Get_Write_Latency <> 0 ) then
    begin
        ShowMessage( 'Invalid default write latency' ) ;
    end ;
    Test.Set_Read_Latency( 12 ) ;
    if( Test.Get_Read_Latency <> 12 ) then
    begin
        ShowMessage( 'Read latency did not change' ) ;
    end ;
    Test.Set_Write_Latency( 34 ) ;
    if( Test.Get_Write_Latency <> 34 ) then
    begin
        ShowMessage( 'Write latency did not change' ) ;
    end ;
    if( Test.Get_Access_Mode( 0, True ) <> Access_All ) then
    begin
        ShowMessage( 'Invalid default access mode' ) ;
    end ;

    { Test access failures... }
    E := Test.Set_Access_Mode( 0, 255, False, Access_None ) ;
    Check_E ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_None ) ;
    Check_E ;
    Size := 8 ;
    E := Test.Examine( 0, Size, @Work, True ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, False ) ; { Non memory write }
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ; { Memory write }
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Read ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Write ) ;
    if( Test.Read( 0, 8, True ) ) then
    begin
        ShowMessage( 'Read with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Execute ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;

    { Test I/O components... }
    E := Test.Set_Access_Mode( 0, 255, True, Access_RW ) ;
    Check_E ;
    E := Test.Connect_Input( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on nil input connection' ) ;
    end ;
    E := Test.Connect_Output( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on nil output connection' ) ;
    end ;
    Test_UI.Allow_Profile( 1, Access_Write ) ;
    Test_Output_Component.Enabled := True ;
    Test_Input_Component.Enabled := True ;
    if( Test.Output_Component( 0 ) <> Test_Output_Component ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    E := Test.Write( 1, 1, 8, True ) ;
    if( E.Code <> 0 ) then
    begin
        ShowMessage( 'Error on write' ) ;
    end ;
    if( not Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile not triggered' ) ;
    end ;
    if( Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watch incorrectly triggered' ) ;
    end ;
    if( Test.Read( 1, 8, False ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    Test_UI.Allow_Profile( 1, Access_Read ) ;
    if( not Test.Read( 1, 8, True ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    if(
        ( Test_Output_Component._Address <> 1 )
        or
        ( Test_Output_Component._Size <> 8 )
        or
        ( Test_Output_Component._Value <> 1 )
      ) then
    begin
        ShowMessage( 'Read failure' ) ;
    end ;
    Test_UI.Allow_Profile( -1, 0 ) ;
    Test.Set_Profiling( False, False ) ;
    Test_UI.Profile_Triggered := False ;

    { Test watchpoints... }
    E := Test.Set_Watchpoint( 0, False, Access_Write ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 0, True, Access_Read ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 0, True, Access_Write ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 1, True, Access_Read ) ;
    Check_E ;
    Test_UI.Allow_Watch( 0, Access_Write ) ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code <> 0 ) then
    begin
        ShowMessage( 'Error on write' ) ;
    end ;
    if( not Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watchpoint not triggered' ) ;
    end ;
    if( Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile incorrectly triggered' ) ;
    end ;
    Test_UI.Watch_Triggered := False ;
    Test_UI.Allow_Watch( 1, Access_Read ) ;
    if( not Test.Read( 1, 8, True ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    if( not Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watchpoint not triggered' ) ;
    end ;
    if( Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile incorrectly triggered' ) ;
    end ;
    E := Test.Clear_Watchpoint( 1, False, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 0, True, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 0, True, Access_Write ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 1, True, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 1, True, Access_Write ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from clearing non-existant watchpoint' ) ;
    end ;

    { Test disconnections... }
    E := Test.Disconnect_Output( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from Disconnecting nil output' ) ;
    end ;
    E := Test.Disconnect_Input( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from Disconnecting nil input' ) ;
    end ;
    E := Test.Disconnect_Output( Test_Output_Component ) ;
    Check_E ;
    E := Test.Disconnect_Input( Test_Input_Component ) ;
    Check_E ;
    if( Test.Output_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Output_Component( 1 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Input_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;
    if( Test.Input_Component( 1 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;

    { Test save/restore contents... }
    for Dummy := 0 to 255 do
    begin
        Work := Dummy div 2 ;
        E := Test.Deposit( Dummy, 8, @Work, True ) ;
        Check_E ;
    end ;
    E := Test.Save_Contents( Test_Stream ) ;
    Check_E ;
    Work := 255 ;
    for Dummy := 0 to 255 do
    begin
        E := Test.Deposit( Dummy, 8, @Work, True ) ;
        Check_E ;
    end ;
    Test_Stream.Seek( 0 ) ;
    E := Test.Restore_Contents( Test_Stream ) ;
    Check_E ;
    Size := 8 ;
    for Dummy := 0 to 255 do
    begin
        Work := 0 ;
        E := Test.Examine( Dummy, Size, @Work, True ) ;
        Check_E ;
        if( Work <> Dummy div 2 ) then
        begin
            ShowMessage( 'Save/Restore failure' ) ;
        end ;
    end ;

    { Test save/restore state... }
    E := Test_Stream.Seek( 0 ) ;
    Check_E ;
    Test.Save_State( Test_Stream ) ;
    Test.Set_Address_Range( 128, 1024 ) ;
    Test.Set_Read_Latency( 12345 ) ;
    Test.Set_Write_Latency( 6789 ) ;
    Test.Set_Profiling( True, True ) ;
    Test_Stream.Seek( 0 ) ;
    E := Test.Restore_State( Test_Stream ) ;
    Check_E ;
    if( ( Test._Low <> 0 ) or ( Test._High <> 255 ) ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;
    if( Test.Get_Profiling ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;
    if( ( Test.Get_Read_Latency <> 12 ) or ( Test.Get_Write_Latency <> 34 ) ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;

    { Test termination... }
    E := Test.Terminate ;
    Check_E ;

    { Cleanup... }
    E := Test.Terminate ;
    Check_E ;
end ;
{$ENDIF}


const Memory_Facility_Name : PChar = 'IMM860' ;


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

