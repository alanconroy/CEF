{$N+}
{
        Program Name : DL11W
        Package Name : CEF
        Purpose      : DEC DL11-W line clock component for CEF
        Institution  : 
        Date Written : 5-Mar-2008
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

	  This unit implements the line clock portion of a DEC DL11-W UNIBUS
        single-line serial interface.


        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan


        Conditionals:
            TEST    Causes test to run upon unit initialization.
}

unit DL_11W ;

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
     _UEHDefs, { TUEC }

     // CEF...
     CEF, { TMemory }
     _CEFUtil ; // TCEF_Watchpoint

const CEFDL11_Facility = -1 ;
      CEFDL11Err_Success = 0 ;
      CEFDL11Err_Invalid_Range = 1 ;
      CEFDL11Err_Component_Not_Found = 2 ;
      CEFDL11Err_No_Matching_Watchpoint = 3 ;
      CEFDL11Err_Access_Violation = 4 ;
      CEFDL11Err_Address_Out_Of_Range = 5 ;
      CEFDL11Err_Invalid_Component = 6 ;

type TDL11W_IO = class;

     TDL11W = class( TComponent )
        private { Instance data... }
            Access_Mode : integer ;
            Inputs, Outputs : TList ;
            Profiling : boolean ; { True if profiling accesses }
            _UI : TUI_Interface ;
            Watchpoint_List : TCEF_Watchpoint_Manager ;
            _Tag : longint ;
            _Parent : TComponent ;
            IO : TDL11W_IO ;
            Temp_Signal_Name : string ;
            _Hertz : integer ; // Line frequency

        private // Interrupt data...
            Pending_Vector : integer ; // The vector to respond with when asked
            Send_Vector : boolean ; // True to respond to Bus data requiest
            Wait_For_Interrupt : boolean ; // True if waiting for interrupt acknowlegement

        private // Registers...
            LKS : word ;

        private // Internal utility routines...
            function Default_Input : TComponent ;
            function Default_Output : TComponent ;
            procedure Init ;
            function Set_Error( Code : integer ) : TUEC ;
            procedure Vector( V : word ) ;
            function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

	public { Public instance data... }
            _Serial_Number : integer ;

        public { API... }
            function Facility_Code : longint ; override ;

            function Initialize( UI : TUI_Interface ) : TUEC ; override ;

            function Terminate : TUEC ; override ;

            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUEC ; override ;

            function Component_Type : longint ; override ;

            function Connect_Input( Component : TComponent ) : TUEC ; override ;

            function Connect_Output( Component : TComponent ) : TUEC ; override ;

            function Debugger : TDebug_Interface ; override ;

            function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                Memory : boolean ) : TUEC ; override ;

            function Disconnect_Input( Component : TComponent ) : TUEC ;
                override ;

            function Disconnect_Output( Component : TComponent ) : TUEC ;
                override ;

            function Examine( Address : int64 ; var Size : longint ;
                Buffer : pointer ; Memory : boolean ) : TUEC ; override ;

            function Get_Access_Mode( Address : int64 ;
                Memory : boolean ) : longint ; override ;

            function Get_Profiling : boolean ; override ;

            function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                override ;

            function Input_Component( Index : longint ) : TComponent ; override ;

            function Memory : TMemory ; override ;

            function Name : PChar ; override ;

            function Output_Component( Index : longint ) : TComponent ;
                override ;

            function Read( Address : int64 ; Size : longint ;
                IO_Type : longint ) : boolean ; override ;

            function Restore_Contents( Stream : TCOM_Stream ) : TUEC ; override ;

            function Restore_State( Stream : TCOM_Stream ) : TUEC ; override ;

            function Save_Contents( Stream : TCOM_Stream ) : TUEC ; override ;

            function Save_State( Stream : TCOM_Stream ) : TUEC ; override ;

            function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                Typ : longint ) : TUEC ; override ;

            procedure Set_Signal( Name : PChar ; State : boolean ) ; override ;

            procedure Set_Profiling( _On, Children : boolean ) ;  override ;

            procedure Set_Up( P : PChar ) ; override ;

            function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUEC ; override ;

            procedure Show_Status ; override ;

            function Signal_Count : longint ; override ;

            function Signal_Name( Index : longint ) : PChar ; override ;

            function Signal_Out( Index : longint ) : boolean ; override ;

            function Signal_Active_Low( Index : longint ) : boolean ; override ;

            function Signal_Index( Name : PChar ) : integer ; override ;

            procedure Wake ; override ;
            
            function Write( Address : int64 ; Value, Size : longint ;
                IO_Type : longint ) : TUEC ; override ;

            procedure Set_Tag( Value : longint ) ; override ;

            function Get_Tag : longint ; override ;

            function Get_Parent : TComponent ; override ;

            procedure Set_Parent( Component : TComponent ) ; override ;
     end ;

     TDL11W_IO = class( TMemory )
                     public // API...
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
     ErrMastr, // Register_Error_Interface
     Num1s,
     Parse ; // TString_Parser

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ;
    external 'CEF_Util.dll' ;



{ TCEF_DL11_Debugger methods... }

type TCEF_DL11_Debugger = class( TText_Debugger )
                                private
                                    _IO : TDL11W ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property IO : TDL11W
                                                 read _IO
                                                 write _IO ;
                            end ;

function TCEF_DL11_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
        3 : I.Title := Pchar( 'Watchpoint_List = ' + Pointer_To_String( pointer( IO.Watchpoint_List ) ) ) ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TCEF_DL11_Debugger.Count : longint ;

begin
    Result := 7 ;
end ;


{ TDL11W methods... }

function TDL11W.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TDL11W.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


procedure TDL11W.Init ;

begin
    LKS := 128 ;
    if( _UI.Clock <> nil ) then
    begin
        _UI.Clock.Block( self, 1000000000 div _Hertz ) ; // Hertz converted to nanoseconds
    end ;
end ;


function TDL11W.Set_Error( Code : integer ) : TUEC ;

begin
    Result.Facility := Facility_Code ;
    Result.Code := Code ;
end ;


procedure TDL11W.Vector( V : word ) ;

var Component : TComponent ;
    Loop : integer ;

begin
    if( Wait_For_Interrupt ) then
    begin
        exit ; // Already waiting for an interrupt to be acknowledged
    end ;
    Wait_For_Interrupt := True ;
    Pending_Vector := V ;
    for Loop := 0 to Outputs.Count - 1 do
    begin
        Component := TComponent( Outputs[ Loop ] ) ;
        Component.Set_Signal( 'BR6', True ) ;
    end ;
    _UI.Signal_Change_Notice( self, 0, True ) ; // BR6
end ;


function TDL11W.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_List.Watchpoint_At( Address ) ;
end ;


{ API... }

function TDL11W.Facility_Code : longint ;

begin
    Facility_Code := CEFDL11_Facility ;
end ;


function TDL11W.Initialize( UI : TUI_Interface ) : TUEC ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    Profiling := False ;
    _UI := UI ;
    Watchpoint_List := Get_Watchpoint_Manager ;
    Access_Mode := Access_RW or Access_Execute ;
    IO := TDL11W_IO.Create ;
    _Hertz := 60 ; // Default to 60 Hz.

    Initialize := Set_Error( CEFDL11Err_Success ) ;
    Init ;
end ; { TDL11W.Initialize }


function TDL11W.Terminate : TUEC ;

begin
    Watchpoint_List.Terminate ;
    Watchpoint_List := nil ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    IO.Free ;
    IO := nil ;
    Terminate := Set_Error( CEFDL11Err_Success ) ;
end ; { TDL11W.Terminate }



function TDL11W.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUEC ;

begin
//    Clear_Watchpoint := Set_Error( CEFDL11Err_Success ) ;
    if( not Memory ) then
    begin
        Result := Watchpoint_List.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( CEFDL11Err_No_Matching_Watchpoint ) ;
    end ;
end ;


function TDL11W.Component_Type : longint ;

begin
    Component_Type := Component_Type_Memory ;
end ;


function TDL11W.Connect_Input( Component : TComponent ) : TUEC ;

var Code : longint ;
    P : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := Set_Error( CEFDL11Err_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        P := integer( Component ) ;
        Parent.Child_Notification( self, Code, P ) ;
    end ;
    Connect_Input := Set_Error( CEFDL11Err_Success ) ;
end ;


function TDL11W.Connect_Output( Component : TComponent ) : TUEC ;

var Code : longint ;
    P : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := Set_Error( CEFDL11Err_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        P := integer( Component ) ;
        Parent.Child_Notification( self, Code, P ) ;
    end ;
    Connect_Output := Set_Error( CEFDL11Err_Success ) ;
end ;


function TDL11W.Debugger : TDebug_Interface ;

begin
    Result := TCEF_DL11_Debugger.Create ;
    TCEF_DL11_Debugger( Result ).IO := Self ;
end ;


function TDL11W.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUEC ;

var _Buffer : PChar ;
    Value : word ;

begin
    if( ( not Memory ) or ( Address <> 261990 ) or ( Size = 0 ) ) then
    begin
        Deposit := Set_Error( CEFDL11Err_Address_Out_Of_Range ) ;
        exit ;
    end ;
    _Buffer := PChar( Buffer ) ;
    Value := ord( _Buffer[ 0 ] ) and $C0 ;
    try
        LKS := Value ;
    except
    end ;
    Deposit := Set_Error( CEFDL11Err_Success ) ;
end ; { TDL11W.Deposit }


function TDL11W.Disconnect_Input( Component : TComponent ) : TUEC ;

var Code : longint ;
    P : int64 ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
        Disconnect_Input := Set_Error( CEFDL11Err_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := Set_Error( CEFDL11Err_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            P := integer( Component ) ;
            Parent.Child_Notification( self, Code, P ) ;
        end ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TDL11W.Disconnect_Output( Component : TComponent ) : TUEC ;

var Code : longint ;
    P : int64 ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
        Disconnect_Output := Set_Error( CEFDL11Err_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := Set_Error( CEFDL11Err_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            P := integer( Component ) ;
            Parent.Child_Notification( self, Code, P ) ;
        end ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TDL11W.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUEC ;

var _Buffer : PChar ;
    Count : integer ;
    Value : integer ;

begin
    if( ( not Memory ) or ( Address <> 261990 ) or ( Size = 0 ) or ( Size > 16 ) ) then
    begin
        Examine := Set_Error( CEFDL11Err_Address_Out_Of_Range ) ;
        exit ;
    end ;
    Count := ( Size + 7 ) div 8 ; { Number of bytes to examine }
    fillchar( Buffer^, Count, 0 ) ;
    _Buffer := Buffer ;

    // Retrieve data...
    try
        Value := LKS ;
        _Buffer[ 0 ] := char( Value ) ;
    except
    end ;
    Examine := Set_Error( CEFDL11Err_Success ) ;
end ; { TDL11W.Examine }


function TDL11W.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    if( ( not Memory ) and ( Address <> 261990 ) ) then
    begin
        Get_Access_Mode := Access_Mode ;
    end else
    begin
        Get_Access_Mode := Access_None ;
    end ;
end ;


function TDL11W.Get_Profiling : boolean ;

begin
    Get_Profiling := Profiling ;
end ;


function TDL11W.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

begin
    Result := True ;
    if( string( Name ) = 'BR6' ) then
    begin
        State := Wait_For_Interrupt ;
    end else
    begin
        Result := False ;
    end ;
end ;


function TDL11W.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


function TDL11W.Memory : TMemory ;

begin
    Result := IO ;
end ;


const _Name : PChar = 'DEC DL11W'#0 ;

function TDL11W.Name : PChar ;

begin
    Name := _Name ;
end ;


function TDL11W.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


function TDL11W.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

var Buffer : integer ;
    Watch : TCEF_Watchpoint ;

    procedure Write_Bus( Size : integer ) ;

    var Loop : integer ;

    begin
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
    end ;

begin // TDL11W.Read
    Read := False ;
    if(
        ( IO_Type = IO_Type_Memory )
        and
        ( ( Address = 0 ) or ( Address = 1 ) )
        and
        Send_Vector
        and
        ( Size < 17 )
      ) then
    begin
        case Address of
            0 : Buffer := 64 ;
            1 : Buffer := 0 ;
        end ;
        Write_Bus( Size ) ;
        Result := True ;
        exit ;
    end ;
    if(
        ( Address = 261990 )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Read ) = 0 ) then
        begin
            exit ;
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

	Read := True ;
        Buffer := 0 ;
        Examine( Address, Size, PChar( @Buffer ), True ) ;
        Write_Bus( Size ) ;
    end ;
end ; // TDL11W.Read


function TDL11W.Restore_Contents( Stream : TCOM_Stream ) : TUEC ;

var S : longint ;
    Loop : integer ;
    Value : byte ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    S := sizeof( Loop ) ;
    Stream.Read( Loop, S ) ;
    S := 1 ;
    Stream.Read( Value, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    Deposit( 261990, 1, @Value, True ) ;
end ;


function TDL11W.Restore_State( Stream : TCOM_Stream ) : TUEC ;

var S : longint ;

begin
    S := sizeof( Profiling ) ;
    Stream.Read( Profiling, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
end ;


function TDL11W.Save_Contents( Stream : TCOM_Stream ): TUEC ;

var Loop : int64 ;
    S : longint ;
    Value : integer ;

begin { TDL11W.Save_Contents }
    fillchar( Result, sizeof( Result ), 0 ) ;

    { Write contents... }
    Loop := 0 ;
    Stream.Write( Loop, sizeof( Loop ) ) ;
    Stream.Last_Error( Result ) ;
    if( Result.Code <> 0 ) then
    begin
        exit ;
    end ;
    S := 1 ;
    Examine( 261990, S, @Value, True ) ;
    Stream.Write( Value, 1 ) ;
    Stream.Last_Error( Result ) ;
end ; { TDL11W.Save_Contents }


function TDL11W.Save_State( Stream : TCOM_Stream ) : TUEC ;

begin
    Stream.Write( Profiling, sizeof( Profiling ) ) ;
    Stream.Last_Error( Result ) ;
    if( Result.Code <> 0 ) then
    begin
        exit ;
    end ;
end ;


function TDL11W.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUEC ;

begin
    if( ( not Memory ) and ( 261990 = High ) and ( Low = 261990 ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := Set_Error( CEFDL11Err_Success ) ;
end ;


procedure TDL11W.Set_Signal( Name : PChar ; State : boolean ) ;

var S : string ;

begin
    S := string( Name ) ;
    if( S = 'BG6' ) then
    begin
        if( Wait_for_Interrupt ) then
        begin
            Wait_For_Interrupt := False ;
            Send_Vector := True ;
        end else
        begin
            Send_Vector := False ; // Acknowleging someone else's interrupt
        end ;
    end else
    if( S = 'INIT' ) then
    begin
        Init ;
    end ;
end ;


procedure TDL11W.Set_Profiling( _On, Children : boolean ) ;

begin
    Profiling := _On ;
end ;


procedure TDL11W.Set_Up( P : PChar ) ;

var H : integer ;
    Parser : TString_Parser ;
    S : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'HERTZ' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                H := strtoint( S ) ;
                if( H < 1 ) then
                begin
                    H := 1 ;
                end ;
                _Hertz := H ;
            except
            end ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ;


function TDL11W.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUEC ;

begin
    Set_Watchpoint := Set_Error( CEFDL11Err_Success ) ;
    if( ( not Memory ) and ( 261990 = Address ) ) then
    begin
        Watchpoint_List.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TDL11W.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TDL11W.Signal_Count : longint ;

begin
    Result := 1 ;
end ;


function TDL11W.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := '' ;
    case Index of
        0 : Temp_Signal_Name := 'BR6' ;
        1 : Temp_Signal_Name := 'INIT' ;
        2 : Temp_Signal_Name := 'BG6' ;
    end ;
    Result := PChar( Temp_Signal_Name ) ;
end ;


function TDL11W.Signal_Out( Index : longint ) : boolean ;

begin
    Result := ( Index = 0 ) ;
end ;


function TDL11W.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TDL11W.Signal_Index( Name : PChar ) : integer ;

begin
    if( string( Name ) = 'BR6' ) then
    begin
        Result := 0 ;
    end else
    if( string( Name ) = 'INIT' ) then
    begin
        Result := 1 ;
    end else
    if( string( Name ) = 'BG6' ) then
    begin
        Result := 2 ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


procedure TDL11W.Wake ;

begin
    // Line clock tick.  QUESTION: Does this generate an interrupt if the bit is already set?
    LKS := LKS or 128 ;
    if( ( LKS and 64 ) <> 0 ) then
    begin
        Vector( 64 ) ;
    end ;
    if( _UI.Clock <> nil ) then
    begin
        _UI.Clock.Block( self, 1000000000 div _Hertz ) ; // Hertz converted to nanoseconds
    end ;
end ;


function TDL11W.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUEC ;

var Watch : TCEF_Watchpoint ;

begin
    if(
        ( Address = 261990 )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Write ) = 0 ) then
        begin
            Write := Set_Error( CEFDL11Err_Access_Violation ) ;
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
        if( ( LKS and Value and 128 ) = 0 ) then
        begin
            Value := Value and ( not 128 ) ; // Bit will stay clear
        end ;
        Write := Deposit( Address, Size, @Value, True ) ;
    end else
    begin
	Write := Set_Error( CEFDL11Err_Success ) ;
    end ;
end ; // TDL11W.Write


procedure TDL11W.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TDL11W.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TDL11W.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TDL11W.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


procedure TDL11W_IO.Get_Address_Range( var Low, High : int64 ) ;

begin
    Low := 261990 ;
    High := 261990 ;
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

                                function Initialize( UI : TUI_Interface ) : TUEC ;  override ;

                                function Terminate : TUEC ; override ;

                                function Child_Component( Index : longint ) : TComponent ; override ;

                                function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUEC ; override ;

                                function Component_Type : longint ; override ;

                                function Connect_Input( Component : TComponent ) : TUEC ; override ;

                                function Connect_Output( Component : TComponent ) : TUEC ; override ;

                                function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                                    Memory : boolean ) : TUEC ; override ;

                                function Disconnect_Input( Component : TComponent ) : TUEC ; override ;

                                function Disconnect_Output( Component : TComponent ) : TUEC ; override ;

                                function Examine( Address : int64 ; var Size : longint ;
                                    Buffer : pointer ; Memory : boolean ) : TUEC ; override ;

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

                                function Restore_Contents( Stream : TCOM_Stream ) : TUEC ; override ;

                                function Restore_State( Stream : TCOM_Stream ) : TUEC ; override ;

                                function Save_Contents( Stream : TCOM_Stream ) : TUEC ; override ;

                                function Save_State( Stream : TCOM_Stream ) : TUEC ; override ;

                                function Set_Access_Mode( Low, High : int64 ;
                                    Memory : boolean ; Typ : longint ) : TUEC ;
                                    override ;

                                procedure Set_Profiling( _On, Children : boolean ) ; override ;

                                procedure Set_Read_Latency( Value : longint ) ; override ;

                                function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUEC ; override ;

                                procedure Set_Write_Latency( Value : longint ) ;  override ;

                                procedure Show_Status ; override ;

                                function Version : integer ; override ;

                                procedure Wake ; override ;

                                function Write( Address : int64 ; Value, Size : longint ;
                                    Memory : boolean ) : TUEC ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; Memory : boolean ) : TUEC ; override ;
                             end ;

    TTest_Input_Component = class( TComponent )
                              public
                                Enabled : boolean ;
                                _Address : int64 ;
                                _Value : longint ;
                                _Size : longint ;

                              public
                                function Facility_Code : longint ; override ;

                                function Initialize( UI : TUI_Interface ) : TUEC ;  override ;

                                function Terminate : TUEC ; override ;

                                function Child_Component( Index : longint ) : TComponent ; override ;

                                function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUEC ; override ;

                                function Component_Type : longint ; override ;

                                function Connect_Input( Component : TComponent ) : TUEC ; override ;

                                function Connect_Output( Component : TComponent ) : TUEC ; override ;

                                function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                                    Memory : boolean ) : TUEC ; override ;

                                function Disconnect_Input( Component : TComponent ) : TUEC ; override ;

                                function Disconnect_Output( Component : TComponent ) : TUEC ; override ;

                                function Examine( Address : int64 ; var Size : longint ;
                                    Buffer : pointer ; Memory : boolean ) : TUEC ; override ;

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

                                function Restore_Contents( Stream : TCOM_Stream ) : TUEC ; override ;

                                function Restore_State( Stream : TCOM_Stream ) : TUEC ; override ;

                                function Save_Contents( Stream : TCOM_Stream ) : TUEC ; override ;

                                function Save_State( Stream : TCOM_Stream ) : TUEC ; override ;

                                function Set_Access_Mode( Low, High : int64 ;
                                    Memory : boolean ; Typ : longint ) : TUEC ;
                                    override ;

                                procedure Set_Profiling( _On, Children : boolean ) ; override ;

                                procedure Set_Read_Latency( Value : longint ) ; override ;

                                function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUEC ; override ;

                                procedure Set_Write_Latency( Value : longint ) ;  override ;

                                procedure Show_Status ; override ;

                                function Version : integer ; override ;

                                procedure Wake ; override ;

                                function Write( Address : int64 ; Value, Size : longint ;
                                    Memory : boolean ) : TUEC ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; Memory : boolean ) : TUEC ; override ;
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


function TTest_Output_Component.Initialize( UI : TUI_Interface ) : TUEC ;

begin
end ;


function TTest_Output_Component.Terminate : TUEC ;

begin
end ;


function TTest_Output_Component.Child_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Child_Component' ) ;
end ;


function TTest_Output_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUEC ;

begin
    ShowMessage( 'Call to component.Clear_Watch' ) ;
end ;


function TTest_Output_Component.Component_Type : longint ;

begin
    ShowMessage( 'Call to component.Component_Type' ) ;
end ;


function TTest_Output_Component.Connect_Input( Component : TComponent ) : TUEC ;

begin
    ShowMessage( 'Call to component.Connect_Input' ) ;
end ;


function TTest_Output_Component.Connect_Output( Component : TComponent ) : TUEC ;

begin
    ShowMessage( 'Call to component.Connect_Output' ) ;
end ;


function TTest_Output_Component.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUEC ;

begin
    ShowMessage( 'Call to component.Deposit' ) ;
end ;


function TTest_Output_Component.Disconnect_Input( Component : TComponent ) : TUEC ;

begin
    ShowMessage( 'Call to component.Disconnect_Input' ) ;
end ;


function TTest_Output_Component.Disconnect_Output( Component : TComponent ) : TUEC ;

begin
    ShowMessage( 'Call to component.Disconnect_Output' ) ;
end ;


function TTest_Output_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUEC ;

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


function TTest_Output_Component.Restore_Contents( Stream : TCOM_Stream ) : TUEC ;

begin
end ;


function TTest_Output_Component.Restore_State( Stream : TCOM_Stream ) : TUEC ;

begin
end ;


function TTest_Output_Component.Save_Contents( Stream : TCOM_Stream ) : TUEC ;

begin
end ;


function TTest_Output_Component.Save_State( Stream : TCOM_Stream ) : TUEC ;

begin
end ;


function TTest_Output_Component.Set_Access_Mode( Low, High : int64 ;
    Memory : boolean ; Typ : longint ) : TUEC ;

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
    Access : longint ) : TUEC ;

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
    Memory : boolean ) : TUEC ;

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
    Size : longint ; Memory : boolean ) : TUEC ;

begin
    Write( Address, ord( Value[ 0 ] ), Size, Memory ) ;
end ;


function TTest_Input_Component.Facility_Code : longint ;

begin
    Result := 0 ;
end ;



function TTest_Input_Component.Initialize( UI : TUI_Interface ) : TUEC ;

begin
end ;


function TTest_Input_Component.Terminate : TUEC ;

begin
end ;


function TTest_Input_Component.Child_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Child_Component' ) ;
end ;


function TTest_Input_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUEC ;

begin
    ShowMessage( 'Call to component.Clear_Watch' ) ;
end ;


function TTest_Input_Component.Component_Type : longint ;

begin
    ShowMessage( 'Call to component.Component_Type' ) ;
end ;


function TTest_Input_Component.Connect_Input( Component : TComponent ) : TUEC ;

begin
    ShowMessage( 'Call to component.Connect_Input' ) ;
end ;


function TTest_Input_Component.Connect_Output( Component : TComponent ) : TUEC ;

begin
    ShowMessage( 'Call to component.Connect_Output' ) ;
end ;


function TTest_Input_Component.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUEC ;

begin
    ShowMessage( 'Call to component.Deposit' ) ;
end ;


function TTest_Input_Component.Disconnect_Input( Component : TComponent ) : TUEC ;

begin
    ShowMessage( 'Call to component.Disconnect_Input' ) ;
end ;


function TTest_Input_Component.Disconnect_Output( Component : TComponent ) : TUEC ;

begin
    ShowMessage( 'Call to component.Disconnect_Output' ) ;
end ;


function TTest_Input_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUEC ;

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


function TTest_Input_Component.Restore_Contents( Stream : TCOM_Stream ) : TUEC ;

begin
end ;


function TTest_Input_Component.Restore_State( Stream : TCOM_Stream ) : TUEC ;

begin
end ;


function TTest_Input_Component.Save_Contents( Stream : TCOM_Stream ) : TUEC ;

begin
end ;


function TTest_Input_Component.Save_State( Stream : TCOM_Stream ) : TUEC ;

begin
end ;


function TTest_Input_Component.Set_Access_Mode( Low, High : int64 ;
    Memory : boolean ; Typ : longint ) : TUEC ;

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
    Access : longint ) : TUEC ;

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
    Memory : boolean ) : TUEC ;

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
    Size : longint ; Memory : boolean ) : TUEC ;

begin
    ShowMessage( 'Call to component.Write_String' ) ;
end ;


type TTest_Streamer = class( TCOM_Stream )
        public
            S : TMemoryStream ;

            constructor Create ;
            function At_End : boolean ; override ;
            function Facility_Code : longint ; override ;
            function Read( var Buffer ; var Size : longint ) : TUEC ;
                override ;
            function Read_Line( var Buffer ; var Size : longint ) : TUEC ;
                override ;
            function Seek( Position : longint ) : TUEC ; override ;
            function Write( var Buffer ; size : longint ) : TUEC ; override ;
            function Write_Line( Buffer : PChar ) : TUEC ; override ;
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


function TTest_Streamer.Read( var Buffer ; var Size : longint ) : TUEC ;

begin
    S.ReadBuffer( Buffer, Size ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Read_Line( var Buffer ; var Size : longint ) : TUEC ;

begin
    ShowMessage( 'Call to Read_Line' ) ;
end ;


function TTest_Streamer.Seek( Position : longint ) : TUEC ;

begin
    S.Seek( 0, soFromBeginning ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Write( var Buffer ; size : longint ) : TUEC ;

begin
    S.WriteBuffer( Buffer, Size ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Write_Line( Buffer : PChar ) : TUEC ;

begin
    ShowMessage( 'Call to Write_Line' ) ;
end ;


procedure Test_Unit ;

var Address, Count, Temp : int64 ;
    E : TUEC ;
    Test : TDL11W ;
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
    Test := TDL11W.Create ;
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


type TDL11W_EI = class( tError_Interface )
                            public
                                function Get_Facility : longint ;
                                    override ; stdcall ;
                                function Get_Name : PChar ; override ; stdcall ;
                                function Translate_Error( Code : longint ) : PChar ;
                                    override ; stdcall ;
                                procedure Done ; override ; stdcall ;
                                function Facility_Version : longint ; override ;
                                function Version : longint ; override ;
                                function Severity( Code : longint ) : longint ;
                                    override ; stdcall ;
                                function Translate_Error_Ex( Code : longint ;
                                    var Size, Typ : longint ) : PChar ;
                                    override ; stdcall ;
                  end ;
     pDL11_EI = TDL11W_EI ;


{ TDL11W_EI methods... }

function TDL11W_EI.Get_Facility : longint ;

begin
    Get_Facility := CEFDl11_Facility ;
end ;


const Memory_Facility_Name : PChar = 'DEC DL11' ;

function TDL11W_EI.Get_Name : PChar ;

begin
    Get_Name := Memory_Facility_Name ;
end ;


var Memory_Error_Text : array[ 0..127 ] of char ;

function TDL11W_EI.Translate_Error( Code : longint ) : PChar ;

var _Error : string ;

begin
    case Code of
        CEFDL11Err_Success: _Error := 'Success' ;
        CEFDL11Err_Invalid_Range: _Error := 'Invalid range' ;
        CEFDL11Err_Component_Not_Found: _Error := 'Component not found' ;
        CEFDL11Err_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        CEFDL11Err_Access_Violation: _Error := 'Access violation' ;
        CEFDL11Err_Address_Out_Of_Range: _Error := 'Address out of range' ;
        CEFDL11Err_Invalid_Component: _Error := 'Invalid component' ;
	else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    _Error := _Error + #0 ;
    move( _Error[ 1 ], Memory_Error_Text[ 0 ], length( _Error ) ) ;
    Translate_Error := @Memory_Error_Text ;
end ; { tFS_EI.Translate_Error }


procedure TDL11W_EI.Done ;

begin
    { Done by unit }
end ;


function TDL11W_EI.Facility_Version : longint ;

begin
    Facility_Version := 0 ;
end ;


function TDL11W_EI.Version : longint ;

begin
    Version := 11 ; { Compatible with UEH V1.1 }
end ;


function TDL11W_EI.Severity( Code : longint ) : longint ;

begin
    Severity := 2 ;
end ;


function TDL11W_EI.Translate_Error_Ex( Code : longint ;
    var Size, Typ : longint ) : PChar ;

begin
    Translate_Error_Ex := Translate_Error( Code ) ;
end ;


var MEI : pDL11_EI ;

initialization
    { Set up the memory error interface... }
    MEI := TDL11W_EI.Create ;
//    Register_Error_Interface( MEI ) ;
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}

finalization
    Unregister_Error_Interface( MEI.Get_Facility ) ;
    MEI.Free ;
end.

