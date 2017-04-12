{$N+}
{
        Program Name : RL11
        Package Name : CEF
        Purpose      : DEC RL11 component for CEF
        Institution  : 
        Date Written : 5-Jun-2008
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

	  This unit implements a DEC RL11 UNIBUS disk interface.


        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan


        Conditionals:
            TEST    Causes test to run upon unit initialization.
}

unit RL_11 ;

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
     _CEF,  // TUI_Interface
     CEF, { TBase_Memory }
     _CEFUtil ; // TCEF_Watchpoint

const CEFRL11_Facility = -1 ;
      CEFRL11Err_Success = 0 ;
      CEFRL11Err_Invalid_Range = 1 ;
      CEFRL11Err_Component_Not_Found = 2 ;
      CEFRL11Err_No_Matching_Watchpoint = 3 ;
      CEFRL11Err_Access_Violation = 4 ;
      CEFRL11Err_Address_Out_Of_Range = 5 ;
      CEFRL11Err_Invalid_Component = 6 ;
      CEFRL11Err_Memory_Exhausted = 7 ; { Couldn't allocate any more memory }

type TDrive_Record = record
                         Read_Only : boolean ;
                         Current_Track : integer ; // Current track = Cylinder & Head (Head is bit 0)
                     end ;

     TRL11_IO = class;

     TRL11 = class( TBase_Component )
        private { Instance data... }
            Access_Mode : integer ;
            Inputs, Outputs : TList ;
            Profiling : boolean ; { True if profiling accesses }
            _UI : TUI_Interface ;
            Watchpoint_List : TCEF_Watchpoint_Manager ;
            _Tag : longint ;
            _Parent : TComponent ;
            IO : TRL11_IO ;
            _Vector : integer ; // Vector
            Memory_Data_Latch : word ;
            Waiting_For_Data : boolean ; // True if waiting for response to memory read operation
            Do_Interrupt : boolean ; // True to do interrupt when clock next wakes us
            Disabled : boolean ;
            _Logger : TCEF_Logger ;

        private // Result strings...
            Temp_Signal_Name : string ;

        private // Drive information...
            Current_Drive : integer ;
            Drives : array[ 0..3 ] of TDrive_Record ;
            Disk_File : array[ 0..3 ] of file ;
            Disk_Filename : array[ 0..3 ] of string ; // Null if no disk loaded

        private // Interrupt data...
            Pending_Vector : integer ; // The vector to respond with when asked
            Send_Vector : boolean ; // True to respond to Bus data requiest
            Wait_For_Interrupt : boolean ; // True if waiting for interrupt acknowlegement

        private // Registers...
            CSR : word ;
            BAR : word ;
            DAR : word ;
            MPR : array[ 0..2 ] of word ;

        private // Internal utility routines...
            procedure Clear_Interrupt ;
            function Default_Input : TComponent ;
            function Default_Output : TComponent ;
            procedure Init ;
            procedure Read_Bus( Address : int64 ; var Buffer : word ;
                Size : integer ; var Failure : boolean ) ;
            procedure Vector( V : word ) ;
            function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;
            procedure Write_Bus( Address : int64 ; Buffer : word ;
                Size : integer ) ;

	    protected
            function Translate_Error( Code : longint ) : string ; override ;

        public { Public instance data... }
            _Serial_Number : integer ;

        public { API... }
            function Facility_Code : longint ; override ;

            function Initialize( UI : TUI_Interface ) : TUnified_Exception ; override ;

            function Terminate : TUnified_Exception ; override ;

            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; override ;

            function Component_Type : longint ; override ;

            function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

            function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

            function Debugger : TDebug_Interface ; override ;

            function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                Memory : boolean ) : TUnified_Exception ; override ;

            function Disconnect_Input( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Disconnect_Output( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Examine( Address : int64 ; var Size : longint ;
                Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

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

            procedure Wake ; override ;

            function Get_Logger : TCEF_Logger ; override ;

            procedure Set_Logger( Value : TCEF_Logger ) ; override ;
     end ; // TRL11

     TRL11_IO = class( TBase_Memory )
                     public // Constructors...
                         constructor Create ;

                     private // Instance data...
                         Parent : TRL11 ;
                          _Low : int64 ; // Lowest port and number of ports
                          _Logger : TCEF_Logger ;

                     public // API...
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
     Num1s,
     Parse ; // TString_Parser

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ;
    external 'CEF_Util.dll' ;


function Octal( const Value : string ) : integer ;

var Loop : integer ;

begin
    Result := 0 ;
    for Loop := 1 to length( Value ) do
    begin
        Result := ( Result shl 3 ) or ( ord( Value[ Loop ] ) - 48 ) ;
    end ;
end ;


{ TCEF_RL11_Debugger methods... }

type TCEF_RL11_Debugger = class( TText_Debugger )
                                private
                                    _IO : TRL11 ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property IO : TRL11
                                                 read _IO
                                                 write _IO ;
                            end ;

function TCEF_RL11_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TCEF_RL11_Debugger.Count : longint ;

begin
    Result := 9 ;
end ;


{ TRL11 methods... }

function tRL11.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        CEFRL11Err_Success: _Error := 'Success' ;
        CEFRL11Err_Invalid_Range: _Error := 'Invalid range' ;
        CEFRL11Err_Component_Not_Found: _Error := 'Component not found' ;
        CEFRL11Err_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        CEFRL11Err_Access_Violation: _Error := 'Access violation' ;
        CEFRL11Err_Address_Out_Of_Range: _Error := 'Address out of range' ;
        CEFRL11Err_Invalid_Component: _Error := 'Invalid component' ;
        CEFRL11Err_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { tRT11.Translate_Error }


function TRL11.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TRL11.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


procedure TRL11.Init ;

begin
    if( Disk_Filename[ Current_Drive ] = '' ) then
    begin
        CSR := 128 ; // Controller ready
    end else
    begin
        CSR := 129 ; // Controller and drive ready
    end ;
    BAR := 0 ;
    Send_Vector := False ;
    if( Wait_For_Interrupt ) then
    begin
        _UI.Signal_Change_Notice( self, 0, False ) ; // Clear UNIBUS_BR5
    end ;
    Wait_For_Interrupt := False ;
    Do_Interrupt := False ;
end ;


procedure TRL11.Read_Bus( Address : int64 ; var Buffer : word ; Size : integer ;
   var Failure : boolean ) ;

var Loop : integer ;

begin
    Failure := False ;
    Memory_Data_Latch := 65535 ;
    Waiting_For_Data := True ;
    try
        if( Default_Output <> nil ) then
        begin
            for Loop := 0 to Outputs.Count - 1 do
            begin
                if( TComponent( Outputs[ Loop ] ).Read( Address, Size, IO_Type_Memory ) ) then
                begin
                    Buffer := Memory_Data_Latch ;
                    exit ;
                end ;
            end ;
        end ;
        if( Default_Input <> nil ) then
        begin
            for Loop := 0 to Inputs.Count - 1 do
            begin
                if( TComponent( Inputs[ Loop ] ).Read( Address, Size, IO_Type_Memory ) ) then
                begin
                    Buffer := Memory_Data_Latch ;
                    exit ;
                end ;
            end ;
        end ;
        Failure := True ;
    finally
        Waiting_For_Data := False ;
    end ;
end ; // TRL11.Read_Bus


procedure TRL11.Vector( V : word ) ;

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
        Component.Set_Signal( 'UNIBUS_BR5', True ) ;
    end ;
    _UI.Signal_Change_Notice( self, 0, True ) ; // BR5
end ;


function TRL11.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_List.Watchpoint_At( Address ) ;
end ;


{ API... }

function TRL11.Facility_Code : longint ;

begin
    Facility_Code := CEFRL11_Facility ;
end ;


function TRL11.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    Profiling := False ;
    _UI := UI ;
    Watchpoint_List := Get_Watchpoint_Manager ;
    Access_Mode := Access_RW or Access_Execute ;
    IO := TRL11_IO.Create ;
    _Vector := Octal( '160' ) ;

    _UI.Want_Signals( self, True ) ;

    Initialize := Set_Error( CEFRL11Err_Success ) ;
    Init ;
end ; { TRL11.Initialize }


function TRL11.Terminate : TUnified_Exception ;

var U : integer ;

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
    IO.Free ;
    IO := nil ;
    for U := 0 to 3 do
    begin
        if( Disk_Filename[ U ] <> '' ) then
        begin
            {$I-}
            closefile( Disk_File[ U ] ) ;
            {$I+}
            IOResult ;
            Disk_Filename[ U ] := '' ;
        end ;
    end ;
    Terminate := Set_Error( CEFRL11Err_Success ) ;
end ; { TRL11.Terminate }



function TRL11.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Last_Error( nil ) ;
    Result := nil ;
//    Clear_Watchpoint := Set_Error( CEFRL11Err_Success ) ;
    if( not Memory ) then
    begin
        Result := Watchpoint_List.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( CEFRL11Err_No_Matching_Watchpoint ) ;
    end ;
end ;


function TRL11.Component_Type : longint ;

begin
    Component_Type := Component_Type_Memory ;
end ;


function TRL11.Connect_Input( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := Set_Error( CEFRL11Err_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        P := integer( Component ) ;
        Parent.Child_Notification( self, Code, P ) ;
    end ;
    Connect_Input := Set_Error( CEFRL11Err_Success ) ;
end ;


function TRL11.Connect_Output( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := Set_Error( CEFRL11Err_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        P := integer( Component ) ;
        Parent.Child_Notification( self, Code, P ) ;
    end ;
    Connect_Output := Set_Error( CEFRL11Err_Success ) ;
end ;


function TRL11.Debugger : TDebug_Interface ;

begin
    Result := TCEF_RL11_Debugger.Create ;
    TCEF_RL11_Debugger( Result ).IO := Self ;
end ;


function TRL11.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count : integer ;
    Value : word ;

begin
    Set_Last_Error( nil ) ;
    Result := nil ;
    if( ( not Memory ) or ( IO._Low > Address ) or ( IO._Low + 7 < Address ) or ( Size = 0 ) ) then
    begin
        exit ;
    end ;
    _Buffer := PChar( Buffer ) ;
    Count := ( Size + 7 ) div 8 ; // Bits to bytes
    if( Count > 2 ) then
    begin
        while( Count > 0 ) do
        begin
            if( Count = 1 ) then
            begin
                Deposit( Address, 1, Buffer, Memory ) ;
            end else
            begin
                Deposit( Address, 2, Buffer, Memory ) ;
            end ;
            Count := Count - 2 ;
            Address := Address + 2 ;
            Buffer := pointer( integer( Buffer ) + 2 ) ;
        end ;
        exit ;
    end ;
    Value := ord( _Buffer[ 0 ] ) ;
    if( Count > 1 ) then
    begin
        Value := Value or ( ord( _Buffer[ 1 ] ) shl 8 ) ;
    end ;
    Address := Address - IO._Low ;
    try
        case Address of
            0 : // CSR
                begin
                    if( Count = 1 ) then
                    begin
                        Value := Value or ( CSR and $FF00 ) ;
                    end ;
                    CSR := Value ;
                end ;
            1 : // CSR high byte
                begin
                    Value := ( Value shl 8 ) or ( CSR and $FF ) ;
                    CSR := Value ;
                end ;
            2 : // BAR
                begin
                    if( Count = 1 ) then
                    begin
                        Value := Value or ( BAR and $FF00 ) ;
                    end ;
                    BAR := Value ;
                end ;
            3 : // BAR high byte
                begin
                    Value := ( Value shl 8 ) or ( BAR and $FF ) ;
                    BAR := Value ;
                end ;
            4 : // DAR
                begin
                    if( Count = 1 ) then
                    begin
                        Value := Value or ( DAR and $FF00 ) ;
                    end ;
                    DAR := Value ;
                end ;
            5 : // DAR high byte
                begin
                    Value := ( Value shl 8 ) or ( DAR and $FF ) ;
                    DAR := Value ;
                end ;
            6 : // MPR
                begin
                    if( Count = 1 ) then
                    begin
                        Value := Value or ( MPR[ 0 ] and $FF00 ) ;
                    end ;
                    MPR[ 0 ] := Value ;
                    MPR[ 1 ] := MPR[ 0 ] ;
                    MPR[ 2 ] := MPR[ 0 ] ;
                end ;
            7 : // MPR high byte
                begin
                    Value := ( Value shl 8 ) or ( MPR[ 0 ] and $FF ) ;
                    MPR[ 0 ] := Value ;
                    MPR[ 1 ] := MPR[ 0 ] ;
                    MPR[ 2 ] := MPR[ 0 ] ;
                end ;
        end ; // case Address of
    except
    end ;
    Deposit := Set_Error( CEFRL11Err_Success ) ;
end ; { TRL11.Deposit }


function TRL11.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
        Disconnect_Input := Set_Error( CEFRL11Err_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := Set_Error( CEFRL11Err_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            P := integer( Component ) ;
            Parent.Child_Notification( self, Code, P ) ;
        end ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TRL11.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
        Disconnect_Output := Set_Error( CEFRL11Err_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := Set_Error( CEFRL11Err_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            P := integer( Component ) ;
            Parent.Child_Notification( self, Code, P ) ;
        end ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TRL11.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count, _Size : integer ;
    Value : integer ;

begin
    Result := nil ;
    Set_Last_Error( nil ) ;
    if( ( not Memory ) or ( IO._Low > Address ) or ( IO._Low + 7 < Address ) ) then
    begin
        Examine := Set_Error( CEFRL11Err_Address_Out_Of_Range ) ;
        exit ;
    end ;
    Count := ( Size + 7 ) div 8 ; { Number of bytes to examine }
    if( Count > 2 ) then
    begin
        while( Count > 0 ) do
        begin
            if( Count = 1 ) then
            begin
                _Size := 1 ;
            end else
            begin
                _Size := 2 ;
            end ;
            Examine( Address, _Size, Buffer, Memory ) ;
            Count := Count - 2 ;
            Address := Address + 2 ;
            Buffer := pointer( integer( Buffer ) + 2 ) ;
        end ;
        exit ;
    end ;
    fillchar( Buffer^, Count, 255 ) ;
    _Buffer := Buffer ;
    Address := Address - IO._Low ;

    // Retrieve data...
    try
        case Address of
            0 :
                begin
                    if( Disk_Filename[ Current_Drive ] = '' ) then
                    begin
                        CSR := CSR and $FFFE ; // Clear drive ready bit
                    end ;
                    Value := CSR ;
                end ;
            1 : Value := CSR shr 8 ;
            2 : Value := BAR ;
            3 : Value := BAR shr 8 ;
            4 : Value := DAR ;
            5 : Value := DAR shr 8 ;
            6 : Value := MPR[ 0 ] ;
            else Value := MPR[ 0 ] shr 8 ;
        end ; // Case Address of
        _Buffer[ 0 ] := char( Value ) ;
        if( Count > 1 ) then
        begin
            _Buffer[ 1 ] := char( Value shr 8 ) ;
        end ;
    except
    end ;
    Examine := Set_Error( CEFRL11Err_Success ) ;
end ; { TRL11.Examine }


function TRL11.Get_Access_Mode( Address : int64 ;
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


function TRL11.Get_Profiling : boolean ;

begin
    Get_Profiling := Profiling ;
end ;


function TRL11.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

begin
    Result := True ;
    if( string( Name ) = 'UNIBUS_BR5' ) then
    begin
        State := Wait_For_Interrupt ;
    end else
    begin
        Result := False ;
    end ;
end ;


function TRL11.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


function TRL11.Memory : TMemory ;

begin
    Result := IO ;
end ;


const _Name : PChar = 'DEC RL11'#0 ;

function TRL11.Name : PChar ;

begin
    Name := _Name ;
end ;


function TRL11.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


procedure TRL11.Write_Bus( Address : int64 ; Buffer : word ; Size : integer ) ;

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


function TRL11.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

var Buffer : word ;

    procedure _Read( Address : int64 ; Size : longint ) ;

    var B : integer ;
        Watch : TCEF_Watchpoint ;

    begin
        if(
            ( Address >= IO._Low )
            and
            ( Address <= IO._Low + 7 )
          ) then
        begin
            Buffer := 0 ;
            Examine( Address, Size, PChar( @Buffer ), True ) ;
            case Address - IO._Low of
                2 : Buffer := Buffer and $FFFE ; // Clear low bit
                6..7 : // MPR
                       begin
                           Buffer := MPR[ 0 ] ;
                           MPR[ 0 ] := MPR[ 1 ] ;
                           MPR[ 1 ] := MPR[ 2 ] ;
                           if( Address - IO._Low = 7 ) then
                           begin
                               Buffer := Buffer shr 8 ;
                           end ;
                       end ;
            end ;
            if( _Logger <> nil ) then
            begin
                B := _Logger.Data_Radix ;
                if( ( B < 2 ) or ( B > 36 ) ) then
                begin
                    B := 10 ;
                end ;
                _Logger.Log( self, PChar( cvtb( 10, B, inttostr( Buffer ) ) +
                    ' from CSR+' + inttostr( Address and 7 ) ), -1, False, LT_Read ) ;
            end ;
            Write_Bus( Address, Buffer, Size ) ;
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
        end ;
    end ; // TRL11.Read._Read

var Count : integer ;

begin
    Read := False ;
    if(
        ( IO_Type = IO_Type_Bus )
        and
        ( Address = 0 )
        and
        Send_Vector
        and
        ( Size = 16 )
      ) then
    begin
        Buffer := _Vector ;
        Write_Bus( Address, Buffer, Size ) ;
        Result := True ;
        if( _Logger <> nil ) then
        begin
            _Logger.Log( self, 'responding to interrupt vector request', -1, False, LT_Other ) ;
        end ;
        exit ;
    end ;

    if(
        ( Address >= IO._Low )
        and
        ( Address <= IO._Low + 7 )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Read ) = 0 ) then
        begin
            exit ;
        end ;
	Read := True ;
        Count := ( Size + 7 ) div 8 ;
        while( Count > 0 ) do
        begin
            if( Count = 1 ) then
            begin
                _Read( Address, 8 ) ;
            end else
            begin
                _Read( Address, 16 ) ;
            end ;
            Count := Count - 2 ;
            Address := Address + 2 ;
        end ;
    end ;
end ; // TRL11.Read


function TRL11.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

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
	Deposit( Loop, 1, @Value, True ) ;
	Loop := Loop + 1 ;
    end ;
end ;


function TRL11.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var S : longint ;

begin
    Result := nil ;
    Set_Last_Error( nil ) ;
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


function TRL11.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

var Loop : int64 ;
    High : int64 ;
    S : longint ;
    Value : integer ;

begin { TRL11.Save_Contents }
    { Write contents... }
    Loop := 0 ;
    Stream.Set_Last_Error( nil ) ;
    Stream.Write( Loop, sizeof( Loop ) ) ;
    Result := Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( High, sizeof( High ) ) ;
    Result := Last_Error ;
    if( Result <> nil ) then
    begin
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
            exit ;
        end ;
	    Loop := Loop + 1 ;
    end ;
end ; { TRL11.Save_Contents }


function TRL11.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Stream.Write( IO._Low, sizeof( IO._Low ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( Profiling, sizeof( Profiling ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
end ;


function TRL11.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    if( ( not Memory ) and ( IO._Low <= Low ) and ( IO._Low + 7 >= High ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := Set_Error( CEFRL11Err_Success ) ;
end ;


procedure TRL11.Clear_Interrupt ;

begin
    if( Wait_for_Interrupt ) then
    begin
        Wait_For_Interrupt := False ;
        Do_Interrupt := False ;
        Send_Vector := False ;
        _UI.Signal_Change_Notice( self, 0, False ) ; // Clear BR5
    end ;
end ;


procedure TRL11.Set_Signal( Name : PChar ; State : boolean ) ;

var S : string ;

begin
    S := string( Name ) ;
    if( S = 'UNIBUS_BG5' ) then
    begin
        if( Wait_for_Interrupt ) then
        begin
            Clear_Interrupt ;
            Send_Vector := True ;
        end else
        begin
            Send_Vector := False ; // Someone else's interrupt
        end ;
    end else
    if( S = 'UNIBUS_INIT' ) then
    begin
        Init ;
    end ;
end ;


procedure TRL11.Set_Profiling( _On, Children : boolean ) ;

begin
    Profiling := _On ;
end ;


function TRL11.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Watchpoint := Set_Error( CEFRL11Err_Success ) ;
    if( ( not Memory ) and ( IO._Low <= Address ) and ( IO._Low + 7 >= Address ) ) then
    begin
        Watchpoint_List.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TRL11.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TRL11.Signal_Count : longint ;

begin
    Result := 1 ;
end ;


function TRL11.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := '' ;
    if( Index = 0 ) then
    begin
        Temp_Signal_Name := 'UNIBUS_BR5' ;
    end ;
    Result := PChar( Temp_Signal_Name ) ;
end ;


function TRL11.Signal_Out( Index : longint ) : boolean ;

begin
    Result := ( Index = 0 ) ;
end ;


function TRL11.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TRL11.Signal_Index( Name : PChar ) : integer ;

begin
    if( string( Name ) = 'UNIBUS_BR5' ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := - 1 ;
    end ;
end ;


procedure TRL11.Wake ;

begin
    CSR := CSR or 128 ; // Controller is ready
    if( Disk_Filename[ Current_Drive ] <> '' ) then
    begin
        CSR := CSR or 1 ; // Drive ready
    end ;
    if( Do_Interrupt and ( ( CSR and 64 ) <> 0 ) ) then // Request for interrupts
    begin
        Vector( _Vector ) ;
    end ;
    Do_Interrupt := False ;
end ;


function TRL11.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TRL11.Set_Logger( Value : TCEF_Logger ) ;

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
    IO._Logger := Value ;
end ;


const Sectors = 40 ;
      Sector_Size = 256 ;
      Track_Size = Sectors * Sector_Size ; // Data per track
      Surface_Size = 256 * Track_Size ;
      Max_Cylinder = 255 ; // Cylinders 0-255

function TRL11.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

    procedure Set_Status( Value, Interval : integer ) ; // Set completion status

    begin
        Do_Interrupt := True ;
        CSR := ( ( CSR and $FFFE ) or Value ) and ( not 128 ) ;
        if( Interval = 0 ) then
        begin
            Wake ;
        end else
        begin
            if( _UI.Clock <> nil ) then
            begin
                _UI.Clock.Block( self, Interval * 1000000 ) ; // ms to ns
            end ;
        end ;
    end ;


    function Check_Position : boolean ;

    begin // Return True if positioning error
        Result := False ;
        if(
            ( ( DAR and $3F ) > Sectors ) // Bad sector
            or
            ( ( Drives[ Current_Drive ].Current_Track ) <> ( DAR shr 6 ) ) // Bad cylinder
          ) then
        begin
            Set_Status( $9401, 0 ) ; // Incomplete and Data Late & Header
            Result := True ;
        end ;
    end ;


    procedure Write_Check ;

    var Address : longint ; // Disk address
        Buffer1, Buffer2 : array[ 0..Track_Size div 2 ] of word ;
        Count, _Count, Temp, Index : integer ;
        Failure : boolean ;
        Sector : integer ;
        Value : word ;

    begin
        if( Disk_Filename[ Current_Drive ] = '' ) then
        begin
            CSR := CSR and $FFFE ; // Clear drive ready bit
            exit ; // No disk
        end ;

        if( Check_Position ) then
        begin
            exit ;
        end ;

        // Calculate address in disk file...
        Drives[ Current_Drive ].Current_Track := ( DAR shr 6 ) and 511 ;
        Sector := ( DAR and 63 ) ;
        Address := ( ( DAR shr 6 ) * Sectors + Sector ) * Sector_Size ;

        // Calculate number of bytes to read...
        MPR[ 0 ] := MPR[ 0 ] and $FFFE ;
        Count := smallint( MPR[ 0 ] ) * -2 ; // Convert from negative words to positive bytes
        if( Count + Sector * Sector_Size > Track_Size ) then
        begin
            Count := Track_Size - Sector * Sector_Size ;
        end ;
        _Count := Count ;

        // Read data from disk...
        seek( Disk_File[ Current_Drive ], Address ) ;
        blockread( Disk_File[ Current_Drive ], Buffer2, Count ) ;

        // Read buffer from memory...
        Temp := BAR or ( ( CSR and $30 ) shl 12 ) ;
        Index := 0 ;
        while( Count > 0 ) do
        begin
            Read_Bus( Temp, Value, 16, Failure ) ; // Read a word
            if( Failure ) then
            begin
                Set_Status( $A001, 2 ) ; // NXM error
                exit ;
            end ;
            Buffer1[ Index ] := Value ;
            inc( Index ) ;

            // Increment Bus address...
            Temp := Temp + 2 ;
            BAR := Temp and $FFFF ;
            CSR := ( CSR and $FFCF ) or ( ( Temp and $30000 ) shr 12 ) ;

            // Decrement count
            Count := Count - 2 ;
            MPR[ 0 ] := MPR[ 0 ] + 2 ;
        end ; // while( Count > 0 )

        for Count := 0 to _Count - 1 do
        begin
            //TODO: What should this do?
        end ;
        MPR[ 1 ] := MPR[ 0 ] ;
        MPR[ 2 ] := MPR[ 0 ] ;
        Set_Status( 1, 13 ) ; // 13 ms latency average
    end ; // .Write_Check


    procedure Get_Status ;

    begin
        if( Disk_Filename[ Current_Drive ] = '' ) then
        begin
            MPR[ 0 ] := 32 ; // Cover open, Load
            MPR[ 1 ] := 32 ;
            MPR[ 2 ] := 32 ;
            CSR := CSR and $FFFE ; // Clear drive ready bit
            Set_Status( 0, 0 ) ;
            exit ; // No disk
        end ;

        MPR[ 0 ] :=    5 // Lock on
                    or 8 // Brush Home
                    or 16 // Heads out
                    or 0 // Cover closed
                    or ( ( Drives[ Current_Drive ].Current_Track and 1 ) shl 6 ) // Head
                    or 0 // reserved
                    or 0 // No drive selection error
                    or 0 // No volume check (is this correct?)
                    or 0 // No write gate check
                    or 0 // No spin error
                    or 0 // No seek error
                    or 0 // No write-lock
                    or 0 // No current head error
                    or 0 ; // No write data error
        if( Drives[ Current_Drive ].Read_Only ) then
        begin
            MPR[ 0 ] := MPR[ 0 ] or 8192 ; // Write lock
        end ;
        MPR[ 1 ] := MPR[ 0 ] ;
        MPR[ 2 ] := MPR[ 0 ] ;
        Set_Status( 1, 0 ) ;
    end ; // .Get_Status


    procedure _Seek ;

    var Dummy : integer ;
        T : integer ;

    begin
        if( Disk_Filename[ Current_Drive ] = '' ) then
        begin
            CSR := CSR and $FFFE ; // Clear drive ready bit
            exit ; // No disk
        end ;

        Dummy := ( DAR shr 7 ) and $FF ; // Offset to move
        T := Dummy ;
        if( ( DAR and 4 ) = 0 ) then
        begin
            Dummy := -Dummy ; // Negative movement
        end ;
        Dummy := ( Drives[ Current_Drive ].Current_Track shr 1 ) + Dummy ;
        if( Dummy > Max_Cylinder ) then
        begin
            Dummy := Max_Cylinder ;
        end else
        if( Dummy < 0 ) then
        begin
            Dummy := 0 ;
        end ;
        Dummy := Dummy shl 1 ;
        if( ( DAR and 16 ) <> 0 ) then // Head select
        begin
            Dummy := Dummy or 1 ;
        end ;
        Drives[ Current_Drive ].Current_Track := Dummy ;
        T := T div 2 ; // Average 1 ms per 2 tracks
        if( T < 15 ) then
        begin
            T := 15 ; //Minimum (Single track) = 15 ms
        end ;
        Set_Status( 0, T div 4{2} ) ;  // for testing
    end ; // ._Seek


    procedure Read_Header ;

    begin
        if( Disk_Filename[ Current_Drive ] = '' ) then
        begin
            CSR := CSR and $FFFE ; // Clear drive ready bit
            exit ; // No disk
        end ;

        MPR[ 0 ] := ( ( Drives[ Current_Drive ].Current_Track shl 6 ) or ( DAR and $3F ) ) and $7FFF ;
        MPR[ 1 ] := 0 ; // Always 0
        MPR[ 2 ] := 0 ; // CRC (not implemented)
        Set_Status( 0, 2{13} ) ; // Average latency = 12.5 ms - 2 ms for testing
    end ;


    procedure Write_Data ;

    var Address : longint ; // Disk address
        Buffer : array[ 0..Track_Size div 2 ] of word ;
        Count, _Count, Temp, Index : integer ;
        Failure : boolean ;
        Offset : integer ; // Byte offset within sector
        Sector : integer ;
        Value : word ;

    begin
        if( Disk_Filename[ Current_Drive ] = '' ) then
        begin
            CSR := CSR and $FFFE ; // Clear drive ready bit
            Set_Status( 0, 0 ) ;
            exit ; // No disk
        end ;

        if( Drives[ Current_Drive ].Read_Only ) then
        begin
            Set_Status( $C001, 0 ) ; // Drive error
            exit ;
        end ;

        if( Check_Position ) then
        begin
            exit ;
        end ;

        // Calculate address in disk file...
        Drives[ Current_Drive ].Current_Track := ( DAR shr 6 ) and 511 ;
        Sector := ( DAR and 63 ) ;
        Address := ( ( DAR shr 6 ) * Sectors + Sector ) * Sector_Size ;

        // Calculate number of bytes to write...
        MPR[ 0 ] := MPR[ 0 ] and $FFFE ;
        Count := smallint( MPR[ 0 ] ) * -2 ; // Convert from negative words to positive bytes
        if( Count + Sector * Sector_Size > Track_Size ) then
        begin
            Count := Track_Size - Sector * Sector_Size ;
        end ;

        // Read buffer from memory...
        _Count := 0 ; // Bytes read from memory
        Temp := BAR or ( ( CSR and $30 ) shl 12 ) ;
        Index := 0 ;
        Offset := 0 ;
        while( Count > 0 ) do // While bytes remaining to be read
        begin
            Read_Bus( Temp, Value, 16, Failure ) ; // Read a word
            if( Failure ) then
            begin
                Set_Status( $A001, 2 ) ; // NXM error
                break ;
            end ;
            Buffer[ Index ] := Value ;
            inc( Index ) ;

            // Increment Bus address...
            Temp := Temp + 2 ;
            BAR := Temp and $FFFF ;
            CSR := ( CSR and $FFCF ) or ( ( Temp and $30000 ) shr 12 ) ;

            // Decrement count...
            Count := Count - 2 ;
            _Count := _Count + 2 ; // Actual bytes read
            MPR[ 0 ] := smallint( MPR[ 0 ] ) + 1 ;
            Offset := Offset + 2 ;
            if( Offset >= Sector_Size ) then
            begin
                Offset := Offset - Sector_Size ;
                inc( Sector ) ;
                DAR := DAR and ( not 63 ) or Sector ;
            end ;
        end ; // while( Count > 0 )

        // Write data to disk...
        seek( Disk_File[ Current_Drive ], Address ) ;
        blockwrite( Disk_File[ Current_Drive ], Buffer, _Count ) ;
        if( _Logger <> nil ) then
        begin
            _Logger.Log( self, PChar( @Buffer ), _Count, True, LT_Data ) ;
        end ;
        MPR[ 1 ] := MPR[ 0 ] ;
        MPR[ 2 ] := MPR[ 0 ] ;
        Set_Status( 1, 13 ) ; // Average latency = 12.5 ms
    end ; // .Write_Data


    procedure Read_Data( Check_Header : boolean ) ;

    var Address : longint ; // Disk address
        Buffer : array[ 0..Track_Size div 2 ] of word ;
        Count, Temp, Index : integer ;
        Offset : integer ;
        Sector : integer ;
        Value : word ;

    begin
        if( Disk_Filename[ Current_Drive ] = '' ) then
        begin
            CSR := CSR and $FFFE ; // Clear drive ready bit
            exit ; // No disk
        end ;

        if( Check_Header ) then
        begin
            if( Check_Position ) then
            begin
                exit ;
            end ;
        end ;

        // Calculate address in disk file...
        Drives[ Current_Drive ].Current_Track := ( DAR shr 6 ) and 511 ;
        Sector := DAR and 63 ;
        Address := ( ( DAR shr 6 ) * Sectors + Sector ) * Sector_Size ;

        // Calculate number of bytes to read...
        MPR[ 0 ] := MPR[ 0 ] and $FFFE ;
        Count := smallint( MPR[ 0 ] ) * -2 ; // Convert from negative words to positive bytes
        if( Count + Sector * Sector_Size > Track_Size ) then
        begin
            Count := Track_Size - Sector * Sector_Size ;
        end ;

        // Read data from disk...
        seek( Disk_File[ Current_Drive ], Address ) ;
        blockread( Disk_File[ Current_Drive ], Buffer, Count ) ;
        if( _Logger <> nil ) then
        begin
            _Logger.Log( self, PChar( @Buffer ), Count, True, LT_Data ) ;
        end ;

        // Write buffer to memory...
        Temp := BAR or ( ( CSR and $30 ) shl 12 ) ;
        Index := 0 ;
        Offset := 0 ;
        while( Count > 0 ) do
        begin
            Value := Buffer[ Index ] ;
            Write_Bus( Temp, Value, 16 ) ; // Write a word
            inc( Index ) ;

            // Increment Bus address...
            Temp := Temp + 2 ;
            BAR := Temp and $FFFF ;
            CSR := ( CSR and $FFCF ) or ( ( Temp and $30000 ) shr 12 ) ;

            // Decrement count
            Count := Count - 2 ;
            MPR[ 0 ] := smallint( MPR[ 0 ] ) + 1 ;
            Offset := Offset + 2 ;
            if( Offset >= Sector_Size ) then
            begin
                Offset := Offset - Sector_Size ;
                inc( Sector ) ;
                DAR := DAR and ( not 63 ) or Sector ;
            end ;
        end ; // while( Count > 0 )
        MPR[ 1 ] := MPR[ 0 ] ;
        MPR[ 2 ] := MPR[ 0 ] ;
        Set_Status( 0, 13 ) ; // Average latency = 12.5 ms
    end ; // .Read_Data


    procedure _Write( Address : int64 ; Size : longint ) ;

    var B, Dummy : integer ;
        S : string ;
        Watch : TCEF_Watchpoint ;

    begin
        if(
            ( Address >= IO._Low )
            and
            ( Address <= IO._Low + 7 )
          ) then
        begin
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
            case Address - IO._Low of
                0 : // CSR
                    begin
                        Value := ( Value and $3FE ) or ( CSR and $FC01 ) ; // Clear read-only bits
                    end ;
                1 : exit ;
            end ;
            Write := Deposit( Address, Size, @Value, True ) ;
            case Address - IO._Low of
                0 : // CSR
                    begin
                        if( ( CSR and 128 ) = 0 ) then // Execute command
                        begin
                            Clear_Interrupt ;
                            CSR := CSR and $0CFF ; // Clear error flags
                            Current_Drive := ( CSR shr 8 ) and 3 ;
                            if( _Logger <> nil ) then
                            begin
                                B := _Logger.Data_Radix ;
                                if( ( B < 2 ) or ( B > 36 ) ) then
                                begin
                                    B := 10 ;
                                end ;
                                S := 'CSR=' + cvtb( 10, B, inttostr( CSR ) ) + ' ' +
                                    'BAR=' + cvtb( 10, B, inttostr( BAR ) ) + ' ' +
                                    'DAR=' + cvtb( 10, B, inttostr( DAR ) ) + ' ' +
                                    'MPR0=' + cvtb( 10, B, inttostr( MPR[ 0 ] ) ) + ' ' +
                                    'MPR1=' + cvtb( 10, B, inttostr( MPR[ 1 ] ) ) + ' ' +
                                    'MPR2=' + cvtb( 10, B, inttostr( MPR[ 2 ] ) ) ;
                            end ;
                            case ( CSR shr 1 ) and 7 of
                                0 : Set_Status( ord( Disk_Filename[ Current_Drive ] = '' ), 0 ) ; // No Op
                                1 : begin
                                        if( _Logger <> nil ) then
                                        begin
                                            _Logger.Log( self, PChar( S + ' Write Check' ), -1, False, LT_Execution ) ;
                                        end ;
                                        Write_Check ;
                                    end ;
                                2 : begin
                                        Get_Status ;
                                    end ;
                                3 : begin
                                        if( _Logger <> nil ) then
                                        begin
                                            Dummy := ( DAR shr 7 ) and $FF ; // Offset to move
                                            if( ( DAR and 4 ) = 0 ) then
                                            begin
                                                Dummy := -Dummy ; // Negative movement
                                            end ;
                                            _Logger.Log( self, PChar( S + ' Seek ' + inttostr( Dummy ) ), -1, False, LT_Execution ) ;
                                        end ;
                                        _Seek ;
                                    end ;
                                4 : begin
                                        if( _Logger <> nil ) then
                                        begin
                                            _Logger.Log( self, PChar( S + ' Read Header' ), -1, False, LT_Execution ) ;
                                        end ;
                                        Read_Header ;
                                    end ;
                                5 : begin
                                        if( _Logger <> nil ) then
                                        begin
                                            _Logger.Log( self, PChar( S + ' Write Data' ), -1, False, LT_Execution ) ;
                                        end ;
                                        Write_Data ;
                                    end ;
                                6 : begin
                                        if( _Logger <> nil ) then
                                        begin
                                            _Logger.Log( self, PChar( S + ' Write Data' ), -1, False, LT_Execution ) ;
                                        end ;
                                        Read_Data( True ) ;
                                    end ;
                                7 : begin
                                        if( _Logger <> nil ) then
                                        begin
                                            _Logger.Log( self, PChar( S + ' Read Data' ), -1, False, LT_Execution ) ;
                                        end ;
                                        Read_Data( False ) ;
                                    end ;
                            end ;
                        end else
                        begin
                            if( ( Value and 64 ) = 0 ) then // Clearing interrupt enable
                            begin
                                Clear_Interrupt ;
                            end else
                            if( ( CSR and ( 128 or 64 ) ) = 128 ) then
                            begin
                                Vector( _Vector ) ;
                            end ;
                            if( Disabled ) then
                            begin
                                CSR := 0 ;
                            end ;
                        end ;
                    end ;
                4 : Value := Value and $C7 ; // DAR
                5 : exit ;
                6 : Value := Value and 255 ; // MPR
                7 : exit ;
            end ;
        end ;
    end ; // _Write

var B, Count : integer ;

begin
    if( Waiting_For_Data ) then
    begin
        Result := Set_Error( 0 ) ;
        Memory_Data_Latch := Value ;
        exit ;
    end ;

    if(
        ( Address >= IO._Low )
        and
        ( Address <= IO._Low + 7 )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Write ) = 0 ) then
        begin
            Write := Set_Error( CEFRL11Err_Access_Violation ) ;
            exit ;
        end ;
        Count := ( Size + 7 ) div 8 ;
        if( _Logger <> nil ) then
        begin
            B := _Logger.Data_Radix ;
            if( ( B < 2 ) or ( B > 36 ) ) then
            begin
                B := 10 ;
            end ;
            _Logger.Log( self, PChar( cvtb( 10, B, inttostr( Value ) ) + ' to CSR+' +
                cvtb( 10, B, inttostr( Address and 7 ) ) ), -1, False, LT_Write ) ;
        end ;
        while( Count > 0 ) do
        begin
            if( Count = 1 ) then
            begin
                _Write( Address, 8 ) ;
            end else
            begin
                _Write( Address, 16 ) ;
            end ;
            Count := Count - 2 ;
            Address := Address - 2 ;
        end ;
    end else
    begin
	    Write := Set_Error( CEFRL11Err_Success ) ;
    end ;
end ;


constructor TRL11_IO.Create ;

begin
    inherited Create ;

    _Low := Octal( '774400' ) ;
end ;


function TRL11_IO.Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;

begin
    if( ( Low < 0 ) or ( Low > High ) ) then
    begin
	    Set_Address_Range := Parent.Set_Error( CEFRL11Err_Invalid_Range ) ;
	    exit ;
    end ;
    Set_Address_Range := Parent.Set_Error( CEFRL11Err_Success ) ;
    _Low := Low ;
end ;


procedure TRL11_IO.Get_Address_Range( var Low, High : int64 ) ;

begin
    Low := _Low ;
    High := _Low + 7 ;
end ;


procedure TRL11.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TRL11.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TRL11.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TRL11.Set_Parent( Component : TComponent ) ;

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


procedure TRL11.Set_Up( P : PChar ) ;

var Dummy : integer ;
    F : file ;
    Parser : TString_Parser ;
    S, S1 : string ;
    U : integer ;

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
        if( S = 'DISABLE' ) then
        begin
            Disabled := True ;
        end else
        if( S = 'VECTOR' ) then
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
            _Vector := Convert_Value( S ) ;
        end else
        if( S = 'DISK' ) then
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
            U := Convert_Value( S ) ; // Disk unit
            S := uppercase( Parser.Token ) ;
            if( ( U >= 0 ) and ( U <= 3 ) and ( S <> '' ) ) then
            begin
                if( copy( S, 1, 1 ) = '"' ) then
                begin
                    S := copy( S, 2, length( S ) ) ;
                    Dummy := pos( '"', S + '"' ) ;
                    S := copy( S, 1, Dummy - 1 ) ;
                end ;
                if( copy( S, 1, 1 ) = #39 ) then
                begin
                    S := copy( S, 2, length( S ) ) ;
                    Dummy := pos( #39, S + #39 ) ;
                    S := copy( S, 1, Dummy - 1 ) ;
                end ;
                assignfile( F, S ) ;
                {$I-}
                System.reset( F, 1 ) ;
                {$I+}
                Dummy := IOResult ;
                if( Dummy = 0 ) then
                begin
                    if( Disk_Filename[ U ] <> '' ) then // Something already open on that disk
                    begin
                        {$I-}
                        closefile( Disk_File[ u ] ) ;
                        {$I+}
                        IOResult ;
                    end ;
                    {$I-}
                    closefile( F ) ;
                    {$I+}
                    IOResult ;
                    Disk_Filename[ U ] := S ;
                    assignfile( Disk_File[ U ], S ) ;
                    {$I-}
                    System.reset( Disk_File[ U ], 1 ) ;
                    {$I+}
                    Dummy := IOResult ;
                    if( Dummy = 0 ) then
                    begin
                        S := uppercase( Parser.Token ) ;
                        if( S = 'RO' ) then
                        begin
                            Drives[ U ].Read_Only := True ;
                        end else
                        begin
                            if( length( S ) > 0 ) then
                            begin
                                Parser.Put_Token( S ) ;
                            end ;
                            Drives[ U ].Read_Only := False ;
                        end ;
                    end else
                    begin
                        Disk_Filename[ U ] := '' ;
                    end ; // if( Dummy = 0 )
                end ; // if( Dummy = 0 )
            end ; // if( ( U >= 0 ) and ( U <= 3 ) )
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ; // TRL11.Set_Up


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
    Test : TRL11 ;
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
    Test := TRL11.Create ;
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


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

