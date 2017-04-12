{
        Program Name : Serial_Cable
        Package Name : CEF
        Purpose      : CEF serial cable component
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

        Copyright (C) 2006-2016 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

         23-Jan-2007    EAC         Prevent ringing on signals.

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This unit provides support for serial cable components.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Serial_Cable ;

interface

uses // Borland...
     Classes, // TList

     // CEF...
     _CEF, // TUser_Interface
     CEF, // TBase_Component
     Collect, // TCollection
     _DebugIn, // TDebug_Interface
     _Streams, // TCOM_Stream
     _UE ; // TUnified_Exception

const CEFSerial_Facility = -1 ;
const TSerialErr_Success = 0 ;
const TSerialErr_Invalid_Range = 1 ;
const TSerialErr_Component_Not_Found = 2 ;
const TSerialErr_Memory_Exhausted = 3 ;
const TSerialErr_Access_Violation = 4 ;
const TSerialErr_Address_Out_Of_Range = 5 ;
const TSerialErr_Invalid_Component = 6 ;

const Signal_CD = 0 ; // Carrier Detect
const Signal_DSR = 1 ; // Data Set Ready
const Signal_CTS = 2 ; // Clear To Send
const Signal_RTS = 3 ; // Request To Send
const Signal_DTR = 4 ; // Data Terminal Ready
const Signal_RI = 5 ; // Ring Indicator
const Signal_Break = 6 ; // Indicates whether or not a break was sent/received

type TSerial_Cable_Cable = class ;

     TSerial_Cable = class( TBase_Component )
	     private { Instance data... }
            _Cable : TSerial_Cable_Cable ;
            Inputs, Outputs : TList ;
	        _UI : TUI_Interface ;
            _Tag : longint ;
            _Parent : TComponent ;
            Signals : array[ 0..6 ] of boolean ;
            Temp_Signal_Name : string ;
            Processing_Signals : TStringList ;

        private // Internal utility routines...
            procedure Add_States( Component : TComponent ) ;
            procedure Clear_States ;
            function Default_Input : TComponent ;
            function Default_Output : TComponent ;
            procedure Send_Signal( const Name : string ; Index : integer ;
                Value : boolean ) ;
            procedure _Initialize ;

        protected
            function Translate_Error( Code : longint ) : string ; override ;

	    public { Public instance data... }
            _Serial_Number : integer ;

        public { API... }
            function Facility_Code : longint ; override ;

            function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                override ;

            function Terminate : TUnified_Exception ; override ;

            function Cable : TCable ; override ;

            function Component_Type : longint ; override ;

            function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

            function Debugger : TDebug_Interface ; override ;

            function Disconnect_Input( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Disconnect_Output( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Input_Component( Index : longint ) : TComponent ; override ;

            function Name : PChar ; override ;

            function Output_Component( Index : longint ) : TComponent ;
                override ;

            function Read( Address : int64 ; Size : longint ;
                IO_Type : longint ) : boolean ; override ;

            function Write( Address : int64 ; Value, Size : longint ;
                IO_Type : longint ) : TUnified_Exception ; override ;

            procedure Set_Tag( Value : longint ) ; override ;

            function Get_Tag : longint ; override ;

            function Get_Parent : TComponent ; override ;

            procedure Set_Parent( Component : TComponent ) ; override ;

            procedure Set_Up( P : PChar ) ; override ;

            function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                override ;

            procedure Set_Signal( Name : PChar ; State : boolean ) ; override ;

            function Signal_Count : longint ; override ;

            function Signal_Name( Index : longint ) : PChar ; override ;

            function Signal_Out( Index : longint ) : boolean ; override ;
     end ; // TSerial_Cable


     TSerial_Cable_Cable = class( TBase_Cable )
                               private // Instance data...
                                    _Current_Data : record
                                                        Value : integer ;
                                                        Data_Size : integer ;
                                                        Stop_Bits : integer ;
                                                        Speed : int64 ;
                                                        Valid : boolean ;
                                                    end ;

                               public
                                   Parent : TSerial_Cable ;

                                   function Serial : boolean ; override ;

                                   function Protocol : PChar ; override ;

                                   function Transmit( Speed : int64 ; Value, Data_Size, Stop_Bits : longint ) : TUnified_Exception ;
                                       override ;

                                   function Transmit_String( Speed : int64 ; Value : PChar ;
                                       Data_Size, Stop_Bits : integer ) : TUnified_Exception ;
                                       override ;

                                   procedure Receive( Source : TComponent ; Speed : int64 ;
                                       Value, Data_Size, Stop_Bits : longint ) ; override ;

                                   function Get_Data( var Speed : int64 ;
                                       var Value, Data_Size, Stop_Bits : integer ) : boolean ;
                                       override ;
                           end ;


{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
     Menus, // TPopupMenu
     ExtCtrls, // TShape
     StdCtrls, // TLabel
     SysUtils, // strtoint

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     DebugInt, // TText_Debugger
     Num1s, // Num1
     Parse, // TString_Parser
     UE ; // Create_Simple_UE


{ TSerial_Cable methods... }

function TSerial_Cable.Translate_Error( Code : longint ) : string ;

begin
    case Code of
        TSerialErr_Success: Result := 'Success' ;
        TSerialErr_Invalid_Range: Result := 'Invalid range' ;
        TSerialErr_Component_Not_Found: Result := 'Component not found' ;
        TSerialErr_Access_Violation: Result := 'Access violation' ;
        TSerialErr_Address_Out_Of_Range: Result := 'Address out of range' ;
        TSerialErr_Invalid_Component: Result := 'Invalid component' ;
        TSerialErr_Memory_Exhausted: Result := 'Memory exhausted' ;
        else Result := 'Unknown error number ' + Num1( Code ) ;
    end ;
end ; // Translate_Error


type TSerial_Cable_Debugger = class( TText_Debugger )
                                private
                                    _Cable : TSerial_Cable ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Panel : TSerial_Cable
                                                 read _Cable
                                                 write _Cable ;
                            end ;

function TSerial_Cable_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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



var I : TText_Debugger ;
    I1 : TDebug_Interface ;

begin
    I := TText_Debugger.Create ;
    case Ordinal of
        0 : begin
                if( Panel.Default_Input = nil ) then
                begin
                    I.Title := 'Default_Input = nil' ;
                end else
                begin
                    I1 := Panel.Default_Input.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Input = ' + Pointer_To_String( Panel.Default_Input ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Input = ' + Pointer_To_String( Panel.Default_Input ) ) ;
                end ;
            end ;
        1 : begin
                if( Panel.Default_Output = nil ) then
                begin
                    I.Title := 'Default_Output = nil' ;
                end else
                begin
                    I1 := Panel.Default_Output.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Output = ' + Pointer_To_String( Panel.Default_Output ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Output = ' + Pointer_To_String( Panel.Default_Output ) ) ;
                end ;
            end ;
        2 : I.Title := PChar( '_Serial_Number = ' + Num1( Panel._Serial_Number ) ) ;
        3 : begin
                if( Panel._UI = nil ) then
                begin
                    I.Title := '_UI = nil' ;
                end else
                begin
                    I1 := Panel._UI.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( '_UI = ' + Pointer_To_String( Panel._UI ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( '_UI = ' + Pointer_To_String( Panel._UI ) ) ;
                end ;
            end ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TSerial_Cable_Debugger.Count : longint ;

begin
    Result := 4 ;
end ;


{ TSerial_Cable methods... }

procedure TSerial_Cable.Add_States( Component : TComponent ) ;

begin
end ; // TSerial_Cable.Add_States


procedure TSerial_Cable.Clear_States ;

begin
end ;


function TSerial_Cable.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TSerial_Cable.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


procedure TSerial_Cable.Send_Signal( const Name : string ; Index : integer ;
    Value : boolean ) ;

var Component : TComponent ;
    Loop : integer ;

begin
    for Loop := 0 to Outputs.Count - 1 do
    begin
        Component := TComponent( Outputs[ Loop ] ) ;
        Component.Set_Signal( PChar( Name ), Value ) ;
    end ;
    _UI.Signal_Change_Notice( self, Index, Value ) ;
end ;


function TSerial_Cable.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

var S : string ;

begin
    Result := True ;
    S := uppercase( string( Name ) ) ;
    if( S = 'CD' ) then
    begin
        State := Signals[ Signal_CD ] ;
    end else
    if( S = 'DSR' ) then
    begin
        State := Signals[ Signal_DSR ] ;
    end else
    if( S = 'CTS' ) then
    begin
        State := Signals[ Signal_CTS ] ;
    end else
    if( S = 'RTS' ) then
    begin
        State := Signals[ Signal_RTS ] ;
    end else
    if( S = 'DTR' ) then
    begin
        State := Signals[ Signal_DTR ] ;
    end else
    if( S = 'RI' ) then
    begin
        State := Signals[ Signal_RI ] ;
    end else
    if( S = 'BREAK' ) then
    begin
        State := Signals[ Signal_Break ] ;
    end else
    begin
        Result := False ;
    end ;
end ;


procedure TSerial_Cable.Set_Signal( Name : PChar ; State : boolean ) ;

var Index : integer ;
    S : string ;

begin
    S := uppercase( string( Name ) ) ;
    if( S = 'CD' ) then
    begin
        Index := Signal_CD ;
    end else
    if( S = 'DSR' ) then
    begin
        Index := Signal_DSR ;
    end else
    if( S = 'CTS' ) then
    begin
        Index := Signal_CTS ;
    end else
    if( S = 'RTS' ) then
    begin
        Index := Signal_RTS ;
    end else
    if( S = 'DTR' ) then
    begin
        Index := Signal_DTR ;
    end else
    if( S = 'RI' ) then
    begin
        Index := Signal_RI ;
    end else
    if( S = 'BREAK' ) then
    begin
        Index := Signal_Break ;
    end else
    begin
        exit ;
    end ;
    if( Processing_Signals.Indexof( S ) <> -1 ) then // Already in the process of setting this signal
    begin
        exit ; // Prevent ringing
    end ;
    Processing_Signals.Add( S ) ;
    Signals[ Index ] := State ;
    Send_Signal( S, Index, State ) ;
    Index := Processing_Signals.IndexOf( S ) ;
    Processing_Signals.Delete( Index ) ;
end ;


function TSerial_Cable.Signal_Count : longint ;

begin
    Result := Signal_Break + 1 ;
end ;


function TSerial_Cable.Signal_Name( Index : longint ) : PChar ;

begin
    case Index of
        Signal_CD : Temp_Signal_Name := 'CD' ;
        Signal_DSR : Temp_Signal_Name := 'DSR' ;
        Signal_CTS : Temp_Signal_Name := 'CTS' ;
        Signal_RTS : Temp_Signal_Name := 'RTS' ;
        Signal_DTR : Temp_Signal_Name := 'DTR' ;
        Signal_RI : Temp_Signal_Name := 'RI' ;
        Signal_Break : Temp_Signal_Name := 'BREAK' ;
        else Temp_Signal_Name := '' ;
    end ;
    Result := PChar( Temp_Signal_Name ) ;
end ;


function TSerial_Cable.Signal_Out( Index : longint ) : boolean ;

begin
    Result := True ;
end ;


procedure TSerial_Cable._Initialize ;

begin
end ;


{ API... }

function TSerial_Cable.Facility_Code : longint ;

begin
    Facility_Code := CEFSerial_Facility ;
end ;


function TSerial_Cable.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    Processing_Signals := TStringList.Create ;
    _UI := UI ;
    _Cable := TSerial_Cable_Cable.Create ;
    _Cable.Parent := self ;

    _Initialize ;

    Initialize := Set_Error( TSerialErr_Success ) ;
end ;


function TSerial_Cable.Terminate : TUnified_Exception ;

var Notice : integer ;
    P : int64 ;

begin
    Result := nil ;
    if( _UI <> nil ) then
    begin
        _UI.Termination_Notice( self ) ;
    end ;
    if( Parent <> nil ) then
    begin
        Notice := Child_Notice_Request_Terminate ;
        Parent.Child_Notification( self, Notice, P ) ;
        if( Notice = 0 ) then // Parent denied termination
        begin
            exit ;
        end ;
    end ;
    if( Parent <> nil ) then
    begin
        Notice := Child_Notice_Terminating ;
        Parent.Child_Notification( self, Notice, P ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Processing_Signals.Free ;
    Processing_Signals := nil ;
    Terminate := Set_Error( TSerialErr_Success ) ;
    Free ;
end ;



function TSerial_Cable.Cable : TCable ;

begin
    Result := _Cable ;
end ;


function TSerial_Cable.Component_Type : longint ;

begin
    Component_Type := Component_Type_Cable ;
end ;


function TSerial_Cable.Connect_Input( Component : TComponent ) : TUnified_Exception ;

var Code : integer ;
    Param : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := Set_Error( TSerialErr_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Add_States( Component ) ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        Param := integer( Component ) ;
        Parent.Child_Notification( self, Code, Param ) ;
    end ;
    Connect_Input := Set_Error( TSerialErr_Success ) ;
end ;


function TSerial_Cable.Connect_Output( Component : TComponent ) : TUnified_Exception ;

var Code : integer ;
    Param : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := Set_Error( TSerialErr_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Add_States( Component ) ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        Param := integer( Component ) ;
        Parent.Child_Notification( self, Code, Param ) ;
    end ;
    Connect_Output := Set_Error( TSerialErr_Success ) ;
end ;


function TSerial_Cable.Debugger : TDebug_Interface ;

begin
    Result := TSerial_Cable_Debugger.Create ;
end ;


function TSerial_Cable.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

var Code : integer ;
    Param : int64 ;

begin
    if( ( Inputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Disconnect_Input := Set_Error( TSerialErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := Set_Error( TSerialErr_Success ) ;
	Inputs.Remove( Component ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            Param := integer( Component ) ;
            Parent.Child_Notification( self, Code, Param ) ;
        end ;
        if( Inputs.Count = 0 ) then
        begin
            Clear_States ;
        end ;
    end ;
end ;


function TSerial_Cable.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

var Code : integer ;
    Param : int64 ;

begin
    if( ( Outputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Disconnect_Output := Set_Error( TSerialErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := Set_Error( TSerialErr_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            Param := integer( Component ) ;
            Parent.Child_Notification( self, Code, Param ) ;
        end ;
	Outputs.Remove( Component ) ;
        if( Outputs.Count = 0 ) then
        begin
            Clear_States ;
        end ;
    end ;
end ;


function TSerial_Cable.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Inputs.Count ) ) then
    begin
        Result := Inputs[ Index ] ;
    end else
    begin
	Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'CEF Generic Serial Cable'#0 ;

function TSerial_Cable.Name : PChar ;

begin
    Name := _Name ;
end ;


function TSerial_Cable.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Outputs.Count ) ) then
    begin
        Result := Outputs[ Index ] ;
    end else
    begin
	Output_Component := nil ;
    end ;
end ;


function TSerial_Cable.Read( Address : int64 ; Size : longint ;
            IO_Type : longint ) : boolean ;

begin
    Read := False ;
end ;


function TSerial_Cable.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    Write := Set_Error( TSerialErr_Address_Out_Of_Range ) ;
end ;


procedure TSerial_Cable.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TSerial_Cable.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TSerial_Cable.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TSerial_Cable.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


procedure TSerial_Cable.Set_Up( P : PChar ) ;

begin
    _Initialize ;
end ;


function TSerial_Cable_Cable.Serial : boolean ;

begin
    Result := True ;
end ;


function TSerial_Cable_Cable.Protocol : PChar ;

begin
    Result := nil ;
end ;


function TSerial_Cable_Cable.Transmit( Speed : int64 ;
    Value, Data_Size, Stop_Bits : longint ) : TUnified_Exception ;

var C : TComponent ;
    Loop : integer ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        C := Parent.Outputs[ Loop ] ;
        if( C.Cable <> nil ) then
        begin
            C.Cable.Receive( Parent, Speed, Value, Data_Size, Stop_bits ) ;
        end ;
    end ;
    for Loop := 0 to Parent.Inputs.Count - 1 do
    begin
        C := Parent.Inputs[ Loop ] ;
        if( Parent.Outputs.Indexof( C ) = -1 ) then
        begin
            if( C.Cable <> nil ) then
            begin
                C.Cable.Receive( Parent, Speed, Value, Data_Size, Stop_bits ) ;
            end ;
        end ;
    end ;
end ; // TSerial_Cable_Cable.Transmit


function TSerial_Cable_Cable.Transmit_String( Speed : int64 ; Value : PChar ;
    Data_Size, Stop_Bits : integer ) : TUnified_Exception ;

var Loop : integer ;
    S : string ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    S := string( Value ) ;
    for Loop := 0 to length( S ) do
    begin
        Result := Transmit( Speed, ord( S[ Loop ] ), Data_Size, Stop_Bits ) ;
    end ;
end ;


procedure TSerial_Cable_Cable.Receive( Source : TComponent ; Speed : int64 ;
    Value, Data_Size, Stop_Bits : longint ) ;

var C : TComponent ;
    Loop : integer ;
    N : integer ;
    P : int64 ;

begin
    // Pass data onto rest of cable...
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        C := Parent.Outputs[ Loop ] ;
        if( ( C.Cable <> nil ) and ( Source <> C ) ) then
        begin
            C.Cable.Receive( Parent, Speed, Value, Data_Size, Stop_bits ) ;
        end ;
    end ;
    for Loop := 0 to Parent.Inputs.Count - 1 do
    begin
        C := Parent.Inputs[ Loop ] ;
        if( ( C.Cable <> nil ) and ( Source <> C ) ) then
        begin
            C.Cable.Receive( Parent, Speed, Value, Data_Size, Stop_bits ) ;
        end ;
    end ;

    // Store data and notify parent...
    _Current_Data.Value := Value ;
    _Current_Data.Data_Size := Data_Size ;
    _Current_Data.Stop_Bits := Stop_Bits ;
    _Current_Data.Speed := Speed ;
    _Current_Data.Valid := True ;
    if( Parent <> nil ) then
    begin
        N := Child_Notice_Receive_Data ;
        Parent.Parent.Child_Notification( Parent, N, P ) ;
    end ;
end ;


function TSerial_Cable_Cable.Get_Data( var Speed : int64 ;
    var Value, Data_Size, Stop_Bits : integer ) : boolean ;

begin
    Value := _Current_Data.Value ;
    Data_Size := _Current_Data.Data_Size ;
    Stop_Bits := _Current_Data.Stop_Bits ;
    Speed := _Current_Data.Speed ;
    Result := _Current_Data.Valid ;
    _Current_Data.Valid := False ;
end ;


const Screen_Facility_Name : PChar = 'CEF Screen' ;

initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

