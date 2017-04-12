{
        Program Name : DEC_UNIBUS
        Package Name : CEF
        Purpose      : DEC PDP-11 UNIBUS CEF component
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2007 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

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

          This is the DEC UNIBUS bus component.  Note that no interrupt passing
        mechanism is needed since the master clock insures that no two events
        occur simultaneously (even if they are logically simultaneous).  This
        both simplifies the code and also prevents associated performance hits.  

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit DEC_UNIBUS ;

interface

uses // Borland...
     Windows, // TRect

     Buttons, // TSPeed_Button
     Classes, // TShiftState
     Controls, // TMouseButton
     Extctrls, // TShape
     Forms, // TForm
     Graphics, // TBitmap

     // CEF...
     _CEF, // TUser_Interface
     CEF, // TBase_Component
     Collect, // TCollection
     _DebugIn, // TDebug_Interface
     _Streams, // TCOM_Stream
     PortMan, // TPort_Manager
     UNIBUS_Form, // TFront_Panel_Form
     _UE ; // TUnified_Exception

const DECUNIBUS_Facility = -1 ;
const TDECUNIBUSErr_Success = 0 ;
const TDECUNIBUSErr_Invalid_Range = 1 ;
const TDECUNIBUSErr_Component_Not_Found = 2 ;
const TDECUNIBUSErr_Memory_Exhausted = 3 ;
const TDECUNIBUSErr_Access_Violation = 4 ;
const TDECUNIBUSErr_Address_Out_Of_Range = 5 ;
const TDECUNIBUSErr_Invalid_Component = 6 ;

type TDEC_UNIBUS_UI = class ;

     TDEC_UNIBUS = class( TBase_Component )
                        private { Instance data... }
                            Access_Mode : integer ;
                            Inputs, Outputs, Non_Peripherals : TList ;
                            _Tag : longint ;
                            _Parent : _CEF.TComponent ;
                            Passing_Data : boolean ; // True if passing data to peripherals
                            In_Read : boolean ; // True if waiting for response to read request

                        private // Internal utility routines...
                            function Default_Input : _CEF.TComponent ;
                            function Default_Output : _CEF.TComponent ;

                        public { Public instance data... }
                            _Serial_Number : integer ;
                            _User_Interface : TDEC_UNIBUS_UI ;
                            Display_Register : longint ;
                            Port_Manager : TPort_Manager ;

                        public // API...
                            function User_Interface : TUser_Interface ; override ;

                            function Facility_Code : longint ; override ;

                            function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                                override ;

                            function Terminate : TUnified_Exception ; override ;

                            function Component_Type : longint ; override ;

                            function Connect_Input( Component : _CEF.TComponent ) : TUnified_Exception ;
                                override ;

                            function Connect_Output( Component : _CEF.TComponent ) : TUnified_Exception ;
                                override ;

                            function Debugger : TDebug_Interface ; override ;

                            function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                                Memory : boolean ) : TUnified_Exception ; override ;

                            function Disconnect_Input( Component : _CEF.TComponent ) : TUnified_Exception ;
                                override ;

                            function Disconnect_Output( Component : _CEF.TComponent ) : TUnified_Exception ;
                                override ;

                            function Examine( Address : int64 ; var Size : longint ;
                                Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

                            function Input_Component( Index : longint ) : _CEF.TComponent ;
                                override ;

                            function Name : PChar ; override ;

                            function Output_Component( Index : longint ) : _CEF.TComponent ;
                                override ;

                            function Read( Address : int64 ; Size : longint ;
                                IO_Type : longint ) : boolean ; override ;

                            function Write( Address : int64 ; Value, Size : longint ;
                                IO_Type : longint ) : TUnified_Exception ; override ;

                            procedure Set_Tag( Value : longint ) ; override ;

                            function Get_Tag : longint ; override ;

                            function Get_Parent : _CEF.TComponent ; override ;

                            procedure Set_Parent( Component : _CEF.TComponent ) ;
                                override ;

                            procedure Set_Up( P : PChar ) ; override ;

                            procedure Signal_Change_Notice( Component : _CEF.TComponent ;
                                Index : longint ; Active : boolean ) ; override ;

                            procedure Child_Notification( Child : _CEF.TComponent ;
                                var Notice : longint ; var Params : int64 ) ;
                                override ;

                            function Get_Port_Name( Index : longint ) : PChar ;
                                override ;

                            function Get_Port_Description( Index : longint ) : PChar ;
                                override ;

                            function Get_Port( Index : longint ) : _CEF.TComponent ;
                                override ;

                            function Get_Port_Connection( Index : longint ) : _CEF.TComponent ;
                                override ;

                            procedure UI_Notice( Code : longint ;
                                var Data : int64 ) ; override ;

                            function Respond_To_Address( Address : int64 ;
                                Typ : integer ; Examine : boolean ) : boolean ;
                                override ;
                    end ; // TDEC_UNIBUS

     TDEC_UNIBUS_UI = class( TBase_User_Interface )
	    public // Constructors and destructors...
            destructor Destroy ; override ;

        private { Instance data... }
            Our_Screen : TFront_Panel_Form ;
            Caption : string ;
            Top, Left, Height, Width : integer ; // Saved metrics
            Data_LEDS : array[ 0..15 ] of TShape ;
            Data_Switches : array[ 0..21 ] of TSpeedButton ;
            Address_LEDs : array[ 0..21 ] of TShape ;
            Parent : TDEC_UNIBUS ;

        public // Public instance data...
            _UI : TUI_Interface ;

        private // Internal utility routines...
            procedure CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
                Shift : TShiftState ; X, Y : Integer ) ;
            procedure CB_Register_Change( Sender : TObject ) ;
            procedure CB_Toggle_Embed( Sender : TObject ) ;
            procedure _Initialize ;
            procedure Update_Address( Value : int64 ) ;
            procedure Update_Data( Value : int64 ) ;

        protected
            function Translate_Error( Code : longint ) : string ; override ;

        public { API... }
            function Get_Hidden : boolean ; override ;
            procedure Set_Hidden( Value : boolean ) ; override ;

            function Get_Parent_Window : THandle ; override ;
            procedure Set_Parent_Window( Value : THandle ) ; override ;

            function Get_Caption : PChar ; override ;
            procedure Set_Caption( Value : PChar ) ; override ;

            procedure Set_Size( Height, Width : integer ) ; override ;

            procedure Initialize ; override ;
     end ; // TDEC_UNIBUS

var _UNIBUS : TDEC_UNIBUS ;

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

const O775610 = 261000 ;
const O776500 = 261440 ;
const O777570 = 262008 ;
const O777571 = 262009 ;


implementation

uses // Borland...
     Menus, // TPopupMenu
     SysUtils, // strtoint

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     DebugInt, // TText_Debugger
     Num1s, // Num1
     _Octal,
     Parse ; // TString_Parser

{ Internal unit utility routines... }

function Get_Mem( Size : integer ) : pointer ;

var Res : pointer ;

begin
    Res := nil ;
    try
        getmem( Res, Size ) ;
    except
    end ;
    Get_Mem := Res ;
end ;



{ TCEF_Panel_Debugger methods... }

type TDEC_UNIBUS_Debugger = class( TText_Debugger )
                                private
                                    _Panel : TDEC_UNIBUS ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Panel : TDEC_UNIBUS
                                                 read _Panel
                                                 write _Panel ;
                            end ;

function TDEC_UNIBUS_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
        0 : I.Title := PChar( 'Access_Mode = ' + Access_Mode_To_String( Panel.Access_Mode ) ) ;
        1 : begin
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
        2 : begin
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
        3 : I.Title := PChar( '_Serial_Number = ' + Num1( Panel._Serial_Number ) ) ;
        4 : begin
                if( Panel._User_Interface._UI = nil ) then
                begin
                    I.Title := '_UI = nil' ;
                end else
                begin
                    I1 := Panel._User_Interface._UI.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( '_UI = ' + Pointer_To_String( Panel._User_Interface._UI ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( '_UI = ' + Pointer_To_String( Panel._User_Interface._UI ) ) ;
                end ;
            end ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TDEC_UNIBUS_Debugger.Count : longint ;

begin
    Result := 5 ;
end ;



// TDEC_UNIBUS_UI methods...

// Constructors and destructors...

destructor TDEC_UNIBUS_UI.Destroy ;

begin
    Our_Screen.Close ;
    Our_Screen := nil ;
    if( _UNIBUS <> nil ) then
    begin
        _UNIBUS.Terminate ;
    end ;

    inherited Destroy ;
end ; // TDEC_UNIBUS_UI.Destroy


function TDEC_UNIBUS_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        TDECUNIBUSErr_Success: _Error := 'Success' ;
        TDECUNIBUSErr_Invalid_Range: _Error := 'Invalid range' ;
        TDECUNIBUSErr_Component_Not_Found: _Error := 'Component not found' ;
        TDECUNIBUSErr_Access_Violation: _Error := 'Access violation' ;
        TDECUNIBUSErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        TDECUNIBUSErr_Invalid_Component: _Error := 'Invalid component' ;
        TDECUNIBUSErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { TDEC_UNIBUS_UI.Translate_Error }


var Pop_Up : TPopupMenu = nil ;

procedure TDEC_UNIBUS_UI.CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
    Shift : TShiftState ; X, Y : Integer ) ;

var M : TMenuItem ;
    P : TPoint ;

begin
    if( Button = mbRight ) then
    begin
        if( Pop_Up <> nil ) then
        begin
            Pop_Up.Free ;
        end ;
        Pop_Up := TPopupMenu.Create( Application ) ;
        M := TMenuItem.Create( Pop_Up ) ;
        if( Our_Screen.ParentWindow = 0 ) then
        begin
            M.Caption := 'Embed' ;
        end else
        begin
            M.Caption := 'Unembed' ;
        end ;
        Pop_Up.Items.Add( M ) ;
        M.OnClick := CB_Toggle_Embed ;
        P.X := X ;
        P.Y := Y ;
        P := Our_Screen.ClientToScreen( P ) ;
        Pop_Up.Popup( P.X, P.Y ) ;
    end ;
end ;


procedure TDEC_UNIBUS_UI.CB_Register_Change( Sender : TObject ) ;

begin
    if( Our_Screen.Enabled1.Checked ) then
    begin
        if( Our_Screen.Display_Register_LED.Brush.Color = clRed ) then
        begin
            Update_Data( Parent.Display_Register ) ;
        end ;
    end ;
end ;


procedure TDEC_UNIBUS_UI.CB_Toggle_Embed( Sender : TObject ) ;

begin
    _UI.Toggle_Embed( Parent ) ;
end ;


const LED_Margin = 32 ;

procedure TDEC_UNIBUS_UI._Initialize ;

begin
    // Setup front-panel...
    Data_LEDs[ 0 ] := Our_Screen.Data_LED0 ;
    Data_LEDs[ 1 ] := Our_Screen.Data_LED1 ;
    Data_LEDs[ 2 ] := Our_Screen.Data_LED2 ;
    Data_LEDs[ 3 ] := Our_Screen.Data_LED3 ;
    Data_LEDs[ 4 ] := Our_Screen.Data_LED4 ;
    Data_LEDs[ 5 ] := Our_Screen.Data_LED5 ;
    Data_LEDs[ 6 ] := Our_Screen.Data_LED6 ;
    Data_LEDs[ 7 ] := Our_Screen.Data_LED7 ;
    Data_LEDs[ 8 ] := Our_Screen.Data_LED8 ;
    Data_LEDs[ 9 ] := Our_Screen.Data_LED9 ;
    Data_LEDs[ 10 ] := Our_Screen.Data_LED10 ;
    Data_LEDs[ 11 ] := Our_Screen.Data_LED11 ;
    Data_LEDs[ 12 ] := Our_Screen.Data_LED12 ;
    Data_LEDs[ 13 ] := Our_Screen.Data_LED13 ;
    Data_LEDs[ 14 ] := Our_Screen.Data_LED14 ;
    Data_LEDs[ 15 ] := Our_Screen.Data_LED15 ;
    Address_LEDs[ 0 ] := Our_Screen.Address_LED0 ;
    Address_LEDs[ 1 ] := Our_Screen.Address_LED1 ;
    Address_LEDs[ 2 ] := Our_Screen.Address_LED2 ;
    Address_LEDs[ 3 ] := Our_Screen.Address_LED3 ;
    Address_LEDs[ 4 ] := Our_Screen.Address_LED4 ;
    Address_LEDs[ 5 ] := Our_Screen.Address_LED5 ;
    Address_LEDs[ 6 ] := Our_Screen.Address_LED6 ;
    Address_LEDs[ 7 ] := Our_Screen.Address_LED7 ;
    Address_LEDs[ 8 ] := Our_Screen.Address_LED8 ;
    Address_LEDs[ 9 ] := Our_Screen.Address_LED9 ;
    Address_LEDs[ 10 ] := Our_Screen.Address_LED10 ;
    Address_LEDs[ 11 ] := Our_Screen.Address_LED11 ;
    Address_LEDs[ 12 ] := Our_Screen.Address_LED12 ;
    Address_LEDs[ 13 ] := Our_Screen.Address_LED13 ;
    Address_LEDs[ 14 ] := Our_Screen.Address_LED14 ;
    Address_LEDs[ 15 ] := Our_Screen.Address_LED15 ;
    Address_LEDs[ 16 ] := Our_Screen.Address_LED16 ;
    Address_LEDs[ 17 ] := Our_Screen.Address_LED17 ;
    Address_LEDs[ 18 ] := Our_Screen.Address_LED18 ;
    Address_LEDs[ 19 ] := Our_Screen.Address_LED19 ;
    Address_LEDs[ 20 ] := Our_Screen.Address_LED20 ;
    Address_LEDs[ 21 ] := Our_Screen.Address_LED21 ;
    Data_Switches[ 0 ] := Our_Screen.Button0 ;
    Data_Switches[ 1 ] := Our_Screen.Button1 ;
    Data_Switches[ 2 ] := Our_Screen.Button2 ;
    Data_Switches[ 3 ] := Our_Screen.Button3 ;
    Data_Switches[ 4 ] := Our_Screen.Button4 ;
    Data_Switches[ 5 ] := Our_Screen.Button5 ;
    Data_Switches[ 6 ] := Our_Screen.Button6 ;
    Data_Switches[ 7 ] := Our_Screen.Button7 ;
    Data_Switches[ 8 ] := Our_Screen.Button8 ;
    Data_Switches[ 9 ] := Our_Screen.Button9 ;
    Data_Switches[ 10 ] := Our_Screen.Button10 ;
    Data_Switches[ 11 ] := Our_Screen.Button11 ;
    Data_Switches[ 12 ] := Our_Screen.Button12 ;
    Data_Switches[ 13 ] := Our_Screen.Button13 ;
    Data_Switches[ 14 ] := Our_Screen.Button14 ;
    Data_Switches[ 15 ] := Our_Screen.Button15 ;
    Data_Switches[ 16 ] := Our_Screen.Button16 ;
    Data_Switches[ 17 ] := Our_Screen.Button17 ;
    Data_Switches[ 18 ] := Our_Screen.Button18 ;
    Data_Switches[ 19 ] := Our_Screen.Button19 ;
    Data_Switches[ 20 ] := Our_Screen.Button20 ;
    Data_Switches[ 21 ] := Our_Screen.Button21 ;
end ; // TDEC_UNIBUS._Initialize


procedure TDEC_UNIBUS_UI.Initialize ;

begin
    Our_Screen := TFront_Panel_Form.Create( Application ) ;
    Our_Screen._UI := _UI ;
    Our_Screen.OnMouseDown := CB_MouseDown ;
    Our_Screen.Control_Box.OnMouseDown := CB_MouseDown ;
    Our_Screen.Data_Box.OnMouseDown := CB_MouseDown ;
    Our_Screen.Address_Box.OnMouseDown := CB_MouseDown ;
    Our_Screen.Caption := 'PDP-11' ;
    Caption := 'PDP-11' ;
    Our_Screen.BorderIcons := [] ;
    Our_Screen.Visible := True ;
    Our_Screen.On_Register_Change := CB_Register_Change ;
    _Initialize ;
    Our_Screen.Create_Console ;
end ;


procedure TDEC_UNIBUS_UI.Update_Address( Value : int64 ) ;

var Loop : integer ;
    Shape : TShape ;

begin
    if( not Our_Screen.Enabled1.Checked ) then
    begin
        exit ;
    end ;
    for Loop := 0 to 21 do
    begin
        Shape := Address_LEDs[ Loop ] ;
        if( ( Value and 1 ) = 0 ) then
        begin
            Shape.Brush.Color := clMaroon ;
        end else
        begin
            Shape.Brush.Color := clRed ;
        end ;
        Value := Value shr 1 ;
    end ;
end ;


procedure TDEC_UNIBUS_UI.Update_Data( Value : int64 ) ;

var Loop : integer ;
    Shape : TShape ;

begin
    if( not Our_Screen.Enabled1.Checked ) then
    begin
        exit ;
    end ;
    for Loop := 0 to 15 do
    begin
        Shape := Data_LEDs[ Loop ] ;
        if( ( Value and 1 ) = 0 ) then
        begin
            Shape.Brush.Color := clMaroon ;
        end else
        begin
            Shape.Brush.Color := clRed ;
        end ;
        Value := Value shr 1 ;
    end ;
end ;


function TDEC_UNIBUS_UI.Get_Hidden : boolean ;

begin
    Result := not Our_Screen.Visible ;
end ;


procedure TDEC_UNIBUS_UI.Set_Hidden( Value : boolean ) ;

begin
    Our_Screen.Visible := not Value ;
end ;


function TDEC_UNIBUS_UI.Get_Parent_Window : THandle ;

begin
    Result := Our_Screen.ParentWindow ;
end ;


procedure TDEC_UNIBUS_UI.Set_Parent_Window( Value : THandle ) ;

begin
    if( Value <> Our_Screen.ParentWindow ) then
    begin
        if( Value = 0 ) then // Stand-alone
        begin
            Our_Screen.ParentWindow := Value ;
            Our_Screen.Top := Top ;
            Our_Screen.Left := Left ;
            Our_Screen.Height := Height ;
            Our_Screen.Width := Width ;
            Our_Screen.Caption := Caption ;
        end else
        begin
            Top := Our_Screen.Top ;
            Left := Our_Screen.Left ;
            Height := Our_Screen.Height ;
            Width := Our_Screen.Width ;
            Our_Screen.ParentWindow := Value ;
            Our_Screen.Caption := '' ;
        end ;
    end ;
end ;


function TDEC_UNIBUS_UI.Get_Caption : PChar ;

begin
    Result := PChar( Caption ) ;
end ;


procedure TDEC_UNIBUS_UI.Set_Caption( Value : PChar ) ;

begin
    Caption := string( Value ) ;
end ;


procedure TDEC_UNIBUS_UI.Set_Size( Height, Width : integer ) ;

begin
    Our_Screen.Height := Height + GetSystemMetrics( SM_CXBORDER ) + GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Width := Width + GetSystemMetrics( SM_CXBORDER ) * 2 ;
    Our_Screen.Top := -GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Left := -GetSystemMetrics( SM_CXBORDER ) ;
end ;



// TDEC_UNIBUS methods...

function TDEC_UNIBUS.Default_Input : _CEF.TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TDEC_UNIBUS.Default_Output : _CEF.TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


// API...

function TDEC_UNIBUS.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TDEC_UNIBUS.Facility_Code : longint ;

begin
    Facility_Code := DECUNIBUS_Facility ;
end ;


function TDEC_UNIBUS.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    if( _User_Interface = nil ) then
    begin
        // General setup...
        _UNIBUS := self ;
        Inputs := TList.Create ;
        Outputs := TList.Create ;
        Non_Peripherals := TList.Create ;
        Port_Manager := TPort_Manager.Create ;
        _User_Interface := TDEC_UNIBUS_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface._UI := UI ;
        _User_Interface.Initialize ;
        UI.Want_Signals( self, True ) ;
        _User_Interface.Our_Screen.Outputs := Outputs ;
        _User_Interface.Our_Screen.Parent := self ;
        _User_Interface.Our_Screen._UI := UI ;
    end ;
    Initialize := _User_Interface.Set_Error( TDECUNIBUSErr_Success ) ;
end ; { TDEC_UNIBUS.Initialize }


function TDEC_UNIBUS.Terminate : TUnified_Exception ;

begin
    if( _User_Interface._UI <> nil ) then
    begin
        _User_Interface._UI.Termination_Notice( self ) ;
    end ;
    Terminate := _User_Interface.Set_Error( TDECUNIBUSErr_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    _UNIBUS := nil ;
    Port_Manager.Free ;
    Port_Manager := nil ;
    Free ;
end ; { TDEC_UNIBUS.Terminate }



function TDEC_UNIBUS.Component_Type : longint ;

begin
    Component_Type := Component_Type_UI ;
end ;


function TDEC_UNIBUS.Connect_Input( Component : _CEF.TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( TDECUNIBUSErr_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    if( not _User_Interface.Our_Screen.Adding_Peripheral ) then
    begin
        if( Component.Name = 'DEC DL11' ) then
        begin
            Port_Manager.Add_Port( 'DL11', Component ) ;
        end else
        if( Component.Name = 'DEC KW11L' ) then
        begin
            Port_Manager.Add_Port( 'KW11L', Component ) ;
        end else
        begin
            Non_Peripherals.Add( Component ) ;
        end ;
    end ;
    Connect_Input := _User_Interface.Set_Error( TDECUNIBUSErr_Success ) ;
end ;


function TDEC_UNIBUS.Connect_Output( Component : _CEF.TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( TDECUNIBUSErr_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    if( not _User_Interface.Our_Screen.Adding_Peripheral ) then
    begin
        Non_Peripherals.Add( Component ) ;
    end ;
    Connect_Output := _User_Interface.Set_Error( TDECUNIBUSErr_Success ) ;
end ;


function TDEC_UNIBUS.Debugger : TDebug_Interface ;

begin
    Result := TDEC_UNIBUS_Debugger.Create ;
    TDEC_UNIBUS_Debugger( Result ).Panel := Self ;
end ;


function TDEC_UNIBUS.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

    procedure Do_Deposit( C : TComponent ) ;

    var UEC : TUnified_Exception ;

    begin
        if( C <> nil ) then
        begin
            UEC := C.Deposit( Address, Size, Buffer, Memory ) ;
            if( UEC <> nil ) then
            begin
                Result := UEC ;
            end ;
        end ;
    end ;

var Loop, Value : integer ;

begin
    Deposit := _User_Interface.Set_Error( TDECUNIBUSErr_Address_Out_Of_Range ) ;
    if( not Memory ) then
    begin
        exit ;
    end ;
    if(
        ( Address = O777570 )
        or
        ( Address = O777571 )
      ) then // Display register
    begin
        if( not _User_Interface.Our_Screen.Support_Switch_Register ) then
        begin
            exit ;
        end ;
        Value := 0 ;
        if( odd( Address ) ) then
        begin
            Size := 8 ;
        end ;
        move( Buffer^, Value, ( Size + 7 ) div 8 ) ;
        if( Size = 8 ) then
        begin
            if( odd( Address ) ) then // Writing to high byte
            begin
                Display_Register := ( Value shl 8 ) or low( Display_Register ) ;
            end else
            begin
                Display_Register := high( Display_Register ) or low( Value ) ;
            end ;
        end else
        begin
            Display_Register := Value ;
        end ;
        Deposit := _User_Interface.Set_Error( 0 ) ;
        if( _User_Interface.Our_Screen.Display_Register_LED.Brush.Color = clRed ) then
        begin
            _User_Interface.Update_Data( Display_Register ) ;
        end ;
    end else
    begin
        Do_Deposit( _User_Interface.Our_Screen.KE11_Component ) ;
        Do_Deposit( _User_Interface.Our_Screen.DL11W ) ;
        Do_Deposit( _User_Interface.Our_Screen.Console ) ;
        for Loop := 0 to _User_Interface.Our_Screen.KL11s.Count - 1 do
        begin
            Do_Deposit( _User_Interface.Our_Screen.KL11s[ Loop ].Component ) ;
        end ;
        for Loop := 0 to _User_Interface.Our_Screen.DL11Ds.Count - 1 do
        begin
            Do_Deposit( _User_Interface.Our_Screen.DL11Ds[ Loop ].Component ) ;
        end ;
    end ;
end ; // TDEC_UNIBUS.Deposit


function TDEC_UNIBUS.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Disconnect_Input := _User_Interface.Set_Error( TDECUNIBUSErr_Component_Not_Found ) ;
    end else
    begin
	    Disconnect_Input := _User_Interface.Set_Error( TDECUNIBUSErr_Success ) ;
	    Inputs.Remove( Component ) ;
    end ;
    Non_Peripherals.Remove( Component ) ;
end ;


function TDEC_UNIBUS.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Disconnect_Output := _User_Interface.Set_Error( TDECUNIBUSErr_Component_Not_Found ) ;
    end else
    begin
	    Disconnect_Output := _User_Interface.Set_Error( TDECUNIBUSErr_Success ) ;
	    Outputs.Remove( Component ) ;
    end ;
    Non_Peripherals.Remove( Component ) ;
end ;


function TDEC_UNIBUS.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

    procedure Do_Examine( C : TComponent ) ;

    var UEC : TUnified_Exception ;

    begin
        if( C <> nil ) then
        begin
            UEC := C.Examine( Address, Size, Buffer, Memory ) ;
            if( UEC <> nil ) then
            begin
                Result := UEC ;
            end ;
        end ;
    end ;

var Data, Loop : integer ;

begin
    if( not Memory ) then
    begin
        Result := Set_Error( TDECUNIBUSErr_Address_Out_Of_Range ) ;
        exit ;
    end ;
    Result := Set_Error( 0 ) ;
    if(
        ( Address = O777570 )
        or
        ( Address = O777571 )
      ) then // Front panel switches
    begin
        if( not _User_Interface.Our_Screen.Support_Switch_Register ) then
        begin
            Result := Set_Error( TDECUNIBUSErr_Address_Out_Of_Range ) ;
            exit ;
        end ;
        if( Size > 16 ) then
        begin
            Size := 16 ;
        end ;
        if( odd( Address ) ) then
        begin
            Size := 8 ;
        end ;
        Data := 0 ;
        for Loop := 15 downto 0 do
        begin
            Data := Data shl 1 ;
            if( TSpeedButton( _User_Interface.Data_Switches[ Loop ] ).Down ) then
            begin
                Data := Data or 1 ;
            end ;
        end ;
        if( odd( Address ) ) then
        begin
            Data := Data shr 8 ;
        end ;
        move( Data, Buffer^, ( Size + 7 ) div 8 ) ;
    end else
    begin
        Do_Examine( _User_Interface.Our_Screen.KE11_Component ) ;
        Do_Examine( _User_Interface.Our_Screen.DL11W ) ;
        Do_Examine( _User_Interface.Our_Screen.Console ) ;
        for Loop := 0 to _User_Interface.Our_Screen.KL11s.Count - 1 do
        begin
            Do_Examine( _User_Interface.Our_Screen.KL11s[ Loop ].Component ) ;
        end ;
        for Loop := 0 to _User_Interface.Our_Screen.DL11Ds.Count - 1 do
        begin
            Do_Examine( _User_Interface.Our_Screen.DL11Ds[ Loop ].Component ) ;
        end ;
    end ; // if
end ; // TDEC_UNIBUS.Examine


function TDEC_UNIBUS.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Inputs.Count ) ) then
    begin
        Result := Inputs[ Index ] ;
    end else
    begin
	    Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'UNIBUS'#0 ;

function TDEC_UNIBUS.Name : PChar ;

begin
    Name := _Name ;
end ;


function TDEC_UNIBUS.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Outputs.Count ) ) then
    begin
        Result := Outputs[ Index ] ;
    end else
    begin
        Output_Component := nil ;
    end ;
end ;


function TDEC_UNIBUS.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

    procedure Do_Read( C : TComponent ) ;

    begin
        if( C <> nil ) then
        begin
            if( C.Read( Address, Size, IO_Type ) ) then
            begin
                Result := True ;
            end ;
        end ;
    end ;


var Data, Loop : integer ;
    UEC : TUnified_Exception ;

begin
    In_Read := True ;
    try
        Read := False ;
        if( ( IO_Type <> IO_Type_Memory ) and ( IO_Type <> IO_Type_Bus ) ) then
        begin
            exit ;
        end ;
        if(
            ( IO_Type = IO_Type_Memory )
            and
            (
              ( Address = O777570 )
              or
              ( Address = O777571 )
            )
          ) then // Front panel switches
        begin
            if( not _User_Interface.Our_Screen.Support_Switch_Register ) then
            begin
                exit ;
            end ;
            UEC := Examine( Address, Size, @Data, True ) ;
            if( UEC <> nil ) then
            begin
                exit ;
            end ;
            Read := True ;
            if( Default_Output <> nil ) then
            begin
                for Loop := 0 to Outputs.Count - 1 do
                begin
                    TComponent( Outputs[ Loop ] ).Write_String( Address, @Data, 8, IO_Type_IO ) ;
                end ;
            end ;
            if( Default_Input <> nil ) then
            begin
                for Loop := 0 to Inputs.Count - 1 do
                begin
                    try
                        TComponent( Inputs[ Loop ] ).Write_String( Address, @Data, 8, IO_Type_IO ) ;
                    except
                    end ;
                end ;
            end ;
        end else
        begin
            Passing_Data := True ;
            try
                Do_Read( _User_Interface.Our_Screen.KE11_Component ) ;
                Do_Read( _User_Interface.Our_Screen.DL11W ) ;
                Do_Read( _User_Interface.Our_Screen.Console ) ;
                for Loop := 0 to _User_Interface.Our_Screen.KL11s.Count - 1 do
                begin
                    Do_Read( _User_Interface.Our_Screen.KL11s[ Loop ].Component ) ;
                end ;
                for Loop := 0 to _User_Interface.Our_Screen.DL11Ds.Count - 1 do
                begin
                    Do_Read( _User_Interface.Our_Screen.DL11Ds[ Loop ].Component ) ;
                end ;
            finally
                Passing_Data := False ;
            end ;
        end ;
        if( _User_Interface.Our_Screen.Enabled1.Checked ) then
        begin
            _User_Interface.Our_Screen.Read_LED.Brush.Color := clRed ;
            _User_Interface.Our_Screen.Write_LED.Brush.Color := clMaroon ;
            _User_Interface.Update_Address( Address ) ;
        end ;
    finally
        In_Read := False ;
    end ;
end ; // TDEC_UNIBUS.Read


function TDEC_UNIBUS.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

    procedure Do_Write( C : TComponent ) ;

    var UEC : TUnified_Exception ;

    begin
        if( C <> nil ) then
        begin
            UEC := C.Write( Address, Value, Size, IO_Type ) ;
            if( UEC = nil ) then
            begin
                Result := UEC ;
            end ;
        end ;
    end ;

var C : TComponent ;
    Loop : integer ;
    UEC : TUnified_Exception ;

begin
    Result := _User_Interface.Set_Error( TDECUNIBUSErr_Address_Out_Of_Range ) ;
    if( Passing_Data ) then
    begin
        for Loop := 0 to Inputs.Count - 1 do
        begin
            C := TComponent( Inputs[ Loop ] ) ;
            if( Non_Peripherals.IndexOf( C ) <> -1 ) then // Not a peripheral
            begin
                UEC := C.Write( Address, Value, Size, IO_Type ) ;
                if( UEC = nil ) then
                begin
                    Result := UEC ;
                end ;
            end ;
        end ; // for Loop := 0 to Inputs.Count - 1
        for Loop := 0 to Outputs.Count - 1 do
        begin
            C := TComponent( Outputs[ Loop ] ) ;
            if( Non_Peripherals.IndexOf( C ) <> -1 ) then // Not a peripheral
            begin
                if( Inputs.IndexOf( C ) = -1 ) then // Didn't already set
                begin
                    UEC := C.Write( Address, Value, Size, IO_Type ) ;
                    if( UEC = nil) then
                    begin
                        Result := UEC ;
                    end ;
                end ;
            end ;
        end ; // for Loop := 0 to Outputs.Count - 1
        if( _User_Interface.Our_Screen.Bus_Reg_LED.Brush.Color = clRed ) then
        begin
            _User_Interface.Update_Data( Value ) ;
        end ;
        exit ;
    end ;
    Write := _User_Interface.Set_Error( TDECUNIBUSErr_Address_Out_Of_Range ) ;
    if( IO_Type <> IO_Type_Memory ) then
    begin
        exit ; // Don't update panel except for memory access
    end ;
    if( In_Read ) then // This is a response to the previous Read operation
    begin
        In_Read := False ;
    end else
    begin
        _User_Interface.Update_Address( Address ) ;
        if( _User_Interface.Our_Screen.Enabled1.Checked ) then
        begin
            _User_Interface.Our_Screen.Read_LED.Brush.Color := clMaroon ;
            _User_Interface.Our_Screen.Write_LED.Brush.Color := clRed ;
        end ;
        if(
            ( Address = O777570 )
            or
            ( Address = O777571 )
          ) then // Display register
        begin
            if( not _User_Interface.Our_Screen.Support_Switch_Register ) then
            begin
                exit ;
            end ;
            Result := Deposit( Address, Size, @Value, True ) ;
        end else
        begin
            Do_Write( _User_Interface.Our_Screen.KE11_Component ) ;
            Do_Write( _User_Interface.Our_Screen.DL11W ) ;
            Do_Write( _User_Interface.Our_Screen.Console ) ;
            for Loop := 0 to _User_Interface.Our_Screen.KL11s.Count - 1 do
            begin
                Do_Write( _User_Interface.Our_Screen.KL11s[ Loop ].Component ) ;
            end ;
            for Loop := 0 to _User_Interface.Our_Screen.DL11Ds.Count - 1 do
            begin
                Do_Write( _User_Interface.Our_Screen.DL11Ds[ Loop ].Component ) ;
            end ;
        end ;
    end ;
    if( _User_Interface.Our_Screen.Bus_Reg_LED.Brush.Color = clRed ) then
    begin
        _User_Interface.Update_Data( Value ) ;
    end ;
end ; // TDEC_UNIBUS.Write


procedure TDEC_UNIBUS.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TDEC_UNIBUS.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TDEC_UNIBUS.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TDEC_UNIBUS.Set_Parent( Component : TComponent ) ;

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


procedure TDEC_UNIBUS.Set_Up( P : PChar ) ;

var List : TStringList ;
    Loop : integer ;
    Model : string ;
    Parser : TString_Parser ;
    S : string ;
    KL11s, DL11s, KE11s, DL11Ws : integer ;

begin
    // Setup...
    KE11s := 0 ;
    KL11s := 0 ;
    DL11s := 0 ;
    DL11Ws := 1 ;
    Model := '' ;
    Parser := TString_Parser.Create ;
    try
        Parser.Set_Source( string( P ) ) ;
        List := TStringList.Create ;

        // Process setup string...
        try
            S := uppercase( Parser.Token ) ;
            while( S <> '' ) do
            begin
                if( S = 'TITLE' ) then
                begin
                    S := uppercase( Parser.Token ) ;
                    if( copy( S, 1, 1 ) = '"' ) then
                    begin
                        S := copy( S, 2, length( S ) ) ;
                        if( copy( S, length( S ), 1 ) = '"' ) then
                        begin
                            setlength( S, length( S ) - 1 ) ;
                        end ;
                    end ;
                    _User_Interface.Our_Screen.Caption := S ;
                    _User_Interface.Our_Screen.Label22.Caption := S ;
                    _User_Interface.Caption := S ;
                end else
                if( S = 'KE11' ) then
                begin
                    KE11s := 1 ;
                end else
                if( S = 'NOKE11' ) then
                begin
                    KE11s := 0 ;
                end else
                if( S = 'DL11W' ) then
                begin
                    DL11Ws := 1 ;
                end else
                if( S = 'NODL11W' ) then
                begin
                    DL11Ws := 0 ;
                end else
                if( S = 'KL11S' ) then
                begin
                    S := uppercase( Parser.Token ) ;
                    try
                        KL11s := strtoint( S ) ;
                    except
                    end ;
                end else
                if( S = 'SWITCHES' ) then
                begin
                    _User_Interface.Our_Screen.Support_Switch_Register := True ;
                end else
                if( S = 'NOSWITCHES' ) then
                begin
                    _User_Interface.Our_Screen.Support_Switch_Register := False ;
                end else
                if( S = 'DL11S' ) then
                begin
                    S := uppercase( Parser.Token ) ;
                    try
                        DL11s := strtoint( S ) ;
                    except
                    end ;
                end else
                if( S = 'TERMINAL' ) then
                begin
                    List.Add( Parser.Token ) ;
                end else
                if( S = 'MODEL' ) then
                begin
                    Model := Parser.Token ;
                end ;
                S := uppercase( Parser.Token ) ;
            end ; // while( S <> '' )

            // Final setup...
            _User_Interface._Initialize ;
            _User_Interface.Our_Screen.Configure( DL11Ws, KL11s, DL11s, KE11s ) ;
            if( Model = '' ) then
            begin
                Model := '34' ;
            end ;
            _User_Interface.Our_Screen.Label22.Caption :=
                _User_Interface.Our_Screen.Label22.Caption + Model ;

            for Loop := 0 to List.Count - 1 do
            begin
                _User_Interface.Our_Screen.Attach_Terminals( List ) ;
            end ;
        finally
            List.Free ;
        end ;
    finally
        Parser.Free ;
    end ;
end ; // TDEC_UNIBUS.Set_Up


procedure TDEC_UNIBUS.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

var S : string ;

begin
    if( Active ) then
    begin
        if( not _User_Interface.Our_Screen.Enabled1.Checked ) then
        begin
            exit ;
        end ;
        S := string( Component.Signal_Name( Index ) ) ;
        if( S = '16 BIT' ) then
        begin
            _User_Interface.Our_Screen.Set_Addressing( 16 ) ;
        end else
        if( S = '18 BIT' ) then
        begin
            _User_Interface.Our_Screen.Set_Addressing( 18 ) ;
        end else
        if( S = '22 BIT' ) then
        begin
            _User_Interface.Our_Screen.Set_Addressing( 22 ) ;
        end else
        if( S = 'KERNEL' ) then
        begin
            _User_Interface.Our_Screen.Set_Mode( 0 ) ;
        end else
        if( S = 'SUPER' ) then
        begin
            _User_Interface.Our_Screen.Set_Mode( 1 ) ;
        end else
        if( S = 'USER' ) then
        begin
            _User_Interface.Our_Screen.Set_Mode( 3 ) ;
        end ;
    end ;
end ; // TDEC_UNIBUS.Signal_Change_Notice


procedure TDEC_UNIBUS.Child_Notification( Child : TComponent ;
    var Notice : longint ; var Params : int64 ) ;

var C : TComponent ;
    Loop : integer ;

begin
    if( Child = nil ) then
    begin
        exit ;
    end ;
    if( Notice = Child_Notice_Terminating ) then
    begin
        _UNIBUS.Port_Manager.Remove_Component( Child ) ;
    end else
    if( Notice = Child_Notice_Connect ) then
    begin
        C := TComponent( pointer( Params ) ) ;
        Loop := Port_Manager.Index_Of_Component( C ) ;
        if( Loop <> -1 ) then
        begin
            Port_Manager.Set_Terminal( Loop, C ) ;
        end ;
    end else
    if( Notice = Child_Notice_Disconnect ) then
    begin
        _UNIBUS.Port_Manager.Remove_Component( Child ) ;
    end ;
end ;


function TDEC_UNIBUS.Get_Port_Name( Index : longint ) : PChar ;

begin
    if( ( Index < 0 ) or ( Index > Port_Manager.Count - 1 ) ) then
    begin
        Result := nil ;
        exit ;
    end ;
    Result := Port_Manager.Port_Name( Index ) ;
end ;


function TDEC_UNIBUS.Get_Port_Description( Index : longint ) : PChar ;

var C : TComponent ;

begin
    if( ( Index < 0 ) or ( Index > Port_Manager.Count - 1 ) ) then
    begin
        Result := nil ;
        exit ;
    end ;
    C := Port_Manager.Port_Device( Index ) ;
    if( C = nil ) then
    begin
        Result := nil ;
    end else
    begin
        Result := C.Name ;
    end ;
end ;


function TDEC_UNIBUS.Get_Port( Index : longint ) : TComponent ;

begin
    Result := nil ;
    if( ( Index < 0 ) or ( Index > Port_Manager.Count - 1 ) ) then
    begin
        exit ;
    end ;
    Result := Port_Manager.Port_Device( Index ) ;
end ;


function TDEC_UNIBUS.Get_Port_Connection( Index : longint ) : TComponent ;

begin
    Result := nil ;
    if( ( Index < 0 ) or ( Index > Port_Manager.Count - 1 ) ) then
    begin
        exit ;
    end ;
    Result := Port_Manager.Port_Connection( Index ) ;
end ;


procedure TDEC_UNIBUS.UI_Notice( Code : longint ; var Data : int64 ) ;

begin
    if( Code = UI_Notice_Changed_Run_State ) then
    begin
        _User_Interface.Our_Screen.Run_Button.Down := ( Data = 1 ) ;
    end ;
end ;


function TDEC_UNIBUS.Respond_To_Address( Address : int64 ; Typ : integer ;
    Examine : boolean ) : boolean ;

    function Check( C : TComponent ) : boolean ;

    begin
        if( C = nil ) then
        begin
            Result := False ;
        end else
        begin
            Result := C.Respond_To_Address( Address, Typ, Examine ) ;
        end ;
    end ;

var Loop : integer ;

begin
    Result := False ;
    if( Typ <> IO_Type_Memory ) then
    begin
        exit ;
    end ;
    if(
         ( Address = O777570 )
         or
         ( Address = O777571 )
      ) then
    begin
        if( not _User_Interface.Our_Screen.Support_Switch_Register ) then
        begin
            exit ;
        end ;
        Result := True ;
        exit ;
    end ;
    if( Check( _User_Interface.Our_Screen.KE11_Component ) ) then
    begin
        Result := True ;
        exit ;
    end ;
    if( Check( _User_Interface.Our_Screen.DL11W ) ) then
    begin
        Result := True ;
        exit ;
    end ;
    if( Check( _User_Interface.Our_Screen.Console ) ) then
    begin
        Result := True ;
        exit ;
    end ;
    for Loop := 0 to _User_Interface.Our_Screen.KL11s.Count - 1 do
    begin
        if( Check( _User_Interface.Our_Screen.KL11s[ Loop ].Component ) ) then
        begin
            Result := True ;
            exit ;
        end ;
    end ;
    for Loop := 0 to _User_Interface.Our_Screen.DL11Ds.Count - 1 do
    begin
        if( Check( _User_Interface.Our_Screen.DL11Ds[ Loop ].Component ) ) then
        begin
            Result := True ;
            exit ;
        end ;
    end ;
end ; // TDEC_UNIBUS.Respond_To_Address


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

