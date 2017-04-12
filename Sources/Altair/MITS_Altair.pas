{
        Program Name : MITS_Altair
        Package Name : CEF
        Purpose      : MITS Altair 8800 CEF component
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
        *    DATE        BY          REASON                         *
        *                                                           *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This component handles the system unit of an Altair 8800 and
        IMSAI 8080.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit MITS_Altair ;

interface

uses // Borland...
     Windows, // TRect

     Classes, // TShiftState
     Controls, // TMouseButton
     Forms, // TForm
     Graphics, // TBitmap

     // CEF...
     _CEF, // TUser_Interface
     CEF, // TBase_Component
     Collect, // TCollection
     _DebugIn, // TDebug_Interface
     _Streams, // TCOM_Stream
     Altair_Form, // TFront_Panel_Form
     _UE ; // TUnified_Exception

const MITSAltair_Facility = -1 ;
const TMITSAltairErr_Success = 0 ;
const TMITSAltairErr_Invalid_Range = 1 ;
const TMITSAltairErr_Component_Not_Found = 2 ;
const TMITSAltairErr_Memory_Exhausted = 3 ;
const TMITSAltairErr_Access_Violation = 4 ;
const TMITSAltairErr_Address_Out_Of_Range = 5 ;
const TMITSAltairErr_Invalid_Component = 6 ;
const TMITSAltairErr_Component_Already_Connected = 7 ;

type TMITS_Altair_UI = class ;

     TMITS_Altair = class( TBase_Component )
                        private { Instance data... }
                            Access_Mode : integer ;
                            Inputs, Outputs : TList ;
                            _Tag : longint ;
                            _Parent : TComponent ;
                            In_Read : boolean ; // True if waiting for response to read request
                            Temp_Port_Name : string ;
                            Temp_Port_Description : string ;
                            
                        private // Internal utility routines...
                            function Default_Input : TComponent ;
                            function Default_Output : TComponent ;
                            function Port_Index( Index : integer ) : integer ;

                        public { Public instance data... }
                            _Serial_Number : integer ;
                            _User_Interface : TMITS_Altair_UI ;
                            Port_Component_Handles : array[ boolean, 0..255 ] of THandle ; // Library module handles for Port_Components
                            Port_Components : array[ boolean, 0..255 ] of TComponent ; // Components for ports (e.g. 88-SIO)
                            Port_Connections : array[ boolean, 0..255 ] of TComponent ; // Connections to ports (e.g. VT52 connected to an 88-SIO)

                        public // API...
                            function User_Interface : TUser_Interface ; override ;

                            function Facility_Code : longint ; override ;

                            function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                                override ;

                            function Terminate : TUnified_Exception ; override ;

                            function Component_Type : longint ; override ;

                            function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                                override ;

                            function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                                override ;

                            function Debugger : TDebug_Interface ; override ;

                            function Disconnect_Input( Component : TComponent ) : TUnified_Exception ;
                                override ;

                            function Disconnect_Output( Component : TComponent ) : TUnified_Exception ;
                                override ;

                            function Input_Component( Index : longint ) : TComponent ;
                                override ;

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

                            procedure Set_Parent( Component : TComponent ) ;
                                override ;

                            procedure Set_Up( P : PChar ) ; override ;

                            procedure Signal_Change_Notice( Component : TComponent ;
                                Index : longint ; Active : boolean ) ; override ;

                            procedure Child_Notification( Child : TComponent ;
                                var Notice : longint ; var Params : int64 ) ;
                                override ;

                            function Get_Port_Name( Index : longint ) : PChar ;
                                override ;

                            function Get_Port_Description( Index : longint ) : PChar ;
                                override ;

                            function Get_Port( Index : longint ) : TComponent ;
                                override ;

                            function Get_Port_Connection( Index : longint ) : TComponent ;
                                override ;

                            procedure UI_Notice( Code : longint ;
                                var Data : int64 ) ; override ;
                    end ; // TMITS_Altair

     TMITS_Altair_UI = class( TBase_User_Interface )
                           public // Constructors and destructors...
                                   constructor Create ;
                                   destructor Destroy ; override ;

                               private { Instance data... }
                                   Our_Screen : TFront_Panel_Form ;
                                   Caption : string ;
                                   Top, Left, Height, Width : integer ; // Saved metrics
                                   _Data_Bits, _Address_Bits : integer ;
                                   Data_LEDS, Data_Labels, Data_Switches : TList ;
                                   Address_LEDs, Address_Labels : TList ;
                                   State_LEDs, State_Labels : TList ;
                                   IMSAI_LEDs : TList ;
                                   Parent : TMITS_Altair ;

                               public // Public instance data...
                                   _UI : TUI_Interface ;

                               private // Internal utility routines...
                                   procedure Add_States( Component : TComponent ) ;
                                   procedure CB_MouseDown( Sender : TObject ;
                                       Button : TMouseButton ;
                                       Shift : TShiftState ; X, Y : Integer ) ;
                                   procedure CB_Toggle_Embed( Sender : TObject ) ;
                                   function State_Index( S : string ) : integer ;
                                   procedure Clear_States ;
                                   procedure _Initialize ;
                                   procedure Update_Address( Value : int64 ) ;
                                   procedure Update_Data( Value : int64 ) ;

                               protected
                                   function Translate_Error( Code : longint ) : string ;
                                       override ;

                               public { API... }
                                   procedure Initialize ; override ;

                                   function Get_Hidden : boolean ; override ;
                                   procedure Set_Hidden( Value : boolean ) ;
                                       override ;

                                   function Get_Parent_Window : THandle ;
                                       override ;
                                   procedure Set_Parent_Window( Value : THandle ) ;
                                       override ;

                                   function Get_Caption : PChar ; override ;
                                   procedure Set_Caption( Value : PChar ) ;
                                       override ;

                                   procedure Set_Size( Height, Width : integer ) ;
                                       override ;
                            end ; // TMITS_Altair_UI

var _Altair : TMITS_Altair ;

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
     Buttons, // TSPeedButton
     Menus, // TPopupMenu
     ExtCtrls, // TShape
     StdCtrls, // TLabel
     SysUtils, // strtoint

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     DebugInt, // TText_Debugger
     Num1s, // Num1
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

type TMITS_Altair_Debugger = class( TText_Debugger )
                                private
                                    _Panel : TMITS_Altair ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Panel : TMITS_Altair
                                                 read _Panel
                                                 write _Panel ;
                            end ;

function TMITS_Altair_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TMITS_Altair_Debugger.Count : longint ;

begin
    Result := 5 ;
end ;



// TMITS_Altair_UI methods...

// Constructors and destructors...

constructor TMITS_Altair_UI.Create ;

begin
    inherited Create ;
    
    Our_Screen := TFront_Panel_Form.Create( Application ) ;
    Our_Screen.OnMouseDown := CB_MouseDown ;
    Our_Screen.Control_Box.OnMouseDown := CB_MouseDown ;
    Our_Screen.Data_Box.OnMouseDown := CB_MouseDown ;
    Our_Screen.Address_Box.OnMouseDown := CB_MouseDown ;
    Our_Screen.Caption := 'MITS Altair 8800' ;
    Caption := 'MITS Altair 8800' ;
    Our_Screen.BorderIcons := [] ;
    Our_Screen.Visible := True ;
    _Data_Bits := 8 ;
    _Address_Bits := 16 ;
    Data_LEDs := TList.Create ;
    Address_LEDs := TList.Create ;
    Data_Labels := TList.Create ;
    Address_Labels := TList.Create ;
    Data_Switches := TList.Create ;
    State_LEDS := TList.Create ;
    State_Labels := TList.Create ;
    IMSAI_LEDs := TList.Create ;
    _Initialize ;
end ;


destructor TMITS_Altair_UI.Destroy ;

var Loop : integer ;

begin
    Data_LEDS.Free ;
    Address_LEDS.Free ;
    Data_LEDS := nil ;
    Address_LEDS := nil ;
    Data_Labels.Free ;
    Address_Labels.Free ;
    Data_Labels := nil ;
    Address_Labels := nil ;
    State_LEDS.Free ;
    State_LEDS := nil ;
    State_Labels.Free ;
    State_Labels := nil ;
    IMSAI_LEDs.Free ;
    IMSAI_LEDs := nil ;
    Data_Switches.Free ;
    Data_Switches := nil ;
    Our_Screen.Close ;
    Our_Screen := nil ;
    if( _Altair <> nil ) then
    begin
        for Loop := 0 to 255 do
        begin
            if( _Altair.Port_Components[ True, Loop ] <> nil ) then
            begin
                _Altair.Port_Components[ True, Loop ].Terminate ;
                _Altair.Port_Components[ True, Loop ] := nil  ;
            end ;
            if( _Altair.Port_Components[ False, Loop ] <> nil ) then
            begin
                _Altair.Port_Components[ False, Loop ].Terminate ;
                _Altair.Port_Components[ False, Loop ] := nil  ;
            end ;

            if( _Altair.Port_Connections[ True, Loop ] <> nil ) then
            begin
                _Altair.Port_Connections[ True, Loop ].Terminate ;
                _Altair.Port_Connections[ True, Loop ] := nil  ;
            end ;
            if( _Altair.Port_Connections[ False, Loop ] <> nil ) then
            begin
                _Altair.Port_Connections[ False, Loop ].Terminate ;
                _Altair.Port_Connections[ False, Loop ] := nil  ;
            end ;
            if( _Altair.Port_Component_Handles[ True, Loop ] <> 0 ) then
            begin
                FreeLibrary( _Altair.Port_Component_Handles[ True, Loop ] ) ;
                _Altair.Port_Component_Handles[ True, Loop ] := 0 ;
            end ;
            if( _Altair.Port_Component_Handles[ False, Loop ] <> 0 ) then
            begin
                FreeLibrary( _Altair.Port_Component_Handles[ False, Loop ] ) ;
                _Altair.Port_Component_Handles[ False, Loop ] := 0 ;
            end ;
        end ;
    end ;

    inherited Destroy ;
end ; // TMITS_Altair_UI.Destroy


function TMITS_Altair_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        TMITSAltairErr_Success: _Error := 'Success' ;
        TMITSAltairErr_Invalid_Range: _Error := 'Invalid range' ;
        TMITSAltairErr_Component_Not_Found: _Error := 'Component not found' ;
        TMITSAltairErr_Access_Violation: _Error := 'Access violation' ;
        TMITSAltairErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        TMITSAltairErr_Invalid_Component: _Error := 'Invalid component' ;
        TMITSAltairErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { TMITS_Altair_UI.Translate_Error }


procedure TMITS_Altair_UI.Add_States( Component : TComponent ) ;

var B : boolean ;
    L : TLabel ;
    Loop : integer ;
    S : string ;
    Shape : TShape ;
    X : integer ;

begin
    X := ( State_LEDs.Count + 5 ) * 40 ; // Position of next LED is after 4 fixed LEDs and count
    for Loop := 0 to Component.Signal_Count - 1 do
    begin
        if( not Component.Signal_Out( Loop ) ) then
        begin
            continue ;
        end ;
        S := string( Component.Signal_Name( Loop ) ) ;
        if( State_Index( S ) <> -1 ) then
        begin
            continue ;
        end ;

        Shape := TShape.Create( Our_Screen.Control_Box ) ;
        Shape.Parent := Our_Screen.Control_Box ;
        Shape.Height := 15 ;
        Shape.Width := 15 ;
        B := False ;
        Component.Get_Signal( PChar( S ), B ) ;
        if( B ) then
        begin
            Shape.Brush.Color := clRed ;
        end else
        begin
            Shape.Brush.Color := clMaroon ;
        end ;
        Shape.Shape := stCircle ;
        Shape.Top := 19 ;
        Shape.Left := X ;
        State_LEDs.Add( Shape ) ;

        L := TLabel.Create( Our_Screen.Control_Box ) ;
        L.Parent := Our_Screen.Control_Box ;
        L.AutoSize := True ;
        L.Caption := S ;
        L.Left := Shape.Left ;
        L.Top := 45 ;
        State_Labels.Add( L ) ;

        X := X + 45 ;
    end ;
end ; // TMITS_Altair.Add_States


var Pop_Up : TPopupMenu = nil ;

procedure TMITS_Altair_UI.CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
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


procedure TMITS_Altair_UI.CB_Toggle_Embed( Sender : TObject ) ;

begin
    _UI.Toggle_Embed( Parent ) ;
end ;


function TMITS_Altair_UI.State_Index( S : string ) : integer ;

var Loop : integer ;

begin
    Result := -1 ; // Assume failure
    S := uppercase( S ) ;
    for Loop := 0 to State_Labels.Count - 1 do
    begin
        if( uppercase( TLabel( State_Labels[ Loop ] ).Caption ) = S ) then
        begin
            Result := Loop ;
            exit ;
        end ;
    end ;
end ;


procedure TMITS_Altair_UI.Clear_States ;

var Loop : integer ;

begin
    for Loop := 0 to State_LEDs.Count - 1 do
    begin
        TShape( State_LEDs[ Loop ] ).Free ;
        TLabel( State_Labels[ Loop ] ).Free ;
    end ;
    State_LEDs.Clear ;
    State_Labels.Clear ;
end ;


procedure TMITS_Altair_UI.Initialize ;

var C : TComponent ;
    H : THandle ;
    P : function( Serial_Number : integer ; UI : TUI_Interface ) : Tcomponent ; stdcall ;

begin
    // Create default port...
    H := LoadLibrary( _UI.Get_Component_Filename( 'SIO' ) ) ;
    C := nil ;
    if( H <> 0 ) then
    begin
        P := GetProcAddress( H, 'Make_Instance' ) ;
        if( assigned( P ) ) then
        begin
            C := P( 0, _UI ) ;
        end else
        begin
            H := 0 ;
        end ;
    end ;

    _Altair.Port_Component_Handles[ True, 0 ] := H ;
    _Altair.Port_Components[ True, 0 ] := C ;
    if( C <> nil ) then
    begin
        C.Parent := _Altair ;
        C.Connect_Output( _Altair ) ;
    end ;
end ;


const LED_Margin = 32 ;

procedure TMITS_Altair_UI._Initialize ;

var B : TSpeedButton ;
    L : TLabel ;
    Shape : TShape ;
    Y : integer ;

begin
    // Setup front-panel...
    while( Data_LEDs.Count > _Data_Bits ) do
    begin
        TControl( Data_LEDs[ Data_LEDs.Count - 1 ] ).Free ;
        Data_LEDs.Delete( Data_LEDs.Count - 1 ) ;
        TControl( Data_Labels[ Data_LEDs.Count - 1 ] ).Free ;
        Data_Labels.Delete( Data_LEDs.Count - 1 ) ;
    end ;
    while( Data_LEDs.Count < _Data_Bits ) do
    begin
        Shape := TShape.Create( Our_Screen.Data_Box ) ;
        Shape.Parent := Our_Screen.Data_Box ;
        Shape.Shape := stCircle ;
        Shape.Height := 15 ;
        Shape.Width := 15 ;
        Shape.Brush.Color := clMaroon ;
        Shape.Top := 20 ;
        Shape.Left := Our_Screen.Data_Box.ClientWidth - LED_Margin - ( Shape.Width + LED_Margin ) * ( Data_LEDs.Count + 1 ) ;
        L := TLabel.Create( Our_Screen.Data_Box ) ;
        L.Parent := Our_Screen.Data_Box ;
        L.Autosize := True ;
        L.Caption := 'D' + inttostr( Data_LEDs.Count ) ;
        L.Left := Shape.Left ;
        L.Top := Shape.Top + Shape.Height + 8 ;
        B := TSpeedButton.Create( Our_Screen ) ;
        B.Parent := Our_Screen ;
        B.Height := 15 ;
        B.Width := 15 ;
        B.Top := Our_Screen.Data_Box.Top + Our_Screen.Data_Box.Height + 8 ;
        B.Left := Shape.Left + Our_Screen.Data_Box.Left ;
        B.GroupIndex := Data_Switches.Count + 1 ;
        B.AllowAllUp := True ;
        Data_Switches.Add( B ) ;
        Data_LEDs.Add( Shape ) ;
        Data_Labels.Add( L ) ;
    end ;
    while( Address_LEDs.Count > _Address_Bits ) do
    begin
        TControl( Address_LEDs[ Address_LEDs.Count - 1 ] ).Free ;
        Address_LEDs.Delete( Address_LEDs.Count - 1 ) ;
        TControl( Address_Labels[ Address_LEDs.Count - 1 ] ).Free ;
        Address_Labels.Delete( Address_LEDs.Count - 1 ) ;
    end ;
    while( Address_LEDs.Count < _Address_Bits ) do
    begin
        Shape := TShape.Create( Our_Screen.Address_Box ) ;
        Shape.Parent := Our_Screen.Address_Box ;
        Shape.Shape := stCircle ;
        Shape.Height := 15 ;
        Shape.Width := 15 ;
        Shape.Brush.Color := clMaroon ;
        Shape.Top := 20 ;
        Shape.Left := Our_Screen.Address_Box.ClientWidth - LED_Margin -
            ( Shape.Width + LED_Margin ) * ( Address_LEDs.Count + 1 ) ;
        L := TLabel.Create( Our_Screen.Address_Box ) ;
        L.Parent := Our_Screen.Address_Box ;
        L.Autosize := True ;
        L.Caption := 'A' + inttostr( Address_LEDs.Count ) ;
        L.Left := Shape.Left ;
        L.Top := Shape.Top + Shape.Height + 8 ;
        Address_LEDs.Add( Shape ) ;
        Address_Labels.Add( L ) ;
    end ;
    Our_Screen.IMSAI_Panel.ClientWidth := 8 * ( 15 + LED_Margin ) ;
    Y := 20 - Our_Screen.IMSAI_Panel.Top ;
    if( Y < 0 ) then
    begin
        Y := 0 ;
    end ;
    while( IMSAI_LEDs.Count < 8 ) do
    begin
        Shape := TShape.Create( Our_Screen.IMSAI_Panel ) ;
        Shape.Parent := Our_Screen.IMSAI_Panel ;
        Shape.Shape := stCircle ;
        Shape.Height := 15 ;
        Shape.Width := 15 ;
        Shape.Brush.Color := clMaroon ;
        Shape.Top := Y ;
        Shape.Left := Our_Screen.IMSAI_Panel.ClientWidth - LED_Margin - ( Shape.Width + LED_Margin ) * ( IMSAI_LEDs.Count + 1 ) ;
        IMSAI_LEDs.Add( Shape ) ;
    end ;

    // Initialize ports...
    fillchar( _Altair.Port_Component_Handles, sizeof( _Altair.Port_Component_Handles ), 0 ) ;
    fillchar( _Altair.Port_Components, sizeof( _Altair.Port_Components ), 0 ) ;
    fillchar( _Altair.Port_Connections, sizeof( _Altair.Port_Connections ), 0 ) ;
end ; // TMITS_Altair._Initialize


procedure TMITS_Altair_UI.Update_Address( Value : int64 ) ;

var Loop : integer ;
    Shape : TShape ;

begin
    for Loop := 0 to Address_LEDs.Count - 1 do
    begin
        Shape := TShape( Address_LEDs[ Loop ] ) ;
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


procedure TMITS_Altair_UI.Update_Data( Value : int64 ) ;

var Loop : integer ;
    Shape : TShape ;

begin
    for Loop := 0 to Data_LEDs.Count - 1 do
    begin
        Shape := TShape( Data_LEDs[ Loop ] ) ;
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


function TMITS_Altair_UI.Get_Hidden : boolean ;

begin
    Result := not Our_Screen.Visible ;
end ;


procedure TMITS_Altair_UI.Set_Hidden( Value : boolean ) ;

begin
    Our_Screen.Visible := not Value ;
end ;


function TMITS_Altair_UI.Get_Parent_Window : THandle ;

begin
    Result := Our_Screen.ParentWindow ;
end ;


procedure TMITS_Altair_UI.Set_Parent_Window( Value : THandle ) ;

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


function TMITS_Altair_UI.Get_Caption : PChar ;

begin
    Result := PChar( Caption ) ;
end ;


procedure TMITS_Altair_UI.Set_Caption( Value : PChar ) ;

begin
    Caption := string( Value ) ;
end ;


procedure TMITS_Altair_UI.Set_Size( Height, Width : integer ) ;

begin
    Our_Screen.Height := Height + GetSystemMetrics( SM_CXBORDER ) + GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Width := Width + GetSystemMetrics( SM_CXBORDER ) * 2 ;
    Our_Screen.Top := -GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Left := -GetSystemMetrics( SM_CXBORDER ) ;
end ;



// TMITS_Altair methods...

function TMITS_Altair.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TMITS_Altair.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


function TMITS_Altair.Port_Index( Index : integer ) : integer ;

var Loop : integer ;

begin
    for Loop := 0 to 255 do
    begin
        if( Port_Components[ True, Loop ] <> nil ) then
        begin
            dec( Index ) ;
            if( Index < 0 ) then
            begin
                Result := Loop ;
                exit ;
            end ;
        end ;
    end ;
    Result := -1 ;
end ;


// API...

function TMITS_Altair.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TMITS_Altair.Facility_Code : longint ;

begin
    Facility_Code := MITSAltair_Facility ;
end ;


function TMITS_Altair.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    if( _User_Interface = nil ) then
    begin
        // General setup...
        _Altair := self ;
        Inputs := TList.Create ;
        Outputs := TList.Create ;
        _User_Interface := TMITS_Altair_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface._UI := UI ;
        _User_Interface.Initialize ;
        UI.Want_Signals( self, True ) ;
    end ;
    Initialize := _User_Interface.Set_Error( TMITSAltairErr_Success ) ;
end ; { TMITS_Altair.Initialize }


function TMITS_Altair.Terminate : TUnified_Exception ;

begin
    if( _User_Interface._UI <> nil ) then
    begin
        _User_Interface._UI.Termination_Notice( self ) ;
    end ;
    Terminate := _User_Interface.Set_Error( TMITSAltairErr_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    _Altair := nil ;
    Free ;
end ; { TMITS_Altair.Terminate }



function TMITS_Altair.Component_Type : longint ;

begin
    Component_Type := Component_Type_UI ;
end ;


function TMITS_Altair.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( TMITSAltairErr_Invalid_Component ) ;
        exit ;
    end ;
    if( Inputs.Indexof( Component ) <> -1 ) then
    begin
        Connect_Input := _User_Interface.Set_Error( TMITSAltairErr_Component_Already_Connected ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    _User_Interface.Add_States( Component ) ;
    Connect_Input := _User_Interface.Set_Error( TMITSAltairErr_Success ) ;
end ;


function TMITS_Altair.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( TMITSAltairErr_Invalid_Component ) ;
        exit ;
    end ;
    if( Outputs.Indexof( Component ) <> -1 ) then
    begin
        Connect_Output := _User_Interface.Set_Error( TMITSAltairErr_Component_Already_Connected ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    _User_Interface.Add_States( Component ) ;
    Connect_Output := _User_Interface.Set_Error( TMITSAltairErr_Success ) ;
end ;


function TMITS_Altair.Debugger : TDebug_Interface ;

begin
    Result := TMITS_Altair_Debugger.Create ;
    TMITS_Altair_Debugger( Result ).Panel := Self ;
end ;


function TMITS_Altair.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( TMITSAltairErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( TMITSAltairErr_Success ) ;
	Inputs.Remove( Component ) ;
        if( Inputs.Count = 0 ) then
        begin
            _User_Interface.Clear_States ;
        end ;
    end ;
end ;


function TMITS_Altair.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( TMITSAltairErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( TMITSAltairErr_Success ) ;
	Outputs.Remove( Component ) ;
        if( Outputs.Count = 0 ) then
        begin
            _User_Interface.Clear_States ;
        end ;
    end ;
end ;


function TMITS_Altair.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Inputs.Count ) ) then
    begin
        Result := Inputs[ Index ] ;
    end else
    begin
	Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'Altair 8800'#0 ;

function TMITS_Altair.Name : PChar ;

begin
    Name := _Name ;
end ;


function TMITS_Altair.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Outputs.Count ) ) then
    begin
        Result := Outputs[ Index ] ;
    end else
    begin
	Output_Component := nil ;
    end ;
end ;


function TMITS_Altair.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

var C : TComponent ;
    Data, Loop : integer ;
    Success : boolean ;

begin
    In_Read := True ;
    try
        Read := False ;
        case IO_Type of
          IO_Type_Memory:
              begin
                  _User_Interface.Our_Screen.IO_LED.Brush.Color := clMaroon ;
                  _User_Interface.Our_Screen.Memory_LED.Brush.Color := clRed ;
              end ;
          IO_Type_IO:
              begin
                  _User_Interface.Our_Screen.IO_LED.Brush.Color := clRed ;
                  _User_Interface.Our_Screen.Memory_LED.Brush.Color := clMaroon ;
                  if( Address = 255 ) then // Front panel switches
                  begin
                      Read := True ;
                      Data := 0 ;
                      for Loop := 7 downto 0 do
                      begin
                          Data := Data shl 1 ;
                          if( TSpeedButton( _User_Interface.Data_Switches[ Loop ] ).Down ) then
                          begin
                              Data := Data or 1 ;
                          end ;
                      end ;
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
                      // Pass the request on down the bus...
                      for Loop := 0 to Inputs.Count - 1 do
                      begin
                          C := Inputs[ Loop ] ;
                          Success := C.Read( Address, Size, IO_Type ) ;
                          if( Success ) then
                          begin
                              Result := True ;
                          end ;
                      end ;
                      for Loop := 0 to Outputs.Count - 1 do
                      begin
                          C := Outputs[ Loop ] ;
                          if( Inputs.IndexOf( C ) = -1 ) then
                          begin
                              Success := C.Read( Address, Size, IO_Type ) ;
                              if( Success ) then
                              begin
                                  Result := True ;
                              end ;
                          end ;
                      end ;
                  end ; // if( Address = 255 )
              end ;
          else
              begin
                  _User_Interface.Our_Screen.IO_LED.Brush.Color := clMaroon ;
                  _User_Interface.Our_Screen.Memory_LED.Brush.Color := clMaroon ;
              end ;
        end ;
        _User_Interface.Our_Screen.Read_LED.Brush.Color := clRed ;
        _User_Interface.Our_Screen.Write_LED.Brush.Color := clMaroon ;
        _User_Interface.Update_Address( Address ) ;
    finally
        In_Read := False ;
    end ;
end ; { TMITS_Altair.Read }


function TMITS_Altair.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var C : TComponent ;
    Loop : integer ;

begin
    Write := _User_Interface.Set_Error( TMITSAltairErr_Address_Out_Of_Range ) ;
    if( not In_Read ) then // This is not a response to the previous Read operation
    begin
        case IO_Type of
          IO_Type_Memory:
              begin
                  _User_Interface.Our_Screen.IO_LED.Brush.Color := clMaroon ;
                  _User_Interface.Our_Screen.Memory_LED.Brush.Color := clRed ;
              end ;
          IO_Type_IO:
              begin
                  _User_Interface.Our_Screen.IO_LED.Brush.Color := clRed ;
                  _User_Interface. Our_Screen.Memory_LED.Brush.Color := clMaroon ;
                  if( ( Address = 255 ) and ( _User_Interface.Our_Screen.IMSAI1.Checked ) ) then
                  begin
                      Write := _User_Interface.Set_Error( TMITSAltairErr_Success ) ;
                      for Loop := 0 to _User_Interface.IMSAI_LEDs.Count - 1 do
                      begin
                          if( ( Value and 1 ) = 0 ) then
                          begin
                              TShape( _User_Interface.IMSAI_LEDS[ Loop ] ).Brush.Color := clMaroon ;
                          end else
                          begin
                              TShape( _User_Interface.IMSAI_LEDS[ Loop ] ).Brush.Color := clRed ;
                          end ;
                          Value := Value shr 1 ;
                      end ;
                  end ; // if( ( Address = 255 ) and ( _User_Interface.Our_Screen.IMSAI1.Checked ) )
              end ; // IO_Type_IO
          else
              begin
                  _User_Interface.Our_Screen.IO_LED.Brush.Color := clMaroon ;
                  _User_Interface.Our_Screen.Memory_LED.Brush.Color := clMaroon ;
              end ;
        end ; // case IO_Type
        _User_Interface.Our_Screen.Read_LED.Brush.Color := clMaroon ;
        _User_Interface.Our_Screen.Write_LED.Brush.Color := clRed ;
        _User_Interface.Update_Address( Address ) ;
    end ; // if( not In_Read )
    
    // Pass the data on down the bus...
    for Loop := 0 to Inputs.Count - 1 do
    begin
        C := Inputs[ Loop ] ;
        Result := C.Write( Address, Value, Size, IO_Type ) ;
    end ;
    for Loop := 0 to Outputs.Count - 1 do
    begin
        C := Outputs[ Loop ] ;
        if( Inputs.IndexOf( C ) = -1 ) then
        begin
            Result := C.Write( Address, Value, Size, IO_Type ) ;
        end ;
    end ;

    _User_Interface.Update_Data( Value ) ;
end ; // TMITS_Altair.Write


procedure TMITS_Altair.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TMITS_Altair.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TMITS_Altair.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TMITS_Altair.Set_Parent( Component : TComponent ) ;

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


procedure TMITS_Altair.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S, S1 : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'DATA' ) then
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
            _User_Interface._Data_Bits := Convert_Value( S ) ;
        end else
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
            _User_Interface._Address_Bits := Convert_Value( S ) ;
        end else
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
            _User_Interface.Caption := S ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' ) 
    Parser.Free ;
    _User_Interface._Initialize ;
end ; // TMITS_Altair.Set_Up


procedure TMITS_Altair.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

var I : integer ;
    S : string ;

begin
    S := string( Component.Get_State_Name( Index ) ) ;
    I := _User_Interface.State_Index( S ) ;
    if( I <> -1 ) then
    begin
        if( Component.Signal_Active_Low( Index ) ) then
        begin
            Active := not Active ;
        end ;
        if( Active ) then
        begin
            TShape( _User_Interface.State_LEDs[ I ] ).Brush.Color := clRed ;
        end else
        begin
            TShape( _User_Interface.State_LEDs[ I ] ).Brush.Color := clMaroon ;
        end ;
    end ;
end ;


procedure TMITS_Altair.Child_Notification( Child : TComponent ;
    var Notice : longint ; var Params : int64 ) ;

var Loop : integer ;

begin
    if( Child = nil ) then
    begin
        exit ;
    end ;
    if( Notice = Child_Notice_Terminating ) then
    begin
        _Altair.Disconnect_Input( Child ) ;
        _Altair.Disconnect_Output( Child ) ;
        for Loop := 0 to 255 do
        begin
            if( Child = _Altair.Port_Connections[ True, Loop ] ) then
            begin
                try
                    _Altair.Port_Components[ True, Loop ].Disconnect_Output( Child ) ;
                    _Altair.Port_Components[ True, Loop ].Disconnect_Input( Child ) ;
                except
                end ;
                _Altair.Port_Connections[ True, Loop ] := nil ;
                exit ;
            end ;
        end ;
    end else
    if( Notice = Child_Notice_Connect ) then
    begin
        for Loop := 0 to 255 do
        begin
            if( Child = Port_Components[ True, Loop ] ) then
            begin
                if( TComponent( pointer( Params ) ) <> _Altair ) then
                begin
                    Port_Connections[ True, Loop ] := TComponent( pointer( Params ) ) ;
                end ;
                _Altair.Connect_Input( Child ) ;
                _Altair.Connect_Output( Child ) ;
                exit ;
            end ;
        end ;
    end else
    if( Notice = Child_Notice_Disconnect ) then
    begin
        _Altair.Disconnect_Input( Child ) ;
        _Altair.Disconnect_Output( Child ) ;
        for Loop := 0 to 255 do
        begin
            if( Child = Port_Components[ True, Loop ] ) then
            begin
                Port_Connections[ True, Loop ] := nil ;
                exit ;
            end ;
        end ;
    end ;
end ; // TMITS_Altair.Child_Notification


function TMITS_Altair.Get_Port_Name( Index : longint ) : PChar ;

begin
    Index := Port_Index( Index ) ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        Temp_Port_Name := 'Port ' + inttostr( Index div 2 ) ;
        Result := PChar( Temp_Port_Name ) ;
    end ;
end ;


function TMITS_Altair.Get_Port_Description( Index : longint ) : PChar ;

begin
    Index := Port_Index( Index ) ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        Temp_Port_Description := Port_Components[ True, Index ].Name ;
        Result := PChar( Temp_Port_Description ) ;
    end ;
end ;


function TMITS_Altair.Get_Port( Index : longint ) : TComponent ;

begin
    Index := Port_Index( Index ) ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        Result := Port_Components[ True, Index ] ;
    end ;
end ;


function TMITS_Altair.Get_Port_Connection( Index : longint ) : TComponent ;

begin
    Index := Port_Index( Index ) ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        Result := Port_Connections[ True, Index ] ;
    end ;
end ;


procedure TMITS_Altair.UI_Notice( Code : longint ; var Data : int64 ) ;

begin
    if( Code = UI_Notice_Changed_Run_State ) then
    begin
        _User_Interface.Our_Screen.Run_Button.Down := ( Data = 1 ) ;
    end ;
end ;



initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

