{
        Program Name : CEF_Panel
        Package Name : Panel
        Purpose      : CEF Front Panel component
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

        Copyright (C) 2005 by Alan Conroy.  Released to the public domain.

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

          This unit manages a generic front-panel component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CEF_Panel ;

interface

uses // Borland...
     Windows, // TRect

     Classes, // TShiftState
     Controls, // TMouseButton
     Forms, // TForm
     Graphics, // TBitmap

     // CEF...
     _CEF, // TUI_Interface
     CEF, // TBase_User_Interface
     Collect, // TCollection
     _DebugIn, // TDebug_Interface
     _Streams, // TCOM_Stream
     Panel_Form, // TFront_Panel_Form
     _UE ; // TUnified_Exception

const CEFPanel_Facility = -1 ;
const TCEFPanelErr_Success = 0 ;
const TCEFPanelErr_Invalid_Range = 1 ;
const TCEFPanelErr_Component_Not_Found = 2 ;
const TCEFPanelErr_Memory_Exhausted = 3 ;
const TCEFPanelErr_Access_Violation = 4 ;
const TCEFPanelErr_Address_Out_Of_Range = 5 ;
const TCEFPanelErr_Invalid_Component = 6 ;

type TCEF_Panel_UI = class ;

     TCEF_Panel = class( TBase_Component )
                      private // Instance data...
                          _User_Interface : TCEF_Panel_UI ;
                          Inputs, Outputs : TList ;
                          _Tag : longint ;
                          _Parent : TComponent ;
                          In_Read : boolean ; // True if waiting for response to read request

                      private // Internal utility routines...
                          function Default_Input : TComponent ;
                          function Default_Output : TComponent ;

                      public { Public instance data... }
                          _Serial_Number : integer ;

                      public // API...
                          function Facility_Code : longint ; override ;

                          function Initialize( UI : TUI_Interface ) : TUnified_Exception ; override ;

                          function Terminate : TUnified_Exception ; override ;

                          function Component_Type : longint ; override ;

                          function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

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

                          procedure Signal_Change_Notice( Component : TComponent ;
                              Index : longint ; Active : boolean ) ; override ;

                          function User_Interface : TUser_Interface ; override ;
                  end ; // TCEF_Panel

     TCEF_Panel_UI = class( TBase_User_Interface )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private { Instance data... }
            _UI : TUI_Interface ;
            _Our_Screen : TFront_Panel_Form ;
            Caption : string ;
            Top, Left, Height, Width : integer ; // Saved metrics
            _Data_Bits, _Address_Bits : integer ;
            Data_LEDS, Data_Labels : TList ;
            Address_LEDs, Address_Labels : TList ;
            State_LEDs, State_Labels : TList ;
            Parent : TCEF_Panel ;

        protected
            function Translate_Error( Code : longint ) : string ; override ;

        private // Internal utility routines...
            procedure Add_States( Component : TComponent ) ;
            procedure CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
                Shift : TShiftState ; X, Y : Integer ) ;
            procedure CB_Toggle_Embed( Sender : TObject ) ;
            function State_Index( S : string ) : integer ;
            procedure Clear_States ;
            procedure _Initialize ;
            procedure Update_Address( Value : int64 ) ;
            procedure Update_Data( Value : int64 ) ;

        protected // Property handlers...
            function Get_Our_Screen : TFront_Panel_Form ;

        public { API... }
            function Facility_Code : longint ; override ;
            function Get_Hidden : boolean ; override ;
            procedure Set_Hidden( Value : boolean ) ; override ;

            function Get_Parent_Window : THandle ; override ;
            procedure Set_Parent_Window( Value : THandle ) ; override ;

            function Get_Caption : PChar ; override ;
            procedure Set_Caption( Value : PChar ) ; override ;

            procedure Set_Size( Height, Width : integer ) ; override ;

        public // Properties...
            property Our_Screen : TFront_Panel_Form
                read Get_Our_Screen
                write _Our_Screen ;
     end ; // TCEF_Panel_UI

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
     UE, // Create_Simple_UE
     Num1s, // Num1
     Parse ; // TString_Parser

     
function TCEF_Panel_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        TCEFPanelErr_Success: _Error := 'Success' ;
        TCEFPanelErr_Invalid_Range: _Error := 'Invalid range' ;
        TCEFPanelErr_Component_Not_Found: _Error := 'Component not found' ;
        TCEFPanelErr_Access_Violation: _Error := 'Access violation' ;
        TCEFPanelErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        TCEFPanelErr_Invalid_Component: _Error := 'Invalid component' ;
        TCEFPanelErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
        else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { tFS_EI.Translate_Error }


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

type TCEF_Panel_Debugger = class( TText_Debugger )
                                private
                                    _Panel : TCEF_Panel ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Panel : TCEF_Panel
                                                 read _Panel
                                                 write _Panel ;
                            end ;

function TCEF_Panel_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TCEF_Panel_Debugger.Count : longint ;

begin
    Result := 4 ;
end ;


// TCEF_Panel_UI methods...

// Constructors and destructors...

constructor TCEF_Panel_UI.Create ;

begin
    inherited Create ;

    Caption := 'Front Panel' ;
    _Data_Bits := 8 ;
    _Address_Bits := 16 ;
    Data_LEDs := TList.Create ;
    Address_LEDs := TList.Create ;
    Data_Labels := TList.Create ;
    Address_Labels := TList.Create ;
    State_LEDS := TList.Create ;
    State_Labels := TList.Create ;

    _Initialize ;
end ;


destructor TCEF_Panel_UI.Destroy ;

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
    Our_Screen.Close ;
    Our_Screen := nil ;

    inherited Destroy ;
end ;


// Property handlers...

function TCEF_Panel_UI.Get_Our_Screen : TFront_Panel_Form ;

begin
    if( _Our_Screen = nil ) then
    begin
        _Our_Screen := TFront_Panel_Form.Create( Application ) ;
        _Our_Screen.OnMouseDown := CB_MouseDown ;
        _Our_Screen.Control_Box.OnMouseDown := CB_MouseDown ;
        _Our_Screen.Data_Box.OnMouseDown := CB_MouseDown ;
        _Our_Screen.Address_Box.OnMouseDown := CB_MouseDown ;
        _Our_Screen.Caption := 'Front Panel' ;
        _Our_Screen.BorderIcons := [] ;
        _Our_Screen.Visible := True ;
    end ;
    Result := _Our_Screen ;
end ;


// Internal utility routines...

procedure TCEF_Panel_UI.Add_States( Component : TComponent ) ;

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
end ; // TCEF_Panel.Add_States


var Pop_Up : TPopupMenu = nil ;

procedure TCEF_Panel_UI.CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
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


procedure TCEF_Panel_UI.CB_Toggle_Embed( Sender : TObject ) ;

begin
    _UI.Toggle_Embed( Parent ) ;
end ;


function TCEF_Panel_UI.State_Index( S : string ) : integer ;

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


procedure TCEF_Panel_UI.Clear_States ;

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


function TCEF_Panel.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TCEF_Panel.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


const LED_Margin = 16 ;

procedure TCEF_Panel_UI._Initialize ;

var L : TLabel ;
    Shape : TShape ;

begin
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
        Shape.Left := Our_Screen.Address_Box.ClientWidth - LED_Margin - ( Shape.Width + LED_Margin ) * ( Address_LEDs.Count + 1 ) ;
        L := TLabel.Create( Our_Screen.Address_Box ) ;
        L.Parent := Our_Screen.Address_Box ;
        L.Autosize := True ;
        L.Caption := 'A' + inttostr( Address_LEDs.Count ) ;
        L.Left := Shape.Left ;
        L.Top := Shape.Top + Shape.Height + 8 ;
        Address_LEDs.Add( Shape ) ;
        Address_Labels.Add( L ) ;
    end ;
end ; // TCEF_Panel._Initialize


procedure TCEF_Panel_UI.Update_Address( Value : int64 ) ;

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


procedure TCEF_Panel_UI.Update_Data( Value : int64 ) ;

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


{ API... }

function TCEF_Panel.Facility_Code : longint ;

begin
    Facility_Code := CEFPanel_Facility ;
end ;


function TCEF_Panel.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    _User_Interface := TCEF_Panel_UI.Create ;
    _User_Interface._UI := UI ;
    _User_Interface.Parent := self ;
    UI.Want_Signals( self, True ) ;

    Initialize := _User_Interface.Set_Error( TCEFPanelErr_Success ) ;
end ; { TCEF_Panel.Initialize }


function TCEF_Panel.Terminate : TUnified_Exception ;

begin
    Terminate := _User_Interface.Set_Error( TCEFPanelErr_Success ) ;
    if( ( _User_Interface <> nil ) and ( _User_Interface._UI <> nil ) ) then
    begin
        _User_Interface._UI.Termination_Notice( self ) ;
    end ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    Free ;
end ; { TCEF_Panel.Terminate }



function TCEF_Panel.Component_Type : longint ;

begin
    Component_Type := Component_Type_UI ;
end ;


function TCEF_Panel.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( TCEFPanelErr_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    _User_Interface.Add_States( Component ) ;
    Connect_Input := _User_Interface.Set_Error( TCEFPanelErr_Success ) ;
end ;


function TCEF_Panel.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( TCEFPanelErr_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    _User_Interface.Add_States( Component ) ;
    Connect_Output := _User_Interface.Set_Error( TCEFPanelErr_Success ) ;
end ;


function TCEF_Panel.Debugger : TDebug_Interface ;

begin
    Result := TCEF_Panel_Debugger.Create ;
    TCEF_Panel_Debugger( Result ).Panel := Self ;
end ;


function TCEF_Panel.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( TCEFPanelErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( TCEFPanelErr_Success ) ;
	Inputs.Remove( Component ) ;
        if( Inputs.Count = 0 ) then
        begin
            _User_Interface.Clear_States ;
        end ;
    end ;
end ;


function TCEF_Panel.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( TCEFPanelErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( TCEFPanelErr_Success ) ;
	Outputs.Remove( Component ) ;
        if( Outputs.Count = 0 ) then
        begin
            _User_Interface.Clear_States ;
        end ;
    end ;
end ;


function TCEF_Panel.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Inputs.Count ) ) then
    begin
        Result := Inputs[ Index ] ;
    end else
    begin
	Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'CEF Generic Front Panel'#0 ;

function TCEF_Panel.Name : PChar ;

begin
    Name := _Name ;
end ;


function TCEF_Panel.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Outputs.Count ) ) then
    begin
        Result := Outputs[ Index ] ;
    end else
    begin
	Output_Component := nil ;
    end ;
end ;


function TCEF_Panel.Read( Address : int64 ; Size : longint ;
            IO_Type : longint ) : boolean ;

begin
    In_Read := True ;
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
          end ;
      else
          begin
              _User_Interface.Our_Screen.IO_LED.Brush.Color := clMaroon ;
              _User_Interface.Our_Screen.Memory_LED.Brush.Color := clMaroon ;
          end ;
    end ;
    _User_Interface.Our_Screen.Read_LED.Brush.COlor := clRed ;
    _User_Interface.Our_Screen.Write_LED.Brush.COlor := clMaroon ;
    _User_Interface.Update_Address( Address ) ;
    Read := False ;
end ; { TCEF_Panel.Read }


function TCEF_Panel.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    if( In_Read ) then // This is a response to the previous Read operation
    begin
        In_Read := false ;
    end else
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
                  _User_Interface.Our_Screen.Memory_LED.Brush.Color := clMaroon ;
              end ;
          else
              begin
                  _User_Interface.Our_Screen.IO_LED.Brush.Color := clMaroon ;
                  _User_Interface.Our_Screen.Memory_LED.Brush.Color := clMaroon ;
              end ;
        end ;
        _User_Interface.Our_Screen.Read_LED.Brush.Color := clMaroon ;
        _User_Interface.Our_Screen.Write_LED.Brush.Color := clRed ;
        _User_Interface.Update_Address( Address ) ;
    end ;
    _User_Interface.Update_Data( Value ) ;
    Write := _User_Interface.Set_Error( TCEFPanelErr_Address_Out_Of_Range ) ;
end ; // TCEF_Panel.Write


procedure TCEF_Panel.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TCEF_Panel.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TCEF_Panel.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TCEF_Panel.Set_Parent( Component : TComponent ) ;

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


procedure TCEF_Panel.Set_Up( P : PChar ) ;

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
end ; // TCEF_Panel.Set_Up


function TCEF_Panel.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


procedure TCEF_Panel.Signal_Change_Notice( Component : TComponent ;
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


function TCEF_Panel_UI.Facility_Code : longint ;

begin
    Result := CEFPanel_Facility ;
end ;


function TCEF_Panel_UI.Get_Hidden : boolean ;

begin
    Result := not Our_Screen.Visible ;
end ;


procedure TCEF_Panel_UI.Set_Hidden( Value : boolean ) ;

begin
    Our_Screen.Visible := not Value ;
end ;


function TCEF_Panel_UI.Get_Parent_Window : THandle ;

begin
    Result := Our_Screen.ParentWindow ;
end ;


procedure TCEF_Panel_UI.Set_Parent_Window( Value : THandle ) ;

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


function TCEF_Panel_UI.Get_Caption : PChar ;

begin
    Result := PChar( Caption ) ;
end ;


procedure TCEF_Panel_UI.Set_Caption( Value : PChar ) ;

begin
    Caption := string( Value ) ;
end ;


procedure TCEF_Panel_UI.Set_Size( Height, Width : integer ) ;

begin
    Our_Screen.Height := Height + GetSystemMetrics( SM_CXBORDER ) + GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Width := Width + GetSystemMetrics( SM_CXBORDER ) * 2 ;
    Our_Screen.Top := -GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Left := -GetSystemMetrics( SM_CXBORDER ) ;
end ;



const Screen_Facility_Name : PChar = 'CEF Screen' ;

initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

