{
        Program Name : Elf
        Package Name : CEF
        Purpose      : Cosmac Elf CEF component
        Institution  : Conroy & Conroy Co.
        Date Written : 10-Mar-2007
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

          This component handles the "system" hardware for the Cosmac Elf for
        CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Elf ;

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
     Elf_Form, // TFront_Panel_Form
     _UE ; // TUnified_Exception

const CosmacElf_Facility = -1 ;
const TCosmacElfErr_Success = 0 ;
const TCosmacElfErr_Invalid_Range = 1 ;
const TCosmacElfErr_Component_Not_Found = 2 ;
const TCosmacElfErr_Memory_Exhausted = 3 ;
const TCosmacElfErr_Access_Violation = 4 ;
const TCosmacElfErr_Address_Out_Of_Range = 5 ;
const TCosmacElfErr_Invalid_Component = 6 ;

type TCosmac_Elf_UI = class ;

     TCosmac_Elf = class( TBase_Component )
                        private { Instance data... }
                            Access_Mode : integer ;
                            Inputs, Outputs : TList ;
                            _Tag : longint ;
                            _Parent : TComponent ;
                            In_Read : boolean ; // True if waiting for response to read request
                            Temp_Port_Name : string ;
                            Temp_Port_Description : string ;
                            Temp_Signal_Name : string ;
                            EF4, MP : boolean ;
                            _Memory : TComponent ;
                            _CPU : TComponent ;

                        private // Internal utility routines...
                            function Default_Input : TComponent ;
                            function Default_Output : TComponent ;
                            function Port_Index( Index : integer ) : integer ;

                        public { Public instance data... }
                            _Serial_Number : integer ;
                            _User_Interface : TCosmac_Elf_UI ;
                            Port_Component_Handles : array[ boolean, 0..255 ] of THandle ; // Library module handles for Port_Components
                            Port_Components : array[ boolean, 0..255 ] of TComponent ; // Components for ports
                            Port_Connections : array[ boolean, 0..255 ] of TComponent ; // Connections to ports

                        public // API...
                            procedure Set_EF4( State : boolean ) ;

                            procedure Memory_Protect( State : boolean ) ;

                            function DepositX( Component : TComponent ;
                                Address : int64 ; Size : longint ;
                                Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

                            function ExamineX( Component : TComponent ;
                                Address : int64 ; var Size : longint ;
                                Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

                            function ReadX( Component : TComponent ;
                                Address : int64 ; Size : longint ;
                                IO_Type : longint ) : boolean ;

                            function WriteX( Component : TComponent ; Address : int64 ;
                                Value, Size : longint ; IO_Type : longint ) : TUnified_Exception ;

                        public // Overrides...
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

                            function Get_Signal( Name : PChar ;
                                var State : boolean ) : boolean ; override ;
                            function Signal_Count : longint ; override ;
                            function Signal_Name( Index : longint ) : PChar ;
                                override ;
                            function Signal_Out( Index : longint ) : boolean ;
                                override ;
                            function Signal_Active_Low( Index : longint ) : boolean ;
                                override ;
                            function Signal_Index( Name : PChar ) : integer ;
                                override ;
                            procedure UI_Notice( Code : longint ;
                                var Data : int64 ) ; override ;
                    end ; // TCosmac_Elf

     TCosmac_Elf_UI = class( TBase_User_Interface )
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
            Parent : TCosmac_Elf ;

        public // Public instance data...
            _UI : TUI_Interface ;

        private // Internal utility routines...
            procedure CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
                Shift : TShiftState ; X, Y : Integer ) ;
            procedure CB_Toggle_Embed( Sender : TObject ) ;
            procedure _Initialize ;

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
     end ; // TCosmac_Elf

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

type TCosmac_Elf_Debugger = class( TText_Debugger )
                                private
                                    _Panel : TCosmac_Elf ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Panel : TCosmac_Elf
                                                 read _Panel
                                                 write _Panel ;
                            end ;

function TCosmac_Elf_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TCosmac_Elf_Debugger.Count : longint ;

begin
    Result := 5 ;
end ;



type TMemory_Interface = class( TBase_Component )
                             public
                                 Elf : TCosmac_Elf ;
                                 _Component : TComponent ;

                                 function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                                     override ; stdcall ;

                                 function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                                     override ; stdcall ;

                                 function Deposit( Address : int64 ;
                                     Size : longint ; Buffer : pointer ;
                                     Memory : boolean ) : TUnified_Exception ;
                                     override ; stdcall ;
                                 function Examine( Address : int64 ;
                                     var Size : longint ; Buffer : pointer ;
                                     Memory : boolean ) : TUnified_Exception ;
                                     override ; stdcall ;

                                 function Read( Address : int64 ;
                                     Size : longint ;
                                     IO_Type : longint ) : boolean ;
                                     override ; stdcall ;
                                 function Write( Address : int64 ;
                                     Value, Size : longint ;
                                     IO_Type : longint ) : TUnified_Exception ;
                                     override ; stdcall ;
                         end ;

function TMemory_Interface.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _Component := Component ;
    _Component.Connect_Output( self ) ;
end ;


function TMemory_Interface.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _Component := Component ;
    _Component.Connect_Input( self ) ;
end ;


function TMemory_Interface.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    Result := Elf.DepositX( self, Address, Size, Buffer, Memory ) ;
end ;


function TMemory_Interface.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    Result := Elf.ExamineX( self, Address, Size, Buffer, Memory ) ;
end ;


function TMemory_Interface.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := Elf.ReadX( self, Address, Size, IO_Type ) ;
end ;


function TMemory_Interface.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    Result := Elf.WriteX( self, Address, Value, Size, IO_Type ) ;
end ;



// TCosmac_Elf_UI methods...

// Constructors and destructors...

constructor TCosmac_Elf_UI.Create ;

begin
    inherited Create ;

    Our_Screen := TFront_Panel_Form.Create( Application ) ;
    Our_Screen.OnMouseDown := CB_MouseDown ;
    Our_Screen.Caption := 'Cosmac Elf' ;
    Caption := 'Cosmac Elf' ;
    Our_Screen.BorderIcons := [] ;
    Our_Screen.Visible := True ;
    _Data_Bits := 8 ;
    _Address_Bits := 16 ;
    Data_LEDs := TList.Create ;
    Address_LEDs := TList.Create ;
    Data_Labels := TList.Create ;
    Address_Labels := TList.Create ;
    Data_Switches := TList.Create ;
    _Initialize ;
end ;


destructor TCosmac_Elf_UI.Destroy ;

begin
    Data_LEDS.Free ;
    Address_LEDS.Free ;
    Data_LEDS := nil ;
    Address_LEDS := nil ;
    Data_Labels.Free ;
    Address_Labels.Free ;
    Data_Labels := nil ;
    Address_Labels := nil ;
    Data_Switches.Free ;
    Data_Switches := nil ;
    Our_Screen.Close ;
    Our_Screen := nil ;

    inherited Destroy ;
end ; // TCosmac_Elf_UI.Destroy


function TCosmac_Elf_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        TCosmacElfErr_Success: _Error := 'Success' ;
        TCosmacElfErr_Invalid_Range: _Error := 'Invalid range' ;
        TCosmacElfErr_Component_Not_Found: _Error := 'Component not found' ;
        TCosmacElfErr_Access_Violation: _Error := 'Access violation' ;
        TCosmacElfErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        TCosmacElfErr_Invalid_Component: _Error := 'Invalid component' ;
        TCosmacElfErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { TCosmac_Elf_UI.Translate_Error }


var Pop_Up : TPopupMenu = nil ;

procedure TCosmac_Elf_UI.CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
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


procedure TCosmac_Elf_UI.CB_Toggle_Embed( Sender : TObject ) ;

begin
    _UI.Toggle_Embed( Parent ) ;
end ;


const LED_Margin = 32 ;

procedure TCosmac_Elf_UI._Initialize ;

begin
end ; // TCosmac_Elf._Initialize


function TCosmac_Elf_UI.Get_Hidden : boolean ;

begin
    Result := not Our_Screen.Visible ;
end ;


procedure TCosmac_Elf_UI.Set_Hidden( Value : boolean ) ;

begin
    Our_Screen.Visible := not Value ;
end ;


function TCosmac_Elf_UI.Get_Parent_Window : THandle ;

begin
    Result := Our_Screen.ParentWindow ;
end ;


procedure TCosmac_Elf_UI.Set_Parent_Window( Value : THandle ) ;

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


function TCosmac_Elf_UI.Get_Caption : PChar ;

begin
    Result := PChar( Caption ) ;
end ;


procedure TCosmac_Elf_UI.Set_Caption( Value : PChar ) ;

begin
    Caption := string( Value ) ;
end ;


procedure TCosmac_Elf_UI.Set_Size( Height, Width : integer ) ;

begin
    Our_Screen.Height := Height + GetSystemMetrics( SM_CXBORDER ) + GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Width := Width + GetSystemMetrics( SM_CXBORDER ) * 2 ;
    Our_Screen.Top := -GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Left := -GetSystemMetrics( SM_CXBORDER ) ;
end ;



// TCosmac_Elf methods...

function TCosmac_Elf.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TCosmac_Elf.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


function TCosmac_Elf.Port_Index( Index : integer ) : integer ;

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

procedure TCosmac_Elf.Set_EF4( State : boolean ) ;

var C : TComponent ;
    Index : integer ;

begin
    EF4 := State ;
    Index := 0 ;
    C := Output_Component( Index ) ;
    while( C <> nil ) do
    begin
        C.Set_Signal( 'EF4', State ) ;
        inc( Index ) ;
        C := Output_Component( Index ) ;
    end ;
    if( _Memory <> nil ) then
    begin
        TMemory_Interface( _Memory )._Component.Set_Signal( 'EF4', State ) ;
    end ;
    if( _CPU <> nil ) then
    begin
        TMemory_Interface( _CPU )._Component.Set_Signal( 'EF4', State ) ;
    end ;
    _User_Interface._UI .Signal_Change_Notice( self, 0, State ) ;
end ;


procedure TCosmac_Elf.Memory_Protect( State : boolean ) ;

begin
    MP := State ;
end ;


function TCosmac_Elf.DepositX( Component : TComponent ; Address : int64 ;
    Size : longint ; Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    Result := Deposit( Address, Size, Buffer, Memory ) ; // Default
    if( ( _Memory <> nil ) and ( Component <> _Memory ) ) then
    begin
        Result := TMemory_Interface( _Memory )._Component.Deposit( Address, Size, Buffer, Memory ) ;
    end ;
    if( ( _CPU <> nil ) and ( Component <> _CPU ) ) then
    begin
        Result := TMemory_Interface( _CPU )._Component.Deposit( Address, Size, Buffer, Memory ) ;
    end ;
end ;


function TCosmac_Elf.ExamineX( Component : TComponent ; Address : int64 ;
    var Size : longint ; Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    Result := Examine( Address, Size, Buffer, Memory ) ; // Default
    if( ( _Memory <> nil ) and ( Component <> _Memory ) ) then
    begin
        Result := TMemory_Interface( _Memory )._Component.Examine( Address, Size, Buffer, Memory ) ;
    end ;
    if( ( _CPU <> nil ) and ( Component <> _CPU ) ) then
    begin
        Result := TMemory_Interface( _CPU )._Component.Examine( Address, Size, Buffer, Memory ) ;
    end ;
end ;


function TCosmac_Elf.ReadX( Component : TComponent ; Address : int64 ;
    Size : longint ; IO_Type : longint ) : boolean ;

begin
    Result := Read( Address, Size, IO_Type ) ; // Default
    if( ( _Memory <> nil ) and ( Component <> _Memory ) ) then
    begin
        Result := TMemory_Interface( _Memory )._Component.Read( Address, Size, IO_Type ) ;
    end ;
    if( ( _CPU <> nil ) and ( Component <> _CPU ) ) then
    begin
        Result := TMemory_Interface( _CPU )._Component.Read( Address, Size, IO_Type ) ;
    end ;
end ;


function TCosmac_Elf.WriteX( Component : TComponent ; Address : int64 ;
    Value, Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    Result := Write( Address, Value, Size, IO_Type ) ; // Default
    if( ( _CPU <> nil ) and ( Component <> _CPU ) ) then
    begin
        Result := TMemory_Interface( _CPU )._Component.Write( Address, Value, Size, IO_Type ) ;
    end ;
    if( ( _Memory <> nil ) and ( Component <> _Memory ) and ( not MP ) ) then
    begin
        Result := TMemory_Interface( _Memory )._Component.Write( Address, Value, Size, IO_Type ) ;
    end ;
end ;


// Overrides...

function TCosmac_Elf.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TCosmac_Elf.Facility_Code : longint ;

begin
    Facility_Code := CosmacElf_Facility ;
end ;


function TCosmac_Elf.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    if( _User_Interface = nil ) then
    begin
        // General setup...
        Inputs := TList.Create ;
        Outputs := TList.Create ;
        _User_Interface := TCosmac_Elf_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface._UI := UI ;
        _User_Interface.Our_Screen._UI := UI ;
        _User_Interface.Our_Screen.CPU := self ;
        UI.Want_Signals( self, True ) ;
        _Memory := TMemory_Interface.Create ;
        TMemory_Interface( _Memory ).Elf := self ;
        _CPU := TMemory_Interface.Create ;
        TMemory_Interface( _CPU ).Elf := self ;
    end ;
    Initialize := _User_Interface.Set_Error( TCosmacElfErr_Success ) ;
end ; { TCosmac_Elf.Initialize }


function TCosmac_Elf.Terminate : TUnified_Exception ;

begin
    if( _User_Interface._UI <> nil ) then
    begin
        _User_Interface._UI.Termination_Notice( self ) ;
    end ;
    Terminate := _User_Interface.Set_Error( TCosmacElfErr_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    Free ;
end ; { TCosmac_Elf.Terminate }



function TCosmac_Elf.Component_Type : longint ;

begin
    Component_Type := Component_Type_UI ;
end ;


function TCosmac_Elf.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( TCosmacElfErr_Invalid_Component ) ;
        exit ;
    end ;
    if( Component.CPU <> nil ) then
    begin
        _CPU.Connect_Input( Component ) ;
        Connect_Input := _User_Interface.Set_Error( 0 ) ;
        exit ;
    end ;
    if( Component.Memory <> nil ) then
    begin
        _Memory.Connect_Input( Component ) ;
        Connect_Input := _User_Interface.Set_Error( 0 ) ;
        exit ;
    end ;

    Inputs.Add( Component ) ;
    Connect_Input := _User_Interface.Set_Error( TCosmacElfErr_Success ) ;
end ;


function TCosmac_Elf.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( TCosmacElfErr_Invalid_Component ) ;
        exit ;
    end ;
    if( Component.CPU <> nil ) then
    begin
        _CPU.Connect_Output( Component ) ;
        Result := _User_Interface.Set_Error( 0 ) ;
        exit ;
    end ;
    if( Component.Memory <> nil ) then
    begin
        _Memory.Connect_Output( Component ) ;
        Result := _User_Interface.Set_Error( 0 ) ;
        exit ;
    end ;

    Outputs.Add( Component ) ;
    Connect_Output := _User_Interface.Set_Error( TCosmacElfErr_Success ) ;
end ;


function TCosmac_Elf.Debugger : TDebug_Interface ;

begin
    Result := TCosmac_Elf_Debugger.Create ;
    TCosmac_Elf_Debugger( Result ).Panel := Self ;
end ;


function TCosmac_Elf.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( TCosmacElfErr_Component_Not_Found ) ;
    end else
    begin
        if( Component.CPU <> nil ) then
        begin
            Result := _CPU.Disconnect_Input( Component ) ;
            exit ;
        end ;
        if( Component.Memory <> nil ) then
        begin
            Result := _Memory.Disconnect_Input( Component ) ;
            exit ;
        end ;
	Disconnect_Input := _User_Interface.Set_Error( TCosmacElfErr_Success ) ;
	Inputs.Remove( Component ) ;
        if( ( Outputs.Count = 0 ) and ( Inputs.Count = 0 ) ) then
        begin
            _User_Interface.Our_Screen.Q_LED.Brush.Color := clMaroon ;
        end ;
    end ;
end ;


function TCosmac_Elf.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.IndexOf( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Disconnect_Output :=
            _User_Interface.Set_Error( TCosmacElfErr_Component_Not_Found ) ;
    end else
    begin
        if( Component.CPU <> nil ) then
        begin
            Result := _CPU.Disconnect_Output( Component ) ;
            exit ;
        end ;
        if( Component.Memory <> nil ) then
        begin
            Result := _Memory.Disconnect_Output( Component ) ;
            exit ;
        end ;

	Disconnect_Output := _User_Interface.Set_Error( TCosmacElfErr_Success ) ;
	Outputs.Remove( Component ) ;
        if( ( Outputs.Count = 0 ) and ( Inputs.Count = 0 ) ) then
        begin
            _User_Interface.Our_Screen.Q_LED.Brush.Color := clMaroon ;
        end ;
    end ;
end ;


function TCosmac_Elf.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Inputs.Count ) ) then
    begin
        Result := Inputs[ Index ] ;
    end else
    begin
	Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'Cosmac Elf'#0 ;

function TCosmac_Elf.Name : PChar ;

begin
    Name := _Name ;
end ;


function TCosmac_Elf.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index >= 0 ) and ( Index < Outputs.Count ) ) then
    begin
        Result := Outputs[ Index ] ;
    end else
    begin
	Output_Component := nil ;
    end ;
end ;


function TCosmac_Elf.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    In_Read := True ;
    Read := False ;
end ; { TCosmac_Elf.Read }


function TCosmac_Elf.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    Write := _User_Interface.Set_Error( TCosmacElfErr_Address_Out_Of_Range ) ;
    case IO_Type of
      IO_Type_Memory, IO_Type_IO:
          begin
              _User_Interface.Our_Screen.Data_Low.Caption := copy( '0123456789ABCDEF', ( Value and $F ) + 1, 1 ) ;
              _User_Interface.Our_Screen.Data_High.Caption := copy( '0123456789ABCDEF', ( ( Value shr 4 ) and $F ) + 1, 1 ) ;
          end ;
    end ;
end ; // TCosmac_Elf.Write


procedure TCosmac_Elf.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TCosmac_Elf.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TCosmac_Elf.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TCosmac_Elf.Set_Parent( Component : TComponent ) ;

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


procedure TCosmac_Elf.Set_Up( P : PChar ) ;

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
end ; // TCosmac_Elf.Set_Up


procedure TCosmac_Elf.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

var S : string ;

begin
    S := string( Component.Signal_Name( Index ) ) ;
    if( S = 'Q' ) then
    begin
        if( Component.Signal_Active_Low( Index ) ) then
        begin
            Active := not Active ;
        end ;
        if( Active ) then
        begin
            _User_Interface.Our_Screen.Q_LED.Brush.Color := clRed ;
        end else
        begin
            _User_Interface.Our_Screen.Q_LED.Brush.Color := clMaroon ;
        end ;
    end ;
end ;


procedure TCosmac_Elf.Child_Notification( Child : TComponent ;
    var Notice : longint ; var Params : int64 ) ;

begin
    if( Child = nil ) then
    begin
        exit ;
    end ;
end ;


function TCosmac_Elf.Get_Port_Name( Index : longint ) : PChar ;

begin
    Index := Port_Index( Index ) ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        Temp_Port_Name := 'Port ' + inttostr( Index div 2 ) ;
        Result := PChar( Temp_Port_Name ) ;
    end ;
end ;


function TCosmac_Elf.Get_Port_Description( Index : longint ) : PChar ;

begin
    Index := Port_Index( Index ) ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        Temp_Port_Description := Port_Components[ True, Index ].Name ;
        Result := PChar( Temp_Port_Description ) ;
    end ;
end ;


function TCosmac_Elf.Get_Port( Index : longint ) : TComponent ;

begin
    Index := Port_Index( Index ) ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        Result := Port_Components[ True, Index ] ;
    end ;
end ;


function TCosmac_Elf.Get_Port_Connection( Index : longint ) : TComponent ;

begin
    Index := Port_Index( Index ) ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        Result := Port_Connections[ True, Index ] ;
    end ;
end ;



function TCosmac_Elf.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

begin
    Result := False ;
    if( string( Name ) = 'EF4' ) then
    begin
        Result := True ;
        State := EF4 ;
    end ;
end ;


function TCosmac_Elf.Signal_Count : longint ;

begin
    Result := 1 ;
end ;


function TCosmac_Elf.Signal_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( Index = 0 ) then
    begin
        Temp_Signal_Name := 'EF4' ;
        Result := PChar( Temp_Signal_Name ) ;
    end ;
end ;


function TCosmac_Elf.Signal_Out( Index : longint ) : boolean ;

begin
    Result := ( Index = 0 ) ;
end ;


function TCosmac_Elf.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := ( Index = 0 ) ;
end ;


function TCosmac_Elf.Signal_Index( Name : PChar ) : integer ;

begin
    Result := -1 ;
    if( string( Name ) = 'EF4' ) then
    begin
        Result := 0 ;
    end ;
end ;


procedure TCosmac_Elf.UI_Notice( Code : longint ; var Data : int64 ) ;

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

