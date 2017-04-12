{$N+}
{
        Program Name : SOL_20_KB
        Package Name : CEF
        Purpose      : Processor Technology SOL-20 keyboard CEF component
        Institution  : 
        Date Written : 10-Apr-2006
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

	  This unit implements the Processor Technology SOL-20 keyboard
        component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit SOL_20_KB ;

interface

{$I EDEFINES.INC}

uses // C&C...
     Compatib,
     Classes,
     Collect, { TCollection }
     _DebugIn, // TDebug_Interface
     DebugInt, // TText_Debugger
     _Streams, // TCOM_Stream
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TUI_Interface
     CEF, { TBase_Memory }
     _CEFUtil, // TCEF_Watchpoint

     // SOL-20...
     SOL20KB_Main ; // TMain_Form

const SOL20_KB_Facility = -1 ;
      SOL20_KB_Success = 0 ;
      SOL20_KB_Component_Not_Found = 1 ;
      SOL20_KB_Invalid_Component = 2 ;

type TSOL20_KB_UI = class ;
     TSOL20_KB_KB = class ;

     TSOL20_KB = class( TBase_Component )
                     private // Instance data...
                         Inputs, Outputs : TList ;
                         _Tag : longint ;
                         _Parent : TComponent ;

                     private // Internal utility routines...
                         function Default_Input : TComponent ;
                         function Default_Output : TComponent ;

                     public { Public instance data... }
                         _Serial_Number : integer ;
                         _User_Interface : TSOL20_KB_UI ;
                         _Keyboard : TSOL20_KB_KB ;

                     public // API...
                         function USer_Interface : TUser_Interface ; override ;

                         function Keyboard : TKeyboard ; override ;

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

                         procedure Show_Status ; override ;

                         procedure Set_Tag( Value : longint ) ; override ;

                         function Get_Tag : longint ; override ;

                         function Get_Parent : TComponent ; override ;

                         procedure Set_Parent( Component : TComponent ) ;
                             override ;

                         procedure Set_Up( P : PChar ) ; override ;
                 end ;

     TSOL20_KB_UI = class( TBase_User_Interface )
	    public // COnstructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private // Instance data...
            Keys : TStringList ;
            Main_Form : TMain_Form ;
            Parent : TSOL20_KB ;
            _UI : TUI_Interface ;

        protected
            function Translate_Error( Code : longint ) : string ; override ;

        public // TUser_Intrerface overrides...
            function Get_Hidden : boolean ; override ;

            procedure Set_Hidden( Value : boolean ) ; override ;

            function Get_Parent_Window : THandle ; override ;

            procedure Set_Parent_Window( Value : THandle ) ; override ;

            function Optimal_Height : integer ; override ;

            function Optimal_Width : integer ; override ;
     end ; // TSOL20_KB


     TSOL20_KB_KB = class( TBase_Keyboard )
                        private // Instance data...
                            Temp_Key_Name : string ;
                            Temp_Key : string ;
                            Temp_LED_Name : string ;
                            Parent : TSOL20_KB ;

                        public // TKeyboard overrides...
                            function Get_Key : PChar ; override ;

                            function Get_Key_Down( Name : PChar ) : boolean ;
                                override ;

                            function Get_Key_Name( Index : integer ) : PChar ;
                                override ;

                            function Get_LED_State( Name : PChar ) : boolean ;
                                override ;

                            procedure Set_LED_State( Name : PChar ; State : boolean ) ;
                                override ;

                            function Get_LED_Name( Index : integer ) : PChar ;
                                override ;

                        public // API...
                            procedure Add_Key( Key : string ; Value, Up : integer ) ;
                    end ;

var _SOL20KB : TSOL20_KB ;

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
     Windows, // MessageBeep
     Graphics, // TColor
     Forms, // Application
     Sysutils, // Allocmem

     // C&C...
     CommonUt, // Edit
     Num1s ;

{ TSOL20_KB_KB_Debugger methods... }

type TSOL20_KB_KB_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TSOL20_KB ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TSOL20_KB
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TSOL20_KB_KB_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
                if( Memory.Default_Input = nil ) then
                begin
                    I.Title := 'Default_Input = nil' ;
                end else
                begin
                    I1 := Memory.Default_Input.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Input = ' + Pointer_To_String( Memory.Default_Input ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Input = ' + Pointer_To_String( Memory.Default_Input ) ) ;
                end ;
            end ;
        1 : begin
                if( Memory.Default_Output = nil ) then
                begin
                    I.Title := 'Default_Output = nil' ;
                end else
                begin
                    I1 := Memory.Default_Output.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Output = ' + Pointer_To_String( Memory.Default_Output ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Output = ' + Pointer_To_String( Memory.Default_Output ) ) ;
                end ;
            end ;
        2 : I.Title := PChar( '_Serial_Number = ' + Num1( Memory._Serial_Number ) ) ;
        3 : begin
                if( Memory._User_Interface._UI = nil ) then
                begin
                    I.Title := '_UI = nil' ;
                end else
                begin
                    I1 := Memory._User_Interface._UI.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( '_UI = ' + Pointer_To_String( Memory._User_Interface._UI ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( '_UI = ' + Pointer_To_String( Memory._User_Interface._UI ) ) ;
                end ;
            end ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TSOL20_KB_KB_Debugger.Count : longint ;

begin
    Result := 4 ;
end ;



// TSOL20_KB_UI methods...

// COnstructors and destructors...

constructor TSOL20_KB_UI.Create ;

begin
    inherited Create ;

    Keys := TStringList.Create ;

    Application.CreateForm( TMain_Form, Main_Form ) ;
    Main_Form.Visible := True ;
end ;


destructor TSOL20_KB_UI.Destroy ;

begin
    Keys.Free ;
    Keys := nil ;
    Main_Form.Close ;
    Main_Form := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TSOL20_KB_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        SOL20_KB_Success: _Error := 'Success' ;
        SOL20_KB_Component_Not_Found : _Error := 'Component not found' ;
        SOL20_KB_Invalid_Component : _Error := 'Invalid component' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { tFS_EI.Translate_Error }


// API...

function TSOL20_KB_UI.Get_Hidden : boolean ;

begin
    Result := Main_Form.Visible ;
end ;


procedure TSOL20_KB_UI.Set_Hidden( Value : boolean ) ;

begin
    Main_Form.Visible := not Value ;
end ;


function TSOL20_KB_UI.Get_Parent_Window : THandle ;

begin
    Result := Main_Form.ParentWindow ;
end ;


procedure TSOL20_KB_UI.Set_Parent_Window( Value : THandle ) ;

begin
    Main_Form.ParentWindow := Value ;
    Main_Form.Top := 0 ;
    Main_Form.Left := 0 ;
end ;


function TSOL20_KB_UI.Optimal_Height : integer ;

begin
    Result := Main_Form.Height ;
end ;


function TSOL20_KB_UI.Optimal_Width : integer ;

begin
    Result := Main_Form.Width ;
end ;



// TSOL20_KB_KB methods...

function TSOL20_KB_KB.Get_Key : PChar ;

begin
    Result := nil ;
    if( Parent._User_Interface.Keys.Count = 0 ) then // No keys waiting
    begin
        exit ;
    end ;
    Temp_Key := Parent._User_Interface.Keys[ 0 ] ;
    Parent._User_Interface.Keys.Delete( 0 ) ;
    Result := PChar( Temp_Key ) ;
end ;


function TSOL20_KB_KB.Get_Key_Down( Name : PChar ) : boolean ;

var I, Loop : integer ;
    S : string ;

begin
    Result := False ; // Assume failure
    S := string( Name ) ;
    S := uppercase( S ) ;
    I := Parent._User_Interface.Keys.IndexOf( S ) ;
    if( I = -1 ) then
    begin
        exit ;
    end ;
    for Loop := I downto 0 do
    begin
        if( Parent._User_Interface.Keys[ Loop ] = #0 ) then
        begin
            exit ; // This means the key was found in another keystroke
        end ;
    end ;
    Result := True ;
end ;


function TSOL20_KB_KB.Get_Key_Name( Index : integer ) : PChar ;

begin
    Result := nil ; // Assume failure
    if( ( Index < 0 ) or ( Index > 143 ) ) then
    begin
        exit ;
    end ;
    if( ( Index < 31 ) and ( Index <> 8 ) ) then
    begin
        Index := Index + 64 ;
    end ;
    Index := ord( upcase( char( Index ) ) ) ;
    case Index of
        0 : Temp_Key_Name := 'BREAK' ;
        1..127 : Temp_Key_Name := chr( Index ) ;
        128 : Temp_Key_Name := 'LEFT' ;
        129 : Temp_Key_Name := 'RIGHT' ;
        130 : Temp_Key_Name := 'UP' ;
        131 : Temp_Key_Name := 'DOWN' ;
        132 : Temp_Key_Name := 'LEFT_SHIFT' ;
        133 : Temp_Key_Name := 'RIGHT_SHIFT' ;
        134 : Temp_Key_Name := 'SHIFT LOCK' ;
        135 : Temp_Key_Name := 'DIVIDE' ;
        136 : Temp_Key_Name := 'UPPER CASE' ;
        137 : Temp_Key_Name := 'LOCAL' ;
        138 : Temp_Key_Name := 'CLEAR' ;
        139 : Temp_Key_Name := 'HOME CURSOR' ;
        140 : Temp_Key_Name := 'LOAD' ;
        141 : Temp_Key_Name := 'MODE SELECT' ;
        142 : Temp_Key_Name := 'LEFT_CONTROL' ;
        143 : Temp_Key_Name := 'RIGHT_CONTROL' ;
        else exit ;
    end ;

    Result := PChar( Temp_Key_Name ) ;
end ;


function TSOL20_KB_KB.Get_LED_State( Name : PChar ) : boolean ;

var S : string ;

begin
    Result := False ;
    S := string( Name ) ;
    S := uppercase( S ) ;
    if( S = 'LOCK' ) then
    begin
        Result := ( Parent._User_Interface.Main_Form.Shift_Lock_LED.Brush.Color = clLime ) ;
    end else
    if( S = 'LOCAL' ) then
    begin
        Result := ( Parent._User_Interface.Main_Form.Local_LED.Brush.Color = clLime ) ;
    end else
    if( S = 'UPPER' ) then
    begin
        Result := ( Parent._User_Interface.Main_Form.Uppercase_LED.Brush.Color = clLime ) ;
    end ;
end ;


procedure TSOL20_KB_KB.Set_LED_State( Name : PChar ; State : boolean ) ;

var Color : TColor ;
    S : string ;

begin
    S := string( Name ) ;
    S := uppercase( S ) ;
    if( State ) then
    begin
        Color := clLime ;
    end else
    begin
        Color := clGreen ;
    end ;
    if( S = 'LOCK' ) then
    begin
        Parent._User_Interface.Main_Form.Shift_Lock_LED.Brush.Color := Color ;
    end else
    if( S = 'LOCAL' ) then
    begin
        Parent._User_Interface.Main_Form.Local_LED.Brush.Color := Color ;
    end else
    if( S = 'UPPER' ) then
    begin
        Parent._User_Interface.Main_Form.Uppercase_LED.Brush.Color := Color ;
    end ;
end ;


function TSOL20_KB_KB.Get_LED_Name( Index : integer ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : Temp_LED_Name := 'LOCK' ;
        1 : Temp_LED_Name := 'LOCAL' ;
        2 : Temp_LED_Name := 'UPPER' ;
        else Temp_LED_Name := '' ;
    end ;
    if( Temp_LED_Name <> '' ) then
    begin
        Result := PChar( Temp_LED_Name ) ;
    end ;
end ;


procedure TSOL20_KB_KB.Add_Key( Key : string ; Value, Up : integer ) ;

var C : TComponent ;
    Loop : integer ;

begin
    if( Up = 1 ) then
    begin
        Parent._User_Interface.Keys.Add( Key ) ;
    end ;
    for Loop := 0 to Parent.Inputs.Count - 1 do
    begin
        C := TComponent( Parent.Inputs[ Loop ] ) ;
        C.Write( Value, Up, 1, IO_Type_IO )
    end ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        C := TComponent( Parent.Outputs[ Loop ] ) ;
        C.Write( Value, Up, 1, IO_Type_IO )
    end ;
end ;



// TSOL20_KB methods...

function TSOL20_KB.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TSOL20_KB.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


{ API... }

function TSOL20_KB.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TSOL20_KB.Keyboard : TKeyboard ;

begin
    Result := _Keyboard ; 
end ;


function TSOL20_KB.Facility_Code : longint ;

begin
    Facility_Code := SOL20_KB_Facility ;
end ;


function TSOL20_KB.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    if( _User_Interface = nil ) then
    begin
        _User_Interface := TSOL20_KB_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface._UI := UI ;
        _Keyboard := TSOL20_KB_KB.Create ;
        _Keyboard.Parent := self ;
    end ;
    _SOL20KB := self ;

    Initialize := _User_Interface.Set_Error( SOL20_KB_Success ) ;
end ; { TSOL20_KB.Initialize }


function TSOL20_KB.Terminate : TUnified_Exception ;

begin
    if( _User_Interface._UI <> nil ) then
    begin
        _User_Interface._UI.Termination_Notice( self ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Terminate := _User_Interface.Set_Error( SOL20_KB_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    _Keyboard.Free ;
    _Keyboard := nil ;
    Free ;
end ; { TSOL20_KB.Terminate }



function TSOL20_KB.Component_Type : longint ;

begin
    Component_Type := Component_Type_Keyboard ;
end ;


function TSOL20_KB.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( SOL20_KB_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Connect_Input := _User_Interface.Set_Error( SOL20_KB_Success ) ;
end ;


function TSOL20_KB.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( SOL20_KB_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Connect_Output := _User_Interface.Set_Error( SOL20_KB_Success ) ;
end ;


function TSOL20_KB.Debugger : TDebug_Interface ;

begin
    Result := TSOL20_KB_KB_Debugger.Create ;
    TSOL20_KB_KB_Debugger( Result ).Memory := Self ;
end ;


function TSOL20_KB.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
	    Disconnect_Input := _User_Interface.Set_Error( SOL20_KB_Component_Not_Found ) ;
    end else
    begin
	    Disconnect_Input := _User_Interface.Set_Error( SOL20_KB_Success ) ;
	    Inputs.Remove( Component ) ;
    end ;
end ;


function TSOL20_KB.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( SOL20_KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( SOL20_KB_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TSOL20_KB.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


const _Name : PChar = 'SOL-20 Keyboard'#0 ;

function TSOL20_KB.Name : PChar ;

begin
    Name := _Name ;
end ;


function TSOL20_KB.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


procedure TSOL20_KB.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


procedure TSOL20_KB.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TSOL20_KB.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TSOL20_KB.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TSOL20_KB.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


procedure TSOL20_KB.Set_Up( P : PChar ) ;

begin
end ;



initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

