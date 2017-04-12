{$N+}
{
        Program Name : LB_201_KB
        Package Name : CEF
        Purpose      : DEC LK201 keyboard
        Institution  : 
        Date Written : 31-Dec-2006
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

	  This unit DEC LK201 keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit LK_201_KB ;

interface

{$I EDEFINES.INC}

uses // Borland...
     Classes,

     // C&C...
     Compatib,
     Collect, { TCollection }
     _DebugIn, // TDebug_Interface
     DebugInt, // TText_Debugger
     _Streams, // TCOM_Stream
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TUser_Interface
     CEF, { TBase_Memory }
     _CEFUtil, // TCEF_Watchpoint

     // LK201_KB...
     LK201KB_Main ; // TMain_Form

const LK201KB_Facility = -1 ;
      LK201KB_Success = 0 ;
      LK201KB_Component_Not_Found = 1 ;
      LK201KB_Invalid_Component = 2 ;
      LK201KB_No_Matching_Watchpoint = 3 ;

type TLK201KB_UI = class ;
     TLK201KB_KB = class ;

     TLK201KB = class( TBase_Component )
                    private // Instance data...
                        Inputs, Outputs : TList ;
                        _Tag : longint ;
                        _Parent : TComponent ;

                    private // Internal utility routines...
                        function Default_Input : TComponent ;
                        function Default_Output : TComponent ;

                    public { Public instance data... }
                        _Serial_Number : integer ;
                        _User_Interface : TLK201KB_UI ;
                        _Keyboard : TLK201KB_KB ;

                    public // API...
                        function User_Interface : TUser_Interface ; override ;

                        function Keyboard : TKeyboard ; override ;

                        function Facility_Code : longint ; override ;

                        function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                            override ;

                        function Terminate : TUnified_Exception ; override ;

                        function Clear_Watchpoint( Address : int64 ;
                            Memory : boolean ; Access : longint ) : TUnified_Exception ;
                            override ;

                        function Component_Type : longint ; override ;

                        function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                            override ;

                        function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                            override ;

                        function Debugger : TDebug_Interface ; override ;

                        function Deposit( Address : int64 ; Size : longint ;
                            Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;
                            override ;

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

                        function Get_Read_Latency : longint ; override ;

                        function Get_Write_Latency : longint ; override ;

                        function Input_Component( Index : longint ) : TComponent ;
                            override ;

                        function Name : PChar ; override ;

                        function Output_Component( Index : longint ) : TComponent ;
                            override ;

                        function Read( Address : int64 ; Size : longint ;
                            IO_Type : longint ) : boolean ; override ;

                        function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                            override ;

                        function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                            override ;

                        function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                            override ;

                        function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                            override ;

                        function Set_Access_Mode( Low, High : int64 ;
                            Memory : boolean ; Typ : longint ) : TUnified_Exception ; override ;

                        procedure Set_Profiling( _On, Children : boolean ) ;
                            override ;

                        procedure Set_Read_Latency( Value : longint ) ; override ;

                        function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                            Access : longint ) : TUnified_Exception ; override ;

                        procedure Set_Write_Latency( Value : longint ) ;
                            override ;

                        procedure Show_Status ; override ;

                        function Write( Address : int64 ; Value, Size : longint ;
                            IO_Type : longint ) : TUnified_Exception ; override ;

                        procedure Set_Tag( Value : longint ) ; override ;

                        function Get_Tag : longint ; override ;

                        function Get_Parent : TComponent ; override ;

                        procedure Set_Parent( Component : TComponent ) ;
                            override ;

                        procedure Set_Up( P : PChar ) ; override ;
                end ;

     TLK201KB_UI = class( TBase_User_Interface )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

	    private // Instance data...
            Keys : TStringList ;
            Main_Form : TMain_Form ;
            Parent : TLK201KB ;

          protected // Internal utility routines...
            function Translate_Error( Code : longint ) : string ; override ;

          public // TUser_Intrerface overrides...
            function Get_Hidden : boolean ; override ;

            procedure Set_Hidden( Value : boolean ) ; override ;

            function Get_Parent_Window : THandle ; override ;

            procedure Set_Parent_Window( Value : THandle ) ; override ;

            function Optimal_Height : integer ; override ;

            function Optimal_Width : integer ; override ;
     end ;


     TLK201KB_KB = class( TBase_Keyboard )
                       private // Instance data...
                           Temp_Key : string ;
                           Temp_Key_Name : string ;
                           Temp_LED_Name : string ;
                           Parent : TLK201KB ;
                           
                       public // TKeyboard overrides...
                           function Get_Key : PChar ; override ;

                           function Get_Key_Down( Name : PChar ) : boolean ; override ;

                           function Get_Key_Name( Index : integer ) : PChar ; override ;

                           function Get_LED_State( Name : PChar ) : boolean ; override ;

                           procedure Set_LED_State( Name : PChar ; State : boolean ) ;
                               override ;

                           function Get_LED_Name( Index : integer ) : PChar ; override ;

                       public // API...
                           procedure Add_Key( Key : string ; Value, Up : integer ) ;
                   end ;

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
     Windows, // MessageBeep
     Forms, // Application
     Graphics, // TColor
     Sysutils, // Allocmem

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     Num1s,
     Parse ; // TString_Parser


{ TLK201KB_Debugger methods... }

type TLK201KB_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TLK201KB ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TLK201KB
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TLK201KB_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TLK201KB_Debugger.Count : longint ;

begin
    Result := 3 ;
end ;



// TLK201KB_UI methods...

// Constructors and destructors...

constructor TLK201KB_UI.Create ;

begin
    inherited Create ;
    
    Keys := TStringList.Create ;

    Application.CreateForm( TMain_Form, Main_Form ) ;
    try
        Main_Form.Visible := True ;
    except
    end ;
end ;


destructor TLK201KB_UI.Destroy ;

begin
    Keys.Free ;
    Keys := nil ;
    Main_Form.Close ;
    Main_Form := nil ;

    inherited Destroy ;
end ;


// API...

function TLK201KB_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        LK201KB_Success: _Error := 'Success' ;
        LK201KB_Component_Not_Found : _Error := 'Component not found' ;
        LK201KB_Invalid_Component : _Error := 'Invalid component' ;
        LK201KB_No_Matching_Watchpoint : _Error := 'No Matching Watchpoint' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { TLK201KB_UI.Translate_Error }


function TLK201KB_UI.Get_Hidden : boolean ;

begin
    Result := Main_Form.Visible ;
end ;


procedure TLK201KB_UI.Set_Hidden( Value : boolean ) ;

begin
    Main_Form.Visible := not Value ;
end ;


function TLK201KB_UI.Get_Parent_Window : THandle ;

begin
    Result := Main_Form.ParentWindow ;
end ;


procedure TLK201KB_UI.Set_Parent_Window( Value : THandle ) ;

begin
    Main_Form.ParentWindow := Value ;
    Main_Form.Top := 0 ;
    Main_Form.Left := 0 ;
end ;


function TLK201KB_UI.Optimal_Height : integer ;

begin
    Result := Main_Form.Height ;
end ;


function TLK201KB_UI.Optimal_Width : integer ;

begin
    Result := Main_Form.Width ;
end ;



// TLK201KB_KB methods...

function TLK201KB_KB.Get_Key : PChar ;

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


function TLK201KB_KB.Get_Key_Down( Name : PChar ) : boolean ;

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


function TLK201KB_KB.Get_Key_Name( Index : integer ) : PChar ;

begin
    Result := nil ; // Assume failure
    if( ( Index < 0 ) or ( Index > 179 ) ) then
    begin
        exit ;
    end ;
    if( ( Index < 31 ) and ( Index <> 8 ) ) then
    begin
        Index := Index + 64 ;
    end ;
    Index := ord( upcase( char( Index ) ) ) ;
    case Index of
        0 : Temp_Key_Name := 'COMPOSE CHARACTER' ;
        1..127 : Temp_Key_Name := chr( Index ) ;
        128 : Temp_Key_Name := 'LEFT' ;
        129 : Temp_Key_Name := 'RIGHT' ;
        130 : Temp_Key_Name := 'UP' ;
        131 : Temp_Key_Name := 'DOWN' ;
        132 : Temp_Key_Name := 'HELP' ;
        133 : Temp_Key_Name := 'CONTROL' ;
        134 : Temp_Key_Name := 'LOCK' ;
        135 : Temp_Key_Name := 'LEFT_SHIFT' ;
        136 : Temp_Key_Name := 'RIGHT_SHIFT' ;
        137 : Temp_Key_Name := 'PF1' ;
        138 : Temp_Key_Name := 'PF2' ;
        139 : Temp_Key_Name := 'PF3' ;
        140 : Temp_Key_Name := 'PF4' ;
        141 : Temp_Key_Name := 'NKP_0' ;
        142 : Temp_Key_Name := 'NKP_1' ;
        143 : Temp_Key_Name := 'NKP_2' ;
        144 : Temp_Key_Name := 'NKP_3' ;
        145 : Temp_Key_Name := 'NKP_4' ;
        146 : Temp_Key_Name := 'NKP_5' ;
        147 : Temp_Key_Name := 'NKP_6' ;
        148 : Temp_Key_Name := 'NKP_7' ;
        149 : Temp_Key_Name := 'NKP_8' ;
        150 : Temp_Key_Name := 'NKP_9' ;
        151 : Temp_Key_Name := 'NKP_.' ;
        152 : Temp_Key_Name := 'NKP_,' ;
        153 : Temp_Key_Name := 'NKP_-' ;
        154 : Temp_Key_Name := 'NKP_ENTER' ;
        155 : Temp_Key_Name := 'F1' ;
        156 : Temp_Key_Name := 'F2' ;
        157 : Temp_Key_Name := 'F3' ;
        158 : Temp_Key_Name := 'F4' ;
        159 : Temp_Key_Name := 'F5' ;
        160 : Temp_Key_Name := 'F6' ;
        161 : Temp_Key_Name := 'F7' ;
        162 : Temp_Key_Name := 'F8' ;
        163 : Temp_Key_Name := 'F9' ;
        164 : Temp_Key_Name := 'F10' ;
        165 : Temp_Key_Name := 'F11' ;
        166 : Temp_Key_Name := 'F12' ;
        167 : Temp_Key_Name := 'F13' ;
        168 : Temp_Key_Name := 'F14' ;
        169 : Temp_Key_Name := 'F17' ;
        170 : Temp_Key_Name := 'F18' ;
        171 : Temp_Key_Name := 'F19' ;
        172 : Temp_Key_Name := 'F20' ;
        173 : Temp_Key_Name := 'FIND' ;
        174 : Temp_Key_Name := 'INSERT HERE' ;
        175 : Temp_Key_Name := 'REMOVE' ;
        176 : Temp_Key_Name := 'SELECT' ;
        177 : Temp_Key_Name := 'PREV SCREEN' ;
        178 : Temp_Key_Name := 'NEXT SCREEN' ;
        179 : Temp_Key_Name := 'DO' ;
        else exit ;
    end ;

    Result := PChar( Temp_Key_Name ) ;
end ;


function TLK201KB_KB.Get_LED_State( Name : PChar ) : boolean ;

var S : string ;

begin
    Result := False ;
    S := string( Name ) ;
    if( S = 'HOLD SCREEN' ) then
    begin
        Result := ( Parent._User_Interface.Main_Form.Hold_Screen_LED.Brush.Color = clRed ) ;
    end else
    if( S = 'WAIT' ) then
    begin
        Result := ( Parent._User_Interface.Main_Form.Wait_LED.Brush.Color = clRed ) ;
    end else
    if( S = 'COMPOSE' ) then
    begin
        Result := ( Parent._User_Interface.Main_Form.Compose_LED.Brush.Color = clRed ) ;
    end else
    if( S = 'LOCK' ) then
    begin
        Result := ( Parent._User_Interface.Main_Form.Lock_LED.Brush.Color = clRed ) ;
    end ;
end ;


procedure TLK201KB_KB.Set_LED_State( Name : PChar ; State : boolean ) ;

var Color : TColor ;
    S : string ;

begin
    // Setup...
    S := string( Name ) ;
    if( State ) then
    begin
        Color := clRed ;
    end else
    begin
        Color := clMaroon ;
    end ;

    // Set the LED...
    if( S = 'HOLD SCREEN' ) then
    begin
        Parent._User_Interface.Main_Form.Hold_Screen_LED.Brush.Color := Color ;
    end else
    if( S = 'WAIT' ) then
    begin
        Parent._User_Interface.Main_Form.Wait_LED.Brush.Color := Color ;
    end else
    if( S = 'COMPOSE' ) then
    begin
        Parent._User_Interface.Main_Form.Compose_LED.Brush.Color := Color ;
    end else
    if( S = 'LOCK' ) then
    begin
        Parent._User_Interface.Main_Form.Lock_LED.Brush.Color := Color ;
    end ;
end ;


function TLK201KB_KB.Get_LED_Name( Index : integer ) : PChar ;

begin
    case Index of
        0 : Temp_LED_Name := 'HOLD SCREEN' ;
        1 : Temp_LED_Name := 'WAIT' ;
        2 : Temp_LED_Name := 'COMPOSE' ;
        3 : Temp_LED_Name := 'LOCK' ;
        else
            begin
                Result := nil ;
                exit ;
            end ;
    end ;
    Result := PChar( Temp_LED_Name ) ;
end ;


procedure TLK201KB_KB.Add_Key( Key : string ; Value, Up : integer ) ;

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



// TLK201KB methods...

function TLK201KB.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TLK201KB.Default_Output : TComponent ;

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

function TLK201KB.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TLK201KB.Keyboard : TKeyboard ;

begin
    Result := _Keyboard ;
end ;


function TLK201KB.Facility_Code : longint ;

begin
    Facility_Code := LK201KB_Facility ;
end ;


function TLK201KB.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    if( _User_Interface = nil ) then // Not already initialized
    begin
        { General setup... }
        Inputs := TList.Create ;
        Outputs := TList.Create ;
        _User_Interface := TLK201KB_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface.Main_Form._UI := UI ;
        _User_Interface.Main_Form._LK201KB := self ;
        _Keyboard := TLK201KB_KB.Create ;
        _Keyboard.Parent := self ;
    end ;
    Initialize := _User_Interface.Set_Error( LK201KB_Success ) ;
end ; { TLK201KB.Initialize }


function TLK201KB.Terminate : TUnified_Exception ;

begin
    if( _User_Interface.Main_Form._UI <> nil ) then
    begin
        _User_Interface.Main_Form._UI.Termination_Notice( self ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Terminate := _User_Interface.Set_Error( LK201KB_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    _Keyboard.Free ;
    _Keyboard := nil ;
    Free ;
end ; { TLK201KB.Terminate }



function TLK201KB.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLK201KB.Component_Type : longint ;

begin
    Component_Type := Component_Type_Keyboard ;
end ;


function TLK201KB.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( LK201KB_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Connect_Input := _User_Interface.Set_Error( LK201KB_Success ) ;
end ;


function TLK201KB.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( LK201KB_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Connect_Output := _User_Interface.Set_Error( LK201KB_Success ) ;
end ;


function TLK201KB.Debugger : TDebug_Interface ;

begin
    Result := TLK201KB_Debugger.Create ;
    TLK201KB_Debugger( Result ).Memory := Self ;
end ;


function TLK201KB.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLK201KB.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( LK201KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( LK201KB_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TLK201KB.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( LK201KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( LK201KB_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TLK201KB.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLK201KB.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    Result := 0 ;
end ;


function TLK201KB.Get_Profiling : boolean ;

begin
    Get_Profiling := False ;
end ;


function TLK201KB.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := 0 ;
end ;


function TLK201KB.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := 0 ;
end ;


function TLK201KB.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


const _Name : PChar = 'LK201 Keyboard'#0 ;

function TLK201KB.Name : PChar ;

begin
    Name := _Name ;
end ;


function TLK201KB.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


function TLK201KB.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ;
end ; { TLK201KB.Read }


function TLK201KB.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLK201KB.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLK201KB.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLK201KB.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLK201KB.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TLK201KB.Set_Profiling( _On, Children : boolean ) ;

begin
    // This routine left intentionally blank
end ;


procedure TLK201KB.Set_Read_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TLK201KB.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TLK201KB.Set_Write_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank - no status to show }
end ;


procedure TLK201KB.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TLK201KB.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ; // TLK201KB.Write


procedure TLK201KB.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TLK201KB.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TLK201KB.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TLK201KB.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


procedure TLK201KB.Set_Up( P : PChar ) ;

begin
end ;


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

