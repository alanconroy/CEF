{$N+}
{
        Program Name : ASR_33_KB
        Package Name : CEF
        Purpose      : ASR33 keyboard
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
        *    DATE        BY          REASON                         *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	  This unit emulates the ASR33 keyboard component for CEF.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan
        
}

unit ASR_33_KB ;

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
     _CEF,  // TUser_Interface
     CEF, { TBase_Memory }
     _CEFUtil, // TCEF_Watchpoint

     // ASR33_KB...
     ASR33KB_Main ; // TMain_Form

const ASR33KB_Facility = -1 ;
      ASR33KB_Success = 0 ;
      ASR33KB_Component_Not_Found = 1 ;
      ASR33KB_Invalid_Component = 2 ;
      ASR33KB_No_Matching_Watchpoint = 3 ;

type TASR33KB_UI = class ;
     TASR33KB_KB = class ;

     TASR33KB = class( TBase_Component )
                    private // Instance data...
                        Inputs, Outputs : TList ;
                        _Tag : longint ;
                        _Parent : TComponent ;

                    public { Public instance data... }
                        _Serial_Number : integer ;
                        _User_Interface : TASR33KB_UI ;
                        _Keyboard : TASR33KB_KB ;

                    public // Internal utility routines...
                        function Default_Input : TComponent ;
                        function Default_Output : TComponent ;

                    public // API...
                        function User_Interface : TUser_Interface ; override ;

                        function Keyboard : TKeyboard ; override ;

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
                            Memory : boolean ; Typ : longint ) : TUnified_Exception ;
                            override ;

                        procedure Set_Profiling( _On, Children : boolean ) ;
                            override ;

                        procedure Set_Read_Latency( Value : longint ) ;
                            override ;

                        function Set_Watchpoint( Address : int64 ;
                            Memory : boolean ; Access : longint ) : TUnified_Exception ;
                            override ;

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

     TASR33KB_KB = class( TBase_Keyboard )
                       private // Instance data...
                           Parent : TASR33KB ;
                           Temp_Key : string ;
                           Temp_Key_Name : string ;

                       public // TKeyboard overrides...
                           function Get_Key : PChar ; override ;

                           function Get_Key_Down( Name : PChar ) : boolean ;
                               override ;

                           function Get_Key_Name( Index : integer ) : PChar ;
                               override ;

                       public // API...
                           procedure Add_Key( Key : string ; Value, Up : integer ) ;
                   end ;

     TASR33KB_UI = class( TBase_User_Interface )
	public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private // Instance data...
            Keys : TStringList ;
            Main_Form : TMain_Form ;
            Parent : TASR33KB ;
            _UI : TUI_Interface ;

          public // TUser_Intrerface overrides...
            function Translate_Error( Code : longint ) : string ; override ;
            
            function Get_Hidden : boolean ; override ;

            procedure Set_Hidden( Value : boolean ) ; override ;

            function Get_Parent_Window : THandle ; override ;

            procedure Set_Parent_Window( Value : THandle ) ; override ;

            function Optimal_Height : integer ; override ;

            function Optimal_Width : integer ; override ;
     end ;

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
     Windows, // MessageBeep
     Forms, // Application
     Sysutils, // Allocmem

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     Num1s,
     Parse ; // TString_Parser


{ TASR33KB_Debugger methods... }

type TASR33KB_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TASR33KB ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TASR33KB
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TASR33KB_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TASR33KB_Debugger.Count : longint ;

begin
    Result := 3 ;
end ;



// Constructors and destructors...

constructor TASR33KB_UI.Create ;

begin
    inherited Create ;

    Keys := TStringList.Create ;

    Application.CreateForm( TMain_Form, Main_Form ) ;
    try
        Main_Form.Visible := True ;
    except
    end ;
end ;


destructor TASR33KB_UI.Destroy ;

begin
    Keys.Free ;
    Keys := nil ;
    Main_Form.Close ;
    Main_Form := nil ;

    inherited Destroy ;
end ;



function TASR33KB_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        ASR33KB_Success: _Error := 'Success' ;
        ASR33KB_Component_Not_Found : _Error := 'Component not found' ;
        ASR33KB_Invalid_Component : _Error := 'Invalid component' ;
        ASR33KB_No_Matching_Watchpoint : _Error := 'No Matching Watchpoint' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { TASR33KB_UI.Translate_Error }


function TASR33KB_UI.Get_Hidden : boolean ;

begin
    Result := Main_Form.Visible ;
end ;


procedure TASR33KB_UI.Set_Hidden( Value : boolean ) ;

begin
    Main_Form.Visible := not Value ;
end ;


function TASR33KB_UI.Get_Parent_Window : THandle ;

begin
    Result := Main_Form.ParentWindow ;
end ;


procedure TASR33KB_UI.Set_Parent_Window( Value : THandle ) ;

begin
    Main_Form.ParentWindow := Value ;
    Main_Form.Top := 0 ;
    Main_Form.Left := 0 ;
end ;


function TASR33KB_UI.Optimal_Height : integer ;

begin
    Result := Main_Form.Height ;
end ;


function TASR33KB_UI.Optimal_Width : integer ;

begin
    Result := Main_Form.Width ;
end ;


// TASR22KB_KB methods...

function TASR33KB_KB.Get_Key : PChar ;

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


function TASR33KB_KB.Get_Key_Down( Name : PChar ) : boolean ;

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


function TASR33KB_KB.Get_Key_Name( Index : integer ) : PChar ;

begin
    Result := nil ; // Assume failure
    if( ( Index < 0 ) or ( Index > 132 ) ) then
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
        128 : Temp_Key_Name := 'LEFT_SHIFT' ;
        129 : Temp_Key_Name := 'RIGHT_SHIFT' ;
        130 : Temp_Key_Name := 'REPT' ;
        131 : Temp_Key_Name := 'HERE IS' ;
        132 : Temp_Key_Name := 'CONTROL' ;
        else exit ;
    end ;

    Result := PChar( Temp_Key_Name ) ;
end ;


procedure TASR33KB_KB.Add_Key( Key : string ; Value, Up : integer ) ;

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



// TASR33KB methods...

function TASR33KB.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TASR33KB.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;



// TASR33KB methods...

// API...

function TASR33KB.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TASR33KB.Keyboard : TKeyboard ;

begin
    Result := _Keyboard ;
end ;


function TASR33KB.Facility_Code : longint ;

begin
    Facility_Code := ASR33KB_Facility ;
end ;


function TASR33KB.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    if( _User_Interface = nil ) then // Not already initialized
    begin
        { General setup... }
        Inputs := TList.Create ;
        Outputs := TList.Create ;
        _User_Interface := TASR33KB_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface._UI := UI ;
        _User_Interface.Main_Form._ASR33KB := self ;
        _User_Interface.Main_Form._UI := UI ;
        _Keyboard := TASR33KB_KB.Create ;
        _Keyboard.Parent := self ;
    end ;
    Initialize := _User_Interface.Set_Error( ASR33KB_Success ) ;
end ; { TASR33KB.Initialize }


function TASR33KB.Terminate : TUnified_Exception ;

begin
    if( _User_Interface.Main_Form._UI <> nil ) then
    begin
        _User_Interface.Main_Form._UI.Termination_Notice( self ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Terminate := _User_Interface.Set_Error( ASR33KB_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    _Keyboard.Free ;
    _Keyboard := nil ;
    Free ;
end ; { TASR33KB.Terminate }



function TASR33KB.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TASR33KB.Component_Type : longint ;

begin
    Component_Type := Component_Type_Keyboard ;
end ;


function TASR33KB.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( ASR33KB_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Connect_Input := _User_Interface.Set_Error( ASR33KB_Success ) ;
end ;


function TASR33KB.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( ASR33KB_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Connect_Output := _User_Interface.Set_Error( ASR33KB_Success ) ;
end ;


function TASR33KB.Debugger : TDebug_Interface ;

begin
    Result := TASR33KB_Debugger.Create ;
    TASR33KB_Debugger( Result ).Memory := Self ;
end ;


function TASR33KB.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TASR33KB.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( ASR33KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( ASR33KB_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TASR33KB.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( ASR33KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( ASR33KB_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TASR33KB.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TASR33KB.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    Result := 0 ;
end ;


function TASR33KB.Get_Profiling : boolean ;

begin
    Get_Profiling := False ;
end ;


function TASR33KB.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := 0 ;
end ;


function TASR33KB.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := 0 ;
end ;


function TASR33KB.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


const _Name : PChar = 'ASR33 Keyboard'#0 ;

function TASR33KB.Name : PChar ;

begin
    Name := _Name ;
end ;


function TASR33KB.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


function TASR33KB.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ;
end ; { TASR33KB.Read }


function TASR33KB.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TASR33KB.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TASR33KB.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TASR33KB.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TASR33KB.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TASR33KB.Set_Profiling( _On, Children : boolean ) ;

begin
    // This routine left intentionally blank
end ;


procedure TASR33KB.Set_Read_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TASR33KB.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TASR33KB.Set_Write_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank - no status to show }
end ;


procedure TASR33KB.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TASR33KB.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ; // TASR33KB.Write


procedure TASR33KB.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TASR33KB.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TASR33KB.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TASR33KB.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


procedure TASR33KB.Set_Up( P : PChar ) ;

begin
end ;


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

