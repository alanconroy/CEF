{$N+}
{
        Program Name : ZX_81_KB
        Package Name : CEF
        Purpose      : Sinclair ZX81 keyboard
        Institution  : 
        Date Written : 31-Dec-2008
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

	  This unit Sinclair ZX81 keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit ZX_81_KB ;

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

     // ZX81_KB...
     ZX81KB_Main ; // TMain_Form

const ZX81KB_Facility = -1 ;
      ZX81KB_Success = 0 ;
      ZX81KB_Component_Not_Found = 1 ;
      ZX81KB_Invalid_Component = 2 ;
      ZX81KB_No_Matching_Watchpoint = 3 ;

type TZX81KB_UI = class ;
     TZX81KB_KB = class ;

     TZX81KB = class( TBase_Component )
                   private // Instance data...
                        Inputs, Outputs : TList ;
                        _Tag : longint ;
                        _Parent : TComponent ;

                   private // Internal utility routines...
                       function Default_Input : TComponent ;
                       function Default_Output : TComponent ;

                   public { Public instance data... }
                       _Serial_Number : integer ;
                       _User_Interface : TZX81KB_UI ;
                       _Keyboard : TZX81KB_KB ;

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

                       function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                           Typ : longint ) : TUnified_Exception ; override ;

                       procedure Set_Profiling( _On, Children : boolean ) ;
                           override ;

                       procedure Set_Read_Latency( Value : longint ) ; override ;

                       function Set_Watchpoint( Address : int64 ;
                           Memory : boolean ; Access : longint ) : TUnified_Exception ;
                           override ;

                       procedure Set_Write_Latency( Value : longint ) ; override ;

                       procedure Show_Status ; override ;

                       function Write( Address : int64 ; Value, Size : longint ;
                           IO_Type : longint ) : TUnified_Exception ; override ;

                       procedure Set_Tag( Value : longint ) ; override ;

                       function Get_Tag : longint ; override ;

                       function Get_Parent : TComponent ; override ;

                       procedure Set_Parent( Component : TComponent ) ; override ;

                       procedure Set_Up( P : PChar ) ; override ;
               end ;

     TZX81KB_UI = class( TBase_User_Interface )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

	private // Instance data...
            Keys : TStringList ;
            Main_Form : TMain_Form ;
            Parent : TZX81KB ;

          protected
            function Translate_Error( Code : longint ) : string ; override ;

          public // TUser_Intrerface overrides...
            function Get_Hidden : boolean ; override ;

            procedure Set_Hidden( Value : boolean ) ; override ;

            function Get_Parent_Window : THandle ; override ;

            procedure Set_Parent_Window( Value : THandle ) ; override ;

            function Optimal_Height : integer ; override ;

            function Optimal_Width : integer ; override ;
     end ;


     TZX81KB_KB = class( TBase_Keyboard )
                      private // Instance data...
                          Parent : TZX81KB ;
                          Temp_Key : string ;
                          Temp_Key_Name : string ;

                      public // TKeyboard overrides...
                          function Get_Key : PChar ; override ;

                          function Get_Key_Down( Name : PChar ) : boolean ; override ;

                          function Get_Key_Name( Index : integer ) : PChar ; override ;

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
     Sysutils, // Allocmem

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     Num1s,
     Parse ; // TString_Parser


{ TZX81KB_Debugger methods... }

type TZX81KB_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TZX81KB ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TZX81KB
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TZX81KB_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TZX81KB_Debugger.Count : longint ;

begin
    Result := 3 ;
end ;



// TZX81KB_UI methods...

constructor TZX81KB_UI.Create ;

begin
    inherited Create ;

    Keys := TStringList.Create ;

    Application.CreateForm( TMain_Form, Main_Form ) ;
    try
        Main_Form.Visible := True ;
    except
    end ;
end ;


destructor TZX81KB_UI.Destroy ;

begin
    Keys.Free ;
    Keys := nil ;
    Main_Form.Close ;
    Main_Form := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TZX81KB_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        ZX81KB_Success: _Error := 'Success' ;
        ZX81KB_Component_Not_Found : _Error := 'Component not found' ;
        ZX81KB_Invalid_Component : _Error := 'Invalid component' ;
        ZX81KB_No_Matching_Watchpoint : _Error := 'No Matching Watchpoint' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { TZX81KB_UI.Translate_Error }


// API...

function TZX81KB_UI.Get_Hidden : boolean ;

begin
    Result := Main_Form.Visible ;
end ;


procedure TZX81KB_UI.Set_Hidden( Value : boolean ) ;

begin
    Main_Form.Visible := not Value ;
end ;


function TZX81KB_UI.Get_Parent_Window : THandle ;

begin
    Result := Main_Form.ParentWindow ;
end ;


procedure TZX81KB_UI.Set_Parent_Window( Value : THandle ) ;

begin
    Main_Form.ParentWindow := Value ;
    Main_Form.Top := 0 ;
    Main_Form.Left := 0 ;
end ;


function TZX81KB_UI.Optimal_Height : integer ;

begin
    Result := Main_Form.Height ;
end ;


function TZX81KB_UI.Optimal_Width : integer ;

begin
    Result := Main_Form.Width ;
end ;



// TZX81KB_KB methods...

function TZX81KB_KB.Get_Key : PChar ;

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


function TZX81KB_KB.Get_Key_Down( Name : PChar ) : boolean ;

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


function TZX81KB_KB.Get_Key_Name( Index : integer ) : PChar ;

begin
    Result := nil ; // Assume failure
    if( ( Index < 0 ) or ( Index > 150 ) ) then
    begin
        exit ;
    end ;
    if( ( Index < 31 ) and ( Index <> 8 ) ) then
    begin
        Index := Index + 64 ;
    end ;
    Index := ord( upcase( char( Index ) ) ) ;
    case Index of
        0 : Temp_Key_Name := 'SHIFT' ;
        1..127 : Temp_Key_Name := chr( Index ) ;
    end ;

    Result := PChar( Temp_Key_Name ) ;
end ;


procedure TZX81KB_KB.Add_Key( Key : string ; Value, Up : integer ) ;

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



// TZX81KB methods...

function TZX81KB.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TZX81KB.Default_Output : TComponent ;

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

function TZX81KB.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TZX81KB.Keyboard : TKeyboard ;

begin
    Result := _Keyboard ;
end ;


function TZX81KB.Facility_Code : longint ;

begin
    Facility_Code := ZX81KB_Facility ;
end ;


function TZX81KB.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    if( _User_Interface = nil ) then // Not already initialized
    begin
        { General setup... }
        Inputs := TList.Create ;
        Outputs := TList.Create ;
        _User_Interface := TZX81KB_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface.Main_Form._UI := UI ;
        _User_Interface.Main_Form._ZX81KB := self ;
        _Keyboard := TZX81KB_KB.Create ;
        _Keyboard.Parent := self ;
    end ;
    Initialize := _User_Interface.Set_Error( ZX81KB_Success ) ;
end ; { TZX81KB.Initialize }


function TZX81KB.Terminate : TUnified_Exception ;

begin
    if( _User_Interface.Main_Form._UI <> nil ) then
    begin
        _User_Interface.Main_Form._UI.Termination_Notice( self ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Terminate := _User_Interface.Set_Error( ZX81KB_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    _Keyboard.Free ;
    _Keyboard := nil ;
    Free ;
end ; { TZX81KB.Terminate }



function TZX81KB.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TZX81KB.Component_Type : longint ;

begin
    Component_Type := Component_Type_Keyboard ;
end ;


function TZX81KB.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( ZX81KB_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Connect_Input := _User_Interface.Set_Error( ZX81KB_Success ) ;
end ;


function TZX81KB.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( ZX81KB_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Connect_Output := _User_Interface.Set_Error( ZX81KB_Success ) ;
end ;


function TZX81KB.Debugger : TDebug_Interface ;

begin
    Result := TZX81KB_Debugger.Create ;
    TZX81KB_Debugger( Result ).Memory := Self ;
end ;


function TZX81KB.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TZX81KB.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( ZX81KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( ZX81KB_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TZX81KB.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( ZX81KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( ZX81KB_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TZX81KB.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TZX81KB.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    Result := 0 ;
end ;


function TZX81KB.Get_Profiling : boolean ;

begin
    Get_Profiling := False ;
end ;


function TZX81KB.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := 0 ;
end ;


function TZX81KB.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := 0 ;
end ;


function TZX81KB.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


const _Name : PChar = 'ZX81 Keyboard'#0 ;

function TZX81KB.Name : PChar ;

begin
    Name := _Name ;
end ;


function TZX81KB.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


function TZX81KB.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ;
end ; { TZX81KB.Read }


function TZX81KB.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TZX81KB.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TZX81KB.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TZX81KB.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TZX81KB.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TZX81KB.Set_Profiling( _On, Children : boolean ) ;

begin
    // This routine left intentionally blank
end ;


procedure TZX81KB.Set_Read_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TZX81KB.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TZX81KB.Set_Write_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank - no status to show }
end ;


procedure TZX81KB.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TZX81KB.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ; // TZX81KB.Write


procedure TZX81KB.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TZX81KB.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TZX81KB.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TZX81KB.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


procedure TZX81KB.Set_Up( P : PChar ) ;

begin
end ;


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

