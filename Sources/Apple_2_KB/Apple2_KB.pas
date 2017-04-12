{$N+}
{
        Program Name : Apple2_KB
        Package Name : CEF
        Purpose      : Apple II keyboard CEF component
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
        *    DATE        BY          REASON                         *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	  This unit implements the Apple II keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Apple2_KB ;

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
     _CEF, // TUser_Interface
     CEF, { TBase_Memory }
     _CEFUtil, // TCEF_Watchpoint

     // Apple 2 KB...
     Apple2KB_Main ; // TMain_Form

const Apple2_KB_Facility = -1 ;
      Apple2_KB_Success = 0 ;
      Apple2_KB_Component_Not_Found = 1 ;
      Apple2_KB_Invalid_Component = 2 ;

type TApple2_KB_UI = class ;
     TApple2_KB_KB = class ;

     TApple2_KB = class( TBase_Component )
                      private // Instance data...
                          Inputs, Outputs : TList ;
                          _Tag : longint ;
                          _Parent : TComponent ;

                      private // Internal utility routines...
                          function Default_Input : TComponent ;
                          function Default_Output : TComponent ;

                      public { Public instance data... }
                          _Serial_Number : integer ;
                          _User_Interface : TApple2_KB_UI ;
                          _Keyboard : TApple2_KB_KB ;

                      public // API...
                          function User_Interface : TUser_Interface ; override ;

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

     TApple2_KB_UI = class( TBase_User_Interface )
                         public // Constructors and destructors...
                             constructor Create ;
                             destructor Destroy ; override ;

                         private // Instance data...
                             Keys : TStringList ;
                             Main_Form : TMain_Form ;
                             Parent : TApple2_KB ;
                             _UI : TUI_Interface ;

                         public // TUser_Intrerface overrides...
                             function Translate_Error( Code : longint ) : string ;
                                 override ;

            function Get_Hidden : boolean ; override ;

            procedure Set_Hidden( Value : boolean ) ; override ;

            function Get_Parent_Window : THandle ; override ;

            procedure Set_Parent_Window( Value : THandle ) ; override ;

            function Optimal_Height : integer ; override ;

            function Optimal_Width : integer ; override ;
     end ; // TApple2_KB


     TApple2_KB_KB = class( TBase_Keyboard )
                         private // Instance data...
                             Parent : TApple2_KB ;
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

var _Apple2KB : TApple2_KB ;

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

{ TApple2_KB_KB_Debugger methods... }

type TApple2_KB_KB_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TApple2_KB ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TApple2_KB
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TApple2_KB_KB_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TApple2_KB_KB_Debugger.Count : longint ;

begin
    Result := 4 ;
end ;



// TApple2_KB_UI methods...

// Constructors and destructors...

constructor TApple2_KB_UI.Create ;

begin
    inherited Create ;
    
    Keys := TStringList.Create ;

    Application.CreateForm( TMain_Form, Main_Form ) ;
    Main_Form.Visible := True ;
end ;


destructor TApple2_KB_UI.Destroy ;

begin
    Keys.Free ;
    Keys := nil ;
    Main_Form.Close ;
    Main_Form := nil ;

    inherited Destroy ;
end ;


// API...

function TApple2_KB_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        Apple2_KB_Success: _Error := 'Success' ;
        Apple2_KB_Component_Not_Found : _Error := 'Component not found' ;
        Apple2_KB_Invalid_Component : _Error := 'Invalid component' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { Translate_Error }


function TApple2_KB_UI.Get_Hidden : boolean ;

begin
    Result := Main_Form.Visible ;
end ;


procedure TApple2_KB_UI.Set_Hidden( Value : boolean ) ;

begin
    Main_Form.Visible := not Value ;
end ;


function TApple2_KB_UI.Get_Parent_Window : THandle ;

begin
    Result := Main_Form.ParentWindow ;
end ;


procedure TApple2_KB_UI.Set_Parent_Window( Value : THandle ) ;

begin
    Main_Form.ParentWindow := Value ;
    Main_Form.Top := 0 ;
    Main_Form.Left := 0 ;
end ;


function TApple2_KB_UI.Optimal_Height : integer ;

begin
    Result := Main_Form.Height ;
end ;


function TApple2_KB_UI.Optimal_Width : integer ;

begin
    Result := Main_Form.Width ;
end ;



// TApple2_KB_KB methods...

function TApple2_KB_KB.Get_Key : PChar ;

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


function TApple2_KB_KB.Get_Key_Down( Name : PChar ) : boolean ;

var I, Loop : integer ;
    S : string ;

begin
    Result := False ; // Assume failure
    S := string( Name ) ;
    S := uppercase( S ) ;
    I :=Parent._User_Interface. Keys.IndexOf( S ) ;
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


function TApple2_KB_KB.Get_Key_Name( Index : integer ) : PChar ;

begin
    Result := nil ; // Assume failure
    if( ( Index < 0 ) or ( Index > 133 ) ) then
    begin
        exit ;
    end ;
    if( ( Index < 31 ) and ( Index <> 8 ) ) then
    begin
        Index := Index + 64 ;
    end ;
    Index := ord( upcase( char( Index ) ) ) ;
    case Index of
        0 : Temp_Key_Name := 'REPT' ;
        1..127 : Temp_Key_Name := chr( Index ) ;
        128 : Temp_Key_Name := 'LEFT' ;
        129 : Temp_Key_Name := 'RIGHT' ;
        130 : Temp_Key_Name := 'LEFT_SHIFT' ;
        131 : Temp_Key_Name := 'RIGHT_SHIFT' ;
        132 : Temp_Key_Name := 'CONTROL' ;
        133 : Temp_Key_Name := 'RESET' ;
        else exit ;
    end ;

    Result := PChar( Temp_Key_Name ) ;
end ;


procedure TApple2_KB_KB.Add_Key( Key : string ; Value, Up : integer ) ;

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



// TApple2_KB methods...

function TApple2_KB.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TApple2_KB.Default_Output : TComponent ;

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

function TApple2_KB.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TApple2_KB.Keyboard : TKeyboard ;

begin
    Result := _Keyboard ;
end ;


function TApple2_KB.Facility_Code : longint ;

begin
    Facility_Code := Apple2_KB_Facility ;
end ;


function TApple2_KB.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    if( _User_Interface = nil ) then
    begin
        _User_Interface := TApple2_KB_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface._UI := UI ;
        _Keyboard := TApple2_KB_KB.Create ;
        _Keyboard.Parent := self ;
        _Apple2KB := self ;
    end ;
    Initialize := _User_Interface.Set_Error( Apple2_KB_Success ) ;
end ; { TApple2_KB.Initialize }


function TApple2_KB.Terminate : TUnified_Exception ;

begin
    if( ( _User_Interface <> nil ) and ( _User_Interface._UI <> nil ) ) then
    begin
        _User_Interface._UI.Termination_Notice( self ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Terminate := _User_Interface.Set_Error( Apple2_KB_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    _Keyboard.Free ;
    _Keyboard := nil ;
    Free ;
end ; { TApple2_KB.Terminate }



function TApple2_KB.Component_Type : longint ;

begin
    Component_Type := Component_Type_Keyboard ;
end ;


function TApple2_KB.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( Apple2_KB_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Connect_Input := _User_Interface.Set_Error( Apple2_KB_Success ) ;
end ;


function TApple2_KB.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( Apple2_KB_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Connect_Output := _User_Interface.Set_Error( Apple2_KB_Success ) ;
end ;


function TApple2_KB.Debugger : TDebug_Interface ;

begin
    Result := TApple2_KB_KB_Debugger.Create ;
    TApple2_KB_KB_Debugger( Result ).Memory := Self ;
end ;


function TApple2_KB.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( Apple2_KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( Apple2_KB_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TApple2_KB.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( Apple2_KB_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( Apple2_KB_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TApple2_KB.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


const _Name : PChar = 'Apple II Keyboard'#0 ;

function TApple2_KB.Name : PChar ;

begin
    Name := _Name ;
end ;


function TApple2_KB.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


procedure TApple2_KB.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


procedure TApple2_KB.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TApple2_KB.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TApple2_KB.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TApple2_KB.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


procedure TApple2_KB.Set_Up( P : PChar ) ;

begin
end ;



initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

