{$N+}
{
        Program Name : SVM_Screen
        Package Name : CEF
        Purpose      : SVM Screen component
        Institution  : 
        Date Written : 10-Dec-2014
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

	  This unit implements the user I/O for the CEF SVM CPU.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit SVM_Screen ;

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
     _UEHDefs, { TUEC }

     // CEF...
     CEF, { TMemory }
     _CEFUtil, // TCEF_Watchpoint
     Serial_Cable, // TSerial_Cable

     // SVMScreen...
     SVM_Screen_Main ; // TMain_Form

const SVMScreen_Facility = 16 ;
      SVMScreen_Success = 0 ;
      SVMScreen_Invalid_Range = 1 ;
      SVMScreen_Component_Not_Found = 2 ;
      SVMScreen_Address_Out_Of_Range = 3 ;
      SVMScreen_Invalid_Component = 4 ;
      SVMScreen_No_Matching_Watchpoint = 5 ;
      SVMScreen_Access_Violation = 6 ;

type TSVMScreen_Cable = class ;

     TSVMScreen = class( TSerial_Cable )
         private // Instance data...
            Main_Form : TMain_Form ;
            _Notifications : TList ;
            Default_Input : textfile ;

        private // Internal utility routines...
            function Set_Error( Code : integer ) : TUEC ;

        public { API... }
            _Cable : TSVMScreen_Cable ;

            function Facility_Code : longint ; override ;

            function Initialize( UI : TUI_Interface ) : TUEC ; override ;

            function Terminate : TUEC ; override ;

            function Add_Notification( Component : TComponent ) : TUEC ;
                override ;

            function Cable : TCable ; override ;

            procedure Child_Notification( Child : TComponent ;
                var Notice : longint ; var Params : int64 ) ; override ;

            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUEC ; override ;

            function Component_Type : longint ; override ;

            function Connect_Input( Component : TComponent ) : TUEC ; override ;

            function Connect_Output( Component : TComponent ) : TUEC ; override ;

            function Debugger : TDebug_Interface ; override ;

            function Delete_Notification( Component : TComponent ) : TUEC ;
                override ;

            function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                Memory : boolean ) : TUEC ; override ;

            function Examine( Address : int64 ; var Size : longint ;
                Buffer : pointer ; Memory : boolean ) : TUEC ; override ;

            function Get_Access_Mode( Address : int64 ;
                Memory : boolean ) : longint ; override ;

            function Get_Profiling : boolean ; override ;

            function Get_Read_Latency : longint ; override ;

            function Get_Write_Latency : longint ; override ;

            function Name : PChar ; override ;

            function Read( Address : int64 ; Size : longint ;
                IO_Type : longint ) : boolean ; override ;

            function Restore_Contents( Stream : TCOM_Stream ) : TUEC ; override ;

            function Restore_State( Stream : TCOM_Stream ) : TUEC ; override ;

            function Save_Contents( Stream : TCOM_Stream ) : TUEC ; override ;

            function Save_State( Stream : TCOM_Stream ) : TUEC ; override ;

            function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                Typ : longint ) : TUEC ; override ;

            procedure Set_Profiling( _On, Children : boolean ) ;  override ;

            procedure Set_Read_Latency( Value : longint ) ; override ;

            function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUEC ; override ;

            procedure Set_Write_Latency( Value : longint ) ; override ;

            procedure Show_Status ; override ;

            function Write( Address : int64 ; Value, Size : longint ;
                IO_Type : longint ) : TUEC ; override ;

            procedure Set_Up( P : PChar ) ; override ;

            procedure Signal_Change_Notice( Component : TComponent ;
                Index : longint ; Active : boolean ) ; override ; 
     end ; // TSVMScreen


     TSVMScreen_Cable = class( TSerial_Cable_Cable )
                       private // Instance data...
                           Main_Form : TMain_Form ;
                           P : array[ 0..8 ] of longint ; // Function parameters (0 = current parameter)
                           Current_SMU_Function : longint ;

                       public // API...
                            procedure Receive( Source : TComponent ; Speed : int64 ;
                                Value, Data_Size, Stop_Bits : longint ) ;
                                override ;
                   end ; // TSVMScreen_Cable

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
     ErrMastr, // Register_Error_Interface
     Num1s,
     Parse, // TString_Parser
     SMU, // SMU_Initialized

     // SVM...
     _SVM ; // SVM_SCU_Initialized


{ TSVMScreen_Debugger methods... }

type TSVMScreen_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TSVMScreen ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TSVMScreen
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TSVMScreen_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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

begin
    I := TText_Debugger.Create ;
    Result := PDebug_Interface( I ) ;
end ;


function TSVMScreen_Debugger.Count : longint ;

begin
    Result := 0 ;
end ;


{ TSVMScreen methods... }

function TSVMScreen.Set_Error( Code : integer ) : TUEC ;

begin
    Result.Facility := Facility_Code ;
    Result.Code := Code ;
end ;


{ API... }

function TSVMScreen.Facility_Code : longint ;

begin
    Facility_Code := SVMScreen_Facility ;
end ;


function TSVMScreen.Initialize( UI : TUI_Interface ) : TUEC ;

begin
    if( Main_Form = nil ) then // Not already initialized
    begin
        Result := inherited Initialize( UI ) ;

        { General setup... }
        Pending_UI := UI ;
        Application.CreateForm( TMain_Form, Main_Form ) ;
        Main_Form._SVMScreen := self ;
        Main_Form._UI := UI ;
        try
            Main_Form.Visible := True ;
        except
        end ;

        _Cable := TSVMScreen_Cable.Create ;
        _Cable.Parent := self ;
        _Cable.Main_Form := Main_Form ;

        _Notifications := TList.Create ;
    end ;
    Initialize := Set_Error( SVMScreen_Success ) ;
end ; { TSVMScreen.Initialize }


function TSVMScreen.Terminate : TUEC ;

var Loop : integer ;
    Notice : longint ;
    P : int64 ;

begin
    if( Main_Form._UI <> nil ) then
    begin
        Main_Form._UI.Termination_Notice( self ) ;
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
    for Loop := 0 to _Notifications.Count - 1 do
    begin
        Notice := Child_Notice_Terminating ;
        TComponent( _Notifications[ Loop ] ).Child_Notification( self, Notice, P ) ;
    end ;
    _Notifications.Free ;
    _Notifications := nil ;
    Main_Form.ParentWindow := 0 ;
    Main_Form.Close ;
    Main_Form := nil ;
    Result := inherited Terminate ;
end ; // TSVMScreen.Terminate


function TSVMScreen.Add_Notification( Component : TComponent ) : TUEC ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( SVMScreen_Invalid_Component ) ;
        exit ;
    end ;
    if( _Notifications.Indexof( Component ) = -1 ) then
    begin
        _Notifications.Add( Component ) ;
    end ;
end ;


function TSVMScreen.Cable : TCable ;

begin
    Result := _Cable ;
end ;


procedure TSVMScreen.Child_Notification( Child : TComponent ; var Notice : longint ;
    var Params : int64 ) ;

begin
    if( Notice = Child_Notice_Terminating ) then
    begin
        if( Child = Main_Form.Cable_Component ) then
        begin
            Disconnect_Input( Child ) ;
            Main_Form.Cable_Component := nil ;
        end ;
    end ;
end ;


function TSVMScreen.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TSVMScreen.Component_Type : longint ;

begin
    Component_Type := Component_Type_Cable ;
end ;


function TSVMScreen.Connect_Input( Component : TComponent ) : TUEC ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := Set_Error( SVMScreen_Invalid_Component ) ;
        exit ;
    end ;
    inherited Connect_Input( Component ) ;
    Connect_Input := Set_Error( SVMScreen_Success ) ;
end ;


function TSVMScreen.Connect_Output( Component : TComponent ) : TUEC ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := Set_Error( SVMScreen_Invalid_Component ) ;
        exit ;
    end ;
    inherited Connect_Output( Component ) ;
    Connect_Output := Set_Error( SVMScreen_Success ) ;
end ;


function TSVMScreen.Debugger : TDebug_Interface ;

begin
    Result := TSVMScreen_Debugger.Create ;
    TSVMScreen_Debugger( Result ).Memory := Self ;
end ;


function TSVMScreen.Delete_Notification( Component : TComponent ) : TUEC ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( SVMScreen_Invalid_Component ) ;
        exit ;
    end ;
    if( _Notifications.Indexof( Component ) = -1 ) then
    begin
        Result.Facility := Facility_Code ;
        Result.Code := ComponentErr_Component_Not_Found ;
    end else
    begin
        _Notifications.Remove( Component ) ;
    end ;
end ;


function TSVMScreen.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUEC ;

begin
    Result := Set_Error( SVMScreen_Address_Out_Of_Range ) ;
end ;


function TSVMScreen.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUEC ;

var I : longint ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( SVMScreen_Address_Out_Of_Range ) ;
        exit ;
    end ;
    fillchar( Result, sizeof( Result ), 0 ) ;
    case Address of
        SVM_SMU_Clear_Window : I := integer( Create_Window( _Cable.P[ 1 ], _Cable.P[ 2 ], _Cable.P[ 3 ] ) ) ;
        SVM_SMU_Current_Window : I := integer( Current_Window ) ;
        SVM_SMU_Get_Screen : I := Get_Screen( _Cable.P[ 1 ], _Cable.P[ 2 ] ) ;
        SVM_SMU_Window_Attribute : I := Get_Window_Attribute( _Cable.P[ 1 ], _Cable.P[ 2 ] ) ;
        SVM_SMU_Window_Char : I := Get_Window_Char( _Cable.P[ 1 ], _Cable.P[ 2 ] ) ;
        SVM_SMU_Readln_KB_Buffer :
            begin
                {$I-}
                readln( Default_Input, KB_Buffer ) ;
                {$I+}
                I := IOResult ;
            end ;
        SVM_SMU_Initialized : I := ord( SMU_Initialized ) ;
        SVM_SMU_Window_Mapped : I := ord( Window_Mapped( PWindow( _Cable.P[ 1 ] ) ) ) ;
        else exit ;
    end ;
    Size := ( Size + 7 ) div 8 ;
    move( I, Buffer^, Size ) ;
    Size := Size * 8 ;
end ;


function TSVMScreen.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    Result := 0 ;
end ;


function TSVMScreen.Get_Profiling : boolean ;

begin
    Get_Profiling := False ;
end ;


function TSVMScreen.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := 0 ;
end ;


function TSVMScreen.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := 0 ;
end ;


const _Name : PChar = 'SVMScreen'#0 ;

function TSVMScreen.Name : PChar ;

begin
    Name := _Name ;
end ;


function TSVMScreen.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ;
end ; { TSVMScreen.Read }


function TSVMScreen.Restore_Contents( Stream : TCOM_Stream ) : TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TSVMScreen.Restore_State( Stream : TCOM_Stream ) : TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TSVMScreen.Save_Contents( Stream : TCOM_Stream ): TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TSVMScreen.Save_State( Stream : TCOM_Stream ) : TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TSVMScreen.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TSVMScreen.Set_Profiling( _On, Children : boolean ) ;

begin
    // This routine left intentionally blank
end ;


procedure TSVMScreen.Set_Read_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank }
end ;


function TSVMScreen.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TSVMScreen.Set_Write_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank }
end ;


procedure TSVMScreen.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TSVMScreen.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ; // TSVMScreen.Write


procedure TSVMScreen.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

var S : string ;

begin
    _Cable.P[ 0 ] := 1 ;
    case Index of
        SVM_SMU_Clear_KB_Buffer :
            begin
                Set_KB_Buffer( Current_Window, '' ) ;
                exit ;
            end ;
        SVM_SMU_KB_Buffer :
            begin
                S := Get_KB_Buffer( Current_Window ) ;
                Component.Write_String( 0, PChar( S ), length( S ) * 8, IO_Type_IO ) ;
                exit ;
            end ;
        SVM_SMU_Update_screen :
            begin
                Update_Screen ;
                exit ;
            end ;
    end ;
    _Cable.Current_SMU_Function := Index ;
end ;


procedure TSVMScreen.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'CAPTION' ) then
        begin
            S := Parser.Token ;
            if( ( copy( S, 1, 1 ) = '"' ) or ( copy( S, 1, 1 ) = #39 ) ) then
            begin
                if( copy( S, length( S ), 1 ) = copy( S, 1, 1 ) ) then // Terminated literal
                begin
                    S := copy( S, 1, length( S ) - 1 ) ; // Trim trailing quote
                end ;
                S := copy( S, 2, length( S ) ) ;
            end ;
            Main_Form.Caption := S ;
        end else
        if( S = 'COLUMN' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form.Cursor_Column := strtoint( S ) ;
            except
            end ;
        end else
        if( S = 'ROW' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form.Cursor_Row := strtoint( S ) ;
            except
            end ;
        end ;

        // If we get here it is a unknown token - just ignore it and continue on
        S := uppercase( Parser.Token ) ; // Get next token
    end ; // while( S <> '' )
    Parser.Free ;
end ; // TSVMScreen.Set_Up


procedure TSVMScreen_Cable.Receive( Source : TComponent ; Speed : int64 ;
    Value, Data_Size, Stop_Bits : longint ) ;

begin
    if( Stop_Bits = 1 ) then // Output
    begin
        Main_Form.Receive_Char( Value ) ;
        exit ;
    end ;
    P[ P[ 0 ] ] := Value ;
    inc( P[ 0 ] ) ;
    case Current_SMU_Function of
        SVM_SMU_Clear_Cursor :
            if( P[ 0 ] = 3 ) then
            begin
                Clear_Cursor( P[ 1 ], P[ 2 ] ) ;
            end ;
        SVM_SMU_Clear_Window :
            if( P[ 0 ] = 2 ) then
            begin
                Clear_Window( PWindow( P[ 1 ] ) ) ;
            end ;
        SVM_SMU_Delete_Line :
            if( P[ 0 ] = 3 ) then
            begin
                Delete_Line( PWindow( P[ 1 ] ), P[ 2 ] ) ;
            end ;
        SVM_SMU_Delete_Window :
            if( P[ 0 ] = 2 ) then
            begin
                Delete_Window( PWindow( P[ 1 ] ) ) ;
            end ;
        SVM_SMU_Enable_SMU :
            if( P[ 0 ] = 2 ) then
            begin
                Enable_SMU( P[ 1 ] <> 0 ) ;
            end ;
        SVM_SMU_Erase_EOL :
            if( P[ 0 ] = 2 ) then
            begin
                Erase_EOL( PWindow( P[ 1 ] ) ) ;
            end ;
        SVM_SMU_Erase_EOW :
            if( P[ 0 ] = 2 ) then
            begin
                Erase_EOW( PWindow( P[ 1 ] ) ) ;
            end ;
        SVM_SMU_Init_SMU :
            if( P[ 0 ] = 5 ) then
            begin
                Init_SMU( P[ 1 ], P[ 2 ], P[ 3 ], P[ 4 ] ) ;
            end ;
        SVM_SMU_Insert_Line :
            if( P[ 0 ] = 3 ) then
            begin
                Insert_Line( PWindow( P[ 1 ] ), P[ 2 ] ) ;
            end ;
        SVM_SMU_Load_Driver :
            if( P[ 0 ] = 2 ) then
            begin
                Load_Driver( PChar( pointer( P[ 1 ] ) ) ) ;
            end ;
        SVM_SMU_Map_Window :
            if( P[ 0 ] = 9 ) then
            begin
                Map_Window( PWindow( P[ 1 ] ), P[ 2 ], P[ 3 ], P[ 4 ], P[ 5 ], P[ 6 ], P[ 7 ], P[ 8 ] ) ;
            end ;
        SVM_SMU_Refresh_Window :
            if( P[ 0 ] = 2 ) then
            begin
                Refresh_Window( PWindow( P[ 1 ] ) ) ;
            end ;
        SVM_SMU_Select_Window :
            if( P[ 0 ] = 2 ) then
            begin
                Select_Window( PWindow( P[ 1 ] ) ) ;
            end ;
        SVM_SMU_Set_Cursor :
            if( P[ 0 ] = 3 ) then
            begin
                Set_Cursor( P[ 1 ], P[ 2 ] ) ;
            end ;
        SVM_SMU_Set_Screen :
            if( P[ 0 ] = 5 ) then
            begin
                Set_Screen( P[ 1 ], P[ 2 ], P[ 3 ], P[ 4 ] ) ;
            end ;
        SVM_SMU_Set_Viewpoint :
            if( P[ 0 ] = 3 ) then
            begin
                Set_Viewport( P[ 1 ], P[ 2 ] ) ;
            end ;
        SVM_SMU_Set_Window :
            if( P[ 0 ] = 6 ) then
            begin
                Set_Window( PWindow( P[ 1 ] ), P[ 2 ], P[ 3 ], P[ 4 ], P[ 5 ] ) ;
            end ;
        SVM_SMU_Unmap_Window :
            if( P[ 0 ] = 2 ) then
            begin
                Unmap_Window( PWindow( P[ 1 ] ) ) ;
            end ;
        SVM_SMU_Update_Window :
            if( P[ 0 ] = 2 ) then
            begin
                Update_Window( PWindow( P[ 1 ] ) ) ;
            end ;
        SVM_SMU_Window_Read_Input :
            if( P[ 0 ] = 3 ) then
            begin
                Window_Read_Input( P[ 1 ], P[ 2 ] ) ;
            end ;
    end ; // case Current_SMU_Function
end ; // TSVMScreen_Cable.Receive


type tSVMScreen_EI = class( tError_Interface )
                            public
                                function Get_Facility : longint ;
                                    override ; stdcall ;
                                function Get_Name : PChar ; override ; stdcall ;
                                function Translate_Error( Code : longint ) : PChar ;
                                    override ; stdcall ;
                                procedure Done ; override ; stdcall ;
                                function Facility_Version : longint ; override ;
                                function Version : longint ; override ;
                                function Severity( Code : longint ) : longint ;
                                    override ; stdcall ;
                                function Translate_Error_Ex( Code : longint ;
                                    var Size, Typ : longint ) : PChar ;
                                    override ; stdcall ;
                  end ;
     pSVMScreen_EI = tSVMScreen_EI ;


{ tSVMScreen_EI methods... }

function tSVMScreen_EI.Get_Facility : longint ;

begin
    Get_Facility := SVMScreen_Facility ;
end ;


const SVMScreen_Facility_Name : PChar = 'SVMScreen' ;

function tSVMScreen_EI.Get_Name : PChar ;

begin
    Get_Name := SVMScreen_Facility_Name ;
end ;


var Memory_Error_Text : array[ 0..127 ] of char ;

function tSVMScreen_EI.Translate_Error( Code : longint ) : PChar ;

var _Error : string ;

begin
    case Code of
        SVMScreen_Success: _Error := 'Success' ;
        SVMScreen_Invalid_Range : _Error := 'Invalid range' ;
        SVMScreen_Component_Not_Found : _Error := 'Component not found' ;
        SVMScreen_Address_Out_Of_Range : _Error := 'Address out of range' ;
        SVMScreen_Invalid_Component : _Error := 'Invalid component' ;
        SVMScreen_No_Matching_Watchpoint : _Error := 'No Matching Watchpoint' ;
        SVMScreen_Access_Violation : _Error := 'Access Violation' ;
	else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    _Error := _Error + #0 ;
    move( _Error[ 1 ], Memory_Error_Text[ 0 ], length( _Error ) ) ;
    Translate_Error := @Memory_Error_Text ;
end ; { tFS_EI.Translate_Error }


procedure tSVMScreen_EI.Done ;

begin
    { Done by unit }
end ;


function tSVMScreen_EI.Facility_Version : longint ;

begin
    Facility_Version := 0 ;
end ;


function tSVMScreen_EI.Version : longint ;

begin
    Version := 11 ; { Compatible with UEH V1.1 }
end ;


function tSVMScreen_EI.Severity( Code : longint ) : longint ;

begin
    Severity := 2 ;
end ;


function tSVMScreen_EI.Translate_Error_Ex( Code : longint ;
    var Size, Typ : longint ) : PChar ;

begin
    Translate_Error_Ex := Translate_Error( Code ) ;
end ;


var MEI : pSVMScreen_EI ;

initialization
    { Set up the memory error interface... }
    MEI := tSVMScreen_EI.Create ;
//    Register_Error_Interface( MEI ) ;
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}

finalization
    Unregister_Error_Interface( MEI.Get_Facility ) ;
    MEI.Free ;
end.

