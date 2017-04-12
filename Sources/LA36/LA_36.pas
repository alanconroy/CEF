{$N+}
{
        Program Name : LA_36
        Package Name : CEF
        Purpose      : DEC LA36 hard copy CEF component
        Institution  : 
        Date Written : 10-Dec-2013
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

	    This unit implements the CEF32 component emulating the DEC
        LA36 and TTY hard copy terminals.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit LA_36 ;

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
     _CEF, { TCable }
     _CEFUtil, // TCEF_Watchpoint
     Serial_Cable, // TSerial_Cable

     // LA36...
     LA36_Main ; // TMain_Form

const LA36_Facility = 127 ;
      LA36_Success = 0 ;
      LA36_Invalid_Range = 1 ;
      LA36_Component_Not_Found = 2 ;
      LA36_Address_Out_Of_Range = 3 ;
      LA36_Invalid_Component = 4 ;
      LA36_No_Matching_Watchpoint = 5 ;
      LA36_Access_Violation = 6 ;

type TLA36_Cable = class ;

     TLA36 = class( TSerial_Cable )
	    private // Instance data...
            Main_Form : TMain_Form ;
            _Notifications : TList ;

        protected
            function Translate_Error( Code : longint ) : string ; override ;

        public { API... }
            _Cable : TLA36_Cable ;

            function Facility_Code : longint ; override ;

            function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                override ;

            function Terminate : TUnified_Exception ; override ;

            function Add_Notification( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Cable : TCable ; override ;

            procedure Child_Notification( Child : TComponent ;
                var Notice : longint ; var Params : int64 ) ; override ;

            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; override ;

            function Component_Type : longint ; override ;

            function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

            function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

            function Debugger : TDebug_Interface ; override ;

            function Delete_Notification( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                Memory : boolean ) : TUnified_Exception ; override ;

            function Examine( Address : int64 ; var Size : longint ;
                Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

            function Get_Access_Mode( Address : int64 ;
                Memory : boolean ) : longint ; override ;

            function Get_Profiling : boolean ; override ;

            function Get_Read_Latency : longint ; override ;

            function Get_Write_Latency : longint ; override ;

            function Name : PChar ; override ;

            function Read( Address : int64 ; Size : longint ;
                IO_Type : longint ) : boolean ; override ;

            function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                Typ : longint ) : TUnified_Exception ; override ;

            procedure Set_Profiling( _On, Children : boolean ) ;  override ;

            procedure Set_Read_Latency( Value : longint ) ; override ;

            function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; override ;

            procedure Set_Write_Latency( Value : longint ) ; override ;

            procedure Show_Status ; override ;

            function Write( Address : int64 ; Value, Size : longint ;
                IO_Type : longint ) : TUnified_Exception ; override ;

            procedure Set_Up( P : PChar ) ; override ;
     end ; // TLA36


     TLA36_Cable = class( TSerial_Cable_Cable )
                       private // Instance data...
                           Main_Form : TMain_Form ;

                       public // API...
                            procedure Receive( Source : TComponent ; Speed : int64 ;
                                Value, Data_Size, Stop_Bits : longint ) ;
                                override ;
                   end ; // TLA36_Cable

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
     Parse, // TString_Parser
     UE, // Create_Simple_UE

     // CEF...
     CEFUtil_Int ;

{ TLA36_Debugger methods... }

type TLA36_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TLA36 ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TLA36
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TLA36_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TLA36_Debugger.Count : longint ;

begin
    Result := 0 ;
end ;


{ TLA36 methods... }

function tLA36.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        LA36_Success: _Error := 'Success' ;
        LA36_Invalid_Range : _Error := 'Invalid range' ;
        LA36_Component_Not_Found : _Error := 'Component not found' ;
        LA36_Address_Out_Of_Range : _Error := 'Address out of range' ;
        LA36_Invalid_Component : _Error := 'Invalid component' ;
        LA36_No_Matching_Watchpoint : _Error := 'No Matching Watchpoint' ;
        LA36_Access_Violation : _Error := 'Access Violation' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { tLA36.Translate_Error }


{ API... }

function TLA36.Facility_Code : longint ;

begin
    Facility_Code := LA36_Facility ;
end ;


function TLA36.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    if( Main_Form = nil ) then // Not already initialized
    begin
        Result := inherited Initialize( UI ) ;

        { General setup... }
        Pending_UI := UI ;
        Application.CreateForm( TMain_Form, Main_Form ) ;
        Main_Form._LA36 := self ;
        Main_Form._UI := UI ;
        try
            Main_Form.Visible := True ;
        except
        end ;
        Set_Signal( 'DTR', True ) ;

        _Cable := TLA36_Cable.Create ;
        _Cable.Parent := self ;
        _Cable.Main_Form := Main_Form ;

        _Notifications := TList.Create ;
    end ;
    Initialize := Set_Error( LA36_Success ) ;
end ; { TLA36.Initialize }


function TLA36.Terminate : TUnified_Exception ;

var Loop : integer ;
    Notice : longint ;
    P : int64 ;

begin
    Result := Set_Error( 0 ) ;
    if( ( Main_Form <> nil ) and ( Main_Form._UI <> nil ) ) then
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
    if( Main_Form <> nil ) then
    begin
        Main_Form.ParentWindow := 0 ;
        Main_Form.Timer.Enabled := False ;
        Main_Form.Close ;
        Main_Form := nil ;
    end ;
    Result := inherited Terminate ;
end ; // TLA36.Terminate


function TLA36.Add_Notification( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    if( Component = nil ) then
    begin
        Result := Set_Error( LA36_Invalid_Component ) ;
        exit ;
    end ;
    if( _Notifications.Indexof( Component ) = -1 ) then
    begin
        _Notifications.Add( Component ) ;
    end ;
end ;


function TLA36.Cable : TCable ;

begin
    Result := _Cable ;
end ;


procedure TLA36.Child_Notification( Child : TComponent ; var Notice : longint ;
    var Params : int64 ) ;

begin
    if( Notice = Child_Notice_Terminating ) then
    begin
        Disconnect_Input( Child ) ;
        Main_Form.Cable_Component := nil ;
    end ;
end ;


function TLA36.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLA36.Component_Type : longint ;

begin
    Component_Type := Component_Type_Cable ;
end ;


function TLA36.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := Set_Error( LA36_Invalid_Component ) ;
        exit ;
    end ;
    inherited Connect_Input( Component ) ;
    Set_Signal( 'DTR', True ) ;
    Connect_Input := Set_Error( LA36_Success ) ;
end ;


function TLA36.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := Set_Error( LA36_Invalid_Component ) ;
        exit ;
    end ;
    inherited Connect_Output( Component ) ;
    Set_Signal( 'DTR', True ) ;
    Connect_Output := Set_Error( LA36_Success ) ;
end ;


function TLA36.Debugger : TDebug_Interface ;

begin
    Result := TLA36_Debugger.Create ;
    TLA36_Debugger( Result ).Memory := Self ;
end ;


function TLA36.Delete_Notification( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    if( Component = nil ) then
    begin
        Result := Set_Error( LA36_Invalid_Component ) ;
        exit ;
    end ;
    if( _Notifications.Indexof( Component ) = -1 ) then
    begin
        Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Component_Not_Found,
            UE_Error, Translate_Error( ComponentErr_Component_Not_Found ) , '' ) ;
    end else
    begin
        _Notifications.Remove( Component ) ;
    end ;
end ;


function TLA36.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLA36.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLA36.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    Result := 0 ;
end ;


function TLA36.Get_Profiling : boolean ;

begin
    Get_Profiling := False ;
end ;


function TLA36.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := 0 ;
end ;


function TLA36.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := 0 ;
end ;


const _Name : PChar = 'LA36'#0 ;

function TLA36.Name : PChar ;

begin
    Name := _Name ;
end ;


function TLA36.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ;
end ; { TLA36.Read }


function TLA36.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLA36.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLA36.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLA36.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TLA36.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TLA36.Set_Profiling( _On, Children : boolean ) ;

begin
    // This routine left intentionally blank
end ;


procedure TLA36.Set_Read_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TLA36.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


procedure TLA36.Set_Write_Latency( Value : longint ) ;

begin
    { This routine intentionally left blank - no status to show }
end ;


procedure TLA36.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TLA36.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ; // TLA36.Write


procedure TLA36.Set_Up( P : PChar ) ;

var Dummy : integer ;
    Parser : TString_Parser ;
    S, S1 : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'ALLOW_HEREIS' ) then
        begin
            Main_Form.Here_Is_Enabled := True ;
        end else
        if( S = 'ALT_CHARACTER_SET' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'ENABLE' ) then
            begin
                Main_Form.AlternateCharacterSet1.Checked := True ;
            end else
            if( S = 'LOCK' ) then
            begin
                Main_Form.LockCharacterSet1.Checked := True ;
            end else
            if( S = 'USE' ) then
            begin
                Main_Form.UseAlternateCharacterSet1Click( nil ) ;
            end ;
        end else
        if( S = 'ALTERNATE_FONT' ) then
        begin
            Main_Form.Load_Alt_Character_Set( Parser.Token ) ;
        end else
        if( S = 'AUTOLF' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'TRANSMIT' ) then
            begin
                Main_Form.AutoLF1.Checked := True ;
            end else
            if( S = 'RECEIVE' ) then
            begin
                Main_Form.Receive1.Checked := True ;
            end ;
        end else
        if( S = 'BAUD' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'AUTO' ) then
            begin
                S := '0' ;
            end ;
            try
                Main_Form.Baud := strtoint( S ) ;
                case strtoint( S ) of
                    0 : Main_Form.Auto1.Checked := True ;
                    75 : Main_Form.N751.Checked := True ;
                    110 : Main_Form.N1101.Checked := True ;
                    150 : Main_Form.N1501.Checked := True ;
                    300 : Main_Form.N3001.Checked := True ;
                    600 : Main_Form.N6001.Checked := True ;
                    1200 : Main_Form.N12001.Checked := True ;
                    2400 : Main_Form.N24001.Checked := True ;
                    4800 : Main_Form.N48001.Checked := True ;
                    9600 : Main_Form.N96001.Checked := True ;
                end ;
            except
            end ;
        end else
        if( S = 'BLEED' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Dummy := strtoint( S ) ;
                if( Dummy >= 0 ) then
                begin
                    Main_Form.Bleed := Dummy ;
                end ;
            except
            end ;
        end else
        if( S = 'BLUEBAR' ) then
        begin
            Main_Form.Barred := True ;
            Main_Form.Bar_Color := $FFE0D7 ;
        end else
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
        if( S = 'COLUMNS' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Dummy := strtoint( S ) ;
                if( Dummy > 0 ) then
                begin
                    Main_Form.Max_Width := Dummy ;
                end ;
            except
            end ;
        end else
        if( S = 'COLUMN' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form.Print_Head_Char_Column := strtoint( S ) ;
                if( Main_Form.Character_Set <> nil ) then
                begin
                    Main_Form.Print_Head_Column :=
                        Main_Form.Print_Head_Char_Column * Main_Form.Character_Set.Width( 32 ) + Main_Form.Margin ;
                end ;
            except
            end ;
        end else
        if( S = 'COMPRESSED' ) then
        begin
            Main_Form.CompressedFont1.Checked := True ;
            Main_Form.CompressedFont1Click( Main_Form.CompressedFont1 ) ;
        end else
        if( S = 'DATA_BITS' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form.Data_Bits := strtoint( S ) ;
            except
            end ;
        end else
        if( S = 'EMULATION' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( ( S = 'TTY' ) or ( S = 'ASR33' ) ) then
            begin
                Main_Form.Set_Mode( HCM_ASR33 ) ;
            end else
            if( S = 'KSR33' ) then
            begin
                Main_Form.Set_Mode( HCM_KSR33 ) ;
            end else
            if( S = 'RO33' ) then
            begin
                Main_Form.Set_Mode( HCM_RO33 ) ;
            end else
            if( S = 'LA30' ) then
            begin
                Main_Form.Set_Mode( HCM_LA30 ) ;
            end else
            if( S = 'LA36' ) then
            begin
                Main_Form.Set_Mode( HCM_LA36 ) ;
            end ;
        end else
        if( S = 'FLOW' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'NONE' ) then
            begin
                Main_Form.None1.Checked := True ;
            end else
            if( S = 'XOFF' ) then
            begin
                Main_Form.XOnXOff1.Checked := True ;
            end else
            if( S = 'CTS' ) then
            begin
                Main_Form.CTSRTS1.Checked := True ;
            end ;
        end else
        if( S = 'FORM_CONTROL' ) then
        begin
            Main_Form.FormControlOption1.Checked := True ;
        end else
        if( S = 'FONT' ) then
        begin
            Main_Form.Load_Character_Set( Parser.Token ) ;
        end else
        if( S = 'GREENBAR' ) then
        begin
            Main_Form.Barred := True ;
            Main_Form.Bar_Color := $C7D0BB ;
        end else
        if( S = 'HEIGHT' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Dummy := trunc( int( strtofloat( S ) * 10 ) ) ;
                if( Dummy > 0 ) then
                begin
                    Main_Form.Paper_Height := Dummy ;
                end ;
            except
            end ;
        end else
        if( S = 'HEREIS' ) then
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
            Main_Form.Here_Is := S ;
        end else
        if( S = 'KEYBOARD' ) then
        begin
            S := uppercase( Parser.Token ) ;
            Main_Form.Load_Keyboard( S, True ) ;
        end else
        if( S = 'LOCAL_ECHO' ) then
        begin
            S := uppercase( Parser.Token ) ;
            Main_Form.Local1.Checked := True ;
        end else
        if( S = 'LOG' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = '/APPEND' ) then
            begin
                S := uppercase( Parser.Token ) ;
                assignfile( Main_Form.Log_File, S ) ;
                {$I-}
                System.reset( Main_Form.Log_File, 1 ) ;
                {$I-}
                if( IOResult = 0 ) then
                begin
                    {$I-}
                    seek( Main_Form.Log_File, filesize( Main_Form.Log_File ) ) ;
                    {$I-}
                end ;
            end else
            begin
                assignfile( Main_Form.Log_File, S ) ;
                {$I-}
                rewrite( Main_Form.Log_File, 1 ) ;
                {$I-}
                IOResult ;
            end ;
        end else
        if( S = 'MAGNIFICATION' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Dummy := strtoint( S ) ;
                if( Dummy >= 0 ) then
                begin
                    Main_Form.Magnification := Dummy ;
                end ;
            except
            end ;
        end else
        if( S = 'MAP' ) then
        begin
            S := uppercase( Parser.Token ) ;
            S1 := uppercase( Parser.Token ) ;
            if( S1 = '""' ) then
            begin
                S1 := '' ;
            end ;
            Main_Form.Key_Mapper.Set_Mapping( PChar( S ), PChar( S1 ) ) ;
        end else
        if( S = 'MARGIN' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form.Margin := strtoint( S ) ;
            except
            end ;
        end else
        if( S = 'MARGIN_BELL' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form._Margin_Bell := strtoint( S ) ;
            except
            end ;
        end else
        if( S = 'MAX_PAGE' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form.Max_Page := strtoint( S ) ;
            except
            end ;
        end else
        if( S = 'MIXEDCASE' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'IN' ) then
            begin
                Main_Form._Uppercase_In := False ;
            end else
            begin
                Main_Form._Uppercase_Out := False ;
            end ;
        end ;
        if( S = 'NOBAR' ) then
        begin
            Main_Form.Barred := False ;
        end else
        if( S = 'NOKEYBOARD' ) then
        begin
            if( Main_Form.Panel2.Visible ) then
            begin
                Main_Form.Keyboard1Click( nil ) ;
            end ;
        end else
        if( S = 'NOMARGIN_BELL' ) then
        begin
            Main_Form._Margin_Bell := 0 ;
        end else
        if( S = 'NOPARALLEL' ) then
        begin
            Main_Form.Threaded := False ;
        end else
        if( S = 'NOWRAP' ) then
        begin
            Main_Form._Line_Wrap := False ;
        end else
        if( S = 'OFFLINE' ) then
        begin
            Main_Form.Online1.Checked := False ;
        end else
        if( S = 'ONLINE' ) then
        begin
            Main_Form.Online1.Checked := True ;
        end else
        if( S = 'PAGE' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form.Print_Head_Page := strtoint( S ) ;
            except
            end ;
        end else
        if( S = 'PAGE_HEIGHT' ) then
        begin
            S := uppercase( Parser.Token ) ;
            Dummy := pos( '.', S + '.' ) ;
            S1 := copy( S, Dummy + 1, length( S ) ) ;
            S := copy( S, 1, Dummy - 1 ) ;
            try
                Dummy := strtoint( S ) * 10 ;
                if( S1 <> '' ) then
                begin
                    setlength( S1, 1 ) ;
                    Dummy := Dummy + strtoint( S1 ) ;
                end ;
                if( Dummy > 0 ) then
                begin
                    Main_Form.Paper_Height := Dummy ;
                end ;
            except
            end ;
        end else
        if( S = 'PAGE_WIDTH' ) then
        begin
            S := uppercase( Parser.Token ) ;
            Dummy := pos( '.', S + '.' ) ;
            S1 := copy( S, Dummy + 1, length( S ) ) ;
            S := copy( S, 1, Dummy - 1 ) ;
            try
                Dummy := strtoint( S ) * 10 ;
                if( S1 <> '' ) then
                begin
                    setlength( S1, 1 ) ;
                    Dummy := Dummy + strtoint( S1 ) ;
                end ;
                if( Dummy > 0 ) then
                begin
                    Main_Form.Paper_Width := Dummy ;
                end ;
            except
            end ;
        end else
        if( S = 'PAPER' ) then
        begin
            Main_Form.Load_Paper( Parser.Token ) ;
        end else
        if( S = 'PAPER_TAPE' ) then
        begin
            Main_Form.Load_Paper_Tape( Parser.Token ) ;
        end else
        if( S = 'PARITY' ) then
        begin
            Main_Form.Parity1.Checked := True ;
        end else
        if( ( S = 'ROW' ) or ( S = 'LINE' ) ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                Main_Form.Print_Head_Char_Line := strtoint( S ) ;
                if( Main_Form.Character_Set <> nil ) then
                begin
                    Main_Form.Print_Head_Row :=
                        Main_Form.Print_Head_Char_Line * Main_Form.Character_Set.Height + Main_Form.Margin ;
                end ;
            except
            end ;
        end else
        if( S = 'SELECTIVE_ADDRESSING' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'ENABLE' ) then
            begin
                Main_Form.Enable1.Checked := True ;
            end else
            if( S = 'SELECTED' ) then
            begin
                Main_Form.Selected1.Checked := True ;
            end else
            if( S = 'MASTER' ) then
            begin
                Main_Form.Master1.Checked := True ;
            end else
            if( S = 'UNIT' ) then
            begin
                S := uppercase( Parser.Token ) ;
                try
                    Main_Form.Unit_Code := strtoint( S ) ;
                except
                end ;
            end else
            if( S = 'GROUP' ) then
            begin
                S := uppercase( Parser.Token ) ;
                try
                    Main_Form.Group_Code := strtoint( S ) ;
                except
                end ;
            end ;
        end else
        if( S = 'STOP' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = '1' ) then
            begin
                Main_Form.N2stopbits1.Checked := False ;
            end else
            begin
                Main_Form.N2stopbits1.Checked := True ;
            end ;
        end else
        if( S = 'TABS' ) then
        begin
            Main_Form.Horizontal_Tabs.Clear ;
            S := uppercase( Parser.Token ) ;
            while( S <> ';' )  do
            begin
                if( S <> ',' ) then
                begin
                    try
                        if( Main_Form.Horizontal_Tabs.indexof( strtoint( S ) ) = -1 ) then
                        begin
                            Main_Form.Horizontal_Tabs.Add( strtoint( S ) ) ;
                        end ;
                    except
                    end ;
                end ;
                S := uppercase( Parser.Token ) ;
            end ;
            Main_Form.Horizontal_Tabs.Sort( -1 ) ;
        end else
        if( S = 'UPPERCASE' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'IN' ) then
            begin
                Main_Form._Uppercase_In := True ;
            end else
            begin
                Main_Form._Uppercase_Out := True ;
            end ;
        end else
        if( S = 'VERTICAL_TABS' ) then
        begin
            Main_Form.Vertical_Tabs.Clear ;
            S := uppercase( Parser.Token ) ;
            while( S <> ';' )  do
            begin
                if( S <> ',' ) then
                begin
                    try
                        if( Main_Form.Vertical_Tabs.indexof( strtoint( S ) ) = -1 ) then
                        begin
                            Main_Form.Vertical_Tabs.Add( strtoint( S ) ) ;
                        end ;
                    except
                    end ;
                end ;
                S := uppercase( Parser.Token ) ;
            end ;
            Main_Form.Vertical_Tabs.Sort( -1 ) ;
        end else
        if( S = 'WRAP' ) then
        begin
            Main_Form._Line_Wrap := True ;
        end ;

        // If we get here it is a unknown token - just ignore it and continue on
        S := uppercase( Parser.Token ) ; // Get next token
    end ; // while( S <> '' )
    Parser.Free ;
    if( Main_Form.Print_Head_Row > Main_Form.LPI * Main_Form.Paper_Height div 10 ) then
    begin
        Main_Form.Print_Head_Row := Main_Form.LPI * Main_Form.Paper_Height div 10 ;
    end ;
    if( Main_Form.Print_Head_Column > Main_Form.CPI * Main_Form.Paper_Width div 10 ) then
    begin
        Main_Form.Print_Head_Column := Main_Form.CPI * Main_Form.Paper_Width div 10 ;
    end ;
end ; // TLA36.Set_Up


procedure TLA36_Cable.Receive( Source : TComponent ; Speed : int64 ;
    Value, Data_Size, Stop_Bits : longint ) ;

var S : string ;

begin
    if( ( Speed <> 0 ) and ( Main_Form.Baud <> 0 ) and ( Speed <> Main_Form.Baud ) ) then
    begin
        S := Translate_Serial_Data( chr( Value ), Speed, Main_Form.Baud, Main_Form.Data_Bits, Stop_Bits ) ;
        Value := ord( S[ 1 ] ) ;
    end ;
    Main_Form.Receive_Char( Value ) ;
end ;


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

