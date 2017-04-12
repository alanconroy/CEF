{
        Program Name : TRS_Screen
        Package Name : TRS80
        Purpose      : TRS-80 Screen component for CEF
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

          This component handles the screen for the TRS80 model I, for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit trs80_Screen ;

interface

uses // Borland...
     Windows, // TRect

     Classes, // TShiftState
     Controls, // TMouseButton
     Forms, // TForm
     Graphics, // TBitmap

     // C&C...
     Collect, // TCollection
     _DebugIn, // TDebug_Interface
     _Streams, // TCOM_Stream
     TPanels, // TTransparent_Panel
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TUser_Interface
     CEF, // TBase_Component
     _CEFUtil ; // TCEF_Watchpoint

const TRS80Screen_Facility = -1 ;
const Z80ScreenErr_Success = 0 ;
const Z80ScreenErr_Invalid_Range = 1 ;
const Z80ScreenErr_Component_Not_Found = 2 ;
const Z80ScreenErr_No_Matching_Watchpoint = 3 ;
const Z80ScreenErr_Access_Violation = 4 ;
const Z80ScreenErr_Address_Out_Of_Range = 5 ;
const Z80ScreenErr_Invalid_Component = 6 ;
const Z80ScreenErr_Memory_Exhausted = 7 ;

type TZ80_Screen_UI = class ;

     TZ80_Screen = class( TBase_Component )
                       private { Instance data... }
                           Access_Mode : integer ;
                           Default_Input, Default_Output : TComponent ;
                           _Tag : longint ;
                           _Parent : TComponent ;
                           Read_Latency, Write_Latency : longint ;

                       public { Public instance data... }
                           _Serial_Number : integer ;
                           _User_Interface : TZ80_Screen_UI ;

                       public // API...
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

                           function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                               Memory : boolean ) : TUnified_Exception ; override ;

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

                           procedure Set_Read_Latency( Value : longint ) ;
                               override ;

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

                           function User_Interface : TUser_Interface ; override ;
                   end ;

     TZ80_Screen_UI = class( TBase_User_Interface )
	    public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private { Instance data... }
            _Low, _High : int64 ; { Current memory range }
            Profiling : boolean ; { True if profiling memory accesses }
            _UI : TUI_Interface ;
            Watchpoint_List : TCEF_Watchpoint_Manager ;
            Data : array[ 0..$FFFF ] of byte ;
            Our_Screen : TForm ;
            _Columns, _Rows : integer ;
            Bitmap_Image : TBitmap ;
            Caption : string ;
            Top, Left, Height, Width : integer ; // Saved metrics
            Parent : TZ80_Screen ;

        private // Internal utility routines...
            procedure CB_Paint( Sender : Tobject ) ;
            procedure CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
                Shift : TShiftState ; X, Y : Integer ) ;
            procedure CB_Toggle_Embed( Sender : TObject ) ;
	        procedure Clear_Memory ;
            function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

        protected
            function Translate_Error( Code : longint ) : string ; override ;

        public { API... }
            function Facility_Code : longint ; override ;
            function Get_Hidden : boolean ; override ;
            procedure Set_Hidden( Value : boolean ) ; override ;

            function Get_Parent_Window : THandle ; override ;
            procedure Set_Parent_Window( Value : THandle ) ; override ;

            function Get_Caption : PChar ; override ;
            procedure Set_Caption( Value : PChar ) ; override ;

            procedure Set_Size( Height, Width : integer ) ; override ;
     end ; // TZ80_Screen

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
     Menus, // TPopupMenu
     SysUtils, // strtoint
     
     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     DebugInt, // TText_Debugger
     Num1s, // Num1
     Parse ; // TString_Parser

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ; external 'CEF_Util.dll' ;

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



{ TZ80_Screen_Debugger methods... }

type TZ80_Screen_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TZ80_Screen ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TZ80_Screen
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TZ80_Screen_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
        0 : I.Title := PChar( 'Access_Mode = ' + Access_Mode_To_String( Memory.Access_Mode ) ) ;
        1 : begin
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
        2 : begin
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
        3 : I.Title := PChar( '_High = ' + Num1( Memory._User_Interface._High ) ) ;
        4 : I.Title := PChar( '_Low = ' + Num1( Memory._User_Interface._Low ) ) ;
        5 : I.Title := PChar( 'Profiling = ' + Boolean_To_String( Memory._User_Interface.Profiling ) ) ;
        6 : I.Title := PChar( 'Read_Latency = ' + Num1( Memory.Read_Latency ) ) ;
        7 : I.Title := PChar( '_Serial_Number = ' + Num1( Memory._Serial_Number ) ) ;
        8 : begin
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
        9 : I.Title := Pchar( 'Watchpoint_List = ' + Pointer_To_String( pointer( Memory._User_Interface.Watchpoint_List ) ) ) ;
        10 : I.Title := PChar( 'Write_Latency = ' + Num1( Memory.Write_Latency ) ) ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TZ80_Screen_Debugger.Count : longint ;

begin
    Result := 11 ;
end ;


// TZ80_Screen_UI methods...

// Constructors and destructors...

constructor TZ80_Screen_UI.Create ;

var Panel : TTransparent_Panel ;

begin
    inherited Create ;

    Watchpoint_List := Get_Watchpoint_Manager ;

    { Allow us to cover the entire memory range... }
    _Low := 0 ;
    fillchar( _High, sizeof( _High ), 127 ) ;
    fillchar( _High, sizeof( _High ) - 1, 255 ) ;

    _Columns := 80 ;
    _Rows := 25 ;

    Our_Screen := TForm.Create( Application ) ;
    Panel := TTransparent_Panel.Create( Our_Screen ) ;
    Panel.Parent := Our_Screen ;
    Panel.Align := alClient ;
    Panel.Canvas.Brush.Color := clBlack ;
    Panel.Canvas.Pen.Color := clWhite ;
    Panel.Color := clBlack ;
    Panel.Font.Color := clWhite ;
    Panel.OnPaint := CB_Paint ;
    Panel.OnMouseDown := CB_MouseDown ;
    Our_Screen.Caption := 'Screen' ;
    Caption := 'Screen' ;
    Our_Screen.BorderIcons := [] ;
    Our_Screen.Visible := True ;
    Profiling := False ;
end ;


destructor TZ80_Screen_UI.Destroy ;

begin
    Clear_Memory ;
    Watchpoint_List.Terminate ;
    Watchpoint_List := nil ;
    Our_Screen.Close ;
    Our_Screen := nil ;

    inherited Destroy ;
end ;



// TZ80_Screen methods...

function TZ80_Screen_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        Z80ScreenErr_Success: _Error := 'Success' ;
        Z80ScreenErr_Invalid_Range: _Error := 'Invalid range' ;
        Z80ScreenErr_Component_Not_Found: _Error := 'Component not found' ;
        Z80ScreenErr_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        Z80ScreenErr_Access_Violation: _Error := 'Access violation' ;
        Z80ScreenErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        Z80ScreenErr_Invalid_Component: _Error := 'Invalid component' ;
        Z80ScreenErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { TZ80_Screen_UI.Translate_Error }


procedure TZ80_Screen_UI.CB_Paint( Sender : Tobject ) ;

var C, R : integer ; // Current position
    P : TTransparent_Panel ;
    Rect : TRect ;
    S : string ;
    X, Y : integer ; // Cell width and height

begin
    // Create and size bitmap...
    if( Bitmap_Image = nil ) then
    begin
        Bitmap_Image := TBitmap.Create ;
        Bitmap_Image.Canvas.Brush.Color := clBlack ;
        Bitmap_Image.Canvas.Font.Color := clWhite ;
    end ;
    Y := Bitmap_Image.Canvas.TextHeight( 'W' ) ;
    Bitmap_Image.Height := Y * _Rows ;
    X := Bitmap_Image.Canvas.TextWidth( 'W' ) ;
    Bitmap_Image.Width := X * _Columns ;

    // Write data to bitmap...
    fillchar( Rect, sizeof( Rect ), 0 ) ;
    Rect.Bottom := Bitmap_Image.Height ;
    Rect.Right := Bitmap_Image.Width ;
    Bitmap_Image.Canvas.FillRect( Rect ) ;
    for R := 1 to _Rows do
    begin
        for C := 1 to _Columns do
        begin
            S := chr( Data[ _Low + ( R - 1 ) * C + C - 1 ] ) ;
            SetBkMode( Bitmap_Image.Canvas.Handle, OPAQUE ) ;
            Bitmap_Image.Canvas.TextOut( ( C - 1 ) * X, ( R - 1 ) * Y, S ) ;
        end ;
    end ;

    // Draw on window...
    P := TTransparent_Panel( Our_Screen.Controls[ 0 ] ) ;
    P.Canvas.StretchDraw( P.ClientRect, Bitmap_Image ) ;
end ; // TZ80_Screen.CB_Paint


var Pop_Up : TPopupMenu = nil ;

procedure TZ80_Screen_UI.CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
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


procedure TZ80_Screen_UI.CB_Toggle_Embed( Sender : TObject ) ;

begin
    _UI.Toggle_Embed( Parent ) ;
end ;


procedure TZ80_Screen_UI.Clear_Memory ;

begin
    fillchar( Data, sizeof( Data ), 0 ) ;
end ; { TZ80_Screen.Clear_Memory }


function TZ80_Screen_UI.Facility_Code : longint ;

begin
    Result := Parent.Facility_Code ;
end ;


function TZ80_Screen_UI.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_List.Watchpoint_At( Address ) ;
end ;


function TZ80_Screen_UI.Get_Hidden : boolean ;

begin
    Result := not Our_Screen.Visible ;
end ;


procedure TZ80_Screen_UI.Set_Hidden( Value : boolean ) ;

begin
    Our_Screen.Visible := not Value ;
end ;


function TZ80_Screen_UI.Get_Parent_Window : THandle ;

begin
    Result := Our_Screen.ParentWindow ;
end ;


procedure TZ80_Screen_UI.Set_Parent_Window( Value : THandle ) ;

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


function TZ80_Screen_UI.Get_Caption : PChar ;

begin
    Result := PChar( Caption ) ;
end ;


procedure TZ80_Screen_UI.Set_Caption( Value : PChar ) ;

begin
    Caption := string( Value ) ;
end ;


procedure TZ80_Screen_UI.Set_Size( Height, Width : integer ) ;

begin
    Our_Screen.Height := Height + GetSystemMetrics( SM_CXBORDER ) + GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Width := Width + GetSystemMetrics( SM_CXBORDER ) * 2 ;
    Our_Screen.Top := -GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Left := -GetSystemMetrics( SM_CXBORDER ) ;
end ;



// TZ80_Screen methods...

// API...

function TZ80_Screen.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TZ80_Screen.Facility_Code : longint ;

begin
    Facility_Code := TRS80Screen_Facility ;
end ;


function TZ80_Screen.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    if( _User_Interface = nil ) then
    begin
        { General setup... }
        Read_Latency := 0 ; { Infinitely fast memory :) }
        Write_Latency := 0 ;
        Default_Input := nil ;
        Default_Output := nil ;
        _User_Interface := TZ80_Screen_UI.Create ;
        _User_Interface.Parent := self ;
        _User_Interface._UI := UI ;
        Access_Mode := Access_RW or Access_Execute ;
    end ;

    Initialize := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
end ; { TZ80_Screen.Initialize }


function TZ80_Screen.Terminate : TUnified_Exception ;

begin
    if( _User_Interface._UI <> nil ) then
    begin
        _User_Interface._UI.Termination_Notice( self ) ;
    end ;
    Terminate := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    Free ;
end ; { TZ80_Screen.Terminate }



function TZ80_Screen.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Clear_Watchpoint := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
    if( Memory and ( _User_Interface._Low <= Address ) and ( _User_Interface._High >= Address ) ) then
    begin
        Result := _User_Interface.Watchpoint_List.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( Z80ScreenErr_No_Matching_Watchpoint ) ;
    end ;
end ;


function TZ80_Screen.Component_Type : longint ;

begin
    Component_Type := Component_Type_UI ;
end ;


function TZ80_Screen.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( Z80ScreenErr_Invalid_Component ) ;
        exit ;
    end ;
    Default_Input := Component ;
    Connect_Input := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
end ;


function TZ80_Screen.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( Z80ScreenErr_Invalid_Component ) ;
        exit ;
    end ;
    Default_Output := Component ;
    Connect_Output := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
end ;


function TZ80_Screen.Debugger : TDebug_Interface ;

begin
    Result := TZ80_Screen_Debugger.Create ;
    TZ80_Screen_Debugger( Result ).Memory := Self ;
end ;


function TZ80_Screen.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count : integer ;

begin
    if( ( not Memory ) or ( _User_Interface._Low > Address ) or ( _User_Interface._High < Address ) ) then
    begin
        Count := 0 ;
    end else
    begin
        Count := ( Size + 7 ) div 8 ;
    end ;
    _Buffer := PChar( Buffer ) ;

    while( Count > 0 ) do
    begin
        _User_Interface.Data[ Address ] := ord( _Buffer[ 0 ] ) ;
        _Buffer := _Buffer + 1 ;
        dec( Count ) ;
        inc( Address ) ;
    end ; { while( Count > 0 ) }
    Deposit := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
end ; { TZ80_Screen.Deposit }


function TZ80_Screen.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component <> Default_Input ) or ( Component = nil ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( Z80ScreenErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
	Default_Input := nil ;
    end ;
end ;


function TZ80_Screen.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component <> Default_Output ) or ( Component = nil ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( Z80ScreenErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
	Default_Output := nil ;
    end ;
end ;


function TZ80_Screen.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count : integer ;

begin
    if( ( not Memory ) or ( _User_Interface._Low > Address ) or ( _User_Interface._High < Address ) ) then
    begin
        Examine := _User_Interface.Set_Error( Z80ScreenErr_Address_Out_Of_Range ) ;
        exit ;
    end ;
    Count := ( Size + 7 ) div 8 ; { Number of bytes to examine }
    fillchar( Buffer^, Count, 255 ) ;
    _Buffer := Buffer ;
    try
        while( Count > 0 ) do
        begin
            _Buffer[ 0 ] := char( _User_Interface.Data[ Address ] ) ;
            _Buffer := _Buffer + 1 ;
            dec( Count ) ; { One less byte to read }
            inc( Address ) ;
        end ; { while( Count > 0 ) }
    except
    end ;
    Examine := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
end ; { TZ80_Screen.Examine }


function TZ80_Screen.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    if( Memory and ( _User_Interface._Low <= Address ) and ( _User_Interface._High >= Address ) ) then
    begin
        Get_Access_Mode := Access_Mode ;
    end else
    begin
        Get_Access_Mode := Access_None ;
    end ;
end ;


function TZ80_Screen.Get_Profiling : boolean ;

begin
    Get_Profiling := _User_Interface.Profiling ;
end ;


function TZ80_Screen.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := Read_Latency ;
end ;


function TZ80_Screen.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := Write_Latency ;
end ;


function TZ80_Screen.Input_Component( Index : longint ) : TComponent ;

begin
    if( Index = 0 ) then
    begin
	Input_Component := Default_Input ;
    end else
    begin
	Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'TRS80 Screen'#0 ;

function TZ80_Screen.Name : PChar ;

begin
    Name := _Name ;
end ;


function TZ80_Screen.Output_Component( Index : longint ) : TComponent ;

begin
    if( Index = 0 ) then
    begin
	Output_Component := Default_Output ;
    end else
    begin
	Output_Component := nil ;
    end ;
end ;


function TZ80_Screen.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

var Buffer : PChar ;
    Count : integer ;
    Watch : TCEF_Watchpoint ;

begin
    if( ( Address >= _User_Interface._Low ) and ( Address <= _User_Interface._High ) and ( IO_Type = IO_Type_Memory ) ) then
    begin
        if( ( Access_Mode and Access_Read ) = 0 ) then
        begin
            Read := False ; { Z80ScreenErr_Access_Violation }
            exit ;
        end ;
	Read := True ;
	if( _User_Interface._UI.Clock <> nil ) then
        begin
            _User_Interface._UI.Clock.Block( self, Read_Latency ) ;
        end ;
        if( Size = 0 ) then
        begin
            Count := 1 ;
        end else
        begin
            Count := ( Size + 7 ) div 8 ;
        end ;
	if( Default_Output <> nil ) then
	begin
	    getmem( Buffer, Count ) ;
	    Examine( Address, Size, Buffer, True ) ;
	    Default_Output.Write_String( Address, Buffer, Size, IO_Type_Memory ) ;
	    freemem( Buffer, Count ) ;
	end ;
	if( Default_Input <> nil ) then
	begin
	    getmem( Buffer, Count ) ;
	    Examine( Address, Size, Buffer, True ) ;
	    Default_Input.Write_String( Address, Buffer, Size, IO_Type_Memory ) ;
	    freemem( Buffer, Count ) ;
	end ;
        Watch := _User_Interface.Watchpoint_At( Address ) ;
        if( Watch <> nil ) then
        begin
            if( ( Watch.Access and Access_Read ) <> 0 ) then
            begin
                _User_Interface._UI.Watchpoint_Notice( Address, Access_Read, 0, TComponent( self ), True, False, False ) ;
            end ;
        end ;
	if( _User_Interface.Profiling ) then
        begin
             //~~~
        end ;
    end else
    begin
	Read := False ;
    end ;
end ; { TZ80_Screen.Read }


function TZ80_Screen.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

var Loop, High : int64 ;
    S : longint ;
    Value : byte ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    _User_Interface.Clear_Memory ;
    S := sizeof( Loop ) ;
    Stream.Read( Loop, S ) ;
    Stream.Read( High, S ) ;
    S := 1 ;
    while( Loop <= High ) do
    begin
	Stream.Read( Value, S ) ;
        if( S = 0 ) then
        begin
            exit ;
        end ;
	Deposit( Loop, 1, @Value, True ) ;
	Loop := Loop + 1 ;
    end ;
end ;


function TZ80_Screen.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var S : longint ;

begin
    Result := Set_Error( 0 ) ;
    S := sizeof( _User_Interface._Low ) ;
    Stream.Read( _User_Interface._Low, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    Stream.Read( _User_Interface._High, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    S := sizeof( Read_Latency ) ;
    Stream.Read( Read_Latency, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
     Stream.Read( Write_Latency, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    S := sizeof( _User_Interface.Profiling ) ;
    Stream.Read( _User_Interface.Profiling, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
end ;


function TZ80_Screen.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

var Loop : int64 ;
    High : int64 ;
    S : longint ;
    Value : integer ;

begin { TZ80_Screen.Save_Contents }
    Result := Set_Error( 0 ) ;

    { Write contents... }
    Loop := 0 ;
    Stream.Write( Loop, sizeof( Loop ) ) ;
    Stream.Set_Last_Error( Result ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( High, sizeof( High ) ) ;
    Stream.Set_Last_Error( Result ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    S := 1 ;
    while( Loop <= High ) do
    begin
        Examine( Loop, S, @Value, True ) ;
	    Stream.Write( Value, 1 ) ;
        Stream.Set_Last_Error( Result ) ;
        if( Result <> nil ) then
        begin
            exit ;
        end ;
	    Loop := Loop + 1 ;
    end ;
end ; { TZ80_Screen.Save_Contents }


function TZ80_Screen.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    Stream.Write( _User_Interface._Low, sizeof( _User_Interface._Low ) ) ;
    Stream.Set_Last_Error( Result ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( _User_Interface._High, sizeof( _User_Interface._High ) ) ;
    Stream.Set_Last_Error( Result ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( Read_Latency, sizeof( Read_Latency ) ) ;
    Stream.Set_Last_Error( Result ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( Write_Latency, sizeof( Write_Latency ) ) ;
    Stream.Set_Last_Error( Result ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( _User_Interface.Profiling, sizeof( _User_Interface.Profiling ) ) ;
    Stream.Set_Last_Error( Result ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
end ;


function TZ80_Screen.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    if( Memory and ( _User_Interface._Low <= Low ) and ( _User_Interface._High >= High ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
end ;


procedure TZ80_Screen.Set_Profiling( _On, Children : boolean ) ;

begin
    _User_Interface.Profiling := _On ;
end ;


procedure TZ80_Screen.Set_Read_Latency( Value : longint ) ;

begin
    Read_Latency := Value ;
end ;


function TZ80_Screen.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Watchpoint := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
    if( Memory and ( _User_Interface._Low <= Address ) and ( _User_Interface._High >= Address ) ) then
    begin
        _User_Interface.Watchpoint_List.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TZ80_Screen.Set_Write_Latency( Value : longint ) ;

begin
    Write_Latency := Value ;
end ;


procedure TZ80_Screen.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TZ80_Screen.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var Watch : TCEF_Watchpoint ;

begin
    if(
        ( Address >= _User_Interface._Low )
        and
        ( Address <= _User_Interface._High )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Write ) = 0 ) then
        begin
            Write := _User_Interface.Set_Error( Z80ScreenErr_Access_Violation ) ;
            exit ;
        end ;
        if( _User_Interface.Profiling ) then
        begin
            //~~~
        end ;
        Watch := _User_Interface.Watchpoint_At( Address ) ;
        if( Watch <> nil ) then
        begin
            if( ( Watch.Access and Access_Write ) <> 0 ) then
            begin
                _User_Interface._UI.Watchpoint_Notice( Address, Access_Write, 0, TComponent( self ), True, False, False ) ;
            end ;
        end ;
	if( _User_Interface._UI.Clock <> nil ) then
        begin
            _User_Interface._UI.Clock.Block( self, Write_Latency ) ;
        end ;
	Write := Deposit( Address, Size, @Value, True ) ;
        _User_Interface.Our_Screen.Repaint ;
    end else
    begin
	Write := _User_Interface.Set_Error( Z80ScreenErr_Success ) ;
    end ;
end ; // TZ80_Screen.Write


procedure TZ80_Screen.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TZ80_Screen.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TZ80_Screen.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TZ80_Screen.Set_Parent( Component : TComponent ) ;

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


procedure TZ80_Screen.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S, S1 : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'START' ) then
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
            _User_Interface._Low := Convert_Value( S ) ;
        end else
        if( S = 'ROWS' ) then
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
            _User_Interface._Rows := Convert_Value( S ) ;
        end else
        if( S = 'COLUMNS' ) then
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
            _User_Interface._Columns := Convert_Value( S ) ;
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
    _User_Interface._High := _User_Interface._Low + _User_Interface._Rows * _User_Interface._Columns ;
    Parser.Free ;
end ; // TZ80_Screen.Set_Up


{$IFDEF Test}

{ All following code is for testing the TRS80 screen class... }

type TTest_Clock = class( TMaster_Clock )
                       procedure Block( Component : TComponent ;
                           Time_Delta : int64 ) ; override ;

                       procedure Initialize( UI : TUI_Interface ) ; override ;

                       function Get_Time_Index : int64 ; override ;

                       function Version : integer ; override ;
                   end ;

var Test_Clock : TTest_Clock ;

type TTest_UI = class( TUI_Interface )
                   private
                       Profile_Address : int64 ;
                       Profile_Access : integer ;
                       Watch_Address : int64 ;
                       Watch_Access : integer ;

                   public
                       Profile_Triggered : boolean ;
                       Watch_Triggered : boolean ;

                   public
                       procedure Block( Component : TComponent ; Blocked : boolean ) ;
                           override ;

                       function Breakpoint_Notice( Address : int64 ; Physical : boolean ;
                           CPU : TCPU ) : boolean ; override ;

                       function Clock : TMaster_Clock ; override ;

                       procedure Log_Error( Text : PChar ; Severity : longint ) ;
                           override ;

                       procedure Log_Simulated_Error( Text : PChar ;
                           Severity : longint ) ; override ;

                       procedure Log_Status( Text : PChar ; Index : longint ) ;
                           override ;

                       procedure Profile_Notice( Address : int64 ; Access : longint ;
                           Tag, Time : longint ; Component : TComponent ) ; override ;

                       function Version : integer ; override ;

                       procedure Watchpoint_Notice( Address : int64 ;
                           Access, Tag : longint ; Component : TComponent ) ;
                           override ;

                           
                       procedure Allow_Profile( Address : int64 ; Access : integer ) ;

                       procedure Allow_Watch( Address : int64 ; Access : integer ) ;
                end ;

    TTest_Output_Component = class( TComponent )
                              public
                                Enabled : boolean ;
                                _Address : int64 ;
                                _Value : longint ;
                                _Size : longint ;

                              public
                                function Facility_Code : longint ; override ;

                                function Initialize( UI : TUI_Interface ) : TUnified_Exception ;  override ;

                                function Terminate : TUnified_Exception ; override ;

                                function Child_Component( Index : longint ) : TComponent ; override ;

                                function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                function Component_Type : longint ; override ;

                                function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                                function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Disconnect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Disconnect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                                function Examine( Address : int64 ; var Size : longint ;
                                    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

                                function Get_Access_Mode( Address : int64 ;
                                    Memory : boolean ) : longint ; override ;

                                function Get_Profiling : boolean ; override ;

                                function Get_Read_Latency : longint ; override ;

                                function Get_Write_Latency : longint ; override ;

                                function Input_Component( Index : longint ) : TComponent ; override ;

                                function Name : PChar ; override ;

                                function Output_Component( Index : longint ) : TComponent ; override ;

                                function Read( Address : int64 ; Size : longint ;
                                    Memory : boolean ) : boolean ; override ;

                                function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Set_Access_Mode( Low, High : int64 ;
                                    Memory : boolean ; Typ : longint ) : TUnified_Exception ;
                                    override ;

                                procedure Set_Profiling( _On, Children : boolean ) ; override ;

                                procedure Set_Read_Latency( Value : longint ) ; override ;

                                function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                procedure Set_Write_Latency( Value : longint ) ;  override ;

                                procedure Show_Status ; override ;

                                function Version : integer ; override ;

                                procedure Wake ; override ;

                                function Write( Address : int64 ; Value, Size : longint ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; Memory : boolean ) : TUnified_Exception ; override ;
                             end ;

    TTest_Input_Component = class( TComponent )
                              public
                                Enabled : boolean ;
                                _Address : int64 ;
                                _Value : longint ;
                                _Size : longint ;

                              public
                                function Facility_Code : longint ; override ;

                                function Initialize( UI : TUI_Interface ) : TUnified_Exception ;  override ;

                                function Terminate : TUnified_Exception ; override ;

                                function Child_Component( Index : longint ) : TComponent ; override ;

                                function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                function Component_Type : longint ; override ;

                                function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                                function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Disconnect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Disconnect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                                function Examine( Address : int64 ; var Size : longint ;
                                    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

                                function Get_Access_Mode( Address : int64 ;
                                    Memory : boolean ) : longint ; override ;

                                function Get_Profiling : boolean ; override ;

                                function Get_Read_Latency : longint ; override ;

                                function Get_Write_Latency : longint ; override ;

                                function Input_Component( Index : longint ) : TComponent ; override ;

                                function Name : PChar ; override ;

                                function Output_Component( Index : longint ) : TComponent ; override ;

                                function Read( Address : int64 ; Size : longint ;
                                    Memory : boolean ) : boolean ; override ;

                                function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Set_Access_Mode( Low, High : int64 ;
                                    Memory : boolean ; Typ : longint ) : TUnified_Exception ;
                                    override ;

                                procedure Set_Profiling( _On, Children : boolean ) ; override ;

                                procedure Set_Read_Latency( Value : longint ) ; override ;

                                function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                procedure Set_Write_Latency( Value : longint ) ;  override ;

                                procedure Show_Status ; override ;

                                function Version : integer ; override ;

                                procedure Wake ; override ;

                                function Write( Address : int64 ; Value, Size : longint ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; Memory : boolean ) : TUnified_Exception ; override ;
                             end ;

procedure TTest_UI.Block( Component : TComponent ; Blocked : boolean ) ;

begin
end ;


function TTest_UI.Breakpoint_Notice( Address : int64 ; Physical : boolean ;
   CPU : TCPU ) : boolean ;

begin
    ShowMessage( 'Breakpoint notice' ) ;
end ;


function TTest_UI.Clock : TMaster_Clock ;

begin
    Result := Test_Clock ;
end ;


procedure TTest_UI.Log_Error( Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TTest_UI.Log_Simulated_Error( Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TTest_UI.Log_Status( Text : PChar ; Index : longint ) ;

begin
end ;


procedure TTest_UI.Profile_Notice( Address : int64 ; Access : longint ;
   Tag, Time : longint ; Component : TComponent ) ;

begin
    if( ( Address <> Profile_Address ) or ( Access <> Profile_Access ) ) then
    begin
        ShowMessage( 'Invalid profile notice' ) ;
    end ;
    Profile_Triggered := True ;
end ;


function TTest_UI.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_UI.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
   Component : TComponent ) ;

begin
    if( ( Address <> Watch_Address ) or ( Access <> Watch_Access ) ) then
    begin
        ShowMessage( 'Invalid watchpoint notice' ) ;
    end ;
    Watch_Triggered := True ;
end ;


procedure TTest_UI.Allow_Profile( Address : int64 ; Access : integer ) ;

begin
    Profile_Address := Address ;
    Profile_Access := Access ;
end ;


procedure TTest_UI.Allow_Watch( Address : int64 ; Access : integer ) ;

begin
    Watch_Address := Address ;
    Watch_Access := Access ;
end ;


procedure TTest_Clock.Block( Component : TComponent ; Time_Delta : int64 ) ;

begin
end ;


procedure TTest_Clock.Initialize( UI : TUI_Interface ) ;

begin
end ;


function TTest_Clock.Get_Time_Index : int64 ;

begin
    Result := 0 ;
end ;


function TTest_Clock.Version : integer ;

begin
    Result := 0 ;
end ;



function TTest_Output_Component.Facility_Code : longint ;

begin
    Result := 0 ;
end ;


function TTest_Output_Component.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Terminate : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Child_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Child_Component' ) ;
end ;


function TTest_Output_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Clear_Watch' ) ;
end ;


function TTest_Output_Component.Component_Type : longint ;

begin
    ShowMessage( 'Call to component.Component_Type' ) ;
end ;


function TTest_Output_Component.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Input' ) ;
end ;


function TTest_Output_Component.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Output' ) ;
end ;


function TTest_Output_Component.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Deposit' ) ;
end ;


function TTest_Output_Component.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Input' ) ;
end ;


function TTest_Output_Component.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Output' ) ;
end ;


function TTest_Output_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Examine' ) ;
end ;


function TTest_Output_Component.Get_Access_Mode( Address : int64 ;
    Memory : boolean ) : longint ;

begin
    ShowMessage( 'Call to component.Get_Access_Mode' ) ;
end ;


function TTest_Output_Component.Get_Profiling : boolean ;

begin
    ShowMessage( 'Call to component.Get_Profiling' ) ;
end ;


function TTest_Output_Component.Get_Read_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Read_Latency' ) ;
end ;


function TTest_Output_Component.Get_Write_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Write_Latency' ) ;
end ;


function TTest_Output_Component.Input_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Input_Component' ) ;
end ;


function TTest_Output_Component.Name : PChar ;

begin
    ShowMessage( 'Call to component.Name' ) ;
end ;


function TTest_Output_Component.Output_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Output_Component' ) ;
end ;


function TTest_Output_Component.Read( Address : int64 ; Size : longint ;
    Memory : boolean ) : boolean ;

begin
end ;


function TTest_Output_Component.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Set_Access_Mode( Low, High : int64 ;
    Memory : boolean ; Typ : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Access_Mode' ) ;
end ;


procedure TTest_Output_Component.Set_Profiling( _On, Children : boolean ) ;

begin
    ShowMessage( 'Call to component.Set_Profiling' ) ;
end ;


procedure TTest_Output_Component.Set_Read_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Read_Latency' ) ;
end ;


function TTest_Output_Component.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Watchpoint' ) ;
end ;


procedure TTest_Output_Component.Set_Write_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Write_Latency' ) ;
end ;


procedure TTest_Output_Component.Show_Status ;

begin
    ShowMessage( 'Call to component.Show_Status' ) ;
end ;


function TTest_Output_Component.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_Output_Component.Wake ;

begin
    ShowMessage( 'Call to component.Wake' ) ;
end ;


function TTest_Output_Component.Write( Address : int64 ; Value, Size : longint ;
    Memory : boolean ) : TUnified_Exception ;

begin
    if( Enabled and Memory ) then
    begin
        _Address := Address ;
        _Size := Size ;
        _Value := Value ;
    end else
    begin
        ShowMessage( 'Unexpected Write to output component' ) ;
    end ;
end ;


function TTest_Output_Component.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; Memory : boolean ) : TUnified_Exception ;

begin
    Write( Address, ord( Value[ 0 ] ), Size, Memory ) ;
end ;


function TTest_Input_Component.Facility_Code : longint ;

begin
    Result := 0 ;
end ;



function TTest_Input_Component.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Terminate : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Child_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Child_Component' ) ;
end ;


function TTest_Input_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Clear_Watch' ) ;
end ;


function TTest_Input_Component.Component_Type : longint ;

begin
    ShowMessage( 'Call to component.Component_Type' ) ;
end ;


function TTest_Input_Component.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Input' ) ;
end ;


function TTest_Input_Component.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Output' ) ;
end ;


function TTest_Input_Component.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Deposit' ) ;
end ;


function TTest_Input_Component.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Input' ) ;
end ;


function TTest_Input_Component.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Output' ) ;
end ;


function TTest_Input_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Examine' ) ;
end ;


function TTest_Input_Component.Get_Access_Mode( Address : int64 ;
    Memory : boolean ) : longint ;

begin
    ShowMessage( 'Call to component.Get_Access_Mode' ) ;
end ;


function TTest_Input_Component.Get_Profiling : boolean ;

begin
    ShowMessage( 'Call to component.Get_Profiling' ) ;
end ;


function TTest_Input_Component.Get_Read_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Read_Latency' ) ;
end ;


function TTest_Input_Component.Get_Write_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Write_Latency' ) ;
end ;


function TTest_Input_Component.Input_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Input_Component' ) ;
end ;


function TTest_Input_Component.Name : PChar ;

begin
    ShowMessage( 'Call to component.Name' ) ;
end ;


function TTest_Input_Component.Output_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Output_Component' ) ;
end ;


function TTest_Input_Component.Read( Address : int64 ; Size : longint ;
    Memory : boolean ) : boolean ;

begin
end ;


function TTest_Input_Component.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Set_Access_Mode( Low, High : int64 ;
    Memory : boolean ; Typ : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Access_Mode' ) ;
end ;


procedure TTest_Input_Component.Set_Profiling( _On, Children : boolean ) ;

begin
    ShowMessage( 'Call to component.Set_Profiling' ) ;
end ;


procedure TTest_Input_Component.Set_Read_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Read_Latency' ) ;
end ;


function TTest_Input_Component.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Watchpoint' ) ;
end ;


procedure TTest_Input_Component.Set_Write_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Write_Latency' ) ;
end ;


procedure TTest_Input_Component.Show_Status ;

begin
    ShowMessage( 'Call to component.Show_Status' ) ;
end ;


function TTest_Input_Component.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_Input_Component.Wake ;

begin
    ShowMessage( 'Call to component.Wake' ) ;
end ;


function TTest_Input_Component.Write( Address : int64 ; Value, Size : longint ;
    Memory : boolean ) : TUnified_Exception ;

begin
    if( Enabled and Memory ) then
    begin
        _Address := Address ;
        _Size := Size ;
        _Value := Value ;
    end else
    begin
        ShowMessage( 'Unexpected Write to output component' ) ;
    end ;
end ;


function TTest_Input_Component.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Write_String' ) ;
end ;


type TTest_Streamer = class( TCOM_Stream )
        public
            S : TMemoryStream ;

            constructor Create ;
            function At_End : boolean ; override ;
            function Facility_Code : longint ; override ;
            function Read( var Buffer ; var Size : longint ) : TUnified_Exception ;
                override ;
            function Read_Line( var Buffer ; var Size : longint ) : TUnified_Exception ;
                override ;
            function Seek( Position : longint ) : TUnified_Exception ; override ;
            function Write( var Buffer ; size : longint ) : TUnified_Exception ; override ;
            function Write_Line( Buffer : PChar ) : TUnified_Exception ; override ;
      end ;

constructor TTest_Streamer.Create ;

begin
    inherited Create ;

    S := TMemoryStream.Create ;
end ;


function TTest_Streamer.At_End : boolean ;

begin
    Result := ( S.Position >= S.Size ) ;
end ;


function TTest_Streamer.Facility_Code : longint ;

begin
    Result := -1 ;
end ;


function TTest_Streamer.Read( var Buffer ; var Size : longint ) : TUnified_Exception ;

begin
    S.ReadBuffer( Buffer, Size ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Read_Line( var Buffer ; var Size : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to Read_Line' ) ;
end ;


function TTest_Streamer.Seek( Position : longint ) : TUnified_Exception ;

begin
    S.Seek( 0, soFromBeginning ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Write( var Buffer ; size : longint ) : TUnified_Exception ;

begin
    S.WriteBuffer( Buffer, Size ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Write_Line( Buffer : PChar ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to Write_Line' ) ;
end ;


procedure Test_Unit ;

var Address, Count, Temp : int64 ;
    E : TUnified_Exception ;
    Test : TZ80_Screen ;
    Test_UI : TTest_UI ;
    Test_Output_Component : TTest_Output_Component ;
    Test_Input_Component : TTest_Input_Component ;
    Test_Stream : TTest_Streamer ;

    procedure Check_E ;

    begin
        if( E.Code <> 0 ) then
        begin
            ShowMessage( 'Error: ' + inttostr( E.Code ) ) ;
        end ;
    end ;

var Dummy, Size, Work : integer ;

begin
    { Coverage test... }

    { Setup... }
    Test_Clock := TTest_Clock.Create ;
    Test := TZ80_Screen.Create ;
    Test_UI := TTest_UI.Create ;
    Test_UI.Allow_Profile( -1, 0 ) ;
    Test_Output_Component := TTest_Output_Component.Create ;
    Test_Output_Component.Enabled := False ;
    Test_Input_Component := TTest_Input_Component.Create ;
    Test_Input_Component.Enabled := False ;
    Test_Stream := TTest_Streamer.Create ;
    E := Test.Initialize( Test_UI ) ;
    Check_E ;

    { Check for invalid conditions... }
    if( Test.Output_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Input_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;
    E := Test.Connect_Output( Test_Output_Component ) ;
    Check_E ;
    E := Test.Connect_Input( Test_Input_Component ) ;
    Check_E ;
    E := Test.Deposit( 0, 8, @Dummy, False ) ;
    Check_E ;
    Size := 8 ;
    E := Test.Examine( 0, Size, @Work, False ) ;
    if( E.COde = 0 ) then
    begin
        ShowMessage( 'No error on non-memory' ) ;
    end ;
    Work := 0 ;
    E := Test.Examine( 0, Size, @Work, True ) ;
    Check_E ;
    if( Work <> 255 ) then
    begin
        ShowMessage( 'Invalid examine address' ) ;
    end ;
    Test.Save_Contents( Test_Stream ) ;
    Test_Stream.Seek( 0 ) ;

    { Test reads/writes... }
    for Dummy := 0 to 255 do
    begin
        E := Test.Deposit( Dummy * 2, 8, @Dummy, True ) ;
        Check_E ;
    end ;
    Size := 8 ;
    E := Test.Examine( 1024, Size, @Work, False ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on accessing non-memory' ) ;
    end ;
    for Dummy := 0 to 255 do
    begin
        Size := 8 ;
        Work := 0 ;
        E := Test.Examine( Dummy * 2, Size, @Work, True ) ;
        Check_E ;
        if( Size <> 8 ) then
        begin
            ShowMessage( 'Illegal size returned from Examine' ) ;
        end ;
        if( Work <> Dummy ) then
        begin
            ShowMessage( 'Examine doesn''t match deposit' ) ;
        end ;
    end ;

    { Misc other tests... }
    if( Test.Get_Access_Mode( 0, False ) <> Access_None ) then
    begin
        ShowMessage( 'Invalid access mode returned' ) ;
    end ;
    if( Test.Name <> 'TRS80 Screen' ) then
    begin
        ShowMessage( 'Invalid name returned' ) ;
    end ;
    E := Test.Set_Address_Range( 0, 255 ) ;
    Check_E ;
    E := Test.Set_Address_Range( 255, 0 ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on Invalid set_address_range' ) ;
    end ;
    E := Test.Examine( 1024, Size, @Work, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on accessing memory out of range' ) ;
    end ;
    if( Test.Get_Profiling = True ) then
    begin
        ShowMessage( 'Bad profiling return' ) ;
    end ;
    Test.Set_Profiling( True, True ) ;
    if( Test.Get_Profiling <> True ) then
    begin
        ShowMessage( 'Bad profiling return' ) ;
    end ;
    if( Test.Get_Read_Latency <> 0 ) then
    begin
        ShowMessage( 'Invalid default read latency' ) ;
    end ;
    if( Test.Get_Write_Latency <> 0 ) then
    begin
        ShowMessage( 'Invalid default write latency' ) ;
    end ;
    Test.Set_Read_Latency( 12 ) ;
    if( Test.Get_Read_Latency <> 12 ) then
    begin
        ShowMessage( 'Read latency did not change' ) ;
    end ;
    Test.Set_Write_Latency( 34 ) ;
    if( Test.Get_Write_Latency <> 34 ) then
    begin
        ShowMessage( 'Write latency did not change' ) ;
    end ;
    if( Test.Get_Access_Mode( 0, True ) <> Access_All ) then
    begin
        ShowMessage( 'Invalid default access mode' ) ;
    end ;

    { Test access failures... }
    E := Test.Set_Access_Mode( 0, 255, False, Access_None ) ;
    Check_E ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_None ) ;
    Check_E ;
    Size := 8 ;
    E := Test.Examine( 0, Size, @Work, True ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, False ) ; { Non memory write }
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ; { Memory write }
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Read ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Write ) ;
    if( Test.Read( 0, 8, True ) ) then
    begin
        ShowMessage( 'Read with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Execute ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;

    { Test I/O components... }
    E := Test.Set_Access_Mode( 0, 255, True, Access_RW ) ;
    Check_E ;
    E := Test.Connect_Input( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on nil input connection' ) ;
    end ;
    E := Test.Connect_Output( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on nil output connection' ) ;
    end ;
    Test_UI.Allow_Profile( 1, Access_Write ) ;
    Test_Output_Component.Enabled := True ;
    Test_Input_Component.Enabled := True ;
    if( Test.Output_Component( 0 ) <> Test_Output_Component ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    E := Test.Write( 1, 1, 8, True ) ;
    if( E.Code <> 0 ) then
    begin
        ShowMessage( 'Error on write' ) ;
    end ;
    if( not Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile not triggered' ) ;
    end ;
    if( Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watch incorrectly triggered' ) ;
    end ;
    if( Test.Read( 1, 8, False ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    Test_UI.Allow_Profile( 1, Access_Read ) ;
    if( not Test.Read( 1, 8, True ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    if(
        ( Test_Output_Component._Address <> 1 )
        or
        ( Test_Output_Component._Size <> 8 )
        or
        ( Test_Output_Component._Value <> 1 )
      ) then
    begin
        ShowMessage( 'Read failure' ) ;
    end ;
    Test_UI.Allow_Profile( -1, 0 ) ;
    Test.Set_Profiling( False, False ) ;
    Test_UI.Profile_Triggered := False ;

    { Test watchpoints... }
    E := Test.Set_Watchpoint( 0, False, Access_Write ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 0, True, Access_Read ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 0, True, Access_Write ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 1, True, Access_Read ) ;
    Check_E ;
    Test_UI.Allow_Watch( 0, Access_Write ) ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code <> 0 ) then
    begin
        ShowMessage( 'Error on write' ) ;
    end ;
    if( not Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watchpoint not triggered' ) ;
    end ;
    if( Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile incorrectly triggered' ) ;
    end ;
    Test_UI.Watch_Triggered := False ;
    Test_UI.Allow_Watch( 1, Access_Read ) ;
    if( not Test.Read( 1, 8, True ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    if( not Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watchpoint not triggered' ) ;
    end ;
    if( Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile incorrectly triggered' ) ;
    end ;
    E := Test.Clear_Watchpoint( 1, False, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 0, True, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 0, True, Access_Write ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 1, True, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 1, True, Access_Write ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from clearing non-existant watchpoint' ) ;
    end ;

    { Test disconnections... }
    E := Test.Disconnect_Output( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from Disconnecting nil output' ) ;
    end ;
    E := Test.Disconnect_Input( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from Disconnecting nil input' ) ;
    end ;
    E := Test.Disconnect_Output( Test_Output_Component ) ;
    Check_E ;
    E := Test.Disconnect_Input( Test_Input_Component ) ;
    Check_E ;
    if( Test.Output_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Output_Component( 1 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Input_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;
    if( Test.Input_Component( 1 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;

    { Test save/restore contents... }
    for Dummy := 0 to 255 do
    begin
        Work := Dummy div 2 ;
        E := Test.Deposit( Dummy, 8, @Work, True ) ;
        Check_E ;
    end ;
    E := Test.Save_Contents( Test_Stream ) ;
    Check_E ;
    Work := 255 ;
    for Dummy := 0 to 255 do
    begin
        E := Test.Deposit( Dummy, 8, @Work, True ) ;
        Check_E ;
    end ;
    Test_Stream.Seek( 0 ) ;
    E := Test.Restore_Contents( Test_Stream ) ;
    Check_E ;
    Size := 8 ;
    for Dummy := 0 to 255 do
    begin
        Work := 0 ;
        E := Test.Examine( Dummy, Size, @Work, True ) ;
        Check_E ;
        if( Work <> Dummy div 2 ) then
        begin
            ShowMessage( 'Save/Restore failure' ) ;
        end ;
    end ;

    { Test save/restore state... }
    E := Test_Stream.Seek( 0 ) ;
    Check_E ;
    Test.Save_State( Test_Stream ) ;
    Test.Set_Address_Range( 128, 1024 ) ;
    Test.Set_Read_Latency( 12345 ) ;
    Test.Set_Write_Latency( 6789 ) ;
    Test.Set_Profiling( True, True ) ;
    Test_Stream.Seek( 0 ) ;
    E := Test.Restore_State( Test_Stream ) ;
    Check_E ;
    if( ( Test._Low <> 0 ) or ( Test._High <> 255 ) ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;
    if( Test.Get_Profiling ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;
    if( ( Test.Get_Read_Latency <> 12 ) or ( Test.Get_Write_Latency <> 34 ) ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;

    { Test termination... }
    E := Test.Terminate ;
    Check_E ;


    { Stress test... }

    { Setup... }
    E := Test.Initialize( Test_UI ) ;
    Check_E ;
    E := Test.Set_Address_Range( 0, $7FFFFFFFFFFFFFFF ) ;
    Check_E ;
    E := Test.Set_Access_Mode( 0, $7FFFFFFFFFFFFFFF, True, Access_RW ) ;
    Check_E ;
    Count := 0 ;
    Randomize ;

    { Stress! }
    while( True ) do
    begin
        { Generate random 63-bit address... }
        Address := random( $7FFFFFFF ) ;
        Address := ( Address shl 31 ) or random( $7FFFFFFF ) ;
        Address := ( Address shl 1 ) or random( 2 ) ;

        { Do some read/write operations... }
        E := Test.Deposit( Address, 64, @Address, True ) ;
        Check_E ;
        Dummy := 64 ;
        E := Test.Examine( Address, Dummy, @Temp, True ) ;
        Check_E ;
        if( Temp <> Address ) then
        begin
            ShowMessage( 'R/W to address ' + inttostr( Address ) + ' failed' ) ;
        end ;

        { Count the accesses }
        inc( Count ) ;
    end ;

    { Cleanup... }
    E := Test.Terminate ;
    Check_E ;
end ;
{$ENDIF}


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

