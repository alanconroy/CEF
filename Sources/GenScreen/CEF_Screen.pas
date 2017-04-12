{
        Program Name : CEF_Screen
        Package Name : GenScreen
        Purpose      : CEF Screen component
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0A

	    Copyright (C) 2005-2016 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

          14-Oct-2007   EAC         Added threading support.

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This unit handles the actual screen output for the GenScreen CEF32
        component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CEF_Screen ;

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
     _CEF, // TMemory
     CEF, // TBase_Component
     _CEFUtil ; // TCEF_Watchpoint

const CEFScreen_Facility = -1 ;
const TCEFScreenErr_Success = 0 ;
const TCEFScreenErr_Invalid_Range = 1 ;
const TCEFScreenErr_Component_Not_Found = 2 ;
const TCEFScreenErr_No_Matching_Watchpoint = 3 ;
const TCEFScreenErr_Access_Violation = 4 ;
const TCEFScreenErr_Address_Out_Of_Range = 5 ;
const TCEFScreenErr_Invalid_Component = 6 ;
const TCEFScreenErr_Memory_Exhausted = 7 ;

type TCEF_Screen_UI = class;

     TScreen_Image = class
                         public // Constructors and destructors...
                             constructor Create ;

                         public // Instance data...
                             Bitmap_Image : TBitmap ;
                             Data : array[ 0..$FFFF ] of byte ;
                             First_Dirty, Last_Dirty : integer ; // Range of updated, but undisplayed, data
                             Columns, Rows : integer ;
                             Low, High : int64 ; { Current memory range }
                             Need_Update : boolean ;

                         public // API...
                             function Build_Image : boolean ;
                             procedure Clear_Memory ;
                     end ;

     TCEF_Screen = class( TBase_Component )
                       private
                           _User_Interface : TCEF_Screen_UI ;
                           Access_Mode : integer ;
                           Read_Latency, Write_Latency : longint ;
                           Default_Input, Default_Output : TComponent ;
                           _Our_Screen : TForm ;
                           _Tag : longint ;
                           _Parent : TComponent ;
                           _Logger : TCEF_Logger ;

                       public { Public instance data... }
                           _Serial_Number : integer ;

                       protected // Property handlers...
                           function Get_Our_Screen : TForm ;

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

                           function Get_Logger : TCEF_Logger ; override ;

                           procedure Set_Logger( Value : TCEF_Logger ) ;
                               override ;

                           property Our_Screen : TForm
                               read Get_Our_Screen
                               write _Our_Screen ;
                   end ;

     TUpdate_Thread = class( TThread )
                          public
                              Last_Width, Last_Height : integer ;
                              UI : TCEF_Screen_UI ;
                              WControl : TWinControl ;
                              ID : integer ;

                              procedure Execute ; override ;
                      end ;

     TCEF_Screen_UI = class( TBase_User_Interface )
                          public // Constructors and destructors...
                              constructor Create ;
                              destructor Destroy ; override ;

                          private { Instance data... }
                              Profiling : boolean ; { True if profiling memory accesses }
                              _UI : TUI_Interface ;
                              Watchpoints : TCEF_Watchpoint_Manager ;
                              Caption : string ;
                              Top, Left, Height, Width : integer ; // Saved metrics
                              Parent : TCEF_Screen ;
                              Update_Thread : TUpdate_Thread ;
                              Image : TScreen_Image ;
                              Threaded : boolean ;

                          private // Internal utility routines...
                              procedure CB_Paint( Sender : Tobject ) ;
                              procedure CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
                                  Shift : TShiftState ; X, Y : Integer ) ;
                              procedure CB_Toggle_Embed( Sender : TObject ) ;
                              procedure Clear_Memory ;
                              function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

                          public { API... }
                              function Get_Hidden : boolean ; override ;
                              procedure Set_Hidden( Value : boolean ) ; override ;

                              function Get_Parent_Window : THandle ; override ;
                              procedure Set_Parent_Window( Value : THandle ) ;
                                  override ;

                              function Get_Caption : PChar ; override ;
                              procedure Set_Caption( Value : PChar ) ; override ;

                              procedure Set_Size( Height, Width : integer ) ;
                                  override ;
                      end ; // TCEF_Screen

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
     UE, // Create_Simple_UE
     Num1s, // Num1
     Parse ; // TString_Parser

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ; external 'CEF_Util.dll' ;


function Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        TCEFScreenErr_Success: _Error := 'Success' ;
        TCEFScreenErr_Invalid_Range: _Error := 'Invalid range' ;
        TCEFScreenErr_Component_Not_Found: _Error := 'Component not found' ;
        TCEFScreenErr_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        TCEFScreenErr_Access_Violation: _Error := 'Access violation' ;
        TCEFScreenErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        TCEFScreenErr_Invalid_Component: _Error := 'Invalid component' ;
        TCEFScreenErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { Translate_Error }


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



{ TCEF_Screen_Debugger methods... }

type TCEF_Screen_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TCEF_Screen ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TCEF_Screen
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TCEF_Screen_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
        3 : I.Title := PChar( '_High = ' + Num1( Memory._User_Interface.Image.High ) ) ;
        4 : I.Title := PChar( '_Low = ' + Num1( Memory._User_Interface.Image.Low ) ) ;
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
        9 : I.Title := Pchar( 'Watchpoints = ' + Pointer_To_String( pointer( Memory._User_Interface.Watchpoints ) ) ) ;
        10 : I.Title := PChar( 'Write_Latency = ' + Num1( Memory.Write_Latency ) ) ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TCEF_Screen_Debugger.Count : longint ;

begin
    Result := 11 ;
end ;


// TUpdate_Thread methods...

procedure TUpdate_Thread.Execute ;

var _Priority : longint ;

begin
    while( not Terminated ) do
    begin
        try
            if( not UI.Image.Need_Update ) then
            begin
                UI._UI.Process_End( ID ) ;
                Suspend ; // Wait for something to change
            end ;
            UI._UI.Process_Start( ID, _Priority ) ;
            if( ( _Priority >= -32767 ) and ( _Priority <= 32767 ) ) then
            begin
                _Priority := _Priority shr 13 ;
                if( _Priority > 3 ) then
                begin
                    _Priority := _Priority or $FFFFFFFC ; // Sign extend
                end ;
                case _Priority of
                    -3 : Priority := tpIdle ;
                    -2 : Priority := tpLowest ;
                    -1 : Priority := tpLower ;
                    0 : Priority := tpNormal ;
                    1 : Priority := tpHigher ;
                    2 : Priority := tpHighest ;
                    3 : Priority := tpTimeCritical ;
                end ;
            end ;
            UI.Image.Build_Image ;
            WControl.Invalidate ;
        except
        end ;
    end ;
end ;



// TScreen_Image methods...

// Constructors and destructors...

constructor TScreen_Image.Create ;

begin
    inherited Create ;

    Columns := 80 ;
    Rows := 25 ;

    { Allow us to cover the entire memory range... }
    Low := 0 ;
    fillchar( High, sizeof( High ), 127 ) ;
    fillchar( High, sizeof( High ) - 1, 255 ) ;
end ;


// API...

function TScreen_Image.Build_Image : boolean ;

var A, C, R : integer ; // Current position
    X, Y : integer ; // Cell width and height
    Rect : TRect ;
    S : string ;

begin
    // Create and size bitmap...
    if( Bitmap_Image = nil ) then
    begin
        Bitmap_Image := TBitmap.Create ;
        Bitmap_Image.Canvas.Brush.Color := clBlack ;
        Bitmap_Image.Canvas.Font.Color := clWhite ;
    end ;
    Bitmap_Image.Canvas.Trylock ;
    try
        Y := Bitmap_Image.Canvas.TextHeight( 'W' ) ;
        Bitmap_Image.Height := Y * Rows ;
        X := Bitmap_Image.Canvas.TextWidth( 'W' ) ;
        Bitmap_Image.Width := X * Columns ;
        Result := False ;

        // Write data to bitmap...
        fillchar( Rect, sizeof( Rect ), 0 ) ;
        Rect.Bottom := Bitmap_Image.Height ;
        Rect.Right := Bitmap_Image.Width ;
        Bitmap_Image.Canvas.FillRect( Rect ) ;
        for R := 1 to Rows do
        begin
            for C := 1 to Columns do
            begin
                A := Low + ( R - 1 ) * Columns + C - 1 ;
                if( ( A >= First_Dirty ) and ( A <= Last_Dirty ) ) then
                begin
                    Result := True ;
                    S := chr( Data[ A ] ) ;
                    SetBkMode( Bitmap_Image.Canvas.Handle, OPAQUE ) ;
                    Bitmap_Image.Canvas.TextOut( ( C - 1 ) * X, ( R - 1 ) * Y, S ) ;
                    if( A = First_Dirty ) then
                    begin
                        First_Dirty := A + 1 ;
                    end ;
                    if( A = Last_Dirty ) then
                    begin
                        Last_Dirty := A - 1 ;
                    end ;
                end ;
            end ;
        end ;
    finally
        Bitmap_Image.Canvas.Unlock ;
    end ;
end ;


procedure TScreen_Image.Clear_Memory ;

begin
    fillchar( Data, sizeof( Data ), 0 ) ;
end ;



// TCEF_Screen_UI methods...

// Constructors and destructors...

const CEF_Screen_Thread_Name : string = 'CEF Screen' ;

constructor TCEF_Screen_UI.Create ;

begin
    inherited Create ;

    Image := TScreen_Image.Create ;
    Profiling := False ;
    Watchpoints := Get_Watchpoint_Manager ;

    Caption := 'Screen' ;
end ;


destructor TCEF_Screen_UI.Destroy ;

begin
    if( Watchpoints <> nil ) then
    begin
        Watchpoints.Terminate ;
        Watchpoints := nil ;
    end ;
    if( Image <> nil ) then
    begin
        Image.Free ;
        Image := nil ;
    end ;
    if( _UI <> nil ) then
    begin
        _UI.Process_Deleted( Update_Thread.ID ) ;
    end ;
    if( Update_Thread <> nil ) then
    begin
        Update_Thread.Terminate ;
    end ;

    inherited Destroy ;
end ;


// Callbacks...

procedure TCEF_Screen_UI.CB_Paint( Sender : Tobject ) ;

    procedure _Draw ;

    var P : TTransparent_Panel ;

    begin
        // Draw on window...
        try
            P := TTransparent_Panel( Update_Thread.WControl ) ;
            while( true ) do // Keep trying until canvas is unlocked
            begin
                if( Image.Bitmap_Image.Canvas.Trylock ) then
                begin
                    try
                        if( Image.Bitmap_Image <> nil ) then
                        begin
                            P.Canvas.StretchDraw( P.ClientRect, Image.Bitmap_Image ) ;
                        end ;
                    finally
                        Image.Bitmap_Image.Canvas.Unlock ;
                    end ;
                    exit ;
                end ;
            end ;
        except
        end ;
    end ;

begin
    if( Threaded ) then
    begin
        Update_Thread.Resume ;
    end else
    begin
        Image.Build_Image ;
    end ;
    _Draw ;
end ; // TCEF_Screen_UI.CB_Paint


var Pop_Up : TPopupMenu = nil ;

procedure TCEF_Screen_UI.CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
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
        if( Parent.Our_Screen.ParentWindow = 0 ) then
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
        P := Parent.Our_Screen.ClientToScreen( P ) ;
        Pop_Up.Popup( P.X, P.Y ) ;
    end ;
end ;


procedure TCEF_Screen_UI.CB_Toggle_Embed( Sender : TObject ) ;

begin
    _UI.Toggle_Embed( Parent ) ;
end ;


procedure TCEF_Screen_UI.Clear_Memory ;

begin
    Image.Clear_Memory ;
end ; { TCEF_Screen_UI.Clear_Memory }


function TCEF_Screen_UI.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoints.Watchpoint_At( Address ) ;
end ;



// TCEF_Screen methods...

{ API... }

function TCEF_Screen.Get_Our_Screen : TForm ;

var Panel : TTransparent_Panel ;

begin
    if( _Our_Screen = nil ) then
    begin
        TCEF_Screen_UI( _User_Interface ).Update_Thread := TUpdate_Thread.Create( True ) ;
        TCEF_Screen_UI( _User_Interface ).Update_Thread.UI := _User_Interface ;
        TCEF_Screen_UI( _User_Interface ).Update_Thread.ID := TCEF_Screen_UI( _User_Interface )._UI.Process_ID( PChar( CEF_Screen_Thread_Name ), False ) ;
        TCEF_Screen_UI( _User_Interface ).Threaded := ( TCEF_Screen_UI( _User_Interface ).Update_Thread.ID <> -1 ) ;

        _Our_Screen := TForm.Create( Application ) ;
        Panel := TTransparent_Panel.Create( _Our_Screen ) ;
        Panel.Parent := _Our_Screen ;
        Panel.Align := alClient ;
        Panel.Canvas.Brush.Color := clBlack ;
        Panel.Canvas.Pen.Color := clWhite ;
        Panel.Color := clBlack ;
        Panel.Font.Color := clWhite ;
        Panel.OnPaint := _User_Interface.CB_Paint ;
        Panel.OnMouseDown := _User_Interface.CB_MouseDown ;
        _Our_Screen.Caption := 'Screen' ;
        _Our_Screen.BorderIcons := [] ;
        _Our_Screen.Visible := True ;
        _User_Interface.Update_Thread.WControl := Panel ;
    end ;
    Result := _Our_Screen ;
end ;


function TCEF_Screen.Facility_Code : longint ;

begin
    Facility_Code := CEFScreen_Facility ;
end ;


function TCEF_Screen.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Read_Latency := 0 ; { Infinitely fast memory :) }
    Write_Latency := 0 ;
    Default_Input := nil ;
    Default_Output := nil ;
    Access_Mode := Access_RW or Access_Execute ;
    _User_Interface := TCEF_Screen_UI.Create ;
    _User_Interface._UI := UI ;
    _User_Interface.Parent := self ;

    Initialize := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
end ; { TCEF_Screen.Initialize }


function TCEF_Screen.Terminate : TUnified_Exception ;

begin
    if( _User_Interface._UI <> nil ) then
    begin
        _User_Interface._UI.Termination_Notice( self ) ;
    end ;
    Our_Screen.Close ;
    Our_Screen := nil ;
    Terminate := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
    _User_Interface.Free ;
    _User_Interface := nil ;
    Free ;
end ; { TCEF_Screen.Terminate }



function TCEF_Screen.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Clear_Watchpoint := nil ;
    if( Memory and ( _User_Interface.Image.Low <= Address ) and ( _User_Interface.Image.High >= Address ) ) then
    begin
        Result := _User_Interface.Watchpoints.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( TCEFScreenErr_No_Matching_Watchpoint ) ;
    end ;
end ;


function TCEF_Screen.Component_Type : longint ;

begin
    Component_Type := Component_Type_UI ;
end ;


function TCEF_Screen.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( TCEFScreenErr_Invalid_Component ) ;
        exit ;
    end ;
    Default_Input := Component ;
    Connect_Input := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
end ;


function TCEF_Screen.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( TCEFScreenErr_Invalid_Component ) ;
        exit ;
    end ;
    Default_Output := Component ;
    Connect_Output := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
end ;


function TCEF_Screen.Debugger : TDebug_Interface ;

begin
    Result := TCEF_Screen_Debugger.Create ;
    TCEF_Screen_Debugger( Result ).Memory := Self ;
end ;


function TCEF_Screen.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count : integer ;

begin
    if( ( not Memory ) or ( _User_Interface.Image.Low > Address ) or ( _User_Interface.Image.High < Address ) ) then
    begin
        Count := 0 ;
    end else
    begin
        if( Size = 0 ) then
        begin
            Count := 1 ; // Default6 is 1 byte
        end else
        begin
            Count := ( Size + 7 ) div 8 ; // Round to byte
        end ;
    end ;
    _Buffer := PChar( Buffer ) ;

    while( Count > 0 ) do
    begin
        if( _User_Interface.Image.Last_Dirty > Address ) then
        begin
            _User_Interface.Image.Last_Dirty := Address ;
        end ;
        if( _User_Interface.Image.First_Dirty < Address ) then
        begin
            _User_Interface.Image.First_Dirty := Address ;
        end ;
        _User_Interface.Image.Data[ Address ] := ord( _Buffer[ 0 ] ) ;
        _Buffer := _Buffer + 1 ;
        dec( Count ) ;
        inc( Address ) ;
    end ; { while( Count > 0 ) }
    Deposit := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
end ; { TCEF_Screen.Deposit }


function TCEF_Screen.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component <> Default_Input ) or ( Component = nil ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( TCEFScreenErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
	Default_Input := nil ;
    end ;
end ;


function TCEF_Screen.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component <> Default_Output ) or ( Component = nil ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( TCEFScreenErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
	Default_Output := nil ;
    end ;
end ;


function TCEF_Screen.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count : integer ;

begin
    if( ( not Memory ) or ( _User_Interface.Image.Low > Address ) or ( _User_Interface.Image.High < Address ) ) then
    begin
        Examine := _User_Interface.Set_Error( TCEFScreenErr_Address_Out_Of_Range ) ;
        exit ;
    end ;
    if( Size = 0 ) then
    begin
        Count := 1 ; // Default6 is 1 byte
    end else
    begin
        Count := ( Size + 7 ) div 8 ; // Round to byte
    end ;
    fillchar( Buffer^, Count, 255 ) ;
    _Buffer := Buffer ;
    try
        while( Count > 0 ) do
        begin
            _Buffer[ 0 ] := char( _User_Interface.Image.Data[ Address ] ) ;
            _Buffer := _Buffer + 1 ;
            dec( Count ) ; { One less byte to read }
            inc( Address ) ;
        end ; { while( Count > 0 ) }
    except
    end ;
    Examine := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
end ; { TCEF_Screen.Examine }


function TCEF_Screen.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    if( Memory and ( _User_Interface.Image.Low <= Address ) and ( _User_Interface.Image.High >= Address ) ) then
    begin
        Get_Access_Mode := Access_Mode ;
    end else
    begin
        Get_Access_Mode := Access_None ;
    end ;
end ;


function TCEF_Screen.Get_Profiling : boolean ;

begin
    Get_Profiling := _User_Interface.Profiling ;
end ;


function TCEF_Screen.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := Read_Latency ;
end ;


function TCEF_Screen.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := Write_Latency ;
end ;


function TCEF_Screen.Input_Component( Index : longint ) : TComponent ;

begin
    if( Index = 0 ) then
    begin
	Input_Component := Default_Input ;
    end else
    begin
	Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'CEF Generic Screen'#0 ;

function TCEF_Screen.Name : PChar ;

begin
    Name := _Name ;
end ;


function TCEF_Screen.Output_Component( Index : longint ) : TComponent ;

begin
    if( Index = 0 ) then
    begin
	Output_Component := Default_Output ;
    end else
    begin
	Output_Component := nil ;
    end ;
end ;


function TCEF_Screen.Read( Address : int64 ; Size : longint ;
            IO_Type : longint ) : boolean ;

var Buffer : PChar ;
    Count : integer ;
    Watch : TCEF_Watchpoint ;

begin
    if( ( Address >= _User_Interface.Image.Low ) and ( Address <= _User_Interface.Image.High ) and ( IO_Type = IO_Type_Memory ) ) then
    begin
        if( ( Access_Mode and Access_Read ) = 0 ) then
        begin
            Read := False ; { TCEFScreenErr_Access_Violation }
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
end ; { TCEF_Screen.Read }


function TCEF_Screen.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

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


function TCEF_Screen.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var S : longint ;

begin
    Result := nil ;
    S := sizeof( _User_Interface.Image.Low ) ;
    Stream.Read( _User_Interface.Image.Low, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    Stream.Read( _User_Interface.Image.High, S ) ;
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


function TCEF_Screen.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

var Loop : int64 ;
    High : int64 ;
    S : longint ;
    Value : integer ;

begin { TCEF_Screen.Save_Contents }
    fillchar( Result, sizeof( Result ), 0 ) ;

    { Write contents... }
    Loop := 0 ;
    Stream.Write( Loop, sizeof( Loop ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( High, sizeof( High ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    S := 1 ;
    while( Loop <= High ) do
    begin
        Examine( Loop, S, @Value, True ) ;
	    Stream.Write( Value, 1 ) ;
        Result := Stream.Last_Error ;
        if( Result <> nil ) then
        begin
            exit ;
        end ;
	    Loop := Loop + 1 ;
    end ;
end ; { TCEF_Screen.Save_Contents }


function TCEF_Screen.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Stream.Write( _User_Interface.Image.Low, sizeof( _User_Interface.Image.Low ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( _User_Interface.Image.High, sizeof( _User_Interface.Image.High ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( Read_Latency, sizeof( Read_Latency ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( Write_Latency, sizeof( Write_Latency ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( _User_Interface.Profiling, sizeof( _User_Interface.Profiling ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
end ;


function TCEF_Screen.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    if( Memory and ( _User_Interface.Image.Low <= Low ) and ( _User_Interface.Image.High >= High ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
end ;


procedure TCEF_Screen.Set_Profiling( _On, Children : boolean ) ;

begin
    _User_Interface.Profiling := _On ;
end ;


procedure TCEF_Screen.Set_Read_Latency( Value : longint ) ;

begin
    Read_Latency := Value ;
end ;


function TCEF_Screen.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Watchpoint := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
    if( Memory and ( _User_Interface.Image.Low <= Address ) and ( _User_Interface.Image.High >= Address ) ) then
    begin
        _User_Interface.Watchpoints.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TCEF_Screen.Set_Write_Latency( Value : longint ) ;

begin
    Write_Latency := Value ;
end ;


procedure TCEF_Screen.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TCEF_Screen.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var B : integer ;
    Watch : TCEF_Watchpoint ;

begin
    if(
        ( Address >= _User_Interface.Image.Low )
        and
        ( Address <= _User_Interface.Image.High )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Write ) = 0 ) then
        begin
            Write := _User_Interface.Set_Error( TCEFScreenErr_Access_Violation ) ;
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
        if( _Logger <> nil ) then
        begin
            B := _Logger.Data_Radix ;
            if( ( B < 2 ) or ( B > 36 ) ) then
            begin
                B := 10 ;
            end ;
            _Logger.Log( self, PChar( cvtb( 10, B, inttostr( Value ) ) +
                ' (' + inttostr( Size ) + ')' + ' @' +
                cvtb( 10, B, inttostr( Address ) ) ), -1, False, LT_Write ) ;
        end ;
        Our_Screen.Repaint ;
    end else
    begin
	    Write := _User_Interface.Set_Error( TCEFScreenErr_Success ) ;
    end ;
end ; // TCEF_Screen.Write


procedure TCEF_Screen.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TCEF_Screen.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TCEF_Screen.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TCEF_Screen.Set_Parent( Component : TComponent ) ;

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


function TCEF_Screen.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TCEF_Screen.Set_Logger( Value : TCEF_Logger ) ;

begin
    if( _Logger <> nil ) then
    begin
        _Logger.Detach ;
    end ;
    _Logger := Value ;
    if( Value <> nil ) then
    begin
        _Logger.Attach ;
    end ;
end ;


function TCEF_Screen.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


procedure TCEF_Screen.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S, S1 : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'PARALLEL' ) then
        begin
            _User_Interface.Threaded := True ;
            if( _User_Interface.Update_Thread.ID = -1 ) then
            begin
                _User_Interface.Update_Thread.ID :=
                    _User_Interface._UI.Process_ID( PChar( CEF_Screen_Thread_Name ), True ) ;
            end ;
        end else
        if( S = 'NOPARALLEL' ) then
        begin
            _User_Interface.Threaded := False ;
        end else
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
            _User_Interface.Image.Low := Convert_Value( S ) ;
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
            _User_Interface.Image.Rows := Convert_Value( S ) ;
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
            _User_Interface.Image.Columns := Convert_Value( S ) ;
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
            Our_Screen.Caption := S ;
            _User_Interface.Caption := S ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' ) 
    _User_Interface.Image.High := _User_Interface.Image.Low + _User_Interface.Image.Rows * _User_Interface.Image.Columns ;
    Parser.Free ;
end ; // TCEF_Screen.Set_Up


function TCEF_Screen_UI.Get_Hidden : boolean ;

begin
    Result := not Parent.Our_Screen.Visible ;
end ;


procedure TCEF_Screen_UI.Set_Hidden( Value : boolean ) ;

begin
    Parent.Our_Screen.Visible := not Value ;
end ;


function TCEF_Screen_UI.Get_Parent_Window : THandle ;

begin
    Result := Parent.Our_Screen.ParentWindow ;
end ;


procedure TCEF_Screen_UI.Set_Parent_Window( Value : THandle ) ;

begin
    if( Value <> Parent.Our_Screen.ParentWindow ) then
    begin
        if( Value = 0 ) then // Stand-alone
        begin
            Parent.Our_Screen.ParentWindow := Value ;
            Parent.Our_Screen.Top := Top ;
            Parent.Our_Screen.Left := Left ;
            Parent.Our_Screen.Height := Height ;
            Parent.Our_Screen.Width := Width ;
            Parent.Our_Screen.Caption := Caption ;
        end else
        begin
            Top := Parent.Our_Screen.Top ;
            Left := Parent.Our_Screen.Left ;
            Height := Parent.Our_Screen.Height ;
            Width := Parent.Our_Screen.Width ;
            Parent.Our_Screen.ParentWindow := Value ;
            Parent.Our_Screen.Caption := '' ;
        end ;
    end ;
end ;


function TCEF_Screen_UI.Get_Caption : PChar ;

begin
    Result := PChar( Caption ) ;
end ;


procedure TCEF_Screen_UI.Set_Caption( Value : PChar ) ;

begin
    Caption := string( Value ) ;
end ;


procedure TCEF_Screen_UI.Set_Size( Height, Width : integer ) ;

begin
    Parent.Our_Screen.Height := Height + GetSystemMetrics( SM_CXBORDER ) + GetSystemMetrics( SM_CYCAPTION ) ;
    Parent.Our_Screen.Width := Width + GetSystemMetrics( SM_CXBORDER ) * 2 ;
    Parent.Our_Screen.Top := -GetSystemMetrics( SM_CYCAPTION ) ;
    Parent.Our_Screen.Left := -GetSystemMetrics( SM_CXBORDER ) ;
end ;


const Screen_Facility_Name : PChar = 'CEF Screen' ;


end.

