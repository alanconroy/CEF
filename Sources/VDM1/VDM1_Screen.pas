{
        Program Name : VDM_Screen
        Package Name : GenScreen
        Purpose      : VDM-1 Screen component
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0A

        Copyright (C) 2006-2007 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

          14-Oct-2007   EAC         Added threading.

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This unit contains the logic for drawing on the VDM1 screen.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit VDM1_Screen ;

interface

uses // Borland...
     Windows, // TRect

     Classes, // TShiftState
     Controls, // TMouseButton
     Extctrls, // TTimer
     Forms, // TForm
     Graphics, // TBitmap

     // C&C...
     Collect, // TCollection
     _DebugIn, // TDebug_Interface
     _Streams, // TCOM_Stream
     TPanels, // TTransparent_Panel
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TCEF_Logger
     CEF, // TBase_User_Interface
     _CEFUtil ; // TCEF_Watchpoint_Manager

const VDM1Screen_Facility = -1 ;
const TVDM1ScreenErr_Success = 0 ;
const TVDM1ScreenErr_Invalid_Range = 1 ;
const TVDM1ScreenErr_Component_Not_Found = 2 ;
const TVDM1ScreenErr_No_Matching_Watchpoint = 3 ;
const TVDM1ScreenErr_Access_Violation = 4 ;
const TVDM1ScreenErr_Address_Out_Of_Range = 5 ;
const TVDM1ScreenErr_Invalid_Component = 6 ;
const TVDM1ScreenErr_Memory_Exhausted = 7 ;

type TVDM1_Screen_UI = class ;

     TScreen_Image = class
                         public // Constructors and destructors...
                             constructor Create ;

                         public // Instance data...
                             Bitmap_Image : TBitmap ;
                             Character_Set : TCEF_Character_Set ;
                             Cursor_Mode : integer ; { 0 = None, 1 = Blinking, 2 = Solid }
                             Cursor_State : boolean ; // True if blinking cursor is currently on
                             Data : array[ 0..1023 ] of byte ;
                             Dirty : array[ 0..1023 ] of bytebool ;
                             Display_Control : boolean ; { True to display control characters }
                             _Invert : boolean ;
                             Top_Line : integer ;
                             Need_Update : boolean ;
                             WControl : TWinControl ;

                         protected // Property handlers...
                             procedure Set_Invert( Value : boolean ) ;

                         public // API...
                             function Build_Image : boolean ;
                             procedure Clear_Memory ;
                             procedure Load( S : string ) ;
                             procedure Resize ;

                             property Invert : boolean
                                 read _Invert
                                 write Set_Invert ;
                     end ;

     TUpdate_Thread = class( TThread )
                          public
                              Last_Width, Last_Height : integer ;
                              UI : TVDM1_Screen_UI ;
                              ID : integer ;

                              procedure Execute ; override ;
                      end ;

     TVDM1_Screen = class( TBase_Component )
                        private // Instance data...
                            _User_Interface : TVDM1_Screen_UI ;
                            Default_Input, Default_Output : TComponent ;
                            _Tag : longint ;
                            _Parent : TComponent ;
                            Access_Mode : integer ;
                            Read_Latency, Write_Latency : longint ;
                            _Logger : TCEF_Logger ;

                        public { Public instance data... }
                            _Serial_Number : integer ;

                        public // API...
                            function User_Interface : TUser_Interface ;
                                override ;

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

                            function Get_Logger : TCEF_Logger ; override ;
                            
                            procedure Set_Logger( Value : TCEF_Logger ) ;
                                override ;
                    end ;

     TVDM1_Screen_UI = class( TBase_User_Interface )
                           public // Constructors and destructors...
                               constructor Create ;
                               destructor Destroy ; override ;

                           private { Instance data... }
                               Parent : TVDM1_Screen ;
                               _Low, _High : int64 ; { Current memory range }
                               Profiling : boolean ; { True if profiling memory accesses }
                               _UI : TUI_Interface ;
                               Watchpoint_List : TCEF_Watchpoint_Manager ;
                               _Port : integer ;
                               _Our_Screen : TForm ;
                               Caption : string ;
                               Top, Left, Height, Width : integer ; // Saved metrics
                               Threaded : boolean ;

                               Timer : TTimer ;
                               Update_Thread : TUpdate_Thread ;
                               Image : TScreen_Image ;

                           protected // Property handlers...
                               function Get_Our_Screen : TForm ;
                               function Translate_Error( Code : longint ) : string ;
                                   override ;

                           private // Internal utility routines...
                               procedure CB_Timer( Sender : TObject ) ;
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
                               procedure Set_Parent_Window( Value : THandle ) ; override ;

                               function Get_Caption : PChar ; override ;
                               procedure Set_Caption( Value : PChar ) ; override ;

                               procedure Set_Size( Height, Width : integer ) ; override ;

                               //function User_Interface : TUser_Interface ; override ;

                              public // Properties...
                               property Our_Screen : TForm
                                   read Get_Our_Screen
                                   write _Our_Screen ;
                       end ; // TVDM1_Screen_UI

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
     Parse, // TString_Parser
     Standard ; // Extension_Pos

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ; external 'CEF_Util.dll' ;
function Get_Character_Set : TCEF_Character_Set ; stdcall ; external 'CEF_Util.dll' ;


function TVDM1_Screen_UI.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        TVDM1ScreenErr_Success: _Error := 'Success' ;
        TVDM1ScreenErr_Invalid_Range: _Error := 'Invalid range' ;
        TVDM1ScreenErr_Component_Not_Found: _Error := 'Component not found' ;
        TVDM1ScreenErr_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        TVDM1ScreenErr_Access_Violation: _Error := 'Access violation' ;
        TVDM1ScreenErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        TVDM1ScreenErr_Invalid_Component: _Error := 'Invalid component' ;
        TVDM1ScreenErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
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



const Margin = 2 ;

// TScreen_Image methods...

// Constructors and destructors...

constructor TScreen_Image.Create ;

begin
    inherited Create ;

    fillchar( Dirty, sizeof( Dirty ), 255 ) ;
    Display_Control := True ;
    Cursor_Mode := 1 ; { Blinking }
    Character_Set := Get_Character_Set ;
    Character_Set.Load( '6574' ) ;
end ;


// Property handlers...

procedure TScreen_Image.Set_Invert( Value : boolean ) ;

begin
    if( Value <> _Invert ) then
    begin
        _Invert := Value ;
        if( Character_Set <> nil ) then
        begin
            Character_Set.Invert := Value ;
        end ;
    end ;
end ;


function TScreen_Image.Build_Image : boolean ;

var A : integer ;
    C_Width, R_Height : integer ;
    C, R : integer ; // Current position
    Rect : Windows.TRect ;
    Row : integer ;
    S : string ;
    X, Y : integer ; // Cell width and height

begin
    Need_Update := False ;
    Result := False ; // Assume no updates

    // Ensure bitmap is created...
    if( Bitmap_Image = nil ) then
    begin
        Bitmap_Image := TBitmap.Create ;
        if( Invert ) then
        begin
            Bitmap_Image.Canvas.Brush.Color := clWhite ;
            Bitmap_Image.Canvas.Font.Color := clBlack ;
        end else
        begin
            Bitmap_Image.Canvas.Brush.Color := clBlack ;
            Bitmap_Image.Canvas.Font.Color := clWhite ;
        end ;
    end ;

    if( not Bitmap_Image.Canvas.trylock ) then
    begin
        exit ;
    end ;
    try
        // Make sure we have a character set...
        if( Character_Set = nil ) then
        begin
            Y := Bitmap_Image.Canvas.TextHeight( 'W' ) ;
            X := Bitmap_Image.Canvas.TextWidth( 'W' ) ;
        end else
        begin
            Y := Character_Set.Height ;
            X := Character_Set.Width( 0 ) ;
        end ;

        // Make sure bitmap is sized correctly...
        Bitmap_Image.Height := Y * 16 + Margin ;
        Bitmap_Image.Width := X * 64 + Margin ;

        // Determine on-screen row height and column width
        R_Height := WControl.ClientHeight div 16 ;
        C_Width := WControl.ClientWidth div 64 ;
    finally
        Bitmap_Image.Canvas.Unlock ;
    end ;

    // Write data to bitmap...
    for R := 1 to 16 do
    begin
        Row := R + Top_Line ;
        if( Row > 16 ) then
        begin
            Row := Row - 16 ;
        end ;
        for C := 1 to 64 do
        begin
            A := ( Row - 1 ) * 64 + C - 1 ;
            if( Dirty[ A ] ) then
            begin
                if( not Bitmap_Image.Canvas.trylock ) then
                begin
                    exit ;
                end ;
                try
                    Result := True ;
                    Dirty[ A ] := false ;
                    A := Data[ A ] ;
                    S := chr( A and $7F ) ;
                    if( A < 32 ) then
                    begin
                        if( not Display_Control ) then
                        begin
                            A := 32 ;
                        end ;
                    end ;
                    if( A = 127 ) then
                    begin
                        //TODO: Support special glyph
                        continue ;
                    end ;
                    if(
                        ( Character_Set = nil )
                        or
                        ( not Character_Set.Has_Glyph( A and 127 ) )
                      ) then
                    begin
                        SetBkMode( Bitmap_Image.Canvas.Handle, OPAQUE ) ;
                        Bitmap_Image.Canvas.TextOut( ( C - 1 ) * X + Margin div 2, ( Row - 1 ) * Y + Margin div 2, S ) ;
                    end else
                    begin
                        Character_Set.Draw( Bitmap_Image.Canvas.Handle, ( C - 1 ) * X + Margin div 2, ( Row - 1 ) * Y + Margin div 2, A and 127 ) ;
                    end ;
                    if(
                        ( A > 127 ) // Cursor
                        and
                        (
                          ( Cursor_Mode = 2 )
                          or
                          (
                            ( Cursor_Mode = 1 )
                            and
                            Cursor_State
                          )
                        )
                      ) then
                    begin
                        if( Invert ) then
                        begin
                            selectobject( Bitmap_Image.Canvas.Handle, GetStockObject( BLACK_PEN ) ) ;
                        end else
                        begin
                            selectobject( Bitmap_Image.Canvas.Handle, GetStockObject( WHITE_PEN ) ) ;
                        end ;
                        MoveToEx( Bitmap_Image.Canvas.Handle, ( C - 1 ) * X + Margin div 2, ( Row ) * Y - 1 + Margin div 2, nil ) ;
                        LineTo( Bitmap_Image.Canvas.Handle, C * X + Margin div 2 - 1, ( Row ) * Y - 1  + Margin div 2) ;
                    end ;

                    // Invalidate area of window...
                    Rect.Top := R_Height * ( R - 2 ) ;
                    Rect.Bottom := R_Height * ( R + 3 ) ;
                    Rect.Left := C_Width * ( C - 2 ) ;
                    Rect.Right := C_Width * ( C + 3 ) ;
                    InvalidateRect( WControl.Handle, @Rect, False ) ;
                finally
                    Bitmap_Image.Canvas.Unlock ;
                end ;
            end ; // if( not Dirty[ ( Row - 1 ) * 64 + C - 1 ] )
        end ; // for C := 1 to 64
    end ; // for R := 1 to 16
end ; // TScreen_Image.Build_Image


procedure TScreen_Image.Clear_Memory ;

begin
    fillchar( Data, sizeof( Data ), 32 ) ;
end ;


procedure TScreen_Image.Load( S : string ) ;

begin
    Character_Set.Load( PChar( S ) ) ;
end ;


procedure TScreen_Image.Resize ;

var Rect : TRect ;

begin
    fillchar( Rect, sizeof( Rect ), 0 ) ;
    Rect.Bottom := Bitmap_Image.Height ;
    Rect.Right := Bitmap_Image.Width ;
    Bitmap_Image.Canvas.FillRect( Rect ) ;
    fillchar( Dirty, sizeof( Dirty ), 255 ) ;
end ;


// TUpdate_Thread methods...

procedure TUpdate_Thread.Execute ;

var _Priority : longint ;

begin
    while( not Terminated ) do
    begin
        try
            if( UI <> nil ) then
            begin
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
            end ;
        except
        end ;
    end ;
end ;


{ TVDM1_Screen_Debugger methods... }

type TVDM1_Screen_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TVDM1_Screen ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TVDM1_Screen
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TVDM1_Screen_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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


function TVDM1_Screen_Debugger.Count : longint ;

begin
    Result := 11 ;
end ;



const VDM1_Thread_Name : string = 'VDM1' ;

// TVDM1_Screen_UI methods...

// Constructors and destructors...

constructor TVDM1_Screen_UI.Create ;

begin
    inherited Create ;

    Image := TScreen_Image.Create ;
    Profiling := False ;
    Watchpoint_List := Get_Watchpoint_Manager ;
    _Port := $C8 ;
    _Low := $CC00 ;
    _High := $CFFF ;

    Caption := 'Screen' ;

    Update_Thread := TUpdate_Thread.Create( True ) ;
    Update_Thread.UI := self ;

    Timer := TTimer.Create( Our_Screen ) ;
    Timer.Interval := 250 ;
    Timer.OnTimer := CB_Timer ;
end ; // TVDM1_Screen_UI.Create


destructor TVDM1_Screen_UI.Destroy ;

begin
    Clear_Memory ;
    Timer.Enabled := False ;
    Our_Screen.Close ;
    Our_Screen := nil ;
    Watchpoint_List.Terminate ;
    Watchpoint_List := nil ;
    _UI.Process_Deleted( Update_Thread.ID ) ;
    Update_Thread.Terminate ;

    inherited Destroy ;
end ;


// Property handlers...

function TVDM1_Screen_UI.Get_Our_Screen : TForm ;

var Panel : TTransparent_Panel ;

begin
    if( _Our_Screen = nil ) then
    begin
        _Our_Screen := TForm.Create( Application ) ;
        Panel := TTransparent_Panel.Create( _Our_Screen ) ;
        Panel.Parent := _Our_Screen ;
        Panel.Align := alClient ;
        Panel.Canvas.Brush.Color := clBlack ;
        Panel.Canvas.Pen.Color := clWhite ;
        Panel.Color := clBlack ;
        Panel.Font.Color := clWhite ;
        Panel.OnPaint := CB_Paint ;
        Panel.OnMouseDown := CB_MouseDown ;
        Panel.BevelOuter := bvNone ;
        _Our_Screen.Caption := 'Screen' ;
        _Our_Screen.BorderIcons := [] ;
        _Our_Screen.Visible := True ;
        Image.WControl := Panel ;
    end ;
    Result := _Our_Screen ;
end ;


// Callbacks..

procedure TVDM1_Screen_UI.CB_Timer( Sender : TObject ) ;

begin
    if( Image.Cursor_Mode = 1 ) then // Blinking cursor
    begin
        Image.Cursor_State := not Image.Cursor_State ;
        fillchar( Image.Dirty, sizeof( Image.Dirty ), 255 ) ;
        Image.Need_Update := True ;
        Update_Thread.Resume ;
    end ;
end ;


procedure TVDM1_Screen_UI.CB_Paint( Sender : Tobject ) ;


    procedure _Draw ;

    var P : TTransparent_Panel ;

    begin
        // Draw on window...
        try
            while( true ) do // Keep trying until canvas is unlocked
            begin
                if( Image.Bitmap_Image.Canvas.Trylock ) then
                begin
                    try
                        if( Image.Bitmap_Image <> nil ) then
                        begin
                            // Draw on window...
                            P := TTransparent_Panel( Our_Screen.Controls[ 0 ] ) ;
                            try
                                P.Canvas.StretchDraw( P.ClientRect, Image.Bitmap_Image ) ;
                            except
                            end ;
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
end ; // TVDM1_Screen.CB_Paint


var Pop_Up : TPopupMenu = nil ;

procedure TVDM1_Screen_UI.CB_MouseDown( Sender : TObject ; Button : TMouseButton ;
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


procedure TVDM1_Screen_UI.CB_Toggle_Embed( Sender : TObject ) ;

begin
    _UI.Toggle_Embed( Parent ) ;
end ;


procedure TVDM1_Screen_UI.Clear_Memory ;

begin
    Image.Clear_Memory ;
end ;


function TVDM1_Screen_UI.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_List.Watchpoint_At( Address ) ;
end ;



// TVDM1_Screen methods...

// API...

function TVDM1_Screen.User_Interface : TUser_Interface ;

begin
    Result := _User_Interface ;
end ;


function TVDM1_Screen.Facility_Code : longint ;

begin
    Facility_Code := VDM1Screen_Facility ;
end ;


function TVDM1_Screen.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Read_Latency := 0 ; // Infinitely fast memory :)
    Write_Latency := 0 ;
    Default_Input := nil ;
    Default_Output := nil ;
    _User_Interface := TVDM1_Screen_UI.Create ;
    _User_Interface.Parent := self ;
    _User_Interface._UI := UI ;
    _User_Interface.Update_Thread.ID := UI.Process_ID( PChar( VDM1_Thread_Name ), False ) ;
    _User_Interface.Threaded := ( _User_Interface.Update_Thread.ID <> -1 ) ;
    Access_Mode := Access_RW or Access_Execute ;

    Initialize := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
end ; { TVDM1_Screen.Initialize }


function TVDM1_Screen.Terminate : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    if( _User_Interface <> nil ) then
    begin
        if( _User_Interface._UI <> nil ) then
        begin
            _User_Interface._UI.Termination_Notice( self ) ;
        end ;
        Terminate := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
        _User_Interface.Free ;
        _User_Interface := nil ;
    end ;
    Free ;
end ; { TVDM1_Screen.Terminate }



function TVDM1_Screen.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Clear_Watchpoint := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
    if( Memory and ( _User_Interface._Low <= Address ) and ( _User_Interface._High >= Address ) ) then
    begin
        Result := _User_Interface.Watchpoint_List.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( TVDM1ScreenErr_No_Matching_Watchpoint ) ;
    end ;
end ;


function TVDM1_Screen.Component_Type : longint ;

begin
    Component_Type := Component_Type_UI ;
end ;


function TVDM1_Screen.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _User_Interface.Set_Error( TVDM1ScreenErr_Invalid_Component ) ;
        exit ;
    end ;
    Default_Input := Component ;
    Connect_Input := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
end ;


function TVDM1_Screen.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _User_Interface.Set_Error( TVDM1ScreenErr_Invalid_Component ) ;
        exit ;
    end ;
    Default_Output := Component ;
    Connect_Output := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
end ;


function TVDM1_Screen.Debugger : TDebug_Interface ;

begin
    Result := TVDM1_Screen_Debugger.Create ;
    TVDM1_Screen_Debugger( Result ).Memory := Self ;
end ;


function TVDM1_Screen.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count : integer ;

begin
    if( ( not Memory ) or ( _User_Interface._Low > Address ) or ( _User_Interface._High < Address ) ) then
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
        _User_Interface.Image.Data[ Address - _User_Interface._Low ] := ord( _Buffer[ 0 ] ) ;
        _User_Interface.Image.Dirty[ Address - _User_Interface._Low ] := True ;
        _User_Interface.Image.Need_Update := True ;
        _User_Interface.Update_Thread.Resume ;
        _Buffer := _Buffer + 1 ;
        dec( Count ) ;
        inc( Address ) ;
    end ; { while( Count > 0 ) }
    Deposit := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
end ; { TVDM1_Screen.Deposit }


function TVDM1_Screen.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component <> Default_Input ) or ( Component = nil ) ) then
    begin
	Disconnect_Input := _User_Interface.Set_Error( TVDM1ScreenErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
	Default_Input := nil ;
    end ;
end ;


function TVDM1_Screen.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component <> Default_Output ) or ( Component = nil ) ) then
    begin
	Disconnect_Output := _User_Interface.Set_Error( TVDM1ScreenErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
	Default_Output := nil ;
    end ;
end ;


function TVDM1_Screen.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count : integer ;

begin
    if( ( not Memory ) or ( _User_Interface._Low > Address ) or ( _User_Interface._High < Address ) ) then
    begin
        Examine := _User_Interface.Set_Error( TVDM1ScreenErr_Address_Out_Of_Range ) ;
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
            _Buffer[ 0 ] := char( _User_Interface.Image.Data[ Address - _User_Interface._Low ] ) ;
            _Buffer := _Buffer + 1 ;
            dec( Count ) ; { One less byte to read }
            inc( Address ) ;
        end ; { while( Count > 0 ) }
    except
    end ;
    Examine := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
end ; { TVDM1_Screen.Examine }


function TVDM1_Screen.Get_Access_Mode( Address : int64 ;
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


function TVDM1_Screen.Get_Profiling : boolean ;

begin
    Get_Profiling := _User_Interface.Profiling ;
end ;


function TVDM1_Screen.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := Read_Latency ;
end ;


function TVDM1_Screen.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := Write_Latency ;
end ;


function TVDM1_Screen.Input_Component( Index : longint ) : TComponent ;

begin
    if( Index = 0 ) then
    begin
	Input_Component := Default_Input ;
    end else
    begin
	Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'VDM1 Screen'#0 ;

function TVDM1_Screen.Name : PChar ;

begin
    Name := _Name ;
end ;


function TVDM1_Screen.Output_Component( Index : longint ) : TComponent ;

begin
    if( Index = 0 ) then
    begin
	Output_Component := Default_Output ;
    end else
    begin
	Output_Component := nil ;
    end ;
end ;


function TVDM1_Screen.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

var Buffer : PChar ;
    Count : integer ;
    Watch : TCEF_Watchpoint ;

begin
    if( ( Address >= _User_Interface._Low ) and ( Address <= _User_Interface._High ) and ( IO_Type = IO_Type_Memory ) ) then
    begin
        if( ( Access_Mode and Access_Read ) = 0 ) then
        begin
            Read := False ; { TVDM1ScreenErr_Access_Violation }
            exit ;
        end ;
	Read := True ;
	if( _User_Interface._UI.Clock <> nil ) then
        begin
            _User_Interface._UI.Clock.Block( self, Read_Latency ) ;
        end ;
        if( Size = 0 ) then
        begin
            Count := 1 ; // Default6 is 1 byte
        end else
        begin
            Count := ( Size + 7 ) div 8 ; // Round to byte
        end ;
	if( Default_Output <> nil ) then
	begin
	    getmem( Buffer, Count) ;
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
end ; { TVDM1_Screen.Read }


function TVDM1_Screen.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

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


function TVDM1_Screen.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var S : longint ;

begin
    Result := nil ;
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


function TVDM1_Screen.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

var Loop : int64 ;
    High : int64 ;
    S : longint ;
    Value : integer ;

begin { TVDM1_Screen.Save_Contents }
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
end ; { TVDM1_Screen.Save_Contents }


function TVDM1_Screen.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Stream.Write( _User_Interface._Low, sizeof( _User_Interface._Low ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( _User_Interface._High, sizeof( _User_Interface._High ) ) ;
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


function TVDM1_Screen.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    if( Memory and ( _User_Interface._Low <= Low ) and ( _User_Interface._High >= High ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
end ;


procedure TVDM1_Screen.Set_Profiling( _On, Children : boolean ) ;

begin
    _User_Interface.Profiling := _On ;
end ;


procedure TVDM1_Screen.Set_Read_Latency( Value : longint ) ;

begin
    Read_Latency := Value ;
end ;


function TVDM1_Screen.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Watchpoint := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
    if( Memory and ( _User_Interface._Low <= Address ) and ( _User_Interface._High >= Address ) ) then
    begin
        _User_Interface.Watchpoint_List.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TVDM1_Screen.Set_Write_Latency( Value : longint ) ;

begin
    Write_Latency := Value ;
end ;


procedure TVDM1_Screen.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TVDM1_Screen.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var B : integer ;
    Watch : TCEF_Watchpoint ;

begin
    Write := _User_Interface.Set_Error( TVDM1ScreenErr_Success ) ;
    if( ( IO_Type = IO_Type_IO ) and ( Address = $C8 ) ) then
    begin
        _User_Interface.Image.Top_Line := ( Value shr 4 ) and 15 ; // shr 4 = FDSP = First displayed line screen position
{
        and 15 = BDLA = Beginning display line absolute address
}
    end else
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
            Write := _User_Interface.Set_Error( TVDM1ScreenErr_Access_Violation ) ;
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
        _User_Interface.Update_Thread.Resume ;
    end ;
end ; // TVDM1_Screen.Write


procedure TVDM1_Screen.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TVDM1_Screen.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TVDM1_Screen.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TVDM1_Screen.Set_Parent( Component : TComponent ) ;

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


procedure TVDM1_Screen.Set_Up( P : PChar ) ;

var C : byte ;
    Loop : integer ;
    Parser : TString_Parser ;
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
        if( S = 'CLEAR' ) then
        begin
            _User_Interface.Image.Clear_Memory ;
        end else
        if( S = 'CONTROL' ) then
        begin
            _User_Interface.Image.Display_Control := True ;
        end else
        if( S = 'CURSOR' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'NONE' ) then
            begin
                _User_Interface.Image.Cursor_Mode := 0 ;
            end else
            if( S = 'BLINK' ) then
            begin
                _User_Interface.Image.Cursor_Mode := 1 ;
            end else
            begin
                _User_Interface.Image.Cursor_Mode := 2 ;
            end ;
        end else
        if( S = 'FONT' ) then
        begin
            S := uppercase( Parser.Token ) ;
            _User_Interface.Image.Load( PChar( S ) ) ;
        end else
        if( S = 'PORT' ) then
        begin
            S := uppercase( Parser.Token ) ;
            try
                _User_Interface._Port := Convert_Value( S ) ;
            except
            end ;
        end else
        if( S = 'TOP' ) then
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
            _User_Interface.Image.Top_Line := Convert_Value( S ) ;
            if( _User_Interface.Image.Top_Line < 0 ) then
            begin
                _User_Interface.Image.Top_Line := 0 ;
            end ;
            if( _User_Interface.Image.Top_Line > 15 ) then
            begin
                _User_Interface.Image.Top_Line := 15 ;
            end ;
        end else
        if( S = 'INVERT' ) then
        begin
            _User_Interface.Image.Invert := True ;
        end else
        if( S = 'NOCONTROL' ) then
        begin
            _User_Interface.Image.Display_Control := False ;
        end else
        if( S = 'NOPARALLEL' ) then
        begin
            _User_Interface.Threaded := False ;
        end else
        if( S = 'NORMAL' ) then
        begin
            _User_Interface.Image.Invert := False ;
        end else
        if( S = 'PARALLEL' ) then
        begin
            _User_Interface.Threaded := True ;
            if( _User_Interface.Update_Thread.ID = -1 ) then
            begin
                _User_Interface.Update_Thread.ID :=
                    _User_Interface._UI.Process_ID( PChar( VDM1_Thread_Name ), True ) ;
            end ;
        end else
        if( S = 'TEST' ) then // Test pattern
        begin
            C := 0 ;
            for Loop := _User_Interface._Low to _User_Interface._High do
            begin
                Deposit( Loop, 1, @C, True ) ;
                inc( C ) ;
            end ;
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
    _User_Interface._High := _User_Interface._Low + 16 * 64 ;
    Parser.Free ;
    _User_Interface.Update_Thread.Resume ;
end ; // TVDM1_Screen.Set_Up


function TVDM1_Screen.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TVDM1_Screen.Set_Logger( Value : TCEF_Logger ) ;

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


function TVDM1_Screen_UI.Get_Hidden : boolean ;

begin
    Result := not Our_Screen.Visible ;
end ;


procedure TVDM1_Screen_UI.Set_Hidden( Value : boolean ) ;

begin
    Our_Screen.Visible := not Value ;
end ;


function TVDM1_Screen_UI.Get_Parent_Window : THandle ;

begin
    Result := Our_Screen.ParentWindow ;
end ;


procedure TVDM1_Screen_UI.Set_Parent_Window( Value : THandle ) ;

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
            Our_Screen.BorderIcons := [biSystemMenu, biMinimize, biMaximize] ;
            Our_Screen.Borderstyle := bsSizeable ;
        end else
        begin
            Top := Our_Screen.Top ;
            Left := Our_Screen.Left ;
            Height := Our_Screen.Height ;
            Width := Our_Screen.Width ;
            Our_Screen.ParentWindow := Value ;
            Our_Screen.Caption := '' ;
            Our_Screen.BorderIcons := [] ;
            Our_Screen.Borderstyle := bsNone ;
        end ;
    end ;
end ;


function TVDM1_Screen_UI.Get_Caption : PChar ;

begin
    Result := PChar( Caption ) ;
end ;


procedure TVDM1_Screen_UI.Set_Caption( Value : PChar ) ;

begin
    Caption := string( Value ) ;
end ;


procedure TVDM1_Screen_UI.Set_Size( Height, Width : integer ) ;

begin
    Our_Screen.Height := Height + GetSystemMetrics( SM_CXBORDER ) + GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Width := Width + GetSystemMetrics( SM_CXBORDER ) * 2 ;
//    Our_Screen.Top := -GetSystemMetrics( SM_CYCAPTION ) ;
    Our_Screen.Left := -GetSystemMetrics( SM_CXBORDER ) ;
end ;



{$IFDEF Test}

{ All following code is for testing the CEF Memory class... }

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
    Test : TVDM1_Screen ;
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
    Test := TVDM1_Screen.Create ;
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
    if( Test.Name <> 'CEF Generic memory' ) then
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

