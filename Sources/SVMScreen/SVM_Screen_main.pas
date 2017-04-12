{
        Program Name : SVMScreen_Main
        Package Name : CEF
        Purpose      : Simple terminal component main form
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Dec-2014
        Written By   : Alan Conroy
        Version      : 1.0

	Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

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

        This is the main form for the CEF SVM user I/O.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit SVM_Screen_Main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     TPanels, // TTransparent_Panel

     // CEF...
     CEF, // TCable
     Serial_Cable ; // TSerial_Cable

type TMain_Form = class( TForm )
                    Screen_Panel: TPanel;
                    StatusBar1: TStatusBar;
                    procedure FormCreate(Sender: TObject);
                    procedure BufferKeyDown(Sender: TObject; var Key: Word;
                      Shift: TShiftState);
                    procedure BufferKeyUp(Sender: TObject; var Key: Word;
                      Shift: TShiftState);
                    procedure BufferKeyPress(Sender: TObject; var Key: Char);
                    procedure FormKeyDown(Sender: TObject; var Key: Word;
                      Shift: TShiftState);
                    procedure FormDestroy(Sender: TObject);
                    procedure FormClose(Sender: TObject; var Action: TCloseAction);

                  private // Instance data...
                      Screen : TTransparent_Panel ;

                  protected // Callbacks...
                      procedure CB_Paint_Screen( Sender : Tobject ) ;

                      procedure WMGetDlgCode( var Message : TMessage ) ;
                          message WM_GetDlgCode ;

                  private // Internal utility routines...
                      procedure Update_Status ;
                      procedure _Write( Value : longint ) ;

                  public // API...
                      CAD_State : integer ; // VT05 CAD state
                      CAD_Y : integer ; // VT05 CAD Y value

                      Auto_Copy : boolean ;
                      _Ignore_Control : boolean ; // True to ignore non-cursor control characters
                      Cursor_Rate : cardinal ; // Cursor cycle time in ms
                      _Scroll : boolean ; // True to scroll screen when cursor goes off bottom
                      Cursor_Row : integer ;
                      Cursor_Column : integer ;
                      KB_Receiver : TComponent ;
                      _Line_Wrap : boolean ;
                      _Uppercase_Out : boolean ; // True if to transmit only uppercase alphas
                      _Uppercase_In : boolean ; // True if to display only uppercase alphas
                      _UI : TUI_Interface ;
                      _SVMScreen : TComponent ;
                      _Margin_Bell : boolean ;
                      Cable_Component : TComponent ;

                      procedure New_Char( Value : integer ) ;
                      procedure Receive_Char( Value : longint ) ;
                      procedure Write_Serial( Value : integer ) ;
                  end ; // TMain_Form

var Pending_UI : TUI_Interface = nil ;

implementation

uses // Borland...
     Clipbrd, // Clipboard

     // C&C...
     ASCIIDef, // ESSC
     SMU, // Output_Text
     Standard, // ERT
     _UEHDEFs, // TUEC
     UStrings, // Extract

     // SVMScreen...
     SVM_Screen ; // _UI

{$R *.dfm}

// TMain_Form methods...

procedure TMain_Form.Update_Status ;

begin
    StatusBar1.Panels[ 0 ].Text := 'Row ' + inttostr( Cursor_Row ) ;
    StatusBar1.Panels[ 1 ].Text := 'Col ' + inttostr( Cursor_Column ) ;
end ;


procedure TMain_Form._Write( Value : longint ) ;

begin
    _SVMScreen.Cable.Transmit( 0, Value, 8, 1 ) ;
end ;


procedure TMain_Form.New_Char( Value : integer ) ;

begin
    if( not SMU_Initialized ) then
    begin
        Select_Window( Create_Window_Wrapper( Screen.Handle ) ) ; // Initializes SMU and assigns form as default window
    end ;

    Output_Text( chr( Value ) ) ;
    Update_Status ;
end ; // TMain_Form.New_Char


procedure TMain_Form.Receive_Char( Value : longint ) ;

begin
    New_Char( Value ) ;
end ;


procedure TMain_Form.Write_Serial( Value : integer ) ;

begin
    _SVMScreen.Set_Signal( 'BREAK', False ) ;
    if( _Uppercase_Out ) then
    begin
        Value := ord( upcase( chr( Value ) ) ) ;
    end ;
    _Write( Value ) ;
end ;


procedure TMain_Form.WMGetDlgCode( var Message : TMessage ) ;

begin
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS or DLGC_WANTTAB ;
end ;


// Property handlers...

// Callbacks...

procedure TMain_Form.CB_Paint_Screen( Sender : Tobject ) ;

begin
    Refresh_Screen ;
end ; // TMain_Form.CB_Paint_Screen


procedure TMain_Form.FormCreate( Sender : TObject ) ;

begin
    // Setup screen...
    Cursor_Row := 1 ;
    Cursor_Column := 1 ;
    Screen := TTransparent_Panel.Create( self ) ;
    Screen.Parent := Screen_Panel ;
    Screen.Align := alClient ;
    Screen.OnPaint := CB_Paint_Screen ;
end ; // TMain_Form.FormCreate


procedure TMain_Form.BufferKeyDown( Sender : TObject ; var Key : Word ;
    Shift : TShiftState ) ;

var _Buffer : array[ 0..2 ] of char ;
    Defined : boolean ;
    KeyStates : TKeyboardState ;

    procedure Substitute( CB : string ) ;

    var S : string ;

    begin
        S := CB ;
        if( S <> '' ) then
        begin
            Defined := True ;
            if( length( S ) = 1 ) then
            begin
                Key := ord( S[ 1 ] ) ;
            end else
            if( S = 'SPACE' ) then
            begin
                Key := 32 ;
            end else
            if( S = 'CR' ) then
            begin
                Key := 13 ;
            end else
            if( S = 'DEL' ) then
            begin
                Key := 127 ;
            end else
            if( S = 'LF' ) then
            begin
                Key := 10 ;
            end else
            if( S = 'ESC' ) then
            begin
                Key := ord( ESC ) ;
            end else
            if( S = 'TAB' ) then
            begin
                Key := 9 ;
            end else
            if( S = 'SHIFT LOCK' ) then
            begin
                Key := 0 ;
                exit ;
            end ;
        end ; // if( S <> '' )
    end ; // .Substitute

begin
    Defined := False ;
    case Key of
        VK_BACK :
            begin
                Key :=  $5F ;
                Defined := True ;
            end ;
        VK_F1 : Substitute( 'F1' ) ;
        VK_F2 : Substitute( 'F2' ) ;
        VK_F3 : Substitute( 'F3' ) ;
        VK_F4 : Substitute( 'F4' ) ;
        VK_F5 : Substitute( 'F5' ) ;
        VK_F6 : Substitute( 'F6' ) ;
        VK_F7 : Substitute( 'F7' ) ;
        VK_F8 : Substitute( 'F8' ) ;
        VK_F9 : Substitute( 'F9' ) ;
        VK_F10 : Substitute( 'F10' ) ;
        VK_F11 : Substitute( 'F11' ) ;
        VK_F12 : Substitute( 'F12' ) ;
        VK_CANCEL : Substitute( 'CANCEL' ) ;
        VK_CLEAR : Substitute( 'CLEAR' ) ;
        VK_PAUSE : Substitute( 'PAUSE' ) ;
        VK_PRIOR : Substitute( 'PAGE_UP' ) ;
        VK_NEXT : Substitute( 'PAGE_DOWN' ) ;
        VK_END : Substitute( 'END_CB' ) ;
        VK_PRINT : Substitute( 'PRINT' ) ;
        VK_INSERT : Substitute( 'INSERT' ) ;
    end ;
    if( not Defined ) then
    begin
        GetKeyboardState( KeyStates ) ;
        if( ToAscii( Key, MapVirtualKey( Key, 0 ), KeyStates, _Buffer, 0 ) = 1 ) then
        begin
            Key := ord( _Buffer[ 0 ] ) ;
        end else
        begin
            case Key of
                VK_UP : Key := $97 ;
                VK_DOWN : Key := $9A ;
                VK_LEFT : Key := $81 ;
                VK_RIGHT : Key := $93 ;
                VK_TAB : Key := 9 ;
                VK_ESCAPE : Key := ord( ESC ) ;
                VK_RETURN : Key := ord( CR ) ;
                VK_HOME : Key := $8E ;
                VK_DELETE : Key := $5F ;
                VK_BACK : Key := $5F ;
                else exit ;
            end ;
        end ;
    end ;

    Key := 0 ;
end ; // TMain_Form.BufferKeyDown


procedure TMain_Form.BufferKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);

begin
    Key := 0 ;
end ;


procedure TMain_Form.BufferKeyPress(Sender: TObject; var Key: Char);

begin
    Key := #0 ;
end ;


procedure TMain_Form.FormKeyDown( Sender : TObject ; var Key : Word ;
    Shift : TShiftState ) ;

var _Buffer : array[ 0..2 ] of char ;
    Defined : boolean ;
    KeyStates : TKeyboardState ;

begin
    Defined := False ;
    GetKeyboardState( KeyStates ) ;
    if( ToAscii( Key, MapVirtualKey( Key, 0 ), KeyStates, _Buffer, 0 ) = 1 ) then
    begin
        Key := ord( _Buffer[ 0 ] ) ;
    end else
    begin
        case Key of
            VK_TAB : Key := 9 ;
            VK_ESCAPE : Key := ord( ESC ) ;
            VK_RETURN : Key := ord( CR ) ;
            VK_DELETE : Key := 127 ;
            VK_BACK : Key := 127 ;
            VK_UP :
                begin
                    Key := 0 ;
                end ;
            VK_DOWN :
                begin
                    Key := 0 ;
                end ;
            VK_LEFT :
                begin
                    New_Char( 8 ) ;
                    Key := 0 ;
                end ;
            VK_RIGHT :
                begin
                    Key := 0 ;
                end ;
            else exit ;
        end ;
    end ;

    if( ( ( Key > 0 ) and ( Key < 27 ) )
        or
        ( pos( chr( Key ), '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!@#$%^&*()-=_+\|[]{};:<>,./?`~" '#39#$97#$9A#$81#$93#9#13#$8E#$5F+ESC ) > 0 )
      ) then
    begin
        Write_Serial( Key ) ;
        Key := 0 ;
    end ;
end ; // TMain_Form.FormKeyDown


procedure TMain_Form.FormDestroy( Sender : TObject ) ;

begin
    KB_Receiver.Free ;
    KB_Receiver := nil ;
end ;


procedure TMain_Form.FormClose( Sender : TObject ; var Action : TCloseAction ) ;

begin
    Action := caFree ;
end ;


end.
