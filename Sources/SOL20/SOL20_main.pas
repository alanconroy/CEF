{
        Program Name : SOL20_Main
        Package Name : CEF
        Purpose      : Processor Technology SOL-20 component main form
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Apr-2006
        Written By   : Alan Conroy
        Version      : 1.0A

	Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

         20-Jan-2007    EAC         Handle shutdown better.        

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

        This is the main form for the SOL-20 Emulator for CEF.  It encapsulates
        the screen and keyboard.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit SOL20_Main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // CEF...
     _CEF, // TMemory
     _CEFUtil ;  // TCEF_Key_Mapper

type
  TMain_Form = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    SerialPort1: TMenuItem;
    Exit1: TMenuItem;
    Baudrate1: TMenuItem;
    Full1: TMenuItem;
    N8databits1: TMenuItem;
    N2stopbits1: TMenuItem;
    Parity1: TMenuItem;
    N3001: TMenuItem;
    N51: TMenuItem;
    N61: TMenuItem;
    N71: TMenuItem;
    N81: TMenuItem;
    N751: TMenuItem;
    N1101: TMenuItem;
    N1501: TMenuItem;
    N6001: TMenuItem;
    N12001: TMenuItem;
    N24001: TMenuItem;
    N48001: TMenuItem;
    N96001: TMenuItem;
    Video1: TMenuItem;
    Cursor1: TMenuItem;
    Display1: TMenuItem;
    DisplayControlCharacters1: TMenuItem;
    Blanking1: TMenuItem;
    None1: TMenuItem;
    Blinking1: TMenuItem;
    Solid1: TMenuItem;
    Options1: TMenuItem;
    Bufferkeystrokes1: TMenuItem;
    Reset1: TMenuItem;
    Edit1: TMenuItem;
    Copyscreen1: TMenuItem;
    Paste1: TMenuItem;
    N1: TMenuItem;
    Screen_Panel: TPanel;
    StatusBar1: TStatusBar;
    Bottom_Panel: TPanel;
    Panel2: TPanel;
    Cassette1: TMenuItem;
    Drive11: TMenuItem;
    ransport21: TMenuItem;
    Tape1_File_Menu: TMenuItem;
    Tape2_File_Menu: TMenuItem;
    Tape1_Unload: TMenuItem;
    Tape2_Unload: TMenuItem;
    OpenDialog1: TOpenDialog;
    Keymapping1: TMenuItem;
    Connect1: TMenuItem;
    Open_Component: TOpenDialog;
    Panel1: TPanel;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    Label2: TLabel;
    SpeedButton75: TSpeedButton;
    Buffer: TEdit;
    ParallelPort1: TMenuItem;
    Debug1: TMenuItem;
    Keyboard1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    procedure Check_Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Screen_PanelResize(Sender: TObject);
    procedure BufferKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BufferKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BufferKeyPress(Sender: TObject; var Key: Char);
    procedure Blinking1Click(Sender: TObject);
    procedure None1Click(Sender: TObject);
    procedure Solid1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Reset1Click(Sender: TObject);
    procedure SpeedButton75Click(Sender: TObject);
    procedure Display1Click(Sender: TObject);
    procedure Blanking1Click(Sender: TObject);
    procedure DisplayControlCharacters1Click(Sender: TObject);
    procedure Copyscreen1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Baud_Click(Sender: TObject);
    procedure Bits_Check(Sender: TObject);
    procedure Tape_Unload(Sender: TObject);
    procedure Tape_File(Sender: TObject);
    procedure Keymapping1Click(Sender: TObject);
    procedure Connect1Click(Sender: TObject);
    procedure ParallelPort1Click(Sender: TObject);
    procedure Debug1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Keyboard1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);

  private // Instance data...
      Keyboard : TComponent ;
      KB_Reciever : TComponent ;
      _Key_Mapper : TCEF_Key_Mapper ;
      Saved_Panel2_Height : integer ;

  protected // Property handlers...
      function _Get_Key_Mapper : TCEF_Key_Mapper ;

  public // API...
      Sense_Switches : array[ 0..7 ] of TSpeedButton ;
      Serial_Port : TComponent ;
      Serial_Component : TComponent ;
      Parallel_Port : TComponent ;
      Parallel_Component : TComponent ;
      _SOL20 : TMemory ;
      
      procedure Write_Serial( Value : integer ) ;

      procedure Write_Parallel( Value : integer ) ;

      property Key_Mapper : TCEF_Key_Mapper
          read _Get_Key_Mapper
          write _Key_Mapper ;
  end ;

var _UI : TUI_Interface = nil ;

implementation

uses // Borland...
     Clipbrd, // Clipboard

     // C&C...
     _ASCII, // ESC
     Standard, // ERT
     _UE, // TUnified_Exception
     UE, // DOS_ERT

     // CEF...
     CEF, // TBase_Component

     // SOL20...
     AboutBox, // About_Form
     SOL_20 ; // Screen

{$R *.dfm}

function Get_Key_Mapper : TCEF_Key_Mapper ; stdcall ; external 'CEF_Util.dll' ;

type TKBComponent = class( TBase_Component )
                        public
                            Main_Form : TMain_Form ;

                            function Write( Address : int64 ;
                                Value, Size : longint ;
                                IO_Type : longint ) : TUnified_Exception ; override ;
                    end ;

function TKBComponent.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var P : PChar ;
    S : string ;

begin
    Result := Set_Error( 0 ) ;
    if( Value = 1 ) then // Key down
    begin
        P := Main_Form.Keyboard.Keyboard.Get_Key ;
        while( P <> nil ) do
        begin
            S := string( P ) ;
            if( length( S ) = 1 ) then
            begin
                Main_Form.Write_Serial( ord( S[ 1 ] ) ) ;
            end else
            if( S = 'UP' ) then
            begin
                Main_Form.Write_Serial( $97 ) ;
            end else
            if( S = 'DOWN' ) then
            begin
                Main_Form.Write_Serial( $9A ) ;
            end else
            if( S = 'LEFT' ) then
            begin
                Main_Form.Write_Serial( $81 ) ;
            end else
            if( S = 'RIGHT' ) then
            begin
                Main_Form.Write_Serial( $93 ) ;
            end else
            if( S = 'DIVIDE' ) then
            begin
                Main_Form.Write_Serial( $2F ) ;
            end else
            if( S = 'CLEAR' ) then
            begin
                Main_Form.Write_Serial( $8B ) ;
            end else
            if( S = 'HOME CURSOR' ) then
            begin
                Main_Form.Write_Serial( $8E ) ;
            end else
            if( S = 'LOAD' ) then
            begin
                Main_Form.Write_Serial( $8C ) ;
            end else
            if( S = 'MODE SELECT' ) then
            begin
                Main_Form.Write_Serial( $80 ) ;
            end else
            if( S = 'BREAK' ) then
            begin
                if(
                    ( Main_Form.Serial_Port <> nil )
                  ) then // In local mode with a serial port
                begin
                    Main_Form.Serial_Port.Set_Signal( 'BREAK', True ) ;
                end ;
                exit ;
            end ;
            // Ignore all other keys
            P := Main_Form.Keyboard.Keyboard.Get_Key ;
        end ; // while( P <> nil )
    end ; // if( Value = 1 )
end ; // TKBComponent.Write



type TKey_Button = class( TSpeedButton )
                       public
                           LED : TShape ;
                           LED_Position : TPoint ;

                           procedure Paint ; override ;

                       published
                           property Color ;
                   end ;

                   
// TMain_Form methods...

// Property handlers...

function TMain_Form._Get_Key_Mapper : TCEF_Key_Mapper ;

var Loop : integer ;
    S : string ;

begin
    if( _Key_Mapper = nil ) then
    begin
        _Key_Mapper := Get_Key_Mapper ;
        Key_Mapper.Add_Key( 'F1' ) ;
        Key_Mapper.Add_Key( 'F2' ) ;
        Key_Mapper.Add_Key( 'F3' ) ;
        Key_Mapper.Add_Key( 'F4' ) ;
        Key_Mapper.Add_Key( 'F5' ) ;
        Key_Mapper.Add_Key( 'F6' ) ;
        Key_Mapper.Add_Key( 'F7' ) ;
        Key_Mapper.Add_Key( 'F8' ) ;
        Key_Mapper.Add_Key( 'F9' ) ;
        Key_Mapper.Add_Key( 'F10' ) ;
        Key_Mapper.Add_Key( 'F11' ) ;
        Key_Mapper.Add_Key( 'F12' ) ;
        Key_Mapper.Add_Key( 'CANCEL' ) ;
        Key_Mapper.Add_Key( 'CLEAR' ) ;
        Key_Mapper.Add_Key( 'PAUSE' ) ;
        Key_Mapper.Add_Key( 'PAGE UP' ) ;
        Key_Mapper.Add_Key( 'PAGE DOWN' ) ;
        Key_Mapper.Add_Key( 'END' ) ;
        Key_Mapper.Add_Key( 'PRINT' ) ;
        Key_Mapper.Add_Key( 'INSERT' ) ;
        for Loop := 33 to 126 do
        begin
            if( Upcase( char( Loop ) ) = char( Loop ) ) then
            begin
                S := char( Loop ) ;
                Key_Mapper.Add_Mapping( PChar( S ) ) ;
            end ;
        end ;
        Key_Mapper.Add_Mapping( 'SPACE' ) ;
        Key_Mapper.Add_Mapping( 'TAB' ) ;
        Key_Mapper.Add_Mapping( 'ESCAPE' ) ;
        Key_Mapper.Add_Mapping( 'RETURN' ) ;
        Key_Mapper.Add_Mapping( 'LOAD' ) ;
        Key_Mapper.Add_Mapping( 'LINE FEED' ) ;
        Key_Mapper.Add_Mapping( 'MODE SELECT' ) ;
        Key_Mapper.Add_Mapping( 'DIVIDE' ) ;
        Key_Mapper.Add_Mapping( 'HOME' ) ;
        Key_Mapper.Add_Mapping( 'CLEAR' ) ;
        Key_Mapper.Add_Mapping( 'DEL' ) ;
        Key_Mapper.Add_Mapping( 'BREAK' ) ;
    end ;
    Result := _Key_Mapper ;
end ;


// API...

procedure TMain_Form.Write_Serial( Value : integer ) ;

var Stops : integer ;

begin
    if( Bufferkeystrokes1.Checked ) then
    begin
        Buffer.Text := Buffer.Text + chr( Value ) ;
    end else
    begin
        Buffer.Text := chr( Value ) ;
    end ;
    if( Keyboard.Keyboard.Get_LED_State( 'LOCAL' ) ) then // In local mode
    begin
        Buffer.Text := '' ;
        TSOL20_System( _SOL20 ).UART_Data := Value ;
        if( TSOL20_System( _SOL20 ).UART_Data_Ready ) then
        begin
            TSOL20_System( _SOL20 ).UART_Overrun := True ;
        end ;
        TSOL20_System( _SOL20 ).UART_Data_Ready := True ;
        exit ;
    end ;
    if( Serial_Port <> nil ) then
    begin
        if( N2stopbits1.Checked ) then
        begin
            Stops := 2 ;
        end else
        begin
            Stops := 1 ;
        end ;
        Serial_Port.Cable.Transmit( SP_Baud, Value, SP_Data_Bits, Stops ) ;
    end ;
end ;


procedure TMain_Form.Write_Parallel( Value : integer ) ;

begin
    if( Parallel_Port <> nil ) then
    begin
        Parallel_Port.Cable.Transmit( 0, Value, 0, 0 ) ;
    end ;
end ;


procedure TMain_Form.Check_Click( Sender : TObject ) ;

begin
    TMenuItem( Sender ).Checked := not TMenuItem( Sender ).Checked ;
end ;


procedure TMain_Form.FormCreate( Sender : TObject ) ;

    function Update_Key( Original : TSpeedButton ; Color : TColor ) : TKey_Button ;

    begin
        Result := TKey_Button.Create( Original.Owner ) ;
        Result.Parent := Original.Parent ;
        Result.AllowAllUp := Original.AllowAllUp ;
        Result.Left := Original.Left ;
        Result.Top := Original.Top ;
        Result.Width := Original.Width ;
        Result.Height := Original.Height ;
        Result.Caption := Original.Caption ;
        Result.OnClick := Original.OnClick ;
        Result.Color := Color ;
        Result.Font := Original.Font ;
        Result.Font.Color := Original.Font.Color ;
        Result.Spacing := Original.Spacing ;
        Result.Glyph := Original.Glyph ;
        Result.GroupIndex := Original.GroupIndex ;
        Original.Glyph := nil ;
        Original.Free ;
    end ;

begin
    // General setup...
    Sense_Switches[ 0 ] := SpeedButton1 ;
    Sense_Switches[ 1 ] := SpeedButton2 ;
    Sense_Switches[ 2 ] := SpeedButton3 ;
    Sense_Switches[ 3 ] := SpeedButton4 ;
    Sense_Switches[ 4 ] := SpeedButton5 ;
    Sense_Switches[ 5 ] := SpeedButton6 ;
    Sense_Switches[ 6 ] := SpeedButton7 ;
    Sense_Switches[ 7 ] := SpeedButton8 ;

    // Setup screen...
    Screen := _UI.Load_Component( 'VDM1' ) ;
    if( Screen = nil ) then
    begin
        ShowMessage( 'Screen initialization failure' ) ;
        exit ;
    end ;
    Screen.User_Interface.Set_Parent_Window( Screen_Panel.Handle ) ;
    Screen_PanelResize( Screen_Panel ) ;

    // Setup keyboard...
    Keyboard := _UI.Load_Component( 'SOL20_KB' ) ;
    if( Keyboard = nil ) then
    begin
        ShowMessage( 'Keyboard initialization failure' ) ;
        exit ;
    end ;
    Keyboard.User_Interface.Set_Parent_Window( Panel2.Handle ) ;

    KB_Reciever := TKBComponent.Create ;
    TKBComponent( KB_Reciever ).Main_Form := self ;
    Keyboard.Connect_Output( KB_Reciever ) ;
end ; // TMain_Form.FormCreate


procedure TKey_Button.Paint ;

    procedure Draw_Glyph( const GlyphPos : TPoint ) ;

    begin
        if( ( Glyph = nil ) or ( Glyph.Width = 0 ) or ( Glyph.Height = 0 ) ) then
        begin
            Exit ;
        end ;
        Canvas.Draw( GlyphPos.X, GlyphPos.Y, Glyph ) ;
    end ;


    procedure Calculate_Layout( const Client : TRect ; const Offset : TPoint ;
        var GlyphPos : TPoint ) ;

    var TextPos : TPoint ;
        ClientSize, GlyphSize, TextSize : TPoint ;
        TotalSize : TPoint ;
        TextBounds : TRect ;

    begin
      { calculate the item sizes }
      ClientSize := Point( Client.Right - Client.Left, Client.Bottom - Client.Top ) ;

      GlyphSize := Point(Glyph.Width div NumGlyphs, Glyph.Height) ;

      TextBounds := Rect( 0, 0, 0, 0 ) ;
      TextSize := Point( 0, 0 );

      { If the layout has the glyph on the right or the left, then both the
        text and the glyph are centered vertically.  If the glyph is on the top
        or the bottom, then both the text and the glyph are centered horizontally.}
      if Layout in [blGlyphLeft, blGlyphRight] then
      begin
          GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2 ;
          TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2 ;
      end else
      begin
          GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2 ;
          TextPos.X := (ClientSize.X - TextSize.X + 1) div 2 ;
      end ;

      Spacing := 0 ;

      { adjust Margin and Spacing }
      if Margin = -1 then
      begin
          TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
            Spacing + TextSize.Y);
          Margin := (ClientSize.X - TotalSize.X + 1) div 2
      end else
      begin
          if Spacing = -1 then
          begin
              TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
                  (Margin + GlyphSize.Y));
              Spacing := (TotalSize.X - TextSize.X) div 2
          end ;
      end ;

      case Layout of
        blGlyphLeft:
          begin
              GlyphPos.X := Margin;
              TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
          end ;
      end ;

      { fixup the result variables }
      with GlyphPos do
      begin
          Inc(X, Client.Left + Offset.X);
          Inc(Y, Client.Top + Offset.Y);
      end ;

      OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
    end ;


    procedure Draw( const Client : TRect ; const Offset : TPoint ) ;

    var GlyphPos: TPoint;

    begin
        Calculate_Layout(Client, Offset, GlyphPos ) ;
        Draw_Glyph( GlyphPos ) ;
    end ;

var PaintRect : TRect ;
    DrawFlags : Integer ;
    Dummy : integer ;
    Offset : TPoint ;
    S1, S2 : string ;

begin
    Canvas.Font := Font ;
    Canvas.Font.Size := Font.Size - 1 ;
    Canvas.Font.Size := Font.Size ;

    PaintRect := Rect( 0, 0, Width, Height ) ;

    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT ;
    if( FState in [ bsDown, bsExclusive ] ) then
    begin
        DrawFlags := DrawFlags or DFCS_PUSHED ;
    end ;
    DrawFrameControl( Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags ) ;

    InflateRect( PaintRect, -1, -1 ) ;
    Canvas.Brush.Color := Color ;
    Canvas.FillRect( PaintRect ) ;
    InflateRect( PaintRect, -1, -1 ) ;

    if( FState in [ bsDown, bsExclusive ] ) then
    begin
        Offset.X := 1 ;
        Offset.Y := 1 ;
    end else
    begin
        Offset.X := 0 ;
        Offset.Y := 0 ;
    end ;
    if( LED <> nil ) then
    begin
        LED.Left := LED_Position.X + Offset.X ;
        LED.Top := LED_Position.Y + Offset.Y ;
    end ;
    if( Caption = '' ) then
    begin
        Draw( PaintRect, Offset ) ;
    end else
    begin
        selectobject( Canvas.Handle, Font.Handle ) ;
        Dummy := pos( #13, Caption ) ;
        if( Dummy > 0 ) then
        begin
            S1 := copy( Caption, 1, Dummy - 1 ) ;
            S2 := copy( Caption, Dummy + 1, length( Caption ) ) ;
            Dummy := ( Width - Canvas.TextWidth( S1 ) ) div 2 ;
            Canvas.TextOut( Dummy + Offset.X,
                ( ( Height div 2 ) - Canvas.TextHeight( S1 ) ) + Offset.Y,
                S1 ) ;
            Dummy := ( Width - Canvas.TextWidth( S2 ) ) div 2 ;
            Canvas.TextOut( Dummy + Offset.X,
                ( Height div 2 ) + Offset.Y, S2 ) ;
        end else
        begin
            Dummy := ( Width - Canvas.TextWidth( Caption ) ) div 2 ;
            Canvas.TextOut( Dummy + Offset.X,
                ( ( Height - Canvas.TextHeight( Caption ) ) div 2 ) + Offset.Y,
                Caption ) ;
        end ;
    end ;
end ; // TKey_Button.Paint


procedure TMain_Form.Screen_PanelResize( Sender : TObject ) ;

begin
    Screen.User_Interface.Set_Size( Screen_Panel.ClientHeight, Screen_Panel.ClientWidth ) ;
end ;


procedure TMain_Form.BufferKeyDown( Sender : TObject ; var Key : Word ;
    Shift : TShiftState ) ;

var _Buffer : array[ 0..2 ] of char ;
    Defined : boolean ;
    KeyStates : TKeyboardState ;

    procedure Substitute( CB : string ) ;

    var S : string ;

    begin
        S := Key_Mapper.Mapping( PChar( CB ) ) ;
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
            if( S = 'DIVIDE' ) then
            begin
            end else
            if( S = 'RETURN' ) then
            begin
                Key := 13 ;
            end else
            if( S = 'DEL' ) then
            begin
                Key := $5F ;
            end else
            if( S = 'LOAD' ) then
            begin
                Key := $8C ;
            end else
            if( S = 'LINE FEED' ) then
            begin
                Key := 10 ;
            end else
            if( S = 'MODE SELECT' ) then
            begin
                Key := $80 ;
            end else
            if( S = 'HOME CURSOR' ) then
            begin
                Key := $8E ;
            end else
            if( S = 'CLEAR' ) then
            begin
                Key := $8B ;
            end else
            if( S = 'LEFT' ) then
            begin
                Key := $81 ;
            end else
            if( S = 'RIGHT' ) then
            begin
                Key := $93 ;
            end else
            if( S = 'UP' ) then
            begin
                Key := $97 ;
            end else
            if( S = 'DOWN' ) then
            begin
                Key := $9A ;
            end else
            if( S = 'ESCAPE' ) then
            begin
                Key := ord( ESC ) ;
            end else
            if( S = 'BREAK' ) then
            begin
                if( Serial_Port <> nil ) then // In local mode with a serial port
                begin
                    Serial_Port.Set_Signal( 'BREAK', True ) ;
                end ;
                exit ;
            end else
            if( S = 'TAB' ) then
            begin
                Key := 9 ;
            end else
            if( S = 'LOCAL' ) then
            begin
                if( Keyboard.Keyboard.Get_LED_State( 'LOCAL' ) ) then // In local mode
                begin
                    Buffer.Text := '' ;
                end ;
                Key := 0 ;
                exit ;
            end else
            if( S = 'UPPER CASE' ) then
            begin
                Key := 0 ;
                exit ;
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
        VK_PRIOR : Substitute( 'PAGE UP' ) ;
        VK_NEXT : Substitute( 'PAGE DOWN' ) ;
        VK_END : Substitute( 'END' ) ;
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

    if( ( ( Key > 0 ) and ( Key < 27 ) )
        or
        ( pos( chr( Key ), '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!@#$%^&*()-=_+\|[]{};:<>,./?`~" '#39#$97#$9A#$81#$93#9#13#$8E#$5F+ESC ) > 0 )
      ) then
    begin
        if( Keyboard.Keyboard.Get_LED_State( 'LOCAL' ) ) then // In local mode
        begin
            if( Key = $8B ) then // Clear
            begin
                Screen.Set_Up( 'CLEAR' ) ;
            end else
            begin
                //~~~
            end ;
            Key := 0 ;
            exit ;
        end ;
        if( Bufferkeystrokes1.Checked ) then
        begin
            Buffer.Text := Buffer.Text + chr( Key ) ;
        end else
        begin
            Buffer.Text := chr( Key ) ;
        end ;
        Key := 0 ;
    end ;
end ;


procedure TMain_Form.BufferKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);

begin
    Key := 0 ;
end ;


procedure TMain_Form.BufferKeyPress(Sender: TObject; var Key: Char);

begin
    Key := #0 ;
end ;


procedure TMain_Form.Blinking1Click( Sender : TObject ) ;

begin
    Blinking1.Checked := True ;
    Screen.Set_Up( 'CURSOR BLINK' ) ;
end ;


procedure TMain_Form.None1Click( Sender : TObject ) ;

begin
    None1.Checked := True ;
    Screen.Set_Up( 'CURSOR NONE' ) ;
end ;


procedure TMain_Form.Solid1Click( Sender : TObject ) ;

begin
    Solid1.Checked := True ;
    Screen.Set_Up( 'CURSOR SOLID' ) ;
end ;


procedure TMain_Form.Exit1Click( Sender : TObject ) ;

begin
    TSOL20_System( _SOL20 )._UI.Terminate ;
end ;


procedure TMain_Form.Reset1Click( Sender : TObject ) ;

var C : TComponent ;
    CPU : TComponent ;
    Loop : integer ;

begin
    TSOL20_System( _SOL20 ).Parent.Reset ;
    CPU := nil ;
    Loop := 0 ;
    C := TSOL20_System( _SOL20 ).Parent.Input_Component( Loop ) ;
    while( C <> nil ) do
    begin
        C.Reset ;
        if( C.Component_Type = Component_Type_CPU ) then
        begin
            CPU := C ;
        end ;
        inc( Loop ) ;
        C := TSOL20_System( _SOL20 ).Parent.Input_Component( Loop ) ;
    end ;
    Loop := 0 ;
    C := TSOL20_System( _SOL20 ).Parent.Output_Component( Loop ) ;
    while( C <> nil ) do
    begin
        C.Reset ;
        if( C.Component_Type = Component_Type_CPU ) then
        begin
            CPU := C ;
        end ;
        inc( Loop ) ;
        C := TSOL20_System( _SOL20 ).Parent.Output_Component( Loop ) ;
    end ;
    if( CPU <> nil ) then
    begin
        CPU.CPU.Set_Current_Address( 0, True, $C000 ) ;
        CPU.CPU.Run ;
    end ;
end ;


procedure TMain_Form.SpeedButton75Click( Sender : TObject ) ;

begin
    Buffer.Text := '' ;
end ;


procedure TMain_Form.Display1Click( Sender : TObject ) ;

begin
    Check_Click( Sender ) ;
    if( Display1.Checked ) then
    begin
        Screen.Set_Up( 'INVERT' ) ;
    end else
    begin
        Screen.Set_Up( 'NORMAL' ) ;
    end ;
end ;


procedure TMain_Form.Blanking1Click( Sender : TObject ) ;

begin
    Check_Click( Sender ) ;
    //~~~
end ;


procedure TMain_Form.DisplayControlCharacters1Click( Sender : TObject ) ;

begin
    Check_Click( Sender ) ;
    if( DisplayControlCharacters1.Checked ) then
    begin
        Screen.Set_Up( 'CONTROL' ) ;
    end else
    begin
        Screen.Set_Up( 'NOCONTROL' ) ;
    end ;
end ;


procedure TMain_Form.Copyscreen1Click( Sender : TObject ) ;

var Loop : integer ;
    S : string ;
    UEC : TUnified_Exception ;

begin
    Loop := 16 * 64 * 8 ; // Rows x Columns x bits/byte
    setlength( S, Loop ) ;
    UEC := Screen.Examine( $CC00, Loop, PChar( S ), True ) ;
    if( UEC = nil ) then
    begin
        Clipboard.AsText := S ;
    end ;
end ;


procedure TMain_Form.Paste1Click( Sender : TObject ) ;

begin
    Buffer.Text := Clipboard.AsText ;
end ;


procedure TMain_Form.Baud_Click( Sender : TObject ) ;

begin
    Check_Click( Sender ) ;
    SP_Baud := strtoint( TMenuItem( Sender ).Caption ) ;
end ;


procedure TMain_Form.Bits_Check( Sender : TObject ) ;

begin
    Check_Click( Sender ) ;
    SP_Data_Bits := strtoint( TMenuItem( Sender ).Caption ) ;
end ;


procedure TMain_Form.Tape_Unload( Sender : TObject ) ;

begin
    if( Sender = Tape1_Unload ) then
    begin
        if( Tape1_Filename <> '' ) then
        begin
            closefile( Tape1_File ) ;
        end ;
        Tape1_Filename := '' ;
        Tape1_File_Menu.Caption := '&File...' ;
    end else
    begin
        if( Tape2_Filename <> '' ) then
        begin
            closefile( Tape2_File ) ;
        end ;
        Tape2_Filename := '' ;
        Tape2_File_Menu.Caption := '&File...' ;
    end ;
end ;


procedure TMain_Form.Tape_File( Sender : TObject ) ;

var Dummy : integer ;

begin
    if( Sender = Tape1_File_Menu ) then
    begin
        OpenDialog1.Filename := Tape1_FileName ;
    end else
    begin
        OpenDialog1.Filename := Tape2_FileName ;
    end ;
    if( OpenDialog1.Execute ) then
    begin
        if( Sender = Tape1_File_Menu ) then
        begin
            Tape_Unload( Tape1_Unload ) ;
            assignfile( Tape1_File, OpenDialog1.Filename ) ;
            if( FileExists( OpenDialog1.Filename ) ) then
            begin
                {$I-}
                reset( Tape1_File, 1 ) ;
                {$I+}
            end else
            begin
                {$I-}
                rewrite( Tape1_File, 1 ) ;
                {$I+}
            end ;
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( DOS_ERT( Dummy ) ) ;
                exit ;
            end ;
            Tape1_Filename := Opendialog1.Filename ;
            Tape1_File_Menu.Caption := '&File... ' + Tape1_Filename ;
        end else
        begin
            Tape_Unload( Tape2_Unload ) ;
            assignfile( Tape2_File, OpenDialog1.Filename ) ;
            if( FileExists( OpenDialog1.Filename ) ) then
            begin
                {$I-}
                reset( Tape2_File, 1 ) ;
                {$I+}
            end else
            begin
                {$I-}
                rewrite( Tape2_File, 1 ) ;
                {$I+}
            end ;
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                exit ;
            end ;
            Tape2_Filename := Opendialog1.Filename ;
            Tape2_File_Menu.Caption := '&File... ' + Tape2_Filename ;
        end ;
    end ;
end ;


procedure TMain_Form.Keymapping1Click( Sender : TObject ) ;

begin
    Key_Mapper.Query ;
end ;


procedure TMain_Form.Connect1Click( Sender : TObject ) ;

var C : TComponent ;

begin
    if( Open_Component.Execute ) then
    begin
        C := TSOL20_System( _SOL20 )._UI.Load_Component( PChar( Open_Component.Filename ) ) ;
        if( C = nil ) then
        begin
            ShowMessage( 'Serial connection initialization failure' ) ;
            exit ;
        end ;
        if( C.Cable = nil ) then
        begin
            C.Free ;
            ShowMessage( 'Component is not a cable type' ) ;
            exit ;
        end ;
        if( Serial_Component <> nil ) then
        begin
            Serial_Component.Free ;
        end ;
        Serial_Component := C ;
        Serial_Port.Connect_Output( Serial_Component ) ;
    end ;
end ;


procedure TMain_Form.ParallelPort1Click(Sender: TObject);

var C : TComponent ;

begin
    if( Open_Component.Execute ) then
    begin
        C := TSOL20_System( _SOL20 )._UI.Load_Component( PChar( Open_Component.Filename ) ) ;
        if( C = nil ) then
        begin
            ShowMessage( 'Parallel connection initialization failure' ) ;
            exit ;
        end ;
        if( C.Component_Type <> Component_Type_Cable ) then
        begin
            C.Free ;
            ShowMessage( 'Component is not a cable type' ) ;
            exit ;
        end ;
        if( Parallel_Component <> nil ) then
        begin
            Parallel_Component.Free ;
        end ;
        Parallel_Component := C ;
        Parallel_Port.Connect_Output( Parallel_Component ) ;
    end ;
end ;


procedure TMain_Form.Debug1Click( Sender : TObject ) ;

begin
    TSOL20_System( _SOL20 )._UI.Hide( True ) ;
end ;


procedure TMain_Form.FormClose( Sender : TObject ; var Action : TCloseAction ) ;

begin
    Action := caFree ;
end ;


procedure TMain_Form.Keyboard1Click( Sender : TObject ) ;

begin
    TMenuItem( Sender ).Checked := not TMenuItem( Sender ).Checked ;
    Panel2.Visible := TMenuItem( Sender ).Checked ;
    if( Panel2.Visible ) then
    begin
        Bottom_Panel.Height := Bottom_Panel.Height + Saved_Panel2_Height ;
        Height := Height + Saved_Panel2_Height ;
    end else
    begin
        Saved_Panel2_Height := Panel2.Height ;
        Bottom_Panel.Height := Bottom_Panel.Height - Saved_Panel2_Height ;
        Height := Height - Saved_Panel2_Height ;
    end ;
end ;


procedure TMain_Form.About1Click( Sender : TObject ) ;

begin
   About_Form.ShowModal ;
end ;


end.
