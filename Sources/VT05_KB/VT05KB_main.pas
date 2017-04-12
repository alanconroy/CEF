{
        Program Name : VT05KB_Main
        Package Name : CEF
        Purpose      : DEC VT05 keyboard
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Dec-2006
        Written By   : Alan Conroy
        Version      : 1.0

	Released to the public domain.

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

          This is the visual portion of the DEC VT05 keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit VT05KB_Main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // CEF...
     _CEF, // TCable
     TPanels ; // TTransparent_Panel

type
  TMain_Form = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    Escape_Button: TSpeedButton;
    Left_Control_Button: TSpeedButton;
    Button_1: TSpeedButton;
    Button_2: TSpeedButton;
    Button_3: TSpeedButton;
    Button_4: TSpeedButton;
    Button_5: TSpeedButton;
    Button_6: TSpeedButton;
    Button_7: TSpeedButton;
    Button_8: TSpeedButton;
    Button_9: TSpeedButton;
    SpeedButton89: TSpeedButton;
    Button_Equal: TSpeedButton;
    Button_Tilde: TSpeedButton;
    Button_Left_Brace: TSpeedButton;
    Button_Vertical: TSpeedButton;
    Button_Right_Brace: TSpeedButton;
    SpeedButton110: TSpeedButton;
    SpeedButton111: TSpeedButton;
    SpeedButton112: TSpeedButton;
    SpeedButton113: TSpeedButton;
    SpeedButton114: TSpeedButton;
    SpeedButton115: TSpeedButton;
    SpeedButton116: TSpeedButton;
    Line_Feed_Button: TSpeedButton;
    SpeedButton119: TSpeedButton;
    SpeedButton120: TSpeedButton;
    SpeedButton121: TSpeedButton;
    SpeedButton122: TSpeedButton;
    Button_Accent: TSpeedButton;
    Return_Button: TSpeedButton;
    SpeedButton125: TSpeedButton;
    Shift_Lock_Button: TSpeedButton;
    Up_Button: TSpeedButton;
    Left_Button: TSpeedButton;
    Left_Shift_Button: TSpeedButton;
    SpeedButton131: TSpeedButton;
    SpeedButton132: TSpeedButton;
    SpeedButton133: TSpeedButton;
    SpeedButton134: TSpeedButton;
    SpeedButton135: TSpeedButton;
    SpeedButton136: TSpeedButton;
    SpeedButton137: TSpeedButton;
    SpeedButton138: TSpeedButton;
    SpeedButton139: TSpeedButton;
    SpeedButton140: TSpeedButton;
    SpeedButton141: TSpeedButton;
    SpeedButton142: TSpeedButton;
    SpeedButton143: TSpeedButton;
    SpeedButton144: TSpeedButton;
    SpeedButton145: TSpeedButton;
    Button_Plus: TSpeedButton;
    Button_Asterisk: TSpeedButton;
    Button_Underscore: TSpeedButton;
    Repeat_Button: TSpeedButton;
    Button_Less_Than: TSpeedButton;
    Button_Greater_Than: TSpeedButton;
    Button_Question_Mark: TSpeedButton;
    Right_Button: TSpeedButton;
    Down_Button: TSpeedButton;
    Space_Button: TSpeedButton;
    Right_Shift_Button: TSpeedButton;
    Break_Button: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure SpeedButton110Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    public // API...
      _UI : TUI_Interface ;
      _VT05KB : TComponent ;
  end ;

implementation

uses // C&C...
     _ASCII, // ESC
     _UE, // TUnified_Exception

     // CEF...
     VT_05_KB ; // _UI

{$R *.dfm}

type TKey_Button = class( TSpeedButton )
                       public
                           procedure Paint ; override ;

                       published
                           property Color ;
                   end ;

                   
// TMain_Form methods...

procedure TMain_Form.SpeedButton110Click( Sender : TObject ) ;

    procedure Add_Key( Name : string ; Value, State : integer ) ;

    begin
        TVT05KB( _VT05KB )._Keyboard.Add_Key( Name, Value, State ) ;
    end ;

var C : char ;
    Dummy : integer;
    Button : TSpeedButton ;
    S : string ;

begin
    Button := TSpeedButton( Sender ) ;
    if( Button = Up_Button ) then
    begin
        Add_Key( 'UP', 130, 1 ) ;
        Add_Key( 'UP', 130, 0 ) ;
        exit ;
    end ;
    if( Button = Down_Button ) then
    begin
        Add_Key( 'DOWN', 131, 1 ) ;
        Add_Key( 'DOWN', 131, 0 ) ;
        exit ;
    end ;
    if( Button = Left_Button ) then
    begin
        Add_Key( 'LEFT', 128, 1 ) ;
        Add_Key( 'LEFT', 128, 0 ) ;
        exit ;
    end ;
    if( Button = Right_Button ) then
    begin
        Add_Key( 'RIGHT', 129, 1 ) ;
        Add_Key( 'RIGHT', 129, 0 ) ;
        exit ;
    end ;
    if( Button = Left_Shift_Button ) then
    begin
        Right_Shift_Button.Down := Left_Shift_Button.Down ;
        if( Right_Shift_Button.Down ) then
        begin
            Add_Key( 'SHIFT', 132, 1 ) ;
        end else
        begin
            Add_Key( 'SHIFT', 132, 0 ) ;
        end ;
        exit ;
    end else
    if( Button = Right_Shift_Button ) then
    begin
        Left_Shift_Button.Down := Right_Shift_Button.Down ;
        if( Right_Shift_Button.Down ) then
        begin
            Add_Key( 'SHIFT', 132, 1 ) ;
        end else
        begin
            Add_Key( 'SHIFT', 132, 0 ) ;
        end ;
        exit ;
    end else
    if( Button.Caption = 'BREAK' ) then
    begin
        Add_Key( 'BREAK', 0, 1 ) ;
        Add_Key( 'BREAK', 0, 0 ) ;
        exit ;
    end else
    if( Button.Caption = 'EEOL' ) then
    begin
        Add_Key( 'EEOL', 135, 1 ) ;
        Add_Key( 'EEOL', 135, 0 ) ;
        exit ;
    end else
    if( Button.Caption = 'EEOS' ) then
    begin
        Add_Key( 'EEOS', 136, 1 ) ;
        Add_Key( 'EEOS', 136, 0 ) ;
        exit ;
    end ;
    if( Button.Caption = 'LOCK' ) then
    begin
        if( Right_Shift_Button.Down ) then
        begin
            Add_Key( 'SHIFT', 132, 0 ) ;
        end ;
        Right_Shift_Button.Down := False ;
        Left_Shift_Button.Down := False ;
        if( Button.Down ) then
        begin
            Add_Key( 'LOCK', 134, 1 ) ;
        end else
        begin
            Add_Key( 'LOCK', 134, 0 ) ;
        end ;
        exit ;
    end ;
    if( Button.Caption = 'CTRL' ) then
    begin
        if( Left_Control_Button.Down ) then
        begin
            Add_Key( 'CONTROL', 133, 1 ) ;
        end else
        begin
            Add_Key( 'CONTROL', 133, 0 ) ;
        end ;
        exit ;
    end ;

    if( Button.Caption = '' ) then // Space bar
    begin
        C := ' ' ;
    end else
    if( length( Button.Caption ) = 1 ) then
    begin
        C := Button.Caption[ 1 ] ;
    end else
    if( Button.Caption = 'TAB' ) then
    begin
        C := HT ;
    end else
    if( Button.Caption = 'ESC' ) then
    begin
        C := ESC ;
    end else
    if( Button.Caption = 'CR' ) then
    begin
        C := CR ;
    end else
    if( Button.Caption = 'LF' ) then
    begin
        C := LF ;
    end else
    begin // If we get here it is a button with multiple glyphs
        S := Button.Caption ;
        Dummy := pos( #13, S ) ;
        if( Right_Shift_Button.Down ) then
        begin
            S := copy( S, 1, Dummy - 1 ) ;
        end else
        begin
            S := copy( S, Dummy + 1, length( S ) ) ;
        end ;
        if( S = '&&' ) then
        begin
            C := '&' ;
        end else
        if( S = 'RUB OUT' ) then
        begin
            C := #127 ;
        end else
        begin
            C := S[ 1 ] ;
        end ;
    end ;

    if(
        ( C >= 'A' ) and ( C <= 'Z' )
        and
        ( not Right_Shift_Button.Down )
      ) then
    begin
        C := lowercase( C )[ 1 ] ;
    end ;
    if( Left_Control_Button.Down ) then
    begin
        C := chr( ord( C ) - 64 ) ;
    end ;

    Add_Key( C, ord( C ), 1 ) ;
    Add_Key( C, ord( C ), 0 ) ;

    if( Right_Shift_Button.Down ) then
    begin
        Add_Key( 'SHIFT', 132, 0 ) ;
    end ;
    Right_Shift_Button.Down := False ;
    Left_Shift_Button.Down := False ;
    if( Left_Control_Button.Down ) then
    begin
        Add_Key( 'CONTROL', 133, 0 ) ;
    end ;
    Left_Control_Button.Down := False ;
end ; // TMain_Form.SpeedButton110Click


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

var Changes : boolean ;
    Loop : integer ;

begin
    // Setup keyboard...
    Button_1.Caption := '!'#13'1' ;
    Button_2.Caption := '"'#13'2' ;
    Button_3.Caption := '#'#13'3' ;
    Button_4.Caption := '$'#13'4' ;
    Button_5.Caption := '%'#13'5' ;
    Button_6.Caption := '&'#13'6' ;
    Button_7.Caption := #39#13'7' ;
    Button_8.Caption := '('#13'8' ;
    Button_9.Caption := ')'#13'9' ;
    Button_Equal.Caption := '='#13'-' ;
    Button_Tilde.Caption := '~'#13'^' ;
    Button_Left_Brace.Caption := '{'#13'[' ;
    Button_Vertical.Caption := '|'#13'\' ;
    Button_Right_Brace.Caption := '}'#13']' ;
    Button_Plus.Caption := '+'#13';' ;
    Button_Asterisk.Caption := '*'#13':' ;
    Button_Underscore.Caption := 'RUB'#13'OUT' ;
    Button_Less_Than.Caption := '<'#13',' ;
    Button_Greater_Than.Caption := '>'#13'.' ;
    Button_Question_Mark.Caption := '?'#13'/' ;
    Shift_Lock_Button.Caption := 'LOCK' ;
    Line_Feed_Button.Caption := 'LF' ;
    Button_Accent.Caption := '`'#13'@' ;

    Escape_Button := Update_Key( Escape_Button, clBlack ) ;
    Left_Control_Button := Update_Key( Left_Control_Button, clBlack ) ;
    Button_1 := Update_Key( Button_1, clBlack ) ;
    Button_2 := Update_Key( Button_2, clBlack ) ;
    Button_3 := Update_Key( Button_3, clBlack ) ;
    Button_4 := Update_Key( Button_4, clBlack ) ;
    Button_5 := Update_Key( Button_5, clBlack ) ;
    Button_6 := Update_Key( Button_6, clBlack ) ;
    Button_7 := Update_Key( Button_7, clBlack ) ;
    Button_8 := Update_Key( Button_8, clBlack ) ;
    Button_9 := Update_Key( Button_9, clBlack ) ;
    Button_Equal := Update_Key( Button_Equal, clBlack ) ;
    Button_Tilde := Update_Key( Button_Tilde, clBlack ) ;
    Button_Left_Brace := Update_Key( Button_Left_Brace, clBlack ) ;
    Button_Vertical := Update_Key( Button_Vertical, clBlack ) ;
    Button_Right_Brace := Update_Key( Button_Right_Brace, clBlack ) ;
    Line_Feed_Button := Update_Key( Line_Feed_Button, clBlack ) ;
    Button_Accent := Update_Key( Button_Accent, clBlack ) ;
    Shift_Lock_Button := Update_Key( Shift_Lock_Button, clBlack ) ;
    Up_Button := Update_Key( Up_Button, clBlack ) ;
    Left_Button := Update_Key( Left_Button, clBlack ) ;
    Left_Shift_Button := Update_Key( Left_Shift_Button, clBlack ) ;
    Button_Plus := Update_Key( Button_Plus, clBlack ) ;
    Button_Asterisk := Update_Key( Button_Asterisk, clBlack ) ;
    Button_Underscore := Update_Key( Button_Underscore, clBlack ) ;
    Button_Less_Than := Update_Key( Button_Less_Than, clBlack ) ;
    Button_Greater_Than := Update_Key( Button_Greater_Than, clBlack ) ;
    Button_Question_Mark := Update_Key( Button_Question_Mark, clBlack ) ;
    Right_Button := Update_Key( Right_Button, clBlack ) ;
    Down_Button := Update_Key( Down_Button, clBlack ) ;
    Right_Shift_Button := Update_Key( Right_Shift_Button, clBlack ) ;
    Break_Button := Update_Key( Break_Button, clBlack ) ;
    Return_Button := Update_Key( Return_Button, clBlack ) ;
    Repeat_Button := Update_Key( Repeat_Button, clBlack ) ;
    Space_Button := Update_Key( Space_Button, clBlack ) ;

    repeat
        Changes := False ;
        for Loop := 0 to Panel3.ControlCount - 1 do
        begin
            if(
                ( Panel3.Controls[ Loop ] is TSpeedButton )
                and
                ( not ( Panel3.Controls[ Loop ] is TKey_Button ) ) // Not already converted
              ) then
            begin
                Update_Key( TSpeedButton( Panel3.Controls[ Loop ] ), clBlack ) ;
                Changes := True ; // Need to run through the loop again
            end ;
        end ;
    until ( not Changes ) ;
end ;


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



procedure TMain_Form.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState) ;

begin
    //~~~
end ;


procedure TMain_Form.FormClose( Sender : TObject ; var Action : TCloseAction ) ;

begin
    Action := caFree ;
end ;


end.
