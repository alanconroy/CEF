{
        Program Name : ASR33KB_Main
        Package Name : CEF
        Purpose      : ASR33 keyboard
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

          This is the visual portion of the ASR33 keyboard component for CEF.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit ASR33KB_main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // TBased_Cable
     TPanels ; // TTransparent_Panel

type
  TMain_Form = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
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
    O_Button: TSpeedButton;
    SpeedButton122: TSpeedButton;
    Return_Button: TSpeedButton;
    SpeedButton125: TSpeedButton;
    Shift_Lock_Button: TSpeedButton;
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
    N_Button: TSpeedButton;
    SpeedButton145: TSpeedButton;
    Button_Plus: TSpeedButton;
    Button_Underscore: TSpeedButton;
    Repeat_Button: TSpeedButton;
    Button_Less_Than: TSpeedButton;
    Button_Greater_Than: TSpeedButton;
    Button_Question_Mark: TSpeedButton;
    Space_Button: TSpeedButton;
    Right_Shift_Button: TSpeedButton;
    Break_Button: TSpeedButton;
    procedure SpeedButton110Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    public // API...
      _UI : TUI_Interface ;
      _ASR33KB : TComponent ;
  end ;

implementation

uses // C&C...
     _ASCII, // ESC
     _UE, // TUnified_Exception

     // CEF...
     ASR_33_KB ; // _UI

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
        TASR33KB( _ASR33KB )._Keyboard.Add_Key( Name, Value, State ) ;
    end ;

var C : char ;
    Dummy : integer;
    Button : TSpeedButton ;
    S : string ;

begin
    // Setup
    Button := TSpeedButton( Sender ) ;

    // Handle keys with special up/down processing...
    if( Button = Left_Shift_Button ) then
    begin
        if( Left_Shift_Button.Down ) then
        begin
            Add_Key( 'LEFT_SHIFT', 128, 1 ) ;
        end else
        begin
            Add_Key( 'LEFT_SHIFT', 128, 0 ) ;
        end ;
        exit ;
    end ;
    if( Button = Right_Shift_Button ) then
    begin
        if( Right_Shift_Button.Down ) then
        begin
            Add_Key( 'RIGHT_SHIFT', 129, 1 ) ;
        end else
        begin
            Add_Key( 'RIGHT_SHIFT', 129, 0 ) ;
        end ;
        exit ;
    end ;
    if( Button.Caption = 'CTRL' ) then
    begin
        if( Left_Control_Button.Down ) then
        begin
            Add_Key( 'CONTROL', 132, 1 ) ;
        end else
        begin
            Add_Key( 'CONTROL', 132, 0 ) ;
        end ;
        exit ;
    end ;

    // Handle other special keys...
    if( Button.Caption = 'BREAK' ) then
    begin
        Add_Key( 'BREAK', 0, 1 ) ;
        Add_Key( 'BREAK', 0, 0 ) ;
        exit ;
    end ;
    if( Button.Caption = 'REPT' ) then
    begin
        Add_Key( 'REPT', 130, 1 ) ;
        Add_Key( 'REPT', 130, 0 ) ;
        exit ;
    end ;
    if( Button.Caption = 'HERE'#13'IS' ) then
    begin
        Add_Key( 'HERE IS', 131, 1 ) ;
        Add_Key( 'HERE IS', 131, 0 ) ;
        exit ;
    end ;

    if( Button = O_Button ) then
    begin
        if(
            ( Right_Shift_Button.Down )
            or
            ( Left_Shift_Button.Down )
          ) then
        begin
            C := BS ;
        end else
        begin
            C := 'O' ;
        end ;
    end else
    if( Button = N_Button ) then
    begin
        if(
            ( Right_Shift_Button.Down )
            or
            ( Left_Shift_Button.Down )
          ) then
        begin
            C := '^' ;
        end else
        begin
            C := 'N' ;
        end ;
    end else
    if( Button.Caption = 'X-ON'#13'Q' ) then
    begin
        C := 'Q' ;
    end else
    if( Button.Caption = 'X-OFF'#13'S' ) then
    begin
        C := 'S' ;
    end else
    if( Button.Caption = 'BELL'#13'G' ) then
    begin
        C := 'G' ;
    end else
    if( Button.Caption = '' ) then // Space bar
    begin
        C := ' ' ;
    end else
    if( length( Button.Caption ) = 1 ) then
    begin
        C := Button.Caption[ 1 ] ;
    end else
    if( Button.Caption = 'ESC' ) then
    begin
        C := ESC ;
    end else
    if( Button.Caption = 'RE-'#13'TURN' ) then
    begin
        C := CR ;
    end else
    if( Button.Caption = 'LINE'#13'FEED' ) then
    begin
        C := LF ;
    end else
    if( Button.Caption = 'RUB'#13'OUT' ) then
    begin
        C := #127 ;
    end else
    begin // If we get here it is a button with multiple glyphs
        S := Button.Caption ;
        Dummy := pos( #13, S ) ;
        if(
            ( Right_Shift_Button.Down )
            or
            ( Left_Shift_Button.Down )
          ) then
        begin
            S := copy( S, 1, Dummy - 1 ) ;
        end else
        begin
            S := copy( S, Dummy + 1, length( S ) ) ;
        end ;
        C := S[ 1 ] ;
    end ;

    if( Left_Control_Button.Down ) then
    begin
        C := chr( ord( C ) - 64 ) ;
    end ;

    Add_Key( C, ord( C ), 1 ) ;
    Add_Key( C, ord( C ), 0 ) ;

    if( Right_Shift_Button.Down ) then
    begin
        Add_Key( 'RIGHT_SHIFT', 129, 0 ) ;
    end ;
    if( Left_Shift_Button.Down ) then
    begin
        Add_Key( 'LEFT_SHIFT', 128, 0 ) ;
    end ;
    Right_Shift_Button.Down := False ;
    Left_Shift_Button.Down := False ;
    if( Left_Control_Button.Down ) then
    begin
        Add_Key( 'CONTROL', 132, 0 ) ;
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
        Result.Font.Name := Original.Font.Name ;
        Result.Font.Height := Original.Font.Height ;
        Result.Font.Size := Original.Font.Size ;
        Result.Spacing := Original.Spacing ;
        Result.Glyph := Original.Glyph ;
        Result.GroupIndex := Original.GroupIndex ;
        Original.Glyph := nil ;
        Original.Free ;
    end ;

var Changes : boolean ;
    Dummy : integer ;
    Key : TKey_Button ;
    Loop : integer ;

begin
    // Setup keyboard...
    Left_Control_Button := Update_Key( Left_Control_Button, clGray ) ;
    Button_1 := Update_Key( Button_1, clGray ) ;
    Button_2 := Update_Key( Button_2, clGray ) ;
    Button_3 := Update_Key( Button_3, clGray ) ;
    Button_4 := Update_Key( Button_4, clGray ) ;
    Button_5 := Update_Key( Button_5, clGray ) ;
    Button_6 := Update_Key( Button_6, clGray ) ;
    Button_7 := Update_Key( Button_7, clGray ) ;
    Button_8 := Update_Key( Button_8, clGray ) ;
    Button_9 := Update_Key( Button_9, clGray ) ;
    Button_Equal := Update_Key( Button_Equal, clGray ) ;
    Button_Right_Brace := Update_Key( Button_Right_Brace, clGray ) ;
    Line_Feed_Button := Update_Key( Line_Feed_Button, clGray ) ;
    Shift_Lock_Button := Update_Key( Shift_Lock_Button, clGray ) ;
    Left_Shift_Button := Update_Key( Left_Shift_Button, clGray ) ;
    Button_Plus := Update_Key( Button_Plus, clGray ) ;
    Button_Underscore := Update_Key( Button_Underscore, clGray ) ;
    Button_Less_Than := Update_Key( Button_Less_Than, clGray ) ;
    Button_Greater_Than := Update_Key( Button_Greater_Than, clGray ) ;
    Button_Question_Mark := Update_Key( Button_Question_Mark, clGray ) ;
    Right_Shift_Button := Update_Key( Right_Shift_Button, clGray ) ;
    Break_Button := Update_Key( Break_Button, clGray ) ;
    Return_Button := Update_Key( Return_Button, clGray ) ;
    Repeat_Button := Update_Key( Repeat_Button, clGray ) ;
    Space_Button := Update_Key( Space_Button, clGray ) ;
    N_Button := Update_Key( N_Button, clGray ) ;
    O_Button := Update_Key( O_Button, clGray ) ;

    repeat
        Changes := False ;
        for Loop := 0 to Panel3.ControlCount - 1 do
        begin
            if( Panel3.Controls[ Loop ] is TKey_Button ) then // Already converted
            begin
                Key := TKey_Button( TSpeedButton( Panel3.Controls[ Loop ] ) ) ;
            end else
            if( Panel3.Controls[ Loop ] is TSpeedButton ) then
            begin
                Key := Update_Key( TSpeedButton( Panel3.Controls[ Loop ] ), clGray ) ;
                Changes := True ; // Need to run through the loop again
            end else
            begin
                Key := nil ;
            end ;
            if( Key <> nil ) then
            begin
                Dummy := pos( ' ', Key.Caption ) ;
                while( Dummy > 0 ) do
                begin
                    Key.Caption := copy( Key.Caption, 1, Dummy - 1 ) + #13 + copy( Key.Caption, Dummy + 1, length( Key.Caption ) ) ;
                    Dummy := pos( ' ', Key.Caption ) ;
                end ;
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
          Inc( X, Client.Left + Offset.X ) ;
          Inc( Y, Client.Top + Offset.Y ) ;
      end ;

      OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
    end ;


    procedure Draw( const Client : TRect ; const Offset : TPoint ) ;

    var GlyphPos: TPoint;

    begin
        Calculate_Layout(Client, Offset, GlyphPos ) ;
        if( Caption <> '' ) then
        begin
            GlyphPos.Y := Client.Top + Offset.Y ;
        end ;
        Draw_Glyph( GlyphPos ) ;
    end ;

var PaintRect, Saved_PaintRect : TRect ;
    DrawFlags : Integer ;
    Dummy : integer ;
    Offset : TPoint ;
    S1, S2 : string ;
    Saved : THandle ;
    Y : integer ;

begin
    // Setup...
    Canvas.Font := Font ;
    Canvas.Font.Size := Font.Size - 1 ;
    Canvas.Font.Size := Font.Size ;
    PaintRect := Rect( 0, 0, Width, Height ) ;

    // Draw border...
    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT ;
    if( FState in [ bsDown, bsExclusive ] ) then
    begin
        DrawFlags := DrawFlags or DFCS_PUSHED ;
    end ;
    DrawFrameControl( Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags ) ;

    // Fill interior of button...
    InflateRect( PaintRect, -1, -1 ) ;
    Canvas.Brush.Color := Color ;
    Saved_PaintRect := PaintRect ;
    Canvas.FillRect( PaintRect ) ;
    PaintRect := Saved_PaintRect ;
    InflateRect( PaintRect, -1, -1 ) ;

    // Draw button face...
    if( FState in [ bsDown, bsExclusive ] ) then
    begin
        Offset.X := 1 ;
        Offset.Y := 1 ;
    end else
    begin
        Offset.X := 0 ;
        Offset.Y := 0 ;
    end ;
    Draw( PaintRect, Offset ) ;
    if( Caption <> '' ) then
    begin
        Saved := selectobject( Canvas.Handle, Font.Handle ) ;
        try
            Dummy := pos( #13, Caption ) ;
            if( Dummy > 0 ) then
            begin
                // Determine total text height...
                Y := 0 ;
                S1 := Caption ;
                while( Dummy > 0 ) do
                begin
                    S2 := copy( S1, 1, Dummy - 1 ) ;
                    S1 := copy( S1, Dummy + 1, length( S1 ) ) ;
                    if( S2 = '' ) then
                    begin
                        S2 := ' ' ;
                    end ;
                    Y := Y + Canvas.TextHeight( S2 ) ;
                    Dummy := pos( #13, S1 ) ;
                end ;
                Y := Y + Canvas.TextHeight( S1 ) ;

                // Write text...
                Offset.Y := ( Height - Y ) div 2 ;
                if( Offset.Y < PaintRect.Top ) then
                begin
                    Offset.Y := PaintRect.Top ;
                end ;
                Dummy := pos( #13, Caption ) ;
                S1 := Caption ;
                while( Dummy > 0 ) do
                begin
                    S2 := copy( S1, 1, Dummy - 1 ) ;
                    S1 := copy( S1, Dummy + 1, length( S1 ) ) ;
                    if( S2 = '' ) then
                    begin
                        S2 := ' ' ;
                    end ;
                    Dummy := ( Width - Canvas.TextWidth( S2 ) ) div 2 ;
                    if( Dummy < PaintRect.Left ) then
                    begin
                        Dummy := PaintRect.Left ;
                    end ;
                    Canvas.TextOut( Dummy + Offset.X, Offset.Y, S2 ) ;
                    Offset.Y := Offset.Y + Canvas.TextHeight( S2 ) ;
                    Dummy := pos( #13, S1 ) ;
                end ;
                Dummy := ( Width - Canvas.TextWidth( S1 ) ) div 2 ;
                if( Dummy < PaintRect.Left ) then
                begin
                    Dummy := PaintRect.Left ;
                end ;
                Canvas.TextOut( Dummy + Offset.X, Offset.Y, S1 ) ;
            end else
            begin
                Dummy := ( Width - Canvas.TextWidth( Caption ) ) div 2 ;
                Canvas.TextOut( Dummy + Offset.X,
                    ( ( Height - Canvas.TextHeight( Caption ) ) div 2 ) + Offset.Y,
                    Caption ) ;
            end ;
        finally
            selectobject( Canvas.Handle, Saved ) ;
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
