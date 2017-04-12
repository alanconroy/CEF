{
        Program Name : Apple2KB_Main
        Package Name : CEF
        Purpose      : Apple II keyboard component main form
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

        This is the main form for the Apple II keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Apple2KB_main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // CEF...
     CEF ; // TCable

type
  TMain_Form = class(TForm)
    Panel2: TPanel;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    SpeedButton27: TSpeedButton;
    SpeedButton28: TSpeedButton;
    SpeedButton29: TSpeedButton;
    SpeedButton30: TSpeedButton;
    SpeedButton31: TSpeedButton;
    SpeedButton32: TSpeedButton;
    SpeedButton33: TSpeedButton;
    SpeedButton34: TSpeedButton;
    SpeedButton35: TSpeedButton;
    SpeedButton36: TSpeedButton;
    SpeedButton37: TSpeedButton;
    SpeedButton38: TSpeedButton;
    SpeedButton39: TSpeedButton;
    SpeedButton40: TSpeedButton;
    SpeedButton41: TSpeedButton;
    SpeedButton42: TSpeedButton;
    SpeedButton43: TSpeedButton;
    SpeedButton44: TSpeedButton;
    SpeedButton45: TSpeedButton;
    SpeedButton46: TSpeedButton;
    SpeedButton47: TSpeedButton;
    SpeedButton48: TSpeedButton;
    SpeedButton49: TSpeedButton;
    SpeedButton50: TSpeedButton;
    SpeedButton51: TSpeedButton;
    SpeedButton52: TSpeedButton;
    SpeedButton53: TSpeedButton;
    SpeedButton54: TSpeedButton;
    SpeedButton55: TSpeedButton;
    SpeedButton56: TSpeedButton;
    SpeedButton57: TSpeedButton;
    SpeedButton58: TSpeedButton;
    SpeedButton59: TSpeedButton;
    SpeedButton60: TSpeedButton;
    SpeedButton61: TSpeedButton;
    SpeedButton62: TSpeedButton;
    SpeedButton63: TSpeedButton;
    SpeedButton64: TSpeedButton;
    SpeedButton65: TSpeedButton;
    SpeedButton66: TSpeedButton;
    SpeedButton67: TSpeedButton;
    SpeedButton68: TSpeedButton;
    SpeedButton69: TSpeedButton;
    SpeedButton70: TSpeedButton;
    SpeedButton71: TSpeedButton;
    SpeedButton72: TSpeedButton;
    SpeedButton73: TSpeedButton;
    SpeedButton74: TSpeedButton;
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
    SpeedButton111: TSpeedButton;
    SpeedButton112: TSpeedButton;
    SpeedButton113: TSpeedButton;
    SpeedButton114: TSpeedButton;
    SpeedButton115: TSpeedButton;
    SpeedButton116: TSpeedButton;
    SpeedButton119: TSpeedButton;
    SpeedButton120: TSpeedButton;
    SpeedButton121: TSpeedButton;
    SpeedButton122: TSpeedButton;
    Return_Button: TSpeedButton;
    SpeedButton125: TSpeedButton;
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
    Repeat_Button: TSpeedButton;
    Button_Less_Than: TSpeedButton;
    Button_Greater_Than: TSpeedButton;
    Button_Question_Mark: TSpeedButton;
    Right_Button: TSpeedButton;
    Space_Button: TSpeedButton;
    Clear_Button: TSpeedButton;
    Right_Shift_Button: TSpeedButton;
    procedure SpeedButton110Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private // Instance data...
      
  public // API...
      procedure Write_Serial( Value : integer ) ;
  end ;

implementation

uses // C&C...
     _ASCII, // ESC
     _UE, // TUnified_Exception

     // CEF...
     Apple2_KB ; // Screen

{$R *.dfm}

type TKey_Button = class( TSpeedButton )
                       public
                           LED : TShape ;
                           LED_Position : TPoint ;

                           procedure Paint ; override ;

                       published
                           property Color ;
                   end ;

                   
// TMain_Form methods...

procedure TMain_Form.Write_Serial( Value : integer ) ;

begin
end ;


procedure TMain_Form.SpeedButton110Click( Sender : TObject ) ;

var C : char ;
    Dummy : integer;
    Button : TSpeedButton ;
    S : string ;

    procedure Cycle_Key( const Name : string ; Index : integer ) ;

    begin
        _Apple2KB._Keyboard.Add_Key( Name, Index, 1 ) ;
        _Apple2KB._Keyboard.Add_Key( Name, Index, 0 ) ;
    end ;

begin
    Button := TSpeedButton( Sender ) ;

    // Handle keys with special up/down processing...
    if( Button = Left_Shift_Button ) then
    begin
        if( Left_Shift_Button.Down ) then
        begin
            _Apple2KB._Keyboard.Add_Key( 'LEFT_SHIFT', 130, 0 ) ;
        end else
        begin
            _Apple2KB._Keyboard.Add_Key( 'LEFT_SHIFT', 130, 1 ) ;
        end ;
        exit ;
    end ;
    if( Button = Right_Shift_Button ) then
    begin
        if( Right_Shift_Button.Down ) then
        begin
            _Apple2KB._Keyboard.Add_Key( 'RIGHT_SHIFT', 131, 0 ) ;
        end else
        begin
            _Apple2KB._Keyboard.Add_Key( 'RIGHT_SHIFT', 131, 1 ) ;
        end ;
        exit ;
    end ;
    if( Button = Left_Control_Button ) then
    begin
        if( Left_Control_Button.Down ) then
        begin
            _Apple2KB._Keyboard.Add_Key( 'CONTROL', 132, 0 ) ;
        end else
        begin
            _Apple2KB._Keyboard.Add_Key( 'CONTROL', 132, 1 ) ;
        end ;
        exit ;
    end ;

    // Handle other special keys...
    if( Button = Left_Button ) then
    begin
        Cycle_Key( 'LEFT', 128 ) ;
        exit ;
    end ;
    if( Button = Right_Button ) then
    begin
        Cycle_Key( 'RIGHT', 129 ) ;
        exit ;
    end ;
    if( Button.Caption = 'RESET' ) then
    begin
        Cycle_Key( 'RESET', 133 ) ;
        exit ;
    end ;
    if( Button.Caption = 'REPT' ) then
    begin
        Cycle_Key( 'REPT', 0 ) ;
        exit ;
    end ;

    // Process other keys...
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
    if( Button.Caption = 'RETURN' ) then
    begin
        C := CR ;
    end else
    if( Button.Caption = 'BELL'#13'G' ) then
    begin
        C := 'G' ;
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

        C := S[ 1 ] ;
    end ;

    if(
        ( C >= 'A' ) and ( C <= 'Z' )
        and
        ( not Right_Shift_Button.Down )
        and
        ( not Left_Shift_Button.Down )
      ) then
    begin
        C := lowercase( C )[ 1 ] ;
    end ;
    if( Left_Control_Button.Down ) then
    begin
        C := chr( ord( C ) - 64 ) ;
    end ;

    Cycle_Key( C, ord( C ) ) ;

    if( Right_Shift_Button.Down ) then
    begin
        _Apple2KB._Keyboard.Add_Key( 'RIGHT_SHIFT', 131, 0 ) ;
    end ;
    if( Left_Shift_Button.Down ) then
    begin
        _Apple2KB._Keyboard.Add_Key( 'LEFT_SHIFT', 130, 0 ) ;
    end ;
    Right_Shift_Button.Down := False ;
    Left_Shift_Button.Down := False ;
    if( Left_Control_Button.Down ) then
    begin
        _Apple2KB._Keyboard.Add_Key( 'CONTROL', 132, 0 ) ;
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
    Dummy : integer ;
    Key : TKey_Button ;
    Loop : integer ;

begin
    // Setup keyboard...
    Escape_Button := Update_Key( Escape_Button, clDkGray ) ;
    Left_Control_Button := Update_Key( Left_Control_Button, clDkGray ) ;
    Button_1 := Update_Key( Button_1, clDkGray ) ;
    Button_2 := Update_Key( Button_2, clDkGray ) ;
    Button_3 := Update_Key( Button_3, clDkGray ) ;
    Button_4 := Update_Key( Button_4, clDkGray ) ;
    Button_5 := Update_Key( Button_5, clDkGray ) ;
    Button_6 := Update_Key( Button_6, clDkGray ) ;
    Button_7 := Update_Key( Button_7, clDkGray ) ;
    Button_8 := Update_Key( Button_8, clDkGray ) ;
    Button_9 := Update_Key( Button_9, clDkGray ) ;
    Button_Equal := Update_Key( Button_Equal, clDkGray ) ;
    Button_Tilde := Update_Key( Button_Tilde, clDkGray ) ;
    Left_Button := Update_Key( Left_Button, clDkGray ) ;
    Left_Shift_Button := Update_Key( Left_Shift_Button, clDkGray ) ;
    Button_Plus := Update_Key( Button_Plus, clDkGray ) ;
    Button_Less_Than := Update_Key( Button_Less_Than, clDkGray ) ;
    Button_Greater_Than := Update_Key( Button_Greater_Than, clDkGray ) ;
    Button_Question_Mark := Update_Key( Button_Question_Mark, clDkGray ) ;
    Right_Button := Update_Key( Right_Button, clDkGray ) ;
    Right_Shift_Button := Update_Key( Right_Shift_Button, clDkGray ) ;
    Clear_Button := Update_Key( Clear_Button, clDkGray ) ;
    Repeat_Button := Update_Key( Repeat_Button, clDkGray ) ;
    Return_Button := Update_Key( Return_Button, clDkGray ) ;

    repeat
        Changes := False ;
        for Loop := 0 to Panel3.ControlCount - 1 do
        begin
            if( Panel3.Controls[ Loop ] is TSpeedButton ) then
            begin
                if( Panel3.Controls[ Loop ] is TKey_Button ) then
                begin
                    Key := TKey_Button( Panel3.Controls[ Loop ] ) ;
                end else
                begin
                    Key := Update_Key( TSpeedButton( Panel3.Controls[ Loop ] ), clDkGray ) ;
                    Changes := True ; // Need to run through the loop again
                end ;
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
    if( Caption = '' ) then
    begin
        Draw( PaintRect, Offset ) ;
    end else
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


procedure TMain_Form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
    Action := caFree ;
end ;


end.
