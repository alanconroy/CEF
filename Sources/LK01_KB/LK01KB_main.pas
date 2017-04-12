{
        Program Name : LK01KB_Main
        Package Name : CEF
        Purpose      : DEC LK01 keyboard
        Institution  : Conroy & Conroy Co.
        Date Written : 27-Jan-2007
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

          This is the visual portion of the DEC LK01 keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit LK01KB_main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // CEF...
     _CEF, // TUI_Interface
     TPanels ; // TTransparent_Panel

type
  TMain_Form = class(TForm)
    Panel3: TPanel;
    Top_Left_Button: TSpeedButton;
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
    Button_Vertical: TSpeedButton;
    SpeedButton110: TSpeedButton;
    SpeedButton111: TSpeedButton;
    SpeedButton112: TSpeedButton;
    SpeedButton113: TSpeedButton;
    SpeedButton114: TSpeedButton;
    SpeedButton115: TSpeedButton;
    SpeedButton116: TSpeedButton;
    Bar_Button: TSpeedButton;
    SpeedButton119: TSpeedButton;
    SpeedButton120: TSpeedButton;
    SpeedButton121: TSpeedButton;
    SpeedButton122: TSpeedButton;
    Button_Accent: TSpeedButton;
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
    SpeedButton144: TSpeedButton;
    SpeedButton145: TSpeedButton;
    Button_Plus: TSpeedButton;
    Button_Asterisk: TSpeedButton;
    Return_Button: TSpeedButton;
    Repeat_Button: TSpeedButton;
    Button_Less_Than: TSpeedButton;
    Button_Greater_Than: TSpeedButton;
    Button_Question_Mark: TSpeedButton;
    Space_Button: TSpeedButton;
    Right_Shift_Button: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    Button_Line_Feed: TSpeedButton;
    Local_Line_Feed_Button: TSpeedButton;
    Mode_Button: TSpeedButton;
    Baud_Rate_Button: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Shift_Lock_LED: TShape;
    procedure Shift_Lock_ButtonClick(Sender: TObject);
    procedure SpeedButton110Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    public // API...
      _UI : TUI_Interface ;
      _LK01KB : TComponent ;
  end ;

implementation

uses // C&C...
     _ASCII, // ESC
     _UE, // TUnified_Exception

     // CEF...
     LK_01_KB ; // _UI

{$R *.dfm}

type TKey_Button = class( TSpeedButton )
                       private // Instance data...
                           Compound : TSpeedButton ; // The button that this is part of
                           Side : TAlign ; // Which side connects with Compound
                           LED : TShape ;
                           LED_Position : TPoint ;

                       public // Overrrides...
                           procedure Paint ; override ;

                       published
                           property Color ;
                   end ;


// TMain_Form methods...

procedure TMain_Form.Shift_Lock_ButtonClick(Sender: TObject) ;

begin
    if( Shift_Lock_Button.Down ) then
    begin
        Shift_Lock_LED.Brush.Color := clRed ;
        TLK01KB( _LK01KB )._Keyboard.Add_Key( 'SHIFT LOCK', Shift_Lock_Button.Tag, 1 ) ;
    end else
    begin
        Shift_Lock_LED.Brush.Color := clMaroon ;
        TLK01KB( _LK01KB )._Keyboard.Add_Key( 'SHIFT LOCK', Shift_Lock_Button.Tag, 0 ) ;
    end ;
end ;


procedure TMain_Form.SpeedButton110Click( Sender : TObject ) ;

    procedure Add_Key( Name : string ; Value, State : integer ) ;

    begin
        TLK01KB( _LK01KB )._Keyboard.Add_Key( Name, Value, State ) ;
    end ;


    procedure Cycle_Key( const Name : string ; Index : integer ) ;

    begin
        Add_Key( Name, Index, 1 ) ;
        Add_Key( Name, Index, 0 ) ;
    end ;

var C : char ;
    Dummy : integer ;
    Button : TKey_Button ;
    S : string ;

begin
    // Setup...
    Button := TKey_Button( Sender ) ;

    // Handle buttons with special up/down processing...
    if( Button = Mode_Button ) then
    begin
        if( Button.Down ) then
        begin
            Add_Key( 'LOCAL', 157, 0 ) ;
            Add_Key( 'LINE', 128, 1 ) ;
        end else
        begin
            Add_Key( 'LINE', 157, 0 ) ;
            Add_Key( 'LOCAL', 128, 1 ) ;
        end ;
        exit ;
    end else
    if( Button = Baud_Rate_Button ) then
    begin
        if( Button.Down ) then
        begin
            Add_Key( 'BAUD RATE 110', 155, 0 ) ;
            Add_Key( 'BAUD RATE 300', 156, 1 ) ;
        end else
        begin
            Add_Key( 'BAUD RATE 300', 156, 0 ) ;
            Add_Key( 'BAUD RATE 110', 155, 1 ) ;
        end ;
        exit ;
    end else
    if( Button = Left_Shift_Button ) then
    begin
        if( Button.Down ) then
        begin
            Add_Key( 'LEFT_SHIFT', 135, 1 ) ;
        end else
        begin
            Add_Key( 'LEFT_SHIFT', 132, 0 ) ;
        end ;
        exit ;
    end else
    if( Button = Right_Shift_Button ) then
    begin
        if( Button.Down ) then
        begin
            Add_Key( 'RIGHT_SHIFT', 136, 1 ) ;
        end else
        begin
            Add_Key( 'RIGHT_SHIFT', 132, 0 ) ;
        end ;
        exit ;
    end ;
    if( Button.Caption = 'SHIFT'#13'LOCK' ) then
    begin
        if( Button.Down ) then
        begin
            Add_Key( 'SHIFT LOCK', 134, 1 ) ;
        end else
        begin
            Add_Key( 'SHIFT LOCK', 134, 0 ) ;
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

    // Handle other special keys...
    if( Button.Caption = 'BREAK' ) then
    begin
        Cycle_Key( 'BREAK', 0 ) ;
        exit ;
    end else
    if( Button = Local_Line_Feed_Button ) then
    begin
        Cycle_Key( 'LOCAL LINE FEED', Button.Tag ) ;
        exit ;
    end else

    // Handle keypad keys...
    case Button.Tag of
        128 : begin Cycle_Key( 'LINE LOCAL', Button.Tag ) ; exit ; end ;
        129 : begin Cycle_Key( 'HERE IS', Button.Tag ) ; exit ; end ;
        130 : begin Cycle_Key( 'LOCAL FORM FEED', Button.Tag ) ; exit ; end ;
        131 : begin Cycle_Key( 'LOCAL LINE FEED', Button.Tag ) ; exit ; end ;
        155 : if Button.Down then
              begin
                  Add_Key( 'MODE LOCAL', 155, 0 ) ;
                  Add_Key( 'MODE LINE', 155, 1 ) ;
                  exit ;
              end else
              begin
                  Add_Key( 'MODE LINE', 155, 0 ) ;
                  Add_Key( 'MODE LOCAL', 155, 1 ) ;
                  exit ;
              end ;
        156 : if Button.Down then
              begin
                  Add_Key( 'BAUD RATE 110', 156, 0 ) ;
                  Add_Key( 'BAUD RATE 300', 156, 1 ) ;
                  exit ;
              end else
              begin
                  Add_Key( 'BAUD RATE 300', 156, 0 ) ;
                  Add_Key( 'BAUD RATE 110', 156, 1 ) ;
                  exit ;
              end ;
    end ;

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
    if( Button.Caption = 'TAB' ) then
    begin
        C := HT ;
    end else
    if( ( Button.Caption = 'ESC' ) or ( Button.Caption = 'ALT' ) ) then
    begin
        C := ESC ;
    end else
    if( ( Button.Caption = 'RETURN' ) or ( Button.Caption = 'CR' ) or ( Button.Caption = 'ENTER' ) ) then
    begin
        C := CR ;
    end else
    if( ( Button.Caption = 'LINE'#13'FEED' ) or ( Button.Caption = 'LF' ) ) then
    begin
        C := LF ;
    end else
    if( ( Button.Caption = 'DELETE' ) or ( Button.Caption = 'RUB OUT' ) ) then
    begin
        C := #127 ;
    end else
    begin // If we get here it is a button with multiple glyphs
        S := Button.Caption ;
        Dummy := pos( #13, S ) ;
        if( Right_Shift_Button.Down or Left_Shift_Button.Down or Shift_Lock_Button.Down ) then
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
        begin
            C := S[ 1 ] ;
        end ;
    end ;

    if(
        ( C >= 'A' ) and ( C <= 'Z' )
        and
        ( not Right_Shift_Button.Down )
        and
        ( not Left_Shift_Button.Down )
        and
        ( not Shift_Lock_Button.Down )
      ) then
    begin
        C := lowercase( C )[ 1 ] ;
    end ;
    if( Left_Control_Button.Down ) then
    begin
        C := upcase( C ) ;
        C := chr( ord( C ) - 64 ) ;
    end ;

    Add_Key( C, ord( C ), 1 ) ;
    Add_Key( C, ord( C ), 0 ) ;

    if( Right_Shift_Button.Down ) then
    begin
        Add_Key( 'RIGHT_SHIFT', 136, 0 ) ;
    end ;
    if( Left_Shift_Button.Down ) then
    begin
        Add_Key( 'LEFT_SHIFT', 135, 0 ) ;
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
    Dummy, Loop : integer ;
    Key : TKey_Button ;

begin
    // Setup keyboard...
    Top_Left_Button := Update_Key( Top_Left_Button, clBlack ) ;
    Left_Control_Button := Update_Key( Left_Control_Button, clBlack ) ;
    SpeedButton89 := Update_Key( SpeedButton89, clBlack ) ;
    Button_Equal := Update_Key( Button_Equal, clBlack ) ;
    Button_Vertical := Update_Key( Button_Vertical, clBlack ) ;
    SpeedButton110 := Update_Key( SpeedButton110, clBlack ) ;
    SpeedButton111 := Update_Key( SpeedButton111, clBlack ) ;
    SpeedButton112 := Update_Key( SpeedButton112, clBlack ) ;
    SpeedButton113 := Update_Key( SpeedButton113, clBlack ) ;
    SpeedButton114 := Update_Key( SpeedButton114, clBlack ) ;
    SpeedButton115 := Update_Key( SpeedButton115, clBlack ) ;
    SpeedButton116 := Update_Key( SpeedButton116, clBlack ) ;
    Bar_Button := Update_Key( Bar_Button, clBlack ) ;
    SpeedButton119 := Update_Key( SpeedButton119, clBlack ) ;
    SpeedButton120 := Update_Key( SpeedButton120, clBlack ) ;
    SpeedButton121 := Update_Key( SpeedButton121, clBlack ) ;
    SpeedButton122 := Update_Key( SpeedButton122, clBlack ) ;
    Button_Accent := Update_Key( Button_Accent, clBlack ) ;
    SpeedButton125 := Update_Key( SpeedButton125, clBlack ) ;
    Shift_Lock_Button := Update_Key( Shift_Lock_Button, clBlack ) ;
    Left_Shift_Button := Update_Key( Left_Shift_Button, clBlack ) ;
    SpeedButton131 := Update_Key( SpeedButton131, clBlack ) ;
    SpeedButton132 := Update_Key( SpeedButton132, clBlack ) ;
    SpeedButton133 := Update_Key( SpeedButton133, clBlack ) ;
    SpeedButton134 := Update_Key( SpeedButton134, clBlack ) ;
    SpeedButton135 := Update_Key( SpeedButton135, clBlack ) ;
    SpeedButton136 := Update_Key( SpeedButton136, clBlack ) ;
    SpeedButton137 := Update_Key( SpeedButton137, clBlack ) ;
    SpeedButton138 := Update_Key( SpeedButton138, clBlack ) ;
    SpeedButton139 := Update_Key( SpeedButton139, clBlack ) ;
    SpeedButton140 := Update_Key( SpeedButton140, clBlack ) ;
    SpeedButton141 := Update_Key( SpeedButton141, clBlack ) ;
    SpeedButton142 := Update_Key( SpeedButton142, clBlack ) ;
    SpeedButton143 := Update_Key( SpeedButton143, clBlack ) ;
    SpeedButton144 := Update_Key( SpeedButton144, clBlack ) ;
    SpeedButton145 := Update_Key( SpeedButton145, clBlack ) ;
    Button_Plus := Update_Key( Button_Plus, clBlack ) ;
    Button_Asterisk := Update_Key( Button_Asterisk, clBlack ) ;
    Return_Button := Update_Key( Return_Button, clBlack ) ;
    Repeat_Button := Update_Key( Repeat_Button, clBlack ) ;
    Button_Less_Than := Update_Key( Button_Less_Than, clBlack ) ;
    Button_Greater_Than := Update_Key( Button_Greater_Than, clBlack ) ;
    Button_Question_Mark := Update_Key( Button_Question_Mark, clBlack ) ;
    Space_Button := Update_Key( Space_Button, clBlack ) ;
    Right_Shift_Button := Update_Key( Right_Shift_Button, clBlack ) ;
    SpeedButton2 := Update_Key( SpeedButton2, clBlack ) ;
    SpeedButton4 := Update_Key( SpeedButton4, clBlack ) ;
    SpeedButton5 := Update_Key( SpeedButton5, clBlack ) ;
    SpeedButton6 := Update_Key( SpeedButton6, clBlack ) ;
    SpeedButton7 := Update_Key( SpeedButton7, clBlack ) ;
    SpeedButton8 := Update_Key( SpeedButton8, clBlack ) ;
    SpeedButton9 := Update_Key( SpeedButton9, clBlack ) ;
    SpeedButton10 := Update_Key( SpeedButton10, clBlack ) ;
    SpeedButton11 := Update_Key( SpeedButton11, clBlack ) ;
    Button_Line_Feed := Update_Key( Button_Line_Feed, clBlack ) ;
    Local_Line_Feed_Button := Update_Key( Local_Line_Feed_Button, clBlack ) ;
    Mode_Button := Update_Key( Mode_Button, clBlack ) ;
    Baud_Rate_Button := Update_Key( Baud_Rate_Button, clBlack ) ;
    SpeedButton1 := Update_Key( SpeedButton1, clBlack ) ;
    SpeedButton3 := Update_Key( SpeedButton3, clBlack ) ;
    Button_1 := Update_Key( Button_1, clBlack ) ;
    Button_2 := Update_Key( Button_2, clBlack ) ;
    Button_3 := Update_Key( Button_3, clBlack ) ;
    Button_4 := Update_Key( Button_4, clBlack ) ;
    Button_5 := Update_Key( Button_5, clBlack ) ;
    Button_6 := Update_Key( Button_6, clBlack ) ;
    Button_7 := Update_Key( Button_7, clBlack ) ;
    Button_8 := Update_Key( Button_8, clBlack ) ;
    Button_9 := Update_Key( Button_9, clBlack ) ;

    TKey_Button( Shift_Lock_Button ).LED := Shift_Lock_LED ;
    TKey_Button( Shift_Lock_Button ).LED_Position := Point( Shift_Lock_LED.Left, Shift_Lock_LED.Top ) ;

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
                Key := Update_Key( TSpeedButton( Panel3.Controls[ Loop ] ), clBlack ) ;
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
    Shift_Lock_LED.BringToFront ;
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

    // Handle compound buttons...
    if( Compound <> nil ) then
    begin
        if( Compound.Down <> Down ) then
        begin
            Compound.Down := Down ;
            Compound.Invalidate ;
        end ;
        if( ( FState = bsDown ) <> ( TKey_Button( Compound ).FState = bsDown ) ) then
        begin
            TKey_Button( Compound ).FState := FState ;
            Compound.Invalidate ;
        end ;
    end ;

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
    if( Compound <> nil ) then
    begin
        case Side of
            alTop : PaintRect.Top := 0 ;
            alBottom : PaintRect.Top := Height ;
            alLeft : PaintRect.Top := 0 ;
            alRight : PaintRect.Top := Width ;
        end ;
    end ;
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
