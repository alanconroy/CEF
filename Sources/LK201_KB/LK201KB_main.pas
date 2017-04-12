{
        Program Name : LK201KB_Main
        Package Name : CEF
        Purpose      : DEC LK201 keyboard
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

          This is the visual portion of the DEC LK201 keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit LK201KB_main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // TBase_Cable
     TPanels ; // TTransparent_Panel

type
  TMain_Form = class(TForm)
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
    Select_Button: TSpeedButton;
    SpeedButton110: TSpeedButton;
    SpeedButton111: TSpeedButton;
    SpeedButton112: TSpeedButton;
    SpeedButton113: TSpeedButton;
    SpeedButton114: TSpeedButton;
    SpeedButton115: TSpeedButton;
    SpeedButton116: TSpeedButton;
    F1_Button: TSpeedButton;
    SpeedButton119: TSpeedButton;
    SpeedButton120: TSpeedButton;
    SpeedButton121: TSpeedButton;
    SpeedButton122: TSpeedButton;
    Button_Accent: TSpeedButton;
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
    Return_Button: TSpeedButton;
    Repeat_Button: TSpeedButton;
    Button_Less_Than: TSpeedButton;
    Button_Greater_Than: TSpeedButton;
    Button_Question_Mark: TSpeedButton;
    Right_Button: TSpeedButton;
    Down_Button: TSpeedButton;
    Space_Button: TSpeedButton;
    Right_Shift_Button: TSpeedButton;
    Delete_Button: TSpeedButton;
    Next_Screen_Button: TSpeedButton;
    Prev_Screen_Button: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
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
    Button_Left_Brace: TSpeedButton;
    Button_Vertical: TSpeedButton;
    Find_Button: TSpeedButton;
    Insert_Here_Button: TSpeedButton;
    Remove_Button: TSpeedButton;
    F6_Button: TSpeedButton;
    F4_Button: TSpeedButton;
    F3_Button: TSpeedButton;
    F2_Button: TSpeedButton;
    F9_Button: TSpeedButton;
    F8_Button: TSpeedButton;
    F7_Button: TSpeedButton;
    F11_Button: TSpeedButton;
    F14_Button: TSpeedButton;
    F13_Button: TSpeedButton;
    F12_Button: TSpeedButton;
    Do_Button: TSpeedButton;
    Help_Button: TSpeedButton;
    SpeedButton41: TSpeedButton;
    SpeedButton42: TSpeedButton;
    SpeedButton43: TSpeedButton;
    SpeedButton44: TSpeedButton;
    F17_Button: TSpeedButton;
    F18_Button: TSpeedButton;
    F19_Button: TSpeedButton;
    F20_Button: TSpeedButton;
    F5_Button: TSpeedButton;
    F10_Button: TSpeedButton;
    Hold_Screen_LED: TShape;
    Wait_LED: TShape;
    Compose_LED: TShape;
    Lock_LED: TShape;
    F1_Label: TLabel;
    F2_Label: TLabel;
    F3_Label: TLabel;
    F4_Label: TLabel;
    F5_Label: TLabel;
    F6_Label: TLabel;
    F7_Label: TLabel;
    F8_Label: TLabel;
    F9_Label: TLabel;
    F10_Label: TLabel;
    F11_Label: TLabel;
    F12_Label: TLabel;
    F13_Label: TLabel;
    F14_Label: TLabel;
    F17_Label: TLabel;
    F18_Label: TLabel;
    F19_Label: TLabel;
    F20_Label: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    procedure SpeedButton110Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    public // API...
      _UI : TUI_Interface ;
      _LK201KB : TComponent ;
  end ;

implementation

uses // C&C...
     _ASCII, // ESC
     _UE, // TUnified_Exception
     UStrings, // Replace

     // CEF...
     LK_201_KB ; // _UI

{$R *.dfm}

type TKey_Button = class( TSpeedButton )
                       private // Instance data...
                           Compound : TSpeedButton ; // The button that this is part of
                           Side : TAlign ; // Which side connects with Compound

                       public // Overrrides...
                           procedure Paint ; override ;

                       published
                           property Color ;
                   end ;


// TMain_Form methods...

procedure TMain_Form.SpeedButton110Click( Sender : TObject ) ;

    procedure Add_Key( Name : string ; Value, State : integer ) ;

    begin
        TLK201KB( _LK201KB )._Keyboard.Add_Key( Name, Value, State ) ;
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

    // Handle glyph buttons...
    if( Button = Up_Button ) then
    begin
        Cycle_Key( 'UP', 130 ) ;
        exit ;
    end ;
    if( Button = Down_Button ) then
    begin
        Cycle_Key( 'DOWN', 131 ) ;
        exit ;
    end ;
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

    // Handle buttons with special up/down processing...
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

    // Handle other special keys...
    if( Button.Caption = 'Lock' ) then
    begin
        Cycle_Key( 'LOCK', 134 ) ;
        if( Lock_LED.Brush.COlor = clRed ) then
        begin
            Lock_LED.Brush.COlor := clMaroon ;
        end else
        begin
            Lock_LED.Brush.COlor := clRed ;
        end ;
        exit ;
    end ;
    if( Button.Caption = 'Ctrl' ) then
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
    if( Button.Caption = 'Compose'#13'__Character' ) then
    begin
        Cycle_Key( 'COMPOSE CHARACTER', 0 ) ;
        exit ;
    end ;

    // Handle keypad keys...
    case Button.Tag of
        137 : begin Cycle_Key( 'PF1', Button.Tag ) ; exit ; end ;
        138 : begin Cycle_Key( 'PF2', Button.Tag ) ; exit ; end ;
        139 : begin Cycle_Key( 'PF3', Button.Tag ) ; exit ; end ;
        140 : begin Cycle_Key( 'PF4', Button.Tag ) ; exit ; end ;
        141 : begin Cycle_Key( 'NKP_0', Button.Tag ) ; exit ; end ;
        142 : begin Cycle_Key( 'NKP_1', Button.Tag ) ; exit ; end ;
        143 : begin Cycle_Key( 'NKP_2', Button.Tag ) ; exit ; end ;
        144 : begin Cycle_Key( 'NKP_3', Button.Tag ) ; exit ; end ;
        145 : begin Cycle_Key( 'NKP_4', Button.Tag ) ; exit ; end ;
        146 : begin Cycle_Key( 'NKP_5', Button.Tag ) ; exit ; end ;
        147 : begin Cycle_Key( 'NKP_6', Button.Tag ) ; exit ; end ;
        148 : begin Cycle_Key( 'NKP_7', Button.Tag ) ; exit ; end ;
        149 : begin Cycle_Key( 'NKP_8', Button.Tag ) ; exit ; end ;
        150 : begin Cycle_Key( 'NKP_9', Button.Tag ) ; exit ; end ;
        151 : begin Cycle_Key( 'NKP_.', Button.Tag ) ; exit ; end ;
        152 : begin Cycle_Key( 'NKP_,', Button.Tag ) ; exit ; end ;
        153 : begin Cycle_Key( 'NKP_-', Button.Tag ) ; exit ; end ;
        154 : begin Cycle_Key( 'NKP_ENTER', Button.Tag ) ; exit ; end ;
        155 : begin Cycle_Key( 'F1', Button.Tag ) ; exit ; end ;
        156 : begin Cycle_Key( 'F2', Button.Tag ) ; exit ; end ;
        157 : begin Cycle_Key( 'F3', Button.Tag ) ; exit ; end ;
        158 : begin Cycle_Key( 'F4', Button.Tag ) ; exit ; end ;
        159 : begin Cycle_Key( 'F5', Button.Tag ) ; exit ; end ;
        160 : begin Cycle_Key( 'F6', Button.Tag ) ; exit ; end ;
        161 : begin Cycle_Key( 'F7', Button.Tag ) ; exit ; end ;
        162 : begin Cycle_Key( 'F8', Button.Tag ) ; exit ; end ;
        163 : begin Cycle_Key( 'F9', Button.Tag ) ; exit ; end ;
        164 : begin Cycle_Key( 'F10', Button.Tag ) ; exit ; end ;
        165 : begin Cycle_Key( 'F11', Button.Tag ) ; exit ; end ;
        166 : begin Cycle_Key( 'F12', Button.Tag ) ; exit ; end ;
        167 : begin Cycle_Key( 'F13', Button.Tag ) ; exit ; end ;
        168 : begin Cycle_Key( 'F14', Button.Tag ) ; exit ; end ;
        169 : begin Cycle_Key( 'F17', Button.Tag ) ; exit ; end ;
        170 : begin Cycle_Key( 'F18', Button.Tag ) ; exit ; end ;
        171 : begin Cycle_Key( 'F19', Button.Tag ) ; exit ; end ;
        172 : begin Cycle_Key( 'F20', Button.Tag ) ; exit ; end ;
        173 : begin Cycle_Key( 'FIND', Button.Tag ) ; exit ; end ;
        174 : begin Cycle_Key( 'INSERT HERE', Button.Tag ) ; exit ; end ;
        175 : begin Cycle_Key( 'REMOVE', Button.Tag ) ; exit ; end ;
        176 : begin Cycle_Key( 'SELECT', Button.Tag ) ; exit ; end ;
        177 : begin Cycle_Key( 'PREV SCREEN', Button.Tag ) ; exit ; end ;
        178 : begin Cycle_Key( 'NEXT SCREEN', Button.Tag ) ; exit ; end ;
        179 : begin Cycle_Key( 'DO', Button.Tag ) ; exit ; end ;
    end ;

    if( Button = Delete_Button ) then
    begin
        C := #127 ;
    end else
    if( Button.Caption = '' ) then // Space bar
    begin
        C := ' ' ;
    end else
    if( length( Button.Caption ) = 1 ) then
    begin
        C := Button.Caption[ 1 ] ;
    end else
    if( Button.Caption = 'Tab' ) then
    begin
        C := HT ;
    end else
    if( Button.Caption = 'Return' ) then
    begin
        C := CR ;
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
        ( Lock_LED.Brush.COlor <> clRed )
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
    Escape_Button := Update_Key( Escape_Button, clWhite ) ;
    Left_Control_Button := Update_Key( Left_Control_Button, clWhite ) ;
    Button_1 := Update_Key( Button_1, clWhite ) ;
    Button_2 := Update_Key( Button_2, clWhite ) ;
    Button_3 := Update_Key( Button_3, clWhite ) ;
    Button_4 := Update_Key( Button_4, clWhite ) ;
    Button_5 := Update_Key( Button_5, clWhite ) ;
    Button_6 := Update_Key( Button_6, clWhite ) ;
    Button_7 := Update_Key( Button_7, clWhite ) ;
    Button_8 := Update_Key( Button_8, clWhite ) ;
    Button_9 := Update_Key( Button_9, clWhite ) ;
    Button_Equal := Update_Key( Button_Equal, clWhite ) ;
    Button_Tilde := Update_Key( Button_Tilde, clWhite ) ;
    Button_Accent := Update_Key( Button_Accent, clWhite ) ;
    Shift_Lock_Button := Update_Key( Shift_Lock_Button, clWhite ) ;
    Left_Shift_Button := Update_Key( Left_Shift_Button, clWhite ) ;
    Button_Plus := Update_Key( Button_Plus, clWhite ) ;
    Button_Asterisk := Update_Key( Button_Asterisk, clWhite ) ;
    Button_Less_Than := Update_Key( Button_Less_Than, clWhite ) ;
    Button_Greater_Than := Update_Key( Button_Greater_Than, clWhite ) ;
    Button_Question_Mark := Update_Key( Button_Question_Mark, clWhite ) ;
    Right_Shift_Button := Update_Key( Right_Shift_Button, clWhite ) ;
    Delete_Button := Update_Key( Delete_Button, clWhite ) ;
    Space_Button := Update_Key( Space_Button, clWhite ) ;
    Return_Button := Update_Key( Return_Button, clWhite ) ;
    Button_Left_Brace := Update_Key( Button_Left_Brace, clWhite ) ;
    Button_Vertical := Update_Key( Button_Vertical, clWhite ) ;

    Right_Button := Update_Key( Right_Button, clLtGray ) ;
    Up_Button := Update_Key( Up_Button, clLtGray ) ;
    Down_Button := Update_Key( Down_Button, clLtGray ) ;
    Left_Button := Update_Key( Left_Button, clLtGray ) ;
    Find_Button := Update_Key( FInd_Button, clLtGray ) ;
    Insert_Here_Button := Update_Key( Insert_Here_Button, clLtGray ) ;
    Remove_Button := Update_Key( Remove_Button, clLtGray ) ;
    Select_Button := Update_Key( Select_Button, clLtGray ) ;
    Prev_Screen_Button := Update_Key( Prev_Screen_Button, clLtGray ) ;
    Next_Screen_Button := Update_Key( Next_Screen_Button, clLtGray ) ;
    F1_Button := Update_Key( F1_Button, clLtGray ) ;
    F2_Button := Update_Key( F2_Button, clLtGray ) ;
    F3_Button := Update_Key( F3_Button, clLtGray ) ;
    F4_Button := Update_Key( F4_Button, clLtGray ) ;
    F5_Button := Update_Key( F5_Button, clLtGray ) ;
    F6_Button := Update_Key( F6_Button, clLtGray ) ;
    F7_Button := Update_Key( F7_Button, clLtGray ) ;
    F8_Button := Update_Key( F8_Button, clLtGray ) ;
    F9_Button := Update_Key( F9_Button, clLtGray ) ;
    F10_Button := Update_Key( F10_Button, clLtGray ) ;
    F11_Button := Update_Key( F11_Button, clLtGray ) ;
    F12_Button := Update_Key( F12_Button, clLtGray ) ;
    F13_Button := Update_Key( F13_Button, clLtGray ) ;
    F14_Button := Update_Key( F14_Button, clLtGray ) ;
    F17_Button := Update_Key( F17_Button, clLtGray ) ;
    F18_Button := Update_Key( F18_Button, clLtGray ) ;
    F19_Button := Update_Key( F19_Button, clLtGray ) ;
    F20_Button := Update_Key( F20_Button, clLtGray ) ;
    Help_Button := Update_Key( Help_Button, clLtGray ) ;
    Do_Button := Update_Key( Do_Button, clLtGray ) ;

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
                Key := Update_Key( TSpeedButton( Panel3.Controls[ Loop ] ), clWhite ) ;
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
      if( Margin = -1 ) then
      begin
          TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
            Spacing + TextSize.Y) ;
          Margin := (ClientSize.X - TotalSize.X + 1) div 2 ;
      end else
      begin
          if( Spacing = -1 ) then
          begin
              TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
                  (Margin + GlyphSize.Y)) ;
              Spacing := (TotalSize.X - TextSize.X) div 2 ;
          end ;
      end ;

      case Layout of
        blGlyphLeft:
          begin
              GlyphPos.X := Margin;
              TextPos.X := GlyphPos.X + GlyphSize.X + Spacing ;
          end ;
      end ;

      { fixup the result variables }
      with GlyphPos do
      begin
          Inc( X, Client.Left + Offset.X ) ;
          Inc( Y, Client.Top + Offset.Y ) ;
      end ;

      OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
    end ; // .Calculate_Layout


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
                if( ( length( Caption ) > 3 ) and ( Color = clWhite ) ) then
                begin
                    Offset.Y := PaintRect.Top + 4 ;
                end else
                begin
                    Offset.Y := ( Height - Y ) div 2 ;
                end ;
                if( Offset.Y < PaintRect.Top ) then
                begin
                    Offset.Y := PaintRect.Top ;
                end ;
                Dummy := pos( #13, Caption ) ;
                S1 := Caption ;
                while( Dummy > 0 ) do // Multi-line
                begin
                    S2 := copy( S1, 1, Dummy - 1 ) ;
                    S1 := copy( S1, Dummy + 1, length( S1 ) ) ;
                    if( S2 = '' ) then
                    begin
                        S2 := ' ' ;
                    end ;
                    S2 := Replace( S2, '_', ' ', 0 );
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
                if( ( length( Caption ) > 3 ) and ( Color = clWhite ) ) then
                begin
                    Dummy := PaintRect.Left + 4 ;
                end ;
                S1 := Replace( S1, '_', ' ', 0 );
                Canvas.TextOut( Dummy + Offset.X, Offset.Y, S1 ) ;
            end else
            begin
                Dummy := ( Width - Canvas.TextWidth( Caption ) ) div 2 ;
                if( ( length( Caption ) > 3 ) and ( Color = clWhite ) ) then
                begin
                    Dummy := PaintRect.Left + 4 ;
                end ;
                if( ( length( Caption ) > 3 ) and ( Color = clWhite ) ) then
                begin
                    Offset.Y := Offset.Y + 4 ;
                end else
                begin
                    Offset.Y := Offset.Y + ( Height - Canvas.TextHeight( Caption ) ) div 2 ;
                end ;
                Canvas.TextOut( Dummy + Offset.X, Offset.Y, Caption ) ;
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
