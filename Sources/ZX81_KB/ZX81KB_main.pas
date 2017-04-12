{
        Program Name : ZX81KB_Main
        Package Name : CEF
        Purpose      : Sinclair ZX81 keyboard
        Institution  : Conroy & Conroy Co.
        Date Written : 27-Jan-2009
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

          This is the visual portion of the Sinclair ZX81 keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit ZX81KB_main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // CEF...
     _CEF, // TCable
     TPanels ; // TTransparent_Panel

type
  TMain_Form = class(TForm)
    Panel3: TPanel;
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
    SpeedButton125: TSpeedButton;
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
    Return_Button: TSpeedButton;
    Button_Less_Than: TSpeedButton;
    Space_Button: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
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
    PI_Label: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    procedure SpeedButton110Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    public // API...
      _UI : TUI_Interface ;
      _ZX81KB : TComponent ;
  end ;

implementation

uses // C&C...
     _ASCII, // ESC
     _UE, // TUnified_Exception

     // CEF...
     ZX_81_KB ; // _UI

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
        TZX81KB( _ZX81KB )._Keyboard.Add_Key( Name, Value, State ) ;
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
    if( Button = Left_Shift_Button ) then
    begin
        if( Button.Down ) then
        begin
            Add_Key( 'SHIFT', 135, 1 ) ;
        end else
        begin
            Add_Key( 'SHIFT', 135, 0 ) ;
        end ;
        exit ;
    end ;

    // Handle other special keys...
    if( Button.Caption = '' ) then // Space bar
    begin
        C := ' ' ;
    end else
    if( length( Button.Caption ) = 1 ) then
    begin
        C := Button.Caption[ 1 ] ;
    end else
    if( Button.Caption = 'ENTER' ) then
    begin
        C := CR ;
    end else
    begin // If we get here it is a button with multiple glyphs
        S := Button.Caption ;
        Dummy := pos( #13, S ) ;
        if( Left_Shift_Button.Down ) then
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
        ( not Left_Shift_Button.Down )
      ) then
    begin
        C := lowercase( C )[ 1 ] ;
    end ;

    Add_Key( C, ord( C ), 1 ) ;
    Add_Key( C, ord( C ), 0 ) ;

    if( Left_Shift_Button.Down ) then
    begin
        Add_Key( 'SHIFT', 136, 0 ) ;
    end ;
    Left_Shift_Button.Down := False ;
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
        Result.Tag := Original.Tag ;
        Original.Glyph := nil ;
        Original.Free ;
    end ;

var Changes : boolean ;
    Dummy, Loop : integer ;
    Key : TKey_Button ;

begin
    // Setup keyboard...
    Return_Button := Update_Key( Return_Button, clWhite ) ;
    Left_Shift_Button := Update_Key( Left_Shift_Button, clWhite ) ;

    Button_1 := Update_Key( Button_1, clWhite ) ;
    Button_2 := Update_Key( Button_2, clWhite ) ;
    Button_3 := Update_Key( Button_3, clWhite ) ;
    Button_4 := Update_Key( Button_4, clWhite ) ;
    Button_5 := Update_Key( Button_5, clWhite ) ;
    Button_6 := Update_Key( Button_6, clWhite ) ;
    Button_7 := Update_Key( Button_7, clWhite ) ;
    Button_8 := Update_Key( Button_8, clWhite ) ;
    Button_9 := Update_Key( Button_9, clWhite ) ;
    Space_Button := Update_Key( Space_Button, clWhite ) ;
    SpeedButton89 := Update_Key( SpeedButton89, clWhite ) ;
    Button_Less_Than := Update_Key( Button_Less_Than, clWhite ) ;
    SpeedButton4 := Update_Key( SpeedButton4, clWhite ) ;
    SpeedButton5 := Update_Key( SpeedButton5, clWhite ) ;
    SpeedButton6 := Update_Key( SpeedButton6, clWhite ) ;
    SpeedButton7 := Update_Key( SpeedButton7, clWhite ) ;
    SpeedButton8 := Update_Key( SpeedButton8, clWhite ) ;
    SpeedButton9 := Update_Key( SpeedButton9, clWhite ) ;
    SpeedButton10 := Update_Key( SpeedButton10, clWhite ) ;
    SpeedButton11 := Update_Key( SpeedButton11, clWhite ) ;
    SpeedButton111 := Update_Key( SpeedButton111, clWhite ) ;
    SpeedButton112 := Update_Key( SpeedButton112, clWhite ) ;
    SpeedButton113 := Update_Key( SpeedButton113, clWhite ) ;
    SpeedButton114 := Update_Key( SpeedButton114, clWhite ) ;
    SpeedButton115 := Update_Key( SpeedButton115, clWhite ) ;
    SpeedButton116 := Update_Key( SpeedButton116, clWhite ) ;
    SpeedButton119 := Update_Key( SpeedButton119, clWhite ) ;
    SpeedButton120 := Update_Key( SpeedButton120, clWhite ) ;
    SpeedButton121 := Update_Key( SpeedButton121, clWhite ) ;
    SpeedButton122 := Update_Key( SpeedButton122, clWhite ) ;
    SpeedButton125 := Update_Key( SpeedButton125, clWhite ) ;
    SpeedButton131 := Update_Key( SpeedButton131, clWhite ) ;
    SpeedButton132 := Update_Key( SpeedButton132, clWhite ) ;
    SpeedButton133 := Update_Key( SpeedButton133, clWhite ) ;
    SpeedButton134 := Update_Key( SpeedButton134, clWhite ) ;
    SpeedButton135 := Update_Key( SpeedButton135, clWhite ) ;
    SpeedButton136 := Update_Key( SpeedButton136, clWhite ) ;
    SpeedButton137 := Update_Key( SpeedButton137, clWhite ) ;
    SpeedButton138 := Update_Key( SpeedButton138, clWhite ) ;
    SpeedButton139 := Update_Key( SpeedButton139, clWhite ) ;
    SpeedButton140 := Update_Key( SpeedButton140, clWhite ) ;
    SpeedButton141 := Update_Key( SpeedButton141, clWhite ) ;
    SpeedButton142 := Update_Key( SpeedButton142, clWhite ) ;
    SpeedButton143 := Update_Key( SpeedButton143, clWhite ) ;
    SpeedButton144 := Update_Key( SpeedButton144, clWhite ) ;
    SpeedButton145 := Update_Key( SpeedButton145, clWhite ) ;

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
end ; // TMain_Form.FormCreate


procedure TKey_Button.Paint ;

var Offset : TPoint ;

    procedure Draw( const Client : TRect ; const Offset : TPoint ) ;

    var GlyphPos: TPoint;
        ClientSize, GlyphSize : TPoint ;
        M : integer ;
    begin
        if( ( Glyph = nil ) or ( Glyph.Width = 0 ) or ( Glyph.Height = 0 ) ) then
        begin
            Exit ;
        end ;
        M := Margin ;
        if( M = -1 ) then
        begin
            M := 0 ;
        end ;

        { calculate the item sizes }
        ClientSize := Point( Client.Right - Client.Left, Client.Bottom - Client.Top ) ;

        GlyphSize := Point(Glyph.Width div NumGlyphs, Glyph.Height) ;

        GlyphPos.X := ClientSize.X - GlyphSize.X - M + Offset.X ;
        GlyphPos.Y := ClientSize.Y - GlyphSize.Y - M + Offset.Y ;

        Canvas.Draw( GlyphPos.X, GlyphPos.Y, Glyph ) ;
    end ;

var PaintRect, Saved_PaintRect : TRect ;
    DrawFlags : Integer ;
    Dummy : integer ;
    Red_Font : TFont ;
    S1 : string ;
    Saved : THandle ;
    Size : TSize ;
    This_Y, Y : integer ;

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
    if( Caption <> '' ) then
    begin
        Saved := selectobject( Canvas.Handle, Font.Handle ) ;
        try
            Dummy := pos( #13, Caption ) ;
            if( Dummy > 0 ) then
            begin
                Y := Margin ;
                if( Y = -1 ) then
                begin
                    Y := 0 ;
                end ;

                // Write main text...
                S1 := copy( Caption, 1, Dummy - 1 ) ;
                This_Y := PaintRect.Bottom - Canvas.TextHeight( S1 ) - Y ;
                Canvas.TextOut( PaintRect.Left + Offset.X, This_Y  + Offset.Y, S1 ) ;

                // Do red text...
                Red_Font := TFont.Create ;
                try
                    Red_Font.Assign( Font ) ;
                    S1 := copy( Caption, Dummy + 1, length( Caption ) ) ;
                    if( S1 <> 'FUNCTION' ) then
                    begin
                        if( S1 = 'GRAPHICS' ) then
                        begin
                            Red_Font.Size := Red_Font.Size - 9 ;
                        end else
                        if( pos( S1[ 1 ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ) > 0 ) then
                        begin
                            Red_Font.Size := Red_Font.Size - 8 ;
                        end else
                        if( S1 = '£' ) then
                        begin
                            Red_Font.Size := 14 ;
                        end else
                        begin
                            Red_Font.Size := Red_Font.Size - 2 ;
                        end ;
                    end ;
                    selectobject( Canvas.Handle, Red_Font.Handle ) ;
                    GetTextExtentPoint( Canvas.Handle, PChar( S1 ), length( S1 ), Size ) ;
                    Dummy := PaintRect.Right - PaintRect.Left - Y - 1 - Size.cx ;
                    if( Dummy < 0 ) then
                    begin
                        Dummy := 0 ;
                    end ;
                    SetBkMode( Canvas.Handle, Windows.TRANSPARENT ) ;
                    SetTextColor( Canvas.Handle, clRed ) ;
                    TextOut( Canvas.Handle, Dummy + Offset.X, Y + Offset.Y + 1, PChar( S1 ), length( S1 ) ) ;
                finally
                    Red_Font.Free ;
                end ;
            end else
            begin
                if( Caption = 'SHIFT' ) then
                begin
                    // Center text...
                    Dummy := ( PaintRect.Right - PaintRect.Left - Canvas.TextWidth( Caption ) ) div 2 + PaintRect.Left ;
                    This_Y := ( PaintRect.Bottom - PaintRect.Top - Canvas.TextHeight( Caption ) ) div 2 + PaintRect.Top ;
                    Canvas.TextOut( Dummy + Offset.X, This_Y + Offset.Y, Caption ) ;
                end else
                begin
                    Dummy := PaintRect.Left ;
                    This_Y := PaintRect.Bottom - Canvas.TextHeight( Caption ) - Margin ;
                    Canvas.TextOut( Dummy + Offset.X, This_Y + Offset.Y, Caption ) ;
                end ;
            end ;
        finally
            selectobject( Canvas.Handle, Saved ) ;
        end ;
    end ;
    Draw( PaintRect, Offset ) ;
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
