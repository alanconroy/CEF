{
        Program Name : Edit_Font
        Package Name : CEF32
        Purpose      : Font editor
        Institution  : Conroy & Conroy Co.
        Date Written : 31-Dec-2009
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

          This form is used to display formatted tape data.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Edit_Font ;

interface

uses // Pascal...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, StdCtrls, Grids, Buttons;

type
  TEdit_Font_Form = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Index_Label: TLabel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Preview_Image: TImage;
    DrawGrid1: TDrawGrid;
    Image2: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Edit_Index_Label: TLabel;
    Save_Edit_Button: TSpeedButton;
    Non_Display: TCheckBox;
    Label4: TLabel;
    Edit1: TEdit;
    Copy_Button: TSpeedButton;
    Paste_Button: TSpeedButton;
    Panel4: TPanel;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure Edit_Index_LabelClick(Sender: TObject);
    procedure Paste_ButtonClick(Sender: TObject);
    procedure Copy_ButtonClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Non_DisplayClick(Sender: TObject);
    procedure Save_Edit_ButtonClick(Sender: TObject);
    procedure DrawGrid1Click(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Image1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ScrollBox1Resize(Sender: TObject);
  private
    _Height, _Width : integer ;
    Chars : TList ;
    Comments : TStringList ;
    Edit_Image : TBitmap ;

    procedure Clear_Chars ;

  public // API...
    Dirty : boolean ;

    function Allow_Close : Boolean ;
    procedure Open( Name : string ) ;
    procedure Save( const Name : string ) ;
    procedure Set_Metrics( Pixel_Width, Raster_Height, Max_Chars : integer ) ;
    procedure Check_For_Bitmap ;
    procedure Edit_Glyph( Index : integer ) ;
  end ;


implementation

{$R *.dfm}

uses // Borland...
     Clipbrd,

     // Third-party...
     CommonUt, // Edit
     CVT, // cvtb
     UE ; // ERT

// TEdit_Font_Form methods...

procedure TEdit_Font_Form.Check_For_Bitmap ;

begin
    Paste_Button.Enabled := Clipboard.HasFormat( CF_BITMAP ) ;
end ;


procedure TEdit_Font_Form.Clear_Chars ;

var Dummy : integer ;

begin
    if( Chars <> nil ) then
    begin
        for Dummy := 0 to Chars.Count - 1 do
        begin
            TInteger_List( Chars[ Dummy ] ).Free ;
            Chars[ Dummy ] := nil ;
        end ;
        Chars.Free ;
    end ;
    Chars := TList.Create ;
    Comments := TStringList.Create ;
end ;


procedure TEdit_Font_Form.Copy_ButtonClick(Sender: TObject) ;

begin
    Clipboard.Assign( Edit_Image ) ;
end ;


procedure TEdit_Font_Form.DrawGrid1Click( Sender : TObject ) ;

var Point : TPoint ;
    C, R, Color : integer ;

begin
    if( Edit_Image = nil ) then
    begin
        exit ;
    end ;
    Point := DrawGrid1.ScreenToClient( Mouse.CursorPos ) ;
    R := Point.Y div 9 ;
    C := Point.X div 9 ;
    Color := Edit_Image.Canvas.Pixels[ C, R ] ;
    if( Color = clBlack ) then
    begin
        Color := clWhite ;
    end else
    begin
        Color := clBlack ;
    end ;
    Edit_Image.Canvas.Pixels[ C, R ] := Color ;
    Image2.Picture.Bitmap.Assign( Edit_Image ) ;
    Drawgrid1.Repaint ;
    Save_Edit_Button.Enabled := True ;
end ;


procedure TEdit_Font_Form.DrawGrid1DrawCell(Sender: TObject; ACol,
    ARow: Integer; Rect: TRect; State: TGridDrawState) ;

begin
    if( Edit_Image = nil ) then
    begin
        DrawGrid1.Canvas.Brush.Color := clWhite ;
    end else
    begin
        DrawGrid1.Canvas.Brush.Color := Edit_Image.Canvas.Pixels[ ACol, ARow ] ;
    end ;
    DrawGrid1.Canvas.FillRect( Rect ) ;
end ;


procedure TEdit_Font_Form.Edit1Change(Sender: TObject) ;

begin
    Save_Edit_Button.Enabled := True ;
end ;


procedure TEdit_Font_Form.Edit_Index_LabelClick(Sender: TObject) ;

var I : integer ;
    S : string ;

begin
    I := 0 ;
    if( Edit_Index_Label.Caption <> '' ) then
    begin
        I := strtoint( Edit_Index_Label.Caption ) ;
    end ;
    S := inttostr( I ) ;
    if InputQuery( 'Glyph index', 'Enter the glyph index to edit', S ) then
    begin
        try
            I := strtoint( S ) ;
        except
            MessageBeep( 0 ) ;
            exit ;
        end ;
        if( ( I < 0 ) or ( I > Chars.Count - 1 ) ) then
        begin
            MessageBeep( 0 ) ;
            exit ;
        end ;
        Edit_Glyph( I ) ;
    end ;
end ;


function TEdit_Font_Form.Allow_Close : Boolean ;

begin
    Result := True ;
    if( Save_Edit_Button.Enabled ) then
    begin
        case MessageBox( 0, 'Glyph has changed.  Do you want to save the changes', 'Unsaved changes', mb_YesNoCancel ) of
            id_Cancel : Result := False ;
            id_Yes :
                begin
                    Save_Edit_ButtonClick( nil ) ;
                end ;
        end ;
    end ;
end ;


procedure TEdit_Font_Form.FormDestroy(Sender: TObject) ;

begin
    Clear_Chars ;
    Chars.Free ;
    Chars := nil ;
    Comments.Free ;
    Comments := nil ;
end ;


procedure TEdit_Font_Form.Edit_Glyph( Index : integer ) ;

var C, R, Columns : integer ;
    Src, Dst : TRect ;

begin
    if( ( Index < Chars.Count ) and ( inttostr( Index ) <> Edit_Index_Label.Caption ) ) then
    begin
        if( Save_Edit_Button.Enabled ) then
        begin
            case MessageBox( 0, 'Glyph has changed.  Do you want to save the changes', 'Unsaved changes', mb_YesNoCancel ) of
                id_Cancel : exit ;
                id_Yes :
                    begin
                        Save_Edit_ButtonClick( nil ) ;
                    end ;
            end ;
        end ;
        Non_Display.OnClick := nil ;
        Non_Display.Checked := ( ( Chars[ Index ] = nil ) or ( TInteger_List( Chars[ Index ] ).Count = 0 ) ) ;
        Non_Display.OnClick := Non_DisplayClick ;
        Save_Edit_Button.Enabled := False ;
        Copy_Button.Enabled := True ;
        Edit_Index_Label.Caption := inttostr( Index ) ;
        Non_Display.Enabled := True ;
        Image2.Height := _Height ;
        Image2.Width := _Width ;
        Edit_Image := TBitmap.Create ;
        Edit_Image.Height := _Height ;
        Edit_Image.Width := _Width ;
        if( Non_Display.Checked ) then
        begin
            Image2.Picture.Bitmap.Assign( Edit_Image ) ;
            Edit_Image.Free ;
            Edit_Image := nil ;
        end else
        begin
            Columns := Image1.Width div _Width ; // Number of characters that fit across
            R := Index div Columns ;
            C := Index - R * Columns ;
            Edit_Image := TBitmap.Create ;
            Edit_Image.Height := _Height ;
            Edit_Image.Width := _Width ;
            Dst.Top := 0 ;
            Dst.Left := 0 ;
            Dst.Bottom := _Height ;
            Dst.Right := _Width ;
            Src.Top := R * _Height ;
            Src.Bottom := Src.Top + _Height ;
            Src.Left := C * _Width ;
            Src.Right := Src.Left + _Width ;
            Edit_Image.Canvas.CopyRect( Dst, Image1.Picture.Bitmap.Canvas, Src ) ;
            Image2.Picture.Bitmap.Assign( Edit_Image ) ;
        end ;
        DrawGrid1.Repaint ;
        Edit1.OnChange := nil ;
        Edit1.Text := Comments[ Index ] ;
        Edit1.OnChange := Edit1Change ;
    end ;
end ; // TEdit_Font_Form.Edit_Glyph


procedure TEdit_Font_Form.Image1Click(Sender: TObject) ;

var Point : TPoint ;
    C, R, Columns, Index : integer ;

begin
    Point := Image1.ScreenToClient( Mouse.CursorPos ) ;
    R := Point.Y div _Height ;
    C := Point.X div _Width ;
    Columns := Image1.Width div _Width ; // Number of characters that fit across
    Index := C + R * Columns ;
    Edit_Glyph( Index ) ;
end ; // TEdit_Font_Form.Image1Click


procedure TEdit_Font_Form.Image1MouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer) ;

var Bitmap : TBitmap ;
    C, R, Columns, Index : integer ;
    Src, Dst : TRect ;

begin
    R := Y div _Height ;
    C := X div _Width ;
    Columns := Image1.Width div _Width ; // Number of characters that fit across
    Index := C + R * Columns ;
    if( Index < Chars.Count ) then
    begin
        Index_Label.Caption := inttostr( Index ) ;
        Bitmap := TBitmap.Create ;
        Bitmap.Height := _Height ;
        Bitmap.Width := _Width ;
        Dst.Top := 0 ;
        Dst.Left := 0 ;
        Dst.Bottom := _Height ;
        Dst.Right := _Width ;
        Src.Top := R * _Height ;
        Src.Bottom := Src.Top + _Height ;
        Src.Left := C * _Width ;
        Src.Right := Src.Left + _Width ;
        Bitmap.Canvas.CopyRect( Dst, Image1.Picture.Bitmap.Canvas, Src ) ;
        Preview_Image.Height := ( ( _Height * 10 div _Width ) * Preview_Image.Width ) div 10 ; // Maintain proper aspect ratio
        Preview_Image.Picture.Bitmap.Assign( Bitmap ) ;
        Bitmap.Free ;
    end ;
end ;


procedure TEdit_Font_Form.Non_DisplayClick(Sender: TObject) ;

begin
    Save_Edit_Button.Enabled := True ;
    Edit_Image := TBitmap.Create ;
    Edit_Image.Height := _Height ;
    Edit_Image.Width := _Width ;
    if( Non_Display.Checked ) then
    begin
        Image2.Picture.Bitmap.Assign( Edit_Image ) ;
        Edit_Image.Free ;
        Edit_Image := nil ;
        TInteger_List( Chars[ strtoint( Edit_Index_Label.Caption ) ] ).Free ;
        Chars[ strtoint( Edit_Index_Label.Caption ) ] := nil ;
    end else
    begin
        if( Chars[ strtoint( Edit_Index_Label.Caption ) ] = nil ) then
        begin
            Chars[ strtoint( Edit_Index_Label.Caption ) ] := TInteger_List.Create ;
        end ;
    end ;
end ;


procedure TEdit_Font_Form.Save( const Name : string ) ;

var Dummy, I, Index : integer ;
    F : textfile ;
    Rows : TInteger_List ;

begin
    // Open the file...
    assignfile( F, Name ) ;
    {$I-}
    rewrite( F ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( DOS_ERT( Dummy ) + ' while creating ' + Name ) ;
        exit ;
    end ;
    try
        {$I-}
        writeln( F, 'RASTER' ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( DOS_ERT( Dummy ) + ' while writing ' + Name ) ;
            exit ;
        end ;
        {$I-}
        writeln( F, _Width ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( DOS_ERT( Dummy ) + ' while writing ' + Name ) ;
            exit ;
        end ;
        for Index := 0 to Chars.Count - 1 do
        begin
            Rows := TInteger_List( Chars[ Index ] ) ;
            if( ( Rows = nil ) or ( Rows.Count = 0 ) ) then
            begin
                write( F, '0' ) ; // Invisible character
            end else
            begin
                for I := 0 to Rows.Count - 1 do
                begin
                    write( F, Rows[ I ] );
                    if( I < Rows.Count - 1 ) then
                    begin
                        write( F, ', ' ) ;
                    end ;
                end ;
            end ;
            if( Comments[ Index ] <> '' ) then
            begin
                write( F, ' ; ', Comments[ Index ] ) ;
            end ;
            writeln( F ) ;
        end ;
    finally
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
        Dirty := False ;
    end ;
end ; // TEdit_Font_Form.Save


procedure TEdit_Font_Form.Set_Metrics( Pixel_Width, Raster_Height, Max_Chars : integer ) ;

begin
    _Width := Pixel_Width ;
    _Height := Raster_Height ;
    Clear_Chars ;
    while( Max_Chars > 0 ) do
    begin
        dec( Max_Chars ) ;
        Chars.Add( TInteger_List.Create ) ;
        Comments.Add( '' ) ;
    end ;
    Dirty := False ;
    DrawGrid1.Height := _Height * 9 + 3 ;
    DrawGrid1.Width := _Width * 9 + 3 ;
    DrawGrid1.ColCount := _Width ;
    DrawGrid1.RowCount := _Height ;
    Edit_Image.Free ;
    Edit_Image := TBitmap.Create ;
    Preview_Image.Picture.Bitmap.Assign( Edit_Image ) ;
    Image2.Picture.Bitmap.Assign( Edit_Image ) ;
    Edit_Image.Free ;
    Edit_Image := nil ;
    DrawGrid1.Repaint ;
    Save_Edit_Button.Enabled := False ;
    Copy_Button.Enabled := False ;
    ScrollBox1Resize( nil ) ;
    Edit_Index_Label.Caption := '' ;
    Non_Display.Enabled := False ;
    Edit1.Text := '' ;
end ; // TEdit_Font_Form.Set_Metrics


procedure TEdit_Font_Form.SpeedButton1Click(Sender: TObject) ;

var B, Dummy, Index, C, R, Work : integer ;
    Rows : TInteger_List ;

begin
    if( Save_Edit_Button.Enabled ) then
    begin
        case MessageBox( 0, 'Glyph has changed.  Do you want to save the changes', 'Unsaved changes', mb_YesNoCancel ) of
            id_Cancel : exit ;
            id_Yes :
                begin
                    Save_Edit_ButtonClick( nil ) ;
                end ;
        end ;
    end ;
    B := 1 ;
    R := ( _Width + 1 ) div 2 ;
    for C := 1 to R do
    begin
        B := B shl 1 ;
    end ;
    for Index := 0 to Chars.Count - 1 do
    begin
        Rows := TInteger_List( Chars[ Index ] ) ;
        if( Rows <> nil ) then
        begin
            for R := 0 to Rows.Count - 1 do
            begin
                Dummy := Rows[ R ] ;
                Work := 0 ;
                for C := 0 to _Width - 1 do
                begin
                    if( ( C and 1 ) = 0 ) then // Every-other pixel
                    begin
                        if( ( Dummy and 3 ) <> 0 ) then
                        begin
                            Work := Work or B ;
                        end ;
                        Work := Work shr 1 ;
                    end ;
                    Dummy := Dummy shr 1 ;
                end ; // for C := 0 to _Width - 1
                Rows[ R ] := Work ;
            end ; // for R := 0 to Rows.Count - 1
        end ; // if( Rows <> nil )
    end ; // for Index := 0 to Chars.Count - 1
    _Width := ( _Width + 1 ) div 2 ;
    Edit_Image.Free ;
    Edit_Image := TBitmap.Create ;
    Preview_Image.Picture.Bitmap.Assign( Edit_Image ) ;
    Image2.Picture.Bitmap.Assign( Edit_Image ) ;
    Edit_Image.Free ;
    Edit_Image := nil ;
    DrawGrid1.ColCount := _Width ;
    DrawGrid1.Width := _Width * 9 + 3 ;
    DrawGrid1.Repaint ;
    Save_Edit_Button.Enabled := False ;
    Copy_Button.Enabled := False ;
    ScrollBox1Resize( nil ) ;
    Edit_Index_Label.Caption := '' ;
    Non_Display.Enabled := False ;
    Edit1.Text := '' ;
end ; // TEdit_Font_Form.SpeedButton1Click


procedure TEdit_Font_Form.Open( Name : string ) ;

var Dummy : integer ;
    F : textfile ;
    First : boolean ;
    Rows : Tinteger_List ;
    S, Work : string ;
    Saved : integer ;

begin
    // Open the file...
    assignfile( F, Name ) ;
    Saved := FileMode ;
    FileMode := fmOpenRead ;
    {$I-}
    reset( F ) ;
    {$I+}
    FileMode := Saved ;
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( ERT( Dummy ) + ' while opening ' + Name ) ;
        exit ;
    end ;

    // Create new character set...
    if( Chars <> nil ) then
    begin
        for Dummy := 0 to Chars.Count - 1 do
        begin
            TInteger_List( Chars[ Dummy ] ).Free ;
            Chars[ Dummy ] := nil ;
        end ;
        Chars.Free ;
    end ;
    Chars := TList.Create ;
    Comments := TStringList.Create ;

    // Read in font...
    try
        _Width := 8 ;
        _Height := 8 ;
        First := True ;
        while( not eof( F ) ) do
        begin
            // Process next character...
            {$I-}
            readln( F, S ) ;
            {$I+}
            if( IOResult <> 0 ) then
            begin
                break ;
            end ;
            if( First ) then // First line in file...
            begin
                First := False ;
                if( Edit( S, -1 ) = 'RASTER' ) then
                begin
                    {$I-}
                    readln( F, S ) ;
                    {$I+}
                    if( IOResult <> 0 ) then
                    begin
                        break ;
                    end ;
                    try
                        _Width := strtoint( S ) ;
                    except
                    end ;

                    // Get first line of raster data
                    {$I-}
                    readln( F, S ) ;
                    {$I+}
                    if( IOResult <> 0 ) then
                    begin
                        break ;
                    end ;
                end ;
            end ;

            // Process this character...
            Dummy := pos( ';', S + ';' ) ;
            Comments.Add( trim( copy( S, Dummy + 1, length( S ) ) ) ) ;
            S := trim( copy( S, 1, Dummy - 1 ) ) ; // Trim comments
            if( S = '0' ) then // Invisible character
            begin
                S := '' ;
            end ;
            Rows := Tinteger_List.Create ;
            while( length( S ) > 0 ) do
            begin
                Dummy := pos( ',', S + ',' ) ;
                Work := copy( S, 1, Dummy - 1 ) ;
                S := trim( copy( S, Dummy + 1, length( S ) ) ) ;
                Work := Edit( Work, -1 ) ;
                try
                    if( copy( Work, 1, 2 ) = '0B' ) then
                    begin
                        Rows.Add( strtoint( CVTB( 2, 10, copy( Work, 3, length( Work ) ) ) ) ) ;
                    end else
                    if( copy( Work, 1, 2 ) = '0X' ) then
                    begin
                        Rows.Add( strtoint( CVTB( 16, 10, copy( Work, 3, length( Work ) ) ) ) ) ;
                    end else
                    if( copy( Work, 1, 1 ) = '0' ) then
                    begin
                        Rows.Add( strtoint( CVTB( 8, 10, Work ) ) ) ;
                    end else
                    if( copy( Work, 1, 1 ) = '$' ) then
                    begin
                        Rows.Add( strtoint( CVTB( 16, 10, Work ) ) ) ;
                    end else
                    if( Work = '' ) then
                    begin
                        Rows.Add( 0 ) ;
                    end else
                    begin
                        Rows.Add( strtoint( Work ) ) ;
                    end ;
                except
                end ;
                if( Rows.Count > _Height ) then
                begin
                    _Height := Rows.Count ;
                end ;
            end ; // while( ( length( S ) > 0 ) and ( Line < 64 ) )
            Chars.Add( Rows ) ;
        end ; // while( not eof( F ) )
    finally
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
        Dirty := False ;
        DrawGrid1.Height := _Height * 9 + 3 ;
        DrawGrid1.Width := _Width * 9 + 3 ;
        DrawGrid1.ColCount := _Width ;
        DrawGrid1.RowCount := _Height ;
        Edit_Image.Free ;
        Edit_Image := TBitmap.Create ;
        Preview_Image.Picture.Bitmap.Assign( Edit_Image ) ;
        Image2.Picture.Bitmap.Assign( Edit_Image ) ;
        Edit_Image.Free ;
        Edit_Image := nil ;
        DrawGrid1.Repaint ;
        Save_Edit_Button.Enabled := False ;
        Copy_Button.Enabled := False ;
        ScrollBox1Resize( nil ) ;
        Edit_Index_Label.Caption := '' ;
        Non_Display.Enabled := False ;
        Edit1.Text := '' ;
    end ;
end ; // TEdit_Font_Form.Open


procedure TEdit_Font_Form.Panel1Resize(Sender: TObject) ;

begin
    Edit1.Width := Panel1.ClientWidth - Edit1.Left - 16 ;
end ;


procedure TEdit_Font_Form.Paste_ButtonClick(Sender: TObject) ;

var Bitmap : TBitmap ;
    Dst, Src : TRect ;

begin
    Bitmap := TBitmap.Create ;
    Bitmap.Assign( Clipboard ) ;
    Dst.Top := 0 ;
    Dst.Left := 0 ;
    Dst.Bottom := _Height ;
    Dst.Right := _Width ;
    Src.Top := 0 ;
    Src.Left := 0 ;
    Src.Bottom := _Height ;
    Src.Right := _Width ;
    Edit_Image.Canvas.Brush.Color := ClWhite ;
    Edit_Image.Canvas.FillRect( Dst ) ;
    Edit_Image.Canvas.CopyRect( Dst, Bitmap.Canvas, Src ) ;
    Bitmap.Free ;

    Save_Edit_Button.Enabled := True ;
    DrawGrid1.Repaint ;
end ;


procedure TEdit_Font_Form.Save_Edit_ButtonClick(Sender: TObject) ;

var Dst, Src : TRect ;
    Columns, Dummy, Index, C, R : integer ;
    Rows : TInteger_List ;

begin
    Save_Edit_Button.Enabled := False ;
    Dirty := True ;

    Columns := Image1.Width div _Width ;
    Index := strtoint( Edit_Index_Label.Caption ) ;
    R := Index div Columns ;
    C := Index - R * Columns ;
    Dst.Top := R * _Height ;
    Dst.Left := C * _Width ;
    Dst.Bottom := Dst.Top + _Height ;
    Dst.Right := Dst.Left + _Width ;
    Src.Top := 0 ;
    Src.Bottom := _Height ;
    Src.Left := 0 ;
    Src.Right := _Width ;
    Image1.Picture.Bitmap.Canvas.CopyRect( Dst, Edit_Image.Canvas, Src ) ;
    ScrollBox1.Repaint ;
    Preview_Image.Picture.Bitmap.Assign( Edit_Image ) ;
    Preview_Image.Repaint ;
    Rows := TInteger_List( Chars[ Index ] ) ;
    if( Rows <> nil ) then
    begin
        while( Rows.Count < _Height ) do
        begin
            Rows.Add( 0 ) ;
        end ;
        for R := 0 to Rows.Count - 1 do
        begin
            Dummy := 0 ;
            for C := 0 to _Width - 1 do
            begin
                Dummy := Dummy shl 1 ;
                if( Edit_Image.Canvas.Pixels[ C, R ] = clBlack ) then
                begin
                    Dummy := Dummy or 1 ;
                end ;
            end ; // for C := 0 to _Width - 1
            Rows[ R ] := Dummy ;
        end ; // for R := 0 to Rows.Count - 1
    end ; // if( Rows <> nil )
    Comments[ Index ] := Edit1.Text ;
end ; // TEdit_Font_Form.Save_Edit_ButtonClick


procedure TEdit_Font_Form.ScrollBox1Resize(Sender: TObject) ;

var Bitmap : TBitmap ;
    C, R, P : integer ;
    Columns, Dummy : integer ;
    Rows : TInteger_List ;
    Rect : TRect ;
    X, Y : integer ;

begin
    Columns := Image1.Width div _Width ; // Number of characters that fit across
    Image1.Width := Columns * _Width ;
    Image1.Height := Chars.Count div Columns * _Height ;

    // Now that the scrollbox may have resized, recalculate...
    Columns := Image1.Width div _Width ; // Number of characters that fit across
    Image1.Width := Columns * _Width ;
    Image1.Height := ( Chars.Count + Columns - 1 ) div Columns * _Height ;

    // Build font image...
    Bitmap := TBitmap.Create ;
    Bitmap.Height := Image1.Height ;
    Bitmap.Width := Image1.Width ;
    Rect.Top := 0 ;
    Rect.Left := 0 ;
    Rect.Right := Bitmap.Width - 1 ;
    Rect.Bottom := Bitmap.Height - 1 ;
    Bitmap.Canvas.Brush.Color := clwhite ;
    Bitmap.Canvas.FillRect( Rect ) ;

    for C := 0 to Chars.Count - 1 do
    begin
        Y := C div Columns ; // Which row
        X := C - Y * Columns ; // Which column
        Y := Y * _Height ; // Y pixel
        X := X * _Width ; // X pixel
        Rows := TInteger_List( Chars[ C ] ) ;
        for R := 0 to Rows.Count - 1 do
        begin
            Dummy := Rows[ R ] ;
            P := _Width + X - 1 ;
            while( Dummy > 0 ) do
            begin
                if( ( Dummy and 1 ) <> 0 ) then
                begin
                    Bitmap.Canvas.Pixels[ P, Y ] := clBlack ;
                end ;
                dec( P ) ;
                Dummy := Dummy shr 1 ;
            end ; // while( Dummy > 0 )
            inc( Y ) ;
        end ; // for R := 0 to Rows.Count - 1
    end ; // for C := 0 to Chars.Count - 1

    Image1.Picture.Bitmap.Assign( Bitmap ) ;
    Bitmap.Free ;
end ;


end.
