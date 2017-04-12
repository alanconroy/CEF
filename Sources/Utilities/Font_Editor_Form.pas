unit Font_Editor_Form ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls,

  Edit_Font ; // Edit_Font_Form

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    Exit1: TMenuItem;
    New1: TMenuItem;
    SaveDialog: TSaveDialog;
    PageControl1: TPageControl;
    N1: TMenuItem;
    N2: TMenuItem;
    Close1: TMenuItem;
    Closeall1: TMenuItem;
    Saveall1: TMenuItem;
    N3: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Saveall1Click(Sender: TObject);
    procedure Closeall1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    File_Name : string ;

    procedure Close_Tab( Tab : integer ) ;
    function Edit_Font_Form : TEdit_Font_Form ;
    procedure Set_Save_All ;
    procedure CB_Idle(Sender: TObject; var Done: Boolean) ;

  public
    procedure Save ;
    procedure Save_As ;
  end;

var Form1: TForm1;

implementation

{$R *.dfm}

uses New_Font_Form ;


procedure TForm1.Close_Tab( Tab : integer ) ;

var Edit_Font_Form : TEdit_Font_Form ;
    Saved : integer ;
    T : TTabSheet ;

begin
    Saved := PageControl1.ActivePageIndex ;
    try
        T := PageControl1.Pages[ Tab ] ;
        Edit_Font_Form := TEdit_Font_Form( T.Controls[ 0 ] ) ;
        if( not Edit_Font_Form.Allow_Close ) then
        begin
            exit ;
        end ;
        PageControl1.ActivePageIndex := Tab ;
        if( Edit_Font_Form.Dirty ) then
        begin
            case MessageBox( 0, 'Do you want to save your existing changes?', pchar( 'Unsaved changes in ' + T.Caption ), mb_YesNoCancel ) of
                id_Cancel : exit ;
                id_Yes :
                    begin
                        if( T.Caption = '<new>' ) then
                        begin
                            Save_As ;
                        end else
                        begin
                            Save ;
                        end ;
                        if( Edit_Font_Form.Dirty ) then
                        begin
                            exit ;
                        end ;
                    end ;
            end ;
        end ; // if( Edit_Font_Form.Dirty )
        Edit_Font_Form.Parent.Free ;
    finally
        if( Saved <> Tab ) then
        begin
            PageControl1.ActivePageIndex := Saved ;
        end ;
    end ;
end ; // TForm1.Close_Tab


procedure TForm1.Close1Click(Sender: TObject) ;

begin
    Close_Tab( PageControl1.ActivePageIndex ) ;
end ;


procedure TForm1.Closeall1Click(Sender: TObject) ;

var I : integer ;

begin
    for I := PageControl1.PageCount - 1 downto 0 do
    begin
        Close_Tab( I ) ;
    end ;
    if( PageControl1.PageCount > 0 ) then
    begin
        PageControl1.ActivePageIndex := 0 ;
    end ;
end ;


procedure TForm1.Set_Save_All ;

var E : boolean ;
    Edit_Font_Form : TEdit_Font_Form ;
    I : integer ;

begin
    E := False ;
    for I := 0 to PageControl1.PageCount - 1 do
    begin
        Edit_Font_Form := TEdit_Font_Form( PageControl1.Pages[ I ].Controls[ 0 ] ) ;
        if( Edit_Font_Form.Dirty ) then
        begin
            E := True ;
            break ;
        end ;
    end ;
    Saveall1.Enabled := E ;
end ;


function TForm1.Edit_Font_Form : TEdit_Font_Form ;

begin
    if( PageControl1.PageCount = 0 ) then
    begin
        Result := nil ;
        exit ;
    end ;
    Result := TEdit_Font_Form( PageControl1.ActivePage.Controls[ 0 ] ) ;
end ;


procedure TForm1.Save ;

begin
    Edit_Font_Form.Save( PageControl1.ActivePage.Caption ) ;
end ;


procedure TForm1.Save_As ;

begin
    if( SaveDialog.Execute ) then
    begin
        File_Name := SaveDialog.Filename ;
        Edit_Font_Form.Save( File_Name ) ;
        PageControl1.ActivePage.Caption := File_Name ;
    end ;
end ;


procedure TForm1.Exit1Click(Sender: TObject) ;

var Loop : integer ;

begin
    for Loop := 0 to PageControl1.PageCount - 1 do
    begin
        Close_Tab( Loop ) ;
    end ;
    Close ;
end ;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean) ;

begin
    Closeall1Click( nil ) ;
    CanClose := PageControl1.PageCount = 0 ;
end ;


procedure TForm1.CB_Idle(Sender: TObject; var Done: Boolean) ;

begin
    if( Edit_Font_Form <> nil ) then
    begin
        Edit_Font_Form.Check_For_Bitmap ;
        Save1.Enabled := Edit_Font_Form.Dirty ;
        Saveas1.Enabled := PageControl1.PageCount > 0 ;
    end ;
end ;


procedure TForm1.FormShow(Sender: TObject) ;

begin
    Application.OnIdle := CB_Idle ;
end ;


procedure TForm1.New1Click(Sender: TObject);

var Edit_Font_Form : TEdit_Font_Form ;
    T : TTabSheet ;

begin
    if( New_Font_Dialog.ShowModal = mrOk ) then
    begin
        File_Name := '' ;
        T := TTabSheet.Create( PageControl1 ) ;
        T.Caption := '<new>' ;
        T.PageControl := PageControl1 ;
        Edit_Font_Form := TEdit_Font_Form.Create( PageControl1 ) ;
        Edit_Font_Form.Parent := T ;
        Edit_Font_Form.Align := alClient ;
        Edit_Font_Form.Set_Metrics( New_Font_Dialog.Pixel_Width.Value,
            New_Font_Dialog.Raster_Height.Value, New_Font_Dialog.Max_Chars.Value ) ;
        Edit_Font_Form.Visible := True ;
        Save1.Enabled := False ;
        Saveas1.Enabled := True ;
        Set_Save_All ;
        PageControl1.ActivePage := T ;
        Caption := 'CEF Font Editor - <new>' ;
    end ;
end ;


procedure TForm1.Open1Click(Sender: TObject) ;

var Edit_Font_Form : TEdit_Font_Form ;
    T : TTabsheet ;

begin
    if( OpenDialog1.Execute ) then
    begin
        File_Name := OpenDialog1.Filename ;
        T := TTabSheet.Create( PageControl1 ) ;
        T.Caption := File_Name ;
        T.PageControl := PageControl1 ;
        Edit_Font_Form := TEdit_Font_Form.Create( PageControl1 ) ;
        Edit_Font_Form.Parent := T ;
        Edit_Font_Form.Align := alClient ;
        Edit_Font_Form.Open( File_Name ) ;
        Edit_Font_Form.Visible := True ;
        Save1.Enabled := False ;
        Saveas1.Enabled := True ;
        Set_Save_All ;
        PageControl1.ActivePage := T ;
        Caption := 'CEF Font Editor - ' + File_Name ;
    end ;
end ;


procedure TForm1.PageControl1Change(Sender: TObject) ;

begin
    Save1.Enabled := Edit_Font_Form.Dirty ;
    Saveas1.Enabled := PageControl1.PageCount > 0 ;
    Set_Save_All ;
end ;


procedure TForm1.Save1Click(Sender: TObject) ;

begin
    Save ;
    Save1.Enabled := Edit_Font_Form.Dirty ;
    Saveas1.Enabled := Edit_Font_Form.Dirty ;
    Set_Save_All ;
end ;


procedure TForm1.Saveall1Click(Sender: TObject) ;

var Edit_Font_Form : TEdit_Font_Form ;
    I, Saved : integer ;

begin
    Saved := PageControl1.ActivePageIndex ;
    for I := 0 to PageControl1.PageCount - 1 do
    begin
        PageControl1.ActivePageIndex := I ;
        Edit_Font_Form := TEdit_Font_Form( PageControl1.Pages[ I ].Controls[ 0 ] ) ;
        if( Edit_Font_Form.Dirty ) then
        begin
            Save ;
        end ;
    end ;
    PageControl1.ActivePageIndex := Saved ;
    Save1.Enabled := Edit_Font_Form.Dirty ;
    Saveas1.Enabled := Edit_Font_Form.Dirty ;
    Set_Save_All ;
end ;


procedure TForm1.Saveas1Click(Sender: TObject) ;

begin
    Save_As ;
    if( File_Name = '' ) then
    begin
        Save1.Enabled := False ;
        Caption := 'CEF Font Editor - <new>' ;
    end else
    begin
        Caption := 'CEF Font Editor - ' + File_Name ;
    end ;
    Saveas1.Enabled := Edit_Font_Form.Dirty ;
    Set_Save_All ;
end ;


end.
