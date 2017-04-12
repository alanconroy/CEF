unit New_Font_Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Buttons, ExtCtrls;

type
  TNew_Font_Dialog = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Pixel_Width: TSpinEdit;
    Raster_Height: TSpinEdit;
    Max_Chars: TSpinEdit;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  New_Font_Dialog: TNew_Font_Dialog;

implementation

{$R *.dfm}

end.
