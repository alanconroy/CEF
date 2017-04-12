unit Selective_Addressing_Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons;

type
  TSelective_Addressing_Dialog = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Unit_Code: TSpinEdit;
    Group_Code: TSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Selective_Addressing_Dialog: TSelective_Addressing_Dialog;

implementation

{$R *.dfm}

end.
