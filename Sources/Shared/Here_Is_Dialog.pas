unit Here_Is_Dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  THere_Is_Form = class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Here_Is_Form: THere_Is_Form;

implementation

{$R *.dfm}

end.
