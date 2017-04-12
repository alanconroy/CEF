unit Goto_Page_Form ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Buttons, ExtCtrls;

type
  TGoto_Page_Dialog = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Goto_Page_Dialog : TGoto_Page_Dialog = nil;

implementation

{$R *.dfm}

end.
