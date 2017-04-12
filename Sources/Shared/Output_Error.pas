unit Output_Error;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TOutput_Error_Form = class(TForm)
    Message_Text: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Output_Error_Form: TOutput_Error_Form;

implementation

uses // CEF...
     TextStrs ; // Text_*

{$R *.dfm}

procedure TOutput_Error_Form.FormShow(Sender: TObject);

begin
    BitBtn1.Caption := Text_Button_Caption_Skip ;
    BitBtn2.Caption := Text_Button_Caption_Skip_All ;
    BitBtn3.Caption := Text_Button_Caption_Cancel ;
    BitBtn4.Caption := Text_Button_Caption_Retry ;
end ;


end.
