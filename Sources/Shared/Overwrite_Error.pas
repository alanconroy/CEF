unit Overwrite_Error;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TOverwrite_Form = class(TForm)
    Message_Text: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Overwrite_Form: TOverwrite_Form;

implementation

uses // CEF...
     Textstrs ; // Text_*

{$R *.dfm}

procedure TOverwrite_Form.FormShow(Sender: TObject);

begin
    Message_Text.Caption := Text_File_Already_Exists ;
    BitBtn1.Caption := Text_Button_Caption_Skip ;
    BitBtn2.Caption := Text_Button_Caption_Skip_All ;
    BitBtn3.Caption := Text_Button_Caption_Cancel ;
    BitBtn4.Caption := Text_Button_Caption_Overwrite ;
    BitBtn5.Caption := Text_Button_Caption_Overwrite_All ;
end ;


end.
