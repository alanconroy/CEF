unit VT_Dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TVT_Form = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    BitBtn2: TBitBtn;
    Memo1: TMemo;
    procedure Memo1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VT_Form: TVT_Form;

implementation

{$R *.dfm}

procedure TVT_Form.Memo1Change(Sender: TObject) ;

var Dummy, Dummy1 : integer ;
    S : string ;

begin
    for Dummy := 0 to Memo1.Lines.Count - 1 do
    begin
        S := trim( Memo1.Lines[ Dummy ] ) ;
        if( S <> '' ) then
        begin
            if( not trystrtoint( S, Dummy1 ) ) then
            begin
                OK_Button.Enabled := False ;
                exit;
            end ;
        end ;
    end ;
    OK_Button.Enabled := True ;
end ;


end.
