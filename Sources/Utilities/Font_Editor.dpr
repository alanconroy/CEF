program Font_Editor;

uses
  Forms,
  Font_Editor_Form in 'Font_Editor_Form.pas' {Form1},
  Edit_Font in 'Edit_Font.pas' {Edit_Font_Form},
  New_Font_Form in 'New_Font_Form.pas' {New_Font_Dialog};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CEF Font Editor';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TNew_Font_Dialog, New_Font_Dialog);
  Application.Run;
end.
