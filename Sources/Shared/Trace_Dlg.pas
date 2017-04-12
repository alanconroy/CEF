{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 Trace configuration form
        Institution  :
        Date Written : 27-Apr-2000
        Written By   : Alan Conroy
        Version      : 1.1

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *    DATE        BY          REASON                         *
        *                                                           *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This form is used to configure trace options.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Trace_Dlg ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Spin;

type
  TTrace_Form = class(TForm)
    Button_Panel: TPanel;
    OK_button: TBitBtn;
    Cancel_Button: TBitBtn;
    Help_Button: TBitBtn;
    Main_Panel: TPanel;
    Top_Panel: TPanel;
    Label1: TLabel;
    Count: TSpinEdit;
    Clear_All_Button: TSpeedButton;
    procedure Clear_All_ButtonClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Trace_Form : TTrace_Form = nil ;

implementation

uses // CEF...
     Textstrs ; // Text_*

{$R *.dfm}

procedure TTrace_Form.Clear_All_ButtonClick( Sender : TObject ) ;

var Loop : integer ;
    CB : TCheckbox ;

begin
    for Loop := 0 to Main_Panel.ControlCount -1 do
    begin
        CB := TCheckBox( Main_Panel.Controls[ Loop ] ) ;
        CB.Checked := False ;
    end ;
end ;


procedure TTrace_Form.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



procedure TTrace_Form.FormShow(Sender: TObject) ;

begin
    Caption := Text_Caption_Traces ;
    OK_Button.Caption := Text_Button_Caption_OK ;
    Cancel_Button.Caption := Text_Button_Caption_Cancel ;
    Help_Button.Caption := Text_Button_Caption_Help ;
    Label1.Caption := Text_Maximum_Events_To_Trace ;
    Clear_All_Button.Caption := Text_Button_Caption_Clear_All ;
end ;


end.

