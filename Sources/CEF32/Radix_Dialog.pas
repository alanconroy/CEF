{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 radix dialog
        Institution  :
        Date Written : 4-Aug-2001
        Written By   : Alan Conroy
        Version      : 1.0

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

          This unit defines the dialog for selecting a radix from 2 to 49.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Radix_Dialog ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, Spin, Buttons, ExtCtrls;

type
  TRadix_Form = class(TForm)
    Label1: TLabel;
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Spin_Edit: TSpinEdit;
    Help_Button: TBitBtn;
    procedure Help_ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Radix_Form : TRadix_Form ;

implementation

{$R *.DFM}

procedure TRadix_Form.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.
