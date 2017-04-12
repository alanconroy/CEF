{
        Program Name : CEF32
        Module Name  : Info
        Package Name : CEF
        Purpose      : CEF Assembly information
        Institution  : Conroy & Conroy Co.
        Date Written : 10-Nov-1992
        Written By   : Alan Conroy
        Version      : 1.3

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

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

          This dialog shows information on the last successful assembly.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Info ;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TAssembly_Information = class(TForm)
    BitBtn1: TBitBtn;
    Help_Button: TBitBtn;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Source_Lines: TLabel;
    Data_Size: TLabel;
    Code_Size: TLabel;
    procedure Help_ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Assembly_Information : TAssembly_Information = nil ;

implementation

{$R *.DFM}

procedure TAssembly_Information.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.
