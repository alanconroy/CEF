{
        Program Name : Status
        Package Name : CEF32
        Purpose      : Assembly status dialog.
        Institution  : Conroy & Conroy Co.
        Date Written : 31-Dec-2006
        Written By   : Alan Conroy
        Version      : 1.0

	Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *    DATE        BY          REASON                         *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This form is used to show assembly status.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Status ;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TCompile_Status = class(TForm)
    Button: TBitBtn;
    Panel1: TPanel;
    Label1: TLabel;
    Status_Label: TLabel;
    Label2: TLabel;
    Filename_Label: TLabel;
    Label3: TLabel;
    Line_Label: TLabel;
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end ;

var Abort_Compile : boolean ;
    Compile_Status : TCompile_Status ;

implementation

{$R *.DFM}

procedure TCompile_Status.ButtonClick( Sender : TObject ) ;

begin
    Abort_Compile := True ;
    Visible := False ;
end ;



end.
