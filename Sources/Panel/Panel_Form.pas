{
        Program Name : Panel
        Package Name : CEF
        Purpose      : Generic Front Panel
        Institution  : Conroy & Conroy Co.
        Date Written : 13-July-2005
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2005-2007 by Alan Conroy.  Released to the public domain.

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

          The form for a generic Front Panel.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Panel_Form ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls ;

type
  TFront_Panel_Form = class(TForm)
    Control_Box: TGroupBox;
    Data_Box: TGroupBox;
    Address_Box: TGroupBox;
    Read_LED: TShape;
    Write_LED: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Memory_LED: TShape;
    IO_LED: TShape;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end ;

implementation

{$R *.dfm}

procedure TFront_Panel_Form.FormResize(Sender: TObject);

var Control : TControl ;
    Dummy, Loop : integer ;

begin
    Dummy := Control_Box.Width ;
    Control_Box.Width := ClientWidth - COntrol_Box.Left * 2 ;
    Address_Box.Width := Control_Box.Width ;
    Data_Box.Width := Control_Box.Width ;
    Dummy := Control_Box.Width - Dummy ; // Difference in width
    for Loop := 0 to Data_Box.ControlCount - 1 do
    begin
        Control := Data_Box.Controls[ Loop ] ;
        Control.Left := Control.Left + Dummy ;
    end ;
    for Loop := 0 to Address_Box.ControlCount - 1 do
    begin
        Control := Address_Box.Controls[ Loop ] ;
        Control.Left := Control.Left + Dummy ;
    end ;
end ;



end.

