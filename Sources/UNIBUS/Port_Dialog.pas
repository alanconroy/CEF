{
        Program Name : UNIBUS
        Package Name : CEF
        Purpose      : DEC PDP-11 UNIBUS CEF emulator device dialog
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2008 by Alan Conroy.  Released to the public domain.

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

          This form is used to configure the UNIBUS peripherals.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Port_Dialog ;

interface

uses // Borland...
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, Buttons, ExtCtrls, Grids, Spin ;

type
  TPort_Form = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Open_Dialog: TOpenDialog;
    Label1: TLabel;
    Console: TSpinEdit;
    Label2: TLabel;
    KL11: TSpinEdit;
    Label3: TLabel;
    DL11D: TSpinEdit;
    Label4: TLabel;
    KE11: TSpinEdit;
    Support_Switch_Register: TCheckBox;
    procedure ConsoleChange(Sender: TObject);

  public // API...
end ;

var Port_Form : TPort_Form ;

implementation

{$R *.dfm}



procedure TPort_Form.ConsoleChange(Sender: TObject);

begin
    DL11D.MaxValue := 31 - KL11.Value ;
end ;


end.
