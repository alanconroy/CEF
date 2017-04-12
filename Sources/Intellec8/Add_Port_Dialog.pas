{
        Program Name : Add_Port_Dialog
        Package Name : CEF
        Purpose      : Altair 8800 emulator port configuration UI
        Institution  : Conroy & Conroy Co.
        Date Written : 13-Jan-2007
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2007 by Alan Conroy.  Released to the public domain.

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

          Provides a port configuration UI for the Altair 8800 microcomputer
          emulator.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Add_Port_Dialog ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Buttons, ExtCtrls;

type
  TAdd_Port_Form = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Count: TSpinEdit;
    Label2: TLabel;
    Start: TSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Add_Port_Form: TAdd_Port_Form;

implementation

{$R *.dfm}

end.
