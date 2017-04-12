{
        Program Name : AboutBox
        Package Name : CEF
        Purpose      : DEC PDP-11 UNIBUS CEF component about form
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

          This provides a UNIBUS system front panel about box.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit AboutBox ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TAbout_Form = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var About_Form : TAbout_Form ;

implementation

{$R *.dfm}

end.
