{
        Program Name : Keymap_Form 
        Package Name : CEF
        Purpose      : Keyboard mapping UI
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

          This form is the UI for keyboard mapping.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Keymap_Form ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TKey_Mapper_Dialog = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    F1: TComboBox;
    F2: TComboBox;
    F3: TComboBox;
    F4: TComboBox;
    F5: TComboBox;
    F6: TComboBox;
    F7: TComboBox;
    F8: TComboBox;
    F9: TComboBox;
    F10: TComboBox;
    F11: TComboBox;
    F12: TComboBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    PAGE_DOWN: TComboBox;
    PAGE_UP: TComboBox;
    PAUSE: TComboBox;
    CLEAR: TComboBox;
    CANCEL: TComboBox;
    END_CB: TComboBox;
    SELECT: TComboBox;
    PRINT: TComboBox;
    INSERT_CB: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Key_Mapper_Dialog : TKey_Mapper_Dialog = nil ;

implementation

{$R *.dfm}

end.
