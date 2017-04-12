{
        Program Name : CEF32
        Package Name : CEF32
        Purpose      : About CEF32 dialog
        Institution  :
        Date Written : 27-Apr-2000
        Written By   : Alan Conroy
        Version      : 1.0

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.
        
        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON                         

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This unit contains the About box for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit About ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, Buttons;

type
  TAbout_Box = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Version_Label: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  About_Box: TAbout_Box;

implementation

{$R *.DFM}

uses // C&C...
     _Applic, // Application_Manager

     // CEF32...
     TextStrs ; // Text_*

procedure TAbout_Box.FormShow( Sender : TObject ) ;

begin
    Caption := Text_Caption_About ;
    Label1.Caption := Text_Computer_Emulation_Framework ;
    Label2.Caption := Text_Generic_Computer_Emulator ;
    BitBtn1.Caption := Text_Button_Caption_OK_amp ;
end ;


procedure TAbout_Box.FormCreate(Sender: TObject);

begin
    Version_Label.Caption := Application_Manager^.Version ;
end ;


end.
