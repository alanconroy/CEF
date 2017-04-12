{
        Program Name : Port_IO_Form
        Package Name : CEF32
        Purpose      : CEF32 port specification form
        Institution  :
        Date Written : 17-Feb-2008
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

          This form allows the user to specify a port and an I/O size.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Port_IO_Form ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Spin, StdCtrls, Buttons, ExtCtrls,

     // C&C...
     CVT, // Valid_Base
     CommonUt ; // Edit

type
  TPort_IO_Dialog = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    Cancel_Button: TBitBtn;
    Label1: TLabel;
    Port: TEdit;
    Label2: TLabel;
    Size: TSpinEdit;
    Label3: TLabel;
    Value: TEdit;
    procedure PortChange(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private // Instance data...
      _Base : integer ;

  protected // Property handlers...
      procedure Set_Base( Value : integer ) ;

  public // API...
      property Base : integer
          read _Base
          write Set_Base ;
  end ;

var Port_IO_Dialog : TPort_IO_Dialog ;

implementation

uses // CEF...
     TextStrs ; // Text_*

{$R *.dfm}

// TPort_IO_Dialog methods...

procedure TPort_IO_Dialog.PortChange( Sender : TObject ) ;

begin
    OK_Button.Enabled := ( Edit( Port.Text, -1 ) <> '' )
                         and
                         ( Edit( Value.Text, -1 ) <> '' )
                         and
                         Valid_Base( Port.Text, Base )
                         and
                         Valid_Base( Value.Text, Base ) ;
end ;


// Property handlers...

procedure TPort_IO_Dialog.Set_Base( Value : integer ) ;

begin
    _Base := Value ;
    PortChange( nil ) ;
end ;


procedure TPort_IO_Dialog.FormShow(Sender: TObject);

begin
    Caption := Text_Caption_Port_IO ;
    Label1.Caption := Text_Port_Colon ;
    Label2.Caption := Text_Size_Bits_Colon ;
    Label3.Caption := Text_Value_Colon ;
    OK_Button.Caption := Text_Button_Caption_OK ;
    Cancel_Button.Caption := Text_Button_Caption_Cancel ;
end ;


end.
