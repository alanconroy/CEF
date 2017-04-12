{
        Program Name : ModRef
        Package Name : CEF
        Purpose      : CEF32 register modification dialog
        Institution  :
        Date Written : 27-Apr-2002
        Written By   : Alan Conroy
        Version      : 1.1

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

          This dialog is used to query the user for register value changes.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit ModReg ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, Buttons, ExtCtrls ;

type
  TModify_Register = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    Cancel_Button: TBitBtn;
    Help_Button: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    procedure Edit1Change(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private // Private declarations

  public // API...
      Base : integer ; // Default radix
      Size : integer ; // Size, in bits
      Value : int64 ; // Converted value that was entered
  end ;

var Modify_Register : TModify_Register ;

implementation

uses // Other...
     CVT, // CVTB

     // CEF32...
     TextStrs ; // Text_*

{$R *.DFM}

procedure TModify_Register.Edit1Change( Sender : TObject ) ;

var E : integer ;
    _Value : int64 ;

begin
    // Handle null...
    if( length( Edit1.Text ) = 0 ) then
    begin
        OK_Button.Enabled := False ;
        exit ;
    end ;

    // Check for valid digits for base...
    E := Base ;
    OK_Button.Enabled := Translate_Value( Edit1.Text, 0, E, _Value ) ;
    if( not OK_Button.Enabled ) then // Invalid value
    begin
        exit ;
    end ;

    // Check for value size...
    if( ( _Value shr Size ) <> 0 ) then // Too big
    begin
        OK_Button.Enabled := False ;
        exit ;
    end ;
    Value := _Value ;
end ;


procedure TModify_Register.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;


procedure TModify_Register.FormShow( Sender : TObject ) ;

begin
    Label1.Caption := Text_Value_Colon ;
    OK_Button.Caption := Text_Button_Caption_OK_amp ;
    Cancel_Button.Caption := Text_Button_Caption_Cancel_amp ;
    Help_Button.Caption := Text_Button_Caption_Help_amp ;
end ;



end.
