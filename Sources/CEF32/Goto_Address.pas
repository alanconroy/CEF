{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 Goto Address dialog
        Institution  :
        Date Written : 4-Aug-2001
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

          This unit defines the dialog for querying the user for an address.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Goto_Address ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, Buttons, ExtCtrls ;

type
  TGoto_Address_Form = class(TForm)
    Button_Panel: TPanel;
    Label1: TLabel;
    OK_Button: TBitBtn;
    BitBtn2: TBitBtn;
    ComboBox1: TComboBox;
    Help_Button: TBitBtn;
    procedure ComboBox1KeyPress(Sender: TObject; var Key: Char);
    procedure OK_ButtonClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);

  private // Internal...
      _Base : integer ;

      procedure Set_Base( Value : integer ) ;

  public // API...
      property Base : integer
                   read _Base
                   write Set_Base ;
  end ;

var Goto_Address_Form : TGoto_Address_Form ;

implementation

uses CVT ; // Valid_Base

{$R *.DFM}

// TGoto_Address_Form methods...

procedure TGoto_Address_Form.ComboBox1KeyPress( Sender : TObject ;
    var Key : Char ) ;

begin
    if( Key <> #8 ) then
    begin
        Key := upcase( Key ) ;
        if( not Valid_Base( Key, Base ) ) then
        begin
            Key := #0 ;
            MessageBeep( 0 ) ;
        end ;
    end ;
end ;


procedure TGoto_Address_Form.OK_ButtonClick( Sender : TObject ) ;

begin
    if( ComboBox1.Items.Indexof( ComboBox1.Text ) = -1 ) then
    begin
        ComboBox1.Items.Add( ComboBox1.Text ) ;
    end ;
end ;


// Internal...

procedure TGoto_Address_Form.Set_Base( Value : integer ) ;

begin
    if( Base <> Value ) then
    begin
        _Base := Value ;
        ComboBox1.Items.Clear ;
    end ;
end ;


procedure TGoto_Address_Form.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.
