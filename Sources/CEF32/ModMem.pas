{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : Memory modification dialog
        Institution  :
        Date Written : 14-Jun-2002
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

          This dialog is used to modify memory.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit ModMem ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, Buttons, ExtCtrls;

type
  TModify_Memory_Dialog = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Label1: TLabel;
    Address: TEdit;
    Label2: TLabel;
    Data: TEdit;
    Literal_Text: TCheckBox;
    GroupBox1: TGroupBox;
    Byte_RB: TRadioButton;
    Word_RB: TRadioButton;
    Long_RB: TRadioButton;
    procedure BitBtn3Click(Sender: TObject);
    procedure AddressChange(Sender: TObject);

  private
    { Private declarations }

  public // API...
      Address_Value : int64 ;
      Base : integer ;

      function Get_Size : integer ;
      procedure Set_Size( Value : integer ) ;

      property Size : integer
          read Get_Size
          write Set_Size ;
  end ;

var Modify_Memory_Dialog : TModify_Memory_Dialog = nil ;

implementation

uses CVT, // Translate_Value
     CommonUt ; // Edit

{$R *.DFM}

// TModify_Memory_Dialog methods...

procedure TModify_Memory_Dialog.BitBtn3Click( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;


procedure TModify_Memory_Dialog.AddressChange( Sender : TObject ) ;

var B : boolean ;
    Dummy : integer ;
    S, Work : string ;
    V : int64 ;

begin
    if( length( Address.Text ) = 0 ) then
    begin
        OK_Button.Enabled := False ;
        exit ;
    end ;
    B := Translate_Value( Edit( Address.Text, 2 ), 0, Base, Address_Value ) ;
    S := Edit( Data.Text, 16 ) ;
    if( length( S ) = 0 ) then
    begin
        OK_Button.Enabled := False ;
        exit ;
    end ;
    if( not Literal_Text.Checked ) then
    begin
        while( length( S ) > 0 ) do
        begin
            Dummy := pos( ' ', S + ' ' ) ;
            Work := copy( S, 1, Dummy - 1 ) ;
            S := copy( S, Dummy + 1, length( S ) ) ;
            B := Translate_Value( Work, 0, Base, V ) ;
            if( not B ) then
            begin
                break ;
            end ;
        end ;
    end ;
    OK_Button.Enabled := B ;
end ;


function TModify_Memory_Dialog.Get_Size : integer ;

begin
    if( Byte_RB.Checked ) then
    begin
        Result := 1 ;
    end else
    if( Word_RB.Checked ) then
    begin
        Result := 2 ;
    end else
    begin
        Result := 4 ;
    end ;
end ;


procedure TModify_Memory_Dialog.Set_Size( Value : integer ) ;

begin
    case Value of
        1 : Byte_RB.Checked := True ;
        2 : Word_RB.Checked := True ;
        4 : Long_RB.Checked := True ;
    end ;
end ;


end.
