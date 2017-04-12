{
        Program Name    : New_Watch
        Package Name    : CEF
        Purpose         : Enter/edit watches
        Institution     : Conroy & Conroy Co.
        Date written    : 10-Apr-2005
        Written by      : Alan Conroy
        Version         : 1.0

        Copyright (C) 2005 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

      *******************************************************
      *                                                     *
      *      M O D I F I C A T I O N   H I S T O R Y        *
      *                                                     *
      *    DATE        BY            REASON                 *
      *                                                     *
      *******************************************************

      *******************************************************
      *                                                     *
      *          P R O G R A M   P U R P O S E              *
      *                                                     *
      *******************************************************

        This dialog is used to enter/edit watches for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit New_Watch ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, Spin, StdCtrls, Buttons, ExtCtrls,

     // C&C...
     TDialogs, // TDialog

     // CEF...
     _CEF ; // TCEF_Assembler_Context

type
  TNew_Watch_Dialog = class( TDialog )
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    Cancel_Button: TBitBtn;
    Panel1: TPanel;
    Label1: TLabel;
    Address_Value: TEdit;
    Size: TSpinEdit;
    Label2: TLabel;
    Base_Panel: TPanel;
    Binary_RB: TRadioButton;
    Octal_RB: TRadioButton;
    Decimal_RB: TRadioButton;
    Hexadecimal_RB: TRadioButton;
    Other_RB: TRadioButton;
    Base_Spin: TSpinEdit;
    ASCII_RB: TRadioButton;
    EBCDIC_RB: TRadioButton;
    Radix50_RB: TRadioButton;
    Label3: TLabel;
    Single_RB: TRadioButton;
    Double_RB: TRadioButton;
    Extended_RB: TRadioButton;
    Help_Button: TBitBtn;
    procedure Address_ValueChange(Sender: TObject);
    procedure Address_ValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Single_RBClick(Sender: TObject);
    procedure Double_RBClick(Sender: TObject);
    procedure Extended_RBClick(Sender: TObject);
    procedure Binary_RBClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);

  private
    function Valid : boolean ;
    function Get_Value : int64 ;
    procedure Set_Value( Value : int64 ) ;

  public // API...
      _Assembler : TCEF_Assembler_Context ;
      Base : integer ;
      Minimum, Maximum : int64 ;

      property Value : int64
          read Get_Value
          write Set_Value ;
  end ;

var New_Watch_Dialog : TNew_Watch_Dialog = nil ;

implementation

uses // C&C...
     ASCIIDef, // BS
     Cvt, // Valid_Base
     EBCDICs, // From_EBCDIC
     Num1s, // Num1
     Radix50s ; // Radix50

{$R *.dfm}

procedure TNew_Watch_Dialog.Address_ValueChange( Sender : TObject ) ;

var V : integer ;

begin
    if( _Assembler <> nil ) then
    begin
        if( _Assembler.Symbol_Size( PChar( Address_Value.Text ), V ) ) then
        begin
            if( V = 0 ) then
            begin
                V := 1 ;
            end ;
            Size.Value := V ;
        end ;
    end ;
    OK_Button.Enabled := Valid ;
end ;


function TNew_Watch_Dialog.Valid : boolean ;

var V : int64 ;

begin
    Result := ( length( Address_Value.Text ) > 0 ) ;
    if( Result ) then
    begin
        if( _Assembler <> nil ) then
        begin
            if( not _Assembler.Symbol_Value( PChar( Address_Value.Text ), V ) ) then
            begin
                Result := Valid_Base( Address_Value.Text, Base ) ;
            end ;
        end else
        begin
            Result := Valid_Base( Address_Value.Text, Base ) ;
        end ;
    end ;
    if( Result ) then // Validate range
    begin
        Result := (
                    ( ( Value >= Minimum ) and ( Value <= Maximum ) )
                    or
                    ( Minimum > Maximum )
                  ) ;
    end ;
end ;


function TNew_Watch_Dialog.Get_Value : int64 ;

var V : int64 ;

begin
    if( _Assembler <> nil ) then
    begin
        if( _Assembler.Symbol_Value( PChar( Address_Value.Text ), V ) ) then
        begin
            Result := V ;
            exit ;
        end ;
    end ;
    Result := 0 ;
    try
        Result := strtoint64( CvtB( Base, 10, Address_Value.Text ) ) ;
    except
    end ;
end ;


procedure TNew_Watch_Dialog.Set_Value( Value : int64 ) ;

var S : string ;
    V : record
            case Integer of
               0 : ( I : int64 ) ;
               1 : ( S : single ) ;
               2 : ( D : double ) ;
               3 : ( E : extended ) ;
        end ;

begin
    setlength( S, sizeof( Value ) ) ;
    move( Value, S[ 1 ], length( S ) ) ;
    V.I := Value ;
    case Base of
        0 : Address_Value.Text := S ;
        1 : Address_Value.Text := From_EBCDIC( S ) ;
        50 : Address_Value.Text := Rad( Value ) ;
        -4 : Address_Value.Text := floattostr( V.S ) ;
        -8 : Address_Value.Text := floattostr( V.D ) ;
        -10 : Address_Value.Text := floattostr( V.E ) ;
        else Address_Value.Text := CvtB( 10, Base, num1( Value ) ) ;
    end ;
end ;


procedure TNew_Watch_Dialog.Address_ValueKeyDown( Sender : TObject ;
    var Key : Word ; Shift : TShiftState ) ;

begin
    if( ( Key = VK_RETURN ) and OK_Button.Enabled ) then
    begin
        Key := 0 ;
        ModalResult := mrOK ;
    end ;
end ;


procedure TNew_Watch_Dialog.Single_RBClick(Sender: TObject);

begin
    Size.Value := 4 ;
    Size.Enabled := False ;
end ;


procedure TNew_Watch_Dialog.Double_RBClick(Sender: TObject);

begin
    Size.Value := 8 ;
    Size.Enabled := False ;
end ;


procedure TNew_Watch_Dialog.Extended_RBClick(Sender: TObject);

begin
    Size.Value := 10 ;
    Size.Enabled := False ;
end ;


procedure TNew_Watch_Dialog.Binary_RBClick( Sender : TObject ) ;

begin
    Size.Enabled := True ;
end ;


procedure TNew_Watch_Dialog.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.
