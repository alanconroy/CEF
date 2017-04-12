unit DOS11_Label;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Spin;

type
  TDOS11_Label_Form = class(TForm)
    Label5: TLabel;
    Proj: TSpinEdit;
    Prog: TSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    FileName: TEdit;
    Ext: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Prot: TSpinEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label11: TLabel;
    Date_Value: TSpinEdit;
    procedure FileNameKeyPress(Sender: TObject; var Key: Char);
    procedure ExtKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function DOS11_Label_Form : TDOS11_Label_Form ;

implementation

{$R *.dfm}

var _DOS11_Label_Form : TDOS11_Label_Form = nil ;

function DOS11_Label_Form : TDOS11_Label_Form ;

begin
    if( _DOS11_Label_Form = nil ) then
    begin
        _DOS11_Label_Form := TDOS11_Label_Form.Create( Application ) ;
    end ;
    Result := _DOS11_Label_Form ;
end ;


procedure TDOS11_Label_Form.FileNameKeyPress(Sender: TObject; var Key: Char);

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    Key := upcase( Key ) ;
    if( pos( Key, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$' ) = 0 ) then
    begin
        Key := #0 ;
        exit ;
    end ;
    if( length( FileName.Text ) > 5 ) then
    begin
        Key := #0 ;
    end ;
end ;


procedure TDOS11_Label_Form.ExtKeyPress(Sender: TObject; var Key: Char);

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    Key := upcase( Key ) ;
    if( pos( Key, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$' ) = 0 ) then
    begin
        Key := #0 ;
        exit ;
    end ;
    if( length( Ext.Text ) > 2 ) then
    begin
        Key := #0 ;
    end ;
end ;


end.
