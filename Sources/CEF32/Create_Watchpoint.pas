{
        Program Name    : Create_Watchpoint
        Package Name    : CEF
        Purpose         : Memory watchpoints entry dialog
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

        This dialog is used to create watchpoints for CEF.

      *******************************************************
      *                                                     *
      *        C O N T R I B U T O R S                      *
      *                                                     *
      *******************************************************

      AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Create_Watchpoint;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, StdCtrls, Buttons, Spin,

     // CEF...
     CEF ; // TCPU

type
  TNew_Watchpoint = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Address: TEdit;
    GroupBox1: TGroupBox;
    Read: TCheckBox;
    Write: TCheckBox;
    Label2: TLabel;
    Size: TSpinEdit;
    Help_Button: TBitBtn;
    procedure AddressChange(Sender: TObject);
    procedure Button_PanelResize(Sender: TObject);
    procedure OK_ButtonClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public // API...
      Base : integer ;
      CPU : TComponent ;
      Low, High : int64 ;
  end ;

var New_Watchpoint : TNew_Watchpoint ;

implementation

uses CVT, // Valid_Base
     VCL_Std ; // Adjust_Button_Panel

{$R *.dfm}

procedure TNew_Watchpoint.AddressChange( Sender : TObject ) ;

var I : int64 ;
    Index : integer ;
    Res : boolean ;
    S : string ;
    Size : integer ;

begin
    Res := ( Address.Text <> '' )
           and
           ( Read.Checked or Write.Checked or ( not GroupBox1.Visible ) ) ;
    if( CPU = nil ) then
    begin
        Res := Res and Valid_Base( uppercase( Address.Text ), Base ) ;
        if( Res ) then
        begin
            I := strtoint( CvtB( Base, 10, uppercase( Address.Text ) ) ) ;
            Res := ( ( I >= Low ) and ( I <= High ) ) ;
        end ;
    end else
    begin
        if( Res ) then
        begin
            Res := False ;
            Index := 0 ;
            while( True ) do
            begin
                Size := CPU.CPU.Register_Size( Index ) ;
                if( Size = 0 ) then
                begin
                    break ;
                end ;
                S := CPU.CPU.Register_Name( Index ) ;
                if( uppercase( S ) = uppercase( Address.Text ) ) then
                begin
                    Res := True ;
                    break ;
                end ;
                inc( Index ) ;
            end ;
        end ;
    end ;
    OK_Button.Enabled := Res ;
end ; // TNew_Watchpoint.AddressChange


procedure TNew_Watchpoint.Button_PanelResize( Sender : TObject ) ;

begin
    Adjust_Button_Panel( Button_Panel ) ;
end ;


procedure TNew_Watchpoint.OK_ButtonClick( Sender : TObject ) ;

begin
    Address.Text := uppercase( Address.Text ) ;
    ModalResult := mrOK ;
end ;


procedure TNew_Watchpoint.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.
