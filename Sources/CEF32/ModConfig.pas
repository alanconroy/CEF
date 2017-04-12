{
        Program Name : ModConfig
        Package Name : CEF
        Purpose      : CEF Component configuration dialog
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Jun-2003
        Written By   : Alan Conroy
        Version      : 1.0

	Released to the public domain.

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

          This form allows the user to modify component configurations for
        CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit ModConfig ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls, Spin, ComCtrls,

     // C&C...
     TDialogs ; // TDialog

type
  TComponent_Configuration_Dialog = class(TDialog)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    BitBtn2: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Latency: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Read_Latency: TSpinEdit;
    Write_Latency: TSpinEdit;
    CPU: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Clock_Speed: TLabel;
    Clock: TSpinEdit;
    Signals_Tab_Sheet: TTabSheet;
    Memory: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Low_Address: TSpinEdit;
    High_Address: TSpinEdit;
    Help_Button: TBitBtn;
    procedure Button_PanelResize(Sender: TObject);
    procedure ClockChange(Sender: TObject);
    procedure High_AddressChange(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Component_Configuration_Dialog: TComponent_Configuration_Dialog;

implementation

uses // C&C...
     Num1s, // Num1
     VCL_Std, // Adjust_Button_Panel

     // CEF32...
     Textstrs ; // Text_

{$R *.dfm}

procedure TComponent_Configuration_Dialog.Button_PanelResize( Sender : TObject ) ;

begin
    Adjust_Button_Panel( Button_Panel ) ;
end ;


procedure TComponent_Configuration_Dialog.ClockChange( Sender : TObject ) ;

var Dummy : integer ;
    R : extended ;
    S, T : string ;

begin
    R := Clock.Value ;
    S := 'KHz' ;
    if( R > 1000.0 ) then
    begin
        R := R / 1000.0 ;
        S := 'MHz' ;
        if( R > 1000.0 ) then
        begin
            R := R / 1000.0 ;
            S := 'GHz' ;
        end ;
    end else
    begin
        Clock_Speed.Caption := '' ;
        exit ;
    end ;
    T := num1( R ) ;
    Dummy := pos( '.', T ) ;
    if( Dummy > 0 ) then
    begin
        Dummy := Dummy + 2 ;
        if( Dummy > length( T ) ) then
        begin
            Dummy := length( T ) ;
        end ;
        setlength( T, Dummy ) ;
    end else
    begin
        T := T + '.' ;
    end ;
    while( T[ length( T ) ] = '0' ) do
    begin
        setlength( T, length( T ) - 1 ) ; // Trim trailing zeroes
    end ;
    if( T[ length( T ) ] = '.' ) then
    begin
        setlength( T, length( T ) - 1 ) ; // Trim extraneous decimal
    end ;
    S := T + ' ' + S ;
    Clock_Speed.Caption := '(' + S + ')' ;
end ;



procedure TComponent_Configuration_Dialog.High_AddressChange(
    Sender : TObject ) ;

begin
    OK_Button.Enabled := ( Low_Address.Value <= High_Address.Value ) ;
end ;


procedure TComponent_Configuration_Dialog.Help_ButtonClick( Sender: TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;


procedure TComponent_Configuration_Dialog.FormShow( Sender : TObject ) ;

begin
    Caption := Text_Caption_Configure_Component ;
    OK_Button.Caption := Text_Button_Caption_OK_amp ;
    BitBtn2.Caption := Text_Button_Caption_Cancel_amp ;
    Help_Button.Caption := Text_Button_Caption_Help_amp ;
    TabSheet1.Caption := Text_Caption_Attributes ;
    Latency.Caption := Text_Latency ;
    Label1.Caption := Text_Read_Colon ;
    Label2.Caption := Text_Write_Colon ;
    CPU.Caption := Text_CPU ;
    Label5.Caption := Text_Clock_Colon ;
    Clock_Speed.Caption := Text_Clock_Speed ;
    Memory.Caption := Text_Memory ;
    Label7.Caption := Text_Low_Address_Colon ;
    Label8.Caption := Text_High_Address_Colon ;
    Signals_Tab_Sheet.Caption := Text_Caption_Signals ;
end ;


end.
