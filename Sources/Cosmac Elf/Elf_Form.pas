{
        Program Name : Cosmac_Elf
        Package Name : CEF
        Purpose      : Cosmac Elf CEF component main form
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2007 by Alan Conroy.  Released to the public domain.

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

          This form represents the visual portion of the Cosmac Elf hardware,
        for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Elf_Form ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls, Menus, Buttons,

     // CEF...
     _CEF ; // TComponent

type
  TFront_Panel_Form = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Debug1: TMenuItem;
    Q_LED: TShape;
    Label1: TLabel;
    Data_High: TPanel;
    Data_Low: TPanel;
    In_Button: TSpeedButton;
    Load_Button: TSpeedButton;
    MP_Button: TSpeedButton;
    Run_Button: TSpeedButton;
    Help1: TMenuItem;
    About1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure Debug1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure In_ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure In_ButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Run_ButtonClick(Sender: TObject);
    procedure MP_ButtonClick(Sender: TObject);
    procedure About1Click(Sender: TObject);

  private // Instance data...

  public // API...
      CPU : TComponent ;
      _UI : TUI_Interface ;
  end ;

implementation

uses // Cosmac Elf...
     AboutBox, // About_Form
     Elf ; // TCosmac_Elf

{$R *.dfm}

// TFront_Panel_Form...

procedure TFront_Panel_Form.Exit1Click(Sender: TObject);

begin
    TCosmac_Elf( CPU )._User_Interface._UI.Terminate ;
end ;


procedure TFront_Panel_Form.Debug1Click(Sender: TObject);

begin
    TCosmac_Elf( CPU )._User_Interface._UI.Hide( True ) ;
end ;


procedure TFront_Panel_Form.FormClose( Sender : TObject ;
    var Action : TCloseAction ) ;

begin
    Action := caFree ;
end ;


procedure TFront_Panel_Form.In_ButtonMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;

begin
    TCosmac_Elf( CPU ).Set_EF4( True ) ;
end ;


procedure TFront_Panel_Form.In_ButtonMouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer) ;

begin
    TCosmac_Elf( CPU ).Set_EF4( False ) ;
end ;


procedure TFront_Panel_Form.Run_ButtonClick(Sender: TObject) ;

begin
    if( Run_Button.Down ) then
    begin
        _UI.Run( True ) ;
    end else
    begin
        _UI.Run( False ) ;
    end ;
end ;


procedure TFront_Panel_Form.MP_ButtonClick( Sender : TObject ) ;

begin
    TCosmac_Elf( CPU ).Memory_Protect( MP_Button.Down ) ;
end ;


procedure TFront_Panel_Form.About1Click( Sender : TObject ) ;

begin
    About_Form.ShowModal ;
end ;


end.

