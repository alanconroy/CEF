{
        Program Name : Intellec8
        Package Name : CEF
        Purpose      : Intellec8 CEF component main form
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

          This form represents the Intel Intellec8 front-panel.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Intellec8_Form ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls, Menus, Buttons ;

type
  TFront_Panel_Form = class(TForm)
    Control_Box: TGroupBox;
    Data_Box: TGroupBox;
    Address_Box: TGroupBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Debug1: TMenuItem;
    Run_Button: TSpeedButton;
    GroupBox1: TGroupBox;
    Memory_LED: TShape;
    Label3: TLabel;
    IO_LED: TShape;
    Label4: TLabel;
    Fetch: TShape;
    Label5: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Write_LED: TShape;
    Read_LED: TShape;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Int_LED: TShape;
    DA: TShape;
    Stack_LED: TShape;
    Label11: TLabel;
    Run_LED: TShape;
    Shape2: TShape;
    Label12: TLabel;
    Shape3: TShape;
    Label13: TLabel;
    Shape4: TShape;
    Label14: TLabel;
    Label15: TLabel;
    Shape5: TShape;
    Label16: TLabel;
    Shape6: TShape;
    Label17: TLabel;
    Shape7: TShape;
    Label18: TLabel;
    Shape8: TShape;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Help1: TMenuItem;
    About1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Debug1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Run_ButtonClick(Sender: TObject);
    procedure About1Click(Sender: TObject);

  private // Instance data...

  public
    { Public declarations }
  end ;

implementation

uses // CEF...
     CEF, // TUI_Interface

     // Intellec8...
     Intellec_8 , AboutBox; // _UI

{$R *.dfm}

procedure TFront_Panel_Form.FormResize( Sender : TObject ) ;

var Control : TControl ;
    Dummy, Loop : integer ;

begin
    Dummy := Control_Box.Width ;
    Control_Box.Width := ClientWidth - COntrol_Box.Left * 2 ;
    Address_Box.Width := Control_Box.Width ;
    Data_Box.Width := Control_Box.Width ;
    Dummy := Control_Box.Width - Dummy ; // Difference in width
    for Loop := 0 to Data_Box.ControlCount - 1 do
    begin
        Control := Data_Box.Controls[ Loop ] ;
        Control.Left := Control.Left + Dummy ;
    end ;
    for Loop := 0 to Address_Box.ControlCount - 1 do
    begin
        Control := Address_Box.Controls[ Loop ] ;
        Control.Left := Control.Left + Dummy ;
    end ;
end ;



procedure TFront_Panel_Form.Exit1Click(Sender: TObject);

begin
    _Intellec8._User_Interface._UI.Terminate ;
end ;


procedure TFront_Panel_Form.Debug1Click(Sender: TObject);

begin
    _Intellec8._User_Interface._UI.Hide( True ) ;
end ;


procedure TFront_Panel_Form.FormClose( Sender : TObject ;
    var Action : TCloseAction ) ;

begin
    Action := caFree ;
end ;


procedure TFront_Panel_Form.Run_ButtonClick( Sender : TObject ) ;

begin
    if( Run_Button.Down ) then
    begin
        _Intellec8._User_Interface._UI.Run( True ) ;
        Run_LED.Brush.Color := clRed ;
    end else
    begin
        _Intellec8._User_Interface._UI.Run( False ) ;
        Run_LED.Brush.Color := clMaroon ;
    end ;
end ;


procedure TFront_Panel_Form.About1Click( Sender : TObject ) ;

begin
    About_Form.ShowModal ;
end ;


end.

