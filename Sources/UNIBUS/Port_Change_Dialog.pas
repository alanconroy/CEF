{
        Program Name : Port_Change_Dialog
        Package Name : CEF
        Purpose      : Port configuration UI for UNIBUS emulator
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

          This form is used to manage the UNIBUS port configuration.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Port_Change_Dialog ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Grids, StdCtrls, Buttons, ExtCtrls,

     // CEF
     Portman ; // TPort_Manager

type
  TPort_Change_Form = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    Grid: TStringGrid;
    Open_Component: TOpenDialog;
    Delete_Button: TBitBtn;
    Change_Button: TBitBtn;
    BitBtn2: TBitBtn;
    procedure GridClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure Change_ButtonClick(Sender: TObject);
    procedure Delete_ButtonClick(Sender: TObject);

  private // Instance data...
      _Port_Manager : TPort_Manager ;

  protected // Property handlers...
      procedure Set_Port_Manager( Value : TPort_Manager ) ;

  public // API...
      procedure Change_Cell( C, R : integer ) ;

      property Port_Manager : TPort_Manager
          read _Port_Manager
          write Set_Port_Manager ;
  end ;

var Port_Change_Form : TPort_Change_Form ;

implementation

uses // C&C...
     Standard ; // Extension_Pos

{$R *.dfm}

procedure TPort_Change_Form.GridClick( Sender : TObject ) ;

var C, R : integer ;
    Point : TPoint ;

begin
    Point := ScreenToClient( Mouse.CursorPos ) ;
    Grid.MouseToCell( Point.X, Point.Y, C, R ) ;
    if( ( C < 1 ) or ( R < 1 ) ) then // Invalid click
    begin
        Delete_Button.Enabled := False ;
    end else
    begin
        Delete_Button.Enabled := True ;
    end ;
    Change_Button.Enabled := Delete_Button.Enabled ;
end ;


procedure TPort_Change_Form.GridDblClick( Sender : TObject ) ;

var C, R : integer ;
    Point : TPoint ;

begin
    Point := ScreenToClient( Mouse.CursorPos ) ;
    Grid.MouseToCell( Point.X, Point.Y, C, R ) ;
    if( ( C < 1 ) or ( R < 1 ) ) then // Invalid click
    begin
        exit ;
    end ;
    Change_Cell( C, R ) ;
end ;


procedure TPort_Change_Form.Change_ButtonClick( Sender : TObject ) ;

var Rect : TGridRect ;

begin
    Rect := Grid.Selection ;
    Change_Cell( Rect.Left, Rect.Top ) ;
end ;


procedure TPort_Change_Form.Delete_ButtonClick(Sender: TObject);

var Rect : TGridRect ;

begin
    Rect := Grid.Selection ;
    Grid.Cells[ Rect.Left, Rect.Top ] := '' ;
end ;


procedure TPort_Change_Form.Change_Cell( C, R : integer ) ;

var Dummy : integer ;
    S : string ;

begin
    Open_Component.Filename := Grid.Cells[ C, R ] + '.dll' ;
    if( Open_Component.Execute ) then
    begin
        S := Open_Component.Filename ;
        Dummy := Extension_Pos( S ) ;
        Grid.Cells[ C, R ] := copy( S, 1, Dummy - 1 ) ;
    end ;
end ;


// Property handlers...

procedure TPort_Change_Form.Set_Port_Manager( Value : TPort_Manager ) ;

var Loop : integer ;

begin
    _Port_Manager := Value ;
    Grid.RowCount := _Port_Manager.Count + 1 ;
    for Loop := 0 to _Port_Manager.Count - 1 do
    begin
        Grid.Cells[ 0, Loop + 1 ] := Port_Manager.Port_Name( Loop ) ;
    end ;
    Grid.Cells[ 0, 0 ] := 'Port' ;
    Grid.Cells[ 1, 0 ] := 'Terminal' ;
end ;


end.
