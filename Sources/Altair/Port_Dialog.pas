{
        Program Name : MITS_Altair
        Package Name : CEF
        Purpose      : MITS Altair 8800 CEF emulator port assignment dialog
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

          This dialog is used to assign cable devices to the ports of an
        Altair 8800/IMSAI 8080.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Port_Dialog ;

interface

uses // Pascal...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls, Grids;

type
  TPort_Form = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Port_Grid: TStringGrid;
    Panel1: TPanel;
    Delete_Button: TSpeedButton;
    Add_Button: TSpeedButton;
    Connect_Button: TSpeedButton;
    Open_Dialog: TOpenDialog;
    Disconnect_Button: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure Port_GridClick(Sender: TObject);
    procedure Add_ButtonClick(Sender: TObject);
    procedure Delete_ButtonClick(Sender: TObject);
    procedure Connect_ButtonClick(Sender: TObject);
    procedure Disconnect_ButtonClick(Sender: TObject);

  private // Instance data...
        Selected_Row : integer ;
        Selected_Column : integer ;

  public // API...
    Connections : array[ 0..255 ] of string ;

    procedure Clear ;
    procedure Define( Port : integer ; Read : boolean ; Text : string ) ;
    function Defined( _Read : boolean ; Port : integer ) : string ;
end ;

var Port_Form : TPort_Form ;

implementation

uses // C&C...
     CVT, // CVTB

     // CEF...
     _CEF, // TUI_Interface

     // Altair...
     Add_Port_Dialog, // TAdd_Port_Form
     MITS_Altair, // _UI
     Port_Change_Dialog ; // TPort_Change_Form

{$R *.dfm}

//TPort_Form methods...

procedure TPort_Form.FormCreate( Sender : TObject ) ;

var Loop : integer ;
    S, S1 : string ;

begin
    Port_Grid.Cells[ 0, 0 ] := 'Ports' ;
    Port_Grid.Cells[ 1, 0 ] := 'Input' ;
    Port_Grid.Cells[ 2, 0 ] := 'Output' ;
    for Loop := 0 to 255 do
    begin
        S := CVTB( 10, 16, inttostr( Loop ) ) ;
        if( length( S ) = 1 ) then
        begin
            S := '0' + S ;
        end ;
        S1 := CVTB( 10, 8, inttostr( Loop ) ) ;
        while( length( S1 ) < 3 ) do
        begin
            S1 := '0' + S1 ;
        end ;
        Port_Grid.Cells[ 0, Loop + 1 ] :=
            inttostr( Loop ) + ' (' + S + '/' + S1 + ')' ;
        Port_Grid.Cells[ 1, Loop + 1 ] := 'Unassigned' ;
        Port_Grid.Cells[ 2, Loop + 1 ] := 'Unassigned' ;
    end ;
end ;



// API...

procedure TPort_Form.Clear ;

var Loop : integer ;

begin
    for Loop := 0 to 255 do
    begin
        Port_Grid.Cells[ 1, Loop + 1 ] := 'Unassigned' ;
        Port_Grid.Cells[ 2, Loop + 1 ] := 'Unassigned' ;
        Connections[ LOop ] := '' ;
    end ;
end ;


procedure TPort_Form.Define( Port : integer ; Read : boolean ; Text : string ) ;

var Index : integer ;

begin
    if( Read ) then
    begin
        Index := 1 ;
    end else
    begin
        Index := 2 ;
    end ;
    Port_Grid.Cells[ Index, Port + 1 ] := Text ;
end ;


function TPort_Form.Defined( _Read : boolean ; Port : integer ) : string ;

var C : integer ;

begin
    if( _Read ) then
    begin
        C := 1 ;
    end else
    begin
        C := 2 ;
    end ;
    Result := Port_Grid.Cells[ C, Port + 1 ] ;
end ;


procedure TPort_Form.Port_GridClick( Sender : TObject ) ;

var C, R : integer ;
    P : TPoint ;
    S : string ;

begin
    P := Mouse.CursorPos ;
    P := ScreenToClient( P ) ;
    Port_Grid.MouseToCell( P.X, P.Y, C, R ) ;
    S := Port_Grid.Cells[ C, R ] ;
    Port_Grid.Selection := TGridRect( Rect( C, R, C, R ) ) ;
    if( copy( S, 1, 1 ) = '*' ) then
    begin
        Delete_Button.Enabled := False ;
        Connect_Button.Enabled := False ;
        Disconnect_Button.Enabled := False ;
    end else
    if( S = 'Unassigned' ) then
    begin
        Delete_Button.Enabled := False ;
        Connect_Button.Enabled := False ;
        Disconnect_Button.Enabled := False ;
    end else
    begin
        Delete_Button.Enabled := True ;
        Connect_Button.Enabled := ( pos( ' -> ', S ) = 0 ) ;
        Disconnect_Button.Enabled := not Connect_Button.Enabled ;
        Selected_Row := R ;
        Selected_Column := C ;
    end ;
end ;


procedure TPort_Form.Add_ButtonClick( Sender : TObject ) ;

var Dlg : TAdd_Port_Form ;
    Loop : integer ;

begin
    Dlg := TAdd_Port_Form.Create( Application ) ;
    try
        if( Dlg.ShowModal = mrOK ) then
        begin
            Loop := Dlg.Start.Value ;
            while( Loop < Dlg.Start.Value + ( Dlg.Count.Value * 2 ) ) do
            begin
                if( Loop > 253 ) then
                begin
                    break ;
                end ;
                Port_Grid.Cells[ 1, Loop + 1 ] :=
                    'MITS 88-SIO port ' + inttostr( Loop div 2 ) + ' Status' ;
                Port_Grid.Cells[ 2, Loop + 1 ] :=
                    'MITS 88-SIO port ' + inttostr( Loop div 2 ) + ' Status' ;
                Port_Grid.Cells[ 1, Loop + 2 ] :=
                    'MITS 88-SIO port ' + inttostr( Loop div 2 ) + ' Input channel' ;
                Port_Grid.Cells[ 2, Loop + 2 ] :=
                    'MITS 88-SIO port ' + inttostr( Loop div 2 ) + ' Output channel' ;
                Loop := Loop + 2 ;
            end ;
        end ;
    finally
        Dlg.Free ;
    end ;
end ;


procedure TPort_Form.Delete_ButtonClick( Sender : TObject ) ;

var S : string ;

begin
    S := Port_Grid.Cells[ 1, Selected_Row ] ;
    if( copy( S, 1, 17 ) = 'MITS 88-SIO port ' ) then
    begin
        if( pos( 'Status', S ) = 0 ) then
        begin
            dec( Selected_Row ) ;
        end ;
        Port_Grid.Cells[ 1, Selected_Row ] := 'Unassigned' ;
        Port_Grid.Cells[ 2, Selected_Row ] := 'Unassigned' ;
        Port_Grid.Cells[ 1, Selected_Row + 1 ] := 'Unassigned' ;
        Port_Grid.Cells[ 2, Selected_Row + 1 ] := 'Unassigned' ;
    end else
    begin
        Port_Grid.Cells[ 1, Selected_Row ] := 'Unassigned' ;
        Port_Grid.Cells[ 2, Selected_Row ] := 'Unassigned' ;
    end ;
    Delete_Button.Enabled := False ;
    Connect_Button.Enabled := False ;
end ;


procedure TPort_Form.Connect_ButtonClick( Sender : TObject ) ;

var C : TComponent ;
    Dummy : integer ;
    H : THandle ;
    N : string ;
    P : function( Serial_Number : integer ; UI : TUI_Interface ) : TComponent ; stdcall ;
    S : string ;

begin
    while( true ) do
    begin
        if( Open_Dialog.Execute ) then
        begin
            H := LoadLibrary( PChar( Open_Dialog.Filename ) ) ;
            if( H <> 0 ) then
            begin
                P := GetProcAddress( H, 'Make_Instance' ) ;
                if( assigned( P ) ) then
                begin
                    C := P( 0, _Altair._User_Interface._UI ) ;
                    if( C.Component_Type <> Component_Type_Cable ) then
                    begin
                        ShowMessage( 'Cannot load component: not a cable component' ) ;
                        try
                            C.Terminate ;
                            FreeLibrary( H ) ;
                        except
                        end ;
                        continue ;
                    end ;
                    N := string( C.Name ) ;
                    S := Port_Grid.Cells[ 1, Selected_Row ] ;
                    if( copy( S, 1, 17 ) = 'MITS 88-SIO port ' ) then
                    begin
                        if( pos( 'Status', S ) <> 0 ) then
                        begin
                            inc( Selected_Row ) ;
                        end ;
                        S := Port_Grid.Cells[ 1, Selected_Row ] ;
                        Dummy := pos( ' -> ', S + ' -> ' ) ;
                        S := copy( S, 1, Dummy - 1 ) + ' -> ' + N ;
                        Port_Grid.Cells[ 1, Selected_Row ] := S ;
                        S := Port_Grid.Cells[ 2, Selected_Row ] ;
                        Dummy := pos( ' -> ', S + ' -> ' ) ;
                        S := copy( S, 1, Dummy - 1 ) + ' -> ' + N ;
                        Port_Grid.Cells[ 2, Selected_Row ] := S ;
                        Connections[ Selected_Row - 1 ] := Open_Dialog.Filename ;
                    end ;
                    try
                        C.Terminate ;
                    except
                    end ;
                end else
                begin
                    H := 0 ;
                end ;
            end else
            begin
                H := 0 ;
            end ;
            if( H = 0 ) then
            begin
                ShowMessage( 'Cannot load component' ) ;
                continue ;
            end ;
        end ;
        exit ;
    end ;
end ; // TPort_Form.Connect_ButtonClick


procedure TPort_Form.Disconnect_ButtonClick( Sender : TObject ) ;

var Dummy : integer ;
    S : string ;

begin
    S := Port_Grid.Cells[ 1, Selected_Row ] ;
    if( copy( S, 1, 17 ) = 'MITS 88-SIO port ' ) then
    begin
        if( pos( 'Status', S ) <> 0 ) then
        begin
            inc( Selected_Row ) ;
        end ;
        S := Port_Grid.Cells[ 1, Selected_Row ] ;
        Dummy := pos( ' -> ', S ) ;

        S := copy( S, 1, Dummy - 1 ) ;
        Port_Grid.Cells[ 1, Selected_Row ] := S ;
        S := Port_Grid.Cells[ 2, Selected_Row ] ;
        Dummy := pos( ' -> ', S ) ;
        S := copy( S, 1, Dummy - 1 ) ;
        Port_Grid.Cells[ 2, Selected_Row ] := S ;
        Connections[ Selected_Row - 1 ] := '' ;
        Disconnect_Button.Enabled := False ;
        Connect_Button.Enabled := True ;
    end ;
end ;



end.
