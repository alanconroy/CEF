{
        Program Name : UNIBUS
        Package Name : CEF
        Purpose      : DEC PDP-11 UNIBUS system emulator
        Institution  : Conroy & Conroy Co.
        Date Written : 13-Jan-2007
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

          Provides a DEC PDP-11 UNIBUS system.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

program UNIBUS_Test ;

uses // Borland...
     Forms,

     // C&C...
     _App,
     App,

     // CEF...
     CEF,

     // UNIBUS...
     DEC_UNIBUS in 'DEC_UNIBUS.pas',
     UNIBUS_Form in 'UNIBUS_Form.pas' {Front_Panel_Form},
     Port_Dialog in 'Port_Dialog.pas' {Port_Form},
     Port_Change_Dialog in 'Port_Change_Dialog.pas' {Port_Change_Form};

{$R *.res}

function Facility_Code : integer ; stdcall ;

begin
    Result := DECUNIBUS_Facility ;
end ;


function Make_Instance( Serial_Number : integer ;
    UI : TUI_Interface ) : TDEC_UNIBUS ; stdcall ;

begin
    Result := TDEC_UNIBUS.Create ;
    Result.Initialize( UI ) ;
    Result._Serial_Number := Serial_Number ;
end ;


function Version : longint ;

begin
    Version := 20 ; // Version 2.0
end ;


var F : TForm ;
    UI : TUI_Interface ;

begin
    Application.Initialize ;
    Application.CreateForm(TForm, F);
  F.Visible := False ;
    UI := TUI_Interface.Create ;
    Make_Instance( 0, UI ) ;
    Application.Run ;
end.
