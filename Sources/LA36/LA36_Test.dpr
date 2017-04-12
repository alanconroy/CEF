{
        Program Name : VT52
        Package Name : CEF
        Purpose      : Generic Video Terminal
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Dec-2006
        Written By   : Alan Conroy
        Version      : 1.0A

	Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *    DATE        BY          REASON                         *
        *                                                           *
        * 20-Jan-2007    EAC         Handle shutdown better.        *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This is a bare-bones video terminal emulator component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

Program VT52_Test ;

uses
  Forms,
  App,
  CEF,
  VT_52 in 'VT_52.pas',
  VT52_Main in 'VT52_main.pas' {Main_Form},
  Video_Options_Form in 'Video_Options_Form.pas' {Video_Options_Dialog},
  Send in 'Send.pas' {Send_Dialog};

{$R *.res}

const VT52_Facility = -1 ;

function Facility_Code : integer ; stdcall ;

begin
    Result := VT52_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TVT52 ;
    stdcall ;

begin
    Result := TVT52.Create ;
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
