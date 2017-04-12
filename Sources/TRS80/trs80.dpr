{
        Program Name : TRS80
        Package Name : CEF
        Purpose      : TRS80 hardware emulator
        Institution  : Conroy & Conroy Co.
        Date Written : 8-July-2005
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2005 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          Provides a TRS-80 (model I) hardware emulation.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library TRS80 ;

uses // Borland...
     sysutils, // inttostr
     Forms,

     // C&C...
     _Applic,
     Applic,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     TRS80_Screen,
     trs80_main in 'trs80_main.pas' {GenScreen_Form};

{$R *.res}

function Facility_Code : integer ; stdcall ;

begin
    Result := TRS80Screen_Facility ;
end ;


function Make_Instance( Serial_Number : integer ;
    UI : TUI_Interface ) : TZ80_Screen ; stdcall ;

begin
    Result := TZ80_Screen.Create ;
    Result.Initialize( UI ) ;
    Result._Serial_Number := Serial_Number ;
end ;


function Version : longint ; stdcall ;

begin
    Version := Interface_Version ;
end ;


function Query_Info : TCEF_Component_Query ; stdcall ;

var S : string ;

begin
    S := inttostr( Version ) ;
    S := copy( S, 1, length( S ) - 1 ) + '.' + copy( S, length( S ), 1 ) ;
    Result := Create_Component_Query( Component_Type_UI, S, 'TRS-80 system unit' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}


var F : TForm ;

begin
    Application.Initialize;
    Application.CreateForm( TForm, F ) ;
    F.Visible := False ;
    { VCL kills the DLL when the first created form is closed, so create this
      form so if the screen is destroyed (by terminating the TVT05
      object), the DLL remains. }
end.

