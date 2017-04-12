{
        Program Name : VDM1
        Package Name : CEF
        Purpose      : S100 VDM-1 screen
        Institution  : Conroy & Conroy Co.
        Date Written : 8-Apr-2005
        Written By   : Alan Conroy
        Version      : 1.0A

        Copyright (C) 2006-2007 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

         20-Jan-2007    EAC         Handle shutdown better.

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          Emulates an S100 VDM-1 video card and monitor.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library VDM1 ;

uses // Borland...
     sysutils, // inttostr
     Forms,

     // C&C...
     _Applic,
     Applic,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query

     // VDM1...
     VDM1_Screen in 'VDM1_Screen.pas',
     vdm1_main in 'vdm1_main.pas' {VDM_Form};

{$R *.res}

function Facility_Code : integer ; stdcall ;

begin
    Result := VDM1Screen_Facility ;
end ;


function Make_Instance( Serial_Number : integer ;
    UI : TUI_Interface ) : TVDM1_Screen ; stdcall ;

begin
    Result := TVDM1_Screen.Create ;
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
    Result := Create_Component_Query( Component_Type_UI, S, 'VDM-1 display screen' ) ;
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
    Application.CreateForm(TForm, F);
    F.Visible := False ;
    { VCL kills the DLL when the first created form is closed, so create this
      form so if the screen is destroyed (by terminating the CEF TComponent
      object), the DLL remains. }
end.

