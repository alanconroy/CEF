{
        Program Name : Apple2_KB
        Package Name : CEF
        Purpose      : Apple II keyboard component
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Apr-2006
        Written By   : Alan Conroy
        Version      : 1.0A

	Released to the public domain.

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

        This is a Apple II keyboard component for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library Apple_2_KB ;

uses // Borland...
     sysutils, // inttostr
     Forms,

     // C&C...
     Applic,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     Apple2KB_main in 'Apple2KB_main.pas' {Main_Form},
     Apple2_KB in 'Apple2_KB.pas';

{$R *.res}

const Apple2_KB_Facility = -1 ;

function Facility_Code : integer ; stdcall ;

begin
    Result := Apple2_KB_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TApple2_KB ;
    stdcall ;

begin
    Result := TApple2_KB.Create ;
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
    Result := Create_Component_Query( Component_Type_Keyboard, S, 'Appple II keyboard' ) ;
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
      form so if the screen is destroyed (by terminating the TVT05
      object), the DLL remains. }
end.
