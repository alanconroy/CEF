{
        Program Name : ASR33_KB
        Package Name : CEF
        Purpose      : ASR33 keyboard
        Institution  : Conroy & Conroy Co.
        Date Written : 31-Dec-2006
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

          This is a CEF keyboard component with the ASR33 layout.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library ASR33_KB ;

uses // Borland...
     sysutils, // inttostr
     Forms,

     // C&C...
     Applic,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     ASR_33_KB in 'ASR_33_KB.pas',
     ASR33KB_main in 'ASR33KB_main.pas' {Main_Form};

{$R *.res}

const ASR33KB_Facility = -1 ;

function Facility_Code : integer ; stdcall ;

begin
    Result := ASR33KB_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TASR33KB ;
    stdcall ;

begin
    Result := TASR33KB.Create ;
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
    Result := Create_Component_Query( Component_Type_Keyboard, S, 'Teletype Model 33 ASR keyboard' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

var F : TForm ;

begin
    Application.Initialize ;
    Application.CreateForm(TForm, F);
    F.Visible := False ;
    { Delphi kills the DLL when the first created form is closed, so create this
      form so if the screen is destroyed (by terminating the TASR33
      object), the DLL remains. }
end.
