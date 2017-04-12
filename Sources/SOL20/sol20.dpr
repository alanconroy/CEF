{
        Program Name : SOL20
        Package Name : CEF
        Purpose      : Processor Technology SOL-20 component
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

        This is the SOL-20 Emulator for CEF.  It provides support for the
        integrating the various SOL-20 components.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library SOL20 ;

uses // Borland...
     sysutils, // inttostr
     Forms,

     // C&C...
     Applic,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     AboutBox in 'Aboutbox.pas' {About_Form},
     SOL20_Main in 'SOL20_main.pas' {Main_Form},
     SOL_20 in 'SOL_20.pas',
     Choose_Emulator_Port_Dlg in '..\Shared\Choose_Emulator_Port_Dlg.pas' {Choose_Emulator_Port_Form};

{$R *.res}

const SOL20_Facility = -1 ;

function Facility_Code : integer ; stdcall ;

begin
    Result := SOL20_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TSOL20 ;
    stdcall ;

begin
    Result := TSOL20.Create ;
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
    Result := Create_Component_Query( Component_Type_Memory, S, 'Processor Technology SOL-20 System unit' ) ;
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
