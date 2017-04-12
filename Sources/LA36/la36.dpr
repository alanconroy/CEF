{
        Program Name : LA36
        Package Name : CEF
        Purpose      : Generic Hard Copy Terminal
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Dec-2006
        Written By   : Alan Conroy
        Version      : 1.0

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

          This is a bare-bones hard copy terminal emulator component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library LA36 ;

uses // Borland...
     sysutils, // inttostr
     Forms,

     // C&C...
     Applic,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     AboutBox in 'AboutBox.pas' {About_Form},
     LA_36 in 'LA_36.pas',
     LA36_main in 'LA36_main.pas' {Main_Form},
     Printer_Options_Form in 'Printer_Options_Form.pas' {Printer_Options_Dialog},
     Send in 'Send.pas' {Send_Dialog},
     Here_Is_Dialog in 'Here_Is_Dialog.pas' {Here_Is_Form},
     VT_Dialog in 'VT_Dialog.pas' {VT_Form},
     Selective_Addressing_Form in 'Selective_Addressing_Form.pas' {Selective_Addressing_Dialog};

{$R *.res}

const LA36_Facility = -1 ;

function Facility_Code : integer ; stdcall ;

begin
    Result := LA36_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TLA36 ;
    stdcall ;

begin
    Result := TLA36.Create ;
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
    Result := Create_Component_Query( Component_Type_Cable, S, 'Teletype Model 33 ASR/KSR/RO, DECWriter LA30/LA36' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

begin
    Application.Initialize ;
    Application.CreateForm(TAbout_Form, About_Form);
  Application.CreateForm(THere_Is_Form, Here_Is_Form);
  Application.CreateForm(TVT_Form, VT_Form);
  Application.CreateForm(TSelective_Addressing_Dialog, Selective_Addressing_Dialog);
  About_Form.Visible := False ;
    { VCL kills the DLL when the first created form is closed, so create this
      form so if the screen is destroyed (by terminating the TLA36
      object), the DLL remains. }
end.
