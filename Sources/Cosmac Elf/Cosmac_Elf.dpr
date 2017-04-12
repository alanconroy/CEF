{
        Program Name : Cosmac_Elf
        Package Name : CEF
        Purpose      : Cosmac Elf emulator
        Institution  : Conroy & Conroy Co.
        Date Written : 10-Mar-2007
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2007 by Alan Conroy.  Released to the public domain.

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

          Provides a Cosmac Elf microcomputer.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library Cosmac_Elf ;

uses // Borland...
     sysutils, // inttostr
     Forms,

     // C&C...
     _Applic,
     Applic,

     // CEF32...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     AboutBox in 'AboutBox.pas' {About_Form},
     Elf in 'Elf.pas',
     Elf_Form in 'Elf_Form.pas' {Front_Panel_Form};

{$R *.res}

function Facility_Code : integer ; stdcall ;

begin
    Result := CosmacElf_Facility ;
end ;


function Make_Instance( Serial_Number : integer ;
    UI : TUI_Interface ) : TCosmac_Elf ; stdcall ;

begin
    Result := TCosmac_Elf.Create ;
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
    Result := Create_Component_Query( Component_Type_UI, S, 'Cosmac Elf System unit' ) ;
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
    About_Form.Visible := False ;
    { Delphi kills the DLL when the first created form is closed, so create this
      form so if the Front Panel is destroyed (by terminating the TCosmac_Elf
      object), the DLL remains. }
end.

