{
        Program Name : Gigabyte_uCode
        Package Name : CEF
        Purpose      : Gigabyte Micro-code engine
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2011 by Alan Conroy.  Released to the public domain.

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

          This is Gigabyte microcode CPU component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library GB_uCode ;

{$R *.RES}

uses // Borland...
     sysutils, // inttostr
     Forms,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     Gigabyte_uCode_ASM in 'Gigabyte_uCode_ASM.pas',
     Gigabyte_uCode_CPU in 'Gigabyte_uCode_CPU.pas';

function Facility_Code : longint ; stdcall ;

begin
    Result := 125 ;
end ;


function Make_Instance( Serial_Number : longint ;
    UI : TUI_Interface ) : TGigabyte_uCode ; stdcall ;

begin
    Result := TGigabyte_uCode.Create ;
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
    Result := Create_Component_Query( Component_Type_CPU, S, 'Gigabyte Microcode engine' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}


end.
