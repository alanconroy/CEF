{
        Program Name : SRAM
        Package Name : CEF
        Purpose      : CEF static RAM memory component
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Jan-2015
        Written By   : Alan Conroy
        Version      : 1.0

	Released to the public domain.

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

          This is a Static 64 Kb RAM component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library SRAM ;

uses
  sysutils,
  CEF,
  SRAM_Memory in 'SRAM_Memory.pas';

{$R *.RES}

function Facility_Code : integer ; stdcall ;

begin
    Result := CEFMemory_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TCEF_SRAM ;
    stdcall ;

begin
    Result := TCEF_SRAM.Create ;
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
    Result := Create_Component_Query( Component_Type_Memory, S, 'Static RAM' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

end.

