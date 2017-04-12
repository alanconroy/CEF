{
        Program Name : PC11
        Package Name : CEF
        Purpose      : DEC PC11 CEF component
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Jan-2007
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

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This component emulates a DEC PC11 paper tape punch for
        CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library PC11 ;

uses // Borland...
     sysutils, // inttostr
     Forms,

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     PC_11 in 'PC_11.pas' ; // TPC11

{$R *.RES}

function Facility_Code : integer ; stdcall ;

begin
    Result := CEFPC11_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TPC11 ;
    stdcall ;

begin
    Result := TPC11.Create ;
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
    Result := Create_Component_Query( Component_Type_Memory, S, 'DEC PC11 UNIBUS paper tape punch' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

end.

