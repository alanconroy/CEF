{
        Program Name : S4M_RAM
        Package Name : CEF
        Purpose      : Generic CEF memory component
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Jun-2008
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

          This is a Static 4 Mb RAM component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library S4M_RAM ;

uses // Borland...
     sysutils, // inttostr

     // CEF32...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     Memory_4M in 'Memory_4M.pas' ;

{$R *.RES}

function Facility_Code : integer ; stdcall ;

begin
    Result := CEFMemory_Facility ;
end ;


function Make_Instance( Serial_Number : integer ;
    UI : TUI_Interface ) : TCEF_4M_Memory ; stdcall ;

begin
    Result := TCEF_4M_Memory.Create ;
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
    Result := Create_Component_Query( Component_Type_Memory, S, '4Mb Static RAM' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

end.

