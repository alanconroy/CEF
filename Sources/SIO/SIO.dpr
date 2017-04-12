{
        Program Name : SIO
        Package Name : CEF
        Purpose      : MITS 88-SIO CEF memory component
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

          This is a MITS 88-SIO serial port component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library SIO ;

uses // Borland...
     sysutils, // inttostr

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     S_IO in 'S_IO.pas' ; // TS_IO

{$R *.RES}

function Facility_Code : integer ; stdcall ;

begin
    Result := CEFSIO_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TS_IO ;
    stdcall ;

begin
    Result := TS_IO.Create ;
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
    Result := Create_Component_Query( Component_Type_Memory, S, 'MITs 88-SIO for S100' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

end.

