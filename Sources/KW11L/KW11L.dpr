{
        Program Name : KW11L
        Package Name : CEF
        Purpose      : DEC KW11-L line clock CEF component
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Mar-2008
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

          This component emulates the line clock portion of a DEC KW11-L serial
        line interface card, for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library KW11L ;

uses // Borland...
     sysutils, // inttostr

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     KW11_L in 'KW11_L.pas' ; // TKW11L

{$R *.RES}

function Facility_Code : integer ; stdcall ;

begin
    Result := CEFKW11_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TKW11L ;
    stdcall ;

begin
    Result := TKW11L.Create ;
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
    Result := Create_Component_Query( Component_Type_Memory, S, 'DEC KW11-L UNIBUS line clock' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

end.

