{
        Program Name : PR11
        Package Name : CEF
        Purpose      : DEC PR11 CEF component
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

          This component emulates a DEC PR11 paper tape reader for
        CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library PR11 ;

uses // Borland...
     sysutils, // inttostr
     
     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     PR_11 in 'PR_11.pas' ; // TPR11

{$R *.RES}

function Facility_Code : integer ; stdcall ;

begin
    Result := CEFPR11_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TPR11 ;
    stdcall ;

begin
    Result := TPR11.Create ;
    Result.Initialize( UI ) ;
    Result._Serial_Number := Serial_Number ;
end ;


function Version : longint ;

begin
    Version := Interface_Version ;
end ;


function Query_Info : TCEF_Component_Query ;

var S : string ;

begin
    S := inttostr( Version ) ;
    S := copy( S, 1, length( S ) - 1 ) + '.' + copy( S, length( S ), 1 ) ;
    Result := Create_Component_Query( Component_Type_Memory, S, 'DEC PR11 UNIBUS paper tape reader' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

end.

