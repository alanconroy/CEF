{
        Program Name : DL11W
        Package Name : CEF
        Purpose      : DEC DL11W line clock CEF component
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
        *    DATE        BY          REASON                         *
        *                                                           *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This component emulates the line clock portion of a DEC DL11-W serial
        line interface card, for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library DL11W ;

uses CEF,
     DL_11W in 'DL_11W.pas' ; // TDL11W

{$R *.RES}

function Facility_Code : integer ; stdcall ;

begin
    Result := CEFDL11_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TDL11W ;
    stdcall ;

begin
    Result := TDL11W.Create ;
    Result.Initialize( UI ) ;
    Result._Serial_Number := Serial_Number ;
end ;


function Version : longint ;

begin
    Version := 20 ; // Version 2.0
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2 ;
{$WARNINGS ON}

end.

