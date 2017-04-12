{
        Program Name : PDP11
        Package Name : CEF
        Purpose      : PDP-11 CPU emulator for CEF32
        Institution  : Conroy & Conroy Co.
        Date Written : 13-July-2007
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2007 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *    DATE        BY          REASON                         *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This is a CPU component for CEF32 that emulates the PDP-11.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

Program PDP11_Test ;

{$R *.RES}

uses // CEF32...
    CEF,
    PDP11_CPU in 'PDP11_CPU.pas',
    New_Watch in '..\shared\New_Watch.pas' {New_Watch_Dialog},
    Conditions_Dlg in '..\Shared\Conditions_Dlg.pas' {Conditions_Form},
    Trace_Log_Dlg in '..\Shared\Trace_Log_Dlg.pas' {Trace_Log_Form},
    Trace_Dlg in '..\Shared\Trace_Dlg.pas' {Trace_Form};

function Facility_Code : longint ; stdcall ;

begin
    Result := 42 ;
end ;


function Make_Instance( Serial_Number : longint ; UI : TUI_Interface ) : TPDP11 ;
    stdcall ;

begin
    Result := TPDP11.Create ;
    Result.Initialize( UI ) ;
    Result._Serial_Number := Serial_Number ;
end ;


function Version : longint ;

begin
    Version := 20 ; // Version 2.0
end ;


var UI : TUI_Interface ;

begin
    UI := TUI_Interface.Create ;
    Make_Instance( 0, UI ) ;
end.
