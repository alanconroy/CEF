{
        Program Name : Z80 
        Package Name : CEF
        Purpose      : Intel 8080/8085/Zilog Z80 CPU component for CEF32
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2006-2007 by Alan Conroy.  Released to the public domain.

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

          This is Intel 8080/8085/Zilog Z80 CPU component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library Z80 ;

{$R *.RES}

{%File '..\CEF32\cef32.bdsproj'}

uses // Borland...
     sysutils, // inttostr

     // CEF...
     _CEF, // TUI_Interface
     CEF, // Create_Component_Query
     Z80ASM in 'Z80ASM.pas',
     Z80CPU in 'Z80CPU.pas',
     New_Watch in '..\shared\New_Watch.pas' {New_Watch_Dialog},
     Conditions_Dlg in '..\Shared\Conditions_Dlg.pas' {Conditions_Form},
     Trace_Log_Dlg in '..\Shared\Trace_Log_Dlg.pas' {Trace_Log_Form},
     Trace_Dlg in '..\Shared\Trace_Dlg.pas' {Trace_Form};

// TZ80

function Facility_Code : longint ; stdcall ;

begin
    Result := 42 ;
end ;


function Make_Instance( Serial_Number : longint ; UI : TUI_Interface ) : TZ80 ;
    stdcall ;

begin
    Result := TZ80.Create ;
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
    Result := Create_Component_Query( Component_Type_CPU, S, 'Zilog Z80, Intel 8080/8085 8-bit CPU' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}


end.
