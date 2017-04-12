{
        Program Name : CEFSVM
        Package Name : CEF
        Purpose      : SVM CPU emulator for CEF32
        Institution  : Conroy & Conroy Co.
        Date Written : 6-Feb-2015
        Written By   : Alan Conroy
        Version      : 1.0

	    Copyright (C) 2015 by Alan Conroy.  Released to the public domain.

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

          This is an SVM CPU component for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library CEFSVM ;

{$R *.RES}

uses
  sysutils,
  CEF,
  SVM_CPU in 'SVM_CPU.pas',
  New_Watch in '..\shared\New_Watch.pas' {New_Watch_Dialog},
  Conditions_Dlg in '..\Shared\Conditions_Dlg.pas' {Conditions_Form},
  Trace_Log_Dlg in '..\Shared\Trace_Log_Dlg.pas' {Trace_Log_Form},
  Trace_Dlg in '..\Shared\Trace_Dlg.pas' {Trace_Form},
  SVM_ASM in 'SVM_ASM.pas';

function Facility_Code : longint ; stdcall ;

begin
    Result := -1 ;
end ;


function Make_Instance( Serial_Number : longint ; UI : TUI_Interface ) : TSVM ;
    stdcall ;

begin
    Result := TSVM.Create ;
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
    Result := Create_Component_Query( Component_Type_CPU, S, 'Sirius Virtual Machine' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

end.

