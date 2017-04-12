{
        Program Name : SVMScreen
        Package Name : CEF
        Purpose      : SCreen interface for CEF SVM
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Dec-2014
        Written By   : Alan Conroy
        Version      : 1.0A

	Released to the public domain.

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

          This is the screen/keyboard interface for SVM.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library SVMScreen ;

uses
  sysutils,
  Forms,
  Applic,
  _SVM in '..\..\..\foundation\SVM32\Sources\_SVM.pas',
  CEF,
  SVM_Screen in 'SVM_Screen.pas',
  SVM_Screen_main in 'SVM_Screen_main.pas' {Main_Form};

{$R *.res}

const SVMScreen_Facility = -1 ;

function Facility_Code : integer ; stdcall ;

begin
    Result := SVMScreen_Facility ;
end ;


function Make_Instance( Serial_Number : integer ; UI : TUI_Interface ) : TSVMScreen ;
    stdcall ;

begin
    Result := TSVMScreen.Create ;
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
    Result := Create_Component_Query( Component_Type_Cable, S, 'SVM Screen' ) ;
end ;


{$WARNINGS OFF}
exports Facility_Code index 0,
        Make_Instance index 1,
        Version index 2,
        Query_Info index 3 ;
{$WARNINGS ON}

var Form : TForm ;

begin
    Application.Initialize ;
    Application.CreateForm(TForm, Form) ;
    Form.Visible := False ;
    { VCL kills the DLL when the first created form is closed, so create this
      form so if the screen is destroyed (by terminating the TSVMScreen
      object), the DLL remains. }
end.
