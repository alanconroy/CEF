{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 ANSI Tape data presenter
        Institution  :
        Date Written : 27-Apr-2000
        Written By   : Alan Conroy
        Version      : 1.3

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

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

          This library serves up the ANSI Tape media presentor.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library MM_Tape_ANSI ;

uses // Borland...
     Sysutils, // uppercase

     // CEF...
     CEFMedia, // TPresentation_Manager

     // CEF32...
     ANSIPres ; // TANSI_Tape_Presenter


type TANSI_Presentation_Manager = class( TPresentation_Manager )
                                      public // API...
                                          Temp : string ;

                                          function Get_Presenter( S : PChar ;
                                              Parent : THandle ;
                                              _Array : TMedia ;
                                              Header : PCEF_Media_File_Header ) : TPresenter ;
                                              override ;

                                          function Get_Name( Index : longint ) : PChar ;
                                              override ;

                                          function Get_Path( Index : longint ) : PChar ;
                                              override ;

                                          function Version : integer ; override ;
                                  end ;


function TANSI_Presentation_Manager.Get_Presenter( S : PChar ;
    Parent : THandle ; _Array : TMedia ;
    Header : PCEF_Media_File_Header ) : TPresenter ;

var N : string ;

begin
    Result := nil ; // Assume failure
    N := string( S ) ;
    if( uppercase( N ) <> 'ANSI TAPE' ) then
    begin
        exit ;
    end ;

    Result := TANSI_Tape_Presenter.Create( Parent, _Array, Header ) ;
end ;


function TANSI_Presentation_Manager.Get_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( Index = 0 ) then
    begin
        Temp := 'ANSI Tape' ;
        Result := PChar( Temp ) ;
    end ;
end ;


function TANSI_Presentation_Manager.Get_Path( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( Index = 0 ) then
    begin
        Temp := 'Tape\ANSI' ;
        Result := PChar( Temp ) ;
    end ;
end ;


function TANSI_Presentation_Manager.Version : integer ;

begin
    Result := 10 ; // V1.0
end ;


function Get_Presentation_Server : TPresentation_Manager ;

begin
    Result := TANSI_Presentation_Manager.Create ;
end ;



function Version : longint ;

begin
    Result := 10 ; // Version 1.0
end ;


exports Get_Presentation_Server index 1,
        Version index 2 ;

begin
end.
