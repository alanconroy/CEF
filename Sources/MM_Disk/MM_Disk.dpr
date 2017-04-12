{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 Disk data presenter
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
        *************************************************************

            DATE        BY          REASON

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This library serves up the disk media presentor.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library MM_Disk ;

uses // Pascal...
     Sysutils,

     // CEF...
     CEFMedia,
     DiskPres ; // TGeneric_Disk_Presenter

// TGeneric_Disk_Presenter

type TDisk_Presentation_Manager = class( TPresentation_Manager )
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


function TDisk_Presentation_Manager.Get_Presenter( S : PChar ;
    Parent : THandle ; _Array : TMedia ;
    Header : PCEF_Media_File_Header ) : TPresenter ;

var N : string ;

begin
    Result := nil ; // Assume failure
    N := string( S ) ;
    if( uppercase( N ) <> 'DISK IMAGE' ) then
    begin
        exit ;
    end ;

    Result := TGeneric_Disk_Presenter.Create( Parent, _Array, Header ) ;
end ;


function TDisk_Presentation_Manager.Get_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( Index = 0 ) then
    begin
        Temp := 'Disk image' ;
        Result := PChar( Temp ) ;
    end ;
end ;


function TDisk_Presentation_Manager.Get_Path( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( Index = 0 ) then
    begin
        Temp := 'Disk\Generic' ;
        Result := PChar( Temp ) ;
    end ;
end ;


function TDisk_Presentation_Manager.Version : integer ;

begin
    Result := 10 ; // V1.0
end ;


function Get_Presentation_Server : TPresentation_Manager ;

begin
    Result := TDisk_Presentation_Manager.Create ;
end ;



function Version : longint ;

begin
    Result := 10 ; // Version 1.0
end ;


exports Get_Presentation_Server index 1,
        Version index 2 ;

begin
end.
