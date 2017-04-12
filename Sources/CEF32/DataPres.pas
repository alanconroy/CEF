{
        Program Name : DataPres
        Package Name : CEF32
        Purpose      : Data Presentation layer
        Institution  : Conroy & Conroy Co.
        Date Written : 31-Dec-2009
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

          This unit provides the generic data presenter and the interface to all
        custom data presenter plugins.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit DataPres ;

interface

uses // Borland...
     Classes, // TStringList
     
     // C&C...
     _UE, // TUnified_Exception

     // CEF
     _CEF, // PCEF_Media_File_Header
     CEFMedia ; // TPresenter

function Get_Presentation( S, N : string ; Parent : THandle ;
    _Int : TMedia ; Header : PCEF_Media_File_Header ) : TPresenter ;
function Get_Formats : TStringList ;

implementation

uses // Borland...
     Windows, // LoadLibrary
     Forms, // Application

     // C&C...
     _FileSys, // TFind_Record
     _Fip, // Lookup
     O_S, // OS

     // CEF...
     DataDis ; // TData_Display

var Handles : array of THandle ;
var Servers : array of TPresentation_Manager ;

type TGeneric_Presenter = class( TPresenter )
                              public // Constructors and destructors...
                                  constructor Create( Parent : THandle ;
                                      _Int : TMedia ;
                                      Header : PCEF_Media_File_Header ) ;

                              private // Instance data...
                                  Panel : TData_Display ;

                              public // API...
                                  function Terminate : TUnified_Exception ; override ;
                                  procedure Redraw ; override ;
                                  procedure Set_Bounds( Width, Height : integer ) ;
                                      override ;
                                  procedure Get_Header( var Header : PCEF_Media_File_Header ) ;
                                      override ;
                              end ;

// Constructors and destructors...

constructor TGeneric_Presenter.Create( Parent : THandle ;
    _Int : TMedia ; Header : PCEF_Media_File_Header ) ;

begin
    inherited Create ;

    Panel := TData_Display.Create( Application ) ;
    Panel.ParentWindow := Parent ;
    Panel._Array := _Int ;
end ;


// API...

function TGeneric_Presenter.Terminate : TUnified_Exception ;

begin
    Result := nil ;
    Panel.Free ;
    Panel := nil ;
    Free ;
end ;


procedure TGeneric_Presenter.Redraw ;

begin
    Panel.Display_Panel.Repaint ;
end ;


procedure TGeneric_Presenter.Set_Bounds( Width, Height : integer ) ;

begin
    Panel.SetBounds( 0, 0, Width, Height ) ;
    Panel.Display_Panel.Repaint ;
end ;


procedure TGeneric_Presenter.Get_Header( var Header : PCEF_Media_File_Header ) ;

begin
    fillchar( Header, sizeof( Header ), 0 ) ;
    Header.Prefix := 65535 ;
    Header.ID := 255 ;
    Header.Facility := -1 ; // Undefined
    Header.Version := 10 ;
end ;


// Unit API...

function Get_Presentation( S, N : string ; Parent : THandle ;
    _Int : TMedia ; Header : PCEF_Media_File_Header ) : TPresenter ;

var Loop : integer ;

begin
    for Loop := 0 to length( Servers ) - 1 do
    begin
        Result := Servers[ Loop ].Get_Presenter( PChar( S ), Parent, _Int, Header ) ;
        if( Result <> nil ) then
        begin
            exit ;
        end ;
    end ;

    // No presenters found for requested format, so use generic presenter
    Result := TGeneric_Presenter.Create( Parent, _Int, Header ) ;
end ;


function Get_Formats : TStringList ;

var Loop, Index : integer ;
    P : PChar ;

begin
    Result := TStringList.Create ;
    Result.Add( 'Binary (generic)' ) ;

    for Loop := 0 to length( Servers ) - 1 do
    begin
        Index := 0 ;
        P := Servers[ Loop ].Get_Name( Index ) ;
        while( P <> nil ) do
        begin
            inc( Index ) ;
            Result.Add( string( P ) ) ;
            P := Servers[ Loop ].Get_Name( Index ) ;
        end ;
    end ;
end ;


type TMM_API = function() : TPresentation_Manager ;

procedure Load_Presentation_Servers ; // Load all presentation servers...

var A : TMM_API ;
    Find_Record : TFind_Record ;
    H : THandle ;
    S : string ;

begin
    Initialize_Find_Record( Find_Record ) ;
    try
        S := Lookup( OS^.Application_Path + 'MM\*.dll', Find_Record ) ;
        while( length( S ) > 0 ) do
        begin
            H := LoadLibrary( PChar( S ) ) ;
            if( H <> 0 ) then
            begin
                A := GetProcAddress( H, 'Get_Presentation_Server' ) ;
                if( @A <> nil ) then
                begin
                    setlength( Handles, length( Handles ) + 1 ) ;
                    Handles[ length( Handles ) - 1 ] := H ;
                    setlength( Servers, length( Servers ) + 1 ) ;
                    Servers[ length( Servers ) - 1 ] := A ;
                end else
                begin
                    FreeLibrary( H ) ;
                end ;
            end ;
            S := Lookup( '', Find_Record ) ;
        end ;
    except
        Uninitialize_Find_Record( Find_Record ) ;
    end ;
end ; // Load_Presentation_Servers


initialization
    Load_Presentation_Servers ;
end.
