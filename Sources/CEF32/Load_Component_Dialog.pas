{
              Program Name : Load_Component_Dialog
              Package Name : CEF32
              Purpose      : Load a component
              Institution  : Conroy & Conroy
              Date Written : 7-Dec-2014
              Written By   : Alan Conroy
              Version      : 2.0

              Copyright (C) 2014 by Alan Conroy.  Released to the public domain.

              TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *********************************************************
        *                                                       *
        *        M O D I F I C A T I O N   H I S T O R Y        *
        *                                                       *
        *********************************************************

             DATE      BY             REASON                   

        *********************************************************
        *                                                       *
        *           P R O G R A M   P U R P O S E               *
        *                                                       *
        *********************************************************

          This unit defines the assembler for the Gigabyte micro-code component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Load_Component_Dialog ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,

     // CEF...
     _CEF ; // TUI_Interface


type
  TLoad_Component_Form = class(TForm)
    Button_Panel: TPanel;
    Load_Button: TBitBtn;
    BitBtn1: TBitBtn;
    ListView1: TListView;
    Label1: TLabel;
    Filename: TEdit;
    SpeedButton1: TSpeedButton;
    Open_Component_Dialog: TOpenDialog;
    Label2: TLabel;
    Component_Name: TEdit;
    Domain: TEdit;
    Label3: TLabel;
    Setup: TEdit;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FilenameChange(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
  private
      function Get_Description( S : string ) : TStringList ;
      procedure wmuser( var Message : TMessage ) ; message wm_user ;
  public
      UI : TUI_Interface ;
  end ;

var Load_Component_Form : TLoad_Component_Form ;

implementation

uses // C&C...
     O_S, // OS
     Parse, // Program_Path

     // CEF...
     TextStrs ; // Text_*

{$R *.dfm}

procedure TLoad_Component_Form.FilenameChange( Sender : TObject ) ;

var F : function( Serial_Number : integer ; UI : TUI_Interface ) : TComponent ; stdcall ;
    H : THandle ;

begin
    Load_Button.Enabled := False ;
    H := Loadlibrary( PChar( Filename.Text ) ) ;
    if( H = 0 ) then
    begin
        exit ;
    end ;
    F := GetProcAddress( H, 'Make_Instance' ) ;
    if( @F = nil ) then
    begin
        FreeLibrary( H ) ;
        exit ;
    end ;
    FreeLibrary( H ) ;
    Load_Button.Enabled := True ;
end ;


function Component_Type_Name( Component_Type : longint ) : string ;

begin
    case Component_Type of
        Component_Type_CPU : Result := 'CPU' ;
        Component_Type_Bus : Result := 'Bus' ;
        Component_Type_Memory : Result := 'Memory' ;
        Component_Type_Motherboard : Result := 'Motherboard' ;
        Component_Type_IO : Result := 'I/O' ;
        Component_Type_UI : Result := 'UI' ;
        Component_Type_Cable : Result := 'Cable' ;
        Component_Type_Keyboard : Result := 'Keyboard' ;
        else Result := 'Unknown' ;
    end ;
end ;


function Get_Value( List : TStringList ; Key : string ) : string ;

var I : integer ;

begin
    I := List.Indexof( Key ) ;
    if( I = -1 ) then
    begin
        Result := '' ;
    end else
    begin
        Result := List[ I + 1 ] ;
    end ;
end ;


function TLoad_Component_Form.Get_Description( S : string ) : TStringList ;

var C : TComponent ;
    F : function( Serial_Number : integer ; UI : TUI_Interface ) : TComponent ; stdcall ;
    FQ : function() : TCEF_Component_Query ; stdcall ;
    H : THandle ;

begin
    Result := nil ;
    S := OS^.Application_Path + 'Components\' + S ;
    H := Loadlibrary( PChar( S ) ) ;
    if( H = 0 ) then
    begin
        exit ;
    end ;
    try
        F := GetProcAddress( H, 'Make_Instance' ) ;
        if( @F = nil ) then
        begin
            FreeLibrary( H ) ;
            exit ;
        end ;
        Result := TStringList.Create ;
        FQ := GetProcAddress( H, 'Query_Info' ) ;
        if( @FQ <> nil ) then
        begin
            if( FQ <> nil ) then
            begin
                Result.Add( 'Type' ) ;
                Result.Add( Component_Type_Name( FQ.Component_Type ) ) ;
                Result.Add( 'Version' ) ;
                Result.Add( ' ' + FQ.Version ) ;
                Result.Add( 'Emulates' ) ;
                Result.Add( ' ' + FQ.Emulates ) ;
                FQ.Terminate ;
                exit ;
            end ;
        end ;
        C := F( 0, UI ) ;
        Result.Add( 'Type' ) ;
        Result.Add( Component_Type_Name( C.Component_Type ) ) ;
        S := inttostr( C.Version ) ;
        S := copy( S, 1, length( S ) - 1 ) + '.' + copy( S, length( S ), 1 ) ;
        Result.Add( 'Version' ) ;
        Result.Add( ' ' + S ) ;
        UI.Termination_Notice( C ) ; // In case the component doesn't do this
        try
            C.Terminate ;
        except
        end ;
    finally
        try
            FreeLibrary( H ) ;
        except
        end ;
    end ;
end ;


procedure TLoad_Component_Form.FormShow( Sender : TObject ) ;

begin
    ListView1.Clear ;
    Cursor := crHourglass ;
    Application.ProcessMessages ;
    PostMessage( Handle, wm_User, 0, 0 ) ;
end ;


procedure TLoad_Component_Form.wmuser( var Message : TMessage ) ;

var F : TSearchRec ;
    Item : TListItem ;
    List : TStringList ;

begin
    Open_Component_Dialog.Filter := Text_File_Filter_Dynamic_Link_Libraries + '|*.dll' ;
    Open_Component_Dialog.Title := Text_Menu_Caption_Open_Component ;
    Open_Component_Dialog.InitialDir := OS^.Application_Path + 'Components\' ;
    if( Findfirst( Program_Path + 'Components\*.dll', -1, F ) = 0 ) then
    begin
        List := Get_Description( F.Name ) ;
        Item := ListView1.Items.Add ;
        Item.Caption := copy( F.Name, 1, length( F.Name ) - 4 ) ;
        Item.SubItems.Add( Get_Value( List, 'Type' ) ) ;
        Item.SubItems.Add( Get_Value( List, 'Version' ) ) ;
        Item.SubItems.Add( Get_Value( List, 'Emulates' ) ) ;
        List.Free ;
        while( Findnext( F ) = 0 ) do
        begin
            List := Get_Description( F.Name ) ;
            Item := ListView1.Items.Add ;
            Item.Caption := copy( F.Name, 1, length( F.Name ) - 4 ) ;
            Item.SubItems.Add( Get_Value( List, 'Type' ) ) ;
            Item.SubItems.Add( Get_Value( List, 'Version' ) ) ;
            Item.SubItems.Add( Get_Value( List, 'Emulates' ) ) ;
            List.Free ;
        end ;
        ListView1.AlphaSort ;
    end ; // if...
    Caption := 'Load component' ;
    Cursor := crDefault ;
end ; // TLoad_Component_Form.wmuser


procedure TLoad_Component_Form.ListView1Click( Sender : TObject ) ;

begin
    if( ListView1.Selected = nil ) then
    begin
        exit ;
    end ;
    Filename.Text := Program_Path + 'Components\' + ListView1.Selected.Caption + '.dll' ;
end ;


procedure TLoad_Component_Form.SpeedButton1Click( Sender : TObject ) ;

begin
    if( Open_Component_Dialog.Execute ) then
    begin
        Filename.Text := Open_Component_Dialog.Filename ;
    end ;
end ;


end.
