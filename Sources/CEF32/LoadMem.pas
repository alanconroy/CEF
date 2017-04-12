{
              Program Name : LoadMem
              Package Name : CEF
              Purpose      : Load memory image dialog
              Institution  : Conroy & Conroy
              Date Written : 19-Mar-2005
              Written By   : Alan Conroy
              Version      : 1.0

              Copyright (C) 2005 by Alan Conroy.  Released to the public domain.

              TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *********************************************************
        *                                                       *
        *        M O D I F I C A T I O N   H I S T O R Y        *
        *                                                       *
        *     DATE      BY             REASON                   *
        *                                                       *
        *********************************************************

        *********************************************************
        *                                                       *
        *        P R O G R A M   P U R P O S E                  *
        *                                                       *
        *********************************************************

          This dialog is used to load a memory image.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit LoadMem ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TLoad_Memory_Form = class(TForm)
    Label1: TLabel;
    File_Name: TEdit;
    Label2: TLabel;
    Starting_Address: TEdit;
    Label4: TLabel;
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    BitBtn2: TBitBtn;
    Dump_Dialog: TOpenDialog;
    Browse_Button: TSpeedButton;
    Count_Label: TLabel;
    Help_Button: TBitBtn;
    procedure File_NameChange(Sender: TObject);
    procedure Browse_ButtonClick(Sender: TObject);
    procedure Starting_AddressKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public // API...
      Base : integer ;
  end;

var Load_Memory_Form : TLoad_Memory_Form = nil ;

implementation

uses // C&C...
     CommonUt, // Edit
     CVT, // Valid_Base
     Standard ; // Extension_Pos

{$R *.dfm}

procedure TLoad_Memory_Form.File_NameChange( Sender : TObject ) ;

var Dummy : integer ;
    Ending : int64 ;
    F : textfile ;
    F1 : file ;
    Last, S : string ;

begin
    OK_Button.Enabled := ( File_Name.Text <> '' ) ;
    Dummy := Extension_Pos( File_Name.Text ) ;
    if( Dummy = 0 ) then
    begin
        Dummy := length( File_Name.Text ) + 1 ;
    end ;
    if( uppercase( copy( File_Name.Text, Dummy + 1, length( File_Name.Text ) ) ) = 'ROM' ) then
    begin
        if( FileExists( File_Name.Text ) ) then
        begin
            assignfile( F1, File_Name.Text ) ;
            {$I-}
            reset( F1, 1 ) ;
            {$I+}
            if( IOResult = 0 ) then
            begin
                try
                    Count_Label.Caption := inttostr( filesize( F1 ) ) ;
                finally
                    closefile( F ) ;
                end ;
            end ;
        end ;
    end else
    if( uppercase( copy( File_Name.Text, Dummy + 1, length( File_Name.Text ) ) ) = 'ENT' ) then
    begin
        if( FileExists( File_Name.Text ) ) then
        begin
            assignfile( F, File_Name.Text ) ;
            {$I-}
            reset( F ) ;
            {$I+}
            if( IOResult = 0 ) then
            begin
                try
                    {$I-}
                    readln( F, S ) ;
                    {$I+}
                    if( IOResult = 0 ) then
                    begin
                        if( copy( S, 1, 2 ) = 'EN' ) then
                        begin
                            {$I-}
                            readln( F, S ) ;
                            {$I+}
                            if( IOResult = 0 ) then
                            begin
                                Dummy := pos( ':', S + ':' ) ;
                                S := Edit( copy( S, 1, Dummy - 1 ), 8 or 128 ) ;
                                if( Valid_Base( S, 16 ) ) then
                                begin
                                    Starting_Address.Text := CvtB( 16, Base, S ) ;
                                end ;
                                Last := S ;
                                while( not eof( F ) ) do
                                begin
                                    readln( F, S ) ;
                                    S := Edit( S, 8 or 16 or 128 ) ;
                                    if( pos( ':', S ) > 0 ) then
                                    begin
                                        Last := S ;
                                    end ;
                                end ;
                                Dummy := pos( ':', Last ) ;
                                Ending := strtoint( CvtB( 16, 10, copy( S, 1, Dummy - 1 ) ) ) ;
                                S := copy( S, Dummy + 1, length( S ) ) ;
                                Dummy := pos( '/', S + '/' ) ;
                                S := copy( S, 1, Dummy - 1 ) ;
                                S := Edit( S, 8 or 128 ) ;
                                while( length( S ) > 0 ) do
                                begin
                                    Dummy := pos( ' ', S + ' ' ) ;
                                    Ending := Ending + ( ( Dummy + 1 ) div 2 ) ;
                                    S := copy( S, Dummy + 1, length( S ) ) ;
                                    S := Edit( S, 8 or 128 ) ;
                                end ;
                                Count_Label.Caption := inttostr( Ending - strtoint( CVTb( Base, 10, Starting_Address.Text ) ) ) ;
                            end ;
                        end ;
                    end ;
                finally
                    closefile( F ) ;
                end ;
            end ; // if( IOResult = 0 )
        end ; // if( FileExists( File_Name.Text ) )
    end ; // if
end ; // TLoad_Memory_Form.File_NameChange


procedure TLoad_Memory_Form.Browse_ButtonClick(Sender: TObject);

begin
    if( Dump_Dialog.Execute ) then
    begin
        File_Name.Text := Dump_Dialog.FileName ;
    end ;
end ;


procedure TLoad_Memory_Form.Starting_AddressKeyPress(Sender: TObject;
    var Key: Char) ;

begin
    //~~~
end ;


procedure TLoad_Memory_Form.FormCreate( Sender : TObject ) ;

begin
    Base := 10 ;
end ;


procedure TLoad_Memory_Form.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.

