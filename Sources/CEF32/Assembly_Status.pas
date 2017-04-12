{
        Program Name : Assembly_Status
        Package Name : CEF
        Purpose      : CEF assembly status
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Jun-2004
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

          This form is used to show assembly status as it progresses, for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Assembly_Status ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls,
     StdCtrls, Buttons,

     // C&C...
     CommonUt, // TInteger_List
     TDialogs, // TDialog
     TextStrs ; // Text_*

type
  TAssembly_Statistics = class( TDialog )
    Button_Panel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Data_Label: TLabel;
    Code_Label: TLabel;
    Warnings_Label: TLabel;
    Errors_Label: TLabel;
    Label5: TLabel;
    Filename: TLabel;
    List_Box: TListBox;
    Abort_Button: TSpeedButton;
    procedure Abort_ButtonClick(Sender: TObject);
    procedure List_BoxDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    { Private declarations }

  public // API...
      Aborted : boolean ;
      Filenames : TStringList ;
      Lines : TInteger_List ;
  end ;

var Assembly_Statistics : TAssembly_Statistics ;

implementation

uses CEFMain ;

{$R *.dfm}

procedure TAssembly_Statistics.Abort_ButtonClick( Sender : TObject ) ;

begin
    if( Abort_Button.Caption = Text_Button_Caption_Abort ) then
    begin
        Aborted := True ;
        ModalResult := mrNone ;
    end else
    begin
        ModalResult := mrOK ;
        Hide ;
    end ;
end ;


procedure TAssembly_Statistics.List_BoxDblClick( Sender : TObject ) ;

begin
    Main_Form.Show_Error( List_Box ) ;
end ;


procedure TAssembly_Statistics.FormCreate(Sender: TObject);

begin
    Filenames := TStringList.Create ;
    Lines := TInteger_List.Create ;
end ;


procedure TAssembly_Statistics.FormDestroy(Sender: TObject);

begin
    Filenames.Free ;
    Filenames := nil ;
    Lines.Free ;
    Lines := nil ;
end ;


procedure TAssembly_Statistics.FormShow( Sender : TObject ) ;

begin
    Caption := 'Assembly status' ;
    Label1.Caption := 'Data: ' ;
    Label2.Caption := 'Code:' ;
    Label3.Caption := 'Warnings:' ;
    Label4.Caption := 'Errors:' ;
    Label5.Caption := 'File:' ;
end ;


end.
