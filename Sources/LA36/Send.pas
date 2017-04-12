{
        Program Name : Send
        Package Name : CEF
        Purpose      : Simple terminal component File send form
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Jan-2007
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

          This is the file transfer form for a simple hardcopy terminal Emulator
        for CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Send ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TSend_Dialog = class(TForm)
    Panel1: TPanel;
    Cancel_Button: TBitBtn;
    Transfer_Button: TBitBtn;
    Label1: TLabel;
    File_Name: TEdit;
    Updates: TCheckBox;
    Progress_Bar: TProgressBar;
    Status: TLabel;
    Browse_Button: TSpeedButton;
    Transfer_File_Dialog: TOpenDialog;
    procedure Cancel_ButtonClick(Sender: TObject);
    procedure Transfer_ButtonClick(Sender: TObject);
    procedure Browse_ButtonClick(Sender: TObject);
    procedure File_NameChange(Sender: TObject);

  private // Instance data...
    Cancelling : boolean ;
    Transferring : boolean ;

  public // API...
    Main_Form : TForm ;
  end ;

implementation

{$R *.dfm}

uses // C&C...
     Standard, // ERT
     UE, // MSDOS_ERT

     // LA36...
     LA36_Main ; // TMain_Form

// TSend_Dialog methods...

procedure TSend_Dialog.Cancel_ButtonClick( Sender : TObject ) ;

begin
    if( not Transferring ) then
    begin
        exit ;
    end ;
    ModalResult := mrNone ;
    Cancelling := True ;
end ;


procedure TSend_Dialog.Transfer_ButtonClick( Sender : TObject ) ;

var Data : byte ;
    Dummy : integer ;
    F : file ;
    Last_Update : cardinal ;
    Saved : integer ;

begin
    Transfer_Button.Enabled := False ;
    Transferring := True ;
    Saved := FileMode ;
    try
        assignfile( F, File_Name.Text ) ;
        FileMode := fmOpenRead ;
        {$I-}
        reset( F, 1 ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( DOS_ERT( Dummy ) ) ;
            exit ;
        end ;
        Progress_Bar.Max := filesize( F ) ;
        Progress_Bar.Position := 0 ;
        Last_Update := GetTickCount ;
        TMain_Form( Main_Form ).Block_New_Chars := not Updates.Checked ;
        while( not eof( F ) ) do
        begin
            if( Cancelling ) then
            begin
                Status.Caption := 'Transfer aborted' ;
                break ;
            end ;
            if( TMain_Form( Main_Form ).Transmit_Paused ) then
            begin
                Application.ProcessMessages ;
                continue ;
            end ;
            if( not TMain_Form( Main_Form ).Check_Flow_Control ) then
            begin
                Application.ProcessMessages ;
                continue ;
            end ;
            {$I-}
            blockread( F, Data, 1 ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( DOS_ERT( Dummy ) ) ;
                exit ;
            end ;
            TMain_Form( Main_Form ).Write_Serial( Data ) ;
            Progress_Bar.Position := Progress_Bar.Position + 1 ;
            Status.Caption := inttostr( filepos( F ) ) + ' bytes of ' + inttostr( filesize( F ) ) + ' transferred' ;
            if( Last_Update > GetTickCount ) then // Handle wrap-around
            begin
                Last_Update := GetTickCount ;
            end ;
            if( Last_Update < GetTickCount - 250 ) then // Update every quarter second
            begin
                Last_Update := GetTickCount ;
                Application.ProcessMessages ;
            end ;
        end ;
        Status.Caption := 'Transfer complete' ;
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
    finally
        TMain_Form( Main_Form ).Block_New_Chars := False ;
        FileMode := Saved ;
        Transferring := False ;
        Transfer_Button.Enabled := True ;
    end ;
end ;


procedure TSend_Dialog.Browse_ButtonClick( Sender : TObject ) ;

begin
    if( Transfer_File_Dialog.Execute ) then
    begin
        File_Name.Text := Transfer_File_Dialog.Filename ;
    end ;
end ;


procedure TSend_Dialog.File_NameChange( Sender : TObject ) ;

begin
    Transfer_Button.Enabled := FileExists( File_Name.Text ) ;
end ;


end.
