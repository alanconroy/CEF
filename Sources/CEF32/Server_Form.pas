{
        Program Name : CEF32
        Package Name : CEF32
        Purpose      : CEF32 Server form
        Institution  :
        Date Written : 2-Nov-2014
        Written By   : Alan Conroy
        Version      : 2.0

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

          This form manages serve remotes for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Server_Form ;

interface

uses
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls,

     // CEF32...
     _CEF ; // TUI_Interface

type
  TServer_Dialog = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ScrollBox1: TScrollBox;
    procedure FormShow(Sender: TObject);

  private
      Server_Info : TList ;

      procedure Open_DLL( S : string ) ;
      procedure Enable_Button( Sender : TObject ) ;
      procedure Disable_Button( Sender : TObject ) ;
      procedure Install_Button( Sender : TObject ) ;
      procedure Uninstall_Button( Sender : TObject ) ;

  public
      UI : TUI_Interface ;
  end ;

var Server_Dialog : TServer_Dialog = nil ;


implementation

{$R *.dfm}

uses // C&C...
     O_S ;

var Servers : TList ;

type TServer_Record = class
                          Name : TLabel ;
                          Edit_Box : TEdit ;
                          Enable_Button : TSpeedButton ;
                          Disable_Button : TSpeedButton ;
                          Install_Button : TSpeedButton ;
                          Uninstall_Button : TSpeedButton ;
                      end ;

type FGet_Instance = function ( UI : TUI_Interface ) : TCEF_Remote ; stdcall ;

procedure TServer_Dialog.Open_DLL( S : string ) ;

var F : FGet_Instance ;
    H : THandle ;

begin
    H := loadlibrary( PChar( S ) ) ;
    if( H <> 0 ) then
    begin
        F := GetProcAddress( H, 'Get_Instance' ) ;
        if( @F <> nil ) then
        begin
            Servers.Add( F( UI ) ) ;
        end ;
    end ;
end ;


procedure TServer_Dialog.Enable_Button( Sender : TObject ) ;

var Index : integer ;
    Server_Record : TServer_Record ;

begin
    Index := TSpeedButton( Sender ).Tag ;
    Server_Record := TServer_Record( Server_Info[ Index ] ) ;
    TCEF_Remote( Servers[ Index ] ).Enable ;
    Server_Record.Disable_Button.Enabled := True ;
    Server_Record.Enable_Button.Enabled := False ;
end ;


procedure TServer_Dialog.Disable_Button( Sender : TObject ) ;

var Index : integer ;
    Server_Record : TServer_Record ;

begin
    Index := TSpeedButton( Sender ).Tag ;
    Server_Record := TServer_Record( Servers[ Index ] ) ;
    TCEF_Remote( Servers[ Index ] ).Disable ;
    Server_Record.Disable_Button.Enabled := False ;
    Server_Record.Enable_Button.Enabled := True ;
end ;


procedure TServer_Dialog.Install_Button( Sender : TObject ) ;

var Index : integer ;
    Server_Record : TServer_Record ;

begin
    Index := TSpeedButton( Sender ).Tag ;
    Server_Record := TServer_Record( Servers[ Index ] ) ;
    //~~~
end ;


procedure TServer_Dialog.Uninstall_Button( Sender : TObject ) ;

var Index : integer ;
    Server_Record : TServer_Record ;

begin
    Index := TSpeedButton( Sender ).Tag ;
    Server_Record := TServer_Record( Servers[ Index ] ) ;
end ;


procedure TServer_Dialog.FormShow(Sender: TObject) ;

var B : TSpeedButton ;
    Dummy, Dummy1 : integer ;
    E : TEdit ;
    L : TLabel ;
    S : string ;
    T : integer ;
    TR : TSearchRec ;
    Server : TCEF_Remote ;
    Server_Record : TServer_Record ;

begin
    if( ScrollBox1.ControlCount > 0 ) then // Already set up
    begin
        exit ;
    end ;

    // Setup...
    Server_Info := TList.Create ;

    // Load interfaces...
    S := OS^.Application_Path + 'remotes\' ;
    if( FindFirst( S + '*.dll', -1, TR ) = 0 ) then
    begin
        Open_DLL( S + TR.Name ) ;
        while( FindNext( TR ) = 0 ) do
        begin
            Open_DLL( S + TR.Name ) ;
        end ;
    end ;
    FindClose( TR ) ;

    // Build list of interfaces...
    T := 8 ;
    for Dummy := 0 to Servers.Count - 1 do
    begin
        Server := TCEF_Remote( Servers[ Dummy ] ) ;
        L := TLabel.Create( self ) ;
        L.Parent := ScrollBox1 ;
        L.Top := T ;
        L.Left := 8 ;
        L.Caption := Server.Name ;
        E := TEdit.Create( self ) ;
        E.Parent := ScrollBox1 ;
        E.Left := 250 ;
        E.Top := T ;
        E.Text := Server.Get_ID( Dummy1 ) ;
        B := TSpeedButton.Create( self ) ;
        B.Parent := ScrollBox1 ;
        B.Tag := Dummy ;
        B.Top := T ;
        B.Left := 380 ;
        B.Caption := 'Disable' ;
        B.Width := 50 ;
        B.OnClick := Disable_Button ;
        Server_Record := TServer_Record.Create ;
        Server_Record.Disable_Button := B ;
        B := TSpeedButton.Create( self ) ;
        B.Parent := ScrollBox1 ;
        B.Tag := Dummy ;
        B.Top := T ;
        B.Left := 440 ;
        B.Caption := 'Enable' ;
        B.Enabled := False ;
        B.Tag := Dummy ;
        B.Width := 50 ;
        B.OnClick := Enable_Button ;
        Server_Record.Enable_Button := B ;
        B := TSpeedButton.Create( self ) ;
        B.Parent := ScrollBox1 ;
        B.Tag := Dummy ;
        B.Top := T ;
        B.Left := 500 ;
        B.Caption := 'Install' ;
        B.Tag := Dummy ;
        B.Width := 50 ;
        B.OnClick := Install_Button ;
        Server_Record.Install_Button := B ;
        B := TSpeedButton.Create( self ) ;
        B.Parent := ScrollBox1 ;
        B.Tag := Dummy ;
        B.Top := T ;
        B.Left := 560 ;
        B.Caption := 'Uninstall' ;
        B.Tag := Dummy ;
        B.Width := 50 ;
        B.OnClick := Uninstall_Button ;
        Server_Record.Uninstall_Button := B ;
        T := T + L.Height + 8 ;
        Server_Record.Name := L ;
        Server_Record.Edit_Box := E ;
        Server_Info.Add( Server_Record ) ;
    end ; // for Dummy := 0 to Servers.Count - 1
end ;


procedure Close_Servers ;

var I : integer ;

begin
    for I := 0 to Servers.Count - 1 do
    begin
        TCEF_Remote( Servers[ I ] ).Terminate ;
        Servers[ I ] := nil ;
    end ;
    Servers.Free ;
end ;


initialization
    Servers := TList.Create ;

finalization
    Close_Servers ;
end.
