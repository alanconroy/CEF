{
        Program Name : MITS_Altair
        Package Name : CEF
        Purpose      : MITS Altair 8800 CEF component main form
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2007 by Alan Conroy.  Released to the public domain.

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

          This form represents the Altair 8800 front panel (and IMSAI
        8080).

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Altair_Form ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls, Menus, Buttons ;

type
  TFront_Panel_Form = class(TForm)
    Control_Box: TGroupBox;
    Data_Box: TGroupBox;
    Address_Box: TGroupBox;
    Read_LED: TShape;
    Write_LED: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Memory_LED: TShape;
    IO_LED: TShape;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    IMSAI_Panel: TPanel;
    Options1: TMenuItem;
    Emulation1: TMenuItem;
    Altair1: TMenuItem;
    IMSAI1: TMenuItem;
    Debug1: TMenuItem;
    Ports1: TMenuItem;
    Run_Button: TSpeedButton;
    Help1: TMenuItem;
    About1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure IMSAI1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Debug1Click(Sender: TObject);
    procedure Ports1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Run_ButtonClick(Sender: TObject);
    procedure About1Click(Sender: TObject);

  private // Instance data...

  public
    { Public declarations }
  end ;

implementation

uses // CEF...
     _CEF, // TUI_Interface

     // Altair...
     AboutBox, // About_Form
     MITS_Altair, // _UI
     Port_Dialog ; // TPort_Form

{$R *.dfm}

procedure TFront_Panel_Form.FormResize( Sender : TObject ) ;

var Control : TControl ;
    Dummy, Loop : integer ;

begin
    Dummy := Control_Box.Width ;
    Control_Box.Width := ClientWidth - COntrol_Box.Left * 2 ;
    Address_Box.Width := Control_Box.Width ;
    Data_Box.Width := Control_Box.Width ;
    Dummy := Control_Box.Width - Dummy ; // Difference in width
    for Loop := 0 to Data_Box.ControlCount - 1 do
    begin
        Control := Data_Box.Controls[ Loop ] ;
        Control.Left := Control.Left + Dummy ;
    end ;
    for Loop := 0 to Address_Box.ControlCount - 1 do
    begin
        Control := Address_Box.Controls[ Loop ] ;
        Control.Left := Control.Left + Dummy ;
    end ;
end ;



procedure TFront_Panel_Form.IMSAI1Click(Sender: TObject);

begin
    TMenuItem( Sender ).Checked := True ;
    IMSAI_Panel.Visible := IMSAI1.Checked ;
    if( IMSAI1.Checked ) then
    begin
        Caption := 'IMSAI 8080' ;
    end else
    begin
        Caption := 'Altair 8800' ;
    end ;
end ;


procedure TFront_Panel_Form.Exit1Click(Sender: TObject);

begin
    _Altair._User_Interface._UI.Terminate ;
end ;


procedure TFront_Panel_Form.Debug1Click(Sender: TObject);

begin
    _Altair._User_Interface._UI.Hide( True ) ;
end ;


procedure TFront_Panel_Form.Ports1Click( Sender : TObject ) ;

    procedure Delete_Port( Loop : integer ) ;

    begin
        if( _Altair.Port_Components[ True, Loop ] <> nil ) then
        begin
            _Altair.Port_Components[ True, Loop ].Terminate ;
            FreeLibrary( _Altair.Port_Component_Handles[ True, Loop ] ) ;
            _Altair.Port_Components[ True, Loop ] := nil ;
        end ;
        if( _Altair.Port_Components[ False, Loop ] <> nil ) then
        begin
            _Altair.Port_Components[ False, Loop ].Terminate ;
            FreeLibrary( _Altair.Port_Component_Handles[ False, Loop ] ) ;
            _Altair.Port_Components[ False, Loop ] := nil ;
        end ;
        if( _Altair.Port_Components[ True, Loop + 1 ] <> nil ) then
        begin
            _Altair.Port_Components[ True, Loop + 1 ].Terminate ;
            FreeLibrary( _Altair.Port_Component_Handles[ True, Loop + 1 ] ) ;
            _Altair.Port_Components[ True, Loop + 1 ] := nil ;
        end ;
        if( _Altair.Port_Components[ False, Loop + 1 ] <> nil ) then
        begin
            _Altair.Port_Components[ False, Loop + 1 ].Terminate ;
            FreeLibrary( _Altair.Port_Component_Handles[ False, Loop + 1 ] ) ;
            _Altair.Port_Components[ False, Loop + 1 ] := nil ;
        end ;
    end ;

var C : TComponent ;
    Dlg : TPort_Form ;
    Dummy, Loop : integer ;
    H : THandle ;
    P : function( Serial_Number : integer ; UI : TUI_Interface ) : TComponent ; stdcall ;
    S, S1 : string ;

begin
    Dlg := TPort_Form.Create( Application ) ;
    try
        Dlg.Define( 255, True, '* Panel switches' ) ;
        if( IMSAI1.Checked ) then
        begin
            Dlg.Define( 255, False, '* Panel LEDs' ) ;
        end ;
        for Loop := 0 to 255 do
        begin
            if( _Altair.Port_Components[ True, Loop ] <> nil ) then
            begin
                S := string( _Altair.Port_Components[ True, Loop ].Name ) ;
                if( S = 'MITS 88-SIO' ) then
                begin
                    S := S + ' port ' + inttostr( Loop div 2 ) ;
                    Dlg.Define( Loop, True, S + ' Status' ) ;
                    Dlg.Define( Loop, False, S + ' Status' ) ;
                    if( _Altair.Port_Connections[ True, Loop ] <> nil ) then
                    begin
                        S1 := ' -> ' + string( _Altair.Port_Connections[ True, Loop ].Name ) ;
                    end else
                    begin
                        S1 := '' ;
                    end ;
                    Dlg.Define( Loop + 1, True, S + ' Input channel' + S1 ) ;
                    if( _Altair.Port_Connections[ False, Loop ] <> nil ) then
                    begin
                        S1 := ' -> ' + string( _Altair.Port_Connections[ False, Loop ].Name ) ;
                    end else
                    begin
                        S1 := '' ;
                    end ;
                    Dlg.Define( Loop + 1, False, S + ' Output channel' + S1 ) ;
                    continue ;
                end ;
                Dlg.Define( Loop, True, S ) ;
            end ;
            if( _Altair.Port_Components[ False, Loop ] <> nil ) then
            begin
                Dlg.Define( Loop, False, string( _Altair.Port_Components[ True, Loop ].Name ) ) ;
            end ;
        end ;
        if( Dlg.ShowModal = mrOK ) then
        begin
            Loop := 0 ;
            while( Loop <= 255 ) do
            begin
                S := Dlg.Defined( True, Loop ) ;
                if( copy( S, 1, 17 ) = 'MITS 88-SIO port ' ) then
                begin
                    if(
                        ( _Altair.Port_Components[ True, Loop ] = nil ) // Added port
                        or
                        ( _Altair.Port_Components[ True, Loop ].Name <> 'MITS 88-SIO' ) // Replaced one port with a serial port
                      ) then
                    begin
                        Delete_Port( Loop ) ; // Delete existing port

                        // Create new serial port
                        H := LoadLibrary( _Altair._User_Interface._UI.Get_Component_Filename( 'SIO' ) ) ;
                        C := nil ;
                        if( H <> 0 ) then
                        begin
                            P := GetProcAddress( H, 'Make_Instance' ) ;
                            if( assigned( P ) ) then
                            begin
                                C := P( 0, _Altair._User_Interface._UI ) ;
                            end else
                            begin
                                H := 0 ;
                            end ;
                        end ;
                        _Altair.Port_Component_Handles[ True, Loop ] := H ;
                        _Altair.Port_Components[ True, Loop ] := C ;
                        C.Memory.Set_Address_Range( Loop, Loop + 1 ) ;
                        C.Parent := _Altair ;
                        C.Connect_Output( _Altair ) ;
                    end ; // if

                    S := Dlg.Defined( True, Loop + 1 ) ;
                    Dummy := pos( ' -> ', S ) ;
                    if( Dummy = 0 ) then // No port connection
                    begin
                        if( _Altair.Port_Connections[ True, Loop ] <> nil ) then // Remove existing port connection
                        begin
                            _Altair.Port_Connections[ True, Loop ].Terminate ;
                            _Altair.Port_Connections[ True, Loop ] := nil ;
                        end ;
                    end else
                    begin
                        // Connection was specified in dialog...
                        S := copy( S, Dummy + 4, length( S ) ) ; // Get connection name
                        if( _Altair.Port_Connections[ True, Loop ] <> nil ) then
                        begin
                            if( string( _Altair.Port_Connections[ True, Loop ].Name ) <> S ) then
                            begin // Current connection is different than dialog, so delete current
                                _Altair.Port_Connections[ True, Loop ].Terminate ;
                                _Altair.Port_Connections[ True, Loop ] := nil ;
                            end ;
                        end ;
                        if( _Altair.Port_Connections[ True, Loop ] = nil ) then // Need to connect
                        begin
                            // Load port connection component...
                            H := LoadLibrary( PChar( Dlg.Connections[ Loop + 1 ] ) ) ;
                            C := nil ;
                            if( H <> 0 ) then
                            begin
                                P := GetProcAddress( H, 'Make_Instance' ) ;
                                if( assigned( P ) ) then
                                begin
                                    C := P( 0, _Altair._User_Interface._UI ) ;
                                end ;
                            end ;
                            _Altair.Port_Connections[ True, Loop ] := C ;
                            C.Parent := _Altair ;

                            // Connect component to port...
                            _Altair.Port_Components[ True, Loop ].Connect_Input( C ) ;
                            _Altair.Port_Components[ True, Loop ].Connect_Output( C ) ;
                            C.Connect_Output( _Altair.Port_Components[ True, Loop ] ) ;
                            C.Connect_Input( _Altair.Port_Components[ True, Loop ] ) ;
                        end ;
                    end ;
                    Loop := Loop + 1 ;
                end else
                if( S = 'Unassigned' ) then // An unassigned port
                begin
                    if( _Altair.Port_Components[ True, Loop ] <> nil ) then
                    begin
                        Delete_Port( Loop ) ; // Delete existing port
                    end ;
                end ;
                Loop := Loop + 1 ;
            end ; // while( Loop <= 255 )
        end ; // if( Dlg.ShowModal = mrOK )
    finally
        Dlg.Free ;
    end ;
end ; // TFront_Panel_Form.Ports1Click


procedure TFront_Panel_Form.FormClose( Sender : TObject ;
    var Action : TCloseAction ) ;

begin
    Action := caFree ;
end ;


procedure TFront_Panel_Form.Run_ButtonClick( Sender : TObject ) ;

begin
    if( Run_Button.Down ) then
    begin
        _Altair._User_Interface._UI.Run( True ) ;
    end else
    begin
        _Altair._User_Interface._UI.Run( False ) ;
    end ;
end ;


procedure TFront_Panel_Form.About1Click( Sender : TObject ) ;

begin
    About_Form.ShowModal ;
end ;


end.

