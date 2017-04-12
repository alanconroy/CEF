{
        Program Name : UNIBUS
        Package Name : CEF
        Purpose      : DEC PDP-11 UNIBUS CEF component main form
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2008 by Alan Conroy.  Released to the public domain.

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

          This provides a UNIBUS system front panel.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit UNIBUS_Form ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls, Menus, Buttons,

     // C&C...
     CommonUt, // TInteger_List
     
     // CEF...
     PortMan, // TPort_Manager
     _CEF ; // TUI_Interface

type TDL11 = class
                 public // Constructors and destructors...
                     destructor Destroy ; override ;

                 public // API...
                     Component : TComponent ;
                     Terminal_Component : TComponent ;

                     procedure Kill ;
             end ;

     TDL11_List = class( TList )
                       public // Constructors and destructors...
                           destructor Destroy ; override ;

                       protected { Property handlers }
                           function Get_Index( Index : integer ) : TDL11 ;
                               virtual ;
                           procedure Set_Index( Index : integer ; Value : TDL11 ) ;
                               virtual ;

                       public { API... }
                           function Add( Value : TDL11 ) : integer ;
                               virtual ;
                           procedure Insert( Index : integer ; Value : TDL11 ) ;
                               virtual ;
                           procedure Delete( Index : integer ) ; virtual ;

                           property Items[ Index : integer ] : TDL11
                               read Get_Index
                               write Set_Index ; default ;
                  end ;

type TFront_Panel_Form = class(TForm)
    Control_Box: TGroupBox;
    Data_Box: TGroupBox;
    Address_Box: TGroupBox;
    Read_LED: TShape;
    Write_LED: TShape;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Debug1: TMenuItem;
    Run_Button: TSpeedButton;
    Run_LED: TShape;
    Label3: TLabel;
    Pause_LED: TShape;
    Label4: TLabel;
    User_LED: TShape;
    Super_LED: TShape;
    Kernel_LED: TShape;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Data_LED: TShape;
    Label8: TLabel;
    Addressing_16_LED: TShape;
    Addressing_18_LED: TShape;
    Addressing_22_LED: TShape;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Mode_Box: TGroupBox;
    Prog_Phys_LED: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Data_Display_Box: TGroupBox;
    Data_Paths_LED: TShape;
    Bus_Reg_LED: TShape;
    Shape20: TShape;
    Display_Register_LED: TShape;
    Label21: TLabel;
    Data_Paths_Label: TLabel;
    Bus_Reg_Label: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Data_LED0 : TShape ;
    Data_LED1 : TShape ;
    Data_LED2 : TShape ;
    Data_LED3 : TShape ;
    Data_LED4 : TShape ;
    Data_LED5 : TShape ;
    Data_LED6 : TShape ;
    Data_LED7 : TShape ;
    Data_LED8 : TShape ;
    Data_LED9 : TShape ;
    Data_LED10 : TShape ;
    Data_LED11 : TShape ;
    Data_LED12 : TShape ;
    Data_LED13 : TShape ;
    Data_LED14 : TShape ;
    Data_LED15 : TShape ;
    Address_LED0 : TShape ;
    Address_LED1 : TShape ;
    Address_LED2 : TShape ;
    Address_LED3 : TShape ;
    Address_LED4 : TShape ;
    Address_LED5 : TShape ;
    Address_LED6 : TShape ;
    Address_LED7 : TShape ;
    Address_LED8 : TShape ;
    Address_LED9 : TShape ;
    Address_LED10 : TShape ;
    Address_LED11 : TShape ;
    Address_LED12 : TShape ;
    Address_LED13 : TShape ;
    Address_LED14 : TShape ;
    Address_LED15 : TShape ;
    Address_LED16 : TShape ;
    Address_LED17 : TShape ;
    Address_LED18 : TShape ;
    Address_LED19 : TShape ;
    Address_LED20 : TShape ;
    Address_LED21 : TShape ;
    Button0 : TSpeedButton ;
    Button1 : TSpeedButton ;
    Button2 : TSpeedButton ;
    Button3 : TSpeedButton ;
    Button4 : TSpeedButton ;
    Button5 : TSpeedButton ;
    Button6 : TSpeedButton ;
    Button7 : TSpeedButton ;
    Button8 : TSpeedButton ;
    Button9 : TSpeedButton ;
    Button10 : TSpeedButton ;
    Button11 : TSpeedButton ;
    Button12 : TSpeedButton ;
    Button13 : TSpeedButton ;
    Button14 : TSpeedButton ;
    Button15 : TSpeedButton ;
    Button16 : TSpeedButton ;
    Button17 : TSpeedButton ;
    Button18 : TSpeedButton ;
    Button19 : TSpeedButton ;
    Button20 : TSpeedButton ;
    Button21 : TSpeedButton ;
    Devices1: TMenuItem;
    Devices2: TMenuItem;
    erminals1: TMenuItem;
    Label22: TLabel;
    Halt_Button: TSpeedButton;
    Enabled1: TMenuItem;
    N1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Reset_Button: TSpeedButton;
    procedure Exit1Click(Sender: TObject);
    procedure Debug1Click(Sender: TObject);
    procedure Ports1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Run_ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Reset_ButtonClick(Sender: TObject);
    procedure Bus_Reg_LabelClick(Sender: TObject);
    procedure Display_Register_LabelClick(Sender: TObject);
    procedure Terminals1Click(Sender: TObject);
    procedure Enabled1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Halt_ButtonClick(Sender: TObject);

  private // Instance data...
      Mode : integer ;
      Addressing : integer ;

  public // Instance data...
      KE11_Component : TComponent ;
      DL11W : TComponent ; // DL-11 with Line clock

      KL11s : TDL11_List ;
      DL11Ds : TDL11_List ;
      Console : TComponent ; // Console DL11-W

      Support_Switch_Register : boolean ;

      Adding_Peripheral : boolean ; // True when connecting a peripheral

      On_Register_Change : TNotifyEvent ;
      _Init : boolean ;
      Outputs : TList ;
      Parent : TComponent ;
      _UI : TUI_Interface ;

  protected // Utility routines...
      procedure Reconcile( Terminal : string ; Port_Manager : TPort_Manager ;
          Index : longint ) ;

  public // API...
      procedure Configure( _DL11Ws, _KL11s, _DL11s, _KE11s : integer ) ;
      procedure Create_Console ;
      procedure Attach_Terminals( List : TStringList ) ;
      procedure Set_Addressing( Value : integer ) ;
      procedure Set_Mode( Value : integer ) ;
  end ;

implementation

uses // C&C...
     Button_2D, // TXButton
     _Octal,
     Standard, // Get_Serial_Number
     VCL_Std, // Clone_Speed_Button
     
     // UNIBUS...
     AboutBox, // About_Form
     Choose_Emulator_Port_Dlg, // Choose_Emulator_Port_Form
     DEC_UNIBUS, // _UI
     Port_Change_Dialog, // TPort_Change_Form
     Port_Dialog ; // TPort_Form

{$R *.dfm}

// TDL11_List methods...

// Constructors and destructors...

destructor TDL11_List.Destroy ;

var Loop : integer ;

begin
    for Loop := 0 to Count - 1 do
    begin
        if( Items[ Loop ] <> nil ) then
        begin
            Items[ Loop ].Free ;
        end ;
    end ;

    inherited Destroy ;
end ;


// Property handlers

function TDL11_List.Get_Index( Index : integer ) : TDL11 ;

begin
    Result := TDL11( inherited Items[ Index ] ) ;
end ;


procedure TDL11_List.Set_Index( Index : integer ; Value : TDL11 ) ;

begin
    inherited Items[ Index ] := Value ;
end ;


// API...

function TDL11_List.Add( Value : TDL11 ) : integer ;

begin
    inherited Add( Value ) ;

    Result := Count - 1 ;
end ;


procedure TDL11_List.Insert( Index : integer ; Value : TDL11 ) ;

begin
    inherited Insert( Index, Value ) ;
end ;


procedure TDL11_List.Delete( Index : integer ) ;

begin
    if( Items[ Index ] <> nil ) then
    begin
        Items[ Index ].Free ;
    end ;

    inherited Delete( Index ) ;
end ;



// TDL11 methods...

// Constructors and destructors...

destructor TDL11.Destroy ;

begin
    Kill ;

    inherited Destroy ;
end ;


procedure TDL11.Kill ;

begin
    if( Component <> nil ) then
    begin
        Component.Terminate ;
        Component := nil ;
        if( Terminal_Component <> nil ) then
        begin
            Terminal_Component.Terminate ;
        end ;
    end ;
end ;



// TFront_Panel_Form methods...

procedure TFront_Panel_Form.Exit1Click(Sender: TObject);

begin
    _UNIBUS._User_Interface._UI.Terminate ;
end ;


procedure TFront_Panel_Form.Debug1Click(Sender: TObject);

begin
    _UNIBUS._User_Interface._UI.Hide( True ) ;
end ;


procedure TFront_Panel_Form.Ports1Click( Sender : TObject ) ;

var Dlg : TPort_Form ;

begin
    Dlg := TPort_Form.Create( Application ) ;
    try
        if( DL11W <> nil ) then // A DL11-W console
        begin
            Dlg.Console.Value := 1 ;
            Dlg.KL11.MaxValue := 30 ;
            Dlg.KL11.Value := KL11s.Count ;
        end else
        begin
            Dlg.Console.Value := 0 ;
            Dlg.KL11.MaxValue := 31 ;
            Dlg.KL11.Value := KL11s.Count + 1 ;
        end ;
        if( KE11_Component <> nil ) then
        begin
            Dlg.KE11.Value := 1 ;
        end else
        begin
            Dlg.KE11.Value := 0 ;
        end ;
        Dlg.DL11D.Value := DL11Ds.Count ;
        Dlg.Support_Switch_Register.Checked := Support_Switch_Register ;

        if( Dlg.ShowModal = mrOK ) then
        begin
            Configure( Dlg.Console.Value, Dlg.KL11.Value, Dlg.DL11D.Value, Dlg.KE11.Value ) ;
            Support_Switch_Register := Dlg.Support_Switch_Register.Checked ;
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
    _UNIBUS._User_Interface._UI.Run( True ) ;
end ;


type TFake_SpeedButton = class( TSpeedButton ) ; // For access to protected methods...

procedure TFront_Panel_Form.FormCreate( Sender : TObject ) ;

    function Load( Filename : string ) : TComponent ;

    begin
        Result := _UNIBUS._User_Interface._UI.Load_Component( PChar( Filename ) ) ;
        if( Result = nil ) then
        begin
            exit ;
        end ;
        Adding_Peripheral := True ;
        try
            Result.Connect_Input( _UNIBUS ) ;
        finally
            Adding_Peripheral := False ;
        end ;
    end ;


    procedure Replace_Button( var Button : TSpeedButton ; Background : TColor ) ;

    var B : TXButton ;

    begin
        B := TXButton.Create( Button.Owner ) ;
        Clone_Speed_Button( Button, B ) ;
        B.Color := Background ;
        Button.Visible := False ;
        Button.Top := -100 ;
//        Button.Free ;
        Button := B ;
    end ;


begin // TFront_Panel_Form.FormCreate
    KL11s := TDL11_List.Create ;
    DL11Ds := TDL11_List.Create ;
//    Create_Console ;
    Replace_Button( Button0, clMaroon ) ;
    Replace_Button( Button1, clMaroon ) ;
    Replace_Button( Button2, clMaroon ) ;
    Replace_Button( Button3, clPurple ) ;
    Replace_Button( Button4, clPurple ) ;
    Replace_Button( Button5, clPurple ) ;
    Replace_Button( Button6, clMaroon ) ;
    Replace_Button( Button7, clMaroon ) ;
    Replace_Button( Button8, clMaroon ) ;
    Replace_Button( Button9, clPurple ) ;
    Replace_Button( Button10, clPurple ) ;
    Replace_Button( Button11, clPurple ) ;
    Replace_Button( Button12, clMaroon ) ;
    Replace_Button( Button13, clMaroon ) ;
    Replace_Button( Button14, clMaroon ) ;
    Replace_Button( Button15, clPurple ) ;
    Replace_Button( Button16, clPurple ) ;
    Replace_Button( Button17, clPurple ) ;
    Replace_Button( Button18, clMaroon ) ;
    Replace_Button( Button19, clMaroon ) ;
    Replace_Button( Button20, clMaroon ) ;
    Replace_Button( Button21, clPurple ) ;
    Replace_Button( Run_Button, clMaroon ) ;
    Replace_Button( Halt_Button, clMaroon ) ;
    Replace_Button( Reset_Button, clMaroon ) ;
end ; // TFront_Panel_Form.FormCreate


procedure TFront_Panel_Form.FormDestroy( Sender : TObject ) ;

begin
    Console.Terminate ;
    Console := nil ;
    if( KE11_Component <> nil ) then
    begin
        KE11_Component.Terminate ;
    end ;
    DL11W := nil  ;
    KE11_Component := nil ;

    KL11s.Free ;
    KL11s := nil ;
    DL11Ds.Free ;
    DL11Ds := nil ;
end ;


procedure TFront_Panel_Form.Reset_ButtonClick( Sender : TObject ) ;

begin
    _UI.Signal_Change_Notice( Parent, 0, True ) ;
    _UI.Signal_Change_Notice( Parent, 0, False ) ;
end ;


procedure TFront_Panel_Form.Bus_Reg_LabelClick(Sender: TObject);

begin
    Bus_Reg_LED.Brush.Color := clRed ;
    Display_Register_LED.Brush.Color := clMaroon ;
end ;


procedure TFront_Panel_Form.Display_Register_LabelClick(Sender: TObject);

begin
    Bus_Reg_LED.Brush.Color := clMaroon ;
    Display_Register_LED.Brush.Color := clRed ;
    On_Register_Change( self ) ;
end ;


procedure TFront_Panel_Form.Terminals1Click( Sender : TObject ) ;

begin
    Choose_Emulator_Port_Form := TChoose_Emulator_Port_Form.Create( Application ) ;
    Choose_Emulator_Port_Form.Caption := 'Change port configuration' ;
    try
        Choose_Emulator_Port_Form.Component := _UNIBUS ;
        Choose_Emulator_Port_Form.UI := _UI ;
        Choose_Emulator_Port_Form.ShowModal ;
    finally
        Choose_Emulator_Port_Form.Free ;
        Choose_Emulator_Port_Form := nil ;
    end ;
end ; // TFront_Panel_Form.Terminals1Click


// Utility routines...

procedure TFront_Panel_Form.Create_Console ;

begin
    if( Console = nil ) then // Not already created
    begin
        DL11W := _UI.Load_Component( 'KW11L' ) ;
        Console := _UI.Load_Component( 'DL11' ) ;
        if( Console = nil ) then
        begin
            DL11W.Terminate ;
            DL11W := nil ;
            exit ;
        end ;
        Console.Set_Up( 'MODEL W' ) ;
        if( _UNIBUS.Port_Manager.Count = 0 ) then
        begin
            _UNIBUS.Port_Manager.Add_Port( 'Console', Console ) ;
        end else
        begin
            _UNIBUS.Port_Manager.Set_Port( 0, Console ) ;
        end ;
    end ; // if( Console = nil )
end ;


procedure TFront_Panel_Form.Reconcile( Terminal : string ; Port_Manager : TPort_Manager ;
    Index : longint ) ;

var Component, Existing_Terminal : TComponent ;
    S : string ;

begin
    Existing_Terminal := Port_Manager.Port_Connection( Index ) ;
    if( Existing_Terminal = nil ) then
    begin
        S := '' ;
    end else
    begin
        S := Existing_Terminal.Name ;
    end ;
    Terminal := Edit( Terminal, 4 or 8 or 32 or 128 ) ;
    if( Terminal <> S ) then // Terminal changed
    begin
        if( Terminal = '' ) then // Remove terminal
        begin
            if( Existing_Terminal <> nil ) then
            begin
                Existing_Terminal.Terminate ;
            end ;
        end else
        begin // Added terminal
            Adding_Peripheral := True ;
            try
                Component := _UI.Load_Component( PChar( Terminal ) ) ;
                if( Component <> nil ) then
                begin
                    Port_Manager.Set_Terminal( Index, Component ) ;
                    Component.Connect_Input( Port_Manager.Port_Device( Index ) ) ;
                end ;
            finally
                Adding_Peripheral := False ;
            end ;
        end ;
    end ; // if( Terminal <> S )
end ; // TFront_Panel_Form.Terminals1Click.Reconcile

// API...

procedure TFront_Panel_Form.Configure( _DL11Ws, _KL11s, _DL11s, _KE11s : integer ) ;

var A, V : integer ;
    C : TComponent ;
    DL11 : TDL11 ;
    I, Loop : integer ;
    S : string ;

begin
    if( DL11W = nil ) then
    begin
        I := 0 ;
    end else
    begin
        I := 1 ;
    end ;
    Adding_Peripheral := True ;
    try
        // Handle DL11-W
        if( _DL11Ws = 0 ) then
        begin
            if( Console <> nil ) then
            begin
                if( DL11W <> nil ) then
                begin
                    DL11W.Terminate ;
                end ;
                if( Console <> nil ) then
                begin
                    Console.Free ;
                end ;
                DL11W := nil ;
                Console := nil ;
                _UNIBUS.Port_Manager.Delete_Port( 0 ) ;
            end ;
        end else
        begin
            Create_Console ;
            I := 1 ;
        end ;

        // Handle KL11/DL11s
        while( KL11s.Count > _KL11s ) do // Removed some KL11s.
        begin
            KL11s.Delete( KL11s.Count - 1 ) ;
            _UNIBUS.Port_Manager.Delete_Port( I ) ;
        end ;
        while( KL11s.Count < _KL11s ) do // Added some KL11s.
        begin
            C := _UI.Load_Component( 'DL11' ) ;
            if( C <> nil ) then
            begin
                DL11 := TDL11.Create ;
                DL11.Component := C ;
                KL11s.Add( DL11 ) ;
                _UNIBUS.Port_Manager.Insert_Port( I, C.Name, C ) ;
            end ;
        end ;
        I := I + KL11s.Count ;

        // Set up KL11 addresses...
        A := O776500 ;
        V := O300 ;
        for Loop := 0 to KL11s.Count - 1 do
        begin
            C := KL11s[ Loop ].Component ;
            S := 'ADDRESS ' + inttostr( A ) + '. VECTOR ' + inttostr( V ) + '.' ;
            C.Set_up( PChar( S ) ) ;
            A := A + 8 ;
            V := V + 4 ;
        end ;

        // Handle DL11Ds
        while( DL11Ds.Count > _DL11s ) do // Removed DL11s
        begin
            DL11Ds.Delete( DL11Ds.Count - 1 ) ;
            _UNIBUS.Port_Manager.Delete_Port( I ) ;
        end ;
        while( DL11Ds.Count < _DL11s ) do // Added some DL11s
        begin
            C := _UI.Load_Component( 'DL11' ) ;
            if( C <> nil ) then
            begin
                DL11 := TDL11.Create ;
                DL11.Component := C ;
                DL11Ds.Add( DL11 ) ;
                _UNIBUS.Port_Manager.Insert_Port( I, C.Name, C ) ;
            end ;
        end ;

        // Set up DL11 addresses...
        A := O775610 ;
        for Loop := 0 to DL11Ds.Count - 1 do
        begin
            C := DL11Ds[ Loop ].Component ;
            S := 'ADDRESS ' + inttostr( A ) + '. VECTOR ' + inttostr( V ) + '.' ;
            C.Set_up( PChar( S ) ) ;
            A := A + 8 ;
            V := V + 4 ;
        end ;

        // Handle KE11
        if( _KE11s = 0 ) then
        begin
            if( KE11_Component <> nil ) then // Removing existing KE11
            begin
                KE11_Component.Terminate ;
                KE11_Component := nil ;
            end ;
        end else
        begin
            if( KE11_Component = nil ) then // Adding KE11
            begin
                KE11_Component := _UI.Load_Component( 'EAE' ) ;
            end ;
        end ;
    finally
        Adding_Peripheral := False ;
    end ;
end ; // TFront_Panel_Form.Configure


procedure TFront_Panel_Form.Attach_Terminals( List : TStringList ) ;

var C : TComponent ;
    I, Loop : integer ;
    Offset : integer ;

begin // TFront_Panel_Form.Attach_Terminals
    Offset := 0 ;
    I := 0 ;
    for Loop := 0 to List.Count - 1 do
    begin
        if( ( Loop = 0 ) and ( Console <> nil ) ) then // Console
        begin
            inc( Offset ) ;
            Create_Console ; // Make sure we have a console
            Reconcile( List[ Loop ], _UNIBUS.Port_Manager, Loop ) ;
            if( Console <> nil ) then
            begin
                Console.Set_Up( 'CAPTION Console' ) ;
            end ;
        end else
        if( Loop - Offset <= KL11s.Count ) then
        begin
            C := KL11s[ Loop - Offset ].Terminal_Component ;
            Reconcile( List[ Loop ], _UNIBUS.Port_Manager, Loop ) ;
            KL11s[ Loop - Offset ].Terminal_Component := C ;
            if( C <> nil ) then
            begin
                C.Set_Up( PChar( 'CAPTION DL' + inttostr( I ) ) ) ;
                inc( I ) ;
            end ;
        end else
        begin
            C := DL11Ds[ Loop - KL11s.Count - Offset ].Terminal_Component ;
            Reconcile( List[ Loop ], _UNIBUS.Port_Manager, Loop ) ;
            DL11Ds[ Loop - KL11s.Count - Offset ].Terminal_Component := C ;
            if( C <> nil ) then
            begin
                C.Set_Up( PChar( 'CAPTION DL' + inttostr( I ) ) ) ;
                inc( I ) ;
            end ;
        end ;
    end ;
end ; // TFront_Panel_Form.Attach_Terminals



procedure TFront_Panel_Form.Enabled1Click( Sender : TObject ) ;

begin
    Enabled1.Checked := not Enabled1.Checked ;
    if( Enabled1.Checked ) then // Make sure state lights are correctly set
    begin
        Set_Addressing( Addressing ) ;
        Set_Mode( Mode ) ;
    end ;
end ;


procedure TFront_Panel_Form.About1Click( Sender : TObject ) ;

begin
    About_Form.ShowModal ;
end ;


procedure TFront_Panel_Form.Set_Addressing( Value : integer ) ;

begin
    Addressing := Value ;
    case Value of
        16 :
            begin
                Addressing_16_LED.Brush.Color := clRed ;
                Addressing_18_LED.Brush.Color := clMaroon ;
                Addressing_22_LED.Brush.Color := clMaroon ;
            end ;
        18 :
            begin
                Addressing_16_LED.Brush.Color := clMaroon ;
                Addressing_18_LED.Brush.Color := clRed ;
                Addressing_22_LED.Brush.Color := clMaroon ;
            end ;
        22 :
            begin
                Addressing_16_LED.Brush.Color := clMaroon ;
                Addressing_18_LED.Brush.Color := clMaroon ;
                Addressing_22_LED.Brush.Color := clRed ;
            end ;
    end ;
end ;


procedure TFront_Panel_Form.Set_Mode( Value : integer ) ;

begin
    Mode := Value ;
    case Value of
        0 :
            begin
                Kernel_LED.Brush.Color := clRed ;
                Super_LED.Brush.Color := clMaroon ;
                User_LED.Brush.Color := clMaroon ;
            end ;
        3 :
            begin
                Kernel_LED.Brush.Color := clMaroon ;
                Super_LED.Brush.Color := clMaroon ;
                User_LED.Brush.Color := clRed ;
            end ;
        else
            begin
                Kernel_LED.Brush.Color := clMaroon ;
                Super_LED.Brush.Color := clRed ;
                User_LED.Brush.Color := clMaroon ;
            end ;
    end ;
end ;



procedure TFront_Panel_Form.Halt_ButtonClick(Sender: TObject);

begin
    _UNIBUS._User_Interface._UI.Run( False ) ;
end ;


end.

