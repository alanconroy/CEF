{
        Program Name : LA36_Main
        Package Name : CEF
        Purpose      : Simple printing terminal/printer component main form
        Institution  : Conroy & Conroy Co.
        Date Written : 2-Dec-2013
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

          This is the main form for a simple hard copy terminal Emulator for CEF.
        It encapsulates the print out and keyboard.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit LA36_Main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     TPanels, // TTransparent_Panel

     // CEF...
     _CEF, // TCable
     _CEFUtil, // TCEF_Character_Set
     CEFMedia, // TCEF_Paper_Media_Header
     Printer_Options_Form, // Printer_Options_Dialog
     Serial_Cable ; // TSerial_Cable

type THC_Mode = ( HCM_KSR33, // KSR 33 Teletype
                  HCM_ASR33, // ASR 33 Teletype (KSR with paper tape punch/reader)
                  HCM_RO33, // KSR with not keyboard
                  HCM_LA30, // DEC LA30 emulation
                  HCM_LA36 // DEC LA36 emulation
                ) ;

type TMain_Form = class( TForm )
                    Screen_Panel: TPanel;
                    StatusBar1: TStatusBar;
                    MainMenu1: TMainMenu;
                    File1: TMenuItem;
                    Exit1: TMenuItem;
                    Edit1: TMenuItem;
                    Copyscreen1: TMenuItem;
                    SerialPort1: TMenuItem;
                    Baudrate1: TMenuItem;
                    N751: TMenuItem;
                    N1101: TMenuItem;
                    N1501: TMenuItem;
                    N3001: TMenuItem;
                    N6001: TMenuItem;
                    N12001: TMenuItem;
                    N24001: TMenuItem;
                    N48001: TMenuItem;
                    N96001: TMenuItem;
                    N8databits1: TMenuItem;
                    N71: TMenuItem;
                    N81: TMenuItem;
                    N2stopbits1: TMenuItem;
                    Parity1: TMenuItem;
                    Video1: TMenuItem;
                    Options1: TMenuItem;
                    Keymapping1: TMenuItem;
                    LoadPaper: TMenuItem;
                    Flowcontrol1: TMenuItem;
                    None1: TMenuItem;
                    XOnXOff1: TMenuItem;
                    CTSRTS1: TMenuItem;
                    Connect1: TMenuItem;
                    Open_Dialog: TOpenDialog;
                    N2: TMenuItem;
                    Logtofile1: TMenuItem;
                    Sendfile1: TMenuItem;
                    Log_File_Dialog: TSaveDialog;
                    Cancellogging1: TMenuItem;
                    ChangeKeyboard1: TMenuItem;
                    Keyboard1: TMenuItem;
                    Auto1: TMenuItem;
                    ConnecttoEmulatorport1: TMenuItem;
                    N4: TMenuItem;
                    Disconnect1: TMenuItem;
                    Emulation1: TMenuItem;
                    TY1: TMenuItem;
                    VT051: TMenuItem;
                    Help1: TMenuItem;
                    About1: TMenuItem;
                    ScrollBox1: TScrollBox;
                    Image: TImage;
                    DECLA301: TMenuItem;
                    Uppercase1: TMenuItem;
                    Uppercaseinput1: TMenuItem;
                    N1: TMenuItem;
                    Settings1: TMenuItem;
                    Unloadpaper1: TMenuItem;
                    Pageup1: TMenuItem;
                    Pagedown1: TMenuItem;
                    Gotopage1: TMenuItem;
                    Local1: TMenuItem;
                    Online1: TMenuItem;
                    Gotoprinthead1: TMenuItem;
                    OpenPaperDialog: TOpenDialog;
                    HereIs1: TMenuItem;
                    KSR331: TMenuItem;
                    N3: TMenuItem;
                    RO331: TMenuItem;
                    Transmit1: TMenuItem;
                    Configure1: TMenuItem;
                    Panel1: TPanel;
                    Panel2: TPanel;
                    N5: TMenuItem;
                    N6: TMenuItem;
                    Loadpapertape1: TMenuItem;
                    Unloadpapertape1: TMenuItem;
                    Open_Tape_Dialog: TOpenDialog;
                    N7: TMenuItem;
                    AdvancepaperlocalLF1: TMenuItem;
                    opofformlocalFF1: TMenuItem;
                    AlternateCharacterSet: TMenuItem;
                    N8: TMenuItem;
                    AlternateCharacterSet1: TMenuItem;
                    N9: TMenuItem;
                    UseAlternateCharacterSet1: TMenuItem;
                    UseStandardCharacterSet1: TMenuItem;
                    AutoLF: TMenuItem;
                    LockCharacterSet1: TMenuItem;
                    SelectiveAddressing1: TMenuItem;
                    Enable1: TMenuItem;
                    Selected1: TMenuItem;
                    CompressedFont1: TMenuItem;
                    FormControl1: TMenuItem;
                    FormControlOption1: TMenuItem;
                    VerticalTabPositions1: TMenuItem;
                    AutoLF1: TMenuItem;
                    Receive1: TMenuItem;
                    HorizontalTabPositions1: TMenuItem;
                    SetCodes1: TMenuItem;
                    Master1: TMenuItem;
                    Newpaper1: TMenuItem;
                    procedure Newpaper1Click(Sender: TObject);
                    procedure Master1Click(Sender: TObject);
                    procedure Enable1Click(Sender: TObject);
                    procedure SetCodes1Click(Sender: TObject);
                    procedure HorizontalTabPositions1Click(Sender: TObject);
                    procedure VerticalTabPositions1Click(Sender: TObject);
                    procedure CompressedFont1Click(Sender: TObject);
                    procedure Selected1Click(Sender: TObject);
                    procedure AutoLFClick(Sender: TObject);
                    procedure LockCharacterSet1Click(Sender: TObject);
                    procedure UseStandardCharacterSet1Click(Sender: TObject);
                    procedure UseAlternateCharacterSet1Click(Sender: TObject);
                    procedure opofformlocalFF1Click(Sender: TObject);
                    procedure AdvancepaperlocalLF1Click(Sender: TObject);
                    procedure Unloadpapertape1Click(Sender: TObject);
                    procedure Loadpapertape1Click(Sender: TObject);
                    procedure Gotopage1Click(Sender: TObject);
                    procedure Pagedown1Click(Sender: TObject);
                    procedure Pageup1Click(Sender: TObject);
                    procedure Gotoprinthead1Click(Sender: TObject);
                    procedure Transmit1Click(Sender: TObject);
                    procedure KSR331Click(Sender: TObject);
                    procedure RO331Click(Sender: TObject);
                    procedure HereIs1Click(Sender: TObject);
                    procedure LoadPaperClick(Sender: TObject);
                    procedure Unloadpaper1Click(Sender: TObject);
                    procedure DECLA301Click(Sender: TObject);
                    procedure Check_Click(Sender: TObject);
                    procedure FormCreate(Sender: TObject);
                    procedure BufferKeyDown(Sender: TObject; var Key: Word;
                      Shift: TShiftState);
                    procedure BufferKeyUp(Sender: TObject; var Key: Word;
                      Shift: TShiftState);
                    procedure BufferKeyPress(Sender: TObject; var Key: Char);
                    procedure Exit1Click(Sender: TObject);
                    procedure Display1Click(Sender: TObject);
                    procedure Copyscreen1Click(Sender: TObject);
                    procedure Baud_Click(Sender: TObject);
                    procedure Bits_Check(Sender: TObject);
                    procedure Keymapping1Click(Sender: TObject);
                    procedure FormKeyDown(Sender: TObject; var Key: Word;
                      Shift: TShiftState);
                    procedure FormDestroy(Sender: TObject);
                    procedure Options2Click(Sender: TObject);
                    procedure FormClose(Sender: TObject; var Action: TCloseAction);
                    procedure Connect1Click(Sender: TObject);
                    procedure Logtofile1Click(Sender: TObject);
                    procedure Sendfile1Click(Sender: TObject);
                    procedure Cancellogging1Click(Sender: TObject);
                    procedure Uppercaseoutput1Click(Sender: TObject);
                    procedure Uppercaseinput1Click(Sender: TObject);
                    procedure ChangeKeyboard1Click(Sender: TObject);
                    procedure Keyboard1Click(Sender: TObject);
                    procedure ConnecttoEmulatorport1Click(Sender: TObject);
                    procedure Disconnect1Click(Sender: TObject);
                    procedure TY1Click(Sender: TObject);
                    procedure VT051Click(Sender: TObject);
                    procedure Screen_PanelResize(Sender: TObject);
                    procedure About1Click(Sender: TObject);
//                    procedure Configure1DrawItem(Sender: TObject; ACanvas: TCanvas;
//                      ARect: TRect; Selected: Boolean);

                  private // Instance data...
                      Alternate_KeyPad_Mode : boolean ;
                      _Key_Mapper : TCEF_Key_Mapper ;
                      Output_Queue : TInteger_List ;
                      Input_Queue : TInteger_List ;
                      In_Escape_Sequence : boolean ; // True if in LA36 escape sequence processing
                      Escape_Sequence : string ;
                      Graphics_Mode : boolean ;
                      Text_File : boolean ; // True if paper file is plain text, false if media file

                  protected // Property handlers...
                      function _Get_Key_Mapper : TCEF_Key_Mapper ;

                  protected // Callbacks...
                      procedure CB_Timer( Sender : TObject ) ;

                      procedure WMGetDlgCode( var Message : TMessage ) ;
                          message WM_GetDlgCode ;

                  private // Internal utility routines...
                      function Execute_Escape_Sequence : boolean ;
                      procedure Set_Alternate_KeyPad_Mode( Value : boolean ) ;
                      function Valid_Escape_Sequence : boolean ;
                      procedure Update_Status ;
                      procedure _Write( Value : longint ) ;
                      procedure Line_Feed ;
                      procedure Local_Line_Feed ;
                      procedure Next_Page ;
                      procedure Build_Page( Page : integer ; Update_Pos : boolean = False ) ;
                      procedure Replay_Paper( Show : boolean ) ; // Replay FPaper from current position
                      procedure Output_Char( V_Column, V_Row, Ch : integer ) ;
                      procedure Update_Settings_From_Header( Header : TCEF_Paper_Media_Header ) ;
                      procedure Write_cef32_Media_Code( Code : byte ; Value : longint ;
                          const S_Value : string ) ;

                  public // API...
                      _Scroll : boolean ; // True to scroll screen when cursor goes off bottom
                      Baud : longint ;
                      Block_New_Chars : boolean ;
                      Print_Head_Row : integer ; // Pixel
                      Print_Head_Column : integer ; // Pixel
                      Print_Head_Page : integer ;
                      Print_Head_Char_Column : integer ; // Character
                      Print_Head_Char_Line : integer ; // Character
                      Data_Bits : integer ;
                      KB_Receiver : TComponent ;
                      _Line_Wrap : boolean ;
                      Log_File : file ;
                      Timer : TTimer ;
                      Transmit_Paused : boolean ;
                      _Uppercase_Out : boolean ; // True if to transmit only uppercase alphas
                      _Uppercase_In : boolean ; // True if to display only uppercase alphas
                      _UI : TUI_Interface ;
                      _LA36 : TComponent ;
                      _Margin_Bell : integer ; // Position of margin bell (0=none)
                      Mode : THC_Mode ;
                      Keyboard : TComponent ;
                      Cable_Component : TComponent ;
                      Threaded : boolean ;
                      Caps_Lock : boolean ; // True if caps-lock is down
                      Shifted : boolean ; // True if shift is down
                      Max_Width : integer ; // Max width in characters, regardless of paper width
                      Paper_File : string ; // Name of paper file
                      Tape_File : string ; // Name of paper tape file
                      FPaper, FTape : file ;
                      Character_Set, Standard_Character_Set, Alt_Character_Set : TCEF_Character_Set ;
                      Margin : integer ; // Pixel margin on paper display
                      Paper_Height, Paper_Width : integer ; // Paper size in tenths of inches (eg 85 = 8.5")
                      LPI, CPI : integer ; // Lines per inch, characters per inch
                      Hide_Printing : boolean ; // True if not to show printing
                      Hard_Tabs : integer ; // Hard HT position interval (in tenths of inches)
                      Here_Is : string ; // Code for here-is and enquiry
                      Here_Is_Enabled : boolean ; // True if here-is is enabled
                      Alt_Current_Font, Current_Font : string ;
                      Displayed_Page : integer ;
                      Tape_Running : boolean ; // True if a tape is running
                      Bleed : integer ; // Ink bleed factor
                      Auto_LF : boolean ; // True if auto LF
                      Char_Set_Lock : boolean ; // True if char set lock
                      Horizontal_Tabs : TInteger_List ;
                      Vertical_Tabs : TInteger_List ;
                      Address_Mode : boolean ; // True if in address mode
                      Transmit_Disabled : boolean ; // True if cannot transmit due to device select
                      Unselect_Locked : boolean ; // True if locked into unselected mode
                      Waiting_For_Lock : boolean ;
                      Master : boolean ; // True if master terminal
                      Max_Page : integer ;
                      Barred : boolean ;
                      Bar_Color : TColor ;
                      Unit_Code, Group_Code : integer ;
                      Magnification : integer ;

                      function Check_Flow_Control : boolean ; // Return true if can send data
                      procedure Pause_Input( Value : boolean ) ; // True to request host to stop sending data.  False to request host to send data.
                      procedure Flush_Queue ;
                      procedure Load_Character_Set( const Text : string ) ;
                      procedure Load_Alt_Character_Set( const Text : string ) ;
                      function Load_Keyboard( Name : string ; Silent : boolean ) : boolean ;
                      procedure New_Char( Value : integer ) ;
                      procedure Print_Char( Value : integer ) ;
                      procedure Receive_Char( Value : longint ) ;
                      procedure Set_Mode( _Mode : THC_Mode ) ;
                      procedure Write_Serial( Value : integer ) ;
                      function Printer_Options : TPrinter_Options_Dialog ;
                      procedure New_Page ;
                      procedure Bring_Print_Head_Into_View ;
                      procedure Start_Tape ;
                      procedure Stop_Tape ;
                      function Cancel_Timer : boolean ;
                      procedure Set_Alt_Character_Set ;
                      procedure Set_Standard_Character_Set ;
                      procedure Select_Device( Selected : boolean ) ;
                      procedure Load_Paper( Filename : string ) ;
                      procedure Load_Paper_Tape( Filename : string ) ;
                      function Paper_Out : boolean ;
                      function New_Paper( Filename : string ) : integer ;

                      property Key_Mapper : TCEF_Key_Mapper
                          read _Get_Key_Mapper
                          write _Key_Mapper ;

                      // Special behaviors...
                      procedure Correct_For_Cursor( Old_Column : integer ) ;
                      procedure Transmit_Here_Is ;
                  end ; // TMain_Form

const LA36_Thread_Name : string = 'LA36' ;

var Pending_UI : TUI_Interface = nil ;


implementation

uses // Borland...
     Clipbrd, // Clipboard

     // C&C...
     _ASCII, // ESC
     Standard, // ERT
     _UE, // TUnified_Exception
     UStrings, // Extract
     _OS, // ADPF_Create
     O_S, // OS
     UE, // DOS_ERT

     // CEF...
     Choose_Emulator_Port_Dlg, // Choose_Emulator_Port_Form
     
     // LA36...
     AboutBox, // About_Form
     CEF, // TBase_Component
     Goto_Page_Form, // Goto_Page_Dialog
     LA_36, // _UI
     Send,
     Here_Is_Dialog,
     VT_Dialog,
     Selective_Addressing_Form;

{$R *.dfm}

function Get_Character_Set : TCEF_Character_Set ; stdcall ; external 'CEF_Util.dll' ;
function Get_Key_Mapper : TCEF_Key_Mapper ; stdcall ; external 'CEF_Util.dll' ;



// TKBComponent methods...

type TKBComponent = class( TBase_Component )
                        public
                            Main_Form : TMain_Form ;

                            function Write( Address : int64 ;
                                Value, Size : longint ;
                                IO_Type : longint ) : TUnified_Exception ; override ;

                            procedure Process_Key( S : string ; Value : integer ) ;
                    end ;

procedure TKBComponent.Process_Key( S : string ; Value : integer ) ;

    procedure Send_Escape_Sequence( const S : string ) ;

    var Loop : integer ;

    begin
        Main_Form.Write_Serial( ord( ESC ) ) ;
        for Loop := 1 to length( S ) do
        begin
            Main_Form.Write_Serial( ord( S[ Loop ] ) ) ;
        end ;
    end ;

var Dummy : integer ;

begin
    if( Value = 0 ) then // Key up
    begin
        if( copy( S, 1, 5 ) = 'LEFT_' ) then
        begin
            S := copy( S, 6, length( S ) ) ;
        end ;
        if( copy( S, 1, 6 ) = 'RIGHT_' ) then
        begin
            S := copy( S, 7, length( S ) ) ;
        end ;
        if( S = 'SHIFT' ) then
        begin
            Main_Form.Shifted := False ;
        end else
        if( S = 'CAPS LOCK' ) then
        begin
            Main_Form.Caps_Lock := False ;
        end else
        if( S = 'AUTO LF' ) then
        begin
            Main_Form.Auto_LF := False ;
            Main_Form.AutoLF1.Checked := False ;
        end else
        if( S = 'CHAR SET LOCK' ) then
        begin
            Main_Form.Char_Set_Lock := False ;
            Main_Form.LockCharacterSet1.Checked := False ;
        end ;
    end else
    begin
        if( copy( S, 1, 5 ) = 'LEFT_' ) then
        begin
            S := copy( S, 6, length( S ) ) ;
        end ;
        if( copy( S, 1, 6 ) = 'RIGHT_' ) then
        begin
            S := copy( S, 7, length( S ) ) ;
        end ;
        if( copy( S, 1, 4 ) = 'NKP_' ) then
        begin
            S := copy( S, 5, length( S ) ) ;
        end ;

        if( length( S ) = 1 ) then
        begin
            if( Main_Form.Caps_Lock ) then
            begin
                S[ 1 ] := upcase( S[ 1 ] ) ;
            end ;
            Main_Form.Write_Serial( ord( S[ 1 ] ) ) ;
        end else
        if( S = 'SHIFT' ) then
        begin
            Main_Form.Shifted := True ;
        end else
        if( S = 'CAPS LOCK' ) then
        begin
            Main_Form.Caps_Lock := True ;
        end else
        if( ( S = 'ENTER' ) or ( S = 'RETURN' ) ) then
        begin
            Main_Form.Write_Serial( ord( CR ) ) ;
        end else
        if( S = 'BREAK' ) then
        begin
            Set_Signal( 'BREAK', True ) ;
        end else
        if( S = 'ALT CHAR SET' ) then
        begin
            Main_Form.Set_Alt_Character_Set ;
        end else
        if( S = 'STD CHAR SET' ) then
        begin
            Main_Form.Set_Standard_Character_Set ;
        end else
        if( S = 'HERE IS' ) then
        begin
            Main_Form.Transmit_Here_Is ;
        end else
        if( S = 'AUTO LF' ) then
        begin
            Main_Form.Auto_LF := True ;
            Main_Form.AutoLF1.Checked := True ;
        end else
        if( S = 'CHAR SET LOCK' ) then
        begin
            Main_Form.Char_Set_Lock := True ;
            Main_Form.LockCharacterSet1.Checked := True ;
        end else
        if( S = 'LINE' ) then
        begin
            Main_Form.Online1.Checked := True ;
            Main_Form.Update_Status ;
        end else
        if( S = 'LOCAL' ) then
        begin
            Main_Form.Online1.Checked := False ;
            Main_Form.Update_Status ;
        end else
        if( S = 'LOCAL LINE FEED' ) then
        begin
            Main_Form.Local_Line_Feed ;
            Main_Form.Update_Status ;
        end else
        if( copy( S, 1, 10 ) = 'BAUD RATE ' ) then
        begin
            S := copy( S, 11, length( S ) ) ;
            for Dummy := 0 to Main_Form.BaudRate1.Count - 1 do
            begin
                if( S = Extract( Main_Form.BaudRate1.Items[ Dummy ].Caption, '&', 1, -1 ) ) then
                begin
                    Main_Form.BaudRate1.Items[ Dummy ].Checked := True ;
                end ;
            end ;
        end ;
        // Ignore all other keys
    end ;
end ; // TKBComponent.Process_Key


function TKBComponent.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var P : PChar ;
    S : string ;

begin
    P := Main_Form.Keyboard.Keyboard.Get_Key ;
    if( P <> nil ) then
    begin
        if( Value = 0 ) then // Key up
        begin
            Process_Key( string( P ), Value ) ;
        end else
        begin
            while( P <> nil ) do
            begin
                S := string( P ) ;
                Process_Key( S, Value ) ;
                P := Main_Form.Keyboard.Keyboard.Get_Key ;
            end ; // while( P <> nil )
        end ; // if( Value = 1 )
    end ;
end ; // TKBComponent.Write


// Unit utility functions...

function Choose_Emulator_Port( UI : TUI_Interface ) : TComponent ;

begin
    Result := nil ;
    Choose_Emulator_Port_Form := TChoose_Emulator_Port_Form.Create( Application ) ;
    try
        Choose_Emulator_Port_Form.UI := UI ;
        if( Choose_Emulator_Port_Form.ShowModal = mrOK ) then
        begin
            Result := Choose_Emulator_Port_Form.Cable ;
        end ;
    finally
        Choose_Emulator_Port_Form.Free ;
        Choose_Emulator_Port_Form := nil ;
    end ;
end ;



// TMain_Form methods...

procedure TMain_Form.Unloadpaper1Click(Sender: TObject);

begin
    if( Paper_File <> '' ) then
    begin
        closefile( FPaper ) ;
        Paper_File := '' ;
    end ;
    Displayed_Page := 0 ;
    Update_Status ;
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', True ) ;
    end ;
end ;


procedure TMain_Form.Unloadpapertape1Click(Sender: TObject);

begin
    if( Tape_File <> '' ) then
    begin
        closefile( FTape ) ;
        Tape_File := '' ;
        Unloadpapertape1.Enabled := False ;
    end ;
end ;


procedure TMain_Form.Update_Status ;

var S : string ;

begin
    case Mode of
        HCM_LA30 : S := 'LA30' ;
        HCM_LA36 : S := 'LA36' ;
        HCM_RO33 : S := 'RO33' ;
        HCM_KSR33 : S := 'KSR33' ;
        else S := 'ASR33' ;
    end ;
    StatusBar1.Panels[ 0 ].Text := S ;
    if( Online1.Checked ) then
    begin
        StatusBar1.Panels[ 1 ].Text := 'Online' ;
    end else
    begin
        StatusBar1.Panels[ 1 ].Text := 'Local' ;
    end ;

    StatusBar1.Panels[ 2 ].Text := 'Row ' + inttostr( Print_Head_Char_Line ) ;
    StatusBar1.Panels[ 3 ].Text := 'Col ' + inttostr( Print_Head_Char_Column ) ;
    StatusBar1.Panels[ 4 ].Text := 'Page ' + inttostr( Displayed_Page + 1 ) ;
    if( Paper_Out ) then
    begin
        StatusBar1.Panels[ 5 ].Text := 'Out of paper' ;
    end else
    begin
        StatusBar1.Panels[ 5 ].Text := '' ;
    end ;
end ; // TMain_Form.Update_Status


procedure TMain_Form._Write( Value : longint ) ;

var Stops : integer ;

begin
    if( N2stopbits1.Checked ) then
    begin
        Stops := 2 ;
    end else
    begin
        Stops := 1 ;
    end ;
    _LA36.Cable.Transmit( Baud, Value, Data_Bits, Stops ) ;
end ;


procedure TMain_Form.Line_Feed ;

begin
    if( not Paper_Out ) then
    begin
        Print_Char( ord( LF ) ) ;
    end ;
end ;


procedure TMain_Form.Next_Page ;

begin
    Print_Head_Row := Margin ;
    Print_Head_Char_Line := 1 ;
    if( Displayed_Page = Print_Head_Page ) then
    begin
        inc( Displayed_Page ) ;
    end ;
    Write_cef32_Media_Code( MCC_Advance_Page, 0, '' ) ;
    inc( Print_Head_Page ) ;
    New_Page ;
    Scrollbox1.HorzScrollBar.Position := 0 ;
    Scrollbox1.VertScrollBar.Position := 0 ;
end ;


procedure TMain_Form.opofformlocalFF1Click(Sender: TObject) ;

begin
    if( not Paper_Out ) then
    begin
        inc( Print_Head_Page ) ;
        Displayed_Page := Print_Head_Page ;
        Print_Head_Row := Margin ;
        Print_Head_Char_Line := 1 ;
        Build_Page( -1 ) ;
        Update_Status ;
    end ;
end ;


function TMain_Form.Printer_Options : TPrinter_Options_Dialog ;

begin
    if( Printer_Options_Dialog = nil ) then
    begin
        Printer_Options_Dialog := TPrinter_Options_Dialog.Create( Application ) ;
        Printer_Options_Dialog.Current_Font := Current_Font ;
        Printer_Options_Dialog.Alt_Current_Font := Alt_Current_Font ;
    end ;
    Result := Printer_Options_Dialog ;
end ;


procedure TMain_Form.Start_Tape ;

begin
    if( not Tape_Running ) then
    begin
        if( Tape_File <> '' ) then
        begin
            Tape_Running := True ;
            Timer.Interval := 100 ;
            Timer.Enabled := True ;
        end ; // if( Tape_File <> '' )
    end ; // if( not Tape_Running )
end ; // TMain_Form.Start_Tape


procedure TMain_Form.Stop_Tape ;

begin
    Tape_Running := False ;
    Timer.Interval := 1000 ;
    Cancel_Timer ;
end ;


function TMain_Form.Cancel_Timer : boolean ;

begin
    Result := ( Keyboard <> nil ) and ( Keyboard.User_Interface.Parent_Window <> 0 ) and ( not Tape_Running ) ;
    Timer.Enabled := not Result ;
end ;


procedure TMain_Form.Set_Alt_Character_Set ;

begin
    if( Mode = HCM_LA36 ) then
    begin
        if( ( AlternateCharacterSet.Enabled ) and ( Alt_Character_Set <> nil ) ) then
        begin
            Character_Set := Alt_Character_Set ;
            AlternateCharacterSet1.Checked := True ;
            if( Keyboard <> nil ) then
            begin
                Keyboard.Keyboard.Set_LED_State( 'ALT CHAR SET', True ) ;
                Keyboard.Keyboard.Set_LED_State( 'STD CHAR SET', False ) ;
                Keyboard.Keyboard.Set_Key_Down( 'ALT CHAR SET', True ) ;
            end ;
        end ;
    end ;
end ;


procedure TMain_Form.Set_Standard_Character_Set ;

begin
    if( Mode = HCM_LA36 ) then
    begin
        Character_Set := Standard_Character_Set ;
        AlternateCharacterSet1.Checked := False ;
        if( Keyboard <> nil ) then
        begin
            Keyboard.Keyboard.Set_LED_State( 'ALT CHAR SET', False ) ;
            Keyboard.Keyboard.Set_LED_State( 'STD CHAR SET', True ) ;
            Keyboard.Keyboard.Set_Key_Down( 'ALT CHAR SET', False ) ;
            Keyboard.Keyboard.Set_Key_Down( 'STD CHAR SET', True ) ;
        end ;
    end ;
end ;


procedure TMain_Form.Bring_Print_Head_Into_View ;

begin
    if( Scrollbox1.VertScrollBar.ScrollPos + Scrollbox1.Height <= Print_Head_Row + Character_Set.Height ) then
    begin
        ScrollBox1.VertScrollBar.Position := Print_Head_Row + Character_Set.Height - Scrollbox1.Height ;
    end else
    if( Scrollbox1.VertScrollBar.ScrollPos > Print_Head_Row ) then
    begin
        ScrollBox1.VertScrollBar.Position := Print_Head_Row ;
    end ;
end ;


procedure TMain_Form.New_Page ;

var Bar_Height, Image_Height : integer ;
    Rect : TRect ;

begin
    Image_Height := Image.Picture.Bitmap.Height ;
    Rect.Left := 0 ;
    Rect.Top := 0 ;
    Rect.Right := Image.Picture.Bitmap.Width ;
    Rect.Bottom := Image_Height ;
    Image.Picture.Bitmap.Canvas.Brush.Color := clWhite ;
    Image.Picture.Bitmap.Canvas.FillRect( Rect ) ;
    Image.Picture.Bitmap.Canvas.Pen.Color := clBlack ;
    if( Barred ) then
    begin
        // Bars ar .5" high and .5" apart...
        if( ( Paper_Height div 5 ) = 0 ) then
        begin
            Bar_Height := 1 ;
        end else
        begin
            Bar_Height := Image_Height div ( Paper_Height div 5 ) ;
            if( Bar_Height < 1 ) then
            begin
                Bar_Height := 1 ;
            end ;
        end ;
        Image.Picture.Bitmap.Canvas.Brush.Color := Bar_Color ;
        Rect.Top := Bar_Height + Margin ;
        Rect.Left := Margin ;
        Rect.Right := Rect.Right - Margin ;
        while( Rect.Top < Image_Height ) do
        begin
            Rect.Bottom := Rect.Top + Bar_Height ;
            Image.Picture.Bitmap.Canvas.FillRect( Rect ) ;
            Rect.Top := Rect.Bottom + Bar_Height ;
        end ;
        Image.Picture.Bitmap.Canvas.Brush.Color := clWhite ;
    end ;
end ;


procedure TMain_Form.Output_Char( V_Column, V_Row, Ch : integer ) ;

var B : TBitmap ;
    Bleeding : integer ;
    Distance : integer ; // X and/or Y Distance from original pixel location
    C, R : integer ; // Source column & row
    DC, DR : integer ; // Destination column & row
    P, DP : TColor ;
    VX, VY : integer ; // X and Y vectors
    OX, OY : integer ; // Original X, Y for this pass

begin
    B := TBitmap.Create ;
    try
        B.Height := Character_Set.Height ;
        B.Width := Character_Set.Width( Ch ) ;
        Character_Set.DrawEx( B.Canvas.Handle, 0, 0, Ch, 1 ) ;
        for C := 0 to B.Width - 1 do
        begin
            for R := 0 to B.Height - 1 do
            begin
                P := B.Canvas.Pixels[ C, R ] ;
                if( P <> clWhite ) then // Need to draw this pixel
                begin
                    DC := V_Column + C ;
                    DR := V_Row + R ;
                    Distance := 0 ;
                    OX := DC ;
                    OY := DR ;
                    VX := 0 ;
                    VY := 0 ;
                    Bleeding := Bleed ;
                    while( Bleeding > 0 ) do
                    begin
                        if( Distance > Bleed ) then
                        begin
                            break ; // Limit bleeding
                        end ;
                        DP := Image.Canvas.Pixels[ DC, DR ] ;
                        if( DP <> clBlack ) then
                        begin
                            if( DP = clWhite ) then
                            begin
                                Image.Canvas.Pixels[ DC, DR ] := P ;
                            end else
                            begin
                                Image.Canvas.Pixels[ DC, DR ] := clBlack ;
                            end ;
                            dec( Bleeding ) ;
                            continue ;
                        end ;
                        DC := DC + VX ;
                        DR := DR + VY ;
                        if( ( DC >= OX ) and ( DR = OY ) ) then // Circled around back to starting point
                        begin
                            inc( Distance ) ;
                            VY := -1 ;
                            VX := 0 ;
                            inc( DC ) ;
                        end else
                        if( VY < 0 ) then // Going up
                        begin
                            if( DR < OY - Distance ) then
                            begin
                                VY := 0 ;
                                VX := -1 ;
                                inc( DR ) ;
                                dec( DC ) ;
                            end ;
                        end else
                        if( VX < 0 ) then // Going left
                        begin
                            if( DC < OX - Distance ) then
                            begin
                                VY := 1 ;
                                VX := 0 ;
                                inc( DC ) ;
                                inc( DR ) ;
                            end ;
                        end else
                        if( VY > 0 ) then // Going down
                        begin
                            if( DR > OY + Distance ) then
                            begin
                                VY := 0 ;
                                VX := 1 ;
                                dec( DR ) ;
                                inc( DC ) ;
                            end ;
                        end else // Going right
                        begin
                            if( DC > OX + Distance ) then
                            begin
                                VY := -1 ;
                                VX := 0 ;
                                dec( DC ) ;
                                dec( DR ) ;
                                if( ( DC >= OX ) and ( DR = OY ) ) then // Circled around back to starting point
                                begin
                                    inc( Distance ) ;
                                    VY := -1 ;
                                    VX := 0 ;
                                    inc( DC ) ;
                                end ;
                            end ;
                        end ;
                    end ; // while( True ) do
                end ;
            end ;
        end ; // for C := 0 to B.Width - 1
        Character_Set.DrawEx( Image.Canvas.Handle, V_Column, V_Row, Ch, 1 ) ;
    finally
        B.Free ;
    end ;
end ; // TMain_Form.Output_Char


procedure TMain_Form.Build_Page( Page : integer ; Update_Pos : boolean = False ) ;

var Image_Height, Image_Width : integer ;
    C : char ;
    Count : integer ;
    Header : TCEF_Paper_Media_Header ;
    Tab_Pixels : integer ;
    V_Page, V_Row, V_Column : integer ; // Virtual position of print head
    V_Char_Column, V_Char_Line : integer ;
    W, X : integer ;

begin
    // Setup...
    if( Page < 0 ) then
    begin
        Page := Displayed_Page ;
    end ;
    Image_Height := Image.Picture.Bitmap.Height ;
    Image_Width := Image.Picture.Bitmap.Width ;

    // Draw paper...
    New_Page ;
    if( ( Character_Set = nil ) or Paper_Out ) then
    begin
        exit ;
    end ;

    // Find and draw the printout...
    V_Page := 0 ;
    V_Row := Margin ;
    V_Column := Margin ;
    V_Char_Column := 1 ;
    V_Char_Line := 1 ;
    reset( FPaper, 1 ) ;
    if( not Text_File ) then
    begin
        blockread( FPaper, Header, sizeof( Header ) ) ;
    end ;
    while( not eof( FPaper ) ) do
    begin
        blockread( FPaper, C, 1, Count ) ;
        if( Count = 0 ) then
        begin
            break ;
        end ;
        if( Page = V_Page ) then // On the right page
        begin
            Output_Char( V_Column, V_Row, ord( C ) ) ;
        end ;
        if( C = LF ) then
        begin
            V_Row := V_Row + Character_Set.Height ;
            inc( V_Char_Line ) ;
            if( V_Row >= Image_Height - Margin ) then // Move to next page
            begin
                if( Page = V_Page ) then // On the right page
                begin
                    V_Row := Margin ;
                    V_Char_Line := 1 ;
                    V_Column := Margin ;
                    V_Char_Column := 1 ;
                    break ; // Done.  NOTE: When we support reverse linefeed, we will need to rewrite this
                end ;
                inc( V_Page ) ;
                V_Row := Margin ;
                V_Char_Line := 1 ;
            end ;
        end else
        if( C = FF ) then
        begin
            if( Mode = HCM_LA36 ) then
            begin
                if( Page = V_Page ) then // On the right page
                begin
                    V_Row := Margin ;
                    V_Char_Line := 1 ;
                    V_Column := Margin ;
                    V_Char_Column := 1 ;
                    break ; // Done.  NOTE: When we support reverse linefeed, we will need to rewrite this
                end ;
                inc( V_Page ) ;
                V_Row := Margin ;
                V_Char_Line := 1 ;
            end ;
        end else
        if( C = CR ) then
        begin
            V_Column := Margin ;
            V_Char_Column := 1 ;
        end else
        if( C = HT ) then
        begin
            Tab_Pixels := Paper_Width div Hard_Tabs ; // Number of tab columns on paper
            Tab_Pixels := Image.Picture.Bitmap.Width div Tab_Pixels ; // Number of pixels per tab
            if( Tab_Pixels = 0 ) then
            begin
                Tab_Pixels := 1 ;
            end ;
            W := Tab_Pixels ;
            while( W < V_Column ) do
            begin
                W := W + Tab_Pixels ;
            end ;
            X := Character_Set.Width( 32 ) ;
            while( V_Column < W ) do
            begin
                V_Column := V_Column + X ;
                inc( V_Char_Column ) ;
            end ;
        end else
        if( C = BS ) then
        begin
            V_Column := V_Column - Character_Set.Width( 32 ) ;
            if( V_Column < Margin ) then
            begin
                V_Column := Margin ;
            end ;
            dec( V_Char_Column ) ;
            if( V_Char_Column < 1 ) then
            begin
                V_Char_Column := 1 ;
            end ;
        end else
        begin
            W := Character_Set.Width( ord( C ) ) ;
            V_Column := V_Column + W ;
            inc( V_Char_Column ) ;
        end ;
        if( V_Column > Image_Width - Margin * 2 ) then
        begin
            V_Column := Image_Width - Margin * 2 ;
        end ;
        if( V_Char_Column > Max_Width ) then
        begin
            V_Char_Column := Max_Width ;
        end ;
    end ; // while( not eof( FPaper ) )
    if( Update_Pos ) then
    begin
        Print_Head_Row := V_Row ;
        Print_Head_Char_Line := V_Char_Line ;
        Print_Head_Column := V_Column ;
        Print_Head_Char_Column := V_Char_Column ;
    end ;
end ; // TMain_Form.Build_Page


procedure TMain_Form.Replay_Paper( Show : boolean ) ;

var C : char ;
    I : longint ;
    Res : integer ;
    S : string ;
    Saved : boolean ;
    V : byte ;

begin
    Saved := Hide_Printing ;
    try
        Hide_Printing := not Show ;
        Displayed_Page := -1 ; // Allow processing of characters on all pages
        Print_Head_Page := 0 ;
        Print_Head_Row := Margin ;
        Print_Head_Column := Margin ;
        Print_Head_Char_Line := 1 ;
        Print_Head_Char_Column := 1 ;
        while( not eof( FPaper ) ) do
        begin
            blockread( FPaper, C, 1, Res ) ;
            if( Res = 0 ) then // Nothing more in file
            begin
                break ;
            end ;
            V := ord( C ) ;
            if( Text_File or ( V <> 0 ) ) then
            begin
                Print_Char( V ) ;
            end else
            begin
                blockread( FPaper, V, 1, Res ) ;
                if( Res = 0 ) then // Nothing more in file
                begin
                    break ;
                end ;
                case V of
                    0 : Print_Char( V ) ; // Escaped null
                    1 : if( Vertical_Tabs <> nil ) then
                        begin
                            Vertical_Tabs.Clear ;
                        end ;
                    2 : begin
                            blockread( FPaper, I, 4, Res ) ;
                            if( Res <> 4 ) then
                            begin
                                break ;
                            end ;
                            if( Vertical_Tabs <> nil ) then
                            begin
                                Vertical_Tabs := TInteger_List.Create ;
                            end ;
                            Vertical_Tabs.Add( I ) ;
                            Vertical_Tabs.Sort( -1 ) ;
                        end ;
                    3 : if( Horizontal_Tabs <> nil ) then
                        begin
                            Horizontal_Tabs.Clear ;
                        end ;
                    4 : begin
                            blockread( FPaper, I, 4, Res ) ;
                            if( Res <> 4 ) then
                            begin
                                break ;
                            end ;
                            if( Horizontal_Tabs <> nil ) then
                            begin
                                Horizontal_Tabs := TInteger_List.Create ;
                            end ;
                            Horizontal_Tabs.Add( I ) ;
                            Horizontal_Tabs.Sort( -1 ) ;
                        end ;
                    6 : begin
                            blockread( FPaper, I, 4, Res ) ;
                            if( Res <> 4 ) then
                            begin
                                break ;
                            end ;
                            LPI := I ;
                        end ;
                    7 : begin
                            blockread( FPaper, I, 4, Res ) ;
                            if( Res <> 4 ) then
                            begin
                                break ;
                            end ;
                            CPI := I ;
                        end ;
                    8 : begin
                            S := '' ;
                            V := 1 ;
                            while( V <> 0 ) do
                            begin
                                blockread( FPaper, V, 1, Res ) ;
                                if( Res = 0 ) then
                                begin
                                    break ;
                                end ;
                                if( V = 0 ) then // End of name
                                begin
                                    break ;
                                end ;
                                S := S + chr( V ) ;
                            end ;
                            if( Res = 0 ) then
                            begin
                                break ;
                            end ;
                            Load_Character_Set( S ) ;
                        end ;
                    9 : Local_Line_Feed ;
                    10 : Next_Page ;
                end ;
            end ;
        end ;
        Displayed_Page := Print_Head_Page ;
    finally
        Hide_Printing := Saved ;
    end ;
end ;


procedure TMain_Form.RO331Click(Sender: TObject) ;

begin
    Set_Mode( HCM_RO33 ) ;
    RO331.Checked := True ;
end ;


procedure TMain_Form.Update_Settings_From_Header( Header : TCEF_Paper_Media_Header ) ;

begin
    if( Header.ID = 65535 ) then // Valid header
    begin
        Printer_Options.Width.Value := Header.Width div 10 ;
        Printer_Options.Width_Modification.Value := Header.Width - ( Printer_Options.Width.Value * 10 ) ;
        Printer_Options.Height.Value := Header.Height div 10 ;
        Printer_Options.Height_Modification.Value := Header.Height - ( Printer_Options.Height.Value * 10 ) ;
        Printer_Options.Barred_RB.Checked := Header.Typ <> 0 ;
        if( Header.Typ <> 0 ) then
        begin
            Printer_Options.Color := Header.Typ ;
        end ;
        Printer_Options.Max_Pages.Value := Header.Max_Page ;
        Max_Page := Header.Max_Page ;
        Bleed := Header.Bleed ;
    end ;
    Paper_Width := Printer_Options.Width.Value * 10 + Printer_Options_Dialog.Width_Modification.Value ;
    if( Paper_Width < 10 ) then // Minimum paper size = 1" x 1"
    begin
        Paper_Width := 10 ;
    end ;
    Paper_Height := Printer_Options.Height.Value * 10 + Printer_Options_Dialog.Height_Modification.Value ;
    if( Paper_Height < 10 ) then
    begin
        Paper_Height := 10 ;
    end ;
    Margin := Printer_Options.Margin.Value ;
    Image.Picture.Bitmap.Height := Margin * 2 + ( LPI * Paper_Height div 10 ) * Character_Set.Height ;
    Image.Picture.Bitmap.Width := Margin * 2 + ( CPI * Paper_Width div 10 ) * Character_Set.Width( ord( 'W' ) ) ;
    Image.Height := Image.Picture.Bitmap.Height * Magnification ;
    Image.Width := Image.Picture.Bitmap.Width * Magnification ;
end ; // TMain_Form.Update_Settings_From_Header


procedure TMain_Form.Load_Paper( Filename : string ) ;

var Dummy : integer ;
    Header : TCEF_Paper_Media_Header ;

begin
    // Close existing paper file
    if( Paper_File <> '' ) then
    begin
        closefile( FPaper ) ;
    end ;

    // Open the paper file...
    assignfile( FPaper, Filename ) ;
    if( FileExists( Filename ) ) then
    begin
        {$I-}
        reset( FPaper, 1 ) ;
        {$I+}
        Dummy := IOResult ;
    end else
    begin
        Dummy := New_Paper( Filename ) ;
    end ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( DOS_ERT( Dummy ) ) ;
        assignfile( FPaper, Filename ) ;
        {$I-}
        reset( FPaper, 1 ) ;
        {$I+}
        if( IOResult <> 0 ) then
        begin
            Paper_File := '' ;
            if( Keyboard <> nil ) then
            begin
                Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', True ) ;
            end ;
            exit ;
        end ;
    end ;

    // Check for/process media header...
    fillchar( Header, sizeof( Header ), 0 ) ;
    if( filesize( FPaper ) >= sizeof( Header ) ) then
    begin
        {$I-}
        seek( FPaper, 0 ) ;
        blockread( FPaper, Header, sizeof( Header ) ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( DOS_ERT( Dummy ) ) ;
            Paper_File := '' ;
            if( Keyboard <> nil ) then
            begin
                Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', True ) ;
            end ;
            exit ;
        end ;
    end ;
    Text_File := Header.Prefix <> 65535 ;
    if( Text_File ) then
    begin
        seek( FPaper, 0 ) ;
    end else
    begin
        if( ( Header.ID <> 255 ) or ( Header.Facility <> 125 ) or ( Header.Version <> 10 ) ) then
        begin
            Header.Prefix := 0 ;
            Text_File := True ; // Not a CEF media file - assume text
            {$I-}
            reset( FPaper, 1 ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                Paper_File := '' ;
                if( Keyboard <> nil ) then
                begin
                    Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', True ) ;
                end ;
                exit ;
            end ;
        end else
        begin
            Max_Page := Header.Max_Page ;
            if( Header.Typ = 0 ) then
            begin
                Barred := False ;
            end else
            begin
                Bar_Color := Header.Typ ;
            end ;
            Paper_Height := Header.Height ;
            Paper_Width := Header.Width ;
            Bleed := Header.Bleed ;
        end ;
        if( Horizontal_Tabs <> nil ) then
        begin
            Horizontal_Tabs.Clear ;
        end ;
        if( Vertical_Tabs <> nil ) then
        begin
            Vertical_Tabs.Clear ;
        end ;
    end ; // if( not Text_File )
    Paper_File := Filename ;
    Update_Settings_From_Header( Header ) ;

    // Replay paper...
    StatusBar1.Panels[ 5 ].Text := 'Loading paper...' ;
    Application.ProcessMessages ;
    Replay_Paper( False ) ;
    Build_Page( -1, True ) ;
    Update_Status ;
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', False ) ;
    end ;

    // Release any pending characters...
    if( Input_Queue.Count > 0 ) then
    begin
        // Process items in input queue
        while( Input_Queue.Count > 0 ) do
        begin
            Print_Char( Input_Queue[ 0 ] ) ;
            Input_Queue.Delete( 0 ) ;
        end ;
    end ;
end ; //  TMain_Form.Load_Paper


procedure TMain_Form.LoadPaperClick(Sender: TObject) ;

begin
    OpenPaperDialog.Initialdir := OS^.Application_Data_Path( 'CEF32', 'Media', ADPF_Create ) ;
    if( OpenPaperDialog.Execute ) then
    begin
        Load_Paper( OpenPaperDialog.Filename ) ;
    end ; // if( OpenPaperDialog.Execute )
end ; // TMain_Form.LoadPaperClick


procedure TMain_Form.Load_Paper_Tape( Filename : string ) ;

var Dummy : integer ;

begin
    if( Tape_File <> '' ) then
    begin
        closefile( FTape ) ;
        Tape_File := '' ;
        Unloadpapertape1.Enabled := False ;
    end ;
    assignfile( FTape, Filename ) ;
    {$I-}
    reset( FTape, 1 ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( ERT( Dummy ) + ' while opening file.' ) ;
        exit ;
    end ;
    Tape_File := Filename ;
    Unloadpapertape1.Enabled := True ;
end ;


procedure TMain_Form.Loadpapertape1Click(Sender: TObject) ;

begin
    if( Open_Tape_Dialog.Execute ) then
    begin
        Load_Paper_Tape( Open_Tape_Dialog.Filename ) ;
    end ;
end ;


function TMain_Form.Paper_Out : boolean ;

begin
    Result := ( Paper_File = '' ) or ( ( Max_Page > 0 ) and ( Print_Head_Page >= Max_Page ) ) ;
end ;


procedure TMain_Form.Load_Character_Set( const Text : string ) ;

begin
    if( Text <> Current_Font ) then
    begin
        Standard_Character_Set.Load( Pchar( Text ) ) ;
        Standard_Character_Set.Invert := True ; // Black text on white background
        Current_Font := Text ;
        Write_cef32_Media_Code( MCC_Change_Font, 0, Text ) ;
    end ;
end ;


procedure TMain_Form.Load_Alt_Character_Set( const Text : string ) ;

begin
    if( Text <> Alt_Current_Font ) then
    begin
        if( Alt_Character_Set = nil ) then
        begin
            Alt_Character_Set := Get_Character_Set ;
        end ;
        Alt_Character_Set.Load( Pchar( Text ) ) ;
        Alt_Character_Set.Invert := True ; // Black text on white background
        Alt_Current_Font := Text ;
    end ;
end ;


procedure TMain_Form.Correct_For_Cursor( Old_Column : integer ) ;

begin
    // Handle cursor bounds
    if( Print_Head_Char_Column > Max_Width ) then
    begin
        if( _Line_Wrap ) then
        begin
            Print_Head_Char_Column := 1 ;
            Print_Head_Column := Margin ;
            Line_Feed ;
        end else
        begin
            Print_Head_Char_Column := Max_Width ;
            Print_Head_Column := Image.Picture.Bitmap.Width - Margin - Character_Set.Width( 32 ) ;
        end ;
    end ;
    if( Print_Head_Row >= Image.Picture.Bitmap.Height - Margin ) then
    begin
        Next_Page ;
    end ;
    if(
        ( _Margin_Bell > 0 )
        and
        ( Print_Head_Char_Column >= _Margin_Bell )
        and
        ( Old_Column < Print_Head_Char_Column )
        and
        ( Old_Column < _Margin_Bell )
      ) then
    begin
        messagebeep( 0 ) ;
    end ;

    Update_Status ;
end ; // TMain_Form.Correct_For_Cursor


function TMain_Form.Load_Keyboard( Name : string ; Silent : boolean ) : boolean ;

var C : TComponent ;

begin
    Result := False ; // Assume failure
    try
        if( _UI = nil ) then
        begin
            _UI := Pending_UI ;
        end ;
        C := _UI.Load_Component( PChar( Name ) ) ;
        if( C = nil ) then
        begin
            if( not Silent ) then
            begin
                ShowMessage( 'Cannot load component' ) ;
            end ;
            exit ;
        end ;
        if( C.Component_Type <> Component_Type_Keyboard ) then
        begin
            if( not Silent ) then
            begin
                ShowMessage( 'Cannot load component: not a keyboard component' ) ;
            end ;
            try
                C.Terminate ;
            except
            end ;
            exit ;
        end ;
        if( Keyboard <> nil ) then
        begin
            Keyboard.Terminate ;
        end ;
        Keyboard := C ;
        KB_Receiver := TKBComponent.Create ;
        TKBComponent( KB_Receiver ).Main_Form := self ;
        Keyboard.Connect_Output( KB_Receiver ) ;
        if( not Panel2.Visible ) then
        begin
            Keyboard1Click( nil ) ;
        end ;
        if( Online1.Checked ) then
        begin
            Keyboard.Keyboard.Set_Key_Down( 'LINE', True ) ;
        end else
        begin
            Keyboard.Keyboard.Set_Key_Down( 'LOCAL', True ) ;
        end ;
        Keyboard.Keyboard.Set_Key_Down( PChar( 'BAUD RATE ' + inttostr( Baud ) ), True ) ;
        Result := True ;
    except
    end ;
end ; // TMain_Form.Load_Keyboard


procedure TMain_Form.Enable1Click(Sender: TObject);

begin
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_LED_State( 'SELECT AVAIL', Enable1.Checked ) ;
    end ;
    if( not Enable1.Checked ) then
    begin
        Address_Mode := False ;
        Transmit_Disabled := False ;
        Unselect_Locked := False ;
        Waiting_For_Lock := False ;
        Master := False ;
    end ;
end ;


procedure TMain_Form.Write_cef32_Media_Code( Code : byte ; Value : longint ;
    const S_Value : string ) ;

var D : byte ;

begin
    if( Paper_Out or Hide_Printing ) then
    begin
        exit ;
    end ;
    D := 0 ;
    blockwrite( FPaper, D, 1 ) ;
    blockwrite( FPaper, Code, 1 ) ;
    case Code of
        MCC_VT, MCC_HT, MCC_LPI, MCC_CPI : blockwrite( FPaper, Value, 4 ) ;
        MCC_Change_Font : begin
                blockwrite( FPaper, PChar( S_Value )[ 0 ], length( S_Value ) ) ;
                blockwrite( FPaper, D, 1 ) ;
            end ;
    end ;
end ;


function TMain_Form.Execute_Escape_Sequence : boolean ; // LA36 escape processing

begin
    Result := True ;
    case Escape_Sequence[ 1 ] of
        '1' : begin
                  if( Horizontal_Tabs.Indexof( Print_Head_Char_Column ) = -1 ) then
                  begin
                      Horizontal_Tabs.Add( Print_Head_Char_Column ) ;
                      Horizontal_Tabs.Sort( -1 ) ;
                      Write_cef32_Media_Code( MCC_HT, Print_Head_Char_Column, '' ) ;
                      Print_Char( 32 ) ;
                  end ;
              end ;
        '2' : begin
                  Horizontal_Tabs.Clear ;
                  Write_cef32_Media_Code( MCC_Clear_HT, 0, '' ) ;
              end ;
        '3' : begin
                  if( Vertical_Tabs.Indexof( Print_Head_Char_Line ) = -1 ) then // Not already defined
                  begin
                      Vertical_Tabs.Add( Print_Head_Char_Line ) ;
                      Vertical_Tabs.Sort( -1 ) ;
                      Write_cef32_Media_Code( MCC_VT, Print_Head_Char_Line, '' ) ;
                  end ;
              end ;
        '4' : begin
                  Vertical_Tabs.Clear ;
                  Write_cef32_Media_Code( MCC_Clear_VT, 0, '' ) ;
              end ;
        else Result := False ;
    end ;
end ; // TMain_Form.Execute_Escape_Sequence


procedure TMain_Form.SetCodes1Click(Sender: TObject) ;

begin
    Selective_Addressing_Dialog.Unit_Code.Value := Unit_Code ;
    Selective_Addressing_Dialog.Group_Code.Value := Group_Code ;
    if( Selective_Addressing_Dialog.ShowModal = mrOK ) then
    begin
        Unit_Code := Selective_Addressing_Dialog.Unit_Code.Value ;
        Group_Code := Selective_Addressing_Dialog.Group_Code.Value ;
    end ;
end ;


procedure TMain_Form.Set_Alternate_KeyPad_Mode( Value : boolean ) ;

begin
    Alternate_KeyPad_Mode := Value ;
end ;


function TMain_Form.Valid_Escape_Sequence : boolean ;

begin
    Result := False ;
    if( pos( Escape_Sequence[ 1 ], '=>ABCDFGHIJKYZ[\' ) > 0 ) then
    begin
        Result := True ;
    end ;
end ;


procedure TMain_Form.VerticalTabPositions1Click(Sender: TObject) ;

var Dummy, Loop : integer ;

begin
    VT_Form.Caption := 'Vertical tabs' ;
    VT_Form.Memo1.Text := '' ;
    for Loop := 0 to Vertical_Tabs.Count - 1 do
    begin
        VT_Form.Memo1.Lines.Add( inttostr( Vertical_Tabs[ Loop ] ) ) ;
    end ;
    if( VT_Form.ShowModal = mrOK ) then
    begin
        Vertical_Tabs.Clear ;
        for Loop := 0 to VT_Form.Memo1.Lines.Count - 1 do
        begin
            Dummy := strtoint( VT_Form.Memo1.Lines[ Loop ] ) ;
            if( Vertical_Tabs.Indexof( Dummy ) = -1 ) then
            begin
                Vertical_Tabs.Add( Dummy ) ;
            end ;
        end ;
        Vertical_Tabs.Sort( -1 ) ;
    end ;
end ;


procedure TMain_Form.Transmit_Here_Is ;

var I : integer ;

begin
    if( Here_Is_Enabled ) then
    begin
        for I := 1 to length( Here_Is ) do
        begin
            Write_Serial( ord( Here_Is[ I ] ) ) ;
        end ;
        Write_Serial( 0 ) ;
        if( length( Here_Is ) < 21 ) then
        begin
            for I := length( Here_Is ) to 21 do // Fill with NULs to 22 characters
            begin
                Write_Serial( 0 ) ;
            end ;
        end ;
    end ;
end ;


procedure TMain_Form.Local_Line_Feed ;

begin
    Print_Char( ord( LF ) ) ;
end ;


procedure TMain_Form.LockCharacterSet1Click(Sender: TObject) ;

begin
    Char_Set_Lock := LockCharacterSet1.Checked ;
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_Key_Down( 'CHAR SET LOCK', Char_Set_Lock ) ;
    end ;
end ;


procedure TMain_Form.Print_Char( Value : integer ) ;

var Dummy, Old_Column : integer ;
    Found : boolean ;
    Rect : TRect ;
    Tab_Pixels, W, X : integer ;

begin
    // Handle device select logic...
    if( SelectiveAddressing1.Enabled and Enable1.Checked and not Master ) then
    begin
        if( Waiting_For_Lock and ( Value <> 2 ) ) then
        begin
            Transmit_Disabled := True ;
        end ;
        Waiting_For_Lock := False ;
        if( Keyboard <> nil ) then
        begin
            Keyboard.Keyboard.Set_LED_State( 'SELECT AVAIL', False ) ;
        end ;
        if( Value = 0 ) then // Set all to address mode
        begin
            Address_Mode := True ;
        end else
        if( Value = 2 ) then
        begin
            Unselect_Locked := not Selected1.Checked ;
            Transmit_Disabled := Unselect_Locked ;
            Address_Mode := False ;
        end else
        if( Value = 3 ) then
        begin
            Address_Mode := True ; // Set all to address mode
        end else
        if( Value = 4 ) then // Deselect all
        begin
            Select_Device( False ) ;
            Transmit_Disabled := False ;
            if( Keyboard <> nil ) then
            begin
                Keyboard.Keyboard.Set_LED_State( 'SELECT AVAIL', True ) ;
            end ;
        end else
        if( Value = ord( ENQ ) ) then
        begin
            Waiting_For_Lock := True ;
        end else
        if( Value = 7 ) then // Select all
        begin
            Select_Device( True ) ;
            Transmit_Disabled := True ;
        end else
        if( Address_Mode ) then
        begin
            if( ( Value = Unit_Code ) or ( Value = Group_Code ) ) then
            begin
                Select_Device( True ) ;
                Transmit_Disabled := False ;
            end else
            begin
                Transmit_Disabled := True ;
            end ;
        end ;
    end ;
    if( Unselect_Locked ) then
    begin
        exit ;
    end ;

    // Process character...
    case Mode of
        HCM_ASR33, HCM_KSR33, HCM_RO33 :
            begin
                Value := Value and 127 ;
                case Value of
                    0..31 : case chr( Value ) of
                                BEL, BS, HT, CR, LF, ENQ, DC1, DC3:; // These are processed
                                else exit ;
                            end ;
                    127 : exit ;
                end ;
            end ;
        HCM_LA30 :
            begin
                Value := Value and 127 ;
                case Value of
                    0..31 : case chr( Value ) of
                                BEL, BS, HT, CR, LF:; // These are processed
                                else exit ;
                            end ;
                    127 : exit ;
                end ;
            end ;
        HCM_LA36 :
            begin
                case Value of
                    0..31 : case chr( Value ) of
                                BEL, BS, HT, CR, LF, VT, FF, ENQ, ESC:; // These are processed
                                else exit ;
                            end ;
                    127..255 : exit ;
                end ;
            end ;
    end ; // case Mode

    if( ( not Hide_Printing ) and ( not Paper_Out ) ) then
    begin
        seek( FPaper, filesize( FPaper ) ) ;
        blockwrite( FPaper, Value, 1 ) ;
        if( Value = 0 ) then // A null, need to escape it
        begin
            blockwrite( FPaper, Value, 1 ) ;
        end ;
    end ;
    if( Print_Head_Page <> Displayed_Page ) then
    begin
        if( Displayed_Page <> -1 ) then
        begin
            exit ;
        end ;
    end ;
    Old_Column := Print_Head_Column ;

    if( Print_Head_Column < Margin ) then
    begin
        Print_Head_Column := Margin ;
    end ;
    if( Print_Head_Row < Margin ) then
    begin
        Print_Head_Row := Margin ;
    end ;

    // Handle character...
    if( ( Mode = HCM_LA36 ) and FormControl1.Enabled and FormControlOption1.Checked ) then
    begin
        if( Value = ord( ESC ) ) then // Beginning new escape sequence
        begin
            In_Escape_Sequence := True ;
            Escape_Sequence := '' ;
        end else
        if( In_Escape_Sequence ) then
        begin
            Escape_Sequence := Escape_Sequence + chr( Value ) ;
            if( Execute_Escape_Sequence ) then // Sucessfully executed the sequence
            begin
                In_Escape_Sequence := False ;
                exit ;
            end ;

            // If we get here, the sequence is not valid
            In_Escape_Sequence := False ;
            Escape_Sequence := '' ;
        end ;
    end ;

    if( Value = ord( DC1 ) ) then
    begin
        if( ( Mode = HCM_ASR33 ) and ( not Hide_Printing ) ) then
        begin
            Start_Tape ;
        end ;
    end else
    if( Value = ord( DC3 ) ) then
    begin
        if( ( Mode = HCM_ASR33 ) and ( not Hide_Printing ) ) then
        begin
            Stop_Tape ;
        end ;
    end else
    if( Value = ord( CR ) ) then
    begin
        Print_Head_Char_Column := 1 ;
        Print_Head_Column := Margin ;
    end else
    if( Value = ord( VT ) ) then
    begin
        if( ( Mode = HCM_LA36 ) and FormControlOption1.Checked ) then
        begin
            Found := False ;
            for Dummy := 0 to Vertical_Tabs.Count - 1 do // Find next vertical tab
            begin
                if( Vertical_Tabs[ Dummy ] > Print_Head_Char_Line ) then
                begin
                    Print_Head_Char_Line := Vertical_Tabs[ Dummy ] ;
                    Found := True ;
                    break ;
                end ;
            end ;
            if( not Found ) then
            begin
                Next_Page ;
            end ;
        end ;
    end else
    if( Value = ord( FF ) ) then
    begin
        if( Mode = HCM_LA36 ) then
        begin
            Next_Page ;
        end ;
    end else
    if( Value = ord( LF ) ) then
    begin
        inc( Print_Head_Char_Line ) ;
        Print_Head_Row := Print_Head_Row + Character_Set.Height ;
        if( Print_Head_Row >= Image.Picture.Bitmap.Height - Margin ) then
        begin
            Next_Page ;
        end else
        begin
            if( not Hide_Printing ) then
            begin
                Bring_Print_Head_Into_View ;
            end ;
        end ;
    end else
    if( Value = ord( BEL ) ) then
    begin
        messagebeep( 0 ) ;
    end else
    if( Value = ord( ENQ ) ) then
    begin
        Transmit_Here_Is ;
    end else
    if( Value = ord( HT ) ) then
    begin
        Found := False ;
        if( ( Mode = HCM_LA36 ) and FormControlOption1.Checked ) then
        begin
            for Dummy := 0 to Horizontal_Tabs.Count - 1 do // Find next vertical tab
            begin
                if( Horizontal_Tabs[ Dummy ] > Print_Head_Char_Column ) then
                begin
                    Print_Head_Char_Column := Horizontal_Tabs[ Dummy ] ;
                    Print_Head_Column :=
                        Print_Head_Char_Column * Character_Set.Width( 32 ) ;
                    Found := True ;
                    break ;
                end ;
            end ;
        end ;
        if( not Found ) then
        begin
            Tab_Pixels := Paper_Width div Hard_Tabs ; // Number of tab columns on paper
            Tab_Pixels := Image.Picture.Bitmap.Width div Tab_Pixels ; // Number of pixels per tab
            if( Tab_Pixels = 0 ) then
            begin
                Tab_Pixels := 1 ;
            end ;
            W := Tab_Pixels ;
            while( W < Print_Head_Column ) do
            begin
                W := W + Tab_Pixels ;
            end ;
            X  := Character_Set.Width( 32 ) ;
            while( Print_Head_Column < W ) do
            begin
                Print_Head_Column := Print_Head_Column + X ;
                inc( Print_Head_Char_Column ) ;
            end ;
        end ;
    end else
    if( Value = ord( BS ) ) then
    begin
        dec( Print_Head_Char_Column ) ;
        if( Print_Head_Char_Column < 1 ) then
        begin
            Print_Head_Char_Column := 1 ;
        end ;
        Print_Head_Column := Print_Head_Column - Character_Set.Width( 32 ) ;
        if( Print_Head_Column < Margin ) then
        begin
            Print_Head_Column := Margin ;
        end ;
    end else
    begin
        if( _Uppercase_In ) then
        begin
            Value := ord( upcase( chr( Value ) ) ) ;
        end ;
        if( not Hide_Printing ) then
        begin
            // Output the glyph...
            if( Character_Set = nil ) then
            begin
                exit ;
            end ;
            Output_Char( Print_Head_Column, Print_Head_Row, Value ) ;
            Value := Character_Set.Width( Value ) ;
            Rect.Top := Print_Head_Row ;
            Rect.Left := Print_Head_Column ;
            Rect.Right := Print_Head_Row + Value ;
            Rect.Bottom := Print_Head_Column + Character_Set.Height ;
            Image.Invalidate ;
            //InvalidateRect( Image.Picture.Handle, @Rect, False ) ;
            if( Value <> 0 ) then
            begin
                inc( Print_Head_Char_Column ) ;
            end ;
            Print_Head_Column := Print_Head_Column + Value ;
        end ;
        Update_Status ;
    end ;
    Correct_For_Cursor( Old_Column ) ;
end ; // TMain_Form.Print_Char


procedure TMain_Form.Transmit1Click(Sender: TObject) ;

begin
    Transmit_Here_Is ;
end ;


function TMain_Form.New_Paper( Filename : string ) : integer ;

var Dummy : integer ;
    Header : TCEF_Paper_Media_Header ;

begin
    // Defaults for filename...
    if( Extension_pos( Filename ) = 0 ) then
    begin
        Filename := Filename + '.cef_m' ;
    end ;
    if( pos( ':', Filename ) + pos( '\', Filename ) = 0 ) then
    begin
        Filename := OS^.Application_Data_Path( 'CEF32', 'Media', 0 ) + Filename ;
    end ;

    // Create file...
    assignfile( FPaper, Filename ) ;
    {$I-}
    rewrite( FPaper, 1 ) ;
    {$I+}
    Result := IOResult ;
    if( Result <> 0 ) then
    begin
        exit ;
    end ;
    fillchar( Header, sizeof( Header ), 0 ) ;
    Header.Prefix := 65535 ;
    Header.ID := 255 ;
    Header.Facility := 125 ;
    Header.Version := 10 ;
    Header.Max_Page := Max_Page ;
    if( Barred ) then
    begin
        Header.Typ := Bar_Color ;
    end else
    begin
        Header.Typ := 0 ;
    end ;
    Header.Height := Paper_Height ;
    Header.Width := Paper_Width ;
    Header.Bleed := Bleed ;
    {$I-}
    blockwrite( FPaper, Header, sizeof( Header ) ) ;
    {$I+}
    Result := IOResult ;
    if( Result <> 0 ) then
    begin
        exit ;
    end ;
    if( Horizontal_Tabs <> nil ) then
    begin
        for Dummy := 0 to Horizontal_Tabs.Count - 1 do
        begin
            Write_cef32_Media_Code( MCC_HT, Horizontal_Tabs[ Dummy ], '' ) ;
        end ;
    end ;
    if( Vertical_Tabs <> nil ) then
    begin
        for Dummy := 0 to Vertical_Tabs.Count - 1 do
        begin
            Write_cef32_Media_Code( MCC_VT, Vertical_Tabs[ Dummy ], '' ) ;
        end ;
    end ;
    Write_cef32_Media_Code( MCC_LPI, LPI, '' ) ;
    Write_cef32_Media_Code( MCC_CPI, CPI, '' ) ;
    Write_cef32_Media_Code( MCC_Change_Font, 0, Current_Font ) ;
end ; // TMain_Form.New_Paper


procedure TMain_Form.Newpaper1Click(Sender: TObject);

var Dummy : integer ;
    Header : TCEF_Paper_Media_Header ;
    S : string ;

begin
    if( not InputQuery( 'New paper', 'Paper filename', S ) ) then
    begin
        exit ;
    end ;
    if( pos( ':', S ) + pos( '\', S ) = 0 ) then // No path
    begin
        S := OS^.Application_Data_Path( 'CEF32', 'Media', ADPF_Create ) + S ;
    end ;
    Dummy := New_Paper( S ) ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( ERT( Dummy ) ) ;
        Paper_File := '' ;
        if( Keyboard <> nil ) then
        begin
            Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', True ) ;
        end ;
        exit ;
    end ;
    Text_File := False ;

    fillchar( Header, sizeof( Header ), 0 ) ;
    {$I-}
    blockread( FPaper, Header, sizeof( Header ) ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy = 0 ) then
    begin
        {$I-}
        reset( FPaper, 1 ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) ) ;
            Paper_File := '' ;
            if( Keyboard <> nil ) then
            begin
                Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', True ) ;
            end ;
            exit ;
        end ;
    end ;
    Update_Settings_From_Header( Header ) ;
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', False ) ;
    end ;
    Paper_File := S ;
end ; // TMain_Form.Newpaper1Click


procedure TMain_Form.New_Char( Value : integer ) ;

var I : integer ;

begin
    if( Cancellogging1.Enabled and ( Value < 256 ) ) then
    begin
        {$I-}
        blockwrite( Log_File, Value, 1 ) ;
        {$I+}
        I := IOResult ;
        if( I <> 0 ) then
        begin
            Cancellogging1Click( nil ) ;
            ShowMessage( 'Logging error: ' + ERT( I ) + '. Logging disabled' ) ;
        end ;
    end ;
    if( Block_New_Chars ) then
    begin
        exit ;
    end ;
    if( Paper_Out ) then // Paper out
    begin
        Input_Queue.Add( Value ) ;
        exit ;
    end ;
    if( Input_Queue.Count > 0 ) then
    begin
        // Process items in input queue
        while(
               ( Input_Queue.Count > 0 )
             ) do
        begin
            Print_Char( Input_Queue[ 0 ] ) ;
            Input_Queue.Delete( 0 ) ;
        end ;
    end ;

    Print_Char( Value ) ;
end ; // TMain_Form.New_Char


procedure TMain_Form.Pagedown1Click(Sender: TObject) ;

begin
    inc( Displayed_Page ) ;
    Build_Page( Displayed_Page ) ;
    Update_Status ;
end ;


procedure TMain_Form.Pageup1Click(Sender: TObject) ;

begin
    if( Displayed_Page = 0 ) then
    begin
        exit ;
    end ;
    dec( Displayed_Page ) ;
    Build_Page( Displayed_Page ) ;
    Update_Status ;
end ;


procedure TMain_Form.Pause_Input( Value : boolean ) ;

begin
    if( CTSRTS1.Checked ) then
    begin
        _LA36.Set_Signal( 'DTR', not Value ) ;
    end else
    if( XOnXOff1.Checked ) then
    begin
        if( Value ) then
        begin
            Write_Serial( 19 ) ; // XOFF
        end else
        begin
            Write_Serial( 17 ) ; // XON
        end ;
    end ;
end ;


function TMain_Form.Check_Flow_Control : boolean ; // Return true if can send data

begin
    Result := True ;
    if( CTSRTS1.Checked ) then
    begin
        _LA36.Set_Signal( 'RTS', True ) ;
        _LA36.Get_Signal( 'CTS', Result ) ;
    end ;
end ;


procedure TMain_Form.Flush_Queue ;

begin
    while( not Transmit_Paused ) do
    begin
        if( Output_Queue.Count > 0 ) then
        begin
            if( not Check_Flow_Control ) then
            begin
                break ;
            end ;
            _Write( Output_Queue[ 0 ] ) ;
            New_Char( Output_Queue[ 0 ] ) ;
            Output_Queue.Delete( 0 ) ;
        end else
        begin
            break ;
        end ;
    end ;
end ;


procedure TMain_Form.Receive_Char( Value : longint ) ;

begin
    if( XonXoff1.Checked ) then
    begin
        if( Value = 17 ) then // Control-Q
        begin
            Transmit_Paused := False ;
            Flush_Queue ;
            exit ;
        end ;
        if( Value = 19 ) then // Control-S
        begin
            Transmit_Paused := True ;
            exit ;
        end ;
    end ;
    New_Char( Value ) ;
    if( ( Value = ord( CR ) ) and Receive1.Checked ) then
    begin
        New_Char( ord( LF ) ) ;
    end ;
end ;


procedure TMain_Form.Set_Mode( _Mode : THC_Mode ) ;

var F, KB : string ;

begin
    if( Character_Set = nil ) then
    begin
        Character_Set := Get_Character_Set ;
        Standard_Character_Set := Character_Set ;
    end ;
    AlternateCharacterSet.Enabled := False ;
    SelectiveAddressing1.Enabled := False ;
    CompressedFont1.Enabled := False ;
    FormControl1.Enabled := False ;
    if( ( _Mode = HCM_ASR33 ) or ( _Mode = HCM_KSR33 ) or ( _Mode = HCM_RO33 ) ) then
    begin
        _Margin_Bell := 0 ;
        _Uppercase_Out := True ;
        _Uppercase_In := True ;
        _Line_Wrap := False ;
        if( _Mode = HCM_RO33 ) then
        begin
            KB := '' ;
        end else
        begin
            KB := 'ASR33_KB' ;
        end ;
        CPI := 10 ;
        LPI := 6 ;
        Max_Width := 80 ;
        Hard_Tabs := 10 ; // Tabs at each 1" (8 characters)
        F := 'Courier New' ;
    end else
    if( _Mode = HCM_LA30 ) then
    begin
        _Margin_Bell := 64 ;
        _Uppercase_Out := False ;
        _Uppercase_In := True ;
        _Line_Wrap := False ;
        CPI := 10 ;
        LPI := 6 ;
        KB := 'LK01_KB' ;
        Max_Width := 80 ;
        Hard_Tabs := 10 ; // Tabs at each 1" (8 characters)
        F := 'LA30' ;
    end else
    if( _Mode = HCM_LA36 ) then
    begin
        _Margin_Bell := 64 ;
        _Uppercase_Out := False ;
        _Uppercase_In := False ;
        _Line_Wrap := False ;
        CPI := 10 ;
        LPI := 6 ;
        Max_Width := 132 ;
        KB := 'LK02_KB' ;
        Hard_Tabs := 10 ; // Tabs at each 1" (8 characters)
        F := 'LA36' ;
        AlternateCharacterSet.Enabled := True ;
        SelectiveAddressing1.Enabled := True ;
        CompressedFont1.Enabled := True ;
        FormControl1.Enabled := True ;
    end ;
    if( _Mode = HCM_ASR33 ) then
    begin
        Loadpapertape1.Enabled := True ;
        if( Tape_File <> '' ) then
        begin
            Unloadpapertape1.Enabled := True ;
        end ;
    end else
    begin
        Loadpapertape1.Enabled := False ;
        Unloadpapertape1.Enabled := False ;
        Tape_Running := False ;
        Timer.Interval := 1000 ;
        Cancel_Timer ;
    end ;
    if( Mode <> HCM_LA36 ) then
    begin
        Transmit_Disabled := False ;
        Address_Mode := False ;
        Unselect_Locked := False ;
        Waiting_For_Lock := False ;
        Master := False ;
    end ;
    Load_Character_Set( F ) ;
    Correct_For_Cursor( Print_Head_Column ) ;
    Mode := _Mode ;
    if( not CompressedFont1.Enabled ) then
    begin
        CompressedFont1.Checked := False ;
    end ;

    // Setup keyboard...
    if( KB <> '' ) then
    begin
        if( not Load_Keyboard( KB, True ) ) then
        begin
            Load_Keyboard( 'ASR33_KB', True ) ;
        end ;
    end ;
    Panel2.Visible := ( KB <> '' ) ;

    // Final setup...
    Write_cef32_Media_Code( MCC_LPI, LPI, '' ) ;
    Write_cef32_Media_Code( MCC_CPI, CPI, '' ) ;
    Set_Standard_Character_Set ;
    Update_Status ;
end ; // TMain_Form.Set_Mode


procedure TMain_Form.Write_Serial( Value : integer ) ;

begin
    if( Transmit_Disabled ) then
    begin
        exit ;
    end ;
    if(
        SelectiveAddressing1.Enabled
        and
        Enable1.Checked
        and
        ( not Selected1.Checked )
        and
        ( not Address_Mode )
        and
        ( not Unselect_Locked )
      ) then
    begin
        if( Value = 4 ) then
        begin
            Master := not Master ;
        end ;
    end ;
    _LA36.Set_Signal( 'BREAK', False ) ;
    if( _Uppercase_Out ) then
    begin
        Value := ord( upcase( chr( Value ) ) ) ;
    end ;
    Flush_Queue ;
    if( Transmit_Paused ) then
    begin
        Output_Queue.Add( Value ) ;
        exit ;
    end ;
    if( not Check_Flow_Control ) then
    begin
        Output_Queue.Add( Value ) ;
        exit ;
    end ;
    if( Local1.Checked ) then
    begin
        New_Char( Value ) ;
        if( ( Value = ord( CR ) ) and AutoLF1.Checked ) then
        begin
            New_Char( ord( LF ) ) ;
        end ;
    end ;
    if( Online1.Checked ) then
    begin
        _Write( Value ) ;
        if( ( Value = ord( CR ) ) and AutoLF1.Checked ) then
        begin
            _Write( ord( LF ) ) ;
        end ;
    end ;
end ;


procedure TMain_Form.WMGetDlgCode( var Message : TMessage ) ;

begin
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS or DLGC_WANTTAB ;
end ;


procedure TMain_Form.CB_Timer( Sender : TObject ) ;

var Count, Value : integer ;

begin
    Timer.Enabled := False ;
    if( HandleAllocated ) then
    begin
        if( Keyboard.User_Interface.Parent_Window = 0 ) then
        begin
            Keyboard.User_Interface.Parent_Window := Panel2.Handle ;
            Keyboard.User_Interface.Set_Hidden( False ) ;
            Panel2.ClientHeight := Keyboard.User_Interface.Optimal_Height ;
        end ;
        if( Tape_Running and ( not Paper_Out ) ) then
        begin
            Value := 0 ;
            if( eof( FTape ) ) then
            begin
                Tape_Running := False ;
                Timer.Interval := 1000 ;
            end ;
            if( Tape_Running ) then
            begin
                blockread( FTape, Value, 1, Count ) ;
                if( Count > 0 ) then
                begin
                    Write_Serial( Value ) ;
                end ; // if( Count > 0 )
            end ;
        end ;
    end ;
    if( not Cancel_Timer ) then
    begin
        Timer.Enabled := True ;
    end ;
end ;


// Property handlers...

function TMain_Form._Get_Key_Mapper : TCEF_Key_Mapper ;

var Loop : integer ;
    S : string ;

begin
    if( _Key_Mapper = nil ) then
    begin
        _Key_Mapper := Get_Key_Mapper ;
        Key_Mapper.Add_Key( 'F1' ) ;
        Key_Mapper.Add_Key( 'F2' ) ;
        Key_Mapper.Add_Key( 'F3' ) ;
        Key_Mapper.Add_Key( 'F4' ) ;
        Key_Mapper.Add_Key( 'F5' ) ;
        Key_Mapper.Add_Key( 'F6' ) ;
        Key_Mapper.Add_Key( 'F7' ) ;
        Key_Mapper.Add_Key( 'F8' ) ;
        Key_Mapper.Add_Key( 'F9' ) ;
        Key_Mapper.Add_Key( 'F10' ) ;
        Key_Mapper.Add_Key( 'F11' ) ;
        Key_Mapper.Add_Key( 'F12' ) ;
        Key_Mapper.Add_Key( 'CANCEL' ) ;
        Key_Mapper.Add_Key( 'CLEAR' ) ;
        Key_Mapper.Add_Key( 'PAUSE' ) ;
        Key_Mapper.Add_Key( 'PAGE_UP' ) ;
        Key_Mapper.Add_Key( 'PAGE_DOWN' ) ;
        Key_Mapper.Add_Key( 'END_CB' ) ;
        Key_Mapper.Add_Key( 'PRINT' ) ;
        Key_Mapper.Add_Key( 'INSERT' ) ;
        for Loop := 33 to 126 do
        begin
            if( Upcase( char( Loop ) ) = char( Loop ) ) then
            begin
                S := char( Loop ) ;
                Key_Mapper.Add_Mapping( PChar( S ) ) ;
            end ;
        end ;
        Key_Mapper.Add_Mapping( 'SPACE' ) ;
        Key_Mapper.Add_Mapping( 'TAB' ) ;
        Key_Mapper.Add_Mapping( 'ESCAPE' ) ;
        Key_Mapper.Add_Mapping( 'CR' ) ;
        Key_Mapper.Add_Mapping( 'LF' ) ;
        Key_Mapper.Add_Mapping( 'RUB OUT' ) ;
        Key_Mapper.Add_Mapping( 'BREAK' ) ;
    end ;
    Result := _Key_Mapper ;
end ; // TMain_Form._Get_Key_Mapper


// Callbacks...

procedure TMain_Form.Check_Click( Sender : TObject ) ;

begin
    TMenuItem( Sender ).Checked := not TMenuItem( Sender ).Checked ;
    if( Keyboard <> nil ) then
    begin
        if( Online1.Checked ) then
        begin
            Keyboard.Keyboard.Set_Key_Down( 'LINE', True ) ;
        end else
        begin
            Keyboard.Keyboard.Set_Key_Down( 'LOCAL', True ) ;
        end ;
    end ;
end ;


procedure TMain_Form.FormCreate( Sender : TObject ) ;

begin
    // Setup screen...
    Magnification := 1 ;
    Margin := 2 ;
    Print_Head_Row := Margin ;
    Print_Head_Column := Margin ;
    Print_Head_Char_Column := 1 ;
    Print_Head_Char_Line := 1 ;
    Timer := TTimer.Create( Application ) ;
    Timer.OnTimer := CB_Timer ;
    Timer.Enabled := True ;
    Output_Queue := TInteger_List.Create ;
    Input_Queue := TInteger_List.Create ;
    Paper_Width := 85 ; // 8.5 x 11 paper
    Paper_Height := 110 ;
    Bleed := 1 ;
    Set_Mode( HCM_LA36 ) ;
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_LED_State( 'PAPER OUT', True ) ;
    end ;
    Vertical_Tabs := TInteger_List.Create ;
end ; // TMain_Form.FormCreate


procedure TMain_Form.BufferKeyDown( Sender : TObject ; var Key : Word ;
    Shift : TShiftState ) ;

var _Buffer : array[ 0..2 ] of char ;
    Defined : boolean ;
    KeyStates : TKeyboardState ;

    procedure Substitute( CB : string ) ;

    var S : string ;

    begin
        S := string( Key_Mapper.Mapping( Pchar( CB ) ) ) ;
        if( S <> '' ) then
        begin
            Defined := True ;
            if( length( S ) = 1 ) then
            begin
                Key := ord( S[ 1 ] ) ;
            end else
            if( S = 'SPACE' ) then
            begin
                Key := 32 ;
            end else
            if( S = 'CR' ) then
            begin
                Key := 13 ;
            end else
            if( S = 'DEL' ) then
            begin
                Key := 127 ;
            end else
            if( S = 'LF' ) then
            begin
                Key := 10 ;
            end else
            if( S = 'ESC' ) then
            begin
                Key := ord( ESC ) ;
            end else
            if( S = 'BREAK' ) then
            begin
                _LA36.Set_Signal( 'BREAK', True ) ;
            end else
            if( S = 'TAB' ) then
            begin
                Key := 9 ;
            end else
            if( S = 'SHIFT LOCK' ) then
            begin
                Keyboard.Keyboard.Set_Key_Down( 'LOCK', True ) ;
                Key := 0 ;
                exit ;
            end ;
        end ; // if( S <> '' )
    end ; // .Substitute

begin
    Defined := False ;
    case Key of
        VK_BACK :
            begin
                Key :=  $5F ;
                Defined := True ;
            end ;
        VK_F1 : Substitute( 'F1' ) ;
        VK_F2 : Substitute( 'F2' ) ;
        VK_F3 : Substitute( 'F3' ) ;
        VK_F4 : Substitute( 'F4' ) ;
        VK_F5 : Substitute( 'F5' ) ;
        VK_F6 : Substitute( 'F6' ) ;
        VK_F7 : Substitute( 'F7' ) ;
        VK_F8 : Substitute( 'F8' ) ;
        VK_F9 : Substitute( 'F9' ) ;
        VK_F10 : Substitute( 'F10' ) ;
        VK_F11 : Substitute( 'F11' ) ;
        VK_F12 : Substitute( 'F12' ) ;
        VK_CANCEL : Substitute( 'CANCEL' ) ;
        VK_CLEAR : Substitute( 'CLEAR' ) ;
        VK_PAUSE : Substitute( 'PAUSE' ) ;
        VK_PRIOR : Substitute( 'PAGE_UP' ) ;
        VK_NEXT : Substitute( 'PAGE_DOWN' ) ;
        VK_END : Substitute( 'END_CB' ) ;
        VK_PRINT : Substitute( 'PRINT' ) ;
        VK_INSERT : Substitute( 'INSERT' ) ;
    end ;
    if( not Defined ) then
    begin
        GetKeyboardState( KeyStates ) ;
        if( ToAscii( Key, MapVirtualKey( Key, 0 ), KeyStates, _Buffer, 0 ) = 1 ) then
        begin
            Key := ord( _Buffer[ 0 ] ) ;
        end else
        begin
            case Key of
                VK_UP : Key := $97 ;
                VK_DOWN : Key := $9A ;
                VK_LEFT : Key := $81 ;
                VK_RIGHT : Key := $93 ;
                VK_TAB : Key := 9 ;
                VK_ESCAPE : Key := ord( ESC ) ;
                VK_RETURN : Key := ord( CR ) ;
                VK_HOME : Key := $8E ;
                VK_DELETE : Key := $5F ;
                VK_BACK : Key := $5F ;
                else exit ;
            end ;
        end ;
    end ;

    Key := 0 ;
end ; // TMain_Form.BufferKeyDown


procedure TMain_Form.BufferKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);

begin
    Key := 0 ;
end ;


procedure TMain_Form.BufferKeyPress(Sender: TObject; var Key: Char);

begin
    Key := #0 ;
end ;


procedure TMain_Form.Exit1Click( Sender : TObject ) ;

begin
    _LA36.Terminate ;
end ;


procedure TMain_Form.Display1Click( Sender : TObject ) ;

begin
    Check_Click( Sender ) ;
    Image.Invalidate ;
end ;


procedure TMain_Form.Copyscreen1Click( Sender : TObject ) ;

begin
    Clipboard.Assign( Image.Picture.Bitmap ) ;
end ;


procedure TMain_Form.AdvancepaperlocalLF1Click(Sender: TObject) ;

begin
    Local_Line_Feed ;
    Update_Status ;
end ;


procedure TMain_Form.AutoLFClick(Sender: TObject) ;

begin
    Auto_LF := AutoLF1.Checked ;
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_Key_Down( 'AUTO LF', Auto_LF ) ;
    end ;
end ;


procedure TMain_Form.Baud_Click( Sender : TObject ) ;

begin
    Check_Click( Sender ) ;
    if( TMenuItem( Sender ).Caption = '&Auto' ) then
    begin
        Baud := 0 ;
    end else
    begin
        Baud := strtoint( Extract( TMenuItem( Sender ).Caption, '&', 1, -1 ) ) ;
    end ;
    Keyboard.Keyboard.Set_Key_Down( PChar( 'BAUD RATE ' + inttostr( Baud ) ), True ) ;
end ;


procedure TMain_Form.Bits_Check( Sender : TObject ) ;

begin
    Check_Click( Sender ) ;
    Data_Bits := strtoint( TMenuItem( Sender ).Caption ) ;
end ;


procedure TMain_Form.Keymapping1Click( Sender : TObject ) ;

begin
    Key_Mapper.Query ;
end ;


procedure TMain_Form.KSR331Click(Sender: TObject) ;

begin
    Set_Mode( HCM_KSR33 ) ;
    KSR331.Checked := True ;
end ;


procedure TMain_Form.FormKeyDown( Sender : TObject ; var Key : Word ;
    Shift : TShiftState ) ;

var _Buffer : array[ 0..2 ] of char ;
    Defined : boolean ;
    KeyStates : TKeyboardState ;

    procedure Substitute( CB : string ) ;

    var S : string ;

    begin
        S := string( Key_Mapper.Mapping( PChar( CB ) ) ) ;
        if( S <> '' ) then
        begin
            Defined := True ;
            if( length( S ) = 1 ) then
            begin
                Key := ord( S[ 1 ] ) ;
            end else
            if( S = 'SPACE' ) then
            begin
                Key := 32 ;
            end else
            if( S = 'RETURN' ) then
            begin
                Key := 13 ;
            end else
            if( S = 'DEL' ) then
            begin
                Key := 127 ;
            end else
            if( S = 'LF' ) then
            begin
                Key := 10 ;
            end else
            if( S = 'ESCAPE' ) then
            begin
                Key := ord( ESC ) ;
            end else
            if( S = 'BREAK' ) then
            begin
                _LA36.Set_Signal( 'BREAK', True ) ;
            end else
            if( S = 'TAB' ) then
            begin
                Key := 9 ;
            end else
            if( S = 'LOCK' ) then
            begin
                Keyboard.Keyboard.Set_Key_Down( 'LOCK', True ) ;
                Key := 0 ;
            end ;
        end ; // if( S <> '' )
    end ; // .Substitute

begin
    Defined := False ;
    case Key of
        VK_BACK :
            begin
                Key := 127 ;
                Defined := True ;
            end ;
        VK_F1 : Substitute( 'F1' ) ;
        VK_F2 : Substitute( 'F2' ) ;
        VK_F3 : Substitute( 'F3' ) ;
        VK_F4 : Substitute( 'F4' ) ;
        VK_F5 : Substitute( 'F5' ) ;
        VK_F6 : Substitute( 'F6' ) ;
        VK_F7 : Substitute( 'F7' ) ;
        VK_F8 : Substitute( 'F8' ) ;
        VK_F9 : Substitute( 'F9' ) ;
        VK_F10 : Substitute( 'F10' ) ;
        VK_F11 : Substitute( 'F11' ) ;
        VK_F12 : Substitute( 'F12' ) ;
        VK_CANCEL : Substitute( 'CANCEL' ) ;
        VK_CLEAR : Substitute( 'CLEAR' ) ;
        VK_PAUSE : Substitute( 'PAUSE' ) ;
        VK_PRIOR : Substitute( 'PAGE_UP' ) ;
        VK_NEXT : Substitute( 'PAGE_DOWN' ) ;
        VK_END : Substitute( 'END_' ) ;
        VK_PRINT : Substitute( 'PRINT' ) ;
        VK_INSERT : Substitute( 'INSERT' ) ;
        VK_UP : Substitute( 'UP' ) ;
        VK_DOWN : Substitute( 'DOWN' ) ;
        VK_LEFT : Substitute( 'LEFT' ) ;
        VK_RIGHT : Substitute( 'RIGHT' ) ;
    end ;
    if( not Defined ) then
    begin
        GetKeyboardState( KeyStates ) ;
        case Key of
            VK_NUMPAD0 :
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_0', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD1:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_1', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD2:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_2', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD3:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_3', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD4:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_4', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD5:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_5', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD6:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_6', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD7:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_7', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD8:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_8', 1 ) ;
                    exit ;
                end ;
            VK_NUMPAD9:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_9', 1 ) ;
                    exit ;
                end ;
            VK_SEPARATOR:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_ENTER', 1 ) ;
                    exit ;
                end ;
            VK_DECIMAL:
                begin
                    Key := 0 ;
                    TKBComponent( KB_Receiver ).Process_Key( 'NKP_.', 1 ) ;
                    exit ;
                end ;
            VK_PRIOR:
                begin
                    Key := 0 ;
                    Pageup1Click( nil ) ;
                    exit ;
                end ;
            VK_NEXT:
                begin
                    Key := 0 ;
                    Pagedown1Click( nil ) ;
                    exit ;
                end ;
        end ;
        if( ToAscii( Key, MapVirtualKey( Key, 0 ), KeyStates, _Buffer, 0 ) = 1 ) then
        begin
            Key := ord( _Buffer[ 0 ] ) ;
        end else
        begin
            case Key of
                VK_TAB : Key := 9 ;
                VK_ESCAPE : Key := ord( ESC ) ;
                VK_RETURN : Key := ord( CR ) ;
                VK_DELETE : Key := 127 ;
                VK_BACK : Key := 127 ;
                else exit ;
            end ;
        end ;
    end ;

    if( ( ( Key > 0 ) and ( Key < 27 ) )
        or
        ( pos( chr( Key ), '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!@#$%^&*()-=_+\|[]{};:<>,./?`~" '#39#$97#$9A#$81#$93#9#13#$8E#$5F+ESC ) > 0 )
      ) then
    begin
        Write_Serial( Key ) ;
        Key := 0 ;
    end ;
end ; // TMain_Form.FormKeyDown


procedure TMain_Form.Gotopage1Click(Sender: TObject) ;

begin
    if( Goto_Page_Dialog = nil ) then
    begin
        Goto_Page_Dialog := TGoto_Page_Dialog.Create( Application ) ;
    end ;
    if( Goto_Page_Dialog.ShowModal = mrOK ) then
    begin
        Displayed_Page := Goto_Page_Dialog.SpinEdit1.Value - 1 ;
        Build_Page( Displayed_Page ) ;
        Update_Status ;
    end ;
end ;


procedure TMain_Form.Gotoprinthead1Click(Sender: TObject) ;

begin
    if( Displayed_Page <> Print_Head_Page ) then
    begin
        Displayed_Page := Print_Head_Page ;
        Build_Page( Displayed_Page, True ) ;
    end ;
    Bring_Print_Head_Into_View ;
    Update_Status ;
end ;


procedure TMain_Form.HereIs1Click(Sender: TObject) ;

begin
    Here_Is_Form.CheckBox1.Checked := Here_Is_Enabled ;
    Here_Is_Form.Edit1.Text := Here_Is ;
    if( Here_Is_Form.ShowModal = mrOK ) then
    begin
        Here_Is_Enabled := Here_Is_Form.CheckBox1.Checked ;
        Here_Is := Here_Is_Form.Edit1.Text ;
    end ;
    Transmit1.Enabled := Here_Is_Enabled ;
end ; // TMain_Form.FormKeyDown


procedure TMain_Form.HorizontalTabPositions1Click(Sender: TObject) ;

var Dummy, Loop : integer ;

begin
    VT_Form.Caption := 'Horizontal tabs' ;
    VT_Form.Memo1.Text := '' ;
    for Loop := 0 to Horizontal_Tabs.Count - 1 do
    begin
        VT_Form.Memo1.Lines.Add( inttostr( Horizontal_Tabs[ Loop ] ) ) ;
    end ;
    if( VT_Form.ShowModal = mrOK ) then
    begin
        Horizontal_Tabs.Clear ;
        for Loop := 0 to VT_Form.Memo1.Lines.Count - 1 do
        begin
            Dummy := strtoint( VT_Form.Memo1.Lines[ Loop ] ) ;
            if( Horizontal_Tabs.Indexof( Dummy ) = -1 ) then
            begin
                Horizontal_Tabs.Add( Dummy ) ;
            end ;
        end ;
        Horizontal_Tabs.Sort( -1 ) ;
    end ;
end ;


procedure TMain_Form.FormDestroy( Sender : TObject ) ;

begin
    Keyboard.Terminate ;
    Keyboard := nil ;
    KB_Receiver.Free ;
    KB_Receiver := nil ;
    Output_Queue.Free ;
    Output_Queue := nil ;
    Input_Queue.Free ;
    Input_Queue := nil ;
    if( not Paper_Out ) then
    begin
        {$I-}
        closefile( FPaper ) ;
        {$I+}
        IOResult ;
    end ;
    if( Tape_File <> '' ) then
    begin
        {$I-}
        closefile( FTape ) ;
        {$I+}
        IOResult ;
    end ;
    if( Cancellogging1.Enabled ) then
    begin
        {$I-}
        closefile( Log_File ) ;
        {$I+}
        IOResult ;
    end ;
    Vertical_Tabs.Free ;
end ;


procedure TMain_Form.Options2Click( Sender : TObject ) ;

var Dummy : integer ;
    Header : TCEF_Paper_Media_Header ;

begin
    Printer_Options.Margin.Value := Margin ;
    Printer_Options.Width.Value := Paper_Width div 10 ;
    Printer_Options.Width_Modification.Value :=
        Paper_Width - Printer_Options.Width.Value * 10 ;
    Printer_Options.Height.Value := Paper_Height div 10 ;
    Printer_Options.Height_Modification.Value :=
        Paper_Height - Printer_Options.Height.Value * 10 ;
    Printer_Options.Bleed.Value := Bleed ;
    Printer_Options.Magnification.Value := Magnification ;
    if( Barred ) then
    begin
        Printer_Options.Barred_RB.Checked := True ;
    end else
    begin
        Printer_Options.Plain_RB.Checked := True ;
    end ;
    Printer_Options.Max_Pages.Value := Max_Page ;
    if( Printer_Options.ShowModal = mrOK ) then
    begin
        Margin := Printer_Options.Margin.Value ;
        Paper_Width := Printer_Options.Width.Value * 10 + Printer_Options.Width_Modification.Value ;
        Paper_Height := Printer_Options.Height.Value * 10 + Printer_Options.Height_Modification.Value ;
        if( Paper_Width < 10 ) then // Minimum paper size = 1" x 1"
        begin
            Paper_Width := 10 ;
        end ;
        Paper_Height := Printer_Options.Height.Value * 10 + Printer_Options_Dialog.Height_Modification.Value ;
        if( Paper_Height < 10 ) then
        begin
            Paper_Height := 10 ;
        end ;
        if( Printer_Options.RadioButton1.Checked and ( Printer_Options.Filename.Text <> '' ) ) then
        begin
            Load_Character_Set( Printer_Options.Filename.Text ) ;
        end else
        begin
            Load_Character_Set( Printer_Options.Windows_Font.Text ) ;
        end ;
        if( Printer_Options.RadioButton3.Checked and ( Printer_Options.Alternate_Filename.Text <> '' ) ) then
        begin
            Load_Alt_Character_Set( Printer_Options.Alternate_Filename.Text ) ;
        end else
        begin
            Load_Alt_Character_Set( Printer_Options.Alternate_Windows_Font.Text ) ;
        end ;
        Magnification := Printer_Options.Magnification.Value ;
        Image.Picture.Bitmap.Height := Margin * 2 + ( LPI * Paper_Height div 10 ) * Character_Set.Height ;
        Image.Picture.Bitmap.Width := Margin * 2 + ( CPI * Paper_Width div 10 ) * Character_Set.Width( ord( 'W' ) ) ;
        Image.Height := Image.Picture.Bitmap.Height * Magnification ;
        Image.Width := Image.Picture.Bitmap.Width * Magnification ;
        if( Print_Head_Row >= Image.Picture.Bitmap.Height - Margin * 2 ) then
        begin
            Next_Page ;
        end ;
        if( Print_Head_Char_Column > Max_Width ) then
        begin
            Print_Head_Char_Column := Max_Width ;
        end ;
        if( Print_Head_Column >= Image.Picture.Bitmap.Width - Margin * 2 ) then
        begin
            Print_Head_Column := Image.Picture.Bitmap.Width - Margin * 2 ;
        end ;
        Bleed := Printer_Options.Bleed.Value ;
        Barred := Printer_Options.Barred_RB.Checked ;
        Bar_Color := Printer_Options.Bar_Color ;
        Max_Page := Printer_Options.Max_Pages.Value ;
        if( ( not Text_File ) and ( Paper_File <> '' ) ) then // Need to update header
        begin
            try
                // Get header...
                seek( FPaper, 0 ) ;
                {$I-}
                blockread( FPaper, Header, sizeof( Header ) ) ;
                {$I+}
                Dummy := IOResult ;
                if( Dummy <> 0 ) then
                begin
                    ShowMessage( ERT( Dummy ) + ' updating paper file header' ) ;
                    exit ;
                end ;

                // Set header values...
                Header.Width := Paper_Width ;
                Header.Height := Paper_Height ;
                if( Barred ) then
                begin
                    Header.Typ := Printer_Options.Color ;
                end else
                begin
                    Header.Typ := 0 ;
                end ;
                Header.Max_Page := Max_Page ;
                Header.Bleed := Bleed ;

                // Write header...
                seek( FPaper, 0 ) ;
                {$I-}
                blockwrite( FPaper, Header, sizeof( Header ) ) ;
                {$I+}
                Dummy := IOResult ;
                if( Dummy <> 0 ) then
                begin
                    ShowMessage( ERT( Dummy ) + ' updating paper file header' ) ;
                    exit ;
                end ;
            finally
                seek( FPaper, filesize( FPaper ) ) ;
            end ;
        end ;
        Build_Page( -1 ) ;
    end ;
end ; // TMain_Form.Options2Click


procedure TMain_Form.FormClose( Sender : TObject ; var Action : TCloseAction ) ;

begin
    Action := caFree ;
end ;


procedure TMain_Form.CompressedFont1Click(Sender: TObject);

begin
    if( CompressedFont1.Checked ) then
    begin
        Load_Character_Set( 'LA36c' ) ;
    end else
    begin
        Load_Character_Set( 'LA36' ) ;
    end ;
    Character_Set := Standard_Character_Set ;
end ;


procedure TMain_Form.Connect1Click( Sender : TObject ) ;

var C : TComponent ;
    H : THandle ;
    P : function( Serial_Number : integer ; UI : TUI_Interface ) : TComponent ; stdcall ;

begin
    while( true ) do
    begin
        Open_Dialog.Title := 'Connect to a component' ;
        if( Open_Dialog.Execute ) then
        begin
            H := LoadLibrary( PChar( Open_Dialog.Filename ) ) ;
            if( H <> 0 ) then
            begin
                P := GetProcAddress( H, 'Make_Instance' ) ;
                if( assigned( P ) ) then
                begin
                    C := P( 0, _UI ) ;
                    if( C = nil ) then
                    begin
                        ShowMessage( 'Cannot load component' ) ;
                        try
                            FreeLibrary( H ) ;
                        except
                        end ;
                        continue ;
                    end ;
                    if( C.Component_Type <> Component_Type_Cable ) then
                    begin
                        ShowMessage( 'Cannot load component: not a cable component' ) ;
                        try
                            C.Terminate ;
                            FreeLibrary( H ) ;
                        except
                        end ;
                        continue ;
                    end ;
                    _LA36.Connect_Input( C ) ;
                    C.Connect_Output( _LA36 ) ;
                    _LA36.Set_Signal( 'DTR', True ) ;
                    Cable_Component := C ;
                    Cable_Component.Add_Notification( _LA36 ) ;
                end else
                begin
                    H := 0 ;
                end ;
            end else
            begin
                H := 0 ;
            end ;
            if( H = 0 ) then
            begin
                ShowMessage( 'Cannot load component' ) ;
                continue ;
            end ;
        end ; // if( Open_Dialog.Execute )
        exit ;
    end ; // while( True )
end ;


procedure TMain_Form.Logtofile1Click( Sender : TObject ) ;

var Appending : boolean ;

begin
    if( Log_File_Dialog.Execute ) then
    begin
        Appending := False ;
        if( FileExists( Log_File_Dialog.Filename ) ) then
        begin
            case MessageBox( 0, 'Do you want to overwrite the file (No to append)', 'Logging file already exists', MB_YesNoCancel ) of
                idCancel : exit ;
                idNo : Appending := True ;
            end ;
        end ;
        if( Cancellogging1.Enabled ) then
        begin
            {$I-}
            closefile( Log_File ) ;
            {$I+}
            IOResult ;
        end ;
        assignfile( Log_File, Log_File_Dialog.Filename ) ;
        if( Appending ) then
        begin
            reset( Log_File, 1 ) ;
            seek( Log_File, filesize( Log_File ) ) ;
        end else
        begin
            rewrite( Log_File, 1 ) ;
        end ;
        Cancellogging1.Enabled := True ;
    end ;
end ; // TMain_Form.Logtofile1Click


procedure TMain_Form.Master1Click(Sender: TObject) ;

begin
    Master := Master1.Checked ;
    Address_Mode := False ;
    Transmit_Disabled := False ;
    Unselect_Locked := False ;
    Waiting_For_Lock := False ;
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_LED_State( 'DEVICE SELECT', Master ) ;
    end ;
end ;


procedure TMain_Form.Selected1Click(Sender: TObject) ;

begin
    Select_Device( Selected1.Checked ) ;
end ;


procedure TMain_Form.Select_Device( Selected : boolean ) ;

begin
    if( Keyboard <> nil ) then
    begin
        Keyboard.Keyboard.Set_LED_State( 'DEVICE SELECT', Selected ) ;
    end ;
    Selected1.Checked := Selected ;
end ;


procedure TMain_Form.Sendfile1Click( Sender : TObject ) ;

var Dlg : TSend_Dialog ;

begin
    Dlg := TSend_Dialog.Create( Application ) ;
    Dlg.Main_Form := self ;
    Dlg.ShowModal ;
    Dlg.Free ;
end ;


procedure TMain_Form.Cancellogging1Click( Sender : TObject ) ;

begin
    {$I-}
    closefile( Log_File ) ;
    {$I+}                                                            
    IOResult ;
    Cancellogging1.Enabled := False ;
end ;


procedure TMain_Form.Uppercaseoutput1Click( Sender : TObject ) ;

begin
    TMenuItem( Sender ).Checked := not TMenuItem( Sender ).Checked ;
    _Uppercase_Out := Uppercase1.Checked ;
end ;


procedure TMain_Form.UseAlternateCharacterSet1Click(Sender: TObject) ;

begin
    Set_Alt_Character_Set ;
end ;


procedure TMain_Form.UseStandardCharacterSet1Click(Sender: TObject) ;

begin
    Set_Standard_Character_Set ;
end ;


procedure TMain_Form.Uppercaseinput1Click( Sender : TObject ) ;

begin
    TMenuItem( Sender ).Checked := not TMenuItem( Sender ).Checked ;
    _Uppercase_In := Uppercaseinput1.Checked ;
end ;


procedure TMain_Form.ChangeKeyboard1Click( Sender : TObject ) ;

begin
    while( true ) do
    begin
        Open_Dialog.Title := 'Select a keyboard' ;
        if( Open_Dialog.Execute ) then
        begin
            if( Load_Keyboard( Open_Dialog.Filename, False ) ) then
            begin
                exit ;
            end ;
        end ; // if( Open_Dialog.Execute )
        exit ;
    end ; // while( True )
end ;


procedure TMain_Form.Keyboard1Click( Sender : TObject ) ;

begin
    Keyboard1.Checked := not Keyboard1.Checked ;
    Panel2.Visible := Keyboard1.Checked ;
    if( Panel2.Visible ) then
    begin
        Height := Height + Panel2.Height ;
    end else
    begin
        Height := Height - Panel2.Height ;
    end ;
end ;



procedure TMain_Form.ConnecttoEmulatorport1Click( Sender : TObject ) ;

var C : TComponent ;

begin
    C := Choose_Emulator_Port( _UI ) ;
    if( C <> nil ) then
    begin
        _LA36.Connect_Input( C ) ;
        C.Connect_Output( _LA36 ) ;
        _LA36.Set_Signal( 'DTR', True ) ;
        Cable_Component := C ;
        Cable_Component.Add_Notification( _LA36 ) ;
    end ;
end ;


procedure TMain_Form.DECLA301Click(Sender: TObject);

begin
    Set_Mode( HCM_LA30 ) ;
    DECLA301.Checked := True ;
end ;


procedure TMain_Form.Disconnect1Click(Sender: TObject);

begin
    if( Cable_Component <> nil ) then
    begin
        Cable_Component.Disconnect_Input( _LA36 ) ;
        _LA36.Disconnect_Output( Cable_Component ) ;
        Cable_Component := nil ;
    end ;
end ;


procedure TMain_Form.TY1Click(Sender: TObject);

begin
    Set_Mode( HCM_ASR33 ) ;
    TY1.Checked := True ;
end ;


procedure TMain_Form.VT051Click(Sender: TObject);

begin
    Set_Mode( HCM_LA36 ) ;
    VT051.Checked := True ;
end ;


procedure TMain_Form.Screen_PanelResize( Sender : TObject ) ;

begin
    Build_Page( -1 ) ;
end ;


procedure TMain_Form.About1Click( Sender : TObject ) ;

begin
    About_Form.ShowModal ;
end ;



end.
