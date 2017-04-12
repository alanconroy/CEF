{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 main form
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

        23-Mar-2006   AC          Open all input files in read mode.
                                  Implement Dump.
                                  Can now load/dump *.ent files.
        14-Oct-2007   AC          Retain CPU status position on update.
                                  Dialog for changing registers more accurately
                                  restricts the range of valid values.
                                  Changing registers whose length is less than 8
                                  bits now works.
                                  When changing data in the CPU status pane,
                                  only the numeric value is shown in the dialog.
                                  When a CPU component is loaded, the memory
                                  display now defaults to the default base of
                                  the CPU and the disassembly tab updates
                                  properly.
                                  Disassembly pane shows data properly in all
                                  bases.
                                  Disassembly pane scrollbar positions correctly.
                                  Fixed various other problems with the
                                  Disassembly pane.
                                  Pressing the UP arrow key when the immediate
                                  mode edit box is focused will load the edit
                                  box with the last immediate mode command.
                                  Fixed Delete All execution breakpoints from
                                  dialog.
                                  Memory pane popup menu supports "Pattern".
                                  Reworked Modify Memory dialog.
                                  Added Assemble | Show Errors.
                                  Added "Emulator ports" item on Components menu.
                                  File | Close and Assemble | Assemble are only
                                  enabled when a tab with source is focused.
                                  Other fixes.
        8-Jun-2008    AC          Combined assembly status and error dialogs
                                  into a single dialog.
                                  Fixed problems with numbers sometimes
                                  displaying off by one.
                                  Watch values now show in the specified base.
                                  Address spaces now show in memory view popup.
                                  UI properly handles sparse overlapping memory
                                  devices.
                                  Fix abend in File | Save As when used in
                                  Disassembly.
                                  File | Save disabled when in Disassembly.
                                  Other fixes.
        8-Jul-2010    AC          Added Media Manager.
                                  Localization changes.
        11-Mar-2015   AC          Handle Reopen menus better.
                                  Added new Load Component dialog.
                                  Support clipboard in CPU popup menu.
                                  Fixed exceptions when closing editor tabs.
                                  Prevent Disassembly tab from being closed with
                                  menu shortcut.
                                  Fix 64-bit address range handling by Memory
                                  tab scrollbar.
                                  Port tab range now reflects master CPU port
                                  range.
                                  Support master CPU selection in menu.
                                  Other V2.6 specification-related changes.

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This unit defines the main form for the generic CEF user interface.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CEFMain ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     ExtCtrls, Menus, ComCtrls, StdCtrls,

     // C&C...
     ArrayDis, // TArray_Display
     ArrayInt, // TArray_Interface
     CommonUt, // TInteger_List
     EditWin, // tEditor_Window
     _Streams, // TCOM_Stream
     SStreams, // TCOM_String_Stream
     Standard, // TInteger64_List
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TAssembler_Status
     CEF, // TBase_Component
     _CEFUtil, // TCEF_Watchpoint_Manager

     // CEF32...
     Conditions_Dlg ; // Conditions_Form


type TMemory_Array_Interface = class( TCOM_Array_Interface )
                                   public // API...
                                       function Is_Class( _N : PChar ) : boolean ;
                                           override ;
                                       function Get_Byte( Index : int64 ) : byte ;
                                           override ;
                                       procedure Set_Byte( Index : int64 ;
                                           Value : byte ) ; override ;

                                       function Read_Only : boolean ; override ;

                                       function Low_Bound( Subscript : integer ) : int64 ;
                                           override ;
                                       function High_Bound( Subscript : integer ) : int64 ;
                                           override ;
                                       function Subscripts : integer ;
                                           override ;
                               end ;

     TPort_Array_Interface = class( TCOM_Array_Interface )
                                   public // API...
                                       function Is_Class( _N : PChar ) : boolean ;
                                           override ;
                                       function Get_Byte( Index : int64 ) : byte ;
                                           override ;
                                       procedure Set_Byte( Index : int64 ;
                                           Value : byte ) ; override ;

                                       function Read_Only : boolean ; override ;

                                       function Low_Bound( Subscript : integer ) : int64 ;
                                           override ;
                                       function High_Bound( Subscript : integer ) : int64 ;
                                           override ;
                                       function Subscripts : integer ;
                                           override ;
                               end ;

type TLine_Streamer = class( TCOM_String_Stream )
                          public
                              Current_Line : integer ;
                      end ;

type TCEF_Assembler_Status = class( TAssembler_Status )
                                 public // Constructors and destructors...
                                     constructor Create ;
                                     destructor Destroy ; override ;

                                 private
                                     _Aborted : boolean ;
                                     _Code : int64 ;
                                     _Data : int64 ;
                                     _Warnings : longint ;
                                     _Errors : longint ;
                                     _Error_Text : string ;
                                     Last_File : string ;
                                     Line : integer ;
                                     Paging : boolean ;

                                     // Listing file...
                                     Page, Page_Line : integer ;
                                     Listing_File : textfile ;
                                     Title : string ;
                                     SubTitle : string ;
                                     Allow_Listing : boolean ;
                                     Data_String : string ;

                                 public // API...
                                     Error_List : TStringList ;
                                     Current_File : string ;
                                     In_Streamer : TLine_Streamer ;

                                     procedure Update_Display ;

                                     function Filename : PChar ; override ;
                                     function Get_Code : int64 ; override ;
                                     function Get_Data : int64 ; override ;
                                     function Get_Errors : integer ; override ;
                                     function Get_Warnings : integer ;
                                         override ;
                                     function Get_Error_Text : PChar ;
                                         override ;
                                     procedure Set_Code( Value : int64 ) ;
                                         override ;
                                     procedure Set_Data( Value : int64 ) ;
                                         override ;
                                     procedure Set_Errors( Value : integer ) ;
                                         override ;
                                     procedure Set_Warnings( Value : integer ) ;
                                         override ;
                                     procedure Set_Error_Text( Value : PChar ) ;
                                         override ;
                                     procedure Log_Error( Text, Filename : PChar ;
                                         Line, Severity : longint ) ; override ;
                                     procedure Get_Error( Index : longint ;
                                         var Text, Filename : PChar ;
                                         var Line, Severity : longint ) ;
                                         override ;
                                     procedure Set_Aborted( Value : boolean ) ;
                                         override ;
                                     function Get_Aborted : boolean ; override ;
                                     procedure Set_Line( Value : integer ) ;
                                         override ;
                                     procedure Output_To_Listing( Text : PChar ;
                                         Text_Type : integer ) ; override ;
                             end ; // TCEF_Assembler_Status

// TPaint_Panel...

type TPaint_Panel = class( TPanel )
                        public
                            procedure wmPaint( var Message ) ;
                                message WM_Paint ;
                    end ;


     TCEF32_Interface = class( TBase_UI_Interface )
                            public // Constructors and destructors...
                                constructor Create ;
                                destructor Destroy ; override ;

                            private // Instance data...
                                _Clock : TMaster_Clock ;
                                Signal_Components : TList ;
                                Threads : TInteger_List ;
                                Thread_ID : integer ; // Next thread ID to assign
                                Temp : string ;

                            protected // Internal utility routines...
                                function Determine_Port_Component_Index( var Index : longint ) : TComponent ;

                            public // API...
                                procedure Idle( Component : TComponent ) ;
                                    override ;
                                function Breakpoint_Notice( Address : int64 ;
                                    Physical : boolean ; Space : integer ;
                                    CPU : TComponent ) : boolean ; override ;
                                function Clock : TMaster_Clock ; override ;
                                procedure Log_Error( Component : TComponent ;
                                    Text : PChar ; Severity : longint ) ;
                                    override ;
                                procedure Log_Simulated_Error( Component : TComponent ;
                                    Text : PChar ; Severity : longint ) ;
                                    override ;
                                procedure State_Change_Notice( Component : TComponent ;
                                    Index : longint ; Active : boolean ) ;
                                    override ;
                                procedure Signal_Change_Notice( Component : TComponent ;
                                    Index : longint ; Active : boolean ) ;
                                    override ;
                                procedure Signal_Exception( Component : TComponent ;
                                    Description : PChar ; Index : longint ) ;
                                    override ;
                                procedure Watchpoint_Notice( Address : int64 ;
                                    Access, Tag : longint ; Component : TComponent ;
                                    Memory, Internal, Port : boolean ) ;
                                    override ;
                                procedure Log_Trace( Component : TComponent ;
                                    Description : PChar ) ; override ;
                                function Get_File_Stream( Name : PChar ) : TCOM_Stream ;
                                    override ;
                                procedure Toggle_Embed( Component : TComponent ) ;
                                    override ;
                                procedure Want_Signals( Component : TComponent ;
                                    Value : boolean ) ; override ;
                                procedure Terminate ; override ;
                                procedure Hide( Flag : boolean ) ; override ;
                                function Get_Port_Name( Index : longint ) : PChar ;
                                    override ;
                                function Get_Port_Description( Index : longint ) : PChar ;
                                    override ;
                                function Get_Port( Index : longint ) : TComponent ;
                                    override ;
                                function Get_Port_Connection( Index : longint ) : TComponent ;
                                    override ;
                                function Port_Parent_Component( Index : longint ) : TComponent ;
                                    override ;
                                procedure Run( State : boolean ) ; override ;
                                function Process_ID( Name : PChar ; Force : boolean ) : integer ;
                                    override ;
                                function Process_Start( ID : longint ;
                                    var Priority : longint ) : boolean ; override ;
                                procedure Process_Deleted( ID : longint ) ;
                                    override ;
                                procedure Add_Port_Breakpoint ; override ;
                                procedure Add_Breakpoint ; override ;
                                procedure Add_Register_Breakpoint ; override ;
                                procedure Create_New_Breakpoint ; override ;
                                function Get_Component_Filename( Name : PChar ) : PChar ;
                                    override ;
                                procedure Termination_Notice( C : TComponent ) ;
                                    override ;
                                function Load_Component( Name : PChar ) : TComponent ;
                                    override ;
                        end ; // TCEF32_Interface

type TCEF_TabSheet = class( TTabSheet )
                         public // API...
                             Editor : tEditor_Window ; // nil if not an editor
                             Disassembly : boolean ; // True if a disassembly sheet
                             UI : TComponent ;

                             procedure SetBounds( ALeft, ATop, AWidth, AHeight : Integer ) ;
                                 override ;
                     end ;

type TComponent_Parent = class( TBase_Component )
                             public // API...
                                 procedure Child_Notification( Child : TComponent ;
                                     var Notice : longint ; var Params : int64 ) ;
                                     override ;
                         end ;

type TMemory_Tab = class ;

     TMemory_Tab_Array_Interface = class( TMemory_Array_Interface )
                                       public // Constructors and destructors...
                                           constructor Create ;
                                           destructor Destroy ; override ;

                                       private // Instance data...
                                           _Components : TList ;
                                           _Low, _High : int64 ;

                                       public // API...
                                           Tab : TMemory_Tab ;

                                       public // API...
                                           procedure Add( C : TComponent ) ;

                                           function Get_Byte( Index : int64 ) : byte ;
                                               override ;
                                           procedure Set_Byte( Index : int64 ;
                                               Value : byte ) ; override ;

                                           function Read_Only : boolean ; override ;

                                           function Low_Bound( Subscript : integer ) : int64 ;
                                               override ;
                                           function High_Bound( Subscript : integer ) : int64 ;
                                               override ;
                                           function Subscripts : integer ;
                                               override ;
                                   end ;

     TMemory_Tab = class
                       public // Constructors and destructors...
                           constructor Create ;
                           destructor Destroy ; override ;

                       private // Instance data...
                           _Components : TList ;

                       public // API...
                           AI : TCOM_Array_Interface ;
                           AD : TArray_Display ;
                           Name : string ;

                           procedure Add( C : TComponent ) ;
                           procedure Remove( C : TComponent ) ;
                           function Count : integer ;
                           function Has_Component( C : TComponent ) : boolean ;
                           procedure Set_Watch( Address : int64 ; M : boolean ; Access : longint ) ;
                   end ;

type TMain_Form = class( TForm )
    Top_panel : TPanel ;
    Bottom_Panel : TPanel ;
    Main_Splitter : TSplitter ;
    MainMenu1: TMainMenu ;
    File1 : TMenuItem ;
    Help1 : TMenuItem ;
    Help2 : TMenuItem ;
    About1 : TMenuItem ;
    Exit1 : TMenuItem ;
    Open1 : TMenuItem ;
    Open_Dialog : TOpenDialog ;
    Watch_Panel : TPanel ;
    Splitter1 : TSplitter ;
    StatusBar: TStatusBar;
    Internals1: TMenuItem;
    Popup_Menu: TPopupMenu;
    Radix1: TMenuItem;
    Signed1: TMenuItem;
    Size1: TMenuItem;
    EBCDIC1: TMenuItem;
    Binary1: TMenuItem;
    Octal1: TMenuItem;
    Decimal1: TMenuItem;
    Hexadecimal1: TMenuItem;
    Other1: TMenuItem;
    Byte1: TMenuItem;
    Word1: TMenuItem;
    Long1: TMenuItem;
    Double1: TMenuItem;
    N1: TMenuItem;
    Gotoaddress1: TMenuItem;
    Find1: TMenuItem;
    Radix50: TMenuItem;
    Physical1: TMenuItem;
    Save_Memory_Dialog: TSaveDialog;
    Save_Memory_State_Dialog: TSaveDialog;
    N2: TMenuItem;
    Save1: TMenuItem;
    Restore1: TMenuItem;
    N3: TMenuItem;
    Save2: TMenuItem;
    Restore2: TMenuItem;
    PageControl1: TPageControl;
    CPU_Panel: TPanel;
    Splitter2: TSplitter;
    N4: TMenuItem;
    Save3: TMenuItem;
    SaveAs1: TMenuItem;
    SaveAll1: TMenuItem;
    New1: TMenuItem;
    N5: TMenuItem;
    OpenEmulator1: TMenuItem;
    Save_Dialog: TSaveDialog;
    Restore_Memory_Dialog: TOpenDialog;
    Restore_memory_State_Dialog: TOpenDialog;
    Components1: TMenuItem;
    DefaultMemory1: TMenuItem;
    Assemble1: TMenuItem;
    Assemble2: TMenuItem;
    AssembleAll1: TMenuItem;
    N7: TMenuItem;
    Printersetup1: TMenuItem;
    Print1: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    N8: TMenuItem;
    Close1: TMenuItem;
    CloseAll1: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N51: TMenuItem;
    N61: TMenuItem;
    N71: TMenuItem;
    N81: TMenuItem;
    N91: TMenuItem;
    N10: TMenuItem;
    Load1: TMenuItem;
    N20: TMenuItem;
    N12: TMenuItem;
    N22: TMenuItem;
    N32: TMenuItem;
    N42: TMenuItem;
    N52: TMenuItem;
    N62: TMenuItem;
    N72: TMenuItem;
    N82: TMenuItem;
    N92: TMenuItem;
    CPU_Status: TListBox;
    CPU_Caption: TPanel;
    Modify1: TMenuItem;
    CPU_PopupMenu: TPopupMenu;
    Watchpoints1: TMenuItem;
    Createnew1: TMenuItem;
    View1: TMenuItem;
    Run1: TMenuItem;
    Execute1: TMenuItem;
    StepOver1: TMenuItem;
    StepInto1: TMenuItem;
    Runimmediate1: TMenuItem;
    UnloadAll1: TMenuItem;
    Reopenemulator1: TMenuItem;
    Reopensource1: TMenuItem;
    N13: TMenuItem;
    N23: TMenuItem;
    N33: TMenuItem;
    N43: TMenuItem;
    N53: TMenuItem;
    N63: TMenuItem;
    N73: TMenuItem;
    N83: TMenuItem;
    N93: TMenuItem;
    Panel1: TPanel;
    Immediate_Mode_Panel: TPanel;
    Label1: TLabel;
    Immediate_Mode_Edit: TEdit;
    Pause1: TMenuItem;
    Increment1: TMenuItem;
    Decrement1: TMenuItem;
    Change1: TMenuItem;
    Zero1: TMenuItem;
    AddWatch1: TMenuItem;
    AddBreakpoint1: TMenuItem;
    AddRegisterBreakpoint1: TMenuItem;
    N6: TMenuItem;
    RegisterBreakpoints1: TMenuItem;
    Watch_List_Box: TListBox;
    Watches_Popup_Menu: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    DeleteAll1: TMenuItem;
    N9: TMenuItem;
    Edit1: TMenuItem;
    AddPortBreakpoint1: TMenuItem;
    N14: TMenuItem;
    PortBreakpoints1: TMenuItem;
    ExecutionBreakpoints1: TMenuItem;
    N15: TMenuItem;
    Profiling1: TMenuItem;
    ProfileReport1: TMenuItem;
    N16: TMenuItem;
    Trace1: TMenuItem;
    TraceLog1: TMenuItem;
    AddressSpace1: TMenuItem;
    Disassembly_Popup: TPopupMenu;
    Showsource1: TMenuItem;
    Stack_List_Box: TListBox;
    Splitter3: TSplitter;
    Gotoaddress2: TMenuItem;
    GotoCurrent1: TMenuItem;
    Stack_Popup: TPopupMenu;
    TopofStack1: TMenuItem;
    Gotoaddress3: TMenuItem;
    CPU_State: TListBox;
    Splitter4: TSplitter;
    Open_Source_Dialog: TOpenDialog;
    Options2: TMenuItem;
    Pattern1: TMenuItem;
    N17: TMenuItem;
    ShowErrors: TMenuItem;
    Emulatorport1: TMenuItem;
    N18: TMenuItem;
    Default1: TMenuItem;
    Ignore1: TMenuItem;
    Synchronize1: TMenuItem;
    Port_Popup_Menu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem20: TMenuItem;
    Read1: TMenuItem;
    Output1: TMenuItem;
    Disable1: TMenuItem;
    N19: TMenuItem;
    Unblockall1: TMenuItem;
    N24: TMenuItem;
    CEFSpecification1: TMenuItem;
    ComponentHelp1: TMenuItem;
    MediaManager1: TMenuItem;
    ClientServer1: TMenuItem;
    Server1: TMenuItem;
    Panel2: TPanel;
    Memory_ScrollBar: TScrollBar;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    N25: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    SelectCPU: TMenuItem;
    N26: TMenuItem;
    procedure Server1Click(Sender: TObject);
    procedure About1Click( Sender : TObject ) ;
    procedure Help2Click( Sender : TObject ) ;
    procedure Exit1Click( Sender : TObject ) ;
    procedure Open1Click( Sender : TObject ) ;
    procedure Memory_BoxPaint( Sender: TObject ) ;
    procedure FormShow( Sender : TObject ) ;
    procedure FormDestroy( Sender : TObject ) ;
    procedure FormCreate( Sender : TObject ) ;
    procedure Internals1Click(Sender: TObject);
    procedure EBCDIC1Click(Sender: TObject);
    procedure Signed1Click(Sender: TObject);
    procedure Binary1Click(Sender: TObject);
    procedure Octal1Click(Sender: TObject);
    procedure Decimal1Click(Sender: TObject);
    procedure Hexadecimal1Click(Sender: TObject);
    procedure Other1Click(Sender: TObject);
    procedure Byte1Click(Sender: TObject);
    procedure Word1Click(Sender: TObject);
    procedure Long1Click(Sender: TObject);
    procedure Double1Click(Sender: TObject);
    procedure Gotoaddress1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Radix50Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Restore1Click(Sender: TObject);
    procedure Save2Click(Sender: TObject);
    procedure Restore2Click(Sender: TObject);
    procedure Physical1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Save3Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure SaveAll1Click(Sender: TObject);
    procedure OpenEmulator1Click(Sender: TObject);
    procedure Assemble2Click(Sender: TObject);
    procedure AssembleAll1Click(Sender: TObject);
    procedure Printersetup1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure CloseAll1Click(Sender: TObject);
    procedure Reopen_File(Sender: TObject);
    procedure Reopen_Emulator(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CPU_StatusDblClick(Sender: TObject);
    procedure Modify1Click(Sender: TObject);
    procedure Createnew1Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure Execute1Click(Sender: TObject);
    procedure StepOver1Click(Sender: TObject);
    procedure StepInto1Click(Sender: TObject);
    procedure Runimmediate1Click(Sender: TObject);
    procedure UnloadAll1Click(Sender: TObject);
    procedure Immediate_Mode_PanelResize(Sender: TObject);
    procedure Immediate_Mode_EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Pause1Click(Sender: TObject);
    procedure Increment1Click(Sender: TObject);
    procedure Decrement1Click(Sender: TObject);
    procedure Change1Click(Sender: TObject);
    procedure CPU_StatusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Zero1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure AddWatch1Click(Sender: TObject);
    procedure AddBreakpoint1Click(Sender: TObject);
    procedure AddRegisterBreakpoint1Click(Sender: TObject);
    procedure RegisterBreakpoints1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure DeleteAll1Click(Sender: TObject);
    procedure Watch_List_BoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Watch_List_BoxDblClick(Sender: TObject);
    procedure Watch_List_BoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ExecutionBreakpoints1Click(Sender: TObject);
    procedure AddPortBreakpoint1Click(Sender: TObject);
    procedure PortBreakpoints1Click(Sender: TObject);
    procedure Clear_Watches ;
    procedure Profiling1Click(Sender: TObject);
    procedure ProfileReport1Click(Sender: TObject);
    procedure TraceLog1Click(Sender: TObject);
    procedure Trace1Click(Sender: TObject);
    procedure Showsource1Click(Sender: TObject);
    procedure GotoCurrent1Click(Sender: TObject);
    procedure Gotoaddress2Click(Sender: TObject);
    procedure TopofStack1Click(Sender: TObject);
    procedure Gotoaddress3Click(Sender: TObject);
    procedure Options2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Pattern1Click(Sender: TObject);
    procedure ShowErrorsClick(Sender: TObject);
    procedure Emulatorport1Click(Sender: TObject);
    procedure Memory_ScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure Default1Click(Sender: TObject);
    procedure Ignore1Click(Sender: TObject);
    procedure Synchronize1Click(Sender: TObject);
    procedure GotoPortClick(Sender: TObject);
    procedure Read1Click(Sender: TObject);
    procedure Output1Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure Disable1Click(Sender: TObject);
    procedure Unblockall1Click(Sender: TObject);
    procedure CEFSpecification1Click(Sender: TObject);
    procedure ComponentHelp1Click(Sender: TObject);
    procedure MediaManager1Click(Sender: TObject);
    procedure Popup_MenuPopup(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure SelectCPUClick(Sender: TObject);
    procedure N24Click(Sender: TObject);

  private // Instance data...
      Remove_Component_List : TList ;
      Component_List : TStringList ;
      Current_Address_Space : integer ;
      Disassembly_Sheet : TCEF_TabSheet ;
      Disassembly_List_Box : TListBox ;
      Disassembly_Top, Disassembly_Next, Disassembly_Second : int64 ;
      _Hide : boolean ; // True to hide form on startup
      In_Assembly : boolean ;
      In_Execution : boolean ;
      Last_Editor : TEditor_Window ;
      Last_Immediate_Mode_Edit_Text : string ;
      _Master_Assembler : TMaster_Assembler ;
      Master_CPU : TComponent ;
      Memory_DLL : integer ; // DLL handle for default main memory
      Parent_Component : TComponent_Parent ;
      Reopen_Menus : array[ 1..9 ] of TMenuItem ;
      EReopen_Menus : array[ 1..9 ] of TMenuItem ;
      Physical_Address : boolean ;
      Register_Watchpoints : TCEF_Watchpoint_Manager ;
      Execution_Watchpoints : TCEF_Watchpoint_Manager ;
      Port_Watchpoints : TCEF_Watchpoint_Manager ;
      Start_Execution : boolean ;
      Status : TCEF_Assembler_Status ;
      Disassembly_Scrollbar : TScrollbar ;
      Range_Shift : integer ; // How many bits the memory scrollbar range had to be shifted to make it fit in a longint
      Stack_Top : int64 ;
      Startup_Command_File : boolean ;
      Terminate_Requested : boolean ;
      Terminate_Base_Time : cardinal ;
      Terminate_Wait : boolean ;
      TabSheet_1 : TCEF_TabSheet ;
      Traces : TStringList ; // Trace queue
      Trace_Position : integer ; // Current insertion position for trace queue
      UI : TCEF32_Interface ;
      Watches : TList ;
      Watchpoints : TCEF_Watchpoint_Manager ; // Memory watchpoints
      Write_Errors : boolean ;

      function phGet_Master_Assembler : TMaster_Assembler ;
      procedure phSet_Master_Assembler( Value : TMaster_Assembler ) ;

      property Master_Assembler : TMaster_Assembler
          read phGet_Master_Assembler
          write phSet_Master_Assembler ;

  private // Internal utility routines...
      function _Load_Component( Filename : string ; UI : TUI_Interface ) : TComponent ;
      procedure Add_To_Reopen( const S : string ) ;
      function Address_Size : integer ;
      function Assemble( Editor : TEditor_Window ;
          Status : TCEF_Assembler_Status ) : boolean ;
      function Configure_Component_Menus( C : TComponent ;
          M : TMenuItem ) : TMenuItem ;
      function Create_Assembler : TMaster_Assembler ;
      procedure Create_Memory_Tab( C : TComponent ; Domain : string ) ;
      procedure Open_Emulator( Filename : string ) ;
      procedure Open_File( Name : string ) ;
      procedure ConfigureClick( Sender : TObject ) ;
      procedure ConnectionsClick( Sender : TObject ) ;
      procedure ConditionsClick( Sender : TObject ) ;
      procedure Disassembly_List_BoxKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
      procedure Disassembly_List_BoxMouseUp(Sender: TObject;
        Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure Disassembly_List_Resize( Sender : TObject ) ;
      function Domain_Index( const Domain : string ) : integer ;
      function Get_Disassembly( var Address : int64 ; Physical : int64 ;
          Show_Address, Show_Value : boolean ; _Assembler : TAssembler ) : string ;
      function Get_File_Stream( Name : string ) : TCOM_Stream ;
      procedure ProfileClick( Sender : TObject ) ;
      procedure ProfileReportClick( Sender : TObject ) ;
      procedure Remove_Component( Index : integer ; _Free : boolean ) ;
      procedure Update_CPU_State( Forced : boolean ) ;
      procedure Update_Watches ;
      procedure UnloadClick( Sender : TObject ) ;
      function Load_Component( Filename, Setup, Name, Domain : string ) : boolean ;
      procedure Log_Trace( Component : TComponent ; Description : string ) ;
      procedure Set_Number_Size( Size : integer ) ;
      procedure Update_Disassembly ;
      procedure Update_Stack ;
      procedure Map_Source( Force : boolean ) ;
      procedure Report_Profile( Component : TComponent ) ;
      procedure Reset1Click( Sender : TObject ) ;
      procedure Restart1Click( Sender : TObject ) ;
      procedure Disassembly_ScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode;
        var ScrollPos: Integer);
      procedure ToggleEmbedClick( Sender : TObject ) ;
      procedure Set_Master_CPU( CPU : TComponent ) ;
      function _Examine_Memory( Address : int64 ;
          Physical_Address : boolean ; Address_Space : integer ) : byte ;
      procedure Dump1Click( Sender : TObject ) ;
      procedure LoadMem1Click( Sender : TObject ) ;
      function Current_Tab : TMemory_Tab ;
      function Memory_Tab_With_Component( Component : TComponent ) : TMemory_Tab ;
      procedure Termination_Notice( C : TComponent ) ;

  private // Callbacks...
      procedure CB_Address_Space_Click( Sender : TObject ) ;
      procedure CB_Update_Menu( Sender : TObject ) ;
      procedure CB_Kill( Sender : TObject )  ;
      procedure CB_State_Change( Sender : TObject ) ;
      procedure CB_Idle( Sender : TObject ; var Done : boolean ) ;

  public // API...
      Low_Address, High_Address : int64 ;
      Low_Physical_Address, High_Physical_Address : int64 ;
      Low_Port_Address, High_Port_Address : int64 ;
      Main_Memory : TComponent ;
      Paint_Panel, Port_Paint_Panel : TPaint_Panel ;
      Memory_Tabs : TList ;
      Main_Memory_Tab : TMemory_Tab ;
      Port_Tab : TMemory_Tab ;

      procedure Write_Memory( Address : int64 ; Value : byte ) ;
      function Deposit_Memory( Address : int64 ; Value : byte ) : TUnified_Exception ;
      function Examine_Memory( Address : int64 ) : byte ;
      function Deposit_Port( Address : int64 ; Value : byte ) : TUnified_Exception ;
      function Examine_Port( Address : int64 ) : byte ;
      procedure Show_Error( Sender : TListBox ) ;
      function Menu_For_Component( Component : TComponent ) : TMenuItem ;
  end ; // TMain_Form

var Main_Form : TMain_Form ;

implementation

uses // Borland...
     Clipbrd, // Clipboard
     ShellAPI, // ShellExecute
     Spin, // TSpinEdit

     // Other...
     _ASCII, // CR
     Choose_Emulator_Port_Dlg, // Choose_Emulator_Port_Form
     CVT, // CVTB
     Debug_Dialog, // Begin_Debugger
     EBCDICs, // From_EBCDIC
     Filestd, // Qualify_Filename
     Radix_Dialog,
     Goto_Address,
     Find_Data_Dialog,
     FStreams, // Create_File_Stream
     L10n, // Substitute2
     Num1s, // Num1
     O_S, // OS
     Parse, // TString_Parser
     Radix50s, // Rad
     SaveEdit, // Save_Modified_Dialog
     TypeDefs, // TTri_State
     UE, // ERT
     UStrings, // Trim_Quotes
     VCL_Std, // Delete_All_Children

     // CEF32...
     About,
     CEFUtil_Int,
     CEFUtil,
     Clock, // TCEF_Generic_Clock
     ModReg,
     ModMem,
     ModConfig,
     Assembly_Status,
     DisMem, // TSave_Disassembly_Form
     Error_List,
     New_Watch, // New_Watch_Dialog
     Trace_Dlg, // Trace_Form
     Trace_Log_Dlg, // Trace_Log_Form
     CCM,
     Profile_Report,
     DumpMem, // TDump_Memory_Form
     LoadMem,
     Options, // Options_Form
     Pattern_Form, // Pattern_Dialog
     TextStrs, // Text_*
     Port_IO_Form,
     Load_Component_Dialog, // Load_Component_Form
     Media_Manager_Dialog,
     Value_Edit_Dialog, // Edit_Value_Form
     Server_Form,
     CPU_Selection_Dialog ;

{$R *.DFM}

type TDefault_Data_Type = class( TData_Type )
                              public // API...
                                  _Endian : boolean ;
                                  _Size : longint ;

                                  // General...
                                  function Family : longint ; override ;
                                  function Data_Type : longint ; override ;
                                  function Size : longint ; override ;
                                  function Big_Endian : boolean ; override ;
                                  function Max_Size : longint ; override ;

                                  // Numeric types...
                                  function Signed : TTri_State ; override ;
                                  function Mantissa : longint ; override ;
                                  function Exponent : longint ; override ;
                                  function Fixed : boolean ; override ;
                                  function Fixed_Position : longint ; override ;
                                  function Pack : boolean ; override ;

                                  // String types...
                                  function Length_Encoding : longint ; override ;
                                  function Prefix_Size : longint ; override ;
                                  function Encoding : longint ; override ;
                          end ;

// API...

function TDefault_Data_Type.Family : longint ;

begin
    Result := DataType_Family_Numeric ;
end ;


function TDefault_Data_Type.Data_Type : longint ;

begin
    Result := DataType_Integer ;
end ;


function TDefault_Data_Type.Size : longint ;

begin
    Result := _Size ;
end ;


function TDefault_Data_Type.Big_Endian : boolean ;

begin
    Result := _Endian ;
end ;


function TDefault_Data_Type.Max_Size : longint ;

begin
    Result := Size ;
end ;


function TDefault_Data_Type.Signed : TTri_State ;

begin
    Result := TS_Dont_Care ; // Could be either
end ;


function TDefault_Data_Type.Mantissa : longint ;

begin
    Result := _Size ;
end ;


function TDefault_Data_Type.Exponent : longint ;

begin
    Result := 0 ;
end ;


function TDefault_Data_Type.Fixed : boolean ;

begin
    Result := False ;
end ;


function TDefault_Data_Type.Fixed_Position : longint ;

begin
    Result := 0 ;
end ;


function TDefault_Data_Type.Pack : boolean ;

begin
    Result := False ;
end ;


function TDefault_Data_Type.Length_Encoding : longint ;

begin
    Result := 0 ;
end ;


function TDefault_Data_Type.Prefix_Size : longint ;

begin
    Result := 0 ;
end ;


function TDefault_Data_Type.Encoding : longint ;

begin
    Result := 0 ;
end ;



var _Info : TDefault_Data_Type = nil ;

function Register_Information( CPU : TCPU ; Index : integer ) : _CEF.TData_Type ;

var S : integer ;

begin
    if( CPU.Version > 25 ) then // V2.6+
    begin
        Result := CPU.Register_Information( Index ) ;
        exit ;
    end ;

    // Handle older model CPU components...
    S := CPU.Register_Size( Index ) ;
    if( S = 0 ) then
    begin
        Result := nil ;
        exit ;
    end ;
    if( _Info = nil ) then
    begin
        _Info := TDefault_Data_Type.Create ;
    end ;
    _Info._Endian := CPU.Big_Endian ;
    _Info._Size := S ;
    Result := _Info ;
end ;


// TMemory_Tab_Array_Interface methods...

// Constructors and destructors...

constructor TMemory_Tab_Array_Interface.Create ;

begin
    inherited Create ;

    _Components := TList.Create ;
    _High := 0 ;
    _Low := $7FFFFFFFFFFFFFFF ;
end ;


destructor TMemory_Tab_Array_Interface.Destroy ;

begin
    _Components.Free ;

    inherited Destroy ;
end ;


// API...

procedure TMemory_Tab_Array_Interface.Add( C : TComponent ) ;

var L, H : int64 ;

begin
    _Components.Add( C ) ;
    if( C.Component_Type = Component_Type_Memory ) then
    begin
        C.Memory.Get_Address_Range( L, H ) ;
        if( L < _Low ) then
        begin
            _Low := L ;
        end ;
        if( H > _High ) then
        begin
            _High := H ;
        end ;
    end ;
end ;


function TMemory_Tab_Array_Interface.Get_Byte( Index : int64 ) : byte ;

var C : TComponent ;
    I : integer ;
    Size : longint ;
    UEC : TUnified_Exception ;

begin
    Size := 8 ;
    for I := 0 to Tab._Components.Count - 1 do
    begin
        C := TComponent( Tab._Components[ I ] ) ;
        if( C.Respond_To_Address( Index, IO_Type_Memory, False ) ) then
        begin
            UEC := C.Examine( Index, Size, @Result, True ) ;
            if( UEC = nil ) then // No error
            begin
                exit ;
            end ;
        end ;
    end ;
    Result := 255 ;
end ;


procedure TMemory_Tab_Array_Interface.Set_Byte( Index : int64 ; Value : byte ) ;

var C : TComponent ;
    I : integer ;

begin
    for I := 0 to _Components.Count - 1 do
    begin
        C := TComponent( _Components[ I ] ) ;
        C.Deposit( Index, 1, @Value, True ) ;
    end ;
end ;


function TMemory_Tab_Array_Interface.Read_Only : boolean ;

begin
    Result := False ;
end ;


function TMemory_Tab_Array_Interface.Low_Bound( Subscript : integer ) : int64 ;

begin
    Result := _Low ;
end ;


function TMemory_Tab_Array_Interface.High_Bound( Subscript : integer ) : int64 ;

begin
    Result := _High ;
end ;


function TMemory_Tab_Array_Interface.Subscripts : integer ;

begin
    Result := 1 ;
end ;



// TMemory_Tab...

// Constructors and destructors...

constructor TMemory_Tab.Create ;

begin
    inherited Create ;

    _Components := TList.Create ;
    if( self <> Main_Form.Main_Memory_Tab ) then
    begin
        AI := TMemory_Tab_Array_Interface.Create ;
        TMemory_Tab_Array_Interface( AI ).Tab := self ;
    end ;
end ;


destructor TMemory_Tab.Destroy ;

begin
    AD.Free ;
    AD := nil ;
    AI.Free ;
    AI := nil ;
    _Components.Free ;
    _Components := nil ;

    inherited Destroy ;
end ;



procedure TMemory_Tab.Add( C : TComponent ) ;

begin
    _Components.Add( C ) ;
    if( self <> Main_Form.Main_Memory_Tab ) then
    begin
        TMemory_Tab_Array_Interface( AI ).Add( C ) ;
        AD.Home_Index := AI.Low_Bound( 0 ) ;
    end ;
end ;


procedure TMemory_Tab.Remove( C : TComponent ) ;

begin
    _Components.Remove( C ) ;
end ;


function TMemory_Tab.Count : integer ;

begin
    Result := _Components.Count ;
end ;


function Tmemory_Tab.Has_Component( C : TComponent ) : boolean ;

var I : integer ;

begin
    I := _Components.Indexof( C ) ;
    Result := ( I <> -1 ) ;
end ;


procedure TMemory_Tab.Set_Watch( Address : int64 ; M : boolean ; Access : longint ) ;

var C : TComponent ;
    Loop : integer ;

begin
    for Loop := 0 to _Components.Count - 1 do
    begin
        C := TComponent( _Components[ Loop ] ) ;
        if( C.Respond_To_Address( Address, IO_Type_Memory, False ) ) then
        begin
            C.Set_Watchpoint( Address, M, Access ) ;
        end ;
    end ;
end ;



// TWatch...

type TWatch = class
                 public
                     Address : int64 ;
                     Size : longint ;
                     Base : integer ; // (0 = ASCII, 1 = EBCDIC, 2 = Binary, etc, 50 = Radix50)
                     Tab : TMemory_Tab ;
              end ;

type TError_Information = class
                              public
                                  Line : longint ;
                                  Severity : longint ;
                                  Filename : string ;
                          end ;

type TEditor_Streamer = class( TLine_Streamer )
                            private // Instance data...
                                _Editor : TEditor_Window ;
                                Line_Buffer : string ;

                            public // Constructors and destructors...
                                constructor Create( Editor : TEditor_Window ) ;

                            public // API...
                                function At_End : boolean ; override ; stdcall ;

                                function Facility_ID : longint ;
                                    override ; stdcall ;

                                function Last_Error : TUnified_Exception ;
                                    override ;

                                function Read( var Buffer ; Size : longint ) : longint ;
                                    override ; stdcall ;

                                procedure Read_Line( var Buffer ; var Size : longint ) ;
                                    override ; stdcall ;

                                procedure Seek( Position : longint ) ;
                                    override ; stdcall ;

                                function Size : longint ; override ;

                                procedure Write( var Buffer ; size : longint ) ;
                                    override ; stdcall ;

                                procedure Write_Line( Buffer : PChar ) ;
                                    override ; stdcall ;
                        end ;



type TReciever_Component = class( TBase_Component )
                               private
                                   _Value : int64 ;

                               public
                                   function Write( Address : int64 ;
                                       Value, Size : longint ;
                                       IO_Type : integer ) : TUnified_Exception ; override ;

                                   property Value : int64
                                       read _Value ;
                           end ;

function TReciever_Component.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : integer ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    _Value := Value ;
end ;



// TComponent_Parent methods...

// API...

procedure TComponent_Parent.Child_Notification( Child : TComponent ;
    var Notice : longint ; var Params : int64 ) ;

var Index : integer ;

begin
    if( Notice = Child_Notice_Terminating ) then
    begin
        Index := Main_Form.Component_List.IndexOfObject( Child ) ;
        if( Index <> -1 ) then
        begin
            Main_Form.Remove_Component( Index, False ) ;
        end ;
    end ;
end ;



// TCEF_TabSheet methods...

procedure TCEF_TabSheet.SetBounds( ALeft, ATop, AWidth, AHeight : Integer ) ;

begin
    inherited SetBounds( ALeft, ATop, AWidth, AHeight ) ;

    if( UI <> nil ) then
    begin
        UI.User_Interface.Set_Size( AHeight, AWidth ) ;
    end ;
end ;



// TCEF_Assembler_Status methods...

// Constructors and destructors...

constructor TCEF_Assembler_Status.Create ;

begin
    inherited Create ;

    Error_List := TStringList.Create ;
    Paging := True ;
end ;


destructor TCEF_Assembler_Status.Destroy ;

var Loop : integer ;

begin
    for Loop := 0 to Error_List.Count - 1 do
    begin
        Error_List.Objects[ Loop ].Free ;
        Error_List.Objects[ Loop ] := nil ;
    end ;
    Error_List.Free ;

    inherited Destroy ;
end ;


function Bytes_Text( Value : int64 ) : string ;

begin
    if( Value <> 1 ) then
    begin
        Result := Substitute1( Text_Bytes, inttostr( Value ) ) ;
    end else
    begin
        Result := Text_1_Byte ;
    end ;
end ;


// API...

procedure TCEF_Assembler_Status.Update_Display ;

var S : string ;

begin
    Last_File := Current_File ;
    Assembly_Statistics.Data_Label.Caption := Bytes_Text( Data ) ;
    Assembly_Statistics.Code_Label.Caption := Bytes_Text( Code ) ;
    Assembly_Statistics.Warnings_Label.Caption := inttostr( Warnings ) ;
    Assembly_Statistics.Errors_Label.Caption := inttostr( Errors ) ;
    S := Current_File ;
    if( Line <> 0 ) then
    begin
        S := S + ' (' + inttostr( Line ) + ')' ;
    end ;
    Assembly_Statistics.Filename.Caption := S ;
end ;


function TCEF_Assembler_Status.Filename : PChar ;

begin
    Result := PChar( Current_File ) ;
end ;


function TCEF_Assembler_Status.Get_Code : int64 ;

begin
    Result := _Code ;
end ;


function TCEF_Assembler_Status.Get_Data : int64 ;

begin
    Result := _Data ;
end ;


function TCEF_Assembler_Status.Get_Errors : integer ;

begin
    Result := _Errors ;
end ;


function TCEF_Assembler_Status.Get_Warnings : integer ;

begin
    Result := _Warnings ;
end ;


function TCEF_Assembler_Status.Get_Error_Text : PChar ;

begin
    if( ( length( _Error_Text ) = 0 ) and ( Error_List.Count > 0  ) ) then
    begin
        Result := PChar( Error_List[ 0 ] ) ;
    end else
    begin
        Result := PChar( _Error_Text ) ;
    end ;
end ;


procedure TCEF_Assembler_Status.Set_Code( Value : int64 ) ;

begin
    _Code := Value ;
end ;


procedure TCEF_Assembler_Status.Set_Data( Value : int64 ) ;

begin
    _Data := Value ;
end ;


procedure TCEF_Assembler_Status.Set_Errors( Value : integer ) ;

begin
    _Errors := Value ;
end ;


procedure TCEF_Assembler_Status.Set_Warnings( Value : integer ) ;

begin
    _Warnings := Value ;
end ;


procedure TCEF_Assembler_Status.Set_Error_Text( Value : PChar ) ;

begin
    _Error_Text := Value ;
end ;


procedure TCEF_Assembler_Status.Log_Error( Text, Filename : PChar ;
    Line, Severity : longint ) ;

var P : TError_Information ;
    S : string ;

begin
    P := TError_Information.Create ;
    if( Line = -1 ) then
    begin
        if( In_Streamer <> nil ) then
        begin
            P.Line := In_Streamer.Current_Line ;
        end ;
    end else
    begin
        P.Line := Line ;
    end ;
    if( Filename = nil ) then
    begin
        P.Filename := Current_File ;
    end else
    begin
        P.Filename := string( Filename ) ;
    end ;
    P.Severity := Severity ;
    case Severity of
        Severity_Warning :
            begin
                inc( _Warnings ) ;
                S := Text_Bracket_Warning ;
            end ;
        Severity_Error, Severity_Fatal :
            begin
                inc( _Errors ) ;
                S := Text_Bracket_Error ;
            end ;
        else S := Text_Bracket_Hint ;
    end ;
    S := S + P.Filename ;
    if( P.Line > 0 ) then
    begin
        S := S + '(' + inttostr( P.Line ) + ')' ;
    end ;
    Error_List.AddObject( S + ' ' + string( Text ), P ) ;
end ; // TCEF_Assembler_Status.Log_Error


procedure TCEF_Assembler_Status.Get_Error( Index : longint ;
    var Text, Filename : PChar ; var Line, Severity : longint ) ;

var Info : TError_Information ;

begin
    if( ( Index < 0 ) or ( Index >= Error_List.Count ) ) then
    begin
        Text := nil ;
        Filename := nil ;
        Line := 0 ;
        Severity := 0 ;
        exit ;
    end ;
    Info := TError_Information( Error_List.Objects[ Index ] ) ;
    Text := PChar( Error_List[ Index ] ) ;
    Filename := PChar( Info.Filename ) ;
    Line := Info.Line ;
    Severity := Info.Severity ;
end ;


procedure TCEF_Assembler_Status.Set_Aborted( Value : boolean ) ;

begin
    _Aborted := Value ;
end ;


function TCEF_Assembler_Status.Get_Aborted : boolean ;

begin
    // Check aborted status
    if( Assembly_Statistics = nil ) then
    begin
        Result := False ;
        exit ;
    end ;
    if( Assembly_Statistics.Aborted ) then
    begin
        _Aborted := True ;
    end ;
    Result := _Aborted ;

    // Update the dialog...
    if( _Aborted or ( ( Line and 32 ) = 0 ) or ( Last_File <> Current_File ) ) then
    begin
        Update_Display ;
    end ;
end ;


procedure TCEF_Assembler_Status.Set_Line( Value : integer ) ;

begin
    Line := Value ;
end ;


procedure TCEF_Assembler_Status.Output_To_Listing( Text : PChar ;
    Text_Type : integer ) ;

    procedure Check_Heading ;

    var S : string ;

    begin
        if( Page_Line = 0 ) then
        begin
            {$I-}
            writeln( Listing_File ) ;
            {$I+}
            IOResult ;

            S := Title ;
            while( length( S ) < 70 ) do // TODO: Allow user to specify page width
            begin
                S := S + ' ' ;
            end ;
            S := Substitute( Text_Page, inttostr( Page ) ) ;
            {$I-}
            writeln( Listing_File, S ) ;
            {$I+}
            IOResult ;

            if( Subtitle <> '' ) then
            begin
                {$I-}
                writeln( Listing_File, Subtitle ) ;
                {$I+}
                IOResult ;
            end ;

            {$I-}
            writeln( Listing_File ) ;
            {$I+}
            IOResult ;

            Page_Line := 3 ;
        end ;
    end ;


    procedure New_Page ;

    begin
        if( not Paging ) then
        begin
            exit ;
        end ;
        inc( Page ) ;
        Page_Line := 0 ; // Force heading
        {$I-}
        write( Listing_File, FF ) ;
        {$I+}
        IOResult ;
        Check_Heading ;
    end ;


begin // TCEF_Assembler_Status.Output_To_Listing
    if( Allow_Listing and Options_Form.Generate_Listings.Checked ) then
    begin
        {$I-}
        case Text_Type of
            ListOut_Title_Text: Title := string( Text ) ;
            ListOut_SubTitle: SubTitle := string( Text ) ;
            ListOut_New_Line:
                begin
                    Check_Heading ;
                    writeln( Listing_File ) ;
                    inc( Page_Line ) ;
                    if( Page_Line > 63 ) then
                    begin
                        New_Page ;
                    end ;
                end ;
            ListOut_New_Page:
                begin
                    New_Page ;
                end ;
            ListOut_Generated_Data:
                begin
                    Data_String := Data_String + string( Text ) ;
                end ;
            ListOut_Paging: Paging := True ;
            ListOut_No_Paging: Paging := False ;
            ListOut_Message:
                begin
                    MessageBox( 0, Text, 'Message', 0 ) ;
                end ;
            else
                begin
                    Check_Heading ;
                    while( length( Data_String ) < 20 ) do
                    begin
                        Data_String := Data_String + ' ' ;
                    end ;
                    write( Listing_File, Data_String, string( Text ) ) ;
                    Data_String := '' ;
                end ;
        end ; // case Text_Type
        {$I+}
        IOResult ;
    end ; // if
end ; // TCEF_Assembler_Status.Output_To_Listing


function Component_Name( Component : TComponent ) : string ;

begin
    if( Component = nil ) then
    begin
        Result := Text_Emulator ;
    end else
    begin
        Result := string( Component.Name ) ;
        Component := Component.Parent ;
        while( Component <> nil ) do
        begin
            Result := string( Component.Name ) + '.' + Result ;
            Component := Component.Parent ;
        end ;
    end ;
end ;


// TCEF32_Interface methods...

// Constructors and destructors...

constructor TCEF32_Interface.Create ;

begin
    inherited Create ;

    Signal_Components := TList.Create ;
    Threads := TInteger_List.Create ;
end ;


destructor TCEF32_Interface.Destroy ;

begin
    Signal_Components.Free ;
    Signal_Components := nil ;
    Threads.Free ;
    Threads := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TCEF32_Interface.Determine_Port_Component_Index( var Index : longint ) : TComponent ;

var C : TComponent ;
    I, Loop : integer ;

begin
    Result := nil ; // Assume failure
    for Loop := 0 to Main_Form.Component_List.Count - 1 do
    begin
        C := TComponent( Main_Form.Component_List.Objects[ Loop ] ) ;
        if( C <> nil ) then
        begin
            if( C.Version >= 20 ) then // Supports emulator ports
            begin
                I := 0 ;
                while( C.Get_Port( I ) <> nil ) do
                begin
                    if( Index = 0 ) then
                    begin
                        Result := C ;
                        Index := I ;
                        exit ;
                    end ;
                    dec( Index ) ;
                    inc( I ) ;
                end ;
            end ;
        end ;
    end ;
end ;


// API...

procedure TCEF32_Interface.Idle( Component : TComponent ) ;

begin
    Application.ProcessMessages ;
    if( ( Component <> nil ) and ( _Clock <> nil ) ) then
    begin
        _Clock.Block( nil, 0 ) ; // Release next blocked component
    end ;
end ;


function TCEF32_Interface.Breakpoint_Notice( Address : int64 ;
    Physical : boolean ; Space : integer ; CPU : TComponent ) : boolean ;

var Tab : TMemory_Tab ;

begin
    if( ( Main_Form.Master_CPU = nil ) or ( Main_Form.Master_CPU.Version < 26 ) ) then
    begin
        Tab := Main_Form.Main_Memory_Tab ;
    end else
    begin
        Tab := Main_Form.Memory_Tab_With_Component( Main_Form.Master_CPU.CPU.Get_Target_Memory ) ;
        if( Tab = nil ) then
        begin
            Tab := Main_Form.Main_Memory_Tab ;
        end ;
    end ;
    ShowMessage( Substitute2( Text_Breakpoint_At_Address, Component_Name( CPU ),
        CvtB( 10, Tab.AD.Base, inttostr( Address ) ) ) ) ;
    CPU.CPU.Stop ;
    Result := True ;
end ;


function TCEF32_Interface.Clock : TMaster_Clock ;

begin
    if( Main_Form.Disable1.Checked ) then
    begin
        Result := nil ;
        exit ;
    end ;
    if( _Clock = nil ) then
    begin
        _Clock := TCEF_Generic_Clock.Create ;
        _Clock.Initialize( self ) ;
        if( Main_Form.Default1.Checked ) then
        begin
            _Clock.Mode := 0 ;
        end else
        if( Main_Form.Synchronize1.Checked ) then
        begin
            _Clock.Mode := 2 ;
        end else
        begin
            _Clock.Mode := 1 ;
        end ;
    end ;
    Result := _Clock ;
end ;


procedure TCEF32_Interface.Log_Error( Component : TComponent ; Text : PChar ;
    Severity : longint ) ;

var S : string ;

begin
    S := Component_Name( Component ) + ': ' + string( Text ) ;
    ShowMessage( S ) ;
    if( Severity = Severity_Fatal ) then
    begin
        Main_Form.Close ;
    end else
    if(
        ( Main_Form.Master_CPU <> nil )
        and
        ( TComponent_Info( Main_Form.Master_CPU.Tag ).Ignore_Errors[ Severity ] )
      ) then
    begin
        Main_Form.Master_CPU.CPU.Stop ;
    end ;
end ;


procedure TCEF32_Interface.Log_Simulated_Error( Component : TComponent ;
    Text : PChar ; Severity : longint ) ;

var S : string ;

begin
    S := Component_Name( Component ) + ': ' + string( Text ) ;
    ShowMessage( S ) ;
    if( Severity = Severity_Fatal ) then
    begin
        Main_Form.Close ;
    end else
    if(
        ( Main_Form.Master_CPU <> nil )
        and
        ( TComponent_Info( Main_Form.Master_CPU.Tag ).Ignore_Errors[ Severity ] )
      ) then
    begin
        Main_Form.Master_CPU.CPU.Stop ;
    end ;
end ;


procedure TCEF32_Interface.State_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

var Info : TComponent_Info ;

begin
    Info := TComponent_Info( Component.Tag ) ;
    if(
        ( Info <> nil )
        and
        ( Info.State_Changes.IndexOf( Index ) >= 0 )
        and
        Main_Form.In_Execution
      ) then
    begin
        ShowMessage( Substitute2( Text_Break_On_State_Change, Component_name( Component ), Component.Get_State_Name( Index ) ) ) ;
        if( Main_Form.Master_CPU <> nil ) then
        begin
            Main_Form.Master_CPU.CPU.Stop ;
        end ;
    end ;
end ;


procedure TCEF32_Interface.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

var C : TComponent ;
    I : integer ;
    Info : TComponent_Info ;
    Loop : integer ;
    SN : string ;

begin
    // Notify all components that request signal change notices...
    SN := Component.Signal_Name( Index ) ;
    for Loop := 0 to Signal_Components.Count - 1 do
    begin
        C := TComponent( Signal_Components[ Loop ] ) ;
        if( C <> Component ) then // Don't send to originating component
        begin
            try
                C.Signal_Change_Notice( Component, Index, Active ) ;
                C.Set_Signal( PChar( SN ), Active ) ;
                I := C.Signal_Index( PChar( SN ) ) ;
                if( I >= 0 ) then
                begin
                    Info := TComponent_Info( C.Tag ) ;
                    if( Info <> nil ) then // If component is one that we loaded
                    begin
                        Info.Set_Signal( I, True, Active ) ;
                    end ;
                end ;
            except
            end ;
        end ; // if( C <> Component )
    end ; // for Loop := 0 to Signal_Components.Count - 1

    // Check for signal breakpoint...
    Info := TComponent_Info( Component.Tag ) ;
    if( Info <> nil ) then
    begin
        Info.Set_Signal( Index, True, Active ) ;
        if(
            ( Info.Signal_Changes.IndexOf( Index ) >= 0 )
            and
            Main_Form.In_Execution
          ) then
        begin
            ShowMessage( Substitute2( Text_Break_On_State_Change, Component_Name( Component ), Component.Signal_Name( Index ) ) ) ;
            if( Main_Form.Master_CPU <> nil ) then
            begin
                Main_Form.Master_CPU.CPU.Stop ;
            end ;
        end ;
    end ; // if( Info <> nil )
end ; // TCEF32_Interface.Signal_Change_Notice


procedure TCEF32_Interface.Signal_Exception( Component : TComponent ;
    Description : PChar ; Index : longint ) ;

var Info : TComponent_Info ;

begin
    Info := TComponent_Info( Component.Tag ) ;
    if(
        ( Info.Exceptions.IndexOf( Index ) >= 0 )
        and
        Main_Form.In_Execution
      ) then
    begin
        ShowMessage( Substitute2( Text_Exception, Component_name( Component ), Component.Get_Exception_Description( Index ) ) ) ;
        if( Main_Form.Master_CPU <> nil ) then
        begin
            Main_Form.Master_CPU.CPU.Stop ;
        end ;
    end ;
end ;


procedure TCEF32_Interface.Log_Trace( Component : TComponent ;
    Description : PChar ) ;

begin
    Main_Form.Log_Trace( Component, string( Description ) ) ;
end ;


function TCEF32_Interface.Get_File_Stream( Name : PChar ) : TCOM_Stream ;

begin
    Result := Main_Form.Get_File_Stream( string( Name ) ) ;
end ;


procedure TCEF32_Interface.Toggle_Embed( Component : TComponent ) ;

var M : TMenuItem ;

begin
    if( Component <> nil ) then
    begin
        M := Main_Form.Menu_For_Component( Component ) ;
        if( M <> nil ) then
        begin
            Main_Form.ToggleEmbedClick( M ) ;
        end ;
    end ;
end ;


procedure TCEF32_Interface.Want_Signals( Component : TComponent ;
    Value : boolean ) ;

begin
    if( Component <> nil ) then
    begin
        if( Value ) then
        begin
            Signal_Components.Add( Component ) ;
        end else
        begin
            Signal_Components.Remove( Component ) ;
        end ;
    end ;
end ;


procedure TCEF32_Interface.Terminate ;

begin
    Main_Form.Visible := True ;
    Main_Form.Terminate_Requested := True ;
    SetFocus( Main_Form.Handle ) ;
end ;


procedure TCEF32_Interface.Hide( Flag : boolean ) ;

begin
    Main_Form.Visible := Flag ;
end ;


procedure TCEF32_Interface.Watchpoint_Notice( Address : int64 ;
    Access, Tag : longint ; Component : TComponent ;
    Memory, Internal, Port : boolean ) ;

var S : string ;
    Tab : TMemory_Tab ;

begin
    case Access of
        Access_Read : S := Text_Access_Read ;
        Access_Write : S := Text_Access_Write ;
        else S := Text_Access ;
    end ;
    if( Port ) then
    begin
        case Access of
            Access_Read : S := Text_Access_Input ;
            Access_Write : S := Text_Access_Output ;
        end ;
        ShowMessage( Substitute3( Text_Port_Watchpoint_Triggered_For_Port, Component_Name( Component ), S, CvtB( 10, Main_Form.Port_Tab.AD.Base, inttostr( Address ) ) ) ) ;
    end else
    if( Internal and ( Component.Component_Type = Component_Type_CPU ) ) then
    begin
        if( Memory ) then
        begin
            ShowMessage( Substitute3( Text_Cache_Watchpoint_At_Address, Component_Name( Component ), S, CvtB( 10, Main_Form.Main_Memory_Tab.AD.Base, inttostr( Address ) ) ) ) ;
        end else
        begin
            ShowMessage( Substitute3( Text_Register_Watchpoint_Triggered_For_Register, Component_Name( Component ), S, Component.CPU.Register_Name( Address ) ) ) ;
        end ;
    end else
    begin
        Tab := Main_Form.Main_Memory_Tab ;
        if( Component.Component_Type = Component_Type_Memory ) then
        begin
            Tab := Main_Form.Memory_Tab_With_Component( Component ) ;
            if( Tab = nil ) then
            begin
                Tab := Main_Form.Main_Memory_Tab ;
            end ;
        end ;
        ShowMessage( Substitute2( Text_Watchpoint_At_Address, Component_Name( Component ), CvtB( 10, Tab.AD.Base, inttostr( Address ) ) ) ) ;
    end ;
    if( Main_Form.Master_CPU <> nil ) then
    begin
        Main_Form.Master_CPU.CPU.Stop ;
    end ;
end ; // TCEF32_Interface.Watchpoint_Notice


function TCEF32_Interface.Get_Port_Name( Index : longint ) : PChar ;

var C : TComponent ;

begin
    Result := nil ;
    C := Determine_Port_Component_Index( Index ) ;
    if( C <> nil ) then
    begin
        Result := C.Get_Port_Name( Index ) ;
    end ;
end ;


function TCEF32_Interface.Get_Port_Description( Index : longint ) : PChar ;

var C : TComponent ;

begin
    Result := nil ;
    C := Determine_Port_Component_Index( Index ) ;
    if( C <> nil ) then
    begin
        Result := C.Get_Port_Description( Index ) ;
    end ;
end ;


function TCEF32_Interface.Get_Port( Index : longint ) : TComponent ;

var C : TComponent ;

begin
    Result := nil ;
    C := Determine_Port_Component_Index( Index ) ;
    if( C <> nil ) then
    begin
        Result := C.Get_Port( Index ) ;
    end ;
end ;


function TCEF32_Interface.Get_Port_Connection( Index : longint ) : TComponent ;

var C : TComponent ;

begin
    Result := nil ;
    C := Determine_Port_Component_Index( Index ) ;
    if( C <> nil ) then
    begin
        Result := C.Get_Port_Connection( Index ) ;
    end ;
end ;


function TCEF32_Interface.Port_Parent_Component( Index : longint ) : TComponent ;

begin
    Result := Determine_Port_Component_Index( Index ) ;
end ;


procedure TCEF32_Interface.Run( State : boolean ) ;

begin
    if( State ) then
    begin
        Main_Form.Execute1Click( nil ) ;
    end else
    begin
        Main_Form.Pause1Click( nil ) ;
    end ;
end ;


function TCEF32_Interface.Process_ID( Name : PChar ; Force : boolean ) : integer ;

begin
    if( Force or ( Threads.Count < Options_Form.Max_Threads.Value ) ) then
    begin
        Threads.Add( Thread_ID ) ;
        Result := Thread_ID ;
        inc( Thread_ID ) ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TCEF32_Interface.Process_Start( ID : longint ;
    var Priority : longint ) : boolean ;

begin
    Priority := ( Options_Form.Thread_Priority.ItemIndex - 3 ) shl 13 ;
    Result := True ;
end ;


procedure TCEF32_Interface.Process_Deleted( ID : longint ) ;

begin
    Threads.Remove( ID ) ;
end ;


procedure TCEF32_Interface.Add_Port_Breakpoint ;

begin
    Main_Form.AddPortBreakpoint1Click( nil ) ;
end ;


procedure TCEF32_Interface.Add_Breakpoint ;

begin
    Main_Form.AddBreakpoint1Click( nil ) ;
end ;


procedure TCEF32_Interface.Add_Register_Breakpoint ;

begin
    Main_Form.AddRegisterBreakpoint1Click( nil ) ;
end ;


procedure TCEF32_Interface.Create_New_Breakpoint ;

begin
   Main_Form.Createnew1Click( nil ) ;
end ;


function TCEF32_Interface.Get_Component_Filename( Name : PChar ) : PChar ;

begin
    Temp := string( Name ) ;
    if( pos( ':', Temp ) + pos( '\', Temp ) = 0 ) then
    begin
        Temp := Program_Path + 'Components\' + Temp ;
    end ;
    Result := PChar( Temp ) ;
end ;


procedure TCEF32_Interface.Termination_Notice( C : TComponent ) ;

begin
    Main_Form.Termination_Notice( C ) ;
    Signal_Components.Remove( C ) ;
end ;


function TCEF32_Interface.Load_Component( Name : PChar ) : TComponent ;

begin
    Result := Main_Form._Load_Component( Get_Component_Filename( Name ), self ) ;
end ;


type TFile_Streamer = class( TCOM_Stream )
                          private // Instance data...
                              F : file ;

                          public // Constructors and destructors...
                              constructor Create( Name : string ; _Create : boolean ) ;

                              destructor Destroy ; override ;

                          public // API...
                              function Is_Class( _N : PChar ) : boolean ;
                                  override ;

                              function At_End : boolean ; override ; stdcall ;

                              function Facility_ID : longint ;
                                  override ; stdcall ;

                              function Read( var Buffer ; Size : longint ) : longint ;
                                  override ; stdcall ;

                              procedure Read_Line( var Buffer ; var Size : longint ) ;
                                  override ; stdcall ;

                              procedure Seek( Position : longint ) ;
                                  override ; stdcall ;

                              function Size : longint ;
                                  override ;

                              procedure Write( var Buffer ; size : longint ) ;
                                  override ; stdcall ;

                              procedure Write_Line( Buffer : PChar ) ;
                                  override ; stdcall ;
                      end ; // TFile_Streamer

// Constructors and destructors...

constructor TFile_Streamer.Create( Name : string ; _Create : boolean ) ;

begin
    inherited Create ;

    assignfile( F, Name ) ;
    {$I-}
    if( _Create ) then
    begin
        rewrite( F, 1 ) ;
    end else
    begin
        reset( F, 1 ) ;
    end ;
    {$I+}
    Set_Last_Error( Create_MSDOS_UE( IOResult, Text_Error_While_Accessing_File ) ) ;
end ;


destructor TFile_Streamer.Destroy ;

begin
    closefile( F ) ;

    inherited Destroy ;
end ;


// API...

function TFile_Streamer.Is_Class( _N : PChar ) : boolean ;

var N : string ;

begin
    N := lowercase( string( _N ) ) ;
    Result := ( N = 'tfile_streamer' ) or ( N = 'tcom_stream' ) ;
end ;


{ Returns True if there is no more data in the stream to read. }
function TFile_Streamer.At_End : boolean ;

begin
    Result := EOF( F ) ;
end ;


{ Returns the facility code for this class. }
function TFile_Streamer.Facility_ID : longint ;

begin
    Result := 53 ;
end ;


{ Reads the specified number of bytes from the stream.  Size is modified
  to be the actual bytes transferred. }
function TFile_Streamer.Read( var Buffer ; Size : longint ) : longint ;

var Count : integer ;

begin
    {$I-}
    blockread( F, Buffer, Size, Count ) ;
    {$I+}
    Set_Last_Error( Create_MSDOS_UE( IOResult, '' ) ) ;
    Read := Count ;
end ;


{ Reads one line of input, up to the size of the buffer.  The line is
  assumed to end at an ASCII 13 code.  The ASCII 13 code is not included
  in the returned data.  Size is modified to be the actual bytes
  transferred. }
procedure TFile_Streamer.Read_Line( var Buffer ; var Size : longint ) ;

var C : char ;
    Count : integer ;
    S : string ;
    Temp : longint ;

begin
    Set_Last_Error( nil ) ;
    Count := 0 ;
    while( ( Count <= Size ) and ( not At_End ) ) do
    begin
        Temp := 1 ;
        Read( C, Temp ) ;
        if( Last_Error <> nil ) then
        begin
            Size := Count ;
            move( S[ 1 ], Buffer, Count ) ;
            exit ;
        end ;
        if( C = CR ) then // End of line
        begin
            break ;
        end ;
        S := S + C ;
        inc( Count ) ;
    end ;
    Size := Count ;
    move( S[ 1 ], Buffer, Count ) ;
end ;


{ Position to specified byte within streamed data. }
procedure TFile_Streamer.Seek( Position : longint ) ;

begin
    {$I-}
    System.Seek( F, Position ) ;
    {$I+}
    Set_Last_Error( Create_MSDOS_UE( IOResult, '' ) ) ;
end ;


function TFile_Streamer.Size : longint ;

begin
    Size := filesize( F ) ;
end ;


{ Writes the specified buffer, of the specified size in bytes, to the
  stream. }
procedure TFile_Streamer.Write( var Buffer ; size : longint ) ;

begin
    {$I-}
    blockwrite( F, Buffer, Size ) ;
    {$I+}
    Set_Last_Error( Create_MSDOS_UE( IOResult, '' ) ) ;
end ;


{ Writes the specified null-terminated text to the stream.  An ASCII
  code 13 is appended to the text on output. }
procedure TFile_Streamer.Write_Line( Buffer : PChar ) ;

var S : char ;

begin
    Write( Buffer[ 0 ], strlen( Buffer ) ) ;
    if( Last_Error = nil ) then
    begin
        S := CR ;
        Write( S, 1 ) ;
    end ;
end ;



// Constructors and destructors...

constructor TEditor_Streamer.Create( Editor : TEditor_Window ) ;

begin
    inherited Create ;

    _Editor := Editor ;
    if( _Editor.Max_Line > 0 ) then
    begin
        Line_Buffer := _Editor.Get_Line( 1 ) + CR ;
    end ;
    Current_Line := 1 ;
end ;


// API...

function TEditor_Streamer.At_End : boolean ;

begin
    Result := ( ( length( Line_Buffer ) = 0 ) and ( Current_Line > _Editor.Max_Line ) ) ;
end ;


function TEditor_Streamer.Facility_ID : longint ;

begin
    Result := 55 ;
end ;


function TEditor_Streamer.Last_Error : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TEditor_Streamer.Read( var Buffer ; Size : longint ) : longint ;

begin
    Read_Line( Buffer, Size ) ; // No difference from Read_Line
    Read := Size ;
end ;


procedure TEditor_Streamer.Read_Line( var Buffer ; var Size : longint ) ;

begin
    if( length( Line_Buffer ) = 0 ) then // Nothing in buffer
    begin
        inc( Current_Line ) ;
        if( not At_End ) then
        begin
            Line_Buffer := _Editor.Get_Line( Current_Line ) + CR ;
        end ;
    end ;
    if( Size > length( Line_Buffer ) ) then
    begin
        Size := length( Line_Buffer ) ;
    end ;
    move( Line_Buffer[ 1 ], Buffer, Size ) ;
    Line_Buffer := copy( Line_Buffer, Size + 1, length( Line_Buffer ) ) ;
end ;


procedure TEditor_Streamer.Seek( Position : longint ) ;

begin
    Current_Line := 1 ;
    Line_Buffer := '' ;
    while( Position > 0 ) do
    begin
        if( At_End ) then
        begin
            exit ;
        end ;
        Line_Buffer := _Editor.Get_Line( Current_Line ) + CR ;
        Position := Position - length( Line_Buffer ) ;
        if( Position < 0 ) then
        begin
            Line_Buffer := copy( Line_Buffer, -Position, length( Line_Buffer ) ) ;
        end ;
    end ;
end ;


function TEditor_Streamer.Size : longint ;

begin
    Size := -1 ;
end ;


procedure TEditor_Streamer.Write( var Buffer ; size : longint ) ;

begin
    // We don't support the assembler writing back to the editor.
end ;


procedure TEditor_Streamer.Write_Line( Buffer : PChar ) ;

begin
    // We don't support the assembler writing back to the editor.
end ;


type TNull_Streamer = class( TCOM_Stream )
                            public // API...
                                Write_Size : int64 ;

                                function At_End : boolean ; override ; stdcall ;

                                function Facility_ID : longint ;
                                    override ; stdcall ;

                                function Last_Error : TUnified_Exception ;
                                    override ;

                                function Read( var Buffer ; Size : longint ) : longint ;
                                    override ; stdcall ;

                                procedure Read_Line( var Buffer ; var Size : longint ) ;
                                    override ; stdcall ;

                                procedure Seek( Position : longint ) ;
                                    override ; stdcall ;

                                function Size : longint ; override ;

                                procedure Write( var Buffer ; size : longint ) ;
                                    override ; stdcall ;

                                procedure Write_Line( Buffer : PChar ) ;
                                    override ; stdcall ;
                        end ;

// API...

function TNull_Streamer.At_End : boolean ;

begin
    Result := True ;
end ;


function TNull_Streamer.Facility_ID : longint ;

begin
    Result := -1 ;
end ;


function TNull_Streamer.Last_Error : TUnified_Exception ;

begin
    Last_Error := nil ;
end ;


function TNull_Streamer.Read( var Buffer ; Size : longint ) : longint ;

begin
    Read := 0 ;
end ;


procedure TNull_Streamer.Read_Line( var Buffer ; var Size : longint ) ;

begin
    Size := 0 ;
end ;


procedure TNull_Streamer.Seek( Position : longint ) ;

begin
end ;


function TNull_Streamer.Size : longint ;

begin
    Result := 0 ;
end ;


procedure TNull_Streamer.Write( var Buffer ; size : longint ) ;

begin
    Write_Size := Write_Size + Size ;
end ;


procedure TNull_Streamer.Write_Line( Buffer : PChar ) ;

begin
    Write_Size := Write_Size + strlen( Buffer ) ;
end ;



type TOutput_Streamer = class( TCEF_Stream )
                            private // Instance data...
                                Address : int64 ;

                            public // API...
                                Code_Size : int64 ;
                                Target : TComponent ;

                                function Is_Class( _N : PChar ) : boolean ;
                                    override ;

                                function At_End : boolean ; override ; stdcall ;

                                function Facility_ID : longint ;
                                    override ; stdcall ;

                                function Last_Error : TUnified_Exception ;
                                    override ;

                                function Read( var Buffer ; Size : longint ) : longint ;
                                    override ; stdcall ;

                                procedure Read_Line( var Buffer ; var Size : longint ) ;
                                    override ; stdcall ;

                                procedure Seek( Position : longint ) ;
                                    override ; stdcall ;

                                function Size : longint ; override ;

                                procedure Write( var Buffer ; size : longint ) ;
                                    override ; stdcall ;

                                procedure Write_Line( Buffer : PChar ) ;
                                    override ; stdcall ;

                                procedure Set_Component( Value : TComponent ) ;
                                    override ; stdcall ;

                                function Get_Component : TComponent ;
                                    override ; stdcall ;
                        end ;

// API...

function TOutput_Streamer.Is_Class( _N : PChar ) : boolean ;

var N : string ;

begin
    N := lowercase( string( _N ) ) ;
    Result := ( N = 'toutput_streamer' ) or ( N = 'tcef_stream' ) ;
end ;


function TOutput_Streamer.At_End : boolean ;

begin
    Result := True ;
end ;


function TOutput_Streamer.Facility_ID : longint ;

begin
    Result := -1 ;
end ;


function TOutput_Streamer.Last_Error : TUnified_Exception ;

begin
    Last_Error := nil ;
end ;


function TOutput_Streamer.Read( var Buffer ; Size : longint ) : longint ;

begin
    // This is an output-only streamer
    Read := 0 ;
end ;


procedure TOutput_Streamer.Read_Line( var Buffer ; var Size : longint ) ;

begin
    // This is an output-only streamer
end ;


procedure TOutput_Streamer.Seek( Position : longint ) ;

begin
    Address := Position ;
end ;


function TOutput_Streamer.Size : longint ;

begin
    Size := Code_Size ;
end ;


procedure TOutput_Streamer.Write( var Buffer ; size : longint ) ;

var Value : integer ;
    P : PChar ;

begin
    Code_Size := Code_Size + Size ;
    P := PChar( @Buffer ) ;
    while( Size > 0 ) do
    begin
        Value := 0 ;
        move( P[ 0 ], Value, 1 ) ;
        if( ( Target = nil ) or ( Target.Memory = nil ) ) then
        begin
            Main_Form.Write_Memory( Address, Value ) ;
        end else
        begin
            Target.Deposit( Address, 1, @Value, True ) ;
        end ;
        inc( Address ) ;
        dec( Size ) ;
        inc( P ) ;
    end ;
end ;


procedure TOutput_Streamer.Write_Line( Buffer : PChar ) ;

var Loop : integer ;

begin
    Loop := 0 ;
    while( Buffer[ Loop ] <> CR ) do
    begin
        Write( Buffer[ Loop ], 1 ) ;
        inc( Loop ) ;
    end ;
end ;


procedure TOutput_Streamer.Set_Component( Value : TComponent ) ;

begin
    Target := Value ;
end ;


function TOutput_Streamer.Get_Component : TComponent ;

begin
    Result := Target ;
end ;



// TPaint_Panel methods...

procedure TPaint_Panel.wmPaint( var Message ) ;

begin
    inherited ;

    Main_Form.Memory_BoxPaint( self ) ;
end ;



// TMemory_Array_Interface methods...

function TMemory_Array_Interface.Is_Class( _N : PChar ) : boolean ;

var N : string ;

begin
    N := lowercase( string( _N ) ) ;
    Result := ( N = 'tmemory_array_interface' ) ;
end ;


function TMemory_Array_Interface.Get_Byte( Index : int64 ) : byte ;

begin
    Result := Main_Form.Examine_Memory( Index ) ;
end ;


procedure TMemory_Array_Interface.Set_Byte( Index : int64 ; Value : byte ) ;

var UEC : TUnified_Exception ;
    _S, _T : integer ;

begin
    UEC := Main_Form.Deposit_Memory( Index, Value ) ;
    if( UEC <> nil ) then
    begin
        ShowMessage( UEC.Error_Text( _S, _T ) ) ;
    end ;
end ;


function TMemory_Array_Interface.Read_Only : boolean ;

begin
    Result := False ;
end ;


function TMemory_Array_Interface.Low_Bound( Subscript : integer ) : int64 ;

begin
    if( Main_Form.Physical_Address ) then
    begin
        Result := Main_Form.Low_Physical_Address ;
    end else
    begin
        Result := Main_Form.Low_Address ;
    end ;
end ;


function TMemory_Array_Interface.High_Bound( Subscript : integer ) : int64 ;

begin
    if( Main_Form.Physical_Address ) then
    begin
        Result := Main_Form.High_Physical_Address ;
    end else
    begin
        Result := Main_Form.High_Address ;
    end ;
end ;


function TMemory_Array_Interface.Subscripts : integer ;

begin
    Result := 1 ;
end ;



// TPort_Array_Interface methods...

function TPort_Array_Interface.Is_Class( _N : PChar ) : boolean ;

var N : string ;

begin
    N := lowercase( string( _N ) ) ;
    Result := ( N = 'tport_array_interface' ) or ( N = 'tcom_array_interface' ) ;
end ;


function TPort_Array_Interface.Get_Byte( Index : int64 ) : byte ;

begin
    Result := Main_Form.Examine_Port( Index ) ;
end ;


procedure TPort_Array_Interface.Set_Byte( Index : int64 ; Value : byte ) ;

var UEC : TUnified_Exception ;
    _S, _T : integer ;

begin
    UEC := Main_Form.Deposit_Port( Index, Value ) ;
    if( UEC <> nil ) then
    begin
        ShowMessage( UEC.Error_Text( _S, _T ) ) ;
    end ;
end ;


function TPort_Array_Interface.Read_Only : boolean ;

begin
    Result := False ;
end ;


function TPort_Array_Interface.Low_Bound( Subscript : integer ) : int64 ;

begin
    Result := Main_Form.Low_Port_Address ;
end ;


function TPort_Array_Interface.High_Bound( Subscript : integer ) : int64 ;

begin
    Result := Main_Form.High_Port_Address ;
end ;


function TPort_Array_Interface.Subscripts : integer ;

begin
    Result := 1 ;
end ;



// TMain_Form methods...

procedure TMain_Form.About1Click( Sender : TObject ) ;

begin
    About_Box.ShowModal ;
end ;


procedure TMain_Form.Help2Click( Sender : TObject ) ;

var S : string ;

begin
    S := Parse.Program_Path + 'Docs\help.html' ;
    if( FileExists( S ) ) then
    begin
        OS.Spawn( S, '', '', 0 ) ;
        exit ;
    end ;
    OS.Spawn( 'http://cef.sourceforge.net/docs/help.html', '', '', 0 ) ;
end ;


procedure TMain_Form.Exit1Click( Sender : TObject ) ;

begin
    Close ;
end ;


procedure TMain_Form.Open1Click( Sender : TObject ) ;

begin
    if( Open_Source_Dialog.Execute ) then
    begin
        Open_File( Open_Source_Dialog.Filename ) ;
    end ;
end ;


procedure TMain_Form.Memory_BoxPaint( Sender : TObject ) ;

var Range : int64 ;
    T : TMemory_Tab ;
    Tab_Tag : integer ;

begin
    // Paint the panel...
    Tab_Tag := TWinControl( Sender ).Tag ;
    T := TMemory_Tab( Memory_Tabs[ Tab_Tag ] ) ;
    if( TWinControl( Sender ).Parent = Tabsheet2 ) then // Port panel
    begin
        try
            Port_Tab.AD.Display ;
            Memory_Scrollbar.LargeChange := T.AD.LPP ;
            Range := Port_Tab.AI.High_Bound( 0 ) ;
            Range := Range - Port_Tab.AI.Low_Bound( 0 ) + 1 ;
            Range_Shift := 0 ;
            if( Range < 0 ) then // Overflow
            begin
                Range := $7FFFFFFFFFFFFFFF ;
                Range_Shift := 1 ;
            end ;
            if( Port_Tab.AD.BPL = 0 ) then
            begin
                Memory_Scrollbar.Max := 0 ;
            end else
            begin
                Range := ( Range div Port_Tab.AD.BPL ) * Port_Tab.AD.LPP - 1 ;
                while( Range > $7FFFFFFF ) do // Overflow
                begin
                    Range := Range shr 1 ;
                    inc( Range_Shift ) ;
                end ;
                Memory_Scrollbar.Max := Range ;
            end ;
        except
        end ;
    end else
    begin
        if( ( T.Count > 0 ) or ( Tab_Tag = 0 ) ) then
        begin
            try
                T.AD.Display ;
                Memory_Scrollbar.LargeChange := T.AD.LPP ;
                Range := T.AI.High_Bound( 0 ) ;
                Range := Range - T.AI.Low_Bound( 0 ) + 1 ;
                Range_Shift := 0 ;
                if( Range < 0 ) then // Overflow
                begin
                    Range := $7FFFFFFFFFFFFFFF ;
                    Range_Shift := 1 ;
                end ;
                Range := Range div T.AD.BPL ;
                if( Range < 0 ) then
                begin
                    Range := 0 ;
                end ;
                while( Range > $7FFFFFFF ) do // Overflow
                begin
                    Range := Range shr 1 ;
                    inc( Range_Shift ) ;
                end ;
                Memory_Scrollbar.Max := Range ;
                Range := T.AD.LPP shr Range_Shift ;
                if( Range < 1 ) then
                begin
                    Range := 1 ;
                end ;
                Memory_Scrollbar.LargeChange := Range ;
            except
            end ;
        end ;
    end ;
end ; // TMain_Form.Memory_BoxPaint


procedure TMain_Form.FormShow( Sender : TObject ) ;

    procedure Process_Boolean_Option( S : string ; CB : TCheckBox ;
        const Name : string ) ;

    begin
        if( copy( S, 1, length( Name ) + 1 ) = Name + '=' ) then
        begin
            S := copy( S, length( Name ) + 2, length( S ) ) ;
            CB.Checked := ( S = 'YES' ) ;
        end ;
    end ;


    procedure Process_List_Option( S : string ; CB : TComboBox ;
        const Name : string ) ;

    begin
        if( copy( S, 1, length( Name ) + 1 ) = Name + '=' ) then
        begin
            S := copy( S, length( Name ) + 2, length( S ) ) ;
            try
                CB.ItemIndex := strtoint( S ) ;
            except
            end ;
        end ;
    end ;


    procedure Process_Integer_Option( S : string ; SE : TSpinEdit ;
        const Name : string ) ;

    begin
        if( copy( S, 1, length( Name ) + 1 ) = Name + '=' ) then
        begin
            S := copy( S, length( Name ) + 2, length( S ) ) ;
            try
               SE.Value := strtoint( S ) ;
            except
            end ;
        end ;
    end ;

var F : textfile ;
    Editor : TEditor_Window ;
    Dummy, ELoop, Loop : integer ;
    Path : string ;
    S : string ;
    Saved : integer ;

begin // TMainForm.FormShow
    Options_Form.Max_Threads.Value := OS.Logical_CPUs - 1 ;

    Editor := TEditor_Window.Create( TabSheet_1 ) ;
    Editor.Parent := TabSheet_1 ;
    Editor.Visible := True ;
    Editor.Caption := '' ;
    Editor.BorderStyle := bsNone ;
    Editor.BorderIcons := [] ;
    Editor.Align := alClient ;
    Editor.SetFocus ;
    Editor.OnUpdateMenu := CB_Update_Menu ;
    Editor.On_State_Change := CB_State_Change ;
    TabSheet_1.Editor := Editor ;

    Path := paramstr( 0 ) ;
    Loop := length( Path ) ;
    while( ( Loop > 0 ) and ( pos( Path[ Loop ], ':\' ) = 0 ) ) do
    begin
        dec( Loop ) ;
    end ;
    Path := copy( Path, 1, Loop ) ;
    Saved := FileMode ;
    FileMode := fmOpenRead ;
    assignfile( F, Path + 'CEF.cfg' ) ;
    {$I-}
    reset( F ) ;
    {$I+}
    FileMode := Saved ;
    if( IOResult = 0 ) then
    begin
        Loop := 1 ;
        ELoop := 1 ;
        while( not eof( F ) ) do
        begin
            {$I-}
            readln( F, S ) ;
            {$I+}
            if( IOResult = 0 ) then
            begin
                if( copy( S, 1, 2 ) = 'F ' ) then
                begin
                    if( Loop <= high( Reopen_Menus ) ) then
                    begin
                        S := copy( S, 3, length( S ) ) ;
                        Dummy := pos( ' ', S ) ;
                        S := copy( S, Dummy + 1, length( S ) ) ;
                        if( FileExists( S ) ) then
                        begin
                            Reopen_Menus[ Loop ].Caption := '&' + inttostr( Loop ) + ' ' + S ;
                            Reopen_Menus[ Loop ].Visible := True ;
                            inc( Loop ) ;
                        end ;
                    end ;
                end else
                if( copy( S, 1, 2 ) = 'E ' ) then
                begin
                    if( ELoop <= high( EReopen_Menus ) ) then
                    begin
                        S := copy( S, 3, length( S ) ) ;
                        Dummy := pos( ' ', S ) ;
                        S := copy( S, Dummy + 1, length( S ) ) ;
                        if( FileExists( S ) ) then
                        begin
                            EReopen_Menus[ ELoop ].Caption := '&' + inttostr( ELoop ) + ' ' + S ;
                            EReopen_Menus[ ELoop ].Visible := True ;
                            inc( ELoop ) ;
                        end ;
                    end ;
                end else
                if( copy( S, 1, 2 ) = 'O ' ) then // Options
                begin
                    S := uppercase( copy( S, 3, length( S ) ) ) ;
                    Process_Boolean_Option( S, Options_Form.Generate_Listings, 'GENERATE_LISTINGS' ) ;
                    Process_Boolean_Option( S, Options_Form.Immediate_Mode_Unblock, 'IMMEDIATE_MODE_UNBLOCK' ) ;
                    Process_Boolean_Option( S, Options_Form.Clock_Enabled, 'CLOCK_ENABLED' ) ;
                    Process_Boolean_Option( S, Options_Form.Generate_XRef_List, 'XREF_LIST' ) ;
                    Process_Boolean_Option( S, Options_Form.Physical, 'PHYSICAL' ) ;
                    Process_Boolean_Option( S, Options_Form.Generate_Symbol_Table, 'SYMBOL_TABLE_LIST' ) ;

                    Process_Integer_Option( S, Options_Form.Max_Threads, 'MAX_THREADS' ) ;

                    Process_List_Option( S, Options_Form.Thread_Priority, 'THREAD_PRIORITY' ) ;
                    Process_List_Option( S, Options_Form.Clock_Mode, 'CLOCK_MODE' ) ;
                    Disable1.Checked := not Options_Form.Clock_Enabled.Checked ;
                    case Options_Form.Clock_Mode.ItemIndex of
                        0 : Default1.Checked := True ;
                        1 : Ignore1.Checked := True ;
                        2 : Synchronize1.Checked := True ;
                    end ;
                end ;
            end ; // if( IOResult = 0 )
        end ; // while( not eof( F ) )
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
    end ; // if( IOResult = 0 )

    S := Command_Line ;
    Startup_Command_File := ( copy( S, 1, 1 ) = '@' ) ;
    if( Startup_Command_File ) then
    begin
        Open_Emulator( copy( S, 2, length( S ) ) ) ;
    end ;

    Update_Disassembly ;
    Update_Stack ;

    Application.OnIdle := CB_Idle ;

    // Text substitutions...
    Label1.Caption := Text_Immediate_Mode ;
    TabSheet1.Caption := Text_Memory ;
    TabSheet2.Caption := Text_Ports ;
    File1.Caption := Text_Menu_Caption_File_amp ;
    OpenEmulator1.Caption := Text_Menu_Caption_Open_Emulator_amp ;
    Reopenemulator1.Caption := Text_Menu_Caption_Reopen_Emulator ;
    New1.Caption := Text_Menu_Caption_New_amp ;
    Open1.Caption := Text_Menu_Caption_Open_Source_amp ;
    Reopensource1.Caption := Text_Menu_Caption_Reopen_Source ;
    Save3.Caption := Text_Menu_Caption_Save_amp ;
    SaveAs1.Caption := Text_Menu_Caption_Save_As_amp ;
    SaveAll1.Caption := Text_Menu_Caption_Save_All ;
    MediaManager1.Caption := Text_Menu_Caption_Media_Manager_amp ;
    Close1.Caption := Text_Menu_Caption_Close_amp ;
    CloseAll1.Caption := Text_Menu_Caption_Close_All ;
    Printersetup1.Caption := Text_Menu_Caption_Printer_Setup ;
    Print1.Caption := Text_Menu_Caption_Print_amp ;
    Exit1.Caption := Text_Menu_Caption_Exit_amp ;
    Components1.Caption := Text_Menu_Caption_Components_amp ;
    Load1.Caption := Text_Menu_Caption_Load_amp ;
    UnloadAll1.Caption := Text_Menu_Caption_Unload_All_amp ;
    Emulatorport1.Caption := Text_Menu_Caption_Emulator_Ports_amp ;
    N18.Caption := Text_Menu_Caption_Clock_Mode_amp ;
    Default1.Caption := Text_Menu_Caption_Default_amp ;
    Ignore1.Caption := Text_Menu_Caption_Ignore_amp ;
    Synchronize1.Caption := Text_Menu_Caption_Synchronize_amp ;
    Disable1.Caption := Text_Menu_Caption_Disable_amp ;
    Unblockall1.Caption := Text_Menu_Caption_Unblock_All_amp ;
    DefaultMemory1.Caption := Text_Menu_Caption_Default_Memory_amp ;
    Assemble1.Caption := Text_Menu_Caption_Assemble_amp ;
    Assemble2.Hint := Text_Watchpoint_Watchpoint ;
    AssembleAll1.Caption := Text_Menu_Caption_Assemble_All_amp ;
    ShowErrors.Caption := Text_Menu_Caption_Show_Errors_amp ;
    Run1.Caption := Text_Menu_Caption_Run_amp ;
    Execute1.Caption := Text_Menu_Caption_Execute_amp ;
    StepOver1.Caption := Text_Menu_Caption_Step_Over_amp ;
    StepInto1.Caption := Text_Menu_Caption_Step_Into_amp ;
    Pause1.Caption := Text_Menu_Caption_Program_Pause_amp ;
    Runimmediate1.Caption := Text_Menu_Caption_Run_Immediate_amp ;
    AddWatch1.Caption := Text_Menu_Caption_Add_Watch_amp ;
    AddBreakpoint1.Caption := Text_Menu_Caption_Add_Execution_Breakpoint_amp ;
    ExecutionBreakpoints1.Caption := Text_Menu_Caption_Execution_Breakpoints ;
    AddPortBreakpoint1.Caption := Text_Menu_Caption_Add_Port_Breakpoint ;
    PortBreakpoints1.Caption := Text_Menu_Caption_Port_Breakpoints ;
    Profiling1.Caption := Text_Menu_Caption_Profiling ;
    ProfileReport1.Caption := Text_Menu_Caption_Profile_Report ;
    Trace1.Caption := Text_Menu_Caption_Trace ;
    TraceLog1.Caption := Text_Menu_Caption_Trace_Log_amp ;
    Internals1.Caption := Text_Menu_Caption_Internals_amp ;
    Options2.Caption := Text_Menu_Caption_Options_amp ;
    Help1.Caption := Text_Menu_Caption_Help_amp ;
    Help2.Caption := Text_Menu_Caption_Help_amp ;
    CEFSpecification1.Caption := Text_Menu_Caption_CEF_Specification ;
    ComponentHelp1.Caption := Text_Menu_Caption_Component_Help ;
    About1.Caption := Text_Menu_Caption_About_amp ;
    Open_Dialog.Filter := Text_File_Filter_CEF_Files + '|*.cef' ;
    Open_Dialog.Title := Text_Menu_Caption_Open_Emulator ;
    Radix1.Caption := Text_Menu_Caption_Radix_amp ;
    Binary1.Caption := Text_Menu_Caption_Binary_amp ;
    Octal1.Caption := Text_Menu_Caption_Octal_amp ;
    Decimal1.Caption := Text_Menu_Caption_Decimal_amp ;
    Hexadecimal1.Caption := Text_Menu_Caption_Hexadecimal ;
    Radix50.Caption := Text_Menu_Caption_Radix_50 ;
    Other1.Caption := Text_Menu_Caption_Other ;
    Signed1.Caption := Text_Menu_Caption_Signed_amp ;
    Size1.Caption := Text_Menu_Caption_Size_amp ;
    Byte1.Caption := Text_Menu_Caption_Byte_amp ;
    Word1.Caption := Text_Menu_Caption_Word_amp ;
    Long1.Caption := Text_Menu_Caption_Long_amp ;
    Double1.Caption := Text_Menu_Caption_Quad_amp ;
    EBCDIC1.Caption := Text_Menu_Caption_EBCDIC_amp ;
    Physical1.Caption := Text_Menu_Caption_Physical_amp ;
    AddressSpace1.Caption := Text_Menu_Caption_Address_Space ;
    Gotoaddress1.Caption := Text_Menu_Caption_Goto_Address_amp ;
    Find1.Caption := Text_Menu_Caption_Find_amp ;
    Modify1.Caption := Text_Menu_Caption_Modify_amp ;
    Watchpoints1.Caption := Text_Menu_Caption_Watchpoints ;
    Createnew1.Caption := Text_Menu_Caption_Create_New ;
    View1.Caption := Text_Menu_Caption_View ;
    Pattern1.Caption := Text_Menu_Caption_Pattern_amp ;
    Save1.Caption := Text_Menu_Caption_Save_Contents ;
    Restore1.Caption := Text_Menu_Caption_Restore_Contents ;
    Save2.Caption := Text_Menu_Caption_Save_State ;
    Restore2.Caption := Text_Menu_Caption_Restore_State ;
    Save_Memory_Dialog.Filter :=
        Text_File_Filter_CEF_Memory_Contents + '|*.cmc|' + Text_File_Filter_All_Files + '|*.*' ;
    Save_Memory_Dialog.Title := Text_Menu_Caption_Save_Memory_Contents ;
    Save_Memory_State_Dialog.Filter :=
        Text_File_Filter_CEF_Memory_State + '|*.cms|' + Text_File_Filter_All_Files + '|*.*' ;
    Save_Memory_State_Dialog.Title := Text_Menu_Caption_Save_Memory_State ;
    Restore_Memory_Dialog.Filter :=
        Text_File_Filter_CEF_Memory_Contents + '|*.cmc|' + Text_File_Filter_All_Files + '|*.*' ;
    Restore_Memory_Dialog.Title := Text_Menu_Caption_Restore_Memory ;
    Restore_memory_State_Dialog.Filter :=
        Text_File_Filter_CEF_Memory_State + '|*.cms|' + Text_File_Filter_All_Files + '|*.*' ;
    Restore_memory_State_Dialog.Title := Text_Menu_Caption_Restore_Memory_State ;
    Increment1.Caption := Text_Menu_Caption_Increment_amp ;
    Decrement1.Caption := Text_Menu_Caption_Decrement_amp ;
    Copy1.Caption := Text_Menu_Caption_Copy ;
    Paste1.Caption := Text_Menu_Caption_Paste ;
    Change1.Caption := Text_Menu_Caption_Change_amp ;
    Zero1.Caption := Text_Menu_Caption_Zero_amp ;
    AddRegisterBreakpoint1.Caption := Text_Menu_Caption_Add_Register_Breakpoint_amp ;
    RegisterBreakpoints1.Caption := Text_Menu_Caption_Register_Breakpoints_amp ;
    Add1.Caption := Text_Menu_Caption_Add_amp ;
    Edit1.Caption := Text_Menu_Caption_Edit_amp ;
    Delete1.Caption := Text_Menu_Caption_Delete_amp ;
    DeleteAll1.Caption := Text_Menu_Caption_Delete_All_amp ;
    Showsource1.Caption := Text_Menu_Caption_Show_Source_amp ;
    Gotoaddress2.Caption := Text_Menu_Caption_Goto_Address_amp ;
    GotoCurrent1.Caption := Text_Menu_Caption_Goto_Current_amp ;
    TopofStack1.Caption := Text_Menu_Caption_Top_Of_Stack_amp ;
    Gotoaddress3.Caption := Text_Menu_Caption_Goto_Address_amp ;
    Open_Source_Dialog.Title := Text_Menu_Caption_Open_Source ;
    MenuItem1.Caption := Text_Menu_Caption_Radix_amp ;
    MenuItem2.Caption := Text_Menu_Caption_Binary_amp ;
    MenuItem3.Caption := Text_Menu_Caption_Octal_amp ;
    MenuItem4.Caption := Text_Menu_Caption_Decimal_amp ;
    MenuItem5.Caption := Text_Menu_Caption_Hexadecimal ;
    MenuItem6.Caption := Text_Menu_Caption_Radix_50 ;
    MenuItem7.Caption := Text_Menu_Caption_Other ;
    MenuItem8.Caption := Text_Menu_Caption_Signed_amp ;
    MenuItem9.Caption := Text_Menu_Caption_Size_amp ;
    MenuItem10.Caption := Text_Menu_Caption_Byte_amp ;
    MenuItem11.Caption := Text_Menu_Caption_Word_amp ;
    MenuItem12.Caption := Text_Menu_Caption_Long_amp ;
    MenuItem13.Caption := Text_Menu_Caption_Quad_amp ;
    MenuItem14.Caption := Text_Menu_Caption_EBCDIC_amp ;
    MenuItem18.Caption := Text_Menu_Caption_Goto_Port_amp ;
    MenuItem20.Caption := Text_Menu_Caption_Modify_amp ;
    Read1.Caption := Text_Menu_Caption_Input_amp ;
    Output1.Caption := Text_Menu_Caption_Output_amp ;
end ; // TMain_Form.FormShow


procedure TMain_Form.FormDestroy( Sender : TObject ) ;

var I : integer ;
   Tab : TMemory_Tab ;

begin
    Main_Memory.Free ;
    Main_Memory := nil ;
    if( Memory_DLL <> 0 ) then
    begin
        FreeLibrary( Memory_DLL ) ;
        Memory_DLL := 0 ;
    end ;
    UI.Free ;
    UI := nil ;
    for I := 0 to Memory_Tabs.Count - 1 do
    begin
        Tab := TMemory_Tab( Memory_Tabs[ I ] ) ;
        Tab.Free ;
        Memory_Tabs[ I ] := nil ;
    end ;
    Component_List.Free ;
    Component_List := nil ;
    Watchpoints.Terminate ;
    Watchpoints := nil ;
    Register_Watchpoints.Terminate ;
    Register_Watchpoints := nil ;
    Clear_Watches ;
    Watches.Free ;
    Watches := nil ;
    Execution_Watchpoints.Terminate ;
    Execution_Watchpoints := nil ;
    Port_Watchpoints.Terminate ;
    Port_Watchpoints := nil ;
    Traces.Free ;
    Traces := nil ;
    Parent_Component.Free ;
    Parent_Component := nil ;
    Remove_Component_List.Free ;
    Remove_Component_List := nil ;
end ; // TMain_Form.FormDestroy


procedure TMain_Form.FormCreate( Sender : TObject ) ;

var P : function( Serial_Number : integer ; UI : TUI_Interface ) : TComponent ; stdcall ;

begin
    // Setup...
    Remove_Component_List := TList.Create ;
    Memory_Tabs := TList.Create ;

    TabSheet_1 := TCEF_TabSheet.Create( self ) ;
    TabSheet_1.PageControl := PageControl1 ;
    TabSheet_1.Align := alClient ;
    TabSheet_1.Caption := '' ;
    TabSheet_1.TabVisible := False ;

    Disassembly_Sheet := TCEF_TabSheet.Create( self ) ;
    Disassembly_Sheet.PageControl := PageControl1 ;
    Disassembly_Sheet.Align := alClient ;
    Disassembly_Sheet.Caption := Text_Caption_Disassembly ;
    Disassembly_Scrollbar := TScrollbar.Create( Disassembly_Sheet ) ;
    Disassembly_Scrollbar.Parent := Disassembly_Sheet ;
    Disassembly_Scrollbar.Align := alRight ;
    Disassembly_Scrollbar.Kind := sbVertical ;
    Disassembly_Scrollbar.OnScroll := Disassembly_ScrollbarScroll ;
    Disassembly_Scrollbar.TabStop := False ;
    Disassembly_List_Box := TListBox.Create( Disassembly_Sheet ) ;
    Disassembly_List_Box.Parent := Disassembly_Sheet ;
    Disassembly_List_Box.Align := alClient ;
    Disassembly_List_Box.TabOrder := 0 ;
    Disassembly_List_Box.Font.Name := 'Courier New' ;
    Disassembly_List_Box.OnKeyDown := Disassembly_List_BoxKeyDown ;
    Disassembly_List_Box.OnMouseUp := Disassembly_List_BoxMouseUp ;
    Disassembly_Sheet.OnResize := Disassembly_List_Resize ;

    Physical_Address := True ;
    High_Physical_Address := $7FFFFFFFFFFFFFFF ; // Default high address
    Paint_Panel := TPaint_Panel.Create( Owner ) ;
    Paint_Panel.Parent := Tabsheet1 ;
    Paint_Panel.Align := alClient ;
    Paint_Panel.BevelOuter := bvNone ;
    Paint_Panel.PopupMenu := Popup_Menu ;

    High_Port_Address := $7FFFFFFFFFFFFFFF ;
    Port_Paint_Panel := TPaint_Panel.Create( Owner ) ;
    Port_Paint_Panel.Parent := Tabsheet2 ;
    Port_Paint_Panel.Align := alClient ;
    Port_Paint_Panel.BevelOuter := bvNone ;
    Port_Paint_Panel.PopupMenu := Port_Popup_Menu ;
    Port_Paint_Panel.Tag := 1 ;

    Component_List := TStringList.Create ;
    Watchpoints := Get_Watchpoint_Manager ;
    Register_Watchpoints := Get_Watchpoint_Manager ;
    Watches := TList.Create ;
    Execution_Watchpoints := Get_Watchpoint_Manager ;
    Port_Watchpoints := Get_Watchpoint_Manager ;
    Traces := TStringList.Create ;

    Reopen_Menus[ 1 ] := N11 ;
    Reopen_Menus[ 2 ] := N21 ;
    Reopen_Menus[ 3 ] := N31 ;
    Reopen_Menus[ 4 ] := N41 ;
    Reopen_Menus[ 5 ] := N51 ;
    Reopen_Menus[ 6 ] := N61 ;
    Reopen_Menus[ 7 ] := N71 ;
    Reopen_Menus[ 8 ] := N81 ;
    Reopen_Menus[ 9 ] := N91 ;

    EReopen_Menus[ 1 ] := N13 ;
    EReopen_Menus[ 2 ] := N23 ;
    EReopen_Menus[ 3 ] := N33 ;
    EReopen_Menus[ 4 ] := N43 ;
    EReopen_Menus[ 5 ] := N53 ;
    EReopen_Menus[ 6 ] := N63 ;
    EReopen_Menus[ 7 ] := N73 ;
    EReopen_Menus[ 8 ] := N83 ;
    EReopen_Menus[ 9 ] := N93 ;

    if( UI = nil ) then
    begin
        UI := TCEF32_Interface.Create ;
    end ;

    if( Main_Memory = nil ) then
    begin
        Memory_DLL := LoadLibrary( PChar( Qualify_Filename( 'CEFMemory', Program_Path + 'Components\', OS^.Shared_Library_Extension ) ) ) ;
        if( Memory_DLL <> 0 ) then
        begin
            P := GetProcAddress( Memory_DLL, 'Make_Instance' ) ;
            if( assigned( P ) ) then
            begin
                Main_Memory := P( 0, UI ) ;
                Main_Memory.Tag := integer( TComponent_Info.Create ) ;
                TComponent_Info( Main_Memory.Tag ).DLL_Handle := Memory_DLL ;
                TComponent_Info( Main_Memory.Tag ).Menu :=
                    Configure_Component_Menus( Main_Memory, DefaultMemory1 ) ;
            end ;
        end ;
    end ;
    if( Main_Memory_Tab = nil ) then
    begin
        Main_Memory_Tab := TMemory_Tab.Create ;
        Main_Memory_Tab.AI := TMemory_Array_Interface.Create ;
        Main_Memory_Tab.AD := TArray_Display.Create( Paint_Panel,
            Main_Memory_Tab.AI, Font.Handle, clWindow, clWindowText ) ;
        Main_Memory_Tab.Name := 'main' ;
        Memory_Tabs.Add( Main_Memory_Tab ) ;
    end ;
    if( Port_Tab = nil ) then
    begin
        Port_Tab := TMemory_Tab.Create ;
        Port_Tab.AI := TPort_Array_Interface.Create ;
        Port_Tab.AD := TArray_Display.Create( Port_Paint_Panel, Port_Tab.AI,
            Font.Handle, clWindow, clWindowText ) ;
        Port_Tab.Name := 'main' ;
        Memory_Tabs.Add( Port_Tab ) ;
    end ;

    Parent_Component := TComponent_Parent.Create ;
end ; // TMain_Form.FormCreate


procedure TMain_Form.Internals1Click( Sender : TObject ) ;

begin
    Begin_Debugger ;
end ;


procedure TMain_Form.EBCDIC1Click( Sender : TObject ) ;

begin
    if( EBCDIC1.Caption = Text_Menu_Caption_ASCII_amp ) then
    begin
        EBCDIC1.Caption := Text_Menu_Caption_EBCDIC_amp ;
        Current_Tab.AD.Ascii_Ebcdic := 0 ;
        Port_Tab.AD.Ascii_Ebcdic := 0 ;
    end else
    begin
        EBCDIC1.Caption := Text_Menu_Caption_ASCII_amp ;
        Current_Tab.AD.Ascii_Ebcdic := 1 ;
        Port_Tab.AD.Ascii_Ebcdic := 1 ;
    end ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
end ;


procedure TMain_Form.Signed1Click( Sender : TObject ) ;

begin
    Signed1.Checked := not Signed1.Checked ;
    Current_Tab.AD.Unsigned := not Signed1.Checked ;
    Port_Tab.AD.Unsigned := not Signed1.Checked ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
end ;


procedure TMain_Form.Binary1Click( Sender : TObject ) ;

begin
    Current_Tab.AD.Base := 2 ;
    Port_Tab.AD.Base := 2 ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    Binary1.Checked := True ;
end ;


procedure TMain_Form.Octal1Click( Sender : TObject ) ;

begin
    Current_Tab.AD.Base := 8 ;
    Port_Tab.AD.Base := 8 ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    Octal1.Checked := True ;
end ;


procedure TMain_Form.Decimal1Click( Sender : TObject ) ;

begin
    Current_Tab.AD.Base := 10 ;
    Port_Tab.AD.Base := 10 ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    Decimal1.Checked := True ;
end ;


procedure TMain_Form.Hexadecimal1Click( Sender : TObject ) ;

begin
    Current_Tab.AD.Base := 16 ;
    Port_Tab.AD.Base := 16 ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    Hexadecimal1.Checked := True ;
end ;


procedure TMain_Form.Other1Click( Sender : TObject ) ;

begin
    Radix_Form.Spin_Edit.Value := Current_Tab.AD.Base ;
    if( Radix_Form.ShowModal = mrOK ) then
    begin
        Current_Tab.AD.Base := Radix_Form.Spin_Edit.Value ;
        Port_Tab.AD.Base := Radix_Form.Spin_Edit.Value ;
        Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
        case Current_Tab.AD.Base of
            2 : Binary1.Checked := True ;
            8 : Octal1.Checked := True ;
            10 : Decimal1.Checked := True ;
            16 : Hexadecimal1.Checked := True ;
            else Other1.Checked := True ;
        end ;
    end ;
end ;


procedure TMain_Form.Byte1Click( Sender : TObject ) ;

begin
    Set_Number_Size( 1 ) ;
end ;


procedure TMain_Form.Word1Click( Sender : TObject ) ;

begin
    Set_Number_Size( 2 ) ;
end ;


procedure TMain_Form.Long1Click( Sender : TObject ) ;

begin
    Set_Number_Size( 4 ) ;
end ;


procedure TMain_Form.Double1Click( Sender : TObject ) ;

begin
    Set_Number_Size( 8 ) ;
end ;


procedure TMain_Form.Gotoaddress1Click( Sender : TObject ) ;

begin
    Goto_Address_Form.Base := Current_Tab.AD.Base ;
    if( Goto_Address_Form.ShowModal = mrOK ) then
    begin
        Current_Tab.AD.Home_Index :=
            strtoint( Cvtb( Current_Tab.AD.Base, 10, Goto_Address_Form.ComboBox1.Text ) ) ;
        Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    end ;
end ;


procedure TMain_Form.Find1Click( Sender : TObject ) ;

var B : char ;
    I : int64 ;
    Matched : integer ; // Number of bytes matched
    S : string ;
    Tab : TMemory_Tab ;

begin
    Tab := Current_Tab ;
    if( Tab = Port_Tab ) then // Don't search ports
    begin
        Tab := TMemory_Tab( Memory_Tabs[ 0 ] ) ;
    end ;
    Find_Form.Base := Tab.AD.Base ;
    if( Find_Form.ShowModal = mrOK ) then
    begin
        S := Find_Form.Find_Image ;
        if( Find_Form.Literal_Text.Checked ) then
        begin
            if( not Find_Form.Case_Sensitive.Checked ) then
            begin
                S := uppercase( S ) ;
            end ;
            if( EBCDIC1.Caption = Text_Menu_Caption_EBCDIC_amp ) then
            begin
                S := From_EBCDIC( S ) ;
            end ;
        end ;

        // Now search...
        I := Tab.AD.Home_Index ;
        Matched := 0 ;
        while( ( I >= 0 ) and ( I <= High_Address ) ) do
        begin
            B := char( Tab.AI.Get_Byte( I ) ) ;
            if( not Find_Form.Case_Sensitive.Checked ) then
            begin
                B := upcase( B ) ;
            end ;
            if( B <> S[ Matched + 1 ] ) then
            begin
                Matched := 0 ;
            end else
            begin
                inc( Matched ) ;
            end ;
            if( Matched = length( S ) ) then // Found entire data
            begin
                Tab.AD.Home_Index := I ;
                exit ;
            end ;
            I := I + 1 ;
        end ;
        ShowMessage( Text_Error_Nothing_Found ) ;
    end ; // if( Find_Form.ShowModal = mrOK )
end ; // TMain_Form.Find1Click


procedure TMain_Form.Radix50Click( Sender : TObject ) ;

begin
    Current_Tab.AD.Base := 50 ;
    Port_Tab.AD.Base := 50 ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    Radix50.Checked := True ;
end ;


procedure TMain_Form.Save1Click( Sender : TObject ) ;

var C : TComponent ;
    Stream : TFile_Streamer ;

begin
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    if( Save_Memory_Dialog.Execute ) then
    begin
        Stream := TFile_Streamer.Create( Save_Memory_Dialog.FileName, True ) ;
        C.Save_Contents( Stream ) ;
        Stream.Free ;
    end ;
end ;


procedure TMain_Form.Restore1Click( Sender : TObject ) ;

var C : TComponent ;
    Stream : TFile_Streamer ;

begin
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    if( Restore_Memory_Dialog.Execute ) then
    begin
        Stream := TFile_Streamer.Create( Restore_Memory_Dialog.FileName, False ) ;
        C.Restore_Contents( Stream ) ;
        Stream.Free ;
    end ;
end ;


procedure TMain_Form.Save2Click( Sender : TObject ) ;

var C : TComponent ;
    Stream : TFile_Streamer ;

begin
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    if( Save_Memory_State_Dialog.Execute ) then
    begin
        Stream := TFile_Streamer.Create( Save_Memory_State_Dialog.FileName, True ) ;
        C.Save_State( Stream ) ;
        Stream.Free ;
    end ;
end ;


procedure TMain_Form.Restore2Click( Sender : TObject ) ;

var C : TComponent ;
    Stream : TFile_Streamer ;

begin
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    if( Restore_Memory_State_Dialog.Execute ) then
    begin
        Stream := TFile_Streamer.Create( Restore_Memory_State_Dialog.FileName, False ) ;
        C.Restore_State( Stream ) ;
        Stream.Free ;
    end ;
end ;


procedure TMain_Form.Physical1Click( Sender : TObject ) ;

begin
    if( Physical1.Caption = Text_Caption_Physical_amp ) then
    begin
        Physical1.Caption := Text_Caption_Logical_amp ;
        Physical_Address := False ;
    end else
    begin
        Physical1.Caption := Text_Caption_Physical_amp ;
        Physical_Address := True ;
    end ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    Update_Disassembly ;
end ;



procedure TMain_Form.New1Click( Sender : TObject ) ;

var Editor : TEditor_Window ;
    Tab_Control : TCEF_TabSheet ;

begin
    // Create a new tab with editor...
    Tab_Control := TCEF_TabSheet.Create( self ) ;
    Tab_Control.PageControl := PageControl1 ;
    Tab_Control.Align := alClient ;
    Tab_Control.Caption := Text_Noname ;

    Editor := TEditor_Window.Create( Tab_Control ) ;
    Editor.Parent := Tab_Control ;
    Editor.Visible := True ;
    Editor.Caption := '' ;
    Editor.BorderStyle := bsNone ;
    Editor.BorderIcons := [] ;
    Editor.Align := alClient ;
    Tab_Control.Editor := Editor ;
    PageControl1.ActivePage := Tab_Control ;
    Editor.OnUpdateMenu := CB_Update_Menu ;
    Editor.On_Kill := CB_Kill ;
    Editor.On_State_Change := CB_State_Change ;
    Editor.SetFocus ;
    PageControl1Change( nil ) ; // Adjust menus

    Assemble2.Enabled := True ;
    Close1.Enabled := True ;
end ; // TMain_Form.New1Click


procedure TMain_Form.Save3Click(Sender: TObject);

var Editor : TEditor_Window ;

begin
    Editor := TEditor_Window( PageControl1.ActivePage.Controls[ 0 ] ) ;
    if( ( Editor.File_Name = Text_Noname ) and ( Editor.Max_Line = 0 ) ) then
    begin
        exit ;
    end ;

    if( Editor.New_File ) then
    begin
        SaveAs1Click( Sender ) ;
    end else
    begin
        Editor.Save_File ;
    end ;
end ;


function TMain_Form.phGet_Master_Assembler : TMaster_Assembler ;

begin
    if( _Master_Assembler = nil ) then
    begin
        _Master_Assembler := Create_Assembler ;
    end ;
    Result := _Master_Assembler ;
end ;


procedure TMain_Form.phSet_Master_Assembler( Value : TMaster_Assembler ) ;

begin
    if( _Master_Assembler <> nil ) then
    begin
        _Master_Assembler.Terminate ;
    end ;
    _Master_Assembler := Value ;
end ;


procedure TMain_Form.SaveAs1Click( Sender : TObject ) ;

var _Assembler : TAssembler ;
    Dummy : integer ;
    Editor : TEditor_Window ;
    F : textfile ;
    S : string ;
    Starting, Ending, Temp : int64 ;
    Tab : TMemory_Tab ;

begin
    // For disassembly...
    if( PageControl1.ActivePage = Disassembly_Sheet ) then
    begin
        Dummy := 0 ;
        if( Master_CPU <> nil ) then
        begin
            S := string( Master_CPU.CPU.Get_Assembler( Master_Assembler ).Source_Extensions ) ;
            Dummy := pos( '|', S ) ;
        end ;
        if( ( Master_CPU = nil ) or ( Dummy = 0 ) ) then
        begin
            Save_Disassembly_Form.Save_Dialog.Filter :=
               Text_File_Filter_Text_Files + '|*.txt|' + Text_File_Filter_All_Files + '|*.*' ;
        end else
        begin
            S := copy( S, 1, Dummy - 1 ) ;
            Save_Disassembly_Form.Save_Dialog.Filter :=
                Text_File_Filter_Source_Files + '|*.' + S + Text_File_Filter_All_Files + '*.*' ;
        end ;
        Save_Disassembly_Form.Base := Main_Memory_Tab.AD.Base ; //~~~
        if( ( Disassembly_Top = -1 ) and ( Master_CPU <> nil ) ) then
        begin
            Starting := Master_CPU.CPU.Get_Current_Address( 0, True ) ;
        end else
        begin
            Starting := Disassembly_Top ;
        end ;
        Save_Disassembly_Form.Starting_Address.Text :=
            CVTB( 10, Main_Memory_Tab.AD.Base, inttostr( Starting ) ) ;
        Save_Disassembly_Form.Ending_Address.Text :=
            Save_Disassembly_Form.Starting_Address.Text ;
        if( Save_Disassembly_Form.ShowModal = mrOK ) then
        begin
            // Setup...
            if( ( Master_CPU = nil ) or ( Master_CPU.Version < 26 ) ) then
            begin
                Tab := Main_Memory_Tab ;
            end else
            begin
                Tab := Memory_Tab_With_Component( Main_Form.Master_CPU.CPU.Get_Target_Memory ) ;
                if( Tab = nil ) then
                begin
                    Tab := Main_Memory_Tab ;
                end ;
            end ;
            Starting := strtoint( CVTB( Tab.AD.Base, 10, Save_Disassembly_Form.Starting_Address.Text ) ) ;
            Ending := strtoint( CVTB( Tab.AD.Base, 10, Save_Disassembly_Form.Ending_Address.Text ) ) ;
            _Assembler := nil ;
            if( Master_CPU <> nil ) then
            begin
                Status := TCEF_Assembler_Status.Create ;
                _Assembler := Master_CPU.CPU.Get_Assembler( Master_Assembler ) ;
            end ;
            assignfile( F, Save_Disassembly_Form.File_Name.Text ) ;
            {$I-}
            rewrite( F ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( Substitute2( Text_Error_While_Creating_File, ERT( Dummy ), Save_Disassembly_Form.File_Name.Text ) ) ;
                exit ;
            end ;

            // Build file...
            try
                {$I-}
                writeln( F, '.RADIX ', Tab.AD.Base ) ;
                {$I+}
                Dummy := IOResult ;
                if( Dummy <> 0 ) then
                begin
                    ShowMessage( Substitute2( Text_Error_While_Creating_File, ERT( Dummy ), Save_Disassembly_Form.File_Name.Text ) ) ;
                    exit ;
                end ;
                if( not ( Save_Disassembly_Form.Show_Address.Checked or Save_Disassembly_Form.Show_Values.Checked ) ) then
                begin
                    {$I-}
                    writeln( F, '.ORG ', Save_Disassembly_Form.Starting_Address.Text ) ;
                    {$I+}
                    Dummy := IOResult ;
                    if( Dummy <> 0 ) then
                    begin
                        ShowMessage( Substitute2( Text_Error_While_Creating_File, ERT( Dummy ), Save_Disassembly_Form.File_Name.Text ) ) ;
                        exit ;
                    end ;
                end ;
                while( Starting <= Ending ) do
                begin
                    Temp := Starting ;
                    S := Get_Disassembly( Starting, Temp,
                        Save_Disassembly_Form.Show_Address.Checked,
                        Save_Disassembly_Form.Show_Values.Checked, _Assembler ) ;
                    {$I-}
                    writeln( F, S ) ;
                    {$I+}
                    Dummy := IOResult ;
                    if( Dummy <> 0 ) then
                    begin
                        ShowMessage( Substitute2( Text_Error_While_Creating_File, ERT( Dummy ), Save_Disassembly_Form.File_Name.Text ) ) ;
                        exit ;
                    end ;
                end ;
            finally
                {$I-}
                closefile( F ) ;
                {$I+}
                IOResult ;
                if( _Assembler <> nil ) then
                begin
                    _Assembler.Terminate ;
                end ;
            end ;
        end ; // if( Save_Disassembly_Form.ShowModal = mrOK )
        exit ;
    end ; // if( PageControl1.ActivePage = Disassembly_Sheet )

    // For editors...
    Editor := TEditor_Window( PageControl1.ActivePage.Controls[ 0 ] ) ;
    Save_Dialog.FileName := Editor.File_Name ;
    if( Save_Dialog.Execute ) then
    begin
        Editor.File_Name := Save_Dialog.FileName ;
        PageControl1.ActivePage.Caption := Save_Dialog.FileName ;
        Editor.New_File := False ;
        Save3Click( Sender ) ;
    end ;
end ; // TMain_Form.SaveAs1Click


procedure TMain_Form.SaveAll1Click( Sender : TObject ) ;

var Editor : TEditor_Window ;
    Loop : integer ;

begin
    for Loop := 0 to PageControl1.PageCount - 1 do
    begin
        Editor := TCEF_Tabsheet( PageControl1.Pages[ Loop ] ).Editor ;
        if(
            ( Editor <> nil )
            and
            ( ( Editor.File_Name <> Text_Noname ) or ( Editor.Max_Line > 0 ) )
          ) then
        begin
            if( Editor.New_File ) then
            begin
                Save_Dialog.FileName := Editor.File_Name ;
                if( Save_Dialog.Execute ) then
                begin
                    Editor.File_Name := Save_Dialog.FileName ;
                    PageControl1.Pages[ Loop ].Caption := Save_Dialog.FileName ;
                    Editor.New_File := False ;
                    Save3Click( Sender ) ;
                end ;
            end else
            begin
                Editor.Save_File ;
            end ;
        end ;
    end ;
end ;


procedure TMain_Form.OpenEmulator1Click( Sender : TObject ) ;

begin
    if( Open_Dialog.Execute ) then
    begin
        _Hide := False ;
        Open_Emulator( Open_Dialog.Filename ) ;
        if( _Hide ) then
        begin
            Hide ;
            _Hide := False ;
        end ;
    end ; // if( Open_Dialog.Execute )
end ; // TMain_Form.OpenEmulator1Click


procedure TMain_Form.Add_To_Reopen( const S : string ) ;

var Loop : integer ;

begin
    // First, see if file is already in a menu
    for Loop := 1 to 9 do
    begin
        if( Reopen_Menus[ Loop ].Visible ) then
        begin
            if( uppercase( copy( Reopen_Menus[ Loop ].Caption, 4, 255 ) ) = uppercase( S ) ) then
            begin
                exit ; // Already in menus
            end ;
        end ;
    end ;

    // Now find a place to add the file
    for Loop := 1 to 9 do
    begin
        if( not Reopen_Menus[ Loop ].Visible ) then
        begin
            Reopen_Menus[ Loop ].Visible := True ;
            Reopen_Menus[ Loop ].Caption := '&' + inttostr( Loop ) + ' ' + S ;
            exit ;
        end ;
    end ;

    // No room, so shift existing files and add at the end...
    for Loop := 2 to 9 do
    begin
        Reopen_Menus[ Loop - 1 ].Visible := Reopen_Menus[ Loop ].Visible ;
        Reopen_Menus[ Loop - 1 ].Caption := Reopen_Menus[ Loop ].Caption ;
    end ;
    Reopen_Menus[ 9 ].Visible := True ;
    Reopen_Menus[ 9 ].Caption := '&9 ' + S ;
end ; // Add_To_Reopen


procedure TMain_Form.Assemble2Click( Sender : TObject ) ;

var Editor : TEditor_Window ;
    Info : TComponent_Info ;
    Loop : integer ;
    Out_Streamer : TOutput_Streamer ;
    T, F : PChar ;
    L, S : longint ;

begin
    if( In_Assembly or In_Execution ) then
    begin
        exit ;
    end ;
    if( PageControl1.ActivePage = Disassembly_Sheet ) then
    begin
        exit ; // Cannot assemble the disassembly
    end ;
    ShowErrors.Enabled := False ;
    In_Assembly := True ;
    Write_Errors := False ;
    StatusBar.Panels.Items[ 2 ].Text := Text_Status_Assembling ;
    Master_Assembler := nil ; // Kill old assembler context
    Info := TComponent_Info( Master_CPU.Tag ) ;
    if( Info.Assembler_Context <> nil ) then
    begin
        Info.Assembler_Context.Terminate ;
        Info.Assembler_Context := nil ;
    end ;
    Editor := TEditor_Window( PageControl1.ActivePage.Controls[ 0 ] ) ;
    Status := TCEF_Assembler_Status.Create ;
    try
        Status.Current_File := Editor.File_Name ;
        Assembly_Statistics.Abort_Button.Caption := Text_Button_Caption_Abort ;
        Assembly_Statistics.Show ;
        Assemble( Editor, Status ) ;
        if( ( Master_Assembler <> nil ) and ( not Assembly_Statistics.Aborted ) ) then
        begin
            Out_Streamer := TOutput_Streamer.Create ;
            try
                try
                    Master_Assembler.Backpatch( Status, Out_Streamer ) ;
                except
                end ;
            finally
                Out_Streamer.Free ;
            end ;
            Info.Assembler_Context := Master_Assembler.Assembler_Context ;
            Master_Assembler.Assembler_Context := nil ;
            Status.Update_Display ;
        end ;
        Assembly_Statistics.Code_Label.Caption := inttostr( Status.Code ) ;
        Assembly_Statistics.Data_Label.Caption := inttostr( Status.Data ) ;
        Assembly_Statistics.Abort_Button.Caption := Text_Button_Caption_Close ;
        StatusBar.Panels.Items[ 2 ].Text := '' ;
        Assembly_Statistics.List_Box.Clear ;
        Assembly_Statistics.Filenames.Clear ;
        Assembly_Statistics.Lines.Clear ;
        for Loop := 0 to Status.Error_List.Count - 1 do
        begin
            Assembly_Statistics.List_Box.Items.Add( Status.Error_List[ Loop ] ) ;
            Status.Get_Error( Loop, T, F, L, S ) ;
            Assembly_Statistics.Filenames.Add( string( F ) ) ;
            Assembly_Statistics.Lines.Add( L ) ;
        end ;
    finally
        Status.Free ;
        Status := nil ;
    end ;
    In_Assembly := False ;
    Disassembly_Top := -1 ;
    Stack_Top := -1 ;
    Update_Disassembly ;
    Update_CPU_State( False ) ;
    Current_Tab.AD.Display ;
end ; // TMain_Form.Assemble2Click


procedure TMain_Form.AssembleAll1Click( Sender : TObject ) ;

var Editor : TEditor_Window ;
    Info : TComponent_Info ;
    Loop : integer ;
    Out_Streamer : TOutput_Streamer ;
    T, F : PChar ;
    L, S : longint ;

begin
    if( In_Assembly or In_Execution ) then
    begin
        exit ;
    end ;
    ShowErrors.Enabled := False ;
    In_Assembly := True ;
    Write_Errors := False ;
    StatusBar.Panels.Items[ 2 ].Text := Text_Status_Assembling ;
    Master_Assembler := nil ; // Kill old assembler context
    Info := TComponent_Info( master_CPU.Tag ) ;
    if( Info.Assembler_Context <> nil ) then
    begin
        Info.Assembler_Context.Terminate ;
        Info.Assembler_Context := nil ;
    end ;
    Status := TCEF_Assembler_Status.Create ;
    Assembly_Statistics.Abort_Button.Caption := Text_Button_Caption_Abort ;
    Assembly_Statistics.Show ;
    try
        for Loop := 0 to PageControl1.PageCount - 1 do
        begin
            if( TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor <> nil ) then
            begin
                Editor := TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor ;
                Status.Current_File := Editor.File_Name ;
                if( not Assemble( Editor, Status ) ) then
                begin
                    exit ;
                end ;
                Assembly_Statistics.Code_Label.Caption := inttostr( Status.Code ) ;
                Assembly_Statistics.Data_Label.Caption := inttostr( Status.Data ) ;
                if( Status.Aborted ) then
                begin
                    exit ;
                end ;
            end ;
        end ;
    finally
        StatusBar.Panels.Items[ 2 ].Text := '' ;
        Assembly_Statistics.Visible := False ;
        Assembly_Statistics.Code_Label.Caption := inttostr( Status.Code ) ;
        Assembly_Statistics.Data_Label.Caption := inttostr( Status.Data ) ;
        Assembly_Statistics.Abort_Button.Caption := Text_Button_Caption_Close ;
        if( ( Master_Assembler <> nil ) and ( not Assembly_Statistics.Aborted ) ) then
        begin
            Out_Streamer := TOutput_Streamer.Create ;
            Master_Assembler.Backpatch( Status, Out_Streamer ) ;
            Out_Streamer.Free ;
            Info.Assembler_Context := Master_Assembler.Assembler_Context ;
            Master_Assembler.Assembler_Context := nil ;
        end ;
        ShowErrors.Enabled := Status.Errors + Status.Warnings > 0 ;
        Assembly_Statistics.List_Box.Clear ;
        Assembly_Statistics.Filenames.Clear ;
        Assembly_Statistics.Lines.Clear ;
        for Loop := 0 to Status.Error_List.Count - 1 do
        begin
            Assembly_Statistics.List_Box.Items.Add( Status.Error_List[ Loop ] ) ;
            Status.Get_Error( Loop, T, F, L, S ) ;
            Assembly_Statistics.Filenames.Add( string( F ) ) ;
            Assembly_Statistics.Lines.Add( L ) ;
        end ;
        Assembly_Statistics.Show ;
        Status.Free ;
        Status := nil ;
        In_Assembly := False ;
        Disassembly_Top := -1 ;
        Stack_Top := -1 ;
        Update_Disassembly ;
        Update_CPU_State( False ) ;
    end ;
end ; // TMain_Form.AssembleAll1Click


// Internal utility routines...

function TMain_Form.Assemble( Editor : TEditor_Window ;
    Status : TCEF_Assembler_Status ) : boolean ;

var Dummy : integer ;
    In_Streamer : TEditor_Streamer ;
    Out_Streamer : TOutput_Streamer ;
    S : string ;
    UEC : TUnified_Exception ;

begin
    Status.Allow_Listing := Options_Form.Generate_Listings.Checked ;
    try
        if( Status.Allow_Listing ) then
        begin
            S := Status.Current_File ;
            Dummy := Extension_Pos( S ) ;
            S := copy( S, 1, Dummy ) + 'lst' ;
            assignfile( Status.Listing_File, S ) ;
            {$I-}
            rewrite( Status.Listing_File ) ;
            {$I+}
            if( IOResult <> 0 ) then
            begin
                Status.Allow_Listing := False ;
            end ;
        end ;

        In_Streamer := TEditor_Streamer.Create( Editor ) ;
        Out_Streamer := TOutput_Streamer.Create ;
        if( Master_CPU <> nil ) then
        begin
            Out_Streamer.Target := Master_CPU.CPU.Get_Target_Memory ;
        end ;
        try
            TCEF_Assembler_Status( Status ).In_Streamer := In_Streamer ;
            if( Master_Assembler.Version < 22 ) then
            begin
                UEC := Master_Assembler.Assemble( In_Streamer, Out_Streamer, nil, Status ) ;
            end else
            begin
                Dummy := ASF_Extended ;
                if( Options_Form.Generate_Symbol_Table.Checked ) then
                begin
                    Dummy := Dummy or ASF_Want_Symbol_Table ;
                end ;
                if( Options_Form.Generate_XRef_List.Checked ) then
                begin
                    Dummy := Dummy or ASF_Want_XRef ;
                end ;
                if( Options_Form.Physical.Checked ) then
                begin
                    Dummy := Dummy or ASF_Generate_Virtual ;
                end ;
                UEC := Master_Assembler.Assemble_Ex( In_Streamer, Out_Streamer, nil, Status, Dummy ) ;
            end ;
            TCEF_Assembler_Status( Status ).In_Streamer := nil ;
            Result := ( UEC = nil ) ;
            Status.Code := Status.Code - Status.Data ;
        finally
            In_Streamer.Free ;
            Out_Streamer.Free ;
        end ;

        Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
        Update_Watches ;
    finally
        if( Status.Allow_Listing ) then
        begin
            {$I-}
            closefile( Status.Listing_File ) ;
            {$I+}
            IOResult ;
        end ;
        Status.Allow_Listing := False ;
    end ;
end ; // TMain_Form.Assemble


function TMain_Form.Configure_Component_Menus( C : TComponent ;
    M : TMenuItem ) : TMenuItem ;

var Menu : TMenuItem ;

begin
    Menu := TMenuItem.Create( M ) ;
    M.Tag := integer( C ) ;
    Result := Menu ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Reset_amp ;
    Menu.OnClick := Reset1Click ;
    M.Add( Menu ) ;

    if( C.Component_Type = Component_Type_CPU ) then
    begin
        Menu := TMenuItem.Create( M ) ;
        Menu.Tag := integer( C ) ;
        Menu.Caption := Text_Menu_Caption_Restart_amp ;
        Menu.OnClick := Restart1Click ;
        M.Add( Menu ) ;
    end ;

    if( C.Component_Type = Component_Type_UI ) then
    begin
        Menu := TMenuItem.Create( M ) ;
        Menu.Tag := integer( C ) ;
        Menu.Caption := Text_Menu_Caption_Embed ;
        Menu.OnClick := ToggleEmbedClick ;
        M.Add( Menu ) ;
    end ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Caption := '-' ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Save_Contents ;
    Menu.OnClick := Save1Click ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Restore_Contents ;
    Menu.OnClick := Restore1Click ;
    M.Add( Menu ) ;

    if( C.Component_Type = Component_Type_Memory ) then
    begin
        Menu := TMenuItem.Create( M ) ;
        Menu.Tag := integer( C ) ;
        Menu.Caption := Text_Menu_Caption_Dump ;
        Menu.OnClick := Dump1Click ;
        M.Add( Menu ) ;

        Menu := TMenuItem.Create( M ) ;
        Menu.Tag := integer( C ) ;
        Menu.Caption := Text_Menu_Caption_Load ;
        Menu.OnClick := LoadMem1Click ;
        M.Add( Menu ) ;
    end ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Caption := '-' ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Save_State ;
    Menu.OnClick := Save2Click ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Restore_State ;
    Menu.OnClick := Restore2Click ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Caption := '-' ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Profile_amp ;
    Menu.OnClick := ProfileClick ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Profile_Report ;
    Menu.OnClick := ProfileReportClick ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Caption := '-' ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Connections_amp ;
    Menu.OnClick := ConnectionsClick ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Condition_Handling_amp ;
    Menu.OnClick := ConditionsClick ;
    M.Add( Menu ) ;

    Menu := TMenuItem.Create( M ) ;
    Menu.Tag := integer( C ) ;
    Menu.Caption := Text_Menu_Caption_Configure_amp ;
    Menu.OnClick := ConfigureClick ;
    M.Add( Menu ) ;

    if( C <> Main_Memory ) then
    begin
        Menu := TMenuItem.Create( M ) ;
        Menu.Tag := integer( C ) ;
        Menu.Caption := Text_Menu_Caption_Unload_amp ;
        Menu.OnClick := UnloadClick ;
        M.Add( Menu ) ;
    end ;
end ; // TMain_Form.Configure_Component_Menusend ;


function TMain_Form.Create_Assembler : TMaster_Assembler ;//TCEF_Assembler ;

var C : TComponent ;
    Loop : integer ;

begin
    Result := Get_Master_Assembler( Master_CPU, UI ) ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        C := TComponent( Component_List.Objects[ Loop ] ) ;
        if( C.CPU <> nil ) then
        begin
            Result.Add_CPU( C, PChar( Component_List[ Loop ] ) ) ;
        end ;
    end ;
end ;


procedure TMain_Form.Open_Emulator( Filename : string ) ;

    function Find_Component( S : string ) : TComponent ;

    var Loop : integer ;

    begin
        S := uppercase( S ) ;
        if( S = 'DEFAULT MEMORY' ) then
        begin
            Result := Main_Memory ;
            exit ;
        end ;
        Result := nil ;
        for Loop := 0 to Component_List.Count - 1 do
        begin
            if( S = uppercase( Component_List[ Loop ] ) ) then
            begin
                Result := TComponent( Component_List.Objects[ Loop ] ) ;
                exit ;
            end ;
        end ;
    end ;

var C, C1 : TComponent ;
    Domain : string ;
    Dummy : integer ;
    F : textfile ;
    Loop : integer ;
    Name, S, S1 : string ;
    Parser : TStrings_Parser ;
    Saved_B : boolean ;
    Saved : integer ;
    SL : TStringList ;

begin // TMain_Form.Open_Emulator
    Filename := Qualify_Filename( Filename, Program_Path, '.cef' ) ;
    Saved := FileMode ;
    FileMode := fmOpenRead ;
    assignfile( F, Filename ) ;
    {$I-}
    reset( F ) ;
    {$I+}
    FileMode := Saved ;
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( Substitute2( Text_Error_While_Opening_File, ERT( Dummy ), Filename ) ) ;
        exit ;
    end ;
    SL := TStringList.Create ;
    try
        while( not eof( F ) ) do
        begin
            {$I-}
            readln( F, S ) ;
            {$I+}
            Dummy := IOResult ;
            {$I-}
            if( Dummy <> 0 ) then
            begin
                ShowMessage( substitute2( Text_Error_While_Reading_File, ERT( Dummy ), Filename ) ) ;
                exit ;
            end ;
            SL.Add( S ) ;
        end ;
        closefile( F ) ;
        {$I+}
        Clear_IOResult ;
        Start_Execution := False ;

        // Load emulator definition
        Parser := TStrings_Parser.Create ;
        try
            Parser.Set_Source( SL ) ;
            S := Parser.Token ;
            while( S <> '' ) do
            begin
                if( S = ';' ) then // Comment
                begin
                    Parser.Grab_Line ; // Eat rest of line
                end else
                if( S = 'ASCII' ) then
                begin
                    EBCDIC1.Caption := Text_Menu_Caption_ASCII_amp ;
                    Main_Memory_Tab.AD.Ascii_Ebcdic := 0 ;
                    Port_Tab.AD.Ascii_Ebcdic := 0 ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'BYTE' ) then
                begin
                    Set_Number_Size( 1 ) ;
                end else
                if( S = 'CAPTION' ) then
                begin
                    Caption := Trim_Quotes( Parser.Token ) ;
                end else
                if( S = 'CONNECT' ) then
                begin
                    S := Trim_Quotes( Parser.Token ) ;
                    S1 := Trim_Quotes( Parser.Token ) ;
                    C := Find_Component( S ) ;
                    C1 := Find_Component( S1 ) ;
                    if( C = nil ) then
                    begin
                        ShowMessage( Substitute1( Text_Error_Cannot_Find_Component, S ) ) ;
                        exit ;
                    end ;
                    if( C1 = nil ) then
                    begin
                        ShowMessage( Substitute1( Text_Error_Cannot_Find_Component, S1 ) ) ;
                        exit ;
                    end ;
                    C.Connect_Output( C1 ) ;
                    C1.Connect_Input( C ) ;
                    C.Connect_Input( C1 ) ;
                    C1.Connect_Output( C ) ;
                end else
                if( S = 'DISCONNECT' ) then
                begin
                    S := Trim_Quotes( Parser.Token ) ;
                    S1 := Trim_Quotes( Parser.Token ) ;
                    C := Find_Component( S ) ;
                    C1 := Find_Component( S1 ) ;
                    if( C = nil ) then
                    begin
                        ShowMessage( Substitute1( Text_Error_Cannot_Find_Component, S ) ) ;
                        exit ;
                    end ;
                    if( C1 = nil ) then
                    begin
                        ShowMessage( Substitute1( Text_Error_Cannot_Find_Component, S1 ) ) ;
                        exit ;
                    end ;
                    C.Disconnect_Output( C1 ) ;
                    C1.Disconnect_Output( C ) ;
                    C.Disconnect_Input( C1 ) ;
                    C1.Disconnect_Input( C ) ;
                end else
                if( S = 'DOUBLE' ) then
                begin
                    Set_Number_Size( 8 ) ;
                end else
                if( S = 'EBCDIC' ) then
                begin
                    EBCDIC1.Caption := '&EBCDIC' ;
                    Main_Memory_Tab.AD.Ascii_Ebcdic := 1 ;
                    Port_Tab.AD.Ascii_Ebcdic := 1 ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'HIDE' ) then
                begin
                    _Hide := True ;
                end else
                if( S = 'HIGH' ) then
                begin
                    S := Parser.Token ;
                    if( not Valid_Base( S, Main_Memory_Tab.AD.Base ) ) then
                    begin
                        ShowMessage( Substitute2( Text_Error_Is_Not_A_Valid_Number_In_Radix, S, inttostr( Main_Memory_Tab.AD.Base ) ) ) ;
                        exit ;
                    end ;
                    High_Physical_Address := strtoint64( CvtB( Main_Memory_Tab.AD.Base, 10, S ) ) ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'HIGH_PORT' ) then
                begin
                    S := Parser.Token ;
                    if( not Valid_Base( S, Main_Memory_Tab.AD.Base ) ) then
                    begin
                        ShowMessage( Substitute2( Text_Error_Is_Not_A_Valid_Number_In_Radix, S, inttostr( Main_Memory_Tab.AD.Base ) ) ) ;
                        exit ;
                    end ;
                    High_Port_Address := strtoint64( CvtB( Main_Memory_Tab.AD.Base, 10, S ) ) ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'IMMEDIATE' ) then
                begin
                    Saved_B := Options_Form.Immediate_Mode_Unblock.Checked ;
                    Options_Form.Immediate_Mode_Unblock.Checked := False ;
                    try
                        S := Parser.Token ;
                        if( ( copy( S, 1, 1 ) = '"' ) or ( copy( S, 1, 1 ) = #39 ) ) then
                        begin
                            if( copy( S, length( S ), 1 ) = S[ 1 ] ) then
                            begin
                                setlength( S, length( S ) - 1 ) ;
                            end ;
                            S := copy( S, 2, length( S ) ) ;
                        end ;
                        Immediate_Mode_Edit.Text := S ;
                        try
                            Runimmediate1Click( nil ) ;
                        except
                        end ;
                        Immediate_Mode_Edit.Text := '' ;
                    finally
                        Options_Form.Immediate_Mode_Unblock.Checked := Saved_B ;
                    end ;
                end else
                if( S = 'LOAD' ) then
                begin
                    Name := '' ;
                    Domain := '' ;
                    S := Parser.Token ; // Component
                    S1 := '' ;
                    if( not Parser.Token_EOL ) then
                    begin
                        S1 := Parser.Grab_Line ;
                    end ;
                    Parse_Switch( 'NAME', '', S1, Name ) ;
                    Parse_Switch( 'DOMAIN', '', S1, Domain ) ;
                    if( not Load_Component( S, S1, Name, Domain ) ) then
                    begin
                        exit ;
                    end ;
                end else
                if( S = 'LONG' ) then
                begin
                    Set_Number_Size( 4 ) ;
                end else
                if( S = 'LOW' ) then
                begin
                    S := Parser.Token ;
                    if( not Valid_Base( S, Main_Memory_Tab.AD.Base ) ) then
                    begin
                        ShowMessage( Substitute2( Text_Error_Is_Not_A_Valid_Number_In_Radix, S, inttostr( Main_Memory_Tab.AD.Base ) ) ) ;
                        exit ;
                    end ;
                    Low_Physical_Address := strtoint64( CvtB( Main_Memory_Tab.AD.Base, 10, S ) ) ;
                    Low_Address := Low_Physical_Address ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'LOW_PORT' ) then
                begin
                    S := Parser.Token ;
                    if( not Valid_Base( S, Main_Memory_Tab.AD.Base ) ) then
                    begin
                        ShowMessage( Substitute2( Text_Error_Is_Not_A_Valid_Number_In_Radix, S, inttostr( Main_Memory_Tab.AD.Base ) ) ) ;
                        exit ;
                    end ;
                    Low_Port_Address := strtoint64( CvtB( Main_Memory_Tab.AD.Base, 10, S ) ) ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'RADIX' ) then
                begin
                    S := Parser.Token ;
                    try
                        Dummy := strtoint( S ) ;
                    except
                        Dummy := -1 ;
                    end ;
                    if( ( Dummy < 2 ) or ( Dummy > 50 ) ) then
                    begin
                        ShowMessage( Substitute1( Text_Error_Invalid_Radix, S ) ) ;
                        exit ;
                    end ;
                    Main_Memory_Tab.AD.Base := Dummy ;
                    Port_Tab.AD.Base := Dummy ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'RUN' ) then
                begin
                    Start_Execution := True ;
                end else
                if( S = 'SIGNED' ) then
                begin
                    Signed1.Checked := True ;
                    Main_Memory_Tab.AD.Unsigned := not Signed1.Checked ;
                    Port_Tab.AD.Unsigned := not Signed1.Checked ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'UNLOAD_ALL' ) then
                begin
                    UnloadAll1Click( nil ) ;
                end else
                if( S = 'UNLOAD' ) then
                begin
                    S := Parser.Token ;
                    Dummy := Component_List.IndexOf( S ) ;
                    if( Dummy = -1 ) then
                    begin
                        ShowMessage( Substitute1( Text_Error_Unknown_Component_On_Unload, S ) ) ;
                        exit ;
                    end else
                    begin
                        Remove_Component( Dummy, True ) ;
                    end ;
                end else
                if( S = 'UNSIGNED' ) then
                begin
                    Signed1.Checked := False ;
                    Main_Memory_Tab.AD.Unsigned := not Signed1.Checked ;
                    Port_Tab.AD.Unsigned := not Signed1.Checked ;
                    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
                end else
                if( S = 'WORD' ) then
                begin
                    Set_Number_Size( 2 ) ;
                end else
                begin
                    ShowMessage( Substitute1( Text_Error_Unknown_Emulator_Command, S ) ) ;
                    exit ;
                end ;
                S := uppercase( Parser.Token ) ;
            end ;
        finally
            Parser.Free ;

            for Loop := 1 to 9 do
            begin
                if( EReopen_Menus[ Loop ].Visible ) then
                begin
                    if( uppercase( copy( EReopen_Menus[ Loop ].Caption, 4, 255 ) ) = uppercase( FileName ) ) then // Already in list
                    begin
                        Filename := '' ;
                        break ;
                    end ;
                end ;
            end ;
            if( length( Filename ) > 0 ) then
            begin
                for Loop := 1 to 9 do
                begin
                    if( not EReopen_Menus[ Loop ].Visible ) then
                    begin
                        EReopen_Menus[ Loop ].Visible := True ;
                        EReopen_Menus[ Loop ].Caption := '&' + inttostr( Loop ) + ' ' +
                            Filename ;
                        break ;
                    end ;
                end ;
            end ;
            Update_Disassembly ;
        end ; // try
    finally
        SL.Free ;
    end ;
end ; // TMain_Form.Open_Emulator


procedure TMain_Form.Open_File( Name : string ) ;

var Editor : TEditor_Window ;
    Dummy, Loop : integer ;

begin
    // Open the file...
    New1Click( self ) ; // Create a new edit window
    Editor := TEditor_Window( PageControl1.ActivePage.Controls[ 0 ] ) ;
    Editor.Open_File( Name ) ;
    PageControl1.ActivePage.Caption := Editor.File_Name ;
    Editor.FormResize( nil ) ;

    // Remove from reopen menus...
    for Loop := 1 to 9 do
    begin
        if( not Reopen_Menus[ Loop ].Visible ) then
        begin
            break ; // Past current files
        end ;
        if( copy( Reopen_Menus[ Loop ].Caption, 4, 255 ) = Name ) then
        begin
            Reopen_Menus[ Loop ].Visible := False ;
            for Dummy := Loop to 8 do
            begin
                Reopen_Menus[ Dummy ].Caption := Reopen_Menus[ Dummy + 1 ].Caption ;
                Reopen_Menus[ Dummy ].Visible := Reopen_Menus[ Dummy + 1 ].Visible ;
            end ;
            break ;
        end ;
    end ;
end ; // TMain_Form.Open_File


procedure TMain_Form.ConnectionsClick( Sender : TObject ) ;

var Component : TComponent ;

begin
    Component := TComponent( TMenuItem( Sender ).Tag ) ;
    Component_Connection_Manager.Components := Component_List ;
    Component_Connection_Manager.Set_Component( string( Component.Name ) ) ;
    Component_Connection_Manager.ShowModal ;
end ;


procedure TMain_Form.Copy1Click(Sender: TObject) ;

var DT : _CEF.TData_Type ;
    Index : integer ;
    Size : integer ;
    P : pointer ;
    S : string ;
    Value : int64 ;

begin
    Index := CPU_Status.ItemIndex ;
    if( Index <> -1 ) then
    begin
        DT := Register_Information( Master_CPU.CPU, Index ) ;
        if( DT.Data_Type = DataType_String ) then
        begin
            setlength( S, ( DT.Size + 7 ) div 8 ) ;
            Size := DT.Size ;
            Master_CPU.Examine( Index, Size, Pchar( S ), False ) ;
        end else
        begin
            Size := Master_CPU.CPU.Register_Size( Index ) ;
            Value := 0 ;
            P := @Value ;
            Master_CPU.Examine( Index, Size, P, False ) ;
            S := inttostr( Value ) ;
        end ;
        Clipboard.AsText := S ;
    end ;
end ;


procedure TMain_Form.ConditionsClick( Sender : TObject ) ;

begin
    Conditions_Form.Component := TComponent( TMenuItem( Sender ).Tag ) ;
    Conditions_Form.ShowModal ;
end ;


procedure TMain_Form.ConfigureClick( Sender : TObject ) ;

var Active_Value : boolean ;
    B : boolean ;
    C : TComponent ;
    CB : TCheckBox ;
    Info : TComponent_Info ;
    Loop, Dummy : integer ;
    M : TMenuItem ;
    Y : integer ;
    Low, High : int64 ;

begin
    // Setup...
    M := TMenuItem( Sender ) ;
    C := TComponent( M.Tag ) ;

    // Intialize configuration dialog with component's attributes...
    Component_Configuration_Dialog.CPU.Visible :=
        ( C.Component_Type = Component_Type_CPU ) ;
    Component_Configuration_Dialog.Read_Latency.Value := C.Get_Read_Latency ;
    Component_Configuration_Dialog.Write_Latency.Value := C.Get_Write_Latency ;
    if( C.Component_Type = Component_Type_CPU ) then
    begin
        Component_Configuration_Dialog.Clock.Value := C.CPU.Get_Clock_Speed ;
    end ;
    Component_Configuration_Dialog.Memory.Visible :=
        ( C.Component_Type = Component_Type_Memory ) ;
    if( C.Component_Type = Component_Type_Memory ) then
    begin
        Component_Configuration_Dialog.Memory.Left := 8 ;
        C.Memory.Get_Address_Range( Low, High ) ;
        Component_Configuration_Dialog.Low_Address.Value := Low ;
        Component_Configuration_Dialog.High_Address.Value := High ;
    end else
    begin
        // Do this so the OK button will be enabled
        Component_Configuration_Dialog.Low_Address.Value := 0 ;
        Component_Configuration_Dialog.High_Address.Value := 0 ;
    end ;

    // Do signal processing...
    Delete_All_Children( Component_Configuration_Dialog.Signals_Tab_Sheet ) ;
    Dummy := C.Signal_Count ;
    Component_Configuration_Dialog.Signals_Tab_Sheet.Visible := ( Dummy > 0 ) ;
    Y := 8 ;
    for Loop := 0 to Dummy - 1 do
    begin
        CB := TCheckBox.Create( Component_Configuration_Dialog ) ;
        CB.Parent := Component_Configuration_Dialog.Signals_Tab_Sheet ;
        CB.Left := 8 ;
        CB.Top := Y ;
        Y := Y + CB.Height + 2 ;
        CB.Caption := C.Signal_Name( Loop ) ;
        Active_Value := not C.Signal_Active_Low( Loop ) ;
        if( C.Signal_Out( Loop ) ) then
        begin
            C.Get_Signal( PChar( CB.Caption ), B ) ;
        end else
        begin
            Info := TComponent_Info( C.Tag ) ;
            B := Info.Signal_Set( Dummy ) ;
        end ;
        CB.Checked := ( B = Active_Value ) ;
        CB.Enabled := not C.Signal_Out( Loop ) ;
    end ;
    Component_Configuration_Dialog.TabSheet1.BringToFront ;

    // Now show user...
    if( Component_Configuration_Dialog.ShowModal = mrOK ) then
    begin
        // Latency...
        C.Set_Read_Latency( Component_Configuration_Dialog.Read_Latency.Value ) ;
        C.Set_Write_Latency( Component_Configuration_Dialog.Write_Latency.Value ) ;

        // Component-specific settings...
        if( C.Component_Type = Component_Type_CPU ) then
        begin
            C.CPU.Set_Clock_Speed( Component_Configuration_Dialog.Clock.Value ) ;
        end ;
        if( C.Component_Type = Component_Type_Memory ) then
        begin
            Low := Component_Configuration_Dialog.Low_Address.Value ;
            High := Component_Configuration_Dialog.High_Address.Value ;
            C.Memory.Set_Address_Range( Low, High ) ;
        end ;

        // Signals...
        for Loop := 0 to Component_Configuration_Dialog.Signals_Tab_Sheet.ControlCount - 1 do
        begin
            if( not C.Signal_Out( Loop ) ) then
            begin
                CB := TCheckBox( Component_Configuration_Dialog.Signals_Tab_Sheet.Controls[ Loop ] ) ;
                if( CB.Enabled ) then
                begin
                    B := CB.Checked ;
                    if( C.Signal_Active_Low( Loop ) ) then
                    begin
                        B := not B ;
                    end ;
                    C.Set_Signal( PChar( CB.Caption ), B )
                end ; // if( CB.Enabled )
            end ;
        end ; // for Loop
    end ; // if( Component_Configuration_Dialog.ShowModal = mrOK )
end ; // TMain_Form.ConfigureClick


function TMain_Form.Get_File_Stream( Name : string ) : TCOM_Stream ;

var _Assembler : TAssembler ;
    Editor : TEditor_Window ;
    Loop : integer ;
    S : string ;
    _S, _T : integer ;
    UEC : TUnified_Exception ;

begin
    // Setup...
    Name := uppercase( Name ) ;
    if( Extension_Pos( Name ) = 0 ) then
    begin
        if( Master_CPU = nil ) then
        begin
            Name := Name + '.asm' ;
        end else
        begin
            _Assembler := Master_CPU.CPU.Get_Assembler( nil ) ;
            S := string( _Assembler.Source_Extensions ) + '|' ;
            Loop := pos( '|', S ) ;
            S := copy( S, 1, Loop - 1 ) ;
            if( copy( S, 1, 1 ) <> '.' ) then
            begin
                S := '.' + S ;
            end ;
            Name := Name + S ;
            _Assembler.Terminate ;
        end ;
    end ; // if( Extension_Pos( Name ) = 0 )

    // See if file is opened
    for Loop := 0 to PageControl1.PageCount - 1 do
    begin
        if( TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor <> nil ) then
        begin
            Editor := TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor ;
            if( uppercase( Editor.File_Name ) = Name ) then
            begin
                Result := TEditor_Streamer.Create( Editor ) ;
                exit ;
            end ;
        end ;
    end ;

    // Try to open the file...
    Result := Create_File_Stream( Name, UEC ) ;
    if( Result = nil ) then
    begin
        ShowMessage( Substitute2( Text_Error_While_Opening_File, UEC.Error_Text( _S, _T ), Name ) ) ;
    end ;
end ; // TMain_Form.Get_File_Stream


procedure TMain_Form.ProfileClick( Sender : TObject ) ;

var B : boolean ;
    C : TComponent ;
    M : TMenuItem ;

begin
    case Application.MessageBox( PChar( Text_Prompt_Include_Children_In_Change ),
      PChar( Text_Caption_Profile ), MB_YESNOCANCEL ) of
        ID_YES : B := True ;
        ID_NO : B := False ;
        else exit ;
    end ;
    M := TMenuItem( Sender ) ;
    M.Checked := not M.Checked ;
    C := TComponent( M.Tag ) ;
    C.Set_Profiling( M.Checked, B ) ;
end ;


procedure TMain_Form.ProfileReportClick( Sender : TObject ) ;

var C : TComponent ;
    M : TMenuItem ;

begin
    M := TMenuItem( Sender ) ;
    C := TComponent( M.Tag ) ;
    Report_Profile( C ) ;
end ;


procedure TMain_Form.UnloadClick( Sender : TObject ) ;

var C : TComponent ;
    Index : integer ;
    Loop : integer ;

begin
    // Setup...
    if( In_Assembly or In_Execution ) then
    begin
        exit ;
    end ;
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    Index := -1 ;
    for Loop := 0 to Component_List.Count - 2 do
    begin
        if( Component_List.Objects[ Loop ] = C ) then
        begin
            Index := Loop ;
            break ;
        end ;
    end ;
    Remove_Component( Index, True ) ;
end ;


var Get_Serial_Number : integer = 0 ;

function TMain_Form._Load_Component( Filename : string ; UI : TUI_Interface ) : TComponent ;

var F : function( Serial_Number : integer ; UI : TUI_Interface ) : TComponent ; stdcall ;
    Module : integer ;

begin
    // Load the DLL...
    Result := nil ;
    Module := LoadLibrary( PChar( FileName ) ) ;
    if( Module = 0 ) then
    begin
        ShowMessage( Substitute1( Text_Error_Load_Failure_For, FileName ) ) ;
        exit ;
    end ;
    F := GetProcAddress( Module, 'Make_Instance' ) ;
    if( @F = nil ) then
    begin
        FreeLibrary( Module ) ;
        ShowMessage( Substitute1( Text_Error_Not_A_Valid_CEF_Component, Filename ) ) ;
        exit ;
    end ;
    Result := F( Get_Serial_Number, UI ) ;
    inc( Get_Serial_Number ) ;
    if( Result = nil ) then
    begin
        FreeLibrary( Module ) ;
        ShowMessage( Substitute1( Text_Error_Component_Creation_Failure, Filename ) ) ;
        exit ;
    end ;
    if( Result.Version < Base_Interface_Version ) then // Obsolete component version
    begin
        FreeLibrary( Module ) ;
        ShowMessage( Substitute1( Text_Error_Component_Is_An_Obsolete_Version, Filename ) ) ;
        Result.Terminate ;
        Result := nil ;
        exit ;
    end ;
    if( Result.Version >= Base_Interface_Version + 10 ) then // Future uncompatible component version
    begin
        FreeLibrary( Module ) ;
        ShowMessage( Substitute1( Text_Error_Component_Is_An_Unsupported_Version, Filename ) ) ;
        Result.Terminate ;
        Result := nil ;
        exit ;
    end ;
end ; // TMain_Form._Load_Component


procedure TMain_Form.Create_Memory_Tab( C : TComponent ; Domain : string ) ;

var Dummy : integer ;
    Panel : TPaint_Panel ;
    Tab : TMemory_Tab ;
    TS : TTabSheet ;

begin
    TS := TTabSheet.Create( PageControl2 ) ;
    TS.Parent := PageControl2 ;
    TS.PageControl := PageControl2 ;
    TS.Caption := Domain ;
    Panel := TPaint_Panel.Create( Owner ) ;
    Panel.Parent := TS ;
    Panel.Align := alClient ;
    Panel.BevelOuter := bvNone ;
    Panel.PopupMenu := Popup_Menu ;
    Tab := TMemory_Tab.Create ;
    Tab.AI := TMemory_Tab_Array_Interface.Create ;
    TMemory_Tab_Array_Interface( Tab.AI ).Tab := Tab ;
    Tab.AD := TArray_Display.Create( Panel, Tab.AI,
        Font.Handle, clWindow, clWindowText ) ;
    Tab.Name := lowercase( Domain ) ;
    Memory_Tabs.Add( Tab ) ;
    Dummy := Memory_Tabs.Count - 1 ;
    Panel.Tag := Dummy ;
    TS.Tag := Dummy ;
    TS.Visible := True ;
    TMemory_Tab( Memory_Tabs[ Dummy ] ).Add( C ) ;
end ;


function TMain_Form.Load_Component( Filename, Setup, Name, Domain : string ) : boolean ;

var _Assembler : TAssembler ;
    C, SC : TComponent ;
    Dummy : integer ;
    M : TMenuItem ;
    Module : integer ;
    E, S, Temp : string ;

begin
    Result := False ; // Assume failure
    // Strip quotes...
    Filename := trim( Filename ) ;
    if( copy( Filename, 1, 1 ) = '"' ) then
    begin
        Filename := copy( Filename, 2, length( Filename ) ) ;
        if( copy( Filename, length( Filename ), 1 ) = '"' ) then
        begin
            setlength( Filename, length( Filename ) - 1 ) ;
        end ;
    end ;
    Name := Trim( Name ) ;
    if( copy( Name, 1, 1 ) = '"' ) then
    begin
        Name := copy( Name, 2, length( Name ) ) ;
        if( copy( Name, length( Name ), 1 ) = '"' ) then
        begin
            setlength( Name, length( Name ) - 1 ) ;
        end ;
    end ;
    Setup := trim( Setup ) ;
    if( copy( Setup, 1, 1 ) = '"' ) then
    begin
        Setup := copy( Setup, 2, length( Setup ) ) ;
        if( copy( Setup, length( Setup ), 1 ) = '"' ) then
        begin
            setlength( Setup, length( Setup ) - 1 ) ;
        end ;
    end ;

    // Default the extension and path...
    Filename := Qualify_Filename( Filename, Program_Path + 'Components\', OS^.Shared_Library_Extension ) ;
    C := _Load_Component( Filename, UI ) ;

    // Set up component...
    if( Name = '' ) then
    begin
        Name := C.Name ;
    end ;
    C.Tag := integer( TComponent_Info.Create ) ;
    TComponent_Info( C.Tag ).DLL_Handle := Module ;
    if( Component_List.Indexof( Name ) <> -1 ) then // Duplicate name
    begin
        Dummy := 2 ;
        while( Component_List.Indexof( Name + inttostr( Dummy ) ) <> -1 ) do
        begin
            inc( Dummy ) ;
        end ;
        Name := Name + inttostr( Dummy ) ;
    end ;
    Component_List.AddObject( Name, C ) ;
    C.Parent := Parent_Component ;
    if( C.CPU <> nil ) then // CPU
    begin
        SelectCPU.Enabled := True ;
        Set_Master_CPU( C ) ;
        Master_Assembler := nil ;
        Update_CPU_State( False ) ;
        Update_Watches ;
        _Assembler := Master_CPU.CPU.Get_Assembler( Master_Assembler ) ;
        try
            if( _Assembler.Source_Extensions <> nil ) then
            begin
                E := string( _Assembler.Source_Extensions ) ;
                Temp := '' ;
                while( length( E ) > 0 ) do
                begin
                    Dummy := pos( '|', E + '|' ) ;
                    if( length( Temp ) > 0 ) then
                    begin
                        Temp := Temp + '|' ;
                    end ;
                    Temp := Temp + Substitute1( Text_Source_Dialog_Filter, copy( E, 1, Dummy - 1 ) ) ;
                    E := copy( E, Dummy + 1, length( E ) ) ;
                end ;
                if( length( Temp ) > 0 ) then
                begin
                    Open_Source_Dialog.Filter := Temp ;
                end ;
            end ;
        finally
            _Assembler.Terminate ;
        end ;
        if( C.CPU.Version >= 26 ) then
        begin
            Dummy := 0 ;
            while( True ) do
            begin
                S := C.CPU.Memory_Space_Description( Dummy, True ) ;
                if( S = '' ) then
                begin
                    break ;
                end ;
                SC := C.CPU.Get_Store( Dummy ) ;
                inc( Dummy ) ;
                if( SC <> nil ) then
                begin
                    Create_Memory_Tab( SC, S ) ;
                end ;
            end ;
            SC := C.CPU.Get_Target_Memory ;
            if( ( SC <> nil ) and ( Component_List.IndexOfObject( SC ) = -1 ) ) then
            begin
                Create_Memory_Tab( SC, Name ) ;
            end ;
        end ;
        Update_Disassembly ;
    end ; // if( C.CPU <> nil )
    if( C.Component_Type <> Component_Type_Memory ) then
    begin
        C.Connect_Output( Main_Memory ) ;
        C.Connect_Input( Main_Memory ) ;
        Main_Memory.Connect_Input( C ) ;
        Main_Memory.Connect_Output( C ) ;
    end ;

    Dummy := 3 ;
    while( Components1.Items[ Dummy ].Caption <> '-' ) do
    begin
        inc( Dummy ) ;
    end ;
    M := TMenuItem.Create( Components1 ) ;
    M.Caption := Name ;
    M.Enabled := True ;
    try
        Components1.Insert( Dummy, M ) ;
    except
    end ;
    TComponent_Info( C.Tag ).Menu := Configure_Component_Menus( C, M ) ;

    if( length( Setup ) <> 0 ) then
    begin
        C.Set_up( PChar( Setup ) ) ;
    end ;

    if( C.Component_Type = Component_Type_Memory ) then
    begin
        if( Domain = '' ) then
        begin
            Main_Memory_Tab.Add( C ) ;
        end else
        begin
            Dummy := Domain_Index( lowercase( Domain ) ) ;
            if( Dummy = -1 ) then
            begin
                Create_Memory_Tab( C, Domain ) ;
            end else
            begin
                TMemory_Tab( Memory_Tabs[ Dummy ] ).Add( C ) ;
            end ;
        end ;
    end ;
    if( C.User_Interface <> nil ) then
    begin
        C.User_Interface.Set_Hidden( False ) ; // Make sure it is visible
    end ;
    Result := True ;
end ; // TMain_Form.Load_Component


procedure TMain_Form.Log_Trace( Component : TComponent ; Description : string ) ;

begin
    Description := Component_Name( Component ) + ': ' + Description ;
    if( Traces.Count = 0 ) then // Tracing is disabled
    begin
        exit ;
    end ;
    if( Trace_Position = Traces.Count ) then // Need to wrap position
    begin
        Trace_Position := 0 ;
    end ;
    Traces[ Trace_Position ] := Description ;
    inc( Trace_Position ) ;
end ;


procedure TMain_Form.Set_Number_Size( Size : integer ) ;

begin
    Current_Tab.AD.Number_Size := Size ;
    Port_Tab.AD.Number_Size := Size ;
    case Size of
        2 : Word1.Checked := True ;
        4 : Long1.Checked := True ;
        8 : Double1.Checked := True ;
        else Byte1.Checked := True ;
    end ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
end ;


function TMain_Form.Address_Size : integer ;

var B : int64 ;
    S, S1 : string ;

begin
    // Determine maximum address width...
    if( Physical_Address ) then
    begin
        B := High_Physical_Address ;
    end else
    begin
        B := High_Address  ;
    end ;
    S := CVTB( 10, Current_Tab.AD.Base, inttostr( B ) ) ;
    S1 := S ;
    if( ( Master_CPU <> nil ) and ( Master_CPU.Version >= 22 ) ) then
    begin
        S := string( Master_CPU.CPU.Address_Representation( Main_Memory_Tab.AD.Base, B ) ) ;
        if( S <> '' ) then
        begin
            S := S1 ;
        end ;
    end ;
    Address_Size := length( S ) ;
end ;


const Bit_Masks : array[ 1..8 ] of byte = ( 1, 3, 7, $F, $1F, $3F, $7F, $FF ) ;


function TMain_Form.Domain_Index( const Domain : string ) : integer ;

var I : integer ;

begin
    Result := -1 ;
    for I := 0 to Memory_Tabs.Count - 1 do
    begin
        if( TMemory_Tab( Memory_Tabs[ I ] ).Name = Domain ) then
        begin
            Result := I ;
            exit ;
        end ;
    end ;
end ;


function TMain_Form.Get_Disassembly( var Address : int64 ; Physical : int64 ;
    Show_Address, Show_Value : boolean ; _Assembler : TAssembler ) : string ;

var Len : integer ;
    Bit_Offset : integer ; // Bit offset within (byte) Offset
    Offset : integer ; // Byte offset from starting address

    function EOB : boolean ;

    begin
        Result := Offset >= Len ;
    end ;

var Byte_Size : integer ;

    function Pad( const S : string ) : string ;

    begin
        Result := S ;
        while( length( Result ) < Byte_Size ) do
        begin
            Result := '0' + Result ;
        end ;
    end ;

var B : integer ;
    S, S1 : string ;
    Segment, Segment_Size : integer ;
    Stream : TCOM_String_Stream ;


    function Get_Bits( Bit_Count : integer ; S : string ) : int64 ;

        function Read_Bits( O : integer ) : byte ;

        begin
            if( ( O < 0 ) or ( O >= length( S ) ) ) then
            begin
                Result := 0 ;
            end else
            begin
                Result := ord( S[ O + 1 ] ) ;
            end ;
        end ;

    var Byte_Off, Bit_Off : integer ; // Our working offsets
        Bits : integer ; // How many bits to move to result
        Temp : int64 ;
        Only_Byte : boolean ;

    begin
        // Setup...
        if( Bit_Count > 64 ) then
        begin
            Bit_Count := 64 ; // Can only do 8 bytes at a time
        end ;

        // Find ending byte/bit of data...
        Bit_Off := Bit_Offset + Bit_Count - 1 ;
        Byte_Off := Offset ;
        while( Bit_Off > 7 ) do
        begin
            Bit_Off := Bit_Off - 8 ;
            inc( Byte_Off ) ; // Move to next byte
        end ;

        // Accumulate data...
        Only_Byte := True ;
        Result := 0 ;
        while( Byte_Off > Offset ) do
        begin
            Temp := Read_Bits( Byte_Off ) ; // Get byte

            // Determine number of bits to apply from this byte
            Bits := Bit_Off + 1 ;
            Bit_Off := 7 ;
            dec( Byte_Off ) ;

            Result := Result shl Bits ; // Move previous data to the left
            Result := Result or ( Temp and Bit_Masks[ Bits ] ) ; // Add in these bits
            Only_Byte := False ;
        end ;
        // At this point, Byte_Off = Offset (first byte)
        Temp := Read_Bits( Byte_Off ) ; // Get byte

        // Determine number of bits to apply from this byte
        if( Only_Byte ) then
        begin
            Bits := Bit_Count ;
        end else
        begin
            Bits := 8 - Bit_Offset ;
        end ;

        Result := Result shl Bits ; // Move previous data to the left
        Result := Result or ( ( Temp shr Bit_Offset ) and Bit_Masks[ Bits ] ) ; // Add in these bits

        // Move our position...
        Bit_Offset := Bit_Offset + Bit_Count ;
        while( Bit_Offset > 7 ) do
        begin
            Bit_Offset := Bit_Offset - 8 ;
            inc( Offset ) ; // Move to next byte
        end ;
    end ; // Get_Bits

var C : TComponent ;
    Ignore, Machine : PChar ;
    M : string ;
    Saved_Base, Size : integer ;
    Status : TCEF_Assembler_Status ;
    Temp : string ;
    Work : byte ;

begin // .Get_Disassembly
    // Setup...
    Byte_Size := length( CVTB( 10, Main_Memory_Tab.AD.Base, '255' ) ) ;
    S := '' ;

    // Build result string...
    C := nil ;
    if( Show_Address ) then
    begin
        S := CvtB( 10, Main_Memory_Tab.AD.Base, inttostr( Address ) ) ;
        while( length( S ) < Address_Size ) do
        begin
            S := '0' + S ;
        end ;
        if( ( Master_CPU <> nil ) and ( Master_CPU.Version >= 22 ) ) then
        begin
            if( Master_CPU.Version >= 26 ) then
            begin
                if( Master_CPU.CPU.Get_Target_Memory <> nil ) then
                begin
                    C := Master_CPU.CPU.Get_Target_Memory ;
                end ;
            end ;
            S1 := string( Master_CPU.CPU.Address_Representation_Ex( C, Main_Memory_Tab.AD.Base, Address ) ) ;
            if( S1 <> '' ) then
            begin
                S := S1 ;
                while( length( S ) < Address_Size ) do
                begin
                    S := ' ' + S ;
                end ;
            end ;
        end ;

        S := S + ': ' ;
    end ; // if( Show_Address )

    if( Master_CPU = nil ) then // No CPU
    begin
        B := _Examine_Memory( Physical, True, 0 ) ;
        if( Show_Value ) then
        begin
            S := S + Pad( CvtB( 10, Main_Memory_Tab.AD.Base, inttostr( B ) ) ) ;
        end ;
        S := S + '       .DB   ' + CVTB( 10, Main_Memory_Tab.AD.Base, inttostr( B ) ) ;
        inc( Address ) ;
        inc( Physical_Address ) ;
    end else
    begin
        Stream := TCOM_String_Stream.Create ;
        Status := TCEF_Assembler_Status.Create ;
        try
            Master_CPU.CPU.Disassemble( Physical, Main_Memory_Tab.AD.Base, 1, Stream ) ;
            Saved_Base := _Master_Assembler.Base ;
            _Master_Assembler.Base := Main_Memory_Tab.AD.Base ;
            try
                if( _Assembler.Version < 22 ) then
                begin
                    _Assembler.Assemble( Stream.As_String, Ignore, Machine, Len, Address, Segment, Status ) ;
                end else
                begin
                    _Assembler.Assemble_Ex( Stream.As_String, Ignore, Machine, Len, Address, Segment, Status, ASF_Disassemble ) ;
                end ;
            finally
                _Master_Assembler.Base := Saved_Base ;
            end ;
            if( Len = 0 ) then // This indicates an invalid op-code (or a CPU component bug)
            begin
                Len := 1 ; // Always grab at least one byte
                if( C <> nil ) then
                begin
                    Size := 8 ;
                    C.Examine( Physical, Size, @Work, True ) ;
                    Temp := chr( B ) ;
                end else
                begin
                    Temp := chr( _Examine_Memory( Physical, True, 0 ) ) ;
                end ;
                Machine := PChar( Temp ) ;
            end ;

            if( Show_Value ) then
            begin
                setlength( M, Len ) ;
                move( Machine[ 0 ], PChar( M )[ 0 ], Len ) ;
                Segment := 0 ;
                Offset := 0 ;
                Bit_Offset := 0 ;
                while( not EOB ) do
                begin
                    if( Master_CPU.CPU.Version < 22 ) then
                    begin
                        Segment_Size := ( Len - Offset ) * 8 ; // Get all the bits
                    end else
                    begin
                        Segment_Size := Master_CPU.CPU.Segment_Size( Segment ) ;
                        if( Segment_Size = 0 ) then
                        begin
                            Segment_Size := ( Len - Offset ) * 8 ; // Get all the bits
                        end ;
                    end ;
                    inc( Segment ) ;
                    B := Get_Bits( Segment_Size, M ) ;
                    S := S + Pad( CvtB( 10, Main_Memory_Tab.AD.Base, inttostr( B ) ) ) + ' ' ;
                end ;
                while( length( S ) < 30 ) do
                begin
                    S := S + ' ' ;
                end ;
            end ; // if( Show_Value )

            S := S + ' ' + string( Stream.As_String ) ;
            Address := Address + Len ;
            //Physical := Physical + Len ;
        finally
            Stream.Free ;
            Status.Free ;
        end ;
    end ; // if( Master_CPU = nil )
    Result := S ;
end ; // TMain_Form.Get_Disassembly


procedure TMain_Form.Update_Disassembly ;

var Address, This_Address : int64 ;
    _Assembler : TAssembler ;
    Count, Loop : integer ;
    S : string ;
    Saved : integer ;

begin
    // Setup...
    Saved := Disassembly_List_Box.ItemIndex ;
    Disassembly_List_Box.Clear ;
    Count := Disassembly_List_Box.ClientHeight div Disassembly_List_Box.ItemHeight ;

    if( ( Disassembly_Top = -1 ) and ( Master_CPU <> nil ) ) then
    begin
        Address := Master_CPU.CPU.Get_Current_Address( 0, True ) ;
    end else
    begin
        Address := Disassembly_Top ;
    end ;
    _Assembler := nil ;
    if( Master_CPU <> nil ) then
    begin
        _Assembler := Master_CPU.CPU.Get_Assembler( Master_Assembler ) ;
    end ;

    // Show disassembly
    try
        for Loop := 0 to Count - 1 do
        begin
            This_Address := Address ;
            if( Physical_Address ) then
            begin
                if( Address > High_Physical_Address ) then
                begin
                    break ;
                end ;
            end else
            begin
                if( Address > High_Address ) then
                begin
                    break ;
                end ;
                if( Master_CPU <> nil ) then
                begin
                    This_Address := Master_CPU.CPU.Translate( -1, Address ) ;
                end ;
            end ;
            S := Get_Disassembly( Address, This_Address, True, True, _Assembler ) ;
            Disassembly_List_Box.Items.Add( S ) ;
            if( Loop = 0 ) then
            begin
                Disassembly_Second := Address ;
            end ;
        end ; // for Loop := 0 to Count - 1
        Disassembly_Next := Address ;
        Disassembly_List_Box.ItemIndex := Saved ;
    finally
        // Clean up
        if( Master_CPU <> nil ) then
        begin
            _Assembler.Terminate ;
        end ;
    end ;
end ; // TMain_Form.Update_Disassembly


procedure TMain_Form.Update_Stack ;

var Byte_Size : integer ;

    function Pad( S : string ) : string ;

    begin
        while( length( S ) < Byte_Size ) do
        begin
            S := '0' + S ;
        end ;
        Result := S ;
    end ;

var Address : int64 ;
    Address_Size : integer ;
    B : byte ;
    B1 : int64 ;
    Grow_Up : boolean ;
    S : string ;
    Saved : integer ;
    Stack_Interface : TCEF_Stack_Interface ;
    Target : int64 ;

begin // .Update_Stack
    // Setup...
    Saved := Stack_List_Box.ItemIndex ;
    Stack_List_Box.Clear ;
    Stack_Interface := nil ;
    if( Master_CPU <> nil ) then
    begin
        if( Master_CPU.Version > 21 ) then
        begin
            Stack_Interface := Master_CPU.CPU.Get_Stack_Interface( 0 ) ;
        end ;
    end ;

    if( Stack_Interface <> nil ) then
    begin
        try
            Byte_Size := Stack_Interface.Item_Size ;
            Grow_Up := False ;
            if( Master_CPU.Version > 25 ) then
            begin
                Grow_Up := Stack_Interface.Grow_Up ;
            end ;
            Byte_Size := length( CVTB( 10, Main_Memory_Tab.AD.Base, inttostr( Bit_Values[ Byte_Size ] - 1 ) ) ) ;
            if( Grow_Up ) then
            begin
                if( Stack_Top = -1 ) then
                begin
                    Address := Master_CPU.CPU.Top_Of_Stack( 0 ) ;
                end else
                begin
                    Address := Stack_Top ;
                end ;
                Address_Size := length( CVTB( 10, Main_Memory_Tab.AD.Base, inttostr( Address ) ) ) ;
                Target := 0 ;
            end else
            begin
                Address_Size := length( CVTB( 10, Main_Memory_Tab.AD.Base, inttostr( Stack_Interface.High_Bound ) ) ) ;
                Address := 0 ;
                if( Stack_Top = -1 ) then
                begin
                    Target := Master_CPU.CPU.Top_Of_Stack( 0 ) ;
                end else
                begin
                    Target := Stack_Top ;
                end ;
            end ;

            // Build stack...
            while( True ) do
            begin
                if( Stack_List_Box.Count > 1000 ) then
                begin
                    break ;
                end ;
                S := CvtB( 10, Main_Memory_Tab.AD.Base, inttostr( Address ) ) ;
                while( length( S ) < Address_Size ) do
                begin
                    S := '0' + S ;
                end ;
                S := S + ': ' ;
                B1 := Stack_Interface.Value( Address ) ;
                S := S + Pad( CvtB( 10, Main_Memory_Tab.AD.Base, inttostr( B1 ) ) ) + '  ' + chr( B1 ) ;
                Stack_List_Box.Items.Add( S ) ;
                if( Grow_Up ) then
                begin
                    Address := Address - 1 ;
                    if( Address < Target ) then
                    begin
                        break ;
                    end ;
                end else
                begin
                    Address := Address + 1 ;
                    if( Address > Target ) then
                    begin
                        break ;
                    end ;
                end ;
            end ; // while( True )
        finally
            Stack_Interface.Terminate ;
        end ;
    end else
    begin
        Byte_Size := length( CVTB( 10, Main_Memory_Tab.AD.Base, '255' ) ) ;
        if( Physical_Address ) then
        begin
            Address_Size := length( CVTB( 10, Main_Memory_Tab.AD.Base, inttostr( High_Physical_Address ) ) ) ;
        end else
        begin
            Address_Size := length( CVTB( 10, Main_Memory_Tab.AD.Base, inttostr( High_Address ) ) ) ;
        end ;

        if( ( Stack_Top = -1 ) and ( Master_CPU <> nil ) ) then
        begin
            Address := Master_CPU.CPU.Top_Of_Stack( 0 ) ;
        end else
        begin
            Address := Stack_Top ;
        end ;

        // Build stack...
        if( Master_CPU <> nil ) then
        begin
            while( Address >= 0 ) do
            begin
                if( Stack_List_Box.Count > 1000 ) then
                begin
                    break ;
                end ;
                if( Physical_Address ) then
                begin
                    if( Address > High_Physical_Address ) then
                    begin
                        break ;
                    end ;
                end else
                begin
                    if( Address > High_Address ) then
                    begin
                        break ;
                    end ;
                end ;
                S := CvtB( 10, Main_Memory_Tab.AD.Base, inttostr( Address ) ) ;
                while( length( S ) < Address_Size ) do
                begin
                    S := '0' + S ;
                end ;
                S := S + ': ' ;
                B := _Examine_Memory( Address, True, 0 ) ;
                S := S + Pad( CvtB( 10, Main_Memory_Tab.AD.Base, inttostr( B ) ) ) + '  ' + chr( B ) ;
                Stack_List_Box.Items.Add( S ) ;
                Address := Address + 1 ;
            end ; // while( Address >= 0 )
        end ; // if( Master_CPU <> nil )
    end ;
    Stack_List_Box.ItemIndex := Saved ;
end ; // TMain_Form.Update_Stack


procedure TMain_Form.Map_Source( Force : boolean ) ;

    function Open_Tab( S : string ) : boolean ;

    var Dummy : integer ;
        Editor : TEditor_Window ;
        Loop : integer ;

    begin
        Result := True ;
        S := uppercase( S ) ;
        for Loop := 0 to PageControl1.PageCount - 1 do
        begin
            if( TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor <> nil ) then
            begin
                Editor := TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor ;
                if( uppercase( Editor.File_Name ) = S ) then
                begin
                    PageControl1.ActivePage := PageControl1.Pages[ Loop ] ;
                    exit ;
                end ;
            end ;
        end ;

        // If we get here, the file is not currently open...
        Dummy := PageControl1.PageCount ;
        Open_File( S ) ;
        if( Dummy = PageControl1.PageCount ) then // Open failed
        begin
            Result := False ;
        end ;
    end ;


var Active_Page : TCEF_TabSheet ;
    Editor : TEditor_Window ;
    Info : TComponent_Info ;
    F : PChar ;
    Line : integer ;
    Was_CPU_View : boolean ;

begin
    Info := nil ;
    Line := -1 ;
    Active_Page := TCEF_TabSheet( PageControl1.ActivePage ) ;
    Was_CPU_View := ( not Force ) and ( Active_Page = Disassembly_Sheet ) ;
    if( Master_CPU <> nil ) then
    begin
        Info := TComponent_Info( Master_CPU.Tag ) ;
    end ;
    if( ( Info <> nil ) and ( Info.Assembler_Context <> nil ) ) then
    begin
        Line := Info.Assembler_Context.Mapping( Master_CPU.CPU.Get_Current_Address( 0, True ), F ) ;
        if( Line > 0 ) then // Found a mapping
        begin
            if( not Open_Tab( string( F ) ) ) then
            begin
                Line := -1 ; // Cannot find source file
            end else
            begin
                // Position editor...
                Editor := TEditor_Window( PageControl1.ActivePage.Controls[ 0 ] ) ;
                Editor.Select_Line( Line ) ;
                Editor.Bring_Into_View ;
            end ;
        end ;
    end ;
    if( ( Was_CPU_View ) or ( Line < 1 ) ) then // Source not found or else in disassembly
    begin
        PageControl1.ActivePage := Disassembly_Sheet ;
    end ;
end ; // TMain_Form.Map_Source


procedure TMain_Form.Report_Profile( Component : TComponent ) ;

begin
    Profile_Report_Form.Set_Component( Component ) ;
    Profile_Report_Form.ShowModal ;
end ;


procedure TMain_Form.Reset1Click( Sender : TObject ) ;

var C : TComponent ;

begin
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    C.Reset ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    Update_Disassembly ;
    Update_Stack ;
    Update_CPU_State( False ) ;
    Map_Source( False ) ;
end ;


procedure TMain_Form.Restart1Click( Sender : TObject ) ;

var C : TComponent ;

begin
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    C.CPU.Restart ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    Update_Disassembly ;
    Update_Stack ;
    Update_CPU_State( False ) ;
    Map_Source( False ) ;
end ;


procedure TMain_Form.ToggleEmbedClick( Sender : TObject ) ;

var C : TComponent ;
    M : TMenuItem ;
    T : TWinControl ;
    TS : TCEF_TabSheet ;

begin
    M := TMenuItem( Sender ) ;
    C := TComponent( M.Tag ) ;
    M.Checked := not M.Checked ; // Toggle checkmark
    if( M.Checked ) then // Change to embedded
    begin
        TS := TCEF_TabSheet.Create( self ) ;
        TS.PageControl := PageControl1 ;
        TS.UI := C ;
        C.User_Interface.Set_Parent_Window( TS.Handle ) ;
        TS.Align := alClient ;
        TS.Caption := C.User_Interface.Get_Caption ;
    end else
    begin
        T := FindControl( C.User_Interface.Get_Parent_Window ) ;
        C.User_Interface.Set_Parent_Window( 0 ) ;
        if( T <> nil ) then
        begin
            if( T is TCEF_TabSheet ) then
            begin
                TCEF_TabSheet( T ).UI := nil ;
            end ;
            T.Free ;
        end ;
    end ;
end ;


procedure TMain_Form.SelectCPUClick(Sender: TObject);

var C : TComponent ;
    Loop : integer ;

begin
    CPU_Selection_Form.ListBox1.Items.Clear ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        C := TComponent( Component_List.Objects[ Loop ] ) ;
        if( C.CPU <> nil ) then
        begin
            CPU_Selection_Form.ListBox1.Items.Add( Component_List[ Loop ] ) ;
            if( Master_CPU = C ) then
            begin
                CPU_Selection_Form.ListBox1.ItemIndex := Loop ;
            end ;
        end ;
    end ;
    if( CPU_Selection_Form.ShowModal = mrOK ) then
    begin
        C :=  TComponent( Component_List.Objects[ CPU_Selection_Form.ListBox1.ItemIndex ] ) ;
        if( C <> Master_CPU ) then
        begin
            Set_Master_CPU( C ) ;
        end ;
    end ;
end ;


procedure TMain_Form.Server1Click(Sender: TObject) ;

begin
    Server_Dialog.UI := UI ;
    Server_Dialog.ShowModal ;
end ;


procedure TMain_Form.Set_Master_CPU( CPU : TComponent ) ;

var Loop : integer ;
    M : TMenuItem ;
    P : PChar ;

begin
    // Setup...
    Master_CPU := CPU ;
    if( Master_CPU = nil ) then
    begin
        Physical_Address := True ;
    end else
    begin
        Low_Address := Master_CPU.CPU.Get_Low_Memory ;
        High_Address := Master_CPU.CPU.Get_High_Memory ;
        Low_Port_Address := Master_CPU.CPU.Get_Low_Port ;
        High_Port_Address := Master_CPU.CPU.Get_High_Port ;
    end ;
    Run1.Enabled := ( CPU <> nil ) ;
    Assemble1.Enabled := Run1.Enabled ;
    Immediate_Mode_Edit.Enabled := Run1.Enabled ;
    CPU_Status.Enabled := Run1.Enabled ;
    if( CPU <> nil ) then
    begin
        P := CPU.CPU.Memory_Space_Description( 1, True ) ;
        Physical1.Enabled := CPU.CPU.Support_Virtual_Address ;
    end else
    begin
        P := nil ;
        Physical1.Enabled := False ;
    end ;
    AddressSpace1.Visible := ( P <> nil ) ;
    Current_Address_Space := 0 ;

    for Loop := AddressSpace1.Count - 1 downto 0 do // Remove all address space subitems
    begin
        AddressSpace1.Items[ Loop ].Free ;
    end ;

    // Rebuild address space subitems...
    Loop := 0 ;
    while( P <> nil ) do
    begin
        P := CPU.CPU.Memory_Space_Description( Loop, True ) ;
        if( P = nil ) then
        begin
            break ;
        end ;
        M := TMenuItem.Create( AddressSpace1 ) ;
        AddressSpace1.Add( M ) ;
        M.Caption := string( P ) ;
        M.OnClick := CB_Address_Space_Click ;
        M.RadioItem := True ;
        M.GroupIndex := 1 ;
        M.Tag := Loop ;
        if( Loop = 0 ) then
        begin
            M.Checked := True ;
        end ;
        inc( Loop ) ;
    end ;

    // Have memory use same base as CPU
    if( CPU <> nil ) then
    begin
        Main_Memory_Tab.AD.Base := CPU.CPU.Default_Base ;
        Port_Tab.AD.Base := CPU.CPU.Default_Base ;
        Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    end ;

    Master_Assembler := nil ;
end ; // TMain_Form.Set_Master_CPU


function TMain_Form._Examine_Memory( Address : int64 ;
    Physical_Address : boolean ; Address_Space : integer ) : byte ;

var B : byte ;
    C : TComponent ;
    Loop : integer ;
    Low, High : int64 ;
    Size : integer ;
    UEC : TUnified_Exception ;

begin
    if( ( Master_CPU <> nil ) and not Physical_Address ) then
    begin
        Address := Master_CPU.CPU.Translate( Address_Space, Address ) ;
    end ;
    Size := 8 ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        try
            C := TComponent( Component_List.Objects[ Loop ] ) ;
            if( C.Memory <> nil ) then
            begin
                C.Memory.Get_Address_Range( Low, High ) ;
                if( ( Address >= Low ) and ( Address <= High ) ) then
                begin
                    UEC := C.Examine( Address, Size, @B, True ) ;
                    if( UEC = nil ) then // Successful examine
                    begin
                        Result := B ;
                        exit ;
                    end ;
                end ;
            end ;
        except
        end ;
    end ;
    if( Main_Memory = nil ) then
    begin
        B := 255 ;
    end else
    begin
        Main_Memory.Examine( Address, Size, @B, True ) ;
    end ;
    Result := B ;
end ;


procedure TMain_Form.Dump1Click( Sender : TObject ) ;

var Address : int64 ;
    C : TComponent ;
    File_Name : string ;
    Size : int64 ;

    procedure _Dump_Image ;

    var Buffer : pointer ;
        Dummy : integer ;
        F : file ;

    begin
        Buffer := allocmem( Size ) ;
        try
            C.Memory.Dump( Address, Size, Buffer ) ;
            assignfile( F, File_Name ) ;
            {$I-}
            rewrite( F, 1 ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                exit ;
            end ;
            try
                {$I-}
                blockwrite( F, Buffer^, Size ) ;
                {$I+}
                Dummy := IOResult ;
                if( Dummy <> 0 ) then
                begin
                    ShowMessage( ERT( Dummy ) ) ;
                end ;
            finally
                {$I-}
                closefile( F ) ;
                {$I+}
                IOResult ; // Ignore close errors
            end ;
        finally
            freemem( Buffer ) ;
        end ;
    end ;


    procedure _Dump_Ent ;

    var Dummy : integer ;
        F : textfile ;
        Item : integer ;
        S : string ;

    begin
        assignfile( F, File_Name ) ;
        {$I-}
        rewrite( F ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) ) ;
            exit ;
        end ;
        try
            // Write the entry point...
            {$I-}
            writeln( F, 'EN ', CVTB( Dump_Memory_Form.Base, 16, Dump_Memory_Form.Starting_Address.Text ) ) ;
            {$I+}
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                exit ;
            end ;

            // Start first line...
            S := CVTB( 10, 16, inttostr( Address ) ) ;
            while( length( S ) < 4 ) do
            begin
               S := '0' + S ;
            end ;
            {$I-}
            write( F, S, ':' ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                exit ;
            end ;

            // Write the data...
            Item := 1 ;
            while( Size > 0 ) do
            begin
                S := CVTB( 10, 16, inttostr( Examine_Memory( Address ) ) ) ;
                if( length( S ) = 1 ) then
                begin
                    S := '0' + S ;
                end ;
                {$I-}
                write( F, ' ', S ) ;
                {$I+}
                Dummy := IOResult ;
                if( Dummy <> 0 ) then
                begin
                    ShowMessage( ERT( Dummy ) ) ;
                    exit ;
                end ;
                Address := Address + 1 ;
                Size := Size - 1 ;
                inc( Item ) ;
                if( ( Item > 16 ) and ( Size > 0 ) ) then
                begin
                    Item := 1 ;
                    {$I-}
                    writeln( F ) ;
                    {$I+}
                    Dummy := IOResult ;
                    if( Dummy <> 0 ) then
                    begin
                        ShowMessage( ERT( Dummy ) ) ;
                        exit ;
                    end ;
                    S := CVTB( 10, 16, inttostr( Address ) ) ;
                    while( length( S ) < 4 ) do
                    begin
                       S := '0' + S ;
                    end ;
                    {$I-}
                    write( F, S, ':' ) ;
                    {$I+}
                    Dummy := IOResult ;
                    if( Dummy <> 0 ) then
                    begin
                        ShowMessage( ERT( Dummy ) ) ;
                        exit ;
                    end ;
                end ;
            end ;

            // End the file...
            {$I-}
            writeln( F, '/' ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                exit ;
            end ;
        finally
            {$I-}
            closefile( F ) ;
            {$I+}
            IOResult ; // Ignore close errors
        end ;
    end ;


var Dummy : integer ;

begin
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    if( ( Current_Tab.AD.Base > 1 ) and ( Current_Tab.AD.Base < 50 ) ) then
    begin
        Dump_Memory_Form.Base := Current_Tab.AD.Base ;
    end else
    begin
        Dump_Memory_Form.Base := 10 ;
    end ;
    if( Dump_Memory_Form.ShowModal = mrOK ) then
    begin
        File_Name := Dump_Memory_Form.File_Name.Text ;
        Dummy := Extension_Pos( File_Name ) ;
        if( Dummy = 0 ) then
        begin
            Dummy := length( File_Name ) + 1 ;
        end ;
        Address := strtoint( CVTB( Dump_Memory_Form.Base, 10, uppercase( Dump_Memory_Form.Starting_Address.Text ) ) ) ;
        Size := strtoint( Dump_Memory_Form.Size.Text ) ;
        if( uppercase( copy( File_Name, Dummy + 1, 4 ) ) = 'ENT' ) then
        begin
            _Dump_Ent ;
        end else
        begin
            _Dump_Image ;
        end ;
    end ;
end ; // TMain_Form.Dump1Click


procedure TMain_Form.LoadMem1Click( Sender : TObject ) ;

var Buffer : PChar ;
    C : TComponent ;
    Dummy : integer ;
    F : file ;
    F1 : textfile ;
    File_Name : string ;


    procedure _Load_Image ;

    begin
        Buffer := Allocmem( filesize( F ) ) ;
        try
            {$I-}
            blockread( F, Buffer[ 0 ], filesize( F ) ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                exit ;
            end ;
            C.Memory.Load( strtoint( CVTB( Load_Memory_Form.Base, 10, Load_Memory_Form.Starting_Address.Text ) ), filesize( F ), Buffer ) ;
        finally
            freemem( Buffer ) ;
        end ;
    end ;


    procedure _Load_Ent ;

    var Address : int64 ;
        Dummy : integer ;
        Entry_Point : int64 ;
        S, S1 : string ;
        Value : int64 ;
        
    begin
        if( Master_CPU <> nil ) then
        begin
            Entry_Point := Master_CPU.CPU.Get_Current_Address( 0, True ) ;
        end ;
        assignfile( F1, File_Name ) ;
        {$I-}
        reset( F1 ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) ) ;
            exit ;
        end ;
        try
            // Read entry point...
            {$I-}
            readln( F1, S ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                exit ;
            end ;
            S := Edit( S, 8 or 16 or 32 or 128 ) ;
            if( copy( S, 1, 2 ) = 'EN' ) then
            begin
                S := copy( S, 3, length( S ) ) ;
                S := Edit( S, 8 ) ;
                if( Valid_Base( S, 16 ) ) then
                begin
                    Entry_Point := strtoint( CvtB( 16, 10, S ) ) ;
                end ;
            end ;
            Address := strtoint( CVTB( Load_Memory_Form.Base, 10, Load_Memory_Form.Starting_Address.Text ) ) ;
            while( not eof( F1 ) ) do
            begin
                {$I-}
                readln( F1, S ) ;
                {$I+}
                Dummy := IOResult ;
                if( Dummy <> 0 ) then
                begin
                    ShowMessage( ERT( Dummy ) ) ;
                    exit ;
                end ;
                Dummy := pos( ':', S ) ;
                S := copy( S, Dummy + 1, length( S ) ) ;
                S := Edit( S, 8 or 16 or 32 or 128 ) ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( ' ', S + ' ' ) ;
                    S1 := copy( S, 1, Dummy - 1 ) ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    S := Edit( S, 8 ) ;
                    if( copy( S1, length( S1 ), 1 ) = '/' ) then // End of data
                    begin
                        S1 := copy( S1, 1, length( S1 ) - 1 ) ;
                        if( not Valid_Base( S1, 16 ) ) then
                        begin
                            ShowMessage( Text_Error_File_Contains_Invalid_Hexadecimal_Values ) ;
                            exit ;
                        end ;
                        Value := strtoint( CVTB( 16, 10, S1 ) ) ;
                        C.Deposit( Address, ( length( S1 ) + 1 ) * 4, @Value, True ) ;
                        break ;
                    end else
                    begin
                        Value := strtoint( CVTB( 16, 10, S1 ) ) ;
                        if( not Valid_Base( S1, 16 ) ) then
                        begin
                            ShowMessage( Text_Error_File_Contains_Invalid_Hexadecimal_Values ) ;
                            exit ;
                        end ;
                        C.Deposit( Address, ( length( S1 ) + 1 ) * 4, @Value, True ) ;
                        Address := Address + ( ( length( S1 ) + 1 ) div 2 ) ;
                    end ;
                end ;
            end ;
        finally
            closefile( F1 ) ;
        end ;
        if( Master_CPU <> nil ) then
        begin
            Master_CPU.CPU.Set_Current_Address( 0, True, Entry_Point ) ;
        end ;
    end ; // _Load_Ent

var Saved : integer ;

begin
    C := TComponent( TMenuItem( Sender ).Tag ) ;
    if( ( Current_Tab.AD.Base > 1 ) and ( Current_Tab.AD.Base < 50 ) ) then
    begin
        Load_Memory_Form.Base := Current_Tab.AD.Base ;
    end else
    begin
        Load_Memory_Form.Base := 10 ;
    end ;
    if( Load_Memory_Form.ShowModal = mrOK ) then
    begin
        Saved := FileMode ;
        FileMode := fmOpenRead ;
        File_Name := Load_Memory_Form.File_Name.Text ;
        try
            assignfile( F, File_Name ) ;
            {$I-}
            reset( F, 1 ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( ERT( Dummy ) ) ;
                exit ;
            end ;
            Dummy := Extension_Pos( File_Name ) ;
            if( Dummy = 0 ) then
            begin
                Dummy := length( File_Name ) + 1 ;
            end ;
            try
                if( uppercase( copy( File_Name, Dummy + 1, 4 ) ) = 'ENT' ) then
                begin
                    _Load_Ent ;
                end else
                begin
                    _Load_Image ;
                end ;
            finally
                {$I-}
                closefile( F ) ;
                {$I+}
                IOResult ;
            end ;
        finally
            FileMode := Saved ;
        end ;
        Disassembly_Top := -1 ;
        Stack_Top := -1 ;
        Update_Disassembly ;
    end ; // if( Load_Memory_Form.ShowModal = mrOK )
end ; // TMain_Form.LoadMem1Click


function TMain_Form.Current_Tab : TMemory_Tab ;

var I : integer ;

begin
    I := PageControl2.ActivePage.Tag ;
    Result := TMemory_Tab( Memory_Tabs[ I ] ) ;
end ;


function TMain_Form.Memory_Tab_With_Component( Component : TComponent ) : TMemory_Tab ;

var I : integer ;

begin
    if( Component = nil ) then
    begin
        Result := Main_Memory_Tab ;
        exit ;
    end ;
    for I := 0 to Memory_Tabs.Count - 1 do
    begin
        Result := TMemory_Tab( Memory_Tabs[ I ] ) ;
        if( Result.Has_Component( Component ) ) then
        begin
            exit ;
        end ;
    end ;
    Result := nil ;
end ;


procedure TMain_Form.Termination_Notice( C : TComponent ) ;

var Index : integer ;

begin
    for Index := 0 to Component_List.Count - 1 do
    begin
        if( Component_List.Objects[ Index ] = C ) then
        begin
            Remove_Component( Index, False ) ;
            exit ;
        end ;
    end ;
end ;


procedure TMain_Form.Remove_Component( Index : integer ; _Free : boolean ) ;

    procedure Disconnect( C : TComponent ) ;

    var Loop : integer ;
        This_Component : TComponent ;

    begin
        for Loop := 0 to Component_List.Count - 1 do
        begin
            This_Component := TComponent( Component_List.Objects[ Loop ] ) ;
            This_Component.Disconnect_Input( C ) ;
            This_Component.Disconnect_Output( C ) ;
            C.Disconnect_Input( This_Component ) ;
            C.Disconnect_Output( This_Component ) ;
        end ;
    end ;

var C : TComponent ;
    Info : TComponent_Info ;
    Loop : integer ;
    Tab : TMemory_Tab ;

begin
    // Setup...
    C := TComponent( Component_List.Objects[ Index ] ) ;
    if( C = Main_Memory ) then
    begin
        exit ; // Cannot remove default memory
    end ;
    if( Remove_Component_List.Indexof( C ) <> -1 ) then
    begin
        exit ; // Prevent infinite loops
    end ;

    Remove_Component_List.Add( C ) ;
    try
        // Remove from notice list
        UI.Want_Signals( C, False ) ;

        // Find corresponding menu and remove it
        for Loop := 0 to Components1.Count - 1 do
        begin
            if( TComponent( Components1.Items[ Loop ].Tag ) = C ) then
            begin
                Components1.Items[ Loop ].Free ;
                break ;
            end ;
        end ;

        // If unloading a CPU, choose another master CPU...
        if( C = Master_CPU ) then
        begin
            Set_Master_CPU( nil ) ;
            Register_Watchpoints.Clear ;
            Execution_Watchpoints.Clear ;
            Port_Watchpoints.Clear ;
        end ;
        if( C.Component_Type = Component_Type_CPU ) then
        begin
            for Loop := 0 to Component_List.Count - 1 do
            begin
                if( Loop <> Index ) then
                begin
                    if( TComponent( Component_List.Objects[ Loop ] ).Component_Type = Component_Type_CPU ) then
                    begin
                        Set_Master_CPU( TComponent( Component_List.Objects[ Loop ] ) ) ;
                        break ;
                    end ;
                end ;
            end ;
            Update_Disassembly ;
            Update_CPU_State( False ) ;
            Update_Stack ;
            Update_Watches ;
        end ;

        // Disconnect this component from all others and remove menu...
        Info := TComponent_Info( C.Tag ) ;
        Disconnect( C ) ;

        // Remove from memory tabs...
        Tab := Memory_Tab_With_Component( C ) ;
        while( Tab <> nil ) do
        begin
            Tab.Remove( C ) ;
            if( Tab.Count = 0 ) then // No more components on tab
            begin
                Loop := Memory_Tabs.Indexof( Tab ) ;
                if( Loop < 2 ) then // Special tabs
                begin
                    break ;
                end ;
            end ;
            Tab := Memory_Tab_With_Component( C ) ;
        end ;

        // Finally, remove it from the component list and free it...
        Component_List.Objects[ Index ] := nil ;
        Component_List.Delete( Index ) ;
        if( _Free ) then
        begin
            C.Terminate ;
            try
                if( Info.DLL_Handle <> 0 ) then
                begin
                    FreeLibrary( Info.DLL_Handle ) ; // Unload DLL
                end ;
            except
            end ;
        end ;
        try
            Info.Free ;
        except
        end ;

        Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
        Update_Disassembly ;
        Update_Stack ;
    finally
        Remove_Component_List.Remove( C ) ;
    end ;
end ; // TMain_Form.Remove_Component


procedure TMain_Form.Update_CPU_State( Forced : boolean ) ;

var B : boolean ;
    DT : TData_Type ;
    Index : integer ;
    PC : PChar ;
    RName, S, S1 : string ;
    Saved_Index, Saved_Top_Index : integer ;
    Len, Size : integer ;
    Value : int64 ;

begin
    Saved_Index := CPU_Status.ItemIndex ;
    Saved_Top_Index := CPU_Status.TopIndex ;
    CPU_Status.Clear ;
    CPU_State.Clear ;
    if( Master_CPU = nil ) then
    begin
        CPU_Caption.Caption := '' ;
    end else
    begin
        // Do caption...
        Index := Component_List.IndexOfObject( Master_CPU ) ;
        S := Component_List[ Index ] ;
        if( Master_CPU.CPU.Halted and ( not Forced ) ) then
        begin
            S := Substitute1( Text_CPU_Halted, S ) ;
        end ;
        CPU_Caption.Caption := S ;
        if( Forced ) then // This means that execution has started, so just update the heading
        begin
            exit ;
        end ;

        // Do registers...
        Index := 0 ;
        while( True ) do
        begin
            // Get register's data type...
            DT := Register_Information( Master_CPU.CPU, Index ) ;
            if( DT = nil ) then
            begin
                break ;
            end ;
            Size := DT.Size ;
            Len := Size ;
            
            // Get register's name...
            RName := Master_CPU.CPU.Register_Name( Index ) ;
            S := RName + ' ' ;
            while( length( S ) < 6 ) do
            begin
                S := S + ' ' ;
            end ;

            // Get register's value...
            setlength( S1, ( Size + 7 ) div 8 ) ;
            Master_CPU.Examine( Index, Len, PChar( S1 ), False ) ;
            case DT.Data_Type of
                DataType_Boolean:
                    begin
                        if( S1[ 1 ] = #0 ) then
                        begin
                            S1 := 'False' ;
                        end else
                        begin
                            S1 := 'True' ;
                        end ;
                    end ;
                DataType_String:
                    begin
                        case DT.Length_Encoding of
                            Datatype_String_Length_Terminated: // String is terminated by a null
                                begin
                                    Value := pos( #0, S1 ) ;
                                    if( Value > 0 ) then
                                    begin
                                        setlength( S1, Value - 1 ) ;
                                    end ;
                                end ;
                            Datatype_String_Length_Prefix: // String is prefixed with length
                                begin
                                    if( DT.Prefix_Size < 32 ) then
                                    begin
                                        Value := 0 ;
                                        Size := DT.Prefix_Size ;
                                        move( PChar( S1 )[ 0 ], Value, ( Size + 7 ) div 8 ) ; // Get length
                                        Value := Value and ( Bit_Values[ Size ] - 1 ) ;
                                        S1 := copy( S1, ( Size + 7 ) div 8, Value ) ; // Get string
                                    end else
                                    begin
                                        S1 := '?String too long' ;
                                    end ;
                                end ;
                        end ; // case DT.Length_Encoding
                        case DT.Encoding of
                            Datatype_String_Encoding_EBCDIC: S1 := From_EBCDIC( S1 ) ;
                            Datatype_String_Encoding_Radix50: S1 := RAD_To_ASCII( S1 ) ;
                            Datatype_String_Encoding_UTF8: S1 := Utf8ToAnsi( S1 ) ;
                            Datatype_String_Encoding_Unicode16: ; // 16-bit UNICODE not supported in UI
                            Datatype_String_Encoding_Unicode32: ; // 32-bit UNICODE not supported in UI
                        end ; // case DT.Encoding
                    end ;
                DataType_Integer:
                    begin
                        Value := 0 ;
                        move( PChar( S1 )[ 0 ], Value, ( Size + 7 ) div 8 ) ;
                        S1 := CVTB( 10, Master_CPU.CPU.Default_Base, inttostr( Value ) ) ;
                        S1 := Number_Format( Master_CPU.CPU.Default_Base, Size, S1 ) ;
                    end ;
                DataType_Real: ; //~~~ Not yet supported
                DataType_BCD:
                    begin
                        S1 := Decode_BCD( S1, DT.Pack ) ;
                        if( DT.Fixed ) then
                        begin
                            S1 := copy( S1, 1, DT.Fixed_Position - 1 ) + '.' +
                                copy( S1, DT.Fixed_Position, length( S1 ) ) ; 
                        end ;
                    end ;
                else S := '???' ; // Unknown type
            end ;
            S := S + S1 ;
            PC := Master_CPU.CPU.Register_Description( Index ) ;
            if( PC <> nil ) then
            begin
                if( string( PC ) <> RName ) then // Don't be redundant
                begin
                    S := S + '    ' + string( PC ) ;
                end ;
            end ;
            CPU_Status.Items.Add( S ) ;
            inc( Index ) ;
        end ; // while( True )

        // Display states and signals...
        for Index := 0 to Master_CPU.Signal_Count - 1 do
        begin
            if( Master_CPU.Signal_Out( Index ) ) then
            begin
                S := string( Master_CPU.Signal_Name( Index ) ) ;
                Master_CPU.Get_Signal( PChar( S ), B ) ;
                if( Master_CPU.Signal_Active_Low( Index ) = B ) then
                begin
                    S := Substitute1( Text_Signal_Inactive, S ) ;
                end else
                begin
                    S := Substitute1( Text_Signal_Active, S ) ;
                end ;
                CPU_State.Items.Add( S ) ;
            end ;
        end ; // for Index := 0 to Master_CPU.Signal_Count - 1
        if( Saved_Index < CPU_Status.Count ) then
        begin
            CPU_Status.ItemIndex := Saved_Index ;
        end ;
        if( Saved_Top_Index < CPU_Status.Count ) then
        begin
            CPU_Status.TopIndex := Saved_Top_Index ;
        end ;
    end ; // if( Master_CPU = nil )
end ; // TMain_Form.Update_CPU_State


procedure TMain_Form.Update_Watches ;

var Loop, Loop1, Index : integer ;
    S, S1 : string ;
    Size : integer ;
    Value : record
                case integer of
                    0 : ( I : int64 ) ;
                    1 : ( B : array[ 0..sizeof( extended ) - 1 ] of byte ) ;
                    2 : ( S : single ) ;
                    3 : ( D : double ) ;
                    4 : ( E : extended ) ;
            end ;
    W : TWatch ;

begin
    Watch_List_Box.Items.Clear ;
    for Loop := 0 to Watches.Count - 1 do
    begin
        W := TWatch( Watches[ Loop ] ) ;
        S := CvtB( 10, W.Tab.AD.Base, Num2( W.Address ) ) + ':  ' ;
        Loop1 := 0 ;
        while( Loop1 < W.Size ) do
        begin
            Value.I := 0 ;
            Index := 0 ;
            if( W.Base = -10 ) then
            begin
                Size := sizeof( Extended ) ;
            end else
            begin
                Size := sizeof( int64 ) ;
            end ;
            while( ( Index < Size ) and ( Index + Loop1 < W.Size ) ) do
            begin
                Value.B[ Index ] := W.Tab.AI.Get_Byte( W.Address + Loop1 + Index ) ;
                inc( Index ) ;
            end ;
            case W.Base of
                0 : // ASCII
                    begin
                        setlength( S1, Index ) ;
                        move( Value.I, S1[ 1 ], Index ) ;
                        S := S + S1 ;
                    end ;
                1 : // EBCDIC
                    begin
                        setlength( S1, Index ) ;
                        move( Value.I, S1[ 1 ], Index ) ;
                        S := S + From_EBCDIC( S1 ) ;
                    end ;
                50 : // Radix-50
                    begin
                        S := S + Rad( Value.I ) ;
                    end ;
                -4 : // Single
                    begin
                        try
                            S := S + floattostr( Value.S ) ;
                        except
                            S := Substitute1( Text_Error_Invalid_Floating_Point_Format, S ) ;
                        end ;
                    end ;
                -8 : // Double
                    begin
                        try
                            S := S + floattostr( Value.D ) ;
                        except
                            S := Substitute1( Text_Error_Invalid_Floating_Point_Format, S ) ;
                        end ;
                    end ;
                -10 : // Extended
                    begin
                        try
                            S := S + floattostr( Value.E ) ;
                        except
                            S := Substitute1( Text_Error_Invalid_Floating_Point_Format, S ) ;
                        end ;
                    end ;
                else
                    begin
                        if( W.Base = 10 ) then
                        begin
                            Index := 1 ;
                        end else
                        begin
                            Index := length( CvtB( 10, W.Base, Num2( Bit_Masks[ ( W.Size - Loop1 ) * 8 - 1 ] ) ) ) ;
                        end ;
                        S1 := CvtB( 10, W.Base, Num2( Value.I ) ) ;
                        while( length( S1 ) < Index ) do
                        begin
                            S1 := '0' + S1 ;
                        end ;
                        S := S + S1 + ' ' ;
                    end ; // else
            end ; // case W.Base
            Loop1 := Loop1 + sizeof( int64 ) ;
        end ; // while( Loop1 < W.Size )
        Watch_List_Box.Items.Add( S ) ;
    end ;
    DeleteAll1.Enabled := ( Watches.Count > 0 ) ;
    Delete1.Enabled := ( Watches.Count > 0 ) ;
    Edit1.Enabled := ( Watches.Count > 0 ) ;
end ; // TMain_Form.Update_Watches


// Callbacks...

procedure TMain_Form.CB_Address_Space_Click( Sender : TObject ) ;

begin
    Current_Address_Space := TMenuItem( Sender ).Tag ;
    TMenuItem( Sender ).Checked := True ;
    PageControl2.ActivePage.Controls[ 0 ].Invalidate ;
end ;


procedure TMain_Form.CB_Update_Menu( Sender : TObject ) ;

begin
//    UpdateMenuItems( Application.MainForm ) ;
end ;


procedure TMain_Form.CB_Kill( Sender : TObject )  ;

var Editor : TEditor_Window ;

begin
    Editor := TEditor_Window( Sender ) ;
    Add_To_Reopen( Editor.File_Name ) ;
end ;


procedure TMain_Form.CB_Idle( Sender : TObject ; var Done : boolean ) ;

begin
    if( _Hide ) then
    begin
        Hide ;
        _Hide := False ;
    end ;
    if( Start_Execution ) then
    begin
        Start_Execution := False ;
        Execute1Click( nil ) ;
    end ;
    if( Terminate_Requested ) then
    begin
        Terminate_Base_Time := GetTickCount ;
        Terminate_Requested := False ;
        Terminate_Wait := True ;
    end else
    if( Terminate_Wait ) then
    begin
        if( GetTickCount < Terminate_Base_Time ) then
        begin
            Terminate_Base_Time := GetTickCount ; // Had a time rollover
        end else
        if( Terminate_Base_Time - GetTickCount > 500 ) then
        begin
            Terminate_Wait := False ;
            if( not Startup_Command_File ) then
            begin
                UnloadAll1Click( nil ) ;
            end else
            begin
                Close ;
            end ;
        end ;
    end ;
    UI.Idle( TComponent( self ) ) ;
end ; // TMain_Form.CB_Idle


procedure TMain_Form.CB_State_Change( Sender : TObject ) ;

var Editor : TEditor_Window ;

begin
    if( StatusBar = nil ) then
    begin
        exit ; // In setup phase
    end ;
    Editor := TEditor_Window( Sender ) ;
    if( Editor.Modified ) then
    begin
        StatusBar.Panels.Items[ 1 ].Text := Text_Status_Modified ;
    end else
    begin
        StatusBar.Panels.Items[ 1 ].Text := '' ;
    end ;
    StatusBar.Panels.Items[ 0 ].Text := inttostr( Editor.Current_Row ) +
        ':' + inttostr( Editor.Current_Column ) ;
end ;


procedure TMain_Form.Printersetup1Click( Sender : TObject ) ;

begin
    PrinterSetupDialog1.Execute ;
end ;


procedure TMain_Form.Print1Click(Sender: TObject);

var Editor : TEditor_Window ;

begin
    Editor := TEditor_Window( PageControl1.ActivePage.Controls[ 0 ] ) ;
    Editor.Print ;
end ;


procedure TMain_Form.Close1Click(Sender: TObject);

var Editor : TEditor_Window ;
    T : TCEF_TabSheet ;

begin
    T := TCEF_TabSheet( PageControl1.ActivePage ) ;
    if( T.Editor = nil ) then // Disassembly tab
    begin
        exit ;
    end ;
    Editor := TEditor_Window( PageControl1.ActivePage.Controls[ 0 ] ) ;
    if( Editor.Modified ) then
    begin
        Save_Modified_Dialog.Label1.Caption :=
            Substitute1( Text_Prompt_Save_Changes_To, Editor.File_Name ) ;
        case Save_Modified_Dialog.ShowModal of
            mrCancel: exit ;
	        mrYes: Editor.Save_File ;
        end ;
    end ;
    Add_To_Reopen( Editor.File_Name ) ;
    PageControl1.ActivePage.Free ;
    //Editor.Close ;
end ;


procedure TMain_Form.CloseAll1Click( Sender : TObject ) ;

var Editor : TEditor_Window ;
    Loop : integer ;

begin
    for Loop := 0 to PageControl1.PageCount - 1 do
    begin
        if( TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor <> nil ) then
        begin
            Editor := TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor ;
            Add_To_Reopen( Editor.File_Name ) ;
            Editor.Close ;
        end ;
    end ;
end ;


procedure TMain_Form.Reopen_File( Sender : TObject ) ;

var Menu : TMenuItem ;

begin
    Menu := TMenuItem( Sender ) ;
    if( Menu.Visible ) then
    begin
        Open_File( copy( Menu.Caption, 4, 255 ) ) ;
        exit ;
    end ;
end ;


procedure TMain_Form.Reopen_Emulator( Sender : TObject ) ;

var Menu : TMenuItem ;

begin
    Menu := TMenuItem( Sender ) ;
    if( Menu.Visible ) then
    begin
        _Hide := False ;
        Open_Emulator( copy( Menu.Caption, 4, 255 ) ) ;
        if( _Hide ) then
        begin
            Hide ;
            _Hide := False ;
        end ;
    end ;
end ;


procedure TMain_Form.Load1Click( Sender : TObject ) ;

begin
    Load_Component_Form.UI := UI ;
    if( Load_Component_Form.ShowModal = mrOK ) then
    begin
        Load_Component( Load_Component_Form.Filename.Text,
            Load_Component_Form.Setup.Text,
            Load_Component_Form.Component_Name.Text,
            Load_Component_Form.Domain.Text ) ;
    end ; 
end ; // TMain_Form.Load1Click


procedure TMain_Form.FormCloseQuery( Sender : TObject ;
    var CanClose : Boolean ) ;

var Editor : TEditor_Window ;
    Loop : integer ;

begin
    CanClose := False ;
    for Loop := 0 to PageControl1.PageCount - 1 do
    begin
        Editor := TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor ;
        if( ( Editor <> nil ) and Editor.Modified ) then
        begin
            Save_Modified_Dialog.Label1.Caption :=
                Substitute1( Text_Prompt_Save_Changes_To, Editor.File_Name ) ;
            case Save_Modified_Dialog.ShowModal of
                mrCancel: exit ;
                mrYes: Editor.Save_File ;
            end ;
        end ;
    end ;
    CanClose := True ;
    for Loop := 0 to PageControl1.PageCount - 1 do
    begin
        Editor := TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor ;
        if( Editor <> nil ) then
        begin
            Editor.On_Kill := nil ;
        end ;
    end ;
    UnloadAll1Click( nil ) ;
end ; // TMain_Form.FormCloseQuery


procedure TMain_Form.CPU_StatusDblClick( Sender : TObject ) ;

var Dummy, Len, Size, Value : integer ;
    S : string ;

begin
    if( CPU_Status.ItemIndex <> -1 ) then
    begin
        Size := Master_CPU.CPU.Register_Size( CPU_Status.ItemIndex ) ;
        S := CPU_Status.Items[ CPU_Status.ItemIndex ] ;
        Dummy := pos( ' ', S + ' ' ) ;
        S := copy( S, 1, Dummy - 1 ) ;
        Modify_Register.Caption := Substitute1( Text_Caption_Modify_Register, S ) ;
        Modify_Register.Base := Master_CPU.CPU.Default_Base ;
        Modify_Register.Size := Size ;
        Value := 0 ;
        Len := Size ;
        Master_CPU.Examine( CPU_Status.ItemIndex, Len, @Value, False ) ;
        Modify_Register.Edit1.Text := CVTB( 10, Master_CPU.CPU.Default_Base, inttostr( Value ) ) ;
        Modify_Register.OK_Button.Enabled := True ;
        Modify_Register.Value := Value ;
        if( Modify_Register.ShowModal = mrOK ) then
        begin
            Master_CPU.Deposit( CPU_Status.ItemIndex, Size,
                @Modify_Register.Value, False ) ;
            Update_CPU_State( False ) ;
        end ;
    end ;
end ;


procedure TMain_Form.Modify1Click( Sender : TObject ) ;

var Address : int64 ;
    Base : integer ;
    Data, Work : string ;
    Dummy, Loop : integer ;
    V : int64 ;

begin                           
    Modify_Memory_Dialog.Data.Text :=
        CVTB( 10, Current_Tab.AD.Base, inttostr( Current_Tab.AI.Get_Byte( Modify_Memory_Dialog.Address_Value ) ) ) ;
    Modify_Memory_Dialog.Base := Current_Tab.AD.Base ;
    Modify_Memory_Dialog.OK_Button.Enabled := True ;
    Modify_Memory_Dialog.Size := Current_Tab.AD.Number_Size ;
    if( Modify_Memory_Dialog.ShowModal = mrOK ) then
    begin
        Address := Modify_Memory_Dialog.Address_Value ;
        Data := Modify_Memory_Dialog.Data.Text ;
        if( Modify_Memory_Dialog.Literal_Text.Checked ) then
        begin
            for Dummy := 1 to length( Data ) do
            begin
                Current_Tab.AI.Set_Byte( Address + Dummy - 1, ord( Data[ Dummy ] ) ) ;
            end ;
        end else
        begin
            while( length( Data ) > 0 ) do
            begin
                Dummy := pos( ' ', Data + ' ' ) ;
                Work := copy( Data, 1, Dummy - 1 ) ;
                Data := copy( Data, Dummy + 1, length( Work ) ) ;
                Base := Current_Tab.AD.Base ;
                Translate_Value( Work, 0, Base, V ) ;
                for Loop := 1 to Modify_Memory_Dialog.Size do
                begin
                    Current_Tab.AI.Set_Byte( Address, V and $FF ) ;
                    V := V shr 8 ;
                    inc( Address ) ;
                end ;
            end ;
        end ;
        Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
        Update_Disassembly ;
        Update_Stack ;
    end ;
end ; // TMain_Form.Modify1Click


procedure TMain_Form.Createnew1Click( Sender : TObject ) ;

var Address : int64 ;
    Access : integer ;

begin
    Address := Current_Tab.AD.Home_Index ;
    Access := 0 ;
    Add_Watchpoint_Ex( UI, nil, Component_Type_Memory, Watchpoints,
        Current_Tab.AD.Base, Current_Tab.AD.Number_Size, 0, High_Address, True,
        Address, Access, Current_Tab, Current_Tab.Name ) ;
    if( ( Address <= -1 ) and ( Access = -1 ) ) then
    begin
        exit ;
    end ;
    Current_Tab.Set_Watch( Address, True, Access ) ;
end ;


procedure TMain_Form.View1Click( Sender : TObject ) ;

begin
    Show_Watchpoints( UI, nil, Watchpoints, False ) ;
end ;


procedure TMain_Form.Execute1Click( Sender : TObject ) ;

var C : TComponent ;
    Data : int64 ;
    Loop : integer ;

begin
    if( In_Assembly or In_Execution ) then
    begin
        exit ;
    end ;

    // Pre-emptive notice to components...
    for Loop := 0 to Component_List.Count - 1 do
    begin
        Data := 1 ;
        C := TComponent( Component_List.Objects[ Loop ] ) ;
        C.UI_Notice( UI_Notice_Request_Changed_Run_State, Data ) ;
        if( Data = 2 ) then // Prevented
        begin
            exit ;
        end ;
    end ;

    // Set execution state...
    StatusBar.Panels.Items[ 2 ].Text := Text_Status_Executing ;
    In_Execution := True ;
    Update_CPU_State( True ) ;

    // Notice to components...
    for Loop := 0 to Component_List.Count - 1 do
    begin
        Data := 1 ;
        C := TComponent( Component_List.Objects[ Loop ] ) ;
        C.UI_Notice( UI_Notice_Changed_Run_State, Data ) ;
    end ;

    // Start runnning...
    try
        Master_CPU.CPU.Run ;
    except
    end ;
    In_Execution := False ;
    StatusBar.Panels.Items[ 2 ].Text := Text_Status_Stopped ;
    Update_CPU_State( False ) ;
    Update_Watches ;
    Disassembly_Top := -1 ;
    Stack_Top := -1 ;
    Update_Disassembly ;
    Update_Stack ;
    Map_Source( False ) ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;

    // Notice to components...
    for Loop := 0 to Component_List.Count - 1 do
    begin
        Data := 0 ;
        C := TComponent( Component_List.Objects[ Loop ] ) ;
        C.UI_Notice( UI_Notice_Changed_Run_State, Data ) ;
    end ;
end ; // TMain_Form.Execute1Click


procedure TMain_Form.StepOver1Click(Sender: TObject) ;

begin
    if( In_Assembly or In_Execution ) then
    begin
        exit ;
    end ;
    StatusBar.Panels.Items[ 2 ].Text := Text_Status_Executing ;
    In_Execution := True ;
    Master_CPU.CPU.Step( False ) ;
    In_Execution := False ;
    StatusBar.Panels.Items[ 2 ].Text := Text_Status_Stopped ;
    Update_CPU_State( False ) ;
    Update_Watches ;
    Disassembly_Top := -1 ;
    Stack_Top := -1 ;
    Update_Disassembly ;
    Update_Stack ;
    Map_Source( False ) ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
end ;


procedure TMain_Form.StepInto1Click(Sender: TObject);

begin
    if( In_Assembly or In_Execution ) then
    begin
        exit ;
    end ;
    StatusBar.Panels.Items[ 2 ].Text := Text_Status_Executing ;
    In_Execution := True ;
    Master_CPU.CPU.Step( True ) ;
    In_Execution := False ;
    StatusBar.Panels.Items[ 2 ].Text := Text_Status_Stopped ;
    Update_CPU_State( False ) ;
    Update_Watches ;
    Disassembly_Top := -1 ;
    Stack_Top := -1 ;
    Update_Disassembly ;
    Update_Stack ;
    Map_Source( False ) ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
end ;


procedure TMain_Form.Runimmediate1Click( Sender : TObject ) ;

var B : byte ;
    In_Streamer, Out_Streamer : TLine_Streamer ;
    Loop : integer ;
    Old : int64 ;
    S : string ;
    Size : integer ;
    Status : TCEF_Assembler_Status ;

begin
    if( In_Assembly or In_Execution ) then
    begin
        exit ;
    end ;
    S := Immediate_Mode_Edit.Text ;
    Last_Immediate_Mode_Edit_Text := S ;
    In_Streamer := TLine_Streamer.Create ;
    Out_Streamer := TLine_Streamer.Create ;
    Status := TCEF_Assembler_Status.Create ;
    try
        TCEF_Assembler_Status( Status ).In_Streamer := In_Streamer ;
        In_Streamer.Write( S[ 1 ], length( S ) ) ;
        In_Streamer.Seek( 0 ) ;
        if( Master_Assembler.Version < 22 ) then
        begin
            Master_Assembler.Assemble( In_Streamer, Out_Streamer, nil, Status ) ;
        end else
        begin
            Master_Assembler.Assemble_Ex( In_Streamer, Out_Streamer, nil, Status, ASF_Immediate_Mode ) ;
        end ;
        if( Status.Errors > 0 ) then
        begin
            ShowMessage( Status.Get_Error_Text ) ;
            exit ;
        end ;
        Old := Master_CPU.CPU.Get_Current_Address( 0, True ) ;
        Out_Streamer.Seek( 0 ) ;
        if( Status.Data > 0 ) then
        begin
            for Loop := 0 to Out_Streamer.Size - 1 do
            begin
                Size := 1 ;
                Out_Streamer.Read( B, Size ) ;
                Deposit_Memory( Old, B ) ;
                inc( Old ) ;
            end ;
        end else
        if( Out_Streamer.Size > 0 ) then // Have code to execute
        begin
            StatusBar.Panels.Items[ 2 ].Text := Text_Status_Executing ;
            In_Execution := True ;
            try
                Master_CPU.CPU.Run_From_Stream( Out_Streamer ) ;
            finally
                In_Execution := False ;
            end ;
            StatusBar.Panels.Items[ 2 ].Text := Text_Status_Stopped ;
        end ;

    finally
        // Clean up
        In_Streamer.Free ;
        Out_Streamer.Free ;
        Status.Free ;
    end ;

    // Make sure nothing stays blocked in immediate mode
    if( Options_Form.Immediate_Mode_Unblock.Checked ) then
    begin                                   
        UI.Clock.Unblock ;
    end ;

    // Update various panes...
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    if( Old <> Master_CPU.CPU.Get_Current_Address( 0, True ) ) then // Position changed by user
    begin
        Disassembly_Top := -1 ;
    end ;
    Stack_Top := -1 ;
    Update_Disassembly ;
    Update_Stack ;
    Update_CPU_State( False ) ;
    Update_Watches ;
    Map_Source( False ) ;
end ; // TMain_Form.Runimmediate1Click


procedure TMain_Form.UnloadAll1Click( Sender : TObject ) ;

var Loop : integer ;

begin
    Pause1Click( nil ) ; // Terminate execution
    if( In_Assembly or In_Execution ) then
    begin
        exit ;
    end ;
    for Loop := Component_List.Count - 1 downto 0 do
    begin
        Remove_Component( Loop, True ) ;
    end ;
    Low_Physical_Address := 0 ;
    High_Physical_Address := $7FFFFFFFFFFFFFFF ; // Default high address
    Low_Port_Address := 0 ;
    High_Port_Address := $7FFFFFFFFFFFFFFF ;
    Physical_Address := True ;
    Caption := 'CEF32' ;
end ;


procedure TMain_Form.Write_Memory( Address : int64 ; Value : byte ) ;

var C : TComponent ;
    Loop : integer ;
    Low, High : int64 ;

begin
    for Loop := 0 to Component_List.Count - 1 do
    begin
        C := TComponent( Component_List.Objects[ Loop ] ) ;
        if( C.Memory <> nil ) then
        begin
            try
                C.Memory.Get_Address_Range( Low, High ) ;
                if( ( Address >= Low ) and ( Address <= High ) ) then
                begin
                    C.Write( Address, Value, 8, IO_Type_Memory ) ;
                end ;
            except // Don't let bad component abort program
            end ;
        end ;
    end ;
    if( Main_Memory <> nil ) then
    begin
        Main_Memory.Write( Address, Value, 8, IO_Type_Memory ) ;
    end ;
    if( In_Assembly and ( not Write_Errors ) ) then
    begin
        if( Examine_Memory( Address ) <> Value ) then
        begin
            Write_Errors := True ;
            Status.Log_Error( PChar( Text_Error_Could_Not_Modify_Memory ), nil, -1, Severity_Fatal ) ;
        end ;
    end ;
end ; // TMain_Form.Write_Memory


function TMain_Form.Deposit_Memory( Address : int64 ; Value : byte ) : TUnified_Exception ;

var C : TComponent ;
    Loop : integer ;
    Low, High : int64 ;
    Success : boolean ;

begin
    if( ( Master_CPU <> nil ) and not Physical_Address ) then
    begin
        Address := Master_CPU.CPU.Translate( Current_Address_Space, Address ) ;
    end ;
    fillchar( Result, sizeof( Result ), 0 ) ;
    Success := False ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        C := TComponent( Component_List.Objects[ Loop ] ) ;
        if( C.Memory <> nil ) then
        begin
            C.Memory.Get_Address_Range( Low, High ) ;
            if( ( Address >= Low ) and ( Address <= High ) ) then
            begin
                Result := C.Deposit( Address, 8, @Value, True ) ;
                if( Result = nil ) then
                begin
                    Success := True ;
                end ;
            end ;
        end ;
    end ;
    if( Success ) then
    begin
        fillchar( Result, sizeof( Result ), 0 ) ;
        exit ;
    end ;
    if( Main_Memory <> nil ) then
    begin
        Result := Main_Memory.Deposit( Address, 8, @Value, True ) ;
    end ;
end ;


function TMain_Form.Examine_Memory( Address : int64 ) : byte ;

begin
    Result := _Examine_Memory( Address, Physical_Address, Current_Address_Space ) ;
end ;


function TMain_Form.Deposit_Port( Address : int64 ; Value : byte ) : TUnified_Exception ;

var C : TComponent ;
    Loop : integer ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        C := TComponent( Component_List.Objects[ Loop ] ) ;
        Result := C.Deposit( Address, 8, @Value, False ) ;
        if( Result <> nil ) then
        begin
            exit ;
        end ;
    end ;
end ;


function TMain_Form.Examine_Port( Address : int64 ) : byte ;

var B : byte ;
    C : TComponent ;
    Loop : integer ;
    Size : integer ;

begin
    Size := 8 ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        try
            C := TComponent( Component_List.Objects[ Loop ] ) ;
            if( C.Examine( Address, Size, @B, False ) = nil ) then
            begin
                Result := B ;
                exit ;
            end ;
        except
        end ;
    end ;
    Result := 0 ;
end ;


procedure TMain_Form.Show_Error( Sender : TListBox ) ;

var Editor : TEditor_Window ;
    Filename : string ;
    Index, Line, Loop : longint ;

begin
    Index := Sender.ItemIndex ;
    Filename := Assembly_Statistics.Filenames[ Index ] ;
    Line := Assembly_Statistics.Lines[ Index ] ;
    for Loop := 0 to PageControl1.PageCount - 1 do
    begin
        if( TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor <> nil ) then
        begin
            Editor := TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor ;
            if( Editor.File_Name = Filename ) then
            begin
                Editor.Select_Line( Line ) ;
                exit ;
            end ;
        end ;
    end ;
end ;


function TMain_Form.Menu_For_Component( Component : TComponent ) : TMenuItem ;

var Loop : integer ;
    M : TMenuItem ;

begin
    Result := nil ; // Assume failure
    for Loop := 0 to Components1.Count - 1 do
    begin
        M := Components1[ Loop ] ;
        if( M.Tag = integer( Component ) ) then
        begin
            Result := M ;
            exit ;
        end ;
    end ;
end ;


procedure TMain_Form.Disassembly_ScrollbarScroll( Sender : TObject ;
    ScrollCode : TScrollCode ; var ScrollPos : Integer ) ;

var R : double ;
    HA : int64 ;

    procedure Adjust_Thumb ;

    begin
        if( Disassembly_Top <= Low_Address ) then
        begin
            Disassembly_Top := Low_Address ;
        end ;
        R := Disassembly_Top - Low_Address ;
        if( HA = Low_Address ) then
        begin
            R := 0 ;
        end else
        begin
            R := R / ( HA - Low_Address ) ;
        end ;
        R := int( R * 100 ) ;
        ScrollPos := trunc( R ) ;
    end ;

begin // Disassembly scroll
    if( Physical_Address ) then
    begin
        HA := High_Physical_Address ;
    end else
    begin
        HA := High_Address ;
    end ;
    if( HA = 0 ) then // Memory bounds not set
    begin
        ScrollPos := 0 ;
        exit ;
    end ;
    case ScrollCode of
        scLineUp :
            begin
                if( ( Disassembly_Top = -1 ) and ( Master_CPU <> nil ) ) then
                begin
                    Disassembly_Top := Master_CPU.CPU.Get_Current_Address( 0, True ) ;
                end ;
                if( Disassembly_Top <= Low_Address ) then
                begin
                    exit ;
                end ;
                dec( Disassembly_Top ) ;
                Adjust_Thumb ;
            end ;
        scLineDown :
            begin
                Disassembly_Top := Disassembly_Second ;
                Adjust_Thumb ;
            end ;
        scPageUp :
            begin
                if( Disassembly_Top <= Low_Address ) then
                begin
                    exit ;
                end ;
                Disassembly_Top :=
                    Disassembly_Top - ( Disassembly_List_Box.ClientHeight div Disassembly_List_Box.ItemHeight ) ;
                Adjust_Thumb ;
            end ;
        scPageDown :
            begin
                if( Disassembly_Next >= HA ) then
                begin
                    exit ;
                end ;
                Disassembly_Top := Disassembly_Next ;
                Adjust_Thumb ;
            end ;
        scPosition :
            begin
                R := ScrollPos ;
                R := R / 100.0 ;
                R := R * ( HA - Low_Address ) ;
                R := int( R + Low_Address ) ;
                Disassembly_Top := trunc( R ) ;
            end ;
        scTop :
            begin
                Disassembly_Top := Low_Address ;
            end ;
        scBottom :
            begin
                Disassembly_Top := HA ;
            end ;
        else exit ;
    end ;
    Update_Disassembly ;
end ; // TMain_Form.Disassembly_ScrollbarScroll


procedure TMain_Form.Disassembly_List_BoxKeyDown(Sender: TObject;
    var Key: Word; Shift: TShiftState) ;

var ScrollPos : Integer ;

begin
    case Key of
        VK_UP :
            begin
                if( ssCtrl in Shift ) then
                begin
                    exit ;
                end ;
                if( Disassembly_List_Box.ItemIndex = 0 ) then
                begin
                    Disassembly_ScrollbarScroll( Disassembly_Scrollbar, scLineUp, ScrollPos ) ;
                end ;
            end ;
        VK_DOWN :
            begin
                if( ssCtrl in Shift ) then
                begin
                    exit ;
                end ;
                if( Disassembly_List_Box.ItemIndex = Disassembly_List_Box.ClientHeight div Disassembly_List_Box.ItemHeight - 1 ) then
                begin
                    Disassembly_ScrollbarScroll( Disassembly_Scrollbar, scLineDown, ScrollPos ) ;
                end ;
            end ;
        VK_NEXT :
            begin
                Key := 0 ;
                if( ssCtrl in Shift ) then
                begin
                    Disassembly_ScrollbarScroll( Disassembly_Scrollbar, scBottom, ScrollPos ) ;
                    exit ;
                end ;
                Disassembly_ScrollbarScroll( Disassembly_Scrollbar, scPageDown, ScrollPos ) ;
            end ;
        VK_PRIOR :
            begin
                Key := 0 ;
                if( ssCtrl in Shift ) then
                begin
                    Disassembly_ScrollbarScroll( Disassembly_Scrollbar, scTop, ScrollPos ) ;
                    exit ;
                end ;
                Disassembly_ScrollbarScroll( Disassembly_Scrollbar, scPageUp, ScrollPos ) ;
            end ;
        VK_HOME :
            begin
                if( ssCtrl in Shift ) then
                begin
                    Key := 0 ;
                    Disassembly_ScrollbarScroll( Disassembly_Scrollbar, scTop, ScrollPos ) ;
                end ;
            end ;
        VK_END :
            begin
                if( ssCtrl in Shift ) then
                begin
                    Key := 0 ;
                    Disassembly_ScrollbarScroll( Disassembly_Scrollbar, scBottom, ScrollPos ) ;
                end ;
            end ;
    end ;
end ;


procedure TMain_Form.Immediate_Mode_PanelResize( Sender : TObject ) ;

begin
    Immediate_Mode_Edit.Width :=
        Immediate_Mode_Panel.Width - Immediate_Mode_Edit.Left ;
end ;


procedure TMain_Form.Immediate_Mode_EditKeyDown(Sender: TObject;
    var Key: Word; Shift: TShiftState);

begin
    if( Key = VK_RETURN ) then
    begin
        Key := 0 ;
        Runimmediate1Click( nil ) ;
        Immediate_Mode_Edit.Text := '' ;
    end else
    if( Key = VK_UP ) then
    begin
        Key := 0 ;
        Immediate_Mode_Edit.Text := Last_Immediate_Mode_Edit_Text ;
    end ;
end ;


procedure TMain_Form.Pause1Click( Sender : TObject ) ;

var C : TComponent ;
    Data : int64 ;
    Loop : integer ;

begin
    if( Master_CPU <> nil ) then
    begin
        // Pre-emptive notice to components...
        for Loop := 0 to Component_List.Count - 1 do
        begin
            Data := 0 ;
            C := TComponent( Component_List.Objects[ Loop ] ) ;
            C.UI_Notice( UI_Notice_Request_Changed_Run_State, Data ) ;
            if( Data = 2 ) then // Prevented
            begin
                exit ;
            end ;
        end ;

        // Notice to components...
        for Loop := 0 to Component_List.Count - 1 do
        begin
            Data := 0 ;
            C := TComponent( Component_List.Objects[ Loop ] ) ;
            C.UI_Notice( UI_Notice_Changed_Run_State, Data ) ;
        end ;

        // Pause CPU...
        Master_CPU.CPU.Stop ;
    end ;
end ;


procedure TMain_Form.Increment1Click( Sender : TObject ) ;

var Index, Size, Len : integer ;
    P : pointer ;
    Value : int64 ;

begin
    Index := CPU_Status.ItemIndex ;
    if( Index <> -1 ) then
    begin
        Size := Master_CPU.CPU.Register_Size( Index ) ;
        Len := Size ;
        Value := 0 ;
        P := @Value ;
        Master_CPU.Examine( Index, Size, P, False ) ;
        inc( Value ) ;
        Master_CPU.Deposit( Index, Len, P, False ) ;
        Update_CPU_State( False ) ;
        CPU_Status.ItemIndex := Index ;
    end ;
end ;


procedure TMain_Form.Decrement1Click( Sender : TObject ) ;

var Index, Size, Len : integer ;
    P : pointer ;
    Value : int64 ;

begin
    Index := CPU_Status.ItemIndex ;
    if( Index <> -1 ) then
    begin
        Size := Master_CPU.CPU.Register_Size( Index ) ;
        Len := Size ;
        Value := 0 ;
        P := @Value ;
        Master_CPU.Examine( Index, Size, P, False ) ;
        dec( Value ) ;
        Master_CPU.Deposit( Index, Len, P, False ) ;
        Update_CPU_State( False ) ;
        CPU_Status.ItemIndex := Index ;
    end ;
end ;


procedure TMain_Form.Change1Click( Sender : TObject ) ;

var Dlg : TEdit_Value_Form ;
    Index : integer ;
    P : PChar ;
    Size : integer ;
    Value : string ;

begin
    Index := CPU_Status.ItemIndex ;
    if( Index = -1 ) then
    begin
        exit ;
    end ;

    Dlg := TEdit_Value_Form.Create( self ) ;
    Dlg.Data_Type := Register_Information( Master_CPU.CPU, Index ) ;
    Dlg.Base := Master_CPU.CPU.Default_Base ;
    setlength( Value, ( Dlg.Data_Type.Size + 7 ) div 8 ) ;
    Size := Dlg.Data_Type.Size ;
    Master_CPU.Examine( Index, Size, PChar( Value ), False ) ;
    Dlg.Value := Value ;
    if( Dlg.ShowModal = mrOK ) then
    begin
        Value := Dlg.Value ;
        P := PChar( Value ) ;
        if( Dlg.Data_Type.Data_Type = DataType_String ) then
        begin
            Size := length( Value ) * 8 ;
        end ;
        Master_CPU.Deposit( Index, Size, P, False ) ;
        Update_CPU_State( False ) ;
    end ;
end ; // TMain_Form.Change1Click



procedure TMain_Form.CPU_StatusMouseUp( Sender : TObject ;
    Button : TMouseButton ; Shift : TShiftState ; X, Y : Integer ) ;

var DT : _CEF.TData_Type ;
    Index : integer ;

begin
    if( Button = mbRight ) then
    begin
        Index := CPU_Status.ItemAtPos( Point( X, Y ), True ) ;
        if( Index <> -1 ) then
        begin
            CPU_Status.ItemIndex := Index ;
            DT := Register_Information( Master_CPU.CPU, Index ) ;
            Increment1.Enabled := DT.Data_Type = DataType_Integer ;
            Decrement1.Enabled := DT.Data_Type = DataType_Integer ;
            CPU_PopupMenu.Popup( Mouse.CursorPos.X, Mouse.CursorPos.Y ) ;
        end ;
    end ;
end ;


procedure TMain_Form.Zero1Click( Sender : TObject ) ;

var DT : _CEF.TData_Type ;
    Index, Size : integer ;
    P : pointer ;
    Value : int64 ;

begin
    Index := CPU_Status.ItemIndex ;
    if( Index <> -1 ) then
    begin
        DT := Register_Information( Master_CPU.CPU, Index ) ;
        if( DT.Data_Type = DataType_String ) then
        begin
            Size := 0 ;
            Master_CPU.Deposit( Index, Size, nil, False ) ;
        end else
        begin
            Size := Master_CPU.CPU.Register_Size( Index ) ;
            Value := 0 ;
            P := @Value ;
            Master_CPU.Deposit( Index, Size, P, False ) ;
        end ;
        Update_CPU_State( False ) ;
        CPU_Status.ItemIndex := Index ;
    end ;
end ;


procedure TMain_Form.PageControl1Change( Sender : TObject ) ;

var T : TCEF_TabSheet ;
    V : boolean ;

begin
    if( Last_Editor <> nil ) then
    begin
        MainMenu1.UnMerge( Last_Editor.Main_Menu ) ;
    end ;
    T := TCEF_TabSheet( PageControl1.ActivePage ) ;
    V := ( T.Editor <> nil ) ;
    Assemble2.Enabled := V ;
    Tabsheet_1.Editor.Edit1.Enabled := V ;
    Close1.Enabled := V ;
    Save3.Enabled := V ;
    Last_Editor := T.Editor ;
    if( Last_Editor <> nil ) then
    begin
        MainMenu1.Merge( Last_Editor.Main_Menu ) ;
    end ;
end ;


procedure TMain_Form.Paste1Click( Sender : TObject ) ;

var DT : _CEF.TData_Type ;
    Index : integer ;
    Len, Size : integer ;
    P : pointer ;
    S : string ;
    Value : int64 ;

begin
    Index := CPU_Status.ItemIndex ;
    if( Index <> -1 ) then
    begin
        S := Clipboard.AsText ;
        try
            DT := Register_Information( Master_CPU.CPU, Index ) ;
            if( DT.Data_Type = DataType_String ) then
            begin
                Len := length( S ) * 8 ;
                Master_CPU.Deposit( Index, Len, PChar( S ), False ) ;
            end else
            begin
                Size := Master_CPU.CPU.Register_Size( Index ) ;
                Value := strtoint64( S ) ;
                P := @Value ;
                Len := Size ;
                Master_CPU.Deposit( Index, Len, P, False ) ;
            end ;
            Update_CPU_State( False ) ;
            CPU_Status.ItemIndex := Index ;
        except
        end ;
    end ;
end ; // TMain_Form.Paste1Click


procedure TMain_Form.AddWatch1Click( Sender : TObject ) ;

var P : TWatch ;

begin
    if( New_Watch_Dialog = nil ) then
    begin
        New_Watch_Dialog := TNew_Watch_Dialog.Create( self ) ;
    end ;
    if( Physical_Address ) then
    begin
        New_Watch_Dialog.Minimum := Low_Physical_Address ;
        New_Watch_Dialog.Maximum := High_Physical_Address ;
    end else
    begin
        New_Watch_Dialog.Minimum := Low_Address ;
        New_Watch_Dialog.Maximum := High_Address ;
    end ;
    New_Watch_Dialog.Base := Current_Tab.AD.Base ;
    if( Master_CPU = nil ) then
    begin
        New_Watch_Dialog._Assembler := nil ;
    end else
    begin
        New_Watch_Dialog._Assembler :=
            TComponent_Info( Master_CPU.Tag ).Assembler_Context ;
    end ;

    if( New_Watch_Dialog.ShowModal = mrOK ) then
    begin
        P := TWatch.Create ;
        P.Address := New_Watch_Dialog.Value ;
        P.Size := New_Watch_Dialog.Size.Value ;
        P.Tab := Current_Tab ;
        if( New_Watch_Dialog.Binary_RB.Checked ) then
        begin
            P.Base := 2 ;
        end else
        if( New_Watch_Dialog.Octal_RB.Checked ) then
        begin
            P.Base := 8 ;
        end else
        if( New_Watch_Dialog.Decimal_RB.Checked ) then
        begin
            P.Base := 10 ;
        end else
        if( New_Watch_Dialog.Hexadecimal_RB.Checked ) then
        begin
            P.Base := 16 ;
        end else
        if( New_Watch_Dialog.Other_RB.Checked ) then
        begin
            P.Base := New_Watch_Dialog.Base_Spin.Value ;
        end else
        if( New_Watch_Dialog.ASCII_RB.Checked ) then
        begin
            P.Base := 0 ;
        end else
        if( New_Watch_Dialog.EBCDIC_RB.Checked ) then
        begin
            P.Base := 1 ;
        end else
        if( New_Watch_Dialog.Radix50_RB.Checked ) then
        begin
            P.Base := 50 ;
        end else
        if( New_Watch_Dialog.Single_RB.Checked ) then
        begin
            P.Base := -4 ;
        end else
        if( New_Watch_Dialog.Double_RB.Checked ) then
        begin
            P.Base := -8 ;
        end else
        if( New_Watch_Dialog.Extended_RB.Checked ) then
        begin
            P.Base := -10 ;
        end ;
        Watches.Add( P ) ;
        Update_Watches ;
    end ;
end ;


procedure TMain_Form.AddBreakpoint1Click( Sender : TObject ) ;

var Tab : TMemory_Tab ;

begin
    if( Master_CPU <> nil ) then
    begin
        if( Main_Form.Master_CPU.Version < 26 ) then
        begin
            Tab := Main_Form.Main_Memory_Tab ;
        end else
        begin
            Tab := Main_Form.Memory_Tab_With_Component( Master_CPU.CPU.Get_Target_Memory ) ;
            if( Tab = nil ) then
            begin
                Tab := Main_Form.Main_Memory_Tab ;
            end ;
        end ;
        Add_Breakpoint( UI, Master_CPU, Execution_Watchpoints, Tab.AD.Base,
            Tab.AD.Number_Size, False ) ;
    end ;
end ;


procedure TMain_Form.AddRegisterBreakpoint1Click( Sender : TObject ) ;

var Address : int64 ;
    Access : integer ;

begin
    Add_Watchpoint_Ex( UI, Master_CPU, Component_Type_CPU, Register_Watchpoints,
        Master_CPU.CPU.Default_Base, ( Master_CPU.CPU.Register_Size( Address ) + 7 ) div 8,
        0, 0, False, Address, Access, nil, Master_CPU.Name ) ;
end ;


procedure TMain_Form.RegisterBreakpoints1Click( Sender : TObject ) ;

begin
    Show_Watchpoints( UI, Master_CPU, Register_Watchpoints, False ) ;
end ;


procedure TMain_Form.Edit1Click(Sender: TObject);

var P : TWatch ;

begin
    if( New_Watch_Dialog = nil ) then
    begin
        New_Watch_Dialog := TNew_Watch_Dialog.Create( self ) ;
    end ;
    P := TWatch( Watches[ Watch_List_Box.ItemIndex ] ) ;
    New_Watch_Dialog.Size.Value := P.Size ;
    New_Watch_Dialog.Minimum := Low_Address ;
    New_Watch_Dialog.Maximum := High_Address ;
    New_Watch_Dialog.Base := P.Base ;
    New_Watch_Dialog._Assembler := TComponent_Info( Master_CPU.Tag ).Assembler_Context ;
    case P.Base of
        2 : New_Watch_Dialog.Binary_RB.Checked := True ;
        8 : New_Watch_Dialog.Octal_RB.Checked := True ;
        10 : New_Watch_Dialog.Decimal_RB.Checked := True ;
        16 : New_Watch_Dialog.Hexadecimal_RB.Checked := True ;
        0 : New_Watch_Dialog.ASCII_RB.Checked := True ;
        1 : New_Watch_Dialog.EBCDIC_RB.Checked := True ;
        50 : New_Watch_Dialog.Radix50_RB.Checked := True ;
        -4 : New_Watch_Dialog.Single_RB.Checked := True ;
        -8 : New_Watch_Dialog.Double_RB.Checked := True ;
        -10 : New_Watch_Dialog.Extended_RB.Checked := True ;
        else
            begin
                New_Watch_Dialog.Other_RB.Checked := True ;
                New_Watch_Dialog.Base_Spin.Value := P.Base ;
            end ;
    end ;
    New_Watch_Dialog.Value := P.Address ;
    if( New_Watch_Dialog.ShowModal = mrOK ) then
    begin
        P.Address := New_Watch_Dialog.Value ;
        P.Size := New_Watch_Dialog.Size.Value ;
        if( New_Watch_Dialog.Binary_RB.Checked ) then
        begin
            P.Base := 2 ;
        end else
        if( New_Watch_Dialog.Octal_RB.Checked ) then
        begin
            P.Base := 8 ;
        end else
        if( New_Watch_Dialog.Decimal_RB.Checked ) then
        begin
            P.Base := 10 ;
        end else
        if( New_Watch_Dialog.Hexadecimal_RB.Checked ) then
        begin
            P.Base := 16 ;
        end else
        if( New_Watch_Dialog.Other_RB.Checked ) then
        begin
            P.Base := New_Watch_Dialog.Base_Spin.Value ;
        end else
        if( New_Watch_Dialog.ASCII_RB.Checked ) then
        begin
            P.Base := 0 ;
        end else
        if( New_Watch_Dialog.EBCDIC_RB.Checked ) then
        begin
            P.Base := 1 ;
        end else
        if( New_Watch_Dialog.Radix50_RB.Checked ) then
        begin
            P.Base := 50 ;
        end else
        if( New_Watch_Dialog.Single_RB.Checked ) then
        begin
            P.Base := -4 ;
        end else
        if( New_Watch_Dialog.Double_RB.Checked ) then
        begin
            P.Base := -8 ;
        end else
        if( New_Watch_Dialog.Extended_RB.Checked ) then
        begin
            P.Base := -10 ;
        end ;
        Update_Watches ;
    end ;
end ; // TMain_Form.Edit1Click


procedure TMain_Form.Delete1Click( Sender : TObject ) ;

begin
    if( Watch_List_Box.ItemIndex <> -1 ) then
    begin
        Watches.Delete( Watch_List_Box.ItemIndex ) ;
        Watch_List_Box.Items.Delete( Watch_List_Box.ItemIndex ) ;
        Update_Watches ;
    end ;
end ;


procedure TMain_Form.DeleteAll1Click(Sender: TObject);

begin
    if( MessageBox( 0, PChar( Text_Prompt_Delete_All_Watches ), 'CEF32', MB_YESNO ) = IDYES ) then
    begin
        Clear_Watches ;
        Update_Watches ;
    end ;
end ;


procedure TMain_Form.Watch_List_BoxMouseUp( Sender : TObject ;
    Button : TMouseButton ; Shift : TShiftState ; X, Y : Integer ) ;

var Index : integer ;

begin
    if( Button = mbRight ) then
    begin
        Index := Watch_List_Box.ItemAtPos( Point( X, Y ), True ) ;
        if( Index <> -1 ) then
        begin
            Watch_List_Box.ItemIndex := Index ;
        end ;
        Watches_Popup_Menu.Popup( Mouse.CursorPos.X, Mouse.CursorPos.Y ) ;
    end ;
end ;


procedure TMain_Form.Watch_List_BoxDblClick( Sender : TObject ) ;

begin
    if( Watch_List_Box.ItemIndex = -1 ) then
    begin
        AddWatch1Click( Sender ) ;
    end else
    begin
        Edit1Click( nil ) ;
    end ;
end ;


procedure TMain_Form.Watch_List_BoxKeyDown( Sender : TObject ; var Key : Word ;
    Shift : TShiftState ) ;

begin
    if( ( Key = VK_Delete ) and ( Watch_List_Box.ItemIndex <> -1 ) ) then
    begin
        Delete1Click( nil ) ;
    end ;
end ;


procedure TMain_Form.ExecutionBreakpoints1Click( Sender : TObject ) ;

begin
    if( Master_CPU <> nil ) then
    begin
        Show_Watchpoints( UI, Master_CPU, Execution_Watchpoints, False ) ;
    end ;
end ;


procedure TMain_Form.AddPortBreakpoint1Click( Sender : TObject ) ;

begin
    if( Master_CPU <> nil ) then
    begin
        Add_Breakpoint( UI, Master_CPU, Port_Watchpoints, Port_Tab.AD.Base,
            Port_Tab.AD.Number_Size, True ) ;
    end ;
end ;


procedure TMain_Form.Popup_MenuPopup( Sender : TObject ) ;

var T : TMemory_Tab ;

begin
    // Make sure memory tab popup reflects this tab's settings...
    T := Current_Tab ;
    case T.AD.Base of
        2 : Binary1.Checked := True ;
        8 : Octal1.Checked := True ;
        10 : Decimal1.Checked := True ;
        16 : Hexadecimal1.Checked := True ;
        50 :  Radix50.Checked := True ;
        else Other1.Checked := True ;
    end ;
    Signed1.Checked := not T.AD.Unsigned ;
    case T.AD.Number_Size of
        1 : Byte1.Checked := True ;
        2 : Word1.Checked := True ;
        4 : Long1.Checked := True ;
    end ;
    EBCDIC1.Checked := ( T.AD.Ascii_Ebcdic = 1 ) ;
end ;


procedure TMain_Form.PortBreakpoints1Click( Sender : TObject ) ;

begin
    if( Master_CPU <> nil ) then
    begin
        Show_Watchpoints( UI, nil, Port_Watchpoints, True ) ;
    end ;
end ;


procedure TMain_Form.Clear_Watches ;

var Loop : integer ;

begin
    for Loop := 0 to Watches.Count - 1  do
    begin
        TWatch( Watches[ Loop ] ).Free ;
    end ;
    Watches.Clear ;
    Update_Watches ;
end ;


procedure TMain_Form.Profiling1Click( Sender : TObject ) ;

begin
    Profiling1.Checked := not Profiling1.Checked ;
    Master_CPU.Set_Profiling( Profiling1.Checked, False ) ;
end ;


procedure TMain_Form.ProfileReport1Click(Sender: TObject);

begin
    Report_Profile( Master_CPU ) ;
end ;


procedure TMain_Form.TraceLog1Click( Sender : TObject ) ;

var Loop : integer ;
    S : string ;

begin
    Trace_Log_Form.List_Box.Clear ;
    for Loop := Trace_Position to Traces.Count - 1 do
    begin
        S := Traces[ Loop ] ;
        if( length( S ) > 0 ) then
        begin
            Trace_Log_Form.List_Box.Items.Add( S ) ;
        end ;
    end ;
    for Loop := 0 to Trace_Position -1 do
    begin
        Trace_Log_Form.List_Box.Items.Add( Traces[ Loop ] ) ;
    end ;
    Trace_Log_Form.ShowModal ;
end ;


procedure TMain_Form.Trace1Click( Sender : TObject ) ;

var CB_List : TList ;
    Y : integer ;

    procedure Configure_Component( Component : TComponent ) ;

    var CB : TCheckBox ;
        S : string ;

    begin
        CB := TCheckBox.Create( Trace_Form ) ;
        CB.Parent := Trace_Form.Main_Panel ;
        CB.Left := 8 ;
        CB.Width := 256 ;
        CB.Top := Y ;
        Y := Y + CB.Height + 4 ;
        S := string( Component.Name ) ;
        if(
            ( Component.Input_Component( 0 ) = nil )
            and
            ( Component.Output_Component( 0 ) = nil )
          ) then
        begin
            S := Substitute1( Text_No_Connections, S ) ;
        end ;
        CB.Caption := S ;
        CB.Checked := Component.Get_Trace ;
        CB_List.Add( CB ) ;
    end ;

var CB : TCheckBox ;
    Component : TComponent ;
    Loop : integer ;

begin
    if( Traces.Count > 0 ) then
    begin
        Trace_Form.Count.Value := Traces.Count ;
    end else
    begin
        Trace_Form.Count.Value := 10 ;
    end ;
    Delete_All_Children( Trace_Form.Main_Panel ) ;
    Y := 3 ;
    CB_List := TList.Create ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        Component := TComponent( Component_List.Objects[ Loop ] ) ;
        Configure_Component( Component ) ;
    end ;
    Configure_Component( Main_Memory ) ;

    if( Trace_Form.ShowModal = mrOK ) then
    begin
        if( Traces.Count <> Trace_Form.Count.Value ) then
        begin
            Traces.Clear ;
            while( Traces.Count < Trace_Form.Count.Value ) do
            begin
                Traces.Add( '' ) ;
            end ;
            Trace_Position := 0 ;
        end ;
        for Loop := 0 to Component_List.Count - 1 do
        begin
            Component := TComponent( Component_List.Objects[ Loop ] ) ;
            CB := TCheckBox( CB_List[ Loop ] ) ;
            Component.Set_Trace( CB.Checked ) ;
        end ;
        Main_Memory.Set_Trace( TCheckBox( CB_List[ CB_List.Count - 1 ] ).Checked ) ;
    end ;
    CB_List.Free ;
end ; // TMain_Form.Trace1Click



procedure TMain_Form.Disassembly_List_BoxMouseUp( Sender : TObject ;
    Button : TMouseButton ; Shift : TShiftState ; X, Y : Integer ) ;

var Point : TPoint ;

begin
    if( Button = mbRight ) then
    begin
        Point.X := X ;
        Point.Y := Y ;
        Point := Disassembly_List_Box.ClientToScreen( Point ) ;
        Disassembly_Popup.Popup( Point.X, Point.Y ) ;
    end ;
end ;


procedure TMain_Form.Disassembly_List_Resize( Sender : TObject ) ;

begin
    Update_Disassembly ;
end ;


procedure TMain_Form.Showsource1Click( Sender : TObject ) ;

begin
    Map_Source( True ) ;
end ;


procedure TMain_Form.GotoCurrent1Click( Sender : TObject ) ;

begin
    Disassembly_Top := -1 ;
    Update_Disassembly ;
    Disassembly_List_Box.ItemIndex := 0 ;
end ;


procedure TMain_Form.Gotoaddress2Click( Sender : TObject ) ;

begin
    if( Master_CPU <> nil ) then
    begin
        Goto_Address_Form.Base := Current_Tab.AD.Base ;
        Goto_Address_Form.ComboBox1.Text :=
            Cvtb( 10, Current_Tab.AD.Base, inttostr( Master_CPU.CPU.Get_Current_Address( 0, True ) ) ) ;
        if( Goto_Address_Form.ShowModal = mrOK ) then
        begin
            Disassembly_Top :=
                strtoint( Cvtb( Current_Tab.AD.Base, 10, Goto_Address_Form.ComboBox1.Text ) ) ;
            Update_Disassembly ;
        end ;
    end ;
end ;


procedure TMain_Form.TopofStack1Click(Sender: TObject);

begin
    Update_Stack ;
    Stack_List_Box.ItemIndex := 0 ;
end ;


procedure TMain_Form.Gotoaddress3Click(Sender: TObject);

begin
    if( Master_CPU <> nil ) then
    begin
        Goto_Address_Form.Base := Current_Tab.AD.Base ;
        Goto_Address_Form.ComboBox1.Text :=
            Cvtb( 10, Current_Tab.AD.Base, inttostr( Master_CPU.CPU.Top_Of_Stack( 0 ) ) ) ;
        if( Goto_Address_Form.ShowModal = mrOK ) then
        begin
            Stack_Top :=
                strtoint( Cvtb( Current_Tab.AD.Base, 10, Goto_Address_Form.ComboBox1.Text ) ) ;
            Update_Stack ;
        end ;
    end ;
end ;


procedure TMain_Form.Options2Click( Sender : TObject ) ;

begin
    // Save current options...
    Options_Form.Save ;

    // Show dialog
    if( Options_Form.ShowModal = mrCancel ) then
    begin
        // Restore original options...
        Options_Form.Restore ;
    end else
    begin
        UI._Clock.Mode := Options_Form.Clock_Mode.ItemIndex ;
        Disable1.Checked := not Options_Form.Clock_Enabled.Checked ;
    end ;
end ;



procedure TMain_Form.FormClose( Sender : TObject ; var Action : TCloseAction ) ;

var Editor : TEditor_Window ;
    Loop : integer ;
    F : textfile ;
    Path : string ;

    procedure Process_Boolean_Option( CB : TCheckBox ; Name : string ) ;

    var S : string ;

    begin
        if( CB.Checked ) then
        begin
            S := 'Yes' ;
        end else
        begin
            S := 'No' ;
        end ;
        {$I-}
        writeln( F, 'O ', Name, '=', S ) ;
        {$I+}
        IOResult ;
    end ;


    procedure Process_Integer_Option( Value : integer ; Name : string ) ;

    begin
       {$I-}
        writeln( F, 'O ', Name, '=', Value ) ;
        {$I+}
        IOResult ;
    end ;

begin
    // Add currently open files to reopen menus so they are added to config file...
    for Loop := 0 to PageControl1.PageCount - 1 do
    begin
        if( TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor <> nil ) then
        begin
            Editor := TCEF_TabSheet( PageControl1.Pages[ Loop ] ).Editor ;
            Add_To_Reopen( Editor.File_Name ) ;
        end ;
    end ;

    // Write configuration file...
    Path := paramstr( 0 ) ;
    Loop := length( Path ) ;
    while( ( Loop > 0 ) and ( pos( Path[ Loop ], ':\' ) = 0 ) ) do
    begin
        dec( Loop ) ;
    end ;
    Path := copy( Path, 1, Loop ) ;
    assignfile( F, Path + 'CEF.cfg' ) ;
    {$I-}
    rewrite( F ) ;
    {$I+}
    IOResult ;
    for Loop := 1 to 9 do
    begin
        if( Reopen_Menus[ Loop ].Visible ) then
        begin
            {$I-}
            writeln( F, 'F ' + Reopen_Menus[ Loop ].Caption ) ;
            {$I+}
            IOResult ;
        end ;
    end ;
    for Loop := 1 to 9 do
    begin
        if( EReopen_Menus[ Loop ].Visible ) then
        begin
            {$I-}
            writeln( F, 'E ' + EReopen_Menus[ Loop ].Caption ) ;
            {$I+}
            IOResult ;
        end ;
    end ;

    // Write options...
    Process_Boolean_Option( Options_Form.Generate_Listings, 'GENERATE_LISTINGS' ) ;
    Process_Boolean_Option( Options_Form.Immediate_Mode_Unblock, 'IMMEDIATE_MODE_UNBLOCK' ) ;
    Process_Boolean_Option( Options_Form.Clock_Enabled, 'CLOCK_ENABLED' ) ;
    Process_Boolean_Option( Options_Form.Generate_XRef_List, 'XREF_LIST' ) ;
    Process_Boolean_Option( Options_Form.Physical, 'PHYSICAL' ) ;
    Process_Boolean_Option( Options_Form.Generate_Symbol_Table, 'SYMBOL_TABLE_LIST' ) ;

    Process_Integer_Option( Options_Form.Max_Threads.Value, 'MAX_THREADS' ) ;

    Process_Integer_Option( Options_Form.Thread_Priority.ItemIndex, 'THREAD_PRIORITY' ) ;
    Process_Integer_Option( Options_Form.Clock_Mode.ItemIndex, 'CLOCK_MODE' ) ;

    {$I-}
    closefile( F ) ;
    {$I+}
    IOResult ;
end ; // TMain_Form.FormClose


procedure TMain_Form.Pattern1Click( Sender : TObject ) ;

var Address, I, Low, High, Work : int64 ;
    Loop, Size : integer ;
    S : string ;

begin
    if( Pattern_Dialog = nil ) then
    begin
        Pattern_Dialog := TPattern_Dialog.Create( Application ) ;
    end ;
    Pattern_Dialog.Set_Base( Current_Tab.AD.Base ) ;
    case Current_Tab.AD.Base of
        2 : Pattern_Dialog.Binary.Checked := True ;
        8 : Pattern_Dialog.Octal.Checked := True ;
        10 : Pattern_Dialog.Decimal.Checked := True ;
        16 : Pattern_Dialog.Hexadecimal.Checked := True ;
    end ;
    case Current_Tab.AD.Number_Size of
        1 : Pattern_Dialog.Size.ItemIndex := 0 ;
        2 : Pattern_Dialog.Size.ItemIndex := 1 ;
        4 : Pattern_Dialog.Size.ItemIndex := 2 ;
        8 : Pattern_Dialog.Size.ItemIndex := 3 ;
    end ;
    if( Pattern_Dialog.ShowModal = mrOK ) then
    begin
        Size := strtoint( Pattern_Dialog.Size.Text ) ;
        Pattern_Dialog.Get_Address_Range( Low, High ) ;
        S := Pattern_Dialog.Get_Value ;
        if( Pattern_Dialog.Increment_RB.Checked ) then
        begin
            S := copy( S, 1, Size ) ; // Get Starting value
            I := 0 ;
            move( PChar( S )[ 0 ], I, length( S ) ) ;
            Address := Low ;
            Work := I ;
            Loop := 1 ;
            while( Address <= High ) do
            begin
                Deposit_Memory( Address, Work and 255 ) ;
                Work := Work shr 8 ;
                inc( Loop ) ;
                if( Loop > Size ) then
                begin
                    Loop := 1 ;
                    inc( I ) ;
                    Work := I ;
                end ;
                inc( Address ) ;
            end ; // while( Address <= High )
        end else
        begin
            Loop := 1 ;
            Address := Low ;
            while( Address <= High ) do
            begin
                Deposit_Memory( Address, ord( S[ Loop ] )  ) ;
                inc( Address ) ;
                inc( Loop ) ;
                if( Loop > length( S ) ) then
                begin
                    Loop := 1 ;
                end ;
            end ;
        end ;
    end ;
    Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
end ; // TMain_Form.Pattern1Click


procedure TMain_Form.ShowErrorsClick( Sender : TObject ) ;

begin
    Assembly_Statistics.Show ;
end ;


procedure TMain_Form.Emulatorport1Click(Sender: TObject);

begin
    Choose_Emulator_Port_Form := TChoose_Emulator_Port_Form.Create( Application ) ;
    Choose_Emulator_Port_Form.Caption := Text_Caption_Emulator_Ports ;
    try
        Choose_Emulator_Port_Form.UI := UI ;
        Choose_Emulator_Port_Form.ShowModal ;
    finally
        Choose_Emulator_Port_Form.Free ;
        Choose_Emulator_Port_Form := nil ;
    end ;
end ;


procedure TMain_Form.Memory_ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer) ;

var Start : int64 ;
    Tab : TMemory_Tab ;

begin
    Tab := Current_Tab ;
    case ScrollCode of
        scEndScroll :
            begin
                PageControl2.Invalidate ;
                exit ;
            end ;
        scLineDown :
            begin
                Start := Tab.AD.Home_Index + Tab.AD.BPL ;
                if( Start > Tab.AI.High_Bound( 0 ) - Tab.AD.Max_Data * Tab.AD.LPP ) then
                begin
                    Start := Tab.AI.High_Bound( 0 ) - Tab.AD.Max_Data * Tab.AD.LPP ;
                end ;
                Tab.AD.Home_Index := Start ;
            end ;
        scLineUp :
            begin
                Start := Tab.AD.Home_Index - Tab.AD.BPL ;
                if( Start < Tab.AI.Low_Bound( 0 ) ) then
                begin
                    Start := Tab.AI.Low_Bound( 0 ) ;
                end ;
                Tab.AD.Home_Index := Start ;
            end ;
        scPageDown :
            begin
                Start := Tab.AD.Home_Index + Tab.AD.BPL * Tab.AD.LPP ;
                if( Start > Tab.AI.High_Bound( 0 ) - Tab.AD.Max_Data * Tab.AD.LPP ) then
                begin
                    Start := Tab.AI.High_Bound( 0 ) - Tab.AD.Max_Data * Tab.AD.LPP ;
                end ;
                Tab.AD.Home_Index := Start ;
            end ;
        scPageUp :
            begin
                Start := Tab.AD.Home_Index - Tab.AD.BPL * Tab.AD.LPP ;
                if( Start < Tab.AI.Low_Bound( 0 ) ) then
                begin
                    Start := Tab.AI.Low_Bound( 0 ) ;
                end ;
                Tab.AD.Home_Index := Start ;
            end ;
        scTop : Tab.AD.Home_Index := 0 ;
        scBottom : Tab.AD.Home_Index := Tab.AI.High_Bound( 0 ) - Tab.AD.BPL * Tab.AD.LPP ;
        scPosition, scTrack :
            begin
                Start := ScrollPos ;
                Start := ( ( Start + Tab.AI.Low_Bound( 0 ) ) * Tab.AD.BPL ) shl Range_Shift ;
                Tab.AD.Home_Index := Start ;
            end ;
    end ; // case ScrollCode of
    PageControl2.ActivePage.Controls[ 0 ].Invalidate ;
    ScrollPos := ( ( Tab.AD.Home_Index - Tab.AI.Low_Bound( 0 ) ) div Tab.AD.BPL ) shr Range_Shift ;
end ; // TMain_Form.Memory_ScrollBarScroll


procedure TMain_Form.Default1Click( Sender : TObject ) ;

begin
    if( UI._Clock <> nil ) then
    begin
        UI._Clock.Mode := MCM_Default ;
    end ;
end ;


procedure TMain_Form.Ignore1Click( Sender : TObject ) ;

begin
    if( UI._Clock <> nil ) then
    begin
        UI._Clock.Mode := MCM_Ignore ;
    end ;
end ;


procedure TMain_Form.Synchronize1Click( Sender : TObject ) ;

begin
    if( UI._Clock <> nil ) then
    begin
        UI._Clock.Mode := MCM_Synchronize ;
    end ;
end ;


procedure TMain_Form.GotoPortClick( Sender : TObject ) ;

begin
    Goto_Address_Form.Base := Port_Tab.AD.Base ;
    if( Goto_Address_Form.ShowModal = mrOK ) then
    begin
        Port_Tab.AD.Home_Index :=
            strtoint( Cvtb( Port_Tab.AD.Base, 10, Goto_Address_Form.ComboBox1.Text ) ) ;
        Pagecontrol2.ActivePage.Controls[ 0 ].Invalidate ;
    end ;
end ;


procedure TMain_Form.Read1Click( Sender : TObject ) ;

var C : TComponent ;
    Loop : integer ;
    R : TReciever_Component ;
    Success : boolean ;
    Value : int64 ;

begin // Input from port
    Port_IO_Dialog.Value.Visible := False ;
    Port_IO_Dialog.Label3.Visible := False ;
    Port_IO_Dialog.Base := Port_Tab.AD.Base ;
    if( Port_IO_Dialog.ShowModal <> mrOK ) then
    begin
        Port_IO_Dialog.Value.Visible := True ;
        Port_IO_Dialog.Label3.Visible := True ;
        exit ;
    end ;
    Port_IO_Dialog.Value.Visible := True ;
    Port_IO_Dialog.Label3.Visible := True ;
    Success := False ;
    Value := strtoint( CVTB( Port_Tab.AD.Base, 10, Port_IO_Dialog.Value.Text ) ) ;
    R := TReciever_Component.Create ;
    try
        for Loop := 0 to Component_List.Count - 1 do
        begin
            C := TComponent( Main_Form.Component_List.Objects[ Loop ] ) ;
            try
                C.Connect_Input( R ) ;
                C.Connect_Output( R ) ;
                if( C.Read( Value, Port_IO_Dialog.Size.Value, IO_Type_IO ) ) then
                begin
                    Success := True ;
                end ;
                C.Disconnect_Input( R ) ;
                C.Disconnect_Output( R ) ;
            except // Don't let faulty component end program
            end ;
        end ;
        if( not Success ) then
        begin
            ShowMessage( Text_Error_No_Components_Responded ) ;
        end else
        begin
            ShowMessage( Substitute1( Text_Response, CVTB( 10, Port_Tab.AD.Base, inttostr( R.Value ) ) ) ) ;
        end ;
    finally
        R.Free ;
    end ;
end ; // TMain_Form.Read1Click


procedure TMain_Form.Output1Click( Sender : TObject ) ;

var C : TComponent ;
    Loop : integer ;
    Success : boolean ;
    UEC : TUnified_Exception ;
    Value : int64 ;

begin // Output to port
    Port_IO_Dialog.Base := Port_Tab.AD.Base ;
    if( Port_IO_Dialog.ShowModal <> mrOK ) then
    begin
        exit ;
    end ;
    Success := False ;
    Value := strtoint( CVTB( Port_Tab.AD.Base, 10, Port_IO_Dialog.Value.Text ) ) ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        try
            C := TComponent( Main_Form.Component_List.Objects[ Loop ] ) ;
            UEC := C.Write( strtoint( CVTB( Port_Tab.AD.Base, 10, Port_IO_Dialog.Port.Text ) ),
                Value, Port_IO_Dialog.Size.Value, IO_Type_IO ) ;
            if( UEC = nil ) then
            begin
                Success := True ;
            end ;
        except // Don't let faulty component end program
        end ;
    end ;
    if( not Success ) then
    begin
        ShowMessage( Text_Error_No_Components_Responded ) ;
    end ;
end ;


procedure TMain_Form.MenuItem20Click( Sender : TObject ) ;

var C : TComponent ;
    Loop : integer ;
    Success : boolean ;
    UEC : TUnified_Exception ;
    Value : int64 ;

begin // Deposit to port
    Port_IO_Dialog.Base := Port_Tab.AD.Base ;
    if( Port_IO_Dialog.ShowModal <> mrOK ) then
    begin
        exit ;
    end ;
    Success := False ;
    Value := strtoint( CVTB( Port_Tab.AD.Base, 10, Port_IO_Dialog.Value.Text ) ) ;
    for Loop := 0 to Component_List.Count - 1 do
    begin
        C := TComponent( Main_Form.Component_List.Objects[ Loop ] ) ;
        try
            UEC := C.Deposit( strtoint( CVTB( Port_Tab.AD.Base, 10, Port_IO_Dialog.Port.Text ) ),
                Port_IO_Dialog.Size.Value, @Value, False ) ;
            if( UEC = nil ) then
            begin
                Success := True ;
            end ;
        except // Don't let faulty components end the program
        end ;
    end ;
    if( not Success ) then
    begin
        ShowMessage( Text_Error_No_Components_Responded ) ;
    end ;
end ;


procedure TMain_Form.Disable1Click(Sender: TObject);

begin
    Disable1.Checked := not Disable1.Checked ;
end ;


procedure TMain_Form.Unblockall1Click(Sender: TObject);

begin
    if( UI.Clock <> nil ) then
    begin
        UI._Clock.Unblock ;
    end ;
end ;


procedure TMain_Form.CEFSpecification1Click(Sender: TObject);

var S : string ;

begin
    S := Parse.Program_Path + 'Docs\spec_base.html' ;
    if( FileExists( S ) ) then
    begin
        OS.Spawn( S, '', '', 0 ) ;
        exit ;
    end ;
    OS.Spawn( 'http://cef.sourceforge.net/docs/spec_base.html', '', '', 0 ) ;
end ;


procedure TMain_Form.ComponentHelp1Click(Sender: TObject);

var S : string ;

begin
    S := Parse.Program_Path + 'Docs\cef32_doc.html' ;
    if( FileExists( S ) ) then
    begin
        OS.Spawn( S, '', '', 0 ) ;
        exit ;
    end ;
    OS.Spawn( 'http://cef.sourceforge.net/docs/cef32_doc.html', '', '', 0 ) ;
end ;


procedure TMain_Form.N24Click(Sender: TObject);

var S : string ;

begin
    S := Parse.Program_Path + 'Docs\spec_util.html' ;
    if( FileExists( S ) ) then
    begin
        OS.Spawn( S, '', '', 0 ) ;
        exit ;
    end ;
    OS.Spawn( 'http://cef.sourceforge.net/docs/spec_util.html', '', '', 0 ) ;
end ;


procedure TMain_Form.MediaManager1Click( Sender : TObject ) ;

begin
    Media_Manager.Showmodal ;
end ;


end.
