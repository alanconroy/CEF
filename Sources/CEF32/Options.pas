{
        Program Name : Options
        Package Name : CEF32
        Purpose      : Options form for CEF32
        Institution  :
        Date Written : 19-Mar-2007
        Written By   : Alan Conroy
        Version      : 1.8

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

        DATE           BY          REASON
        ----           --          ------
        8-Mar-2008     EAC         Added Unlock_Immediate_Mode

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	  This form is the options dialog for CEF32.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Options ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls, Spin, // TSpinEdit
     ComCtrls;

type
  TOptions_Form = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    Cancel_Button: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Generate_Listings: TCheckBox;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Max_Threads: TSpinEdit;
    Label2: TLabel;
    Thread_Priority: TComboBox;
    Generate_Symbol_Table: TCheckBox;
    Generate_XRef_List: TCheckBox;
    TabSheet3: TTabSheet;
    Immediate_Mode_Unblock: TCheckBox;
    Clock_Enabled: TCheckBox;
    Label3: TLabel;
    Clock_Mode: TComboBox;
    Physical: TCheckBox;
    procedure FormShow(Sender: TObject);

  private // Instance data...
    _Generate_Listings : boolean ;
    _Max_Threads : integer ;
    _Thread_Priority : integer ;
    _Generate_Symbol_Table : boolean ;
    _Generate_XRef_List : boolean ;
    _Immediate_Mode_Unblock : boolean ;
    _Clock_Enabled : boolean ;
    _Clock_Mode : integer ;
    _Physical : boolean ;

  public // API...
      procedure Save ;
      procedure Restore ;
  end ;

var Options_Form: TOptions_Form = nil ;

implementation

uses // C&C...
     TextStrs ; // Text_*

{$R *.dfm}

// API...

procedure TOptions_Form.Save ;

begin
    _Generate_Listings := Generate_Listings.Checked ;
    _Max_Threads := Max_Threads.Value ;
    _Thread_Priority := Thread_Priority.ItemIndex ;
    _Generate_Symbol_Table := Generate_Symbol_Table.Checked ;
    _Generate_XRef_List := Generate_XRef_List.Checked ;
    _Immediate_Mode_Unblock := Immediate_Mode_Unblock.Checked ;
    _Clock_Enabled := Clock_Enabled.Checked ;
    _Clock_Mode := Clock_Mode.ItemIndex ;
    _Physical := Physical.Checked ;
end ;


procedure TOptions_Form.Restore ;

begin
    Generate_Listings.Checked := _Generate_Listings ;
    Max_Threads.Value := _Max_Threads ;
    Thread_Priority.ItemIndex := _Thread_Priority ;
    Generate_Symbol_Table.Checked := _Generate_Symbol_Table ;
    Generate_XRef_List.Checked := _Generate_XRef_List ;
    Physical.Checked := _Physical ;
    Immediate_Mode_Unblock.Checked := _Immediate_Mode_Unblock ;
    Clock_Enabled.Checked := _Clock_Enabled ;
    Clock_Mode.ItemIndex := _Clock_Mode ;
end ;


procedure TOptions_Form.FormShow(Sender: TObject);

begin
    Caption := Text_Caption_Options ;
    OK_Button.Caption := Text_Button_Caption_OK ;
    Cancel_Button.Caption := Text_Button_Caption_Cancel ;
    TabSheet1.Caption := Text_Caption_Assembler ;
    Generate_Listings.Caption := Text_Generate_List_Files ;
    Generate_Symbol_Table.Caption := Text_Generate_Symbol_Table ;
    Generate_XRef_List.Caption := Text_Generate_Cross_Reference_List ;
    Physical.Caption := Text_Assemble_Into_Physical_Address_Space ;
    TabSheet2.Caption := Text_Caption_Threading ;
    Label1.Caption := Text_Max_Threads ;
    Label2.Caption := Text_Thread_Priority ;
    Thread_Priority.Items.Add( Text_Idle_Only_Not_Recommended ) ;
    Thread_Priority.Items.Add( Text_Lower ) ;
    Thread_Priority.Items.Add( Text_Low ) ;
    Thread_Priority.Items.Add( Text_Normal ) ;
    Thread_Priority.Items.Add( Text_High ) ;
    Thread_Priority.Items.Add( Text_Higher ) ;
    Thread_Priority.Items.Add( Text_Real_Time_Not_Recommended ) ;
    Thread_Priority.Text := Text_Normal ;
    TabSheet3.Caption := Text_Caption_Master_Clock ;
    Label3.Caption := Text_Mode ;
    Immediate_Mode_Unblock.Caption := Text_Unblock_All_Components_After_Immediate_Mode_Execution ;
    Clock_Enabled.Caption := Text_Enabled ;
    Clock_Mode.Items.Clear ;
    Clock_Mode.Items.Add( Text_Default ) ;
    Clock_Mode.Items.Add( Text_Ignore ) ;
    Clock_Mode.Items.Add( Text_Synchronize ) ;
    Clock_Mode.Text := Text_Default ;
    Clock_Mode.ItemIndex := 0 ;
end ;


end.
