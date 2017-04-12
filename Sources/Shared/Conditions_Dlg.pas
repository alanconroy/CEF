{
        Program Name : Conditions_Dlg
        Package Name : CEF
        Purpose      : CEF32 conditions dialog
        Institution  :
        Date Written : 27-Apr-2005
        Written By   : Alan Conroy
        Version      : 1.1

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

          This form is used to maintain condition handling for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Conditions_Dlg ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls,
     Menus, // TMenuItem

     // CEF...
     _CEF, //TCEF_Assembler_Context
     CEF, // TComponent
     CEF_Assembler, // TAssembler_Context

     // Other...
     CommonUt, // TInteger_List
     VCL_Std ; // Delete_All_Children

type TError_Array = array[ 0..3 ] of boolean ;

type
  TConditions_Form = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    Cancel_Button: TBitBtn;
    Help_Button: TBitBtn;
    PageControl1: TPageControl;
    States_Sheet: TTabSheet;
    Signals_Sheet: TTabSheet;
    Exceptions_Sheet: TTabSheet;
    Errors_Sheet: TTabSheet;
    CB_Hints: TCheckBox;
    CB_Warnings: TCheckBox;
    CB_Errors: TCheckBox;
    procedure CB_HintsClick(Sender: TObject);
    procedure CB_WarningsClick(Sender: TObject);
    procedure CB_ErrorsClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    
  private // Instance data...
      _Component : TComponent ;

  private // Internal utility routines...
      procedure CB_States_Click( Sender : TObject ) ;
      procedure CB_Signals_Click( Sender : TObject ) ;
      procedure CB_Exceptions_Click( Sender : TObject ) ;

  protected // Property handlers...
      procedure Set_Component( Value : TComponent ) ;

  public // API...
      property Component : TComponent
          read _Component
          write Set_Component ;
  end ;


type TComponent_Info = class
                           public // Constructors and destructors...
                               constructor Create ;
                               destructor Destroy ; override ;

                           public // Instance data...
                               DLL_Handle : THandle ;
                               Menu : TMenuItem ;
                               Ignore_Errors : TError_Array ;
                               State_Changes : TInteger_List ; // Indexes of states to break on if changed
                               Signal_Changes : TInteger_List ; // Indexes of signals to break on if changed
                               Signals_Set : TInteger_List ; // List of signals set via CEF32 dialog:
                                                             //    Index is sungal index
                                                             //    Value is bit mask:
                                                             //        and 1 = This has been set by user
                                                             //        and 2 = 0 : Signal low
                                                             //                2 : Signal high
                               Exceptions : TInteger_List ;
                               Assembler_Context : TCEF_Assembler_Context ;

                           public // API...
                               procedure Set_Signal( Index : integer ;
                                   Valid, State : boolean ) ;
                               function Signal_Set( Index : integer ) : boolean ;
                               function Signal_Valid( Index : integer ) : boolean ;
                       end ;

var Conditions_Form : TConditions_Form ;

implementation

{$R *.dfm}

uses // CEF32...
     L10n, // Substitute1
     TextStrs ; // Substitute1

// TComponent_Info methods...

// Constructors and destructors...

constructor TComponent_Info.Create ;

begin
    inherited Create ;

    State_Changes := TInteger_List.Create ;
    Signal_Changes := TInteger_List.Create ;
    Exceptions := TInteger_List.Create ;
    Signals_Set := TInteger_List.Create ;
end ;


destructor TComponent_Info.Destroy ;

begin
    State_Changes.Free ;
    Signal_Changes.Free ;
    Exceptions.Free ;
    Signals_Set.Free ;
    State_Changes := nil ;
    Signal_Changes := nil ;
    Exceptions := nil ;
    Signals_Set := nil ;

    inherited Destroy ;
end ;


// API...

procedure TComponent_Info.Set_Signal( Index : integer ; Valid, State : boolean ) ;

var V : integer ;

begin
    while( Index >= Signals_Set.Count ) do
    begin
        Signals_Set.Add( 0 ) ;
    end ;
    V := 1 ; // Valid
    if( State ) then
    begin
        V := V or 2 ;
    end ;
    Signals_Set[ Index ] := V ;
end ;


function TComponent_Info.Signal_Set( Index : integer ) : boolean ;

begin
    if( Index >= Signals_Set.Count ) then
    begin
        Result := False ;
        exit ;
    end ;
    Result := ( ( Signals_Set[ Index ] and 2 ) <> 0 ) ;
end ;


function TComponent_Info.Signal_Valid( Index : integer ) : boolean ;

begin
    if( Index >= Signals_Set.Count ) then
    begin
        Result := False ;
        exit ;
    end ;
    Result := ( ( Signals_Set[ Index ] and 1 ) <> 0 ) ;
end ;



// TConditions_Form methods...

// Property handlers...

procedure TConditions_Form.Set_Component( Value : TComponent ) ;

var CB : TCheckbox ;
    Index : integer ;
    P : PChar ;
    Y : integer ;

begin
    _Component := Value ;

    Delete_All_Children( States_Sheet ) ;
    Delete_All_Children( Signals_Sheet ) ;
    Delete_All_Children( Exceptions_Sheet ) ;

    if( _Component <> nil ) then
    begin
        // Load States...
        Y := 4 ;
        Index := 0 ;
        P := Component.Get_State_Name( Index ) ;
        while( P <> nil ) do
        begin
            CB := TCheckbox.Create( self ) ;
            CB.Parent := States_Sheet ;
            CB.Top := Y ;
            CB.Left := 8 ;
            Y := Y + CB.Height + 4 ;
            CB.Caption := Substitute1( Text_Break_On, string( P ) ) ;
            CB.Width := ClientWidth * 9 div 10 ;
            CB.Tag := Index ;
            CB.Checked := ( TComponent_Info( Component.Tag ).State_Changes.Indexof( Index ) >= 0 ) ;
            CB.OnClick := CB_States_Click ;
            inc( Index ) ;
            P := Component.Get_State_Name( Index ) ;
        end ;

        // Load signals...
        Y := 4 ;
        Index := 0 ;
        P := Component.Signal_Name( Index ) ;
        while( P <> nil ) do
        begin
            CB := TCheckbox.Create( self ) ;
            CB.Parent := Signals_Sheet ;
            CB.Top := Y ;
            CB.Left := 8 ;
            Y := Y + CB.Height + 4 ;
            CB.Caption := Substitute1( Text_Break_On, string( P ) ) ;
            CB.Width := ClientWidth * 9 div 10 ;
            CB.Tag := Index ;
            CB.Checked := ( TComponent_Info( Component.Tag ).Signal_Changes.Indexof( Index ) >= 0 ) ;
            CB.OnClick := CB_Signals_Click ;
            inc( Index ) ;
            P := Component.Signal_Name( Index ) ;
        end ;

        // Load exceptions...
        Y := 4 ;
        Index := 0 ;
        P := Component.Get_Exception_Description( Index ) ;
        while( P <> nil ) do
        begin
            CB := TCheckbox.Create( self ) ;
            CB.Parent := Exceptions_Sheet ;
            CB.Top := Y ;
            CB.Left := 8 ;
            Y := Y + CB.Height + 4 ;
            CB.Caption := Substitute1( Text_Break_On, string( P ) ) ;
            CB.Width := ClientWidth * 9 div 10 ;
            CB.Tag := Index ;
            CB.Checked := ( TComponent_Info( Component.Tag ).Exceptions.Indexof( Index ) >= 0 ) ;
            CB.OnClick := CB_Exceptions_Click ;
            inc( Index ) ;
            P := Component.Get_Exception_Description( Index ) ;
        end ;
    end ; // if( _Component <> nil )

    // Setup errors sheet...
    CB_Hints.Checked := TComponent_Info( Component.Tag ).Ignore_Errors[ 0 ] ;
    CB_Warnings.Checked := TComponent_Info( Component.Tag ).Ignore_Errors[ 1 ] ;
    CB_Errors.Checked := TComponent_Info( Component.Tag ).Ignore_Errors[ 2 ] ;
end ; // TCOnditions_Form.Set_Component



procedure TConditions_Form.CB_HintsClick(Sender: TObject);

begin
    TComponent_Info( Component.Tag ).Ignore_Errors[ 0 ] := CB_Hints.Checked ;
end ;


procedure TConditions_Form.CB_WarningsClick(Sender: TObject);

begin
    TComponent_Info( Component.Tag ).Ignore_Errors[ 1 ] := CB_Warnings.Checked ;
end ;


procedure TConditions_Form.CB_ErrorsClick(Sender: TObject);

begin
    TComponent_Info( Component.Tag ).Ignore_Errors[ 2 ] := CB_Errors.Checked ;
end ;


procedure TConditions_Form.CB_States_Click( Sender : TObject ) ;

var CB : TCheckBox ;

begin
    CB := TChecKBox( Sender ) ;
    if( CB.Checked ) then
    begin
        if( TComponent_Info( Component.Tag ).State_Changes.IndexOf( CB.Tag ) <> -1 ) then
        begin
            TComponent_Info( Component.Tag ).State_Changes.Add( CB.Tag ) ;
        end ;
    end else
    begin
        TComponent_Info( Component.Tag ).State_Changes.Remove( CB.Tag ) ;
    end ;
end ;


procedure TConditions_Form.CB_Signals_Click( Sender : TObject ) ;

var CB : TCheckBox ;

begin
    CB := TChecKBox( Sender ) ;
    if( CB.Checked ) then
    begin
        if( TComponent_Info( Component.Tag ).Signal_Changes.IndexOf( CB.Tag ) <> -1 ) then
        begin
            TComponent_Info( Component.Tag ).Signal_Changes.Add( CB.Tag ) ;
        end ;
    end else
    begin
        TComponent_Info( Component.Tag ).Signal_Changes.Remove( CB.Tag ) ;
    end ;
end ;


procedure TConditions_Form.CB_Exceptions_Click( Sender : TObject ) ;

var CB : TCheckBox ;

begin
    CB := TChecKBox( Sender ) ;
    if( CB.Checked ) then
    begin
        if( TComponent_Info( Component.Tag ).Exceptions.IndexOf( CB.Tag ) <> -1 ) then
        begin
            TComponent_Info( Component.Tag ).Exceptions.Add( CB.Tag ) ;
        end ;
    end else
    begin
        TComponent_Info( Component.Tag ).Exceptions.Remove( CB.Tag ) ;
    end ;
end ;


procedure TConditions_Form.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



procedure TConditions_Form.FormShow(Sender: TObject) ;

begin
    Caption := Text_Caption_Conditions ;
    OK_Button.Caption := Text_Button_Caption_OK_amp ;
    Cancel_Button.Caption := Text_Button_Caption_Cancel_amp ;
    Help_Button.Caption := Text_Button_Caption_Help_amp ;
    States_Sheet.Caption := Text_Caption_States ;
    Signals_Sheet.Caption := Text_Caption_Signals ;
    Exceptions_Sheet.Caption := Text_Caption_Exceptions ;
    Errors_Sheet.Caption := Text_Caption_Errors ;
    CB_Hints.Caption := Text_Caption_Break_On_Informational_Messagaes ;
    CB_Warnings.Caption := Text_Caption_Break_On_Warnings ;
    CB_Errors.Caption := Text_Caption_Break_On_Errors ;
end ;


end.

