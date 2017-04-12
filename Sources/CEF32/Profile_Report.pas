{$N+}
{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF Profile report form
        Institution  :
        Date Written : 9-Aug-2005
        Written By   : Alan Conroy
        Version      : 1.0

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

	  This form is used to show component profiling reports.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Profile_Report ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,

     // Other...
     TDialogs, // TDialog

     // CEF
     _CEF ; // TComponent

type
  TProfile_Report_Form = class( TDialog )
    Button_Panel: TPanel;
    Close_Button: TBitBtn;
    Help_Button: TBitBtn;
    PageControl1: TPageControl;
    Button_Clear: TBitBtn;
    Button_Clear_All: TBitBtn;
    procedure Button_ClearClick(Sender: TObject);
    procedure Button_Clear_AllClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private // Instance data...
      _Component : TComponent ;

  public // API...
      procedure Set_Component( Component : TComponent ) ;
  end ;

var Profile_Report_Form : TProfile_Report_Form ;

implementation

uses // C&C...
     Textstrs ; // Text_*

{$R *.dfm}

// TProfile_Report_Form methods...

procedure TProfile_Report_Form.Button_ClearClick( Sender : TObject ) ;

begin
    _Component.Profiler.Clear( PageControl1.ActivePageIndex ) ;
end ;


procedure TProfile_Report_Form.Button_Clear_AllClick( Sender : TObject ) ;

begin
    _Component.Profiler.Clear( -1 ) ;
end ;


// API...

procedure TProfile_Report_Form.Set_Component( Component : TComponent ) ;

var LB : TListBox ;
    Index, Loop : integer ;
    P : PChar ;
    Profiler : TProfiler ;
    Sheet : TTabSheet ;

begin
    // Setup...
    _Component := Component ;
    Profiler := Component.Profiler ;
    Caption := 'Profiling report for ' + string( Component.Name ) ;
    if( Profiler = nil ) then
    begin
        LB.Items.Add( 'No profiling information is available' ) ;
        exit ;
    end ;

    // Clear existing pages...
    for Loop := PageControl1.PageCount - 1 downto 0 do
    begin
        PageControl1.Pages[ Loop ].Free ;
    end ;

    // Build report...
    Loop := 0 ;
    P := Profiler.Domain_Name( Loop ) ;
    while( P <> nil ) do
    begin
        Sheet := TTabSheet.Create( PageControl1 ) ;
        Sheet.Parent := PageControl1 ;
        Sheet.Caption := string( P ) ;
        Sheet.PageControl := PageControl1 ;
        Sheet.Visible := True ;
        LB := TListBox.Create( PageControl1 ) ;
        LB.Parent := Sheet ;
        LB.Align := alClient ;
        Index := 0 ;
        P := Profiler.Report_Line( Loop, Index ) ;
        while( P <> nil ) do
        begin
            LB.Items.Add( string( P ) ) ;
            inc( Index ) ;
            P := Profiler.Report_Line( Loop, Index ) ;
        end ;

        inc( Loop ) ;
        P := Profiler.Domain_Name( Loop ) ;
    end ;

    // Leave first page as active page
    if( PageControl1.PageCount > 0 ) then
    begin
        PageControl1.ActivePage := PageControl1.Pages[ PageControl1.PageCount - 1 ] ;
        PageControl1.ActivePage := PageControl1.Pages[ 0 ] ;
    end ;
end ; // TProfile_Report_Form.Set_Component


procedure TProfile_Report_Form.Help_ButtonClick(Sender: TObject);

begin
    Application.HelpContext( HelpContext ) ;
end ;


procedure TProfile_Report_Form.FormShow(Sender: TObject);

begin
    Caption := Text_Caption_Profile_Report ;
    Close_Button.Caption := Text_Button_Caption_Close ;
    Help_Button.Caption := Text_Button_Caption_Help ;
    Button_Clear.Caption := Text_Button_Caption_Clear ;
    Button_Clear_All.Caption := Text_Button_Caption_Clear_All ;
end ;


end.

