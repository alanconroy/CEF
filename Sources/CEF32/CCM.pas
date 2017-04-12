{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF Component Connection Manager
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

          This dialog is used to manager interconnections between components.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CCM ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, StdCtrls, Buttons;

type
  TComponent_Connection_Manager = class(TForm)
    Button_Panel: TPanel ;
    Component_LB: TListBox;
    Label1: TLabel;
    Panel1: TPanel;
    Input_LB: TListBox;
    Label2: TLabel;
    Output_LB: TListBox;
    Label3: TLabel;
    OK_Button: TBitBtn;
    Help_Button: TBitBtn;
    Delete_Input: TSpeedButton;
    Add_Input: TSpeedButton;
    Delete_Output: TSpeedButton;
    Add_Output: TSpeedButton;
    procedure Component_LBClick(Sender: TObject);
    procedure Input_LBKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Output_LBKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Delete_InputClick(Sender: TObject);
    procedure Delete_OutputClick(Sender: TObject);
    procedure Input_LBClick(Sender: TObject);
    procedure Output_LBClick(Sender: TObject);
    procedure Add_InputClick(Sender: TObject);
    procedure Add_OutputClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);

  private // Instance data...
      _Components : TStringList ;

  protected // Property handlers...
      procedure Set_Components( Value : TStringList ) ;

  public // API...
      procedure Add_Connection( LB : TListBox ) ;

      procedure Delete_Connection( LB : TListBox ) ;

      procedure Set_Component( const Name : string ) ;

      property Components : TStringList
          read _Components
          write Set_Components ;
  end ;

var Component_Connection_Manager : TComponent_Connection_Manager ;

implementation

uses // CEF...
     _CEF, // TComponent
     CCM_Add_Dlg ;

{$R *.dfm}

// TComponent_Connection_Manager methods...

procedure TComponent_Connection_Manager.Component_LBClick( Sender : TObject ) ;

var Component, Child : TComponent ;
    Index : integer ;

begin
    if( Component_LB.ItemIndex <> -1 ) then
    begin
        // Setup...
        Input_LB.Clear ;
        Output_LB.Clear ;
        Component := TComponent( Components.Objects[ Component_LB.ItemIndex ] ) ;

        // Load inputs...
        Index := 0 ;
        Child := Component.Input_Component( Index ) ;
        while( Child <> nil ) do
        begin
            Input_LB.Items.Add( Child.Name ) ;
            inc( Index ) ;
            Child := Component.Input_Component( Index ) ;
        end ;
        Delete_Input.Enabled := ( Input_LB.Items.Count > 0 ) ;
        if( Delete_Input.Enabled ) then
        begin
            Input_LB.ItemIndex := 0 ;
        end ;

        // Load outputs...
        Index := 0 ;
        Child := Component.Output_Component( Index ) ;
        while( Child <> nil ) do
        begin
            Output_LB.Items.Add( Child.Name ) ;
            inc( Index ) ;
            Child := Component.Output_Component( Index ) ;
        end ;
        Delete_Output.Enabled := ( Output_LB.Items.Count > 0 ) ;
        if( Delete_Output.Enabled ) then
        begin
            Output_LB.ItemIndex := 0 ;
        end ;
    end ; // if( Component_LB.ItemIndex <> -1 )
end ; // TComponent_Connection_Manager.Component_LBClick


procedure TComponent_Connection_Manager.Input_LBKeyUp( Sender : TObject ;
    var Key : Word ; Shift : TShiftState ) ;

begin
    if( ( Key = VK_Delete ) and ( Input_LB.ItemIndex <> -1 ) ) then
    begin
        Delete_Connection( Input_LB ) ;
        Key := 0 ;
    end ;
end ;



procedure TComponent_Connection_Manager.Output_LBKeyUp( Sender : TObject ;
    var Key : Word ; Shift : TShiftState ) ;

begin
    if( ( Key = VK_Delete ) and ( Output_LB.ItemIndex <> -1 ) ) then
    begin
        Delete_Connection( Output_LB ) ;
        Key := 0 ;
    end ;
end ;


// Property handlers...

procedure TComponent_Connection_Manager.Set_Components( Value : TStringList ) ;

var Loop : integer ;

begin
    _Components := Value ;
    Component_LB.Clear ;
    Input_LB.Clear ;
    Output_LB.Clear ;
    if( Value <> nil ) then
    begin
        for Loop := 0 to Value.Count - 1 do
        begin
            Component_LB.Items.Add( Value[ Loop ] ) ;
        end ;
        Component_LB.ItemIndex := 0 ;
        Component_LBClick( nil ) ;
    end ;
end ;


// API...

procedure TComponent_Connection_Manager.Add_Connection( LB : TListBox ) ;

var Component, Child : TComponent ;
    Index, Loop : integer ;

begin
    Component := TComponent( Components.Objects[ Component_LB.ItemIndex ] ) ;
    CCM_Add_Form.Setup( Components, LB ) ;
    if( CCM_Add_Form.ShowModal = mrOK ) then
    begin
        for Loop := 0 to CCM_Add_Form.List_Box.Items.Count - 1 do
        begin
            if( CCM_Add_Form.List_Box.Selected[ Loop ] ) then
            begin
                Index := Components.IndexOf( CCM_Add_Form.List_Box.Items[ Loop ] ) ;
                Child := TComponent( Components.Objects[ Index ] ) ;
                if( LB = Input_LB ) then
                begin
                    Component.Connect_Input( Child ) ;
                end else
                begin
                    Component.Connect_Output( Child ) ;
                end ;
            end ;
        end ; // for Loop := 0 to CCM_Add_Form.List_Box.Items.Count - 1
        Component_LBClick( nil ) ; // Rebuild lists
    end ; // if( CCM_Add_Form.ShowModal = mrOK )
end ; // TComponent_Connection_Manager.Add_Connection


procedure TComponent_Connection_Manager.Delete_Connection( LB : TListBox ) ;

var Component, Child : TComponent ;

begin
    if( ( LB.ItemIndex <> -1 ) and ( Component_LB.ItemIndex <> -1 ) ) then
    begin
        Component := TComponent( Components.Objects[ Component_LB.ItemIndex ] ) ;
        if( LB = Input_LB ) then
        begin
            Child := Component.Input_Component( LB.ItemIndex ) ;
            Component.Disconnect_Input( Child ) ;
        end else
        begin
            Child := Component.Output_Component( LB.ItemIndex ) ;
            Component.Disconnect_Output( Child ) ;
        end ;
        Component_LBClick( nil ) ; // Rebuild lists
    end ;
end ;


procedure TComponent_Connection_Manager.Set_Component( const Name : string ) ;

begin
    Component_LB.ItemIndex := Component_LB.Items.IndexOf( Name ) ;
    Component_LBClick( nil ) ;
end ;


procedure TComponent_Connection_Manager.Delete_InputClick( Sender : TObject ) ;

begin
    Delete_Connection( Input_LB ) ;
end ;


procedure TComponent_Connection_Manager.Delete_OutputClick( Sender : TObject ) ;

begin
    Delete_Connection( Output_LB ) ;
end ;


procedure TComponent_Connection_Manager.Input_LBClick( Sender : TObject ) ;

begin
    Delete_Input.Enabled := ( Input_LB.ItemIndex <> -1 ) ;
end ;


procedure TComponent_Connection_Manager.Output_LBClick( Sender : TObject ) ;

begin
    Delete_Output.Enabled := ( Output_LB.ItemIndex <> -1 ) ;
end ;


procedure TComponent_Connection_Manager.Add_InputClick( Sender : TObject ) ;

begin
    Add_Connection( Input_LB ) ;
end ;


procedure TComponent_Connection_Manager.Add_OutputClick( Sender : TObject ) ;

begin
    Add_Connection( Output_LB ) ;
end ;


procedure TComponent_Connection_Manager.Help_ButtonClick(Sender: TObject);

begin
    Application.HelpContext( HelpContext ) ;
end ;


end.

