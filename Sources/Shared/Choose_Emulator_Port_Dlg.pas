{
        Program Name : Choose_Emulator_Port_Dlg 
        Package Name : CEF
        Purpose      : Emulator port choice dialog
        Institution  : Conroy & Conroy Co.
        Date Written : 13-July-2005
        Written By   : Alan Conroy
        Version      : 1.0

	    Copyright (C) 2005-2015 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *    DATE        BY          REASON                         *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This form allows the choice of an emulator port.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Choose_Emulator_Port_Dlg ;

interface

uses
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls, Buttons,

     // CEF...
     _CEF ; // TCable

type
  TChoose_Emulator_Port_Form = class( TForm )
                                    Panel1: TPanel;
                                    OK_Button: TBitBtn;
                                    BitBtn2: TBitBtn;
                                    Panel2: TPanel;
                                    Splitter1: TSplitter;
                                    Panel3: TPanel;
                                    Label1: TLabel;
                                    Panel4: TPanel;
                                    Label2: TLabel;
                                    Splitter2: TSplitter;
                                    Panel5: TPanel;
                                    Label3: TLabel;
                                    ListBox1: TListBox;
                                    ListBox2: TListBox;
                                    ListBox3: TListBox;
                                    ScrollBar1: TScrollBar;
                                    procedure ListBox1DblClick(Sender: TObject);
                                    procedure ListBox1Click(Sender: TObject);
                                    procedure OK_ButtonClick(Sender: TObject);

                                   private // Instance data...
                                       _Component : TComponent ;
                                       _UI : TUI_Interface ;

                                   private // Property handlers...
                                       procedure Set_Component( Value : TComponent ) ;
                                       procedure Set_UI( UI : TUI_Interface ) ;

                                   public // API...
                                       Cable : TComponent ;

                                       property UI : TUI_Interface
                                           write Set_UI ;

                                       property Component : TComponent
                                           write Set_Component ;
                               end ; // TChoose_Emulator_Port_Form

var Choose_Emulator_Port_Form : TChoose_Emulator_Port_Form ;

implementation

{$R *.dfm}

procedure TChoose_Emulator_Port_Form.Set_Component( Value : TComponent ) ;

begin
    _Component := Value ;
    Panel2.Visible := ( Value = nil ) ;
end ;


procedure TChoose_Emulator_Port_Form.Set_UI( UI : TUI_Interface ) ;

var C : TComponent ;
    Loop : integer ;
    Description, Name : string ;

begin
    _UI := UI ;
    ListBox1.Items.Clear ;
    ListBox2.Items.Clear ;
    ListBox3.Items.Clear ;
    Loop := 0 ;
    if( _Component <> nil ) then
    begin
        C := _Component.Get_Port( Loop ) ;
        while( C <> nil ) do
        begin
            Description := string( _Component.Get_Port_Description( Loop ) ) ;
            Name := string( _Component.Get_Port_Name( Loop ) ) ;
            if( Description = Name ) then
            begin
                Description := '' ;
            end ;
            if( Description <> '' ) then
            begin
                Name := Name + ' (' + Description + ')' ;
            end ;
            ListBox2.Items.Add( Name ) ;
            C := _Component.Get_Port_Connection( Loop ) ;
            if( C <> nil ) then
            begin
                ListBox3.Items.Add( C.Name ) ;
            end else
            begin
                ListBox3.Items.Add( '' ) ;
            end ;

            inc( Loop ) ;
            C := _Component.Get_Port( Loop ) ;
        end ;
        exit ;
    end ;

    C := UI.Get_Port( Loop ) ;
    while( C <> nil ) do
    begin
        ListBox1.Items.Add( UI.Port_Parent_Component( Loop ).Name ) ;
        Name := string( UI.Get_Port_Name( Loop ) ) + ' (' + string( UI.Get_Port_Description( Loop ) ) + ')' ;
        if( copy( Name, length( Name ) - 2, 3 ) = ' ()' ) then
        begin
            Name := copy( Name, 1, length( Name ) - 3 ) ;
        end ;
        ListBox2.Items.Add( Name ) ;
        C := UI.Get_Port_Connection( Loop ) ;
        if( C <> nil ) then
        begin
            ListBox3.Items.Add( C.Name ) ;
        end else
        begin
            ListBox3.Items.Add( '' ) ;
        end ;

        inc( Loop ) ;
        C := UI.Get_Port( Loop ) ;
    end ;
end ; // TChoose_Emulator_Port_Form.Set_UI


procedure TChoose_Emulator_Port_Form.ListBox1DblClick(Sender: TObject);

begin
    if( OK_Button.Enabled ) then
    begin
        OK_Button.Click ;
    end ;
end ;


procedure TChoose_Emulator_Port_Form.ListBox1Click( Sender : TObject ) ;

var Index : integer ;

begin
    LockWindowUpdate( Handle ) ;
    Index := TListBox( Sender ).ItemIndex ;
    ListBox1.ItemIndex := Index ;
    ListBox2.ItemIndex := Index ;
    ListBox3.ItemIndex := Index ;
    LockWindowUpdate( 0 ) ;
    OK_Button.Enabled := ( Index >= 0 ) and ( ListBox3.Items[ Index ] = '' ) ;
end ;


procedure TChoose_Emulator_Port_Form.OK_ButtonClick( Sender : TObject ) ;

begin
    Cable := _UI.Get_Port( ListBox1.ItemIndex ) ;
end ;


end.
