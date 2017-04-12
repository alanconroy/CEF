{
        Program Name : CCM_Add_Dlg
        Package Name : CEF32
        Purpose      : Add a component dialog
        Institution  :
        Date Written : 27-Apr-2000
        Written By   : Alan Conroy
        Version      : 1.0

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.
        
        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *    DATE        BY          REASON                         *
        *                                                           *
        *                                                           *
        *************************************************************

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          This dialog allows the user to add a component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CCM_Add_Dlg ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls ;

type
  TCCM_Add_Form = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    Cancel_Button: TBitBtn;
    Help_Button: TBitBtn;
    List_Box: TListBox;
    procedure List_BoxClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);

  private { Private declarations }

  public // API...
      procedure Setup( Components : TStringList ; LB : TListBox ) ;
  end ;

var CCM_Add_Form : TCCM_Add_Form ;

implementation

{$R *.dfm}

// TCCM_Add_Form methods...

// API...

procedure TCCM_Add_Form.Setup( Components : TStringList ; LB : TListBox ) ;

var Loop : integer ;

begin
    List_Box.Clear ;
    for Loop := 0 to Components.Count - 1 do
    begin
        if( LB.Items.IndexOf( Components[ Loop ] ) = -1 ) then
        begin
            List_Box.Items.Add( Components[ Loop ] ) ;
        end ;
    end ;
end ;


procedure TCCM_Add_Form.List_BoxClick( Sender : TObject ) ;

begin
    OK_Button.Enabled := ( List_Box.ItemIndex <> -1 ) ;
end ;


procedure TCCM_Add_Form.Help_ButtonClick(Sender: TObject);

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.

