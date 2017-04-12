{
        Program Name : Error_List
        Package Name : CEF32
        Purpose      : Error listing dialog.
        Institution  : Conroy & Conroy Co.
        Date Written : 31-Dec-2006
        Written By   : Alan Conroy
        Version      : 1.0

	Released to the public domain.

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

          This form is used to show errors form assembly.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Error_List ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls, Buttons,

     // C&C...
     CommonUt, // TInteger_List
     TDialogs ; // TDialog

type
  TError_Dialog = class( TDialog )
    Button_Panel: TPanel;
    List_Box: TListBox;
    Abort_Button: TBitBtn;
    procedure List_BoxDblClick(Sender: TObject);

  private
    { Private declarations }

  public // API...
      Filenames : TStringList ;
      Lines : TInteger_List ;

      constructor Create( AOwner : TComponent ) ; override ;
      destructor Destroy ; override ;
  end;

var Error_Dialog : TError_Dialog ;

implementation

uses CEFMain;

{$R *.dfm}

// TError_Dialog methods...

procedure TError_Dialog.List_BoxDblClick(Sender: TObject);

begin
    Main_Form.Show_Error( List_Box ) ;
end ;


constructor TError_Dialog.Create( AOwner : TComponent ) ;

begin
    inherited Create( AOwner ) ;

    Filenames := TStringList.Create ;
    Lines := TInteger_List.Create ;
end ;


destructor TError_Dialog.Destroy ;

begin
    Filenames.Free ;
    Filenames := nil ;
    Lines.Free ;
    Lines := nil ;

    inherited Destroy ;
end ;



end.
