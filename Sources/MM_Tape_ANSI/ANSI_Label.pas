{
        Program Name : ANSIPres
        Package Name : CEF32
        Purpose      : ANSI Tape Labeller
        Institution  : Conroy & Conroy Co.
        Date Written : 31-Oct-2009
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

          This form is used to add labels to ANSI tapes.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit ANSI_Label ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls;

type
  TANSI_Label_Form = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Pagecontrol: TPageControl;
    HDR: TTabSheet;
    HDR2: TTabSheet;
    EOF1: TTabSheet;
    EOF2: TTabSheet;
    VOL1: TTabSheet;
    EOV1: TTabSheet;
    EOV2: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label9: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label34: TLabel;
    Label33: TLabel;
    Label32: TLabel;
    Label30: TLabel;
    Label28: TLabel;
    VOL1_Volume_ID: TEdit;
    VOL1_Accessibility: TEdit;
    VOL1_Owner: TEdit;
    VOL1_Version: TEdit;
    HDR1_Filename: TEdit;
    HDR1_System: TEdit;
    HDR1_Block_Count: TEdit;
    HDR1_Accessibility: TEdit;
    HDR1_Expiration_Date: TEdit;
    HDR1_Creation_Date: TEdit;
    HDR1_Gen_Version: TEdit;
    HDR1_Gen_Number: TEdit;
    HDR1_Sequence: TEdit;
    HDR1_Section: TEdit;
    HDR1_File_Set: TEdit;
    EOF1_Filename: TEdit;
    EOF1_System: TEdit;
    EOF1_Block_Count: TEdit;
    EOF1_Accessibility: TEdit;
    EOF1_Expiration_Date: TEdit;
    EOF1_Creation_Date: TEdit;
    EOF1_Gen_Version: TEdit;
    EOF1_Gen_Number: TEdit;
    EOF1_Sequence: TEdit;
    EOF1_Section: TEdit;
    EOF1_File_Set: TEdit;
    EOV1_Filename: TEdit;
    EOV1_System: TEdit;
    EOV1_Block_Count: TEdit;
    EOV1_Accessibility: TEdit;
    EOV1_Expiration_Date: TEdit;
    EOV1_Creation_Date: TEdit;
    EOV1_Gen_Version: TEdit;
    EOV1_Gen_Number: TEdit;
    EOV1_Sequence: TEdit;
    EOV1_Section: TEdit;
    EOV1_File_Set: TEdit;
    HDR2_Format: TEdit;
    HDR2_Block_Length: TEdit;
    HDR2_Record_Length: TEdit;
    HDR2_Depends: TEdit;
    HDR2_Offset: TEdit;
    EOF2_Format: TEdit;
    EOF2_Block_Length: TEdit;
    EOF2_Record_Length: TEdit;
    EOF2_Depends: TEdit;
    EOF2_Offset: TEdit;
    EOV2_Format: TEdit;
    EOV2_Block_Length: TEdit;
    EOV2_Record_Length: TEdit;
    EOV2_Depends: TEdit;
    EOV2_Offset: TEdit;
    procedure Edit_KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ANSI_Label_Form : TANSI_Label_Form ;

implementation

{$R *.dfm}

var _ANSI_Label_Form : TANSI_Label_Form = nil ;

function ANSI_Label_Form : TANSI_Label_Form ;

begin
    if( _ANSI_Label_Form = nil ) then
    begin
        _ANSI_Label_Form := TANSI_Label_Form.Create( Application ) ;
    end ;
    Result := _ANSI_Label_Form ;
end ;


procedure TANSI_Label_Form.Edit_KeyPress(Sender: TObject;
    var Key: Char);

var E : TEdit ;

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    E := TEdit( Sender ) ;
    if( length( E.Text ) > E.Tag ) then
    begin
        Key := #0 ;
        exit ;
    end ;
end ;



end.
