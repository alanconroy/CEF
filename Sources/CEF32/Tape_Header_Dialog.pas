{
        Program Name : Tape_Header_Dialog
        Package Name : CEF32
        Purpose      : CEF32 Tape virtual media file header dialog
        Institution  :
        Date Written : 27-Apr-2009
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

          This program is the dialog for showing/changing virtual tape
        media headers.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Tape_Header_Dialog ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Spin;

type
  TTape_Header = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Internal_Format: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Include_After: TCheckBox;
    Size_Record: TSpinEdit;
    BPI: TSpinEdit;
    Length: TSpinEdit;
    Format: TEdit;
    SpeedButton1: TSpeedButton;
    Label9: TLabel;
    Label10: TLabel;
    IRG_Length: TSpinEdit;
    TM_Length: TSpinEdit;
    Label11: TLabel;
    Label12: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Tape_Header : TTape_Header ;

implementation

uses // CEF32...
     TextStrs ; // Text_*

{$R *.dfm}

var _Tape_Header : TTape_Header = nil ;

function Tape_Header : TTape_Header ;

begin
    if( _Tape_Header = nil ) then
    begin
        _Tape_Header := TTape_Header.Create( Application ) ;
    end ;
    Result := _Tape_Header ;
end ;


procedure TTape_Header.SpeedButton1Click( Sender : TObject ) ;

begin
    Format.Text := 'DOS-11 Tape' ;
    BPI.Value := 0 ;
    Length.Value := 0 ;
    if( Internal_Format.Enabled ) then
    begin
        Size_Record.Value := 4 ;
        Include_After.Checked := True ;
        IRG_Length.Value := 750 ;
        TM_Length.Value := 7500 ;
    end ;
end ;


procedure TTape_Header.FormShow(Sender: TObject);

begin
    Caption := Text_Caption_Tape_Media_Information ;
    Label1.Caption := Text_Format_Colon ;
    Label2.Caption := Text_BPI ;
    Label3.Caption := Text_0_Undefined ;
    Label4.Caption := Text_Length_Colon ;
    Label5.Caption := Text_0_Infinite ;
    Label6.Caption := Text_Feet ;
    SpeedButton1.Caption := Text_Button_Caption_Defaults ;
    Label9.Caption := Text_IRG_Length_Colon ;
    Label10.Caption := Text_TM_Length_Colon ;
    Label11.Caption := Text_1_1000 ;
    Label12.Caption := Text_1_1000 ;
    BitBtn1.Caption := Text_Button_Caption_OK ;
    Internal_Format.Caption := Text_Internal_Format ;
    Label7.Caption := Text_Note_Changing_These_Settings_Affects_The_Internal_Media_File_Data ;
    Label8.Caption := Text_Size_Record_Length_Colon ;
    Include_After.Caption := Text_Include_Size_Records_Before_And_After_Records ;
end ;


end.
