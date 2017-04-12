unit TapePres_Add_Data;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Spin;

type
  TTape_Add_Data_Dialog = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Top_Panel: TPanel;
    Memo1: TMemo;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    Max_Bytes: TSpinEdit;
    Label3: TLabel;
    Max_Block_Size: TSpinEdit;
    Label4: TLabel;
    Min_Block_Size: TSpinEdit;
    procedure SpeedButton1Click(Sender: TObject);
    procedure Min_Block_SizeChange(Sender: TObject);
    procedure Max_Block_SizeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Tape_Add_Data_Dialog : TTape_Add_Data_Dialog ;

implementation

uses // CEF...
     TextStrs, // Text_*

     // C&C...
     UE ; // ERT

{$R *.dfm}

var _Tape_Add_Data_Dialog : TTape_Add_Data_Dialog = nil ;

function Tape_Add_Data_Dialog : TTape_Add_Data_Dialog ;

begin
    if( _Tape_Add_Data_Dialog = nil ) then
    begin
        _Tape_Add_Data_Dialog := TTape_Add_Data_Dialog.Create( Application ) ;
    end ;
    Result := _Tape_Add_Data_Dialog ;
end ;


procedure TTape_Add_Data_Dialog.SpeedButton1Click(Sender: TObject);

var Dummy : integer ;
    F : file ;
    S : string ;

begin
    if( OpenDialog1.Execute ) then
    begin
        assignfile( F, OpenDialog1.Filename ) ;
        {$I-}
        reset( F, 1 ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) + ' while opening ' + OpenDialog1.Filename ) ;
            exit ;
        end ;
        try
            setlength( S, filesize( F ) ) ;
            blockread( F, Pchar( S )[ 0 ], length( S ) ) ;
            Memo1.Text := S ;
            Max_Block_Size.Value := length( S ) ;
            Min_Block_Size.Value := length( S ) ;
            Max_Bytes.Value := length( S ) ;
        finally
            {$I-}
            closefile( F ) ;
            {$I+}
            IOResult ;
        end ;
    end ;
end ;


procedure TTape_Add_Data_Dialog.Min_Block_SizeChange(Sender: TObject);

begin
    if( Max_Block_Size.Value > 0 ) then
    begin
        if( Max_Block_Size.Value < Min_Block_Size.Value ) then
        begin
            Max_Block_Size.Value := Min_Block_Size.Value ;
        end ;
    end ;
end ;


procedure TTape_Add_Data_Dialog.Max_Block_SizeChange(Sender: TObject);

begin
    try
        if( Max_Block_Size.Value > 0 ) then
        begin
            if( Max_Block_Size.Value < Min_Block_Size.Value ) then
            begin
                Min_Block_Size.Value := Max_Block_Size.Value ;
            end ;
        end ;
    except
    end ;
end ;


procedure TTape_Add_Data_Dialog.FormShow(Sender: TObject);

begin
    Caption := Text_Caption_Add_Data_To_Tape ;
    BitBtn1.Caption := Text_Button_Caption_OK ;
    BitBtn2.Caption := Text_Button_Caption_Cancel ;
    SpeedButton1.Caption := Text_Button_Caption_Add_From_File ;
    Label1.Caption := Text_Enter_Data_To_Add ;
    Label2.Caption := Text_Max_Bytes ;
    Label3.Caption := Text_Max_Block_Size_In_Bytes ;
    Label4.Caption := Text_Min_Block_Size_In_Bytes ;
    OpenDialog1.Title := Text_Caption_Import_Data_From_File ;
end ;


end.
