{
        Program Name : Printer_Options_Form 
        Package Name : CEF
        Purpose      : Printer Options dialog for LA36 printer terminal
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

	    Copyright (C) 2006-2013 by Alan Conroy.  Released to the public domain.

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

          This is the UI interface to the LA36 display options.


        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Printer_Options_Form ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Spin, ComCtrls;

type
  TPrinter_Options_Dialog = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    BitBtn2: TBitBtn;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Max_Pages: TSpinEdit;
    Label9: TLabel;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Width: TSpinEdit;
    Height: TSpinEdit;
    Height_Modification: TSpinEdit;
    Width_Modification: TSpinEdit;
    GroupBox3: TGroupBox;
    Plain_RB: TRadioButton;
    Barred_RB: TRadioButton;
    Green_Panel: TPanel;
    Green_Image: TImage;
    Blue_Panel: TPanel;
    Blue_Image: TImage;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    Browse_Button: TSpeedButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Windows_Font: TComboBox;
    Filename: TEdit;
    Label1: TLabel;
    Margin: TSpinEdit;
    Label5: TLabel;
    Bleed: TSpinEdit;
    Label10: TLabel;
    GroupBox4: TGroupBox;
    Alternate_Browse_Button: TSpeedButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Alternate_Windows_Font: TComboBox;
    Alternate_Filename: TEdit;
    Label11: TLabel;
    Magnification: TSpinEdit;
    procedure Alternate_Browse_ButtonClick(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure Blue_ImageClick(Sender: TObject);
    procedure Green_ImageClick(Sender: TObject);
    procedure HeightChange(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Browse_ButtonClick(Sender: TObject);
  private
    Font_List : TStringList ;

    function Get_Bar_Color : TColor ;
    procedure Set_Bar_Color( Value : TColor ) ;

  public
    Alt_Current_Font, Current_Font : string ;

    property Bar_Color : TColor
        read Get_Bar_Color
        write Set_Bar_Color ;
  end;

var Printer_Options_Dialog : TPrinter_Options_Dialog ;

implementation

{$R *.dfm}

uses // LB...
     O_S ; // OS

function CB_FontEnum( LogFont : pLogFont ; TextMetric : pTextMetric ;
    Typ, Context : integer ) : integer ; far ; stdcall ;


begin
    Printer_Options_Dialog.Font_List.Add( LogFont^.lfFaceName ) ;
    Result := -1 ; // Don't interrupt the iteration
end ;


procedure TPrinter_Options_Dialog.Set_Bar_Color( Value : TColor ) ;

begin
    if( Value = Green_Image.Picture.Bitmap.Canvas.Pixels[ 0, 0 ] ) then
    begin
        Green_Panel.BevelOuter := bvLowered ;
        Blue_Panel.BevelOuter := bvRaised ;
    end else
    begin
        Blue_Panel.BevelOuter := bvLowered ;
        Green_Panel.BevelOuter := bvRaised ;
    end ;
end ;


function TPrinter_Options_Dialog.Get_Bar_Color : TColor ;

var I : TImage ;

begin
    Result := clWhite ;
    if( Green_Panel.BevelOuter = bvLowered ) then
    begin
        I := Green_Image ;
    end else
    if( Blue_Panel.BevelOuter = bvLowered ) then
    begin
        I := Blue_Image ;
    end else
    begin
        exit ;
    end ;
    Result := I.Picture.Bitmap.Canvas.Pixels[ 0, 0 ] ;
end ;


procedure TPrinter_Options_Dialog.HeightChange( Sender : TObject ) ;

begin
    OK_Button.Enabled := True ;
    if( RadioButton1.Checked and ( not FileExists( Filename.Text ) ) ) then
    begin
        OK_Button.Enabled := False ;
    end ;
end ;


procedure TPrinter_Options_Dialog.RadioButton1Click( Sender : TObject ) ;

begin
    Filename.Enabled := RadioButton1.Checked ;
    Browse_Button.Enabled := RadioButton1.Checked ;
    Windows_Font.Enabled := RadioButton2.Checked ;
end ;


procedure TPrinter_Options_Dialog.RadioButton3Click(Sender: TObject) ;

begin
    Alternate_Filename.Enabled := RadioButton3.Checked ;
    Alternate_Browse_Button.Enabled := RadioButton3.Checked ;
    Alternate_Windows_Font.Enabled := RadioButton4.Checked ;
end ;


procedure TPrinter_Options_Dialog.FormShow( Sender : TObject ) ;

var DC : HDC ;
    Dummy : integer ;

begin
    Font_List := TStringList.Create ;
    try
        // Build windows font list...
        DC := GetDC( Handle ) ;
        try
            EnumFonts( DC, nil, @CB_FontEnum, nil ) ;
        finally
            ReleaseDC( Handle, DC ) ;
        end ;
        Font_List.Sort ;
        Windows_Font.Items.Assign( Font_List ) ;
        Alternate_Windows_Font.Items.Assign( Font_List ) ;

        Dummy := Windows_Font.Items.Indexof( Current_Font ) ;
        if( Dummy = -1 ) then
        begin
            Dummy := Windows_Font.Items.Indexof( 'Courier New' ) ;
            RadioButton1.Checked := True ;
            if( pos( '\', Current_Font ) = 0 ) then
            begin
                Current_Font := OS^.Application_Path + 'fonts\' + Current_Font ;
            end ;
            Filename.Text := Current_Font + '.c' ;
        end else
        begin
            RadioButton2.Checked := True ;
        end ;
        if( Dummy = -1 ) then
        begin
            Windows_Font.ItemIndex := 0 ;
        end else
        begin
            Windows_Font.ItemIndex := Dummy ;
        end ;

        Dummy := Windows_Font.Items.Indexof( Alt_Current_Font ) ;
        if( Dummy = -1 ) then
        begin
            Alternate_Windows_Font.ItemIndex := Windows_Font.ItemIndex ;
            RadioButton3.Checked := True ;
            Alternate_Filename.Text := Alt_Current_Font ;
            if( Alt_Current_Font <> '' ) then
            begin
                if( pos( '\', Alt_Current_Font ) = 0 ) then
                begin
                    Alt_Current_Font := OS^.Application_Path + 'fonts\' + Alt_Current_Font ;
                end ;
                Alternate_Filename.Text := Alt_Current_Font + '.c' ;
            end ;
        end else
        begin
            Alternate_Windows_Font.ItemIndex := Dummy ;
            RadioButton4.Checked := True ;
        end ;
    finally
        Font_List.Free ;
    end ;
    OpenDialog1.InitialDir := OS^.Application_Path + 'fonts' ;
end ;


procedure TPrinter_Options_Dialog.Green_ImageClick(Sender: TObject);

begin
    Blue_Panel.BevelOuter := bvRaised ;
    Green_Panel.BevelOuter := bvLowered ;
end ;


procedure TPrinter_Options_Dialog.Blue_ImageClick(Sender: TObject) ;

begin
    Green_Panel.BevelOuter := bvRaised ;
    Blue_Panel.BevelOuter := bvLowered
end ;


procedure TPrinter_Options_Dialog.Browse_ButtonClick(Sender: TObject) ;

begin
    if( OpenDialog1.Execute ) then
    begin
        Filename.Text := OpenDialog1.Filename ;
    end ;
end ;


procedure TPrinter_Options_Dialog.Alternate_Browse_ButtonClick(Sender: TObject) ;

begin
    if( OpenDialog1.Execute ) then
    begin
        Alternate_Filename.Text := OpenDialog1.Filename ;
    end ;
end ;


end.
