{
              Program Name : DumpMem
              Package Name : CEF
              Purpose      : Dump memory contents dialog
              Institution  : Conroy & Conroy
              Date Written : 19-Mar-2005
              Written By   : Alan Conroy
              Version      : 1.0

              Copyright (C) 2005 by Alan Conroy.  Released to the public domain.

              TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *********************************************************
        *                                                       *
        *        M O D I F I C A T I O N   H I S T O R Y        *
        *                                                       *
        *     DATE      BY             REASON                   *
        *                                                       *
        *********************************************************

        *********************************************************
        *                                                       *
        *           P R O G R A M   P U R P O S E               *
        *                                                       *
        *********************************************************

          This dialog is used to dump a memory image.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit DumpMem ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, Buttons, StdCtrls;

type TDump_Memory_Form = class(TForm)
                            Label1: TLabel;
                            File_Name: TEdit;
                            SpeedButton1: TSpeedButton;
                            Save_Dump_Dialog: TSaveDialog;
                            Button_Panel: TPanel;
                            Label2: TLabel;
                            Starting_Address: TEdit;
                            Label3: TLabel;
                            Ending_Address: TEdit;
                            Label4: TLabel;
                            Size: TEdit;
                            OK_Button: TBitBtn;
                            BitBtn2: TBitBtn;
                            Help_Button: TBitBtn;
                            procedure File_NameChange(Sender: TObject);
                            procedure Starting_AddressChange(Sender: TObject);
                            procedure Ending_AddressChange(Sender: TObject);
                            procedure SizeChange(Sender: TObject);
                            procedure Help_ButtonClick(Sender: TObject);
                            procedure SpeedButton1Click(Sender: TObject);

                          private // Internal utility routines...
                              procedure Validate ;

                          public
                              Base : integer ;
                         end ;


var Dump_Memory_Form : TDump_Memory_Form = nil ;

implementation

uses // C&C...
     CVT ; // CVTB

{$R *.dfm}

procedure TDump_Memory_Form.Validate ;

begin
    OK_Button.Enabled := False ;
    if( File_Name.Text = '' ) then
    begin
        exit ;
    end ;
    if( Size.Text = '' ) then
    begin
        exit ;
    end ;
    if( not Valid_Base( uppercase( Starting_Address.Text ), Base ) ) then
    begin
        exit ;
    end ;
    if( not Valid_Base( uppercase( Ending_Address.Text ), Base ) ) then
    begin
        exit ;
    end ;
    OK_Button.Enabled := True ;
end ;


procedure TDump_Memory_Form.File_NameChange(Sender: TObject) ;

begin
    Validate ;
end ;


procedure TDump_Memory_Form.Starting_AddressChange(Sender: TObject) ;

begin
    try
        Size.Text :=
          inttostr( strtoint( CVTB( Base, 10, uppercase( Ending_Address.Text ) ) ) -
          strtoint( CVTB( Base, 10, uppercase( Starting_Address.Text ) ) ) + 1 ) ;
        Validate ;
    except
        OK_Button.Enabled := False ;
    end ;
end ;


procedure TDump_Memory_Form.Ending_AddressChange(Sender: TObject) ;

begin
    try
        Size.Text :=
          inttostr( strtoint( CVTB( Base, 10, uppercase( Ending_Address.Text ) ) ) -
          strtoint( CVTB( Base, 10, uppercase( Starting_Address.Text ) ) ) + 1 ) ;
        Validate ;
    except
        OK_Button.Enabled := False ;
    end ;
end ;


procedure TDump_Memory_Form.SizeChange( Sender : TObject ) ;

begin
    try
        Ending_Address.Text :=
            CVTB( 10, Base, inttostr( strtoint( CVTB( Base, 10, uppercase( Starting_Address.Text ) ) ) + strtoint( Size.Text ) - 1 ) ) ;
        Validate ;
    except
        OK_Button.Enabled := False ;
    end ;
end ;


procedure TDump_Memory_Form.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



procedure TDump_Memory_Form.SpeedButton1Click( Sender : TObject ) ;

begin
    if( Save_Dump_Dialog.Execute ) then
    begin
        File_Name.Text := Save_Dump_Dialog.Filename ;
    end ;
end ;


end.

