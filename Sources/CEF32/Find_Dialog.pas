{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 find dialog
        Institution  :
        Date Written : 3-Aug-2001
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

          This form is used to query the user for search data.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Find_Dialog ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, Buttons, ExtCtrls;

type
  TFind_Form = class(TForm)
    Panel1: TPanel;
    OK_Button: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    Options: TGroupBox;
    Bytes: TRadioButton;
    Literal_Text: TRadioButton;
    Case_Sensitive: TCheckBox;
    Little_Endian: TCheckBox;
    Help_Button: TBitBtn;
    procedure Literal_TextClick(Sender: TObject);
    procedure BytesClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit1Change(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);

  private // Instance data...
      _Base : integer ;

  public // API...
      property Base : integer
               read _Base
               write _Base ;
      function Find_Image : string ;
  end ;

var Find_Form : TFind_Form ;

implementation

uses CVT, // Valid_Base
     CommonUt ; // Edit

{$R *.DFM}

// TFind_Form methods...

procedure TFind_Form.Literal_TextClick( Sender : TObject ) ;

begin
    Case_Sensitive.Enabled := True ;
    Little_Endian.Enabled := False ;
    Edit1.Text := Find_Image ;
end ;


procedure TFind_Form.BytesClick( Sender : TObject ) ;

var Loop : integer ;
    S : string ;

begin
    Case_Sensitive.Enabled := False ;
    Little_Endian.Enabled := True ;

    // Convert the string to a series of byte values...
    S := '' ;
    for Loop := 1 to length( Edit1.Text ) do
    begin
        S := S + inttostr( ord( Edit1.Text[ Loop ] ) ) + ' ' ;
    end ;
    Edit1.Text := S ;
end ;


procedure TFind_Form.Edit1KeyPress( Sender : TObject ; var Key : Char ) ;

begin
    if( Bytes.Checked ) then
    begin
        if( ( Key <> #8 ) and ( Key <> ' ' ) ) then
        begin
            if( not Valid_Base( Key, Base ) ) then
            begin
                Key := #0 ;
                MessageBeep( 0 ) ;
            end ;
        end ;
    end ;
end ;


procedure TFind_Form.Edit1Change( Sender : TObject ) ;

begin
    if( Bytes.Checked ) then
    begin
        if( not Valid_Base( Edit( Edit1.Text, 2 ), Base ) ) then // Validate pastes
        begin
            Edit1.Text := '' ;
            MessageBeep( 0 ) ;
        end ;
    end ;
    OK_Button.Enabled := ( length( Edit1.Text ) > 0 ) ;
end ;


// API...

// Return the image string to search for
function TFind_Form.Find_Image : string ;

var Dummy : integer ;
    I : int64 ;
    S, Work, Temp : string ;

begin
    if( Bytes.Checked ) then
    begin
        Work := Edit( Edit1.Text, 8 or 16 or 128 ) ;
        while( length( Work ) > 0 ) do
        begin
            Dummy := pos( ' ', Work + ' ' ) ;
            Temp := copy( Work, 1, Dummy - 1 ) ;
            Work := copy( Work, Dummy + 1, length( Work ) ) ;
            // TODO: Make leading zeroes significant (for multi-byte values)
            I := strtoint( CVTB( Base, 10, Temp ) ) ; // TODO: validate size
            Temp := '' ;
            if( Little_Endian.Checked ) then
            begin
                while( I > 255 ) do
                begin
                    Temp := Temp + char( I and 255 ) ;
                    I := I shr 8 ;
                end ;
                Temp := Temp + char( I ) ;
            end else
            begin
                while( I > 255 ) do
                begin
                    Temp := char( I and 255 ) + Temp ;
                    I := I shr 8 ;
                end ;
                Temp := char( I ) + Temp ;
            end ;
            S := S + Temp ;
        end ;
        Result := S ;
    end else
    begin
        Result := Edit1.Text ;
    end ;
end ;


procedure TFind_Form.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.
