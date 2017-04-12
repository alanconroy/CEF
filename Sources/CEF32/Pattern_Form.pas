{
        Program Name : Pattern_Form
        Package Name : CEF32
        Purpose      : Memory pattern form for CEF32
        Institution  :
        Date Written : 19-Nov-2007
        Written By   : Alan Conroy
        Version      : 1.6

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

	  This form allows the user to specify a pattern with which to fill
        memory for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Pattern_Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, StdCtrls, Buttons, ExtCtrls;

type
  TPattern_Dialog = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    Cancel_Button: TBitBtn;
    Label1: TLabel;
    Start_Address: TEdit;
    Label2: TLabel;
    End_Address: TEdit;
    Pattern: TGroupBox;
    Label3: TLabel;
    Size: TComboBox;
    Label4: TLabel;
    Values: TMemo;
    Increment_RB: TRadioButton;
    Values_RB: TRadioButton;
    Radix: TGroupBox;
    Binary: TRadioButton;
    Octal: TRadioButton;
    Decimal: TRadioButton;
    Hexadecimal: TRadioButton;
    Other: TRadioButton;
    Base: TSpinEdit;
    ASCII: TRadioButton;
    EBCDIC: TRadioButton;
    procedure BinaryClick(Sender: TObject);
    procedure OctalClick(Sender: TObject);
    procedure DecimalClick(Sender: TObject);
    procedure HexadecimalClick(Sender: TObject);
    procedure OtherClick(Sender: TObject);
    procedure ASCIIClick(Sender: TObject);
    procedure EBCDICClick(Sender: TObject);
    procedure Start_AddressKeyPress(Sender: TObject; var Key: Char);
    procedure End_AddressKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure Start_AddressChange(Sender: TObject);
    procedure ValuesKeyPress(Sender: TObject; var Key: Char);
    procedure BaseChange(Sender: TObject);

  private // Instance data...
      Base_Value, Last_Base_Value : integer ;

  public // API...
      procedure Set_Base( Value : integer ) ;
      procedure Get_Address_Range( var Low, High : int64 ) ;
      function Get_Value : string ;
  end ;

var Pattern_Dialog : TPattern_Dialog = nil ;

implementation

uses // C&C...
     _ASCII,
     CVT, // CVTB
     EBCDICs ;

{$R *.dfm}

procedure TPattern_Dialog.BinaryClick( Sender : TObject ) ;

begin
    Base.Enabled := False ;
    Set_Base( 2 ) ;
end ;


procedure TPattern_Dialog.OctalClick(Sender: TObject);

begin
    Base.Enabled := False ;
    Set_Base( 8 ) ;
end ;


procedure TPattern_Dialog.DecimalClick(Sender: TObject);

begin
    Base.Enabled := False ;
    Set_Base( 10 ) ;
end ;


procedure TPattern_Dialog.HexadecimalClick(Sender: TObject);

begin
    Base.Enabled := False ;
    Set_Base( 16 ) ;
end ;


procedure TPattern_Dialog.OtherClick(Sender: TObject);

begin
    Base.Enabled := True ;
end ;


procedure TPattern_Dialog.ASCIIClick(Sender: TObject);

begin
    Base.Enabled := False ;
    Set_Base( 0 ) ;
end ;


procedure TPattern_Dialog.EBCDICClick(Sender: TObject);

begin
    Base.Enabled := False ;
    Set_Base( 1 ) ;
end ;


// API...

procedure TPattern_Dialog.Set_Base( Value : integer ) ;

var I : int64 ;
    Loop, Work : integer ;
    S : string ;

begin
    if( Value <> Base_Value ) then
    begin
        if( Value >= 2 ) then // Numeric
        begin
            Base.Value := Value ;
            Start_Address.Text := CVTB( Last_Base_Value, Value, Start_Address.Text ) ;
            End_Address.Text := CVTB( Last_Base_Value, Value, End_Address.Text ) ;
            Last_Base_Value := Value ;
        end else
        if( Base_Value < 2 ) then // Converting textual representation to numeric
        begin
            // Reorganize values to match the word size...
            S := '' ;
            for Loop := 0 to Values.Lines.Count - 1 do
            begin
                S := S + Values.Lines[ Loop ] ;
            end ;
            Loop := strtoint( Size.Text ) ;
            Values.Lines.Clear ;
            while( length( S ) > 0 ) do
            begin
                Values.Lines.Add( copy( S, 1, Loop ) ) ;
                S := copy( S, Loop + 1, length( S ) ) ;
            end ;
        end ;

        // Convert values...
        for Loop := 0 to Values.Lines.Count - 1 do
        begin
            S := Values.Lines[ Loop ] ;
            case Base_Value of
                0 : // From ASCII
                    begin
                        if( Value = 1 ) then // To EBCDIC
                        begin
                            S := From_EBCDIC( To_EBCDIC( S ) ) ; // Get rid of invalid characters
                        end else
                        begin
                            I := 0 ;
                            for Work := length( S ) downto 1 do
                            begin
                                I := ( I shl 8 ) or ord( S[ Work ] ) ;
                            end ;
                            S := inttostr( I ) ;
                            S := CVTB( 10, Value, S ) ;
                        end ;
                    end ;
                1 : // From EBCDIC
                    begin
                        if( Value = 0 ) then // To ASCII
                        begin
                        end else
                        begin
                            S := To_EBCDIC( S ) ; // Convert ASCII representation to actual EBCDIC values
                            I := 0 ;
                            for Work := length( S ) downto 1 do
                            begin
                                I := ( I shl 8 ) or ord( S[ Work ] ) ;
                            end ;
                            S := inttostr( I ) ;
                            S := CVTB( 10, Value, S ) ;
                        end ;
                    end ;
                else
                    begin
                        case Value of
                            0 : // To ASCII
                                begin
                                    S := CVTB( Base_Value, 10, S ) ;
                                    I := strtoint( S ) ;
                                    S := '' ;
                                    for Work := 1 to strtoint( Size.Text ) do
                                    begin
                                        S := S + chr( I ) ;
                                        I := I shr 8 ;
                                    end ;
                                end ;
                            1 : // To EBCDIC
                                begin
                                    S := CVTB( Base_Value, 10, S ) ;
                                    I := strtoint( S ) ;
                                    S := '' ;
                                    for Work := 1 to strtoint( Size.Text ) do
                                    begin
                                        S := S + chr( I ) ;
                                        I := I shr 8 ;
                                    end ;
                                    S := From_EBCDIC( S ) ;
                                end ;
                            else
                                begin
                                    S := CVTB( Base_Value, Value, S ) ;
                                end ;
                        end ;
                    end ;
            end ; // case Base_Value
            Values.Lines[ Loop ] := S ;
        end ; // for Loop := 0 to Values.Lines.Count - 1
        Base_Value := Value ;
    end ; // if( Value <> Base_Value )
end ; // TPattern_Dialog.Set_Base


procedure TPattern_Dialog.Start_AddressKeyPress(Sender: TObject;
    var Key: Char);

begin
    if( ( Key <> DEL ) and ( Key <> BS ) ) then
    begin
        if( Base_Value >= 2 ) then
        begin
            if( not Valid_Base( Key, Base_Value ) ) then
            begin
                Key := #0 ;
            end ;
        end else
        begin
            if( not Valid_Base( Key, Last_Base_Value ) ) then
            begin
                Key := #0 ;
            end ;
        end ;
    end ;
end ;


procedure TPattern_Dialog.End_AddressKeyPress(Sender: TObject;
    var Key: Char);

begin
    if( ( Key <> DEL ) and ( Key <> BS ) ) then
    begin
        if( Base_Value >= 2 ) then
        begin
            if( not Valid_Base( Key, Base_Value ) ) then
            begin
                Key := #0 ;
            end ;
        end else
        begin
            if( not Valid_Base( Key, Last_Base_Value ) ) then
            begin
                Key := #0 ;
            end ;
        end ;
    end ;
end ;


procedure TPattern_Dialog.FormCreate(Sender: TObject);

begin
    Base_Value := 10 ;
    Last_Base_Value := 10 ;
end ;


procedure TPattern_Dialog.Start_AddressChange(Sender: TObject);

begin
    OK_Button.Enabled := ( Start_Address.Text <> '' ) and ( End_Address.Text <> '' ) ;
end ;


procedure TPattern_Dialog.ValuesKeyPress(Sender: TObject; var Key: Char);

begin
    if( ( Key <> CR ) and ( Key <> BS ) and ( Key <> DEL ) ) then
    begin
        if( Base_Value >= 2 ) then
        begin
            if( not Valid_Base( Key, Base_Value ) ) then
            begin
                Key := #0 ;
            end ;
        end ;
    end ;
end ;


procedure TPattern_Dialog.BaseChange(Sender: TObject);

begin
    if( Base.Enabled ) then // If user is changing the value
    begin
        Set_Base( Base.Value ) ;
    end ;
end ;


procedure TPattern_Dialog.Get_Address_Range( var Low, High : int64 ) ;

var S : string ;

begin
    S := CVTB( Last_Base_Value, 10, Start_Address.Text ) ;
    Low := strtoint( S ) ;
    S := CVTB( Last_Base_Value, 10, End_Address.Text ) ;
    High := strtoint( S ) ;
end ;


function TPattern_Dialog.Get_Value : string ;

var I : int64 ;
    Loop, Work : integer ;
    S : string ;

begin
    Result := '' ;
    if( Base_Value < 2 ) then // Textual
    begin
        for Loop := 0 to Values.Lines.Count - 1 do
        begin
            Result := Result + Values.Lines[ Loop ] ;
        end ;
        if( Base_Value = 1 ) then // EBCDIC
        begin
            Result := To_EBCDIC( Result ) ;
        end ;
        exit ;
    end ;

    for Loop := 0 to Values.Lines.Count - 1 do
    begin
        S := Values.Lines[ Loop ] ;
        if( Base_Value <> 10 ) then
        begin
            S := CVTB( Base_Value, 10, S ) ;
        end ;
        I := strtoint( S ) ;
        for Work := 1 to strtoint( Size.Text ) do
        begin
            Result := Result + chr( I ) ;
            I := I shr 8 ;
        end ;
    end ;
end ; // TPattern_Dialog.Get_Value


end.
