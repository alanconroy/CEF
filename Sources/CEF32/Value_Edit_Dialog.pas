{
        Program Name : GenNum_Dialog
        Package Name : LB
        Purpose      : Generic number entry dialog
        Institution  : Conroy & Conroy Co.
        Date Written : 19-April-2005
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2005 by Alan Conroy.  Released to the Public Domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

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

          This dialog is used to get a number from the user.  It is a general-
        purpose dialog with several advanced features.  Currently, this is
        limited to numeric input from -2^63 to 2^63 - 1.
}

unit Value_Edit_Dialog ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, Spin, Grids,

     // C&C...
     TDialogs, // TDialog

     // CEF...
     _CEF ; // TData_Type

type
  TEdit_Value_Form = class(TForm)
    Button_Panel: TPanel;
    OK_Button: TBitBtn;
    Cancel_Button: TBitBtn;
    Panel1: TPanel;
    Value_Edit: TEdit;
    Instruction_Label: TLabel;
    UpDown: TUpDown;
    Base_Panel: TPanel;
    Binary_RB: TRadioButton;
    Octal_RB: TRadioButton;
    Decimal_RB: TRadioButton;
    Hexadecimal_RB: TRadioButton;
    Other_RB: TRadioButton;
    Base_Spin: TSpinEdit;
    Grid: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure Value_EditKeyPress(Sender: TObject; var Key: Char);
    procedure Value_EditChange(Sender: TObject);
    procedure Base_SpinChange(Sender: TObject);
    procedure Binary_RBClick(Sender: TObject);
    procedure Octal_RBClick(Sender: TObject);
    procedure Decimal_RBClick(Sender: TObject);
    procedure Hexadecimal_RBClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridKeyPress(Sender: TObject; var Key: Char);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);

  private // Internal data...
      _Base : word ;
      _Maximum : int64 ;
      _Minimum : int64 ;
      _Max_Size : word ;
      _Data_Type : TData_Type ;
      _Value : string ;
      In_UpDown : boolean ; // True if in updown processing (to prevent infinite loops)
      In_Grid_Edit : boolean ; // True if editing the grid

  protected // Property handlers...
      function Get_Base : word ;
      procedure Set_Base( Value : word ) ;
      procedure Set_Maximum( Value : int64 ) ;
      procedure Set_Minimum( Value : int64 ) ;
      procedure Set_Value( Value : string ) ;

  protected // Internal utility routines...
      procedure Update_Grid ;

  public // API...
      property Data_Type : TData_Type
          read _Data_Type
          write _Data_Type ;
      function Valid : boolean ;

  published // API...
      property Base : word
          read Get_Base
          write Set_Base ;
      property Max_Size : word
          read _Max_Size
          write _Max_Size ;
      property Maximum : int64
          read _Maximum
          write Set_Maximum ;
      property Minimum : int64
          read _Minimum
          write Set_Minimum ;
      property Value : string
          read _Value
          write Set_Value ;
  end ;

implementation

{$R *.dfm}

uses // C&C...
     _ASCII, // NUL
     CommonUt, // Edit
     CVT, // CvtB
     EBCDICs, // To_EBCDIC
     L10N, // Text_*
     Num1s, // Num1
     Radix50s, // RAD_To_ASCII
     Standard, // Bit_Values
     TypeDefs, // TTri_State

     // CEF...
     CEFUtil ; // Number_Format

// TEdit_Value_Form...

procedure TEdit_Value_Form.FormCreate(Sender: TObject) ;

begin
    _Base := 10 ;
    _Maximum := -1 ;
end ;


procedure TEdit_Value_Form.FormShow(Sender: TObject);

begin
    UpDown.Height := Value_Edit.Height ;
    UpDown.Width := UpDown.Height ;
    UpDown.Left := Value_Edit.Left + Value_Edit.Width ;

    Caption := Text_Caption_Enter_Numeric_Value ;
    OK_Button.Caption := Text_Button_Caption_OK ;
    Cancel_Button.Caption := Text_Button_Caption_Cancel ;
    Instruction_Label.Caption := Text_Enter_The_Value ;
    Binary_RB.Caption := Text_Binary ;
    Octal_RB.Caption := Text_Octal ;
    Decimal_RB.Caption := Text_Decimal ;
    Hexadecimal_RB.Caption := Text_Hexadecimal ;
    Other_RB.Caption := Text_Other ;
end ;



procedure TEdit_Value_Form.UpDownChangingEx(Sender: TObject;
    var AllowChange: Boolean; NewValue: Smallint;
    Direction: TUpDownDirection ) ;

var Bytes : integer ;
    I64 : int64 ;

begin
    if( In_UpDown ) then
    begin
        exit ;
    end ;
    In_UpDown := True ;
    try
        if( Valid ) then
        begin
            if( Data_Type.Data_Type = DataType_Real ) then
            begin
                exit ; // Not supported yet
            end else
            begin
                I64 := 0 ;
                move( PChar( _Value )[ 0 ], I64, ( Data_Type.Size + 7 ) div 8 ) ;
                case Direction of
                    updNone : exit ;
                    updUp : if( I64 < _Maximum ) then I64 := I64 + 1 ;
                    updDown : if( I64 > _Minimum ) then I64 := I64 - 1 ;
                end ;
                Bytes := ( Data_Type.Size + 7 ) div 8 ;
                if( length( _Value ) < Bytes ) then
                begin
                    setlength( _Value, Bytes ) ;
                end ;
                move( I64, PChar( _Value )[ 0 ], Bytes ) ;
                Value := _Value ;
            end ;
        end ;
        UpDown.Position := 50 ;
    finally
        In_UpDown := False ;
    end ;
end ;


// Property handlers...

function TEdit_Value_Form.Get_Base : word ;

begin
    Result := _Base ;
end ;


procedure TEdit_Value_Form.GridKeyPress(Sender: TObject; var Key: Char) ;

begin
    if( ( Key <> BS ) and ( not Valid_Base( Key, Base ) ) ) then
    begin
        Key := NUL ;
    end ;
end ;


procedure TEdit_Value_Form.GridSetEditText( Sender : TObject ; ACol, ARow: Integer ;
    const Value : string ) ;

var C, R : integer ;
    Dummy : integer ;
    S : string ;
    V : int64 ;

begin
    In_Grid_Edit := True ;
    try
        C := ( ARow - 1 ) * ( Grid.ColCount - 1 ) + ACol - 1 ;
        if( C >= Data_Type.Max_Size ) then
        begin
            exit ; // Beyond end of data
        end ;
        for Dummy := 0 to length( _Value ) - 1 do
        begin
            C := Dummy - ( Dummy div ( Grid.ColCount - 1 ) ) + 1 ;
            R := Dummy div ( Grid.ColCount - 1 ) + 1 ;
            if( ( ACol = C ) and ( ARow = R ) ) then
            begin
                S := Value ;
                if( S = '' ) then
                begin
                    exit ;
                end ;
            end else
            begin
                S := Grid.Cells[ C, R ] ;
            end ;
            if( S = '' ) then
            begin
                S := '0' ;
            end ;
            V := strtoint( cvtb( Base, 10, S ) ) ;
            if( V <= 255 ) then
            begin
                _Value[ Dummy + 1 ] := chr( V ) ;
            end else
            if( ( ACol = C ) and ( ARow = R ) ) then
            begin
                Grid.Cells[ C, R ] := cvtb( 10, Base, inttostr( ord( _Value[ Dummy ] ) ) ) ;
            end ;
        end ; // for Dummy := 0 to length( _Value ) - 1 
        Set_Value( _Value ) ;
    finally
        In_Grid_Edit := False ;
    end ;
end ; // TEdit_Value_Form.GridSetEditText


procedure TEdit_Value_Form.Set_Base( Value : word ) ;

var Old : word ;

begin
    if( Value <> _Base ) then
    begin
        if( ( Value > 1 ) and ( Value < 50 ) ) then
        begin
            case Value of
                2 : Binary_RB.Checked := True ;
                8 : Octal_RB.Checked := True ;
                10 : Decimal_RB.Checked := True ;
                16 : Hexadecimal_RB.Checked := True ;
                else Other_RB.Checked := True ;
            end ;
            Old := Base ;
            _Base := Value ;
            if( Data_Type.Data_Type = DataType_Integer ) then
            begin
                if( Valid_Base( Value_Edit.Text, Old ) ) then
                begin
                    Value_Edit.Text := CvtB( Old, Value, Value_Edit.Text ) ;
                end ;
            end ;
            Base_Spin.Value := Value ;
        end ; // if
        Update_Grid ;
    end ; // if( Value <> _Base )
end ; // TEdit_Value_Form.Set_Base


procedure TEdit_Value_Form.Set_Maximum( Value : int64 ) ;

begin
    _Maximum := Value ;
    Value_EditChange( Value_Edit ) ;
end ;


procedure TEdit_Value_Form.Set_Minimum( Value : int64 ) ;

begin
    _Minimum := Value ;
    Value_EditChange( Value_Edit ) ;
end ;


procedure TEdit_Value_Form.Update_Grid ;

var C, R, Dummy, Offset : integer ;

begin
    if( In_Grid_Edit ) then
    begin
        exit ;
    end ;

    // Set up edit grid...
    if( Data_Type.Max_Size > length( _Value ) * 8 ) then
    begin
        C := 2 ;
    end else
    if( length( Value ) < 8 ) then
    begin
        C := length( Value ) + 1 ;
    end else
    begin
        C := 9 ;
    end ;
    if( C < 2 ) then
    begin
        C := 2 ;
    end ;
    Grid.ColCount := C ;
    R := ( ( length( _Value ) + C - 2 ) div ( C - 1 ) ) + 1 ;
    if( R < 2 ) then
    begin
        R := 2 ;
    end ;
    Grid.RowCount := R ;
    Grid.FixedCols := 1 ;
    Grid.FixedRows := 1 ;
    for Dummy := 1 to Grid.RowCount - 1 do
    begin
        Grid.Cells[ 0, Dummy ] := inttostr( ( Dummy - 1 ) * ( Grid.ColCount - 1 ) ) ;
        for C := 0 to Grid.ColCount - 2 do
        begin
            if( C > length( _Value ) ) then
            begin
                exit ;
            end ;
            Offset := ( Dummy - 1 ) * ( Grid.ColCount - 1 ) + C + 1 ;
            if( Offset > length( _Value ) ) then
            begin
                Grid.Cells[ C + 1, Dummy ] := '' ;
            end else
            begin
                Grid.Cells[ C + 1, Dummy ] :=
                    cvtb( 10, Base, inttostr( ord( _Value[ Offset ] ) ) ) ;
            end ;
        end ;
    end ; // for Dummy := 1 to Grid.RowCount - 1
end ; // TEdit_Value_Form.Update_Grid


procedure TEdit_Value_Form.Set_Value( Value : string ) ;

var Dummy : integer ;
    I64 : int64 ;
    S : string ;

begin
    _Value := Value ;
    
    // Handle numeric-only controls...
    UpDown.Enabled := ( Data_Type.Family = DataType_Family_Numeric ) ;

    // Determine maximum/minimum integer values...
    if( Data_Type.Size >= 64 ) then
    begin
        Maximum := $7FFFFFFFFFFFFFFF ;
        Minimum := -Maximum - 1 ;
    end else
    if( Data_Type.Size <= 1 ) then
    begin
        Maximum := 1 ;
        Minimum := 0 ;
    end else
    begin
        Maximum := Bit_Values[ Data_Type.Size ] - 1 ;
        Minimum := -Maximum - 1 ;
        if( Data_Type.Data_Type = DataType_Family_Numeric ) then
        begin
            case Data_Type.Signed of
                TS_Dont_Care : if( Data_Type.Size < 63 ) then Maximum := Bit_Values[ Data_Type.Size + 1 ] - 1 ;
                TS_False : begin
                               if( Data_Type.Size < 63 ) then Maximum := Bit_Values[ Data_Type.Size + 1 ] - 1 ;
                               Minimum := 0 ; // Unsigned can't be less than 0
                           end ;
            end ;
        end ;
    end ;
    case Data_Type.Data_Type of
        DataType_Boolean:
            begin
                if( Value[ 1 ] = #0 ) then
                begin
                    Value_Edit.Text := 'False' ;
                end else
                begin
                    Value_Edit.Text := 'True' ;
                end ;
            end ;

        DataType_String:
            begin
                case Data_Type.Encoding of
                    Datatype_String_Encoding_EBCDIC: Value_Edit.Text := From_EBCDIC( S ) ;
                    Datatype_String_Encoding_Radix50: Value_Edit.Text := RAD_To_ASCII( Value ) ;
                    Datatype_String_Encoding_UTF8: Value_Edit.Text := Utf8ToAnsi( Value ) ;
                    else Value_Edit.Text := Value ;
                end ;
            end ;

        DataType_Integer:
            begin
                I64 := 0 ;
                move( PChar( Value )[ 0 ], I64, length( Value ) ) ;
                S := CVTB( 10, Base, inttostr( I64 ) ) ;
                S := Number_Format( Base, Data_Type.Size, S ) ;
                Dummy := pos( ' ', S ) ;
                if( Dummy > 0 ) then
                begin
                    S := Edit( copy( S, Dummy, length( S ) ), 8 ) ;
                end ;
                Value_Edit.Text := S ;
            end ;

        DataType_Real: ; //~~~
        DataType_BCD: Value_Edit.Text := Decode_BCD( Value, Data_Type.Pack ) ;
    end ; // case Data_Type.Data_Type

    Update_Grid ;
end ; // TEdit_Value_Form.Set_Value


procedure TEdit_Value_Form.Value_EditKeyPress(Sender: TObject; var Key: Char);

begin
    if( Key = BS ) then // Delete is valid
    begin
        exit ;
    end ;

    case Data_Type.Data_Type of
        DataType_Boolean:
            begin
                if( pos( Key, 'TRUEFALSNOY01truefalsnoy' ) = 0 ) then
                begin
                    Key := NUL ;
                    MessageBeep( 0 ) ;
                end ;
            end ;

        DataType_String:
            begin
                case Data_Type.Encoding of
                    Datatype_String_Encoding_EBCDIC:
                        if( not Valid_EBCDIC( Key ) ) then
                        begin
                            Key := NUL ;
                            MessageBeep( 0 ) ;
                        end ;

                    Datatype_String_Encoding_Radix50:
                        if( not Valid_Radix50( Key ) ) then
                        begin
                            Key := NUL ;
                            MessageBeep( 0 ) ;
                        end ;
                end ;
            end ;

        DataType_Integer:
            begin
                Key := upcase( Key ) ;
                if( not Valid_Base( Key, Base ) ) then
                begin
                    if( Key <> '-' ) then
                    begin
                        Key := NUL ;
                        MessageBeep( 0 ) ;
                    end ;
                end ;
            end ;

        DataType_Real: ; //~~~
        DataType_BCD:
            begin
                if( pos( Key, '0123456789-+' ) = 0 ) then
                begin
                    if( ( not Data_Type.Fixed ) or ( Key <> '.' ) ) then
                    begin
                        Key := NUL ;
                        MessageBeep( 0 ) ;
                    end ;
                end ;
            end ;
    end ; // case Data_Type.Data_Type
end ; // TEdit_Value_Form.Value_EditKeyPress


procedure TEdit_Value_Form.Value_EditChange( Sender : TObject ) ;

var I64 : int64 ;
    S : string ;

begin
    OK_Button.Enabled := Valid ;
    if( In_Grid_Edit or In_UpDown ) then
    begin
        exit ;
    end ;
    if( not OK_Button.Enabled ) then
    begin
        exit ;
    end ;
    case Data_Type.Data_Type of
        DataType_Boolean:
            begin
                S := lowercase( Value_Edit.Text ) ;
                if( ( S = 't' )
                    or
                    ( S = 'true' )
                    or
                    ( S = '1' )
                    or
                    ( S = 'yes' )
                  ) then
                begin
                    _Value := #1 ;
                end else
                begin
                    _Value := #0 ;
                end ;
            end ;

        DataType_String:
            begin
                case Data_Type.Encoding of
                    Datatype_String_Encoding_EBCDIC: _Value := To_EBCDIC( Value_Edit.Text ) ;
                    Datatype_String_Encoding_Radix50: _Value := ASCII_To_RAD( Value_Edit.Text ) ;
                    Datatype_String_Encoding_UTF8: _Value := AnsiToUtf8( Value_Edit.Text ) ;
                    else _Value := Value_Edit.Text ;
                end ;
            end ;

        DataType_Integer:
            begin
                S := cvtb( Base, 10, Value_Edit.Text ) ;
                I64 := strtoint64( S ) ;
                move( I64, PChar( _Value )[ 0 ], ( Data_Type.Size + 7 ) div 8 ) ;
            end ;

        DataType_Real: ; //~~~
        DataType_BCD: _Value := Encode_BCD( Value_Edit.Text, Data_Type.Pack ) ;
    end ; // case Data_Type.Data_Type
    Update_Grid ;
end ;


procedure TEdit_Value_Form.Base_SpinChange( Sender : TObject ) ;

begin
    Base := Base_Spin.Value ;
end ;


procedure TEdit_Value_Form.Binary_RBClick(Sender: TObject);

begin
    Base := 2 ;
end ;

procedure TEdit_Value_Form.Octal_RBClick(Sender: TObject);

begin
    Base := 8 ;
end ;


procedure TEdit_Value_Form.Decimal_RBClick(Sender: TObject);

begin
    Base := 10 ;
end ;


procedure TEdit_Value_Form.Hexadecimal_RBClick(Sender: TObject);

begin
    Base := 16 ;
end ;


function TEdit_Value_Form.Valid : boolean ;

var Dummy, Dummy1 : integer ;
    I64 : int64 ;
    S : string ;

begin
    Result := True ; // Assume all is good
    case _Data_Type.Data_Type of
        DataType_Boolean:
            begin
                S := lowercase( Value_Edit.Text ) ;
                Result := ( S = 't' )
                          or
                          ( S = 'f' )
                          or
                          ( S = 'true' )
                          or
                          ( S = 'false' )
                          or
                          ( S = '0' )
                          or
                          ( S = '1' )
                          or
                          ( S = 'yes' )
                          or
                          ( s = 'no' ) ;
            end ;

        DataType_Integer :
            begin
                I64 := strtoint64( CvtB( Base, 10, Value_Edit.Text ) ) ;
                Result := (
                            ( ( I64 >= Minimum ) and ( I64 <= Maximum ) )
                            or
                            ( Minimum > Maximum )
                          ) ;
            end ;

        DataType_String:
            begin
                case Data_Type.Encoding of
                    Datatype_String_Encoding_EBCDIC:
                        Result := ( Valid_EBCDIC( Value_Edit.Text ) and ( length( Value_Edit.Text ) < Data_Type.Max_Size ) ) ;

                    Datatype_String_Encoding_Radix50:
                        Result := ( Valid_Radix50( Value_Edit.Text ) and ( length( Value_Edit.Text ) * 2 div 3 < Data_Type.Max_Size ) ) ;

                    else
                        Result := ( ( length( Value_Edit.Text ) < Data_Type.Max_Size ) ) ;
                end ;
            end ;

        DataType_BCD:
            begin
                S := '0123456789-+' ;
                if( Data_Type.Fixed ) then
                begin
                    S := S + '.' ;
                end ;
                Dummy1 := 0 ; // How many decimals
                for Dummy := 1 to length( Value_Edit.Text ) do
                begin
                    if( pos( Value_Edit.Text[ Dummy ], S ) = 0 ) then
                    begin
                        Result := False ;
                        exit ;
                    end ;
                    if( Value_Edit.Text[ Dummy ] = '.' ) then
                    begin
                        inc( Dummy1 ) ;
                    end ;
                end ;
                Dummy := Data_Type.Max_Size ;
                if( Data_Type.Pack ) then
                begin
                    Dummy := Dummy * 2 ;
                end ;
                if( length( Value_Edit.Text ) - Dummy1 > Dummy ) then
                begin
                    Result := False ;
                end ;
            end ;
    end ; // case _Data_Type.Data_Type
end ; // TEdit_Value_Form.Valid


end.
