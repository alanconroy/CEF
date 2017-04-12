{
              Program Name : F3850
              Package Name : F3850
              Purpose      : Fairchild 3850 CPU assembler
              Institution  : Conroy & Conroy
              Date Written : 19-Jun-2007
              Written By   : Alan Conroy
              Version      : 1.0

              Copyright (C) 2007 by Alan Conroy.  Released to the public domain.

              TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *********************************************************
        *                                                       *
        *        M O D I F I C A T I O N   H I S T O R Y        *
        *                                                       *
        *********************************************************

             DATE      BY             REASON                   

        *********************************************************
        *                                                       *
        *           P R O G R A M   P U R P O S E               *
        *                                                       *
        *********************************************************

          This unit defines the assembler for the CEF F3850 component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit F3850_ASM ;

interface

uses { CEF... }
     _CEF, // TCPU
     CEF, { TBase_Assembler }

     { Other... }
     _UE, // TUnified_Exception
     CommonUt ; // TInteger_List

const F3850AssemblerErr_Facility = 52 ;
      F3850AssemblerErr_Success = 0 ;
      F3850AssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
      F3850AssemblerErr_Illegal_Expression = 2 ; // Illegal expression
      F3850AssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
      F3850AssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol

const Translate_Timer : array[ 0..255 ] of integer = (
                                                        24, // 0
                                                        23,
                                                        81,
                                                        22,
                                                        175,
                                                        80,
                                                        41,
                                                        21,
                                                        86,
                                                        174,
                                                        166,
                                                        79,
                                                        138,
                                                        40,
                                                        10,
                                                        20,
                                                        85, // 10
                                                        98,
                                                        173,
                                                        94,
                                                        165,
                                                        51,
                                                        78,
                                                        159,
                                                        137,
                                                        132,
                                                        39,
                                                        210,
                                                        9,
                                                        243,
                                                        19,
                                                        232,
                                                        84, // 20
                                                        169,
                                                        97,
                                                        213,
                                                        172,
                                                        195,
                                                        93,
                                                        146,
                                                        164,
                                                        192,
                                                        50,
                                                        67,
                                                        77,
                                                        120,
                                                        158,
                                                        3,
                                                        90, // 30
                                                        136,
                                                        71,
                                                        131,
                                                        187,
                                                        38,
                                                        223,
                                                        209,
                                                        143,
                                                        8,
                                                        127,
                                                        242,
                                                        181,
                                                        18,
                                                        58,
                                                        231,
                                                        83, // 40
                                                        43,
                                                        168,
                                                        12,
                                                        96,
                                                        161,
                                                        212,
                                                        234,
                                                        171,
                                                        197,
                                                        194,
                                                        122,
                                                        92,
                                                        189,
                                                        145,
                                                        183,
                                                        45, // 50
                                                        163,
                                                        199,
                                                        191,
                                                        47,
                                                        49,
                                                        105,
                                                        66,
                                                        103,
                                                        76,
                                                        110,
                                                        119,
                                                        64,
                                                        157,
                                                        34,
                                                        2,
                                                        27, // 60
                                                        89,
                                                        101,
                                                        135,
                                                        216,
                                                        70,
                                                        74,
                                                        130,
                                                        237,
                                                        186,
                                                        108,
                                                        37,
                                                        219,
                                                        222,
                                                        117,
                                                        208,
                                                        142, // 70
                                                        247,
                                                        7,
                                                        62,
                                                        126,
                                                        114,
                                                        241,
                                                        155,
                                                        180,
                                                        151,
                                                        17,
                                                        32,
                                                        57,
                                                        205,
                                                        230,
                                                        0,
                                                        25, // 80
                                                        82,
                                                        176,
                                                        42,
                                                        87,
                                                        167,
                                                        139,
                                                        11,
                                                        99,
                                                        95,
                                                        52,
                                                        160,
                                                        133,
                                                        211,
                                                        244,
                                                        233,
                                                        170, // 90
                                                        214,
                                                        196,
                                                        147,
                                                        193,
                                                        68,
                                                        121,
                                                        4,
                                                        91,
                                                        72,
                                                        188,
                                                        224,
                                                        144,
                                                        128,
                                                        182,
                                                        59,
                                                        44, // A0
                                                        13,
                                                        162,
                                                        235,
                                                        198,
                                                        123,
                                                        190,
                                                        184,
                                                        46,
                                                        200,
                                                        48,
                                                        106,
                                                        104,
                                                        111,
                                                        65,
                                                        35,
                                                        28, // B0
                                                        102,
                                                        217,
                                                        75,
                                                        238,
                                                        109,
                                                        220,
                                                        118,
                                                        248,
                                                        63,
                                                        115,
                                                        156,
                                                        152,
                                                        33,
                                                        206,
                                                        1,
                                                        26, // C0
                                                        177,
                                                        88,
                                                        140,
                                                        100,
                                                        53,
                                                        134,
                                                        245,
                                                        215,
                                                        148,
                                                        69,
                                                        5,
                                                        73,
                                                        225,
                                                        129,
                                                        60,
                                                        14, // D0
                                                        236,
                                                        124,
                                                        185,
                                                        201,
                                                        107,
                                                        112,
                                                        36,
                                                        29,
                                                        218,
                                                        239,
                                                        221,
                                                        249,
                                                        116,
                                                        153,
                                                        207,
                                                        178, // E0
                                                        141,
                                                        54,
                                                        246,
                                                        149,
                                                        6,
                                                        226,
                                                        61,
                                                        15,
                                                        125,
                                                        202,
                                                        113,
                                                        30,
                                                        240,
                                                        250,
                                                        154,
                                                        179, // F0
                                                        55,
                                                        150,
                                                        227,
                                                        16,
                                                        203,
                                                        31,
                                                        251,
                                                        56,
                                                        228,
                                                        204,
                                                        252,
                                                        229,
                                                        253,
                                                        254,
                                                        -1 // FF
                                                      ) ;

type TF3850_Assembler = class( TBase_Assembler )
                          public // Instance data...
                              Assembled_Code : string ;
                              Base : integer ;
                              Err : boolean ; { True if an error }
                              _Master : TMaster_Assembler ;
                              PC : integer ; { Current target address }
                              CPU : TCPU ;
                              Temp_Normalize_Expression : string ;
                              CEF_Mode : boolean ; // True if in CEF-compliant mode
                              Segments : TInteger_List ;

                          private // Internal utility routines...
                              procedure Add_Segments( S1, S2, S3 : integer ) ;
                              function Ar( S : string ) : integer ;
                              function Evaluate( const X : string ;
                                  var _Result : longint ) : TUnified_Exception ;
                              function Handle_Directives( var Value : string ;
                                  Status : TAssembler_Status ) : boolean ;

                          public // API...
                              procedure Initialize( Master : TMaster_Assembler ) ;
                                  override ;

                              procedure Terminate ; override ; stdcall ;

                              function Assemble_Ex( Inputs : PChar ;
                                  var Outputs, Machines : PChar ;
                                  var MachineL : longint ; var Address : int64 ;
                                  var Segment : longint ;
                                  Status : TAssembler_Status ;
                                  Flags : longint ) : TUnified_Exception ; override ;

                              function Default_Radix : longint ; override ;

                              function Default_Size : longint ; override ;

                              function Facility_Code : longint ; override ;

                              function Source_Extensions : PChar ; override ;

                              function Valid_Symbol_Initial : PChar ; override ;

                              function Valid_Symbol_After : PChar ; override ;

                              function Normalize_Expression( Expression : PChar ;
                                  Numeric : boolean ;
                                  Status : TAssembler_Status ) : PChar ;
                                  override ;
                      end ; // TF3850_Assembler

implementation

uses // Borland...
     SysUtils, // strpas

     // C&C...
     _ASCII, // CR
     CVT, // CVTB
     Express,
     Parse,
     Standard,
     Helps,
     Instrs,
     Maths,
     NUM1S,
     TypeDefs, // Set_Of_Digits
     UStrings,

     // F3850...
     F3850_CPU ; // TF3850

function UnTranslate_Timer( X : integer ) : integer ;

var Dummy : integer ;

begin
    Result := -1 ;
    for Dummy := 0 to 255 do
    begin
        if( Translate_Timer[ Dummy ] = X ) then
        begin
            Result := Dummy ;
            exit ;
        end ;
    end ;
end ;



// TF3850_Assembler methods...

procedure TF3850_Assembler.Initialize( Master : TMaster_Assembler ) ;

begin
    _Master := Master ;

    CEF_Mode := False ;
end ;


{ Informs the assembler that the current assembly operation is now
  complete. }
procedure TF3850_Assembler.Terminate ;

begin
    Free ;
end ;


procedure TF3850_Assembler.Add_Segments( S1, S2, S3 : integer ) ;

begin
    Segments.Clear ;
    Segments.Add( S1 * 8 ) ;
    Segments.Add( S2 * 8 ) ;
    Segments.Add( S3 * 8 ) ;
end ;


function TF3850_Assembler.Ar( S : string ) : integer ;

begin
    Result := -1 ; // Assume failure

    S := Edit( S, -1 ) ;
    if( S = 'S' ) then
    begin
        Result := 12 ;
        exit ;
    end ;
    if( S = 'I' ) then
    begin
        Result := 13 ;
        exit ;
    end ;
    if( S = 'D' ) then
    begin
        Result := 14 ;
        exit ;
    end ;
    if( copy( S, 1, 1 ) = 'R' ) then
    begin
        S := copy( S, 2, length( S ) ) ;
    end ;

    // Parse the value...
    if( S = '' ) then // No value
    begin
        exit ;
    end ;
    S := CVTB( Base, 10, S ) ;
    try
        Result := strtoint( S ) ;
        if( ( Result < 0 ) or ( Result > 11 ) ) then
        begin
            Result := -1 ; // Out of range
        end ;
    except
        Result := -1 ;
    end ;
end ;


function Bit_Range( Start, Length : integer ) : integer ;
// Pack Start and Length into a bit range (Start value in low 16 bits, Length value in high 16 bits)

begin
    Result := ( Length shl 16 ) or Start ;
end ;


function ET( Code : integer ) : string ;

begin
    case Code of
        1 : Result := 'Invalid digits' ;
        2 : Result := 'Illegal expression' ;
        3 : Result := 'Unterminated string literal' ;
        4 : Result := 'Undefined symbol' ;
        5 : Result := 'Illegal Instruction' ;
    end ;
end ;


function TF3850_Assembler.Assemble_Ex( Inputs : PChar ;
    var Outputs, Machines : PChar ;
    var MachineL : longint ; var Address : int64 ;
    var Segment : longint ; Status : TAssembler_Status ;
    Flags : longint ) : TUnified_Exception ;

var _Value : string ;
    Input_Buffer : string ;

    function Get_Token : string ;

    var C : char ;
        Dummy : integer ;
        Only_Numbers : boolean ;

    begin
        Input_Buffer := Edit( Input_Buffer, 8 ) ; // Ignore leading spaces...

        if( length( Input_Buffer ) > 0 ) then
        begin
            C := Input_Buffer[ 1 ] ;
        end else
        begin
            C := ' ' ;
        end ;
        if( ( C = '"' ) or ( C = #39 ) ) then
        begin
            Dummy := Instr( 2, Input_Buffer + C, C ) ;
            Result := copy( Input_Buffer, 1, Dummy ) ;
            Input_Buffer := copy( Input_Buffer, Dummy + 1, length( Input_Buffer ) ) ;
            exit ;
        end ; // if( ( C = '"' ) or ( C = #39 ) )
        Only_Numbers := True ;
        for Dummy := 1 to length( Input_Buffer ) do
        begin
            if( pos( Input_Buffer[ Dummy ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$_' ) = 0 ) then
            begin
                if( Dummy = 1 ) then
                begin
                    Result := Input_Buffer[ 1 ] ;
                    Input_Buffer := copy( Input_Buffer, 2, length( Input_Buffer ) ) ;
                end else
                begin
                    if( ( Input_Buffer[ Dummy ] = '.' ) and Only_Numbers ) then
                    begin
                        continue ;
                    end ;
                    Result := copy( Input_Buffer, 1, Dummy - 1 ) ;
                    Input_Buffer := copy( Input_Buffer, Dummy, length( Input_Buffer ) ) ;
                end ;
                exit ;
            end ;
            if( pos( Input_Buffer[ Dummy ], '0123456789' ) = 0 ) then
            begin
                Only_Numbers := False ;
            end ;
        end ; // for Dummy := 1 to length( Input_Buffer )
        Result := Input_Buffer ;
        Input_Buffer := '' ;
    end ; // Get_Token


    function Peek_Token : string ;

    begin
        Result := Get_Token ;
        if( length( Result ) > 0 ) then
        begin
            Input_Buffer := Result + ' ' + Input_Buffer ;
        end ;
        Result := Edit( Result, 32 or 256 ) ;
    end ;


    function Grab_Line : string ;

    begin
        Result := Input_Buffer ;
        Input_Buffer := '' ;
    end ;


    function Value : string ; // Get operand 2

    var S : string ;

    begin
        if( length( _Value ) = 0 ) then
        begin
            _Value := uppercase( Get_Token ) ;
            if( Peek_Token = '.' ) then // Decimal value
            begin
                _Value := _Value + '.' ;
                Get_Token ; // Eat period
            end else
            if( Peek_Token = '(' ) then // Parameter (probably to a macro)
            begin
                _Value := _Value + Get_Token ;
                S := Peek_Token ;
                while( ( S <> '' ) and ( S <> ')' ) ) do
                begin
                    _Value := _Value + Get_Token ;
                    S := Peek_Token ;
                end ;
                if( Peek_Token = ')' ) then
                begin
                    _Value := _Value + ')' ;
                end ;
            end ;
        end ;
        Result := _Value ;
    end ;

var _Operand1 : string ;

    function Operand1 : string ;

    var S : string ;

    begin
        if( length( _Operand1 ) = 0 ) then
        begin
            _Operand1 := uppercase( Get_Token ) ;
            if( Peek_Token = '.' ) then // Decimal value
            begin
                _Operand1 := _Operand1 + '.' ;
                Get_Token ; // Eat period
            end else
            if( Peek_Token = '(' ) then // Parameter (probably to a macro)
            begin
                _Operand1 := _Operand1 + Get_Token ;
                S := Peek_Token ;
                while( ( S <> '' ) and ( S <> ')' ) ) do
                begin
                    _Operand1 := _Operand1 + Get_Token ;
                    S := Peek_Token ;
                end ;
                if( Peek_Token = ')' ) then
                begin
                    _Operand1 := _Operand1 + ')' ;
                    Get_Token ;
                end ;
            end else
            if( _Operand1 = '-' ) then
            begin
                _Operand1 := Operand1 + Get_Token ;
            end ;
        end ;
        Operand1 := _Operand1 ;
    end ;


var A, Aa, AAL : string ;
    B, C : longint ;
    Leading_Space : boolean ;
    P : PChar ;
    PL : integer ;
    Next, Temp : string ;
    UEC : TUnified_Exception ;

label Ae, End_Assemble, Start_Over ;

begin // TF3850_Assembler.Assemble
    // Setup...
    PC := Address ;
    Outputs := nil ;
    Machines := nil ;
    MachineL := 0 ;
    Assembled_Code := '' ;
    Leading_Space := _Master.Leading_Whitespace ;

    // Get line to assemble...
    if( Inputs <> nil ) then
    begin
        Input_Buffer := Inputs ;
    end else
    begin
        P := _Master.Grab_Line( False ) ;
        Input_Buffer := string( P ) ;
    end ;
    Err := False ; { No errors so far }
    fillchar( Result, sizeof( Result ), 0 ) ;

    if(
        ( copy( Input_Buffer, 1, 1 ) = ' ' )
        or
        ( copy( Input_Buffer, 1, 1 ) = HT )
      ) then
    begin
        Leading_Space := True ;
    end ;
    A := Edit( Input_Buffer, 16 or 128 or 256 ) ;
    if( length( A ) = 0 ) then // Blank line
    begin
        goto End_Assemble ;
    end ;
    Input_Buffer := A ;
    A := '' ;

    // Get and process next token...
Start_Over:
    AAL := Get_Token ;
    _Master.Map( PC ) ;
    AA := Edit( AAL, 511 ) ;
    Next := Peek_Token ;
    if( ( AA = 'EQU' ) and ( ( Next = '.' ) or ( Next = '$' ) ) ) then
    begin
        // Handle odd situation where we just have EQU $ - which does nothing
        Get_Token ; // Gobble $ or .
        goto Start_Over ;
    end ;
    if( not Leading_Space ) then // Must be a label
    begin
        _Master.UnMap ;
        AA := AA + ':' ;
        if( Handle_Directives( AA, Status ) ) then { If a directive }
        begin
            A := AA ;
            if( length( Input_Buffer ) = 0 ) then
            begin
                goto End_Assemble ;
            end ;
        end ;
        Leading_Space := True ;
        goto Start_Over ;
    end else
    if( copy( AA, 1, 1 ) = '*' ) then // Comment
    begin
        goto End_Assemble ;
    end else
    if( Next = ':' ) then // A label
    begin
        _Master.UnMap ;
        Get_Token ; // Gobble colon
        AA := AA + ':' ;
        if( Handle_Directives( AA, Status ) ) then { If a directive }
        begin
            A := AA ;
            if( length( Input_Buffer ) > 0 ) then
            begin
                _Master.Put_Token( PChar( Input_Buffer ) ) ;
            end ;
            goto End_Assemble ;
        end ;
        goto Start_Over ;
    end ;
    Temp := AA ;
    if( AA = '.' ) then
    begin
        AA := AA + Input_Buffer ;
        AAL := AAL + Input_Buffer ;
    end else
    begin
        AA := AA + ' ' + Input_Buffer ;
        AAL := AAL + ' ' + Input_Buffer ;
    end ;
    AA := Temp ;

    B := 0 ;
    C := 1 ;
    if( length( AA ) = 0 ) then // Blank line
    begin
        exit ;
    end ;

    { Handle instructions... }
    if( Aa = 'ADC' ) then
    begin
        A := Chr( $8E ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'AI' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < -127 ) or ( C > 255 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $24 ) + chr( C ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'AM' ) then
    begin
        A := Chr( $88 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'AMD' ) then
    begin
        A := Chr( $89 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'AS' ) then
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $C0 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ASD' ) then
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $D0 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( copy( AA, 1, 1 ) = 'B' ) then // Branch
    begin
        AA := copy( AA, 2, length( AA ) ) ;
        if(
            ( AA = 'R' ) or ( AA = 'T' ) or ( AA = 'P' ) or ( AA = 'C' ) or
            ( AA = 'Z' ) or ( AA = 'M' ) or ( AA = 'NC' ) or ( AA = 'NZ' ) or
            ( AA = 'F' ) or ( AA = 'F' ) or ( AA = 'NO' ) or ( AA = 'RZ' )
          ) then
        begin
            A := Operand1 ;
            if( ( AA = 'T' ) or ( AA = 'F' ) ) then
            begin
                if( Value <> ',' ) then
                begin
                    Status.Log_Error( PChar( 'Expected ",", found "' + Value + '"' ), nil, -1, 3 ) ;
                    goto AE ;
                end ;
                _Value := '' ;
                A := Value ;
            end ;
            UEC := Evaluate( A, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            if( UEC <> nil ) then
            begin
                goto AE ;
            end ;
            if( ( C < -127 ) or ( C > 255 ) ) then
            begin
                Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( AA = 'R' ) then
            begin
                A := Chr( $90 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'T' ) then
            begin
                A := Operand1 ;
                UEC := Evaluate( A, B ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    goto AE ;
                end ;
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( A ), -Bit_Range( 0, 3 ), PC ) ;
                    B := 0 ;
                end else
                if( UEC <> nil ) then
                begin
                    goto AE ;
                end ;
                if( ( B < 0 ) or ( B > 7 ) ) then
                begin
                    Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
                    goto AE ;
                end ;
                A := chr( $80 or B ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'P' ) then
            begin
                A := Chr( $81 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'C' ) then
            begin
                A := Chr( $82 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'Z' ) then
            begin
                A := Chr( $84 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'M' ) then
            begin
                A := Chr( $91 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'NC' ) then
            begin
                A := Chr( $92 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'NZ' ) then
            begin
                A := Chr( $94 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'F' ) then
            begin
                A := Operand1 ;
                UEC := Evaluate( A, B ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    goto AE ;
                end ;
                if( UEC <> nil ) then
                begin
                    if( UEC.Get_Error = 4 ) then
                    begin
                        _Master.Add_Reference( PChar( A ), -Bit_Range( 0, 4 ), PC ) ;
                        B := 0 ;
                    end else
                    begin
                        goto AE ;
                    end ;
                end ;
                if( ( B < 0 ) or ( B > 15 ) ) then
                begin
                    Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
                    goto AE ;
                end ;
                A := Chr( $90 or B ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'NO' ) then
            begin
                A := Chr( $98 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end else
            if( AA = 'RZ' ) then
            begin
                A := Chr( $8F ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
            end ;
            goto End_Assemble
        end ;
    end ;
    if( Aa = 'CI' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < -127 ) or ( C > 255 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $25 ) + chr( C ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'CLR' ) then
    begin
        A := Chr( $70 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'CM' ) then
    begin
        A := Chr( $8D ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'COM' ) then
    begin
        A := Chr( $18 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'DCI' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 2, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 65535 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $2A ) + chr( C ) + chr( C shr 8 ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'DI' ) then
    begin
        A := Chr( $1A ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'DS' ) then
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $30 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'EI' ) then
    begin
        A := Chr( $1B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'IN' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 255 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $26 ) + chr( C ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'INC' ) then
    begin
        A := Chr( $1F ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'INS' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), -Bit_Range( 0, 4 ), PC ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 15 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $A0 or C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'JMP' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 2, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 65535 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $29 ) + chr( C ) + chr( C shr 8 ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LI' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < -127 ) or ( C > 255 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $20 ) + chr( C ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LIS' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), -Bit_Range( 0, 4 ), PC ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 15 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $70 or C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'LISL' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), -Bit_Range( 0, 23), PC ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 7 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $68 or C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'LISU' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), -Bit_Range( 0, 23 ), PC ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 7 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $60 or C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'LM' ) then
    begin
        A := Chr( $16 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'LR' ) then
    begin
        A := Operand1 ;
        if( Value <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + Value + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        _Value := uppercase( Get_Token ) ;
        if( A = 'A' ) then // LR A,*
        begin
            if( Value = 'KU' ) then
            begin
                A := chr( 0 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            if( Value = 'KL' ) then
            begin
                A := chr( 1 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            if( Value = 'QU' ) then
            begin
                A := chr( 2 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            if( Value = 'QL' ) then
            begin
                A := chr( 3 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            if( Value = 'IS' ) then
            begin
                A := chr( $A ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            B := Ar( Value ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;
            A := chr( $40 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ; // if( A = 'A' )
        if( A = 'DC' ) then // LR DC, *
        begin
            if( Value = 'Q' ) then
            begin
                A := chr( $F ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            if( Value = 'H' ) then
            begin
                A := chr( $10 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'H' ) then // LR H, DC
        begin
            if( Value = 'DC' ) then
            begin
                A := chr( $11 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'IS' ) then // LR IS, A
        begin
            if( Value = 'A' ) then
            begin
                A := chr( $B ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'J' ) then // LR J, W
        begin
            if( Value = 'W' ) then
            begin
                A := chr( $1E ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'K' ) then // LR K, P
        begin
            if( Value = 'P' ) then
            begin
                A := chr( 8 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'KL' ) then // LR KL, A
        begin
            if( Value = 'A' ) then
            begin
                A := chr( 5 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'KU' ) then // LR KU, A
        begin
            if( Value = 'A' ) then
            begin
                A := chr( 4 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'P' ) then // LR P, K
        begin
            if( Value = 'K' ) then
            begin
                A := chr( 9 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'P0' ) then // LR P0, Q
        begin
            if( Value = 'Q' ) then
            begin
                A := chr( $D ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'Q' ) then // LR Q, DC
        begin
            if( Value = 'DC' ) then
            begin
                A := chr( $E ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'QL' ) then // LR QL, A
        begin
            if( Value = 'A' ) then
            begin
                A := chr( 7 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'QU' ) then // LR QU, A
        begin
            if( Value = 'A' ) then
            begin
                A := chr( 6 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;
        if( A = 'W' ) then // LR W, J
        begin
            if( Value = 'J' ) then
            begin
                A := chr( $1D ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            goto Ae ;
        end ;

        if( Value = 'A' ) then // LR *, A
        begin
            B := Ar( Operand1 ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;
            A := chr( $50 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        goto Ae ;
    end ; // if( Aa = 'LR' )
    if( Aa = 'LNK' ) then
    begin
        A := Chr( $19 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'NI' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < -127 ) or ( C > 255 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $21 ) + chr( C ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'NM' ) then
    begin
        A := Chr( $8A ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'NOP' ) then
    begin
        A := Chr( $2B ) ;
        goto End_Assemble
    end ;
    if( Aa = 'NS' ) then
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $F0 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'OI' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < -127 ) or ( C > 255 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $22 ) + chr( C ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'OM' ) then
    begin
        A := Chr( $8B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'OUT' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 255 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $27 ) + chr( C ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'OUTS' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), -Bit_Range( 0, 4 ), PC ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 15 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $B0 or C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'PI' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < 0 ) or ( C > 65535 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $28 ) + chr( C ) + chr( C shr 8 ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'PK' ) then
    begin
        A := Chr( $C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'POP' ) then
    begin
        A := Chr( $1C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SL' ) then
    begin
        if( Operand1 = '1' ) then
        begin
            A := chr( $13 ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        if( Operand1 = '4' ) then
        begin
            A := chr( $15 ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        goto Ae ;
    end ;
    if( Aa = 'SR' ) then
    begin
        if( Operand1 = '1' ) then
        begin
            A := chr( $12 ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        if( Operand1 = '4' ) then
        begin
            A := chr( $14 ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        goto Ae ;
    end ;
    if( Aa = 'ST' ) then
    begin
        A := Chr( $17 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'XDC' ) then
    begin
        A := Chr( $2C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'XI' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( A ), 1, PC + 1 ) ;
                C := 0 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( C < -127 ) or ( C > 255 ) ) then
        begin
            Status.Log_Error( PChar( 'Value out of range: "' + Operand1 + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        A := Chr( $23 ) + chr( C ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'XM' ) then
    begin
        A := Chr( $8C ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'XS' ) then
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $E0 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Handle_Directives( AAL, Status ) ) then { If a directive }
    begin
        A := AAL ;
	goto End_Assemble ;
    end ;
    if( Length( Aa ) > 0 ) then
    begin
        Status.Log_Error( PChar( 'Unknown instruction: "' + AA + '"' ), nil, -1, 3 ) ;
AE:
        // Unrecognized mnuemonic...
        _Master.Put_Token( PChar( AA ) ) ;
        UEC := _Master.Expand( nil, P, PL, Status ) ;
        if( UEC <> nil ) then
        begin
            Err := True ;
        end else
        begin
            setlength( A, PL ) ;
            move( P[ 0 ], A[ 1 ], PL ) ;
        end ;
    end else
    begin
        if( Status <> nil ) then
        begin
            Status.Code := Status.Code + length( A ) ;
        end ;
    end ;

End_Assemble:
    Assembled_Code := A ; { Return value }
    Machines := PChar( Assembled_Code ) ;
    MachineL := length( Assembled_Code ) ;
    Outputs := nil ;

    Address := PC ;
end ; // TF3850_Assembler.Assemble


{ Returns the default radix (base) of numeric literals. }
function TF3850_Assembler.Default_Radix : longint ;

begin
    Result := Base ;
end ;


{ Returns the default size of numeric literals, in bits. }
function TF3850_Assembler.Default_Size : longint ;

begin
    Default_Size := 8 ;
end ;


{ Returns facility code for this class. }
function TF3850_Assembler.Facility_Code : longint ;

begin
    Result := F3850AssemblerErr_Facility ;
end ;


const _Extensions : string = 'asm' ;

function TF3850_Assembler.Source_Extensions : PChar ;

begin
    Result := PChar( _Extensions ) ;
end ;


const _Valid_Symbol_Initial : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_' ;

function TF3850_Assembler.Valid_Symbol_Initial : PChar ;

begin
    Result := PChar( _Valid_Symbol_Initial ) ;
end ;


const _Valid_Symbol_After : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_0123456789' ;

function TF3850_Assembler.Valid_Symbol_After : PChar ;

begin
    Result := PChar( _Valid_Symbol_After ) ;
end ;


function TF3850_Assembler.Normalize_Expression( Expression : PChar ;
    Numeric : boolean ; Status : TAssembler_Status ) : PChar ;

var C : char ;
    Dummy, Dummy1 : integer ;
    Last_Symbol_Was_Operator : boolean ;
    S, Work, X : string ;

begin
    Result := Expression ;
    if( CEF_Mode ) then
    begin
        exit ; // In CEF-compliant mode, don't process the expression
    end ;
    Last_Symbol_Was_Operator := True ;
    X := string( Expression ) ;
    while( length( X ) > 0 ) do
    begin
        C := X[ 1 ] ;
        if( ( C = '"' ) or ( C = #39 ) ) then // A quote
        begin
            Dummy := 2 ;
            while( ( Dummy < length( X ) ) and ( X[ Dummy ] <> C ) ) do
            begin
                inc( Dummy ) ;
            end ;
            S := S + copy( X, 1, Dummy ) ;
            X := copy( X, Dummy + 1, length( X ) ) ;
            Last_Symbol_Was_Operator := False ;
        end else
        if( ( C >= '0' ) and ( C <= '9' ) ) then // A numeric constant
        begin
            Dummy := 2 ;
            while( ( Dummy <= length( X ) ) and ( X[ Dummy ] in Set_Of_Digits ) ) do
            begin
                inc( Dummy ) ;
            end ;
            if( Dummy < length( X ) ) then // More data in string
            begin
                if( X[ Dummy ] = '.' ) then
                begin
                    inc( Dummy ) ;
                    while( ( Dummy < length( X ) ) and ( X[ Dummy ] <> C ) ) do
                    begin
                        inc( Dummy ) ;
                    end ;
                end ;
            end ;
            if( Dummy < length( X ) ) then // More data in string
            begin
                if( upcase( X[ Dummy ] ) = 'E' ) then // E-notation
                begin
                    inc( Dummy ) ;
                    if( ( X[ Dummy ] = '-' ) or ( X[ Dummy ] = '+' ) ) then
                    begin
                        inc( Dummy ) ;
                    end ;
                    while( ( Dummy <= length( X ) ) and ( X[ Dummy ] in Set_Of_Digits ) ) do
                    begin
                        inc( Dummy ) ;
                    end ;
                end ;
            end ;
            S := S + copy( X, 1, Dummy - 1 ) ;
            X := copy( X, Dummy, length( X ) ) ;
            Last_Symbol_Was_Operator := False ;
        end else
        if( X[ 1 ] = '*' ) then
        begin
            if( copy( X, 2, 1 ) = '*' ) then // **
            begin
                S := S + '^' ;
                X := copy( X, 3, length( X ) ) ;
                Last_Symbol_Was_Operator := True ;
            end else
            begin
                if( Last_Symbol_Was_Operator ) then
                begin
                    S := S + ' . ' ; // A PC reference
                    Last_Symbol_Was_Operator := False ;
                end else
                begin
                    S := S + '*' ;
                    Last_Symbol_Was_Operator := True ;
                end ;
                X := copy( X, 2, length( X ) ) ;
            end ;
        end else
        if( X[ 1 ] in Set_Of_Alpha ) then
        begin
            if( copy( X, 2, 1 ) = #39 ) then // Constant
            begin
                if( pos( X[ 1 ], 'DHBOCT' ) > 0 ) then
                begin
                    Dummy := 3 ;
                    while( ( Dummy <= length( X ) ) and ( X[ Dummy ] <> #39 ) ) do
                    begin
                        inc( Dummy ) ;
                    end ;
                    Work := copy( X, 3, Dummy - 3 ) ;
                    case X[ 1 ] of
                        'D' : S := S + Work + 'D' ;
                        'H' : S := S + Work + 'H' ;
                        'B' : S := S + Work + 'B' ;
                        'O' : S := S + Work + 'O' ;
                        'C' : S := S + #39 + Work + #39 ;
                        'T' : begin // Timer value
                                  try
                                      Dummy1 := strtoint( Work ) ;
                                  except
                                      Status.Log_Error( PChar( 'Illegal timer value: "' + Work + '"' ), nil, -1, 3 ) ;
                                      Dummy1 := 0 ;
                                  end ;
                                  if( ( Dummy1 < 0 ) or ( Dummy1 > 255 ) ) then
                                  begin
                                      Status.Log_Error( PChar( 'timer value out of range: "' + Work + '"' ), nil, -1, 3 ) ;
                                      exit ;
                                  end ;
                                  if( Dummy1 < 255 ) then
                                  begin
                                      Dummy := UnTranslate_Timer( Dummy1 ) ;
                                  end ;
                                  S := S + inttostr( Dummy ) ;
                              end ;
                    end ; // case X[ 1 ]
                    X := copy( X, Dummy + 1, length( X ) ) ;
                end ; // if( pos( X[ 1 ], 'DHBOCT' ) > 0 )
            end else
            begin
                Dummy := 2 ;
                while( ( Dummy <= length( X ) ) and ( X[ Dummy ] in Set_Of_AlphaNum ) ) do
                begin
                    inc( Dummy ) ;
                end ;
                S := S + copy( X, 1, Dummy - 1 ) ;
                X := copy( X, Dummy, length( X ) ) ;
            end ;
            Last_Symbol_Was_Operator := False ;
        end else
        begin
            if( pos( X[ 1 ], '*/()-+^' ) > 0 ) then
            begin
                Last_Symbol_Was_Operator := True ;
            end else
            if( ( X[ 1 ] <> ' ' ) and ( X[ 1 ] <> HT ) ) then
            begin
                Last_Symbol_Was_Operator := False ;
            end ;
            S := S + X[ 1 ] ;
            X := copy( X, 2, length( X ) ) ;
        end ;
    end ; // while( length( X ) > 0 )

    Temp_Normalize_Expression := S ;
    Result := PChar( Temp_Normalize_Expression ) ;
end ;


function TF3850_Assembler.Evaluate( const X : string ;
    var _Result : longint ) : TUnified_Exception ;

var I64 : int64 ;

begin
    Result := _Master.Evaluate( PChar( X ), I64 ) ;
    _Result := I64 ;
end ;


function TF3850_Assembler.Handle_Directives( var Value : string ;
    Status : TAssembler_Status ) : boolean ;

var P : PChar ;
    PL : longint ;
    UEC : TUnified_Exception ;

begin
    // Setup...
    Value := Edit( Value, 128 ) ;
    if( uppercase( Value ) = '.CEF' ) then
    begin
        CEF_Mode := True ;
        Result := True ;
        exit ;
    end ;
    if( uppercase( Value ) = '.F8' ) then
    begin
        CEF_Mode := False ;
        Result := True ;
        exit ;
    end ;

    // Handle directive...
    UEC := _Master.Expand( PChar( Value ), P, PL, Status ) ;
    Handle_Directives := ( UEC = nil ) ;
    setlength( Value, PL ) ;
    move( P[ 0 ], Value[ 1 ], PL ) ;
end ; { Handle_Directives }



end.
