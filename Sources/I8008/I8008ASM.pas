{
              Program Name : I8008
              Package Name : I8008
              Purpose      : 8080/08085/I8008 assembler
              Institution  : Conroy & Conroy
              Date Written : 19-Mar-87
              Written By   : Alan Conroy
              Version      : 1.1

              Copyright (C) 1987 by Alan Conroy.  Released to the public domain.

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

          This unit defines the assembler for the CEF I8008 component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit I8008ASM ;

interface

uses { CEF... }
     _CEF, { TMaster_Assembler }
     CEF, { TBase_Assembler }

     { Other... }
     CommonUt, // TInteger_List
     _UE ; // TUnified_Exception

const I8008AssemblerErr_Facility = 52 ;
      I8008AssemblerErr_Success = 0 ;
      I8008AssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
      I8008AssemblerErr_Illegal_Expression = 2 ; // Illegal expression
      I8008AssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
      I8008AssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol

type TI8008_Assembler = class( TBase_Assembler )
                          public // Instance data...
                              Assembled_Code : string ;
                              Base : integer ;
                              Err : boolean ; { True if an error }
                              _Master : TMaster_Assembler ;
                              PC : integer ; { Current target address }
                              CPU : TCPU ;
                              Segments : TInteger_List ;

                          private // Internal utility routines...
                              procedure Add_Segments( S1, S2 : integer ) ;
                              function Evaluate( X : string ;
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
                      end ;

implementation

uses SysUtils, // strpas

     I8008CPU, // TI8008_CPU
     _ASCII, // CR
     Express,
     Parse, Standard, CVT, Helps, Instrs, Maths, NUM1S, UStrings ;

// TI8008_Assembler methods...

procedure TI8008_Assembler.Initialize( Master : TMaster_Assembler ) ;

begin
    _Master := Master ;
end ;


{ Informs the assembler that the current assembly operation is now
  complete. }
procedure TI8008_Assembler.Terminate ;

begin
    Free ;
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


function Bit_Range( Start, Length : integer ) : integer ;
// Pack Start and Length into a bit range (Start value in low 16 bits, Length value in high 16 bits)

begin
    Result := ( Length shl 16 ) or Start ;
end ;


function TI8008_Assembler.Assemble_Ex( Inputs : PChar ;
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
                    Input_Buffer := Edit( copy( Input_Buffer, Dummy, length( Input_Buffer ) ), 32 ) ;
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
    end ;


    function Grab_Line : string ;

    begin
        Result := Input_Buffer ;
        Input_Buffer := '' ;
    end ;


    function Value : string ; // Get operand 2

    begin
        if( length( _Value ) = 0 ) then
        begin
            _Value := Get_Token ;
            if( Peek_Token = '.' ) then // Decimal value
            begin
                _Value := _Value + '.' ;
                Get_Token ; // Eat period
            end ;
        end ;
        Result := _Value ;
    end ;


    var _Operand1 : string ;

    function Operand1 : string ;

    begin
        if( length( _Operand1 ) = 0 ) then
        begin
            _Operand1 := Get_Token ;
            if( Peek_Token = '.' ) then // Decimal value
            begin
                _Operand1 := _Operand1 + '.' ;
                Get_Token ; // Eat period
            end ;
        end ;
        Operand1 := _Operand1 ;
    end ;


var A, Aa, AAL : string ;
    B, C : longint ;
    Continuation : string ;
    P : PChar ;
    PL : longint ;
    Leading_Space : boolean ;
    Next, Temp : string ;
    Reg2, Reg3 : integer ; // Registers for positions 2 and 3
    _S, _T : integer ;
    UEC : TUnified_Exception ;

label Ae, End_Assemble, Start_Over ;

begin // TI8008_Assembler.Assemble
    // Setup...
    PC := Address ;
    Outputs := nil ;
    Machines := nil ;
    MachineL := 0 ;
    Assembled_Code := '' ;
    Continuation := '' ;
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
                exit ;
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
    if( length( AA ) = 3 ) then // All mnuemonics are 3 characters
    begin
        if( AA = 'DAD' ) then // Define address directive
        begin
            AA := '.DW ' + Input_Buffer ; // Same as Define Word
            Handle_Directives( AA, Status ) ;
            Assembled_Code := AA ; { Return value }
            Machines := PChar( Assembled_Code ) ;
            MachineL := length( Assembled_Code ) ;
            exit ;
        end ;
        if( AA = 'DEF' ) then // Define data
        begin
            AA := '.DB ' + Input_Buffer ; // Same as Define Byte
            Handle_Directives( AA, Status ) ;
            Assembled_Code := AA ; { Return value }
            Machines := PChar( Assembled_Code ) ;
            MachineL := length( Assembled_Code ) ;
            exit ;
        end ;
        Reg2 := pos( AA[ 2 ], 'ABCDEHLM' ) - 1 ;
        Reg3 := pos( AA[ 3 ], 'ABCDEHLM' ) - 1 ;

        if( ( AA[ 1 ] = 'L' ) and ( Reg2 >= 0 ) and (  AA[ 3 ] = 'I' ) ) then // LrI/LMI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( 6 or ( Reg2 shl 3 ) ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble
        end ;
        if( ( AA[ 1 ] = 'L' ) and ( Reg2 >= 0 ) and ( Reg3 >= 0 ) ) then // Lrr/LMr/LrM
        begin
            A := Chr( $C0 or ( Reg2 shl 3 ) or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( ( copy( AA, 1, 2 ) = 'IN' ) and ( Reg3 > 0 ) and ( Reg3 < 7 ) ) then // INr
        begin
            A := Chr( Reg3 shl 3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( ( copy( AA, 1, 2 ) = 'DC' ) and ( Reg3 > 0 ) and ( Reg3 < 7 ) ) then // DCr
        begin
            A := Chr( 1 or ( Reg3 shl 3 ) ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( ( copy( AA, 1, 2 ) = 'AD' ) and ( Reg3 >= 0 ) ) then // ADr
        begin
            A := Chr( $80 or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( AA = 'ADI' ) then // ADI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( 4 ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble
        end ;
        if( ( copy( AA, 1, 2 ) = 'AC' ) and ( Reg3 >= 0 ) ) then // ACr
        begin
            A := Chr( $88 or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( AA = 'ACI' ) then // ACI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $C ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble
        end ;
        if( ( copy( AA, 1, 2 ) = 'SU' ) and ( Reg3 >= 0 ) ) then // SUr
        begin
            A := Chr( $90 or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble ;
        end ;
        if( AA = 'SUI' ) then // SUI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $14 ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble ;
        end ;
        if( ( copy( AA, 1, 2 ) = 'SB' ) and ( Reg3 >= 0 ) ) then // SBr
        begin
            A := Chr( $98 or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( AA = 'SBI' ) then // SBI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $1C ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble ;
        end ;
        if( ( copy( AA, 1, 2 ) = 'ND' ) and ( Reg3 >= 0 ) ) then // NDr
        begin
            A := Chr( $A0 or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble ;
        end ;
        if( AA = 'NDI' ) then // NDI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $24 ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble ;
        end ;
        if( ( copy( AA, 1, 2 ) = 'XR' ) and ( Reg3 >= 0 ) ) then // XRr
        begin
            A := Chr( $A8 or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( AA = 'XRI' ) then // XRI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $2C ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble
        end ;
        if( ( copy( AA, 1, 2 ) = 'OR' ) and ( Reg3 >= 0 ) ) then // ORr
        begin
            A := Chr( $B0 or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( AA = 'ORI' ) then // ORI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $34 ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble ;
        end ;
        if( ( copy( AA, 1, 2 ) = 'CP' ) and ( Reg3 >= 0 ) ) then // CPr
        begin
            A := Chr( $B8 or Reg3 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble ;
        end ;
        if( AA = 'CPI' ) then // CPI
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $3C ) + chr( C ) ;
            Add_Segments( 1, 1 ) ;
            goto End_Assemble
        end ;
        if( AA = 'RLC' ) then // RLC
        begin
            A := Chr( 2 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( AA = 'RRC' ) then // RRC
        begin
            A := Chr( $A ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( AA = 'RAL' ) then // RAL
        begin
            A := Chr( $12 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( AA = 'RAR' ) then // RAR
        begin
            A := Chr( $1A ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'JMP' ) then // JMP
        begin
            UEC := Evaluate( Operand1, B ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            A := Chr( $44 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
            Add_Segments( 1, 2 ) ;
            goto End_Assemble ;
        end ;
        if( copy( Aa, 1, 2 ) = 'JF' ) then // JFc
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            case AA[ 3 ] of
                'C' : B := 0 ;
                'Z' : B := 1 ;
                'S' : B := 2 ;
                'P' : B := 3 ;
                else B := -1 ;
            end ;
            if( B >= 0 ) then
            begin
                A := chr( $40 or ( B shl 3 ) ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
                Add_Segments( 1, 2 ) ;
                goto End_Assemble ;
            end ;
        end ;
        if( copy( Aa, 1, 2 ) = 'JT' ) then // JTc
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            case AA[ 3 ] of
                'C' : B := 0 ;
                'Z' : B := 1 ;
                'S' : B := 2 ;
                'P' : B := 3 ;
                else B := -1 ;
            end ;
            if( B >= 0 ) then
            begin
                A := chr( $60 or ( B shl 3 ) ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
                Add_Segments( 1, 2 ) ;
                goto End_Assemble ;
            end ;
        end ;
        if( Aa = 'CAL' ) then // CAL
        begin
            UEC := Evaluate( Operand1, B ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            A := Chr( $46 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
            Add_Segments( 1, 2 ) ;
            goto End_Assemble ;
        end ;
        if( copy( Aa, 1, 2 ) = 'CF' ) then // CFc
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            case AA[ 3 ] of
                'C' : B := 0 ;
                'Z' : B := 1 ;
                'S' : B := 2 ;
                'P' : B := 3 ;
                else B := -1 ;
            end ;
            if( B >= 0 ) then
            begin
                A := chr( $42 or ( B shl 3 ) ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
                Add_Segments( 1, 2 ) ;
                goto End_Assemble ;
            end ;
        end ;
        if( copy( Aa, 1, 2 ) = 'CT' ) then // CTc
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            case AA[ 3 ] of
                'C' : B := 0 ;
                'Z' : B := 1 ;
                'S' : B := 2 ;
                'P' : B := 3 ;
                else B := -1 ;
            end ;
            if( B >= 0 ) then
            begin
                A := chr( $62 or ( B shl 3 ) ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
                Add_Segments( 1, 2 ) ;
                goto End_Assemble ;
            end ;
        end ;
        if( Aa = 'RET' ) then // RET
        begin
            A := chr( 7 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble ;
        end ;
        if( copy( Aa, 1, 2 ) = 'RF' ) then // RFc
        begin
            case AA[ 3 ] of
                'C' : B := 0 ;
                'Z' : B := 1 ;
                'S' : B := 2 ;
                'P' : B := 3 ;
                else B := -1 ;
            end ;
            if( B >= 0 ) then
            begin
                A := chr( 3 or ( B shl 3 ) ) ;
                Add_Segments( 1, 0 ) ;
                goto End_Assemble ;
            end ;
        end ;
        if( copy( Aa, 1, 2 ) = 'RT' ) then // RTc
        begin
            case AA[ 3 ] of
                'C' : B := 0 ;
                'Z' : B := 1 ;
                'S' : B := 2 ;
                'P' : B := 3 ;
                else B := -1 ;
            end ;
            if( B >= 0 ) then
            begin
                A := chr( $23 or ( B shl 3 ) ) ;
                Add_Segments( 1, 0 ) ;
                goto End_Assemble ;
            end ;
        end ;
        if( AA = 'RST' ) then // RST
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), -Bit_Range( 3, 5 ), PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < 0 ) or ( C > 7 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( 5 or ( C shl 3 ) ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble ;
        end ;
        if( AA = 'INP' ) then // INP
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), -Bit_Range( 1, 3 ), PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < 0 ) or ( C > 7 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $41 or ( C shl 1 ) ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble ;
        end ;
        if( AA = 'OUT' ) then // OUT
        begin
            UEC := Evaluate( Operand1, C ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( UEC.Error_Text( _S, _T ) + ' "' + Operand1 + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), -Bit_Range( 1, 3 ), PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( C < 1 ) or ( C > 31 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $41 or ( C shl 1 ) ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'HLT' ) then // HLT
        begin
            A := chr( 255 ) ;
            Add_Segments( 1, 0 ) ;
            goto End_Assemble ;
        end ;
    end ;
    if Length( Aa ) > 0 then
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
end ; // TI8008_Assembler.Assemble


{ Returns the default radix (base) of numeric literals. }
function TI8008_Assembler.Default_Radix : longint ;

begin
    Result := Base ;
end ;


{ Returns the default size of numeric literals, in bits. }
function TI8008_Assembler.Default_Size : longint ;

begin
    Default_Size := 8 ;
end ;


{ Returns facility code for this class. }
function TI8008_Assembler.Facility_Code : longint ;

begin
    Result := I8008AssemblerErr_Facility ;
end ;


const _Extensions : string = 'asm' ;

function TI8008_Assembler.Source_Extensions : PChar ;

begin
    Result := PChar( _Extensions ) ;
end ;


const _Valid_Symbol_Initial : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_' ;

function TI8008_Assembler.Valid_Symbol_Initial : PChar ;

begin
    Result := PChar( _Valid_Symbol_Initial ) ;
end ;


const _Valid_Symbol_After : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_0123456789' ;

function TI8008_Assembler.Valid_Symbol_After : PChar ;

begin
    Result := PChar( _Valid_Symbol_After ) ;
end ;


procedure TI8008_Assembler.Add_Segments( S1, S2 : integer ) ;

begin
    Segments.Clear ;
    Segments.Add( S1 * 8 ) ;
    Segments.Add( S2 * 8 ) ;
end ;


function TI8008_Assembler.Evaluate( X : string ; var _Result : longint ) : TUnified_Exception ;

var I64 : int64 ;

begin
    Result := _Master.Evaluate( PChar( X ), I64 ) ;
    _Result := I64 ;
end ; { TI8008_Assembler.Evaluate }


function TI8008_Assembler.Handle_Directives( var Value : string ;
    Status : TAssembler_Status ) : boolean ;

var P : PChar ;
    PL : longint ;
    UEC : TUnified_Exception ;

begin
    // Setup...
    Value := Edit( Value, 128 ) ;

    // Handle directive...
    UEC := _Master.Expand( PChar( Value ), P, PL, Status ) ;
    Handle_Directives := ( UEC = nil ) ;
    setlength( Value, PL ) ;
    move( P[ 0 ], Value[ 1 ], PL ) ;
end ; { Handle_Directives }


end.
