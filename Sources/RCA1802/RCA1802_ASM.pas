{
              Program Name : RCA1802
              Package Name : RCA1802
              Purpose      : RCA 1802 CPU assembler
              Institution  : Conroy & Conroy
              Date Written : 19-Jan-2007
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

          This unit defines the assembler for the CEF RCA 1802 component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit RCA1802_ASM ;

interface

uses { CEF... }
    _CEF, // TMaster_Assembler
     CEF, { TBase_Assembler }

     { Other... }
     _UE ; // TUnified_Exception

const RCA1802AssemblerErr_Facility = 52 ;
      RCA1802AssemblerErr_Success = 0 ;
      RCA1802AssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
      RCA1802AssemblerErr_Illegal_Expression = 2 ; // Illegal expression
      RCA1802AssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
      RCA1802AssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol

type TMode = ( M_1802, M_1806 ) ;

type TRCA1802_Assembler = class( TBase_Assembler )
                          public // Instance data...
                              Assembled_Code : string ;
                              Base : integer ;
                              Err : boolean ; { True if an error }
                              _Master : TMaster_Assembler ;
                              PC : integer ; { Current target address }
                              CPU : TCPU ;

                          protected
                              function Evaluate( const X : string ;
                                  var _Result : longint ;
                                  PC_Adjustment : integer ) : TUnified_Exception ;
                                  override ;

                          private // Internal utility routines...
                              function Handle_Directives( var Value : string ;
                                  Status : TAssembler_Status ) : boolean ;

                          public // API...
                              Mode : TMode ;

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

                              function Backpatching( Name : PChar ;
                                  Address : int64 ; var Value : int64 ;
                                  var Size : longint ;
                                  Context, Options, Line : longint ;
                                  Filename : PChar ;
                                  Status : TAssembler_Status ) : boolean ;
                                  override ;
                      end ; // TRCA1802_Assembler

implementation

uses // Borland...
     SysUtils, // strpas

     // C&C...
     ASCIIDef, // CR
     CommonUt, // Edit
     CVT, // CVTB
     Express,
     Parse,
     Standard,
     Helps,
     Instrs,
     Maths,
     NUM1S,
     UStrings,

     // RCA1802...
     RCA1802_CPU ; // TRCA1802

// TRCA1802_Assembler methods...

procedure TRCA1802_Assembler.Initialize( Master : TMaster_Assembler ) ;

begin
    _Master := Master ;
end ;


{ Informs the assembler that the current assembly operation is now
  complete. }
procedure TRCA1802_Assembler.Terminate ;

begin
    Free ;
end ;


function Ar( S : string ) : integer ;

begin
    // Setup and sdanity checks...
    Result := -1 ; // Assume failure
    S := Edit( S, -1 ) ;
    if( ( length( S ) < 2 ) or ( length( S ) > 3 ) ) then
    begin
        exit ;
    end ;

    // Parse the value...
    if( S[ 1 ] <> 'R' ) then
    begin
        exit ;
    end ;
    S := copy( S, 2, length( S ) ) ;
    try
        Result := strtoint( S ) ;
        if( ( Result < 0 ) or ( Result > 15 ) ) then
        begin
            Result := -1 ; // Out of range
        end ;
    except
        Result := -1 ;
    end ;
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


function TRCA1802_Assembler.Assemble_Ex( Inputs : PChar ;
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
            _Value := Get_Token ;
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


    function Range_Value( Operand1 : string ; Low, High : integer ;
        var B : longint ; Size, Offset : integer ) : boolean ;

    var UEC : TUnified_Exception ;
        Work, Work1 : string ;

    begin
        Work := Grab_Line ;
        Work1 := Work ;
        Work := Parse_Parameter( ';', Work1 ) ;
        UEC := Evaluate( Work, B, 0 ) ;
        Range_Value := True ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + Work + '"' ), nil, -1, 3 ) ;
            exit ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then // Undefined symbol
            begin
                _Master.Add_Reference( PChar( Operand1 ), Size, PC + Offset ) ;
            end else
            begin
                Range_Value := False ;
            end ;
        end else
        if( ( B < Low ) or ( B > High ) ) then
        begin
            Range_Value := False ;
        end ;
    end ;

var _Operand1 : string ;

    function Operand1 : string ;

    var S : string ;

    begin
        if( length( _Operand1 ) = 0 ) then
        begin
            _Operand1 := Get_Token ;
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
                end ;
            end ;
        end ;
        Operand1 := _Operand1 ;
    end ;


var A, Aa, AAL : string ;
    B, C : longint ;
    Continuation : string ;
    P : PChar ;
    PL : integer ;
    Next, Temp : string ;
    S : string ;
    UEC : TUnified_Exception ;

label Ae, End_Assemble, Start_Over ;

begin // TRCA1802_Assembler.Assemble
    // Setup...
    PC := Address ;
    Outputs := nil ;
    Machines := nil ;
    MachineL := 0 ;
    Assembled_Code := '' ;
    Continuation := '' ;

    // Get line to assemble...
    if( Inputs <> nil ) then
    begin
        Input_Buffer := Inputs ;
    end else
    begin
        P := _Master.Grab_Line( False ) ;
        Input_Buffer := string( P ) ;
        S := Parse_Parameter( ';', Input_Buffer ) ;
        Input_Buffer := Parse_Parameter( '\', S ) ;
        if( S <> '' ) then
        begin
            Continuation := '\' + S ;
        end ;
    end ;
    Err := False ; { No errors so far }
    fillchar( Result, sizeof( Result ), 0 ) ;

    // Remove any comments...
    A := Edit( Parse_Parameter( ';', Input_Buffer ), 16 or 128 or 256 ) ;
    if( length( A ) = 0 ) then // Blank line (or comment only)
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
    AAL := AAL + Continuation ;
    if( Handle_Directives( AAL, Status ) ) then { If a directive }
    begin
        A := AAL ;
	goto End_Assemble ;
    end ;
    if( length( Continuation ) > 0 ) then
    begin
        _Master.Put_Token( PChar( Continuation ) ) ;
    end ;
    AA := Temp ;

    B := 0 ;
    C := 1 ;
    while( C <= Length( AA ) ) do
    begin
        if( ( AA[ C ] = ';' ) and ( B = 0 ) ) then // A comment
	begin
	    Set_Length( AA, C - 1 ) ;
	end ;
        if( copy( AA, C, 2 ) = '..' ) then // A comment
        begin
	    Set_Length( AA, C - 1 ) ;
        end ;
        C := C + 1 ;
    end ; // while( C <= Length( AA ) )
    if( B = -1 ) then
    begin
        Status.Log_Error( 'Unterminated quote', nil, -1, 2 ) ;
        goto End_Assemble ;
    end ;
    if( length( AA ) = 0 ) then // Blank line
    begin
        exit ;
    end ;

    { Handle instructions... }
    if( Mode = M_1806 ) then
    begin
        if( Aa = 'STPC' ) then { Stop Counter }
        begin
            A := Chr( $68 ) + chr( 0 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'DTC' ) then { Decrement timer/counter }
        begin
            A := Chr( $68 ) + chr( 1 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'SPM2' ) then { Set pulse width mode 2 and start }
        begin
            A := Chr( $68 ) + chr( 2 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'SCM2' ) then { Set counter mode 2 and start }
        begin
            A := Chr( $68 ) + chr( 3 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'SPM1' ) then { Set pulse width mode 1 and start }
        begin
            A := Chr( $68 ) + chr( 4 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'SCM1' ) then { Set counter mode 1 and start }
        begin
            A := Chr( $68 ) + chr( 5 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'LDC' ) then { Load counter }
        begin
            A := Chr( $68 ) + chr( 6 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'STM' ) then { Set Timer mode and start }
        begin
            A := Chr( $68 ) + chr( 7 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'GEC' ) then { Get Counter }
        begin
            A := Chr( $68 ) + chr( 8 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'ETQ' ) then { Enable toggle Q }
        begin
            A := Chr( $68 ) + chr( 9 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'XIE' ) then { External interrupt enable }
        begin
            A := Chr( $68 ) + chr( $A ) ;
            goto End_Assemble
        end ;
        if( Aa = 'XID' ) then { External interrupt disable }
        begin
            A := Chr( $68 ) + chr( $B ) ;
            goto End_Assemble
        end ;
        if( Aa = 'CIE' ) then { Counter interrupt enable }
        begin
            A := Chr( $68 ) + chr( $C ) ;
            goto End_Assemble
        end ;
        if( Aa = 'CID' ) then { Counter interrupt disable }
        begin
            A := Chr( $68 ) + chr( $D ) ;
            goto End_Assemble
        end ;
        if( Aa = 'BCI' ) then { Short branch on counter interrupt }
        begin
            A := Operand1 ;
            UEC := Evaluate( A, C, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference_Ex( PChar( A ), 1, PC + 1, 1, 0 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            A := Chr( $68 ) + chr( $3E ) + chr( C ) ;
            goto End_Assemble
        end ;
        if( Aa = 'BXI' ) then { Short branch on external interrupt }
        begin
            A := Operand1 ;
            UEC := Evaluate( A, C, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference_Ex( PChar( A ), 1, PC + 1, 1, 0 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            A := Chr( $68 ) + chr( $3F ) + chr( C ) ;
            goto End_Assemble
        end ;
        if( Aa = 'DADC' ) then { Decimal Add with Carry }
        begin
            A := Chr( $68 ) + chr( $74 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'DSAV' ) then { Save T, D, DF }
        begin
            A := Chr( $68 ) + chr( $76 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'DSMB' ) then { Decimal subtract Memory with Borrow }
        begin
            A := Chr( $68 ) + chr( $77 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'DADD' ) then { Decimal Add }
        begin
            A := Chr( $68 ) + chr( $F4 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'DSM' ) then { Decimal Subtract memory }
        begin
            A := Chr( $68 ) + chr( $F7 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'DACI' ) then { Decimal add with carry, immediate }
        begin
            A := Operand1 ;
            UEC := Evaluate( A, C, 0 ) ;
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
            A := chr( $68 ) + Chr( $7C ) + Chr( C ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'DSBI' ) then { Decimal Subtract Memory with Borrow Immediate }
        begin
            A := Operand1 ;
            UEC := Evaluate( A, C, 0 ) ;
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
            A := chr( $68 ) + Chr( $7F ) + Chr( C ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'DADI' ) then { Decimal Add Immediate }
        begin
            A := Operand1 ;
            UEC := Evaluate( A, C, 0 ) ;
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
            A := chr( $68 ) + Chr( $FC ) + Chr( C ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'DSMI' ) then { Decimal subtract memory, immediate }
        begin
            A := Operand1 ;
            UEC := Evaluate( A, C, 0 ) ;
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
            A := chr( $68 ) + Chr( $FF ) + Chr( C ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'DBNZ' ) then { Decrement register N and long branch is not equal 0 }
        begin
            B := Ar( Operand1 ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;

            A := Get_Token ;
            if( A <> ',' ) then
            begin
                Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
                Err := True ;
                goto AE ;
            end ;

            A := Value ;
            UEC := Evaluate( A, C, 0 ) ;
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
            A := chr( $68 ) + chr( $20 or B ) + chr( hi( C ) ) + chr( lo( C ) ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'RLXA' ) then { Register Load via X and advance }
        begin
            B := Ar( Operand1 ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;
            A := chr( $68 ) + chr( $60 or B ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'SCAL' ) then { Standard call }
        begin
            B := Ar( Operand1 ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;
            A := chr( $68 ) + chr( $80 or B ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'SRET' ) then { Standard return }
        begin
            B := Ar( Operand1 ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;
            A := chr( $68 ) + chr( $90 or B ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'RSXD' ) then { Register store via X and decrement }
        begin
            B := Ar( Operand1 ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;
            A := chr( $68 ) + chr( $A0 or B ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'RNX' ) then { Register N to register X copy }
        begin
            B := Ar( Operand1 ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;
            A := chr( $68 ) + chr( $B0 or B ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'RLDI' ) then { Register Load Immediate }
        begin
            B := Ar( Operand1 ) ;
            if( B = -1 ) then
            begin
                goto Ae ;
            end ;

            A := Get_Token ;
            if( A <> ',' ) then
            begin
                Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
                Err := True ;
                goto AE ;
            end ;

            A := Value ;
            UEC := Evaluate( A, C, 0 ) ;
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
            A := chr( $68 ) + chr( $C0 or B ) + chr( hi( C ) ) + chr( lo( C ) ) ;
            goto End_Assemble ;
        end ;
    end ; // if( Mode = M_1806 )
    if( Aa = 'ADC' ) then { Add with carry }
    begin
        A := Chr( $74 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'ADCI' ) then { Add Immediate with carry }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $7C ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ADD' ) then { Add }
    begin
        A := Chr( $F4 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'ADI' ) then { Add Immediate }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $FC ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'AND' ) then { And }
    begin
        A := Chr( $F2 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'ANI' ) then { And Immediate }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $FA ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'DEC' ) then { Decrement Reg N }
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $20 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'DIS' ) then // Disable and return
    begin
        A := Chr( $71 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'GHI' ) then { Get High }
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $90 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'GLO' ) then { Get Low }
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $80 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'IDL' ) then { Wait }
    begin
        A := Chr( 0 ) ;
        goto End_Assemble
    end ;
    if( ( Aa = 'IN' ) or ( Aa = 'INP' ) ) then { Input }
    begin
        try
            B := strtoint( Operand1 ) ;
        except
            B := -1 ;
        end ;
        if( ( B = -1 ) or ( B < 1 ) or ( B > 7 ) ) then
        begin
            goto Ae ;
        end ;
        A := chr( $60 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'INC' ) then { Increment Reg N }
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $10 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'IRX' ) then { Increment Reg X }
    begin
        A := chr( $60 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LDA' ) then { Load and Advance }
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $40 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LDI' ) then { Load Immediate }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $F8 ) + chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LDN' ) then { Load via N }
    begin
        B := Ar( Operand1 ) ;
        if( ( B = -1 ) or ( B = 0 ) ) then
        begin
            goto Ae ;
        end ;
        A := chr( B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LDX' ) then { Load via X }
    begin
        A := Chr( $F0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LDXA' ) then { Load via X and Advance }
    begin
        A := Chr( $72 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'MARK' ) then // Push X, P to stack
    begin
        A := Chr( $79 ) ;
        goto End_Assemble
    end ;
    if( ( Aa = 'NBR' ) or ( AA = 'SKP' ) ) then { Skip }
    begin
        A := Chr( $38 ) ;
        goto End_Assemble
    end ;
    if( ( Aa = 'NLBR' ) or ( AA = 'LSKP' ) ) then // Long skip
    begin
        A := Chr( $C8 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'NOP' ) then // No-op
    begin
        A := Chr( $C4 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'OR' ) then { Or }
    begin
        A := Chr( $F1 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'ORI' ) then { Or Immediate }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $F9 ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'OUT' ) then { Output }
    begin
        try
            B := strtoint( Operand1 ) ;
        except
            B := -1 ;
        end ;
        if( ( B = -1 ) or ( B < 1 ) or ( B > 7 ) ) then
        begin
            goto Ae ;
        end ;
        A := chr( $60 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PHI' ) then { Put High }
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $B0 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PLO' ) then { Put Low }
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $A0 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'REQ' ) then // Reset Q
    begin
        A := Chr( $7A ) ;
        goto End_Assemble
    end ;
    if( Aa = 'RET' ) then // Return
    begin
        A := Chr( $70 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SAV' ) then // Save
    begin
        A := Chr( $78 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SD' ) then { Subtract D }
    begin
        A := Chr( $F5 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SDB' ) then { Subtract with borrow }
    begin
        A := Chr( $75 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SDBI' ) then { Subtract Immediate with borrow }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $7D ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SDI' ) then { Subtract Immediate }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $FD ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SEP' ) then // Set P
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $D0 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SEQ' ) then // Set Q
    begin
        A := Chr( $7B ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SEX' ) then // Set X
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $E0 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SHL' ) then { Shift left }
    begin
        A := Chr( $FE ) ;
        goto End_Assemble
    end ;
    if( ( Aa = 'SHLC' ) or ( AA = 'RSHL' ) ) then { Shift right with carry }
    begin
        A := Chr( $7E ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SHR' ) then { Shift right }
    begin
        A := Chr( $F6 ) ;
        goto End_Assemble
    end ;
    if( ( Aa = 'SHRC' ) or ( AA = 'RSHR' ) ) then { Shift right with carry }
    begin
        A := Chr( $76 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SM' ) then { Subtract memory }
    begin
        A := Chr( $F7 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SMI' ) then { Subtract Memory Immediate }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $FF ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SMB' ) then { Subtract memory with borrow }
    begin
        A := Chr( $77 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'SMBI' ) then { Subtract Memory Immediate with borrow }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $7F ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'STR' ) then { Store via N }
    begin
        B := Ar( Operand1 ) ;
        if( B = -1 ) then
        begin
            goto Ae ;
        end ;
        A := chr( $50 or B ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'STXD' ) then { Store via X and Decrement }
    begin
        A := Chr( $73 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'XOR' ) then { Xor }
    begin
        A := Chr( $F3 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'XRI' ) then { Xor Immediate }
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        A := Chr( $FB ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa[ 1 ] = 'B' ) then { Short branch }
    begin
        AA := copy( AA, 2, length( AA ) ) ;
        if( ( AA <> 'Z' ) and ( AA <> 'NZ' ) and ( AA <> 'DF' ) and
            ( AA <> 'PZ' ) and ( AA <> 'GE' ) and ( AA <> 'NF' ) and
            ( AA <> 'M' ) and ( AA <> 'L' ) and ( AA <> 'Q' ) and
            ( AA <> 'NQ' ) and ( AA <> '1' ) and ( AA <> 'N1' ) and
            ( AA <> '2' ) and ( AA <> 'N2' ) and
            ( AA <> '3' ) and ( AA <> 'N3' ) and
            ( AA <> '4' ) and ( AA <> 'N4' ) and ( AA <> 'R' )
          ) then
        begin
            goto AE ;
        end ;
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        if( AA = 'Z' ) then
        begin
            A := chr( $32 ) + chr( C ) ;
        end else
        if( AA = 'NZ' ) then
        begin
            A := chr( $3A ) + chr( C ) ;
        end else
        if( ( AA = 'DF' ) or ( AA = 'PZ' ) or ( AA = 'GE' ) ) then
        begin
            A := chr( $33 ) + chr( C ) ;
        end else
        if( ( AA = 'NF' ) or ( AA = 'M' ) or ( AA = 'L' ) or ( AA = 'Q' ) ) then
        begin
            A := chr( $3B ) + chr( C ) ;
        end else
        if( AA = 'NQ' ) then
        begin
            A := chr( $39 ) + chr( C ) ;
        end else
        if( AA = '1' ) then
        begin
            A := chr( $34 ) + chr( C ) ;
        end else
        if( AA = 'N1' ) then
        begin
            A := chr( $3C ) + chr( C ) ;
        end else
        if( AA = '2' ) then
        begin
            A := chr( $35 ) + chr( C ) ;
        end else
        if( AA = 'N2' ) then
        begin
            A := chr( $3D ) + chr( C ) ;
        end else
        if( AA = '3' ) then
        begin
            A := chr( $36 ) + chr( C ) ;
        end else
        if( AA = 'N3' ) then
        begin
            A := chr( $3E ) + chr( C ) ;
        end else
        if( AA = '4' ) then
        begin
            A := chr( $37 ) + chr( C ) ;
        end else
        if( AA = 'N4' ) then
        begin
            A := chr( $3F ) + chr( C ) ;
        end else
        if( Aa = 'R' ) then // BR
        begin
            A := chr( $30 ) + chr( C ) ;
        end ;
        goto End_Assemble ;
    end ;
    if( copy( Aa, 1, 2 ) = 'LS' ) then { Long skip }
    begin
        AA := copy( AA, 3, length( AA ) ) ;
        if( ( AA <> 'Z' ) and ( AA <> 'NZ' ) and ( AA <> 'DF' ) and
            ( AA <> 'NF' ) and ( AA <> 'Q' ) and
            ( AA <> 'NQ' ) and ( AA <> 'IE' )
          ) then
        begin
            goto AE ;
        end ;
        if( AA = 'Z' ) then
        begin
            A := chr( $CE ) ;
        end else
        if( AA = 'NZ' ) then
        begin
            A := chr( $C6 ) ;
        end else
        if( AA = 'DF' ) then
        begin
            A := chr( $CF ) ;
        end else
        if( AA = 'NF' ) then
        begin
            A := chr( $C7 ) ;
        end else
        if( AA = 'IE' ) then
        begin
            A := chr( $CC ) ;
        end else
        if( AA = 'Q' ) then
        begin
            A := chr( $CD ) ;
        end else
        if( AA = 'NQ' ) then
        begin
            A := chr( $C5 ) ;
        end ;
        goto End_Assemble ;
    end ;
    if( copy( Aa, 1, 2 ) = 'LB' ) then { Long branch }
    begin
        AA := copy( AA, 3, length( AA ) ) ;
        if( ( AA <> 'Z' ) and ( AA <> 'NZ' ) and ( AA <> 'DF' ) and
            ( AA <> 'NF' ) and ( AA <> 'Q' ) and
            ( AA <> 'NQ' ) and ( AA <> 'R' )
          ) then
        begin
            goto AE ;
        end ;
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
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
        if( AA = 'R' ) then
        begin
            A := chr( $C0 ) + chr( hi( C ) ) + chr( lo( C ) ) ;
        end else
        if( AA = 'Z' ) then
        begin
            A := chr( $C2 ) + chr( hi( C ) ) + chr( lo( C ) ) ;
        end else
        if( AA = 'NZ' ) then
        begin
            A := chr( $CA ) + chr( hi( C ) ) + chr( lo( C ) ) ;
        end else
        if( AA = 'DF' ) then
        begin
            A := chr( $C3 ) + chr( hi( C ) ) + chr( lo( C ) ) ;
        end else
        if( AA = 'NF' ) then
        begin
            A := chr( $CB ) + chr( hi( C ) ) + chr( lo( C ) ) ;
        end else
        if( AA = 'Q' ) then
        begin
            A := chr( $C1 ) + chr( hi( C ) ) + chr( lo( C ) ) ;
        end else
        if( AA = 'NQ' ) then
        begin
            A := chr( $C9 ) + chr( hi( C ) ) + chr( lo( C ) ) ;
        end ;
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
end ; // TRCA1802_Assembler.Assemble


{ Returns the default radix (base) of numeric literals. }
function TRCA1802_Assembler.Default_Radix : longint ;

begin
    Result := Base ;
end ;


{ Returns the default size of numeric literals, in bits. }
function TRCA1802_Assembler.Default_Size : longint ;

begin
    Default_Size := 8 ;
end ;


{ Returns facility code for this class. }
function TRCA1802_Assembler.Facility_Code : longint ;

begin
    Result := RCA1802AssemblerErr_Facility ;
end ;


const _Extensions : string = 'asm' ;

function TRCA1802_Assembler.Source_Extensions : PChar ;

begin
    Result := PChar( _Extensions ) ;
end ;


const _Valid_Symbol_Initial : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_' ;

function TRCA1802_Assembler.Valid_Symbol_Initial : PChar ;

begin
    Result := PChar( _Valid_Symbol_Initial ) ;
end ;


const _Valid_Symbol_After : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_0123456789' ;

function TRCA1802_Assembler.Valid_Symbol_After : PChar ;

begin
    Result := PChar( _Valid_Symbol_After ) ;
end ;


function TRCA1802_Assembler.Backpatching( Name : PChar ; Address : int64 ;
    var Value : int64 ; var Size : longint ;
    Context, Options, Line : longint ; Filename : PChar ;
    Status : TAssembler_Status ) : boolean ;

begin
    Result := True ; // Assume success
    if( ( Size = 1 ) and ( Context = 1 ) ) then
    begin
        if( ( Address and $FF00 ) <> ( Value and $FF00 ) ) then
        begin
            Status.Log_Error( 'Short branch into different page', Filename, Line, 3 ) ;
        end ;
    end ;
end ;


function TRCA1802_Assembler.Evaluate( const X : string ; var _Result : longint ;
    PC_Adjustment : integer ) : TUnified_Exception ;

var I64 : int64 ;

begin
    Result := _Master.Evaluate( PChar( X ), I64 ) ;
    _Result := I64 ;
end ; { TRCA1802_Assembler.Evaluate }


function TRCA1802_Assembler.Handle_Directives( var Value : string ;
    Status : TAssembler_Status ) : boolean ;

var P : PChar ;
    PL : longint ;
    UEC : TUnified_Exception ;

begin
    // Setup...
    Value := Edit( Value, 128 ) ;

    // Handle directive...
    if( Value = '.1802' ) then
    begin
        Mode := M_1802 ;
        TRCA1802_CPU( CPU ).Mode := Mode ;
	Handle_Directives := True ;
        _Master.Expand( '.DEFINE CDP1802', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE CDP1806', P, PL, Status ) ;
        Value := '' ;
    end else
    if( Value = '.1806' ) then
    begin
        Mode := M_1806 ;
        TRCA1802_CPU( CPU ).Mode := Mode ;
	Handle_Directives := True ;
        _Master.Expand( '.DEFINE CDP1806', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE CDP1802', P, PL, Status ) ;
        Value := '' ;
    end else
    begin
        UEC := _Master.Expand( PChar( Value ), P, PL, Status ) ;
        Handle_Directives := ( UEC = nil ) ;
        setlength( Value, PL ) ;
        move( P[ 0 ], Value[ 1 ], PL ) ;
    end ;
end ; { Handle_Directives }



end.
