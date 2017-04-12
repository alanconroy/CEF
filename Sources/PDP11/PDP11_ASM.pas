{
              Program Name : PDP11
              Package Name : PDP11
              Purpose      : PDP-11 CPU assembler
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

          This unit defines the assembler for the CEF PDP11 component.  Note:
        The assembler will not work correctly under all circumstances with any
        version of a master assembler below V2.2.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit PDP11_ASM ;

interface

uses { CEF... }
     _CEF, // TMaster_Assembler
     CEF, { TBase_Assembler }

     { Other... }
     _UE ; // TUnified_Exception

const PDP11AssemblerErr_Facility = 52 ;
      PDP11AssemblerErr_Success = 0 ;
      PDP11AssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
      PDP11AssemblerErr_Illegal_Expression = 2 ; // Illegal expression
      PDP11AssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
      PDP11AssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol

type TPDP11_Assembler = class( TBase_Assembler )
                          public // Instance data...
                              Assembled_Code : string ;
                              Base : integer ;
                              Err : boolean ; { True if an error }
                              _Master : TMaster_Assembler ;
                              PC : integer ; { Current target address }
                              CPU : TCPU ;
                              CPU_Parent : TComponent ;
                              CEF_Mode : boolean ; // True if in CEF-compliant mode
                              Temp_Normalize_Expression : string ;

                          private // Internal utility routines...
                              function Handle_Directives( var Value : string ;
                                  Status : TAssembler_Status ) : boolean ;
                              procedure Log_Error( Status : TAssembler_Status ;
                                  const Message : string ; Level : integer ) ;

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

                              function Normalize_Expression( Expression : PChar ;
                                  Numeric : boolean ;
                                  Status : TAssembler_Status ) : PChar ; override ;

                              function Source_Extensions : PChar ; override ;

                              function Valid_Symbol_Initial : PChar ; override ;

                              function Valid_Symbol_After : PChar ; override ;

                              function Evaluate( const X : string ;
                                  var _Result : longint ; PC_Adjustment : integer ) : TUnified_Exception ;
                                  override ;
                      end ; // TPDP11_Assembler

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
     Radix50s, // Rad50
     TypeDefs, // Set_Of_Digits
     UStrings,

     // PDP11...
     PDP11_CPU, // TPDP11
     PDP11_Util ; // Octal

// TPDP11_Assembler methods...

procedure TPDP11_Assembler.Initialize( Master : TMaster_Assembler ) ;

begin
    _Master := Master ;

    CEF_Mode := False ;
end ;


{ Informs the assembler that the current assembly operation is now
  complete. }
procedure TPDP11_Assembler.Terminate ;

begin
    Free ;
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


function TPDP11_Assembler.Assemble_Ex( Inputs : PChar ;
    var Outputs, Machines : PChar ;
    var MachineL : longint ; var Address : int64 ;
    var Segment : longint ; Status : TAssembler_Status ;
    Flags : longint ) : TUnified_Exception ;

var _Value : string ;
    Input_Buffer : string ;

    function _Next_Token( Peek : boolean ) : string ;

    var C : char ;
        Dummy : integer ;
        _Input_Buffer : string ;
        Only_Numbers : boolean ;

    begin
        _Input_Buffer := Edit( Input_Buffer, 8 ) ; // Ignore leading spaces...
        try
            if( length( _Input_Buffer ) > 0 ) then
            begin
                C := _Input_Buffer[ 1 ] ;
            end else
            begin
                C := ' ' ;
            end ;
            if( C = '"' ) then
            begin
                Dummy := Instr( 2, _Input_Buffer + C, C ) ;
                Result := copy( _Input_Buffer, 1, Dummy ) ;
                _Input_Buffer := copy( _Input_Buffer, Dummy + 1, length( _Input_Buffer ) ) ;
                exit ;
            end ; // if( C = '"' )
            Only_Numbers := True ;
            for Dummy := 1 to length( _Input_Buffer ) do
            begin
                if( pos( _Input_Buffer[ Dummy ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$_' ) = 0 ) then
                begin
                    if( Dummy = 1 ) then
                    begin
                        Result := _Input_Buffer[ 1 ] ;
                        _Input_Buffer := copy( _Input_Buffer, 2, length( _Input_Buffer ) ) ;
                    end else
                    begin
                        if( ( _Input_Buffer[ Dummy ] = '.' ) and Only_Numbers ) then
                        begin
                            continue ;
                        end ;
                        Result := copy( _Input_Buffer, 1, Dummy - 1 ) ;
                        _Input_Buffer := copy( _Input_Buffer, Dummy, length( _Input_Buffer ) ) ;
                    end ;
                    exit ;
                end ;
                if( pos( _Input_Buffer[ Dummy ], '0123456789' ) = 0 ) then
                begin
                    Only_Numbers := False ;
                end ;
            end ; // for Dummy := 1 to length( Input_Buffer )
            Result := _Input_Buffer ;
            _Input_Buffer := '' ;
        finally
            if( not Peek ) then
            begin
                Input_Buffer := _Input_Buffer ;
            end ;
        end ;
    end ; // _Next_Token


    function Get_Token : string ;

    begin
        Result := _Next_Token( False ) ;
    end ;


    function Peek_Token : string ;

    begin
        Result := _Next_Token( True ) ;
    end ;


    procedure Put_Token( const S : string ) ;

    begin
        Input_Buffer := S + ' ' + Input_Buffer ;
    end ;


    function Grab_Line : string ;

    begin
        Result := Input_Buffer ;
        Input_Buffer := '' ;
    end ;


    function Get_Operand : string ;

    var Dummy : integer ;
        S : string ;

        procedure Get_Parentheses ;

        begin
            Result := Result + Get_Token ;
            S := Peek_Token ;
            while( ( S <> '' ) and ( S <> ')' ) ) do
            begin
                Result := Result + Get_Token ;
                S := Peek_Token ;
            end ;
            if( Peek_Token = ')' ) then
            begin
                Result := Result + ')' ;
                Get_Token ;
            end ;
        end ;

    begin
        Result := uppercase( Get_Token ) ;
        if( Result = ';' ) then
        begin
            Result := '' ;
            Input_Buffer := '' ; // Comment ends line
            exit ;
        end ;
        if( Peek_Token = '.' ) then // Decimal value
        begin
            Result := Result + '.' ;
            Get_Token ; // Eat period
        end else
        if( Peek_Token = '(' ) then // Parameter (probably to a macro)
        begin
            Get_Parentheses ;
        end else
        if( Result = '-' ) then
        begin
            Result := '-' + Get_Token ;
        end ;
        if( Result = '@' ) then
        begin
            Result := '@' + Get_Token ;
        end ;
        if( Result = '@#' ) then
        begin
            Result := '@#' + Get_Token ;
        end ;
        if( Result = '#' ) then
        begin
            Result := '#' + Get_Token ;
        end ;
        if( Result = '(' ) then
        begin
            Get_Parentheses ;
        end ;
        if( trystrtoint( Result, Dummy ) ) then
        begin
            if( Peek_Token = '(' ) then
            begin
                Result := Result + Get_Token ;
                Get_Parentheses ;
                exit ;
            end ;
        end ;
        while( True ) do
        begin
            S := uppercase( Peek_Token ) ;
            if( S = '' ) then
            begin
                break ; // No more tokens
            end ;
            if( ( S = '+' ) or ( S = '-' ) or ( S = '*' ) or ( S = '/' ) or ( S = '!' ) or ( S = '^' ) or ( S = 'AND' ) or ( S = 'OR' ) ) then
            begin
                Result := Result + Get_Token + Get_Operand ;
            end else
            begin
                break ; // Token is not an binary operator
            end ;
        end ;
    end ; // .Get_Operand


    function Value : string ; // Get operand 2

    begin
        if( length( _Value ) = 0 ) then
        begin
            _Value := Get_Operand ;
        end ; // if( length( _Value ) = 0 )
        Result := _Value ;
    end ; // .Value

var _Operand1 : string ;

    function Operand1 : string ;

    begin
        if( length( _Operand1 ) = 0 ) then
        begin
            _Operand1 := Get_Operand ;
        end ; // if( length( _Operand1 ) = 0 )
        Operand1 := _Operand1 ;
    end ; // .Operand1


    function Register_Number( O : string ) : integer ;

    var Dummy : integer ;
        UEC : TUnified_Exception ;

    begin
        Result := -1 ; // Assume failure
        O := Edit( O, 8 or 128 ) ; // Trim leading/trailing spaces
        if( O = 'SP' ) then
        begin
            Result := 6 ;
        end else
        if( O = 'PC' ) then
        begin
            Result := 7 ;
        end else
        if(
            ( copy( O, 1, 1 ) = 'R' )
            or
            ( copy( O, 1, 1 ) = 'r' )
          ) then
        begin
            try
                Dummy := strtoint( copy( O, 2, length( O ) ) ) ;
            except
                Dummy := -1 ;
            end ;
            if( ( Dummy >= 0 ) and ( Dummy <= 7 ) ) then
            begin
                Result := Dummy ;
            end ;
        end else
        if( copy( O, 1, 1 ) = '%' ) then
        begin
            UEC := Evaluate( O, Dummy, 0 ) ;
            if( UEC <> nil ) then // Immediate mode error
            begin
                exit ;
            end ;
            if( ( Dummy >= 0 ) and ( Dummy <= 7 ) ) then
            begin
                Result := Dummy ;
            end ;
        end ;
    end ; // Register_Number


    function Calculate_Offset( O : string ; Size : longint ) : integer ;

    var C : integer ;
        Indirect : boolean ; // Indirection

    begin
        Result := 0 ;
        Indirect := copy( O, 1, 1 ) = '@' ;
        if( Indirect ) then
        begin
            O := copy( O, 2, length( O ) ) ;
        end ;
        C := Register_Number( O ) ;
        if( C >= 0 ) then // Register mode
        begin
            exit ;
        end ;
        if( copy( O, 1, 1 ) = '-' ) then // Autodecrement
        begin
            exit ;
        end ;
        if( copy( O, 1, 1 ) = '(' ) then
        begin
            exit ;
        end ;
        if( ( pos( '(', O ) > 1 ) or ( Peek_Token = '(' ) ) then // Index mode
        begin
            Result := 2 ;
            exit ;
        end ;

        // Must be immediate mode...
        if( copy( O, 1, 1 ) = '#' ) then // Numeric literal - absolute mode - equivalent to (PC)+
        begin
            if( Indirect ) then // @# - absolute address
            begin
                Result := Calculate_Offset( '@(PC)+', Size ) ;
            end else
            begin
                Result := Calculate_Offset( '(PC)+', Size ) ;
            end ;
            Result := Result + 2 ;
            exit ;
        end ;
        if( Indirect and ( pos( '(', O ) = 0 ) ) then
        begin // Relative address - equivalent to @X(PC)
            Result := Calculate_Offset( '@' + O + '(PC)', Size ) ;
            exit ;
        end ;
        Result := 2 ;
    end ;


    function Parse_Destination( O : string ; B, Size, Offset, Data_Offset : longint ;
        var E : boolean ) : string ;
    // Parse a source/destination specification.  O is the specification
    //   (additional information can be pulled from token stream).  B is the
    //   instruction code which is merged with the destination bits calculated
    //   by this routine.  Size is the size of the destination item (1=byte,
    //   2=word).  Offset is the additional offset to the next instruction (used
    //   for PC-relative calculations, which are always relative to the start of
    //   the next instruction).  Data_Offset is the additional offset after the
    //   instruction for any back-patched data (forward references).  E is
    //   ignored on call, on return it is True if there is an error in the
    //   specification.  The function returns a string which contains the
    //   instruction and any additional words for the destination (such as
    //   immediate data).

    var C, D : integer ;
        Indirect : boolean ; // Indirection
        Is_PC : boolean ;
        UEC : TUnified_Exception ;

    begin
        E := False ; // Assume no error
        Result := '' ;
        Indirect := copy( O, 1, 1 ) = '@' ;
        if( Indirect ) then
        begin
            B := B or 8 ;
            O := copy( O, 2, length( O ) ) ;
        end ;
        C := Register_Number( O ) ;
        if( C >= 0 ) then // Register mode
        begin
            B := B or C ;
            Result := chr( B and 255 ) + chr( B shr 8 ) ;
            exit ;
        end ;
        if( ( copy( O, 1, 1 ) = '-' ) and ( pos( '(', O ) = 2 ) ) then // Autodecrement
        begin
            O := copy( O, 2, length( O ) ) ;
            B := B or 32 ;
            if( copy( O, 1, 1 ) <> '(' ) then
            begin
                Log_Error( Status, 'Syntax error - Expected "("', 3 ) ;
                E := True ;
                exit ;
            end ;
            if( copy( O, length( O ), 1 ) <> ')' ) then
            begin
                Log_Error( Status, 'Syntax error - Expected ")"', 3 ) ;
                E := True ;
                exit ;
            end ;
            C := Register_Number( copy( O, 2, length( O ) - 2 ) ) ;
            if( C < 0 ) then
            begin
                Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
                E := True ;
                exit ;
            end else
            begin
                B := B or C ;
            end ;
            Result := chr( B and 255 ) + chr( B shr 8 ) ;
            exit ;
        end ;
        if( copy( O, 1, 1 ) = '(' ) then
        begin
            if( copy( O, length( O ), 1 ) = '+' ) then
            begin
                O := copy( O, 1, length( O ) - 1 ) ;
                Put_Token( '+' ) ;
            end ;
            if( copy( O, length( O ), 1 ) <> ')' ) then
            begin
                Log_Error( Status, 'Syntax error - Expected ")"', 3 ) ;
                E := True ;
                exit ;
            end ;
            C := Register_Number( copy( O, 2, length( O ) - 2 ) ) ;
            if( Indirect ) then
            begin
                if( Peek_Token = '+' ) then
                begin
                    Get_Token ;
                    B := B or 16 ;
                end ;
            end else
            begin
                if( Peek_Token <> '+' ) then
                begin
                    if( C >= 0 ) then
                    begin
                        Indirect := True ;
                        B := B or 8 ;
                    end else
                    begin
                        Log_Error( Status, 'Syntax error - Expected "+"', 3 ) ;
                        E := True ;
                        exit ;
                    end ;
                end else
                begin
                    Get_Token ;
                    B := B or 16 ; // Autoincrement
                end ;
            end ;
            B := B or C ;
            Result := chr( B and 255 ) + chr( B shr 8 ) ;
            exit ;
        end ; // if( copy( O, 1, 1 ) = '(' )
        if( Peek_Token = '(' ) then // Index mode
        begin
            UEC := Evaluate_Symbol( O, _Master, 2, Size, PC + 2 + Data_Offset, ( Flags and 1 ) = 1, C ) ;
            if( UEC <> nil ) then
            begin
                Log_Error( Status, ET( UEC.Get_Error ) + ' "' + O + '"', 3 ) ;
                E := True ;
                exit ;
            end ;
            O := Get_Token ;
            if( copy( O, 1, 1 ) <> '(' ) then
            begin
                Log_Error( Status, 'Syntax error - Expected "("', 3 ) ;
                E := True ;
                exit ;
            end ;
            if( O = '(' ) then // Just the parenthesis
            begin
                O := O + Get_Token + Get_Token ; // Get register and closing parenthesis
            end ;
            if( copy( O, length( O ), 1 ) <> ')' ) then
            begin
                Log_Error( Status, 'Syntax error - Expected ")"', 3 ) ;
                E := True ;
                exit ;
            end ;
            D := Register_Number( copy( O, 2, length( O ) - 2 ) ) ;
            if( D < 0 ) then
            begin
                Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
                E := True ;
                exit ;
            end else
            begin
                B := B or D ;
            end ;
            Result := chr( B and 255 ) + chr( B shr 8 ) + chr( C and 255 ) + chr( C shr 8 ) ;
            exit ;
        end ; // if( Peek_Token = '(' )

        // Must be immediate mode...
        if( copy( O, 1, 1 ) = '#' ) then // Numeric literal - absolute mode - equivalent to (PC)+
        begin
            O := copy( O, 2, length( O ) ) ; // Trim "#"
            Put_Token( '+' ) ;
            if( Indirect ) then // @# - absolute address
            begin
                Result := Parse_Destination( '@(PC)', B, Size, Offset, Data_Offset, E ) ;
            end else
            begin
                Result := Parse_Destination( '(PC)', B, Size, Offset, Data_Offset, E ) ;
            end ;
            if( E ) then
            begin
                exit ;
            end ;
            UEC := Evaluate_Symbol( O, _Master, 2, Size, PC + 2 + Data_Offset, ( Flags and 1 ) = 1, C ) ;
            if( UEC <> nil ) then
            begin
                Log_Error( Status, ET( UEC.Get_Error ) + ' "' + O + '"', 3 ) ;
                E := True ;
                exit ;
            end ;
            Result := Result + chr( C and 255 ) + chr( C shr 8 ) ;
            exit ;
        end ;
        if( Indirect and ( pos( '(', O ) = 0 ) ) then
        begin // Relative address - equivalent to @X(PC)
            O := '@' + O ;
            Put_Token( '(PC)' ) ;
            Result := Parse_Destination( O, B, Size, Offset, Data_Offset, E ) ;
            exit ;
        end ;
        D := pos( '(', O ) ;
        if( D > 0 ) then
        begin
            Put_Token( copy( O, D, length( O ) ) ) ;
            Is_PC := ( uppercase( copy( O, D, length( O ) ) ) = '(PC)' )
                     or
                     ( uppercase( copy( O, D, length( O ) ) ) = '(R7)' ) ;
            O := copy( O, 1, D - 1 ) ;
        end else // Must be XXX Y - same as XXX A(PC)
        begin
            Put_Token( '(PC)' ) ;
            Is_PC := True ;
        end ;
        if( Is_PC ) then
        begin
            UEC := Evaluate_Symbol( O, _Master, 3, Size, PC + 4 + Data_Offset, ( Flags and 1 ) = 1, C ) ;
                                                      // PC + 4 = PC + instruction + this data word
        end else
        begin
            UEC := Evaluate_Symbol( O, _Master, 3, Size, PC + 2 + Data_Offset, ( Flags and 1 ) = 1, C ) ;
        end ;
        if( UEC <> nil ) then
        begin
            Log_Error( Status, ET( UEC.Get_Error ) + ' "' + O + '"', 3 ) ;
            E := True ;
            exit ;
        end ;
        if( Is_PC ) then
        begin
            C := C - ( PC + 4 + Offset ) ; // Make relative to PC
        end ;
        if( Peek_Token = '(' ) then // Index mode
        begin
            Get_Token ; // Skip "("
            O := Get_Token ;
            D := Register_Number( O ) ;
            if( D < 0 ) then
            begin
                Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
                E := True ;
                exit ;
            end ;
            if( Peek_Token <> ')' ) then
            begin
                Log_Error( Status, 'Syntax error - expected ")", found "' + Get_Token + '"', 3 ) ;
                E := True ;
                exit ;
            end ;
            Get_Token ; // Eat ")"
            B := B or 48 or D ; // Index mode
        end else
        begin
            B := B or 31 ; // Use @(PC)+
        end ;
        Result := chr( B and 255 ) + chr( B shr 8 ) + chr( C and 255 ) + chr( C shr 8 ) ;
    end ; // .Parse_Destination


    function Assemble_Single_Destination( const AA : string ; B : integer ;
        var E : boolean ) : string ;

    var C : integer ;

    begin
        C := 2 ;
        if( copy( AA, 4, 1 ) = 'B' ) then
        begin
            B := B or $8000 ; // Byte mode
            C := 1 ;
        end ;
        Result := Parse_Destination( Operand1, B, C, 0, 0, E ) ;
    end ;


    function Assemble_Double_Destination( const AA : string ; B : integer ;
        var E : boolean ) : string ;

    var C : integer ;
        Dst_Offset, Src_Offset : integer ;
        S, W : string ;

    begin
        // setup...
        C := 2 ;
        if( copy( AA, 4, 1 ) = 'B' ) then
        begin
            C := 1 ;
        end ;

        while( ( Peek_Token <> ',' ) and ( Peek_Token <> '' ) and ( Peek_Token <> ';' ) ) do
        begin
            _Operand1 := _Operand1 + Get_Token ;
        end ;

        Src_Offset := Calculate_Offset( Operand1, C ) ;

        // Gobble comma...
        W := Get_Token ;
        if( W <> ',' ) then
        begin
            E := True ;
            Log_Error( Status, 'Expected comma, found "' + W + '"', 3 ) ;
            exit ;
        end ;
        Value ;
        S := _Value + Grab_Line ;
        _Value := Edit( Parse_Parameter( ';', S ), 8 or 128 ) ;
        Dst_Offset := Calculate_Offset( Value, C ) ;

        // Get source...
        Result := Parse_Destination( Operand1, 0, C, Dst_Offset, 0, E ) ;
        if( E ) then
        begin
            while( length( Result ) < 2 ) do
            begin
                Result := Result + #0 ;
            end ;
        end ;
        B := ( ( ord( Result[ 1 ] ) or ord( Result[ 2 ] ) * 256 ) shl 6 ) or B ;

        if( C = 1 ) then
        begin
            B := B or $8000 ; // Byte mode
        end ;

        // Get destination...
        S := Parse_Destination( Value, B, C, length( Result ) - 2, Src_Offset, E ) ;

        Result := copy( S, 1, 2 ) + copy( Result, 3, length( Result ) ) +
            copy( S, 3, length( S ) ) ;
    end ; // .Assemble_Double_Destination


var A, Aa, AAL : string ;
    B, C : longint ;
    E : boolean ;
    P : PChar ;
    PL : integer ;
    Next, Temp : string ;
    UEC : TUnified_Exception ;

label Ae, End_Assemble, Start_Over ;

begin // TPDP11_Assembler.Assemble_Ex
    // Setup...
    PC := Address ;
    Outputs := nil ;
    Machines := nil ;
    MachineL := 0 ;
    Assembled_Code := '' ;

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
    if( Next = '=' ) then // An assignment
    begin
        _Master.UnMap ;
        Get_Token ; // Gobble equal
        AA := AA + '=' + Input_Buffer ;
        if( Handle_Directives( AA, Status ) ) then { If a directive }
        begin
            A := AA ;
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
    if( B = -1 ) then
    begin
        Log_Error( Status, 'Unterminated quote', 2 ) ;
        goto End_Assemble ;
    end ;
    if( length( AA ) = 0 ) then // Blank line
    begin
        exit ;
    end ;

    { Handle instructions... }
    if( ( Aa = 'ADC' ) or ( Aa = 'ADCB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '5500' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ADD' ) then
    begin
        A := Assemble_Double_Destination( AA, Octal( '60000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'ASL' ) or ( Aa = 'ASLB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '6300' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'ASR' ) or ( Aa = 'ASRB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '6200' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'BIC' ) or ( Aa = 'BICB' ) ) then
    begin
        A := Assemble_Double_Destination( AA, Octal( '40000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'BIS' ) or ( Aa = 'BISB' ) ) then
    begin
        A := Assemble_Double_Destination( AA, Octal( '50000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'BIT' ) or ( Aa = 'BITB' ) ) then
    begin
        A := Assemble_Double_Destination( AA, Octal( '30000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'BR' ) or ( AA = 'BEQ' ) or ( AA = 'BNE' ) or ( AA = 'BMI' ) or
        ( AA = 'BPL' ) or ( AA = 'BCS' ) or ( AA = 'BLO' ) or ( AA = 'BCC' ) or
        ( AA = 'BHIS' ) or ( AA = 'BVS' ) or ( AA = 'BVC' ) or ( AA = 'BLT' ) or
        ( AA = 'BGE' ) or ( AA = 'BLE' ) or ( AA = 'BGT' ) or ( AA = 'BHI' ) or
        ( AA = 'BLOS' ) ) then
    begin
        A := Operand1 ;
        UEC := Evaluate( A, C, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Log_Error( Status, ET( UEC.Get_Error ) + ' "' + A + '"', 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then // Undefined
            begin
                A := '((' + A + ')-' + inttostr( PC + 2 ) + '.)' + '/2' ; // Branch distance is always words - not bytes
                _Master.Add_Reference( PChar( A ), 1, PC ) ;
                C := PC + 2 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        C := ( C - ( PC + 2 ) ) div 2 ;
        if( ( C < -128 ) or ( C > 127 ) ) then
        begin
            Log_Error( Status, 'Value out of range: "' + Operand1 + '"', 3 ) ;
            goto AE ;
        end ;
        if( AA = 'BR' ) then
        begin
            A := chr( 1 ) ;
        end else
        if( AA = 'BEQ' ) then
        begin
            A := chr( 3 ) ;
        end else
        if( AA = 'BNE' ) then
        begin
            A := chr( 2 ) ;
        end else
        if( AA = 'BMI' ) then
        begin
            A := chr( 129 ) ;
        end else
        if( AA = 'BPL' ) then
        begin
            A := chr( 128 ) ;
        end else
        if( ( AA = 'BCS' ) or ( AA = 'BLO' ) ) then
        begin
            A := chr( Octal( '1034' ) shr 2 ) ;
        end else
        if( ( AA = 'BCC' ) or ( AA = 'BHIS' ) ) then
        begin
            A := chr( Octal( '1030' ) shr 2 ) ;
        end else
        if( AA = 'BVS' ) then
        begin
            A := chr( Octal( '1024' ) shr 2 ) ;
        end else
        if( AA = 'BVC' ) then
        begin
            A := chr( Octal( '1020' ) shr 2 ) ;
        end else
        if( AA = 'BLT' ) then
        begin
            A := chr( 5 ) ;
        end else
        if( AA = 'BGE' ) then
        begin
            A := chr( 4 ) ;
        end else
        if( AA = 'BLE' ) then
        begin
            A := chr( 7 ) ;
        end else
        if( AA = 'BGT' ) then
        begin
            A := chr( 6 ) ;
        end else
        if( AA = 'BHI' ) then
        begin
            A := chr( Octal( '1010' ) shr 2 ) ;
        end else
        begin // BLOS
            A := chr( Octal( '1014' ) shr 2 ) ;
        end ;
        A := chr( C ) + A ;
        goto End_Assemble ;
    end ;
    if( Aa = 'BPT' ) then
    begin
        A := chr( 3 ) + chr( 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CCC' ) then
    begin
        C := Octal( '257' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CLC' ) then
    begin
        C := Octal( '241' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CLN' ) then
    begin
        C := Octal( '250' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'CLR' ) or ( Aa = 'CLRB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '5000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CLV' ) then
    begin
        C := Octal( '242' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CLZ' ) then
    begin
        C := Octal( '244' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'CMP' ) or ( Aa = 'CMPB' ) ) then
    begin
        A := Assemble_Double_Destination( AA, Octal( '20000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'COM' ) or ( Aa = 'COMB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '5100' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'DEC' ) or ( Aa = 'DECB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '5300' ), E ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'EMT' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate_Symbol( A, _Master, 0, 1, PC, ( Flags and 1 ) = 1, C ) ;
        if( UEC <> nil ) then
        begin
            goto AE ;
        end ;
        if( ( C < 0 ) or ( C > 255 ) ) then
        begin
            Log_Error( Status, 'Value out of range: "' + Operand1 + '"', 3 ) ;
            goto AE ;
        end ;
        A := chr( C ) + chr( Octal( '104000' ) shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'HALT' ) then
    begin
        A := chr( 0 ) + chr( 0 ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'INC' ) or ( Aa = 'INCB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '5200' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'IOT' ) then
    begin
        A := chr( 4 ) + chr( 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'JMP' ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '100' ), E ) ;
        if( not E ) then
        begin
            C := ord( A[ 1 ] ) ;
            if( ( ( C shr 3 ) and 7 ) = 0 ) then // Mode 0 illegal in JMP
            begin
                Log_Error( Status, 'Mode 0 is not valid here', 3 ) ;
                E := True ;
            end ;
        end ;
        goto End_Assemble ;
    end ;
    if( Aa = 'JSR' ) then
    begin
        C := Register_Number( Operand1 ) ;
        if( C < 0 ) then
        begin
            Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
            E := True ;
            goto End_Assemble
        end ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            E := True ;
            Log_Error( Status, 'Expected comma, found "' + A + '"', 3 ) ;
            goto End_Assemble
        end ;
        C := ( C shl 6 ) or Octal( '4000' ) ;
        A := Parse_Destination( Value, C, 2, 0, 0, E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SOB' ) then
    begin
        C := Register_Number( Operand1 ) ;
        if( C < 0 ) then
        begin
            Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
            E := True ;
            goto End_Assemble
        end ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            E := True ;
            Log_Error( Status, 'Expected comma, found "' + A + '"', 3 ) ;
            goto End_Assemble
        end ;
        B := ( C shl 6 ) or Octal( '077000' ) ;
        UEC := Evaluate( Value, C, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Log_Error( Status, ET( UEC.Get_Error ) + ' "' + A + '"', 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then // Undefined
            begin
                A := '((' + A + ')-' + inttostr( PC + 2 ) + '.)' + '/2' ; // Branch distance is always words - not bytes
                _Master.Add_Reference( PChar( A ), 1, PC ) ;
                C := PC + 2 ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        C := ( C - ( PC + 2 ) ) div 2 ;
        if( ( C < -128 ) or ( C > 127 ) ) then
        begin
            Log_Error( Status, 'Value out of range: "' + Operand1 + '"', 3 ) ;
            goto AE ;
        end ;
        B := B or C ;
        A := chr( B ) + chr( B shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SPL' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate_Symbol( A, _Master, 0, 1, PC, ( Flags and 1 ) = 1, C ) ;
        if( UEC <> nil ) then
        begin
            goto AE ;
        end ;
        if( ( C < 0 ) or ( C > 7 ) ) then
        begin
            Log_Error( Status, 'Value out of range: "' + Operand1 + '"', 3 ) ;
            goto AE ;
        end ;
        C := Octal( '000230' ) or C ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'XOR' ) then
    begin
        C := Register_Number( Operand1 ) ;
        if( C < 0 ) then
        begin
            Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
            E := True ;
            goto End_Assemble
        end ;
        C := ( C shl 6 ) or Octal( '074000' ) ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            E := True ;
            Log_Error( Status, 'Expected comma, found "' + A + '"', 3 ) ;
            goto End_Assemble
        end ;
        A := Parse_Destination( Value, C, 2, 0, 0, E ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'MUL' ) then
    begin
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            E := True ;
            Log_Error( Status, 'Expected comma, found "' + A + '"', 3 ) ;
            goto End_Assemble
        end ;
        C := Register_Number( Value ) ;
        if( C < 0 ) then
        begin
            Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
            E := True ;
            goto End_Assemble
        end ;
        C := ( C shl 6 ) or Octal( '070000' ) ;
        A := Parse_Destination( Operand1, C, 2, 0, 0, E ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'DIV' ) then
    begin
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            E := True ;
            Log_Error( Status, 'Expected comma, found "' + A + '"', 3 ) ;
            goto End_Assemble
        end ;
        C := Register_Number( Value ) ;
        if( C < 0 ) then
        begin
            Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
            E := True ;
            goto End_Assemble
        end ;
        C := ( C shl 6 ) or Octal( '071000' ) ;
        A := Parse_Destination( Operand1, C, 2, 0, 0, E ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'ASH' ) then
    begin
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            E := True ;
            Log_Error( Status, 'Expected comma, found "' + A + '"', 3 ) ;
            goto End_Assemble
        end ;
        C := Register_Number( Value ) ;
        if( C < 0 ) then
        begin
            Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
            E := True ;
            goto End_Assemble
        end ;
        C := ( C shl 6 ) or Octal( '072000' ) ;
        A := Parse_Destination( Operand1, C, 2, 0, 0, E ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'ASHC' ) then
    begin
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            E := True ;
            Log_Error( Status, 'Expected comma, found "' + A + '"', 3 ) ;
            goto End_Assemble
        end ;
        C := Register_Number( Value ) ;
        if( C < 0 ) then
        begin
            Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
            E := True ;
            goto End_Assemble
        end ;
        C := ( C shl 6 ) or Octal( '073000' ) ;
        A := Parse_Destination( Operand1, C, 2, 0, 0, E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SXT' ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '006700' ), E ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'MARK' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate_Symbol( A, _Master, 0, 1, PC, ( Flags and 1 ) = 1, C ) ;
        if( UEC <> nil ) then
        begin
            goto AE ;
        end ;
        if( ( C < 0 ) or ( C > 63 ) ) then
        begin
            Log_Error( Status, 'Value out of range: "' + Operand1 + '"', 3 ) ;
            goto AE ;
        end ;
        A := chr( C ) + chr( Octal( '006400' ) shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'MFPD' ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '106500' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'MFPI' ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '006500' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'MFPS' ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '106700' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'MOV' ) or ( Aa = 'MOVB' ) ) then
    begin
        A := Assemble_Double_Destination( AA, Octal( '10000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'MTPD' ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '106600' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'MTPI' ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '006600' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'MTPS' ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '106400' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'NEG' ) or ( Aa = 'NEGB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '5400' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'NOP' ) then
    begin
        C := Octal( '240' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RESET' ) then
    begin
        A := chr( 5 ) + chr( 0 ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'ROL' ) or ( Aa = 'ROLB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '6100' ), E ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'ROR' ) or ( Aa = 'RORB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '6000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RTI' ) then
    begin
        A := chr( 2 ) + chr( 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RTS' ) then
    begin
        C := Register_Number( Operand1 ) ;
        if( C < 0 ) then
        begin
            Log_Error( Status, 'Syntax error - Expected register', 3 ) ;
            E := True ;
            goto End_Assemble
        end ;
        C := C or Octal( '200' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RTT' ) then
    begin
        A := chr( 6 ) + chr( 0 ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'SBC' ) or ( Aa = 'SBCB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '5600' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SCC' ) then
    begin
        C := Octal( '277' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SEC' ) then
    begin
        C := Octal( '261' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SEZ' ) then
    begin
        C := Octal( '264' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SEV' ) then
    begin
        C := Octal( '262' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SEN' ) then
    begin
        C := Octal( '270' ) ;
        A := chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SUB' ) then
    begin
        A := Assemble_Double_Destination( AA, Octal( '160000' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SWAB' ) then
    begin
        A := Assemble_Single_Destination( '', Octal( '0300' ), E ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'TRAP' ) then
    begin
        A := Operand1 ;
        UEC := Evaluate_Symbol( A, _Master, 0, 1, PC, ( Flags and 1 ) = 1, C ) ;
        if( UEC <> nil ) then
        begin
            goto AE ;
        end ;
        if( ( C < 0 ) or ( C > 255 ) ) then
        begin
            Log_Error( Status, 'Value out of range: "' + Operand1 + '"', 3 ) ;
            goto AE ;
        end ;
        A := chr( C ) + chr( Octal( '104400' ) shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( ( Aa = 'TST' ) or ( Aa = 'TSTB' ) ) then
    begin
        A := Assemble_Single_Destination( AA, Octal( '5700' ), E ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'WAIT' ) then
    begin
        A := chr( 1 ) + chr( 0 ) ;
        goto End_Assemble ;
    end ;

    if( Handle_Directives( AAL, Status ) ) then { If a directive }
    begin
        A := AAL ;
	goto End_Assemble ;
    end ;
    if( Length( Aa ) > 0 ) then
    begin
        UEC := Evaluate_Symbol( AA, _Master, 0, 2, PC + 1, ( Flags and 1 ) = 1, C ) ;
        if( UEC = nil ) then // Resolved the value
        begin
            A := chr( C ) + chr( C shr 8 ) ;
            goto End_Assemble ;
        end ;
        Log_Error( Status, 'Unknown symbol: "' + AA + '"', 3 ) ;
AE:
        // Unrecognized mnuemonic...
        _Master.Put_Token( PChar( AA ) ) ;
        UEC := _Master.Expand( nil, P, PL, Status ) ;
        if( UEC <> nil ) then
        begin
            Err := True ;
            A := chr( 0 ) + chr( 0 ) ;
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
end ; // TPDP11_Assembler.Assemble


{ Returns the default radix (base) of numeric literals. }
function TPDP11_Assembler.Default_Radix : longint ;

begin
    Result := Base ;
end ;


{ Returns the default size of numeric literals, in bits. }
function TPDP11_Assembler.Default_Size : longint ;

begin
    Default_Size := 8 ;
end ;


{ Returns facility code for this class. }
function TPDP11_Assembler.Facility_Code : longint ;

begin
    Result := PDP11AssemblerErr_Facility ;
end ;


function TPDP11_Assembler.Normalize_Expression( Expression : PChar ;
    Numeric : boolean ; Status : TAssembler_Status ) : PChar ;

var Dummy, Start : integer ;

begin
    Temp_Normalize_Expression := string( Expression ) ;
    if( Numeric ) then
    begin
        Dummy := Parse_Position( #39, Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            if( Dummy = length( Temp_Normalize_Expression ) ) then
            begin
                Log_Error( Status, 'Syntax error', 3 ) ;
            end ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) +
                inttostr( ord( Temp_Normalize_Expression[ Dummy + 1 ] ) ) + '.' +
                copy( Temp_Normalize_Expression, Dummy + 2, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( #39, Temp_Normalize_Expression ) ;
        end ;

        Dummy := Parse_Position( '!', Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) +
                ' OR ' +
                copy( Temp_Normalize_Expression, Dummy + 1, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( '!', Temp_Normalize_Expression ) ;
        end ;

        Dummy := Parse_Position( '<', Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) +
                '(' +
                copy( Temp_Normalize_Expression, Dummy + 1, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( '<', Temp_Normalize_Expression ) ;
        end ;

        Dummy := Parse_Position( '>', Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) +
                ')' +
                copy( Temp_Normalize_Expression, Dummy + 1, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( '>', Temp_Normalize_Expression ) ;
        end ;

        Dummy := Parse_Position( '^D', Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            Start := Dummy ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) +
                copy( Temp_Normalize_Expression, Dummy + 2, length( Temp_Normalize_Expression ) ) ;
            while(
                   ( Dummy <= length( Temp_Normalize_Expression ) )
                   and
                   ( pos( Temp_Normalize_Expression[ Dummy ], '0123456789' ) > 0 )
                 ) do
            begin
                inc( Dummy ) ;
            end ;
            if( Start = Dummy ) then
            begin
                Log_Error( Status, 'Syntax error', 3 ) ;
            end ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) + '.' +
                copy( Temp_Normalize_Expression, Dummy, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( '^D', Temp_Normalize_Expression ) ;
        end ;

        Dummy := Parse_Position( '^O', Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            Start := Dummy ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) +
                copy( Temp_Normalize_Expression, Dummy + 2, length( Temp_Normalize_Expression ) ) ;
            while(
                   ( Dummy <= length( Temp_Normalize_Expression ) )
                   and
                   ( pos( Temp_Normalize_Expression[ Dummy ], '01234567' ) > 0 )
                 ) do
            begin
                inc( Dummy ) ;
            end ;
            if( Start = Dummy ) then
            begin
                Log_Error( Status, 'Syntax error', 3 ) ;
            end ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) + 'Q' +
                copy( Temp_Normalize_Expression, Dummy, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( '^O', Temp_Normalize_Expression ) ;
        end ;

        Dummy := Parse_Position( '^B', Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            Start := Dummy ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) +
                copy( Temp_Normalize_Expression, Dummy + 2, length( Temp_Normalize_Expression ) ) ;
            while(
                   ( Dummy <= length( Temp_Normalize_Expression ) )
                   and
                   ( pos( Temp_Normalize_Expression[ Dummy ], '01' ) > 0 )
                 ) do
            begin
                inc( Dummy ) ;
            end ;
            if( Start = Dummy ) then
            begin
                Log_Error( Status, 'Syntax error', 3 ) ;
            end ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) + 'B' +
                copy( Temp_Normalize_Expression, Dummy, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( '^B', Temp_Normalize_Expression ) ;
        end ;

        Dummy := Parse_Position( '^C', Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) + ' NOT ' +
                copy( Temp_Normalize_Expression, Dummy + 1, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( '^C', Temp_Normalize_Expression ) ;
        end ;

        Dummy := Parse_Position( '^R', Temp_Normalize_Expression ) ;
        while( Dummy > 0 ) do
        begin
            Start := Dummy ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Dummy - 1 ) +
                copy( Temp_Normalize_Expression, Dummy + 2, length( Temp_Normalize_Expression ) ) ;
            while(
                   ( Dummy <= length( Temp_Normalize_Expression ) )
                   and
                   ( pos( Temp_Normalize_Expression[ Dummy ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ$. 0123456789' ) > 0 )
                 ) do
            begin
                inc( Dummy ) ;
            end ;
            if( Start = Dummy ) then
            begin
                Log_Error( Status, 'Syntax error', 3 ) ;
            end ;
            Temp_Normalize_Expression := copy( Temp_Normalize_Expression, 1, Start - 1 ) +
                inttostr( Rad50( copy( Temp_Normalize_Expression, Start + 2, Dummy - Start - 1 ) ) ) + '.' +
                copy( Temp_Normalize_Expression, Dummy, length( Temp_Normalize_Expression ) ) ;
            Dummy := Parse_Position( '^R', Temp_Normalize_Expression ) ;
        end ;

        // TODO: Handle ^F - floating point: 1 sign, 8 exponent, 7 mantissa bits
    end ; // if( Numeric )
    Result := PChar( Temp_Normalize_Expression ) ;
end ;


const _Extensions : string = 'mac' ;

function TPDP11_Assembler.Source_Extensions : PChar ;

begin
    Result := PChar( _Extensions ) ;
end ;


const _Valid_Symbol_Initial : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_' ;

function TPDP11_Assembler.Valid_Symbol_Initial : PChar ;

begin
    Result := PChar( _Valid_Symbol_Initial ) ;
end ;


const _Valid_Symbol_After : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_0123456789' ;

function TPDP11_Assembler.Valid_Symbol_After : PChar ;

begin
    Result := PChar( _Valid_Symbol_After ) ;
end ;


function TPDP11_Assembler.Evaluate( const X : string ;
    var _Result : longint ; PC_Adjustment : integer ) : TUnified_Exception ;

var I64 : int64 ;

begin
    Result := _Master.Evaluate( PChar( X ), I64 ) ;
    if( ( Result <> nil ) and ( Result.Get_Error = 4 ) ) then // Undefined
    begin
        if( ( copy( X, 1, 4 ) = 'MARK' ) and ( length( X ) > 4 ) ) then
        begin
            try
                I64 := strtoint( copy( X, 5, length( X ) ) ) ;
                if( ( I64 >= 0 ) and ( I64 <= 63 ) ) then
                begin
                    I64 := I64 or Octal( '006400' ) ;
                    Result := nil ;
                end ;
            except
            end ;
        end ;
    end ;
    _Result := I64 ;
end ;


function TPDP11_Assembler.Handle_Directives( var Value : string ;
    Status : TAssembler_Status ) : boolean ;

var P : PChar ;
    PL : longint ;
    UEC : TUnified_Exception ;

begin
    // Setup...
    Value := Edit( Value, 128 ) ;
    Result := False ;

    // Handle directive...
    if( Value = '.KA11' ) then
    begin
        CPU_Parent.Set_Up( 'KA11' ) ;
        Handle_Directives := True ;
        _Master.Expand( '.DEFINE KA11', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE KC11', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE KD11B', P, PL, Status ) ;
        Value := '' ;
    end else
    if( Value = '.KC11' ) then
    begin
        CPU_Parent.Set_Up( 'KC11' ) ;
        Handle_Directives := True ;
        _Master.Expand( '.DEFINE KC11', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE KA11', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE KD11B', P, PL, Status ) ;
        Value := '' ;
    end else
    if( Value = '.KD11B' ) then
    begin
        CPU_Parent.Set_Up( 'KD11-B' ) ;
        Handle_Directives := True ;
        _Master.Expand( '.DEFINE KD11B', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE KA11', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE KC11', P, PL, Status ) ;
        Value := '' ;
    end else
    if( Value = '.ZERO' ) then
    begin
        TPDP11_CPU( CPU ).Zero ;
        Value := '' ;
    end else
    begin
        UEC := _Master.Expand( PChar( Value ), P, PL, Status ) ;
        Handle_Directives := ( UEC = nil ) ;
        setlength( Value, PL ) ;
        move( P[ 0 ], Value[ 1 ], PL ) ;
    end ;
end ; { Handle_Directives }


procedure TPDP11_Assembler.Log_Error( Status : TAssembler_Status ;
    const Message : string ; Level : integer ) ;

begin
    Status.Log_Error( PChar( Message ), nil, -1, 3 ) ;
end ;



end.
