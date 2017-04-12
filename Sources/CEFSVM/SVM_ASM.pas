{
              Program Name : CEFSVM
              Package Name : CEFSVM
              Purpose      : SVM assembler
              Institution  : Conroy & Conroy
              Date Written : 7-Feb-2015
              Written By   : Alan Conroy
              Version      : 1.0

              Copyright (C) 2015 by Alan Conroy.  Released to the public domain.

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

          This unit defines the assembler for the CEF SVM component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit SVM_ASM ;

interface

uses { CEF... }
     CEF, { TAssembler }

     { Other... }
     _UEHDefs ; { TUEC }

const SVMAssemblerErr_Facility = 118 ;
      SVMAssemblerErr_Success = 0 ;
      SVMAssemblerErr_Unknown_Switch = 1 ; // Unknown switch
      SVMAssemblerErr_No_Files_Found = 2 ; // No files specified

type TDelete_Notification = procedure( Sender : TAssembler ) of object ;
type TFinish_Notification = procedure( Sender : TAssembler ;
    Status : TAssembler_Status ; PC : int64 ) of object ;


type TSVM_Assembler = class( TAssembler )
                          public // Instance data...
                              Assembled_Code : string ;
                              Base : integer ;
                              Err : boolean ; { True if an error }
                              _Master : TMaster_Assembler ;
                              PC : integer ; { Current target address }
                              CPU : TCPU ;
                              SMU : boolean ;
                              In_Assembly : boolean ;
                              In_Data : boolean ;
                              Data_Address : longint ; // Current data address
                              High_Code : int64 ; // Highest address we assembled to
                              On_Delete : TDelete_Notification ;
                              On_Finish : TFinish_Notification ;

                          private // Internal utility routines...
                              function Evaluate( X : string ;
                                  var _Result : int64 ) : TUEC ;

                              function Evaluate_Number( Value : string ;
                                  var C : int64 ; Lowest, Highest : int64 ;
                                  Size : integer ; Status : TAssembler_Status ;
                                  Relative, Code_Address : boolean ) : boolean ;

                              function Data_Address_Evaluate( Value : string ;
                                  var C : int64 ; Lowest, Highest : int64 ;
                                  Size : integer ;
                                  Status : TAssembler_Status ) : boolean ;

                              function Call_Address_Evaluate( const Value : string ;
                                  var C : int64 ; Lowest, Highest : int64 ;
                                  Size : integer ;
                                  Status : TAssembler_Status ) : boolean ;

                              function Handle_Directives( var Value : string ;
                                  Status : TAssembler_Status ) : boolean ;

                          public // API...
                              procedure Initialize( Master : TMaster_Assembler ) ;
                                  override ;

                              procedure Terminate ; override ;

                              function Assemble_Ex( Inputs : PChar ;
                                  var Outputs, Machines : PChar ;
                                  var MachineL : longint ; var Address : int64 ;
                                  var Segment : longint ;
                                  Status : TAssembler_Status ; Flags : longint ) : TUEC ;
                                  override ;

                              function Default_Radix : longint ; override ;

                              function Default_Size : longint ; override ;

                              function Facility_Code : longint ; override ;

                              function Source_Extensions : PChar ; override ;

                              function Valid_Symbol_Initial : PChar ; override ;

                              function Valid_Symbol_After : PChar ; override ;

                              function Begin_Assembly : int64 ; override ;

                              function Finish_Assembly( Context : int64 ; var outputs, machines : PChar ;
                                  var MachineL : longint ; var Address : int64 ;
                                  Status : TAssembler_Status ; Flags : longint ) : TUEC ;
                                  override ;

                              function Request_Data( Address, Size : int64 ) : int64 ;
                                  override ;
                      end ; // TSVM_Assembler


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

     // SVM...
     SiriusDT, // Type_Procedure
     SopCodes, // Op_Null
     SVM_CPU ; // TCEFSVM

// TSVM_Assembler methods...

function TSVM_Assembler.Evaluate( X : string ; var _Result : int64 ) : TUEC ;

var I64 : int64 ;

begin
    Result := _Master.Evaluate( PChar( X ), I64 ) ;
    _Result := I64 ;
end ; { TSVM_Assembler.Evaluate }


procedure TSVM_Assembler.Initialize( Master : TMaster_Assembler ) ;

begin
    _Master := Master ;
end ;


{ Informs the assembler that the current assembly operation is now complete. }
procedure TSVM_Assembler.Terminate ;

begin
    if( assigned( On_Delete ) ) then
    begin
        On_Delete( self ) ; // Notify CPU that we are being deleted
    end ;
    Free ;
end ;


function ET( Code : integer ) : string ;

begin
    case Code of
        1 : Result := 'Invalid digits' ;
        2 : Result := 'Illegal expression' ;
        3 : Result := 'Unterminated string literal' ;
    end ;
end ;


function TSVM_Assembler.Evaluate_Number( Value : string ; var C : int64 ;
    Lowest, Highest : int64 ; Size : integer ; Status : TAssembler_Status ;
    Relative, Code_Address : boolean ) : boolean ;

var P : PSymbol_Record ;
    UEC : TUEC ;

begin
    Result := False ; // Assume failure
    if( Relative ) then
    begin
        P := _Master.Get_Symbol( PChar( Value ) ) ;
        if( P <> nil ) then
        begin
            if( ( P^.Flags and SF_Label ) <> 0 ) then // A label
            begin
                Value := Value + ' - ' + inttostr( PC + Size + 1 ) ;
            end ;
        end ;
    end ;
    UEC := Evaluate( Value, C ) ;
    if( Relative and ( UEC.Code = 4 ) ) then // Undefined symbol assumed to be a relative forward reference
    begin
        Value := Value + ' - ' + inttostr( PC + Size + 1 ) ;
    end ;
    if( UEC.Code = 0 ) then { A number }
    begin
        if( Lowest < Highest ) then // If Lowest >= Highest then don't do bounds check
        begin
            if( ( C < Lowest ) or ( C > Highest ) ) then
            begin
                Status.Log_Error( PChar( 'Value out of range: ' + inttostr( C ) ), nil, -1, 3 ) ;
                exit ;
            end ;
        end ;
        Result := True ; // Success
    end else
    if( UEC.Code = 4 ) then // A potential number
    begin
        _Master.Add_Reference( PChar( Value ), Size, PC + 1 ) ;
        Result := True ; // Success
        C := 0 ;
    end else
    begin
        case UEC.Code of
            1 : Status.Log_Error( PChar( 'Invalid digits' ), nil, -1, 3 ) ;
            2 : Status.Log_Error( PChar( 'Illegal expression' ), nil, -1, 3 ) ;
            3 : Status.Log_Error( PChar( 'Unterminated string literal' ), nil, -1, 3 ) ;
        end ;
    end ;
end ; // TSVM_Assembler.Evaluate_Number


function TSVM_Assembler.Data_Address_Evaluate( Value : string ;
    var C : int64 ; Lowest, Highest : int64 ; Size : integer ;
    Status : TAssembler_Status ) : boolean ;

var SF : boolean ;
    UEC : TUEC ;

begin
    Result := False ; // Assume failure
    Value := Edit( Value, 8 or 32 or 128 ) ;
    SF := False ;
    if( copy( Value, 1, 2 ) = 'SF' ) then
    begin
        if( pos( copy( Value, 3, 1 ), '+-' ) <> 0 ) then
        begin
            if( Value[ 3 ] = '+' ) then
            begin
                Value := copy( Value, 4, length( Value ) ) ;
            end else
            begin
                Value := copy( Value, 3, length( Value ) ) ;
                Lowest := Highest ; // Don't range check a negative SF-relative value
            end ;
            SF := True ;
        end ;
    end ;
    UEC := Evaluate( Value, C ) ;
    if( ( UEC.Code = 0 ) or ( UEC.Code = 4 ) ) then { A (potential) number }
    begin
        if( UEC.Code = 4 ) then
        begin
            _Master.Add_Reference( PChar( Value ), 4, PC + 1 ) ;
        end else
        if( Lowest < Highest ) then // If Lowest >= Highest then don't do bounds check
        begin
            if( ( C < Lowest ) or ( C > Highest ) ) then
            begin
                Status.Log_Error( PChar( 'Value out of range: ' + inttostr( C ) ), nil, -1, 3 ) ;
                exit ;
            end ;
        end ;
        if( SF ) then
        begin
            if( Size < 4 ) then
            begin
                Status.Log_Error( PChar( 'Stack-frame-relative addresses not allowed here' ), nil, -1, 3 ) ;
                exit ;
            end ;
            C := C or 1073741824 ;
        end ;
        Result := True ; // Success
    end ;
end ; // TSVM_Assembler.Data_Address_Evaluate


function TSVM_Assembler.Call_Address_Evaluate( const Value : string ;
    var C : int64 ; Lowest, Highest : int64 ; Size : integer ;
    Status : TAssembler_Status ) : boolean ;

begin
    Result := Evaluate_Number( Value, C, Lowest, Highest, Size, Status, False, True ) ;
    if( Result ) then
    begin
        if( ( C >= 100 ) and ( C <= 127 ) ) then
        begin
            SMU := True ;
        end ;
    end ;
end ;


function TSVM_Assembler.Handle_Directives( var Value : string ;
    Status : TAssembler_Status ) : boolean ;

var Flags : integer ;
    P : PChar ;
    PL : longint ;
    PS : PSymbol_Record ;
    Name, S : string ;
    Org : int64 ;
    Size : integer ;
    Typ : integer ;
    UEC : TUEC ;

begin
    // Setup...
    Value := Edit( Value, 128 ) ;
    Handle_Directives := False ;

    // Handle directive...
    if( Value = '.ORG' ) then
    begin
	Handle_Directives := True ;
        UEC := Evaluate( Value, Org ) ;
        Value := '' ;
    end else
    if( copy( Value, 1, 9 ) = '.DECLARE ' ) then
    begin
        Size := 0 ;
        Typ := 0 ;
        Flags := 0 ;
        Value := Edit( copy( Value, 10, length( Value ) ), 8 or 128 ) ;
        Name := Parse_Parameter( ' ', Value ) ; // Symbol name
        UEC := _Master.Expand( PChar( Name + ':' ), P, PL, Status ) ;
        Handle_Directives := ( UEC.Code = 0 ) ;
        S := Parse_Parameter( ' ', Value ) ; // Scope specification
        if( S = 'LOCAL' ) then
        begin
            Flags := 0 ;
        end else
        if( S = 'GLOBAL' ) then
        begin
            Flags := SF_Global ;
        end else
        begin
            Status.Log_Error( 'Missing scope specification', '', -1, 3 ) ; // Error
        end ;
        S := Parse_Parameter( ' ', Value ) ; // Scope type
        if( S = 'PROCEDURE' ) then
        begin
            Typ := Type_Procedure ;
            Size := 4 ;
        end else
        begin
            Status.Log_Error( 'Missing type specification', '', -1, 3 ) ; // Error
        end ;
        if( Result ) then
        begin
            new( PS ) ;
            fillchar( PS^, sizeof( P^ ), 0 ) ;
            PS^.Address := PC ;
            PS^.Typ := Typ ;
            PS^.Flags := Flags ;
            PS^.Size := Size ;
            _Master.Assembler_Context.Add_Symbol( PChar( Name ), PS ) ;
        end ;
        Value := '' ;
    end else
    if( Value = '.DATA' ) then
    begin
	    Handle_Directives := True ;
        In_Data := True ;
        Value := '' ;
    end else
    if( Value = '.CODE' ) then
    begin
	    Handle_Directives := True ;
        In_Data := False ;
        Value := '' ;
    end else
    if( Value <> 'END' ) then
    begin
        UEC := _Master.Expand( PChar( Value ), P, PL, Status ) ;
        Handle_Directives := ( UEC.Code = 0 ) ;
        setlength( Value, PL ) ;
        move( P[ 0 ], Value[ 1 ], PL ) ;
    end ;
end ; // TSVM_Assembler.Handle_Directives


var Neg2G : int64 = -2147483647 - 1 ;


function TSVM_Assembler.Assemble_Ex( Inputs : PChar ;
    var Outputs, Machines : PChar ; var MachineL : longint ;
    var Address : int64 ; var Segment : longint ;
    Status : TAssembler_Status ; Flags : longint ) : TUEC ;

var A, Aa, AAL : string ;
    B : longint ;
    C, D : int64 ;
    Continuation : string ;
    P : PChar ;
    PL : longint ;
    Next, Temp : string ;
    UEC : TUEC ;

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
            if( ( Dummy = 1 ) and ( pos( Input_Buffer[ Dummy ], '-+' ) > 0 ) ) then
            begin
                while( copy( Input_Buffer, 2, 1 ) = ' ' ) do // Trim spaces following sign
                begin
                    Input_Buffer := copy( Input_Buffer, 1, 1 ) + copy( Input_Buffer, 2, length( Input_Buffer ) ) ;
                end ;
                continue ;
            end ;
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
                    Input_Buffer := Edit( copy( Input_Buffer, Dummy, length( Input_Buffer ) ), 32 or 256 ) ;
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


    function _Get_Token : string ;

    var S : string ;

    begin
        Result := Get_Token ;
        S := Peek_Token ;
        if( S = '.' ) then // Decimal value
        begin
            Result := Result + '.' ;
            Get_Token ; // Eat period
        end else
        if( uppercase( Result ) = 'SF' ) then
        begin
            if( ( copy( S, 1, 1 ) = '-' ) or ( copy( S, 1, 1 ) = '+' ) ) then
            begin
                Result := Result + Get_Token ;
            end ;
        end ;
    end ;


    function Value : string ; // Get operand 2

    begin
        if( length( _Value ) = 0 ) then
        begin
            _Value := _Get_Token ;
        end ;
        Result := _Value ;
    end ;


var _Operand1 : string ;

    function Operand1 : string ;

    begin
        if( length( _Operand1 ) = 0 ) then
        begin
            _Operand1 := _Get_Token ;
        end ;
        Operand1 := _Operand1 ;
    end ;


    function Handle_Integer_Opcode( Op_Code : string ; V : integer ) : integer ;

    var Instruction, S : string ;
        UEC : TUEC ;

    begin
        S := copy( AA, length( AA ), 1 ) ;
        if( ( S <> 'U' ) and ( S <> 'S' ) ) then // Unsigned or Signed
        begin
            Result := 2 ;
            exit ;
        end ;
        Instruction := copy( AA, 1, length( AA ) - 1 ) ;
        if( Instruction = Op_Code ) then
        begin
            Result := 1 ; // Assume failure
            UEC := Evaluate( Value, C ) ;
            if( UEC.Code = 0 ) then { A (potential) number }
            begin
                if ( C < 1 ) or ( C > 127 ) then
                begin
                    Result := 1 ;
                    Status.Log_Error( PChar( 'Invalid integer size: ' + inttostr( C ) ), nil, -1, 3 ) ;
                    exit ;
                end ;
                if( S = 'U' ) then
                begin
                    C := -C ;
                end ;
                A := Chr( V ) + Chr( C ) ;
                Result := 0 ; // Assume success
            end ;
            if( Result = 1 ) then
            begin
                Status.Log_Error( PChar( 'Invalid value: "' + Value + '"' ), nil, -1, 3 ) ;
            end ;
        end else
        begin
            Result := 2 ; // No match
        end ;
    end ;

var S : string ;

label AE, End_Assemble, Start_Over ;

begin // TSVM_Assembler.Assemble
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
            setlength( AA, PL ) ;
            move( P[ 0 ], AA[ 1 ], PL ) ;
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
    AA := Temp ;

    B := 0 ;
    C := 1 ;
    while( C <= Length( AA ) ) do
    begin
        if( AA[ C ] = #39 ) then {Handle quotes}
        begin
            if( B = 0 ) then
            begin
                B := -1 ;
            end else
            begin
                B := 0 ;
            end ;
        end ;
        if( ( AA[ C ] = ';' ) and ( B = 0 ) ) then // A comment
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
    if( Aa = 'NULL' ) then
    begin
        A := Chr( Op_Null ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'EXIT' ) then
    begin
        A := Chr( Op_Exit ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'STOP' ) then
    begin
        A := Chr( Op_Stop ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'GOTOL' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 4294967295, 4, Status, False, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Goto_Long ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CALLL' ) then
    begin
        if( not Call_Address_Evaluate( Value, C, 0, 4294967295, 4, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Long ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'END' ) then
    begin
        A := Chr( Op_End ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'IF' ) then
    begin
        UEC := Evaluate( Value, C ) ;
        if( not Evaluate_Number( Value, C, 0, 4294967295, 4, Status, False, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_If ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHILA' ) then
    begin
        // Note: We allow pushing of negative addresses, because they indicate pass-by-ref to CALLN
        if( not Data_Address_Evaluate( Value, C, -2147483647, 4294967295, 4, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Push_Long_Address ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHDL' ) then
    begin
        if( not Data_Address_Evaluate( Value, C, 0, 4294967295, 4, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Push_Direct_Long ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POPDL' ) then
    begin
        if( not Data_Address_Evaluate( Value, C, 0, 4294967295, 4, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Pop_Direct_Long ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHIW' ) then
    begin
        if( not Data_Address_Evaluate( Value, C, 0, 65535, 2, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Push_Word ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ZERO' ) then
    begin
        A := Chr( Op_Zero_Temp0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CONT' ) then
    begin
        A := Chr( Op_Concat_Stack_To_Temp0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'NES' ) then
    begin
        A := Chr( Op_String_Compare_NE ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'EQS' ) then
    begin
        A := Chr( Op_String_Compare_EQ ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LTS' ) then
    begin
        A := Chr( Op_String_Compare_LT ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'GTS' ) then
    begin
        A := Chr( Op_String_Compare_GT ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LES' ) then
    begin
        A := Chr( Op_String_Compare_LE ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'GES' ) then
    begin
        A := Chr( Op_String_Compare_GE ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CONVERT' ) then
    begin
        A := Chr( Op_Convert_Number ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CREATE' ) then
    begin
        A := Chr( Op_Create_String_From_Temp0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'NOTS' ) then
    begin
        A := Chr( Op_NOT ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ANDS' ) then
    begin
        A := Chr( Op_AND ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'NANDS' ) then
    begin
        A := Chr( Op_NAND ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ORS' ) then
    begin
        A := Chr( Op_OR ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'NORS' ) then
    begin
        A := Chr( Op_NOR ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'XORS' ) then
    begin
        A := Chr( Op_XOR ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'XNORS' ) then
    begin
        A := Chr( Op_XNOR ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CONS' ) then
    begin
        A := Chr( Op_Concat_Indirect ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POP' ) then
    begin
        A := Chr( Op_Pop ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'COPY' ) then
    begin
        A := Chr( Op_Copy_String_Indirect ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'DEREF' ) then
    begin
        A := Chr( Op_Dereference ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ZEROS' ) then
    begin
        A := Chr( Op_Zero_String ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PROWL' ) then
    begin
        A := Chr( Op_Promote_Word_To_Long ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RANGE' ) then
    begin
        A := Chr( Op_Range_Check ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POPS' ) then
    begin
        A := Chr( Op_Pop_To_Stack ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ADDL' ) then
    begin
        A := Chr( Op_Add_Long ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SUBL' ) then
    begin
        A := Chr( Op_Subtract_Long ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'MULL' ) then
    begin
        A := Chr( Op_Multiply_Long ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'DIVL' ) then
    begin
        A := Chr( Op_Divide_Long ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CALLIL' ) then
    begin
        A := Chr( Op_Call_Long_Indirect ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CALLV' ) then
    begin
        A := Chr( Op_Call_Vector ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SKIP1' ) then
    begin
        A := Chr( Op_Skip_1 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SKIP2' ) then
    begin
        A := Chr( Op_Skip_2 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SKIP3' ) then
    begin
        A := Chr( Op_Skip_3 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SKIP4' ) then
    begin
        A := Chr( Op_Skip_4 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SKIP5' ) then
    begin
        A := Chr( Op_Skip_5 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SKIP6' ) then
    begin
        A := Chr( Op_Skip_6 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RCOPY' ) then
    begin
        A := Chr( Op_Reverse_Copy_Strings ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'NEGSI' ) then
    begin
        A := Chr( Op_Negate_Word ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ADDW' ) then
    begin
        A := Chr( Op_Add_Word ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SUBW' ) then
    begin
        A := Chr( Op_Subtract_Word ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'MULW' ) then
    begin
        A := Chr( Op_Multiply_Word ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'DIVW' ) then
    begin
        A := Chr( Op_Divide_Word ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'UEXT' ) then
    begin
        A := Chr( Op_User_Extension ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHNL' ) then
    begin
        if( not Evaluate_Number( Operand1, D, 0, 255, 1, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( not Data_Address_Evaluate( Value, C, 0, 4294967295, 4, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Push_n_Direct_Long ) + chr( D ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POPNL' ) then
    begin
        if( not Evaluate_Number( Operand1, D, 0, 255, 1, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( not Data_Address_Evaluate( Value, C, 0, 4294967295, 4, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Pop_n_Direct_Long ) + chr( D ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHIL' ) then
    begin
        UEC := Evaluate( Value, C ) ;
        if( not Evaluate_Number( Value, C, 0, 4294967295, 4, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Push_Immediate_Long ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RGOTOL' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 5 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 4, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end else
        if( not Evaluate_Number( S, C, Neg2G, 2147483647, 4, Status, True, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Goto_Long_Relative ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RCALLL' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 5 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 4, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end else
        if( not Evaluate_Number( S, C, Neg2G, 2147483647, 4, Status, True, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Long_Relative ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RIF' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 5 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 4, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end else
        if( not Evaluate_Number( S, C, Neg2G, 2147483647, 4, Status, True, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_If_Relative ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CALLD' ) then
    begin
        if( not Data_Address_Evaluate( Value, C, 0, 4294967295, 4, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Dynamic ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CALLN' ) then
    begin
        if( not Data_Address_Evaluate( Value, C, 0, 4294967295, 4, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Native ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'STOREDL' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 4294967295, 4, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Store_Temp0_Direct ) + Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHDW' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Push_Direct_Word ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POPDW' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Pop_Direct_Word ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CALLW' ) then
    begin
        if( not Call_Address_Evaluate( Value, C, 0, 65535, 2, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Word ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'GOTOW' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Goto_Word ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSH0' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Push_Zeroes ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POPN' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Pop_n ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RCALLW' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 3 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 2, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end else
        if( not Evaluate_Number( S, C, -32768, 32767, 2, Status, True, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Word_Relative ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RGOTOW' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 3 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 2, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end else
        if( not Evaluate_Number( S, C, -32768, 32767, 2, Status, True, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Goto_Word_Relative ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'DEREFI' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 255, 1, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Dereference_Integer ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'COPYNI' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 1, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Copy_n_Indirect ) + Chr( C ) + chr( c shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'NEM' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 255, 1, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Memory_Compare_NE ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'EQM' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Memory_Compare_EQ ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LTM' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Memory_Compare_LT ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'GTM' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Memory_Compare_GT ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LEM' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Memory_Compare_LE ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'GEM' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 65535, 2, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Memory_Compare_GE ) + Chr( C ) + chr( C shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHIB' ) then
    begin
        if( not Evaluate_Number( Value, C, 0, 255, 1, Status, False, False ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Push_Immediate_Byte ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CALLB' ) then
    begin
        if( not Call_Address_Evaluate( Value, C, 0, 255, 1, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Byte ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RGOTOB' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 2 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 1, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end else
        if( not Evaluate_Number( S, C, -128, 127, 1, Status, True, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Goto_Byte_Relative ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RCALLB' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 2 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 1, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end else
        if( not Evaluate_Number( S, C, -128, 127, 1, Status, True, True ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Byte_Relative ) + Chr( C ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CALLQ' ) then
    begin
        if( not Call_Address_Evaluate( Value, C, 0, -1, 8, Status ) ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Quad ) +
            Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) +
            chr( C shr 32 ) + chr( C shr 40 ) + chr( C shr 48 ) + chr( C shr 56 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RCALLQ' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 9 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 8, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Call_Quad_Relative ) +
            Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) +
            chr( C shr 32 ) + chr( C shr 40 ) + chr( C shr 48 ) + chr( C shr 56 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'GOTOQ' ) then
    begin
        UEC := Evaluate( Value, C ) ;
        if( ( UEC.Code = 0 ) or ( UEC.Code = 4 ) ) then { A (potential) number }
        begin
            if( UEC.Code = 4 ) then
            begin
                _Master.Add_Reference( PChar( Value ), 8, PC + 1 ) ;
            end ;
            A := Chr( Op_Goto_Quad ) +
                Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) +
                chr( C shr 32 ) + chr( C shr 40 ) + chr( C shr 48 ) + chr( C shr 56 ) ;
            goto End_Assemble ;
        end ;
        goto Ae ;
    end ;
    if( Aa = 'RGOTOQ' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 9 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 8, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_Goto_Quad_Relative ) +
            Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) +
            chr( C shr 32 ) + chr( C shr 40 ) + chr( C shr 48 ) + chr( C shr 56 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'IFQ' ) then
    begin
        UEC := Evaluate( Value, C ) ;
        if( ( UEC.Code = 0 ) or ( UEC.Code = 4 ) ) then { A (potential) number }
        begin
            if( UEC.Code = 4 ) then
            begin
                _Master.Add_Reference( PChar( Value ), 8, PC + 1 ) ;
            end ;
            A := Chr( Op_If_Quad ) +
                Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) +
                chr( C shr 32 ) + chr( C shr 40 ) + chr( C shr 48 ) + chr( C shr 56 ) ;
            goto End_Assemble ;
        end ;
        goto Ae ;
    end ;
    if( Aa = 'RIFQ' ) then
    begin
        S := Value ;
        UEC := Evaluate( S, C ) ;
        if( ( UEC.Code <> 0 ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( PChar( ET( UEC.Code ) + ' "' + S + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC.Code = 4 ) then // Undefined
        begin
            S := '((' + S + ')-' + inttostr( PC + 9 ) + '.)' ;
            _Master.Add_Reference( PChar( S ), 8, PC + 1 ) ;
            C := PC ;
        end else
        if( UEC.Code <> 0 ) then
        begin
            goto AE ;
        end ;
        A := Chr( Op_If_Quad_Relative ) +
            Chr( C ) + chr( C shr 8 ) + chr( C shr 16 ) + chr( C shr 24 ) +
            chr( C shr 32 ) + chr( C shr 40 ) + chr( C shr 48 ) + chr( C shr 56 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHIN' ) then
    begin
        A := Operand1 ;
        if( ( copy( A, 1, 1 ) = '"' ) or ( copy( A, 1, 1 ) = #39 ) ) then
        begin
            S := Parse_String( A ) ;
            A := Chr( Op_Push_n_Immediate ) + Chr( length( S ) ) + S ;
            goto End_Assemble ;
        end ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        UEC := Evaluate( Operand1, C ) ;
        if( not Evaluate_Number( Operand1, C, 0, 255, 1, Status, False, False ) ) then
        begin
            C := 0 ;
        end ;
        S := '' ;
        while( length( S ) < C ) do
        begin
            S := S + chr( 0 ) ;
        end ;
        if( Value = '*' ) then // Followed by .DEF
        begin
            A := Chr( Op_Push_n_Immediate ) + Chr( C ) ;
            goto End_Assemble ;
        end ;
        UEC := Evaluate( Value, C ) ;
        if( ( UEC.Code = 0 ) or ( UEC.Code = 4 ) ) then { A (potential) number }
        begin
            if( UEC.Code = 4 ) then
            begin
                _Master.Add_Reference( PChar( Value ), C, PC + 2 ) ;
            end ;
        end ;
        A := Chr( Op_Push_n_Immediate ) + Chr( C ) + S ;
        goto End_Assemble ;
    end ;
    if( Aa = 'CONL' ) then
    begin
        A := Parse_Parameter( ';', Input_Buffer ) ;
        A := Edit( A, 8 ) ; // Trim leading spaces
        if( ( copy( A, 1, 1 ) = '"' ) or ( copy( A, 1, 1 ) = #39 ) ) then
        begin
            S := Parse_String( A ) ;
            A := Chr( Op_Concat_Literal_To_Temp0 ) + Chr( length( S ) ) + S ;
            goto End_Assemble ;
        end ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( not Evaluate_Number( Operand1, C, 0, 255, 1, Status, False, False ) ) then
        begin
            C := 0 ;
        end ;
        S := '' ;
        while( length( S ) < C ) do
        begin
            S := S + chr( 0 ) ;
        end ;
        if( Value = '*' ) then // Followed by .DEF
        begin
            A := Chr( Op_Concat_Literal_To_Temp0 ) + Chr( C ) ;
            goto End_Assemble ;
        end ;
        UEC := Evaluate( Value, C ) ;
        if( ( UEC.Code = 0 ) or ( UEC.Code = 4 ) ) then { A (potential) number }
        begin
            if( UEC.Code = 4 ) then
            begin
                _Master.Add_Reference( PChar( Value ), C, PC + 2 ) ;
            end ;
        end ;
        A := Chr( Op_Concat_Literal_To_Temp0 ) + Chr( C ) + S ;
        goto End_Assemble ;
    end ;
    if( Aa = 'PUSHNW' ) then
    begin
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( not Evaluate_Number( Operand1, C, 0, 255, 1, Status, False, False ) ) then
        begin
            C := 0 ;
        end ;
        UEC := Evaluate( Value, D ) ;
        if( ( UEC.Code = 0 ) or ( UEC.Code = 4 ) ) then { A (potential) number }
        begin
            if ( D < 0 ) or ( D > 65535 ) then
            begin
                Status.Log_Error( PChar( 'Value out of range: ' + inttostr( C ) ), nil, -1, 3 ) ;
                goto Ae ;
            end ;
            if( UEC.Code = 4 ) then
            begin
                _Master.Add_Reference( PChar( Value ), 2, PC + 2 ) ;
            end ;
        end ;
        A := Chr( Op_Push_n_Direct_Word ) + Chr( C ) + chr( D ) + chr( D shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POPNW' ) then
    begin
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( not Evaluate_Number( Operand1, C, 0, 255, 1, Status, False, False ) ) then
        begin
            C := 0 ;
        end ;
        UEC := Evaluate( Value, D ) ;
        if( ( UEC.Code = 0 ) or ( UEC.Code = 4 ) ) then { A (potential) number }
        begin
            if ( D < 0 ) or ( D > 65535 ) then
            begin
                Status.Log_Error( PChar( 'Value out of range: ' + inttostr( C ) ), nil, -1, 3 ) ;
                goto Ae ;
            end ;
            if( UEC.Code = 4 ) then
            begin
                _Master.Add_Reference( PChar( Value ), 2, PC + 2 ) ;
            end ;
        end ;
        A := Chr( Op_Pop_n_Direct_Word ) + Chr( C ) + chr( D ) + chr( D shr 8 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POPNW' ) then
    begin
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( not Evaluate_Number( Operand1, C, 0, 255, 1, Status, False, False ) ) then
        begin
            C := 0 ;
        end ;
        UEC := Evaluate( Value, D ) ;
        if( ( UEC.Code = 0 ) or ( UEC.Code = 4 ) ) then { A (potential) number }
        begin
            if ( D < 0 ) or ( D > 65535 ) then
            begin
                Status.Log_Error( PChar( 'Value out of range: ' + inttostr( D ) ), nil, -1, 3 ) ;
                goto Ae ;
            end ;
            if( UEC.Code = 4 ) then
            begin
                _Master.Add_Reference( PChar( Value ), 4, PC + 2 ) ;
            end ;
        end ;
        A := Chr( Op_Pop_n_To_Stack_Indirect_Long ) + Chr( C ) + chr( D ) + chr( D shr 8 ) + chr( D shr 16 ) + chr( D shr 24 ) ;
        goto End_Assemble ;
    end ;
    case Handle_Integer_Opcode( 'NEI', Op_Integer_Compare_NE ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'EQI', Op_Integer_Compare_EQ ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'LTI', Op_Integer_Compare_LT ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'GTI', Op_Integer_Compare_GT ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'LEI', Op_Integer_Compare_LE ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'GEI', Op_Integer_Compare_GE ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'NEGIS', Op_Negate_Stack ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'ADDI', Op_Add_Integer ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'SUBI', Op_Subtract_Integer ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'MULI', Op_Multiply_Integer ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'DIVI', Op_Divide_Integer ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'PERI', Op_Percent_Integer ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'EXPI', Op_Exponent_Integer ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    case Handle_Integer_Opcode( 'FACI', Op_Factorial_Integer ) of
        0 : goto End_Assemble ; // Valid
        1 : goto AE ; // Error
        // else no match
    end ;
    if( Length( Aa ) > 0 ) then
    begin
        Status.Log_Error( PChar( 'Unknown instruction: "' + AA + '"' ), nil, -1, 3 ) ;
AE:
        // Unrecognized mnuemonic...
        _Master.Put_Token( PChar( AA ) ) ;
        UEC := _Master.Expand( nil, P, PL, Status ) ;
        if( UEC.Code <> 0 ) then
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
    if( PC + MachineL > High_Code ) then
    begin
        High_Code := PC + MachineL ;
    end ;
end ; // TSVM_Assembler.Assemble


{ Returns the default radix (base) of numeric literals. }
function TSVM_Assembler.Default_Radix : longint ;

begin
    Result := Base ;
end ;


{ Returns the default size of numeric literals, in bits. }
function TSVM_Assembler.Default_Size : longint ;

begin
    Default_Size := 8 ;
end ;


{ Returns facility code for this class. }
function TSVM_Assembler.Facility_Code : longint ;

begin
    Result := SVMAssemblerErr_Facility ;
end ;


const _Extensions : string = 'asm' ;

function TSVM_Assembler.Source_Extensions : PChar ;

begin
    Result := PChar( _Extensions ) ;
end ;


const _Valid_Symbol_Initial : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_' ;

function TSVM_Assembler.Valid_Symbol_Initial : PChar ;

begin
    Result := PChar( _Valid_Symbol_Initial ) ;
end ;


const _Valid_Symbol_After : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_0123456789' ;

function TSVM_Assembler.Valid_Symbol_After : PChar ;

begin
    Result := PChar( _Valid_Symbol_After ) ;
end ;


function TSVM_Assembler.Begin_Assembly : int64 ;

begin
    In_Assembly := True ;
    Result := 0 ;
end ;


function TSVM_Assembler.Finish_Assembly( Context : int64 ; var outputs, machines : PChar ;
    var MachineL : longint ; var Address : int64 ; Status : TAssembler_Status ;
    Flags : longint ) : TUEC ;

begin
    if( assigned( On_Finish ) ) then
    begin
        On_Finish( self, Status, Address ) ;
    end ;
    In_Assembly := False ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TSVM_Assembler.Request_Data( Address, Size : int64 ) : int64 ;

begin
    Result := Address ;
    if( Size > 65536 ) then
    begin
        Result := -1 ; // Cannot allocate more than 64Kb at a time
        exit ;
    end ;
    if( ( Address shr 16 ) <> ( ( Address + Size ) shr 16 ) ) then // Crosses 64Kb boundary
    begin
        Result := ( Address + Size ) and ( not $FFFF ) ;
    end ;
    if( Result + Size > $7FFFFFFF ) then // Exceeded maximum data space
    begin
        Result := -1 ; // Cannot allocate more than 64Kb at a time
    end ;
end ;


end.
