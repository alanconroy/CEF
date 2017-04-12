{
              Program Name : Z80
              Package Name : Z80
              Purpose      : 8080/8085/Z80 assembler
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

          This unit defines the assembler for the CEF Z80 component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Z80ASM ;

interface

uses { CEF... }
     _CEF, // TMaster_Assembler
     CEF, { TBase_Assembler }

     { Other... }
     _UE, // TUnified_Exception
     CommonUt ; // TInteger_List

const Z80AssemblerErr_Facility = 52 ;
      Z80AssemblerErr_Success = 0 ;
      Z80AssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
      Z80AssemblerErr_Illegal_Expression = 2 ; // Illegal expression
      Z80AssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
      Z80AssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol

type TMode = ( Z80, M8080, M8085 ) ;

type TZ80_Assembler = class( TBase_Assembler )
                          public // Instance data...
                              Assembled_Code : string ;
                              Base : integer ;
                              Err : boolean ; { True if an error }
                              _Master : TMaster_Assembler ;
                              Mode : TMode ;
                              PC : integer ; { Current target address }
                              CPU : TCPU ;
                              Segments : TInteger_List ;

                          private // Internal utility routines...
                              procedure Add_Segments( S1, S2, S3 : integer ) ;
                              function Addss( A : String ) : Integer ;
                              function AQQ( A : string ) : integer ;
                              function Ar( A : String ) : Integer ;
                              function ARR( A : string ) : integer ;
                              function Aix( const A : String ) : integer ;
                              function Aiy( const A : String ) : integer ;
                              function Handle_Directives( var Value : string ;
                                  Status : TAssembler_Status ) : boolean ;

                          protected        
                              function Evaluate( const X : string ;
                                  var _Result : longint ;
                                  PC_Adjustment : integer ) : TUnified_Exception ;
                                  override ;

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

     Z80CPU, // TZ80_CPU
     ASCIIDef, // CR
     Express,
     Parse, Standard, CVT, Helps, Instrs, Maths, NUM1S, UStrings ;

// TZ80_Assembler methods...

procedure TZ80_Assembler.Initialize( Master : TMaster_Assembler ) ;

begin
    _Master := Master ;
end ;


{ Informs the assembler that the current assembly operation is now
  complete. }
procedure TZ80_Assembler.Terminate ;

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


function TZ80_Assembler.Assemble_Ex( Inputs : PChar ;
    var Outputs, Machines : PChar ;
    var MachineL : longint ; var Address : int64 ;
    var Segment : longint ; Status : TAssembler_Status ;
    Flags : longint ) : TUnified_Exception ;

    function Ac( A : String ) : Integer ; {Condition field c}

    begin
        Ac := -1 ;
        if( A = 'NZ' ) then Ac := 0 ;
        if( A = 'Z' ) then Ac := 1 ;
        if( A = 'NC' ) then Ac := 2 ;
        if( A = 'C' ) then Ac := 3 ;
        if( A = 'PO' ) then Ac := 4 ;
        if( A = 'PE' ) then Ac := 5 ;
        if( A = 'P' ) then Ac := 6 ;
        if( A = 'M' ) then Ac := 7 ;
    end ;


    function APP( A : String ) : Integer ; {Register pair pp}

    begin
        App := -1 ;
        if( A = 'BC' ) then App := 0 ;
        if( A = 'DE' ) then App := 1 ;
        if( A = 'IX' ) then App := 2 ;
        if( A = 'SP' ) then App := 3 ;
    end ;


var Er : boolean ; // True if an error
    _Value : string ;
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


    function Assemble_Z80A( var A : string ; AA, Operand1 : string ;
        B, C, D : longint ) : integer ;

        function Range_Value( Operand1 : string ; Low, High : integer ;
            var B : longint ; Size, Offset : integer ) : boolean ;

        var UEC : TUnified_Exception ;
            Work, Work1 : string ;

        begin
            Work := Grab_Line ;
            Work1 := Work ;
            Work := Parse_Parameter( ';', Work1 ) ;
	        UEC := Evaluate( Work, B, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                Range_Value := False ;
                exit ;
            end ;
            Range_Value := True ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then // Undefined symbol
                begin
                    _Master.Add_Reference( PChar( Operand1 ), Size, PC + Offset ) ;
                end else
                begin
                    Assemble_Z80A := 1 ;
                    Range_Value := False ;
                end ;
            end else
            if( ( B < Low ) or ( B > High ) ) then
            begin
                Assemble_Z80A := 1 ;
                Range_Value := False ;
            end ;
        end ;

    var UEC : TUnified_Exception ;

    begin
	Assemble_Z80A := 0 ; { Assume no matches }
	if( Aa = 'AND' ) then {Logical AND}
	begin
            if( Length( Value ) > 0 ) then
	    begin
		Assemble_Z80A := 1 ; { Only one operand allowed }
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
		A := Chr( $A6 ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then {IX}
            begin
		A := Chr( $DD ) + Chr( $A6 ) + Chr( B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 1, 0 ) ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B > -32767 then {IY}
            begin
		A := Chr( $FD ) + Chr( $A6 ) + Chr( B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 1, 0 ) ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
		A := Chr( $A0 or B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
	    if( not Range_Value( Operand1, -128, 255, B, 1, 1 ) ) then
	    begin
		A := Chr( $E6 ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
	end ;
	if( Aa = 'BIT' ) then {Bit test}
	begin
            if( Range_Value( Operand1, 0, 7, B, -$46, 3 ) ) then
	    begin
		exit ;
	    end ;
            B := B * 8 ;
            C := Aix( Value ) ;
            if C > -32767 then {IX}
            begin
		A := Chr( $DD ) + Chr( $CB ) + Chr( C ) + Chr( $46 or B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 2, 0 ) ;
		exit ;
            end ;
            C := Aiy( Value ) ;
            if C > -32767 then {IY}
            begin
                A := Chr( $FD ) + Chr( $CB ) + Chr( C ) + Chr( $46 or B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 2, 0 ) ;
		exit ;
            end ;
            C := Ar( Value ) ;
            if( C = -1 ) then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $CB ) + Chr( $40 or B or C ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 0, 0 ) ;
	    end ;
	    exit ;
	end ;
        if( Aa = 'CP' ) then {Compare}
        begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ; { Only one operand allowed }
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $BE ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then {IX}
            begin
                A := Chr( $DD ) + Chr( $BE ) + Chr( B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 1, 0 ) ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B > -32767 then {IY}
            begin
                A := Chr( $FD ) + Chr( $BE ) + Chr( B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 1, 0 ) ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
                A := Chr( $B8 or B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
            if( Range_Value( Operand1, -128, 255, B, 1, 1 ) ) then
 	    begin
		exit ;
	    end ;
            A := Chr( $FE ) + Chr( B ) ;
	    Assemble_Z80A := 2 ;
            Add_Segments( 1, 1, 0 ) ;
	    exit ;
        end ;
        if Aa = 'CPD' then {Block compare, no repeat}
        begin
            A := Chr( $ED ) + Chr( $A9 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ; { No operands }
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
            Add_Segments( 2, 0, 0 ) ;
	    exit ;
        end ;
        if Aa = 'CPDR' then {Block compare, repeat}
        begin
            A := Chr( $ED ) + Chr( $B9 ) ;
            if( Length( Operand1 + Value ) > 0 ) then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
            Add_Segments( 2, 0, 0 ) ;
            exit ;
        end ;
        if Aa = 'CPI' then { Block compare, no repeat }
        begin
            A := Chr( $ED ) + Chr( $A1 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ; { No operands }
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
            Add_Segments( 2, 0, 0 ) ;
            exit ;
        end ;
        if Aa = 'CPIR' then {Block compare, repeat}
        begin
            A := Chr( $ED ) + Chr( $B1 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ; { No operands }
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
            Add_Segments( 2, 0, 0 ) ;
	    exit ;
        end ;
	if( Aa = 'DEC' ) then { Decrement }
	begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ; { Only one operand allowed }
	    end ;
            if Operand1 = '(HL)' then
            begin
        	A := Chr( $35 ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
		B := B * 8 ;
		A := Chr( $05 or B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
            if Operand1 = 'IX' then
            begin
		A := Chr( $DD ) + Chr( $2B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 0, 0 ) ;
		exit ;
            end ;
            if Operand1 = 'IY' then
            begin
		A := Chr( $FD ) + Chr( $2B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 0, 0 ) ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then {IX}
            begin
		A := Chr( $DD ) + Chr( $35 ) + Chr( B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 1, 0 ) ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B > -32767 then {IY}
            begin
		A := Chr( $FD ) + Chr( $35 ) + Chr( B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 1, 0 ) ;
		exit ;
            end ;
            B := Addss( Operand1 ) ;
            if( B < 0 ) then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            B := B * 16 ;
            A := Chr( $0B or B ) ;
	    Assemble_Z80A := 2 ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
	end ;
	if( Aa = 'DJNZ' ) then { Decrement and jump not zero }
	begin
	    UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
		    Assemble_Z80A := 1 ;
            exit ;
        end ;
	    if( UEC <> nil ) then
	    begin
            if( UEC.Get_Error = 4 ) then // Undefined symbol
            begin
                _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                B := 0 ;
            end else
            begin
                Assemble_Z80A := 1 ;
                exit ;
            end ;
	        end else
            begin
                B := B - 2 ;
                if( ( B < -128 ) or ( B > 127 ) or ( Length( Value ) > 0 ) ) then
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
            end ;
            A := Chr( $10 ) + Chr( B ) ;
            Add_Segments( 1, 1, 0 ) ;
	    Assemble_Z80A := 2 ;
	    exit ;
	end ;
        if Aa = 'EX' then {Exchange}
        begin
            if Operand1 = '(SP)' then
            begin
                if Value = 'HL' then
                begin
                    A := Chr( $E3 ) ;
		    Assemble_Z80A := 2 ;
                    Add_Segments( 1, 0, 0 ) ;
		    exit ;
                end ;
                if Value = 'IX' then
                begin
                    A := Chr( $DD ) + Chr( $E3 ) ;
		    Assemble_Z80A := 2 ;
                    Add_Segments( 2, 0, 0 ) ;
		    exit ;
                end ;
                if Value = 'IY' then
                begin
                    A := Chr( $FD ) + Chr( $E3 ) ;
		    Assemble_Z80A := 2 ;
                    Add_Segments( 2, 0, 0 ) ;
                end else
		begin
		    Assemble_Z80A := 1 ;
		end ;
		exit ;
            end ;
            if Operand1 = 'AF' then
            begin
                if Value <> 'AF' + Chr( 39 ) then
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
                A := Chr( $08 ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if ( Operand1 <> 'DE' ) or ( Value <> 'HL' ) then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            A := Chr( $EB ) ;
            Add_Segments( 1, 0, 0 ) ;
	    Assemble_Z80A := 2 ;
	    exit ;
        end ;
        if Aa = 'EXX' then {Change register sets}
        begin
            A := Chr( $D9 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ; {No operands}
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
        end ;
	if( Aa = 'IM' ) then {Set interrupt mode}
	begin
	    if( Range_Value( Operand1, 0, 2, B, 0, 0{~~~} ) or ( length( Value ) > 0 ) ) then
	    begin
		exit ;
	    end ;
            case B of
		0 : A := Chr( $ED ) + Chr( $46 ) ;
		1 : A := Chr( $ED ) + Chr( $56 ) ;
		2 : A := Chr( $ED ) + Chr( $5E ) ;
		else
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
	    end ;
            Add_Segments( 2, 0, 0 ) ;
	    Assemble_Z80A := 2 ;
	    exit ;
	end ;
        if Aa = 'INC' then { Increment }
        begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $34 ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
            if Operand1 = 'IX' then
            begin
                A := Chr( $DD ) + Chr( $23 ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 0, 0 ) ;
		exit ;
            end ;
            if Operand1 = 'IY' then
            begin
                A := Chr( $FD ) + Chr( $23 ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 0, 0 ) ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
                B := B * 8 ;
                A := Chr( $04 or B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
                A := Chr( $DD ) + Chr( $34 ) + Chr( B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 1, 0 ) ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B > -32767 then
            begin
                A := Chr( $FD ) + Chr( $34 ) + Chr( B ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 2, 1, 0 ) ;
		exit ;
            end ;
            B := Addss( Operand1 ) ;
            if B < 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            B := B * 16 ;
            A := Chr( $03 or B ) ;
	    Assemble_Z80A := 2 ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
        end ;
        if Aa = 'IND' then {Block input}
        begin
            A := Chr( $ED ) + Chr( $AA ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
	    Assemble_Z80A := 2 ;
            Add_Segments( 2, 0, 0 ) ;
	    exit ;
        end ;
        if Aa = 'INDR' then {Block input, repeat}
        begin
            A := Chr( $ED ) + Chr( $Ba ) ;
            if Length( Operand1 + Value ) > 0 then
            begin
                Assemble_Z80A := 1 ;
                exit ;
            end ;
            Assemble_Z80A := 2 ;
            Add_Segments( 2, 0, 0 ) ;
            exit ;
        end ;
        if Aa = 'INI' then {Block input}
        begin
            A := Chr( $ED ) + Chr( $A2 ) ;
            if Length( Operand1 + Value ) > 0 then
            begin
                Assemble_Z80A := 1 ;
                exit ;
            end ;
            Assemble_Z80A := 2 ;
            Add_Segments( 2, 0, 0 ) ;
            exit ;
        end ;
        if Aa = 'INIR' then {Block input, repeat}
        begin
            A := Chr( $ED ) + Chr( $B2 ) ;
            if Length( Operand1 + Value ) > 0 then
            begin
                Assemble_Z80A := 1 ;
                exit ;
            end ;
            Assemble_Z80A := 2 ;
            Add_Segments( 2, 0, 0 ) ;
            exit ;
        end ;
        if( AA = 'JP' ) then { Jump }
        begin
            if Length( Value ) > 0 then
            begin
                B := Ac( Operand1 ) ;
                if( B < 0 ) then // Jump to address
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
        UEC := Evaluate( Value, C, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            Assemble_Z80A := 1 ;
            exit ;
        end ;
        if( UEC <> nil ) then
		begin
            if( UEC.Get_Error = 4 ) then // Undefined symbol
            begin
                _Master.Add_Reference( PChar( Value ), 2, PC + 1 ) ;
            end else
            begin
                Assemble_Z80A := 1 ;
                exit ;
            end ;
		end ;
        B := B * 8 ;
        A := Chr( $C2 or B ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
        Add_Segments( 1, 2, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $E9 ) ;
		Assemble_Z80A := 2 ;
                Add_Segments( 1, 0, 0 ) ;
		exit ;
            end ;
            if Operand1 = '(IX)' then
            begin
                A := Chr( $DD ) + Chr( $E9 ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = '(IY)' then
            begin
                A := Chr( $FD ) + Chr( $E9 ) ;
                Add_Segments( 2, 0, 0 ) ;
                Assemble_Z80A := 2 ;
                exit ;
            end ;
            UEC := Evaluate( Operand1, B, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                Assemble_Z80A := 1 ;
                exit ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then // Undefined symbol
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
                end else
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
            end ;
            A := Chr( $C3 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
            Add_Segments( 1, 2, 0 ) ;
            Assemble_Z80A := 2 ;
            exit ;
        end ;
        if( Aa = 'JR' ) then { Jump relative }
        begin
            if( Length( Value ) = 0 ) then // Direct to address
            begin
		        UEC := Evaluate( Operand1, B, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if( UEC <> nil ) then
                begin
                    if( UEC.Get_Error = 4 ) then // Undefined symbol
                    begin
                        _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                    end else
                    begin
                        Assemble_Z80A := 1 ;
                        exit ;
                    end ;
                end else
                begin
                    B := B - 2 ;
                    if( ( B < -128 ) or ( B > 255 ) ) then
                    begin
                        Assemble_Z80A := 1 ;
                        exit ;
                    end ;
                end ;
                A := Chr( $18 ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if( Value <> ',' ) then
            begin
                Status.Log_Error( PChar( 'Expected ",", found "' + AA + '"' ), nil, -1, 3 ) ;
                Assemble_Z80A := 1 ;
                exit ;
            end ;
            A := Get_Token ;
            if( A = '+' ) then
            begin
                A := Get_Token ;
            end else
            if( A = '-' ) then
            begin
                A := '-' + Get_Token ;
            end ;
            if( Peek_Token = '.' ) then
            begin
                Get_Token ;
                A := A + '.' ;
            end ;
            UEC := Evaluate( A, B, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                Assemble_Z80A := 1 ;
                exit ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then // Undefined symbol
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                end else
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
            end else
            begin
                B := B - 2 ;
                if( ( B < -128 ) or ( B > 127 ) or Er ) then
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
            end ;
            if Operand1 = 'C' then
            begin
                A := Chr( $38 ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = 'NC' then
            begin
                A := Chr( $30 ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = 'NZ' then
            begin
                A := Chr( $20 ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 <> 'Z' then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            A := Chr( $28 ) + Chr( B ) ;
            Add_Segments( 1, 1, 0 ) ;
	    Assemble_Z80A := 2 ;
	    exit ;
        end ;
        if( Aa = 'LD' ) then { Load }
        begin
            AA := Get_Token ;
            if( AA <> ',' ) then
            begin
                Status.Log_Error( PChar( 'Expected ",", found "' + AA + '"' ), nil, -1, 3 ) ;
                Assemble_Z80A := 1 ;
                exit ;
            end ;
            if Operand1 = 'A' then { LD A,? }
            begin
                if Value = '(BC)' then
                begin
                    A := Chr( $0A ) ;
                    Add_Segments( 1, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                if Value = '(DE)' then
                begin
                    A := Chr( $1A ) ;
                    Add_Segments( 1, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                if Value = '(HL)' then
                begin
                    A := Chr( $7E ) ;
                    Add_Segments( 1, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                if Value = 'I' then
                begin
                    A := Chr( $ED ) + Chr( $57 ) ;
                    Add_Segments( 2, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                if Value = 'R' then
                begin
                    A := Chr( $ED ) + Chr( $5F ) ;
                    Add_Segments( 2, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                B := Ar( Value ) ;
                if B >= 0 then
                begin
                    A := Chr( $78 or B ) ;
                    Add_Segments( 1, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                UEC := Evaluate( Value, B, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if( UEC = nil ) then
                begin
                    A := Chr( $3E ) + Chr( B ) ;
                    Add_Segments( 1, 1, 0 ) ;
                    Assemble_Z80A := 2 ;
                    exit ;
                end else
                if( UEC.Get_Error = 4 ) then // Undefined symbol
                begin
                    _Master.Add_Reference( PChar( Value ), 1, PC + 1 ) ;
                end else
                if ( Value[ 1 ] <> '(' ) or ( Value[ Length( Value ) ] <> ')' ) then
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                UEC := Evaluate( Copy( Value, 2, Length( Value ) - 2 ), B, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if( UEC = nil ) then
                begin
                    A := Chr( $3A ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 1, 2, 0 ) ;
                    Assemble_Z80A := 2 ;
                    exit ;
                end ;
                if( UEC.Get_Error = 4 ) then // Undefined symbol
                begin
                    _Master.Add_Reference( PChar( Copy( Value, 2, Length( Value ) - 2 ) ), 2, PC + 1 ) ;
                end ;
                B := Aix( Value ) ;
                if( B > -32767 ) then
                begin
                    A := Chr( $DD ) + Chr( $7E ) + Chr( B ) ;
                    Add_Segments( 2, 1, 0 ) ;
                    Assemble_Z80A := 2 ;
                    exit ;
                end ;
                B := Aiy( Value ) ;
                if B = -32767 then
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
                A := Chr( $FD ) + Chr( $7E ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = '(BC)' then { LD (BC),? }
            begin
                if Value <> 'A' then
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
                A := Chr( $2 ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = '(DE)' then { LD (DE),? }
            begin
                if Value <> 'A' then
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
                A := Chr( $12 ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = '(HL)' then { LD (HL),? }
            begin
                B := Ar( Value ) ;
                if B >= 0 then
                begin
                    A := Chr( $70 or B ) ;
                    Add_Segments( 1, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
		if( Range_Value( Value, -128, 255, B, 1, 1 ) ) then
		begin
		    exit ;
		end ;
                A := Chr( $36 ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if( Operand1 = 'HL' ) then { LD HL,(?) }
            begin
                if( ( Value[ 1 ] = '(' ) and ( Value[ Length( Value ) ] = ')' ) ) then
                begin
                    UEC := Evaluate( Copy( Value, 2, Length( Value ) - 2 ), B, 0 ) ;
                    if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                    begin
                        Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                        Assemble_Z80A := 1 ;
                        exit ;
                    end ;
                    if( UEC <> nil ) then
                    begin
                        if( UEC.Get_Error = 4 ) then // Undefined symbol
                        begin
                            _Master.Add_Reference( PChar( Copy( Value, 2, Length( Value ) - 2 ) ), 2, PC + 1 ) ;
                        end else
                        begin
                            Assemble_Z80A := 1 ;
                            exit ;
                        end ;
                    end ;
                    A := Chr( $2A ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 1, 2, 0 ) ;
                    Assemble_Z80A := 2 ;
                    exit ;
                end ;
            end ;
            if Operand1 = 'I' then { LD I,? }
            begin
                if Value <> 'A' then
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
                A := Chr( $ED ) + Chr( $47 ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = 'IX' then { LD IX,? }
            begin
                if ( Value[ 1 ] = '(' ) and ( Value[ Length( Value ) ] = ')' ) then
                begin
		    UEC := Evaluate( Copy( Value, 2, Length( Value ) - 2 ), B, 0 ) ;
                    if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                    begin
                        Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                        Assemble_Z80A := 1 ;
                        exit ;
                    end ;
                    if( UEC <> nil ) then
                    begin
                        if( UEC.Get_Error = 4 ) then // Undefined symbol
                        begin
                            _Master.Add_Reference( PChar( Copy( Value, 2, Length( Value ) - 2 ) ), 2, PC + 2 ) ;
                        end else
                        begin
                            Assemble_Z80A := 1 ;
                            exit ;
                        end ;
                    end ;
                    A := Chr( $DD ) + Chr( $2A ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 2, 2, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
		UEC := Evaluate( Value, B, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if( UEC <> nil ) then
                begin
                    if( UEC.Get_Error = 4 ) then // Undefined symbol
                    begin
                        _Master.Add_Reference( PChar( Value ), 2, PC + 2 ) ;
                    end else
                    begin
                        Assemble_Z80A := 1 ;
                        exit ;
                    end ;
                end ;
                A := Chr( $DD ) + Chr( $21 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                Add_Segments( 2, 2, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = 'IY' then { LD IY,? }
            begin
                if ( Value[ 1 ] = '(' ) and ( Value[ Length( Value ) ] = ')' ) then
                begin
                    UEC := Evaluate( Copy( Value, 2, Length( Value ) - 2 ), B, 0 ) ;
                    if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                    begin
                        Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                        Assemble_Z80A := 1 ;
                        exit ;
                    end ;
                    if( UEC <> nil ) then
                    begin
                        if( UEC.Get_Error = 4 ) then // Undefined symbol
                        begin
                            _Master.Add_Reference( PChar( Copy( Value, 2, Length( Value ) - 2 ) ), 2, PC + 2 ) ;
                        end else
                        begin
                            Assemble_Z80A := 1 ;
                            exit ;
                        end ;
                    end ;
                    A := Chr( $FD ) + Chr( $2A ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 2, 2, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                UEC := Evaluate( Value, B, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if( UEC <> nil ) then
                begin
                    if( UEC.Get_Error = 4 ) then // Undefined symbol
                    begin
                        _Master.Add_Reference( PChar( Value ), 2, PC + 2 ) ;
                    end else
                    if Er then
                    begin
                        Assemble_Z80A := 1 ;
                        exit ;
                    end ;
                end ;
                A := Chr( $FD ) + Chr( $21 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                Add_Segments( 2, 2, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 = 'R' then { LD R,? }
            begin
                if Value <> 'A' then
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
                A := Chr( $ED ) + Chr( $4F ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if( Operand1 = 'SP' ) then { LD SP,? }
            begin
                if( Value = 'HL' ) then { LD SP, HL }
                begin
                    A := Chr( $F9 ) ;
                    Add_Segments( 1, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                if Value = 'IX' then { LD SP, IX }
                begin
                    A := Chr( $DD ) + Chr( $F9 ) ;
                    Add_Segments( 2, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
		if( Value = 'IY' ) then { LD SP, IY }
		begin
		    A := Chr( $FD ) + Chr( $F9 ) ;
                    Add_Segments( 2, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
		end ;
            end ; { if( Operand1 = 'SP' ) }
            B := Aix( Operand1 ) ;
            if B > -32767 then { IX }
            begin
                C := Ar( Value ) ;
                if( C >= 0 ) then
                begin
                    A := Chr( $DD ) + Chr( $70 or C ) + Chr( B ) ;
                    Add_Segments( 2, 1, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
		if( Range_Value( Value, -128, 255, C, 1, 3 ) ) then
		begin
		    exit ;
		end ;
                A := Chr( $DD ) + Chr( $36 ) + Chr( B ) + Chr( C ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B > -32767 then { IY }
            begin
                C := Ar( Value ) ;
                if( C >= 0 ) then
                begin
                    A := Chr( $FD ) + Chr( $70 or C ) + Chr( B ) ;
                    Add_Segments( 2, 1, 0 ) ;
                    Assemble_Z80A := 2 ;
                    exit ;
                end ;
                if( Range_Value( Value, -128, 255, C, 1, 3 ) ) then
                begin
                    exit ;
                end ;
                A := Chr( $FD ) + Chr( $36 ) + Chr( B ) + Chr( C ) ;
                Add_Segments( 2, 2, 0 ) ;
                Assemble_Z80A := 2 ;
                exit ;
            end ;
            if Operand1[ 1 ] = '(' then { LD (?),? }
            begin
                if Operand1[ Length( Operand1 ) ] <> ')' then
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                Operand1 := Copy( Operand1, 2, Length( Operand1 ) - 2 ) ;
                UEC := Evaluate( Operand1, B, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if( ( UEC <> nil ) and ( UEC.Get_Error <> 4 ) ) then
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if Value = 'A' then
                begin
                    if( UEC <> nil ) then // Undefined symbol
                    begin
                        _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
                    end ;
                    A := Chr( $32 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 1, 2, 0 ) ;
                    Assemble_Z80A := 2 ;
                    exit ;
                end ;
                if Value = 'HL' then
                begin
                    if( UEC <> nil ) then // Undefined symbol
                    begin
                        _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
                    end ;
                    A := Chr( $22 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 1, 2, 0 ) ;
                    Assemble_Z80A := 2 ;
                    exit ;
                end ;

                if( UEC <> nil ) then // Undefined symbol
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 2, PC + 2 ) ;
                end ;
                if Value = 'IX' then
                begin
                    A := Chr( $DD ) + Chr( $22 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 2, 2, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                if Value = 'IY' then
                begin
                    A := Chr( $FD ) + Chr( $22 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 2, 2, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                C := Addss( Value ) ;
                if C >= 0 then
                begin
                    C := C * 16 ;
                    A := Chr( $ED ) + Chr( C or $43 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                    Add_Segments( 2, 2, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                C := Ar( Value ) ;
                if C < 0 then
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
                C := C * 16 ;
                A := Chr( $ED ) + Chr( $43 or C ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
                Add_Segments( 2, 2, 0 ) ;
                Assemble_Z80A := 2 ;
                exit ;
            end ;

	    { LD ?, ? }
            B := AR( Operand1 ) ; { See if operand 1 is a register }
            if( B >= 0 ) then { yes }
            begin
                B := B * 8 ;
                if Value = '(HL)' then
                begin
                    A := Chr( $46 or B ) ;
                    Add_Segments( 1, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                C := Ar( Value ) ; { See if second operand is a register }
                if( C >= 0 ) then { yes }
                begin
                    A := Chr( $40 or B or C ) ; { LD r,r' }
                    Add_Segments( 1, 0, 0 ) ;
		    Assemble_Z80A := 2 ;
		    exit ;
                end ;
                UEC := Evaluate( Value, C, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if( ( UEC = nil ) or ( UEC.Get_Error = 4 ) ) then
                begin
                    _Master.Add_Reference( PChar( Value ), 1, PC + 1 ) ;
                    if ( C < -128 ) or ( C > 255 ) then
                    begin
                        Assemble_Z80A := 1 ;
                    end else
                    begin
                        A := Chr( $06 or B ) + Chr( C ) ;
                        Add_Segments( 1, 1, 0 ) ;
                        Assemble_Z80A := 2 ;
                    end ;
                    exit ;
                end ;
                if( UEC <> nil ) then
                begin
                    _Master.Add_Reference( PChar( Value ), 1, PC + 2 ) ;
                end ;
                C := Aix( Value ) ;
                if C > -32767 then
                begin
                    A := Chr( $FD ) + Chr( $46 or B ) + Chr( C ) ;
                    Add_Segments( 2, 1, 0 ) ;
                    Assemble_Z80A := 2 ;
                    exit ;
                end ;
                C := Aiy( Value ) ;
                if C = -32767 then
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                A := Chr( $DD ) + Chr( $46 or B ) + Chr( C ) ;
                Add_Segments( 2, 1, 0 ) ;
                Assemble_Z80A := 2 ;
                exit ;
            end ;
            B := Addss( Operand1 ) ;
            if B = -1 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            B := B * 16 ;
            if Value[ 1 ] = '(' then
            begin
                if Value[ Length( Value ) ] <> ')' then
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                UEC := Evaluate( copy( Value, 2, Length( Value ) - 2 ), C, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
                if( UEC <> nil ) then
                begin
                    if( UEC.Get_Error = 4 ) then
                    begin
                        _Master.Add_Reference( PChar( copy( Value, 2, Length( Value ) - 2 ) ), 2, PC + 2 ) ;
                    end else
                    begin
                        Assemble_Z80A := 1 ;
                        exit ;
                    end ;
                end ;
                A := Chr( $ED ) + Chr( $4B or B ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
                Add_Segments( 2, 2, 0 ) ;
                Assemble_Z80A := 2 ;
                exit ;
            end ;
            UEC := Evaluate( Value, C, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                Assemble_Z80A := 1 ;
                exit ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Value ), 2, PC + 1 ) ;
                end else
                begin
                    Assemble_Z80A := 1 ;
                    exit ;
                end ;
            end ;
            A := Chr( $01 or B ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
            Add_Segments( 1, 2, 0 ) ;
            Assemble_Z80A := 2 ;
	    exit ;
        end ; { if( AA = 'LD' ) }
        if Aa = 'LDD' then {Block load, forward, no repeat}
        begin
            A := Chr( $ED ) + Chr( $A8 ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if( Aa = 'LDDR' ) then {Block load, forward, repeat}
        begin
            A := Chr( $ED ) + Chr( $B8 ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'LDI' then {Block load, backward, no repeat}
        begin
            A := Chr( $ED ) + Chr( $A0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if( Aa = 'LDIR' ) then {Block load, backward, repeat}
        begin
            A := Chr( $ED ) + Chr( $B0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'NEG' then {Negate (2's complement)}
        begin
            A := Chr( $ED ) + Chr( $44 ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'OR' then {Logical OR}
        begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $B6 ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
                A := Chr( $B0 or B ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then {IX}
            begin
                A := Chr( $DD ) + Chr( $B6 ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B > -32767 then {IY}
            begin
                A := Chr( $FD ) + Chr( $B6 ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
	    if( not Range_Value( Operand1, -128, 255, B, 1, 1 ) ) then
	    begin
		A := Chr( $F6 ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if( Aa = 'OTDR' ) then { Block output, backward, repeat }
        begin
            A := Chr( $ED ) + Chr( $BB ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if( Aa = 'OTIR' ) then { Block output, forward, repeat }
        begin
            A := Chr( $ED ) + Chr( $B3 ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'OUTD' then {Block output, backward, no repeat}
        begin
            A := Chr( $ED ) + Chr( $AB ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'OUTI' then {Block output, forward, no repeat}
        begin
            A := Chr( $ED ) + Chr( $A3 ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
	if( Aa = 'RES' ) then {Bit reset}
	begin
            if( Value = '(HL)' ) then
            begin
                if( Range_Value( Operand1, 0, 7, B, -$86, 1 ) ) then
                begin
                    exit ;
                end ;
                B := B * 8 ;
		A := Chr( $CB ) + Chr( $86 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            C := Ar( Value ) ;
            if C >= 0 then
            begin
                if( Range_Value( Operand1, 0, 7, B, -( $80 or C ), 1 ) ) then
                begin
                    exit ;
                end ;
                B := B * 8 ;
		A := Chr( $CB ) + Chr( $80 or B or C ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            C := Aix( Value ) ;
            if C > -32767 then
            begin
                if( Range_Value( Operand1, 0, 7, B, -$86, 3 ) ) then
                begin
                    exit ;
                end ;
                B := B * 8 ;
		A := Chr( $DD ) + Chr( $CB ) + Chr( C ) + Chr( $86 or B ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if( Range_Value( Operand1, 0, 7, B, -$86, 3 ) ) then
            begin
                exit ;
            end ;
            B := B * 8 ;
            C := Aiy( Value ) ;
            if C = -32767 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $FD ) + Chr( $CB ) + Chr( C ) + Chr( $86 or B ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
	end ;
        if( AA = 'RETI' ) then { Return from interrupt }
        begin
            A := Chr( $ED ) + Chr( $4D ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if( AA = 'RETN' ) then { Return from NMI }
        begin
            A := Chr( $ED ) + Chr( $45 ) ;
            Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
	if( Aa = 'RL' ) then { Rotate left through carry }
	begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
		A := Chr( $CB ) + Chr( $16 ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
		A := Chr( $CB ) + Chr( $10 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
		A := Chr( $DD ) + Chr( $CB ) + Chr( B ) + Chr( $16 ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B = -32767 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $FD ) + Chr( $CB ) + Chr( B ) + Chr( $16 ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
	end ;
	if( Aa = 'RLC' ) then {Rotate left circular}
	begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
		A := Chr( $CB ) + Chr( $06 ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
		A := Chr( $CB ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
		A := Chr( $DD ) + Chr( $CB ) + Chr( B ) + Chr( $06 ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B = -32767 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $FD ) + Chr( $CB ) + Chr( B ) + Chr( $06 ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
	end ;
        if( Aa = 'RLD' ) then { Rotate BCD digit left }
        begin
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $ED ) + Chr( $6F ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'RR' then {Rotate right through carry}
        begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $CB ) + Chr( $1E ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
                A := Chr( $CB ) + Chr( $18 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
                A := Chr( $DD ) + Chr( $CB ) + Chr( B ) + Chr( $1E ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B = -32767 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $FD ) + Chr( $CB ) + Chr( B ) + Chr( $1E ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
	if( Aa = 'RRC' ) then {Rotate right circular}
	begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
		A := Chr( $CB ) + Chr( $0E ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
		A := Chr( $CB ) + Chr( $08 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
		A := Chr( $DD ) + Chr( $CB ) + Chr( B ) + Chr( $0E ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B = -32767 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $FD ) + Chr( $CB ) + Chr( B ) + Chr( $0E ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
	end ;
	if( Aa = 'RRD' ) then { Rotate BCD digit right }
	begin
            A := Chr( $ED ) + Chr( $67 ) ;
                Add_Segments( 2, 0, 0 ) ;
            if Length( Operand1 + Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
	end ;
	if ( Aa = 'SBC' ) and ( Mode = Z80 ) then {Subtract with carry}
	begin
            if Operand1 = 'HL' then
            begin
		B := Addss( Value ) ;
		if B < 0 then
		begin
		    Assemble_Z80A := 1 ;
		    exit ;
		end ;
		B := B * 16 ;
		A := Chr( $ED ) + Chr( $42 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if Operand1 <> 'A' then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Value = '(HL)' then
            begin
		A := Chr( $9E ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Value ) ;
            if B >= 0 then
            begin
		A := Chr( $98 or B ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Value ) ;
            if B > -32767 then
            begin
		A := Chr( $DD ) + Chr( $9E ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Value ) ;
            if B > -32767 then
            begin
		A := Chr( $FD ) + Chr( $9E ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
	    if( not Range_Value( Value, -128, 255, B, 1, 1 ) ) then
	    begin
		A := Chr( $DE ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
	end ;
        if( Aa = 'SET' ) then {Set bit}
        begin
            if Value = '(HL)' then
            begin
                if( Range_Value( Operand1, 0, 7, B, -$C6, 1 ) ) then
                begin
                    exit ;
                end ;
                B := B * 8 ;
                A := Chr( $CB ) + Chr( $C6 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            C := Ar( Value ) ;
            if C >= 0 then
            begin
                if( Range_Value( Operand1, 0, 7, B, -( $C0 or C ), 1 ) ) then
                begin
                    exit ;
                end ;
                B := B * 8 ;
                A := Chr( $CB ) + Chr( $C0 or B or C ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            C := Aix( Value ) ;
            if C > -32767 then
            begin
                if( Range_Value( Operand1, 0, 7, B, -$C6, 3 ) ) then
                begin
                    exit ;
                end ;
                B := B * 8 ;
                A := Chr( $DD ) + Chr( $CB ) + Chr( C ) + Chr( $C6 or B ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            if( Range_Value( Operand1, 0, 7, B, -$C6, 3 ) ) then
            begin
                exit ;
            end ;
            B := B * 8 ;
            C := Aiy( Value ) ;
            if C = -32767 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $FD ) + Chr( $CB ) + Chr( C ) + Chr( $C6 or B ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'SLA' then {Shift left arithmetic}
        begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $CB ) + Chr( $26 ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
                A := Chr( $CB ) + Chr( $20 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
                A := Chr( $DD ) + Chr( $CB ) + Chr( B ) + Chr( $26 ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B = -32767 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            A := Chr( $FD ) + Chr( $CB ) + Chr( B ) + Chr( $26 ) ;
            Add_Segments( 2, 1, 1 ) ;
	    Assemble_Z80A := 2 ;
	    exit ;
        end ;
        if Aa = 'SRA' then {Shift right arithmetic}
        begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $CB ) + Chr( $2E ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
                A := Chr( $CB ) + Chr( $28 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
                A := Chr( $DD ) + Chr( $CB ) + Chr( B ) + Chr( $2E ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B = -32767 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $FD ) + Chr( $CB ) + Chr( B ) + Chr( $2E ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'SRL' then {Shift right logical}
        begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $CB ) + Chr( $3E ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
                A := Chr( $CB ) + Chr( $38 or B ) ;
                Add_Segments( 2, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
                A := Chr( $DD ) + Chr( $CB ) + Chr( B ) + Chr( $3E ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B = -32767 then
	    begin
		Assemble_Z80A := 1 ;
	    end else
	    begin
		A := Chr( $FD ) + Chr( $CB ) + Chr( B ) + Chr( $3E ) ;
                Add_Segments( 2, 1, 1 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ;
        if Aa = 'SUB' then {Subtract}
        begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80A := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
                A := Chr( $96 ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
                A := Chr( $90 or B ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then {IX}
            begin
                A := Chr( $DD ) + Chr( $96 ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B > -32767 then {IY}
            begin
                A := Chr( $FD ) + Chr( $96 ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80A := 2 ;
		exit ;
            end ;
	    if( not Range_Value( Operand1, -128, 255, B, 1, 1 ) ) then
	    begin
		A := Chr( $D6 ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
		Assemble_Z80A := 2 ;
	    end ;
	    exit ;
        end ; // SUB
    end ; { Assemble_Z80A }


    function Assemble_Z80B( var A : string ; AA, Operand1 : string ;
        B, C, D : longint ) : integer ;

    var UEC : TUnified_Exception ;

    begin
        Assemble_Z80B := 0 ;
	if( Aa = 'XOR' ) then {Logical XOR}
	begin
            if Length( Value ) > 0 then
	    begin
		Assemble_Z80B := 1 ;
		exit ;
	    end ;
            if Operand1 = '(HL)' then
            begin
		A := Chr( $AE ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80B := 2 ;
		exit ;
            end ;
            B := Ar( Operand1 ) ;
            if B >= 0 then
            begin
		A := Chr( $A8 or B ) ;
                Add_Segments( 1, 0, 0 ) ;
		Assemble_Z80B := 2 ;
		exit ;
            end ;
            B := Aix( Operand1 ) ;
            if B > -32767 then
            begin
		A := Chr( $DD ) + Chr( $AE ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80B := 2 ;
		exit ;
            end ;
            B := Aiy( Operand1 ) ;
            if B > -32767 then
            begin
		A := Chr( $FD ) + Chr( $AE ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
		Assemble_Z80B := 2 ;
		exit ;
            end ;
	    UEC := Evaluate( Operand1, B, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                Assemble_Z80B := 1 ;
                exit ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                end else
                begin
                    Assemble_Z80B := 1 ;
                    exit ;
                end ;
            end ;
            if( ( B < -128 ) or ( B > 255 ) ) then
            begin
                Assemble_Z80B := 1 ;
            end else
            begin
                A := Chr( $EE ) + Chr( B ) ;
                Add_Segments( 1, 1, 0 ) ;
                Assemble_Z80B := 2 ;
            end ;
            exit ;
        end ;
    end ; { Assemble_Z80B }


    function Assemble_Z80( var A : string ; AA, Operand1 : string ;
        B, C, D : longint ) : integer ;

    var X : integer ;

    begin
	X := Assemble_Z80A( A, AA, Operand1, B, C, D ) ;
	if( X = 0 ) then
	begin
	    X := Assemble_Z80B( A, AA, Operand1, B, C, D ) ;
	end ;
	Assemble_Z80 := X ;
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
    B, C, D : longint ;
    Continuation : string ;
    P : PChar ;
    PL : longint ;
    Next, Temp : string ;
    UEC : TUnified_Exception ;

label Ae, End_Assemble, Start_Over ;

begin // TZ80_Assembler.Assemble
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
            D := C ;
            while( ( AA[ D ] <> ',' ) and ( AA[ D ] <> ' ' ) and ( D > 0 ) ) do
            begin
                D := D - 1 ;
            end ;
            if( Copy( AA, D + 1, C - D - 1 ) = 'AF' ) then
            begin
                D := 0 ;
            end else
            begin
                D := -1 ;
            end ;
            if( D < 0 ) then
            begin
                if( B = 0 ) then
                begin
                    B := -1 ;
                end else
                begin
                    B := 0 ;
                end ;
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
    if( Aa = 'ADC' ) then { Add with carry }
    begin
        Operand1 ;
        if( Mode <> Z80 ) then
        begin
            B := Ar( Operand1 ) ;
            if( B < 0 ) then goto Ae ;
            A := Chr( $88 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( Operand1 = 'HL' ) then
        begin
            B := Addss( Value ) ; { Get register pair }
            if( B < 0 ) then goto Ae ;
            B := ( B * 16 ) or $4A ;
            A := Chr( $ED ) + Chr( B ) ;
            Add_Segments( 1, 1, 0 ) ;
            goto End_Assemble
        end ;
        if( Operand1 <> 'A' ) then goto Ae ;
        if Value = '(HL)' then
        begin
            A := Chr( $8E ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        B := Ar( Value ) ; {A register}
        if B >= 0 then {yes}
        begin
            A := Chr( $88 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        UEC := Evaluate( Value, C, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC = nil ) or ( UEC.Get_Error = 4 ) ) then { A (potential) number }
        begin
            if ( C < -128 ) or ( C > 255 ) then
            begin
                goto Ae ;
            end ;
            if( UEC <> nil ) then
            begin
                _Master.Add_Reference( PChar( Value ), 1, PC + 1 ) ;
            end ;
            A := Chr( $CE ) + Chr( C ) ;
            Add_Segments( 1, 1, 0 ) ;
            goto End_Assemble ;
        end ;
        B := Aix( Value ) ;
        if B > -32767 then {IX}
        begin
            A := Chr( $DD ) + Chr( $8E ) + Chr( B ) ;
            Add_Segments( 2, 1, 0 ) ;
            goto End_Assemble ;
        end ;
        B := Aiy( Value ) ;
        if B > -32767 then {IY}
        begin
            A := Chr( $FD ) + Chr( $8E ) + Chr( B ) ;
            Add_Segments( 2, 1, 0 ) ;
            goto End_Assemble ;
        end ;
        goto Ae ;
    end ;
    if( Aa = 'ADD' ) then { Add }
    begin
        if Mode <> Z80 then
        begin
            B := Ar( Operand1 ) ;
            if B < 0 then goto Ae ;
            A := Chr( $80 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if Length( Value ) = 0 then goto Ae ;
        {Requires two operands}
        if Operand1 = 'A' then
        begin
            if Value = '(HL)' then
            begin
                A := Chr( $86 ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble
            end ;
            B := Aix( Value ) ;
            if B > -32767 then {IX}
            begin
                A := Chr( $DD ) + Chr( $86 ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
                goto End_Assemble
            end ;
            B := Aiy( Value ) ;
            if B > -32767 then {IY}
            begin
                A := Chr( $FD ) + Chr( $86 ) + Chr( B ) ;
                Add_Segments( 2, 1, 0 ) ;
                goto End_Assemble
            end ;
            B := Ar( Value ) ; {A register?}
            if B >= 0 then {yes}
            begin
                A := Chr( $80 or B ) ;
                Add_Segments( 1, 0, 0 ) ;
                goto End_Assemble ;
            end ;
	    UEC := Evaluate( Value, C, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Value ), 1, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end else
            if( ( C < -128 ) or ( C > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $C6 ) + Chr( C ) ;
            Add_Segments( 1, 1, 0 ) ;
            goto End_Assemble ;
        end ;
        if Operand1 = 'HL' then
        begin
            B := Addss( Value ) ;
            if B = -1 then goto Ae ;
            B := B * 16 ;
            A := Chr( $09 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        if Operand1 = 'IX' then
        begin
            B := App( Value ) ;
            if B = -1 then goto Ae ;
            B := B * 16 ;
            A := Chr( $DD ) + Chr( $09 or B ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble
        end ;
        if Operand1 = 'IY' then
        begin
            B := Aqq( Value ) ;
            if B = -1 then goto Ae ;
            B := B * 16 ;
            A := Chr( $FD ) + Chr( $09 or B ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        goto Ae ;
    end ;
    if( Aa = 'CALL' ) then { Subroutine call }
    begin
        Operand1 ; // Get operand or address
        A := Get_Token ;
        if( A = ',' ) then
        begin
            A := Grab_Line ;
        end else
        begin
            if( length( A ) > 0 ) then
            begin
                _Operand1 := _Operand1 + A + Grab_Line ;
                A := '' ;
            end ;
        end ;
        if( ( Length( A ) > 0 ) and ( Mode = Z80 ) ) then
        begin
	    UEC := Evaluate( A, B, 0 ) ;
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
                end else
                begin
                    goto AE ;
                end ;
            end ;
            C := Ac( Operand1 ) ;
            if( C < 0 ) then goto Ae ;
            C := C * 8 ;
            A := Chr( $C4 or C ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
            Add_Segments( 1, 2, 0 ) ;
            goto End_Assemble ;
        end ;
        UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
        A := Chr( $CD ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble ;
    end ;
    if ( ( Aa = 'CCF' ) and ( Mode = Z80 ) ) or ( ( Aa = 'CMC' ) and ( Mode <> Z80 ) ) then
    {Complement carry flag}
    begin
        A := Chr( $3F ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Mode = Z80 ) then
    begin
	case Assemble_Z80( A, AA, Operand1, B, C, D ) of
	    1 : goto AE ;
	    2 : goto End_Assemble ;
	end ;
    end else { Not Z80 mode }
    begin
	if( Aa = 'ANA' ) then {Logical AND A}
	begin
            B := Ar( Operand1 ) ;
            if B < 0 then goto Ae ;
            A := Chr( $A0 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
	end ;
	if( Aa[ 1 ] = 'C' ) then { Possibly 8080 Call }
	begin
            B := Ac( Copy( Aa, 2, Length( Aa ) - 1 ) ) ;
            if( B >= 0 ) then
            begin
		UEC := Evaluate( Operand1, C, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
                B := B * 8 ;
                A := Chr( $C4 or B ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
                Add_Segments( 1, 2, 0 ) ;
		goto End_Assemble ;
	    end ;
	end ;
	if( Aa = 'CMP' ) then { Compare }
	begin
            B := Ar( Operand1 ) ;
            if B < 0 then goto Ae ;
            A := Chr( $B8 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
	end ;
        if( Aa = 'DCR' ) then
        begin
            B := Ar( Operand1 ) ;
            if( B < 0 ) then goto Ae ;
            B := B * 8 ;
            A := Chr( $05 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'DCX' ) then
        begin
            B := Addss( Operand1 ) ;
            if( B < 0 ) then goto Ae ;
            B := B * 16 ;
            A := Chr( $0B or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'INR' ) then
        begin
            B := Ar( Operand1 ) ;
            if( B < 0 ) then goto Ae ;
            B := B * 8 ;
            A := Chr( $04 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        if( Aa = 'INX' ) then
        begin
            B := Addss( Operand1 ) ;
            if ( B < 0 ) then goto Ae ;
            B := B * 16 ;
            A := Chr( $03 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'JMP' ) then {Jump}
        begin
            UEC := Evaluate( Operand1, B, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
            A := Chr( $C3 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
            Add_Segments( 1, 2, 0 ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'PCHL' ) then {Load PC with HL}
        begin
            A := Chr( $E9 ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        if( Aa[ 1 ] = 'J' ) then
        begin
            B := Ac( Copy( Aa, 2, Length( Aa ) - 1 ) ) ;
            if B >= 0 then
            begin
		        UEC := Evaluate( Operand1, C, 0 ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
                B := B * 8 ;
                A := Chr( $C2 or B ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
                Add_Segments( 1, 2, 0 ) ;
                goto End_Assemble ;
            end ;
        end ;
        if( Aa = 'MOV' ) then {Move}
        begin
            B := Ar( Operand1 ) ;
            if( B < 0 ) then goto Ae ;
            B := B * 8 ;
            A := Get_Token ;
            if( A <> ',' ) then
            begin
                Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
                Err := True ;
                goto AE ;
            end ;
            C := Ar( Value ) ;
            if( C < 0 ) then goto Ae ;
            A := Chr( $40 or B or C ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        if( Aa = 'MVI' ) then { Move immediate }
        begin
            B := Ar( Operand1 ) ;
            A := Get_Token ;
            if( A <> ',' ) then
            begin
                Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
                Err := True ;
                goto AE ;
            end ;
            UEC := Evaluate( Value, C, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Value ), 1, PC + 1 ) ;
                    C := 0 ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( B < 0 ) or ( C < -128 ) or ( C > 255 ) ) then goto Ae ;
            B := B * 8 ;
            A := Chr( $06 or B ) + Chr( C ) ;
            Add_Segments( 1, 1, 0 ) ;
            goto End_Assemble ;
        end ;
	if( Aa = 'ORA' ) then {Logical OR accumulator}
	begin
            B := Ar( Operand1 ) ;
            if( B < 0 ) then goto Ae ;
            A := Chr( $B0 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
	end ;
	if( Aa[ 1 ] = 'R' ) then { Possible 8080 return }
	begin
            B := Ac( Copy( Aa, 2, Length( Aa ) - 1 ) ) ;
            if( B >= 0 ) then
            begin
		B := B * 8 ;
		A := Chr( $C0 or B ) ;
                Add_Segments( 1, 0, 0 ) ;
		goto End_Assemble ;
	    end
	end ;
	if( Aa = 'SBB' ) then {Subtract with borrow}
	begin
            B := Ar( Operand1 ) ;
            if ( B < 0 ) then goto Ae ;
            A := Chr( $98 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
	end ;
	if( Aa = 'SUB' ) then {Subtract}
	begin
            B := Ar( Operand1 ) ;
            if ( B < 0 ) then goto Ae ;
            A := Chr( $90 or B ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
	end ;
    end ; { if Mode = Z80 else }
    if ( ( Aa = 'CPL' ) and ( Mode = Z80 ) ) or ( ( Aa = 'CMA' ) and ( Mode <> Z80 ) ) then
    {Complement accumulator}
    begin
        A := Chr( $2F ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'DAA' then { Decimal adjust A }
    begin
        A := Chr( $27 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'DI' then { Disable interrupts }
    begin
        A := Chr( $F3 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'EI' then {Enable interrupts}
    begin
        A := Chr( $FB ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if ( ( Aa = 'HALT' ) and ( Mode = Z80 ) ) or ( ( Aa = 'HLT' ) and ( Mode <> Z80 ) ) then {Halt}
    begin
        A := Chr( $76 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'IN' then {Input}
    begin
        if( Mode <> Z80 ) then
        begin
	    UEC := Evaluate( Operand1, B, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( B < -128 ) or ( B > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $DB ) + Chr( B ) ;
            Add_Segments( 1, 1, 0 ) ;
            goto End_Assemble ;
        end ;
        Operand1 ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( Operand1 = 'A' ) then
        begin
            if ( Value[ 1 ] <> '(' ) or ( Value[ Length( Value ) ] <> ')' ) then 
            begin
                goto Ae ;
            end ;
            UEC := Evaluate( Copy( Value, 2, Length( Value ) - 2 ), B, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Copy( Value, 2, Length( Value ) - 2 ) ), 1, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            A := Chr( $DB ) + Chr( B ) ;
            Add_Segments( 1, 1, 0 ) ;
            goto End_Assemble
        end ;
        B := Ar( Operand1 ) ;
        if ( Value <> '(C)' ) or ( B < 0 ) then goto Ae ;
        B := B * 8 ;
        A := Chr( $ED ) + Chr( $40 or B ) ;
        Add_Segments( 2, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'NOP' ) then { No-op }
    begin
        A := Chr( 0 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'OUT' ) then {Output}
    begin
        if( Mode <> Z80 ) then
        begin
	        UEC := Evaluate( Operand1, B, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Operand1 ), 1, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
            if( ( B < -128 ) or ( B > 255 ) ) then
            begin
                goto Ae ;
            end ;
            A := Chr( $D3 ) + Chr( B ) ;
            Add_Segments( 1, 1, 0 ) ;
            goto End_Assemble ;
        end ;
        Operand1 ;
        if( Operand1 = '(' ) then
        begin
            A := Get_Token ;
            while( ( A <> '' ) and ( A <> ')' ) ) do
            begin
                _Operand1 := _Operand1 + A ;
                A := Get_Token ;
            end ;
            _Operand1 := _Operand1 + ')' ;
        end ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( Operand1 = '(C)' ) then
        begin
            B := Ar( Value ) ;
            if B < 0 then goto Ae ;
            B := B * 8 ;
            A := Chr( $ED ) + Chr( $41 or B ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        if ( Value <> 'A' ) or ( Operand1[ 1 ] <> '(' ) or
            ( Operand1[ Length( Operand1 ) ] <> ')' ) then
        begin
            goto Ae ;
        end ;
        UEC := Evaluate( Copy( Operand1, 2, Length( Operand1 ) - 2 ), B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Copy( Operand1, 2, Length( Operand1 ) - 2 ) ), 1, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end ;
        if ( B < -128 ) or ( B > 255 ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $D3 ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'POP' ) then { Pop from stack }
    begin
        if( Mode = Z80 ) then
        begin
            if( Operand1 = 'IX' ) then
            begin
                A := Chr( $DD ) + Chr( $E1 ) ;
                Add_Segments( 2, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            if( Operand1 = 'IY' ) then
            begin
                A := Chr( $FD ) + Chr( $E1 ) ;
                Add_Segments( 2, 0, 0 ) ;
                goto End_Assemble ;
            end ;
        end ;
        B := ARR( Operand1 ) ;
        if( B < 0 ) then
	begin
	    goto Ae ;
	end ;
        B := B * 16 ;
        A := Chr( $C1 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( AA = 'PUSH' ) then { Push to stack }
    begin
        if( Mode = Z80 ) then
        begin
            if Operand1 = 'IX' then
            begin
                A := Chr( $DD ) + Chr( $E5 ) ;
                Add_Segments( 2, 0, 0 ) ;
                goto End_Assemble ;
            end ;
            if Operand1 = 'IY' then
            begin
                A := Chr( $FD ) + Chr( $E5 ) ;
                Add_Segments( 2, 0, 0 ) ;
                goto End_Assemble ;
            end ;
        end ;
        B := ARR( Operand1 ) ;
        if( B < 0 ) then
	begin
	    goto Ae ;
	end ;
        B := B * 16 ;
        A := Chr( $C5 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'RET' ) then { Return }
    begin
        A := Edit( Grab_Line, -1 ) ;
        if( ( length( A ) = 0 ) or ( copy( A, 1, 1 ) = ';' ) ) then // Nothing more on line
        begin
            A := Chr( $C9 ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        if( Mode <> Z80 ) then
        begin
            _Master.Put_Token( PChar( A ) ) ;
            goto Ae ;
        end ;
        B := Ac( A ) ;
        if( B < 0 ) then goto Ae ;
        B := B * 8 ;
        A := Chr( $C0 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if ( ( Aa = 'RLA' ) and ( Mode = Z80 ) ) or ( ( Aa = 'RAL' ) and ( Mode <> Z80 ) ) then
    { Rotate A left through carry }
    begin
        A := Chr( $17 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if ( ( Aa = 'RLCA' ) and ( Mode = Z80 ) ) or ( ( Aa = 'RLC' ) and ( Mode <> Z80 ) ) then
    { Rotate left circular A }
    begin
        A := Chr( $07 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if ( ( Aa = 'RRA' ) and ( Mode = Z80 ) ) or ( ( Aa = 'RAR' ) and ( Mode <> Z80 ) ) then
    {Rotate A right through carry}
    begin
        A := Chr( $1F ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if ( ( Aa = 'RRCA' ) and ( Mode = Z80 ) ) or ( ( Aa = 'RRC' ) and ( Mode <> Z80 ) ) then
    {Rotate A right circular}
    begin
        A := Chr( $0F ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'RST' ) then { Restart }
    begin
        UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( Operand1 ), -$C7, PC ) ;
            end else
            begin
                goto AE ;
            end ;
        end ;
        if( ( B < 0 ) or ( B > 7 ) ) then
        begin
            goto Ae ;
        end ;
        B := B * 8 ;
        A := Chr( $C7 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if ( ( Aa = 'SCF' ) and ( Mode = Z80 ) ) or ( ( Aa = 'STC' ) and ( Mode <> Z80 ) ) then
    {Set carry flag}
    begin
        A := Chr( $37 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Mode = Z80 ) then
    begin
        Status.Log_Error( PChar( 'Unknown instruction: "' + AA + '"' ), nil, -1, 3 ) ;
        goto Ae ;
    end ;
    if Aa = 'XRA' then {XOR accumulator}
    begin
        B := Ar( Operand1 ) ;
        if ( B < 0 ) then goto Ae ;
        A := Chr( $A8 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'DAD' ) then { Double ADD }
    begin
        B := Addss( Operand1 ) ;
        if ( B < 0 ) then goto Ae ;
        B := B * 16 ;
        A := Chr( $09 or B ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble
    end ;
    if( Aa = 'XTHL' ) then {Exhange stack}
    begin
        A := Chr( $E3 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'SPHL' ) then {Load SP from HL}
    begin
        A := Chr( $F9 ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'XCHG' ) then {Exchange registers}
    begin
        A := Chr( $EB ) ;
        Add_Segments( 1, 0, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'LXI' ) then {Load register pair}
    begin
        B := Addss( Operand1 ) ;
        A := Get_Token ;
        if( A <> ',' ) then
        begin
            Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
            Err := True ;
            goto AE ;
        end ;
        if( B < 0 ) then
        begin
            goto Ae ;
        end ;
        A := Grab_Line ;
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
            end else
            begin
                goto AE ;
            end ;
        end ;
        B := B * 16 ;
        A := Chr( 1 or B ) + Chr( Lo( C ) ) + Chr( Hi( C ) ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'STAX' ) then // Store accumulator
    begin
        if Operand1 = 'B' then
        begin
            A := Chr( $02 ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        if Operand1 = 'D' then
        begin
            A := Chr( $12 ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble ;
        end ;
        goto Ae
    end ;
    if Aa = 'LDAX' then {Load accumulator}
    begin
        if Operand1 = 'B' then
        begin
            A := Chr( $0A ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        if Operand1 = 'D' then
        begin
            A := Chr( $1A ) ;
            Add_Segments( 1, 0, 0 ) ;
            goto End_Assemble
        end ;
        goto Ae
    end ;
    if Aa = 'LHLD' then {Load HL direct}
    begin
        UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
        A := Chr( $2A ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'LDA' then {Load A direct}
    begin
        UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if ( UEC <> nil ) then
        begin
            if( UEC.Get_Error = 4 ) then
            begin
                _Master.Add_Reference( PChar( Operand1 ), 2, PC + 1 ) ;
            end else
            begin
                goto Ae ;
            end ;
        end ;
        A := Chr( $3A ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble
    end ;
    if Aa = 'SHLD' then {Store HL direct}
    begin
        UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
        A := Chr( $22 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'STA' then {Store A direct}
    begin
        UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
        A := Chr( $32 ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
        Add_Segments( 1, 2, 0 ) ;
        goto End_Assemble
    end ;
    if Aa = 'ADI' then { ADD immediate}
    begin
        UEC := Evaluate( Input_Buffer, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC <> nil ) and ( UEC.Get_Error = 4 ) ) then
        begin
            _Master.Add_Reference( PChar( Input_Buffer ), 1, PC + 1 ) ;
            A := Chr( $C6 ) + Chr( 0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble ;
        end else
        begin
            ER := ( UEC <> nil ) ;
        end ;
        if( ER or ( B < -128 ) or ( B > 255 ) ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $C6 ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble ;
    end ;
    if( Aa = 'ACI' ) then { ADC immediate }
    begin
        UEC := Evaluate( Input_Buffer, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC <> nil ) and ( UEC.Get_Error = 4 ) ) then
        begin
            _Master.Add_Reference( PChar( Input_Buffer ), 1, PC + 1 ) ;
            A := Chr( $CE ) + Chr( 0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble ;
        end else
        begin
            ER := ( UEC <> nil ) ;
        end ;
	if( ER or ( B < -128 ) or ( B > 255 ) ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $CE ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'SUI' then {SUB immediate}
    begin
        UEC := Evaluate( Input_Buffer, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC <> nil ) and ( UEC.Get_Error = 4 ) ) then
        begin
            _Master.Add_Reference( PChar( Input_Buffer ), 1, PC + 1 ) ;
            A := Chr( $D6 ) + Chr( 0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble ;
        end else
        begin
            ER := ( UEC <> nil ) ;
        end ;
	if( ER or ( B < -128 ) or ( B > 255 ) ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $D6 ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'SBI' then {SBB immediate}
    begin
        UEC := Evaluate( Input_Buffer, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC <> nil ) and ( UEC.Get_Error = 4 ) ) then
        begin
            _Master.Add_Reference( PChar( Input_Buffer ), 1, PC + 1 ) ;
            A := Chr( $DE ) + Chr( 0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble
        end else
        begin
            ER := ( UEC <> nil ) ;
        end ;
	if ER or ( B < -128 ) or ( B > 255 ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $DE ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if Aa = 'ANI' then {ANA immediate}
    begin
        UEC := Evaluate( Input_Buffer, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC <> nil ) and ( UEC.Get_Error = 4 ) ) then
        begin
            _Master.Add_Reference( PChar( Input_Buffer ), 1, PC + 1 ) ;
            A := Chr( $E6 ) + Chr( 0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble
        end else
        begin
            ER := ( UEC <> nil ) ;
        end ;
	if ER or ( B < -128 ) or ( B > 255 ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $E6 ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if Aa = 'XRI' then {XRA immediate}
    begin
        UEC := Evaluate( Input_Buffer, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC <> nil ) and ( UEC.Get_Error = 4 ) ) then
        begin
            _Master.Add_Reference( PChar( Input_Buffer ), 1, PC + 1 ) ;
            A := Chr( $EE ) + Chr( 0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble ;
        end else
        begin
            ER := ( UEC <> nil ) ;
        end ;
	if ER or ( B < -128 ) or ( B > 255 ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $EE ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble ;
    end ;
    if Aa = 'ORI' then { ORA immediate }
    begin
        UEC := Evaluate( Input_Buffer, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC <> nil ) and ( UEC.Get_Error = 4 ) ) then
        begin
            _Master.Add_Reference( PChar( Input_Buffer ), 1, PC + 1 ) ;
            A := Chr( $F6 ) + Chr( 0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble
        end else
        begin
            ER := ( UEC <> nil ) ;
        end ;
	if ER or ( B < -128 ) or ( B > 255 ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $F6 ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if Aa = 'CPI' then { CMP immediate }
    begin
        UEC := Evaluate( Input_Buffer, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
            goto AE ;
        end ;
        if( ( UEC <> nil ) and ( UEC.Get_Error = 4 ) ) then
        begin
            _Master.Add_Reference( PChar( Input_Buffer ), 1, PC + 1 ) ;
            A := Chr( $FE ) + Chr( 0 ) ;
            Add_Segments( 2, 0, 0 ) ;
            goto End_Assemble
        end else
        begin
            ER := ( UEC <> nil ) ;
        end ;
	if ER or ( B < -128 ) or ( B > 255 ) then
        begin
            goto Ae ;
        end ;
        A := Chr( $FE ) + Chr( B ) ;
        Add_Segments( 1, 1, 0 ) ;
        goto End_Assemble
    end ;
    if Mode = M8085 then
    begin
        if Aa = 'RIM' then
        begin
            A := Chr( $20 ) ;
            Add_Segments( 1, 0, 0 ) ;
            Exit ;
        end ;
        if Aa = 'SIM' then
        begin
            A := Chr( $30 ) ;
            Add_Segments( 1, 0, 0 ) ;
            Exit ;
        end ;
	if( ( Aa = 'DSUB' ) and ( Operand1 = 'B' ) ) then
	begin
	    A := chr( $08 ) ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
	end ;
	if( Aa = 'RHR' ) then
	begin
	    A := chr( $10 ) ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
	end ;
	if( Aa = 'RDL' ) then
	begin
	    A := chr( $18 ) ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
	end ;
	if( Aa = 'DMOV' ) then
	begin
            Operand1 ;
            A := Get_Token ;
            if( A <> ',' ) then
            begin
                Status.Log_Error( PChar( 'Expected ",", found "' + A + '"' ), nil, -1, 3 ) ;
                Err := True ;
                goto AE ;
            end ;
	    UEC := Evaluate( Value, C, 0 ) ;
            if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
            begin
                Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                goto AE ;
            end ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = 4 ) then
                begin
                    _Master.Add_Reference( PChar( Value ), 1, PC + 1 ) ;
                end else
                begin
                    goto AE ;
                end ;
            end else
            if ( C < -128 ) or ( C > 255 ) then
            begin
                goto Ae ;
            end ;
	    if( Operand1 = 'H' ) then
	    begin
		A := chr( $28 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
		exit ;
	    end ;
	    if( Operand1 = 'SP' ) then
	    begin
		A := chr( $38 ) + chr( C ) ;
                Add_Segments( 1, 1, 0 ) ;
		exit ;
	    end ;
	end ;
	if( Aa = 'RSTV' ) then
	begin
	    A := chr( $CB ) ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
	end ;
	if( Aa = 'SHLX' ) then
	begin
	    A := chr( $D9 ) ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
	end ;
	if( Aa = 'JND' ) then
	begin
        UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
        A := Chr( $DD ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
        Add_Segments( 1, 2, 0 ) ;
	    exit ;
	end ;
	if( Aa = 'LHLX' ) then
	begin
	    A := chr( $ED ) ;
            Add_Segments( 1, 0, 0 ) ;
	    exit ;
	end ;
	if( Aa = 'JD' ) then
	begin
        UEC := Evaluate( Operand1, B, 0 ) ;
        if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
        begin
            Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
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
        A := Chr( $FD ) + Chr( Lo( B ) ) + Chr( Hi( B ) ) ;
        Add_Segments( 1, 2, 0 ) ;
	    exit ;
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
end ; // TZ80_Assembler.Assemble


{ Returns the default radix (base) of numeric literals. }
function TZ80_Assembler.Default_Radix : longint ;

begin
    Result := Base ;
end ;


{ Returns the default size of numeric literals, in bits. }
function TZ80_Assembler.Default_Size : longint ;

begin
    Default_Size := 8 ;
end ;


{ Returns facility code for this class. }
function TZ80_Assembler.Facility_Code : longint ;

begin
    Result := Z80AssemblerErr_Facility ;
end ;


const _Extensions : string = 'asm' ;

function TZ80_Assembler.Source_Extensions : PChar ;

begin
    Result := PChar( _Extensions ) ;
end ;


const _Valid_Symbol_Initial : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_' ;

function TZ80_Assembler.Valid_Symbol_Initial : PChar ;

begin
    Result := PChar( _Valid_Symbol_Initial ) ;
end ;


const _Valid_Symbol_After : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_0123456789' ;

function TZ80_Assembler.Valid_Symbol_After : PChar ;

begin
    Result := PChar( _Valid_Symbol_After ) ;
end ;


function TZ80_Assembler.Evaluate( const X : string ; var _Result : longint ;
    PC_Adjustment : integer ) : TUnified_Exception ;

var I64 : int64 ;

begin
    Result := _Master.Evaluate( PChar( X ), I64 ) ;
    _Result := I64 ;
end ; { TZ80_Assembler.Evaluate }


function TZ80_Assembler.Handle_Directives( var Value : string ;
    Status : TAssembler_Status ) : boolean ;

var P : PChar ;
    PL : longint ;
    UEC : TUnified_Exception ;

begin
    // Setup...
    Value := Edit( Value, 128 ) ;

    // Handle directive...
    if( Value = '.Z80' ) then
    begin
        Mode := Z80 ;
        Base := 16 ;
        TZ80_CPU( CPU ).Mode := Mode ;
        TZ80_CPU( CPU ).Base := Base ;
	Handle_Directives := True ;
        _Master.Expand( '.RADIX 16', P, PL, Status ) ;
        _Master.Expand( '.DEFINE Z80', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE I8085', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE I8080', P, PL, Status ) ;
        Value := '' ;
    end else
    if( Value = '.8080' ) then
    begin
        Mode := M8080 ;
        Base := 10 ;
        TZ80_CPU( CPU ).Mode := Mode ;
        TZ80_CPU( CPU ).Base := Base ;
	Handle_Directives := True ;
        _Master.Expand( '.RADIX 10', P, PL, Status ) ;
        _Master.Expand( '.DEFINE I8080', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE I8085', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE Z80', P, PL, Status ) ;
        Value := '' ;
    end else
    if( Value = '.8085' ) then
    begin
        Mode := M8085 ;
        Base := 10 ;
        TZ80_CPU( CPU ).Mode := Mode ;
        TZ80_CPU( CPU ).Base := Base ;
	Handle_Directives := True ;
        _Master.Expand( '.RADIX 10', P, PL, Status ) ;
        _Master.Expand( '.DEFINE I8085', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE Z80', P, PL, Status ) ;
        _Master.Expand( '.UNDEFINE I8080', P, PL, Status ) ;
        Value := '' ;
    end else
    begin
        UEC := _Master.Expand( PChar( Value ), P, PL, Status ) ;
        Handle_Directives := ( UEC = nil ) ;
        setlength( Value, PL ) ;
        move( P[ 0 ], Value[ 1 ], PL ) ;
    end ;
end ; { Handle_Directives }


procedure TZ80_Assembler.Add_Segments( S1, S2, S3 : integer ) ;

begin
    Segments.Clear ;
    Segments.Add( S1 * 8 ) ;
    Segments.Add( S2 * 8 ) ;
    Segments.Add( S3 * 8 ) ;
end ;


function TZ80_Assembler.Addss( A : String ) : Integer ; { Register pair dd or ss }

begin
    Addss := -1 ;
    if( Mode = Z80 ) then
    begin
        if A = 'BC' then Addss := 0 ;
        if A = 'DE' then Addss := 1 ;
        if A = 'HL' then Addss := 2 ;
        if A = 'SP' then Addss := 3 ;
        Exit ;
    end ;
    if A = 'B' then Addss := 0 ;
    if A = 'D' then Addss := 1 ;
    if A = 'H' then Addss := 2 ;
    if A = 'SP' then Addss := 3 ;
end ;


function TZ80_Assembler.AQQ( A : string ) : integer ; { Register pair qq }

begin
    Aqq := -1 ;
    if( Mode = Z80 ) then
    begin
        if A = 'BC' then Aqq := 0 ;
        if A = 'DE' then Aqq := 1 ;
        if A = 'IY' then Aqq := 2 ;
        if A = 'SP' then Aqq := 3 ;
        Exit
    end ;
    if A = 'B' then Aqq := 0 ;
    if A = 'D' then Aqq := 1 ;
    if A = 'H' then Aqq := 2 ;
    if A = 'PSW' then Aqq := 3
end ;


function TZ80_Assembler.Ar( A : String ) : Integer ; {Register r}

begin
    Ar := -1 ;
    if A = 'B' then Ar := 0 ;
    if A = 'C' then Ar := 1 ;
    if A = 'D' then Ar := 2 ;
    if A = 'E' then Ar := 3 ;
    if A = 'H' then Ar := 4 ;
    if A = 'L' then Ar := 5 ;
    if( ( A = 'M' ) and ( Mode <> Z80 ) ) then Ar := 6 ;
    if( ( A = '(HL)' ) and ( Mode = Z80 ) ) then Ar := 6 ;
    if A = 'A' then Ar := 7 ;
end ;


function TZ80_Assembler.ARR( A : string ) : integer ; { Register pair rr }

begin
    ARR := -1 ;
    if( Mode = Z80 ) then
    begin
	if A = 'BC' then ARR := 0 ;
	if A = 'DE' then ARR := 1 ;
	if A = 'HL' then ARR := 2 ;
	if A = 'AF' then ARR := 3 ;
    end else
    begin
	if A = 'B' then ARR := 0 ;
	if A = 'D' then ARR := 1 ;
	if A = 'H' then ARR := 2 ;
	if A = 'PSW' then ARR := 3 ;
    end ;
end ;


function TZ80_Assembler.Aix( const A : String ) : Integer ; {(IX+d)}

var X : longint ;
    UEC : TUnified_Exception ;

begin
    Aix := -32767 ;
    if A = '(IX)' then
    begin
        Aix := 0 ;
        Exit
    end ;
    if( Copy( A, 1, 4 ) <> '(IX+' ) then Exit ;
    if( A[ Length( A ) ] <> ')' ) then Exit ;
    UEC := Evaluate( Copy( A, 5, Length( A ) - 5 ), X, 0 ) ;
    if( UEC <> nil ) then
    begin
        if( UEC.Get_Error = 4 ) then
        begin
            _Master.Add_Reference( PChar( Copy( A, 5, Length( A ) - 5 ) ), 1, PC + 2 ) ;
        end else
        begin
            exit ;
        end ;
    end else
    if( ( X < -128 ) or ( X > 127 ) ) then
    begin
        Exit ;
    end ;
    Aix := X
end ;


function TZ80_Assembler.Aiy( const A : String ) : Integer ; {(IY+d)}

var X : longint ;
    UEC : TUnified_Exception ;

begin
    Aiy := -32767 ;
    if( A = '(IY)' ) then
    begin
        Aiy := 0 ;
        Exit ;
    end ;
    if( Copy( A, 1, 4 ) <> '(IY+' ) then Exit ;
    if( A[ Length( A ) ] <> ')' ) then Exit ;
    UEC := Evaluate( Copy( A, 5, Length( A ) - 5 ), X, 0 ) ;
    if( UEC <> nil ) then
    begin
        if( UEC.Get_Error = 4 ) then
        begin
            _Master.Add_Reference( PChar( Copy( A, 5, Length( A ) - 5 ) ), 1, PC + 2 ) ;
        end else
        begin
            exit ;
        end ;
    end else
    if( ( X < -128 ) or ( X > 127 ) ) then
    begin
        Exit ;
    end ;
    Aiy := X ;
end ;



end.
