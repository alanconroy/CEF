{
              Program Name : Gigabyte_uCode_ASM
              Package Name : Gigabyte_uCode
              Purpose      : Gigabyte micro-code assembler
              Institution  : Conroy & Conroy
              Date Written : 4-Dec-2011
              Written By   : Alan Conroy
              Version      : 2.0

              Copyright (C) 2011 by Alan Conroy.  Released to the public domain.

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

          This unit defines the assembler for the Gigabyte micro-code component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Gigabyte_uCode_ASM ;

interface

uses { Borland... }
     Classes, // TStringList

     { CEF... }
     _CEF, // TMaster_Assembler
     CEF, { TBase_Assembler }

     { Other... }
     _UE, // TUnified_Exception
     CommonUt ; // TInteger_List

const Gigabyte_uCodeAssemblerErr_Facility = 52 ;
      Gigabyte_uCodeAssemblerErr_Success = 0 ;
      Gigabyte_uCodeAssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
      Gigabyte_uCodeAssemblerErr_Illegal_Expression = 2 ; // Illegal expression
      Gigabyte_uCodeAssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
      Gigabyte_uCodeAssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol

type TGigabyte_uCode_Assembler = class( TBase_Assembler )
                          public // Instance data...
                              Addresses : array[ 0..65540 ] of word ;
                              Assembled_Code : string ;
                              Base : integer ;
                              Err : boolean ; { True if an error }
                              _Master : TMaster_Assembler ;
                              PC : integer ; { Current target address }
                              CPU : TCPU ;
                              Defines, Macros, Current_Macro : TStringList ;
                              Define : string ;

                          private // Internal utility routines...
                              function Handle_Directives( var Value : string ;
                                  Status : TAssembler_Status ) : boolean ;

                          protected
                              function Evaluate( const X : string ;
                                  var _Result : longint ; PC_Adjustment : longint ) : TUnified_Exception ;
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

                              function Begin_Assembly : int64 ; override ;

                              function Finish_Assembly( Context : int64 ;
                                  var outputs, machines : PChar ;
                                  var MachineL : longint ; var Address : int64 ;
                                  Status : TAssembler_Status ;
                                  Flags : longint ) : TUnified_Exception ;
                                  override ;
                      end ;

// ROM constants...
const ROM_Internal_Operation = $1F ; // 31
    const ROM_Internal_No_Op = 0 ;
    const ROM_Internal_To_Bus = 1 ;
    const ROM_Internal_From_Bus = 2 ;
    const ROM_Internal_Clear_Carry = 3 ;
    const ROM_Internal_Jump = 4 ;
    const ROM_Internal_Halt = 5 ;
    const ROM_Internal_Enable_Interrupts = 6 ;
    const ROM_Internal_Disable_Interrupts = 7 ;
    const ROM_Internal_ALU_Op = 8 ;
    const ROM_Internal_Set_Bit_In_Z = 9 ;
    const ROM_Internal_Clear_Bit_In_Z = 10 ;
    const ROM_Internal_If_Bit_In_Z = 11 ;
    const ROM_Internal_If_Not_Bit_In_Z = 12 ;
    const ROM_Internal_If_temp_Zero = 13 ;
    const ROM_Internal_If_Bit_In_Status = 14 ;
    const ROM_Internal_If_Not_Bit_In_Status = 15 ;
    const ROM_Internal_If_Bit_In_Temp = 16 ;
    const ROM_Internal_If_Not_Bit_In_Temp = 17 ;
    const ROM_Internal_If_Temp_Not_Zero = 18 ;
    const ROM_Internal_Clear_Temp_Bit = 19 ;
    const ROM_Internal_Load_Resume_Register = 20 ;
    const ROM_Internal_If_Not_Kernel = 21 ;
    const ROM_Internal_Kernel_Check = 22 ;
    const ROM_Internal_Clear_Clock = 23 ;
    const ROM_Internal_Set_Temp_Bit = 24 ;
const ROM_Source_Destination = $3E0 ; // 992
const ROM_Source_Destination_Shift = 5 ;
    const ROM_Source_Register_X = 0 ;
    const ROM_Source_Register_Y = 1 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_Z = 2 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_Status = 3 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_TD = 4 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Data = 5 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_TS = 6 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_S = 7 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_M = 8 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_H = 9 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_DW = 10 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_MO = 11 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Register_TY = 12 shl ROM_Source_Destination_Shift ;
    const ROM_Source_ALUA_Lo = 14 shl ROM_Source_Destination_Shift ;
    const ROM_Source_ALUA_Hi = 15 shl ROM_Source_Destination_Shift ;
    const ROM_Source_ALUB_Lo = 16 shl ROM_Source_Destination_Shift ;
    const ROM_Source_ALUB_Hi = 17 shl ROM_Source_Destination_Shift ;
    const ROM_Source_PC_Lo = 18 shl ROM_Source_Destination_Shift ;
    const ROM_Source_PC_Hi = 19 shl ROM_Source_Destination_Shift ;
    const ROM_Source_SP_Lo = 20 shl ROM_Source_Destination_Shift ;
    const ROM_Source_SP_Hi = 21 shl ROM_Source_Destination_Shift ;
    const ROM_Source_DA_Lo = 23 shl ROM_Source_Destination_Shift ;
    const ROM_Source_DA_Hi = 24 shl ROM_Source_Destination_Shift ;
    const ROM_Source_IP_Lo = 25 shl ROM_Source_Destination_Shift ;
    const ROM_Source_IP_Hi = 26 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Temp_Lo = 27 shl ROM_Source_Destination_Shift ;
    const ROM_Source_Temp_Hi = 28 shl ROM_Source_Destination_Shift ;
    const ROM_Source_DA1_Lo = 29 shl ROM_Source_Destination_Shift ;
    const ROM_Source_DA1_Hi = 30 shl ROM_Source_Destination_Shift ;
const ROM_Size = $C00 ; // 3072
const ROM_Size_Shift = 10 ;
    const ROM_32_Bit = 0 ;
    const ROM_16_Bit = 1 shl ROM_Size_Shift ;
    const ROM_8_Bit = 2 shl ROM_Size_Shift ;
const ROM_End = $1000 ; // 4096
const ROM_Inc_PC = $2000 ; // 8192
const ROM_Inc_DA = $4000 ; // 16384
const ROM_Inc_SP = $8000 ; // 32768
const ROM_Dec_SP = $10000 ; // 65536
const ROM_Dec_Temp = $20000 ; // 131072
const ROM_External_Operation = $3C0000 ;
const ROM_External_Operation_Shift = 18 ;
    const ROM_External_No_Op = 0 ;
    const ROM_External_Read_Memory = 1 shl ROM_External_Operation_Shift ;
    const ROM_External_Write_Memory = 2 shl ROM_External_Operation_Shift ;
    const ROM_External_Input = 3 shl ROM_External_Operation_Shift ;
    const ROM_External_Output = 4 shl ROM_External_Operation_Shift ;
    const ROM_External_Lock_Bus = 5 shl ROM_External_Operation_Shift ;
    const ROM_External_Latch_SP = 6 shl ROM_External_Operation_Shift ;
    const ROM_External_Latch_PC = 7 shl ROM_External_Operation_Shift ;
    const ROM_External_Unlatch = 8 shl ROM_External_Operation_Shift ;
    const ROM_External_Latch_DA1 = 9 shl ROM_External_Operation_Shift ;
    const ROM_External_Unlock_Bus = 10 shl ROM_External_Operation_Shift ;
    const ROM_External_Set_INTA = 12 shl ROM_External_Operation_Shift ;
    const ROM_External_Clear_INTA = 13 shl ROM_External_Operation_Shift ;
const ROM_Inc_DA1 = $400000 ;
const ROM_Data = $7FFF800000 ;
const ROM_Data_Shift = 23 ;
const ROM_Dec_PC = $1000000000 ;

{ Based on the 74181 ALU (active-low data pins).
  bits  meaning
  ----  -------
  0-3   S0-S3
  4     M (1 = logic, 0 = arithmetic)
  5     Non 74181 operation
  6     Enable carry (if not set, carry is 0)
  7     Force carry into ALU
  8     Use carry as borrow
}
const ALU_Complement = $10 ;
const ALU_Increment = $8F ;
const ALU_Decrement = $00 ;
const ALU_AND = $1E ;
const ALU_NAND = $11 ;
const ALU_OR = $1B ;
const ALU_NOR = $14 ;
const ALU_XOR = $19 ;
const ALU_XNOR = $16 ;
const ALU_ADD = $09 ;
const ALU_ADDC = $49 ;
const ALU_SUB = $06 ;
const ALU_SUBB = $106 ;
const ALU_Multiply = $20 ;
const ALU_DAA = $21 ;
const ALU_SHL = $22 ;
const ALU_SHLC = $23 ;
const ALU_SHR = $24 ;
const ALU_SHRC = $25 ;
const ALU_DAS = $26 ;


implementation

uses SysUtils, // strpas

     Gigabyte_uCode_CPU, // TGigabyte_uCode_CPU
     ASCIIDef, // CR
     Express,
     Parse,
     Standard,
     CVT,
     Helps,
     Instrs,
     Maths,
     NUM1S,
     UStrings ;

function Source( Dst : boolean ; A : string ) : int64 ;

begin
    A := trim( lowercase( A ) ) ;
    if( copy( A, 1, 2 ) = 't ' ) then
    begin
        A := 't' + copy( A, 3, length( A ) ) ;
    end ;
    Result := -1 ;
    if( A = 'x' ) then
    begin
        Result := ROM_Source_Register_X ;
        exit ;
    end ;
    if( A = 'y' ) then
    begin
        Result := ROM_Source_Register_Y ;
        exit ;
    end ;
    if( A = 'z' ) then
    begin
        Result := ROM_Source_Register_Z ;
        exit ;
    end ;
    if( A = 'sb' ) then
    begin
        Result := ROM_Source_Register_Status ;
        exit ;
    end ;
    if( A = 't(ts)' ) then
    begin
        Result := ROM_Source_Register_TS ;
        exit ;
    end ;
    if( A = 't(s)' ) then
    begin
        Result := ROM_Source_Register_S ;
        exit ;
    end ;
    if( A = 't(d)' ) then
    begin
        Result := ROM_Source_Register_TD ;
        exit ;
    end ;
    if( A = 't(m)' ) then
    begin
        Result := ROM_Source_Register_M ;
        exit ;
    end ;
    if( A = 't(h)' ) then
    begin
        Result := ROM_Source_Register_H ;
        exit ;
    end ;
    if( A = 't(dw)' ) then
    begin
        Result := ROM_Source_Register_DW ;
        exit ;
    end ;
    if( A = 't(mo)' ) then
    begin
        Result := ROM_Source_Register_MO ;
        exit ;
    end ;
    if( A = 't(y)' ) then
    begin
        Result := ROM_Source_Register_TY ;
        exit ;
    end ;
    if( Dst ) then
    begin
        if( A = 'alual' ) then
        begin
            Result := ROM_Source_ALUA_Lo ;
            exit ;
        end ;
        if( A = 'aluah' ) then
        begin
            Result := ROM_Source_ALUA_Hi ;
            exit ;
        end ;
        if( A = 'alubl' ) then
        begin
            Result := ROM_Source_ALUB_Lo ;
            exit ;
        end ;
        if( A = 'alubh' ) then
        begin
            Result := ROM_Source_ALUB_Hi ;
            exit ;
        end ;
    end else
    begin
        if( A = 'alul' ) then
        begin
            Result := ROM_Source_ALUA_Lo ;
            exit ;
        end ;
        if( A = 'aluh' ) then
        begin
            Result := ROM_Source_ALUA_Hi ;
            exit ;
        end ;
    end ;
    if( A = 'pcl' ) then
    begin
        Result := ROM_Source_PC_Lo ;
        exit ;
    end ;
    if( A = 'pch' ) then
    begin
        Result := ROM_Source_PC_Hi ;
        exit ;
    end ;
    if( A = 'spl' ) then
    begin
        Result := ROM_Source_SP_Lo ;
        exit ;
    end ;
    if( A = 'sph' ) then
    begin
        Result := ROM_Source_SP_Hi ;
        exit ;
    end ;
    if( A = 'templ' ) then
    begin
        Result := ROM_Source_Temp_Lo ;
        exit ;
    end ;
    if( A = 'temph' ) then
    begin
        Result := ROM_Source_Temp_Hi ;
        exit ;
    end ;
    if( A = 'ipl' ) then
    begin
        Result := ROM_Source_IP_Lo ;
        exit ;
    end ;
    if( A = 'iph' ) then
    begin
        Result := ROM_Source_IP_Hi ;
        exit ;
    end ;
    if( A = 'dal' ) then
    begin
        Result := ROM_Source_DA_Lo ;
        exit ;
    end ;
    if( A = 'dah' ) then
    begin
        Result := ROM_Source_DA_Hi ;
        exit ;
    end ;
    if( A = 'da1l' ) then
    begin
        Result := ROM_Source_DA1_Lo ;
        exit ;
    end ;
    if( A = 'da1h' ) then
    begin
        Result := ROM_Source_DA1_Hi ;
        exit ;
    end ;
end ; // Source


function Valid_Label( A : string ) : boolean ;

var Dummy : integer ;

begin
    Result := False ; // Assume invalid
    A := lowercase( A ) ;
    if( length( A ) = 0 ) then
    begin
        exit ;
    end ;
    if( pos( A[ 1 ], 'abcdefghijklmnopqrstuvwxyz' ) = 0 ) then
    begin
        exit ;
    end ;
    for Dummy := 2 to length( A ) do
    begin
        if( pos( A[ Dummy ], 'abcdefghijklmnopqrstuvwxyz0123456789_$' ) = 0 ) then
        begin
            exit ;
        end ;
    end ;
    Result := True ;
end ;


// TGigabyte_uCode_Assembler methods...

procedure TGigabyte_uCode_Assembler.Initialize( Master : TMaster_Assembler ) ;

begin
    _Master := Master ;
end ;


{ Informs the assembler that the current assembly operation is now
  complete. }
procedure TGigabyte_uCode_Assembler.Terminate ;

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


function TGigabyte_uCode_Assembler.Assemble_Ex( Inputs : PChar ;
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

var A, AA, AAL, Next, Temp : string ;
    Any_Code : boolean ;
    B : int64 ;
    C, D : integer ;
    C1, C2 : char ;
    Conflict : boolean ;
    Dummy, Dummy1 : integer ;
    Cd, Code, Mask : int64 ;
    P : PChar ;
    S : string ;
    Symbol : PSymbol_Record ;
    UEC : TUnified_Exception ;
    ALU_Used, PC_Used, SP_Used, DA_Used, DA1_Used, Temp_Used : boolean ;
    TPC : word ;

label Ae, End_Assemble, Start_Over, Next_Item ;

begin // TGigabyte_uCode_Assembler.Assemble_Ex
    // Setup...
    TPC := Address ;
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

    // Remove any comments...
    A := Edit( Parse_Parameter( ';', Input_Buffer ), 16 or 128 or 256 ) ;
    if( length( A ) = 0 ) then // Blank line (or comment only)
    begin
        goto End_Assemble ;
    end ;
    Input_Buffer := A ;
    A := '' ;
    Any_Code := False ;

    // Get and process next token...
Start_Over:
    AAL := Get_Token ;
    _Master.Map( PC ) ;
    AA := Edit( AAL, 511 ) ;
    Next := copy( Input_Buffer, 1, 1 ) ;
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
        if( not Valid_Label( AA ) ) then
        begin
            Status.Log_Error( PChar( 'Invalid label: ' + AA ), nil, -1, 3 ) ;
            exit ;
        end ;
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
    if( ( Temp <> 'DEFINE' ) and ( Temp <> 'END' ) ) then
    begin
        if( Handle_Directives( AAL, Status ) ) then { If a directive }
        begin
            A := AAL ;
            goto End_Assemble ;
        end ;
    end ;
    if( length( Temp ) = 0 ) then // Blank line
    begin
        exit ;
    end ;
    Temp := Temp + ' ' + Input_Buffer ;

    { Handle instructions... }
    Code := 0 ;
    Mask := 0 ;
    ALU_Used := False ;
    PC_Used := False ;
    SP_Used := False ;
    DA_Used := False ;
    DA1_Used := False ;
    Temp_Used := False ;
    while( length( Temp ) > 0 ) do
    begin
        Dummy := pos( '/', Temp + '/' ) ;
        A := Edit( copy( Temp, 1, Dummy - 1 ), 8 + 16 + 128 + 256 ) ;
        Temp := copy( Temp, Dummy + 1, length( Temp ) ) ;
        if( ( Flags and ( ASF_Immediate_Mode or ASF_Disassemble ) ) = 0 ) then
        begin
            if( copy( A, 1, 7 ) = 'DEFINE ' ) then
            begin
                if( ( Current_Macro = nil ) and ( Define <> '' ) ) then // In a definition
                begin
                    Code := ROM_End ;
                end ;
                Current_Macro := nil ;
                A := lowercase( copy( A, 8, length( A ) ) ) ;
                if( ( A = 'reset' ) or ( A = 'check' ) or ( A = 'int' ) or ( A = 'vint' ) or ( A = 'pint' ) or ( A = '****' ) ) then
                begin
                    // These are valid special cases
                    if( Defines.IndexOf( A ) <> -1 ) then
                    begin
                        Status.Log_Error( PChar( 'Multiple definitions: ' + A ), nil, -1, 3 ) ;
                        exit ;
                    end ;
                end else
                begin
                    if( length( A ) <> 4 ) then
                    begin
                        Status.Log_Error( 'Invalid instruction', nil, -1, 3 ) ;
                        exit ;
                    end ;
                    for Dummy := 1 to 4 do
                    begin
                        if( pos( A[ Dummy ], '0123456789ABCDEFabcdef*' ) = 0 ) then
                        begin
                            Status.Log_Error( 'Invalid instruction', nil, -1, 3 ) ;
                            exit ;
                        end ;
                    end ;
                    for Dummy := 0 to Defines.Count - 1 do
                    begin
                        S := Defines[ Dummy ] ;
                        if( ( S = 'reset' ) or ( S = 'check' ) or ( S = 'int' ) or ( S = 'vint' ) or ( S = 'pint' ) or ( S = '****' ) ) then
                        begin
                            if( S = A ) then
                            begin
                                Status.Log_Error( PChar( 'Instruction conflict: ' + A ), nil, -1, 3 ) ;
                                exit ;
                            end ;
                            continue ; // Skip special cases
                        end ;
                        Conflict := False ;
                        for Dummy1 := 1 to 4 do
                        begin
                            C1 := A[ Dummy1 ] ;
                            C2 := S[ Dummy1 ] ;
                            if( ( ( C1 = '*' ) and ( C2 <> '*' ) ) or ( ( C1 <> '*' ) and ( C2 = '*' ) ) ) then
                            begin
                                Conflict := True ;
                                break ;
                            end ;
                            if( C1 <> C2 ) then
                            begin
                                break ;
                            end ;
                            if( Dummy1 = 4 ) then // Must have matched all positions
                            begin
                                Conflict := True ;
                            end ;
                        end ;
                        if( Conflict ) then
                        begin
                            Status.Log_Error( PChar( 'Instruction conflict: ' + A + ' conflicts with ' + S ), nil, -1, 3 ) ;
                            exit ;
                        end ;
                    end ;
                end ;
                Defines.AddObject( A, pointer( PC ) ) ;
                if( Define <> '' ) then
                begin
                    Code := ROM_End ;
                    Any_Code := True ;
                end ;
                Define := A ;
                continue ;
            end ;
            if( Current_Macro <> nil ) then
            begin
                Current_Macro.Add( A + ' / ' + AA ) ;
                AA := '' ;
                continue ;
            end ;
            if( Define = '' ) then
            begin
                Status.Log_Error( 'Code must be within DEFINE or MACRO', nil, -1, 3 ) ;
                continue ;
            end ;
        end ;

        Any_Code := True ;
        if( copy( A, 1, 1 ) = '+' ) then
        begin
            A := trim( uppercase( copy( A, 2, length( A ) ) ) ) ;
            if( A = 'PC' ) then
            begin
                if( ( Mask and ROM_Inc_PC ) <> 0 ) then
                begin
                    Status.Log_Error( 'Multiple PC increments', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( PC_Used ) then
                begin
                    Status.Log_Error( 'Incompatible SP accesses', nil, -1, 2 ) ;
                    exit ;
                end ;
                Code := Code or ROM_Inc_PC ;
                Mask := Mask or ROM_Inc_PC ;
                PC_Used := True ;
            end else
            if( A = 'DA' ) then
            begin
                if( ( Code and ROM_Inc_DA ) <> 0 ) then
                begin
                    Status.Log_Error( 'Multiple D/A increments', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( DA_Used ) then
                begin
                    Status.Log_Error( 'Incompatible D/A accesses', nil, -1, 2 ) ;
                    exit ;
                end ;
                Code := Code or ROM_Inc_DA ;
                Mask := Mask or ROM_Inc_DA ;
                DA_Used := True ;
            end else
            if( A = 'SP' ) then
            begin
                if( ( Code and ROM_Inc_SP ) <> 0 ) then
                begin
                    Status.Log_Error( 'Multiple SP increments', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( SP_Used ) then
                begin
                    Status.Log_Error( 'Incompatible SP accesses', nil, -1, 2 ) ;
                    exit ;
                end ;
                Code := Code or ROM_Inc_SP ;
                Mask := Mask or ROM_Inc_SP ;
                SP_Used := True ;
            end else
            if( A = 'DA1' ) then
            begin
                if( ( Mask and ROM_Inc_DA1 ) <> 0 ) then
                begin
                    Status.Log_Error( 'Multiple D/A1 increments', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( DA1_Used ) then
                begin
                    Status.Log_Error( 'Incompatible D/A1 accesses', nil, -1, 2 ) ;
                    exit ;
                end ;
                Code := Code or ROM_Inc_DA1 ;
                Mask := Mask or ROM_Inc_DA1 ;
                DA1_Used := True ;
            end else
            begin
                Status.Log_Error( 'Invalid increment target', nil, -1, 2 ) ;
                exit ;
            end ;
            continue
        end ;
        if( copy( A, 1, 1 ) = '-' ) then
        begin
            A := trim( uppercase( copy( A, 2, length( A ) ) ) ) ;
            if( A = 'SP' ) then
            begin
                if( ( Code and ROM_Dec_SP ) <> 0 ) then
                begin
                    Status.Log_Error( 'Multiple SP decrements', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( SP_Used ) then
                begin
                    Status.Log_Error( 'Incompatible SP accesses', nil, -1, 2 ) ;
                    exit ;
                end ;
                Code := Code or ROM_Dec_SP ;
                Mask := Mask or ROM_Dec_SP ;
                SP_Used := True ;
            end else
            if( A = 'PC' ) then
            begin
                if( ( Code and ROM_Dec_PC ) <> 0 ) then
                begin
                    Status.Log_Error( 'Multiple PC decrements', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( PC_Used ) then
                begin
                    Status.Log_Error( 'Incompatible PC accesses', nil, -1, 2 ) ;
                    exit ;
                end ;
                Code := Code or ROM_Dec_PC ;
                Mask := Mask or ROM_Dec_PC ;
                PC_Used := True ;
            end else
            if( A = 'TEMP' ) then
            begin
                if( ( Mask and ROM_Dec_Temp ) <> 0 ) then
                begin
                    Status.Log_Error( 'Multiple TEMP decrements', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( Temp_Used ) then
                begin
                    Status.Log_Error( 'Multiple Temp operations', nil, -1, 2 ) ;
                    exit ;
                end ;
                Code := Code or ROM_Dec_Temp ;
                Mask := Mask or ROM_Dec_Temp ;
                Temp_Used := True ;
            end else
            begin
                Status.Log_Error( PChar( 'Invalid decrement target: ' + A ), nil, -1, 2 ) ;
                exit ;
            end ;
            continue ;
        end ;
        if( A = 'RESUME' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at resume', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Load_Resume_Register ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'CHECK' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at check', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Kernel_Check ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'HALT' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at check', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Halt ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'SINTA' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at check', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Set_INTA ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'CINTA' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at check', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Clear_INTA ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'LOCK' ) then
        begin
            if( ( Code and ROM_External_Lock_Bus ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple bus locks', nil, -1, 3 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at lock', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Lock_Bus ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'UNLOCK' ) then
        begin
            if( ( Code and ROM_External_Unlock_Bus ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple bus unlocks', nil, -1, 3 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at unlock', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Unlock_Bus ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'LATCH SP' ) then
        begin
            if( ( Code and ROM_External_Latch_SP ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple SP latches', nil, -1, 3 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at latch SP', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Latch_SP ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'LATCH PC' ) then
        begin
            if( ( Code and ROM_External_Latch_PC ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple PC latches', nil, -1, 3 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at latch PC', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Latch_PC ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'UNLATCH' ) then
        begin
            if( ( Code and ROM_External_Unlatch ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple unlatches', nil, -1, 3 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at unlatch', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Unlatch ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'LATCH DA1' ) then
        begin
            if( ( Code and ROM_External_Latch_DA1 ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple D/A1 latches', nil, -1, 3 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at latch DA1', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Latch_DA1 ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'DI' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at DI', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Disable_Interrupts ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'EI' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at EI', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Enable_Interrupts ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'CC' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at CC', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Clear_Carry ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'CT' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at CT', nil, -1, 2 ) ;
                exit ;
            end ;
            if( Temp_Used ) then
            begin
                Status.Log_Error( 'Multiple uses of Temp register at CT', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Clear_Temp_Bit ;
            Mask := Mask or ROM_Internal_Operation ;
            Temp_Used := True ;
            continue ;
        end ;
        if( A = 'END' ) then
        begin
            if( ( Code and ROM_End ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple Ends', nil, -1, 3 ) ;
                exit ;
            end ;
            Code := Code or ROM_End ;
            Mask := Mask or ROM_End ;
            continue ;
        end ;
        if( A = 'SBBZ' ) then
        begin
            if( ( Code and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at SBBZ', nil, -1, 3 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Set_Bit_In_Z ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'CBBZ' ) then
        begin
            if( ( Code and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at CBBZ', nil, -1, 3 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Clear_Bit_In_Z ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'MREAD' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at MREAD', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Read_Memory or ROM_32_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'MREAD8' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at MREAD8', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Read_Memory or ROM_8_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'MREAD16' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at MREAD16', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Read_Memory or ROM_16_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'MWRITE' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at MWRITE', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Write_Memory or ROM_32_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'MWRITE8' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at MWRITE8', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Write_Memory or ROM_8_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'MWRITE16' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at MWRITE16', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Write_Memory or ROM_16_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'INPUT' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at INPUT', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Input or ROM_32_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'INPUT8' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at INPUT8', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Input or ROM_8_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'INPUT16' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at INPUT16', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Input or ROM_16_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'OUTPUT' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at OUTPUT', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Output or ROM_32_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'OUTPUT8' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at OUTPUT8', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Output or ROM_8_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'OUTPUT16' ) then
        begin
            if( ( Mask and ROM_External_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple external operations at OUTPUT16', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_External_Output or ROM_16_Bit ;
            Mask := Mask or ROM_External_Operation ;
            continue ;
        end ;
        if( A = 'IFZS' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFZS', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_Bit_In_Z ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'IFZC' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFZC', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_Not_Bit_In_Z ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'IFZ' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFZ', nil, -1, 2 ) ;
                exit ;
            end ;
            if( Temp_Used ) then
            begin
                Status.Log_Error( 'Multiple uses of Temp register at EI', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_temp_Zero ;
            Mask := Mask or ROM_Internal_Operation ;
            Temp_Used := True ;
            continue ;
        end ;
        if( A = 'IFNZ' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFNZ', nil, -1, 2 ) ;
                exit ;
            end ;
            if( Temp_Used ) then
            begin
                Status.Log_Error( 'Multiple uses of Temp register at EI', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_Temp_Not_Zero ;
            Mask := Mask or ROM_Internal_Operation ;
            Temp_Used := True ;
            continue ;
        end ;
        if( A = 'IFSS' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFSS', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_Bit_In_Status ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'IFSC' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFSC', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_Not_Bit_In_Status ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'IFTS' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFTS', nil, -1, 2 ) ;
                exit ;
            end ;
            if( Temp_Used ) then
            begin
                Status.Log_Error( 'Multiple uses of Temp register at EI', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_Bit_In_Temp ;
            Mask := Mask or ROM_Internal_Operation ;
            Temp_Used := True ;
            continue ;
        end ;
        if( A = 'IFTC' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFTC', nil, -1, 2 ) ;
                exit ;
            end ;
            if( Temp_Used ) then
            begin
                Status.Log_Error( 'Multiple uses of Temp register at EI', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_Not_Bit_In_Temp ;
            Mask := Mask or ROM_Internal_Operation ;
            Temp_Used := True ;
            continue ;
        end ;
        if( A = 'IFNK' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at IFNK', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_If_Not_Kernel ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'CLEAR CLOCK' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at CLEAR CLOCK', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Clear_Clock ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'SHL' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at SHL', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_SHL shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'SHLC' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at SHLC', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_SHLC shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'SHR' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at SHR', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_SHR shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'SHRC' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at SHRC', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_SHRC shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'ST' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at ST', nil, -1, 2 ) ;
                exit ;
            end ;
            if( Temp_Used ) then
            begin
                Status.Log_Error( 'Multiple uses of Temp register at ST', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_Set_Temp_Bit ;
            Mask := Mask or ROM_Internal_Operation ;
            Temp_Used := True ;
            continue ;
        end ;
        if( A = 'COMPLEMENT' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at COMPLEMENT', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_Complement shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'INCREMENT' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at INCREMENT', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_Increment shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'DECREMENT' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at DECREMENT', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_Decrement shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'AND' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at AND', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_AND shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'NAND' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at NAND', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_NAND shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'OR' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at OR', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_OR shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'NOR' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at NOR', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_NOR shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'XOR' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at XOR', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_XOR shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'XNOR' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at XNOR', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_XNOR shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'ADD' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at ADD', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_ADD shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'ADDC' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at ADDC', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_ADDC shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'SUBTRACT' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at SUBTRACT', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_SUB shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'SUBTRACTB' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at SUBTRACTB', nil, -1, 2 ) ;
                exit ;
            end ;
            Cd := ALU_SUBB ;
            Cd := Cd shl ROM_Data_Shift ;
            Code := Code or ROM_Internal_ALU_Op or Cd ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'DAA' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at DAA', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_DAA shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'DAS' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at DAS', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_DAS shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( A = 'NOP' ) then
        begin
            continue ;
        end ;
        if( A = 'MULTIPLY' ) then
        begin
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at MULTIPLY', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or ROM_Internal_ALU_Op or ( ALU_Multiply shl ROM_Data_Shift ) ;
            Mask := Mask or ROM_Internal_Operation ;
            continue ;
        end ;
        if( copy( A, 1, 5 ) = 'JUMP ' ) then
        begin
            A := copy( A, 6, length( A ) ) ;
            if( Defines = nil ) then
            begin
                D := -1 ;
            end else
            begin
                D := Defines.Indexof( A ) ;
            end ;
            if( D <> -1 ) then
            begin
                C := integer( Defines.Objects[ D ] ) ;
            end else
            begin
                UEC := Evaluate( A, C, D ) ;
                if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                begin
                    Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                    exit ;
                end ;
                if( UEC <> nil ) then
                begin
                    if( UEC.Get_Error <> 4 ) then
                    begin
                        continue ;
                    end ;
                    _Master.Add_Reference( PChar( A ), 2, PC + 1 ) ;
                    C := 0 ;
                end ;
                C := C div 5 ; // Master assembler store byte addresses, so convert to a microcode word index
            end ;
            if( ( C < 0 ) or ( C > 65535 ) ) then
            begin
                Status.Log_Error( 'Jump destination is out of range', nil, -1, 3 ) ;
                exit ;
            end ;
            if( C = TPC ) then
            begin
                Status.Log_Error( 'Infinite loop', nil, -1, 3 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_Data ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple immediate data operations at JUMP', nil, -1, 2 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations at JUMP', nil, -1, 2 ) ;
                exit ;
            end ;
            B := C and $FFFF ; // Trim off extra bits
            Code := Code or ( B shl ROM_Data_Shift ) or ROM_Internal_Jump ;
            Mask := Mask or ROM_Data or ROM_Internal_Operation ;
            continue ;
        end ;

        Dummy := pos( '>', A ) ;
        if( Dummy > 0 ) then // Move instruction
        begin
            AA := lowercase( Trim( copy( A, Dummy + 1, length( A ) ) ) ) ;
            A := lowercase( Trim( copy( A, 1, Dummy - 1 ) ) ) ;
            if( ( Mask and ROM_Source_Destination ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple move operations', nil, -1, 2 ) ;
                exit ;
            end ;
            if( ( Mask and ROM_Internal_Operation ) <> 0 ) then
            begin
                Status.Log_Error( 'Multiple internal operations', nil, -1, 2 ) ;
                exit ;
            end ;

            if( A = 'bus' ) then // from bus
            begin
                if( AA = 'bus' ) then
                begin
                    Status.Log_Error( 'bus cannot be both source or destination', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( AA = 'alul' ) then
                begin
                    Status.Log_Error( 'Invalid destination', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( AA = 'aluh' ) then
                begin
                    Status.Log_Error( 'Invalid destination', nil, -1, 2 ) ;
                    exit ;
                end ;
                B := Source( True, AA ) ;
                if( B = -1 ) then
                begin
                    Status.Log_Error( PChar( 'Invalid destination: ' + AA ), nil, -1, 2 ) ;
                    exit ;
                end ;
                Code := Code or ROM_Internal_From_Bus ;
                AA := '' ;
            end else
            if( AA = 'bus' ) then  // To bus
            begin
                if( A = 'bus' ) then
                begin
                    Status.Log_Error( 'bus cannot be both source or destination', nil, -1, 2 ) ;
                    exit ;
                end ;
                if( A = 'alual' ) then
                begin
                    Status.Log_Error( PChar( 'Invalid source: ' + A ), nil, -1, 2 ) ;
                    exit ;
                end ;
                if( A = 'aluah' ) then
                begin
                    Status.Log_Error( PChar( 'Invalid source: ' + A ), nil, -1, 2 ) ;
                    exit ;
                end ;
                if( A = 'alubl' ) then
                begin
                    Status.Log_Error( PChar( 'Invalid source: ' + A ), nil, -1, 2 ) ;
                    exit ;
                end ;
                if( A = 'alubh' ) then
                begin
                    Status.Log_Error( PChar( 'Invalid source: ' + A ), nil, -1, 2 ) ;
                    exit ;
                end ;
                B := Source( False, A ) ;
                if( B = -1 ) then // Numeric literal or invalid
                begin
                    UEC := _Master.Evaluate_Ex( PChar( A ), B, 0 ) ;
                    if( ( UEC <> nil ) and ( ( Flags and 1 ) = 1 ) ) then // Immediate mode error
                    begin
                        Status.Log_Error( Pchar( ET( UEC.Get_Error ) + ' "' + A + '"' ), nil, -1, 3 ) ;
                        exit ;
                    end ;
                    if( UEC <> nil ) then
                    begin
                        if( UEC.Get_Error <> 4 ) then
                        begin
                            continue ;
                        end ;
                        Status.Log_Error( PChar( 'Invalid source: ' + A ), nil, -1, 2 ) ; // Must be a pre-defined value
                        exit ;
                    end ;
                    if( ( B > 65535 ) or ( B < -32768 ) ) then
                    begin
                        Status.Log_Error( 'Numeric value out of range', nil, -1, 2 ) ;
                        exit ;
                    end ;
                    if( ( Mask and ROM_Data ) <> 0 ) then
                    begin
                        Status.Log_Error( 'Multiple uses of immediate data', nil, -1, 2 ) ;
                        exit ;
                    end ;
                    B := B and $FFFF ;
                    Code := Code or B shl ROM_Data_Shift ;
                    Mask := Mask or ROM_Data ;
                    B := ROM_Source_Data ;
                end ; // if( B = -1 )
                Code := Code or ROM_Internal_To_Bus ;
                AA := '' ;
            end else
            begin
                Status.Log_Error( 'bus must be either source or destination', nil, -1, 2 ) ;
                exit ;
            end ;
            Code := Code or B ;
            Mask := Mask or ROM_Source_Destination ;
            case Code and ROM_Source_Destination of
                ROM_Source_PC_Lo,
                ROM_Source_PC_Hi:
                    begin
                        if( PC_Used ) then
                        begin
                            Status.Log_Error( 'Multiple uses of SP', nil, -1, 2 ) ;
                            exit ;
                        end ;
                        PC_Used := True ;
                    end ;
                ROM_Source_SP_Lo,
                ROM_Source_SP_Hi:
                    begin
                        if( SP_Used ) then
                        begin
                            Status.Log_Error( 'Multiple uses of SP', nil, -1, 2 ) ;
                            exit ;
                        end ;
                        SP_Used := True ;
                    end ;
                ROM_Source_DA_Lo,
                ROM_Source_DA_Hi:
                    begin
                        if( DA_Used ) then
                        begin
                            Status.Log_Error( 'Multiple uses of DA', nil, -1, 2 ) ;
                            exit ;
                        end ;
                        DA_Used := True ;
                    end ;
                ROM_Source_Temp_Lo,
                ROM_Source_Temp_Hi:
                    begin
                        if( Temp_Used ) then
                        begin
                            Status.Log_Error( 'Multiple uses of Temp', nil, -1, 2 ) ;
                            exit ;
                        end ;
                        Temp_Used := True ;
                    end ;
                ROM_Source_DA1_Lo,
                ROM_Source_DA1_Hi:
                    begin
                        if( DA1_Used ) then
                        begin
                            Status.Log_Error( 'Multiple uses of DA1', nil, -1, 2 ) ;
                            exit ;
                        end ;
                        DA1_Used := True ;
                    end ;
            end ;

            continue ;
        end ; // if( Dummy > 0 )

        if Length( A ) > 0 then
        begin
            Status.Log_Error( PChar( 'Unknown instruction: "' + A + '"' ), nil, -1, 3 ) ;
AE:
            // Unrecognized mnuemonic...
            _Master.Put_Token( PChar( AA ) ) ;
            UEC := _Master.Expand( nil, P, C, Status ) ;
            if( UEC <> nil ) then
            begin
                Err := True ;
            end else
            begin
                setlength( A, C ) ;
                move( P[ 0 ], A[ 1 ], C ) ;
            end ;
        end else
        begin
            if( Status <> nil ) then
            begin
                Status.Code := Status.Code + length( A ) ;
            end ;
        end ;
    end ; // while( length( AA ) > 0 )

End_Assemble:

    if(
        ( ( Code and ROM_Internal_Operation ) = ROM_Internal_To_Bus )
        or
        ( ( Code and ROM_Internal_Operation ) = ROM_Internal_From_Bus )
      ) then // Move to/from Bus
    begin
        if(
            ( ( Code and ROM_External_Operation ) = ROM_External_Read_Memory )
            or
            ( ( Code and ROM_External_Operation ) = ROM_External_Write_Memory )
            or
            ( ( Code and ROM_External_Operation ) = ROM_External_Input )
            or
            ( ( Code and ROM_External_Operation ) = ROM_External_Output )
          ) then // I/O to Bus
        begin
            Status.Log_Error( 'Multiple uses of Bus register', nil, -1, 3 ) ;
            exit ;
        end ;
    end ;
    if(
        ( ( Code and ROM_Internal_Operation ) = ROM_Internal_Set_Bit_In_Z )
        or
        ( ( Code and ROM_Internal_Operation ) = ROM_Internal_Clear_Bit_In_Z )
        or
        ( ( Code and ROM_Internal_Operation ) = ROM_Internal_If_Bit_In_Z )
        or
        ( ( Code and ROM_Internal_Operation ) = ROM_Internal_If_Not_Bit_In_Z )
        or
        ( ( Code and ROM_Internal_Operation ) = ROM_Internal_If_Bit_In_Status )
        or
        ( ( Code and ROM_Internal_Operation ) = ROM_Internal_If_Not_Bit_In_Status )
      ) then // Move to/from Bus
    begin
        if(
            ( ( Code and ROM_External_Operation ) = ROM_External_Read_Memory )
            or
            ( ( Code and ROM_External_Operation ) = ROM_External_Input )
          ) then // I/O changing Bus
        begin
            Status.Log_Error( 'Multiple uses of Bus register', nil, -1, 3 ) ;
            exit ;
        end ;
    end ;

    if( Any_Code ) then
    begin
        setlength( Assembled_Code, 5 ) ; { Return value }
        Machines := PChar( Assembled_Code ) ;
        MachineL := length( Assembled_Code ) ;
        move( Code, Machines[ 0 ], MachineL ) ;
    end else
    begin
        MachineL := 0 ;
        Machines := nil ;
    end ;
    Outputs := nil ;

    Address := TPC ;
end ; // TGigabyte_uCode_Assembler.Assemble_Ex


{ Returns the default radix (base) of numeric literals. }
function TGigabyte_uCode_Assembler.Default_Radix : longint ;

begin
    Result := Base ;
end ;


{ Returns the default size of numeric literals, in bits. }
function TGigabyte_uCode_Assembler.Default_Size : longint ;

begin
    Default_Size := 8 ;
end ;


{ Returns facility code for this class. }
function TGigabyte_uCode_Assembler.Facility_Code : longint ;

begin
    Result := Gigabyte_uCodeAssemblerErr_Facility ;
end ;


const _Extensions : string = 'mic' ;

function TGigabyte_uCode_Assembler.Source_Extensions : PChar ;

begin
    Result := PChar( _Extensions ) ;
end ;


const _Valid_Symbol_Initial : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_' ;

function TGigabyte_uCode_Assembler.Valid_Symbol_Initial : PChar ;

begin
    Result := PChar( _Valid_Symbol_Initial ) ;
end ;


const _Valid_Symbol_After : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_0123456789' ;

function TGigabyte_uCode_Assembler.Valid_Symbol_After : PChar ;

begin
    Result := PChar( _Valid_Symbol_After ) ;
end ;


function TGigabyte_uCode_Assembler.Begin_Assembly : int64 ;

var outputs, machines : PChar ;
    MachineL : longint ;
    Address : int64 ;

begin
    Result := 0 ;

    // Make sure old data is deleted
    Outputs := nil ;
    machines := nil ;
    MachineL := 0 ;

    Address := 0 ;
    Defines.Free ;
    Current_Macro.Free ;
    Macros.Free ;
    Defines := nil ;
    Current_Macro := nil ;
    Macros := nil ;

    // Create contextual structures...
    Defines := TStringList.Create ;
    Current_Macro := nil ;
    Macros := TStringList.Create ;
    Define := '' ;
end ;


function TGigabyte_uCode_Assembler.Finish_Assembly( Context : int64 ; var outputs,
    machines : PChar ; var MachineL : longint ; var Address : int64 ;
    Status : TAssembler_Status ; Flags : longint ) : TUnified_Exception ;

var A : array[ 0..1, 0..3 ] of integer ;
    ALoop, Loop, Loop0, Loop1, Loop2, Loop3 : integer ;
    Index, Max : integer ;
    S : string ;

begin
    Result := Set_Error( 0 ) ;

    // Setup...
    Machines := nil ;
    if( ( Flags and ( ASF_Immediate_Mode or ASF_Disassemble ) ) = 0 ) then
    begin
        fillchar( Addresses, sizeof( Addresses ), 0 ) ;
        Defines.Sort ;
        Max := Defines.Count - 1 ;

        // Build list of special addresses...
        for Loop := Defines.Count - 1 downto 0 do
        begin
            S := Defines[ Loop ] ;
            if( S = '****' ) then
            begin
                Addresses[ 65540 ] := integer( Defines.Objects[ Loop ] ) ;
                Max := Loop ;
            end else
            if( S = 'check' ) then
            begin
                Addresses[ 65539 ] := integer( Defines.Objects[ Loop ] ) ;
                Max := Loop ;
            end else
            if( S = 'int' ) then
            begin
                Addresses[ 65538 ] := integer( Defines.Objects[ Loop ] ) ;
                Max := Loop ;
            end else
            if( S = 'vint' ) then
            begin
                Addresses[ 65537 ] := integer( Defines.Objects[ Loop ] ) ;
                Max := Loop ;
            end else
            if( S = 'pint' ) then
            begin
                Addresses[ 65536 ] := integer( Defines.Objects[ Loop ] ) ;
                Max := Loop ;
            end ;
        end ; // for Loop := Defines.Count - 1 downto 0

        // Build instruction index...
        for Loop := 0 to Max - 1 do
        begin
            S := Defines[ Loop ] ;
            for ALoop := 0 to 3 do
            begin
                if( S[ ALoop ] = '*' ) then
                begin
                    A[ 0, ALoop ] := 0 ;
                    A[ 1, ALoop ] := 255 ;
                end else
                begin
                    A[ 0, ALoop ] := ord( S[ ALoop ] ) ;
                    A[ 1, ALoop ] := A[ 1, ALoop ] ;
                end ;
            end ; // for ALoop := 0 to 3
            for Loop0 := A[ 0, 0 ] to A[ 1, 0 ] do
            begin
                for Loop1 := A[ 0, 1 ] to A[ 1, 1 ] do
                begin
                    for Loop2 := A[ 0, 2 ] to A[ 1, 2 ] do
                    begin
                        for Loop3 := A[ 0, 3 ] to A[ 1, 3 ] do
                        begin
                            Index := ( Loop0 shl 12 ) or ( Loop1 shl 8 ) or ( Loop2 shl 4 ) or Loop3 ;
                            Addresses[ Index ] := integer( Defines.Objects[ Loop ] ) ;
                        end ;
                    end ;
                end ;
            end ; // for Loop0 := A[ 0, 0 ] to A[ 1, 0 ]
        end ; // for Loop := 0 to Max

        S := '.=65535.' ;
        Handle_Directives( S, Status ) ; // Point us to the vector area

        // Return results...
        Machines := PChar( @Addresses ) ;
        MachineL := sizeof( Addresses ) ;
    end ;
    
    // Free up the contextual data...
    Defines.Free ;
    Current_Macro.Free ;
    Macros.Free ;
    Defines := nil ;
    Current_Macro := nil ;
    Macros := nil ;
end ; // TGigabyte_uCode_Assembler.Finish_Assembly


function TGigabyte_uCode_Assembler.Evaluate( const X : string ;
    var _Result : longint ; PC_Adjustment : longint ) : TUnified_Exception ;

var I64 : int64 ;
    Imaginary : extended ;
    E, P : integer ;
    _R : extended ;

begin
    Result := Set_Error( 0 ) ;
    Eval( X, _R, Imaginary, E, P ) ;
    if( E = 0 ) then // No symbols involved
    begin
        _Result := trunc( _R ) * 5 ;
        exit ;
    end ;
    Result := _Master.Evaluate( PChar( X ), I64 ) ;
    _Result := I64 ;
end ; { TGigabyte_uCode_Assembler.Evaluate }


function TGigabyte_uCode_Assembler.Handle_Directives( var Value : string ;
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
