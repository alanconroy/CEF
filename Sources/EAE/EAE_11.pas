{$N+}
{
        Program Name : EAE
        Package Name : CEF
        Purpose      : DEC EAE component for CEF
        Institution  : 
        Date Written : 5-Jul-2007
        Written By   : Alan Conroy
        Version      : 1.0

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

	  This unit implements a DEC EAE.


        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan


        Conditionals:
            TEST    Causes test to run upon unit initialization.
}

unit EAE_11 ;

interface

{$I EDEFINES.INC}

uses // Borland...
     Windows, // LoadLibrary
     Classes,

     // C&C...
     Compatib,
     Collect, { TCollection }
     _DebugIn, // TDebug_Interface
     DebugInt, // TText_Debugger
     _Streams, // TCOM_Stream
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TUI_Interface 
     CEF, { TBase_Memory }
     _CEFUtil ; // TCEF_Watchpoint

const CEFEAE_Facility = -1 ;
      CEFEAEErr_Success = 0 ;
      CEFEAEErr_Invalid_Range = 1 ;
      CEFEAEErr_Component_Not_Found = 2 ;
      CEFEAEErr_No_Matching_Watchpoint = 3 ;
      CEFEAEErr_Access_Violation = 4 ;
      CEFEAEErr_Address_Out_Of_Range = 5 ;
      CEFEAEErr_Invalid_Component = 6 ;
      CEFEAEErr_Busy = 7 ;

type TEAE_Memory = class( TBase_Memory )
                       public
                           function Facility_Code : longint ; override ;
                           procedure Get_Address_Range( var Low, High : int64 ) ;
                               override ;
                   end ;

type TEAE = class( TBase_Component )
        private { Instance data... }
            Access_Mode : integer ;
            Inputs, Outputs : TList ;
            Profiling : boolean ; { True if profiling accesses }
            _UI : TUI_Interface ;
            Watchpoint_List : TCEF_Watchpoint_Manager ;
            _Tag : longint ;
            _Parent : TComponent ;
            Blocked : boolean ;
            _Memory : TEAE_Memory ;
            _Logger : TCEF_Logger ;

        private // Registers...
            AC : word ;
            MQ : word ;
            SC : word ;
            SR : word ;
            Normalize : word ;

        private // Internal utility routines...
            function Default_Input : TComponent ;
            function Default_Output : TComponent ;
            function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

        protected // Property handlers...
            function Get_ACMQ : longint ;
            procedure Set_ACMQ( Value : longint ) ;

            function Translate_Error( Code : longint ) : string ; override ;

    	public { Public instance data... }
            _Serial_Number : integer ;

            property ACMQ : longint
                read Get_ACMQ
                write Set_ACMQ ;

        public { API... }
            function Facility_Code : longint ; override ;

            function Initialize( UI : TUI_Interface ) : TUnified_Exception ; override ;

            function Terminate : TUnified_Exception ; override ;

            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; override ;

            function Component_Type : longint ; override ;

            function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

            function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

            function Debugger : TDebug_Interface ; override ;

            function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                Memory : boolean ) : TUnified_Exception ; override ;

            function Disconnect_Input( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Disconnect_Output( Component : TComponent ) : TUnified_Exception ;
                override ;

            function Examine( Address : int64 ; var Size : longint ;
                Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

            function Get_Access_Mode( Address : int64 ;
                Memory : boolean ) : longint ; override ;

            function Get_Profiling : boolean ; override ;

            procedure Increment_Clock( Count : integer ) ;

            function Input_Component( Index : longint ) : TComponent ; override ;

            function Memory : TMemory ; override ;

            function Name : PChar ; override ;

            function Output_Component( Index : longint ) : TComponent ;
                override ;

            function Read( Address : int64 ; Size : longint ;
                IO_Type : longint ) : boolean ; override ;

            function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                Typ : longint ) : TUnified_Exception ; override ;

            procedure Set_Profiling( _On, Children : boolean ) ;  override ;

            function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; override ;

            procedure Show_Status ; override ;

            function Write( Address : int64 ; Value, Size : longint ;
                IO_Type : longint ) : TUnified_Exception ; override ;

            procedure Set_Tag( Value : longint ) ; override ;

            function Get_Tag : longint ; override ;

            function Get_Parent : TComponent ; override ;

            procedure Set_Parent( Component : TComponent ) ; override ;

            procedure Wake ; override ;

            function Get_Logger : TCEF_Logger ; override ;

            procedure Set_Logger( Value : TCEF_Logger ) ; override ;
     end ; // TEAE

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
{$IFDEF Test}
     Dialogs,
{$ENDIF}
     Sysutils, // Allocmem

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     UE, // Create_Simple_UE
     Num1s,
     Parse ; // TString_Parser

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ;
    external 'CEF_Util.dll' ;


function TEAE.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        CEFEAEErr_Success: _Error := 'Success' ;
        CEFEAEErr_Invalid_Range: _Error := 'Invalid range' ;
        CEFEAEErr_Component_Not_Found: _Error := 'Component not found' ;
        CEFEAEErr_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        CEFEAEErr_Access_Violation: _Error := 'Access violation' ;
        CEFEAEErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        CEFEAEErr_Invalid_Component: _Error := 'Invalid component' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { tFS_EI.Translate_Error }


function Octal( const Value : string ) : integer ; overload ;

var Loop : integer ;

begin
    Result := 0 ;
    for Loop := 1 to length( Value ) do
    begin
        Result := ( Result shl 3 ) or ( ord( Value[ Loop ] ) - 48 ) ;
    end ;
end ;


function Octal( Value : integer ) : integer ; overload ;

begin
    Result := Octal( inttostr( Value ) ) ;
end ;



{ TCEF_EAE_Debugger methods... }

type TCEF_EAE_Debugger = class( TText_Debugger )
                                private
                                    _IO : TEAE ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property IO : TEAE
                                        read _IO
                                        write _IO ;
                            end ;

function TCEF_EAE_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

    function Pointer_To_String( P : pointer ) : string ;

    begin
        if( P = nil ) then
        begin
            Result := 'nil' ;
        end else
        begin
            Result := Num1( longint( P ) ) ;
        end ;
    end ;


    function Boolean_To_String( B : boolean ) : string ;

    begin
        if( B ) then
        begin
            Result := 'True' ;
        end else
        begin
            Result := 'False' ;
        end ;
    end ;


    function Access_Mode_To_String( AM : integer ) : string ;

    begin
        if( ( AM and Access_All ) = Access_All ) then
        begin
            Result := 'Access_All' ;
            exit ;
        end ;
        if( ( AM and Access_RW ) = Access_RW ) then
        begin
            Result := 'Access_RW' ;
            exit ;
        end ;
        if( AM = 0 ) then
        begin
            Result := 'Access_None' ;
            exit ;
        end ;
        if( ( AM and Access_Read ) = Access_Read ) then
        begin
            Result := 'Access_Read' ;
        end ;
        if( ( AM and Access_Write ) = Access_Write ) then
        begin
            if( Result <> '' ) then
            begin
                Result := Result + ', ' ;
            end ;
            Result := Result + 'Access_Write' ;
        end ;
        if( ( AM and Access_Execute ) = Access_Execute ) then
        begin
            if( Result <> '' ) then
            begin
                Result := Result + ', ' ;
            end ;
            Result := Result + 'Access_Execute' ;
        end ;
        if( Result = '' ) then
        begin
            Result := '* unknown *' ;
        end ;
    end ;


var I : TText_Debugger ;
    I1 : TDebug_Interface ;

begin
    I := TText_Debugger.Create ;
    case Ordinal of
        0 : I.Title := PChar( 'Access_Mode = ' + Access_Mode_To_String( IO.Access_Mode ) ) ;
        1 : begin
                if( IO.Default_Input = nil ) then
                begin
                    I.Title := 'Default_Input = nil' ;
                end else
                begin
                    I1 := IO.Default_Input.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Input = ' + Pointer_To_String( IO.Default_Input ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Input = ' + Pointer_To_String( IO.Default_Input ) ) ;
                end ;
            end ;
        2 : begin
                if( IO.Default_Output = nil ) then
                begin
                    I.Title := 'Default_Output = nil' ;
                end else
                begin
                    I1 := IO.Default_Output.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Output = ' + Pointer_To_String( IO.Default_Output ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Output = ' + Pointer_To_String( IO.Default_Output ) ) ;
                end ;
            end ;
        3 : I.Title := PChar( 'Profiling = ' + Boolean_To_String( IO.Profiling ) ) ;
        4 : I.Title := PChar( '_Serial_Number = ' + Num1( IO._Serial_Number ) ) ;
        5 : begin
                if( IO._UI = nil ) then
                begin
                    I.Title := '_UI = nil' ;
                end else
                begin
                    I1 := IO._UI.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( '_UI = ' + Pointer_To_String( IO._UI ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( '_UI = ' + Pointer_To_String( IO._UI ) ) ;
                end ;
            end ;
        6 : I.Title := Pchar( 'Watchpoint_List = ' + Pointer_To_String( pointer( IO.Watchpoint_List ) ) ) ;
        7 : I.Title := PChar( 'Blocked = ' + Boolean_To_String( IO.Blocked ) ) ;
        8 : I.Title := PChar( 'AC = ' + inttostr( IO.AC ) ) ;
        9 : I.Title := PChar( 'MQ = ' + inttostr( IO.MQ ) ) ;
        10 : I.Title := PChar( 'SC = ' + inttostr( IO.SC ) ) ;
        11 : I.Title := PChar( 'SR = ' + inttostr( IO.SR ) ) ;
    end ;

    Result := PDebug_Interface( I ) ;
end ;


function TCEF_EAE_Debugger.Count : longint ;

begin
    Result := 8 ;
end ;


// TEAE_Memory methods...

function TEAE_Memory.Facility_Code : longint ;

begin
    Result := CEFEAE_Facility ;
end ;


procedure TEAE_Memory.Get_Address_Range( var Low, High : int64 ) ;

begin
    Low := Octal( '777300' ) ;
    High := Octal( '777317' ) ;
end ;



{ TEAE methods... }

function TEAE.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TEAE.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


function TEAE.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_List.Watchpoint_At( Address ) ;
end ;


// Property handlers...

function TEAE.Get_ACMQ : longint ;

begin
    Result := AC ;
    Result := Result shl 16 ;
    Result := Result or MQ ;
end ;


procedure TEAE.Set_ACMQ( Value : longint ) ;

begin
    AC := Value shr 16 ;
    MQ := Value and $FFFF ;
end ;


{ API... }

function TEAE.Facility_Code : longint ;

begin
    Facility_Code := CEFEAE_Facility ;
end ;


function TEAE.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    Profiling := False ;
    _UI := UI ;
    Watchpoint_List := Get_Watchpoint_Manager ;
    Access_Mode := Access_RW or Access_Execute ;
    _Memory := TEAE_Memory.Create ;

    Initialize := Set_Error( CEFEAEErr_Success ) ;
end ; { TEAE.Initialize }


function TEAE.Terminate : TUnified_Exception ;

begin
    if( _UI <> nil ) then
    begin
        _UI.Termination_Notice( self ) ;
    end ;
    Watchpoint_List.Terminate ;
    Watchpoint_List := nil ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Terminate := Set_Error( CEFEAEErr_Success ) ;
end ; { TEAE.Terminate }



function TEAE.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Clear_Watchpoint := nil ;
    if( not Memory ) then
    begin
        Result := Watchpoint_List.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( CEFEAEErr_No_Matching_Watchpoint ) ;
    end ;
end ;


function TEAE.Component_Type : longint ;

begin
    Component_Type := Component_Type_Memory ;
end ;


function TEAE.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := Set_Error( CEFEAEErr_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Connect_Input := Set_Error( CEFEAEErr_Success ) ;
end ;


function TEAE.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := Set_Error( CEFEAEErr_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Connect_Output := Set_Error( CEFEAEErr_Success ) ;
end ;


function TEAE.Debugger : TDebug_Interface ;

begin
    Result := TCEF_EAE_Debugger.Create ;
    TCEF_EAE_Debugger( Result ).IO := Self ;
end ;


function Special_Form( V : longint ) : boolean ;

begin
    if( ( V and 63 ) <> 0 ) then
    begin
        Result := False ;
        exit ;
    end ;
    if( ( longword( V ) and $E0000000 ) <> $E0000000 ) then
    begin
        Result := False ;
        exit ;
    end ;
    while( ( V and 1 ) = 0 ) do
    begin
        V := ( V shr 1 ) or $80000000 ;
    end ;
    Result := ( V = -1 ) ;
end ;


function TEAE.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count : integer ;
    Temp : longint ;
    Temp_High : longint ;
    Value : smallint ;

begin
    Result := nil ;
    if(
        ( not Memory )
        or
        ( Address < Octal( '777300' ) )
        or
        ( Address > Octal( '777317' ) )
        or
        ( Size = 0 )
      ) then
    begin
        exit ;
    end ;
    _Buffer := PChar( Buffer ) ;
    Count := ( Size + 7 ) div 8 ; // Bits to bytes
    if( Count > 2 ) then
    begin
        while( Count > 0 ) do
        begin
            if( Count = 1 ) then
            begin
                Deposit( Address, 1, Buffer, Memory ) ;
            end else
            begin
                Deposit( Address, 2, Buffer, Memory ) ;
            end ;
            Count := Count - 2 ;
            Address := Address + 2 ;
            Buffer := pointer( integer( Buffer ) + 2 ) ;
        end ;
        exit ;
    end ;
    Value := ord( _Buffer[ 0 ] ) ;
    if( Count > 1 ) then
    begin
        Value := Value or ( ord( _Buffer[ 1 ] ) shl 8 ) ;
    end ;
    try
        try
            case Address - Octal( '777300' ) of
                0 : // Divide (777300)
                    begin
                        if( Count = 1 ) then
                        begin
                            exit ;
                        end ;
                        Temp := ACMQ ;
                        if( Value = 0 ) then // Division by 0 = overflow
                        begin
                            if( ( Value and $8000 ) <> 0 ) then
                            begin
                                SR := ( SR or 64 ) and 127 ;
                            end else
                            begin
                                SR := ( SR or 128 ) and ( not 64 ) ;
                            end ;
                        end else
                        begin
                            MQ := Temp div Value ;
                            AC := Temp mod Value ;
                            if(
                                ( ( Temp div Value ) > 32767 )
                                or
                                ( ( Temp div Value ) < -32768 )
                              ) then // Overflow
                            begin
                                if( ( Value and $8000 ) <> 0 ) then
                                begin
                                    SR := ( SR or 64 ) and 127 ;
                                end else
                                begin
                                    SR := ( SR or 128 ) and ( not 64 ) ;
                                end ;
                            end else
                            begin
                                if( MQ > 32767 ) then
                                begin
                                    SR := SR or 64 or 128 ;
                                end else
                                begin
                                    SR := SR and 63 ;
                                end ;
                            end ;
                        end ;
                        SC := 16 ;
                        SR := SR and ( not 2 ) ;
                    end ;
                2 : // AC (777302)
                    begin
                        if( Count = 1 ) then
                        begin
                            Value := Value or ( AC and $FF00 ) ;
                        end ;
                        AC := Value ;
                        exit ;
                    end ;
                3 : // AC high byte  (777303)
                    begin
                        Value := ( Value shl 8 ) or ( AC and $FF ) ;
                        AC := Value ;
                        exit ;
                    end ;
                4 : // MQ (777304)
                    begin
                        if( Count = 1 ) then
                        begin
                            Value := Value or ( MQ and $FF00 ) ;
                        end ;
                        MQ := Value ;
                        exit ;
                    end ;
                5 : // MQ high byte (777305)
                    begin
                        Value := ( Value shl 8 ) or ( MQ and $FF ) ;
                        MQ := Value ;
                        exit ;
                    end ;
                6 : // Multiply (777306)
                    begin
                        if( Count = 1 ) then
                        begin
                            exit ;
                        end ;
                        ACMQ := smallint( MQ ) ;
                        ACMQ := ACMQ * Value ; // ACMQ = MQ * Value
                        SC := 16 ;
                        SR := SR and ( not 130 ) ; // 128 + 2
                        if( ( AC and $8000 ) = $8000 ) then
                        begin
                            SR := SR or 64 ;
                        end else
                        begin
                            SR := SR and ( not 64 ) ;
                        end ;
                    end ;
                8 : // SC (777310)
                    begin
                        if( Count = 2 ) then // Also set SR
                        begin
                            SR := Value shr 16 ;
                        end ;
                        SC := Value and 63 ;
                        exit ;
                    end ;
                11 : // SR (777311) - byte write to SR only supported in Deposit
                    begin
                        SR := Value ;
                        exit ;
                    end ;
                10 : // Normalize (777312)
                    begin
                        if( Count = 1 ) then
                        begin
                            exit ;
                        end ;
                        Temp := ACMQ ;
                        if( Temp = 0 ) then
                        begin
                            Normalize := 31 ;
                            SC := Normalize ;
                            exit ;
                        end ;
                        if( Temp = -1 ) then
                        begin
                            Normalize := 30 ;
                            SC := Normalize ;
                            ACMQ := Octal( '140000' ) shl 16 ;
                            exit ;
                        end ;
                        Normalize := 0 ;
                        if( Special_Form( Temp ) ) then
                        begin
                            while( longword( Temp ) <> $C0000000 ) do
                            begin
                                Temp := Temp shl 1 ;
                                inc( Normalize ) ;
                            end ;
                        end else
                        begin
                            while(
                                   ( ( Temp and $C0000000 ) = 0 )
                                   or
                                   ( ( Temp and $C0000000 ) = $C0000000 )
                                 ) do
                            begin
                                Temp := Temp shl 1 ;
                                inc( Normalize ) ;
                            end ;
                        end ;
                        SC := Normalize ;
                        SR := SR and ( not 130 ) ; // 128 + 2
                        if( ( AC and $8000 ) = $8000 ) then
                        begin
                            SR := SR or 64 ;
                        end else
                        begin
                            SR := SR and ( not 64 ) ;
                        end ;
                        ACMQ := Temp ;
                    end ;
                12 : // Logical shift (777314)
                    begin
                        if( Count = 1 ) then
                        begin
                            exit ;
                        end ;
                        if( Value > 31 ) then // Negative value
                        begin
                            Value := Value or $FFC0 ;
                        end ;
                        if( Value >= 0 ) then // Shift left
                        begin
                            Value := Value and 63 ;
                            SR := SR and ( not 129 ) ; // 128 + 1
                            while( Value > 1 ) do
                            begin
                                dec( Value ) ;
                                if( AC > 32767 ) then // Carry
                                begin
                                    SR := SR or 1 ;
                                end ;
                                if(
                                    ( ( AC and $8000 ) = 0 ) <> ( ( AC and $4000 ) = 0 )
                                  ) then // Bit 15 changed (Bit 15 <> bit 14)
                                begin
                                    SR := SR or 128 ; // Overflow
                                end ;
                                ACMQ := ACMQ shl 1 ;
                            end ;
                            ACMQ := ACMQ shl 1 ;
                        end else
                        begin
                            SR := SR and ( not 129 ) ;
                            Value := ( -Value ) and 63 ;
                            ACMQ := ACMQ shr ( Value - 1 ) ;
                            SR := ( SR and ( not 1 ) ) or ( MQ and 1 ) ;
                            ACMQ := ACMQ shr 1 ;
                        end ;
                        if(
                            ( AC = 0 ) and ( ( MQ and $8000 ) = 0 )
                            or
                            ( AC = 65535 ) and ( ( MQ and $8000 ) = $8000 )
                          ) then
                        begin
                            SR := SR or 2 ;
                        end else
                        begin
                            SR := SR and ( not 2 ) ;
                        end ;
                        if( ( AC and $8000 ) = $8000 ) then
                        begin
                            SR := SR or 64 ;
                        end else
                        begin
                            SR := SR and ( not 64 ) ;
                        end ;
                        SC := Value ;
                    end ;
                14 : // Arithmetic shift (777316)
                    begin
                        if( Count = 1 ) then
                        begin
                            exit ;
                        end ;
                        Temp_High := ACMQ and $80000000 ; // Save high bit
                        if( Value > 31 ) then // Negative value
                        begin
                            Value := Value or $FFC0 ;
                        end ;
                        if( Value >= 0 ) then // Shift left
                        begin
                            Value := Value and 63 ;
                            SC := Value ;
                            SR := SR and ( not 129 ) ; // 128 + 1
                            while( Value > 0 ) do
                            begin
                                dec( Value ) ;
                                if(
                                    ( ( AC and $8000 ) = 0 ) <> ( ( AC and $4000 ) = 0 )
                                  ) then // Bit 15 changed (Bit 15 <> bit 14)
                                begin
                                    SR := SR or 128 ; // Overflow
                                end ;
                                ACMQ := ACMQ shl 1 ;
                                if( ( AC and $8000 ) <> ( Value and $8000 ) ) then // Carry
                                begin
                                    SR := SR or 1 ;
                                end ;
                            end ;
                            ACMQ := ( ACMQ and $7FFFFFFF ) or Temp_High ; // Preserve high bit
                        end else
                        begin
                            Value := ( -Value ) and 63 ;
                            SC := Value ;
                            SR := SR and ( not 128 ) ;
                            while( Value > 0 ) do
                            begin
                                dec( Value ) ;
                                ACMQ := ACMQ shr 1 ;
                                ACMQ := ( ACMQ and $7FFFFFFF ) or Temp_High ; // Sign extend
                                SR := ( SR and ( not 1 ) ) or ( MQ and 1 ) ;
                            end ;
                        end ;
                        if(
                            ( AC = 0 ) and ( ( MQ and $8000 ) = 0 )
                            or
                            ( AC = 65535 ) and ( ( MQ and $8000 ) = $8000 )
                          ) then
                        begin
                            SR := SR or 2 ;
                        end else
                        begin
                            SR := SR and ( not 2 ) ;
                        end ;
                        if( ( AC and $8000 ) = $8000 ) then
                        begin
                            SR := SR or 64 ;
                        end else
                        begin
                            SR := SR and ( not 64 ) ;
                        end ;
                    end ;
                else exit ;
            end ; // case Address of

            // Set flags...
            if( ( AC = 0 ) and ( MQ = 0 ) ) then
            begin
                SR := SR or 4 ;
            end else
            begin
                SR := SR and ( not 4 ) ;
            end ;
            if( MQ = 0 ) then
            begin
                SR := SR or 8 ;
            end else
            begin
                SR := SR and ( not 8 ) ;
            end ;
            if( AC = 0 ) then
            begin
                SR := SR or 16 ;
            end else
            begin
                SR := SR and ( not 16 ) ;
            end ;
            if( AC = $FFFF ) then
            begin
                SR := SR or 32 ;
            end else
            begin
                SR := SR and ( not 32 ) ;
            end ;
        except
        end ;
    finally
        Deposit := Set_Error( CEFEAEErr_Success ) ;
    end ;
end ; { TEAE.Deposit }


function TEAE.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
        Disconnect_Input := Set_Error( CEFEAEErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := Set_Error( CEFEAEErr_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TEAE.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
        Disconnect_Output := Set_Error( CEFEAEErr_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := Set_Error( CEFEAEErr_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TEAE.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;
    Count, _Size : integer ;
    Value : integer ;

begin
    Result := nil ;
    if( ( not Memory ) or ( Address < Octal( '777300' ) ) or ( Address > Octal( '777317' ) ) or ( Size = 0 ) ) then
    begin
        Examine := Set_Error( CEFEAEErr_Address_Out_Of_Range ) ;
        exit ;
    end ;
    Count := ( Size + 7 ) div 8 ; { Number of bytes to examine }
    if( Count > 2 ) then
    begin
        while( Count > 0 ) do
        begin
            if( Count = 1 ) then
            begin
                _Size := 1 ;
            end else
            begin
                _Size := 2 ;
            end ;
            Examine( Address, _Size, Buffer, Memory ) ;
            Count := Count - 2 ;
            Address := Address + 2 ;
            Buffer := pointer( integer( Buffer ) + 2 ) ;
        end ;
        exit ;
    end ;
    fillchar( Buffer^, Count, 255 ) ;
    _Buffer := Buffer ;

    // Retrieve data...
    try
        case Address - Octal( '777300' ) of
            0 : // Divide (777300)
                begin
                    Value := 0 ;
                end ;
            2 : // AC (777302)
                begin
                    Value := AC ;
                    if( Count = 1 ) then
                    begin
                        Value := Value and $FF ;
                    end ;
                end ;
            3 : // AC high byte  (777303)
                begin
                    Value := AC shr 8 ;
                end ;
            4 : // MQ (777304)
                begin
                    Value := MQ ;
                    if( Count = 1 ) then
                    begin
                        Value := Value and $FF ;
                    end ;
                end ;
            5 : // MQ high byte (777305)
                begin
                    Value := MQ shr 8 ;
                end ;
            6 : // Multiply (777306)
                begin
                    Value := 0 ;
                end ;
            8 : // SC (777310)
                begin
                    Value := SC ;
                    if( Count = 2 ) then // Also set SR
                    begin
                        Value := Value or ( SC shl 8 ) ;
                    end ;
                end ;
            9 : // SR (777311) - byte write to SR only supported in Deposit
                begin
                    Value := SR ;
                end ;
            10 : // Normalize (777312)
                begin
                    Value := Normalize ;
                    if( Count = 1 ) then
                    begin
                        Value := Value and $FF ;
                    end ;
                end ;
            else // Logical shift (777314) and Arithmetic shift (777316) - and anything else
                begin
                    Value := 0 ;
                end ;
        end ; // Case Address of
        _Buffer[ 0 ] := char( Value ) ;
        if( Count > 1 ) then
        begin
            _Buffer[ 1 ] := char( Value shr 8 ) ;
        end ;
    except
    end ;
    Examine := Set_Error( CEFEAEErr_Success ) ;
end ; { TEAE.Examine }


function TEAE.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    if( ( not Memory ) and ( Address >= Octal( '777300' ) ) and ( Address <= Octal( '777317' ) ) ) then
    begin
        Get_Access_Mode := Access_Mode ;
    end else
    begin
        Get_Access_Mode := Access_None ;
    end ;
end ;


function TEAE.Get_Profiling : boolean ;

begin
    Get_Profiling := Profiling ;
end ;


procedure TEAE.Increment_Clock( Count : integer ) ;

begin
    try
        if( _UI.Clock <> nil ) then
        begin
            Blocked := True ;
            _UI.Clock.Block( self, Count ) ;
        end ;
    except
    end ;
end ;


function TEAE.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


function TEAE.Memory : TMemory ;

begin
    Result := _Memory ;
end ;


const _Name : PChar = 'DEC EAE'#0 ;

function TEAE.Name : PChar ;

begin
    Name := _Name ;
end ;


function TEAE.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


function TEAE.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

    procedure _Read( Address : int64 ; Size : longint ) ;

    var Buffer : word ;
        Loop : integer ;
        Watch : TCEF_Watchpoint ;

    begin
        if(
            ( Address >= Octal( '777300' ) )
            and
            ( Address <= Octal( '777317' ) )
          ) then
        begin
            Buffer := 0 ;
            Examine( Address, Size, PChar( @Buffer ), True ) ;
            if( Default_Output <> nil ) then
            begin
                for Loop := 0 to Outputs.Count - 1 do
                begin
                    TComponent( Outputs[ Loop ] ).Write_String( Address, PChar( @Buffer ), Size, IO_Type_Memory ) ;
                end ;
            end ;
            if( Default_Input <> nil ) then
            begin
                for Loop := 0 to Inputs.Count - 1 do
                begin
                    try
                        TComponent( Inputs[ Loop ] ).Write_String( Address, PChar( @Buffer ), Size, IO_Type_Memory ) ;
                    except
                    end ;
                end ;
            end ;
            Watch := Watchpoint_At( Address ) ;
            if( Watch <> nil ) then
            begin
                if( ( Watch.Access and Access_Read ) <> 0 ) then
                begin
                    _UI.Watchpoint_Notice( Address, Access_Read, 0, TComponent( self ), True, False, False ) ;
                end ;
            end ;
            if( Profiling ) then
            begin
                 //~~~
            end ;
        end ;
    end ; // TEAE.Read._Read

var Count : integer ;

begin
    Read := False ;
    if(
        ( Address >= Octal( '777300' ) )
        and
        ( Address <= Octal( '777317' ) )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Read ) = 0 ) then
        begin
            exit ;
        end ;
        if( Blocked ) then
        begin
            exit ;
        end ;
	Read := True ;
        Count := ( Size + 7 ) div 8 ;
        while( Count > 0 ) do
        begin
            if( Count = 1 ) then
            begin
                _Read( Address, 8 ) ;
            end else
            begin
                _Read( Address, 16 ) ;
            end ;
            Count := Count - 2 ;
            Address := Address + 2 ;
        end ;
    end ;
end ; // TEAE.Read


function TEAE.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

var Loop, High : int64 ;
    S : longint ;
    Value : byte ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    S := sizeof( Loop ) ;
    Stream.Read( Loop, S ) ;
    Stream.Read( High, S ) ;
    S := 1 ;
    while( Loop <= High ) do
    begin
	Stream.Read( Value, S ) ;
        if( S = 0 ) then
        begin
            exit ;
        end ;
	Deposit( Loop, 1, @Value, True ) ;
	Loop := Loop + 1 ;
    end ;
end ;


function TEAE.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var S : longint ;

begin
    Result := nil ;
    S := sizeof( Profiling ) ;
    Stream.Read( Profiling, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
end ;


function TEAE.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

var Loop : int64 ;
    High : int64 ;
    S : longint ;
    Value : integer ;

begin { TEAE.Save_Contents }
    fillchar( Result, sizeof( Result ), 0 ) ;

    { Write contents... }
    Loop := 0 ;
    Stream.Write( Loop, sizeof( Loop ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( High, sizeof( High ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    S := 1 ;
    while( Loop <= High ) do
    begin
        Examine( Loop, S, @Value, True ) ;
	    Stream.Write( Value, 1 ) ;
        Result := Stream.Last_Error ;
        if( Result <> nil ) then
        begin
            exit ;
        end ;
	    Loop := Loop + 1 ;
    end ;
end ; { TEAE.Save_Contents }


function TEAE.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Stream.Write( Profiling, sizeof( Profiling ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
end ;


function TEAE.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    if( ( not Memory ) and ( Octal( '777300' ) <= Low ) and ( Octal( '777317' ) >= High ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := Set_Error( CEFEAEErr_Success ) ;
end ;


procedure TEAE.Set_Profiling( _On, Children : boolean ) ;

begin
    Profiling := _On ;
end ;


function TEAE.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Watchpoint := Set_Error( CEFEAEErr_Success ) ;
    if( ( not Memory ) and ( Octal( '777300' ) <= Address ) and ( Octal( '777317' ) >= Address ) ) then
    begin
        Watchpoint_List.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TEAE.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TEAE.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

    procedure _Write( Address : int64 ; Size : longint ) ;

    var Watch : TCEF_Watchpoint ;

    begin
        if(
            ( Address >= Octal( '777300' ) )
            and
            ( Address <= Octal( '777317' ) )
          ) then
        begin
            if( Profiling ) then
            begin
                //~~~
            end ;
            Watch := Watchpoint_At( Address ) ;
            if( Watch <> nil ) then
            begin
                if( ( Watch.Access and Access_Write ) <> 0 ) then
                begin
                    _UI.Watchpoint_Notice( Address, Access_Write, 0, TComponent( self ), True, False, False ) ;
                end ;
            end ;
            if( Address = Octal( '777311' ) ) then // Byte write to SR does nothing
            begin
                exit ;
            end ;
            if( ( Address = Octal( '777310' ) ) and ( Size = 2 ) ) then // SC/SR
            begin
                Value := Value and ( not $3E00 ) or ( ( SR shl 8 ) and $3E00 ) ; // Preserve R/O bits
            end ;
            Write := Deposit( Address, Size, @Value, True ) ;
            case Address - Octal( '777300' ) of
                0 : // Divide (777300)
                    begin
                        Increment_Clock( 4250 ) ;
                    end ;
                2 : // AC
                    begin
                        if( Size = 8 ) then // Low byte
                        begin
                            // Sign-extend AC
                            if( ( AC and 128 ) = 128 ) then
                            begin
                                AC := AC or $FF00 ;
                            end else
                            begin
                                AC := AC and $FF ;
                            end ;
                        end ;
                    end ;
                4 : // MQ
                    begin
                        if( Size = 8 ) then // Low byte
                        begin
                            // Sign-extend MQ
                            if( ( MQ and 128 ) = 128 ) then
                            begin
                                MQ := MQ or $FF00 ;
                            end else
                            begin
                                MQ := MQ and $FF ;
                            end ;
                        end ;

                        // Sign-extend AC
                        if( ( MQ and 32768 ) = 32768 ) then
                        begin
                            AC := $FFFF ;
                        end else
                        begin
                            AC := 0 ;
                        end ;
                    end ;
                5 : // MQ (high)
                    begin
                        // Sign-extend AC
                        if( ( MQ and 32768 ) = 32768 ) then
                        begin
                            AC := $FFFF ;
                        end else
                        begin
                            AC := 0 ;
                        end ;
                    end ;
                6 : // Multiply (777306)
                    begin
                        Increment_Clock( 4000 ) ;
                    end ;
                10 : // Normalize (777312)
                    begin
                        Increment_Clock( 400 ) ;
                    end ;
                12 : // Logical shift (777314)
                    begin
                        Increment_Clock( 400 ) ;
                    end ;
                14 : // Arithmetic shift (777316)
                    begin
                        Increment_Clock( 400 ) ;
                    end ;
            end ; // case Address - Octal( '777300' )
        end ; // if
    end ; // .Write

var A : int64 ;
    B, Count : integer ;
    S : string ;

begin
    if(
        ( Address >= Octal( '777300' ) )
        and
        ( Address <= Octal( '777317' ) )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Write ) = 0 ) then
        begin
            Write := Set_Error( CEFEAEErr_Access_Violation ) ;
            exit ;
        end ;
        if( _Logger <> nil ) then
        begin
            A := Address ;
            B := _Logger.Data_Radix ;
            if( ( B < 2 ) or ( B > 36 ) ) then
            begin
                B := 10 ;
            end ;
            S := '' ;
        end ;
        if( Blocked ) then
        begin
            Write := Set_Error( CEFEAEErr_Busy ) ;
            exit ;
        end ;
        Count := ( Size + 7 ) div 8 ;
        while( Count > 0 ) do
        begin
            if( Count = 1 ) then
            begin
                _Write( Address, 8 ) ;
                if( _Logger <> nil ) then
                begin
                    S := ' ' + cvtb( 10, B, inttostr( Value and 255 ) ) ;
                end ;
            end else
            begin
                _Write( Address, 16 ) ;
                if( _Logger <> nil ) then
                begin
                    S := ' ' + cvtb( 10, B, inttostr( Value and 65535 ) ) ;
                end ;
            end ;
            Count := Count - 2 ;
            Address := Address - 2 ;
        end ;
        if( _Logger <> nil ) then
        begin
            _Logger.Log( self, PChar( S +
                ' (' + inttostr( Size ) + ')' + ' @' +
                cvtb( 10, B, inttostr( A ) ) ), -1, False, LT_Write ) ;
            _Logger.Log( self, PChar( 'AC=' + cvtb( 10, B, inttostr( AC ) ) + 'MQ=' +
                cvtb( 10, B, inttostr( MQ ) ) ), -1, True, LT_Execution ) ;
        end ;
    end else
    begin
	    Write := Set_Error( CEFEAEErr_Success ) ;
    end ;
end ;


procedure TEAE.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TEAE.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TEAE.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TEAE.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


procedure TEAE.Wake ;

begin
    Blocked := False ;
end ;


function TEAE.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TEAE.Set_Logger( Value : TCEF_Logger ) ;

begin
    if( _Logger <> nil ) then
    begin
        _Logger.Detach ;
    end ;
    _Logger := Value ;
    if( Value <> nil ) then
    begin
        _Logger.Attach ;
    end ;
end ;



{$IFDEF Test}

type TTest_UI = class( TUI_Interface )
                   private
                       Profile_Address : int64 ;
                       Profile_Access : integer ;
                       Watch_Address : int64 ;
                       Watch_Access : integer ;

                   public
                       Profile_Triggered : boolean ;
                       Watch_Triggered : boolean ;

                   public
                       procedure Block( Component : TComponent ; Blocked : boolean ) ;
                           override ;

                       function Breakpoint_Notice( Address : int64 ; Physical : boolean ;
                           CPU : TCPU ) : boolean ; override ;

                       function Clock : TMaster_Clock ; override ;

                       procedure Log_Error( Text : PChar ; Severity : longint ) ;
                           override ;

                       procedure Log_Simulated_Error( Text : PChar ;
                           Severity : longint ) ; override ;

                       procedure Log_Status( Text : PChar ; Index : longint ) ;
                           override ;

                       procedure Profile_Notice( Address : int64 ; Access : longint ;
                           Tag, Time : longint ; Component : TComponent ) ; override ;

                       function Version : integer ; override ;

                       procedure Watchpoint_Notice( Address : int64 ;
                           Access, Tag : longint ; Component : TComponent ) ;
                           override ;


                       procedure Allow_Profile( Address : int64 ; Access : integer ) ;

                       procedure Allow_Watch( Address : int64 ; Access : integer ) ;
                end ;

    TTest_Output_Component = class( TComponent )
                              public
                                Enabled : boolean ;
                                _Address : int64 ;
                                _Value : longint ;
                                _Size : longint ;

                              public
                                function Facility_Code : longint ; override ;

                                function Initialize( UI : TUI_Interface ) : TUnified_Exception ;  override ;

                                function Terminate : TUnified_Exception ; override ;

                                function Child_Component( Index : longint ) : TComponent ; override ;

                                function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                function Component_Type : longint ; override ;

                                function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                                function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Disconnect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Disconnect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                                function Examine( Address : int64 ; var Size : longint ;
                                    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

                                function Get_Access_Mode( Address : int64 ;
                                    Memory : boolean ) : longint ; override ;

                                function Get_Profiling : boolean ; override ;

                                function Get_Read_Latency : longint ; override ;

                                function Get_Write_Latency : longint ; override ;

                                function Input_Component( Index : longint ) : TComponent ; override ;

                                function Name : PChar ; override ;

                                function Output_Component( Index : longint ) : TComponent ; override ;

                                function Read( Address : int64 ; Size : longint ;
                                    Memory : boolean ) : boolean ; override ;

                                function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Set_Access_Mode( Low, High : int64 ;
                                    Memory : boolean ; Typ : longint ) : TUnified_Exception ;
                                    override ;

                                procedure Set_Profiling( _On, Children : boolean ) ; override ;

                                procedure Set_Read_Latency( Value : longint ) ; override ;

                                function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                procedure Set_Write_Latency( Value : longint ) ;  override ;

                                procedure Show_Status ; override ;

                                function Version : integer ; override ;

                                procedure Wake ; override ;

                                function Write( Address : int64 ; Value, Size : longint ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; Memory : boolean ) : TUnified_Exception ; override ;
                             end ;

    TTest_Input_Component = class( TComponent )
                              public
                                Enabled : boolean ;
                                _Address : int64 ;
                                _Value : longint ;
                                _Size : longint ;

                              public
                                function Facility_Code : longint ; override ;

                                function Initialize( UI : TUI_Interface ) : TUnified_Exception ;  override ;

                                function Terminate : TUnified_Exception ; override ;

                                function Child_Component( Index : longint ) : TComponent ; override ;

                                function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                function Component_Type : longint ; override ;

                                function Connect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Connect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                                function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Disconnect_Input( Component : TComponent ) : TUnified_Exception ; override ;

                                function Disconnect_Output( Component : TComponent ) : TUnified_Exception ; override ;

                                function Examine( Address : int64 ; var Size : longint ;
                                    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ;

                                function Get_Access_Mode( Address : int64 ;
                                    Memory : boolean ) : longint ; override ;

                                function Get_Profiling : boolean ; override ;

                                function Get_Read_Latency : longint ; override ;

                                function Get_Write_Latency : longint ; override ;

                                function Input_Component( Index : longint ) : TComponent ; override ;

                                function Name : PChar ; override ;

                                function Output_Component( Index : longint ) : TComponent ; override ;

                                function Read( Address : int64 ; Size : longint ;
                                    Memory : boolean ) : boolean ; override ;

                                function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ; override ;

                                function Set_Access_Mode( Low, High : int64 ;
                                    Memory : boolean ; Typ : longint ) : TUnified_Exception ;
                                    override ;

                                procedure Set_Profiling( _On, Children : boolean ) ; override ;

                                procedure Set_Read_Latency( Value : longint ) ; override ;

                                function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                                    Access : longint ) : TUnified_Exception ; override ;

                                procedure Set_Write_Latency( Value : longint ) ;  override ;

                                procedure Show_Status ; override ;

                                function Version : integer ; override ;

                                procedure Wake ; override ;

                                function Write( Address : int64 ; Value, Size : longint ;
                                    Memory : boolean ) : TUnified_Exception ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; Memory : boolean ) : TUnified_Exception ; override ;
                             end ;

procedure TTest_UI.Block( Component : TComponent ; Blocked : boolean ) ;

begin
end ;


function TTest_UI.Breakpoint_Notice( Address : int64 ; Physical : boolean ;
   CPU : TCPU ) : boolean ;

begin
    ShowMessage( 'Breakpoint notice' ) ;
end ;


function TTest_UI.Clock : TMaster_Clock ;

begin
    Result := Test_Clock ;
end ;


procedure TTest_UI.Log_Error( Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TTest_UI.Log_Simulated_Error( Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TTest_UI.Log_Status( Text : PChar ; Index : longint ) ;

begin
end ;


procedure TTest_UI.Profile_Notice( Address : int64 ; Access : longint ;
   Tag, Time : longint ; Component : TComponent ) ;

begin
    if( ( Address <> Profile_Address ) or ( Access <> Profile_Access ) ) then
    begin
        ShowMessage( 'Invalid profile notice' ) ;
    end ;
    Profile_Triggered := True ;
end ;


function TTest_UI.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_UI.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
   Component : TComponent ) ;

begin
    if( ( Address <> Watch_Address ) or ( Access <> Watch_Access ) ) then
    begin
        ShowMessage( 'Invalid watchpoint notice' ) ;
    end ;
    Watch_Triggered := True ;
end ;


procedure TTest_UI.Allow_Profile( Address : int64 ; Access : integer ) ;

begin
    Profile_Address := Address ;
    Profile_Access := Access ;
end ;


procedure TTest_UI.Allow_Watch( Address : int64 ; Access : integer ) ;

begin
    Watch_Address := Address ;
    Watch_Access := Access ;
end ;


function TTest_Output_Component.Facility_Code : longint ;

begin
    Result := 0 ;
end ;


function TTest_Output_Component.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Terminate : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Child_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Child_Component' ) ;
end ;


function TTest_Output_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Clear_Watch' ) ;
end ;


function TTest_Output_Component.Component_Type : longint ;

begin
    ShowMessage( 'Call to component.Component_Type' ) ;
end ;


function TTest_Output_Component.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Input' ) ;
end ;


function TTest_Output_Component.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Output' ) ;
end ;


function TTest_Output_Component.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Deposit' ) ;
end ;


function TTest_Output_Component.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Input' ) ;
end ;


function TTest_Output_Component.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Output' ) ;
end ;


function TTest_Output_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Examine' ) ;
end ;


function TTest_Output_Component.Get_Access_Mode( Address : int64 ;
    Memory : boolean ) : longint ;

begin
    ShowMessage( 'Call to component.Get_Access_Mode' ) ;
end ;


function TTest_Output_Component.Get_Profiling : boolean ;

begin
    ShowMessage( 'Call to component.Get_Profiling' ) ;
end ;


function TTest_Output_Component.Get_Read_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Read_Latency' ) ;
end ;


function TTest_Output_Component.Get_Write_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Write_Latency' ) ;
end ;


function TTest_Output_Component.Input_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Input_Component' ) ;
end ;


function TTest_Output_Component.Name : PChar ;

begin
    ShowMessage( 'Call to component.Name' ) ;
end ;


function TTest_Output_Component.Output_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Output_Component' ) ;
end ;


function TTest_Output_Component.Read( Address : int64 ; Size : longint ;
    Memory : boolean ) : boolean ;

begin
end ;


function TTest_Output_Component.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Output_Component.Set_Access_Mode( Low, High : int64 ;
    Memory : boolean ; Typ : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Access_Mode' ) ;
end ;


procedure TTest_Output_Component.Set_Profiling( _On, Children : boolean ) ;

begin
    ShowMessage( 'Call to component.Set_Profiling' ) ;
end ;


procedure TTest_Output_Component.Set_Read_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Read_Latency' ) ;
end ;


function TTest_Output_Component.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Watchpoint' ) ;
end ;


procedure TTest_Output_Component.Set_Write_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Write_Latency' ) ;
end ;


procedure TTest_Output_Component.Show_Status ;

begin
    ShowMessage( 'Call to component.Show_Status' ) ;
end ;


function TTest_Output_Component.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_Output_Component.Wake ;

begin
    ShowMessage( 'Call to component.Wake' ) ;
end ;


function TTest_Output_Component.Write( Address : int64 ; Value, Size : longint ;
    Memory : boolean ) : TUnified_Exception ;

begin
    if( Enabled and Memory ) then
    begin
        _Address := Address ;
        _Size := Size ;
        _Value := Value ;
    end else
    begin
        ShowMessage( 'Unexpected Write to output component' ) ;
    end ;
end ;


function TTest_Output_Component.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; Memory : boolean ) : TUnified_Exception ;

begin
    Write( Address, ord( Value[ 0 ] ), Size, Memory ) ;
end ;


function TTest_Input_Component.Facility_Code : longint ;

begin
    Result := 0 ;
end ;



function TTest_Input_Component.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Terminate : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Child_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Child_Component' ) ;
end ;


function TTest_Input_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Clear_Watch' ) ;
end ;


function TTest_Input_Component.Component_Type : longint ;

begin
    ShowMessage( 'Call to component.Component_Type' ) ;
end ;


function TTest_Input_Component.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Input' ) ;
end ;


function TTest_Input_Component.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Connect_Output' ) ;
end ;


function TTest_Input_Component.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Deposit' ) ;
end ;


function TTest_Input_Component.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Input' ) ;
end ;


function TTest_Input_Component.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Disconnect_Output' ) ;
end ;


function TTest_Input_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Examine' ) ;
end ;


function TTest_Input_Component.Get_Access_Mode( Address : int64 ;
    Memory : boolean ) : longint ;

begin
    ShowMessage( 'Call to component.Get_Access_Mode' ) ;
end ;


function TTest_Input_Component.Get_Profiling : boolean ;

begin
    ShowMessage( 'Call to component.Get_Profiling' ) ;
end ;


function TTest_Input_Component.Get_Read_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Read_Latency' ) ;
end ;


function TTest_Input_Component.Get_Write_Latency : longint ;

begin
    ShowMessage( 'Call to component.Get_Write_Latency' ) ;
end ;


function TTest_Input_Component.Input_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Input_Component' ) ;
end ;


function TTest_Input_Component.Name : PChar ;

begin
    ShowMessage( 'Call to component.Name' ) ;
end ;


function TTest_Input_Component.Output_Component( Index : longint ) : TComponent ;

begin
    ShowMessage( 'Call to component.Output_Component' ) ;
end ;


function TTest_Input_Component.Read( Address : int64 ; Size : longint ;
    Memory : boolean ) : boolean ;

begin
end ;


function TTest_Input_Component.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
end ;


function TTest_Input_Component.Set_Access_Mode( Low, High : int64 ;
    Memory : boolean ; Typ : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Access_Mode' ) ;
end ;


procedure TTest_Input_Component.Set_Profiling( _On, Children : boolean ) ;

begin
    ShowMessage( 'Call to component.Set_Profiling' ) ;
end ;


procedure TTest_Input_Component.Set_Read_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Read_Latency' ) ;
end ;


function TTest_Input_Component.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Set_Watchpoint' ) ;
end ;


procedure TTest_Input_Component.Set_Write_Latency( Value : longint ) ;

begin
    ShowMessage( 'Call to component.Set_Write_Latency' ) ;
end ;


procedure TTest_Input_Component.Show_Status ;

begin
    ShowMessage( 'Call to component.Show_Status' ) ;
end ;


function TTest_Input_Component.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_Input_Component.Wake ;

begin
    ShowMessage( 'Call to component.Wake' ) ;
end ;


function TTest_Input_Component.Write( Address : int64 ; Value, Size : longint ;
    Memory : boolean ) : TUnified_Exception ;

begin
    if( Enabled and Memory ) then
    begin
        _Address := Address ;
        _Size := Size ;
        _Value := Value ;
    end else
    begin
        ShowMessage( 'Unexpected Write to output component' ) ;
    end ;
end ;


function TTest_Input_Component.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; Memory : boolean ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Write_String' ) ;
end ;


type TTest_Streamer = class( TCOM_Stream )
        public
            S : TMemoryStream ;

            constructor Create ;
            function At_End : boolean ; override ;
            function Facility_Code : longint ; override ;
            function Read( var Buffer ; var Size : longint ) : TUnified_Exception ;
                override ;
            function Read_Line( var Buffer ; var Size : longint ) : TUnified_Exception ;
                override ;
            function Seek( Position : longint ) : TUnified_Exception ; override ;
            function Write( var Buffer ; size : longint ) : TUnified_Exception ; override ;
            function Write_Line( Buffer : PChar ) : TUnified_Exception ; override ;
      end ;

constructor TTest_Streamer.Create ;

begin
    inherited Create ;

    S := TMemoryStream.Create ;
end ;


function TTest_Streamer.At_End : boolean ;

begin
    Result := ( S.Position >= S.Size ) ;
end ;


function TTest_Streamer.Facility_Code : longint ;

begin
    Result := -1 ;
end ;


function TTest_Streamer.Read( var Buffer ; var Size : longint ) : TUnified_Exception ;

begin
    S.ReadBuffer( Buffer, Size ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Read_Line( var Buffer ; var Size : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to Read_Line' ) ;
end ;


function TTest_Streamer.Seek( Position : longint ) : TUnified_Exception ;

begin
    S.Seek( 0, soFromBeginning ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Write( var Buffer ; size : longint ) : TUnified_Exception ;

begin
    S.WriteBuffer( Buffer, Size ) ;
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TTest_Streamer.Write_Line( Buffer : PChar ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to Write_Line' ) ;
end ;


procedure Test_Unit ;

var Address, Count, Temp : int64 ;
    E : TUnified_Exception ;
    Test : TEAE ;
    Test_UI : TTest_UI ;
    Test_Output_Component : TTest_Output_Component ;
    Test_Input_Component : TTest_Input_Component ;
    Test_Stream : TTest_Streamer ;

    procedure Check_E ;

    begin
        if( E.Code <> 0 ) then
        begin
            ShowMessage( 'Error: ' + inttostr( E.Code ) ) ;
        end ;
    end ;

var Dummy, Size, Work : integer ;

begin
    { Coverage test... }

    { Setup... }
    Test := TEAE.Create ;
    Test_UI := TTest_UI.Create ;
    Test_UI.Allow_Profile( -1, 0 ) ;
    Test_Output_Component := TTest_Output_Component.Create ;
    Test_Output_Component.Enabled := False ;
    Test_Input_Component := TTest_Input_Component.Create ;
    Test_Input_Component.Enabled := False ;
    Test_Stream := TTest_Streamer.Create ;
    E := Test.Initialize( Test_UI ) ;
    Check_E ;

    { Check for invalid conditions... }
    if( Test.Output_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Input_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;
    E := Test.Connect_Output( Test_Output_Component ) ;
    Check_E ;
    E := Test.Connect_Input( Test_Input_Component ) ;
    Check_E ;
    E := Test.Deposit( 0, 8, @Dummy, False ) ;
    Check_E ;
    Size := 8 ;
    E := Test.Examine( 0, Size, @Work, False ) ;
    if( E.COde = 0 ) then
    begin
        ShowMessage( 'No error on non-memory' ) ;
    end ;
    Work := 0 ;
    E := Test.Examine( 0, Size, @Work, True ) ;
    Check_E ;
    if( Work <> 255 ) then
    begin
        ShowMessage( 'Invalid examine address' ) ;
    end ;
    Test.Save_Contents( Test_Stream ) ;
    Test_Stream.Seek( 0 ) ;

    { Test reads/writes... }
    for Dummy := 0 to 255 do
    begin
        E := Test.Deposit( Dummy * 2, 8, @Dummy, True ) ;
        Check_E ;
    end ;
    Size := 8 ;
    E := Test.Examine( 1024, Size, @Work, False ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on accessing non-memory' ) ;
    end ;
    for Dummy := 0 to 255 do
    begin
        Size := 8 ;
        Work := 0 ;
        E := Test.Examine( Dummy * 2, Size, @Work, True ) ;
        Check_E ;
        if( Size <> 8 ) then
        begin
            ShowMessage( 'Illegal size returned from Examine' ) ;
        end ;
        if( Work <> Dummy ) then
        begin
            ShowMessage( 'Examine doesn''t match deposit' ) ;
        end ;
    end ;

    { Misc other tests... }
    if( Test.Get_Access_Mode( 0, False ) <> Access_None ) then
    begin
        ShowMessage( 'Invalid access mode returned' ) ;
    end ;
    if( Test.Name <> 'CEF Generic memory' ) then
    begin
        ShowMessage( 'Invalid name returned' ) ;
    end ;
    E := Test.Examine( 1024, Size, @Work, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on accessing memory out of range' ) ;
    end ;
    if( Test.Get_Profiling = True ) then
    begin
        ShowMessage( 'Bad profiling return' ) ;
    end ;
    Test.Set_Profiling( True, True ) ;
    if( Test.Get_Profiling <> True ) then
    begin
        ShowMessage( 'Bad profiling return' ) ;
    end ;
    if( Test.Get_Read_Latency <> 0 ) then
    begin
        ShowMessage( 'Invalid default read latency' ) ;
    end ;
    if( Test.Get_Write_Latency <> 0 ) then
    begin
        ShowMessage( 'Invalid default write latency' ) ;
    end ;
    Test.Set_Read_Latency( 12 ) ;
    if( Test.Get_Read_Latency <> 12 ) then
    begin
        ShowMessage( 'Read latency did not change' ) ;
    end ;
    Test.Set_Write_Latency( 34 ) ;
    if( Test.Get_Write_Latency <> 34 ) then
    begin
        ShowMessage( 'Write latency did not change' ) ;
    end ;
    if( Test.Get_Access_Mode( 0, True ) <> Access_All ) then
    begin
        ShowMessage( 'Invalid default access mode' ) ;
    end ;

    { Test access failures... }
    E := Test.Set_Access_Mode( 0, 255, False, Access_None ) ;
    Check_E ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_None ) ;
    Check_E ;
    Size := 8 ;
    E := Test.Examine( 0, Size, @Work, True ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, False ) ; { Non memory write }
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ; { Memory write }
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Read ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Write ) ;
    if( Test.Read( 0, 8, True ) ) then
    begin
        ShowMessage( 'Read with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Execute ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;

    { Test I/O components... }
    E := Test.Set_Access_Mode( 0, 255, True, Access_RW ) ;
    Check_E ;
    E := Test.Connect_Input( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on nil input connection' ) ;
    end ;
    E := Test.Connect_Output( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on nil output connection' ) ;
    end ;
    Test_UI.Allow_Profile( 1, Access_Write ) ;
    Test_Output_Component.Enabled := True ;
    Test_Input_Component.Enabled := True ;
    if( Test.Output_Component( 0 ) <> Test_Output_Component ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    E := Test.Write( 1, 1, 8, True ) ;
    if( E.Code <> 0 ) then
    begin
        ShowMessage( 'Error on write' ) ;
    end ;
    if( not Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile not triggered' ) ;
    end ;
    if( Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watch incorrectly triggered' ) ;
    end ;
    if( Test.Read( 1, 8, False ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    Test_UI.Allow_Profile( 1, Access_Read ) ;
    if( not Test.Read( 1, 8, True ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    if(
        ( Test_Output_Component._Address <> 1 )
        or
        ( Test_Output_Component._Size <> 8 )
        or
        ( Test_Output_Component._Value <> 1 )
      ) then
    begin
        ShowMessage( 'Read failure' ) ;
    end ;
    Test_UI.Allow_Profile( -1, 0 ) ;
    Test.Set_Profiling( False, False ) ;
    Test_UI.Profile_Triggered := False ;

    { Test watchpoints... }
    E := Test.Set_Watchpoint( 0, False, Access_Write ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 0, True, Access_Read ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 0, True, Access_Write ) ;
    Check_E ;
    E := Test.Set_Watchpoint( 1, True, Access_Read ) ;
    Check_E ;
    Test_UI.Allow_Watch( 0, Access_Write ) ;
    E := Test.Write( 0, 1, 8, True ) ;
    if( E.Code <> 0 ) then
    begin
        ShowMessage( 'Error on write' ) ;
    end ;
    if( not Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watchpoint not triggered' ) ;
    end ;
    if( Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile incorrectly triggered' ) ;
    end ;
    Test_UI.Watch_Triggered := False ;
    Test_UI.Allow_Watch( 1, Access_Read ) ;
    if( not Test.Read( 1, 8, True ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    if( not Test_UI.Watch_Triggered ) then
    begin
        ShowMessage( 'Watchpoint not triggered' ) ;
    end ;
    if( Test_UI.Profile_Triggered ) then
    begin
        ShowMessage( 'Profile incorrectly triggered' ) ;
    end ;
    E := Test.Clear_Watchpoint( 1, False, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 0, True, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 0, True, Access_Write ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 1, True, Access_Read ) ;
    Check_E ;
    E := Test.Clear_Watchpoint( 1, True, Access_Write ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from clearing non-existant watchpoint' ) ;
    end ;

    { Test disconnections... }
    E := Test.Disconnect_Output( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from Disconnecting nil output' ) ;
    end ;
    E := Test.Disconnect_Input( nil ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Incorrect result from Disconnecting nil input' ) ;
    end ;
    E := Test.Disconnect_Output( Test_Output_Component ) ;
    Check_E ;
    E := Test.Disconnect_Input( Test_Input_Component ) ;
    Check_E ;
    if( Test.Output_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Output_Component( 1 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Output_Component result' ) ;
    end ;
    if( Test.Input_Component( 0 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;
    if( Test.Input_Component( 1 ) <> nil ) then
    begin
        ShowMessage( 'Invalid Input_Component result' ) ;
    end ;

    { Test save/restore contents... }
    for Dummy := 0 to 255 do
    begin
        Work := Dummy div 2 ;
        E := Test.Deposit( Dummy, 8, @Work, True ) ;
        Check_E ;
    end ;
    E := Test.Save_Contents( Test_Stream ) ;
    Check_E ;
    Work := 255 ;
    for Dummy := 0 to 255 do
    begin
        E := Test.Deposit( Dummy, 8, @Work, True ) ;
        Check_E ;
    end ;
    Test_Stream.Seek( 0 ) ;
    E := Test.Restore_Contents( Test_Stream ) ;
    Check_E ;
    Size := 8 ;
    for Dummy := 0 to 255 do
    begin
        Work := 0 ;
        E := Test.Examine( Dummy, Size, @Work, True ) ;
        Check_E ;
        if( Work <> Dummy div 2 ) then
        begin
            ShowMessage( 'Save/Restore failure' ) ;
        end ;
    end ;

    { Test save/restore state... }
    E := Test_Stream.Seek( 0 ) ;
    Check_E ;
    Test.Save_State( Test_Stream ) ;
    Test.Set_Read_Latency( 12345 ) ;
    Test.Set_Write_Latency( 6789 ) ;
    Test.Set_Profiling( True, True ) ;
    Test_Stream.Seek( 0 ) ;
    E := Test.Restore_State( Test_Stream ) ;
    Check_E ;
    if( ( Test._Low <> 0 ) or ( Test._High <> 255 ) ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;
    if( Test.Get_Profiling ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;
    if( ( Test.Get_Read_Latency <> 12 ) or ( Test.Get_Write_Latency <> 34 ) ) then
    begin
        ShowMessage( 'Save/Restore failed' ) ;
    end ;

    { Test termination... }
    E := Test.Terminate ;
    Check_E ;

    { Cleanup... }
    E := Test.Terminate ;
    Check_E ;
end ;
{$ENDIF}


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

