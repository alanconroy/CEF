{$N+}
{
        Program Name : Memory
        Package Name : CEF
        Purpose      : Generic CEF Memory
        Institution  : 
        Date Written : 28-Apr-2000
        Written By   : Alan Conroy
        Version      : 1.1

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

          9-June-2002   EAC         Don't require clock component.
          10-Feb-2015   EAC         Expand to 64-bit addressing.

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	    This unit implements a generic memory component for CEF, for Borland
        Pascal and FPK.

	    The memory is infinitely fast (as far as the Emulator is concerned),
        single-port, static RAM, with a maximum address size of 63 bits.


        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan


        Conditionals:
            TEST    Causes test to run upon unit initialization.
}

unit Memory ;

interface

{$I EDEFINES.INC}

uses // C&C...
{$IFDEF DELPHI}
     Compatib,
  {$IFDEF Test}
     Classes,
  {$ENDIF}
{$ENDIF}
     Collect, { TCollection }
     _DebugIn, // TDebug_Interface
     DebugInt, // TText_Debugger
     _Streams, // TCOM_Stream
     _UE, // TUnified_Exception

     // CEF...
     _CEF, { TMemory }
     CEF, // TBase_Memory
     _CEFUtil ; // TCEF_Watchpoint

const CEFMemory_Facility = 16 ;
      CEFMemoryErr_Success = 0 ;
      CEFMemoryErr_Invalid_Range = 1 ;
      CEFMemoryErr_Component_Not_Found = 2 ;
      CEFMemoryErr_No_Matching_Watchpoint = 3 ;
      CEFMemoryErr_Access_Violation = 4 ;
      CEFMemoryErr_Address_Out_Of_Range = 5 ;
      CEFMemoryErr_Invalid_Component = 6 ;
      CEFMemoryErr_Memory_Exhausted = 7 ; { Couldn't allocate any more memory }

type TPointer_Block = array[ 0..255 ] of pointer ;
     PPointer_Block = ^TPointer_Block ;
     TBlock = array[ 0..255 ] of byte ;
     PBlock = ^TBlock ;

type TCEF_Memory_Memory = class ;
     TCEF_Memory = class( TBase_Component )
                       private // Instance data...
                           _Memory : TCEF_Memory_Memory ;
                           Access_Mode : integer ;
                           Default_Input, Default_Output : TComponent ;
                           _Parent : TComponent ;
                           _Tag : longint ;
                           Read_Latency, Write_Latency : longint ;
                           _Logger : TCEF_Logger ;

                       public { Public instance data... }
                           _Serial_Number : integer ;

                       public // API...
                           function Memory : TMemory ; override ;

                           function Facility_Code : longint ; override ;

                           function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                               override ;

                           function Terminate : TUnified_Exception ; override ;

                           function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                               Access : longint ) : TUnified_Exception ; override ;

                           function Component_Type : longint ; override ;

                           function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                               override ;

                           function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                               override ;

                           function Debugger : TDebug_Interface ; override ;

                           function Deposit( Address : int64 ; Size : longint ;
                               Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;
                               override ;

                           function Disconnect_Input( Component : TComponent ) : TUnified_Exception ;
                               override ;

                           function Disconnect_Output( Component : TComponent ) : TUnified_Exception ;
                               override ;

                           function Examine( Address : int64 ; var Size : longint ;
                               Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;
                               override ;

                           function Get_Access_Mode( Address : int64 ;
                               Memory : boolean ) : longint ; override ;

                           function Get_Profiling : boolean ; override ;

                           function Get_Read_Latency : longint ; override ;

                           function Get_Write_Latency : longint ; override ;

                           function Input_Component( Index : longint ) : TComponent ;
                               override ;

                           function Name : PChar ; override ;

                           function Output_Component( Index : longint ) : TComponent ;
                               override ;

                           function Read( Address : int64 ; Size : longint ;
                               IO_Type : longint ) : boolean ; override ;

                           function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                               override ;

                           function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                               override ;

                           function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                               override ;

                           function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                               override ;

                           function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                               Typ : longint ) : TUnified_Exception ; override ;

                           procedure Set_Profiling( _On, Children : boolean ) ;
                               override ;

                           procedure Set_Read_Latency( Value : longint ) ;
                               override ;

                           function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                               Access : longint ) : TUnified_Exception ; override ;

                           procedure Set_Write_Latency( Value : longint ) ;
                               override ;

                           procedure Show_Status ; override ;

                           function Write( Address : int64 ; Value, Size : longint ;
                               IO_Type : longint ) : TUnified_Exception ; override ;

                           procedure Set_Tag( Value : longint ) ; override ;

                           function Get_Tag : longint ; override ;

                           function Get_Parent : TComponent ; override ;

                           procedure Set_Parent( Component : TComponent ) ;
                               override ;

                          function Get_Logger : TCEF_Logger ; override ;

                          procedure Set_Logger( Value : TCEF_Logger ) ; override ;
                   end ; // TCEF_Memory

     TCEF_Memory_Memory = class( TBase_Memory )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private { Instance data... }
            _Low, _High : int64 ; { Current memory range }
            Physical_Memory : PPointer_Block ; { Allocated memory blocks for low memory }
            Physical_Memory_Hi : PPointer_Block ; { Allocated memory blocks for high memory}
            Profiling : boolean ; { True if profiling memory accesses }
            _UI : TUI_Interface ;
            Watchpoint_List : TCEF_Watchpoint_Manager ;
            Parent : TCEF_Memory ;
            _Logger : TCEF_Logger ;

        public // Property handlers...
            function Translate_Error( Code : longint ) : string ; override ;

        private // Internal utility routines...
            procedure Clear_Memory ;
            function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

        public { API... }
            function Facility_Code : longint ; override ;
            function Set_Address_Range( Low, High : int64 ) : TUnified_Exception ; override ;

            procedure Get_Address_Range( var Low, High : int64 ) ; override ;

            procedure Dump( Start, Size : int64 ; Buffer : pointer ) ; override ;
            procedure Load( Start, Size : int64 ; Buffer : pointer ) ; override ;
            function Map_Virtual_To_Physical( Virt : int64 ) : int64 ; override ;
     end ; // TCEF_Memory_Memory

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
{$IFDEF DELPHI}
  {$IFDEF Test}
     Dialogs,
  {$ENDIF}
{$ENDIF}
     Sysutils,

     // C&C...
     cvt, // cvtb
     Num1s,
     UE ; // Create_Simple_UE

function TCEF_Memory_Memory.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        CEFMemoryErr_Success: _Error := 'Success' ;
        CEFMemoryErr_Invalid_Range: _Error := 'Invalid range' ;
        CEFMemoryErr_Component_Not_Found: _Error := 'Component not found' ;
        CEFMemoryErr_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        CEFMemoryErr_Access_Violation: _Error := 'Access violation' ;
        CEFMemoryErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        CEFMemoryErr_Invalid_Component: _Error := 'Invalid component' ;
        CEFMemoryErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { Translate_Error }


function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ; external 'CEF_Util.dll' ;

{ Internal unit utility routines... }

function Get_Mem( Size : integer ) : pointer ;

var Res : pointer ;

begin
{$IFDEF DELPHI}
    Res := nil ;
    try
{$ENDIF}
        getmem( Res, Size ) ;
{$IFDEF DELPHI}
    except
    end ;
{$ENDIF}
    Get_Mem := Res ;
end ;



{ TCEF_Memory_Debugger methods... }

type TCEF_Memory_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TCEF_Memory ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TCEF_Memory
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TCEF_Memory_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
        0 : I.Title := PChar( 'Access_Mode = ' + Access_Mode_To_String( Memory.Access_Mode ) ) ;
        1 : begin
                if( Memory.Default_Input = nil ) then
                begin
                    I.Title := 'Default_Input = nil' ;
                end else
                begin
                    I1 := Memory.Default_Input.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Input = ' + Pointer_To_String( Memory.Default_Input ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Input = ' + Pointer_To_String( Memory.Default_Input ) ) ;
                end ;
            end ;
        2 : begin
                if( Memory.Default_Output = nil ) then
                begin
                    I.Title := 'Default_Output = nil' ;
                end else
                begin
                    I1 := Memory.Default_Output.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( 'Default_Output = ' + Pointer_To_String( Memory.Default_Output ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( 'Default_Output = ' + Pointer_To_String( Memory.Default_Output ) ) ;
                end ;
            end ;
        3 : I.Title := PChar( '_High = ' + Num1( Memory._Memory._High ) ) ;
        4 : I.Title := PChar( '_Low = ' + Num1( Memory._Memory._Low ) ) ;
        5 : I.Title := PChar( 'Physical_Memory = ' + Pointer_To_String( Memory._Memory.Physical_Memory ) ) ;
        6 : I.Title := PChar( 'Profiling = ' + Boolean_To_String( Memory._Memory.Profiling ) ) ;
        7 : I.Title := PChar( 'Read_Latency = ' + Num1( Memory.Read_Latency ) ) ;
        8 : I.Title := PChar( '_Serial_Number = ' + Num1( Memory._Serial_Number ) ) ;
        9 : begin
                if( Memory._Memory._UI = nil ) then
                begin
                    I.Title := '_UI = nil' ;
                end else
                begin
                    I1 := Memory._Memory._UI.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( '_UI = ' + Pointer_To_String( Memory._Memory._UI ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( '_UI = ' + Pointer_To_String( Memory._Memory._UI ) ) ;
                end ;
            end ;
        10 : I.Title := Pchar( 'Watchpoint_List = ' + Pointer_To_String( pointer( Memory._Memory.Watchpoint_List ) ) ) ;
        11 : I.Title := PChar( 'Write_Latency = ' + Num1( Memory.Write_Latency ) ) ;
        12 : I.Title := PChar( 'Physical_Memory_Hi = ' + Pointer_To_String( Memory._Memory.Physical_Memory_Hi ) ) ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TCEF_Memory_Debugger.Count : longint ;

begin
    Result := 13 ;
end ;



// TCEF_Memory_Memory methods...

// Constructors and destructors...

constructor TCEF_Memory_Memory.Create ;

begin
    inherited Create ;

    Watchpoint_List := Get_Watchpoint_Manager ;
    Physical_Memory := nil ;
    Physical_Memory_Hi := nil ;
    Profiling := False ;

    { Allow us to cover the entire memory range... }
    _Low := 0 ;
    _High := -1 ;
end ;


destructor TCEF_Memory_Memory.Destroy ;

begin
    Clear_Memory ;
    Watchpoint_List.Terminate ;
    Watchpoint_List := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

procedure TCEF_Memory_Memory.Clear_Memory ;

    procedure Clear( P : PPointer_Block ; Level : integer ) ;

    var Dummy : integer ;

    begin
        if( Level = 7 ) then { Last pointer is to a data block }
        begin
            freemem( P, sizeof( TBlock ) ) ;
        end else
        begin
            { First free our children }
            for Dummy := 0 to 255 do
            begin
                if( P^[ Dummy ] <> nil ) then
                begin
                    Clear( P^[ Dummy ], Level + 1 ) ;
                end ;
            end ;

            { Now free ourself }
            freemem( P, sizeof( P^ ) ) ;
        end ;
    end ;

begin
    if( Physical_Memory <> nil ) then
    begin
        Clear( Physical_Memory, 0 ) ;
	    Physical_Memory := nil ;
    end ; { if( Physical_Memory <> nil ) }
    if( Physical_Memory_Hi <> nil ) then
    begin
        Clear( Physical_Memory_Hi, 0 ) ;
	    Physical_Memory_Hi := nil ;
    end ; { if( Physical_Memory_Hi <> nil ) }
end ; { TCEF_Memory.Clear_Memory }


function TCEF_Memory_Memory.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_List.Watchpoint_At( Address ) ;
end ;


// TCEF_Memory methods...

// API...

function TCEF_Memory.Memory : TMemory ;

begin
    Result := _Memory ;
end ;


function TCEF_Memory.Facility_Code : longint ;

begin
    Facility_Code := CEFMemory_Facility ;
end ;


function TCEF_Memory.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    _Memory := TCEF_Memory_Memory.Create ;
    _Memory.Parent := self ;
    Read_Latency := 0 ; { Infinitely fast memory :) }
    Write_Latency := 0 ;
    Default_Input := nil ;
    Default_Output := nil ;
    _Memory._UI := UI ;
    Access_Mode := Access_RW or Access_Execute ;

    Initialize := _Memory.Set_Error( CEFMemoryErr_Success ) ;
end ; { TCEF_Memory.Initialize }


function TCEF_Memory.Terminate : TUnified_Exception ;

begin
    if( _Memory._UI <> nil ) then
    begin
        _Memory._UI.Termination_Notice( self ) ;
    end ;
    Terminate := _Memory.Set_Error( CEFMemoryErr_Success ) ;
    _Memory.Free ;
end ; { TCEF_Memory.Terminate }



function In_Range_64( Address, _Low, _High : int64 ) : boolean ;

var A, L, H : array[ 0..7 ] of byte ;
    Loop : integer ;

begin
    Result := True ; // Assume Address is in range
    move( Address, A, sizeof( Address ) ) ;
    move( _Low, L, sizeof( _Low ) ) ;
    move( _High, H, sizeof( _High ) ) ;
    for Loop := 7 downto 0 do
    begin
        if( A[ Loop ] > H[ Loop ] ) then
        begin
            Result := False ;
            exit ;
        end ;
        if( A[ Loop ] < H[ Loop ] ) then
        begin
            break ;
        end ;
    end ;
    for Loop := 7 downto 0 do
    begin
        if( A[ Loop ] < L[ Loop ] ) then
        begin
            Result := False ;
            exit ;
        end ;
        if( A[ Loop ] > L[ Loop ] ) then
        begin
            break ;
        end ;
    end ;
end ;


function TCEF_Memory.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := _Memory.Set_Error( 0 ) ;
    if( Memory and In_Range_64( Address, _Memory._Low, _Memory._High ) ) then
    begin
        Result := _Memory.Watchpoint_List.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( CEFMemoryErr_No_Matching_Watchpoint ) ;
    end ;
end ;


function TCEF_Memory.Component_Type : longint ;

begin
    Component_Type := Component_Type_Memory ;
end ;


function TCEF_Memory.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := _Memory.Set_Error( CEFMemoryErr_Invalid_Component ) ;
        exit ;
    end ;
    Default_Input := Component ;
    Connect_Input := _Memory.Set_Error( CEFMemoryErr_Success ) ;
end ;


function TCEF_Memory.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := _Memory.Set_Error( CEFMemoryErr_Invalid_Component ) ;
        exit ;
    end ;
    Default_Output := Component ;
    Connect_Output := _Memory.Set_Error( CEFMemoryErr_Success ) ;
end ;


function TCEF_Memory.Debugger : TDebug_Interface ;

begin
    Result := TCEF_Memory_Debugger.Create ;
    TCEF_Memory_Debugger( Result ).Memory := Self ;
end ;


function TCEF_Memory.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var A : array[ 0..7 ] of byte ;
    _Buffer : PChar ;
    Count : integer ;
    Dummy : integer ;
    P : PPointer_Block ;
    _Mem : PPointer_Block ;

begin
    // Quick early-out checks
    if( not Memory ) then
    begin
        Deposit := _Memory.Set_Error( CEFMemoryErr_Success ) ;
        exit ;
    end ;
    if( not In_Range_64( Address, _Memory._Low, _Memory._High ) ) then
    begin
        Deposit := _Memory.Set_Error( CEFMemoryErr_Address_Out_Of_Range ) ;
        exit ;
    end ;

    // Setup...
    if( Size = 0 ) then
    begin
        Count := 1 ; // Default is 1 byte
    end else
    begin
        Count := ( Size + 7 ) div 8 ; // Round to byte
    end ;
    _Buffer := PChar( Buffer ) ;
    move( Address, A, sizeof( Address ) ) ;

    // Make sure base structure is allocated and zeroed.
    if( Address < 0 ) then
    begin
        if( _Memory.Physical_Memory_Hi = nil ) then
        begin
            getmem( _Memory.Physical_Memory_Hi, sizeof( _Memory.Physical_Memory_Hi^ ) ) ;
            fillchar( _Memory.Physical_Memory_Hi^, sizeof( _Memory.Physical_Memory_Hi^ ), 0 ) ;
        end ;
        _Mem := _Memory.Physical_Memory_Hi ;
    end else
    begin
        if( _Memory.Physical_Memory = nil ) then
        begin
            getmem( _Memory.Physical_Memory, sizeof( _Memory.Physical_Memory^ ) ) ;
            fillchar( _Memory.Physical_Memory^, sizeof( _Memory.Physical_Memory^ ), 0 ) ;
        end ;
        _Mem := _Memory.Physical_Memory ;
    end ;

    while( Count > 0 ) do
    begin
        P := _Mem ;
        for Dummy := 7 downto 2 do
        begin
            if( P^[ A[ Dummy ] ] = nil ) then
            begin
                P^[ A[ Dummy ] ] := Get_Mem( sizeof( P^ ) ) ;
                if( P^[ A[ Dummy ] ] = nil ) then
                begin
                    Deposit := _Memory.Set_Error( CEFMemoryErr_Memory_Exhausted ) ;
                    exit ;
                end ;
                fillchar( P^[ A[ Dummy ] ]^, sizeof( P^ ), 0 ) ;
            end ;
            P := P^[ A[ Dummy ] ] ;
        end ;
        if( P^[ A[ 1 ] ] = nil ) then { Uninitialized memory }
        begin
            P^[ A[ 1 ] ] := Get_Mem( sizeof( P^ ) ) ;
            if( P^[ A[ 1 ] ] = nil ) then
            begin
                Deposit := _Memory.Set_Error( CEFMemoryErr_Memory_Exhausted ) ;
                exit ;
            end ;
            fillchar( P^[ A[ 1 ] ]^, 256, 255 ) ;
        end ;
        repeat { Avoid 8-level lookups while within a single block }
            PBlock( P^[ A[ 1 ] ] )^[ A[ 0 ] ] := ord( _Buffer[ 0 ] ) ;
            _Buffer := _Buffer + 1 ;
            dec( Count ) ;
            inc( Address ) ;
            move( Address, A, sizeof( Address ) ) ;
        until ( ( A[ 0 ] = 0 ) or ( Count = 0 ) ) ;
    end ; { while( Count > 0 ) }
    Deposit := _Memory.Set_Error( CEFMemoryErr_Success ) ;
end ; { TCEF_Memory.Deposit }


function TCEF_Memory.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component <> Default_Input ) or ( Component = nil ) ) then
    begin
	    Disconnect_Input := _Memory.Set_Error( CEFMemoryErr_Component_Not_Found ) ;
    end else
    begin
	    Disconnect_Input := _Memory.Set_Error( CEFMemoryErr_Success ) ;
	    Default_Input := nil ;
    end ;
end ;


function TCEF_Memory.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component <> Default_Output ) or ( Component = nil ) ) then
    begin
	    Disconnect_Output := _Memory.Set_Error( CEFMemoryErr_Component_Not_Found ) ;
    end else
    begin
	    Disconnect_Output :=_Memory.Set_Error( CEFMemoryErr_Success ) ;
	    Default_Output := nil ;
    end ;
end ;


function TCEF_Memory.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var A : array[ 0..7 ] of byte ;
    _Buffer : PChar ;
    Count : integer ;
    Dummy : integer ;
    _Mem : PPointer_Block ;
    P : PPointer_Block ;
    Value : byte ;

begin
    // Quick early-out checks
    if( not Memory ) then
    begin
        Examine := _Memory.Set_Error( CEFMemoryErr_Success ) ;
        exit ;
    end ;
    if( not In_Range_64( Address, _Memory._Low, _Memory._High ) ) then
    begin
        Examine := _Memory.Set_Error( CEFMemoryErr_Address_Out_Of_Range ) ;
        exit ;
    end ;

    // Setup...
    if( Size = 0 ) then
    begin
        Count := 1 ; // Default is 1 byte
    end else
    begin
        Count := ( Size + 7 ) div 8 ; // Round to byte
    end ;
    move( Address, A, sizeof( Address ) ) ;

    fillchar( Buffer^, Count, 255 ) ;
    _Buffer := Buffer ;
    if( Address < 0 ) then
    begin
        if( _Memory.Physical_Memory_Hi = nil ) then
        begin
            Examine := _Memory.Set_Error( CEFMemoryErr_Success ) ;
            exit ;
        end ;
        _Mem := _Memory.Physical_Memory_Hi ;
    end else
    begin
        if( _Memory.Physical_Memory = nil ) then
        begin
            Examine := _Memory.Set_Error( CEFMemoryErr_Success ) ;
            exit ;
        end ;
        _Mem := _Memory.Physical_Memory ;
    end ;

    while( Count > 0 ) do
    begin
        Value := 255 ; { Default value if specified byte is undefined }
        P := _Mem ;
        for Dummy := 7 downto 1 do
        begin
            if( P^[ A[ Dummy ] ] = nil ) then { Not defined at this level }
            begin
                P := nil ;
                break ;
            end ;
            P := P^[ A[ Dummy ] ] ; { Follow link to next level }
        end ;
        if( P <> nil ) then { Found a block of defined memory }
        begin
            while( P <> nil ) do { Avoid 8-level lookup while within a single block }
            begin
                Value := PBlock( P )^[ A[ 0 ] ] ;
                _Buffer[ 0 ] := chr( Value ) ;
                _Buffer := _Buffer + 1 ;
                dec( Count ) ; { One less byte to read }
                if( Count = 0 ) then { We are done }
                begin
                    break ;
                end ;
                inc( Address ) ;
                move( Address, A, sizeof( Address ) ) ;
                if( A[ 0 ] = 0 ) then
                begin
                    break ; { Reached end of current block }
                end ;
            end ; { while( P <> nil ) }
            continue ;
        end ; { if( P <> nil ) }
        _Buffer[ 0 ] := chr( Value ) ;
        _Buffer := _Buffer + 1 ;
        dec( Count ) ; { One less byte to read }
        inc( Address ) ;
    end ; { while( Count > 0 ) }
    Examine := _Memory.Set_Error( CEFMemoryErr_Success ) ;
end ; { TCEF_Memory.Examine }


function TCEF_Memory.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    if( Memory and ( In_Range_64( Address, _Memory._Low, _Memory._High ) ) ) then
    begin
        Get_Access_Mode := Access_Mode ;
    end else
    begin
        Get_Access_Mode := Access_None ;
    end ;
end ;


function TCEF_Memory.Get_Profiling : boolean ;

begin
    Get_Profiling := _Memory.Profiling ;
end ;


function TCEF_Memory.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := Read_Latency ;
end ;


function TCEF_Memory.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := Write_Latency ;
end ;


function TCEF_Memory.Input_Component( Index : longint ) : TComponent ; 

begin
    if( Index = 0 ) then
    begin
	Input_Component := Default_Input ;
    end else
    begin
	Input_Component := nil ;
    end ;
end ;


const _Name : PChar = 'CEF Generic memory'#0 ;

function TCEF_Memory.Name : PChar ;

begin
    Name := _Name ;
end ;


function TCEF_Memory.Output_Component( Index : longint ) : TComponent ;

begin
    if( Index = 0 ) then
    begin
	    Output_Component := Default_Output ;
    end else
    begin
	    Output_Component := nil ;
    end ;
end ;


function TCEF_Memory.Read( Address : int64 ; Size, IO_Type : longint ) : boolean ;

var Buffer : PChar ;
    Count : integer ;
    Watch : TCEF_Watchpoint ;

begin
    if(
        In_Range_64( Address, _Memory._Low, _Memory._High )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Read ) = 0 ) then
        begin
            Read := False ; { CEFMemoryErr_Access_Violation }
            exit ;
        end ;
        if( Size = 0 ) then
        begin
            Count := 1 ;
        end else
        begin
            Count := ( Size + 7 ) div 8 ;
        end ;
        Read := True ;
        if( _Memory._UI.Clock <> nil ) then
        begin
            _Memory._UI.Clock.Block( self, Read_Latency ) ;
        end ;
        if( Default_Output <> nil ) then
        begin
            getmem( Buffer, Count ) ;
            Examine( Address, Size, Buffer, True ) ;
            Default_Output.Write_String( Address, Buffer, Size, IO_Type_Memory ) ;
            freemem( Buffer, Count ) ;
        end ;
        Watch := _Memory.Watchpoint_At( Address ) ;
        if( Watch <> nil ) then
        begin
            if( ( Watch.Access and Access_Read ) <> 0 ) then
            begin
                _Memory._UI.Watchpoint_Notice( Address, Access_Read, 0, TComponent( self ), True, False, False ) ;
            end ;
        end ;
	    if( _Memory.Profiling ) then
        begin
            //~~~
        end ;
    end else
    begin
	    Read := False ;
    end ;
end ; { TCEF_Memory.Read }


function TCEF_Memory.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

var Loop, High : int64 ;
    S : longint ;
    Value : byte ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    _Memory.Clear_Memory ;
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


function TCEF_Memory.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var S : longint ;

begin
    Result := nil ;
    S := sizeof( _Memory._Low ) ;
    Stream.Read( _Memory._Low, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    Stream.Read( _Memory._High, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    S := sizeof( Read_Latency ) ;
    Stream.Read( Read_Latency, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
     Stream.Read( Write_Latency, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    S := sizeof( _Memory.Profiling ) ;
    Stream.Read( _Memory.Profiling, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
end ;


function TCEF_Memory.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

    function First_In( P : PPointer_Block ) : integer ;
    { Find first non-nil pointer in block. }

    var Loop : integer ;

    begin
        for Loop := 0 to 255 do
        begin
            if( P^[ Loop ] <> nil ) then
            begin
                First_In := Loop ;
                exit ;
            end ;
        end ;
        First_In := -1 ;
    end ;


    function Last_In( P : PPointer_Block ) : integer ;
    { Find last non-nil pointer in block. }

    var Loop : integer ;

    begin
        for Loop := 255 downto 0 do
        begin
            if( P^[ Loop ] <> nil ) then
            begin
                Last_In := Loop ;
                exit ;
            end ;
        end ;
        Last_In := -1 ;
    end ;

var A : array[ 0..7 ] of byte ;
    Dummy : integer ;
    _E : TUnified_Exception ;
    Loop : int64 ;
    High : int64 ;
    P : PPointer_Block ;
    S : longint ;
    Value : integer ;

begin { TCEF_Memory.Save_Contents }
    fillchar( Result, sizeof( Result ), 0 ) ;
    if( ( _Memory.Physical_Memory = nil ) and ( _Memory.Physical_Memory_Hi = nil ) ) then
    begin
	    Loop := 0 ;
	    High := 0 ; { Write only one byte if no memory written to yet }
    end else
    begin
        { Find lowest used address... }
        P := _Memory.Physical_Memory ;
        if( P = nil ) then
        begin
            P := _Memory.Physical_Memory_Hi ;
        end ;
        for Dummy := 7 downto 1 do
        begin
            A[ Dummy ] := First_In( P ) ;
            P := P^[ A[ 7 ] ] ;
        end ;
        A[ 0 ] := 0 ;
        move( A, Loop, sizeof( Loop ) ) ;

        { Find highest used address... }
        P := _Memory.Physical_Memory_Hi ;
        if( P = nil ) then
        begin
            P := _Memory.Physical_Memory ;
        end ;
        for Dummy := 7 downto 1 do
        begin
            A[ Dummy ] := Last_In( P ) ;
            P := P^[ A[ 7 ] ] ;
        end ;
        A[ 0 ] := 0 ;
        move( A, High, sizeof( High ) ) ;
    end ; // if( ( _Memory.Physical_Memory = nil ) and ( _Memory.Physical_Memory_Hi = nil ) )

    { Write contents... }
    Stream.Write( Loop, sizeof( Loop ) ) ;
    _E := Stream.Last_Error ;
    if( _E <> nil ) then
    begin
        Result := _E ;
        exit ;
    end ;
    Stream.Write( High, sizeof( High ) ) ;
    _E := Stream.Last_Error ;
    if( _E <> nil ) then
    begin
        Result := _E ;
        exit ;
    end ;
    S := 1 ;
    while( Loop <= High ) do
    begin
        Examine( Loop, S, @Value, True ) ;
	    Stream.Write( Value, 1 ) ;
        _E := Stream.Last_Error ;
        if( _E <> nil ) then
        begin
            Result := _E ;
            exit ;
        end ;
	    Loop := Loop + 1 ;
    end ;
end ; { TCEF_Memory.Save_Contents }


function TCEF_Memory.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var _E : TUnified_Exception ;

begin
    Result := nil ;
    Stream.Write( _Memory._Low, sizeof( _Memory._Low ) ) ;
    _E := Stream.Last_Error ;
    if( _E <> nil ) then
    begin
        Result := _E ;
        exit ;
    end ;
    Stream.Write( _Memory._High, sizeof( _Memory._High ) ) ;
    _E := Stream.Last_Error ;
    if( _E <> nil ) then
    begin
        Result := _E ;
        exit ;
    end ;
    Stream.Write( Read_Latency, sizeof( Read_Latency ) ) ;
    _E := Stream.Last_Error ;
    if( _E <> nil ) then
    begin
        Result := _E ;
        exit ;
    end ;
    Stream.Write( Write_Latency, sizeof( Write_Latency ) ) ;
    _E := Stream.Last_Error ;
    if( _E <> nil ) then
    begin
        Result := _E ;
        exit ;
    end ;
    Stream.Write( _Memory.Profiling, sizeof( _Memory.Profiling ) ) ;
    _E := Stream.Last_Error ;
    if( _E <> nil ) then
    begin
        Result := _E ;
        exit ;
    end ;
end ; // TCEF_Memory.Save_State


function TCEF_Memory.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    if( Memory and In_Range_64( Low, _Memory._Low, _Memory._High ) and In_Range_64( High, _Memory._Low, _Memory._High ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := _Memory.Set_Error( CEFMemoryErr_Success ) ;
end ;


procedure TCEF_Memory.Set_Profiling( _On, Children : boolean ) ;  

begin
    _Memory.Profiling := _On ;
end ;


procedure TCEF_Memory.Set_Read_Latency( Value : longint ) ; 

begin
    Read_Latency := Value ;
end ;


function TCEF_Memory.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Watchpoint := _Memory.Set_Error( CEFMemoryErr_Success ) ;
    if( Memory and In_Range_64( Address, _Memory._Low, _Memory._High ) ) then
    begin
        _Memory.Watchpoint_List.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TCEF_Memory.Set_Write_Latency( Value : longint ) ;  

begin
    Write_Latency := Value ;
end ;


procedure TCEF_Memory.Show_Status ; 

begin
    { This routine intentionally left blank - no status to show }
end ;


function TCEF_Memory.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var B : integer ;
    Watch : TCEF_Watchpoint ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    if(
        In_Range_64( Address, _Memory._Low, _Memory._High )
        and
        ( IO_Type = IO_Type_Memory )
      ) then
    begin
        if( ( Access_Mode and Access_Write ) = 0 ) then
        begin
            Write := _Memory.Set_Error( CEFMemoryErr_Access_Violation ) ;
            exit ;
        end ;
        if( _Memory.Profiling ) then
        begin
            //~~~
        end ;
        Watch := _Memory.Watchpoint_At( Address ) ;
        if( Watch <> nil ) then
        begin
            if( ( Watch.Access and Access_Write ) <> 0 ) then
            begin
                if( _Memory._UI <> nil ) then
                begin
                    _Memory._UI.Watchpoint_Notice( Address, Access_Write, 0, TComponent( self ), True, False, False ) ;
                end ;
            end ;
        end ;
	    if( ( _Memory._UI <> nil ) and ( _Memory._UI.Clock <> nil ) ) then
        begin
            _Memory._UI.Clock.Block( self, Write_Latency ) ;
            if( _Logger <> nil ) then
            begin
                B := _Logger.Data_Radix ;
                if( ( B < 2 ) or ( B > 36 ) ) then
                begin
                    B := 10 ;
                end ;
                _Logger.Log( self, PChar( cvtb( 10, B, inttostr( Value ) ) +
                    ' (' + inttostr( Size ) + ')' + ' @' +
                    cvtb( 10, B, inttostr( Address ) ) ), -1, False, LT_Write ) ;
            end ;
        end ;
	    Write := Deposit( Address, Size, @Value, True ) ;
    end else
    begin
	    Write := _Memory.Set_Error( CEFMemoryErr_Success ) ;
    end ;
end ; // TCEF_Memory.Write


function TCEF_Memory_Memory.Facility_Code : longint ;

begin
    Result := CEFMemory_Facility ;
end ;


function TCEF_Memory_Memory.Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;

begin
    if( ( Low < 0 ) or ( Low > High ) ) then
    begin
	    Set_Address_Range := Set_Error( CEFMemoryErr_Invalid_Range ) ;
	    exit ;
    end ;
    Set_Address_Range := Set_Error( CEFMemoryErr_Success ) ;
    _Low := Low ;
    _High := High ;
end ;


procedure TCEF_Memory_Memory.Get_Address_Range( var Low, High : int64 ) ;

begin
    Low := _Low ;
    High := _High ;
end ;


procedure TCEF_Memory.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TCEF_Memory.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TCEF_Memory.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TCEF_Memory.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function TCEF_Memory.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TCEF_Memory.Set_Logger( Value : TCEF_Logger ) ;

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
    _Memory._Logger := Value ;
end ;


procedure TCEF_Memory_Memory.Dump( Start, Size : int64 ; Buffer : pointer ) ;

var P : Pchar ;
    Index, Loop : integer ;
    S : longint ;
    V : integer ;

begin
    // Setup...
    Index := 0 ;
    P := PChar( Buffer ) ;

    // Do the dump...
    Loop := Start ;
    while( Size > 0 ) do
    begin
        S := 1 ;
        Parent.Examine( Loop, S, @V, True ) ;
        P[ Index ] := chr( V ) ;
        inc( Index ) ;
        inc( Loop ) ;
        dec( Size ) ;
    end ;
end ;


procedure TCEF_Memory_Memory.Load( Start, Size : int64 ; Buffer : pointer ) ;

var P : Pchar ;
    Index, Loop : integer ;
    V : integer ;

begin
    // Setup...
    Index := 0 ;
    P := PChar( Buffer ) ;

    // Do the load
    Loop := Start ;
    while( Size > 0 ) do
    begin
        V := ord( P[ Index ] ) ;
        Parent.Deposit( Loop, 8, @V, True ) ;
        inc( Index ) ;
        dec( Size ) ;
        inc( Loop ) ;
    end ;
end ;


function TCEF_Memory_Memory.Map_Virtual_To_Physical( Virt : int64 ) : int64 ;

var A : array[ 0..7 ] of byte ;
    Dummy : integer ;
    _Mem : PPointer_Block ;
    P : PPointer_Block ;

begin
    Result := 0 ; // Assume no mapping
    // Quick early-out check
    if( not In_Range_64( Virt, _Low, _High ) ) then
    begin
        exit ;
    end ;

    // Setup...
    move( Virt, A, sizeof( Virt ) ) ;

    if( Virt < 0 ) then
    begin
        if( Physical_Memory_Hi = nil ) then
        begin
            exit ;
        end ;
        _Mem := Physical_Memory_Hi ;
    end else
    begin
        if( Physical_Memory = nil ) then
        begin
            exit ;
        end ;
        _Mem := Physical_Memory ;
    end ;

    P := _Mem ;
    for Dummy := 7 downto 1 do
    begin
        if( P^[ A[ Dummy ] ] = nil ) then { Not defined at this level }
        begin
            exit ;
        end ;
        P := P^[ A[ Dummy ] ] ; { Follow link to next level }
    end ;
    if( P <> nil ) then { Found a block of defined memory }
    begin
        Result := integer( P ) + A[ 0 ] ;
    end ; { if( P <> nil ) }
end ;


{$IFDEF Test}

{ All following code is for testing the CEF Memory class... }

type TTest_Clock = class( TMaster_Clock )
                       procedure Block( Component : TComponent ;
                           Time_Delta : int64 ) ; override ;

                       procedure Initialize( UI : TUI_Interface ) ; override ;

                       function Get_Time_Index : int64 ; override ;

                       function Version : integer ; override ;
                   end ;

var Test_Clock : TTest_Clock ;

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

                       function Breakpoint_Notice( Address : int64 ;
                           Physical : boolean ; Space : integer ;
                           CPU : TCPU ) : boolean ; override ;

                       function Clock : TMaster_Clock ; override ;

                       procedure Log_Error( Component : TComponent ;
                           Text : PChar ; Severity : longint ) ;
                           override ;

                       procedure Log_Simulated_Error( Component : TComponent ;
                           Text : PChar ; Severity : longint ) ; override ;

                       procedure Log_Status( Text : PChar ; Index : longint ) ;
                           override ;

                       function Version : integer ; override ;

                       procedure Watchpoint_Notice( Address : int64 ;
                           Access, Tag : longint ; Component : TComponent ;
                           Memory, Internal, Port : boolean ) ;
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
                                    IO_Type : longint ) : boolean ; override ;

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
                                    IO_Type : longint ) : TUnified_Exception ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; IO_Type : longint ) : TUnified_Exception ; override ;
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
                                    IO_Type : longint ) : boolean ; override ;

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
                                    IO_Type : longint ) : TUnified_Exception ; override ;

                                function Write_String( Address : int64 ; Value : PChar ;
                                    Size : longint ; IO_Type : longint ) : TUnified_Exception ; override ;
                             end ;

procedure TTest_UI.Block( Component : TComponent ; Blocked : boolean ) ;

begin
end ;


function TTest_UI.Breakpoint_Notice( Address : int64 ; Physical : boolean ;
    Space : integer ; CPU : TCPU ) : boolean ;

begin
    ShowMessage( 'Breakpoint notice' ) ;
end ;


function TTest_UI.Clock : TMaster_Clock ;

begin
    Result := Test_Clock ;
end ;


procedure TTest_UI.Log_Error( Component : TComponent ; Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TTest_UI.Log_Simulated_Error( Component : TComponent ; Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TTest_UI.Log_Status( Text : PChar ; Index : longint ) ;

begin
end ;


function TTest_UI.Version : integer ;

begin
    Result := 0 ;
end ;


procedure TTest_UI.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
   Component : TComponent ; Memory, Internal, Port : boolean ) ;

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


procedure TTest_Clock.Block( Component : TComponent ; Time_Delta : int64 ) ;

begin
end ;


procedure TTest_Clock.Initialize( UI : TUI_Interface ) ;

begin
end ;


function TTest_Clock.Get_Time_Index : int64 ;

begin
    Result := 0 ;
end ;


function TTest_Clock.Version : integer ;

begin
    Result := 0 ;
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
    IO_Type : longint ) : boolean ;

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
    IO_Type : longint ) : TUnified_Exception ;

begin
    if( Enabled and ( IO_Type = IO_Type_Memory ) ) then
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
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    Write( Address, ord( Value[ 0 ] ), Size, IO_Type ) ;
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
    IO_Type : longint ) : boolean ;

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
    IO_Type : longint ) : TUnified_Exception ;

begin
    if( Enabled and ( IO_Type = IO_Type_Memory ) ) then
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
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    ShowMessage( 'Call to component.Write_String' ) ;
end ;


type TTest_Streamer = class( TCOM_Stream )
        public
            S : TMemoryStream ;

            constructor Create ;
            function At_End : boolean ; override ;
            function Facility_Code : longint ;
            procedure Read( var Buffer ; var Size : longint ) ; override ;
            procedure Read_Line( var Buffer ; var Size : longint ) ; override ;
            procedure Seek( Position : longint ) ; override ;
            procedure Write( const Buffer ; size : longint ) ; override ;
            procedure Write_Line( Buffer : PChar ) ; override ;
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


procedure TTest_Streamer.Read( var Buffer ; var Size : longint ) ;

begin
    S.ReadBuffer( Buffer, Size ) ;
end ;


procedure TTest_Streamer.Read_Line( var Buffer ; var Size : longint ) ;

begin
    ShowMessage( 'Call to Read_Line' ) ;
end ;


procedure TTest_Streamer.Seek( Position : longint ) ;

begin
    S.Seek( 0, soFromBeginning ) ;
end ;


procedure TTest_Streamer.Write( const Buffer ; size : longint ) ;

begin
    S.WriteBuffer( Buffer, Size ) ;
end ;


procedure TTest_Streamer.Write_Line( Buffer : PChar ) ;

begin
    ShowMessage( 'Call to Write_Line' ) ;
end ;


procedure Test_Unit ;

var Address, Count, Temp : int64 ;
    E : TUnified_Exception ;
    Test : TCEF_Memory ;
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
    Test_Clock := TTest_Clock.Create ;
    Test := TCEF_Memory.Create ;
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
    E := Test.Set_Address_Range( 0, 255 ) ;
    Check_E ;
    E := Test.Set_Address_Range( 255, 0 ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'No error on Invalid set_address_range' ) ;
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
    E := Test.Write( 0, 1, 8, 1 ) ; { Non memory write }
    Check_E ;
    E := Test.Write( 0, 1, 8, 0 ) ; { Memory write }
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Read ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, 0 ) ;
    if( E.Code = 0 ) then
    begin
        ShowMessage( 'Write with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Write ) ;
    if( Test.Read( 0, 8, 0 ) ) then
    begin
        ShowMessage( 'Read with invalid access did not fail' ) ;
    end ;
    E := Test.Set_Access_Mode( 0, 255, True, Access_Execute ) ;
    Check_E ;
    E := Test.Write( 0, 1, 8, 0 ) ;
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
    E := Test.Write( 1, 1, 8, 0 ) ;
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
    if( Test.Read( 1, 8, 1 ) ) then
    begin
        ShowMessage( 'Error on read' ) ;
    end ;
    Test_UI.Allow_Profile( 1, Access_Read ) ;
    if( not Test.Read( 1, 8, 0 ) ) then
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
    E := Test.Write( 0, 1, 8, 0 ) ;
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
    if( not Test.Read( 1, 8, 0 ) ) then
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
    Test_Stream.Seek( 0 ) ;
    Test.Save_State( Test_Stream ) ;
    Test.Set_Address_Range( 128, 1024 ) ;
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


    { Stress test... }

    { Setup... }
    E := Test.Initialize( Test_UI ) ;
    Check_E ;
    E := Test.Set_Address_Range( 0, $7FFFFFFFFFFFFFFF ) ;
    Check_E ;
    E := Test.Set_Access_Mode( 0, $7FFFFFFFFFFFFFFF, True, Access_RW ) ;
    Check_E ;
    Count := 0 ;
    Randomize ;

    { Stress! }
    while( True ) do
    begin
        { Generate random 63-bit address... }
        Address := random( $7FFFFFFF ) ;
        Address := ( Address shl 31 ) or random( $7FFFFFFF ) ;
        Address := ( Address shl 1 ) or random( 2 ) ;

        { Do some read/write operations... }
        E := Test.Deposit( Address, 64, @Address, True ) ;
        Check_E ;
        Dummy := 64 ;
        E := Test.Examine( Address, Dummy, @Temp, True ) ;
        Check_E ;
        if( Temp <> Address ) then
        begin
            ShowMessage( 'R/W to address ' + inttostr( Address ) + ' failed' ) ;
        end ;

        { Count the accesses }
        inc( Count ) ;
    end ;

    { Cleanup... }
    E := Test.Terminate ;
    Check_E ;
end ;
{$ENDIF}


const Memory_Facility_Name : PChar = 'CEF Memory' ;


initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

