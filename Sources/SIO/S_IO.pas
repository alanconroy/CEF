{$N+}
{
        Program Name : SIO
        Package Name : CEF
        Purpose      : S-100 SIO component for CEF
        Institution  : 
        Date Written : 5-Jan-2007
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

	  This unit implements an S-100 MITs 88-SIO board as a CEF component.
        The component emulates a single I/O port (two physical port addresses)
        at port 0 & 1, but can be set to any base port.

        First port is status:
          Input:
                and 1 = Input device not ready (RTS inactive)
                and 2 = Transmit buffer empty
                and 4 = Parity error (on received data)
                and 8 = Framing error (no valid stop bit)
                and 16 = Data overflow (multipel received characters before they
                         were read).
                and 32 = Data available (input received)
                and 64 = unused
                and 128 = Device is not ready for output (DTR inactive)
          Output:
                and 1 = input interrupt enable
                and 2 = output interrupt enable
                and 4-128 = unused
                
        Second port is read for input and write for output.


        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan


        Conditionals:
            TEST    Causes test to run upon unit initialization.
}

unit S_IO ;

interface

{$I EDEFINES.INC}

uses // Borland...
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

const CEFSIO_Facility = -1 ;
      CEFSIOErr_Success = 0 ;
      CEFSIOErr_Invalid_Range = 1 ;
      CEFSIOErr_Component_Not_Found = 2 ;
      CEFSIOErr_No_Matching_Watchpoint = 3 ;
      CEFSIOErr_Access_Violation = 4 ;
      CEFSIOErr_Address_Out_Of_Range = 5 ;
      CEFSIOErr_Invalid_Component = 6 ;
      CEFSIOErr_Memory_Exhausted = 7 ; { Couldn't allocate any more memory }

type TS_IO = class ;

     TReceiver = class( TBase_Cable )
                     private
                         Cable : TComponent ;
                         IO : TS_IO ;
                         Parent : TComponent ;

                     public
                         procedure Receive( Source : TComponent ; Speed : int64 ;
                             Value, Data_Size, Stop_Bits : longint ) ;
                             override ;

                         procedure Send_Data( Value : longint ) ;
                 end ;

     TS_IO_SIO = class;

     TS_IO = class( TBase_Component )
	    private { Instance data... }
	        Access_Mode : integer ;
            Inputs, Outputs : TList ;
	        Profiling : boolean ; { True if profiling accesses }
	        _UI : TUI_Interface ;
            Watchpoint_List : TCEF_Watchpoint_Manager ;
            _Tag : longint ;
            _Parent : TComponent ;
            Status, Data, Interrupts : byte ;
            Doing_Write : boolean ; // True if we are doing a write (to prevent ringing)
            Send_Parity : boolean ;
            _Baud : integer ;
            _Bits : integer ;
            _Stop_Bits : integer ;
            _Parity : integer ;
            _Cable : TReceiver ;
            Allow_Input_Interrupt, Allow_Output_Interrupt : boolean ;
            Vector : integer ; // Interrupt vector
            Waiting_For_Interrupt : boolean ; // True if sent interrupt
            Send_Vector : boolean ;
            SIO : TS_IO_SIO ;

        protected
            function Translate_Error( Code : longint ) : string ; override ;

        private // Internal utility routines...
            function Default_Input : TComponent ;
            function Default_Output : TComponent ;
            function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

	    public { Public instance data... }
            _Serial_Number : integer ;

        public { API... }
            function Facility_Code : longint ; override ;

            function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                override ;

            function Terminate : TUnified_Exception ; override ;

            function Cable : TCable ; override ;

            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; override ;

            function Component_Type : longint ; override ;

            function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                override ;

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

            function Input_Component( Index : longint ) : TComponent ; override ;

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

            procedure Set_Up( P : PChar ) ; override ;

            procedure Signal_Change_Notice( Component : TComponent ;
                Index : longint ; Active : boolean ) ; override ;
     end ;

     TS_IO_SIO = class( TBase_Memory )
                     public // Constructors...
                         constructor Create ;

                     private // Instance data...
                         Parent : TS_IO ;
                          _Low : int64 ; // Lowest port and number of ports

                     public // API...
                         function Facility_Code : longint ; override ;

                         function Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;
                             override ;

                         procedure Get_Address_Range( var Low, High : int64 ) ;
                             override ;
                  end ;

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
     UE, // Create_Simple_UI
     Num1s,
     Parse, // TString_Parser

     // CEF...
     CEFUtil_Int ; // Get_Watchpoint_Manager


function TS_IO.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        CEFSIOErr_Success: _Error := 'Success' ;
        CEFSIOErr_Invalid_Range: _Error := 'Invalid range' ;
        CEFSIOErr_Component_Not_Found: _Error := 'Component not found' ;
        CEFSIOErr_No_Matching_Watchpoint: _Error := 'No matching watchpoint' ;
        CEFSIOErr_Access_Violation: _Error := 'Access violation' ;
        CEFSIOErr_Address_Out_Of_Range: _Error := 'Address out of range' ;
        CEFSIOErr_Invalid_Component: _Error := 'Invalid component' ;
        CEFSIOErr_Memory_Exhausted: _Error := 'Memory exhausted' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { Translate_Error }


// TReceiver methods...

procedure TReceiver.Receive( Source : TComponent ; Speed : int64 ;
    Value, Data_Size, Stop_Bits : longint ) ;

var S : string ;

begin
    if( ( IO.Status and 32 ) = 0 ) then
    begin
        IO.Status := IO.Status or 16 ; // Overflow
    end ;
    IO.Status := IO.Status or 32 ; // Data available
    if( ( Speed <> IO._Baud ) and ( Speed <> 0 ) and ( IO._Baud <> 0 ) ) then
    begin
        IO.Status := IO.Status or 12 ; // Parity and framing errors
        S := Translate_Serial_Data( chr( Value ), Speed, IO._Baud, 8, Stop_Bits ) ;
        IO.Data := ord( S[ 1 ] ) ;
    end else
    begin
        IO.Data := Value ;
    end ;
end ;


procedure TReceiver.Send_Data( Value : longint ) ;

begin
    if( Cable <> nil ) then
    begin
        Cable.Cable.Receive( Parent, 110, Value, 8, 1 ) ;
    end ;
end ;



{ TCEF_SIO_Debugger methods... }

type TCEF_SIO_Debugger = class( TText_Debugger )
                                private
                                    _IO : TS_IO ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property IO : TS_IO
                                                 read _IO
                                                 write _IO ;
                            end ;

function TCEF_SIO_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
        3 : I.Title := PChar( '_Low = ' + Num1( IO.SIO._Low ) ) ;
        4 : I.Title := PChar( 'Profiling = ' + Boolean_To_String( IO.Profiling ) ) ;
        5 : I.Title := PChar( '_Serial_Number = ' + Num1( IO._Serial_Number ) ) ;
        6 : begin
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
        7 : I.Title := Pchar( 'Watchpoint_List = ' + Pointer_To_String( pointer( IO.Watchpoint_List ) ) ) ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TCEF_SIO_Debugger.Count : longint ;

begin
    Result := 9 ;
end ;


{ TS_IO methods... }

function TS_IO.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TS_IO.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


function TS_IO.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_List.Watchpoint_At( Address ) ;
end ;


{ API... }

function TS_IO.Facility_Code : longint ;

begin
    Facility_Code := CEFSIO_Facility ;
end ;


function TS_IO.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    { General setup... }
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    Profiling := False ;
    _UI := UI ;
    Watchpoint_List := Get_Watchpoint_Manager ;
    Access_Mode := Access_RW or Access_Execute ;
    _Cable := TReceiver.Create ;
    _Cable.IO := self ;
    Initialize := Set_Error( CEFSIOErr_Success ) ;
    SIO := TS_IO_SIO.Create ;
    Status := 129 ; // Nothing ready
    Interrupts := 0 ; // No interrupts
    Allow_Input_Interrupt := False ;
    Allow_Output_Interrupt := False ;
    Vector := 255 ; // RST7
    _UI.Want_Signals( self, True ) ; // So we know when cable signal states change
end ; { TS_IO.Initialize }


function TS_IO.Terminate : TUnified_Exception ;

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
    Terminate := Set_Error( CEFSIOErr_Success ) ;
end ; { TS_IO.Terminate }



function TS_IO.Cable : TCable ;

begin
    Result := _Cable ;
end ;


function TS_IO.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Clear_Watchpoint := nil ;
    if( not Memory ) then
    begin
        Result := Watchpoint_List.Clear_Watchpoint( Address, Access ) ;
//        Clear_Watchpoint := Set_Error( CEFSIOErr_No_Matching_Watchpoint ) ;
    end ;
end ;


function TS_IO.Component_Type : longint ;

begin
    Component_Type := Component_Type_Memory ;
end ;


function TS_IO.Connect_Input( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := Set_Error( CEFSIOErr_Invalid_Component ) ;
        exit ;
    end ;
    if( Component.Component_Type = Component_Type_Cable ) then
    begin
        _Cable.Cable := Component ;
        Component.Connect_Input( self ) ;
        Status := Status and ( not 1 ) ; // Input device is ready
    end else
    begin
        Inputs.Add( Component ) ;
    end ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        P := integer( Component ) ;
        Parent.Child_Notification( self, Code, P ) ;
    end ;
    Connect_Input := Set_Error( CEFSIOErr_Success ) ;
end ;


function TS_IO.Connect_Output( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := Set_Error( CEFSIOErr_Invalid_Component ) ;
        exit ;
    end ;
    if( Component.Cable <> nil ) then
    begin
        _Cable.Cable := Component ;
        Status := Status and ( not 1 ) ; // Input device ready
    end else
    begin
        Outputs.Add( Component ) ;
    end ;
    if( Parent <> nil ) then
    begin
        Code := Child_Notice_Connect ;
        P := integer( Component ) ;
        Parent.Child_Notification( self, Code, P ) ;
    end ;
    Connect_Output := Set_Error( CEFSIOErr_Success ) ;
end ;


function TS_IO.Debugger : TDebug_Interface ;

begin
    Result := TCEF_SIO_Debugger.Create ;
    TCEF_SIO_Debugger( Result ).IO := Self ;
end ;


function TS_IO.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var _Buffer : PChar ;

begin
    Result := nil ;
    if( ( not Memory ) or ( SIO._Low > Address ) or ( SIO._Low + 1 < Address ) or ( Size = 0 ) ) then
    begin
        exit ;
    end ;
    _Buffer := PChar( Buffer ) ;

    if( Address = SIO._Low + 1 ) then
    begin
        _Cable.Send_Data( ord( _Buffer[ 0 ] ) );
    end else
    begin
        Interrupts := ord( _Buffer[ 0 ] ) and 3 ;
    end ;
    Deposit := Set_Error( CEFSIOErr_Success ) ;
end ; { TS_IO.Deposit }


function TS_IO.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    Result := nil ;
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
	    if( Component = TComponent( _Cable.Cable ) ) then
        begin
            if( Parent <> nil ) then
            begin
                Code := Child_Notice_Disconnect ;
                P := integer( _Cable.Cable ) ;
                Parent.Child_Notification( self, Code, P ) ;
            end ;
            _Cable.Cable := nil ;
            Status := Status or 129 ; // Input and output device not ready
        end else
        begin
            Disconnect_Input := Set_Error( CEFSIOErr_Component_Not_Found ) ;
        end ;
    end else
    begin
	Disconnect_Input := Set_Error( CEFSIOErr_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            P := integer( Component ) ;
            Parent.Child_Notification( self, Code, P ) ;
        end ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TS_IO.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

var Code : longint ;
    P : int64 ;

begin
    Result := nil ;
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
	    if( Component = TComponent( _Cable.Cable ) ) then
        begin
            if( Parent <> nil ) then
            begin
                Code := Child_Notice_Disconnect ;
                P := integer( _Cable.Cable ) ;
                Parent.Child_Notification( self, Code, P ) ;
            end ;
            _Cable.Cable := nil ;
            Status := Status or 129 ; // Input and output device not ready
        end else
        begin
            Disconnect_Output := Set_Error( CEFSIOErr_Component_Not_Found ) ;
        end ;
    end else
    begin
	    Disconnect_Output := Set_Error( CEFSIOErr_Success ) ;
        if( Parent <> nil ) then
        begin
            Code := Child_Notice_Disconnect ;
            P := integer( _Cable.Cable ) ;
            Parent.Child_Notification( self, Code, P ) ;
        end ;
        Outputs.Remove( Component ) ;
    end ;
end ;


function TS_IO.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var B : boolean ;
    _Buffer : PChar ;
    Count : integer ;

begin
    if( ( not Memory ) or ( SIO._Low > Address ) or ( SIO._Low +  1 < Address ) ) then
    begin
        Examine := Set_Error( CEFSIOErr_Address_Out_Of_Range ) ;
        exit ;
    end ;
    if( Size = 0 ) then
    begin
        Count := 1 ; // Default6 is 1 byte
    end else
    begin
        Count := ( Size + 7 ) div 8 ; // Round to byte
    end ;
    fillchar( Buffer^, Count, 255 ) ;
    _Buffer := Buffer ;
    try
        if( Address = SIO._Low ) then
        begin
            // Make sure signal states are set...
            if( ( _Cable <> nil ) and ( _Cable.Cable <> nil ) ) then
            begin
                if( _Cable.Cable.Get_Signal( 'DTR', B ) ) then
                begin
                    if( B ) then
                    begin
                        Status := Status or 2 ; // Transmit buffer empty
                        Status := Status and ( not 128 ) ; // Device is ready to be output to
                    end else
                    begin
                        Status := Status or 128 ; // Device is not ready for output
                    end ;
                end ;
                if( _Cable.Cable.Get_Signal( 'RTS', B ) ) then
                begin
                    if( B ) then
                    begin
                        Status := Status and ( not 1 ) ; // Device has input for us
                    end else
                    begin
                        Status := Status or 1 ; // Device is not ready for input
                    end ;
                end ;
            end ;
            _Buffer[ 0 ] := char( Status ) ;
        end else
        begin
            _Buffer[ 0 ] := char( Data ) ;
        end ;
    except
    end ;
    Examine := Set_Error( CEFSIOErr_Success ) ;
end ; { TS_IO.Examine }


function TS_IO.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    if( ( not Memory ) and ( SIO._Low <= Address ) and ( SIO._Low + 1 >= Address ) ) then
    begin
        Get_Access_Mode := Access_Mode ;
    end else
    begin
        Get_Access_Mode := Access_None ;
    end ;
end ;


function TS_IO.Get_Profiling : boolean ;

begin
    Get_Profiling := Profiling ;
end ;


function TS_IO.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	    Input_Component := nil ;
    end else
    begin
	    Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


const _Name : PChar = 'MITS 88-SIO'#0 ;

function TS_IO.Name : PChar ;

begin
    Name := _Name ;
end ;


function TS_IO.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	    Output_Component := nil ;
    end else
    begin
	    Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


function TS_IO.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

var Buffer : PChar ;
    Count, Loop : integer ;
    Watch : TCEF_Watchpoint ;

begin
    Read := False ;
    if( ( IO_Type = IO_Type_Bus ) and Send_Vector ) then
    begin
        Read := True ;
        Send_Vector := False ;
        Buffer := PChar( @Vector ) ;
        if( Default_Output <> nil ) then
        begin
            for Loop := 0 to Outputs.Count - 1 do
            begin
                try
                    TComponent( Outputs[ Loop ] ).Write_String( Address, Buffer, Size, IO_Type_Bus ) ;
                except
                end ;
            end ;
        end ;
        if( Default_Input <> nil ) then
        begin
            for Loop := 0 to Inputs.Count - 1 do
            begin
                if( Outputs.Indexof( Inputs[ Loop ] ) = -1 ) then
                begin
                    try
                        TComponent( Inputs[ Loop ] ).Write_String( Address, Buffer, Size, IO_Type_Bus ) ;
                    except
                    end ;
                end ;
            end ;
        end ;
        exit ;
    end ;
    if(
        ( Address >= SIO._Low )
        and
        ( Address <= SIO._Low + 1 )
        and
        ( IO_Type = IO_Type_IO )
      ) then
    begin
        if( ( Access_Mode and Access_Read ) = 0 ) then
        begin
            Read := False ; { CEFSIOErr_Access_Violation }
            exit ;
        end ;
        if( Size = 0 ) then
        begin
            Count := 1 ; // Default is 1 byte
        end else
        begin
            Count := ( Size + 7 ) div 8 ; // Round to byte
        end ;
	    Read := True ;
        Doing_Write := True ;
        try
            getmem( Buffer, Count ) ;
            try
                Examine( Address, Size, Buffer, True ) ;
                if( Default_Output <> nil ) then
                begin
                    for Loop := 0 to Outputs.Count - 1 do
                    begin
                        try
                            TComponent( Outputs[ Loop ] ).Write_String( Address, Buffer, Size, IO_Type_Bus ) ;
                        except
                        end ;
                    end ;
                end ;
                if( Default_Input <> nil ) then
                begin
                    for Loop := 0 to Inputs.Count - 1 do
                    begin
                        if( Outputs.Indexof( Inputs[ Loop ] ) = -1 ) then
                        begin
                            try
                                TComponent( Inputs[ Loop ] ).Write_String( Address, Buffer, Size, IO_Type_Bus ) ;
                            except
                            end ;
                        end ;
                    end ;
                end ;
            finally
                freemem( Buffer, Count ) ;
            end ;
        finally
            Doing_Write := False ;
        end ;
        if( Address = SIO._Low + 1 ) then
        begin
            Status := Status and $83 ; // Clear input waiting and error flags
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
end ; { TS_IO.Read }


function TS_IO.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

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


function TS_IO.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var S : longint ;

begin
    Result := nil ;
    S := sizeof( SIO._Low ) ;
    Stream.Read( SIO._Low, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
    S := sizeof( Profiling ) ;
    Stream.Read( Profiling, S ) ;
    if( S = 0 ) then
    begin
        exit ;
    end ;
end ;


function TS_IO.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

var Loop : int64 ;
    High : int64 ;
    S : longint ;
    Value : integer ;

begin { TS_IO.Save_Contents }
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
end ; { TS_IO.Save_Contents }


function TS_IO.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Stream.Write( SIO._Low, sizeof( SIO._Low ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Stream.Write( Profiling, sizeof( Profiling ) ) ;
    Result := Stream.Last_Error ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
end ;


function TS_IO.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    if( ( not Memory ) and ( SIO._Low <= Low ) and ( SIO._Low + 1 >= High ) ) then
    begin
        Access_Mode := Typ ;
    end ;
    Set_Access_Mode := Set_Error( CEFSIOErr_Success ) ;
end ;


procedure TS_IO.Set_Profiling( _On, Children : boolean ) ;

begin
    Profiling := _On ;
end ;


function TS_IO.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Set_Watchpoint := Set_Error( CEFSIOErr_Success ) ;
    if( ( not Memory ) and ( SIO._Low <= Address ) and ( SIO._Low + 1 >= Address ) ) then
    begin
        Watchpoint_List.Create_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TS_IO.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TS_IO.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var Watch : TCEF_Watchpoint ;

begin
    if( Doing_Write ) then // We must be here through singal ringing
    begin
	Write := Set_Error( CEFSIOErr_Success ) ;
        exit ;
    end ;
    if(
        ( Address >= SIO._Low )
        and
        ( Address <= SIO._Low + 1 )
        and
        ( IO_Type = IO_Type_IO )
      ) then
    begin
        if( ( Access_Mode and Access_Write ) = 0 ) then
        begin
            Write := Set_Error( CEFSIOErr_Access_Violation ) ;
            exit ;
        end ;
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
	    Write := Deposit( Address, Size, @Value, True ) ;
    end else
    begin
	    Write := Set_Error( CEFSIOErr_Success ) ;
    end ;
end ; // TS_IO.Write


constructor TS_IO_SIO.Create ;

begin
    inherited Create ;

    _Low := 0 ;
end ;


function TS_IO_SIO.Facility_Code : longint ;

begin
    Result := CEFSIO_Facility ;
end ;


function TS_IO_SIO.Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;

begin
    if( ( Low < 0 ) or ( Low > High ) ) then
    begin
	Set_Address_Range := Parent.Set_Error( CEFSIOErr_Invalid_Range ) ;
	exit ;
    end ;
    Set_Address_Range := Parent.Set_Error( CEFSIOErr_Success ) ;
    _Low := Low ;
end ;


procedure TS_IO_SIO.Get_Address_Range( var Low, High : int64 ) ;

begin
    Low := _Low ;
    High := _Low + 1 ;
end ;


procedure TS_IO.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TS_IO.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TS_IO.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TS_IO.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function Convert_Value( S : string ) : integer ;

begin
    Result := 0 ;
    S := Edit( S, -1 ) ;
    if( copy( S, length( S ), 1 ) = '.' ) then
    begin
        S := copy( S, 1, length( S ) - 1 ) ; // A decimal value
    end else
    begin
        S := CVTB( 16, 10, S ) ; // Convert from hexadecimal
    end ;
    try
        Result := strtoint( S ) ;
    except
    end ;
end ;


procedure TS_IO.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S, S1 : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'PORT' ) then
        begin
            S := uppercase( Parser.Token ) ;
            S1 := Parser.Token ;
            if( S1 = '.' ) then
            begin
                S := S + '.' ;
            end else
            begin
                Parser.Put_Token( S1 ) ;
            end ;
            SIO._Low := Convert_Value( S ) ;
        end else
        if( S = 'SEND_PARITY' ) then
        begin
            Send_Parity := True ;
        end else
        if( S = 'NOSEND_PARITY' ) then
        begin
            Send_Parity := False ;
        end else
        if( S = 'BAUD' ) then
        begin
            S := uppercase( Parser.Token ) ;
            _Baud := Convert_Value( S ) ;
        end else
        if( S = 'VECTOR' ) then
        begin
            S := uppercase( Parser.Token ) ;
            Vector := Convert_Value( S ) ;
        end else
        if( S = 'BITS' ) then
        begin
            S := uppercase( Parser.Token ) ;
            _Bits := Convert_Value( S ) ;
        end else
        if( S = 'STOP_BITS' ) then
        begin
            S := uppercase( Parser.Token ) ;
            _Stop_Bits := Convert_Value( S ) ;
        end else
        if( S = 'PARITY' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'ODD' ) then
            begin
                _Parity := 1 ;
            end else
            if( S = 'EVEN' ) then
            begin
                _Parity := 2 ;
            end else
            begin
                _Parity := 0 ; // Don't care
            end ;
        end else
        if( S = 'INTERRUPT' ) then
        begin
            S := uppercase( Parser.Token ) ;
            if( S = 'BOTH' ) then
            begin
                Allow_Input_Interrupt := True ;
                Allow_Output_Interrupt := True ;
            end else
            if( ( S = 'IN' ) or ( S = 'INPUT' ) ) then
            begin
                Allow_Input_Interrupt := True ;
                Allow_Output_Interrupt := False ;
            end else
            if( ( S = 'OUT' ) or ( S = 'OUTPUT' ) ) then
            begin
                Allow_Input_Interrupt := False ;
                Allow_Output_Interrupt := True ;
            end else
            begin
                Allow_Input_Interrupt := False ;
                Allow_Output_Interrupt := False ;
            end ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ; // TS_IO.Set_Up


procedure TS_IO.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

var B : boolean ;

begin
    if( Waiting_For_Interrupt and ( Index = 1 ) ) then // INTA
    begin
        _UI.Signal_Change_Notice( self, 0, False ) ; // Clear INT
        Waiting_For_Interrupt := False ;
        Send_Vector := True ;
    end ;
    if( ( Component = _Cable.Cable ) and ( _Cable.Cable <> nil ) ) then
    begin
        try
            if( _Cable.Cable.Get_Signal( 'DTR', B ) ) then
            begin
                if( B and ( ( Status and 128 ) = 0 ) ) then // Is active, and previously wasn't
                begin
                    Status := Status or 128 ;
                    if( ( Interrupts and 2 ) = 2 ) then // Interrupt on output ready
                    begin
                        if( Allow_Output_Interrupt ) then
                        begin
                            Waiting_For_Interrupt := True ;
                            _UI.Signal_Change_Notice( self, 0, True ) ; // Set INT
                        end ;
                    end ;
                end ;
            end ;
            if( _Cable.Cable.Get_Signal( 'RTS', B ) ) then
            begin
                if( B and ( ( Status and 1 ) = 0 ) ) then // Is active, and previously wasn't
                begin
                    Status := Status or 1 ;
                    if( ( Interrupts and 1 ) = 1 ) then // Interrupt on input ready
                    begin
                        if( Allow_Input_Interrupt ) then
                        begin
                            Waiting_For_Interrupt := True ;
                            _UI.Signal_Change_Notice( self, 0, True ) ; // Set INT
                        end ;
                    end ; // if( ( Interrupts and 1 ) = 1 )
                end ; // if( B and ( ( Status and 1 ) = 0 ) )
            end ; // if( _Cable.Cable.Get_Signal( 'RTS', B ) )
        except
        end ;
    end ; // if( ( Component = _Cable.Cable ) and ( _Cable.Cable <> nil ) )
end ; // TS_IO.Signal_Change_Notice



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
    Test : TS_IO ;
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
    Test := TS_IO.Create ;
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

