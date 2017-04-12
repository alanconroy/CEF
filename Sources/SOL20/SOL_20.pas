{$N+}
{
        Program Name : SOL_20
        Package Name : CEF
        Purpose      : Processor Technology SOL-20 CEF component
        Institution  : 
        Date Written : 10-Apr-2006
        Written By   : Alan Conroy
        Version      : 1.0A

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

         20-Jan-2007    EAC         Handle shutdown better.

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	  This unit implements the Processor Technology SOL-20 component for
        CEF.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit SOL_20 ;

interface

{$I EDEFINES.INC}

uses // C&C...
     Compatib,
     Classes,
     Collect, { TCollection }
     _DebugIn, // TDebug_Interface
     DebugInt, // TText_Debugger
     _Streams, // TCOM_Stream
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TMemory
     CEF, { TBase_Component }
     _CEFUtil, // TCEF_Watchpoint

     // SOL20...
     SOL20_Main ; // TMain_Form

const SOL20_Facility = 16 ;
      SOL20_Success = 0 ;
      SOL20_Invalid_Range = 1 ;
      SOL20_Component_Not_Found = 2 ;
      SOL20_Address_Out_Of_Range = 3 ;
      SOL20_Invalid_Component = 4 ;
      SOL20_No_Matching_Watchpoint = 5 ;
      SOL20_Access_Violation = 6 ;

type TSOL20_System = class ;

     TSOL20 = class( TBase_Component )
                  private // Instance data...
                      SOL20 : TSOL20_System ;
                      Inputs, Outputs : TList ;
                      _Parent : TComponent ;
                      _Tag : longint ;
                      Read_Latency, Write_Latency : longint ;
                      Temp_Port_Description : string ;
                      Temp_Port_Name : string ;

                  private // Internal utility routines...
                      function Default_Input : TComponent ;
                      function Default_Output : TComponent ;

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

                      function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                          Memory : boolean ) : TUnified_Exception ; override ;

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

                      procedure Set_Read_Latency( Value : longint ) ; override ;

                      function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                          Access : longint ) : TUnified_Exception ; override ;

                      procedure Set_Write_Latency( Value : longint ) ; override ;

                      procedure Show_Status ; override ;

                      function Write( Address : int64 ; Value, Size : longint ;
                          IO_Type : longint ) : TUnified_Exception ; override ;

                      procedure Set_Tag( Value : longint ) ; override ;

                      function Get_Tag : longint ; override ;

                      function Get_Parent : TComponent ; override ;

                      procedure Set_Parent( Component : TComponent ) ; override ;

                      procedure Set_Up( P : PChar ) ; override ;

                      procedure Child_Notification( Child : TComponent ;
                          var Notice : longint ; var Params : int64 ) ;
                          override ;

                      function Get_Port_Name( Index : longint ) : PChar ;
                          override ;

                      function Get_Port_Description( Index : longint ) : PChar ;
                          override ;

                      function Get_Port( Index : longint ) : TComponent ;
                          override ;

                      function Get_Port_Connection( Index : longint ) : TComponent ;
                          override ;
              end ;

     TSOL20_System = class( TBase_Memory )
	    public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private // Instance data...
	        Access_Mode : integer ;
	        Profiling : boolean ; { True if profiling memory accesses }
            Watchpoint_List : TCEF_Watchpoint_Manager ;
            Data : array[ 0..$FFFF ] of byte ;
            Main_Form : TMain_Form ;

        public // Serial port UART status...
            UART_Data_Ready : boolean ; // True if input pending from serial port
            UART_Data : byte ; // Input waiting from serial port
            UART_Overrun : boolean ; // True if a serial port data overrun
            UART_Baud_Mismatch : boolean ; // Received data with a baud mismatch

        public // Other public data...
            Parent : TComponent ;
            _UI : TUI_Interface ;

        private // Internal utility routines...
	        procedure Clear_Memory ;

        protected
            function Translate_Error( Code : longint ) : string ; override ;

        public { API... }
            function Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;
                override ;

            procedure Get_Address_Range( var Low, High : int64 ) ; override ;

            procedure Dump( Start, Size : int64 ; Buffer : pointer ) ;
                override ;

            procedure Load( Start, Size : int64 ; Buffer : pointer ) ;
                override ;
     end ;

var Screen : TComponent = nil ;
    SP_Data_Bits : byte ; // Number of data bits for serial port
    SP_Baud : integer ; // Serial port baud rate
    Tape1_Filename : string = '' ;
    Tape2_Filename : string = '' ;
    Tape1_File : file ;
    Tape2_File : file ;
    Selected_Tape : byte ; // 1 or 2

{$IFDEF Test}
procedure Test_Unit ;
{$ENDIF}

implementation

uses // Borland...
     Windows, // MessageBeep
     Forms, // Application
     Sysutils, // Allocmem

     // C&C...
     CommonUt, // Edit
     CVT, // CVTB
     Num1s,
     Parse ; // TString_Parser

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ; external 'CEF_Util.dll' ;

{ Internal unit utility routines... }

function Get_Mem( Size : integer ) : pointer ;

var Res : pointer ;

begin
    Res := nil ;
    try
        getmem( Res, Size ) ;
    except
    end ;
    Get_Mem := Res ;
end ;



{ TSOL20_Debugger methods... }

type TSOL20_Debugger = class( TText_Debugger )
                                private
                                    _Memory : TSOL20 ;

                                public { API... }
                                    function Child( Ordinal : longint ) : PDebug_Interface ;
                                        override ;

                                    function Count : longint ; override ;

                                    property Memory : TSOL20
                                                 read _Memory
                                                 write _Memory ;
                            end ;

function TSOL20_Debugger.Child( Ordinal : longint ) : PDebug_Interface ;

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
        0 : I.Title := PChar( 'Access_Mode = ' + Access_Mode_To_String( Memory.SOL20.Access_Mode ) ) ;
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
        3 : I.Title := PChar( 'Profiling = ' + Boolean_To_String( Memory.SOL20.Profiling ) ) ;
        4 : I.Title := PChar( 'Read_Latency = ' + Num1( Memory.Read_Latency ) ) ;
        5 : I.Title := PChar( '_Serial_Number = ' + Num1( Memory._Serial_Number ) ) ;
        6 : begin
                if( Memory.SOL20._UI = nil ) then
                begin
                    I.Title := '_UI = nil' ;
                end else
                begin
                    I1 := Memory.SOL20._UI.Debugger ;
                    if( I1 <> nil ) then
                    begin
                        I.Free ;
                        I1.Set_Title( PChar( '_UI = ' + Pointer_To_String( Memory.SOL20._UI ) ) ) ;
                        Result := PDebug_Interface( I1 ) ;
                        exit ;
                    end ;
                    I.Title := PChar( '_UI = ' + Pointer_To_String( Memory.SOL20._UI ) ) ;
                end ;
            end ;
        7 : I.Title := Pchar( 'Watchpoint_List = ' + Pointer_To_String( pointer( Memory.SOL20.Watchpoint_List ) ) ) ;
        8 : I.Title := PChar( 'Write_Latency = ' + Num1( Memory.Write_Latency ) ) ;
    end ;
    Result := PDebug_Interface( I ) ;
end ;


function TSOL20_Debugger.Count : longint ;

begin
    Result := 9 ;
end ;



// TSOL20_System methods...

// Constructors and destructors...

constructor TSOL20_System.Create ;

begin
    inherited Create ;

    Profiling := False ;
    Watchpoint_List := Get_Watchpoint_Manager ;
    Access_Mode := Access_RW or Access_Execute ;
end ;


destructor TSOL20_System.Destroy ;

begin
    Clear_Memory ;
    Watchpoint_List.Terminate ;
    Watchpoint_List := nil ;
    Main_Form.Close ;
    Main_Form := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TSOL20_System.Translate_Error( Code : longint ) : string ;

var _Error : string ;

begin
    case Code of
        SOL20_Success: _Error := 'Success' ;
        SOL20_Invalid_Range : _Error := 'Invalid range' ;
        SOL20_Component_Not_Found : _Error := 'Component not found' ;
        SOL20_Address_Out_Of_Range : _Error := 'Address out of range' ;
        SOL20_Invalid_Component : _Error := 'Invalid component' ;
        SOL20_No_Matching_Watchpoint : _Error := 'No Matching Watchpoint' ;
        SOL20_Access_Violation : _Error := 'Access Violation' ;
	    else _Error := 'Unknown error number ' + Num1( Code ) ;
    end ;
    Translate_Error := _Error ;
end ; { TSOL20_System.Translate_Error }


procedure TSOL20_System.Clear_Memory ;

begin
    fillchar( Data, sizeof( Data ), 0 ) ;
end ;


function TSOL20_System.Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
end ;


procedure TSOL20_System.Get_Address_Range( var Low, High : int64 ) ;

begin
    Low := $CC00 ;
    High := $CFFF ;
end ;


procedure TSOL20_System.Dump( Start, Size : int64 ; Buffer : pointer ) ;

begin
end ;


procedure TSOL20_System.Load( Start, Size : int64 ; Buffer : pointer ) ;

begin
end ;



// TSOL20 methods...

function TSOL20.Default_Input : TComponent ;

begin
    if( Inputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Inputs[ 0 ] ) ;
    end ;
end ;


function TSOL20.Default_Output : TComponent ;

begin
    if( Outputs.Count = 0 ) then
    begin
        Result := nil ;
    end else
    begin
        Result := TComponent( Outputs[ 0 ] ) ;
    end ;
end ;


{ API... }

function TSOL20.Memory : TMemory ;

begin
    Result := SOL20 ;
end ;


function TSOL20.Facility_Code : longint ;

begin
    Facility_Code := SOL20_Facility ;
end ;


function TSOL20.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

var C : TComponent ;

begin
    { General setup... }
    Read_Latency := 0 ; { Infinitely fast memory :) }
    Write_Latency := 0 ;
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    SOL20 := TSOL20_System.Create ;
    SOL20._UI := UI ;
    SOL20.Parent := self ;

    SOL20_Main._UI := UI ;
    Application.CreateForm( TMain_Form, SOL20.Main_Form ) ;
    SOL20.Main_Form.Visible := True ;
    SOL20.Main_Form._SOL20 := SOL20 ;
    C := UI.Load_Component( 'Serial' ) ;
    if( C <> nil ) then
    begin
        SOL20.Main_Form.Serial_Port := C ;
        SOL20.Main_Form.Serial_Port.Parent := self ;
    end ;

    C := UI.Load_Component( 'Serial' ) ;
    if( C <> nil ) then
    begin
        SOL20.Main_Form.Parallel_Port := C ;
        SOL20.Main_Form.Parallel_Port.Connect_Output( self ) ;
    end ;

    Initialize := SOL20.Set_Error( SOL20_Success ) ;
end ; { TSOL20.Initialize }


function TSOL20.Terminate : TUnified_Exception ;

begin
    if( SOL20._UI <> nil ) then
    begin
        SOL20._UI.Termination_Notice( self ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    Terminate := SOL20.Set_Error( SOL20_Success ) ;
    SOL20.Free ;
    SOL20 := nil ;
    Free ;
end ; { TSOL20.Terminate }



function TSOL20.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Result := Screen.Clear_Watchpoint( Address, Memory, Access ) ;
end ;


function TSOL20.Component_Type : longint ;

begin
    Component_Type := Component_Type_Memory ;
end ;


function TSOL20.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Input := SOL20.Set_Error( SOL20_Invalid_Component ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Connect_Input := SOL20.Set_Error( SOL20_Success ) ;
end ;


function TSOL20.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Connect_Output := SOL20.Set_Error( SOL20_Invalid_Component ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Connect_Output := SOL20.Set_Error( SOL20_Success ) ;
end ;


function TSOL20.Debugger : TDebug_Interface ;

begin
    Result := TSOL20_Debugger.Create ;
    TSOL20_Debugger( Result ).Memory := Self ;
end ;


function TSOL20.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    Result := Screen.Deposit( Address, Size, Buffer, Memory ) ;
end ;


function TSOL20.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Inputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Input := SOL20.Set_Error( SOL20_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Input := SOL20.Set_Error( SOL20_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TSOL20.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Component = nil ) or ( Outputs.IndexOf( Component ) = -1 ) ) then
    begin
	Disconnect_Output := SOL20.Set_Error( SOL20_Component_Not_Found ) ;
    end else
    begin
	Disconnect_Output := SOL20.Set_Error( SOL20_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TSOL20.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    Result := Screen.Examine( Address, Size, Buffer, Memory ) ;
end ; 


function TSOL20.Get_Access_Mode( Address : int64 ;
            Memory : boolean ) : longint ;

begin
    Result := Screen.Get_Access_Mode( Address, Memory ) ;
end ;


function TSOL20.Get_Profiling : boolean ;

begin
    Get_Profiling := SOL20.Profiling ;
end ;


function TSOL20.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := Read_Latency ;
end ;


function TSOL20.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := Write_Latency ;
end ;


function TSOL20.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Input_Component := nil ;
    end else
    begin
	Input_Component := TComponent( Inputs[ Index ] ) ;
    end ;
end ;


const _Name : PChar = 'SOL-20'#0 ;

function TSOL20.Name : PChar ;

begin
    Name := _Name ;
end ;


function TSOL20.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
	Output_Component := nil ;
    end else
    begin
	Output_Component := TComponent( Outputs[ Index ] ) ;
    end ;
end ;


function TSOL20.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

var Data : byte ;
    UEC : TUnified_Exception ;

    procedure Respond ;

    var Loop : integer ;

    begin
	if( Default_Output <> nil ) then
	begin
	    for Loop := 0 to Outputs.Count - 1 do
            begin
                TComponent( Outputs[ Loop ] ).Write( Address, Data, 8, IO_Type ) ;
            end ;
	end ;
	if( Default_Input <> nil ) then
	begin
	    for Loop := 0 to Inputs.Count - 1 do
            begin
                try
                    TComponent( Inputs[ Loop ] ).Write( Address, Data, 8, IO_Type ) ;
                except
                end ;
            end ;
	end ;
    end ;

var B : boolean ;
    Loop : integer ;

begin
    Result := False ;
    if( ( IO_Type = IO_Type_IO ) and ( Address >= $F8 ) and ( Address <= $FF ) ) then
    begin
        Data := 0 ;
        case Address of
            $F8 : // Status, serial comm channel
                  begin
                      if( SOL20.Main_Form.Serial_Port <> nil ) then
                      begin
                          if( SOL20.Main_Form.Serial_Port.Get_Signal( 'CD', B ) ) then
                          begin
                              if( B ) then
                              begin
                                  Data := Data or 1 ;
                              end ;
                          end ;
                          if( SOL20.Main_Form.Serial_Port.Get_Signal( 'DSR', B ) ) then
                          begin
                              if( B ) then
                              begin
                                  Data := Data or 2 ;
                              end ;
                          end ;
                          if( SOL20.Main_Form.Serial_Port.Get_Signal( 'CTS', B ) ) then
                          begin
                              if( B ) then
                              begin
                                  Data := Data or 32 ;
                              end ;
                          end ;
                          Data := Data or 128 ; // UART serial transmit buffer always empty
                      end ; // if( Serial_Port <> nil )
                      if( SOL20.UART_Data_Ready ) then
                      begin
                          Data := Data or 64 ;
                      end ;
                      if( SOL20.UART_Overrun ) then
                      begin
                          Data := Data or 16 ;
                      end ;
                      if( SOL20.UART_Baud_Mismatch ) then
                      begin
                          Data := Data or 8 ; // Framing error
                      end ;
                  end ;
{
                    and 4 = SPE - serial parity error                  1 = error
}
            $F9 : // Serial comm channel data
                  begin
                      SOL20.UART_Data_Ready := False ;
                      Data := SOL20.UART_Data ;
                      SOL20.UART_Overrun := False ;
                  end ;
            $FA : // Aux status, cassette tape interface, parallel I/O, keyboard input
{
                    and 2 = PDR - parallel data ready                  0 = ready
                    and 4 = PXDR - parallel external device ready      0 = ready
                    and 8 = TFE - Tape framing error                   1 = error
                    and 16 = TOE - Tape overrun error                  1 = error
                    and 32 = not used
}
                  begin
                      Data := 128 ; // Tape transmit buffer always empty
                      if( SOL20.Main_Form.Buffer.Text = '' ) then
                      begin
                          Data := Data or 1 ; // KDR - Keyboard data ready (0 = ready)
                      end ;
                      if(
                          ( Tape1_Filename <> '' )
                          and
                          ( filepos( Tape1_File ) < filesize( Tape1_File ) )
                        ) then
                      begin
                          Data := Data or 64 ; // Tape data ready
                      end ;
                  end ;
            $FB : // Audio cassette data
                  begin
                      if(
                          ( Tape1_Filename <> '' )
                          and
                          ( filepos( Tape1_File ) < filesize( Tape1_File ) )
                        ) then
                      begin
                          {$I-}
                          blockread( Tape1_File, Data, 1 ) ;
                          {$I+}
                          IOResult ;
                      end ;
                  end ;
            $FC : // Keyboard data
                  begin
                      if( SOL20.Main_Form.Buffer.Text <> '' ) then
                      begin
                          Data := ord( SOL20.Main_Form.Buffer.Text[ 1 ] ) ;
                          SOL20.Main_Form.Buffer.Text :=
                              copy( SOL20.Main_Form.Buffer.Text, 2, length( SOL20.Main_Form.Buffer.Text ) ) ;
                      end ;
                  end ;
            $FD : ; // Parallel port data
            $FE : Data := 0 ; // Display status
                  // and 1 = SOK - Scroll OK                   0 = time complete
            $FF : // Sense switch
                  begin
                      for Loop := 7 downto 0 do
                      begin
                          if( SOL20.Main_Form.Sense_Switches[ Loop ].Down ) then
                          begin
                              Data := Data or 1 ;
                          end ;
                          Data := Data shl 1 ;
                      end ;
                  end ;
        end ; // case Address
        Respond ;
        Result := True ;
    end else
    if( IO_Type = IO_Type_Memory ) then
    begin
        Size := ( SIze + 7 ) div 8 ;
        while( Size > 0 ) do
        begin
            Loop := 8 ;
            UEC := Screen.Examine( Address, Loop, @Data, True ) ;
            Result := ( UEC = nil ) ;
            if( Result ) then
            begin
                Respond ;
            end ;
            dec( Size ) ;
            Address := Address + 1 ;
        end ;
    end ;
end ; { TSOL20.Read }


function TSOL20.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Screen.Restore_Contents( Stream ) ;
end ;


function TSOL20.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Screen.Restore_State( Stream ) ;
end ;


function TSOL20.Save_Contents( Stream : TCOM_Stream ): TUnified_Exception ;

begin
    Result := Screen.Save_Contents( Stream ) ;
end ;


function TSOL20.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Screen.Save_State( Stream ) ;
end ;


function TSOL20.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    Result := Screen.Set_Access_Mode( Low, High, Memory, Typ ) ;
end ;


procedure TSOL20.Set_Profiling( _On, Children : boolean ) ;

begin
    SOL20.Profiling := _On ;
end ;


procedure TSOL20.Set_Read_Latency( Value : longint ) ;

begin
    Read_Latency := Value ;
end ;


function TSOL20.Set_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ;

begin
    Result := Screen.Set_Watchpoint( Address, Memory, Access ) ;
end ;


procedure TSOL20.Set_Write_Latency( Value : longint ) ;

begin
    Write_Latency := Value ;
end ;


procedure TSOL20.Show_Status ;

begin
    { This routine intentionally left blank - no status to show }
end ;


function TSOL20.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

var Pos : longint ;

begin
    if( ( IO_Type = IO_Type_IO ) and ( Address >= $F8 ) and ( Address <= $FE ) ) then
    begin
        case Address of
            $F8 : // Serial comm channel control
                  begin
                      if( SOL20.Main_Form.Serial_Port <> nil ) then
                      begin
                          SOL20.Main_Form.Serial_Port.Set_Signal( 'RTS', ( ( Value and 16 ) = 16 ) ) ;
                      end ;
                  end ;
            $F9 : // Serial comm channel data
                  begin
                      SOL20.Main_Form.Write_Serial( Value ) ;
                  end ;
            $FA : // Control, parallel I/O, cassette I/O
                  begin
                      Selected_Tape := 0 ; // No tape transport running
                      if( ( Value and 64 ) = 0 ) then
                      begin
                          Selected_Tape := 2 ; // Run tape tansport 2
                      end ;
                      if( ( Value and 128 ) = 0 ) then
                      begin
                          Selected_Tape := 1 ; // Run tape transport 1
                      end ;
                  end ;
{
                    and 8 = PIE - Parallel input enable
                    and 16 = PUS - parallel unit select
                    and 32 = TBR - Tape baud rate (300/1200)    0 = 1200
}
            $FB : // Audio cassette data
                  begin
                      if( Selected_Tape = 1 ) then
                      begin
                          if( Tape1_Filename <> '' ) then
                          begin
                              Pos := filepos( Tape1_File ) ;
                              {$I-}
                              blockwrite( Tape1_File, Value, 1 ) ;
                              {$I+}
                              IOResult ;
                              {$I-}
                              seek( Tape1_File, Pos + 1 ) ;
                              {$I+}
                              IOResult ;
                          end ;
                      end else
                      if( Selected_Tape = 2 ) then
                      begin
                          if( Tape2_Filename <> '' ) then
                          begin
                              Pos := filepos( Tape2_File ) ;
                              {$I-}
                              blockwrite( Tape2_File, Value, 1 ) ;
                              {$I+}
                              IOResult ;
                              {$I-}
                              seek( Tape2_File, Pos + 1 ) ;
                              {$I+}
                              IOResult ;
                          end ;
                      end ;
                  end ;
            $FC : Messagebeep( 0 ) ; // Alarm
            $FD : // Parallel output data channel
                  begin
                      SOL20.Main_Form.Write_Parallel( Value ) ;
                  end ;
            $FE : // Scroll control, display section
                  begin
                      Screen.Set_Up( PChar( 'TOP ' + inttostr( ( Value shr 4 ) and 15 ) ) ) ;
{
                      and 15 = BDLA = Beginning display line absolute address
                      shr 4 = FDSP = First displayed line screen position
}
                  end ;
        end ;
        fillchar( Result, sizeof( Result ), 0 ) ;
    end else
    begin
        Result := Screen.Write( Address, Value, Size, IO_Type ) ;
    end ;
end ; // TSOL20.Write


procedure TSOL20.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TSOL20.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TSOL20.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TSOL20.Set_Parent( Component : TComponent ) ;

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


procedure TSOL20.Set_Up( P : PChar ) ;

begin
end ; 


procedure TSOL20.Child_Notification( Child : TComponent ;
    var Notice : longint ; var Params : int64 ) ;

var Speed : int64 ;
    Value, Data_Size, Stop_Bits : integer ;

begin
    if( ( Notice = Child_Notice_Receive_Data ) and ( Child = SOL20.Main_Form.Serial_Port ) ) then
    begin
        SOL20.Main_Form.Serial_Port.Cable.Get_Data( Speed, Value, Data_Size, Stop_Bits ) ;
        SOL20.UART_Data := Value ;
        if( SOL20.UART_Data_Ready ) then
        begin
            SOL20.UART_Overrun := True ;
        end ;
        SOL20.UART_Data_Ready := True ;
    end else
    if( Notice = Child_Notice_Connect ) then
    begin
        if( Child = SOL20.Main_Form.Serial_Port ) then
        begin
            SOL20.Main_Form.Serial_Component := pointer( Params ) ;
        end else
        if( Child = SOL20.Main_Form.Parallel_Port ) then
        begin
            SOL20.Main_Form.Parallel_Component := pointer( Params ) ;
        end ;
    end else
    if( Notice = Child_Notice_Disconnect ) then
    begin
        if( Child = SOL20.Main_Form.Serial_Port ) then
        begin
            SOL20.Main_Form.Serial_Component := nil ;
        end else
        if( Child = SOL20.Main_Form.Parallel_Port ) then
        begin
            SOL20.Main_Form.Parallel_Component := nil ;
        end ;
    end ;
end ;


function TSOL20.Get_Port_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : Temp_Port_Name := 'Serial port' ;
        1 : Temp_Port_Name := 'Parallel port' ;
        else exit ;
    end ;
    Result := PChar( Temp_Port_Name ) ;
end ;


function TSOL20.Get_Port_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : Temp_Port_Description := 'RS-232' ;
        1 : Temp_Port_Description := 'Parallel' ;
        else exit ;
    end ;
    Result := PChar( Temp_Port_Description ) ;
end ;


function TSOL20.Get_Port( Index : longint ) : TComponent ;

begin
    Result := nil ;
    case Index of
        0 : Result := SOL20.Main_Form.Serial_Port ;
        1 : Result := SOL20.Main_Form.Parallel_Port ;
        else exit ;
    end ;
end ;


function TSOL20.Get_Port_Connection( Index : longint ) : TComponent ;

begin
    Result := nil ;
    case Index of
        0 : Result := SOL20.Main_Form.Serial_Component ;
        1 : Result := SOL20.Main_Form.Parallel_Component ;
        else exit ;
    end ;
end ;



initialization
{$IFDEF Test}
    Test_Unit ;
{$ENDIF}
end.

