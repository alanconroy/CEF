{$N+}
{
        Program Name : _CEF
        Package Name : CEF
        Purpose      : CEF definitions
        Institution  :
        Date Written : 27-Apr-2000
        Written By   : Alan Conroy
        Version      : 3.0

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

          This unit contains all of the CEF type definitions in a format for
        Delphi.
}

unit _CEF ;

interface

uses // C&C Subroutine Library...
     _DebugIn, // TDebug_Interface
     _Streams, // TCOM_Stream
     _UE, // TUnified_Exception
     TypeDefs ; // TTri_State

      // Constants...
const Base_Interface_Version = 30 ; // 3.0
      Interface_Version = 30 ; // 3.0

      Severity_Information = 0 ; { Informational message }
      Severity_Warning = 1 ; { Warning }
      Severity_Error = 2 ; { Non-fatal error }
      Severity_Fatal = 3 ; { Fatal error.  The application should shut down. }

      Access_None = 0 ; { Not supported }
      Access_Read = 1 ; { Read (input) }
      Access_Input = Access_Read ;
      Access_Write = 2 ; { Write (output) }
      Access_Output = Access_Write ;
      Access_RW = Access_Read or Access_Write ; { Read or write (input or output) }
      Access_IO = Access_RW ;
      Access_Execute = 4 ; { Execution }
      Access_All = Access_RW or Access_Execute ;

      Component_Type_Unknown = 0 ;
      Component_Type_CPU = 1 ;
      Component_Type_Bus = 2 ;
      Component_Type_Memory = 3 ;
      Component_Type_Motherboard = 4 ;
      Component_Type_IO = 5 ;
      Component_Type_UI = 6 ;
      Component_Type_Cable = 7 ;
      Component_Type_Keyboard = 8 ;

      Segment_Default_Code = 0 ;
      Segment_Default_Data = 1 ;

      IO_Type_Memory = 0 ; // Memory read/write
      IO_Type_IO = 1 ; // I/O read/write
      IO_Type_Bus =  2 ; // Bus state read/write
      // Note: all other IO types are specific to components

      { Child_Notice codes... }
      Child_Notice_Terminating = 0 ; // Child is about to terminate
      Child_Notice_Request_Terminate = 1 ; // Child is requesting termination.  If notice is set to 0 on return, the termination is disallowed
      Child_Notice_Receive_Data = 2 ; // Child has received asynchronus data
      Child_Notice_Connect = 3 ; // Successful connection of child to a component (Params2)
      Child_Notice_Disconnect = 4 ; // Successful disconnection of child from a component (Params2)

      { TUI_Interface notice codes... }
      UI_Notice_Changed_Run_State = 0 ; // Run state is about to change.  Data = 0 (pause) or 1 (run).
      UI_Notice_Request_Changed_Run_State = 1 ; // Run state is about to change.  Data = 0 (pause) or 1 (run).  Set Data to 2 to prevent.

      // Listing output codes...
      ListOut_Source = 0 ;
      ListOut_Generated_Data = 1 ;
      ListOut_New_Line = 2 ;
      ListOut_New_Page = 3 ;
      ListOut_Title_Text = 4 ;
      ListOut_Paging = 5 ;
      ListOut_No_Paging = 6 ;
      ListOut_Table = 7 ;
      ListOut_Message = 8 ;
      ListOut_SubTitle = 9 ;

      // Master Clock Modes...
      MCM_Default = 0 ; // (Default)  Unblock components in order of time
      MCM_Ignore = 1 ; // Immediately unblock calling components
      MCM_Synchronize = 2 ; // Unblock components in order of time, but synchronize emulator time to real system time

{ Error codes... }
const { TStreamer error codes... }
      StreamerErr_Facility = -1 ;
      StreamerErr_Success = 0 ;
      StreamerErr_No_Stream = 1 ; { No stream is open }

      { TComponent error codes... }
      ComponentErr_Facility = -1 ;
      ComponentErr_Success = 0 ;
      ComponentErr_No_Watchpoints = 1 ;
      ComponentErr_Invalid_Operation = 2 ;
      ComponentErr_Component_Not_Found = 3 ;
      ComponentErr_Address_Out_Of_Range = 4 ;

      { TMemory error codes... }
      MemoryErr_Facility = -1 ;
      MemoryErr_Success = 0 ;

      { TMaster_Assembler error codes... }
      MasterAssemblerErr_Facility = -1 ;
      MasterAssemblerErr_Success = 0 ;
      MasterAssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
      MasterAssemblerErr_Illegal_Expression = 2 ; // Illegal expression
      MasterAssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
      MasterAssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol

      { TAssembler error codes... }
      AssemblerErr_Facility = -1 ;
      AssemblerErr_Success = 0 ;

      { TCPU error codes... }
      CPUErr_Facility = -1 ;
      CPUErr_Success = 0 ;
      CPUErr_No_Breakpoint = 1 ;

      // Emulation flags...
const RTS_Want_Nothing = 0 ;
      RTS_Want_Calls = 1 ;
      RTS_Want_Jumps = 2 ;
      RTS_Want_Registers = 4 ;
      RTS_Want_Interrupts = 8 ;
      RTS_Want_Returns = 16 ;

      // Symbol flags...
      SF_Constant = 1 ; // Constant (otherwise a variable)
      SF_Big_Endian = 2 ; // Symbol is big-endian
      SF_Label = 4 ; // Label
      SF_Global = 8 ; // Global symbol (local otherwise)
      SF_External = 16 ; // External symbol (not used by CEF)

      // Data type flags...
        // Families...
      DataType_Family_Undefined = 0 ;
      DataType_Family_Logical = 1 ;
      DataType_Family_String = 2 ;
      DataType_Family_Numeric = 3 ;
      DataType_Family_Procedure = 4 ;
      // Families 5-7 are reserved for future use

        // Types...
      DataType_Boolean = DataType_Family_Logical + 0 ;

      DataType_String = DataType_Family_String + 0 ;

      DataType_Integer = DataType_Family_Numeric + 0 ;
      DataType_Real = DataType_Family_Numeric + ( 1 shl 3 ) ;
      DataType_BCD = DataType_Family_Numeric + ( 2 shl 3 ) ;

        // String length encodings...
      Datatype_String_Length_Other = 0 ; // Fixed or otherwise unknown length encoding
      Datatype_String_Length_Terminated = 1 ; // String is terminated by a null
      Datatype_String_Length_Prefix = 2 ; // String is prefixed with length

        // String Encoding...
      Datatype_String_Encoding_Unknown = 0 ; // Undefined
      Datatype_String_Encoding_ASCII = 1 ; // 7-bit ASCII (8th-bit ignored)
      Datatype_String_Encoding_EBCDIC = 2 ; // EBCDIC
      Datatype_String_Encoding_Radix50 = 3 ; // Radix-50
      Datatype_String_Encoding_UTF8 = 4 ; // 8-bit UNICODE (utf8)
      Datatype_String_Encoding_Unicode16 = 5 ; // 16-bit UNICODE
      Datatype_String_Encoding_Unicode32 = 6 ; // 32-bit UNICODE

     // Helper structures and classes...
type TData_Type = class
                      public // API...
                          // General...
                          function Family : longint ;
                              virtual ; stdcall ; abstract ;
                          function Data_Type : longint ;
                              virtual ; stdcall ; abstract ;
                          function Size : longint ; // Total current size in bits
                              virtual ; stdcall ; abstract ;
                          function Big_Endian : boolean ; // True if big-endian
                              virtual ; stdcall ; abstract ;
                          function Max_Size : longint ; // Maximum size in bits
                              virtual ; stdcall ; abstract ;

                          // Numeric types...
                          function Signed : TTri_State ; // True if signed, False if unsigned, Dont_Care if either/neither
                              virtual ; stdcall ; abstract ;
                          function Mantissa : longint ; // Number of bits in mantissa (including sign, if signed)
                              virtual ; stdcall ; abstract ;
                          function Exponent : longint ; // Number of bits in exponent (DataType_Real only)
                              virtual ; stdcall ; abstract ;
                          function Fixed : boolean ; // True if fixed point number (DataType_Real and Data_Type_BCD only) or fixed-length string
                              virtual ; stdcall ; abstract ;
                          function Fixed_Position : longint ; // If fixed, this is the position of the decimal place (eg 3 means the 3rd digit is the first digit of the fractional part)
                              virtual ; stdcall ; abstract ;
                          function Pack : boolean ; // True if packed (DataType_BCD only)
                              virtual ; stdcall ; abstract ;

                          // String types...
                          function Length_Encoding : longint ; // See Datatype_String_Length
                              virtual ; stdcall ; abstract ;
                          function Prefix_Size : longint ; // Number of bits of length for strings with length prefix
                              virtual ; stdcall ; abstract ;
                          function Encoding : longint ; // see Datatype_String_Encoding
                              virtual ; stdcall ; abstract ;
                  end ;

     TSymbol_Record = record
        Address : int64 ; { Address of identifier's storage within segment. }
        Segment : longint ; { Segment for identifier. }
        Size : longint ; { Size of identifier's data, in bits. }
        Typ : longint ; { Data type of identifier:
                                    0 = Undefined
                                    1 = Integer
                                    2 = IEEE double-precision floating point
                                    3 = string
                                    4 = boolean
                                    5 = character
                                    other = other }
        Flags :	longint ; { Flags for identifier: see SF_*}
        Data : int64 ;	{ Actual value of identifier, if < 65 bits }
        DataP :	pointer ; { Pointer to value, if > 64 bits }
        Context : longint ;
        Line : longint ;
        Filename : PChar ;
    end ; // TSymbol_Record
    PSymbol_Record = ^TSymbol_Record ;

    TCEF_Stack_Interface = class
                               public // API...
                                   procedure Terminate ; // Destruct the object
                                       virtual ; stdcall ; abstract ;

                                   // Lowest possible stack entry address...
                                   function Low_Bound : int64 ;
                                       virtual ; stdcall ; abstract ;

                                   // Highest possible stack entry address...
                                   function High_Bound : int64 ;
                                       virtual ; stdcall ; abstract ;

                                   // Size of each stack entry, in bits
                                   function Item_Size : longint ;
                                       virtual ; stdcall ; abstract ;

                                   // Returns Indexth item in stack
                                   function Value( Index : int64 ) : int64 ;
                                       virtual ; stdcall ; abstract ;

                                   // V2.6...

                                   // True if the stack grows up, false if it grows down
                                   function Grow_Up : boolean ;
                                       virtual ; stdcall ; abstract ;
                           end ;

    TAssembler_Status = class
                            public
                                // Returns name of source file
                                function Filename : PChar ;
                                    virtual ; stdcall ; abstract ;

                                function Get_Aborted : boolean ;
                                    virtual ; stdcall ; abstract ;
                                function Get_Code : int64 ;
                                    virtual ; stdcall ; abstract ;
                                function Get_Data : int64 ;
                                    virtual ; stdcall ; abstract ;
                                function Get_Errors : longint ;
                                    virtual ; stdcall ; abstract ;
                                function Get_Warnings : longint ;
                                    virtual ; stdcall ; abstract ;

                                // Declining feature.  Use Get_Error instead.
                                function Get_Error_Text : PChar ;
                                    virtual ; stdcall ; abstract ;

                                procedure Set_Aborted( Value : boolean ) ;
                                    virtual ; stdcall ; abstract ;
                                procedure Set_Code( Value : int64 ) ;
                                    virtual ; stdcall ; abstract ;
                                procedure Set_Data( Value : int64 ) ;
                                    virtual ; stdcall ; abstract ;
                                procedure Set_Errors( Value : longint ) ;
                                    virtual ; stdcall ; abstract ;
                                procedure Set_Warnings( Value : longint ) ;
                                    virtual ; stdcall ; abstract ;

                                // Declining feature.  Use Log_Error instead.
			                    procedure Set_Error_Text( Value : PChar ) ;
                                    virtual ; stdcall ; abstract ;

                                procedure Get_Error( Index : longint ;
                                    var Text, Filename : PChar ;
                                    var Line, Severity : longint ) ;
                                    virtual ; stdcall ; abstract ;

                                procedure Log_Error( Text, Filename : PChar ;
                                    Line, Severity : longint ) ;
                                    virtual ; stdcall ; abstract ;

                                procedure Set_Line( Value : longint ) ;
                                    virtual ; stdcall ; abstract ;

                                procedure Output_To_Listing( Text : PChar ;
                                    Text_Type : integer ) ;
                                    virtual ; stdcall ; abstract ;

                                property Aborted : boolean
                                    read Get_Aborted
                                    write Set_Aborted ;
                                property Code : int64
                                    read Get_Code
                                    write Set_Code ;
                                property Data : int64
                                    read Get_Data
                                    write Set_Data ;
                                property Errors : longint
                                    read Get_Errors
                                    write Set_Errors ;
                                property Warnings : longint
                                    read Get_Warnings
                                    write Set_Warnings ;
                                property Error_Text : PChar
                                    read Get_Error_Text
                                    write Set_Error_Text ;
                        end ; // TAssembler_Status

    TAssembler_Extension = class
                               public
                                   { Process a directive.  Res is any generated
                                     code.  Res_Length is the length of the
                                     generated code. }
                                   function Process_Directive( Source : PChar ;
                                       var Res : PChar ;
                                       var Res_Length : longint ;
                                       Status : TAssembler_Status ) : TUnified_Exception ;
                                       virtual ; stdcall ; abstract ;

                                   { Notice of external symbol reference.
                                     Source contains the symbole name, a space,
                                     and the address where it is referenced. }
                                   procedure External_Symbol( Source : PChar ) ;
                                       virtual ; stdcall ; abstract ;
                           end ;

      // Logger options...
const LO_Append = 1 ; // Append to existing log file
      LO_Include_TimeStamp = 2 ; // Include Timestamp in log
      LO_Include_Name = 4 ; // Include Component name in log
      LO_Hide_Execution = 8 ;
      LO_Hide_Data = 16 ;
      LO_Hide_Sent_Signal = 32 ;
      LO_Hide_Received_Signal = 64 ;
      LO_Hide_State_Change = 128 ;
      LO_Hide_Read = 256 ;
      LO_Hide_Write = 512 ;
      LO_Hide_Input = 1024 ;
      LO_Hide_Output = 2048 ;
      LO_Hide_Other = 4096 ;
      // 8192+ = reserved for future use

      // Log Types...
const LT_Execution = 0 ; // Sent at the start of an operation (eg CPU instruction execution)
      LT_Data = 1 ; // Data
      LT_Sent_Signal = 2 ; // Sent signal
      LT_Received_Signal = 3 ; // Received signal
      LT_State_Change = 4 ; // State change
      LT_Read = 5 ; // A read request (memory or memory-like component)
      LT_Write = 6 ; // A write request (memory or memory-like component)
      LT_Input = 7 ; // An input request
      LT_Output = 8 ; // An output request
      LT_Other = 9 ; // Some other situation

      // Assembly flags...
const ASF_Immediate_Mode = 1 ; // Immediate mode
      ASF_Want_Symbol_Table = 2 ; // Generatea a symbol table list (not passed to CPU assembler)
      ASF_Want_XRef = 4 ; // Generate a cross-reference list (not passed to CPU assembler)
      ASF_Generate_Virtual = 8 ; // Generate into physical address space (rather than virtual)
      ASF_Disassemble = 16 ; // Disassembly is using assembly to get information on instruction
      ASF_Extended = 32 ; // Passed stream is a TCEF_Stream, otherwise a TCOM_Stream
      // 64+ = reserved for future use


      // Logging radix values...
const CEF_LR_Print_Friendly = 0 ;
const CEF_LR_ASCII = 1 ;
const CEF_LR_Radix50 = 50 ;
const CEF_LR_EBCDIC = 51 ;


type TComponent = class ;
     TCPU = class ;
     TCable = class ;
     TMemory = class ;
     TMaster_Clock = class ;
     TUser_Interface = class ;
     TKeyboard = class ;
     TUI_Interface = class ;
     TRun_Time_System = class ;

     TCEF_Stream = class( TCOM_Stream )
                       public // API...
                           function Get_Component : TComponent ;
                              virtual ; stdcall ; abstract ;
                           procedure Set_Component( Value : TComponent ) ;
                              virtual ; stdcall ; abstract ;

                           property Component : TComponent
                               read Get_Component
                               write Set_Component ;
                   end ;

     TCEF_Logger = class
                       public // API...
                           procedure Attach ;
                               virtual ; stdcall ; abstract ;
                           procedure Detach ;
                               virtual ; stdcall ; abstract ;
                           function Get_Data_Radix : longint ;
                               virtual ; stdcall ; abstract ;
                           function Get_Options : longint ;
                               virtual ; stdcall ; abstract ;
                           function Get_Paused : boolean ;
                               virtual ; stdcall ; abstract ;
                           function Get_Wrap_Column : longint ;
                               virtual ; stdcall ; abstract ;
                           procedure Set_Data_Radix( Value : longint ) ;
                               virtual ; stdcall ; abstract ;
                           procedure Set_Options( Value : longint ) ;
                               virtual ; stdcall ; abstract ;
                           procedure Set_Paused( Value : boolean ) ;
                               virtual ; stdcall ; abstract ;
                           procedure Set_Wrap_Column( Value : longint ) ;
                               virtual ; stdcall ; abstract ;

                           { Set UI component for the logger (for retrieving
                             clock times, etc. }
                           procedure Set_UI( Value : TUI_Interface ) ;
                               virtual ; stdcall ; abstract ;

                           // Returns name of current log file...
                           function Filename : PChar ;
                               virtual ; stdcall ; abstract ;

                           { Open the specified file as the log file.  If Append
                             is true, the existing file is appended to.  If the
                             file doesn't exist it is created.  If Append is
                             false, any existing file is overwritten.  Returns
                             True if the file was opened and false otherwise.
                           }
                           function Open( Filename : PChar ; Append : boolean ) : boolean ;
                               virtual ; stdcall ; abstract ;

                           { Terminate the logger instance.  This should be done
                             via the Deatch call. }
                           procedure Terminate ; virtual ; stdcall ; abstract ;

                           { Log an item to the log file.  S is the data to log.
                             Len is the length of S.  If Len is -1, S is assumed
                             to be null-terminated.  Continuation is false if
                             this is the first item of a log operation.  If
                             continuation is true, this is considered to be part
                             of the preceeding log item.  Log_Type indicates the
                             type of information being logged (see LT_*). }
                           procedure Log( C : TComponent ; S : PChar ; Len : longint ;
                               Continuation : boolean ; Log_Type : longint ) ;
                               virtual ; stdcall ; abstract ;

                           { Log an item to the log file.  The logger gathers
                             the state from the component and does the log. This
                             is used for CPUs and the PC corresponding to the
                             beginning of the current instruction is passed. I
                             is the instruction about to be executed. This
                             should be called after the instruction is fetched
                             but before it is executed. }
                           procedure Update( C : TComponent ; PC, I : int64 ) ;
                               virtual ; stdcall ; abstract ;

                           { Radix to use for logged data (see also CEF_LR_*):
                               0 = Print-friendly ASCII string (control codes replaced with ".")
                               1 = Literal ASCII string
                               2-36 = Base to represent data in
                               37-49 = reserved for future use
                               50 = Radix 50 representation
                               51 = Convert data from EBCDIC to ASCII
                               52+ = reserved for future use
                           }
                           property Data_Radix : longint
                               read Get_Data_Radix
                               write Set_Data_Radix ;

                           // See LO_* for valid options...
                           property Options : longint
                               read Get_Options
                               write Set_Options ;

                           // Whether or not logging is paused
                           property Paused : boolean
                               read Get_Paused
                               write Set_Paused ;

                           // Column to wrap log lines at (0=no wrap)
                           property Wrap_Column : longint
                               read Get_Wrap_Column
                               write Set_Wrap_Column ;
                   end ; // TCEF_Logger

     TUI_Interface = class
                         public
                             { This method is called whenever a component blocks (Blocked is
                               true) or unblocks (Blocked is false).  Component is the component
                               in question. }
                             procedure Block( Component : TComponent ; Blocked : boolean ) ;
                                 virtual ; stdcall ; abstract ;

                             { This method is called when a CPU triggers a breakpoint.  The method
                               returns True if the CPU is to continue execution of the instruction
                               or False if not. CPU is the CPU component which triggered the
                               breakpoint, Address is the execution address, and Physical indicates
                               whether the address is physical or virtual.  Space is the index of
                               the address space to assign the breakpoint to.  0 is the default
                               address space. }
                             function Breakpoint_Notice( Address : int64 ; Physical : boolean ;
                                 Space : integer ; CPU : TComponent ) : boolean ;
                                 virtual ; stdcall ; abstract ;

                             { This function returns the instance of the Master clock object for the
                               application.  NULL is a valid return value and should be tested by
                               any code calling this program (indicating that there is no master
                               clock and timing issues are to be ignored). }
                             function Clock : TMaster_Clock ;
                                 virtual ; stdcall ; abstract ;

                             { Returns a deugger object for this object. }
                             function Debugger : TDebug_Interface ;
                                 virtual ; stdcall ; abstract ;

                             { Returns a stream for the passed (partial) filename.  If file cannot
                               be opened/accessed, nil is returned. }
                             function Get_File_Stream( Name : PChar ) : TCOM_Stream ;
                                 virtual ; stdcall ; abstract ;

                             // If Flag is true, the UI is hidden, otherwise it is made visible.
                             procedure Hide( Flag : boolean ) ;
                                 virtual ; stdcall ; abstract ;

                             { Called by a blocked component (such as a CPU in wait state). }
                             procedure Idle( Component : TComponent ) ;
                                 virtual ; stdcall ; abstract ;

                             { This logs an actual program error (exception or warning).  Severity
                               is one of the following values:
                                   0 = Informational message
                                   1 = Warning
                                   2 = Non-fatal error
                                   3 = Fatal error.  The application should shut down. }
                             procedure Log_Error( Component : TComponent ; Text : PChar ;
                                 Severity : longint ) ; virtual ; stdcall ; abstract ;

                             { This logs a simulated hardware error.  Severity is one of the
                                following values:
                                        0 = Informational message
                                        1 = Warning
                                        2 = Non-fatal error
                                        3 = Fatal simulated hardware failure. }
                             procedure Log_Simulated_Error( Component : TComponent ; Text : PChar ;
                                 Severity : longint ) ;
                                 virtual ; stdcall ; abstract ;

                             { A component calls this method in response to a Show_Status request.
                               Each call provides one line of status information.  Index is 0 for
                               the first line, 1 for the second, etc. }
                             procedure Log_Status( Text : PChar ; Index : longint ) ;
                                 virtual ; stdcall ; abstract ;

                             { Logs a trace event for the specified component. }
                             procedure Log_Trace( Component : TComponent ; Description : PChar ) ;
                                 virtual ; stdcall ; abstract ;

                             { Called when a component changes a signal state.  Index is the
                               state index.  Active is true if the new state of the indexed
                               signal is active.  All components that wanted to be notified of
                               signal changes are notified via their Signal_Change_Notice
                               method. }
                             procedure Signal_Change_Notice( Component : TComponent ;
                                 Index : longint ; Active : boolean ) ;
                                 virtual ; stdcall ; abstract ;

                             { Called when a component suffers some form of exception.  Note that
                               CPUs that support exception handling must report those exceptions via
                               State_Change_Notice. }
                             procedure Signal_Exception( Component : TComponent ;
                                 Description : PChar ; Index : longint ) ;
                                 virtual ; stdcall ; abstract ;

                             { Called when a component changes a state.  Index is the state index.
                               Active is true if the new state of the indexed state is active. }
                             procedure State_Change_Notice( Component : TComponent ;
                                 Index : longint ; Active : boolean ) ;
                                 virtual ; stdcall ; abstract ;

                             // Terminates the UI, which usually terminates the application
                             procedure Terminate ; virtual ; stdcall ; abstract ;

                             { Toggle the embededed state of the specified TUser_Interface
                               component. }
                             procedure Toggle_Embed( Component : TComponent ) ;
                                 virtual ; stdcall ; abstract ;

                             { Returns the CEF specification version number to which this interface
                               conforms. }
                             function Version : integer ; virtual ; stdcall ; abstract ;

                             { Indicates that the specified component wishes to be notified of
                               signal changes (or not). }
                             procedure Want_Signals( Component : TComponent ; Value : boolean ) ;
                                 virtual ; stdcall ; abstract ;

                             { This method is called when a component triggers a watchpoint.
                               Address is the address that triggered the watchpoint.  Access
                               indicates the type of access:
                                   2 = Read (input)
                                   3 = Write (output)
                               Tag is dependant upon the component and Component is the component
                               which triggered the watchpoint.  Memory is true if this is a memory
                               watchpoint.  Internal is true if this is a watch on component
                               internals.  Port is true if this is a port I/O access. }
                             procedure Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
                                 Component : TComponent ; Memory, Internal, Port : boolean ) ;
                                 virtual ; stdcall ; abstract ;

                             // Port name for port Index across all loaded components.
                             function Get_Port_Name( Index : longint ) : PChar ;
                                 virtual ; stdcall ; abstract ;

                             // Port description for port Index across all loaded components.
                             function Get_Port_Description( Index : longint ) : PChar ;
                                 virtual ; stdcall ; abstract ;

                             // Port component for port Index across all loaded components.
                             function Get_Port( Index : longint ) : TComponent ;
                                virtual ; stdcall ; abstract ;

                             // Connected component for port Index across all loaded components.
                             function Get_Port_Connection( Index : longint ) : TComponent ;
                                 virtual ; stdcall ; abstract ;

                             // Parent of port Index.
                             function Port_Parent_Component( Index : longint ) : TComponent ;
                                 virtual ; stdcall ; abstract ;

                             // True = start running CPUs; False = pause CPUs
                             procedure Run( State : boolean ) ;
                                 virtual ; stdcall ; abstract ;

                             // V2.2:

                             // Obtain a process ID for a parallel process.  Returns -1 if no
                             // (more) parallel processes are allowed (or not supported).  If
                             // Force is True, an ID is always returns (unless not supported,
                             // in which case -1 is returned).
                             function Process_ID( Name : PChar ; Force : boolean ) : integer ;
                                 virtual ; stdcall ; abstract ;

                             // Indicates that the specified process ID is about to start
                             // running.  False is returned if parallel processing is disabled.
                             // Priority is ignored on call.  On return, it is the priority
                             // that the process should run at.  This value is 0 to indicate
                             // default (or normal) priority.  > 0 means elevated priority and
                             // < 0 means lowered priority.  Anything outside the range of
                             // -32767 <= Priority <= 32767 should be ignored.  Within the range
                             // of 1 to 32767 (or -1 to -32767), the entire possible range of
                             // the system running the emulator is mapped on this range.  For
                             // instance, if the system supports 2 levels of priority above and
                             // below normal, then the priority value should be shifted 15 bits
                             // to the right and that value maps onto -2, -1, 0, 1, or 2.
                             function Process_Start( ID : longint ;
                                 var Priority : longint ) : boolean ;
                                 virtual ; stdcall ; abstract ;

                             // Indicates that the specified process ID is not running.
                             procedure Process_End( ID : longint ) ;
                                 virtual ; stdcall ; abstract ;

                             // Indicates that the specified process has been destructed.
                             procedure Process_Deleted( ID : longint ) ;
                                 virtual ; stdcall ; abstract ;

                             // V2.5 and later...

                             // Prompt user and add a new port breakpoint.
                             procedure Add_Port_Breakpoint ;
                                 virtual ; stdcall ; abstract ;

                             // Prompt user and add an execution breakpoint.
                             procedure Add_Breakpoint ;
                                 virtual ; stdcall ; abstract ;

                             // Prompt user and add register breakpoint.
                             procedure Add_Register_Breakpoint ;
                                 virtual ; stdcall ; abstract ;

                             // Prompt user and create a new memory breakpoint.
                             procedure Create_New_Breakpoint ;
                                 virtual ; stdcall ; abstract ;

                             // V2.6 and later...
             
                             // Return passed component name qualified by path, etc.
                             function Get_Component_Filename( Name : PChar ) : PChar ;
                                  virtual ; stdcall ; abstract ;

                             procedure Termination_Notice( C : TComponent ) ;
                                  virtual ; stdcall ; abstract ;

                             // Load and return a component with the specified name.
                             function Load_Component( Name : PChar ) : TComponent ;
                                  virtual ; stdcall ; abstract ;
                     end ; // TUI_Interface


     TProfiler = class
                     public
                         { Clears profiling information for specified domain.
                           If Domain is -1, all domains are cleared. }
                         procedure Clear( Domain : integer ) ;
                             virtual ; stdcall ; abstract ;

                         { Returns profiling domain name for given index.  If
                           the index is invalid, the function returns nil.  A
                           domain indicates a type of profiling data.  For
                           instance, a CPU may track the number of calls to
                           certain addresses as one domain, and the number of
                           calls to specific types of instructions in another
                           domain, etc. }
                         function Domain_Name( Index : integer ) : PChar ;
                             virtual ; stdcall ; abstract ;

                         { Returns a line of information associated with the
                           specified domain.  The first line is Index 0.  If an
                           invalid index is passed, the function returns nil. }
                         function Report_Line( Domain, Index : integer ) : PChar ;
                             virtual ; stdcall ; abstract ;

                         { Returns the CEF specification version number to which
                           this interface conforms. }
                         function Version : integer ; virtual ; stdcall ; abstract ;
                 end ; // TProfiler


     TComponent = class
                      public // API...
            { This function returns a facility code for the component class. }
            function Facility_Code : longint ; virtual ; stdcall ; abstract ;

            { Reinitalizes the component.  Note that this is not a constructor!
              It is not necessary for the application to call this method, since
              Make_Instance should have initialized the component appropriately.
              This simply allows the component to be initialized again. }
            function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { Terminates the component.  Note that this is not a destructor,
              although it should eventually cause the instance to be destructed.
              By convention, once this method is called, the instance is no
              longer valid. }
            function Terminate : TUnified_Exception ; virtual ; stdcall ; abstract ;

            { Returns number assigned by component creator. }
            function Serial_Number : integer ; virtual ; stdcall ; abstract ;

            { Adds the specified component to the list of components to notify
              along with Parent in Child_Notifications. }
            function Add_Notification( Component : TComponent ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            function Cable : TCable ; virtual ; stdcall ; abstract ;

            { This method returns the Indexth child component for this component.
              It returns nil if Index is outside of the range: 0 <= n <
              Max_child_component.  Child components are components owned by
              this component (such as a motherboard would own the CPU component). }
            function Child_Component( Index : longint ) : TComponent ;
                virtual ; stdcall ; abstract ;

            { Indicates a notice from a child component.  Child is the component
              and Notice is a value from Child_Notice_*. }
            procedure Child_Notification( Child : TComponent ;
                var Notice : longint ; var Params : int64 ) ; virtual ; stdcall ; abstract ;

            { Clears a watchpoint at the specified address.  Access is the type
              of access:
                            1 = Read or write (input or output)
                            2 = Read (input)
                            3 = Write (output)
              If Memory is true, the watch occurs on memory accesses from the
              component.
              If Memory is false, the watch occurs on port accesses from the
              component. }
            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

            { This method returns a value indicating the type of the component,
              as follows:
                            0 = Unknown (TComponent)
                            1 = CPU
                            2 = Bus
                            3 = Memory
                            4 = Motherboard
                            5 = I/O device
                            6 = User Interface }
            function Component_Type : longint ; virtual ; stdcall ; abstract ;

            { For components which accept input, this connects another component
              to this component's default input port. }
            function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { For components which generate output, this connnects another
              component to this component's default output port. }
            function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            function CPU : TCPU ; virtual ; stdcall ; abstract ;

            { Returns debug interface for this component. }
            function Debugger : TDebug_Interface ; virtual ; stdcall ; abstract ;

            { This removes the specified component from the list of components
              to be notified.  The component must have been added via
              Add_Notification. }
            function Delete_Notification( Component : TComponent ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { This method writes Size bits of data from the specified address
              from the specified buffer.  Memory is True if this is a memory
              write versus a I/O write.  This is solely for the UI, and the
              write does not trigger watchpoints or have any other side-effects
              other than changing the contents of the component. }
            function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                Memory : boolean ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

            { This disconnects the specified component from this component's
              default input port. }
            function Disconnect_Input( Component : TComponent ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { This disconnects the specified component from this component's
              default output port. }
            function Disconnect_Output( Component : TComponent ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { This method reads Size bits of data from the specified address
              into the specified buffer.  Size is updated with the actual number
              of bits copied.  Memory is True if this is a memory read versus an
              I/O read.  This is solely for the UI - the read triggers no
              watchpoints and has no other side-effects.  It also ignores access
              modes. }
            function Examine( Address : int64 ; var Size : longint ;
                Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

            { Returns the access mode of the specified address.  Return values
              are:
                            0 = Not supported (address out of range, etc.)
                            1 = Read/Write (I/O)
                            2 = Read-only (input-only)
                            3 = Write-only (write-only)
              Memory is true if address is a memory address and false if it is
              an I/O address. }
            function Get_Access_Mode( Address : int64 ;
                Memory : boolean ) : longint ; virtual ; stdcall ; abstract ;

            { Provides a description of the exception conditions signaled via
              Signal_Exception. }
            function Get_Exception_Description( Index : longint ) : PChar ;
                virtual ; stdcall ; abstract ;

            function Get_Parent : TComponent ; virtual ; stdcall ; abstract ;

            { Returns True if profiling for this component is on. }
            function Get_Profiling : boolean ; virtual ; stdcall ; abstract ;

            { Returns the component's default read latency, in nanoseconds.  If
              the component doesn't support latency, this should return 0. }
            function Get_Read_Latency : longint ; virtual ; stdcall ; abstract ;

            { This returns the state of a signal with the specified name.  If
              the component recognizes the signal name, it returns True.
              Otherwise it returns False.  If the result is True, State is set
              to the current value of the signal. }
            function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                virtual ; stdcall ; abstract ;

            { Get the name of a component state.  If index is invalid, the
              function returns nil. }
            function Get_State_Name( Index : longint ) : PChar ;
                virtual ; stdcall ; abstract ;

            function Get_Tag : longint ; virtual ; stdcall ; abstract ;

            function Get_Trace : boolean ; virtual ; stdcall ; abstract ;

            { Returns the component's default write latency, in nanoseconds.  If
              the component doesn't support latency, this should return 0. }
            function Get_Write_Latency : longint ; virtual ; stdcall ; abstract ;

            { This method returns the Indexth input component for this component.
              It returns NULL if Index is outside of the range: 0 <= n <
              Max_input_component }
            function Input_Component( Index : longint ) : TComponent ;
                virtual ; stdcall ; abstract ;

            function Keyboard : TKeyboard ; virtual ; stdcall ; abstract ;

            function Memory : TMemory ; virtual ; stdcall ; abstract ;

            { This method returns a string containing the name of this component. }
            function Name : PChar ; virtual ; stdcall ; abstract ;

            { This method returns the Indexth output component for this
              component.  It returns NULL if Index is outside of the range:
              0 <= n < Max_output_component }
            function Output_Component( Index : longint ) : TComponent ;
                virtual ; stdcall ; abstract ;

            function Profiler : TProfiler ; virtual ; stdcall ; abstract ;

            { Calling this method requests the component to transmit data to its
              default output port.  IO_Type is the type of read operation (see
              IO_Type_*).  Size is the number of bits being requested (0=default
              for component).  The component should verify that the address is
              within its range before responding.  If not, it should do nothing.
              If the component will respond to the request, the method returns
              true, otherwise it returns false. }
            function Read( Address : int64 ; Size : longint ;
                IO_Type : longint ) : boolean ; virtual ; stdcall ; abstract ;

            { Reset the component.  Behavior is implementation dependant. }
            procedure Reset ; virtual ; stdcall ; abstract ;

            { This method causes the component to restore its contents (but NOT
              state) from the passed stream.  It is the responsibility of the
              component to restore the contents of any child components (but not
              attached components). }
            function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { This method causes the component to restore its current state (but
              NOT contents) from the passed stream.  It is the responsibility of
              the component to restore the state of any child components (but
              not attached components). }
            function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { This method causes the component to save its contents (but NOT
              state) to the passed stream.  It is the responsibility of the
              component to save the contents of any child components (but not
              attached components). }
            function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { This method causes the component to save its current state (but
              NOT contents) to the passed stream.  It is the responsibility of
              the component to save the state of any child components (but not
              attached components). }
            function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                virtual ; stdcall ; abstract ;

            { Sets the access mode of the specified address range (Low to High,
              inclusive).  Typ is:
                            1 = Read/Write (I/O)
                            2 = Read-only (input-only)
                            3 = Write-only (write-only)
              Memory is true if address is a memory address and false if it is
              an I/O address. }
            function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                Typ : longint ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

            procedure Set_Parent( Component : TComponent ) ; virtual ; stdcall ; abstract ;

            { Turns profiling for this component on or off.  Profiling is off by
              default.  If Children is true, all child components will be
              assigned the same profiling value (this doesn't affect input or
              output components). }
            procedure Set_Profiling( _On, Children : boolean ) ;
                virtual ; stdcall ; abstract ;

            { Sets the component's default read latency in nanoseconds.  If the
              component supports latency, it should block by this amount on each
              read. }
            procedure Set_Read_Latency( Value : longint ) ; virtual ; stdcall ; abstract ;

            { This sets the state of a signal with the specified name.  This
              should only be called when the signal state actually changes (that
              is, State should never be the same on two consecutive calls). }
            procedure Set_Signal( Name : PChar ; State : boolean ) ;
                virtual ; stdcall ; abstract ;

            { User-defined value associated with component. }
            procedure Set_Tag( Value : longint ) ; virtual ; stdcall ; abstract ;

            procedure Set_Trace( Value : boolean ) ; virtual ; stdcall ; abstract ;

            { An component-specific initialization string can be sent to the
              component after initialization, via this method. }
            procedure Set_Up( P : PChar ) ; virtual ; stdcall ; abstract ;

            { Sets a watchpoint at the specified address.  When the component is
              about to access this address, it will generate a watchpoint notice
              to the UI interface.  Access is the type of access:
                            1 = Read or write (input or output)
                            2 = Read (input)
                            3 = Write (output)
              If Memory is true, the watch occurs on memory accesses from the
              CPU.
              If Memory is false, the watch occurs on port accesses from the CPU. }
            function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

            { Sets the component's default write latency in nanoseconds.  If the
              component supports latency, it should block by this amount on each
              write. }
            procedure Set_Write_Latency( Value : longint ) ;  virtual ; stdcall ; abstract ;

            { Calling this method causes the component to report its status to
              the UI interface, in a human-readable form.  For instance, a CPU
              component would report its registers and their contents. }
            procedure Show_Status ; virtual ; stdcall ; abstract ;

            { Called when the specified component has a signal change.  This is
              only called if this component requested signal notices from the
              UI. }
            procedure Signal_Change_Notice( Component : TComponent ;
                Index : longint ; Active : boolean ) ; virtual ; stdcall ; abstract ;

            { Returns the number of defined signals. }
            function Signal_Count : longint ; virtual ; stdcall ; abstract ;

            { Returns the name of the supported signals.  The first index is 0.
              If there is no signal for the specified index, or index is less
              than 0, the result is nil. }
            function Signal_Name( Index : longint ) : PChar ; virtual ; stdcall ; abstract ;

            { Returns True if the signal is an output from the component.
              Result is indefined if Index is invalid. }
            function Signal_Out( Index : longint ) : boolean ; virtual ; stdcall ; abstract ;

            { Returns True if the signal is active-low.  Result is undefined if
              Index is invalid. }
            function Signal_Active_Low( Index : longint ) : boolean ;
                virtual ; stdcall ; abstract ;

            { Returns the index of the signal with the specified name.  If the
              name is invalid, -1 is returned. }
            function Signal_Index( Name : PChar ) : integer ; virtual ; stdcall ; abstract ;

            { Returns True if the component supports the specified feature.  If
              the component does not inherently support the feature (such as a
              CPU feature for a memory component), the result is undefined. }
            function Support_Feature( ID : longint ) : boolean ;
                virtual ; stdcall ; abstract ;

            function User_Interface : TUser_Interface ;
                virtual ; stdcall ; abstract ;

            { Returns the CEF specification version number to which this
              interface conforms. }
            function Version : longint ; virtual ; stdcall ; abstract ;

            { Calling this allows a component to resume its activity after it
              sends a Block request to the Master clock. }
            procedure Wake ; virtual ; stdcall ; abstract ;

            { Calling this method writes Value to the component's input port
              with the specified address.  IO_Type is the type of write
              operation (see IO_Type_*).  Size is the number of bits in Value to
              write (0=default for component) and may be 0 to 32, inclusive.
              The component should verify that the address is within its range
              before responding.  If not, it should do nothing, and return no
              errors. }
            function Write( Address : int64 ; Value, Size : longint ;
                IO_Type : longint ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

            { This is the same as calling Write_Byte once for each byte in the
              string, incrementing Address once per byte.  It is provided to
              allow more efficient means of transmitting large amounts of data.
              Size is the number of bits in Value to write (0=default for
              component).  The component should verify that the address is
              within its range before responding.  If not, it should do nothing,
              and return no errors. }
            function Write_String( Address : int64 ; Value : PChar ;
                Size : longint ; IO_Type : longint ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

            function Get_Port_Name( Index : longint ) : PChar ;
                virtual ; stdcall ; abstract ;

            function Get_Port_Description( Index : longint ) : PChar ;
                virtual ; stdcall ; abstract ;

            function Get_Port( Index : longint ) : TComponent ;
                virtual ; stdcall ; abstract ;

            function Get_Port_Connection( Index : longint ) : TComponent ;
                virtual ; stdcall ; abstract ;

            { Notice from TUI_Interface component.  Code is UI_Notice_Code_*
              code.  Data depends on the calue of Code. }
            procedure UI_Notice( Code : longint ; var Data : int64 ) ;
                virtual ; stdcall ; abstract ;

            { Returns True if the component responds to to I/O type Typ at
              address Address.  Examine is True to check for response to
              examines/deposits, and False for normal reads/writes. }
            function Respond_To_Address( Address : int64 ; Typ : integer ;
                Examine : boolean ) : boolean ; virtual ; stdcall ; abstract ;

            { V2.6 or later... }

            function Get_Logger : TCEF_Logger ; virtual ; stdcall ; abstract ;

            procedure Set_Logger( Value : TCEF_Logger ) ; virtual ; stdcall ; abstract ;

        public // Properties...
            property Tag : longint
                read Get_Tag
                write Set_Tag ;

            property Parent : TComponent
                read Get_Parent
                write Set_Parent ;
        end ; // TComponent


    TMaster_Clock = class
        { This procedure registers a request for component to receive a Wake
           signal at the specified time delta (in nanoseconds).  If component is
           nil, the next item in the queue is unblocked. }
	    procedure Block( Component : TComponent ; Time_Delta : int64 ) ;
             virtual ; stdcall ; abstract ;

         { Returns debug interface for this clock. }
         function Debugger : TDebug_Interface ; virtual ; stdcall ; abstract ;

	    { Initializes the clock to time index 0 and sets the UI interface. }
	    procedure Initialize( UI : TUI_Interface ) ; virtual ; stdcall ; abstract ;

	    { This function returns the current simulated master clock (which is
           measured in nanoseconds) time index. }
	    function Get_Time_Index : int64 ; virtual ; stdcall ; abstract ;

         { Returns value passed by creator. }
         function Serial_Number : integer ; virtual ; stdcall ; abstract ;

         { Returns True if the clock supports the specified feature.  If the
           clock does not inherently support the feature (such as a CPU
           feature), the result is undefined. }
         function Support_Feature( ID : integer ) : boolean ;
             virtual ; stdcall ; abstract ;

         { Returns the CEF specification version number to which this interface
           conforms. }
         function Version : integer ; virtual ; stdcall ; abstract ;

         { V2.3 or later... }

         { Set clock mode.  See MCM_*  }
         procedure Set_Mode( M : integer ) ; virtual ; stdcall ; abstract ;

         { Fet clock mode.  See MCM_*  }
         function Get_Mode : integer ; virtual ; stdcall ; abstract ;

         { Unblock all blocked components }
         procedure Unblock ; virtual ; stdcall ; abstract ;

         procedure Terminate ; virtual ; stdcall ; abstract ;

         property Mode : integer
             read Get_Mode
             write Set_Mode ;
    end ; // TMaster_Clock


     TCable = class
                public
                    { Returns true if this cable is a serial cable.  It is a
                      parallel cable otherwise. }
                    function Serial : boolean ; virtual ; stdcall ; abstract ;

                    { Returns name of the protocol supported by the cable (nil
                      if generic/any). }
                    function Protocol : PChar ; virtual ; stdcall ; abstract ;

                    { Transmit one data item.  Speed is the bit rate of the
                      transfer (0=automatch), Value is the data to transmit,
                      Data_Size is the number of bits in Value (32 max),
                      Stop_Bits indicates the number of stop bits. }
                    function Transmit( Speed : int64 ;
                        Value, Data_Size, Stop_Bits : longint ) : TUnified_Exception ;
                        virtual ; stdcall ; abstract ;

                    { Transmits a string of data by calling Transmit once for
                      each data item in Value. }
                    function Transmit_String( Speed : int64 ; Value : PChar ;
                        Data_Size, Stop_Bits : longint ) : TUnified_Exception ;
                        virtual ; stdcall ; abstract ;

                    { Called when received data from another cable component.
                      Source is the component that the data is coming from.
                      Speed is the bits/second data rate.  Value is the data
                      being sent from the source.  Data_Size is the size of the
                      passed data, in bits (32 max).  Stop_Bits is the number of
                      stop bits (when appropriate). }
                    procedure Receive( Source : TComponent ; Speed : int64 ;
                        Value, Data_Size, Stop_Bits : integer ) ;
                        virtual ; stdcall ; abstract ;

                    function Get_Data( var Speed : int64 ;
                        var Value, Data_Size, Stop_Bits : integer ) : boolean ;
                        virtual ; stdcall ; abstract ;
             end ; // TCable


     TUser_Interface = class
                           public
                               function Get_Hidden : boolean ;
                                   virtual ; stdcall ; abstract ;
                               procedure Set_Hidden( Value : boolean ) ;
                                   virtual ; stdcall ; abstract ;

                               function Get_Parent_Window : THandle ;
                                   virtual ; stdcall ; abstract ;
                               procedure Set_Parent_Window( Value : THandle ) ;
                                   virtual ; stdcall ; abstract ;

                               function Get_Caption : PChar ;
                                   virtual ; stdcall ; abstract ;
                               procedure Set_Caption( Value : PChar ) ;
                                   virtual ; stdcall ; abstract ;

                               procedure Set_Size( Height, Width : integer ) ;
                                   virtual ; stdcall ; abstract ;

                               function Optimal_Height : integer ;
                                   virtual ; stdcall ; abstract ;

                               function Optimal_Width : integer ;
                                   virtual ; stdcall ; abstract ;

                               { Returns the CEF specification version number to
                                 which this interface conforms. }
                               function Version : integer ; virtual ; stdcall ; abstract ;

                               // V2.4 and later...

                               procedure Initialize ; virtual ; stdcall ; abstract ;

                           public // Properties...
                               property Hidden : boolean
                                   read Get_Hidden
                                   write Set_Hidden ;
                               property Parent_Window : THandle
                                   read Get_Parent_Window
                                   write Set_Parent_Window ;
                               property Caption : PChar
                                   read Get_Caption
                                   write Set_Caption ;
                       end ; // TUser_Interface


    TKeyboard = class
                    public
                        { Retrieves the next key in a keystroke from the
                          keyboard.  Returns nil or null string if end of
                          current keystroke. }
                        function Get_Key : PChar ; virtual ; stdcall ; abstract ;

                        { Returns True if the specified key is down in the
                          pending keystroke.  If Name is nil, returns True if
                          any key is down. }
                        function Get_Key_Down( Name : PChar ) : boolean ;
                            virtual ; stdcall ; abstract ;

                        { Sets the state of a given key to up or down.  The key
                          is reset when the next keystroke is returned.  This
                          can be used to synchronize the keyboard component's
                          visible state with the physical keyboard. }
                        procedure Set_Key_Down( Name : PChar ; State : boolean ) ;
                            virtual ; stdcall ; abstract ;

                        { Returns the name of the key associated with the
                          passed index.  Returns nil if the index is invalid. }
                        function Get_Key_Name( Index : integer ) : PChar ;
                            virtual ; stdcall ; abstract ;

                        { Returns True if the specified LED is lit.  If the LED
                          name is not recognized, the function returns false. }
                        function Get_LED_State( Name : PChar ) : boolean ;
                            virtual ; stdcall ; abstract ;

                        { Sets the state of a given LED to lit or not.  This
                          can be used to synchronize the keyboard component's
                          visible state with the physical keyboard. }
                        procedure Set_LED_State( Name : PChar ; State : boolean ) ;
                            virtual ; stdcall ; abstract ;

                        { Returns the name of the LED associated with the
                          passed index.  Returns nil if the index is invalid. }
                        function Get_LED_Name( Index : integer ) : PChar ;
                            virtual ; stdcall ; abstract ;

                        { Returns the CEF specification version number to which this interface
                          conforms. }
                        function Version : integer ; virtual ; stdcall ; abstract ;
                end ;


     TMemory = class
                   public // API...
                       { This function returns a facility code for the component class. }
                       function Facility_Code : longint ; virtual ; stdcall ; abstract ;

                       { Dumps an image of memory between Start and Start + Size - 1, inclusive,
                         to the passed buffer, which must be large enough to hold Size bytes.
                         Any range of addresses that are outside the memory's range are zero
                         filled. }
                       procedure Dump( Start, Size : int64 ; Buffer : pointer ) ;
                           virtual ; stdcall ; abstract ;

                       { Returns the set of addresses that this component
                         responds to. }
                       procedure Get_Address_Range( var Low, High : int64 ) ;
                           virtual ; stdcall ; abstract ;

                       { Loads memory between Start and Start + Size - 1, inclusive,
                         with the data in the passed buffer.  Any data outside
                         the memory's range is ignored. }
                       procedure Load( Start, Size : int64 ; Buffer : pointer ) ;
                           virtual ; stdcall ; abstract ;

                       { Defines the set of addresses that this component r
                         esponds to. }
                       function Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;
                           virtual ; stdcall ; abstract ;

                       { Returns the CEF specification version number to which
                         this interface conforms. }
                       function Version : integer ; virtual ; stdcall ; abstract ;

                       // V2.6...
                       function Map_Virtual_To_Physical( Virt : int64 ) : int64 ;
                           virtual ; stdcall ; abstract ;
               end ; { TMemory }


     TCEF_Assembler_Context = class
                                  public // API...
                                      procedure Initialize ; virtual ; stdcall ; abstract ;

                                      procedure Terminate ; virtual ; stdcall ; abstract ;

                                      function Add_Mapping( Filename : PChar ;
                                          Address : int64 ;
                                          Line : integer ) : integer ;
                                          virtual ; stdcall ; abstract ;

                                      function Add_Symbol( Name : PChar ;
                                          P : pSymbol_Record ) : TUnified_Exception ;
                                          virtual ; stdcall ; abstract ;

                                      procedure Delete( Sym : PChar ) ;
                                          virtual ; stdcall ; abstract ;

                                      procedure Delete_Mapping( Index : integer ) ;
                                          virtual ; stdcall ; abstract ;

                                      function Find( Sym : PChar ;
                                          var Addr : int64 ;
                                          var Flg, D_T, Siz : longint ;
                                          var Dat : pointer ) : integer ;
                                          virtual ; stdcall ; abstract ;

                                      { Returns filename and line number
                                         corresponding to the passed address.  If
                                         there is no mapping, it returns -1. }
                                      function Mapping( Address : int64 ;
                                          var Filename : PChar ) : integer ;
                                          virtual ; stdcall ; abstract ;

                                      procedure Pop_Level ; virtual ; stdcall ; abstract ;

                                      procedure Push_Level ; virtual ; stdcall ; abstract ;

                                      { Get value associated with passed symbol.  If
                                        symbol is unknown, it returns False. }
                                      function Symbol_Value( Name : PChar ;
                                          var Value : int64 ) : boolean ;
                                          virtual ; stdcall ; abstract ;

                                      { Get size associated with passed symbol.  If
                                        symbol is unknown, it returns False. }
                                      function Symbol_Size( Name : PChar ;
                                          var Value : integer ) : boolean ;
                                          virtual ; stdcall ; abstract ;

                                      function Get_Case_Sensitive : boolean ;
                                          virtual ; stdcall ; abstract ;

                                      procedure Set_Case_Sensitive( Value : boolean ) ;
                                          virtual ; stdcall ; abstract ;

                                      // V2.2:

                                      function Find_First( var Sym : PChar ;
                                          var Addr : int64 ;
                                          var Flg, D_T, Siz : longint ;
                                          var Dat : pointer ) : integer ;
                                          virtual ; stdcall ; abstract ;

                                      function Find_Next( var Sym : PChar ;
                                          var Addr : int64 ;
                                          var Flg, D_T, Siz : longint ;
                                          var Dat : pointer ) : integer ;
                                          virtual ; stdcall ; abstract ;

                                      property Case_Sensitive : boolean
                                          read Get_Case_Sensitive
                                          write Set_Case_Sensitive ;
                              end ; // TCEF_Assembler_Context

     TMaster_Assembler = class
                             public // API...
        { Adds a reference for back-patching. Use Add_Reference_Ex instead. }
        procedure Add_Reference( Name : PChar ; Size : longint ;
            Address : int64 ) ; virtual ; stdcall ; abstract ;

        { Adds a symbol to the current scope, with the name in Name and the
          specified information. }
        function Add_Symbol( Name : PChar ; P : pSymbol_Record ) : TUnified_Exception ;
            virtual ; stdcall ; abstract ;

        { An old version of Assemble_Ex without Flags.  See Assemble_Ex for
          details. }
        function Assemble( Input, Output, Listing : TCOM_Stream ;
            Status : TAssembler_Status ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

        { Apply all backpatches. }
        procedure Backpatch( Status : TAssembler_Status ;
            Stream : TCOM_Stream ) ; virtual ; stdcall ; abstract ;

        { Evaluates an expression, returning the numeric value in Value. }
        function Evaluate( X : PChar ; var Value : int64 ) : TUnified_Exception ;
            virtual ; stdcall ; abstract ;

        { This method is used during assmebly by a TAssembler to insert assembly
          source in-place.  For instance, translating a directive into a
          standard directive, or doing macro expansions, etc.  If the expanded
          item is a directive that results in output, Res will be that output
          and Res_Length will be the size of that data, in bytes. }
        function Expand( Source : PChar ; var Res : PChar ;
            var Res_Length : longint ; Status : TAssembler_Status ) : TUnified_Exception ;
            virtual ; stdcall ; abstract ;

        { Returns facility code for class }
        function Facility_Code : longint ; virtual ; stdcall ; abstract ;

        { Returns True if case sensitivity to identifier names is on. }
        function Get_Case_Sensitive : boolean ; virtual ; stdcall ; abstract ;

        { Returns the symbol record for the identifier in Name.  Returns NULL,
          if Name is undeclared. }
        function Get_Symbol( Name : PChar ) : PSymbol_Record ;
            virtual ; stdcall ; abstract ;

        { Returns the next token in the input stream during an assembly.
          Returns a null string if an error occurs (such as no more input).
          This is useful for assemblers which need more input for a directive
          which is continued on additional lines. }
        function Get_Token : PChar ; virtual ; stdcall ; abstract ;

        { Returns entire current (remaining) input line. }
        function Grab_Line( Current : boolean ) : PChar ; virtual ; stdcall ; abstract ;

        { Inserts the data from Input into the current token stream. }
        procedure In_Line( Input : TCOM_Stream ) ; virtual ; stdcall ; abstract ;

        { Logs an assembly error.  Severity indicates the seriousness of the
          error:
                        0 = Informational
                        1 = Warning
                        2 = Error
                        3 = Fatal error (abort assembly) }
        procedure Log_Error( Text : PChar ; Severity : longint ) ; virtual ; stdcall ; abstract ;

        // Map current token position to the passed address.
        procedure Map( Address : int64 ) ; virtual ; stdcall ; abstract ;

        { Return next token without removing from the token stream.  If Same_Line
          is True, only return the next token on the current line. }
        function Peek_Token( Same_Line : boolean ) : PChar ; virtual ; stdcall ; abstract ;

        { Ends the current sub-scope for identifiers. }
        procedure Pop_Scope ; virtual ; stdcall ; abstract ;

        { Begins a sub-scope for identifiers within the current scope. }
        procedure Push_Scope ; virtual ; stdcall ; abstract ;

        { Returns a token to the input stream during an assembly. }
        procedure Put_Token( Token : PChar ) ; virtual ; stdcall ; abstract ;

        { Sets case sensitivity on identifier names. }
        procedure Set_Case_Sensitive( Value : boolean ) ; virtual ; stdcall ; abstract ;

        // Remove previous mapping added with Map().
        procedure UnMap ; virtual ; stdcall ; abstract ;

        { Returns the CEF specification version number to which this interface
          conforms. }
        function Version : integer ; virtual ; stdcall ; abstract ;

        { Adds a reference for back-patching.  Context is a caller-defined
          value which is passed back via the Backpatching method of TAssembler.
          Options indicate processing options (currently reserved for future
          use). Size is the size of the value to backpatch, in bytes. }
        procedure Add_Reference_Ex( Name : PChar ; Size : longint ;
            Address : int64 ; Context, Options : longint ) ; virtual ; stdcall ; abstract ;

        { Adds a CPU component to the assembler's list of CPUs for the
          .SET_TARGET_CPU directive. }
        procedure Add_CPU( CPU : TComponent ; Name : PChar ) ; virtual ; stdcall ; abstract ;

        // Removes all CPUs added with Add_CPU.
        procedure Clear_CPUs ; virtual ; stdcall ; abstract ;

        procedure Set_Assembler_Context( Value : TCEF_Assembler_Context ) ;
            virtual ; stdcall ; abstract ;

        function Get_Assembler_Context : TCEF_Assembler_Context ;
            virtual ; stdcall ; abstract ;

        procedure Terminate ; virtual ; stdcall ; abstract ;

        // V2.2:

        function Leading_Whitespace : boolean ; virtual ; stdcall ; abstract ;

        { Assembles code from the Input stream, directing the image data to the
          Output stream, and placing any list output to the Listing stream.
          Note that Listing can be NULL if no listing is desired.  Output can be
          NULL to do a syntax-check-only assembly.  Status is used to provide
          assembly statistics.  See ASF_* for flag values... }
        function Assemble_Ex( Input, Output, Listing : TCOM_Stream ;
            Status : TAssembler_Status ; Flags : longint ) : TUnified_Exception ;
            virtual ; stdcall ; abstract ;

        { Same as evaluate, but the result has PC_Adjustment added to any
          references to the current PC. }
        function Evaluate_Ex( Value : PChar ; var _Result : int64 ;
            PC_Adjustment : int64 ) : TUnified_Exception ; virtual ; stdcall ; abstract ;


        // V2.5:

        function Get_Base : integer ; virtual ; stdcall ; abstract ;

        procedure Set_Base( Value : integer ) ; virtual ; stdcall ; abstract ;

        // V2.6:

        procedure Register_Extension( Extension : TAssembler_Extension ) ;
            virtual ; stdcall ; abstract ;

        property Case_Sensitive : boolean
            read Get_Case_Sensitive
            write Set_Case_Sensitive ;

        property Assembler_Context : TCEF_Assembler_Context
            read Get_Assembler_Context
            write Set_Assembler_Context ;

        property Base : integer
            read Get_Base
            write Set_Base ;
     end ; // TMaster_Assembler


     TAssembler = class
        public
        { Informs the assembler the a new assembly operation is beginning.  This
          assembly will use the passed master assembler. }
        procedure Initialize( Master : TMaster_Assembler ) ; virtual ; stdcall ; abstract ;

        { Informs the assembler that the current assembly operation is now
          complete. }
        procedure Terminate ; virtual ; stdcall ; abstract ;

        { An old version of Assemble_Ex without Flags.  See Assemble_Ex for
          details. }
        function Assemble( inputs : PChar ; var outputs, machines : PChar ;
            var MachineL : longint ; var Address : int64 ;
            var Segment : longint ; Status : TAssembler_Status ) : TUnified_Exception ;
            virtual ; stdcall ; abstract ;

        { Returns the default radix (base) of numeric literals. }
        function Default_Radix : longint ; virtual ; stdcall ; abstract ;

        { Returns the default size of numeric literals, in bits. }
        function Default_Size : longint ; virtual ; stdcall ; abstract ;

        { Returns facility code for this class. }
        function Facility_Code : longint ; virtual ; stdcall ; abstract ;

        { Return a bar-delimited list of source file extensions.  The first
          one should be the default. }
        function Source_Extensions : PChar ; virtual ; stdcall ; abstract ;

        { Returns a list of valid characters for starting a symbol name. }
        function Valid_Symbol_Initial : PChar ; virtual ; stdcall ; abstract ;

        { Returns a list of valid characters for a symbol name, past the first. }
        function Valid_Symbol_After : PChar ; virtual ; stdcall ; abstract ;

        { Returns the CEF specification version number to which this interface
          conforms. }
        function Version : integer ; virtual ; stdcall ; abstract ;

        { Called by master assembler just before it backpatches an address.  If
          the function returns False, the master assembler doesn't perform the
          backpatch.  Otherwise it does.  Name is the expression that was
          evaluated for this backpatch, Address is the address of the backpatch
          data, Value is the data to write to Address, and Size is the size of
          the backpatch, in bytes.  Context and Options are the values passed
          to the Add_Reference_Ex method of the master assembler. }
        function Backpatching( Name : PChar ; Address : int64 ;
            var Value : int64 ; var Size : longint ;
            Context, Options, Line : longint ; Filename : PChar ;
            Status : TAssembler_Status ) : boolean ;
            virtual ; stdcall ; abstract ;

        // V2.2:

        function Normalize_Expression( Expression : PChar ; Numeric : boolean ;
            Status : TAssembler_Status ) : PChar ; virtual ; stdcall ; abstract ;

	    { This method assembles the source code in the inputs string.  If inputs
          is a null string, the assembler should fetch the data from the master
          assembler (Get_Token method).  Formatted output (if any) is returned
          in outputs, which is assumed to be in a human-readable form.  Upon
          return, machines contains the generated machine code for the
          instruction(s) passed.  Machinel is the length of the data in
          Machines.  Address is the target memory address where the Machines
          data is to go.  Segment is the current destination segment for the
          data in machines.  It has the following values:
            0 = Default code segment
            1 = Default data segment
            other = CPU-dependant or user-defined segments
          Machinel, Address, and Segment are passed the current values from the
          master assembler, but the called assembler may change them.  Address
          is virtual (physical if virtual addresssing not supported by CPU).
          Flags is some combination of ASF_* constants. }
        function Assemble_Ex( inputs : PChar ; var outputs, machines : PChar ;
            var MachineL : longint ; var Address : int64 ;
            var Segment : longint ; Status : TAssembler_Status ;
            Flags : longint ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

        // V2.6:
        { Informs the assembler that an assembly operation is about to begin.
          The return value is a context that is passed to Finish_Assembly. }
        function Begin_Assembly : int64 ; virtual ; stdcall ; abstract ;

        { Informs the assembler that the last assembly operation has completed.
          The value from Begin_Assembly is passed.  See Assemble_Ex for flags
          meanings. }
        function Finish_Assembly( Context : int64 ; var outputs, machines : PChar ;
            var MachineL : longint ; var Address : int64 ;
            Status : TAssembler_Status ; Flags : longint ) : TUnified_Exception ;
            virtual ; stdcall ; abstract ;

        { When the master assembler is about to allocate data (eg due to a .DB
          directive), it calls the CPU assembler with the address and size of
          the data.  The assembler can then adjust the address, if necessary,
          and return the address to which the data is to be placed.  If there is
          no place for the data, -1 is returned. }
        function Request_Data( Address, Size : int64 ) : int64 ;
            virtual ; stdcall ; abstract ;
     end ; { TAssembler }


     TCPU = class
        { Returns true if the CPU is big-endian.  Returns False if small-endian. }
        function Big_Endian : boolean ; virtual ; stdcall ; abstract ;

        { Clears a breakpoint at the specified address.  If Physical is true,
          the address is a physical address, otherwise it is a virtual
          address. }
        function Cancel_Breakpoint( Address : int64 ; Space : longint ;
            Physical : boolean ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

        { Clears a watchpoint at the specified address.  Access is the type of
          access:
          1 = Read or write (input or output)
          2 = Read (input)
          3 = Write (output)

          If Memory is true, the address is an internal cache address.  Otherwise
          it is a register index. }
        function Clear_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

        { Indicates the specified base used by the CPU instruction set. }
        function Default_Base : longint ; virtual ; stdcall ; abstract ;

        { Disassembles the instructions at the specified physical memory
          address, for the specified number of bytes and directs the output to
          the passed stream.  Constant values are shown in the specified base.
          If base is 0, the CPU's default base is used. }
        function Disassemble( Address : int64 ; Base, Size : longint ;
            Stream : TCOM_Stream ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

        // Returns facility code
        function Facility_Code : longint ; virtual ; stdcall ; abstract ;

        { This method cosntructs and returns an assembler object for this CPU.
          It is passed a TMaster_Assembler object. }
        function Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;
            virtual ; stdcall ; abstract ;

        { Returns the CPU clock speed (in Hz). }
        function Get_Clock_Speed : longint ; virtual ; stdcall ; abstract ;

        { Returns the current CPU memory position (usually the Program Counter). }
        function Get_Current_Address( Space : longint ;
            Physical : boolean ) : int64 ; virtual ; stdcall ; abstract ;

        { Indicates the lowest physical memory address accessable by the CPU. }
        function Get_Low_Memory : int64 ; virtual ; stdcall ; abstract ;

        { Indicates the highest physical memory address accessable by the CPU. }
        function Get_High_Memory : int64 ; virtual ; stdcall ; abstract ;

        { Indicates the lowest port address accessable by the CPU for all port
          spaces. }
        function Get_Low_Port : int64 ; virtual ; stdcall ; abstract ;

        { Indicates the highest port address accessable by the CPU for all port
          spaces. }
        function Get_High_Port : int64 ; virtual ; stdcall ; abstract ;

        { Indicates the lowest virtual memory address accessable by the CPU. }
        function Get_Low_Virtual_Memory( Space : longint ) : int64 ;
            virtual ; stdcall ; abstract ;

        { Indicates the highest virtual memory address accessable by the CPU. }
        function Get_High_Virtual_Memory( Space : integer ) : int64 ; virtual ; stdcall ; abstract ;

        { Halts CPU execution.  The effect upon the CPU is implementation
          dependant. }
        procedure Halt ; virtual ; stdcall ; abstract ;

        { Returns true if the CPU is in a halted state (non-execution mode). }
        function Halted : boolean ; virtual ; stdcall ; abstract ;

        { Returns a description of the memory space with the specified index.
          It returns nil for any invalid index.  Physical is true for physical
          memory space and false for virtual address spaces.  Physical space 0
          is always valid. }
        function Memory_Space_Description( Index : longint ;
            Physical : boolean ) : PChar ; virtual ; stdcall ; abstract ;

        { Returns memory page size, in bytes.  Returns 0 if non-paging CPU. }
        function Page_Size : longint ; virtual ; stdcall ; abstract ;

        { Returns a description of the register associated with the passed
          index.  }
        function Register_Description( Index : longint ) : PChar ;
            virtual ; stdcall ; abstract ;

        { Returns the name corresponding to the register index passed.  A null
          string or NULL result indicates that the index is out of range.  Note
          that all registers start at offset 0. }
        function Register_Name( Index : longint ) : PChar ; virtual ; stdcall ; abstract ;

        { Returns the size (in bits) of the register associated with the passed
          index.  A result of 0 indicates that the index is out of range.  Note
          that all registers start at offset 0. }
        function Register_Size( Index : longint ) : integer ;
            virtual ; stdcall ; abstract ;

        // Power-on (cold) reset.
        procedure Restart ; virtual ; stdcall ; abstract ;

        { Begins CPU execution from the current CPU state.  Typically one
          instruction is executed, and then the CPU requests a block from the
          master clock. }
        procedure Run ; virtual ; stdcall ; abstract ;

        { The causes the CPU to execute instructions directly from the passed
          stream.  This is usually for immediate instruction execution from the
          user interface, thus no profiling is done, no breakpoints apply, and
          single-stepping does not occur. }
        procedure Run_From_Stream( Stream : TCOM_Stream ) ; virtual ; stdcall ; abstract ;

        { Sets a breakpoint at the specified address.  When the CPU is about to
          begin execution at this address, it will generate a breakpoint notice
          to the UI interface.  If Physical is true, the address is a physical
          address, otherwise it is a virtual address. }
        function Set_Breakpoint( Address : int64 ; Space : integer ;
            Physical : boolean ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

        { Sets the CPU clock speed (in Hz). }
        procedure Set_Clock_Speed( Value : longint ) ; virtual ; stdcall ; abstract ;

        { Sets the current CPU memory position. }
        procedure Set_Current_Address( Space : integer ; Physical : boolean ;
            Value : int64 ) ; virtual ; stdcall ; abstract ;

        { Sets a watchpoint at the specified address.  When the CPU is about to
                 access this address, it will generate a watchpoint notice to the UI
                 interface.  Access is the type of access:
           1 = Read or write (input or output)
           2 = Read (input)
           3 = Write (output)

           If Memory is true, the address applies to internal cache.  If Memory is
           false, address is the register. }
        function Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : integer ) : TUnified_Exception ; virtual ; stdcall ; abstract ;

        { Executes a single instruction based on the current CPU context.  This
          is the same as Run(), but execution doesn't continue after the CPU is
          unblocked.  If Into is true, only a single instruction is excecuted.
          If Into is false, and the instruction to be executed is a nested
          instruction (such as a subroutine call), execution continues until the
          execution returns to the instruction following the call. }
        procedure Step( Into : boolean ) ; virtual ; stdcall ; abstract ;

        { Stop the CPU execution. }
        procedure Stop ; virtual ; stdcall ; abstract ;

        { Returns True if the CPU supports virtual address mapping. }
        function Support_Virtual_Address : boolean ; virtual ; stdcall ; abstract ;

        { Returns address of top of the indexth stack. }
        function Top_Of_Stack( Index : longint ) : int64 ; virtual ; stdcall ; abstract ;

        { Returns the physical address for the passed virtual address in the
          indicated address space.  For a CPU with no virtual addresses, this
          returns the value passed.  If Space is -1, the current default address
          space is used. }
        function Translate( Space : longint ; Address : int64 ) : int64 ;
            virtual ; stdcall ; abstract ;

        { Returns the CEF specification version number to which this interface
          conforms. }
        function Version : integer ; virtual ; stdcall ; abstract ;


        // V2.2 and later:

        { Indicates the size (in bits) of the Indexth segment in the last
          assembled instruction.  A value of 0 indicates either an invalid Index
          or that the information is otherwise not available.  Index < 0 is not
          valid.  The result can be used to separate the various segments of
          assembled instructions.  For instance, a 2-byte instruction followed
          by two 2-byte operands would return 2 for indexes 0, 1, and 2. }
        function Segment_Size( Index : integer ) : integer ; virtual ; stdcall ; abstract ;

        { Declining feature.  Use Address_Representation_Ex instead. }
        function Address_Representation( Base : integer ;
            Address : int64 ) : PChar ; virtual ; stdcall ; abstract ;

        { Used to convert an address specification into a linear address value.
          B is ignored on call and is True on return if the address is a valid
          format.  Base is the base that any numeric portion of the address is
          assumed to be. }
        function Translate_Address( var B : boolean ; Base : integer ;
            Address : PChar ) : int64 ;
            virtual ; stdcall ; abstract ;

        { Indicates the lowest input port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. }
        function Get_Low_Input_Port( Space : integer ) : int64 ;
            virtual ; stdcall ; abstract ;

        { Indicates the highest input port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. }
        function Get_High_Input_Port( Space : integer ) : int64 ;
            virtual ; stdcall ; abstract ;

        { Indicates the lowest output port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. }
        function Get_Low_Output_Port( Space : integer ) : int64 ;
            virtual ; stdcall ; abstract ;

        { Indicates the highest output port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. }
        function Get_High_Output_Port( Space : integer ) : int64 ;
            virtual ; stdcall ; abstract ;

        { Returns an instance of a stack interface for accessing non-standard
          stacks.  Returns nil if no stack or standard stack handling. }
        function Get_Stack_Interface( Space : integer ) : TCEF_Stack_Interface ;
            virtual ; stdcall ; abstract ;

        // V2.6 and later...

        { Return memory component that the CPU executes/disassembles from and/or
          assembles to.  If the CPU uses the main memory, this should return
          nil.  This is used when the CPU has a code store that is separate from
          main system memory (such as for a microcode engine). }
        function Get_Target_Memory : TComponent ; virtual ; stdcall ; abstract ;

        { Returns a representation of the passed address, or nil if not
          supported, or the address is invalid.  This is used when addresses are
          displayed in a format not otherwise supported by CEF.  Base is the
          base to use for any numeric representation. C is the memory component
          associated with the address (or nil if main memory). }
        function Address_Representation_Ex( C : TComponent ; Base : integer ;
            Address : int64 ) : PChar ; virtual ; stdcall ; abstract ;

        { Registers a run-time system with the CPU, with the specified notice
          flags (see RTS_*). }
        function Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;
            virtual ; stdcall ; abstract ;

        { Returns detailed information about the specified register.  A result
          of nil indicates that the index is out of range.  Note that all
          registers start at offset 0. Note that the returned object is only
          valid until the next call to Register_Information. }
        function Register_Information( Index : longint ) : TData_Type ;
            virtual ; stdcall ; abstract ;

        { Returns the data/code store for the specified index.  Returns nil if
          there is no separate store for the index, or if the index is out of
          range.  The indexes correspond to the memory space(s) used by
          Memory_Space_Description.  This is used for CPUs that store code
          and/or data in separate address spaces. }
        function Get_Store( Index : longint ) : TComponent ;
            virtual ; stdcall ; abstract ;

        { Return memory address space that the CPU executes/disassembles from
          and/or assembles to.  If the CPU uses the main memory, this should
          return 0.  This is used, for instance, when the assembler directs
          compiled value to different code and data address spaces. }
        function Get_Target_Address_Space : longint ; virtual ; stdcall ; abstract ;
    end ; // TCPU


     // TRun_Time_System is used when a CPU needs to communicate with an emulator to
     // inform it of certain operations. V2.6
     TRun_Time_System = class
         public
            { Indicates a call to the passed address.  Returns true if the call
              was handled by the emulator, false if the CPU should make the call
              itself. }
            function Call( Address : int64 ) : boolean ; virtual ; stdcall ; abstract ;

            { Indicates that the specified interrupt/trap instruction is about
              to be processed. Returns true if the trap is handled by the
              emulator and that the CPU should skip it, false if the CPU should
              handle it normally. }
            function Trap( Address : int64 ) : boolean ; virtual ; stdcall ; abstract ;

            { Indicates that the CPU has jumped/branched to a new address. }
            procedure Jumped ; virtual ; stdcall ; abstract ;

            { Indicates that the CPU has executed a halt operation }
            procedure Halted ; virtual ; stdcall ; abstract ;

            { Indicates a pending register value change.  Returns value to
              assign. Index is the register index and Value is the new value. }
            function Register_Change( Index : longint ; Value : int64 ) : int64 ;
                virtual ; stdcall ; abstract ;

            { Indicates a return. }
            function Return : boolean ; virtual ; stdcall ; abstract ;
    end ; // TRun_Time_System



{ File layouts... }

{ Dump file layout }

type TDump_File_Header = record
        Signature : word ;
        Subsignature : byte ; { 0 = CEF }
        Version : byte ; { File format version number times 10 (12=1.2) }
        CEF_Type : byte ; { 0 = Dump }
        Stamp : longint ; { Optional packed date/time stamp }
        Start : int64 ; { First address in image }
        Filler : array[ 17..255 ] of byte ; { Filler - should be 0 }
    end ;


{ State file layout }

type TState_File_Header = record
        Signature : word ;
        Subsignature : byte ; { 0 = CEF }
        Version : byte ; { File format version number times 10 (1.2=12) }
        CEF_Type : byte ; { 1 = State }
        Stamp : longint ; { Optional packed date/time stamp }
        Filler : array[ 9..255 ] of byte ; { Filler - should be 0 }
    end ;



type TState_File_Sub_Header = record
        Facility : longint ; { Facility code of component }
        Serial_Number : longint ; { Serial number of component }
    end ;



type TCEF_Component_Query = class
                                public // API...
                                    function Query_Version : longint ;
                                        virtual ; stdcall ; abstract ;
                                    procedure Terminate ;
                                        virtual ; stdcall ; abstract ;

                                    function Component_Type : longint ;
                                        virtual ; stdcall ; abstract ;
                                    function Version : PChar ;
                                        virtual ; stdcall ; abstract ;
                                    function Emulates : PChar ;
                                        virtual ; stdcall ; abstract ; 
                            end ;


// Remote access classes:
const RAC_Command = 1 ; // Allow control of CEF framework
const RAC_Connection = 2 ; // Allow connection to emulated ports
const RAC_Connection_Query = 4 ; // Allow query of available ports
const RAC_Query = 8 ; // Allow query of CEF framework

type TCEF_Remote = class
                       public
                           // Return ID of remote access object
                           function Get_ID( var Size : longint ) : PChar ;
                               virtual ; stdcall ; abstract ;

                           // Set ID of remote access interface
                           procedure Set_ID( ID : PChar ; Length : longint ) ;
                               virtual ; stdcall ; abstract ;

                           // Return human-readable name of remote access object
                           function Name : PChar ; virtual ; stdcall ; abstract ;


                           // Install remote access interface
                           function Install : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;

                           // Uninstall remote access interface
                           function Uninstall : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;


                           // Set access
                           function Set_Access( Access : longint ) : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;

                           // Stop the component and close connection(s)
                           function Stop : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;

                           // Start the component as a server
                           function Start_Server : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;

                           // Start the component as a client
                           function Start_Client : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;

                           // Poll remote access interface - for ones that are
                           // synchronous.
                           procedure Poll ; virtual ; stdcall ; abstract ;

                           // Send a command to the remote
                           procedure Send_Command( Command : PChar ) ;
                               virtual ; stdcall ; abstract ;

                           // Send data to the remote
                           function Send_Data( Data: PChar ;
                               Size : Integer ) : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;

                           // Handle reception of a command from the remote.
                           procedure Receive_Command( Command : PChar ) ;
                               virtual ; stdcall ; abstract ;

                           // Handle reception of data from the remote.
                           procedure Receive_Data( Data: PChar ; Size : Integer ) ;
                               virtual ; stdcall ; abstract ;

                           procedure Terminate ;
                               virtual ; stdcall ; abstract ;

                           procedure Enable ;
                               virtual ; stdcall ; abstract ;

                           procedure Disable ;
                               virtual ; stdcall ; abstract ;
                   end ; // TCEF_Remote


implementation


end.
