{$N+}
{
        Program Name : CEF
        Package Name : CEF
        Purpose      : Base CEF classes for Pascal
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

          This unit defines base classes for Pascal, which descend from the
        base CEF abstract classes.  
}

unit CEF ;

interface

uses // C&C Subroutine Library...
     _UE, // TUnified_Exception
     _DebugIn, // TDebug_Interface
     _Streams, // TCOM_Stream
     TypeDefs, // TTri_State

     // CEF...
     _CEF ; // TUI_Interface

type TBase_UI_Interface = class( TUI_Interface )
         public
             { This method is called whenever a component blocks (Blocked is
               true) or unblocks (Blocked is false).  Component is the component
               in question. }
             procedure Block( Component : TComponent ; Blocked : boolean ) ;
                 override ; stdcall ;

             { This method is called when a CPU triggers a breakpoint.  The method
               returns True if the CPU is to continue execution of the instruction
               or False if not. CPU is the CPU component which triggered the
               breakpoint, Address is the execution address, and Physical indicates
               whether the address is physical or virtual.  Space is the index of
               the address space to assign the breakpoint to.  0 is the default
               address space. }
             function Breakpoint_Notice( Address : int64 ; Physical : boolean ;
                 Space : integer ; CPU : TComponent ) : boolean ; override ; stdcall ;

             { This function returns the instance of the Master clock object for the
               application.  NULL is a valid return value and should be tested by
               any code calling this program (indicating that there is no master
               clock and timing issues are to be ignored). }
             function Clock : TMaster_Clock ; override ; stdcall ;

             { Returns a deugger object for this object. }
             function Debugger : TDebug_Interface ; override ; stdcall ;

             { Returns a stream for the passed (partial) filename.  If file cannot
               be opened/accessed, nil is returned. }
             function Get_File_Stream( Name : PChar ) : TCOM_Stream ;
                 override ; stdcall ;

             // If Flag is true, the UI is hidden, otherwise it is made visible.
             procedure Hide( Flag : boolean ) ; override ; stdcall ;

             { Called by a blocked component (such as a CPU in wait state). }
             procedure Idle( Component : TComponent ) ; override ; stdcall ;

             { This logs an actual program error (exception or warning).  Severity
               is one of the following values:
                   0 = Informational message
                   1 = Warning
                   2 = Non-fatal error
                   3 = Fatal error.  The application should shut down. }
             procedure Log_Error( Component : TComponent ; Text : PChar ;
                 Severity : longint ) ; override ; stdcall ;

             { This logs a simulated hardware error.  Severity is one of the
                following values:
                        0 = Informational message
                        1 = Warning
                        2 = Non-fatal error
                        3 = Fatal simulated hardware failure. }
             procedure Log_Simulated_Error( Component : TComponent ; Text : PChar ;
                 Severity : longint ) ; override ; stdcall ;

             { A component calls this method in response to a Show_Status request.
               Each call provides one line of status information.  Index is 0 for
               the first line, 1 for the second, etc. }
             procedure Log_Status( Text : PChar ; Index : longint ) ;
                 override ; stdcall ;

             { Logs a trace event for the specified component. }
             procedure Log_Trace( Component : TComponent ; Description : PChar ) ;
                 override ; stdcall ;

             { Called when a component changes a signal state.  Index is the
               state index.  Active is true if the new state of the indexed
               signal is active.  All components that wanted to be notified of
               signal changes are notified via their Signal_Change_Notice
               method. }
             procedure Signal_Change_Notice( Component : TComponent ;
                 Index : longint ; Active : boolean ) ; override ; stdcall ;

             { Called when a component suffers some form of exception.  Note that
               CPUs that support exception handling must report those exceptions via
               State_Change_Notice. }
             procedure Signal_Exception( Component : TComponent ;
                 Description : PChar ; Index : longint ) ; override ; stdcall ;

             { Called when a component changes a state.  Index is the state index.
               Active is true if the new state of the indexed state is active. }
             procedure State_Change_Notice( Component : TComponent ;
                 Index : longint ; Active : boolean ) ; override ; stdcall ;

             // Terminates the UI, which usually terminates the application
             procedure Terminate ; override ; stdcall ;

             { Toggle the embededed state of the specified TUser_Interface
               component. }
             procedure Toggle_Embed( Component : TComponent ) ; override ; stdcall ;

             { Returns the CEF specification version number to which this interface
               conforms. }
             function Version : integer ; override ; stdcall ;

             { Indicates that the specified component wishes to be notified of
               signal changes (or not). }
             procedure Want_Signals( Component : TComponent ; Value : boolean ) ;
                 override ; stdcall ;

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
                 override ; stdcall ;

             // Port name for port Index across all loaded components.
             function Get_Port_Name( Index : longint ) : PChar ;
                 override ; stdcall ;

             // Port description for port Index across all loaded components.
             function Get_Port_Description( Index : longint ) : PChar ;
                 override ; stdcall ;

             // Port component for port Index across all loaded components.
             function Get_Port( Index : longint ) : TComponent ;
                override ; stdcall ;

             // Connected component for port Index across all loaded components.
             function Get_Port_Connection( Index : longint ) : TComponent ;
                 override ; stdcall ;

             // Parent of port Index.
             function Port_Parent_Component( Index : longint ) : TComponent ;
                 override ; stdcall ;

             // True = start running CPUs; False = pause CPUs
             procedure Run( State : boolean ) ; override ; stdcall ;

             // V2.2:

             // Obtain a process ID for a parallel process.  Returns -1 if no
             // (more) parallel processes are allowed (or not supported).  If
             // Force is True, an ID is always returns (unless not supported,
             // in which case -1 is returned).
             function Process_ID( Name : PChar ; Force : boolean ) : integer ;
                 override ; stdcall ;

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
                 var Priority : longint ) : boolean ; override ; stdcall ;

             // Indicates that the specified process ID is not running.
             procedure Process_End( ID : longint ) ; override ; stdcall ;

             // Indicates that the specified process has been destructed.
             procedure Process_Deleted( ID : longint ) ; override ; stdcall ;

             // V2.5 and later...

             // Prompt user and add a new port breakpoint.
             procedure Add_Port_Breakpoint ; override ; stdcall ;

             // Prompt user and add an execution breakpoint.
             procedure Add_Breakpoint ; override ; stdcall ;

             // Prompt user and add register breakpoint.
             procedure Add_Register_Breakpoint ; override ; stdcall ;

             // Prompt user and create a new memory breakpoint.
             procedure Create_New_Breakpoint ; override ; stdcall ;

             // V2.6 and later...
             
             // Return passed component name qualified by path, etc.
             function Get_Component_Filename( Name : PChar ) : PChar ;
                  override ; stdcall ;

             procedure Termination_Notice( C : TComponent ) ;
                  override ; stdcall ;

             // Load and return a component with the specified name.
             function Load_Component( Name : PChar ) : TComponent ;
                  override ; stdcall ;
     end ; // TBase_UI_Interface


     TBase_Profiler = class( TProfiler )
                     public
                         { Clears profiling information for specified domain.
                           If Domain is -1, all domains are cleared. }
                         procedure Clear( Domain : integer ) ;
                             override ; stdcall ;

                         { Returns profiling domain name for given index.  If
                           the index is invalid, the function returns nil.  A
                           domain indicates a type of profiling data.  For
                           instance, a CPU may track the number of calls to
                           certain addresses as one domain, and the number of
                           calls to specific types of instructions in another
                           domain, etc. }
                         function Domain_Name( Index : integer ) : PChar ;
                             override ; stdcall ;

                         { Returns a line of information associated with the
                           specified domain.  The first line is Index 0.  If an
                           invalid index is passed, the function returns nil. }
                         function Report_Line( Domain, Index : integer ) : PChar ;
                             override ; stdcall ;

                         { Returns the CEF specification version number to which
                           this interface conforms. }
                         function Version : integer ; override ; stdcall ;
                 end ; // TBase_Profiler


     TBase_Component = class( TComponent )
                           private // Instance data...
                               _Last_Error : TUnified_Exception ;

                           protected // Internal utility routines...
                               function Translate_Error( C : integer ) : string ;
                                   virtual ;
                               function Set_Error( C : integer ) : TUnified_Exception ;
                                   virtual ;

                           public // API...
            { This function returns a facility code for the component class. }
            function Facility_Code : longint ; override ; stdcall ;

            { Reinitalizes the component.  Note that this is not a constructor!
              It is not necessary for the application to call this method, since
              Make_Instance should have initialized the component appropriately.
              This simply allows the component to be initialized again. }
            function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                override ; stdcall ;

            { Terminates the component.  Note that this is not a destructor,
              although it should eventually cause the instance to be destructed.
              By convention, once this method is called, the instance is no
              longer valid. }
            function Terminate : TUnified_Exception ; override ; stdcall ;

            { Returns number assigned by component creator. }
            function Serial_Number : integer ; override ; stdcall ;

            { Adds the specified component to the list of components to notify
              along with Parent in Child_Notifications. }
            function Add_Notification( Component : TComponent ) : TUnified_Exception ;
                override ; stdcall ;

            function Cable : TCable ; override ; stdcall ;

            { This method returns the Indexth child component for this component.
              It returns nil if Index is outside of the range: 0 <= n <
              Max_child_component.  Child components are components owned by
              this component (such as a motherboard would own the CPU component). }
            function Child_Component( Index : longint ) : TComponent ;
                override ; stdcall ;

            { Indicates a notice from a child component.  Child is the component
              and Notice is a value from Child_Notice_*. }
            procedure Child_Notification( Child : TComponent ;
                var Notice : longint ; var Params : int64 ) ; override ; stdcall ;

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
                Access : longint ) : TUnified_Exception ; override ; stdcall ;

            { This method returns a value indicating the type of the component,
              as follows:
                            0 = Unknown (TComponent)
                            1 = CPU
                            2 = Bus
                            3 = Memory
                            4 = Motherboard
                            5 = I/O device
                            6 = User Interface }
            function Component_Type : longint ; override ; stdcall ;

            { For components which accept input, this connects another component
              to this component's default input port. }
            function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                override ; stdcall ;

            { For components which generate output, this connnects another
              component to this component's default output port. }
            function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                override ; stdcall ;

            function CPU : TCPU ; override ; stdcall ;

            { Returns debug interface for this component. }
            function Debugger : TDebug_Interface ; override ; stdcall ;

            { This removes the specified component from the list of components
              to be notified.  The component must have been added via
              Add_Notification. }
            function Delete_Notification( Component : TComponent ) : TUnified_Exception ;
                override ; stdcall ;

            { This method writes Size bits of data from the specified address
              from the specified buffer.  Memory is True if this is a memory
              write versus a I/O write.  This is solely for the UI, and the
              write does not trigger watchpoints or have any other side-effects
              other than changing the contents of the component. }
            function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                Memory : boolean ) : TUnified_Exception ; override ; stdcall ;

            { This disconnects the specified component from this component's
              default input port. }
            function Disconnect_Input( Component : TComponent ) : TUnified_Exception ;
                override ; stdcall ;

            { This disconnects the specified component from this component's
              default output port. }
            function Disconnect_Output( Component : TComponent ) : TUnified_Exception ;
                override ; stdcall ;

            { This method reads Size bits of data from the specified address
              into the specified buffer.  Size is updated with the actual number
              of bits copied.  Memory is True if this is a memory read versus an
              I/O read.  This is solely for the UI - the read triggers no
              watchpoints and has no other side-effects.  It also ignores access
              modes. }
            function Examine( Address : int64 ; var Size : longint ;
                Buffer : pointer ; Memory : boolean ) : TUnified_Exception ; override ; stdcall ;

            { Returns the access mode of the specified address.  Return values
              are:
                            0 = Not supported (address out of range, etc.)
                            1 = Read/Write (I/O)
                            2 = Read-only (input-only)
                            3 = Write-only (write-only)
              Memory is true if address is a memory address and false if it is
              an I/O address. }
            function Get_Access_Mode( Address : int64 ;
                Memory : boolean ) : longint ; override ; stdcall ;

            { Provides a description of the exception conditions signaled via
              Signal_Exception. }
            function Get_Exception_Description( Index : longint ) : PChar ;
                override ; stdcall ;

            function Get_Parent : TComponent ; override ; stdcall ;

            { Returns True if profiling for this component is on. }
            function Get_Profiling : boolean ; override ; stdcall ;

            { Returns the component's default read latency, in nanoseconds.  If
              the component doesn't support latency, this should return 0. }
            function Get_Read_Latency : longint ; override ; stdcall ;

            { This returns the state of a signal with the specified name.  If
              the component recognizes the signal name, it returns True.
              Otherwise it returns False.  If the result is True, State is set
              to the current value of the signal. }
            function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                override ; stdcall ;

            { Get the name of a component state.  If index is invalid, the
              function returns nil. }
            function Get_State_Name( Index : longint ) : PChar ;
                override ; stdcall ;

            function Get_Tag : longint ; override ; stdcall ;

            function Get_Trace : boolean ; override ; stdcall ;

            { Returns the component's default write latency, in nanoseconds.  If
              the component doesn't support latency, this should return 0. }
            function Get_Write_Latency : longint ; override ; stdcall ;

            { This method returns the Indexth input component for this component.
              It returns NULL if Index is outside of the range: 0 <= n <
              Max_input_component }
            function Input_Component( Index : longint ) : TComponent ;
                override ; stdcall ;

            function Keyboard : TKeyboard ; override ; stdcall ;

            function Memory : TMemory ; override ; stdcall ;

            { This method returns a string containing the name of this component. }
            function Name : PChar ; override ; stdcall ;

            { This method returns the Indexth output component for this
              component.  It returns NULL if Index is outside of the range:
              0 <= n < Max_output_component }
            function Output_Component( Index : longint ) : TComponent ;
                override ; stdcall ;

            function Profiler : TProfiler ; override ; stdcall ;

            { Calling this method requests the component to transmit data to its
              default output port.  IO_Type is the type of read operation (see
              IO_Type_*).  Size is the number of bits being requested (0=default
              for component).  The component should verify that the address is
              within its range before responding.  If not, it should do nothing.
              If the component will respond to the request, the method returns
              true, otherwise it returns false. }
            function Read( Address : int64 ; Size : longint ;
                IO_Type : longint ) : boolean ; override ; stdcall ;

            { Reset the component.  Behavior is implementation dependant. }
            procedure Reset ; override ; stdcall ;

            { This method causes the component to restore its contents (but NOT
              state) from the passed stream.  It is the responsibility of the
              component to restore the contents of any child components (but not
              attached components). }
            function Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                override ; stdcall ;

            { This method causes the component to restore its current state (but
              NOT contents) from the passed stream.  It is the responsibility of
              the component to restore the state of any child components (but
              not attached components). }
            function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                override ; stdcall ;

            { This method causes the component to save its contents (but NOT
              state) to the passed stream.  It is the responsibility of the
              component to save the contents of any child components (but not
              attached components). }
            function Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;
                override ; stdcall ;

            { This method causes the component to save its current state (but
              NOT contents) to the passed stream.  It is the responsibility of
              the component to save the state of any child components (but not
              attached components). }
            function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;
                override ; stdcall ;

            { Sets the access mode of the specified address range (Low to High,
              inclusive).  Typ is:
                            1 = Read/Write (I/O)
                            2 = Read-only (input-only)
                            3 = Write-only (write-only)
              Memory is true if address is a memory address and false if it is
              an I/O address. }
            function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                Typ : longint ) : TUnified_Exception ; override ; stdcall ;

            procedure Set_Parent( Component : TComponent ) ; override ; stdcall ;

            { Turns profiling for this component on or off.  Profiling is off by
              default.  If Children is true, all child components will be
              assigned the same profiling value (this doesn't affect input or
              output components). }
            procedure Set_Profiling( _On, Children : boolean ) ;
                override ; stdcall ;

            { Sets the component's default read latency in nanoseconds.  If the
              component supports latency, it should block by this amount on each
              read. }
            procedure Set_Read_Latency( Value : longint ) ; override ; stdcall ;

            { This sets the state of a signal with the specified name.  This
              should only be called when the signal state actually changes (that
              is, State should never be the same on two consecutive calls). }
            procedure Set_Signal( Name : PChar ; State : boolean ) ;
                override ; stdcall ;

            { User-defined value associated with component. }
            procedure Set_Tag( Value : longint ) ; override ; stdcall ;

            procedure Set_Trace( Value : boolean ) ; override ; stdcall ;

            { An component-specific initialization string can be sent to the
              component after initialization, via this method. }
            procedure Set_Up( P : PChar ) ; override ; stdcall ;

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
                Access : longint ) : TUnified_Exception ; override ; stdcall ;

            { Sets the component's default write latency in nanoseconds.  If the
              component supports latency, it should block by this amount on each
              write. }
            procedure Set_Write_Latency( Value : longint ) ;  override ; stdcall ;

            { Calling this method causes the component to report its status to
              the UI interface, in a human-readable form.  For instance, a CPU
              component would report its registers and their contents. }
            procedure Show_Status ; override ; stdcall ;

            { Called when the specified component has a signal change.  This is
              only called if this component requested signal notices from the
              UI. }
            procedure Signal_Change_Notice( Component : TComponent ;
                Index : longint ; Active : boolean ) ; override ; stdcall ;

            { Returns the number of defined signals. }
            function Signal_Count : longint ; override ; stdcall ;

            { Returns the name of the supported signals.  The first index is 0.
              If there is no signal for the specified index, or index is less
              than 0, the result is nil. }
            function Signal_Name( Index : longint ) : PChar ; override ; stdcall ;

            { Returns True if the signal is an output from the component.
              Result is indefined if Index is invalid. }
            function Signal_Out( Index : longint ) : boolean ; override ; stdcall ;

            { Returns True if the signal is active-low.  Result is undefined if
              Index is invalid. }
            function Signal_Active_Low( Index : longint ) : boolean ;
                override ; stdcall ;

            { Returns the index of the signal with the specified name.  If the
              name is invalid, -1 is returned. }
            function Signal_Index( Name : PChar ) : integer ; override ; stdcall ;

            { Returns True if the component supports the specified feature.  If
              the component does not inherently support the feature (such as a
              CPU feature for a memory component), the result is undefined. }
            function Support_Feature( ID : longint ) : boolean ;
                override ; stdcall ;

            function User_Interface : TUser_Interface ;
                override ; stdcall ;

            { Returns the CEF specification version number to which this
              interface conforms. }
            function Version : longint ; override ; stdcall ;

            { Calling this allows a component to resume its activity after it
              sends a Block request to the Master clock. }
            procedure Wake ; override ; stdcall ;

            { Calling this method writes Value to the component's input port
              with the specified address.  IO_Type is the type of write
              operation (see IO_Type_*).  Size is the number of bits in Value to
              write (0=default for component) and may be 0 to 32, inclusive.
              The component should verify that the address is within its range
              before responding.  If not, it should do nothing, and return no
              errors. }
            function Write( Address : int64 ; Value, Size : longint ;
                IO_Type : longint ) : TUnified_Exception ; override ; stdcall ;

            { This is the same as calling Write_Byte once for each byte in the
              string, incrementing Address once per byte.  It is provided to
              allow more efficient means of transmitting large amounts of data.
              Size is the number of bits in Value to write (0=default for
              component).  The component should verify that the address is
              within its range before responding.  If not, it should do nothing,
              and return no errors. }
            function Write_String( Address : int64 ; Value : PChar ;
                Size : longint ; IO_Type : longint ) : TUnified_Exception ; override ; stdcall ;

            function Get_Port_Name( Index : longint ) : PChar ;
                override ; stdcall ;

            function Get_Port_Description( Index : longint ) : PChar ;
                override ; stdcall ;

            function Get_Port( Index : longint ) : TComponent ;
                override ; stdcall ;

            function Get_Port_Connection( Index : longint ) : TComponent ;
                override ; stdcall ;

            { Notice from TUI_Interface component.  Code is UI_Notice_Code_*
              code.  Data depends on the calue of Code. }
            procedure UI_Notice( Code : longint ; var Data : int64 ) ;
                override ; stdcall ;

            { Returns True if the component responds to to I/O type Typ at
              address Address.  Examine is True to check for response to
              examines/deposits, and False for normal reads/writes. }
            function Respond_To_Address( Address : int64 ; Typ : integer ;
                Examine : boolean ) : boolean ; override ; stdcall ;

            { V2.6 or later... }
            function Get_Logger : TCEF_Logger ; override ; stdcall ;

            procedure Set_Logger( Value : TCEF_Logger ) ; override ;

            procedure Set_Last_Error( Value : TUnified_Exception ) ;
                 virtual ;

            public // Properties...
                property Last_Error : TUnified_Exception
                    read _Last_Error
                    write Set_Last_Error ;
        end ; // TBase_Component


    TBase_Master_Clock = class( TMaster_Clock )
                             public // API...
        { This procedure registers a request for component to receive a Wake
           signal at the specified time delta (in nanoseconds).  If component is
           nil, the next item in the queue is unblocked. }
	    procedure Block( Component : TComponent ; Time_Delta : int64 ) ;
             override ; stdcall ;

         { Returns debug interface for this clock. }
         function Debugger : TDebug_Interface ; override ; stdcall ;

	    { Initializes the clock to time index 0 and sets the UI interface. }
	    procedure Initialize( UI : TUI_Interface ) ; override ; stdcall ;

	    { This function returns the current simulated master clock (which is
           measured in nanoseconds) time index. }
	    function Get_Time_Index : int64 ; override ; stdcall ;

         { Returns value passed by creator. }
         function Serial_Number : integer ; override ; stdcall ;

         { Returns True if the clock supports the specified feature.  If the
           clock does not inherently support the feature (such as a CPU
           feature), the result is undefined. }
         function Support_Feature( ID : integer ) : boolean ;
             override ; stdcall ;

         { Returns the CEF specification version number to which this interface
           conforms. }
         function Version : integer ; override ; stdcall ;

         { V2.3 or later... }

         { Set clock mode.  See MCM_*  }
         procedure Set_Mode( M : integer ) ; override ; stdcall ;

         { Fet clock mode.  See MCM_*  }
         function Get_Mode : integer ; override ; stdcall ;

         { Unblock all blocked components }
         procedure Unblock ; override ; stdcall ;

         procedure Terminate ; override ; stdcall ;
    end ; // TBase_Master_Clock


     TBase_Cable = class( TCable )
                public // API...
                    { Returns true if this cable is a serial cable.  It is a
                      parallel cable otherwise. }
                    function Serial : boolean ; override ; stdcall ;

                    { Returns name of the protocol supported by the cable (nil
                      if generic/any). }
                    function Protocol : PChar ; override ; stdcall ;

                    { Transmit one data item.  Speed is the bit rate of the
                      transfer (0=automatch), Value is the data to transmit,
                      Data_Size is the number of bits in Value (32 max),
                      Stop_Bits indicates the number of stop bits. }
                    function Transmit( Speed : int64 ;
                        Value, Data_Size, Stop_Bits : longint ) : TUnified_Exception ;
                        override ; stdcall ;

                    { Transmits a string of data by calling Transmit once for
                      each data item in Value. }
                    function Transmit_String( Speed : int64 ; Value : PChar ;
                        Data_Size, Stop_Bits : longint ) : TUnified_Exception ;
                        override ; stdcall ;

                    { Called when received data from another cable component.
                      Source is the component that the data is coming from.
                      Speed is the bits/second data rate.  Value is the data
                      being sent from the source.  Data_Size is the size of the
                      passed data, in bits (32 max).  Stop_Bits is the number of
                      stop bits (when appropriate). }
                    procedure Receive( Source : TComponent ; Speed : int64 ;
                        Value, Data_Size, Stop_Bits : integer ) ;
                        override ; stdcall ;

                    function Get_Data( var Speed : int64 ;
                        var Value, Data_Size, Stop_Bits : integer ) : boolean ;
                        override ; stdcall ;
             end ; // TBase_Cable


     TBase_User_Interface = class( TUser_Interface )
                                private // Instance data...
                                    _Last_Error : TUnified_Exception ;
                                    
                                protected // Internal utility routines...
                                    function Translate_Error( C : integer ) : string ;
                                        virtual ;
                                    function Set_Error( C : integer ) : TUnified_Exception ;
                                        virtual ;

                                public // API...
                                    { This function returns a facility code for the component class. }
                                    function Facility_Code : longint ; virtual ;

                               function Get_Hidden : boolean ;
                                   override ; stdcall ;
                               procedure Set_Hidden( Value : boolean ) ;
                                   override ; stdcall ;

                               function Get_Parent_Window : THandle ;
                                   override ; stdcall ;
                               procedure Set_Parent_Window( Value : THandle ) ;
                                   override ; stdcall ;

                               function Get_Caption : PChar ;
                                   override ; stdcall ;
                               procedure Set_Caption( Value : PChar ) ;
                                   override ; stdcall ;

                               procedure Set_Size( Height, Width : integer ) ;
                                   override ; stdcall ;

                               function Optimal_Height : integer ;
                                   override ; stdcall ;

                               function Optimal_Width : integer ;
                                   override ; stdcall ;

                               { Returns the CEF specification version number to
                                 which this interface conforms. }
                               function Version : integer ; override ; stdcall ;

                               // V2.4 and later...

                               procedure Initialize ; override ; stdcall ;

                               procedure Set_Last_Error( Value : TUnified_Exception ) ;
                                    virtual ;

                           public // Properties...
                               property Last_Error : TUnified_Exception
                                   read _Last_Error
                                   write Set_Last_Error ;
                       end ; // TBase_User_Interface


    TBase_Keyboard = class( TKeyboard )
                    public
                        { Retrieves the next key in a keystroke from the
                          keyboard.  Returns nil or null string if end of
                          current keystroke. }
                        function Get_Key : PChar ; override ; stdcall ;

                        { Returns True if the specified key is down in the
                          pending keystroke.  If Name is nil, returns True if
                          any key is down. }
                        function Get_Key_Down( Name : PChar ) : boolean ;
                            override ; stdcall ;

                        { Sets the state of a given key to up or down.  The key
                          is reset when the next keystroke is returned.  This
                          can be used to synchronize the keyboard component's
                          visible state with the physical keyboard. }
                        procedure Set_Key_Down( Name : PChar ; State : boolean ) ;
                            override ; stdcall ;

                        { Returns the name of the key associated with the
                          passed index.  Returns nil if the index is invalid. }
                        function Get_Key_Name( Index : integer ) : PChar ;
                            override ; stdcall ;

                        { Returns True if the specified LED is lit.  If the LED
                          name is not recognized, the function returns false. }
                        function Get_LED_State( Name : PChar ) : boolean ;
                            override ; stdcall ;

                        { Sets the state of a given LED to lit or not.  This
                          can be used to synchronize the keyboard component's
                          visible state with the physical keyboard. }
                        procedure Set_LED_State( Name : PChar ; State : boolean ) ;
                            override ; stdcall ;

                        { Returns the name of the LED associated with the
                          passed index.  Returns nil if the index is invalid. }
                        function Get_LED_Name( Index : integer ) : PChar ;
                            override ; stdcall ;

                        { Returns the CEF specification version number to which this interface
                          conforms. }
                        function Version : integer ; override ; stdcall ;
                end ;


     TBase_Memory = class( TMemory )
                        private // Instance data...
                            _Last_Error : TUnified_Exception ;

                        protected // Internal utility routines...
                            function Translate_Error( C : integer ) : string ;
                                virtual ;
                            function Set_Error( C : integer ) : TUnified_Exception ;
                                virtual ;

                        public // API...
                            { This function returns a facility code for the component class. }
                            function Facility_Code : longint ; override ;

                           { Dumps an image of memory between Start and Start + Size - 1, inclusive,
                             to the passed buffer, which must be large enough to hold Size bytes.
                             Any range of addresses that are outside the memory's range are zero
                             filled. }
                           procedure Dump( Start, Size : int64 ; Buffer : pointer ) ;
                               override ; stdcall ;

                           { Returns the set of addresses that this component
                             responds to. }
                           procedure Get_Address_Range( var Low, High : int64 ) ;
                               override ; stdcall ;

                           { Loads memory between Start and Start + Size - 1, inclusive,
                             with the data in the passed buffer.  Any data outside
                             the memory's range is ignored. }
                           procedure Load( Start, Size : int64 ; Buffer : pointer ) ;
                               override ; stdcall ;

                           { Defines the set of addresses that this component r
                             esponds to. }
                           function Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;
                               override ; stdcall ;

                           { Returns the CEF specification version number to which
                             this interface conforms. }
                           function Version : integer ; override ; stdcall ;

                           // V2.6...
                           function Map_Virtual_To_Physical( Virt : int64 ) : int64 ;
                               override ; stdcall ;

                           procedure Set_Last_Error( Value : TUnified_Exception ) ;
                                virtual ;

                        public // Properties...
                            property Last_Error : TUnified_Exception
                                read _Last_Error
                                write Set_Last_Error ;
                    end ; { TBase_Memory }


     TBase_CEF_Assembler_Context = class( TCEF_Assembler_Context )
                                       private // Instance data....
                                           _Last_Error : TUnified_Exception ;

                                       public // API...
                                      procedure Initialize ; override ; stdcall ;

                                      procedure Terminate ; override ; stdcall ;

                                      function Add_Mapping( Filename : PChar ;
                                          Address : int64 ;
                                          Line : integer ) : integer ;
                                          override ; stdcall ;

                                      function Add_Symbol( Name : PChar ;
                                          P : pSymbol_Record ) : TUnified_Exception ;
                                          override ; stdcall ;

                                      procedure Delete( Sym : PChar ) ;
                                          override ; stdcall ;

                                      procedure Delete_Mapping( Index : integer ) ;
                                          override ; stdcall ;

                                      function Find( Sym : PChar ;
                                          var Addr : int64 ;
                                          var Flg, D_T, Siz : longint ;
                                          var Dat : pointer ) : integer ;
                                          override ; stdcall ;

                                      { Returns filename and line number
                                         corresponding to the passed address.  If
                                         there is no mapping, it returns -1. }
                                      function Mapping( Address : int64 ;
                                          var Filename : PChar ) : integer ;
                                          override ; stdcall ;

                                      procedure Pop_Level ; override ; stdcall ;

                                      procedure Push_Level ; override ; stdcall ;

                                      { Get value associated with passed symbol.  If
                                        symbol is unknown, it returns False. }
                                      function Symbol_Value( Name : PChar ;
                                          var Value : int64 ) : boolean ;
                                          override ; stdcall ;

                                      { Get size associated with passed symbol.  If
                                        symbol is unknown, it returns False. }
                                      function Symbol_Size( Name : PChar ;
                                          var Value : integer ) : boolean ;
                                          override ; stdcall ;

                                      function Get_Case_Sensitive : boolean ;
                                          override ; stdcall ;

                                      procedure Set_Case_Sensitive( Value : boolean ) ;
                                          override ; stdcall ;

                                      // V2.2:

                                      function Find_First( var Sym : PChar ;
                                          var Addr : int64 ;
                                          var Flg, D_T, Siz : longint ;
                                          var Dat : pointer ) : integer ;
                                          override ; stdcall ;

                                      function Find_Next( var Sym : PChar ;
                                          var Addr : int64 ;
                                          var Flg, D_T, Siz : longint ;
                                          var Dat : pointer ) : integer ;
                                          override ; stdcall ;

                                      protected // Property handlers...
                                          procedure Set_Last_Error( Value : TUnified_Exception ) ;

                              end ; // TBase_CEF_Assembler_Context

     TBase_Master_Assembler = class( TMaster_Assembler )
                             private // Instance data...
                                 _Last_Error : TUnified_Exception ;
                                 
// Internal utility routines...

                             protected
                                 function Translate_Error( C : integer ) : string ;

                                 function Set_Error( C : integer ) : TUnified_Exception ;

                             public // API...
        { Adds a reference for back-patching. Use Add_Reference_Ex instead. }
        procedure Add_Reference( Name : PChar ; Size : longint ;
            Address : int64 ) ; override ; stdcall ;

        { Adds a symbol to the current scope, with the name in Name and the
          specified information. }
        function Add_Symbol( Name : PChar ; P : pSymbol_Record ) : TUnified_Exception ;
            override ; stdcall ;

        { An old version of Assemble_Ex without Flags.  See Assemble_Ex for
          details. }
        function Assemble( Input, Output, Listing : TCOM_Stream ;
            Status : TAssembler_Status ) : TUnified_Exception ; override ; stdcall ;

        { Apply all backpatches. }
        procedure Backpatch( Status : TAssembler_Status ;
            Stream : TCOM_Stream ) ; override ; stdcall ;

        { Evaluates an expression, returning the numeric value in Value. }
        function Evaluate( X : PChar ; var Value : int64 ) : TUnified_Exception ;
            override ; stdcall ;

        { This method is used during assmebly by a TAssembler to insert assembly
          source in-place.  For instance, translating a directive into a
          standard directive, or doing macro expansions, etc.  If the expanded
          item is a directive that results in output, Res will be that output
          and Res_Length will be the size of that data, in bytes. }
        function Expand( Source : PChar ; var Res : PChar ;
            var Res_Length : longint ; Status : TAssembler_Status ) : TUnified_Exception ;
            override ; stdcall ;

        { Returns facility code for class }
        function Facility_Code : longint ; override ; stdcall ;

        { Returns True if case sensitivity to identifier names is on. }
        function Get_Case_Sensitive : boolean ; override ; stdcall ;

        { Returns the symbol record for the identifier in Name.  Returns NULL,
          if Name is undeclared. }
        function Get_Symbol( Name : PChar ) : PSymbol_Record ;
            override ; stdcall ;

        { Returns the next token in the input stream during an assembly.
          Returns a null string if an error occurs (such as no more input).
          This is useful for assemblers which need more input for a directive
          which is continued on additional lines. }
        function Get_Token : PChar ; override ; stdcall ;

        { Returns entire current (remaining) input line. }
        function Grab_Line( Current : boolean ) : PChar ; override ; stdcall ;

        { Inserts the data from Input into the current token stream. }
        procedure In_Line( Input : TCOM_Stream ) ; override ; stdcall ;

        { Logs an assembly error.  Severity indicates the seriousness of the
          error:
                        0 = Informational
                        1 = Warning
                        2 = Error
                        3 = Fatal error (abort assembly) }
        procedure Log_Error( Text : PChar ; Severity : longint ) ; override ; stdcall ;

        // Map current token position to the passed address.
        procedure Map( Address : int64 ) ; override ; stdcall ;

        { Return next token without removing from the token stream.  If Same_Line
          is True, only return the next token on the current line. }
        function Peek_Token( Same_Line : boolean ) : PChar ; override ; stdcall ;

        { Ends the current sub-scope for identifiers. }
        procedure Pop_Scope ; override ; stdcall ;

        { Begins a sub-scope for identifiers within the current scope. }
        procedure Push_Scope ; override ; stdcall ;

        { Returns a token to the input stream during an assembly. }
        procedure Put_Token( Token : PChar ) ; override ; stdcall ;

        { Sets case sensitivity on identifier names. }
        procedure Set_Case_Sensitive( Value : boolean ) ; override ; stdcall ;

        // Remove previous mapping added with Map().
        procedure UnMap ; override ; stdcall ;

        { Returns the CEF specification version number to which this interface
          conforms. }
        function Version : integer ; override ; stdcall ;

        { Adds a reference for back-patching.  Context is a caller-defined
          value which is passed back via the Backpatching method of TAssembler.
          Options indicate processing options (currently reserved for future
          use). Size is the size of the value to backpatch, in bytes. }
        procedure Add_Reference_Ex( Name : PChar ; Size : longint ;
            Address : int64 ; Context, Options : longint ) ; override ; stdcall ;

        { Adds a CPU component to the assembler's list of CPUs for the
          .SET_TARGET_CPU directive. }
        procedure Add_CPU( CPU : TComponent ; Name : PChar ) ; override ; stdcall ;

        // Removes all CPUs added with Add_CPU.
        procedure Clear_CPUs ; override ; stdcall ;

        procedure Set_Assembler_Context( Value : TCEF_Assembler_Context ) ;
            override ; stdcall ;

        function Get_Assembler_Context : TCEF_Assembler_Context ;
            override ; stdcall ;

        procedure Terminate ; override ; stdcall ;

        // V2.2:

        function Leading_Whitespace : boolean ; override ; stdcall ;

        { Assembles code from the Input stream, directing the image data to the
          Output stream, and placing any list output to the Listing stream.
          Note that Listing can be NULL if no listing is desired.  Output can be
          NULL to do a syntax-check-only assembly.  Status is used to provide
          assembly statistics.  See ASF_* for flag values... }
        function Assemble_Ex( Input, Output, Listing : TCOM_Stream ;
            Status : TAssembler_Status ; Flags : longint ) : TUnified_Exception ;
            override ; stdcall ;

        { Same as evaluate, but the result has PC_Adjustment added to any
          references to the current PC. }
        function Evaluate_Ex( Value : PChar ; var _Result : int64 ;
            PC_Adjustment : int64 ) : TUnified_Exception ; override ; stdcall ;


        function Get_Base : integer ; override ; stdcall ;

        procedure Set_Base( Value : integer ) ; override ; stdcall ;

        procedure Register_Extension( Extension : TAssembler_Extension ) ;
            override ; stdcall ;

            protected
                procedure Set_Last_Error( Value : TUnified_Exception ) ;

            public // Properties...
                property Last_Error : TUnified_Exception
                    read _Last_Error
                    write Set_Last_Error ;
     end ; // TBase_Master_Assembler


     TBase_Assembler = class( TAssembler )
                           private // Instance data...
                               _Last_Error : TUnified_Exception ;

                           protected // Internal utility routines...
                               function Translate_Error( C : integer ) : string ;
                                   virtual ;
                               function Set_Error( C : integer ) : TUnified_Exception ;
                                   virtual ;

        public // API...
        { Informs the assembler the a new assembly operation is beginning.  This
          assembly will use the passed master assembler. }
        procedure Initialize( Master : TMaster_Assembler ) ; override ; stdcall ;

        { Informs the assembler that the current assembly operation is now
          complete. }
        procedure Terminate ; override ; stdcall ;

        { An old version of Assemble_Ex without Flags.  See Assemble_Ex for
          details. }
        function Assemble( inputs : PChar ; var outputs, machines : PChar ;
            var MachineL : longint ; var Address : int64 ;
            var Segment : longint ; Status : TAssembler_Status ) : TUnified_Exception ;
            override ; stdcall ;

        { Returns the default radix (base) of numeric literals. }
        function Default_Radix : longint ; override ; stdcall ;

        { Returns the default size of numeric literals, in bits. }
        function Default_Size : longint ; override ; stdcall ;

        { Returns facility code for this class. }
        function Facility_Code : longint ; override ; stdcall ;

        { Return a bar-delimited list of source file extensions.  The first
          one should be the default. }
        function Source_Extensions : PChar ; override ; stdcall ;

        { Returns a list of valid characters for starting a symbol name. }
        function Valid_Symbol_Initial : PChar ; override ; stdcall ;

        { Returns a list of valid characters for a symbol name, past the first. }
        function Valid_Symbol_After : PChar ; override ; stdcall ;

        { Returns the CEF specification version number to which this interface
          conforms. }
        function Version : integer ; override ; stdcall ;

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
            override ; stdcall ;

        // V2.2:

        function Normalize_Expression( Expression : PChar ; Numeric : boolean ;
            Status : TAssembler_Status ) : PChar ; override ; stdcall ;

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
            Flags : longint ) : TUnified_Exception ; override ; stdcall ;

        // V2.6:
        { Informs the assembler that an assembly operation is about to begin.
          The return value is a context that is passed to Finish_Assembly. }
        function Begin_Assembly : int64 ; override ; stdcall ;

        { Informs the assembler that the last assembly operation has completed.
          The value from Begin_Assembly is passed.  See Assemble_Ex for flags
          meanings. }
        function Finish_Assembly( Context : int64 ; var outputs, machines : PChar ;
            var MachineL : longint ; var Address : int64 ;
            Status : TAssembler_Status ; Flags : longint ) : TUnified_Exception ;
            override ; stdcall ;

        { When the master assembler is about to allocate data (eg due to a .DB
          directive), it calls the CPU assembler with the address and size of
          the data.  The assembler can then adjust the address, if necessary,
          and return the address to which the data is to be placed.  If there is
          no place for the data, -1 is returned. }
        function Request_Data( Address, Size : int64 ) : int64 ;
            override ; stdcall ;

        protected // Internal utility routines
            function Evaluate( const X : string ;
                var _Result : longint ; PC_Adjustment : integer ) : TUnified_Exception ;
                virtual ;

            function Evaluate_Symbol( O : string ; _Master : TMaster_Assembler ;
                PC_Adjustment, Size, Patch_Address : longint ;
                Immediate_Mode : boolean ; var C : longint ) : TUnified_Exception ;
                virtual ;

            procedure Set_Last_Error( Value : TUnified_Exception ) ;
                 virtual ;

        public // Properties...
            property Last_Error : TUnified_Exception
                read _Last_Error
                write Set_Last_Error ;
     end ; { TBase_Assembler }


     TBase_CPU = class( TCPU )
                     private // Instance data...
                         _Last_Error : TUnified_Exception ;

                     public // Internal utility routines...
                         function Translate_Error( C : integer ) : string ;
                             virtual ;
                         function Set_Error( C : integer ) : TUnified_Exception ;
                             virtual ;

                     public // API...
        { Returns true if the CPU is big-endian.  Returns False if small-endian. }
        function Big_Endian : boolean ; override ; stdcall ;

        { Clears a breakpoint at the specified address.  If Physical is true,
          the address is a physical address, otherwise it is a virtual
          address. }
        function Cancel_Breakpoint( Address : int64 ; Space : longint ;
            Physical : boolean ) : TUnified_Exception ; override ; stdcall ;

        { Clears a watchpoint at the specified address.  Access is the type of
          access:
          1 = Read or write (input or output)
          2 = Read (input)
          3 = Write (output)

          If Memory is true, the address is an internal cache address.  Otherwise
          it is a register index. }
        function Clear_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : longint ) : TUnified_Exception ; override ; stdcall ;

        { Indicates the specified base used by the CPU instruction set. }
        function Default_Base : longint ; override ; stdcall ;

        { Disassembles the instructions at the specified physical memory
          address, for the specified number of bytes and directs the output to
          the passed stream.  Constant values are shown in the specified base.
          If base is 0, the CPU's default base is used. }
        function Disassemble( Address : int64 ; Base, Size : longint ;
            Stream : TCOM_Stream ) : TUnified_Exception ; override ; stdcall ;

        // Returns facility code
        function Facility_Code : longint ; override ; stdcall ;

        { This method cosntructs and returns an assembler object for this CPU.
          It is passed a TMaster_Assembler object. }
        function Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;
            override ; stdcall ;

        { Returns the CPU clock speed (in Hz). }
        function Get_Clock_Speed : longint ; override ; stdcall ;

        { Returns the current CPU memory position (usually the Program Counter). }
        function Get_Current_Address( Space : longint ;
            Physical : boolean ) : int64 ; override ; stdcall ;

        { Indicates the lowest physical memory address accessable by the CPU. }
        function Get_Low_Memory : int64 ; override ; stdcall ;

        { Indicates the highest physical memory address accessable by the CPU. }
        function Get_High_Memory : int64 ; override ; stdcall ;

        { Indicates the lowest port address accessable by the CPU for all port
          spaces. }
        function Get_Low_Port : int64 ; override ; stdcall ;

        { Indicates the highest port address accessable by the CPU for all port
          spaces. }
        function Get_High_Port : int64 ; override ; stdcall ;

        { Indicates the lowest virtual memory address accessable by the CPU. }
        function Get_Low_Virtual_Memory( Space : longint ) : int64 ;
            override ; stdcall ;

        { Indicates the highest virtual memory address accessable by the CPU. }
        function Get_High_Virtual_Memory( Space : integer ) : int64 ; override ; stdcall ;

        { Halts CPU execution.  The effect upon the CPU is implementation
          dependant. }
        procedure Halt ; override ; stdcall ;

        { Returns true if the CPU is in a halted state (non-execution mode). }
        function Halted : boolean ; override ; stdcall ;

        { Returns a description of the memory space with the specified index.
          It returns nil for any invalid index.  Physical is true for physical
          memory space and false for virtual address spaces.  Physical space 0
          is always valid. }
        function Memory_Space_Description( Index : longint ;
            Physical : boolean ) : PChar ; override ; stdcall ;

        { Returns memory page size, in bytes.  Returns 0 if non-paging CPU. }
        function Page_Size : longint ; override ; stdcall ;

        { Returns a description of the register associated with the passed
          index.  }
        function Register_Description( Index : longint ) : PChar ;
            override ; stdcall ;

        { Returns the name corresponding to the register index passed.  A null
          string or NULL result indicates that the index is out of range.  Note
          that all registers start at offset 0. }
        function Register_Name( Index : longint ) : PChar ; override ; stdcall ;

        { Returns the size (in bits) of the register associated with the passed
          index.  A result of 0 indicates that the index is out of range.  Note
          that all registers start at offset 0. }
        function Register_Size( Index : longint ) : integer ;
            override ; stdcall ;

        // Power-on (cold) reset.
        procedure Restart ; override ; stdcall ;

        { Begins CPU execution from the current CPU state.  Typically one
          instruction is executed, and then the CPU requests a block from the
          master clock. }
        procedure Run ; override ; stdcall ;

        { The causes the CPU to execute instructions directly from the passed
          stream.  This is usually for immediate instruction execution from the
          user interface, thus no profiling is done, no breakpoints apply, and
          single-stepping does not occur. }
        procedure Run_From_Stream( Stream : TCOM_Stream ) ; override ; stdcall ;

        { Sets a breakpoint at the specified address.  When the CPU is about to
          begin execution at this address, it will generate a breakpoint notice
          to the UI interface.  If Physical is true, the address is a physical
          address, otherwise it is a virtual address. }
        function Set_Breakpoint( Address : int64 ; Space : integer ;
            Physical : boolean ) : TUnified_Exception ; override ; stdcall ;

        { Sets the CPU clock speed (in Hz). }
        procedure Set_Clock_Speed( Value : longint ) ; override ; stdcall ;

        { Sets the current CPU memory position. }
        procedure Set_Current_Address( Space : integer ; Physical : boolean ;
            Value : int64 ) ; override ; stdcall ;

        { Sets a watchpoint at the specified address.  When the CPU is about to
                 access this address, it will generate a watchpoint notice to the UI
                 interface.  Access is the type of access:
           1 = Read or write (input or output)
           2 = Read (input)
           3 = Write (output)

           If Memory is true, the address applies to internal cache.  If Memory is
           false, address is the register. }
        function Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
            Access : integer ) : TUnified_Exception ; override ; stdcall ;

        { Executes a single instruction based on the current CPU context.  This
          is the same as Run(), but execution doesn't continue after the CPU is
          unblocked.  If Into is true, only a single instruction is excecuted.
          If Into is false, and the instruction to be executed is a nested
          instruction (such as a subroutine call), execution continues until the
          execution returns to the instruction following the call. }
        procedure Step( Into : boolean ) ; override ; stdcall ;

        { Stop the CPU execution. }
        procedure Stop ; override ; stdcall ;

        { Returns True if the CPU supports virtual address mapping. }
        function Support_Virtual_Address : boolean ; override ; stdcall ;

        { Returns address of top of the indexth stack. }
        function Top_Of_Stack( Index : longint ) : int64 ; override ; stdcall ;

        { Returns the physical address for the passed virtual address in the
          indicated address space.  For a CPU with no virtual addresses, this
          returns the value passed.  If Space is -1, the current default address
          space is used. }
        function Translate( Space : longint ; Address : int64 ) : int64 ;
            override ; stdcall ;

        { Returns the CEF specification version number to which this interface
          conforms. }
        function Version : integer ; override ; stdcall ;


        // V2.2 and later:

        { Indicates the size (in bits) of the Indexth segment in the last
          assembled instruction.  A value of 0 indicates either an invalid Index
          or that the information is otherwise not available.  Index < 0 is not
          valid.  The result can be used to separate the various segments of
          assembled instructions.  For instance, a 2-byte instruction followed
          by two 2-byte operands would return 2 for indexes 0, 1, and 2. }
        function Segment_Size( Index : integer ) : integer ; override ; stdcall ;

        { Declining feature.  Use Address_Representation_Ex instead. }
        function Address_Representation( Base : integer ;
            Address : int64 ) : PChar ; override ; stdcall ;

        { Used to convert an address specification into a linear address value.
          B is ignored on call and is True on return if the address is a valid
          format.  Base is the base that any numeric portion of the address is
          assumed to be. }
        function Translate_Address( var B : boolean ; Base : integer ;
            Address : PChar ) : int64 ;
            override ; stdcall ;

        { Indicates the lowest input port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. }
        function Get_Low_Input_Port( Space : integer ) : int64 ;
            override ; stdcall ;

        { Indicates the highest input port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. }
        function Get_High_Input_Port( Space : integer ) : int64 ;
            override ; stdcall ;

        { Indicates the lowest output port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. }
        function Get_Low_Output_Port( Space : integer ) : int64 ;
            override ; stdcall ;

        { Indicates the highest output port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. }
        function Get_High_Output_Port( Space : integer ) : int64 ;
            override ; stdcall ;

        { Returns an instance of a stack interface for accessing non-standard
          stacks.  Returns nil if no stack or standard stack handling. }
        function Get_Stack_Interface( Space : integer ) : TCEF_Stack_Interface ;
            override ; stdcall ;

        // V2.6 and later...

        { Return memory component that the CPU executes/disassembles from and/or
          assembles to.  If the CPU uses the main memory, this should return
          nil.  This is used when the CPU has a code store that is separate from
          main system memory (such as for a microcode engine). }
        function Get_Target_Memory : TComponent ; override ; stdcall ;

        { Returns a representation of the passed address, or nil if not
          supported, or the address is invalid.  This is used when addresses are
          displayed in a format not otherwise supported by CEF.  Base is the
          base to use for any numeric representation. C is the memory component
          associated with the address (or nil if main memory). }
        function Address_Representation_Ex( C : TComponent ; Base : integer ;
            Address : int64 ) : PChar ; override ; stdcall ;

        { Registers a run-time system with the CPU, with the specified notice
          flags (see RTS_*). }
        function Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;
            override ; stdcall ;

        { Returns detailed information about the specified register.  A result
          of nil indicates that the index is out of range.  Note that all
          registers start at offset 0. Note that the returned object is only
          valid until the next call to Register_Information. }
        function Register_Information( Index : longint ) : TData_Type ;
            override ; stdcall ;

        { Returns the data/code store for the specified index.  Returns nil if
          there is no separate store for the index, or if the index is out of
          range.  The indexes correspond to the memory space(s) used by
          Memory_Space_Description.  This is used for CPUs that store code
          and/or data in separate address spaces. }
        function Get_Store( Index : longint ) : TComponent ;
            override ; stdcall ;

        { Return memory address space that the CPU executes/disassembles from
          and/or assembles to.  If the CPU uses the main memory, this should
          return 0.  This is used, for instance, when the assembler directs
          compiled value to different code and data address spaces. }
        function Get_Target_Address_Space : longint ; override ; stdcall ;
        
            procedure Set_Last_Error( Value : TUnified_Exception ) ;
                 virtual ;

            property Last_Error : TUnified_Exception
                read _Last_Error
                write Set_Last_Error ;
    end ; // TCPU

    TBase_CEF_Stack_Interface = class( TCEF_Stack_Interface )
                                    public // API...
                                        procedure Terminate ; // Destruct the object
                                            override ; stdcall ;

                                        // Lowest possible stack entry address...
                                        function Low_Bound : int64 ;
                                            override ; stdcall ;

                                        // Highest possible stack entry address...
                                        function High_Bound : int64 ;
                                            override ; stdcall ;

                                        // Size of each stack entry, in bits
                                        function Item_Size : longint ;
                                            override ; stdcall ;

                                        // Returns Indexth item in stack
                                        function Value( Index : int64 ) : int64 ;
                                            override ; stdcall ;

                                        // V2.6...

                                        // True if the stack grows up, false if it grows down
                                        function Grow_Up : boolean ;
                                            override ; stdcall ;
                                end ;


function Create_Component_Query( _Type : longint ; Version, Emulates : string ) : TCEF_Component_Query ;


implementation

uses // Borland...
     SysUtils, // strtoint

     // C&C Subroutine Library...
     CVT, // Valid_Base
     UE ; // Create_Simple_UE

// TCEF_Stack_Interface methods...

// API...

procedure TBase_CEF_Stack_Interface.Terminate ;

begin
end ;


function TBase_CEF_Stack_Interface.Low_Bound : int64 ;

begin
    Result := 0 ;
end ;


function TBase_CEF_Stack_Interface.High_Bound : int64 ;

begin
    Result := 0 ;
end ;


function TBase_CEF_Stack_Interface.Item_Size : longint ;

begin
    Result := 1 ;
end ;


function TBase_CEF_Stack_Interface.Value( Index : int64 ) : int64 ;

begin
    Result := 0 ;
end ;


function TBase_CEF_Stack_Interface.Grow_Up : boolean ;

begin
    Result := False ;
end ;


type TDefault_Data_Type = class( TData_Type )
                              public // API...
                                  _Endian : boolean ;
                                  _Size : longint ;

                                  // General...
                                  function Family : longint ; override ;
                                  function Data_Type : longint ; override ;
                                  function Size : longint ; override ;
                                  function Big_Endian : boolean ; override ;
                                  function Max_Size : longint ; override ;

                                  // Numeric types...
                                  function Signed : TTri_State ; override ;
                                  function Mantissa : longint ; override ;
                                  function Exponent : longint ; override ;
                                  function Fixed : boolean ; override ;
                                  function Fixed_Position : longint ; override ;
                                  function Pack : boolean ; override ;

                                  // String types...
                                  function Length_Encoding : longint ; override ;
                                  function Prefix_Size : longint ; override ;
                                  function Encoding : longint ; override ;
                          end ;

// API...

function TDefault_Data_Type.Family : longint ;

begin
    Result := DataType_Family_Numeric ;
end ;


function TDefault_Data_Type.Data_Type : longint ;

begin
    Result := DataType_Integer ;
end ;


function TDefault_Data_Type.Size : longint ;

begin
    Result := _Size ;
end ;


function TDefault_Data_Type.Big_Endian : boolean ;

begin
    Result := _Endian ;
end ;


function TDefault_Data_Type.Max_Size : longint ;

begin
    Result := Size ;
end ;


function TDefault_Data_Type.Signed : TTri_State ;

begin
    Result := TS_Dont_Care ; // Could be either
end ;


function TDefault_Data_Type.Mantissa : longint ;

begin
    Result := _Size ;
end ;


function TDefault_Data_Type.Exponent : longint ;

begin
    Result := 0 ;
end ;


function TDefault_Data_Type.Fixed : boolean ;

begin
    Result := False ;
end ;


function TDefault_Data_Type.Fixed_Position : longint ;

begin
    Result := 0 ;
end ;


function TDefault_Data_Type.Pack : boolean ;

begin
    Result := False ;
end ;


function TDefault_Data_Type.Length_Encoding : longint ;

begin
    Result := 0 ;
end ;


function TDefault_Data_Type.Prefix_Size : longint ;

begin
    Result := 0 ;
end ;


function TDefault_Data_Type.Encoding : longint ;

begin
    Result := 0 ;
end ;



{ Note that Borland Pascal doesn't allow pure virtual base classes, so we must
  define a generic implementation of each of class. }

{ TBase_UI_Interface methods... }

procedure TBase_UI_Interface.Block( Component : TComponent ; Blocked : boolean ) ;

begin
end ;


function TBase_UI_Interface.Breakpoint_Notice( Address : int64 ; Physical : boolean ;
    Space : integer ; CPU : TComponent ) : boolean ;

begin
    Breakpoint_Notice := False ;
end ;


function TBase_UI_Interface.Clock : TMaster_Clock ;

begin
    Clock := nil ;
end ;


procedure TBase_UI_Interface.Log_Error( Component : TComponent ; Text : PChar ;
    Severity : longint ) ;

begin
end ;


procedure TBase_UI_Interface.Log_Simulated_Error( Component : TComponent ;
    Text : PChar ; Severity : longint ) ;

begin
end ;


procedure TBase_UI_Interface.Log_Status( Text : PChar ; Index : longint ) ;

begin
    writeln( Text ) ;
end ;


function TBase_UI_Interface.Debugger : TDebug_Interface ;

begin
    Result := nil ;
end ;


function TBase_UI_Interface.Version : integer ;

begin
    Version := Interface_Version ;
end ;


procedure TBase_UI_Interface.Watchpoint_Notice( Address : int64 ;
    Access, Tag : longint ; Component : TComponent ;
    Memory, Internal, Port : boolean ) ;

begin
end ;


function TBase_UI_Interface.Get_Port_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_UI_Interface.Get_Port_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_UI_Interface.Get_Port( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TBase_UI_Interface.Get_Port_Connection( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TBase_UI_Interface.Port_Parent_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


procedure TBase_UI_Interface.Run( State : boolean ) ;

begin
end ;


function TBase_UI_Interface.Process_ID( Name : PChar ; Force : boolean ) : integer ;

begin
    Result := -1 ; // No threading by default
end ;


function TBase_UI_Interface.Process_Start( ID : longint ; var Priority : longint ) : boolean ;

begin
    Priority := 0 ;
    Result := True ;
end ;


procedure TBase_UI_Interface.Process_End( ID : longint ) ;

begin
end ;


procedure TBase_UI_Interface.Process_Deleted( ID : longint ) ;

begin
end ;


procedure TBase_UI_Interface.Terminate ;

begin
end ;


procedure TBase_UI_Interface.Hide( Flag : boolean ) ;

begin
end ;


procedure TBase_UI_Interface.Idle( Component : TComponent ) ;

begin
end ;


procedure TBase_UI_Interface.State_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

begin
end ;


procedure TBase_UI_Interface.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

begin
end ;


procedure TBase_UI_Interface.Signal_Exception( Component : TComponent ;
    Description : PChar ; Index : longint ) ;

begin
end ;


procedure TBase_UI_Interface.Log_Trace( Component : TComponent ;
    Description : PChar ) ;

begin
end ;


function TBase_UI_Interface.Get_File_Stream( Name : PChar ) : TCOM_Stream ;

begin
    Result := nil ;
end ;


procedure TBase_UI_Interface.Toggle_Embed( Component : TComponent ) ;

begin
end ;


procedure TBase_UI_Interface.Want_Signals( Component : TComponent ;
    Value : boolean ) ;

begin
end ;


procedure TBase_UI_Interface.Add_Port_Breakpoint ;

begin
end ;


procedure TBase_UI_Interface.Add_Breakpoint ;

begin
end ;


procedure TBase_UI_Interface.Add_Register_Breakpoint ;

begin
end ;


procedure TBase_UI_Interface.Create_New_Breakpoint ;

begin
end ;


function TBase_UI_Interface.Get_Component_Filename( Name : PChar ) : PChar ;

begin
    Result := Name ;
end ;


procedure TBase_UI_Interface.Termination_Notice( C : TComponent ) ;

begin
end ;


function TBase_UI_Interface.Load_Component( Name : PChar ) : TComponent ;

begin
    Result := nil ;
end ;



{ TBase_Component methods... }

function TBase_Component.Translate_Error( C : integer ) : string ;

begin
    Result := '' ;
end ;


function TBase_Component.Set_Error( C : integer ) : TUnified_Exception ;

begin
    if( C = 0 ) then
    begin
        Last_Error := nil ;
    end else
    begin
        Last_Error := Create_Simple_UE( Facility_Code, 10, C, UE_Error, Translate_Error( C ), '' ) ;
    end ;
    Result := Last_Error ;
end ;


function TBase_Component.Facility_Code : longint ;

begin
    Facility_Code := ComponentErr_Facility ;
end ;


function TBase_Component.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


procedure TBase_Component.Set_Last_Error( Value : TUnified_Exception ) ;

begin
    if( Value <> nil ) then
    begin
        Value.Attach ;
    end ;
    if( _Last_Error <> nil ) then
    begin
        _Last_Error.Detach ;
    end ;
    _Last_Error := Value ;
end ;


function TBase_Component.Terminate : TUnified_Exception ;

var N : longint ;
    P : int64 ;

begin
    Result := nil ;
    if( Parent <> nil ) then
    begin
        N := Child_Notice_Terminating ;
        Child_Notification( self, N, P ) ;
    end ;
end ;


function TBase_Component.Serial_Number : integer ;

begin
    Result := 0 ;
end ;


function TBase_Component.Add_Notification( Component : TComponent ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TBase_Component.Cable : TCable ;

begin
    Result := nil ;
end ;


function TBase_Component.Child_Component( Index : longint ) : TComponent ;

begin
    Child_Component := nil ;
end ;


function TBase_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_No_Watchpoints, UE_ERROR, 'No watchpoints', '' ) ;
end ;


function TBase_Component.Component_Type : longint ;

begin
    Result := Component_Type_Unknown ;
end ;


function TBase_Component.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Invalid_Operation, UE_ERROR, 'Invalid operation', '' ) ;
end ;


function TBase_Component.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Invalid_Operation, UE_ERROR, 'Invalid operation', '' ) ;
end ;


function TBase_Component.CPU : TCPU ;

begin
    Result := nil ;
end ;


function TBase_Component.Debugger : TDebug_Interface ;

begin
    Debugger := nil ; // Not supported
end ;


function TBase_Component.Delete_Notification( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Component_Not_Found, UE_ERROR, 'Component not found', '' ) ;
end ;


function TBase_Component.Deposit( Address : int64 ; Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Address_Out_Of_Range, UE_ERROR, 'Address out of range', '' ) ;
end ;


function TBase_Component.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Component_Not_Found, UE_ERROR, 'Component not found', '' ) ;
end ;


function TBase_Component.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Component_Not_Found, UE_ERROR, 'Component not found', '' ) ;
end ;


function TBase_Component.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Address_Out_Of_Range, UE_ERROR, 'Address out of range', '' ) ;
end ;


function TBase_Component.Get_Access_Mode( Address : int64 ; Memory : boolean ) : longint ;

begin
    Get_Access_Mode := Access_None ;
end ;


function TBase_Component.Get_Profiling : boolean ;

begin
    Get_Profiling := False ;
end ;


function TBase_Component.Get_Read_Latency : longint ;

begin
    Get_Read_Latency := 0 ;
end ;


function TBase_Component.Get_Write_Latency : longint ;

begin
    Get_Write_Latency := 0 ;
end ;


function TBase_Component.Input_Component( Index : longint ) : TComponent ;

begin
    Input_Component := nil ;
end ;


function TBase_Component.Keyboard : TKeyboard ;

begin
    Result := nil ;
end ;


function TBase_Component.Memory : TMemory ;

begin
    Result := nil ;
end ;


function TBase_Component.Name : PChar ;

begin
    Name := nil ;
end ;


function TBase_Component.Output_Component( Index : longint ) : TComponent ;

begin
    Output_Component := nil ;
end ;


function TBase_Component.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Read := False ;
end ;


function TBase_Component.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TBase_Component.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TBase_Component.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TBase_Component.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TBase_Component.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Address_Out_Of_Range, UE_ERROR, 'Address out of range', '' ) ;
end ;


procedure TBase_Component.Set_Profiling( _On, Children : boolean ) ;

begin
end ;


procedure TBase_Component.Set_Read_Latency( Value : longint ) ;

begin
end ;


function TBase_Component.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, ComponentErr_Invalid_Operation, UE_ERROR, 'Invalid operation', '' ) ;
end ;


procedure TBase_Component.Set_Write_Latency( Value : longint ) ;

begin
end ;


procedure TBase_Component.Show_Status ;

begin
end ;


procedure TBase_Component.Set_Up( P : PChar ) ;

begin
end ;


function TBase_Component.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


function TBase_Component.User_Interface : TUser_Interface ;

begin
    Result := nil ;
end ;


function TBase_Component.Version : integer ;

begin
    Version := Interface_Version ;
end ;


procedure TBase_Component.Wake ;

begin
end ;


function TBase_Component.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : longint ) : TUnified_Exception ;

begin
    Write := Deposit( Address, Size, @Value, ( IO_Type = IO_Type_Memory ) ) ;
end ;


function TBase_Component.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

var Loop : integer ;

begin
    Result := nil ;
    for Loop := 0 to ( ( Size + 7 ) div 8 ) - 1 do
    begin
	    Write_String := Write( Address, ord( Value[ Loop ] ), 8, IO_Type ) ;
	    Address := Address + 1 ;
    end ;
end ;


procedure TBase_Component.Child_Notification( Child : TComponent ;
    var Notice : longint ; var Params : int64 ) ;

begin
end ;


procedure TBase_Component.Set_Signal( Name : PChar ; State : boolean ) ;

begin
end ;


function TBase_Component.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

begin
    Result := False ;
end ;


function TBase_Component.Signal_Count : longint ;

begin
    Result := 0 ;
end ;


function TBase_Component.Signal_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_Component.Signal_Out( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TBase_Component.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TBase_Component.Signal_Index( Name : PChar ) : integer ;

begin
    Result := -1 ;
end ;


procedure TBase_Component.Reset ;

begin
end ;


function TBase_Component.Get_State_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_Component.Get_Exception_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


procedure TBase_Component.Set_Tag( Value : longint ) ;

begin
end ;


function TBase_Component.Get_Tag : longint ;

begin
    Result := 0 ;
end ;


function TBase_Component.Get_Parent : TComponent ;

begin
    Result := nil ;
end ;


procedure TBase_Component.Set_Parent( Component : TComponent ) ;

begin
end ;


function TBase_Component.Profiler : TProfiler ;

begin
    Result := nil ;
end ;


function TBase_Component.Get_Trace : boolean ;

begin
    Result := False ;
end ;


procedure TBase_Component.Set_Trace( Value : boolean ) ;

begin
end ;


procedure TBase_Component.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

begin
end ;


function TBase_Component.Get_Port_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_Component.Get_Port_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_Component.Get_Port( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TBase_Component.Get_Port_Connection( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


procedure TBase_Component.UI_Notice( Code : longint ; var Data : int64 ) ;

begin
end ;


function TBase_Component.Respond_To_Address( Address : int64 ; Typ : integer ;
    Examine : boolean ) : boolean ;

var L, H : int64 ;

begin
    Result := False ;
    if( ( not Examine ) and ( Typ = IO_Type_Memory ) and ( Memory <> nil ) ) then
    begin
        Memory.Get_Address_Range( L, H ) ;
        Result := ( Address >= L ) and ( Address <= H ) ;
    end ;
end ;


function TBase_Component.Get_Logger : TCEF_Logger ;

begin
    Result := nil ;
end ;


procedure TBase_Component.Set_Logger( Value : TCEF_Logger ) ;

begin
end ;



{ TBase_Master_Clock methods... }

procedure TBase_Master_Clock.Block( Component : TComponent ; Time_Delta : int64 ) ;

begin
    Component.Wake ;
end ;


procedure TBase_Master_Clock.Initialize( UI : TUI_Interface ) ;

begin
end ;


function TBase_Master_Clock.Get_Time_Index : int64 ;

begin
    Get_Time_Index := 0 ;
end ;


function TBase_Master_Clock.Serial_Number : integer ;

begin
    Result := 0 ;
end ;


function TBase_Master_Clock.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


function TBase_Master_Clock.Debugger : TDebug_Interface ;

begin
    Result := nil ;
end ;


function TBase_Master_Clock.Version : integer ;

begin
    Version := Interface_Version ;
end ;


procedure TBase_Master_Clock.Set_Mode( M : integer ) ;

begin
    // This routine left intentionally blank
end ;


function TBase_Master_Clock.Get_Mode : integer ;

begin
    Result := MCM_Default ;
end ;


procedure TBase_Master_Clock.Unblock ;

begin
end ;


procedure TBase_Master_Clock.Terminate ;

begin
    Free ;
end ;



{ TBase_Memory methods...}

// Internal utility routines...

function TBase_Memory.Translate_Error( C : integer ) : string ;

begin
    Result := '' ;
end ;


function TBase_Memory.Set_Error( C : integer ) : TUnified_Exception ;

begin
    if( C = 0 ) then
    begin
        Last_Error := nil ;
    end else
    begin
        Last_Error := Create_Simple_UE( Facility_Code, 10, C, UE_Error, Translate_Error( C ), '' ) ;
    end ;
    Result := Last_Error ;
end ;


function TBase_Memory.Facility_Code : longint ;

begin
    Facility_Code := ComponentErr_Facility ;
end ;


// API...

function TBase_Memory.Set_Address_Range( Low, High : int64 ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


procedure TBase_Memory.Get_Address_Range( var Low, High : int64 ) ;

begin
end ;


procedure TBase_Memory.Dump( Start, Size : int64 ; Buffer : pointer ) ;

begin
end ;


procedure TBase_Memory.Load( Start, Size : int64 ; Buffer : pointer ) ;

begin
end ;


function TBase_Memory.Version : integer ;

begin
    Version := Interface_Version ;
end ;


function TBase_Memory.Map_Virtual_To_Physical( Virt : int64 ) : int64 ;

begin
    Result := 0 ;
end ;


procedure TBase_Memory.Set_Last_Error( Value : TUnified_Exception ) ;

begin
    if( Value <> nil ) then
    begin
        Value.Attach ;
    end ;
    if( _Last_Error <> nil ) then
    begin
        _Last_Error.Detach ;
    end ;
    _Last_Error := Value ;
end ;



// TBase_CEF_Assembler_Context methods...

procedure TBase_CEF_Assembler_Context.Initialize ;

begin
end ;


procedure TBase_CEF_Assembler_Context.Terminate ;

begin
    Free ;
end ;


function TBase_CEF_Assembler_Context.Add_Mapping( Filename : PChar ;
    Address : int64 ; Line : integer ) : integer ;

begin
    Result := -1 ;
end ;


function TBase_CEF_Assembler_Context.Add_Symbol( Name : PChar ; P : pSymbol_Record ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


procedure TBase_CEF_Assembler_Context.Delete( Sym : PChar ) ;

begin
end ;


procedure TBase_CEF_Assembler_Context.Delete_Mapping( Index : integer ) ;

begin
end ;


function TBase_CEF_Assembler_Context.Find( Sym : PChar ; var Addr : int64 ;
    var Flg, D_T, Siz : longint ; var Dat : pointer ) : integer ;

begin
    Result := -1 ;
end ;


function TBase_CEF_Assembler_Context.Mapping( Address : int64 ;
    var Filename : PChar ) : integer ;

begin
    Result := 0 ;
end ;


procedure TBase_CEF_Assembler_Context.Pop_Level ;

begin
end ;


procedure TBase_CEF_Assembler_Context.Push_Level ;

begin
end ;


function TBase_CEF_Assembler_Context.Symbol_Value( Name : PChar ;
    var Value : int64 ) : boolean ;

begin
    Result := False ;
end ;


function TBase_CEF_Assembler_Context.Symbol_Size( Name : PChar ;
    var Value : integer ) : boolean ;

begin
    Result := False ;
end ;


function TBase_CEF_Assembler_Context.Get_Case_Sensitive : boolean ;

begin
    Result := False ;
end ;


procedure TBase_CEF_Assembler_Context.Set_Case_Sensitive( Value : boolean ) ;

begin
end ;


function TBase_CEF_Assembler_Context.Find_First( var Sym : PChar ; var Addr : int64 ;
    var Flg, D_T, Siz : longint ; var Dat : pointer ) : integer ;

begin
    Result := -1 ;
end ;


function TBase_CEF_Assembler_Context.Find_Next( var Sym : PChar ; var Addr : int64 ;
    var Flg, D_T, Siz : longint ; var Dat : pointer ) : integer ;

begin
    Result := -1 ;
end ;


procedure TBase_CEF_Assembler_Context.Set_Last_Error( Value : TUnified_Exception ) ;

begin
    if( Value <> nil ) then
    begin
        Value.Attach ;
    end ;
    if( _Last_Error <> nil ) then
    begin
        _Last_Error.Detach ;
    end ;
    _Last_Error := Value ;
end ;


{ TMaster_Assembler methods... }

// Internal utility routines...

function TBase_Master_Assembler.Translate_Error( C : integer ) : string ;

begin
    Result := '' ;
end ;


function TBase_Master_Assembler.Set_Error( C : integer ) : TUnified_Exception ;

begin
    if( C = 0 ) then
    begin
        Last_Error := nil ;
    end else
    begin
        Last_Error := Create_Simple_UE( Facility_Code, 10, C, UE_Error, Translate_Error( C ), '' ) ;
    end ;
    Result := Last_Error ;
end ;


// API...

function TBase_Master_Assembler.Assemble( Input, Output, Listing : TCOM_Stream ;
    Status : TAssembler_Status ) : TUnified_Exception ;

begin
    Result := Assemble_Ex( Input, Output, Listing, Status, 0 ) ;
end ;


procedure TBase_Master_Assembler.Add_Reference( Name : PChar ; Size : longint ;
    Address : int64 ) ;

begin
    Add_Reference_Ex( Name, Size, Address, 0, 0 ) ;
end ;


procedure TBase_Master_Assembler.Backpatch( Status : TAssembler_Status ;
    Stream : TCOM_Stream ) ;

begin
end ;


function TBase_Master_Assembler.Evaluate( X : PChar ; var Value : int64 ) : TUnified_Exception ;

begin
    Result := Evaluate_Ex( X, Value, 0 ) ;
end ;


function TBase_Master_Assembler.Evaluate_Ex( Value : PChar ; var _Result : int64 ;
    PC_Adjustment : int64 ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TBase_Master_Assembler.Expand( Source : PChar ; var Res : PChar ;
    var Res_Length : longint ; Status : TAssembler_Status ) : TUnified_Exception ;


begin
    Result := nil ;
end ;


function TBase_Master_Assembler.Facility_Code : longint ;

begin
    Result := MasterAssemblerErr_Facility ;
end ;


procedure TBase_Master_Assembler.Log_Error( Text : PChar ; Severity : longint ) ;

begin
    writeln( Text ) ;
end ;


procedure TBase_Master_Assembler.In_Line( Input : TCOM_Stream ) ;

begin
end ;


function TBase_Master_Assembler.Get_Token : PChar ;

begin
    Get_Token := nil ;
end ;


procedure TBase_Master_Assembler.Put_Token( Token : PChar ) ;

begin
end ;


function TBase_Master_Assembler.Peek_Token( Same_Line : boolean ) : PChar ;

begin
   Peek_Token := nil ;
end ;


procedure TBase_Master_Assembler.Push_Scope ;

begin
end ;


procedure TBase_Master_Assembler.Pop_Scope ;

begin
end ;


function TBase_Master_Assembler.Get_Symbol( Name : PChar ) : PSymbol_Record ;

begin
    Get_Symbol := nil ;
end ;


function TBase_Master_Assembler.Add_Symbol( Name : PChar ; P : pSymbol_Record ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


procedure TBase_Master_Assembler.Set_Case_Sensitive( Value : boolean ) ;

begin
end ;


function TBase_Master_Assembler.Get_Case_Sensitive : boolean ;

begin
    Get_Case_Sensitive := False ;
end ;


function TBase_Master_Assembler.Grab_Line( Current : boolean ) : PChar ;

begin
    Result := nil ;
end ;


procedure TBase_Master_Assembler.Map( Address : int64 ) ;

begin
end ;


procedure TBase_Master_Assembler.UnMap ;

begin
end ;


function TBase_Master_Assembler.Version : integer ;

begin
    Version := Interface_Version ;
end ;


procedure TBase_Master_Assembler.Add_Reference_Ex( Name : PChar ; Size : longint ;
    Address : int64 ; Context, Options : longint ) ;

begin
end ;


procedure TBase_Master_Assembler.Add_CPU( CPU : TComponent ; Name : PChar ) ;

begin
end ;


procedure TBase_Master_Assembler.Clear_CPUs ;

begin
end ;


function TBase_Master_Assembler.Get_Assembler_Context : TCEF_Assembler_Context ;

begin
    Result := nil ;
end ;


procedure TBase_Master_Assembler.Set_Assembler_Context( Value : TCEF_Assembler_Context ) ;

begin
end ;


procedure TBase_Master_Assembler.Terminate ;

begin
    Free ;
end ;


function TBase_Master_Assembler.Leading_Whitespace : boolean ;

begin
    Result := False ;
end ;


function TBase_Master_Assembler.Assemble_Ex( Input, Output, Listing : TCOM_Stream ;
    Status : TAssembler_Status ; Flags : longint ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TBase_Master_Assembler.Get_Base : integer ;

begin
    Result := 10 ;
end ;


procedure TBase_Master_Assembler.Set_Base( Value : integer ) ;

begin
end ;


procedure TBase_Master_Assembler.Register_Extension( Extension : TAssembler_Extension ) ;

begin
end ;


procedure TBase_Master_Assembler.Set_Last_Error( Value : TUnified_Exception ) ;

begin
    if( Value <> nil ) then
    begin
        Value.Attach ;
    end ;
    if( _Last_Error <> nil ) then
    begin
        _Last_Error.Detach ;
    end ;
    _Last_Error := Value ;
end ;


{ TAssembler methods... }

// Internal utility routines...

function TBase_Assembler.Translate_Error( C : integer ) : string ;

begin
    Result := '' ;
end ;


function TBase_Assembler.Set_Error( C : integer ) : TUnified_Exception ;

begin
    if( C = 0 ) then
    begin
        Last_Error := nil ;
    end else
    begin
        Last_Error := Create_Simple_UE( Facility_Code, 10, C, UE_Error, Translate_Error( C ), '' ) ;
    end ;
    Result := Last_Error ;
end ;


// API...

procedure TBase_Assembler.Initialize( Master : TMaster_Assembler ) ;

begin
end ;


procedure TBase_Assembler.Terminate ;

begin
end ;


function TBase_Assembler.Assemble( Inputs : PChar ; var outputs, Machines : PChar ;
    var MachineL : longint ; var Address : int64 ;
    var Segment : longint ; Status : TAssembler_Status ) : TUnified_Exception ;

begin
    Result := Assemble_Ex( Inputs, Outputs, Machines, MachineL, Address, Segment,
        Status, 0 ) ;
end ;


function TBase_Assembler.Assemble_Ex( inputs : PChar ; var outputs, Machines : PChar ;
    var MachineL : longint ; var Address : int64 ;
    var Segment : longint ; Status : TAssembler_Status ;
    Flags : longint ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TBase_Assembler.Begin_Assembly : int64 ;

begin
    Result := 0 ;
end ;


function TBase_Assembler.Finish_Assembly( Context : int64 ; var outputs, machines : PChar ;
    var MachineL : longint ; var Address : int64 ;
    Status : TAssembler_Status ; Flags : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ; // No error
    MachineL := 0 ; // Nothing returnes
end ;


function TBase_Assembler.Request_Data( Address, Size : int64 ) : int64 ;

begin
    Result := Address ;
end ;



function TBase_Assembler.Evaluate( const X : string ; var _Result : longint ;
    PC_Adjustment : integer ) : TUnified_Exception ;

begin
    Result := nil ;
    _Last_Error := nil ;
    _Result := 0 ;
end ;


function TBase_Assembler.Evaluate_Symbol( O : string ; _Master : TMaster_Assembler ;
    PC_Adjustment, Size, Patch_Address : longint ;
    Immediate_Mode : boolean ; var C : longint ) : TUnified_Exception ;

begin
    Result := Evaluate( O, C, 2 ) ;
    if( Result <> nil ) then
    begin
        if( ( Result.Get_Error = 4 ) and ( not Immediate_Mode ) ) then
        begin
            _Master.Add_Reference( PChar( O ), Size, Patch_Address ) ;
            C := 0 ;
            Result := nil ;
        end ;
    end ;
end ;


procedure TBase_Assembler.Set_Last_Error( Value : TUnified_Exception ) ;

begin
    if( Value <> nil ) then
    begin
        Value.Attach ;
    end ;
    if( _Last_Error <> nil ) then
    begin
        _Last_Error.Detach ;
    end ;
    _Last_Error := Value ;
end ;


function TBase_Assembler.Default_Radix : longint ;

begin
    Default_Radix := 16 ; { Hexadecimal }
end ;


function TBase_Assembler.Default_Size : longint ;

begin
    Default_Size := 32 ; { 32-bit words }
end ;


function TBase_Assembler.Facility_Code : longint ;

begin
    Result := AssemblerErr_Facility ;
end ;


function TBase_Assembler.Source_Extensions : PChar ;

begin
    Result := nil ;
end ;


function TBase_Assembler.Valid_Symbol_Initial : PChar ;

begin
    Result := nil ;
end ;


function TBase_Assembler.Valid_Symbol_After : PChar ;

begin
    Result := nil ;
end ;


function TBase_Assembler.Version : integer ;

begin
    Version := Interface_Version ;
end ;


function TBase_Assembler.Backpatching( Name : PChar ; Address : int64 ;
    var Value : int64 ; var Size : longint ;
    Context, Options, Line : longint ; Filename : PChar ;
    Status : TAssembler_Status ) : boolean ;

begin
    Result := True ;
end ;


function TBase_Assembler.Normalize_Expression( Expression : PChar ;
    Numeric : boolean ; Status : TAssembler_Status ) : PChar ;

begin
    Result := Expression ;
end ;



{ TCPU methods... }

function TBase_CPU.Translate_Error( C : integer ) : string ;

begin
    Result := '' ;
end ;


function TBase_CPU.Set_Error( C : integer ) : TUnified_Exception ;

begin
    if( C = 0 ) then
    begin
        Last_Error := nil ;
    end else
    begin
        Last_Error := Create_Simple_UE( Facility_Code, 10, C, UE_Error, Translate_Error( C ), '' ) ;
    end ;
    Result := Last_Error ;
end ;


procedure TBase_CPU.Set_Last_Error( Value : TUnified_Exception ) ;

begin
    if( Value <> nil ) then
    begin
        Value.Attach ;
    end ;
    if( _Last_Error <> nil ) then
    begin
        _Last_Error.Detach ;
    end ;
    _Last_Error := Value ;
end ;


function TBase_CPU.Big_Endian : boolean ;

begin
    Result := False ; // Default to little endian
end ;


function TBase_CPU.Facility_Code : longint ;

begin
    Result := -1 ;
end ;


function TBase_CPU.Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;

begin
    Get_Assembler := nil ;
end ;


function TBase_CPU.Cancel_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, CPUErr_No_Breakpoint, UE_ERROR, 'No breakpoint', '' ) ;
end ;


function TBase_CPU.Disassemble( Address : int64 ; Base, Size : longint ;
    Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, CPUErr_No_Breakpoint, UE_ERROR, 'No breakpoint', '' ) ;
end ;


function TBase_CPU.Get_Clock_Speed : longint ;

begin
    Get_Clock_Speed := 1 ; { 1 Hz }
end ;


procedure TBase_CPU.Halt ;

begin
end ;


function TBase_CPU.Halted : boolean ; 

begin
    Halted := True ;
end ;


procedure TBase_CPU.Run ;

begin
end ;


procedure TBase_CPU.Run_From_Stream( Stream : TCOM_Stream ) ;

begin
end ;


function TBase_CPU.Set_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

begin
    Result := nil ;
end ;


procedure TBase_CPU.Set_Clock_Speed( Value : longint ) ;

begin
end ;


procedure TBase_CPU.Step( Into : boolean ) ;

begin
end ;


function TBase_CPU.Translate( Space : integer ; Address : int64 ) : int64 ;

begin
    Translate := Address ;
end ;


function TBase_CPU.Default_Base : integer ;

begin
    Result := 10 ;
end ;


function TBase_CPU.Get_Low_Memory : int64 ;

begin
    Result := 0 ;
end ;


function TBase_CPU.Get_High_Memory : int64 ;

begin
    Result := $7FFFFFFFFFFFFFFF ;
end ;


function TBase_CPU.Get_Low_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TBase_CPU.Get_High_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TBase_CPU.Get_Low_Port : int64 ;

begin
    Result := -1 ;
end ;


function TBase_CPU.Get_High_Port : int64 ;

begin
    Result := -1 ;
end ;


function TBase_CPU.Support_Virtual_Address : boolean ;

begin
    Result := False ;
end ;


function TBase_CPU.Register_Name( Index : integer ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_CPU.Register_Size( Index : integer ) : integer ;

begin
    Result := 0 ;
end ;


function TBase_CPU.Register_Description( Index : integer ) : PChar ;

begin
    Result := nil ;
end ;


procedure TBase_CPU.Restart ;

begin
end ;


function TBase_CPU.Clear_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    Result := Create_Simple_UE( Facility_Code, 10, CPUErr_No_Breakpoint, UE_ERROR, 'No breakpoint', '' ) ;
end ;


function TBase_CPU.Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TBase_CPU.Get_Current_Address( Space : integer ; Physical : boolean ) : int64 ;

begin
    Result := 0 ;
end ;


procedure TBase_CPU.Set_Current_Address( Space : integer ; Physical : boolean ;
    Value : int64 ) ;

begin
end ;


procedure TBase_CPU.Stop ;

begin
end ;


var Temp_Memory_Space_Description : string = 'Default' ;

function TBase_CPU.Memory_Space_Description( Index : integer ;
    Physical : boolean ) : PChar ;

begin
    if( Index = 0 ) then
    begin
        Result := PChar( Temp_Memory_Space_Description ) ;
    end else
    begin
        Result := nil ;
    end ;
end ;


function TBase_CPU.Page_Size : integer ;

begin
    Result := 0 ;
end ;


function TBase_CPU.Top_Of_Stack( Index : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TBase_CPU.Version : integer ;

begin
    Version := Interface_Version ;
end ;


function TBase_CPU.Segment_Size( Index : integer ) : integer ;

begin
    Result := 0 ;
end ;


function TBase_CPU.Address_Representation( Base : integer ;
    Address : int64 ) : PChar ;

begin
    Result := Address_Representation_Ex( nil, Base, Address ) ;
end ;


function TBase_CPU.Translate_Address( var B : boolean ; Base : integer ;
    Address : PChar ) : int64 ;

var S : string ;

begin
    S := string( Address ) ;
    if( not Valid_Base( S, Base ) ) then
    begin
        B := False ;
        Result := 0 ;
        exit ;
    end ;
    Result := strtoint( CVTB( Base, 10, S ) ) ;
    B := True ;
end ;


function TBase_CPU.Get_Low_Input_Port( Space : integer ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := Get_Low_Port ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TBase_CPU.Get_High_Input_Port( Space : integer ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := Get_High_Port ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TBase_CPU.Get_Low_Output_Port( Space : integer ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := Get_Low_Port ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TBase_CPU.Get_High_Output_Port( Space : integer ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := Get_High_Port ;
    end else
    begin
        Result := -1 ;
    end ;
end ;


function TBase_CPU.Get_Stack_Interface( Space : integer ) : TCEF_Stack_Interface ;

begin
    Result := nil ;
end ;


function TBase_CPU.Get_Target_Memory : TComponent ;

begin
    Result := nil ;
end ;


function TBase_CPU.Address_Representation_Ex( C : TComponent ; Base : integer ;
    Address : int64 ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_CPU.Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;

begin
    Result := nil ;
end ;


var _Info : TDefault_Data_Type = nil ;

function TBase_CPU.Register_Information( Index : longint ) : TData_Type ;

var S : integer ;

begin
    S := Register_Size( Index ) ;
    if( S = 0 ) then
    begin
        Result := nil ;
        exit ;
    end ;
    if( _Info = nil ) then
    begin
        _Info := TDefault_Data_Type.Create ;
    end ;
    _Info._Endian := Big_Endian ;
    _Info._Size := S ;
    Result := _Info ;
end ;


function TBase_CPU.Get_Store( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TBase_CPU.Get_Target_Address_Space : longint ;

begin
    Result := 0 ;
end ;



// TBase_Profiler methods...

procedure TBase_Profiler.Clear( Domain : integer ) ;

begin
end ;


function TBase_Profiler.Domain_Name( Index : integer ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_Profiler.Report_Line( Domain, Index : integer ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_Profiler.Version : integer ;

begin
    Result := Interface_Version ;
end ;



// TBase_Cable methods...

function TBase_Cable.Serial : boolean ;

begin
    Result := True ;
end ;


function TBase_Cable.Protocol : PChar ;

begin
    Result := nil ;
end ;


function TBase_Cable.Transmit( Speed : int64 ; Value, Data_Size, Stop_Bits : longint ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
end ;


function TBase_Cable.Transmit_String( Speed : int64 ; Value : PChar ;
    Data_Size, Stop_Bits : longint ) : TUnified_Exception ;

var Dummy : integer ;
    V : string ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    V := string( Value ) ;
    for Dummy := 1 to length( V ) do
    begin
        Result := Transmit( Speed, ord( V[ Dummy ] ), Data_Size, Stop_Bits ) ;
        if( Result <> nil ) then
        begin
            exit ;
        end ;
    end ;
end ;


procedure TBase_Cable.Receive( Source : TComponent ; Speed : int64 ;
    Value, Data_Size, Stop_Bits : integer ) ;

begin
end ;


function TBase_Cable.Get_Data( var Speed : int64 ;
    var Value, Data_Size, Stop_Bits : integer ) : boolean ;

begin
    Result := False ;
end ;



// TBase_Keyboard methods...

function TBase_Keyboard.Get_Key : PChar ;

begin
    Result := nil ;
end ;


function TBase_Keyboard.Get_Key_Down( Name : PChar ) : boolean ;

begin
    Result := False ;
end ;


procedure TBase_Keyboard.Set_Key_Down( Name : PChar ; State : boolean ) ;

begin
end ;


function TBase_Keyboard.Get_Key_Name( Index : integer ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_Keyboard.Get_LED_State( Name : PChar ) : boolean ;

begin
    Result := False ;
end ;


procedure TBase_Keyboard.Set_LED_State( Name : PChar ; State : boolean ) ;

begin
end ;


function TBase_Keyboard.Get_LED_Name( Index : integer ) : PChar ;

begin
    Result := nil ;
end ;


function TBase_Keyboard.Version : integer ;

begin
    Version := Interface_Version ;
end ;



// TBase_User_Interface methods...

// Internal utility routines...

function TBase_User_Interface.Translate_Error( C : integer ) : string ;

begin
    Result := '' ;
end ;


function TBase_User_Interface.Set_Error( C : integer ) : TUnified_Exception ;

begin
    if( C = 0 ) then
    begin
        Last_Error := nil ;
    end else
    begin
        Last_Error := Create_Simple_UE( Facility_Code, 10, C, UE_Error, Translate_Error( C ), '' ) ;
    end ;
    Result := Last_Error ;
end ;


// API...

function TBase_User_Interface.Facility_Code : longint ;

begin
    Result := -1 ;
end ;


function TBase_User_Interface.Get_Hidden : boolean ;

begin
    Result := True ;
end ;


procedure TBase_User_Interface.Set_Hidden( Value : boolean ) ;

begin
end ;


function TBase_User_Interface.Get_Parent_Window : THandle ;

begin
    Result := 0 ;
end ;


procedure TBase_User_Interface.Set_Parent_Window( Value : THandle ) ;

begin
end ;


procedure TBase_User_Interface.Set_Size( Height, Width : integer ) ;

begin
end ;


function TBase_User_Interface.Optimal_Height : integer ;

begin
    Result := 0 ;
end ;


function TBase_User_Interface.Optimal_Width : integer ;

begin
    Result := 0 ;
end ;


function TBase_User_Interface.Get_Caption : PChar ;

begin
    Result := nil ;
end ;


procedure TBase_User_Interface.Set_Caption( Value : PChar ) ;

begin
end ;


function TBase_User_Interface.Version : integer ;

begin
    Version := Interface_Version ;
end ;


procedure TBase_User_Interface.Initialize ;

begin
end ;


procedure TBase_User_Interface.Set_Last_Error( Value : TUnified_Exception ) ;

begin
    if( Value <> nil ) then
    begin
        Value.Attach ;
    end ;
    if( _Last_Error <> nil ) then
    begin
        _Last_Error.Detach ;
    end ;
    _Last_Error := Value ;
end ;



// TCEF32_Component_Query...

type TCEF32_Component_Query = class( TCEF_Component_Query )
                                  private // Instance data...
                                      _Type : longint ;
                                      _Version : string ;
                                      _Emulates : string ;
                                      
                                  public // API...
                                      function Query_Version : longint ;
                                          override ; stdcall ;
                                      procedure Terminate ;
                                          override ; stdcall ;

                                      function Component_Type : longint ;
                                          override ; stdcall ;
                                      function Version : PChar ;
                                          override ; stdcall ;
                                      function Emulates : PChar ;
                                          override ; stdcall ;
                              end ;


// API...

function TCEF32_Component_Query.Query_Version : longint ;

begin
    Result := Interface_Version ;
end ;


procedure TCEF32_Component_Query.Terminate ;

begin
    Free ;
end ;


function TCEF32_Component_Query.Component_Type : longint ;

begin
    Result := _Type ;
end ;


function TCEF32_Component_Query.Version : PChar ;

begin
    Result := PChar( _Version ) ;
end ;


function TCEF32_Component_Query.Emulates : PChar ;

begin
    Result := PChar( _Emulates ) ;
end ;


function Create_Component_Query( _Type : longint ; Version, Emulates : string ) : TCEF_Component_Query ;

begin
    Result := TCEF32_Component_Query.Create ;
    TCEF32_Component_Query( Result )._Type := _Type ;
    TCEF32_Component_Query( Result )._Version := Version ;
    TCEF32_Component_Query( Result )._Emulates := Emulates ;
end ;


end.
