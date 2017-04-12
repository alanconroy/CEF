/*
        Program Name : CEF
        Package Name : CEF
        Purpose      : CEF definitions
        Institution  :
        Date Written : 27-Apr-2000
		Written By   : Alan Conroy
		Version      : 2.6

		TO THE GLORY OF GOD THROUGH JESUS CHRIST

		Released to the public domain.

		*************************************************************
		*                                                           *
		*        M O D I F I C A T I O N   H I S T O R Y            *
		*                                                           *
		*    DATE        BY          REASON                         *
		*                                                           *
		*                                                           *
		*************************************************************

		*************************************************************
		*                                                           *
		*        P R O G R A M   P U R P O S E                      *
		*                                                           *
		*************************************************************

		  This header file contains all of the CEF type definitions.
*/

struct TUEC
{
	int Facility ;
	int Code ;
} ;


#define int64 __int64

class TCEF_Stack_Interface
{
	public: // API...
		virtual void __stdcall Terminate() = 0 ; // Destruct the object

		// Lowest possible stack entry address...
		virtual int64 __stdcall Low_Bound() = 0 ;

		// Highest possible stack entry address...
		virtual int64 __stdcall High_Bound() = 0 ;

		// Size of each stack entry, in bits
		virtual int __stdcall Item_Size() = 0 ;

		// Returns Indexth item in stack
		virtual int64 __stdcall Value( int64 Index ) = 0 ;

		// True if the stack grows up, false if it grows down
		virtual bool __stdcall Grow_Up() = 0 ;
};


class TCEF_Component_Query
{
    public: // API...
        // Return version of query interface.
        virtual int __stdcall Query_Version() = 0 ;

        // Terminate the instance.
        virtual void __stdcall Terminate() = 0 ;

        // Returns component type.  See Component_Type_*.
        virtual int __stdcall Component_Type() = 0 ;

        // Returns component's version.
        virtual char* __stdcall Version() = 0 ;

        // Returns description of what the component emulates.
        virtual char* __stdcall Emulates() = 0 ;
};


class TDebug_Interface
{
    public: // API...
		virtual int __stdcall Child_Count() = 0 ;
        /* Returns the number of child components under this level.  If this is
           a terminal (leaf) node, the method returns 0. */

		virtual char* __stdcall Get_Title() = 0 ;
        // Returns text representing this level (such as "Cached data").

        virtual bool __stdcall Has_Children() = 0 ;
        // Returns True if we have child nodes under this level.

        virtual void __stdcall Set_Title( char* Value ) = 0 ;
        // Sets text representing this level.

        virtual void __stdcall Activate() = 0 ;
        /* Indicates that the user wished to activate this level.  The
           behavior is undefined and up to the implementor.  Typically, this
		   does nothing unless this level is a terminal representing some
           complex data which is not viewable in a hierarchical form. */

        virtual bool __stdcall Can_Change() = 0 ;
		// Returns True if this item can be modified.

        virtual TDebug_Interface* __stdcall Child( int Index ) = 0 ;
        /* Returns a debug interface for the specified child level (first
           child level is 0). */

        virtual void __stdcall Kill() = 0 ;
        // Tells object to destroy itself

        virtual char* __stdcall Modify( char* Data, int Size ) = 0 ;
        /* Modifies data.  If Size is -1, Data is assumed to be
           null-terminated and its length is automatically determined.  The
           result will be NULL or a pointer to a null string to indicate
           success, and error text otherwise. */

        virtual int __stdcall Get_Context() = 0 ;
        // Retrives user-context.

        virtual void __stdcall Set_Context( int Value ) = 0 ;
        // Set user-context.
};


class TCommon_COM_Interface
{
     public: // API...
         // Initialize the object.  Returns result of initialization attempt.
         virtual TUEC __stdcall Initialize() = 0;

         // Terminate the use of the object.  Returns result of termination.
         // This generally should not be used - instead, use the Detach method.
		 virtual TUEC __stdcall Terminate() = 0;

         // Increments the reference count for the object.
         virtual void __stdcall Attach() = 0;

         // Decrements the reference count for the object.  When count reaches
         // 0, the object is destroyed.
         virtual void __stdcall Detach() = 0;

         // Version of the COM interface for this object.
		 virtual int __stdcall Interface_Version() = 0;

         // Facility ID for the facility represented by this object.  Returns -1
         // if no facility assigned. }
         virtual int __stdcall Facility_ID() = 0;

         // Facility version.  Result only has meaning if Facility_ID doesn't
         // return -1. }
         virtual int __stdcall Facility_Version() = 0;

         // Returns a debugging interface for the object.  Returns nil if not s
         // upported.
		 virtual TDebug_Interface* __stdcall Debugger() = 0;

         // Returns a pointer to an object with extended common COM methods for
         // object.  Always returns nil for now. }
         virtual void* __stdcall Extension() ;
} ; // TCommon_COM_Interface


class TCOM_Stream : public TCommon_COM_Interface
{
   public: // API...
      // Returns True if there is no more data in the stream to read.
      virtual bool __stdcall At_End() = 0 ;

      // Returns last error code
      virtual void __stdcall Last_Error( TUEC& UEC ) = 0 ;

	  // Reads the specified number of bytes from the stream.
      // Size is modified to be the actual bytes transferred. }
      virtual void __stdcall Read( void* Buffer, int& _Size ) = 0 ;

      // Reads one line of input, up to the size of the buffer.
      // The line is assumed to end at an ASCII 13 code.  The
      // ASCII 13 code is not included in the returned data.
      // Size is modified to be the actual bytes transferred.
      virtual void __stdcall Read_Line( void* Buffer, int& _Size ) = 0 ;

      // Position to specified byte within streamed data.
      virtual void __stdcall Seek( int Position ) = 0 ;

      // Returns size of stream data, in bytes.  -1 indicate that
      // the size is unknown or larger than 2^31.
      virtual int __stdcall Size() = 0 ;

      // Writes the specified buffer, of the specified size in
      // bytes, to the stream.
      virtual void __stdcall Write( void* Buffer, int _Size ) = 0 ;

      // Writes the specified null-terminated text to the stream.
      // An ASCII code 13 is appended to the text on output.
      virtual void __stdcall Write_Line( char* Buffer ) = 0 ;
}; // TCOM_Stream


const int Base_Interface_Version = 20 ; // 2.0
const int Interface_Version = 26 ; // 2.6

const int Severity_Information = 0 ; // Informational message
const int Severity_Warning = 1 ; // Warning
const int Severity_Error = 2 ; // Non-fatal error
const int Severity_Fatal = 3 ; // Fatal error.  The application should shut down.

const int Access_None = 0 ; // Not supported
const int Access_Read = 1 ; // Read (input)
const int Access_Input = Access_Read ;
const int Access_Write = 2 ; // Write (output)
const int Access_Output = Access_Write ;
const int Access_RW = Access_Read | Access_Write ; // Read or write (input or output)
const int Access_IO = Access_RW ;
const int Access_Execute = 4 ; // Execution
const int Access_All = Access_RW | Access_Execute ;

const int Component_Type_Unknown = 0 ;
const int Component_Type_CPU = 1 ;
const int Component_Type_Bus = 2 ;
const int Component_Type_Memory = 3 ;
const int Component_Type_Motherboard = 4 ;
const int Component_Type_IO = 5 ;
const int Component_Type_UI = 6 ;
const int Component_Type_Cable = 7 ;
const int Component_Type_Keyboard = 8 ;

const int Segment_Default_Code = 0 ;
const int Segment_Default_Data = 1 ;

const int IO_Type_Memory = 0 ; // Memory read/write
const int IO_Type_IO = 1 ; // I/O read/write
const int IO_Type_Bus = 2 ; // Bus state read/write
      // Note: all other IO types are specific to components

// Child_Notice codes...
const int Child_Notice_Terminating = 0 ; // Child is about to terminate
const int Child_Notice_Request_Terminate = 1 ; // Child is requesting termination
const int Child_Notice_Receive_Data = 2 ; // Child has received asynchronous data
const int Child_Notice_Connect = 3 ; // Successful connection of child to a component
const int Child_Notice_Disconnect = 4 ; // Successful disconnection of child from a component

// TUI_Interface Notice codes...
const int UI_Notice_Changed_Run_State = 0 ; // Run state is about to change.  Data = 0 (pause) or 1 (run).
const int UI_Notice_Request_Changed_Run_State = 1 ; // Run state is about to change.  Data = 0 (pause) or 1 (run).  Set Data to 2 to prevent.

// Output listing codes...
const int ListOut_Source = 0 ;
const int ListOut_Generated_Data = 1 ;
const int ListOut_New_Line = 2 ;
const int ListOut_New_Page = 3 ;
const int ListOut_Title_Text = 4 ;
const int ListOut_Paging = 5 ;
const int ListOut_No_Paging = 6 ;
const int ListOut_Table = 7 ;
const int ListOut_Message = 8 ;
const int ListOut_SubTitle = 9 ;

// Error codes...

// TStreamer error codes...
const int StreamerErr_Facility = -1 ;
const int StreamerErr_Success = 0 ;
const int StreamerErr_No_Stream = 1 ; // No stream is open

      // TComponent error codes...
const int ComponentErr_Facility = -1 ;
const int ComponentErr_Success = 0 ;
const int ComponentErr_No_Watchpoints = 1 ;
const int ComponentErr_Invalid_Operation = 2 ;
const int ComponentErr_Component_Not_Found = 3 ;
const int ComponentErr_Address_Out_Of_Range = 4 ;

      // TBus error codes...
const int BusErr_Facility = -1 ;
const int BusErr_Success = 0 ;
const int BusErr_Component_Not_Found = 1 ;

      // TMemory error codes...
const int MemoryErr_Facility = -1 ;
const int MemoryErr_Success = 0 ;

      // TMaster_Assembler error codes...
const int MasterAssemblerErr_Facility = -1 ;
const int MasterAssemblerErr_Success = 0 ;
const int MasterAssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
const int MasterAssemblerErr_Illegal_Expression = 2 ; // Illegal expression
const int MasterAssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
const int MasterAssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol

      // TAssembler error codes...
const int AssemblerErr_Facility = -1 ;
const int AssemblerErr_Success = 0 ;

      // TCPU error codes...
const int CPUErr_Facility = -1 ;
const int CPUErr_Success = 0 ;
const int CPUErr_No_Breakpoint = 1 ;

      // Master clock modes...
const int MCM_Default = 0 ; // (Default)  Unblock components in order of time
const int MCM_Ignore = 1 ; // Immediately unblock calling components
const int MCM_Synchronize = 2 ; // Unblock components in order of time, but synchronize emulator time to real system time

	  // Emulation flags...
const int RTS_Want_Nothing = 0 ;
const int RTS_Want_Calls = 1 ;
const int RTS_Want_Jumps = 2 ;
const int RTS_Want_Registers = 4 ;
const int RTS_Want_Interrupts = 8 ;
const int RTS_Want_Returns = 16 ;

	  // Symbol flags...
const int SF_Constant = 1 ; // Constant (otherwise a variable)
const int SF_Big_Endian = 2 ; // Symbol is big-endian
const int SF_Label = 4 ; // Label
const int SF_Global = 8 ; // Global symbol (local otherwise)
const int SF_External = 16 ; // External symbol (not used by CEF)

	  // Data type flags...
        // Families...
const int DataType_Family_Undefined = 0 ;
const int DataType_Family_Logical = 1 ;
const int DataType_Family_String = 2 ;
const int DataType_Family_Numeric = 3 ;
const int DataType_Family_Procedure = 4 ;
	  // Families 5-7 are reserved for future use

		// Types...
const int DataType_Boolean = DataType_Family_Logical + 0 ;

const int DataType_String = DataType_Family_String + 0 ;

const int DataType_Integer = DataType_Family_Numeric + 0 ;
const int DataType_Real = DataType_Family_Numeric + ( 1 << 3 ) ;
const int DataType_BCD = DataType_Family_Numeric + ( 2 << 3 ) ;

		// String length encodings...
const int Datatype_String_Length_Other = 0 ; // Fixed or otherwise unknown length encoding
const int Datatype_String_Length_Terminated = 1 ; // String is terminated by a null
const int Datatype_String_Length_Prefix = 2 ; // String is prefixed with length

		// String Encoding...
const int Datatype_String_Encoding_Unknown = 0 ; // Undefined
const int Datatype_String_Encoding_ASCII = 1 ; // 7-bit ASCII (8th-bit ignored)
const int Datatype_String_Encoding_EBCDIC = 2 ; // EBCDIC
const int Datatype_String_Encoding_Radix50 = 3 ; // Radix-50
const int Datatype_String_Encoding_UTF8 = 4 ; // 8-bit UNICODE (utf8)
const int Datatype_String_Encoding_Unicode16 = 5 ; // 16-bit UNICODE
const int Datatype_String_Encoding_Unicode32 = 6 ; // 32-bit UNICODE

enum TTri_State { TS_False, TS_True, TS_Dont_Care } ;

	 // Helper structures and classes...
class TData_Type
{
	  public: // API...
		  // General...
		  virtual int __stdcall Family() = 0 ;
		  virtual int __stdcall Data_Type() = 0 ;
		  virtual int __stdcall Size() = 0 ; // Total current size in bits
		  virtual bool __stdcall Big_Endian() = 0 ; // True if big-endian
		  virtual int __stdcall Max_Size() = 0 ; // Maximum size in bits

		  // Numeric types...
		  virtual TTri_State __stdcall Signed()  = 0 ; // True if signed, False if unsigned, Dont_Care if either/neither
		  virtual int __stdcall Mantissa() = 0 ; // Number of bits in mantissa (including sign, if signed)
		  virtual int __stdcall Exponent() = 0 ; // Number of bits in exponent (DataType_Real only)
		  virtual bool __stdcall Fixed() = 0 ; // True if fixed point number (DataType_Real and Data_Type_BCD only) or fixed-length string
		  virtual int __stdcall Fixed_Position() = 0 ; // If fixed, this is the position of the decimal place (eg 3 means the 3rd digit is the first digit of the fractional part)
		  virtual bool __stdcall Pack() = 0 ; // True if packed (DataType_BCD only)

		  // String types...
		  virtual int __stdcall Length_Encoding() = 0 ; // See Datatype_String_Length
		  virtual int __stdcall Prefix_Size() = 0 ; // Number of bits of length for strings with length prefix
		  virtual int __stdcall Encoding() = 0 ; // see Datatype_String_Encoding
} ; // TData_Type

struct TSymbol_Record
{
	int64 Address ; // Address of identifier's storage within segment.
	int Segment ; // Segment for identifier.
	int Size ; // Size of identifier's data, in bits.
	int Typ ; /* Data type of identifier:
								0 = Undefined
                                1 = Integer
                                2 = IEEE double-precision floating point
                                3 = string
                                4 = boolean
                                5 = character
                                other = other */
	int Flags ; /* Flags for identifier
								& 1 = Constant
								& 2 = Global
								& 4 - 32768 = reserved
                                other = other */
    int64 Data ;	// Actual value of identifier, if < 65 bits
    void* DataP ; // Pointer to value, if > 64 bits
} ;

class TAssembler_Status
{
               public:
					virtual char* __stdcall Filename() = 0 ;

					virtual bool __stdcall Get_Aborted() = 0 ;
                    virtual int64 __stdcall Get_Code() = 0 ;
                    virtual int64 __stdcall Get_Data() = 0 ;
					virtual int __stdcall Get_Errors() = 0 ;
                    virtual int __stdcall Get_Warnings() = 0 ;
                    virtual char* __stdcall Get_Error_Text() = 0 ;

					virtual void __stdcall Set_Aborted( bool Value ) = 0 ;
                    virtual void __stdcall Set_Code( int64 Value ) = 0 ;
                    virtual void __stdcall Set_Data( int64 Value ) = 0 ;
                    virtual void __stdcall Set_Errors( int Value ) = 0 ;
					virtual void __stdcall Set_Warnings( int Value ) = 0 ;
			        virtual void __stdcall Set_Error_Text( char* Value ) = 0 ;

                    virtual void __stdcall Get_Error( int Index,
                        char* &Text, char* &Filename,
                        int Line, int Severity ) = 0 ;

                    virtual void __stdcall Log_Error( char* Text, char* Filename,
                        int Line, int Severity ) = 0 ;

                    virtual void __stdcall Set_Line( int Value ) = 0 ;

					virtual void __stdcall Output_To_Listing( char* Text,
                        int Text_Type ) = 0 ;
} ; // TAssembler_Status

class TAssembler_Extension
{
	   public:
		   /* Process a directive.  Res is any generated
			 code.  Res_Length is the length of the generated code. */
		   virtual TUEC __stdcall Process_Directive( char* Source,
			   char* &Res,
			   int &Res_Length,
			   TAssembler_Status* Status ) = 0 ;

		   /* Notice of external symbol reference.
			 Source contains the symbole name, a space,
			 and the address where it is referenced. */
		   virtual void __stdcall External_Symbol( char* Source ) = 0 ;
} ;

	  // Logger options...
const int LO_Append = 1 ; // Append to existing log file
const int LO_Include_TimeStamp = 2 ; // Include Timestamp in log
const int LO_Include_Name = 4 ; // Include Component name in log
const int LO_Hide_Execution = 8 ;
const int LO_Hide_Data = 16 ;
const int LO_Hide_Sent_Signal = 32 ;
const int LO_Hide_Received_Signal = 64 ;
const int LO_Hide_State_Change = 128 ;
const int LO_Hide_Read = 256 ;
const int LO_Hide_Write = 512 ;
const int LO_Hide_Input = 1024 ;
const int LO_Hide_Output = 2048 ;
const int LO_Hide_Other = 4096 ;
      // 8192+ = reserved for future use

	  // Log Types...
const int LT_Execution = 0 ; // Sent at the start of an operation (eg CPU instruction execution)
const int LT_Data = 1 ; // Data
const int LT_Sent_Signal = 2 ; // Sent signal
const int LT_Received_Signal = 3 ; // Received signal
const int LT_State_Change = 4 ; // State change
const int LT_Read = 5 ; // A read request (memory or memory-like component)
const int LT_Write = 6 ; // A write request (memory or memory-like component)
const int LT_Input = 7 ; // An input request
const int LT_Output = 8 ; // And output request
const int LT_Other = 9 ; // Some other situation

      // Assembly flags... 
const int ASF_Immediate_Mode = 1 ; // Immediate mode
const int ASF_Want_Symbol_Table = 2 ; // Generatea a symbol table list (not passed to CPU assembler)
const int ASF_Want_XRef = 4 ; // Generate a cross-reference list (not passed to CPU assembler)
const int ASF_Generate_Virtual = 8 ; // Generate into physical address space (rather than virtual)
const int ASF_Disassemble = 16 ; // Disassembly is using assembly to get information on instruction
const int ASF_Extended = 32 ; // Passed stream is a TCEF_Stream, otherwise a TCOM_Stream
      // 64+ = reserved for future use


enum TMemory_Type { MT_Unknown, 
                    MT_Heap, 
                    MT_Data, 
                    MT_Stack, 
                    MT_Code } ;

class tUnified_Exception
{
		public:
            virtual void __stdcall Attach() = 0 ;
            virtual void __stdcall Detach() = 0 ;
            virtual bool __stdcall Is_Class( char* Name ) = 0 ;
            virtual int __stdcall Get_Facility() = 0 ;
            virtual int __stdcall Get_Facility_Version() = 0 ;
            virtual int __stdcall Version() = 0 ;
            virtual int __stdcall Severity( int Code ) = 0 ;
            virtual char* __stdcall Error_Text( int &Size, int &Typ ) = 0 ;
            virtual int __stdcall Get_Error() = 0 ;
            virtual tUnified_Exception* __stdcall Get_Previous() = 0 ;
            virtual void __stdcall Terminate() = 0 ;
            virtual void __stdcall Set_Memory_Type( TMemory_Type MT ) = 0 ;
            virtual TMemory_Type __stdcall Get_Memory_Type() = 0 ;
};

class TMaster_Clock ;
class TComponent ;
class TCPU ;
class TCable ;
class TMemory ;
class TUser_Interface ;
class TKeyboard ;
class TRun_Time_System ;
class TUI_Interface ;

class TCEF_Stream : public TCOM_Stream
{
	   public: // API...
		   virtual TComponent* __stdcall Get_Component() = 0 ;
		   virtual void __stdcall Set_Component( TComponent* Value ) = 0 ;
} ;

      // Logging radix values...
const int CEF_LR_Print_Friendly = 0 ;
const int CEF_LR_ASCII = 1 ;
const int CEF_LR_Radix50 = 50 ;
const int CEF_LR_EBCDIC = 51 ;

class TCEF_Logger
{
	   public: // API...
		   virtual void Attach() = 0 ;
		   virtual void Detach() = 0 ;
		   virtual int Get_Data_Radix() = 0 ;
		   virtual int Get_Options() = 0 ;
		   virtual bool Get_Paused() = 0 ;
		   virtual int Get_Wrap_Column() = 0 ;
		   virtual void Set_Data_Radix( int Value ) = 0 ;
		   virtual void Set_Options( int Value ) = 0 ;
		   virtual void Set_Paused( bool Value ) = 0 ;
		   virtual void Set_Wrap_Column( int Value ) = 0 ;

		   // Set UI component for the logger (for retrieving clock times, etc.
		   virtual void Set_UI( TUI_Interface* Value ) = 0 ;

		   // Returns name of current log file...
		   virtual char* Filename() = 0 ;

		   /* Open the specified file as the log file.  If Append
			 is true, the existing file is appended to.  If the
			 file doesn't exist it is created.  If Append is
			 false, any existing file is overwritten.  Returns
			 True if the file was opened and false otherwise. */
		   virtual bool Open( char* Filename, bool Append ) = 0 ;

		   /* Terminate the logger instance.  This should be done via the Deatch
			  call. */
		   virtual void Terminate() = 0 ;

		   /* Log an item to the log file.  S is the data to log.
			 Len is the length of S.  If Len is -1, S is assumed
			 to be null-terminated.  Continuation is false if
			 this is the first item of a log operation.  If
			 continuation is true, this is considered to be part
			 of the preceeding log item.  Log_Type indicates the
			 type of information being logged (see LT_*). */
		   virtual void Log( TComponent* C, char* S, int Len,
			   bool Continuation, int Log_Type ) = 0 ;

		   /* Log an item to the log file.  The logger gathers
			 the state from the component and does the log. This
			 is used for CPUs and the PC corresponding to the
			 beginning of the current instruction is passed. I
			 is the instruction about to be executed. This
			 should be called after the instruction is fetched
			 but before it is executed. */
		   virtual void Update( TComponent* C, int64 PC, int64 I ) = 0 ;
} ; // TCEF_Logger

class TUI_Interface
{
		  public:
         /* This method is called whenever a component blocks (Blocked is true)
           or unblocks (Blocked is false).  Component is the component in
           question. */
		 virtual void __stdcall Block( TComponent* Component, bool Blocked ) ;

         /* This method is called when a CPU triggers a breakpoint.  The method
           returns True if the CPU is to continue execution of the instruction
           or False if not. CPU is the CPU component which triggered the
		   breakpoint, Address is the execution address, and Physical indicates
           whether the address is physical or virtual.  Space is the index of
           the address space to assign the breakpoint to.  0 is the default
           address space. */
         virtual bool __stdcall Breakpoint_Notice( int64 Address, bool Physical,
			 int Space, TCPU* CPU ) ;

         /* This method returns the instance of the Master clock object for the
           application.  NULL is a valid return value and should be tested by
           any code calling this program (indicating that there is no master
           clock and timing issues are to be ignored). */
         virtual TMaster_Clock* __stdcall Clock() ;

         // Returns a deugger object for this object.
         virtual TDebug_Interface* __stdcall Debugger() ;

         /* Returns a stream for the passed (partial) filename.  If file cannot
           be opened/accessed, NULL is returned. */
		 virtual TCOM_Stream* __stdcall Get_File_Stream( char* Name ) ;

         /* If flag is true, the UI is hidden, otherwise it is made visible. */
         virtual void __stdcall Hide( bool Flag ) ;

         // Called by a blocked component (such as a CPU in wait state).
         virtual void __stdcall Idle( TComponent* Component ) ;

         /* This logs an actual program error (exception or warning).  Severity
		   is one of the following values:
               0 = Informational message
               1 = Warning
			   2 = Non-fatal error
               3 = Fatal error.  The application should shut down. */
         virtual void __stdcall Log_Error( TComponent* Component, char* Text,
             int Severity ) ;

         /* This logs a simulated hardware error.  Severity is one of the
            following values:
                    0 = Informational message
                    1 = Warning
                    2 = Non-fatal error
                    3 = Fatal simulated hardware failure. */
         virtual void __stdcall Log_Simulated_Error( TComponent* Component, char* Text,
             int Severity ) ;

         /* A component calls this method in response to a Show_Status request.
           Each call provides one line of status information.  Index is 0 for
           the first line, 1 for the second, etc. */
         virtual void __stdcall Log_Status( char* Text, int Index ) ;

         // Logs a trace event for the specified component.
         virtual void __stdcall Log_Trace( TComponent* Component, char* Description ) ;

         /* Called when a component changes a signal state.  Index is the state
           index. Active is true if the new state of the indexed signal is
           active. */
         virtual void __stdcall Signal_Change_Notice( TComponent* Component,
             int Index, bool Active ) ;

         /* Called when a component suffers some form of exception.  Note that
           CPUs that support exception handling must report those exceptions via
           State_Change_Notice. */
         virtual void __stdcall Signal_Exception( TComponent* Component,
             char* Description, int Index ) ;

         /* Called when a component changes a state.  Index is the state index.
           Active is true if the new state of the indexed state is active. */
         virtual void __stdcall State_Change_Notice( TComponent* Component,
             int Index, bool Active ) ;

		 /* Terminates the UI, which usuallay terminates the application */
         virtual void __stdcall Terminate() ;

         /* Toggle the embededed state of the specified TUser_Interface
           component. */
         virtual void __stdcall Toggle_Embed( TComponent* Component ) ;

         /* Returns the CEF specification version number to which this interface
           conforms. */
         virtual int __stdcall Version() ;

         /* Indicates that the specified component wishes to be notified of
           signal changes (or not). */
         virtual void __stdcall Want_Signals( TComponent* Component, bool Value ) ;

         /* This method is called when a component triggers a watchpoint.
           Address is the address that triggered the watchpoint.  Access
           indicates the type of access:
               2 = Read (input)
               3 = Write (output)
           Tag is dependant upon the component and Component is the component
           which triggered the watchpoint.  Memory is true if this is a memory
           watchpoint.  Internal is true if this is a watch on component
           internals.  Port is true if this is a port I/O access. */
         virtual void __stdcall Watchpoint_Notice( int64 Address, int Access, int Tag,
             TComponent* Component, bool Memory, bool Internal, bool Port ) ;

         /* Returns name of port at specified index.  Returns NULL if not a valid
            index. */
         virtual char* __stdcall Get_Port_Name( int Index ) ;

         /* Returns description of port at specified index.  Returns NULL if not
            a valid index. */
         virtual char* __stdcall Get_Port_Description( int Index ) ;

		 /* Returns the component for the port with the specified index. If Index
            is invalid, NULL is returned. This component is the object to which
            to connect another component. */
         virtual TCable* __stdcall Get_Port( int Index ) ;

		 /* Returns the component currently connected to the port with the
            specified index. If Index is invalid, NULL is returned.  If no
            component is connected to the port, NULL is returned. */
         virtual TComponent* __stdcall Get_Port_Connection( int Index ) ;

         /* Returns the component providing the port with the
            specified index. If Index is invalid, NULL is returned.  If no
            component is connected to the port, NULL is returned. */
		 virtual TComponent* __stdcall Port_Parent_Component( int Index ) ;

         // True = start running CPUs; False = pause CPUs
         virtual void __stdcall Run( bool State ) ;

         virtual int __stdcall TUI_Interface::Process_ID( char* Name, bool Force ) ;

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
         virtual bool __stdcall TUI_Interface::Process_Start( int ID, int& Priority ) ;

		 virtual void __stdcall TUI_Interface::Process_End( int ID ) ;

         // Indicates that the specified process has been destructed.
		 virtual void __stdcall Process_Deleted( int ID ) ;

		 virtual void __stdcall Add_Port_Breakpoint() ;

		 // Prompt user and add an execution breakpoint.
		 virtual void __stdcall Add_Breakpoint() ;

         // Prompt user and add register breakpoint.
		 virtual void __stdcall Add_Register_Breakpoint() ;

         // Prompt user and create a new memory breakpoint.
		 virtual void __stdcall Create_New_Breakpoint() ;

		 // Return passed component name qualified by path, etc.
		 virtual char* __stdcall Get_Component_Filename( char* Name ) ;

		 virtual void __stdcall Termination_Notice( TComponent* C ) ;

		 // Load and return a component with the specified name.
		 virtual TComponent* __stdcall Load_Component( char* Name ) ;
} ; // TUI_Interface


class TProfiler
{
    public:
        /* Clears profiling information for specified domain. If Domain is -1, all
           domains are cleared. */
        virtual void __stdcall Clear( int Domain ) ;

        /* Returns profiling domain name for given index.  If the index is invalid,
           the method returns NUll.  A domain indicates a type of profiling data.
           For instance, a CPU may track the number of calls to certain addresses as
           one domain, and the number of calls to specific types of instructions in
           another domain, etc. */
         virtual char* __stdcall Domain_Name( int Index ) ;

         /* Returns a line of information associated with the specified domain.  The
            first line is Index 0.  If an invalid index is passed, the method returns
            NULL. */
         virtual char* __stdcall Report_Line( int Domain, int Index ) ;

         /* Returns the CEF specification version number to which this interface
            conforms. */
         virtual int __stdcall Version() ;
} ; // TProfiler


class TComponent
{
          public:
        // This method returns a UEH facility code for the component class.
        virtual int __stdcall Facility_Code() ;

        /* Reinitalizes the component.  Note that this is not a constructor!  It
          is not necessary for the application to call this method, since
          Make_Instance should have initialized the component appropriately.
          This simply allows the component to be initialized again. */
		virtual TUEC __stdcall Initialize( TUI_Interface UI ) ;

        /* Terminates the component.  Note that this is not a destructor,
          although it should eventually cause the instance to be destructed.  By
          convention, once this method is called, the instance is no longer
          valid. */
        virtual TUEC __stdcall Terminate() ;

        // Returns number assigned by component creator.
        virtual int __stdcall Serial_Number() ;

        /* Adds the specified component to the list of components to notify along
           with Parent in Child_Notifications. */
        virtual TUEC __stdcall Add_Notification( TComponent* Component ) ;

        // Returns a cable interface.
        virtual TCable* __stdcall Cable() ;

        /* This method returns the Indexth child component for this component.
          It returns NULL if Index is outside of the range: 0 <= n <
          Max_child_component.  Child components are components owned by this
          component (such as a motherboard would own the CPU component). */
        virtual TComponent* __stdcall Child_Component( int Index ) ;

        /* Indicates a notice from a child component.  Child is the component and
          Notice is a value from Child_Notice_*. */
        virtual void __stdcall Child_Notification( TComponent * Child,
            int &Notice, int64 &Params) ;

        /* Clears a watchpoint at the specified address.  Access is the type of
          access:
					1 = Read or write (input or output)
                    2 = Read (input)
                    3 = Write (output)
		  If Memory is true, the watch occurs on memory accesses from the
		  component.
          If Memory is false, the watch occurs on port accesses from the
          component. */
        virtual TUEC __stdcall Clear_Watchpoint( int64 Address, bool Memory,
            int Access ) ;

		/* This method returns a value indicating the type of the component, as
          follows:
                        0 = Unknown (TComponent)
                        1 = CPU
                        2 = Bus
                        3 = Memory
                        4 = Motherboard
                        5 = I/O device
                        6 = User Interface */
        virtual int __stdcall Component_Type() ;

        /* For components which accept input, this connects another component to
          this component's default input port. */
        virtual TUEC __stdcall Connect_Input( TComponent* Component ) ;

        /* For components which generate output, this connnects another component
          to this component's default output port. */
        virtual TUEC __stdcall Connect_Output( TComponent* Component ) ;

        // Returns a CPU interface.
        virtual TCPU* __stdcall CPU() ;

        // Returns debug interface for this component.
        virtual TDebug_Interface* __stdcall Debugger() ;

        /* This removes the specified component from the list of components to
           be notified.  The component must have been added via
           Add_Notification. */
        virtual TUEC __stdcall Delete_Notification( TComponent* Component ) ;

        /* This method writes Size bits of data from the specified address from
		  the specified buffer.  Memory is True if this is a memory write versus
		  a I/O write.  This is solely for the UI, and the write does not
          trigger watchpoints or have any other side-effects other than changing
          the contents of the component. */
        virtual TUEC __stdcall Deposit( int64 Address, int Size, void* Buffer,
            bool Memory ) ;

        /* This disconnects the specified component from this component's default
		  input port. */
        virtual TUEC __stdcall Disconnect_Input( TComponent* Component ) ;

        /* This disconnects the specified component from this component's default
          output port. */
        virtual TUEC __stdcall Disconnect_Output( TComponent* Component ) ;

        /* This method reads Size bits of data from the specified address into
          the specified buffer.  Size is updated with the actual number of bits
          copied.  Memory is True if this is a memory read versus an I/O read.
          This is solely for the UI - the read triggers no watchpoints and has
          no other side-effects.  It also ignores access modes. */
        virtual TUEC __stdcall Examine( int64 Address, int& Size,
            void* Buffer, bool Memory ) ;

        /* Returns the access mode of the specified address.  Return values are:
                        0 = Not supported (address out of range, etc.)
                        1 = Read/Write (I/O)
                        2 = Read-only (input-only)
                        3 = Write-only (write-only)
          Memory is true if address is a memory address and false if it is an
          I/O address. */
        virtual int __stdcall Get_Access_Mode( int64 Address,
            bool Memory ) ;

        /* Provides a description of the exception conditions signaled via
          Signal_Exception. */
        virtual char* __stdcall Get_Exception_Description( int Index ) ;

        virtual TComponent* __stdcall Get_Parent() ;

        // Returns True if profiling for this component is on.
		virtual bool __stdcall Get_Profiling() ;

        /* Returns the component's default read latency.  If the component
          doesn't support latency, this should return 0. */
        virtual int __stdcall Get_Read_Latency() ;

        /* This returns the state of a signal with the specified name.  If the
          component recognizes the signal name, it returns True.  Otherwise it
          returns False.  If the result is True, State is set to the current
          value of the signal. */
        virtual bool __stdcall Get_Signal( char* Name, bool& State ) ;

        /* Get the name of a component state.  If index is invalid, the method
          returns NULL. */
        virtual char* __stdcall Get_State_Name( int Index ) ;

        virtual int __stdcall Get_Tag() ;

        virtual bool __stdcall Get_Trace() ;

        /* Returns the component's default write latency.  If the component
          doesn't support latency, this should return 0. */
        virtual int __stdcall Get_Write_Latency() ;

        /* This method returns the Indexth input component for this component.
          It returns NULL if Index is outside of the range: 0 <= n <
          Max_input_component */
        virtual TComponent* __stdcall Input_Component( int Index ) ;

        // Return a keyboard interface.
        virtual TKeyboard* __stdcall Keyboard() ;

        // Return a memory interface.
        virtual TMemory* __stdcall Memory() ;

        // This method returns a string containing the name of this component.
        virtual char* __stdcall Name() ;

        /* This method returns the Indexth output component for this component.
		  It returns NULL if Index is outside of the range: 0 <= n <
          Max_output_component */
        virtual TComponent* __stdcall Output_Component( int Index ) ;

        virtual TProfiler* __stdcall Profiler() ;

        /* Calling this method requests the component to transmit data to its
          default output port.  IO_Type is the type of read operation (see
          IO_Type_*).  Size is the number of bits being requested (0=default for
          component).  The component should verify that the address is within
          its range before responding.  If not, it should do nothing.  If the
          component will respond to the request, the method returns true,
          otherwise it returns false. */
		virtual bool __stdcall Read( int64 Address, int Size,
            int IO_Type ) ;

        // Reset the component.  Behavior is implementation dependant.
        virtual void __stdcall Reset() ;

        /* This method causes the component to restore its contents (but NOT
          state) from the passed stream.  It is the responsibility of the
          component to restore the contents of any child components (but not
          attached components). */
        virtual TUEC __stdcall Restore_Contents( TCOM_Stream* Stream ) ;

        /* This method causes the component to restore its current state (but NOT
          contents) from the passed stream.  It is the responsibility of the
          component to restore the state of any child components (but not
          attached components). */
        virtual TUEC __stdcall Restore_State( TCOM_Stream* Stream ) ;

        /* This method causes the component to save its contents (but NOT state)
          to the passed stream.  It is the responsibility of the component to
          save the contents of any child components (but not attached
          components). */
        virtual TUEC __stdcall Save_Contents( TCOM_Stream* Stream ) ;

        /* This method causes the component to save its current state (but NOT
          contents) to the passed stream.  It is the responsibility of the
		  component to save the state of any child components (but not attached
          components). */
        virtual TUEC __stdcall Save_State( TCOM_Stream* Stream ) ;

        /* Sets the access mode of the specified address range (Low to High,
		  inclusive).  Typ is:
                        1 = Read/Write (I/O)
                        2 = Read-only (input-only)
                        3 = Write-only (write-only)
          Memory is true if address is a memory address and false if it is an
          I/O address. */
        virtual TUEC __stdcall Set_Access_Mode( int64 Low, int64 High, bool Memory,
            int Typ ) ;

        virtual void __stdcall Set_Parent( TComponent* Component ) ;

        /* Turns profiling for this component on or off.  Profiling is off by
          default.  If Children is true, all child components will be assigned
          the same profiling value (this doesn't affect input or output
          components). */
        virtual void __stdcall Set_Profiling( bool _On, bool Children ) ;

        /* Sets the component's default read latency in nanoseconds.  If the
          component supports latency, it should block by this amount on each
          read. */
        virtual void __stdcall Set_Read_Latency( int Value ) ;

        /* This sets the state of a signal with the specified name.  This should
          only be called when the signal state actually changes (that is, State
          should never be the same on two consecutive calls). */
        virtual void __stdcall Set_Signal( char* Name, bool State ) ;

        // User-defined value associated with component.
        virtual void __stdcall Set_Tag( int Value ) ;

        virtual void __stdcall Set_Trace( bool Value ) ;

        /* An component-specific initialization string can be sent to the
          component after initialization, via this method. */
		virtual void __stdcall Set_Up( char* P ) ;

        /* Sets a watchpoint at the specified address.  When the component is
          about to access this address, it will generate a watchpoint notice to
          the UI interface.  Access is the type of access:
                        1 = Read or write (input or output)
                        2 = Read (input)
						3 = Write (output)
          If Memory is true, the watch occurs on memory accesses from the CPU.
          If Memory is false, the watch occurs on port accesses from the CPU. */
        virtual TUEC __stdcall Set_Watchpoint( int64 Address, bool Memory,
            int Access ) ;

        /* Sets the component's default write latency in nanoseconds.  If the
          component supports latency, it should block by this amount on each
          write. */
        virtual void __stdcall Set_Write_Latency( int Value ) ;

        /* Calling this method causes the component to report its status to the
          UI interface, in a human-readable form.  For instance, a CPU component
          would report its registers and their contents. */
        virtual void __stdcall Show_Status() ;

        /* Called when the specified component has a signal change.  This is only
          called if this component requested signal notices from the UI. */
        virtual void __stdcall Signal_Change_Notice( TComponent* Component,
            int Index, bool Active ) ;

        // Returns the number of defined signals.
        virtual int __stdcall Signal_Count() ;

        /* Returns the name of the supported signals.  The first index is 0.  If
          there is no signal for the specified index, or index is less than 0,
          the result is NULL. */
        virtual char* __stdcall Signal_Name( int Index ) ;

        /* Returns True if the signal is an output from the component.  Result is
          indefined if Index is invalid. */
        virtual bool __stdcall Signal_Out( int Index ) ;

        /* Returns True if the signal is active-low.  Result is undefined if
          Index is invalid. */
        virtual bool __stdcall Signal_Active_Low( int Index ) ;

        /* Returns True if the component supports the specified feature.  If the
          component does not inherently support the feature (such as a CPU
          feature for a memory component), the result is undefined. */
        virtual bool __stdcall Support_Feature( int ID ) ;

        // return a user-interface interface.
        virtual TUser_Interface* __stdcall User_Interface() ;

        /* Returns the CEF specification version number to which this interface
          conforms. */
        virtual int __stdcall Version() ;

        /* Calling this allows a component to resume its activity after it sends
          a Block request to the Master clock. */
		virtual void __stdcall Wake() ;

        /* Calling this method writes Value to the component's input port with
          the specified address.  IO_Type is the type of write operation (see
          IO_Type_*).  Size is the number of bits in Value to write (0=default
          for component) and may be 0 to 32, inclusive.  The component should
          verify that the address is within its range before responding.  If
          not, it should do nothing, and return no errors. */
        virtual TUEC __stdcall Write( int64 Address, int Value, int Size,
            int IO_Type ) ;

        /* This is the same as calling Write_Byte once for each byte in the
          string, incrementing Address once per byte.  It is provided to allow
          more efficient means of transmitting large amounts of data.  Size is
          the number of bits in Value to write (0=default for component).  The
          component should verify that the address is within its range before
          responding.  If not, it should do nothing, and return no errors. */
        virtual TUEC __stdcall Write_String( int64 Address, char* Value,
            int Size, int IO_Type ) ;

		/* Returns name of port at specified index.  Returns NULL if not a valid
           index. */
		virtual char* __stdcall Get_Port_Name( int Index ) ;

        /* Returns description of port at specified index.  Returns NULL if not
           a valid index. */
		virtual char* __stdcall Get_Port_Description( int Index ) ;

        /* Returns the component for the port with the specified index. If Index
           is invalid, NULL is returned. This component is the object to which
           to connect another component. */
		virtual TComponent* __stdcall Get_Port( int Index ) ;

        /* Returns the component currently connected to the port with the
           specified index. If Index is invalid, NULL is returned.  If no
           component is connected to the port, NULL is returned. */
        virtual TComponent* __stdcall Get_Port_Connection( int Index ) ;

        /* Notice from TUI_Interface component.  Code is UI_Notice_Code_* code.
           Data depends on the calue of Code. */
		virtual void __stdcall UI_Notice( int Code, int64& Data ) ;

		/* Returns True if the component responds to to I/O type Typ at address
		   Address.  Examine is True to check for response to examines/deposits,
		   and False for normal reads/writes. */
		virtual bool __stdcall Respond_To_Address( int64 Address, int Typ,
				bool Examine ) ;

		virtual TCEF_Logger* __stdcall Get_Logger() ;

		virtual void __stdcall Set_Logger( TCEF_Logger* Value ) ;
} ; // TComponent


class TMaster_Clock
{
   public:
	 /* This method registers a request for component to receive a Wake
           signal at the specified time delta (in picoseconds). */
	 virtual void __stdcall Block( TComponent* Component, int64 Time_Delta ) ;

         // Returns debug interface for this clock.
         virtual TDebug_Interface* __stdcall Debugger() ;

	     // Initializes the clock to time index 0 and sets the UI interface.
	     virtual void __stdcall Initialize( TUI_Interface* UI ) ;

         /* This method returns the current simulated master clock (which is
           measured in nanoseconds) time index. */
		 virtual int64 __stdcall Get_Time_Index() ;

         // Returns value passed by creator.
         virtual int __stdcall Serial_Number() ;

         /* Returns True if the clock supports the specified feature.  If the
           clock does not inherently support the feature (such as a CPU
           feature), the result is undefined. */
         virtual bool __stdcall Support_Feature( int ID ) ;

         /* Returns the CEF specification version number to which this interface
           conforms. */
         virtual int __stdcall Version() ;

         // V2.3 or later...

         // Set clock mode.  See MCM_*
         virtual void __stdcall Set_Mode( int M ) ;

         // Fet clock mode.  See MCM_*
         virtual int __stdcall Get_Mode() ;

         // Unblock all blocked components
		 virtual void __stdcall Unblock() ;
} ;


class TCable
{
    public:
        /* Returns true if this cable is a serial cable.  It is a parallel cable
           otherwise. */
        virtual bool __stdcall Serial() ;

        /* Returns name of the protocol supported by the cable (nil if generic/any). */
        virtual char* __stdcall Protocol() ;

        /* Transmit one data item.  Speed is the bit rate of the transfer (0=automatch),
		   Value is the data to transmit, Data_Size is the number of bits in Value (32
		   max), Stop_Bits indicates the number of stop bits. */
        virtual TUEC __stdcall Transmit( int64 Speed, int Value, int Data_Size, int Stop_Bits ) ;

        /* Transmits a string of data by calling Transmit once for each data item in
           Value. */
        virtual TUEC __stdcall Transmit_String( int64 Speed, char* Value, int Data_Size, int Stop_Bits ) ;

        /* Called when received data from another cable component. */
        virtual void __stdcall Receive( TComponent* Source, int64 Speed, int Value, int Data_Size, int Stop_Bits ) ;

		virtual bool __stdcall Get_Data( int64 &Speed, int &Value, int &Data_Size, int &Stop_Bits ) ;
} ;


class TUser_Interface
{
    public:
        virtual bool __stdcall Get_Hidden() ;
        virtual void __stdcall Set_Hidden( bool Value ) ;

        virtual int __stdcall Get_Parent_Window() ;
        virtual void __stdcall Set_Parent_Window( int Value ) ;

        virtual char* __stdcall Get_Caption() ;
        virtual void __stdcall Set_Caption( char* Value ) ;

        virtual void __stdcall Set_Size( int Height, int Width ) ;
                               
        virtual int __stdcall Optimal_Height() ;

        virtual int __stdcall Optimal_Width() ;

        /* Returns the CEF specification version number to which this interface
           conforms. */
        virtual int __stdcall Version() ;

        virtual void __stdcall Initialize() ;
} ;


class TKeyboard
{
    public:
        /* Retrieves the next key in a keystroke from the keyboard.  Returns nil or
           null string if end of current keystroke. */
        virtual char* __stdcall Get_Key() ; 

        /* Returns True if the specified key is down in the pending keystroke.  If Name
           is nil, returns True if any key is down. */
        virtual bool __stdcall Get_Key_Down( char* Name ) ;

        /* Sets the state of a given key to up or down.  The key is reset when the next
           keystroke is returned.  This can be used to synchronize the keyboard
           component's visible state with the physical keyboard. */
        virtual void __stdcall Set_Key_Down( char* Name, bool State ) ;

        /* Returns the name of the key associated with the passed index.  Returns nil
           if the index is invalid. */
		virtual char* __stdcall Get_Key_Name( int Index ) ;

        /* Returns True if the specified LED is lit.  If the LED name is not recognized,
           the function returns false. */
        virtual bool __stdcall Get_LED_State( char* Name ) ;

        /* Sets the state of a given LED to lit or not.  This can be used to synchronize
           the keyboard component's visible state with the physical keyboard. */
		virtual void __stdcall Set_LED_State( char* Name, bool State ) ;

        /* Returns the name of the LED associated with the passed index.  Returns
           nil if the index is invalid. */
        virtual char* __stdcall Get_LED_Name( int Index ) ;

        /* Returns the CEF specification version number to which this interface
           conforms. */
        virtual int __stdcall Version() ;
} ;


class TMemory
{
    public:
		// This function returns a facility code for the component class.
		virtual int __stdcall Facility_Code() ;

		/* Dumps an image of memory between Start and Start + Size - 1, inclusive,
          to the passed buffer, which must be large enough to hold Size bytes.
          Any range of addresses that are outside the memory's range are zero
          filled. */
        virtual void __stdcall Dump( int64 Start, int64 Size, void* Buffer ) ;

        // Returns the set of addresses that this component responds to.
        virtual void __stdcall Get_Address_Range( int64& Low, int64& High ) ;

        /* Loads memory between Start and Start + Size - 1, inclusive, with the
          data in the passed buffer.  Any data outside the memory's range is
          ignored. */
        virtual void __stdcall Load( int64 Start, int64 Size, void* Buffer  ) ;

        // Defines the set of addresses that this component responds to.
        virtual TUEC __stdcall Set_Address_Range( int64 Low, int64 High ) ;

        /* Returns the CEF specification version number to which this interface
           conforms. */
		virtual int __stdcall Version() ;

		virtual int __stdcall Map_Virtual_To_Physical( int64 Virt ) ;
} ; // TMemory


class TCEF_Assembler_Context
{
    public: // API...
		virtual void __stdcall TCEF_Assembler_Context::Initialize();

        virtual void __stdcall TCEF_Assembler_Context::Terminate();

        virtual int __stdcall Add_Mapping( char* Filename, int64 Address,
            int Line ) ;

        virtual TUEC __stdcall Add_Symbol( char* Name, TSymbol_Record* P ) ;

        virtual void __stdcall Delete( char* Sym ) ;

        virtual void __stdcall Delete_Mapping( int Index ) ;

        virtual int __stdcall Find( char* Sym, int64& Addr,
            int& Flg, int& D_T, int& Siz, void*& Dat ) ;

        /* Returns filename and line number corresponding to the passed address.
           If there is no mapping, it returns -1. */
        virtual int __stdcall Mapping( int64 Address, char*& Filename ) ;

        virtual void __stdcall Pop_Level() ;

        virtual void __stdcall Push_Level() ;

        /* Get value associated with passed symbol.  If symbol is unknown, it
           returns False. */
        virtual bool __stdcall Symbol_Value( char* Name, int64& Value ) ;

        /* Get size associated with passed symbol.  If
           symbol is unknown, it returns False. */
        virtual bool __stdcall Symbol_Size( char* Name, int& Value ) ;

        virtual bool __stdcall Get_Case_Sensitive() ;

        virtual void __stdcall Set_Case_Sensitive( bool Value ) ;
} ; // TCEF_Assembler_Context


class TMaster_Assembler
{
    public:
		// Adds a reference for back-patching.
        virtual void __stdcall Add_Reference( char* Name, int Size,
            int64 Address ) ;

        /* Adds a symbol to the current scope, with the name in Name and the
          specified information. */
        virtual TUEC __stdcall Add_Symbol( char* Name, TSymbol_Record* P ) ;

		/* An older version of Assemble_Ex without Flags.  See Assemble_Ex for
           details. */
        virtual TUEC __stdcall Assemble( TCOM_Stream* Input, TCOM_Stream* Output, TCOM_Stream* Listing,
            TAssembler_Status* Status ) ;

        // Apply all backpatches.
        virtual void __stdcall Backpatch( TAssembler_Status* Status,
            TCOM_Stream* Stream ) ;

        // Evaluates an expression, returning the numeric value in Value.
        virtual TUEC __stdcall Evaluate( char* X, int64& Value ) ;

        /* This method is used during assmebly by a TAssembler to insert assembly
          source in-place.  For instance, translating a directive into a
          standard directive, or doing macro expansions, etc.  If the expanded
          item is a directive that results in output, Res will be that output
          and Res_Length will be the size of that data, in bytes. */
        virtual TUEC __stdcall Expand( char* Source, char* &Res,
            int& Res_Length, TAssembler_Status* Status ) ;

        // Returns facility code for class
        virtual int __stdcall Facility_Code() ; 

        // Returns True if case sensitivity to identifier names is on.
        virtual bool __stdcall Get_Case_Sensitive() ;

        /* Returns the symbol record for the identifier in Name.  Returns NULL,
          if Name is undeclared. */
        virtual TSymbol_Record* __stdcall Get_Symbol( char* Name ) ;

        /* Returns the next token in the input stream during an assembly.
          Returns a null string if an error occurs (such as no more input).
          This is useful for assemblers which need more input for a directive
		  which is continued on additional lines. */
        virtual char* __stdcall Get_Token() ;

        // Returns entire current (remaining) input line.
        virtual char* __stdcall Grab_Line( bool Current ) ;

		// Inserts the data from Input into the current token stream.
        virtual void __stdcall In_Line( TCOM_Stream* Input ) ;

        /* Logs an assembly error.  Severity indicates the seriousness of the
          error:
                        0 = Informational
                        1 = Warning
                        2 = Error
                        3 = Fatal error (abort assembly) */
        virtual void __stdcall Log_Error( char* Text, int Severity ) ;

        // Map current token position to the passed address.
        virtual void __stdcall Map( int64 Address ) ;

        /* Return next token without removing from the token stream.  If Same_Line
          is True, only return the next token on the current line. */
        virtual char* __stdcall Peek_Token( bool Same_Line ) ;

        // Ends the current sub-scope for identifiers.
        virtual void __stdcall Pop_Scope() ;

        // Begins a sub-scope for identifiers within the current scope.
        virtual void __stdcall Push_Scope() ;

        // Returns a token to the input stream during an assembly.
        virtual void __stdcall Put_Token( char* Token ) ;

        // Sets case sensitivity on identifier names.
        virtual void __stdcall Set_Case_Sensitive( bool Value ) ;

        // Remove previous mapping added with Map().
        virtual void __stdcall UnMap() ;

        /* Returns the CEF specification version number to which this interface
           conforms. */
		virtual int __stdcall Version() ;

        /* Adds a reference for back-patching.  Context is a caller-defined
           value which is passed back via the Backpatching method of TAssembler.
		   Options indicate processing options (currently reserved for future
           use). */
        virtual void __stdcall Add_Reference_Ex( char* Name, int Size,
            int64 Address, int Context, int Options ) ;

        /* Adds a CPU component to the assembler's list of CPUs for the
          .SET_TARGET_CPU directive. */
        virtual void __stdcall Add_CPU( TComponent* CPU, char* Name ) ;

        // Removes all CPUs added with Add_CPU.
        virtual void __stdcall Clear_CPUs() ;

        // Set assembler context.
        virtual void __stdcall Set_Assembler_Context( TCEF_Assembler_Context* Value ) ;

        // Return current assembler context.
        virtual TCEF_Assembler_Context* __stdcall Get_Assembler_Context() ;

        virtual void __stdcall Terminate() ;

        virtual bool __stdcall Leading_Whitespace() ;

        /* Assembles code from the Input stream, directing the image data to the
          Output stream, and placing any list output to the Listing stream.
          Note that Listing can be NULL if no listing is desired.  Output can be
          NULL to do a syntax-check-only assembly.  Status is used to provide
          assembly statistics.  Flags:
            Bit    Description
            ---    -----------
            1      Immediate mode.
            2+     reserved. */
        virtual TUEC __stdcall Assemble_Ex( TCOM_Stream* Input, TCOM_Stream* Output, TCOM_Stream* Listing,
            TAssembler_Status* Status, int Flags ) ;

        // Evaluates an expression, returning the numeric value in Value.
        // PC_Adjustment is the value to add to any PC references.
        virtual TUEC __stdcall Evaluate_Ex( char* X, int64& Value, int64 PC_Adjustment ) ;

		virtual int __stdcall Get_Base() ;

		virtual void __stdcall Set_Base( int Value ) ;

		virtual void __stdcall Register_Extension( TAssembler_Extension* Extension ) ;
} ; // TMaster_Assembler


class TAssembler
{
          public:
        /* Informs the assembler the a new assembly operation is beginning.  This
          assembly will use the passed master assembler. */
        virtual void __stdcall Initialize( TMaster_Assembler* Master ) ;

        /* Informs the assembler that the current assembly operation is now
          complete. */
        virtual void __stdcall Terminate() ;

	/* An old version of Assemble_Ex without Flags.  See Assemble_Ex for
       details. */
        virtual TUEC __stdcall Assemble( char* inputs, char* &outputs, char* &machines,
            int& MachineL, int64& Address,
            int& Segment, TAssembler_Status* Status ) ;

        // Returns the default radix (base) of numeric literals.
        virtual int __stdcall Default_Radix() ;

        // Returns the default size of numeric literals, in bits.
		virtual int __stdcall Default_Size() ;

        // Returns facility code for this class.
        virtual int __stdcall Facility_Code() ;

        /* Return a bar-delimited list of source file extensions.  The first
          one should be the default. */
        virtual char* __stdcall Source_Extensions() ;

        // Returns a list of valid characters for starting a symbol name.
        virtual char* __stdcall Valid_Symbol_Initial() ;

        // Returns a list of valid characters for a symbol name, past the first.
		virtual char* __stdcall Valid_Symbol_After() ;

		/* Returns the CEF specification version number to which this interface
		   conforms. */
		virtual int __stdcall Version();

		/* Called by master assembler just before it backpatches an address.  If
           the function returns False, the master assembler doesn't perform the
           backpatch.  Otherwise it does.  Name is the expression that was
           evaluated for this backpatch, Address is the address of the backpatch
           data, Value is the data to write to Address, and Size is the size of
           the backpatch, in bytes. */
        virtual bool __stdcall Backpatching( char* Name, int64 Address,
            int64& Value, int& Size, int Context, int Options, int Line,
            char* Filename, TAssembler_Status* Status ) ;

	/* This method assembles the source code in the inputs string.  If inputs
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
          Flags:
            Flag    Description
            ----    -----------
            1       Immediate mode assembly
            2+      reserved
        */
		virtual TUEC __stdcall Assemble_Ex( char* inputs, char* &outputs,
			char* &machines, int& MachineL, int64& Address, int& Segment,
			TAssembler_Status* Status, int Flags ) ;

		/* Informs the assembler that an assembly operation is about to begin.
		  The return value is a context that is passed to Finish_Assembly. */
		virtual int64 __stdcall Begin_Assembly() ;

		/* Informs the assembler that the last assembly operation has completed.
		  The value from Begin_Assembly is passed.  See Assemble_Ex for flags
		  meanings. */
		virtual TUEC __stdcall Finish_Assembly( int64 Context, char* &outputs,
			char* &machines, int &MachineL, int64 &Address,
			TAssembler_Status* Status, int Flags ) ;

		/* When the master assembler is about to allocate data (eg due to a .DB
		  directive), it calls the CPU assembler with the address and size of
		  the data.  The assembler can then adjust the address, if necessary,
		  and return the address to which the data is to be placed.  If there is
		  no place for the data, -1 is returned. */
		virtual int64 __stdcall Request_Data( int64 Address, int64 Size ) ;
} ; // TAssembler


class TCPU
{
	public:
		/* Clears a breakpoint at the specified address.  If Physical is true,
		  the address is a physical address, otherwise it is a virtual
		  address. */
		virtual TUEC __stdcall Cancel_Breakpoint( int64 Address, int Space,
			bool Physical ) ;

	/* Clears a watchpoint at the specified address.  Access is the type of
	  access:
		1 = Read or write (input or output)
		2 = Read (input)
		3 = Write (output)
		  If Memory is true, the address is an internal cache address.  Otherwise
          it is a register index. */
        virtual TUEC __stdcall Clear_Internal_Watchpoint( int64 Address, bool Memory,
            int Access ) ;

        // Indicates the specified base used by the CPU instruction set.
        virtual int __stdcall Default_Base() ;

        /* Disassembles the instructions at the specified address, for the
          specified number of bytes and directs the output to the passed
          stream.  Constant values are shown in the specified base.  If base is
          0, the CPU's default base is used. */
		virtual TUEC __stdcall Disassemble( int64 Address, int Base, int Size,
            TCOM_Stream* Stream ) ;

        /* This method cosntructs and returns an assembler object for this CPU.
          It is passed a TMaster_Assembler object. */
        virtual TAssembler* __stdcall Get_Assembler( TMaster_Assembler* Master ) ;

        // Returns the CPU clock speed (in Hz).
        virtual int __stdcall Get_Clock_Speed() ;

        // Returns the current CPU memory position (usually the Program Counter).
        virtual int64 __stdcall Get_Current_Address( int Space,
            bool Physical ) ;

        // Indicates the lowest physical memory address accessable by the CPU.
        virtual int64 __stdcall Get_Low_Memory() ;

        // Indicates the highest physical memory address accessable by the CPU.
        virtual int64 __stdcall Get_High_Memory() ;

        // Indicates the lowest port address accessable by the CPU.
        virtual int64 __stdcall Get_Low_Port() ;

        // Indicates the highest port address accessable by the CPU.
        virtual int64 __stdcall Get_High_Port() ;

        // Indicates the lowest virtual memory address accessable by the CPU.
		virtual int64 __stdcall Get_Low_Virtual_Memory( int Space ) ;

        // Indicates the highest virtual memory address accessable by the CPU.
        virtual int64 __stdcall Get_High_Virtual_Memory( int Space ) ;

        /* Halts CPU execution.  The effect upon the CPU is implementation
          dependant. */
        virtual void __stdcall Halt() ;

        // Returns true if the CPU is in a halted state (non-execution mode).
        virtual bool __stdcall Halted() ;

        /* Returns a description of the memory space with the specified index.
          It returns NULL for any invalid index.  0 is always valid.  Physical is
		  true for physical memory space and false for virtual address spaces. */
        virtual char* __stdcall Memory_Space_Description( int Index,
            bool Physical ) ;

        // Returns memory page size, in bytes.  Returns 0 if non-paging CPU.
        virtual int __stdcall Page_Size() ;

		/* Returns a description of the register associated with the passed
          index.  */
        virtual char* __stdcall Register_Description( int Index ) ;

        /* Returns the name corresponding to the register index passed.  A null
          string or NULL result indicates that the index is out of range.  Note
		  that all registers start at offset 0. */
        virtual char* __stdcall Register_Name( int Index ) ;

        /* Returns the size (in bits) of the register associated with the passed
          index.  A result of 0 indicates that the index is out of range.  Note
          that all registers start at offset 0. */
        virtual int __stdcall Register_Size( int Index ) ;

        // Power-on (cold) reset.
        virtual void __stdcall Restart() ;

        /* Begins CPU execution from the current CPU state.  Typically one
		  instruction is executed, and then the CPU requests a block from the
          master clock. */
        virtual void __stdcall Run() ;

        /* The causes the CPU to execute instructions directly from the passed
          stream.  This is usually for immediate instruction execution from the
          user interface, thus no profiling is done, no breakpoints apply, and
          single-stepping does not occur. */
        virtual void __stdcall Run_From_Stream( TCOM_Stream* Stream ) ;

        /* Sets a breakpoint at the specified address.  When the CPU is about to
          begin execution at this address, it will generate a breakpoint notice
          to the UI interface.  If Physical is true, the address is a physical
          address, otherwise it is a virtual address. */
        virtual TUEC __stdcall Set_Breakpoint( int64 Address, int Space,
            bool Physical ) ;

        // Sets the CPU clock speed (in Hz).
        virtual void __stdcall Set_Clock_Speed( int Value ) ;

        // Sets the current CPU memory position.
		virtual void __stdcall Set_Current_Address( int Space, bool Physical,
            int64 Value ) ;

	/* Sets a watchpoint at the specified address.  When the CPU is about to
          access this address, it will generate a watchpoint notice to the UI
          interface.  Access is the type of access:
		1 = Read or write (input or output)
		2 = Read (input)
		3 = Write (output)
          If Memory is true, the address applies to internal cache.  If Memory is
          false, address is the register. */
        virtual TUEC __stdcall Set_Internal_Watchpoint( int64 Address, bool Memory,
            int Access ) ;

        /* Executes a single instruction based on the current CPU context.  This
          is the same as Run(), but execution doesn't continue after the CPU is
          unblocked.  If Into is true, only a single instruction is excecuted.
          If Into is false, and the instruction to be executed is a nested
		  instruction (such as a subroutine call), execution continues until the
          execution returns to the instruction following the call. */
        virtual void __stdcall Step( bool Into ) ;

        // Stop the CPU execution.
        virtual void __stdcall Stop() ;

        // Returns True if the CPU supports virtual address mapping.
        virtual bool __stdcall Support_Virtual_Address() ;

        // Returns address of top of the indexth stack.
        virtual int64 __stdcall Top_Of_Stack( int Index ) ;

        /* Returns the physical address for the passed virtual address in the
          indicated address space.  For a CPU with no virtual addresses, this
          returns the value passed. */
        virtual int64 __stdcall Translate( int Space, int64 Address ) ;

		/* Returns the CEF specification version number to which this interface
           conforms. */
        virtual int __stdcall Version() ;

        /* Indicates the size (in bits) of the Indexth segment in the last
          assembled instruction.  A value of 0 indicates either an invalid Index
          or that the information is otherwise not available.  Index < 0 is not
          valid.  The result can be used to separate the various segments of
          assembled instructions.  For instance, a 2-byte instruction followed
          by two 2-byte operands would return 2 for indexes 0, 1, and 2. */
        virtual int __stdcall Segment_Size( int Index ) ;

        /* Returns a representation of the passed address, or nil if not
		  supported, or the address is invalid.  This is used when addresses are
          displayed in a format not otherwise supported by CEF.  Base is the
          base to use for any numeric representation. */
        virtual char* __stdcall Address_Representation( int Base, int64 Address ) ;

        /* Used to convert an address specification into a linear address value.
          B is ignored on call and is True on return if the address is a valid
          format.  Base is the base that any numeric portion of the address is
		  assumed to be. */
        virtual int64 __stdcall Translate_Address( bool& B, int Base, char* Address ) ;

        /* Indicates the lowest input port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. */
        virtual int64 __stdcall Get_Low_Input_Port( int Space ) ;

        /* Indicates the highest input port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. */
        virtual int64 __stdcall Get_High_Input_Port( int Space ) ;

        /* Indicates the lowest output port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. */
        virtual int64 __stdcall Get_Low_Output_Port( int Space ) ;

        /* Indicates the highest output port address accessable by the CPU for
          port space Space.  -1 indicates an invalid port space. */
        virtual int64 __stdcall Get_High_Output_Port( int Space ) ;

		/* Returns an instance of a stack interface for accessing non-standard
		  stacks.  Returns nil if no stack or standard stack handling. */
		virtual TCEF_Stack_Interface* __stdcall Get_Stack_Interface( int Space ) ;

		/* Return memory component that the CPU executes/disassembles from and/or
		  assembles to.  If the CPU uses the main memory, this should return
		  nil.  This is used when the CPU has a code store that is separate from
		  main system memory (such as for a microcode engine). */
		virtual TComponent* __stdcall Get_Target_Memory() ;

		/* Returns a representation of the passed address, or nil if not
		  supported, or the address is invalid.  This is used when addresses are
		  displayed in a format not otherwise supported by CEF.  Base is the
		  base to use for any numeric representation. C is the memory component
		  associated with the address (or nil if main memory). */
		virtual char* __stdcall Address_Representation_Ex( TComponent* C, int Base,
		   int64 Address ) ;

		/* Registers a run-time system with the CPU, with the specified notice
		  flags (see CEF_EF_*). */
		virtual tUnified_Exception* __stdcall Register_RTS( TRun_Time_System* RTS, int Flags ) ;

		/* Returns detailed information about the specified register.  A result
		  of nil indicates that the index is out of range.  Note that all
		  registers start at offset 0. Note that the returned object is only
		  valid until the next call to Register_Information. */
		virtual TData_Type* __stdcall Register_Information( int Index ) ;

		/* Returns the data/code store for the specified index.  Returns nil if
		  there is no separate store for the index, or if the index is out of
		  range.  The indexes correspond to the memory space(s) used by
		  Memory_Space_Description.  This is used for CPUs that store code
		  and/or data in separate address spaces. */
		virtual TComponent* __stdcall Get_Store( int Index ) ;

		/* Return memory address space that the CPU executes/disassembles from
		  and/or assembles to.  If the CPU uses the main memory, this should
		  return 0.  This is used, for instance, when the assembler directs
		  compiled value to different code and data address spaces. */
		virtual int __stdcall Get_Target_Address_Space() ;
} ; // TCPU


// TRun_Time_System is used when a CPU needs to communicate with an emulator to
// inform it of certain operations.
class TRun_Time_System
{
		 public:
			/* Indicates a call to the passed address.  Returns true if the call
			  was handled by the emulator, false if the CPU should make the call
			  itself. */
			virtual bool __stdcall Call( int64 Address ) = 0 ;

			/* Indicates that the specified interrupt/trap instruction is about
              to be processed. Returns true if the trap is handled by the
              emulator and that the CPU should skip it, false if the CPU should
			  handle it normally. */
			virtual bool __stdcall Trap( int64 Address ) = 0 ;

			// Indicates that the CPU has jumped/branched to a new address.
			virtual void __stdcall Jumped() = 0 ;

			// Indicates that the CPU has executed a halt operation
			virtual void __stdcall Halted() = 0 ;

			/* Indicates a pending register value change.  Returns value to
			  assign. Index is the register index and Value is the new value. */
			virtual int64 __stdcall Register_Change( int Index, int64 Value ) = 0 ;

			// Indicates a return.
			virtual bool __stdcall Return() = 0 ;
} ; // TRun_Time_System



// File layouts...

// Dump file layout

struct TDump_File_Header
{
	short int Signature ;
    unsigned char Subsignature ; // 0 = CEF
	unsigned char Version ; // File format version number times 10 (1.2=12)
    unsigned char CEF_Type ; // 0 = Dump
    int Stamp ; // Optional packed date/time stamp
    int64 Start ; // First address in image
    unsigned char Fill[ 256 - 17 ] ; // Filler - should be 0
} ;


// State file layout

struct TState_File_Header
{
    short int Signature ;
    unsigned char Subsignature ; // 0 = CEF
    unsigned char Version ; // File format version number times 10 (1.2=12)
    unsigned char CEF_Type ; // 1 = State
    int Stamp ; // Optional packed date/time stamp
    unsigned char Filler[ 256 - 9 ] ; // Filler - should be 0
} ;



struct TState_File_Sub_Header
{
    int Facility ; // Facility code of component
    int Serial_Number ; // Serial number of component
} ;


class TCEF_Exception
{
	public: // API...
		virtual char* __stdcall Text() ;
		virtual void __stdcall Set_Text( char* T ) ;
		virtual void __stdcall Terminate() ;
} ;

// Remote access classes:
const int RAC_Command = 1 ; // Allow control of CEF framework
const int RAC_Connection = 2 ; // Allow connection to emulated ports
const int RAC_Connection_Query = 4 ; // Allow query of available ports
const int RAC_Query = 8 ; // Allow query of CEF framework

class TCEF_Remote
{
	// Return ID of remote access object
	virtual char* __stdcall Get_ID( int &Size ) = 0 ;

	// Return human-readable name of remote access object
	virtual char* __stdcall Name() = 0 ;

	// Install remote access interface
	virtual TCEF_Exception* __stdcall Install() = 0 ;

	// Uninstall remote access interface
	virtual TCEF_Exception* __stdcall Uninstall() = 0 ;

	// Set ID of remote access interface
	virtual void __stdcall Set_ID( char* ID, int Length ) = 0 ;

	// Set enabled state of type of access
	virtual TCEF_Exception* __stdcall Set_Enabled( int Access ) = 0 ;

	// Poll remote access interface - for ones that are synchronous.
	virtual char* __stdcall Poll( int &Size ) = 0 ;
} ;

