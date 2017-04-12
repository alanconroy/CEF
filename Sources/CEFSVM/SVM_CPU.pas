{$N+}
{
        Program Name : CEFSVM
        Package Name : CEF
        Purpose      : SVM CPU (CEF components)
        Institution  :
        Date Written : 6-Feb-2015
        Written By   : Alan Conroy
        Version      : 1.0

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

        DATE          BY      REASON
        ----          --      ------


        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	    This unit implements an SVM CPU for CEF.


        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan
}

unit SVM_CPU ;

interface

uses // Borland...
     Classes, // TStringList

     // C&C...
     CommonUt, // TInteger_List
     _DebugIn, // TDebug_Interface
     _Files, // PSFile
     _Streams, // TCOM_Stream
     _Types, // Small_String
     _UE, // TUnified_Exception
     _UEHDefs, // TUEC

     // CEF...
     _CEFUtil, // TCEF_Watchpoint_Manager
     CEF, // TProfiler

     // SVM...
     _SVM ; // Execution_Types


type TSVM_Profiler = class( TProfiler )
                         private // Instance data...
                             // Profile data...
                             _Instruction_Count : integer ;
                             Execution_Addresses : array[ 0..$FFFF ] of integer ;
                             Instructions : array[ 0..255 ] of integer ;

                             // Report data...
                             Outputs : TStringList ;
                             Inputs : TStringList ;
                             Addresses : TStringList ;
                             Instruction_Lines : TStringList ;

                             // Temps used for returning PChars...
                             Temp_Domain_Name : string ;
                             Temp_Report_Line : string ;

                             // Other data...
                             Dirty : boolean ;

                         public // API...
                             procedure Generate_Report ;
                             procedure Increment( Domain, Index : integer ) ;

                         public // Overrides...
                             procedure Clear( Domain : integer ) ; override ;

                             function Domain_Name( Index : integer ) : PChar ;
                                 override ;

                             function Report_Line( Domain, Index : integer ) : PChar ;
                                 override ;
                     end ; // TSVM_Profiler


type TSVM_Image = class
                      public
                          Name : string[ 255 ] ; { Name of image file }
                          //Image_File : file of Code_Block ; { Opened image file }
                          Code_Start : integer ; { Starting block of code in file }
                          Code_Low, Code_High : longint ; { Mapped code address range }
                           Data_Base : longint ; { Mapped data address base }
                          Symbol_Start : longint ; { Starting block of symbol table }
                  end ;

type TSVM_CPU = class ;
     TSVM = class( TComponent )
                  private // Instance data...
                      _CPU : TSVM_CPU ;
                      _Memory : TMemory ;
                      Inputs : TList ;
                      Outputs : TList ;
                      _Parent : TComponent ;
                      _Tag : longint ;
                      _Logger : TCEF_Logger ;

                  protected // Internal utility routines...
                       function Set_Error( C : integer ) : TUEC ;

                  public // API...
                      _Serial_Number : integer ;

                      function Initialize( UI : TUI_Interface ) : TUEC ;
                          override ;

                      function Terminate : TUEC ; override ;

                      function Serial_Number : integer ; override ;

                      function Child_Component( Index : longint ) : TComponent ;
                          override ;

                      function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                          Access : longint ) : TUEC ; override ;

                      function Component_Type : longint ; override ;

                      function Connect_Input( Component : TComponent ) : TUEC ;
                          override ;

                      function Connect_Output( Component : TComponent ) : TUEC ;
                          override ;

                      function CPU : TCPU ; override ;

                      function Debugger : TDebug_Interface ; override ;

                      function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                          Memory : boolean ) : TUEC ; override ;

                      function Disconnect_Input( Component : TComponent ) : TUEC ;
                          override ;

                      function Disconnect_Output( Component : TComponent ) : TUEC ;
                          override ;

                      function Examine( Address : int64 ; var Size : longint ;
                          Buffer : pointer ; Memory : boolean ) : TUEC ;
                          override ;

                      function Facility_Code : longint ; override ;

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

                      function Restore_Contents( Stream : TCOM_Stream ) : TUEC ;
                          override ;

                      function Restore_State( Stream : TCOM_Stream ) : TUEC ;
                          override ;

                      function Save_Contents( Stream : TCOM_Stream ) : TUEC ;
                          override ;

                      function Save_State( Stream : TCOM_Stream ) : TUEC ;
                          override ;

                      function Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
                          Typ : longint ) : TUEC ; override ;

                      procedure Set_Profiling( _On, Children : boolean ) ;
                          override ;

                      procedure Set_Read_Latency( Value : longint ) ;
                          override ;

                      function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                          Access : longint ) : TUEC ; override ;

                      procedure Set_Write_Latency( Value : longint ) ;
                          override ;

                      procedure Show_Status ; override ;

                      function Support_Feature( ID : integer ) : boolean ;
                          override ; stdcall ;

                      procedure Wake ; override ;

                      function Write( Address : int64 ; Value, Size : longint ;
                          IO_Type : longint ) : TUEC ; override ;

                      function Write_String( Address : int64 ; Value : PChar ;
                          Size : longint ; IO_Type : longint ) : TUEC ;
                          override ;

                      procedure Set_Up( P : PChar ) ; override ;

                      procedure Reset ; override ;

                      procedure Set_Signal( Name : PChar ; State : boolean ) ;
                          override ;

                      function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                          override ;

                      function Signal_Count : longint ; override ;

                      function Signal_Name( Index : longint ) : PChar ;
                          override ;

                      function Signal_Out( Index : longint ) : boolean ;
                          override ;

                      function Signal_Active_Low( Index : longint ) : boolean ;
                          override ;

                      function Get_State_Name( Index : longint ) : PChar ;
                          override ;

                      function Get_Exception_Description( Index : longint ) : PChar ;
                         override ;

                      procedure Set_Tag( Value : longint ) ; override ;

                      function Get_Tag : longint ; override ;

                      function Get_Parent : TComponent ; override ;

                      procedure Set_Parent( Component : TComponent ) ;
                          override ;

                      function Profiler : TProfiler ; override ;

                      function Get_Trace : boolean ; override ;

                      procedure Set_Trace( Value : boolean ) ; override ;

                      function Signal_Index( Name : PChar ) : integer ;
                          override ;

                      function Get_Logger : TCEF_Logger ; override ;

                      procedure Set_Logger( Value : TCEF_Logger ) ; override ;
              end ; // TSVM

     TSVM_CPU = class( TCPU )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private // Generic CPU support...
            _State : integer ;
            Parent : TSVM ;
            _UI : TUI_Interface ;
            _Register_Watchpoints : array[ 0..3 ] of integer ; // Access mode for registers
            _Profiling : boolean ; // True if profiling
            _Memory_Watchpoints : TCEF_Watchpoint_Manager ;
            _Breakpoints : TInteger_List ;
            _Last_Error : TUEC ;
            Memory_Data_Latch : int64 ; // Last byte or word sent to us
            _Run_Stream : TCOM_Stream ;
            _Profiler : TSVM_Profiler ;
            _Trace : boolean ; // True to trace execution
            Blocked : boolean ;
            _Logger : TCEF_Logger ;
            _RTS : TRun_Time_System ;
            _RTS_Flags : longint ;
            _Speed : longint ;
            Current_Thread : integer ; { Currently running thread }
            Threads : TList ; // list of SVM_CPUs
            Execution : Execution_Types ; {Current execution "mode" for debugger}
            Default_Input_Redirected : boolean ;
            Default_Input : textfile ;
            Default_Output_Redirected : boolean ;
            Default_Output : textfile ;
            Max_Threads : longint ; { Maximum number of concurrent threads, if 0 then we are a thread and cannot have threads of our own }
            _Code : TComponent ;
            _Data_Cache : TComponent ;
            _Stack : TComponent ;
            Stack_Size : longint ;
            Code_Base : int64 ; { Base code address of currently executing image }
            Data_Base : longint ; { Base data address of currently executing image }
            Base_Image_Code_Size : int64 ;
            Images : TList ; // list of TSVM_Images
            Native_Images : TList ; { Native image list }
            Extensions : TList ;
            Buffer_Empty : Boolean ; {True if line of input is empty}
            Control_C : boolean ;
            Using_SMU : boolean ; // True if SMU has been initialized by the user program
            _Observer : TSVM_Observer ;
            _Redirect_Path : string ; // Default current path for files
            Code_High_Address : int64 ;
            Data_High_Address : longint ;
            Loaded_Symbol_Address : int64 ;
            Segments : TInteger_List ;
            _Data_Type : CEF.TData_Type ;
            Last_Assembler : TAssembler ;
            _Terminal : TComponent ; // I/O terminal
            _KB_Buffer : string ;

        private // PChar holding strings...
            Temp_Register_Name : string ;
            Temp_Register_Description : string ;
            Temp_Log_Trace : string ;
            Temp_Memory_Space_Description : string ;
            Temp_Disassemble : string ;

        private // Processor internals...
            _IP : int64 ;
            _SP : int64 ;
            _SF : int64 ;
            _Temp0 : string ;

            _Halted : boolean ; // True if last instruction was a halt
            Waiting : boolean ; // True if in a wait state

        private // Internal utility routines...
            procedure Delete_Assembler( Sender : TAssembler ) ;
            procedure Finish_Assembler( Sender : TAssembler ;
                Status : TAssembler_Status ; PC : int64 ) ;
            function Load_Native_Image( const File_Name : string ) : string ;
            function Load_Native_Symbol( const File_Name, Symbol : string ;
                Vector : longint ) : string ;
            function Map_Virtual_To_Physical( A : longint ) : longint ;

            procedure Err( S : string ) ;
            { Converts a numeric value to a human-readable version }
            function Number_To_String( AR : int64 ; Data_Type : TData_Type ;
                var _Result : integer ) : string ;
            function Call( XX : Integer ) : boolean ; {Call a standard routine}
            procedure _Data_Write( A : Small_String ; B, C : int64 ;
                D : Integer ) ;
            procedure Set_State( Value : integer ) ;
            function Terminal : TComponent ; // I/O terminal
            function Code : TComponent ;
            function Data_Cache : TComponent ;
            function Stack : TComponent ;
            function Data_Cache_Read( Address : int64 ; Size : integer ) : int64 ;
            function Stack_Read( Address : int64 ; Size : integer ) : int64 ;
            function _Read_Stack( Stack_Pointer : int64 ; Y : Integer ) : int64 ;
            procedure Write_Stack( Stack_Pointer, X : int64 ; Y : Integer ) ;
            function _Read_Data( Address : int64 ; Y : Integer ) : int64 ; // Read data from stack
            function Read_String( A : int64 ; var _Result : integer ) : string ;
            function Read_String_Data( B : int64 ) : String ; { Read pre-defined string }
            procedure Read_File_Integer( X : Longint ; var E : Boolean ;
                F : _Files.PSFile ; DT : TData_Type ) ;
            procedure Read_File_String( X : Longint ; F : _Files.PSFile ) ;
            {INPUT a string}
            function Data_Read_Val( B : int64 ; G : Integer ;
                var _Result : integer ) : Longint ;
            function Data_Read_n( B : int64 ; G : integer ;
                var _Result : integer ) : string ;
            function Data_Read_Comp( B : int64 ; G : Integer ;
                var _Result : integer ) : int64 ;
            function Data_Read_Num( B : int64 ;
                var _Result : integer ) : Integer ;
            procedure Data_Write_Comp( XInt : int64 ; X : int64 ;
                Size : integer ) ; { Write CInt value to address X, Size bytes long. }
            function Pop_Byte : Byte ;
            function Pop_Word : smallint ;
            function Pop_Long : Longint ;
            function Pop_64 : int64 ;
            function Pop_3 : longint ;
            function Pop_Comp( X : integer ) : int64 ;
            procedure _Push( X : int64 ; Y : Integer ) ; {Push value to stack}
            procedure Pushn( P : PChar ; Y : integer ) ;
            function Read_Number_Data( B : int64 ) : Integer ; {Read pre-defined numeric}
            function Read_Code( X : Integer ) : int64 ; { Read X bytes from file }
            function Read_Comp( Stack_Pointer : int64 ; Y : integer ) : int64 ;
            function Read_Opcode : Integer ; {Read op-code}
            procedure Read_Integer( X : Longint ; var E : Boolean ;
                Z : Integer ; DT : TData_Type ) ;
            { INPUT an integer.  X is the address to write the value to.  E is true if a
              data error.  Z is 128 if doing NCM Input.  DT is the data type to input. }
            procedure Input_String( X : int64 ; Y : Integer ) ;
            { INPUT to string at address X, with flags Y }
            procedure Get_Input( Flags : integer ) ;
            function Resolve_Filename( const S : string ;
                System_File : boolean ) : string ;
            function _Disassemble( Address : int64 ; Base : longint ;
                var Size : longint ) : string ;
            function Set_Error( C : integer ) : TUEC ;
            function Bus_Read( Address : int64 ; Size, IO_Type : longint ;
                var E : boolean ) : int64 ; { Return data at Address }
            procedure Execute( Single_Step, Into : boolean ) ;
            procedure Clear_Watchpoints ;
            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUEC ;
            function Restore_State( Stream : TCOM_Stream ) : TUEC ;
            function Save_State( Stream : TCOM_Stream ) : TUEC ;
            procedure State_Change_Notice( Index : integer ; State : boolean ) ;
            procedure Log_Trace( const Description : string ) ;
            procedure Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
                Memory, Internal, Port : boolean ) ;
            function Instruction_At( Address : int64 ) : string ;
            procedure Redirect_Input( Source : PChar ) ;
            procedure Redirect_Output( Source : PChar ) ;
            function _Load_Image( const File_Name : string ;
                var Address : int64 ) : string ; { Load and map image in specified file }

            function Load_Symbol( File_Name : string ; const Symbol : string ;
                Vector : int64 ) : string ;
            { Load specified dynamic vector with address of symbol in image file.  Note that
              if Vector is 0, the address will be written to the global variable
              Loaded_Symbol_Address.  Note: This only works with code addresses. }

            function Get_IP : int64 ;
            procedure Set_IP( Value : int64 ) ;
            function Get_SP : int64 ;
            procedure Set_SP( Value : int64 ) ;
            function Get_SF : int64 ;
            procedure Set_SF( Value : int64 ) ;
            function Get_Temp0 : string ;
            procedure Set_Temp0( Value : string ) ;

            property IP : int64
                read Get_IP
                write Set_IP ;
            property SP : int64
                read Get_SP
                write Set_SP ;
            property SF : int64
                read Get_SF
                write Set_SF ;
            property Logger : TCEF_Logger
                read _Logger
                write _Logger ;
            property State : integer
                read _State
                write _State ;
            property Temp0 : string
                read Get_Temp0
                write Set_Temp0 ;

        protected // I/O routines...
            procedure Output_Text( S : string ) ;
            procedure Clear_KB_Buffer ;
            procedure Clear_Cursor( Vr, Vc : Integer ) ;
            procedure Clear_Window( W : integer ) ;
            function Create_Window( Row_Count, Column_Count, Flag : Integer ) : integer ;
            function Current_Window : integer ;
            procedure Delete_Line( W : integer ; Direction : Byte ) ;
            procedure Delete_Window( W : integer ) ;
            procedure Enable_SMU( Value : boolean ) ;
            procedure Erase_EOL( W : integer ) ;
            procedure Erase_EOW( W : integer ) ;
            function KB_Buffer : string ;
            function Get_Screen( Row, Col : Integer ) : Byte ;
            function Get_Window_Attribute( R, C : Integer ) : Byte ;
            function Get_Window_Char( R, C : Integer ) : Byte ;
            procedure Init_SMU( X, Y, Z, A : Integer ) ;
            procedure Insert_Line( W : integer ; Direction : Byte ) ;
            procedure Load_Driver( X : string ) ;
            procedure Map_Window( W : integer ; Row, Row_Count, Column,
		        Column_Count, View_R, View_C, Level : Byte ) ;
            function Readln_KB_Buffer : integer ;
            procedure Refresh_Screen ;
            procedure Refresh_Window( W : integer ) ;
            procedure Select_Window( W : integer ) ;
            procedure Set_Cursor( Vr, Vc : Integer ) ;
            procedure Set_Screen( Model, Printer_Model, Input_Buffers, Mode : Integer ) ;
            procedure Set_Viewport( Row, Column : Byte ) ;
            procedure Set_Window( W : integer ; Row, Column, Flag,
                Attributes : Integer ) ;
            function SMU_Initialized : boolean ;
            procedure Unmap_Window( W : integer ) ;
            procedure Update_Screen ;
            procedure Update_Window( W : integer ) ;
            function Window_Mapped( W : integer ) : boolean ;
            procedure Window_Read_Input( Handle, Flags : Integer ) ;

        public // API...
            Base : integer ;

            function Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;
                override ;

            function Cancel_Breakpoint( Address : int64 ;
                Space : integer ; Physical : boolean ) : TUEC ; override ;

            function Disassemble( Address : int64 ; Base, Size : longint ;
                Stream : TCOM_Stream ) : TUEC ; override ;

            function Get_Clock_Speed : longint ; override ;

            procedure Halt ; override ;

            function Halted : boolean ; override ;

            function Memory_Space_Description( Index : longint ;
                Physical : boolean ) : PChar ; override ;

            procedure Run ; override ;

            procedure Run_From_Stream( Stream : TCOM_Stream ) ; override ;

            function Set_Breakpoint( Address : int64 ; Space : integer ;
                Physical : boolean ) : TUEC ; override ;

            procedure Set_Clock_Speed( Value : longint ) ; override ;

            procedure Step( Into : boolean ) ; override ;

            function Translate( Space : integer ; Address : int64 ) : int64 ;
                override ;

            function Default_Base : integer ; override ;

            function Get_Low_Memory : int64 ; override ;

            function Get_High_Memory : int64 ; override ;

            function Get_Low_Virtual_Memory( Space : integer ) : int64 ; override ;

            function Get_High_Virtual_Memory( Space : integer ) : int64 ; override ;

            function Get_Low_Port : int64 ; override ;

            function Get_High_Port : int64 ; override ;

            function Support_Virtual_Address : boolean ; override ;

            function Segment_Size( Index : integer ) : integer ; override ;

            function Register_Name( Index : integer ) : PChar ; override ;

            function Register_Size( Index : integer ) : integer ; override ;

            function Register_Description( Index : integer ) : PChar ;
                override ;

            procedure Restart ; override ;

            function Page_Size : longint ; override ;

            function Clear_Internal_Watchpoint( Address : int64 ;
                Memory : boolean ; Access : integer ) : TUEC ; override ;

            function Set_Internal_Watchpoint( Address : int64 ;
                Memory : boolean ; Access : integer ) : TUEC ; override ;

            function Get_Current_Address( Space : integer ; Physical : boolean ) : int64 ;
                override ;

            procedure Set_Current_Address( Space : integer ; Physical : boolean ;
                Value : int64 ) ; override ;

            procedure Stop ; override ;

            function Top_Of_Stack( Index : integer ) : int64 ; override ;

            function Big_Endian : boolean ; override ;

            function Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;
                override ;

            function Register_Information( Index : longint ) : CEF.TData_Type ;
               override ;

            function Get_Store( Index : longint ) : TComponent ; override ;

            function Get_Target_Memory : TComponent ; override ;

            function Get_Target_Address_Space : longint ; override ;

            function Get_Stack_Interface( Space : integer ) : TCEF_Stack_Interface ;
                override ;
     end ; // TSVM_CPU


implementation

uses { Borland... }
     Windows, // LoadLibrary
     SysUtils, { Allocmem }

     { Other... }
     ASCIIDef, { CR }
     Compatib, // Small_String
     CVT, { Cvtb }
     Dates, // Sirius_Timestamp
     ErrMastr, // Get_Error_Text
     Filestd, // FILNAM
     _FileSys, // TFSSS
     _FiP,
     FSError, // FSErr_FileNotFound
     Helps, // Help
     HTML, { TXML_Parser }
     Instrs, // Instr
     Maths, // Power
     Num1s, { num1 }
     Parse, // TString_Parser
     SStreams, // TCOM_String_Stream
     Standard, // Bit_Values
     TypeDefs, // pByte_Array
     UStrings, // Extract

     { CEFSVM... }
     SVM_ASM, // TSVM_Assembler
     SOBJ, // POBJ_Symbol
     SopCodes, // Op_Null
     SiriusDT, // Type_Integer

     // CEF...
     CEFUtil_Int ; // Get_Watchpoint_Manager


const State_Normal = 0 ;
const State_Invalid_Memory_Reference = 1 ;
const State_Stack_Underflow = 2 ;

const Start_Block = 2 ; // Code starts in the second block of the image

const SVMErr_Facility = 6 ;
      SVMErr_Success = 0 ;
      SVMErr_Illegal_File_Format = 1 ;
      SVMErr_Invalid_Address = 2 ;
      SVMErr_No_Cache = 3 ;
      SVMErr_Invalid_Register = 4 ;
      SVMErr_No_Breakpoint = 5 ;
      SVMErr_Breakpoint_Exists = 6 ;
      SVMErr_Invalid_Component = 7 ;
      SVMErr_Already_Connected = 8 ;
      SVMErr_Component_Not_Found = 9 ;
      SVMErr_Invalid_Operation = 10 ;

type Pc_Type = array[ 0..7 ] of Byte ;

     TThread_Context = record
                           ID : longint ; { ID of this thread }
                           PC : PC_Type ; { Current PC of thread }
                           SF : longint ; { Current Stack Frame of thread }
                           SP : longint ; { Current Stack Pointer of thread }
                           Temp0 : string[ 255 ] ; { Current Temp0 of thread }
                       end ;
     PThread_Context = ^TThread_Context ;

     TImage = class
                  Name : string[ 255 ] ; { Name of image file }
                  Code_Start : integer ; { Starting block of code in file }
                  Code_Low, Code_High : longint ; { Mapped code address range }
                  Data_Base : longint ; { Mapped data address base }
                  Symbol_Start : longint ; { Starting block of symbol table }
              end ;

     PNative_Image = ^TNative_Image ;
     TNative_Image = record
                         Name : string[ 255 ] ; { Name of image file }
                         Handle : THandle ;
                     end ;


     TSVM_Stack_Interface = class( TCEF_Stack_Interface )
                                private // Instance data...
                                    _CPU : TSVM_CPU ;

                                public // API...
                                    procedure Terminate ; override ;

                                    // Lowest possible stack entry address...
                                    function Low_Bound : int64 ; override ;

                                    // Highest possible stack entry address...
                                    function High_Bound : int64 ; override ;

                                    // Size of each stack entry, in bits
                                    function Item_Size : longint ; override ;

                                    // Returns Indexth item in stack
                                    function Value( Index : int64 ) : int64 ;
                                        override ;

                                    // True if the stack grows up, false if it grows down
                                    function Grow_Up : boolean ; override ;
                            end ;

// API...

procedure TSVM_Stack_Interface.Terminate ;

begin
    Free ;
end ;


function TSVM_Stack_Interface.Low_Bound : int64 ;

begin
    Result := 0 ;
end ;


function TSVM_Stack_Interface.High_Bound : int64 ;

begin
    Result := _CPU._SP ;
end ;


function TSVM_Stack_Interface.Item_Size : longint ;

begin
    Result := 8 ;
end ;


function TSVM_Stack_Interface.Value( Index : int64 ) : int64 ;

var Size : integer ;

begin
    Size := 8 ;
    Result := 0 ;
    _CPU._Stack.Examine( Index, Size, @Result, True ) ;
end ;


function TSVM_Stack_Interface.Grow_Up : boolean ;

begin
    Result := True ;
end ;


// Profiler domains...
const Domain_Execution_Addresses = 0 ;
const Domain_Instructions = 1 ;
const Domain_Other = 2 ;
  const Domain_Other_Instruction_Count = 0 ;
  const Domain_Other_Clock = 1 ;


type TSVM_Data_Type = class( CEF.TData_Type )
                          public // Instance data...
                              _Data_Type : longint ;
                              _Size : longint ;
                              _Max_Size : longint ;
                              
                          public // API...
                              function Family : longint ; override ;
                              function Data_Type : longint ; override ;
                              function Size : longint ; override ;
                              function Big_Endian : boolean ; override ;
                              function Max_Size : longint ; override ;
                              function Signed : TTri_State ; override ;
                              function Mantissa : longint ; override ;
                              function Exponent : longint ; override ;
                              function Fixed : boolean ; override ;
                              function Fixed_Position : longint ; override ;
                              function Pack : boolean ; override ;
                              function Length_Encoding : longint ; override ;
                              function Prefix_Size : longint ; override ;
                              function Encoding : longint ; override ;
                      end ;

function TSVM_Data_Type.Family : longint ;

begin
    Result := _Data_Type and 7 ;
end ;


function TSVM_Data_Type.Data_Type : longint ;

begin
    Result := _Data_Type ;
end ;


function TSVM_Data_Type.Size : longint ;

begin
    Result := _Size ;
end ;


function TSVM_Data_Type.Big_Endian : boolean ;

begin
    Result := False ;
end ;


function TSVM_Data_Type.Max_Size : longint ;

begin
    Result := _Max_Size ;
end ;


function TSVM_Data_Type.Signed : TTri_State ;

begin
    Result := TS_Dont_Care ;
end ;


function TSVM_Data_Type.Mantissa : longint ;

begin
    Result := 64 ;
end ;


function TSVM_Data_Type.Exponent : longint ;

begin
    Result := 0 ;
end ;


function TSVM_Data_Type.Fixed : boolean ;

begin
    Result := False ;
end ;


function TSVM_Data_Type.Fixed_Position : longint ;

begin
    Result := 0 ;
end ;


function TSVM_Data_Type.Pack : boolean ;

begin
    Result := False ;
end ;


function TSVM_Data_Type.Length_Encoding : longint ;

begin
    Result := Datatype_String_Length_Terminated ;
end ;


function TSVM_Data_Type.Prefix_Size : longint ;

begin
    Result := 0 ;
end ;


function TSVM_Data_Type.Encoding : longint ;

begin
    Result := 0 ;
end ;



// Utility routines...

function Number_Format( Base : integer ; Value : String ; Size : integer ) : String ;
{ Return a number with proper number of leading zeroes.  Size is bytes }

var A : Integer ;

begin
    A := Size * 8 ;
    if( Base = 8 ) then
    begin
        A := A div 3 + 1 ;
    end else
    begin
        A := A div 4 ; { Determine number of digits }
    end ;
    if( Base = 10 ) then
    begin
        A := A + 1 ;
    end ;
    while( Length( Value ) < A ) do
    begin
        Value := '0' + Value ; { Pad with zeroes }
    end ;
    Number_Format := Value ; { Return result }
end ; { Numer_Format }


function Instruction_Name( I : integer ) : string ;

begin
    Result := inttostr( I ) ;
    case I of
        Op_Null : Result := 'NULL' ;
        Op_Exit : Result := 'EXIT' ;
        Op_Skip_1 : Result := 'SKIP1' ;
        Op_Skip_2 : Result := 'SKIP2' ;
        Op_Skip_3 : Result := 'SKIP3' ;
        Op_Skip_4 : Result := 'SKIP4' ;
        Op_Skip_5 : Result := 'SKIP5' ;
        Op_Skip_6 : Result := 'SKIP6' ;
        Op_Stop : Result := 'STOP' ;
        Op_Goto_Long : Result := 'GOTOL' ;
        Op_Goto_Word : Result := 'GOTOW' ;
        Op_Goto_Byte_Relative : Result := 'RGOTOB' ;
        Op_Goto_Long_Relative : Result := 'RGOTOL' ;
        Op_Goto_Quad : Result := 'GOTOQ' ;
        Op_Goto_Quad_Relative : Result := 'RGOTOQ' ;
        Op_Call_Long : Result := 'CALLL' ;
        Op_Call_Quad : Result := 'CALLQ' ;
        Op_Call_Quad_Relative : Result := 'RCALLQ' ;
        Op_End : Result := 'END' ;
        Op_If : Result := 'IF' ;
        Op_If_Relative : Result := 'RIF' ;
        Op_If_Quad : Result := 'IFQ' ;
        Op_If_Quad_Relative : Result := 'RIFQ' ;
        Op_Push_Long_Address : Result := 'PUSHILA' ;
        Op_Push_Direct_Long : Result := 'PUSHDL' ;
        Op_Pop_Direct_Long : Result := 'POPDL' ;
        Op_Push_Word : Result := 'PUSHIW' ;
        Op_Zero_Temp0 : Result := 'ZERO' ;
        Op_Push_n_Immediate : Result := 'PUSHIN' ;
        Op_Concat_Stack_To_Temp0 : Result := 'CONT' ;
        Op_Concat_Literal_To_Temp0 : Result := 'CONL' ;
        Op_Store_Temp0_Direct : Result := 'STOREDL' ;
        Op_Negate_Word : Result := 'NEGSI' ;
        Op_Add_Word : Result := 'ADDW' ;
        Op_Add_Long : Result := 'ADDL' ;
        Op_Subtract_Word : Result := 'SUBW' ;
        Op_Subtract_Long : Result := 'SUBL' ;
        Op_Multiply_Word : Result := 'MULW' ;
        Op_Multiply_Long : Result := 'MULL' ;
        Op_Divide_Word : Result := 'DIVW' ;
        Op_Divide_Long : Result := 'DIVL' ;
        Op_Integer_Compare_NE : Result := 'NEI' ;
        Op_Integer_Compare_EQ : Result := 'EQI' ;
        Op_Integer_Compare_LT : Result := 'LTI' ;
        Op_Integer_Compare_GT : Result := 'GTI' ;
        Op_Integer_Compare_LE : Result := 'LEI' ;
        Op_Integer_Compare_GE : Result := 'GEI' ;
        Op_String_Compare_NE : Result := 'NES' ;
        Op_String_Compare_EQ : Result := 'EQS' ;
        Op_String_Compare_LT : Result := 'LTS' ;
        Op_String_Compare_GT : Result := 'GTS' ;
        Op_String_Compare_LE : Result := 'LES' ;
        Op_String_Compare_GE : Result := 'GES' ;
        Op_Convert_Number : Result := 'CONVERT' ;
        Op_Create_String_From_Temp0 : Result := 'CREATE' ;
        Op_Push_Immediate_Longword_Type_Address : Result := 'PUSHITA' ;
        Op_Push_Direct_Word : Result := 'PUSHDW' ;
        Op_Pop_Direct_Word : Result := 'POPDW' ;
        Op_Call_Word : Result := 'CALLW' ;
        Op_NOT : Result := 'NOTS' ;
        Op_AND : Result := 'ANDS' ;
        Op_NAND : Result := 'NANDS' ;
        Op_OR : Result := 'ORS' ;
        Op_NOR : Result := 'NORS' ;
        Op_XOR : Result := 'XORS' ;
        Op_XNOR : Result := 'XNORS' ;
        Op_Push_Immediate_Byte : Result := 'PUSHIB' ;
        Op_Concat_Indirect : Result := 'CONS' ;
        Op_Pop : Result := 'POP' ;
        Op_Call_Byte : Result := 'CALLB' ;
        Op_Copy_String_Indirect : Result := 'COPY' ;
        Op_Push_n_Direct_Word : Result := 'POPNW' ;
        Op_Pop_n_Direct_Word : Result := 'PUSHNW' ;
        Op_Push_n_Direct_Long : Result := 'PUSHNL' ;
        Op_Pop_n_Direct_Long : Result := 'POPNL' ;
        Op_Push_Immediate_Long : Result := 'PUSHIL' ;
        Op_Push_Zeroes : Result := 'PUSH0' ;
        Op_Pop_n : Result := 'POPN' ;
        Op_Dereference : Result := 'DEREF' ;
        Op_Zero_String : Result := 'ZEROS' ;
        Op_Promote_Word_To_Long : Result := 'PROWL' ;
        Op_Range_Check : Result := 'RANGE' ;
        Op_Pop_To_Stack : Result := 'POPS' ;
        Op_Pop_n_To_Stack_Indirect_Long : Result := 'POPNSIL' ;
        Op_Reverse_Copy_Strings : Result := 'RCOPY' ;
        Op_Call_Long_Relative : Result := 'RCALLL' ;
        Op_Call_Long_Indirect : Result := 'CALLIL' ;
        Op_Memory_Compare_NE : Result := 'NEM' ;
        Op_Memory_Compare_EQ : Result := 'EQM' ;
        Op_Memory_Compare_LT : Result := 'LTM' ;
        Op_Memory_Compare_GT : Result := 'GTM' ;
        Op_Memory_Compare_LE : Result := 'LEM' ;
        Op_Memory_Compare_GE : Result := 'GEM' ;
        Op_Copy_n_Indirect : Result := 'COPYNI' ;
        Op_Call_Word_Relative : Result := 'RCALLW' ;
        Op_Goto_Word_Relative : Result := 'RGOTOW' ;
        Op_Call_Byte_Relative : Result := 'RCALLB' ;
        Op_Dereference_Integer : Result := 'DEREFI' ;
        Op_Call_Vector : Result := 'CALLV' ;
        Op_Negate_Stack : Result := 'NEGIS' ;
        Op_Add_Integer : Result := 'ADDI' ;
        Op_Subtract_Integer : Result := 'SUBI' ;
        Op_Multiply_Integer : Result := 'MULI' ;
        Op_Divide_Integer : Result := 'DIVI' ;
        Op_Percent_Integer : Result := 'PERI' ;
        Op_Exponent_Integer : Result := 'EXPI' ;
        Op_Factorial_Integer : Result := 'FACI' ;
        Op_Call_Dynamic : Result := 'CALLD' ;
        Op_Call_Native : Result := 'CALLN' ;
        Op_User_Extension : Result := 'UEXT' ;
    end ;
end ; // Instruction_Name


// TSVM_Profiler methods...

// API...

procedure TSVM_Profiler.Generate_Report ;

var Base : integer ;
    Loop : integer ;

begin
    // Clear dirty flag and set up...
    Dirty := False ;
    if( Outputs <> nil ) then
    begin
        Outputs.Clear ;
        Inputs.Clear ;
        Addresses.Clear ;
        Instruction_Lines.Clear ;
    end else
    begin
        Outputs := TStringList.Create ;
        Inputs := TStringList.Create ;
        Addresses := TStringList.Create ;
        Instruction_Lines := TStringList.Create ;
    end ;
    Base := 8 ;

    // Generate execution address report
    for Loop := 0 to 65535 do
    begin
        if( Execution_Addresses[ Loop ] <> 0 ) then
        begin
            Addresses.Add( cvtb( 10, Base, inttostr( Loop ) ) + ': ' + inttostr( Execution_Addresses[ Loop ] ) ) ;
        end ;
    end ;

    // Generate instruction report
    for Loop := 0 to 65535 do
    begin
        if( Instructions[ Loop ] <> 0 ) then
        begin
            Instruction_Lines.Add( Instruction_Name( Loop ) + ': ' + inttostr( Instructions[ Loop ] ) ) ;
        end ;
    end ;
    Instruction_Lines.Sort ;
end ; // TSVM_Profiler.Generate_Report


procedure TSVM_Profiler.Increment( Domain, Index : integer ) ;

begin
    Dirty := True ;
    case Domain of
        Domain_Execution_Addresses: inc( Execution_Addresses[ Index ] ) ;
        Domain_Instructions: inc( Instructions[ Index ] ) ;
        Domain_Other: if( Index = Domain_Other_Instruction_Count ) then
                      begin
                          inc( _Instruction_Count ) ;
                      end ;
    end ;
end ;


// Overrides...

procedure TSVM_Profiler.Clear( Domain : integer ) ;

begin
    if( Domain = -1 ) then
    begin
        Clear( Domain_Execution_Addresses ) ;
        Clear( Domain_Instructions ) ;
        Clear( Domain_Other ) ;
        exit ;
    end ;

    case Domain of
        Domain_Execution_Addresses :
            begin
                fillchar( Execution_Addresses, sizeof( Execution_Addresses ), 0 ) ;
                if( Addresses <> nil ) then
                begin
                    Addresses.Clear ;
                end ;
            end ;
        Domain_Instructions :
            begin
                fillchar( Instructions, sizeof( Instructions ), 0 ) ;
                if( Instruction_Lines <> nil ) then
                begin
                    Instruction_Lines.Clear ;
                end ;
            end ;
        Domain_Other :
            begin
                _Instruction_Count := 0 ;
            end ;
    end ; // case Domain
end ; // TSVM_Profiler.Clear


function TSVM_Profiler.Domain_Name( Index : integer ) : PChar ;

begin
    case Index of
        Domain_Execution_Addresses: Temp_Domain_Name := 'Executed addresses' ;
        Domain_Instructions: Temp_Domain_Name := 'Instructions' ;
        Domain_Other: Temp_Domain_Name := 'Other information' ;
        else
            begin
                Result := nil ;
                exit ;
            end ;
    end ;
    Result := PChar( Temp_Domain_Name ) ;
end ;


function TSVM_Profiler.Report_Line( Domain, Index : integer ) : PChar ;

begin
    if( Dirty ) then
    begin
        Generate_Report ;
    end ;
    Result := nil ;
    if( Index >= 0 ) then
    begin
        case Domain of
            Domain_Execution_Addresses:
                if( Index < Addresses.Count ) then
                begin
                    Temp_Report_Line := Addresses[ Index ] ;
                end else
                begin
                    exit ;
                end ;
            Domain_Instructions:
                if( Index < Instruction_Lines.Count ) then
                begin
                    Temp_Report_Line := Instruction_Lines[ Index ] ;
                end else
                begin
                    exit ;
                end ;
            Domain_Other:
                case Index of
                    Domain_Other_Instruction_Count : Temp_Report_Line := 'Instructions executed: ' + inttostr( _Instruction_Count ) ;
                    else exit ;
                end
            else exit ;
        end ;
    end ;
    Result := PChar( Temp_Report_Line ) ;
end ; // TSVM_Profiler.Report_Line



// TSVM_CPU methods...

// Constructors and destructors...

constructor TSVM_CPU.Create ;

begin
    inherited Create ;

    _Memory_Watchpoints := Get_Watchpoint_Manager ;
    _Breakpoints := TInteger_List.Create ;
    Base := Default_Base ;
    Max_Threads := 1 ;
    Code_High_Address := 255 ;
    Extensions := TList.Create ;
end ;


destructor TSVM_CPU.Destroy ;

begin
    Clear_Watchpoints ;
    _Memory_Watchpoints.Terminate ;
    _Memory_Watchpoints := nil ;
    _Breakpoints.Free ;
    _Breakpoints := nil ;
    Extensions.Free ;
    Extensions := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TSVM_CPU.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUEC ;

begin
    if( Memory ) then
    begin
        Result := _Memory_Watchpoints.Clear_Watchpoint( Address, Access ) ;
    end else
    begin
        Result := Set_Error( SVMErr_Invalid_Address ) ;
    end ; // if( Memory )
end ; // TSVM_CPU.Clear_Watchpoint


procedure TSVM_CPU.State_Change_Notice( Index : integer ; State : boolean ) ;

begin
    _UI.State_Change_Notice( Parent, Index, State ) ;
end ;


procedure TSVM_CPU.Log_Trace( const Description : string ) ;

begin
    if( _Trace ) then
    begin
        Temp_Log_Trace := Description ;
        _UI.Log_Trace( Parent, PChar( Temp_Log_Trace ) ) ;
    end ;
end ;


procedure TSVM_CPU.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
    Memory, Internal, Port : boolean ) ;

begin
    _UI.Watchpoint_Notice( Address, Access, Tag, Parent, Memory, Internal, Port ) ;
end ;


function TSVM_CPU.Instruction_At( Address : int64 ) : string ;

var Stream : TCOM_String_Stream ;

begin
    Stream := TCOM_String_Stream.Create ;
    Disassemble( Address, Base, 1, Stream ) ;
    Result := string( Stream.As_String ) ;
    Stream.Detach ;
end ;


procedure TSVM_CPU.Redirect_Input( Source : PChar ) ;

begin
    // Close existing redirection file...
    if( Default_Input_Redirected ) then
    begin
        Default_Input_Redirected := False ;
        {$I-}
        closefile( Default_Input ) ;
        {$I+}
        IOResult ;
    end ;
    if( Source <> nil ) then
    begin
        assignfile( Default_Input, string( Source ) ) ;
        {$I-}
        reset( Default_Input ) ;
        {$I+}
        if( IOResult = 0 ) then
        begin
            Default_Input_Redirected := True ;
        end ;
    end ;
end ;


procedure TSVM_CPU.Redirect_Output( Source : PChar ) ;

begin
    // Close existing redirection file...
    if( Default_Output_Redirected ) then
    begin
        Default_Output_Redirected := False ;
        {$I-}
        closefile( Default_Output ) ;
        {$I+}
        IOResult ;
    end ;
    if( Source <> nil ) then
    begin
        assignfile( Default_Output, string( Source ) ) ;
        {$I-}
        rewrite( Default_Output ) ;
        {$I+}
        if( IOResult = 0 ) then
        begin
            Default_Output_Redirected := True ;
        end ;
    end ;
end ;


type Code_Block = array[ 0..255 ] of byte ;

function TSVM_CPU._Load_Image( const File_Name : string ;
    var Address : int64 ) : string ; { Load and map image in specified file }

var C, E : Longint ;
    Dummy : integer ;
    Header : TSVM_Header ;
    Load : Code_Block ;
    Loop : integer ;
    Image : TSVM_Image ;
    Saved : integer ;
    Sharable : file ;
    This_Name : string ;

begin
    { Setup }
    Address := 0 ;
    _Load_Image := '' ; { Assume no errors }

    { Normalize file name }
    This_Name := Resolve_Filename( File_Name, True ) ;
    if( not FileExists( This_Name ) ) then
    begin
        This_Name := FILNAM( This_Name, False ) ;
        if( This_Name = '' ) then
        begin
            _Load_Image := 'Cannot find file: ' + File_Name ;
            exit ;
        end ;
    end ;

    { Open and check file... }
    assign( Sharable, This_Name ) ;
    Saved := FileMode ;
    FileMode := fmOpenRead or fmShareDenyNone ; { Read-only }
    {$I-}
    reset( Sharable, 1 ) ;
    {$I+}
    FileMode := Saved ;
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        _Load_Image := ERT( Dummy ) + ': ' + This_Name ;
        exit ;
    end ;
    try
        {$I-}
        blockread( Sharable, Header, sizeof( Header ) ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            _Load_Image := ERT( Dummy ) + ': ' + This_Name ;
            exit ;
        end ;
        if(
            ( Header.Format = 0 ) // .SOB
            or
            ( Header.OK_Flag <> 0 )
            or
            ( Header.Version > High_Version )
            or
            ( Header.Version < Low_Version )
          ) then { Not a binary file }
        begin
            _Load_Image := 'Invalid image file format: ' + This_Name ;
            exit ;
        end ;

        { Create image record (after checking for already loaded) }
        if( Images = nil ) then
        begin
            Images := TList.Create ;
            Image := TSVM_Image.Create ;
            Images.Add( Image ) ;
        end else
        begin
            for Loop := 0 to Images.Count - 1 do
            begin
                Image := TSVM_Image( Images[ Loop ] ) ;
                if( Image.Name = This_Name ) then { Already loaded }
                begin
                    Address := Image.Code_Low ;
                    _Load_Image := '' ;
                    exit ;
                end ;
            end ; { while( Image <> nil ) }
            Image := TSVM_Image.Create ;
            Images.Add( Image ) ;
        end ;

        { Set up image... }
        Image.Name := This_Name ;
        Image.Code_Start := 1 ;
        Image.Code_Low := Code_High_Address + 1 ; // Map image into memory
        Address := Image.Code_Low ;
        Image.Code_High := Image.Code_Low + Header.Total_Code_Size - 257 ;
        Code_High_Address := Image.Code_High ;
        E := ( Header.Total_Data_Size + 255 ) div 256 ;
        Dummy := ( Header.Total_Code_Size + 255 ) div 256 ;
        Image.Symbol_Start := E + Dummy ;
        if( ( Data_High_Address shr 16 ) <> ( ( Data_High_Address + E * 256 ) shr 16 ) ) then
        begin // No room in the current 64Kb data segment for this image's data
            Data_High_Address := ( Data_High_Address + 65535 ) and ( not 65535 ) ; // Position to next 64Kb boundary
        end ;
        Data_High_Address := Data_High_Address + 1;
        Image.Data_Base := Data_High_Address ;

        { Now load data space from image... }
        C := ( Header.Total_Code_Size + 255 ) div 256 ;
        Seek( Sharable, C * 256 ) ; { Position to beginning of data area }
        C := 0 ;
        while( C < E ) do { Load up data space }
        begin
            blockread( Sharable, Load, 256 ) ; { Get next 256 bytes }
            _Data_Cache.Write_String( Data_High_Address, PChar( @Load ), 256 * 8, IO_Type_Memory ) ;
            Inc( C ) ; { Count loaded bytes }
            Data_High_Address := Data_High_Address + 256 ;
        end ;
        Data_High_Address := Data_High_Address - 1;
    finally
        {$I-}
        close( Sharable ) ;
        {$I+}
        IOResult ;
    end ;
    _Code.Set_Up( PChar( 'LOAD "' + This_Name + '" FILE_OFFSET 256 CODE_OFFSET ' +
        inttostr( Image.Code_Low ) + ' SIZE ' + inttostr( Header.Total_Code_Size - 257 ) ) ) ;
end ; { TSVM_Interpreter._Load_Image }


function TSVM_CPU.Load_Symbol( File_Name : string ; const Symbol : string ;
    Vector : int64 ) : string ;
{ Load specified dynamic vector with address of symbol in image file.  Note that
  if Vector is 0, the address will be written to the global variable
  Loaded_Symbol_Address.  Note: This only works with code addresses. }

var Current_Symbol : integer ; { Current symbol in current block }
    Dummy : integer ; { Loop control }
    Found : boolean ;
    FSSS : TFSSS ;
    Image : TImage ;
    Image_File : file of Code_Block ;
    Load : Code_Block ;
    Loop : integer ;
    P : POBJ_Symbol ;
    Saved : integer ;

begin
    { Setup }
    Load_Symbol := '' ;
    if( Vector = 0 ) then
    begin
        Loaded_Symbol_Address := 0 ;
    end ;

    { Find image... }
    if( Images = nil ) then
    begin
        Load_Symbol := 'Image not loaded: ' + File_Name ;
        exit ;
    end ;
    Filespec_String_Scan( File_Name, '\', FSSS ) ;
    File_Name := FSSS.Filename ;
    Found := False ;
    for Loop := 0 to Images.Count - 1 do
    begin
        Image := TImage( Images[ Loop ] ) ;
        Filespec_String_Scan( Image.Name, '\', FSSS ) ;
        if( FSSS.Filename = File_Name ) then
        begin
            Found := True ;
            break ;
        end ;
    end ; // for Loop := 0 to Images.Count - 1
    if( not Found ) then
    begin
	    Load_Symbol := 'Image not loaded: ' + File_Name ;
	    exit ;
    end ;

    { Locate symbol in image's symbol table... }
    assignfile( Image_File, Image.Name ) ;
    Saved := FileMode ;
    FileMode := fmOpenRead or fmShareDenyNone ;
    reset( Image_File ) ;
    FileMode := Saved ;
    try
        Current_Symbol := 0 ;
        seek( Image_File, Image.Symbol_Start ) ;
        while( True ) do
        begin
            {$I-}
            read( Image_File, Load ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                Load_Symbol := 'Symbol not found: ' + Symbol ;
                exit ;
            end ;
            for Dummy := 0 to 7 do
            begin
                P := SOBJ.Get_Symbol( Current_Symbol, Object_Block( Load ) ) ;
                if( P^.Flags <> 0 ) then
                begin
                    if( ( P^.Flags and 4 ) = 4 ) then // Indicates a global symbol in an SVM image file
                    begin
                        if( edit( P^.Name, -1 ) = Symbol ) then
                        begin
                            if( Vector = 0 ) then
                            begin
                                Loaded_Symbol_Address :=
                                    P^.Address + Image.Code_Low ;
                            end else
                            begin
                                _Data_Write( '', P^.Address + Image.Code_Low,
                                    Vector, -4 ) ;
                            end ;
                            exit ;
                        end ; // if( edit( P^.Name, -1 ) = Symbol )
                    end ; // if( ( P^.Flags and 4 ) = 4 )
                end ; // if( P^.Flags <> 0 )
            end ; // for Dummy := 0 to 7
        end ; // while( True )
        Load_Symbol := 'Symbol not found: ' + Symbol ;
    finally
        closefile( Image_File ) ;
    end ;
end ; { Load_Symbol }


function Address_For_Log( Address : int64 ; Base : longint ) : string ;

begin
    if( ( Base < 2 ) or ( Base > 36 ) ) then // Makes no sense for an address
    begin
        Base := 10 ;
    end ;
    Result := cvtb( 10, Base, inttostr( Address ) ) ;
end ;


function Data_For_Log( Data, Base : longint ) : string ;

begin
    if( ( Base < 2 ) or ( Base > 36 ) ) then // Makes no sense for an address
    begin
        Base := 10 ;
    end ;
    Result := cvtb( 10, Base, inttostr( Data ) ) ;
end ;


function TSVM_CPU.Bus_Read( Address : int64 ; Size, IO_Type : longint ;
    var E : boolean ) : int64 ; { Return data at Address }

var Component : TComponent ;
    Loop : integer ;

begin
    E := False ;
    Memory_Data_Latch := 0 ; // Default if nothing responds
    Result := 0 ;

    try
        if( IO_Type = IO_Type_Memory ) then
        begin
            if( _Logger <> nil ) then
            begin
                if( ( IO_Type = IO_Type_Memory ) and ( not E ) ) then
                begin
                    _Logger.Log( Parent, PChar( Address_For_Log( Address, _Logger.Data_Radix ) +
                        ' = ' + Data_For_Log( Memory_Data_Latch, _Logger.Data_Radix ) ),
                         -1, False, LT_Read ) ;
                end ;
            end ;
        end ;

        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Address, Size, IO_Type ) ) then
            begin
                exit ;
            end ;
        end ;

        // If no memory connected to inputs, try outputs...
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
            if( Component.Read( Address, Size, IO_Type ) ) then
            begin
                exit ;
            end ;
        end ;

        E := True ;
    finally
        Bus_Read := Memory_Data_Latch ;
    end ;
end ; // TSVM_CPU.Bus_Read


function TSVM_CPU.Set_Error( C : integer ) : TUEC ;

begin
    _Last_Error.Facility := Facility_Code ;
    _Last_Error.Code := C ;
    Result := _Last_Error ;
end ;


function TSVM_CPU.Load_Native_Image( const File_Name : string ) : string ;
{ Load native image in specified file }

var Command : string ;
    FSSS : TFSSS ;
    Handle : THandle ;
    Image : PNative_Image ;
    Loop : integer ;
    P : PChar ;
    S, This_Name : string ;

begin
    { Setup... }
    Load_Native_Image := '' ; { Assume all is well }

    { Load the image... }
    S := File_Name ;
    if( _Observer <> nil ) then
    begin
        P := _Observer.DLL_Load( PChar( S ) ) ;
        if( P = nil ) then
        begin
            Output_Text( 'Error loading ' + S + CRLF ) ;
            exit ;
        end ;
        S := string( P ) ;
    end ;

    // Locate the file...
    This_Name := S ;
    if( ( pos( ':', S ) = 0 ) and ( copy( S, 1, 1 ) <> '\' ) ) then // No path, or relative path
    begin
        Filespec_String_Scan( paramstr( 0 ), '\', FSSS ) ;
        This_Name := FSSS.Device + FSSS.Path + S ;
        if( This_Name = '' ) then // Try directory with interpreter
        begin
            Filespec_String_Scan( paramstr( 0 ), '\', FSSS ) ;
            This_Name := FSSS.Device + FSSS.Path + S ;
        end ;
        if( This_Name = '' ) then // Try directory with program
        begin
            This_Name := FILNAM( S, False ) ;
        end ;
    end ;

    if( This_Name = '' ) then
    begin
        This_Name := S ;
    end ;

    Handle := LoadLibrary( PChar( This_Name ) ) ;
    if( Handle <= 32 ) then
    begin
	case Handle of
	    0 : Command := '?Corrupt file' ;
	    2 : Command := '?file not found' ;
	    3 : Command := '?Path not found' ;
	    5 : Command := '?Sharing or network protection error' ;
	    6 : Command := '?Library requires separate data segments' ;
	    7 : Command := '?Insufficient memory' ;
	    10 : Command := '?Incorrect windows version' ;
	    11 : Command := '?Invalid .EXE file' ;
	    12 : Command := '?Wrong operating system' ;
	    13 : Command := '?DOS 4.0 application' ;
	    14 : Command := '?Unknown .EXE type' ;
	    15 : Command := '?.EXE for earlier version of Windows' ;
	    16 : Command := '?Second instance of .EXE' ;
	    17 : Command := '?DLL in use' ;
	    18 : Command := '?Protected mode only' ;
	    19 : Command := '?Compressed executable' ;
	    20 : Command := '?Invalid DLL' ;
	    21 : Command := '?Requires Windows-32' ;
	    else Command := '?Unknown load failure' ;
	end ;
	Load_Native_Image := Command + ' - ' + File_Name ;
    end ;

    { Create image record (after checking for already loaded) }
    if( Native_Images = nil ) then
    begin
	    Native_Images := TList.Create ;
    end else
    begin
        for Loop := 0 to Native_Images.Count - 1 do
        begin
            Image := PNative_Image( Native_Images[ Loop ] ) ;
            if( Image^.Name = File_Name ) then { Already loaded }
            begin
                exit ;
            end ;
        end ; { while( Image <> nil ) }
    end ;
    new( Image ) ;

    { Set up image... }
    Image^.Name := File_Name ;
    Image^.Handle := Handle ;
    Native_Images.Add( Image ) ;
end ;


function TSVM_CPU.Load_Native_Symbol( const File_Name, Symbol : string ;
    Vector : longint ) : string ;
{ Load specified dynamic vector with address of symbol in native image file }

var Found : boolean ;
    Handle : TFarProc ;
    Image : PNative_Image ;
    Loop : integer ;

begin
    { Setup... }
    Load_Native_Symbol := '' ; { Assume all is well... }

    { Find image... }
    if( Native_Images = nil ) then
    begin
	    Load_Native_Symbol := 'Image not loaded: ' + File_Name ;
	    exit ;
    end ;
    Found := False ;
    for Loop := 0 to Native_Images.Count - 1 do
    begin
        Image := PNative_Image( Native_Images[ Loop ] ) ;
        if( Image.Name = File_Name) then
        begin
            Found := True ;
            break ;
        end ;
    end ; { while( ( Image <> nil ) and ( Image^.Name <> File_Name ) ) }
    if( not Found ) then
    begin
	    Load_Native_Symbol := 'Image not loaded: ' + File_Name ;
	    exit ;
    end ;

    { Locate symbol in image's symbol table... }
    Handle := GetProcAddress( Image^.Handle, PChar( Symbol ) ) ;
    if( Handle = nil ) then
    begin
	    Load_Native_Symbol := 'Symbol not found: ' + Symbol ;
	    exit ;
    end ;

    _Data_Write( '', longint( Handle ), Vector, -4 ) ;
end ;


function TSVM_CPU.Map_Virtual_To_Physical( A : longint ) : longint ;

begin
    Result := 0 ;
    if ( A and 1073741824 ) <> 0 then { Stack reference }
    begin
        if( _Stack <> nil ) then
        begin
            Result := Stack.Memory.Map_Virtual_To_Physical( A ) ;
        end ;
    end else
    begin
        if( _Data_Cache <> nil ) then
        begin
            Result := _Data_Cache.Memory.Map_Virtual_To_Physical( A ) ;
        end ;
    end ;
end ;


procedure TSVM_CPU.Err( S : string ) ;

begin
    _UI.Signal_Exception( Parent, PChar( S ), 0 ) ;
    _Halted := True ;
end ;


{ Converts a numeric value to a human-readable version }
function TSVM_CPU.Number_To_String( AR : int64 ; Data_Type : TData_Type ;
    var _Result : integer ) : string ;

var IP : double ;
    X : Comp ;
    YY : Small_String ;

begin
    { Handle reals }
    if( Data_Type.ID = Type_Real ) then
    begin
        YY := Data_Read_n( AR, Data_Type.Size, _Result ) ;
        move( YY, IP, Data_Type.Size ) ;
        str( IP, YY ) ;
        if( copy( YY, length( YY ) - 5, 6 ) = 'E+0000' ) then
        begin
            YY[ 0 ] := chr( length( YY ) - 6 ) ;
        end ;
        if( pos( '.', YY ) > 0 ) then
        begin
            while( YY[ length( YY ) ] = '0' ) do
            begin
                YY[ 0 ] := chr( length( YY ) - 1 ) ;
            end ;
        end ;
        Number_To_String := YY ;
        exit ;
    end ;

    { Handle integers }
    if(
	 ( Data_Type.Size < 4 )
	 or
	 (
	   ( Data_Type.Size = 4 )
	   and
	   ( Data_Type.Signed )
	 )
      ) then
    begin
        if(
             ( Data_Type.Size < 2 )
             or
             (
            ( Data_Type.Size = 2 )
            and
            ( Data_Type.Signed )
             )
          ) then
        begin
            Ar := Data_Read_Num( AR, _Result ) ;
            if( Data_Type.Size = 1 ) then
            begin
                AR := AR and $FF ;
                if( Data_Type.Signed and ( AR > $7F ) ) then
                begin
                    AR := AR or $FFFFFFFFFFFFFF00 ;
                end ;
            end ;
            if( Data_Type.Size = 2 ) then
            begin
                AR := AR and $FFFF ;
                if( Data_Type.Signed and ( AR > $7FFF ) ) then
                begin
                    AR := AR or $FFFFFFFFFFFF0000 ;
                end ;
            end ;
        end else
        begin
            // Integer > 2 bytes
            Ar := Data_Read_Val( AR, Data_Type.Size, _Result ) ;
            if( Data_Type.Signed ) then
            begin
                if( AR > Bit_Values[ Data_Type.Size * 8 - 1 ] ) then
                begin
                    case Data_Type.Size of { Sign-extend }
                        2 : AR := AR or $FFFFFFFFFF00 ;
                        3 : AR := AR or $FFFFFFFFFF0000 ;
                    end ;
                end ;
            end ;
        end ; // if
        Str( Ar, Yy ) ;
    end else
    begin
        // Integer > 4 bytes
        X := Data_Read_Comp( AR, Data_Type.Size, _Result ) ;
        if( Data_Type.Signed and ( Data_Type.Size < 8 ) ) then
        begin
            if( X > XBit_Values[ Data_Type.Size * 8 - 1 ] ) then
            begin
                move( X, YY, 8 ) ; { Sign-extend }
                fillchar( YY[ Data_Type.Size ], 8 - Data_Type.Size, 255 ) ;
                move( YY, X, 8 ) ;
            end ;
        end ;
        str( X:16:0, YY ) ;
        while( YY[ 1 ] = ' ' ) do
        begin
            YY := copy( YY, 2, length( YY ) ) ;
        end ;
    end ; // if
    Number_To_String := YY ;
end ; { Number_To_String }


function TSVM_CPU.Call( XX : Integer ) : boolean ; {Call a standard routine}

    function Parse_Parameter( const Separator : string ; var Command : string ) : string ;

    var S : string ;

    begin
        S := Command ;
        Result := Parse.Parse_Parameter( Separator, S ) ;
        Command := S ;
    end ;

var A, B, C : Integer ;
    Ar, Br, Cr : Longint ;
    B1, B3 : Boolean ;
    Buffer : array[ 0..255 ] of byte ; { 256 byte buffer }
    Data_Type : TData_Type ;
    Extension : TSVM_Extension ;
    F : Integer ;
    Fil : _Files.PSFile ;
    Int : Array[ 1..8 ] Of Integer ;
    IO : integer ;
    IP : integer ;
    Long : Array[ 1..8 ] Of Longint ;
    P, P1 : PChar ;
    Read_In : integer ;
    Temp_File : File ;
    In_File, Out_File : Longint ; {Pointer to input and output files }
    In_Fil, Out_Fil : _Files.PSFile ;
    UEC : tUEC ;
    Written_Out : integer ; { Number of bytes written out }
    X, X1 : int64 ;
    YY, YYY : string ;
    Z : Integer ;

begin // TSVM_CPU.Call
    // Setup and sanity check...
    Call := False ;

    // Give extensions a chance to execute
    for A := 0 to Extensions.Count - 1 do
    begin
        Extension := TSVM_Extension( Extensions[ A ] ) ;
        if( Extension.Valid_Call( XX ) ) then
        begin
            Call := Extension.Call( XX ) ;
            exit ;
        end ;
    end ;

    // Execute function...
    case XX of
	 0 : { RANDOM }
	     begin
             B := Pop_Word ; {Pop operand}
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             _Push( Random( B ), 2 )
	     end ;
	 1 : { EDIT }
	     begin
             B := Pop_Word ; {Get editing option}
             Ar := Pop_Long ; {Get string address}
             Cr := Pop_Long ; {Get temp string _Result address}
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             _Push( Cr, 4 ) ; {Temp string is return value}
             _Data_Write( Edit( Read_String( Ar, IP ), B ), 0, Cr, Type_String ) ;
             {Store _Result}
             if( IP <> 0 ) then
             begin
                 Err( '??Data address out of range' ) ;
                 Call := True ;
             end ;
	     end ;
	 2 : { COMPARE }
	     begin
             X := Pop_Comp( 8 ) ;
             X1 := Pop_Comp( 8 ) ;
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             X := X and X1 ;
             Pushn( @X, sizeof( X ) ) ;
	     end ;
	 3 : { SET }
	     begin
             {NOTE: Stack pointer stuff is done because first parameter
              is passed by reference, and the stack may contain
              subscripting or other information in which case both the
              Data_Read and Data_Write use it.  By restoring after the
              read, the Data_Write will work properly.}
             X := Pop_Comp( 8 ) ; {Get value}
             Ar := Pop_Long ; { RTTI for variable to set }
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             Data_Type.ID := Data_Read_Val( AR, 2, IP ) ; { Get type ID }
             Data_Type.Size := Data_Read_Val( AR + 2 + sizeof( boolean ), 2, IP ) ;
             Ar := Pop_Long ; { Address of variable to set }
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             Cr := _SP ; {Save current stack position}
             X1 := Data_Read_Comp( AR, Data_Type.Size, IP ) ;
             SP := Cr ; { Restore stack for modification }
             if( IP <> 0 ) then
             begin
	            Err( '??Data address out of range' ) ;
	            Call := True ;
                exit ;
             end ;
             X := X or X1 ;
             Data_Write_Comp( X, Ar, Data_Type.Size )
	     end ;
	 4 : { CLEAR }
	     begin
             X1 := Pop_Comp( 8 ) ;
             Ar := Pop_Long ; { RTTI for variable to set }
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             Data_Type.ID := Data_Read_Val( AR, 2, IP ) ; { Get type ID }
             Data_Type.Size := Data_Read_Val( AR + 2 + sizeof( boolean ), 2, IP ) ;
             Ar := Pop_Long ; { Address of variable to clear }
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             C := _SP ;
             X := Data_Read_Num( AR, IP ) ;
             SP := C ;
             if( IP <> 0 ) then
             begin
	            Err( '??Data address out of range' ) ;
	            Call := True ;
                exit ;
             end ;
             X := X and ( not X1 ) ;
             Data_Write_Comp( X, Ar, Data_Type.Size )
	     end ;
	 5 : { OUTPUT }
	     begin
             Out_File := 0 ;
             Z := Pop_Byte ; {Number of parameters}
             B := Z * 8 ; { Size of stack frame (minus 1) }
             { All parameters passed to OUTPUT consist of 2 stack entries }
             SP := _SP - B ;
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             { Backup to first parameter.  Each parameter is 8 bytes:
                4 bytes = value or address
                4 bytes = address of type information }
             while( Z > 0 ) do
             begin
                 Ar := _Read_Stack( _SP - Z * 8 + B, 4 ) ;
                 { Get parameter address/value }
                 if( State = State_Invalid_Memory_Reference ) then
                 begin
                     exit ;
                 end ;
                 IP := 0 ;
                 CR := _Read_Stack( _SP - Z * 8 + B + 4, 4 ) ;
                 { Get parameter type address }
                 if( State = State_Invalid_Memory_Reference ) then
                 begin
                     exit ;
                 end ;
                 Data_Type.ID := Data_Read_Val( CR, 2, IP ) ; { Get type ID }
                 Data_Type.Signed := Data_Read_Val( CR + 2, sizeof( boolean ), IP ) <> 0 ;
                 Data_Type.Size := Data_Read_Val( CR + 2 + sizeof( boolean ), 2, IP ) ;
                 if( Data_Type.ID = Type_External_File ) then
                 begin
                     Out_File := Data_Read_Val( Ar, 4, IP ) ;
                     { Get physical pointer from address of pointer }
                     Fil := _Files.PSFile( Out_File ) ;
                 end else
                 begin
                     YY := '' ;
                     try
                         if( Data_Type.ID = Type_String ) then { string }
                         begin
                             YY := Read_String( Ar, IP ) ;
                         end else
                         begin
                             YY := Number_To_String( AR, Data_Type, IP ) ;
                         end ;
                     except
                         on E : Exception do
                         begin
                             Err( '?' + E.Message ) ;
                             Call := True ;
                             exit ;
                         end ;
                     end ;
                     if( IP <> 0 ) then
                     begin
                        Err( '??Data address out of range' ) ;
                        Call := True ;
                        exit ;
                     end ;
                     if( Data_Type.ID <> Type_Ncm ) then {Not NCM}
                     begin
                         if( Out_File = 0 ) then
                         begin
                             Output_Text( YY ) ;
                         end else { Output to a file }
                         begin
                             Fil^.Blockwrite( PChar( Yy )[ 0 ], Length( Yy ), IO ) ;
                         end ;
                     end ;
                 end ;
                 if( SMU_Initialized ) then
                 begin
                     Update_Window( Current_Window ) ;
                 end ;
                 if ( Z = 1 ) and ( Data_Type.ID = Type_Ncm ) then
                 begin
                     exit ; {NCM is last parameter}
                 end ;
                 Z := Z - 1 ;
             end ; { while( Z > 0 ) }
             if( Out_File = 0 ) then
             begin
                 Output_Text( CRLF ) ;
             end else
             begin
                 Yy := CRLF ;
                 Fil^.Blockwrite( PChar( Yy )[ 0 ], 2, IO ) ;
             end
	     end ;
	 6 : { INPUT }
	     begin
             Out_File := 0 ;
             Z := Pop_Byte ; {Number of parameters}
             {All parameters passed to OUTPUT consist of 2 stack entries}
             CR := Pop_Long ; {Peek at last parameter}
             _Push( CR, 4 ) ;
             C := Data_Read_Val( CR, 2, IP ) ;
             if( C = Type_Ncm )
                then
                    F := 128 {do window input with NCM}
                else
                    F := 0 ;
             SP := _SP - Z * 8 ;
             if( _SP < 0 ) then { Stack underflow }
             begin
                 Err( '??Stack underflow' ) ;
                 Call := True ;
                 exit ;
             end ;
             { Backup to first parameter.  Each parameter is 8 bytes:
                4 bytes = value or address
                4 byte = address of type information }
             B3 := False ; {No data format errors thus far}
             B := 0 ;
             while( Z > 0 ) do {while parameters for INPUT()}
             begin
                 Ar := _Read_Stack( _SP + B, 4 ) ;
                 {Parameter address/value}
                 if( State = State_Invalid_Memory_Reference ) then
                 begin
                     exit ;
                 end ;
                 {INPUT integers are always passed by ref (address)}
                 CR := _Read_Stack( _SP + B + 4, 4 ) ;
                 { Get parameter type address }
                 if( State = State_Invalid_Memory_Reference ) then
                 begin
                     exit ;
                 end ;
                 Data_Type.ID := Data_Read_Val( CR, 2, IP ) ;
                 Data_Type.Signed := Data_Read_Val( CR + 2, sizeof( boolean ), IP ) <> 0 ;
                 Data_Type.Size := Data_Read_Val( CR + 2 + sizeof( boolean ), 2, IP ) ;
                 if( Data_Type.ID = Type_External_File ) then
                 begin
                     Out_File := Data_Read_Val( Ar, 4, IP ) ;
                     if( IP <> 0 ) then
                     begin
                         Err( '??Data address out of range' ) ;
                         Call := True ;
                         exit ;
                     end ;
                     Fil := _Files.PSFile( Out_File ) ;
                 end else
                 if( Out_File = 0 ) then // Keyboard input
                 begin
                     if( ( Data_Type.ID and 7 ) = Type_String ) then {string}
                     begin
                         Input_String( Ar, F ) ; { Input a string }
                     end else
                     if( Data_Type.ID = Type_NCM ) then { NCM }
                     begin
                         if( ( Z = 1 ) and ( not B3 ) ) then
                         begin
                             Exit ;
                         end ;
                         { NCM is last parameter and not any
                          errors }
                     end else
                     begin
                         Read_Integer( Ar, B1, F, Data_Type ) ;
                         {Input an integer}
                         B3 := B3 or B1 ;
                         {Accumulate any data format errors}
                     end ;
                 end else
                 if( ( Data_Type.ID and 7 ) = Type_String ) {string}
                     then
                         Read_File_String( Ar, Fil )
                         { Input a string }
                     else
                     if( Data_Type.ID <> Type_Ncm ) then
                     begin
                         Read_File_Integer( Ar, B1, Fil, Data_Type ) ;
                         { Input an integer }
                         B3 := B3 or B1 ;
                         {Accumulate any data format errors}
                     end ;
                 B := B + 8 ; { Increase stack offset }
                 Z := Z - 1 ; { One less parameter left }
             end ;
             if( B3 ) then
             begin
                 Output_Text( '%Data format error' + CRLF ) ;
                 Buffer_Empty := True ;
             end ;
             { Any integer problems }
	     end ;
	   7 : { OPEN file }
		begin
		    Long[ 2 ] := Pop_Long ; { Pop filename }
		    Long[ 1 ] := Pop_Long ; { Pop file variable }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Br := Data_Read_Val( Long[ 1 ], 4, IP ) ; { Get pointer to file record }
		    if( IP <> 0 ) then
		    begin
	            Err( '??Data address out of range' ) ;
                Call := True ;
		    end ;
		    if( Br <> 0 ) then { Not closed yet }
		    begin
                Fil := _Files.PSFile( BR ) ;
                dispose( Fil, Done ) ;
                _Data_Write( '', 0, Long[ 1 ], -4 ) ;
		    end ;
                YY := Read_String( Long[ 2 ], IP ) ;
                if( _Observer <> nil ) then
                begin
                    P := PChar( YY ) ;
                    P1 := nil ;
                    _Observer.File_Operation( P, P1, FO_Open ) ;
                    if( P = nil ) then
                    begin
                        Output_Text( 'Error opening ' + YY + CRLF ) ;
                        exit ;
                    end ;
                    YY := string( P ) ;
                end ;
		    Fil := _FiP.Open( YY, FSOM_Input, UEC ) ;
		    if(
                (
                  ( UEC.Code = 2 )
                  and
                  (
                    ( UEC.Facility = 1 )
                    or
                    ( UEC.Facility = 2 )
                  )
                )
                or
                (
                  ( UEC.Code = FSErr_FileNotFound )
                  and
                  ( UEC.Facility = FSErr_Facility )
                )
              ) then
		    begin { File not found }
			    Fil := _FiP.Open( Read_String( Long[ 2 ], IP ),
			    FSOM_Output, UEC ) ;
		    end ;
		    BR := longint( Fil ) ;
		    _Data_Write( '', Br, Long[ 1 ], -4 ) ;
		    if( UEC.Code <> 0 ) then
		    begin
			    Output_Text( strpas( Get_Error_Text( UEC, -1 ) ) + ' opening ' +
                    Read_String( Long[ 2 ], IP ) + CRLF ) ;
		    end ;
		end ;
	   8 : { CLOSE }
		begin
		    Long[ 1 ] := Pop_Long ;
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Br := Data_Read_Val( Long[ 1 ], 4, IP ) ;
		    if( IP <> 0 ) then
		    begin
                Err( '??Data address out of range' ) ;
                Call := True ;
		    end else
		    if( BR <> 0 ) then { Not already closed }
		    begin
                Fil := _Files.PSFile( BR ) ;
                dispose( Fil, Done ) ;
                _Data_Write( '', 0, Long[ 1 ], - 4 ) ;
		    end ;
		end ;
	   9 : { DELETE }
		begin
		    Long[ 1 ] := Pop_Long ; { Pop operand }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
            YY := Read_String( Long[ 1 ], IP ) ;
            if( _Observer <> nil ) then
            begin
                P := PChar( YY ) ;
                P1 := nil ;
                _Observer.File_Operation( P, P1, FO_Open ) ;
                if( P = nil ) then
                begin
                    Output_Text( 'Error deleting ' + YY + CRLF ) ;
                    Call := True ;
                    exit ;
                end ;
                YY := string( P ) ;
            end ;
		    if( IP <> 0 ) then
		    begin
	               Err( '??Data address out of range' ) ;
	               Call := True ;
		    end else
                    begin
                        assign( Temp_File, Resolve_Filename( YY, False ) ) ;
                        {$I-}
                        Erase( Temp_File ) ;
                        {$I+}
                        IP := IOResult ;
                        if( IP <> 0 ) then
                        begin
                           Err( ERT( IP ) + ' deleting file ' + YY ) ;
                           Call := True ;
                        end ;
                    end ;
		end ;
	  10 : { ABS }
		begin
		    X := Pop_Comp( 8 ) ;
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    if( X < 0 ) then
		    begin
			    X := -X ;
		    end ;
		    Pushn( @X, sizeof( X ) ) ;
		end ;
	  11 : { BINARY }
		begin
		    Long[ 1 ] := Pop_Long ; {Pop string header}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Val( Read_String( Long[ 1 ], IP ), X, Z ) ;
		    Pushn( @X, sizeof( X ) ) ;
		    if( IP <> 0 ) then
		    begin
	            Err( '??Data address out of range' ) ;
	            Call := True ;
		    end ;
		end ;
	  12 : { INSTR }
		begin
		    Long[ 3 ] := Pop_Long ; {Get substring header}
		    Long[ 2 ] := Pop_Long ; {Get search string header}
		    Long[ 1 ] := Pop_Word ; {Get start position}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    YY := Read_String( Long[ 2 ], IP ) ;
		    if( IP <> 0 ) then
		    begin
	            Err( '??Data address out of range: ' + inttostr( Long[ 2 ] ) ) ;
	            Call := True ;
		    end ;
		    _Push( Instr( Long[ 1 ], YY,
			Read_String( Long[ 3 ], IP ) ), 2 ) ;
		    if( IP <> 0 ) then
		    begin
	            Err( '??Data address out of range' ) ;
	            Call := True ;
		    end ;
		end ;
	  13 : {LENGTH}
		begin
		    _Push( Length( Read_String( Pop_Long, IP ) ), 2 ) ;
		    if( IP <> 0 ) then
		    begin
	            Err( '??Data address out of range' ) ;
	            Call := True ;
		    end ;
		end ;
	  14 : {SIGN}
		begin
		    Z := Pop_Word ;
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    if Z = 0
			then
			    _Push( 0, 2 )
			else
            if Z < 0
            then
                _Push( -1, 2 )
            else
                _Push( 1, 2 ) ;
		end ;
	  15 : { ASCII }
		begin
		    Long[ 2 ] := Pop_Word ; {Get integer}
		    Long[ 1 ] := Pop_Long ; {Get _Result string header}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Long[ 1 ], 4 ) ; {Leave _Result on stack}
		    _Data_Write( chr( Long[ 2 ] ), 0, Long[ 1 ], Type_String ) ;
		end ;
	  16 : { CHAR }
		begin
		    Long[ 1 ] := Pop_Long ; { Get value }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Yy := Read_String( Long[ 1 ], IP ) ;
		    if( IP <> 0 ) then
		    begin
	            Err( '??Data address out of range' ) ;
	            Call := True ;
		    end ;
		    if( Length( Yy ) = 0 )
			then
			    _Push( 0, 2 ) { Null strings always return 0 }
			else
			    _Push( Ord( Yy[ 1 ] ), 2 ) ;
		end ;
	  17 : { LEFT }
		begin
		    Long[ 3 ] := Pop_Word ; {Get value}
		    Long[ 2 ] := Pop_Long ; {Get string header}
		    Long[ 1 ] := Pop_Long ; {Get _Result string header}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Long[ 1 ], 4 ) ; {Leave _Result string address on stack}
		    _Data_Write( Copy( Read_String( Long[ 2 ], IP ), 1,
			Long[ 3 ] ), 0, Long[ 1 ], Type_String ) ;
		    if( IP <> 0 ) then
		    begin
	                Err( '??Data address out of range' ) ;
	                Call := True ;
		    end ;
		end ;
	  18 : {RIGHT}
		begin
		    Long[ 3 ] := Pop_Word ; {Get value}
		    Long[ 2 ] := Pop_Long ; {Get string header}
		    Long[ 1 ] := Pop_Long ; {Get _Result string header}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Long[ 1 ], 4 ) ; {Leave _Result string address on stack}
		    _Data_Write( Copy( Read_String( Long[ 2 ], IP ), Long[ 3 ],
			255 ), 0, Long[ 1 ], Type_String ) ;
		    if( IP <> 0 ) then
		    begin
	                Err( '??Data address out of range' ) ;
	                Call := True ;
		    end ;
		end ;
	  19 : { String }
		begin
		    Long[ 3 ] := Pop_Word ; {ASCII value}
		    Long[ 2 ] := Pop_Word ; {Count}
		    Long[ 1 ] := Pop_Long ; {Get _Result string header}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Long[ 1 ], 4 ) ; {Leave _Result string address on stack}
		    Set_Length( Yy, Long[ 2 ] ) ;
                    while( Long[ 2 ] > 0 ) do
                    begin
                        YY[ Long[ 2 ] ] := chr( Long[ 3 ] ) ;
                        dec( Long[ 2 ] ) ;
                    end ;
		    _Data_Write( Yy, 0, Long[ 1 ], Type_String ) ;
		end ;
	  20 : { TEST }
		begin
		    X := Pop_Comp( 8 ) ; { Value to update with }
		    Ar := Pop_Long ; { RTTI for variable to set }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Data_Type.ID := Data_Read_Val( AR, 2, IP ) ; { Get type ID }
		    Data_Type.Size := Data_Read_Val( AR + 2 + sizeof( boolean ), 2, IP ) ;
		    Long[ 1 ] := Pop_Long ; {Address of variable to test}
		    Z := Data_Read_Num( Long[ 1 ], IP ) ;
		    if( IP <> 0 ) then
		    begin
	                Err( '??Data address out of range' ) ;
	                Call := True ;
		    end ;
		    if( Z = 0 ) then { Semaphore not set }
		    begin
			_Push( -1, 2 ) ; {Push _Result}
			Data_Write_Comp( X, Long[ 1 ], Data_Type.Size )
		    end else
		    begin
			_Push( 0, 2 ) ;
		    end ;
		end ;
	  21 : { MID }
		begin
		    Long[ 4 ] := Pop_Word ; {Get value}
		    Long[ 3 ] := Pop_Word ; {Get value}
		    Long[ 2 ] := Pop_Long ; {Get string header}
		    Long[ 1 ] := Pop_Long ; {Get _Result string header}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Long[ 1 ], 4 ) ; {Leave _Result string address on stack}
		    _Data_Write( Copy( Read_String( Long[ 2 ], IP ),
			Long[ 3 ], Long[ 4 ] ), 0, Long[ 1 ], Type_String ) ;
		    if( IP <> 0 ) then
		    begin
	                Err( '??Data address out of range' ) ;
	                Call := True ;
		    end ;
		end ;
	  22 : { MAX }
		begin
		    X := Pop_Comp( 8 ) ; { Get values }
		    X1 := Pop_Comp( 8 ) ;
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    if( X > X1 ) then
		    begin
			    Pushn( @X, sizeof( X ) ) ;
		    end else
		    begin
			    Pushn( @X1, sizeof( X1 ) ) ;
		    end ;
		end ;
	  23 : { MIN }
		begin
		    X := Pop_Comp( 8 ) ; { Get values }
		    X1 := Pop_Comp( 8 ) ;
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    if( X < X1 ) then
		    begin
			    Pushn( @X, sizeof( X ) ) ;
		    end else
		    begin
			    Pushn( @X1, sizeof( X1 ) ) ;
		    end ;
		end ;
	  24 : { TRANSFER }
		begin
		    { Get file variables... }
		    Long[ 2 ] := Pop_Long ; { Destination file }
		    Long[ 1 ] := Pop_Long ; { Source file }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Out_File := Data_Read_Val( Long[ 2 ], 4, IP ) ;
		    In_File := Data_Read_Val( Long[ 1 ], 4, IP ) ;
		    Out_Fil := _Files.PSFile( Out_File ) ;
		    In_Fil := _Files.PSFile( In_File ) ;

		    { Reset files... }
		    AR := In_Fil^.Filepos ;
		    In_Fil^.Seek( 0 ) ;
		    YY := Out_Fil^.Filename ;
		    Out_Fil^.Close ;
		    Out_Fil^.Rewrite( YY ) ;

		    { Now copy the file... }
		    repeat
			    In_Fil^.blockread( Buffer, sizeof( Buffer ), Read_In ) ;
		        UEC := In_Fil^.IO_Error ;
			    if(
                    ( Read_In = 0 )
                    and
                    ( UEC.Code = FSErr_EndofFile )
                    and
                    ( UEC.Facility = FSErr_Facility )
                  ) then
                begin
                    break ;
                end ;
                Out_Fil^.blockwrite( Buffer, Read_In, Written_Out ) ;
                if( UEC.Code <> 0 ) then
                begin
                    Output_Text( strpas( Get_Error_Text( UEC, -1 ) ) + CRLF ) ;
                    exit ;
                end ;
                UEC := Out_Fil^.IO_Error ;
                if( UEC.Code <> 0 ) then
                begin
                    Output_Text( strpas( Get_Error_Text( UEC, -1 ) ) + CRLF ) ;
                    exit ;
                end ;
		    until ( Read_In = 0 ) or ( Read_In <> Written_Out ) ;

		    { end by setting file positions... }
		    In_Fil^.seek( AR ) ;
		    Out_Fil^.seek( AR ) ;
		end ;
	  25 : { CVT }
		begin
		    Long[ 4 ] := Pop_Word ; {Get value}
		    Long[ 3 ] := Pop_Word ; {Get value}
		    Long[ 2 ] := Pop_Long ; { string header }
		    Long[ 1 ] := Pop_Long ; {Get _Result string header}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Long[ 1 ], 4 ) ; {Leave _Result string address on stack}
		    _Data_Write( CVTB( Long[ 3 ], Long[ 4 ],
			Read_String( Long[ 2 ], IP ) ),
			0, Long[ 1 ], Type_String ) ;
		    if( IP <> 0 ) then
		    begin
                Err( '??Data address out of range' ) ;
                Call := True ;
		    end ;
		end ;

	  26 : { Empty }
		begin
		    Long[ 1 ] := Pop_Word ; { Starting position }
		    AR := Pop_Long ; { String header for string }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    YY := Read_String( Ar, IP ) ;
		    AR := Pop_Long ; { String header for substring }
		    YYY := Read_String( Ar, IP ) ;
		    Cr := Pop_Long ; {Get temp string _Result address}
		    _Push( Cr, 4 ) ; {Temp string is return value}
		    YY := Extract( YY, YYY, Long[ 1 ], 0 ) ;
		    _Data_Write( YY, 0, Cr, Type_String ) ;
		    { Store _Result }
		    if( IP <> 0 ) then
		    begin
			    Err( '??Data address out of range' ) ;
			    Call := True ;
		    end ;
		end ;

	  27 : { Time }
		begin
		    X := Sirius_Timestamp ;
		    Pushn( @X, 8 ) ;
		end ;

	  28 : { Command_Line }
		begin
		    Cr := Pop_Long ; {Get temp string _Result address}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Cr, 4 ) ; {Temp string is return value}
		    _Data_Write( Parse_Command_Line, 0, Cr, Type_String ) ;
		    { Store Result }
		end ;

	  29 : { Parse }
		begin
		    Long[ 2 ] := Pop_Long ; { Get source string }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    YYY := Read_String( Long[ 2 ], IP ) ;
		    Long[ 1 ] := Pop_Long ; { Get separator string }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    YY := Read_String( Long[ 1 ], IP ) ;
		    Cr := Pop_Long ; {Get temp string result address}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Cr, 4 ) ; {Temp string is return value}
		    _Data_Write( Parse_Parameter( YY, YYY ), 0, Cr, Type_String ) ;
		    { Store Result }
		    if( IP <> 0 ) then
		    begin
			    Err( '??Data address out of range' ) ;
			    Call := True ;
                exit ;
		    end ;
		    _Data_Write( YYY, 0, Long[ 2 ], Type_String ) ;
		    { Modify source string }
		end ;

	  99 : { Help }
		begin
		    Long[ 2 ] := Pop_Long ; { Get filename string }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    YYY := Read_String( Long[ 2 ], IP ) ;
		    Long[ 1 ] := Pop_Long ; { Get subject string }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    YY := Read_String( Long[ 1 ], IP ) ;
		    Help( YY, YYY ) ;
		end ;

	 100 : {WINDOW_MAPPED}
		begin
		    Long[ 1 ] := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    B1 := Window_Mapped( Long[ 1 ] ) ;
		    if B1
			then
			    _Push( -1, 2 )
			else
			    _Push( 0, 2 ) ;
		end ;
	 103 : {CREATE_WINDOW}
		begin
		    Int[ 4 ] := Pop_Word ; {Pop operand}
		    Int[ 3 ] := Pop_Word ; {Pop operand}
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Long[ 1 ] := Pop_Long ; { _Result address }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Long[ 1 ], 4 ) ; { _Result address is return value }
		    _Data_Write( '',
			longint( Create_Window( Int[ 2 ], Int[ 3 ], Int[ 4 ] ) ),
			Long[ 1 ], -4 ) ;
		end ;
	 104 : { UNMAP_WINDOW }
		begin
		    Br := Pop_Long ; { Pop operand }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Unmap_Window( Br ) ;
		end ;
	 105 : {MAP_WINDOW}
		begin
			Int[ 8 ] := Pop_Word ; {Pop operand}
			Int[ 7 ] := Pop_Word ; {Pop operand}
			Int[ 6 ] := Pop_Word ; {Pop operand}
			Int[ 5 ] := Pop_Word ; {Pop operand}
			Int[ 4 ] := Pop_Word ; {Pop operand}
			Int[ 3 ] := Pop_Word ; {Pop operand}
			Int[ 2 ] := Pop_Word ; {Pop operand}
			Long[ 1 ] := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Map_Window( Long[ 1 ], Int[ 2 ], Int[ 3 ],
			Int[ 4 ], Int[ 5 ], Int[ 6 ], Int[ 7 ], Int[ 8 ] )
		end ;
	 106 : {DELETE_WINDOW}
		begin
		    Br := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Delete_Window( Br )
		end ;
	 107 : {CLEAR_WINDOW}
		begin
		    Br := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Clear_Window( Br )
		end ;
	 108 : {UPDATE_WINDOW}
		begin
		    Br := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Update_Window( Br )
		end ;
	 109 : {UPDATE_SCREEN}
		Update_Screen ;
	 110 : {REFRESH_WINDOW}
		begin
		    Br := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Refresh_Window( Br )
		end ;
	 111 : {REFRESH_SCREEN}
		Refresh_Screen ;
	 112 : {INIT_SMU}
		begin
		    Int[ 4 ] := Pop_Word ; {Pop operand}
		    Int[ 3 ] := Pop_Word ; {Pop operand}
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Int[ 1 ] := Pop_Word ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Init_Smu( Int[ 1 ], Int[ 2 ], Int[ 3 ], Int[ 4 ] ) ;
            Using_SMU := True ;
		end ;
	 113 : {SET_SCREEN}
		begin
		    Int[ 4 ] := Pop_Word ; {Pop operand}
		    Int[ 3 ] := Pop_Word ; {Pop operand}
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Int[ 1 ] := Pop_Word ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Set_Screen( Int[ 1 ], Int[ 2 ], Int[ 3 ], Int[ 4 ] )
		end ;
	 114 : {SET_CURSOR}
		begin
		    B := Pop_Word ; {Pop operand}
		    A := Pop_Word ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Set_Cursor( A, B )
		end ;
	 115 : {CLEAR_CURSOR}
		begin
		    B := Pop_Word ; {Pop operand}
		    A := Pop_Word ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Clear_Cursor( A, B )
		end ;
	 116 : {SET_WINDOW}
		begin
		    Int[ 5 ] := Pop_Word ; {Pop operand}
		    Int[ 4 ] := Pop_Word ; {Pop operand}
		    Int[ 3 ] := Pop_Word ; {Pop operand}
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Long[ 1 ] := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Set_Window( Long[ 1 ], Int[ 2 ], Int[ 3 ],
			Int[ 4 ], Int[ 5 ] )
		end ;
	 117 : { SELECT_WINDOW }
		begin
		    Br := Pop_Long ; {Pop operand}
		    Select_Window( Br )
		end ;
	 118 : { ERASE_EOW }
		begin
		    Br := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Erase_EOW( Br )
		end ;
	 119 : { ERASE_EOL }
		begin
		    Br := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Erase_EOL( Br )
		end ;
	 120 : { INSERT_LINE }
		begin
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Long[ 1 ] := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Insert_Line( Long[ 1 ], Int[ 2 ] )
		end ;
	 121 : { DELETE_LINE }
		begin
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Long[ 1 ] := Pop_Long ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Delete_Line( Long[ 1 ], Int[ 2 ] ) ;
		end ;
	 122 : { GET_WINDOW_ATTRIBUTE }
		begin
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Int[ 1 ] := Pop_Word ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Get_Window_Attribute( Int[ 1 ], Int[ 2 ] ), 2 ) ;
		end ;
	 123 : { GET_SCREEN }
		begin
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Int[ 1 ] := Pop_Word ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Get_Screen( Int[ 1 ], Int[ 2 ] ), 2 ) ;
		end ;
	 124 : { LOAD_DRIVER }
		begin
		    Ar := Pop_Long ;
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Load_Driver( Read_String( Ar, IP ) ) ;
		    if( IP <> 0 ) then
		    begin
		        Err( '??Data address out of range' ) ;
		        Call := True ;
		    end ;
		end ;
	 125 : { GET_WINDOW_CHAR }
		begin
		    Int[ 2 ] := Pop_Word ; {Pop operand}
		    Int[ 1 ] := Pop_Word ; {Pop operand}
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Get_Window_Char( Int[ 1 ], Int[ 2 ] ), 2 ) ;
		end ;
	 126 : {SET_VIEWPORT}
		begin
		    B := Pop_Word ; { Pop operand }
		    A := Pop_Word ; { Pop operand }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    Set_Viewport( A, B )
		end ;
	 127 : {CURRENT_WINDOW}
		begin
		    Long[ 1 ] := Pop_Long ; { _Result address }
            if( _SP < 0 ) then { Stack underflow }
            begin
                Err( '??Stack underflow' ) ;
                Call := True ;
                exit ;
            end ;
		    _Push( Long[ 1 ], 4 ) ;
		    _Data_Write( '', Longint( Current_Window ), Long[ 1 ], -4 ) ;
		end ;
	 else
            begin
                Str( Xx, Yy ) ;
                Err( '?Call to bad address ' + Yy ) ; {Show error}
                Execution := SVM_IT_End_Abnormal ;
                Call := True ;
            end ;
    end ;
end ; { TSVM_CPU.Call }


type PSmall_String = ^Small_String ;

function Make_String( Len : integer ) : PSmall_String ;

begin
    if( Len > 256 ) then
    begin
        raise Exception.Create( 'String too long' ) ;
    end ;
    if( Len = 0 ) then
    begin
        raise Exception.Create( 'String too short' ) ;
    end ;
    Getmem( Result, Len ) ; { Allocate memory for string }
end ;


procedure Free_String( S_P : PSmall_String ; Len : integer ) ;

begin
    if( Len > 256 ) then
    begin
        raise Exception.Create( 'String too long' ) ;
    end ;
    if( Len = 0 ) then
    begin
        raise Exception.Create( 'String too short' ) ;
    end ;
    if( S_P^[ 0 ] <> chr( Len - 1 ) ) then
    begin
        raise Exception.Create( 'String length mismatch' ) ;
    end ;
    Freemem( S_P, Len ) ; { Allocate memory for string }
end ;


procedure TSVM_CPU._Data_Write( A : Small_String ; B, C : int64 ;
    D : Integer ) ;
{ A = string value to write, B = Value to write, C = Address, D = Datatype
  (if D<0 then the absolute value of D is the number of bytes to copy) }

var Loop : Integer ;
    Extension : TSVM_Extension ;
    F, Gr : Longint ;
    S : string ;
    S_P : PSmall_String ;

begin
    // Give extensions a chance to execute
    for Loop := 0 to Extensions.Count - 1 do
    begin
        Extension := TSVM_Extension( Extensions[ Loop ] ) ;
        if( Extension.Valid_Memory( C ) ) then
        begin
            S := A ;
            Extension.Write_Data( PChar( S ), B, C, D ) ;
            exit ;
        end ;
    end ;

    if( ( C and 1073741824 ) <> 0 ) then { Stack reference }
    begin
	    if( C > 0 ) then
        begin
            C := C and 1073741823 ; {Get actual offset}
        end ;
        C := C + _SF ; {Determine address on stack}
        if( D = Type_String ) then {Write a string}
        begin
            F := _Read_Stack( C, 4 ) ; { Get string header address from stack }
            Move( F, S_P, 4 ) ; { Convert address to string pointer }
            {string handling:
            Source string	Destination string	Action
            -------------	------------------	------
            1	zero-length	zero-length (nil)	do nothing
            2	zero-length	non-zero-length		Release space
            3	non-zero-length	zero-length (nil)	Allocate space
            4	non-zero-length	non-zero-length		if diffent length,
                                release space and
                                reallocate space.
                                if same length, copy.
            }
            if( S_P = nil ) then {Not defined yet}
            begin
                if( Length( A ) = 0 ) then
                begin
                    Exit ; {do nothing if nothing to do (1)}
                end ;
                S_P := Make_String( length( A ) + 1 ) ;
                Move( S_P, Gr, 4 ) ;
                Write_Stack( C, Gr + 1, 4 ) ; { Update string header }
                S_P^[ 0 ] := A[ 0 ] ;
            end else
            begin
                S_P := pointer( longint( S_P ) - 1 ) ;
                { string headers actually contain a pointer to the first
                  byte of string data.  The byte prior to that contains the
                  length. }
            end ;
            try
                if( Length( A ) = 0 ) then {Free up string memory (2)}
                begin
                    Free_String( S_P, Length( S_P^ ) + 1 ) ; {Release storage}
                    Write_Stack( C, 0, 4 ) ; {Zero string header}
                    Exit ;
                end ;
                if( length( A ) <> length( S_P^ ) ) then {(4)}
                begin
                    Free_String( S_P, Length( S_P^ ) + 1 ) ;
                    S_P := Make_String( Length( A ) + 1 ) ;
                    Move( S_P, Gr, 4 ) ;
                    Write_Stack( C, Gr + 1, 4 ) ;
                end ;
                S_P^ := A ; {Write string to memory}
            except
                on E : Exception do Err( '??' + E.Message ) ;
            end ;
        end else {Write anything else}
        begin
            if( D = Type_Integer ) then
            begin
                D := -2 ; { Number of bytes to write }
            end ;
            Write_Stack( C, B, -D ) ; { do the copy }
        end ;
        exit ;
    end ;
    if( D = Type_String ) then {Write a string}
    begin
        S_P := PSmall_String( Data_Cache_Read( C, 4 ) ) ;
        if( S_P = nil ) then { Not defined yet }
        begin
            if( Length( A ) = 0 ) then
            begin
                Exit ; {do nothing if nothing to do}
            end ;
            S_P := Make_String( Length( A ) + 1 ) ;
            Gr := longint( S_P ) + 1 ;
            Data_Cache.Write( C, Gr, 32, IO_Type_Memory ) ;
            S_P^[ 0 ] := A[ 0 ] ;
        end else
        begin
            S_P := pointer( longint( S_P ) - 1 ) ;
        end ;
        if( Length( A ) = 0 ) then
        begin
            Free_String( S_P, Length( S_P^ ) + 1 ) ; { Release storage }
            Data_Cache.Write( C, 0, 32, IO_Type_Memory ) ; { Update string header }
            Exit ;
        end ;
        if( length( A ) <> length( S_P^ ) ) then
        begin
            try
                Free_String( S_P, Length( S_P^ ) + 1 ) ;
                S_P := Make_String( Length( A ) + 1 ) ;
            except
                on E : Exception do
                begin
                    Err( '?' + E.Message ) ;
                    exit ;
                end ;
            end ;
            Gr := longint( S_P ) + 1 ;
            Data_Cache.Write( C, Gr, 32, IO_Type_Memory ) ;
        end ;
        S_P^ := A ; { Write string to memory }
    end else { Write anything else }
    begin
        if( D = Type_Integer ) then
        begin
            D := -2 ; {Number of bytes to write}
        end ;
        D := D * 8 ; // Convert from bytes to bits
        Data_Cache.Write( C, B, -D, IO_Type_Memory ) ; { do the copy }
    end ;
end ; // TSVM_CPU._Data_Write


procedure TSVM_CPU.Set_State( Value : integer ) ;

begin
    _State := Value ;
end ;


procedure TSVM_CPU.Clear_Watchpoints ;

begin
end ;


function TSVM_CPU.Register_Description( Index : integer ) : PChar ;

begin
    case Index of
        0 : Temp_Register_Description := 'IP' ;
        1 : Temp_Register_Description := 'SP' ;
        2 : Temp_Register_Description := 'SF' ;
        3 : Temp_Register_Description := 'Temp0' ;
        else Temp_Register_Description := '' ;
    end ;
    Result := PChar( Temp_Register_Description ) ;
end ;


procedure TSVM_CPU.Delete_Assembler( Sender : TAssembler ) ;

begin
    if( Sender = Last_Assembler ) then
    begin
        Last_Assembler := nil ;
    end ;
end ;


procedure TSVM_CPU.Finish_Assembler( Sender : TAssembler ;
    Status : TAssembler_Status ; PC : int64 ) ;

begin
    if( Code_High_Address < TSVM_Assembler( Sender ).High_Code ) then
    begin
        Code_High_Address := TSVM_Assembler( Sender ).High_Code ;
    end ;
    Data_High_Address := Data_High_Address + Status.Data ;
end ;


// I/O routines...

procedure TSVM_CPU.Output_Text( S : string ) ;

var Dummy : integer ;

begin
    if( Terminal <> nil ) then
    begin
        for Dummy := 1 to length( S ) do
        begin
            Terminal.Cable.Receive( nil, 0, ord( S[ Dummy ] ), 8, 1 ) ;
        end ;
    end ;
end ;


procedure TSVM_CPU.Clear_KB_Buffer ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Clear_KB_Buffer, True ) ;
    end ;
end ;


procedure TSVM_CPU.Clear_Cursor( Vr, Vc : Integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Clear_Cursor, True ) ;
        Terminal.Cable.Receive( nil, 0, VR, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, VC, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Clear_Window( W : integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Clear_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
    end ;
end ;


function TSVM_CPU.Create_Window( Row_Count, Column_Count, Flag : Integer ) : integer ;

var Size : integer ;

begin
    Result := 0 ;
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Create_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, Row_Count, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Column_Count, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Flag, 8, 0 ) ;
        Size := 32 ;
        Terminal.Examine( SVM_SMU_Create_Window, Size, @Result, False ) ;
    end ;
end ;


function TSVM_CPU.Current_Window : integer ;

var Size : integer ;

begin
    Result := 0 ;
    if( Terminal <> nil ) then
    begin
        Size := 32 ;
        Terminal.Examine( SVM_SMU_Current_Window, Size, @Result, False ) ;
    end ;
end ;


procedure TSVM_CPU.Delete_Line( W : integer ; Direction : Byte ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Delete_Line, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Direction, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Delete_Window( W : integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Delete_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Enable_SMU( Value : boolean ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Enable_SMU, True ) ;
        Terminal.Cable.Receive( nil, 0, ord( Value ), 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Erase_EOL( W : integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Erase_EOL, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Erase_EOW( W : integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Erase_EOW, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
    end ;
end ;


function TSVM_CPU.KB_Buffer : string ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_KB_Buffer, True ) ;
        Result := _KB_Buffer ;
        Clear_KB_Buffer ;
    end ;
end ;


function TSVM_CPU.Get_Screen( Row, Col : Integer ) : Byte ;

var Size : integer ;

begin
    Result := 0 ;
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Get_Screen, True ) ;
        Terminal.Cable.Receive( nil, 0, Row, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Col, 8, 0 ) ;
        Size := 32 ;
        Terminal.Examine( SVM_SMU_Get_Screen, Size, @Result, False ) ;
    end ;
end ;


function TSVM_CPU.Get_Window_Attribute( R, C : Integer ) : Byte ;

var Size : integer ;

begin
    Result := 0 ;
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Window_Attribute, True ) ;
        Terminal.Cable.Receive( nil, 0, R, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, C, 8, 0 ) ;
        Size := 32 ;
        Terminal.Examine( SVM_SMU_Window_Attribute, Size, @Result, False ) ;
    end ;
end ;


function TSVM_CPU.Get_Window_Char( R, C : Integer ) : Byte ;

var Size : integer ;

begin
    Result := 0 ;
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Window_Char, True ) ;
        Terminal.Cable.Receive( nil, 0, R, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, C, 8, 0 ) ;
        Size := 32 ;
        Terminal.Examine( SVM_SMU_Window_Char, Size, @Result, False ) ;
    end ;
end ;


procedure TSVM_CPU.Init_SMU( X, Y, Z, A : Integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Init_SMU, True ) ;
        Terminal.Cable.Receive( nil, 0, X, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Y, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Z, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, A, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Insert_Line( W : integer ; Direction : Byte ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Insert_Line, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Direction, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Load_Driver( X : string ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Load_Driver, True ) ;
        Terminal.Cable.Transmit_String( 0, PChar( X ), 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Map_Window( W : integer ; Row, Row_Count, Column,
    Column_Count, View_R, View_C, Level : Byte ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Map_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Row, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Row_Count, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Column_Count, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, View_R, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, View_C, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Level, 8, 0 ) ;
    end ;
end ;


function TSVM_CPU.Readln_KB_Buffer : integer ;

var Size : integer ;

begin
    Result := 0 ;
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Readln_KB_Buffer, True ) ;
        Size := 32 ;
        Terminal.Examine( SVM_SMU_Readln_KB_Buffer, Size, @Result, False ) ;
    end ;
end ;


procedure TSVM_CPU.Refresh_Screen ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Refresh_Screen, True ) ;
    end ;
end ;


procedure TSVM_CPU.Refresh_Window( W : integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Refresh_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Select_Window( W : integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Select_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Set_Cursor( Vr, Vc : Integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Set_Cursor, True ) ;
        Terminal.Cable.Receive( nil, 0, Vr, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Vc, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Set_Screen( Model, Printer_Model, Input_Buffers, Mode : Integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Set_Screen, True ) ;
        Terminal.Cable.Receive( nil, 0, Model, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Printer_Model, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Input_Buffers, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Mode, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Set_Viewport( Row, Column : Byte ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Set_Viewpoint, True ) ;
        Terminal.Cable.Receive( nil, 0, Row, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Column, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Set_Window( W : integer ; Row, Column, Flag,
    Attributes : Integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Set_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Row, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Column, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Flag, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Attributes, 8, 0 ) ;
    end ;
end ;


function TSVM_CPU.SMU_Initialized : boolean ;

var I : byte ;
    Size : integer ;

begin
    Result := False ;
    if( Terminal <> nil ) then
    begin
        I := 0 ;
        Size := 1 ;
        Terminal.Examine( SVM_SMU_Initialized, Size, @I, False ) ;
        Result := I <> 0 ;
    end ;
end ;


procedure TSVM_CPU.Unmap_Window( W : integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Unmap_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
    end ;
end ;


procedure TSVM_CPU.Update_Screen ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Update_screen, True ) ;
    end ;
end ;


procedure TSVM_CPU.Update_Window( W : integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Update_Window, True ) ;
        Terminal.Cable.Receive( nil, 0, W, 8, 0 ) ;
    end ;
end ;


function TSVM_CPU.Window_Mapped( W : integer ) : boolean ;

var I : integer ;
    Size : integer ;

begin
    Result := False ;
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Window_Mapped, True ) ;
        I := 0 ;
        Terminal.Cable.Receive( nil, 0, W, 1, 0 ) ;
        Size := 1 ;
        Terminal.Examine( SVM_SMU_Window_Mapped, Size, @I, False ) ;
        Result := I <> 0 ;
    end ;
end ;


procedure TSVM_CPU.Window_Read_Input( Handle, Flags : Integer ) ;

begin
    if( Terminal <> nil ) then
    begin
        Terminal.Signal_Change_Notice( Parent, SVM_SMU_Window_Read_Input, True ) ;
        Terminal.Cable.Receive( nil, 0, Handle, 8, 0 ) ;
        Terminal.Cable.Receive( nil, 0, Flags, 8, 0 ) ;
    end ;
end ;


function TSVM_CPU.Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;

begin
    Result := TSVM_Assembler.Create ;
    Last_Assembler := Result ;
    TSVM_Assembler( Result ).CPU := self ;
    //TSVM_Assembler( Result ).CPU_Parent := Parent ;
    Result.Initialize( Master ) ;
    TSVM_Assembler( Result ).Base := Base ;
    TSVM_Assembler( Result ).On_Delete := Delete_Assembler ;
    TSVM_Assembler( Result ).On_Finish := Finish_Assembler ;
end ;


function TSVM_CPU.Cancel_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUEC ;

var Index : integer ;

begin
    Result := Set_Error( SVMErr_No_Breakpoint ) ; // Assume failure
    if( ( Address < 0 ) or ( Address > $FFFF ) ) then
    begin
        exit ;
    end ;
    Index := _Breakpoints.Indexof( Address ) ;
    if( Index = -1 ) then
    begin
        exit ;
    end ;
    _Breakpoints.Delete( Index ) ;
    Result := Set_Error( 0 ) ;
end ;


function TSVM_CPU.Get_Clock_Speed : longint ;

begin
    Result := _Speed ;
end ;


procedure TSVM_CPU.Halt ;

begin
    _Halted := True ;
end ;


function TSVM_CPU.Halted : boolean ;

begin
    Result := _Halted ;
end ;


function TSVM_CPU.Memory_Space_Description( Index : longint ;
    Physical : boolean ) : PChar ;

begin
    case Index of
        0 : Temp_Memory_Space_Description := 'Code' ;
        1 : Temp_Memory_Space_Description := 'Data' ;
        2 : Temp_Memory_Space_Description := 'Stack' ;
        else Temp_Memory_Space_Description := '' ;
    end ;
    if( Temp_Memory_Space_Description = '' ) then
    begin
        Result := nil ;
    end else
    begin
        Result := PChar( Temp_Memory_Space_Description ) ;
    end ;
end ;


procedure TSVM_CPU.Run_From_Stream( Stream : TCOM_Stream ) ;

begin
    _Run_Stream := Stream ;
    Execute( False, False ) ;
    _Run_Stream := nil ;
end ;


procedure TSVM_CPU.Run ;

begin
    _Halted := False ;
    Execute( False, False ) ;
end ;


function TSVM_CPU.Set_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUEC ;

begin
    if( ( Address < 0 ) or ( Address > $FFFF ) ) then
    begin
        Result := Set_Error( SVMErr_Invalid_Address ) ;
        exit ;
    end ;
    if( _Breakpoints.Indexof( Address ) <> -1 ) then
    begin
        Result := Set_Error( SVMErr_Breakpoint_Exists ) ;
        exit ;
    end ;
    _Breakpoints.Add( Address ) ;
    Result := Set_Error( 0 ) ;
end ;


procedure TSVM_CPU.Set_Clock_Speed( Value : longint ) ;

begin
    _Speed := Value ;
end ;


procedure TSVM_CPU.Step( Into : boolean ) ;

begin
    _Halted := False ;
    Execute( True, Into ) ;
end ;


function TSVM_CPU.Page_Size : longint ;

begin
    Result := 4096 ; // Max page length
end ;


function TSVM_CPU.Clear_Internal_Watchpoint( Address : int64 ;
    Memory : boolean ; Access : integer ) : TUEC ;

var Max_Register : integer ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( SVMErr_No_Cache ) ;
    end else
    begin
        Max_Register := 3 ;
        if( ( Address < 0 ) or ( Address > Max_Register ) ) then
        begin
            Result := Set_Error( SVMErr_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] :=
                _Register_Watchpoints[ Address ] and ( not Access_None ) ;
            Result := Set_Error( 0 ) ;
        end ;
    end ;
end ;

function TSVM_CPU.Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUEC ;

var Max_Register : integer ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( SVMErr_No_Cache ) ;
    end else
    begin
        Max_Register := 3 ;
        if( ( Address < 0 ) or ( Address > Max_Register ) ) then
        begin
            Result := Set_Error( SVMErr_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] :=
                _Register_Watchpoints[ Address ] or Access ;
            Result := Set_Error( 0 ) ;
        end ;
    end ;
end ;


procedure TSVM_CPU.Stop ;

begin
    //Halt ;
end ;


function TSVM_CPU.Restore_State( Stream : TCOM_Stream ) : TUEC ;

var Dummy : integer ;
    Loop : integer ;
    Parser : TXML_Parser ;
    S : string ;

begin
    // Setup default state...
    _Halted := False ;
    Waiting := False ;
    _Profiling := False ;
    fillchar( _Register_Watchpoints, sizeof( _Register_Watchpoints ), 0 ) ;
    _Breakpoints.Clear ;
    Clear_Watchpoints ;

    // Parse the image...
    Parser := TXML_Parser.Create ;
    Dummy := Stream.Size ;
    setlength( S, Dummy ) ;
    Stream.Read( S[ 1 ], Dummy ) ;
    Parser.Set_Source( PChar( S ) ) ;
    while( not Parser.EOF ) do
    begin
        S := uppercase( Parser.Token ) ;
        if( copy( S, 1, 1 ) = '<' ) then
        begin
            if( S = '<BASE>' ) then
            begin
                S := Parser.Get_Section( 'base' ) ;
                try
                    Base := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<WAIT/>' ) then
            begin
                Waiting := True ;
            end else
            if( S = '<TRACE/>' ) then
            begin
                _Trace := True ;
            end else
            if( S = '<SPEED>' ) then
            begin
                S := Parser.Get_Section( 'speed' ) ;
                try
                    _Speed := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<PROFILING/>' ) then
            begin
                _Profiling := True ;
            end else
            if( S = '<IP>' ) then
            begin
                S := Parser.Get_Section( 'IP' ) ;
                try
                    _IP := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<SF>' ) then
            begin
                S := Parser.Get_Section( 'SF' ) ;
                try
                    _SF := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<BREAKPOINTS>' ) then
            begin
                S := Parser.Get_Section( 'Breakpoints' ) ;
                _Breakpoints.DeSerialize( S ) ;
            end else
            if( S = '<REGISTER_WATCHPOINTS>' ) then
            begin
                S := Parser.Get_Section( 'Register_Watchpoints' ) ;
                S := copy( S, 2, length( S ) ) ; // Trim first bar
                Loop := 0 ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( '|', S ) ;
                    try
                        _Register_Watchpoints[ Loop ] := strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop ) ;
                    if( Loop > 20 ) then
                    begin
                        break ;
                    end ;
                end ;
            end else
            if( S = '<MEMORY_WATCHPOINTS>' ) then
            begin
                _Memory_Watchpoints.Deserialize( PChar( Parser.Get_Section( 'Memory_watchpoints' ) ) ) ;
            end else
            begin
                if( copy( S, length( S ) - 1, 2 ) <> '/>' ) then
                begin
                    Parser.Get_Section( PChar( S ) ) ;
                end ;
            end ;
        end ; // if( copy( S, 1, 1 ) = '<' )
    end ; // while( not Parser.EOF )
    Parser.Free ;
end ; // TSVM_CPU.Restore_State


function TSVM_CPU.Save_State( Stream : TCOM_Stream ) : TUEC ;

    procedure Output( S : string ) ;

    begin
        Stream.Write( S[ 1 ], length( S ) ) ;
    end ;

var Loop : integer ;

begin
    Output( '<base>' + inttostr( Base ) + '</base>' ) ;
    if( _Trace ) then
    begin
        Output( '<trace/>' ) ;
    end ;
    Output( '<speed>' + inttostr( _Speed ) + '</speed>' ) ;
    if( _Profiling ) then
    begin
        Output( '<Profiling/>' ) ;
    end ;

    Output( '<ip>' + inttostr( _IP ) + '</ip>' ) ;
    Output( '<sf>' + inttostr( _SF ) + '</sf>' ) ;
    Output( '<sp>' + inttostr( _SP ) + '</sp>' ) ;
    Output( '<Breakpoints>' + _Breakpoints.Serialize + '</Breakpoints>' ) ;
    Output( '<Register_Watchpoints>' ) ;
    for Loop := 0 to 3 do
    begin
        Output( '|' + inttostr( _Register_Watchpoints[ Loop ] ) ) ;
    end ;
    Output( '</Register_Watchpoints>' ) ;

    Output( '<Memory_watchpoints>' ) ;
    Output( '<watchpoint>' + string( _Memory_Watchpoints.Serialize ) ) ;
    Output( '</Memory_watchpoints>' ) ;
end ; // TSVM_CPU.Save_State


function TSVM_CPU.Resolve_Filename( const S : string ;
    System_File : boolean ) : string ;

var Dummy : integer ;
    FSSS : TFSSS ;
    This_Name : string ;

begin
    Result := S ;
    if( pos( ':', S ) > 0  ) then // Fully qualified or root
    begin
        exit ;
    end ;
    if( System_File ) then // Look for system files in program's location
    begin
        Filespec_String_Scan( paramstr( 0 ), '\', FSSS ) ;
        This_Name := FSSS.Device + FSSS.Path + S ;
        if( not FileExists( This_Name ) ) then // Non-existant (system files must exist)
        begin
            // Try directory with interpreter
            Filespec_String_Scan( paramstr( 0 ), '\', FSSS ) ;
            This_Name := FSSS.Device + FSSS.Path + S ;
        end ;
        Result := This_Name ;
        exit ;
    end ;
    if( _Redirect_Path <> '' ) then
    begin
        if( copy( S, 1, 1 ) = '\' ) then // Root specified
        begin
            Dummy := pos( ':', _Redirect_Path ) ;
            if( Dummy > 0 ) then // Device in redirect
            begin
                Result := copy( _Redirect_Path, 1, Dummy ) + S ; // Prepend device
            end ;
            exit ;
        end ;
        Result := _Redirect_Path + S ;
    end ;
end ; // TSVM_CPU.Resolve_Filename


function Data_Address_Description( A : int64 ) : string ;

begin
    Result := inttostr( A ) ;
    if( ( A and 1073741824 ) = 1073741824 ) then
    begin
        Result := 'SF+' + inttostr( A and 1073741823 ) ;
    end ;
end ;


function TSVM_CPU._Disassemble( Address : int64 ; Base : longint ;
    var Size : longint ) : string ;

    function Base_Suffix( Base : integer ) : string ;

    begin
        case Base of
            16 : Result := 'H' ;
            10 : Result := '.' ;
            2 : Result := 'B' ;
            else Result := '' ;
        end ;
    end ;


    function Cvis( Value, Size : int64 ) : string ;

    begin
        if( Value < 0 ) then
        begin
            Result := Num1( Value ) + '.' ;
            exit ;
        end ;
        Result := Cvtb( 10, Base, Num1( Value ) ) ;
        while( length( Result ) < Size ) do
        begin
            Result := '0' + Result ;
        end ;
        if( pos( Result[ 1 ], '0123456789' ) = 0 ) then
        begin
            Result := '0' + Result ;
        end ;
        Result := Result + Base_Suffix( Base ) ;
    end ;


var _Result : string ;

    procedure Set_Result( const OpCode, Operands : string ) ;

    begin
        _Result := OpCode ;
        if( ( OpCode <> '' ) and ( Operands <> '' ) ) then
        begin
            _Result := _Result + ' ' + Operands ;
        end ;
    end ;


    function Read_Opcode : byte ; {Get next op-code}

    var E : boolean ;

    begin
        if( Code.Read( Address, 8, IO_Type_Memory ) ) then // Code memory responded
        begin
            Result := Memory_Data_Latch ;
            inc( Address ) ;
            exit ;
        end ;
        Result := Bus_Read( Address, 8, IO_Type_Memory, E ) ;
        inc( Address ) ;
    end ;


    function Read_Byte : byte ;

    begin
        Segments.Add( 8 ) ;
        Result := Read_OpCode ;
    end ;


    function Read_Word : int64 ;

    var E : boolean ;

    begin
        Segments.Add( 16 ) ;
        if( Code.Read( Address, 16, IO_Type_Memory ) ) then // Code memory responded
        begin
            Result := Memory_Data_Latch ;
            Address := Address + 2 ;
            exit ;
        end ;
        Result := Bus_Read( Address, 16, IO_Type_Memory, E ) ;
        Address := Address + 2 ;
    end ;


    function Read_Long : longint ;

    var E : boolean ;

    begin
        Segments.Add( 32 ) ;
        if( Code.Read( Address, 32, IO_Type_Memory ) ) then // Code memory responded
        begin
            Result := Memory_Data_Latch ;
            Address := Address + 4 ;
            exit ;
        end ;
        Result := Bus_Read( Address, 32, IO_Type_Memory, E ) ;
        Address := Address + 4 ;
    end ;


    function Read_Quad : int64 ;

    var E : boolean ;

    begin
        Segments.Add( 64 ) ;
        if( Code.Read( Address, 64, IO_Type_Memory ) ) then // Code memory responded
        begin
            Result := Memory_Data_Latch ;
            Address := Address + 8 ;
            exit ;
        end ;
        Result := Bus_Read( Address, 64, IO_Type_Memory, E ) ;
        Address := Address + 8 ;
    end ;


var D : Longint ;
    A, B, C : Integer ;
    Br : int64 ;
    Extension : TSVM_Extension ;
    Loop : integer ;
    P : PChar ;
    PC : int64 ;
    Temp0, Temp1 : string ;

begin // TSVM_CPU._Disassemble
    PC := Address ;
    A := Read_Opcode ;
    _Result := '' ;
    Segments.Free ;
    Segments := TInteger_List.Create ;
    Segments.Add( 8 ) ; // Opcode

    case A of
         Op_Null : Set_Result( 'NULL', '' ) ; {Null}
         Op_Exit : Set_Result( 'EXIT', '' ) ; {EXIT}
         Op_Stop : Set_Result( 'STOP', '' ) ; {STOP}
         Op_Goto_Long : {GOTO longword}
             Begin
                 BR := Read_Long ;
		         Set_Result( 'GOTOL', Cvis( int64( Br ), 0 ) ) ;
             End ;
         Op_Call_Long : {Call routine (longword)}
             Begin
                 BR := Read_Long ; {Address of called routine}
		         Set_Result( 'CALLL', Cvis( int64( Br ), 0 ) ) ;
             End ;
         Op_End : Set_Result( 'END', '' ) ; {END}
         Op_If : {IF...THEN}
             Begin
                 BR := Read_Long ;
		         Set_Result( 'IF', Num1( int64( Br ) ) ) ;
             End ;
         Op_Push_Long_Address : {Push immediate longword address}
             Begin
                 BR := Read_Long ;
                 Set_Result( 'PUSHILA', Num1( int64( Br ) ) ) ;
             End ;
         Op_Push_Direct_Long : {Push direct (longword address)}
             Begin
                 BR := Read_Long ;
                 Set_Result( 'PUSHDL', Num1( int64( Br ) ) ) ;
             End ;
         Op_Pop_Direct_Long : {Pop word direct (longword address)}
             Begin
                 BR := Read_Long ;
                 Set_Result( 'POPDL', Num1( Br ) ) ;
             End ;
        Op_Push_Word : {Push immediate word}
             Begin
                 B := Read_Word ;
                 Set_Result( 'PUSHIW', Num1( B ) ) ;
             End ;
        Op_Zero_Temp0 : Set_Result( 'ZERO', '' ) ; {Zero Temp0}
        Op_Push_n_Immediate :
            begin
                Segments.Add( 8 ) ;
                C := Read_Opcode ; {Get length}
                if( C > 0 ) then
                begin
                    Segments.Add( C * 8 ) ;
                end ;
                Temp0 := '' ;
                Set_Result( 'PUSHIN', Num1( C ) ) ;
                if( C > 8 ) then
                begin
                    _Result := _Result + ',*' ;
                end else
                begin
                    Temp0 := '' ;
                    for D := 1 to C do
                    begin
                        Temp0 := Temp0 + Chr( Read_Opcode ) ;
                    end ;
                    BR := 0 ;
                    move( PChar( Temp0 )[ 0 ], BR, C ) ;
                    _Result := _Result + ',' + num1( BR ) ;
                end ;
            end ;
        Op_Concat_Stack_To_Temp0 : Set_Result( 'CONT', '' ) ;
	     {Concatenate top of stack to Temp0}
        Op_Concat_Literal_To_Temp0 : {Concatenate literal to Temp0}
             begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ; {Get length}
		         Temp0 := '' ;
                 if( C > 0 ) then
                 begin
                     Segments.Add( C * 8 ) ;
                     for D := 1 to C do
                     begin
                         Temp0 := Temp0 + Chr( Read_Opcode ) ;
                     end ;
                 end ;
		         Set_Result( 'CONL "' + Temp0 + '"', '' ) ;
             end ;
        Op_Store_Temp0_Direct : {Store Temp0 direct}
             Begin
                 BR := Read_Long ;
                 Set_Result( 'STOREDL', Num1( int64( Br ) ) ) ;
             End ;
        Op_Negate_Word : Set_Result( 'NEGSI', '' ) ;
	     {Negate top of stack - integer}
        Op_Add_Word : Set_Result( 'ADDW', '' ) ;
	     {Add - integer}
        Op_Subtract_Word : Set_Result( 'SUBW', '' ) ;
	     {Subtract - integer}
        Op_Multiply_Word : Set_Result( 'MULW', '' ) ;
	     {Multiply - integer}
        Op_Divide_Word : Set_Result( 'DIVW', '' ) ;
	     {Divide - integer}
        Op_Integer_Compare_NE : {Integer comparison <>}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 if( C > 127 ) then
                 begin
                     C := C Or Swap( 255 ) ; {Sign extend}
                     Set_Result( 'NEIU', Num1( C ) ) ;
                 end else
                 begin
                     Set_Result( 'NEIS', Num1( C ) ) ;
                 end ;
             End ;
        Op_Integer_Compare_EQ : {Integer comparison =}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 if C > 127 then
                 begin
                     C := C Or Swap( 255 ) ; {Sign extend}
                     Set_Result( 'EQIU', Num1( C ) ) ;
                 end else
                 begin
                     Set_Result( 'EQIS', Num1( C ) ) ;
                 end ;
             End ;
        Op_Integer_Compare_LT : {Integer comparison <}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 if C > 127 then
                 begin
                     C := C Or Swap( 255 ) ; {Sign extend}
                     Set_Result( 'LTIU', Num1( C ) ) ;
                 end else
                 begin
                     Set_Result( 'LTIS', Num1( C ) ) ;
                 end ;
             End ;
        Op_Integer_Compare_GT : {Integer comparison >}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 if C > 127 then
                 begin
                     C := C Or Swap( 255 ) ; {Sign extend}
                     Set_Result( 'GTIU', Num1( C ) ) ;
                 end else
                 begin
                     Set_Result( 'GTIS', Num1( C ) ) ;
                 end ;
             End ;
        Op_Integer_Compare_LE : {Integer comparison <=}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 if C > 127 then
                 begin
                     C := C Or Swap( 255 ) ; {Sign extend}
                     Set_Result( 'LEIU', Num1( C ) ) ;
                 end else
                 begin
                     Set_Result( 'LEIS', Num1( C ) ) ;
                 end ;
             End ;
        Op_Integer_Compare_GE : {Integer comparison >=}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 if C > 127 then
                 begin
                     C := C Or Swap( 255 ) ; {Sign extend}
                     Set_Result( 'GEIU', Num1( C ) ) ;
                 end else
                 begin
                     Set_Result( 'GEIS', Num1( C ) ) ;
                 end ;
             End ;
        Op_String_Compare_NE : Set_Result( 'NES', '' ) ;
        Op_String_Compare_EQ : Set_Result( 'EQS', '' ) ;
        Op_String_Compare_LT : Set_Result( 'LTS', '' ) ;
        Op_String_Compare_GT : Set_Result( 'GTS', '' ) ;
        Op_String_Compare_LE : Set_Result( 'LES', '' ) ;
        Op_String_Compare_GE : Set_Result( 'GES', '' ) ;
        Op_Convert_Number : Set_Result( 'CONVERT', '' ) ;
        Op_Create_String_From_Temp0 : Set_Result( 'CREATE', '' ) ;
             {Create string from TEMP0 and put address on stack}
        Op_Push_Immediate_Longword_Type_Address :
            begin
                BR := Read_Long ;
		        Set_Result( 'PUSHITA', Num1( int64( Br ) ) ) ;
            end ;
        Op_Push_Direct_Word : {Push word direct (word)}
             Begin
                 B := Read_Word ;
		         Set_Result( 'PUSHDW', Num1( B ) ) ;
             End ;
        Op_Pop_Direct_Word : {Pop word direct (word)}
             Begin
                 B := Read_Word ; {Get low byte}
		         Set_Result( 'POPDW', Num1( B ) )
             End ;
        Op_Call_Word : {Call routine (word)}
             Begin
                 B := Read_Word ; {Get low byte}
		         Set_Result( 'CALLW', Num1( B ) ) ;
		         if B < ( Start_Block - 1 ) * 256 then
                 begin
		             _Result := _Result + ' (internal)' ;
                 end ;
             End ;
        Op_NOT : Set_Result( 'NOTS', '' ) ; {NOT}
        Op_AND : Set_Result( 'ANDS', '' ) ; {AND}
	    Op_NAND : Set_Result( 'NANDS', '' ) ; {NAND}
        Op_OR : Set_Result( 'ORS', '' ) ; {OR}
	    Op_NOR : Set_Result( 'NORS', '' ) ; {NOR}
        Op_XOR : Set_Result( 'XORS', '' ) ; {XOR}
	    Op_XNOR : Set_Result( 'XNORS', '' ) ; {XNOR}
        Op_Push_Immediate_Byte : {Push immediate byte}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 Set_Result( 'PUSHIB', Num1( C ) ) ;
             End ;
        Op_Goto_Word : {GOTO word}
             Begin
                 Segments.Add( 8 ) ;
                 BR := Read_Word ;
		         Set_Result( 'GOTOW', Num1( int64( Br ) ) )
             End ;
        Op_Concat_Indirect : Set_Result( 'CONS', '' ) ;
	     {Concatenate strings address by stack}
        Op_Pop : Set_Result( 'POP', '' ) ; {Pop Stack}
        Op_Call_Byte : {Call routine (byte) - this will only call built-in routines}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 Set_Result( 'CALLB', Num1( C ) ) ;
             End ;
        Op_Copy_String_Indirect : Set_Result( 'COPY', '' ) ;
	     {Copy strings whose addresses are on stack}
        Op_Push_n_Direct_Word : {Push n bytes direct (word)}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ; {Number of bytes to push}
                 B := Read_Word ; {Address to read from}
		         Set_Result( 'PUSHNW', Num1( C ) + ' ' + Num1( B ) ) ;
             End ;
        Op_Pop_n_Direct_Word : {Pop n bytes direct (word)}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ; {Number of bytes to push}
                 B := Read_Word ;
		         Set_Result( 'POPNW', Num1( C ) + ' ' + Num1( B ) ) ;
             End ;
         Op_Push_n_Direct_Long : {Push n bytes direct (longword address)}
             Begin
                 Segments.Add( 8 ) ;
                 B := Read_Opcode ; {Number of bytes to push}
                 BR := Read_Long ;
                 Set_Result( 'PUSHNL', Num1( B ) + ' ' + Num1( int64( Br ) ) ) ;
             End ;
         Op_Pop_n_Direct_Long : {Pop n bytes direct (longword address)}
             Begin
                 Segments.Add( 8 ) ;
                 B := Read_Opcode ; {Number of bytes to pop}
                 Segments.Add( 8 ) ;
                 BR := Read_Long ;
                 Set_Result( 'POPNL', Num1( B ) + ' ' + Num1( int64( Br ) ) ) ;
             End ;
        Op_Push_Immediate_Long : {Push immediate longword constant}
             Begin
                 BR := Read_Long ;
                 Set_Result( 'PUSHIL', Num1( int64( Br ) ) ) ;
             End ;
        Op_Push_Zeroes : {Push zeroes to stack}
	         Begin
                 B := Read_Word ; {Get count}
		         Set_Result( 'PUSH0', Num1( B ) ) ;
	         End ;
        Op_Pop_n : {Pop n bytes from stack}
             begin
                B := Read_Word ; {Get count}
		        Set_Result( 'POPN', Num1( B ) ) ;
	         End ;
        Op_Dereference : Set_Result( 'DEREF', '' ) ;
        Op_Zero_String : Set_Result( 'ZEROS', '' ) ;
        Op_Promote_Word_To_Long : Set_Result( 'PROWL', '' ) ;
        Op_Range_Check : Set_Result( 'RANGE', '' ) ;
        Op_Pop_To_Stack : Set_Result( 'POPS', '' ) ;
        Op_Pop_n_To_Stack_Indirect_Long : {Pop n bytes to stack indirect (longword address)}
             Begin
                 Segments.Add( 8 ) ;
                 B := Read_Opcode ; {Number of bytes to pop}
                 C := Read_Long ;
                 Set_Result( 'POPNSIL', Num1( B ) + ' ' + Data_Address_Description( C ) ) ;
             End ;
        Op_Reverse_Copy_Strings : Set_Result( 'RCOPY', '' ) ;
	     {Copy strings whose addresses are on stack}
        Op_Goto_Byte_Relative : {GOTO byte relative}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
		         if C > 127 then
			         C := C or ( not $FF ) ;
		         Set_Result( 'RGOTOB', Num1( C ) ) ;
             End ;
        Op_Goto_Long_Relative : {GOTO longword relative}
             Begin
                 BR := Read_Long ;
		         Set_Result( 'RGOTOL', Num1( int64( Br ) ) ) ;
             End ;
        Op_Call_Long_Relative : {Call routine (longword relative)}
             Begin
                 BR := Read_Long ;
		         Set_Result( 'RCALLL', Num1( int64( Br ) ) ) ;
             End ;
        Op_If_Relative : {IF...THEN (relative)}
             begin
                 BR := Read_Long ;
		         Set_Result( 'RIF', Num1( int64( Br ) ) ) ;
             end ;
	    Op_Add_Long : Set_Result( 'ADDL', '' ) ;
	     {Add - long}
        Op_Subtract_Long : Set_Result( 'SUBL', '' ) ;
	     {Subtract - long}
        Op_Multiply_Long : Set_Result( 'MULL', '' ) ;
	     {Multiply - long}
        Op_Divide_Long : Set_Result( 'DIVL', '' ) ;
	     {Divide - long}
	    Op_Call_Long_Indirect: Set_Result( 'CALLIL', '' ) ;
	    Op_Copy_n_Indirect:
	        begin
                C := Read_Word ;
		        Set_Result( 'COPYNI', Num1( C ) ) ;
	        end ;
        Op_Memory_Compare_NE :
	         begin
                 C := Read_Word ;
		         Set_Result( 'NEM', Num1( C ) ) ;
	         end ;
        Op_Memory_Compare_EQ :
	         begin
                 C := Read_Word ;
		         Set_Result( 'EQM', Num1( C ) ) ;
	         end ;
        Op_Memory_Compare_LT :
	         begin
                 C := Read_Word ;
                 Set_Result( 'LTM', Num1( C ) ) ;
             end ;
        Op_Memory_Compare_GT :
	         begin
                 C := Read_Word ;
                 Set_Result( 'GTM', Num1( C ) ) ;
             end ;
        Op_Memory_Compare_LE :
	         begin
                 C := Read_Word ;
                 Set_Result( 'LEM', Num1( C ) ) ;
             end ;
        Op_Memory_Compare_GE :
	         begin
                 C := Read_Word ;
		         Set_Result( 'GEM', Num1( C ) ) ;
	         end ;
        Op_Call_Word_Relative : {Call routine (word relative)}
	         begin
                 C := Read_Word ;
                 if( C > 32767 ) then
                 begin
{$R-}
                     C := C or $FFFF0000 ;
{$R+}
                 end ;
                 Set_Result( 'RCALLW', Num1( C ) ) ;
             end ;
        Op_Goto_Word_Relative : {GOTO word relative}
             Begin
                 C := Read_Word ;
                 if( C > 32767 ) then
                 begin
{$R-}
                     C := C or $FFFF0000 ;
{$R+}
                 end ;
		         Set_Result( 'RGOTOW', Num1( C ) ) ;
             End ;
       Op_Call_Byte_Relative : {Call routine (byte relative)}
             Begin
                 Segments.Add( 8 ) ;
                 C := Read_Opcode ;
                 Segments.Add( 8 ) ;
                 if C > 127 then
                     C := C or ( not $FF ) ;
                 Set_Result( 'RCALLB', Num1( C ) ) ;
             End ;
       Op_Dereference_Integer : {Dereference integer on top of stack}
             begin
                 Segments.Add( 8 ) ;
                 C := Read_OpCode ;
                 Segments.Add( 8 ) ;
                 Set_Result( 'DEREFI', Num1( C ) )
             end ;
       Op_Call_Vector :
           begin
               Set_Result( 'CALLV', '' ) ;
           end ;
       Op_Negate_Stack : {Negate top of stack - integer}
           begin
               Segments.Add( 8 ) ;
               C := Read_Opcode ;
               Segments.Add( 8 ) ;
               if C > 127 then
               begin
                   C := C Or Swap( 255 ) ; {Sign extend}
               end ;
               Set_Result( 'NEGIS', Num1( C ) ) ;
            end ;
       Op_Add_Integer,Op_Subtract_Integer,Op_Multiply_Integer,Op_Divide_Integer:
            begin
                Segments.Add( 8 ) ;
                C := Read_Opcode ;
                Segments.Add( 8 ) ;
                if C > 127 then
                begin
                    C := C Or Swap( 255 ) ; {Sign extend}
                end ;
                case A of
                    Op_Add_Integer:
                                begin
                                    Temp1 := 'ADDI' ;
                                end ;
                    Op_Subtract_Integer:
                                begin
                                    Temp1 := 'SUBI' ;
                                end ;
                    Op_Multiply_Integer:
                                begin
                                    Temp1 := 'MULI' ;
                                end ;
                    Op_Divide_Integer:
                                begin
                                    Temp1 := 'DIVI' ;
                                end ;
                end ;
                Set_Result( Temp1, Num1( C ) ) ;
             end ;
       Op_Percent_Integer :
           begin
               Segments.Add( 8 ) ;
               C := Read_Opcode ;
               if C > 127 then
               begin
                   C := C Or Swap( 255 ) ; {Sign extend}
               end ;
               Set_Result( 'PERI', Num1( C ) ) ;
            end ;
       Op_Exponent_Integer :
            begin
                Segments.Add( 8 ) ;
                C := Read_Opcode ;
                if C > 127 then
                begin
                    C := C Or Swap( 255 ) ; {Sign extend}
                end ;
                Set_Result( 'EXPI', Num1( C ) ) ;
             end ;
       Op_Factorial_Integer :
           begin
                Segments.Add( 8 ) ;
                C := Read_Opcode ;
                if C > 127 then
                begin
                    C := C Or Swap( 255 ) ; {Sign extend}
                end ;
                Set_Result( 'FACI', Num1( C ) ) ;
             end ;
         Op_Call_Dynamic : { Call dynamic routine (longword) }
             Begin
                 BR := Read_Long ; { Address of vector }
                 Set_Result( 'CALLD', Num1( int64( Br ) ) ) ;
             End ;
         Op_Call_Native : { Call native routine (longword) }
             Begin
                 BR := Read_Long ; { Address of vector }
                 Set_Result( 'CALLN', Num1( int64( Br ) ) ) ;
             End ;
	     Op_User_Extension :
            begin
                Segments.Add( 8 ) ;
                A := Read_Opcode ;
                for Loop := 0 to Extensions.Count - 1 do
                begin
                    Extension := TSVM_Extension( Extensions[ Loop ] ) ;
                    if( Extension.Valid_Instruction( A ) ) then
                    begin
                        P := Extension.Disassemble( Address ) ;
                        _Result := string( P ) ;
                        A := 256 ;
                    end ;
                end ;

                if( A < 256 ) then  // Not recognized by any loaded extension
                begin
                    Set_Result( 'UEXT', Num1( A ) ) ; { User extension }
                end ;
            end ;
        Op_Skip_1 : Set_Result( 'SKIP1', '' ) ;
        Op_Skip_2 : Set_Result( 'SKIP2', '' ) ;
        Op_Skip_3 : Set_Result( 'SKIP3', '' ) ;
        Op_Skip_4 : Set_Result( 'SKIP4', '' ) ;
        Op_Skip_5 : Set_Result( 'SKIP5', '' ) ;
        Op_Skip_6 : Set_Result( 'SKIP6', '' ) ;
        Op_Call_Quad :
            begin
                BR := Read_Quad ;
                Set_Result( 'CALLQ', Num1( int64( Br ) ) ) ;
            end ;
        Op_Call_Quad_Relative :
            begin
                BR := Read_Quad ;
                Set_Result( 'RCALLQ', Num1( int64( Br ) ) ) ;
            end ;
        Op_Goto_Quad :
            begin
                BR := Read_Quad ;
                Set_Result( 'GOTOQ', Num1( int64( Br ) ) ) ;
            end ;
        Op_Goto_Quad_Relative :
            begin
                BR := Read_Quad ;
                Set_Result( 'RGOTOQ', Num1( int64( Br ) ) ) ;
            end ;
        Op_If_Quad :
            begin
                BR := Read_Quad ;
		        Set_Result( 'IFQ', Num1( int64( Br ) ) ) ;
            end ;
        Op_If_Quad_Relative :
            begin
                BR := Read_Quad ;
                Set_Result( 'RIFQ', Num1( int64( Br ) ) ) ;
            end ;
        else {Unused}
            begin
                Set_Result( '?Illegal op-code', '' ) ;
            end ;
    end ;
    Temp_Disassemble := trim( _Result ) ;
    Result := PChar( Temp_Disassemble ) ;
end ; { TSVM_CPU._Disassemble }


function TSVM_CPU.Terminal : TComponent ;

begin
    if( _Terminal = nil ) then
    begin
        _Terminal := _UI.Load_Component( 'SVMScreen' ) ;
    end ;
    Result := _Terminal ;
end ;


function TSVM_CPU.Stack : TComponent ;

var Loop : integer ;

begin
    if( _Stack = nil ) then
    begin
        _Stack := _UI.Load_Component( 'SRAM' ) ;
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            _Stack.Connect_Input( Parent.Inputs[ Loop ] ) ; // Connect default RAM to code RAM
        end ;
        _Stack.Connect_Output( Parent ) ; // So it sends data back to us
    end ;
    Result := _Stack ;
end ;


function TSVM_CPU.Data_Cache : TComponent ;

var Loop : integer ;

begin
    if( _Data_Cache = nil ) then
    begin
        _Data_Cache := _UI.Load_Component( 'Blockram' ) ;
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            _Data_Cache.Connect_Input( Parent.Inputs[ Loop ] ) ; // Connect default RAM to code RAM
        end ;
        _Data_Cache.Connect_Output( Parent ) ; // So it sends data back to us
    end ;
    Result := _Data_Cache ;
end ;


function TSVM_CPU.Code : TComponent ;

var Loop : integer ;

begin
    if( _Code = nil ) then
    begin
        _Code := _UI.Load_Component( 'SVM_Code_RAM' ) ;
        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            _Code.Connect_Input( Parent.Inputs[ Loop ] ) ; // Connect default RAM to code RAM
        end ;
        _Code.Connect_Output( Parent ) ; // So it sends data back to us
    end ;
    Result := _Code ;
end ;


function TSVM_CPU.Data_Cache_Read( Address : int64 ; Size : integer ) : int64 ;

begin
    Size := Size * 8 ; // Convert from bytes to bits
    if( not Data_Cache.Read( Address, Size, IO_Type_Memory ) ) then
    begin
        Err( '??Cannot read from Data' ) ;
    end ;
    Result := Memory_Data_Latch ;
end ;


function TSVM_CPU.Stack_Read( Address : int64 ; Size : integer ) : int64 ;

begin
    Size := Size * 8 ; // Convert from bytes to bits
    _Stack.Read( Address, Size, IO_Type_Memory ) ;
    Result := Memory_Data_Latch ;
end ;


function TSVM_CPU._Read_Stack( Stack_Pointer : int64 ; Y : Integer ) : int64 ;

begin
    if( Stack_Pointer < 0 ) then
    begin
        Set_State( State_Stack_Underflow ) ;
        Result := 0 ;
        exit ;
    end ;
    if ( Stack_Pointer < 0 ) or ( Stack_Pointer > Stack_Size ) then
    begin
	    Set_State( State_Invalid_Memory_Reference ) ;
        _Read_Stack := 0 ;
	    exit ;
    end ;
    _Read_Stack := Stack_Read( Stack_Pointer, Y ) ;
    if( Y = 4 ) then // Longword read
    begin
        if( Result > 2147483647 ) then
        begin
            Result := Result or ( not 2147483647 ) ; // Sign extend
        end ;
    end ;
end ; { _Read_Stack }


procedure TSVM_CPU.Write_Stack( Stack_Pointer, X : int64 ; Y : Integer ) ;

var Offset : integer ;

begin
    if( Stack_Pointer < 0 ) then
    begin
	    Set_State( State_Invalid_Memory_Reference ) ;
	    exit ;
    end ;
    if( ( Stack_Pointer < 0 ) or ( Stack_Pointer > Stack_Size ) ) then
    begin
	    Set_State( State_Invalid_Memory_Reference ) ;
	    exit ;
    end ;
    if( Y > 8 ) then
    begin
        Y := 8 ;
    end ;
    Y := Y * 8 ; // Convert from bytes to bits
    Offset := 0 ;
    while( Y > 32 ) do
    begin
        _Stack.Write( Stack_Pointer + Offset, integer( X and $FFFFFFFF ), 32, IO_Type_Memory ) ;
        Y := Y - 32 ;
        X := X shr 32 ;
        Offset := Offset + 4 ;
    end ;
    if( Y > 0 ) then
    begin
        _Stack.Write( Stack_Pointer + Offset, integer( X and $FFFFFFFF ), Y, IO_Type_Memory ) ;
    end ;
end ; { Write_Stack }


function TSVM_CPU.Read_String_Data( B : int64 ) : String ; { Read pre-defined string }

begin
end ;


procedure TSVM_CPU.Read_File_Integer( X : Longint ; var E : Boolean ;
    F : _Files.PSFile ; DT : TData_Type ) ;

Label Loop ;

var A : string ;
    B : Char ;
    C, D : Integer ;
    IP : tUEC ;
    W : integer ;
    XInt : int64 ;

begin
    E := False ;
    A := '' ;
Loop:
    F^.Blockread( B, 1, W ) ;
    IP := F^.IO_Error ;
    if( IP.Code <> 0 ) then
    begin
	    Output_Text( strpas( Get_Error_Text( IP, -1 ) ) + CRLF ) ;
	    exit ;
    end ;
    if( B = ' ' ) then
        if( Length( A ) = 0 ) then
        begin
            goto Loop ; {Skip over leading spaces}
        end ;
    if ( ( B >= '0' ) and ( B <= '9' ) ) or ( ( Length( A ) = 0 ) and ( ( B = '-' ) or ( B = '+' ) ) ) then
    {A digit or a leading sign}
    begin
	    A := A + B ; {Accumulate number}
	    Goto Loop ;
    end ;
    if ( B <> ',' ) and ( B <> CR ) and ( B <> LF ) and ( B <> FF ) and
	    ( B <> '' ) and ( B <> ESC ) then {Not followed by a delimiter}
    begin
	    E := True ; {Bad format}
	    Exit ;
    end ;
    if( B = CR ) then
    begin
        F^.Blockread( B, 1, W ) ; {if CR, then read LF}
        IP := F^.IO_Error ;
        if( IP.Code <> 0 ) then
        begin
            Output_Text( strpas( Get_Error_Text( IP, -1 ) ) + CRLF ) ;
            exit ;
        end ;
    end ;
    if( B <> LF ) then
    begin
        F^.Seek( F^.Filepos - 1 ) ; {Backup one character if not LF}
        IP := F^.IO_Error ;
        if( IP.Code <> 0 ) then
        begin
            Output_Text( strpas( Get_Error_Text( IP, -1 ) ) + CRLF ) ;
            exit ;
        end ;
    end ;
    if( A[ 1 ] = '+' ) then
    begin
        A := Copy( A, 2, Length( A ) - 1 ) ;
    end ;
    {VAL function doesn't like leading plus sign}
    Val( A, XInt, D ) ; {Convert string to number}
    if( D = 0 ) then
    begin
	    E := True ;
    end else
    begin
        { Validate number }
        if( DT.Size < 8 ) then
        begin
            D := DT.Size * 8 ;
            if( DT.Signed ) then
            begin
                dec( D ) ;
            end ;
            if( X >= 0 ) then
            begin
                if( X >= XBit_Values[ D ] ) then
                begin
                    E := True ;
                    exit ;
                end ;
            end else
            begin
                if( -X >= XBit_Values[ D ] ) then
                begin
                    E := True ;
                    exit ;
                end ;
            end ;
        end ;
        Data_Write_Comp( C, X, DT.Size ) ;
    end ;
end ; { TSVM_CPU.Read_File_Integer }


procedure TSVM_CPU.Read_File_String( X : Longint ; F : _Files.PSFile ) ;
{INPUT a string}

Label Loop ;

var A : string ;
    B : Char ;
    D : integer ;
    IP : tUEC ;

begin
    A := '' ;
Loop:
    F^.Blockread( B, 1, D ) ;
    IP := F^.IO_Error ;
    if( IP.Code <> 0 ) then
    begin
	Output_Text( strpas( Get_Error_Text( IP, -1 ) ) + CRLF ) ;
	exit ;
    end ;
    if ( B <> CR ) and ( B <> LF ) and ( B <> '' ) and ( B <> '' ) and
	( B <> '' ) then { Not a delimiter }
    begin
	if( Length( A ) < 256 ) then
	begin
            A := A + B ;
	end ;
        goto Loop ;
    end ;
    if( B = CR ) then
    begin
	F^.Blockread( B, 1, D ) ; { if CR, then read LF }
    end ;
    if( B <> LF ) then
    begin
	F^.Seek( F^.Filepos - 1 ) ; {Backup one character if not LF}
    end ;
    _Data_Write( A, 0, X, Type_String ) ;
end ; { TSVM_CPU.Read_File_String }


function TSVM_CPU.Data_Read_Val( B : int64 ; G : Integer ;
    var _Result : integer ) : Longint ;
{ Read a value from address B, with length of G bytes.  Return in 4 byte longint }

var A : integer ;
    Extension : TSVM_Extension ;
    I : int64 ;

begin
    // Give extensions a chance to execute
    for A := 0 to Extensions.Count - 1 do
    begin
        Extension := TSVM_Extension( Extensions[ A ] ) ;
        if( Extension.Valid_Memory( B ) ) then
        begin
            Result := Extension.Data_Read_Val( B, G, _Result ) ;
            exit ;
        end ;
    end ;

    Result := 0 ;
    _Result := 0 ;
    if( B < Low_Data ) then if( B >= 0 ) then {Pre-defined variable}
    begin
        if( B <> 2 ) then { Not MAIN_WINDOW }
        begin
            Data_Read_Val := Read_Number_Data( B ) ;
            Exit ;
        end ;
    end ;
    if ( B and 1073741824 ) <> 0 then {Stack reference}
    begin
	    if( B > 0 ) then
        begin
            B := B and 1073741823 ; {Get actual offset}
        end ;
        B := B + _SF ; {Determine address on stack}
        Data_Read_Val := _Read_Stack( B, G ) ;
        exit ;
    end ;

    I := Data_Cache_Read( B, G ) ;
    move( I, Result, 4 ) ;
end ; { TSVM_CPU.Data_Read_Val }


function TSVM_CPU.Data_Read_n( B : int64 ; G : integer ;
    var _Result : integer ) : string ;

var A : integer ;
    Extension : TSVM_Extension ;
    D : integer ;
    P : PChar ;
    X : longint ;

begin
    // Give extensions a chance to execute
    for A := 0 to Extensions.Count - 1 do
    begin
        Extension := TSVM_Extension( Extensions[ A ] ) ;
        if( Extension.Valid_Memory( B ) ) then
        begin
            Extension.Data_Read_n( B, G, _Result, P ) ;
            Result := string( P ) ;
            exit ;
        end ;
    end ;

    setlength( Result, G ) ;
    D := 0 ;
    while( G > 4 ) do
    begin
        X := Data_Read_Val( B + D, 4, _Result ) ;
        if( _Result <> 0 ) then
        begin
            exit ;
        end ;
        move( X, PChar( Result )[ D ], 4 ) ;
        G := G - 4 ;
        D := D + 4 ;
    end ;
    if( G > 0 ) then
    begin
	    X := Data_Read_Val( B + D, G, _Result ) ;
	    move( X, PChar( Result )[ D ], 4 ) ;
    end ;
end ;


function TSVM_CPU.Data_Read_Comp( B : int64 ; G : Integer ;
    var _Result : integer ) : int64 ;

var A : integer ;
    Extension : TSVM_Extension ;
    S : string[ 7 ] ;
    X : int64 ;
    Y1 : longint ;

begin
    // Give extensions a chance to execute
    for A := 0 to Extensions.Count - 1 do
    begin
        Extension := TSVM_Extension( Extensions[ A ] ) ;
        if( Extension.Valid_Memory( B ) ) then
        begin
            Result := Extension.Data_Read_Comp( B, G, _Result ) ;
            exit ;
        end ;
    end ;

    if( G < 4 ) then
    begin
	    Data_Read_Comp := Data_Read_Val( B, G, _Result ) ;
    end else
    begin
	    Y1 := Data_Read_Val( B, 4, _Result ) ;
        move( Y1, S, 4 ) ;
        if( ( _Result = 0 ) and ( G > 4 ) ) then
        begin
            Y1 := Data_Read_Val( B + 4, G - 4, _Result ) ;
        end else
        begin
            Y1 := 0 ;
        end ;
        move( Y1, S[ 4 ], 4 ) ;
        move( S, X, 8 ) ;
        Data_Read_Comp := X ;
    end ;
end ; { Data_Read_Comp }


function TSVM_CPU.Read_String( A : int64 ; var _Result : integer ) : string ;
{ Return string whose header is at address A }

var Loop : integer ;
    Extension : TSVM_Extension ;
    D : Longint ;
    P : PChar ;
    S_P : ^Small_String ;

begin
    // Give extensions a chance to execute
    for Loop := 0 to Extensions.Count - 1 do
    begin
        Extension := TSVM_Extension( Extensions[ Loop ] ) ;
        if( Extension.Valid_Memory( A ) ) then
        begin
            P := Extension.Data_Read_Str( A, _Result ) ;
            Result := string( P ) ;
            exit ;
        end ;
    end ;

    _Result := 0 ;
    if( A < Low_Data ) and ( A >= 0 ) then {Pre-defined variable}
    begin
	    Result := Read_String_Data( A ) ;
	    Exit ;
    end ;
    if( ( A and 1073741824 ) <> 0 ) then { Stack reference }
    begin
	    if( A > 0 ) then
        begin
            A := A and 1073741823 ; {Get actual offset}
        end ;
        A := A + _SF ; {Determine address on stack}
        D := _Read_Stack( A, 4 ) ; {Get string header from stack}
        if( State = State_Invalid_Memory_Reference ) then
        begin
            exit ;
        end ;
	    Move( D, S_P, 4 ) ; {Get address from string header}
    end else
    begin
        S_P := pointer( Data_Cache_Read( A, 4 ) ) ; {Get pointer to string}
    end ; // if( ( A and 1073741824 ) <> 0 )
    if( S_P = nil ) then { Pointer is nil }
    begin
	    Result := '' ; { ...which means the string is null }
    end else
    begin
	    S_P := pointer( longint( S_P ) - 1 ) ;
	    Result := S_P^ ; { Otherwise return the string it points to }
    end ;
end ;


function TSVM_CPU._Read_Data( Address : int64 ; Y : Integer ) : int64 ;

begin
    if( Y > 8 ) then
    begin
        Y := 8 ;
    end ;
    Y := Y * 8 ; // Convert from bytes to bits
    Data_Cache.Read( Address, Y, IO_Type_Memory ) ;
    _Read_Data := Memory_Data_Latch ; {Return result}
end ;


function TSVM_CPU.Pop_Byte : Byte ;

begin
    SP := _SP - 1 ;
    Pop_Byte := _Read_Stack( _SP, 1 ) ;
end ;


function TSVM_CPU.Pop_Word : smallint ;

begin
    SP := _SP - 2 ;
{$R-}
    Pop_Word := _Read_Stack( _SP, 2 ) ;
{$R+}
end ;


function TSVM_CPU.Pop_Long : Longint ;

begin
    SP := _SP - 4 ;
{$R-}
    Pop_Long := _Read_Stack( _SP, 4 ) ;
{$R+}
end ;


function TSVM_CPU.Pop_64 : int64 ;

begin
    SP := _SP - 8 ;
    Result := _Read_Stack( _SP, 8 ) ;
end ;


function TSVM_CPU.Pop_3 : longint ;

begin
    SP := _SP - 3 ;
{$R-}
    Pop_3 := _Read_Stack( _SP, 3 ) ;
{$R+}
end ;


function TSVM_CPU.Pop_Comp( X : integer ) : int64 ;
{ Pop 1-8 bytes from stack }

var XX : int64 ;
    XS : string[ 7 ] ;
    Signed : boolean ;
    Dummy : integer ;

begin
    Signed := ( X > 127 ) ;
    X := X and 127 ;
    fillchar( XS, 8, 0 ) ;
    Dummy := X - 1 ;
    while( Dummy >= 0 ) do
    begin
        dec( _SP ) ;
        XS[ Dummy ] := chr( _Read_Stack( _SP, 1 ) ) ;
        dec( Dummy ) ;
    end ;
    if( Signed ) then { if reading signed }
    begin
        if( X < 8 ) then { Don't bother with full 8 bytes }
        begin
            if( XS[ 8 - X ] > #127 ) then { Negative }
            begin
                for Dummy := X to 7 do XS[ Dummy ] := #255 ; { Sign extend }
            end ;
        end ;
    end ;
    move( XS, XX, 8 ) ;
    Pop_Comp := XX ;
end ; { Pop_Comp }


procedure TSVM_CPU._Push( X : int64 ; Y : Integer ) ; { Push X (length Y) to stack }

begin
    if ( _SP + Y > 1073741823 ) or ( _SP = -1 ) then
    begin
	    _SP := -1 ; // Signal error
	    Exit ;
    end ;
    if( _SP + Y >= Stack_Size ) then
    begin
        Stack_Size := _SP + Y + 1 ;
    end ;
    Write_Stack( _SP, X, Y ) ;
    SP := _SP + Y ; // Increment stack pointer
end ;


procedure TSVM_CPU.Pushn( P : PChar ; Y : integer ) ;
{ Push Y bytes to stack from dereferenced pointer }

var Temp : integer ;

begin
    if( ( _SP + Y > 1073741823 ) or ( _SP = -1 ) ) then
    begin
        _SP := -1 ; { Signal error }
        exit ;
    end ;
    if( _SP + Y >= Stack_Size ) then
    begin
        Stack_Size := _SP + Y + 1 ;
    end ;
    Temp := Y ;
    while( Temp > 0 ) do
    begin
        Write_Stack( _SP, ord( P[ 0 ] ), 1 ) ;
        inc( P ) ;
        dec( Temp ) ;
        inc( _SP ) ;
    end ;
end ;


function TSVM_CPU.Read_Number_Data( B : int64 ) : Integer ; {Read pre-defined numeric}

Var F : Integer ;
    D : Longint ;

begin
    if( B = 0 ) then { MEMORY }
    begin
	    F := Pop_Word ;
	    D := Pop_Word ;
        D := D shl 16 ;
        D := D or F ;
        Read_Number_Data := PShortInt( D )^ ;
	    exit ;
    end ;

    Result := 0 ;
end ;


function TSVM_CPU.Read_Code( X : Integer ) : int64 ; { Read X bytes from RAM }

type CVT64 = record
                 case boolean of
                     false : ( Bytes : array[ 0..7 ] of byte ) ;
                     true : ( I64 : int64 ) ;
             end ;

var C : int64 ;
    E : boolean ;
    Size : longint ;

begin
    Result := 0 ;
    if( X > 8 ) then
    begin
        X := 8 ; // Maximum size we can read
    end else
    if( X = 0 ) then
    begin
        exit ;
    end ;
    Size := X * 8 ; // Convert from bytes to bits
    if( Code.Read( _IP, Size, IO_Type_Memory ) ) then
    begin
        Result := Memory_Data_Latch ;
    end else
    begin
        Result := Bus_Read( _IP, X, IO_Type_Memory, E ) ;
    end ;
    _IP := _IP + X ;
end ; { Read_Code }


function TSVM_CPU.Read_Comp( Stack_Pointer : int64 ; Y : integer ) : int64 ;

begin
    if( ( Stack_Pointer < 0 ) or ( Stack_Pointer > Stack_Size ) ) then
    begin
	    Set_State( State_Invalid_Memory_Reference ) ; { Invalid memory reference }
        Read_Comp := 0 ;
	    exit ;
    end ;
    Result := Stack_Read( Stack_Pointer, 1 ) ;
end ; { Read_Comp }


function TSVM_CPU.Read_Opcode : Integer ; // Read op-code from IP position

begin
    Code.Read( _IP, 8, IO_Type_Memory ) ;
    Result := Memory_Data_Latch ;
    IP := _IP + 1 ;
end ; // Read_OpCode


procedure TSVM_CPU.Read_Integer( X : Longint ; var E : Boolean ; Z : Integer ;
    DT : TData_Type ) ;
{ INPUT an integer.  X is the address to write the value to.  E is true if a
  data error.  Z is 128 if doing NCM Input.  DT is the data type to input. }

label Loop ;

var A : string ;
    B : Char ;
    C, D : Integer ;
    XInt : int64 ;

begin
    E := False ;
    A := '' ;
Loop:
    if( ( Buffer_Empty ) or ( Length( _KB_Buffer ) = 0 ) ) then
    begin
        Get_Input( Z ) ;
        Buffer_Empty := False ;
        _KB_Buffer := KB_Buffer + CR ; { Terminate with CR }
    end ;
    B := _KB_Buffer[ 1 ] ; { Get next character }
    _KB_Buffer := Copy( _Kb_Buffer, 2, Length( _Kb_Buffer ) ) ;
    { Strip first character }
    if( Length( _KB_Buffer ) = 0 ) then
    begin
        Buffer_Empty := True ;
    end ;
    if( B = ' ' ) then if Length( _KB_Buffer ) > 0 then if Length( A ) = 0 then
    begin
        goto Loop ; { Skip over leading spaces }
    end ;
    if( B = #0 ) then // Ignore nulls
    begin
        goto Loop ;
    end ;
    if(
        ( B >= '0' ) and ( B <= '9' ) )
        or
        ( ( Length( A ) = 0 ) and ( ( B = '-' ) or ( B = '+' ) )
      ) then { A digit or a leading sign }
    begin
        A := A + B ; { Accumulate number }
        goto Loop ;
    end ;
    if ( B <> ',' ) and ( B <> CR ) and ( B <> LF ) and ( B <> FF ) and
	( B <> '' ) and ( B <> ESC ) then { Not followed by a delimiter }
    begin
        E := True ;
        Exit
    end ;
    if( ( length( A ) > 0 ) and ( A[ 1 ] = '+' ) ) then
    begin
        A := Copy( A, 2, Length( A ) - 1 ) ;
        { VAL function doesn't like leading plus sign }
    end ;
    val( A, XInt, D ) ; { Convert string to number }
    if( D <> 0 ) then
    begin
	    E := True ;
    end else
    begin
        { Validate number }
        if( DT.Size < 8 ) then
        begin
            D := DT.Size * 8 ;
            if( DT.Signed ) then
            begin
                dec( D ) ;
            end ;
            if( X >= 0 ) then
            begin
                if( X >= XBit_Values[ D ] ) then
                begin
                    E := True ;
                    exit ;
                end ;
            end else
            begin
                if( -X >= XBit_Values[ D ] ) then
                begin
                    E := True ;
                    exit ;
                end ;
            end ;
        end ;
        while( DT.Size > 0 ) do
        begin
            C := 0 ;
            move( XInt, C, 2 ) ;
            XInt := XInt div 65536 ;
            if( DT.Size = 1 ) then
            begin
                _Data_Write( '', C, X, -1 ) ;
            end else
            begin
                _Data_Write( '', C, X, Type_Integer ) ;
            end ;
            inc( X, 2 ) ;
            dec( DT.Size, 2 ) ;
        end ;
    end ;
end ; { Read_Integer }


procedure TSVM_CPU.Input_String( X : int64 ; Y : Integer ) ;
{ INPUT to string at address X, with flags Y }

begin
    if Buffer_Empty then
    begin
        Get_Input( Y ) ;
    end ;
    _Data_Write( _KB_Buffer, 0, X, Type_String ) ;
    _KB_Buffer := '' ;
    Buffer_Empty := True ;
end ;


procedure TSVM_CPU.Get_Input( Flags : integer ) ;

begin
    if( Default_Input_Redirected and ( not Using_SMU ) ) then
    begin
        if( Readln_KB_Buffer = 0 ) then
        begin
            Output_Text( _KB_Buffer ) ; // Echo input to output
            if( ( Flags and 128 ) = 0 ) then // Not NCM
            begin
                Output_Text( CRLF ) ;
            end ;
            exit ;
        end ;
        Redirect_Input( nil ) ; // Cannot read file
    end ;

    if( not Using_SMU ) then
    begin
        Enable_SMU( False ) ;
    end ;
    try
        Window_Read_Input( 0, Flags ) ; { Get line }
    finally
        Enable_SMU( True ) ;
    end ;
end ;


procedure TSVM_CPU.Data_Write_Comp( XInt : int64 ; X : int64 ;
    Size : integer ) ; { Write CInt value to address X, Size bytes long. }

var O : integer ;
    CVT : record
	          C : array[ 0..4 ] of smallint ;
	      end ;

begin
    move( XInt, CVT.C, sizeof( XInt ) ) ;
    O := 0 ;
    while( Size > 0 ) do
    begin
        if( Size = 1 ) then
        begin
            _Data_Write( '', CVT.C[ O ], X, -1 ) ;
        end else
        begin
            _Data_Write( '', CVT.C[ O ], X, Type_Integer ) ;
        end ;
        inc( X, 2 ) ;
        inc( O ) ;
        dec( Size, 2 ) ;
    end ;
end ; { Data_Write_Comp }


function TSVM_CPU.Data_Read_Num( B : int64 ; var _Result : integer ) : Integer ;

begin
    _Result := 0 ;
    if ( B < Low_Data ) and ( B >= 0 ) then {Pre-defined variable}
    begin
	    Data_Read_Num := Read_Number_Data( B ) ;
	    Exit ;
    end ;
    if ( B and 1073741824 ) <> 0 then {Stack reference}
    begin
    	if B > 0 then
        begin
            B := B and 1073741823 ; {Get actual offset}
        end ;
        B := B + _SF ; {Determine address on stack}
        Data_Read_Num := _Read_Stack( B, 2 ) ; {Get value from stack}
        Exit ;
    end ;

	Data_Read_Num := _Read_Data( B, 2 ) ;
end ; { TSVM_Interpreter.Data_Read_Num }


function TSVM_CPU.Disassemble( Address : int64 ; Base, Size : longint ;
    Stream : TCOM_Stream ) : TUEC ;

var S : string ;

begin
    if( Base = 0 ) then
    begin
        Base := Default_Base ;
    end ;
    while( Size > 0 ) do
    begin
        if( length( S ) > 0 ) then
        begin
            S := S + CR ;
        end ;
        S := S + _Disassemble( Address, Base, Size ) ;
        dec( Size ) ;
    end ;
    Stream.Write( PChar( S )[ 0 ], length( S ) ) ;
end ; // TSVM_CPU.Disassemble


function Bit_Count( Bits : integer ) : integer ;

begin
    Result := 0 ;
    while( Bits <> 0 ) do
    begin
        if( ( Bits and 1 ) <> 0 ) then
        begin
            inc( Result ) ;
        end ;
        Bits := Bits shr 1 ;
    end ;
end ;


procedure TSVM_CPU.Execute( Single_Step, Into : boolean ) ;

    function Get_Stack( C : integer ) : longint ;

    var X : longint ;

    begin
        case ( C and 127 ) of
            1 : begin
                    X := Pop_Byte ;
                    if( ( C > 127 ) and ( X > $7F ) ) then { signed }
                    begin
                        X := X or ( not $FF ) ;
                    end ;
                end ;
            2 : begin
                    X := Pop_Word ;
                    if( ( C > 127 ) and ( X > $7FFF ) ) then { signed }
                    begin
                        X := X or ( not $FFFF )
                    end ;
                end ;
            3 : begin
                    X := Pop_3 ;
                    if( ( C > 127 ) and ( X > $7FFFFF ) ) then { signed }
                    begin
                        X := X or ( not $FFFFFF ) ;
                    end ;
                end ;
            4 : begin
                    X := Pop_Long ;
                end ;
        end ;
        Get_Stack := X ;
    end ;


    function Execute_SVM( A : integer ) : boolean ;

        procedure Call_To_Address( BR : int64 ) ;

        begin
            if( BR < ( Start_Block - 1 ) * 256 ) then
            begin
                if( Call( BR ) ) then
                begin { OTS routine }
                    _Halted := True ;
                end ;
            end else { User routines }
            begin
                _Push( IP, 8 ) ;
                _Push( _SF, 8 ) ;
                SF := _SP ;
                IP := Br ; { Jump to routine }
            end ;
        end ;


        procedure Determine_Data_Base ;

        var Image : TSVM_Image ;
            Loop : integer ;

        begin
            Data_Base := 0 ;
            Code_Base := 0 ;
            if( ( Images = nil ) or ( Images.Count = 0 ) ) then
            begin
                exit ;
            end ;
            if( IP < Base_Image_Code_Size ) then
            begin
                exit ;
            end ;
            for Loop := 0 to Images.Count - 1 do
            begin
                Image := TSVM_Image( Images[ Loop ] ) ;
                if(
                    ( IP >= Image.Code_Low )
                    and
                    ( IP <= Image.Code_High )
                  ) then { Cannot assume images are in address order }
                begin
                    Data_Base := Image.Data_Base ;
                    Code_Base := Image.Code_Low ;
                    exit ;
                end ;
            end ;
        end ;


        function Translate_Data_Address( Address : int64 ) : int64 ;

        begin
            Determine_Data_Base ;
            if( ( Address < 0 ) or ( Address > 2147483647 ) ) then
            begin
                Translate_Data_Address := Address ; { Don't translate stack address }
            end else
            begin
                Translate_Data_Address := Address + Data_Base ;
            end ;
        end ;


        function Read_Immediate_Word : SmallInt ; // Signed 2-byte integer

        begin
            Code.Read( _IP, 16, IO_Type_Memory ) ;
            move( Memory_Data_Latch, Result, 2 ) ;
            IP := _IP + 2 ;
        end ;


        function Read_Immediate_Word_Unsigned : int64 ;

        begin
            Code.Read( _IP, 16, IO_Type_Memory ) ;
            Result := Memory_Data_Latch ;
            IP := _IP + 2 ;
        end ;


        function Read_Immediate_Long : longint ;

        begin
            Code.Read( _IP, 32, IO_Type_Memory ) ;
            move( Memory_Data_Latch, Result, 4 ) ;
            IP := _IP + 4 ;
        end ;


        function Read_Immediate_Quad : int64 ;

        begin
            Code.Read( _IP, 64, IO_Type_Memory ) ;
            Result := Memory_Data_Latch ;
            IP := _IP + 8 ;
        end ;


    var Ar, B, CR, E, F : int64 ;
        Br : Pc_Type ;
        By_Ref : boolean ;
       	C, D : SmallInt ; { Various uses }
        DT, DT1 : TData_Type ;
	    Data_Type : TData_Type ;
        Extension : TSVM_Extension ;
        Loop : integer ;
        Native_Address : longint ;
	    P : PChar ;
        Parameter : integer ;
	    Res : integer ; { Data read _Result }
        S, S1 : string ;
	    Save_ESI, Save_EBX : integer ;
	    Stack_Address : longint ;
        Temp : string ;
        XInt, XInt1 : int64 ;
        XS : string[ 7 ] ;
        YYY : string ;
	    YY, Y_Y : Small_String ;

    label Load_Word, Next_Word, Do_Push ;

    begin { Do_Execute.Execute_SVM }
        Result := True ; {Normal return}
        case A of
             Op_Null : Exit ;
             Op_Exit :
                 begin
                     A := 0 ;
                     if( _SP > 0 ) then
                     begin
                         _SP := _SF ;
                         if( _SP > 0 ) then
                         begin
                            _SF := Pop_64 ;
                            IP := Pop_64 ;
                            if( _SP < 0 ) then { Stack underflow }
                            begin
                                Err( '??Stack underflow' ) ;
                                exit ;
                            end ;
                            if( IP <> 0 ) then { Return to 0 is the same as STOP }
                            begin
                                A := 1 ;
                            end ;
                         end ;
                     end ;
                     if( A = 0 ) then { Exiting from top level }
                     begin
                         _Halted := true ;
                         Execution := SVM_IT_End_Normal ;
                         Result := False ;
                         _Halted := True ;
                     end ;
                     if( Default_Output_Redirected ) then
                     begin
                         {$I-}
                         flush( Default_Output ) ;
                         {$I+}
                         IOResult ;
                     end ;
                 end ;
             Op_Skip_1..Op_Skip_6:
                 begin
                     IP := IP + A - Op_Skip_1 + 1 ;
                 end ;
             Op_Stop :
                 begin
                     _Halted := True ;
                     Execution := SVM_IT_End_Normal ;
                     if( Default_Output_Redirected ) then
                     begin
                         {$I-}
                         flush( Default_Output ) ;
                         {$I+}
                         IOResult ;
                     end ;
                 end ;
             Op_Goto_Long : {GOTO longword}
                 begin
                     IP := Read_Immediate_Long ;
		             Determine_Data_Base ;
		             IP := IP + Code_Base ;
                 end ;
            Op_Goto_Word : {GOTO word}
                 begin
                     IP := Read_Immediate_Word_Unsigned ;
                 end ;
            Op_Goto_Byte_Relative : {GOTO byte relative}
                 begin
                     Cr := Read_Opcode ;
                     if( Cr > 127 ) then
                     begin
                         Cr := Cr or ( not $FF ) ;
                     end ;
                     IP := IP + Cr ;
                 end ;
            Op_Goto_Long_Relative : {GOTO longword relative}
                 begin
                     CR := Read_Immediate_Long ;
                     IP := IP + CR ;
                 end ;
             Op_Goto_Quad :
                 begin
                     IP := Read_Immediate_Quad ;
		             Determine_Data_Base ;
		             IP := IP + Code_Base ;
                 end ;
             Op_Goto_Quad_Relative :
                 begin
                     CR := Read_Immediate_Quad ;
                     IP := IP + CR ;
                 end ;
             Op_Call_Long : { Call routine (longword) }
                 begin
                     AR := Read_Immediate_Long ; {Address of called routine}
                     if( AR < ( Start_Block - 1 ) * 256 ) then
                     begin
                         if( Call( AR ) ) then
                         begin { OTS routine }
                             _Halted := True ;
                         end ;
                     end else { User routines }
                     begin
                         Determine_Data_Base ;
                         AR := AR + Code_Base ;
			             Call_to_Address( AR ) ;
                     end ;
                 end ;
             Op_Call_Quad :
                 begin
                     AR := Read_Immediate_Quad ;
                     if( AR < ( Start_Block - 1 ) * 256 ) then
                     begin
                         if( Call( AR ) ) then
                         begin {OTS routine}
                             _Halted := True ;
                         end ;
                     end else { User routines }
                     begin
                         Determine_Data_Base ;
                         AR := AR + Code_Base ;
			             Call_to_Address( AR ) ;
                     end ;
                 end ;
             Op_Call_Quad_Relative :
                 begin
                     AR := Read_Immediate_Quad ;
                     Call_To_Address( _IP + AR ) ;
                 end ;
             Op_End :
                 begin
                     _Halted := True ;
                     Execution := SVM_IT_End_Normal ;
                     if( Default_Output_Redirected ) then
                     begin
                         {$I-}
                         flush( Default_Output ) ;
                         {$I+}
                         IOResult ;
                     end ;
                 end ;
             Op_If : {if...then}
                 begin
                     AR := Read_Immediate_Long ;
                     B := Pop_Word ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     if( B = 0 ) then
                     begin
                         IP := AR ; { Jump-if-false address }
                         Determine_Data_Base ;
                         IP := IP + Code_Base ;
                     end ;
                 end ;
            Op_If_Relative : {if...then (relative)}
                 begin
                     AR := Read_Immediate_Long ;
                     B := Pop_Word ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     if( B = 0 ) then
                     begin
                         IP := IP + AR ;
                         {Jump-if-false address}
                     end ;
                 end ;
             Op_If_Quad :
                 begin
                     AR := Read_Immediate_Quad ;
                     B := Pop_Word ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     if( B = 0 ) then
                     begin
                         IP := AR ; { Jump-if-false address }
                         Determine_Data_Base ;
                         IP := IP + Code_Base ;
                     end ;
                 end ;
             Op_If_Quad_Relative :
                 begin
                     AR := Read_Immediate_Quad ;
                     B := Pop_Word ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     if( B = 0 ) then
                     begin
                         IP := IP + AR ;
                         {Jump-if-false address}
                     end ;
                 end ;
             Op_Push_Long_Address : { Push immediate long address }
                 begin
                     AR := Translate_Data_Address( Read_Immediate_Long ) ;
                     _Push( AR, 4 ) ;
                 end ;
             Op_Push_Direct_Long : { Push word to direct (longword address) }
                 begin
                     Ar := Translate_Data_Address( Read_Immediate_Long ) ;
		             { Get address to read from }
                     _Push( Data_Read_Num( AR, Res ), 2 ) ;
                     if( Res <> 0 ) then
                     begin
                        Err( '??Data address out of range' ) ;
                     end ;
                 end ;
             Op_Pop_Direct_Long : { Pop word to direct long address }
                 begin
                     Ar := Translate_Data_Address( Read_Immediate_Long ) ;
		     { Get address to write to }
                     C := Pop_Word ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Data_Write( '', C, Ar, Type_Integer )
                 end ;
            Op_Push_Word : { Push immediate word }
                 begin
                     _Push( Read_Immediate_Word, 2 ) ;
                 end ;
            Op_Zero_Temp0 : Temp0 := '' ; { Zero Temp0 }
            Op_Push_n_Immediate :
                begin
                    C := Read_Opcode ; { Get length }
                    for D := 1 To C do
                    begin
                        _Push( Read_Opcode, 1 ) ;
                    end ;
                end ;
            Op_Concat_Stack_To_Temp0 : {Concatenate top of stack to Temp0  * NOT USED YET *}
                 begin
                     Ar := Pop_Long ; {Get address from stack}
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     Temp0 := Temp0 + Read_String( AR, Res ) ;
                     if( Res <> 0 ) then
                     begin
                        Err( '??Data address out of range' ) ;
                     end ;
                 end ;
            Op_Concat_Literal_To_Temp0 : {Concatenate literal to Temp0}
                 begin
                     C := Read_Opcode ; {Get length}
                     if( C > 0 ) then For D := 1 To C do
                     begin
                         Temp0 := Temp0 + Chr( Read_Opcode ) ;
                     end ;
                     {Build string}
                 end ;
            Op_Store_Temp0_Direct : {Store Temp0 direct}
                 begin
		             AR := Translate_Data_Address( Read_Immediate_Long ) ;
                     _Data_Write( Temp0, 0, AR, Type_String ) ;
                 end ;
            Op_Negate_Word : {Negate top of stack - 2 byte integer }
                 begin
                     C := Pop_Word ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Push( -C, 2 ) ;
                 end ;
            Op_Add_Word : {Add 2 byte integer }
                 begin
                    C := Pop_Word + Pop_Word ;
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                    _Push( C, 2 ) ;
                 end ;
            Op_Add_Long : {Add 4 byte integer }
                 begin
                    Cr := Pop_Long + Pop_Long ;
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                    _Push( Cr, 4 ) ;
                 end ;
            Op_Subtract_Word : {Subtract 2 byte integer }
                 begin
                     C := Pop_Word ;
                     C := Pop_Word - C ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                    _Push( C, 2 ) ;
                 end ;
            Op_Subtract_Long : { Subtract 4 byte integer }
                begin
                    Cr := Pop_Long ;
                    CR := Pop_Long - CR ;
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                    _Push( Cr, 4 ) ;
                 end ;
            Op_Multiply_Word : {Multiply two byte integer }
                 begin
                    C := Pop_Word * Pop_Word ;
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                    _Push( C, 2 ) ;
                end ;
            Op_Multiply_Long : { Multiply 4 byte integer }
                begin
                    Cr := Pop_Long * Pop_Long ;
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                    _Push( Cr, 4 ) ;
                 end ;
            Op_Divide_Word : {Divide 2 byte integer }
                 begin
                     C := Pop_Word ;
                     C := Pop_Word div C ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Push( C, 2 ) ;
                 end ;
            Op_Divide_Long : { Divide 4 byte integer }
                begin
                    Cr := Pop_Long ;
                    CR := Pop_Long div CR ;
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                    _Push( Cr, 4 ) ;
                 end ;
            Op_Integer_Compare_NE..Op_Integer_Compare_GE : { Integer comparisons }
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    AR := Get_Stack( C and 127 ) ;
                                    CR := Get_Stack( C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) ;
                                     XInt1 := Pop_Comp( 4 ) ;
                                 end else
                                 begin
                                     AR := Pop_Long ;
                                     CR := Pop_Long ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := Pop_Comp( C ) ;
                                    XInt1 := Pop_Comp( C ) ;
                                end ;
                     end ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     if( ( C < 4 ) or ( ( C = 4 ) and ( C > 127 ) ) ) then
                     begin
                         case A Of
                             Op_Integer_Compare_NE : if Cr <> Ar then D := -1 else D := 0 ;
                             Op_Integer_Compare_EQ : if Cr = Ar then D := -1 else D := 0 ;
                             Op_Integer_Compare_LT : if Cr < Ar then D := -1 else D := 0 ;
                             Op_Integer_Compare_GT : if Cr > Ar then D := -1 else D := 0 ;
                             Op_Integer_Compare_LE : if Cr <= Ar then D := -1 else D := 0 ;
                             Op_Integer_Compare_GE : if Cr >= Ar then D := -1 else D := 0
                         end ;
                     end else
                     begin
                         case A Of
                             Op_Integer_Compare_NE : if XInt1 <> XInt then D := -1 else D := 0 ;
                             Op_Integer_Compare_EQ : if XInt1 = XInt then D := -1 else D := 0 ;
                             Op_Integer_Compare_LT : if XInt1 < XInt then D := -1 else D := 0 ;
                             Op_Integer_Compare_GT : if XInt1 > XInt then D := -1 else D := 0 ;
                             Op_Integer_Compare_LE : if XInt1 <= XInt then D := -1 else D := 0 ;
                             Op_Integer_Compare_GE : if XInt1 >= XInt then D := -1 else D := 0
                         end ;
                     end ;
                     _Push( D, 2 ) ; {Store _Result}
                 end ;
            Op_String_Compare_NE..Op_String_Compare_GE : {string comparisons}
                 begin
                     Ar := Pop_Long ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     S := Read_String( Ar, Res ) ;
                     if( Res <> 0 ) then
                     begin
                         Err( '??Data address out of range' ) ;
                     end ;
                     Cr := Pop_Long ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     S1 := Read_String( Cr, Res ) ;
                     if( Res <> 0 ) then
                     begin
                         Err( '??Data address out of range' ) ;
                     end ;
                     {Get two strings whose addresses are on stack}
                     D := 0 ;
                     case A Of
                         Op_String_Compare_NE : if S1 <> S then D := -1 ;
                         Op_String_Compare_EQ : if S1 = S then D := -1 ;
                         Op_String_Compare_LT : if S1 < S then D := -1 ;
                         Op_String_Compare_GT : if S1 > S then D := -1 ;
                         Op_String_Compare_LE : if S1 <= S then D := -1 ;
                         Op_String_Compare_GE : if S1 >= S then D := -1
                     end ;
                     _Push( D, 2 ) ; {Store _Result}
                 end ;
            Op_Convert_Number : { Convert a number (promote/demote) }
                 begin { NOTE: For now, this converts only integers }
                    AR := Pop_Long ; { Get desired type }
                    CR := Pop_Long ; { Get actual type }
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;

                    { Fill data type records... }
                    DT.ID := Data_Read_Val( CR, 2, Res ) ;
                    DT.Signed := Data_Read_Val( CR + 2, sizeof( boolean ), Res ) <> 0 ;
                    DT.Size := Data_Read_Val( CR + 2 + sizeof( boolean ), 2, Res ) ;
                    DT1.ID := Data_Read_Val( AR, 2, Res ) ;
                    DT1.Signed := Data_Read_Val( AR + 2, sizeof( boolean ), Res ) <> 0 ;
                    DT1.Size := Data_Read_Val( AR + 2 + sizeof( boolean ), 2, Res ) ;

                    { do the conversion }
                    if( DT.Size <> DT1.Size ) then
                    begin
                        XInt := Pop_Comp( DT.Size ) ;
                        if( _SP < 0 ) then { Stack underflow }
                        begin
                            Err( '??Stack underflow' ) ;
                            exit ;
                        end ;
                        if(
                            ( DT1.Size > DT.Size )
                            and
                            ( XInt < 0 )
                            and
                            ( DT.Signed )
                            and
                            ( DT1.Signed )
                          ) then
                        begin { Sign extend }
                            XInt1 := 255 ;
                            for C := 1 to DT1.Size - DT.Size do
                            begin
                                XInt1 := XInt1 * 256 + 255 ;
                            end ;
                            for C := 1 to DT1.Size do
                            begin
                                XInt1 := XInt1 * 256 ;
                            end ;
                            XInt := XInt1 + XInt ;
                        end ;
                        move( XInt, XS, 8 ) ;
                        for C := 0 to DT1.Size - 1 do
                        begin
                            _Push( ord( XS[ C ] ), 1 ) ;
                        end ;
                    end ;
                 end ;
            Op_Create_String_From_Temp0 : {Put TEMP0 to string whose address is on stack * NOT USED *}
                 begin
                     AR := Pop_Long ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Data_Write( Temp0, 0, AR, Type_String ) ;
                 end ;
            Op_Push_Immediate_Longword_Type_Address :
                begin
                    AR := Translate_Data_Address( Read_Code( 4 ) ) ;
	                _Push( AR, 4 ) ;
                end ;
            Op_Push_Direct_Word : {Push word direct (word)}
                 begin
                     _Push( Data_Read_Num( Read_Immediate_Word, Res ), 2 ) ;
                     if( Res <> 0 ) then
                     begin
                        Err( '??Data address out of range' ) ;
                     end ;
                 end ;
            Op_Pop_Direct_Word : {Pop word direct to word address}
                 begin
                     B := Read_Immediate_Word ;
                     C := Translate_Data_Address( Pop_Word ) ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Data_Write( '', C, B, Type_Integer ) ; {Write it out}
                 end ;
            Op_Call_Word : { Call routine (word) }
                 begin
                     B := Read_Immediate_Word ;
                     if( B < ( Start_Block - 1 ) * 256 ) then { Special routine }
                     begin
                        if( Call( B ) ) then
                        begin
                            _Halted := True ;
                        end ;
                     end else
                     begin
                         Call_To_Address( B ) ;
                    end ;
                 end ;
            Op_NOT : {NOT}
                 begin
                     B := Pop_Word ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     if B = 0 then
                         B := -1
                     else
                         B := 0 ;
                     _Push( B, 2 ) ;
                 end ;
            Op_AND..Op_XNOR : {Logical functions}
                 begin
                     C := Pop_Word ;
                     D := Pop_Word ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     case A Of
                          Op_AND..Op_NAND : {and/NAND}
                                   B := D and C ;
                          Op_OR..Op_NOR : {or/NOR}
                                   B := D or C ;
                          Op_XOR..Op_XNOR : {XOR/XNOR}
                                   if ( D = 0 ) or ( C = 0 ) and ( D <> C ) then
                                       B := -1 else B := 0
                     end ;
                     if B <> 0 then B := -1 ;
                     if ( A and 1 ) = 0 then if B = 0 then B := -1 else B := 0 ; {NOT}
                     _Push( B, 2 ) ;
                 end ;
            Op_Push_Immediate_Byte : {Push immediate byte}
                 _Push( Read_Opcode, 1 ) ;
            Op_Concat_Indirect : {Concatenate strings addressed by stack}
                 begin
                     E := Pop_Long ; { Get Result address }
                     Ar := Pop_Long ; { Second string to concatenate }
                     Cr := Pop_Long ; { First string to concatenate }
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Push( E, 4 ) ; { Leave Result address on the stack }
                     Temp := Read_String( AR, Res ) ;
                     if( Res <> 0 ) then
                     begin
                         Err( '??Data address out of range' ) ;
                     end ;
                     Temp := Read_String( CR, Res ) + Temp ; // Concatenate
                     if( Res <> 0 ) then
                     begin
                         Err( '??Data address out of range' ) ;
                     end ;
                     _Data_Write( Temp, 0, E, Type_String ) ;
                     { Write result to Result address }
                 end ;
            Op_Pop :
                begin
                    Dec( _SP ) ; {Pop Stack}
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                end ;
            Op_Call_Byte : { Call routine (byte) - this will only call built-in routines }
                 if( Call( Read_Opcode ) ) then
                 begin
                    _Halted := True ;
                 end ;
            Op_Copy_String_Indirect : {Copy strings whose addresses are on stack}
                 begin
                     Ar := Pop_Long ; {Destination address}
                     Cr := Pop_Long ; {Source address}
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Data_Write( Read_String( CR, Res ), 0, Ar, Type_String ) ;
                     {Copy}
                     if( Res <> 0 ) then
                     begin
                        Err( '??Data address out of range' ) ;
                     end ;
                 end ;
            Op_Push_n_Direct_Word : {Push n bytes from word direct (word)}
                 begin
                     C := Read_Opcode ; {Number of bytes to push}
                     B := Translate_Data_Address( Read_Immediate_Word ) ;
                     YY := Data_Read_n( B, C, Res ) ;
                     P := PChar( @YY ) ;
                     inc( P ) ;
                     Pushn( P, C ) ;
                     if( Res <> 0 ) then
                     begin
                         Err( '??Data address out of range' ) ;
                     end ;
                 end ;
            Op_Pop_n_Direct_Word : { Pop n bytes direct to word address }
                 begin
                     C := Read_Opcode ; {Number of bytes to pop}
                     B := Translate_Data_Address( Read_Immediate_Word ) ;

                     _SP := _SP - C ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         exit ;
                     end ;
                     AR := _SP ;
                     while( C > 0 ) do
                     begin
                         CR := C ;
                         if( CR > 4 ) then
                         begin
                             CR := 4 ;
                         end ;
                         _Data_Write( '', _Read_Stack( AR, CR ), B, -CR ) ;
                         { Write it out }
                         AR := AR + 4 ; { Source for next bytes }
                         C := C - 4 ; { Number of bytes left to pop }
			             B := B + 4 ; { Destination for next bytes }
                     end ;
                 end ;
             Op_Push_n_Direct_Long : {Push n bytes from direct (longword address)}
                 begin
                     B := Read_Opcode ; {Number of bytes to push}
                     Ar := Translate_Data_Address( Read_Code( 4 ) ) ;
                     { Get address to read from }
                     YY := Data_Read_n( AR, B, Res ) ;
                     P := PChar( @YY ) ;
                     inc( P ) ;
                     Pushn( P, B ) ;
                     if( Res <> 0 ) then
                     begin
                         Err( '??Data address out of range' ) ;
                     end ;
                 end ;
            Op_Pop_n_Direct_Long : {Pop n bytes direct (longword address)}
                 begin
                     C := Read_Opcode ; {Number of bytes to pop}
                     D := Translate_Data_Address( Read_Code( 4 ) ) ;
		             { Get address to write to }
                     _SP := _SP - C ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         exit ;
                     end ;
                     AR := _SP ;
                     while( C > 0 ) do
                     begin
                         CR := C ;
                         if( CR > 4 ) then
                         begin
                             CR := 4 ;
                         end ;
                         _Data_Write( '', _Read_Stack( AR, CR ), D, -CR ) ;
                         { Write it out }
                         AR := AR + 4 ; { Source address }
                         C := C - 4 ; { Amount left to pop }
                         D := D + 4 ; { Destination address }
                     end ;
                 end ;
            Op_Push_Immediate_Long : { Push immediate longword constant }
                 begin
                    _Push( Read_Code( 4 ), 4 ) ;
                 end ;
            Op_Push_Zeroes : {Push zeroes to stack  * NOT USED YET * }
                 begin
                     B := Read_Immediate_Word ;
                     while( B > 3 ) do
                     begin
                        _Push( 0, 4 ) ;
                        B := B - 4 ;
                     end ;
                     while B > 0 do
                     begin
                        _Push( 0, 1 ) ;
                        Dec( B ) ;
                     end ;
                 end ;
            Op_Pop_n : {Pop n bytes from stack}
                 begin
                    B := Read_Immediate_Word ;
                    Dec( _SP, B ) ; {Pop Stack}
                 end ;
            Op_Dereference : {Dereference top of stack}
                 begin
                     AR := Pop_Long ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Push( Data_Read_Val( AR, 4, Res ), 4 ) ;
                     if( Res <> 0 ) then
                     begin
                         Err( '??Data address out of range' ) ;
                     end ;
                 end ;
            Op_Zero_String : { Zero string on top of stack }
                begin
                     AR := Pop_Long ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                    _Data_Write( '', 0, AR, Type_String ) ;
                end ;
            Op_Promote_Word_To_Long : {Promote word to long}
                begin
                    Ar := Pop_Word ;
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                   _Push( Ar, 4 ) ;
                end ;
            Op_Range_Check : {Range check}
                begin
                    B := Pop_Word ; {High range}
                    C := Pop_Word ; {Low range}
                    F := Pop_Word ; {Subscript}
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                    if( ( F < C ) or ( F > B ) ) then
                    begin
                        Err( '?Subscript out of range' ) ;
                    end else _Push( F, 2 ) ; {Write subscript back to stack}
                 end ;
            Op_Pop_To_Stack : { Pop word to stack indirect longword address }
                 begin
                     C := Pop_Word ;
                     Ar := Pop_Long ;
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Data_Write( '', C, Ar, Type_Integer )
                 end ;
            Op_Pop_n_To_Stack_Indirect_Long : {Pop n bytes to stack indirect longword address}
                 begin
                     C := Read_Opcode ; {Number of bytes to pop}
                     if( C > 127 ) then
                     begin
                        C := C or ( not $FF ) ; {Sign extend}
                     end ;
                     if( _SP - abs( C ) - 4 < 0 ) then { Stack underflow }
                     begin
                         _SP := _SP - Abs( C ) - 4 ;
                         exit ;
                     end ;
                     Ar := _Read_Stack( _SP - Abs( C ) - 4, 4 ) ;
                     {Get address to write to}
                     if( State_Invalid_Memory_Reference = 1 ) then
                     begin
                         exit ;
                     end ;
                     _SP := _SP - Abs( C ) ;
                     _Data_Write( '', _Read_Stack( _SP, Abs( C ) ), Ar, - C ) ;
                     _SP := _SP - 4 ; {Remove destination address}
                 end ;
            Op_Reverse_Copy_Strings : {Copy strings whose addresses are on stack}
                 begin
                     Cr := Pop_Long ; {Source address}
                     Ar := Pop_Long ; {Destination address}
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     _Data_Write( Read_String( CR, Res ), 0, Ar, Type_String ) ; {Copy}
                     if( Res <> 0 ) then
                     begin
                        Err( '??Data address out of range' ) ;
                     end ;
                 end ;
            Op_Call_Long_Relative : { Call routine (longword relative) }
                 begin
                     AR := Read_Immediate_Long ; { Address of called routine }
                     Call_To_Address( _IP + AR ) ; { Jump to routine }
                 end ;
            Op_Call_Long_Indirect:
                begin
                    Cr := Pop_Long ; { Get address }
                    if( _SP < 0 ) then { Stack underflow }
                    begin
                        Err( '??Stack underflow' ) ;
                        exit ;
                    end ;
                    Call_To_Address( CR ) ;
                end ;
           Op_Memory_Compare_NE..Op_Memory_Compare_GE :
                begin
                     C := Read_Immediate_Word_Unsigned ;
                     CR := Pop_Long ; { Source address }
                     AR := Pop_Long ; { Destination address }
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     if( A = Op_Memory_Compare_EQ ) then
                     begin
                        D := -1 ; { Assume equal unless we find otherwise }
                     end else
                     begin
                        D := 0 ; { Assume failure unless we find otherwise }
                     end ;
                     while( C > 0 ) do
                     begin
                         case A of
                             Op_Memory_Compare_NE:
                                 if( Data_Read_Val( AR, 1, Res ) <> Data_Read_Val( CR, 1, Res ) ) then
                                 begin
                                     D := -1 ;
                                     break ;
                                 end ;
                             Op_Memory_Compare_EQ:
                                 if( Data_Read_Val( AR, 1, Res ) <> Data_Read_Val( CR, 1, Res ) ) then
                                 begin
                                     D := 0 ;
                                     break ;
                                 end ;
                             Op_Memory_Compare_LT:
                                 if( Data_Read_Val( AR, 1, Res ) < Data_Read_Val( CR, 1, Res ) ) then
                                 begin
                                     D := -1 ;
                                     break ;
                                 end ;
                             Op_Memory_Compare_GT:
                                 if( Data_Read_Val( AR, 1, Res ) > Data_Read_Val( CR, 1, Res ) ) then
                                 begin
                                     D := -1 ;
                                     break ;
                                 end ;
                             Op_Memory_Compare_LE:
                                 if( Data_Read_Val( AR, 1, Res ) <= Data_Read_Val( CR, 1, Res ) ) then
                                 begin
                                     D := -1 ;
                                     break ;
                                 end ;
                             Op_Memory_Compare_GE:
                                 if( Data_Read_Val( AR, 1, Res ) >= Data_Read_Val( CR, 1, Res ) ) then
                                 begin
                                     D := -1 ;
                                     break ;
                                 end ;
                         end ;
                         inc( CR ) ;
                         inc( AR ) ;
                         dec( C ) ;
                     end ;
                     _Push( D, 2 ) ; {Store _Result}
                end ;
           Op_Copy_n_Indirect: { Copy n bytes indirect to indirect }
                 begin
                     C := Read_Immediate_Word_Unsigned ;
                     CR := Pop_Long ; { Source address }
                     AR := Pop_Long ; { Destination address }
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     while( C > 0 ) do
                     begin
                         _Data_Write( '', Data_Read_Val( CR, 1, Res ), AR, -1 ) ;
                         inc( CR ) ;
                         inc( AR ) ;
                         dec( C ) ;
                     end ;
                 end ;
           Op_Call_Word_Relative : { Call routine (word relative) }
                 begin
                     C := Read_Immediate_Word ;
                     Call_To_Address( _IP + C ) ;
                 end ;
           Op_Goto_Word_Relative : {GOTO word relative}
                 begin
                     C := Read_Immediate_Word ;
                     CR := C ;
                     IP := _IP + Cr ;
                 end ;
           Op_Call_Byte_Relative : { Call routine (byte relative) }
                 begin
                     Cr := Read_Opcode ; { Offset address of called routine }
                     if( Cr > 127 ) then
                     begin
                         Cr := Cr or ( not $FF ) ;
                     end ;
                     Call_To_Address( _IP + CR ) ;
                 end ;
           Op_Dereference_Integer : {Dereference integer from top of stack}
                 begin
                     CR := Pop_Long ; { Get address }
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     C := Read_OpCode ; { Get integer size }
                     XInt := Data_Read_Comp( CR, C, Res ) ;
                     Pushn( @XInt, C ) ; { Push integer }
                     if( Res <> 0 ) then
                     begin
                        Err( '??Data address out of range' ) ;
                     end ;
                 end ;
           Op_Call_Vector : { Call through a vector }
                 begin
                     B := Pop_Word ; {Get vector number from stack}
                     if( _SP < 0 ) then { Stack underflow }
                     begin
                         Err( '??Stack underflow' ) ;
                         exit ;
                     end ;
                     Err( '?Call through non-existant vector ' + Num1( B ) ) ;
                 end ;
           Op_Negate_Stack : {Negate top of stack - integer }
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    CR := Get_Stack( C and 127 ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    _Push( -CR, C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     move( XInt, CR, 4 ) ;
                                     _Push( CR, 4 ) ;
                                 end else
                                 begin
                                     C := Pop_Long ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( -C, 4 ) ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := -Pop_Comp( C ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
				                    Pushn( @XInt, C and 127 ) ;
                                end ;
                     end ;
                 end ;
           Op_Add_Integer : {Add}
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    CR := Get_Stack( C and 127 ) + Get_Stack( C and 127 ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    _Push( CR, C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) ;
                                     XInt := XInt + Pop_Comp( 4 ) ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     move( XInt, CR, 4 ) ;
                                     _Push( CR, 4 ) ;
                                 end else
                                 begin
                                     CR := Pop_Long + Pop_Long ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( CR, 4 ) ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := Pop_Comp( C ) ;
                                    XInt := XInt + Pop_Comp( C and 127 ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    Pushn( @XInt, C ) ;
                                end ;
                     end ;
                 end ;
           Op_Subtract_Integer : {Subtract}
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    CR := Get_Stack( C and 127 ) ;
                                    CR := Get_Stack( C and 127 ) - CR ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    _Push( CR, C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) ;
                                     XInt := Pop_Comp( 4 ) - XInt ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     move( XInt, CR, 4 ) ;
                                     _Push( CR, 4 ) ;
                                 end else
                                 begin
                                     CR := Pop_Long ;
                                     CR := Pop_Long - CR ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( CR, 4 ) ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := Pop_Comp( C ) ;
                                    XInt := Pop_Comp( C ) - XInt ;
				                    Pushn( @XInt, C and 127 ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                end ;
                     end ;
                 end ;
           Op_Multiply_Integer : {Multiply}
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    CR := Get_Stack( C and 127 ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    _Push( CR * Get_Stack( C and 127 ), C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) ;
                                     XInt := XInt * Pop_Comp( 4 ) ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     move( XInt, CR, 4 ) ;
                                     _Push( CR, 4 ) ;
                                 end else
                                 begin
                                     CR := Pop_Long ;
                                     CR := Pop_Long * CR ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( CR, 4 ) ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := Pop_Comp( C ) ;
                                    XInt := XInt * Pop_Comp( C ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
				                    Pushn( @XInt, C and 127 ) ;
                                end ;
                     end ;
                 end ;
           Op_Divide_Integer : {Divide}
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    CR := Get_Stack( C and 127 ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    _Push( Get_Stack( C and 127 ) div CR, C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) ;
                                     XInt := Pop_Comp( 4 ) div XInt ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     move( XInt, CR, 4 ) ;
                                     _Push( CR, 4 ) ;
                                 end else
                                 begin
                                     CR := Pop_Long ;
                                     CR := Pop_Long div CR ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( CR, 4 ) ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := Pop_Comp( C ) ;
                                    XInt := Pop_Comp( C ) div XInt ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
				                    Pushn( @XInt, C and 127 ) ;
                                end ;
                     end ;
                 end ;
           Op_Percent_Integer :
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    CR := Get_Stack( C and 127 ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    _Push( CR div 100, C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) div 100 ;
                                     move( XInt, CR, 4 ) ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( CR, 4 ) ;
                                 end else
                                 begin
                                     CR := Pop_Long ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( CR div 100, 4 ) ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := Pop_Comp( C ) div 100 ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
				                    Pushn( @XInt, C and 127 ) ;
                                end ;
                     end ;
                 end ;
           Op_Exponent_Integer :
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    CR := Get_Stack( C and 127 ) ;
                                    CR := trunc( Power( Get_Stack( C and 127 ), CR, Res ) ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    _Push( CR, C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) ;
                                     XInt := trunc( int( Power( Pop_Comp( 4 ), XInt, Res ) ) ) ;
                                     move( XInt, CR, 4 ) ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( CR, 4 ) ;
                                 end else
                                 begin
                                     CR := Pop_Long ;
                                     CR := trunc( Power( Pop_Long, CR, Res ) ) ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     _Push( CR, 4 ) ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := Pop_Comp( C ) ;
                                    XInt := trunc( int( Power( Pop_Comp( C ), XInt, Res ) ) ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
				                    Pushn( @XInt, C and 127 ) ;
                                end ;
                     end ;
                     case Res of
                        7 : begin
                                Err( '?Integer _Result too large' ) ;
                            end ;
                       11 : begin
                                Err( '?Illegal argument in LOG' ) ;
                            end ;
                       12 : begin
                                Err( '?Argument too large in exponent' ) ;
                            end ;
                     end ;
                 end ;
           Op_Factorial_Integer :
                 begin
                     C := Read_OpCode ;
                     case ( C and 127 ) of
                         1..3 : begin
                                    CR := Get_Stack( C and 127 ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    AR := CR - 1 ;
                                    while( AR > 1 ) do
                                    begin
                                        CR := CR * AR ;
                                        dec( AR ) ;
                                    end ;
                                    _Push( CR, C and 127 ) ;
                                end ;
                         4 : begin
                                 if( C < 128 ) then { unsigned }
                                 begin
                                     XInt := Pop_Comp( 4 ) ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     XInt1 := XInt - 1 ;
                                     while( XInt1 > 1 ) do
                                     begin
                                         XInt := XInt * XInt1 ;
                                         XInt1 := XInt1 - 1 ;
                                     end ;
                                     move( XInt, CR, 4 ) ;
                                     _Push( CR, 4 ) ;
                                 end else
                                 begin
                                     CR := Pop_Long ;
                                     if( _SP < 0 ) then { Stack underflow }
                                     begin
                                         Err( '??Stack underflow' ) ;
                                         exit ;
                                     end ;
                                     AR := CR - 1 ;
                                     while( AR > 1 ) do
                                     begin
                                         CR := CR * AR ;
                                         dec( AR ) ;
                                     end ;
                                     _Push( CR, 4 ) ;
                                 end ;
                             end ;
                         5..8 : begin
                                    XInt := Pop_Comp( C ) ;
                                    if( _SP < 0 ) then { Stack underflow }
                                    begin
                                        Err( '??Stack underflow' ) ;
                                        exit ;
                                    end ;
                                    XInt1 := XInt - 1 ;
                                    while( XInt1 > 1 ) do
                                    begin
                                        XInt := XInt * XInt1 ;
                                        XInt1 := XInt1 - 1 ;
                                    end ;
				                    Pushn( @XInt, C and 127 ) ;
                                end ;
                     end ;
                 end ;
	         Op_Call_Dynamic:
                begin
                    CR := Translate_Data_Address( Read_Immediate_Long ) ;
                    int64( BR ) := CR ;
                    AR := Data_Read_Val( CR, 4, Res ) ;
                    if( AR = 0 ) then { Not called before }
                    begin
                        { Get routine name... }
                        C := Data_Read_Val( CR + 4, 1, Res ) ;
                        SetLength( YY, C ) ;
                        for D := 1 to C do
                        begin
                            YY[ D ] := chr( Data_Read_Val( int64( BR ) + 4 + D, 1, Res ) ) ;
                        end ;

                        { Get library name... }
                        int64( BR ) := int64( BR ) + 5 + C ;
                        C := Data_Read_Val( int64( BR ), 1, Res ) ;
                        Set_Length( YYY, C ) ;
                        for D := 1 to C do
                        begin
                            YYY[ D ] := chr( Data_Read_Val( int64( BR ) + D, 1, Res ) ) ;
                        end ;

                        { Load the image... }
                        if( _Observer <> nil ) then
                        begin
                            P := _Observer.SSL_Load( PChar( YYY ) ) ;
                            if( P = nil ) then
                            begin
                                Output_Text( 'Error loading ' + YYY + CRLF ) ;
                                _Halted := True ;
                                exit ;
                            end ;
                            YYY := string( P ) ;
                        end ;
                        Y_Y := _Load_Image( YYY, E ) ;
                        if( length( Y_Y ) > 0 ) then
                        begin
                            Err( Y_Y ) ;
                        end ;

                        { Load the symbol... }
                        Y_Y := Load_Symbol( YYY, YY, CR ) ;
                        if( length( Y_Y ) > 0 ) then
                        begin
                            Err( Y_Y ) ;
                        end else
                        begin
                            AR := Data_Read_Val( CR, 4, Res ) ;
                        end ;
                    end ;
                    if( AR <> 0 ) then
                    begin
                        Call_to_Address( AR ) ;
                    end ;
                end ;
            Op_Call_Native:
                begin
                    CR := Translate_Data_Address( Read_Immediate_Long ) ; // Get vector
                    int64( BR ) := CR ;
                    AR := Data_Read_Val( CR, 4, Res ) ; // Get routine address
                    if( AR = 0 ) then { Not called before }
                    begin
                        { Get routine name... }
                        C := Data_Read_Val( CR + 4, 1, Res ) ;
                        YY[ 0 ] := chr( C ) ;//Set_Length( YY, C ) ;
                        for D := 1 to C do
                        begin
                            YY[ D ] := chr( Data_Read_Val( int64( BR ) + 4 + D, 1, Res ) ) ;
                        end ;

                        { Get image name... }
                        int64( BR ) := int64( BR ) + 5 + C ;
                        C := Data_Read_Val( int64( BR ), 1, Res ) ;
                        Set_Length( YYY, C ) ;
                        for D := 1 to C do
                        begin
                            YYY[ D ] := chr( Data_Read_Val( int64( BR ) + D, 1, Res ) ) ;
                        end ;

                        { Load the image... }
                        Y_Y := Load_Native_Image( YYY ) ;
                        if( length( Y_Y ) > 0 ) then
                        begin
                            Err( Y_Y ) ;
                        end ;

                        { Load the symbol... }
                        Y_Y := Load_Native_Symbol( YYY, YY, CR ) ;
                        if( length( Y_Y ) > 0 ) then
                        begin
                            Err( Y_Y ) ;
                        end else
                        begin
                            AR := Data_Read_Val( CR, 4, Res ) ;
                        end ;
                    end ;
                    if( AR <> 0 ) then // We have a routine address
                    begin
                        Native_Address := AR ;
                        Parameter := Pop_Byte ; { Number of parameters }
                        if( _SP < 0 ) then { Stack underflow }
                        begin
                            Err( '??Stack underflow' ) ;
                            exit ;
                        end ;
                        B := Parameter * 8 ; { Size of stack frame (minus 1) }
                        { All parameters passed to native routines consist of 2
                          stack entries of 4 bytes each }
                        _SP := _SP - B ;
                        { Backup to first parameter.  Each parameter is 8 bytes:
                            4 bytes = value or address
                            4 bytes = address of type information }
                        if( _SP < 0 ) then
                        begin
                            exit ; { Stack underflow }
                        end ;

                        { Validate parameters... }
                        while( Parameter > 0 ) do
                        begin
                            Ar := _Read_Stack( _SP - Parameter * 8 + B, 4 ) ;
                            { Get parameter address/value }
                            if( State = State_Invalid_Memory_Reference ) then
                            begin
                                exit ;
                            end ;

                            { Get parameter type address... }
                            Res := 0 ;
                            CR := _Read_Stack( _SP - Parameter * 8 + B + 4, 4 ) ;
                            if( CR > 2147483647 ) then // Pass by reference
                            begin
                                CR := CR or (not 2147483647) ; // Sign extend
                            end ;
                            if( CR < 0 ) then // Pass-by-reference flag
                            begin
                                CR := -CR ;
                            end ;
                            if( State = State_Invalid_Memory_Reference ) then
                            begin
                                exit ;
                            end ;

                            Data_Type.ID := Data_Read_Val( CR, 2, Res ) ;
                            { Get type ID }
                            Data_Type.Size :=
                            Data_Read_Val( CR + 2 + sizeof( boolean ), 2, Res ) ;
                            if( Data_Type.ID = Type_External_File ) then
                            begin
                                Err( '??Cannot pass files to native routines' ) ;
                                exit ;
                            end else
                            begin
                                if( Data_Type.ID = Type_String ) then { string }
                                begin
                                    AR := Data_Read_Val( AR, 4, Res ) ;
                                    if( Res <> 0 ) then
                                    begin
                                        Err( '??Data address out of range' ) ;
                                        exit ;
                                    end ;
                                end ;
                            end ;
                            Parameter := Parameter - 1 ;
                        end ; { while( Parameter > 0 ) }
                        if( State = State_Invalid_Memory_Reference ) then
                        begin
                            exit ;
                        end ;

                        { Now push parameters onto actual stack... }
                        Parameter := B div 8 ;
                        asm
                            PUSH DS { Save DS and BP }
                            PUSH EBP
                        end ;
                        while( Parameter > 0 ) do
                        begin
                            Ar := _Read_Stack( _SP - Parameter * 8 + B, 4 ) ;
                            { Get parameter address }
                            if( State = State_Invalid_Memory_Reference ) then
                            begin
                                exit ;
                            end ;

                            { Get parameter type address... }
                            Res := 0 ;
                            CR := _Read_Stack( _SP - Parameter * 8 + B + 4, 4 ) ;
                            if( State = State_Invalid_Memory_Reference ) then
                            begin
                                exit ;
                            end ;
                            if( CR > 2147483647 ) then // Pass by reference
                            begin
                                CR := CR or (not 2147483647) ; // Sign extend
                            end ;
                            By_Ref := ( CR < 0 ) ;
                            if( By_Ref ) then
                            begin
                                CR := -CR ;
                            end ;

                            Data_Type.ID := Data_Read_Val( CR, 2, Res ) ;
                            { Get type ID }
                            Data_Type.Size :=
                                Data_Read_Val( CR + 2 + sizeof( boolean ), 2, Res ) ;
                            if( By_Ref ) then
                            begin
                                Data_Type.Size := 4 ; { Size of address }
                            end ;

                            { At this point, AR is the interpreter's address of
                              the data.  Convert it to a physical address.  In
                              the case of strings, the string is treated as a
                              PChar, and AR is the address of the string
                              header, which has a pointer to the physical
                              string data memory at offset 0.  if data is a
                              pointer, we need to get the pointer value,
                              convert it to a physical address and then pass
                              that instead. }
                            if( Data_Type.ID = Type_Generic_Pointer ) then
                            begin
                                AR := Data_Read_Val( AR, 4, Res ) ;
                                { Get pointer value }
                                AR := Map_Virtual_To_Physical( AR ) ;
                                Stack_Address := longint( @AR ) ;
                            end else
                            begin
                                Stack_Address := Map_Virtual_To_Physical( AR ) ;
                                if( By_Ref ) then
                                begin
                                    AR := Stack_Address ;
                                    Stack_Address := longint( @AR ) ; { Pass address }
                                end ;
                            end ;
                            { Stack_Address is now the physical location of
                              the data. }

                            asm
                                MOV Save_ESI, ESI
                                MOV Save_EBX, EBX
                                MOV ECX, 0
                                MOV CX, Data_Type.Size { Get parameter size }
                                MOV ESI, Stack_Address
                                MOV BX, CX { Save for comparisons }
                                AND BX, 3
                                ADD ECX, 3 { Only even longwords }
                                AND CL, $FC
                                ADD ESI, ECX { Start with high longword }
                                SUB ESI, 4
                                SHR ECX, 2 { convert from bytes to longwords }
Next_Word: { (or next longword) }
                                MOV EAX, 0 { Zero AX }
                                CMP BX, 0
                                JNE Load_Word { if on first longword of odd-word parameter }
                                MOV EAX, [ESI] { Load longword from memory }
                                JMP Do_Push
Load_Word:
                                MOV AX, [ESI] { Load word from memory }
                                MOV BX, 0 { Clear flag }
Do_Push:
                                PUSH EAX { Push longword to stack }
                                SUB ESI, 4 { Point to next longword }
                                LOOP Next_Word { Loop }
                                MOV ESI, Save_ESI
                                MOV EBX, Save_EBX
                            end ;
                            Parameter := Parameter - 1 ;
                        end ; { while( Parameter > 0 ) }
                        asm
                            MOV EBX, A
                            MOV ESI, 0
                            CALL Native_Address { Far call to routine }

                            POP EBP
                            POP DS
                        end ;
                        end ;
                    end ;
                Op_User_Extension:
                    begin
                        A := Read_Opcode ; // Get extension op-code
                        for Loop := 0 to Extensions.Count - 1 do
                        begin
                            Extension := TSVM_Extension( Extensions[ Loop ] ) ;
                            if( Extension.Valid_Instruction( A ) ) then
                            begin
                                Extension.Execute_OpCode( IP ) ;
                                exit ;
                            end ;
                        end ;

                        Err( '?Illegal op-code at address' ) ;
                    end ;
                else {Unused}
                begin
                    Err( '??Illegal op-code at address' ) ;
                end ;
        end ; { case A }
        Execute_SVM := False ; { Failure }
    end ; // .Execute_SVM


var A : integer ;
    Count : integer ;
    Nest_Level : integer ;
    Original_PC : int64 ;

begin // TSVM_CPU.Execute
    // Setup...
    Count := 0 ;
    Nest_Level := 0 ;

    // Execution loop
    while( True ) do
    begin
        if( Nest_Level = 0 ) then
        begin
            inc( Count ) ;
        end ;
        if( Single_Step and ( Count > 1 ) ) then
        begin
            exit ;
        end ;

        // Exit if we are done
        if( _Run_Stream = nil ) then // Not in immediate mode
        begin
            if( _Halted and ( not Single_Step ) ) then // Single-step should work if halted
            begin
                exit ;
            end ;
            _Halted := False ;
        end else
        begin
            if( _Run_Stream.At_End ) then
            begin
                exit ;
            end ;
        end ;

        if( _Run_Stream = nil ) then // Not in immediate mode
        begin
            if( Waiting ) then
            begin
                continue ;
            end ;
        end ;
        if( ( Count > 1 ) and ( _Run_Stream = nil ) ) then
        begin
            if( _Breakpoints.Indexof( _IP ) <> -1 ) then
            begin
                _UI.Breakpoint_Notice( _IP, True, 0, Parent ) ;
            end ;
        end ;
        if( _Profiling ) then
        begin
            TSVM_Profiler( Parent.Profiler ).Increment( Domain_Execution_Addresses, IP ) ;
            TSVM_Profiler( Parent.Profiler ).Increment( Domain_Other, Domain_Other_Instruction_Count ) ;
        end ;
        Log_Trace( 'Executing instruction at address ' + cvtb( 10, Base, inttostr( IP ) ) + ': ' + Instruction_At( IP ) ) ;
        Original_Pc := _IP ;
        A := Read_Opcode ; { Get next op-code }

        if( _Logger <> nil ) then
        begin
            _Logger.Update( Parent, Original_PC, A ) ;
        end ;

        if( _Profiling ) then
        begin
            TSVM_Profiler( Parent.Profiler ).Increment( Domain_Instructions, A ) ;
        end ;

        if( Execute_SVM( A ) ) then
        begin
            continue ;
        end ;

        // Reserved instruction...
        if( _Run_Stream <> nil ) then // Immediate mode
        begin
            exit ; // Shouldn't happen
        end ;
    end ; // while( True )
end ; // TSVM_CPU.Execute



function TSVM_CPU.Get_IP : int64 ;

begin
    Result := _IP ;
end ;


procedure TSVM_CPU.Set_IP( Value : int64 ) ;

begin
    _IP := Value ;
end ;


function TSVM_CPU.Get_SP : int64 ;

begin
    Result := _SP ;
end ;


procedure TSVM_CPU.Set_SP( Value : int64 ) ;

begin
    _SP := Value ;
end ;


function TSVM_CPU.Get_SF : int64 ;

begin
    Result := _SF ;
end ;


procedure TSVM_CPU.Set_SF( Value : int64 ) ;

begin
    _SF := Value ;
end ;


function TSVM_CPU.Get_Temp0 : string ;

begin
    Result := _Temp0 ;
end ;


procedure TSVM_CPU.Set_Temp0( Value : string ) ;

begin
    _Temp0 := Value ;
end ;


function TSVM_CPU.Translate( Space : integer ; Address : int64 ) : int64 ;

begin
    Result := Address ;
end ;


function TSVM_CPU.Default_Base : integer ;

begin
    Result := 10 ;
end ;


function TSVM_CPU.Get_Low_Memory : int64 ;

begin
    Result := 0 ;
end ;


function TSVM_CPU.Get_High_Memory : int64 ;

begin
    Result := $7FFFFFFFFFFFFFFF ;
end ;


function TSVM_CPU.Get_Low_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TSVM_CPU.Get_High_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := $7FFFFFFFFFFFFFFF ;
end ;


function TSVM_CPU.Get_Low_Port : int64 ;

begin
    Result := -1 ;
end ;


function TSVM_CPU.Get_High_Port : int64 ;

begin
    Result := -1 ;
end ;


function TSVM_CPU.Support_Virtual_Address : boolean ;

begin
    Result := False ;
end ;


function TSVM_CPU.Segment_Size( Index : integer ) : integer ;

begin
    if( ( Index < 0 ) or ( Segments = nil ) or ( Index >= Segments.Count ) ) then
    begin
        Result := 0 ;
        exit ;
    end ;
    Result := Segments[ Index ] ; 
end ;


function TSVM_CPU.Register_Name( Index : integer ) : PChar ;

begin
    case Index of
        0 : Temp_Register_Name := 'IP' ;
        1 : Temp_Register_Name := 'SP' ;
        2 : Temp_Register_Name := 'SF' ;
        3 : Temp_Register_Name := 'Temp0' ;
    end ;
    Result := PChar( Temp_Register_Name ) ;
end ;


function TSVM_CPU.Register_Size( Index : integer ) : integer ;

begin
    Result := 0 ;
    case Index of
        0 : Result := 64 ;
        1 : Result := 31 ;
        2 : Result := 31 ;
        3 : Result := length( _Temp0 ) * 8 ;
    end ; // case Index
end ;


procedure TSVM_CPU.Restart ;

var Image : TImage ;
    Loop : integer ;

begin
    // Reset processor state...
    _IP := 256 ;
    _SP := 0 ;
    _SF := 0 ;
    _Temp0 := '' ;
    Blocked := false ;
    fillchar( _Last_Error, sizeof( _Last_Error ), 0 ) ;
    _State := State_Normal ;

    // Reset I/O state...
    Buffer_Empty := True ; {Nothing in input buffer}
    Control_C := False ; {No control-C pressed}

    // Unload all loaded images...
    if( Images <> nil ) then
    begin
        for Loop := Images.Count - 1 downto 0 do
        begin
            Image := TImage( Images[ Loop ] ) ;
            Image.Free ;
        end ;
        Images.Clear ;
    end ;
    Code.Set_up( 'CLEAR' ) ;

    // Cancel p_Halted immediate mode commands
    _Run_Stream := nil ;
end ;


function TSVM_CPU.Top_Of_Stack( Index : integer ) : int64 ;

begin
    Top_Of_Stack := _SP ;
end ;


function TSVM_CPU.Big_Endian : boolean ;

begin
    Result := False ;
end ;


function TSVM_CPU.Register_RTS( RTS : TRun_Time_System ;
    Flags : longint ) : tUnified_Exception ;

begin
    _RTS := RTS ;
    _RTS_Flags := Flags ;
    Result := nil ;
end ;


function TSVM_CPU.Register_Information( Index : longint ) : CEF.TData_Type ;

var DT : TSVM_Data_Type ;

begin
    if( _Data_Type = nil ) then
    begin
        _Data_Type := TSVM_Data_Type.Create ;
    end ;
    DT := TSVM_Data_Type( _Data_Type ) ;
    case Index of
        0 :
            begin
                DT._Size := 64 ;
                DT._Data_Type := DataType_Integer ;
                DT._Max_Size := 64 ;
            end ;
        1..2 :
            begin
                DT._Size := 31 ;
                DT._Data_Type := DataType_Integer ;
                DT._Max_Size := 31 ;
            end ;
        3 : // Temp0
            begin
                DT._Size := 8 * length( _Temp0 ) ;
                DT._Data_Type := DataType_String ;
                DT._Max_Size := 255 * 8 ;
            end ;
        else
            begin
                Result := nil ;
                exit ;
            end ;
    end ;
    Result := _Data_Type ;
end ;


function TSVM_CPU.Get_Store( Index : longint ) : TComponent ;

begin
    case Index of
        0 : Result := Code ;
        1 : Result := Data_Cache ;
        2 : Result := Stack ;
        else Result := nil ;
    end ;
end ;


function TSVM_CPU.Get_Target_Memory : TComponent ;

begin
    Result := nil ;
    if( ( Last_Assembler <> nil )
        and
        TSVM_Assembler( Last_Assembler ).In_Assembly
        and
        TSVM_Assembler( Last_Assembler ).In_Data
      ) then
    begin
        Result := Data_Cache ;
    end ;
end ;


function TSVM_CPU.Get_Target_Address_Space : longint ;

begin
    Result := 0 ;
    if( ( Last_Assembler <> nil )
        and
        TSVM_Assembler( Last_Assembler ).In_Assembly
        and
        TSVM_Assembler( Last_Assembler ).In_Data
      ) then
    begin
        Result := 1 ;
    end ;
end ;


function TSVM_CPU.Get_Stack_Interface( Space : integer ) : TCEF_Stack_Interface ;

begin
    Result := TSVM_Stack_Interface.Create ;
    TSVM_Stack_Interface( Result )._CPU := self ;
end ;


function TSVM_CPU.Get_Current_Address( Space : integer ;
    Physical : boolean ) : int64 ;

begin
    Result := _IP ;
    if( ( Space = 1 ) // Data space
        and
        ( Last_Assembler <> nil )
        and
        TSVM_Assembler( Last_Assembler ).In_Assembly
        and
        TSVM_Assembler( Last_Assembler ).In_Data
      ) then
    begin
        Result := TSVM_Assembler( Last_Assembler ).Data_Address ;
    end ;
end ;


procedure TSVM_CPU.Set_Current_Address( Space : integer ; Physical : boolean ;
    Value : int64 ) ;

begin
    if( Space = 0 ) then
    begin
        _IP := Value ;
    end else
    if( ( Space = 1 ) // Data space
        and
        ( Last_Assembler <> nil )
        and
        TSVM_Assembler( Last_Assembler ).In_Assembly
        and
        TSVM_Assembler( Last_Assembler ).In_Data
      ) then
    begin
        TSVM_Assembler( Last_Assembler ).Data_Address := Value ;
    end ;
end ;


// TSVM methods...

// Internal utility routines...

function TSVM.Set_Error( C : integer ) : TUEC ;

begin
    Result.Facility := Facility_Code ;
    Result.Code := C ;
end ;


// API...

function TSVM.Facility_Code : longint ;

begin
    Result := SVMErr_Facility ;
end ;


function TSVM.Initialize( UI : TUI_Interface ) : TUEC ;

begin
    _CPU := TSVM_CPU.Create ;
    _CPU._UI := UI ;
    _CPU.Parent := self ;
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    CPU.Restart ; // Do power-on reset
end ;


function TSVM.Terminate : TUEC ;

begin
    if( _CPU._UI <> nil ) then
    begin
        _CPU._UI.Termination_Notice( self ) ;
    end ;
    Inputs.Free ;
    Inputs := nil ;
    Outputs.Free ;
    Outputs := nil ;
    CPU.Free ;
    _Memory.Free ;
    _Memory := nil ;
end ;


function TSVM.Serial_Number : integer ;

begin
    Result := _Serial_Number ;
end ;


function TSVM.Child_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TSVM.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUEC ;

begin
    Result := _CPU.Clear_Watchpoint( Address, Memory, Access ) ;
end ;


function TSVM.Component_Type : longint ;

begin
    Result := Component_Type_CPU ; // CPU
end ;


function TSVM.Connect_Input( Component : TComponent ) : TUEC ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( SVMErr_Invalid_Component ) ;
        exit ;
    end ;
    Result := Set_Error( SVMErr_Success ) ;
    if( Component.Cable <> nil ) then
    begin
        _CPU._Terminal := Component ;
        exit ;
    end ;
    if( Inputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( SVMErr_Already_Connected ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    if( _CPU._Code <> nil ) then
    begin
        _CPU._Code.Connect_Input( Component ) ;
    end ;
end ;


function TSVM.Connect_Output( Component : TComponent ) : TUEC ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( SVMErr_Invalid_Component ) ;
        exit ;
    end ;
    Result := Set_Error( SVMErr_Success ) ;
    if( Component.Cable <> nil ) then
    begin
        _CPU._Terminal := Component ;
        exit ;
    end ;
    if( Outputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( SVMErr_Already_Connected ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    if( _CPU._Code <> nil ) then
    begin
        _CPU._Code.Connect_Output( Component ) ;
    end ;
end ;


function TSVM.CPU : TCPU ;

begin
    Result := _CPU ;
end ;


function TSVM.Debugger : TDebug_Interface ;

begin
    Result := nil ; // TODO
end ;


function TSVM.Disconnect_Input( Component : TComponent ) : TUEC ;

begin
    Result := Set_Error( SVMErr_Success ) ;
    if( ( Component <> nil ) and ( _CPU._Terminal = Component ) ) then
    begin
        _CPU._Terminal := nil ;
        exit ;
    end ;
    if( ( Inputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Result := Set_Error( SVMErr_Component_Not_Found ) ;
    end else
    begin
	    Inputs.Remove( Component ) ;
        if( _CPU._Code <> nil ) then
        begin
            _CPU._Code.Disconnect_Input( Component ) ;
        end ;
    end ;
end ;


function TSVM.Disconnect_Output( Component : TComponent ) : TUEC ;

begin
    Result := Set_Error( SVMErr_Success ) ;
    if( ( Component <> nil ) and ( _CPU._Terminal = Component ) ) then
    begin
        _CPU._Terminal := nil ;
        exit ;
    end ;
    if( ( Outputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	    Result := Set_Error( SVMErr_Component_Not_Found ) ;
    end else
    begin
	    Outputs.Remove( Component ) ;
        if( _CPU._Code <> nil ) then
        begin
            _CPU._Code.Disconnect_Output( Component ) ;
        end ;
    end ;
end ;


function TSVM.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUEC ;

var S : string ;
    V : int64 ;

begin
    if( Memory ) then
    begin
        Set_Error( SVMErr_No_Cache ) ;
    end else
    begin
        if( Address >= 5 ) then
        begin
            Set_Error( SVMErr_Invalid_Register ) ;
            exit ;
        end ;
        Result := Set_Error( 0 ) ;

        if( Size = 0 ) then
        begin
            exit ;
        end ;
        if( Address = 3 ) then // Temp
        begin
            if( Size > 2040 ) then // 255 bytes
            begin
                Size := 2040 ;
            end ;
        end else
        if( Size > _CPU.Register_Size( Address ) ) then
        begin
            Size := _CPU.Register_Size( Address ) ;
        end ;
        Size := ( Size + 7 ) div 8 ; // Number of bytes
        V := 0 ;
        move( Buffer^, V, Size ) ;
        setlength( S, Size ) ;
        move( Buffer^, PChar( S )[ 0 ], Size ) ;
        case Address of
            0 : _CPU._IP := V ;
            1 : _CPU._SP := V ;
            2 : _CPU._SF := V ;
            3 : _CPU._Temp0 := S ;
            4 : _CPU._KB_Buffer := S ;
        end ;
    end ; // if( Memory )
end ; // TSVM.Deposit


function TSVM.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUEC ;

var Len : integer ;
    V : int64 ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( SVMErr_No_Cache ) ;
    end else
    begin
        if( Address >= 4 ) then
        begin
            Result := Set_Error( SVMErr_Invalid_Register ) ;
            exit ;
        end ;
        Result := Set_Error( 0 ) ;

        if( Size = 0 ) then
        begin
            exit ;
        end ;
        Len := _CPU.Register_Size( Address ) ;
        if( Size > Len ) then
        begin
            Size := Len ;
        end ;
        case Address of
            0 : V := _CPU._IP ;
            1 : V := _CPU._SP ;
            2 : V := _CPU._SF ;
        end ;
        if( Address = 3 ) then
        begin
            move( PChar( _CPU._Temp0 )[ 0 ], Buffer^, ( Size + 7 ) div 8 ) ;
        end else
        begin
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
        end ;
    end ; // if( Memory )
end ; // TSVM.Examine


function TSVM.Get_Access_Mode( Address : int64 ; Memory : boolean ) : longint ;

begin
    if( Memory ) then
    begin
        Result := Access_None ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 3 ) ) then
        begin
            Result := Access_None ;
        end else
        begin
            Result := Access_All ;
        end ;
    end ;
end ;


function TSVM.Get_Profiling : boolean ;

begin
    Result := _CPU._Profiling ;
end ;


function TSVM.Get_Read_Latency : longint ;

begin
    Result := 0 ;
end ;


function TSVM.Get_Write_Latency : longint ;

begin
    Result := 0 ;
end ;


function TSVM.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
        Result := nil ;
        _CPU.Set_Error( SVMErr_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Inputs[ Index ] ;
end ;


const SVM_Name : string = 'SVM' ;

function TSVM.Name : PChar ;

begin
    Result := PChar( SVM_Name ) ;
end ;


function TSVM.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Outputs.Count ) ) then
    begin
        Result := nil ;
        _CPU.Set_Error( SVMErr_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Outputs[ Index ] ;
end ;


function TSVM.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ;
end ;


function TSVM.Restore_Contents( Stream : TCOM_Stream ) : TUEC ;

begin
    Result := _CPU.Set_Error( 0 ) ;
end ;


function TSVM.Save_Contents( Stream : TCOM_Stream ) : TUEC ;

begin
    Result := _CPU.Set_Error( 0 ) ;
end ;


function TSVM.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUEC ;

begin
    Result := _CPU.Set_Error( SVMErr_Invalid_Operation ) ;
end ;


procedure TSVM.Set_Profiling( _On, Children : boolean ) ;

begin
    _CPU._Profiling := _On ;
end ;


procedure TSVM.Set_Read_Latency( Value : longint ) ;

begin
    // Do nothing - we have no read latency
end ;


function TSVM.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUEC ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    if( Access = 0 ) then // No effect
    begin
        exit ;
    end ;
    if( Memory ) then
    begin
        _CPU._Memory_Watchpoints.Create_Watchpoint( Address, Access ) ;
    end else
    begin
        Result := _CPU.Set_Error( SVMErr_Invalid_Address ) ;
    end ;
end ; // TSVM.Set_Watchpoint


procedure TSVM.Set_Write_Latency( Value : longint ) ;

begin
    // Intentionally left blank - no latency
end ;


function TSVM.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


procedure TSVM.Wake ;

begin
    _CPU.Blocked := False ;
end ;


function TSVM.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : integer ) : TUEC ;

var S : string ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    Size := ( Size + 7 ) div 8 ;
    setlength( S, Size ) ;
    move( Value, PChar( S )[ 0 ], Size ) ;
    _CPU._KB_Buffer := _CPU._KB_Buffer + S ;
end ;


function TSVM.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; IO_Type : longint ) : TUEC ;

var S : string ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    Size := ( Size + 7 ) div 8 ;
    
    if( IO_Type = IO_Type_Memory ) then
    begin
        _CPU.Memory_Data_Latch := 0 ;
        if( Size > sizeof( _CPU.Memory_Data_Latch ) ) then
        begin
            Size := sizeof( _CPU.Memory_Data_Latch ) ;
        end ;
        move( Value[ 0 ], _CPU.Memory_Data_Latch, Size ) ;
        exit ;
    end ;

    setlength( S, Size ) ;
    move( Value[ 0 ], PChar( S )[ 0 ], Size ) ;
    _CPU._KB_Buffer := _CPU._KB_Buffer + S ;
end ;


procedure TSVM.Set_Up( P : PChar ) ;

var S : string ;

begin
    S := string( P ) ;
    if( copy( S, 1, 6 ) = 'STACK ' ) then
    begin
        S := copy( S, 7, length( S ) ) ;
        _CPU._Stack := _CPU._UI.Load_Component( PChar( S ) ) ;
    end ;
end ;


procedure TSVM.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TSVM.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TSVM.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TSVM.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function TSVM.Profiler : TProfiler ;

begin
    if( _CPU._Profiler = nil ) then
    begin
        _CPU._Profiler := TSVM_Profiler.Create ;
    end ;
    Result := _CPU._Profiler ;
end ;


function TSVM.Get_Trace : boolean ;

begin
    Result := _CPU._Trace ;
end ;


procedure TSVM.Set_Trace( Value : boolean ) ;

begin
    _CPU._Trace := Value ;
end ;


function TSVM.Restore_State( Stream : TCOM_Stream ) : TUEC ;

begin
    Result := _CPU.Restore_State( Stream ) ;
end ;


function TSVM.Save_State( Stream : TCOM_Stream ) : TUEC ;

begin
    Result := _CPU.Save_State( Stream ) ;
end ;


procedure TSVM.Show_Status ;

    function Show( X, Size : Integer ) : String ; { Return formatted number X }

    begin
	    Show := Number_Format( _CPU.Base, Cvtb( 10, _CPU.Base, num1( X ) ), Size )
    end ;


var Index : integer ;
    S : string ;

    procedure Output( S : string ) ;

    begin
        _CPU._UI.Log_Status( PChar( S ), Index ) ;
        S := '' ;
        inc( Index ) ;
    end ;


var Loop : integer ;

begin
    Index := 0 ;
    Output( 'SVM' ) ;
    for Loop := 0 to 3 do
    begin
        S := 'IP=' + Show( _CPU._IP, 1 ) ;
        Output( S ) ;
        S := 'SP=' + Show( _CPU._SP, 1 ) ;
        Output( S ) ;
        S := 'SF=' + Show( _CPU._SF, 1 ) ;
        Output( S ) ;
        S := 'Temp0=' + _CPU._Temp0 ;
        Output( S ) ;
    end ;
    Output( S ) ;
end ; // TSVM.Show_Status


procedure TSVM.Reset ;

begin
    _CPU.Restart ;
end ;


procedure TSVM.Set_Signal( Name : PChar ; State : boolean ) ;

begin
end ; // TSVM.Set_Signal


function TSVM.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

begin
    Result := False ;
end ;


function TSVM.Signal_Count : longint ;

begin
    Result := 0 ;
end ;


function TSVM.Signal_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TSVM.Signal_Out( Index : longint ) : boolean ;

begin
    Result := True ;
end ;


function TSVM.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TSVM.Get_State_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TSVM.Get_Exception_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TSVM.Signal_Index( Name : PChar ) : integer ;

begin
    Result := -1 ;
end ;


function TSVM.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TSVM.Set_Logger( Value : TCEF_Logger ) ;

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
    _CPU.Logger := Value ;
end ;


end.
