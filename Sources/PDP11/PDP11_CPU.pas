{$N+}
{
        Program Name : PDP11
        Package Name : CEF
        Purpose      : PDP-11 CPU (CEF components)
        Institution  :
        Date Written : 9-Oct-2007
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
        26-Jun-2008   AC      Fixed subtract/Compare carry bit assignment.


        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

	  This unit implements a PDP-11 CPU emulator as a CEF CPU component.


        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan


          Unimplemented compatibility flags:
note: * = Required for 11/35 and 11/40

          Flag    Description
          ----    -----------
          CF3     True if PC contains instruction address + 2 for:
                  XXX PC,X(Rn)
                  XXX PC,@X(Rn)
                  XXX PC,A
                  XXX PC,@A
          CF8e    MFPT supported
          CF8l    CSM supported
*          CF14    Only RTI, RTT, traps, and interrupts can set T bit.
          CF18a   PSW address supported
          CF37    MMR0 bit 8 not implemented in memory management
          CF39a   PS=01 causes trap on memory reference.
          CF39b   PS=10 treated as PS=00
          CF39c   PS=10 causes trap on memory reference
          CF40    MTPS in non-user mode never traps.
          CF41    MFPS in user mode never traps.
*          CF28    PC is unchanged if bus error occurs when memory non-existant.
          CF29    Mode 2 register is unchanged if bus error occurs when memory
                  is non-existant.
*          CF30    Mode 2 register is unchanged if bus error occurs when register
                  has odd address.
*          CF37    MMR0 bit 8 is implemented.
*          CF43    HALT in user/superviser mode traps through 10 (else trap to 4)
*          CF44    PDR bit 0 not implemented
*          CF45    PDR bit 7 not implemented
*          CF46    Full PAR implemented
*          CF47    MMR0 bit 12 not implemented
          CF48    MMR3 D-space not implemented
          CF49    MMR3 IOMAP not implemented
          CF50    MMR3 bit 3 not implemented
*          CF51    MMR2 tracks only fetches
*          CF52    MTTPx R6m MTPx R6 when PS=10 uses user SP.
}

unit PDP11_CPU ;

interface

uses { Borland... }
     Classes, { TList }

     { CEF... }
     _CEF, // TMemory
     CEF, { TBase_CPU }
     _CEFUtil, // TCEF_Watchpoint

     { Other... }
     _DebugIn, { TDebug_Interface }
     CommonUt, { TInteger_List }
     _Streams, // TCOM_Stream
     _UE ; // TUnified_Exception

const SF_Predefined = 1 ; { Predefined symbol }

const PDP11Err_Facility = -1 ;
      PDP11Err_Success = 0 ;
      PDP11Err_Invalid_Component = 1 ;
      PDP11Err_Already_Connected = 2 ;
      PDP11Err_Component_Not_Found = 3 ;
      PDP11Err_No_Cache = 4 ;
      PDP11Err_Invalid_Register = 5 ;
      PDP11_No_Breakpoint = 6 ;
      PDP11_Breakpoint_Exists = 7 ;
      PDP11_Invalid_Address = 8 ;
      PDP11_Invalid_Operation = 9 ;
      PDP11_Invalid_Instruction = 10 ;

type TPDP11_Profiler = class( TBase_Profiler )
                         private // Instance data...
                             // Profile data...
                             _Clock : integer ;
                             _Instruction_Count : integer ;
                             Execution_Addresses : array[ 0..$FFFF ] of integer ;
                             Instructions : array[ 0..65535 ] of integer ;

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
                             procedure Increment_Clock( Count : integer ) ;

                         public // Overrides...
                             procedure Clear( Domain : integer ) ; override ;

                             function Domain_Name( Index : integer ) : PChar ;
                                 override ;

                             function Report_Line( Domain, Index : integer ) : PChar ;
                                 override ;
                     end ; // TPDP11_Profiler

type TMemory_Management_Unit = class
                                   public // APi...
                                       function Addressing : integer ;
                                           virtual ; abstract ;
                                       function Deposit( Address : integer ;
                                           Value : word ) : boolean ;
                                           virtual ; abstract ;
                                       function Examine( Address : integer ;
                                           var Success : boolean ) : word ;
                                           virtual ; abstract ;
                                       function Memory_Space_Description( Index : longint ;
                                           Physical : boolean ) : string ;
                                           virtual ; abstract ;
                                       function Read( Address : integer ;
                                           var Success : boolean ) : word ;
                                           virtual ; abstract ;
                                       function Relocate( Address : int64 ;
                                           PS : integer ; Read : boolean ;
                                           var Abort : boolean ; Data : boolean = True ) : int64 ;
                                           virtual ; abstract ;
                                       procedure Set_PC( Address : word ) ;
                                           virtual ; abstract ;
                                       function Write( Address : integer ;
                                           Value, PS : word ;
                                           var Abort : boolean ) : boolean ;
                                           virtual ; abstract ;
                                       function Write_Byte( Address : integer ;
                                           Value, PS : word ;
                                           var Abort : boolean ) : boolean ;
                                           virtual ; abstract ;
                               end ; // TMemory_Management_Unit

type TPDP11_CPU = class ;
     TPDP11 = class( TBase_Component )
                  private // Instance data...
                      _CPU : TPDP11_CPU ;
                      _Memory : TMemory ;
                      Inputs : TList ;
                      Outputs : TList ;
                      _Parent : TComponent ;
                      _Tag : longint ;
                      _Logger : TCEF_Logger ;

                      Temp_Signal_Name : string ;
                      Temp_Get_State_Name : string ;
                      Temp_Get_Exception_Description : string ;

                  public // API...
                      _Serial_Number : integer ;

                      function Initialize( UI : TUI_Interface ) : TUnified_Exception ;
                          override ;

                      function Terminate : TUnified_Exception ; override ;

                      function Serial_Number : integer ; override ;

                      function Child_Component( Index : longint ) : TComponent ;
                          override ;

                      function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                          Access : longint ) : TUnified_Exception ; override ;

                      function Component_Type : longint ; override ;

                      function Connect_Input( Component : TComponent ) : TUnified_Exception ;
                          override ;

                      function Connect_Output( Component : TComponent ) : TUnified_Exception ;
                          override ;

                      function CPU : TCPU ; override ;

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

                      function Facility_Code : longint ; override ;

                      function Get_Access_Mode( Address : int64 ;
                          Memory : boolean ) : longint ; override ;

                      function Get_Profiling : boolean ; override ;

                      function Get_Read_Latency : longint ; override ;

                      function Get_Write_Latency : longint ; override ;

                      function Input_Component( Index : longint ) : TComponent ;
                          override ;

                      function Memory : TMemory ; override ;

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

                      function Support_Feature( ID : integer ) : boolean ;
                          override ; stdcall ;

                      procedure Wake ; override ;

                      function Write( Address : int64 ; Value, Size : longint ;
                          IO_Type : longint ) : TUnified_Exception ; override ;

                      function Write_String( Address : int64 ; Value : PChar ;
                          Size : longint ; IO_Type : longint ) : TUnified_Exception ;
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
              end ; // TPDP11

     TPDP11_CPU = class( TBase_CPU )
        public // Constructors and destructors...
            constructor Create ;
            destructor Destroy ; override ;

        private // Generic CPU support...
            Parent : TPDP11 ;
            _UI : TUI_Interface ;
            _Speed : integer ; // KHz
            _Register_Watchpoints : array[ 0..23 ] of integer ; // Access mode for registers
            _Profiling : boolean ; // True if profiling
            _Memory_Watchpoints : TCEF_Watchpoint_Manager ;
            _Breakpoints : TInteger_List ;
            Memory_Data_Latch : word ; // Last byte or word sent to us
            _Run_Stream : TCOM_Stream ;
            _Stream_PC_Offset : integer ;
            _Profiler : TPDP11_Profiler ;
            _Trace : boolean ; // True to trace execution
            Blocked : boolean ;
            In_Trap : boolean ;
            Double_Trap : boolean ;
            _MMU : TMemory_Management_Unit ;
            Model : integer ; // PDP-11 model (eg 20 = PDP-11/20)
            Low_BR : integer ; // Lowest support BR/BG signals
            _Logger : TCEF_Logger ;
            _RTS : TRun_Time_System ;
            _RTS_Flags : longint ;

        private // Compatibilty flags...
            CF1 : boolean ;
                  // True if original register value is used for source in:
                  //   XXX Rn,(Rn)+
                  //   XXX rn,-(Rn)
                  //   XXX Rn,@(Rn)+
                  //   XXX Rn,@-(Rn)
            CF4 : boolean ; // True if JMP (Rn)+ and JSR reg,(Rn)+ use inital
                            // value of Rn for PC (else increment by 2 first).
            CF5 : boolean ; // True if JMP Rn or JSR reg, Rn traps to 4 (else 10).
            CF6 : boolean ; // True if SWAB clears V.
            CF8b : boolean ; // True if SOB, RTT, SXT supported
            CF8b1 : boolean ; // MARK supported
            CF8j : boolean ; // MFPD, MFPI, MTPD, MTPI supported (Kernel mode supported)
            CF8k : boolean ; // SPL supported
            CF11 : boolean ; // T acknowledged immediately after RTI (else after
                             // instruction following RTI).
            CF12 : boolean ; // Interrupt acknowleged ahead of T bit.
            CF13 : boolean ; // T bit trap exits WAIT state
            CF15 : boolean ; // Odd address/non-existent references using SP
                             // cause fatal trap (else HALT).
            CF17 : boolean ; // Dual register set supported
            CF18b : boolean ; // MTPS and MFPS supported.
            CF21 : boolean ; // Odd address trap enabled
            EIS : boolean ; // ASH, ASHC, DIV, MUL, XOR supported
            CPU_Error_Register : word ; // For 11/70, 11/73, (and ?)
            Use_Stack_Limit : boolean ; // True to use Kernel mode stack limit register // TODO: Implement
            Register_Map : TInteger_List ; // Maps from current model registers to maximal register set

            Memory_System : boolean ; // True if memory system registers used
            Memory_Control_Register : word ;

        private // PChar holding strings...
            Temp_Register_Name : string ;
            Temp_Register_Description : string ;
            Temp_Log_Trace : string ;
            Temp_Memory_Space_Description : string ;

        private // Processor internals...
            _Registers : array[ 0..1, 0..5 ] of word ;
            _R6 : array[ 0..3 ] of word ;
            _PC : word ;
            __PS : word ; // Processor status
            _Stack_Limit : word ;

            _Halted : boolean ; // True if last instruction was a halt
            Pending_Interrupt : boolean ; // Pending external interrupt
            Waiting : boolean ; // True if in a wait state
            Inhibit_T_Trap : boolean ; // True if T-Trap inhibited for one instruction

        private // Signal states...
            BG, BR : array[ 0..7 ] of boolean ;
            Signals : array[ 0..22 ] of boolean ;
            _Init : boolean ;

        private // Internal utility routines...
            function _Disassemble( Address : int64 ; Base : longint ;
                var Size : longint ) : string ;
            procedure Do_Wait ;
            function ByteRead( Address : Integer ; var E : boolean ) : Char ; { Return data at Address }
            function Byte_Read( Address : integer ; var E : boolean ) : byte ;
            function Word_Read( Address : integer ; var E : boolean ;
                _Virtual : boolean = True ) : integer ;
            procedure Byte_Write( Address, Value : Integer ; var E : boolean ) ; { Write to memory }
            procedure Word_Write( Address, Value : Integer ; var E : boolean ;
                _Virtual : boolean = True ) ;
            procedure Increment_Clock( Count : integer ) ;
            function Bus_Read( Address : Integer ; IO_Type : longint ;
                var E : boolean ) : Char ; { Return data at Address }
            function Bus_Examine( Address : int64 ) : Char ; { Return data at memory Address }
            function Bus_Read_Word( Address : Integer ; IO_Type : longint ;
                var E : boolean ; _Virtual : boolean = True ) : word ; { Return word data at Address }
            procedure Execute( Single_Step, Into : boolean ) ;
            procedure Send_Signal( Name : string ; Value : boolean ) ;
            procedure Clear_Watchpoints ;
            function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                Access : longint ) : TUnified_Exception ;
            function MMU : TMemory_Management_Unit ;
            function Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;
            function Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;
            procedure Set_Model( _Model : integer ) ;
            procedure State_Change_Notice( Index : integer ; State : boolean ) ;
            procedure Log_Trace( const Description : string ) ;
            procedure Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
                Memory, Internal, Port : boolean ) ;
            function Instruction_At( Address : integer ) : string ;
            procedure Push( Value : word ) ;
            procedure Trap( A : word ) ;

            function Get_PC : word ;
            procedure Set_PC( Value : word ) ;
            function Get_SP : word ;
            procedure Set_SP( Value : word ) ;
            function Get__SP : word ;
            procedure Set__SP( Value : word ) ;
            function Get_PS : word ;
            procedure Set_PS( Value : word ) ;
            procedure Set__PS( Value : word ) ;
            function Get_Register( Index : byte ) : word ;
            procedure Set_Register( Index : byte ; Value : word ) ;
            function Get_Register_Low( Index : byte ) : byte ;
            procedure Set_Register_Low( Index : byte ; Value : byte ) ;
            function Get__Register( Index : byte ) : word ;
            procedure Set__Register( Index : byte ; Value : word ) ;
            function Get__Register_Low( Index : byte ) : byte ;
            procedure Set__Register_Low( Index : byte ; Value : byte ) ;
            function Get_Priority : byte ; // Processor priority from PSW

            function PS_N : integer ; // Returns N flag (0 or 1)
            function PS_Z : integer ; // Returns Z flag (0 or 1)
            function PS_V : integer ; // Returns V flag (0 or 1)
            function PS_C : integer ; // Returns C flag (0 or 1)

            property _Register[ Index : byte ] : word
                read Get__Register
                write Set__Register ;
            property PC : word
                read Get_PC
                write Set_PC ;
            property SP : word
                read Get_SP
                write Set_SP ;
            property _SP : word
                read Get__SP
                write Set__SP ;
            property PS : word
                read Get_PS
                write Set_PS ;
            property _PS : word
                read __PS
                write Set__PS ;
            property Register[ Index : byte ] : word
                read Get_Register
                write Set_Register ;
            property Register_Low[ Index : byte ] : byte
                read Get_Register_Low
                write Set_Register_Low ;
            property _Register_Low[ Value : byte ] : byte
                read Get__Register_Low
                write Set__Register_Low ;
            property Priority : byte
                read Get_Priority ;
            property Logger : TCEF_Logger
                read _Logger
                write _Logger ;

        public // API...
            Base : integer ;

            procedure Zero ;

            function Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;
                override ;

            function Cancel_Breakpoint( Address : int64 ;
                Space : integer ; Physical : boolean ) : TUnified_Exception ; override ;

            function Disassemble( Address : int64 ; Base, Size : longint ;
                Stream : TCOM_Stream ) : TUnified_Exception ; override ;

            function Get_Clock_Speed : longint ; override ;

            procedure Halt ; override ;

            function Halted : boolean ; override ;

            function Memory_Space_Description( Index : longint ;
                Physical : boolean ) : PChar ; override ;

            procedure Run ; override ;

            procedure Run_From_Stream( Stream : TCOM_Stream ) ; override ;

            function Set_Breakpoint( Address : int64 ; Space : integer ;
                Physical : boolean ) : TUnified_Exception ; override ;

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
                Memory : boolean ; Access : integer ) : TUnified_Exception ; override ;

            function Set_Internal_Watchpoint( Address : int64 ;
                Memory : boolean ; Access : integer ) : TUnified_Exception ; override ;

            function Get_Current_Address( Space : integer ; Physical : boolean ) : int64 ;
                override ;

            procedure Set_Current_Address(Space : integer ; Physical : boolean ;
                Value : int64 ) ; override ;

            procedure Stop ; override ;

            function Top_Of_Stack( Index : integer ) : int64 ; override ;

            function Big_Endian : boolean ; override ;

            function Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;
                override ;
     end ; // TPDP11


     { This is for when there is no MMU.  To simplify coding, we always use MMU
       (a TMemory_Management_Unit), so this class is used for models with no
       MMU. }
type TBasic_Memory_Management_Unit = class( TMemory_Management_Unit )
                                         public // APi...
                                             function Addressing : integer ;
                                                 override ;
                                             function Deposit( Address : integer ;
                                                 Value : word ) : boolean ;
                                                 override ;
                                             function Examine( Address : integer ;
                                                 var Success : boolean ) : word ;
                                                 override ;
                                             function Memory_Space_Description( Index : longint ;
                                                 Physical : boolean ) : string ;
                                                 override ;
                                             function Read( Address : integer ;
                                                 var Success : boolean ) : word ;
                                                 override ;
                                             function Relocate( Address : int64 ;
                                                 PS : integer ; Read : boolean ;
                                                 var Abort : boolean ;
                                                 Data : boolean = True ) : int64 ;
                                                 override ;
                                             procedure Set_PC( Address : word ) ;
                                                 override ;
                                             function Write( Address : integer ;
                                                 Value, PS : word ;
                                                 var Abort : boolean ) : boolean ;
                                                 override ;
                                             function Write_Byte( Address : integer ;
                                                 Value, PS : word ;
                                                 var Abort : boolean ) : boolean ;
                                                 override ;
                                     end ; // TBasic_Memory_Management_Unit

     TAPR = record
                PAR : word ;
                PDR : word ;
            end ;

     TAPR_Set = array[ 0..7 ] of TAPR ;

     TKT11D_MMU = class( TMemory_Management_Unit ) // Memory management unit for 11/34, 11/35, and 11/40
                      private // Instance data...
                          SR0 : word ;
                          SR2 : word ;
                          APRs : array[ 0..3 ] of TAPR_Set ;

                      public // APi...
                          function Addressing : integer ; override ;
                          function Deposit( Address : integer ;
                              Value : word ) : boolean ; override ;
                          function Examine( Address : integer ;
                              var Success : boolean ) : word ; override ;
                          function Memory_Space_Description( Index : longint ;
                              Physical : boolean ) : string ; override ;
                          function Read( Address : integer ;
                              var Success : boolean ) : word ; override ;
                          function Relocate( Address : int64 ;
                              PS : integer ; Read : boolean ;
                              var Abort : boolean ;
                              Data : boolean = True ) : int64 ; override ;
                          procedure Set_PC( Address : word ) ; override ;
                          function Write( Address : integer ;
                              Value, PS : word ;
                              var Abort : boolean ) : boolean ; override ;
                          function Write_Byte( Address : integer ;
                              Value, PS : word ;
                              var Abort : boolean ) : boolean ;
                              override ;
                  end ; // TKT11D_MMU

implementation

uses { Borland... }
     SysUtils, { Allocmem }

     { PDP-11... }
     PDP11_ASM, // TPDP11_Assembler
     PDP11_Util,

     { Other... }
     _Octal,
     _ASCII, { CR }
     CVT, { Cvtb }
     HTML, { TXML_Parser }
     Num1s, { num1 }
     Parse, // TString_Parser
     SStreams, // TCOM_String_Stream
     Standard ; // Bit_Values

const O772300 = 259264 ;
const O772340 = 259296 ;
const O777572 = 262010 ;
const O777573 = 262011 ;
const O777576 = 262014 ;
const O777577 = 262015 ;
const O777600 = 262016 ;
const O777640 = 262048 ;
const O777700 = 262080 ;
const O777701 = 262081 ;
const O777702 = 262082 ;
const O777703 = 262083 ;
const O777704 = 262084 ;
const O777705 = 262085 ;
const O777706 = 262086 ;
const O777710 = 262088 ;
const O777711 = 262089 ;
const O777712 = 262090 ;
const O777713 = 262091 ;
const O777714 = 262092 ;
const O777715 = 262093 ;
const O777716 = 262094 ;
const O777717 = 262095 ;
const O777744 = 262116 ;
const O777745 = 262117 ;
const O777746 = 262118 ;
const O777747 = 262119 ;
const O777766 = 262134 ;
const O777767 = 262135 ;
const O777774 = 262140 ;
const O777776 = 262142 ;
const O777777 = 262143 ;

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ; external 'CEF_Util.dll' ;

// Profiler domains...
const Domain_Execution_Addresses = 0 ;
const Domain_Instructions = 1 ;
const Domain_Other = 2 ;
  const Domain_Other_Instruction_Count = 0 ;
  const Domain_Other_Clock = 1 ;


// CPU states...

const State_Interrupt = 0 ;
const State_Wait = 1 ;
const State_16_Bit = 2 ;
const State_18_Bit = 3 ;
const State_22_Bit = 4 ;
const State_Kernel = 5 ;
const State_Super = 6 ;
const State_User = 7 ;



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
        0 : Result := 'HALT' ;
        1 : Result := 'WAIT' ;
        2 : Result := 'RTI' ;
        3 : Result := 'BPT' ;
        4 : Result := 'IOT' ;
        5 : Result := 'RESET' ;
        6 : Result := 'RTT' ;
        O100..O177 : Result := 'JMP' ;
        O200..O207 : Result := 'RTS' ;
        O230..O237 : Result := 'SPL' ;
        O240..O257 :
            case I of
                O241 : Result := 'CLC' ;
                O242 : Result := 'CLV' ;
                O244 : Result := 'CLZ' ;
                O250 : Result := 'CLN' ;
                O257 : Result := 'CCC' ;
                else Result := 'CLEARCC' ; // Any other combination
            end ;
         O260..O277 :
            case I of
                O261 : Result := 'SEC' ;
                O262 : Result := 'SEV' ;
                O264 : Result := 'SEZ' ;
                O270 : Result := 'SEN' ;
                O277 : Result := 'SCC' ;
                else Result := 'SETCC' ; // Any other combination
            end ;
        O300..O377 : Result := 'SWAB' ;
        O400..O777 : Result := 'BR' ;
        O1000..O1377 : Result := 'BNE' ;
        O1400..O1777 : Result := 'BEQ' ;
        O2000..O2377 : Result := 'BGE' ;
        O2400..O2777 : Result := 'BLT' ;
        O3000..O3377 : Result := 'BGT' ;
        O3400..O3777 : Result := 'BLE' ;
        O4000..O4777 : Result := 'JSR' ;
        O5000..O5077 : Result := 'CLR' ;
        O5100..O5177 : Result := 'COM' ;
        O5200..O5277 : Result := 'INC' ;
        O5300..O5377 : Result := 'DEC' ;
        O5400..O5477 : Result := 'NEG' ;
        O5500..O5577 : Result := 'ADC' ;
        O5600..O5677 : Result := 'SBC' ;
        O5700..O5777 : Result := 'TST' ;
        O6000..O6077 : Result := 'ROR' ;
        O6100..O6177 : Result := 'ROL' ;
        O6200..O6277 : Result := 'ASR' ;
        O6300..O6377 : Result := 'ASL' ;
        O6500..O6577 : Result := 'MFPI' ;
        O6600..O6677 : Result := 'MTPI' ;
        O6700..O6777 : Result := 'SXT' ;
        O10000..O17777 : Result := 'MOV' ;
        O20000..O27777 : Result := 'CMP' ;
        O30000..O37777 : Result := 'BIT' ;
        O40000..O47777 : Result := 'BIC' ;
        O50000..O57777 : Result := 'BIS' ;
        O60000..O67777 : Result := 'ADD' ;
        O77000..O77777 : Result := 'SOB' ;
        O100000..O100377 : Result := 'BPL' ;
        O100400..O100777 : Result := 'BMI' ;
        O101000..O101377 : Result := 'BHI' ;
        O101400..O101777 : Result := 'BLOS' ;
        O102000..O102377 : Result := 'BVC' ;
        O102400..O102777 : Result := 'BVS' ;
        O103000..O103377 : Result := 'BCC' ;
        O103400..O103777 : Result := 'BCS' ;
        O104000..O104377 : Result := 'EMT' ;
        O104400..O104777 : Result := 'TRAP' ;
        O105000..O105077 : Result := 'CLRB' ;
        O105100..O105177 : Result := 'COMB' ;
        O105200..O105277 : Result := 'INCB' ;
        O105300..O105377 : Result := 'DECB' ;
        O105400..O105477 : Result := 'NEGB' ;
        O105500..O105577 : Result := 'ADCB' ;
        O105600..O105677 : Result := 'SBCB' ;
        O105700..O105777 : Result := 'TSTB' ;
        O106000..O106077 : Result := 'RORB' ;
        O106100..O106177 : Result := 'ROLB' ;
        O106200..O106277 : Result := 'ASRB' ;
        O106300..O106377 : Result := 'ASLB' ;
        O106500..O106577 : Result := 'MFPD' ;
        O106600..O106677 : Result := 'MTPD' ;
        O110000..O117777 : Result := 'MOVB' ;
        O120000..O127777 : Result := 'CMPB' ;
        O130000..O137777 : Result := 'BITB' ;
        O140000..O147777 : Result := 'BICB' ;
        O150000..O157777 : Result := 'BISB' ;
        O160000..O167777 : Result := 'SUB' ;
    end ;
end ; // Instruction_Name


function Octal( S : string ) : integer ;

begin
    Result := strtoint( cvtb( 8, 10, S ) ) ;
end ;


type TPDP11_Memory = class( TBase_Memory )
                         public // API...
                             function Facility_Code : longint ; override ;
                             procedure Get_Address_Range( var Low, High : int64 ) ;
                                 override ;
                     end ;

// API...

function TPDP11_Memory.Facility_Code : longint ;

begin
    Result := PDP11Err_Facility ;
end ;


procedure TPDP11_Memory.Get_Address_Range( var Low, High : int64 ) ;

begin
    Low := O772300 ;
    High := O777717 ;
end ;



// TBasic_Memory_Management_Unit methods...

// API...

function TBasic_Memory_Management_Unit.Addressing : integer ;

begin
    Result := 16 ;
end ;


function TBasic_Memory_Management_Unit.Deposit( Address : integer ;
    Value : word ) : boolean ;

begin
    Result := False ;
end ;


function TBasic_Memory_Management_Unit.Examine( Address : integer ;
    var Success : boolean ) : word ;

begin
    Result := Read( Address, Success ) ;
end ;


function TBasic_Memory_Management_Unit.Memory_Space_Description( Index : longint ;
    Physical : boolean ) : string ;

begin
    Result := '' ;
end ;


function TBasic_Memory_Management_Unit.Read( Address : integer ;
    var Success : boolean ) : word ;

begin
    Success := False ;
    Result := $FFFF ;
end ;


function TBasic_Memory_Management_Unit.Relocate( Address : int64 ;
    PS : integer ; Read : boolean ; var Abort : boolean ;
    Data : boolean = True ) : int64 ;

begin
    Address := Address and $FFFF ;
    if( ( Address and $E000 ) = $E000 ) then // Upper 4K of address range
    begin
        Address := Address or $30000 ; // Remap to upper 4K of UNIBUS
    end ;
    Result := Address ;
    Abort := False ;
end ;


procedure TBasic_Memory_Management_Unit.Set_PC( Address : word ) ;

begin
    // Intentionally left blank
end ;


function TBasic_Memory_Management_Unit.Write( Address : integer ;
    Value, PS : word ; var Abort : boolean ) : boolean ;

begin
    Result := False ;
    Abort := False ;
end ;


function TBasic_Memory_Management_Unit.Write_Byte( Address : integer ;
    Value, PS : word ; var Abort : boolean ) : boolean ;

begin
    Result := False ;
    Abort := False ;
end ;



// TKT11D_MMU methods...

// API...

function TKT11D_MMU.Addressing : integer ;

begin
    if( ( SR0 and 1 ) = 0 ) then
    begin
        Result := 16 ;
    end else
    begin
        Result := 18 ;
    end ;
end ;


function TKT11D_MMU.Deposit( Address : integer ;
    Value : word ) : boolean ;

var A : word ;

begin
    Result := False ; // Assume "failure"
    if( Address = O777572 ) then // SR0
    begin
        Result := True ;
        SR0 := Value ;
        exit ;
    end ;
    if( Address = O777576 ) then // SR2
    begin
        Result := True ; // Read-only
        SR2 := Value ;
        exit ;
    end ;
    A := Address - O772300 ;
    case A of
        0..15 : begin
                    Result := True ;
                    APRs[ 0 ][ A div 2 ].PDR := Value ;
                    exit ;
                end ;
    end ;
    A := Address - O772340 ;
    case A of
        0..15 : begin
                    Result := True ;
                    APRs[ 0 ][ A div 2 ].PAR := Value ;
                    exit ;
                end ;
    end ;

    A := Address - O777600 ;
    case A of
        0..15 : begin
                    Result := True ;
                    APRs[ 3 ][ A div 2 ].PDR := Value ;
                    exit ;
                end ;
    end ;
    A := Address - O777640 ;
    case A of
        0..15 : begin
                    Result := True ;
                    APRs[ 3 ][ A div 2 ].PAR := Value ;
                    exit ;
                end ;
    end ;
end ;


function TKT11D_MMU.Examine( Address : integer ;
    var Success : boolean ) : word ;

begin
    Result := Read( Address, Success ) ;
end ;


function TKT11D_MMU.Memory_Space_Description( Index : longint ;
    Physical : boolean ) : string ;

begin
    Result := '' ;
    if( Index < 0 ) then
    begin
        exit ;
    end ;
    case Index of
        0 : Result := 'Program' ;
        1 : Result := 'User' ;
        2 : Result := 'Kernel' ;
    end ;
end ;


function TKT11D_MMU.Read( Address : integer ; var Success : boolean ) : word ;

var A : word ;

begin
    Success := False ;
    Result := $FFFF ;
    if( Address = O777572 ) then // SR0
    begin
        Result := SR0 ;
        Success := True ;
        exit ;
    end ;
    if( Address = O777576 ) then // SR2
    begin
        Result := SR2 ;
        Success := True ;
        exit ;
    end ;
    A := Address - O772300 ;
    case A of
        0..15 : begin
                    Success := True ;
                    Result := APRs[ 0 ][ A div 2 ].PDR ;
                    exit ;
                end ;
    end ;
    A := Address - O772340 ;
    case A of
        0..15 : begin
                    Success := True ;
                    Result := APRs[ 0 ][ A div 2 ].PAR ;
                    exit ;
                end ;
    end ;
    A := Address - O777600 ;
    case A of
        0..15 : begin
                    Success := True ;
                    Result := APRs[ 3 ][ A div 2 ].PDR ;
                    exit ;
                end ;
    end ;
    A := Address - O777640 ;
    case A of
        0..15 : begin
                    Success := True ;
                    Result := APRs[ 3 ][ A div 2 ].PAR ;
                    exit ;
                end ;
    end ;
end ; // TKT11D_MMU.Read


function TKT11D_MMU.Relocate( Address : int64 ;
    PS : integer ; Read : boolean ; var Abort : boolean ;
    Data : boolean = True ) : int64 ;

var AFC, Block, Displacement, Length, Mode, Index : integer ;

begin
    Abort := False ;
    Address := Address and $FFFF ;
    Result := Address ;
    if(
        ( ( SR0 and 1 ) = 1 ) // Management enabled
        or
        (
          ( ( SR0 and 256 ) = 256 ) // Maintenance mode
          and
          ( not Read )
        )
      ) then
    begin
        Mode := PS shr 14 ;
        Index := Address shr 13 ;
        Block := ( Address shr 6 ) and 127 ;
        Displacement := Address and 63 ;
        AFC := ( APRs[ Mode ][ Index ].PDR shr 1 ) and 3 ;
        case AFC of
            0, 2 : begin
                       Abort := True ;
                       SR0 := SR0 or $8000 ;
                       SR0 := ( SR0 and $FF91 ) or ( Mode shl 5 ) or ( Index shl 1 ) ;
                       exit ;
                   end ;
            1 : if( not Read ) then // Write to read-only
                begin
                    Abort := True ;
                    SR0 := SR0 or $2000 ;
                    SR0 := ( SR0 and $FF91 ) or ( Mode shl 5 ) or ( Index shl 1 ) ;
                    exit ;
                end ;
        end ;
        Length := ( APRs[ Mode ][ Index ].PDR shr 8 ) and 127 ;
        Address := ( ( APRs[ Mode ][ Index ].PAR + Block ) shl 6 ) or Displacement ;
        if( ( APRs[ Mode ][ Index ].PDR and 8 ) = 0 ) then // Upward expandable
        begin
            if( Block > Length ) then
            begin
                Abort := True ;
                SR0 := SR0 or $4000 ; // Page length error
                SR0 := ( SR0 and $FF91 ) or ( Mode shl 5 ) or ( Index shl 1 ) ;
                exit ;
            end ;
        end else
        begin
            if( Block <= Length ) then
            begin
                Abort := True ;
                SR0 := SR0 or $4000 ; // Page length error
                SR0 := ( SR0 and $FF91 ) or ( Mode shl 5 ) or ( Index shl 1 ) ;
                exit ;
            end ;
        end ; // if( ( APRs[ Mode ][ Index ].PDR and 8 ) = 0 )
        if( not Read ) then
        begin
            APRs[ Mode ][ Index ].PDR := APRs[ Mode ][ Index ].PDR or 64 ; // W bit
        end ;
    end else
    if( ( Address and $E000 ) = $E000 ) then // Upper 4K of address range
    begin
        Address := Address or $30000 ; // Remap to upper 4K of UNIBUS
    end ;
    Result := Address ;
end ; // TKT11D_MMU.Relocate


procedure TKT11D_MMU.Set_PC( Address : word ) ;

begin
    if( ( SR0 and $E000 ) <> 0 ) then
    begin
        SR2 := Address ;
    end ;
end ;


function TKT11D_MMU.Write( Address : integer ;
    Value, PS : word ; var Abort : boolean ) : boolean ;

var A : word ;

begin
    Result := False ; // Assume "failure"
    Abort := False ;
    if( Address = O777572 ) then // SR0
    begin
        Result := True ;
        if(
            ( ( SR0 and 257 ) = 0 )
            and
            ( ( Value and 257 ) <> 0 )
          ) then // Enabling relocation
        begin
            if(
                ( ( PS shr 14 ) <> 0 )
                and
                ( ( PS shr 14 ) <> 3 )
              ) then // Illegal mode in PS
            begin
                SR0 := SR0 or $8000 ;
                Abort := True ;
                exit ;
            end ;
        end ;
        SR0 := Value ;
        exit ;
    end ;
    if( Address = O777576 ) then // SR2
    begin
        Result := True ; // Read-only
        exit ;
    end ;
    A := Address - O772300 ;
    case A of
        0..15 : begin
                    Result := True ;
                    APRs[ 0 ][ A div 2 ].PDR := Value and $7F0E ;
                    exit ;
                end ;
    end ;
    A := Address - O772340 ;
    case A of
        0..15 : begin
                    Result := True ;
                    APRs[ 0 ][ A div 2 ].PAR := Value and $FFF ;
                    APRs[ 0 ][ A div 2 ].PDR :=
                        APRs[ 0 ][ A div 2 ].PDR and $FF0F ; // Clear W
                    exit ;
                end ;
    end ;
    A := Address - O777600 ;
    case A of
        0..15 : begin
                    Result := True ;
                    APRs[ 3 ][ A div 2 ].PDR := Value and $7F0E ;
                    exit ;
                end ;
    end ;
    A := Address - O777640 ;
    case A of
        0..15 : begin
                    Result := True ;
                    APRs[ 3 ][ A div 2 ].PAR := Value and $FFF ;
                    APRs[ 3 ][ A div 2 ].PDR :=
                        APRs[ 3 ][ A div 2 ].PDR and $FF0F ; // Clear W
                    exit ;
                end ;
    end ;
end ; // TKT11D_MMU.Write


function TKT11D_MMU.Write_Byte( Address : integer ;
    Value, PS : word ; var Abort : boolean ) : boolean ;

var A : word ;

begin
    Result := False ; // Assume "failure"
    Abort := False ;
    Value := Value and 255 ;
    if( odd( Address ) ) then
    begin
        Value := Value shl 8 ;
    end ;
    if( Address = O777572 ) then // SR0 low
    begin
        Result := True ;
        if(
            ( ( SR0 and 257 ) = 0 )
            and
            ( ( Value and 257 ) <> 0 )
          ) then // Enabling relocation
        begin
            if(
                ( ( PS shr 14 ) <> 0 )
                and
                ( ( PS shr 14 ) <> 3 )
              ) then // Illegal mode in PS
            begin
                SR0 := SR0 or $8000 ;
                Abort := True ;
                exit ;
            end ;
        end ;
        SR0 := ( SR0 and $FF00 ) or Value ;
        exit ;
    end ;
    if( Address = O777573 ) then // SR0 high
    begin
        Result := True ;
        if(
            ( ( SR0 and 257 ) = 0 )
            and
            ( ( Value and 257 ) <> 0 )
          ) then // Enabling relocation
        begin
            if(
                ( ( PS shr 14 ) <> 0 )
                and
                ( ( PS shr 14 ) <> 3 )
              ) then // Illegal mode in PS
            begin
                SR0 := SR0 or $8000 ;
                Abort := True ;
                exit ;
            end ;
        end ;
        SR0 := ( SR0 and $FF ) or Value ;
        exit ;
    end ;
    if( ( Address = O777576 ) or ( Address = O777577 ) ) then // SR2
    begin
        Result := True ; // Read-only
        exit ;
    end ;
    A := Address - O772300 ;
    case A of
        0..15 : begin
                    Result := True ;
                    if( odd( A ) ) then
                    begin
                        APRs[ 0 ][ A div 2 ].PDR :=
                            ( APRs[ 0 ][ A div 2 ].PDR and $FF00 ) or ( Value and $7F0E ) ;
                    end else
                    begin
                        APRs[ 0 ][ A div 2 ].PDR :=
                            ( APRs[ 0 ][ A div 2 ].PDR and $FF ) or Value and $7F0E ;
                    end ;
                    exit ;
                end ;
    end ;
    A := Address - O772340 ;
    case A of
        0..15 : begin
                    Result := True ;
                    if( odd( A ) ) then
                    begin
                        APRs[ 0 ][ A div 2 ].PAR :=
                            ( APRs[ 0 ][ A div 2 ].PAR and $FF00 ) or ( Value and $FFF ) ;
                    end else
                    begin
                        APRs[ 0 ][ A div 2 ].PAR :=
                            ( APRs[ 0 ][ A div 2 ].PAR and $FF ) or ( Value and $FFF ) ;
                    end ;
                    APRs[ 0 ][ A div 2 ].PDR :=
                        APRs[ 0 ][ A div 2 ].PDR and $FF0F ; // Clear W
                    exit ;
                end ;
    end ;
    A := Address - O777600 ;
    case A of
        0..15 : begin
                    Result := True ;
                    if( odd( A ) ) then
                    begin
                        APRs[ 3 ][ A div 2 ].PDR :=
                            ( APRs[ 3 ][ A div 2 ].PDR and $FF00 ) or ( Value and $7F0E ) ;
                    end else
                    begin
                        APRs[ 3 ][ A div 2 ].PDR :=
                            ( APRs[ 3 ][ A div 2 ].PDR and $FF ) or ( Value and $7F0E ) ;
                    end ;
                    exit ;
                end ;
    end ;
    A := Address - O777640 ;
    case A of
        0..15 : begin
                    Result := True ;
                    if( odd( A ) ) then
                    begin
                        APRs[ 3 ][ A div 2 ].PAR :=
                            ( APRs[ 3 ][ A div 2 ].PAR and $FF00 ) or ( Value and $FFF ) ;
                    end else
                    begin
                        APRs[ 3 ][ A div 2 ].PAR :=
                            ( APRs[ 3 ][ A div 2 ].PAR and $FF ) or ( Value and $FFF ) ;
                    end ;
                    APRs[ 3 ][ A div 2 ].PDR :=
                        APRs[ 3 ][ A div 2 ].PDR and $FF0F ; // Clear W
                    exit ;
                end ;
    end ;
end ; // TKT11D_MMU.Write_Byte



// TPDP11_Profiler methods...

// API...

procedure TPDP11_Profiler.Generate_Report ;

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
end ; // TPDP11_Profiler.Generate_Report


procedure TPDP11_Profiler.Increment( Domain, Index : integer ) ;

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


procedure TPDP11_Profiler.Increment_Clock( Count : integer ) ;

begin
    Dirty := True ;
    _Clock := _Clock + Count ;
end ;


// Overrides...

procedure TPDP11_Profiler.Clear( Domain : integer ) ;

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
                _Clock := 0 ;
                _Instruction_Count := 0 ;
            end ;
    end ; // case Domain
end ; // TPDP11_Profiler.Clear


function TPDP11_Profiler.Domain_Name( Index : integer ) : PChar ;

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


function TPDP11_Profiler.Report_Line( Domain, Index : integer ) : PChar ;

begin
    if( Dirty ) then
    begin
        Generate_Report ;
    end ;
    Result := nil ;
    if( Outputs = nil ) then // Not set up
    begin
        exit ;
    end ;
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
                    Domain_Other_Clock : Temp_Report_Line := 'Clock cycles: ' + inttostr( _Clock ) ;
                    else exit ;
                end
            else exit ;
        end ;
    end ;
    Result := PChar( Temp_Report_Line ) ;
end ; // TPDP11_Profiler.Report_Line



// TPDP11_CPU methods...

// Constructors and destructors...

constructor TPDP11_CPU.Create ;

begin
    inherited Create ;

    _Memory_Watchpoints := Get_Watchpoint_Manager ;
    _Breakpoints := TInteger_List.Create ;
    _Speed := 1000000 ; // 1 GHz (cycle time - not actual clock time) - so we can measure in ns
    Base := Default_Base ;
    Register_Map := TInteger_List.Create ;
end ;


destructor TPDP11_CPU.Destroy ;

begin
    Clear_Watchpoints ;
    _Memory_Watchpoints.Terminate ;
    _Memory_Watchpoints := nil ;
    _Breakpoints.Free ;
    _Breakpoints := nil ;
    Register_Map.Free ;
    Register_Map := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TPDP11_CPU.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    if( Memory ) then
    begin
        Result := _Memory_Watchpoints.Clear_Watchpoint( Address, Access ) ;
    end else
    begin
        Result := Set_Error( PDP11_Invalid_Address ) ;
    end ; // if( Memory )
end ; // TPDP11_CPU.Clear_Watchpoint


procedure TPDP11_CPU.Set_Model( _Model : integer ) ;

var Loop : integer ;

begin
    Model := _Model ;
    _MMU.Free ;
    _MMU := nil ;
    State_Change_Notice( State_16_Bit, True ) ;
    Register_Map.Clear ;
    for Loop := 0 to 8 do
    begin
        Register_Map.Add( Loop ) ;
    end ;
    Low_BR := 0 ;
    Memory_System := False ;
    case _Model of
        15 : begin
                 CF1 := False ;
                 CF4 := False ;
                 CF5 := False ;
                 CF6 := False ; // Note: DEC's documentation is conflicting on this: PDP-11/15/20 processor handbook says True, Micro/PDP-11 Handbook says False
                 CF8b := False ;
                 CF8b1 := False ;
                 CF8k := False ;
                 CF8j := False ;
                 CF11 := False ;
                 CF12 := False ;
                 CF13 := False ;
                 CF15 := False ;
                 CF17 := False ;
                 CF18b := False ;
                 CF21 := True ;
                 EIS := False ;
                 Use_Stack_Limit := False ;
                 Low_BR := 4 ;
             end ;
        20 : begin
                 CF1 := False ;
                 CF4 := False ;
                 CF5 := False ;
                 CF6 := False ;
                 CF8b := False ;
                 CF8b1 := False ;
                 CF8k := False ;
                 CF8j := False ;
                 CF11 := False ;
                 CF13 := False ;
                 CF15 := False ;
                 CF17 := False ;
                 CF18b := False ;
                 CF21 := True ;
                 EIS := False ;
                 Use_Stack_Limit := False ;
                 Low_BR := 4 ;
             end ;
        34 : begin
                 CF1 := True ;
                 CF4 := True ;
                 CF5 := False ;
                 CF6 := True ;
                 CF8b := True ;
                 CF8b1 := True ;
                 CF8k := False ;
                 CF8j := True ;
                 CF11 := True ;
                 CF12 := False ;
                 CF13 := False ;
                 CF15 := False ;
                 CF17 := False ;
                 CF18b := True ;
                 CF21 := True ;
                 EIS := True ;
                 Use_Stack_Limit := False ;
                 _MMU := TKT11D_MMU.Create ;
                 Register_Map.Add( 9 ) ;
                 Register_Map.Add( 11 ) ;
                 Low_BR := 4 ;
             end ;
{
        35, 40 : begin
                     CF1 := False ;
                     CF4 := True ;
                     CF5 := False ;
                     CF6 := True ;
                     CF8b := True ;
                     CF8b1 := False ;?
                     CF8k := False ;
                     CF8j := True ;
                     CF11 := False ;
                     CF13 := False ;
                     CF15 := True ;
                     CF17 := False ;
                     CF18b := False ;
                     CF21 := True ;
                     EIS := True ;
                     Use_Stack_Limit := False ;
                     Low_BR := 4 ;
                     _MMU := TKT11D_MMU.Create ;
                 end ;
}
        44 : begin
                 CF1 := True ;
                 CF4 := True ;
                 CF5 := True ;
                 CF6 := True ;
                 CF8b := True ;
                 CF8b1 := True ;
                 CF8k := True ;
                 CF8j := True ;
                 CF11 := True ;
                 CF12 := False ;
                 CF13 := False ;
                 CF15 := False ;
                 CF17 := False ;
                 CF18b := False ;
                 CF21 := True ;
                 EIS := True ;
                 Use_Stack_Limit := True ;
                 Register_Map.Add( 9 ) ;
                 Register_Map.Add( 11 ) ;
             end ;
        45 : begin
                 CF1 := True ;
                 CF4 := True ;
                 CF5 := True ;
                 CF6 := True ;
                 CF8b := True ;
                 CF8b1 := True ;
                 CF8k := True ;
                 CF8j := True ;
                 CF11 := True ;
                 CF12 := True ;
                 CF13 := True ;
                 CF15 := True ;
                 CF17 := True ;
                 CF18b := False ;
                 CF21 := True ;
                 EIS := True ;
                 Use_Stack_Limit := True ;
                 Register_Map.Add( 9 ) ;
                 for Loop := 11 to 23 do
                 begin
                     Register_Map.Add( Loop ) ;
                 end ;
             end ;
        70 : begin
                 CF1 := True ;
                 CF4 := True ;
                 CF5 := True ;
                 CF6 := True ;
                 CF8b := True ;
                 CF8b1 := True ;
                 CF8k := True ;
                 CF8j := True ;
                 CF11 := True ;
                 CF12 := True ;
                 CF13 := True ;
                 CF15 := True ;
                 CF17 := True ;
                 CF18b := False ;
                 CF21 := True ;
                 EIS := True ;
                 Use_Stack_Limit := True ;
                 Register_Map.Add( 9 ) ;
                 for Loop := 11 to 23 do
                 begin
                     Register_Map.Add( Loop ) ;
                 end ;
                 Memory_System := True ;
             end ;
        53, 73, 93, 94 : // Micro/J-11
            begin
                 CF1 := False ;
                 CF4 := True ;
                 CF5 := True ;
                 CF6 := True ;
                 CF8b := True ;
                 CF8b1 := True ;
                 CF8k := True ;
                 CF8j := True ;
                 CF11 := True ;
                 CF12 := False ;
                 CF13 := False ;
                 CF15 := True ;
                 CF17 := True ;
                 CF18b := True ;
                 CF21 := True ;
                 EIS := True ;
                 Use_Stack_Limit := True ;
                 Register_Map.Add( 9 ) ;
                 for Loop := 11 to 23 do
                 begin
                     Register_Map.Add( Loop ) ;
                 end ;
             end ;
        else
            begin // 11/05 and 11/10
                CF1 := True ;
                CF4 := False ;
                CF5 := False ;
                CF6 := True ;
                CF8b := False ;
                CF8b1 := False ;
                CF8k := False ;
                CF8j := False ;
                CF11 := False ;
                CF12 := False ;
                CF13 := False ;
                CF15 := False ;
                CF17 := False ;
                CF18b := False ;
                CF21 := True ;
                EIS := False ;
                Use_Stack_Limit := False ;
                Low_BR := 4 ;
            end ;
    end ; // case Model
end ; // TPDP11.Set_Model


function _Signal_Name( Index : integer ) : string ;

begin
    case Index of
        0..7 : _Signal_Name := 'UNIBUS_BR' + inttostr( Index ) ;
        8 : _Signal_Name := 'UNIBUS_INIT' ;
        9..16 : _Signal_Name := 'UNIBUS_BG' + inttostr( Index - 9 ) ; // Interrupt acknowledge
        17 : _Signal_Name := '16 BIT' ;
        18 : _Signal_Name := '18 BIT' ;
        19 : _Signal_Name := '22 BIT' ;
        20 : _Signal_Name := 'KERNEL' ;
        21 : _Signal_Name := 'SUPER' ;
        22 : _Signal_Name := 'USER' ;
    end ;
end ;


procedure TPDP11_CPU.State_Change_Notice( Index : integer ; State : boolean ) ;

begin
    _UI.State_Change_Notice( Parent, Index, State ) ;

    case Index of
        State_Interrupt : Log_Trace( 'Process interrupt' ) ;
        State_Wait : Log_Trace( 'Process wait' ) ;
        State_16_Bit..State_User : Send_Signal( _Signal_Name( Index + 15 ), State ) ;
    end ;
end ;


procedure TPDP11_CPU.Log_Trace( const Description : string ) ;

begin
    if( _Trace ) then
    begin
        Temp_Log_Trace := Description ;
        _UI.Log_Trace( Parent, PChar( Temp_Log_Trace ) ) ;
    end ;
end ;


procedure TPDP11_CPU.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
    Memory, Internal, Port : boolean ) ;

begin
    _UI.Watchpoint_Notice( Address, Access, Tag, Parent, Memory, Internal, Port ) ;
end ;


function TPDP11_CPU.Instruction_At( Address : integer ) : string ;

var Stream : TCOM_String_Stream ;

begin
    Stream := TCOM_String_Stream.Create ;
    Disassemble( Address, Base, 1, Stream ) ;
    Result := string( Stream.As_String ) ;
    Stream.Detach ;
end ;


procedure TPDP11_CPU.Push( Value : word ) ;

var E : boolean ;

begin
    if( ( _SP and 1 ) = 1 ) then // Odd address
    begin
        _SP := _SP - 1 ; // Make an even (word) boundary
    end ;
    SP := _SP - 2 ;
    Word_Write( _SP, Value, E ) ;
end ;


procedure TPDP11_CPU.Trap( A : word ) ;

var Address : integer ;
    E : boolean ;
    Old_PC, Old_PS : word ;

begin
    if( ( _RTS_Flags and RTS_Want_Interrupts ) <> 0 ) then
    begin
        if( _RTS.Trap( A ) ) then
        begin
            exit ;
        end ;
    end ;
    if( In_Trap ) then // Trap caused a trap
    begin
        if( not CF15 ) then
        begin
            _Halted := True ;
        end ;
        exit ; // No infinite loops...
    end ;
    In_Trap := True ;
    try
        Address := MMU.Relocate( A, 0, True, E ) ; // Trap via Kernel address space
        if( E ) then
        begin
            Trap( 250 ) ; // MMU fault
        end ;
        if( _Halted ) then
        begin
            exit ;
        end ;
        Old_PC := _PC ;
        Old_PS := _PS ;
        _PC := Word_Read( Address, E ) ;
        if( E ) then
        begin
            Trap( 4 ) ; // Bus error
        end ;
        if( _Halted ) then
        begin
            exit ;
        end ;
        A := A + 2 ;
        Address := MMU.Relocate( A, 0, True, E ) ;
        if( E ) then
        begin
            Trap( 250 ) ; // MMU fault
        end ;
        if( _Halted ) then
        begin
            exit ;
        end ;
        _PS := Word_Read( Address, E ) ;
        if( E ) then
        begin
            Trap( 4 ) ; // Bus error
        end ;
        if( _Halted ) then
        begin
            exit ;
        end ;
        Push( Old_PS ) ;
        Push( Old_PC ) ;
        if( CF8j ) then
        begin
            _PS := ( _PS and $CFFF ) or ( ( Old_PS shr 2 ) and $3000 ) ; // Set previous mode
        end ;
    finally
        In_Trap := False ;
    end ;
    if( ( not Double_Trap ) and ( _SP < O400 ) ) then
    begin
        Double_Trap := True ;
        Trap( 4 ) ; // Stack overflow
        Double_Trap := False ;
    end ;
end ; // TPDP11_CPU.Trap


procedure TPDP11_CPU.Do_Wait ;

begin
    try
        _UI.Idle( Parent ) ;
    except
    end ;
end ;


function TPDP11_CPU.Bus_Examine( Address : int64 ) : Char ; { Return data at memory Address }

var Abort : boolean ;
    Component : TComponent ;
    Loop, Size : integer ;
    UEC : TUnified_Exception ;

begin
    Result := #255 ; // No component responded to this address
    Address := MMU.Relocate( Address, _PS, True, Abort ) ;
    if( Abort ) then
    begin
        Trap( O250 ) ;
        exit ;
    end ;
    if( ( Model <> 5 ) and ( Model <> 10 ) ) then
    begin
        if( ( Address >= O777700 ) and ( Address <= O777717 ) ) then
        begin
            Trap( 4 ) ; // Bus error
            exit ;
        end ;
    end ;

    for Loop := 0 to Parent.Inputs.Count - 1 do
    begin
        Size := 8 ;
        Component := TComponent( Parent.Inputs[ Loop ] ) ;
        UEC := Component.Examine( Address, Size, @Result, True ) ;
        if( UEC = nil ) then
        begin
            exit ;
        end ;
    end ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Size := 8 ;
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        UEC := Component.Examine( Address, Size, @Result, True ) ;
        if( UEC = nil ) then
        begin
            exit ;
        end ;
    end ;

    // Nothing responded...
    if( Memory_System ) then
    begin
        if( ( Address = O777744 ) or ( Address = O777745 ) ) then // Memory system error register
        begin
            Memory_Data_Latch := 0 ;
            exit ;
        end ;
        if( Address = O777746 ) then
        begin
            Memory_Data_Latch := Memory_Control_Register and 255 ;
            exit ;
        end ;
        if( Address = O777747 ) then
        begin
            Memory_Data_Latch := Memory_Control_Register shr 8 ;
            exit ;
        end ;
    end ;
//todo: only if 11/70, 11/73, etc...
        if( Address = O777766 ) then
        begin
            Memory_Data_Latch := CPU_Error_Register and 255 ;
            exit ;
        end ;
        if( Address = O777767 ) then
        begin
            Memory_Data_Latch := CPU_Error_Register shr 8 ;
            exit ;
        end ;
//...
    if( Address = O777700 ) then
    begin
        Result := char( _Registers[ 1, 0 ] ) ;
        exit ;
    end else
    if( Address = O777701 ) then
    begin
        Result := char( _Registers[ 1, 1 ] ) ;
        exit ;
    end else
    if( Address = O777702 ) then
    begin
        Result := char( _Registers[ 1, 2 ] ) ;
        exit ;
    end else
    if( Address = O777703 ) then
    begin
        Result := char( _Registers[ 1, 3 ] ) ;
        exit ;
    end else
    if( Address = O777704 ) then
    begin
        Result := char( _Registers[ 1, 4 ] ) ;
        exit ;
    end else
    if( Address = O777705 ) then
    begin
        Result := char( _Registers[ 1, 5 ] ) ;
        exit ;
    end else
    if( Address = O777706 ) then
    begin
        Result := char( _R6[ 0 ] ) ; // Kernel SP
        exit ;
    end else
    if( CF17 ) then
    begin
        if( Address = O777710 ) then
        begin
            Result := char( _Registers[ 1, 0 ] ) ;
            exit ;
        end else
        if( Address = O777711 ) then
        begin
            Result := char( _Registers[ 1, 1 ] ) ;
            exit ;
        end else
        if( Address = O777712 ) then
        begin
            Result := char( _Registers[ 1, 2 ] ) ;
            exit ;
        end else
        if( Address = O777713 ) then
        begin
            Result := char( _Registers[ 1, 3 ] ) ;
            exit ;
        end else
        if( Address = O777714 ) then
        begin
            Result := char( _Registers[ 1, 4 ] ) ;
            exit ;
        end else
        if( Address = O777715 ) then
        begin
            Result := char( _Registers[ 1, 5 ] ) ;
            exit ;
        end ;
    end ;
    if( CF8j ) then
    begin
        if( Address = O777716 ) then
        begin
            Result := char( _R6[ 1 ] ) ; // Supervisor SP
            exit ;
        end else
        if( Address = O777717 ) then
        begin
            Result := char( _R6[ 3 ] ) ; // User SP
            exit ;
        end ;
    end ;
    if( Address = O777776 ) then
    begin
        Result := char( PS ) ;
        exit ;
    end ;

    // Otherwise, trap...
    Trap( 4 ) ; // Bus error
end ; // TPDP11_CPU.Bus_Examine


function Address_For_Log( Virt_Address, Phys_Address, Base : longint ) : string ;

begin
    if( ( Base < 2 ) or ( Base > 36 ) ) then // Makes no sense for an address
    begin
        Base := 10 ;
    end ;
    Result := cvtb( 10, Base, inttostr( Virt_Address ) ) ;
    if( Virt_Address <> Phys_Address ) then
    begin
        Result := Result + ' (' + cvtb( 10, Base, inttostr( Phys_Address ) ) + ')' ;
    end ;
end ;


function Data_For_Log( Data, Base : longint ) : string ;

begin
    if( ( Base < 2 ) or ( Base > 36 ) ) then // Makes no sense for an address
    begin
        Base := 10 ;
    end ;
    Result := cvtb( 10, Base, inttostr( Data ) ) ;
end ;


function TPDP11_CPU.Bus_Read( Address : Integer ; IO_Type : longint ;
    var E : boolean ) : Char ; { Return data at Address }

var Abort : boolean ;
    Component : TComponent ;
    Loop : integer ;
    Saved : integer;

begin
    E := False ;
    Memory_Data_Latch := 255 ; // Default if nothing responds
    Result := #255 ;
    Saved := Address ;

    try
        if( IO_Type = IO_Type_Memory ) then
        begin
            if( _Logger <> nil ) then
            begin
                if( ( IO_Type = IO_Type_Memory ) and ( not E ) ) then
                begin
                    _Logger.Log( Parent, PChar( Address_For_Log( Saved, Address, _Logger.Data_Radix ) +
                        ' = ' + Data_For_Log( Memory_Data_Latch, _Logger.Data_Radix ) ),
                         -1, False, LT_Read ) ;
                end ;
            end ;
            Address := MMU.Relocate( Address, _PS, True, Abort ) ;
            if( Abort ) then
            begin
                Trap( O250 ) ;
                E := True ;
                exit ;
            end ;
        end ;
        if( ( Model <> 5 ) and ( Model <> 10 ) ) then // Only these two models can access registers via addresses
        begin
            if( ( Address >= O777700 ) and ( Address <= O777717 ) ) then
            begin
                Trap( 4 ) ; // Bus error
                E := True ;
                exit ;
            end ;
        end ;

        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Address, 8, IO_Type ) ) then
            begin
                exit ;
            end ;
        end ;

        // If no memory connected to inputs, try outputs...
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
            if( Component.Read( Address, 8, IO_Type ) ) then
            begin
                exit ;
            end ;
        end ;

        // Nothing responded...
        if( Memory_System ) then
        begin
            if( ( Address = O777744 ) or ( Address = O777745 ) ) then // Memory system error register
            begin
                Memory_Data_Latch := 0 ;
                exit ;
            end ;
            if( Address = O777746 ) then
            begin
                Memory_Data_Latch := Memory_Control_Register and 255 ;
                exit ;
            end ;
            if( Address = O777747 ) then
            begin
                Memory_Data_Latch := Memory_Control_Register shr 8 ;
                exit ;
            end ;
        end ;
//todo: only for 11/70, 11/73, etc.
        if( Address = O777766 ) then
        begin
            Memory_Data_Latch := CPU_Error_Register and 255 ;
            exit ;
        end ;
        if( Address = O777767 ) then
        begin
            Memory_Data_Latch := CPU_Error_Register shr 8 ;
            exit ;
        end ;
//...
        if( Address = O777700 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 0 ] ;
            exit ;
        end else
        if( Address = O777701 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 1 ] ;
            exit ;
        end else
        if( Address = O777702 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 2 ] ;
            exit ;
        end else
        if( Address = O777703 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 3 ] ;
            exit ;
        end else
        if( Address = O777704 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 4 ] ;
            exit ;
        end else
        if( Address = O777705 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 5 ] ;
            exit ;
        end else
        if( Address = O777706 ) then
        begin
            Memory_Data_Latch := _R6[ 0 ] ; // Kernel SP
            exit ;
        end else
        if( CF17 ) then
        begin
            if( Address = O777710 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 0 ] ;
                exit ;
            end else
            if( Address = O777711 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 1 ] ;
                exit ;
            end else
            if( Address = O777712 ) then
            begin
                Memory_Data_Latch :=  _Registers[ 1, 2 ] ;
                exit ;
            end else
            if( Address = O777713 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 3 ] ;
                exit ;
            end else
            if( Address = O777714 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 4 ] ;
                exit ;
            end else
            if( Address = O777715 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 5 ] ;
                exit ;
            end ;
        end ;
        if( CF8j ) then
        begin
            if( Address = O777716 ) then
            begin
                Memory_Data_Latch := _R6[ 1 ] ; // Supervisor SP
                exit ;
            end else
            if( Address = O777717 ) then
            begin
                Memory_Data_Latch := _R6[ 3 ] ; // User SP
                exit ;
            end ;
        end ;
        if( Address = O777776 ) then
        begin
            Memory_Data_Latch := PS ;
            exit ;
        end ;
        Memory_Data_Latch := MMU.Read( Address and $3FFFE, E ) ;
        if( E ) then // Success
        begin
            if( Odd( Address ) ) then // Reading high byte
            begin
                Memory_Data_Latch := Memory_Data_Latch shr 8 ;
            end ;
            Memory_Data_Latch := Memory_Data_Latch and 255 ;
            exit ;
        end ;

        // Otherwise, trap...
        Trap( 4 ) ; // Bus error
        E := True ;
    finally
        Bus_Read := char( Memory_Data_Latch ) ;
    end ;
end ; // TPDP11_CPU.Bus_Read


function TPDP11_CPU.Bus_Read_Word( Address : Integer ; IO_Type : longint ;
    var E : boolean ; _Virtual : boolean = True ) : word ;

var Abort : boolean ;
    Component : TComponent ;
    Loop : integer ;
    Saved : integer;

begin
    E := False ;
    Saved := Address;

    Memory_Data_Latch := 65535 ; // Default if nothing responds
    Result := 65535 ;
    if( _Logger <> nil ) then
    begin
        if( ( IO_Type = IO_Type_Memory ) and ( not E ) ) then
        begin
            _Logger.Log( Parent, PChar( Address_For_Log( Saved, Address, _Logger.Data_Radix ) +
                ' = ' + Data_For_Log( Memory_Data_Latch, _Logger.Data_Radix ) ),
                -1, False, LT_Read ) ;
        end ;
    end ;
    try
        if( _Virtual ) then
        begin
            if( In_Trap ) then
            begin
                Address := MMU.Relocate( Address, 0, True, Abort ) ;
            end else
            begin
                Address := MMU.Relocate( Address, _PS, True, Abort ) ;
            end ;
            if( Abort ) then
            begin
                Trap( O250 ) ;
                E := True ;
                exit ;
            end ;
        end ;
        if( ( Model <> 5 ) and ( Model <> 10 ) ) then
        begin
            if( ( Address >= O777700 ) and ( Address <= O777717 ) ) then
            begin
                Trap( 4 ) ; // Bus error
                E := True ;
                exit ;
            end ;
        end ;

        for Loop := 0 to Parent.Inputs.Count - 1 do
        begin
            Component := TComponent( Parent.Inputs[ Loop ] ) ;
            if( Component.Read( Address, 16, IO_Type ) ) then
            begin
                exit ;
            end ;
        end ;

        // If no memory connected to inputs, try outputs...
        for Loop := 0 to Parent.Outputs.Count - 1 do
        begin
            Component := TComponent( Parent.Outputs[ Loop ] ) ;
            if( Component.Read( Address, 16, IO_Type ) ) then
            begin
                exit ;
            end ;
        end ;

        // Nothing responded...
        if( Memory_System ) then
        begin
            if( Address = O777744 ) then // Memory system error register
            begin
                Memory_Data_Latch := 0 ;
                exit ;
            end ;
            if( Address = O777746 ) then
            begin
                Memory_Data_Latch := Memory_Control_Register ;
                exit ;
            end ;
        end ;
//todo: only for 11/70, 11/73, etc.
        if( Address = O777766 ) then
        begin
            Memory_Data_Latch := CPU_Error_Register ;
            exit ;
        end ;
//...
        if( Address = O777700 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 0 ] ;
            exit ;
        end else
        if( Address = O777701 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 1 ] ;
            exit ;
        end else
        if( Address = O777702 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 2 ] ;
            exit ;
        end else
        if( Address = O777703 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 3 ] ;
            exit ;
        end else
        if( Address = O777704 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 4 ] ;
            exit ;
        end else
        if( Address = O777705 ) then
        begin
            Memory_Data_Latch := _Registers[ 1, 5 ] ;
            exit ;
        end else
        if( Address = O777706 ) then
        begin
            Memory_Data_Latch := _R6[ 0 ] ; // Kernel SP
            exit ;
        end else
        if( CF17 ) then
        begin
            if( Address = O777710 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 0 ] ;
                exit ;
            end else
            if( Address = O777711 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 1 ] ;
                exit ;
            end else
            if( Address = O777712 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 2 ] ;
                exit ;
            end else
            if( Address = O777713 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 3 ] ;
                exit ;
            end else
            if( Address = O777714 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 4 ] ;
                exit ;
            end else
            if( Address = O777715 ) then
            begin
                Memory_Data_Latch := _Registers[ 1, 5 ] ;
                exit ;
            end ;
        end ;
        if( CF8j ) then
        begin
            if( Address = O777716 ) then
            begin
                Memory_Data_Latch := _R6[ 1 ] ; // Supervisor SP
                exit ;
            end else
            if( Address = O777717 ) then
            begin
                Memory_Data_Latch := _R6[ 3 ] ; // User SP
                exit ;
            end ;
        end ;
        if( Use_Stack_Limit and ( Address = O777774 ) ) then
        begin
            Memory_Data_Latch := _Stack_Limit ;
            exit ;
        end ;
        if( Address = O777776 ) then
        begin
            Memory_Data_Latch := PS ;
            exit ;
        end ;
        Memory_Data_Latch := MMU.Read( Address, E ) ;
        if( E ) then // Success
        begin
            E := False ;
            exit ;
        end ;

        // Otherwise trap...
        Trap( 4 ) ; // Bus error
        E := True ;
    finally
        Bus_Read_Word := Memory_Data_Latch ;
    end ;
end ; // TPDP11_CPU.Bus_Read_Word


function TPDP11_CPU.ByteRead( Address : Integer ; var E : boolean ) : Char ; { Return data at Address }

begin
    Result := Bus_Read( Address and $FFFF, IO_Type_Memory, E ) ;
end ;


function TPDP11_CPU.Byte_Read( Address : integer ; var E : boolean ) : byte ;
{ Read a byte from the specified address }

begin
    Byte_Read := ord( ByteRead( Address, E ) ) ;
end ;


function TPDP11_CPU.Word_Read( Address : integer ; var E : boolean ;
    _Virtual : boolean = True ) : integer ;

begin
    E := ( Address and 1 ) = 1 ; // Odd address on word read
    if( E ) then
    begin
        Word_Read := 0 ;
        exit ;
    end ;
    Result := Bus_Read_Word( Address and $FFFF, IO_Type_Memory, E, _Virtual ) ;
end ;


procedure TPDP11_CPU.Byte_Write( Address, Value : Integer ; var E : boolean ) ; { Write to memory }

var Abort : boolean ;
    Component : TComponent ;
    Loop : integer ;
    Old : integer ;
    Saved : integer ;
    Success : boolean ;

begin
    Value := Value and 255 ;
    E := False ;
    Saved := Address ;
    Address := MMU.Relocate( Address, _PS, True, Abort ) ;
    if( _Logger <> nil ) then
    begin
        _Logger.Log( Parent, PChar( Address_For_Log( Saved, Address, _Logger.Data_Radix ) +
            ' = ' + Data_For_Log( Value, _Logger.Data_Radix ) ),
             -1, False, LT_Write ) ;
    end ;
    if( Abort ) then
    begin
        Trap( O250 ) ;
        exit ;
    end ;
    if( ( Model <> 5 ) and ( Model <> 10 ) ) then
    begin
        if( ( Address >= O777700 ) and ( Address <= O777717 ) ) then
        begin
            Trap( 4 ) ; // Bus error
            exit ;
        end ;
    end ;

    Success := False ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        if( Component.Respond_To_Address( Address, IO_Type_Memory, False ) ) then
        begin
            Success := True ;
        end ;
        Component.Write( Address, Value, 8, IO_Type_Memory ) ;
    end ;

    if( not Success ) then // No device responded
    begin
        if( Memory_System ) then
        begin
            if( ( Address = O777744 ) or ( Address = O777745 ) ) then // Memory system error register
            begin
                exit ;
            end ;
            if( Address = O777746 ) then
            begin
                Memory_Control_Register := ( Memory_Control_Register and $FF00 ) or ( Value and 63 ) ;
                exit ;
            end ;
            if( Address = O777747 ) then
            begin
                exit ;
            end ;
        end ;
//todo: only for 11/70, 11/73, etc.
        if( Address = O777766 ) then
        begin
            CPU_Error_Register := Value and $FC ;
            exit ;
        end ;
        if( Address = O777767 ) then
        begin
            exit ;
        end ;
//...
        if( Address = O777700 ) then
        begin
            _Registers[ 1, 0 ] := Value ;
            exit ;
        end else
        if( Address = O777701 ) then
        begin
            _Registers[ 1, 1 ] := Value ;
            exit ;
        end else
        if( Address = O777702 ) then
        begin
            _Registers[ 1, 2 ] := Value ;
            exit ;
        end else
        if( Address = O777703 ) then
        begin
            _Registers[ 1, 3 ] := Value ;
            exit ;
        end else
        if( Address = O777704 ) then
        begin
            _Registers[ 1, 4 ] := Value ;
            exit ;
        end else
        if( Address = O777705 ) then
        begin
            _Registers[ 1, 5 ] := Value ;
            exit ;
        end else
        if( Address = O777706 ) then
        begin
            _R6[ 0 ] := Value ; // Kernel SP
            exit ;
        end else
        if( CF17 ) then
        begin
            if( Address = O777710 ) then
            begin
                _Registers[ 1, 0 ] := Value ;
                exit ;
            end else
            if( Address = O777711 ) then
            begin
                _Registers[ 1, 1 ] := Value ;
                exit ;
            end else
            if( Address = O777712 ) then
            begin
                _Registers[ 1, 2 ] := Value ;
                exit ;
            end else
            if( Address = O777713 ) then
            begin
                _Registers[ 1, 3 ] := Value ;
                exit ;
            end else
            if( Address = O777714 ) then
            begin
                _Registers[ 1, 4 ] := Value ;
                exit ;
            end else
            if( Address = O777715 ) then
            begin
                _Registers[ 1, 5 ] := Value ;
                exit ;
            end ;
        end ;
        if( CF8j ) then
        begin
            if( Address = O777716 ) then
            begin
                _R6[ 1 ] := Value ; // Supervisor SP
                exit ;
            end else
            if( Address = O777717 ) then
            begin
                _R6[ 3 ] := Value ; // User SP
                exit ;
            end ;
        end ;
        if( Address = O777776 ) then
        begin
            PS := ( _PS and ( not 255 ) ) or ( Value and 255 ) ;
            exit ;
        end else
        if( Address = O777777 ) then
        begin
            PS := ( Value shl 8 ) or ( _PS and 255 ) ;
            exit ;
        end ;
        Old := MMU.Addressing ;
        if( MMU.Write_Byte( Address, Value, _PS, Abort ) ) then
        begin
            if( Abort ) then
            begin
                Trap( O250 ) ;
                E := True ;
                exit ;
            end ;
            if( Old <> MMU.Addressing ) then
            begin
                case MMU.Addressing of
                    16 : State_Change_Notice( State_16_Bit, True ) ;
                    18 : State_Change_Notice( State_18_Bit, True ) ;
                    22 : State_Change_Notice( State_22_Bit, True ) ;
                end ;
            end ;
            exit ;
        end ;

        Trap( 4 ) ; // Bus error
        E := True ;
    end ;
end ; // TPDP11_CPU.Byte_Write


procedure TPDP11_CPU.Word_Write( Address, Value : Integer ; var E : boolean ;
    _Virtual : boolean = True ) ;

var Abort : boolean ;
    Component : TComponent ;
    Loop : integer ;
    Old : integer ;
    Saved : integer ;
    Success : boolean ;
    UEC : TUnified_Exception ;

begin
    E := ( Address and 1 ) = 1 ; // Odd address on word write
    if( E ) then
    begin
        exit ;
    end ;
    Saved := Address ;
    if( _Virtual ) then
    begin
        Address := MMU.Relocate( Address, _PS, False, Abort ) ;
    end ;
    if( _Logger <> nil ) then
    begin
        _Logger.Log( Parent, PChar( Address_For_Log( Saved, Address, _Logger.Data_Radix ) +
            ' = ' + Data_For_Log( Value shr 8, _Logger.Data_Radix ) ),
             -1, False, LT_Read ) ;
    end ;
    if( _Virtual ) then
    begin
        if( Abort ) then
        begin
            Trap( O250 ) ;
            E := True ;
            exit ;
        end ;
        if( ( Model <> 5 ) and ( Model <> 10 ) ) then
        begin
            if( ( Address >= O777700 ) and ( Address <= O777717 ) ) then
            begin
                Trap( 4 ) ; // Bus error
                exit ;
            end ;
        end ;
    end ;
    if( _Logger <> nil ) then
    begin
        _Logger.Log( Parent, PChar( Address_For_Log( Saved, Address, _Logger.Data_Radix ) +
            ' = ' + Data_For_Log( Value, _Logger.Data_Radix ) ),
             -1, False, LT_Write ) ;
    end ;

    Success := False ;
    for Loop := 0 to Parent.Outputs.Count - 1 do
    begin
        Component := TComponent( Parent.Outputs[ Loop ] ) ;
        if( Component.Respond_To_Address( Address, IO_Type_Memory, False ) ) then
        begin
            Success := True ;
        end ;
        UEC := Component.Write( Address, Value, 16, IO_Type_Memory ) ;
    end ; // for Loop := 0 to Parent.Outputs.Count - 1

    if( not Success ) then // No device responded
    begin
        if( Memory_System ) then
        begin
            if( Address = O777744 ) then // Memory system error register
            begin
                exit ;
            end ;
            if( Address = O777746 ) then
            begin
                Memory_Control_Register := Value and 63 ;
                exit ;
            end ;
        end ;
//todo: only for 11/70, 11/73, etc.
        if( Address = O777766 ) then
        begin
            CPU_Error_Register := Value and $FC ;
            exit ;
        end ;
//...
        if( Address = O777700 ) then
        begin
            _Registers[ 1, 0 ] := Value ;
            exit ;
        end else
        if( Address = O777701 ) then
        begin
            _Registers[ 1, 1 ] := Value ;
            exit ;
        end else
        if( Address = O777702 ) then
        begin
            _Registers[ 1, 2 ] := Value ;
            exit ;
        end else
        if( Address = O777703 ) then
        begin
            _Registers[ 1, 3 ] := Value ;
            exit ;
        end else
        if( Address = O777704 ) then
        begin
            _Registers[ 1, 4 ] := Value ;
            exit ;
        end else
        if( Address = O777705 ) then
        begin
            _Registers[ 1, 5 ] := Value ;
            exit ;
        end else
        if( Address = O777706 ) then
        begin
            _R6[ 0 ] := Value ; // Kernel SP
            exit ;
        end else
        if( CF17 ) then
        begin
            if( Address = O777710 ) then
            begin
                _Registers[ 1, 0 ] := Value ;
                exit ;
            end else
            if( Address = O777711 ) then
            begin
                _Registers[ 1, 1 ] := Value ;
                exit ;
            end else
            if( Address = O777712 ) then
            begin
                _Registers[ 1, 2 ] := Value ;
                exit ;
            end else
            if( Address = O777713 ) then
            begin
                _Registers[ 1, 3 ] := Value ;
                exit ;
            end else
            if( Address = O777714 ) then
            begin
                _Registers[ 1, 4 ] := Value ;
                exit ;
            end else
            if( Address = O777715 ) then
            begin
                _Registers[ 1, 5 ] := Value ;
                exit ;
            end ;
        end ;
        if( CF8j ) then
        begin
            if( Address = O777716 ) then
            begin
                _R6[ 1 ] := Value ; // Supervisor SP
                exit ;
            end else
            if( Address = O777717 ) then
            begin
                _R6[ 3 ] := Value ; // User SP
                exit ;
            end ;
        end ;
        if( Address = O777776 ) then
        begin
            PS := Value ;
            exit ;
        end ;
        Old := MMU.Addressing ;
        if( MMU.Write( Address, Value, _PS, Abort ) ) then
        begin
            if( Abort ) then
            begin
                Trap( O250 ) ;
                E := True ;
                exit ;
            end ;
            if( Old <> MMU.Addressing ) then
            begin
                case MMU.Addressing of
                    16 : State_Change_Notice( State_16_Bit, True ) ;
                    18 : State_Change_Notice( State_18_Bit, True ) ;
                    22 : State_Change_Notice( State_22_Bit, True ) ;
                end ;
            end ;
            exit ;
        end ;

        Trap( 4 ) ; // Bus error
    end ;
end ; // TPDP11_CPU.Word_Write


procedure TPDP11_CPU.Increment_Clock( Count : integer ) ;

var R : extended ;
    W : int64 ; // Amount of time to wait (in picoseconds)

begin
    if( Model = 5 ) then // 11/05 is 29% slower than 11/20
    begin
        Count := ( Count * 129 ) div 100 ;
    end ;

    if( _Profiling ) then
    begin
        TPDP11_Profiler( Parent.Profiler ).Increment_Clock( Count ) ;
    end ;
    R := 1 ;
    R := R / _Speed ; // Cycle time, in seconds
    R := R * Count ;
    R := int( R * 1000000000.0 ) ; // Convert from seconds to nanoseconds
    W := trunc( R ) ;
    try
        if( _UI.Clock <> nil ) then
        begin
            Blocked := True ;
            _UI.Clock.Block( Parent, W ) ;
        end ;
    except
    end ;
end ;


procedure TPDP11_CPU.Clear_Watchpoints ;

begin
end ;


function Mode_Name( M : integer ) : string ;

begin
    case M of
        0 : Result := 'Kernel' ;
        1 : Result := 'Supervisor' ;
        2 : Result := '?' ;
        3 : Result := 'User' ;
    end ;
end ;


function TPDP11_CPU.Register_Description( Index : integer ) : PChar ;

    procedure Check( Bit : integer ; const Flag : string ) ;

    begin
        if( ( _PS and Bit ) = Bit ) then
        begin
            Temp_Register_Description := Temp_Register_Description + Flag ;
        end else
        begin
            Temp_Register_Description := Temp_Register_Description + 'N' + Flag ;
        end ;
        Temp_Register_Description := Temp_Register_Description + ' ' ;
    end ;

begin
    Temp_Register_Description := '' ;
    if( ( Index >= 0 ) and ( Index < Register_Map.Count ) ) then
    begin
        Index := Register_Map[ Index ] ; // Translate to master register
        case Index of
            6 : Temp_Register_Description := ' (SP)' ;
            7 : Temp_Register_Description := ' (PC)' ;
            8 : // PS
                begin
                    Check( 16, 'T' ) ;
                    Check( 8, 'N' ) ;
                    Check( 4, 'Z' ) ;
                    Check( 2, 'V' ) ;
                    Check( 1, 'C' ) ;
                    Temp_Register_Description := Temp_Register_Description +
                        '  Pri: ' + inttostr( ( _PS shr 5 ) and 7 ) ;
                    If( CF8j ) then
                    begin
                        Temp_Register_Description := Temp_Register_Description +
                            '  Mode: ' + Mode_Name( ( _PS shr 14 ) and 3 )[ 1 ] +
                            '  Prev: ' + Mode_Name( ( _PS shr 12 ) and 3 )[ 1 ] ;
                    end ;
                end ;
        end ;
    end ;
    Result := PChar( Temp_Register_Description ) ;
end ;


procedure TPDP11_CPU.Zero ;

begin
    _PS := 0 ;
end ;


function TPDP11_CPU.Get_Assembler( Master : TMaster_Assembler ) : TAssembler ;

begin
    Result := TPDP11_Assembler.Create ;
    TPDP11_Assembler( Result ).CPU := self ;
    TPDP11_Assembler( Result ).CPU_Parent := Parent ;
    Result.Initialize( Master ) ;
    TPDP11_Assembler( Result ).Base := Base ;
end ;


function TPDP11_CPU.Cancel_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

var Index : integer ;

begin
    Result := Set_Error( PDP11_No_Breakpoint ) ; // Assume failure
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


function TPDP11_CPU.Get_Clock_Speed : longint ;

begin
    Result := _Speed ;
end ;


procedure TPDP11_CPU.Halt ;

begin
    _Halted := True ;
end ;


function TPDP11_CPU.Halted : boolean ;

begin
    Result := _Halted ;
end ;


function TPDP11_CPU.Memory_Space_Description( Index : longint ;
    Physical : boolean ) : PChar ;

begin
    Temp_Memory_Space_Description := MMU.Memory_Space_Description( Index, Physical ) ;
    if( Temp_Memory_Space_Description = '' ) then
    begin
        Result := nil ;
    end else
    begin
        Result := PChar( Temp_Memory_Space_Description ) ;
    end ;
end ;


procedure TPDP11_CPU.Run_From_Stream( Stream : TCOM_Stream ) ;

begin
    _Run_Stream := Stream ;
    Execute( False, False ) ;
    _Run_Stream := nil ;
end ;


procedure TPDP11_CPU.Run ;

begin
    _Halted := False ;
    Execute( False, False ) ;
end ;


function TPDP11_CPU.Set_Breakpoint( Address : int64 ; Space : integer ;
    Physical : boolean ) : TUnified_Exception ;

begin
    if( ( Address < 0 ) or ( Address > $FFFF ) ) then
    begin
        Result := Set_Error( PDP11_Invalid_Address ) ;
        exit ;
    end ;
    if( _Breakpoints.Indexof( Address ) <> -1 ) then
    begin
        Result := Set_Error( PDP11_Breakpoint_Exists ) ;
        exit ;
    end ;
    _Breakpoints.Add( Address ) ;
    Result := Set_Error( 0 ) ;
end ;


procedure TPDP11_CPU.Set_Clock_Speed( Value : longint ) ;

begin
    _Speed := Value ;
end ;


procedure TPDP11_CPU.Step( Into : boolean ) ;

begin
    _Halted := False ;
    Execute( True, Into ) ;
end ;


function TPDP11_CPU.Page_Size : longint ;

begin
    Result := 4096 ; // Max page length
end ;


function TPDP11_CPU.Clear_Internal_Watchpoint( Address : int64 ;
    Memory : boolean ; Access : integer ) : TUnified_Exception ;

var Max_Register : integer ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( PDP11Err_No_Cache ) ;
    end else
    begin
        Max_Register := 8 ;
        if( CF8j ) then
        begin
            if( CF17 ) then // Additional registers
            begin
                Max_Register := 23 ;
            end else
            begin
                Max_Register := 11 ;
            end ;
        end ;
        if( ( Address < 0 ) or ( Address > Max_Register ) ) then
        begin
            Result := Set_Error( PDP11Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] :=
                _Register_Watchpoints[ Address ] and ( not Access_None ) ;
            Result := Set_Error( 0 ) ;
        end ;
    end ;
end ;


function TPDP11_CPU.Set_Internal_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : integer ) : TUnified_Exception ;

var Max_Register : integer ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( PDP11Err_No_Cache ) ;
    end else
    begin
        Max_Register := 8 ;
        if( CF8j ) then
        begin
            if( CF17 ) then // Additional registers
            begin
                Max_Register := 23 ;
            end else
            begin
                Max_Register := 11 ;
            end ;
        end ;
        if( ( Address < 0 ) or ( Address > Max_Register ) ) then
        begin
            Result := Set_Error( PDP11Err_Invalid_Register ) ;
        end else
        begin
            _Register_Watchpoints[ Address ] :=
                _Register_Watchpoints[ Address ] or Access ;
            Result := Set_Error( 0 ) ;
        end ;
    end ;
end ;


procedure TPDP11_CPU.Stop ;

begin
    Halt ;
end ;


procedure TPDP11_CPU.Send_Signal( Name : string ; Value : boolean ) ;

var Index : integer ;

begin
    if( Name = 'UNIBUS_INIT' ) then
    begin
        Index := 8 ;
        _Init := Value ;
    end else
    if( ( copy( Name, 1, 9 ) = 'UNIBUS_BG' ) and ( length( Name ) = 10 ) ) then
    begin
        Index := pos( Name[ 10 ], '01234567' ) + 8 ;
        if( Index > 8 ) then
        begin
            BG[ Index - 9 ] := Value ;
        end else
        begin
            exit ;
        end ;
    end else
    if( Name = '16 BIT' ) then
    begin
        Index := 17 ;
        Signals[ Index ] := Value ;
    end else
    if( Name = '18 BIT' ) then
    begin
        Index := 18 ;
        Signals[ Index ] := Value ;
    end else
    if( Name = '22 BIT' ) then
    begin
        Index := 19 ;
        Signals[ Index ] := Value ;
    end else
    if( Name = 'KERNEL' ) then
    begin
        Index := 20 ;
        Signals[ Index ] := Value ;
    end else
    if( Name = 'SUPER' ) then
    begin
        Index := 21 ;
        Signals[ Index ] := Value ;
    end else
    if( Name = 'USER' ) then
    begin
        Index := 22 ;
        Signals[ Index ] := Value ;
    end else
    begin
        exit ; // A signal that we don't care about
    end ;
    if( _Logger <> nil ) then
    begin
        Index := pos( '_', Name + '_' ) ;
        Name := copy( Name, 1, Index - 1 ) ;
        _Logger.Log( Parent, PChar( Name + ' = ' + inttostr( ord( Value ) ) ), -1, True, LT_Sent_Signal ) ;
    end ;
    _UI.Signal_Change_Notice( Parent, Index, Value <> Parent.Signal_Active_Low( Index ) ) ;
end ; // TPDP11_CPU.Send_Signal


function TPDP11_CPU.MMU : TMemory_Management_Unit ;

begin
    if( _MMU = nil ) then
    begin
        _MMU := TBasic_Memory_Management_Unit.Create ;
    end ;
    Result := _MMU ;
end ;


function TPDP11_CPU.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

var Dummy : integer ;
    Loop, Loop1 : integer ;
    Parser : TXML_Parser ;
    S : string ;

begin
    // Setup default state...
    Result := Set_Error( 0 ) ;
    _Halted := False ;
    Waiting := False ;
    _Profiling := False ;
    Pending_Interrupt := False ;
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
            if( S = '<PENDING_INTEERUPT/>' ) then
            begin
                Pending_Interrupt := True ;
            end else
            if( S = '<HALTED/>' ) then
            begin
                _Halted := True ;
            end else
            if( S = '<PROFILING/>' ) then
            begin
                _Profiling := True ;
            end else
            if( S = '<REGISTERS0>' ) then
            begin
                S := Parser.Get_Section( 'Registers' ) ;
                S := copy( S, 2, length( S ) ) ; // Trim first bar
                Loop1 := 0 ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( '|', S ) ;
                    try
                        _Registers[ 0, Loop1 ] := strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop1 ) ;
                    if( Loop1 > 8 ) then // Too many values
                    begin
                        break ;
                    end ;
                end ;
            end else
            if( S = '<REGISTERS1>' ) then
            begin
                S := Parser.Get_Section( 'Registers' ) ;
                S := copy( S, 2, length( S ) ) ; // Trim first bar
                Loop1 := 0 ;
                while( length( S ) > 0 ) do
                begin
                    Dummy := pos( '|', S ) ;
                    try
                        _Registers[ 1, Loop1 ] := strtoint( copy( S, 1, Dummy - 1 ) ) ;
                    except
                    end ;
                    S := copy( S, Dummy + 1, length( S ) ) ;
                    inc( Loop1 ) ;
                    if( Loop1 > 8 ) then // Too many values
                    begin
                        break ;
                    end ;
                end ;
            end else
            if( S = '<SPU>' ) then
            begin
                S := Parser.Get_Section( 'SPU' ) ;
                try
                   _R6[ 0 ] := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<SPS>' ) then
            begin
                S := Parser.Get_Section( 'SPS' ) ;
                try
                    _R6[ 1 ] := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<SPK>' ) then
            begin
                S := Parser.Get_Section( 'SPK' ) ;
                try
                    _R6[ 2 ] := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<PC>' ) then
            begin
                S := Parser.Get_Section( 'PC' ) ;
                try
                    _PC := strtoint( S ) ;
                except
                end ;
            end else
            if( S = '<PS>' ) then
            begin
                S := Parser.Get_Section( 'PS' ) ;
                try
                    _PS := strtoint( S ) ;
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
end ; // TPDP11_CPU.Restore_State


function TPDP11_CPU.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

    procedure Output( S : string ) ;

    begin
        Stream.Write( S[ 1 ], length( S ) ) ;
    end ;

var Loop, Loop1 : integer ;

begin
    Result := Set_Error( 0 ) ;
    Output( '<base>' + inttostr( Base ) + '</base>' ) ;
    if( _Trace ) then
    begin
        Output( '<trace/>' ) ;
    end ;
    Output( '<speed>' + inttostr( _Speed ) + '</speed>' ) ;
    if( Pending_Interrupt ) then
    begin
        Output( '<pending_interrupt/>' ) ;
    end ;
    if( _Halted ) then
    begin
        Output( '<Halted/>' ) ;
    end ;
    if( Waiting ) then
    begin
        Output( '<wait/>' ) ;
    end ;
    if( _Profiling ) then
    begin
        Output( '<Profiling/>' ) ;
    end ;

    Output( '<Registers0>' ) ;
    for Loop1 := 0 to 5 do
    begin
        Output( '|' + inttostr( _Registers[ 0, Loop1 ] ) ) ;
    end ;
    Output( '</Registers0>' ) ;
    Output( '<Registers1>' ) ;
    for Loop1 := 0 to 5 do
    begin
        Output( '|' + inttostr( _Registers[ 1, Loop1 ] ) ) ;
    end ;
    Output( '</Registers1>' ) ;
    Output( '<spu>' + inttostr( _R6[ 0 ] ) + '</spu>' ) ;
    Output( '<sps>' + inttostr( _R6[ 1 ] ) + '</sps>' ) ;
    Output( '<spk>' + inttostr( _R6[ 2 ] ) + '</spk>' ) ;
    Output( '<pc>' + inttostr( _PC ) + '</pc>' ) ;
    Output( '<ps>' + inttostr( _PS ) + '</ps>' ) ;
    Output( '<Breakpoints>' + _Breakpoints.Serialize + '</Breakpoints>' ) ;
    Output( '<Register_Watchpoints>' ) ;
    for Loop := 0 to 8 do
    begin
        Output( '|' + inttostr( _Register_Watchpoints[ Loop ] ) ) ;
    end ;
    Output( '</Register_Watchpoints>' ) ;

    Output( '<Memory_watchpoints>' ) ;
    Output( '<watchpoint>' + string( _Memory_Watchpoints.Serialize ) ) ;
    Output( '</Memory_watchpoints>' ) ;
end ; // TPDP11_CPU.Save_State


function TPDP11_CPU._Disassemble( Address : int64 ; Base : longint ;
    var Size : longint ) : string ;

label Ee ;

var A : longint ;
    Instruction : string ;
    TPC : int64 ;

    function Base_Suffix( Base : integer ) : string ;

    begin
        case Base of
            16 : Result := 'H' ;
            10 : Result := '.' ;
            2 : Result := 'B' ;
            else Result := '' ;
        end ;
    end ;


    function Fetch : Integer ; { Fetch next byte }

    var A : Integer ;
        Temp : string ;

    begin
        A := ord( Bus_Examine( Tpc ) ) ;
        Tpc := Tpc + 1 ;
        Size := Size - 1 ;
        Temp := Cvtb( 10, Base, Num1( A ) ) ;
        if( ( length( Temp ) = 1 ) or ( pos( Temp[ 1 ], '0123456789' ) = 0 ) ) then
        begin
            Instruction := Instruction + '0' ;
        end ;
        Temp := Temp + Base_Suffix( Base ) ;
        Instruction := Instruction + Temp + ' ' ;
        Fetch := A ;
    end ;


    function FetchWord : Integer ; { Fetch next word }

    var A, B, C : Integer ;
        Temp : string ;

    begin
        A := Ord( Bus_Examine( Tpc ) ) ;
        B := Ord( Bus_Examine( Tpc + 1 ) ) ;
        Size := Size - 2 ;
        C := A or Swap( B ) ;
        Temp := Cvtb( 10, Base, Num1( Cvtif( C ) ) ) ;
        while( length( Temp ) < 4 ) do
        begin
            Temp := '0' + Temp ;
        end ;
        if( pos( Temp[ 1 ], '0123456789' ) = 0 ) then
        begin
            Instruction := Instruction + '0' ;
        end ;
        Temp := Temp + Base_Suffix( Base ) ;
        Instruction := Instruction + Temp + ' ' ;
        FetchWord := A or swap( B ) ;
        Tpc := Tpc + 2 ;
    end ;


    function Cvis( Value, Size : longint ) : string ;

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


    function _Reg( A : byte ) : string ;

    begin
        case A of
            0..5 : Result := 'R' + inttostr( A and 7 ) ;
            6 : Result := 'SP' ;
            else Result := 'PC' ;
        end ;
    end ;


    function Single_Operand( D, Offset : integer ) : string ;

    var Mode : integer ;
        Indirect : boolean ;
        Reg : integer ;
        V : integer ;

    begin
        Reg := D and 7 ;
        Indirect := ( ( D and 8 ) <> 0 ) ;
        Mode := ( D shr 4 ) and 3 ;
        Result := _Reg( Reg ) ;
        if( Reg = 7 ) then
        begin
            case Mode of
                1 : Result := '#' + CVTB( 10, Base, inttostr( FetchWord ) ) ;
                2 : Result := '-(' + Result + ')' ;
                3 : begin // Relative addressing
                        V := FetchWord ;
                        if( V > 32767 ) then
                        begin
                            V := V or $FFFF0000 ; // Sign extend
                        end ;
                        V := V + TPC + Offset ;
                        if( TPC < 65536 ) then
                        begin
                            V := V and 65535 ;
                        end ;
                        Result := CVTB( 10, Base, inttostr( V ) ) ;
                    end ;
            end ;
        end else
        begin
            case Mode of
                1 : Result := '(' + Result + ')+' ;
                2 : Result := '-(' + Result + ')' ;
                3 : begin
                        V := FetchWord ;
                        if( V > 32767 ) then
                        begin
                            V := V or $FFFF0000 ; // Sign extend
                        end ;
                        Result := CVTB( 10, Base, inttostr( V ) ) + '(' + Result + ')' ;
                    end ;
            end ;
        end ;
        if( Indirect ) then
        begin
            Result := '@' + Result ;
        end ;
    end ;


    function Double_Operand( D : integer ) : string ;

    var Offset : integer ;

    begin
        Offset := 0 ;
        if( ( ( D shr 4 ) and 3 ) = 3 ) then // Index mode in destination
        begin
            Offset := 2 ;
        end ;
        Result := Single_Operand( ( D shr 6 ) and 63, Offset ) + ',' +
            Single_Operand( D and 63, 0 ) ;
    end ;


var Dummy : integer ;
    Work : string ;

begin
    Result := '' ;
    Tpc := Address ;
    Instruction := '' ;
    A := FetchWord ;
    case A of
        0 : Result := 'HALT' ;
        1 : Result := 'WAIT' ;
        2 : Result := 'RTI' ;
        3 : Result := 'BPT' ;
        4 : Result := 'IOT' ;
        5 : Result := 'RESET' ;
        6 : if( CF8b ) then Result := 'RTT' ;
        O100..O177 : Result := 'JMP   ' + Single_Operand( A and 63, 0 ) ;
        O200..O207 : Result := 'RTS   ' + _Reg( A and 7 ) ;
        O230..O237 : if( CF8k ) then Result := 'SPL   ' + inttostr( A and 7 ) ;
        O240..O257 :
            case A of
                O241 : Result := 'CLC' ;
                O242 : Result := 'CLV' ;
                O244 : Result := 'CLZ' ;
                O250 : Result := 'CLN' ;
                else Result := 'SETCC ' + CVTB( 10, Base, inttostr( A and 31 ) ) ;
            end ;
        O260..O277 :
            case A of
                O261 : Result := 'SEC' ;
                O262 : Result := 'SEV' ;
                O264 : Result := 'SEZ' ;
                O270 : Result := 'SEN' ;
                else Result := 'CLEARCC ' + CVTB( 10, Base, inttostr( A and 31 ) ) ;
            end ;
        O300..O377 : Result := 'SWAB  ' + Single_Operand( A and 63, 0 ) ;
        O400..O777 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BR    ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O1000..O1377 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BNE   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O1400..O1777 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BEQ   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O2000..O2377 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BGE   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O2400..O2777 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BLT   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O3000..O3377 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BGT   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O3400..O3777 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BLE   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O4000..O4777 : Result := 'JSR   ' + _Reg( ( A shr 6 ) and 7 ) + ',' + Single_Operand( A and 63, 0 ) ;
        O5000..O5077 : Result := 'CLR   ' + Single_Operand( A and 63, 0 ) ;
        O5100..O5177 : Result := 'COM   ' + Single_Operand( A and 63, 0 ) ;
        O5200..O5277 : Result := 'INC   ' + Single_Operand( A and 63, 0 ) ;
        O5300..O5377 : Result := 'DEC   ' + Single_Operand( A and 63, 0 ) ;
        O5400..O5477 : Result := 'NEG   ' + Single_Operand( A and 63, 0 ) ;
        O5500..O5577 : Result := 'ADC   ' + Single_Operand( A and 63, 0 ) ;
        O5600..O5677 : Result := 'SBC   ' + Single_Operand( A and 63, 0 ) ;
        O5700..O5777 : Result := 'TST   ' + Single_Operand( A and 63, 0 ) ;
        O6000..O6077 : Result := 'ROR   ' + Single_Operand( A and 63, 0 ) ;
        O6100..O6177 : Result := 'ROL   ' + Single_Operand( A and 63, 0 ) ;
        O6200..O6277 : Result := 'ASR   ' + Single_Operand( A and 63, 0 ) ;
        O6300..O6377 : Result := 'ASL   ' + Single_Operand( A and 63, 0 ) ;
        O6400..O6477 : if( CF8b1 ) then Result := 'MARK  ' + inttostr( A and 63 ) ;
        O6500..O6577 : if( CF8j ) then Result := 'MFPI  ' + Single_Operand( A and 63, 0 ) ;
        O6600..O6677 : if( CF8j ) then Result := 'MTPI  ' + Single_Operand( A and 63, 0 ) ;
        O6700..O6777 : if( CF8b ) then Result := 'SXT   ' + Single_Operand( A and 63, 0 ) ;
        O10000..O17777 : Result := 'MOV   ' + Double_Operand( A ) ;
        O20000..O27777 : Result := 'CMP   ' + Double_Operand( A ) ;
        O30000..O37777 : Result := 'BIT   ' + Double_Operand( A ) ;
        O40000..O47777 : Result := 'BIC   ' + Double_Operand( A ) ;
        O50000..O57777 : Result := 'BIS   ' + Double_Operand( A ) ;
        O60000..O67777 : Result := 'ADD   ' + Double_Operand( A ) ;
        O70000..O70777 : if( EIS ) then Result := 'MUL   ' + Single_Operand( A and 63, 0 ) + ',' + _Reg( ( A shr 6 ) and 7 ) ;
        O71000..O71777 : if( EIS ) then Result := 'DIV  ' + Single_Operand( A and 63, 0 ) + ',' + _Reg( ( A shr 6 ) and 7 ) ;
        O72000..O72777 : if( EIS ) then
                         begin
                             Work := Single_Operand( A and 63, 0 ) ;
                             if( copy( Work, 1, 1 ) = '#' ) then // Immediate value
                             begin
                                 Dummy := strtoint( cvtb( Base, 10, copy( Work, 2, length( Work ) ) ) ) ;
                                 if( Dummy > 32767 ) then
                                 begin
                                     Dummy := Dummy - 65536 ;
                                     Work := '#' + inttostr( Dummy ) ;
                                 end ;
                             end ;
                             Result := 'ASH   ' + Work + ',' + _Reg( ( A shr 6 ) and 7 ) ;
                         end ;
        O73000..O73777 : if( EIS ) then
                         begin
                             Work := Single_Operand( A and 63, 0 ) ;
                             if( copy( Work, 1, 1 ) = '#' ) then // Immediate value
                             begin
                                 Dummy := strtoint( cvtb( Base, 10, copy( Work, 2, length( Work ) ) ) ) ;
                                 if( Dummy > 32767 ) then
                                 begin
                                     Dummy := Dummy - 65536 ;
                                     Work := '#' + inttostr( Dummy ) ;
                                 end ;
                             end ;
                             Result := 'ASHC  ' + Work + ',' + _Reg( ( A shr 6 ) and 7 ) ;
                         end ;
        O74000..O74777 : if( EIS ) then Result := 'XOR   ' + _Reg( ( A shr 6 ) and 7 ) + ',' + Single_Operand( A and 63, 0 ) ;
        O77000..O77777 : if( CF8b ) then  Result := 'SOB   ' + _Reg( ( A shr 6 ) and 7 ) + ',-' + inttostr( A and 63 ) ;
        O100000..O100377 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BPL   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O100400..O100777 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BMI   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O101000..O101377 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BHI   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O101400..O101777 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BLOS  ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O102000..O102377 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BVC   ' + CVTB( 10, Base, inttostr( TPC +  A * 2 ) ) ;
            end ;
        O102400..O102777 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BVS   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O103000..O103377 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BCC   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O103400..O103777 :
            begin
                A := A and 255 ;
                if( A > 127 ) then
                begin
                    A := A or $FFFFFF00 ; // Sign extend
                end ;
                Result := 'BCS   ' + CVTB( 10, Base, inttostr( TPC + A * 2 ) ) ;
            end ;
        O104000..O104377 : Result := 'EMT   ' + CVTB( 10, Base, inttostr( A and 127 ) ) ;
        O104400..O104777 : Result := 'TRAP  ' + CVTB( 10, Base, inttostr( A and 127 ) ) ;
        O105000..O105077 : Result := 'CLRB  ' + Single_Operand( A and 63, 0 ) ;
        O105100..O105177 : Result := 'COMB  ' + Single_Operand( A and 63, 0 ) ;
        O105200..O105277 : Result := 'INCB  ' + Single_Operand( A and 63, 0 ) ;
        O105300..O105377 : Result := 'DECB  ' + Single_Operand( A and 63, 0 ) ;
        O105400..O105477 : Result := 'NEGB  ' + Single_Operand( A and 63, 0 ) ;
        O105500..O105577 : Result := 'ADCB  ' + Single_Operand( A and 63, 0 ) ;
        O105600..O105677 : Result := 'SBCB  ' + Single_Operand( A and 63, 0 ) ;
        O105700..O105777 : Result := 'TSTB  ' + Single_Operand( A and 63, 0 ) ;
        O106000..O106077 : Result := 'RORB  ' + Single_Operand( A and 63, 0 ) ;
        O106100..O106177 : Result := 'ROLB  ' + Single_Operand( A and 63, 0 ) ;
        O106200..O106277 : Result := 'ASRB  ' + Single_Operand( A and 63, 0 ) ;
        O106300..O106377 : Result := 'ASLB  ' + Single_Operand( A and 63, 0 ) ;
        O106400..O106477 : if( CF18b ) then Result := 'MTPS  ' + Single_Operand( A and 63, 0 ) ;
        O106500..O106577 : if( CF8j ) then Result := 'MFPD  ' + Single_Operand( A and 63, 0 ) ;
        O106600..O106677 : if( CF8j ) then Result := 'MTPD  ' + Single_Operand( A and 63, 0 ) ;
        O106700..O106777 : if( CF18b ) then Result := 'MFPS  ' + Single_Operand( A and 63, 0 ) ;
        O110000..O117777 : Result := 'MOVB  ' + Double_Operand( A ) ;
        O120000..O127777 : Result := 'CMPB  ' + Double_Operand( A ) ;
        O130000..O137777 : Result := 'BITB  ' + Double_Operand( A ) ;
        O140000..O147777 : Result := 'BICB  ' + Double_Operand( A ) ;
        O150000..O157777 : Result := 'BISB  ' + Double_Operand( A ) ;
        O160000..O167777 : Result := 'SUB   ' + Double_Operand( A ) ;
    end ;
    if( length( Result ) = 0 ) then
    begin
        _Disassemble := 'DW    ' + Cvis( A, 2 ) ;
    end ;
end ; { TPDP11_CPU._Disassemble }


function TPDP11_CPU.Disassemble( Address : int64 ; Base, Size : longint ;
    Stream : TCOM_Stream ) : TUnified_Exception ;

var S : string ;

begin
    Result := Set_Error( 0 ) ;
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
    end ;
    Stream.Write( PChar( S )[ 0 ], length( S ) ) ;
end ; // TPDP11_CPU.Disassemble


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


procedure TPDP11_CPU.Execute( Single_Step, Into : boolean ) ;

    var Destination_Address, Source_Address : integer ;

    function Pop : word ;

    var E : boolean ;

    begin
        if( ( _SP and 1 ) = 1 ) then // Odd address
        begin
            _SP := _SP - 1 ; // Make an even (word) boundary
        end ;
        Pop := Word_Read( _SP, E ) ;
        SP := _SP + 2 ;
    end ;


    function Fetch( var E : boolean ) : Integer ; { Fetch next byte }

    var Ch : char ;
        Size : longint ;

    begin
        if( _Run_Stream <> nil ) then
        begin
            if( ( _PC < _Stream_PC_Offset ) or ( _Run_Stream.At_End ) ) then
            begin
                _Run_Stream := nil ; // Something made us jump out of the stream
            end else
            begin
                Size := 1 ;
                _Run_Stream.Read( Ch, Size ) ;
                Fetch := ord( Ch ) ;
                exit ;
            end ;
        end ;
        _MMU.Set_PC( _PC ) ;
        Fetch := ord( ByteRead( PC, E ) ) ;
        PC := _PC + 1 ;
    end ;


    function Fetch_Word : integer ; { Fetch a 2-byte word }

    var E : boolean ;
        Size : integer ;

    begin
        if( _Run_Stream <> nil ) then
        begin
            if( ( _PC < _Stream_PC_Offset ) or ( _Run_Stream.At_End ) ) then
            begin
                _Run_Stream := nil ; // Something made us jump out of the stream
            end else
            begin
                Size := 2 ;
                Result := 0 ;
                _Run_Stream.Read( Result, Size ) ;
                exit ;
            end ;
        end ;
        _MMU.Set_PC( _PC ) ;
        Result := Word_Read( PC, E ) ;
        PC := _PC + 2 ;
    end ;


    var Stack_Overflow : boolean ;

    function Resolve_Destination_Address( A, B : integer ;
        var E : boolean ; Read_Value : boolean = true ) : integer ;
    // A is mode from instruction,
    // B is 1 for byte instructions and 2 for word instructions,
    // E is ignored when passed, True if address error on return
    // Read_Value is true to read and return the destination

    var Mode, Reg, Saved : integer ;

    begin
        Result := 0 ; // Reduce warnings
        Stack_Overflow := False ;
        E := False ; // Assume success
        Saved := B ; // Save size
        if( ( A and 8 ) = 8 ) then // Indirect
        begin
            B := 2 ; // Indirect always increment/decrements by 2
        end ;
        Mode := ( A shr 3 ) and 7 ;
        Reg := A and 7 ;
        case Mode of
            0 : begin // Register Mode
                    if( Saved = 1 ) then
                    begin
                        Result := Register_Low[ Reg ] ;
                    end else
                    begin
                        Result := Register[ Reg ] ;
                    end ;
                end ;
            1 : begin // Register Deferred Mode
                    Increment_Clock( 1500 ) ;
                    Result := Register[ Reg ] ;
                    Destination_Address := Result ;
                    if( Read_Value ) then
                    begin
                        if( ( Reg = 7 ) and ( _Run_Stream <> nil ) ) then // Immediate mode accessing (PC)
                        begin
                            Result := Fetch_Word ;
                            exit ;
                        end else
                        begin
                            if( Saved = 1 ) then
                            begin
                                if( ( Result and 1 ) = 1 ) then // Odd address
                                begin
                                    Increment_Clock( 600 ) ;
                                end ;
                                Result := Byte_Read( Result, E ) ;
                            end else
                            begin
                                Result := Word_Read( Result, E ) ;
                            end ;
                            if( E ) then
                            begin
                                exit ;
                            end ;
                        end ;
                    end ;
                end ;
            2 : begin // Autoincrement Mode
                    Increment_Clock( 1500 ) ;
                    Result := Register[ Reg ] ;
                    if( ( _Run_Stream = nil ) or ( Reg < 7 ) ) then
                    begin
                        if( Reg >= 6 ) then
                        begin
                            Register[ Reg ] := _Register[ Reg ] + 2 ; // PC and SP always increment by 2
                        end else
                        begin
                            Register[ Reg ] := _Register[ Reg ] + B ;
                        end ;
                    end ;
                    Destination_Address := Result ;
                    if( Read_Value ) then
                    begin
                        if( ( Reg = 7 ) and ( _Run_Stream <> nil ) ) then // Immediate mode accessing (PC)
                        begin
                            Result := Fetch_Word ;
                            exit ;
                        end else
                        begin
                            if( Saved = 1 ) then
                            begin
                                if( ( Result and 1 ) = 1 ) then // Odd address
                                begin
                                    Increment_Clock( 600 ) ;
                                end ;
                                Result := Byte_Read( Result, E ) ;
                            end else
                            begin
                                Result := Word_Read( Result, E ) ;
                            end ;
                            if( E ) then
                            begin
                                exit ;
                            end ;
                        end ;
                    end ;
                end ;
            3 : begin // Autoincrement Deferred Mode
                    Increment_Clock( 1500 ) ;
                    Result := Register[ Reg ] ;
                    if( ( _Run_Stream = nil ) or ( Reg < 7 ) ) then
                    begin
                        Register[ Reg ] := _Register[ Reg ] + 2 ;
                    end ;
                    if( ( Reg = 7 ) and ( _Run_Stream <> nil ) ) then // Immediate absolute address (PC)
                    begin
                        Result := Fetch_Word ;
                        Destination_Address := Result ;
                        if( Read_Value ) then
                        begin
                            if( Saved = 1 ) then
                            begin
                                if( ( Result and 1 ) = 1 ) then // Odd address
                                begin
                                    Increment_Clock( 600 ) ;
                                end ;
                                Result := Byte_Read( Result, E ) ;
                            end else
                            begin
                                Result := Word_Read( Result, E ) ;
                            end ;
                        end ;
                        exit ;
                    end ;
                    Result := Word_Read( Result, E ) ;
                    if( E ) then
                    begin
                        exit ;
                    end ;
                    Destination_Address := Result ;
                    if( Read_Value ) then
                    begin
                        if( Saved = 1 ) then
                        begin
                            if( ( Result and 1 ) = 1 ) then // Odd address
                            begin
                                Increment_Clock( 600 ) ;
                            end ;
                            Result := Byte_Read( Result, E ) ;
                        end else
                        begin
                            Result := Word_Read( Result, E ) ;
                        end ;
                        if( E ) then
                        begin
                            exit ;
                        end ;
                    end ;
                end ;
            4 : begin // Autodecrement Mode
                    Increment_Clock( 2700 ) ;
                    if( ( _Run_Stream = nil ) or ( Reg < 7 ) ) then
                    begin
                        if( Reg >= 6 ) then
                        begin
                            Register[ Reg ] := _Register[ Reg ] - 2 ; // PC and SP always decrement by 2
                        end else
                        begin
                            Register[ Reg ] := _Register[ Reg ] - B ;
                        end ;
                    end ;
                    Result := Register[ Reg ] ;
                    Destination_Address := Result ;
                    if( Read_Value ) then
                    begin
                        if( ( Reg = 7 ) and ( _Run_Stream <> nil ) ) then // Immediate mode accessing (PC)
                        begin
                            Result := Fetch_Word ;
                            exit ;
                        end else
                        begin
                            if( Saved = 1 ) then
                            begin
                                if( ( Result and 1 ) = 1 ) then // Odd address
                                begin
                                    Increment_Clock( 600 ) ;
                                end ;
                                Result := Byte_Read( Result, E ) ;
                            end else
                            begin
                                Result := Word_Read( Result, E ) ;
                            end ;
                            if( E ) then
                            begin
                                exit ;
                            end ;
                        end ;
                    end ;
                    if( ( Reg = 6 ) and ( _SP < O400 ) ) then
                    begin
                        Stack_Overflow := True ;
                    end ;
                end ;
            5 : begin // Autodecrement Deferred Mode
                    Increment_Clock( 2700 ) ;
                    Register[ Reg ] := _Register[ Reg ] - 2 ;
                    Result := Register[ Reg ] ;
                    Result := Word_Read( Result, E ) ;
                    if( E ) then
                    begin
                        exit ;
                    end ;
                    Destination_Address := Result ;
                    if( Read_Value ) then
                    begin
                        if( Saved = 1 ) then
                        begin
                            if( ( Result and 1 ) = 1 ) then // Odd address
                            begin
                                Increment_Clock( 600 ) ;
                            end ;
                            Result := Byte_Read( Result, E ) ;
                        end else
                        begin
                            Result := Word_Read( Result, E ) ;
                        end ;
                        if( E ) then
                        begin
                            exit ;
                        end ;
                    end ;
                    if( ( Reg = 6 ) and ( _SP < O400 ) ) then
                    begin
                        Stack_Overflow := True ;
                    end ;
                end ;
            6 : begin // Index Mode
                    Increment_Clock( 2700 ) ;
                    Result := Fetch_Word ;
                    Result := ( Result + Register[ Reg ] ) and $FFFF ;
                    Destination_Address := Result ;
                    if( Read_Value ) then
                    begin
                        if( Saved = 1 ) then
                        begin
                            if( ( Result and 1 ) = 1 ) then // Odd address
                            begin
                                Increment_Clock( 600 ) ;
                            end ;
                            Result := Byte_Read( Result, E ) ;
                        end else
                        begin
                            Result := Word_Read( Result, E ) ;
                        end ;
                        if( E ) then
                        begin
                            exit ;
                        end ;
                    end ;
                end ;
            7 : begin // Index Deferred Mode
                    Increment_Clock( 3900 ) ;
                    Result := Fetch_Word ;
                    Result := ( Result + Register[ Reg ] ) and $FFFF ;
                    Result := Word_Read( Result, E ) ;
                    if( E ) then
                    begin
                        exit ;
                    end ;
                    Destination_Address := Result ;
                    if( Read_Value ) then
                    begin
                        if( Saved = 1 ) then
                        begin
                            if( ( Result and 1 ) = 1 ) then // Odd address
                            begin
                                Increment_Clock( 600 ) ;
                            end ;
                            Result := Byte_Read( Result, E ) ;
                        end else
                        begin
                            Result := Word_Read( Result, E ) ;
                        end ;
                        if( E ) then
                        begin
                            exit ;
                        end ;
                    end ;
                end ;
        end ; // case Mode
    end ; // .Resolve_Destination_Address


    procedure Resolve_Addresses( A, B : integer ;
        var E : boolean ; var Src, Dst : integer ;
        Read_Src, Read_Dst : boolean ) ;
    // A is source and destination modes from the instruction,
    // B is 1 for byte instructions and 2 for word instructions,
    // E is ignored when passed, True if address error on return
    // Src is ignored on call.  On return it is the source value,
    // Dst is ignored on call.  On return it is the destination value,
    // Read_Src is true to read and return the source value,
    // Read_Dst is true to read and return the destination value

    var Size : integer ;

    begin
        E := False ; // Assume no errors

        // Get Source
        if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
        begin
            Src := ( A shr 6 ) and 7 ; // Register
            if( not CF1 ) then
            begin
                if(
                    (
                      ( ( ( A shr 4 ) and 3 ) = 2 ) // Decrement mode on destination
                      or
                      ( ( ( A shr 4 ) and 3 ) = 1 ) // Increment mode on destination
                    )
                    and
                    ( Src = A and 7 ) // Same register
                  ) then
                begin
                    if( ( A and 8 ) = 8 ) then // Indirect
                    begin
                        Size := 2 ;
                    end else
                    begin
                        Size := B ;
                    end ;
                    if( ( ( A shr 4 ) and 3 ) = 2 ) then // Decrement mode on destination
                    begin
                        Src := _Register[ Src ] - Size ;
                    end else
                    begin // Increment mode on destination
                        Src := _Register[ Src ] + Size ;
                    end ;
                end else
                begin
                    Src := _Register[ Src ] ;
                end ;
            end else
            begin
                Src := _Register[ Src ] ;
            end ;
            if( B = 1 ) then
            begin
                Src := Src and 255 ;
            end ;
        end else
        begin
            Src := Resolve_Destination_Address( ( A shr 6 ) and 63, B, E, Read_Src ) ;
            if( E ) then
            begin
                exit ;
            end ;
        end ;
        Source_Address := Destination_Address ;

        // Set destination...
        if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
        begin
            Dst := Register[ A and 7 ] ;
            if( B = 1 ) then
            begin
                Dst := Dst and 255 ;
            end ;
        end else
        begin
            Dst := Resolve_Destination_Address( A and 63, B, E, Read_Dst ) ;
            Increment_Clock( -100 ) ;
        end ;
    end ; // .Resolve_Addresses


    procedure Branch_If( T : boolean ; A : longint ) ;

    begin
        if( T ) then
        begin
            Increment_Clock( 2600 ) ;
            A := A and 255 ;
            if( A > 127 ) then
            begin
                A := A or $FFFFFF00 ; // Sign-extend
            end ;
            PC := _PC + A * 2 ;
            if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
            begin
                _RTS.Jumped ;
            end ;
        end else
        begin
            Increment_Clock( 1500 ) ;
        end ;
    end ;


    function Sub( X, Y : word ) : word ; // Return X-Y and set flags

    var B : integer ;
        Temp : cardinal ;

    begin
        Temp := X + ( not Y ) + 1 ;
        Result := Temp ;
        B := _PS and ( not $F ) ;
        if( ( Result and $FFFF ) = 0 ) then
        begin
            B := B or 4 ;
        end else
        if( Result > 32767 ) then
        begin
            B := B or 8 ;
        end ;
        if( X < Y ) then  
        begin
            B := B or 1 ;
        end ;
        if(
            ( ( X and $8000 ) <> ( Y and $8000 ) ) // sign of operands differed
            and
            ( ( Y and $8000 ) = ( Result and $8000 ) ) // Sign of result = sign of Y
          ) then
        begin
            B := B or 2 ;
        end ;
        PS := B ;
    end ;


    function Subb( X, Y : byte ) : byte ; // Return X-Y and set flags (byte)

    var B : integer ;
        Temp : cardinal ;

    begin
        Temp := X + ( not Y ) + 1 ;
        Result := Temp ;
        B := _PS and ( not $F ) ;
        if( ( Result and $FF ) = 0 ) then
        begin
            B := B or 4 ;
        end else
        if( Result > 127 ) then
        begin
            B := B or 8 ;
        end ;
        if( X < Y ) then
        begin
            B := B or 1 ;
        end ;
        if(
            ( ( X and $80 ) <> ( Y and $80 ) ) // sign of operands differed
            and
            ( ( Y and $80 ) = ( Result and $80 ) ) // Sign of result = sign of Y
          ) then
        begin
            B := B or 2 ;
        end ;
        PS := B ;
    end ;


    function Execute_PDP11( A : integer ) : boolean ;

        procedure Set_N_Bit( V : integer ; var F : integer ) ;

        begin
            if( ( V and $8000 ) = $8000 ) then
            begin
                F := F or 8 ;
            end ;
        end ;

    var B, C, F, I, Mode, Src, Dst, V : integer ;
        E : boolean ;
        IP, IP1 : int64 ;
        Temp_High : int64 ;

    begin
        Execute_PDP11 := True ; { Assume success }
        Stack_Overflow := False ;

        case A of
            0 : // HALT
                begin
                    if( CF8j and ( ( _PS shr 14 ) <> 0 ) ) then // Not Kernel mode
                    begin
                        Trap( O10 ) ;
                        exit ;
                    end ;
                    Increment_Clock( 1800 ) ;
                    _Halted := True ;
                    exit ;
                end ;
            1 : // WAIT
                begin
                    Increment_Clock( 1800 ) ;
                    Waiting := True ;
                    State_Change_Notice( State_Wait, True ) ;
                    exit ;
                end ;
            2 : // RTI
                begin
                    if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                    begin
                        if( _RTS.Return ) then
                        begin
                            exit ;
                        end ;
                    end ;
                    Increment_Clock( 4800 ) ;
                    PC := Pop ;
                    A := Pop ;
                    if( CF8j and ( ( _PS shr 14 ) = 3 ) ) then // User mode
                    begin
                        A := A and $71F ; // Mask out bits that cannot be changed
                    end ;
                    PS := A ;
                    if( not CF8b ) then
                    begin
                        Inhibit_T_Trap := True ;
                    end ;
                    State_Change_Notice( State_Interrupt, False ) ;
                    exit ;
                end ;
            3 : // BPT
                begin
                    Increment_Clock( 9300 ) ;
                    Trap( O14 ) ;
                    exit ;
                end ;
            4 : // IOT
                begin
                    Increment_Clock( 9300 ) ;
                    Trap( O20 ) ;
                    exit ;
                end ;
            5 : // RESET
                begin
                    if( _Logger <> nil ) then
                    begin
                        _logger.Log( Parent, 'RESET', -1, False, LT_Other ) ;
                    end ;
                    if( CF8j and ( ( _PS shr 14 ) = 3 ) ) then // User mode
                    begin
                        Increment_Clock( 2300 ) ; // A no-op
                        exit ;
                    end ;
                    Increment_Clock( 20000 ) ; // 20 ms for 5/10, 10 ms for 34/70
                    Send_Signal( 'UNIBUS_INIT', True ) ;
                    Send_Signal( 'UNIBUS_INIT', False ) ;
                    exit ;
                end ;
            6 : // RTT
                begin
                    if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                    begin
                        if( _RTS.Return ) then
                        begin
                            exit ;
                        end ;
                    end ;
                    Increment_Clock( 4800 ) ;
                    PC := Pop ;
                    A := Pop ;
                    if( CF8j and ( ( _PS shr 14 ) = 3 ) ) then // User mode
                    begin
                        A := A and $71F ; // Mask out bits that cannot be changed
                    end ;
                    PS := A ;
                    Inhibit_T_Trap := True ;
                    State_Change_Notice( State_Interrupt, False ) ;
                    exit ;
                end ;
        O100..O177 : // JMP
                    begin
                        Increment_Clock( 1200 ) ;
                        if( ( ( A shr 3 ) and 7 ) = 0 ) then // Illegal instruction (JMP to register)
                        begin
                            if( CF5 ) then
                            begin
                                Trap( 4 ) ; // Illegal instruction
                            end else
                            begin
                                Trap( O10 ) ;
                            end ;
                        end else
                        if( ( ( A shr 3 ) and 7 ) = 2 ) then // Autoincrement - special case
                        begin
                            F := A and 7 ;
                            if( CF4 ) then
                            begin
                                PC := Register[ F ] ;
                                Register[ F ] := _Register[ F ] + 2 ; // Increment after use
                            end else
                            begin
                                Register[ F ] := _Register[ F ] + 2 ; // Increment before use
                                PC := Register[ F ] ;
                            end ;
                            if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                            begin
                                _RTS.Jumped ;
                            end ;
                        end else
                        begin
                            Resolve_Destination_Address( A and 63, 2, E, False ) ;
                            if( not E ) then
                            begin
                                PC := Destination_Address ;
                            end ;
                            if( ( _RTS_Flags and RTS_Want_Jumps ) <> 0 ) then
                            begin
                                _RTS.Jumped ;
                            end ;
                        end ;
                        exit ;
                    end ;
            O200..O207 : // RTS
                        begin
                            if( ( _RTS_Flags and RTS_Want_Returns ) <> 0 ) then
                            begin
                                if( _RTS.Return ) then
                                begin
                                    exit ;
                                end ;
                            end ;
                            Increment_Clock( 3500 ) ;
                            PC := Register[ A and 7 ] ;
                            Register[ A and 7 ] := Pop ;
                            exit ;
                        end ;
            O230..O237 : // SPL
                        if( CF8k ) then
                        begin
                            if( ( _PS and $C000 ) = $C000 ) then // Kernel mode
                            begin
                                Increment_Clock( 2600 ) ;
                                A := ( A and 7 ) shl 5 ;
                                PS := ( _PS and ( not $FF1F ) ) or A ;
                            end ;
                            exit ;
                        end ;
            O240..O257 : // CLC/CLV/CLZ/CLN
                        begin
                            Increment_Clock( 1500 ) ;
                            PS := _PS and ( not ( A and 15 ) ) ;
                            exit ;
                        end ;
            O260..O277 : // SEC/SEV/SEZ/SEN
                        begin
                            Increment_Clock( 1500 ) ;
                            PS := _PS or ( A and 15 ) ;
                            exit ;
                        end ;
            O300..O377 : // SWAB
                        begin
                            Increment_Clock( 2300 ) ;
                            Mode := A and 63 ;
                            if( ( ( Mode  shr 3 ) and 7 ) = 0 ) then // Register mode
                            begin
                                F := swap( _Register[ A and 7 ] ) ;
                                Register[ A and 7 ] := F ;
                            end else
                            begin
                                F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                if( not E ) then
                                begin
                                    Word_Write( Destination_Address, swap( F ), E ) ;
                                end ;
                            end ;
                            if( CF6 ) then
                            begin
                                B := _PS and ( not 3 ) ; // Clear C and V
                            end else
                            begin
                                B := _PS and ( not 1 ) ; // Clear C
                            end ;
                            if( ( F and 255 ) = 0 ) then
                            begin
                                B := B or 4 ; // Set Z
                            end else
                            begin
                                B := B and ( not 4 ) ; // Clear Z
                            end ;
                            if( ( F and 127 ) = 127 ) then
                            begin
                                B := B or 8 ; // Set N
                            end else
                            begin
                                B := B and ( not 8 ) ; // Clear N
                            end ;
                            PS := B ;
                            exit ;
                        end ;
            O400..O777 : // BR
                        begin
                            Branch_If( True, A ) ;
                            exit ;
                        end ;
            O1000..O1377 : // BNE
                            begin
                                Branch_If( ( _PS and 4 ) = 0, A ) ;
                                exit ;
                            end ;
            O1400..O1777 : // BEQ
                            begin
                                Branch_If( ( _PS and 4 ) <> 0, A ) ;
                                exit ;
                            end ;
            O2000..O2377 : // BGE
                            begin
                                Branch_If( ( PS_N xor PS_V ) = 0, A ) ;
                                exit ;
                            end ;
            O2400..O2777 : // BLT
                            begin
                                Branch_If( ( PS_N xor PS_V ) <> 0, A ) ;
                                exit ;
                            end ;
            O3000..O3377 : // BGT
                            begin
                                Branch_If( ( ( PS_N xor PS_V ) or PS_Z ) = 0, A ) ;
                                exit ;
                            end ;
            O3400..O3777 : // BLE
                            begin
                                Branch_If( ( ( PS_N xor PS_V ) or PS_Z ) <> 0, A ) ;
                                exit ;
                            end ;
            O4000..O4777 : // JSR
                            begin
                                Increment_Clock( 4400 ) ;
                                if( ( ( A shr 3 ) and 7 ) = 2 ) then // Autoincrement - special case
                                begin
                                    F := A and 7 ;
                                    if( CF4 ) then
                                    begin
                                        F := Register[ F ] ;
                                        Register[ F ] := _Register[ F ] + 2 ; // Increment after use
                                    end else
                                    begin
                                        Register[ F ] := _Register[ F ] + 2 ; // Increment before use
                                        F := Register[ F ] ;
                                    end ;
                                end else
                                begin
                                    Resolve_Destination_Address( A and 63, 2, E, False ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    F := Destination_Address ;
                                end ;
                                B := ( A shr 6 ) and 7 ;
                                if( ( _RTS_Flags and RTS_Want_Calls ) <> 0 ) then
                                begin
                                    if( _RTS.Call( F ) ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;
                                Push( Register[ B ] ) ;
                                Register[ B ] := PC ;
                                PC := F ;
                                if( _SP < O400 ) then
                                begin
                                    Trap( 4 ) ; // Stack overflow
                                end ;
                                exit ;
                            end ; // JSR
            O5000..O5077 : // CLR
                            begin
                                Increment_Clock( 2300 ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Register[ A and 7 ] := 0 ;
                                end else
                                begin
                                    Resolve_Destination_Address( A and 63, 2, E, False ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    Word_Write( Destination_Address, 0, E ) ;
                                end ;
                                PS := ( _PS and ( not $F ) ) or 4 ; // Clear NVC, set Z
                                exit ;
                            end ;
            O5100..O5177 : // COM
                            begin
                                Increment_Clock( 2300 ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := not _Register[ A and 7 ] ;
                                    Register[ A and 7 ] := F ;
                                end else
                                begin
                                    F := not Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    Word_Write( Destination_Address, F, E ) ;
                                end ;
                                B := _PS and ( not $F ) or 1 ; // Clear ZVN, set C
                                if( ( F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F, B ) ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O5200..O5277 : // INC
                            begin
                                Increment_Clock( 2300 ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    Register[ A and 7 ] := F + 1 ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    Word_Write( Destination_Address, F + 1, E ) ;
                                end ;
                                B := _PS and ( not $E ) ; // Clear ZVN
                                if( ( ( F + 1 ) and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F + 1, B ) ;
                                end ;
                                if( F = 32767 ) then
                                begin
                                    B := B or 2 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O5300..O5377 : // DEC
                            begin
                                Increment_Clock( 2300 ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    Register[ A and 7 ] := F - 1 ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    Word_Write( Destination_Address, F - 1, E ) ;
                                end ;
                                B := _PS and ( not $E ) ; // Clear ZVN
                                if( ( ( F - 1 ) and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F - 1, B ) ;
                                end ;
                                if( word( F ) = 32768 ) then
                                begin
                                    B := B or 2 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O5400..O5477 : // NEG
                            begin
                                Increment_Clock( 2300 ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    if( word( F ) <> 32768 ) then
                                    begin
                                        Register[ A and 7 ] := -F ;
                                    end ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    if( word( F ) <> 32768 ) then
                                    begin
                                        Word_Write( Destination_Address, ( ( -F ) and $FFFF ), E ) ;
                                    end ;
                                end ;
                                B := _PS and ( not $F ) ; // Clear ZVNC
                                if( ( -F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    B := B or 1 ;
                                end ;
                                if( word( -F ) > 32767 ) then
                                begin
                                    B := B or 8 ;
                                end ;
                                if( word( -F ) = 32768 ) then
                                begin
                                    B := B or 2 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ; // NEG
            O5500..O5577 : // ADC
                            begin
                                Increment_Clock( 2300 ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    Register[ A and 7 ] := F + ( _PS and 1 ) ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    Word_Write( Destination_Address, F + ( _PS and 1 ), E ) ;
                                end ;
                                B := _PS and ( not $F ) ; // Clear ZVNC
                                if( ( ( F + ( _PS and 1 ) ) and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F + ( _PS and 1 ), B ) ;
                                end ;
                                if( ( _PS and 1 ) = 1 ) then
                                begin
                                    if( F = 32767 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    if( F = 65535 ) then
                                    begin
                                        B := B or 1 ;
                                    end ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O5600..O5677 : // SBC
                            begin
                                Increment_Clock( 2300 ) ;
                                C := _PS and 1 ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    Register[ A and 7 ] := F - C ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    Word_Write( Destination_Address, F - C, E ) ;
                                end ;
                                B := _PS and ( not $F ) ; // Clear ZVNC
                                if( ( ( F - C ) and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F - C, B ) ;
                                end ;
                                if( word( F - C ) = 32768 ) then
                                begin
                                    B := B or 2 ;
                                end ;
                                if( ( C <> 1 ) or ( F <> 1 ) ) then // Clear C is C is set and result is 0
                                begin
                                    B := B or 1 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O5700..O5777 : // TST
                            begin
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Increment_Clock( 2300 ) ;
                                    F := _Register[ A and 7 ] ;
                                end else
                                begin
                                    Increment_Clock( 1800 - 500 ) ;
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;
                                B := _PS and ( not $F ) ; // Clear ZVNC
                                if( ( F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F, B ) ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O6000..O6077 : // ROR
                            begin
                                Increment_Clock( 2300 ) ;
                                C := _PS and 1 ; // Save C
                                B := _PS and ( not $F ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    B := B or ( F and 1 ) ;
                                    F := F shr 1 ;
                                    if( C <> 0 ) then
                                    begin
                                        F := F or $8000 ;
                                    end ;
                                    Register[ A and 7 ] := F ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    B := B or ( F and 1 ) ;
                                    F := F shr 1 ;
                                    if( C <> 0 ) then
                                    begin
                                        F := F or $8000 ;
                                    end ;
                                    Word_Write( Destination_Address, F, E ) ;
                                end ;
                                if( ( F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F, B ) ;
                                end ;
                                C := ( B xor ( B shr 3 ) ) and 1 ;
                                if( C <> 0 ) then
                                begin
                                    B := B or 2 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ; // ROR
            O6100..O6177 : // ROL
                            begin
                                Increment_Clock( 2300 ) ;
                                C := _PS and 1 ; // Save C
                                B := _PS and ( not $F ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    if( ( F and 32768 ) <> 0 ) then
                                    begin
                                        B := B or 1 ;
                                    end ;
                                    F := ( F shl 1 ) or C ;
                                    Register[ A and 7 ] := F ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    if( ( F and 32768 ) <> 0 ) then
                                    begin
                                        B := B or 1 ;
                                    end ;
                                    F := ( F shl 1 ) or C ;
                                    Word_Write( Destination_Address, F, E ) ;
                                end ;
                                if( ( F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F, B ) ;
                                end ;
                                C := ( B xor ( B shr 3 ) ) and 1 ;
                                if( C <> 0 ) then
                                begin
                                    B := B or 2 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O6200..O6277 : // ASR
                            begin
                                Increment_Clock( 2300 ) ;
                                B := _PS and ( not $F ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    B := B or ( F and 1 ) ;
                                    F := F shr 1 ;
                                    if( F > 16383 ) then
                                    begin
                                        F := F or $8000 ; // Replicate high bit (preserve sign)
                                    end ;
                                    Register[ A and 7 ] := F ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    B := B or ( F and 1 ) ;
                                    F := F shr 1 ;
                                    if( F > 16383 ) then
                                    begin
                                        F := F or $8000 ; // Replicate high bit (preserve sign)
                                    end ;
                                    Word_Write( Destination_Address, F, E ) ;
                                end ;
                                if( ( F and $FFFF )  = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F, B ) ;
                                end ;
                                C := ( B xor ( B shr 3 ) ) and 1 ;
                                if( C <> 0 ) then
                                begin
                                    B := B or 2 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ; // ASR
            O6300..O6377 : // ASL
                            begin
                                Increment_Clock( 2300 ) ;
                                B := _PS and ( not $F ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    F := _Register[ A and 7 ] ;
                                    if( ( F and 32768 ) <> 0 ) then
                                    begin
                                        B := B or 1 ;
                                    end ;
                                    F := F shl 1 ;
                                    Register[ A and 7 ] := F ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    if( ( F and 32768 ) <> 0 ) then
                                    begin
                                        B := B or 1 ;
                                    end ;
                                    F := F shl 1 ;
                                    Word_Write( Destination_Address, F, E ) ;
                                end ;
                                if( ( F and $FFFF )  = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F, B ) ;
                                end ;
                                C := ( B xor ( B shr 3 ) ) and 1 ;
                                if( C <> 0 ) then
                                begin
                                    B := B or 2 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ; // ASL
            O6500..O6577 : // MFPI
                            if( CF8j ) then
                            begin
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    if( ( A and 7 ) = 6 ) then // R6
                                    begin
                                        F := _R6[ ( _PS shr 12 ) and 3 ] ;
                                    end else
                                    begin
                                        F := _Register[ A and 7 ] ;
                                    end ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    F := MMU.Relocate( F, PS shl 2, True, E, False ) ;
                                    if( E ) then
                                    begin
                                        Trap( O250 ) ;
                                        exit ;
                                    end ;
                                    F := Word_Read( F, E, False ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                B := _PS and $FFF1 ;
                                if( ( F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end ;
                                Set_N_Bit( F, B ) ;
                                PS := B ;

                                Push( F ) ;
                                exit ;
                            end ; // MFPI
            O6600..O6677 : // MTPI
                            if( CF8j ) then
                            begin
                                F := Pop ;
                                B := _PS and $FFF1 ; // Clear all flags but C
                                if( ( F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end ;
                                Set_N_Bit( F, B ) ;
                                B := _PS and ( not $F ) ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    if( ( A and 7 ) = 6 ) then // R6
                                    begin
                                        _R6[ ( _PS shr 12 ) and 3 ] := Pop ;
                                    end else
                                    begin
                                        _Register[ A and 7 ] := F ;
                                    end ;
                                end else
                                begin
                                    C := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    Destination_Address := MMU.Relocate( C, PS shl 2, True, E, False ) ;
                                    if( E ) then
                                    begin
                                        Trap( O250 ) ;
                                        exit ;
                                    end ;
                                    Word_Write( Destination_Address, F, E, False ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O6700..O6777 : // SXT
                            if( CF8b ) then
                            begin
                                Increment_Clock( 2300 ) ;
                                if( ( _PS and 8 ) = 0 ) then
                                begin
                                    Src := 0 ;
                                end else
                                begin
                                    Src := 65535 ;
                                end ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Register[ A and 7 ] := Src ;
                                end else
                                begin
                                    F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    Word_Write( Destination_Address, Src, E ) ;
                                end ;
                                B := _PS and ( not 6 ) ; // Clear ZV
                                if( ( _PS and 8 ) = 0 ) then
                                begin
                                    B := B or 4 ; // Set Z
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O10000..O17777 : // MOV
                            begin
                                Increment_Clock( 2300 ) ;
                                B := _PS and ( not $E ) ; // Clear VZN

                                // Get source...
                                Resolve_Addresses( A, 2, E, F, Dst, True, False ) ;
                                if( E ) then
                                begin
                                    exit ;
                                end ;
                                if( ( F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( F, B ) ;
                                end ;
                                PS := B ;

                                // Set destination...
                                if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Register[ A and 7 ] := F ;
                                end else
                                begin
                                    Word_Write( Destination_Address, F, E ) ;
                                end ;
                                exit ;
                            end ;
            O20000..O27777 : // CMP
                            begin
                                // Get source...
                                if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Src := _Register[ ( A shr 6 ) and 7 ] ;
                                end else
                                begin
                                    Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                // Get destination...
                                if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Dst := _Register[ A and 7 ] ;
                                    Increment_Clock( 2300 ) ;
                                end else
                                begin
                                    Increment_Clock( 2300 - 500 ) ;
                                    Dst := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                Sub( Src, Dst ) ;
                                exit ;
                            end ;
            O30000..O37777 : // BIT
                            begin
                                // Get source...
                                if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Src := _Register[ ( A shr 6 ) and 7 ] ;
                                end else
                                begin
                                    Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                // Get destination...
                                if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Increment_Clock( 2900 ) ;
                                    Dst := _Register[ A and 7 ] ;
                                end else
                                begin
                                    Increment_Clock( 2900 - 500 ) ;
                                    Dst := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                Dst := Src and Dst ;

                                B := _PS and ( not $E ) ;
                                if( Dst = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                if( Dst > 32767 ) then // High-order bit of result set
                                begin
                                    B := B or 8 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O40000..O47777 : // BIC
                            begin
                                Increment_Clock( 2900 ) ;
                                // Get source...
                                if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Src := _Register[ ( A shr 6 ) and 7 ] ;
                                end else
                                begin
                                    Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                // Get destination...
                                if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Dst := _Register[ A and 7 ] ;
                                end else
                                begin
                                    Dst := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                Dst := ( not Src ) and Dst ;

                                B := _PS and ( not $E ) ;
                                if( Dst = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                if( Dst > 32767 ) then // High-order bit of result set
                                begin
                                    B := B or 8 ;
                                end ;
                                PS := B ;

                                // Set destination...
                                if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    _Register[ A and 7 ] := Dst ;
                                end else
                                begin
                                    Word_Write( Destination_Address, Dst, E ) ;
                                end ;
                                exit ;
                            end ;
            O50000..O57777 : // BIS
                            begin
                                Increment_Clock( 2300 ) ;
                                // Get source...
                                if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Src := _Register[ ( A shr 6 ) and 7 ] ;
                                end else
                                begin
                                    Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                // Get destination...
                                if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Dst := _Register[ A and 7 ] ;
                                end else
                                begin
                                    Dst := Resolve_Destination_Address( A and 63, 2, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                Dst := Src or Dst ;

                                B := _PS and $FFF1 ; // Clear N, Z. V
                                if( Dst = 0 ) then
                                begin
                                    B := B or 4 ; // Z
                                end else
                                if( Dst > 32767 ) then // High-order bit of result set
                                begin
                                    B := B or 8 ; // N
                                end ;
                                PS := B ;

                                // Set destination...
                                if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    _Register[ A and 7 ] := Dst ;
                                end else
                                begin
                                    Word_Write( Destination_Address, Dst, E ) ;
                                end ;
                                exit ;
                            end ;
            O60000..O67777 : // ADD
                            begin
                                Increment_Clock( 2300 ) ;
                                // Get source and destination...
                                Resolve_Addresses( A, 2, E, Src, Dst, True, True ) ;
                                if( E ) then
                                begin
                                    exit ;
                                end ;

                                F := Dst + Src ;

                                // Set destination...
                                if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Register[ A and 7 ] := F ;
                                end else
                                begin
                                    Word_Write( Destination_Address, F, E ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                end ;

                                // Set flags...
                                B := _PS and ( not $F ) ;
                                if( ( F and $FFFF ) = 0 ) then
                                begin
                                    B := B or 4 ; // Z
                                end else
                                begin
                                    Set_N_Bit( F, B ) ;
                                end ;
                                if( F > 65535 ) then
                                begin
                                    B := B or 1 ; // C
                                end ;
                                if(
                                    ( ( Dst and $8000 ) = ( Src and $8000 ) ) // Src and Dst have the same sign...
                                    and
                                    ( ( F and $8000 ) <> ( Src and $8000 ) ) // ...and result was a different sign
                                  ) then
                                begin
                                    B := B or 2 ; // V
                                end ;
                                PS := B ;
                                exit ;
                            end ; // ADD
            O70000..O70777 : // MUL
                            if( EIS ) then
                            begin
                                F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                if( E ) then
                                begin
                                    exit ;
                                end ;
                                Increment_Clock( 8820 + 200 * Bit_Count( F ) ) ; // 11/34 timing
                                B := ( A shr 6 ) and 7 ;
                                I := _Register[ B ] ;
                                if( F > 32767 ) then
                                begin
                                    F := F - 65536 ;
                                end ;
                                if( I > 32767 ) then
                                begin
                                    I := I - 65536 ;
                                end ;
                                IP := I ;
                                IP := IP * F ;
                                Register[ B ] := IP shr 16 ;
                                Register[ B or 1 ] := IP ;

                                // Set flags...
                                B := _PS and $FFF0 ; // Clear NZVC
                                if( IP = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( IP, B ) ;
                                end ;
                                if( ( IP < -32768 ) or ( IP > 32767 ) ) then
                                begin
                                    B := B or 1 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O71000..O71777 : // DIV
                            if( EIS ) then
                            begin
                                F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                if( E ) then
                                begin
                                    exit ;
                                end ;
                                if( F = 0 ) then // Divide by zero
                                begin
                                    PS := _PS or 3 ; //TODO:Question: are other bits set on divide-by-zero?
                                    Increment_Clock( 2780 ) ; // 11/34 timing
                                    exit ;
                                end ;
                                B := ( A shr 6 ) and 7 ;
                                //TODO:Question: Documentation says R must be even.  What happens if it is not?
                                I := _Register[ B ] ;
                                I := ( I shl 16 ) or _Register[ B or 1 ] ;
                                if( F > 32767 ) then
                                begin
                                    F := F - 65536 ;
                                end ;
                                IP1 := I div F ;
                                IP := I mod F ;
                                if( ( F < 0 ) and ( IP > 0 ) ) then
                                begin
                                    IP := -IP ;
                                end ;
                                Register[ B ] := IP1 ;
                                Register[ B or 1 ] := IP ;
                                B := _PS and $FFF0 ; // Clear flags
                                if( IP1 = 0 ) then
                                begin
                                    B := B or 4 ;
                                end ;
                                if( ( IP1 < -32768 ) or ( IP1 > 32767 ) ) then
                                begin
                                    B := B or 2 ;
                                    Increment_Clock( 2780 ) ; // 11/34 timing
                                end else
                                begin
                                    Increment_Clock( 12480 ) ; // 11/34 timing
                                end ;
                                if( ( IP1 and $8000 ) <> 0 ) then
                                begin
                                    B := B or 8 ;
                                end ;
                                PS := B ;
                                exit ;
                            end ;
            O72000..O72777 : // ASH
                            if( EIS ) then
                            begin
                                F := Resolve_Destination_Address( A and 63, 2, E ) and 63 ;
                                if( E ) then
                                begin
                                    exit ;
                                end ;
                                Increment_Clock( ( F and 31 ) * 200 + 4180 ) ; // 11/34 timing
                                B := ( A shr 6 ) and 7 ;
                                IP := _Register[ B ] ;
                                Temp_High := IP and $8000 ; // Save high bit
                                if( F > 31 ) then // Negative value
                                begin
                                    F := F - 64 ;
                                end ;
                                C := 0 ; // Carry
                                V := 0 ; // Overflow
                                if( F >= 0 ) then // Shift left
                                begin
                                    F := F and 63 ;
                                    while( F > 0 ) do
                                    begin
                                        dec( F ) ;
                                        if(
                                            ( ( IP and $8000 ) = 0 ) <> ( ( IP and $4000 ) = 0 )
                                          ) then // Bit 31 changed (Bit 15 <> bit 14)
                                        begin
                                            V := 2 ; // Overflow
                                        end ;
                                        if( ( IP and $8000 ) <> 0 ) then // Carry
                                        begin
                                            C := 1 ;
                                        end else
                                        begin
                                            C := 0 ;
                                        end ;
                                        IP := IP shl 1 ;
                                    end ;
                                end else
                                begin
                                    F := ( -F ) and 63 ;
                                    while( F > 0 ) do
                                    begin
                                        dec( F ) ;
                                        C := IP and 1 ;
                                        IP := IP shr 1 ;
                                        IP := ( IP and $7FFF ) or Temp_High ; // Sign extend
                                    end ;
                                end ;
                                IP := IP and $FFFF ;
                                Register[ B ] := IP ;
                                B := 0 ;
                                if( IP = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                begin
                                    Set_N_Bit( IP, B ) ;
                                end ;
                                PS := ( _PS and $FFF0 ) or C or V or B ;
                                exit ;
                            end ;
            O73000..O73777 : // ASHC
                            if( EIS ) then
                            begin
                                F := Resolve_Destination_Address( A and 63, 2, E ) and 63 ;
                                if( E ) then
                                begin
                                    exit ;
                                end ;
                                Increment_Clock( ( F and 31 ) * 200 + 4180 ) ; // 11/34 timing
                                B := ( A shr 6 ) and 7 ;
                                IP := _Register[ B ] ;
                                IP := ( IP shl 16 ) or _Register[ B or 1 ] ;
                                Temp_High := IP and $80000000 ; // Save high bit
                                if( F > 31 ) then // Negative value
                                begin
                                    F := F - 64 ;
                                end ;
                                C := 0 ; // Carry
                                V := 0 ; // Overflow
                                if( F >= 0 ) then // Shift left
                                begin
                                    F := F and 63 ;
                                    while( F > 0 ) do
                                    begin
                                        dec( F ) ;
                                        if(
                                            ( ( IP and $80000000 ) = 0 ) <> ( ( IP and $40000000 ) = 0 )
                                          ) then // Bit 31 changed (Bit 31 <> bit 30)
                                        begin
                                            V := 2 ; // Overflow
                                        end ;
                                        if( ( IP and $80000000 ) <> 0 ) then // Carry
                                        begin
                                            C := 1 ;
                                        end else
                                        begin
                                            C := 0 ;
                                        end ;
                                        IP := IP shl 1 ;
                                    end ; // while( F > 0 )
                                end else
                                begin
                                    F := ( -F ) and 63 ;
                                    while( F > 0 ) do
                                    begin
                                        dec( F ) ;
                                        C := IP and 1 ;
                                        IP := IP shr 1 ;
                                        IP := ( IP and $7FFFFFFF ) or Temp_High ; // Sign extend
                                    end ;
                                end ;
                                IP := IP and $FFFFFFFF ;
                                Register[ B or 1 ] := IP ;
                                Register[ B ] := IP shr 16 ;
                                B := 0 ;
                                if( IP = 0 ) then
                                begin
                                    B := B or 4 ;
                                end else
                                if( ( IP and $80000000 ) <> 0 ) then
                                begin
                                    B := B or 8 ;
                                end ;
                                PS := ( _PS and $FFF0 ) or C or V or B ;
                                exit ;
                            end ;
            O74000..O74777 : // XOR
                            if( EIS ) then
                            begin
                                Increment_Clock( 2900 ) ;
                                F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                if( E ) then
                                begin
                                    exit ;
                                end ;
                                B := ( A shr 6 ) and 7 ;
                                B := _Register[ B ] ;
                                F := F xor B ;
                                B := _PS and $FFF1 ; // Clear NZV
                                if( F = 0 ) then
                                begin
                                    B := B or 4 ;
                                end ;
                                Set_N_Bit( F, B ) ;
                                PS := B ;
                                if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                begin
                                    Register[ A and 7 ] := F ;
                                end else
                                begin
                                    Word_Write( Destination_Address, F, E ) ;
                                end ;
                                exit ;
                            end ;
            O77000..O77777 : // SOB
                            if( CF8b ) then
                            begin
                                Src := ( A shr 6 ) and 7 ; // Register
                                Dst := ( A and 63 ) * 2 ;
                                Register[ Src ] := _Register[ Src ] - 1 ;
                                if( _Register[ Src ] = 0 ) then
                                begin
                                    Increment_Clock( 3250 ) ;
                                end else
                                begin
                                    Increment_Clock( 2600 ) ;
                                    PC := _PC - Dst ;
                                end ;
                                exit ;
                            end ;
            O100000..O100377 : // BPL
                                begin
                                    Branch_If( ( _PS and 8 ) = 0, A ) ;
                                    exit ;
                                end ;
            O100400..O100777 : // BMI
                                begin
                                    Branch_If( ( _PS and 8 ) <> 0, A ) ;
                                    exit ;
                                end ;
            O101000..O101377 : // BHI
                                begin
                                    Branch_If( ( PS_C = 0 ) and ( PS_Z = 0 ), A ) ;
                                    exit ;
                                end ;
            O101400..O101777 : // BLOS
                                begin
                                    Branch_If( ( PS_Z xor PS_C ) <> 0, A ) ;
                                    exit ;
                                end ;
            O102000..O102377 : // BVC
                                begin
                                    Branch_If( ( _PS and 2 ) = 0, A ) ;
                                    exit ;
                                end ;
            O102400..O102777 : // BVS
                                begin
                                    Branch_If( ( _PS and 2 ) <> 0, A ) ;
                                    exit ;
                                end ;
            O103000..O103377 : // BCC (BHIS)
                                begin
                                    Branch_If( ( _PS and 1 ) = 0, A ) ;
                                    exit ;
                                end ;
            O103400..O103777 : // BCS (BLO)
                                begin
                                    Branch_If( ( _PS and 1 ) <> 0, A ) ;
                                    exit ;
                                end ;
            O104000..O104377 : // EMT
                                begin
                                    Increment_Clock( 9300 ) ;
                                    Trap( O30 ) ;
                                    exit ;
                                end ;
            O104400..O104777 : // TRAP
                                begin
                                    Increment_Clock( 2250 ) ;
                                    Trap( O34 ) ;
                                    exit ;
                                end ;
            O105000..O105077 : // CLRB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Register_Low[ A and 7 ] := 0 ;
                                    end else
                                    begin
                                        B := Resolve_Destination_Address( A and 63, 1, E, False ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        Byte_Write( B, 0, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    PS := ( _PS and ( not $F ) ) or 4 ; // Clear NVC, set Z
                                    exit ;
                                end ;
            O105100..O105177 : // COMB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        F := not F ;
                                        Register_Low[ A and 7 ] := F ;
                                    end else
                                    begin
                                        F := not Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        Byte_Write( Destination_Address, F and 255, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    B := _PS and ( not $F ) or 1 ; // Clear ZVN, set C
                                    if( ( F and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( ( F < 0 ) or ( F > 127 ) ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O105200..O105277 : // INCB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        Register_Low[ A and 7 ] := F + 1 ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        Byte_Write( Destination_Address, ( F + 1 ) and 255, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    B := _PS and ( not $E ) ; // Clear ZVN
                                    if( ( ( F + 1 ) and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( F + 1 > 127 ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    if( F = 127 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O105300..O105377 : // DECB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        Register_Low[ A and 7 ] := F - 1 ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        Byte_Write( Destination_Address, F - 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    B := _PS and ( not $E ) ; // Clear ZVN
                                    if( ( ( F - 1 ) and $FF ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( ( F - 1 < 0 ) or ( F > 128 ) ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    if( byte( F ) = 128 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ; // DECB
            O105400..O105477 : // NEGB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        if( byte( F ) <> 128 ) then
                                        begin
                                            Register_Low[ A and 7 ] := -F ;
                                        end ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        if( byte( F ) <> 128 ) then
                                        begin
                                            Byte_Write( Destination_Address, -F, E ) ;
                                            if( E ) then
                                            begin
                                                exit ;
                                            end ;
                                        end ;
                                    end ;
                                    B := _PS and ( not $F ) ; // Clear ZVNC
                                    if( -F = 0 ) then
                                    begin
                                        B := B or 5 ;
                                    end else
                                    if( byte( -F ) > 127 ) then // Negative
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    if( byte( -F ) = 128 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ; // NEGB
            O105500..O105577 : // ADCB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        Register_Low[ A and 7 ] := F + ( _PS and 1 ) ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        Byte_Write( Destination_Address, F + ( _PS and 1 ), E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    B := _PS and ( not $F ) ; // Clear ZVNC
                                    if( ( ( F + ( _PS and 1 ) ) and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( shortint( F + ( _PS and 1 ) ) < 0 ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    if( ( _PS and 1 ) = 1 ) then
                                    begin
                                        if( F = 127 ) then
                                        begin
                                            B := B or 2 ;
                                        end ;
                                        if( F = 255 ) then
                                        begin
                                            B := B or 1 ;
                                        end ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O105600..O105677 : // SBCB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    C := _PS and 1 ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        Register_Low[ A and 7 ] := F - C ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        Byte_Write( Destination_Address, F - C, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    B := _PS and ( not $F ) ; // Clear ZVNC
                                    if( ( ( F - C ) and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( ( F - C < 0 ) or ( F - C > 127 ) ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    if( byte( F - C ) = 128 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    if( ( C = 0 ) or ( F <> 1 ) ) then
                                    begin
                                        B := B or 1 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ; // SBCB
            O105700..O105777 : // TSTB
                                begin
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        Increment_Clock( 2300 ) ;
                                    end else
                                    begin
                                        Increment_Clock( 1800 ) ;
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    B := _PS and ( not $F ) ; // Clear ZVNC
                                    if( ( F and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( shortint( F ) < 0 ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ; // TSTB
            O106000..O106077 : // RORB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    C := _PS and 1 ; // Save C
                                    B := _PS and ( not $F ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        B := B or ( F and 1 ) ;
                                        F := F shr 1 ;
                                        if( C <> 0 ) then
                                        begin
                                            F := F or $80 ;
                                        end ;
                                        Register_Low[ A and 7 ] := F ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        B := B or ( F and 1 ) ;
                                        F := F shr 1 ;
                                        if( C <> 0 ) then
                                        begin
                                            F := F or $80 ;
                                        end ;
                                        Byte_Write( Destination_Address, F, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        if( ( Destination_Address and 1 ) = 1 ) then // Odd address
                                        begin
                                            Increment_Clock( 1200 ) ;
                                        end ;
                                    end ;
                                    if( ( F and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( shortint( F ) < 0 ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    C := ( B xor ( B shr 3 ) ) and 1 ;
                                    if( C <> 0 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O106100..O106177 : // ROLB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    C := _PS and 1 ; // Save C
                                    B := _PS and ( not $F ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        if( ( F and 128 ) <> 0 ) then
                                        begin
                                            B := B or 1 ;
                                        end ;
                                        F := ( F shl 1 ) or C ;
                                        Register_Low[ A and 7 ] := F ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        if( ( F and 128 ) <> 0 ) then
                                        begin
                                            B := B or 1 ;
                                        end ;
                                        F := ( F shl 1 ) or C ;
                                        Byte_Write( Destination_Address, F, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        if( ( Destination_Address and 1 ) = 1 ) then // Odd address
                                        begin
                                            Increment_Clock( 1200 ) ;
                                        end ;
                                    end ;
                                    if( ( F and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( shortint( F ) < 0 ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    C := ( B xor ( B shr 3 ) ) and 1 ;
                                    if( C <> 0 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O106200..O106277 : // ASRB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    B := _PS and ( not $F ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        B := B or ( F and 1 ) ;
                                        F := F shr 1 ;
                                        if( F > 63 ) then
                                        begin
                                            F := F or $80 ; // Replicate high bit (preserve sign)
                                        end ;
                                        Register_Low[ A and 7 ] := F ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        B := B or ( F and 1 ) ;
                                        F := F shr 1 ;
                                        if( F > 63 ) then
                                        begin
                                            F := F or $80 ; // Replicate high bit (preserve sign)
                                        end ;
                                        Byte_Write( Destination_Address, F, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        if( ( Destination_Address and 1 ) = 1 ) then
                                        begin
                                            Increment_Clock( 1200 ) ;
                                        end ;
                                    end ;
                                    if( ( F and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( shortint( F ) < 0 ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    C := ( B xor ( B shr 3 ) ) and 1 ;
                                    if( C <> 0 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O106300..O106377 : // ASLB
                                begin
                                    B := _PS and ( not $F ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        if( ( F and 128 ) <> 0 ) then
                                        begin
                                            B := B or 1 ;
                                        end ;
                                        F := F shl 1 ;
                                        Register_Low[ A and 7 ] := F ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        if( ( F and 128 ) <> 0 ) then
                                        begin
                                            B := B or 1 ;
                                        end ;
                                        F := F shl 1 ;
                                        Byte_Write( Destination_Address, F, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        if( ( Destination_Address and 1 ) = 1 ) then
                                        begin
                                            Increment_Clock( 1200 ) ;
                                        end ;
                                    end ;
                                    if( ( F and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( shortint( F ) < 0 ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    C := ( B xor ( B shr 3 ) ) and 1 ;
                                    if( C <> 0 ) then
                                    begin
                                        B := B or 2 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O106500..O106577 : // MFPD
                                if( CF8j ) then
                                begin
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        if( ( A and 7 ) = 6 ) then // R6
                                        begin
                                            F := _R6[ ( _PS shr 12 ) and 3 ] ;
                                        end else
                                        begin
                                            F := _Register[ A and 7 ] ;
                                        end ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 2, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        F := MMU.Relocate( F, PS shl 2, True, E, True ) ;
                                        if( E ) then
                                        begin
                                            Trap( O250 ) ;
                                            exit ;
                                        end ;
                                        F := Word_Read( F, E, False ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    B := _PS and $FFF1 ;
                                    if( ( F and $FFFF )  = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end ;
                                    Set_N_Bit( F, B ) ;
                                    PS := B ;

                                    Push( F ) ;
                                    exit ;
                                end ; // MFPD
            O106600..O106677 : // MTPD
                                if( CF8j ) then
                                begin
                                    F := Pop ;
                                    B := _PS and $FFF1 ; // Clear all flags but C
                                    if( ( F and $FFFF ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end ;
                                    Set_N_Bit( F, B ) ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        if( ( A and 7 ) = 6 ) then // R6
                                        begin
                                            _R6[ ( _PS shr 12 ) and 3 ] := F ;
                                        end else
                                        begin
                                            _Register[ A and 7 ] := F ;
                                        end ;
                                    end else
                                    begin
                                        C := Resolve_Destination_Address( A and 63, 2, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        Destination_Address := MMU.Relocate( C, PS shl 2, True, E, True ) ;
                                        if( E ) then
                                        begin
                                            Trap( O250 ) ;
                                            exit ;
                                        end ;
                                        Word_Write( Destination_Address, F, E, False ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O110000..O117777 : // MOVB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    B := _PS and ( not $E ) ; // Clear VZN
                                    Resolve_Addresses( A, 1, E, F, Dst, True, False ) ;
                                    if( E ) then
                                    begin
                                        exit ;
                                    end ;
                                    F := F and 255 ;
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode for destination
                                    begin
                                        if( F > 127 ) then
                                        begin
                                            F := $FF00 or F ; // Sign extend
                                        end ;
                                    end ;

                                    // Set flags...
                                    if( ( F and 255 ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( shortint( F ) < 0 ) then
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    PS := B ;

                                    // Set destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        _Register[ A and 7 ] := F ;
                                    end else
                                    begin
                                        Byte_Write( Destination_Address, F, E ) ;
                                    end ;
                                    exit ;
                                end ;
            O120000..O127777 : // CMPB
                                begin
                                    // Get source...
                                    if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Src := _Register[ ( A shr 6 ) and 7 ] ;
                                    end else
                                    begin
                                        Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    // Get destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Increment_Clock( 2300 ) ;
                                        Dst := _Register_Low[ A and 7 ] ;
                                    end else
                                    begin
                                        Increment_Clock( 2300 - 500 ) ;
                                        Dst := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    Subb( Src, Dst ) ;
                                    exit ;
                                end ;
            O130000..O137777 : // BITB
                                begin
                                    // Get source...
                                    if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Src := _Register_Low[ ( A shr 6 ) and 7 ] ;
                                    end else
                                    begin
                                        Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    // Get destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Increment_Clock( 2900 ) ;
                                        Dst := _Register_Low[ A and 7 ] ;
                                    end else
                                    begin
                                        Increment_Clock( 2900 - 500 ) ;
                                        Dst := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    Dst := Src and Dst ;

                                    B := _PS and ( not $E ) ;
                                    if( Dst = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( Dst > 127 ) then // High-order bit of result set
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O140000..O147777 : // BICB
                                begin
                                    Increment_Clock( 2900 ) ;
                                    // Get source...
                                    if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Src := _Register_Low[ ( A shr 6 ) and 7 ] ;
                                    end else
                                    begin
                                        Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    // Get destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Dst := _Register_Low[ A and 7 ] ;
                                    end else
                                    begin
                                        Dst := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    Dst := ( not Src )and Dst ;

                                    B := _PS and ( not $E ) ;
                                    if( Dst = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end else
                                    if( Dst > 127 ) then // High-order bit of result set
                                    begin
                                        B := B or 8 ;
                                    end ;
                                    PS := B ;

                                    // Set destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        _Register_Low[ A and 7 ] := Dst ;
                                    end else
                                    begin
                                        Byte_Write( Destination_Address, Dst, E ) ;
                                    end ;
                                    exit ;
                                end ;
            O150000..O157777 : // BISB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    // Get source...
                                    if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Src := _Register_Low[ ( A shr 6 ) and 7 ] ;
                                    end else
                                    begin
                                        Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    // Get destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Dst := _Register_Low[ A and 7 ] ;
                                    end else
                                    begin
                                        Dst := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    Dst := Src or Dst ;

                                    B := _PS and $FFF1 ; // Clear N, Z. V
                                    if( Dst = 0 ) then
                                    begin
                                        B := B or 4 ; // Z
                                    end else
                                    if( Dst > 127 ) then // High-order bit of result set
                                    begin
                                        B := B or 8 ; // N
                                    end ;
                                    PS := B ;

                                    // Set destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        _Register_Low[ A and 7 ] := Dst ;
                                    end else
                                    begin
                                        Byte_Write( Destination_Address, Dst, E ) ;
                                    end ;
                                    exit ;
                                end ;
            O160000..O167777 : // SUB
                                begin
                                    Increment_Clock( 2300 ) ;
                                    // Get source...
                                    if( ( ( A shr 9 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Src := _Register[ ( A shr 6 ) and 7 ] ;
                                    end else
                                    begin
                                        Src := Resolve_Destination_Address( ( A shr 6 ) and 63, 2, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    // Get destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Dst := _Register[ A and 7 ] ;
                                    end else
                                    begin
                                        Dst := Resolve_Destination_Address( A and 63, 2, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    F := Sub( Dst, Src ) ;

                                    // Set destination...
                                    if( ( ( A shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        Register[ A and 7 ] := F ;
                                    end else
                                    begin
                                        Word_Write( Destination_Address, F, E ) ;
                                    end ;
                                    exit ;
                                end ;
            O106700..O106777 : // MFPS
                                if( CF18b ) then
                                begin
                                    B := _PS and $FF ;
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        if( B > 127 ) then
                                        begin
                                            B := B or $FF00 ;
                                        end ;
                                        Register[ A and 7 ] := B ;
                                    end else
                                    begin
                                        F := Resolve_Destination_Address( A and 63, 1, E, False ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                        Byte_Write( Destination_Address, B, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;
                                    B := PS and ( not 2 ) ; // Clear V
                                    if( ( B and $FF ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end ;
                                    if( ( B and 128 ) = 128 ) then
                                    begin
                                        B := B or 8 ;
                                    end else
                                    begin
                                        B := B and ( not 8 ) ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O106400..O106477 : // MTPS
                                if( CF18b ) then
                                begin
                                    if( ( ( A  shr 3 ) and 7 ) = 0 ) then // Register mode
                                    begin
                                        F := _Register_Low[ A and 7 ] ;
                                        Increment_Clock( 2300 ) ;
                                    end else
                                    begin
                                        Increment_Clock( 1800 ) ;
                                        F := Resolve_Destination_Address( A and 63, 1, E ) ;
                                        if( E ) then
                                        begin
                                            exit ;
                                        end ;
                                    end ;

                                    // Preserve T bit...
                                    F := F and ( not 16 ) ;
                                    F := F or ( _PS and 16 ) ;

                                    // Set flags...
                                    _PS := F ;
                                    B := _PS and ( not 2 ) ; // Clear V
                                    if( ( B and $FF ) = 0 ) then
                                    begin
                                        B := B or 4 ;
                                    end ;
                                    if( ( B and 128 ) = 128 ) then
                                    begin
                                        B := B or 8 ;
                                    end else
                                    begin
                                        B := B and ( not 8 ) ;
                                    end ;
                                    PS := B ;
                                    exit ;
                                end ;
            O6400..O6477 : // MARK
                            if( CF8b1 ) then
                            begin
                                SP := _PC + 2 * ( A and 63 ) ;
                                PC := Register[ 5 ] ;
                                Register[ 5 ] := Pop ;
                                exit ;
                            end ;
        end ; // case I of
        Execute_PDP11 := False ; { Failure }
    end ; // .Execute_PDP11


    function Get_Interrupt_Vector( Priority : integer ) : word ;

    var E : boolean ;

    begin
        Memory_Data_Latch := 255 ;
        Send_Signal( 'UNIBUS_BG' + inttostr( Priority ), True ) ;
        Result := Bus_Read_Word( 0, IO_Type_Bus, E, False ) ;
        Send_Signal( 'UNIBUS_BG' + inttostr( Priority ), False ) ;
        if( Waiting ) then
        begin
            Waiting := False ;
            State_Change_Notice( State_Wait, False ) ;
            if( ( ( _PS and 16 ) = 16 ) and ( not CF13 ) ) then // T bit set after WAIT is finished
            begin
                Trap( O14 ) ;
            end ;
        end ;
    end ;


    function Got_Interrupt : boolean ;

    var P : integer ;

    begin
        Result := False ;
        if( Priority < 7 ) then // If any interrupts are allowed
        begin
            for P := 7 downto Priority + 1 do // Find highest current request
            begin
                if( BR[ P ] ) then
                begin
                    Result := True ;
                    exit ;
                end ;
            end ;
        end ;
    end ;


var A, P : integer ;
    Count : integer ;
    Nest_Level : integer ;
    Trap_This_Instruction : boolean ;
    Original_PC : integer;

begin // TPDP11_CPU.Execute
    // Setup...
    Count := 0 ;
    Nest_Level := 0 ;

    // Execution loop
    while( True ) do
    begin
        if( Blocked ) then // Waiting for system to catch up to our clock time
        begin
            Do_Wait ;
            continue ;
        end ;
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
            { Note: the 11/15 and 11/20 always guarantee the execution of the
              first instruction in an interrupt handler before any other
              interrupt is handled whereas all other PDP-11 models allow the
              first instruction to be interrupted.  Just by the nature of the
              way the code is organized, an interrupt can never occur until
              after the next instruction executes.  Thus, the 11/15 and 11/20
              behavior applies to all models.  This should not be an issue with
              the proper emulation for any model. }
            if( Priority < 7 ) then // If any interrupts are allowed
            begin
                for P := 7 downto Priority + 1 do // Find highest current request
                begin
                    if( BR[ P ] ) then
                    begin
                        if( _Logger <> nil ) then
                        begin
                            _logger.Log( Parent, 'Process interrupt', -1, False, LT_State_Change ) ;
                        end ;
                        BR[ P ] := False ;
                        A := Get_Interrupt_Vector( P ) ; // Get vector from interrupting device
                        Trap( A ) ;
                        State_Change_Notice( State_Interrupt, True ) ;
                        break ;
                    end ;
                end ; // for P := 7 downto Priority + 1
            end ;
            if( Waiting ) then
            begin
                continue ;
            end ;
        end ;
        if( ( Count > 1 ) and ( _Run_Stream = nil ) ) then
        begin
            if( _Breakpoints.Indexof( _PC ) <> -1 ) then
            begin
                _UI.Breakpoint_Notice( _PC, True, 0, Parent ) ;
            end ;
        end ;
        if( _Profiling ) then
        begin
            TPDP11_Profiler( Parent.Profiler ).Increment( Domain_Execution_Addresses, PC ) ;
            TPDP11_Profiler( Parent.Profiler ).Increment( Domain_Other, Domain_Other_Instruction_Count ) ;
        end ;
        Log_Trace( 'Executing instruction at address ' + cvtb( 10, Base, inttostr( PC ) ) + ': ' + Instruction_At( PC ) ) ;
        if( ( ( _PC and 1 ) = 1 ) and ( _Run_Stream = nil ) ) then // Attempting fetch from odd address
        begin
            // Odd-address boundary trap (Bus error trap)...
            if( CF21 ) then // Odd-address trap enabled
            begin
                Trap( 4 ) ;
            end ;
        end ;
        Original_Pc := _PC ;
        A := Fetch_Word ;

        if( _Logger <> nil ) then
        begin
            _Logger.Update( Parent, Original_PC, A ) ;
        end ;
if(original_pc=0) then
begin
    __PS := 228;
end ;

        if( _Profiling ) then
        begin
            TPDP11_Profiler( Parent.Profiler ).Increment( Domain_Instructions, A ) ;
        end ;

        Trap_This_Instruction := ( ( _PS and 16 ) = 16 ) and ( _Run_Stream = nil ) ;
        if( Trap_This_Instruction and ( not CF11 ) and ( A = 2 {RTI} ) ) then
        begin
            Trap_This_Instruction := False ;
        end ;
        if( Trap_This_Instruction and CF13 and ( A = 1 {WAIT} ) ) then
        begin
            Trap_This_Instruction := False ;
        end ;
        if( Execute_PDP11( A ) ) then
        begin
            if( _Run_Stream = nil ) then // Not in immediate mode
            begin
                if( Inhibit_T_Trap ) then
                begin
                    Inhibit_T_Trap := False ;
                end else
                begin
                    if( Trap_This_Instruction and ( not Halted ) ) then
                    begin
                        if( ( not CF12 ) or ( not Got_Interrupt ) ) then
                        begin
                            Trap( O14 ) ;
                        end ;
                    end ;
                end ;
            end ;
            if( Stack_Overflow ) then
            begin
                Trap( 4 ) ;
            end ;
            continue ;
        end ;

        // Reserved instruction...
        if( _Run_Stream <> nil ) then // Immediate mode
        begin
            exit ; // Shouldn't happen
        end ;
        Trap( O10 ) ; // Reserved instruction
    end ; // while( True )
end ; // TPDP11_CPU.Execute


function TPDP11_CPU.Get_Register( Index : byte ) : word ;

    procedure Check_Watch( Index : integer ) ;

    begin
        if( ( _Register_Watchpoints[ Index ] and Access_Read ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Read, 0, False, True, False ) ;
        end ;
    end ;

begin
    Result := _Register[ Index ] ;
    if( _Run_Stream = nil ) then
    begin
        Check_Watch( Index ) ;
    end ;
end ;


function TPDP11_CPU.PS_N : integer ; // Returns N flag (0 or 1)

begin
    Result := ( _PS shr 3 ) and 1 ;
end ;


function TPDP11_CPU.PS_Z : integer ; // Returns Z flag (0 or 1)

begin
    Result := ( _PS shr 2 ) and 1 ;
end ;


function TPDP11_CPU.PS_V : integer ; // Returns V flag (0 or 1)

begin
    Result := ( _PS shr 1 ) and 1 ;
end ;


function TPDP11_CPU.PS_C : integer ; // Returns C flag (0 or 1)

begin
    Result := _PS and 1 ;
end ;


procedure TPDP11_CPU.Set_Register( Index : byte ; Value : word ) ;

    procedure Check_Watch( Index : integer ) ;

    begin
        if( ( _Register_Watchpoints[ Index ] and Access_Write ) <> 0 ) then
        begin
            Watchpoint_Notice( Index, Access_Write, 0, False, True, False ) ;
        end ;
    end ;

begin
    if( ( _RTS_Flags and RTS_Want_Registers ) <> 0 ) then
    begin
        Value := _RTS.Register_Change( Index, Value ) ;
    end ;
    _Register[ Index ] := Value ;
    if( _Run_Stream = nil ) then
    begin
        Check_Watch( Index ) ;
    end ;
end ;


function TPDP11_CPU.Get_Register_Low( Index : byte ) : byte ;

begin
    Result := Register[ Index ] and 255 ;
end ;


procedure TPDP11_CPU.Set_Register_Low( Index : byte ; Value : byte ) ;

begin
    Register[ Index ] := ( _Register[ Index ] and ( not 255 ) ) or Value ;
end ;


function TPDP11_CPU.Get__Register( Index : byte ) : word ;

var Mode : integer ;

begin
    Mode := 0 ;
    if( CF8j ) then
    begin
        Mode := PS shr 14 ;
        if( Mode <> 3 ) then // Only Kernel and User currently allowed
        begin
            Mode := 0 ;
        end ;
    end ;
    case Index of
        0..5 : Result := _Registers[ 0, Index ] ;
        6 : Result := _R6[ Mode ] ;
        else Result := _PC ;
    end ;
end ;


procedure TPDP11_CPU.Set__Register( Index : byte ; Value : word ) ;

var Mode : integer ;

begin
    Mode := 0 ;
    if( CF8j ) then
    begin
        Mode := PS shr 14 ;
        if( Mode <> 3 ) then // Only Kernel and User currently allowed
        begin
            Mode := 0 ;
        end ;
    end ;
    case Index of
        0..5 : _Registers[ 0, Index ] := Value ;
        6 : _R6[ Mode ] := Value ;
        7 : _PC := Value ;
    end ;
end ;


function TPDP11_CPU.Get__Register_Low( Index : byte ) : byte ;

begin
    Result := _Register[ Index ] and 255 ;
end ;


procedure TPDP11_CPU.Set__Register_Low( Index : byte ; Value : byte ) ;

begin
    _Register[ Index ] := ( _Register[ Index ] and ( not 255 ) ) or Value ;
end ;


function TPDP11_CPU.Get_Priority : byte ; // Processor priority from PSW

begin
    Result := ( _PS shr 5 ) and 7 ;
end ;


function TPDP11_CPU.Get_PC : word ;

begin
    Result := _PC ;
end ;


procedure TPDP11_CPU.Set_PC( Value : word ) ;

begin
    _PC := Value ;
end ;


function TPDP11_CPU.Get_SP : word ;

begin
    Result := Register[ 6 ] ;
end ;


procedure TPDP11_CPU.Set_SP( Value : word ) ;

begin
    Register[ 6 ] := Value ;
end ;


function TPDP11_CPU.Get__SP : word ;

begin
    Result := _Register[ 6 ] ;
end ;


procedure TPDP11_CPU.Set__SP( Value : word ) ;

begin
    _Register[ 6 ] := Value ;
end ;


function TPDP11_CPU.Get_PS : word ;

begin
    Result := _PS ;
    if( ( _Register_Watchpoints[ 8 ] and Access_Write ) <> 0 ) then
    begin
        Watchpoint_Notice( 8, Access_Read, 0, False, True, False ) ;
    end ;
end ;


procedure TPDP11_CPU.Set__PS( Value : word ) ;

begin
    if( ( ( __PS shr 14 ) <> ( Value shr 14 ) ) and CF8j ) then
    begin
        case Value shr 14 of
            0 : State_Change_Notice( State_Kernel, True ) ;
            3 : State_Change_Notice( State_User, True ) ;
        end ;
    end ;
    __PS := Value
end ;


procedure TPDP11_CPU.Set_PS( Value : word ) ;

begin
    _PS := Value ;
    if( ( _Register_Watchpoints[ 8 ] and Access_Write ) <> 0 ) then
    begin
        Watchpoint_Notice( 8, Access_Write, 0, False, True, False ) ;
    end ;
end ;


function TPDP11_CPU.Translate( Space : integer ; Address : int64 ) : int64 ;

var Abort : boolean ;
    Mode : integer ;

begin
    case Space of
        -1, 0 : Mode := _PS ; // Current mode
        1 : Mode := $C000 ; // User
        2 : Mode := 0 ; // Kernel
        else begin
                 Translate := Address ;
                 exit ;
             end ;
    end ;
    Translate := MMU.Relocate( Address, Mode, True, Abort ) ;
end ;


function TPDP11_CPU.Default_Base : integer ;

begin
    Result := 8 ;
end ;


function TPDP11_CPU.Get_Low_Memory : int64 ;

begin
    Result := 0 ;
end ;


function TPDP11_CPU.Get_High_Memory : int64 ;

begin
    Result := 65535 ;
end ;


function TPDP11_CPU.Get_Low_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TPDP11_CPU.Get_High_Virtual_Memory( Space : integer ) : int64 ;

begin
    Result := 65535 ;
end ;


function TPDP11_CPU.Get_Low_Port : int64 ;

begin
    Result := -1 ;
end ;


function TPDP11_CPU.Get_High_Port : int64 ;

begin
    Result := -1 ;
end ;


function TPDP11_CPU.Support_Virtual_Address : boolean ;

begin
    Result := ( Model = 34 ) ;
end ;


function TPDP11_CPU.Segment_Size( Index : integer ) : integer ;

begin
    Result := 16
end ;


function TPDP11_CPU.Register_Name( Index : integer ) : PChar ;

begin
    Temp_Register_Name := '' ; // Invalid index
    if( ( Index >= 0 ) and ( Index < Register_Map.Count ) ) then
    begin
        Index := Register_Map[ Index ] ; // Map to maximum register set
        case Index of
            0..7 : Temp_Register_Name := 'R' + inttostr( Index ) ;
            8 : Temp_Register_Name := 'PSW' ;
            9 : Temp_Register_Name := 'R6 (Kernel)' ;
            10 : Temp_Register_Name := 'R6 (Super)' ;
            11 : Temp_Register_Name := 'R6 (User)' ;
            12..17 : Temp_Register_Name := 'R' + inttostr( Index - 12 ) + ' (0)' ;
            18..23 : Temp_Register_Name := 'R' + inttostr( Index - 18 ) + ' (1)' ;
        end ;
    end ;
    Result := PChar( Temp_Register_Name ) ;
end ;


function TPDP11_CPU.Register_Size( Index : integer ) : integer ;

begin
    Result := 0 ;
    if( ( Index >= 0 ) and ( Index < Register_Map.Count ) ) then
    begin
        Result := 16 ;
    end ;
end ;


procedure TPDP11_CPU.Restart ;

var E : boolean ;

begin
    Pending_Interrupt := False ;
    Waiting := False ;
    Blocked := false ;
    _Halted := false ;
    Inhibit_T_Trap := false ;
    fillchar( BR, sizeof( BR ), 0 ) ;
    Set_Error( 0 ) ;
    PC := Word_Read( O24, E ) ;
    PS := Word_Read( O26, E ) ;
    Memory_Control_Register := 0 ;
    CPU_Error_Register := 0 ;
    fillchar( _Registers, sizeof( _Registers ), 0 ) ;
    __PS := 228 ;

    _Run_Stream := nil ;
end ;


function TPDP11_CPU.Top_Of_Stack( Index : integer ) : int64 ;

begin
    Top_Of_Stack := _SP ;
end ;


function TPDP11_CPU.Big_Endian : boolean ;

begin
    Result := False ;
end ;


function TPDP11_CPU.Register_RTS( RTS : TRun_Time_System ; Flags : longint ) : tUnified_Exception ;

begin
    _RTS := RTS ;
    _RTS_Flags := Flags ;
    Result := nil ;
end ;


function TPDP11_CPU.Get_Current_Address( Space : integer ;
    Physical : boolean ) : int64 ;

begin
    if( Space = 0 ) then
    begin
        Result := _PC ;
    end else
    begin
        Result := 0 ;
    end ;
end ;


procedure TPDP11_CPU.Set_Current_Address( Space : integer ; Physical : boolean ;
    Value : int64 ) ;

begin
    if( Space = 0 ) then
    begin
        _PC := Value ;
    end ;
end ;



// TPDP11 methods...

// API...

function TPDP11.Facility_Code : longint ;

begin
    Result := PDP11Err_Facility ;
end ;


function TPDP11.Initialize( UI : TUI_Interface ) : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
    _CPU := TPDP11_CPU.Create ;
    _CPU._UI := UI ;
    _CPU.Parent := self ;
    Inputs := TList.Create ;
    Outputs := TList.Create ;
    _CPU.Set_Model( 34 ) ;
    CPU.Restart ; // Do power-on reset
    UI.Want_Signals( self, True ) ;
end ;


function TPDP11.Terminate : TUnified_Exception ;

begin
    Result := Set_Error( 0 ) ;
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


function TPDP11.Serial_Number : integer ;

begin
    Result := _Serial_Number ;
end ;


function TPDP11.Child_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TPDP11.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Clear_Watchpoint( Address, Memory, Access ) ;
end ;


function TPDP11.Component_Type : longint ;

begin
    Result := Component_Type_CPU ; // CPU
end ;


function TPDP11.Connect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( PDP11Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Inputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( PDP11Err_Already_Connected ) ;
        exit ;
    end ;
    Inputs.Add( Component ) ;
    Result := Set_Error( PDP11Err_Success ) ;
end ;


function TPDP11.Connect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( Component = nil ) then
    begin
        Result := Set_Error( PDP11Err_Invalid_Component ) ;
        exit ;
    end ;
    if( Outputs.Indexof( Component ) <> -1 ) then
    begin
        Result := Set_Error( PDP11Err_Already_Connected ) ;
        exit ;
    end ;
    Outputs.Add( Component ) ;
    Result := Set_Error( PDP11Err_Success ) ;
end ;


function TPDP11.CPU : TCPU ;

begin
    Result := _CPU ;
end ;


function TPDP11.Debugger : TDebug_Interface ;

begin
    Result := nil ; // TODO
end ;


function TPDP11.Disconnect_Input( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Inputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Result := Set_Error( PDP11Err_Component_Not_Found ) ;
    end else
    begin
	Result := Set_Error( PDP11Err_Success ) ;
	Inputs.Remove( Component ) ;
    end ;
end ;


function TPDP11.Disconnect_Output( Component : TComponent ) : TUnified_Exception ;

begin
    if( ( Outputs.Indexof( Component ) = -1 ) or ( Component = nil ) ) then
    begin
	Result := Set_Error( PDP11Err_Component_Not_Found ) ;
    end else
    begin
	Result := Set_Error( PDP11Err_Success ) ;
	Outputs.Remove( Component ) ;
    end ;
end ;


function TPDP11.Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
    Memory : boolean ) : TUnified_Exception ;

var Old, S, Temp, V, Value : integer ;

begin
    Result := Set_Error( 0 ) ;
    if( Memory ) then
    begin
        V := Address and $FFFFE ;
        Size := ( Size + 7 ) div 8 ; // Number of bytes
        Value := 0 ;
        move( Buffer^, Value, Size ) ;
        if( Size = 1 ) then // Byte-read
        begin
            Temp := 0 ;
            S := 16 ;
            Examine( V, S, @Temp, True ) ;
            if( V <> Address ) then // Reading odd address
            begin
                Value := ( Value shl 8 ) or ( Temp and $FF ) ; // High byte
            end else
            begin
                Value := ( Value and $FF ) or ( Temp and $FF00 ) ;
            end ;
        end ; // if( Size = 1 )

        // Registers...
        if( V = O777700 ) then
        begin
            _CPU._Registers[ 1, 0 ] := Value ;
            exit ;
        end else
        if( Address = O777701 ) then
        begin
            _CPU._Registers[ 1, 1 ] := Value ;
            exit ;
        end else
        if( V = O777702 ) then
        begin
            _CPU._Registers[ 1, 2 ] := Value ;
            exit ;
        end else
        if( Address = O777703 ) then
        begin
            _CPU._Registers[ 1, 3 ] := Value ;
            exit ;
        end else
        if( V = O777704 ) then
        begin
            _CPU._Registers[ 1, 4 ] := Value ;
            exit ;
        end else
        if( Address = O777705 ) then
        begin
            _CPU._Registers[ 1, 5 ] := Value ;
            exit ;
        end else
        if( V = O777706 ) then
        begin
            _CPU._R6[ 0 ] := Value ; // Kernel SP
            exit ;
        end else
        if( _CPU.CF17 ) then
        begin
            if( V = O777710 ) then
            begin
                _CPU._Registers[ 1, 0 ] := Value ;
                exit ;
            end else
            if( Address = O777711 ) then
            begin
                _CPU._Registers[ 1, 1 ] := Value ;
                exit ;
            end else
            if( V = O777712 ) then
            begin
                _CPU._Registers[ 1, 2 ] := Value ;
                exit ;
            end else
            if( Address = O777713 ) then
            begin
                _CPU._Registers[ 1, 3 ] := Value ;
                exit ;
            end else
            if( V = O777714 ) then
            begin
                _CPU._Registers[ 1, 4 ] := Value ;
                exit ;
            end else
            if( Address = O777715 ) then
            begin
                _CPU._Registers[ 1, 5 ] := Value ;
                exit ;
            end ;
        end ;
        if( _CPU.CF8j ) then
        begin
            if( V = O777716 ) then
            begin
                _CPU._R6[ 1 ] := Value ; // Supervisor SP
                exit ;
            end else
            if( Address = O777717 ) then
            begin
                _CPU._R6[ 3 ] := Value ; // User SP
                exit ;
            end ;
        end ;
        if( Address = O777776 ) then
        begin
            _CPU._PS := Value ;
            exit ;
        end ;

        // MMU...
        Old := _CPU.MMU.Addressing ;
        if( _CPU.MMU.Deposit( V, Value ) ) then // Handle case of changing MMU register
        begin
            if( Old <> _CPU.MMU.Addressing ) then
            begin
                case _CPU.MMU.Addressing of
                    16 : _CPU.State_Change_Notice( State_16_Bit, True ) ;
                    18 : _CPU.State_Change_Notice( State_18_Bit, True ) ;
                    22 : _CPU.State_Change_Notice( State_22_Bit, True ) ;
                end ;
            end ;
            exit ;
        end ;
        Set_Error( PDP11Err_No_Cache ) ;
    end else
    begin
        if( Address >= _CPU.Register_Map.Count ) then
        begin
            Set_Error( PDP11Err_Invalid_Register ) ;
            exit ;
        end ;
        Result := Set_Error( 0 ) ;

        if( Size = 0 ) then
        begin
            exit ;
        end ;
        Address := _CPU.Register_Map[ Address ] ; // Translate to maximum set
        if( Size > _CPU.Register_Size( Address ) ) then
        begin
            Size := _CPU.Register_Size( Address ) ;
        end ;
        Size := ( Size + 7 ) div 8 ; // Number of bytes
        V := 0 ;
        move( Buffer^, V, Size ) ;
        case Address of
            0..7 : _CPU._Register[ Address ] := V ;
            8 : _CPU._PS := V ;
            9 : _CPU._R6[ 0 ] := V ;
            10 : _CPU._R6[ 1 ] := V ;
            11 : _CPU._R6[ 3 ] := V ;
            12..17 : _CPU._Registers[ 0, Address - 12 ] := V ;
            18..23 : _CPU._Registers[ 1, Address - 18 ] := V ;
        end ;
    end ; // if( Memory )
end ; // TPDP11.Deposit


function TPDP11.Examine( Address : int64 ; var Size : longint ;
    Buffer : pointer ; Memory : boolean ) : TUnified_Exception ;

var Len, V : integer ;
    Success : boolean ;

begin
    if( Memory ) then
    begin
        Result := Set_Error( 0 ) ;
        V := Address and $FFFFE ;

        // Registers...
        if( V = O777700 ) then
        begin
            V := _CPU._Registers[ 1, 0 ] ;
            if( odd( V ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end else
        if( Address = O777701 ) then
        begin
            V := _CPU._Registers[ 1, 1 ] ;
            if( odd( V ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end else
        if( V = O777702 ) then
        begin
            V := _CPU._Registers[ 1, 2 ] ;
            if( odd( V ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end else
        if( Address = O777703 ) then
        begin
            V := _CPU._Registers[ 1, 3 ] ;
            if( odd( V ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end else
        if( V = O777704 ) then
        begin
            V := _CPU._Registers[ 1, 4 ] ;
            if( odd( V ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end else
        if( Address = O777705 ) then
        begin
            V := _CPU._Registers[ 1, 5 ] ;
            if( odd( V ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end else
        if( V = O777706 ) then
        begin
            V := _CPU._R6[ 0 ] ; // Kernel SP
            if( odd( V ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end else
        if( _CPU.CF17 ) then
        begin
            if( V = O777710 ) then
            begin
                V := _CPU._Registers[ 1, 0 ] ;
                if( odd( V ) ) then
                begin
                    V := V shr 8 ;
                end ;
                move( V, Buffer^, ( Size + 7 ) div 8 ) ;
                exit ;
            end else
            if( Address = O777711 ) then
            begin
                V := _CPU._Registers[ 1, 1 ] ;
                if( odd( V ) ) then
                begin
                    V := V shr 8 ;
                end ;
                move( V, Buffer^, ( Size + 7 ) div 8 ) ;
                exit ;
            end else
            if( V = O777712 ) then
            begin
                V := _CPU._Registers[ 1, 2 ] ;
                if( odd( V ) ) then
                begin
                    V := V shr 8 ;
                end ;
                move( V, Buffer^, ( Size + 7 ) div 8 ) ;
                exit ;
            end else
            if( Address = O777713 ) then
            begin
                V := _CPU._Registers[ 1, 3 ] ;
                if( odd( V ) ) then
                begin
                    V := V shr 8 ;
                end ;
                move( V, Buffer^, ( Size + 7 ) div 8 ) ;
                exit ;
            end else
            if( V = O777714 ) then
            begin
                V := _CPU._Registers[ 1, 4 ] ;
                if( odd( V ) ) then
                begin
                    V := V shr 8 ;
                end ;
                move( V, Buffer^, ( Size + 7 ) div 8 ) ;
                exit ;
            end else
            if( Address = O777715 ) then
            begin
                V := _CPU._Registers[ 1, 5 ] ;
                if( odd( V ) ) then
                begin
                    V := V shr 8 ;
                end ;
                move( V, Buffer^, ( Size + 7 ) div 8 ) ;
                exit ;
            end ;
        end ;
        if( _CPU.CF8j ) then
        begin
            if( V = O777716 ) then
            begin
                V := _CPU._R6[ 1 ] ; // Supervisor SP
                if( odd( V ) ) then
                begin
                    V := V shr 8 ;
                end ;
                move( V, Buffer^, ( Size + 7 ) div 8 ) ;
                exit ;
            end else
            if( Address = O777717 ) then
            begin
                V := _CPU._R6[ 3 ] ; // User SP
                if( odd( V ) ) then
                begin
                    V := V shr 8 ;
                end ;
                move( V, Buffer^, ( Size + 7 ) div 8 ) ;
                exit ;
            end ;
        end ;
        if( Address = O777776 ) then
        begin
            V := _CPU._PS ;
            if( odd( V ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end ;

        // MMU...
        V := _CPU.MMU.Examine( V, Success ) ;
        if( Success ) then
        begin
            if( odd( Address ) ) then
            begin
                V := V shr 8 ;
            end ;
            move( V, Buffer^, ( Size + 7 ) div 8 ) ;
            exit ;
        end ;
        Result := Set_Error( PDP11Err_No_Cache ) ;
    end else
    begin
        if( Address >= _CPU.Register_Map.Count ) then
        begin
            Result := Set_Error( PDP11Err_Invalid_Register ) ;
            exit ;
        end ;
        Result := Set_Error( 0 ) ;

        if( Size = 0 ) then
        begin
            exit ;
        end ;
        Len := _CPU.Register_Size( Address ) ;
        Address := _CPU.Register_Map[ Address ] ; // Map to maximum set
        if( Size > Len ) then
        begin
            Size := Len ;
        end ;
        case Address of
            0..7 : V := _CPU._Register[ Address ] ;
            8 : V := _CPU._PS ;
            9 : V := _CPU._R6[ 0 ] ;
            10 : V := _CPU._R6[ 1 ] ;
            11 : V := _CPU._R6[ 3 ] ;
            12..17 : V :=_CPU._Registers[ 0, Address - 12 ] ;
            18..23 : V := _CPU._Registers[ 1, Address - 18 ] ;
        end ;
        move( V, Buffer^, ( Size + 7 ) div 8 ) ;
    end ; // if( Memory )
end ; // TPDP11.Examine


function TPDP11.Get_Access_Mode( Address : int64 ; Memory : boolean ) : longint ;

begin
    if( Memory ) then
    begin
        Result := Access_None ;
    end else
    begin
        if( ( Address < 0 ) or ( Address > 9 ) ) then
        begin
            Result := Access_None ;
        end else
        begin
            Result := Access_All ;
        end ;
    end ;
end ;


function TPDP11.Get_Profiling : boolean ;

begin
    Result := _CPU._Profiling ;
end ;


function TPDP11.Get_Read_Latency : longint ;

begin
    Result := 0 ;
end ;


function TPDP11.Get_Write_Latency : longint ;

begin
    Result := 0 ;
end ;


function TPDP11.Input_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Inputs.Count ) ) then
    begin
        Result := nil ;
        _CPU.Set_Error( PDP11Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Inputs[ Index ] ;
end ;


function TPDP11.Memory : TMemory ;

begin
    if( _Memory = nil ) then
    begin
        _Memory := TPDP11_Memory.Create ;
    end ;
    Result := _Memory ;
end ;


const PDP11_Name : string = 'PDP-11' ;

function TPDP11.Name : PChar ;

begin
    Result := PChar( PDP11_Name ) ;
end ;


function TPDP11.Output_Component( Index : longint ) : TComponent ;

begin
    if( ( Index < 0 ) or ( Index >= Outputs.Count ) ) then
    begin
        Result := nil ;
        _CPU.Set_Error( PDP11Err_Component_Not_Found ) ;
        exit ;
    end ;
    Result := Outputs[ Index ] ;
end ;


function TPDP11.Read( Address : int64 ; Size : longint ;
    IO_Type : longint ) : boolean ;

begin
    Result := False ;
end ;


function TPDP11.Restore_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
end ;


function TPDP11.Save_Contents( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
end ;


function TPDP11.Set_Access_Mode( Low, High : int64 ; Memory : boolean ;
    Typ : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( PDP11_Invalid_Operation ) ;
end ;


procedure TPDP11.Set_Profiling( _On, Children : boolean ) ;

begin
    _CPU._Profiling := _On ;
end ;


procedure TPDP11.Set_Read_Latency( Value : longint ) ;

begin
    // Do nothing - we have no read latency
end ;


function TPDP11.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUnified_Exception ;

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
        Result := _CPU.Set_Error( PDP11_Invalid_Address ) ;
    end ;
end ; // TPDP11.Set_Watchpoint


procedure TPDP11.Set_Write_Latency( Value : longint ) ;

begin
    // Intentionally left blank - no latency
end ;


function TPDP11.Support_Feature( ID : integer ) : boolean ;

begin
    Result := False ;
end ;


procedure TPDP11.Wake ;

begin
    _CPU.Blocked := False ;
end ;


function TPDP11.Write( Address : int64 ; Value, Size : longint ;
    IO_Type : integer ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    if( ( Address = 1 ) and ( Size = 8 ) ) then // High byte of a word
    begin
        _CPU.Memory_Data_Latch := ( _CPU.Memory_Data_Latch and 255 ) or ( Value shl 8 ) ;
    end else
    begin
        _CPU.Memory_Data_Latch := Value ;
    end ;
end ;


function TPDP11.Write_String( Address : int64 ; Value : PChar ;
    Size : longint ; IO_Type : longint ) : TUnified_Exception ;

begin
    Result := _CPU.Set_Error( 0 ) ;
    _CPU.Memory_Data_Latch := ord( Value[ ( ( Size + 7 ) div 8 ) - 1 ] ) ;
    if( Size > 8 ) then
    begin
        _CPU.Memory_Data_Latch :=
            ( _CPU.Memory_Data_Latch shl 8 ) or ord( Value[ ( ( Size + 7 ) div 8 ) - 2 ] ) ;
    end ;
end ;


procedure TPDP11.Set_Up( P : PChar ) ;

var Parser : TString_Parser ;
    S : string ;

begin
    Parser := TString_Parser.Create ;
    Parser.Set_Source( string( P ) ) ;
    S := uppercase( Parser.Token ) ;
    while( S <> '' ) do
    begin
        if( S = 'MEMORY' ) then
        begin
            _CPU.Memory_System := True ;
        end else
        if( S = 'MODEL' ) then
        begin
            S := Parser.Token ;
            if( S = '=' ) then
            begin
                S := Parser.Token ;
            end ;
(*
            if( S = '44' ) then
            begin
                _CPU.Set_Model( 44 ) ;
            end else
            if( S = '45' ) then
            begin
                _CPU.Set_Model( 45 ) ;
            end else
            if( S = '70' ) then
            begin
                _CPU.Set_Model( 70 ) ;
            end else
*)
            if( ( S = '5' ) or ( S = '05' ) ) then
            begin
                _CPU.Set_Model( 5 ) ;
            end else
            if( S = '10' ) then
            begin
                _CPU.Set_Model( 10 ) ;
            end else
            if( S = '15' ) then
            begin
                _CPU.Set_Model( 15 ) ;
            end else
            if( S = '20' ) then
            begin
                _CPU.Set_Model( 20 ) ;
            end else
            if( S = '34' ) then
            begin
                _CPU.Set_Model( 34 ) ;
            end ;
        end else
        if( S = 'KD11-B' ) then
        begin
            _CPU.Set_Model( 5 ) ;
        end else
        if( S = 'KC11' ) then
        begin
            _CPU.Set_Model( 15 ) ;
        end else
(*
        if( S = 'KD11A' ) then
        begin
            _CPU.Set_Model( 40 ) ;
        end else
        if( ( S = 'KB11B' ) or ( S = 'KB11C' ) ) then
        begin
            _CPU.Set_Model( 70 ) ;
        end else
        if( ( S = 'KB11' ) or ( S = 'KB11A' ) ) then
        begin
            _CPU.Set_Model( 45 ) ;
        end else
        if( S = 'KB11D' ) then
        begin
            _CPU.Set_Model( 55 ) ;
        end else
        if( S = 'KDJ11' ) then
        begin
            _CPU.Set_Model( 73 ) ;
        end else
*)
        if( S = 'KA11' ) then
        begin
            _CPU.Set_Model( 20 ) ;
        end ;
        S := uppercase( Parser.Token ) ;
    end ; // while( S <> '' )
    Parser.Free ;
end ;


procedure TPDP11.Set_Tag( Value : longint ) ;

begin
    _Tag := Value ;
end ;


function TPDP11.Get_Tag : longint ;

begin
    Result := _Tag ;
end ;


function TPDP11.Get_Parent : TComponent ;

begin
    Result := _Parent ;
end ;


procedure TPDP11.Set_Parent( Component : TComponent ) ;

begin
    _Parent := Component ;
end ;


function TPDP11.Profiler : TProfiler ;

begin
    if( _CPU._Profiler = nil ) then
    begin
        _CPU._Profiler := TPDP11_Profiler.Create ;
    end ;
    Result := _CPU._Profiler ;
end ;


function TPDP11.Get_Trace : boolean ;

begin
    Result := _CPU._Trace ;
end ;


procedure TPDP11.Set_Trace( Value : boolean ) ;

begin
    _CPU._Trace := Value ;
end ;


function TPDP11.Restore_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Restore_State( Stream ) ;
end ;


function TPDP11.Save_State( Stream : TCOM_Stream ) : TUnified_Exception ;

begin
    Result := _CPU.Save_State( Stream ) ;
end ;


procedure TPDP11.Show_Status ;

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
    Output( 'PDP11' ) ;
    for Loop := 0 to 7 do
    begin
        S := 'R' + inttostr( Loop ) + '=' + Show( _CPU._Register[ Loop ], 1 ) ;
        if( Loop = 6 ) then
        begin
           S := S + ' (SP)' ;
        end ;
        if( Loop = 7 ) then
        begin
           S := S + ' (PC)' ;
        end ;
        Output( S ) ;
    end ;
    S := 'PSW=' + Show( _CPU._PS, 2 ) ;
    Output( S ) ;
end ; // TPDP11.Show_Status


procedure TPDP11.Reset ;

begin
    _CPU.Restart ;
end ;


procedure TPDP11.Set_Signal( Name : PChar ; State : boolean ) ;

var BR_Index : integer ;
    Temp : string ;

begin
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    if( copy( Temp, 1, 9 ) = 'UNIBUS_BR' ) then
    begin
        BR_Index := -1 ;
        if( Temp = 'UNIBUS_BR0' ) then
        begin
            BR_Index := 0 ;
        end else
        if( Temp = 'UNIBUS_BR1' ) then
        begin
            BR_Index := 1 ;
        end else
        if( Temp = 'UNIBUS_BR2' ) then
        begin
            BR_Index := 2 ;
        end else
        if( Temp = 'UNIBUS_BR3' ) then
        begin
            BR_Index := 3 ;
        end else
        if( Temp = 'UNIBUS_BR4' ) then
        begin
            BR_Index := 4 ;
        end else
        if( Temp = 'UNIBUS_BR5' ) then
        begin
            BR_Index := 5 ;
        end else
        if( Temp = 'UNIBUS_BR6' ) then
        begin
            BR_Index := 6 ;
        end else
        if( Temp = 'UNIBUS_BR7' ) then
        begin
            BR_Index := 7 ;
        end ;
        if( BR_Index > -1 ) then
        begin
            if( BR_Index < _CPU.Low_BR ) then
            begin
                BR_Index := 4 ;
            end ;
            _CPU.BR[ BR_Index ] := State ;
        end ;
        if( _Logger <> nil ) then
        begin
            BR_Index := pos( '_', Temp + '_' ) ;
            Temp := copy( Temp, 1, BR_Index - 1 ) ;
            _Logger.Log( self, PChar( Name + ' = ' + inttostr( ord( State ) ) ), -1, True, LT_Received_Signal ) ;
        end ;
    end ; // if( copy( Temp, 1, 9 ) = 'UNIBUS_BR' )
end ; // TPDP11.Set_Signal


function TPDP11.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;

var BR_Index : integer ;
    Temp : string ;

begin
    Result := False ;
    Temp := string( Name ) ;
    Temp := uppercase( Name ) ;
    if( copy( Temp, 1, 9 ) = 'UNIBUS_BR' ) then
    begin
        BR_Index := -1 ;
        if( Temp = 'UNIBUS_BR0' ) then
        begin
            BR_Index := 0 ;
        end else
        if( Temp = 'UNIBUS_BR1' ) then
        begin
            BR_Index := 1 ;
        end else
        if( Temp = 'UNIBUS_BR2' ) then
        begin
            BR_Index := 2 ;
        end else
        if( Temp = 'UNIBUS_BR3' ) then
        begin
            BR_Index := 3 ;
        end else
        if( Temp = 'UNIBUS_BR4' ) then
        begin
            BR_Index := 4 ;
        end else
        if( Temp = 'UNIBUS_BR5' ) then
        begin
            BR_Index := 5 ;
        end else
        if( Temp = 'UNIBUS_BR6' ) then
        begin
            BR_Index := 6 ;
        end else
        if( Temp = 'UNIBUS_BR7' ) then
        begin
            BR_Index := 7 ;
        end ;
        if( BR_Index > -1 ) then
        begin
            if( ( BR_Index < 4 ) and ( ( _CPU.Model = 20 ) or ( _CPU.Model = 15 ) ) ) then
            begin
                BR_Index := 4 ;
            end ;
            State := _CPU.BR[ BR_Index ] ;
            Result := True ;
        end ;
    end else
    if( copy( Temp, 1, 2 ) = 'UNIBUS_BG' ) then
    begin
        BR_Index := -1 ;
        if( Temp = 'UNIBUS_BG0' ) then
        begin
            BR_Index := 0 ;
        end else
        if( Temp = 'UNIBUS_BG1' ) then
        begin
            BR_Index := 1 ;
        end else
        if( Temp = 'UNIBUS_BG2' ) then
        begin
            BR_Index := 2 ;
        end else
        if( Temp = 'UNIBUS_BG3' ) then
        begin
            BR_Index := 3 ;
        end else
        if( Temp = 'UNIBUS_BG4' ) then
        begin
            BR_Index := 4 ;
        end else
        if( Temp = 'UNIBUS_BG5' ) then
        begin
            BR_Index := 5 ;
        end else
        if( Temp = 'UNIBUS_BG6' ) then
        begin
            BR_Index := 6 ;
        end else
        if( Temp = 'UNIBUS_BG7' ) then
        begin
            BR_Index := 7 ;
        end ;
        if( BR_Index > -1 ) then
        begin
            if( ( BR_Index < 4 ) and ( ( _CPU.Model = 20 ) or ( _CPU.Model = 15 ) ) ) then
            begin
                BR_Index := 4 ;
            end ;
            State := _CPU.BG[ BR_Index ] ;
            Result := True ;
        end ;
    end else
    if( Temp = 'UNIBUS_INIT' ) then
    begin
        State := _CPU._Init ;
        Result := True ;
    end else
    if( Temp = '16 BIT' ) then
    begin
        Result := _CPU.Signals[ 17 ] ;
    end else
    if( Temp = '18 BIT' ) then
    begin
        Result := _CPU.Signals[ 18 ] ;
    end else
    if( Temp = '22 BIT' ) then
    begin
        Result := _CPU.Signals[ 19 ] ;
    end else
    if( Temp = 'KERNEL' ) then
    begin
        Result := _CPU.Signals[ 20 ] ;
    end else
    if( Temp = 'SUPER' ) then
    begin
        Result := _CPU.Signals[ 21 ] ;
    end else
    if( Temp = 'USER' ) then
    begin
        Result := _CPU.Signals[ 22 ] ;
    end ;
end ;


function TPDP11.Signal_Count : longint ;

begin
    Result := 23 ;
end ;


function TPDP11.Signal_Name( Index : longint ) : PChar ;

begin
    Temp_Signal_Name := _Signal_Name( Index ) ;
    if( Temp_Signal_Name = '' ) then
    begin
        Result := nil ;
    end else
    begin
        Result := PChar( Temp_Signal_Name ) ;
    end ;
end ;


function TPDP11.Signal_Out( Index : longint ) : boolean ;

begin
    case Index of
        0..7 : Result := False ; // BR0-BR7
        8..22 : Result := True ; // INIT, BG0-BG7, 16 BIT, 18 BIT, 22 BIT, KERNEL, SUPER, USER
        else Result := False ;
    end ; // case Index
end ;


function TPDP11.Signal_Active_Low( Index : longint ) : boolean ;

begin
    Result := False ;
end ;


function TPDP11.Get_State_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( ( Index >= 0 ) and ( Index <= 8 ) ) then
    begin
        case Index of
            State_Interrupt : Temp_Get_State_Name := 'Interrupt' ;
            State_Wait : Temp_Get_State_Name := 'Wait' ;
            State_16_Bit : Temp_Get_State_Name := '16 Bit Addressing' ;
            State_18_Bit : Temp_Get_State_Name := '18 Bit Addressing' ;
            State_22_Bit : Temp_Get_State_Name := '22 Bit Addressing' ;
            State_Kernel : Temp_Get_State_Name := 'Kernel' ;
            State_Super : Temp_Get_State_Name := 'Super' ;
            State_User : Temp_Get_State_Name := 'User' ;
        end ;
        Result := PChar( Temp_Get_State_Name ) ;
    end ;
end ;


function TPDP11.Get_Exception_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Temp_Get_Exception_Description := 'Invalid instruction' ;
                Result := PChar( Temp_Get_Exception_Description ) ;
            end ;
    end ;
end ;


function TPDP11.Signal_Index( Name : PChar ) : integer ;

var S : string ;

begin
    S := string( Name ) ;
    Result := -1 ;
    if( S = 'UNIBUS_BR0' ) then
    begin
        Result := 0 ;
    end else
    if( S = 'UNIBUS_BR1' ) then
    begin
        Result := 1 ;
    end else
    if( S = 'UNIBUS_BR2' ) then
    begin
        Result := 2 ;
    end else
    if( S = 'UNIBUS_BR3' ) then
    begin
        Result := 3 ;
    end else
    if( S = 'UNIBUS_BR4' ) then
    begin
        Result := 4 ;
    end else
    if( S = 'UNIBUS_BR5' ) then
    begin
        Result := 5 ;
    end else
    if( S = 'UNIBUS_BR6' ) then
    begin
        Result := 6 ;
    end else
    if( S = 'UNIBUS_BR7' ) then
    begin
        Result := 7 ;
    end else
    if( S = 'UNIBUS_INIT' ) then
    begin
        Result := 8 ;
    end else
    if( S = 'UNIBUS_BG0' ) then
    begin
        Result := 9 ;
    end else
    if( S = 'UNIBUS_BG1' ) then
    begin
        Result := 10 ;
    end else
    if( S = 'UNIBUS_BG2' ) then
    begin
        Result := 11 ;
    end else
    if( S = 'UNIBUS_BG3' ) then
    begin
        Result := 12 ;
    end else
    if( S = 'UNIBUS_BG4' ) then
    begin
        Result := 13 ;
    end else
    if( S = 'UNIBUS_BG5' ) then
    begin
        Result := 14 ;
    end else
    if( S = 'UNIBUS_BG6' ) then
    begin
        Result := 15 ;
    end else
    if( S = 'UNIBUS_BG7' ) then
    begin
        Result := 16 ;
    end else
    if( S = '16 BIT' ) then
    begin
        Result := 17 ;
    end else
    if( S = '18 BIT' ) then
    begin
        Result := 18 ;
    end else
    if( S = '22 BIT' ) then
    begin
        Result := 19 ;
    end else
    if( S = 'KERNEL' ) then
    begin
        Result := 20 ;
    end else
    if( S = 'SUPER' ) then
    begin
        Result := 21 ;
    end else
    if( S = 'USER' ) then
    begin
        Result := 22 ;
    end ;
end ;


function TPDP11.Get_Logger : TCEF_Logger ;

begin
    Result := _Logger ;
end ;


procedure TPDP11.Set_Logger( Value : TCEF_Logger ) ;

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

