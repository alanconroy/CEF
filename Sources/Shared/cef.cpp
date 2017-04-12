/*
        Program Name : CEF
        Package Name : CEF
        Purpose      : CEF definitions
        Institution  :
        Date Written : 27-Apr-2000
        Written By   : Alan Conroy
        Version      : 1.0

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

          This file contains implementations of base classes to make
        CEF component development somewhat easier.  The definition of
        TComponent herein is equivalent to a null component.  
*/

#include <stddef.h>
#include <stdio.h>
#include "cef.h"

// TUI_Interface...

void __stdcall TUI_Interface::Block( TComponent* Component, bool Blocked )
{
}


bool __stdcall TUI_Interface::Breakpoint_Notice( int64 Address, bool Physical,
             int Space, TCPU* CPU )
{
    return false ;
}


TMaster_Clock* __stdcall TUI_Interface::Clock()
{
    return NULL ;
}


TDebug_Interface* __stdcall TUI_Interface::Debugger()
{
    return NULL ;
}


TCOM_Stream* __stdcall TUI_Interface::Get_File_Stream( char* Name )
{
    return NULL ;
}


void __stdcall TUI_Interface::Hide( bool Flag )
{
}


void __stdcall TUI_Interface::Idle( TComponent* Component )
{
}


void __stdcall TUI_Interface::Log_Error( TComponent* Component, char* Text,
             int Severity )
{
}


void __stdcall TUI_Interface::Log_Simulated_Error( TComponent* Component, char* Text,
             int Severity )
{
}


void __stdcall TUI_Interface::Log_Status( char* Text, int Index )
{
    puts( Text ) ;
}


void __stdcall TUI_Interface::Log_Trace( TComponent* Component, char* Description )
{
}


void __stdcall TUI_Interface::Signal_Change_Notice( TComponent* Component,
             int Index, bool Active )
{
}


void __stdcall TUI_Interface::Signal_Exception( TComponent* Component,
             char* Description, int Index )
{
}


void __stdcall TUI_Interface::State_Change_Notice( TComponent* Component,
             int Index, bool Active )
{
}


void __stdcall TUI_Interface::Terminate()
{
    delete this;
}


void __stdcall TUI_Interface::Toggle_Embed( TComponent* Component )
{
}


int __stdcall TUI_Interface::Version()
{
    return Interface_Version ;
}


void __stdcall TUI_Interface::Want_Signals( TComponent* Component, bool Value )
{
}


void __stdcall TUI_Interface::Watchpoint_Notice( int64 Address, int Access, int Tag,
             TComponent* Component, bool Memory, bool Internal, bool Port )
{
}


char* __stdcall TUI_Interface::Get_Port_Name( int Index )
{
    return NULL ;
}


char* __stdcall TUI_Interface::Get_Port_Description( int Index )
{
    return NULL ;
}


TCable* __stdcall TUI_Interface::Get_Port( int Index )
{
    return NULL ;
}


TComponent* __stdcall TUI_Interface::Get_Port_Connection( int Index )
{
    return NULL ;
}


TComponent* __stdcall TUI_Interface::Port_Parent_Component( int Index )
{
    return NULL ;
}


void __stdcall TUI_Interface::Run( bool State )
{
}


int __stdcall TUI_Interface::Process_ID( char* Name, bool Force )
{
    return 0 ;
}


bool __stdcall TUI_Interface::Process_Start( int ID, int& Priority )
{
    Priority = 0 ;
    return true ;
}


void __stdcall TUI_Interface::Process_End( int ID  )
{
}


void __stdcall TUI_Interface::Process_Deleted( int ID )
{
}


void __stdcall TUI_Interface::Add_Port_Breakpoint()
{
}


// Prompt user and add an execution breakpoint.
void __stdcall TUI_Interface::Add_Breakpoint()
{
}


// Prompt user and add register breakpoint.
void __stdcall TUI_Interface::Add_Register_Breakpoint()
{
}


// Prompt user and create a new memory breakpoint.
void __stdcall TUI_Interface::Create_New_Breakpoint()
{
}


// Return passed component name qualified by path, etc.
char* __stdcall TUI_Interface::Get_Component_Filename( char* Name )
{
	return Name;
}


void __stdcall TUI_Interface::Termination_Notice( TComponent* C )
{
}


// Load and return a component with the specified name.
TComponent* __stdcall TUI_Interface::Load_Component( char* Name )
{
	return NULL;
}



// TProfiler...

void __stdcall TProfiler::Clear( int Domain )
{
}


char* __stdcall TProfiler::Domain_Name( int Index )
{
    return NULL;
}


char* __stdcall TProfiler::Report_Line( int Domain, int Index )
{
    return NULL;
}


int __stdcall TProfiler::Version()
{
    return Interface_Version ;
}



// TComponent...

int __stdcall TComponent::Facility_Code()
{
    return ComponentErr_Facility;
}


TUEC __stdcall TComponent::Initialize( TUI_Interface UI )
{
	TUEC res ;
	res.Facility = Facility_Code() ;
	res.Code = 0 ;
	return res ;
}


TUEC __stdcall TComponent::Terminate()
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = 0 ;
    delete this;
    return res ;
}


int __stdcall TComponent::Serial_Number()
{
    return 0 ;
}


TUEC __stdcall TComponent::Add_Notification( TComponent* Component )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = 0 ;
    return res ;
}


TCable* __stdcall TComponent::Cable()
{
    return NULL ;
}


TComponent* __stdcall TComponent::Child_Component( int Index )
{
    return NULL ;
}


TUEC __stdcall TComponent::Clear_Watchpoint( int64 Address, bool Memory,
            int Access )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_No_Watchpoints ;
    return res ;
}


int __stdcall TComponent::Component_Type()
{
    return Component_Type_Unknown ;
}


TUEC __stdcall TComponent::Connect_Input( TComponent* Component )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Invalid_Operation ;
    return res ;
}


TUEC __stdcall TComponent::Connect_Output( TComponent* Component )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Invalid_Operation ;
    return res ;
}


TCPU* __stdcall TComponent::CPU()
{
    return NULL ;
}


TDebug_Interface* __stdcall TComponent::Debugger()
{
    return NULL ;
}


TUEC __stdcall TComponent::Delete_Notification( TComponent* Component )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Component_Not_Found ;
    return res ;
}


TUEC __stdcall TComponent::Deposit( int64 Address, int Size, void* Buffer,
            bool Memory )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Address_Out_Of_Range ;
    return res ;
}


TUEC __stdcall TComponent::Disconnect_Input( TComponent* Component )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Component_Not_Found ;
    return res ;
}


TUEC __stdcall TComponent::Disconnect_Output( TComponent* Component )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Component_Not_Found ;
    return res ;
}


TUEC __stdcall TComponent::Examine( int64 Address, int& Size,
            void* Buffer, bool Memory )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Address_Out_Of_Range ;
    return res ;
}


int __stdcall TComponent::Get_Access_Mode( int64 Address,
            bool Memory )
{
    return Access_None ;
}


char* __stdcall TComponent::Get_Exception_Description( int Index )
{
    return NULL ;
}


TComponent* __stdcall TComponent::Get_Parent()
{
    return NULL ;
}


bool __stdcall TComponent::Get_Profiling()
{
    return false ;
}


int __stdcall TComponent::Get_Read_Latency()
{
    return 0 ;
}


bool __stdcall TComponent::Get_Signal( char* Name, bool& State )
{
    return false ;
}


char* __stdcall TComponent::Get_State_Name( int Index )
{
    return NULL ;
}


int __stdcall TComponent::Get_Tag()
{
    return 0 ;
}


bool __stdcall TComponent::Get_Trace()
{
    return false ;
}


int __stdcall TComponent::Get_Write_Latency()
{
    return 0 ;
}


TComponent* __stdcall TComponent::Input_Component( int Index )
{
    return NULL ;
}


TKeyboard* __stdcall TComponent::Keyboard()
{
    return NULL ;
}


TMemory* __stdcall TComponent::Memory()
{
    return NULL ;
}


char* __stdcall TComponent::Name()
{
    return NULL ;
}


TComponent* __stdcall TComponent::Output_Component( int Index )
{
    return NULL ;
}


TProfiler* __stdcall TComponent::Profiler()
{
    return NULL ;
}


bool __stdcall TComponent::Read( int64 Address, int Size,
            int IO_Type )
{
    return false ;
}


void __stdcall TComponent::Reset()
{
}


TUEC __stdcall TComponent::Restore_Contents( TCOM_Stream* Stream )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Success ;
    return res ;
}


TUEC __stdcall TComponent::Restore_State( TCOM_Stream* Stream )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Success ;
    return res ;
}


TUEC __stdcall TComponent::Save_Contents( TCOM_Stream* Stream )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Success ;
    return res ;
}


TUEC __stdcall TComponent::Save_State( TCOM_Stream* Stream )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Success ;
    return res ;
}


TUEC __stdcall TComponent::Set_Access_Mode( int64 Low, int64 High, bool Memory,
            int Typ )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Address_Out_Of_Range ;
    return res ;
}


void __stdcall TComponent::Set_Parent( TComponent* Component )
{
}


void __stdcall TComponent::Set_Profiling( bool _On, bool Children )
{
}


void __stdcall TComponent::Set_Read_Latency( int Value )
{
}


void __stdcall TComponent::Set_Signal( char* Name, bool State )
{
}


void __stdcall TComponent::Set_Tag( int Value )
{
}


void __stdcall TComponent::Set_Trace( bool Value )
{
}


void __stdcall TComponent::Set_Up( char* P )
{
}


TUEC __stdcall TComponent::Set_Watchpoint( int64 Address, bool Memory,
            int Access )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = ComponentErr_Invalid_Operation ;
    return res ;
}


void __stdcall TComponent::Set_Write_Latency( int Value )
{
}


void __stdcall TComponent::Show_Status()
{
}


void __stdcall TComponent::Signal_Change_Notice( TComponent* Component,
            int Index, bool Active )
{
}


int __stdcall TComponent::Signal_Count()
{
    return 0 ;
}


char* __stdcall TComponent::Signal_Name( int Index )
{
    return NULL ;
}


bool __stdcall TComponent::Signal_Out( int Index )
{
    return false ;
}


bool __stdcall TComponent::Signal_Active_Low( int Index )
{
    return false ;
}


bool __stdcall TComponent::Support_Feature( int ID )
{
    return false ;
}


TUser_Interface* __stdcall TComponent::User_Interface()
{
    return NULL ;
}


int __stdcall TComponent::Version()
{
    return Interface_Version ;
}


void __stdcall TComponent::Wake()
{
}


TUEC __stdcall TComponent::Write( int64 Address, int Value, int Size,
            int IO_Type )
{
    return Deposit( Address, Size, &Value, ( IO_Type = IO_Type_Memory ) ) ;
}


TUEC __stdcall TComponent::Write_String( int64 Address, char* Value,
            int Size, int IO_Type )
{
    TUEC res ;
    for( int Loop = 0 ; Loop < Size ; Loop++ )
    {
	res = Write( Address, Value[ Loop ], 1, IO_Type ) ;
	Address = Address + 1 ;
    } ;
    return res ;
}


char* __stdcall TComponent::Get_Port_Name( int Index )
{
    return NULL ;
}


char* __stdcall TComponent::Get_Port_Description( int Index )
{
    return NULL ;
}


TComponent* __stdcall TComponent::Get_Port( int Index )
{
    return NULL ;
}


TComponent* __stdcall TComponent::Get_Port_Connection( int Index )
{
    return NULL ;
}


void __stdcall TComponent::Child_Notification( TComponent* Child, int &Notice,
    int64 &Params)
{
}


void __stdcall TComponent::UI_Notice( int Code, int64& Data )
{
}


bool __stdcall TComponent::Respond_To_Address( int64 Address, int Typ, bool Examine )
{
    return false;
}


TCEF_Logger* __stdcall TComponent::Get_Logger()
{
	return NULL;
}


void __stdcall TComponent::Set_Logger( TCEF_Logger* Value )
{
}



// TMaster_Clock...

void __stdcall TMaster_Clock::Block( TComponent* Component, int64 Time_Delta )
{
    Component->Wake() ;
}


TDebug_Interface* __stdcall TMaster_Clock::Debugger()
{
    return NULL ;
}


void __stdcall TMaster_Clock::Initialize( TUI_Interface* UI )
{
}


int64 __stdcall TMaster_Clock::Get_Time_Index()
{
    return 0 ;
}


int __stdcall TMaster_Clock::Serial_Number()
{
    return 0 ;
}


bool __stdcall TMaster_Clock::Support_Feature( int ID )
{
    return false ;
}


int __stdcall TMaster_Clock::Version()
{
    return Interface_Version ;
}


void __stdcall TMaster_Clock::Set_Mode( int M )
{
}


int __stdcall TMaster_Clock::Get_Mode()
{
    return 0;
}


void __stdcall TMaster_Clock::Unblock()
{
}



// TCable

bool __stdcall TCable::Serial()
{
    return true;
}


char* __stdcall TCable::Protocol()
{
    return NULL;
}


TUEC __stdcall TCable::Transmit( int64 Speed, int Value, int Data_Size, int Stop_Bits )
{
    TUEC res ;
	res.Facility = 0 ;
    res.Code = 0 ;
    return res ;
}


TUEC __stdcall TCable::Transmit_String( int64 Speed, char* Value, int Data_Size, int Stop_Bits )
{
    TUEC res ;
	res.Facility = 0 ;
    res.Code = 0 ;
    return res ;
}


void __stdcall TCable::Receive( TComponent* Source, int64 Speed, int Value, int Data_Size, int Stop_Bits )
{
}


bool __stdcall TCable::Get_Data( int64 &Speed, int &Value, int &Data_Size, int &Stop_Bits )
{
    return false;
}



// TUser_Interface...

bool __stdcall TUser_Interface::Get_Hidden()
{
    return true ;
}


void __stdcall TUser_Interface::Set_Hidden( bool Value )
{
}


int __stdcall TUser_Interface::Get_Parent_Window()
{
    return 0 ;
}


void __stdcall TUser_Interface::Set_Parent_Window( int Value )
{
}


char* __stdcall TUser_Interface::Get_Caption()
{
    return NULL ;
}


void __stdcall TUser_Interface::Set_Caption( char* Value )
{
}


void __stdcall TUser_Interface::Set_Size( int Height, int Width )
{
}


int __stdcall TUser_Interface::Optimal_Height()
{
    return 0 ;
}


int __stdcall TUser_Interface::Optimal_Width()
{
    return 0 ;
}


int __stdcall TUser_Interface::Version()
{
    return Interface_Version ;
}


void __stdcall TUser_Interface::Initialize()
{
}



// TKeyboard... 

char* __stdcall TKeyboard::Get_Key()
{
    return NULL;
}


bool __stdcall TKeyboard::Get_Key_Down( char* Name )
{
    return false ;
}


void __stdcall TKeyboard::Set_Key_Down( char* Name, bool State )
{
}


char* __stdcall TKeyboard::Get_Key_Name( int Index )
{
    return NULL ;
}


bool __stdcall TKeyboard::Get_LED_State( char* Name )
{
    return false ;
}


void __stdcall TKeyboard::Set_LED_State( char* Name, bool State )
{
}


char* __stdcall TKeyboard::Get_LED_Name( int Index )
{
    return NULL ;
}


int __stdcall TKeyboard::Version()
{
    return Interface_Version ;
}



// TMemory...

int __stdcall TMemory::Facility_Code()
{
    return -1 ;
}


void __stdcall TMemory::Dump( int64 Start, int64 Size, void* Buffer )
{
}


void __stdcall TMemory::Get_Address_Range( int64& Low, int64& High )
{
}


void __stdcall TMemory::Load( int64 Start, int64 Size, void* Buffer  )
{
}


TUEC __stdcall TMemory::Set_Address_Range( int64 Low, int64 High )
{
    TUEC res ;
	res.Facility = 0 ;
    res.Code = MemoryErr_Success ;
    return res ;
}


int __stdcall TMemory::Version()
{
    return Interface_Version ;
}



// TCEF_Assembler_Context methods...

void __stdcall TCEF_Assembler_Context::Initialize()
{
}


void __stdcall TCEF_Assembler_Context::Terminate()
{
    delete this;
}


int __stdcall TCEF_Assembler_Context::Add_Mapping( char* Filename, int64 Address,
    int Line )
{
    return -1 ;
}


TUEC __stdcall TCEF_Assembler_Context::Add_Symbol( char* Name, TSymbol_Record* P )
{
    TUEC result ;
    result.Facility = -1 ;
    result.Code = -1 ;
    return result ;
}


void __stdcall TCEF_Assembler_Context::Delete( char* Sym )
{
}


void __stdcall TCEF_Assembler_Context::Delete_Mapping( int Index )
{
}


int __stdcall TCEF_Assembler_Context::Find( char* Sym, int64& Addr,
    int& Flg, int& D_T, int& Siz, void*& Dat )
{
    return -1 ;
}


/* Returns filename and line number corresponding to the passed address.  If
   there is no mapping, it returns -1. */
int __stdcall TCEF_Assembler_Context::Mapping( int64 Address, char*& Filename )
{
    return -1 ;
}


void __stdcall TCEF_Assembler_Context::Pop_Level()
{
}


void __stdcall TCEF_Assembler_Context::Push_Level()
{
}


/* Get value associated with passed symbol.  If symbol is unknown, it returns
   False. */
bool __stdcall TCEF_Assembler_Context::Symbol_Value( char* Name, int64& Value )
{
    return false ;
}


/* Get size associated with passed symbol.  If symbol is unknown, it returns
   False. */
bool __stdcall TCEF_Assembler_Context::Symbol_Size( char* Name, int& Value )
{
    return false ;
}


bool __stdcall TCEF_Assembler_Context::Get_Case_Sensitive()
{
    return false ;
}


void __stdcall TCEF_Assembler_Context::Set_Case_Sensitive( bool Value )
{
}



// TMaster_Assembler...

void __stdcall TMaster_Assembler::Add_Reference( char* Name, int Size,
    int64 Address )
{
    Add_Reference_Ex( Name, Size, Address, 0, 0 ) ;
}


TUEC __stdcall TMaster_Assembler::Add_Symbol( char* Name, TSymbol_Record* P )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = MasterAssemblerErr_Success ;
    return res ;
}


TUEC __stdcall TMaster_Assembler::Assemble( TCOM_Stream* Input,
    TCOM_Stream* Output, TCOM_Stream* Listing, TAssembler_Status* Status )
{
    return Assemble_Ex( Input, Output, Listing, Status, 0 ) ;
}


void __stdcall TMaster_Assembler::Backpatch( TAssembler_Status* Status,
            TCOM_Stream* Stream )
{
}


TUEC __stdcall TMaster_Assembler::Evaluate( char* X, int64& Value )
{
    return Evaluate_Ex( X, Value, 0 ) ;
}


TUEC __stdcall TMaster_Assembler::Expand( char* Source, char* &Res,
            int& Res_Length, TAssembler_Status* Status )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = MasterAssemblerErr_Success ;
    return res ;
}


int __stdcall TMaster_Assembler::Facility_Code()
{
    return MasterAssemblerErr_Facility ;
}


bool __stdcall TMaster_Assembler::Get_Case_Sensitive()
{
    return false ;
}


TSymbol_Record* __stdcall TMaster_Assembler::Get_Symbol( char* Name )
{
    return NULL ;
}


char* __stdcall TMaster_Assembler::Get_Token()
{
    return NULL ;
}


char* __stdcall TMaster_Assembler::Grab_Line( bool Current )
{
    return NULL ;
}


void __stdcall TMaster_Assembler::In_Line( TCOM_Stream* Input )
{
}


void __stdcall TMaster_Assembler::Log_Error( char* Text, int Severity )
{
    puts( Text ) ;
}


void __stdcall TMaster_Assembler::Map( int64 Address )
{
}


char* __stdcall TMaster_Assembler::Peek_Token( bool Same_Line )
{
    return NULL ;
}


void __stdcall TMaster_Assembler::Pop_Scope()
{
}


void __stdcall TMaster_Assembler::Push_Scope()
{
}


void __stdcall TMaster_Assembler::Put_Token( char* Token )
{
}


void __stdcall TMaster_Assembler::Set_Case_Sensitive( bool Value )
{
}


void __stdcall TMaster_Assembler::UnMap()
{
}


int __stdcall TMaster_Assembler::Version()
{
    return Interface_Version ;
}


void __stdcall TMaster_Assembler::Add_Reference_Ex( char* Name, int Size,
    int64 Address, int Context, int Options )
{
}


void __stdcall TMaster_Assembler::Add_CPU( TComponent* CPU, char* Name )
{
}


void __stdcall TMaster_Assembler::Clear_CPUs()
{
}


void __stdcall TMaster_Assembler::Set_Assembler_Context( TCEF_Assembler_Context* Value )
{
}


TCEF_Assembler_Context* __stdcall TMaster_Assembler::Get_Assembler_Context()
{
    return NULL;
}


void __stdcall TMaster_Assembler::Terminate()
{
    delete this;
}


bool __stdcall TMaster_Assembler::Leading_Whitespace()
{
    return false;
}


TUEC __stdcall TMaster_Assembler::Assemble_Ex( TCOM_Stream* Input,
    TCOM_Stream* Output, TCOM_Stream* Listing, TAssembler_Status* Status,
    int Flags )
{
    TUEC res ;
    res.Facility = Facility_Code() ;
    res.Code = MasterAssemblerErr_Success ;
    return res ;
}


TUEC __stdcall TMaster_Assembler::Evaluate_Ex( char* X, int64& Value, int64 PC_Adjustment )
{
	TUEC res ;
	res.Facility = 0 ;
    res.Code = MasterAssemblerErr_Success ;
    return res ;
}


int __stdcall TMaster_Assembler::Get_Base()
{
	return 10 ;
}


void __stdcall TMaster_Assembler::Set_Base( int Value )
{
}


void __stdcall TMaster_Assembler::Register_Extension( TAssembler_Extension* Extension )
{
}



// TAssembler...

void __stdcall TAssembler::Initialize( TMaster_Assembler* Master )
{
}


void __stdcall TAssembler::Terminate()
{
    delete this;
}


TUEC __stdcall TAssembler::Assemble( char* inputs, char* &outputs, char* &machines,
            int& MachineL, int64& Address,
            int& Segment, TAssembler_Status* Status )
{
	TUEC res ;
	res.Facility = Facility_Code() ;
	res.Code = AssemblerErr_Success ;
	return res ;
}


int __stdcall TAssembler::Default_Radix()
{
    return 16 ; // Hexadecimal
}


int __stdcall TAssembler::Default_Size()
{
    return 32 ; // 32-bits
}


int __stdcall TAssembler::Facility_Code()
{
    return AssemblerErr_Facility ;
}


char* __stdcall TAssembler::Source_Extensions()
{
    return NULL ;
}


char* __stdcall TAssembler::Valid_Symbol_Initial()
{
    return NULL ;
}


char* __stdcall TAssembler::Valid_Symbol_After()
{
    return NULL ;
}


int __stdcall TAssembler::Version()
{
	return Interface_Version ;
}


bool __stdcall TAssembler::Backpatching( char* Name, int64 Address, int64 &Value, int &Size,
    int Context, int Options, int Line, char* Filename,
    TAssembler_Status* Status )
{
	return true;
}


TUEC __stdcall TAssembler::Assemble_Ex( char* inputs, char* &outputs,
	char* &machines, int& MachineL, int64& Address, int& Segment,
	TAssembler_Status* Status, int Flags )
{
	TUEC res ;
	res.Facility = Facility_Code() ;
	res.Code = AssemblerErr_Success ;
	return res ;
}


int64 __stdcall TAssembler::Begin_Assembly()
{
	return 0;
}


TUEC __stdcall TAssembler::Finish_Assembly( int64 Context, char* &outputs,
	char* &machines, int &MachineL, int64 &Address,
	TAssembler_Status* Status, int Flags )
{
	TUEC res ;
	res.Facility = Facility_Code() ;
	res.Code = 0 ;
	return res ;
}


int64 __stdcall TAssembler::Request_Data( int64 Address, int64 Size )
{
	return Address ;
}



// TCPU...

TUEC __stdcall TCPU::Cancel_Breakpoint( int64 Address, int Space,
            bool Physical )
{
    TUEC res ;
	res.Facility = 0 ;
    res.Code = CPUErr_No_Breakpoint ;
    return res ;
}


TUEC __stdcall TCPU::Clear_Internal_Watchpoint( int64 Address, bool Memory,
            int Access )
{
    TUEC res ;
	res.Facility = 0 ;
    res.Code = CPUErr_No_Breakpoint ;
    return res ;
}


int __stdcall TCPU::Default_Base()
{
    return 10 ;
}


TUEC __stdcall TCPU::Disassemble( int64 Address, int Base, int Size,
            TCOM_Stream* Stream )
{
    TUEC res ;
	res.Facility = 0 ;
    res.Code = CPUErr_No_Breakpoint ;
    return res ;
}


TAssembler* __stdcall TCPU::Get_Assembler( TMaster_Assembler* Master )
{
    return NULL ;
}


int __stdcall TCPU::Get_Clock_Speed()
{
    return 1 ; // 1 Hz
}


int64 __stdcall TCPU::Get_Current_Address( int Space,
            bool Physical )
{
    return 0 ;
}

int64 __stdcall TCPU::Get_Low_Memory()
{
    return 0 ;
}


int64 __stdcall TCPU::Get_High_Memory()
{
	return 0x7FFFFFFFFFFFFFFF ;
}


int64 __stdcall TCPU::Get_Low_Port()
{
    return -1 ;
}


int64 __stdcall TCPU::Get_High_Port()
{
    return -1 ;
}


int64 __stdcall TCPU::Get_Low_Virtual_Memory( int Space )
{
    return 0 ;
}


int64 __stdcall TCPU::Get_High_Virtual_Memory( int Space )
{
    return 0 ;
}


void __stdcall TCPU::Halt()
{
}


bool __stdcall TCPU::Halted()
{
    return true ;
}


char* __stdcall TCPU::Memory_Space_Description( int Index,
            bool Physical )
{
    return NULL ;
}


int __stdcall TCPU::Page_Size()
{
    return 0 ;
}


char* __stdcall TCPU::Register_Description( int Index )
{
    return NULL ;
}


char* __stdcall TCPU::Register_Name( int Index )
{
    return NULL ;
}


int __stdcall TCPU::Register_Size( int Index )
{
    return 0 ;
}


void __stdcall TCPU::Restart()
{
}


void __stdcall TCPU::Run()
{
}


void __stdcall TCPU::Run_From_Stream( TCOM_Stream* Stream )
{
}


TUEC __stdcall TCPU::Set_Breakpoint( int64 Address, int Space,
            bool Physical )
{
    TUEC res ;
	res.Facility = 0 ;
    res.Code = CPUErr_Success ;
    return res ;
}


void __stdcall TCPU::Set_Clock_Speed( int Value )
{
}


void __stdcall TCPU::Set_Current_Address( int Space, bool Physical,
            int64 Value )
{
}


TUEC __stdcall TCPU::Set_Internal_Watchpoint( int64 Address, bool Memory,
            int Access )
{
    TUEC res ;
	res.Facility = 0 ;
    res.Code = CPUErr_Success ;
    return res ;
}


void __stdcall TCPU::Step( bool Into )
{
}


void __stdcall TCPU::Stop()
{
}


bool __stdcall TCPU::Support_Virtual_Address()
{
    return false ;
}


int64 __stdcall TCPU::Top_Of_Stack( int Index )
{
    return 0 ;
}


int64 __stdcall TCPU::Translate( int Space, int64 Address )
{
    return Address ;
}


int __stdcall TCPU::Version()
{
    return Interface_Version ;
}

int __stdcall TCPU::Segment_Size( int Index )
{
    return 0 ;
} ;


char* __stdcall TCPU::Address_Representation( int Base, int64 Address )
{
    return NULL ;
} ;


int64 __stdcall TCPU::Translate_Address( bool& B, int Base, char* Address )
{
	B = false ;
	return 0 ;
} ;


int64 __stdcall TCPU::Get_Low_Input_Port( int Space )
{
    return -1 ;
} ;


int64 __stdcall TCPU::Get_High_Input_Port( int Space )
{
    return -1 ;
} ;


int64 __stdcall TCPU::Get_Low_Output_Port( int Space )
{
    return -1 ;
} ;


int64 __stdcall TCPU::Get_High_Output_Port( int Space )
{
	return -1 ;
} ;


TCEF_Stack_Interface* __stdcall TCPU::Get_Stack_Interface( int Space )
{
	return NULL ;
}


TComponent* __stdcall TCPU::Get_Target_Memory()
{
	return NULL;
}


char* __stdcall TCPU::Address_Representation_Ex( TComponent* C, int Base,
   int64 Address )
{
	return NULL;
}


tUnified_Exception* __stdcall TCPU::Register_RTS( TRun_Time_System* RTS, int Flags )
{
	return NULL;
}


TData_Type* __stdcall TCPU::Register_Information( int Index )
{
	return NULL;
}


TComponent* __stdcall TCPU::Get_Store( int Index )
{
	return NULL;
}


int __stdcall TCPU::Get_Target_Address_Space()
{
    return 0;
}

