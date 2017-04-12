{
        Program Name : CEF32
        Package Name : CEF32
        Purpose      : CEF32 Localization
        Institution  :
        Date Written : 27-Apr-2010
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

          This unit provides for localization of CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Textstrs ;

interface

function Get_Text( Index : integer ; const Default : string ) : string ;
procedure Set_Language( const Name : string ) ;
function Substitute1( Source, S1 : string ) : string ;

function Text_1_Byte : string ;
function Text_Bytes : string ;
function Text_Bracket_Warning : string ;
function Text_Bracket_Error : string ;
function Text_Bracket_Hint : string ;
function Text_Page : string ;
function Text_Emulator : string ;
function Text_Breakpoint_At_Address : string ;
function Text_Break_On : string ;
function Text_Break_On_State_Change : string ;
function Text_Exception : string ;
function Text_Access_Read : string ;
function Text_Access_Write : string ;
function Text_Access : string ;
function Text_Access_Input : string ;
function Text_Access_Output : string ;
function Text_Port_Watchpoint_Triggered_For_Port : string ;
function Text_Cache_Watchpoint_At_Address : string ;
function Text_Register_Watchpoint_Triggered_For_Register : string ;
function Text_Watchpoint_At_Address : string ;
function Text_Caption_Disassembly : string ;
function Text_Caption_Traces : string ;
function Text_Error_Nothing_Found : string ;
function Text_Caption_Physical_amp : string ;
function Text_Caption_Logical_amp : string ;
function Text_Noname : string ;
function Text_Error_While_Creating_File : string ;
function Text_Error_While_Accessing_File : string ;
function Text_Status_Assembling : string ;
function Text_Button_Caption_Abort : string ;
function Text_Button_Caption_Close : string ;
function Text_Button_Caption_Clear_All : string ;
function Text_Button_Caption_OK : string ;
function Text_Button_Caption_Cancel : string ;
function Text_Button_Caption_Help : string ;
function Text_Menu_Caption_Reset_amp : string ;
function Text_Menu_Caption_Restart_amp : string ;
function Text_Menu_Caption_Embed : string ;
function Text_Menu_Caption_Save_Contents : string ;
function Text_Menu_Caption_Restore_Contents : string ;
function Text_Menu_Caption_Dump : string ;
function Text_Menu_Caption_Load : string ;
function Text_Menu_Caption_Save_State : string ;
function Text_Menu_Caption_Restore_State : string ;
function Text_Menu_Caption_Profile_amp : string ;
function Text_Menu_Caption_Profile_Report : string ;
function Text_Menu_Caption_Connections_amp : string ;
function Text_Menu_Caption_Condition_Handling_amp : string ;
function Text_Menu_Caption_Configure_amp : string ;
function Text_Menu_Caption_Unload_amp : string ;
function Text_Error_While_Opening_File : string ;
function Text_Error_While_Reading_File : string ;
function Text_Error_Cannot_Find_Component : string ;
function Text_Error_Is_Not_A_Valid_Number_In_Radix : string ;
function Text_Error_Expected_Equal : string ;
function Text_Error_Is_Not_A_Valid_Switch_On_LOAD : string ;
function Text_Error_Invalid_Radix : string ;
function Text_Error_Unknown_Component_On_Unload : string ;
function Text_Error_Unknown_Emulator_Command : string ;
function Text_Caption_Profile : string ;
function Text_Prompt_Include_Children_In_Change : string ;
function Text_Error_Load_Failure_For : string ;
function Text_Error_Not_A_Valid_CEF_Component : string ;
function Text_Error_Component_Creation_Failure : string ;
function Text_Error_Component_Is_An_Obsolete_Version : string ;
function Text_Error_Component_Is_An_Unsupported_Version : string ;
function Text_Source_Dialog_Filter : string ;
function Text_Error_File_Contains_Invalid_Hexadecimal_Values : string ;
function Text_CPU_Halted : string ;
function Text_Signal_Active : string ;
function Text_Signal_Inactive : string ;
function Text_Error_Invalid_Floating_Point_Format : string ;
function Text_Status_Modified : string ;
function Text_Prompt_Save_Changes_To : string ;
function Text_Caption_Modify_Register : string ;
function Text_Status_Executing : string ;
function Text_Status_Stopped : string ;
function Text_Error_Could_Not_Modify_Memory : string ;
function Text_Prompt_Delete_All_Watches : string ;
function Text_No_Connections : string ;
function Text_Caption_Emulator_Ports : string ;
function Text_Error_No_Components_Responded : string ;
function Text_Response : string ;
function Text_File_Filter_All_Files : string ;
function Text_File_Filter_Source_Files : string ;
function Text_File_Filter_Text_Files : string ;
function Text_Immediate_Mode : string ;
function Text_Maximum_Events_To_Trace : string ;
function Text_Memory : string ;
function Text_Ports : string ;
function Text_Menu_Caption_File_amp : string ;
function Text_Menu_Caption_Open_Emulator_amp : string ;
function Text_Menu_Caption_Reopen_Emulator : string ;
function Text_Menu_Caption_New_amp : string ;
function Text_Menu_Caption_Open_Source_amp : string ;
function Text_Menu_Caption_Reopen_Source : string ;
function Text_Menu_Caption_Save_amp : string ;
function Text_Menu_Caption_Save_As_amp : string ;
function Text_Menu_Caption_Save_All : string ;
function Text_Menu_Caption_Media_Manager_amp : string ;
function Text_Menu_Caption_Close_amp : string ;
function Text_Menu_Caption_Close_All : string ;
function Text_Menu_Caption_Printer_Setup : string ;
function Text_Menu_Caption_Print_amp : string ;
function Text_Menu_Caption_Exit_amp : string ;
function Text_Menu_Caption_Components_amp : string ;
function Text_Menu_Caption_Load_amp : string ;
function Text_Menu_Caption_Unload_All_amp : string ;
function Text_Menu_Caption_Emulator_Ports_amp : string ;
function Text_Menu_Caption_Clock_Mode_amp : string ;
function Text_Menu_Caption_Default_amp : string ;
function Text_Menu_Caption_Ignore_amp : string ;
function Text_Menu_Caption_Synchronize_amp : string ;
function Text_Menu_Caption_Disable_amp : string ;
function Text_Menu_Caption_Unblock_All_amp : string ;
function Text_Menu_Caption_Default_Memory_amp : string ;
function Text_Menu_Caption_Assemble_amp : string ;
function Text_Watchpoint_Watchpoint : string ;
function Text_Menu_Caption_Assemble_All_amp : string ;
function Text_Menu_Caption_Show_Errors_amp : string ;
function Text_Menu_Caption_Run_amp : string ;
function Text_Menu_Caption_Execute_amp : string ;
function Text_Menu_Caption_Step_Over_amp : string ;
function Text_Menu_Caption_Step_Into_amp : string ;
function Text_Menu_Caption_Program_Pause_amp : string ;
function Text_Menu_Caption_Run_Immediate_amp : string ;
function Text_Menu_Caption_Add_Watch_amp : string ;
function Text_Menu_Caption_Add_Execution_Breakpoint_amp : string ;
function Text_Menu_Caption_Execution_Breakpoints : string ;
function Text_Menu_Caption_Add_Port_Breakpoint : string ;
function Text_Menu_Caption_Port_Breakpoints : string ;
function Text_Menu_Caption_Profiling : string ;
function Text_Menu_Caption_Profile_Report_amp : string ;
function Text_Menu_Caption_Trace : string ;
function Text_Menu_Caption_Trace_Log_amp : string ;
function Text_Menu_Caption_Internals_amp : string ;
function Text_Menu_Caption_Options_amp : string ;
function Text_Menu_Caption_Help_amp : string ;
function Text_Menu_Caption_CEF_Specification : string ;
function Text_Menu_Caption_Component_Help : string ;
function Text_Menu_Caption_About_amp : string ;
function Text_File_Filter_CEF_Files : string ;
function Text_Menu_Caption_Open_Emulator : string ;
function Text_Menu_Caption_Radix_amp : string ;
function Text_Menu_Caption_Binary_amp : string ;
function Text_Menu_Caption_Octal_amp : string ;
function Text_Menu_Caption_Decimal_amp : string ;
function Text_Menu_Caption_Hexadecimal : string ;
function Text_Menu_Caption_Radix_50 : string ;
function Text_Menu_Caption_Other : string ;
function Text_Menu_Caption_Signed_amp : string ;
function Text_Menu_Caption_Size_amp : string ;
function Text_Menu_Caption_Byte_amp : string ;
function Text_Menu_Caption_Word_amp : string ;
function Text_Menu_Caption_Long_amp : string ;
function Text_Menu_Caption_Quad_amp : string ;
function Text_Menu_Caption_EBCDIC_amp : string ;
function Text_Menu_Caption_Physical_amp : string ;
function Text_Menu_Caption_Address_Space : string ;
function Text_Menu_Caption_Goto_Address_amp : string ;
function Text_Menu_Caption_Find_amp : string ;
function Text_Menu_Caption_Modify_amp : string ;
function Text_Menu_Caption_Watchpoints : string ;
function Text_Menu_Caption_Create_New : string ;
function Text_Menu_Caption_View : string ;
function Text_Menu_Caption_Pattern_amp : string ;
function Text_File_Filter_CEF_Memory_Contents : string ;
function Text_Menu_Caption_Save_Memory_Contents : string ;
function Text_File_Filter_CEF_Memory_State : string ;
function Text_Menu_Caption_Save_Memory_State : string ;
function Text_Menu_Caption_Restore_Memory : string ;
function Text_Menu_Caption_Restore_Memory_State : string ;
function Text_File_Filter_Dynamic_Link_Libraries : string ;
function Text_Menu_Caption_Open_Component : string ;
function Text_Menu_Caption_Increment_amp : string ;
function Text_Menu_Caption_Decrement_amp : string ;
function Text_Menu_Caption_Copy : string ;
function Text_Menu_Caption_Paste : string ;
function Text_Menu_Caption_Change_amp : string ;
function Text_Menu_Caption_Zero_amp : string ;
function Text_Menu_Caption_Add_Register_Breakpoint_amp : string ;
function Text_Menu_Caption_Register_Breakpoints_amp : string ;
function Text_Menu_Caption_Add_amp : string ;
function Text_Menu_Caption_Edit_amp : string ;
function Text_Menu_Caption_Delete_amp : string ;
function Text_Menu_Caption_Delete_All_amp : string ;
function Text_Menu_Caption_Show_Source_amp : string ;
function Text_Menu_Caption_Goto_Current_amp : string ;
function Text_Menu_Caption_Top_Of_Stack_amp : string ;
function Text_Menu_Caption_Open_Source : string ;
function Text_Menu_Caption_Goto_Port_amp : string ;
function Text_Menu_Caption_Input_amp : string ;
function Text_Menu_Caption_Output_amp : string ;
function Text_Caption_Conditions : string ;
function Text_Button_Caption_OK_amp : string ;
function Text_Button_Caption_Cancel_amp : string ;
function Text_Button_Caption_Help_amp : string ;
function Text_Caption_States : string ;
function Text_Caption_Signals : string ;
function Text_Caption_Exceptions : string ;
function Text_Caption_Errors : string ;
function Text_Caption_Break_On_Informational_Messagaes : string ;
function Text_Caption_Break_On_Warnings : string ;
function Text_Caption_Break_On_Errors : string ;
function Text_Caption_About : string ;
function Text_Computer_Emulation_Framework : string ;
function Text_Generic_Computer_Emulator : string ;
function Text_Menu_Caption_ASCII_amp : string ;
function Text_Value_Colon : string ;
function Text_Caption_Configure_Component : string ;
function Text_Caption_Attributes : string ;
function Text_Latency : string ;
function Text_Read_Colon : string ;
function Text_Write_Colon : string ;
function Text_CPU : string ;
function Text_Clock_Colon : string ;
function Text_Clock_Speed : string ;
function Text_Low_Address_Colon : string ;
function Text_High_Address_Colon : string ;
function Text_Caption_Options : string ;
function Text_Caption_Assembler : string ;
function Text_Generate_List_Files : string ;
function Text_Generate_Symbol_Table : string ;
function Text_Generate_Cross_Reference_List : string ;
function Text_Assemble_Into_Physical_Address_Space : string ;
function Text_Caption_Threading : string ;
function Text_Max_Threads : string ;
function Text_Thread_Priority : string ;
function Text_Normal : string ;
function Text_Idle_Only_Not_Recommended : string ;
function Text_Lower : string ;
function Text_Low : string ;
function Text_Synchronize : string ;
function Text_High : string ;
function Text_Higher : string ;
function Text_Real_Time_Not_Recommended : string ;
function Text_Caption_Master_Clock : string ;
function Text_Mode : string ;
function Text_Unblock_All_Components_After_Immediate_Mode_Execution : string ;
function Text_Enabled : string ;
function Text_Default : string ;
function Text_Ignore : string ;
function Text_Button_Caption_Skip : string ;
function Text_Button_Caption_Skip_All : string ;
function Text_Button_Caption_Retry : string ;
function Text_File_Already_Exists : string ;
function Text_Button_Caption_Overwrite : string ;
function Text_Button_Caption_Overwrite_All : string ;
function Text_Caption_Port_IO : string ;
function Text_Port_Colon : string ;
function Text_Size_Bits_Colon : string ;
function Text_Caption_Profile_Report : string ;
function Text_Button_Caption_Clear : string ;
function Text_Caption_Add_Data_To_Tape : string ;
function Text_Caption_Import_Data_From_File : string ;
function Text_Button_Caption_Add_From_File : string ;
function Text_Enter_Data_To_Add : string ;
function Text_Max_Bytes : string ;
function Text_Max_Block_Size_In_Bytes : string ;
function Text_Min_Block_Size_In_Bytes : string ;
function Text_Caption_Trace_Log : string ;
function Text_Button_Caption_Save : string ;
function Text_File_Mask : string ;
function Text_Caption_Save_Log_To_File : string ;
function Text_Caption_Tape_Media_Information : string ;
function Text_Format_Colon : string ;
function Text_BPI : string ;
function Text_0_Undefined : string ;
function Text_Length_Colon : string ;
function Text_0_Infinite : string ;
function Text_Feet : string ;
function Text_Button_Caption_Defaults : string ;
function Text_IRG_Length_Colon : string ;
function Text_TM_Length_Colon : string ;
function Text_1_1000 : string ;
function Text_Internal_Format : string ;
function Text_Note_Changing_These_Settings_Affects_The_Internal_Media_File_Data : string ;
function Text_Size_Record_Length_Colon : string ;
function Text_Include_Size_Records_Before_And_After_Records : string ;
function Text_Change_Port_Configuration : string ;


implementation

uses // Borland...
     Classes, // TStringList
     Dialogs, // ShowMessage
     SysUtils, // uppercase

     // C&C...
     O_S, // OS
    // Standard, // ERT
     UE ; // ERT

var Text_Strings : TStringList ;

procedure Set_Language( const Name : string ) ;

var Dummy : integer ;
    F : text ;
    S : string ;

begin
    if( uppercase( Name ) = 'ENGLISH' ) then
    begin
        Text_Strings.Free ;
        Text_Strings := nil ;
        exit ;
    end ;

    assignfile( F, OS^.Application_Path + 'Languages\' + Name ) ;
    {$I-}
    reset( F ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( ERT( Dummy ) + ' while opening requested language file: ' + Name + ' - continuing' ) ;
        exit ;
    end ;
    try
        if( Text_Strings <> nil ) then
        begin
            Text_Strings.Free ;
        end ;
        Text_Strings := TStringList.Create ;
        while( not eof( F ) ) do
        begin
            readln( F, S ) ;
            Text_Strings.Add( S ) ;
        end ;
    finally
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
    end ;
end ;


function Get_Text( Index : integer ; const Default : string ) : string ;

begin
    if( ( Text_Strings <> nil ) and ( Index >= 0 ) and ( Index < Text_Strings.Count ) ) then
    begin
        Result := Text_Strings[ Index ] ;
        exit ;
    end ;
    Result := Default ;
end ;


function Substitute1( Source, S1 : string ) : string ;

var Dummy : integer ;

begin
    Result := Source ;
    Dummy := pos( '$1', Result ) ;
    while( Dummy > 0 ) do
    begin
        Result := copy( Result, 1, Dummy - 1 ) + S1 + copy( Result, Dummy + 2, length( Result ) ) ;
        Dummy := pos( '$1', Result ) ;
    end ;
end ;


{$i _Textstrs.inc}

function Text_1_Byte : string ;

begin
    Result := Get_Text( ID_Text_1_Byte, _Text_1_Byte ) ;
end ;


function Text_Bytes : string ;

begin
    Result := Get_Text( ID_Text_Bytes, _Text_Bytes ) ;
end ;


function Text_Bracket_Warning : string ;

begin
    Result := Get_Text( ID_Text_Bracket_Warning, _Text_Bracket_Warning ) ;
end ;


function Text_Bracket_Error : string ;

begin
    Result := Get_Text( ID_Text_Bracket_Error, _Text_Bracket_Error ) ;
end ;


function Text_Bracket_Hint : string ;

begin
    Result := Get_Text( ID_Text_Bracket_Hint, _Text_Bracket_Hint ) ;
end ;


function Text_Page : string ;

begin
    Result := Get_Text( ID_Text_Page, _Text_Page ) ;
end ;


function Text_Emulator : string ;

begin
    Result := Get_Text( ID_Text_Emulator, _Text_Emulator ) ;
end ;


function Text_Breakpoint_At_Address : string ;

begin
    Result := Get_Text( ID_Text_Breakpoint_At_Address, _Text_Breakpoint_At_Address ) ;
end ;


function Text_Break_On : string ;

begin
    Result := Get_Text( ID_Text_Break_On, _Text_Break_On ) ;
end ;


function Text_Break_On_State_Change : string ;

begin
    Result := Get_Text( ID_Text_Break_On_State_Change, _Text_Break_On_State_Change ) ;
end ;


function Text_Exception : string ;

begin
    Result := Get_Text( ID_Text_Exception, _Text_Exception ) ;
end ;


function Text_Access_Read : string ;

begin
    Result := Get_Text( ID_Text_Access_Read, _Text_Access_Read ) ;
end ;


function Text_Access_Write : string ;

begin
    Result := Get_Text( ID_Text_Access_Write, _Text_Access_Write ) ;
end ;


function Text_Access : string ;

begin
    Result := Get_Text( ID_Text_Access, _Text_Access ) ;
end ;


function Text_Access_Input : string ;

begin
    Result := Get_Text( ID_Text_Access_Input, _Text_Access_Input ) ;
end ;


function Text_Access_Output : string ;

begin
    Result := Get_Text( ID_Text_Access_Output, _Text_Access_Output ) ;
end ;


function Text_Port_Watchpoint_Triggered_For_Port : string ;

begin
    Result := Get_Text( ID_Text_Port_Watchpoint_Triggered_For_Port, _Text_Port_Watchpoint_Triggered_For_Port ) ;
end ;


function Text_Cache_Watchpoint_At_Address : string ;

begin
    Result := Get_Text( ID_Text_Cache_Watchpoint_At_Address, _Text_Cache_Watchpoint_At_Address ) ;
end ;


function Text_Register_Watchpoint_Triggered_For_Register : string ;

begin
    Result := Get_Text( ID_Text_Register_Watchpoint_Triggered_For_Register, _Text_Register_Watchpoint_Triggered_For_Register ) ;
end ;


function Text_Watchpoint_At_Address : string ;

begin
    Result := Get_Text( ID_Text_Watchpoint_At_Address, _Text_Watchpoint_At_Address ) ;
end ;


function Text_Caption_Disassembly : string ;

begin
    Result := Get_Text( ID_Text_Caption_Disassembly, _Text_Caption_Disassembly ) ;
end ;


function Text_Error_Nothing_Found : string ;

begin
    Result := Get_Text( ID_Text_Error_Nothing_Found, _Text_Error_Nothing_Found ) ;
end ;


function Text_Caption_Physical_amp : string ;

begin
    Result := Get_Text( ID_Text_Caption_Physical_amp, _Text_Caption_Physical_amp ) ;
end ;


function Text_Caption_Logical_amp : string ;

begin
    Result := Get_Text( ID_Text_Caption_Logical_amp, _Text_Caption_Logical_amp ) ;
end ;


function Text_Noname : string ;

begin
    Result := Get_Text( ID_Text_Noname, _Text_Noname ) ;
end ;


function Text_Error_While_Creating_File : string ;

begin
    Result := Get_Text( ID_Text_Error_While_Creating_File, _Text_Error_While_Creating_File ) ;
end ;


function Text_Error_While_Accessing_File : string ;

begin
    Result := Get_Text( ID_Text_Error_While_Accessing_File, _Text_Error_While_Accessing_File ) ;
end ;


function Text_Status_Assembling : string ;

begin
    Result := Get_Text( ID_Text_Status_Assembling, _Text_Status_Assembling ) ;
end ;


function Text_Button_Caption_Abort : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Abort, _Text_Button_Caption_Abort ) ;
end ;


function Text_Button_Caption_Close : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Close, _Text_Button_Caption_Close ) ;
end ;


function Text_Menu_Caption_Reset_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Reset_amp, _Text_Menu_Caption_Reset_amp ) ;
end ;


function Text_Menu_Caption_Restart_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Restart_amp, _Text_Menu_Caption_Restart_amp ) ;
end ;


function Text_Menu_Caption_Embed : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Embed, _Text_Menu_Caption_Embed ) ;
end ;


function Text_Menu_Caption_Save_Contents : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Save_Contents, _Text_Menu_Caption_Save_Contents ) ;
end ;


function Text_Menu_Caption_Restore_Contents : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Restore_Contents, _Text_Menu_Caption_Restore_Contents ) ;
end ;


function Text_Menu_Caption_Dump : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Dump, _Text_Menu_Caption_Dump ) ;
end ;


function Text_Menu_Caption_Load : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Load, _Text_Menu_Caption_Load ) ;
end ;


function Text_Menu_Caption_Save_State : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Save_State, _Text_Menu_Caption_Save_State ) ;
end ;


function Text_Menu_Caption_Restore_State : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Restore_State, _Text_Menu_Caption_Restore_State ) ;
end ;


function Text_Menu_Caption_Profile_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Profile_amp, _Text_Menu_Caption_Profile_amp ) ;
end ;


function Text_Menu_Caption_Profile_Report : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Profile_Report, _Text_Menu_Caption_Profile_Report ) ;
end ;


function Text_Menu_Caption_Connections_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Connections_amp, _Text_Menu_Caption_Connections_amp ) ;
end ;


function Text_Menu_Caption_Condition_Handling_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Condition_Handling_amp, _Text_Menu_Caption_Condition_Handling_amp ) ;
end ;


function Text_Menu_Caption_Configure_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Configure_amp, _Text_Menu_Caption_Configure_amp ) ;
end ;


function Text_Menu_Caption_Unload_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Unload_amp, _Text_Menu_Caption_Unload_amp ) ;
end ;


function Text_Error_While_Opening_File : string ;

begin
    Result := Get_Text( ID_Text_Error_While_Opening_File, _Text_Error_While_Opening_File ) ;
end ;


function Text_Error_While_Reading_File : string ;

begin
    Result := Get_Text( ID_Text_Error_While_Reading_File, _Text_Error_While_Reading_File ) ;
end ;


function Text_Error_Cannot_Find_Component : string ;

begin
    Result := Get_Text( ID_Text_Error_Cannot_Find_Component, _Text_Error_Cannot_Find_Component ) ;
end ;


function Text_Error_Is_Not_A_Valid_Number_In_Radix : string ;

begin
    Result := Get_Text( ID_Text_Error_Is_Not_A_Valid_Number_In_Radix, _Text_Error_Is_Not_A_Valid_Number_In_Radix ) ;
end ;


function Text_Error_Expected_Equal : string ;

begin
    Result := Get_Text( ID_Text_Error_Expected_Equal, _Text_Error_Expected_Equal ) ;
end ;


function Text_Error_Is_Not_A_Valid_Switch_On_LOAD : string ;

begin
    Result := Get_Text( ID_Text_Error_Is_Not_A_Valid_Switch_On_LOAD, _Text_Error_Is_Not_A_Valid_Switch_On_LOAD ) ;
end ;


function Text_Error_Invalid_Radix : string ;

begin
    Result := Get_Text( ID_Text_Error_Invalid_Radix, _Text_Error_Invalid_Radix ) ;
end ;


function Text_Error_Unknown_Component_On_Unload : string ;

begin
    Result := Get_Text( ID_Text_Error_Unknown_Component_On_Unload, _Text_Error_Unknown_Component_On_Unload ) ;
end ;


function Text_Error_Unknown_Emulator_Command : string ;

begin
    Result := Get_Text( ID_Text_Error_Unknown_Emulator_Command, _Text_Error_Unknown_Emulator_Command ) ;
end ;


function Text_Caption_Profile : string ;

begin
    Result := Get_Text( ID_Text_Caption_Profile, _Text_Caption_Profile ) ;
end ;


function Text_Prompt_Include_Children_In_Change : string ;

begin
    Result := Get_Text( ID_Text_Prompt_Include_Children_In_Change, _Text_Prompt_Include_Children_In_Change ) ;
end ;


function Text_Error_Load_Failure_For : string ;

begin
    Result := Get_Text( ID_Text_Error_Load_Failure_For, _Text_Error_Load_Failure_For ) ;
end ;


function Text_Error_Not_A_Valid_CEF_Component : string ;

begin
    Result := Get_Text( ID_Text_Error_Not_A_Valid_CEF_Component, _Text_Error_Not_A_Valid_CEF_Component ) ;
end ;


function Text_Error_Component_Creation_Failure : string ;

begin
    Result := Get_Text( ID_Text_Error_Component_Creation_Failure, _Text_Error_Component_Creation_Failure ) ;
end ;


function Text_Error_Component_Is_An_Obsolete_Version : string ;

begin
    Result := Get_Text( ID_Text_Error_Component_Is_An_Obsolete_Version, _Text_Error_Component_Is_An_Obsolete_Version ) ;
end ;


function Text_Error_Component_Is_An_Unsupported_Version : string ;

begin
    Result := Get_Text( ID_Text_Error_Component_Is_An_Unsupported_Version, _Text_Error_Component_Is_An_Unsupported_Version ) ;
end ;


function Text_Source_Dialog_Filter : string ;

begin
    Result := Get_Text( ID_Text_Source_Dialog_Filter, _Text_Source_Dialog_Filter ) ;
end ;


function Text_Error_File_Contains_Invalid_Hexadecimal_Values : string ;

begin
    Result := Get_Text( ID_Text_Error_File_Contains_Invalid_Hexadecimal_Values, _Text_Error_File_Contains_Invalid_Hexadecimal_Values ) ;
end ;


function Text_CPU_Halted : string ;

begin
    Result := Get_Text( ID_Text_CPU_Halted, _Text_CPU_Halted ) ;
end ;


function Text_Signal_Active : string ;

begin
    Result := Get_Text( ID_Text_Signal_Active, _Text_Signal_Active ) ;
end ;


function Text_Signal_Inactive : string ;

begin
    Result := Get_Text( ID_Text_Signal_Inactive, _Text_Signal_Inactive ) ;
end ;


function Text_Error_Invalid_Floating_Point_Format : string ;

begin
    Result := Get_Text( ID_Text_Error_Invalid_Floating_Point_Format, _Text_Error_Invalid_Floating_Point_Format ) ;
end ;


function Text_Status_Modified : string ;

begin
    Result := Get_Text( ID_Text_Status_Modified, _Text_Status_Modified ) ;
end ;


function Text_Prompt_Save_Changes_To : string ;

begin
    Result := Get_Text( ID_Text_Prompt_Save_Changes_To, _Text_Prompt_Save_Changes_To ) ;
end ;


function Text_Caption_Modify_Register : string ;

begin
    Result := Get_Text( ID_Text_Caption_Modify_Register, _Text_Caption_Modify_Register ) ;
end ;


function Text_Status_Executing : string ;

begin
    Result := Get_Text( ID_Text_Status_Executing, _Text_Status_Executing ) ;
end ;


function Text_Status_Stopped : string ;

begin
    Result := Get_Text( ID_Text_Status_Stopped, _Text_Status_Stopped ) ;
end ;


function Text_Error_Could_Not_Modify_Memory : string ;

begin
    Result := Get_Text( ID_Text_Error_Could_Not_Modify_Memory, _Text_Error_Could_Not_Modify_Memory ) ;
end ;


function Text_Prompt_Delete_All_Watches : string ;

begin
    Result := Get_Text( ID_Text_Prompt_Delete_All_Watches, _Text_Prompt_Delete_All_Watches ) ;
end ;


function Text_No_Connections : string ;

begin
    Result := Get_Text( ID_Text_No_Connections, _Text_No_Connections ) ;
end ;


function Text_Caption_Emulator_Ports : string ;

begin
    Result := Get_Text( ID_Text_Caption_Emulator_Ports, _Text_Caption_Emulator_Ports ) ;
end ;


function Text_Error_No_Components_Responded : string ;

begin
    Result := Get_Text( ID_Text_Error_No_Components_Responded, _Text_Error_No_Components_Responded ) ;
end ;


function Text_Response : string ;

begin
    Result := Get_Text( ID_Text_Response, _Text_Response ) ;
end ;


function Text_File_Filter_All_Files : string ;

begin
    Result := Get_Text( ID_Text_File_Filter_All_Files, _Text_File_Filter_All_Files ) ;
end ;


function Text_File_Filter_Source_Files : string ;

begin
    Result := Get_Text( ID_Text_File_Filter_Source_Files, _Text_File_Filter_Source_Files ) ;
end ;


function Text_File_Filter_Text_Files : string ;

begin
    Result := Get_Text( ID_Text_File_Filter_Text_Files, _Text_File_Filter_Text_Files ) ;
end ;


function Text_Immediate_Mode : string ;

begin
    Result := Get_Text( ID_Text_Immediate_Mode, _Text_Immediate_Mode ) ;
end ;


function Text_Memory : string ;

begin
    Result := Get_Text( ID_Text_Memory, _Text_Memory ) ;
end ;


function Text_Ports : string ;

begin
    Result := Get_Text( ID_Text_Ports, _Text_Ports ) ;
end ;


function Text_Menu_Caption_File_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_File_amp, _Text_Menu_Caption_File_amp ) ;
end ;


function Text_Menu_Caption_Open_Emulator_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Open_Emulator_amp, _Text_Menu_Caption_Open_Emulator_amp ) ;
end ;


function Text_Menu_Caption_Reopen_Emulator : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Reopen_Emulator, _Text_Menu_Caption_Reopen_Emulator ) ;
end ;


function Text_Menu_Caption_New_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_New_amp, _Text_Menu_Caption_New_amp ) ;
end ;


function Text_Menu_Caption_Open_Source_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Open_Source_amp, _Text_Menu_Caption_Open_Source_amp ) ;
end ;


function Text_Menu_Caption_Reopen_Source : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Reopen_Source, _Text_Menu_Caption_Reopen_Source ) ;
end ;


function Text_Menu_Caption_Save_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Save_amp, _Text_Menu_Caption_Save_amp ) ;
end ;


function Text_Menu_Caption_Save_As_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Save_As_amp, _Text_Menu_Caption_Save_As_amp ) ;
end ;


function Text_Menu_Caption_Save_All : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Save_All, _Text_Menu_Caption_Save_All ) ;
end ;


function Text_Menu_Caption_Media_Manager_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Media_Manager_amp, _Text_Menu_Caption_Media_Manager_amp ) ;
end ;


function Text_Menu_Caption_Close_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Close_amp, _Text_Menu_Caption_Close_amp ) ;
end ;


function Text_Menu_Caption_Close_All : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Close_All, _Text_Menu_Caption_Close_All ) ;
end ;


function Text_Menu_Caption_Printer_Setup : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Printer_Setup, _Text_Menu_Caption_Printer_Setup ) ;
end ;


function Text_Menu_Caption_Print_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Print_amp, _Text_Menu_Caption_Print_amp ) ;
end ;


function Text_Menu_Caption_Exit_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Exit_amp, _Text_Menu_Caption_Exit_amp ) ;
end ;


function Text_Menu_Caption_Components_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Components_amp, _Text_Menu_Caption_Components_amp ) ;
end ;


function Text_Menu_Caption_Load_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Load_amp, _Text_Menu_Caption_Load_amp ) ;
end ;


function Text_Menu_Caption_Unload_All_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Unload_All_amp, _Text_Menu_Caption_Unload_All_amp ) ;
end ;


function Text_Menu_Caption_Emulator_Ports_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Emulator_Ports_amp, _Text_Menu_Caption_Emulator_Ports_amp ) ;
end ;


function Text_Menu_Caption_Clock_Mode_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Clock_Mode_amp, _Text_Menu_Caption_Clock_Mode_amp ) ;
end ;


function Text_Menu_Caption_Default_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Default_amp, _Text_Menu_Caption_Default_amp ) ;
end ;


function Text_Menu_Caption_Ignore_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Ignore_amp, _Text_Menu_Caption_Ignore_amp ) ;
end ;


function Text_Menu_Caption_Synchronize_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Synchronize_amp, _Text_Menu_Caption_Synchronize_amp ) ;
end ;


function Text_Menu_Caption_Disable_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Disable_amp, _Text_Menu_Caption_Disable_amp ) ;
end ;


function Text_Menu_Caption_Unblock_All_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Unblock_All_amp, _Text_Menu_Caption_Unblock_All_amp ) ;
end ;


function Text_Menu_Caption_Default_Memory_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Default_Memory_amp, _Text_Menu_Caption_Default_Memory_amp ) ;
end ;


function Text_Menu_Caption_Assemble_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Assemble_amp, _Text_Menu_Caption_Assemble_amp ) ;
end ;


function Text_Watchpoint_Watchpoint : string ;

begin
    Result := Get_Text( ID_Text_Watchpoint_Watchpoint, _Text_Watchpoint_Watchpoint ) ;
end ;


function Text_Menu_Caption_Assemble_All_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Assemble_All_amp, _Text_Menu_Caption_Assemble_All_amp ) ;
end ;


function Text_Menu_Caption_Show_Errors_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Show_Errors_amp, _Text_Menu_Caption_Show_Errors_amp ) ;
end ;


function Text_Menu_Caption_Run_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Run_amp, _Text_Menu_Caption_Run_amp ) ;
end ;


function Text_Menu_Caption_Execute_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Execute_amp, _Text_Menu_Caption_Execute_amp ) ;
end ;


function Text_Menu_Caption_Step_Over_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Step_Over_amp, _Text_Menu_Caption_Step_Over_amp ) ;
end ;


function Text_Menu_Caption_Step_Into_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Step_Into_amp, _Text_Menu_Caption_Step_Into_amp ) ;
end ;


function Text_Menu_Caption_Program_Pause_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Program_Pause_amp, _Text_Menu_Caption_Program_Pause_amp ) ;
end ;


function Text_Menu_Caption_Run_Immediate_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Run_Immediate_amp, _Text_Menu_Caption_Run_Immediate_amp ) ;
end ;


function Text_Menu_Caption_Add_Watch_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Add_Watch_amp, _Text_Menu_Caption_Add_Watch_amp ) ;
end ;


function Text_Menu_Caption_Add_Execution_Breakpoint_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Add_Execution_Breakpoint_amp, _Text_Menu_Caption_Add_Execution_Breakpoint_amp ) ;
end ;


function Text_Menu_Caption_Execution_Breakpoints : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Execution_Breakpoints, _Text_Menu_Caption_Execution_Breakpoints ) ;
end ;


function Text_Menu_Caption_Add_Port_Breakpoint : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Add_Port_Breakpoint, _Text_Menu_Caption_Add_Port_Breakpoint ) ;
end ;


function Text_Menu_Caption_Port_Breakpoints : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Port_Breakpoints, _Text_Menu_Caption_Port_Breakpoints ) ;
end ;


function Text_Menu_Caption_Profiling : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Profiling, _Text_Menu_Caption_Profiling ) ;
end ;


function Text_Menu_Caption_Profile_Report_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Profile_Report_amp, _Text_Menu_Caption_Profile_Report_amp ) ;
end ;


function Text_Menu_Caption_Trace : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Trace, _Text_Menu_Caption_Trace ) ;
end ;


function Text_Menu_Caption_Trace_Log_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Trace_Log_amp, _Text_Menu_Caption_Trace_Log_amp ) ;
end ;


function Text_Menu_Caption_Internals_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Internals_amp, _Text_Menu_Caption_Internals_amp ) ;
end ;


function Text_Menu_Caption_Options_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Options_amp, _Text_Menu_Caption_Options_amp ) ;
end ;


function Text_Menu_Caption_Help_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Help_amp, _Text_Menu_Caption_Help_amp ) ;
end ;


function Text_Menu_Caption_CEF_Specification : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_CEF_Specification, _Text_Menu_Caption_CEF_Specification ) ;
end ;


function Text_Menu_Caption_Component_Help : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Component_Help, _Text_Menu_Caption_Component_Help ) ;
end ;


function Text_Menu_Caption_About_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_About_amp, _Text_Menu_Caption_About_amp ) ;
end ;


function Text_File_Filter_CEF_Files : string ;

begin
    Result := Get_Text( ID_Text_File_Filter_CEF_Files, _Text_File_Filter_CEF_Files ) ;
end ;


function Text_Menu_Caption_Open_Emulator : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Open_Emulator, _Text_Menu_Caption_Open_Emulator ) ;
end ;


function Text_Menu_Caption_Radix_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Radix_amp, _Text_Menu_Caption_Radix_amp ) ;
end ;


function Text_Menu_Caption_Binary_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Binary_amp, _Text_Menu_Caption_Binary_amp ) ;
end ;


function Text_Menu_Caption_Octal_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Octal_amp, _Text_Menu_Caption_Octal_amp ) ;
end ;


function Text_Menu_Caption_Decimal_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Decimal_amp, _Text_Menu_Caption_Decimal_amp ) ;
end ;


function Text_Menu_Caption_Hexadecimal : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Hexadecimal, _Text_Menu_Caption_Hexadecimal ) ;
end ;


function Text_Menu_Caption_Radix_50 : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Radix_50, _Text_Menu_Caption_Radix_50 ) ;
end ;


function Text_Menu_Caption_Other : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Other, _Text_Menu_Caption_Other ) ;
end ;


function Text_Menu_Caption_Signed_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Signed_amp, _Text_Menu_Caption_Signed_amp ) ;
end ;


function Text_Menu_Caption_Size_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Size_amp, _Text_Menu_Caption_Size_amp ) ;
end ;


function Text_Menu_Caption_Byte_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Byte_amp, _Text_Menu_Caption_Byte_amp ) ;
end ;


function Text_Menu_Caption_Word_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Word_amp, _Text_Menu_Caption_Word_amp ) ;
end ;


function Text_Menu_Caption_Long_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Long_amp, _Text_Menu_Caption_Long_amp ) ;
end ;


function Text_Menu_Caption_Quad_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Quad_amp, _Text_Menu_Caption_Quad_amp ) ;
end ;


function Text_Menu_Caption_EBCDIC_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_EBCDIC_amp, _Text_Menu_Caption_EBCDIC_amp ) ;
end ;


function Text_Menu_Caption_Physical_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Physical_amp, _Text_Menu_Caption_Physical_amp ) ;
end ;


function Text_Menu_Caption_Address_Space : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Address_Space, _Text_Menu_Caption_Address_Space ) ;
end ;


function Text_Menu_Caption_Goto_Address_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Goto_Address_amp, _Text_Menu_Caption_Goto_Address_amp ) ;
end ;


function Text_Menu_Caption_Find_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Find_amp, _Text_Menu_Caption_Find_amp ) ;
end ;


function Text_Menu_Caption_Modify_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Modify_amp, _Text_Menu_Caption_Modify_amp ) ;
end ;


function Text_Menu_Caption_Watchpoints : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Watchpoints, _Text_Menu_Caption_Watchpoints ) ;
end ;


function Text_Menu_Caption_Create_New : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Create_New, _Text_Menu_Caption_Create_New ) ;
end ;


function Text_Menu_Caption_View : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_View, _Text_Menu_Caption_View ) ;
end ;


function Text_Menu_Caption_Pattern_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Pattern_amp, _Text_Menu_Caption_Pattern_amp ) ;
end ;


function Text_File_Filter_CEF_Memory_Contents : string ;

begin
    Result := Get_Text( ID_Text_File_Filter_CEF_Memory_Contents, _Text_File_Filter_CEF_Memory_Contents ) ;
end ;


function Text_Menu_Caption_Save_Memory_Contents : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Save_Memory_Contents, _Text_Menu_Caption_Save_Memory_Contents ) ;
end ;


function Text_File_Filter_CEF_Memory_State : string ;

begin
    Result := Get_Text( ID_Text_File_Filter_CEF_Memory_State, _Text_File_Filter_CEF_Memory_State ) ;
end ;


function Text_Menu_Caption_Save_Memory_State : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Save_Memory_State, _Text_Menu_Caption_Save_Memory_State ) ;
end ;


function Text_Menu_Caption_Restore_Memory : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Restore_Memory, _Text_Menu_Caption_Restore_Memory ) ;
end ;


function Text_Menu_Caption_Restore_Memory_State : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Restore_Memory_State, _Text_Menu_Caption_Restore_Memory_State ) ;
end ;


function Text_File_Filter_Dynamic_Link_Libraries : string ;

begin
    Result := Get_Text( ID_Text_File_Filter_Dynamic_Link_Libraries, _Text_File_Filter_Dynamic_Link_Libraries ) ;
end ;


function Text_Menu_Caption_Open_Component : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Open_Component, _Text_Menu_Caption_Open_Component ) ;
end ;


function Text_Menu_Caption_Increment_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Increment_amp, _Text_Menu_Caption_Increment_amp ) ;
end ;


function Text_Menu_Caption_Decrement_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Decrement_amp, _Text_Menu_Caption_Decrement_amp ) ;
end ;


function Text_Menu_Caption_Copy : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Copy, _Text_Menu_Caption_Copy ) ;
end ;


function Text_Menu_Caption_Paste : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Paste, _Text_Menu_Caption_Paste ) ;
end ;


function Text_Menu_Caption_Change_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Change_amp, _Text_Menu_Caption_Change_amp ) ;
end ;


function Text_Menu_Caption_Zero_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Zero_amp, _Text_Menu_Caption_Zero_amp ) ;
end ;


function Text_Menu_Caption_Add_Register_Breakpoint_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Add_Register_Breakpoint_amp, _Text_Menu_Caption_Add_Register_Breakpoint_amp ) ;
end ;


function Text_Menu_Caption_Register_Breakpoints_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Register_Breakpoints_amp, _Text_Menu_Caption_Register_Breakpoints_amp ) ;
end ;


function Text_Menu_Caption_Add_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Add_amp, _Text_Menu_Caption_Add_amp ) ;
end ;


function Text_Menu_Caption_Edit_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Edit_amp, _Text_Menu_Caption_Edit_amp ) ;
end ;


function Text_Menu_Caption_Delete_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Delete_amp, _Text_Menu_Caption_Delete_amp ) ;
end ;


function Text_Menu_Caption_Delete_All_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Delete_All_amp, _Text_Menu_Caption_Delete_All_amp ) ;
end ;


function Text_Menu_Caption_Show_Source_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Show_Source_amp, _Text_Menu_Caption_Show_Source_amp ) ;
end ;


function Text_Menu_Caption_Goto_Current_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Goto_Current_amp, _Text_Menu_Caption_Goto_Current_amp ) ;
end ;


function Text_Menu_Caption_Top_Of_Stack_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Top_Of_Stack_amp, _Text_Menu_Caption_Top_Of_Stack_amp ) ;
end ;


function Text_Menu_Caption_Open_Source : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Open_Source, _Text_Menu_Caption_Open_Source ) ;
end ;


function Text_Menu_Caption_Goto_Port_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Goto_Port_amp, _Text_Menu_Caption_Goto_Port_amp ) ;
end ;


function Text_Menu_Caption_Input_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Input_amp, _Text_Menu_Caption_Input_amp ) ;
end ;


function Text_Menu_Caption_Output_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_Output_amp, _Text_Menu_Caption_Output_amp ) ;
end ;


function Text_Caption_Conditions : string ;

begin
    Result := Get_Text( ID_Text_Caption_Conditions, _Text_Caption_Conditions ) ;
end ;


function Text_Button_Caption_OK_amp : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_OK_amp, _Text_Button_Caption_OK_amp ) ;
end ;


function Text_Button_Caption_Cancel_amp : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Cancel_amp, _Text_Button_Caption_Cancel_amp ) ;
end ;


function Text_Button_Caption_Help_amp : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Help_amp, _Text_Button_Caption_Help_amp ) ;
end ;


function Text_Caption_States : string ;

begin
    Result := Get_Text( ID_Text_Caption_States, _Text_Caption_States ) ;
end ;


function Text_Caption_Signals : string ;

begin
    Result := Get_Text( ID_Text_Caption_Signals, _Text_Caption_Signals ) ;
end ;


function Text_Caption_Exceptions : string ;

begin
    Result := Get_Text( ID_Text_Caption_Exceptions, _Text_Caption_Exceptions ) ;
end ;


function Text_Caption_Errors : string ;

begin
    Result := Get_Text( ID_Text_Caption_Errors, _Text_Caption_Errors ) ;
end ;


function Text_Caption_Break_On_Informational_Messagaes : string ;

begin
    Result := Get_Text( ID_Text_Caption_Break_On_Informational_Messagaes, _Text_Caption_Break_On_Informational_Messagaes ) ;
end ;


function Text_Caption_Break_On_Warnings : string ;

begin
    Result := Get_Text( ID_Text_Caption_Break_On_Warnings, _Text_Caption_Break_On_Warnings ) ;
end ;


function Text_Caption_Break_On_Errors : string ;

begin
    Result := Get_Text( ID_Text_Caption_Break_On_Errors, _Text_Caption_Break_On_Errors ) ;
end ;


function Text_Caption_About : string ;

begin
    Result := Get_Text( ID_Text_Caption_About, _Text_Caption_About ) ;
end ;


function Text_Computer_Emulation_Framework : string ;

begin
    Result := Get_Text( ID_Text_Computer_Emulation_Framework, _Text_Computer_Emulation_Framework ) ;
end ;


function Text_Generic_Computer_Emulator : string ;

begin
    Result := Get_Text( ID_Text_Generic_Computer_Emulator, _Text_Generic_Computer_Emulator ) ;
end ;


function Text_Menu_Caption_ASCII_amp : string ;

begin
    Result := Get_Text( ID_Text_Menu_Caption_ASCII_amp, _Text_Menu_Caption_ASCII_amp ) ;
end ;


function Text_Value_Colon : string ;

begin
    Result := Get_Text( ID_Text_Value_Colon, _Text_Value_Colon ) ;
end ;


function Text_Caption_Configure_Component : string ;

begin
    Result := Get_Text( ID_Text_Caption_Configure_Component, _Text_Caption_Configure_Component ) ;
end ;


function Text_Caption_Attributes : string ;

begin
    Result := Get_Text( ID_Text_Caption_Attributes, _Text_Caption_Attributes ) ;
end ;


function Text_Latency : string ;

begin
    Result := Get_Text( ID_Text_Latency, _Text_Latency ) ;
end ;


function Text_Read_Colon : string ;

begin
    Result := Get_Text( ID_Text_Read_Colon, _Text_Read_Colon ) ;
end ;


function Text_Write_Colon : string ;

begin
    Result := Get_Text( ID_Text_Write_Colon, _Text_Write_Colon ) ;
end ;


function Text_CPU : string ;

begin
    Result := Get_Text( ID_Text_CPU, _Text_CPU ) ;
end ;


function Text_Clock_Colon : string ;

begin
    Result := Get_Text( ID_Text_Clock_Colon, _Text_Clock_Colon ) ;
end ;


function Text_Clock_Speed : string ;

begin
    Result := Get_Text( ID_Text_Clock_Speed, _Text_Clock_Speed ) ;
end ;


function Text_Low_Address_Colon : string ;

begin
    Result := Get_Text( ID_Text_Low_Address_Colon, _Text_Low_Address_Colon ) ;
end ;


function Text_High_Address_Colon : string ;

begin
    Result := Get_Text( ID_Text_High_Address_Colon, _Text_High_Address_Colon ) ;
end ;


function Text_Caption_Options : string ;

begin
    Result := Get_Text( ID_Text_Caption_Options, _Text_Caption_Options ) ;
end ;


function Text_Button_Caption_OK : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_OK, _Text_Button_Caption_OK ) ;
end ;


function Text_Button_Caption_Cancel : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Cancel, _Text_Button_Caption_Cancel ) ;
end ;


function Text_Caption_Assembler : string ;

begin
    Result := Get_Text( ID_Text_Caption_Assembler, _Text_Caption_Assembler ) ;
end ;


function Text_Generate_List_Files : string ;

begin
    Result := Get_Text( ID_Text_Generate_List_Files, _Text_Generate_List_Files ) ;
end ;


function Text_Generate_Symbol_Table : string ;

begin
    Result := Get_Text( ID_Text_Generate_Symbol_Table, _Text_Generate_Symbol_Table ) ;
end ;


function Text_Generate_Cross_Reference_List : string ;

begin
    Result := Get_Text( ID_Text_Generate_Cross_Reference_List, _Text_Generate_Cross_Reference_List ) ;
end ;


function Text_Assemble_Into_Physical_Address_Space : string ;

begin
    Result := Get_Text( ID_Text_Assemble_Into_Physical_Address_Space, _Text_Assemble_Into_Physical_Address_Space ) ;
end ;


function Text_Caption_Threading : string ;

begin
    Result := Get_Text( ID_Text_Caption_Threading, _Text_Caption_Threading ) ;
end ;


function Text_Max_Threads : string ;

begin
    Result := Get_Text( ID_Text_Max_Threads, _Text_Max_Threads ) ;
end ;


function Text_Thread_Priority : string ;

begin
    Result := Get_Text( ID_Text_Thread_Priority, _Text_Thread_Priority ) ;
end ;


function Text_Normal : string ;

begin
    Result := Get_Text( ID_Text_Normal, _Text_Normal ) ;
end ;


function Text_Idle_Only_Not_Recommended : string ;

begin
    Result := Get_Text( ID_Text_Idle_Only_Not_Recommended, _Text_Idle_Only_Not_Recommended ) ;
end ;


function Text_Lower : string ;

begin
    Result := Get_Text( ID_Text_Lower, _Text_Lower ) ;
end ;


function Text_Low : string ;

begin
    Result := Get_Text( ID_Text_Low, _Text_Low ) ;
end ;


function Text_Synchronize : string ;

begin
    Result := Get_Text( ID_Text_Synchronize, _Text_Synchronize ) ;
end ;


function Text_High : string ;

begin
    Result := Get_Text( ID_Text_High, _Text_High ) ;
end ;


function Text_Higher : string ;

begin
    Result := Get_Text( ID_Text_Higher, _Text_Higher ) ;
end ;


function Text_Real_Time_Not_Recommended : string ;

begin
    Result := Get_Text( ID_Text_Real_Time_Not_Recommended, _Text_Real_Time_Not_Recommended ) ;
end ;


function Text_Caption_Master_Clock : string ;

begin
    Result := Get_Text( ID_Text_Caption_Master_Clock, _Text_Caption_Master_Clock ) ;
end ;


function Text_Mode : string ;

begin
    Result := Get_Text( ID_Text_Mode, _Text_Mode ) ;
end ;


function Text_Unblock_All_Components_After_Immediate_Mode_Execution : string ;

begin
    Result := Get_Text( ID_Text_Unblock_All_Components_After_Immediate_Mode_Execution, _Text_Unblock_All_Components_After_Immediate_Mode_Execution ) ;
end ;


function Text_Enabled : string ;

begin
    Result := Get_Text( ID_Text_Enabled, _Text_Enabled ) ;
end ;


function Text_Default : string ;

begin
    Result := Get_Text( ID_Text_Default, _Text_Default ) ;
end ;


function Text_Ignore : string ;

begin
    Result := Get_Text( ID_Text_Ignore, _Text_Ignore ) ;
end ;


function Text_Button_Caption_Skip : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Skip, _Text_Button_Caption_Skip ) ;
end ;


function Text_Button_Caption_Skip_All : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Skip_All, _Text_Button_Caption_Skip_All ) ;
end ;


function Text_Button_Caption_Retry : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Retry, _Text_Button_Caption_Retry ) ;
end ;


function Text_File_Already_Exists : string ;

begin
    Result := Get_Text( ID_Text_File_Already_Exists, _Text_File_Already_Exists ) ;
end ;


function Text_Button_Caption_Overwrite : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Overwrite, _Text_Button_Caption_Overwrite ) ;
end ;


function Text_Button_Caption_Overwrite_All : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Overwrite_All, _Text_Button_Caption_Overwrite_All ) ;
end ;


function Text_Caption_Port_IO : string ;

begin
    Result := Get_Text( ID_Text_Caption_Port_IO, _Text_Caption_Port_IO ) ;
end ;


function Text_Port_Colon : string ;

begin
    Result := Get_Text( ID_Text_Port_Colon, _Text_Port_Colon ) ;
end ;


function Text_Size_Bits_Colon : string ;

begin
    Result := Get_Text( ID_Text_Size_Bits_Colon, _Text_Size_Bits_Colon ) ;
end ;


function Text_Caption_Profile_Report : string ;

begin
    Result := Get_Text( ID_Text_Caption_Profile_Report, _Text_Caption_Profile_Report ) ;
end ;


function Text_Button_Caption_Clear : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Clear, _Text_Button_Caption_Clear ) ;
end ;


function Text_Button_Caption_Clear_All : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Clear_All, _Text_Button_Caption_Clear_All ) ;
end ;


function Text_Button_Caption_Help : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Help, _Text_Button_Caption_Help ) ;
end ;


function Text_Caption_Add_Data_To_Tape : string ;

begin
    Result := Get_Text( ID_Text_Caption_Add_Data_To_Tape, _Text_Caption_Add_Data_To_Tape ) ;
end ;


function Text_Caption_Import_Data_From_File : string ;

begin
    Result := Get_Text( ID_Text_Caption_Import_Data_From_File, _Text_Caption_Import_Data_From_File ) ;
end ;


function Text_Button_Caption_Add_From_File : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Add_From_File, _Text_Button_Caption_Add_From_File ) ;
end ;


function Text_Enter_Data_To_Add : string ;

begin
    Result := Get_Text( ID_Text_Enter_Data_To_Add, _Text_Enter_Data_To_Add ) ;
end ;


function Text_Max_Bytes : string ;

begin
    Result := Get_Text( ID_Text_Max_Bytes, _Text_Max_Bytes ) ;
end ;


function Text_Max_Block_Size_In_Bytes : string ;

begin
    Result := Get_Text( ID_Text_Max_Block_Size_In_Bytes, _Text_Max_Block_Size_In_Bytes ) ;
end ;


function Text_Min_Block_Size_In_Bytes : string ;

begin
    Result := Get_Text( ID_Text_Min_Block_Size_In_Bytes, _Text_Min_Block_Size_In_Bytes ) ;
end ;


function Text_Caption_Traces : string ;

begin
    Result := Get_Text( ID_Text_Caption_Traces, _Text_Caption_Traces ) ;
end ;


function Text_Maximum_Events_To_Trace : string ;

begin
    Result := Get_Text( ID_Text_Maximum_Events_To_Trace, _Text_Maximum_Events_To_Trace ) ;
end ;


function Text_Caption_Trace_Log : string ;

begin
    Result := Get_Text( ID_Text_Caption_Trace_Log, _Text_Caption_Trace_Log ) ;
end ;


function Text_Button_Caption_Save : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Save, _Text_Button_Caption_Save ) ;
end ;


function Text_File_Mask : string ;

begin
    Result := Get_Text( ID_Text_File_Mask, _Text_File_Mask ) ;
end ;


function Text_Caption_Save_Log_To_File : string ;

begin
    Result := Get_Text( ID_Text_Caption_Save_Log_To_File, _Text_Caption_Save_Log_To_File ) ;
end ;


function Text_Caption_Tape_Media_Information : string ;

begin
    Result := Get_Text( ID_Text_Caption_Tape_Media_Information, _Text_Caption_Tape_Media_Information ) ;
end ;


function Text_Format_Colon : string ;

begin
    Result := Get_Text( ID_Text_Format_Colon, _Text_Format_Colon ) ;
end ;


function Text_BPI : string ;

begin
    Result := Get_Text( ID_Text_BPI, _Text_BPI ) ;
end ;


function Text_0_Undefined : string ;

begin
    Result := Get_Text( ID_Text_0_Undefined, _Text_0_Undefined ) ;
end ;


function Text_Length_Colon : string ;

begin
    Result := Get_Text( ID_Text_Length_Colon, _Text_Length_Colon ) ;
end ;


function Text_0_Infinite : string ;

begin
    Result := Get_Text( ID_Text_0_Infinite, _Text_0_Infinite ) ;
end ;


function Text_Feet : string ;

begin
    Result := Get_Text( ID_Text_Feet, _Text_Feet ) ;
end ;


function Text_Button_Caption_Defaults : string ;

begin
    Result := Get_Text( ID_Text_Button_Caption_Defaults, _Text_Button_Caption_Defaults ) ;
end ;


function Text_IRG_Length_Colon : string ;

begin
    Result := Get_Text( ID_Text_IRG_Length_Colon, _Text_IRG_Length_Colon ) ;
end ;


function Text_TM_Length_Colon : string ;

begin
    Result := Get_Text( ID_Text_TM_Length_Colon, _Text_TM_Length_Colon ) ;
end ;


function Text_1_1000 : string ;

begin
    Result := Get_Text( ID_Text_1_1000, _Text_1_1000 ) ;
end ;


function Text_Internal_Format : string ;

begin
    Result := Get_Text( ID_Text_Internal_Format, _Text_Internal_Format ) ;
end ;


function Text_Note_Changing_These_Settings_Affects_The_Internal_Media_File_Data : string ;

begin
    Result := Get_Text( ID_Text_Note_Changing_These_Settings_Affects_The_Internal_Media_File_Data,
        _Text_Note_Changing_These_Settings_Affects_The_Internal_Media_File_Data ) ;
end ;


function Text_Size_Record_Length_Colon : string ;

begin
    Result := Get_Text( ID_Text_Size_Record_Length_Colon, _Text_Size_Record_Length_Colon ) ;
end ;


function Text_Include_Size_Records_Before_And_After_Records : string ;

begin
    Result := Get_Text( ID_Text_Include_Size_Records_Before_And_After_Records,
        _Text_Include_Size_Records_Before_And_After_Records ) ;
end ;


function Text_Change_Port_Configuration : string ;

begin
    Result := Get_Text( ID_Text_Change_Port_Configuration,
        _Text_Change_Port_Configuration ) ;
end ;


procedure Initialize_Unit ;

var F : text ;
    L, S : string ;

begin
    L := '' ;
    assignfile( F, OS^.Application_Path + 'CEF32.ini' ) ;
    {$I-}
    reset( F ) ;
    {$I+}
    if( IOResult <> 0 ) then
    begin
        exit ;
    end ;
    try
        while( not eof( F ) ) do
        begin
            readln( F, S ) ;
            S := uppercase( S ) ;
            if( copy( S, 1, 9 ) = 'LANGUAGE=' ) then
            begin
                L := copy( S, 10, length( S ) ) ;
                if( L <> '' ) then
                begin
                    break
                end ;
            end ;
        end ;
    finally
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
    end ;
    if( L <> '' ) then
    begin
        Set_Language( L ) ;
    end ;
end ;


initialization
    Text_Strings := nil ;
    Initialize_Unit ;
end.
