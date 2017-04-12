{
        Program Name : CEF_Util
        Package Name : CEF
        Purpose      : Utility routines for CEF32
        Institution  : Conroy & Conroy Co.
        Date Written : 13-July-2005
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2005-2007 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

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

          This library provides support to CEF32 and components thereof.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

library CEF_Util ;

uses // Borland...
     Forms,

     // C&C...
     _UE, // TUnified_Exception
     _Streams, // TCOM_Stream
     FStreams, // TCOM_File_Stream

     // CEF...
     _CEFUtil,
     _CEF, // Interface_Version
     CEF, // TBase_Assembler_Context
     util in 'util.pas',
     Goto_Page_Form in '..\LA36\Goto_Page_Form.pas' {Goto_Page_Dialog},
     CEF_Assembler in 'CEF_Assembler.pas',
     Create_Watchpoint in 'Create_Watchpoint.pas' {New_Watchpoint},
     Mem_Watchpoints_Dialog in 'Mem_Watchpoints_Dialog.pas' {Memory_Watchpoints_Dialog};

{$R *.res}

// Unit Exported functions...

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ; stdcall ;

begin
    Result := TStandard_Watchpoint_Manager.Create ;
end ;


function Version : longint ; stdcall ;

begin
    Result := Interface_Version ;
end ;


function Get_Character_Set : TCEF_Character_Set ; stdcall ;

begin
    Result := TCharacter_Set.Create ;
end ;


function Get_Key_Mapper : TCEF_Key_Mapper ; stdcall ;

begin
    Result := TKey_Mapper.Create ;
end ;


function Get_Assembler_Context : TCEF_Assembler_Context ; stdcall ;

begin
    Result := TAssembler_Context.Create ;
end ;


function Get_Master_Assembler( CPU : TComponent ;
    UI : TUI_Interface ) : TMaster_Assembler ; stdcall ;

begin
    Result := TCEF_Assembler.Create( CPU, UI ) ;
end ;


procedure Add_Breakpoint( UI : TUI_Interface ; Component : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Base, Size : longint ;
    Port : boolean ) ; stdcall ;

begin
    Util.Add_Breakpoint( UI, Component, Watchpoints, Base, Size, Port ) ;
end ;


procedure Add_Watchpoint( UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ) ; stdcall ;

begin
    Util.Add_Watchpoint( UI, Component, Component_Type, Watchpoints, Base, Size,
        Low, High, Memory, Address, Access, nil, nil ) ;
end ;


procedure Add_Watchpoint_Ex( UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ; Context : pointer ;
    Domain : PChar ) ; stdcall ;

begin
    Util.Add_Watchpoint( UI, Component, Component_Type, Watchpoints, Base, Size,
        Low, High, Memory, Address, Access, Context, Domain ) ;
end ;



procedure Show_Watchpoints( UI : TUI_Interface ; CPU : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Ports : boolean ) ; stdcall ;

begin
    Util.Show_Watchpoints( UI, CPU, Watchpoints, Ports ) ;
end ;


function Create_Logger( UI : TUI_Interface ; Filename : PChar ;
    Flags : longint ) : TCEF_Logger ; stdcall ;

begin
    Result := Util.Create_Logger( UI, string( Filename ), Flags ) ;
end ;


var Temp : string ;

function Translate_Serial_Data( Source : PChar ; var Size : longint ;
    Source_Speed, Target_Speed : int64 ; Data_Bits, Stop_Bits : longint ) : PChar ;
    stdcall ;

begin
    setlength( Temp, Size ) ;
    move( Source[ 0 ], PChar( Temp )[ 0 ], Size ) ;
    Temp := Util.Translate_Serial_Data( Temp, Source_Speed, Target_Speed, Data_Bits, Stop_Bits ) ;
    Result := PChar( Temp ) ;
    Size := length( Temp ) ;
end ;


function Create_File_Stream( Name : PChar ; var UEC : PSUnified_Exception ) : TCOM_Stream ; stdcall ;

begin
    Result := FStreams.Create_File_Stream( Name, UEC ) ;
end ;



{$WARNINGS OFF}
exports Version index 0,
        Get_Watchpoint_Manager index 1,
        Get_Character_Set index 2,
        Get_Key_Mapper index 3,
        Get_Assembler_Context index 4,
        Get_Master_Assembler index 5,
        Add_Breakpoint index 6, // V2.5
        Add_Watchpoint index 7, // V2.5
        Show_Watchpoints index 8, // V2.5
        Add_Watchpoint_Ex index 9, // V2.6
        Create_Logger index 10, // V2.6
        Translate_Serial_Data index 11, // V2.6
        Create_File_Stream index 12 ; // V2.6
{$WARNINGS ON}

var F : TForm ;

begin
    Application.Initialize ;
    Application.CreateForm( TForm, F ) ;
    F.Visible := False ;
    { Delphi kills the DLL when the first created form is closed, so create this
      form so if any dialogs are destroyed, the DLL remains. }
end.
