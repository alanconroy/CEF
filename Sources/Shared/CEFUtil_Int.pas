{$N+}
{
        Program Name : CEFUtil
        Package Name : CEF
        Purpose      : CEF_Util interface
        Institution  :
        Date Written : 2-Jan-2007
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

          This unit is the interface to the CEFUtil.dll.

}

unit CEFUtil_Int ;

interface

uses // Borland...
     Windows,

     // Other...
     _Streams, // TCOM_Stream
     _UE, // TUnified_Exception

     // CEF..
     _CEFUtil,
     _CEF ; // TComponent

function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ;

function Get_Master_Assembler( CPU : TComponent ;
    UI : TUI_Interface ) : TMaster_Assembler ;

procedure Add_Breakpoint( _UI : TUI_Interface ; Component : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Base, Size : longint ;
    Port : boolean ) ;

procedure Add_Watchpoint_Ex( _UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ; Context : pointer ;
    const Domain : string ) ;

procedure Show_Watchpoints( _UI : TUI_Interface ; CPU : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Ports : boolean ) ;

function Create_File_Stream( Name : string ; var UEC : TUnified_Exception ) : TCOM_Stream ;

function Translate_Serial_Data( Source : string ;
    Source_Speed, Target_Speed : int64 ; Data_Bits, Stop_Bits : longint ) : string ;

function Get_Character_Set : TCEF_Character_Set ;


implementation

uses // C&C...
     Parse ;

var FCreate_File_Stream : function( Name : PChar ; var UEC : TUnified_Exception ) : TCOM_Stream ; stdcall = nil ;

var FGet_Watchpoint_Manager : function() : TCEF_Watchpoint_Manager ; stdcall = nil ;

var FGet_Master_Assembler : function( CPU : TComponent ; UI : TUI_Interface ) : TMaster_Assembler ; stdcall = nil ;

var FAdd_Breakpoint : procedure( _UI : TUI_Interface ; Component : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Base, Size : longint ;
    Port : boolean ) ; stdcall = nil ;

var FAdd_Watchpoint_Ex : procedure( _UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ; Context : pointer ;
    Domain : PChar ) ; stdcall = nil ;

var FShow_Watchpoints : procedure( _UI : TUI_Interface ; CPU : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Ports : boolean ) ; stdcall = nil ;

var FTranslate_Serial_Data : function( Source : PChar ; var Size : longint ;
        Source_Speed, Target_Speed : int64 ; Data_Bits, Stop_Bits : longint ) : PChar ;
        stdcall = nil ;

var FGet_Character_Set : function() : TCEF_Character_Set ; stdcall = nil ;


var _Handle : THandle = 0 ;

function Handle : THandle ;

begin
    if( _Handle = 0 ) then
    begin
        _Handle := loadlibrary( 'CEF_Util.dll' ) ;
    end ;
    if( _Handle = 0 ) then
    begin
        _Handle := loadlibrary( PChar( Program_Path + 'CEF_Util.dll' ) ) ;
    end ;
    Result := _Handle ;
end ;


function Get_Watchpoint_Manager : TCEF_Watchpoint_Manager ;

begin
    if( not assigned( FGet_Watchpoint_Manager ) ) then
    begin
        FGet_Watchpoint_Manager := getprocaddress( Handle, 'Get_Watchpoint_Manager' ) ;
    end ;
    Result := FGet_Watchpoint_Manager ;
end ;


function Get_Master_Assembler( CPU : TComponent ;
    UI : TUI_Interface ) : TMaster_Assembler ;

begin
    if( not assigned( FGet_Master_Assembler ) ) then
    begin
        FGet_Master_Assembler := getprocaddress( Handle, 'Get_Master_Assembler' ) ;
    end ;
    Result := FGet_Master_Assembler( CPU, UI ) ;
end ;


procedure Add_Breakpoint( _UI : TUI_Interface ; Component : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Base, Size : longint ;
    Port : boolean ) ;

begin
    if( not assigned( FAdd_Breakpoint ) ) then
    begin
        FAdd_Breakpoint := getprocaddress( Handle, 'Add_Breakpoint' ) ;
    end ;
    FAdd_Breakpoint( _UI, Component, Watchpoints, Base, Size, Port ) ;
end ;


procedure Add_Watchpoint_Ex( _UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ; Context : pointer ;
    const Domain : string ) ;

begin
    if( not assigned( FAdd_Watchpoint_Ex ) ) then
    begin
        FAdd_Watchpoint_Ex := getprocaddress( Handle, 'Add_Watchpoint_Ex' ) ;
    end ;
    FAdd_Watchpoint_Ex( _UI, Component, Component_Type, Watchpoints, Base, Size,
        Low, High, Memory, Address, Access, Context, PChar( Domain ) ) ;
end ;


procedure Show_Watchpoints( _UI : TUI_Interface ; CPU : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Ports : boolean ) ;

begin
    if( not assigned( FShow_Watchpoints ) ) then
    begin
        FShow_Watchpoints := getprocaddress( Handle, 'Show_Watchpoints' ) ;
    end ;
    FShow_Watchpoints( _UI, CPU, Watchpoints, Ports ) ;
end ;


function Create_File_Stream( Name : string ; var UEC : TUnified_Exception ) : TCOM_Stream ;

begin
    if( not assigned( FCreate_File_Stream ) ) then
    begin
        FCreate_File_Stream := getprocaddress( Handle, 'Create_File_Stream' ) ;
    end ;
    Result := FCreate_File_Stream( PChar( Name ), UEC ) ;
end ;


function Translate_Serial_Data( Source : string ;
    Source_Speed, Target_Speed : int64 ; Data_Bits, Stop_Bits : longint ) : string ;

var P : PChar ;
    S : longint ;

begin
    if( not assigned( FTranslate_Serial_Data ) ) then
    begin
        FTranslate_Serial_Data := getprocaddress( Handle, 'Translate_Serial_Data' ) ;
    end ;
    S := length( Source ) ;
    P := FTranslate_Serial_Data( PChar( Source ), S, Source_Speed, Target_Speed,
        Data_Bits, Stop_Bits ) ;
    setlength( Result, S ) ;
    move( P[ 0 ], Pchar( Result )[ 0 ], S ) ;
end ;


function Get_Character_Set : TCEF_Character_Set ;

begin
    if( not assigned( FGet_Character_Set ) ) then
    begin
        FGet_Character_Set := getprocaddress( Handle, 'Get_Character_Set' ) ;
    end ;
    Result := FGet_Character_Set ;
end ;


end.
