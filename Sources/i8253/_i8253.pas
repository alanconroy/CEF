{
        Program Name : i8253
        Package Name : CEF32
        Purpose      : Intel 8253 Programmable Interrupt Timer
        Institution  : Conroy & Conroy Co.
        Date Written : 8-Aug-2015
        Written By   : Alan Conroy
        Version      : 1.0

	    Copyright (C) 2015 by Alan Conroy.  All rights reserved.

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

          Emulates an Intel 8253 Programmable Interrupt Timer.
}

unit _i8253 ;

interface

uses // C&C...
     _UEHDefs,
     
     // CEF...
     CEF,
     CEF_Template ; // TStandard_Component

type TCounter_Mode = ( CM_Interrupt,
                       CM_One_Shot,
                       CM_Rate_Generator,
                       CM_Square_Wave,
                       CM_Software_Triggered_Strobe,
                       CM_Hardware_Triggered_Strobe
                     ) ;

type TPIT_Clock_Mode = ( PITCM_Manual, // Clock must be manually toggled
                         PITCM_Automatic // Clock has specified clock rate
                       ) ;

type TCounter_Fire_Notice = procedure( Counter : integer ) ;

type TCounter = record
                     Enabled : boolean ;
                     Mode : TCounter_Mode ;
                     Value : word ;
                     Clock_Mode : TPIT_Clock_Mode ;
                     Clock_Rate : int64 ;
                     Notice : TCounter_Fire_Notice ; // Routine to call when counter fires
                end ;

type Ti8253 = class( TStandard_Component )
                  public // Constructors and destructors...
                      constructor Create( Clock : TMaster_Clock ) ;

                  private // Instance data...
                      _Counters : array[ 0..2 ] of TCounter ;
                      _Clock_Mode : TPIT_Clock_Mode ;
                      _Clock : TMaster_Clock ;

                  protected // Property handlers...
                      function Get_Clock_Mode( Index : integer ) : TPIT_Clock_Mode ;
                      function Get_Clock_Rate( Index : integer ) : int64 ;
                      function Get_Enabled( Index : integer ) : boolean ;
                      function Get_Mode( Index : integer ) : TCounter_Mode ;
                      function Get_Notice( Index : integer ) : TCounter_Fire_Notice ;
                      function Get_Value( Index : integer ) : word ;
                      procedure Set_Clock_Mode( Index : integer ; Value : TPIT_Clock_Mode ) ;
                      procedure Set_Clock_Rate( Index : integer ; Value : int64 ) ;
                      procedure Set_Enabled( Index : integer ; Value : boolean ) ;
                      procedure Set_Mode( Index : integer ; Value : TCounter_Mode ) ;
                      procedure Set_Notice( Index : integer ; Value : TCounter_Fire_Notice ) ;
                      procedure Set_Value( Index, Value : integer ) ;

                  public // API...
                      function Facility_Code : longint ; override ; stdcall ;

                      function Initialize( UI : TUI_Interface ) : TUEC ;
                          override ; stdcall ;

                      function Terminate : TUEC ; override ; stdcall ;

                      function Serial_Number : integer ; override ; stdcall ;

                      function Add_Notification( Component : TComponent ) : TUEC ;
                          override ; stdcall ;

                      function Child_Component( Index : longint ) : TComponent ;
                          override ; stdcall ;

                      procedure Child_Notification( Child : TComponent ;
                          var Notice : longint ; var Params : int64 ) ; override ; stdcall ;

                      function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                          Access : longint ) : TUEC ; override ; stdcall ;

                      function Component_Type : longint ; override ; stdcall ;

                      function Connect_Input( Component : TComponent ) : TUEC ;
                          override ; stdcall ;

                      function Connect_Output( Component : TComponent ) : TUEC ;
                          override ; stdcall ;

                      function Delete_Notification( Component : TComponent ) : TUEC ;
                          override ; stdcall ;

                      function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
                          Memory : boolean ) : TUEC ; override ; stdcall ;

                      function Disconnect_Input( Component : TComponent ) : TUEC ;
                          override ; stdcall ;

                      function Disconnect_Output( Component : TComponent ) : TUEC ;
                          override ; stdcall ;

                      function Examine( Address : int64 ; var Size : longint ;
                          Buffer : pointer ; Memory : boolean ) : TUEC ; override ; stdcall ;

                      function Get_Access_Mode( Address : int64 ;
                          Memory : boolean ) : longint ; override ; stdcall ;

                      function Get_Exception_Description( Index : longint ) : PChar ;
                          override ; stdcall ;

                      function Get_Parent : TComponent ; override ; stdcall ;

                      function Get_Read_Latency : longint ; override ; stdcall ;

                      function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                          override ; stdcall ;

                      function Get_State_Name( Index : longint ) : PChar ;
                          override ; stdcall ;

                      function Get_Tag : longint ; override ; stdcall ;

                      function Get_Trace : boolean ; override ; stdcall ;

                      function Get_Write_Latency : longint ; override ; stdcall ;

                      function Input_Component( Index : longint ) : TComponent ;
                          override ; stdcall ;

                      function Keyboard : TKeyboard ; override ; stdcall ;

                      function Memory : TMemory ; override ; stdcall ;

                      function Name : PChar ; override ; stdcall ;

                      function Output_Component( Index : longint ) : TComponent ;
                          override ; stdcall ;

                      function Read( Address : int64 ; Size : longint ;
                          IO_Type : longint ) : boolean ; override ; stdcall ;

                      procedure Reset ; override ; stdcall ;

                      function Restore_Contents( Stream : TCOM_Stream ) : TUEC ;
                          override ; stdcall ;

                      function Restore_State( Stream : TCOM_Stream ) : TUEC ;
                          override ; stdcall ;

                      function Save_Contents( Stream : TCOM_Stream ) : TUEC ;
                          override ; stdcall ;

                      function Save_State( Stream : TCOM_Stream ) : TUEC ;
                          override ; stdcall ;

                      procedure Set_Parent( Component : TComponent ) ; override ; stdcall ;

                      procedure Set_Read_Latency( Value : longint ) ; override ; stdcall ;

                      procedure Set_Signal( Name : PChar ; State : boolean ) ;
                          override ; stdcall ;

                      procedure Set_Tag( Value : longint ) ; override ; stdcall ;

                      procedure Set_Trace( Value : boolean ) ; override ; stdcall ;

                      procedure Set_Up( P : PChar ) ; override ; stdcall ;

                      function Set_Watchpoint( Address : int64 ; Memory : boolean ;
                          Access : longint ) : TUEC ; override ; stdcall ;

                      procedure Set_Write_Latency( Value : longint ) ;  override ; stdcall ;

                      function Signal_Count : longint ; override ; stdcall ;

                      function Signal_Name( Index : longint ) : PChar ; override ; stdcall ;

                      function Signal_Out( Index : longint ) : boolean ; override ; stdcall ;

                      function Signal_Active_Low( Index : longint ) : boolean ;
                          override ; stdcall ;

                      function Signal_Index( Name : PChar ) : integer ; override ; stdcall ;

                      function User_Interface : TUser_Interface ;
                          override ; stdcall ; stdcall ;

                      procedure Wake ; override ; stdcall ;

                      function Write( Address : int64 ; Value, Size : longint ;
                          IO_Type : longint ) : TUEC ; override ; stdcall ;

                      procedure UI_Notice( Code : longint ; var Data : int64 ) ;
                          override ; stdcall ;

                      function Respond_To_Address( Address : int64 ; Typ : integer ;
                          Examine : boolean ) : boolean ; override ; stdcall ;

                      function Get_Logger : TCEF_Logger ; override ; stdcall ;

                      procedure Set_Logger( Value : TCEF_Logger ) ; override ; stdcall ;

                  public // Properties...
                      property Clock_Mode[ Index : integer ] : TPIT_Clock_Mode
                          read Get_Clock_Mode
                          write Set_Clock_Mode ;
                      property Clock_Rate[ Index : integer ] : int64
                          read Get_Clock_Rate
                          write Set_Clock_Rate ;
                      property Enabled[ Index : integer ] : boolean
                          read Get_Enabled
                          write Set_Enabled ;
                      property Mode[ Index : integer ] : TCounter_Mode
                          read Get_Mode
                          write Set_Mode ;
                      property Notice[ Index : integer ] : TCounter_Fire_Notice
                          read Get_Notice
                          write Set_Notice ;
                      property Value[ Index : integer ] : word
                          read Get_Value
                          write Set_Value ;
              end ;

const PIT_OCW_MASK_BINCOUNT	= 1 ;
const PIT_OCW_MASK_MODE	= $E ;
const PIT_OCW_MASK_RL =	$30 ;
const PIT_OCW_MASK_COUNTER = $C0 ;

const PIT_OCW_BINCOUNT_BINARY =	0 ;
const PIT_OCW_BINCOUNT_BCD = 1 ;
const PIT_OCW_MODE_TERMINALCOUNT = 0 ;
const PIT_OCW_MODE_ONESHOT = 2 ;
const PIT_OCW_MODE_RATEGEN = 4 ;
const PIT_OCW_MODE_SQUAREWAVEGEN = 6 ;
const PIT_OCW_MODE_SOFTWARETRIG	= 8	;
const PIT_OCW_MODE_HARDWARETRIG	= $A ;
const PIT_OCW_RL_LATCH = 0 ;
const PIT_OCW_RL_LSBONLY = $10 ;
const PIT_OCW_RL_MSBONLY = $20 ;
const PIT_OCW_RL_DATA =	$30	;
const PIT_OCW_COUNTER_0	= 0	;
const PIT_OCW_COUNTER_1 = $40 ;
const PIT_OCW_COUNTER_2	= $80 ;

(*
const PIC_OCW2_L1 = 1 ; // Level 1 interrupt level
const PIC_OCW2_L2 = 2 ; // Level 2 interrupt level
const PIC_OCW2_L3 = 4 ; // Level 3 interrupt level
const PIC_OCW2_EOI	= $20 ;	// End of Interrupt command
const PIC_OCW2_SL = $40 ; // Select command
const PIC_OCW2_ROTATE = $80 ; // Rotation command

const PIC_OCW3_RIS	= 1	;
const PIC_OCW3_RIR	= 2	;
const PIC_OCW3_MODE = 4 ;
const PIC_OCW3_SMM	= $20 ;
const PIC_OCW3_ESMM = $40 ;
const PIC_OCW3_D7 = $80 ;

const PIC_ICW1_IC4 = 1 ; // Expect ICW 4 bit
const PIC_ICW1_SNGL	= 2	; // Single or Cascaded
const PIC_ICW1_ADI = 4 ; // Call Address Interval
const PIC_ICW1_LTIM	= $8 ; // Operation Mode
const PIC_ICW1_INIT	= $10 ;	// Initialization Command

const PIC_ICW1_IC4_EXPECT =	1 ;	// Use when setting I86_PIC_ICW1_MASK_IC4
const PIC_ICW1_IC4_NO =	0 ;
const PIC_ICW1_SNGL_YES	= 2	; // Use when setting I86_PIC_ICW1_MASK_SNGL
const PIC_ICW1_SNGL_NO = 0 ;
const PIC_ICW1_ADI_CALLINTERVAL4 = 4 ; // Use when setting I86_PIC_ICW1_MASK_ADI
const PIC_ICW1_ADI_CALLINTERVAL8 = 0 ;
const PIC_ICW1_LTIM_LEVELTRIGGERED = 8 ; // Use when setting I86_PIC_ICW1_MASK_LTIM
const PIC_ICW1_LTIM_EDGETRIGGERED =	0 ;
const PIC_ICW1_INIT_YES	= $10 ;	// Use when setting I86_PIC_ICW1_MASK_INIT
const PIC_ICW1_INIT_NO = 0 ;
*)


implementation

// Constructors and destructors...

constructor Ti8253.Create( Clock : TMaster_Clock ) ;

begin
    inherited Create ;

    _Clock := Clock ;
end ;


// Property handlers...

function Ti8253.Get_Clock_Mode( Index : integer ) : TPIT_Clock_Mode ;

begin
    if( ( Index < 0 ) or ( Index > 2 ) ) then
    begin
        Result := PITCM_Manual ;
    end else
    begin
        Result := _Counters[ Index ].Clock_Mode ;
    end ;
end ;


function Ti8253.Get_Clock_Rate( Index : integer ) : int64 ;

begin
    if( ( Index < 0 ) or ( Index > 2 ) ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := _Counters[ Index ].Clock_Rate ;
    end ;
end ;


function Ti8253.Get_Enabled( Index : integer ) : word ;

begin
    if( ( Index < 0 ) or ( Index > 2 ) ) then
    begin
        Result := False ;
    end else
    begin
        Result := _Counters[ Index ].Enabled ;
    end ;
end ;


function Ti8253.Get_Mode( Index : integer ) : TCounter_Mode ;

begin
    if( ( Index < 0 ) or ( Index > 2 ) ) then
    begin
        Result := CM_Interrupt ;
    end else
    begin
        Result := _Counters[ Index ].Mode ;
    end ;
end ;


function Ti8253.Get_Notice( Index : integer ) : TCounter_Fire_Notice ;

begin
    if( ( Index < 0 ) or ( Index > 2 ) ) then
    begin
        Result := nil ;
    end else
    begin
        Result := _Counters[ Index ].Notice ;
    end ;
end ;


function Ti8253.Get_Value( Index : integer ) : word ;

begin
    if( ( Index < 0 ) or ( Index > 2 ) ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := _Counters[ Index ].Value ;
    end ;
end ;


procedure Ti8253.Set_Clock_Mode( Index : integer ; Value : TPIT_Clock_Mode ) ;

begin
    if( ( Index >= 0 ) and ( Index <= 2 ) ) then
    begin
        _Counters[ Index ].Clock_Mode := Value ;
    end ;
end ;


procedure Ti8253.Set_Clock_Rate( Index : integer ; Value : int64 ) ;

begin
    if( ( Index >= 0 ) and ( Index <= 2 ) ) then
    begin
        _Counters[ Index ].Clock_Rate := Value ;
    end ;
end ;


procedure Ti8253.Set_Enabled( Index : integer ; Value : boolean ) ;

begin
    if( ( Index >= 0 ) and ( Index <= 2 ) ) then
    begin
        _Counters[ Index ].Enabled := Value ;
    end ;
end ;


procedure Ti8253.Set_Mode( Index : integer ; Value : TCounter_Mode ) ;

begin
    if( ( Index >= 0 ) and ( Index <= 2 ) ) then
    begin
        _Counters[ Index ].Mode := Value ;
    end ;
end ;


procedure Ti8253.Set_Notice( Index : integer ; Value : TCounter_Fire_Notice ) ;

begin
    if( ( Index >= 0 ) and ( Index <= 2 ) ) then
    begin
        _Counters[ Index ].Notice := Value ;
    end ;
end ;


procedure Ti8253.Set_Value( Index, Value : integer ) ;

begin
    if( ( Index >= 0 ) and ( Index <= 2 ) ) then
    begin
        _Counters[ Index ].Value := Value ;
    end ;
end ;


// API...

function Facility_Code : longint ;

begin

end ;


function Initialize( UI : TUI_Interface ) : TUEC ;

begin

end ;


function Terminate : TUEC ;

begin

end ;


function Serial_Number : integer ;

begin

end ;


function Add_Notification( Component : TComponent ) : TUEC ;

begin

end ;


function Child_Component( Index : longint ) : TComponent ;

begin

end ;


procedure Child_Notification( Child : TComponent ;
  var Notice : longint ; var Params : int64 ) ;

begin

end ;


function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
  Access : longint ) : TUEC ;

begin

end ;


function Component_Type : longint ;

begin

end ;


function Connect_Input( Component : TComponent ) : TUEC ;

begin

end ;


function Connect_Output( Component : TComponent ) : TUEC ;

begin

end ;


function Delete_Notification( Component : TComponent ) : TUEC ;

begin

end ;


function Deposit( Address : int64 ; Size : longint ; Buffer : pointer ;
  Memory : boolean ) : TUEC ;

begin

end ;


function Disconnect_Input( Component : TComponent ) : TUEC ;

begin

end ;


function Disconnect_Output( Component : TComponent ) : TUEC ;

begin

end ;


function Examine( Address : int64 ; var Size : longint ;
  Buffer : pointer ; Memory : boolean ) : TUEC ;

begin
end ;


function Get_Access_Mode( Address : int64 ;
  Memory : boolean ) : longint ;

begin

end ;


function Get_Exception_Description( Index : longint ) : PChar ;

begin

end ;


function Get_Parent : TComponent ;

begin

end ;


function Get_Read_Latency : longint ; override ; stdcall ;

function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
  override ; stdcall ;

function Get_State_Name( Index : longint ) : PChar ;
  override ; stdcall ;

function Get_Tag : longint ; override ; stdcall ;

function Get_Trace : boolean ; override ; stdcall ;

function Get_Write_Latency : longint ; override ; stdcall ;

function Input_Component( Index : longint ) : TComponent ;
  override ; stdcall ;

function Keyboard : TKeyboard ; override ; stdcall ;

function Memory : TMemory ; override ; stdcall ;

function Name : PChar ; override ; stdcall ;

function Output_Component( Index : longint ) : TComponent ;
  override ; stdcall ;

function Read( Address : int64 ; Size : longint ;
  IO_Type : longint ) : boolean ; override ; stdcall ;

procedure Reset ; override ; stdcall ;

function Restore_Contents( Stream : TCOM_Stream ) : TUEC ;
  override ; stdcall ;

function Restore_State( Stream : TCOM_Stream ) : TUEC ;
  override ; stdcall ;

function Save_Contents( Stream : TCOM_Stream ) : TUEC ;
  override ; stdcall ;

function Save_State( Stream : TCOM_Stream ) : TUEC ;
  override ; stdcall ;

procedure Set_Parent( Component : TComponent ) ; override ; stdcall ;

procedure Set_Read_Latency( Value : longint ) ; override ; stdcall ;

procedure Set_Signal( Name : PChar ; State : boolean ) ;
  override ; stdcall ;

procedure Set_Tag( Value : longint ) ; override ; stdcall ;

procedure Set_Trace( Value : boolean ) ; override ; stdcall ;

procedure Set_Up( P : PChar ) ; override ; stdcall ;

function Set_Watchpoint( Address : int64 ; Memory : boolean ;
  Access : longint ) : TUEC ; override ; stdcall ;

procedure Set_Write_Latency( Value : longint ) ;  override ; stdcall ;

function Signal_Count : longint ; override ; stdcall ;

function Signal_Name( Index : longint ) : PChar ; override ; stdcall ;

function Signal_Out( Index : longint ) : boolean ; override ; stdcall ;

function Signal_Active_Low( Index : longint ) : boolean ;
  override ; stdcall ;

function Signal_Index( Name : PChar ) : integer ; override ; stdcall ;

function User_Interface : TUser_Interface ;
  override ; stdcall ; stdcall ;

procedure Wake ; override ; stdcall ;

function Write( Address : int64 ; Value, Size : longint ;
  IO_Type : longint ) : TUEC ; override ; stdcall ;

procedure UI_Notice( Code : longint ; var Data : int64 ) ;
  override ; stdcall ;

function Respond_To_Address( Address : int64 ; Typ : integer ;
  Examine : boolean ) : boolean ; override ; stdcall ;

function Get_Logger : TCEF_Logger ; override ; stdcall ;

procedure Set_Logger( Value : TCEF_Logger ) ; override ; stdcall ;

procedure Ti8253.Write( Port : integer ; Value : integer ) ;

begin
    Port := Port and 3 ;
    ~~~
end ;


end.
