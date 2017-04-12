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

unit i8253 ;

interface

uses // CEF...
     CEF ;

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

type Ti8253 = class
                  public // Constructors and destructors...
                      constructor Create( Clock : TMaster_Clock ) ;

                  private // Instance data...
                      _Counters : array[ 0..2 ] of TCounter ;
                      _Clock_Mode : TPIT_Clock_Mode ;
                      _Clock : TMaster_Clock ;

                  protected // Property handlers...
                      function Get_Clock_Mode( Index : integer ) : TPIT_Clock_Mode ;
                      function Get_Clock_Rate( Index : integer ) : int64 ;
                      function Get_Enabled( Index : integer ) : word ;
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
                      procedure Write( Port : integer ; Value : integer ) ;

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

procedure Ti8253.Write( Port : integer ; Value : integer ) ;

begin
    Port := Port and 3 ;
    ~~~
end ;


end.
