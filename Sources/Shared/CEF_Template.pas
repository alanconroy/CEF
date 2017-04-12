{$N+}
{
        Program Name : CEF
        Package Name : CEF
        Purpose      : CEF Template component
        Institution  :
        Date Written : 27-Aug-2015
        Written By   : Alan Conroy
        Version      : 2.6

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

          This unit defines the CEF_Template class, which provides all of the
        basic support logic for a TComponent.  New components can descend from
        this instead of TComponent so that all the framework logic is consistent
        between components and also to save work doing the same code over and
        over.
}

unit CEF_Template ;

interface

uses // Borland...
     Classes, // TStringList

     // C&C...
     _UEHDefs, // TUEC
     _Streams, // TCOM_Stream

     // CEF
     CEF, // TComponent
     _CEFUtil, // Get_Watchpoint_Manager
     CEFUtil_Int ; // Add_Watchpoint_Ex

type TSignal = class
                   public // API...
                       Signal_Out : boolean ; // True if output signal.  False if input signal
                       Active_Low : boolean ; // True if signal is active low
                       Active : boolean ; // True if signal is active
               end ;

type TStandard_Component = class( TComponent )
                               protected // Instance data...
                                   _Facility_Code : longint ;
                                   _Version : longint ;
                                   _UI : TUI_Interface ;
                                   _Serial_Number : integer ;
                                   _Read_Latency : longint ;
                                   _Write_Latency : longint ;
                                   _Parent : TComponent ;
                                   _Tag : longint ;
                                   CEF_Logger : TCEF_Logger ;
                                   _Signals : TStringList ;
                                   _Component_Type : longint ;
                                   _Trace : boolean ;
                                   _Inputs : TList ;
                                   _Outputs : TList ;
                                   _Notices : TList ;
                                   _Name : string ;
                                   _Memory_Watchpoints : TCEF_Watchpoint_Manager ;
                                   _Watchpoints : TCEF_Watchpoint_Manager ;
                                   Temp : string ;

                               protected // Utility routines...
                                   function Signals : TStringList ;

                               public // Overrides...
                                   function Facility_Code : longint ; override ; stdcall ;

                                   function Initialize( UI : TUI_Interface ) : TUEC ;
                                       override ; stdcall ;

                                   function Terminate : TUEC ; override ; stdcall ;

                                   function Serial_Number : integer ; override ; stdcall ;

                                   function Add_Notification( Component : TComponent ) : TUEC ;
                                       override ; stdcall ;

                                   function Clear_Watchpoint( Address : int64 ; Memory : boolean ;
                                       Access : longint ) : TUEC ; override ; stdcall ;

                                   function Component_Type : longint ; override ; stdcall ;

                                   function Connect_Input( Component : TComponent ) : TUEC ;
                                       override ; stdcall ;

                                   function Connect_Output( Component : TComponent ) : TUEC ;
                                       override ; stdcall ;

                                   function Delete_Notification( Component : TComponent ) : TUEC ;
                                       override ; stdcall ;

                                   function Disconnect_Input( Component : TComponent ) : TUEC ;
                                       override ; stdcall ;

                                   function Disconnect_Output( Component : TComponent ) : TUEC ;
                                       override ; stdcall ;

                                   function Get_Parent : TComponent ; override ; stdcall ;

                                   function Get_Profiling : boolean ; override ; stdcall ;

                                   function Get_Read_Latency : longint ; override ; stdcall ;

                                   function Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
                                       override ; stdcall ;

                                   function Get_Tag : longint ; override ; stdcall ;

                                   function Get_Trace : boolean ; override ; stdcall ;

                                   function Get_Write_Latency : longint ; override ; stdcall ;

                                   function Input_Component( Index : longint ) : TComponent ;
                                       override ; stdcall ;

                                   function Name : PChar ; override ; stdcall ;

                                   function Output_Component( Index : longint ) : TComponent ;
                                       override ; stdcall ;

                                   function Profiler : TProfiler ; override ; stdcall ;

                                   procedure Reset ; override ; stdcall ;

                                   function Restore_State( Stream : TCOM_Stream ) : TUEC ;
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
                                       override ; stdcall;

                                   function Version : longint ; override ; stdcall ;

                                   function Get_Logger : TCEF_Logger ; override ; stdcall ;

                                   procedure Set_Logger( Value : TCEF_Logger ) ; override ; stdcall ;

                               public // API...
                                   procedure Set_Facility_Code( Value : longint ) ;
                                   procedure Set_Version( Value : longint ) ;
                                   procedure Set_Component_Type( Value : longint ) ;
                                   procedure Set_Name( const Value : string ) ;
                           end ; // TStandard_Component


implementation

uses // Borland...
     Sysutils, // trim

     // C&C...
     Parse ; // TString_Parser

// TStandard_Component...

// Utility routines...

function TStandard_Component.Signals : TStringList ;

begin
    if( _Signals = nil ) then
    begin
        _Signals := TStringList.Create ;
    end ;
    Result := _Signals ;
end ;


// Overrides...

function TStandard_Component.Facility_Code : longint ; stdcall ;

begin
    Result := _Facility_Code ;
end ;


function TStandard_Component.Initialize( UI : TUI_Interface ) : TUEC ; stdcall ;

begin
    _UI := UI ;
end ;


function TStandard_Component.Terminate : TUEC ; stdcall ;

begin
    inherited Terminate ;

    if( _Signals <> nil ) then
    begin
        _Signals.Free ;
        _Signals := nil ;
    end ;
    if( _Inputs <> nil ) then
    begin
        _Inputs.Free ;
        _Inputs := nil ;
    end ;
    if( _Outputs <> nil ) then
    begin
        _Outputs.Free ;
        _Outputs := nil ;
    end ;
    if( _Notices <> nil ) then
    begin
        _Notices.Free ;
        _Notices := nil ;
    end ;
end ;


function TStandard_Component.Serial_Number : integer ; stdcall ;

begin
    Result := _Serial_Number ;
end ;


function TStandard_Component.Add_Notification( Component : TComponent ) : TUEC ;
    stdcall ;

begin
    if( _Notices = nil ) then
    begin
        _Notices := TList.Create ;
    end ;
    _Notices.Add( Component ) ;
end ;


function TStandard_Component.Clear_Watchpoint( Address : int64 ; Memory : boolean ;
   Access : longint ) : TUEC ; stdcall ;

begin
    if( Memory ) then
    begin
        if( _Memory_Watchpoints <> nil ) then
        begin
            _Watchpoints.Clear_Watchpoint( Address, Access ) ;
        end ;
    end else
    begin
        if( _Watchpoints <> nil ) then
        begin
            _Watchpoints.Clear_Watchpoint( Address, Access ) ;
        end ;
    end ;
end ;


function TStandard_Component.Component_Type : longint ; stdcall ;

begin
    Result := _Component_Type ;
end ;


function TStandard_Component.Connect_Input( Component : TComponent ) : TUEC ;
    stdcall ;

begin
    if( _Inputs = nil ) then
    begin
        _Inputs := TList.Create ;
    end ;
    _Inputs.Add( Component ) ;
end ;


function TStandard_Component.Connect_Output( Component : TComponent ) : TUEC ;
    stdcall ;

begin
    if( _Outputs = nil ) then
    begin
        _Outputs := TList.Create ;
    end ;
    _Outputs.Add( Component ) ;
end ;


function TStandard_Component.Delete_Notification( Component : TComponent ) : TUEC ;
    stdcall ;

begin
    if( _Notices <> nil ) then
    begin
        _Notices.Remove( Component ) ;
    end ;
end ;


function TStandard_Component.Disconnect_Input( Component : TComponent ) : TUEC ;
    stdcall ;

begin
    if( _Inputs <> nil ) then
    begin
        _Inputs.Remove( Component ) ;
    end ;
end ;


function TStandard_Component.Disconnect_Output( Component : TComponent ) : TUEC ;
    stdcall ;

begin
    if( _Outputs <> nil ) then
    begin
        _Outputs.Remove( Component ) ;
    end ;
end ;


function TStandard_Component.Get_Parent : TComponent ; stdcall ;

begin
    Result := _Parent ;
end ;


function TStandard_Component.Get_Profiling : boolean ; stdcall ;

begin
    Result := False ;
end ;


function TStandard_Component.Get_Read_Latency : longint ; stdcall ;

begin
    Result := _Read_Latency ;
end ;


function TStandard_Component.Get_Signal( Name : PChar ; var State : boolean ) : boolean ;
    stdcall ;

var Index : integer ;

begin
    Index := Signal_Index( Name ) ;
    Result := False ;
    if( ( Index >= 0 ) and ( Index < Signal_Count ) ) then
    begin
        State := TSignal( _Signals[ Index ] ).Active ;
        Result := True ;
    end ;
end ;


function TStandard_Component.Get_Tag : longint ; stdcall ;

begin
    Result := _Tag ;
end ;


function TStandard_Component.Get_Trace : boolean ; stdcall ;

begin
    Result := _Trace ;
end ;


function TStandard_Component.Get_Write_Latency : longint ; stdcall ;

begin
    Result := _Write_Latency ;
end ;


function TStandard_Component.Input_Component( Index : longint ) : TComponent ;
    stdcall ;

begin
    Result := nil ;
    if( _Inputs <> nil ) then
    begin
        if( ( Index >= 0 ) and ( Index < _Inputs.Count ) ) then
        begin
            Result := _Inputs[ Index ] ;
        end ;
    end ;
end ;


function TStandard_Component.Name : PChar ; stdcall ;

begin
    Result := PChar( _Name ) ;
end ;


function TStandard_Component.Output_Component( Index : longint ) : TComponent ;
    stdcall ;

begin
    Result := nil ;
    if( _Outputs <> nil ) then
    begin
        if( ( Index >= 0 ) and ( Index < _Outputs.Count ) ) then
        begin
            Result := _Outputs[ Index ] ;
        end ;
    end ;
end ;


function TStandard_Component.Profiler : TProfiler ; stdcall ;

begin
    // TODO: IMPLEMENT
end ;


procedure TStandard_Component.Reset ; stdcall ;

begin
    _Read_Latency := 0 ;
    _Write_Latency := 0 ;
end ;


function TStandard_Component.Restore_State( Stream : TCOM_Stream ) : TUEC ;
    stdcall ;

begin
    // TODO: IMPLEMENT
end ;


function TStandard_Component.Save_State( Stream : TCOM_Stream ) : TUEC ;
    stdcall ;

begin
    // TODO: IMPLEMENT
end ;


procedure TStandard_Component.Set_Parent( Component : TComponent ) ; stdcall ;

begin
    _Parent := Component ;
end ;


procedure TStandard_Component.Set_Read_Latency( Value : longint ) ; stdcall ;

begin
    _Read_Latency := Value ;
end ;


procedure TStandard_Component.Set_Signal( Name : PChar ; State : boolean ) ;
    stdcall ;

var Index : integer ;

begin
    Index := Signal_Index( Name ) ;
    if( ( Index >= 0 ) and ( Index < Signal_Count ) ) then
    begin
        TSignal( _Signals[ Index ] ).Active := State ;
    end ;
end ;


procedure TStandard_Component.Set_Tag( Value : longint ) ; stdcall ;

begin
    _Tag := Value ;
end ;


procedure TStandard_Component.Set_Trace( Value : boolean ) ; stdcall ;

begin
    _Trace := Value ;
end ;


procedure TStandard_Component.Set_Up( P : PChar ) ; stdcall ;

var Parser : TString_Parser ;
    S : string ;

begin
    Parser := TString_Parser.Create ;
    try
        Parser.Set_Source( string( P ) ) ;
        S := uppercase( Parser.Token ) ;
        while( S <> '' ) do
        begin
            // TODO: IMPLEMENT
            S := uppercase( Parser.Token ) ;
        end ; // while( S <> '' )
    finally
        Parser.Free ;
    end ;
end ;


function TStandard_Component.Set_Watchpoint( Address : int64 ; Memory : boolean ;
    Access : longint ) : TUEC ; stdcall ;

begin
    if( Memory ) then
    begin
        if( _Memory_Watchpoints = nil ) then
        begin
            _Memory_Watchpoints := Get_Watchpoint_Manager ;
        end ;
        _Memory_Watchpoints.Set_Watchpoint( Address, Access ) ;
    end else
    begin
        if( _Watchpoints = nil ) then
        begin
            _Watchpoints := Get_Watchpoint_Manager ;
        end ;
        _Watchpoints.Set_Watchpoint( Address, Access ) ;
    end ;
end ;


procedure TStandard_Component.Set_Write_Latency( Value : longint ) ;   stdcall ;

begin
    _Write_Latency := Value ;
end ;


function TStandard_Component.Signal_Count : longint ; stdcall ;

begin
    Result := Signals.Count ;
end ;


function TStandard_Component.Signal_Name( Index : longint ) : PChar ; stdcall ;

begin
    Temp := '' ;
    if( ( Index >= 0 ) and ( Index < Signal_Count ) ) then
    begin
        Temp := Signal_Count[ Index ] ;
    end ;
    Result := PChar( Temp ) ;
end ;


function TStandard_Component.Signal_Out( Index : longint ) : boolean ; stdcall ;

begin
    Result := False ;
    if( ( Index >= 0 ) and ( Index < Signal_Count ) ) then
    begin
        Result := TSignal( _Signals[ Index ] ).Signal_Out ;
    end ;
end ;


function TStandard_Component.Signal_Active_Low( Index : longint ) : boolean ;
    stdcall ;

begin
    Result := False ;
    if( ( Index >= 0 ) and ( Index < Signal_Count ) ) then
    begin
        Result := TSignal( _Signals[ Index ] ).Active_Low ;
    end ;
end ;


function TStandard_Component.Signal_Index( Name : PChar ) : integer ; stdcall ;

begin
    Result := Signals.IndexOf( trim( Name ) ) ;
end ;


function TStandard_Component.User_Interface : TUser_Interface ;
    stdcall ;

begin
    Result := _UI ;
end ;


function TStandard_Component.Version : longint ; stdcall ;

begin
    Result := _Version ;
end ;


function TStandard_Component.Get_Logger : TCEF_Logger ; stdcall ;

begin
    Result := CEF_Logger ;
end ;


procedure TStandard_Component.Set_Logger( Value : TCEF_Logger ) ; stdcall ;

begin
    if( Value = CEF_Logger ) then
    begin
        exit ; // No change
    end ;

    // Get rid of old logger...
    if( CEF_Logger <> nil ) then
    begin
        CEF_Logger.Detach ;
    end ;

    // Set current logger...
    CEF_Logger := Value ;
    if( Value <> nil ) then
    begin
        CEF_Logger.Attach ;
    end ;
end ;


// API...

procedure TStandard_Component.Set_Facility_Code( Value : longint ) ;

begin
    _Facility_Code := Value ;
end ;


procedure TStandard_Component.Set_Version( Value : longint ) ;

begin
    _Version := Value ;
end ;


procedure TStandard_Component.Set_Component_Type( Value : longint ) ;

begin
    _Component_Type := Value ;
end ;


procedure TStandard_Component.Set_Name( const Value : string ) ;

begin
    _Name := Value ;
end ;


end.
