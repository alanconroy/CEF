{$N+}
{
        Program Name : Clock
        Package Name : CEF
        Purpose      : Generic system clock for CEF
        Institution  : 
        Date Written : 28-Apr-2000
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

	    This unit implements a generic system clock for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Clock ;

interface

uses // Borland...
     Classes, // TList

     // Other...
     CommonUt, // TInteger_List

     { CEF... }
     _CEF, // TUI_Interface
     CEF ; { TBase_Master_Clock }

type TTime_Que_Entry = record
                           Time : int64 ; // 0 = Unused entry
                           Component : TComponent ;
                       end ;

type TTime_Que = class
                     public // Constructors and destructors...
                         constructor Create ;

                         destructor Destroy ; override ;

                     private // Instance data...
                         Data : array of TTime_Que_Entry ; // Dynamic array of queue entries
                         Index : TInteger_List ; // Index of used entries in Data, sorted ascendig by time
                         Frees : TInteger_List ; // Index of unused entries in Data.

                     public // API...
                         procedure Add( _Time : int64 ; _Component : TComponent ) ;

                         procedure Clear ;

                         function Get : TTime_Que_Entry ;

                         function Empty : boolean ;

                         procedure Remove ;

                         function Peek : TTime_Que_Entry ;

                         procedure Test_Class ;
                 end ;

type TCEF_Generic_Clock = class( TBase_Master_Clock )
        private { Instance data... }
            _Time_Index : int64 ; // Current emulator clock time
            _UI : TUI_Interface ;
            _Que : TTime_Que ; // time/component queue
            _Mode : integer ; // Clock mode (MCM_*)
            _Last : int64 ; // System tick count for _Time_Index
            _Locked_Components : TList ; // List of Components that have called Block but have not been returned to yet

        protected // Internal utility routines...
            procedure _Unblock ;

	public { Public instance data... }
           _Serial_Number : integer ;

        public { API... }
           procedure Block( Component : TComponent ; Time_Delta : int64 ) ;
               override ;

           procedure Initialize( UI : TUI_Interface ) ; override ;

           function Get_Time_Index : int64 ; override ;

           function Serial_Number : integer ; override ;

           procedure Set_Mode( M : integer ) ; override ;

           function Get_Mode : integer ; override ;

           procedure Unblock ; override ;

           procedure Terminate ; override ;
     end ;

implementation

uses // Borland...
     Windows, // GetTickCount
     
     { C&C... }
     TypeDefs ; // Max_Memory

// TTime_Que methods...

// Constructors and destructors...

constructor TTime_Que.Create ;

begin
    inherited Create ;

    Index := TInteger_List.Create ;
    Frees := TInteger_List.Create ;
end ;


destructor TTime_Que.Destroy ;

begin
    Index.Free ;
    Index := nil ;
    Frees.Free ;
    Frees := nil ;

    inherited Destroy ;
end ;


procedure TTime_Que.Add( _Time : int64 ; _Component : TComponent ) ;

var Loop, This_Index, T : integer ;

begin
    // Make sure there is room
    if( Frees.Count = 0 ) then // Need more room
    begin
        setlength( Data, length( Data ) + 1 ) ;
        Frees.Add( length( Data ) - 1 ) ;
    end ;

    // Determine index of new record and remove it from the free list...
    // (Remove from end of list to prevent memory moves)
    T := Frees.Count - 1 ;
    This_Index := Frees[ T ] ;
    Frees.Count := T ;

    // Add record to Data...
    Data[ This_Index ].Time := _Time ;
    Data[ This_Index ].Component := _Component ;

    // Insert link to record in proper place...
    for Loop := Index.Count downto 1 do
    begin
        if( Data[ Index[ Loop - 1 ] ].Time <= _Time ) then
        begin
            Index.Insert( Loop, This_Index ) ;
            exit ;
        end ;
    end ;

    // If we get here, all existing records are later than this new one
    Index.Insert( 0, This_Index ) ;
end ;


procedure TTime_Que.Clear ;

var Loop : integer ;

begin
    Frees.Clear ;
    Index.Clear ;
    for Loop := 0 to length( Data ) - 1 do
    begin
        Frees.Add( Loop ) ;
    end ;
end ;


function TTime_Que.Get : TTime_Que_Entry ; // Get and remove earliest entry

begin
    if( Empty ) then
    begin
        fillchar( Result, sizeof( Result ), 0 ) ;
        exit ;
    end ;
    Result := Data[ Index[ 0 ] ] ;
    Remove ;
end ;


function TTime_Que.Empty : boolean ;

begin
    Result := ( Index.Count = 0 ) ;
end ;


procedure TTime_Que.Remove ; // Delete earliest entry

begin
    if( Empty ) then
    begin
        exit ;
    end ;
    Frees.Add( Index[ 0 ] ) ;
    Index.Delete( 0 ) ;
end ;


function TTime_Que.Peek : TTime_Que_Entry ; // Get earliest entry

begin
    if( Empty ) then
    begin
        fillchar( Result, sizeof( Result ), 0 ) ;
        exit ;
    end ;
    Result := Data[ Index[ 0 ] ] ;
end ;


procedure TTime_Que.Test_Class ;

var Class_0, Class_1, Class_2, Class_3 : TComponent ;
    Entry : TTime_Que_Entry ;

begin
    if( not Empty ) then
    begin
        writeln( 'Error' ) ;
    end ;
    Clear ;
    Class_0 := TBase_Component.Create ;
    Class_1 := TBase_Component.Create ;
    Class_2 := TBase_Component.Create ;
    Class_3 := TBase_Component.Create ;
    Add( 1, Class_1 ) ;
    if( Empty ) then
    begin
        writeln( 'Error' ) ;
    end ;
    Add( 3, Class_3 ) ;
    Add( 2, Class_2 ) ;
    Add( 0, Class_0 ) ;
    Get ;
    Add( 1, Class_1 ) ;
    Clear ;
    if( not Empty ) then
    begin
        writeln( 'Error' ) ;
    end ;
    Clear ;
    Remove ;
    Get ;
    Add( 1, Class_1 ) ;
    Add( 3, Class_3 ) ;
    Add( 2, Class_2 ) ;
    Add( 0, Class_0 ) ;
    Entry := Get ;
    if( ( Entry.Time <> 0 ) or ( Entry.Component <> Class_0 ) ) then
    begin
        writeln( 'Error' ) ;
    end ;
    Entry := Get ;
    if( ( Entry.Time <> 1 ) or ( Entry.Component <> Class_1 ) ) then
    begin
        writeln( 'Error' ) ;
    end ;
    Entry := Get ;
    if( ( Entry.Time <> 2 ) or ( Entry.Component <> Class_2 ) ) then
    begin
        writeln( 'Error' ) ;
    end ;
    Entry := Get ;
    if( ( Entry.Time <> 3 ) or ( Entry.Component <> Class_3 ) ) then
    begin
        writeln( 'Error' ) ;
    end ;
end ;



// TCEF_Generic_Clock methods...

// Internal utility routines...

procedure TCEF_Generic_Clock._Unblock ;

var Entry : TTime_Que_Entry ;
    Wait_Time : int64 ;

begin
    Entry := _Que.Get ; // Get next item on queue
    _Locked_Components.Add( Entry.Component ) ;
    try
        if( Mode = MCM_Synchronize ) then
        begin
            Wait_Time := Entry.Time - _Time_Index ; // How long to wait (in ns)
            Wait_Time := Wait_Time div 1000000 ; // How long to wait (in ms)
            Wait_Time := _Last + Wait_Time ;
            while( Wait_Time > Gettickcount ) do
            begin
                if( Gettickcount < _Last ) then // Time wrap-around occurred
                begin
                    Wait_Time := Wait_Time - 4294967296 ;
                end ;
                _UI.Idle( nil ) ;
            end ;
            _Last := Wait_Time ;
        end else
        begin
            _Time_Index := Entry.Time ;
            _UI.Idle( nil ) ;
        end ;
        try
            Entry.Component.Wake ;
        except
        end ;
    finally
        _Locked_Components.Remove( Entry.Component ) ;
    end ;
end ;


// API...

procedure TCEF_Generic_Clock.Block( Component : TComponent ;
    Time_Delta : int64 ) ;

var Entry : TTime_Que_Entry ;

begin
    if( _Locked_Components.IndexOf( Component ) <> -1 ) then
    begin
        { This indicates that a component is calling Block while being
        unblocked.  This is usually indicative of a recurring timer, or perhaps
        a pending operation.  To avoid infinite loops, we want to make sure we
        don't call another unblock from this Block (which is from unblock), so
        just add the item to the queue and exit. }
        _Que.Add( _Time_Index + Time_Delta, Component ) ;
        exit ;
    end ;
    if( Component = nil ) then // Process next item in queue
    begin
        if( not _Que.Empty ) then
        begin
            Entry := _Que.Peek ;
            if( _Locked_Components.IndexOf( Entry.Component ) = -1 ) then
            begin
                _UnBlock ;
            end ;
        end ;
        exit ;
    end ;
    if( Mode = MCM_Ignore ) then
    begin
        _Time_Index := _Time_Index + Time_Delta ;
        _Locked_Components.Add( Component ) ;
        try
            Component.Wake ;
        finally
            _Locked_Components.Remove( Component ) ;
        end ;
        _UI.Idle( nil ) ;
    end else
    begin
        _Que.Add( _Time_Index + Time_Delta, Component ) ;
        _Unblock ;
    end ;
end ;


procedure TCEF_Generic_Clock.Initialize( UI : TUI_Interface ) ;

begin
    _Time_Index := 0 ;
    _UI := UI ;
    _Que := TTime_Que.Create ;
    _Locked_Components := TList.Create ;
end ;


function TCEF_Generic_Clock.Get_Time_Index : int64 ;

begin
    Get_Time_Index := _Time_Index ;
end ;


function TCEF_Generic_Clock.Serial_Number : integer ;

begin
    Result := _Serial_Number ;
end ;


procedure TCEF_Generic_Clock.Set_Mode( M : integer ) ;

begin
    _Mode := M ;
    _Last := GetTickCount ;
end ;


function TCEF_Generic_Clock.Get_Mode : integer ;

begin
    Result := _Mode ;
end ;


procedure TCEF_Generic_Clock.Unblock ;

var Saved : integer ;

begin
    Saved := Mode ;
    Mode := MCM_Default ;
    while( not _Que.Empty ) do
    begin
        _Unblock ;
    end ;
    Mode := Saved ;
end ;


procedure TCEF_Generic_Clock.Terminate ;

begin
    _Que.Free ;
    _Locked_Components.Free ;
    _Que := nil ;
    _Locked_Components := nil ;
    Free ;
end ;



end.

