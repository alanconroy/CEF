{
        Program Name    : Mem_Watchpoints_Dialog
        Package Name    : CEF
        Purpose         : Memory watchpoints edit dialog
        Institution     : Conroy & Conroy Co.
        Date written    : 10-Apr-2005
        Written by      : Alan Conroy
        Version         : 1.0

        Copyright (C) 2005 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

      *******************************************************
      *                                                     *
      *      M O D I F I C A T I O N   H I S T O R Y        *
      *                                                     *
      *    DATE        BY            REASON                 *
      *                                                     *
      *******************************************************

      *******************************************************
      *                                                     *
      *          P R O G R A M   P U R P O S E              *
      *                                                     *
      *******************************************************

        This dialog is used to edit breakpoints and watchpoints for CEF.

      *************************************************************
      *                                                           *
      *        C O N T R I B U T O R S                            *
      *                                                           *
      *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan
}

unit Mem_Watchpoints_Dialog ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls, Grids,
     Menus,

     // C&C...
     CommonUt, // TInteger_List
     Standard, // TInteger_List64

     // CEF...
     _CEF, // TUI_Interface
     _CEFUtil ; // TCEF_Watchpoint_Manager

type TWatchpoint_Set = class
                           public // Constructors and destructors...
                               constructor Create ;
                               destructor Destroy ; override ;

                           private // Instance data...
                               _Addresses : TInteger64_List ;
                               _Accesses : TInteger_List ;
                               _Sizes : TInteger_List ;

                           public // API...
                               procedure Clear ; virtual ; stdcall ;
                               function Count : integer ; virtual ; stdcall ;

                               function Get_Address( Index : integer ) : int64 ;
                               procedure Set_Address( Index : integer ; Value : int64 ) ;
                               function Get_Access( Index : integer ) : integer ;
                               procedure Set_Access( Index, Value : integer ) ;
                               function Get_Size( Index : integer ) : integer ;
                               procedure Set_Size( Index, Value : integer ) ;

                               procedure Add( Address : int64 ; Size, Access : integer ) ;
                                   overload ;
                               procedure Add( Address : int64 ) ; overload ;
                               function Has_Accesses : boolean ;
                               function Has_Sizes : boolean ;

                               property Addresses[ Index : integer ] : int64
                                   read Get_Address
                                   write Set_Address ;
                               property Accesses[ Index : integer ] : integer
                                   read Get_Access
                                   write Set_Access ;
                               property Sizes[ Index : integer ] : integer
                                   read Get_Size
                                   write Set_Size ;
                       end ; // TWatchpoint_Set

type
  TMemory_Watchpoints_Dialog = class(TForm)
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Grid: TStringGrid;
    PopupMenu1: TPopupMenu;
    Delete1: TMenuItem;
    Properties1: TMenuItem;
    Add1: TMenuItem;
    DeleteAll1: TMenuItem;
    Help_Button: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure GridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DeleteAll1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);

  private // Instance data...
      _Watchpoints : TCEF_Watchpoint_Manager ;
      Term : string ; // "breakpoint" or "watchpoint", as appropriate
      _Port : boolean ;

  private // Internal utility routines...
      procedure Delete_Watchpoint ;
      procedure Insert_Watchpoint ;
      procedure Modify_Watchpoint ;

  public // API...
      CPU : TComponent ;

      procedure Set_Data( Watchpoints : TCEF_Watchpoint_Manager ;
          _CPU, Port : boolean ) ;
  end ;

var UI : TUI_Interface = nil ;

procedure Add_Breakpoint( _UI : TUI_Interface ; Component : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Base, Size : longint ;
    Port : boolean ) ;

procedure Add_Watchpoint( _UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ; Context : pointer ;
    Domain : PChar ) ;

procedure Show_Watchpoints( _UI : TUI_Interface ; CPU : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Ports : boolean ) ;


implementation

uses // C&C...
     ASCIIDef, // CR
     CVT, // CVTB

     // CEF...
     Create_Watchpoint ;

{$R *.dfm}

var Memory_Watchpoints_Dialog : TMemory_Watchpoints_Dialog = nil ;

// TWatchpoint_Set methods...

// Constructors and destructors...

constructor TWatchpoint_Set.Create ;

begin
    inherited Create ;

    _Addresses := TInteger64_List.Create ;
    _Accesses := TInteger_List.Create ;
    _Sizes := TInteger_List.Create ;
end ;


destructor TWatchpoint_Set.Destroy ;

begin
    _Addresses.Free ;
    _Addresses := nil ;
    _Accesses.Free ;
    _Accesses := nil ;
    _Sizes.Free ;
    _Sizes := nil ;

    inherited Destroy ;
end ;


// API...

procedure TWatchpoint_Set.Clear ;

begin
    _Addresses.Clear ;
    _Accesses.Clear ;
    _Sizes.Clear ;
end ;


function TWatchpoint_Set.Count : integer ;

begin
    Result := _Addresses.Count ;
end ;


function TWatchpoint_Set.Get_Address( Index : integer ) : int64 ;

begin
    Result := _Addresses[ Index ] ;
end ;


procedure TWatchpoint_Set.Set_Address( Index : integer ; Value : int64 ) ;

begin
    _Addresses[ Index ] := Value ;
end ;


function TWatchpoint_Set.Get_Access( Index : integer ) : integer ;

begin
    Result := _Accesses[ Index ] ;
end ;


procedure TWatchpoint_Set.Set_Access( Index, Value : integer ) ;

begin
    _Accesses[ Index ] := Value ;
end ;


function TWatchpoint_Set.Get_Size( Index : integer ) : integer ;

begin
    Result := _Sizes[ Index ] ;
end ;


procedure TWatchpoint_Set.Set_Size( Index, Value : integer ) ;

begin
    _Sizes[ Index ] := Value ;
end ;


procedure TWatchpoint_Set.Add( Address : int64 ; Size, Access : integer ) ;

begin
    _Addresses.Add( Address ) ;
    _Sizes.Add( Size ) ;
    _Accesses.Add( Access ) ;
end ;


procedure TWatchpoint_Set.Add( Address : int64 ) ;

begin
    _Addresses.Add( Address ) ;
end ;


function TWatchpoint_Set.Has_Accesses : boolean ;

begin
    Result := ( _Accesses.Count > 0 ) ;
end ;


function TWatchpoint_Set.Has_Sizes : boolean ;

begin
    Result := ( _Sizes.Count > 0 ) ; 
end ;



{ This routine takes a set of watchpoint lists, prompts the user for a new
  watchpoint, depending upon the component type and Memory parameter.  If the
  dialog is not cancelled, the watchpoint lists are updated.  Address and Access
  are ignored on call.  On return, they contain the address and access mode
  selected by the user.  If both are -1, there was no change to the watchpoint
  set. }
procedure Add_Watchpoint( _UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ; Context : pointer ;
    Domain : PChar ) ;

var Count, Loop : integer ;
    S : string ;

begin
    if( New_Watchpoint = nil ) then
    begin
        New_Watchpoint := TNew_Watchpoint.Create( Application ) ;
    end ;
    UI := _UI ;
    New_Watchpoint.Base := Base ;
    New_Watchpoint.Size.Value := Size ;
    New_Watchpoint.GroupBox1.Visible := True ;
    if (
         ( Component_Type = Component_Type_CPU )
         and
         ( not Memory )
       ) then
    begin
        if( New_Watchpoint.Size.Visible ) then // Was a memory watchpoint
        begin
            New_Watchpoint.Address.Text := '' ;
        end ;
        New_Watchpoint.Size.Visible := False ;
        New_Watchpoint.Label2.Visible := False ;
        New_Watchpoint.GroupBox1.Visible := True ;
        New_Watchpoint.Label1.Caption := 'Register:' ;
        New_Watchpoint.Caption := 'Create CPU Register Breakpoint' ;
        New_Watchpoint.CPU := Component ;
    end else
    begin
        New_Watchpoint.Address.Enabled := True ;
        if( not New_Watchpoint.Size.Visible ) then // Was a CPU breakpoint
        begin
            New_Watchpoint.Address.Text := '' ;
        end ;
        New_Watchpoint.GroupBox1.Visible := True ;
        New_Watchpoint.Size.Visible := True ;
        New_Watchpoint.Label2.Visible := True ;
        New_Watchpoint.Label1.Caption := 'Address:' ;
        New_Watchpoint.Caption := 'Create Memory Watchpoint' ;
        New_Watchpoint.CPU := nil ;
        New_Watchpoint.Low := Low ;
        New_Watchpoint.High := High ;
    end ;
    New_Watchpoint.Read.Caption := 'Read' ;
    New_Watchpoint.Write.Caption := 'Write' ;
    if( New_Watchpoint.ShowModal = mrOK ) then
    begin
        Access := 0 ;
        if( New_Watchpoint.Read.Checked ) then
        begin
            Access := Access or Access_Read ;
        end ;
        if( New_Watchpoint.Write.Checked ) then
        begin
            Access := Access or Access_Write ;
        end ;
        if (
             ( Component_Type = Component_Type_CPU )
             and
             ( not Memory )
           ) then
        begin
            Address := 0 ;
            while( True ) do
            begin
                Size := Component.CPU.Register_Size( Address ) ;
                if( Size = 0 ) then
                begin
                    break ;
                end ;
                S := Component.CPU.Register_Name( Address ) ;
                if( uppercase( S ) = uppercase( New_Watchpoint.Address.Text ) ) then
                begin
                    break ;
                end ;
                inc( Address ) ;
            end ;
        end else
        begin
            Address := strtoint( Cvtb( Base, 10, New_Watchpoint.Address.Text ) ) ;
        end ;
        if( Component_Type = Component_Type_CPU ) then
        begin
            New_Watchpoint.Size.Value :=
                Component.CPU.Register_Size( Address ) ;
        end ;
	    for Loop := 0 to Watchpoints.Count - 1 do
        begin
            if( Watchpoints.Watchpoints[ Loop ].Address = Address ) then
            begin
                if(
                    Memory
                    or
                    ( Component_Type <> Component_Type_CPU )
                    or
                    ( Watchpoints.Watchpoints[ Loop ].Size = New_Watchpoint.Size.Value )
                  ) then
                begin
                    if( Watchpoints.Watchpoints[ Loop ].Access = Access ) then
                    begin
                        Address := -1 ;
                        Access := -1 ;
                        exit ; // Duplicate watchpoint
                    end ;
                end ;
            end ;
        end ;

        // Update watchpoint set...
        Watchpoints.Create_Watchpoint_Ex( Address, New_Watchpoint.Size.Value, Access, Component, Domain ) ;

        if( Component = nil ) then
        begin
            exit ;
        end ;
        if (
             ( Component.CPU <> nil )
             and
             ( Component_Type = Component_Type_CPU )
             and
             ( not Memory )
           ) then
        begin
            Component.CPU.Set_Internal_Watchpoint( Address, Memory, Access ) ;
        end else
        begin
            Count := New_Watchpoint.Size.Value ;
            for Loop := 0 to Count - 1 do
            begin
                try
                    Component.Set_Watchpoint( Address, Memory, Access ) ;
                except
                end ;
                inc( Address ) ;
            end ;
        end ;
    end else
    begin
        Address := -1 ;
        Access := -1 ;
    end ; // if( New_Watchpoint.ShowModal = mrOK )
end ; // Add_Watchpoint


procedure Add_Breakpoint( _UI : TUI_Interface ; Component : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Base, Size : longint ;
    Port : boolean ) ;

var Access : integer ;
    Address : int64 ;
    Loop : integer ;

begin
    if( New_Watchpoint = nil ) then
    begin
        New_Watchpoint := TNew_Watchpoint.Create( Application ) ;
    end ;
    UI := _UI ;
    New_Watchpoint.Base := Base ;
    New_Watchpoint.Size.Value := Size ;
    New_Watchpoint.Address.Text := '' ;
    New_Watchpoint.Size.Visible := False ;
    New_Watchpoint.Label2.Visible := False ;
    if( Port ) then
    begin
        New_Watchpoint.Label1.Caption := 'Port:' ;
        New_Watchpoint.Caption := 'Create Port Breakpoint' ;
        New_Watchpoint.Low := Component.CPU.Get_Low_Port ;
        New_Watchpoint.High := Component.CPU.Get_High_Port ;
        New_Watchpoint.Read.Caption := 'Input' ;
        New_Watchpoint.Write.Caption := 'Output' ;
        New_Watchpoint.GroupBox1.Visible := True ;
    end else
    begin
        New_Watchpoint.Label1.Caption := 'Address:' ;
        New_Watchpoint.Caption := 'Create Execution Breakpoint' ;
        New_Watchpoint.Low := Component.CPU.Get_Low_Memory ;
        New_Watchpoint.High := Component.CPU.Get_High_Memory ;
        New_Watchpoint.GroupBox1.Visible := False ;
    end ;
    if( New_Watchpoint.ShowModal = mrOK ) then
    begin
        Address := strtoint( Cvtb( Base, 10, New_Watchpoint.Address.Text ) ) ;
	for Loop := 0 to Watchpoints.Count - 1 do
        begin
            if( Watchpoints.Watchpoint_at_Index( Loop ).Address = Address ) then
            begin
                exit ; // Duplicate watchpoint
            end ;
        end ;
        Watchpoints.Create_Watchpoint( Address ) ;
        if( Port ) then
        begin
            Access := 0 ;
            if( New_Watchpoint.Read.Checked ) then
            begin
                Access := Access or Access_Input ;
            end ;
            if( New_Watchpoint.Write.Checked ) then
            begin
                Access := Access or Access_Output ;
            end ;
            Component.Set_Watchpoint( Address, False, Access )
        end else
        begin
            Component.CPU.Set_Breakpoint( Address, 0, True ) ;
        end ;
    end ; // if( New_Watchpoint.ShowModal = mrOK )
end ; // Add_Breakpoint


procedure Show_Watchpoints( _UI : TUI_Interface ; CPU : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Ports : boolean ) ;

begin
    UI := _UI ;
    if( Memory_Watchpoints_Dialog = nil ) then
    begin
        Application.CreateForm( TMemory_Watchpoints_Dialog, Memory_Watchpoints_Dialog ) ;
    end ;
    if( Ports ) then
    begin
        Memory_Watchpoints_Dialog.Caption := 'Port breakpoints' ;
    end else
    if( CPU = nil ) then
    begin
        Memory_Watchpoints_Dialog.Caption := 'Watchpoints' ;
    end else
    begin
        Memory_Watchpoints_Dialog.Caption := 'Breakpoints' ;
    end ;

    Memory_Watchpoints_Dialog.CPU := CPU ;
    Memory_Watchpoints_Dialog.Set_Data( Watchpoints, CPU <> nil, Ports ) ;
    Memory_Watchpoints_Dialog.ShowModal ;
end ;



// TMemory_Watchpoints_Dialog...

// Internal utility routines...

procedure TMemory_Watchpoints_Dialog.Delete_Watchpoint ;

var Loop, Loop1 : integer ;
    Rect : TGridRect ;

begin
    Rect := Grid.Selection ;
    if( Rect.Top > 0 ) then // Not the header
    begin
        for Loop := Rect.Top + 1 to Grid.RowCount - 1 do
        begin
            for Loop1 := 0 to 2 do
            begin
                Grid.Cells[ Loop1, Loop - 1 ] := Grid.Cells[ Loop1, Loop ] ;
            end ;
        end ;
        if( Grid.RowCount = 2 ) then // Never less than 2 rows
        begin
            for Loop1 := 0 to 2 do
            begin
                Grid.Cells[ Loop1, 1 ] := '' ;
            end ;
        end else
        begin
            Grid.RowCount := Grid.RowCount - 1 ;
        end ;
    end ;
end ;


procedure TMemory_Watchpoints_Dialog.Insert_Watchpoint ;

var Count : integer ;
    S : string ;

begin
    Count := _Watchpoints.Count ;
    if( not _Watchpoints.Has_Sizes ) then
    begin
        if( _Port ) then
        begin
            UI.Add_Port_Breakpoint ;
        end else
        if( not _Watchpoints.Has_Accesses ) then
        begin
            UI.Add_Breakpoint ;
        end else
        begin
            UI.Add_Register_Breakpoint ;
        end ;
    end else
    begin
        UI.Create_New_Breakpoint ;
    end ;
    if( Count <> _Watchpoints.Count ) then // Watchpoint added
    begin
        Count := _Watchpoints.Count ;
        Grid.RowCount := Count + 1 ;
        if( _Watchpoints.Has_Sizes ) then
        begin
            Grid.Cells[ 0, Count ] := inttostr( _Watchpoints.Watchpoint_At_Index( Count - 1 ).Address ) ;
            Grid.Cells[ 1, Count ] := inttostr( _Watchpoints.Watchpoint_At_Index( Count - 1 ).Size ) ;
        end else
        begin
            if( _Port or ( not _Watchpoints.Has_Accesses ) ) then
            begin
                Grid.Cells[ 0, Count ] := inttostr( _Watchpoints.Watchpoint_At_Index( Count - 1 ).Address ) ;
            end else
            begin
                Grid.Cells[ 0, Count ] := CPU.CPU.Register_Name( _Watchpoints.Watchpoint_At_Index( Count - 1 ).Address ) ;
            end ;
            Grid.Cells[ 1, Count ] := '' ;
        end ;
        S := '' ;
        if( _Watchpoints.Has_Accesses ) then
        begin
            if( ( _Watchpoints.Watchpoint_At_Index( Count - 1 ).Access and Access_Read ) <> 0 ) then
            begin
                if( _Port ) then
                begin
                    S := 'input' ;
                end else
                begin
                    S := 'read' ;
                end ;
            end ;
            if( ( _Watchpoints.Watchpoint_At_Index( Count - 1 ).Access and Access_Write ) <> 0 ) then
            begin
                if( length( S ) > 0 ) then
                begin
                    S := S + ', ' ;
                end ;
                if( _Port ) then
                begin
                    S := S + 'output' ;
                end else
                begin
                    S := S + 'write' ;
                end ;
            end ;
            if( ( _Watchpoints.Watchpoint_At_Index( Count - 1 ).Access and Access_Execute ) <> 0 ) then
            begin
                if( length( S ) > 0 ) then
                begin
                    S := S + ', ' ;
                end ;
                S := 'execute' ;
            end ;
        end ; // if( _Watchpoints.Accesses.Count <> 0 )
        Grid.Cells[ 2, Count ] := S ;
    end ;
end ; // TMemory_Watchpoints_Dialog.Insert_Watchpoint


procedure TMemory_Watchpoints_Dialog.Modify_Watchpoint ;

var Loop : integer ;
    Rect : TGridRect ;
    S : string ;

begin
    if( New_Watchpoint = nil ) then
    begin
        New_Watchpoint := TNew_Watchpoint.Create( Application ) ;
    end ;
    Rect := Grid.Selection ;
    if( Rect.Top > 0 ) then // Not the header
    begin
        if( _Port ) then // Port breakpoint
        begin
            New_Watchpoint.Caption := 'Port Breakpoint' ;
            New_Watchpoint.Address.Text := inttostr( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Address ) ;
            if( _Watchpoints.Has_Sizes ) then
            begin
                New_Watchpoint.Size.Value := _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Size ;
            end ;
            New_Watchpoint.Address.Enabled := False ;
            New_Watchpoint.Size.Visible := False ;
            New_Watchpoint.Read.Checked :=
                ( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Read ) <> 0 ) ;
            New_Watchpoint.Write.Checked :=
                ( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Write ) <> 0 ) ;
            New_Watchpoint.Read.Caption := 'Input' ;
            New_Watchpoint.Write.Caption := 'Output' ;
            if( New_Watchpoint.ShowModal = mrOK ) then
            begin
                Loop := 0 ;
                if( New_Watchpoint.Read.Checked ) then
                begin
                    Loop := Access_Read ;
                end ;
                if( New_Watchpoint.Write.Checked ) then
                begin
                    Loop := Loop or Access_Write ;
                end ;
                _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access := Loop ;
            end ;
            New_Watchpoint.Address.Enabled := True ;
            New_Watchpoint.Size.Enabled := True ;
            New_Watchpoint.Size.Visible := True ;
        end else
        if( not _Watchpoints.Has_Accesses ) then // Execution breakpoint
        begin
            // Nothing to edit on execution breakpoints
        end else
        if( not _Watchpoints.Has_Sizes ) then // Register breakpoint
        begin
            New_Watchpoint.Caption := 'CPU Register Breakpoint' ;
            New_Watchpoint.Address.Text :=
                CPU.CPU.Register_Name( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Address ) ;
            if( _Watchpoints.Has_Sizes ) then
            begin
                New_Watchpoint.Size.Value := _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Size ;
            end ;
            New_Watchpoint.Address.Enabled := False ;
            New_Watchpoint.Size.Visible := False ;
            New_Watchpoint.Read.Checked :=
                ( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Read ) <> 0 ) ;
            New_Watchpoint.Write.Checked :=
                ( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Write ) <> 0 ) ;
            New_Watchpoint.Read.Caption := 'Read' ;
            New_Watchpoint.Write.Caption := 'Write' ;
            if( New_Watchpoint.ShowModal = mrOK ) then
            begin
                Loop := 0 ;
                if( New_Watchpoint.Read.Checked ) then
                begin
                    Loop := Access_Read ;
                end ;
                if( New_Watchpoint.Write.Checked ) then
                begin
                    Loop := Loop or Access_Write ;
                end ;
                _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access := Loop ;
            end ;
            New_Watchpoint.Address.Enabled := True ;
            New_Watchpoint.Size.Visible := True ;
        end else // Memory watchpoint
        begin
            New_Watchpoint.Caption := 'Memory Watchpoint' ;
            New_Watchpoint.Address.Text :=
                inttostr( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Address ) ;
            if( _Watchpoints.Has_Sizes ) then
            begin
                New_Watchpoint.Size.Value := _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Size ;
            end ;
            New_Watchpoint.Address.Enabled := False ;
            New_Watchpoint.Size.Visible := True ;
            New_Watchpoint.Size.Enabled := False ;
            New_Watchpoint.Read.Checked :=
                ( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Read ) <> 0 ) ;
            New_Watchpoint.Write.Checked :=
                ( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Write ) <> 0 ) ;
            New_Watchpoint.Read.Caption := 'Read' ;
            New_Watchpoint.Write.Caption := 'Write' ;
            if( New_Watchpoint.ShowModal = mrOK ) then
            begin
                Loop := 0 ;
                if( New_Watchpoint.Read.Checked ) then
                begin
                    Loop := Access_Read ;
                end ;
                if( New_Watchpoint.Write.Checked ) then
                begin
                    Loop := Loop or Access_Write ;
                end ;
                _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access := Loop ;
            end ;
            New_Watchpoint.Address.Enabled := True ;
            New_Watchpoint.Size.Enabled := True ;
        end ;
        if( _Watchpoints.Has_Sizes ) then
        begin
            Grid.Cells[ 1, Rect.Top ] := inttostr( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Size ) ;
        end else
        begin
            Grid.Cells[ 1, Rect.Top ] := '' ;
        end ;
        S := '' ;
        if( _Watchpoints.Has_Accesses ) then
        begin
            if( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Read ) <> 0 ) then
            begin
                S := 'read' ;
            end ;
            if( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Write ) <> 0 ) then
            begin
                if( length( S ) > 0 ) then
                begin
                    S := S + ', ' ;
                end ;
                S := 'write' ;
            end ;
            if( ( _Watchpoints.Watchpoint_At_Index( Rect.Top - 1 ).Access and Access_Execute ) <> 0 ) then
            begin
                if( length( S ) > 0 ) then
                begin
                    S := S + ', ' ;
                end ;
                S := 'execute' ;
            end ;
        end ; // if( _Watchpoints.Accesses.Count > 0 )
        Grid.Cells[ 2, Rect.Top ] := S ;
    end ; // if( Rect.Top > 0 )
end ; // TMemory_Watchpoints_Dialog.Modify_Watchpoint


// API...

procedure TMemory_Watchpoints_Dialog.Set_Data( Watchpoints : TCEF_Watchpoint_Manager ;
    _CPU, Port : boolean ) ;

var Loop : integer ;
    S : string ;
    Watchpoint : TCEF_Watchpoint ;

begin
    _Port := Port ;
    _Watchpoints := Watchpoints ;
    if( {( not _Watchpoints.Has_Sizes ) or} _CPU or Port ) then
    begin
        if( Port ) then
        begin
            Caption := 'Port breakpoints' ;
        end else
        if( not _Watchpoints.Has_Accesses ) then
        begin
            Caption := 'Execution breakpoint' ;
        end else
        begin
            Caption := 'Register breakpoints' ;
        end ;
        Term := 'breakpoint' ;
        Grid.Cells[ 1, 0 ] := '' ;
    end else
    begin
        Caption := 'Memory watchpoints' ;
        Term := 'watchpoint' ;
        Grid.Cells[ 1, 0 ] := 'Size' ;
    end ;

    Grid.RowCount := _Watchpoints.Count + 1 ;
    for Loop := 0 to _Watchpoints.Count - 1 do
    begin
        Watchpoint := _Watchpoints.Watchpoint_At_Index( Loop ) ;
        if( _Watchpoints.Has_Sizes ) then
        begin
            Grid.Cells[ 0, Loop + 1 ] := inttostr( Watchpoint.Address ) ;
            Grid.Cells[ 1, Loop + 1 ] := inttostr( Watchpoint.Size ) ;
        end else
        begin
            if( Port or ( not _Watchpoints.Has_Accesses ) ) then
            begin
                Grid.Cells[ 0, Loop + 1 ] := inttostr( Watchpoint.Address ) ;
            end else
            begin
                Grid.Cells[ 0, Loop + 1 ] := CPU.CPU.Register_Name( Watchpoint.Address ) ;
            end ;
            Grid.Cells[ 1, Loop + 1 ] := '' ;
        end ;
        S := '' ;
        if( _Watchpoints.Has_Accesses ) then
        begin
            if( ( Watchpoint.Access and Access_Read ) <> 0 ) then
            begin
                S := 'read' ;
            end ;
            if( ( Watchpoint.Access and Access_Write ) <> 0 ) then
            begin
                if( length( S ) > 0 ) then
                begin
                    S := S + ', ' ;
                end ;
                S := 'write' ;
            end ;
            if( ( Watchpoint.Access and Access_Execute ) <> 0 ) then
            begin
                if( length( S ) > 0 ) then
                begin
                    S := S + ', ' ;
                end ;
                S := 'execute' ;
            end ;
        end ; // if( _Watchpoints.Accesses.Count > 0 )
        Grid.Cells[ 2, Loop + 1 ] := S ;
        S := string( Watchpoint.Domain ) ;
        if( S = '' ) then
        begin
            S := 'Main' ;
        end ;
        Grid.Cells[ 3, Loop + 1 ] := S ;
    end ;
end ; // TMemory_Watchpoints_Dialog.Set_Data


procedure TMemory_Watchpoints_Dialog.FormShow( Sender : TObject ) ;

begin
    Grid.ColWidths[ 0 ] := ClientWidth div 4 - 2 ;
    Grid.ColWidths[ 1 ] := ClientWidth div 4 - 2 ;
    Grid.ColWidths[ 2 ] := ClientWidth div 4 - 2 ;
    Grid.ColWidths[ 3 ] := ClientWidth div 4 - 2 ;
    Grid.Cells[ 0, 0 ] := 'Address' ;
    if( not _Watchpoints.Has_Sizes ) then
    begin
        Grid.Cells[ 1, 0 ] := '' ;
    end else
    begin
        Grid.Cells[ 1, 0 ] := 'Size' ;
    end ;
    Grid.Cells[ 2, 0 ] := 'Access' ;
    Grid.Cells[ 3, 0 ] := 'Domain' ;
end ;


procedure TMemory_Watchpoints_Dialog.GridKeyUp( Sender : TObject ;
    var Key : Word ; Shift : TShiftState ) ;

begin
    if( ( Key = VK_RETURN ) or ( Key = VK_INSERT ) ) then // New watchpoint
    begin
        Insert_Watchpoint ;
        Key := 0 ;
    end else
    if( ( Key = ord( 'D' ) ) and ( ssCtrl in Shift ) ) then // Delete
    begin
        Delete_Watchpoint ;
        Key := 0 ;
    end else
    if( ( Key = ord( 'E' ) ) and ( ssCtrl in Shift ) ) then // Modify
    begin
        Modify_Watchpoint ;
        Key := 0 ;
    end ;
end ; // TMemory_Watchpoints_Dialog.GridKeyUp


procedure TMemory_Watchpoints_Dialog.DeleteAll1Click( Sender : TObject ) ;

var Loop : integer ;

begin
    if( MessageBox( 0, PChar( 'Really delete all ' + Term + 's?' ),
        'Delete all', MB_YESNO or MB_ICONQUESTION ) = mrYes ) then
    begin
       // Clear watchpoints from CPU...
       if( CPU <> nil ) then
       begin
           for Loop := 0 to _Watchpoints.Count - 1 do
           begin
               CPU.Clear_Watchpoint( _Watchpoints.Watchpoint_At_Index( Loop ).Address, False, Access_Execute ) ;
           end ;
       end ;

       // Clear watchpoints
       _Watchpoints.Clear ;

       // Clear Grid...
       Grid.RowCount := 2 ;
       for Loop := 0 to 2 do
       begin
           Grid.Cells[ Loop, 1 ] := '' ;
       end ;
    end ;
end ;


procedure TMemory_Watchpoints_Dialog.Add1Click( Sender : TObject ) ;

begin
    Insert_Watchpoint ;
end ;


procedure TMemory_Watchpoints_Dialog.Properties1Click( Sender : TObject ) ;

begin
    Modify_Watchpoint ;
end ;


procedure TMemory_Watchpoints_Dialog.Delete1Click(Sender: TObject);

begin
    Delete_Watchpoint ;
end ;


procedure TMemory_Watchpoints_Dialog.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;



end.
