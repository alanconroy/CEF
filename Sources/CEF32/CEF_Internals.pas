{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : Internals viewer for CEF32
        Institution  : Conroy & Conroy Co.
        Date Written : 26-Jul-2001
        Written By   : Alan Conroy
        Version      : 1.0

        Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

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

          This is the main debug form for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CEF_Internals ;

interface

uses // Borland...
     Windows,
     Messages,
     SysUtils,
     Classes,
     Graphics,
     Controls,
     Forms, // TForm
     Dialogs,
     ComCtrls, // TTreeView
     VCLDebug ; // TComponent_Debug_Iterator

type TInternals_Form = class( TForm )
                        Tree_View : TTreeView ;

                        procedure FormCreate( Sender : TObject ) ;

                    protected // Callbacks...
                        procedure CB_Click( Sender : TObject ) ;
                        procedure CB_Collapse( Sender : TObject ; Node : TTreeNode ) ;
                        procedure CB_Expand( Sender : TObject ; Node : TTreeNode ) ;
                end ;

implementation

uses // CEF...
     CEFMain, // Main_Form

     // Other...
     _DebugIn, // TDebug_Interface
     DebugInt, // TText_Debugger
     UStrings ; // Pointer_To_String

{$R *.DFM}

type TComponent_Iterator = class( TText_Debugger )
                               protected // Property handlers...
                                   function Child( Index : longint ) : PDebug_Interface ;
                                       override ;

                               public // API...
                                   function Count : integer ; override ;
                           end ;


// TComponent_Iterator methods...

// Property handlers...

function TComponent_Iterator.Child( Index : longint ) : PDebug_Interface ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Result := PDebug_Interface( Main_Form.Main_Memory.Debugger ) ;
                if( Result = nil ) then
                begin
                    Result := PDebug_Interface( TText_Debugger.Create ) ;
                end ;
                Result.Title := 'Memory' ;
            end ;
    end ;
end ;


// API...

function TComponent_Iterator.Count : integer ;

begin
    Result := 1 ;
end ;



type THandle_Debug_Iterator = class( TText_Debugger )
                                protected // Property handlers...
                                    function Child( Index : longint ) : PDebug_Interface ;
                                        override ;

                                public // API...
                                    Handle : integer ;

                                    function Count : integer ; override ;
                            end ;

// THandle_Debug_Iterator methods...

// Property handlers...

function THandle_Debug_Iterator.Child( Index : longint ) : PDebug_Interface ;

var Control : TWinControl ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Result := PDebug_Interface( TText_Debugger.Create ) ;
                Result.Title := PChar( 'Handle = ' + inttostr( Handle ) ) ;
            end ;
        1 : begin
                Control := FindControl( Handle ) ;
                Result := Component_Debugger( Control ) ;
                Result.Title := PChar( 'Object = ' + Component_Description( Control ) ) ;
            end ;
    end ;
end ;


// API...

function THandle_Debug_Iterator.Count : integer ;

begin
    Result := 1 ;
    if( FindControl( Handle ) <> nil ) then
    begin
        inc( Result ) ;
    end ;
end ;



type TMain_Debug_Iterator = class( TText_Debugger )
                                protected // Property handlers...
                                    function Child( Index : longint ) : PDebug_Interface ;
                                        override ;

                                public // API...
                                    function Count : integer ; override ;
                            end ;

// TMain_Debug_Iterator methods...

// Property handlers...

function TMain_Debug_Iterator.Child( Index : longint ) : PDebug_Interface ;

begin
    Result := nil ;
    case Index of
        0 : begin
                Result := PDebug_Interface( TComponent_Iterator.Create ) ;
                Result.Title := 'Components' ;
            end ;
        1 : begin
                Result := PDebug_Interface( THandle_Debug_Iterator.Create ) ;
                THandle_Debug_Iterator( Result ).Handle := GetActiveWindow ;
                Result.Title := 'Focused Control' ;
            end ;
    end ;
end ;


// API...

function TMain_Debug_Iterator.Count : integer ;

begin
    Result := 2 ;
end ;



// Unit variables...

var Iterator : TMain_Debug_Iterator ;


// TForm1 methods...

procedure TInternals_Form.FormCreate( Sender : TObject ) ;

var I : TDebug_Interface ;
    Loop : integer ;
    This_Node : TTreeNode ;
    
begin
    // Initialize the outline...
    Iterator := TMain_Debug_Iterator.Create ;
    for Loop := 0 to Iterator.Count - 1 do
    begin
        I := Iterator.Child( Loop ) ;
        This_Node := Tree_View.Items.AddChildObject( nil, I.Title, I ) ;
        if( I.Count > 0 ) then
        begin
            Tree_View.Items.AddChildObject( This_Node, '', nil ) ; // Place holder
        end ;
    end ;

    // Set up callbacks...
    Tree_View.OnClick := CB_Click ;
    Tree_View.OnCollapsed := CB_Collapse ;
    Tree_View.OnExpanded := CB_Expand ;
end ;


// Callbacks...

procedure TInternals_Form.CB_Click( Sender : TObject ) ;

var I : TDebug_Interface ;

begin
    I := TDebug_Interface( Tree_View.Selected.Data ) ;
    I.Activate ;
end ;


procedure TInternals_Form.CB_Collapse( Sender : TObject ; Node : TTreeNode ) ;

var I : TDebug_Interface ;
    This_Node : TTreeNode ;

begin
    This_Node := Node.GetFirstChild ;
    while( This_Node <> nil ) do
    begin
        I := TDebug_Interface( This_Node.Data ) ;
        I.Free ;
        This_Node := This_Node.GetNext ;
    end ;
    Node.DeleteChildren ;
    I := TDebug_Interface( Node.Data ) ;
    if( I.Count > 0 ) then
    begin
        Tree_View.Items.AddChildObject( Node, '', nil ) ;
    end ;
end ;


procedure TInternals_Form.CB_Expand( Sender : TObject ; Node : TTreeNode ) ;

var I, I1 : TDebug_Interface ;
    Loop : integer ;
    This_Node : TTreeNode ;

begin
    Cursor := crHourGlass ;
    I := TDebug_Interface( Node.Data ) ;
    if( Node.GetFirstChild <> nil ) then
    begin
        Tree_View.Items.Delete( Node.GetFirstChild ) ; // Delete placeholder
    end ;
    for Loop := 0 to I.Count - 1 do
    begin
        I1 := I.Child( Loop ) ;
        This_Node := Tree_View.Items.AddChildObject( Node, I1.Title, I1 ) ;
        if( I1.Count > 0 ) then
        begin
            Tree_View.Items.AddChildObject( This_Node, '', nil ) ; // Place holder
        end ;
    end ;
    Cursor := crDefault	;
end ;



end.
