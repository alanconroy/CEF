unit Macro_11 ;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses // Borland...
     Classes,
     SysUtils,

     // C&C...
     _Streams, // TCOM_Stream

     // CEF...
     CEF ; // TAssembler_Status

{ Do the assembly.  Source = source filename.  Target = Target filename.
  Flags = xref and symbol list flags. Job_Flags = $JSX flags.  Initial_SP =
  initial stack pointer.  JSW = Job Status Word.  Sav_File is true if output to
  a SAV file (otherwise OBJect file). }
procedure Assemble( Source, Target : string ;
    Flags, Job_Flags, Initial_SP, JSW : longint ; Status : TAssembler_Status ;
    Listing : TCOM_Stream ; Sav_File : boolean ; Name : string ) ;


implementation

uses // Borland...
     Windows, // LoadLibrary
     Dialogs, // ShowMessage

     // C&C...
     _DebugIn, // TDebug_Interface
     _UEHDefs, // TUEC
     ASCIIDef, // CR
     FStreams, // TCOM_File_Stream
     _Octal, // O*
     Parse, // TString_Parser
     Radix50s, // RAD50
     Symbols, // SERT

     // CEF...
     _CEFUtil, // TCEF_Assembler
     CEFUtil_Int ; // Get_Master_Assembler

type TOutput_Streamer = class( TCOM_Stream )
                            private // Instance data...
                                Address : longint ; // Current position
                                Code_Size : longint ; // Maximum position

                            public // Public data...
                                Data : array[ 0..131071 ] of byte ;

                            public // API...
                                function At_End : boolean ; override ; stdcall ;

                                function Facility_ID : longint ;
                                    override ; stdcall ;

                                procedure Last_Error( var UEC : TUEC ) ;
                                    override ;

                                procedure Read( var Buffer ; var Size : longint ) ;
                                    override ; stdcall ;

                                procedure Read_Line( var Buffer ; var Size : longint ) ;
                                    override ; stdcall ;

                                procedure Seek( Position : longint ) ;
                                    override ; stdcall ;

                                function Size : longint ; override ;

                                procedure Write( var Buffer ; size : longint ) ;
                                    override ; stdcall ;

                                procedure Write_Line( Buffer : PChar ) ;
                                    override ; stdcall ;
                        end ; // TOutput_Streamer

// API...

function TOutput_Streamer.At_End : boolean ;

begin
    Result := ( Address >= Code_Size ) ;
end ;


function TOutput_Streamer.Facility_ID : longint ;

begin
    Result := -1 ;
end ;


procedure TOutput_Streamer.Last_Error( var UEC : TUEC ) ;

begin
    fillchar( UEC, sizeof( UEC ), 0 ) ;
end ;


procedure TOutput_Streamer.Read( var Buffer ; var Size : longint ) ;

begin
    // This is an output-only streamer
end ;


procedure TOutput_Streamer.Read_Line( var Buffer ; var Size : longint ) ;

begin
    // This is an output-only streamer
end ;


procedure TOutput_Streamer.Seek( Position : longint ) ;

begin
    Address := Position ;
end ;


function TOutput_Streamer.Size : longint ;

begin
    Size := Code_Size ;
end ;


procedure TOutput_Streamer.Write( var Buffer ; size : longint ) ;

var P : PChar ;

begin
    if( Address + Size > sizeof( Data ) ) then
    begin
        exit ;
    end ;
    if( Address + Size > Code_Size ) then
    begin
        Code_Size := Address + Size - 1 ;
    end ;
    P := PChar( @Buffer ) ;
    move( P[ 0 ], Data[ Address ], Size ) ;
end ;


procedure TOutput_Streamer.Write_Line( Buffer : PChar ) ;

var Loop : integer ;

begin
    Loop := 0 ;
    while( Buffer[ Loop ] <> CR ) do
    begin
        Write( Buffer[ Loop ], 1 ) ;
        inc( Loop ) ;
    end ;
end ;



type TAssembler_UI = class( TUI_Interface )
                        procedure Block( Component : TComponent ; Blocked : boolean ) ;
                            override ; stdcall ;

                        function Breakpoint_Notice( Address : int64 ; Physical : boolean ;
                            Space : integer ; CPU : TComponent ) : boolean ; override ; stdcall ;

                        function Clock : TMaster_Clock ; override ; stdcall ;

                        function Debugger : TDebug_Interface ; override ; stdcall ;

                        function Get_File_Stream( Name : PChar ) : TCOM_Stream ;
                            override ; stdcall ;

                        procedure Hide( Flag : boolean ) ; override ; stdcall ;

                        procedure Idle( Component : TComponent ) ; override ; stdcall ;

                        procedure Log_Error( Component : TComponent ; Text : PChar ;
                            Severity : longint ) ; override ; stdcall ;

                        procedure Log_Simulated_Error( Component : TComponent ; Text : PChar ;
                            Severity : longint ) ; override ; stdcall ;

                        procedure Log_Status( Text : PChar ; Index : longint ) ;
                            override ; stdcall ;

                        procedure Log_Trace( Component : TComponent ; Description : PChar ) ;
                            override ; stdcall ;

                        procedure Signal_Change_Notice( Component : TComponent ;
                            Index : longint ; Active : boolean ) ; override ; stdcall ;

                        procedure Signal_Exception( Component : TComponent ;
                            Description : PChar ; Index : longint ) ; override ; stdcall ;

                        procedure State_Change_Notice( Component : TComponent ;
                            Index : longint ; Active : boolean ) ; override ; stdcall ;

                        procedure Terminate ; override ; stdcall ;

                        procedure Toggle_Embed( Component : TComponent ) ; override ; stdcall ;

                        function Version : integer ; override ; stdcall ;

                        procedure Want_Signals( Component : TComponent ; Value : boolean ) ;
                            override ; stdcall ;

                        procedure Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
                            Component : TComponent ; Memory, Internal, Port : boolean ) ;
                            override ; stdcall ;

                        function Get_Port_Name( Index : longint ) : PChar ;
                            override ; stdcall ;

                        function Get_Port_Description( Index : longint ) : PChar ;
                            override ; stdcall ;

                        function Get_Port( Index : longint ) : TComponent ;
                           override ; stdcall ;

                        function Get_Port_Connection( Index : longint ) : TComponent ;
                            override ; stdcall ;

                        function Port_Parent_Component( Index : longint ) : TComponent ;
                            override ; stdcall ;

                        procedure Run( State : boolean ) ; override ; stdcall ;

                        function Process_ID( Name : PChar ; Force : boolean ) : integer ;
                            override ; stdcall ;

                        function Process_Start( ID : longint ;
                            var Priority : longint ) : boolean ; override ; stdcall ;

                        procedure Process_End( ID : longint ) ; override ; stdcall ;

                        procedure Process_Deleted( ID : longint ) ; override ; stdcall ;

                        procedure Add_Port_Breakpoint ; override ; stdcall ;

                        procedure Add_Breakpoint ; override ; stdcall ;

                        procedure Add_Register_Breakpoint ; override ; stdcall ;

                        procedure Create_New_Breakpoint ; override ; stdcall ;

                        function Get_Component_Filename( Name : PChar ) : PChar ;
                             override ; stdcall ;

                        procedure Termination_Notice( C : TComponent ) ;
                             override ; stdcall ;

                        function Load_Component( Name : PChar ) : TComponent ;
                             override ; stdcall ;
                     end ;


procedure TAssembler_UI.Block( Component : TComponent ; Blocked : boolean ) ;

begin
end ;


function TAssembler_UI.Breakpoint_Notice( Address : int64 ; Physical : boolean ;
    Space : integer ; CPU : TComponent ) : boolean ;

begin
    Result := True ;
end ;


function TAssembler_UI.Clock : TMaster_Clock ;

begin
    Result := nil ;
end ;


function TAssembler_UI.Debugger : TDebug_Interface ;

begin
    Result := nil ;
end ;


function TAssembler_UI.Get_File_Stream( Name : PChar ) : TCOM_Stream ;

var UEC : TUEC ;

begin
    Result := Create_File_Stream( Name, UEC ) ;
end ;


procedure TAssembler_UI.Hide( Flag : boolean ) ;

begin
end ;


procedure TAssembler_UI.Idle( Component : TComponent ) ;

begin
end ;


procedure TAssembler_UI.Log_Error( Component : TComponent ; Text : PChar ;
    Severity : longint ) ;

begin
end ;


procedure TAssembler_UI.Log_Simulated_Error( Component : TComponent ; Text : PChar ;
    Severity : longint ) ;

begin
end ;


procedure TAssembler_UI.Log_Status( Text : PChar ; Index : longint ) ;

begin
end ;


procedure TAssembler_UI.Log_Trace( Component : TComponent ; Description : PChar ) ;

begin
end ;


procedure TAssembler_UI.Signal_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

begin
end ;


procedure TAssembler_UI.Signal_Exception( Component : TComponent ;
    Description : PChar ; Index : longint ) ;

begin
end ;


procedure TAssembler_UI.State_Change_Notice( Component : TComponent ;
    Index : longint ; Active : boolean ) ;

begin
end ;


procedure TAssembler_UI.Terminate ;

begin
end ;


procedure TAssembler_UI.Toggle_Embed( Component : TComponent ) ;

begin
end ;


function TAssembler_UI.Version : integer ;

begin
    Result := 26 ;
end ;


procedure TAssembler_UI.Want_Signals( Component : TComponent ; Value : boolean ) ;

begin
end ;


procedure TAssembler_UI.Watchpoint_Notice( Address : int64 ; Access, Tag : longint ;
    Component : TComponent ; Memory, Internal, Port : boolean ) ;

begin
end ;


function TAssembler_UI.Get_Port_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TAssembler_UI.Get_Port_Description( Index : longint ) : PChar ;

begin
    Result := nil ;
end ;


function TAssembler_UI.Get_Port( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TAssembler_UI.Get_Port_Connection( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


function TAssembler_UI.Port_Parent_Component( Index : longint ) : TComponent ;

begin
    Result := nil ;
end ;


procedure TAssembler_UI.Run( State : boolean ) ;

begin
end ;


function TAssembler_UI.Process_ID( Name : PChar ; Force : boolean ) : integer ;

begin
    Result := 0 ;
end ;


function TAssembler_UI.Process_Start( ID : longint ;
    var Priority : longint ) : boolean ;

begin
    Result := True ;
end ;


procedure TAssembler_UI.Process_End( ID : longint ) ;

begin
end ;


procedure TAssembler_UI.Process_Deleted( ID : longint ) ;

begin
end ;


procedure TAssembler_UI.Add_Port_Breakpoint ;

begin
end ;


procedure TAssembler_UI.Add_Breakpoint ;

begin
end ;


procedure TAssembler_UI.Add_Register_Breakpoint ;

begin
end ;


procedure TAssembler_UI.Create_New_Breakpoint ;

begin
end ;


function TAssembler_UI.Get_Component_Filename( Name : PChar ) : PChar ;

begin
    REsult := Name ;
end ;


procedure TAssembler_UI.Termination_Notice( C : TComponent ) ;

begin
end ;


function TAssembler_UI.Load_Component( Name : PChar ) : TComponent ;

begin
    Result := nil ;
end ;


var CPU : TComponent = nil ;
    Master_Assembler : TMaster_Assembler = nil ;


type TPSect = class
                  public // Constructors and destructors...
                      constructor Create ;
                      destructor Destroy ; override ;

                  public // Instance data...
                      Name : string ;
                      Address : int64 ;

                      Externals : TStringList ;

                      Global : boolean ;
                      I_Space : boolean ;
                      Overlay : boolean ;
                      Read_Only : boolean ;
                      Relocatable : boolean ;
                      Sav : boolean ;
              end ;

// Constructors and destructors...

constructor TPSect.Create ;

begin
    inherited Create ;

    Externals := TStringList.Create ;
end ;


destructor TPSect.Destroy ;

begin
    Externals.Free ;

    inherited Destroy ;
end ;


type TExtension = class( TAssembler_Extension )
                      public // Constructors and destructors...
                          constructor Create ;
                          destructor Destroy ; override ;

                      private // Instance data...
                          List : TList ;

                      public // API...
                          function Process_Directive( Source : PChar ;
                              var Res : PChar ;
                              var Res_Length : longint ;
                              Status : TAssembler_Status ) : TUEC ;
                              override ; stdcall ;

                              procedure External_Symbol( Source : PChar ) ;
                                  override ; stdcall ;
                  end ; // TExtension


// Constructors and destructors...

constructor TExtension.Create ;

begin
    inherited Create ;

    List := TList.Create ;
end ;


destructor TExtension.Destroy ;

var Loop : integer ;

begin
    for Loop := 0 to List.Count - 1 do
    begin
        TPSect( List[ Loop ] ).Free ;
        List[ Loop ] := nil ;
    end ;
    List.Free ;
    List := nil ;

    inherited Destroy ;
end ;


// API...

var Have_Limit : boolean = False ;
    Limit : integer ;
    Ident : int64 ;

function TExtension.Process_Directive( Source : PChar ; var Res : PChar ;
    var Res_Length : longint ; Status : TAssembler_Status ) : TUEC ;

var Delimiter : char ;
    Dummy : integer ;
    P : pSymbol_Record ;
    Pc : PChar ;
    S : string ;
    Value, Work : string ;
    PSect : TPSect ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    Value := uppercase( string( Source ) ) ;
    if( copy( Value, 1, 6 ) = '.GLOBL' ) then
    begin
        Value := trim( copy( Value, 7, length( Value ) ) ) ;
        while( Value <> '' ) do
        begin
            Dummy := pos( ',', Value + ',' ) ;
            new( P ) ;
            fillchar( P^, sizeof( P^ ), 0 ) ;
            P^.Size := 2 ;
            P^.Typ := 1 ;
            P^.Flags := SF_Constant or SF_External ;
            Result := Master_Assembler.Add_Symbol( PChar( copy( Value, 1, Dummy - 1 ) ), P ) ;
            if( Result.Code <> 0 ) then
            begin
                Status.Log_Error( PChar( SERT( Result.Code ) ), nil, -1, Severity_Error ) ;
            end ;
            Value := copy( Value, Dummy + 1, length( Value ) ) ;
        end ;
        exit ;
    end ;
    if( Value = '.ASECT' ) then
    begin
        Value := '.PSECT . ABS.,RW,I,GBL,ABS,OVR' ;
    end ;
    if( copy( Value, 1, 6 ) = '.CSECT' ) then
    begin
        Value := trim( copy( Value, 7, length( Value ) ) ) ;
        if( Value = '' ) then
        begin
            Value := '. BLK.' ;
        end ;
        Value := '.PSECT ' + Value + ',RW,I,LCL,REL,CON' ;
    end ;
    if( copy( uppercase( Value ), 1, 7 ) = '.PSECT ' ) then
    begin
        Work := trim( copy( Value, 8, length( Value ) ) ) ;
        if( Work <> '' ) then
        begin
            Dummy := pos( ',', Work + ',' ) ;
            if( pos( ' ', Work + ' ' ) < Dummy ) then
            begin
                Dummy := pos( ' ', Work ) ;
            end ;
            if( pos( HT, Work + HT ) < Dummy ) then
            begin
                Dummy := pos( HT, Work ) ;
            end ;
            Value := copy( Work, 1, Dummy - 1 ) ;
            Work := copy( Work, Dummy + 1, length( Work ) ) ;
            if( Value <> '' ) then
            begin
                PSect := TPSect.Create ;
                List.Add( PSect ) ;
                PSect.Name := Value ;
                PSect.Address := CPU.CPU.Get_Current_Address( 0, True ) ;
                new( P ) ;
                fillchar( P^, sizeof( P^ ), 0 ) ;
                P^.Address := PSect.Address ;
                P^.Typ := 1 ; // integer
                P^.Flags := 1 ; // Constant
                while( Work <> '' ) do
                begin
                    Dummy := pos( ',', Work + ',' ) ;
                    Value := uppercase( copy( Work, 1, Dummy - 1 ) ) ;
                    Work := copy( Work, Dummy + 1, length( Work ) ) ;
                    if( Value = 'RW' ) then
                    begin
                        PSect.Read_Only := False ;
                    end else
                    if( Value = 'RO' ) then
                    begin
                        PSect.Read_Only := True ;
                    end else
                    if( Value = 'I' ) then
                    begin
                        PSect.I_Space := True ;
                    end else
                    if( Value = 'D' ) then
                    begin
                        PSect.I_Space := False ;
                    end else
                    if( Value = 'GBL' ) then
                    begin
                        PSect.Global := True ;
                        P^.Typ := P^.Typ or SF_Global ;
                    end else
                    if( Value = 'LCL' ) then
                    begin
                        PSect.Global := False ;
                    end else
                    if( Value = 'ABS' ) then
                    begin
                        PSect.Relocatable := False ;
                    end else
                    if( Value = 'REL' ) then
                    begin
                        PSect.Relocatable := True ;
                    end else
                    if( Value = 'CON' ) then
                    begin
                        PSect.Overlay := False ;
                    end else
                    if( Value = 'OVR' ) then
                    begin
                        PSect.Overlay := True ;
                    end else
                    if( Value = 'SAV' ) then
                    begin
                        PSect.Sav := True ;
                    end else
                    if( Value = 'NOSAV' ) then
                    begin
                        PSect.Sav := False ;
                    end else
                    begin
                        Status.Log_Error( PChar( 'Invalid attributes ' + Value ), nil, -1, Severity_Error ) ;
                    end ;
                end ;
                Result := Master_Assembler.Add_Symbol( PChar( Value ), P ) ;
                if( Result.Code <> 0 ) then
                begin
                    Status.Log_Error( PChar( SERT( Result.Code ) ), nil, -1, Severity_Error ) ;
                end ;
            end ;
        end ; // if( Work <> '' )
        exit ;
    end else
    if( copy( Value, 1, 6 ) = '.LIMIT' ) then
    begin
        Limit := CPU.CPU.Get_Current_Address( 0, True ) ;
        Value := 'DW' ;
        Result := Master_Assembler.Expand( PChar( Value ), PC, Dummy, nil ) ;
        Have_Limit := True ;
        exit ;
    end else
    if( copy( Value, 1, 6 ) = '.IDENT' ) then
    begin
        Value := trim( copy( Value, 7, length( Value ) ) ) ;
        S := copy( Value, 1, 1 ) ; // Get delimiter
        if( copy( Value, length( Value ), 1 ) <> S ) then
        begin
            Status.Log_Error( 'Unterminated literal', nil, -1, Severity_Error ) ;
            exit ;
        end ;
        Value := copy( Value, 2, length( Value ) ) ;
        setlength( Value, length( Value ) - 1 ) ;
        if( length( Value ) > 6 ) then
        begin
            Status.Log_Error( 'Value too long', nil, -1, Severity_Error ) ;
            exit ;
        end ;
        if( length( Value ) = 0 ) then
        begin
            Status.Log_Error( 'Missing value', nil, -1, Severity_Error ) ;
            exit ;
        end ;
        Ident := Rad50( Value ) ;
        exit ;
    end ;
    Result.Code := -1 ;
end ; // TExtension.Process_Directive


procedure TExtension.External_Symbol( Source : PChar ) ;

var PSect : TPSect ;
    S : string ;

begin
    S := trim( string( Source ) ) ;
    if( List.Count = 0 ) then
    begin
        PSect := TPSect.Create ;
        List.Add( PSect ) ;
    end ;
    PSect := TPSect( List[ List.Count - 1 ] ) ;
    PSect.Externals.Add( Source ) ;
end ;


type Pa = array[ 0..2 ] of Integer ;

const Radix = ' ABCDEFGHIJKLMNOPQRSTUVWXYZ$. 0123456789' ;
      Power : Pa = ( 1, 40, 1600 ) ;



function Build_Complex_Relocation( S : string ) : string ;

var Dummy : integer ;
    Parser : TString_Parser ;
    Term : string ;
    After_Operator : boolean ;

begin
    After_Operator := True ;
    Parser := TString_Parser.Create ;
    Parser.Set_Source( uppercase( S ) ) ;
    while( not Parser.Token_EOL ) do
    begin
        Term := Parser.Token ;
        if( Term = '+' ) then
        begin
            Result := Result + #1 ;
            After_Operator := True ;
        end else
        if( Term = '-' ) then
        begin
            if( After_Operator ) then
            begin
                Result := Result + chr( O10 ) ;
            end else
            begin
                Result := Result + #2 ;
            end ;
            After_Operator := True ;
        end else
        if( Term = '*' ) then
        begin
            Result := Result + #3 ;
            After_Operator := True ;
        end else
        if( Term = '/' ) then
        begin
            Result := Result + #4 ;
            After_Operator := True ;
        end else
        if( ( Term = '&' ) or ( Term = 'AND' ) ) then
        begin
            Result := Result + #5 ;
            After_Operator := True ;
        end else
        if( ( Term = '!' ) or ( Term = 'OR' ) ) then
        begin
            Result := Result + #6 ;
            After_Operator := True ;
        end else
        begin
            Dummy := Rad50( Term ) ;
            Result := Result + chr( O16 ) + chr( Dummy and 255 ) + chr( Dummy shr 8 ) + chr( Dummy shr 16 ) + chr( Dummy shr 24 ) ;
            After_Operator := False ;
        end ;
    end ; // while( not Parser.Token_EOL )
    Result := Result + #10 ; // Store (and end)
end ;


var Ext : TExtension ;
    H : THandle = 0 ;
    F : function( Serial_Number : integer ; UI : TUI_Interface ) : TComponent ; stdcall ;
    UI : TAssembler_UI = nil ;

procedure Assemble( Source, Target : string ;
    Flags, Job_Flags, Initial_SP, JSW : longint ; Status : TAssembler_Status ;
    Listing : TCOM_Stream ; Sav_File : boolean ; Name : string ) ;

var Block_Size : integer ;
    Block_Data : array[ 0..65535 ] of byte ;
    Outfile : file ;
    Output : TCOM_Stream ;

    procedure Start_Binary_Block ;

    begin
        Block_Size := 4 ; // Next offset in Block_Data
        Block_Data[ 0 ] := 1 ;
        Block_Data[ 1 ] := 0 ;
    end ;


    procedure Finish_Binary_Block ;

    var Loop, Checksum : integer ;

    begin
        Block_Data[ 2 ] := Block_Size and 255 ;
        Block_Data[ 3 ] := Block_Size shr 8 ;
        Checksum := 0 ;
        for Loop := 0 to Block_Size - 1 do
        begin
            Checksum := Checksum + Block_Data[ Loop ] ;
        end ;
        Output.Write( Block_Data, Block_Size ) ;
        Checksum := -Checksum ;
        Output.Write( Checksum, 1 ) ;
    end ;


    procedure Write_Block_Byte( Value : byte ) ;

    begin
        Block_Data[ Block_Size ] := Value ;
        inc( Block_Size ) ;
    end ;


    procedure Write_Block_Word( Value : word ) ;

    begin
        Write_Block_Byte( Value and 255 ) ;
        Write_Block_Byte( Value shr 8 ) ;
    end ;


    procedure Write_Block_Longword( Value : longint ) ;

    begin
        Write_Block_Word( Value and $FFFF ) ;
        Write_Block_Word( Value shr 16 ) ;
    end ;


    procedure Write_Block_Data( Address, Size : longint ) ;

    begin
        move( TOutput_Streamer( Output ).Data[ Address ], Block_Data[ Block_Size ], Size ) ;
        Block_Size := Block_Size + Size ;
    end ;


var Assembler_Context : TCEF_Assembler_Context ;
    Dummy : integer ;
    File_Stream : TCOM_Stream ;
    Load_Offset : longint ;
    Load_PSect : int64 ;
    Loop, Loop1 : integer ;
    PSect : TPSect ;
    PName : longint ;
    S : string ;
    Size, Start_PC : longint ;
    UEC : TUEC ;

    Addr : int64 ;
    Flag, DT, DSize : integer ;
    Ptr : pointer ;
    PCh : PChar ;

begin
    // Make sure PDP-11 is loaded...
    if( H = 0 ) then
    begin
        H := Loadlibrary( PChar( 'components\pdp11.dll' ) ) ;
        if( H = 0 ) then
        begin
            ShowMessage( 'Could not load PDP-11 assembler' ) ;
            exit ;
        end ;
        F := GetProcAddress( H, 'Make_Instance' ) ;
        if( @F = nil ) then
        begin
            FreeLibrary( H ) ;
            ShowMessage( 'Could not load PDP-11 assembler.  Invalid component.' ) ;
            exit ;
        end ;

        UI := TAssembler_UI.Create ;

        CPU := F( 0, UI ) ;
        if( CPU.CPU = nil ) then // Not a CPU
        begin
            ShowMessage( 'Could not load PDP-11 assembler.  Invalid component.' ) ;
            exit ;
        end ;

        Master_Assembler := Get_Master_Assembler( CPU, UI ) ;
    end ;

    // Do the assembly...
    Ident := 0 ;
    Have_Limit := False ;
    Output := nil ;
    if( Target <> '' ) then
    begin
        Output := TOutput_Streamer.Create ;
    end ;
    File_Stream := Create_File_Stream( PChar( Source ), UEC ) ;
    if( File_Stream = nil ) then
    begin
        ShowMessage( PChar( 'Could not open file: ' + Source ) ) ;
        exit ;
    end ;
    Ext := TExtension.Create ;
    Master_Assembler.Register_Extension( Ext ) ;
    UEC := Master_Assembler.Assemble_Ex( File_Stream, Output, Listing, Status, Flags ) ;
    File_Stream.Terminate ;

    // Write output file...
    if( Output <> nil ) then
    begin
        if( Sav_File ) then
        begin
            // Write SAV header info...
            Start_PC := CPU.CPU.Get_Current_Address( 0, True ) ;
            Output.Seek( 4 ) ;
            Output.Write( Job_Flags, 2 ) ;
            Output.Seek( 32 ) ;
            Output.Write( Start_PC, 2 ) ;
            Output.Write( Initial_SP, 2 ) ;
            Output.Write( JSW, 2 ) ;
            Output.Seek( 40 ) ;
            Dummy := Output.Size ;
            Output.Write( Dummy, 2 ) ;

            // Write output file...
            assignfile( Outfile, Target ) ;
            {$I-}
            rewrite( Outfile, 1 ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( PChar( 'Cannot create output file: ' + Target ) ) ;
                exit ;
            end ;
            try
                blockwrite( Outfile, TOutput_Streamer( Output ).Data[ 0 ], Output.Size ) ;
            finally
                {$I-}
                closefile( Outfile ) ;
                {$I+}
                IOResult ;
            end ;
        end else
        begin
            Load_PSect := 0 ;

            // Write OBJ file...

            // Write Module name GSD...
            Start_Binary_Block ;
            Write_Block_Byte( 1 ) ; // GSD
            Write_Block_Byte( 0 ) ;
            Dummy := Rad50( Name ) ;
            Write_Block_Longword( Dummy ) ;
            Write_Block_Longword( 0 ) ;

            if( Ident <> 0 ) then
            begin
                Write_Block_Longword( Ident ) ;
                Write_Block_Byte( 0 ) ;
                Write_Block_Byte( 6 ) ; // Ident record
            end ;
            Finish_Binary_Block ;

            if( Ext.List.Count = 0 ) then // No psects encounterd
            begin
                PSect := TPSect.Create ;
                Ext.List.Add( PSect ) ;
            end ;

            // Write code section records...
            for Dummy := 0 to Ext.List.Count - 1 do
            begin
                PSect := TPSect( Ext.List[ Dummy ] ) ;
                PName := Rad50( PSect.Name ) ;
                Start_Binary_Block ;
                Write_Block_Byte( 1 ) ; // GSD
                Write_Block_Byte( 0 ) ;
                Write_Block_Longword( PName ) ;
                Size := 0 ;
                if( PSect.Overlay ) then
                begin
                    Size := 4 ;
                end ;
                if( PSect.Read_Only ) then
                begin
                    Size := Size or 16 ;
                end ;
                if( PSect.Relocatable ) then
                begin
                    Size := Size or 32 ;
                end ;
                if( PSect.Global ) then
                begin
                    Size := Size or 64 ;
                end ;
                if( not PSect.I_Space ) then
                begin
                    Size := Size or 128 ;
                end ;
                Write_Block_Byte( Size ) ; // Flags
                Write_Block_Byte( 5 ) ; // PSect record

                if( Ext.List.Count - 1 = Dummy ) then // This is the last PSect
                begin
                    Size := Output.Size - PSect.Address + 1 ;
                end else
                begin
                    Size := TPSect( Ext.List[ Dummy + 1 ] ).Address - PSect.Address ;
                end ;
                Write_Block_Word( Size ) ;
                Finish_Binary_Block ;
                if(
                    ( PSect.Address <= CPU.CPU.Get_Current_Address( 0, True ) )
                    and
                    ( CPU.CPU.Get_Current_Address( 0, True ) < PSect.Address + Size )
                  ) then
                begin
                    Load_PSect := Rad50( PSect.Name ) ;
                    Load_Offset := CPU.CPU.Get_Current_Address( 0, True ) - PSect.Address ;
                end ;

                // Write RLD...
                Start_Binary_Block ;
                Write_Block_Byte( 4 ) ; // RLD
                Write_Block_Byte( 0 ) ;
                Write_Block_Byte( 7 ) ; // Location counter definition
                Write_Block_Byte( 0 ) ;
                Write_Block_Longword( PName ) ;
                Write_Block_Word( PSect.Address ) ;
                Finish_Binary_Block ;

                // Write TXT...
                Start_Binary_Block ;
                Write_Block_Byte( 3 ) ; // TXT
                Write_Block_Byte( 0 ) ;
                Write_Block_Word( PSect.Address ) ;
                Write_Block_Data( PSect.Address, Size ) ;
                Finish_Binary_Block ;

                // Write global and global ref symbols for this psect in a RLD here
                Start_Binary_Block ;
                Write_Block_Byte( 4 ) ; // RLD
                Write_Block_Byte( 0 ) ;

                Assembler_Context := Master_Assembler.Assembler_Context ;
                for Loop := 0 to PSect.Externals.Count - 1 do
                begin
                    Write_Block_Byte( 15 ) ; // Complex
                    Write_Block_Byte( 0 ) ;
                    Write_Block_Longword( PName ) ;
                    S := Build_Complex_Relocation( PSect.Externals[ Loop ] ) ;
                    Start_Binary_Block ;
                    for Loop1 := 1 to length( S ) do
                    begin
                        Write_Block_Byte( ord( S[ Loop1 ] ) ) ;
                    end ;
                end ;
                Finish_Binary_Block ;
            end ; // for Dummy := 0 to Ext.List.Count - 1

            if( Load_PSect <> 0 ) then
            begin
                Start_Binary_Block ;
                Write_Block_Byte( 3 ) ; // Load address
                Write_Block_Byte( 0 ) ;
                Write_Block_Longword( Load_PSect ) ;
                Write_Block_Word( Load_Offset ) ;
                Finish_Binary_Block ;
            end ;

            Start_Binary_Block ;
            Write_Block_Byte( 2 ) ; // GSD end
            Write_Block_Byte( 0 ) ;
            Finish_Binary_Block ;

            // Write limit RLD...
            if( Have_Limit ) then
            begin
                Start_Binary_Block ;
                Write_Block_Byte( 4 ) ; // RLD
                Write_Block_Byte( 0 ) ;
                Write_Block_Byte( 11 ) ; // Limit
                Write_Block_Byte( Limit ) ;
                Finish_Binary_Block ;
            end ;

            Start_Binary_Block ;
            Write_Block_Byte( 6 ) ; // MOD end
            Write_Block_Byte( 0 ) ;
            Finish_Binary_Block ;

            // Write file...
            assignfile( Outfile, Target ) ;
            {$I-}
            rewrite( Outfile, 1 ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                ShowMessage( PChar( 'Cannot create output file: ' + Target ) ) ;
                exit ;
            end ;
            blockwrite( Outfile, TOutput_Streamer( Output ).Data, Output.Size ) ;
            {$I-}
            closefile( OutFile ) ;
            {$I+}
            IOResult ;
        end ; // if( Sav_File )
        Output.Free ;
    end ; // if( Output <> nil )
    Ext.Free ;
end ; // Assemble


end.

