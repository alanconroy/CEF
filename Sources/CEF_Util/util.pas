unit Util ;

{
        Program Name : CEF_Util
        Package Name : CEF
        Purpose      : Utility routines for CEF32
        Institution  : Conroy & Conroy Co.
        Date Written : 13-July-2005
        Written By   : Alan Conroy
        Version      : 1.0

	Copyright (C) 2005-2014 by Alan Conroy.  Released to the public domain.

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

          CEF32 and components support.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

interface

uses // Borland...
     Classes, // TList

     // C&C...
     _UE, // TUnified_Exception
     Standard, // TInteger64_List
     Symbols, // TSymbol_Table

     // CEF...
     _CEFUtil, // TCEF_Watchpoint
     _CEF, // Interface_Version
     CEF ; // TBase_CEF_Assembler_Context


type TStandard_Watchpoint = class( TCEF_Watchpoint )
                                private // Instance data...
                                    _Address : int64 ;
                                    _Access : longint ;
                                    _Size : longint ;
                                    _Context : pointer ;
                                    _Domain : string ;
                                    Temp_Serialize : string ;

                                public { API... }
                                    function Get_Address : int64 ; override ;
                                    function Get_Access : longint ; override ;
                                    procedure Set_Access( Value : longint ) ;
                                        override ;
                                    function Get_Size : longint ; override ;

                                    function Serialize : PChar ; override ;
                                    procedure Deserialize( Image : PChar ) ;
                                        override ;
                                    function Get_Context : pointer ;
                                        override ;
                                    procedure Set_Context( Value : pointer ) ;
                                        override ;
                                    function Get_Domain : Pchar ;
                                        override ;
                                    procedure Set_Domain( Value : PChar ) ;
                                        override ;
                            end ;

    TStandard_Watchpoint_Manager = class( TCEF_Watchpoint_Manager )
                                       private // Instance data...
                                           _Watchpoints : TList ;
                                           _Has_Accesses : boolean ;
                                           _Has_Sizes : boolean ;
                                           Temp_Serialize : string ;

                                       public // API...
                                           procedure Clear ; override ;
                                           function Clear_Watchpoint( Address : int64 ;
                                               Access : longint ) : TUnified_Exception ;
                                               override ;
                                           function Create_Watchpoint( Ad : int64 ;
                                               Ac : longint ) : TCEF_Watchpoint ;
                                               override ;
                                           function Count : longint ;
                                               override ;
                                           procedure Deserialize( Source : PChar ) ;
                                               override ;
                                           function Serialize : PChar ;
                                               override ;
                                           procedure Terminate ; override ;
                                           function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;
                                               override ;
                                           function Version : integer ;
                                               override ;
                                           procedure Initialize ; override ;
                                           function Has_Accesses : boolean ;
                                               override ;
                                           function Has_Sizes : boolean ;
                                               override ;
                                           function Create_Watchpoint( Ad : int64 ;
                                               Siz, Ac : longint ) : TCEF_Watchpoint ;
                                               override ;
                                           function Create_Watchpoint( Ad : int64 ) : TCEF_Watchpoint ;
                                               override ;
                                           function Watchpoint_At_Index( Index : integer ) : TCEF_Watchpoint ;
                                               override ;
                                           function Add( W : TCEF_Watchpoint ) : integer ;
                                               override ;
                                           function Clear_Watchpoint_Ex( Address : int64 ;
                                               Access : longint ; Context : pointer ) : TUnified_Exception ;
                                               override ;
                                           function Create_Watchpoint_Ex( Ad : int64 ;
                                               Ac : longint ; Context : pointer ;
                                               Domain : PChar ) : TCEF_Watchpoint ; overload ;
                                               override ;
                                           function Create_Watchpoint_Ex( Ad : int64 ;
                                               Siz, Ac : longint ;
                                               Context : pointer ; Domain : PChar ) : TCEF_Watchpoint ; overload ;
                                               override ;
                                           function Watchpoint_At_Ex( Address : int64 ; Context : pointer ) : TCEF_Watchpoint ;
                                               override ;
                                   end ; // TStandard_Watchpoint_Manager

     TKey_Mapper = class( TCEF_Key_Mapper )
                       public // Constructor
                           constructor Create ;

                       private // Instance data..
                           Keys : TStringList ; // WIndows keys that can be mapped
                           _Mapping : TStringList ; // Current mappings for Keys
                           Mappings : TStringList ; // Valid mappings
                           Temp_Mapping : string ;

                       public // API...
                           procedure Add_Key( Name : PChar ) ; override ;
                           procedure Add_Mapping( Name : PChar ) ; override ;
                           procedure Clear_Keys( Mapping_Only : boolean ) ;
                               override ;
                           procedure Clear_Mappings ; override ;
                           function Query : boolean ; override ;
                           function Mapping( Name : PChar ) : PChar ; override ;
                           procedure Set_Mapping( Key, Mapping : PChar ) ;
                               override ;
                           procedure Terminate ; override ;
                   end ;

type TCharacter_Set = class( TCEF_Character_Set )
                          private // Instance data...
                              Character_Set : TList ;//~~~array[ 0..255 ] of TBitmap ;
                              _Invert : boolean ;
                              _Height : integer ;
                              _Width : integer ;

                          public // Property handlers...
                              function Get_Invert : boolean ; override ;

                              procedure Set_Invert( Value : boolean ) ; override ;

                          public // API...
                              procedure Clear ; override ;
                              function Draw( Handle, X, Y, Index : longint ) : boolean ;
                                  override ;
                              function Has_Glyph( Index : integer ) : boolean ;
                                  override ;
                              function Height : integer ; override ;
                              procedure Initialize ; override ;
                              procedure Load( Name : PChar ) ; override ;
                              procedure Terminate ; override ;
                              function Width( Index : longint ) : longint ; override ;
                              function DrawEx( Handle, X, Y, Index, Flags : longint ) : boolean ;
                                  override ;
                      end ; // TCharacter_Set

type TMapping_Info = class
                         public
                             File_Index : integer ;
                             Line : integer ;
                     end ;

type TAssembler_Context = class( TBase_CEF_Assembler_Context )
                              public // Constructors and destructors...
                                  constructor Create ;
                                  destructor Destroy ; override ;

                              private // Instance data...
                                  Filenames : TStringList ;
                                  Addresses : Tinteger64_List ;
                                  Mappings : TList ;
                                  Symbol_Table : TSymbol_Table ;
                                  Temp_Filename : string ;
                                  Temp_Find_Next : string ;
                                  
                              private // Internal utility routines...
                                  function _Add_Mapping( Filename : string ;
                                      Address : int64 ;
                                      Line : integer ) : integer ;

                                  { Returns filename and line number
                                     corresponding to the passed address.  If
                                     there is no mapping, it returns -1. }
                                  function _Mapping( Address : int64 ;
                                      var Filename : string ) : integer ;

                                  { Get value associated with passed symbol.  If
                                    symbol is unknown, it returns False. }
                                  function _Symbol_Value( const Name : string ;
                                      var Value : int64 ) : boolean ;

                                  { Get size associated with passed symbol.  If
                                    symbol is unknown, it returns False. }
                                  function _Symbol_Size( const Name : string ;
                                      var Value : integer ) : boolean ;

                              public // API...
                                  function Add_Mapping( Filename : PChar ;
                                      Address : int64 ;
                                      Line : integer ) : integer ; override ;

                                  function Add_Symbol( Name : PChar ;
                                      P : pSymbol_Record ) : TUnified_Exception ; override ;

                                  procedure Delete( Sym : PChar ) ; override ;

                                  procedure Delete_Mapping( Index : integer ) ;
                                      override ;

                                  function Find( Sym : PChar ; var Addr : int64 ;
                                      var Flg, D_T, Siz : longint ;
                                      var Dat : pointer ) : integer ; override ;

                                  { Returns filename and line number
                                     corresponding to the passed address.  If
                                     there is no mapping, it returns -1. }
                                  function Mapping( Address : int64 ;
                                      var Filename : PChar ) : integer ;
                                      override ;

                                  procedure Pop_Level ; override ;

                                  procedure Push_Level ; override ;

                                  { Get value associated with passed symbol.  If
                                    symbol is unknown, it returns False. }
                                  function Symbol_Value( Name : PChar ;
                                      var Value : int64 ) : boolean ; override ;

                                  { Get size associated with passed symbol.  If
                                    symbol is unknown, it returns False. }
                                  function Symbol_Size( Name : PChar ;
                                      var Value : integer ) : boolean ;
                                      override ;

                                  function Get_Case_Sensitive : boolean ;
                                      override ;

                                  procedure Set_Case_Sensitive( Value : boolean ) ;
                                      override ;

                                  function Find_First( var Sym : PChar ;
                                      var Addr : int64 ;
                                      var Flg, D_T, Siz : longint ;
                                      var Dat : pointer ) : integer ; override ;

                                  function Find_Next( var Sym : PChar ;
                                      var Addr : int64 ;
                                      var Flg, D_T, Siz : longint ;
                                      var Dat : pointer ) : integer ; override ;
                          end ; // TAssembler_Context


procedure Add_Breakpoint( UI : TUI_Interface ; Component : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Base, Size : longint ;
    Port : boolean ) ;
procedure Add_Watchpoint( UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ; Context : pointer ;
    Domain : Pchar ) ;
procedure Show_Watchpoints( UI : TUI_Interface ; CPU : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Ports : boolean ) ;
function Create_Logger( UI : TUI_Interface ; Filename : string ;
    Flags : longint ) : TCEF_Logger ;
function Translate_Serial_Data( Source : string ;
    Source_Speed, Target_Speed : int64 ; Data_Bits, Stop_Bits : longint ) : string ;

implementation

uses // Borland...
     Windows, // HPalette
     Controls, // TControl
     Extctrls, // TPanel
     Forms, // Application
     Graphics, // TBitmap
     StdCtrls, // TComboBox
     SysUtils, // inttostr

     // C&C...
     _Streams, // TCOM_Stream
     CommonUt, // Edit
     CVT, // CVTB
     EBCDICs, //
     HTML, // TXML_Parser
     Mem_Watchpoints_Dialog, // Add_Breakpoint
     Radix50s,
     UE, // DOS_ERT
     UStrings, // Print_Edit

     // CEF...
     Keymap_Form ; // TKey_Mapper_Dialog

type TString_Streamer = class( TCOM_Stream )
                          private // Instance data...
                              _Data : string ;

                          public // API...
                              function Is_Class( _N : PChar ) : boolean ; override ;

                              function At_End : boolean ; override ; stdcall ;

                              function Facility_ID : longint ;
                                  override ; stdcall ;

                              function Last_Error : TUnified_Exception ;
                                  override ;

                              function Read( var Buffer ; _Size : longint ) : longint ;
                                  override ; stdcall ;

                              procedure Read_Line( var Buffer ; var Size : longint ) ;
                                  override ; stdcall ;

                              procedure Seek( Position : longint ) ;
                                  override ; stdcall ;

                              function Size : longint ;
                                  override ;

                              procedure Write( var Buffer ; size : longint ) ;
                                  override ; stdcall ;

                              procedure Write_Line( Buffer : PChar ) ;
                                  override ; stdcall ;
                      end ;

// API...

function TString_Streamer.Is_Class( _N : PChar ) : boolean ;

var N : string ;

begin
    N := lowercase( string( _N ) ) ;
    Result := ( N = 'tstring_streamer' ) ;
end ;


function TString_Streamer.At_End : boolean ;

begin
    Result := False ;
end ;


function TString_Streamer.Facility_ID : longint ;

begin
    Result := 128 ;
end ;


function TString_Streamer.Last_Error : TUnified_Exception ;

begin
    Result := nil ;
end ;


function TString_Streamer.Read( var Buffer ; _Size : longint ) : longint ;

begin
    Read_Line( Buffer, _Size ) ;
    Result := _Size ;
end ;


procedure TString_Streamer.Read_Line( var Buffer ; var Size : longint ) ;

begin
    if( Size < 1 ) then
    begin
        exit ;
    end ;
    if( Size > length( _Data ) ) then
    begin
        Size := length( _Data ) ;
    end ;
    move( PChar( _Data )[ 0 ], Buffer, Size ) ;
end ;


procedure TString_Streamer.Seek( Position : longint ) ;

begin
    // Does nothing...
end ;


function TString_Streamer.Size : longint ;

begin
    Result := length( _Data ) ;
end ;


procedure TString_Streamer.Write( var Buffer ; size : longint ) ;

var L : integer ;

begin
    if( Size < 1 ) then
    begin
        exit ;
    end ;
    L := length( _Data ) ;
    setlength( _Data, L + Size ) ;
    move( Buffer, PChar( _Data )[ L ], Size ) ;
end ;


procedure TString_Streamer.Write_Line( Buffer : PChar ) ;

begin
    _Data := _Data + Buffer ;
end ;



type TStandard_Logger = class( TCEF_Logger )
                           private // Instance data...
                               _Options : longint ;
                               _Data_Radix : longint ;
                               _Paused : boolean ;
                               _Filename : string ;
                               _UI : TUI_Interface ;
                               F : textfile ;
                               _Reference_Count : longint ;
                               _Wrap : longint ;

                           public // API...
                               procedure Attach ; override ;
                               procedure Detach ; override ;
                               function Get_Data_Radix : longint ; override ;
                               function Get_Options : longint ; override ;
                               function Get_Paused : boolean ; override ;
                               function Get_Wrap_Column : longint ; override ;
                               procedure Set_Data_Radix( Value : longint ) ;
                                   override ;
                               procedure Set_Options( Value : longint ) ;
                                   override ;
                               procedure Set_Paused( Value : boolean ) ;
                                   override ;
                               procedure Set_Wrap_Column( Value : longint ) ;
                                   override ;

                               { Set UI component for the logger (for retrieving
                                 clock times, etc. }
                               procedure Set_UI( Value : TUI_Interface ) ;
                                   override ;

                               // Returns name of current log file...
                               function Filename : PChar ; override ;

                               { Open the specified file as the log file.  If Append
                                 is true, the existing file is appended to.  If the
                                 file doesn't exist it is created.  If Append is
                                 false, any existing file is overwritten.  Returns
                                 True if the file was opened and false
                                 otherwise. }
                               function Open( Filename : PChar ; Append : boolean ) : boolean ;
                                   override ;

                               { Terminate the logger instance.  This should be
                                 done via the Deatch call. }
                               procedure Terminate ; override ;

                               { Log an item to the log file.  S is the data to log.
                                 Len is the length of S.  If Len is -1, S is assumed
                                 to be null-terminated.  Continuation is false if
                                 this is the first item of a log operation.  If
                                 continuation is true, this is considered to be part
                                 of the preceeding log item.  Data is true if S is
                                 data that should be displayed according to the
                                 Data_Radix value. }
                               procedure Log( C : TComponent ;
                                   S : PChar ; Len : longint ;
                                   Continuation : boolean ; Log_Type : longint ) ;
                                   override ;

                               procedure Update( C : TComponent ;
                                   PC, I : int64 ) ; override ;
                       end ;


function Create_Logger( UI : TUI_Interface ; Filename : string ;
    Flags : longint ) : TCEF_Logger ;

begin
    Result := TStandard_Logger.Create ;
    Result.Set_UI( UI ) ;
    TStandard_Logger( Result )._Options := Flags ;
    if( Filename <> '' ) then
    begin
        if( not Result.Open( PChar( Filename ), ( Flags and LO_Append ) <> 0 ) ) then
        begin
            Result.Terminate ;
            Result := nil ;
        end ;
    end ;
end ;

{ This function handles data mangling for mismatched data rates.  The receiver
  is assumed to work like a UART, which samples data on each clock to determine
  the next bit. }
function Translate_Serial_Data( Source : string ;
    Source_Speed, Target_Speed : int64 ; Data_Bits, Stop_Bits : longint ) : string ;

var Bits : integer ;
    Accumulator : integer ;

    procedure Write_Bit( B : integer ) ;

    begin
        if( B <> 0 ) then
        begin
            Accumulator := Accumulator or ( 1 shl Bits ) ;
        end ;
        inc( Bits ) ;
        if( Bits >= Data_Bits ) then
        begin
            Bits := 0 ;
            Result := Result + chr( Accumulator ) ;
            Accumulator := 0 ;
        end ;
    end ;

var C : integer ;
    Bit, Dummy, Loop, Ratio : longint ;
    This_Bit : integer ;

begin
    if( ( Source_Speed = Target_Speed ) or ( Source_Speed = 0 ) or ( Target_Speed = 0 ) ) then
    begin
        Result := Source ; // Speeds match
        exit ;
    end ;

    Result := '' ;
    Bits := 0 ;
    Accumulator := 0 ;
    if( Source_Speed < Target_Speed ) then // Must expand source to match target
    begin
        Ratio := Target_Speed div Source_Speed ;
        for Loop := 1 to length( Source ) do
        begin
            C := ord( Source[ Loop ] ) ;
            for Bit := 0 to Data_Bits - 1 do
            begin
                This_Bit := C and ( 1 shl Bit ) ;
                for Dummy := 1 to Ratio do
                begin
                    Write_Bit( This_Bit ) ;
                end ;
            end ;
            for Bit := 1 to Stop_Bits do
            begin
                for Dummy := 1 to Ratio do
                begin
                    Write_Bit( 1 ) ;
                end ;
            end ;
        end ;
        if( Bits > 0 ) then
        begin
            Result := Result + chr( Accumulator ) ;
        end ;
        exit ;
    end ;

    // Must compress source to match target...
    Ratio := Source_Speed div Target_Speed ;
    Bit := Ratio div 2 ;
    Loop := 1 ;
    while( Loop <= length( Source ) ) do
    begin
        if( Bit < Data_Bits ) then
        begin
            C := ord( Source[ Loop ] ) ;
            Write_Bit( C and ( 1 shl Bit ) ) ;
            Bit := Bit + Ratio ; // Source bits until the next sample bit
        end else
        begin
            inc( Loop ) ;
            Bit := Bit - Data_Bits - Stop_Bits ;
            if( Bit < 0 ) then // Next sample was in the stop bits
            begin
                Write_Bit( 1 ) ;
                Bit := Bit + Ratio ;
            end ;
        end ;
    end ;
    if( Bits > 0 ) then
    begin
        Result := Result + chr( Accumulator ) ;
    end ;
end ; // Translate_Serial_Data



// TStandard_Logger methods...

// API...

procedure TStandard_Logger.Attach ;

begin
    inc( _Reference_Count ) ;
end ;


procedure TStandard_Logger.Detach ;

begin
    dec( _Reference_Count ) ;
    if( _Reference_Count <= 0 ) then
    begin
        Terminate ;
    end ;
end ;


function TStandard_Logger.Get_Data_Radix : longint ;

begin
    Result := Data_Radix ;
end ;


function TStandard_Logger.Get_Options : longint ;

begin
    Result := _Options ;
end ;


function TStandard_Logger.Get_Paused : boolean ;

begin
    Result := _Paused ;
end ;


procedure TStandard_Logger.Set_Data_Radix( Value : longint ) ;

begin
    _Data_Radix := Value ;
end ;


procedure TStandard_Logger.Set_Options( Value : longint ) ;

begin
    _Options := Value ;
end ;


procedure TStandard_Logger.Set_Paused( Value : boolean ) ;

begin
    _Paused := Value ;
end ;


procedure TStandard_Logger.Set_UI( Value : TUI_Interface ) ;

begin
    _UI := Value ;
end ;


function TStandard_Logger.Get_Wrap_Column : longint ;

begin
    Result := _Wrap ;
end ;


procedure TStandard_Logger.Set_Wrap_Column( Value : longint ) ;

begin
    _Wrap := Value ;
end ;


function TStandard_Logger.Filename : PChar ;

begin
    Result := PChar( _Filename ) ;
end ;


function TStandard_Logger.Open( Filename : PChar ; Append : boolean ) : boolean ;

begin
    if( _Filename <> '' ) then
    begin
        closefile( F ) ;
    end ;
    assignfile( F, Filename ) ;
    {$I-}
    if( Append and FileExists( Filename ) ) then
    begin
        System.Append( F ) ;
    end else
    begin
        rewrite( F ) ;
    end ;
    {$I+}
    if( IOResult <> 0 ) then
    begin
        Result := False ;
        exit ;
    end ;
    _Filename := Filename ;
    _Paused := False ;
    Result := True ;
end ;


procedure TStandard_Logger.Terminate ;

begin
    if( _Filename <> '' ) then
    begin
        closefile( F ) ;
    end ;
    Free ;
end ;


procedure TStandard_Logger.Log( C : TComponent ; S : PChar ; Len : longint ;
   Continuation : boolean ; Log_Type : longint ) ;

var Loop : integer ;
    Work, Temp : string ;

begin
    if( Paused ) then
    begin
        exit ;
    end ;
    if( Len = -1 ) then
    begin
        Work := string( S ) ;
    end else
    begin
        setlength( Work, Len ) ;
        move( S, PChar( Work )[ 0 ], Len ) ;
    end ;
    case Log_Type of
        LT_Data :
                begin
                    if( ( _Options and LO_Hide_Data ) <> 0 ) then
                    begin
                        exit ;
                    end ;
                    case Data_Radix of
                        1: ; // Don't modify the data
                        2-36 : // Base
                             begin
                                 Temp := '' ;
                                 for Loop := 1 to length( Work ) do
                                 begin
                                     Temp := Temp + cvtb( 10, Data_Radix, inttostr( ord( Work[ Loop ] ) ) ) + ' ' ;
                                 end ;
                                 Work := Temp ;
                             end ;
                        50 : // Radix-50
                             Work := ASCII_To_RAD( Work ) ;
                        51 : // EBCDIC
                             Work := From_EBCDIC( Work ) ;
                        else // Print-friendly ASCII string (control codes replaced with ".")
                            Work := Print_Edit( Work ) ;
                    end ;
                    Work := 'Length=' + inttostr( length( Work ) ) + '=' + Work ;
                end ;
        LT_Sent_Signal : begin
                             if( ( _Options and LO_Hide_Sent_Signal ) <> 0 ) then
                             begin
                                 exit ;
                             end ;
                             Work := '*Send: ' + Work ;
                         end ;
        LT_Received_Signal : begin
                                 if( ( _Options and LO_Hide_Received_Signal ) <> 0 ) then
                                 begin
                                     exit ;
                                 end ;
                                 Work := '*Received: ' + Work ;
                             end ;
        LT_State_Change : begin
                              if( ( _Options and LO_Hide_State_Change ) <> 0 ) then
                              begin
                                  exit ;
                              end ;
                              Work := '!' + Work ;
                          end ;
        LT_Read : begin
                      if( ( _Options and LO_Hide_Read ) <> 0 ) then
                      begin
                          exit ;
                      end ;
                      Work := '<Read: ' + Work ;
                  end ;
        LT_Write : begin
                       if( ( _Options and LO_Hide_Write ) <> 0 ) then
                       begin
                           exit ;
                       end ;
                       Work := '>Write: ' + Work ;
                   end ;
        LT_Input : begin
                       if( ( _Options and LO_Hide_Input ) <> 0 ) then
                       begin
                           exit ;
                       end ;
                       Work := '<Input: ' + Work ;
                   end ;
        LT_Output : begin
                       if( ( _Options and LO_Hide_Output ) <> 0 ) then
                       begin
                           exit ;
                       end ;
                        Work := '>Output: ' + Work ;
                    end ;
        LT_Other : begin
                       if( ( _Options and LO_Hide_Other ) <> 0 ) then
                       begin
                           exit ;
                       end ;
                       Work := '|' + Work ;
                   end ;
        // LT_Execution and any unknowns are not prefixed
        LT_Execution : if( ( _Options and LO_Hide_Execution ) <> 0 ) then
                       begin
                           exit ;
                       end ;
        else ;
    end ; // case Log_Type of
    if( not Continuation ) then
    begin
        if( C <> nil ) then
        begin
            Work := '[' + C.Name + '] ' + Work ;
        end ;
        if( ( _Options and LO_Include_TimeStamp ) <> 0 ) then
        begin
            if( _UI <> nil ) then
            begin
                if( _UI.Clock <> nil ) then
                begin
                    Work := inttostr( _UI.Clock.Get_Time_Index ) + ': ' + Work ;
                end ;
            end ;
        end ;
    end ;
    {$I-}
    writeln( F, Work ) ;
    {$I+}
    Loop := IOResult ;
    if( Loop <> 0 ) then
    begin
        if( Messagebox( 0, PChar( 'Error writing log file: ' + DOS_ERT( Loop ) +
            '. Suspend logging?' ), 'CEF Logger', mb_YesNo ) = idYes ) then
        begin
            Paused := True ;
        end ;
    end ;
end ; // TStandard_Logger.Log


procedure TStandard_Logger.Update( C : TComponent ; PC, I : int64 ) ;

var B, Loop, Temp : integer ;
    S, Work : string ;
    z : TString_Streamer ;

begin
    if( C.CPU <> nil ) then // Only applies to CPUs
    begin
        B := _Data_Radix ;
        if( ( B < 2 ) or ( B > 36 ) ) then
        begin
            B := 10 ; // Default to decimal
        end ;
        S := inttostr( PC ) + ' ' + cvtb( 10, B, inttostr( I ) ) ;
        Loop := 0 ;
        while( C.CPU.Register_Size( Loop ) <> 0 ) do
        begin
            Temp := C.CPU.Register_Size( Loop ) ;
            C.Examine( Loop, Temp, @I, True ) ;
            S := S + ' ' + C.CPU.Register_Name( Loop ) + '=' +
                cvtb( 10, B, inttostr( I ) ) ;
            inc( Loop ) ;
        end ;
        Z := TString_Streamer.Create ;
        C.CPU.Disassemble( PC, B, 1, Z ) ;
        Loop := Z.Size ;
        setlength( Work, Loop ) ;
        Z.Read( PChar( Work )[ 0 ], Loop ) ;
        S := S + '  ' + Work ;
        Z.Free ;
        Log( C, PChar( Temp ), -1, False, LT_Execution ) ;
    end ;
end ;



// TStandard_Watchpoint methods...

// API...

function TStandard_Watchpoint.Get_Address : int64 ;

begin
    Result := _Address ;
end ;


function TStandard_Watchpoint.Get_Access : longint ;

begin
    Result := _Access ;
end ;


procedure TStandard_Watchpoint.Set_Access( Value : longint ) ;

begin
    _Access := Value ;
end ;


function TStandard_Watchpoint.Get_Size : longint ;

begin
    Result := _Size ;
end ;


function TStandard_Watchpoint.Serialize : PChar ;

begin
    Temp_Serialize := '|' + inttostr( Address ) + '|' + inttostr( Access ) + '|' + inttostr( Size ) ;
    Result := PChar( Temp_Serialize ) ;
end ;


procedure TStandard_Watchpoint.Deserialize( Image : PChar ) ;

var Dummy : integer ;
    S : string ;

begin
    S := string( Image ) ;
    S := copy( S, 2, length( S ) ) ;
    Dummy := pos( '|', S ) ;
    _Address := strtoint64( copy( S, 1, Dummy - 1 ) ) ;
    S := copy( S, Dummy + 1, length( S ) ) ;
    Dummy := pos( '|', S + '|' ) ;
    Access := strtoint64( copy( S, 1, Dummy - 1 ) ) ;
    if( Dummy < length( S ) ) then
    begin
        _Size := strtoint( copy( S, Dummy + 1, length( S ) ) ) ;
    end ;
end ;


function TStandard_Watchpoint.Get_Context : pointer ;

begin
    Result := _Context ;
end ;


procedure TStandard_Watchpoint.Set_Context( Value : pointer ) ;

begin
    _Context := Value ;
end ;


function TStandard_Watchpoint.Get_Domain : Pchar ;

begin
    Result := PChar( _Domain ) ;
end ;


procedure TStandard_Watchpoint.Set_Domain( Value : PChar ) ;

begin
    _Domain := string( Value ) ;
end ;



// TStandard_Watchpoint_Manager methods...

// API...

procedure TStandard_Watchpoint_Manager.Clear ;

var Loop : integer ;

begin
    if( _Watchpoints = nil ) then
    begin
        exit ;
    end ;
    for Loop := Count - 1 downto 0 do
    begin
        Watchpoints[ Loop ].Free ;
    end ;
    _Watchpoints.Clear ;
end ;


function TStandard_Watchpoint_Manager.Clear_Watchpoint( Address : int64 ;
    Access : longint ) : TUnified_Exception ;

begin
    Result :=  Clear_Watchpoint_Ex( Address, Access, nil ) ;
end ;


function TStandard_Watchpoint_Manager.Clear_Watchpoint_Ex( Address : int64 ;
   Access : longint ; Context : pointer ) : TUnified_Exception ;

var Index : integer ;
    Watchpoint : TStandard_Watchpoint ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    for Index := 0 to Count - 1 do
    begin
        Watchpoint := TStandard_Watchpoint( Watchpoints[ Index ] ) ;
        if( ( Watchpoint.Address = Address ) and ( Watchpoint.Context = Context ) ) then
        begin
            Watchpoint.Access := Watchpoint.Access and not( Access ) ;
            if( Watchpoint.Access = 0 ) then // No access class remaining
            begin
                Watchpoint.Free ;
                _Watchpoints.Delete( Index ) ;
            end ;
            exit ;
        end ;
    end ;
end ;


function TStandard_Watchpoint_Manager.Create_Watchpoint( Ad : int64 ) : TCEF_Watchpoint ;

var Index : integer ;
    Watchpoint : TStandard_Watchpoint ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    for Index := 0 to Count - 1 do
    begin
        Watchpoint := TStandard_Watchpoint( Watchpoints[ Index ] ) ;
        if( Watchpoint.Address = Ad ) then
        begin
            exit ;
        end ;
    end ;
    Result := TStandard_Watchpoint.Create ;
    TStandard_Watchpoint( Result )._Address := Ad ;
    if( _Watchpoints = nil ) then
    begin
        _Watchpoints := TList.Create ;
    end ;
    _Watchpoints.Add( Result ) ;
end ;


function TStandard_Watchpoint_Manager.Create_Watchpoint( Ad : int64 ;
    Ac : longint ) : TCEF_Watchpoint ;

begin
    Result := Create_Watchpoint_Ex( Ad, Ac, nil, nil ) ;
end ;


function TStandard_Watchpoint_Manager.Create_Watchpoint_Ex( Ad : int64 ;
   Ac : longint ; Context : pointer ; Domain : PChar ) : TCEF_Watchpoint ;

var Index : integer ;
    Watchpoint : TStandard_Watchpoint ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    if( Ac = 0 ) then // No effect
    begin
        exit ;
    end ;
    _Has_Accesses := True ;
    for Index := 0 to Count - 1 do
    begin
        Watchpoint := TStandard_Watchpoint( Watchpoints[ Index ] ) ;
        if( Watchpoint.Address = Ad ) then
        begin
            Watchpoint.Access := Watchpoint.Access or Ac ;
            exit ;
        end ;
    end ;
    Result := TStandard_Watchpoint.Create ;
    TStandard_Watchpoint( Result )._Address := Ad ;
    TStandard_Watchpoint( Result )._Context := Context ;
    TStandard_Watchpoint( Result )._Domain := string( Domain ) ;
    Result.Access := Ac ;
    if( _Watchpoints = nil ) then
    begin
        _Watchpoints := TList.Create ;
    end ;
    _Watchpoints.Add( Result ) ;
end ;


function TStandard_Watchpoint_Manager.Count : longint ;

begin
    if( _Watchpoints = nil ) then
    begin
        Result := 0 ;
    end else
    begin
        Result := _Watchpoints.Count ;
    end ;
end ;


procedure TStandard_Watchpoint_Manager.Deserialize( Source : PChar ) ;

var S : string ;
    Watchpoint : TStandard_Watchpoint ;
    Watchpoint_Parser : TXML_Parser ;

begin
    _Has_Sizes := False ;
    _Has_Accesses := False ;
    Watchpoint_Parser := TXML_Parser.Create ;
    try
        Watchpoint_Parser.Set_Source( Source ) ;
        while( not Watchpoint_Parser.EOF ) do
        begin
            S := uppercase( Watchpoint_Parser.Token ) ;
            if( S = '<WATCHPOINT>' ) then
            begin
                Watchpoint := TStandard_Watchpoint.Create ;
                Watchpoint.Deserialize( Watchpoint_Parser.Get_Section( 'watchpoint' ) ) ;
                Add( Watchpoint ) ;
            end else
            if( S = '<SIZES/>' ) then
            begin
                _Has_Sizes := True ;
            end else
            if( S = '<ACCESSES/>' ) then
            begin
                _Has_Accesses := True ;
            end else
            if( ( copy( S, 1, 1 ) = '<' ) and ( copy( S, length( S ) - 1, 2 ) <> '/>' ) ) then
            begin
                Watchpoint_Parser.Get_Section( PChar( S ) ) ;
            end ;
        end ;
    finally
        Watchpoint_Parser.Free ;
    end ;
end ;


function TStandard_Watchpoint_Manager.Serialize : PChar ;

var Loop : integer ;

begin
    Temp_Serialize := '' ;
    if( Has_Sizes ) then
    begin
        Temp_Serialize := '<sizes/>' ;
    end ;
    if( Has_Accesses ) then
    begin
        Temp_Serialize := Temp_Serialize + '<accesses/>' ;
    end ;
    for Loop := 0 to Count - 1 do
    begin
        Temp_Serialize := Temp_Serialize +
            '<watchpoint>' + TStandard_Watchpoint( Watchpoints[ Loop ] ).Serialize + '</watchpoint>' ;
    end ;
    Result := PChar( Temp_Serialize ) ;
end ;


procedure TStandard_Watchpoint_Manager.Terminate ;

begin
    Clear ;
    _Watchpoints.Free ;
    _Watchpoints := nil ;
    Free ;
end ;


function TStandard_Watchpoint_Manager.Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;

begin
    Result := Watchpoint_At_Ex( Address, nil ) ;
end ;


function TStandard_Watchpoint_Manager.Watchpoint_At_Ex( Address : int64 ;
    Context : pointer ) : TCEF_Watchpoint ;

var Loop : integer ;

begin
    for Loop := 0 to Count - 1 do
    begin
        Result := Watchpoints[ Loop ] ;
        if( ( Result.Address = Address ) and ( Result.Context = Context ) ) then
        begin
            exit ;
        end ;
    end ;
    Result := nil ; { None found }
end ;


function TStandard_Watchpoint_Manager.Version : integer ;

begin
    Result := Interface_Version ;
end ;


procedure TStandard_Watchpoint_Manager.Initialize ;

begin
    _Has_Accesses := False ;
    _Has_Sizes := False ;
end ;


function TStandard_Watchpoint_Manager.Has_Accesses : boolean ;

begin
    Result := _Has_Accesses ;
end ;


function TStandard_Watchpoint_Manager.Has_Sizes : boolean ;

begin
    Result := _Has_Sizes ;
end ;


function TStandard_Watchpoint_Manager.Create_Watchpoint( Ad : int64 ;
    Siz, Ac : longint ) : TCEF_Watchpoint ;

begin
    Result := Create_Watchpoint_Ex( Ad, Siz, Ac, nil, nil ) ;
end ;


function TStandard_Watchpoint_Manager.Create_Watchpoint_Ex( Ad : int64 ;
   Siz, Ac : longint ; Context : pointer ; Domain : PChar ) : TCEF_Watchpoint ;

var Index : integer ;
    Watchpoint : TStandard_Watchpoint ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    if( Ac = 0 ) then // No effect
    begin
        exit ;
    end ;
    _Has_Accesses := True ;
    _Has_Sizes := True ;
    for Index := 0 to Count - 1 do
    begin
        Watchpoint := TStandard_Watchpoint( Watchpoints[ Index ] ) ;
        if( Watchpoint.Address = Ad ) then
        begin
            Watchpoint.Access := Watchpoint.Access or Ac ;
            exit ;
        end ;
    end ;
    Result := TStandard_Watchpoint.Create ;
    TStandard_Watchpoint( Result )._Address := Ad ;
    TStandard_Watchpoint( Result )._Context := Context ;
    TStandard_Watchpoint( Result )._Domain := string( Domain ) ;
    Result.Access := Ac ;
    TStandard_Watchpoint( Result )._Size := Siz ;
    if( _Watchpoints = nil ) then
    begin
        _Watchpoints := TList.Create ;
    end ;
    _Watchpoints.Add( Result ) ;
end ;


function TStandard_Watchpoint_Manager.Watchpoint_At_Index( Index : integer ) : TCEF_Watchpoint ;

begin
    if( ( Index < 0 ) or ( Index >= Count ) ) then
    begin
        Result := nil ;
        exit ;
    end ;
    Result := TCEF_Watchpoint( _Watchpoints[ Index ] ) ;
end ;


function TStandard_Watchpoint_Manager.Add( W : TCEF_Watchpoint ) : integer ;

begin
    if( _Watchpoints = nil ) then
    begin
        _Watchpoints := TList.Create ;
    end ;
    _Watchpoints.Add( W ) ;
    Result := Count - 1 ;
end ;


// TCharacter_Set methdos...

// Property handlers...

function TCharacter_Set.Get_Invert : boolean ;

begin
    Result := _Invert ;
end ;


procedure TCharacter_Set.Set_Invert( Value : boolean ) ;

begin
    _Invert := Value ;
end ;


// API...

procedure TCharacter_Set.Clear ;

var I : integer ;

begin
    if( Character_Set = nil ) then
    begin
        Character_Set := TList.Create ;
        exit ;
    end ;
    for I := 0 to Character_Set.Count - 1 do
    begin
        TBitmap( Character_Set[ I ] ).Free ;
        Character_Set[ I ] := nil ;
    end ;
    Character_Set.Clear ;
end ;


function TCharacter_Set.Draw( Handle, X, Y, Index : integer ) : boolean ;

begin
    Result := DrawEx( Handle, X, Y, Index, 0 ) ;
end ;


function TCharacter_Set.DrawEx( Handle, X, Y, Index, Flags : integer ) : boolean ;

    procedure Draw_Bitmap( B : TBitmap ) ;

    var OldPalette : HPalette ;
        RestorePalette : Boolean ;

    begin
        OldPalette := 0 ;
        RestorePalette := False ;

        if( B.Palette <> 0 ) then
        begin
            OldPalette := SelectPalette( Handle, B.Palette, True ) ;
            RealizePalette( Handle ) ;
            RestorePalette := True ;
        end ;
        try
            if( ( Flags and 1 ) <> 0 ) then // Transparent draw
            begin
                BitBlt( Handle, X, Y, B.Width, B.Height,
                    B.Canvas.Handle, 0, 0, SRCAND ) ;
            end else
            begin
                BitBlt( Handle, X, Y, B.Width, B.Height,
                    B.Canvas.Handle, 0, 0, SRCCOPY ) ;
            end ;
        finally
            if( RestorePalette ) then
            begin
                SelectPalette( Handle, OldPalette, True ) ;
            end ;
        end ;
    end ; // TCharacter_Set.Draw.Draw_Bitmap

var B : TBitmap ;
    C : TColor ;
    LX, LY : integer ;

begin
    Result := False ;
    if( ( Index < 0 ) or ( Index >= Character_Set.Count ) ) then
    begin
        exit ;
    end ;
    if( ( Index > 127 ) and ( Character_Set[ Index ] = nil ) ) then
    begin
        Index := Index - 128 ;
    end ;
    if( Character_Set[ Index ] = nil ) then
    begin
        exit ;
    end ;

    Result := True ;
    if( _Invert ) then
    begin
        B := TBitmap.Create ;
        try
            B.Assign( Character_Set[ Index ] ) ;
            for LX := 0 to B.Width - 1 do
            begin
                for LY := 0 to B.Height - 1 do
                begin
                    C := B.Canvas.Pixels[ LX, LY ] ;
                    C := C and $FF000000 or ( ( not C ) and $FFFFFF ) ;
                    B.Canvas.Pixels[ LX, LY ] := C ;
                end ;
            end ;
            Draw_Bitmap( B ) ;
        finally
            B.Free ;
        end ;
    end else
    begin
        Draw_Bitmap( Character_Set[ Index ] ) ;
    end ;
end ; // TCharacter_Set.DrawEx


function TCharacter_Set.Has_Glyph( Index : integer ) : boolean ;

begin
    Result := False ;
    if(
        ( Index >= 0 )
        and
        ( Index <= Character_Set.Count )
        and
        ( Character_Set[ Index ] <> nil )
      ) then
    begin
        Result := True ;
    end ;
end ;


function TCharacter_Set.Height : integer ;

begin
    Result := _Height ;
end ;


procedure TCharacter_Set.Initialize ;

begin
    _Height := 1 ;
    _Width := 1 ;
end ;


procedure TCharacter_Set.Load( Name : PChar ) ;

var Bitmap : TBitmap ;
    DC : HDC ;
    Dummy : integer ;
    F : textfile ;
    Font : TFont ;
    First : boolean ;
    N : string ;
    R : integer ;
    Rect : TRect ;
    Rows : Tinteger_List ;
    S, Work : string ;
    Saved : THandle ;
    Siz : TSize ;
    X, Y : integer ;

begin
    Clear ;
    _Height := 0 ;
    _Width := 8 ;
    N := string( Name ) ;
    if( Extension_Pos( N ) = 0 ) then
    begin
        N := N + '.c' ;
    end ;
    if( pos( '\', N ) + pos( ':', N ) = 0 ) then
    begin
        N := 'Fonts\' + N ;
    end ;
    if( not FileExists( N ) ) then
    begin
        N := string( Name ) ;
        Font := TFont.Create ;
        try
            Font.Name := N ;
            if( lowercase( Font.Name ) <> lowercase( N ) ) then // Not found
            begin
                exit ;
            end ;
            S := 'W' ;
            DC := GetDC( 0 ) ;
            try
                Saved := selectobject( DC, Font.Handle ) ;
                try
                    GetTextExtentPoint32( DC, PChar( S ), 1, Siz ) ;
                finally
                    selectobject( DC, Saved ) ;
                end ;
            finally
                ReleaseDC( 0, DC ) ;
            end ;
        finally
            Font.Free ;
        end ;
        _Height := Siz.cy ;
        _Width := Siz.cx ;
        for Dummy := 0 to 31 do
        begin
            Character_Set.Add( nil ) ;
        end ;
        for Dummy := 32 to 126 do
        begin
            Rect.Top := 0 ;
            Rect.Left := 0 ;
            Rect.Bottom := _Height ;
            Rect.Right := _Width ;
            Bitmap := TBitmap.Create ;
            Bitmap.Width := _Width ;
            Bitmap.Height := _Height ;
            Bitmap.Canvas.Font.Name := N ;
            Bitmap.Canvas.Font.Color := clWhite ;
            Bitmap.Canvas.Brush.Color := clBlack ;
            Bitmap.Canvas.FillRect( Rect ) ;
            Bitmap.Canvas.TextOut( 0, 0, chr( Dummy ) ) ;
            Character_Set.Add( Bitmap ) ;
        end ;
        Character_Set.Add( nil ) ;
        exit ;
    end ; // if( not File_exists( N ) )

    // Load raster font...
    assignfile( F, N ) ;
    {$I-}
    System.reset( F ) ;
    {$I+}
    if( IOResult <> 0 ) then
    begin
        exit ;
    end ;
    Rows := Tinteger_List.Create ;
    try
        First := True ;
        while( not eof( F ) ) do
        begin
            // Process next character...
            {$I-}
            readln( F, S ) ;
            {$I+}
            if( IOResult <> 0 ) then
            begin
                break ;
            end ;
            if( First ) then // First line in file...
            begin
                First := False ;
                if( Edit( S, -1 ) = 'RASTER' ) then
                begin
                    {$I-}
                    readln( F, S ) ;
                    {$I+}
                    if( IOResult <> 0 ) then
                    begin
                        break ;
                    end ;
                    try
                        _Width := strtoint( S ) ;
                    except
                    end ;

                    // Get first line of raster data
                    {$I-}
                    readln( F, S ) ;
                    {$I+}
                    if( IOResult <> 0 ) then
                    begin
                        break ;
                    end ;
                end ;
            end ;

            // Process this character
            Dummy := pos( ';', S + ';' ) ;
            S := trim( copy( S, 1, Dummy - 1 ) ) ; // Trim comments
            if( S = '0' ) then // Non-displaying character
            begin
                S := '' ;
            end ;
            while( length( S ) > 0 ) do
            begin
                Dummy := pos( ',', S + ',' ) ;
                Work := copy( S, 1, Dummy - 1 ) ;
                S := trim( copy( S, Dummy + 1, length( S ) ) ) ;
                Work := Edit( Work, -1 ) ;
                try
                    if( copy( Work, 1, 2 ) = '0B' ) then
                    begin
                        Rows.Add( strtoint( CVTB( 2, 10, copy( Work, 3, length( Work ) ) ) ) ) ;
                    end else
                    if( copy( Work, 1, 2 ) = '0X' ) then
                    begin
                        Rows.Add( strtoint( CVTB( 16, 10, copy( Work, 3, length( Work ) ) ) ) ) ;
                    end else
                    if( copy( Work, 1, 1 ) = '0' ) then
                    begin
                        Rows.Add( strtoint( CVTB( 8, 10, Work ) ) ) ;
                    end else
                    if( copy( Work, 1, 1 ) = '$' ) then
                    begin
                        Rows.Add( strtoint( CVTB( 16, 10, Work ) ) ) ;
                    end else
                    if( Work = '' ) then
                    begin
                        Rows.Add( 0 ) ;
                    end else
                    begin
                        Rows.Add( strtoint( Work ) ) ;
                    end ;
                except
                end ;
                if( Rows.Count > _Height ) then
                begin
                    _Height := Rows.Count ;
                end ;
            end ; // while( ( length( S ) > 0 ) and ( Line < 64 ) )
            if( Rows.Count > 0 ) then // Displaying character
            begin
                Bitmap := TBitmap.Create ;
                Bitmap.Height := _Height ;
                Bitmap.Width := _Width ;
                Bitmap.Canvas.Brush.Color := clBlack ;
                Rect.Top := 0 ;
                Rect.Left := 0 ;
                Rect.Bottom := _Height ;
                Rect.Right := _Width ;
                Bitmap.Canvas.FillRect( Rect ) ;
                Y := 0 ; // Which row
                for R := 0 to Rows.Count - 1 do
                begin
                    X := _Width - 1 ; // X pixel
                    Dummy := Rows[ R ] ;
                    while( Dummy > 0 ) do
                    begin
                        if( ( Dummy and 1 ) <> 0 ) then
                        begin
                            Bitmap.Canvas.Pixels[ X, Y ] := clWhite ;
                        end ;
                        dec( X ) ;
                        Dummy := Dummy shr 1 ;
                    end ; // while( Dummy > 0 )
                    inc( Y ) ;
                end ; // for R := 0 to Rows.Count - 1
            end else
            begin
                Bitmap := nil ;
            end ;
            Character_Set.Add( Bitmap ) ;
            Rows.Clear ;
        end ; // while( not eof( F ) )
    finally
        Rows.Free ;
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
    end ;
end ; // TCharacter_Set.Load


procedure TCharacter_Set.Terminate ;

begin
    Clear ;
    Free ;
end ;


function TCharacter_Set.Width( Index : longint ) : longint ;

begin
    Result := _Width ;
end ;



// TKey_Mapper methods...

constructor TKey_Mapper.Create ;

begin
    inherited Create ;

    Keys := TStringList.Create ;
    _Mapping := TStringList.Create ;
    Mappings := TStringList.Create ;
end ;


procedure TKey_Mapper.Add_Key( Name : PChar ) ;

var S : string ;

begin
    S := string( Name ) ;
    if( Keys.Indexof( Name ) = -1 ) then
    begin
        Keys.Add( S ) ;
        _Mapping.Add( '' ) ;
    end ;
end ;


procedure TKey_Mapper.Add_Mapping( Name : PChar ) ;

var S : string ;

begin
    S := string( Name ) ;
    if( Mappings.Indexof( S ) = -1 ) then // Mapping not defined
    begin
        Mappings.Add( S ) ;
    end ;
end ;


procedure TKey_Mapper.Clear_Keys( Mapping_Only : boolean ) ;

var I : integer ;

begin
    if( Mapping_Only ) then
    begin
        for I := 0 to _Mapping.Count - 1 do
        begin
            _Mapping[ I ] := '' ;
        end ;
    end else
    begin
        Keys.Clear ;
        _Mapping.Clear ;
    end ;
end ;


procedure TKey_Mapper.Clear_Mappings ;

begin
    Mappings.Clear ;
end ;


function TKey_Mapper.Query : boolean ;

var CB : TComboBox ;
    Control : TControl ;
    Lab : TLabel ;
    Lab_Width : integer ;
    List : TList ;
    Loop : integer ;
    X, Y : integer ;

begin
    // Sanity check
    if( ( Keys.Count = 0 ) or ( Mappings.Count = 0 ) ) then
    begin
        Result := False ;
        exit ; // Nothing for the user to see
    end ;

    // Create dialog
    if( Key_Mapper_Dialog = nil ) then
    begin
        Key_Mapper_Dialog := TKey_Mapper_Dialog.Create( Application ) ;
    end ;

    // Clear existing dialog...
    for Loop := Key_Mapper_Dialog.ControlCount - 1 downto 0 do
    begin
        Control := Key_Mapper_Dialog.Controls[ Loop ] ;
        if( not ( Control is TPanel ) ) then
        begin
            Control.Free ;
        end ;
    end ;

    // Determine metrics...
    Lab_Width := 0 ;
    for Loop := 0 to Keys.Count - 1 do
    begin
        X := Key_Mapper_Dialog.Canvas.TextWidth( Keys[ Loop ] ) ;
        if( X > Lab_Width ) then
        begin
            Lab_Width := X ;
        end ;
    end ;
    Lab_Width := Lab_Width + 4 ;

    List := TList.Create ;
    try
        // Populate and size dialog...
        X := 8 ;
        Y := 8 ;
        for Loop := 0 to Keys.Count - 1 do
        begin
            if( Loop = Keys.Count div 2 ) then
            begin
                // Start second column...
                X := Lab_Width * 3 ;
                Y := 8 ;
            end ;

            // Create label...
            Lab := TLabel.Create( Key_Mapper_Dialog ) ;
            Lab.Parent := Key_Mapper_Dialog ;
            Lab.Caption := Keys[ Loop ] + ':' ;
            Lab.Top := Y + 4 ;
            Lab.Left := X ;

            // Create combobox...
            CB := TComboBox.Create( Key_Mapper_Dialog ) ;
            CB.Style := csDropDownList ;
            CB.Parent := Key_Mapper_Dialog ;
            CB.Top := Y ;
            CB.Left := X + Lab_Width ;
            CB.Items.Assign( Mappings ) ;
            CB.Width := Lab_Width + 2 + GetSystemMetrics( SM_CYVSCROLL ) ;
            List.Add( CB ) ;

            // Set current mapping
            if( _Mapping[ Loop ] <> '' ) then
            begin
                CB.ItemIndex := CB.Items.Indexof( _Mapping[ Loop ] ) ;
            end ;

            Y := Y + CB.Height + 4 ;
        end ;
        Key_Mapper_Dialog.ClientHeight := Y + Key_Mapper_Dialog.Panel1.Height * 2 ;
        Key_Mapper_Dialog.ClientWidth := CB.Left + CB.Width + 4 ;

        // Query user
        Result := ( Key_Mapper_Dialog.ShowModal = mrOK ) ;

        // Apply user's changes
        if( Result ) then
        begin
            for Loop := 0 to Keys.Count - 1 do
            begin
                _Mapping[ Loop ] := TComboBox( List[ Loop ] ).Text ;
            end ;
        end ;
    finally
        List.Free ;
    end ;
end ;


function TKey_Mapper.Mapping( Name : PChar ) : PChar ;

var Index : integer ;
    S : string ;

begin
    S := string( Name ) ;
    Index := Keys.Indexof( S ) ;
    if( Index = -1 ) then
    begin
        Result := nil ;
    end else
    begin
        Temp_Mapping := _Mapping[ Index ] ;
        Result := PChar( Temp_Mapping ) ;
    end ;
end ;


procedure TKey_Mapper.Set_Mapping( Key, Mapping : PChar ) ;

var _Key, __Mapping : string ;
    Index : integer ;

begin
    _Key := string( Key ) ;
    __Mapping := string( Mapping ) ;
    Index := Keys.IndexOf( _Key ) ;
    if( Index <> -1 ) then
    begin
        _Mapping[ Index ] := __Mapping ;
    end ;
end ;


procedure TKey_Mapper.Terminate ;

begin
    Keys.Free ;
    Keys := nil ;
    Mappings.Free ;
    Mappings := nil ;
    _Mapping.Free ;
    _Mapping := nil ;
    Free ;
end ;



// Constructors and destructors...

constructor TAssembler_Context.Create ;

begin
    inherited Create ;

    Symbol_Table.Init( nil ) ;
    Filenames := TStringList.Create ;
    Addresses := Tinteger64_List.Create ;
    Mappings := TList.Create ;
end ;


destructor TAssembler_Context.Destroy ;

var Loop : integer ;

begin
    Symbol_Table.Done ;
    Filenames.Free ;
    Filenames := nil ;
    Addresses.Free ;
    Addresses := nil ;
    for Loop := 0 to Mappings.Count - 1 do
    begin
        TMapping_Info( Mappings[ Loop ] ).Free ;
        Mappings[ Loop ] := nil ;
    end ;
    Mappings.Free ;
    Mappings := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TAssembler_Context._Add_Mapping( Filename : string ; Address : int64 ;
    Line : integer ) : integer ;

var Address_Index, Index : integer ;
    Info : TMapping_Info ;

begin
    Filename := uppercase( Filename ) ;
    Index := Filenames.IndexOf( Filename ) ;
    if( Index = -1 ) then
    begin
        Index := Filenames.Add( Filename ) ;
    end ;

    Info := TMapping_Info.Create ;
    Info.File_Index := Index ;
    Info.Line := Line ;
    Address_Index := Addresses.Indexof( Address ) ;
    if( Address_Index = -1 ) then
    begin
        Address_Index := Addresses.Add( Address ) ;
        Mappings.Add( Info ) ;
    end else
    begin
        TMapping_Info( Mappings[ Address_Index ] ).Free ;
        Mappings[ Address_Index ] := Info ;
    end ;
    Result := Address_Index ;
end ;


function TAssembler_Context._Mapping( Address : int64 ;
    var Filename : string ) : integer ;

var Index : integer ;
    Info : TMapping_Info ;

begin
    Index := Addresses.IndexOf( Address ) ;
    if( Index = -1 ) then
    begin
        Result := -1 ;
        exit ;
    end ;
    Info := TMapping_Info( Mappings[ Index ] ) ;
    Filename := Filenames[ Info.File_Index ] ;
    Result := Info.Line ;
end ;


function TAssembler_Context._Symbol_Value( const Name : string ;
    var Value : int64 ) : boolean ;

var Err, Symbol_D_T, Symbol_Siz, Symbol_Flg : integer ;
    Symbol_Addr : int64 ;
    Symbol_Data : pointer ;

begin
    Err := Symbol_Table.Find( Name, Symbol_Addr,
        Symbol_Flg, Symbol_D_T, Symbol_Siz, Symbol_Data ) ;
    if( Err <> 0 ) then
    begin
        Result := False ;
        exit ;
    end ;
    Value := pSymbol_Record( Symbol_Data )^.Address ;
    Result := True ;
end ;


function TAssembler_Context._Symbol_Size( const Name : string ;
    var Value : integer ) : boolean ;

var Err, Symbol_D_T, Symbol_Siz, Symbol_Flg : integer ;
    Symbol_Addr : int64 ;
    Symbol_Data : pointer ;

begin
    Err := Symbol_Table.Find( Name, Symbol_Addr,
        Symbol_Flg, Symbol_D_T, Symbol_Siz, Symbol_Data ) ;
    if( Err <> 0 ) then
    begin
        Result := False ;
        exit ;
    end ;
    Value := Symbol_Siz ;
    Result := True ;
end ;


// API...

function TAssembler_Context.Add_Mapping( Filename : PChar ; Address : int64 ;
    Line : integer ) : integer ;

begin
    Result := _Add_Mapping( string( Filename ), Address, Line ) ;
end ;


function TAssembler_Context.Add_Symbol( Name : PChar ;
    P : pSymbol_Record ) : TUnified_Exception ;

begin
    Symbol_Table.Add( Name, P^.Address, P^.Flags, P^.Typ, P^.Size, P ) ;
    Result := nil ;
end ;


procedure TAssembler_Context.Delete( Sym : PChar ) ;

begin
    Symbol_Table.Delete( string( Sym ) ) ;
end ;


procedure TAssembler_Context.Delete_Mapping( Index : integer ) ;

begin
    if( Index < Mappings.Count ) then
    begin
        TMapping_Info( Mappings[ Index ] ).Free ;
        Mappings[ Index ] := nil ;
        Mappings.Delete( Index ) ;
        Addresses.Delete( Index ) ;
    end ;
end ;


function TAssembler_Context.Find( Sym : PChar ; var Addr : int64 ;
    var Flg, D_T, Siz : longint ; var Dat : pointer ) : integer ;

begin
    Result := Symbol_Table.Find( string( Sym ), Addr, Flg, D_T, Siz, Dat ) ;
end ;


function TAssembler_Context.Mapping( Address : int64 ;
    var Filename : PChar ) : integer ;

begin
    Result := _Mapping( Address, Temp_Filename ) ;
    Filename := PChar( Temp_Filename ) ;
end ;


procedure TAssembler_Context.Pop_Level ;

begin
    Symbol_Table.Pop_Level ;
end ;


procedure TAssembler_Context.Push_Level ;

begin
    Symbol_Table.Push_Level ;
end ;


function TAssembler_Context.Symbol_Value( Name : PChar ;
    var Value : int64 ) : boolean ;

begin
    Result := _Symbol_Value( string( Name ), Value ) ;
end ;


function TAssembler_Context.Symbol_Size( Name : PChar ;
    var Value : integer ) : boolean ;

begin
    Result := _Symbol_Size( string( Name ), Value ) ;
end ;


function TAssembler_Context.Get_Case_Sensitive : boolean ;

begin
    Result := Symbol_Table.Case_Sensitive ;
end ;


procedure TAssembler_Context.Set_Case_Sensitive( Value : boolean ) ;

begin
    Symbol_Table.Case_Sensitive := Value ;
end ;


function TAssembler_Context.Find_First( var Sym : PChar ; var Addr : int64 ;
    var Flg, D_T, Siz : longint ; var Dat : pointer ) : integer ;

begin
    Result := Symbol_Table.Find( '*', Addr, Flg, D_T, Siz, Dat ) ;
    Temp_Find_Next := Symbol_Table.Last_Symbol^.Name^ ;
    Sym := PChar( Temp_Find_Next ) ;
end ;


function TAssembler_Context.Find_Next( var Sym : PChar ; var Addr : int64 ;
    var Flg, D_T, Siz : longint ; var Dat : pointer ) : integer ;

begin
    Result := Symbol_Table.Find_Next( Addr, Flg, D_T, Siz, Dat ) ;
    Temp_Find_Next := Symbol_Table.Last_Symbol^.Name^ ;
    Sym := PChar( Temp_Find_Next ) ;
end ;


// API...

procedure Add_Breakpoint( UI : TUI_Interface ; Component : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Base, Size : longint ;
    Port : boolean ) ;

begin
    Mem_Watchpoints_Dialog.Add_Breakpoint( UI, Component, Watchpoints, Base,
        Size, Port ) ;
end ;


procedure Add_Watchpoint( UI : TUI_Interface ; Component : TComponent ;
    Component_Type : longint ; Watchpoints : TCEF_Watchpoint_Manager ;
    Base, Size : longint ; Low, High : int64 ; Memory : boolean ;
    var Address : int64 ; var Access : longint ; Context : pointer ;
    Domain : Pchar ) ;

begin
    Mem_Watchpoints_Dialog.Add_Watchpoint( UI, Component, Component_Type,
        Watchpoints, Base, Size, Low, High, Memory, Address, Access, Context,
        Domain ) ;
end ;


procedure Show_Watchpoints( UI : TUI_Interface ; CPU : TComponent ;
    Watchpoints : TCEF_Watchpoint_Manager ; Ports : boolean ) ;

begin
    Mem_Watchpoints_Dialog.Show_Watchpoints( UI, CPU, Watchpoints, Ports ) ;
end ;


end.
