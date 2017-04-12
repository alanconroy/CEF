{
        Program Name : CEF_Assembler
        Package Name : CEF
        Purpose      : Default master assembler
        Institution  : Conroy & Conroy
        Date Written : 19-Mar-87
        Written By   : Alan Conroy
        Version      : 2.5

        Copyright (C) 1987-2015 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *********************************************************
        *                                                       *
        *        M O D I F I C A T I O N   H I S T O R Y        *
        *                                                       *
        *********************************************************

             DATE      BY             REASON                   

        *********************************************************
        *                                                       *
        *           P R O G R A M   P U R P O S E               *
        *                                                       *
        *********************************************************

          This unit defines the master assembler for CEF.

        *********************************************************
        *                                                       *
        *        C O N T R I B U T O R S                        *
        *                                                       *
        *********************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CEF_Assembler ;

interface

uses // Borland...
     Classes, // TStringList
     Forms, // Application

     { CEF... }
     _CEF, // TMaster_Assembler
     CEF, // TBase_Master_Assembler

     { Other... }
     Filestd, // TSStandard_File
     Parse, // TParser
     _Streams, // TCOM_Stream
     Standard, // TInteger64_List
     Symbols, // TSymbol_Table
     _UE ; // TUnified_Exception

const CEFAssemblerErr_Facility = 54 ;
      CEFAssemblerErr_Success = 0 ;
      CEFAssemblerErr_Invalid_Digits = 1 ; // Invalid digits for radix
      CEFAssemblerErr_Illegal_Expression = 2 ; // Illegal expression
      CEFAssemblerErr_Unterminated_String_Literal = 3 ; // Unterminated string literal
      CEFAssemblerErr_Undefined_Symbol = 4 ; // Undefined symbol
      CEFAssemblerErr_Illegal_Instruction = 5 ; // Illegal instruction
      CEFAssemblerErr_Missing_Value = 6 ; // Missing value
      CEFAssemblerErr_Multiply_Defined = 7 ; // Multiply defined symbol

type TStringList_Parser = class( TParser )
                              private // Instance data...
                                  _List : TStringList ;
                                  _Index : integer ;
                                  Parser : TString_Parser ;

                              public { Constructors and destructors... }
                                  constructor Create( List : TStringList ) ;
                                  destructor Destroy ; override ;

                              public { Property handlers... }
                                  function Get_Current_Line : longint ;
                                  function Get_Token_Line : longint ;
                                  function Get_Non_Terminals : string ;
                                  procedure Set_Non_Terminals( S : string ) ;

                              public { API... }
                                  function Grab_Line : string ; override ;
                                  procedure Put_Token( const S : string ) ;
                                      override ;
                                  procedure Reset ;
                                  function Token : string ; override ;
                                  function Token_EOL : boolean ; override ;

                                  property Non_Terminals : string
                                      read Get_Non_Terminals
                                      write Set_Non_Terminals ;
                          end ; // TStringList_Parser

     TState = class
                  public // Constructors and destructors...
                      destructor Destroy ; override ;

                  public // API...
                      State : integer ;
                      Context : string ;
                      List : TStringList ;
                      Tokens : TParser ;
              end ;

     TCEF_Assembler = class( TBase_Master_Assembler )
                          public // Instance data...
                              Altered_Address : int64 ; // If Altered_PC is true, this is the new PC
                              Altered_PC : boolean ; // True if an origin change
                              _Assembler : TAssembler ;
                              _Assembler_Context : TCEF_Assembler_Context ;
                              Backpatch_Table : TStringList ;
                              _Base : integer ;
                              Err : boolean ; { True if an error }
                              _CPU : TComponent ; // Current target CPU
                              CPU_List : TStringList ;
                              PC : int64 ; { Current target address }
                              Segment : integer ; { Current segment }
                              Tokens : TFile_Parser ;
                              Terminated : boolean ; { True if we processed a .END directive }
                              In_Assembly_Status : TAssembler_Status ;
                              Forwards_Names : TStringList ;
                              Forwards_Values : TStringList ;
                              Initial_Symbols : string ;
                              Valid_Symbols : string ;
                              Temp_Expand : string ; // Used by .Expand
                              Last_Mapping_Index : integer ;
                              States : TList ; // Current set of states
                              Start_Address : int64 ; // Starting execution address
                              Have_Start_Address : boolean ; // True if a start address specified
                              _UI : TUI_Interface ;
                              Macros, Macro_Arguments : TStringList ;
                              Current_Created_Local_Label : integer ;
                              Big_Endian : boolean ; // True if data is big-endian
                              List_Codes : boolean ; // True to send generated code to the listing
                              _Status : TAssembler_Status ;
                              Want_Symbol_Table_List : boolean ;
                              Want_XRef_List : boolean ;
                              XRefs : TStringList ;
                              Inhibit_Xref_Add : boolean ; // True to inhibit Adds to XRef list
                              _Extension : TAssembler_Extension ;
                              Temp : string ;

                          public // Constructors and destructors...
                              constructor Create( CPU : TComponent ;
                                  UI : TUI_Interface ) ;

                              destructor Destroy ; override ;

                          private // Internal utility routines...
                              function Forward_Declare( Value : string ;
                                  const Input_Buffer : string ) : boolean ;

                              function Grab_Current_Line : string ;

                              function Handle_Directives( var Value : string ;
                                  var Any_Code : boolean ;
                                  Status : TAssembler_Status ) : boolean ;

                              procedure Enter_State( State : integer ;
                                  const Context : string ; List : TStringList ;
                                  Stream : TCom_Stream ) ;

                              procedure Exit_State ;

                              function _Find( Sym : string ; var Addr : int64 ;
                                  var Flg, D_T, Siz : longint ;
                                  var Dat : pointer ) : integer ;

                              function _Get_Token : string ;

                              function _Grab_Line( Current : boolean ) : string ;

                              procedure _Put_Token( const Token : string ) ;

                              function _Token_EOL : boolean ;

                              function _Next_Token : string ;

                              function Process_ASCII( S : string ;
                                  Status : TAssembler_Status ) : string ;

                              function Process_If( Condition, Argument : string ;
                                  Status : TAssembler_Status ) : boolean ;

                              function Process_If1( Argument1, Condition, Argument2 : string ;
                                  Status : TAssembler_Status ) : boolean ;

                              procedure Process_IRP( Symbol, Value : string ;
                                  Source, Target : TStringList ) ;

                              procedure Set_Target_CPU( CPU : TComponent ) ;

                              function Get_Next_Token( var X : string ) : string ;

                          public // API...
                              procedure Define_Macro( Name : string ;
                                  const Arguments : string ;
                                  SL : TStringList ) ;

                              procedure Define_Macro_SL( Name : string ;
                                  Arguments : string ;
                                  S : string ) ;

                              procedure Delete_Macro( Name : string ) ;

                              // True if error.
                              function Process_Macro( const Name : string ;
                                  Status : TAssembler_Status ;
                                  const Passed_Parameters : string ) : boolean ;

                          public // API (overrides)...
                              property Assembler_Context : TCEF_Assembler_Context
                                  read Get_Assembler_Context
                                  write Set_Assembler_Context ;

                              procedure Add_Reference_Ex( Name : PChar ;
                                  Size : longint ; Address : int64 ;
                                  Context, Flags : longint ) ;
                                  override ; stdcall ;

                              procedure Backpatch( Status : TAssembler_Status ;
                                  Stream : TCOM_Stream ) ;
                                  override ; stdcall ;

                              function Expand( Source : PChar ;
                                  var Res : PChar ;
                                  var Res_Length : longint ;
                                   Status : TAssembler_Status ) : TUnified_Exception ;
                                  override ; stdcall ;

                              function Evaluate( Value : PChar ;
                                  var _Result : int64 ) : TUnified_Exception ;
                                  override ; stdcall ;

                              function Evaluate_Ex( Value : PChar ;
                                  var _Result : int64 ;
                                  PC_Adjustment : int64 ) : TUnified_Exception ;
                                  override ; stdcall ;

                              function Facility_Code : longint ;
                                  override ; stdcall ;

                              procedure Set_Assembler_Context( Value : TCEF_Assembler_Context ) ;
                                  override ; stdcall ;

                              function Get_Assembler_Context : TCEF_Assembler_Context ;
                                  override ; stdcall ;

                              procedure In_Line( Input : TCOM_Stream ) ;
                                  override ; stdcall ;

                              procedure Log_Error( Text : PChar ;
                                  Severity : longint ) ; override ; stdcall ;

                              function Get_Token : PChar ; override ; stdcall ;

                              procedure Put_Token( Token : PChar ) ;
                                  override ; stdcall ;

                              function Peek_Token( Same_Line : boolean ) : PChar ;
                                  override ; stdcall ;

                              procedure Push_Scope ; override ; stdcall ;

                              procedure Pop_Scope ; override ; stdcall ;

                              function Get_Symbol( Name : PChar ) : PSymbol_Record ;
                                  override ; stdcall ;

                              function Add_Symbol( Name : PChar ;
                                  P : pSymbol_Record ) : TUnified_Exception ;
                                  override ; stdcall ;

                              procedure Set_Case_Sensitive( Value : boolean ) ;
                                  override ; stdcall ;

                              function Get_Case_Sensitive : boolean ;
                                  override ; stdcall ;

                              function Grab_Line( Current : boolean ) : PChar ;
                                  override ; stdcall ;

                              procedure Map( Address : int64 ) ;
                                  override ; stdcall ;

                              procedure UnMap ; override ; stdcall ;

                              procedure Add_CPU( CPU : TComponent ; Name : PChar ) ;
                                  override ; stdcall ;

                              procedure Clear_CPUs ; override ; stdcall ;

                              function Leading_Whitespace : boolean ;
                                  override ; stdcall ;

                              function Assemble_Ex( Input, Output, Listing : TCOM_Stream ;
                                  Status : TAssembler_Status ;
                                  Flags : longint ) : TUnified_Exception ;
                                  override ; stdcall ;

                              function Get_Base : integer ; override ; stdcall ;

                              procedure Set_Base( Value : integer ) ;
                                  override ; stdcall ;

                              procedure Register_Extension( Extension : TAssembler_Extension ) ;
                                  override ; stdcall ;
                      end ; // TCEF_Assembler

implementation

uses // Borland...
     Dialogs, // ShowMessage
     SysUtils, // uppercase

     // C&C...
     _ASCII, // CR
     CommonUt, // FEdit
     CVT,
     Express,
     _Stores, // TStore_Address
     Instrs, // Instr
     Maths,
     NUM1S,
     Radix50s, // Valid_Radix50
     SStreams, // TCOM_String_Stream
     TypeDefs, // TRecord_Size
     UStrings ; // String_To_BCD

// Nested states...
const State_Repeat = 1 ;
      State_Include = 2 ;
      State_Macro = 3 ;
      State_IRP = 4 ;
      State_If = 5 ;
      State_Inline = 6 ;

function Get_Assembler_Context : TCEF_Assembler_Context ; stdcall ;
    external 'CEF_Util.dll' ;

// General utility routines...

function DLL_Get_Assembler_Context : TCEF_Assembler_Context ;

begin
    Result := Get_Assembler_Context ;
end ;


function Is_PC( X : string ) : boolean ;

begin
    if( ( X = '.' ) or ( X = '$' ) ) then
    begin
        Result := True ;
        exit ;
    end ;
    Result := False ;
    if( ( copy( X, 1, 1 ) = '.' ) or ( copy( X, 1, 1 ) = '$' ) ) then
    begin
        X := uppercase( copy( X, 2, 1 ) ) ;
        Result := ( pos( X, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_$.' ) = 0 ) ;
    end ;
end ;


// TStringList_Parser methods...

// Constructors and destructors...

constructor TStringList_Parser.Create( List : TStringList ) ;

begin
    inherited Create ;

    _List := List ;
    _Index := 0 ;
    Parser := TString_Parser.Create ;
    Reset ;
end ;


destructor TStringList_Parser.Destroy ;

begin
    Parser.Free ;
    Parser := nil ;

    inherited Destroy ;
end ;



// Property handlers...

function TStringList_Parser.Get_Current_Line : longint ;

begin
    Result := _Index ;
end ;


function TStringList_Parser.Get_Token_Line : longint ;

begin
    Result := _Index ;
end ;


function TStringList_Parser.Get_Non_Terminals : string ;

begin
    Result := Parser.Get_Non_Terminals ;
end ;


procedure TStringList_Parser.Set_Non_Terminals( S : string ) ;

begin
    Parser.Set_Non_Terminals( S ) ;
end ;


// API...

function TStringList_Parser.Grab_Line : string ;

label Try_Again ;

begin
    Result := '' ;
Try_Again:
    if( Parser.Token_EOL ) then
    begin
        inc( _Index ) ;
        if( _Index < _List.Count ) then
        begin
            Parser.Set_Source( _List[ _Index ] ) ;
            goto Try_Again ;
        end ;
        exit ;
    end ;
    Result := Parser.Grab_Line ;

    // Now prepare the next line...
    inc( _Index ) ;
    if( _Index < _List.Count ) then
    begin
        Parser.Set_Source( _List[ _Index ] ) ;
    end ;
end ;


procedure TStringList_Parser.Put_Token( const S : string ) ;

begin
   Parser.Put_Token( S ) ;
end ;


procedure TStringList_Parser.Reset ;

begin
    _Index := 0 ;
    if( _List.Count > 0 ) then
    begin
        Parser.Set_Source( _List[ 0 ] ) ;
    end ;
end ;


function TStringList_Parser.Token : string ;

label Try_Again ;

begin
Try_Again:
    Result := Parser.Token ;
    if( length( Result ) = 0 ) then
    begin
        inc( _Index ) ;
        if( _Index < _List.Count ) then
        begin
            Parser.Set_Source( _List[ _Index ] ) ;
            goto Try_Again ;
        end ;
    end ;
end ;


function TStringList_Parser.Token_EOL : boolean ;

begin
    Result := Parser.Token_EOL ;
end ;



type TStreamer_File = object( TSStandard_File )
                          public
                              Stream : TCOM_Stream ;

                              function Extend( Amount : TStore_Address ) : TStore_Address ;
                                  virtual ;

                              function Get_Name : string ; virtual ;

                              function Max_Storage : TStore_Address ; virtual ;

                              function Min_Storage : TStore_Address ; virtual ;

                              function Contiguous_Store : boolean ; virtual ;

                              procedure Set_Max_Storage( Value : TStore_Address ;
                                  var Res : PSUnified_Exception ) ; virtual ;

                              procedure Reset( const Name : String ) ; virtual ;
                              procedure Rewrite( const Name : String ) ;
                                  virtual ;
                              procedure Append( const Name : String ) ;
                                  virtual ;
                              procedure Close ; virtual ;
                              destructor Done ; virtual ;
                              procedure Seek( Pos : TStore_Address ) ; virtual ;

                              procedure Blockread( var Buf ;
                                  Count : TRecord_Size ;
                                  var Res : TRecord_Size ) ; virtual ;
                              procedure Blockwrite( var Buf ; Count : TRecord_Size ;
                                  var Res : TRecord_Size ) ; virtual ;
                              function EOF : boolean ; virtual ;
                              function FilePos : TStore_Address ; virtual ;
                              function Get_Size : TStore_Address ; virtual ;
                              procedure Set_Size( New_Size : TStore_Address ) ;
                                  virtual ;
                              procedure Flush ; virtual ;
                              procedure GetFAttr( var Attr : Word ) ;
                                  virtual ;
                              procedure Erase( const Name : String ) ; virtual ;
                              function FileName : string ; virtual ;
                              procedure SetFAttr( Attr : Word ) ; virtual ;
                              procedure GetFTime( var _Time : Longint ) ;
                                  virtual ;
                              procedure SetFTime( _Time : longint ) ; virtual ;
                              procedure SetTextBuf( var buf ; size : Word ) ;
                                  virtual ;
                              procedure Set_Extend( State : boolean ) ;
                                  virtual ;
                              procedure Truncate( Size : TStore_Address ) ;
                                  virtual ;
                              procedure Initialize ; virtual ;
                              function Get_Attributes : longint ; virtual ;
                              procedure Set_Attributes( Value : longint ) ;
                                  virtual ;
                              function Facility : longint ; virtual ;

                              function Version : longint ; virtual ;

                              function Facility_Name : PChar ; virtual ;
                              function Opened : boolean ; virtual ;
                      end ; // TStreamer_File
     PStreamer_File = ^TStreamer_File ;

function TStreamer_File.Extend( Amount : TStore_Address ) : TStore_Address ;

begin
    // Has no meaning for us
    Extend := -1 ;
end ;


function TStreamer_File.Get_Name : string ;

begin
    Result := '' ;
end ;


function TStreamer_File.Max_Storage : TStore_Address ;

begin
    Result := 0 ;
end ;


function TStreamer_File.Min_Storage : TStore_Address ;

begin
    Result := 0 ;
end ;


function TStreamer_File.Contiguous_Store : boolean ;

begin
    Result := True ;
end ;


procedure TStreamer_File.Set_Max_Storage( Value : TStore_Address ;
    var Res : PSUnified_Exception ) ;

begin
    // Has no meaning for us
end ;


procedure TStreamer_File.Reset( const Name : String ) ;

begin
    Stream.Seek( 0 ) ;
end ;


procedure TStreamer_File.Rewrite( const Name : String ) ;

begin
    Stream.Seek( 0 ) ;
    Truncate( FilePos ) ;
end ;


procedure TStreamer_File.Append( const Name : String ) ;

var C : char ;
    S : integer ;

begin
    while( not Stream.At_End ) do
    begin
        S := 1 ;
        Stream.Read( C, S ) ;
    end ;
end ;


procedure TStreamer_File.Close ;

begin
    // Has no meaning for us
end ;


destructor TStreamer_File.Done ;

begin
end ;


procedure TStreamer_File.Seek( Pos : TStore_Address ) ;

begin
    Stream.Seek( Pos ) ;
end ;


procedure TStreamer_File.Blockread( var Buf ; Count : TRecord_Size ;
    var Res : TRecord_Size ) ;

begin
    Res := Count ;
    Stream.Read( Buf, Res ) ;
end ;


procedure TStreamer_File.Blockwrite( var Buf ; Count : TRecord_Size ;
    var Res : TRecord_Size ) ;

begin
    Res := Count ;
    Stream.Write( Buf, Res ) ;
end ;


function TStreamer_File.EOF : boolean ;

begin
    Result := Stream.At_End ;
end ;


function TStreamer_File.FilePos : TStore_Address ;

begin
    FilePos := 0 ;
end ;


function TStreamer_File.Get_Size : TStore_Address ;

begin
    Get_Size := 0 ;
end ;


procedure TStreamer_File.Set_Size( New_Size : TStore_Address ) ;

begin
    // Has no meaning for us
end ;


procedure TStreamer_File.Flush ;

begin
    // Has no meaning for us
end ;


procedure TStreamer_File.GetFAttr( var Attr : Word ) ;

begin
    Attr := 0 ;
end ;


procedure TStreamer_File.Erase( const Name : String ) ;

begin
    // Has no meaning for us
end ;


function TStreamer_File.FileName : string ;

begin
    Result := '' ;
end ;


procedure TStreamer_File.SetFAttr( Attr : Word ) ;

begin
    // Has no meaning for us
end ;


procedure TStreamer_File.GetFTime( var _Time : Longint ) ;

begin
    Time := 0 ;
end ;


procedure TStreamer_File.SetFTime( _Time : longint ) ;

begin
    // Has no meaning for us
end ;


procedure TStreamer_File.SetTextBuf( var buf ; size : Word ) ;

begin
    // Has no meaning for us
end ;


procedure TStreamer_File.Set_Extend( State : boolean ) ;

begin
    // Has no meaning for us
end ;


procedure TStreamer_File.Truncate( Size : TStore_Address ) ;

begin
    // Has no meaning for us
end ;


procedure TStreamer_File.Initialize ;

begin
end ;


function TStreamer_File.Get_Attributes : longint ;

begin
    Result := 0 ;
end ;


procedure TStreamer_File.Set_Attributes( Value : longint ) ;

begin
    // Has no meaning for us
end ;


function TStreamer_File.Facility : longint ;

begin
    Result := -1 ;
end ;


function TStreamer_File.Version : longint ;

begin
    Result := 10 ;
end ;


function TStreamer_File.Facility_Name : PChar ;

begin
    Result := nil ;
end ;


function TStreamer_File.Opened : boolean ;

begin
    Result := True ;
end ;


// TCEF_Assembler methods...

// Constructors and destructors...

constructor TCEF_Assembler.Create( CPU : TComponent ; UI : TUI_Interface ) ;

begin
    inherited Create ;

    _CPU := CPU ;
    _UI := UI ;
    if( CPU = nil ) then
    begin
        Base := 16 ;
    end else
    begin
        Base := CPU.CPU.Default_Base ;
    end ;
    PC := 0 ;
    List_Codes := True ;
    CPU_List := TStringList.Create ;

    Tokens := TFile_Parser.Create ;
    Tokens.Non_Terminals := Tokens.Non_Terminals + '.' ;
    Tokens.Case_Sensitive := True ;
    States := TList.Create ;
    Macros := TStringList.Create ;
    Macro_Arguments := TStringList.Create ;
    XRefs := TStringList.Create ;
end ;


destructor TCEF_Assembler.Destroy ;

var Loop : integer ;

begin
    Tokens.Free ;
    Tokens := nil ;
    Backpatch_Table.Free ;
    Backpatch_Table := nil ;
    Forwards_Names.Free ;
    Forwards_Names := nil ;
    Forwards_Values.Free ;
    Forwards_Values := nil ;
    _Assembler_Context.Free ;
    _Assembler_Context := nil ;
    States.Free ;
    States := nil ;
    CPU_List.Free ;
    CPU_List := nil ;
    XRefs.Free ;
    XRefs := nil ;
    
    for Loop := 0 to Macros.Count - 1 do
    begin
        TStringList( Macros.Objects[ Loop ] ).Free ;
        Macros.Objects[ Loop ] := nil ;
    end ;
    Macros.Free ;
    Macros := nil ;
    Macro_Arguments.Free ;
    Macro_Arguments := nil ;

    inherited Destroy ;
end ;


// Internal utility routines...

function TCEF_Assembler.Evaluate( Value : PChar ; var _Result : int64 ) : TUnified_Exception ;

begin
    Result := Evaluate_Ex( Value, _Result, 0 ) ;
end ;


function TCEF_Assembler.Get_Next_Token( var X : string ) : string ;

var Loop : integer ;

begin
    Result := '' ;
    X := Edit( X, 8 or 128 ) ;
    if( X = '' ) then
    begin
        exit ;
    end ;
    if( pos( X[ 1 ], '0123456789' ) > 0 ) then // A number
    begin
        Loop := 2 ;
        while(
               ( Loop <= length( X ) )
               and
               ( pos( X[ Loop ], '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ) > 0 )
             ) do
        begin
            inc( Loop ) ;
        end ;
        Result := copy( X, 1, Loop ) ;
        X := copy( X, Loop + 1, length( X ) ) ;
        exit ;
    end ;
    if( ( copy( X, 1, 1 ) = '"' ) or ( copy( X, 1, 1 ) = #39 ) ) then
    begin
        Loop := 2 ;
        while( ( Loop <= length( X ) ) and ( X[ Loop ] = X[ 1 ] ) ) do
        begin
            inc( Loop ) ;
        end ;
        Result := copy( X, 1, Loop ) ;
        X := copy( X, Loop + 1, length( X ) ) ;
        exit ;
    end ;
    if( copy( X, 1, 2 ) = '>>' ) then
    begin
        Result := '>>' ;
        X := copy( X, 3, length( X ) ) ;
        exit ;
    end ;
    if( copy( X, 1, 2 ) = '<<' ) then
    begin
        Result := '<<' ;
        X := copy( X, 3, length( X ) ) ;
        exit ;
    end ;
    Loop := 2 ;
    if( pos( X[ 1 ], Initial_Symbols ) > 0 ) then
    begin
        while(
           ( Loop <= length( X ) )
           and
           ( pos( X[ Loop ], Valid_Symbols ) > 0 )
         ) do
        begin
            inc( Loop ) ;
        end ;
    end ;
    Result := copy( X, 1, Loop - 1 ) ;
    X := copy( X, Loop, length( X ) ) ;
end ; // Get_Next_Token


function TCEF_Assembler.Evaluate_Ex( Value : PChar ; var _Result : int64 ;
    PC_Adjustment : int64 ) : TUnified_Exception ;

    function Va( X : string ; var ER : boolean ) : int64 ; { Return value of X }

    var A : String ;
        B : Real ;
        C, D, E : Integer ;

        procedure Strip( Y : Integer ) ; { Strip specifier and set E }

        begin
            E := Y ;
            X := Copy( X, 1, Length( X ) - 1 ) ;
        end ;

    begin
        ER := False ;
        Va := 0 ;
        if ( X[ 1 ] = Chr( 39 ) ) or ( X[ 1 ] = '"' ) then { ASCII literal }
        begin
            if X[ Length( X ) ] <> X[ 1 ] then { No closing quote of matching type }
            begin
                Er := True ;
                Exit ;
            end ;
            X := Copy( X, 2, Length( X ) - 2 ) ; { Strip quotes }
            if( ( Length( X ) > 2 ) or ( Length( X ) = 0 ) ) then
            begin
                Er := True ;
                Exit ;
            end ;
            B := Ord( X[ 1 ] ) ;
            if( Length( X ) = 2 ) then
            begin
                B := B + Ord( X[ 2 ] ) * 256 ;
            end ;
        end else
        begin
            if ( Pos( '.', X ) = 0 ) and ( Pos( '-', X ) = 0 ) then
            begin
                D := Length( X ) ;
                E := Base ;
                if( ( upcase( X[ D ] ) = 'H' ) and ( Base < 18 ) ) then Strip( 16 ) else
                   if ( upcase( X[ D ] ) = 'D' ) and ( Base < 14 ) then Strip( 10 ) else
                      if ( upcase( X[ D ] ) = 'B' ) and ( Base < 11 ) then Strip( 2 ) else
                         if ( upcase( X[ D ] ) = 'O' ) or ( upcase( X[ D ] ) = 'Q' ) then Strip( 8 ) ;
                for C := 1 to Length( X ) do
                begin
                    if Pos( upcase( X[ C ] ), Copy( '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', 1, E ) ) = 0 then
                    begin
                        Er := True ;
                        Exit ;
                    end ;
                end ;
                A := Cvtb( E, 10, X ) ;
            end else
            begin
                A := X ;
            end ;
            Val( A, B, C ) ;
            if( ( C <> 0 ) or ( B < low( Result ) ) or ( B > high( Result ) ) ) then
            begin
                Er := True ;
                Exit ;
            end ;
        end ;
        if( B > high( Result ) ) then
        begin
            B := B - high( Result ) - high( Result ) - 2 ;
        end ;
        Va := Trunc( B ) ;
    end ; // .Va.Strip


    procedure Invalid( S, V : string ) ;

    var Dummy : integer ;

    begin
        for Dummy := 1 to length( S ) do
        begin
            if( pos( S[ Dummy ], V ) = 0 ) then
            begin
                Result := Set_Error( CEFAssemblerErr_Invalid_Digits ) ;
                exit ;
            end ;
        end ;
    end ;


var Hex_Literal_Final : integer ;

    function Hex_Literal( X : string ) : boolean ;

    var Dummy : integer ;

    begin
        Result := False ; // Assume not
        if( X[ 1 ] = '$' ) then
        begin
            for Dummy := 2 to length( X ) do
            begin
                if( pos( X[ Dummy ], 'GHIJKLMNOPQRSTUVWXYZ$_.' ) > 0 ) then
                begin
                    exit ; // Not hexadecimal numeric
                end ;
                if( pos( X[ Dummy ], '0123456789ABCDEF' ) = 0 ) then
                begin
                    if( Dummy > 2 ) then // Need at least one valid digit
                    begin
                        X := copy( X, 2, Dummy - 2 ) ;
                        if( not Valid_Base( X, 16 ) ) then
                        begin
                            exit ; // Not a valid hexadecimal value
                        end ;
                        Result := True ;
                        Hex_Literal_Final := Dummy - 1 ;
                    end ;
                    exit ; // Anything else is an operator, comment, or white-space, thus ending the hex value
                end ;
            end ;
            // If we get here, then the whole string was a valid hexadecimal literal
            Result := True ;
            Hex_Literal_Final := length( X ) ;
        end ;
    end ; // .Hex_Literal

var Dummy : integer ;
    ER : boolean ; { True if Va() returned an error }
    Err : integer ;
    Entry : TState ;
    Expression : string ; { Expression to evaluate }
    Expression_Ex : string ; { Expression to evaluate with external symbols }
    Externals : boolean ;
    I, R : Extended ;
    Index : integer ;
    Symbol_Addr : int64 ;
    Symbol_Flg, Symbol_D_T, Symbol_Siz : integer ;
    Symbol_Data : pointer ;
    X : string ;
    Work, Work1 : string ;

begin // TCEF_Assembler.Evaluate_Ex
    { Setup... }
    X := string( Value ) ;
    Expression := '' ;
    Expression_Ex := '' ;
    Result := Set_Error( 0 ) ;
    Externals := False ;
    if( _CPU <> nil ) then
    begin
        if( ( _Assembler <> nil ) and ( _Assembler.Version > 21 ) ) then
        begin
             X := string( _Assembler.Normalize_Expression( Value, True, In_Assembly_Status ) ) ;
        end ;
    end ;

    { Parse each term }
    while( length( X ) > 0 ) do
    begin
        Work := Get_Next_Token( X ) ;
        if( pos( copy( Work, 1, 1 ), '0123456789' ) > 0 ) then { Numeric literal }
        begin
            Dummy := pos( '$', Work ) ;
            if( Dummy > 0 ) then
            begin
                X := copy( Work, Dummy, length( Work ) ) + X ;
                Work := copy( Work, 1, Dummy - 1 ) ;
            end ;
            Dummy := pos( '_', Work ) ;
            if( Dummy > 0 ) then
            begin
                X := copy( Work, Dummy, length( Work ) ) + X ;
                Work := copy( Work, 1, Dummy - 1 ) ;
            end ;
            Expression := Expression + ' ' + num2( Va( Work, ER ) ) ;
            Expression_Ex := Expression_Ex + ' ' + num2( Va( Work, ER ) ) ;
            if( ER ) then
            begin
                Result := Set_Error( CEFAssemblerErr_Illegal_Expression ) ;
                exit ;
            end ;
        end else
        if( ( copy( Work, 1, 1 ) = #39 ) or ( copy( Work, 1, 1 ) = '"' ) ) then // String literal
        begin
            Dummy := instr( 2, Work, copy( X, 1, 1 ) ) ;
            if( Dummy = 0 ) then
            begin
                Result := Set_Error( CEFAssemblerErr_Unterminated_String_Literal ) ;
                exit ;
            end ;
            Expression := Expression + ' ' + num2( Va( Work, ER ) ) ;
            Expression_Ex := Expression_Ex + ' ' + num2( Va( Work, ER ) ) ;
            if( ER ) then
            begin
                Result := Set_Error( CEFAssemblerErr_Illegal_Expression ) ;
                exit ;
            end ;
        end else
        if( Is_PC( Work ) ) then // PC
        begin
            Expression := Expression + ' ' + num2( PC + PC_Adjustment ) ;
            Expression_Ex := Expression_Ex + ' ' + num2( PC + PC_Adjustment ) ;
        end else { if length( Expression ) = 0 }
        if( Work = '&' ) then
        begin
            Expression := Expression + ' AND ' ;
            Expression_Ex := Expression_Ex + ' ' + Work + ' ' ;
        end else
        if( Work = '|' ) then
        begin
            Expression := Expression + ' OR ' ;
            Expression_Ex := Expression_Ex + ' ' + Work + ' ' ;
        end else
        if( copy( X, 1, 2 ) = '<<' ) then
        begin
            Expression := Expression + ' SHL ' ;
            Expression_Ex := Expression_Ex + ' ' + Work + ' ' ;
        end else
        if( copy( X, 1, 2 ) = '>>' ) then
        begin
            Expression := Expression + ' SHR ' ;
            Expression_Ex := Expression_Ex + ' ' + Work + ' ' ;
        end else
        if( Work = '~' ) then
        begin
            Expression := Expression + ' NOT ' ;
            Expression_Ex := Expression_Ex + ' ' + Work + ' ' ;
        end else
        if(
            ( Work = 'SHR' )
            or
            ( Work = 'SHL' )
            or
            ( Work = 'AND' )
            or
            ( Work = 'OR' )
            or
            ( Work = 'XOR' )
          ) then
        begin
            Expression := Expression + ' ' + Work + ' ' ;
            Expression_Ex := Expression_Ex + ' ' + Work + ' ' ;
        end else
        if( Hex_Literal( Work ) ) then
        begin
            Expression := Expression + Cvtb( 16, 10, Work ) ;
            Expression_Ex := Expression_Ex + Cvtb( 16, 10, Work ) ;
        end else
        if( pos( Work[ 1 ], Initial_Symbols ) > 0 ) then { Symbol }
        begin
            Err := _Find( Work, Symbol_Addr,
                Symbol_Flg, Symbol_D_T, Symbol_Siz, Symbol_Data ) ;
            if( Err <> 0 ) then // Not found
            begin
                Index := Macros.IndexOf( Work ) ;
                if( Index > -1 ) then // A macro
                begin
                    if( copy( X, 1, 1 ) = '(' ) then // ...with parameter(s)
                    begin
                        Work1 := Parse_Parameter( ')', X ) + ')' ;
                    end ;
                    if( Process_Macro( Work, _Status, Work1 ) ) then
                    begin
                        continue ; // error
                    end ;
                    Entry := TState( States[ States.Count - 1 ] ) ;
                    X := Entry.List[ 0 ] + X ;
                    Exit_State ;
                    continue ;
                end ;
                if( Valid_Base( Work, Base ) ) then
                begin
                    Expression := Expression + num2( Va( Work, ER ) ) ;
                    continue ;
                end ;
                if( Hex_Literal( Work ) ) then
                begin
                    Work := copy( Work, 1, Hex_Literal_Final ) ;
                    Expression := Expression + Cvtb( 16, 10, Work ) ;
                    continue ;
                end ;
                Result := Set_Error( CEFAssemblerErr_Undefined_Symbol ) ;
                exit ;
            end ; // if( Err <> 0 )
            if( ( Symbol_Flg and SF_External ) <> 0 ) then
            begin
                Externals := True ;
                Expression_Ex := Expression_Ex + ' ' + Work + ' ' ;
            end else
            begin
                Expression_Ex := Expression_Ex + num2( pSymbol_Record( Symbol_Data )^.Address ) ;
            end ;
            Expression := Expression + num2( pSymbol_Record( Symbol_Data )^.Address ) ;
        end else
        begin
            Expression := Expression + Work ;
            Expression_Ex := Expression_Ex + Work ;
        end ;
    end ; // while( length( X ) > 0 )

    Eval( Expression, R, I, Err, Dummy ) ;
    if( Err <> 0 ) then
    begin
	    Result := Set_Error( CEFAssemblerErr_Illegal_Expression ) ;
    end else
    begin
	    _Result := trunc( int( R ) ) ;
    end ;
    if( Externals ) then
    begin
        if( _Extension <> nil ) then
        begin
            Temp := Expression_Ex ;
            _Extension.External_Symbol( PChar( Temp ) ) ;
        end ;
    end ;
end ; // TCEF_Assembler.Evaluate


function ET( Code : integer ) : string ;

begin
    case Code of
        CEFAssemblerErr_Invalid_Digits : Result := 'Invalid digits' ;
        CEFAssemblerErr_Illegal_Expression : Result := 'Illegal expression' ;
        CEFAssemblerErr_Unterminated_String_Literal : Result := 'Unterminated string literal' ;
        CEFAssemblerErr_Undefined_Symbol : Result := 'Undefined symbol' ;
        CEFAssemblerErr_Illegal_Instruction : Result := 'Illegal Instruction' ;
    end ;
end ;


function TCEF_Assembler.Forward_Declare( Value : string ;
    const Input_Buffer : string ) : boolean ;

begin
    Value := uppercase( Value ) ;
    if( Forwards_Names = nil ) then
    begin
        Forwards_Names := TStringList.Create ;
        Forwards_Values := TStringList.Create ;
    end ;
    Result := ( Forwards_Names.Indexof( Value ) <> -1 ) ;
    Forwards_Names.Add( Value ) ;
    Forwards_Values.Add( Input_Buffer ) ;
end ;


function TCEF_Assembler.Grab_Current_Line : string ;

var Temp : string ;

begin
    Result := _Grab_Line( True ) ;
    Temp := Parse_Parameter( ';', Result ) ; // Trim comments
    Result := Temp ;
end ;


function Valid_Condition( Value : string ) : boolean ;

begin
    Result := False ;
    Value := Edit( Value, 8 or 32 or 128 ) ;
    if(
        ( Value = 'EQUAL' )
        or
        ( Value = 'EQ' )
        or
        ( Value = 'NOT_EQUAL' )
        or
        ( Value = 'NE' )
        or
        ( Value = 'GREATER' )
        or
        ( Value = 'G' )
        or
        ( Value = 'LESS_EQUAL' )
        or
        ( Value = 'LE' )
        or
        ( Value = 'LESS_THAN' )
        or
        ( Value = 'LT' )
        or
        ( Value = 'GREATER_EQUAL' )
        or
        ( Value = 'GE' )
        or
        ( Value = 'DEFINED' )
        or
        ( Value = 'DF' )
        or
        ( Value = 'NOT_DEFINED' )
        or
        ( Value = 'BDF' )
        or
        ( Value = 'BLANK' )
        or
        ( Value = 'B' )
        or
        ( Value = 'NOT_BLANK' )
        or
        ( Value = 'NB' )
        or
        ( Value = 'IDENTICAL' )
        or
        ( Value = 'IDN' )
        or
        ( Value = 'DIFFERENT' )
        or
        ( Value = 'DIF' )
      ) then
    begin
        Result := True ;
    end ;
end ; // Valid_Condition


function Create_COM_String_Stream( const S : string ) : TCOM_String_Stream ;

begin
    Result := TCOM_String_Stream.Create ;
    Result.Set_String( PChar( S ) ) ;
end ;


function Is_Directive( const Value, Directive : string ) : boolean ;

begin
    Result := ( Value = '.' + Directive ) ;
end ;


function TCEF_Assembler.Process_ASCII( S : string ;
    Status : TAssembler_Status ) : string ;

label ASCII_Loop ;

var A : integer ;
    C : char ;
    Ex : int64 ;
    In_Expression : boolean ;
    Quote : char ;
    _S, _T : integer ;
    Start : integer ;
    Temp : string ;
    UEC : TUnified_Exception ;

begin
    // Setup...
    Result := '' ;
    Quote := ' ' ;
    In_Expression := False ;
    Start := 0 ;

ASCII_Loop:
    S := Edit( S, 8 ) ; // Trim leading spaces
    if( length( S ) = 0 ) then
    begin
        Status.Log_Error( PChar( 'Unterminated literal' ), nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end ;
    for A := 1 to length( S ) do
    begin
        C := S[ A ] ;
        if( In_Expression ) then
        begin
            if( C <> '>' ) then // not done yet
            begin
                continue ;
            end ;
            In_Expression := False ;
            Temp := copy( S, Start + 1, A - Start - 1 ) ;
            UEC := Evaluate( PChar( Temp ), Ex ) ;
            if( UEC <> nil ) then
            begin
                Status.Log_Error( PChar( UEC.Error_Text( _S, _T ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Result := Result + chr( Ex ) ;
            continue ;
        end ;
        if( Quote = ' ' ) then // Not in a quote
        begin
            if( ( C = ' ' ) or ( C = HT ) ) then // Whitespace outside of quotes ends everything
            begin
                exit ;
            end ;
            if( C = '<' ) then
            begin
                Start := A ;
                In_Expression := True ;
                continue ;
            end else
            begin
                Quote := C ;
            end ;
            continue ;
        end else
        begin
            if( C = Quote ) then // End of quote
            begin
                Quote := ' ' ;
                continue ;
            end ;
        end ;
        Result := Result + C ;
    end ; // for A := 1 to length( S )
end ;


function TCEF_Assembler.Handle_Directives( var Value : string ;
    var Any_Code : boolean ; Status : TAssembler_Status ) : boolean ;

var First_Token : boolean ;
    Have_Value : boolean ;
    Remaining_Tokens : string ;
    Returned_Token : string ;

    function _Next_Token( Case_Sensitive, Peek : boolean ) : string ;

    var Dummy, E : integer ;
        _Remaining_Tokens, S : string ;

    begin
        if( Case_Sensitive ) then
        begin
            E := 0 ;
        end else
        begin
            E := 32 or 256 ;
        end ;
        if( length( Returned_Token ) > 0 ) then
        begin
            Result := Edit( Returned_Token, E ) ;
            if( not Peek ) then
            begin
                Returned_Token := '' ;
            end ;
            exit ;
        end ;
        _Remaining_Tokens := Edit( Remaining_Tokens, 8 ) ; // Strip leading spaces
        try
            if( not First_Token ) then
            begin
                S := Edit( Parse_Parameter( ';', _Remaining_Tokens ), 16 or 128 or 256 ) ; // Remove comments
                _Remaining_Tokens := S ;
            end ;
            if( not Have_Value ) then
            begin
                Result := Edit( _Get_Token, E ) ;
                exit ;
            end ;
            if( copy( _Remaining_Tokens, 1, 1 ) = ',' ) then
            begin
                Result := ',' ;
                _Remaining_Tokens := copy( _Remaining_Tokens, 2, length( _Remaining_Tokens ) ) ;
                exit ;
            end ;
            if(
                ( copy( _Remaining_Tokens, 1, 1 ) = '"' )
                or
                ( copy( _Remaining_Tokens, 1, 1 ) = #39 )
              ) then
            begin
                Dummy := instr( 2, _Remaining_Tokens, copy( _Remaining_Tokens, 1, 1 ) ) ;
                if( Dummy = 0 ) then // Unterminated quoted literal
                begin
                    Result := Edit( _Remaining_Tokens, E ) ; // Entire rest of line is the token
                    _Remaining_Tokens := '' ;
                end else
                begin
                    Result := Edit( copy( _Remaining_Tokens, 1, Dummy ), E ) ;
                    _Remaining_Tokens := copy( _Remaining_Tokens, Dummy + 1, length( _Remaining_Tokens ) ) ;
                end ;
                exit ;
            end ;
            Dummy := 2 ;
            if(
                ( length( _Remaining_Tokens ) > 0 )
                and
                ( pos( _Remaining_Tokens[ 1 ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$.' ) > 0 )
              ) then
            begin
                while( Dummy <= length( _Remaining_Tokens ) ) do
                begin
                    if( pos( _Remaining_Tokens[ Dummy ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$.' ) = 0 ) then
                    begin
                        break ;
                    end ;
                    inc( Dummy ) ;
                end ;
            end ;
(*
            Dummy := pos( ' ', _Remaining_Tokens + ' ' ) ;
            Dummy1 := pos( ',', _Remaining_Tokens + ',' ) ;
            if( Dummy1 < Dummy ) then
            begin
                Dummy := Dummy1 ;
            end ;
*)
            Result := Edit( copy( _Remaining_Tokens, 1, Dummy - 1 ), E ) ;
            _Remaining_Tokens := copy( _Remaining_Tokens, Dummy, length( _Remaining_Tokens ) ) ;
        finally
           if( not Peek ) then
           begin
               Remaining_Tokens := _Remaining_Tokens ;
           end ;
        end ;
    end ; // ._Next_Token


    function Get_Token( Case_Sensitive : boolean = False ) : string ;

    begin
        Result := _Next_Token( Case_Sensitive, False ) ;
    end ;


    procedure Put_Token( const S : string ) ;

    begin
        Returned_Token := S ;
    end ;


    function Grab_This_Line : string ;

    begin
        if( Have_Value ) then
        begin
            Result := '' ;
        end else
        begin
            Result := Grab_Current_Line ;
        end ;
        if( length( Remaining_Tokens ) > 0 ) then
        begin
            if( length( Result ) > 0 ) then
            begin
                Result := ' ' + Result ;
            end ;
            Result := Remaining_Tokens + Result ;
            Remaining_Tokens := '' ;
        end ;
        if( length( Returned_Token ) > 0 ) then
        begin
            if(
                ( length( Result ) > 0 )
                and
                ( pos( Returned_Token[ length( Returned_Token ) ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_abcdefghijklmnopqrstuvwxyz' ) > 0 )
              ) then
            begin
                Result := ' ' + Result ;
            end ;
            Result := Returned_Token + Result ;
            Returned_Token := '' ;
        end ;
    end ;


    function Peek_Token : string ;

    begin
        Result := _Next_Token( False, True ) ;
    end ;


var A : integer ;
    Align : int64 ;
    B : longint ;
    BB : string ;
    C, Dummy : integer ;
    D : double ;
    Entry : TState ;
    Ex : int64 ;
    Found : boolean ;
    In_Text : boolean ;
    Name, Arguments : string ;
    Nest_Level : integer ;   
    Next : string ;
    P : pSymbol_Record ;
    Parser : TString_Parser ;
    S, S1 : string ;
    SL, SL1 : TStringList ;
    Si : single ;
    Stream : TCOM_Stream ;
    T, Original_T : boolean ;
    Temp, Work : string ;
    UEC : TUnified_Exception ;

label Text_Loop ;

begin // TCEF_Assembler.Handle_Directives
    // Setup...
    First_Token := True ;
    Have_Value := ( length( Value ) > 0 ) ;
    Handle_Directives := False ;
    Remaining_Tokens := Edit( Value, 16 or 128 or 256 ) ;
    Value := Get_Token ;
    Temp := Value ;
    if( Value = ';' ) then // Comment started in first token
    begin
        exit ;
    end ;
    P := nil ;
    Next := Peek_Token ; // Peek at the next token on the line
    First_Token := False ;
    if(
        ( copy( Value, length( Value ), 1 ) <> ':' ) // Not a label
        and
        (
          ( Next = 'DB' ) or Is_Directive( Next, 'DB' ) or
          ( Next = 'DC' ) or Is_Directive( Next, 'DC' ) or
          Is_Directive( Next, 'BYTE' ) or
          ( Next = 'DEFB' ) or Is_Directive( Next, 'DEFB' ) or
          Is_Directive( Next, 'SIGNED_BYTE' )
          or
          Is_Directive( Next, 'ADDRESS' ) or Is_Directive( Next, 'BLKA' )
          or
          ( copy( Next, 1, 4 ) = '.REF' ) or
          Is_Directive( Next, 'ALIGN' ) or Is_Directive( Next, 'ASCII' ) or
          Is_Directive( Next, 'ASCIC' ) or Is_Directive( Next, 'ASCIZ' ) or
          Is_Directive( Next, 'BCD' )
          or
          ( Next = 'DW' ) or Is_Directive( Next, 'DW' ) or
          Is_Directive( Next, 'WORD' ) or ( Next = 'DEFW' ) or
          Is_Directive( Next, 'DEFW' ) or Is_Directive( Next, 'SIGNED_WORD' )
          or
          ( Next = 'DS' ) or Is_Directive( Next, 'DS' ) or
          ( Next = 'DEFS' ) or Is_Directive( Next, 'DEFS' ) or
          Is_Directive( Next, 'BLKW' ) or Is_Directive( Next, 'BLKB' ) or
          Is_Directive( Next, 'BLKL' ) or Is_Directive( Next, 'BLKQ' ) or
          Is_Directive( Next, 'BLKO' ) or Is_Directive( Next, 'BLOCK' ) or
          Is_Directive( Next, 'BLKD' ) or Is_Directive( Next, 'BLKF' )
          or
          ( Next = 'DEFM' ) or Is_Directive( Next, 'DEFM' ) or
          Is_Directive( Next, 'TEXT' )
          or
          Is_Directive( Next, 'DOUBLE' ) or Is_Directive( Next, 'D_FLOATING' )
          or
          Is_Directive( Next, 'LONG' )
          or
          Is_Directive( Next, 'PACKED' )
          or
          Is_Directive( Next, 'QUAD' )
          or
          Is_Directive( Next, 'SINGLE' )
          or
          Is_Directive( Next, 'S_FLOATING' )
          or
          Is_Directive( Next, 'FLOAT' )
          or
          Is_Directive( Next, 'S_FLOATING' )
          or
          Is_Directive( Next, 'RAD50' )
          or
          Is_Directive( Next, 'RADIX50' )
        )
      ) then // Data directive
    begin
        new( P ) ;
        fillchar( P^, sizeof( P^ ), 0 ) ;
        P^.Typ := 1 ; // Integer
        P^.Flags := SF_Constant ; // Constant
        P^.Data := PC ;
        UEC := Add_Symbol( PChar( Value ), P ) ;
        if( UEC <> nil ) then
        begin
            Status.Log_Error( PChar( SERT( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
        end ;
        Value := Get_Token ;
(*
    end else
    if( length( Next ) > 0 ) then
    begin
        Put_Token( Next ) ;
*)
    end ;

    if( Is_Directive( Value, 'ADDRESS' ) ) then
    begin
        if( _CPU = nil ) then
        begin
            Value := '.WORD' ;
        end else
        begin
            Dummy := XBits_For_Value( _CPU.CPU.Get_High_Virtual_Memory( 0 ) ) ;
            case Dummy of
                0..8 : Value := '.BYTE' ;
                9..16 : Value := '.WORD' ;
                17..32 : Value := '.LONG' ;
                else Value := '.QUAD' ;
            end ;
        end ;
    end ;
    if( Is_Directive( Value, 'BLKA' ) ) then
    begin
        if( _CPU = nil ) then
        begin
            Value := '.BLKW' ;
        end else
        begin
            Dummy := XBits_For_Value( _CPU.CPU.Get_High_Virtual_Memory( 0 ) ) ;
            case Dummy of
                0..8 : Value := '.BLKB' ;
                9..16 : Value := '.BLKW' ;
                17..32 : Value := '.BLKL' ;
                else Value := '.BLKQ' ;
            end ;
        end ;
    end ;

    if( copy( Value, 1, 4 ) = '.REF' ) then
    begin
        Map( PC ) ;
        Value := copy( Value, 5, length( Value ) ) ;
        if( Value = '1' ) then
        begin
            Value := '.BYTE' ;
        end else
        if( Value = '2' ) then
        begin
            Value := '.WORD' ;
        end else
        if( Value = '4' ) then
        begin
            Value := '.LONG' ;
        end else
        if( Value = '8' ) then
        begin
            Value := '.QUAD' ;
        end else
        if( Value = '16' ) then
        begin
            Value := '.OCTA' ;
        end else
        begin
            Value := '.REF' + Value ; // So error reports actual directive
        end ;
    end ;

    // Process standard directives...
    if( Value = '.' ) then
    begin
        Value := Get_Token ;
        if( Value <> '=' ) then
        begin
            Put_Token( Value ) ;
            Value := '' ;
            Handle_Directives := False ;
            exit ;
        end ;
        if( _Token_EOL ) then
        begin
            Value := '' ;
        end else
        begin
            Value := _Grab_Line( True ) ; // Get value
        end ;
        Value := Edit( Value, 8 or 16 or 128 or 256 ) ;
        if( length( Value ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'Missing expression' ), nil, Tokens.Token_Line, Severity_Error ) ;
            Handle_Directives := False ;
            exit ;
        end ;
        Dummy := pos( '.', Value ) ;
        while( Dummy > 0 ) do
        begin
            Value := copy( Value, 1, Dummy - 1 ) +
                CvtB( 10, 16, num2( PC ) ) + 'H ' +
                copy( Value, Dummy + 1, length( Value ) ) ;
            Dummy := pos( '.', Value ) ;
        end ;
        UEC := Evaluate( PChar( Value ), Ex ) ;
        Last_Error := UEC ;
        PC := Ex ;
        Value := '' ;
	    Handle_Directives := True ;
        Altered_PC := True ;
        Altered_Address := PC ;
    end else
    if( Is_Directive( Value, 'ALIGN' ) ) then
    begin
        Value := uppercase( Get_Token ) ;
        if( Value = 'BYTE' ) then
        begin
            Value := '0' ;
        end else
        if( Value = 'WORD' ) then
        begin
            Value := '1' ;
        end else
        if( Value = 'LONG' ) then
        begin
            Value := '2' ;
        end else
        if( Value = 'QUAD' ) then
        begin
            Value := '3' ;
        end else
        if( ( Value = 'OCTA' ) or ( Value = 'PARAGRAPH' ) ) then
        begin
            Value := '4' ;
        end else
        if( Value = 'PAGE' ) then
        begin
            if( _CPU <> nil ) then
            begin
                if( _CPU.CPU.Page_Size = 0 ) then
                begin
                    Value := '9' ;
                end else
                begin
                    Value := '' ;
                end ;
            end else
            begin
                Value := '9' ;
            end ;
        end ;
        if( Value = '' ) then
        begin
            Align := _CPU.CPU.Page_Size ;
        end else
        begin
            try
                Align := strtoint( Value ) ;
            except
                Status.Log_Error( PChar( 'Invalid alignment' ), nil, Tokens.Token_Line, Severity_Error ) ;
                Handle_Directives := False ;
                exit ;
            end ;
            if( ( Align < 0 ) or ( Align > 63 ) ) then
            begin
                Status.Log_Error( PChar( 'Invalid alignment' ), nil, Tokens.Token_Line, Severity_Error ) ;
                Handle_Directives := False ;
                exit ;
            end ;
            Align := Xbit_Values[ Align ] ;
            if( ( _CPU <> nil ) and ( Align > _CPU.CPU.Get_High_Virtual_Memory( 0 ) ) ) then
            begin
                Status.Log_Error( PChar( 'Invalid alignment' ), nil, Tokens.Token_Line, Severity_Error ) ;
                Handle_Directives := False ;
                exit ;
            end ;
        end ;
        Ex := PC div Align ;
        if( PC <> Ex * Align ) then // Adjustment is needed
        begin
            PC := ( Ex + 1 ) * Align ;
        end ;
        Value := '' ;
	    Handle_Directives := True ;
        Altered_PC := True ;
        Altered_Address := PC ;
    end else
    if( Is_Directive( Value, 'ASCII' ) ) then
    begin
        Map( PC ) ;
	    Handle_Directives := True ;
        Value := Process_ASCII( Edit( Grab_This_Line, 128 ), Status ) ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
    end else
    if( Is_Directive( Value, 'ASCIC' ) ) then
    begin
        Map( PC ) ;
	    Handle_Directives := True ;
        Value := Process_ASCII( Edit( Grab_This_Line, 128 ), Status ) ;
        Value := chr( length( Value ) ) + Value ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
        if( length( Value ) > 256 ) then
        begin
            Status.Log_Error( PChar( 'Counted string is too long' ), nil, Tokens.Token_Line, Severity_Error ) ;
            Handle_Directives := False ;
            exit ;
        end ;
    end else
    if( Is_Directive( Value, 'ASCIZ' ) ) then
    begin
        Map( PC ) ;
	    Handle_Directives := True ;
        Value := Process_ASCII( Edit( Grab_This_Line, 128 ), Status ) + chr( 0 ) ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
    end else
    if( Is_Directive( Value, 'BCD' ) ) then // BCD
    begin
        Map( PC ) ;
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        while( length( Work ) > 0 ) do
        begin
            S := Parse_Parameter( ',', Work ) ;
            try
                if( pos( '.', S ) + pos( 'E', S ) + pos( 'e', S ) > 0 ) then
                begin
                    D := strtofloat( S ) ;
                    S := Num2( D ) ;
                end else
                begin
                    Ex := strtoint( S ) ;
                    S := Num2( Ex ) ;
                end ;
            except
                Status.Log_Error( PChar( 'Invalid number' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            if( pos( '.', S ) > 0 ) then
            begin
                Status.Log_Error( PChar( 'Invalid BCD value' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Value := Value + String_To_BCD( S )
        end ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
    end else
    if( Is_Directive( Value, 'CODES' ) ) then
    begin
        List_Codes := True ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'NOCODES' ) ) then
    begin
        List_Codes := False ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'DEFINE' ) ) then
    begin
        Value := Get_Token ;
        if( length( Value ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'Missing identifier' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        new( P ) ;
        fillchar( P^, sizeof( P^ ), 0 ) ;
        P^.Address := Ex ;
        P^.Typ := 4 ; // Boolean
        P^.Flags := SF_Constant ; // Constant
        UEC := Add_Symbol( PChar( Value ), P ) ;
        if( UEC <> nil ) then
        begin
            Status.Log_Error( PChar( SERT( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
        end ;
	    Handle_Directives := ( UEC = nil ) ;
        Value := '' ;
    end else
    if(
        ( Value = 'DW' ) or Is_Directive( Value, 'DW' ) or
        Is_Directive( Value, 'WORD' ) or ( Value = 'DEFW' ) or
        Is_Directive( Value, 'DEFW' ) or Is_Directive( Value, 'SIGNED_WORD' )
      ) then
    begin
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        Map( PC ) ;
        while( length( Work ) > 0 ) do
        begin
            S := Parse_Parameter( ',', Work ) ;
            S1 := Parse_Parameter( ';', S ) ;
            if( S <> '' ) then // A comment began before the comma
            begin
                Work := '' ;
            end ;
            S := S1 ;
            UEC := Evaluate( PChar( S ), Ex ) ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = CEFAssemblerErr_Undefined_Symbol ) then
                begin
                    Add_Reference( PChar( S ), 2, PC ) ;
                end else
                begin
                    Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                    exit ;
                end ;
            end ;
            if( ( Ex > $FFFF ) or ( -Ex > $FFFF ) ) then
            begin
                Status.Log_Error( PChar( 'Word value out of range' ), nil, Tokens.Token_Line, Severity_Warning ) ;
            end ;
            setlength( S, 2 ) ;
            move( Ex, S[ 1 ], 2 ) ;
            Value := Value + S ;
        end ;
        if( ( _Assembler <> nil ) and ( _Assembler.Version > 25 ) ) then
        begin
            PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        end ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if(
        ( Value = 'DB' ) or Is_Directive( Value, 'DB' ) or
        Is_Directive( Value, 'BYTE' ) or ( Value = 'DEFB' ) or
        Is_Directive( Value, 'DEFB' ) or Is_Directive( Value, 'SIGNED_BYTE' )
      ) then
    begin
        Work := Edit( Grab_This_Line, 128 ) ; // Get the whole line
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ; // New line
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
	    if( length( S ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        Work := S ;
        Value := '' ;
        Map( PC ) ;
        while( length( Work ) > 0 ) do
        begin
            S := Edit( Parse_Parameter( ',', Work ), 8 or 128 ) ;
            if( ( copy( S, 1, 1 ) = '"' ) or ( copy( S, 1, 1 ) = #39 ) ) then
            begin
                if( S[ Length( S ) ] <> S[ 1 ] ) then
                begin
                    Status.Log_Error( PChar( 'No terminating delimeter' ), nil, Tokens.Token_Line, Severity_Error ) ;
                    exit ;
                end ;
                Value := Value + copy( S, 2, length( S ) - 2 ) ;
            end else
            begin
                UEC := Evaluate( PChar( S ), Ex ) ;
                if( UEC <> nil ) then
                begin
                    if( UEC.Get_Error = CEFAssemblerErr_Undefined_Symbol ) then
                    begin
                        Add_Reference( PChar( S ), 1, PC ) ;
                    end else
                    begin
                        Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                        exit ;
                    end ;
                end ;
                if( ( Ex > 255 ) or ( Ex < -128 ) ) then
                begin
                    Status.Log_Error( PChar( 'Byte value out of range' ), nil, Tokens.Token_Line, Severity_Warning ) ;
                    exit ;
                end ;
                Value := Value + chr( Ex ) ;
            end ;
        end ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if( ( Value = 'DC' ) or Is_Directive( Value, 'DC' ) ) then
    begin
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        Map( PC ) ;
        while( length( Work ) > 0 ) do
        begin
            S := Edit( Parse_Parameter( ',', Work ), 8 or 128 ) ;
            UEC := Evaluate( PChar( S ), Ex ) ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = CEFAssemblerErr_Undefined_Symbol ) then
                begin
                    Add_Reference( PChar( S ), 1, PC ) ;
                end else
                begin
                    Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                    exit ;
                end ;
            end ;
            S := chr( Ex ) ;
            Ex := Ex shr 8 ;
            while( EX > 0 ) do
            begin
                S := S + chr( Ex ) ;
                Ex := Ex shr 8 ;
            end ;
            if( Big_Endian ) then
            begin
                S := Reverse_String( S ) ;
            end ;
            Value := Value + S ;
        end ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if( ( Value = 'DS' ) or Is_Directive( Value, 'DS' ) or
        ( Value = 'DEFS' ) or Is_Directive( Value, 'DEFS' ) or
        Is_Directive( Value, 'BLKW' ) or Is_Directive( Value, 'BLKB' ) or
        Is_Directive( Value, 'BLKL' ) or Is_Directive( Value, 'BLKQ' ) or
        Is_Directive( Value, 'BLKO' ) or Is_Directive( Value, 'BLOCK' ) or
        Is_Directive( Value, 'BLKD' ) or Is_Directive( Value, 'BLKF' )
      ) then
    begin
        Map( PC ) ;
        UEC := Evaluate( PChar( Get_Token ), Ex ) ; //Should be Grab_Line
        if( UEC <> nil ) then
        begin
            exit ;
        end ;
        if( Is_Directive( Value, 'BLKW' ) ) then
        begin
            PC := _Assembler.Request_Data( PC, EX * 2 ) ;
            if( P <> nil ) then
            begin
                P^.Address := PC ;
            end ;
        end else
        if( Is_Directive( Value, 'BLKL' ) or Is_Directive( Value, 'BLKF' ) ) then
        begin
            PC := _Assembler.Request_Data( PC, Ex * 4 ) ;
            if( P <> nil ) then
            begin
                P^.Address := PC ;
            end ;
        end else
        if( Is_Directive( Value, 'BLKQ' ) or Is_Directive( Value, 'BLKD' ) ) then
        begin
            PC := _Assembler.Request_Data( PC, Ex * 8 ) ;
            if( P <> nil ) then
            begin
                P^.Address := PC ;
            end ;
        end else
        if( Is_Directive( Value, 'BLKO' ) ) then
        begin
            PC := _Assembler.Request_Data( PC, Ex * 16 ) ;
            if( P <> nil ) then
            begin
                P^.Address := PC ;
            end ;
        end else
        begin
            PC := _Assembler.Request_Data( PC, Ex ) ;
            if( P <> nil ) then
            begin
                P^.Address := PC ;
            end ;
        end ;
	    Handle_Directives := True ;
        Altered_PC := True ;
        Altered_Address := PC ;
        Value := '' ; // No data, just unused space
    end else
    if(
        ( Value = 'DEFM' )
        or
        Is_Directive( Value, 'DEFM' )
        or
        Is_Directive( Value, 'TEXT' )
      ) then
    begin
        In_Text := Is_Directive( Value, 'TEXT' ) ;
        Map( PC ) ;
        BB := Get_Token ;
        if( _CPU <> nil ) then
        begin
            if( _Assembler.Version > 21 ) then
            begin
                BB := string( _Assembler.Normalize_Expression( PChar( BB ), False, Status ) ) ;
            end ;
        end ;
        if( ( BB[ 1 ] <> '"' ) and ( BB[ 1 ] <> #39 ) ) then
        begin
            Status.Log_Error( PChar( 'Illegal value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        if( BB[ Length( BB ) ] <> BB[ 1 ] ) then
	    begin
            Status.Log_Error( PChar( 'No terminating delimeter' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;

        Value := '' ;
Text_Loop:
        for A := 2 to Length( Bb ) - 1 do
        begin
            if Bb[ A ] = Bb[ 1 ] then
            begin
                if( ( A = Length( Bb ) ) or ( Bb[ A + 1 ] = ';' ) ) or ( BB[ A + 1 ] = '\' ) then
                begin
                    break ;
                end ;
                Bb := Copy( Bb, A + 1, Length( Bb ) - A ) ;
                goto Text_Loop ;
            end ;
            Value := Value + Bb[ A ] ;
        end ;
        if( In_Text ) then // .TEXT can include C-type escape codes
        begin
            A := 1 ;
            while( A < length( BB ) ) do
            begin
                if( BB[ A ] = '\' ) then // Start of escape sequence
                begin
                    if( copy( BB, A + 1, 1 ) = '\' ) then // \\
                    begin
                        BB := copy( BB, 1, A - 1 ) + copy( BB, A, length( BB ) ) ;
                    end else
                    if( copy( BB, A + 1, 1 ) = 'b' ) then // \b
                    begin
                        BB := copy( BB, 1, A - 1 ) + BS + copy( BB, A + 1, length( BB ) ) ;
                    end else
                    if( copy( BB, A + 1, 1 ) = 'f' ) then // \f
                    begin
                        BB := copy( BB, 1, A - 1 ) + FF + copy( BB, A + 1, length( BB ) ) ;
                    end else
                    if( copy( BB, A + 1, 1 ) = 'r' ) then // \r
                    begin
                        BB := copy( BB, 1, A - 1 ) + CR + copy( BB, A + 1, length( BB ) ) ;
                    end else
                    if( copy( BB, A + 1, 1 ) = 'n' ) then // \n
                    begin
                        BB := copy( BB, 1, A - 1 ) + LF + copy( BB, A + 1, length( BB ) ) ;
                    end else
                    if( copy( BB, A + 1, 1 ) = 't' ) then // \t
                    begin
                        BB := copy( BB, 1, A - 1 ) + HT + copy( BB, A + 1, length( BB ) ) ;
                    end else
                    if( pos( copy( BB, A + 1, 1 ), '0123' ) > 0 ) then // Octal value
                    begin
                        Work := copy( BB, A, 3 ) ;
                        try
                            if( Valid_Base( Work, 8 ) ) then
                            begin
                                Dummy := strtoint( CVTB( 8, 10, Work ) ) ;
                                Work := chr( Dummy ) ;
                            end ;
                        except
                        end ;
                        BB := copy( BB, 1, A - 1 ) + Work + copy( BB, A + 4, length( BB ) ) ;
                    end ;
                end ; // if( BB[ A ] = '\' )
                inc( A ) ;
            end ; // while( A < length( BB ) )
        end ; // if( In_Text )
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if( Is_Directive( Value, 'DOUBLE' ) or Is_Directive( Value, 'D_FLOATING' ) ) then // Double-precision floating point
    begin
        Map( PC ) ;
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        while( length( Work ) > 0 ) do
        begin
            S := Parse_Parameter( ',', Work ) ;
            try
                D := strtofloat( S ) ;
            except
                Status.Log_Error( PChar( 'Illegal double value' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            setlength( S, sizeof( D ) ) ;
            move( D, S[ 1 ], sizeof( D ) ) ;
            Value := Value + S ;
        end ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if( Is_Directive( Value, 'ELSE' ) ) then
    begin
        Status.Log_Error( '.ELSE outside of .IF block', nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end else
    if( Is_Directive( Value, 'EJECT' ) or ( Value = 'EJECT' ) ) then
    begin
        Status.Output_To_Listing( '', ListOut_New_Page ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'END' ) or ( Value = 'END' ) ) then
    begin
        Terminated := True ;
        if( not _Token_EOL ) then
        begin
            Value := Get_Token ;
            UEC := Evaluate( PChar( Value ), Ex ) ;
            if( UEC <> nil ) then
            begin
                Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Start_Address := Ex ;
            Have_Start_Address := True ;
        end ;
	    Handle_Directives := True ;
        Value := Get_Token ;
        while( Value <> '' ) do
        begin
            if( Value = ';' ) then
            begin
                Grab_Current_Line ;
                Value := Get_Token ;
            end else
            begin
                Status.Log_Error( PChar( 'Lines after .END ignored' ), nil, Tokens.Token_Line, Severity_Warning ) ;
                Value := '' ;
            end ;
        end ;
        Value := '' ;
    end else
    if Is_Directive( Value, 'ENDC' ) then
    begin
        Status.Log_Error( '.ENDC outside of .IF block', nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end else
    if Is_Directive( Value, 'ENDM' ) then
    begin
        Status.Log_Error( '.ENDM outside of .MACRO definition', nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end else
    if Is_Directive( Value, 'ENDR' ) then
    begin
        Status.Log_Error( '.ENDR outside of .REPEAT block', nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end else
    if Is_Directive( Value, 'EOT' ) then // Null directive
    begin
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if Is_Directive( Value, 'ERROR' ) then
    begin
        Value := Grab_This_Line ;
        Work := Parse_Parameter( ';', Value ) ;
        Value := Work ;
        Work := Parse_Parameter( '\', Value ) ;
        if( length( Value ) > 0 ) then
        begin
            Put_Token( Value ) ;
        end ;
        if( length( Work ) > 0 ) then
        begin
            UEC := Evaluate( PChar( Work ), Ex ) ;
            if( UEC <> nil ) then
            begin
                Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Work := inttostr( Ex ) ;
        end ;
        Status.Log_Error( PChar( Work ), nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end else
    if( Is_Directive( Value, 'EVEN' ) ) then
    begin
        if Odd( Pc ) then
        begin
            Pc := Pc + 1 ;
            Altered_PC := True ;
            Altered_Address := PC ;
        end ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Value + Next = '#IF' ) then
    begin
        // Get the first operand
        Value := Get_Token ; // Swallow IF
        Value := Edit( Grab_This_Line, 8 or 128 ) ;
        if( copy( Value, 1, 1 ) <> '(' ) then
        begin
            Status.Log_Error( PChar( 'Expected "("' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        Value := copy( Value, 2, length( Value ) ) ;
        Parser := TString_Parser.Create ;
        try
            Parser.Set_Source( Value ) ;
            Value := Parser.Token ;
            Nest_Level := 0 ;
            if( Value = '[' ) then
            begin
                inc( Nest_Level ) ;
            end ;
            while( Nest_Level > 0 ) do
            begin
                S := Parser.Token ;
                Value := Value + ' ' + S ;
                if( S = ']' ) then
                begin
                    dec( Nest_Level ) ;
                end else
                if( S = '[' ) then
                begin
                    inc( Nest_Level ) ;
                end ;
            end ;

            // Get Operator...
            S := Parser.Token ;
            if( S = '!' ) then
            begin
                S := '!' + Parser.Token ;
            end else
            if( S = '=' ) then
            begin
                S := Parser.Token ;
                if( S = '=' ) then
                begin
                    S := Parser.Token ;
                    if( S <> ':' ) then
                    begin
                        Parser.Put_Token( S ) ;
                    end ;
                    S := '==' ;
                end else
                begin
                    if( S <> ':' ) then
                    begin
                        Parser.Put_Token( S ) ;
                    end ;
                    S := '=' ;
                end ;
            end else
            if( S = '>' ) then
            begin
                S := Parser.Token ;
                if( S = '=' ) then
                begin
                    S := '>=' ;
                end else
                begin
                    Parser.Put_Token( S ) ;
                    S := '>' ;
                end ;
            end else
            if( S = '<' ) then
            begin
                S := Parser.Token ;
                if( S = '=' ) then
                begin
                    S := '<=' ;
                end else
                begin
                    Parser.Put_Token( S ) ;
                    S := '<' ;
                end ;
            end ;
            if(
                ( S <> '!=' )
                and
                ( S <> '==' )
                and
                ( S <> '<=' )
                and
                ( S <> '>=' )
                and
                ( S <> '<' )
                and
                ( S <> '>' )
              ) then
            begin
                Status.Log_Error( PChar( 'Invalid conditional operator' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;

            // Get second operand...
            Work := Parser.Token ;
            Nest_Level := 0 ;
            if( Work = '[' ) then
            begin
                inc( Nest_Level ) ;
            end ;
            while( Nest_Level > 0 ) do
            begin
                BB := Parser.Token ;
                Work := Work + ' ' + BB ;
                if( BB = ']' ) then
                begin
                    dec( Nest_Level ) ;
                end else
                if( BB = '[' ) then
                begin
                    inc( Nest_Level ) ;
                end ;
            end ;

            if( Parser.Token <> ']' ) then // Parser converts it to "]"
            begin
                Status.Log_Error( PChar( 'Missing ")"' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
        finally
            Parser.Free ;
        end ;

        // Process the conditional...
        Have_Value := False ;
        T := Process_If1( Value, S, Work, Status ) ;
        SL := TStringList.Create ;
        Work := uppercase( Get_Token ) ;
        Next := uppercase( Peek_Token ) ;
        Nest_Level := 0 ;
        while( Work <> '' ) do
        begin
            if( Work + Next = '#IF' ) then
            begin
                inc( Nest_Level ) ;
            end else
            if( Work + Next = '#ENDIF' ) then
            begin
                if( Nest_Level > 0 ) then
                begin
                    dec( Nest_Level ) ;
                end else
                begin
                    break ;
                end ;
            end ;
            if( T ) then
            begin
                SL.Add( Work + ' ' + Grab_This_Line ) ;
            end else
            begin
                Grab_This_Line ;
            end ;
            Work := uppercase( Get_Token ) ;
            Next := uppercase( Peek_Token ) ;
        end ; // while( Work <> '' )
        if( SL.Count > 0 ) then
        begin
            Enter_State( State_If, '', SL, nil ) ;
        end else
        begin
            SL.Free ;
        end ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'IF' ) ) then
    begin
        Value := Get_Token ;
        S := Grab_This_Line ;
        Work := Parse_Parameter( ';', S ) ;
        S := Work ;
        Work := Parse_Parameter( '\', S ) ;
        if( length( Value ) > 0 ) then
        begin
            Put_Token( Value ) ;
        end ;
        if( not Valid_Condition( Value ) ) then
        begin
            Status.Log_Error( PChar( 'Invalid condition' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        T := Process_If( Value, Work, Status ) ;
        Original_T := T ;
        Have_Value := False ;
        SL := TStringList.Create ;
        Work := uppercase( Get_Token ) ;
        Nest_Level := 0 ;
        Found := False ; // .ELSE not found
        while( Work <> '' ) do
        begin
            if( Is_Directive( Work, 'IF' ) ) then
            begin
                inc( Nest_Level ) ;
            end else
            if(
                ( Is_Directive( Work, 'IF_TRUE' ) or Is_Directive( Work, 'IFT' ) )
                and
                ( Nest_Level = 0 )
              ) then
            begin
                T := Original_T ;
                Work := uppercase( Get_Token ) ;
                continue ;
            end else
            if(
                ( Is_Directive( Work, 'IF_FALSE' ) or Is_Directive( Work, 'IFF' ) )
                and
                ( Nest_Level = 0 )
              ) then
            begin
                T := not Original_T ;
                Work := uppercase( Get_Token ) ;
                continue ;
            end else
            if(
                ( Is_Directive( Work, 'IF_TRUE_FALSE' ) or Is_Directive( Work, 'IFTF' ) )
                and
                ( Nest_Level = 0 )
              ) then
            begin
                T := True ;
                Work := uppercase( Get_Token ) ;
                continue ;
            end else
            if( Is_Directive( Work, 'ELSE' ) and ( Nest_Level = 0 ) ) then
            begin
                T := not Original_T ;
                Work := uppercase( Get_Token ) ;
                if( Found ) then // Already an ELSE at this level
                begin
                    Status.Log_Error( PChar( 'Multiple .ELSE directives' ), nil, Tokens.Token_Line, Severity_Warning ) ;
                end ;
                Found := True ;
                continue ;
            end else
            if( Is_Directive( Work, 'ENDC' ) ) then
            begin
                if( Nest_Level > 0 ) then
                begin
                    dec( Nest_Level ) ;
                end else
                begin
                    break ;
                end ;
            end ;
            if( T ) then
            begin
                SL.Add( Work + ' ' + Grab_This_Line ) ;
            end else
            begin
                Grab_This_Line ;
            end ;
            Work := uppercase( Get_Token ) ;
        end ; // while( Work <> '' )
	    Handle_Directives := True ;
        Value := '' ;
        if( SL.Count > 0 ) then
        begin
            Enter_State( State_If, '', SL, nil ) ;
        end else
        begin
            SL.Free ;
        end ;
    end else
    if( copy( Value, 1, 4 ) = '.IF_' ) then
    begin
        Status.Log_Error( 'Subconditional outside of .IF block', nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end else
    if( Is_Directive( Value, 'IIF' ) ) then
    begin
        Value := uppercase( Get_Token ) ;
        S := Get_Token ;
        if( S = ',' ) then
        begin
            S := Get_Token ;
        end ;
        if( not Valid_Condition( Value ) ) then
        begin
            Status.Log_Error( PChar( 'Invalid condition' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        if( not Process_If( Value, S, Status ) ) then
        begin
            Grab_This_Line ; // Ignore the line
        end else
        begin
            S := Get_Token ;
            if( S <> ',' ) then
            begin
                Status.Log_Error( PChar( 'Missing comma' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            S := Grab_This_Line ;
            In_Line( Create_COM_String_Stream( S ) ) ;
        end ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'INCLUDE' ) ) then
    begin
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'Missing include value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        Stream := _UI.Get_File_Stream( PChar( Work ) ) ;
        if( Stream = nil ) then
        begin
            exit ;
        end ;
        Enter_State( State_Include, Work, nil, Stream ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'IRP' ) or Is_Directive( Value, 'IRPC' ) ) then
    begin
        Map( PC ) ;
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'Missing parameter(s)' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( ',', Work ) ;
        if( copy( Work, 1, 1 ) = '<' ) then
        begin
            Work := copy( Work, 2, length( Work ) ) ;
            if( copy( Work, length( Work ), 1 ) <> '>' ) then
            begin
                Status.Log_Error( PChar( 'Syntax error' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            setlength( Work, length( Work ) - 1 ) ;
        end else
        if( copy( Work, 1, 1 ) = '^' ) then
        begin
            Work := copy( Work, 2, length( Work ) ) ;
            if( copy( Work, length( Work ), 1 ) <> copy( Work, 1, 1 ) ) then
            begin
                Status.Log_Error( PChar( 'Syntax error' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Work := copy( Work, 2, length( Work ) ) ;
            setlength( Work, length( Work ) - 1 ) ;
        end ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'Invalid parameter' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := S + ',' + Work ;
        Have_Value := False ;

        // Read entire block into stringlist SL...
        SL := TStringList.Create ;
        Work := uppercase( Get_Token ) ;
        Nest_Level := 0 ;
        while( Work <> '' ) do
        begin
            if(
                Is_Directive( Work, 'REPEAT' )
                or
                Is_Directive( Work, 'REPT' )
                or
                Is_Directive( Work, 'IRP' )
                or
                Is_Directive( Work, 'IRPC' )
              ) then
            begin
                inc( Nest_Level ) ;
            end ;
            if( Is_Directive( Work, 'ENDR' ) ) then
            begin
                if( Nest_Level > 0 ) then
                begin
                    dec( Nest_Level ) ;
                end else
                begin
                    break ;
                end ;
            end ;
            SL.Add( Work + ' ' + Grab_This_Line ) ;
            Work := uppercase( Get_Token ) ;
        end ; // while( not Token.EOF )

        // Create string list from repeat...
        SL1 := TStringList.Create ;
        Work := Parse_Parameter( ',', S ) ; // Work is symbol
        if( Is_Directive( Value, 'IRP' ) ) then
        begin
            // IRP...
            while( length( S ) > 0 ) do
            begin
                Temp := Parse_Parameter( ',', S ) ; // This parameter
                Process_IRP( Work, Temp, SL, SL1 ) ;
            end ;
        end else
        begin
            // IRPC...
            for Dummy := 1 to length( S ) do
            begin
                Process_IRP( Work, #39 + S[ Dummy ] + #39, SL, SL1 ) ;
            end ;
        end ;
        Enter_State( State_IRP, '', SL1, nil ) ;

        // Cleanup...
        SL.Free ;
        Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'LONG' ) ) then
    begin
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        Map( PC ) ;
        while( length( Work ) > 0 ) do
        begin
            S := Parse_Parameter( ',', Work ) ;
            UEC := Evaluate( PChar( S ), Ex ) ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = CEFAssemblerErr_Undefined_Symbol ) then
                begin
                    Add_Reference( PChar( S ), 4, PC ) ;
                end else
                begin
                    Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                    exit ;
                end ;
            end ;
            if( ( Ex > $FFFFFFFF ) or ( -Ex > $FFFFFFFF ) ) then
            begin
                Status.Log_Error( PChar( 'Illegal long value' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            setlength( S, 4 ) ;
            move( Ex, S[ 1 ], 4 ) ;
            Value := Value + S ;
        end ; // while( length( Work ) > 0 )
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if( Is_Directive( Value, 'LSFIRST' ) ) then
    begin
        Big_Endian := False ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'MACRO' ) ) then
    begin
        Arguments := trim( Grab_This_Line ) ;
        if( length( Arguments ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'Missing macro name' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        Name := Parse_Parameter( ' ', Arguments ) ;
        Have_Value := False ;
        SL := TStringList.Create ;
        Work := uppercase( Get_Token ) ;
        Nest_Level := 0 ;
        while( Work <> '' ) do
        begin
            if( Is_Directive( Work, 'MACRO' ) ) then
            begin
                inc( Nest_Level ) ;
            end ;
            if( Is_Directive( Work, 'ENDM' ) ) then
            begin
                if( Nest_Level > 0 ) then
                begin
                    dec( Nest_Level ) ;
                end else
                begin
                    break ;
                end ;
            end ;
            SL.Add( Work + ' ' + Grab_This_Line ) ;
            Work := uppercase( Get_Token ) ;
        end ; // while( Work <> '' )
        Define_Macro( Name, Arguments, SL ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Value + Next = '#DEFINE' ) then // Alternate macro definition form
    begin
        Name := Get_Token ; // Get "DEFINE"
        Name := Get_Token( True ) ; // Get macro name
        if( length( Name ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'Missing macro name' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;

        Value := Grab_This_Line ;

        // Find end of name/beginning of parameters...
        Dummy := 1 ;
        while( Dummy <= length( Name ) ) do
        begin
            if( pos( Name[ Dummy ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_abcdefghijklmnopqrstuvwxyz' ) = 0 ) then
            begin
                break ; // Found end of name
            end ;
            inc( Dummy ) ;
        end ;

        Value := Edit( copy( Name, Dummy, length( Name ) ) + Value, 8 or 128 ) ;
        Name := copy( Name, 1, Dummy - 1 ) ;
        Arguments := '' ;
        if( copy( Value, 1, 1 ) = '(' ) then
        begin
            Value := copy( Value, 2, length( Value ) ) ;
            Dummy := pos( ')', Value ) ;
            if( Dummy = 0 ) then
            begin
                Status.Log_Error( PChar( 'Missing right-parenthesis' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Arguments := copy( Value, 1, Dummy - 1 ) ;
            Value := copy( Value, Dummy + 1, length( Value ) ) ;
        end ;

        Define_Macro_SL( Edit( Name, 32 ), Arguments, Value ) ;
	     Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'MDELETE' ) ) then
    begin
        Value := uppercase( Grab_This_Line ) ;
        if( Macros.IndexOf( Value ) <> -1 ) then
        begin
            Delete_Macro( Value ) ;
            Handle_Directives := True ;
        end else
        begin
            Status.Log_Error( PChar( 'Undefined macro' ), nil, Tokens.Token_Line, Severity_Error ) ;
        end ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'MEXIT' ) ) then
    begin
        // See if we are inside any macro...
        Found := False ;
        for A := 0 to States.Count - 1 do
        begin
            Entry := TState( States[ States.Count - 1 ] ) ;
            if( Entry.State = State_Macro ) then
            begin
                Found := True ;
                break ;
            end ;
        end ;
        if( not Found ) then
        begin
            Status.Log_Error( PChar( 'Exit when not in macro' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;

        // Now pop all states back to, and including, the last macro
        while( True ) do
        begin
            Entry := TState( States[ States.Count - 1 ] ) ;
            if( Entry.State = State_Macro ) then
            begin
                Exit_State ;
                break ;
            end ;
            Exit_State ;
        end ;
    end else
    if( Is_Directive( Value, 'MSFIRST' ) ) then
    begin
        Big_Endian := True ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'REM' ) ) then
    begin
	    Handle_Directives := True ;
        Value := trim( Grab_This_Line ) ;
        Value := copy( Value, 1, 1 ) ; // Get delimiter
        Dummy := 0 ;
        while( Dummy = 0 ) do
        begin
            Work := Get_Token ;
            if( Work = '' ) then // End of input
            begin
                break ;
            end ;
            Work := Work + ' ' + Grab_This_Line ;
            Dummy := pos( Value, Work ) ;
        end ;
        Work := trim( copy( Work, Dummy + 1, length( Work ) ) ) ;
        if( length( Work ) > 0 ) then
        begin
            Remaining_Tokens := Work ;
        end ;
    end else
    if( Is_Directive( Value, 'SET_TARGET_CPU' ) ) then
    begin
        Value := Grab_This_Line ;
        Work := Parse_Parameter( ';', Value ) ;
        Value := Work ;
        Work := Parse_Parameter( '\', Value ) ;
        if( length( Value ) > 0 ) then
        begin
            Put_Token( Value ) ;
        end ;
        if( Work = '' ) then
        begin
            Status.Log_Error( 'Missing target', nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        Dummy := CPU_List.IndexOf( uppercase( Work ) ) ;
        if( Dummy = -1 ) then
        begin
            Status.Log_Error( 'Unknown target', nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        Set_Target_CPU( TComponent( CPU_List.Objects[ Dummy ] ) ) ;
        try
            // Try to set CPU to the current address...
            _CPU.CPU.Set_Current_Address( 0, True, PC ) ;
            Altered_PC := True ;
            Altered_Address := PC ;
	    except
        end ;
        Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'TITLE' ) ) then
    begin
        Work := Grab_This_Line ;
        Status.Output_To_Listing( PChar( Work ), ListOut_Title_Text ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'NARG' ) ) then
    begin
        Status.Log_Error( PChar( '.NARG when not in macro' ), nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end else
    if( Is_Directive( Value, 'NCHR' ) ) then
    begin
        Value := Edit( Grab_This_Line, 8 or 128 ) ;
        if( ( length( Value ) = 0 ) or ( Value[ 1 ] = ',' ) ) then
        begin
            Status.Log_Error( 'Missing symbol', nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        Dummy := pos( ',', Value ) ;
        if( Dummy = 0 ) then
        begin
            Status.Log_Error( 'Missing symbol', nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Edit( copy( Value, Dummy + 1, length( Value ) ), 8 or 128 ) ; // String
        Value := Edit( copy( Value, 1, Dummy - 1 ), 8 or 128 ) ; // Symbol name
        if( copy( S, 1, 1 ) = '<' ) then
        begin
            S := copy( S, 2, length( S ) ) ;
            if( copy( S, length( S ), 1 ) <> '>' ) then
            begin
                Status.Log_Error( 'Unterminated string literal', nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            S := copy( S, 1, length( S ) - 1 ) ;
        end else
        if( copy( S, 1, 1 ) = '^' ) then
        begin
            S := copy( S, 2, length( S ) ) ;
            if( copy( S, length( S ), 1 ) <> copy( S, 1, 1 ) ) then
            begin
                Status.Log_Error( PChar( 'Syntax error' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            S := copy( S, 2, length( S ) ) ;
            setlength( S, length( S ) - 1 ) ;
        end ;
        new( P ) ;
        fillchar( P^, sizeof( P^ ), 0 ) ;
        P^.Address := length( S ) ;
        P^.Typ := 1 ; // Integer
        P^.Flags := SF_Constant ; // Constant
        P^.Data := length( S ) ; // Value
        UEC := Add_Symbol( PChar( Value ), P ) ;
        if( UEC <> nil ) then
        begin
            Status.Log_Error( PChar( SERT( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'ODD' ) ) then
    begin
        if not Odd( Pc ) then
        begin
            Pc := Pc + 1 ;
            Altered_PC := True ;
            Altered_Address := PC ;
        end ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'PACKED' ) ) then // Packed BCD
    begin
        Map( PC ) ;
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        while( length( Work ) > 0 ) do
        begin
            S := Parse_Parameter( ',', Work ) ;
            try
                if( pos( '.', S ) + pos( 'E', S ) + pos( 'e', S ) > 0 ) then
                begin
                    D := strtofloat( S ) ;
                    S := Num2( D ) ;
                end else
                begin
                    Ex := strtoint( S ) ;
                    S := Num2( Ex ) ;
                end ;
            except
                Status.Log_Error( PChar( 'Invalid number' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            if( pos( '.', S ) > 0 ) then
            begin
                Status.Log_Error( PChar( 'Invalid BCD value' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Value := Value + String_To_Packed_BCD( S ) ;
            PC := _Assembler.Request_Data( PC, length( Value ) ) ;
            if( P <> nil ) then
            begin
                P^.Address := PC ;
            end ;
            Pc := Pc + length( Value ) ;
        end ;
    end else
    if( Is_Directive( Value, 'PAGE' ) ) then
    begin
        Status.Output_To_Listing( '', ListOut_Paging ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'PSECT' ) ) then
    begin
        Work := Grab_This_Line ;
	    Handle_Directives := True ;
        if( Work <> '' ) then
        begin
            Dummy := pos( ',', Work + ',' ) ;
            Value := copy( Work, 1, Dummy - 1 ) ;
            Work := copy( Work, Dummy + 1, length( Work ) ) ;
            if( Value <> '' ) then
            begin
                Map( PC ) ;
                new( P ) ;
                fillchar( P^, sizeof( P^ ), 0 ) ;
                P^.Address := PC ;
                P^.Typ := 1 ; // integer
                P^.Flags := SF_Constant ; // Constant
                UEC := Add_Symbol( PChar( Value ), P ) ;
                if( UEC <> nil ) then
                begin
                    Status.Log_Error( PChar( SERT( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                end ;
                Handle_Directives := ( UEC = nil ) ;
            end ;
        end ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'NOPAGE' ) ) then
    begin
        Status.Output_To_Listing( '', ListOut_No_Paging ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'QUAD' ) ) then
    begin
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        Map( PC ) ;
        while( length( Work ) > 0 ) do
        begin
            S := Parse_Parameter( ',', Work ) ;
            UEC := Evaluate( PChar( S ), Ex ) ;
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = CEFAssemblerErr_Undefined_Symbol ) then
                begin
                    Add_Reference( PChar( S ), 8, PC ) ;
                end else
                begin
                    Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                    exit ;
                end ;
            end ;
            setlength( S, 8 ) ;
            move( Ex, S[ 1 ], 8 ) ;
            Value := Value + S ;
        end ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if( Is_Directive( Value, 'RADIX' ) or Is_Directive( Value, 'BASE' ) or ( Value = 'BASE' ) ) then // Base conversion
    begin
        Value := Get_Token ;
        if( Value = copy( 'BINARY', 1, length( Value ) ) ) then
        begin
            Value := '2' ;
        end else
        if( Value = copy( 'OCTAL', 1, length( Value ) ) ) then
        begin
            Value := '8' ;
        end else
        if( Value = copy( 'DECIMAL', 1, length( Value ) ) ) then
        begin
            Value := '10' ;
        end else
        if( Value = copy( 'HEXADECIMAL', 1, length( Value ) ) ) then
        begin
            Value := '16' ;
        end ;
        Val( Value, B, C ) ;
        Value := '' ;
        if( ( C <> 0 ) or ( B < 2 ) or ( B > 49 ) ) then
	    begin
            Status.Log_Error( PChar( 'Invalid base' ), nil, Tokens.Token_Line, Severity_Error ) ;
            Err := True ;
            exit ;
        end ;
        Base := B ;
	    Handle_Directives := True ;
    end else
    if( Is_Directive( Value, 'RADIX50' ) or Is_Directive( Value, 'RAD50' )  ) then
    begin
        Map( PC ) ;
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        while( length( Work ) > 0 ) do
        begin
            S := Parse_Parameter( ',', Work ) ;
            if( not Valid_Radix50( S ) ) then
            begin
                Status.Log_Error( PChar( 'Illegal Radix-50 value' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Value := Value + ASCII_To_RAD( S ) ;
        end ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if( Is_Directive( Value, 'REPEAT' ) or Is_Directive( Value, 'REPT' ) ) then
    begin
        Map( PC ) ;
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'Missing repeat value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        UEC := Evaluate( PChar( Work ), Ex ) ;
        if( UEC <> nil ) then
        begin
            Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        Have_Value := False ;
        SL := TStringList.Create ;
        Work := uppercase( Get_Token ) ;
        Nest_Level := 0 ;
        while( Work <> '' ) do
        begin
            if(
                Is_Directive( Work, 'REPEAT' )
                or
                Is_Directive( Work, 'REPT' )
                or
                Is_Directive( Work, 'IRP' )
                or
                Is_Directive( Work, 'IRPC' )
              ) then
            begin
                inc( Nest_Level ) ;
            end ;
            if( Is_Directive( Work, 'ENDR' ) ) then
            begin
                if( Nest_Level > 0 ) then
                begin
                    dec( Nest_Level ) ;
                end else
                begin
                    break ;
                end ;
            end ;
            SL.Add( Work + ' ' + Grab_This_Line ) ;
            Work := uppercase( Get_Token ) ;
        end ; // while( not Token.EOF )
        Enter_State( State_Repeat, inttostr( Ex ), SL, nil ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if(
        Is_Directive( Value, 'SINGLE' )
        or
        Is_Directive( Value, 'S_FLOATING' )
        or
        Is_Directive( Value, 'FLOAT' )
        or
        Is_Directive( Value, 'S_FLOATING' )
      ) then // Single-precision floating point
    begin
        Map( PC ) ;
        Work := Grab_This_Line ;
        if( length( Work ) = 0 ) then
        begin
            Status.Log_Error( PChar( 'missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := Parse_Parameter( '\', Work ) ;
        if( length( Work ) > 0 ) then
        begin
            Put_Token( Work ) ;
        end ;
        Work := S ;
        Value := '' ;
        while( length( Work ) > 0 ) do
        begin
            S := Parse_Parameter( ',', Work ) ;
            try
                Si := strtofloat( S ) ;
            except
                Status.Log_Error( PChar( 'Illegal double value' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            setlength( S, sizeof( Si ) ) ;
            move( Si, S[ 1 ], sizeof( Si ) ) ;
            Value := Value + S ;
        end ;
        PC := _Assembler.Request_Data( PC, length( Value ) ) ;
        if( P <> nil ) then
        begin
            P^.Address := PC ;
        end ;
        Pc := Pc + length( Value ) ;
	    Handle_Directives := True ;
    end else
    if( Is_Directive( Value, 'SBTTL' ) ) then
    begin
        Work := Grab_This_Line ;
        Status.Output_To_Listing( PChar( Work ), ListOut_SubTitle ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'SYMBOL' ) or ( Value = 'SYMBOL' ) ) then
    begin
        Want_Symbol_Table_List := True ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'UNDEFINE' ) ) then
    begin
        Value := Get_Token ;
	    if( length( Value ) = 0 ) then
	    begin
            Status.Log_Error( PChar( 'Missing identifier' ), nil, Tokens.Token_Line, Severity_Error ) ;
	        exit ;
	    end ;
        Assembler_Context.Delete( PChar( Value ) ) ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'WARN' ) or Is_Directive( Value, 'WARNING' ) ) then
    begin
        Value := Grab_This_Line ;
        Work := Parse_Parameter( ';', Value ) ;
        Value := Work ;
        Work := Parse_Parameter( '\', Value ) ;
        if( length( Value ) > 0 ) then
        begin
            Put_Token( Value ) ;
        end ;
        if( length( Work ) > 0 ) then
        begin
            UEC := Evaluate( PChar( Work ), Ex ) ;
            if( UEC <> nil ) then
            begin
                Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            Work := inttostr( Ex ) ;
        end ;
        Status.Log_Error( PChar( Work ), nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end else
    if( Is_Directive( Value, 'XREF' ) or ( Value = 'XREF' ) ) then
    begin
        Want_XRef_List := True ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_Directive( Value, 'NOXREF' ) ) then
    begin
        Want_XRef_List := False ;
	    Handle_Directives := True ;
        Value := '' ;
    end else
    if( Is_PC( Value ) ) then { PC adjustment }
    begin
        Fedit( Value, Value, 511 ) ; { Format it }
        if( copy( Value, 2, 1 ) <> '=' ) then
        begin
            exit ;
        end ;
        Value := Copy( Value, 3, Length( Value ) - 2 ) ;
        UEC := Evaluate( PChar( Value ), Ex ) ;
        Value := '' ;
        if( UEC <> nil ) then
        begin
            Status.Log_Error( PChar( ET( UEC.Get_Error ) + Value ), nil, Tokens.Token_Line, Severity_Error ) ;
            Err := True ;
            exit ;
        end ;
        PC := Ex ;
        Altered_PC := True ;
        Altered_Address := PC ;
	    Handle_Directives := True ;
    end else
    begin
        if( ( Next = ':' ) and ( pos( ':', Value ) = 0 ) ) then
        begin
            Value := Value + ':' ;
            Get_Token ;
        end ;
        if( pos( ':', Value ) = length( Value ) ) then // Label
        begin
            Value := copy( Value, 1, length( Value ) - 1 ) ;
            new( P ) ;
            fillchar( P^, sizeof( P^ ), 0 ) ;
            P^.Address := PC ;
            P^.Typ := 1 ; // Integer
            P^.Flags := SF_Constant or SF_Label ; // Constant label
            P^.Data := PC ;
            UEC := Add_Symbol( PChar( Value ), P ) ;
            Value := '' ;
            if( UEC <> nil ) then
            begin
                Status.Log_Error( PChar( SERT( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
            end ;
            Handle_Directives := ( UEC = nil ) ;
        end ;
    end ;
    if( Result ) then
    begin
        Status.Data := Status.Data + length( Value ) ;
    end ;
end ; { TCEF_Assembler.Handle_Directives }


procedure TCEF_Assembler.Enter_State( State : integer ; const Context : string ;
    List : TStringList ; Stream : TCom_Stream ) ;

var Entry : TState ;
    Stream_File : PStreamer_File ;

begin
    Entry := TState.Create ;
    States.Add( Entry ) ;
    Entry.State := State ;
    Entry.Context := Context ;
    Entry.List := List ;
    case State of
        State_Repeat, State_Macro, State_IRP, State_If :
            Entry.Tokens := TStringList_Parser.Create( List ) ;
        State_Include, State_Inline :
            begin
                Entry.Tokens := TFile_Parser.Create ;
                new( Stream_File, Init( nil, False, False, 1 ) ) ;
                Stream_File^.Stream := Stream ;
                TFile_Parser( Entry.Tokens ).Set_Source( Stream_File ) ;
            end ;
    end ; // case State
end ;


function TCEF_Assembler._Next_Token : string ;

var Entry : TState ;
    I : integer ;

begin
    if( States.Count > 0 ) then
    begin
        Entry := TState( States[ States.Count - 1 ] ) ;
        Result := Entry.Tokens.Token ;
        if( Result = '' ) then // No more tokens
        begin
            if( Entry.State = State_Repeat ) then
            begin
                I := strtoint( Entry.Context ) ;
                I := I - 1 ;
                if( I > 0 ) then
                begin
                    Entry.Context := inttostr( I ) ;
                    TStringList_Parser( Entry.Tokens ).Reset ;
                    Result := _Get_Token ;
                    exit ;
                end ;
            end ;
            Exit_State ;
            Result := _Get_Token ;
        end else
        if( Result = '.' ) then
        begin
            Result := '.' + Entry.Tokens.Token ;
        end ;
        if( Result <> '' ) then
        begin
            if( States.Count > 0 ) then // Previous get_token may have popped us out of our state
            begin
                Entry := TState( States[ States.Count - 1 ] ) ; // Make sure we have the proper state
                Entry.Tokens.Put_Token( Result ) ;
            end else
            begin
                Tokens.Put_Token( Result ) ;
            end ;
        end ;
        exit ;
    end ;
    try
        Result := Tokens.Token ;
        if( Result <> '' ) then
        begin
            Tokens.Put_Token( Result ) ;
        end ;
    except
        Result := '' ;
    end ;
end ; // TCEF_Assembler._Next_Token


function TCEF_Assembler._Find( Sym : string ; var Addr : int64 ;
    var Flg, D_T, Siz : longint ; var Dat : pointer ) : integer ;

var S : string ;

begin
    Result := Assembler_Context.Find( PChar( Sym ), Addr, Flg, D_T, Siz, Dat ) ;
    if( ( Result = 0 ) and ( not Inhibit_XRef_Add ) ) then // Found the symbol
    begin
        S :=  inttostr( Tokens.Token_Line ) ;
        while( length( S ) < 5 ) do
        begin
            S := ' '  + S ;
        end ;
        XRefs.Add( Sym + '|#' + S ) ;
    end ;
end ;


function TCEF_Assembler._Get_Token : string ;

var Entry : TState ;
    I : integer ;

begin
    if( States.Count > 0 ) then
    begin
        Entry := TState( States[ States.Count - 1 ] ) ;
        Result := Entry.Tokens.Token ;
        if( Result = '' ) then
        begin
            if( Entry.State = State_Repeat ) then
            begin
                I := strtoint( Entry.Context ) ;
                I := I - 1 ;
                if( I > 0 ) then
                begin
                    Entry.Context := inttostr( I ) ;
                    TStringList_Parser( Entry.Tokens ).Reset ;
                    Result := _Get_Token ;
                    exit ;
                end ;
            end ;
            Exit_State ;
            Result := _Get_Token ;
        end else
        if( Result = '.' ) then
        begin
            Result := '.' + Entry.Tokens.Token ;
        end ;
        exit ;
    end ;
    try
        Result := Tokens.Token ;
    except
        Result := '' ;
    end ;
end ;


procedure TCEF_Assembler.Exit_State ;

var Index : integer ;

begin
    Index := States.Count - 1 ;
    TState( States[ Index ] ).Free ;
    States.Delete( Index ) ;
end ;


function TCEF_Assembler._Grab_Line( Current : boolean ) : string ;

var Entry : TState ;
    I : integer ;

begin
    if( States.Count > 0 ) then
    begin
        Entry := TState( States[ States.Count - 1 ] ) ;
        Result := Entry.Tokens.Grab_Line ;
        if( Result = '' ) then
        begin
            if( Entry.State = State_Repeat ) then
            begin
                I := strtoint( Entry.Context ) ;
                I := I - 1 ;
                if( I = 0 ) then
                begin
                    Exit_State ;
                end else
                begin
                    Entry.Context := inttostr( I ) ;
                    TStringList_Parser( Entry.Tokens ).Reset ;
                end ;
            end else
            begin
                Exit_State ;
            end ;
            Result := _Grab_Line( Current ) ;
        end ;
        exit ;
    end ; // if( States.Count > 0 )

    if( Tokens.Token_EOL and Current ) then
    begin
        Result := '' ;
    end else
    begin
        Result := Tokens.Grab_Line ; // Get value
    end ;
end ; // TCEF_Assembler._Grab_Line


procedure TCEF_Assembler._Put_Token( const Token : string ) ;

var Entry : TState ;

begin
    if( States.Count > 0 ) then
    begin
        Entry := TState( States[ States.Count - 1 ] ) ;
        Entry.Tokens.Put_Token( Token ) ;
        exit ;
    end ;
    Tokens.Put_Token( Token ) ;
end ;


function TCEF_Assembler._Token_EOL : boolean ;

var Entry : TState ;

begin
    if( States.Count > 0 ) then
    begin
        Entry := TState( States[ States.Count - 1 ] ) ;
        Result := Entry.Tokens.Token_EOL ;
    end else
    begin
        Result := Tokens.Token_EOL ;
    end ;
end ;


function TCEF_Assembler.Process_If1( Argument1, Condition, Argument2 : string ;
    Status : TAssembler_Status ) : boolean ;

var UEC : TUnified_Exception ;
    Value1, Value2 : int64 ;

begin
    Result := False ;
    UEC := Evaluate( PChar( Argument1 ), Value1 ) ;
    if( UEC <> nil ) then
    begin
        Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end ;
    UEC := Evaluate( PChar( Argument2 ), Value2 ) ;
    if( UEC <> nil ) then
    begin
        Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
        exit ;
    end ;
    if( Condition = '!=' ) then
    begin
        Result := Value1 <> Value2 ;
    end else
    if( Condition = '>=' ) then
    begin
        Result := Value1 >= Value2 ;
    end else
    if( Condition = '<=' ) then
    begin
        Result := Value1 <= Value2 ;
    end else
    if( Condition = '>' ) then
    begin
        Result := Value1 > Value2 ;
    end else
    if( Condition = '<' ) then
    begin
        Result := Value1 < Value2 ;
    end else
    begin
        Result := Value1 = Value2 ;
    end ;
end ;


function TCEF_Assembler.Process_If( Condition, Argument : string ;
    Status : TAssembler_Status ) : boolean ;

var Dummy : integer ;
    Negate : boolean ;
    S : string ;
    Symbol_D_T, Symbol_Siz, Symbol_Flg : integer ;
    Symbol_Addr : int64 ;
    Symbol_Data : pointer ;
    UEC : TUnified_Exception ;
    Value : int64 ;

begin
    Result := False ;
    Negate := False ;
    Condition := Edit( Condition, 8 or 32 or 128 ) ;
    if( ( Condition = 'NOT_EQUAL' ) or ( Condition = 'NE' ) ) then
    begin
        Negate := True ;
        Condition := 'EQUAL' ;
    end else
    if( ( Condition = 'LESS_EQUAL' ) or ( Condition = 'LE' ) ) then
    begin
        Negate := True ;
        Condition := 'GREATER' ;
    end else
    if( ( Condition = 'GREATER_EQUAL' ) or ( Condition = 'GE' ) ) then
    begin
        Negate := True ;
        Condition := 'LESS_THAN' ;
    end else
    if( ( Condition = 'NOT_DEFINED' ) or ( Condition = 'NDF' ) ) then
    begin
        Negate := True ;
        Condition := 'DEFINED' ;
    end else
    if( ( Condition = 'NOT_BLANK' ) or ( Condition = 'NB' ) ) then
    begin
        Negate := True ;
        Condition := 'BLANK' ;
    end ;
    if( ( Condition = 'DIFFERENT' ) or ( Condition = 'DIF' ) ) then
    begin
        Negate := True ;
        Condition := 'IDENTICAL' ;
    end ;

    if( ( Condition = 'EQUAL' ) or ( Condition = 'EQ' ) ) then
    begin
        UEC := Evaluate( PChar( Argument ), Value ) ;
        if( UEC = nil ) then
        begin
            Result := ( Value = 0 ) ;
        end else
        begin
            Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
    end else
    if( ( Condition = 'GREATER' ) or ( Condition = 'GT' ) ) then
    begin
        UEC := Evaluate( PChar( Argument ), Value ) ;
        if( UEC = nil ) then
        begin
            Result := ( Value > 0 ) ;
        end else
        begin
            Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
    end else
    if( ( Condition = 'LESS_THAN' ) or ( Condition = 'LT' ) ) then
    begin
        UEC := Evaluate( PChar( Argument ), Value ) ;
        if( UEC = nil ) then
        begin
            Result := ( Value < 0 ) ;
        end else
        begin
            Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
    end else
    if( ( Condition = 'DEFINED' ) or ( Condition = 'DF' ) ) then
    begin
        Result := ( _Find( Argument, Symbol_Addr,
            Symbol_Flg, Symbol_D_T, Symbol_Siz, Symbol_Data ) = 0 ) ;
    end else
    if( ( Condition = 'BLANK' ) or ( Condition = 'B' ) ) then
    begin
        Result := ( Edit( Argument, -1 ) = '' ) ;
    end ;
    if( ( Condition = 'IDENTICAL' ) or ( Condition = 'IDN' ) ) then
    begin
        Dummy := pos( ',', Argument ) ;
        if( Dummy = 0 ) then
        begin
            Status.Log_Error( 'Missing argument', nil, Tokens.Token_Line, Severity_Error ) ;
            exit ;
        end ;
        S := copy( Argument, 1, Dummy - 1 ) ;
        Argument := copy( Argument, Dummy + 1, length( Argument ) ) ;
        Result := ( Edit( Argument, -1 ) = Edit( S, -1 ) ) ;
    end ;
    if( Negate ) then
    begin
        Result := not Result ;
    end ;
end ; // TCEF_Assembler.Process_If


procedure TCEF_Assembler.Process_IRP( Symbol, Value : string ;
    Source, Target : TStringList ) ;

var Dummy : integer ;
    S : string ; // Argument values
    Parser : TString_Parser ;
    Work_S : string ; // Current argument being processed

begin
    // Setup...
    Symbol := Edit( Symbol, 32 or 128 ) ;
    Parser := TString_Parser.Create ;

    // Make copy of macro string list...
    for Dummy := 0 to Source.Count - 1 do // For each line in macro
    begin
        Work_S := Source[ Dummy ] ;
        S := Parse_Parameter( ';', Work_S ) ;
        Work_S := S ;
        S := Parse_Parameter( '\', Work_S ) ;
        if( length( Work_S ) > 0 ) then
        begin
            _Put_Token( Work_S ) ;
        end ;
        Work_S := '' ;
        if( length( S ) > 0 ) then // Non-blank line
        begin
            Parser.Set_Source( S ) ;
            S := Parser.Token ;
            while( length( S ) > 0 ) do // Parse through all tokens in this line
            begin
                if( uppercase( S ) = Symbol ) then // An argument found
                begin
                    S := Value ;
                end ;
                if( length( Work_S ) > 0 ) then
                begin
                    Work_S := Work_S + ' ' ; // Space to separate tokens
                end ;
                Work_S := Work_S + S ;
                S := Parser.Token ;
            end ; // while( length( S ) > 0 )
        end ; // if( length( S ) > 0 )
        Target.Add( Work_S ) ;
    end ; // for Dummy := 0 to Source.Count - 1
    Parser.Free ;
end ; // TCEF_Assembler.Process_IRP


procedure TCEF_Assembler.Set_Target_CPU( CPU : TComponent ) ;

var P : PChar ;
    PL : longint ;

begin
    if( _CPU <> nil ) then
    begin
        Expand( PChar( '.UNDEFINE CPU_VER' + inttostr( _CPU.Version ) ), P, PL, _Status ) ;
    end ;
    _CPU := CPU ;
    Expand( PChar( '.DEFINE CPU_VER' + inttostr( _CPU.Version ) ), P, PL, _Status ) ;
    Big_Endian := _CPU.CPU.Big_Endian ;

    _Assembler := _CPU.CPU.Get_Assembler( Self ) ;
    _Assembler.Initialize( self ) ;
    P := _Assembler.Valid_Symbol_Initial ;
    if( P <> nil ) then
    begin
        Initial_Symbols := string( P ) ;
    end ;
    P := _Assembler.Valid_Symbol_After ;
    if( P <> nil ) then
    begin
        Valid_Symbols := string( P ) ;
    end ;
end ;


// API...

procedure TCEF_Assembler.Set_Assembler_Context( Value : TCEF_Assembler_Context ) ;

begin
    _Assembler_Context := Value ;
end ;


function TCEF_Assembler.Get_Assembler_Context : TCEF_Assembler_Context ;

begin
    if( _Assembler_Context = nil ) then
    begin
        _Assembler_Context := DLL_Get_Assembler_Context ;
    end ;
    Result := _Assembler_Context ;
end ;


procedure TCEF_Assembler.Define_Macro( Name : string ;
    const Arguments : string ; SL : TStringList ) ;

var Index : integer ;

begin
    Name := uppercase( Name ) ;
    Index := Macros.IndexOf( Name ) ;
    if( Index = -1 ) then // New definition
    begin
        Macros.AddObject( Name, SL ) ;
        Macro_Arguments.Add( Arguments ) ;
    end else
    begin
        TStringList( Macros.Objects[ Index ] ).Free ;
        Macros.Objects[ Index ] := SL ;
        Macro_Arguments[ Index ] := Arguments ;
    end ;
end ;


procedure TCEF_Assembler.Define_Macro_SL( Name : string ;
    Arguments : string ; S : string ) ;

var Index : integer ;
    SL : TStringList ;

begin
    if( Arguments <> '' ) then
    begin
        Arguments := '(' + Arguments ; // This marks it as the alternate form
    end ;
    SL := TStringList.Create ;
    SL.Add( S ) ;
    Name := uppercase( Name ) ;
    Index := Macros.IndexOf( Name ) ;
    if( Index = -1 ) then // New definition
    begin
        Macros.AddObject( Name, SL ) ;
        Macro_Arguments.Add( Arguments ) ;
    end else
    begin
        TStringList( Macros.Objects[ Index ] ).Free ;
        Macros.Objects[ Index ] := SL ;
        Macro_Arguments[ Index ] := Arguments ;
    end ;
end ;


procedure TCEF_Assembler.Delete_Macro( Name : string ) ;

var Index : integer ;

begin
    Name := uppercase( Name ) ;
    Index := Macros.IndexOf( Name ) ;
    if( Index <> -1 ) then
    begin
        TStringList( Macros.Objects[ Index ] ).Free ;
        Macros.Delete( Index ) ;
        Macro_Arguments.Delete( Index ) ;
    end ;
end ;


function TCEF_Assembler.Process_Macro( const Name : string ;
    Status : TAssembler_Status ; const Passed_Parameters : string ) : boolean ;

var Case_Sensitive : boolean ;
    Dummy, I, Idx, Index : integer ;
    Args, Default, S, Upper_S : string ; // Argument values
    Parser : TString_Parser ;
    SL, Temp, Arg_Names, Defaults, XLate : TStringList ;
    UEC : TUnified_Exception ;
    Value : int64 ;

    function Find_First_Match( S : string ; var Idx : integer ) : integer ;

    var Loop, Temp : integer ;
        Best, Best_Index : integer ;

    begin
        Best := length( S ) + 1 ;
        Best_Index := -1 ;
        for Loop := 0 to Arg_Names.Count - 1 do
        begin
            Temp := instr( Idx, S, Arg_Names[ Loop ] ) ;
            if( ( Temp > 0 ) and ( Temp < Best ) ) then
            begin
                Best := Temp ;
                Best_Index := Loop ;
            end ;
        end ;
        if( Best_Index = -1 ) then
        begin
            Result := -1 ;
            Idx := 0 ;
        end else
        begin
            Result := Best_Index ;
            Idx := Best ;
        end ;
    end ;

var Work, Work_Arg, Work_S : string ; // Current argument being processed

begin
    // Setup...
    Result := False ; // Assume no errors
    Index := Macros.IndexOf( Name ) ;
    if( Index = -1 ) then
    begin
        Status.Log_Error( 'Unknown macro', nil, Tokens.Token_Line, Severity_Error ) ;
        Result := True ;
        exit ;
    end ;
    XLate := TStringList.Create ; // Used to substitute arguments in macro
    Arg_Names := TStringList.Create ;
    Defaults := TStringList.Create ;
    Parser := TString_Parser.Create ;

    // Validate arguments...
    try
        if( Passed_Parameters = '' ) then
        begin
            S := Edit( _Grab_Line( True ), 8 or 128 ) ; // Trim leading/trailing spaces
            Work := Parse_Parameter( ';', S ) ;
            S := Work ;
            Work := Parse_Parameter( '\', S ) ;
            if( length( S ) > 0 ) then
            begin
                _Put_Token( S ) ;
            end ;
            S := Work ;
        end else
        begin
            S := Edit( Passed_Parameters, 8 or 128 ) ; // Trim leading/trailing spaces
        end ;
        Args := Macro_Arguments[ Index ] ;
        Case_Sensitive := ( copy( Args, 1, 1 ) = '(' ) ;
        if( Case_Sensitive ) then
        begin
            Args := copy( Args, 2, length( Args ) ) ; // Strip indicator
            if( ( copy( S, 1, 1 ) <> '(' ) and ( copy( S, 1, 1 ) <> '[' ) ) then
            begin
                Status.Log_Error( 'Expected parameter list', nil, Tokens.Token_Line, Severity_Error ) ;
                Result := True ;
                exit ;
            end ;
            S := copy( S, 2, length( S ) ) ;
            Dummy := pos( ')', S ) ;
            if( Dummy = 0 ) then
            begin
                Status.Log_Error( 'Missing right-parenthesis', nil, Tokens.Token_Line, Severity_Error ) ;
                Result := True ;
                exit ;
            end ;
            S := copy( S, 1, Dummy - 1 ) ;
        end ;
        while( ( length( S ) > 0 ) and ( length( Args ) > 0 ) ) do
        begin
            // Parse arguments...
            Work_Arg := Parse_Parameter( ',', Args ) ;
            if( not Case_Sensitive ) then
            begin
                Work_Arg := uppercase( Work_Arg ) ;
            end ;
            Dummy := pos( '=', Work_Arg + '=' ) ;
            Default := Edit( copy( Work_Arg, Dummy + 1, length( Work_Arg ) ), 8 or 128 ) ; // Formal argument default value
            Work_Arg := Edit( copy( Work_Arg, 1, Dummy - 1 ), 8 or 128 ) ; // Formal argument name
            Work_S := Edit( Parse_Parameter( ',', S ), 8 or 128 ) ; // Next actual argument
            if( Arg_Names.IndexOf( Work_Arg ) <> -1 ) then
            begin
                Status.Log_Error( 'Duplicate formal arguments', nil, Tokens.Token_Line, Severity_Error ) ;
                Result := True ;
                exit ;
            end ;

            // Handle symbol numeric value translations
            if( copy( Work_S, 1, 1 ) = '\' ) then
            begin
                Work_S := copy( Work_S, 2, length( Work_S ) ) ;
                UEC := Evaluate( PChar( Work_S ), Value ) ;
                if( UEC <> nil ) then
                begin
                    Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                    Result := True ;
                    exit ;
                end ;
                Work_S := inttostr( Value ) ;
            end ;

            // Handle created local labels
            if( copy( Work_Arg, 1, 1 ) = '?' ) then
            begin
                Work_Arg := copy( Work_Arg, 2, length( Work_Arg ) ) ;
                if( length( Default ) = 0 ) then
                begin
                    Default := inttostr( Current_Created_Local_Label ) + '$' ;
                    inc( Current_Created_Local_Label ) ;
                end ;
            end ;

            // Store arguments in lists...
            Arg_Names.Add( Work_Arg ) ; // Formal argument name
            XLate.Add( Work_S ) ; // Actual argument
            Defaults.Add( Default ) ; // Formal argument default value
        end ; // while( ( length( S ) > 0 ) and ( length( Args ) > 0 ) )
        if( S <> '' ) then // User supplied more arguments than macro has
        begin
            Status.Log_Error( 'Too many macro arguments', nil, Tokens.Token_Line, Severity_Error ) ;
            Result := True ;
            exit ;
        end ;
        if( Args <> '' ) then // Not all arguments supplied
        begin
            Status.Log_Error( 'Not enough macro arguments', nil, Tokens.Token_Line, Severity_Error ) ;
            Result := True ;
            exit ;
        end ;

        // Default blank actual arguments...
        for I := 0 to XLate.Count - 1 do
        begin
            if( ( XLate[ I ] = '' ) and ( Defaults.Count > I  ) ) then
            begin
                XLate[ I ] := Defaults[ I ] ;
            end ;
        end ;

        // Make copy of macro string list...
        SL := TStringList.Create ;
        Temp := TStringList( Macros.Objects[ Index ] ) ;
        for Dummy := 0 to Temp.Count - 1 do // For each line in macro
        begin
            Work_S := Temp[ Dummy ] ;
            if( XLate.Count > 0 ) then
            begin
                S := Parse_Parameter( ';', Work_S ) ;
(*
                Work_S := S ;
                S := Parse_Parameter( '\', Work_S ) ;
                if( length( Work_S ) > 0 ) then
                begin
                    _Put_Token( Work_S ) ;
                end ;
*)
                Work_S := '' ;
                if( length( S ) > 0 ) then
                begin
                    // Do argument concatenation...
                    I := pos( #39, S ) ;
                    while( I <> 0 ) do
                    begin
                        Index := instr( I + 1, S + #39, #39 ) ; // Find ending apostrophe (or end of line)
                        Work := copy( S, I + 1, Index - I - 1 ) ;
                        if( Case_Sensitive ) then
                        begin
                            Idx := Arg_Names.IndexOf( Work ) ;
                        end else
                        begin
                            Idx := Arg_Names.IndexOf( uppercase( Work ) ) ;
                        end ;
                        if( Idx = -1 ) then // Not a formal argument
                        begin
                            I := Index + 1 ; // Skip past ending apostrophe
                        end else
                        begin
                            S := copy( S, 1, I - 1 ) + XLate[ Idx ] + copy( S, Index + 1, length( S ) ) ;
                            I := I + length( XLate[ Idx ] ) ;
                        end ;
                        I := instr( I, S, #39 ) ; // Find next apostrophe
                    end ;

                    // Do argument substitution
                    if( Case_Sensitive ) then
                    begin
                        Idx := 1 ;
                        I := Find_First_Match( S, Idx ) ;
                        while( I >= 0 ) do
                        begin
                            S := copy( S, 1, Idx - 1 ) + XLate[ I ] + copy( S, Idx + length( Arg_Names[ I ] ), length( S ) ) ;
                            Idx := Idx + length( XLate[ I ] ) ;
                            I := Find_First_Match( S, Idx ) ;
                        end ;
                        Work_S := S ;
                    end else
                    begin
                        Parser.Set_Source( S ) ;
                        S := Parser.Token ;
                        while( length( S ) > 0 ) do // Parse through all tokens in this line
                        begin
                            if( S = '.' ) then
                            begin
                                S := S + Parser.Token ;
                            end ;
                            Upper_S := uppercase( S ) ;
                            I := Arg_Names.Indexof( Upper_S ) ;
                            if( I <> -1 ) then // An argument found
                            begin
                                S := XLate[ I ] ;
                            end else
                            if( Is_Directive( Upper_S, 'NARG' ) ) then
                            begin
                                Arg_Names.Add( uppercase( Parser.Token ) ) ;
                                XLate.Add( inttostr( Defaults.Count ) ) ;
                                S := Parser.Token ;
                                continue ;
                            end ;
                            if( length( Work_S ) > 0 ) then
                            begin
                                Work_S := Work_S + ' ' ; // Space to separate tokens
                            end ;
                            Work_S := Work_S + S ;
                            S := Parser.Token ;
                        end ; // while( length( S ) > 0 )
                    end ; // if( Case_Sensitive )
                end ; // if( XLate.Count > 0 )
            end ; // if( length( S ) > 0 )
            SL.Add( Work_S ) ;
        end ; // for Dummy := 0 to Temp.Count - 1
    finally
        XLate.Free ;
        Arg_Names.Free ;
        Defaults.Free ;
        Parser.Free ;
    end ;

    // Enter macro state...
    Enter_State( State_Macro, '', SL, nil ) ;
end ; // TCEF_Assembler.Process_Macro


// API (overrides)...

function TCEF_Assembler.Assemble_Ex( Input, Output, Listing : TCOM_Stream ;
    Status : TAssembler_Status ; Flags : longint ) : TUnified_Exception ;

var A : string ;
    Address : int64 ;
    Ignore, Machine : PChar ;
    Index, Len : integer ;
    Last_Line, Loop : integer ;
    Stream_File : TStreamer_File ;
    P : PChar ;
    PL : longint ;
    S, Source_Line : string ;
    UEC : TUnified_Exception ;

    Addr : int64 ;
    Flag, DT, Size : integer ;
    Ptr : pointer ;

    procedure Output_Symbol ;

    var Pt : PSymbol_Record ;

    begin
        Pt := PSymbol_Record( Ptr ) ;
        S := string( P ) ;
        while( length( S ) < 20 ) do
        begin
            S := S + ' ' ;
        end ;
        if( Pt^.Size = 0 ) then
        begin
            S := S + '--' ;
        end else
        begin
            S := S + inttostr( Pt^.Size ) ;
        end ;
        while( length( S ) < 28 ) do
        begin
            S := S + ' ' ;
        end ;
        S := S + inttostr( Pt^.Address ) ;
        Status.Output_To_Listing( PChar( S ), ListOut_Table ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
    end ;


    procedure Output_XRefs ;

    var Dummy, Work : integer ;
        Last, N, S, X : string ;

    begin
        S := '' ;
        Last := '' ;
        for Dummy := 0 to XRefs.Count - 1 do
        begin
            // Get and parse next xref record...
            X := XRefs[ Dummy ] ;
            Work := pos( '|', X ) ;
            N := copy( X, 1, Work - 1 ) ;
            X := copy( X, Work + 1, length( X ) ) ;

            if( N <> Last ) then // New symbol
            begin
                Last := N ;
                if( S <> '' ) then // Output old symbol, if one
                begin
                    Status.Output_To_Listing( PChar( S ), ListOut_Table ) ;
                    Status.Output_To_Listing( '', ListOut_New_Line ) ;
                end ;
                S := N ;
                while( length( S ) < 28 ) do
                begin
                    S := S + ' ' ; // Pad to line columns up
                end ;
                S := S + X ; // Append first reference
            end else
            begin
                S := S + ', ' + X ; // Append reference to current record
            end ;
        end ;
        if( S <> '' )  then
        begin
            Status.Output_To_Listing( PChar( S ), ListOut_Table ) ;
            Status.Output_To_Listing( '', ListOut_New_Line ) ;
        end ;
    end ;


var Context : int64 ;
    Current_Address_Space_Index : integer ;
    Last_Address_Space_Index : integer ;

begin // TCEF_Assembler.Assemble_Ex
    // Setup...
    Result := Set_Error( 0 ) ;
    Current_Created_Local_Label := 30000 ;
    Terminated := False ;
    A := '' ;
    Address := 0 ;
    Err := False ; // No errors so far
    Want_Symbol_Table_List := ( ( Flags and ASF_Want_Symbol_Table ) <> 0 ) ;
    Want_XRef_List := ( ( Flags and ASF_Want_XRef ) <> 0 ) ;
    Flags := Flags and ( not ( ASF_Want_Symbol_Table or ASF_Want_XRef ) ) ; // These flags are not passed on to the CPU's assembler
    Stream_File.Init( nil, True, False, 1 ) ;
    Stream_File.Stream := Input ;
    Tokens.Set_Source( @Stream_File ) ;
    In_Assembly_Status := Status ;
    _Status := Status ;
    XRefs.Free ;
    XRefs := TStringList.Create ;
    Current_Address_Space_Index := 0 ;
    if( _CPU <> nil ) then
    begin
        _Assembler := _CPU.CPU.Get_Assembler( Self ) ;
    end ;

    // Default symbol values...
    Initial_Symbols := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ;
    Valid_Symbols := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' ;
    if( _CPU <> nil ) then
    begin
        Address := _CPU.CPU.Get_Current_Address( 0, True ) ;
    end ;
    Have_Start_Address := False ;

    // Predefines...
    Expand( '.DEFINE VER26', P, PL, Status ) ;
    Expand( '.DEFINE VER20_UP', P, PL, Status ) ;
    Expand( '.DEFINE VER21_UP', P, PL, Status ) ;
    Expand( '.DEFINE VER22_UP', P, PL, Status ) ;
    Expand( '.DEFINE VER23_UP', P, PL, Status ) ;
    Expand( '.DEFINE VER24_UP', P, PL, Status ) ;
    Expand( '.DEFINE VER25_UP', P, PL, Status ) ;
    Expand( '.DEFINE VER26_UP', P, PL, Status ) ;
    Big_Endian := False ;
    if( _CPU <> nil ) then
    begin
        Set_Target_CPU( _CPU ) ;
    end ;
    Context := _Assembler.Begin_Assembly ;

    // Do the assembly...
    Last_Line := -1 ;
    while( not Terminated ) do
    begin
        Status.Set_Line( Tokens.Token_Line ) ;
        if( Last_Line <> Tokens.Token_Line ) then
        begin
            if( Last_Line <> -1 ) then
            begin
                Status.Output_To_Listing( PChar( Source_Line ), ListOut_Source ) ;
                Status.Output_To_Listing( '', ListOut_New_Line ) ;
            end ;
            Source_Line := Tokens.Line_Text ;
            Last_Line := Tokens.Token_Line ;
        end ;
        Application.ProcessMessages ;
	    if( ( _Next_Token = '' ) and Input.At_End and ( States.Count = 0 ) ) then
        begin
            if( ( Flags and ASF_Immediate_Mode ) = 0 ) then
            begin
                Status.Log_Error( 'Unexpected end of source', nil, Tokens.Token_Line, Severity_Warning ) ;
            end ;
            break ;
        end ;
        S := uppercase( _Get_Token ) ;
        Index := Macros.IndexOf( S ) ;
        if( Index = -1 ) then
        begin
            _Put_Token( S ) ;
        end else
        begin
            if( Process_Macro( S, Status, '' ) ) then
            begin
                break ;
            end ;
            continue ;
        end ;
        if( _CPU <> nil ) then
        begin
            PC := Address ;
            if( Status.Aborted ) then
            begin
                break ;
            end ;
            try
                if( _Assembler.Version < 22 ) then
                begin
                    UEC := _Assembler.Assemble( nil, Ignore, Machine, Len, PC, Segment, Status ) ;
                end else
                begin
                    UEC := _Assembler.Assemble_Ex( nil, Ignore, Machine, Len, PC, Segment, Status, Flags ) ;
                end ;
                if( UEC = nil ) then // Success
                begin
                    Last_Address_Space_Index := Current_Address_Space_Index ;
                    if( _CPU.CPU.Version > 25 ) then
                    begin
                        Current_Address_Space_Index := _CPU.CPU.Get_Target_Address_Space ;
                        if( Last_Address_Space_Index <> Current_Address_Space_Index ) then // Changed our address space
                        begin
                            PC := _CPU.CPU.Get_Current_Address( Current_Address_Space_Index, True ) ;
                            Address := PC ;
                        end ;
                    end ;
                    if( Altered_PC ) then
                    begin
                        Address := Altered_Address ;
                        PC := Address ;
                        _CPU.CPU.Set_Current_Address( Current_Address_Space_Index, True, Address ) ;
                    end else
                    if( ( Flags and ASF_Generate_Virtual ) <> 0 ) then // Generate code into virtual address space
                    begin
                        Address := _CPU.CPU.Translate( Current_Address_Space_Index, PC ) ; // Convert virtual address to physical (otherwise PC is physical address)
                    end ;
                    if( ( Output <> nil ) and ( Len > 0 ) ) then
                    begin
                        if( List_Codes ) then
                        begin
                            S := '' ;
                            for Loop := 0 to Len - 1 do
                            begin
                                S := S + CVTB( 10, Base, inttostr( ord( Machine[ Loop ] ) ) ) + ' ' ;
                            end ;
                            Status.Output_To_Listing( PChar( S ), ListOut_Generated_Data ) ;
                        end ;
                        if(
                            ( ( Flags and ASF_Extended ) <> 0 )
                            and
                            ( _CPU.CPU.Version > 25 )
                            and
                            ( _CPU.CPU.Get_Target_Memory <> nil )
                          ) then
                        begin
                            TCEF_Stream( Output ).Component := _CPU.CPU.Get_Target_Memory ;
                        end ;
                        Output.Seek( Address ) ;
                        Output.Write( Machine[ 0 ], Len ) ;
                        Address := Address + Len ;
                        Status.Code := Status.Code + Len ;
                    end ;
                end ; // if( UEC = nil )
            except
            end ;
            Altered_PC := False ;
        end else
        begin
            UEC := Expand( nil, P, PL, Status ) ;
            if( UEC <> nil ) then
            begin
                Status.Log_Error( PChar( 'Illegal instruction' ), nil, Tokens.Token_Line, Severity_Error ) ;
                break ;
            end else
            begin
                S := '' ;
                for Loop := 0 to PL - 1 do
                begin
                    S := S + CVTB( 10, Base, inttostr( ord( P[ Loop ] ) ) ) + ' ' ;
                end ;
                Status.Output_To_Listing( PChar( S ), ListOut_Generated_Data ) ;
                Output.Write( P[ 0 ], PL ) ;
            end ;
        end ; // if( _CPU <> nil )
    end ; // while( True )
    if( Want_Symbol_Table_List ) then
    begin
        Status.Output_To_Listing( 'SYMBOL TABLE', ListOut_Title_Text ) ;
        Status.Output_To_Listing( '', ListOut_New_Page ) ;
        Status.Output_To_Listing( '  SYMBOL TABLE', ListOut_Table ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
        Status.Output_To_Listing( 'Symbol Name         Size    Value', ListOut_Table ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
        Status.Output_To_Listing( '-----------         ----    -----', ListOut_Table ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
        if( Assembler_Context.Find_First( P, Addr, Flag, DT, Size, Ptr ) = 0 ) then
        begin
            Output_Symbol ;
            while( Assembler_Context.Find_Next( P, Addr, Flag, DT, Size, Ptr ) = 0 ) do
            begin
                Output_Symbol ;
            end ;
        end ;
    end ;
    if( Want_XRef_List ) then
    begin
        Status.Output_To_Listing( 'CROSS-REFERENCE TABLE', ListOut_Title_Text ) ;
        Status.Output_To_Listing( '', ListOut_New_Page ) ;
        Status.Output_To_Listing( '  CROSS-REFERENCE TABLE', ListOut_Table ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
        Status.Output_To_Listing( 'Symbol Name         References', ListOut_Table ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
        Status.Output_To_Listing( '-----------         ----------', ListOut_Table ) ;
        Status.Output_To_Listing( '', ListOut_New_Line ) ;
        XRefs.Sort ;
        Output_XRefs ;
    end ;
    _Assembler.Finish_Assembly( Context, Ignore, Machine, Len, PC, Status, Flags ) ;
    Stream_File.Done ;
    In_Assembly_Status := nil ;
    if( _CPU <> nil ) then
    begin
        if( Have_Start_Address ) then
        begin
            _CPU.CPU.Set_Current_Address( 0, True, Start_Address ) ;
        end ;
    end ;
    _Status := nil ;
end ; // TCEF_Assembler.AssembleEx


function TCEF_Assembler.Get_Base : integer ;

begin
    Result := _Base ;
end ;


procedure TCEF_Assembler.Set_Base( Value : integer ) ;

begin
    _Base := Value ;
end ;


procedure TCEF_Assembler.Register_Extension( Extension : TAssembler_Extension ) ;

begin
    _Extension := Extension ;
end ;


procedure TCEF_Assembler.Add_Reference_Ex( Name : PChar ; Size : longint ;
    Address : int64 ; Context, Flags : longint ) ;

    procedure Add_XRef( N : string ) ;

        function Check( const Op : string ) : boolean ;

        var Dummy : integer ;

        begin
            Dummy := pos( Op, N ) ;
            if( Dummy > 0 ) then
            begin
                Add_XRef( copy( N, 1, Dummy - 1 ) ) ;
                Add_XRef( copy( N, Dummy + 1, length( N ) ) ) ;
                Result := True ;
            end else
            begin
                Result := False ;
            end ;
        end ;

    var S : string ;
    
    begin
        // Break apart expressions...
        if( Check( '+' ) ) then
        begin
            exit ;
        end ;
        if( Check( '-' ) ) then
        begin
            exit ;
        end ;
        if( Check( '*' ) ) then
        begin
            exit ;
        end ;
        if( Check( '/' ) ) then
        begin
            exit ;
        end ;
        if( Check( '^' ) ) then
        begin
            exit ;
        end ;
        if( Check( '+' ) ) then
        begin
            exit ;
        end ;
        if( Check( '(' ) ) then
        begin
            exit ;
        end ;
        if( Check( ')' ) ) then
        begin
            exit ;
        end ;
        if( Check( ' ' ) ) then
        begin
            exit ;
        end ;

        N := Edit( N, 8 or 128 ) ;
        if( length( N ) = 0 ) then
        begin
            exit ;
        end ;
        if( N[ 1 ] in Set_Of_Digits ) then // Numeric literal
        begin
            exit ;
        end ;
        S := inttostr( Tokens.Token_Line ) ;
        while( length( S ) < 5 ) do
        begin
            S := ' '  + S ;
        end ;
        XRefs.Add( N + '|@' + S ) ;
    end ;

var N : string ;
    P : PSymbol_Record ;

begin
    if( Backpatch_Table = nil ) then
    begin
        Backpatch_Table := TStringList.Create ;
    end ;
    N := Edit( string( Name ), 8 or 128 ) ; // Trim trailing and leading whitespace
    new( P ) ;
    P^.Address := Address ;
    P^.Size := Size ;
    if( Big_Endian ) then
    begin
        Flags := Flags or SF_Big_Endian ;
    end ;
    P^.Flags := Flags ;
    P^.Context := Context ;
    P^.Line := Tokens.Token_Line ;
    P^.Filename := nil ;
    Backpatch_Table.AddObject( N, TObject( P ) ) ;

    Add_XRef( N ) ;
end ; // TCEF_Assembler.Add_Reference_Ex


procedure TCEF_Assembler.Add_CPU( CPU : TComponent ; Name : PChar ) ;

begin
    if( CPU <> nil ) then
    begin
        CPU_List.AddObject( string( Name ), CPU ) ;
    end ;
end ;


procedure TCEF_Assembler.Clear_CPUs ;

begin
    CPU_List.Clear ;
end ;


procedure TCEF_Assembler.Backpatch( Status : TAssembler_Status ;
    Stream : TCOM_Stream ) ;

    function In_Range( Size : longint ; Value : int64 ) : boolean ;

    var H : int64 ;

    begin
       if( Size < 0 ) then // Bit range
       begin
           Size := ( -Size ) shr 16 ;
           if( Size >= 63 ) then // 8 or more bytes
           begin
               Result := True ;
               exit ;
           end ;
           H := 1 ;
           while( Size > 0 ) do
           begin
               H := ( H * 2 ) or 1 ;
           end ;
       end else
       begin
           case Size of
               1 : H := 255 ;
               2 : H := 65535 ;
               3 : H := $FFFFFF ;
               4 : H := $FFFFFFFF ;
               5 : H := $FFFFFFFFFF ;
               6 : H := $FFFFFFFFFFFF ;
               7 : H := $FFFFFFFFFFFFFF ;
               else
                   begin
                       Result := True ;
                       exit ;
                   end ;
           end ;
       end ;
       if( Value < 0 ) then
       begin
           Value := Value + ( H div 2 ) + 1 ;
       end ;
       In_Range := Value <= H ;
    end ;

var B : byte ;
    Loop, I : integer ;
    Mask : int64 ;
    P : PSymbol_Record ;
    P_Size : integer ;
    Resolved : boolean ;
    UEC : TUnified_Exception ;
    Value : int64 ;
    I64_Map : record
                  case boolean of
                      False: ( I : int64 ) ;
                      True: ( B : array[ 0..7 ] of byte ) ;
              end ;

begin // TCEF_Assembler.Backpatch
    try
        Inhibit_Xref_Add := True ;
        // Resolve forward-declared symbols
        if( Forwards_Names <> nil ) then
        begin
            while( Forwards_Names.Count > 0 ) do
            begin
                Resolved := False ;
                for Loop := Forwards_Names.Count - 1 downto 0 do
                begin
                    UEC := Evaluate( PChar( Forwards_Values[ Loop ] ), Value ) ;
                    if( UEC = nil ) then
                    begin
                        Resolved := True ;
                        new( P ) ;
                        fillchar( P^, sizeof( P^ ), 0 ) ;
                        P^.Address := Value ;
                        P^.Typ := 1 ; // Integer
                        P^.Flags := SF_Constant ; // Constant
                        P^.Data := Value ; // Value
                        Add_Symbol( PChar( Forwards_Values[ Loop ] ), P ) ;
                        Forwards_Names.Delete( Loop ) ;
                        Forwards_Values.Delete( Loop ) ;
                    end ;
                end ;
                if( not Resolved ) then // Nothing resolved in this loop
                begin
                    for Loop := 0 to Forwards_Names.Count - 1 do
                    begin
                        Status.Log_Error( PChar( 'Could not resolve symbol ' + Forwards_Names[ Loop ] ), nil, Tokens.Token_Line, Severity_Error ) ;
                    end ;
                    Forwards_Names.Clear ;
                    Forwards_Values.Clear ;
                end ;
            end ; // while( Forwards_Names.Count > 0 )
        end ;

        // Now backpatch
        if( Backpatch_Table <> nil ) then
        begin
            for Loop := 0 to Backpatch_Table.Count - 1 do
            begin
                UEC := Evaluate( PChar( Backpatch_Table[ Loop ] ), Value ) ;
                if( UEC <> nil ) then
                begin
                    P := new( PSymbol_Record ) ;
                    Add_Symbol( PChar( Backpatch_Table[ Loop ] ), P ) ; // So we don't report this symbol more than once
                    Status.Log_Error( PChar( 'Undefined symbol ' + Backpatch_Table[ Loop ] + ' referenced' ), nil, Tokens.Token_Line, Severity_Error ) ;
                end ;
                try
                    P := PSymbol_Record( Backpatch_Table.Objects[ Loop ] ) ;
                    try
                        if(
                            ( _Assembler.Version < 21 )
                            or
                            _Assembler.Backpatching( PChar( Backpatch_Table[ Loop ] ),
                                P^.Address, Value, P^.Size, P^.Context, P^.Flags,
                                P^.Line, P^.Filename, Status )
                          ) then
                        begin
                            if( ( P^.Flags and SF_Big_Endian ) <> 0 ) then // Big-endian
                            begin
                                // Reverse byte order of value...
                                I64_Map.I := Value ;
                                for I := 0 to 3 do
                                begin
                                    B := I64_Map.B[ I ] ;
                                    I64_Map.B[ I ] := I64_Map.B[ 7 - I ] ;
                                    I64_Map.B[ 7 - I ] := B ;
                                end ;
                                if( P^.Size > 0 ) then // Bit range
                                begin
                                    Value := I64_Map.I shr ( ( 8 - P^.Size ) * 8 ) ;
                                end ;
                            end ;
                            if( P^.Size < 0 ) then // Bit range
                            begin
                                Value := 0 ;
                                Mask := 1 ;
                                P_Size := ( -P^.Size ) shr 16 ; // Size of mask
                                while( P_Size > 1 ) do
                                begin
                                    dec( P_Size ) ;
                                    Mask := ( Mask shl 1 ) or 1 ;
                                end ;
                                if( ( I64_Map.I > Mask ) and ( Mask > 0 ) ) then
                                begin
                                    Status.Log_Error( PChar( 'Value truncated for symbol ' + Backpatch_Table[ Loop ] ), nil, Tokens.Token_Line, Severity_Warning ) ;
                                end ;
                                I64_Map.I := I64_Map.I and Mask ;
                                P_Size := ( -P^.Size ) and $FFFF ; // Starting bit
                                while( P_Size > 0 ) do
                                begin
                                    dec( P_Size ) ;
                                    Mask := Mask shl 1 ;
                                    I64_Map.I := I64_Map.I shl 1 ;
                                end ;
                                P_Size := ( -P^.Size ) shr 16 ; // Size (in bits)
                                P_Size := ( P_Size + 7 ) shr 3 ; // Size (in bytes)
                                Stream.Seek( P^.Address ) ;
                                Stream.Read( Value, P_Size ) ;
                                Value := ( Value and ( not Mask ) ) or I64_Map.I ;
                                Stream.Seek( P^.Address ) ;
                                Stream.Write( Value, P_Size ) ;
                            end else
                            begin
                                Stream.Seek( P^.Address ) ;
                                Stream.Write( Value, P^.Size ) ;
                                if( not In_Range( P^.Size, Value ) ) then
                                begin
                                    Status.Log_Error( PChar( 'Value truncated for symbol ' + Backpatch_Table[ Loop ] ), nil, Tokens.Token_Line, Severity_Warning ) ;
                                end ;
                            end ;
                        end ; // if
                    finally
                        freemem( P ) ;
                    end ;
                except
                    on E : Exception do
                        Status.Log_Error( PChar( E.Message ), nil, Tokens.Token_Line, Severity_Warning ) ;
                end ;
                Backpatch_Table.Objects[ Loop ] := nil ;
            end ;
            Backpatch_Table.Free ;
            Backpatch_Table := nil ;
        end ;
    finally
        Inhibit_Xref_Add := False ;
    end ;
end ; // TCEF_Assembler.Backpatch


{ This method is used during assembly by a TAssembler to insert assembly
  source in-place.  For instance, translating a directive into a
  standard directive, or doing macro expansions, etc. }
function TCEF_Assembler.Expand( Source : PChar ; var Res : PChar ;
    var Res_Length : longint ; Status : TAssembler_Status ) : TUnified_Exception ;

var Input_Buffer : string ;

    function Get_Token : string ;

    var C : char ;
        Dummy : integer ;

    begin
        while( copy( Input_Buffer, 1, 1 ) = ' ' ) do
        begin
            Input_Buffer := copy( Input_Buffer, 2, length( Input_Buffer ) ) ;
        end ;
        if( length( Input_Buffer ) > 0 ) then
        begin
            C := Input_Buffer[ 1 ] ;
        end else
        begin
            C := ' ' ;
        end ;
        if( ( C = '"' ) or ( C = #39 ) ) then
        begin
            Dummy := Instr( 2, Input_Buffer + C, C ) ;
            Result := Edit( copy( Input_Buffer, 1, Dummy ), 32 or 256 ) ;
            Input_Buffer := copy( Input_Buffer, Dummy + 1, length( Input_Buffer ) ) ;
            exit ;
        end ; // if( ( C = '"' ) or ( C = #39 ) )
        for Dummy := 1 to length( Input_Buffer ) do
        begin
            if( pos( Input_Buffer[ Dummy ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$_.' ) = 0 ) then
            begin
                if( Dummy = 1 ) then
                begin
                    Result := Edit( Input_Buffer[ 1 ], 32 ) ;
                    Input_Buffer := copy( Input_Buffer, 2, length( Input_Buffer ) ) ;
                end else
                begin
                    Result := Edit( copy( Input_Buffer, 1, Dummy - 1 ), 32 or 256 ) ;
                    Input_Buffer := copy( Input_Buffer, Dummy, length( Input_Buffer ) ) ;
                end ;
                exit ;
            end ;
        end ; // for Dummy := 1 to length( Input_Buffer )
        Result := Edit( Input_Buffer, 32 or 256 ) ;
        Input_Buffer := '' ;
    end ; // Get_Token


    function Peek_Token : string ;

    begin
        Result := Get_Token ;
        if( length( Result ) > 0 ) then
        begin
            Input_Buffer := Result + ' ' + Input_Buffer ;
        end ;
        Result := Edit( Result, 32 or 256 ) ;
    end ;


var AA : string ;
    Any_Code : boolean ;
    B : integer ;
    Ex : int64 ;
    Global : boolean ;
    Next, Original, S, Value : string ;
    P : pSymbol_Record ;
    UEC : TUnified_Exception ;

begin // TCEF_Assembler.Expand
    // Setup...
    Res := nil ;
    Res_Length := 0 ;
    fillchar( Result, sizeof( Result ), 0 ) ;

    if( Source = nil ) then
    begin
        Input_Buffer := _Grab_Line( True ) ;
    end else
    begin
        Input_Buffer := Source ;
    end ;
    Value := Input_Buffer ;
    Original := Value ;
    Input_Buffer := Edit( Parse_Parameter( ';', Value ), 16 or 128 or 256 ) ;
    if( length( Input_Buffer ) = 0 ) then
    begin
	    exit ;
    end ;
    Value := Get_Token ;
    Next := Peek_Token ; // Peek at the next token on the line
    if( Next = '.' ) then
    begin
        Next := Get_Token ;
        Next := Next + Peek_Token ;
        Input_Buffer := '.' + Input_Buffer ;
    end ;

    // Handle standard directives...
    Global := False ;
    if( ( Next = 'EQU' ) or Is_Directive( Next, 'EQU' ) or ( Next = '=' ) ) then
    begin
        if( Is_PC( Value ) ) then
        begin
            Get_Token ; // Eat the "=" or "EQU"
            Value := 'ORG' ; // "PC = X" is the same as "ORG X"
        end else
        begin
            S := Get_Token ; // Get "EQU" or "="
            if( S = '.' ) then
            begin
                S := Get_Token ; // Handle .EQU
            end ;
            if( S = '=' ) then // == construct
            begin
                S := Get_Token ;
                Global := True ;
            end ;
            if( length( Input_Buffer ) = 0 ) then
            begin
                Result := Set_Error( CEFAssemblerErr_Missing_Value ) ;
                Status.Log_Error( PChar( 'Missing value' ), nil, Tokens.Token_Line, Severity_Error ) ;
                exit ;
            end ;
            UEC := Evaluate( PChar( Input_Buffer ), Ex ) ; // Convert to numeric value
            if( UEC <> nil ) then
            begin
                if( UEC.Get_Error = CEFAssemblerErr_Undefined_Symbol ) then
                begin
                    if( Forward_Declare( Value, Input_Buffer ) ) then
                    begin
                        Status.Log_Error( PChar( 'Symbol redeclared: ' + Value ), nil, Tokens.Token_Line, Severity_Error ) ;
                        Result := Set_Error( CEFAssemblerErr_Multiply_Defined ) ;
                    end ;
                end else
                begin
                    Result := UEC ;
                    Status.Log_Error( PChar( ET( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
                end ;
                exit ;
            end ;
            new( P ) ;
            fillchar( P^, sizeof( P^ ), 0 ) ;
            P^.Address := Ex ;
            P^.Typ := 1 ; // Integer
            P^.Flags := SF_Constant ; // Constant
            if( Global ) then
            begin
                P^.Flags := P^.Flags or SF_Global ;
            end ;
            P^.Data := Ex ; // Value
            UEC := Add_Symbol( PChar( Value ), P ) ;
            if( UEC <> nil ) then
            begin
                Result := UEC ;
                Status.Log_Error( PChar( SERT( UEC.Get_Error ) ), nil, Tokens.Token_Line, Severity_Error ) ;
            end ;
            exit ;
        end ;
    end ; // if( Next = 'EQU' )
    Any_Code := False ;

    if( _Extension <> nil ) then
    begin
        S := Value + ' '  + Input_Buffer ;
        Result := _Extension.Process_Directive( PChar( S ), Res, Res_Length, Status ) ;
        if( Result = nil ) then // Handled
        begin
            exit ;
        end ;
    end ;

    { Handle directives with parameters }
    if( ( Value = 'ORG' ) or Is_Directive( Value, 'ORG' ) ) then { PC placement }
    begin
        Value := Edit( Input_Buffer, 8 ) ;
        B := Pos( ' ', Value + ' ' ) ; { Find end of op-code }
        Aa := Copy( Value, 1, B - 1 ) ; { Get operand }
        Fedit( AA, AA, 511 ) ;
        if( Length( AA ) = 0 ) then
        begin
            Err := True ;
            Value := '' ;
            exit ;
        end ;
        UEC := Evaluate( PChar( AA ), Ex ) ;
        if( UEC <> nil ) then
        begin
            exit ;
        end ;
        Pc := Ex ;
        Altered_PC := True ;
        Altered_Address := PC ;
        exit ;
    end ;

    if( Next = ':' ) then
    begin
        Value := Value + Input_Buffer ;
    end else
    begin
        Value := Value + ' ' + Input_Buffer ;
    end ;
    if( Handle_Directives( Original, Any_Code, Status ) ) then { If a directive }
    begin
        Temp_Expand := Original ;
        Res := PChar( Temp_Expand ) ;
        Res_Length := length( Original ) ;
        exit ;
    end ;
    Result := Set_Error( CEFAssemblerErr_Illegal_Instruction ) ;
end ; // TCEF_Assembler.Expand


function TCEF_Assembler.Facility_Code : longint ;

begin
    Result := CEFAssemblerErr_Facility ;
end ;


procedure TCEF_Assembler.In_Line( Input : TCOM_Stream ) ;

begin
    Enter_State( State_Inline, '', nil, Input ) ;
end ;


{ Logs an assembly error.  Severity indicates the seriousness of the
  error:
                0 = Informational
                1 = Warning
                2 = Error
                3 = Fatal error (abort assembly) }
procedure TCEF_Assembler.Log_Error( Text : PChar ; Severity : longint ) ;

begin
    if( In_Assembly_Status = nil ) then
    begin
        ShowMessage( Text + ' at line ' + Num2( Tokens.Token_Line ) ) ;
    end else
    begin
        In_Assembly_Status.Log_Error( PChar( Text ), nil, Tokens.Token_Line, Severity ) ;
    end ;
end ;


var Static_Get_Token : string ;

function TCEF_Assembler.Get_Token : PChar ;

begin
    Static_Get_Token := _Get_Token ;
    Get_Token := pchar( Static_Get_Token ) ;
end ;


procedure TCEF_Assembler.Put_Token( Token : PChar ) ;

begin
    _Put_Token( string( Token ) ) ;
end ;


function TCEF_Assembler.Peek_Token( Same_Line : boolean ) : PChar ;

begin
    if( Same_Line and _Token_EOL ) then
    begin
        Static_Get_Token := ''
    end else
    begin
        Static_Get_Token := _Get_Token ;
        _Put_Token( Static_Get_Token ) ;
    end ;
    Peek_Token := pchar( Static_Get_Token ) ;
end ;


{ Begins a sub-scope for identifiers within the current scope. }
procedure TCEF_Assembler.Push_Scope ;

begin
    Assembler_Context.Push_Level ;
end ;


{ Ends the current sub-scope for identifiers. }
procedure TCEF_Assembler.Pop_Scope ;

begin
    Assembler_Context.Pop_Level ;
end ;


{ Returns the symbol record for the identifier in Name.  Returns NULL,
  if Name is undeclared. }
function TCEF_Assembler.Get_Symbol( Name : PChar ) : PSymbol_Record ;

var Addr : int64 ;
    Flag, DT, Size : integer ;
    P : pointer ;

begin
    if( Assembler_Context.Find( Name, Addr, Flag, DT, Size, P ) = 0 ) then
    begin
        Result := P ;
    end else
    begin
        Result := nil ;
    end ;
end ;


{ Adds a symbol to the current scope, with the name in Name and the specified
  information. }
function TCEF_Assembler.Add_Symbol( Name : PChar ; P : pSymbol_Record ) : TUnified_Exception ;

var S : string ;

begin
    Result := Set_Error( 0 ) ;
    Assembler_Context.Add_Symbol( Name, P ) ;
    if( not Inhibit_Xref_Add ) then
    begin
        S := inttostr( Tokens.Token_Line ) ;
        while( length( S ) < 5 ) do
        begin
            S := ' ' + S ;
        end ;
        XRefs.Add( Name + '|#' + S ) ;
    end ;
end ;


{ Sets case sensitivity on identifier names. }
procedure TCEF_Assembler.Set_Case_Sensitive( Value : boolean ) ;

begin
    Assembler_Context.Case_Sensitive := Value ;
end ;


{ Returns True if case sensitivity to identifier names is on. }
function TCEF_Assembler.Get_Case_Sensitive : boolean ;

begin
    Result := Assembler_Context.Case_Sensitive ;
end ;


function TCEF_Assembler.Grab_Line( Current : boolean ) : PChar ;

begin
    Static_Get_Token := _Grab_Line( Current ) ; // Get value
    Result := PChar( Static_Get_Token ) ;
end ;


function TCEF_Assembler.Leading_Whitespace : boolean ;

var Entry : TState ;

begin
    if( States.Count > 0 ) then
    begin
        Entry := TState( States[ States.Count - 1 ] ) ;
        Result := Entry.Tokens.Leading_Whitespace ;
        exit ;
    end ;

    Result := Tokens.Leading_Whitespace ;
end ;


procedure TCEF_Assembler.Map( Address : int64 ) ;

var P : PChar ;
    S : string ;

begin
    if( In_Assembly_Status = nil ) then
    begin
        exit ;
    end ;
    P := In_Assembly_Status.Filename ;
    if( P = nil ) then
    begin
        exit ;
    end ;
    S := string( P ) ;
    if( length( S ) > 0 ) then
    begin
        Last_Mapping_Index :=
            Assembler_Context.Add_Mapping( PChar( S ), Address, Tokens.Token_Line ) ;
    end else
    begin
        Last_Mapping_Index := -1 ;
    end ;
end ;


procedure TCEF_Assembler.UnMap ;

begin
    if( Last_Mapping_Index <> -1 ) then
    begin
        Assembler_Context.Delete_Mapping( Last_Mapping_Index ) ;
        Last_Mapping_Index := -1 ;
    end ;
end ;



// TState methods...

destructor TState.Destroy ;

begin
    List.Free ;
    List := nil ;
    Tokens.Free ;
    Tokens := nil ;

    inherited Destroy ;
end ;


end.
