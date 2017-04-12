{
        Program Name : TapePres
        Package Name : CEF32
        Purpose      : Generic Tape displayer
        Institution  : Conroy & Conroy Co.
        Date Written : 31-Dec-2009
        Written By   : Alan Conroy
        Version      : 1.0

	Released to the public domain.

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

          This form is used to display formatted tape data.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit TapePres ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, Buttons, StdCtrls, Spin,

     // CEF32...
     CEF, // PCEF_Media_File_Header
     ArrayInt, // TCOM_Array_Interface
     _UE, // TUnified_Exception
     CEFMedia, // TPresenter
     DataDis ; // TData_Display


type TGeneric_Tape_Form = class(TForm)
            Command_Panel: TPanel;
            Data_Panel: TPanel;
            Label1: TLabel;
            Rewind_Button: TSpeedButton;
            FF_Button: TSpeedButton;
            Label2: TLabel;
            Record_Size_Label: TLabel;
            Current_Record_Label: TLabel;
            Record_Selector: TSpinEdit;
            Splitter1: TSplitter;
            Add_Data_Button: TSpeedButton;
            SpeedButton1: TSpeedButton;
            procedure FormCreate(Sender: TObject);
            procedure Rewind_ButtonClick(Sender: TObject);
            procedure FF_ButtonClick(Sender: TObject);
            procedure Record_SelectorChange(Sender: TObject);
            procedure Add_Data_ButtonClick(Sender: TObject);
            procedure SpeedButton1Click(Sender: TObject);

          private // Instance data...
              __Array : TMedia ;
              Current_Record : integer ;
              Panel : TData_Display ;
              Sub_Array : TCOM_Array_Interface ;
              Tape : TTape_Media ;

          protected // Property handlers...
              procedure Set_Array( A : TMedia ) ;

          public // API...
              procedure Get_Record( Num : int64 ) ;
              property _Array : TMedia
                  read __Array
                  write Set_Array ;
          end ; // TGeneric_Tape_Form

type TGeneric_Tape_Presenter = class( TPresenter )
                                   public // Constructors and destructors...
                                       constructor Create( Parent : THandle ;
                                           _Array : TMedia ;
                                           Header : PCEF_Media_File_Header ) ;

                                   private // Instance data...
                                       Panel : TGeneric_Tape_Form ;

                                   public // API...
                                       function Terminate : TUnified_Exception ; override ;
                                       procedure Redraw ; override ;
                                       procedure Set_Bounds( Width, Height : integer ) ;
                                           override ;
                                       procedure Get_Header( var Header : PCEF_Media_File_Header ) ;
                                           override ;
                               end ;

{ The Generic_Tape_Media object manages tape image files.  The contents of the
  file is a series of records.  Each record has the format:

  Size (default 4 byte)
  Data (size bytes)
  [Optional Size (default 4 bytes) - except for tape marks]

  The tape can be read or written in byte (raw) or record (cooked) mode.  I/O in
  record mode treats the size and data records as single logical records with a
  queriable size.  When doing record mode operations, the file pointer is always
  left at the starting offset of the physical data record (after the first size
  record).  Thus, a raw mode I/O after a record mode position will always
  read/write the physical data record.

  A tape mark is a record with 0 length.  Two tape marks indicate Logical EOT.
}
type TGeneric_Tape_Media = class( TTape_Media ) // Tape with 2-byte count before each record
                               public // Constructors and destructors...
                                   constructor Create( _Int : TMedia ) ;

                               private // Instance data...
                                   _Int : TMedia ;
                                   _BPI : integer ;
                                   _Length : integer ;
                                   _Position : int64 ; // Byte offset in media
                                   _Current_Record : int64 ; // Current record
                                   _Current_Record_Position : int64 ; // Byte offset of current record
                                   _Max_Record : int64 ; // Maximum record in file
                                   _Count_Size : integer ; // Size of counts (in byte)
                                   _After : boolean ; // True if count follows record as well as preceding it
                                   Temp_Read : string ;
                                   _Data_Size : int64 ; // Total size of data on tape
                                   _TM_Count : int64 ; // Total number of tape marks on tape
                                   _IRG_Length : longint ; // Length of each IRG
                                   _TM_Length : longint ; // Length of each TM

                               private // Internal utility routines...
                                   procedure Recalculate ;

                               public // Property handlers...
                                   function Get_After : boolean ; override ;
                                   procedure Set_After( Value : boolean ) ; override ;
                                   function Get_Count_Length : integer ; override ;
                                   procedure Set_Count_Length( Value : integer ) ; override ;
                                   function Get_IRG_Length : integer ; override ;
                                   procedure Set_IRG_Length( Value : integer ) ; override ;
                                   function Get_TM_Length : integer ; override ;
                                   procedure Set_TM_Length( Value : integer ) ; override ;

                               protected // Class support...
                                   function Read_Count : integer ; virtual ;

                               public // API...
                                   function Is_Class( _N : PChar ) : boolean ;
                                       override ;

                                   // Insert Count Items starting at index Index, filling with Fill.
                                   function Insert( Index, Count : int64 ;
                                       Fill : byte ) : TUnified_Exception ; override ;

                                   // Delete Count items starting at index Index (inclusive), truncating file by Count.
                                   function Delete( Index, Count : int64 ) : TUnified_Exception ;
                                       override ;

                                   // Reads Count bytes from media, starting at index Index, into buffer Data
                                   function Get( Index, Count : int64 ; Data : PChar ) : TUnified_Exception ;
                                       override ;

                                   // Writes count bytes from Data to media, starting at index Index.
                                   function Put( Index, Count : int64 ; Data : PChar ) : TUnified_Exception ;
                                       override ;

                                   { Return a byte from the given index. }
                                   function Get_Byte( Index : int64 ) : byte ;
                                       override ;

                                   { Set a byte at a specific index. }
                                   procedure Set_Byte( Index : int64 ;
                                       Value : byte ) ; override ;

                                   { Returns true if the data is read-only. }
                                   function Read_Only : boolean ; override ;

                                   { Returns low bound of specified subscript. }
                                   function Low_Bound( Subscript : integer ) : int64 ;
                                       override ;

                                   { Returns high bound of specified subscript. }
                                   function High_Bound( Subscript : integer ) : int64 ;
                                       override ;

                                   { Returns number of subscripts. }
                                   function Subscripts : integer ; override ;


                                   function Get_BPI : integer ; override ;
                                   // Retrive tape's BPI setting (0 indicates any).

                                   procedure Set_BPI( Value : integer ) ; override ;
                                   // Set tape's BPI setting (0 indicates any).

                                   function Get_Length : integer ; override ;
                                   // Returns length of tape (in feet).  0 = infinite

                                   procedure Set_Length( Value : integer ) ; override ;
                                   // Sets length of tape (in feet).  0 means infinite.

                                   function Record_Count : int64 ; override ;
                                   // Count of records on the tape (includes tape marks)

                                   function Record_Length( Index : int64 ) : int64 ;
                                       override ;
                                   { Returns length of record Index.  If result is 0,
                                     then there is a tape mark at the position. }

                                   function At_BOT : boolean ; override ;
                                   { True if at BOT }

                                   function At_PEOT : boolean ; override ;
                                   { True if at physical EOT}

                                   function At_LEOT : boolean ; override ;

                                   function Position( Records : boolean ) : int64 ;
                                       override ;
                                   // Current position on tape (byte or record offset)

                                   procedure Seek( Count : int64 ; Records, Relative : boolean ) ;
                                       override ;
                                   { Moves current position on tape.  Parameters:
                                         Count : Amount/where to move.
                                         Records: True to interpret Count as record count,
                                                  False to interpret as byte count.
                                         Relative: True to move relative to current
                                                   position.  False to move to absolute
                                                   position.
                                   }

                                   procedure Seek_BOT ; override ;
                                   // Move to beginning of tape.

                                   procedure Seek_LEOT ; override ;
                                   // Move to logical end of tape.

                                   procedure Seek_PEOT ; override ;
                                   // Move to physical end of tape.

                                   function Support_TM : boolean ; override ;
                                   // Returns true if tape marks supported on tape.

                                   procedure Add_TM ; override ;
                                   // Add a tape mark to the tape.

                                   function Read( Data : Pchar ; Count : int64 ) : int64 ;
                                       override ;
                                   { Read Count bytes into Data.  Returns the count
                                     actually read.  Cannot read beyond current record. }

                                   function Read_Record( var Count : int64 ) : PChar ;
                                       override ;
                                   { Returns current reoord.  Count is ignored
                                     on call.  On return, it is the size of the
                                     read record. }

                                   function Write( Data : Pchar ; Count : int64 ) : TUnified_Exception ;
                                       override ;
                                   { Write Count bytes from Data to tape.  Count must be
                                     larger than 0.  This is a raw operation and does
                                     not prefix the data with a count or any other such
                                     processing. }

                                   function Write_Record( Data : Pchar ; Count : int64 ) : TUnified_Exception ;
                                       override ;
                                   { Write a record of Count bytes from Data to tape.
                                     Count must be larger than 0. }

                                   property Int : TMedia
                                       read _Int
                                       write _Int ;
                            end ; // TGeneric_Tape_Media

implementation

uses // CEF32...
     TapePres_Add_Data ;

{$R *.dfm}

// TGeneric_Tape_Media...

// Constructors and destructors...

constructor TGeneric_Tape_Media.Create( _Int : TMedia ) ;

begin
    inherited Create ;

    _Count_Size := 2 ;
    _After := False ;
    Int := _Int ;
    Recalculate ;
end ;


// Internal utility routines...

procedure TGeneric_Tape_Media.Recalculate ;

var Count : integer ;

begin
    _TM_Count := 0 ; // Total number of tape marks on tape
    _Current_Record := 0 ;
    _Max_Record := 0 ;
    _Position := 0 ;
    while( not At_PEOT ) do
    begin
        Count := Read_Count ;
        if( Count = 0 ) then
        begin
            inc( _TM_Count ) ;
        end ;
        _Data_Size := _Data_Size + Count ;
        inc( _Max_Record ) ;
        _Position := _Position + Count ;
        if( _After and ( Count > 0 ) ) then
        begin
            Read_Count ;
        end ;
    end ;
    seek( 0, False, False ) ;
end ;


function TGeneric_Tape_Media.Is_Class( _N : PChar ) : boolean ;

var N : string ;

begin
    N := lowercase( string( _N ) ) ;
    Result := ( N = 'tgeneric_tape_media' ) or ( N = 'ttape_media' ) ;
end ;


function TGeneric_Tape_Media.Get_After : boolean ;

begin
    Result := _After ;
end ;


procedure TGeneric_Tape_Media.Set_After( Value : boolean ) ;

begin
    if( _After <> Value ) then
    begin
        _After := Value ;
        Recalculate ;
    end ;
end ;


function TGeneric_Tape_Media.Get_Count_Length : integer ;

begin
    Result := _Count_Size ;
end ;


procedure TGeneric_Tape_Media.Set_Count_Length( Value : integer ) ;

begin
    if( _Count_Size <> Value ) then
    begin
        _Count_Size := Value ;
        Recalculate ;
    end ;
end ;


function TGeneric_Tape_Media.Get_IRG_Length : integer ;

begin
    Result := _IRG_Length ;
end ;


procedure TGeneric_Tape_Media.Set_IRG_Length( Value : integer ) ;

begin
    _IRG_Length := Value ;
end ;


function TGeneric_Tape_Media.Get_TM_Length : integer ;

begin
    Result := _TM_Length ;
end ;


procedure TGeneric_Tape_Media.Set_TM_Length( Value : integer ) ;

begin
    _TM_Length := Value ;
end ;


function TGeneric_Tape_Media.Read_Count : integer ;

var B, C, I : integer ;

begin
    C := _Count_Size - 1 ; // Bytes remaining
    Result := Int.Get_Byte( _Position ) ;
    inc( _Position ) ;
    I := 8 ;
    while( ( C > 0 ) and ( not At_PEOT ) ) do
    begin
        dec( C ) ;
        B := Int.Get_Byte( _Position ) ;
        Result := Result or ( B shl I ) ;
        I := I + 8 ;
        inc( _Position ) ;
    end ;
end ;


// API...

function TGeneric_Tape_Media.Insert( Index, Count : int64 ; Fill : byte ) : TUnified_Exception ;

begin
    Result := Int.Insert( Index, Count, Fill ) ;
end ;


function TGeneric_Tape_Media.Delete( Index, Count : int64 ) : TUnified_Exception ;

begin
    Result := Int.Delete( Index, Count ) ;
end ;


function TGeneric_Tape_Media.Get( Index, Count : int64 ; Data : PChar ) : TUnified_Exception ;

begin
    Result := Int.Get( Index, Count, Data ) ;
end ;


function TGeneric_Tape_Media.Put( Index, Count : int64 ; Data : PChar ) : TUnified_Exception ;

begin
    Result := Int.Put( Index, Count, Data ) ;
end ;


function TGeneric_Tape_Media.Get_Byte( Index : int64 ) : byte ;

begin
    Result := int.Get_Byte( Index ) ;
end ;


procedure TGeneric_Tape_Media.Set_Byte( Index : int64 ; Value : byte ) ;

begin
    Int.Set_Byte( Index, Value ) ;
end ;


function TGeneric_Tape_Media.Read_Only : boolean ;

begin
    Result := Int.Read_Only ;
end ;


function TGeneric_Tape_Media.Low_Bound( Subscript : integer ) : int64 ;

begin
    Result := Int.Low_Bound( Subscript ) ;
end ;


function TGeneric_Tape_Media.High_Bound( Subscript : integer ) : int64 ;

begin
    Result := Int.High_Bound( Subscript ) ;
end ;


function TGeneric_Tape_Media.Subscripts : integer ;

begin
    Result := Int.Subscripts ;
end ;


function TGeneric_Tape_Media.Get_BPI : integer ;

begin
    Result := _BPI ;
end ;


procedure TGeneric_Tape_Media.Set_BPI( Value : integer ) ;

begin
    _BPI := Value ;
end ;


function TGeneric_Tape_Media.Get_Length : integer ;

begin
    Result := _Length ;
end ;


procedure TGeneric_Tape_Media.Set_Length( Value : integer ) ;

begin
    _Length := Value ;
end ;


function TGeneric_Tape_Media.Record_Count : int64 ;

begin
    Result := _Max_Record ;
end ;


function TGeneric_Tape_Media.Record_Length( Index : int64 ) : int64 ;

begin
    if( Index = -1 ) then
    begin
        Index := _Current_Record ;
    end ;
    if( Index <> _Current_Record ) then
    begin
        Seek( Index, True, False ) ;
    end ;
    _Position := _Position - _Count_Size ;
    if( _Position < 0 ) then
    begin
        _Position := 0 ;
    end ;
    Result := Read_Count ;
end ;


function TGeneric_Tape_Media.At_BOT : boolean ;

begin
    Result := ( _Position <= _Count_Size ) ;
end ;


function TGeneric_Tape_Media.At_PEOT : boolean ;

var X : extended ;

begin
    Result := ( _Position > High_Bound( 0 ) ) ;
    if( ( not Result ) and ( BPI > 0 ) and ( Length > 0 ) )then
    begin
        X := _Max_Record - _TM_Count - 1 ; // IRG count
        X := ( X * IRG_Length / 1000.0 ) / 12.0 ; // Total size of IRGs, in feet
        X := X + ( _TM_Count * TM_Length / 1000.0 ) / 12.0 ; // Add in tape mark lengths, in feet
        X := X + _Data_Size / BPI / 12.0 ; // Add in size of data for total used length of tape, in feet
        Result := ( X > Length ) ;
    end ;
end ;


function TGeneric_Tape_Media.At_LEOT : boolean ;

begin
    // Setup...
    Result := False ;
    if( At_PEOT ) then
    begin
        exit ;
    end ;

    // Backup to first byte of size
    _Position := _Position - _Count_Size ;
    if( _Position < 0 ) then
    begin
        _Position := 0 ;
    end ;

    // See if two EOT marks in a row...
    if( Read_Count <> 0 ) then
    begin
        _Position := _Position - _Count_Size ;
        exit ;
    end ;
    if( At_PEOT ) then
    begin
        _Position := _Position - _Count_Size ;
        exit ;
    end ;
    if( Read_Count <> 0 ) then
    begin
        _Position := _Position - _Count_Size * 2 ;
        exit ;
    end ;
    _Position := _Position - _Count_Size * 2 ;
    Result := True ;
end ;


function TGeneric_Tape_Media.Position( Records : boolean ) : int64 ;

begin
    if( Records ) then
    begin
        Result := _Current_Record ;
    end else
    begin
        Result := _Position ;
    end ;
end ;


procedure TGeneric_Tape_Media.Seek( Count : int64 ; Records, Relative : boolean ) ;

var This_Count : integer ;
    Saved : int64 ;

begin
    if( Records ) then
    begin
        // Position to record...
        if( Relative ) then
        begin
            Count := _Current_Record + Count ;
        end ;
        if( Count < 0 ) then
        begin
            Count := 0 ;
        end ;
        if( Count > _Max_Record ) then
        begin
            Count := _Max_Record ;
        end ;

        if( _Current_Record > Count ) then // Wants a previous record
        begin
            Seek_BOT ;
        end else
        begin
            _Position := _Current_Record_Position - _Count_Size ;
            if( _Position < 0 ) then
            begin
                _Position := 0 ;
            end ;
        end ;
        while( ( _Current_Record < Count ) and ( not At_PEOT ) ) do
        begin
            This_Count := Read_Count ;
            inc( _Current_Record ) ;
            _Position := _Position + This_Count ;
            if( _After and ( This_Count > 0 ) ) then
            begin
                Read_Count ;
            end ;
        end ;
        Read_Count ;
        _Current_Record_Position := _Position ;
    end else
    begin
        if( Relative ) then
        begin
            _Position := _Position + Count ;
        end else
        begin
            _Position := Count ;
        end ;
        if( _Position < 0 ) then
        begin
            _Position := 0 ;
        end ;
        if( _Position > High_Bound( 0 ) + 1 ) then
        begin
            _Position := High_Bound( 0 ) + 1
        end ;
        if( _Position = 0 ) then
        begin
            _Current_Record := 0 ;
            _Current_Record_Position := 0 ;
            exit ;
        end ;

        Saved := _Position ;

        // Determine record that corresponds to position...
        Seek( 0, False, False ) ;
        while( _Position < Saved ) do
        begin
            _Current_Record_Position := _Position ;
            This_Count := Read_Count ;
            inc( _Current_Record ) ;
            if( _After and ( This_Count > 0 ) ) then
            begin
                Read_Count ;
            end ;
            _Position := _Position + This_Count ;
        end ;

        _Position := Saved ;
    end ;
end ; // TGeneric_Tape_Media.Seek


procedure TGeneric_Tape_Media.Seek_BOT ;

begin
    Seek( 0, False, False ) ;
end ;


procedure TGeneric_Tape_Media.Seek_LEOT ;

var Count : integer ;
    First_TM_Position : int64 ;
    TMs : integer ;

begin
    Seek_BOT ;
    TMs := 0 ;
    First_TM_Position := 0 ;
    while( not At_PEOT ) do
    begin
        Count := Read_Count ;
        inc( _Current_Record ) ;
        if( Count = 0 ) then
        begin
            if( TMs = 0 ) then
            begin
                First_TM_Position := _Position ;
            end ;
            if( _After ) then
            begin
//                Read_Count ;
            end ;
            inc( TMs ) ;
            if( TMs = 2 ) then
            begin
                _Position := First_TM_Position ;
                exit ;
            end ;
        end else
        begin
            _Position := _Position + Count ;
            if( _After ) then
            begin
                Read_Count ;
            end ;
            TMs := 0 ;
        end ;
        _Current_Record_Position := _Position ;
    end ;
end ; // TGeneric_Tape_Media.Seek_LEOT


procedure TGeneric_Tape_Media.Seek_PEOT ;

begin
    _Position := Int.High_Bound( 0 ) + 1 ;
    _Current_Record := _Max_Record + 1 ;
end ;


function TGeneric_Tape_Media.Support_TM : boolean ;

begin
    Result := True ;
end ;


procedure TGeneric_Tape_Media.Add_TM ;

var Dummy : integer ;
    Saved : int64 ;

begin
    _Position := _Position - _Count_Size ;
    if( _Position < 0 ) then
    begin
        _Position := 0 ;
    end ;
    Dummy := 0 ;
    Write( @Dummy, _Count_Size ) ;
    if( _After ) then
    begin
//        Write( @Dummy, _Count_Size ) ;
    end ;
    Write( @Dummy, _Count_Size ) ; // Write a count record to be positioned after
    Saved := _Position ;
    Recalculate ;
    _Position := Saved ;
end ;


function TGeneric_Tape_Media.Read( Data : Pchar ; Count : int64 ) : int64 ;

var I : int64 ;

begin
    I := Position( False ) ;
    Int.Get( I, Count, Data ) ;
    Result := Position( False ) - I ;
end ;


function TGeneric_Tape_Media.Read_Record( var Count : int64 ) : PChar ;

var I : int64 ;

begin
    I := Position( False ) ;
    Count := Record_Length( -1 ) ;
    setlength( Temp_Read, Count ) ;
    Int.Get( I, Count, PChar( Temp_Read ) ) ;
    _Position := _Position + Count ;
    if( _After and ( Count > 0 ) ) then
    begin
        _Position := _Position + _Count_Size ;
    end ;
    _Position := _Position + _Count_Size ; // Position past size to next data record
    _Current_Record_Position := _Position ;
    inc( _Current_Record ) ;
    Result := PChar( Temp_Read ) ;
end ;


function TGeneric_Tape_Media.Write( Data : Pchar ; Count : int64 ) : TUnified_Exception ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    Int.Put( Position( False ), Count, Data ) ;
    _Position := _Position + Count ;
end ;


function TGeneric_Tape_Media.Write_Record( Data : Pchar ; Count : int64 ) : TUnified_Exception ;

var Temp : array[ 0..7 ] of char ;
    Max, Saved : int64 ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    if( Count = 0 ) then
    begin
        //~~~set error
        exit ;
    end ;
    case _Count_Size of
        1 : Max := 255 ;
        2 : Max := 65535 ;
        3 : Max := 16777215 ;
        4 : Max := 4294967295 ;
        5 : Max := 1099511627775 ;
        6 : Max := 281474976710655 ;
        7 : Max := 72057594037927900 ;
        else Max := Count ;
    end ;
    if( Count > Max ) then
    begin
        //~~~set error
        exit ;
    end ;

    _Position := _Position - _Count_Size ; // Backup to beginning of current size record
    if( _Position < 0 ) then
    begin
        _Position := 0 ;
    end ;
    move( Count, Temp, sizeof( Count ) ) ;
    Result := Write( Temp, _Count_Size ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    Result := Write( Data, Count ) ;
    if( Result <> nil ) then
    begin
        exit ;
    end ;
    if( _After ) then
    begin
        Result := Write( Temp, _Count_Size ) ;
    end ;
    fillchar( Temp, sizeof( Temp ), 0 ) ;
    Write( Temp, _Count_Size ) ; // Write a count record to be positioned after
    Saved := _Position ;
    Recalculate ;
    _Position := Saved ;
end ; // TGeneric_Tape_Media.Write_Record


type TSub_Array_Interface = class( TCOM_Array_Interface )
                                private // Instance data...
                                    Master_Array : TMedia ;
                                    Offset : int64 ;
                                    Length : int64 ;

                                public // API...
                                    function Is_Class( _N : PChar ) : boolean ;
                                        override ;

                                    function Get_Byte( Index : int64 ) : byte ;
                                        override ;

                                    procedure Set_Byte( Index : int64 ;
                                        Value : byte ) ;
                                        override ;

                                    function Read_Only : boolean ;
                                        override ;

                                    function Low_Bound( Subscript : integer ) : int64 ;
                                        override ;

                                    function High_Bound( Subscript : integer ) : int64 ;
                                        override ;

                                    function Subscripts : integer ; override ;
                            end ;


function TSub_Array_Interface.Is_Class( _N : PChar ) : boolean ;

var N : string ;

begin
    N := lowercase( string( _N ) ) ;
    Result := ( N = 'tsub_array_interface' ) or ( N = 'tcom_array_interface' ) ;
end ;


function TSub_Array_Interface.Get_Byte( Index : int64 ) : byte ;

begin
    if( ( Index < 0 ) or ( Index >= Length ) ) then
    begin
        Result := 0 ;
        exit ;
    end ;
    Result := Master_Array.Get_Byte( Offset + Index ) ;
end ;


procedure TSub_Array_Interface.Set_Byte( Index : int64 ; Value : byte ) ;

begin
    if( ( Index < 0 ) or ( Index >= Length ) ) then
    begin
        exit ;
    end ;
    Master_Array.Set_Byte( Offset + Index, Value ) ;
end ;


function TSub_Array_Interface.Read_Only : boolean ;

begin
    Result := True ;
end ;


function TSub_Array_Interface.Low_Bound( Subscript : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TSub_Array_Interface.High_Bound( Subscript : integer ) : int64 ;

begin
    Result := Length - 1 ;
end ;


function TSub_Array_Interface.Subscripts : integer ;

begin
    Result := 1 ;
end ;



// TGeneric_Tape_Presenter methods...

// Constructors and destructors...

constructor TGeneric_Tape_Presenter.Create( Parent : THandle ;
    _Array : TMedia ; Header : PCEF_Media_File_Header ) ;

var _Header : PCEF_Tape_Media_Header ;

begin
    inherited Create ;

    Panel := TGeneric_Tape_Form.Create( Application ) ;
    Panel.ParentWindow := Parent ;
    Panel.Tape := TGeneric_Tape_Media.Create( _Array ) ;
    if( Header <> nil ) then
    begin
        _Header := PCEF_Tape_Media_Header( Header ) ;
        Panel.Tape.Set_BPI( _Header.BPI ) ;
        Panel.Tape.Set_Length( _Header.Length ) ;
        Panel.Tape.Set_Count_Length( _Header.Size_Length ) ;
        Panel.Tape.Set_After( _Header.After ) ;
        Panel.Tape.Set_IRG_Length( _Header.IRG_Length ) ;
        Panel.Tape.Set_TM_Length( _Header.TM_Length ) ;
    end ;
    Panel._Array := _Array ;
    Panel.Top := 0 ;
    Panel.Left := 0 ;
    Panel.Get_Record( 1 ) ;
end ;


// API...

function TGeneric_Tape_Presenter.Terminate : TUnified_Exception ;

begin
    Result := nil ;
    Panel.Free ;
    Panel := nil ;
    Free ;
end ;


procedure TGeneric_Tape_Presenter.Redraw ;

begin
    Panel.Panel.Repaint ;
end ;


procedure TGeneric_Tape_Presenter.Set_Bounds( Width, Height : integer ) ;

begin
    Panel.SetBounds( 0, 0, Width, Height ) ;
    Panel.Repaint ;
    Redraw ;
end ;


procedure TGeneric_Tape_Presenter.Get_Header( var Header : PCEF_Media_File_Header ) ;

var H : PCEF_Tape_Media_Header ;

begin
    H := PCEF_Tape_Media_Header( Header ) ;
    H.Prefix := 65535 ;
    H.ID := 255 ;
    H.Facility := 123 ; // Tape
    H.Version := 10 ;
    H.Format_Name := 'Tape image' ;
    H.BPI := 0 ;
    H.Length := 0 ;
    H.Size_Length := 4 ;
    H.After := True ;
    fillchar( H.Reserved, sizeof( H.Reserved ), 0 ) ;
end ;


// TGeneric_Tape_Form methods...

procedure TGeneric_Tape_Form.FormCreate(Sender: TObject);

begin
    Panel := TData_Display.Create( Owner ) ;
    Panel.Parent := Data_Panel ;
    Panel.Align := alClient ;
    Sub_Array := TSub_Array_Interface.Create ;
end ;


procedure TGeneric_Tape_Form.Get_Record( Num : int64 ) ;

var Count : integer ;
    Current_Record_Position : int64 ;

begin
    if( ( Tape = nil ) or ( Num < 0 ) or ( Num > Tape.Record_Count ) ) then
    begin
        exit ;
    end ;

    // Position to requested record...
    Current_Record := Num - 1 ;
    Tape.Seek( Current_Record, True, False ) ;
    Current_Record_Position := Tape.Position( False ) ;
    Count := Tape.Record_Length( Current_Record ) ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;

    // Display data for the record...
    Record_Size_Label.Caption := inttostr( Count ) ;

    TSub_Array_Interface( Sub_Array ).Offset := Current_Record_Position ;
    TSub_Array_Interface( Sub_Array ).Length := Count ;
    Panel._Array := Sub_Array ;
    Panel.Repaint ;
end ; // TGeneric_Tape_Form.Get_Record


procedure TGeneric_Tape_Form.Set_Array( A : TMedia ) ;

begin
    __Array := A ;
    TSub_Array_Interface( Sub_Array ).Master_Array := A ;
    Record_Selector.MaxValue := 0 ;
    if( A <> nil ) then
    begin
        TGeneric_Tape_Media( Tape ).Int := A ;
        Record_Selector.MaxValue := Tape.Record_Count ;
    end ;
end ;


procedure TGeneric_Tape_Form.Rewind_ButtonClick(Sender: TObject) ;

begin
    Get_Record( 1 ) ;
    Record_Selector.Value := 1 ;
end ;


procedure TGeneric_Tape_Form.FF_ButtonClick(Sender: TObject) ;

begin
    Get_Record( Tape.Record_Count ) ;
    Record_Selector.Value := Tape.Record_Count ;
end ;


procedure TGeneric_Tape_Form.Record_SelectorChange(Sender: TObject);

begin
    Get_Record( Record_Selector.Value ) ;
end ;


procedure TGeneric_Tape_Form.Add_Data_ButtonClick( Sender : TObject ) ;

var S : string ;

begin
    if( Tape_Add_Data_Dialog.ShowModal = mrOK ) then
    begin
        Tape.Seek_LEOT ;
        S := Tape_Add_Data_Dialog.Memo1.Text ;
        while( length( S ) > 0 ) do
        begin
            while( length( S ) < Tape_Add_Data_Dialog.Min_Block_Size.Value ) do
            begin
                S := S + #0 ;
            end ;
            if( length( S ) > Tape_Add_Data_Dialog.Max_Block_Size.Value ) then
            begin
                Tape.Write_Record( PChar( S ), Tape_Add_Data_Dialog.Max_Block_Size.Value ) ;
                S := copy( S, Tape_Add_Data_Dialog.Max_Block_Size.Value + 1, length( S ) ) ;
            end else
            begin
                Tape.Write_Record( PChar( S ), length( S ) ) ;
                S := '' ;
            end ;
        end ;
    end ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
end ;


procedure TGeneric_Tape_Form.SpeedButton1Click(Sender: TObject);

begin
    Tape.Seek_LEOT ;
    Tape.Add_TM ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
end ;


end.
