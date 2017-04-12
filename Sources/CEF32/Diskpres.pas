{
        Program Name : DiskPres
        Package Name : CEF32
        Purpose      : Generic Disk displayer
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

          This form is used to display formatted disk data.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit DiskPres ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, Buttons, StdCtrls, Spin,

     // CEF32...
     CEF, // PCEF_Media_File_Header
     ArrayInt, // TCOM_Array_Interface
     _UEHDefs, // TUEC
     CEFMedia, // TPresenter
     DataDis ; // TData_Display


type TGeneric_Disk_Form = class(TForm)
            Command_Panel: TPanel;
            Data_Panel: TPanel;
            Label1: TLabel;
            Label2: TLabel;
            Record_Size_Label: TLabel;
            Current_Record_Label: TLabel;
            Record_Selector: TSpinEdit;
            Splitter1: TSplitter;
            procedure FormCreate(Sender: TObject);
            procedure Record_SelectorChange(Sender: TObject);
            procedure SpeedButton1Click(Sender: TObject);

          private // Instance data...
              __Array : TMedia ;
              Current_Sector : int64 ;
              Panel : TData_Display ;
              Sub_Array : TCOM_Array_Interface ;
              Disk : TDisk_Media ;

          protected // Property handlers...
              procedure Set_Array( A : TMedia ) ;

          public // API...
              procedure Get_Record( Num : int64 ) ;
              property _Array : TMedia
                  read __Array
                  write Set_Array ;
          end ; // TGeneric_Disk_Form

type TGeneric_Disk_Presenter = class( TPresenter )
                                   public // Constructors and destructors...
                                       constructor Create( Parent : THandle ;
                                           _Array : TMedia ;
                                           Header : PCEF_Media_File_Header ) ;

                                   private // Instance data...
                                       Panel : TGeneric_Disk_Form ;
                                       __Array : TMedia ;

                                   public // API...
                                       function Terminate : TUEC ; override ;
                                       procedure Redraw ; override ;
                                       procedure Set_Bounds( Width, Height : integer ) ;
                                           override ;
                                       procedure Get_Header( var Header : PCEF_Media_File_Header ) ;
                                           override ;
                               end ;

{ The Generic_Disk_Media object manages disk image files.

  The disk can be read or written in byte (raw) or sector (cooked) mode.  I/O in
  sector mode deals solely with the data "contents".  In raw mode, it includes
  all  (if any exists).
}
type TGeneric_Disk_Media = class( TDisk_Media )
                               public // Constructors and destructors...
                                   constructor Create( _Int : TMedia ) ;

                               private // Instance data...
                                   _Int : TMedia ;
                                   _Sector_Size : integer ; // Size of sectors (in byte)
                                   Temp_Read : string ;

                               public // API...
                                   // Insert Count Items starting at index Index, filling with Fill.
                                   function Insert( Index, Count : int64 ; Fill : byte ) : TUEC ;
                                       override ;

                                   // Delete Count items starting at index Index (inclusive), truncating file by Count.
                                   function Delete( Index, Count : int64 ) : TUEC ;
                                       override ;

                                   // Reads Count bytes from media, starting at index Index, into buffer Data
                                   function Get( Index, Count : int64 ; Data : PChar ) : TUEC ;
                                       override ;

                                   // Writes count bytes from Data to media, starting at index Index.
                                   function Put( Index, Count : int64 ; Data : PChar ) : TUEC ;
                                       override ;

                                   function Sector_Count : int64 ; override ;
                                   // Count of sectors on the disk

                                   function Sector_Size : int64 ; override ;
                                   // Returns sector data size in bytes.

                                   function Raw_Sector_Size : int64 ; override ;
                                   { Returns length of the raw sector data.  This
                                     includes data and all surrounding
                                     formatting data. }

                                   function Read( Data : Pchar ;
                                       Sector, Count : int64 ) : int64 ;
                                       override ;
                                   { Read Count bytes into Data, starting at Sector.
                                     Returns the count actually read. }

                                   function Raw_Read( Data : Pchar ;
                                       Sector, Count : int64 ) : int64 ;
                                       override ;
                                   { Read Count bytes into Data, starting at Sector.
                                     Returns the count actually read.  If count is less
                                     than 0, one full sector is read. }

                                   function Write( Data : Pchar ;
                                       Sector, Count : int64 ) : TUEC ;
                                       override ;
                                   { Write Count bytes from Data to disk.  Count must be
                                     larger than 0. }

                                   function Raw_Write( Data : Pchar ;
                                       Sector, Count : int64 ) : TUEC ;
                                       override ;
                                   { Write Count bytes from Data to disk.  Count must be
                                     larger than 0.  This is a raw operation. }

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

                                   property Int : TMedia
                                       read _Int
                                       write _Int ;
                           end ; // TGeneric_Disk_Media

implementation

{$R *.dfm}

// TGeneric_Disk_Media...

// Constructors and destructors...

constructor TGeneric_Disk_Media.Create( _Int : TMedia ) ;

begin
    inherited Create ;

    Int := _Int ;
end ;


// API...

function TGeneric_Disk_Media.Insert( Index, Count : int64 ; Fill : byte ) : TUEC ;

begin
    Result := Int.Insert( Index, Count, Fill ) ;
end ;


function TGeneric_Disk_Media.Delete( Index, Count : int64 ) : TUEC ;

begin
    Result := Int.Delete( Index, Count ) ;
end ;


function TGeneric_Disk_Media.Get( Index, Count : int64 ; Data : PChar ) : TUEC ;

begin
    Result := Int.Get( Index, Count, Data ) ;
end ;


function TGeneric_Disk_Media.Put( Index, Count : int64 ; Data : PChar ) : TUEC ;

begin
    Result := Int.Put( Index, Count, Data ) ;
end ;


function TGeneric_Disk_Media.Sector_Count : int64 ;

begin
    Result := Int.High_Bound( 0 ) div Raw_Sector_Size ;
end ;


function TGeneric_Disk_Media.Sector_Size : int64 ;

begin
    REsult := _Sector_Size ;
end ;


function TGeneric_Disk_Media.Raw_Sector_Size : int64 ;

begin
    Result := Sector_Size ;
end ;


function TGeneric_Disk_Media.Get_Byte( Index : int64 ) : byte ;

begin
    Result := int.Get_Byte( Index ) ;
end ;


procedure TGeneric_Disk_Media.Set_Byte( Index : int64 ; Value : byte ) ;

begin
    Int.Set_Byte( Index, Value ) ;
end ;


function TGeneric_Disk_Media.Read_Only : boolean ;

begin
    Result := Int.Read_Only ;
end ;


function TGeneric_Disk_Media.Low_Bound( Subscript : integer ) : int64 ;

begin
    Result := Int.Low_Bound( Subscript ) ;
end ;


function TGeneric_Disk_Media.High_Bound( Subscript : integer ) : int64 ;

begin
    Result := Int.High_Bound( Subscript ) ;
end ;


function TGeneric_Disk_Media.Subscripts : integer ;

begin
    Result := Int.Subscripts ;
end ;


function TGeneric_Disk_Media.Read( Data : Pchar ; Sector, Count : int64 ) : int64 ;

begin
    Int.Get( Sector, Count, Data ) ;
    Result := Count ;
end ;


function TGeneric_Disk_Media.Raw_Read( Data : Pchar ; Sector, Count : int64 ) : int64 ;

var I : int64 ;

begin
    setlength( Temp_Read, Count ) ;
    Int.Get( I, Count, PChar( Temp_Read ) ) ;
    Result := Count ;
end ;


function TGeneric_Disk_Media.Write( Data : Pchar ; Sector, Count : int64 ) : TUEC ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    Int.Put( Sector * Sector_Size, Count, Data ) ;
end ;


function TGeneric_Disk_Media.Raw_Write( Data : Pchar ; Sector, Count : int64 ) : TUEC ;

var Temp : array[ 0..7 ] of char ;
    Max, Saved : int64 ;

begin
    fillchar( Result, sizeof( Result ), 0 ) ;
    if( Count = 0 ) then
    begin
        //~~~set error
        exit ;
    end ;

    Result := Write( Data, Sector, Count ) ;
    if( Result.Code <> 0 ) then
    begin
        exit ;
    end ;
    fillchar( Temp, sizeof( Temp ), 0 ) ;
end ; // TGeneric_Disk_Media.Write_Record


type TSub_Array_Interface = class( TCOM_Array_Interface )
                                private // Instance data...
                                    Master_Array : TMedia ;
                                    Offset : int64 ;
                                    Length : int64 ;

                                public // API...
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



// TGeneric_Disk_Presenter methods...

// Constructors and destructors...

constructor TGeneric_Disk_Presenter.Create( Parent : THandle ;
    _Array : TMedia ; Header : PCEF_Media_File_Header ) ;

var _Header : PCEF_Disk_Media_Header ;

begin
    inherited Create ;

    Panel := TGeneric_Disk_Form.Create( Application ) ;
    Panel.ParentWindow := Parent ;
    Panel.Disk := TGeneric_Disk_Media.Create( _Array ) ;
    if( Header <> nil ) then
    begin
        _Header := PCEF_Disk_Media_Header( Header ) ;
    end ;
    Panel._Array := _Array ;
    Panel.Top := 0 ;
    Panel.Left := 0 ;
    Panel.Get_Record( 1 ) ;
end ;


// API...

function TGeneric_Disk_Presenter.Terminate : TUEC ;

begin
    Panel.Free ;
    Panel := nil ;
    Free ;
end ;


procedure TGeneric_Disk_Presenter.Redraw ;

begin
    Panel.Panel.Repaint ;
end ;


procedure TGeneric_Disk_Presenter.Set_Bounds( Width, Height : integer ) ;

begin
    Panel.SetBounds( 0, 0, Width, Height ) ;
    Panel.Repaint ;
    Redraw ;
end ;


procedure TGeneric_Disk_Presenter.Get_Header( var Header : PCEF_Media_File_Header ) ;

var H : PCEF_Disk_Media_Header ;

begin
    H := PCEF_Disk_Media_Header( Header ) ;
    fillchar( H, sizeof( H ), 0 ) ;
    H.Prefix := 65535 ;
    H.ID := 255 ;
    H.Facility := 124 ; // Disk
    H.Version := 10 ;
    H.Format_Name := 'Disk image' ;
    H.Sector_Size := 512 ;
    H.Max_Size := __Array.High_Bound( 0 ) div H.Sector_Size ;
    fillchar( H.Mask, sizeof( H.Mask ), ord( 'S' ) ) ;
    H.Mask[ 0 ] := #63 ;
end ;


// TGeneric_Disk_Form methods...

procedure TGeneric_Disk_Form.FormCreate(Sender: TObject);

begin
    Panel := TData_Display.Create( Owner ) ;
    Panel.Parent := Data_Panel ;
    Panel.Align := alClient ;
    Sub_Array := TSub_Array_Interface.Create ;
end ;


procedure TGeneric_Disk_Form.Get_Record( Num : int64 ) ;

var Count : integer ;
    Current_Record_Position : int64 ;

begin
    if( ( Disk = nil ) or ( Num < 0 ) or ( Num > Disk.Sector_Count ) ) then
    begin
        exit ;
    end ;

    // Position to requested record...
    Current_Sector := Num - 1 ;
    Current_Record_Position := Current_Sector ;
    Count := Disk.Sector_Size ;
    Current_Record_Label.Caption := ' of ' + inttostr( Disk.Sector_Count ) ;

    // Display data for the record...
    Record_Size_Label.Caption := inttostr( Count ) ;

    TSub_Array_Interface( Sub_Array ).Offset := Current_Record_Position ;
    TSub_Array_Interface( Sub_Array ).Length := Count ;
    Panel._Array := Sub_Array ;
    Panel.Repaint ;
end ; // TGeneric_Disk_Form.Get_Record


procedure TGeneric_Disk_Form.Set_Array( A : TMedia ) ;

begin
    __Array := A ;
    TSub_Array_Interface( Sub_Array ).Master_Array := A ;
    Record_Selector.MaxValue := 0 ;
    if( A <> nil ) then
    begin
        TGeneric_Disk_Media( Disk ).Int := A ;
        Record_Selector.MaxValue := Disk.Sector_Count ;
    end ;
end ;


procedure TGeneric_Disk_Form.Record_SelectorChange(Sender: TObject);

begin
    Get_Record( Record_Selector.Value ) ;
end ;


procedure TGeneric_Disk_Form.SpeedButton1Click(Sender: TObject);

begin
    Current_Record_Label.Caption := ' of ' + inttostr( Disk.Sector_Count ) ;
end ;


end.

