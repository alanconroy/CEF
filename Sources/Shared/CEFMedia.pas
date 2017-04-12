{
        Program Name : CEFMedia
        Package Name : CEF
        Purpose      : CEF Virtual Media support
        Institution  :
        Date Written : 27-May-2010
        Written By   : Alan Conroy
        Version      : 2.0

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

          This unit contains common definitions for the CEF Media specification.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CEFMedia ;

interface

uses // C&C...
     ArrayInt, // TCOM_Array_Interface
     _UE, // TUnified_Exception

     // CEF...
     CEF ; // PCEF_Media_File_Header

     { Used to display data.  Descendants of this class are used to interpret
       and display specific data. }
type TMedia = class( TCOM_Array_Interface )
                  public // API...
                      // Insert Count Items starting at index Index, filling with Fill.
                      function Insert( Index, Count : int64 ; Fill : byte ) : TUnified_Exception ;
                          virtual ; stdcall ; abstract ;

                      // Delete Count items starting at index Index (inclusive), truncating file by Count.
                      function Delete( Index, Count : int64 ) : TUnified_Exception ;
                          virtual ; stdcall ; abstract ;

                      // Reads Count bytes from media, starting at index Index, into buffer Data
                      function Get( Index, Count : int64 ; Data : PChar ) : TUnified_Exception ;
                          virtual ; stdcall ; abstract ;

                      // Writes count bytes from Data to media, starting at index Index.
                      function Put( Index, Count : int64 ; Data : PChar ) : TUnified_Exception ;
                          virtual ; stdcall ; abstract ;
              end ; // TMedia

     TDisk_Media = class( TMedia )
                       public // API...
                           function Sector_Count : int64 ;
                               virtual ; stdcall ; abstract ;
                           // Count of records on the tape (includes tape marks)

                           function Sector_Size : int64 ;
                               virtual ; stdcall ; abstract ;
                           { Returns length of record Index.  If result is 0,
                             then there is a tape mark at the position. }

                           function Raw_Sector_Size : int64 ;
                               virtual ; stdcall ; abstract ;
                           { Returns length of the raw sector data.  This
                             includes data and all surrounding formatting data.
                             If result is 0, then there is a tape mark at the
                             position. }

                           function Read( Data : Pchar ; Sector, Count : int64 ) : int64 ;
                               virtual ; stdcall ; abstract ;
                           { Read Count bytes into Data, starting at Sector.
                             Returns the count actually read. }

                           function Raw_Read( Data : Pchar ; Sector, Count : int64 ) : int64 ;
                               virtual ; stdcall ; abstract ;
                           { Read Count bytes into Data, starting at Sector.
                             Returns the count actually read.  If count is less
                             than 0, one full sector is read. }

                           function Write( Data : Pchar ; Sector, Count : int64 ) : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;
                           { Write Count bytes from Data to disk.  Count must be
                             larger than 0. }

                           function Raw_Write( Data : Pchar ; Sector, Count : int64 ) : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;
                           { Write Count bytes from Data to disk.  Count must be
                             larger than 0.  This is a raw operation. }
                   end ; // TDisk_Media

     TTape_Media = class( TMedia )
                       public // API...
                           function Get_BPI : integer ;
                               virtual ; stdcall ; abstract ;
                           // Retrive tape's BPI setting (0 indicates any).

                           procedure Set_BPI( Value : integer ) ;
                               virtual ; stdcall ; abstract ;
                           // Set tape's BPI setting (0 indicates any).

                           function Get_Length : integer ;
                               virtual ; stdcall ; abstract ;
                           // Returns length of tape (in feet).  0 = infinite

                           procedure Set_Length( Value : integer ) ;
                               virtual ; stdcall ; abstract ;
                           // Sets length of tape (in feet).  0 means infinite.

                           function Get_Count_Length : integer ;
                               virtual ; stdcall ; abstract ;
                           // Returns size of tape record size records.

                           procedure Set_Count_Length( Value : integer ) ;
                               virtual ; stdcall ; abstract ;
                           // Sets size of tape record size records.

                           function Get_After : boolean ;
                               virtual ; stdcall ; abstract ;
                           // Returns True if record size records follow data record

                           procedure Set_After( Value : boolean ) ;
                               virtual ; stdcall ; abstract ;
                           // Set to True if record size records follow data record

                           function Get_IRG_Length : integer ;
                               virtual ; stdcall ; abstract ;
                           // Set inter-record gap length

                           procedure Set_IRG_Length( Value : integer ) ;
                               virtual ; stdcall ; abstract ;
                           // Set inter-record gap length

                           function Get_TM_Length : integer ;
                               virtual ; stdcall ; abstract ;
                           // Set tape mark length

                           procedure Set_TM_Length( Value : integer ) ;
                               virtual ; stdcall ; abstract ;
                           // Set tape mark length

                           function Record_Count : int64 ;
                               virtual ; stdcall ; abstract ;
                           // Count of records on the tape (includes tape marks)

                           function Record_Length( Index : int64 ) : int64 ;
                               virtual ; stdcall ; abstract ;
                           { Returns length of record Index.  If result is 0,
                             then there is a tape mark at the position. }

                           function At_BOT : boolean ;
                               virtual ; stdcall ; abstract ;
                           { True if at BOT }

                           function At_PEOT : boolean ;
                               virtual ; stdcall ; abstract ;
                           { True if at physical EOT}

                           function At_LEOT : boolean ;
                               virtual ; stdcall ; abstract ;
                           { True if at logical EOT }

                           function Position( Records : boolean ) : int64 ;
                               virtual ; stdcall ; abstract ;
                           // Current position on tape (byte or record offset)

                           procedure Seek( Count : int64 ; Records, Relative : boolean ) ;
                               virtual ; stdcall ; abstract ;
                           { Moves current position on tape.  Parameters:
                                 Count : Amount/where to move.
                                 Records: True to interpret Count as record count,
                                          False to interpret as byte count.
                                 Relative: True to move relative to current
                                           position.  False to move to absolute
                                           position.
                           }

                           procedure Seek_BOT ; virtual ; stdcall ; abstract ;
                           // Move to beginning of tape.

                           procedure Seek_LEOT ; virtual ; stdcall ; abstract ;
                           // Move to logical end of tape.

                           procedure Seek_PEOT ; virtual ; stdcall ; abstract ;
                           // Move to physical end of tape.

                           function Support_TM : boolean ;
                               virtual ; stdcall ; abstract ;
                           // Returns true if tape marks supported on tape.

                           procedure Add_TM ; virtual ; stdcall ; abstract ;
                           // Add a tape mark to the tape.

                           function Read( Data : Pchar ; Count : int64 ) : int64 ;
                               virtual ; stdcall ; abstract ;
                           { Read Count bytes into Data.  Returns the count
                             actually read.  Cannot read beyond current record. }

                           function Read_Record( var Count : int64 ) : PChar ;
                               virtual ; stdcall ; abstract ;
                           { Returns current reoord.  Count is ignored
                             on call.  On return, it is the size of the
                             read record. }

                           function Write( Data : Pchar ; Count : int64 ) : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;
                           { Write Count bytes from Data to tape.  Count must be
                             larger than 0.  This is a raw operation and does
                             not prefix the data with a count or any other such
                             processing. }

                           function Write_Record( Data : Pchar ; Count : int64 ) : TUnified_Exception ;
                               virtual ; stdcall ; abstract ;
                           { Write a record of Count bytes from Data to tape.
                             Count must be larger than 0. }

                           property After : boolean
                               read Get_After
                               write Set_After ;

                           property BPI : integer
                               read Get_BPI
                               write Set_BPI ;

                           property IRG_Length : integer
                               read Get_IRG_Length
                               write Set_IRG_Length ;

                           property Length : integer
                               read get_Length
                               write Set_Length ;

                           property TM_Length : integer
                               read Get_TM_Length
                               write Set_TM_Length ;
                               
                           property Count_Length : integer
                               read Get_Count_Length
                               write Set_Count_Length ;
                   end ; // TTape_Media

     // Media file header structures (256 bytes)...
type TCEF_Media_File_Header = record // Overlay with other media headers
                                  Prefix : word ; // -1
                                  ID : byte ; // 255 = App foundation
                                  Facility : longint ; // Type of file
                                  Version : longint ; // File format version
                                  Reserved : array[ 11..255 ] of byte ;
                              end ;
     PCEF_Media_File_Header = ^TCEF_Media_File_Header ;

     TCEF_Tape_Media_Header = record
                                  Prefix : word ; // -1
                                  ID : byte ; // 255 = App foundation
                                  Facility : longint ; // 123 = CEF Tape media file
                                  Version : longint ; // 10 = V1.0
                                  Format_Name : string[ 63 ] ; // Tape data format (eg "DOS-11 Tape")
                                  BPI : longint ; // Density of tape in linear bpi (0=not specified)
                                  Length : longint ; // Length of recordable tape, in feet (0=infinite)
                                  Size_Length : byte ; // Length of size records, in bytes (1-8)
                                  After : bytebool ; // True if size record follows AND preceeds data (otherwise only before)
                                  IRG_Length : longint ; // Length of IRGs, in 1/1000" (eg 750 = .75")
                                  TM_Length : longint ; // Length of tape marks, in 1/1000" (eg 10000 = 10")
                                  Reserved : array[ 93..255 ] of byte ;
                              end ;
     PCEF_Tape_Media_Header = ^TCEF_Tape_Media_Header ;

     TCEF_Disk_Media_Header = record
                                  Prefix : word ; // -1
                                  ID : byte ; // 255 = App foundation
                                  Facility : longint ; // 124 = CEF Tape media file
                                  Version : longint ; // 10 = V1.0
                                  Format_Name : string[ 63 ] ; // Disk data format (eg "ODS1")
                                  Max_Size : int64 ; // Maximum disk size, in cooked data bytes (this, divided by Sector_Size, provides the max number of sectors)
                                  Sector_Lead_In_Size : longint ; // Length of formatting data before actual sector data: 0 = none, -1 = variable
                                  Sector_Lead_Out_Size : longint ; // Length of formatting data after actual sector data: 0 = none, -1 = variable
                                  // Note that if both sector lead-in and loead-out are variable, the disk can only be read in raw mode
                                  Track_Lead_In_Size : longint ; // Length of formatting data before first sector on track: 0 = none, -1 = variable
                                  Track_Lead_Out_Size : longint ; // Length of formatting data after last sector on track: 0 = none, -1 = variable
                                  // Note that if both track lead-in and loead-out are variable, the disk can only be read in raw mode
                                  Sector_Size : longint ; // Length of actual (cooked) data for each sector, in bytes
                                  Heads : byte ; // Number of heads/surfaces (0 = unknown)
                                  Tracks : longint ; // Number of tracks/cylinder per head (0 = unknown)
                                  Sectors : longint ; // Number of sectors per track (0 = unknown)
                                  Errors : int64 ; // Pointer to on-disk error data (0 = none)
                                  Default_Data : byte ; // Data to return for phantom sectors
                                  Default_Track_Lead_In : int64 ; // Pointer to default track lead-in data (must be 0 if variable or not used, or if Sectors [per track] is 0)
                                  Default_Track_Lead_Out : int64 ; // Pointer to default track lead-out data (must be 0 if variable or not used or if Sectors [per track] is 0)
                                  Default_Sector_Lead_In : int64 ; // Pointer to default sector lead-in data (must be 0 if variable or not used)
                                  Default_Sector_Lead_Out : int64 ; // Pointer to default sector lead-out data (must be 0 if variable or not used)
                                  Index : int64 ; // Pointer to data index (0 = pre-allocated, which isn't allowed for disks with variable lead-in/out)
                                  Flags : longint ; // and 1 = pre-allocated, Index points to actual data
                                                    // and 2 = Composite.  Data following header is a series of null-terminated filenames, indicating disk data files - in order of sector ranges
                                                    // and 4+ = reserved
                                  Mask : string[ 63 ] ; // Bitmask for addressing: [1] is bit 1, [2] is bit 2, etc.
                                                        // Each element is one of the following:
                                                        //   (space) = Unused
                                                        //   H = head/surface
                                                        //   C = cylinder/track
                                                        //   S = sector
                                                        // For instance, "HHCCCSSSSSSSS" indicates that the lowest 2 bits specify the head (0-3),
                                                        // bits 3-5 indicate cylinder (0-7), and bits 6-13 indicate sector (0-255).
                                                        // If all elements are spaces, the disk cannot be accessed via Head/Cylinder
                                  Flaws : int64 ; // Pointer to flaw data (0 = none)
                                  SN_Size : longint ; // Size of serial number data (0 = none)
                                  Serial_Number : int64 ; // Pointer to pack serial-number data (must be 0 if SN_Size is 0)
                                  Reserved : array[ 253..255 ] of byte ;
                              end ;
     PCEF_Disk_Media_Header = ^TCEF_Disk_Media_Header ;

     TCEF_Paper_Media_Header = record
                                  Prefix : word ; // -1
                                  ID : byte ; // 255 = App foundation
                                  Facility : longint ; // 125 = CEF Paper media file
                                  Version : longint ; // 10 = V1.0
                                  Typ : longint ; // Paper type: 0 = plain, otherwise color
                                  Height : longint ; // Paper height in 1/10 inches
                                  Width : longint ; //  Paper width in 1/10 inches
                                  Max_Page : longint ; // Maximum page (0=infinite)
                                  Bleed : longint ; // Bleed factor
                                  Reserved : array[ 31..255 ] of byte ;
                              end ;
     PCEF_Paper_Media_Header = ^TCEF_Paper_Media_Header ;

     // Data after header is printed text with nulls as escapes.  Escape codes:
     // Data after escape null (media control codes):
const MCC_Null = 0 ; // Literal null
const MCC_Clear_VT = 1 ; // Clear VT
const MCC_VT = 2 ; // VT (next 4 bytes = position)
const MCC_Clear_HT = 3 ; // Clear HT
const MCC_HT = 4 ; // HT (next 4 bytes = position)
const MCC_LPI = 6 ; // LPI (next 4 bytes = value)
const MCC_CPI = 7 ; // CPI (next 4 bytes = value)
const MCC_Change_Font = 8 ; // Change font (name followed by ascii 0)
const MCC_Advance_Line = 9 ; // Advance one line
const MCC_Advance_Page = 10 ; // Advance one page

type TPresenter = class
                      public // API...
                          procedure Redraw ; virtual ; stdcall ; abstract ;
                          procedure Set_Bounds( Width, Height : integer ) ;
                              virtual ; stdcall ; abstract ;
                          function Terminate : TUnified_Exception ;
                              virtual ; stdcall ; abstract ;
                          procedure Get_Header( var Header : PCEF_Media_File_Header ) ;
                              virtual ; stdcall ; abstract ;
                  end ;

type TPresentation_Manager = class
                                 public // API...
                                     function Get_Presenter( S : PChar ;
                                         Parent : THandle ; _Array : TMedia ;
                                         Header : PCEF_Media_File_Header ) : TPresenter ;
                                         virtual ; stdcall ; abstract ;
                                     { Returns a presenter for the passed format
                                       name.  If format name is not supported,
                                       it returns nil. }

                                     function Get_Name( Index : longint ) : PChar ;
                                         virtual ; stdcall ; abstract ;
                                     { Returns a name of a supported media
                                       format.  Index is 0 for the first name.
                                       If Index is out of range, the function
                                       returns nil. }

                                     function Get_Path( Index : longint ) : PChar ;
                                         virtual ; stdcall ; abstract ;
                                     { Returns the path of a supported media
                                       format.  Index is 0 for the first format.
                                       If Index is out of range, the function
                                       returns nil.  This path indicates the
                                       type of media from least- to most-
                                       specific, separated with backslashes.
                                       For instance: "Disk\NTFS",
                                       "Disk\Hard\ODS-1" or "Tape\ANSI".  This
                                       is solely to provide the UI with a means
                                       of presenting all the possible options to
                                       the user in an organized way. }

                                     function Version : integer ;
                                         virtual ; stdcall ; abstract ;
                                     { Returns the version of the specification
                                       that this object conforms to. }
                             end ;
                             
implementation

end.

