/*
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
        *    DATE        BY          REASON                         *
        *                                                           *
        *                                                           *
        *************************************************************

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

*/

#include "cef.h"
#include "arrayint.h"

     /* Used to display data.  Descendants of this class are used to interpret
       and display specific data. */
class TMedia : TArray_Interface
{
    public: // API...
        // Insert Count Items starting at index Index, filling with Fill.
        virtual __stdcall TUEC Insert( int64 Index, int64 Count, unsigned char Fill ) = 0 ;

		// Delete Count items starting at index Index (inclusive), truncating file by Count.
        virtual __stdcall TUEC Delete( int64 Index, int64 Count ) = 0 ;

        // Reads Count bytes from media, starting at index Index, into buffer Data
        virtual __stdcall TUEC Get( Index, Count : int64, char* Data ) = 0 ;

        // Writes count bytes from Data to media, starting at index Index.
        virtual __stdcall TUEC Put( Index, Count : int64, char* Data ) = 0 ;
}

class TTape_Media : TMedia
{
    public // API...
       virtual __stdcall int function Get_BPI() = 0 ;
       // Retrive tape's BPI setting (0 indicates any).

       virtual __stdcall void Set_BPI( int Value ) = 0 ;
       // Set tape's BPI setting (0 indicates any).

       virtual __stdcall int Get_Length() = 0 ;
       // Returns length of tape (in feet).  0 = infinite

       virtual __stdcall void Set_Length( int Value ) = 0 ;
       // Sets length of tape (in feet).  0 means infinite.

       virtual __stdcall int Get_Count_Length() = 0 ;
       // Returns size of tape record size records.

       virtual __stdcall void Set_Count_Length( int Value ) = 0 ;
       // Sets size of tape record size records.

       virtual __stdcall bool Get_After( = 0 ;)
       // Returns True if record size records follow data record

       virtual __stdcall void Set_After( bool Value ) = 0 ;
       // Set to True if record size records follow data record

       virtual __stdcall int Get_IRG_Length() = 0 ;
       // Set inter-record gap length

       virtual __stdcall void Set_IRG_Length( int Value ) = 0 ;
       // Set inter-record gap length

       virtual __stdcall int Get_TM_Length() = 0 ;
       // Set tape mark length

       virtual __stdcall void Set_TM_Length( int Value ) = 0 ;
       // Set tape mark length

       virtual __stdcall int64 Record_Count() = 0 ;
       // Count of records on the tape (includes tape marks)

       virtual __stdcall int64 Record_Length( Index : int64 ) = 0 ;
       /* Returns length of record Index.  If result is 0,
         then there is a tape mark at the position. */

       virtual __stdcall bool At_BOT() = 0 ;
       // True if at BOT

       virtual __stdcall bool At_PEOT() = 0 ;
       // True if at physical EOT

       virtual __stdcall bool At_LEOT() = 0 ;
       // True if at logical EOT

       virtual __stdcall int64 Position( bool Records ) = 0 ;
       // Current position on tape (byte or record offset)

       virtual __stdcall void Seek( int64 Count, bool Records, bool Relative ) = 0 ;
       /* Moves current position on tape.  Parameters:
             Count : Amount/where to move.
             Records: True to interpret Count as record count,
                      False to interpret as byte count.
             Relative: True to move relative to current
                       position.  False to move to absolute
                       position.
       */

	   virtual __stdcall void Seek_BOT() = 0 ;
       // Move to beginning of tape.

       virtual __stdcall void Seek_LEOT() = 0 ;
       // Move to logical end of tape.

       virtual __stdcall void Seek_PEOT() = 0 ;
       // Move to physical end of tape.

       virtual __stdcall bool Support_TM() = 0 ;
       // Returns true if tape marks supported on tape.

       virtual __stdcall void Add_TM() = 0 ;
       // Add a tape mark to the tape.

       virtual __stdcall int64 Read( char* Data, int64 Count ) = 0 ;
       /* Read Count bytes into Data.  Returns the count
         actually read.  Cannot read beyond current record. */

       virtual __stdcall char* Read_Record( int64& Count ) = 0 ;
       /* Returns current reoord.  Count is ignored
         on call.  On return, it is the size of the
         read record. */

       virtual __stdcall TUEC Write( char* Data, int64 Count ) = 0 ;
       /* Write Count bytes from Data to tape.  Count must be
         larger than 0.  This is a raw operation and does
         not prefix the data with a count or any other such
         processing. */

       virtual __stdcall TUEC Write_Record( char* Data, int64 Count ) = 0 ;
       /* Write a record of Count bytes from Data to tape.
         Count must be larger than 0. */
} // TTape_Media

     // Media file header structures (256 bytes)...
struct TCEF_Media_File_Header // Overlay with other media headers
{
    short int Prefix ; // -1
	unsigned char ID ; // 255 = App foundation
    int Facility ; // Type of file
    int Version ; // File format version
    unsigned char Reserved[ 256 - 11 ] ;
}

struct TCEF_Tape_Media_Header
{
    short int Prefix ; // -1
    unsigned char ID ; // 255 = App foundation
    int Facility ; // 123 = CEF Tape media file
    int Version ; // 10 = V1.0
    Format_Name : string[ 63 ] ; // Tape data format (eg "DOS-11 Tape")
    int BPI ; // Density of tape in linear bpi (0=not specified)
    int Length ; // Length of recordable tape, in feet (0=infinite)
    unsigned char Size_Length ; // Length of size records, in bytes (1-8)
    unsigned char After ; // True if size record follows AND preceeds data (otherwise only before)
    int IRG_Length ; // Length of IRGs, in 1/1000" (eg 750 = .75")
    int TM_Length ; // Length of tape marks, in 1/1000" (eg 10000 = 10")
    unsigned char Reserved[ 256 - 93 ] ;
}

struct TCEF_Disk_Media_Header
{
	 short int Prefix ; // -1
	 unsigned char ID ; // 255 = App foundation
	 int Facility ; // 124 = CEF Tape media file
	 int Version ; // 10 = V1.0
	 unsigned char Format_Name_Length ;
     char Format_Name[ 63 ] ; // Disk data format (eg "ODS1")
     int64 Max_Size ; // Maximum disk size, in cooked data bytes (this, divided by Sector_Size, provides the max number of sectors)
     int Sector_Lead_In_Size ; // Length of formatting data before actual sector data: 0 = none, -1 = variable
     int Sector_Lead_Out_Size ; // Length of formatting data after actual sector data: 0 = none, -1 = variable
     // Note that if both sector lead-in and loead-out are variable, the disk can only be read in raw mode
     int Track_Lead_In_Size ; // Length of formatting data before first sector on track: 0 = none, -1 = variable
     int Track_Lead_Out_Size ; // Length of formatting data after last sector on track: 0 = none, -1 = variable
     // Note that if both track lead-in and loead-out are variable, the disk can only be read in raw mode
     int Sector_Size ; // Length of actual (cooked) data for each sector, in bytes
     unsigned char Heads ; // Number of heads/surfaces (0 = unknown)
	 int Tracks ; // Number of tracks/cylinder per head (0 = unknown)
     int Sectors ; // Number of sectors per track (0 = unknown)
     int64 Errors ; // Pointer to on-disk error data (0 = none)
     unsigned char Default_Data ; // Data to return for phantom sectors
     int64 Default_Track_Lead_In ; // Pointer to default track lead-in data (must be 0 if variable or not used, or if Sectors [per track] is 0)
     int64 Default_Track_Lead_Out ; // Pointer to default track lead-out data (must be 0 if variable or not used or if Sectors [per track] is 0)
     int64 Default_Sector_Lead_In ; // Pointer to default sector lead-in data (must be 0 if variable or not used)
     int64 Default_Sector_Lead_Out ; // Pointer to default sector lead-out data (must be 0 if variable or not used)
     int64 Index ; // Pointer to data index (0 = pre-allocated, which isn't allowed for disks with variable lead-in/out)
     int Flags ; // and 1 = pre-allocated, Index points to actual data
                       // and 2 = Composite.  Data following header is a series of null-terminated filenames, indicating disk data files - in order of sector ranges
                       // and 4+ = reserved
     unsigned char Mask_Length ;
     char Mask[ 63 ] ; // Bitmask for addressing: [1] is bit 1, [2] is bit 2, etc.
                           // Each element is one of the following:
                           //   (space) = Unused
                           //   H = head/surface
                           //   C = cylinder/track
                           //   S = sector
                           // For instance, "HHCCCSSSSSSSS" indicates that the lowest 2 bits specify the head (0-3),
                           // bits 3-5 indicate cylinder (0-7), and bits 6-13 indicate sector (0-255).
                           // If all elements are spaces, the disk cannot be accessed via Head/Cylinder
     int64 Flaws ; // Pointer to flaw data (0 = none)
     int SN_Size ; // Size of serial number data (0 = none)
	 int64 Serial_Number ; // Pointer to pack serial-number data (must be 0 if SN_Size is 0)
	 unsigned char Reserved[ 256 - 253 ] ;
}

struct TCEF_Paper_Media_Header
{
	 short int Prefix ; // -1
	 unsigned char ID ; // 255 = App foundation
	 int Facility ; // 124 = CEF Tape media file
	 int Version ; // 10 = V1.0
	 int Typ ; // Paper type: 0 = plain, otherwise color
	 int Height ; // Paper height in 1/10 inches
	 int Width ; //  Paper width in 1/10 inches
	 int Max_Page ; // Maximum page (0=infinite)
	 int Bleed ; // Bleed factor
	 unsigned char Reserved[ 31 - 255 ] ;
}

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


class TPresenter
{
  public: // API...
      virtual __stdcall void Redraw() = 0 ;
      virtual __stdcall void Set_Bounds( int Width, int Height ) = 0 ;
      virtual __stdcall TUEC Terminate() = 0 ;
      virtual __stdcall void Get_Header( *TCEF_Media_File_Header& Header ) = 0 ;
}

class TPresentation_Manager
{
  public: // API...
     virtual __stdcall TPresenter* Get_Presenter( char* S,
         int Parent, TMedia* _Array,
         *TCEF_Media_File_Header Header ) = 0 ;
     /* Returns a presenter for the passed format
       name.  If format name is not supported,
       it returns nil. */

     virtual __stdcall char* Get_Name( int Index ) = 0 ;
     /* Returns a name of a supported media
       format.  Index is 0 for the first name.
       If Index is out of range, the function
       returns nil. */

    virtual __stdcall char* Get_Path( int Index ) = 0 ;
    /* Returns the path of a supported media
       format.  Index is 0 for the first format.
       If Index is out of range, the function
       returns nil.  This path indicates the
       type of media from least- to most-
       specific, separated with backslashes.
       For instance: "Disk\NTFS",
       "Disk\Hard\ODS-1" or "Tape\ANSI".  This
       is solely to provide the UI with a means
       of presenting all the possible options to
       the user in an organized way. */

     virtual __stdcall int Version() = 0 ;
     /* Returns the version of the specification
       that this object conforms to. */
}

