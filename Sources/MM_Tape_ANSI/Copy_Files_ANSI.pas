{
        Program Name : ANSIPres
        Package Name : CEF32
        Purpose      : ANSI Tape Copy
        Institution  : Conroy & Conroy Co.
        Date Written : 31-Oct-2009
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

          This form is used to copy files to/from ANSI tapes.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Copy_Files_ANSI ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Spin, Buttons, StdCtrls, ExtCtrls,

     // CEF
     CEFMedia ; // TTape_Media

type
  TCopy_Tape_ANSI_Form = class(TForm)
    Bottom_Panel: TPanel;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    File_Name: TEdit;
    SpeedButton1: TSpeedButton;
    Label4: TLabel;
    Label7: TLabel;
    Name: TEdit;
    Copy_Button: TSpeedButton;
    Cancel_Copy_Button: TSpeedButton;
    OpenDialog1: TOpenDialog;
    On_Tape: TListBox;
    Label10: TLabel;
    Blocking: TSpinEdit;
    Label11: TLabel;
    Copy_From_Tape_Button: TSpeedButton;
    Label12: TLabel;
    Output_File: TEdit;
    SpeedButton3: TSpeedButton;
    SaveDialog1: TSaveDialog;
    Output_Text: TCheckBox;
    Include_PPN: TCheckBox;
    Date_Value: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Block_Length: TSpinEdit;
    Record_Length: TSpinEdit;
    Label13: TLabel;
    Record_Format: TEdit;
    Expiration_Date: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    System_Code: TEdit;
    Label14: TLabel;
    Form_Control: TEdit;
    procedure SpeedButton1Click(Sender: TObject);
    procedure File_NameChange(Sender: TObject);
    procedure NameChange(Sender: TObject);
    procedure Copy_ButtonClick(Sender: TObject);
    procedure Cancel_Copy_ButtonClick(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure On_TapeClick(Sender: TObject);
    procedure Copy_From_Tape_ButtonClick(Sender: TObject);
    procedure NameKeyPress(Sender: TObject; var Key: Char);
    procedure Form_ControlKeyPress(Sender: TObject; var Key: Char);
    procedure Expiration_DateKeyPress(Sender: TObject; var Key: Char);
    procedure System_CodeKeyPress(Sender: TObject; var Key: Char);

  private // Instance data...
      Multi_Copy_Mode : boolean ; // True if copying multiple files
      Copy_Confirmed : boolean ; // True when user confirms a file during multiple file copy
      Cancel_Copy : boolean ; // True to cancel a multiple file copy
      _Media : TTape_Media ;
      VOL_Label : string ;
      
      procedure Set_Media( M : TTape_Media ) ;

  public // API...
      procedure Copy_File ;

      property Media : TTape_Media
          read _Media
          write Set_Media ;
  end ;

function Copy_Tape_ANSI_Form : TCopy_Tape_ANSI_Form ;

implementation

{$R *.dfm}

uses // C&C...
     CommonUt, // Edit
     _FiP, // Filespec_String_Scan
     _Filesys, // TFSSS
     Radix50s, // Rad
     Standard, // ERT
     UE, // DOS_ERT
     
     // CEF...
     Output_Error,
     Overwrite_Error ;

var _Copy_Tape_ANSI_Form : TCopy_Tape_ANSI_Form = nil ;

function Copy_Tape_ANSI_Form : TCopy_Tape_ANSI_Form ;

begin
    if( _Copy_Tape_ANSI_Form = nil ) then
    begin
        _Copy_Tape_ANSI_Form := TCopy_Tape_ANSI_Form.Create( Application ) ;
    end ;
    Result := _Copy_Tape_ANSI_Form ;
end ;


procedure TCopy_Tape_ANSI_Form.SpeedButton1Click(Sender: TObject) ;

var Loop : integer ;

begin
    if( OpenDialog1.Execute ) then
    begin
        if( OpenDialog1.Files.Count = 1 ) then
        begin
            File_Name.Text := OpenDialog1.Filename ;
            exit ;
        end ;
        Multi_Copy_Mode := True ;
        Cancel_Copy_Button.Enabled := True ;
        Cancel_Copy := False ;
        try
            for Loop := 0 to OpenDialog1.Files.Count - 1 do
            begin
                File_Name.Text := OpenDialog1.Files[ Loop ] ;
                while( True ) do
                begin
                    Application.ProcessMessages ;
                    if( Copy_Confirmed ) then
                    begin
                        Copy_Confirmed := False ;
                        Copy_File ;
                        break ;
                    end else
                    if( Cancel_Copy ) then
                    begin
                        Cancel_Copy := False ;
                        exit ;
                    end ;
                end ;
            end ;
            File_Name.Text := '' ;
            File_NameChange( nil ) ;
        finally
            Multi_Copy_Mode := False ;
            Cancel_Copy_Button.Enabled := False ;
        end ;
    end ;
end ; // TCopy_Tape_ANSI_Form.SpeedButton1Click


procedure TCopy_Tape_ANSI_Form.File_NameChange(Sender: TObject) ;

var FSSS : TFSSS ;
    S : string ;

begin
    Copy_Button.Enabled := ( File_Name.Text <> '' )
                           and
                           ( length( File_Name.Text ) < 13 ) ;
    Filespec_String_Scan( File_Name.Text, '\', FSSS ) ;
    S := Edit( FSSS.Filename, -1 ) ;
    if( S = '' ) then
    begin
        S := 'A' ;
    end ;
    Name.Text := copy( S, 1, 17 ) ;
    NameChange( nil ) ;
end ;


procedure TCopy_Tape_ANSI_Form.NameChange(Sender: TObject) ;

begin
    Copy_Button.Enabled := ( File_Name.Text <> '' )
                           and
                           ( length( Name.Text ) < 18 )
                           and
                           ( FileExists( File_Name.Text ) ) ;
end ;


procedure TCopy_Tape_ANSI_Form.Copy_ButtonClick(Sender: TObject) ;

begin
    if( Multi_Copy_Mode ) then
    begin
        Copy_Confirmed := True ;
        exit ;
    end ;
    Copy_File ;
end ;


type ANSI_Vol_Label = record
                          ID : array[ 1..3 ] of char ;
                          Num : char ;
                          Vol_ID : array[ 5..10 ] of char ;
                          Accessibility : char ;
                          Reserved0 : array[ 12..37 ] of char ;
                          Owner : array[ 38..51 ] of char ;
                          Reserved1 : array[ 52..79 ] of char ;
                          Version : char ;
                      end ;

type ANSI_HDR1_Label = record
                           ID : array[ 1..3 ] of char ;
                           Number : char ;
                           Filename : array[ 5..21 ] of char ;
                           File_Set : array[ 22..27 ] of char ;
                           Section : array[ 28..31 ] of char ;
                           Sequence : array[ 32..35 ] of char ;
                           Generation_Number : array[ 36..39 ] of char ;
                           Generation_Version : array[ 40..41 ] of char ;
                           Creation_Date : array[ 42..47 ] of char ;
                           Expiration_Date : array[ 48..53 ] of char ;
                           Accessibility : char ;
                           Block_Count : array[ 55..60 ] of char ;
                           System : array[ 61..73 ] of char ;
                           Reserved : array[ 74..80 ] of char ;
                       end ;

type ANSI_HDR2_Label = record
                           ID : array[ 1..3 ] of char ;
                           Number : char ;
                           Record_Format : char ;
                           Block_Length : array[ 6..10 ] of char ;
                           Record_Length : array[ 11..15 ] of char ;
                           Depends : array[ 16..50 ] of char ;
                           Offset : array[ 51..52 ] of char ;
                           Reserved : array[ 53..80 ] of char ;
                       end ;


procedure TCopy_Tape_ANSI_Form.Copy_File ;

var Block : array[ 0..32767 ] of byte ;
    Count, Dummy : integer ;
    F : file ;
    Header : ANSI_HDR1_Label ;
    Header2 : ANSI_HDR2_Label ;
    S : string ;

begin
    assignfile( F, File_Name.Text ) ;
    {$I-}
    reset( F, 1 ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( DOS_ERT( Dummy ) + ' while opening file ' + File_Name.Text ) ;
        exit ;
    end ;
    try
        Media.Seek_LEOT ;

        // Write header 1...
        fillchar( Header, sizeof( Header ), 32 ) ;
        Header.ID := 'HDR' ;
        Header.Number := '1' ;
        while( length( Name.Text ) < 12 ) do
        begin
            Name.Text := Name.Text + ' ' ;
        end ;
        move( PChar( Name.Text )[ 0 ], Header.Filename, 12 ) ;
        S := Date_Value.Text ;
        if( ( length( S ) < 5 ) and ( copy( S, 1, 1 ) <> ' ' ) ) then
        begin
            S := ' ' + S ;
        end ;
        while( length( S ) < 5 ) do
        begin
            S := S + '0' ;
        end ;
        move( Pchar( S )[ 0 ], Header.Creation_Date, 5 ) ;
        S := Expiration_Date.Text ;
        if( ( length( S ) < 5 ) and ( copy( S, 1, 1 ) <> ' ' ) ) then
        begin
            S := ' ' + S ;
        end ;
        while( length( S ) < 5 ) do
        begin
            S := S + '0' ;
        end ;
        move( Pchar( S )[ 0 ], Header.Expiration_Date, 5 ) ;
        while( length( VOL_Label ) < 6 ) do
        begin
            VOL_Label := VOL_Label + ' ' ;
        end ;
        move( PChar( Vol_Label )[ 0 ], Header.File_Set, 6 ) ;
        Header.Section := '0001' ;
        Header.Sequence := '0001' ;
        Header.Generation_Number := '0001' ;
        Header.Generation_Version := '00' ;
        Header.Accessibility := ' ' ;
        Header.Block_Count := '000000' ;
        S := System_Code.Text ;
        while( length( S ) < 13 ) do
        begin
            S := S + ' ' ;
        end ;
        move( PChar( S )[ 0 ], Header.System, 13 ) ;
        Media.Write_Record( @Header, 80 ) ;

        // Write header 2...
        fillchar( Header2, sizeof( Header2 ), 32 ) ;
        Header2.ID := 'HDR' ;
        Header2.Number := '2' ;
        S := Record_Format.Text ;
        if( S = '' ) then
        begin
            S := 'U' ;
        end ;
        Header2.Record_Format := S[ 1 ] ;
        S := inttostr( Block_Length.Value ) ;
        while( length( S ) < 5 ) do
        begin
            S := '0' + S ;
        end ;
        move( PChar( S )[ 0 ], Header2.Block_Length, 5 ) ;
        Header2.Record_Length := '00000' ;
        S := System_Code.Text ;
        while( length( S ) < 35 ) do
        begin
            S := ' ' + S ;
        end ;
        move( PChar( S )[ 0 ], Header2.Depends, 35 ) ;
        Header2.Offset := '00' ;
        Media.Write_Record( @Header2, 80 ) ;

        Media.Add_TM ;

        // Write file...
        while( not eof( F ) ) do
        begin
            fillchar( Block, Blocking.Value, 0 ) ;
            blockread( F, Block, Blocking.Value, Count ) ;
            Media.Write_Record( @Block, Blocking.Value ) ;
        end ;

        Media.Add_TM ;

        // Write EOF 1...
        Header.ID := 'EOF' ;
        Media.Write_Record( @Header, 80 ) ;

        // Write EOF 2...
        Header2.ID := 'EOF' ;
        Media.Write_Record( @Header2, 80 ) ;

        Media.Add_TM ;

        On_Tape.Items.Add( Name.Text + inttostr( filesize( F ) ) + ' bytes' ) ;

        // Write LEOT...
        Media.Add_TM ;
        Media.Add_TM ;
    finally
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
    end ;
end ; // TCopy_Tape_DOS11_Form.Copy_File


procedure TCopy_Tape_ANSI_Form.Cancel_Copy_ButtonClick(Sender: TObject);

begin
    Cancel_Copy := True ;
end ;


procedure TCopy_Tape_ANSI_Form.Set_Media( M : TTape_Media ) ;

var Count : integer ;
    HDR1 : ANSI_HDR1_Label ;
    Lab : ANSI_VOL_Label ;
    Loop : integer ;
    Rec : integer ;
    S : string ;
    Temp : array[ 0..13 ] of byte ;
    Size : integer ;

begin
    On_Tape.Items.Clear ;
    _Media := M ;
    if( M = nil ) then
    begin
        exit ;
    end ;

    S := '' ;
    Rec := 0 ;
    Size := 0 ;
    M.Seek_BOT ;
    while( not ( M.At_LEOT or M.At_PEOT ) ) do
    begin
        M.Seek( Rec, True, False ) ;
        Count := M.Record_Length( Rec ) ;
        if( Count = 80 ) then // Label
        begin
            for Loop := 0 to 79 do
            begin
                Temp[ Loop ] := M.Get_Byte( M.Position( False ) + Loop ) ;
            end ;
            move( Temp, Lab, 80 ) ;
            if( Rec = 0 ) then // Volume label
            begin
                if( ( Lab.ID = 'VOL' ) and ( Lab.Num = '1' ) ) then
                begin
                    VOL_Label := Lab.ID ;
                    inc( Rec ) ;
                    continue ;
                end ;
            end ;
            if( ( Lab.ID = 'EOF' ) or ( Lab.ID = 'EOV' ) ) then
            begin
                if( S <> '' ) then
                begin
                    On_Tape.Items.Add( S + ' ' + inttostr( Size ) + ' bytes' ) ;
                    Size := 0 ;
                    S := '' ;
                end ;
            end else
            if( ( Lab.ID = 'HDR' ) and ( Lab.Num = '1' ) ) then
            begin
                if( S <> '' ) then
                begin
                    On_Tape.Items.Add( S + ' ' + inttostr( Size ) + ' bytes' ) ;
                    Size := 0 ;
                end ;
                move( Lab, HDR1, 80 ) ;
                S := Edit( HDR1.Filename, 128 ) ; // Trim trailing spaces
                inc( Rec ) ; // Skip HDR2
                Count := M.Record_Length( Rec ) ;
                if( Count = 80 ) then
                begin
                    inc( Rec ) ; // Skip TM
                    Count := M.Record_Length( Rec ) ;
                end ;
            end ;
        end else
        begin
            Size := Size + Count ;
        end ;
        inc( Rec ) ;
    end ; // while( not M.At_LEOT )
    if( S <> '' ) then
    begin
        On_Tape.Items.Add( S + ' ' + inttostr( Size ) + ' bytes' ) ;
    end ;
end ; // TCopy_Tape_DOS11_Form.Set_Media


procedure TCopy_Tape_ANSI_Form.SpeedButton3Click(Sender: TObject);

begin
    if( SaveDialog1.Execute ) then
    begin
        Output_File.Text := SaveDialog1.FileName ;
    end ;
end ;


procedure TCopy_Tape_ANSI_Form.On_TapeClick( Sender : TObject ) ;

begin
    Copy_From_Tape_Button.Enabled := On_Tape.SelCount > 0 ;
end ;


procedure TCopy_Tape_ANSI_Form.Copy_From_Tape_ButtonClick( Sender: TObject) ;

label Try_Again ;

var Dummy, Loop : integer ;
    Ignore_All : boolean ;
    F : file ;
    FS, FSI, FSO : TFSSS ;
    No_Overwrite_All, Overwrite_All : boolean ;
    P : PChar ;
    S, Work : string ;
    Temp : int64 ;

begin
    Ignore_All := False ;
    Overwrite_All := False ;
    No_Overwrite_All := False ;
    Filespec_String_Scan( Output_File.Text, '\', FS ) ;
    Media.Seek_BOT ;
    for Loop := 0 to On_Tape.Count - 1 do
    begin
        while( ( Media.Record_Length( -1 ) <> 12 ) and ( Media.Record_Length( -1 ) <> 14 ) ) do
        begin
            if( Media.At_PEOT ) then // Shouldn't happen
            begin
                exit ; // Just in case, so we don't go into an infinite loop
            end ;
            Media.Seek( 1, True, True ) ; // Read until we find a file header
        end ;
        Media.Seek( 1, True, True ) ; // Move past file header
        if( On_Tape.Selected[ Loop ] ) then
        begin
            FSO := FS ;
            S := On_Tape.Items[ Loop ] ;

            if( not Include_PPN.Checked ) then
            begin
                // Remove PPN
                Dummy := pos( ']', S ) ;
                S := copy( S, Dummy + 1, length( S ) ) ;
            end ;

            S := Edit( S, 2 ) ; // Remove all spaces
            Filespec_String_Scan( S, '\', FSI ) ;
            if( FSO.Name = '' ) then
            begin
                FSO.Name := FSI.Name ;
            end ;
            if( ( FSO.Flags and FSSS_Null_Extension ) <> 0 ) then
            begin
                FSO.Extension := FSI.Extension ;
            end ;
            S := FSO.Device + FSO.Path + FSO.Name + '.' + FSO.Extension ;
            if( FileExists( S ) ) then
            begin
                if( No_Overwrite_All ) then // User requested Skip All
                begin
                    continue ;
                end ;
                if( not Overwrite_All ) then // User didn't request Overwrite All
                begin
                    Overwrite_Form.Caption := S ;
                    case Overwrite_Form.ShowModal of
                        mrYes : ;
                        mrYesToAll : Overwrite_All := True ;
                        mrNo : continue ;
                        mrNoToAll : begin
                                        No_Overwrite_All := True ;
                                        continue ;
                                    end ;
                        mrCancel : exit ;
                    end ;
                end ;
            end ;
            assignfile( F, S ) ;
Try_Again:
            {$I-}
            rewrite( F, 1 ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                if( Ignore_All ) then
                begin
                    continue ;
                end ;
                Output_Error_Form.Caption := DOS_ERT( Dummy ) ;
                Output_Error_Form.Message_Text.Caption := 'Writing ' + S ;
                case Output_Error_Form.ShowModal of
                    mrIgnore : continue ;
                    mrYesToAll : begin
                                     Ignore_All := True ;
                                     continue ;
                                 end ;
                    mrRetry : goto Try_Again ;
                    mrCancel : exit ;
                end ;
            end ;

            S := '' ;
            while(
                   ( Media.Record_Length( -1 ) <> 12 )
                   and
                   ( Media.Record_Length( -1 ) <> 14 )
                   and
                   ( not ( Media.At_LEOT or Media.At_PEOT ) )
                 ) do
            begin
                setlength( Work, Media.Record_Length( -1 ) ) ;
                P := Media.Read_Record( Temp ) ; // This reads and advances the record
                setlength( Work, Temp ) ;
                move( P[ 0 ], Pchar( Work )[ 0 ], Temp ) ;
                S := S + Work ;
            end ;
            if( Output_Text.Checked ) then
            begin
                Dummy := length( S ) ;
                while( ( Dummy > 0 ) and ( S[ Dummy ] = #0 ) ) do
                begin
                    dec( Dummy ) ;
                end ;
                setlength( S, Dummy ) ;
            end ;
            blockwrite( F, PChar( S )[ 0 ], length( S ) ) ;
            {$I-}
            closefile( F ) ;
            {$I+}
            IOResult ;
        end ; // if( On_Tape.Selected[ Loop ] )
    end ; // for Loop := 0 to On_Tape.Count - 1
end ; // TCopy_Tape_DOS11_Form.Copy_From_Tape_ButtonClick


procedure TCopy_Tape_ANSI_Form.NameKeyPress(Sender: TObject; var Key: Char);

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    if( length( Name.Text ) > 16 ) then
    begin
        Key := #0 ;
    end ;
end ;


procedure TCopy_Tape_ANSI_Form.Form_ControlKeyPress(Sender: TObject;
    var Key: Char) ;

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    if( length( Name.Text ) > 0 ) then
    begin
        Key := #0 ;
    end ;
end ;


procedure TCopy_Tape_ANSI_Form.Expiration_DateKeyPress(Sender: TObject;
    var Key: Char) ;

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    if( length( Name.Text ) > 4 ) then
    begin
        Key := #0 ;
    end ;
end ;


procedure TCopy_Tape_ANSI_Form.System_CodeKeyPress(Sender: TObject;
    var Key: Char) ;

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    if( length( Name.Text ) > 12 ) then
    begin
        Key := #0 ;
    end ;
end ;


end.
