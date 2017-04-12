{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 Media Manager
        Institution  :
        Date Written : 27-Apr-2000
        Written By   : Alan Conroy
        Version      : 1.3

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

          This form is the media manager.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Media_Manager_Dialog ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Buttons, StdCtrls, ExtCtrls, ComCtrls,

     // C&C...
     ArrayInt, // TCOM_Array_Interface
     COMInter, // TCommon_COM_Interface
     DataDis, // TData_Display
     DataPres,
     _UE, // TUnified_Exception

     // CEF...
     _CEF, // TCEF_Tape_Media_Header
     CEFMedia ; // TPresenter


type TMedia_Manager = class ;

     TMedia_Interface = class( TMedia )
                                  private // Instance data...
                                      MM : TMedia_Manager ;

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
                              end ;


     TPaint_Panel = class( TPanel )
                        public // API...
                            MM : TMedia_Manager ;

                            procedure wmPaint( var Message ) ;
                                message WM_Paint ;
                    end ;

  TMedia_Manager = class(TForm)
    OpenDialog1: TOpenDialog;
    GroupBox1: TGroupBox;
    StatusBar1: TStatusBar;
    Button_Panel: TPanel;
    BitBtn1: TBitBtn;
    Header_Panel: TPanel;
    Label1: TLabel;
    File_Name: TEdit;
    SpeedButton1: TSpeedButton;
    Create_Button: TSpeedButton;
    Open_Button: TSpeedButton;
    Close_Button: TSpeedButton;
    Delete_Button: TSpeedButton;
    Convert_Button: TSpeedButton;
    Warning_Label: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Size_Label: TLabel;
    File_Format: TComboBox;
    SaveDialog1: TSaveDialog;
    Edit_Header: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure File_NameChange(Sender: TObject);
    procedure Create_ButtonClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Open_ButtonClick(Sender: TObject);
    procedure Delete_ButtonClick(Sender: TObject);
    procedure Close_ButtonClick(Sender: TObject);
    procedure File_FormatChange(Sender: TObject);
    procedure File_NameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Group_PanelResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Convert_ButtonClick(Sender: TObject);
    procedure Edit_HeaderClick(Sender: TObject);

  private // Instance data...
      Open_File : file ;
      Open_File_Name : string ;
      Modified : boolean ; // True if file modified
      _Array : TMedia_Interface ;
      Editor_Panel : TPresenter ;
      Group_Panel : TPaint_Panel ;
      CEF_File : boolean ;
      Header : TCEF_Tape_Media_Header ;

  public // API...
      procedure Cancel_Changes ;
      procedure Modify ;
      procedure Present_File ;
      procedure Unpresent_File ;
  end ;

var Media_Manager : TMedia_Manager ;

implementation

{$R *.dfm}

uses // C&C
     CommonUt, // Edit
     _FileSys, // TFSSS
     _FiP, // Filespec_String_Scan
     Standard, // ERT
     UE, // DOS_ERT

     // CEF32...
     Tape_Header_Dialog;

// TPaint_Panel methods...

// API...

procedure TPaint_Panel.wmPaint( var Message ) ;

begin
    if( ( MM <> nil ) and ( MM.Editor_Panel <> nil ) ) then
    begin
        MM.Editor_Panel.Redraw ;
        ValidateRect( Handle, nil ) ;
    end ;
end ;



// TMedia_Interface methods...

// API...

function TMedia_Interface.Is_Class( _N : PChar ) : boolean ;

var N : string ;

begin
    N := lowercase( string( _N ) ) ;
    Result := ( N = 'tmedia_interface' ) ;
end ;


function TMedia_Interface.Get_Byte( Index : int64 ) : byte ;

begin
    if( MM.CEF_File ) then
    begin
        Index := Index + 256 ;
    end ;
    seek( MM.Open_File, Index ) ;
    {$I-}
    blockread( MM.Open_File, Result, 1 ) ;
    {$I+}
    if( IOResult <> 0 ) then
    begin
        Result := 0 ;
    end ;
end ;


procedure TMedia_Interface.Set_Byte( Index : int64 ; Value : byte ) ;

begin
    if( MM.CEF_File ) then
    begin
        Index := Index + 256 ;
    end ;
    MM.Modify ;
    seek( MM.Open_File, Index ) ;
    blockwrite( MM.Open_File, Value, 1 ) ;
    MM.Editor_Panel.Redraw ;
    MM.Size_Label.Caption := inttostr( High_Bound( 0 ) ) + ' bytes' ;
end ;


function TMedia_Interface.Read_Only : boolean ;

begin
    Result := False ;
end ;


function TMedia_Interface.Low_Bound( Subscript : integer ) : int64 ;

begin
    Result := 0 ;
end ;


function TMedia_Interface.High_Bound( Subscript : integer ) : int64 ;

begin
    Result := filesize( MM.Open_File ) ;
    if( MM.CEF_File ) then
    begin
        Result := Result - 256 ;
    end ;
end ;


function TMedia_Interface.Subscripts : integer ;

begin
    Result := 1 ;
end ;


function TMedia_Interface.Insert( Index, Count : int64 ; Fill : byte ) : TUnified_Exception ;

var Buffer : array[ 0..65535 ] of byte ;
    This_Count, Original, Position : int64 ;

begin
    // Setup...
    Result := nil ;
    Position := filesize( MM.Open_File ) ;
    Original := Count ;
    if( Index < 0 ) then
    begin
        Index := 0 ;
    end ;
    if( Index > Position ) then
    begin
        Index := Position ;
    end ;
    if( MM.CEF_File ) then
    begin
        Index := Index + 256 ;
    end ;

    // Extend file by requested amount...
    seek( MM.Open_File, filesize( MM.Open_File ) ) ;
    This_Count := Count ;
    while( This_Count > 0 ) do
    begin
        if( This_Count > sizeof( Buffer ) ) then
        begin
            blockwrite( MM.Open_File, Buffer, sizeof( Buffer ) ) ;
        end else
        begin
            blockwrite( MM.Open_File, Buffer, This_Count ) ;
        end ;
        This_Count := This_Count - sizeof( Buffer ) ;
    end ;

    // Shift contents up
    while( Count > 0 ) do
    begin
        This_Count := Count ;
        if( This_Count > sizeof( Buffer ) ) then
        begin
            This_Count := sizeof( Buffer ) ;
        end ;
        if( This_Count > Position - Index ) then
        begin
            This_Count := Position - Index ;
        end ;
        Position := Position - This_Count ;
        seek( MM.Open_File, Position ) ;
        blockread( MM.Open_File, Buffer, This_Count ) ;
        seek( MM.Open_File, Position + This_Count ) ;
        blockwrite( MM.Open_File, Buffer, This_Count ) ;
        Count := Count - This_Count ;
    end ;

    // Fill empty space...
    if( Original > sizeof( Buffer ) ) then
    begin
        fillchar( Buffer, sizeof( Buffer ), Fill ) ;
    end else
    begin
        fillchar( Buffer, Original, Fill ) ;
    end ;
    seek( MM.Open_File, Index ) ;
    Position := Index ;
    while( Original > 0 ) do
    begin
        if( Original > sizeof( Buffer ) ) then
        begin
            blockwrite( MM.Open_File, Buffer, sizeof( Buffer ) ) ;
        end else
        begin
            blockwrite( MM.Open_File, Buffer, Original ) ;
        end ;
        Original := Original - sizeof( Buffer ) ;
        Position := Position + sizeof( Buffer ) ;
    end ;
    MM.Size_Label.Caption := inttostr( High_Bound( 0 ) ) + ' bytes' ;
end ; // TMedia_Interface.Insert


function TMedia_Interface.Delete( Index, Count : int64 ) : TUnified_Exception ;

var Buffer : array[ 0..65535 ] of byte ;
    This_Count, Position : int64 ;

begin
    // Sanity checks...
    Result := nil ;
    if( Index < 0 ) then
    begin
        Count := Count + Index ; // Ignore portion before start of data
        Index := 0 ;
    end ;
    if( Count < 1 ) then
    begin
        exit ; // Nothing to do
    end ;
    if( MM.CEF_File ) then
    begin
        Index := Index + 256 ;
    end ;
    if( Index >= Filesize( MM.Open_File ) ) then
    begin
        exit ; // Nothing to do
    end ;
    if( Count > Filesize( MM.Open_File ) ) then
    begin
        Count := Filesize( MM.Open_File ) ;
    end ;

    // Shift contents down...
    Position := Index + Count ;
    seek( MM.Open_File, Position ) ;
    while( Position < High_Bound( 0 ) ) do
    begin
        This_Count := sizeof( Buffer ) ;
        if( Position + This_Count >= High_Bound( 0 ) ) then
        begin
            This_Count := High_Bound( 0 ) - Position - 1 ;
        end ;
        seek( MM.Open_File, Position ) ;
        blockread( MM.Open_File, Buffer, This_Count ) ;
        seek( MM.Open_File, Position - This_Count ) ;
        blockwrite( MM.Open_File, Buffer, This_Count ) ;
        Position := Position + This_Count ;
    end ;

    // Truncate file...
    seek( MM.Open_File, Filesize( MM.Open_File ) - Count ) ;
    Truncate( MM.Open_File ) ;
    MM.Size_Label.Caption := inttostr( High_Bound( 0 ) ) + ' bytes' ;
end ;


function TMedia_Interface.Get( Index, Count : int64 ; Data : PChar ) : TUnified_Exception ;

begin
    if( MM.CEF_File ) then
    begin
        Index := Index + 256 ;
    end ;
    seek( MM.Open_File, Index ) ;
    {$I-}
    blockread( MM.Open_File, Data[ 0 ], Count ) ;
    {$I+}
    Result := Create_MSDOS_UE( IOResult, '' ) ;
end ;


function TMedia_Interface.Put( Index, Count : int64 ; Data : PChar ) : TUnified_Exception ;

begin
    if( MM.CEF_File ) then
    begin
        Index := Index + 256 ;
    end ;
    seek( MM.Open_File, Index ) ;
    {$I-}
    blockwrite( MM.Open_File, Data^, Count ) ;
    {$I+}
    Result := Create_MSDOS_UE( IOResult, '' ) ;
    MM.Size_Label.Caption := inttostr( High_Bound( 0 ) ) + ' bytes' ;
end ;



// TMedia_Manager methods...

procedure TMedia_Manager.SpeedButton1Click(Sender: TObject) ;

begin
    if( OpenDialog1.Execute ) then
    begin
        File_Name.Text := OpenDialog1.FileName ;
    end ;
end ;


procedure TMedia_Manager.File_NameChange(Sender: TObject);

begin
    if( Edit( File_Name.Text, -1 ) = '' ) then
    begin
        Open_Button.Enabled := False ;
        Create_Button.Enabled := False ;
        exit ;
    end ;
    if( FileExists( File_Name.Text ) ) then
    begin
        Open_Button.Enabled := True ;
        Create_Button.Enabled := False ;
    end else
    begin
        Open_Button.Enabled := False ;
        Create_Button.Enabled := True ;
    end ;
end ;


procedure TMedia_Manager.Create_ButtonClick(Sender: TObject);

var Dummy : integer ;
    F : file ;

begin
    assignfile( F, File_Name.Text ) ;
    {$I-}
    rewrite( F ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        StatusBar1.SimpleText := ERT( Dummy ) + ' while creating ' + File_Name.Text ;
        ShowMessage( StatusBar1.SimpleText ) ;
        exit ;
    end ;
    {$I-}
    closefile( F ) ;
    {$I+}
    IOResult ;
    Open_ButtonClick( Sender ) ;
end ;


procedure TMedia_Manager.BitBtn1Click( Sender : TObject ) ;

begin
    if( Close_Button.Enabled ) then
    begin
        Close_Button.Click ;
    end ;
end ;


const CEF_Tape_Facility_ID = 123 ;

procedure TMedia_Manager.Open_ButtonClick(Sender: TObject);

var Dummy : integer ;
    S : string ;

begin
    if( Close_Button.Enabled ) then
    begin
        Close_Button.Click ;
    end ;
    assignfile( Open_File, File_Name.Text ) ;
    {$I-}
    reset( Open_File, 1 ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        StatusBar1.SimpleText :=
            ERT( Dummy ) + ' while opening ' + File_Name.Text ;
        ShowMessage( StatusBar1.SimpleText ) ;
        exit ;
    end ;
    CEF_File := False ;
    if( filesize( Open_File ) >= 256 ) then
    begin
        blockread( Open_File, Header, sizeof( Header ) ) ;
        if(
            ( Header.Prefix = 65535 )
            and
            ( Header.ID = 255 )
            and
            ( Header.Format_Name[ 0 ] < #64 )
            and
            ( Header.Length >= 0 )
            and
            ( Header.Size_Length > 0 )
            and
            ( Header.Size_Length < 9 )
            and
            ( Header.Version >= 10 )
          ) then // A valid CEF media file header
        begin
            if( Header.Facility <> CEF_Tape_Facility_ID ) then
            begin
                S := 'This file appears to be a non-tape media with a format name of "' +
                    Header.Format_Name +
                    '".  Accessing this file as a tape may cause a loss of data.  Do you wish to open it anyway?' ;
                case MessageBox( 0, PChar( S ), 'Media file', MB_YESNOCANCEL ) of
                    IDNO, IDCANCEL :
                        begin
                            {$I-}
                            closefile( Open_File ) ;
                            {$I+}
                            IOResult ;
                            exit ;
                        end ;
                end ; // case
            end else
            begin
                CEF_File := True ;
            end ; // if( Header.Facility <> CEF_Tape_Facility_ID )
        end ; // if
    end ;
    Edit_Header.Enabled := CEF_File ;
    GroupBox1.Caption := File_Name.Text ;
    Open_File_Name := File_Name.Text ;
    Close_Button.Enabled := True ;
    Delete_Button.Enabled := True ;
    Present_File ;
end ; // TMedia_Manager.Open_ButtonClick


procedure TMedia_Manager.Cancel_Changes ;

var Dummy : integer ;
    F : file ;
    S : string ;

begin
    Modified := False ;
    if( Warning_Label.Caption <> '' ) then // A backup was created
    begin
        // Setup...
        S := uppercase( Open_File_Name ) ;
        Dummy := Extension_Pos( S ) ;
        assignfile( F, copy( S, 1, Dummy - 1 ) + '.BAK' ) ;

        // Delete current file...
        {$I-}
        closefile( Open_File ) ;
        {$I+}
        IOResult ;
        {$I-}
        erase( Open_File ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) + ' deleting modified file - original file is ' + copy( Open_File_Name, 1, Dummy - 1 ) + '.bak' ) ;
            exit ;
        end ;
        {$I-}
        rename( F, Open_File_Name ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) + ' renaming file - original file is ' + copy( Open_File_Name, 1, Dummy - 1 ) + '.bak' ) ;
            exit ;
        end ;
        {$I-}
        reset( Open_File, 1 ) ; // Reopen file
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) + ' opening original file' ) ;
        end ;
        Present_File ;
    end ; // if( Warning_Label.Caption <> '' )
end ; // TMedia_Manager.Cancel_Changes


procedure TMedia_Manager.Modify ;

var Buffer : array[ 0..32767 ] of byte ;
    Count, Dummy : integer ;
    F : file ;
    S : string ;

begin
    if( not Modified ) then // First change
    begin
        S := uppercase( Open_File_Name ) ;
        Dummy := Extension_Pos( S ) ;
        if( copy( S, Dummy, Dummy + 5 ) = '.BAK' ) then
        begin
            StatusBar1.SimpleText := 'File extension precludes creation of backup file' ;
            MessageBeep( 0 ) ;
            Warning_Label.Caption := 'WARNING: No backup file created' ;
        end else
        begin
            assignfile( F, copy( S, 1, Dummy ) + 'bak' ) ;
            {$I-}
            rewrite( F, 1 ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                StatusBar1.SimpleText := ERT( Dummy ) + ' creating backup file - no backup will be made' ;
                MessageBeep( 0 ) ;
                Warning_Label.Caption := 'WARNING: No backup file created' ;
            end else
            begin
                try
                    seek( Open_File, 0 ) ;
                    Count := 1 ;
                    while( Count > 0 ) do
                    begin
                        blockread( Open_File, Buffer, sizeof( Buffer ), Count ) ;
                        if( Count > 0 ) then
                        begin
                            blockwrite( F, Buffer, Count ) ;
                        end ;
                    end ;
                    StatusBar1.SimpleText := 'Backup file created' ;
                    Warning_Label.Caption := '' ;
                finally
                    {$I-}
                    closefile( F ) ;
                    {$I+}
                    IOResult ;
                end ;
            end ; // if( Dummy <> 0 )
        end ; // if( copy( S, Dummy, Dummy + 5 ) = '.BAK' )
    end ; // if( not Modified )
    Modified := True ;
end ; // TMedia_Manager.Modify


procedure TMedia_Manager.Present_File ;

var Dummy : integer ;

begin
    // Present the file contents, interpreted via file format selected

    // Setup...
    if( _Array = nil ) then
    begin
        _Array := TMedia_Interface.Create ;
        _Array.MM := self ;
    end ;
    Unpresent_File ;
    if( Open_File_Name <> '' ) then
    begin
        Size_Label.Caption := inttostr( filesize( Open_File ) ) + ' bytes' ;

        if( CEF_File ) then
        begin
            Dummy := File_Format.Items.IndexOf( Header.Format_Name ) ;
            if( Dummy <> -1 ) then
            begin
                File_Format.ItemIndex := Dummy ;
            end ;
            Editor_Panel := Get_Presentation( File_Format.Text, '', Group_Panel.Handle, _Array, @Header ) ;
        end else
        begin
            Editor_Panel := Get_Presentation( File_Format.Text, '', Group_Panel.Handle, _Array, nil ) ;
        end ;
    end ;
    Group_PanelResize( nil ) ;
    Convert_Button.Enabled := True ;
end ;


procedure TMedia_Manager.Unpresent_File ;

begin
    if( Editor_Panel <> nil ) then
    begin
        Editor_Panel.Terminate ;
        Editor_Panel := nil ;
    end ;
    Convert_Button.Enabled := False ;
end ;


procedure TMedia_Manager.Delete_ButtonClick(Sender: TObject);

var Dummy : integer ;

begin
    case MessageBox( 0, 'Are you sure you want to delete the file?', 'Delete file', MB_YESNOCANCEL ) of
        IDNO, IDCANCEL : exit ;
    end ;
    Cancel_Changes ;
    if( Close_Button.Enabled ) then
    begin
        Close_Button.Click ;
    end ;
    {$I-}
    erase( Open_File ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        StatusBar1.SimpleText := ERT( Dummy ) + ' while opening ' + File_Name.Text ;
        ShowMessage( StatusBar1.SimpleText ) ;
        exit ;
    end ;
    File_NameChange( File_Name ) ;
end ;


procedure TMedia_Manager.Close_ButtonClick(Sender: TObject);

begin
    if( Modified ) then
    begin
        case MessageBox( 0, 'Save modifications to the file?', 'File has been changed', MB_YESNOCANCEL ) of
            IDNO : begin
                       Cancel_Changes ;
                   end ;
            IDCANCEL : exit ;
        end ;
    end ;

    {$I-}
    closefile( Open_File ) ;
    {$I+}
    IOResult ;
    GroupBox1.Caption := '' ;
    Close_Button.Enabled := False ;
    Delete_Button.Enabled := False ;
    Convert_Button.Enabled := False ;
    StatusBar1.SimpleText := '' ;
    Warning_Label.Caption := '' ;
    Open_File_Name := '' ;
    Unpresent_File ;
end ;


procedure TMedia_Manager.File_FormatChange(Sender: TObject);

begin
    Present_File ;
end ;


procedure TMedia_Manager.File_NameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState) ;

begin
    if( Key = VK_RETURN ) then
    begin
        Key := 0 ;
        if( Open_Button.Enabled ) then
        begin
            Open_Button.Click ;
        end else
        if( Create_Button.Enabled ) then
        begin
            Create_Button.Click ;
        end ;
    end ;
end ;


procedure TMedia_Manager.Group_PanelResize(Sender: TObject);

begin
    if( Editor_Panel <> nil ) then
    begin
        Editor_Panel.Set_Bounds( Group_Panel.Width, Group_Panel.Height ) ;
    end ;
end ;


procedure TMedia_Manager.FormCreate( Sender : TObject ) ;

var L : TStringList ;

begin
    Group_Panel := TPaint_Panel.Create( Owner ) ;
    Group_Panel.Parent := GroupBox1 ;
    Group_Panel.Align := alClient ;
    Group_Panel.BevelOuter := bvNone ;
    Group_Panel.Visible := True ;
    Group_Panel.MM := self ;
    Group_Panel.OnResize := Group_PanelResize ;

    L := Get_Formats ;
    File_Format.Items.Assign( L ) ;
    L.Free ;
    File_Format.ItemIndex := 0 ;
end ;


procedure TMedia_Manager.Convert_ButtonClick( Sender : TObject ) ;

var Buffer : array[ 0..32767 ] of byte ;
    Dummy, Loop : integer ;
    Header : TCEF_Tape_Media_Header ;
    FSSS : TFSSS ;
    S : string ;
    FO : file ;

begin
    // Setup
    Filespec_String_Scan( Open_File_Name, '\', FSSS ) ;

    if( CEF_File ) then
    begin
        // Construct a default output filename...
        S := FSSS.Device + FSSS.Path + FSSS.Name + '.tap' ;
        SaveDialog1.FileName := FSSS.Name + '.tap' ;
        if( uppercase( S ) = uppercase( Open_File_Name ) ) then
        begin
            SaveDialog1.FileName := FSSS.Name + '.img' ;
            S := FSSS.Device + FSSS.Path + FSSS.Name + '.img' ;
        end ;

        // Ask user for output filename...
        if( not SaveDialog1.Execute ) then
        begin
            exit ;
        end ;
        S := SaveDialog1.FileName ;
        if( pos( '.', S ) = 0 ) then
        begin
            S := S + '.tap' ;
        end ;

        // Sanity/safety checks...
        if( uppercase( S ) = uppercase( Open_File_Name ) ) then
        begin
            ShowMessage( 'Cannot overwrite file with itself.' ) ;
            exit ;
        end ;
        if( FileExists( S ) ) then
        begin
            if( MessageBox( 0, 'Overwrite existing file?', PChar( S ), MB_YESNO ) = IDNO ) then
            begin
                exit ;
            end ;
        end ;

        // Copy existing file to new file...
        assignfile( FO, S ) ;
        {$I-}
        rewrite( FO, 1 ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) ) ;
            exit ;
        end ;
        try
            seek( Open_File, 256 ) ; // Start just after the header
            Dummy := sizeof( Buffer ) ;
            while( Dummy = sizeof( Buffer ) ) do
            begin
                {$I-}
                blockread( Open_File, Buffer, sizeof( Buffer ), Dummy ) ;
                {$I+}
                Loop := IOResult ;
                if( Loop <> 0 ) then
                begin
                    ShowMessage( ERT( Loop ) ) ;
                    exit ;
                end ;
                {$I-}
                blockwrite( FO, Buffer, Dummy ) ;
                {$I+}
                Loop := IOResult ;
                if( Loop <> 0 ) then
                begin
                    ShowMessage( ERT( Loop ) ) ;
                    exit ;
                end ;
            end ;
        finally
            {$I-}
            closefile( FO ) ;
            {$I+}
            IOResult ;
        end ;

        // Open the new file...
        File_Name.Text := S ;
        Open_Button.Click ;
    end else
    begin
        // Query user for header information...
        Tape_Header.Internal_Format.Enabled := True ;
        if( Tape_Header.ShowModal = mrCancel ) then
        begin
            exit ;
        end ;

        // Construct a default output filename...
        S := FSSS.Device + FSSS.Path + FSSS.Name + '.cef_m' ;
        SaveDialog1.FileName := FSSS.Name + '.cef_m' ;

        // Ask user for output filename...
        if( not SaveDialog1.Execute ) then
        begin
            exit ;
        end ;
        S := SaveDialog1.FileName ;
        if( pos( '.', S ) = 0 ) then
        begin
            S := S + '.tap' ;
        end ;

        // Sanity/safety checks...
        if( uppercase( S ) = uppercase( Open_File_Name ) ) then
        begin
            ShowMessage( 'Cannot overwrite file with itself.' ) ;
            exit ;
        end ;
        if( FileExists( S ) ) then
        begin
            if( MessageBox( 0, 'Overwrite existing file?', PChar( S ), MB_YESNO ) = IDNO ) then
            begin
                exit ;
            end ;
        end ;

        // Copy existing file to new file...
        assignfile( FO, S ) ;
        {$I-}
        rewrite( FO, 1 ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            ShowMessage( ERT( Dummy ) ) ;
            exit ;
        end ;
        fillchar( Header, sizeof( Header ), 0 ) ;
        Header.Prefix := 65535 ;
        Header.ID := 255 ;
        Header.Version := 10 ;
        Header.Facility := 123 ; // Tape
        Header.Format_Name := Tape_Header.Format.Text ;
        Header.BPI := Tape_Header.BPI.Value ;
        Header.Length := Tape_Header.Length.Value ;
        Header.After := Tape_Header.Include_After.Checked ;
        Header.Size_Length := Tape_Header.Size_Record.Value ;
        blockwrite( FO, Header, sizeof( Header ), Dummy ) ;
        try
            seek( Open_File, 0 ) ;
            Dummy := sizeof( Buffer ) ;
            while( Dummy = sizeof( Buffer ) ) do
            begin
                {$I-}
                blockread( Open_File, Buffer, sizeof( Buffer ), Dummy ) ;
                {$I+}
                Loop := IOResult ;
                if( Loop <> 0 ) then
                begin
                    ShowMessage( ERT( Loop ) ) ;
                    exit ;
                end ;
                {$I-}
                blockwrite( FO, Buffer, Dummy ) ;
                {$I+}
                Loop := IOResult ;
                if( Loop <> 0 ) then
                begin
                    ShowMessage( ERT( Loop ) ) ;
                    exit ;
                end ;
            end ;
        finally
            {$I-}
            closefile( FO ) ;
            {$I+}
            IOResult ;
        end ;

        // Open the new file...
        File_Name.Text := S ;
        Open_Button.Click ;
    end ;
end ; // TMedia_Manager.Convert_ButtonClick


procedure TMedia_Manager.Edit_HeaderClick(Sender: TObject);

var Dummy : integer ;

begin
    seek( Open_File, 0 ) ;
    blockread( Open_File, Header, sizeof( Header ) ) ;
    Tape_Header.Format.Text := Header.Format_Name ;
    Tape_Header.BPI.Value := Header.BPI ;
    Tape_Header.Length.Value := Header.Length ;
    Tape_Header.Include_After.Checked := Header.After ;
    Tape_Header.Size_Record.Value := Header.Size_Length ;
    Tape_Header.Internal_Format.Enabled := False ;
    if( Tape_Header.ShowModal = mrCancel ) then
    begin
        exit ;
    end ;
    Header.Format_Name := Tape_Header.Format.Text ;
    Header.BPI := Tape_Header.BPI.Value ;
    Header.Length := Tape_Header.Length.Value ;
    Header.After := Tape_Header.Include_After.Checked ;
    Header.Size_Length := Tape_Header.Size_Record.Value ;
    seek( Open_File, 0 ) ;
    {$I-}
    blockwrite( Open_File, Header, sizeof( Header ) ) ;
    {$I+}
    Dummy := IOResult ;
    if( Dummy <> 0 ) then
    begin
        ShowMessage( ERT( Dummy ) ) ;
    end ;
end ;


end.
