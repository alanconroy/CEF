unit Copy_Files_DOS11;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, Spin, Buttons, StdCtrls, ExtCtrls,

     // CEF
     CEFMedia ; // TTape_Media

type
  TCopy_Tape_DOS11_Form = class(TForm)
    Bottom_Panel: TPanel;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    File_Name: TEdit;
    SpeedButton1: TSpeedButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Proj: TSpinEdit;
    Prog: TSpinEdit;
    Label7: TLabel;
    Name: TEdit;
    Label8: TLabel;
    Ext: TEdit;
    Label9: TLabel;
    Prot: TSpinEdit;
    Copy_Button: TSpeedButton;
    Cancel_Copy_Button: TSpeedButton;
    OpenDialog1: TOpenDialog;
    On_Tape: TListBox;
    Label10: TLabel;
    Blocking: TSpinEdit;
    Label11: TLabel;
    Date_Value: TSpinEdit;
    Copy_From_Tape_Button: TSpeedButton;
    Label12: TLabel;
    Output_File: TEdit;
    SpeedButton3: TSpeedButton;
    SaveDialog1: TSaveDialog;
    Output_Text: TCheckBox;
    Include_PPN: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure File_NameChange(Sender: TObject);
    procedure NameChange(Sender: TObject);
    procedure Copy_ButtonClick(Sender: TObject);
    procedure Cancel_Copy_ButtonClick(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure On_TapeClick(Sender: TObject);
    procedure Copy_From_Tape_ButtonClick(Sender: TObject);
    procedure NameKeyPress(Sender: TObject; var Key: Char);
    procedure ExtKeyPress(Sender: TObject; var Key: Char);

  private // Instance data...
      Multi_Copy_Mode : boolean ; // True if copying multiple files
      Copy_Confirmed : boolean ; // True when user confirms a file during multiple file copy
      Cancel_Copy : boolean ; // True to cancel a multiple file copy
      _Media : TTape_Media ;

      procedure Set_Media( M : TTape_Media ) ;

  public // API...
      procedure Copy_File ;

      property Media : TTape_Media
          read _Media
          write Set_Media ;
  end ;

function Copy_Tape_DOS11_Form : TCopy_Tape_DOS11_Form ;

implementation

{$R *.dfm}

uses // C&C...
     CommonUt, // Edit
     _FiP, // Filespec_String_Scan
     _Filesys, // TFSSS
     Radix50s, // Rad
     UE, // DOS_ERT

     // CEF...
     Output_Error,
     Overwrite_Error ;

var _Copy_Tape_DOS11_Form : TCopy_Tape_DOS11_Form = nil ;

function Copy_Tape_DOS11_Form : TCopy_Tape_DOS11_Form ;

begin
    if( _Copy_Tape_DOS11_Form = nil ) then
    begin
        _Copy_Tape_DOS11_Form := TCopy_Tape_DOS11_Form.Create( Application ) ;
    end ;
    Result := _Copy_Tape_DOS11_Form ;
end ;


procedure TCopy_Tape_DOS11_Form.SpeedButton1Click(Sender: TObject);

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
end ;


procedure TCopy_Tape_DOS11_Form.File_NameChange(Sender: TObject);

var FSSS : TFSSS ;
    Loop : integer ;
    S : string ;

begin
    Copy_Button.Enabled := ( File_Name.Text <> '' )
                           and
                           ( length( File_Name.Text ) < 13 ) ;
    Filespec_String_Scan( File_Name.Text, '\', FSSS ) ;
    S := Edit( FSSS.Name, -1 ) ;
    for Loop := length( S ) downto 1 do
    begin
        if( pos( S[ Loop ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' ) = 0 ) then
        begin
            S := copy( S, 1, Loop - 1 ) + copy( S, Loop + 1, length( S ) ) ;
        end ;
    end ;
    if( S = '' ) then
    begin
        S := 'A' ;
    end ;
    Name.Text := copy( S, 1, 8 ) ;

    S := Edit( FSSS.Extension, -1 ) ;
    for Loop := length( S ) downto 1 do
    begin
        if( pos( S[ Loop ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' ) = 0 ) then
        begin
            S := copy( S, 1, Loop - 1 ) + copy( S, Loop + 1, length( S ) ) ;
        end ;
    end ;
    Ext.Text := copy( S, 1, 3 ) ;
end ;


procedure TCopy_Tape_DOS11_Form.NameChange(Sender: TObject);

begin
    Copy_Button.Enabled := ( ( Name.Text <> '' ) or ( Ext.Text <> '' ) )
                           and
                           ( length( Name.Text ) < 9 )
                           and
                           ( length( Ext.Text ) < 4 ) ;
end ;


procedure TCopy_Tape_DOS11_Form.Copy_ButtonClick(Sender: TObject);

begin
    if( Multi_Copy_Mode ) then
    begin
        Copy_Confirmed := True ;
        exit ;
    end ;
    Copy_File ;
end ;


type DOS_Label = record
                     Name : longint ;
                     Typ : word ;
                     Prog : byte ;
                     Proj : byte ;
                     Prot : byte ;
                     Unused0 : byte ;
                     Date : word ;
                     Unused1 : word ;
                 end ;


procedure TCopy_Tape_DOS11_Form.Copy_File ;

var Block : array[ 0..32767 ] of byte ;
    Count, Dummy : integer ;
    F : file ;
    Header : DOS_Label ;

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
        Media.Seek( 1, True, True ) ; // Move past TM (each file ends with a TM)
        fillchar( Header, sizeof( Header ), 0 ) ;
        while( length( Name.Text ) < 6 ) do
        begin
            Name.Text := Name.Text + ' ' ;
        end ;
        while( length( Ext.Text ) < 3 ) do
        begin
            Ext.Text := Ext.Text + ' ' ;
        end ;
        Header.Name := Rad50( Name.Text ) and $FFFF ;
        Header.Name := Header.Name or ( Rad50( copy( Name.Text, 4, 3 ) ) shl 16 ) ;
        Header.Typ := Rad50( Ext.Text ) ;
        Header.Prog := Prog.Value ;
        Header.Proj := Proj.Value ;
        Header.Prot := Prot.Value ;
        Header.Date := Date_Value.Value ;
        Media.Write_Record( @Header, 14 ) ;
        while( not eof( F ) ) do
        begin
            fillchar( Block, Blocking.Value, 0 ) ;
            blockread( F, Block, Blocking.Value, Count ) ;
            Media.Write_Record( @Block, Blocking.Value ) ;
        end ;
        On_Tape.Items.Add( '[' + inttostr( Proj.Value ) + ',' +
            inttostr( Prog.Value ) + ']' + Name.Text + '.' + Ext.Text + '<' +
            inttostr( Prot.Value ) + '>  ' + inttostr( ( filesize( F ) + 511 ) and ( not 511 ) ) + ' bytes' ) ;
        Media.Add_TM ;
        Media.Add_TM ;
    finally
        {$I-}
        closefile( F ) ;
        {$I+}
        IOResult ;
    end ;
end ; // TCopy_Tape_DOS11_Form.Copy_File 


procedure TCopy_Tape_DOS11_Form.Cancel_Copy_ButtonClick(Sender: TObject);

begin
    Cancel_Copy := True ;
end ;


procedure TCopy_Tape_DOS11_Form.Set_Media( M : TTape_Media ) ;

var Count : integer ;
    Lab : DOS_Label ;
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
        if( ( Count = 14 ) or ( Count = 12 ) ) then // Tape label
        begin
            if( S <> '' ) then
            begin
                On_Tape.Items.Add( S + ' ' + inttostr( Size ) + ' bytes' ) ;
                Size := 0 ;
            end ;
            for Loop := 0 to 13 do
            begin
                Temp[ Loop ] := M.Get_Byte( M.Position( False ) + Loop ) ;
            end ;
            move( Temp, Lab, 14 ) ;
            S := '[' + inttostr( Lab.Proj ) + ',' + inttostr( Lab.Prog ) + ']' +
                Rad( Lab.Name ) + '.' + Edit( Rad( Lab.Typ ), -1 ) +
                '<' + inttostr( Lab.Prot ) + '>' ;
        end else
        begin
            Size := Size + Count ;
            if(
                ( Count = 0 ) // TM
                and
                ( S <> '' )
              ) then
            begin
                On_Tape.Items.Add( S + ' ' + inttostr( Size ) + ' bytes' ) ;
                S := '' ;
                Size := 0 ;
            end ;
        end ;
        inc( Rec ) ;
    end ; // while( not M.At_LEOT )
    if( S <> '' ) then
    begin
        On_Tape.Items.Add( S + ' ' + inttostr( Size ) + ' bytes' ) ;
    end ;
end ; // TCopy_Tape_DOS11_Form.Set_Media


procedure TCopy_Tape_DOS11_Form.SpeedButton3Click(Sender: TObject);

begin
    if( SaveDialog1.Execute ) then
    begin
        Output_File.Text := SaveDialog1.FileName ;
    end ;
end ;


procedure TCopy_Tape_DOS11_Form.On_TapeClick( Sender : TObject ) ;

begin
    Copy_From_Tape_Button.Enabled := On_Tape.SelCount > 0 ;
end ;


procedure TCopy_Tape_DOS11_Form.Copy_From_Tape_ButtonClick( Sender: TObject) ;

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

            // Remove PPN...
            Dummy := pos( '<', S ) ;
            S := copy( S, 1, Dummy - 1 ) ;

            if( not Include_PPN.Checked ) then
            begin
                // Remove protection and size...
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


procedure TCopy_Tape_DOS11_Form.NameKeyPress(Sender: TObject; var Key: Char);

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    Key := upcase( Key ) ;
    if( pos( Key, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$' ) = 0 ) then
    begin
        Key := #0 ;
        exit ;
    end ;
    if( length( Name.Text ) > 5 ) then
    begin
        Key := #0 ;
    end ;
end ;


procedure TCopy_Tape_DOS11_Form.ExtKeyPress(Sender: TObject; var Key: Char);

begin
    if( Key = #8 ) then
    begin
        exit ;
    end ;
    Key := upcase( Key ) ;
    if( pos( Key, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$' ) = 0 ) then
    begin
        Key := #0 ;
        exit ;
    end ;
    if( length( Ext.Text ) > 2 ) then
    begin
        Key := #0 ;
    end ;
end ;


end.
