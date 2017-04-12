{
        Program Name : ANSIPres
        Package Name : CEF32
        Purpose      : ANSI Tape Presenter
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

          This form is used to display formatted DOS11 tape data.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit ANSIPres ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, Buttons, StdCtrls, ComCtrls, Spin,

     // CEF32...
     CEF, // PCEF_Media_File_Header
     ArrayInt, // TCOM_Array_Interface
     _UE, // TUnified_Exception
     CEFMedia, // TPresenter
     DataDis, // TData_Display
     TapePres ; // TGeneric_Tape_Media


type TANSI_Tape_Form = class(TForm)
            Command_Panel: TPanel;
            Record_Size_Label: TLabel;
            PageControl1: TPageControl;
            TabSheet1: TTabSheet;
            TabSheet2: TTabSheet;
            Data_Panel: TPanel;
            Full_Filename: TLabel;
            Error: TLabel;
            Label_Panel: TPanel;
            Label3: TLabel;
            HDR1_Label: TLabel;
            Label4: TLabel;
            Filename_Label: TLabel;
            Label5: TLabel;
            Section_Label: TLabel;
            Label6: TLabel;
            File_Set_Label: TLabel;
            Label7: TLabel;
            Gen_Number_Label: TLabel;
            TM_Panel: TPanel;
            Label8: TLabel;
            Sequence_Label: TLabel;
            Label9: TLabel;
            Copy_Files_Button: TSpeedButton;
            Label1: TLabel;
            Record_Selector: TSpinEdit;
            Current_Record_Label: TLabel;
            Rewind_Button: TSpeedButton;
            FF_Button: TSpeedButton;
            Label2: TLabel;
            Label10: TLabel;
            Add_Data_Button: TSpeedButton;
            SpeedButton1: TSpeedButton;
            Vol_Label_Panel: TPanel;
            Label11: TLabel;
            Vol_Lab_ID: TLabel;
            Label12: TLabel;
            Label13: TLabel;
            Label_Number: TLabel;
            Label14: TLabel;
            Vol_ID: TLabel;
            Label15: TLabel;
            Accessibility: TLabel;
            Label16: TLabel;
            Vol_Owner: TLabel;
            Label17: TLabel;
            Version: TLabel;
            Label18: TLabel;
            Label19: TLabel;
            Proj1: TLabel;
            Prog1: TLabel;
            Label20: TLabel;
            Label21: TLabel;
            Label22: TLabel;
            Label23: TLabel;
            Label24: TLabel;
            Label25: TLabel;
            Gen_Version_Label: TLabel;
            Creation_Date_Label: TLabel;
            Expiration_Date_Label: TLabel;
            Accessibility_Label: TLabel;
            Block_Count_Label: TLabel;
            System_Label: TLabel;
            Label26: TLabel;
            HDR1_Number: TLabel;
            HDR2_Panel: TPanel;
            Label27: TLabel;
            HDR2_Label: TLabel;
            Label29: TLabel;
            HDR2_Number: TLabel;
            Label31: TLabel;
            Label28: TLabel;
            Label30: TLabel;
            Label32: TLabel;
            Label33: TLabel;
            Label34: TLabel;
            HDR2_Offset: TLabel;
            HDR2_Depends: TLabel;
            HDR2_Record_Length: TLabel;
            HDR2_Block_Length: TLabel;
            HDR2_Format: TLabel;
            SpeedButton2: TSpeedButton;
            procedure FormCreate(Sender: TObject);
            procedure Rewind_ButtonClick(Sender: TObject);
            procedure FF_ButtonClick(Sender: TObject);
            procedure Copy_Files_ButtonClick(Sender: TObject);
            procedure Record_SelectorChange(Sender: TObject);
            procedure Add_Data_ButtonClick(Sender: TObject);
            procedure SpeedButton1Click(Sender: TObject);
            procedure SpeedButton2Click(Sender: TObject);

          private // Instance data...
              __Array : TMedia ;
              Panel : TData_Display ;
              Sub_Array : TCOM_Array_Interface ;
              Tape : TGeneric_Tape_Media ;

          protected // Property handlers...
              procedure Set_Array( A : TMedia ) ;

          public // API...
              procedure Get_Record( Num : int64 ) ;
              property _Array : TMedia
                  read __Array
                  write Set_Array ;
          end ;

type TANSI_Tape_Presenter = class( TPresenter )
                                 public // Constructors and destructors...
                                     constructor Create( Parent : THandle ;
                                         _Array : TMedia ;
                                         Header : PCEF_Media_File_Header ) ;

                                 private // Instance data...
                                     Panel : TANSI_Tape_Form ;

                                 public // API...
                                     function Terminate : TUnified_Exception ; override ;
                                     procedure Redraw ; override ;
                                     procedure Set_Bounds( Width, Height : integer ) ;
                                         override ;
                                     procedure Get_Header( var Header : PCEF_Media_File_Header ) ;
                                         override ;
                             end ;

implementation

{$R *.dfm}

uses // C&C...
     CommonUt, // Edit
     Radix50s,
     Copy_Files_ANSI,
     TapePres_Add_Data,
     ANSI_Label ;

type TANSI_Tape_Media = class( TGeneric_Tape_Media )
                             public // Constructors and destructors...
                                 constructor Create( _Int : TMedia ) ;
                         end ;

constructor TANSI_Tape_Media.Create( _Int : TMedia ) ;

begin
    inherited Create( _Int ) ;
    
    After := True ;
    Count_Length := 4 ;
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



// TANSI_Tape_Presenter methods...

// Constructors and destructors...

constructor TANSI_Tape_Presenter.Create( Parent : THandle ;
    _Array : TMedia ; Header : PCEF_Media_File_Header ) ;

var _Header : PCEF_Tape_Media_Header ;

begin
    inherited Create ;

    Panel := TANSI_Tape_Form.Create( Application ) ;
    Panel.ParentWindow := Parent ;
    Panel.Tape := TANSI_Tape_Media.Create( _Array ) ;
    if( Header <> nil ) then
    begin
        _Header := PCEF_Tape_Media_Header( Header ) ;
        Panel.Tape.Set_BPI( _Header.BPI ) ;
        Panel.Tape.Set_Length( _Header.Length ) ;
        Panel.Tape.Set_Count_Length( _Header.Size_Length ) ;
        Panel.Tape.Set_After( _Header.After ) ;
    end ;
    Panel._Array := _Array ;
    Panel.Top := 0 ;
    Panel.Left := 0 ;
    Panel.Get_Record( 1 ) ;
end ;


// API...

function TANSI_Tape_Presenter.Terminate : TUnified_Exception ;

begin
    Result := nil ;
    Panel.Free ;
    Panel := nil ;
    Free ;
end ;


procedure TANSI_Tape_Presenter.Redraw ;

begin
    Panel.Panel.Repaint ;
end ;


procedure TANSI_Tape_Presenter.Set_Bounds( Width, Height : integer ) ;

begin
    Panel.SetBounds( 0, 0, Width, Height ) ;
    Panel.Repaint ;
    Redraw ;
end ;


procedure TANSI_Tape_Presenter.Get_Header( var Header : PCEF_Media_File_Header ) ;

var H : PCEF_Tape_Media_Header ;

begin
    H := PCEF_Tape_Media_Header( Header ) ;
    H.Prefix := 65535 ;
    H.ID := 255 ;
    H.Facility := 123 ; // Tape
    H.Version := 10 ;
    H.Format_Name := 'ANSI Tape' ;
    H.BPI := 0 ;
    H.Length := 0 ;
    H.Size_Length := 4 ;
    H.After := True ;
    fillchar( H.Reserved, sizeof( H.Reserved ), 0 ) ;
end ;


// TANSI_Tape_Form methods...

procedure TANSI_Tape_Form.FormCreate(Sender: TObject);

begin
    Panel := TData_Display.Create( Owner ) ;
    Panel.Parent := Data_Panel ;
    Panel.Align := alClient ;
    Sub_Array := TSub_Array_Interface.Create ;
end ;


procedure TANSI_Tape_Form.Get_Record( Num : int64 ) ;

var Count : integer ;
    Lab : ANSI_HDR1_Label ;
    Lab2 : ANSI_HDR2_Label ;
    Loop : integer ;
    S : string ;
    Temp : array[ 0..79 ] of byte ;
    Vol_Lab : ANSI_Vol_Label ;

begin
    // Setup and sanity check...
    if( ( Tape = nil ) or ( Num < 1 ) or ( Num > Tape.Record_Count ) ) then
    begin
        exit ;
    end ;
    TM_Panel.Top := 0 ;
    Vol_Label_Panel.Top := 0 ;
    Vol_Label_Panel.Left := 0 ;
    HDR2_Panel.Top := 0 ;
    HDR2_Panel.Left := 0 ;

    // Position to requested record...
    Tape.Seek( Num - 1, True, False ) ;
    Count := Tape.Record_Length( Num - 1 ) ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
    Record_Selector.MaxValue := Tape.Record_Count ;
    if( ( Num = 1 ) and ( Count <> 80 ) ) then
    begin
        Error.Caption := 'Invalid ANSI volume label' ;
    end ;
    if( Count = 80 ) then // Tape label
    begin
        for Loop := 0 to 79 do
        begin
            Temp[ Loop ] := _Array.Get_Byte( Tape.Position( False ) + Loop ) ;
        end ;
        setlength( S, 4 ) ;
        move( Temp, Pchar( S )[ 0 ], 4 ) ;
        if( S = 'VOL1' ) then
        begin
            move( Temp, Vol_Lab, 80 ) ;
            setlength( S, 3 ) ;
            move( Vol_Lab.ID, PChar( S )[ 0 ], 3 ) ;
            Vol_Lab_ID.Caption := S ;
            Label_Number.Caption := Vol_Lab.Num ;
            setlength( S, 6 ) ;
            move( Vol_Lab.Vol_ID, PChar( S )[ 0 ], 6 ) ;
            Vol_ID.Caption := S ;
            Accessibility.Caption := Vol_Lab.Accessibility ;
            setlength( S, 14 ) ;
            move( Vol_Lab.Owner, PChar( S )[ 0 ], 14 ) ;
            Vol_Owner.Caption := S ;
            if( copy( S, 1, 3 ) = 'D%B' ) then // DEC PDP-11
            begin
                Proj1.Visible := True ;
                Prog1.Visible := True ;
                Label18.Visible := True ;
                Label19.Visible := True ;
                Proj1.Caption := copy( S, 8, 3 ) ;
                Prog1.Caption := copy( S, 11, 3 ) ;
            end else
            begin
                Proj1.Visible := False ;
                Prog1.Visible := False ;
                Label18.Visible := False ;
                Label19.Visible := False ;
            end ;
            Version.Caption := Vol_Lab.Version ;

            Vol_Label_Panel.Visible := True ;
            Label_Panel.Visible := False ;
            TM_Panel.Visible := False ;
            HDR2_Panel.Visible := False ;
        end else
        if( ( S = 'HDR1' ) or ( S = 'EOF1' ) or ( S = 'EOV1' ) ) then
        begin
            move( Temp, Lab, 80 ) ;
            HDR1_Label.Caption := copy( S, 1, 3 ) ;
            HDR1_Number.Caption := S[ 4 ] ;
            setlength( S, 17 ) ;
            move( Lab.Filename, PChar( S )[ 0 ], 17 ) ;
            Filename_Label.Caption := S ;
            setlength( S, 6 ) ;
            move( Lab.File_Set, PChar( S )[ 0 ], 6 ) ;
            File_Set_Label.Caption := S ;
            setlength( S, 4 ) ;
            move( Lab.Section, PChar( S )[ 0 ], 4 ) ;
            Section_Label.Caption := S ;
            setlength( S, 4 ) ;
            move( Lab.Sequence, PChar( S )[ 0 ], 4 ) ;
            Sequence_Label.Caption := S ;
            setlength( S, 4 ) ;
            move( Lab.Generation_Number, PChar( S )[ 0 ], 4 ) ;
            Gen_Number_Label.Caption := S ;
            setlength( S, 2 ) ;
            move( Lab.Generation_Version, PChar( S )[ 0 ], 2 ) ;
            Gen_Version_Label.Caption := S ;
            setlength( S, 6 ) ;
            move( Lab.Creation_Date, PChar( S )[ 0 ], 6 ) ;
            Creation_Date_Label.Caption := S ;
            setlength( S, 6 ) ;
            move( Lab.Expiration_Date, PChar( S )[ 0 ], 6 ) ;
            Expiration_Date_Label.Caption := S ;
            Accessibility_Label.Caption := Lab.Accessibility ;
            setlength( S, 6 ) ;
            move( Lab.Block_Count, PChar( S )[ 0 ], 6 ) ;
            Block_Count_Label.Caption := S ;
            setlength( S, 13 ) ;
            move( Lab.System, PChar( S )[ 0 ], 13 ) ;
            System_Label.Caption := S ;

            Vol_Label_Panel.Visible := False ;
            Label_Panel.Visible := True ;
            TM_Panel.Visible := False ;
            HDR2_Panel.Visible := False ;
        end else
        if( ( S = 'HDR2' ) or ( S = 'EOF2' ) or ( S = 'EOV2' ) ) then
        begin
            move( Temp, Lab2, 80 ) ;
            HDR2_Label.Caption := copy( S, 1, 3 ) ;
            HDR2_Number.Caption := S[ 4 ] ;
            HDR2_Format.Caption := Lab2.Record_Format ;
            setlength( S, 5 ) ;
            move( Lab2.Block_Length, PChar( S )[ 0 ], 5 ) ;
            HDR2_Block_Length.Caption := S ;
            setlength( S, 5 ) ;
            move( Lab2.Record_Length, PChar( S )[ 0 ], 5 ) ;
            HDR2_Record_Length.Caption := S ;
            setlength( S, 35 ) ;
            move( Lab2.Depends, PChar( S )[ 0 ], 35 ) ;
            HDR2_Depends.Caption := S ;
            setlength( S, 2 ) ;
            move( Lab2.Offset, PChar( S )[ 0 ], 2 ) ;
            HDR2_Offset.Caption := S ;

            Label_Panel.Visible := False ;
            Vol_Label_Panel.Visible := False ;
            TM_Panel.Visible := False ;
            HDR2_Panel.Visible := True ;
        end else
        begin
            Label_Panel.Visible := False ;
            TM_Panel.Visible := False ;
            Vol_Label_Panel.Visible := False ;
            HDR2_Panel.Visible := False ;
        end ;
    end else
    if( Count = 0 ) then
    begin
        Label_Panel.Visible := False ;
        TM_Panel.Visible := True ;
        Vol_Label_Panel.Visible := False ;
        HDR2_Panel.Visible := False ;
    end else
    begin
        Label_Panel.Visible := False ;
        TM_Panel.Visible := False ;
        Vol_Label_Panel.Visible := False ;
        HDR2_Panel.Visible := False ;
    end ;

    // Display data for the record...
    Record_Size_Label.Caption := inttostr( Count ) ;

    TSub_Array_Interface( Sub_Array ).Offset := Tape.Position( False ) ;
    TSub_Array_Interface( Sub_Array ).Length := Count ;
    Panel._Array := Sub_Array ;
    Panel.Repaint ;
end ;


procedure TANSI_Tape_Form.Set_Array( A : TMedia ) ;

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


procedure TANSI_Tape_Form.Rewind_ButtonClick(Sender: TObject) ;

begin
    Get_Record( 1 ) ;
    Record_Selector.Value := 1 ;
end ;


procedure TANSI_Tape_Form.FF_ButtonClick(Sender: TObject) ;

begin
    Get_Record( Tape.Record_Count ) ;
    Record_Selector.Value := Tape.Record_Count ;
end ;


procedure TANSI_Tape_Form.Copy_Files_ButtonClick(Sender: TObject);

begin
    Copy_Tape_ANSI_Form.Media := Tape ;
    Copy_Tape_ANSI_Form.ShowModal ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
    Record_Selector.MaxValue := Tape.Record_Count ;
end ;


procedure TANSI_Tape_Form.Record_SelectorChange(Sender: TObject) ;

begin
    Get_Record( Record_Selector.Value ) ;
end ;



procedure TANSI_Tape_Form.Add_Data_ButtonClick(Sender: TObject);

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
        end ; // while( length( S ) > 0 )
    end ; // if( Tape_Add_Data_Dialog.ShowModal = mrOK )
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
    Record_Selector.MaxValue := Tape.Record_Count ;
end ;


procedure TANSI_Tape_Form.SpeedButton1Click(Sender: TObject) ;

begin
    Tape.Seek_LEOT ;
    Tape.Add_TM ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
    Record_Selector.MaxValue := Tape.Record_Count ;
end ;


procedure TANSI_Tape_Form.SpeedButton2Click(Sender: TObject) ;

    function Get( E : TEdit ) : string ;

    begin
        Result := E.Text ;
        while( length( Result ) < E.Tag ) do
        begin
            Result := Result + ' ' ;
        end ;
    end ;

var Lab : ANSI_HDR1_Label ;
    Lab2 : ANSI_HDR2_Label ;
    Vol_Lab : ANSI_Vol_Label ;
    S : string ;

begin
    if( ANSI_Label_Form.ShowModal = mrOK ) then
    begin
        Tape.Seek_LEOT ;
        Tape.Seek( 1, True, True ) ;
        S := ANSI_Label_Form.PageControl.ActivePage.Caption ;
        if( S = 'HDR1' ) then
        begin
            Lab.ID := 'HDR' ;
            Lab.Number := '1' ;
            S := Get( ANSI_Label_Form.HDR1_Filename ) ;
            move( S[ 1 ], Lab.Filename, sizeof( Lab.Filename ) ) ;
            S := Get( ANSI_Label_Form.HDR1_File_Set ) ;
            move( S[ 1 ], Lab.File_Set, sizeof( Lab.File_Set ) )  ;
            S := Get( ANSI_Label_Form.HDR1_Section ) ;
            move( S[ 1 ], Lab.Section, sizeof( Lab.Section ) ) ;
            S := Get( ANSI_Label_Form.HDR1_Sequence ) ;
            move( S[ 1 ], Lab.Sequence, sizeof( Lab.Sequence ) ) ;
            S := Get( ANSI_Label_Form.HDR1_Gen_Number ) ;
            move( S[ 1 ], Lab.Generation_Number, sizeof( Lab.Generation_Number ) ) ;
            S := Get( ANSI_Label_Form.HDR1_Gen_Version ) ;
            move( S[ 1 ], Lab.Generation_Version, sizeof( Lab.Generation_Version ) ) ;
            S := Get( ANSI_Label_Form.HDR1_Creation_Date ) ;
            move( S[ 1 ], Lab.Creation_Date, sizeof( Lab.Creation_Date ) ) ;
            S := Get( ANSI_Label_Form.HDR1_Expiration_Date ) ;
            move( S[ 1 ], Lab.Expiration_Date, sizeof( Lab.Expiration_Date ) ) ;
            S := Get( ANSI_Label_Form.HDR1_Accessibility ) ;
            move( S[ 1 ], Lab.Accessibility, sizeof( Lab.Accessibility ) ) ;
            S := Get( ANSI_Label_Form.HDR1_Block_Count ) ;
            move( S[ 1 ], Lab.Block_Count, sizeof( Lab.Block_Count ) ) ;
            S := Get( ANSI_Label_Form.HDR1_System ) ;
            move( S[ 1 ], Lab.System, sizeof( Lab.System ) ) ;
            fillchar( Lab.Reserved, sizeof( Lab.Reserved ), 32 )  ;
        end else
        if( S = 'HDR2' ) then
        begin
            Lab2.ID := 'HDR' ;
            Lab2.Number := '2' ;
            S := Get( ANSI_Label_Form.HDR2_Format ) ;
            move( S[ 1 ], Lab2.Record_Format, sizeof( Lab2.Record_Format ) ) ;
            S := Get( ANSI_Label_Form.HDR2_Block_Length ) ;
            move( S[ 1 ], Lab2.Block_Length, sizeof( Lab2.Block_Length ) ) ;
            S := Get( ANSI_Label_Form.HDR2_Record_Length ) ;
            move( S[ 1 ], Lab2.Record_Length, sizeof( Lab2.Record_Length ) ) ;
            S := Get( ANSI_Label_Form.HDR2_Depends ) ;
            move( S[ 1 ], Lab2.Depends, sizeof( Lab2.Depends ) ) ;
            S := Get( ANSI_Label_Form.HDR2_Offset ) ;
            move( S[ 1 ], Lab2.Offset, sizeof( Lab2.Offset ) ) ;
            fillchar( Lab2.Reserved, sizeof( Lab2.Reserved ), 32 )  ;
            move( Lab2, Lab, 80 ) ;
        end else
        if( S = 'EOF1' ) then
        begin
            Lab.ID := 'EOF' ;
            Lab.Number := '1' ;
            S := Get( ANSI_Label_Form.EOF1_Filename ) ;
            move( S[ 1 ], Lab.Filename, sizeof( Lab.Filename ) ) ;
            S := Get( ANSI_Label_Form.EOF1_File_Set ) ;
            move( S[ 1 ], Lab.File_Set, sizeof( Lab.File_Set ) )  ;
            S := Get( ANSI_Label_Form.EOF1_Section ) ;
            move( S[ 1 ], Lab.Section, sizeof( Lab.Section ) ) ;
            S := Get( ANSI_Label_Form.EOF1_Sequence ) ;
            move( S[ 1 ], Lab.Sequence, sizeof( Lab.Sequence ) ) ;
            S := Get( ANSI_Label_Form.EOF1_Gen_Number ) ;
            move( S[ 1 ], Lab.Generation_Number, sizeof( Lab.Generation_Number ) ) ;
            S := Get( ANSI_Label_Form.EOF1_Gen_Version ) ;
            move( S[ 1 ], Lab.Generation_Version, sizeof( Lab.Generation_Version ) ) ;
            S := Get( ANSI_Label_Form.EOF1_Creation_Date ) ;
            move( S[ 1 ], Lab.Creation_Date, sizeof( Lab.Creation_Date ) ) ;
            S := Get( ANSI_Label_Form.EOF1_Expiration_Date ) ;
            move( S[ 1 ], Lab.Expiration_Date, sizeof( Lab.Expiration_Date ) ) ;
            S := Get( ANSI_Label_Form.EOF1_Accessibility ) ;
            move( S[ 1 ], Lab.Accessibility, sizeof( Lab.Accessibility ) ) ;
            S := Get( ANSI_Label_Form.EOF1_Block_Count ) ;
            move( S[ 1 ], Lab.Block_Count, sizeof( Lab.Block_Count ) ) ;
            S := Get( ANSI_Label_Form.EOF1_System ) ;
            move( S[ 1 ], Lab.System, sizeof( Lab.System ) ) ;
            fillchar( Lab.Reserved, sizeof( Lab.Reserved ), 32 )  ;
        end else
        if( S = 'EOF2' ) then
        begin
            Lab2.ID := 'EOF' ;
            Lab2.Number := '2' ;
            S := Get( ANSI_Label_Form.EOF2_Format ) ;
            move( S[ 1 ], Lab2.Record_Format, sizeof( Lab2.Record_Format ) ) ;
            S := Get( ANSI_Label_Form.EOF2_Block_Length ) ;
            move( S[ 1 ], Lab2.Block_Length, sizeof( Lab2.Block_Length ) ) ;
            S := Get( ANSI_Label_Form.EOF2_Record_Length ) ;
            move( S[ 1 ], Lab2.Record_Length, sizeof( Lab2.Record_Length ) ) ;
            S := Get( ANSI_Label_Form.EOF2_Depends ) ;
            move( S[ 1 ], Lab2.Depends, sizeof( Lab2.Depends ) ) ;
            S := Get( ANSI_Label_Form.EOF2_Offset ) ;
            move( S[ 1 ], Lab2.Offset, sizeof( Lab2.Offset ) ) ;
            fillchar( Lab2.Reserved, sizeof( Lab2.Reserved ), 32 )  ;
            move( Lab2, Lab, 80 ) ;
        end else
        if( S = 'EOV1' ) then
        begin
            Lab.ID := 'EOV' ;
            Lab.Number := '1' ;
            S := Get( ANSI_Label_Form.EOV1_Filename ) ;
            move( S[ 1 ], Lab.Filename, sizeof( Lab.Filename ) ) ;
            S := Get( ANSI_Label_Form.EOV1_File_Set ) ;
            move( S[ 1 ], Lab.File_Set, sizeof( Lab.File_Set ) )  ;
            S := Get( ANSI_Label_Form.EOV1_Section ) ;
            move( S[ 1 ], Lab.Section, sizeof( Lab.Section ) ) ;
            S := Get( ANSI_Label_Form.EOV1_Sequence ) ;
            move( S[ 1 ], Lab.Sequence, sizeof( Lab.Sequence ) ) ;
            S := Get( ANSI_Label_Form.EOV1_Gen_Number ) ;
            move( S[ 1 ], Lab.Generation_Number, sizeof( Lab.Generation_Number ) ) ;
            S := Get( ANSI_Label_Form.EOV1_Gen_Version ) ;
            move( S[ 1 ], Lab.Generation_Version, sizeof( Lab.Generation_Version ) ) ;
            S := Get( ANSI_Label_Form.EOV1_Creation_Date ) ;
            move( S[ 1 ], Lab.Creation_Date, sizeof( Lab.Creation_Date ) ) ;
            S := Get( ANSI_Label_Form.EOV1_Expiration_Date ) ;
            move( S[ 1 ], Lab.Expiration_Date, sizeof( Lab.Expiration_Date ) ) ;
            S := Get( ANSI_Label_Form.EOV1_Accessibility ) ;
            move( S[ 1 ], Lab.Accessibility, sizeof( Lab.Accessibility ) ) ;
            S := Get( ANSI_Label_Form.EOV1_Block_Count ) ;
            move( S[ 1 ], Lab.Block_Count, sizeof( Lab.Block_Count ) ) ;
            S := Get( ANSI_Label_Form.EOV1_System ) ;
            move( S[ 1 ], Lab.System, sizeof( Lab.System ) ) ;
            fillchar( Lab.Reserved, sizeof( Lab.Reserved ), 32 )  ;
        end else
        if( S = 'EOV2' ) then
        begin
            Lab2.ID := 'EOV' ;
            Lab2.Number := '2' ;
            S := Get( ANSI_Label_Form.EOV2_Format ) ;
            move( S[ 1 ], Lab2.Record_Format, sizeof( Lab2.Record_Format ) ) ;
            S := Get( ANSI_Label_Form.EOV2_Block_Length ) ;
            move( S[ 1 ], Lab2.Block_Length, sizeof( Lab2.Block_Length ) ) ;
            S := Get( ANSI_Label_Form.EOV2_Record_Length ) ;
            move( S[ 1 ], Lab2.Record_Length, sizeof( Lab2.Record_Length ) ) ;
            S := Get( ANSI_Label_Form.EOV2_Depends ) ;
            move( S[ 1 ], Lab2.Depends, sizeof( Lab2.Depends ) ) ;
            S := Get( ANSI_Label_Form.EOV2_Offset ) ;
            move( S[ 1 ], Lab2.Offset, sizeof( Lab2.Offset ) ) ;
            fillchar( Lab2.Reserved, sizeof( Lab2.Reserved ), 32 )  ;
            move( Lab2, Lab, 80 ) ;
        end else
        if( S = 'VOL1' ) then
        begin
            Vol_Lab.ID := 'VOL' ;
            Vol_Lab.Num := '1' ;
            S := Get( ANSI_Label_Form.VOL1_Volume_ID ) ;
            move( S[ 1 ], Vol_Lab.Vol_ID, sizeof( Vol_Lab.Vol_ID ) ) ;
            S := Get( ANSI_Label_Form.VOL1_Accessibility ) ;
            move( S[ 1 ], Vol_Lab.Accessibility, sizeof( Vol_Lab.Accessibility ) ) ;
            Vol_Lab.Reserved0 := '                          ' ;
            S := Get( ANSI_Label_Form.VOL1_Owner ) ;
            move( S[ 1 ], Vol_Lab.Owner, sizeof( Vol_Lab.Owner ) ) ;
            Vol_Lab.Reserved1 := '                            ' ;
            S := Get( ANSI_Label_Form.VOL1_Version ) ;
            move( S[ 1 ], Vol_Lab.Version, sizeof( Vol_Lab.Version ) ) ;
            move( Vol_Lab, Lab, 80 ) ;
        end ;
        Tape.Write_Record( @Lab, 80 ) ;
        Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
        Record_Selector.MaxValue := Tape.Record_Count ;
    end ;
end ;


end.
