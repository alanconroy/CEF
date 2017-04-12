{
        Program Name : DOS11Pres
        Package Name : CEF32
        Purpose      : DOS11 Tape Presenter
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

unit DOS11Pres ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, Buttons, StdCtrls, ComCtrls, Spin,

     // C&C...
     _UE, // TUnified_Exception

     // CEF...
     CEF, // PCEF_Media_File_Header
     CEFMedia, // TPresenter

     // CEF32...
     ArrayInt, // TCOM_Array_Interface
     DataDis, // TData_Display
     TapePres ; // TGeneric_Tape_Media


type TDOS11_Tape_Form = class(TForm)
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
            Name_Label: TLabel;
            Label4: TLabel;
            Type_Label: TLabel;
            Label5: TLabel;
            Prog_Label: TLabel;
            Label6: TLabel;
            Proj_Label: TLabel;
            Label7: TLabel;
            Date_Label: TLabel;
            TM_Panel: TPanel;
            Label8: TLabel;
            Prot_Label: TLabel;
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

type TDOS11_Tape_Presenter = class( TPresenter )
                                 public // Constructors and destructors...
                                     constructor Create( Parent : THandle ;
                                         _Array : TMedia ;
                                         Header : PCEF_Media_File_Header ) ;

                                 private // Instance data...
                                     Panel : TDOS11_Tape_Form ;

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
     Copy_Files_DOS11,
     TapePres_Add_Data,
     DOS11_Label ;

type TDOS11_Tape_Media = class( TGeneric_Tape_Media )
                             public // Constructors and destructors...
                                 constructor Create( _Int : TMedia ) ;
                         end ;

constructor TDOS11_Tape_Media.Create( _Int : TMedia ) ;

begin
    inherited Create( _Int ) ;
    
    After := True ;
    Count_Length := 4 ;
end ;


type DOS_Label = packed record
                     Name : longint ;
                     Typ : word ;
                     Prog : byte ;
                     Proj : byte ;
                     Prot : byte ;
                     Unused0 : byte ;
                     Date : word ;
                     Unused1 : word ;
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



// TDOS11_Tape_Presenter methods...

// Constructors and destructors...

constructor TDOS11_Tape_Presenter.Create( Parent : THandle ;
    _Array : TMedia ; Header : PCEF_Media_File_Header ) ;

var _Header : PCEF_Tape_Media_Header ;

begin
    inherited Create ;

    Panel := TDOS11_Tape_Form.Create( Application ) ;
    Panel.ParentWindow := Parent ;
    Panel.Tape := TDOS11_Tape_Media.Create( _Array ) ;
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

function TDOS11_Tape_Presenter.Terminate : TUnified_Exception ;

begin
    Result := nil ;
    Panel.Free ;
    Panel := nil ;
    Free ;
end ;


procedure TDOS11_Tape_Presenter.Redraw ;

begin
    Panel.Panel.Repaint ;
end ;


procedure TDOS11_Tape_Presenter.Set_Bounds( Width, Height : integer ) ;

begin
    Panel.SetBounds( 0, 0, Width, Height ) ;
    Panel.Repaint ;
    Redraw ;
end ;


procedure TDOS11_Tape_Presenter.Get_Header( var Header : PCEF_Media_File_Header ) ;

var H : PCEF_Tape_Media_Header ;

begin
    H := PCEF_Tape_Media_Header( Header ) ;
    H.Prefix := 65535 ;
    H.ID := 255 ;
    H.Facility := 123 ; // Tape
    H.Version := 10 ;
    H.Format_Name := 'DOS-11 Tape' ;
    H.BPI := 0 ;
    H.Length := 0 ;
    H.Size_Length := 4 ;
    H.After := True ;
    fillchar( H.Reserved, sizeof( H.Reserved ), 0 ) ;
end ;


// TDOS11_Tape_Form methods...

procedure TDOS11_Tape_Form.FormCreate(Sender: TObject);

begin
    Panel := TData_Display.Create( Owner ) ;
    Panel.Parent := Data_Panel ;
    Panel.Align := alClient ;
    Sub_Array := TSub_Array_Interface.Create ;
end ;


procedure TDOS11_Tape_Form.Get_Record( Num : int64 ) ;

var Count : integer ;
    Lab : DOS_Label ;
    Loop : integer ;
    Temp : array[ 0..13 ] of byte ;

begin
    // Setup and sanity check...
    if( ( Tape = nil ) or ( Num < 1 ) or ( Num > Tape.Record_Count ) ) then
    begin
        exit ;
    end ;
    TM_Panel.Top := 0 ;

    // Position to requested record...
    Tape.Seek( Num - 1, True, False ) ;
    Count := Tape.Record_Length( Num - 1 ) ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
    if( ( Count = 14 ) or ( Count = 12 ) ) then // Tape label
    begin
        for Loop := 0 to 13 do
        begin
            Temp[ Loop ] := _Array.Get_Byte( Tape.Position( False ) + Loop ) ;
        end ;
        move( Temp, Lab, 14 ) ;
        Full_Filename.Caption := '[' + inttostr( Lab.Proj ) + ',' + inttostr( Lab.Prog ) + ']' +
            Rad( Lab.Name ) + '.' + Edit( Rad( Lab.Typ ), -1 ) +
            '<' + inttostr( Lab.Prot ) + '>' ;
        if( ( Lab.Unused0 <> 0 ) or ( ( Lab.Unused1 <> 0 ) and ( Count = 14 ) ) ) then
        begin
            Error.Caption := 'Invalid DOS11 label' ;
        end ;
        Name_Label.Caption := Rad( Lab.Name ) ;
        Type_Label.Caption := Edit( Rad( Lab.Typ ), -1 ) ;
        Proj_Label.Caption := inttostr( Lab.Proj ) ;
        Prog_Label.Caption := inttostr( Lab.Prog ) ;
        Prot_Label.Caption := inttostr( Lab.Prot ) ;
        Date_Label.Caption := inttostr( Lab.Date ) ;
        Label_Panel.Visible := True ;
        TM_Panel.Visible := False ;
    end else // if( ( Count = 14 ) or ( Count = 12 ) )
    if( Count = 0 ) then
    begin
        Label_Panel.Visible := False ;
        TM_Panel.Visible := True ;
    end else
    begin
        Label_Panel.Visible := False ;
        TM_Panel.Visible := False ;
    end ;

(*
    // Position to requested record...
    Position := 0 ;
    Count := 0 ;
    Current_Record_Position := 0 ;
    Current_Record := 0 ;
    while( Current_Record <= Num ) do
    begin
        Label_Panel.Visible := False ;
        TM_Panel.Visible := False ;
        Error.Caption := '' ;
        Current_Record_Position := Position ;
        Count := Tape_Read_Long ;
        inc( Current_Record ) ;
        if( ( Count = 14 ) or ( Count = 12 ) ) then // Tape label
        begin
            for Loop := 0 to 13 do
            begin
                Temp[ Loop ] := _Array.Get_Byte( Position + Loop ) ;
            end ;
            move( Temp, Lab, 14 ) ;
            Full_Filename.Caption := '[' + inttostr( Lab.Proj ) + ',' + inttostr( Lab.Prog ) + ']' +
                Rad( Lab.Name ) + '.' + Edit( Rad( Lab.Typ ), -1 ) +
                '<' + inttostr( Lab.Prot ) + '>' ;
            if( ( Lab.Unused0 <> 0 ) or ( ( Lab.Unused1 <> 0 ) and ( Count = 14 ) ) ) then
            begin
                Error.Caption := 'Invalid DOS11 label' ;
            end ;
            Name_Label.Caption := Rad( Lab.Name ) ;
            Type_Label.Caption := Edit( Rad( Lab.Typ ), -1 ) ;
            Proj_Label.Caption := inttostr( Lab.Proj ) ;
            Prog_Label.Caption := inttostr( Lab.Prog ) ;
            Prot_Label.Caption := inttostr( Lab.Prot ) ;
            Date_Label.Caption := inttostr( Lab.Date ) ;
            Label_Panel.Visible := True ;
        end else // if( ( Count = 14 ) or ( Count = 12 ) )
        if( Count = 0 ) then
        begin
            TM_Panel.Visible := True ;
        end ;
        Position := Position + Count ;
        if( Count > 0 ) then
        begin
            Loop := Tape_Read_long ;
            if( Loop <> Count ) then
            begin
                Error.Caption := 'Invalid tape format' ;
            end ;
        end ;
    end ; // while( Current_Record <= Num )
    Position := Current_Record_Position ;
    Current_Record := Num ;
    Current_Record_Label.Caption := inttostr( Current_Record + 1 ) + ' of ' + inttostr( Record_Count ) ;
*)

    // Display data for the record...
    Record_Size_Label.Caption := inttostr( Count ) ;

    TSub_Array_Interface( Sub_Array ).Offset := Tape.Position( False ) ;
    TSub_Array_Interface( Sub_Array ).Length := Count ;
    Panel._Array := Sub_Array ;
    Panel.Repaint ;
end ;


procedure TDOS11_Tape_Form.Set_Array( A : TMedia ) ;

begin
    __Array := A ;
    TSub_Array_Interface( Sub_Array ).Master_Array := A ;
    Record_Selector.MaxValue := 0 ;
    if( A <> nil ) then
    begin
        TGeneric_Tape_Media( Tape ).Int := A ;
        Record_Selector.MaxValue := Tape.Record_Count ;
    end ;
(*
    Current_Record := 0 ;
    Record_Count := 0 ;
    Position := 0 ;
    while( not Tape.At_LEOT ) do
    begin
        Count := Tape.Record_Count ; // read start count
        inc( Record_Count ) ;
        Position := Position + Count ;
        if( Count > 0 ) then
        begin
            Tape.Record_Count ; // Skip past end count (unless TM)
        end ;
    end ;
    Get_Record( 0 ) ;
*)
end ;


procedure TDOS11_Tape_Form.Rewind_ButtonClick(Sender: TObject) ;

begin
    Get_Record( 1 ) ;
    Record_Selector.Value := 1 ;
end ;


procedure TDOS11_Tape_Form.FF_ButtonClick(Sender: TObject) ;

begin
    Get_Record( Tape.Record_Count ) ;
    Record_Selector.Value := Tape.Record_Count ;
end ;


procedure TDOS11_Tape_Form.Copy_Files_ButtonClick(Sender: TObject);

begin
    Copy_Tape_DOS11_Form.Media := Tape ;
    Copy_Tape_DOS11_Form.ShowModal ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
    Record_Selector.MaxValue := Tape.Record_Count ;
end ;


procedure TDOS11_Tape_Form.Record_SelectorChange(Sender: TObject) ;

begin
    Get_Record( Record_Selector.Value ) ;
end ;



procedure TDOS11_Tape_Form.Add_Data_ButtonClick(Sender: TObject);

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


procedure TDOS11_Tape_Form.SpeedButton1Click(Sender: TObject);

begin
    Tape.Seek_LEOT ;
    Tape.Add_TM ;
    Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
end ;


procedure TDOS11_Tape_Form.SpeedButton2Click(Sender: TObject);

var L : DOS_Label ;

begin
    if( DOS11_Label_Form.ShowModal = mrOK ) then
    begin
        Tape.Seek_LEOT ;
        Tape.Seek( 1, True, True ) ;
        fillchar( L, sizeof( L ), 0 ) ;
        while( length( DOS11_Label_Form.FileName.Text ) < 6 ) do
        begin
            DOS11_Label_Form.FileName.Text := DOS11_Label_Form.FileName.Text + ' ' ;
        end ;
        L.Name := Rad50( DOS11_Label_Form.FileName.Text ) and $FFFF ;
        L.Name := L.Name or ( Rad50( copy( DOS11_Label_Form.FileName.Text, 4, 3 ) ) shl 16 ) ;
        while( length( DOS11_Label_Form.Ext.Text ) < 3 ) do
        begin
            DOS11_Label_Form.Ext.Text := DOS11_Label_Form.Ext.Text + ' ' ;
        end ;
        L.Typ := Rad50( DOS11_Label_Form.Ext.Text ) ;
        L.Prog := DOS11_Label_Form.Prog.Value ;
        L.Proj := DOS11_Label_Form.Proj.Value ;
        L.Prot := DOS11_Label_Form.Prot.Value ;
        L.Date := DOS11_Label_Form.Date_Value.Value ;
        Tape.Write_Record( @L, 14 ) ;
        Tape.Add_TM ;
        Current_Record_Label.Caption := ' of ' + inttostr( Tape.Record_Count ) ;
    end ;
end ;


end.
