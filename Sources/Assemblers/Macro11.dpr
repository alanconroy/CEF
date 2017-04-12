{
        Program Name : Macro11
        Package Name : CEF32
        Purpose      : PDP-11 assembler for Windows
        Institution  : Conroy & Conroy Co.
        Date Written : 27-January-2015
        Written By   : Alan Conroy
        Version      : 1.0

	    Copyright (C) 2015 by Alan Conroy.  All rights reserved.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

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

}

program Macro11 ;

uses // Borland...
     _Streams, // TCOM_Stream
     Sysutils,
     _UEHDefs, // UEC

     // C&C...
     ASCIIDef, // FF
     CommonUt, // Edit
     Dates, // Date
     Parse, // Parse_Switch
     Standard,
     UStrings, // Substitute

     // CEF...
     CEF, // ASF_Want_XRef
     CEFUtil_Int, // Create_File_Stream

     // Macro11...
     Macro_11 ; // Assemble

const Version = 'V2.6'; {Current version}
      Program_Name = 'MACRO11'; {Name of program}

label Skip, Main_Prompt, Process_Next_Command ;

var CCL : boolean ; { If a command-line entry }
    Command : String ; { Input or command line }
    Dummy : integer ;
    EC : integer ; { Exit code }
    End_Of_File : boolean ;
    F : file ; { Used to check attributes on file }
    Filename : Text ;
    Header : Boolean ; { True if file header already printed }
    ICF : text ;
    Initial_SP, Flags, JSW, JSX : integer ;
    Line_Count : integer ;
    Output_File : text ;
    Page_Width : integer = 70 ;
    Paging : boolean ; { True if paging }
    Path : String ;
    Sav_File : boolean ;
    Screen_Size : integer ;
    Temp : string ;
    Use_ICF : boolean ;
    Use_Output_File : boolean ;
    Wildcards : boolean ;


function Parse_Switches( var Command : string ) : boolean ;

var Temp : string ;

begin
    Parse_Switches := False ;
    JSW := 0 ;
    JSX := 0 ;
    Initial_SP := 1024 ;
    Flags := 0 ;
    Sav_File := False ;

    if( Parse_Switch( 'GTLIN', '', Command, Temp ) = 1 ) then
    begin
        JSW := 8 ;
    end ;
    if( Parse_Switch( 'EDIT', 'NOEDIT', Command, Temp ) = 2 ) then
    begin
        JSW := JSW or 16 ;
    end ;
    if( Parse_Switch( 'SPXIT', '', Command, Temp ) = 1 ) then
    begin
        JSW := JSW or 32 ;
    end ;
    if( Parse_Switch( 'TCBIT', '', Command, Temp ) = 1 ) then
    begin
        JSW := JSW or 64 ;
    end ;
    if( Parse_Switch( 'CHAIN', '', Command, Temp ) = 1 ) then
    begin
        JSW := JSW or 256 ;
    end ;
    if( Parse_Switch( 'OVERLAY', '', Command, Temp ) = 1 ) then
    begin
        JSW := JSW or 512 ;
    end ;
    if( Parse_Switch( 'CHNIF', '', Command, Temp ) = 1 ) then
    begin
        JSW := JSW or 2048 ;
    end ;
    if( Parse_Switch( 'SPC', '', Command, Temp ) = 1 ) then
    begin
        JSW := JSW or 4096 ;
    end ;
    if( Parse_Switch( 'REENTER', '', Command, Temp ) = 1 ) then
    begin
        JSW := JSW or 8192 ;
    end ;
    if( Parse_Switch( 'LOWERCASE', '', Command, Temp ) = 1 ) then
    begin
        JSW := JSW or 16384 ;
    end ;

    if( Parse_Switch( 'REQID', '', Command, Temp ) = 1 ) then
    begin
        JSX := 1 ;
    end ;
    if( Parse_Switch( 'USEID', '', Command, Temp ) = 1 ) then
    begin
        JSX := JSX or 2 ;
    end ;
    if( Parse_Switch( 'REQSM', '', Command, Temp ) = 1 ) then
    begin
        JSX := JSX or 4 ;
    end ;
    if( Parse_Switch( 'USESM', '', Command, Temp ) = 1 ) then
    begin
        JSX := JSX or 8 ;
    end ;
    if( Parse_Switch( 'ALL64', '', Command, Temp ) = 1 ) then
    begin
        JSX := JSX or 16 ;
    end ;
    if( Parse_Switch( 'IOPAGE', '', Command, Temp ) = 1 ) then
    begin
        JSX := JSX or 32 ;
    end ;
    case Parse_Switch( 'VBG', '', Command, Temp ) of
        1 : JSX := JSX or 32 ;
        2 : JSX := JSX or 64 ;
    end ;

    if( Parse_Switch( 'SP', '', Command, Temp ) = 1 ) then
    begin
        if( Temp <> '' ) then
        begin
            try
                Initial_SP := strtoint( Temp ) ;
            except
                writeln( 'Invalid initial stack position' ) ;
                Parse_Switches := True ;
            end ;
        end ;
    end ;

    if( Parse_Switch( 'XREF', '', Command, Temp ) = 1 ) then
    begin
        Flags := ASF_Want_XRef ;
    end ;
    if( Parse_Switch( 'CROSSREF', '', Command, Temp ) = 1 ) then
    begin
        Flags := ASF_Want_XRef ;
    end ;
    if( Parse_Switch( 'SYMBOLS', '', Command, Temp ) = 1 ) then
    begin
        Flags := ASF_Want_Symbol_Table ;
    end ;

    if( Parse_Switch( 'PAGEWIDTH', '', Command, Temp ) = 1 ) then
    begin
        if( Temp <> '' ) then
        begin
            try
                Page_Width := strtoint( Temp ) ;
            except
                writeln( 'Invalid page width' ) ;
                Parse_Switches := True ;
            end ;
        end ;
    end ;
    if( Parse_Switch( 'SAV', '', Command, Temp ) = 1 ) then
    begin
        Sav_File := True ;
    end ;
end ; // Parse_Switches


procedure Close_Indirect ;

var Dummy : integer ;

begin
    {$I-}
    close( ICF ) ;
    {$I+}
    Dummy := IOResult ;
    Use_ICF := False ;
end ;


procedure Show_Error( S : string ) ;

begin
    writeln( S ) ;
    Close_Indirect ;
end ;


var Listing : TCOM_Stream ;
    
type TError_Information = class
                              public
                                  Line : longint ;
                                  Severity : longint ;
                                  Filename : string ;
                          end ;

type TStatus = class( TAssembler_Status )
                   private // Instance data...
                       _Code : longint ;
                       _Data : longint ;
                       _Errors : longint ;
                       _Warnings : longint ;
                       _Line : longint ;
                       Output_Line : longint ;
                       Page : longint ;
                       Page_Line : longint ;

                       Data_String : string ;
                       File_Name : string ;
                       Title : string ;
                       Subtitle : string ;

                   public // API
                       function Filename : PChar ;
                           override ;

                       function Get_Aborted : boolean ;
                           override ;
                       function Get_Code : int64 ;
                           override ;
                       function Get_Data : int64 ;
                           override ;
                       function Get_Errors : longint ;
                           override ;
                       function Get_Warnings : longint ;
                           override ;
                       function Get_Error_Text : PChar ;
                           override ;

                       procedure Set_Aborted( Value : boolean ) ;
                           override ;
                       procedure Set_Code( Value : int64 ) ;
                           override ;
                       procedure Set_Data( Value : int64 ) ;
                           override ;
                       procedure Set_Errors( Value : longint ) ;
                           override ;
                       procedure Set_Warnings( Value : longint ) ;
                           override ;
                       procedure Set_Error_Text( Value : PChar ) ;
                           override ;

                       procedure Get_Error( Index : longint ;
                           var Text, Filename : PChar ;
                           var Line, Severity : longint ) ;
                           override ;

                       procedure Log_Error( Text, Filename : PChar ;
                           Line, Severity : longint ) ;
                           override ;

                       procedure Set_Line( Value : longint ) ;
                           override ;

                       procedure Output_To_Listing( Text : PChar ;
                           Text_Type : integer ) ;
                           override ;
               end ;

               
function TStatus.Filename : PChar ;

begin
    Result := PChar( File_Name ) ;
end ;


function TStatus.Get_Aborted : boolean ;

begin
    Result := False ;
end ;


function TStatus.Get_Code : int64 ;

begin
    Result := _Code ;
end ;


function TStatus.Get_Data : int64 ;

begin
    Result := _Data ;
end ;


function TStatus.Get_Errors : longint ;

begin
    Result := _Errors ;
end ;


function TStatus.Get_Warnings : longint ;

begin
    Result := _Warnings ;
end ;


function TStatus.Get_Error_Text : PChar ;

begin
    Result := nil ;
end ;


procedure TStatus.Set_Aborted( Value : boolean ) ;

begin
    // This routine intentionally left blank
end ;


procedure TStatus.Set_Code( Value : int64 ) ;

begin
    _Code := Value ;
end ;


procedure TStatus.Set_Data( Value : int64 ) ;

begin
    _Data := Value ;
end ;


procedure TStatus.Set_Errors( Value : longint ) ;

begin
    _Errors := Value ;
end ;


procedure TStatus.Set_Warnings( Value : longint ) ;

begin
    _Warnings := Value ;
end ;


procedure TStatus.Set_Error_Text( Value : PChar ) ;

begin
    // This routine intentionally left blank
end ;


procedure TStatus.Get_Error( Index : longint ;
   var Text, Filename : PChar ; var Line, Severity : longint ) ;

begin
    // This routine intentionally left blank
end ;


procedure TStatus.Log_Error( Text, Filename : PChar ;
   Line, Severity : longint ) ;

var P : TError_Information ;
    S : string ;

begin
    P := TError_Information.Create ;
    if( Line = -1 ) then
    begin
        P.Line := _Line ;
    end else
    begin
        P.Line := Line ;
    end ;
    if( Filename = nil ) then
    begin
        P.Filename := File_Name ;
    end else
    begin
        P.Filename := string( Filename ) ;
    end ;
    P.Severity := Severity ;
    case Severity of
        Severity_Warning :
            begin
                inc( _Warnings ) ;
                S := '[WARNING] ' ;
            end ;
        Severity_Error, Severity_Fatal :
            begin
                inc( _Errors ) ;
                S := '[ERROR] ' ;
            end ;
        else S := '[HINT] ' ;
    end ;
    S := S + P.Filename ;
    if( P.Line > 0 ) then
    begin
        S := S + '(' + inttostr( P.Line ) + ')' ;
    end ;
    Listing.Write_Line( PChar( S + ' ' + string( Text ) ) ) ;
end ;


procedure TStatus.Set_Line( Value : longint ) ;

begin
    _Line := Value ;
end ;


procedure TStatus.Output_To_Listing( Text : PChar ; Text_Type : integer ) ;

    procedure Check_Heading ;

    var P : PChar ;
        S : string ;

    begin
        if( Page_Line = 0 ) then
        begin
            Listing.Write_Line( '' ) ;

            S := Title + '  MACRO V' + Version + ' ' + Day( Date ) + ' ' + Date + ' ' + Time ;
            Listing.Write_Line( PChar( S ) ) ;
            Listing.Write_Line( PChar( Subtitle ) ) ;

            Listing.Write_Line( '' ) ;
            S := inttostr( Output_Line ) + ' ' ;
            while( length( S ) < 5 ) do
            begin
                S := ' ' + S ;
            end ;
            P := PChar( S ) ;
            Listing.Write( P, length( S ) ) ;

            Page_Line := 3 ;
        end ;
    end ;


    procedure New_Page ;

    var C : char ;
        P : PChar ;
        S : string ;

    begin
        if( not Paging ) then
        begin
            exit ;
        end ;
        inc( Page ) ;
        Page_Line := 0 ; // Force heading
        C := FF ;
        P := PChar( @C ) ;
        Listing.Write( P, 1 ) ;
        S := inttostr( Output_Line ) + ' ' ;
        while( length( S ) < 5 ) do
        begin
            S := ' ' + S ;
        end ;
        P := PChar( S ) ;
        Listing.Write( P, length( S ) ) ;
        Check_Heading ;
    end ;

var P : Pchar ;
    S : string ;

begin
    if( Listing = nil ) then
    begin
        exit ;
    end ;

    case Text_Type of
        ListOut_Title_Text: Title := string( Text ) ;
        ListOut_New_Line:
            begin
                Check_Heading ;
                Listing.Write_Line( '' ) ;
                S := inttostr( Output_Line ) + ' ' ;
                while( length( S ) < 5 ) do
                begin
                    S := ' ' + S ;
                end ;
                P := PChar( S ) ;
                Listing.Write( P, length( S ) ) ;
                inc( Page_Line ) ;
                if( Page_Line > 63 ) then
                begin
                    New_Page ;
                end ;
            end ;
        ListOut_New_Page:
            begin
                New_Page ;
            end ;
        ListOut_Generated_Data:
            begin
                Data_String := Data_String + string( Text ) ;
            end ;
        ListOut_Paging: Paging := True ;
        ListOut_No_Paging: Paging := False ;
        ListOut_SubTitle: Subtitle := string( Text ) ;
        else
            begin
                Check_Heading ;
                while( length( Data_String ) < 22 ) do
                begin
                    Data_String := Data_String + ' ' ;
                end ;
                Data_String := Data_String + string( Text ) ;
                P := PChar( Data_String ) ;
                Listing.Write( P, length( Data_String ) ) ;
                S := inttostr( Output_Line ) + ' ' ;
                while( length( S ) < 5 ) do
                begin
                    S := ' ' + S ;
                end ;
                P := PChar( S ) ;
                Listing.Write( P, length( S ) ) ;
                Data_String := '' ;
            end ;
    end ;
end ;


var A_S : TStatus ;
    List, Source, Target : string ;
    S : string ;
    UEC : TUEC ;

begin
    { Setup... }
    Exitsave := Exitproc ; { Set up error handler }
    Exitproc := @Error ;
    Screen_Size := 24 ;
    Use_Output_File := False ;
    Use_ICF := False ;
{$IFDEF MSDOS}
    Reg.AX := $1B00 ;
    Reg.BX := 0 ;
    Reg.ES := seg( Info ) ;
    Reg.DI := ofs( Info ) ;
    Intr( $10, Reg ) ;
    if( Reg.AL = $1B ) then
    begin
	    Screen_Size := Info[ $22 ] ;
    end ;
{$ENDIF}

    { Handle command line and program header... }
    CCL := False ; { Default to no command line }
    if( ParamCount > 0 ) then
    begin
	    CCL := True ; { If a command line entry }
    end ;
    Command := edit( Parse_Command_Line, 32 or 256 ) ; { Get input }
    if( CCL = False ) then { No command line }
    begin
        writeln( Program_Name, ' ', Version, '  ', OS_ID, '  ', Date, ' at ', Time ) ;
        writeln ;
    end else
    begin
	    goto Skip ;
    end ;
Main_Prompt:
    if( Use_ICF ) then
    begin
        if( eof( ICF ) ) then
        begin
            Close_Indirect ;
        end else
        begin
            {$I-}
            readln( ICF, Command ) ;
            {$I+}
            Dummy := IOResult ;
            if( Dummy <> 0 ) then
            begin
                Close_Indirect ;
            end else
            begin
                goto Process_Next_Command ;
            end ;
        end ;
    end ;
    if( Use_Output_File ) then
    begin
        {$I-}
        close( Output_File ) ;
        {$I+}
        Dummy := IOResult ;
        Use_Output_File := False ;
    end ;
    if( CCL ) then
    begin
	    halt ;
    end ;
    write( Program_Name, '> ' ) ;
    readln( Command ) ;
Process_Next_Command:
    Command := edit( Command, -1 ) ;
    if( length( Command ) = 0 ) then
    begin
	    goto Main_Prompt ;
    end ;
    if( Parse_Switch( 'E|XIT', '', Command, Temp ) = 1 ) then
    begin
        if( length( Temp ) <> 0 ) then
        begin
            val( Temp, EC, Dummy ) ;
            if( Dummy = 0 ) then
            begin
                if( Use_ICF ) then
                begin
                    Close_Indirect ;
                    goto Main_Prompt ;
                end ;
                halt( EC ) ;
            end ;
            Show_Error( 'Invalid parameter on /EXIT: ' + Temp ) ;
            goto Main_Prompt ;
        end ;
        if( Use_ICF ) then
        begin
            Close_Indirect ;
            goto Main_Prompt ;
        end ;
	    halt ;
    end ;

Skip: { Process command }
    { Parse switches... }
    if( Parse_Switch( '?', '', Command, Temp ) = 1 ) then
    begin
	    writeln( 'Command format:' ) ;
	    writeln( '' ) ;
	    writeln( Program_Name + ' {outfile{,listfile}=}infile {switches}' ) ;
	    goto Main_Prompt ;
    end ;
    if( Parse_Switch( 'H|ELP', '', Command, Temp ) = 1 ) then
    begin
{	Help( Program_Base_Name, Temp ) ;}
	    goto Main_Prompt ;
    end ;

    { Set defaults... }
    Paging := False ;

    { Process switches... }
    if( Parse_Switches( Command ) ) then
    begin
	    goto Main_Prompt ;
    end ;
    if( Switch_Present( Command ) > 0 ) then
    begin
	    Show_Error( 'Unknown switch: ' + Command ) ;
	    exit ;
    end ;

    Target := Parse_Parameter( '=', Command ) ; { Filename }
    if( length( Target ) = 0 ) then
    begin
        Show_Error( 'No file specified' ) ;
	    goto Main_Prompt ;
    end ;
    if( length( Command ) = 0 ) then // Only source was provided
    begin
        Command := Target ;
        Dummy := Extension_Pos( Target ) ;
        Target := copy( Target, 1, Dummy - 1 ) ;
    end ;
    if( pos( ',', Target ) = 0 ) then
    begin
        Listing := nil ;
    end else
    begin
        List := Target ;
        Target := Parse_Parameter( ',', List ) ;
        if( Target <> '' ) then
        begin
            Target := Command ;
            Dummy := Extension_Pos( Target ) ;
            Target := copy( Target, 1, Dummy - 1 ) ;
        end ;
        Listing := Create_File_Stream( List, UEC )  ;
    end ;
    if( Target <> '' ) then
    begin
        if( Extension_Pos( Target ) = 0 ) then
        begin
            if( Sav_File ) then
            begin
                Target := Target + '.SAV' ;
            end else
            begin
                Target := Target + '.OBJ' ;
            end ;
        end ;
    end ;

    { Do the work... }
    Line_Count := 0 ;
    A_S := TStatus.Create ;
    A_S.File_Name := Command ;
    S := Command ;
    Dummy := Extension_Pos( S ) ;
    if( Dummy > 0 ) then
    begin
        S := copy( S, 1, Dummy - 1 ) ;
    end ;
    Dummy := length( S ) ;
    while( ( Dummy > 0 ) and ( S[ Dummy ] <> '\' ) and ( S[ Dummy ] <> ':' ) ) do
    begin
        dec( Dummy ) ;
    end ;
    S := copy( S, Dummy + 1, length( S ) ) ;
    A_S.Title := S ;
    A_S.Output_Line := 1 ;
    Assemble( Command, Target, Flags, JSX, Initial_SP, JSW, A_S, Listing, Sav_File, copy( S, 1, 6 ) ) ;
    A_S.Free ;
    goto Main_Prompt ;
end.
