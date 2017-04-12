program compare;

uses Sysutils,
     GetChars ;

function Alter( S : string ) : string ;

var Dummy : integer ;
    Work : string ;

begin
    Dummy := pos( 'write ', S ) ;
    if( Dummy = 0 ) then
    begin
        Result := S ;
        exit ;
    end ;
    Work := copy( S, Dummy + 6, length( S ) ) ;
    S := copy( S, 1, Dummy + 5 ) ;
    Dummy := strtoint( Work ) and $FFFF ;
    Result := S + inttostr( Dummy ) ;
end ;


var FS, FC : textfile ;
    Dummy, Dummy1, Count, LS, LC : integer ;
    S, SS, SC, SS1, SC1, Work : string ;

label Again, Again1 ;

begin
    LS := 0 ;
    LC := 0 ;
    assignfile( FS, 'c:\simh pdp11.log' ) ;
    reset( FS ) ;
    assignfile( FC, 'c:\cef pdp11.log' ) ;
    reset( FC ) ;
    try
        while( not ( eof( FS ) or eof( FC ) ) ) do
        begin
            readln( FS, SS ) ;
            readln( FC, SC ) ;
            inc( LC ) ;
            inc( LS ) ;
Again1:
            if( ( copy( SC, 1, 1 ) <> '*' ) and ( copy( SC, 1, 1 ) <> '-' ) ) then
            begin
                Dummy := 1 ;
                Count := 0 ;
                while( Dummy <= length( SC ) ) do
                begin
                    if( SC[ Dummy ] = ' ' ) then
                    begin
                        inc( Count ) ;
                        if( Count = 10 ) then
                        begin
                            break ;
                        end ;
                    end ;
                    inc( Dummy ) ;
                end ;
                SC := copy( SC, 1, Dummy - 1 ) ;
            end ;
            if( copy( SC, 1, 1 ) = '-' ) then
            begin
                Dummy := pos( ' (', SC + ' (' ) ;
                SC := copy( SC, 1, Dummy - 1 ) ;
                SC := Alter( SC ) ;
            end ;

            if( ( copy( SS, 1, 1 ) = '-' ) and ( copy( SS, 4, 1 ) <> 'R' ) ) then
            begin
                Work := copy( SS, 4, length( SS ) ) ;
                Dummy := pos( ' ', Work ) ;
                Work := copy( Work, 1, Dummy - 1 ) ;
                Dummy1 := strtoint( Work ) ;
                if( Dummy1 > 65535 ) then
                begin
                    Dummy1 := Dummy1 and $FFFF ;
                    SS := '-> ' + inttostr( Dummy1 ) + copy( SS, Dummy + 3, length( SS ) ) ;
                end ;
            end ;
Again:
            if( SC <> SS ) then
            begin
                writeln( '1) ', LS, ': ', SS ) ;
                writeln( '2) ', LC, ': ', SC ) ;
                if( ( pos( 'CSR', SS ) > 0 ) and ( pos( 'CSR', SC ) > 0 ) ) then
                begin // Timing difference
                    while( SS <> SC ) do
                    begin
                        SS1 := copy( SS, 9, length( SS ) ) ;
                        SC1 := copy( SC, 9, length( SC ) ) ;
                        Dummy := pos( ' ', SS1 ) ;
                        SS1 := copy( SS1, Dummy + 1, length( SS1 ) ) ;
                        Dummy := pos( ' ', SS1 ) ;
                        SS1 := copy( SS1, 1, Dummy - 1 ) ;
                        Dummy1 := strtoint( SS1 ) ;

                        Dummy := pos( ' ', SC1 ) ;
                        SC1 := copy( SC1, Dummy + 1, length( SC1 ) ) ;
                        Dummy := pos( ' ', SC1 ) ;
                        SC1 := copy( SC1, 1, Dummy - 1 ) ;
                        Dummy := strtoint( SC1 ) ;
                        if( ( Dummy and 128 ) = ( Dummy1 and 128 ) ) then
                        begin
                            break ; // Some other problem than timing
                        end ;
                        if( ( Dummy and 128 ) = 0 ) then // SC is slower
                        begin
                            SC := '' ;
                            while( pos( 'CSR', SC ) = 0 ) do
                            begin
                                readln( FC, SC ) ;
                                inc( LC ) ;
                            end ;
                        end else
                        begin
                            SS := '' ;
                            while( pos( 'CSR', SS ) = 0 ) do
                            begin
                                readln( FS, SS ) ;
                                inc( LS ) ;
                            end ;
                        end ;
                    end ;
                    if( SS = SC ) then // Resynced
                    begin
                        goto Again ;
                    end ;
                end ;
                write ( '> ' ) ;
                S := Default_Input.Inkey( 0 ) ;
                writeln( '' ) ;
                if( ( S = 'b' ) or ( S = 'B' ) ) then
                begin
                    continue ;
                end ;
                if( S = '1' ) then
                begin
                    SS := '*' ;
                    while( ( copy( SS, 1, 1 ) = '*' ) or ( copy( SS, 1, 1 ) = '-' ) ) do
                    begin
                        readln( FS, SS ) ;
                        inc( LS ) ;
                        if( SS = SC ) then
                        begin
                            break ;
                        end ;
                    end ;
                    goto Again ;
                end ;
                if( S = '2' ) then
                begin
                    SC := '*' ;
                    while( ( copy( SC, 1, 1 ) = '*' ) or ( copy( SC, 1, 1 ) = '-' ) ) do
                    begin
                        readln( FC, SC ) ;
                        inc( LC ) ;
                        if( SS = SC ) then
                        begin
                            break ;
                        end ;
                    end ;
                    goto Again1 ;
                end ;
                if( ( S = 'G' ) or ( S = 'g' ) ) then
                begin
                    write( 'New line for file 1: ' ) ;
                    readln( Dummy ) ;
                    while( LS < Dummy ) do
                    begin
                        readln( FS, SS ) ;
                        inc( LS ) ;
                    end ;
                    write( 'New line for file 2: ' ) ;
                    readln( Dummy ) ;
                    while( LC < Dummy ) do
                    begin
                        readln( FC, SC ) ;
                        inc( LC ) ;
                    end ;
                    goto Again1 ;
                end ;
            end ; // if( SC <> SS )
        end ; // while( not ( eof( FS ) or eof( FC ) ) )
    finally
        closefile( FS ) ;
        closefile( FC ) ;
    end ;
end.
