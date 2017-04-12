program cvtrom ;

uses
  sysutils,
  cvt;

var b : byte ;
    dummy : integer ;
    f : textfile ;
    fo : file ;
    s, s1 : string ;

begin
    assignfile( F, 'F:\Emulators\Solace\solos_rom_cpm.hex.txt' ) ;
    reset(f);
    assignfile(fo, 'f:\work\cef\solos_cpm.rom' );
    rewrite( fo, 1 );
    while( not eof( f ) ) do
    begin
        readln( f, s ) ;
        while( length( s ) > 0 ) do
        begin
            dummy := pos( ',', s ) ;
            s1 := copy( s, 1, dummy - 1 ) ;
            s := copy( s, dummy + 1, length( S ) ) ;
            b := strtoint( cvtb( 16, 10, copy( s1, 4, 2 ) ) ) ;
            blockwrite( fo, b, 1);
        end ;
    end ;
    closefile(fo);
    closefile(f);
end.
