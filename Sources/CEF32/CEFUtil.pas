{
              Program Name : CEFUtil
              Package Name : CEF
              Purpose      : CEF utility routines
              Institution  : Conroy & Conroy
              Date Written : 19-Mar-87
              Written By   : Alan Conroy
              Version      : 1.0

	      Released to the public domain.

              TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *********************************************************
        *                                                       *
        *        M O D I F I C A T I O N   H I S T O R Y        *
        *                                                       *
        *     DATE      BY             REASON                   *
        *                                                       *
        *********************************************************

        *********************************************************
        *                                                       *
        *           P R O G R A M   P U R P O S E               *
        *                                                       *
        *********************************************************

          This unit defines utility functions for CEF32.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit CEFUtil ;

interface

function Number_Format( Base, Size : integer ; Value : String ) : String ;

implementation

function Number_Format( Base, Size : integer ; Value : String ) : String ;
{ Return a number with proper number of leading zeroes.  Base is base to format
  in, Size is number of bits, and Value is the number to format. }

var A : Integer ;

begin
    // Determine number of digits
    if( Base = 2 ) then
    begin
        A := Size ;
    end else
    if( Base = 8 ) then
    begin
        A := ( Size + 2 ) div 3 ;
    end else
    begin
        A := ( Size + 3 ) div 4 ;
    end ;
    if( Base = 10 ) then
    begin
        A := A + 1 ;
    end ;
    while( Length( Value ) < A ) do
    begin
        Value := '0' + Value ; { Pad with zeroes }
    end ;
    Number_Format := Value ; { Return result }
end ; { Numer_Format }


end.
