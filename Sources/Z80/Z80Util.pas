{
              Program Name : Z80Util
              Package Name : Z80
              Purpose      : 8080/8085/Z80 emulator utility routines
              Institution  : Conroy & Conroy
              Date Written : 19-Mar-87
              Written By   : Alan Conroy
              Version      : 1.A

              Copyright (C) 1987 by Alan Conroy.  Released to the public domain.
              
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

          This unit defines utility functions for the Z80CPU CEF component.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Z80Util ;

interface

function Cvtif( Value : Integer ) : Real ; {Convert integer to real}
function Cvtfi( Value : Real ) : Integer ; {Convert floating (real) to integer}
//function CvtS( Input : String ) : String ; {Return value for Input}
//function Cvtfs( Value : Real ) : String ; {Convert floating to string}
function Cvt1( Value : String ) : Real ; {Convert binary string to real}
function Number_Format( Base, Size : integer ; Value : String ) : String ;
function Valid_Identifier( X : string ) : boolean ;
{ Return true if valid identifier }

implementation

uses Instrs, CVT, Maths ;

function Number_Format( Base, Size : integer ; Value : String ) : String ;
{ Return a number with proper number of leading zeroes }

var A : Integer ;

begin
    A := Size * 8 ;
    if( Base = 8 ) then
    begin
        A := A div 3 + 1 ;
    end else
    begin
        A := A div 4 ; { Determine number of digits }
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


function Cvtif( Value : Integer ) : Real ; { Convert integer to real }

begin
    if( Value < 0 ) then
    begin
        Cvtif := Value + 65536.0 ;
    end else
    begin
        Cvtif := Value ;
    end ;
end ;


function Cvtfi( Value : Real ) : Integer ; {Convert floating (real) to integer}

var A : Integer ; {Accumulation}

begin
    if Value = 32768. then
    begin
        A := 32767 ;
        A := A + 1
    end else
        if Value > 32768.0 then A := Trunc( Value - 65536.0 ) else A := Trunc( Value ) ;
    Cvtfi := A {Return value}
end ;


(*
function Cvtfs( Value : Real ) : String ; { Convert floating to string }

label A ;

var B, Bbb : Real ;
    C : String ;
    D, Dummy : Integer ;

begin
    C := '' ;
    B := Value ;
    D := ( Size - 1 ) * 8 ; { Maximum bit number }
A:
    Bbb := Int( B / Power( 2, D, Dummy ) ) ; { Divide by number of bytes }
    C := C + Chr( Trunc( Bbb ) ) ; { Build string }
    B := B - Bbb * Power( 2, D, Dummy ) ; { Subtract value }
    D := D - 8 ;
    if( D >= 0 ) then
    begin
        goto A ; { Loop until done }
    end ;
    Cvtfs := C ; { Return value }
end ;
*)


function Cvt1( Value : String ) : Real ; { Convert binary string to real }

var A : Integer ; { Loop index }
    B : Real ; { Accumulation }
    Dummy : integer ;

begin
    B := 0 ;
    for A := Length( Value ) downto 1 do
    begin
        B := B + Ord( Value[ A ] ) * Power( 2, ( Length( Value ) - A ) * 8, Dummy ) ;
        { Accumulate }
    end ;
    Cvt1 := B ; { Return result }
end ;


(*
function CvtS( Input : String ) : String ; { Return value for Input }

var A : Integer ;

begin
    A := Instr( 1, Input, '.' ) ;
    if A = 0 then
    begin
	Input := Cvtb( Base, 10, Input ) ;
    end else
    begin
	Input := copy( Input, 1, A - 1 ) ; { Convert if not decimal }
    end ;
    if( Length( Input ) = 0 ) then
    begin
        Input := '0' ;
    end ;
    CvtS := Input ; { Return value }
end ; { CVTS }
*)


function Valid_Identifier( X : string ) : boolean ;
{ Return true if valid identifier }

var Loop : integer ;

begin
    Valid_Identifier := True ; { Assume it is good }
    if( ( length( X ) = 0 ) or ( pos( X[ 1 ], '1234567890' ) <> 0 ) ) then
    begin
	Valid_Identifier := False ; { Must start with non-digit and be non-null }
	exit ;
    end ;
    for Loop := 1 to length( X ) do
    begin
	if( pos( X[ Loop ], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$.' ) = 0 ) then
	begin
	    Valid_Identifier := False ;
	    exit ;
	end ;
    end ;
end ;



end.
