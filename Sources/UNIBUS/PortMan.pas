{
        Program Name : Portman
        Package Name : CEF
        Purpose      : Port manager
        Institution  : Conroy & Conroy Co.
        Date Written :
        Written By   : Alan Conroy
        Version      : 1.0

        Copyright (C) 2015 by Alan Conroy.  Released to the public domain.

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

        TPort_Manager is a generic manager of I/O ports.  It maps an I/O
        component with a terminal and a name.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit PortMan ;

interface

uses // Borland...
     Classes, // TStringList

     // CEF...
     _CEF ; // TComponent

type TPort_Manager = class
                         private // Instance data...
                             _Ports : TStringList ;
                             _Terminals : TList ;
                             Temp_Name : string ;

                         private // Utility methods...
                             function Ports : TStringList ;
                             function Terminals : TList ;

                         public // API...
                             // Returns count of managed ports
                             function Count : longint ;

                             // Add a new port named Name, with the specific component.  Returns index of port.
                             function Add_Port( Name : Pchar ; Component : TComponent ) : longint ;

                             // Change the port device...
                             procedure Set_Port( Index : longint ;
                                 Component : TComponent ) ;

                             // Associate passed terminal with the specified port.
                             procedure Set_Terminal( Index : longint ;
                                 Component : TComponent ) ;

                             // Remove port.
                             procedure Delete_Port( Index : longint ) ;

                             // Add a new port named Name, with the specific component.  Returns index of port.
                             procedure Insert_Port( Index : longint ;
                                 Name : Pchar ; Component : TComponent ) ;

                             // Return index of first port with passed name. -1 is returned if not found
                             function Index_Of_Name( Name : PChar ) : longint ;

                             // Return index of port with passed component.  -1 is returned if not found
                             function Index_Of_Component( Component : TComponent ) : longint ;

                             // Return index of port with passed terminal connection. Returns -1 if not found.
                             function Index_Of_Terminal( Terminal : TComponent ) : longint ;

                             // Remove specified component (terminal or I/O device)
                             procedure Remove_Component( Component : TComponent ) ;

                             // Return Name of port at index
                             function Port_Name( Index : longint ) : PChar ;

                             // Return I/O device of port at index
                             function Port_Device( Index : longint ) : TComponent ;

                             // Return terminal of port at index
                             function Port_Connection( Index : longint ) : TComponent ;
                     end ; // TPort_Manager

implementation

// TPort_Manager...

// Utility methods...

function TPort_Manager.Ports : TStringList ;

begin
    if( _Ports = nil ) then
    begin
        _Ports := TStringList.Create ;
        _Terminals := TList.Create ;
    end ;
    Result := _Ports ;
end ;


function TPort_Manager.Terminals : TList ;

begin
    Ports ; // Make sure things are constructed
    Result := _Terminals ;
end ;


// API...

function TPort_Manager.Count : longint ;

begin
    Result := Ports.Count ;
end ;


function TPort_Manager.Add_Port( Name : Pchar ; Component : _CEF.TComponent ) : longint ;

begin
    Result := Ports.Count ;
    _Ports.AddObject( Name, Component ) ;
    _Terminals.Add( nil ) ;
end ;


procedure TPort_Manager.Set_Port( Index : longint ; Component : _CEF.TComponent ) ;

begin
    if( ( Index < 0 ) or ( Index >= Ports.Count ) ) then
    begin
        exit ; // Out of range
    end ;
    Ports.Objects[ Index ] := Component ;
end ;


procedure TPort_Manager.Set_Terminal( Index : longint ; Component : _CEF.TComponent ) ;

begin
    if( ( Index < 0 ) or ( Index >= Ports.Count ) ) then
    begin
        exit ; // Out of range
    end ;
    Terminals[ Index ] := Component ;
end ;


procedure TPort_Manager.Delete_Port( Index : longint ) ;

begin
    if( ( Index < 0 ) or ( Index >= Ports.Count ) ) then
    begin
        exit ; // Out of range
    end ;
    Ports.Delete( Index ) ;
    Terminals.Delete( Index ) ;
end ;


procedure TPort_Manager.Insert_Port( Index : longint ;
    Name : Pchar ; Component : TComponent ) ;

begin
    Ports.InsertObject( Index, Name, Component ) ;
    Terminals.Insert( Index, nil ) ;
end ;


function TPort_Manager.Index_Of_Name( Name : PChar ) : longint ;

begin
    Result := Ports.Indexof( Name ) ;
end ;


function TPort_Manager.Index_Of_Component( Component : TComponent ) : longint ;

var Loop : integer ;

begin
    for Loop := 0 to Ports.Count - 1 do
    begin
        if( Ports.Objects[ Loop ] = Component ) then
        begin
            Result := Loop ;
            exit ;
        end ;
    end ;
    Result := -1 ;
end ;


function TPort_Manager.Index_Of_Terminal( Terminal : TComponent ) : longint ;

begin
    Result := Terminals.Indexof( Terminal ) ;
end ;


procedure TPort_Manager.Remove_Component( Component : TComponent ) ;

var Loop : integer ;

begin
    for Loop := 0 to Ports.Count - 1 do
    begin
        if( Ports.Objects[ Loop ] = Component ) then
        begin
            Ports.Objects[ Loop ] := nil ;
        end ;
    end ;
end ;


function TPort_Manager.Port_Name( Index : longint ) : PChar ;

begin
    Result := nil ;
    if( ( Index < 0 ) or ( Index >= Ports.Count ) ) then
    begin
        exit ; // Out of range
    end ;
    Temp_Name := Ports[ Index ] ;
    Result := PChar( Temp_Name ) ;
end ;


function TPort_Manager.Port_Device( Index : longint ) : TComponent ;

begin
    Result := nil ;
    if( ( Index < 0 ) or ( Index >= Ports.Count ) ) then
    begin
        exit ; // Out of range
    end ;
    Result := TComponent( Ports.Objects[ Index ] ) ;
end ;


function TPort_Manager.Port_Connection( Index : longint ) : TComponent ;

begin
    Result := nil ;
    if( ( Index < 0 ) or ( Index >= Ports.Count ) ) then
    begin
        exit ; // Out of range
    end ;
    Result := TComponent( Terminals[ Index ] ) ;
end ;


end.
