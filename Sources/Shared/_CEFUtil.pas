{$N+}
{
        Program Name : CEFUtil
        Package Name : CEF
        Purpose      : CEF_Util definitions
        Institution  :
        Date Written : 2-Jan-2007
        Written By   : Alan Conroy
        Version      : 1.0

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.
        
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

          This unit contains all of the type definitions for the CEF_Util DLL.

}

unit _CEFUtil ;

interface

uses // C&C...
     _UE ; // TUnified_Exception

type TCEF_Watchpoint = class
                          public { API... }
                              function Get_Address : int64 ;
                                  virtual ; stdcall ; abstract ;

                              function Get_Access : longint ;
                                  virtual ; stdcall ; abstract ;

                              procedure Set_Access( Value : longint ) ;
                                  virtual ; stdcall ; abstract ;


                              function Serialize : PChar ;
                                  virtual ; stdcall ; abstract ;

                              procedure Deserialize( Image : PChar ) ;
                                  virtual ; stdcall ; abstract ;

                              // The following are supported in CEF V2.5 and later:

                              function Get_Size : longint ;
                                  virtual ; stdcall ; abstract ;

                              property Address : int64
                                  read Get_Address ;
                              property Access : longint
                                  read Get_Access
                                  write Set_Access ;
                              property Size : longint
                                  read Get_Size ;

                              // The following are supported in CEF V2.6 and later:

                              function Get_Context : pointer ;
                                  virtual ; stdcall ; abstract ;

                              function Get_Domain : Pchar ;
                                  virtual ; stdcall ; abstract ;

                              procedure Set_Context( Value : pointer ) ;
                                  virtual ; stdcall ; abstract ;

                              procedure Set_Domain( Value : PChar ) ;
                                  virtual ; stdcall ; abstract ;

                              property Context : pointer
                                  read Get_Context
                                  write Set_Context ;

                              property Domain : PChar
                                  read Get_Domain
                                  write Set_Domain ;
                      end ; // TCEF_Watchpoint

    TCEF_Watchpoint_Manager = class
                                  public // API...
                                      procedure Clear ;
                                          virtual ; stdcall ; abstract ;

                                      function Clear_Watchpoint( Address : int64 ;
                                          Access : longint ) : TUnified_Exception ;
                                          virtual ; stdcall ; abstract ;
                                      function Create_Watchpoint( Ad : int64 ;
                                          Ac : longint ) : TCEF_Watchpoint ; overload ;
                                          virtual ; stdcall ; abstract ;
                                      function Count : longint ;
                                          virtual ; stdcall ; abstract ;
                                      procedure Deserialize( Source : PChar ) ;
                                          virtual ; stdcall ; abstract ;
                                      function Serialize : PChar ;
                                          virtual ; stdcall ; abstract ;
                                      procedure Terminate ;
                                          virtual ; stdcall ; abstract ;
                                      function Watchpoint_At( Address : int64 ) : TCEF_Watchpoint ;
                                          virtual ; stdcall ; abstract ;

                                      // The following are supported in CEF V2.5 and later:

                                      function Version : integer ;
                                          virtual ; stdcall ; abstract ;
                                      procedure Initialize ;
                                          virtual ; stdcall ; abstract ;
                                      function Has_Accesses : boolean ;
                                          virtual ; stdcall ; abstract ;
                                      function Has_Sizes : boolean ;
                                          virtual ; stdcall ; abstract ;
                                      function Add( W : TCEF_Watchpoint ) : integer ;
                                          virtual ; stdcall ; abstract ;
                                      function Create_Watchpoint( Ad : int64 ) : TCEF_Watchpoint ; overload ;
                                          virtual ; stdcall ; abstract ;
                                      function Create_Watchpoint( Ad : int64 ;
                                          Siz, Ac : longint  ) : TCEF_Watchpoint ; overload ;
                                          virtual ; stdcall ; abstract ;
                                      function Watchpoint_At_Index( Index : integer ) : TCEF_Watchpoint ;
                                          virtual ; stdcall ; abstract ;

                                      property Watchpoints[ Index : integer ] : TCEF_Watchpoint
                                          read Watchpoint_At_Index ;

                                      // The following are supported in CEF V2.6 and later:

                                      function Clear_Watchpoint_Ex( Address : int64 ;
                                          Access : longint ; Context : pointer ) : TUnified_Exception ;
                                          virtual ; stdcall ; abstract ;
                                      function Create_Watchpoint_Ex( Ad : int64 ;
                                          Ac : longint ; Context : pointer ; Domain : PChar ) : TCEF_Watchpoint ; overload ;
                                          virtual ; stdcall ; abstract ;
                                      function Create_Watchpoint_Ex( Ad : int64 ;
                                          Siz, Ac : longint ; Context : pointer ; Domain : PChar ) : TCEF_Watchpoint ; overload ;
                                          virtual ; stdcall ; abstract ;
                                      function Watchpoint_At_Ex( Address : int64 ; Context : pointer ) : TCEF_Watchpoint ;
                                          virtual ; stdcall ; abstract ;
                              end ; // TCEF_Watchpoint_Manager

type TCEF_Character_Set = class
                              public // Property handlers...
                                  function Get_Invert : boolean ;
                                      virtual ; stdcall ; abstract ;

                                  procedure Set_Invert( Value : boolean ) ;
                                      virtual ; stdcall ; abstract ;

                              public // API...
                                  procedure Clear ;
                                      virtual ; stdcall ; abstract ;
                                  function Draw( Handle, X, Y, Index : integer ) : boolean ;
                                      virtual ; stdcall ; abstract ;
                                  function Has_Glyph( Index : integer ) : boolean ;
                                      virtual ; stdcall ; abstract ;
                                  function Height : integer ;
                                      virtual ; stdcall ; abstract ;
                                  procedure Initialize ;
                                      virtual ; stdcall ; abstract ;
                                  procedure Load( Name : PChar ) ;
                                      virtual ; stdcall ; abstract ;
                                  procedure Terminate ;
                                      virtual ; stdcall ; abstract ;
                                  function Width( Index : longint ) : longint ;
                                      virtual ; stdcall ; abstract ;
                                  function DrawEx( Handle, X, Y, Index, Flags : integer ) : boolean ;
                                      virtual ; stdcall ; abstract ;

                                  property Invert : boolean
                                      read Get_Invert
                                      write Set_Invert ;
                          end ; // TCharacter_Set

     TCEF_Key_Mapper = class
                           public
                               procedure Add_Key( Name : PChar ) ;
                                      virtual ; stdcall ; abstract ;
                               procedure Add_Mapping( Name : PChar ) ;
                                      virtual ; stdcall ; abstract ;
                               procedure Clear_Keys( Mapping_Only : boolean ) ;
                                      virtual ; stdcall ; abstract ;
                               procedure Clear_Mappings ;
                                      virtual ; stdcall ; abstract ;
                               function Query : boolean ;
                                      virtual ; stdcall ; abstract ;
                               function Mapping( Name : PChar ) : PChar ;
                                      virtual ; stdcall ; abstract ;
                               procedure Set_Mapping( Key, Mapping : PChar ) ;
                                      virtual ; stdcall ; abstract ;
                               procedure Terminate ;
                                      virtual ; stdcall ; abstract ;
                       end ;

implementation

end.
