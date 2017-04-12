/*
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

          Type definitions for the CEF_Util DLL.
*/

#include "cef.h"

#define int64 __int64

class TCEF_Watchpoint 
{
    public: // API...
        virtual int64 __stdcall Get_Address() = 0 ;
        virtual int __stdcall Get_Access() = 0 ;
        virtual void __stdcall Set_Access( int Value ) = 0 ;
        virtual char* __stdcall Serialize() = 0 ;
        virtual void __stdcall Deserialize( char* Image ) = 0 ;
}


class TCEF_Watchpoint_Manager
{
    public: // API...
        virtual void __stdcall Clear() = 0 ;
        virtual TUEC __stdcall Clear_Watchpoint( int64 Address, int Access ) = 0 ;
        virtual TCEF_Watchpoint* __stdcall Create_Watchpoint( int64 Address, int Access ) = 0 ;
        virtual int __stdcall Count() = - ;
        virtual void __stdcall Deserialize( char* Source ) = 0 ;
        virtual char* __stdcall Serialize() = 0 ;
        virtual void __stdcall Terminate() = 0 ;
        virtual TCEF_Watchpoint* __stdcall Watchpoint_At( int64 Address ) = 0 ;
}


class TCEF_Character_Set
{
    public: // API...
        virtual bool __stdcall Get_Invert() ;
        virtual void __stdcall Set_Invert( bool Value) ;
        virtual void __stdcall Clear() = 0 ;
        virtual bool __stdcall Draw( int Handle, int X, int Y, int Index ) = 0 ;
        virtual bool __stdcall Has_Glyph( int Index ) = 0 ;
        virtual int __stdcall Height() = 0 ;
        virtual void __stdcall Initialize() = 0 ;
        virtual void __stdcall Load( char* Name ) = 0 ;
        virtual void __stdcall Terminate() = 0 ;
        virtual int __stdcall Width( int Index ) = 0 ;
}


class TCEF_Key_Mapper
{
    public:
        virtual void __stdcall Add_Key( char* Name ) = 0 ;
        virtual void __stdcall Add_Mapping( char* Name ) = 0 ;
        virtual void __stdcall Clear_Keys( bool Mapping_Only ) = 0 ;
        virtual void __stdcall Clear_Mappings() = 0 ;
        virtual bool __stdcall Query() = 0 ;
        virtual char* __stdcall Mapping( char* Name ) = 0 ;
        virtual void __stdcall Set_Mapping( char* Key, char* Mapping ) = 0 ;
        virtual void __stdcall Terminate() = 0 ;
}
