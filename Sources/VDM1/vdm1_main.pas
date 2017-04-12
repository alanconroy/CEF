{
        Program Name : VDM1
        Package Name : CEF
        Purpose      : S100 VDM-1 screen main form
        Institution  : Conroy & Conroy Co.
        Date Written : 8-Apr-2006
        Written By   : Alan Conroy
        Version      : 1.0A

        Copyright (C) 2005-2007 by Alan Conroy.  Released to the public domain.

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        *************************************************************
        *                                                           *
        *        M O D I F I C A T I O N   H I S T O R Y            *
        *                                                           *
        *************************************************************

            DATE        BY          REASON

         20-Jan-2007    EAC         Handle shutdown better.

        *************************************************************
        *                                                           *
        *        P R O G R A M   P U R P O S E                      *
        *                                                           *
        *************************************************************

          Provides a generic "screen" that maps memory.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit vdm1_main ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs ;

type
  TVDM_Form = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private // Internal utility routines...
      procedure CB_Idle( Sender : TObject ; var Done : Boolean ) ;

  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TVDM_Form.FormShow(Sender: TObject);

begin
    Application.OnIdle := CB_Idle ;
end ;


procedure TVDM_Form.CB_Idle( Sender : TObject ; var Done : Boolean ) ;

begin
    try
        if( Visible ) then
        begin
            Visible := False ;
        end ;
    except
    end ;
    Done := True ;
end ;


procedure TVDM_Form.FormClose( Sender : TObject ; var Action : TCloseAction ) ;

begin
    Action := caFree ;
end ;


end.

