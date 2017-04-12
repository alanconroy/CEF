{
        Program Name : CEF32
        Package Name : CEF
        Purpose      : CEF32 trace log form
        Institution  :
        Date Written : 27-Apr-2005
        Written By   : Alan Conroy
        Version      : 1.1

        TO THE GLORY OF GOD THROUGH JESUS CHRIST

        Released to the public domain.

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

          This form is used for showing the trace log.

        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

unit Trace_Log_Dlg ;

interface

uses // Borland...
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons, ExtCtrls,

     // Other...
     TDialogs ; // TDialog

type
  TTrace_Log_Form = class( TDialog )
    Button_Panel: TPanel;
    Close_Button: TBitBtn;
    Help_Button: TBitBtn;
    List_Box: TListBox;
    Save_Button: TBitBtn;
    Save_Dialog: TSaveDialog;
    procedure Save_ButtonClick(Sender: TObject);
    procedure Help_ButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end ;

var Trace_Log_Form : TTrace_Log_Form ;

implementation

uses // Other...
     CommonUt, // Clear_IOResult

     // CEF...
     Textstrs, // Text_*
     UE ; // ERT

{$R *.dfm}

// TTrace_Log_Form methods...

procedure TTrace_Log_Form.Save_ButtonClick( Sender : TObject ) ;

var Dummy : integer ;
    F : textfile ;
    Index : integer ;

begin
    if( Save_Dialog.Execute ) then
    begin
        assignfile( F, Save_Dialog.Filename ) ;
        {$I-}
        rewrite( F ) ;
        {$I+}
        Dummy := IOResult ;
        if( Dummy <> 0 ) then
        begin
            showmessage( ERT( Dummy ) + ' while creating log file: ' + Save_Dialog.Filename ) ;
            exit ;
        end ;
        try
            for Index := 0 to List_Box.Items.Count - 1 do
            begin
                {$I-}
                writeln( F, List_Box.Items[ Index ] ) ;
                {$I+}
                Dummy := IOResult ;
                if( Dummy <> 0 ) then
                begin
                    showmessage( ERT( Dummy ) + ' while writing log file: ' + Save_Dialog.Filename ) ;
                    exit ;
                end ;
            end ;
        finally
            {$I-}
            closefile( F ) ;
            {$I+}
            Clear_IOResult ;
        end ;
    end ;
end ;


procedure TTrace_Log_Form.Help_ButtonClick( Sender : TObject ) ;

begin
    Application.HelpContext( HelpContext ) ;
end ;


procedure TTrace_Log_Form.FormShow(Sender: TObject);

begin
    Caption := Text_Caption_Trace_Log ;
    Close_Button.Caption := Text_Button_Caption_Close ;
    Help_Button.Caption := Text_Button_Caption_Help ;
    Save_Button.Caption := Text_Button_Caption_Save ;
    Save_Dialog.Filter := Text_File_Mask ;
    Save_Dialog.Title := Text_Caption_Save_Log_To_File ;
end ;


end.

