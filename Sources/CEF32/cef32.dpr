{
        Program Name : CEF32
        Package Name : CEF32
        Purpose      : CEF32 application
        Institution  :
        Date Written : 27-Apr-2000
        Written By   : Alan Conroy
        Version      : 1.0

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

          This program is the generic CEF interface.
          
        *************************************************************
        *                                                           *
        *        C O N T R I B U T O R S                            *
        *                                                           *
        *************************************************************

        AC (EAC)    Alan Conroy    http://www.conroyhome.net/alan

}

program CEF32 ;

uses
  Forms,
  _Applic,
  Applic,
  CEF,
  Clock,
  Memory,
  CEFMain in 'CEFMain.pas' {Main_Form},
  About in 'About.pas' {About_Box},
  CEF_Internals in 'CEF_Internals.pas' {Internals_Form},
  Radix50s in 'c:\lb\Radix50s.pas',
  Radix_Dialog in 'c:\lb\Radix_Dialog.pas' {Radix_Form},
  Goto_Address in 'c:\lb\Goto_Address.pas' {Goto_Address_Form},
  Find_Data_Dialog in 'c:\lb\Find_Data_Dialog.pas' {Find_Form},
  ssearch in 'c:\lb\ssearch.pas' {Search_Dialog},
  sreplace in 'c:\lb\sreplace.pas' {Replace_Dialog},
  Linenumb in 'c:\lb\Linenumb.pas' {line_Number_Dialog},
  SaveEdit in 'C:\lb\SaveEdit.pas' {Save_Modified_Dialog},
  EditPref in 'c:\lb\EditPref.pas' {Editor_Preferences},
  ModMem in 'c:\lb\ModMem.pas' {Modify_Memory_Dialog},
  Value_Edit_Dialog in 'Value_Edit_Dialog.pas' {Edit_Value_Form},
  info in 'info.pas' {Assembly_Information},
  ModReg in 'ModReg.pas' {Modify_Register},
  ModConfig in 'ModConfig.pas' {Component_Configuration_Dialog},
  Assembly_Status in 'Assembly_Status.pas' {Assembly_Statistics},
  Error_List in 'Error_List.pas' {Error_Dialog},
  CCM in 'CCM.pas' {Component_Connection_Manager},
  CCM_Add_Dlg in 'CCM_Add_Dlg.pas' {CCM_Add_Form},
  Profile_Report in 'Profile_Report.pas' {Profile_Report_Form},
  DisMem in 'DisMem.pas' {Save_Disassembly_Form},
  LoadMem in 'LoadMem.pas' {Load_Memory_Form},
  DumpMem in 'DumpMem.pas' {Dump_Memory_Form},
  Options in 'Options.pas' {Options_Form},
  Port_IO_Form in 'Port_IO_Form.pas' {Port_IO_Dialog},
  Media_Manager_Dialog in 'Media_Manager_Dialog.pas' {Media_Manager},
  TapePres_Add_Data in 'TapePres_Add_Data.pas' {Tape_Add_Data_Dialog},
  Tape_Header_Dialog in 'Tape_Header_Dialog.pas' {TTape_Header},
  Conditions_Dlg in '..\Shared\Conditions_Dlg.pas' {Conditions_Form},
  Trace_Dlg in '..\Shared\Trace_Dlg.pas' {Trace_Form},
  Trace_Log_Dlg in '..\Shared\Trace_Log_Dlg.pas' {Trace_Log_Form},
  Output_Error in '..\Shared\Output_Error.pas' {Output_Error_Form},
  Overwrite_Error in '..\Shared\Overwrite_Error.pas' {Overwrite_Form},
  Server_Form in 'Server_Form.pas' {Server_Dialog},
  Load_Component_Dialog in 'Load_Component_Dialog.pas' {Load_Component_Form},
  CPU_Selection_Dialog in 'CPU_Selection_Dialog.pas' {CPU_Selection_Form};

{$R *.RES}

begin
    Application_Manager^.Name := 'CEF32' ;
    Application_Manager^.Version := 'V2.1' ;
  Application.Initialize;
  Application.HelpFile := 'CEF32';
  Application.CreateForm(TMain_Form, Main_Form);
  Application.CreateForm(TOptions_Form, Options_Form);
  Application.CreateForm(TAbout_Box, About_Box);
  Application.CreateForm(TRadix_Form, Radix_Form);
  Application.CreateForm(TGoto_Address_Form, Goto_Address_Form);
  Application.CreateForm(TFind_Form, Find_Form);
  Application.CreateForm(TSearch_Dialog, Search_Dialog);
  Application.CreateForm(TReplace_Dialog, Replace_Dialog);
  Application.CreateForm(Tline_Number_Dialog, line_Number_Dialog);
  Application.CreateForm(TEditor_Preferences, Editor_Preferences);
  Application.CreateForm(TSave_Modified_Dialog, Save_Modified_Dialog);
  Application.CreateForm(TAssembly_Information, Assembly_Information);
  Application.CreateForm(TModify_Register, Modify_Register);
  Application.CreateForm(TModify_Memory_Dialog, Modify_Memory_Dialog);
  Application.CreateForm(TComponent_Configuration_Dialog, Component_Configuration_Dialog);
  Application.CreateForm(TAssembly_Statistics, Assembly_Statistics);
  Application.CreateForm(TError_Dialog, Error_Dialog);
  Application.CreateForm(TConditions_Form, Conditions_Form);
  Application.CreateForm(TComponent_Connection_Manager, Component_Connection_Manager);
  Application.CreateForm(TCCM_Add_Form, CCM_Add_Form);
  Application.CreateForm(TProfile_Report_Form, Profile_Report_Form);
  Application.CreateForm(TTrace_Form, Trace_Form);
  Application.CreateForm(TTrace_Log_Form, Trace_Log_Form);
  Application.CreateForm(TSave_Disassembly_Form, Save_Disassembly_Form);
  Application.CreateForm(TLoad_Memory_Form, Load_Memory_Form);
  Application.CreateForm(TDump_Memory_Form, Dump_Memory_Form);
  Application.CreateForm(TPort_IO_Dialog, Port_IO_Dialog);
  Application.CreateForm(TMedia_Manager, Media_Manager);
  Application.CreateForm(TServer_Dialog, Server_Dialog);
  Application.CreateForm(TLoad_Component_Form, Load_Component_Form);
  Application.CreateForm(TCPU_Selection_Form, CPU_Selection_Form);
  Application.Run;
end.
