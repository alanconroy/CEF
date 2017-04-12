object Main_Form: TMain_Form
  Left = 1278
  Top = 592
  Caption = 'CEF32'
  ClientHeight = 537
  ClientWidth = 940
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Main_Splitter: TSplitter
    Left = 0
    Top = 206
    Width = 940
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 626
  end
  object Top_panel: TPanel
    Left = 0
    Top = 0
    Width = 940
    Height = 206
    Align = alTop
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 786
      Top = 1
      Height = 204
      Align = alRight
      Beveled = True
      ExplicitLeft = 472
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 785
      Height = 204
      Align = alClient
      TabOrder = 0
      OnChange = PageControl1Change
    end
    object CPU_Panel: TPanel
      Left = 789
      Top = 1
      Width = 150
      Height = 204
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter4: TSplitter
        Left = 0
        Top = 149
        Width = 150
        Height = 2
        Cursor = crVSplit
        Align = alBottom
        Beveled = True
      end
      object CPU_Status: TListBox
        Left = 0
        Top = 20
        Width = 150
        Height = 129
        Align = alClient
        BorderStyle = bsNone
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 14
        ParentFont = False
        TabOrder = 0
        OnDblClick = CPU_StatusDblClick
        OnMouseUp = CPU_StatusMouseUp
      end
      object CPU_Caption: TPanel
        Left = 0
        Top = 0
        Width = 150
        Height = 20
        Align = alTop
        Alignment = taLeftJustify
        TabOrder = 1
      end
      object CPU_State: TListBox
        Left = 0
        Top = 151
        Width = 150
        Height = 53
        Align = alBottom
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ItemHeight = 13
        ParentColor = True
        TabOrder = 2
      end
    end
  end
  object Bottom_Panel: TPanel
    Left = 0
    Top = 209
    Width = 940
    Height = 309
    Align = alClient
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 1
      Top = 34
      Width = 938
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitWidth = 624
    end
    object Watch_Panel: TPanel
      Left = 1
      Top = 1
      Width = 938
      Height = 33
      Align = alTop
      TabOrder = 0
      object Watch_List_Box: TListBox
        Left = 1
        Top = 1
        Width = 936
        Height = 31
        Align = alClient
        BevelInner = bvNone
        BorderStyle = bsNone
        ExtendedSelect = False
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = Watch_List_BoxDblClick
        OnKeyDown = Watch_List_BoxKeyDown
        OnMouseUp = Watch_List_BoxMouseUp
      end
    end
    object Panel1: TPanel
      Left = 1
      Top = 37
      Width = 938
      Height = 271
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter3: TSplitter
        Left = 836
        Top = 0
        Width = 4
        Height = 252
        Align = alRight
        Beveled = True
        ExplicitLeft = 522
        ExplicitHeight = 105
      end
      object Immediate_Mode_Panel: TPanel
        Left = 0
        Top = 252
        Width = 938
        Height = 19
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        OnResize = Immediate_Mode_PanelResize
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 81
          Height = 13
          Align = alLeft
          Caption = 'Immediate Mode:'
        end
        object Immediate_Mode_Edit: TEdit
          Left = 88
          Top = -2
          Width = 98
          Height = 21
          Enabled = False
          TabOrder = 0
          OnKeyDown = Immediate_Mode_EditKeyDown
        end
      end
      object Stack_List_Box: TListBox
        Left = 840
        Top = 0
        Width = 98
        Height = 252
        Align = alRight
        BevelInner = bvNone
        BevelOuter = bvNone
        Color = clBtnFace
        ExtendedSelect = False
        ItemHeight = 13
        TabOrder = 1
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 836
        Height = 252
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        object Memory_ScrollBar: TScrollBar
          Left = 820
          Top = 0
          Width = 16
          Height = 252
          Align = alRight
          Kind = sbVertical
          PageSize = 0
          TabOrder = 0
          OnScroll = Memory_ScrollBarScroll
        end
        object PageControl2: TPageControl
          Left = 0
          Top = 0
          Width = 820
          Height = 252
          ActivePage = TabSheet1
          Align = alClient
          TabOrder = 1
          object TabSheet1: TTabSheet
            Caption = 'Memory'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
          object TabSheet2: TTabSheet
            Tag = 1
            Caption = 'Ports'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 518
    Width = 940
    Height = 19
    Panels = <
      item
        Width = 75
      end
      item
        Width = 75
      end
      item
        Width = 50
      end>
  end
  object MainMenu1: TMainMenu
    Left = 17
    Top = 45
    object File1: TMenuItem
      Caption = '&File'
      object OpenEmulator1: TMenuItem
        Caption = 'Open &Emulator...'
        OnClick = OpenEmulator1Click
      end
      object Reopenemulator1: TMenuItem
        Caption = 'Reopen emulator'
        object N13: TMenuItem
          Caption = '&1'
          Visible = False
          OnClick = Reopen_Emulator
        end
        object N23: TMenuItem
          Caption = '&2'
          Visible = False
          OnClick = Reopen_Emulator
        end
        object N33: TMenuItem
          Caption = '&3'
          Visible = False
          OnClick = Reopen_Emulator
        end
        object N43: TMenuItem
          Caption = '&4'
          Visible = False
          OnClick = Reopen_Emulator
        end
        object N53: TMenuItem
          Caption = '&5'
          Visible = False
          OnClick = Reopen_Emulator
        end
        object N63: TMenuItem
          Caption = '&6'
          Visible = False
          OnClick = Reopen_Emulator
        end
        object N73: TMenuItem
          Caption = '&7'
          Visible = False
          OnClick = Reopen_Emulator
        end
        object N83: TMenuItem
          Caption = '&8'
          Visible = False
          OnClick = Reopen_Emulator
        end
        object N93: TMenuItem
          Caption = '&9'
          Visible = False
          OnClick = Reopen_Emulator
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object New1: TMenuItem
        Caption = '&New'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = '&Open Source...'
        OnClick = Open1Click
      end
      object Reopensource1: TMenuItem
        Caption = 'Reopen source'
        object N11: TMenuItem
          Caption = '&1'
          Visible = False
          OnClick = Reopen_File
        end
        object N21: TMenuItem
          Caption = '&2'
          Visible = False
          OnClick = Reopen_File
        end
        object N31: TMenuItem
          Caption = '&3'
          Visible = False
          OnClick = Reopen_File
        end
        object N41: TMenuItem
          Caption = '&4'
          Visible = False
          OnClick = Reopen_File
        end
        object N51: TMenuItem
          Caption = '&5'
          Visible = False
          OnClick = Reopen_File
        end
        object N61: TMenuItem
          Caption = '&6'
          Visible = False
          OnClick = Reopen_File
        end
        object N71: TMenuItem
          Caption = '&7'
          Visible = False
          OnClick = Reopen_File
        end
        object N81: TMenuItem
          Caption = '&8'
          Visible = False
          OnClick = Reopen_File
        end
        object N91: TMenuItem
          Caption = '&9'
          Visible = False
          OnClick = Reopen_File
        end
      end
      object Save3: TMenuItem
        Caption = '&Save'
        OnClick = Save3Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
        OnClick = SaveAs1Click
      end
      object SaveAll1: TMenuItem
        Caption = 'Save All'
        OnClick = SaveAll1Click
      end
      object MediaManager1: TMenuItem
        Caption = '&Media Manager...'
        OnClick = MediaManager1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = '&Close'
        Enabled = False
        ShortCut = 16499
        OnClick = Close1Click
      end
      object CloseAll1: TMenuItem
        Caption = 'Close All'
        OnClick = CloseAll1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Printersetup1: TMenuItem
        Caption = 'Printer setup...'
        OnClick = Printersetup1Click
      end
      object Print1: TMenuItem
        Caption = '&Print...'
        OnClick = Print1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Components1: TMenuItem
      Caption = '&Components'
      GroupIndex = 10
      object Load1: TMenuItem
        Caption = '&Load...'
        OnClick = Load1Click
      end
      object UnloadAll1: TMenuItem
        Caption = 'Unload &All'
        OnClick = UnloadAll1Click
      end
      object Emulatorport1: TMenuItem
        Caption = '&Emulator ports...'
        OnClick = Emulatorport1Click
      end
      object N18: TMenuItem
        Caption = '&Clock mode'
        object Default1: TMenuItem
          Caption = '&Default'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = Default1Click
        end
        object Ignore1: TMenuItem
          Caption = '&Ignore'
          GroupIndex = 1
          RadioItem = True
          OnClick = Ignore1Click
        end
        object Synchronize1: TMenuItem
          Caption = '&Synchronize'
          GroupIndex = 1
          RadioItem = True
          OnClick = Synchronize1Click
        end
        object N19: TMenuItem
          Caption = '-'
          GroupIndex = 1
        end
        object Disable1: TMenuItem
          Caption = '&Disable'
          GroupIndex = 1
          OnClick = Disable1Click
        end
      end
      object Unblockall1: TMenuItem
        Caption = '&Unblock all'
        OnClick = Unblockall1Click
      end
      object SelectCPU: TMenuItem
        Caption = '&Select CPU...'
        Enabled = False
        OnClick = SelectCPUClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object DefaultMemory1: TMenuItem
        Caption = 'Default &Memory'
      end
      object N20: TMenuItem
        Caption = '-'
        Visible = False
      end
      object N12: TMenuItem
        Caption = '&1'
        Visible = False
      end
      object N22: TMenuItem
        Caption = '&2'
        Visible = False
      end
      object N32: TMenuItem
        Caption = '&3'
        Visible = False
      end
      object N42: TMenuItem
        Caption = '&4'
        Visible = False
      end
      object N52: TMenuItem
        Caption = '&5'
        Visible = False
      end
      object N62: TMenuItem
        Caption = '&6'
        Visible = False
      end
      object N72: TMenuItem
        Caption = '&7'
        Visible = False
      end
      object N82: TMenuItem
        Caption = '&8'
        Visible = False
      end
      object N92: TMenuItem
        Caption = '&9'
        Visible = False
      end
    end
    object Assemble1: TMenuItem
      Caption = '&Assemble'
      Enabled = False
      GroupIndex = 10
      object Assemble2: TMenuItem
        Caption = '&Assemble'
        Enabled = False
        Hint = 'watchpoint'
        ShortCut = 16504
        OnClick = Assemble2Click
      end
      object AssembleAll1: TMenuItem
        Caption = 'Assemble Al&l'
        OnClick = AssembleAll1Click
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object ShowErrors: TMenuItem
        Caption = '&Show Errors...'
        OnClick = ShowErrorsClick
      end
    end
    object Run1: TMenuItem
      Caption = '&Run'
      Enabled = False
      GroupIndex = 10
      object Execute1: TMenuItem
        Caption = 'E&xecute'
        ShortCut = 120
        OnClick = Execute1Click
      end
      object StepOver1: TMenuItem
        Caption = 'Step &Over'
        ShortCut = 119
        OnClick = StepOver1Click
      end
      object StepInto1: TMenuItem
        Caption = 'Step &Into'
        ShortCut = 118
        OnClick = StepInto1Click
      end
      object Pause1: TMenuItem
        Caption = 'Program &pause'
        OnClick = Pause1Click
      end
      object Runimmediate1: TMenuItem
        Caption = '&Run immediate'
        ShortCut = 121
        OnClick = Runimmediate1Click
      end
      object AddWatch1: TMenuItem
        Caption = 'Add &Watch'
        OnClick = AddWatch1Click
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object AddBreakpoint1: TMenuItem
        Caption = 'Add Execution &Breakpoint...'
        OnClick = AddBreakpoint1Click
      end
      object ExecutionBreakpoints1: TMenuItem
        Caption = 'Execution Breakpoints...'
        OnClick = ExecutionBreakpoints1Click
      end
      object AddPortBreakpoint1: TMenuItem
        Caption = 'Add Port Breakpoint...'
        OnClick = AddPortBreakpoint1Click
      end
      object PortBreakpoints1: TMenuItem
        Caption = 'Port Breakpoints...'
        OnClick = PortBreakpoints1Click
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object Profiling1: TMenuItem
        Caption = 'Profiling'
        OnClick = Profiling1Click
      end
      object ProfileReport1: TMenuItem
        Caption = '&Profile Report...'
        OnClick = ProfileReport1Click
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object Trace1: TMenuItem
        Caption = 'Trace...'
        OnClick = Trace1Click
      end
      object TraceLog1: TMenuItem
        Caption = '&Trace Log...'
        OnClick = TraceLog1Click
      end
    end
    object Internals1: TMenuItem
      Caption = '&Internals'
      GroupIndex = 10
      OnClick = Internals1Click
    end
    object ClientServer1: TMenuItem
      Caption = 'Client/&Server'
      GroupIndex = 10
      Visible = False
      object Server1: TMenuItem
        Caption = '&Server...'
        OnClick = Server1Click
      end
    end
    object Options2: TMenuItem
      Caption = '&Options...'
      GroupIndex = 10
      OnClick = Options2Click
    end
    object Help1: TMenuItem
      Caption = '&Help'
      GroupIndex = 10
      object Help2: TMenuItem
        Caption = '&Help'
        OnClick = Help2Click
      end
      object CEFSpecification1: TMenuItem
        Caption = 'CEF Specification'
        OnClick = CEFSpecification1Click
      end
      object ComponentHelp1: TMenuItem
        Caption = 'Component Help'
        OnClick = ComponentHelp1Click
      end
      object N24: TMenuItem
        Caption = 'Utility help'
        OnClick = N24Click
      end
      object N26: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About'
        OnClick = About1Click
      end
    end
  end
  object Open_Dialog: TOpenDialog
    DefaultExt = 'cef'
    Filter = 'CEF files|*.cef'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open emulator'
    Left = 50
    Top = 81
  end
  object Popup_Menu: TPopupMenu
    OnPopup = Popup_MenuPopup
    Left = 15
    Top = 325
    object Radix1: TMenuItem
      Caption = '&Radix'
      object Binary1: TMenuItem
        Caption = '&Binary'
        GroupIndex = 2
        RadioItem = True
        OnClick = Binary1Click
      end
      object Octal1: TMenuItem
        Caption = '&Octal'
        GroupIndex = 2
        RadioItem = True
        OnClick = Octal1Click
      end
      object Decimal1: TMenuItem
        Caption = '&Decimal'
        Checked = True
        GroupIndex = 2
        RadioItem = True
        OnClick = Decimal1Click
      end
      object Hexadecimal1: TMenuItem
        Caption = 'Hexadecimal'
        GroupIndex = 2
        RadioItem = True
        OnClick = Hexadecimal1Click
      end
      object Radix50: TMenuItem
        Caption = 'Radix-50'
        GroupIndex = 2
        RadioItem = True
        OnClick = Radix50Click
      end
      object Other1: TMenuItem
        Caption = 'Other...'
        GroupIndex = 2
        RadioItem = True
        OnClick = Other1Click
      end
    end
    object Signed1: TMenuItem
      Caption = 'Si&gned'
      OnClick = Signed1Click
    end
    object Size1: TMenuItem
      Caption = '&Size'
      object Byte1: TMenuItem
        Caption = '&Byte'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = Byte1Click
      end
      object Word1: TMenuItem
        Caption = '&Word'
        GroupIndex = 1
        RadioItem = True
        OnClick = Word1Click
      end
      object Long1: TMenuItem
        Caption = '&Long'
        GroupIndex = 1
        RadioItem = True
        OnClick = Long1Click
      end
      object Double1: TMenuItem
        Caption = '&Quad'
        GroupIndex = 1
        RadioItem = True
        Visible = False
        OnClick = Double1Click
      end
    end
    object EBCDIC1: TMenuItem
      Caption = '&EBCDIC'
      Checked = True
      OnClick = EBCDIC1Click
    end
    object Physical1: TMenuItem
      Caption = '&Physical'
      Checked = True
      Enabled = False
      OnClick = Physical1Click
    end
    object AddressSpace1: TMenuItem
      Caption = 'Address space'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Gotoaddress1: TMenuItem
      Caption = '&Goto address...'
      OnClick = Gotoaddress1Click
    end
    object Find1: TMenuItem
      Caption = '&Find...'
      OnClick = Find1Click
    end
    object Modify1: TMenuItem
      Caption = '&Modify...'
      OnClick = Modify1Click
    end
    object Watchpoints1: TMenuItem
      Caption = 'Watchpoints'
      object Createnew1: TMenuItem
        Caption = 'Create new...'
        OnClick = Createnew1Click
      end
      object View1: TMenuItem
        Caption = 'View...'
        OnClick = View1Click
      end
    end
    object Pattern1: TMenuItem
      Caption = '&Pattern...'
      OnClick = Pattern1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Save1: TMenuItem
      Caption = 'Save Contents...'
      OnClick = Save1Click
    end
    object Restore1: TMenuItem
      Caption = 'Restore Contents...'
      OnClick = Restore1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Save2: TMenuItem
      Caption = 'Save State...'
      OnClick = Save2Click
    end
    object Restore2: TMenuItem
      Caption = 'Restore State...'
      OnClick = Restore2Click
    end
  end
  object Save_Memory_Dialog: TSaveDialog
    DefaultExt = 'cmc'
    Filter = 'CEF Memory Contents|*.cmc|All files|*.*'
    Title = 'Save Memory Contents'
    Left = 48
    Top = 322
  end
  object Save_Memory_State_Dialog: TSaveDialog
    DefaultExt = 'cms'
    Filter = 'CEF Memory State|*.cms|All Files|*.*'
    Title = 'Save Memory State'
    Left = 81
    Top = 321
  end
  object Save_Dialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 49
    Top = 43
  end
  object Restore_Memory_Dialog: TOpenDialog
    DefaultExt = 'cmc'
    Filter = 'CEF Memory Contents|*.cmc|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Restore Memory'
    Left = 48
    Top = 357
  end
  object Restore_memory_State_Dialog: TOpenDialog
    DefaultExt = 'cms'
    Filter = 'CEF Memory State|*.cms|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Restore Memory State'
    Left = 87
    Top = 356
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 12
    Top = 81
  end
  object CPU_PopupMenu: TPopupMenu
    Left = 909
    Top = 43
    object Increment1: TMenuItem
      Caption = '&Increment'
      OnClick = Increment1Click
    end
    object Decrement1: TMenuItem
      Caption = '&Decrement'
      OnClick = Decrement1Click
    end
    object Change1: TMenuItem
      Caption = '&Change...'
      OnClick = Change1Click
    end
    object Zero1: TMenuItem
      Caption = '&Zero'
      OnClick = Zero1Click
    end
    object N25: TMenuItem
      Caption = '-'
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      OnClick = Copy1Click
    end
    object Paste1: TMenuItem
      Caption = 'Paste'
      OnClick = Paste1Click
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object AddRegisterBreakpoint1: TMenuItem
      Caption = 'Add Register &Breakpoint...'
      OnClick = AddRegisterBreakpoint1Click
    end
    object RegisterBreakpoints1: TMenuItem
      Caption = '&Register Breakpoints...'
      OnClick = RegisterBreakpoints1Click
    end
  end
  object Watches_Popup_Menu: TPopupMenu
    Left = 101
    Top = 213
    object Add1: TMenuItem
      Caption = '&Add...'
      OnClick = AddWatch1Click
    end
    object Edit1: TMenuItem
      Caption = '&Edit...'
      Enabled = False
      OnClick = Edit1Click
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      Enabled = False
      OnClick = Delete1Click
    end
    object DeleteAll1: TMenuItem
      Caption = 'Delete A&ll'
      Enabled = False
      OnClick = DeleteAll1Click
    end
  end
  object Disassembly_Popup: TPopupMenu
    Left = 14
    Top = 141
    object Showsource1: TMenuItem
      Caption = '&Show source'
      OnClick = Showsource1Click
    end
    object Gotoaddress2: TMenuItem
      Caption = '&Goto address...'
      OnClick = Gotoaddress2Click
    end
    object GotoCurrent1: TMenuItem
      Caption = 'Goto &Current'
      OnClick = GotoCurrent1Click
    end
  end
  object Stack_Popup: TPopupMenu
    Left = 560
    Top = 325
    object TopofStack1: TMenuItem
      Caption = '&Top of Stack'
      OnClick = TopofStack1Click
    end
    object Gotoaddress3: TMenuItem
      Caption = '&Goto address...'
      OnClick = Gotoaddress3Click
    end
  end
  object Open_Source_Dialog: TOpenDialog
    Title = 'Open source'
    Left = 129
    Top = 83
  end
  object Port_Popup_Menu: TPopupMenu
    Left = 14
    Top = 348
    object MenuItem1: TMenuItem
      Caption = '&Radix'
      object MenuItem2: TMenuItem
        Caption = '&Binary'
        GroupIndex = 2
        RadioItem = True
        OnClick = Binary1Click
      end
      object MenuItem3: TMenuItem
        Caption = '&Octal'
        GroupIndex = 2
        RadioItem = True
        OnClick = Octal1Click
      end
      object MenuItem4: TMenuItem
        Caption = '&Decimal'
        Checked = True
        GroupIndex = 2
        RadioItem = True
        OnClick = Decimal1Click
      end
      object MenuItem5: TMenuItem
        Caption = 'Hexadecimal'
        GroupIndex = 2
        RadioItem = True
        OnClick = Hexadecimal1Click
      end
      object MenuItem6: TMenuItem
        Caption = 'Radix-50'
        GroupIndex = 2
        RadioItem = True
        OnClick = Radix50Click
      end
      object MenuItem7: TMenuItem
        Caption = 'Other...'
        GroupIndex = 2
        RadioItem = True
        OnClick = Other1Click
      end
    end
    object MenuItem8: TMenuItem
      Caption = 'Si&gned'
      OnClick = Signed1Click
    end
    object MenuItem9: TMenuItem
      Caption = '&Size'
      object MenuItem10: TMenuItem
        Caption = '&Byte'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = Byte1Click
      end
      object MenuItem11: TMenuItem
        Caption = '&Word'
        GroupIndex = 1
        RadioItem = True
        OnClick = Word1Click
      end
      object MenuItem12: TMenuItem
        Caption = '&Long'
        GroupIndex = 1
        RadioItem = True
        OnClick = Long1Click
      end
      object MenuItem13: TMenuItem
        Caption = '&Quad'
        GroupIndex = 1
        RadioItem = True
        Visible = False
        OnClick = Double1Click
      end
    end
    object MenuItem14: TMenuItem
      Caption = '&EBCDIC'
      Checked = True
      OnClick = EBCDIC1Click
    end
    object MenuItem17: TMenuItem
      Caption = '-'
    end
    object MenuItem18: TMenuItem
      Caption = '&Goto port...'
      OnClick = GotoPortClick
    end
    object MenuItem20: TMenuItem
      Caption = '&Modify...'
      OnClick = MenuItem20Click
    end
    object Read1: TMenuItem
      Caption = '&Input...'
      OnClick = Read1Click
    end
    object Output1: TMenuItem
      Caption = '&Output...'
      OnClick = Output1Click
    end
  end
end
