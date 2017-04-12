object Main_Form: TMain_Form
  Left = 658
  Top = 135
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Printer'
  ClientHeight = 700
  ClientWidth = 1006
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1006
    Height = 681
    Align = alClient
    TabOrder = 0
    object Screen_Panel: TPanel
      Left = 1
      Top = 1
      Width = 1004
      Height = 479
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      OnResize = Screen_PanelResize
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 1004
        Height = 479
        HorzScrollBar.Tracking = True
        VertScrollBar.Tracking = True
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        object Image: TImage
          Left = 0
          Top = 0
          Width = 105
          Height = 105
          Stretch = True
        end
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 480
      Width = 1004
      Height = 200
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 681
    Width = 1006
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object MainMenu1: TMainMenu
    Left = 284
    Top = 4
    object File1: TMenuItem
      Caption = '&File'
      object Logtofile1: TMenuItem
        Caption = '&Log to file...'
        OnClick = Logtofile1Click
      end
      object Sendfile1: TMenuItem
        Caption = '&Send file...'
        OnClick = Sendfile1Click
      end
      object Cancellogging1: TMenuItem
        Caption = '&Cancel logging'
        Enabled = False
        OnClick = Cancellogging1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Copyscreen1: TMenuItem
        Caption = '&Copy page'
        OnClick = Copyscreen1Click
      end
    end
    object SerialPort1: TMenuItem
      Caption = '&Interface'
      object Baudrate1: TMenuItem
        Caption = '&Baud rate'
        object Auto1: TMenuItem
          Caption = '&Auto'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N751: TMenuItem
          Caption = '&75'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N1101: TMenuItem
          Caption = '&110'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N1501: TMenuItem
          Caption = '1&50'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N3001: TMenuItem
          Caption = '&300'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N6001: TMenuItem
          Caption = '&600'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N12001: TMenuItem
          Caption = '1&200'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N24001: TMenuItem
          Caption = '2&400'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N48001: TMenuItem
          Caption = '4&800'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
        object N96001: TMenuItem
          Caption = '&9600'
          GroupIndex = 1
          RadioItem = True
          OnClick = Baud_Click
        end
      end
      object N8databits1: TMenuItem
        Caption = '&Data bits'
        object N71: TMenuItem
          Caption = '&7'
        end
        object N81: TMenuItem
          Caption = '&8'
          Checked = True
        end
      end
      object Flowcontrol1: TMenuItem
        Caption = '&Flow control'
        object None1: TMenuItem
          Caption = '&None'
          Checked = True
          RadioItem = True
        end
        object XOnXOff1: TMenuItem
          Caption = '&XOn/XOff'
          RadioItem = True
        end
        object CTSRTS1: TMenuItem
          Caption = '&CTS/RTS'
          RadioItem = True
        end
      end
      object N2stopbits1: TMenuItem
        Caption = '&2 stop bits'
      end
      object Parity1: TMenuItem
        Caption = '&Parity'
      end
      object Online1: TMenuItem
        Caption = 'Online'
        Checked = True
        Hint = 'Online if checked, local if unchecked'
        OnClick = Check_Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Connect1: TMenuItem
        Caption = '&Connect to new component...'
        OnClick = Connect1Click
      end
      object ConnecttoEmulatorport1: TMenuItem
        Caption = 'Connect to &Emulator port...'
        OnClick = ConnecttoEmulatorport1Click
      end
      object Disconnect1: TMenuItem
        Caption = 'Disconnect...'
        OnClick = Disconnect1Click
      end
    end
    object Video1: TMenuItem
      Caption = '&Paper/Tape'
      object Newpaper1: TMenuItem
        Caption = '&New paper...'
        OnClick = Newpaper1Click
      end
      object LoadPaper: TMenuItem
        Caption = '&Load paper...'
        OnClick = LoadPaperClick
      end
      object Unloadpaper1: TMenuItem
        Caption = '&Unload paper'
        OnClick = Unloadpaper1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Pageup1: TMenuItem
        Caption = 'Page up'
        OnClick = Pageup1Click
      end
      object Pagedown1: TMenuItem
        Caption = 'Page down'
        OnClick = Pagedown1Click
      end
      object Gotopage1: TMenuItem
        Caption = '&Goto page...'
        OnClick = Gotopage1Click
      end
      object Gotoprinthead1: TMenuItem
        Caption = 'Goto &print head'
        OnClick = Gotoprinthead1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object AdvancepaperlocalLF1: TMenuItem
        Caption = 'Advance paper (local LF)'
        OnClick = AdvancepaperlocalLF1Click
      end
      object opofformlocalFF1: TMenuItem
        Caption = 'Top of form (local FF)'
        OnClick = opofformlocalFF1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Loadpapertape1: TMenuItem
        Caption = 'Load paper tape...'
        OnClick = Loadpapertape1Click
      end
      object Unloadpapertape1: TMenuItem
        Caption = 'Unload paper tape'
        Enabled = False
        OnClick = Unloadpapertape1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object Emulation1: TMenuItem
        Caption = '&Emulation'
        object KSR331: TMenuItem
          Caption = 'KSR33'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = KSR331Click
        end
        object TY1: TMenuItem
          Caption = 'ASR33'
          GroupIndex = 1
          RadioItem = True
          OnClick = TY1Click
        end
        object DECLA301: TMenuItem
          Caption = 'DEC LA30'
          GroupIndex = 1
          OnClick = DECLA301Click
        end
        object VT051: TMenuItem
          Caption = 'DEC LA36'
          GroupIndex = 1
          RadioItem = True
          OnClick = VT051Click
        end
        object N3: TMenuItem
          Caption = '-'
          GroupIndex = 1
        end
        object RO331: TMenuItem
          Caption = 'RO33'
          GroupIndex = 1
          RadioItem = True
          OnClick = RO331Click
        end
      end
      object Keymapping1: TMenuItem
        Caption = '&Key mapping...'
        OnClick = Keymapping1Click
      end
      object ChangeKeyboard1: TMenuItem
        Caption = '&Change Keyboard...'
        OnClick = ChangeKeyboard1Click
      end
      object Keyboard1: TMenuItem
        Caption = '&Keyboard'
        Checked = True
        OnClick = Keyboard1Click
      end
      object Uppercase1: TMenuItem
        Caption = 'Uppercase output'
        OnClick = Uppercaseoutput1Click
      end
      object Uppercaseinput1: TMenuItem
        Caption = 'Uppercase input'
        OnClick = Uppercaseinput1Click
      end
      object Local1: TMenuItem
        Caption = '&Local echo'
        OnClick = Check_Click
      end
      object HereIs1: TMenuItem
        Caption = '&Here Is'
        object Transmit1: TMenuItem
          Caption = '&Transmit'
          Enabled = False
          OnClick = Transmit1Click
        end
        object Configure1: TMenuItem
          Caption = '&Configure...'
          OnClick = HereIs1Click
        end
      end
      object AutoLF: TMenuItem
        Caption = 'Auto LF'
        object AutoLF1: TMenuItem
          AutoCheck = True
          Caption = '&Transmit'
        end
        object Receive1: TMenuItem
          AutoCheck = True
          Caption = '&Receive'
        end
      end
      object FormControl1: TMenuItem
        Caption = 'Form Control Option'
        Enabled = False
        object FormControlOption1: TMenuItem
          AutoCheck = True
          Caption = '&Enabled'
        end
        object VerticalTabPositions1: TMenuItem
          Caption = '&Vertical Tab Positions'
          OnClick = VerticalTabPositions1Click
        end
        object HorizontalTabPositions1: TMenuItem
          Caption = '&Horizontal Tab Positions'
          OnClick = HorizontalTabPositions1Click
        end
      end
      object SelectiveAddressing1: TMenuItem
        Caption = 'Selective Addressing'
        Enabled = False
        object Enable1: TMenuItem
          AutoCheck = True
          Caption = '&Enable'
          OnClick = Enable1Click
        end
        object Selected1: TMenuItem
          AutoCheck = True
          Caption = '&Selected'
          OnClick = Selected1Click
        end
        object Master1: TMenuItem
          AutoCheck = True
          Caption = '&Master'
          OnClick = Master1Click
        end
        object SetCodes1: TMenuItem
          Caption = 'Set &Codes...'
          OnClick = SetCodes1Click
        end
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object AlternateCharacterSet: TMenuItem
        Caption = '&Alternate Character Set'
        Enabled = False
        object AlternateCharacterSet1: TMenuItem
          AutoCheck = True
          Caption = '&Enable'
        end
        object LockCharacterSet1: TMenuItem
          AutoCheck = True
          Caption = 'Lock Character Set'
          OnClick = LockCharacterSet1Click
        end
        object N9: TMenuItem
          Caption = '-'
        end
        object UseAlternateCharacterSet1: TMenuItem
          AutoCheck = True
          Caption = 'Use Alternate Character Set'
          GroupIndex = 254
          RadioItem = True
          OnClick = UseAlternateCharacterSet1Click
        end
        object UseStandardCharacterSet1: TMenuItem
          AutoCheck = True
          Caption = 'Use Standard Character Set'
          Checked = True
          GroupIndex = 254
          RadioItem = True
          OnClick = UseStandardCharacterSet1Click
        end
      end
      object CompressedFont1: TMenuItem
        AutoCheck = True
        Caption = 'Compressed Font'
        Enabled = False
        OnClick = CompressedFont1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Settings1: TMenuItem
        Caption = '&Settings'
        OnClick = Options2Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
  object Open_Dialog: TOpenDialog
    DefaultExt = 'dll'
    Filter = 'Components|*.dll|All files|*.*'
    FilterIndex = 0
    Title = 'Connect to a component'
    Left = 323
    Top = 8
  end
  object Log_File_Dialog: TSaveDialog
    DefaultExt = 'bin'
    Filter = 'Binary files|*.bin|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Log to file'
    Left = 285
    Top = 43
  end
  object OpenPaperDialog: TOpenDialog
    DefaultExt = 'cef_m'
    Filter = 'CEF media file|*.cef_m|Text files|*.txt|All files|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofCreatePrompt, ofEnableSizing]
    Title = 'Paper file'
    Left = 319
    Top = 40
  end
  object Open_Tape_Dialog: TOpenDialog
    DefaultExt = 'txt'
    Filter = '*.txt|Text files|*.*|All files'
    FilterIndex = 0
    Title = 'Open paper tape file'
    Left = 322
    Top = 79
  end
end
