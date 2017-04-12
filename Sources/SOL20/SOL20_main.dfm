object Main_Form: TMain_Form
  Left = 591
  Top = 410
  Caption = 'Processor Technology SOL-20'
  ClientHeight = 714
  ClientWidth = 993
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Screen_Panel: TPanel
    Left = 0
    Top = 0
    Width = 993
    Height = 466
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Screen_PanelResize
    ExplicitWidth = 1001
    ExplicitHeight = 450
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 695
    Width = 993
    Height = 19
    Panels = <>
    ExplicitTop = 679
    ExplicitWidth = 1001
  end
  object Bottom_Panel: TPanel
    Left = 0
    Top = 466
    Width = 993
    Height = 229
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 450
    ExplicitWidth = 1001
    object Panel2: TPanel
      Left = 0
      Top = 33
      Width = 1001
      Height = 196
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 1001
      Height = 33
      Align = alTop
      TabOrder = 1
      object Label1: TLabel
        Left = 7
        Top = 8
        Width = 79
        Height = 13
        Caption = 'Sense Switches:'
      end
      object SpeedButton1: TSpeedButton
        Left = 91
        Top = 7
        Width = 8
        Height = 17
        AllowAllUp = True
        GroupIndex = 111
        Caption = '1'
      end
      object SpeedButton2: TSpeedButton
        Left = 104
        Top = 7
        Width = 8
        Height = 17
        AllowAllUp = True
        GroupIndex = 112
        Caption = '2'
      end
      object SpeedButton3: TSpeedButton
        Left = 117
        Top = 7
        Width = 8
        Height = 17
        AllowAllUp = True
        GroupIndex = 113
        Caption = '3'
      end
      object SpeedButton4: TSpeedButton
        Left = 130
        Top = 7
        Width = 8
        Height = 17
        AllowAllUp = True
        GroupIndex = 114
        Caption = '4'
      end
      object SpeedButton5: TSpeedButton
        Left = 143
        Top = 7
        Width = 8
        Height = 17
        AllowAllUp = True
        GroupIndex = 115
        Caption = '5'
      end
      object SpeedButton6: TSpeedButton
        Left = 156
        Top = 7
        Width = 8
        Height = 17
        AllowAllUp = True
        GroupIndex = 116
        Caption = '6'
      end
      object SpeedButton7: TSpeedButton
        Left = 169
        Top = 7
        Width = 8
        Height = 17
        AllowAllUp = True
        GroupIndex = 117
        Caption = '7'
      end
      object SpeedButton8: TSpeedButton
        Left = 182
        Top = 7
        Width = 8
        Height = 17
        AllowAllUp = True
        GroupIndex = 118
        Caption = '8'
      end
      object Label2: TLabel
        Left = 263
        Top = 11
        Width = 31
        Height = 13
        Caption = 'Buffer:'
      end
      object SpeedButton75: TSpeedButton
        Left = 782
        Top = 7
        Width = 31
        Height = 17
        Caption = 'Clear'
        OnClick = SpeedButton75Click
      end
      object Buffer: TEdit
        Left = 299
        Top = 7
        Width = 482
        Height = 21
        TabOrder = 0
        OnKeyDown = BufferKeyDown
        OnKeyPress = BufferKeyPress
        OnKeyUp = BufferKeyUp
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 283
    Top = 5
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Copyscreen1: TMenuItem
        Caption = '&Copy screen'
        OnClick = Copyscreen1Click
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
        OnClick = Paste1Click
      end
    end
    object SerialPort1: TMenuItem
      Caption = '&Serial Port'
      object Baudrate1: TMenuItem
        Caption = '&Baud rate'
        object N751: TMenuItem
          Caption = '&75'
          OnClick = Baud_Click
        end
        object N1101: TMenuItem
          Caption = '&110'
          Checked = True
          OnClick = Baud_Click
        end
        object N1501: TMenuItem
          Caption = '1&50'
          OnClick = Baud_Click
        end
        object N3001: TMenuItem
          Caption = '&300'
          RadioItem = True
          OnClick = Baud_Click
        end
        object N6001: TMenuItem
          Caption = '&600'
          OnClick = Baud_Click
        end
        object N12001: TMenuItem
          Caption = '1&200'
          OnClick = Baud_Click
        end
        object N24001: TMenuItem
          Caption = '2&400'
          OnClick = Baud_Click
        end
        object N48001: TMenuItem
          Caption = '4&800'
          OnClick = Baud_Click
        end
        object N96001: TMenuItem
          Caption = '&9600'
          OnClick = Baud_Click
        end
      end
      object Full1: TMenuItem
        Caption = '&Full duplex'
        OnClick = Check_Click
      end
      object N8databits1: TMenuItem
        Caption = '&Data bits'
        object N51: TMenuItem
          Caption = '&5'
          OnClick = Bits_Check
        end
        object N61: TMenuItem
          Caption = '&6'
          OnClick = Bits_Check
        end
        object N71: TMenuItem
          Caption = '&7'
          OnClick = Bits_Check
        end
        object N81: TMenuItem
          Caption = '&8'
          Checked = True
          OnClick = Bits_Check
        end
      end
      object N2stopbits1: TMenuItem
        Caption = '&2 stop bits'
        OnClick = Check_Click
      end
      object Parity1: TMenuItem
        Caption = '&Parity'
        OnClick = Check_Click
      end
      object Connect1: TMenuItem
        Caption = '&Connect...'
        OnClick = Connect1Click
      end
    end
    object Video1: TMenuItem
      Caption = '&Video'
      object Cursor1: TMenuItem
        Caption = '&Cursor'
        object None1: TMenuItem
          Caption = 'None'
          GroupIndex = 1
          RadioItem = True
          OnClick = None1Click
        end
        object Blinking1: TMenuItem
          Caption = 'Blinking'
          GroupIndex = 1
          RadioItem = True
          OnClick = Blinking1Click
        end
        object Solid1: TMenuItem
          Caption = 'Solid'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = Solid1Click
        end
      end
      object Display1: TMenuItem
        Caption = '&Reverse video'
        OnClick = Display1Click
      end
      object DisplayControlCharacters1: TMenuItem
        Caption = 'Display Control Characters'
        Checked = True
        OnClick = DisplayControlCharacters1Click
      end
      object Blanking1: TMenuItem
        Caption = 'CR and VT Blanking'
        OnClick = Blanking1Click
      end
    end
    object Cassette1: TMenuItem
      Caption = '&Cassette'
      object Drive11: TMenuItem
        Caption = 'Transport &1'
        object Tape1_File_Menu: TMenuItem
          Caption = '&File...'
          OnClick = Tape_File
        end
        object Tape1_Unload: TMenuItem
          Caption = '&Unload'
          OnClick = Tape_Unload
        end
      end
      object ransport21: TMenuItem
        Caption = 'Transport 2'
        object Tape2_File_Menu: TMenuItem
          Caption = '&File...'
          OnClick = Tape_File
        end
        object Tape2_Unload: TMenuItem
          Caption = '&Unload'
          OnClick = Tape_Unload
        end
      end
    end
    object ParallelPort1: TMenuItem
      Caption = '&Parallel Port'
      OnClick = ParallelPort1Click
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object Bufferkeystrokes1: TMenuItem
        Caption = 'Buffer keystrokes'
        Checked = True
        OnClick = Check_Click
      end
      object Keymapping1: TMenuItem
        Caption = 'Key mapping...'
        OnClick = Keymapping1Click
      end
      object Debug1: TMenuItem
        Caption = '&Debug'
        OnClick = Debug1Click
      end
      object Keyboard1: TMenuItem
        Caption = 'Keyboard'
        Checked = True
        OnClick = Keyboard1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Reset1: TMenuItem
        Caption = 'Reset'
        OnClick = Reset1Click
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
  object OpenDialog1: TOpenDialog
    DefaultExt = 'tap'
    Filter = 'Tape file|*.tap|All files|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Connect cassette to file'
    Left = 334
    Top = 6
  end
  object Open_Component: TOpenDialog
    DefaultExt = 'dll'
    Filter = 'Components|*.dll|All files|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 339
    Top = 41
  end
end
