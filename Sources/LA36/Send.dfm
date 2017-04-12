object Send_Dialog: TSend_Dialog
  Left = 187
  Top = 132
  Caption = 'Sending file'
  ClientHeight = 192
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 9
    Width = 48
    Height = 13
    Caption = 'File name:'
  end
  object Status: TLabel
    Left = 5
    Top = 93
    Width = 108
    Height = 13
    Caption = '0 bytes of 0 transferred'
  end
  object Browse_Button: TSpeedButton
    Left = 310
    Top = 31
    Width = 42
    Height = 18
    Caption = 'Browse...'
    OnClick = Browse_ButtonClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 159
    Width = 435
    Height = 33
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 163
    ExplicitWidth = 443
    object Cancel_Button: TBitBtn
      Left = 265
      Top = 5
      Width = 61
      Height = 20
      TabOrder = 0
      OnClick = Cancel_ButtonClick
      Kind = bkCancel
    end
    object Transfer_Button: TBitBtn
      Left = 9
      Top = 7
      Width = 61
      Height = 20
      Caption = 'Transfer'
      TabOrder = 1
      OnClick = Transfer_ButtonClick
    end
  end
  object File_Name: TEdit
    Left = 62
    Top = 7
    Width = 291
    Height = 21
    TabOrder = 1
    OnChange = File_NameChange
  end
  object Updates: TCheckBox
    Left = 7
    Top = 35
    Width = 145
    Height = 14
    Caption = 'Update screen'
    TabOrder = 2
  end
  object Progress_Bar: TProgressBar
    Left = 6
    Top = 67
    Width = 346
    Height = 17
    TabOrder = 3
  end
  object Transfer_File_Dialog: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text file|*.txt|Binary file|*.bin|Data file|*.dat|All files|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Send a file'
    Left = 343
    Top = 41
  end
end
