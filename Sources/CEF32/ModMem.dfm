object Modify_Memory_Dialog: TModify_Memory_Dialog
  Left = 391
  Top = 406
  Width = 745
  Height = 262
  HelpContext = 12
  Caption = 'Modify Memory'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 7
    Top = 7
    Width = 41
    Height = 13
    Caption = 'Address:'
  end
  object Label2: TLabel
    Left = 7
    Top = 40
    Width = 26
    Height = 13
    Caption = 'Data:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 195
    Width = 737
    Height = 33
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 12
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 269
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 1
      Kind = bkCancel
    end
    object BitBtn3: TBitBtn
      Left = 529
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 2
      OnClick = BitBtn3Click
      Kind = bkHelp
    end
  end
  object Address: TEdit
    Left = 59
    Top = 3
    Width = 513
    Height = 24
    TabOrder = 1
    Text = '0'
    OnChange = AddressChange
  end
  object Data: TEdit
    Left = 58
    Top = 36
    Width = 513
    Height = 24
    TabOrder = 2
    OnChange = AddressChange
  end
  object Literal_Text: TCheckBox
    Left = 7
    Top = 72
    Width = 567
    Height = 14
    Caption = 'Literal text'
    TabOrder = 3
    OnClick = AddressChange
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 93
    Width = 564
    Height = 43
    Caption = 'Size'
    TabOrder = 4
    object Byte_RB: TRadioButton
      Left = 8
      Top = 15
      Width = 92
      Height = 13
      Caption = 'Byte'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object Word_RB: TRadioButton
      Left = 114
      Top = 13
      Width = 92
      Height = 14
      Caption = 'Word'
      TabOrder = 1
    end
    object Long_RB: TRadioButton
      Left = 231
      Top = 14
      Width = 92
      Height = 14
      Caption = 'Long'
      TabOrder = 2
    end
  end
end
