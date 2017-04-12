object Find_Form: TFind_Form
  Left = 548
  Top = 240
  Width = 616
  Height = 244
  HelpContext = 6
  ActiveControl = Edit1
  Caption = 'Find data'
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
    Left = 5
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Search for:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 176
    Width = 608
    Height = 34
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 10
      Top = 7
      Width = 61
      Height = 21
      Enabled = False
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 216
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 417
      Top = 6
      Width = 61
      Height = 20
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Edit1: TEdit
    Left = 65
    Top = 5
    Width = 421
    Height = 24
    TabOrder = 1
    OnChange = Edit1Change
    OnKeyPress = Edit1KeyPress
  end
  object Options: TGroupBox
    Left = 7
    Top = 38
    Width = 480
    Height = 86
    Caption = 'Search options'
    TabOrder = 2
    object Bytes: TRadioButton
      Left = 9
      Top = 16
      Width = 92
      Height = 14
      Caption = 'Bytes'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = BytesClick
    end
    object Literal_Text: TRadioButton
      Left = 8
      Top = 37
      Width = 92
      Height = 13
      Caption = 'Literal text'
      TabOrder = 1
      OnClick = Literal_TextClick
    end
    object Case_Sensitive: TCheckBox
      Left = 9
      Top = 59
      Width = 231
      Height = 13
      Caption = 'Case sensitive'
      Enabled = False
      TabOrder = 2
    end
    object Little_Endian: TCheckBox
      Left = 245
      Top = 56
      Width = 225
      Height = 14
      Caption = 'Little endian'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
end
