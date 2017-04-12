object Radix_Form: TRadix_Form
  Left = 669
  Top = 294
  Width = 426
  Height = 134
  HelpContext = 16
  ActiveControl = Spin_Edit
  Caption = 'Choose radix'
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
    Top = 7
    Width = 202
    Height = 13
    Caption = 'Choose the radix for numeric display (2-49):'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 67
    Width = 418
    Height = 33
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 11
      Top = 6
      Width = 61
      Height = 20
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 140
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 266
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Spin_Edit: TSpinEdit
    Left = 223
    Top = 7
    Width = 98
    Height = 26
    MaxValue = 49
    MinValue = 2
    TabOrder = 1
    Value = 2
  end
end
