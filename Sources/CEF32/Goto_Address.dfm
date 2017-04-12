object Goto_Address_Form: TGoto_Address_Form
  Left = 274
  Top = 120
  HelpContext = 7
  ActiveControl = ComboBox1
  Caption = 'Goto Address'
  ClientHeight = 91
  ClientWidth = 341
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
    Left = 7
    Top = 7
    Width = 91
    Height = 13
    Caption = 'Enter new address:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 58
    Width = 341
    Height = 33
    Align = alBottom
    TabOrder = 1
    object OK_Button: TBitBtn
      Left = 10
      Top = 4
      Width = 61
      Height = 25
      TabOrder = 0
      OnClick = OK_ButtonClick
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 111
      Top = 4
      Width = 61
      Height = 25
      TabOrder = 1
      Kind = bkAbort
    end
    object Help_Button: TBitBtn
      Left = 209
      Top = 4
      Width = 61
      Height = 25
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object ComboBox1: TComboBox
    Left = 107
    Top = 5
    Width = 139
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnKeyPress = ComboBox1KeyPress
  end
end
