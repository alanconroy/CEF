object Here_Is_Form: THere_Is_Form
  Left = 0
  Top = 0
  Caption = 'Here Is'
  ClientHeight = 126
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 40
    Width = 46
    Height = 13
    Caption = 'Message:'
  end
  object CheckBox1: TCheckBox
    Left = 7
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Enabled'
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 64
    Top = 36
    Width = 552
    Height = 21
    TabOrder = 1
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 85
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitLeft = 191
    ExplicitTop = 260
    ExplicitWidth = 185
    object BitBtn1: TBitBtn
      Left = 39
      Top = 10
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 451
      Top = 7
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
