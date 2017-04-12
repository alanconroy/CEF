object VT_Form: TVT_Form
  Left = 0
  Top = 0
  Caption = 'Vertical Tab Positions'
  ClientHeight = 300
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
  object Button_Panel: TPanel
    Left = 0
    Top = 259
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitLeft = 369
    ExplicitTop = 239
    ExplicitWidth = 185
    object OK_Button: TBitBtn
      Left = 77
      Top = 10
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 420
      Top = 9
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 635
    Height = 259
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 1
    OnChange = Memo1Change
  end
end
