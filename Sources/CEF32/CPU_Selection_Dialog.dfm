object CPU_Selection_Form: TCPU_Selection_Form
  Left = 0
  Top = 0
  Caption = 'Select Master CPU'
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
  object Panel1: TPanel
    Left = 0
    Top = 259
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitLeft = 338
    ExplicitTop = 247
    ExplicitWidth = 185
    object BitBtn1: TBitBtn
      Left = 61
      Top = 13
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 468
      Top = 12
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 635
    Height = 259
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
    ExplicitTop = 6
  end
end
