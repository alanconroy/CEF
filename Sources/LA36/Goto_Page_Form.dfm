object Goto_Page_Dialog: TGoto_Page_Dialog
  Left = 0
  Top = 0
  Caption = 'Goto page...'
  ClientHeight = 142
  ClientWidth = 375
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
    Left = 17
    Top = 24
    Width = 69
    Height = 13
    Caption = 'Page to view: '
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 101
    Width = 375
    Height = 41
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 34
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 238
      Top = 7
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object SpinEdit1: TSpinEdit
    Left = 92
    Top = 21
    Width = 121
    Height = 22
    MaxValue = 2147483647
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
end
