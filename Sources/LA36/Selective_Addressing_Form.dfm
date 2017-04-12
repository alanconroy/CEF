object Selective_Addressing_Dialog: TSelective_Addressing_Dialog
  Left = 0
  Top = 0
  Caption = 'Selective Addressing'
  ClientHeight = 142
  ClientWidth = 500
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
    Top = 9
    Width = 49
    Height = 13
    Caption = 'Unit code:'
  end
  object Label2: TLabel
    Left = 9
    Top = 41
    Width = 59
    Height = 13
    Caption = 'Group code:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 101
    Width = 500
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitLeft = 354
    ExplicitTop = 202
    ExplicitWidth = 185
    object BitBtn1: TBitBtn
      Left = 69
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 362
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Unit_Code: TSpinEdit
    Left = 77
    Top = 6
    Width = 79
    Height = 22
    MaxValue = 127
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object Group_Code: TSpinEdit
    Left = 77
    Top = 36
    Width = 79
    Height = 22
    MaxValue = 127
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
end
