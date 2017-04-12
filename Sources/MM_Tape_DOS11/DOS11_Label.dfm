object DOS11_Label_Form: TDOS11_Label_Form
  Left = 1440
  Top = 256
  Width = 260
  Height = 255
  Caption = 'DOS11 Label'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 10
    Top = 14
    Width = 21
    Height = 13
    Caption = 'Proj:'
  end
  object Label6: TLabel
    Left = 10
    Top = 43
    Width = 25
    Height = 13
    Caption = 'Prog:'
  end
  object Label7: TLabel
    Left = 10
    Top = 70
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label8: TLabel
    Left = 10
    Top = 93
    Width = 18
    Height = 13
    Caption = 'Ext:'
  end
  object Label9: TLabel
    Left = 10
    Top = 122
    Width = 22
    Height = 13
    Caption = 'Prot:'
  end
  object Label11: TLabel
    Left = 9
    Top = 149
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Proj: TSpinEdit
    Left = 38
    Top = 10
    Width = 60
    Height = 22
    MaxValue = 255
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object Prog: TSpinEdit
    Left = 39
    Top = 39
    Width = 60
    Height = 22
    MaxValue = 255
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object FileName: TEdit
    Left = 48
    Top = 66
    Width = 121
    Height = 21
    TabOrder = 2
    OnKeyPress = FileNameKeyPress
  end
  object Ext: TEdit
    Left = 49
    Top = 90
    Width = 66
    Height = 21
    TabOrder = 3
    OnKeyPress = ExtKeyPress
  end
  object Prot: TSpinEdit
    Left = 49
    Top = 115
    Width = 69
    Height = 22
    MaxValue = 155
    MinValue = 0
    TabOrder = 4
    Value = 155
  end
  object BitBtn1: TBitBtn
    Left = 11
    Top = 184
    Width = 75
    Height = 25
    TabOrder = 5
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 157
    Top = 184
    Width = 75
    Height = 25
    TabOrder = 6
    Kind = bkCancel
  end
  object Date_Value: TSpinEdit
    Left = 48
    Top = 144
    Width = 73
    Height = 22
    MaxValue = 65535
    MinValue = 0
    TabOrder = 7
    Value = 25001
  end
end
