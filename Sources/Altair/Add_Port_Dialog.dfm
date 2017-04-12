object Add_Port_Form: TAdd_Port_Form
  Left = 480
  Top = 320
  Width = 279
  Height = 172
  Caption = 'Add port'
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
    Width = 125
    Height = 13
    Caption = 'Number of devices to add:'
  end
  object Label2: TLabel
    Left = 7
    Top = 40
    Width = 101
    Height = 13
    Caption = 'Starting physical port:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 105
    Width = 271
    Height = 33
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 17
      Top = 7
      Width = 61
      Height = 21
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 133
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Count: TSpinEdit
    Left = 144
    Top = 5
    Width = 49
    Height = 26
    MaxValue = 127
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object Start: TSpinEdit
    Left = 118
    Top = 37
    Width = 49
    Height = 26
    MaxValue = 252
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
end
