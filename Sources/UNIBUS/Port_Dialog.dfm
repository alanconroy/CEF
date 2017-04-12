object Port_Form: TPort_Form
  Left = 0
  Top = 109
  Width = 640
  Height = 341
  Caption = 'Devices'
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
    Top = 12
    Width = 48
    Height = 13
    Caption = 'DL11-Ws:'
  end
  object Label2: TLabel
    Left = 8
    Top = 44
    Width = 115
    Height = 13
    Caption = 'KL11/DL11-A/DL11-Bs:'
  end
  object Label3: TLabel
    Left = 8
    Top = 74
    Width = 86
    Height = 13
    Caption = 'DL11-C/DL11-Ds:'
  end
  object Label4: TLabel
    Left = 279
    Top = 16
    Width = 29
    Height = 13
    Caption = 'KE11:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 273
    Width = 632
    Height = 34
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 11
      Top = 7
      Width = 61
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 184
      Top = 7
      Width = 61
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Console: TSpinEdit
    Left = 127
    Top = 11
    Width = 50
    Height = 22
    MaxValue = 1
    MinValue = 0
    TabOrder = 1
    Value = 1
    OnChange = ConsoleChange
  end
  object KL11: TSpinEdit
    Left = 128
    Top = 39
    Width = 50
    Height = 22
    MaxValue = 15
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object DL11D: TSpinEdit
    Left = 129
    Top = 69
    Width = 50
    Height = 22
    MaxValue = 30
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object KE11: TSpinEdit
    Left = 319
    Top = 10
    Width = 50
    Height = 22
    MaxValue = 1
    MinValue = 0
    TabOrder = 4
    Value = 0
  end
  object Support_Switch_Register: TCheckBox
    Left = 409
    Top = 14
    Width = 176
    Height = 17
    Caption = 'Support Switch Register'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object Open_Dialog: TOpenDialog
    DefaultExt = 'dll'
    Filter = 'Components|*.dll|All files|*.*'
    FilterIndex = 0
    Title = 'Connect cable component to port'
    Left = 193
    Top = 540
  end
end
