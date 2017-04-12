object Port_IO_Dialog: TPort_IO_Dialog
  Left = 1257
  Top = 383
  Width = 423
  Height = 160
  Caption = 'Port I/O'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 10
    Width = 22
    Height = 13
    Caption = 'Port:'
  end
  object Label2: TLabel
    Left = 8
    Top = 39
    Width = 48
    Height = 13
    Caption = 'Size (bits):'
  end
  object Label3: TLabel
    Left = 176
    Top = 12
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 81
    Width = 407
    Height = 41
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 36
      Top = 6
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 321
      Top = 5
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Port: TEdit
    Left = 39
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '0'
    OnChange = PortChange
  end
  object Size: TSpinEdit
    Left = 66
    Top = 36
    Width = 92
    Height = 22
    MaxValue = 64
    MinValue = 1
    TabOrder = 2
    Value = 8
  end
  object Value: TEdit
    Left = 214
    Top = 7
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '0'
    OnChange = PortChange
  end
end
