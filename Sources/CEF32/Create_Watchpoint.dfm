object New_Watchpoint: TNew_Watchpoint
  Left = 1217
  Top = 908
  Width = 383
  Height = 254
  HelpContext = 4
  ActiveControl = Address
  Caption = 'Create Memory Watchpoint'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 41
    Height = 13
    Caption = 'Address:'
  end
  object Label2: TLabel
    Left = 8
    Top = 34
    Width = 23
    Height = 13
    Caption = 'Size:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 187
    Width = 375
    Height = 33
    Align = alBottom
    TabOrder = 0
    OnResize = Button_PanelResize
    object OK_Button: TBitBtn
      Left = 15
      Top = 7
      Width = 61
      Height = 24
      Enabled = False
      TabOrder = 0
      OnClick = OK_ButtonClick
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 122
      Top = 7
      Width = 61
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 229
      Top = 7
      Width = 61
      Height = 24
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Address: TEdit
    Left = 59
    Top = 4
    Width = 234
    Height = 24
    TabOrder = 1
    OnChange = AddressChange
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 58
    Width = 289
    Height = 64
    Caption = 'Type'
    TabOrder = 2
    object Read: TCheckBox
      Left = 7
      Top = 20
      Width = 78
      Height = 14
      Caption = 'Read'
      TabOrder = 0
      OnClick = AddressChange
    end
    object Write: TCheckBox
      Left = 7
      Top = 39
      Width = 78
      Height = 14
      Caption = 'Write'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = AddressChange
    end
  end
  object Size: TSpinEdit
    Left = 59
    Top = 31
    Width = 98
    Height = 26
    MaxValue = 2147483647
    MinValue = 1
    TabOrder = 3
    Value = 1
  end
end
