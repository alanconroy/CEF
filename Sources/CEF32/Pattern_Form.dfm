object Pattern_Dialog: TPattern_Dialog
  Left = 367
  Top = 128
  Caption = '{Write pattern'
  ClientHeight = 330
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 7
    Width = 65
    Height = 13
    Caption = 'Start address:'
  end
  object Label2: TLabel
    Left = 195
    Top = 7
    Width = 62
    Height = 13
    Caption = 'End address:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 297
    Width = 472
    Height = 33
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 302
    ExplicitWidth = 480
    object OK_Button: TBitBtn
      Left = 25
      Top = 5
      Width = 61
      Height = 20
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 165
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Start_Address: TEdit
    Left = 80
    Top = 6
    Width = 99
    Height = 24
    TabOrder = 1
    Text = '0'
    OnChange = Start_AddressChange
    OnKeyPress = Start_AddressKeyPress
  end
  object End_Address: TEdit
    Left = 267
    Top = 6
    Width = 98
    Height = 24
    TabOrder = 2
    Text = '0'
    OnChange = Start_AddressChange
    OnKeyPress = End_AddressKeyPress
  end
  object Pattern: TGroupBox
    Left = 7
    Top = 37
    Width = 364
    Height = 86
    Caption = 'Pattern'
    TabOrder = 3
    object Label3: TLabel
      Left = 7
      Top = 19
      Width = 23
      Height = 13
      Caption = 'Size:'
    end
    object Label4: TLabel
      Left = 128
      Top = 19
      Width = 41
      Height = 13
      Caption = 'Value(s):'
    end
    object Size: TComboBox
      Left = 40
      Top = 17
      Width = 78
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '1'
      Items.Strings = (
        '1'
        '2'
        '4'
        '8')
    end
    object Values: TMemo
      Left = 171
      Top = 18
      Width = 150
      Height = 58
      Lines.Strings = (
        '0')
      ScrollBars = ssVertical
      TabOrder = 1
      OnKeyPress = ValuesKeyPress
    end
    object Increment_RB: TRadioButton
      Left = 7
      Top = 44
      Width = 91
      Height = 14
      Caption = 'Increment'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object Values_RB: TRadioButton
      Left = 7
      Top = 63
      Width = 91
      Height = 14
      Caption = 'Values'
      TabOrder = 3
    end
  end
  object Radix: TGroupBox
    Left = 7
    Top = 130
    Width = 364
    Height = 90
    Caption = 'Radix'
    TabOrder = 4
    object Binary: TRadioButton
      Left = 7
      Top = 18
      Width = 91
      Height = 14
      Caption = 'Binary'
      TabOrder = 0
      OnClick = BinaryClick
    end
    object Octal: TRadioButton
      Left = 7
      Top = 34
      Width = 91
      Height = 14
      Caption = 'Octal'
      TabOrder = 1
      OnClick = OctalClick
    end
    object Decimal: TRadioButton
      Left = 7
      Top = 51
      Width = 91
      Height = 14
      Caption = 'Decimal'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = DecimalClick
    end
    object Hexadecimal: TRadioButton
      Left = 7
      Top = 67
      Width = 91
      Height = 14
      Caption = 'Hexadecimal'
      TabOrder = 3
      OnClick = HexadecimalClick
    end
    object Other: TRadioButton
      Left = 211
      Top = 13
      Width = 92
      Height = 14
      Caption = 'Other:'
      TabOrder = 4
      OnClick = OtherClick
    end
    object Base: TSpinEdit
      Left = 256
      Top = 28
      Width = 98
      Height = 26
      Enabled = False
      MaxValue = 49
      MinValue = 2
      TabOrder = 5
      Value = 2
      OnChange = BaseChange
    end
    object ASCII: TRadioButton
      Left = 211
      Top = 51
      Width = 92
      Height = 14
      Caption = 'ASCII'
      TabOrder = 6
      OnClick = ASCIIClick
    end
    object EBCDIC: TRadioButton
      Left = 211
      Top = 67
      Width = 92
      Height = 14
      Caption = 'EBCDIC'
      TabOrder = 7
      OnClick = EBCDICClick
    end
  end
end
