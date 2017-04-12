object New_Watch_Dialog: TNew_Watch_Dialog
  Left = 375
  Top = 234
  HelpContext = 14
  ActiveControl = Address_Value
  Caption = 'Enter watch'
  ClientHeight = 282
  ClientWidth = 475
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Panel: TPanel
    Left = 0
    Top = 249
    Width = 475
    Height = 33
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 254
    ExplicitWidth = 483
    object OK_Button: TBitBtn
      Left = 11
      Top = 9
      Width = 61
      Height = 20
      Enabled = False
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 156
      Top = 7
      Width = 61
      Height = 21
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 319
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 475
    Height = 73
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 483
    object Label1: TLabel
      Left = 5
      Top = 7
      Width = 85
      Height = 13
      Caption = 'Address to watch:'
    end
    object Label2: TLabel
      Left = 5
      Top = 33
      Width = 23
      Height = 13
      Caption = 'Size:'
    end
    object Label3: TLabel
      Left = 5
      Top = 58
      Width = 51
      Height = 13
      Caption = 'Display as:'
    end
    object Address_Value: TEdit
      Left = 99
      Top = 5
      Width = 233
      Height = 24
      TabOrder = 0
      OnChange = Address_ValueChange
      OnKeyDown = Address_ValueKeyDown
    end
    object Size: TSpinEdit
      Left = 47
      Top = 30
      Width = 98
      Height = 26
      MaxValue = 65535
      MinValue = 1
      TabOrder = 1
      Value = 1
    end
  end
  object Base_Panel: TPanel
    Left = 0
    Top = 73
    Width = 475
    Height = 176
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 483
    ExplicitHeight = 181
    object Binary_RB: TRadioButton
      Left = 7
      Top = 8
      Width = 91
      Height = 14
      Caption = 'Binary'
      TabOrder = 0
      OnClick = Binary_RBClick
    end
    object Octal_RB: TRadioButton
      Left = 7
      Top = 26
      Width = 91
      Height = 14
      Caption = 'Octal'
      TabOrder = 1
      OnClick = Binary_RBClick
    end
    object Decimal_RB: TRadioButton
      Left = 7
      Top = 44
      Width = 91
      Height = 14
      Caption = 'Decimal'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = Binary_RBClick
    end
    object Hexadecimal_RB: TRadioButton
      Left = 7
      Top = 62
      Width = 91
      Height = 14
      Caption = 'Hexadecimal'
      TabOrder = 3
      OnClick = Binary_RBClick
    end
    object Other_RB: TRadioButton
      Left = 7
      Top = 80
      Width = 57
      Height = 13
      Caption = 'Other:'
      Enabled = False
      TabOrder = 4
      OnClick = Binary_RBClick
    end
    object Base_Spin: TSpinEdit
      Left = 66
      Top = 76
      Width = 37
      Height = 26
      MaxValue = 49
      MinValue = 2
      TabOrder = 5
      Value = 10
    end
    object ASCII_RB: TRadioButton
      Left = 260
      Top = 8
      Width = 92
      Height = 14
      Caption = 'ASCII'
      TabOrder = 6
      OnClick = Binary_RBClick
    end
    object EBCDIC_RB: TRadioButton
      Left = 260
      Top = 26
      Width = 92
      Height = 14
      Caption = 'EBCDIC'
      TabOrder = 7
      OnClick = Binary_RBClick
    end
    object Radix50_RB: TRadioButton
      Left = 260
      Top = 44
      Width = 92
      Height = 14
      Caption = 'Radix 50'
      TabOrder = 8
      OnClick = Binary_RBClick
    end
    object Single_RB: TRadioButton
      Left = 260
      Top = 62
      Width = 92
      Height = 14
      Caption = 'Single'
      TabOrder = 9
      OnClick = Single_RBClick
    end
    object Double_RB: TRadioButton
      Left = 260
      Top = 80
      Width = 92
      Height = 13
      Caption = 'Double'
      TabOrder = 10
      OnClick = Double_RBClick
    end
    object Extended_RB: TRadioButton
      Left = 260
      Top = 98
      Width = 92
      Height = 13
      Caption = 'Extended'
      TabOrder = 11
      OnClick = Extended_RBClick
    end
  end
end
