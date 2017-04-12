object Edit_Value_Form: TEdit_Value_Form
  Left = 277
  Top = 215
  ActiveControl = Value_Edit
  Caption = 'Value editor'
  ClientHeight = 243
  ClientWidth = 652
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Panel: TPanel
    Left = 0
    Top = 210
    Width = 652
    Height = 33
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 16
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 276
      Top = 4
      Width = 61
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 652
    Height = 58
    Align = alTop
    TabOrder = 1
    object Instruction_Label: TLabel
      Left = 7
      Top = 8
      Width = 30
      Height = 13
      Caption = 'Value:'
    end
    object Value_Edit: TEdit
      Left = 7
      Top = 27
      Width = 611
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = Value_EditChange
      OnKeyPress = Value_EditKeyPress
    end
    object UpDown: TUpDown
      Left = 616
      Top = 24
      Width = 17
      Height = 26
      Position = 50
      TabOrder = 1
      OnChangingEx = UpDownChangingEx
    end
  end
  object Base_Panel: TPanel
    Left = 0
    Top = 58
    Width = 652
    Height = 152
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
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
      OnClick = Octal_RBClick
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
      OnClick = Decimal_RBClick
    end
    object Hexadecimal_RB: TRadioButton
      Left = 7
      Top = 62
      Width = 91
      Height = 14
      Caption = 'Hexadecimal'
      TabOrder = 3
      OnClick = Hexadecimal_RBClick
    end
    object Other_RB: TRadioButton
      Left = 7
      Top = 80
      Width = 57
      Height = 13
      Caption = 'Other:'
      Enabled = False
      TabOrder = 4
    end
    object Base_Spin: TSpinEdit
      Left = 66
      Top = 76
      Width = 37
      Height = 22
      MaxValue = 49
      MinValue = 2
      TabOrder = 5
      Value = 10
      OnChange = Base_SpinChange
    end
    object Grid: TStringGrid
      Left = 298
      Top = 26
      Width = 320
      Height = 120
      ColCount = 9
      DefaultColWidth = 32
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
      TabOrder = 6
      OnKeyPress = GridKeyPress
      OnSetEditText = GridSetEditText
    end
  end
end
