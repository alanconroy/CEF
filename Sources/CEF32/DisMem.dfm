object Save_Disassembly_Form: TSave_Disassembly_Form
  Left = 204
  Top = 122
  Width = 545
  Height = 208
  HelpContext = 5
  Caption = 'Save diassembly'
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
    Left = 7
    Top = 7
    Width = 76
    Height = 13
    Caption = 'Dump file name:'
  end
  object SpeedButton1: TSpeedButton
    Left = 339
    Top = 7
    Width = 88
    Height = 17
    Caption = 'Browse...'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 7
    Top = 40
    Width = 79
    Height = 13
    Caption = 'Starting address:'
  end
  object Label3: TLabel
    Left = 6
    Top = 67
    Width = 76
    Height = 13
    Caption = 'Ending address:'
  end
  object Label4: TLabel
    Left = 214
    Top = 38
    Width = 80
    Height = 13
    Caption = 'Number of bytes:'
  end
  object File_Name: TEdit
    Left = 92
    Top = 4
    Width = 240
    Height = 21
    TabOrder = 0
    OnChange = File_NameChange
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 140
    Width = 537
    Height = 34
    Align = alBottom
    TabOrder = 1
    object OK_Button: TBitBtn
      Left = 12
      Top = 3
      Width = 61
      Height = 25
      Enabled = False
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 171
      Top = 5
      Width = 61
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 340
      Top = 7
      Width = 61
      Height = 25
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Starting_Address: TEdit
    Left = 94
    Top = 35
    Width = 99
    Height = 21
    TabOrder = 2
    Text = '0'
    OnChange = Starting_AddressChange
  end
  object Ending_Address: TEdit
    Left = 94
    Top = 65
    Width = 99
    Height = 21
    TabOrder = 3
    Text = '0'
    OnChange = Ending_AddressChange
  end
  object Size: TEdit
    Left = 306
    Top = 36
    Width = 99
    Height = 21
    TabOrder = 4
    Text = '0'
    OnChange = SizeChange
  end
  object Show_Address: TCheckBox
    Left = 8
    Top = 96
    Width = 97
    Height = 17
    Caption = 'Show Addresses'
    TabOrder = 5
  end
  object Show_Values: TCheckBox
    Left = 8
    Top = 118
    Width = 97
    Height = 17
    Caption = 'Show Values'
    TabOrder = 6
  end
  object Save_Dialog: TSaveDialog
    DefaultExt = 'dmp'
    Filter = 'Text files|*.txt|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Disassembly file name'
    Left = 503
    Top = 42
  end
end
