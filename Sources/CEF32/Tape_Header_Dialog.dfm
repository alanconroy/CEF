object Tape_Header: TTape_Header
  Left = 1571
  Top = 342
  Width = 321
  Height = 355
  Caption = 'Tape Media Information'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 6
    Width = 35
    Height = 13
    Caption = 'Format:'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 17
    Height = 13
    Caption = 'BPI'
  end
  object Label3: TLabel
    Left = 128
    Top = 32
    Width = 71
    Height = 13
    Caption = '(0 = undefined)'
  end
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 36
    Height = 13
    Caption = 'Length:'
  end
  object Label5: TLabel
    Left = 150
    Top = 57
    Width = 54
    Height = 13
    Caption = '(0 = infinite)'
  end
  object Label6: TLabel
    Left = 127
    Top = 56
    Width = 18
    Height = 13
    Caption = 'feet'
  end
  object SpeedButton1: TSpeedButton
    Left = 239
    Top = 32
    Width = 47
    Height = 22
    Caption = 'Defaults'
    OnClick = SpeedButton1Click
  end
  object Label9: TLabel
    Left = 8
    Top = 87
    Width = 54
    Height = 13
    Caption = 'IRG length:'
  end
  object Label10: TLabel
    Left = 8
    Top = 112
    Width = 51
    Height = 13
    Caption = 'TM length:'
  end
  object Label11: TLabel
    Left = 143
    Top = 83
    Width = 46
    Height = 13
    Caption = '(1/1000")'
  end
  object Label12: TLabel
    Left = 135
    Top = 109
    Width = 46
    Height = 13
    Caption = '(1/1000")'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 276
    Width = 305
    Height = 41
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 23
      Top = 10
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 197
      Top = 9
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Internal_Format: TGroupBox
    Left = 8
    Top = 141
    Width = 278
    Height = 109
    Caption = 'Internal format'
    TabOrder = 1
    object Label7: TLabel
      Left = 4
      Top = 15
      Width = 264
      Height = 47
      AutoSize = False
      Caption = 
        'Note: changing these settings affects the internal media file da' +
        'ta structure and cannot be changed once the tape contains any da' +
        'ta.'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 6
      Top = 66
      Width = 88
      Height = 13
      Caption = 'Size record length:'
    end
    object Include_After: TCheckBox
      Left = 5
      Top = 84
      Width = 258
      Height = 17
      Caption = 'Include size records before AND after records'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object Size_Record: TSpinEdit
      Left = 98
      Top = 63
      Width = 46
      Height = 22
      MaxValue = 8
      MinValue = 1
      TabOrder = 1
      Value = 4
    end
  end
  object BPI: TSpinEdit
    Left = 37
    Top = 28
    Width = 87
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object Length: TSpinEdit
    Left = 47
    Top = 52
    Width = 78
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object Format: TEdit
    Left = 47
    Top = 2
    Width = 241
    Height = 21
    TabOrder = 4
    Text = 'DOS-11 Tape'
  end
  object IRG_Length: TSpinEdit
    Left = 64
    Top = 81
    Width = 72
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 0
  end
  object TM_Length: TSpinEdit
    Left = 62
    Top = 107
    Width = 69
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 0
  end
end
