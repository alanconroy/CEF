object Tape_Add_Data_Dialog: TTape_Add_Data_Dialog
  Left = 1265
  Top = 224
  Caption = 'Add data to tape'
  ClientHeight = 437
  ClientWidth = 647
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
  object Button_Panel: TPanel
    Left = 0
    Top = 396
    Width = 647
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 389
    ExplicitWidth = 639
    object BitBtn1: TBitBtn
      Left = 82
      Top = 7
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 453
      Top = 7
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Top_Panel: TPanel
    Left = 0
    Top = 0
    Width = 647
    Height = 70
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 639
    object SpeedButton1: TSpeedButton
      Left = 109
      Top = 11
      Width = 83
      Height = 22
      Caption = 'Add from file...'
      OnClick = SpeedButton1Click
    end
    object Label1: TLabel
      Left = 3
      Top = 23
      Width = 85
      Height = 13
      Caption = 'Enter data to add:'
    end
    object Label2: TLabel
      Left = 236
      Top = 17
      Width = 51
      Height = 13
      Caption = 'Max bytes:'
    end
    object Label3: TLabel
      Left = 404
      Top = 17
      Width = 112
      Height = 13
      Caption = 'Max block size in bytes:'
    end
    object Label4: TLabel
      Left = 405
      Top = 42
      Width = 109
      Height = 13
      Caption = 'Min block size in bytes:'
    end
    object Max_Bytes: TSpinEdit
      Left = 292
      Top = 11
      Width = 84
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object Max_Block_Size: TSpinEdit
      Left = 516
      Top = 12
      Width = 114
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = Max_Block_SizeChange
    end
    object Min_Block_Size: TSpinEdit
      Left = 516
      Top = 39
      Width = 114
      Height = 22
      MaxValue = 65536
      MinValue = 1
      TabOrder = 2
      Value = 1
      OnChange = Min_Block_SizeChange
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 70
    Width = 647
    Height = 326
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 2
    ExplicitWidth = 639
    ExplicitHeight = 319
  end
  object OpenDialog1: TOpenDialog
    Title = 'Import data from file'
    Left = 142
    Top = 38
  end
end
