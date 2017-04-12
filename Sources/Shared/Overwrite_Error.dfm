object Overwrite_Form: TOverwrite_Form
  Left = 360
  Top = 114
  Width = 384
  Height = 173
  BorderIcons = [biSystemMenu]
  Caption = 'Overwrite_Form'
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
  object Message_Text: TLabel
    Left = 9
    Top = 9
    Width = 82
    Height = 13
    Alignment = taCenter
    Caption = 'File already exists'
    WordWrap = True
  end
  object BitBtn1: TBitBtn
    Left = 4
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Skip'
    TabOrder = 0
  end
  object BitBtn2: TBitBtn
    Left = 94
    Top = 54
    Width = 75
    Height = 25
    Caption = 'Skip All'
    ModalResult = 9
    TabOrder = 1
  end
  object BitBtn3: TBitBtn
    Left = 271
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object BitBtn4: TBitBtn
    Left = 5
    Top = 90
    Width = 75
    Height = 25
    Caption = 'Overwrite'
    ModalResult = 6
    TabOrder = 3
  end
  object BitBtn5: TBitBtn
    Left = 95
    Top = 89
    Width = 75
    Height = 25
    Caption = 'Overwrite All'
    ModalResult = 10
    TabOrder = 4
  end
end
