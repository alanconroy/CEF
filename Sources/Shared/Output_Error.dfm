object Output_Error_Form: TOutput_Error_Form
  Left = 360
  Top = 114
  Width = 384
  Height = 222
  BorderIcons = [biSystemMenu]
  Caption = 'Output_Error_Form'
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
    Width = 70
    Height = 13
    Caption = 'Message_Text'
    WordWrap = True
  end
  object BitBtn1: TBitBtn
    Left = 7
    Top = 145
    Width = 75
    Height = 25
    Caption = 'Skip'
    ModalResult = 5
    TabOrder = 0
  end
  object BitBtn2: TBitBtn
    Left = 97
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Skip All'
    ModalResult = 10
    TabOrder = 1
  end
  object BitBtn3: TBitBtn
    Left = 274
    Top = 143
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object BitBtn4: TBitBtn
    Left = 182
    Top = 145
    Width = 75
    Height = 25
    Caption = 'Retry'
    ModalResult = 4
    TabOrder = 3
  end
end
