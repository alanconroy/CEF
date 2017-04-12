object Modify_Register: TModify_Register
  Left = 192
  Top = 121
  Width = 496
  Height = 160
  HelpContext = 13
  ActiveControl = Edit1
  Caption = 'Modify Register'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 14
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 90
    Width = 480
    Height = 34
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 7
      Top = 4
      Width = 61
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 206
      Top = 4
      Width = 61
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 405
      Top = 4
      Width = 61
      Height = 25
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Edit1: TEdit
    Left = 46
    Top = 8
    Width = 259
    Height = 21
    TabOrder = 1
    OnChange = Edit1Change
  end
end
