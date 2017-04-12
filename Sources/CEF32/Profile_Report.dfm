object Profile_Report_Form: TProfile_Report_Form
  Left = 192
  Top = 132
  HelpContext = 15
  Caption = 'Profile Report'
  ClientHeight = 435
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Panel: TPanel
    Left = 0
    Top = 402
    Width = 780
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Close_Button: TBitBtn
      Left = 16
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkClose
    end
    object Help_Button: TBitBtn
      Left = 553
      Top = 6
      Width = 61
      Height = 24
      TabOrder = 1
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
    object Button_Clear: TBitBtn
      Left = 174
      Top = 5
      Width = 61
      Height = 24
      Caption = 'Clear'
      TabOrder = 2
      OnClick = Button_ClearClick
    end
    object Button_Clear_All: TBitBtn
      Left = 370
      Top = 5
      Width = 61
      Height = 24
      Caption = 'Clear All'
      TabOrder = 3
      OnClick = Button_Clear_AllClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 780
    Height = 402
    Align = alClient
    TabOrder = 1
  end
end
