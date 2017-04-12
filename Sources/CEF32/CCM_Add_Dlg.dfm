object CCM_Add_Form: TCCM_Add_Form
  Left = 192
  Top = 128
  Width = 496
  Height = 458
  HelpContext = 2
  Caption = 'Add a component'
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
    Top = 390
    Width = 488
    Height = 34
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 11
      Top = 5
      Width = 61
      Height = 24
      Enabled = False
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 154
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 307
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object List_Box: TListBox
    Left = 0
    Top = 0
    Width = 488
    Height = 390
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
    OnClick = List_BoxClick
  end
end
