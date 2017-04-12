object Trace_Log_Form: TTrace_Log_Form
  Left = 192
  Top = 128
  HelpContext = 18
  Caption = 'Trace log...'
  ClientHeight = 392
  ClientWidth = 654
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
    Top = 360
    Width = 654
    Height = 32
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 353
    ExplicitWidth = 646
    object Close_Button: TBitBtn
      Left = 17
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkClose
    end
    object Help_Button: TBitBtn
      Left = 431
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 1
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
    object Save_Button: TBitBtn
      Left = 224
      Top = 5
      Width = 61
      Height = 24
      Caption = 'Save...'
      TabOrder = 2
      OnClick = Save_ButtonClick
    end
  end
  object List_Box: TListBox
    Left = 0
    Top = 0
    Width = 654
    Height = 360
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    ItemHeight = 13
    TabOrder = 1
  end
  object Save_Dialog: TSaveDialog
    DefaultExt = 'log'
    Filter = 'Log files|*.log|All files|*.*'
    Title = 'Save Log to file'
    Left = 13
    Top = 17
  end
end
