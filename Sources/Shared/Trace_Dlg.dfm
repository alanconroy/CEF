object Trace_Form: TTrace_Form
  Left = 480
  Top = 320
  HelpContext = 17
  Caption = 'Trace'
  ClientHeight = 523
  ClientWidth = 857
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
    Top = 490
    Width = 857
    Height = 33
    Align = alBottom
    TabOrder = 0
    object OK_button: TBitBtn
      Left = 16
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 321
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 617
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Main_Panel: TPanel
    Left = 0
    Top = 33
    Width = 857
    Height = 457
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Top_Panel: TPanel
    Left = 0
    Top = 0
    Width = 857
    Height = 33
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 11
      Top = 8
      Width = 121
      Height = 13
      Caption = 'Maximum events to trace:'
    end
    object Clear_All_Button: TSpeedButton
      Left = 319
      Top = 8
      Width = 53
      Height = 18
      Caption = 'Clear all'
      OnClick = Clear_All_ButtonClick
    end
    object Count: TSpinEdit
      Left = 141
      Top = 6
      Width = 77
      Height = 22
      MaxValue = 32767
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
  end
end
