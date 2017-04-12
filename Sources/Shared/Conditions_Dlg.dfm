object Conditions_Form: TConditions_Form
  Left = 1148
  Top = 539
  HelpContext = 3
  Caption = 'Conditions'
  ClientHeight = 462
  ClientWidth = 756
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
    Top = 428
    Width = 756
    Height = 34
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 429
    object OK_Button: TBitBtn
      Left = 25
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 280
      Top = 5
      Width = 60
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 508
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 756
    Height = 428
    ActivePage = States_Sheet
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 429
    object States_Sheet: TTabSheet
      Caption = 'States'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object Signals_Sheet: TTabSheet
      Caption = 'Signals'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object Exceptions_Sheet: TTabSheet
      Caption = 'Exceptions'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object Errors_Sheet: TTabSheet
      Caption = 'Errors'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CB_Hints: TCheckBox
        Left = 7
        Top = 3
        Width = 284
        Height = 14
        Caption = 'Break on informational messages'
        TabOrder = 0
        OnClick = CB_HintsClick
      end
      object CB_Warnings: TCheckBox
        Left = 7
        Top = 20
        Width = 284
        Height = 14
        Caption = 'Break on warnings'
        TabOrder = 1
        OnClick = CB_WarningsClick
      end
      object CB_Errors: TCheckBox
        Left = 7
        Top = 37
        Width = 284
        Height = 14
        Caption = 'Break on errros'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CB_ErrorsClick
      end
    end
  end
end
