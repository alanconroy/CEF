object Error_Dialog: TError_Dialog
  Left = 300
  Top = 205
  Width = 812
  Height = 481
  Caption = 'Errors'
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
    Top = 414
    Width = 804
    Height = 33
    Align = alBottom
    TabOrder = 0
    object Abort_Button: TBitBtn
      Left = 296
      Top = 7
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkClose
    end
  end
  object List_Box: TListBox
    Left = 0
    Top = 0
    Width = 804
    Height = 414
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = List_BoxDblClick
  end
end
