object Memory_Watchpoints_Dialog: TMemory_Watchpoints_Dialog
  Left = 913
  Top = 507
  HelpContext = 10
  ActiveControl = Grid
  Caption = 'Memory watchpoints'
  ClientHeight = 475
  ClientWidth = 751
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
    Top = 442
    Width = 751
    Height = 33
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 44
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 282
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 527
      Top = 5
      Width = 60
      Height = 24
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 751
    Height = 442
    Align = alClient
    ColCount = 4
    DefaultColWidth = 186
    FixedCols = 0
    RowCount = 2
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goHorzLine, goColSizing, goRowSelect]
    PopupMenu = PopupMenu1
    TabOrder = 1
    OnKeyUp = GridKeyUp
  end
  object PopupMenu1: TPopupMenu
    Left = 39
    Top = 85
    object Delete1: TMenuItem
      Caption = '&Delete'
      OnClick = Delete1Click
    end
    object DeleteAll1: TMenuItem
      Caption = 'De&lete All'
      OnClick = DeleteAll1Click
    end
    object Properties1: TMenuItem
      Caption = '&Properties...'
      OnClick = Properties1Click
    end
    object Add1: TMenuItem
      Caption = '&Add...'
      OnClick = Add1Click
    end
  end
end
