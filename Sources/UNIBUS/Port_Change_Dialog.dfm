object Port_Change_Form: TPort_Change_Form
  Left = 827
  Top = 383
  Caption = 'Change port configuration'
  ClientHeight = 236
  ClientWidth = 557
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
    Top = 195
    Width = 557
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 469
    ExplicitWidth = 574
    object BitBtn1: TBitBtn
      Left = 29
      Top = 12
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object Delete_Button: TBitBtn
      Left = 463
      Top = 12
      Width = 75
      Height = 25
      Caption = '&Delete'
      Enabled = False
      TabOrder = 1
      OnClick = Delete_ButtonClick
    end
    object Change_Button: TBitBtn
      Left = 322
      Top = 12
      Width = 75
      Height = 25
      Caption = '&Change...'
      Enabled = False
      TabOrder = 2
      OnClick = Change_ButtonClick
    end
    object BitBtn2: TBitBtn
      Left = 178
      Top = 12
      Width = 75
      Height = 25
      TabOrder = 3
      Kind = bkCancel
    end
  end
  object Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 557
    Height = 195
    Align = alClient
    ColCount = 2
    DefaultColWidth = 256
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 1
    OnClick = GridClick
    OnDblClick = GridDblClick
    ExplicitWidth = 574
    ExplicitHeight = 469
  end
  object Open_Component: TOpenDialog
    DefaultExt = 'dll'
    Filter = 'Components|*.dll'
    Options = [ofFileMustExist, ofEnableSizing]
    Title = 'Select terminal'
    Left = 526
    Top = 17
  end
end
