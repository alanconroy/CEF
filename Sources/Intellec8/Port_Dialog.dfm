object Port_Form: TPort_Form
  Left = 960
  Top = 682
  Width = 640
  Height = 480
  Caption = 'Port assignments'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Panel: TPanel
    Left = 0
    Top = 412
    Width = 632
    Height = 34
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 11
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 184
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Port_Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 632
    Height = 364
    Align = alClient
    ColCount = 3
    RowCount = 257
    ScrollBars = ssVertical
    TabOrder = 1
    OnClick = Port_GridClick
    ColWidths = (
      102
      254
      248)
  end
  object Panel1: TPanel
    Left = 0
    Top = 364
    Width = 632
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Delete_Button: TSpeedButton
      Left = 11
      Top = 13
      Width = 61
      Height = 18
      Caption = 'Delete'
      Enabled = False
      OnClick = Delete_ButtonClick
    end
    object Add_Button: TSpeedButton
      Left = 310
      Top = 15
      Width = 104
      Height = 18
      Caption = 'Add Serial I/O'
      OnClick = Add_ButtonClick
    end
    object Connect_Button: TSpeedButton
      Left = 83
      Top = 13
      Width = 60
      Height = 18
      Caption = 'Connect'
      Enabled = False
      OnClick = Connect_ButtonClick
    end
    object Disconnect_Button: TSpeedButton
      Left = 154
      Top = 13
      Width = 61
      Height = 18
      Caption = 'Disconnect'
      Enabled = False
      OnClick = Disconnect_ButtonClick
    end
  end
  object Open_Dialog: TOpenDialog
    DefaultExt = 'dll'
    Filter = 'Components|*.dll|All files|*.*'
    FilterIndex = 0
    Title = 'Connect cable component to port'
    Left = 193
    Top = 540
  end
end
