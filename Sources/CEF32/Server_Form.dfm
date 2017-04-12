object Server_Dialog: TServer_Dialog
  Left = 0
  Top = 0
  Caption = 'Server Settings'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Panel: TPanel
    Left = 0
    Top = 259
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkClose
    end
    object BitBtn2: TBitBtn
      Left = 64
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Start Server'
      TabOrder = 1
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 635
    Height = 259
    Align = alClient
    TabOrder = 1
  end
end
