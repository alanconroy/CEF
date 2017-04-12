object New_Font_Dialog: TNew_Font_Dialog
  Left = 0
  Top = 0
  Caption = 'New font options'
  ClientHeight = 161
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 32
    Height = 13
    Caption = 'Width:'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 35
    Height = 13
    Caption = 'Height:'
  end
  object Label3: TLabel
    Left = 8
    Top = 70
    Width = 48
    Height = 13
    Caption = 'Max char:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 120
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitLeft = 30
    ExplicitTop = 246
    ExplicitWidth = 185
    object BitBtn1: TBitBtn
      Left = 74
      Top = 10
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 420
      Top = 10
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Pixel_Width: TSpinEdit
    Left = 64
    Top = 6
    Width = 65
    Height = 22
    MaxValue = 32
    MinValue = 1
    TabOrder = 1
    Value = 9
  end
  object Raster_Height: TSpinEdit
    Left = 64
    Top = 35
    Width = 64
    Height = 22
    MaxValue = 32767
    MinValue = 1
    TabOrder = 2
    Value = 11
  end
  object Max_Chars: TSpinEdit
    Left = 64
    Top = 64
    Width = 65
    Height = 22
    MaxValue = 32768
    MinValue = 128
    TabOrder = 3
    Value = 256
  end
end
