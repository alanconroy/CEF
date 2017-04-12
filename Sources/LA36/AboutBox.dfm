object About_Form: TAbout_Form
  Left = 258
  Top = 189
  Caption = 'About_Form'
  ClientHeight = 191
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Panel: TPanel
    Left = 0
    Top = 150
    Width = 335
    Height = 41
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Trebuchet MS'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 134
      Top = 8
      Width = 75
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Kind = bkOK
    end
  end
  object StaticText1: TStaticText
    Left = 57
    Top = 18
    Width = 206
    Height = 22
    Alignment = taCenter
    Caption = 'LA36 printer component for CEF32'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object StaticText2: TStaticText
    Left = 106
    Top = 56
    Width = 132
    Height = 17
    Alignment = taCenter
    Caption = 'http://cef.sourceforge.com'
    TabOrder = 2
  end
end
