object About_Box: TAbout_Box
  Left = 274
  Top = 117
  Caption = 'About CEF32'
  ClientHeight = 275
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 160
    Top = 7
    Width = 257
    Height = 20
    Caption = 'Computer Emulation Framework'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 143
    Top = 69
    Width = 290
    Height = 13
    Caption = 'Generic Computer Emulator.  Released to the public domain.  '
  end
  object Version_Label: TLabel
    Left = 277
    Top = 38
    Width = 22
    Height = 13
    Caption = 'V2.1'
  end
  object BitBtn1: TBitBtn
    Left = 258
    Top = 237
    Width = 61
    Height = 21
    TabOrder = 0
    Kind = bkOK
  end
end
