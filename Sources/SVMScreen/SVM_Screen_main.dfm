object Main_Form: TMain_Form
  Left = 658
  Top = 135
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'SVM'
  ClientHeight = 611
  ClientWidth = 1006
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Screen_Panel: TPanel
    Left = 0
    Top = 0
    Width = 1006
    Height = 592
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 569
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 592
    Width = 1006
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    ExplicitTop = 769
  end
end
