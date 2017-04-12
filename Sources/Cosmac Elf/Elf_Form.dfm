object Front_Panel_Form: TFront_Panel_Form
  Left = 747
  Top = 394
  BorderIcons = []
  Caption = 'MITS Altair 8800'
  ClientHeight = 148
  ClientWidth = 233
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Q_LED: TShape
    Left = 91
    Top = 7
    Width = 13
    Height = 13
    Brush.Color = clMaroon
    Shape = stCircle
  end
  object Label1: TLabel
    Left = 93
    Top = 25
    Width = 8
    Height = 13
    Caption = 'Q'
  end
  object In_Button: TSpeedButton
    Left = 7
    Top = 85
    Width = 18
    Height = 17
    Caption = 'IN'
    OnMouseDown = In_ButtonMouseDown
    OnMouseUp = In_ButtonMouseUp
  end
  object Load_Button: TSpeedButton
    Left = 33
    Top = 85
    Width = 33
    Height = 17
    AllowAllUp = True
    GroupIndex = 1
    Caption = 'LOAD'
  end
  object MP_Button: TSpeedButton
    Left = 131
    Top = 84
    Width = 22
    Height = 18
    AllowAllUp = True
    GroupIndex = 2
    Caption = 'MP'
    OnClick = MP_ButtonClick
  end
  object Run_Button: TSpeedButton
    Left = 156
    Top = 84
    Width = 29
    Height = 18
    AllowAllUp = True
    GroupIndex = 3
    Caption = 'RUN'
    OnClick = Run_ButtonClick
  end
  object Data_High: TPanel
    Left = 70
    Top = 46
    Width = 28
    Height = 33
    Caption = '0'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object Data_Low: TPanel
    Left = 98
    Top = 46
    Width = 28
    Height = 33
    Caption = '0'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 1
    Top = 10
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Debug1: TMenuItem
      Caption = '&Debug'
      OnClick = Debug1Click
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
end
