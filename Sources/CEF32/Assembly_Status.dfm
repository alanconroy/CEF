object Assembly_Statistics: TAssembly_Statistics
  Left = 300
  Top = 200
  Caption = 'Assembly status'
  ClientHeight = 298
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 7
    Top = 8
    Width = 29
    Height = 13
    Caption = 'Data: '
  end
  object Label2: TLabel
    Left = 7
    Top = 28
    Width = 28
    Height = 13
    Caption = 'Code:'
  end
  object Label3: TLabel
    Left = 7
    Top = 61
    Width = 48
    Height = 13
    Caption = 'Warnings:'
  end
  object Label4: TLabel
    Left = 7
    Top = 79
    Width = 30
    Height = 13
    Caption = 'Errors:'
  end
  object Data_Label: TLabel
    Left = 40
    Top = 8
    Width = 34
    Height = 13
    Caption = '0 bytes'
  end
  object Code_Label: TLabel
    Left = 40
    Top = 28
    Width = 34
    Height = 13
    Caption = '0 bytes'
  end
  object Warnings_Label: TLabel
    Left = 58
    Top = 61
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Errors_Label: TLabel
    Left = 57
    Top = 79
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label5: TLabel
    Left = 7
    Top = 112
    Width = 19
    Height = 13
    Caption = 'File:'
  end
  object Filename: TLabel
    Left = 37
    Top = 111
    Width = 3
    Height = 13
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 265
    Width = 426
    Height = 33
    Align = alBottom
    TabOrder = 0
    object Abort_Button: TSpeedButton
      Left = 10
      Top = 4
      Width = 45
      Height = 22
      Caption = 'Abort'
      OnClick = Abort_ButtonClick
    end
  end
  object List_Box: TListBox
    Left = 0
    Top = 125
    Width = 426
    Height = 140
    Align = alBottom
    BevelInner = bvNone
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = List_BoxDblClick
  end
end
