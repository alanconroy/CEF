object Front_Panel_Form: TFront_Panel_Form
  Left = 326
  Top = 436
  BorderIcons = []
  Caption = 'MITS Altair 8800'
  ClientHeight = 361
  ClientWidth = 809
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Run_Button: TSpeedButton
    Left = 9
    Top = 258
    Width = 28
    Height = 18
    AllowAllUp = True
    GroupIndex = 1
    Caption = 'Run'
    OnClick = Run_ButtonClick
  end
  object Control_Box: TGroupBox
    Left = 7
    Top = 10
    Width = 650
    Height = 61
    Caption = 'Control'
    TabOrder = 0
    object Read_LED: TShape
      Left = 19
      Top = 15
      Width = 12
      Height = 13
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Write_LED: TShape
      Left = 57
      Top = 15
      Width = 12
      Height = 13
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Label1: TLabel
      Left = 13
      Top = 37
      Width = 26
      Height = 13
      Caption = 'Read'
    end
    object Label2: TLabel
      Left = 50
      Top = 37
      Width = 25
      Height = 13
      Caption = 'Write'
    end
    object Memory_LED: TShape
      Left = 93
      Top = 15
      Width = 13
      Height = 13
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object IO_LED: TShape
      Left = 129
      Top = 15
      Width = 12
      Height = 13
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Label3: TLabel
      Left = 89
      Top = 37
      Width = 23
      Height = 13
      Caption = 'Mem'
    end
    object Label4: TLabel
      Left = 128
      Top = 37
      Width = 16
      Height = 13
      Caption = 'I/O'
    end
  end
  object Data_Box: TGroupBox
    Left = 7
    Top = 82
    Width = 650
    Height = 61
    Caption = 'Data'
    TabOrder = 1
    object IMSAI_Panel: TPanel
      Left = 2
      Top = 15
      Width = 150
      Height = 44
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
    end
  end
  object Address_Box: TGroupBox
    Left = 7
    Top = 184
    Width = 650
    Height = 61
    Caption = 'Address'
    TabOrder = 2
  end
  object MainMenu1: TMainMenu
    Left = 216
    Top = 36
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object Emulation1: TMenuItem
        Caption = '&Emulation'
        object Altair1: TMenuItem
          Caption = '&Altair'
          Checked = True
          RadioItem = True
          OnClick = IMSAI1Click
        end
        object IMSAI1: TMenuItem
          Caption = '&IMSAI'
          RadioItem = True
          OnClick = IMSAI1Click
        end
      end
    end
    object Debug1: TMenuItem
      Caption = '&Debug'
      OnClick = Debug1Click
    end
    object Ports1: TMenuItem
      Caption = 'signal&Ports...'
      OnClick = Ports1Click
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
