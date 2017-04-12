object Front_Panel_Form: TFront_Panel_Form
  Left = 469
  Top = 366
  Width = 790
  Height = 399
  BorderIcons = []
  Caption = 'Intellec8'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefaultPosOnly
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
    Width = 386
    Height = 61
    Caption = 'STATUS'
    TabOrder = 0
    object Label11: TLabel
      Left = 10
      Top = 18
      Width = 24
      Height = 13
      Caption = 'RUN'
    end
    object Run_LED: TShape
      Left = 14
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Shape2: TShape
      Left = 56
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label12: TLabel
      Left = 50
      Top = 18
      Width = 28
      Height = 13
      Caption = 'WAIT'
    end
    object Shape3: TShape
      Left = 98
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label13: TLabel
      Left = 92
      Top = 18
      Width = 28
      Height = 13
      Caption = 'HALT'
    end
    object Shape4: TShape
      Left = 140
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label14: TLabel
      Left = 134
      Top = 18
      Width = 30
      Height = 13
      Caption = 'HOLD'
    end
    object Label15: TLabel
      Left = 172
      Top = 18
      Width = 37
      Height = 13
      Caption = 'COMPL'
    end
    object Shape5: TShape
      Left = 182
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label16: TLabel
      Left = 220
      Top = 18
      Width = 23
      Height = 13
      Caption = 'REQ'
    end
    object Shape6: TShape
      Left = 224
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label17: TLabel
      Left = 264
      Top = 18
      Width = 23
      Height = 13
      Caption = 'REQ'
    end
    object Shape7: TShape
      Left = 266
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label18: TLabel
      Left = 296
      Top = 18
      Width = 45
      Height = 13
      Caption = 'DISABLE'
    end
    object Shape8: TShape
      Left = 308
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label19: TLabel
      Left = 174
      Top = 6
      Width = 30
      Height = 13
      Caption = 'SRCH'
    end
    object Label20: TLabel
      Left = 210
      Top = 6
      Width = 42
      Height = 13
      Caption = 'ACCESS'
    end
    object Label21: TLabel
      Left = 264
      Top = 6
      Width = 18
      Height = 13
      Caption = 'INT'
    end
    object Label22: TLabel
      Left = 308
      Top = 6
      Width = 18
      Height = 13
      Caption = 'INT'
    end
  end
  object Data_Box: TGroupBox
    Left = 8
    Top = 150
    Width = 400
    Height = 61
    Caption = 'INSTRUCTION/DATA'
    TabOrder = 1
  end
  object Address_Box: TGroupBox
    Left = 8
    Top = 80
    Width = 764
    Height = 61
    Caption = 'ADDRESS'
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 414
    Top = 12
    Width = 359
    Height = 59
    Caption = 'CYCLE'
    TabOrder = 3
    object Memory_LED: TShape
      Left = 56
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label3: TLabel
      Left = 50
      Top = 18
      Width = 25
      Height = 13
      Caption = 'MEM'
    end
    object IO_LED: TShape
      Left = 98
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label4: TLabel
      Left = 98
      Top = 18
      Width = 16
      Height = 13
      Caption = 'I/O'
    end
    object Fetch: TShape
      Left = 14
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label5: TLabel
      Left = 5
      Top = 18
      Width = 35
      Height = 13
      Caption = 'FETCH'
    end
    object Label6: TLabel
      Left = 140
      Top = 18
      Width = 15
      Height = 13
      Caption = 'DA'
    end
    object Label1: TLabel
      Left = 172
      Top = 6
      Width = 35
      Height = 13
      Caption = 'READ/'
    end
    object Label2: TLabel
      Left = 210
      Top = 6
      Width = 44
      Height = 13
      Caption = 'WRITE /'
    end
    object Write_LED: TShape
      Left = 224
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Read_LED: TShape
      Left = 182
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Label7: TLabel
      Left = 172
      Top = 18
      Width = 33
      Height = 13
      Caption = 'INPUT'
    end
    object Label8: TLabel
      Left = 210
      Top = 18
      Width = 45
      Height = 13
      Caption = 'OUTPUT'
    end
    object Label9: TLabel
      Left = 266
      Top = 18
      Width = 18
      Height = 13
      Caption = 'INT'
    end
    object Label10: TLabel
      Left = 300
      Top = 18
      Width = 35
      Height = 13
      Caption = 'STACK'
    end
    object Int_LED: TShape
      Left = 266
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object DA: TShape
      Left = 140
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
    object Stack_LED: TShape
      Left = 308
      Top = 32
      Width = 16
      Height = 10
      Brush.Color = clMaroon
    end
  end
  object MainMenu1: TMainMenu
    Left = 237
    Top = 65529
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
