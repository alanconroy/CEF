object Front_Panel_Form: TFront_Panel_Form
  Left = 334
  Top = 625
  Width = 917
  Height = 338
  BorderIcons = []
  Caption = 'Front Panel'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Control_Box: TGroupBox
    Left = 7
    Top = 10
    Width = 723
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
    Width = 722
    Height = 61
    Caption = 'Data'
    TabOrder = 1
  end
  object Address_Box: TGroupBox
    Left = 7
    Top = 158
    Width = 723
    Height = 61
    Caption = 'Address'
    TabOrder = 2
  end
end
