object Generic_Tape_Form: TGeneric_Tape_Form
  Left = 1189
  Top = 181
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 484
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 170
    Top = 0
    Height = 484
  end
  object Command_Panel: TPanel
    Left = 0
    Top = 0
    Width = 170
    Height = 484
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 7
      Top = 10
      Width = 41
      Height = 13
      Caption = 'Record: '
    end
    object Current_Record_Label: TLabel
      Left = 119
      Top = 8
      Width = 18
      Height = 13
      Caption = 'of 0'
    end
    object Rewind_Button: TSpeedButton
      Left = 12
      Top = 33
      Width = 25
      Height = 22
      Caption = 'BOT'
      OnClick = Rewind_ButtonClick
    end
    object FF_Button: TSpeedButton
      Left = 40
      Top = 33
      Width = 25
      Height = 22
      Caption = 'EOT'
      OnClick = FF_ButtonClick
    end
    object Label2: TLabel
      Left = 9
      Top = 66
      Width = 62
      Height = 13
      Caption = 'Record size: '
    end
    object Record_Size_Label: TLabel
      Left = 76
      Top = 66
      Width = 3
      Height = 13
    end
    object Add_Data_Button: TSpeedButton
      Left = 10
      Top = 85
      Width = 86
      Height = 22
      Caption = 'Add data...'
      OnClick = Add_Data_ButtonClick
    end
    object SpeedButton1: TSpeedButton
      Left = 11
      Top = 114
      Width = 85
      Height = 22
      Caption = 'Add Tape Mark'
      OnClick = SpeedButton1Click
    end
    object Record_Selector: TSpinEdit
      Left = 46
      Top = 7
      Width = 70
      Height = 22
      MaxValue = 1
      MinValue = 1
      TabOrder = 0
      Value = 1
      OnChange = Record_SelectorChange
    end
  end
  object Data_Panel: TPanel
    Left = 173
    Top = 0
    Width = 451
    Height = 484
    Align = alClient
    TabOrder = 1
  end
end
