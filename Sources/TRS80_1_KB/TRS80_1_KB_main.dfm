object Main_Form: TMain_Form
  Left = 656
  Top = 785
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 201
  ClientWidth = 761
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
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton4: TSpeedButton
    Left = 475
    Top = 2
    Width = 39
    Height = 39
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Glyph.Data = {
      8E000000424D8E000000000000003E0000002800000014000000140000000100
      010000000000500000000000000000000000020000000000000000000000FFFF
      FF00000000000000000000000000000000000020000000200000002000000020
      0000002000000020000000200000002000000020000001FC000000F800000070
      000000200000000000000000000000000000}
    ParentFont = False
    OnClick = Up_ButtonClick
  end
  object SpeedButton5: TSpeedButton
    Left = 514
    Top = 2
    Width = 40
    Height = 39
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Glyph.Data = {
      8E000000424D8E000000000000003E0000002800000014000000140000000100
      010000000000500000000000000000000000020000000000000000000000FFFF
      FF00000000000000000000000000002000000070000000F8000001FC00000020
      0000002000000020000000200000002000000020000000200000002000000020
      000000000000000000000000000000000000}
    ParentFont = False
    OnClick = Up_ButtonClick
  end
  object SpeedButton6: TSpeedButton
    Left = 554
    Top = 2
    Width = 40
    Height = 39
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Glyph.Data = {
      8E000000424D8E000000000000003E0000002800000014000000140000000100
      010000000000500000000000000000000000020000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000200
      0000060000000E0000001FFF00000E0000000600000002000000000000000000
      000000000000000000000000000000000000}
    ParentFont = False
    OnClick = Up_ButtonClick
  end
  object SpeedButton7: TSpeedButton
    Left = 594
    Top = 2
    Width = 40
    Height = 39
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Glyph.Data = {
      8E000000424D8E000000000000003E0000002800000014000000140000000100
      010000000000500000000000000000000000020000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000004
      000000060000000700000FFF8000000700000006000000040000000000000000
      000000000000000000000000000000000000}
    ParentFont = False
    OnClick = Up_ButtonClick
  end
  object SpeedButton8: TSpeedButton
    Left = 475
    Top = 42
    Width = 39
    Height = 40
    Caption = '_  -'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = Up_ButtonClick
  end
  object SpeedButton9: TSpeedButton
    Left = 514
    Top = 42
    Width = 40
    Height = 40
    AllowAllUp = True
    GroupIndex = 4
    Caption = '+ ='
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object SpeedButton10: TSpeedButton
    Left = 554
    Top = 42
    Width = 40
    Height = 40
    Caption = '~ `'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = Up_ButtonClick
  end
  object SpeedButton11: TSpeedButton
    Left = 593
    Top = 42
    Width = 40
    Height = 40
    Caption = 'BACK SPACE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = Up_ButtonClick
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 761
    Height = 201
    Align = alClient
    BevelOuter = bvNone
    Color = clGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Clear_Button: TSpeedButton
      Left = 532
      Top = 80
      Width = 40
      Height = 39
      AllowAllUp = True
      GroupIndex = 12
      Caption = 'CLEAR'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_1: TSpeedButton
      Left = 20
      Top = 0
      Width = 39
      Height = 40
      Caption = '! 1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_2: TSpeedButton
      Left = 59
      Top = 0
      Width = 40
      Height = 40
      Caption = '" 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_3: TSpeedButton
      Left = 99
      Top = 0
      Width = 40
      Height = 40
      Caption = '# 3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_4: TSpeedButton
      Left = 139
      Top = 0
      Width = 40
      Height = 40
      Caption = '$ 4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_5: TSpeedButton
      Left = 179
      Top = 0
      Width = 40
      Height = 40
      Caption = '% 5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_6: TSpeedButton
      Left = 219
      Top = 0
      Width = 39
      Height = 40
      Caption = '& 6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_7: TSpeedButton
      Left = 258
      Top = 0
      Width = 40
      Height = 40
      Caption = #39' 7'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_8: TSpeedButton
      Left = 298
      Top = 0
      Width = 40
      Height = 40
      Caption = '( 8'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_9: TSpeedButton
      Left = 338
      Top = 0
      Width = 40
      Height = 40
      Caption = ') 9'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton89: TSpeedButton
      Left = 378
      Top = 0
      Width = 40
      Height = 40
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_Equal: TSpeedButton
      Left = 418
      Top = 0
      Width = 39
      Height = 40
      Caption = '* :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Up_Button: TSpeedButton
      Left = -1
      Top = 40
      Width = 40
      Height = 40
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        8E000000424D8E000000000000003E0000002800000014000000140000000100
        010000000000500000000000000000000000020000000000000000000000FFFF
        FF00000000000000000000000000000000000020000000200000002000000020
        0000002000000020000000200000002000000020000001FC000000F800000070
        000000200000000000000000000000000000}
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton111: TSpeedButton
      Left = 39
      Top = 40
      Width = 40
      Height = 40
      Caption = 'Q'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton112: TSpeedButton
      Left = 79
      Top = 40
      Width = 40
      Height = 40
      Caption = 'W'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton113: TSpeedButton
      Left = 119
      Top = 40
      Width = 39
      Height = 40
      Caption = 'E'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton114: TSpeedButton
      Left = 158
      Top = 40
      Width = 40
      Height = 40
      Caption = 'R'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton115: TSpeedButton
      Left = 198
      Top = 40
      Width = 40
      Height = 40
      Caption = 'T'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton116: TSpeedButton
      Left = 238
      Top = 40
      Width = 40
      Height = 40
      Caption = 'Y'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Right_Button: TSpeedButton
      Left = 516
      Top = 40
      Width = 40
      Height = 40
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        8E000000424D8E000000000000003E0000002800000014000000140000000100
        010000000000500000000000000000000000020000000000000000000000FFFF
        FF00000000000000000000000000000000000000000000000000000000000004
        000000060000000700000FFF8000000700000006000000040000000000000000
        000000000000000000000000000000000000}
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton119: TSpeedButton
      Left = 278
      Top = 40
      Width = 40
      Height = 40
      Caption = 'U'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton120: TSpeedButton
      Left = 318
      Top = 40
      Width = 40
      Height = 40
      Caption = 'I'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton121: TSpeedButton
      Left = 358
      Top = 40
      Width = 39
      Height = 40
      Caption = 'O'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton122: TSpeedButton
      Left = 397
      Top = 40
      Width = 40
      Height = 40
      Caption = 'P'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_Accent: TSpeedButton
      Left = 437
      Top = 40
      Width = 40
      Height = 40
      Caption = '@'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton125: TSpeedButton
      Left = 54
      Top = 80
      Width = 39
      Height = 39
      Caption = 'A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Down_Button: TSpeedButton
      Left = 14
      Top = 80
      Width = 40
      Height = 39
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        8E000000424D8E000000000000003E0000002800000014000000140000000100
        010000000000500000000000000000000000020000000000000000000000FFFF
        FF00000000000000000000000000002000000070000000F8000001FC00000020
        0000002000000020000000200000002000000020000000200000002000000020
        000000000000000000000000000000000000}
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Left_Shift_Button: TSpeedButton
      Left = 14
      Top = 119
      Width = 74
      Height = 40
      AllowAllUp = True
      GroupIndex = 11
      Caption = 'SHIFT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton131: TSpeedButton
      Left = 93
      Top = 80
      Width = 40
      Height = 39
      Caption = 'S'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton132: TSpeedButton
      Left = 133
      Top = 80
      Width = 40
      Height = 39
      Caption = 'D'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton133: TSpeedButton
      Left = 173
      Top = 80
      Width = 40
      Height = 39
      Caption = 'F'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton134: TSpeedButton
      Left = 88
      Top = 119
      Width = 40
      Height = 40
      Caption = 'Z'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton135: TSpeedButton
      Left = 213
      Top = 80
      Width = 40
      Height = 39
      Caption = 'G'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton136: TSpeedButton
      Left = 253
      Top = 80
      Width = 40
      Height = 39
      Caption = 'H'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton137: TSpeedButton
      Left = 293
      Top = 80
      Width = 39
      Height = 39
      Caption = 'J'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton138: TSpeedButton
      Left = 332
      Top = 80
      Width = 40
      Height = 39
      Caption = 'K'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton139: TSpeedButton
      Left = 372
      Top = 80
      Width = 40
      Height = 39
      Caption = 'L'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton140: TSpeedButton
      Left = 128
      Top = 119
      Width = 39
      Height = 40
      Caption = 'X'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton141: TSpeedButton
      Left = 167
      Top = 119
      Width = 40
      Height = 40
      Caption = 'C'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton142: TSpeedButton
      Left = 207
      Top = 119
      Width = 40
      Height = 40
      Caption = 'V'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton143: TSpeedButton
      Left = 247
      Top = 119
      Width = 40
      Height = 40
      Caption = 'B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton144: TSpeedButton
      Left = 287
      Top = 119
      Width = 40
      Height = 40
      Caption = 'N'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton145: TSpeedButton
      Left = 327
      Top = 119
      Width = 39
      Height = 40
      Caption = 'M'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_Plus: TSpeedButton
      Left = 412
      Top = 80
      Width = 40
      Height = 39
      Caption = '+ ;'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Return_Button: TSpeedButton
      Left = 452
      Top = 80
      Width = 80
      Height = 39
      Caption = 'ENTER'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Repeat_Button: TSpeedButton
      Left = 457
      Top = 0
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 4
      Caption = '= -'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Button_Less_Than: TSpeedButton
      Left = 366
      Top = 119
      Width = 40
      Height = 40
      Caption = '< ,'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_Greater_Than: TSpeedButton
      Left = 406
      Top = 119
      Width = 40
      Height = 40
      Caption = '> .'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Button_Question_Mark: TSpeedButton
      Left = 446
      Top = 119
      Width = 40
      Height = 40
      Caption = '? /'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Space_Button: TSpeedButton
      Left = 126
      Top = 159
      Width = 321
      Height = 40
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Right_Shift_Button: TSpeedButton
      Left = 486
      Top = 119
      Width = 46
      Height = 40
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'SHIFT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Break_Button: TSpeedButton
      Left = 497
      Top = 0
      Width = 40
      Height = 40
      Caption = 'BREAK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Spacing = 1
      OnClick = Up_ButtonClick
    end
    object SpeedButton16: TSpeedButton
      Tag = 144
      Left = 638
      Top = 24
      Width = 40
      Height = 39
      Caption = '7'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton17: TSpeedButton
      Tag = 145
      Left = 678
      Top = 24
      Width = 39
      Height = 39
      AllowAllUp = True
      GroupIndex = 4
      Caption = '8'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton18: TSpeedButton
      Tag = 146
      Left = 717
      Top = 24
      Width = 40
      Height = 39
      Caption = '9'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton20: TSpeedButton
      Tag = 141
      Left = 638
      Top = 63
      Width = 40
      Height = 40
      Caption = '4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton21: TSpeedButton
      Tag = 142
      Left = 678
      Top = 63
      Width = 39
      Height = 40
      Caption = '5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton22: TSpeedButton
      Tag = 143
      Left = 717
      Top = 63
      Width = 40
      Height = 40
      Caption = '6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton24: TSpeedButton
      Tag = 138
      Left = 638
      Top = 103
      Width = 40
      Height = 40
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton25: TSpeedButton
      Tag = 139
      Left = 678
      Top = 103
      Width = 39
      Height = 40
      AllowAllUp = True
      GroupIndex = 4
      Caption = '2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton26: TSpeedButton
      Tag = 140
      Left = 717
      Top = 103
      Width = 40
      Height = 40
      Caption = '3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object SpeedButton28: TSpeedButton
      Tag = 137
      Left = 638
      Top = 143
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 4
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton29: TSpeedButton
      Tag = 135
      Left = 678
      Top = 143
      Width = 39
      Height = 40
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object Left_Button: TSpeedButton
      Left = 477
      Top = 40
      Width = 40
      Height = 40
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        8E000000424D8E000000000000003E0000002800000014000000140000000100
        010000000000500000000000000000000000020000000000000000000000FFFF
        FF00000000000000000000000000000000000000000000000000000000000200
        0000060000000E0000001FFF00000E0000000600000002000000000000000000
        000000000000000000000000000000000000}
      ParentFont = False
      OnClick = Up_ButtonClick
    end
    object NKP_Return_Button: TSpeedButton
      Tag = 136
      Left = 717
      Top = 143
      Width = 40
      Height = 40
      Caption = 'ENTER'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Up_ButtonClick
    end
  end
end
