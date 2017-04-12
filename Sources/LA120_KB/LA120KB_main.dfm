object Main_Form: TMain_Form
  Left = 542
  Top = 801
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 261
  ClientWidth = 853
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
    OnClick = SpeedButton110Click
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
    OnClick = SpeedButton110Click
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
    OnClick = SpeedButton110Click
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
    OnClick = SpeedButton110Click
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
    OnClick = SpeedButton110Click
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
    OnClick = SpeedButton110Click
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
    OnClick = SpeedButton110Click
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 853
    Height = 261
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
    object Escape_Button: TSpeedButton
      Left = 35
      Top = 63
      Width = 40
      Height = 39
      Caption = 'ESC'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Spacing = 1
      OnClick = SpeedButton110Click
    end
    object Left_Control_Button: TSpeedButton
      Left = -2
      Top = 142
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 12
      Caption = 'CTRL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_1: TSpeedButton
      Left = 75
      Top = 63
      Width = 40
      Height = 39
      Caption = '! 1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_2: TSpeedButton
      Left = 115
      Top = 63
      Width = 39
      Height = 39
      Caption = '@ 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_3: TSpeedButton
      Left = 154
      Top = 63
      Width = 40
      Height = 39
      Caption = '# 3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_4: TSpeedButton
      Left = 194
      Top = 63
      Width = 40
      Height = 39
      Caption = '$ 4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_5: TSpeedButton
      Left = 234
      Top = 63
      Width = 40
      Height = 39
      Caption = '% 5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_6: TSpeedButton
      Left = 274
      Top = 63
      Width = 40
      Height = 39
      Caption = '^ 6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_7: TSpeedButton
      Left = 314
      Top = 63
      Width = 39
      Height = 39
      Caption = '& 7'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_8: TSpeedButton
      Left = 353
      Top = 63
      Width = 40
      Height = 39
      Caption = '* 8'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_9: TSpeedButton
      Left = 393
      Top = 63
      Width = 40
      Height = 39
      Caption = '( 9'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton89: TSpeedButton
      Left = 433
      Top = 63
      Width = 40
      Height = 39
      Caption = ') 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Equal: TSpeedButton
      Left = 473
      Top = 63
      Width = 40
      Height = 39
      Caption = '_ -'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Tilde: TSpeedButton
      Left = -2
      Top = 183
      Width = 40
      Height = 40
      Caption = 'VIEW'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Vertical: TSpeedButton
      Left = 611
      Top = 142
      Width = 40
      Height = 40
      Caption = '| \'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Right_Brace: TSpeedButton
      Left = 610
      Top = 102
      Width = 40
      Height = 39
      Caption = 'DELETE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -7
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton110: TSpeedButton
      Left = 35
      Top = 102
      Width = 59
      Height = 40
      Caption = 'TAB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton111: TSpeedButton
      Left = 94
      Top = 102
      Width = 40
      Height = 40
      Caption = 'Q'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton112: TSpeedButton
      Left = 134
      Top = 102
      Width = 40
      Height = 40
      Caption = 'W'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton113: TSpeedButton
      Left = 174
      Top = 102
      Width = 40
      Height = 40
      Caption = 'E'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton114: TSpeedButton
      Left = 214
      Top = 102
      Width = 40
      Height = 40
      Caption = 'R'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton115: TSpeedButton
      Left = 254
      Top = 102
      Width = 39
      Height = 40
      Caption = 'T'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton116: TSpeedButton
      Left = 293
      Top = 102
      Width = 40
      Height = 40
      Caption = 'Y'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Line_Feed_Button: TSpeedButton
      Left = 605
      Top = 182
      Width = 39
      Height = 40
      Caption = 'LINE FEED'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton119: TSpeedButton
      Left = 333
      Top = 102
      Width = 40
      Height = 40
      Caption = 'U'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton120: TSpeedButton
      Left = 373
      Top = 102
      Width = 40
      Height = 40
      Caption = 'I'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton121: TSpeedButton
      Left = 413
      Top = 102
      Width = 40
      Height = 40
      Caption = 'O'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton122: TSpeedButton
      Left = 453
      Top = 102
      Width = 39
      Height = 40
      Caption = 'P'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Accent: TSpeedButton
      Left = 492
      Top = 102
      Width = 40
      Height = 40
      Caption = '{ ['
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton125: TSpeedButton
      Left = 113
      Top = 142
      Width = 40
      Height = 40
      Caption = 'A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Shift_Lock_Button: TSpeedButton
      Left = 38
      Top = 142
      Width = 75
      Height = 40
      AllowAllUp = True
      GroupIndex = 99
      Caption = 'CAPS LOCK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Up_Button: TSpeedButton
      Tag = 128
      Left = 473
      Top = 22
      Width = 40
      Height = 40
      Caption = 'LINE LOCAL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Left_Button: TSpeedButton
      Tag = 130
      Left = 553
      Top = 22
      Width = 39
      Height = 40
      Caption = 'LOCAL FORM FEED'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Left_Shift_Button: TSpeedButton
      Left = 38
      Top = 182
      Width = 95
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton131: TSpeedButton
      Left = 153
      Top = 142
      Width = 40
      Height = 40
      Caption = 'S'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton132: TSpeedButton
      Left = 193
      Top = 142
      Width = 39
      Height = 40
      Caption = 'D'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton133: TSpeedButton
      Left = 232
      Top = 142
      Width = 40
      Height = 40
      Caption = 'F'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton134: TSpeedButton
      Left = 133
      Top = 182
      Width = 40
      Height = 40
      Caption = 'Z'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton135: TSpeedButton
      Left = 272
      Top = 142
      Width = 40
      Height = 40
      Caption = 'BELL G'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton136: TSpeedButton
      Left = 312
      Top = 142
      Width = 40
      Height = 40
      Caption = 'H'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton137: TSpeedButton
      Left = 352
      Top = 142
      Width = 40
      Height = 40
      Caption = 'J'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton138: TSpeedButton
      Left = 392
      Top = 142
      Width = 39
      Height = 40
      Caption = 'VT K'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton139: TSpeedButton
      Left = 431
      Top = 142
      Width = 40
      Height = 40
      Caption = 'FF L'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton140: TSpeedButton
      Left = 173
      Top = 182
      Width = 40
      Height = 40
      Caption = 'X'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton141: TSpeedButton
      Left = 213
      Top = 182
      Width = 40
      Height = 40
      Caption = 'C'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton142: TSpeedButton
      Left = 253
      Top = 182
      Width = 40
      Height = 40
      Caption = 'V'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton143: TSpeedButton
      Left = 293
      Top = 182
      Width = 39
      Height = 40
      Caption = 'B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton144: TSpeedButton
      Left = 332
      Top = 182
      Width = 40
      Height = 40
      Caption = 'N'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton145: TSpeedButton
      Left = 372
      Top = 182
      Width = 40
      Height = 40
      Caption = 'M'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Plus: TSpeedButton
      Left = 471
      Top = 142
      Width = 40
      Height = 40
      Caption = '+ ;'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Asterisk: TSpeedButton
      Left = 511
      Top = 142
      Width = 40
      Height = 40
      Caption = '* :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Return_Button: TSpeedButton
      Left = 551
      Top = 102
      Width = 60
      Height = 80
      Caption = '   RETURN'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Repeat_Button: TSpeedButton
      Left = 513
      Top = 63
      Width = 40
      Height = 39
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
    object Button_Less_Than: TSpeedButton
      Left = 412
      Top = 182
      Width = 40
      Height = 40
      Caption = '< ,'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Greater_Than: TSpeedButton
      Left = 452
      Top = 182
      Width = 40
      Height = 40
      Caption = '> .'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Question_Mark: TSpeedButton
      Left = 492
      Top = 182
      Width = 39
      Height = 40
      Caption = '? /'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Right_Button: TSpeedButton
      Tag = 131
      Left = 592
      Top = 22
      Width = 40
      Height = 40
      Caption = 'LOCAL FORM FEED'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Down_Button: TSpeedButton
      Tag = 129
      Left = 513
      Top = 22
      Width = 40
      Height = 40
      Caption = 'HERE IS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Space_Button: TSpeedButton
      Left = 150
      Top = 222
      Width = 355
      Height = 40
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Right_Shift_Button: TSpeedButton
      Left = 531
      Top = 182
      Width = 74
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
      OnClick = SpeedButton110Click
    end
    object Break_Button: TSpeedButton
      Left = 631
      Top = 62
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton1: TSpeedButton
      Left = 592
      Top = 63
      Width = 39
      Height = 39
      Caption = 'BACK SPACE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton2: TSpeedButton
      Left = 553
      Top = 63
      Width = 39
      Height = 39
      Caption = '~ `'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton3: TSpeedButton
      Left = 35
      Top = 22
      Width = 52
      Height = 40
      Caption = 'SET-UP'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton12: TSpeedButton
      Left = 693
      Top = 22
      Width = 40
      Height = 40
      Caption = 'PF1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton13: TSpeedButton
      Left = 733
      Top = 22
      Width = 40
      Height = 40
      Caption = 'PF2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton14: TSpeedButton
      Left = 773
      Top = 22
      Width = 40
      Height = 40
      Caption = 'PF3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton15: TSpeedButton
      Left = 813
      Top = 22
      Width = 39
      Height = 40
      Caption = 'PF4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton16: TSpeedButton
      Tag = 148
      Left = 693
      Top = 62
      Width = 40
      Height = 40
      Caption = '7'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton17: TSpeedButton
      Tag = 149
      Left = 733
      Top = 62
      Width = 40
      Height = 40
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
      Tag = 150
      Left = 773
      Top = 62
      Width = 40
      Height = 40
      Caption = '9'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton19: TSpeedButton
      Tag = 153
      Left = 813
      Top = 62
      Width = 39
      Height = 40
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton20: TSpeedButton
      Tag = 145
      Left = 693
      Top = 102
      Width = 40
      Height = 39
      Caption = '4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton21: TSpeedButton
      Tag = 146
      Left = 733
      Top = 102
      Width = 40
      Height = 39
      Caption = '5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton22: TSpeedButton
      Tag = 147
      Left = 773
      Top = 102
      Width = 40
      Height = 39
      Caption = '6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton23: TSpeedButton
      Tag = 152
      Left = 813
      Top = 102
      Width = 39
      Height = 39
      Caption = ','
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton24: TSpeedButton
      Tag = 142
      Left = 693
      Top = 141
      Width = 40
      Height = 40
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton25: TSpeedButton
      Tag = 143
      Left = 733
      Top = 141
      Width = 40
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
      Tag = 144
      Left = 773
      Top = 141
      Width = 40
      Height = 40
      Caption = '3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton27: TSpeedButton
      Tag = 154
      Left = 813
      Top = 141
      Width = 39
      Height = 80
      Caption = 'ENTER'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton28: TSpeedButton
      Tag = 141
      Left = 693
      Top = 181
      Width = 80
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
      Tag = 151
      Left = 773
      Top = 181
      Width = 40
      Height = 40
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Left_Brace: TSpeedButton
      Left = 532
      Top = 102
      Width = 40
      Height = 40
      Caption = '} ]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Label1: TLabel
      Left = 89
      Top = 49
      Width = 17
      Height = 12
      Caption = 'SET'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label2: TLabel
      Left = 156
      Top = 38
      Width = 36
      Height = 21
      Alignment = taCenter
      AutoSize = False
      Caption = 'CLEAR ALL'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object Label3: TLabel
      Left = 204
      Top = 49
      Width = 18
      Height = 12
      Caption = 'TOP'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label4: TLabel
      Left = 235
      Top = 38
      Width = 31
      Height = 21
      AutoSize = False
      Caption = 'TOP LEFT'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object Label5: TLabel
      Left = 286
      Top = 38
      Width = 24
      Height = 21
      AutoSize = False
      Caption = 'BOT RT'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object Label6: TLabel
      Left = 319
      Top = 38
      Width = 31
      Height = 24
      Alignment = taCenter
      Caption = 'MAR CLEAR'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object Label7: TLabel
      Left = 353
      Top = 49
      Width = 33
      Height = 12
      Caption = 'STATUS'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label8: TLabel
      Left = 391
      Top = 38
      Width = 43
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Caption = 'STORE RECALL'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object Label9: TLabel
      Left = 441
      Top = 49
      Width = 26
      Height = 12
      Caption = 'BAUD'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label10: TLabel
      Left = 116
      Top = 49
      Width = 31
      Height = 12
      Caption = 'CLEAR'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label11: TLabel
      Left = 102
      Top = 34
      Width = 16
      Height = 12
      Caption = 'TAB'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label12: TLabel
      Left = 262
      Top = 41
      Width = 20
      Height = 12
      Caption = 'MAR'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object On_Line_LED: TShape
      Left = 171
      Top = 24
      Width = 9
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Local_LED: TShape
      Left = 209
      Top = 24
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Alt_Char_Set_LED: TShape
      Left = 248
      Top = 24
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object LED_3: TShape
      Left = 291
      Top = 24
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object CTS_LED: TShape
      Left = 330
      Top = 24
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object DSR_LED: TShape
      Left = 369
      Top = 24
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Setup_LED: TShape
      Left = 408
      Top = 24
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Paper_Out_LED: TShape
      Left = 447
      Top = 24
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Label13: TLabel
      Left = 158
      Top = 12
      Width = 38
      Height = 12
      Caption = 'ON LINE'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label14: TLabel
      Left = 197
      Top = 12
      Width = 30
      Height = 12
      Caption = 'LOCAL'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label15: TLabel
      Left = 230
      Top = 2
      Width = 47
      Height = 22
      Alignment = taCenter
      AutoSize = False
      Caption = 'ALT CHAR SET'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object Label16: TLabel
      Left = 326
      Top = 12
      Width = 18
      Height = 12
      Caption = 'CTS'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label17: TLabel
      Left = 365
      Top = 12
      Width = 20
      Height = 12
      Caption = 'DSR'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label18: TLabel
      Left = 397
      Top = 12
      Width = 30
      Height = 12
      Caption = 'SETUP'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label19: TLabel
      Left = 427
      Top = 12
      Width = 52
      Height = 12
      Caption = 'PAPER OUT'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Display_Panel: TPanel
      Left = 93
      Top = 12
      Width = 56
      Height = 16
      Alignment = taRightJustify
      BevelInner = bvLowered
      BevelOuter = bvNone
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
end
