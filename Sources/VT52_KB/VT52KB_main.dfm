object Main_Form: TMain_Form
  Left = 803
  Top = 578
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 201
  ClientWidth = 827
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
    Width = 827
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
    object Escape_Button: TSpeedButton
      Left = 6
      Top = 0
      Width = 40
      Height = 40
      Caption = 'ESC (SEL)'
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
      Left = 0
      Top = 80
      Width = 40
      Height = 39
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
      Left = 46
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
      OnClick = SpeedButton110Click
    end
    object Button_2: TSpeedButton
      Left = 85
      Top = 0
      Width = 40
      Height = 40
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
      Left = 125
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
      OnClick = SpeedButton110Click
    end
    object Button_4: TSpeedButton
      Left = 165
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
      OnClick = SpeedButton110Click
    end
    object Button_5: TSpeedButton
      Left = 205
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
      OnClick = SpeedButton110Click
    end
    object Button_6: TSpeedButton
      Left = 245
      Top = 0
      Width = 39
      Height = 40
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
      Left = 284
      Top = 0
      Width = 40
      Height = 40
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
      Left = 324
      Top = 0
      Width = 40
      Height = 40
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
      Left = 364
      Top = 0
      Width = 40
      Height = 40
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
      Left = 404
      Top = 0
      Width = 40
      Height = 40
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
      Left = 444
      Top = 0
      Width = 39
      Height = 40
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
      Left = 0
      Top = 119
      Width = 40
      Height = 40
      Caption = 'SCROLL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -7
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Vertical: TSpeedButton
      Left = 518
      Top = 80
      Width = 39
      Height = 39
      Caption = '} {'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Right_Brace: TSpeedButton
      Left = 581
      Top = 40
      Width = 40
      Height = 40
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
      Left = 6
      Top = 40
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
      Left = 65
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton112: TSpeedButton
      Left = 105
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton113: TSpeedButton
      Left = 145
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton114: TSpeedButton
      Left = 184
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton115: TSpeedButton
      Left = 224
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton116: TSpeedButton
      Left = 264
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
      OnClick = SpeedButton110Click
    end
    object Line_Feed_Button: TSpeedButton
      Left = 542
      Top = 40
      Width = 40
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
      Left = 304
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton120: TSpeedButton
      Left = 344
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton121: TSpeedButton
      Left = 384
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton122: TSpeedButton
      Left = 423
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
      OnClick = SpeedButton110Click
    end
    object Button_Accent: TSpeedButton
      Left = 463
      Top = 40
      Width = 40
      Height = 40
      Caption = '] ['
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton125: TSpeedButton
      Left = 80
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
      OnClick = SpeedButton110Click
    end
    object Shift_Lock_Button: TSpeedButton
      Left = 40
      Top = 80
      Width = 40
      Height = 39
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
      Left = 784
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
        FF00000000000000000000000000000000000020000000200000002000000020
        0000002000000020000000200000002000000020000001FC000000F800000070
        000000200000000000000000000000000000}
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Left_Button: TSpeedButton
      Left = 784
      Top = 121
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
      OnClick = SpeedButton110Click
    end
    object Left_Shift_Button: TSpeedButton
      Left = 40
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton131: TSpeedButton
      Left = 119
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton132: TSpeedButton
      Left = 159
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton133: TSpeedButton
      Left = 199
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton134: TSpeedButton
      Left = 114
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton135: TSpeedButton
      Left = 239
      Top = 80
      Width = 40
      Height = 39
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
      Left = 279
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton137: TSpeedButton
      Left = 319
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton138: TSpeedButton
      Left = 358
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton139: TSpeedButton
      Left = 398
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton140: TSpeedButton
      Left = 154
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton141: TSpeedButton
      Left = 193
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton142: TSpeedButton
      Left = 233
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton143: TSpeedButton
      Left = 273
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton144: TSpeedButton
      Left = 313
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton145: TSpeedButton
      Left = 353
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
      OnClick = SpeedButton110Click
    end
    object Button_Plus: TSpeedButton
      Left = 438
      Top = 80
      Width = 40
      Height = 39
      Caption = ': ;'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Asterisk: TSpeedButton
      Left = 478
      Top = 80
      Width = 40
      Height = 39
      Caption = '" '#39
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Return_Button: TSpeedButton
      Left = 557
      Top = 80
      Width = 81
      Height = 39
      Caption = 'RETURN'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Repeat_Button: TSpeedButton
      Left = 483
      Top = 0
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
      OnClick = SpeedButton110Click
    end
    object Button_Less_Than: TSpeedButton
      Left = 392
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
      OnClick = SpeedButton110Click
    end
    object Button_Greater_Than: TSpeedButton
      Left = 432
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
      OnClick = SpeedButton110Click
    end
    object Button_Question_Mark: TSpeedButton
      Left = 472
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
      OnClick = SpeedButton110Click
    end
    object Right_Button: TSpeedButton
      Left = 784
      Top = 81
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
        FF00000000000000000000000000000000000000000000000000000000000004
        000000060000000700000FFF8000000700000006000000040000000000000000
        000000000000000000000000000000000000}
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Down_Button: TSpeedButton
      Left = 784
      Top = 41
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
        FF00000000000000000000000000002000000070000000F8000001FC00000020
        0000002000000020000000200000002000000020000000200000002000000020
        000000000000000000000000000000000000}
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Space_Button: TSpeedButton
      Left = 121
      Top = 159
      Width = 360
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
      Left = 512
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
      OnClick = SpeedButton110Click
    end
    object Break_Button: TSpeedButton
      Left = 601
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton1: TSpeedButton
      Left = 561
      Top = 0
      Width = 40
      Height = 40
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
      Left = 523
      Top = 0
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
    object Left_Blank: TSpeedButton
      Tag = -1
      Left = 665
      Top = 2
      Width = 39
      Height = 39
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Center_Blank: TSpeedButton
      Tag = -1
      Left = 704
      Top = 2
      Width = 40
      Height = 39
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Right_Blank: TSpeedButton
      Tag = -1
      Left = 744
      Top = 2
      Width = 40
      Height = 39
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
      Left = 665
      Top = 41
      Width = 39
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
      Left = 704
      Top = 41
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton18: TSpeedButton
      Tag = 150
      Left = 744
      Top = 41
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
    object SpeedButton20: TSpeedButton
      Tag = 145
      Left = 665
      Top = 81
      Width = 39
      Height = 40
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
      Left = 704
      Top = 81
      Width = 40
      Height = 40
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
      Left = 744
      Top = 81
      Width = 40
      Height = 40
      Caption = '6'
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
      Left = 665
      Top = 121
      Width = 39
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
      Left = 704
      Top = 121
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton26: TSpeedButton
      Tag = 144
      Left = 744
      Top = 121
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
    object SpeedButton28: TSpeedButton
      Tag = 141
      Left = 665
      Top = 161
      Width = 79
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
      OnClick = SpeedButton110Click
    end
    object SpeedButton29: TSpeedButton
      Tag = 132
      Left = 744
      Top = 161
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
      Left = 503
      Top = 40
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
    object SpeedButton27: TSpeedButton
      Tag = 140
      Left = 784
      Top = 161
      Width = 40
      Height = 40
      Caption = 'ENTER'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton3: TSpeedButton
      Left = 558
      Top = 119
      Width = 40
      Height = 40
      Caption = 'REPEAT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -7
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton15: TSpeedButton
      Left = 598
      Top = 119
      Width = 40
      Height = 40
      Caption = 'COPY'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
  end
end
