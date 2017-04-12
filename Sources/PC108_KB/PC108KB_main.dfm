object Main_Form: TMain_Form
  Left = 370
  Top = 448
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 282
  ClientWidth = 933
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
    Width = 933
    Height = 282
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
      Left = 0
      Top = 0
      Width = 40
      Height = 40
      Caption = 'Esc'
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
      Top = 241
      Width = 65
      Height = 39
      AllowAllUp = True
      GroupIndex = 12
      Caption = 'Ctrl'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_1: TSpeedButton
      Left = 40
      Top = 81
      Width = 40
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
      Left = 80
      Top = 81
      Width = 39
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
      Left = 119
      Top = 81
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
      Left = 159
      Top = 81
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
      Left = 199
      Top = 81
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
      Left = 239
      Top = 81
      Width = 40
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
      Left = 279
      Top = 81
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
      Left = 319
      Top = 81
      Width = 39
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
      Left = 358
      Top = 81
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
      Left = 398
      Top = 81
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
      Left = 438
      Top = 81
      Width = 40
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
      Top = 81
      Width = 40
      Height = 40
      Caption = '~ `'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Vertical: TSpeedButton
      Tag = 159
      Left = 665
      Top = 162
      Width = 39
      Height = 40
      Caption = 'End'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Right_Brace: TSpeedButton
      Left = 624
      Top = 162
      Width = 40
      Height = 40
      Caption = 'Delete'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton110: TSpeedButton
      Left = 0
      Top = 121
      Width = 60
      Height = 40
      Caption = 'Tab'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton111: TSpeedButton
      Left = 60
      Top = 121
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
      Left = 100
      Top = 121
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
      Left = 140
      Top = 121
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
      Left = 180
      Top = 121
      Width = 39
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
      Left = 219
      Top = 121
      Width = 40
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
      Left = 259
      Top = 121
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
    object Popup_Button: TSpeedButton
      Tag = 132
      Left = 498
      Top = 241
      Width = 52
      Height = 40
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        8E000000424D8E000000000000003E0000002800000014000000140000000100
        010000000000500000000000000000000000020000000000000000000000FFFF
        FF00000000007FFE0000400200005FFA40004002A000400320005FFB40004004
        8000400480005FFD000040060000400200007FFE00007FFE0000400200004002
        00007FFE00007FFE00007FFE000000000000}
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton119: TSpeedButton
      Left = 299
      Top = 121
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
      Left = 339
      Top = 121
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
      Left = 379
      Top = 121
      Width = 39
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
      Left = 418
      Top = 121
      Width = 40
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
      Left = 458
      Top = 121
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
      Left = 75
      Top = 161
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
      Tag = 133
      Left = 0
      Top = 161
      Width = 75
      Height = 40
      Caption = 'Caps_lock'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Up_Button: TSpeedButton
      Left = 665
      Top = 202
      Width = 39
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
      OnClick = SpeedButton110Click
    end
    object Left_Button: TSpeedButton
      Left = 625
      Top = 243
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
      Left = 0
      Top = 201
      Width = 99
      Height = 40
      AllowAllUp = True
      GroupIndex = 11
      Caption = 'Shift'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton131: TSpeedButton
      Left = 115
      Top = 161
      Width = 39
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
      Left = 154
      Top = 161
      Width = 40
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
      Left = 194
      Top = 161
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
      Left = 99
      Top = 201
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
      Left = 234
      Top = 161
      Width = 40
      Height = 40
      Caption = 'G'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton136: TSpeedButton
      Left = 274
      Top = 161
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
      Left = 314
      Top = 161
      Width = 39
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
      Left = 353
      Top = 161
      Width = 40
      Height = 40
      Caption = 'K'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton139: TSpeedButton
      Left = 393
      Top = 161
      Width = 40
      Height = 40
      Caption = 'L'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton140: TSpeedButton
      Left = 139
      Top = 201
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
      Left = 179
      Top = 201
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
      Left = 219
      Top = 201
      Width = 39
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
      Left = 258
      Top = 201
      Width = 40
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
      Left = 298
      Top = 201
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
      Left = 338
      Top = 201
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
      Left = 433
      Top = 161
      Width = 40
      Height = 40
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
      Left = 473
      Top = 161
      Width = 40
      Height = 40
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
      Left = 513
      Top = 121
      Width = 83
      Height = 80
      Caption = '   Enter'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Repeat_Button: TSpeedButton
      Left = 478
      Top = 81
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
    object Button_Less_Than: TSpeedButton
      Left = 378
      Top = 201
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
      Left = 418
      Top = 201
      Width = 39
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
      Left = 457
      Top = 201
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
      Left = 704
      Top = 243
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
      Left = 665
      Top = 243
      Width = 39
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
      Left = 171
      Top = 241
      Width = 223
      Height = 39
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Right_Shift_Button: TSpeedButton
      Left = 497
      Top = 201
      Width = 99
      Height = 40
      AllowAllUp = True
      GroupIndex = 11
      Caption = 'Shift'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Break_Button: TSpeedButton
      Tag = 163
      Left = 704
      Top = 162
      Width = 40
      Height = 40
      Caption = 'Page Down'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Spacing = 1
      OnClick = SpeedButton110Click
    end
    object Backspace_Button: TSpeedButton
      Left = 557
      Top = 81
      Width = 39
      Height = 40
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
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
    object SpeedButton2: TSpeedButton
      Left = 518
      Top = 81
      Width = 39
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
    object Left_Windows_Button: TSpeedButton
      Left = 65
      Top = 241
      Width = 54
      Height = 39
      AllowAllUp = True
      GroupIndex = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        8E000000424D8E000000000000003E0000002800000014000000140000000100
        010000000000500000000000000000000000020000000000000000000000FFFF
        FF0000000000000000000AC0600020BFA0008A842000208420008A8420002084
        20008A84200020C460008ABFA000208420008A842000208420008A8420002084
        20008AC46000203F80008000000000000000}
      ParentFont = False
    end
    object SpeedButton12: TSpeedButton
      Tag = 181
      Left = 773
      Top = 81
      Width = 40
      Height = 40
      Caption = 'Num Lock'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton13: TSpeedButton
      Tag = 179
      Left = 813
      Top = 81
      Width = 39
      Height = 40
      Caption = '/'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton14: TSpeedButton
      Tag = 180
      Left = 852
      Top = 81
      Width = 40
      Height = 40
      Caption = '*'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton15: TSpeedButton
      Tag = 178
      Left = 892
      Top = 81
      Width = 40
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
    object SpeedButton16: TSpeedButton
      Tag = 172
      Left = 773
      Top = 121
      Width = 40
      Height = 40
      Caption = '7 Home'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton17: TSpeedButton
      Tag = 173
      Left = 813
      Top = 121
      Width = 39
      Height = 40
      AllowAllUp = True
      GroupIndex = 4
      Caption = '8 '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton18: TSpeedButton
      Tag = 174
      Left = 852
      Top = 121
      Width = 40
      Height = 40
      Caption = '9 PgUp'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton19: TSpeedButton
      Tag = 176
      Left = 892
      Top = 121
      Width = 40
      Height = 80
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton20: TSpeedButton
      Tag = 169
      Left = 773
      Top = 161
      Width = 40
      Height = 40
      Caption = '4 '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton21: TSpeedButton
      Tag = 170
      Left = 813
      Top = 161
      Width = 39
      Height = 40
      Caption = '5 '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton22: TSpeedButton
      Tag = 171
      Left = 852
      Top = 161
      Width = 40
      Height = 40
      Caption = '6 '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton24: TSpeedButton
      Tag = 166
      Left = 773
      Top = 201
      Width = 40
      Height = 40
      Caption = '1 End'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton25: TSpeedButton
      Tag = 167
      Left = 813
      Top = 201
      Width = 39
      Height = 40
      AllowAllUp = True
      GroupIndex = 4
      Caption = '2 '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton26: TSpeedButton
      Tag = 168
      Left = 852
      Top = 201
      Width = 40
      Height = 40
      Caption = '3 PgDn'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton27: TSpeedButton
      Tag = 175
      Left = 892
      Top = 201
      Width = 40
      Height = 79
      Caption = 'Enter'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton28: TSpeedButton
      Tag = 165
      Left = 773
      Top = 241
      Width = 79
      Height = 39
      AllowAllUp = True
      GroupIndex = 4
      Caption = '0 Ins'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton29: TSpeedButton
      Tag = 177
      Left = 852
      Top = 241
      Width = 40
      Height = 39
      Caption = '. Del'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Left_Brace: TSpeedButton
      Left = 498
      Top = 121
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
    object Left_Alt_button: TSpeedButton
      Left = 119
      Top = 241
      Width = 52
      Height = 39
      AllowAllUp = True
      GroupIndex = 14
      Caption = 'Alt'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Right_Windows_Button: TSpeedButton
      Left = 447
      Top = 241
      Width = 52
      Height = 39
      AllowAllUp = True
      GroupIndex = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        8E000000424D8E000000000000003E0000002800000014000000140000000100
        010000000000500000000000000000000000020000000000000000000000FFFF
        FF0000000000000000000AC0600020BFA0008A842000208420008A8420002084
        20008A84200020C460008ABFA000208420008A842000208420008A8420002084
        20008AC46000203F80008000000000000000}
      ParentFont = False
    end
    object Right_Alt_Button: TSpeedButton
      Left = 394
      Top = 241
      Width = 53
      Height = 39
      AllowAllUp = True
      GroupIndex = 14
      Caption = 'Alt'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Right_Control_Button: TSpeedButton
      Left = 550
      Top = 241
      Width = 47
      Height = 40
      AllowAllUp = True
      GroupIndex = 12
      Caption = 'Ctrl'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton3: TSpeedButton
      Tag = 157
      Left = 625
      Top = 121
      Width = 40
      Height = 40
      Caption = 'Insert'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton30: TSpeedButton
      Tag = 158
      Left = 665
      Top = 121
      Width = 39
      Height = 40
      Caption = 'Home'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton31: TSpeedButton
      Tag = 164
      Left = 704
      Top = 121
      Width = 40
      Height = 40
      Caption = 'Page Up'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Spacing = 1
      OnClick = SpeedButton110Click
    end
    object SpeedButton32: TSpeedButton
      Tag = 154
      Left = 625
      Top = 80
      Width = 40
      Height = 40
      Caption = 'Print Scren SysRq'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton33: TSpeedButton
      Tag = 155
      Left = 665
      Top = 80
      Width = 39
      Height = 40
      Caption = 'Scroll Lock'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton34: TSpeedButton
      Tag = 155
      Left = 704
      Top = 80
      Width = 40
      Height = 40
      Caption = 'Pause'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Spacing = 1
      OnClick = SpeedButton110Click
    end
    object SpeedButton35: TSpeedButton
      Tag = 162
      Left = 622
      Top = 0
      Width = 39
      Height = 40
      Caption = 'Wake Up'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton36: TSpeedButton
      Tag = 160
      Left = 661
      Top = 0
      Width = 40
      Height = 40
      Caption = 'Sleep'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton37: TSpeedButton
      Tag = 161
      Left = 701
      Top = 0
      Width = 40
      Height = 40
      Caption = 'Power'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Spacing = 1
      OnClick = SpeedButton110Click
    end
    object SpeedButton38: TSpeedButton
      Tag = 142
      Left = 80
      Top = 0
      Width = 39
      Height = 40
      Caption = 'F1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton39: TSpeedButton
      Tag = 143
      Left = 119
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton40: TSpeedButton
      Tag = 144
      Left = 159
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton41: TSpeedButton
      Tag = 145
      Left = 199
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton42: TSpeedButton
      Tag = 146
      Left = 258
      Top = 0
      Width = 39
      Height = 40
      Caption = 'F5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton43: TSpeedButton
      Tag = 147
      Left = 297
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton44: TSpeedButton
      Tag = 148
      Left = 337
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F7'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton45: TSpeedButton
      Tag = 149
      Left = 377
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F8'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton46: TSpeedButton
      Tag = 150
      Left = 436
      Top = 0
      Width = 39
      Height = 40
      Caption = 'F9'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton47: TSpeedButton
      Tag = 151
      Left = 475
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F10'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton48: TSpeedButton
      Tag = 152
      Left = 515
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F11'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton49: TSpeedButton
      Tag = 153
      Left = 555
      Top = 0
      Width = 40
      Height = 40
      Caption = 'F12'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Num_Lock_LED: TShape
      Left = 806
      Top = 18
      Width = 13
      Height = 13
      Brush.Color = clGreen
      Shape = stCircle
    end
    object Caps_Lock_LED: TShape
      Left = 844
      Top = 18
      Width = 13
      Height = 13
      Brush.Color = clGreen
      Shape = stCircle
    end
    object Scroll_Lock_LED: TShape
      Left = 884
      Top = 18
      Width = 13
      Height = 13
      Brush.Color = clGreen
      Shape = stCircle
    end
    object Label1: TLabel
      Left = 802
      Top = 34
      Width = 25
      Height = 29
      AutoSize = False
      Caption = 'Num Lock'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label2: TLabel
      Left = 839
      Top = 36
      Width = 26
      Height = 29
      AutoSize = False
      Caption = 'Caps Lock'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label3: TLabel
      Left = 879
      Top = 36
      Width = 31
      Height = 29
      AutoSize = False
      Caption = 'Scroll Lock'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
end
