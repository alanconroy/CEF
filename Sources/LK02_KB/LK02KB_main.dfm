object Main_Form: TMain_Form
  Left = 542
  Top = 801
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 215
  ClientWidth = 1000
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
    Width = 1000
    Height = 215
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
      Left = 192
      Top = 8
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
      Left = 188
      Top = 87
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
      Left = 232
      Top = 8
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
      Left = 272
      Top = 8
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
      Left = 311
      Top = 8
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
      Left = 351
      Top = 8
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
      Left = 391
      Top = 8
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
      Left = 431
      Top = 8
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
      Left = 471
      Top = 8
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
      Left = 510
      Top = 8
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
      Left = 550
      Top = 8
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
      Left = 590
      Top = 8
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
      Left = 630
      Top = 8
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
    object Button_Vertical: TSpeedButton
      Left = 706
      Top = 87
      Width = 40
      Height = 40
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
      Left = 766
      Top = 47
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
      Left = 192
      Top = 47
      Width = 56
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
      Left = 248
      Top = 47
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
      Left = 288
      Top = 47
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
      Left = 328
      Top = 47
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
      Left = 368
      Top = 47
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
      Left = 408
      Top = 47
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
      Left = 447
      Top = 47
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
      Left = 746
      Top = 127
      Width = 39
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
    object SpeedButton119: TSpeedButton
      Left = 487
      Top = 47
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
      Left = 527
      Top = 47
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
      Left = 567
      Top = 47
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
      Left = 607
      Top = 47
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
      Left = 646
      Top = 47
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
      Left = 268
      Top = 87
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
      Left = 228
      Top = 87
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 89
      Caption = 'CAPS LOCK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Left_Shift_Button: TSpeedButton
      Left = 228
      Top = 127
      Width = 57
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
      Left = 308
      Top = 87
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
      Left = 348
      Top = 87
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
      Left = 387
      Top = 87
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
      Left = 285
      Top = 127
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
      Left = 427
      Top = 87
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
      Left = 467
      Top = 87
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
      Left = 507
      Top = 87
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
      Left = 547
      Top = 87
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
      Left = 586
      Top = 87
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
      Left = 325
      Top = 127
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
      Left = 365
      Top = 127
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
      Left = 405
      Top = 127
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
      Left = 445
      Top = 127
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
      Left = 485
      Top = 127
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
      Left = 525
      Top = 127
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
      Left = 626
      Top = 87
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
      Left = 666
      Top = 87
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
      Left = 746
      Top = 87
      Width = 70
      Height = 40
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
      Left = 670
      Top = 8
      Width = 40
      Height = 39
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
      Left = 565
      Top = 127
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
      Left = 605
      Top = 127
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
      Left = 645
      Top = 127
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
    object Space_Button: TSpeedButton
      Left = 290
      Top = 167
      Width = 361
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
      Left = 685
      Top = 127
      Width = 61
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
      Left = 788
      Top = 8
      Width = 39
      Height = 39
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
      Left = 749
      Top = 8
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
      Left = 710
      Top = 8
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
    object SpeedButton16: TSpeedButton
      Tag = 148
      Left = 837
      Top = 7
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
      Left = 877
      Top = 7
      Width = 40
      Height = 40
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
      Left = 917
      Top = 7
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
      Left = 957
      Top = 7
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
      Left = 837
      Top = 47
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
      Left = 877
      Top = 47
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
      Left = 917
      Top = 47
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
      Left = 957
      Top = 47
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
      Left = 837
      Top = 86
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
      Left = 877
      Top = 86
      Width = 40
      Height = 40
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
      Left = 917
      Top = 86
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
      Left = 957
      Top = 86
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
      Left = 837
      Top = 126
      Width = 80
      Height = 40
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
      Left = 917
      Top = 126
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
      Left = 686
      Top = 47
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
    object Label7: TLabel
      Left = 6
      Top = 172
      Width = 18
      Height = 10
      Caption = 'STD.'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label9: TLabel
      Left = 42
      Top = 172
      Width = 14
      Height = 10
      Caption = 'ALT.'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object STD_LED: TShape
      Left = 10
      Top = 156
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object ALT_LED: TShape
      Left = 44
      Top = 156
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Device_Select_LED: TShape
      Left = 120
      Top = 157
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Paper_Out_LED: TShape
      Left = 82
      Top = 156
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Line_Local_Button: TSpeedButton
      Tag = 148
      Left = 10
      Top = 56
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 95
      Caption = 'LINE LOCAL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Alt_Char_Set_Button: TSpeedButton
      Tag = 145
      Left = 10
      Top = 96
      Width = 40
      Height = 39
      AllowAllUp = True
      GroupIndex = 96
      Caption = 'ALT CHAR SET'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Char_Set_Lock_Button: TSpeedButton
      Tag = 156
      Left = 50
      Top = 96
      Width = 40
      Height = 39
      AllowAllUp = True
      GroupIndex = 97
      Caption = 'CHAR SET LOCK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Set_Top_Button: TSpeedButton
      Tag = 155
      Left = 50
      Top = 56
      Width = 40
      Height = 40
      Caption = 'SET TOP'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Baud_110_Button: TSpeedButton
      Tag = 150
      Left = 90
      Top = 56
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 99
      Caption = '110'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Auto_LF_Button: TSpeedButton
      Tag = 157
      Left = 90
      Top = 96
      Width = 40
      Height = 39
      AllowAllUp = True
      GroupIndex = 98
      Caption = 'AUTO LF'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Baud_300_Button: TSpeedButton
      Tag = 153
      Left = 130
      Top = 56
      Width = 39
      Height = 40
      AllowAllUp = True
      GroupIndex = 90
      Caption = '300'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Here_Is_Button: TSpeedButton
      Tag = 129
      Left = 130
      Top = 96
      Width = 39
      Height = 39
      Caption = 'HERE IS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Select_Avail_LED: TShape
      Left = 158
      Top = 157
      Width = 10
      Height = 10
      Brush.Color = clMaroon
      Shape = stCircle
    end
    object Label1: TLabel
      Left = 1
      Top = 186
      Width = 67
      Height = 10
      Caption = 'CHARACTER SET'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label2: TLabel
      Left = 75
      Top = 172
      Width = 25
      Height = 10
      Caption = 'PAPER'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label3: TLabel
      Left = 112
      Top = 172
      Width = 29
      Height = 10
      Caption = 'DEVICE'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label4: TLabel
      Left = 150
      Top = 172
      Width = 30
      Height = 10
      Caption = 'SELECT'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label5: TLabel
      Left = 79
      Top = 186
      Width = 17
      Height = 10
      Caption = 'OUT'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label6: TLabel
      Left = 112
      Top = 186
      Width = 30
      Height = 10
      Caption = 'SELECT'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label8: TLabel
      Left = 154
      Top = 186
      Width = 21
      Height = 10
      Caption = 'AVAIL.'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object SpeedButton33: TSpeedButton
      Left = 726
      Top = 47
      Width = 40
      Height = 40
      Caption = 'LINE FEED'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -7
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
  end
end
