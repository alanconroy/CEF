object Main_Form: TMain_Form
  Left = 542
  Top = 801
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 215
  ClientWidth = 823
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
    Width = 823
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
    object Top_Left_Button: TSpeedButton
      Left = 35
      Top = 12
      Width = 40
      Height = 39
      Caption = '{ ['
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
      Left = 35
      Top = 91
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
      Top = 12
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
      Top = 12
      Width = 39
      Height = 39
      Caption = '" 2'
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
      Top = 12
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
      Top = 12
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
      Top = 12
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
      Top = 12
      Width = 40
      Height = 39
      Caption = '& 6'
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
      Top = 12
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
      Top = 12
      Width = 40
      Height = 39
      Caption = '( 8'
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
      Top = 12
      Width = 40
      Height = 39
      Caption = ') 9'
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
      Top = 12
      Width = 40
      Height = 39
      Caption = '0'
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
      Top = 12
      Width = 40
      Height = 39
      Caption = '= -'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Vertical: TSpeedButton
      Left = 551
      Top = 91
      Width = 40
      Height = 40
      Caption = 'RUB OUT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton110: TSpeedButton
      Left = 18
      Top = 51
      Width = 40
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
      Left = 98
      Top = 51
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
      Left = 138
      Top = 51
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
      Left = 178
      Top = 51
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
      Left = 218
      Top = 51
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
      Left = 258
      Top = 51
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
      Left = 297
      Top = 51
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
    object Bar_Button: TSpeedButton
      Left = 577
      Top = 131
      Width = 39
      Height = 40
      Caption = '| \'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton119: TSpeedButton
      Left = 337
      Top = 51
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
      Left = 377
      Top = 51
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
      Left = 417
      Top = 51
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
      Left = 457
      Top = 51
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
      Left = 496
      Top = 51
      Width = 40
      Height = 40
      Caption = '\ @'
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
      Top = 91
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
      Left = 58
      Top = 131
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 99
      Caption = 'SHIFT LOCK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = Shift_Lock_ButtonClick
    end
    object Left_Shift_Button: TSpeedButton
      Left = 98
      Top = 131
      Width = 40
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
      Top = 91
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
      Top = 91
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
      Top = 91
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
      Left = 138
      Top = 131
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
      Top = 91
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
      Top = 91
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
      Top = 91
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
      Top = 91
      Width = 39
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
      Left = 431
      Top = 91
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
      Left = 178
      Top = 131
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
      Left = 218
      Top = 131
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
      Left = 258
      Top = 131
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
      Left = 298
      Top = 131
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
      Left = 338
      Top = 131
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
      Left = 378
      Top = 131
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
      Top = 91
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
      Top = 91
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
      Left = 576
      Top = 51
      Width = 40
      Height = 40
      Caption = 'CR'
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
      Top = 12
      Width = 40
      Height = 39
      AllowAllUp = True
      GroupIndex = 4
      Caption = '_ _'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Button_Less_Than: TSpeedButton
      Left = 418
      Top = 131
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
      Left = 458
      Top = 131
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
      Left = 498
      Top = 131
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
      Left = 150
      Top = 171
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
      Left = 538
      Top = 131
      Width = 40
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
    object SpeedButton2: TSpeedButton
      Left = 553
      Top = 12
      Width = 39
      Height = 39
      Caption = '} ]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Button_Line_Feed: TSpeedButton
      Left = 536
      Top = 51
      Width = 40
      Height = 40
      Caption = 'LF'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Local_Line_Feed_Button: TSpeedButton
      Tag = 131
      Left = 693
      Top = 91
      Width = 40
      Height = 40
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Mode_Button: TSpeedButton
      Tag = 155
      Left = 733
      Top = 91
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 4
      Down = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Baud_Rate_Button: TSpeedButton
      Tag = 156
      Left = 773
      Top = 91
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 99
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Label1: TLabel
      Left = 698
      Top = 60
      Width = 27
      Height = 10
      Caption = 'LOCAL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 702
      Top = 70
      Width = 19
      Height = 10
      Caption = 'LINE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 702
      Top = 80
      Width = 20
      Height = 10
      Caption = 'FEED'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 740
      Top = 80
      Width = 27
      Height = 10
      Caption = 'LOCAL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 740
      Top = 70
      Width = 24
      Height = 10
      Caption = 'MODE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 744
      Top = 134
      Width = 19
      Height = 10
      Caption = 'LINE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 780
      Top = 60
      Width = 23
      Height = 10
      Caption = 'BAUD'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 782
      Top = 70
      Width = 20
      Height = 10
      Caption = 'RATE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 786
      Top = 80
      Width = 12
      Height = 10
      Caption = '110'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 786
      Top = 134
      Width = 12
      Height = 10
      Caption = '300'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = 11
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton1: TSpeedButton
      Left = 58
      Top = 51
      Width = 40
      Height = 40
      Caption = 'ALT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object SpeedButton3: TSpeedButton
      Left = 75
      Top = 91
      Width = 40
      Height = 40
      AllowAllUp = True
      GroupIndex = 12
      Caption = '~ ^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton110Click
    end
    object Shift_Lock_LED: TShape
      Left = 85
      Top = 134
      Width = 8
      Height = 8
      Brush.Color = clMaroon
      Shape = stCircle
    end
  end
end
