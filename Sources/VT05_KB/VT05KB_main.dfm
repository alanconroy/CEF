object Main_Form: TMain_Form
  Left = 563
  Top = 195
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 200
  ClientWidth = 813
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
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 813
    Height = 200
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 813
      Height = 200
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
        Top = 40
        Width = 40
        Height = 40
        Caption = 'ESC'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Spacing = 1
        OnClick = SpeedButton110Click
      end
      object Left_Control_Button: TSpeedButton
        Left = 24
        Top = 80
        Width = 40
        Height = 39
        AllowAllUp = True
        GroupIndex = 12
        Caption = 'CTRL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_1: TSpeedButton
        Left = 66
        Top = 0
        Width = 40
        Height = 40
        Caption = '1 !'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_2: TSpeedButton
        Left = 106
        Top = 0
        Width = 39
        Height = 40
        Caption = '2  "'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_3: TSpeedButton
        Left = 145
        Top = 0
        Width = 40
        Height = 40
        Caption = '3  #'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_4: TSpeedButton
        Left = 185
        Top = 0
        Width = 40
        Height = 40
        Caption = '4  $'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_5: TSpeedButton
        Left = 225
        Top = 0
        Width = 40
        Height = 40
        Caption = '5  %'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_6: TSpeedButton
        Left = 265
        Top = 0
        Width = 40
        Height = 40
        Caption = '6  &'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_7: TSpeedButton
        Left = 305
        Top = 0
        Width = 40
        Height = 40
        Caption = '7  '#39
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_8: TSpeedButton
        Left = 345
        Top = 0
        Width = 39
        Height = 40
        Caption = '8  ('
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_9: TSpeedButton
        Left = 384
        Top = 0
        Width = 40
        Height = 40
        Caption = '9  )'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton89: TSpeedButton
        Left = 424
        Top = 0
        Width = 40
        Height = 40
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Equal: TSpeedButton
        Left = 464
        Top = 0
        Width = 40
        Height = 40
        Caption = '=  -'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Tilde: TSpeedButton
        Left = 64
        Top = 80
        Width = 40
        Height = 39
        Caption = '~  ^'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Left_Brace: TSpeedButton
        Left = 27
        Top = 0
        Width = 40
        Height = 40
        Caption = '{  ['
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Vertical: TSpeedButton
        Left = 562
        Top = 119
        Width = 40
        Height = 40
        Caption = '|  \'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Right_Brace: TSpeedButton
        Left = 544
        Top = 0
        Width = 39
        Height = 40
        Caption = '}  ]'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton110: TSpeedButton
        Left = 46
        Top = 40
        Width = 39
        Height = 40
        Caption = 'TAB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton111: TSpeedButton
        Left = 85
        Top = 40
        Width = 40
        Height = 40
        Caption = 'Q'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton112: TSpeedButton
        Left = 125
        Top = 40
        Width = 40
        Height = 40
        Caption = 'W'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton113: TSpeedButton
        Left = 165
        Top = 40
        Width = 40
        Height = 40
        Caption = 'E'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton114: TSpeedButton
        Left = 205
        Top = 40
        Width = 40
        Height = 40
        Caption = 'R'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton115: TSpeedButton
        Left = 245
        Top = 40
        Width = 39
        Height = 40
        Caption = 'T'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton116: TSpeedButton
        Left = 284
        Top = 40
        Width = 40
        Height = 40
        Caption = 'Y'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Line_Feed_Button: TSpeedButton
        Left = 522
        Top = 40
        Width = 40
        Height = 40
        Caption = 'LF'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton119: TSpeedButton
        Left = 324
        Top = 40
        Width = 40
        Height = 40
        Caption = 'U'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton120: TSpeedButton
        Left = 364
        Top = 40
        Width = 40
        Height = 40
        Caption = 'I'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton121: TSpeedButton
        Left = 404
        Top = 40
        Width = 40
        Height = 40
        Caption = 'O'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton122: TSpeedButton
        Left = 444
        Top = 40
        Width = 39
        Height = 40
        Caption = 'P'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Accent: TSpeedButton
        Left = 483
        Top = 40
        Width = 40
        Height = 40
        Caption = '`  @'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Return_Button: TSpeedButton
        Left = 562
        Top = 40
        Width = 40
        Height = 40
        Caption = 'CR'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton125: TSpeedButton
        Left = 104
        Top = 80
        Width = 40
        Height = 39
        Caption = 'A'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Shift_Lock_Button: TSpeedButton
        Left = 45
        Top = 119
        Width = 40
        Height = 40
        Caption = 'LOCK'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Up_Button: TSpeedButton
        Left = 691
        Top = 0
        Width = 39
        Height = 40
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
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
        Left = 691
        Top = 40
        Width = 39
        Height = 40
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
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
        Left = 85
        Top = 119
        Width = 39
        Height = 40
        AllowAllUp = True
        GroupIndex = 11
        Caption = 'SHIFT'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton131: TSpeedButton
        Left = 144
        Top = 80
        Width = 40
        Height = 39
        Caption = 'S'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton132: TSpeedButton
        Left = 184
        Top = 80
        Width = 39
        Height = 39
        Caption = 'D'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton133: TSpeedButton
        Left = 223
        Top = 80
        Width = 40
        Height = 39
        Caption = 'F'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton134: TSpeedButton
        Left = 124
        Top = 119
        Width = 40
        Height = 40
        Caption = 'Z'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton135: TSpeedButton
        Left = 263
        Top = 80
        Width = 40
        Height = 39
        Caption = 'G'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton136: TSpeedButton
        Left = 303
        Top = 80
        Width = 40
        Height = 39
        Caption = 'H'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton137: TSpeedButton
        Left = 343
        Top = 80
        Width = 40
        Height = 39
        Caption = 'J'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton138: TSpeedButton
        Left = 383
        Top = 80
        Width = 40
        Height = 39
        Caption = 'K'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton139: TSpeedButton
        Left = 423
        Top = 80
        Width = 39
        Height = 39
        Caption = 'L'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton140: TSpeedButton
        Left = 164
        Top = 119
        Width = 40
        Height = 40
        Caption = 'X'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton141: TSpeedButton
        Left = 204
        Top = 119
        Width = 40
        Height = 40
        Caption = 'C'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton142: TSpeedButton
        Left = 244
        Top = 119
        Width = 40
        Height = 40
        Caption = 'V'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton143: TSpeedButton
        Left = 284
        Top = 119
        Width = 39
        Height = 40
        Caption = 'B'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton144: TSpeedButton
        Left = 323
        Top = 119
        Width = 40
        Height = 40
        Caption = 'N'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton145: TSpeedButton
        Left = 363
        Top = 119
        Width = 40
        Height = 40
        Caption = 'M'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Plus: TSpeedButton
        Left = 462
        Top = 80
        Width = 40
        Height = 39
        Caption = '+ ;'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Asterisk: TSpeedButton
        Left = 502
        Top = 80
        Width = 40
        Height = 39
        Caption = '*  :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Underscore: TSpeedButton
        Left = 542
        Top = 80
        Width = 40
        Height = 39
        Caption = 'RUB OUT'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Repeat_Button: TSpeedButton
        Left = 504
        Top = 0
        Width = 40
        Height = 40
        AllowAllUp = True
        GroupIndex = 4
        Caption = '_'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
      end
      object Button_Less_Than: TSpeedButton
        Left = 403
        Top = 119
        Width = 40
        Height = 40
        Caption = '<  ,'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Greater_Than: TSpeedButton
        Left = 443
        Top = 119
        Width = 40
        Height = 40
        Caption = '>  .'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Button_Question_Mark: TSpeedButton
        Left = 483
        Top = 119
        Width = 39
        Height = 40
        Caption = '?  /'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Right_Button: TSpeedButton
        Left = 730
        Top = 40
        Width = 40
        Height = 40
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
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
        Left = 730
        Top = 0
        Width = 40
        Height = 40
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
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
        Left = 141
        Top = 159
        Width = 355
        Height = 40
        OnClick = SpeedButton110Click
      end
      object Right_Shift_Button: TSpeedButton
        Left = 522
        Top = 119
        Width = 40
        Height = 40
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'SHIFT'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object Break_Button: TSpeedButton
        Left = 770
        Top = 0
        Width = 40
        Height = 40
        Caption = 'BREAK'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Spacing = 1
        OnClick = SpeedButton110Click
      end
      object SpeedButton1: TSpeedButton
        Left = 691
        Top = 80
        Width = 39
        Height = 40
        Caption = 'EEOL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
      object SpeedButton2: TSpeedButton
        Left = 730
        Top = 80
        Width = 40
        Height = 40
        Caption = 'EEOS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SpeedButton110Click
      end
    end
  end
end
