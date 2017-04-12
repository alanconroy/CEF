object Printer_Options_Dialog: TPrinter_Options_Dialog
  Left = 192
  Top = 128
  Caption = 'Printer Options'
  ClientHeight = 272
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Panel: TPanel
    Left = 0
    Top = 239
    Width = 516
    Height = 33
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 15
      Top = 6
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 199
      Top = 6
      Width = 61
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 516
    Height = 239
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Paper'
      object Label9: TLabel
        Left = 258
        Top = 37
        Width = 55
        Height = 13
        Caption = 'Max pages:'
      end
      object Max_Pages: TSpinEdit
        Left = 321
        Top = 34
        Width = 121
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object GroupBox2: TGroupBox
        Left = 11
        Top = 3
        Width = 185
        Height = 86
        Caption = 'Paper size'
        TabOrder = 1
        object Label3: TLabel
          Left = 3
          Top = 25
          Width = 34
          Height = 13
          Caption = 'Height:'
        end
        object Label4: TLabel
          Left = 3
          Top = 54
          Width = 31
          Height = 13
          Caption = 'Width:'
        end
        object Label2: TLabel
          Left = 86
          Top = 18
          Width = 8
          Height = 29
          Caption = '.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -24
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label6: TLabel
          Left = 86
          Top = 44
          Width = 8
          Height = 29
          Caption = '.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -24
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label7: TLabel
          Left = 134
          Top = 24
          Width = 31
          Height = 13
          Caption = 'inches'
        end
        object Label8: TLabel
          Left = 134
          Top = 52
          Width = 31
          Height = 13
          Caption = 'inches'
        end
        object Width: TSpinEdit
          Left = 43
          Top = 50
          Width = 39
          Height = 22
          MaxValue = 32767
          MinValue = 1
          TabOrder = 0
          Value = 8
          OnChange = HeightChange
        end
        object Height: TSpinEdit
          Left = 43
          Top = 22
          Width = 39
          Height = 22
          MaxValue = 32767
          MinValue = 1
          TabOrder = 1
          Value = 11
          OnChange = HeightChange
        end
        object Height_Modification: TSpinEdit
          Left = 96
          Top = 21
          Width = 32
          Height = 22
          MaxValue = 9
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object Width_Modification: TSpinEdit
          Left = 96
          Top = 50
          Width = 32
          Height = 22
          MaxValue = 9
          MinValue = 0
          TabOrder = 3
          Value = 5
        end
      end
      object GroupBox3: TGroupBox
        Left = 11
        Top = 110
        Width = 185
        Height = 89
        Caption = 'Paper style'
        TabOrder = 2
        object Plain_RB: TRadioButton
          Left = 8
          Top = 18
          Width = 113
          Height = 17
          Caption = 'Plain'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object Barred_RB: TRadioButton
          Left = 8
          Top = 40
          Width = 113
          Height = 17
          Caption = 'Barred:'
          TabOrder = 1
        end
        object Green_Panel: TPanel
          Left = 26
          Top = 60
          Width = 23
          Height = 23
          BevelOuter = bvLowered
          TabOrder = 2
          object Green_Image: TImage
            Left = 1
            Top = 1
            Width = 21
            Height = 21
            Align = alClient
            Picture.Data = {
              07544269746D6170AE060000424DAE0600000000000036000000280000001700
              000017000000010018000000000078060000C40E0000C40E0000000000000000
              0000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB00
              0000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB00
              0000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB00
              0000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB00
              0000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB00
              0000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB
              C7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000C7D0BBC7D0BBC7D0BBC7D0BBC7D0
              BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7
              D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BBC7D0BB000000}
            OnClick = Green_ImageClick
            ExplicitTop = 3
          end
        end
        object Blue_Panel: TPanel
          Left = 49
          Top = 60
          Width = 23
          Height = 23
          TabOrder = 3
          object Blue_Image: TImage
            Left = 1
            Top = 1
            Width = 21
            Height = 21
            Align = alClient
            Picture.Data = {
              07544269746D6170AE060000424DAE0600000000000036000000280000001700
              000017000000010018000000000078060000C40E0000C40E0000000000000000
              0000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D700
              0000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D700
              0000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D700
              0000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D700
              0000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D700
              0000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7
              FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000FFE0D7FFE0D7FFE0D7FFE0D7FFE0
              D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FF
              E0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7FFE0D7000000}
            OnClick = Blue_ImageClick
            ExplicitLeft = 3
            ExplicitTop = -3
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Visual'
      ImageIndex = 1
      object Label1: TLabel
        Left = 305
        Top = 20
        Width = 35
        Height = 13
        Caption = 'Margin:'
      end
      object Label5: TLabel
        Left = 447
        Top = 20
        Width = 26
        Height = 13
        Caption = 'pixels'
      end
      object Label10: TLabel
        Left = 305
        Top = 51
        Width = 47
        Height = 13
        Caption = 'Ink bleed:'
      end
      object Label11: TLabel
        Left = 305
        Top = 80
        Width = 66
        Height = 13
        Caption = 'Magnification:'
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 289
        Height = 98
        Caption = 'Font'
        TabOrder = 0
        object Browse_Button: TSpeedButton
          Left = 254
          Top = 23
          Width = 18
          Height = 18
          Caption = '...'
          Enabled = False
          OnClick = Browse_ButtonClick
        end
        object RadioButton1: TRadioButton
          Left = 7
          Top = 23
          Width = 72
          Height = 14
          Caption = 'Font file'
          TabOrder = 0
          OnClick = RadioButton1Click
        end
        object RadioButton2: TRadioButton
          Left = 7
          Top = 62
          Width = 91
          Height = 14
          Caption = 'Windows font'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = RadioButton1Click
        end
        object Windows_Font: TComboBox
          Left = 112
          Top = 60
          Width = 159
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
        object Filename: TEdit
          Left = 79
          Top = 20
          Width = 170
          Height = 21
          Enabled = False
          TabOrder = 3
          OnChange = HeightChange
        end
      end
      object Margin: TSpinEdit
        Left = 390
        Top = 16
        Width = 49
        Height = 22
        MaxValue = 32767
        MinValue = 0
        TabOrder = 1
        Value = 2
      end
      object Bleed: TSpinEdit
        Left = 390
        Top = 46
        Width = 49
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 1
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 104
        Width = 289
        Height = 98
        Caption = 'Alternate Font'
        TabOrder = 3
        object Alternate_Browse_Button: TSpeedButton
          Left = 254
          Top = 23
          Width = 18
          Height = 18
          Caption = '...'
          Enabled = False
          OnClick = Alternate_Browse_ButtonClick
        end
        object RadioButton3: TRadioButton
          Left = 7
          Top = 23
          Width = 72
          Height = 14
          Caption = 'Font file'
          TabOrder = 0
          OnClick = RadioButton3Click
        end
        object RadioButton4: TRadioButton
          Left = 7
          Top = 62
          Width = 91
          Height = 14
          Caption = 'Windows font'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = RadioButton1Click
        end
        object Alternate_Windows_Font: TComboBox
          Left = 112
          Top = 60
          Width = 159
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
        object Alternate_Filename: TEdit
          Left = 79
          Top = 20
          Width = 170
          Height = 21
          Enabled = False
          TabOrder = 3
          OnChange = HeightChange
        end
      end
      object Magnification: TSpinEdit
        Left = 391
        Top = 77
        Width = 48
        Height = 22
        MaxValue = 100
        MinValue = 1
        TabOrder = 4
        Value = 1
      end
    end
  end
  object OpenDialog1: TOpenDialog
    InitialDir = 'fonts/'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 594
    Top = 172
  end
end
