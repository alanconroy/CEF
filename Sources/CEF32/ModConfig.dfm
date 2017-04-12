object Component_Configuration_Dialog: TComponent_Configuration_Dialog
  Left = 1190
  Top = 704
  Width = 730
  Height = 336
  HelpContext = 11
  Caption = 'Configure component'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Panel: TPanel
    Left = 0
    Top = 264
    Width = 714
    Height = 34
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 14
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 263
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
    object Help_Button: TBitBtn
      Left = 504
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 2
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 714
    Height = 264
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Attributes'
      object Latency: TGroupBox
        Left = 7
        Top = 9
        Width = 207
        Height = 71
        Caption = 'Latency'
        TabOrder = 0
        object Label1: TLabel
          Left = 7
          Top = 21
          Width = 29
          Height = 13
          Caption = 'Read:'
        end
        object Label2: TLabel
          Left = 7
          Top = 42
          Width = 28
          Height = 13
          Caption = 'Write:'
        end
        object Label3: TLabel
          Left = 153
          Top = 23
          Width = 11
          Height = 13
          Caption = 'ns'
        end
        object Label4: TLabel
          Left = 153
          Top = 45
          Width = 11
          Height = 13
          Caption = 'ns'
        end
        object Read_Latency: TSpinEdit
          Left = 46
          Top = 17
          Width = 98
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object Write_Latency: TSpinEdit
          Left = 46
          Top = 41
          Width = 98
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
      object CPU: TGroupBox
        Left = 7
        Top = 86
        Width = 212
        Height = 47
        Caption = 'CPU'
        TabOrder = 1
        object Label5: TLabel
          Left = 5
          Top = 15
          Width = 30
          Height = 13
          Caption = 'Clock:'
        end
        object Label6: TLabel
          Left = 142
          Top = 15
          Width = 18
          Height = 13
          Caption = 'Khz'
        end
        object Clock_Speed: TLabel
          Left = 163
          Top = 15
          Width = 64
          Height = 13
          Caption = 'Clock_Speed'
        end
        object Clock: TSpinEdit
          Left = 41
          Top = 12
          Width = 99
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = ClockChange
        end
      end
      object Memory: TGroupBox
        Left = 226
        Top = 86
        Width = 343
        Height = 66
        Caption = 'Memory'
        TabOrder = 2
        object Label7: TLabel
          Left = 7
          Top = 14
          Width = 63
          Height = 13
          Caption = 'Low address:'
        end
        object Label8: TLabel
          Left = 7
          Top = 39
          Width = 65
          Height = 13
          Caption = 'High address:'
        end
        object Low_Address: TSpinEdit
          Left = 86
          Top = 11
          Width = 98
          Height = 22
          MaxValue = 2147483647
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = High_AddressChange
        end
        object High_Address: TSpinEdit
          Left = 86
          Top = 35
          Width = 98
          Height = 22
          MaxValue = 2147483647
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = High_AddressChange
        end
      end
    end
    object Signals_Tab_Sheet: TTabSheet
      Caption = 'Signals'
      ImageIndex = 1
    end
  end
end
