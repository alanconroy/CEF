object Options_Form: TOptions_Form
  Left = 907
  Top = 670
  Caption = 'Options'
  ClientHeight = 312
  ClientWidth = 757
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
    Top = 279
    Width = 757
    Height = 33
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 86
      Top = 6
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkOK
    end
    object Cancel_Button: TBitBtn
      Left = 428
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 757
    Height = 279
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Assembler'
      object Generate_Listings: TCheckBox
        Left = 8
        Top = 8
        Width = 340
        Height = 14
        Caption = 'Generate list files'
        TabOrder = 0
      end
      object Generate_Symbol_Table: TCheckBox
        Left = 8
        Top = 27
        Width = 142
        Height = 14
        Caption = 'Generate Symbol table'
        TabOrder = 1
      end
      object Generate_XRef_List: TCheckBox
        Left = 8
        Top = 44
        Width = 214
        Height = 14
        Caption = 'Generate cross-reference list'
        TabOrder = 2
      end
      object Physical: TCheckBox
        Left = 8
        Top = 62
        Width = 232
        Height = 17
        Caption = 'Assemble into physical address space'
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Threading'
      ImageIndex = 1
      object Label1: TLabel
        Left = 7
        Top = 5
        Width = 61
        Height = 13
        Caption = 'Max threads:'
      end
      object Label2: TLabel
        Left = 7
        Top = 33
        Width = 70
        Height = 13
        Caption = 'Thread priority:'
      end
      object Max_Threads: TSpinEdit
        Left = 78
        Top = 1
        Width = 45
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object Thread_Priority: TComboBox
        Left = 88
        Top = 30
        Width = 170
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 3
        TabOrder = 1
        Text = 'Normal'
        Items.Strings = (
          'Idle only (not recommended)'
          'Lower'
          'Low'
          'Normal'
          'High'
          'Higher'
          'Real-time (not recommended)')
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Master Clock'
      ImageIndex = 2
      object Label3: TLabel
        Left = 8
        Top = 29
        Width = 30
        Height = 13
        Caption = 'Mode:'
      end
      object Immediate_Mode_Unblock: TCheckBox
        Left = 8
        Top = 57
        Width = 306
        Height = 17
        Caption = 'Unblock all components after immediate mode execution'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object Clock_Enabled: TCheckBox
        Left = 8
        Top = 4
        Width = 97
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object Clock_Mode: TComboBox
        Left = 47
        Top = 25
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 2
        Text = 'Default'
        Items.Strings = (
          'Default'
          'Ignore'
          'Synchronize')
      end
    end
  end
end
