object Load_Component_Form: TLoad_Component_Form
  Left = 0
  Top = 0
  Caption = 'LOADING COMPONENT LIST - PLEASE WAIT...'
  ClientHeight = 306
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 160
    Width = 46
    Height = 13
    Caption = 'Filename:'
  end
  object SpeedButton1: TSpeedButton
    Left = 545
    Top = 157
    Width = 64
    Height = 22
    Caption = 'Browse...'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 9
    Top = 186
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label3: TLabel
    Left = 8
    Top = 212
    Width = 39
    Height = 13
    Caption = 'Domain:'
  end
  object Label4: TLabel
    Left = 10
    Top = 237
    Width = 32
    Height = 13
    Caption = 'Setup:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 265
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 0
    object Load_Button: TBitBtn
      Left = 31
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Load'
      Enabled = False
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn1: TBitBtn
      Left = 281
      Top = 11
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 635
    Height = 150
    Align = alTop
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Type'
        Width = 100
      end
      item
        Caption = 'Version'
      end
      item
        Caption = 'Emulation'
        Width = 330
      end>
    IconOptions.Arrangement = iaLeft
    Items.ItemData = {
      01400000000100000000000000FFFFFFFFFFFFFFFF0000000000000000154C00
      6F006100640069006E006700200043006F006D0070006F006E0065006E007400
      73002E002E002E00}
    SortType = stText
    TabOrder = 1
    ViewStyle = vsReport
    OnClick = ListView1Click
  end
  object Filename: TEdit
    Left = 63
    Top = 156
    Width = 469
    Height = 21
    TabOrder = 2
    OnChange = FilenameChange
  end
  object Component_Name: TEdit
    Left = 63
    Top = 182
    Width = 469
    Height = 21
    TabOrder = 3
  end
  object Domain: TEdit
    Left = 63
    Top = 209
    Width = 469
    Height = 21
    TabOrder = 4
  end
  object Setup: TEdit
    Left = 63
    Top = 234
    Width = 469
    Height = 21
    TabOrder = 5
  end
  object Open_Component_Dialog: TOpenDialog
    DefaultExt = 'dll'
    Filter = 'Dynamic Link Libraries|*.dll'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open Component'
    Left = 600
    Top = 266
  end
end
