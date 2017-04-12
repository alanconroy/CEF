object Media_Manager: TMedia_Manager
  Left = 1196
  Top = 0
  ActiveControl = File_Name
  Caption = 'Media Manager'
  ClientHeight = 529
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 96
    Width = 716
    Height = 373
    Align = alClient
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 510
    Width = 716
    Height = 19
    Panels = <>
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 469
    Width = 716
    Height = 41
    Align = alBottom
    TabOrder = 2
    object BitBtn1: TBitBtn
      Left = 318
      Top = 7
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = BitBtn1Click
      Kind = bkClose
    end
  end
  object Header_Panel: TPanel
    Left = 0
    Top = 0
    Width = 716
    Height = 68
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label1: TLabel
      Left = 9
      Top = 7
      Width = 45
      Height = 13
      Caption = 'Filename:'
    end
    object SpeedButton1: TSpeedButton
      Left = 261
      Top = 3
      Width = 70
      Height = 22
      Caption = 'Browse...'
      OnClick = SpeedButton1Click
    end
    object Create_Button: TSpeedButton
      Left = 75
      Top = 40
      Width = 60
      Height = 22
      Caption = 'Create'
      Enabled = False
      OnClick = Create_ButtonClick
    end
    object Open_Button: TSpeedButton
      Left = 7
      Top = 40
      Width = 60
      Height = 22
      Caption = 'Open'
      Enabled = False
      OnClick = Open_ButtonClick
    end
    object Close_Button: TSpeedButton
      Left = 494
      Top = 40
      Width = 60
      Height = 22
      Caption = 'Close'
      Enabled = False
      OnClick = Close_ButtonClick
    end
    object Delete_Button: TSpeedButton
      Left = 563
      Top = 40
      Width = 60
      Height = 22
      Caption = 'Delete'
      Enabled = False
      OnClick = Delete_ButtonClick
    end
    object Convert_Button: TSpeedButton
      Left = 631
      Top = 40
      Width = 60
      Height = 22
      Caption = 'Convert'
      Enabled = False
      OnClick = Convert_ButtonClick
    end
    object Warning_Label: TLabel
      Left = 497
      Top = 7
      Width = 3
      Height = 13
    end
    object Edit_Header: TSpeedButton
      Left = 385
      Top = 40
      Width = 99
      Height = 22
      Caption = 'Edit Media Info...'
      Enabled = False
      OnClick = Edit_HeaderClick
    end
    object File_Name: TEdit
      Left = 61
      Top = 2
      Width = 189
      Height = 21
      TabOrder = 0
      OnChange = File_NameChange
      OnKeyDown = File_NameKeyDown
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 68
    Width = 716
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object Label2: TLabel
      Left = 5
      Top = 8
      Width = 51
      Height = 13
      Caption = 'File format:'
    end
    object Label3: TLabel
      Left = 249
      Top = 8
      Width = 26
      Height = 13
      Caption = 'Size: '
    end
    object Size_Label: TLabel
      Left = 278
      Top = 8
      Width = 34
      Height = 13
      Caption = '0 bytes'
    end
    object File_Format: TComboBox
      Left = 61
      Top = 2
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 0
      OnChange = File_FormatChange
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.TAP|Tape image|*.DSK|Disk image|*.*|All files'
    Title = 'Select Media File'
    Left = 337
    Top = 2
  end
  object SaveDialog1: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 373
    Top = 7
  end
end
