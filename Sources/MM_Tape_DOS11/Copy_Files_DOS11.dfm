object Copy_Tape_DOS11_Form: TCopy_Tape_DOS11_Form
  Left = 1204
  Top = 329
  Caption = 'Copy files'
  ClientHeight = 611
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 7
    Width = 63
    Height = 13
    Caption = 'Files on tape:'
  end
  object Label2: TLabel
    Left = 336
    Top = 6
    Width = 52
    Height = 13
    Caption = 'File to add:'
  end
  object Label3: TLabel
    Left = 360
    Top = 25
    Width = 74
    Height = 13
    Caption = 'File name/path:'
  end
  object SpeedButton1: TSpeedButton
    Left = 634
    Top = 23
    Width = 56
    Height = 22
    Caption = 'Browse...'
    OnClick = SpeedButton1Click
  end
  object Label4: TLabel
    Left = 439
    Top = 55
    Width = 84
    Height = 13
    Caption = 'File spec on tape:'
  end
  object Label5: TLabel
    Left = 480
    Top = 76
    Width = 21
    Height = 13
    Caption = 'Proj:'
  end
  object Label6: TLabel
    Left = 480
    Top = 105
    Width = 25
    Height = 13
    Caption = 'Prog:'
  end
  object Label7: TLabel
    Left = 480
    Top = 132
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label8: TLabel
    Left = 480
    Top = 155
    Width = 18
    Height = 13
    Caption = 'Ext:'
  end
  object Label9: TLabel
    Left = 480
    Top = 184
    Width = 22
    Height = 13
    Caption = 'Prot:'
  end
  object Copy_Button: TSpeedButton
    Left = 444
    Top = 259
    Width = 68
    Height = 22
    Caption = 'Copy file(s)'
    Enabled = False
    OnClick = Copy_ButtonClick
  end
  object Cancel_Copy_Button: TSpeedButton
    Left = 524
    Top = 259
    Width = 78
    Height = 22
    Caption = 'Cancel copy'
    Enabled = False
    OnClick = Cancel_Copy_ButtonClick
  end
  object Label10: TLabel
    Left = 480
    Top = 209
    Width = 44
    Height = 13
    Caption = 'Blocking:'
  end
  object Label11: TLabel
    Left = 480
    Top = 233
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Copy_From_Tape_Button: TSpeedButton
    Left = 321
    Top = 346
    Width = 92
    Height = 22
    Caption = 'Copy from tape'
    Enabled = False
    OnClick = Copy_From_Tape_ButtonClick
  end
  object Label12: TLabel
    Left = 423
    Top = 350
    Width = 12
    Height = 13
    Caption = 'to:'
  end
  object SpeedButton3: TSpeedButton
    Left = 635
    Top = 345
    Width = 57
    Height = 22
    Caption = 'Browse...'
    OnClick = SpeedButton3Click
  end
  object Bottom_Panel: TPanel
    Left = 0
    Top = 570
    Width = 700
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 573
    object BitBtn1: TBitBtn
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkClose
    end
  end
  object File_Name: TEdit
    Left = 437
    Top = 22
    Width = 186
    Height = 21
    TabOrder = 1
    OnChange = File_NameChange
  end
  object Proj: TSpinEdit
    Left = 508
    Top = 72
    Width = 60
    Height = 22
    MaxValue = 255
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object Prog: TSpinEdit
    Left = 509
    Top = 101
    Width = 60
    Height = 22
    MaxValue = 255
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object Name: TEdit
    Left = 518
    Top = 128
    Width = 121
    Height = 21
    TabOrder = 4
    OnChange = NameChange
    OnKeyPress = NameKeyPress
  end
  object Ext: TEdit
    Left = 519
    Top = 152
    Width = 66
    Height = 21
    TabOrder = 5
    OnChange = NameChange
    OnKeyPress = ExtKeyPress
  end
  object Prot: TSpinEdit
    Left = 519
    Top = 177
    Width = 69
    Height = 22
    MaxValue = 155
    MinValue = 0
    TabOrder = 6
    Value = 155
  end
  object On_Tape: TListBox
    Left = 8
    Top = 25
    Width = 311
    Height = 532
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 7
    OnClick = On_TapeClick
  end
  object Blocking: TSpinEdit
    Left = 528
    Top = 204
    Width = 73
    Height = 22
    MaxValue = 32768
    MinValue = 0
    TabOrder = 8
    Value = 512
  end
  object Date_Value: TSpinEdit
    Left = 513
    Top = 230
    Width = 73
    Height = 22
    MaxValue = 65535
    MinValue = 0
    TabOrder = 9
    Value = 25001
  end
  object Output_File: TEdit
    Left = 437
    Top = 347
    Width = 194
    Height = 21
    TabOrder = 10
  end
  object Output_Text: TCheckBox
    Left = 439
    Top = 376
    Width = 97
    Height = 17
    Caption = 'Text'
    TabOrder = 11
  end
  object Include_PPN: TCheckBox
    Left = 438
    Top = 398
    Width = 164
    Height = 17
    Caption = 'Include PPN in name'
    TabOrder = 12
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'File(s) to copy to tape'
    Left = 636
    Top = 58
  end
  object SaveDialog1: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Output file'
    Left = 665
    Top = 374
  end
end
