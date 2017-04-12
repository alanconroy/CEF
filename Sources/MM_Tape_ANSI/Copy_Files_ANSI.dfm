object Copy_Tape_ANSI_Form: TCopy_Tape_ANSI_Form
  Left = 1204
  Top = 256
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
  object Label7: TLabel
    Left = 480
    Top = 79
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Copy_Button: TSpeedButton
    Left = 447
    Top = 312
    Width = 68
    Height = 22
    Caption = 'Copy file(s)'
    Enabled = False
    OnClick = Copy_ButtonClick
  end
  object Cancel_Copy_Button: TSpeedButton
    Left = 527
    Top = 312
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
    Left = 479
    Top = 103
    Width = 66
    Height = 13
    Caption = 'Creation date:'
  end
  object Copy_From_Tape_Button: TSpeedButton
    Left = 321
    Top = 415
    Width = 92
    Height = 22
    Caption = 'Copy from tape'
    Enabled = False
    OnClick = Copy_From_Tape_ButtonClick
  end
  object Label12: TLabel
    Left = 423
    Top = 419
    Width = 12
    Height = 13
    Caption = 'to:'
  end
  object SpeedButton3: TSpeedButton
    Left = 635
    Top = 414
    Width = 57
    Height = 22
    Caption = 'Browse...'
    OnClick = SpeedButton3Click
  end
  object Label8: TLabel
    Left = 480
    Top = 156
    Width = 62
    Height = 13
    Caption = 'Block length:'
  end
  object Label9: TLabel
    Left = 480
    Top = 178
    Width = 70
    Height = 13
    Caption = 'Record length:'
  end
  object Label13: TLabel
    Left = 482
    Top = 259
    Width = 70
    Height = 13
    Caption = 'Record format:'
  end
  object Label5: TLabel
    Left = 479
    Top = 129
    Width = 73
    Height = 13
    Caption = 'Expiration date:'
  end
  object Label6: TLabel
    Left = 480
    Top = 236
    Width = 65
    Height = 13
    Caption = 'System Code:'
  end
  object Label14: TLabel
    Left = 480
    Top = 282
    Width = 61
    Height = 13
    Caption = 'Form control:'
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
  object Name: TEdit
    Left = 518
    Top = 75
    Width = 174
    Height = 21
    TabOrder = 2
    OnChange = NameChange
    OnKeyPress = NameKeyPress
  end
  object On_Tape: TListBox
    Left = 8
    Top = 25
    Width = 311
    Height = 532
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 3
    OnClick = On_TapeClick
  end
  object Blocking: TSpinEdit
    Left = 528
    Top = 204
    Width = 73
    Height = 22
    MaxValue = 32768
    MinValue = 0
    TabOrder = 4
    Value = 2048
  end
  object Output_File: TEdit
    Left = 437
    Top = 416
    Width = 194
    Height = 21
    TabOrder = 5
  end
  object Output_Text: TCheckBox
    Left = 439
    Top = 445
    Width = 97
    Height = 17
    Caption = 'Text'
    TabOrder = 6
  end
  object Include_PPN: TCheckBox
    Left = 438
    Top = 467
    Width = 164
    Height = 17
    Caption = 'Include PPN in name'
    TabOrder = 7
  end
  object Date_Value: TEdit
    Left = 552
    Top = 100
    Width = 121
    Height = 21
    TabOrder = 8
    OnKeyPress = Expiration_DateKeyPress
  end
  object Block_Length: TSpinEdit
    Left = 545
    Top = 151
    Width = 101
    Height = 22
    MaxValue = 99999
    MinValue = 1
    TabOrder = 9
    Value = 512
  end
  object Record_Length: TSpinEdit
    Left = 550
    Top = 175
    Width = 100
    Height = 22
    MaxValue = 99999
    MinValue = 1
    TabOrder = 10
    Value = 1
  end
  object Record_Format: TEdit
    Left = 562
    Top = 256
    Width = 28
    Height = 21
    TabOrder = 11
    Text = 'U'
    OnKeyPress = Form_ControlKeyPress
  end
  object Expiration_Date: TEdit
    Left = 557
    Top = 125
    Width = 121
    Height = 21
    TabOrder = 12
    OnKeyPress = Expiration_DateKeyPress
  end
  object System_Code: TEdit
    Left = 551
    Top = 232
    Width = 121
    Height = 21
    TabOrder = 13
    OnKeyPress = System_CodeKeyPress
  end
  object Form_Control: TEdit
    Left = 562
    Top = 279
    Width = 28
    Height = 21
    TabOrder = 14
    Text = 'U'
    OnKeyPress = Form_ControlKeyPress
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
    Top = 443
  end
end
