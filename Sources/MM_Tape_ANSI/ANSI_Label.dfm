object ANSI_Label_Form: TANSI_Label_Form
  Left = 1242
  Top = 263
  Width = 593
  Height = 386
  Caption = 'ANSI Label'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 309
    Width = 577
    Height = 41
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 28
      Top = 7
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 264
      Top = 6
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Pagecontrol: TPageControl
    Left = 0
    Top = 0
    Width = 577
    Height = 309
    ActivePage = EOF1
    Align = alClient
    TabOrder = 1
    object HDR: TTabSheet
      Caption = 'HDR1'
      object Label6: TLabel
        Left = 4
        Top = 7
        Width = 45
        Height = 13
        Caption = 'Filename:'
      end
      object Label8: TLabel
        Left = 4
        Top = 30
        Width = 36
        Height = 13
        Caption = 'File set:'
      end
      object Label7: TLabel
        Left = 4
        Top = 51
        Width = 39
        Height = 13
        Caption = 'Section:'
      end
      object Label10: TLabel
        Left = 4
        Top = 73
        Width = 52
        Height = 13
        Caption = 'Sequence:'
      end
      object Label9: TLabel
        Left = 4
        Top = 96
        Width = 63
        Height = 13
        Caption = 'Gen Number:'
      end
      object Label20: TLabel
        Left = 4
        Top = 117
        Width = 61
        Height = 13
        Caption = 'Gen Version:'
      end
      object Label21: TLabel
        Left = 4
        Top = 139
        Width = 66
        Height = 13
        Caption = 'Creation date:'
      end
      object Label22: TLabel
        Left = 4
        Top = 160
        Width = 73
        Height = 13
        Caption = 'Expiration date:'
      end
      object Label23: TLabel
        Left = 4
        Top = 182
        Width = 60
        Height = 13
        Caption = 'Accessibility:'
      end
      object Label24: TLabel
        Left = 4
        Top = 206
        Width = 60
        Height = 13
        Caption = 'Block count:'
      end
      object Label25: TLabel
        Left = 4
        Top = 230
        Width = 37
        Height = 13
        Caption = 'System:'
      end
      object HDR1_Filename: TEdit
        Tag = 10
        Left = 80
        Top = 3
        Width = 121
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_System: TEdit
        Tag = 13
        Left = 80
        Top = 223
        Width = 171
        Height = 21
        TabOrder = 10
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_Block_Count: TEdit
        Tag = 6
        Left = 80
        Top = 200
        Width = 97
        Height = 21
        TabOrder = 9
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_Accessibility: TEdit
        Tag = 1
        Left = 80
        Top = 177
        Width = 23
        Height = 21
        TabOrder = 8
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_Expiration_Date: TEdit
        Tag = 4
        Left = 80
        Top = 155
        Width = 76
        Height = 21
        TabOrder = 7
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_Creation_Date: TEdit
        Tag = 4
        Left = 80
        Top = 134
        Width = 79
        Height = 21
        TabOrder = 6
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_Gen_Version: TEdit
        Tag = 2
        Left = 80
        Top = 113
        Width = 40
        Height = 21
        TabOrder = 5
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_Gen_Number: TEdit
        Tag = 4
        Left = 80
        Top = 91
        Width = 80
        Height = 21
        TabOrder = 4
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_Sequence: TEdit
        Tag = 4
        Left = 80
        Top = 69
        Width = 83
        Height = 21
        TabOrder = 3
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_Section: TEdit
        Tag = 4
        Left = 80
        Top = 47
        Width = 85
        Height = 21
        TabOrder = 2
        OnKeyPress = Edit_KeyPress
      end
      object HDR1_File_Set: TEdit
        Tag = 6
        Left = 80
        Top = 25
        Width = 92
        Height = 21
        TabOrder = 1
        OnKeyPress = Edit_KeyPress
      end
    end
    object HDR2: TTabSheet
      Caption = 'HDR2'
      ImageIndex = 1
      object Label34: TLabel
        Left = 4
        Top = 110
        Width = 31
        Height = 13
        Caption = 'Offset:'
      end
      object Label33: TLabel
        Left = 4
        Top = 85
        Width = 46
        Height = 13
        Caption = 'Depends:'
      end
      object Label32: TLabel
        Left = 4
        Top = 58
        Width = 70
        Height = 13
        Caption = 'Record length:'
      end
      object Label30: TLabel
        Left = 4
        Top = 31
        Width = 62
        Height = 13
        Caption = 'Block length:'
      end
      object Label28: TLabel
        Left = 4
        Top = 7
        Width = 35
        Height = 13
        Caption = 'Format:'
      end
      object HDR2_Format: TEdit
        Tag = 1
        Left = 50
        Top = 2
        Width = 22
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit_KeyPress
      end
      object HDR2_Block_Length: TEdit
        Tag = 5
        Left = 73
        Top = 28
        Width = 85
        Height = 21
        TabOrder = 1
        OnKeyPress = Edit_KeyPress
      end
      object HDR2_Record_Length: TEdit
        Tag = 5
        Left = 78
        Top = 55
        Width = 85
        Height = 21
        TabOrder = 2
        OnKeyPress = Edit_KeyPress
      end
      object HDR2_Depends: TEdit
        Tag = 34
        Left = 59
        Top = 81
        Width = 213
        Height = 21
        TabOrder = 3
        OnKeyPress = Edit_KeyPress
      end
      object HDR2_Offset: TEdit
        Tag = 2
        Left = 51
        Top = 106
        Width = 36
        Height = 21
        TabOrder = 4
        OnKeyPress = Edit_KeyPress
      end
    end
    object EOF1: TTabSheet
      Caption = 'EOF1'
      ImageIndex = 2
      object Label96: TLabel
        Left = 4
        Top = 7
        Width = 45
        Height = 13
        Caption = 'Filename:'
      end
      object Label98: TLabel
        Left = 4
        Top = 30
        Width = 36
        Height = 13
        Caption = 'File set:'
      end
      object Label97: TLabel
        Left = 4
        Top = 51
        Width = 39
        Height = 13
        Caption = 'Section:'
      end
      object Label100: TLabel
        Left = 4
        Top = 73
        Width = 52
        Height = 13
        Caption = 'Sequence:'
      end
      object Label99: TLabel
        Left = 4
        Top = 96
        Width = 63
        Height = 13
        Caption = 'Gen Number:'
      end
      object Label80: TLabel
        Left = 4
        Top = 117
        Width = 61
        Height = 13
        Caption = 'Gen Version:'
      end
      object Label81: TLabel
        Left = 4
        Top = 139
        Width = 66
        Height = 13
        Caption = 'Creation date:'
      end
      object Label82: TLabel
        Left = 4
        Top = 160
        Width = 73
        Height = 13
        Caption = 'Expiration date:'
      end
      object Label83: TLabel
        Left = 4
        Top = 182
        Width = 60
        Height = 13
        Caption = 'Accessibility:'
      end
      object Label84: TLabel
        Left = 4
        Top = 206
        Width = 60
        Height = 13
        Caption = 'Block count:'
      end
      object Label85: TLabel
        Left = 4
        Top = 230
        Width = 37
        Height = 13
        Caption = 'System:'
      end
      object EOF1_Filename: TEdit
        Tag = 10
        Left = 80
        Top = 3
        Width = 121
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_System: TEdit
        Tag = 13
        Left = 80
        Top = 223
        Width = 171
        Height = 21
        TabOrder = 10
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_Block_Count: TEdit
        Tag = 6
        Left = 80
        Top = 200
        Width = 97
        Height = 21
        TabOrder = 9
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_Accessibility: TEdit
        Tag = 1
        Left = 80
        Top = 177
        Width = 23
        Height = 21
        TabOrder = 8
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_Expiration_Date: TEdit
        Tag = 4
        Left = 80
        Top = 155
        Width = 76
        Height = 21
        TabOrder = 7
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_Creation_Date: TEdit
        Tag = 4
        Left = 80
        Top = 134
        Width = 79
        Height = 21
        TabOrder = 6
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_Gen_Version: TEdit
        Tag = 2
        Left = 80
        Top = 113
        Width = 40
        Height = 21
        TabOrder = 5
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_Gen_Number: TEdit
        Tag = 4
        Left = 80
        Top = 91
        Width = 80
        Height = 21
        TabOrder = 4
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_Sequence: TEdit
        Tag = 4
        Left = 80
        Top = 69
        Width = 83
        Height = 21
        TabOrder = 3
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_Section: TEdit
        Tag = 4
        Left = 80
        Top = 47
        Width = 85
        Height = 21
        TabOrder = 2
        OnKeyPress = Edit_KeyPress
      end
      object EOF1_File_Set: TEdit
        Tag = 6
        Left = 80
        Top = 25
        Width = 92
        Height = 21
        TabOrder = 1
        OnKeyPress = Edit_KeyPress
      end
    end
    object EOF2: TTabSheet
      Caption = 'EOF2'
      ImageIndex = 3
      object Label134: TLabel
        Left = 4
        Top = 110
        Width = 31
        Height = 13
        Caption = 'Offset:'
      end
      object Label133: TLabel
        Left = 4
        Top = 85
        Width = 46
        Height = 13
        Caption = 'Depends:'
      end
      object Label132: TLabel
        Left = 4
        Top = 58
        Width = 70
        Height = 13
        Caption = 'Record length:'
      end
      object Label130: TLabel
        Left = 4
        Top = 31
        Width = 62
        Height = 13
        Caption = 'Block length:'
      end
      object Label128: TLabel
        Left = 4
        Top = 7
        Width = 35
        Height = 13
        Caption = 'Format:'
      end
      object EOF2_Format: TEdit
        Tag = 1
        Left = 50
        Top = 2
        Width = 22
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit_KeyPress
      end
      object EOF2_Block_Length: TEdit
        Tag = 5
        Left = 73
        Top = 28
        Width = 85
        Height = 21
        TabOrder = 1
        OnKeyPress = Edit_KeyPress
      end
      object EOF2_Record_Length: TEdit
        Tag = 5
        Left = 78
        Top = 55
        Width = 85
        Height = 21
        TabOrder = 2
        OnKeyPress = Edit_KeyPress
      end
      object EOF2_Depends: TEdit
        Tag = 34
        Left = 59
        Top = 81
        Width = 213
        Height = 21
        TabOrder = 3
        OnKeyPress = Edit_KeyPress
      end
      object EOF2_Offset: TEdit
        Tag = 2
        Left = 51
        Top = 106
        Width = 36
        Height = 21
        TabOrder = 4
        OnKeyPress = Edit_KeyPress
      end
    end
    object VOL1: TTabSheet
      Caption = 'VOL1'
      ImageIndex = 4
      object Label1: TLabel
        Left = 6
        Top = 9
        Width = 52
        Height = 13
        Caption = 'Volume ID:'
      end
      object Label2: TLabel
        Left = 6
        Top = 32
        Width = 60
        Height = 13
        Caption = 'Accessibility:'
      end
      object Label3: TLabel
        Left = 7
        Top = 56
        Width = 34
        Height = 13
        Caption = 'Owner:'
      end
      object Label4: TLabel
        Left = 5
        Top = 80
        Width = 38
        Height = 13
        Caption = 'Version:'
      end
      object VOL1_Volume_ID: TEdit
        Tag = 5
        Left = 63
        Top = 4
        Width = 121
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit_KeyPress
      end
      object VOL1_Accessibility: TEdit
        Tag = 1
        Left = 71
        Top = 27
        Width = 23
        Height = 21
        TabOrder = 1
      end
      object VOL1_Owner: TEdit
        Tag = 13
        Left = 47
        Top = 51
        Width = 121
        Height = 21
        TabOrder = 2
      end
      object VOL1_Version: TEdit
        Tag = 1
        Left = 50
        Top = 77
        Width = 22
        Height = 21
        TabOrder = 3
        Text = '3'
      end
    end
    object EOV1: TTabSheet
      Caption = 'EOV1'
      ImageIndex = 5
      object Label76: TLabel
        Left = 4
        Top = 7
        Width = 45
        Height = 13
        Caption = 'Filename:'
      end
      object Label78: TLabel
        Left = 4
        Top = 30
        Width = 36
        Height = 13
        Caption = 'File set:'
      end
      object Label77: TLabel
        Left = 4
        Top = 51
        Width = 39
        Height = 13
        Caption = 'Section:'
      end
      object Label60: TLabel
        Left = 4
        Top = 73
        Width = 52
        Height = 13
        Caption = 'Sequence:'
      end
      object Label79: TLabel
        Left = 4
        Top = 96
        Width = 63
        Height = 13
        Caption = 'Gen Number:'
      end
      object Label70: TLabel
        Left = 4
        Top = 117
        Width = 61
        Height = 13
        Caption = 'Gen Version:'
      end
      object Label61: TLabel
        Left = 4
        Top = 139
        Width = 66
        Height = 13
        Caption = 'Creation date:'
      end
      object Label62: TLabel
        Left = 4
        Top = 160
        Width = 73
        Height = 13
        Caption = 'Expiration date:'
      end
      object Label63: TLabel
        Left = 4
        Top = 182
        Width = 60
        Height = 13
        Caption = 'Accessibility:'
      end
      object Label64: TLabel
        Left = 4
        Top = 206
        Width = 60
        Height = 13
        Caption = 'Block count:'
      end
      object Label65: TLabel
        Left = 4
        Top = 230
        Width = 37
        Height = 13
        Caption = 'System:'
      end
      object EOV1_Filename: TEdit
        Tag = 10
        Left = 80
        Top = 3
        Width = 121
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_System: TEdit
        Tag = 13
        Left = 80
        Top = 223
        Width = 171
        Height = 21
        TabOrder = 10
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_Block_Count: TEdit
        Tag = 6
        Left = 80
        Top = 200
        Width = 97
        Height = 21
        TabOrder = 9
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_Accessibility: TEdit
        Tag = 1
        Left = 80
        Top = 177
        Width = 23
        Height = 21
        TabOrder = 8
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_Expiration_Date: TEdit
        Tag = 4
        Left = 80
        Top = 155
        Width = 76
        Height = 21
        TabOrder = 7
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_Creation_Date: TEdit
        Tag = 4
        Left = 80
        Top = 134
        Width = 79
        Height = 21
        TabOrder = 6
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_Gen_Version: TEdit
        Tag = 2
        Left = 80
        Top = 113
        Width = 40
        Height = 21
        TabOrder = 5
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_Gen_Number: TEdit
        Tag = 4
        Left = 80
        Top = 91
        Width = 80
        Height = 21
        TabOrder = 4
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_Sequence: TEdit
        Tag = 4
        Left = 80
        Top = 69
        Width = 83
        Height = 21
        TabOrder = 3
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_Section: TEdit
        Tag = 4
        Left = 80
        Top = 47
        Width = 85
        Height = 21
        TabOrder = 2
        OnKeyPress = Edit_KeyPress
      end
      object EOV1_File_Set: TEdit
        Tag = 6
        Left = 80
        Top = 25
        Width = 92
        Height = 21
        TabOrder = 1
        OnKeyPress = Edit_KeyPress
      end
    end
    object EOV2: TTabSheet
      Caption = 'EOV2'
      ImageIndex = 6
      object Label234: TLabel
        Left = 4
        Top = 110
        Width = 31
        Height = 13
        Caption = 'Offset:'
      end
      object Label233: TLabel
        Left = 4
        Top = 85
        Width = 46
        Height = 13
        Caption = 'Depends:'
      end
      object Label232: TLabel
        Left = 4
        Top = 58
        Width = 70
        Height = 13
        Caption = 'Record length:'
      end
      object Label230: TLabel
        Left = 4
        Top = 31
        Width = 62
        Height = 13
        Caption = 'Block length:'
      end
      object Label228: TLabel
        Left = 4
        Top = 7
        Width = 35
        Height = 13
        Caption = 'Format:'
      end
      object EOV2_Format: TEdit
        Tag = 1
        Left = 50
        Top = 2
        Width = 22
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit_KeyPress
      end
      object EOV2_Block_Length: TEdit
        Tag = 5
        Left = 73
        Top = 28
        Width = 85
        Height = 21
        TabOrder = 1
        OnKeyPress = Edit_KeyPress
      end
      object EOV2_Record_Length: TEdit
        Tag = 5
        Left = 78
        Top = 55
        Width = 85
        Height = 21
        TabOrder = 2
        OnKeyPress = Edit_KeyPress
      end
      object EOV2_Depends: TEdit
        Tag = 34
        Left = 59
        Top = 81
        Width = 213
        Height = 21
        TabOrder = 3
        OnKeyPress = Edit_KeyPress
      end
      object EOV2_Offset: TEdit
        Tag = 2
        Left = 51
        Top = 106
        Width = 36
        Height = 21
        TabOrder = 4
        OnKeyPress = Edit_KeyPress
      end
    end
  end
end
