object ANSI_Tape_Form: TANSI_Tape_Form
  Left = 1202
  Top = 144
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 695
  ClientWidth = 564
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Command_Panel: TPanel
    Left = 0
    Top = 0
    Width = 160
    Height = 695
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Record_Size_Label: TLabel
      Left = 76
      Top = 66
      Width = 3
      Height = 13
    end
    object Full_Filename: TLabel
      Left = 11
      Top = 85
      Width = 3
      Height = 13
    end
    object Error: TLabel
      Left = 10
      Top = 104
      Width = 3
      Height = 13
    end
    object Copy_Files_Button: TSpeedButton
      Left = 11
      Top = 138
      Width = 86
      Height = 22
      Caption = 'Copy files...'
      OnClick = Copy_Files_ButtonClick
    end
    object Label1: TLabel
      Left = 7
      Top = 10
      Width = 41
      Height = 13
      Caption = 'Record: '
    end
    object Current_Record_Label: TLabel
      Left = 117
      Top = 10
      Width = 18
      Height = 13
      Caption = 'of 0'
    end
    object Rewind_Button: TSpeedButton
      Left = 12
      Top = 33
      Width = 25
      Height = 22
      Caption = 'BOT'
      OnClick = Rewind_ButtonClick
    end
    object FF_Button: TSpeedButton
      Left = 40
      Top = 33
      Width = 25
      Height = 22
      Caption = 'EOT'
      OnClick = FF_ButtonClick
    end
    object Label2: TLabel
      Left = 9
      Top = 66
      Width = 62
      Height = 13
      Caption = 'Record size: '
    end
    object Add_Data_Button: TSpeedButton
      Left = 11
      Top = 170
      Width = 86
      Height = 22
      Caption = 'Add data...'
      OnClick = Add_Data_ButtonClick
    end
    object SpeedButton1: TSpeedButton
      Left = 11
      Top = 199
      Width = 86
      Height = 22
      Caption = 'Add Tape Mark'
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 11
      Top = 227
      Width = 86
      Height = 22
      Caption = 'Add Label'
      OnClick = SpeedButton2Click
    end
    object Record_Selector: TSpinEdit
      Left = 46
      Top = 7
      Width = 70
      Height = 22
      MaxValue = 1
      MinValue = 1
      TabOrder = 0
      Value = 1
      OnChange = Record_SelectorChange
    end
  end
  object PageControl1: TPageControl
    Left = 160
    Top = 0
    Width = 404
    Height = 695
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Formatted'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label_Panel: TPanel
        Left = 12
        Top = 17
        Width = 170
        Height = 245
        BevelOuter = bvNone
        TabOrder = 0
        Visible = False
        object Label3: TLabel
          Left = 20
          Top = 25
          Width = 43
          Height = 13
          Caption = 'Label ID:'
        end
        object HDR1_Label: TLabel
          Left = 72
          Top = 25
          Width = 24
          Height = 13
          Caption = 'HDR'
        end
        object Label4: TLabel
          Left = 20
          Top = 59
          Width = 45
          Height = 13
          Caption = 'Filename:'
        end
        object Filename_Label: TLabel
          Left = 69
          Top = 59
          Width = 74
          Height = 13
          Caption = 'Filename_Label'
        end
        object Label5: TLabel
          Left = 20
          Top = 91
          Width = 39
          Height = 13
          Caption = 'Section:'
        end
        object Section_Label: TLabel
          Left = 65
          Top = 92
          Width = 68
          Height = 13
          Caption = 'Section_Label'
        end
        object Label6: TLabel
          Left = 20
          Top = 75
          Width = 36
          Height = 13
          Caption = 'File set:'
        end
        object File_Set_Label: TLabel
          Left = 63
          Top = 75
          Width = 70
          Height = 13
          Caption = 'File_Set_Label'
        end
        object Label7: TLabel
          Left = 20
          Top = 123
          Width = 63
          Height = 13
          Caption = 'Gen Number:'
        end
        object Gen_Number_Label: TLabel
          Left = 88
          Top = 124
          Width = 95
          Height = 13
          Caption = 'Gen_Number_Label'
        end
        object Label8: TLabel
          Left = 20
          Top = 107
          Width = 52
          Height = 13
          Caption = 'Sequence:'
        end
        object Sequence_Label: TLabel
          Left = 79
          Top = 108
          Width = 81
          Height = 13
          Caption = 'Sequence_Label'
        end
        object Label9: TLabel
          Left = 10
          Top = 7
          Width = 26
          Height = 13
          Caption = 'Label'
        end
        object Label20: TLabel
          Left = 20
          Top = 140
          Width = 61
          Height = 13
          Caption = 'Gen Version:'
        end
        object Label21: TLabel
          Left = 20
          Top = 155
          Width = 66
          Height = 13
          Caption = 'Creation date:'
        end
        object Label22: TLabel
          Left = 20
          Top = 171
          Width = 73
          Height = 13
          Caption = 'Expiration date:'
        end
        object Label23: TLabel
          Left = 20
          Top = 187
          Width = 60
          Height = 13
          Caption = 'Accessibility:'
        end
        object Label24: TLabel
          Left = 20
          Top = 204
          Width = 60
          Height = 13
          Caption = 'Block count:'
        end
        object Label25: TLabel
          Left = 20
          Top = 218
          Width = 37
          Height = 13
          Caption = 'System:'
        end
        object Gen_Version_Label: TLabel
          Left = 86
          Top = 139
          Width = 93
          Height = 13
          Caption = 'Gen_Version_Label'
        end
        object Creation_Date_Label: TLabel
          Left = 91
          Top = 155
          Width = 100
          Height = 13
          Caption = 'Creation_Date_Label'
        end
        object Expiration_Date_Label: TLabel
          Left = 95
          Top = 173
          Width = 107
          Height = 13
          Caption = 'Expiration_Date_Label'
        end
        object Accessibility_Label: TLabel
          Left = 83
          Top = 188
          Width = 89
          Height = 13
          Caption = 'Accessibility_Label'
        end
        object Block_Count_Label: TLabel
          Left = 84
          Top = 204
          Width = 93
          Height = 13
          Caption = 'Block_Count_Label'
        end
        object System_Label: TLabel
          Left = 59
          Top = 218
          Width = 66
          Height = 13
          Caption = 'System_Label'
        end
        object Label26: TLabel
          Left = 20
          Top = 42
          Width = 40
          Height = 13
          Caption = 'Number:'
        end
        object HDR1_Number: TLabel
          Left = 65
          Top = 43
          Width = 6
          Height = 13
          Caption = '1'
        end
      end
      object TM_Panel: TPanel
        Left = 17
        Top = 263
        Width = 185
        Height = 211
        Alignment = taLeftJustify
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        object Label10: TLabel
          Left = 8
          Top = 14
          Width = 61
          Height = 13
          Caption = '   Tape Mark'
        end
      end
      object Vol_Label_Panel: TPanel
        Left = 183
        Top = 22
        Width = 185
        Height = 239
        BevelOuter = bvNone
        TabOrder = 2
        object Label11: TLabel
          Left = 12
          Top = 22
          Width = 43
          Height = 13
          Caption = 'Label ID:'
        end
        object Vol_Lab_ID: TLabel
          Left = 62
          Top = 22
          Width = 56
          Height = 13
          Caption = 'Vol_Lab_ID'
        end
        object Label12: TLabel
          Left = 1
          Top = 5
          Width = 64
          Height = 13
          Caption = 'Volume Label'
        end
        object Label13: TLabel
          Left = 12
          Top = 38
          Width = 54
          Height = 13
          Caption = 'Label Num:'
        end
        object Label_Number: TLabel
          Left = 72
          Top = 38
          Width = 69
          Height = 13
          Caption = 'Label_Number'
        end
        object Label14: TLabel
          Left = 12
          Top = 53
          Width = 52
          Height = 13
          Caption = 'Volume ID:'
        end
        object Vol_ID: TLabel
          Left = 70
          Top = 54
          Width = 32
          Height = 13
          Caption = 'Vol_ID'
        end
        object Label15: TLabel
          Left = 12
          Top = 69
          Width = 60
          Height = 13
          Caption = 'Accessibility:'
        end
        object Accessibility: TLabel
          Left = 77
          Top = 69
          Width = 57
          Height = 13
          Caption = 'Accessibility'
        end
        object Label16: TLabel
          Left = 12
          Top = 85
          Width = 34
          Height = 13
          Caption = 'Owner:'
        end
        object Vol_Owner: TLabel
          Left = 52
          Top = 85
          Width = 52
          Height = 13
          Caption = 'Vol_Owner'
        end
        object Label17: TLabel
          Left = 14
          Top = 138
          Width = 38
          Height = 13
          Caption = 'Version:'
        end
        object Version: TLabel
          Left = 55
          Top = 138
          Width = 35
          Height = 13
          Caption = 'Version'
        end
        object Label18: TLabel
          Left = 54
          Top = 100
          Width = 21
          Height = 13
          Caption = 'Proj:'
        end
        object Label19: TLabel
          Left = 54
          Top = 116
          Width = 25
          Height = 13
          Caption = 'Prog:'
        end
        object Proj1: TLabel
          Left = 89
          Top = 100
          Width = 50
          Height = 13
          Caption = 'Proj_Label'
        end
        object Prog1: TLabel
          Left = 89
          Top = 116
          Width = 54
          Height = 13
          Caption = 'Prog_Label'
        end
      end
      object HDR2_Panel: TPanel
        Left = 199
        Top = 261
        Width = 233
        Height = 194
        BevelOuter = bvNone
        TabOrder = 3
        object Label27: TLabel
          Left = 20
          Top = 25
          Width = 43
          Height = 13
          Caption = 'Label ID:'
        end
        object HDR2_Label: TLabel
          Left = 72
          Top = 25
          Width = 24
          Height = 13
          Caption = 'HDR'
        end
        object Label29: TLabel
          Left = 20
          Top = 42
          Width = 40
          Height = 13
          Caption = 'Number:'
        end
        object HDR2_Number: TLabel
          Left = 65
          Top = 43
          Width = 6
          Height = 13
          Caption = '2'
        end
        object Label31: TLabel
          Left = 10
          Top = 7
          Width = 26
          Height = 13
          Caption = 'Label'
        end
        object Label28: TLabel
          Left = 20
          Top = 59
          Width = 35
          Height = 13
          Caption = 'Format:'
        end
        object Label30: TLabel
          Left = 20
          Top = 75
          Width = 62
          Height = 13
          Caption = 'Block length:'
        end
        object Label32: TLabel
          Left = 20
          Top = 91
          Width = 70
          Height = 13
          Caption = 'Record length:'
        end
        object Label33: TLabel
          Left = 20
          Top = 107
          Width = 46
          Height = 13
          Caption = 'Depends:'
        end
        object Label34: TLabel
          Left = 20
          Top = 123
          Width = 31
          Height = 13
          Caption = 'Offset:'
        end
        object HDR2_Offset: TLabel
          Left = 56
          Top = 123
          Width = 95
          Height = 13
          Caption = 'Gen_Number_Label'
        end
        object HDR2_Depends: TLabel
          Left = 73
          Top = 108
          Width = 81
          Height = 13
          Caption = 'Sequence_Label'
        end
        object HDR2_Record_Length: TLabel
          Left = 94
          Top = 91
          Width = 68
          Height = 13
          Caption = 'Section_Label'
        end
        object HDR2_Block_Length: TLabel
          Left = 87
          Top = 75
          Width = 70
          Height = 13
          Caption = 'File_Set_Label'
        end
        object HDR2_Format: TLabel
          Left = 61
          Top = 60
          Width = 74
          Height = 13
          Caption = 'Filename_Label'
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Raw Data'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Data_Panel: TPanel
        Left = 0
        Top = 0
        Width = 486
        Height = 456
        Align = alClient
        TabOrder = 0
      end
    end
  end
end
