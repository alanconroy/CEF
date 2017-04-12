object DOS11_Tape_Form: TDOS11_Tape_Form
  Left = 1280
  Top = 176
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 484
  ClientWidth = 624
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
    Height = 484
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
      Left = 10
      Top = 107
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
      Left = 10
      Top = 139
      Width = 86
      Height = 22
      Caption = 'Add data...'
      OnClick = Add_Data_ButtonClick
    end
    object SpeedButton1: TSpeedButton
      Left = 10
      Top = 168
      Width = 86
      Height = 22
      Caption = 'Add Tape Mark'
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 11
      Top = 198
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
    Width = 464
    Height = 484
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
        Left = 5
        Top = 8
        Width = 170
        Height = 121
        BevelOuter = bvNone
        TabOrder = 0
        Visible = False
        object Label3: TLabel
          Left = 23
          Top = 25
          Width = 34
          Height = 13
          Caption = 'Name: '
        end
        object Name_Label: TLabel
          Left = 58
          Top = 25
          Width = 60
          Height = 13
          Caption = 'Name_Label'
        end
        object Label4: TLabel
          Left = 23
          Top = 41
          Width = 27
          Height = 13
          Caption = 'Type:'
        end
        object Type_Label: TLabel
          Left = 58
          Top = 41
          Width = 56
          Height = 13
          Caption = 'Type_Label'
        end
        object Label5: TLabel
          Left = 23
          Top = 73
          Width = 25
          Height = 13
          Caption = 'Prog:'
        end
        object Prog_Label: TLabel
          Left = 58
          Top = 73
          Width = 54
          Height = 13
          Caption = 'Prog_Label'
        end
        object Label6: TLabel
          Left = 23
          Top = 57
          Width = 21
          Height = 13
          Caption = 'Proj:'
        end
        object Proj_Label: TLabel
          Left = 58
          Top = 57
          Width = 50
          Height = 13
          Caption = 'Proj_Label'
        end
        object Label7: TLabel
          Left = 23
          Top = 105
          Width = 26
          Height = 13
          Caption = 'Date:'
        end
        object Date_Label: TLabel
          Left = 58
          Top = 105
          Width = 55
          Height = 13
          Caption = 'Date_Label'
        end
        object Label8: TLabel
          Left = 24
          Top = 89
          Width = 54
          Height = 13
          Caption = 'Protection: '
        end
        object Prot_Label: TLabel
          Left = 85
          Top = 89
          Width = 51
          Height = 13
          Caption = 'Prot_Label'
        end
        object Label9: TLabel
          Left = 13
          Top = 7
          Width = 60
          Height = 13
          Caption = 'Header label'
        end
      end
      object TM_Panel: TPanel
        Left = 16
        Top = 140
        Width = 185
        Height = 250
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
        Width = 456
        Height = 456
        Align = alClient
        TabOrder = 0
      end
    end
  end
end
