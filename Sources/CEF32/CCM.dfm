object Component_Connection_Manager: TComponent_Connection_Manager
  Left = 200
  Top = 126
  HelpContext = 1
  Caption = 'Component Connection Manager'
  ClientHeight = 500
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 7
    Top = 6
    Width = 62
    Height = 13
    Caption = 'Components:'
  end
  object Button_Panel: TPanel
    Left = 0
    Top = 467
    Width = 782
    Height = 33
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 472
    ExplicitWidth = 790
    object OK_Button: TBitBtn
      Left = 12
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 0
      Kind = bkOK
    end
    object Help_Button: TBitBtn
      Left = 524
      Top = 5
      Width = 61
      Height = 24
      TabOrder = 1
      OnClick = Help_ButtonClick
      Kind = bkHelp
    end
  end
  object Component_LB: TListBox
    Left = 6
    Top = 27
    Width = 187
    Height = 338
    ItemHeight = 13
    TabOrder = 1
    OnClick = Component_LBClick
  end
  object Panel1: TPanel
    Left = 202
    Top = 5
    Width = 437
    Height = 365
    BevelOuter = bvLowered
    TabOrder = 2
    object Label2: TLabel
      Left = 7
      Top = 5
      Width = 32
      Height = 13
      Caption = 'Inputs:'
    end
    object Label3: TLabel
      Left = 222
      Top = 5
      Width = 40
      Height = 13
      Caption = 'Outputs:'
    end
    object Delete_Input: TSpeedButton
      Left = 13
      Top = 340
      Width = 41
      Height = 18
      Caption = 'Delete'
      Enabled = False
      OnClick = Delete_InputClick
    end
    object Add_Input: TSpeedButton
      Left = 137
      Top = 340
      Width = 40
      Height = 18
      Caption = 'Add...'
      OnClick = Add_InputClick
    end
    object Delete_Output: TSpeedButton
      Left = 228
      Top = 340
      Width = 41
      Height = 18
      Caption = 'Delete'
      Enabled = False
      OnClick = Delete_OutputClick
    end
    object Add_Output: TSpeedButton
      Left = 357
      Top = 340
      Width = 40
      Height = 18
      Caption = 'Add...'
      OnClick = Add_OutputClick
    end
    object Input_LB: TListBox
      Left = 7
      Top = 20
      Width = 206
      Height = 308
      ItemHeight = 13
      TabOrder = 0
      OnClick = Input_LBClick
      OnKeyUp = Input_LBKeyUp
    end
    object Output_LB: TListBox
      Left = 219
      Top = 20
      Width = 207
      Height = 308
      ItemHeight = 13
      TabOrder = 1
      OnClick = Output_LBClick
      OnKeyUp = Output_LBKeyUp
    end
  end
end
