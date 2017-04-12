object Choose_Emulator_Port_Form: TChoose_Emulator_Port_Form
  Left = 187
  Top = 128
  Caption = 'Choose Emulator Port'
  ClientHeight = 668
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 150
    Top = 0
    Height = 635
    Beveled = True
    ExplicitHeight = 639
  end
  object Panel1: TPanel
    Left = 0
    Top = 635
    Width = 600
    Height = 33
    Align = alBottom
    TabOrder = 0
    object OK_Button: TBitBtn
      Left = 63
      Top = 7
      Width = 61
      Height = 20
      TabOrder = 0
      OnClick = OK_ButtonClick
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 360
      Top = 7
      Width = 61
      Height = 21
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 150
    Height = 635
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 64
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 'Component'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ListBox1: TListBox
      Left = 0
      Top = 13
      Width = 150
      Height = 622
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
      OnDblClick = ListBox1DblClick
    end
  end
  object Panel3: TPanel
    Left = 153
    Top = 0
    Width = 431
    Height = 635
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 163
      Top = 0
      Width = 2
      Height = 635
      Beveled = True
      ExplicitHeight = 639
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 163
      Height = 635
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 24
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Port'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ListBox2: TListBox
        Left = 0
        Top = 13
        Width = 163
        Height = 622
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBox1Click
        OnDblClick = ListBox1DblClick
      end
    end
    object Panel5: TPanel
      Left = 165
      Top = 0
      Width = 266
      Height = 635
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Label3: TLabel
        Left = 0
        Top = 0
        Width = 77
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Connected to'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ListBox3: TListBox
        Left = 0
        Top = 13
        Width = 266
        Height = 622
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBox1Click
        OnDblClick = ListBox1DblClick
      end
    end
  end
  object ScrollBar1: TScrollBar
    Left = 584
    Top = 0
    Width = 16
    Height = 635
    Align = alRight
    Kind = sbVertical
    PageSize = 0
    TabOrder = 3
  end
end
