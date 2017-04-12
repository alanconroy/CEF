object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'CEF Font Editor'
  ClientHeight = 579
  ClientWidth = 809
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 809
    Height = 579
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    ExplicitHeight = 559
  end
  object OpenDialog1: TOpenDialog
    Left = 138
    Top = 137
  end
  object MainMenu1: TMainMenu
    Left = 72
    Top = 37
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New...'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = '&Open...'
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = '&Close'
        ShortCut = 16499
        OnClick = Close1Click
      end
      object Closeall1: TMenuItem
        Caption = 'Close all'
        OnClick = Closeall1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Caption = '&Save...'
        Enabled = False
        ShortCut = 16467
        OnClick = Save1Click
      end
      object Saveas1: TMenuItem
        Caption = 'Save &as...'
        Enabled = False
        OnClick = Saveas1Click
      end
      object Saveall1: TMenuItem
        Caption = 'Save all'
        OnClick = Saveall1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'c'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save As'
    Left = 219
    Top = 139
  end
end
