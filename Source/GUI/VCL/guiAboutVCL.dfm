object FormAbout: TFormAbout
  Left = 208
  Top = 104
  Caption = 'About YTD'
  ClientHeight = 261
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  DesignSize = (
    414
    261)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelYTD: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 24
    Caption = 'YTD'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object LabelVersionLabel: TLabel
    Left = 8
    Top = 40
    Width = 38
    Height = 13
    Caption = 'Version:'
  end
  object LabelVersion: TLabel
    Left = 112
    Top = 40
    Width = 74
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'LabelVersion'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelNewestVersionLabel: TLabel
    Left = 8
    Top = 56
    Width = 76
    Height = 13
    Caption = 'Newest version:'
  end
  object LabelNewestVersion: TLabel
    Left = 112
    Top = 56
    Width = 55
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'not found'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = LabelNewestVersionClick
  end
  object LabelHomepageLabel: TLabel
    Left = 8
    Top = 72
    Width = 55
    Height = 13
    Caption = 'Homepage:'
  end
  object LabelHomepage: TLabel
    Left = 112
    Top = 72
    Width = 146
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'https://ibv.github.io/YTD'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = LabelHomepageClick
  end
  object LabelMediaProviders: TLabel
    Left = 8
    Top = 96
    Width = 78
    Height = 13
    Caption = 'Media providers:'
  end
  object LabelDefsVersion: TLabel
    Left = 192
    Top = 40
    Width = 100
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'LabelDefsVersion'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelNewestDefsVersion: TLabel
    Left = 192
    Top = 56
    Width = 55
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'not found'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = LabelNewestDefsVersionClick
  end
  object ListProviders: TListView
    Left = 8
    Top = 112
    Width = 409
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Provider'
        Width = 100
      end
      item
        Caption = 'Components'
        Width = 264
      end>
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = ListProvidersData
  end
end
