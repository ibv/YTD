object FormAbout: TFormAbout
  Left = 208
  Height = 300
  Top = 104
  Width = 430
  Caption = 'About YTD'
  ClientHeight = 300
  ClientWidth = 430
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Position = poOwnerFormCenter
  LCLVersion = '1.8.4.0'
  object LabelYTD: TLabel
    Left = 8
    Height = 22
    Top = 8
    Width = 43
    Caption = 'YTD'
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentColor = False
    ParentFont = False
  end
  object LabelVersionLabel: TLabel
    Left = 8
    Height = 13
    Top = 40
    Width = 46
    Caption = 'Version:'
    ParentColor = False
  end
  object LabelVersion: TLabel
    Left = 127
    Height = 13
    Top = 40
    Width = 82
    Caption = 'LabelVersion'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelNewestVersionLabel: TLabel
    Left = 8
    Height = 13
    Top = 56
    Width = 92
    Caption = 'Newest version:'
    ParentColor = False
  end
  object LabelNewestVersion: TLabel
    Left = 127
    Height = 13
    Top = 56
    Width = 62
    Caption = 'not found'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    OnClick = LabelNewestVersionClick
  end
  object LabelHomepageLabel: TLabel
    Left = 8
    Height = 13
    Top = 72
    Width = 65
    Caption = 'Homepage:'
    ParentColor = False
  end
  object LabelHomepage: TLabel
    Left = 127
    Height = 13
    Top = 72
    Width = 157
    Caption = 'https://ibv.github.io/YTD/'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    OnClick = LabelHomepageClick
  end
  object LabelMediaProviders: TLabel
    Left = 8
    Height = 13
    Top = 96
    Width = 95
    Caption = 'Media providers:'
    ParentColor = False
  end
  object LabelDefsVersion: TLabel
    Left = 210
    Height = 13
    Top = 40
    Width = 111
    Caption = 'LabelDefsVersion'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelNewestDefsVersion: TLabel
    Left = 210
    Height = 13
    Top = 56
    Width = 62
    Caption = 'not found'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    OnClick = LabelNewestDefsVersionClick
  end
  object ListProviders: TListView
    Left = 8
    Height = 153
    Top = 112
    Width = 409
    Anchors = [akTop, akLeft, akRight, akBottom]
    Columns = <    
      item
        Caption = 'Provider'
        Width = 100
      end    
      item
        Caption = 'Components'
        Width = 288
      end>
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ScrollBars = ssVertical
    TabOrder = 0
    ViewStyle = vsReport
    OnData = ListProvidersData
  end
end
