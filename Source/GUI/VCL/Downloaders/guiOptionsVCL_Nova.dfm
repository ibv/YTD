inherited FrameDownloaderOptionsPage_Nova: TFrameDownloaderOptionsPage_Nova
  Height = 231
  inherited PanelCommonOptions: TPanel
    inherited CheckDownloadSubtitles: TCheckBox
      Width = 321
    end
    inherited CheckConvertSubtitles: TCheckBox
      Width = 321
    end
    inherited EditSecureToken: TEdit
      Width = 209
    end
    inherited EditUserName: TEdit
      Width = 209
    end
    inherited EditPassword: TEdit
      Width = 209
    end
  end
  inherited PanelSpecificOptions: TPanel
    Height = 70
    object LabelSecretPassword: TLabel
      Left = 8
      Top = 18
      Width = 93
      Height = 13
      Caption = 'Resolver password:'
      FocusControl = EditSecretPassword
    end
    object LabelConfigPassword: TLabel
      Left = 8
      Top = 42
      Width = 81
      Height = 13
      Caption = 'Config password:'
      FocusControl = EditConfigPassword
    end
    object CheckLowQuality: TCheckBox
      Left = 8
      Top = 0
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Download in &low quality'
      TabOrder = 0
    end
    object EditSecretPassword: TEdit
      Left = 112
      Top = 16
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object EditConfigPassword: TEdit
      Left = 112
      Top = 40
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
  end
end
