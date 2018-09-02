inherited FrameDownloaderOptionsPage_Nova: TFrameDownloaderOptionsPage_Nova
  Height = 132
  inherited PanelSpecificOptions: TPanel
    Height = 75
    object LabelSecretPassword: TLabel
      Left = 8
      Top = 18
      Width = 82
      Height = 13
      Caption = 'Secret &password:'
      FocusControl = EditSecretPassword
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
  end
end
