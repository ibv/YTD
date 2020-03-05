inherited FrameDownloaderOptionsPage_YouTube: TFrameDownloaderOptionsPage_YouTube
  Height = 292
  ClientHeight = 292
  DesignLeft = 590
  DesignTop = 306
  inherited PanelCommonOptions: TPanel
    Height = 152
    ClientHeight = 152
    inherited LabelSecureToken: TLabel
      Height = 15
      Top = 71
      Width = 84
    end
    inherited LabelUserName: TLabel
      Height = 15
      Top = 97
      Width = 69
    end
    inherited LabelPassword: TLabel
      Height = 15
      Top = 121
      Width = 59
    end
    inherited CheckDownloadSubtitles: TCheckBox
      Height = 23
    end
    inherited CheckConvertSubtitles: TCheckBox
      Height = 23
    end
    inherited CheckLiveStream: TCheckBox
      Height = 23
      Width = 329
    end
    inherited EditSecureToken: TEdit
      Height = 26
      Top = 69
    end
    inherited EditUserName: TEdit
      Height = 26
      Top = 95
    end
    inherited EditPassword: TEdit
      Height = 26
      Top = 119
    end
    inherited CheckRealtime: TCheckBox
      Height = 23
      Width = 354
    end
  end
  inherited PanelSpecificOptions: TPanel
    Height = 140
    Top = 152
    ClientHeight = 140
    object LabelPreferredLanguages: TLabel[0]
      Left = 8
      Height = 15
      Top = -3
      Width = 174
      Caption = '&Preferred subtitle languages:'
      FocusControl = EditPreferredLanguages
      ParentColor = False
    end
    object LabelMaximumVideoWidth: TLabel[1]
      Left = 8
      Height = 15
      Top = 21
      Width = 137
      Caption = 'Maximum video &width:'
      FocusControl = EditMaximumVideoWidth
      ParentColor = False
    end
    object LabelMaximumVideoHeight: TLabel[2]
      Left = 8
      Height = 15
      Top = 45
      Width = 142
      Caption = 'Maximum video &height:'
      FocusControl = EditMaximumVideoHeight
      ParentColor = False
    end
    object EditPreferredLanguages: TEdit[3]
      Left = 184
      Height = 26
      Top = -3
      Width = 121
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 2
    end
    object EditMaximumVideoWidth: TEdit[4]
      Left = 152
      Height = 26
      Top = 21
      Width = 57
      TabOrder = 0
      Text = '0'
    end
    object EditMaximumVideoHeight: TEdit[5]
      Left = 152
      Height = 26
      Top = 48
      Width = 57
      TabOrder = 1
      Text = '0'
    end
    object CheckAvoidWebM: TCheckBox[6]
      Left = 8
      Height = 23
      Top = 69
      Width = 305
      Anchors = [akTop, akLeft, akRight]
      Caption = '&Avoid .webm format'
      TabOrder = 3
    end
    object CheckDashVideo: TCheckBox[7]
      Left = 8
      Height = 23
      Top = 93
      Width = 211
      Caption = 'Enable download video stream'
      TabOrder = 4
    end
    object CheckDashAudio: TCheckBox[8]
      Left = 8
      Height = 23
      Top = 114
      Width = 212
      Caption = 'Enable download audio stream'
      TabOrder = 5
    end
  end
end
