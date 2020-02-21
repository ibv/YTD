inherited FrameDownloaderOptionsPage_DASH: TFrameDownloaderOptionsPage_DASH
  Height = 220
  Width = 384
  inherited PanelCommonOptions: TPanel
    Width = 384
    inherited LabelSecureToken: TLabel
      Height = 15
      Width = 84
    end
    inherited LabelUserName: TLabel
      Height = 15
      Width = 69
    end
    inherited LabelPassword: TLabel
      Height = 15
      Width = 59
    end
    inherited CheckDownloadSubtitles: TCheckBox
      Height = 23
      Width = 369
    end
    inherited CheckConvertSubtitles: TCheckBox
      Height = 23
      Width = 369
    end
    inherited CheckLiveStream: TCheckBox
      Height = 23
      Width = 329
    end
    inherited EditSecureToken: TEdit
      Height = 26
      Width = 257
    end
    inherited EditUserName: TEdit
      Height = 26
      Width = 257
    end
    inherited EditPassword: TEdit
      Height = 26
      Width = 257
    end
    inherited CheckRealtime: TCheckBox
      Height = 23
      Width = 354
    end
  end
  inherited PanelSpecificOptions: TPanel
    Height = 59
    Width = 384
    object CheckDashVideo: TCheckBox[0]
      Left = 9
      Height = 23
      Top = 3
      Width = 225
      Caption = 'Enable MPEG-DASH video stream'
      TabOrder = 0
    end
    object CheckDashAudio: TCheckBox[1]
      Left = 9
      Height = 23
      Top = 24
      Width = 226
      Caption = 'Enable MPEG-DASH audio stream'
      TabOrder = 1
    end
  end
end
