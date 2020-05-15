inherited FrameDownloaderOptionsPage_CT: TFrameDownloaderOptionsPage_CT
  Width = 384
  ClientWidth = 384
  DesignLeft = 462
  DesignTop = 310
  inherited PanelCommonOptions: TPanel
    Width = 384
    ClientWidth = 384
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
    Width = 384
    ClientWidth = 384
    object LabelMaximumVideoBitrate: TLabel[0]
      Left = 8
      Height = 15
      Top = 19
      Width = 162
      Caption = 'Maximum video &resolution:'
      FocusControl = EditMaximumVideoBitrate
      ParentColor = False
    end
    object EditMaximumVideoBitrate: TEdit[1]
      Left = 185
      Height = 26
      Top = 19
      Width = 79
      TabOrder = 0
      Text = '0'
    end
    object Label1: TLabel[2]
      Left = 9
      Height = 15
      Top = 35
      Width = 170
      Caption = '(512,720,1024,1280,1920)'
      ParentColor = False
    end
    object CheckDash: TCheckBox[3]
      Left = 9
      Height = 23
      Top = -5
      Width = 143
      Caption = 'Enable MPEG-DASH'
      TabOrder = 1
    end
  end
end
