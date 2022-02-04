inherited FrameDownloaderOptionsPage_Prima: TFrameDownloaderOptionsPage_Prima
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
      Width = 98
      Caption = '&User name/mail:'
    end
    inherited LabelPassword: TLabel
      Height = 15
      Width = 60
    end
    inherited CheckDownloadSubtitles: TCheckBox
      Height = 21
      Width = 369
    end
    inherited CheckConvertSubtitles: TCheckBox
      Height = 21
      Width = 369
    end
    inherited CheckLiveStream: TCheckBox
      Height = 21
      Width = 327
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
      EchoMode = emPassword
      PasswordChar = #1
    end
    inherited CheckRealtime: TCheckBox
      Height = 21
      Width = 353
    end
  end
  inherited PanelSpecificOptions: TPanel
    Width = 384
    ClientWidth = 384
    object LabelMaximumVideoBitrate: TLabel[0]
      Left = 8
      Height = 15
      Top = 5
      Width = 162
      Caption = 'Maximum video &resolution:'
      FocusControl = EditMaximumVideoBitrate
    end
    object EditMaximumVideoBitrate: TEdit[1]
      Left = 176
      Height = 26
      Top = 3
      Width = 79
      TabOrder = 0
      Text = '0'
    end
    object Label1: TLabel[2]
      Left = 9
      Height = 15
      Top = 28
      Width = 198
      Caption = '(512,640,768,1024,1280,1920)'
    end
  end
end
