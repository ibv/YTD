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
      Width = 89
    end
    inherited LabelUserName: TLabel
      Height = 15
      Width = 75
    end
    inherited LabelPassword: TLabel
      Height = 15
      Width = 63
    end
    inherited CheckDownloadSubtitles: TCheckBox
      Height = 26
      Width = 369
    end
    inherited CheckConvertSubtitles: TCheckBox
      Height = 26
      Width = 369
    end
    inherited CheckLiveStream: TCheckBox
      Height = 26
      Width = 348
    end
    inherited EditSecureToken: TEdit
      Width = 257
    end
    inherited EditUserName: TEdit
      Width = 257
    end
    inherited EditPassword: TEdit
      Width = 257
    end
    inherited CheckRealtime: TCheckBox
      Height = 26
      Width = 373
    end
  end
  inherited PanelSpecificOptions: TPanel
    Width = 384
    ClientWidth = 384
    object LabelMaximumVideoBitrate: TLabel[0]
      Left = 8
      Height = 15
      Top = 0
      Width = 172
      Caption = 'Maximum video &resolution:'
      FocusControl = EditMaximumVideoBitrate
      ParentColor = False
    end
    object EditMaximumVideoBitrate: TEdit[1]
      Left = 185
      Height = 21
      Top = 0
      Width = 79
      TabOrder = 0
      Text = '0'
    end
    object Label1: TLabel[2]
      Left = 9
      Height = 15
      Top = 20
      Width = 198
      Caption = '(512,640,768,1024,1280,1920)'
      ParentColor = False
    end
  end
end
