inherited FrameDownloaderOptionsPage_CT: TFrameDownloaderOptionsPage_CT
  Width = 384
  ClientWidth = 384
  DesignLeft = 462
  DesignTop = 310
  inherited PanelCommonOptions: TPanel
    Width = 384
    ClientWidth = 384
    inherited CheckDownloadSubtitles: TCheckBox
      Width = 369
    end
    inherited CheckConvertSubtitles: TCheckBox
      Width = 369
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
  end
  inherited PanelSpecificOptions: TPanel
    Width = 384
    ClientWidth = 384
    object LabelMaximumVideoBitrate: TLabel[0]
      Left = 8
      Height = 15
      Top = 19
      Width = 146
      Caption = 'Maximum video &resolution:'
      FocusControl = EditMaximumVideoBitrate
      ParentColor = False
    end
    object EditMaximumVideoBitrate: TEdit[1]
      Left = 185
      Height = 23
      Top = 19
      Width = 79
      TabOrder = 0
      Text = '0'
    end
    object Label1: TLabel[2]
      Left = 9
      Height = 15
      Top = 35
      Width = 128
      Caption = '(512,720,1024,1280,1920)'
      ParentColor = False
    end
    object CheckDash: TCheckBox[3]
      Left = 8
      Height = 19
      Top = 0
      Width = 133
      Caption = 'Support MPEG-DASH'
      TabOrder = 1
    end
  end
end
