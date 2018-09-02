inherited FrameDownloaderOptionsPageCommon: TFrameDownloaderOptionsPageCommon
  object PanelCommonOptions: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 137
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelSecureToken: TLabel
      Left = 8
      Top = 50
      Width = 71
      Height = 13
      Caption = 'Secure &Token:'
      FocusControl = EditSecureToken
    end
    object LabelUserName: TLabel
      Left = 8
      Top = 82
      Width = 54
      Height = 13
      Caption = '&User name:'
      FocusControl = EditUserName
    end
    object LabelPassword: TLabel
      Left = 8
      Top = 106
      Width = 49
      Height = 13
      Caption = '&Password:'
      FocusControl = EditPassword
    end
    object CheckDownloadSubtitles: TCheckBox
      Left = 8
      Top = 0
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Download &subtitles if available'
      TabOrder = 0
    end
    object CheckConvertSubtitles: TCheckBox
      Left = 8
      Top = 16
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Convert subtitles to .SRT format'
      TabOrder = 1
    end
    object CheckLiveStream: TCheckBox
      Left = 8
      Top = 32
      Width = 305
      Height = 17
      Caption = '&Live stream mode (much slower, but more stable)'
      TabOrder = 2
    end
    object EditSecureToken: TEdit
      Left = 112
      Top = 48
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object EditUserName: TEdit
      Left = 112
      Top = 80
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object EditPassword: TEdit
      Left = 112
      Top = 104
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
    end
  end
  object PanelSpecificOptions: TPanel
    Left = 0
    Top = 137
    Width = 320
    Height = 103
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
