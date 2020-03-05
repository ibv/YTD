inherited FrameDownloaderOptionsPageCommon: TFrameDownloaderOptionsPageCommon
  Height = 252
  ClientHeight = 252
  DesignLeft = 903
  DesignTop = 518
  object PanelCommonOptions: TPanel[0]
    Left = 0
    Height = 153
    Top = 0
    Width = 320
    Align = alTop
    BevelOuter = bvNone
    ClientHeight = 153
    ClientWidth = 320
    TabOrder = 0
    object LabelSecureToken: TLabel
      Left = 8
      Height = 15
      Top = 74
      Width = 72
      Caption = 'Secure &Token:'
      FocusControl = EditSecureToken
      ParentColor = False
    end
    object LabelUserName: TLabel
      Left = 8
      Height = 15
      Top = 98
      Width = 59
      Caption = '&User name:'
      FocusControl = EditUserName
      ParentColor = False
    end
    object LabelPassword: TLabel
      Left = 8
      Height = 15
      Top = 122
      Width = 53
      Caption = '&Password:'
      FocusControl = EditPassword
      ParentColor = False
    end
    object CheckDownloadSubtitles: TCheckBox
      Left = 8
      Height = 19
      Top = 0
      Width = 305
      Anchors = [akTop, akLeft, akRight]
      Caption = 'Download &subtitles if available'
      TabOrder = 0
    end
    object CheckConvertSubtitles: TCheckBox
      Left = 8
      Height = 19
      Top = 16
      Width = 305
      Anchors = [akTop, akLeft, akRight]
      Caption = '&Convert subtitles to .SRT format'
      TabOrder = 1
    end
    object CheckLiveStream: TCheckBox
      Left = 8
      Height = 19
      Top = 32
      Width = 282
      Caption = '&Live stream mode (much slower, but more stable)'
      TabOrder = 2
    end
    object EditSecureToken: TEdit
      Left = 112
      Height = 23
      Top = 72
      Width = 193
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 3
    end
    object EditUserName: TEdit
      Left = 112
      Height = 23
      Top = 96
      Width = 193
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 4
    end
    object EditPassword: TEdit
      Left = 112
      Height = 23
      Top = 120
      Width = 193
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 5
    end
    object CheckRealtime: TCheckBox
      Left = 8
      Height = 19
      Top = 48
      Width = 305
      Caption = '&Realtime mode (may be slower, but doesn''t skip back)'
      TabOrder = 6
    end
  end
  object PanelSpecificOptions: TPanel[1]
    Left = 0
    Height = 99
    Top = 153
    Width = 320
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
