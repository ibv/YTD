inherited FrameDownloaderOptionsPageCommon: TFrameDownloaderOptionsPageCommon
  object PanelCommonOptions: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
  end
  object PanelSpecificOptions: TPanel
    Left = 0
    Top = 57
    Width = 320
    Height = 183
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
