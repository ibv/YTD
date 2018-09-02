inherited FrameDownloaderOptionsPage_CT: TFrameDownloaderOptionsPage_CT
  Height = 132
  inherited PanelSpecificOptions: TPanel
    Height = 75
    object CheckLiveStream: TCheckBox
      Left = 8
      Top = 0
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Live stream mode (much slower, but more stable)'
      TabOrder = 0
    end
  end
end
