inherited FrameDownloaderOptionsPage_Barrandov: TFrameDownloaderOptionsPage_Barrandov
  Height = 132
  inherited PanelSpecificOptions: TPanel
    Height = 75
    object LabelSecureToken: TLabel
      Left = 8
      Top = 2
      Width = 71
      Height = 13
      Caption = 'Secure &Token:'
      FocusControl = EditSecureToken
    end
    object EditSecureToken: TEdit
      Left = 112
      Top = 0
      Width = 193
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
end
