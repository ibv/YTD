inherited FrameDownloaderOptionsPage_YouTube: TFrameDownloaderOptionsPage_YouTube
  Height = 156
  inherited PanelSpecificOptions: TPanel
    Height = 99
    object LabelPreferredLanguages: TLabel
      Left = 8
      Top = 0
      Width = 134
      Height = 13
      Caption = '&Preferred subtitle languages:'
      FocusControl = EditPreferredLanguages
    end
    object LabelMaximumVideoWidth: TLabel
      Left = 8
      Top = 24
      Width = 104
      Height = 13
      Caption = 'Maximum video &width:'
      FocusControl = EditMaximumVideoWidth
    end
    object LabelMaximumVideoHeight: TLabel
      Left = 8
      Top = 48
      Width = 108
      Height = 13
      Caption = 'Maximum video &height:'
      FocusControl = EditMaximumVideoHeight
    end
    object EditPreferredLanguages: TEdit
      Left = 152
      Top = 0
      Width = 153
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object EditMaximumVideoWidth: TEdit
      Left = 152
      Top = 24
      Width = 33
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object EditMaximumVideoHeight: TEdit
      Left = 152
      Top = 48
      Width = 33
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object CheckAvoidWebM: TCheckBox
      Left = 8
      Top = 72
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Avoid .webm format'
      TabOrder = 3
    end
  end
end
