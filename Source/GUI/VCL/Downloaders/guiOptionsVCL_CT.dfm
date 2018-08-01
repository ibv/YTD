inherited FrameDownloaderOptionsPage_CT: TFrameDownloaderOptionsPage_CT
  inherited PanelSpecificOptions: TPanel
    object LabelMaximumVideoBitrate: TLabel
      Left = 8
      Top = 0
      Width = 108
      Height = 13
      Caption = 'Maximum video &resolution:'
      FocusControl = EditMaximumVideoBitrate
    end
    object EditMaximumVideoBitrate: TEdit
      Left = 152
      Top = 0
      Width = 80
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object Label1: TLabel[2]
      Left = 9
      Height = 15
      Top = 16
      Width = 170
      Caption = '(512,720,1024,1280,1920)'
      ParentColor = False
    end

  end
end
