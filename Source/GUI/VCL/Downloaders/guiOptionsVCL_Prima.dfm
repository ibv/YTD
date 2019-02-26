inherited FrameDownloaderOptionsPage_Prima: TFrameDownloaderOptionsPage_Prima
  inherited PanelSpecificOptions: TPanel
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
      Top = 16
      Width = 198
      Caption = '(512,640,768,1024,1280,1920)'
      ParentColor = False
    end
  end
end
