inherited FrameDownloaderOptionsPage_CT: TFrameDownloaderOptionsPage_CT
  inherited PanelSpecificOptions: TPanel
    object LabelMaximumVideoBitrate: TLabel
      Left = 8
      Top = 22
      Width = 127
      Height = 13
      Caption = 'Maximum video &resolution:'
      Color = clBtnFace
      FocusControl = EditMaximumVideoBitrate
      ParentColor = False
    end
    object Label1: TLabel
      Left = 9
      Top = 38
      Width = 132
      Height = 13
      Caption = '(512,720,1024,1280,1920)'
      Color = clBtnFace
      ParentColor = False
    end
    object EditMaximumVideoBitrate: TEdit
      Left = 164
      Top = 22
      Width = 79
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object CheckDash: TCheckBox
      Left = 8
      Top = -5
      Width = 261
      Height = 26
      Caption = 'Support MPEG-DASH (experimental)'
      TabOrder = 1
    end
  end
end
