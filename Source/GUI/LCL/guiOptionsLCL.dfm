object FormOptions: TFormOptions
  Left = 284
  Height = 396
  Top = 158
  Width = 551
  Caption = 'Options'
  ClientHeight = 396
  ClientWidth = 551
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poOwnerFormCenter
  LCLVersion = '2.0.0.4'
  object BtnOK: TButton
    Left = 391
    Height = 25
    Top = 365
    Width = 75
    Action = actOK
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 471
    Height = 25
    Top = 365
    Width = 75
    Action = actCancel
    Anchors = [akRight, akBottom]
    Cancel = True
    ModalResult = 2
    TabOrder = 1
  end
  object PageOptions: TPageControl
    Left = 0
    Height = 320
    Top = 0
    Width = 543
    ActivePage = TabMain
    Anchors = [akTop, akLeft, akRight, akBottom]
    TabIndex = 0
    TabOrder = 2
    object TabMain: TTabSheet
      Caption = 'Main settings'
      ClientHeight = 285
      ClientWidth = 535
      object LabelLanguage: TLabel
        Left = 8
        Height = 13
        Top = 180
        Width = 59
        Caption = '&Language:'
        FocusControl = EditLanguage
        ParentColor = False
      end
      object CheckPortableMode: TCheckBox
        Left = 7
        Height = 26
        Top = 8
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = '&Portable mode'
        TabOrder = 0
      end
      object CheckCheckNewVersions: TCheckBox
        Left = 7
        Height = 26
        Top = 24
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = '&Check for new versions on startup'
        TabOrder = 1
      end
      object EditLanguage: TEdit
        Left = 112
        Height = 19
        Top = 176
        Width = 177
        TabOrder = 4
      end
      object BtnDesktopShortcut: TButton
        Left = 8
        Height = 25
        Top = 225
        Width = 512
        Action = actDesktopShortcut
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 5
      end
      object BtnStartMenuShortcut: TButton
        Left = 8
        Height = 25
        Top = 257
        Width = 512
        Action = actStartMenuShortcut
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 6
      end
      object CheckMonitorClipboard: TCheckBox
        Left = 7
        Height = 26
        Top = 40
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Mo&nitor clipboard for downloadable URLs'
        TabOrder = 2
      end
      object CheckIgnoreOpenSSLWarning: TCheckBox
        Left = 7
        Height = 26
        Top = 56
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Disable &OpenSSL warning'
        TabOrder = 3
      end
      object CheckIgnoreRtmpDumpWarning: TCheckBox
        Left = 7
        Height = 26
        Top = 72
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Disable &RtmpDump warning'
        TabOrder = 7
      end
      object CheckIgnoreMSDLWarning: TCheckBox
        Left = 7
        Height = 26
        Top = 88
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Disable &MSDL warning'
        TabOrder = 8
      end
      object CheckMinimizeToTray: TCheckBox
        Left = 7
        Height = 26
        Top = 104
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Minimize to system &tray'
        TabOrder = 9
      end
    end
    object TabDownloadOptions: TTabSheet
      Caption = 'Download settings'
      ClientHeight = 285
      ClientWidth = 535
      ImageIndex = 1
      object LabelOverwriteMode: TLabel
        Left = 8
        Height = 13
        Top = 193
        Width = 76
        Caption = '&Existing files:'
        FocusControl = ComboOverwriteMode
        ParentColor = False
      end
      object LabelDownloadDir: TLabel
        Left = 8
        Height = 13
        Top = 140
        Width = 112
        Caption = '&Download directory:'
        FocusControl = EditDownloadDir
        ParentColor = False
      end
      object LabelConverter: TLabel
        Left = 8
        Height = 13
        Top = 246
        Width = 60
        Caption = '&Converter:'
        FocusControl = ComboConverter
        ParentColor = False
      end
      object LabelRetryCount: TLabel
        Left = 8
        Height = 13
        Top = 221
        Width = 68
        Caption = '&Retry count:'
        FocusControl = EditRetryCount
        ParentColor = False
      end
      object Label1: TLabel
        Left = 8
        Height = 13
        Top = 165
        Width = 175
        Caption = 'Append &index to playlist items:'
        FocusControl = ComboAddIndexToNames
        ParentColor = False
      end
      object CheckAutoDownload: TCheckBox
        Left = 8
        Height = 26
        Top = 8
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = '&Automatically start downloads'
        TabOrder = 0
      end
      object ComboOverwriteMode: TComboBox
        Left = 176
        Height = 25
        Top = 189
        Width = 352
        Anchors = [akTop, akLeft, akRight]
        ItemHeight = 0
        Style = csDropDownList
        TabOrder = 9
      end
      object EditDownloadDir: TEdit
        Left = 176
        Height = 19
        Top = 136
        Width = 336
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 6
      end
      object BtnDownloadDir: TButton
        Left = 511
        Height = 21
        Top = 136
        Width = 17
        Action = actDownloadDir
        Anchors = [akTop, akRight]
        TabOrder = 7
      end
      object ComboConverter: TComboBox
        Left = 176
        Height = 25
        Top = 242
        Width = 352
        Anchors = [akTop, akLeft, akRight]
        ItemHeight = 0
        OnChange = ComboConverterChange
        Style = csDropDownList
        TabOrder = 11
      end
      object CheckSubtitlesEnabled: TCheckBox
        Left = 8
        Height = 26
        Top = 56
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Download &subtitles if available'
        TabOrder = 3
      end
      object CheckAutoTryHtmlParser: TCheckBox
        Left = 8
        Height = 26
        Top = 40
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Automatically try &HTML parser'
        TabOrder = 2
      end
      object CheckDownloadToTempFiles: TCheckBox
        Left = 8
        Height = 26
        Top = 72
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Append a .part e&xtension while downloading'
        TabOrder = 4
      end
      object CheckDownloadToProviderSubdirs: TCheckBox
        Left = 8
        Height = 26
        Top = 88
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Store media into su&bdirectories according to their providers'
        TabOrder = 5
      end
      object EditRetryCount: TEdit
        Left = 176
        Height = 19
        Top = 218
        Width = 49
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 10
      end
      object ComboAddIndexToNames: TComboBox
        Left = 176
        Height = 25
        Top = 160
        Width = 352
        Anchors = [akTop, akLeft, akRight]
        ItemHeight = 0
        Style = csDropDownList
        TabOrder = 8
      end
      object CheckAutoDeleteFinishedDownloads: TCheckBox
        Left = 8
        Height = 26
        Top = 24
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Automatically delete &finished downloads'
        TabOrder = 1
      end
    end
    object TabNetworkOptions: TTabSheet
      Caption = 'Network settings'
      ClientHeight = 285
      ClientWidth = 535
      ImageIndex = 2
      object LabelProxyHost: TLabel
        Left = 8
        Height = 13
        Top = 40
        Width = 64
        Caption = 'Proxy &host:'
        FocusControl = EditProxyHost
        ParentColor = False
      end
      object LabelProxyPort: TLabel
        Left = 8
        Height = 13
        Top = 64
        Width = 62
        Caption = 'Proxy &port:'
        FocusControl = EditProxyPort
        ParentColor = False
      end
      object LabelProxyUser: TLabel
        Left = 8
        Height = 13
        Top = 88
        Width = 97
        Caption = 'Proxy &username:'
        FocusControl = EditProxyUser
        ParentColor = False
      end
      object LabelProxyPass: TLabel
        Left = 8
        Height = 13
        Top = 112
        Width = 95
        Caption = 'Proxy pass&word:'
        FocusControl = EditProxyPass
        ParentColor = False
      end
      object CheckUseProxy: TCheckBox
        Left = 8
        Height = 26
        Top = 8
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'U&se proxy server'
        TabOrder = 0
      end
      object EditProxyHost: TEdit
        Left = 112
        Height = 19
        Top = 36
        Width = 177
        TabOrder = 1
      end
      object EditProxyPort: TEdit
        Left = 112
        Height = 19
        Top = 60
        Width = 177
        TabOrder = 2
      end
      object EditProxyUser: TEdit
        Left = 112
        Height = 19
        Top = 84
        Width = 177
        TabOrder = 3
      end
      object EditProxyPass: TEdit
        Left = 112
        Height = 19
        Top = 108
        Width = 177
        TabOrder = 4
      end
    end
    object TabDownloaderOptions: TTabSheet
      Caption = 'Downloader settings'
      ClientHeight = 285
      ClientWidth = 535
      ImageIndex = 3
      object ListDownloaderOptions: TListBox
        Left = 0
        Height = 285
        Top = 0
        Width = 169
        Align = alLeft
        ItemHeight = 0
        OnClick = ListDownloaderOptionsClick
        ScrollWidth = 165
        Sorted = True
        TabOrder = 0
        TopIndex = -1
      end
      object PanelDownloaderOptions: TPanel
        Left = 169
        Height = 285
        Top = 0
        Width = 366
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
  object ActionList: TActionList
    left = 416
    top = 40
    object actOK: TAction
      Caption = '&OK'
      OnExecute = actOKExecute
      ShortCut = 16397
    end
    object actCancel: TAction
      Caption = '&Cancel'
      DisableIfNoHandler = False
      OnExecute = actCancelExecute
      ShortCut = 27
    end
    object actDownloadDir: TAction
      Caption = '...'
      OnExecute = actDownloadDirExecute
      ShortCut = 32836
    end
    object actDesktopShortcut: TAction
      Caption = 'Create shortcut on &desktop'
      OnExecute = actDesktopShortcutExecute
    end
    object actStartMenuShortcut: TAction
      Caption = 'Create shortcut in &Start menu'
      OnExecute = actStartMenuShortcutExecute
    end
  end
end
