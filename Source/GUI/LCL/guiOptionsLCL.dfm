object FormOptions: TFormOptions
  Left = 273
  Height = 395
  Top = 214
  Width = 551
  Caption = 'Options'
  ClientHeight = 395
  ClientWidth = 551
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poOwnerFormCenter
  LCLVersion = '2.2.0.4'
  object BtnOK: TButton
    Left = 391
    Height = 25
    Top = 364
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
    Top = 364
    Width = 75
    Action = actCancel
    Anchors = [akRight, akBottom]
    Cancel = True
    ModalResult = 2
    TabOrder = 1
  end
  object PageOptions: TPageControl
    Left = 0
    Height = 343
    Top = 0
    Width = 543
    ActivePage = TabDownloadOptions
    Anchors = [akTop, akLeft, akRight, akBottom]
    TabIndex = 1
    TabOrder = 2
    object TabMain: TTabSheet
      Caption = 'Main settings'
      ClientHeight = 317
      ClientWidth = 535
      object LabelLanguage: TLabel
        Left = 8
        Height = 13
        Top = 180
        Width = 51
        Caption = '&Language:'
        FocusControl = EditLanguage
      end
      object CheckPortableMode: TCheckBox
        Left = 7
        Height = 19
        Top = 8
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = '&Portable mode'
        TabOrder = 0
      end
      object CheckCheckNewVersions: TCheckBox
        Left = 7
        Height = 19
        Top = 24
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = '&Check for new versions on startup'
        TabOrder = 1
      end
      object EditLanguage: TEdit
        Left = 112
        Height = 21
        Top = 176
        Width = 177
        TabOrder = 4
      end
      object BtnDesktopShortcut: TButton
        Left = 8
        Height = 25
        Top = 257
        Width = 512
        Action = actDesktopShortcut
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 5
      end
      object BtnStartMenuShortcut: TButton
        Left = 8
        Height = 25
        Top = 289
        Width = 512
        Action = actStartMenuShortcut
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 6
      end
      object CheckMonitorClipboard: TCheckBox
        Left = 7
        Height = 19
        Top = 40
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Mo&nitor clipboard for downloadable URLs'
        TabOrder = 2
      end
      object CheckIgnoreOpenSSLWarning: TCheckBox
        Left = 7
        Height = 19
        Top = 56
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Disable &OpenSSL warning'
        TabOrder = 3
      end
      object CheckIgnoreRtmpDumpWarning: TCheckBox
        Left = 7
        Height = 19
        Top = 72
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Disable &RtmpDump warning'
        TabOrder = 7
      end
      object CheckIgnoreMSDLWarning: TCheckBox
        Left = 7
        Height = 19
        Top = 88
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Disable &MSDL warning'
        TabOrder = 8
      end
      object CheckMinimizeToTray: TCheckBox
        Left = 7
        Height = 19
        Top = 104
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Minimize to system &tray'
        TabOrder = 9
      end
    end
    object TabDownloadOptions: TTabSheet
      Caption = 'Download settings'
      ClientHeight = 317
      ClientWidth = 535
      ImageIndex = 1
      object LabelOverwriteMode: TLabel
        Left = 8
        Height = 13
        Top = 246
        Width = 60
        Caption = '&Existing files:'
        FocusControl = ComboOverwriteMode
      end
      object LabelDownloadDir: TLabel
        Left = 8
        Height = 13
        Top = 201
        Width = 94
        Caption = '&Download directory:'
        FocusControl = EditDownloadDir
      end
      object LabelConverter: TLabel
        Left = 8
        Height = 13
        Top = 292
        Width = 49
        Caption = '&Converter:'
        FocusControl = ComboConverter
      end
      object LabelRetryCount: TLabel
        Left = 8
        Height = 13
        Top = 271
        Width = 58
        Caption = '&Retry count:'
        FocusControl = EditRetryCount
      end
      object Label1: TLabel
        Left = 8
        Height = 13
        Top = 221
        Width = 141
        Caption = 'Append &index to playlist items:'
        FocusControl = ComboAddIndexToNames
      end
      object CheckAutoDownload: TCheckBox
        Left = 8
        Height = 19
        Top = 8
        Width = 202
        Anchors = [akTop, akLeft, akRight]
        Caption = '&Automatically start downloads'
        TabOrder = 0
      end
      object ComboOverwriteMode: TComboBox
        Left = 176
        Height = 21
        Top = 240
        Width = 352
        Anchors = [akTop, akLeft, akRight]
        ItemHeight = 13
        Style = csDropDownList
        TabOrder = 9
      end
      object EditDownloadDir: TEdit
        Left = 176
        Height = 21
        Top = 192
        Width = 336
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 6
      end
      object BtnDownloadDir: TButton
        Left = 513
        Height = 21
        Top = 191
        Width = 17
        Action = actDownloadDir
        Anchors = [akTop, akRight]
        TabOrder = 7
      end
      object ComboConverter: TComboBox
        Left = 176
        Height = 21
        Top = 288
        Width = 352
        Anchors = [akTop, akLeft, akRight]
        ItemHeight = 13
        OnChange = ComboConverterChange
        Style = csDropDownList
        TabOrder = 11
      end
      object CheckSubtitlesEnabled: TCheckBox
        Left = 8
        Height = 19
        Top = 56
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Download &subtitles if available'
        TabOrder = 3
      end
      object CheckAutoTryHtmlParser: TCheckBox
        Left = 8
        Height = 19
        Top = 40
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Automatically try &HTML parser'
        TabOrder = 2
      end
      object CheckDownloadToTempFiles: TCheckBox
        Left = 8
        Height = 19
        Top = 72
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Append a .part e&xtension while downloading'
        TabOrder = 4
      end
      object CheckDownloadToProviderSubdirs: TCheckBox
        Left = 8
        Height = 19
        Top = 88
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Store media into su&bdirectories according to their providers'
        TabOrder = 5
      end
      object EditRetryCount: TEdit
        Left = 176
        Height = 21
        Top = 264
        Width = 49
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 10
      end
      object ComboAddIndexToNames: TComboBox
        Left = 176
        Height = 21
        Top = 216
        Width = 352
        Anchors = [akTop, akLeft, akRight]
        ItemHeight = 13
        Style = csDropDownList
        TabOrder = 8
      end
      object CheckAutoDeleteFinishedDownloads: TCheckBox
        Left = 8
        Height = 19
        Top = 24
        Width = 266
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Automatically delete &finished downloads'
        TabOrder = 1
      end
      object EditStartSoundFile: TEdit
        Left = 176
        Height = 21
        Top = 120
        Width = 336
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 12
      end
      object BtnStartSound: TButton
        Left = 513
        Height = 21
        Top = 121
        Width = 17
        Action = actStartPlaySound
        Anchors = [akTop, akRight]
        TabOrder = 13
      end
      object EditEndSoundFile: TEdit
        Left = 176
        Height = 21
        Top = 144
        Width = 336
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 14
      end
      object BtnEndSound: TButton
        Left = 513
        Height = 21
        Top = 143
        Width = 17
        Action = actEndPlaySound
        Anchors = [akTop, akRight]
        TabOrder = 15
      end
      object CheckStartSound: TCheckBox
        Left = 8
        Height = 19
        Top = 126
        Width = 168
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Start sound'
        OnChange = CheckStartSoundChange
        TabOrder = 16
      end
      object CheckEndSound: TCheckBox
        Left = 8
        Height = 19
        Top = 148
        Width = 168
        Anchors = [akTop, akLeft, akRight]
        Caption = 'End sound'
        OnChange = CheckEndSoundChange
        TabOrder = 17
      end
      object CheckFailSound: TCheckBox
        Left = 8
        Height = 19
        Top = 170
        Width = 168
        Anchors = [akTop, akLeft, akRight]
        Caption = 'Fail sound'
        OnChange = CheckFailSoundChange
        TabOrder = 18
      end
      object EditFailSoundFile: TEdit
        Left = 176
        Height = 21
        Top = 168
        Width = 336
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 19
      end
      object BtnFailSound: TButton
        Left = 513
        Height = 21
        Top = 167
        Width = 17
        Action = actFailPlaySound
        Anchors = [akTop, akRight]
        TabOrder = 20
      end
    end
    object TabNetworkOptions: TTabSheet
      Caption = 'Network settings'
      ClientHeight = 317
      ClientWidth = 535
      ImageIndex = 2
      object LabelProxyHost: TLabel
        Left = 8
        Height = 13
        Top = 40
        Width = 52
        Caption = 'Proxy &host:'
        FocusControl = EditProxyHost
      end
      object LabelProxyPort: TLabel
        Left = 8
        Height = 13
        Top = 64
        Width = 50
        Caption = 'Proxy &port:'
        FocusControl = EditProxyPort
      end
      object LabelProxyUser: TLabel
        Left = 8
        Height = 13
        Top = 88
        Width = 78
        Caption = 'Proxy &username:'
        FocusControl = EditProxyUser
      end
      object LabelProxyPass: TLabel
        Left = 8
        Height = 13
        Top = 112
        Width = 77
        Caption = 'Proxy pass&word:'
        FocusControl = EditProxyPass
      end
      object CheckUseProxy: TCheckBox
        Left = 8
        Height = 19
        Top = 8
        Width = 520
        Anchors = [akTop, akLeft, akRight]
        Caption = 'U&se proxy server'
        TabOrder = 0
      end
      object EditProxyHost: TEdit
        Left = 112
        Height = 21
        Top = 36
        Width = 177
        TabOrder = 1
      end
      object EditProxyPort: TEdit
        Left = 112
        Height = 21
        Top = 60
        Width = 177
        TabOrder = 2
      end
      object EditProxyUser: TEdit
        Left = 112
        Height = 21
        Top = 84
        Width = 177
        TabOrder = 3
      end
      object EditProxyPass: TEdit
        Left = 112
        Height = 21
        Top = 108
        Width = 177
        TabOrder = 4
      end
    end
    object TabDownloaderOptions: TTabSheet
      Caption = 'Downloader settings'
      ClientHeight = 317
      ClientWidth = 535
      ImageIndex = 3
      object ListDownloaderOptions: TListBox
        Left = 0
        Height = 317
        Top = 0
        Width = 169
        Align = alLeft
        ItemHeight = 0
        OnClick = ListDownloaderOptionsClick
        ScrollWidth = 165
        Sorted = True
        TabOrder = 0
      end
      object PanelDownloaderOptions: TPanel
        Left = 169
        Height = 317
        Top = 0
        Width = 366
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
  object ActionList: TActionList
    Left = 16
    Top = 352
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
    object actStartPlaySound: TAction
      Caption = '...'
      OnExecute = actStartPlaySoundExecute
    end
    object actEndPlaySound: TAction
      Caption = '...'
      OnExecute = actEndPlaySoundExecute
    end
    object actFailPlaySound: TAction
      Caption = '...'
      OnExecute = actFailPlaySoundExecute
    end
  end
  object ODlg: TOpenDialog
    Filter = 'All files|*.*|WAV files|*.wav'
    Options = [ofFileMustExist, ofEnableSizing, ofViewDetail]
    Left = 71
    Top = 352
  end
end
