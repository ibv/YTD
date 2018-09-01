object FormOptions: TFormOptions
  Left = 284
  Top = 158
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 180
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelOverwriteMode: TLabel
    Left = 8
    Top = 52
    Width = 60
    Height = 13
    Caption = '&Existing files:'
    FocusControl = ComboOverwriteMode
  end
  object LabelDownloadDir: TLabel
    Left = 8
    Top = 84
    Width = 94
    Height = 13
    Caption = '&Download directory:'
    FocusControl = EditDownloadDir
  end
  object LabelConverter: TLabel
    Left = 8
    Top = 116
    Width = 49
    Height = 13
    Caption = '&Converter:'
    FocusControl = ComboConverter
  end
  object CheckAutoDownload: TCheckBox
    Left = 8
    Top = 16
    Width = 289
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Automatically start downloads'
    TabOrder = 0
  end
  object ComboOverwriteMode: TComboBox
    Left = 112
    Top = 48
    Width = 185
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      'Ask user'
      'Overwrite'
      'Skip'
      'Rename automatically')
  end
  object EditDownloadDir: TEdit
    Left = 112
    Top = 80
    Width = 169
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object BtnDownloadDir: TButton
    Left = 280
    Top = 80
    Width = 17
    Height = 21
    Action = actDownloadDir
    Anchors = [akTop, akRight]
    TabOrder = 3
  end
  object ComboConverter: TComboBox
    Left = 112
    Top = 112
    Width = 185
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
    OnChange = ComboConverterChange
  end
  object BtnOK: TButton
    Left = 144
    Top = 144
    Width = 75
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 224
    Top = 144
    Width = 75
    Height = 25
    Action = actCancel
    Anchors = [akRight, akBottom]
    Cancel = True
    ModalResult = 2
    TabOrder = 6
  end
  object ActionList: TActionList
    Left = 16
    Top = 136
    object actOK: TAction
      Caption = '&OK'
      ShortCut = 16397
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = '&Cancel'
      ShortCut = 27
    end
    object actDownloadDir: TAction
      Caption = '...'
      ShortCut = 121
      OnExecute = actDownloadDirExecute
    end
  end
end
