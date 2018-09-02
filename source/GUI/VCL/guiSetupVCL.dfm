object FormSetup: TFormSetup
  Left = 18
  Top = 95
  Width = 411
  Height = 200
  Caption = 'Setup'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDestinationDir: TLabel
    Left = 8
    Top = 16
    Width = 59
    Height = 13
    Caption = 'Install &folder:'
    FocusControl = EditDestinationDir
  end
  object EditDestinationDir: TEdit
    Left = 8
    Top = 32
    Width = 369
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object BtnDestinationDir: TButton
    Left = 376
    Top = 32
    Width = 17
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = BtnDestinationDirClick
  end
  object CheckDesktopShortcut: TCheckBox
    Left = 8
    Top = 64
    Width = 385
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Create shortcut on &desktop'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object CheckStartMenuShortcut: TCheckBox
    Left = 8
    Top = 80
    Width = 385
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Create shortcut in &Start menu'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object ButtonInstall: TButton
    Left = 8
    Top = 104
    Width = 385
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Install'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object ButtonRun: TButton
    Left = 8
    Top = 136
    Width = 385
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Run from current location without installing'
    ModalResult = 5
    TabOrder = 5
  end
end
