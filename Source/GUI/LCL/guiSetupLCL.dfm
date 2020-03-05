object FormSetup: TFormSetup
  Left = 18
  Height = 200
  Top = 95
  Width = 411
  Caption = 'Setup'
  ClientHeight = 200
  ClientWidth = 411
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  LCLVersion = '1.7'
  Visible = False
  object LabelDestinationDir: TLabel
    Left = 8
    Height = 13
    Top = 16
    Width = 75
    Caption = 'Install &folder:'
    FocusControl = EditDestinationDir
    ParentColor = False
  end
  object EditDestinationDir: TEdit
    Left = 8
    Height = 19
    Top = 32
    Width = 369
    Anchors = [akTop, akLeft, akRight]
    TabOrder = 0
  end
  object BtnDestinationDir: TButton
    Left = 376
    Height = 21
    Top = 32
    Width = 17
    Anchors = [akTop, akRight]
    Caption = '...'
    OnClick = BtnDestinationDirClick
    TabOrder = 1
  end
  object CheckDesktopShortcut: TCheckBox
    Left = 8
    Height = 26
    Top = 64
    Width = 385
    Anchors = [akTop, akLeft, akRight]
    Caption = 'Create shortcut on &desktop'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object CheckStartMenuShortcut: TCheckBox
    Left = 8
    Height = 26
    Top = 80
    Width = 385
    Anchors = [akTop, akLeft, akRight]
    Caption = 'Create shortcut in &Start menu'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object ButtonInstall: TButton
    Left = 8
    Height = 25
    Top = 104
    Width = 385
    Anchors = [akTop, akLeft, akRight]
    Caption = '&Install'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object ButtonRun: TButton
    Left = 8
    Height = 25
    Top = 136
    Width = 385
    Anchors = [akTop, akLeft, akRight]
    Caption = '&Run from current location without installing'
    ModalResult = 5
    TabOrder = 5
  end
end
