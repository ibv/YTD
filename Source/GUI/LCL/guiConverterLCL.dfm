object FormSelectConverter: TFormSelectConverter
  Left = 192
  Height = 77
  Top = 107
  Width = 312
  BorderStyle = bsDialog
  Caption = 'Select converter'
  ClientHeight = 77
  ClientWidth = 312
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  Position = poMainFormCenter
  Visible = False
  object LabelConverter: TLabel
    Left = 8
    Height = 13
    Top = 8
    Width = 60
    Caption = '&Converter:'
    FocusControl = comboConverter
    ParentColor = False
  end
  object comboConverter: TComboBox
    Left = 88
    Height = 27
    Top = 8
    Width = 217
    ItemHeight = 0
    Style = csDropDownList
    TabOrder = 0
  end
  object Button1: TButton
    Left = 152
    Height = 25
    Top = 40
    Width = 75
    Action = actOK
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 232
    Height = 25
    Top = 40
    Width = 75
    Action = actCancel
    Cancel = True
    ModalResult = 2
    TabOrder = 2
  end
  object ActionList: TActionList
    left = 16
    top = 32
    object actOK: TAction
      Caption = '&OK'
      Enabled = False
      ShortCut = 16397
    end
    object actCancel: TAction
      Caption = '&Cancel'
      Enabled = False
    end
  end
end
