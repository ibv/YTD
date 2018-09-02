object FormSelectConverter: TFormSelectConverter
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Select converter'
  ClientHeight = 77
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelConverter: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 13
    Caption = '&Converter:'
    FocusControl = comboConverter
  end
  object comboConverter: TComboBox
    Left = 88
    Top = 8
    Width = 217
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 152
    Top = 40
    Width = 75
    Height = 25
    Action = actOK
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 232
    Top = 40
    Width = 75
    Height = 25
    Action = actCancel
    Cancel = True
    ModalResult = 2
    TabOrder = 2
  end
  object ActionList: TActionList
    Left = 16
    Top = 32
    object actOK: TAction
      Caption = '&OK'
      ShortCut = 16397
    end
    object actCancel: TAction
      Caption = '&Cancel'
    end
  end
end
