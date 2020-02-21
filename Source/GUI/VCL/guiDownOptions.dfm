object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 275
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    505
    275)
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOK: TButton
    Left = 342
    Top = 250
    Width = 75
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 422
    Top = 250
    Width = 75
    Height = 25
    Action = actCancel
    Anchors = [akRight, akBottom]
    Cancel = True
    ModalResult = 2
    TabOrder = 1
  end
  object StringGrid1: TStringGrid
    Left = 72
    Top = 8
    Width = 425
    Height = 236
    BevelInner = bvNone
    BevelOuter = bvNone
    ColCount = 2
    DefaultRowHeight = 36
    DoubleBuffered = False
    RowCount = 6
    ParentDoubleBuffered = False
    ScrollBars = ssNone
    TabOrder = 2
  end
  object ActionList: TActionList
    Left = 8
    Top = 240
    object actOK: TAction
      Caption = '&OK'
      ShortCut = 16397
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = '&Cancel'
      ShortCut = 27
      OnExecute = actCancelExecute
    end
    object actDownloadDir: TAction
      Caption = '...'
      ShortCut = 32836
    end
    object actDesktopShortcut: TAction
      Caption = 'Create shortcut on &desktop'
    end
    object actStartMenuShortcut: TAction
      Caption = 'Create shortcut in &Start menu'
    end
  end
end
