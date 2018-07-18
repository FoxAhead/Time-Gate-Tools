object Form1: TForm1
  Left = 192
  Top = 107
  Width = 1095
  Height = 914
  Caption = 'Time Gate Font'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 300
    Top = 33
    Height = 709
  end
  object ListView1: TListView
    Left = 0
    Top = 33
    Width = 300
    Height = 709
    Align = alLeft
    Columns = <
      item
        Caption = '#'
        Width = 35
      end
      item
        Caption = 'W'
        Width = 35
      end
      item
        Caption = 'H'
        Width = 35
      end
      item
        Caption = 'P1'
        Width = 30
      end
      item
        Caption = 'BH'
        Width = 35
      end
      item
        Caption = 'BW'
        Width = 35
      end
      item
        Caption = 'P2'
        Width = 30
      end
      item
        Caption = 'Off'
        Width = 45
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnKeyDown = ListView1KeyDown
    OnSelectItem = ListView1SelectItem
  end
  object Memo1: TMemo
    Left = 0
    Top = 742
    Width = 1087
    Height = 145
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1087
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 4
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Open Font'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 156
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Load Image'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 80
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Save Image'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 232
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Auto Size'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 308
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Save Font'
      TabOrder = 4
      OnClick = Button5Click
    end
  end
  object ScrollBox1: TScrollBox
    Left = 303
    Top = 33
    Width = 784
    Height = 709
    Align = alClient
    Constraints.MinWidth = 784
    TabOrder = 3
    object Image1: TImage
      Left = 4
      Top = 4
      Width = 512
      Height = 512
      Constraints.MaxHeight = 512
      Constraints.MaxWidth = 512
      Constraints.MinHeight = 512
      Constraints.MinWidth = 512
      OnMouseDown = Image1MouseDown
    end
    object Image2: TImage
      Left = 520
      Top = 4
      Width = 256
      Height = 256
      Constraints.MaxHeight = 256
      Constraints.MaxWidth = 256
      Constraints.MinHeight = 256
      Constraints.MinWidth = 256
      Stretch = True
      OnMouseDown = Image2MouseDown
    end
    object ValueListEditor1: TValueListEditor
      Left = 520
      Top = 264
      Width = 256
      Height = 253
      DefaultColWidth = 100
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goThumbTracking]
      ScrollBars = ssNone
      Strings.Strings = (
        '=')
      TabOrder = 0
      TitleCaptions.Strings = (
        'Char Parameter'
        'Value')
      OnSetEditText = ValueListEditor1SetEditText
      ColWidths = (
        100
        150)
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 28
    Top = 20
  end
  object OpenPictureDialog1: TOpenPictureDialog
    FilterIndex = 0
    Left = 176
    Top = 20
  end
  object SavePictureDialog1: TSavePictureDialog
    DefaultExt = '*.bmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 104
    Top = 20
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 331
    Top = 21
  end
end
