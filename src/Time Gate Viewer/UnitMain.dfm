object Form1: TForm1
  Left = 303
  Top = 141
  Width = 1142
  Height = 785
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 200
    Top = 0
    Height = 625
  end
  object Splitter2: TSplitter
    Left = 545
    Top = 0
    Height = 625
  end
  object Image1: TImage
    Left = 548
    Top = 0
    Width = 586
    Height = 625
    Align = alClient
    Center = True
    Constraints.MinWidth = 64
    Proportional = True
    Stretch = True
  end
  object ShellListView1: TShellListView
    Left = 203
    Top = 0
    Width = 342
    Height = 625
    AutoContextMenus = False
    AutoNavigate = False
    ObjectTypes = [otFolders, otNonFolders]
    Root = 'rfDesktop'
    ShellTreeView = ShellTreeView1
    Sorted = True
    Align = alLeft
    ReadOnly = False
    GridLines = True
    HideSelection = False
    RowSelect = True
    OnChange = ShellListView1Change
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Memo1: TMemo
    Left = 0
    Top = 625
    Width = 1134
    Height = 133
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
    Visible = False
  end
  object ShellTreeView1: TShellTreeView
    Left = 0
    Top = 0
    Width = 200
    Height = 625
    ObjectTypes = [otFolders]
    Root = 'rfDesktop'
    ShellListView = ShellListView1
    UseShellImages = True
    Align = alLeft
    AutoRefresh = False
    HideSelection = False
    Indent = 19
    ParentColor = False
    RightClickSelect = True
    ShowRoot = False
    TabOrder = 2
  end
end
