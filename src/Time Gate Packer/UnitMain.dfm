object Form1: TForm1
  Left = 292
  Top = 168
  AutoScroll = False
  Caption = 'Time Gate Packer'
  ClientHeight = 773
  ClientWidth = 1047
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 200
    Top = 0
    Height = 665
  end
  object Splitter2: TSplitter
    Left = 553
    Top = 0
    Height = 665
  end
  object ShellTreeView1: TShellTreeView
    Left = 0
    Top = 0
    Width = 200
    Height = 665
    AutoContextMenus = False
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
    TabOrder = 0
    OnEnter = ShellTreeView1Enter
  end
  object ShellListView1: TShellListView
    Left = 203
    Top = 0
    Width = 350
    Height = 665
    AutoContextMenus = False
    ObjectTypes = [otNonFolders]
    Root = 'rfDesktop'
    ShellTreeView = ShellTreeView1
    Sorted = True
    OnAddFolder = ShellListView1AddFolder
    Align = alLeft
    ColumnClick = False
    GridLines = True
    HideSelection = False
    RowSelect = True
    OnChange = ShellListView1Change
    TabOrder = 1
    ViewStyle = vsReport
  end
  object Memo1: TMemo
    Left = 0
    Top = 665
    Width = 1047
    Height = 89
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
    Visible = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 754
    Width = 1047
    Height = 19
    Panels = <>
  end
  object Panel2: TPanel
    Left = 556
    Top = 0
    Width = 491
    Height = 665
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 4
    object ListView1: TListView
      Left = 0
      Top = 33
      Width = 491
      Height = 632
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 35
        end
        item
          Caption = 'Off'
          Width = 70
        end
        item
          Caption = 'I'
          Width = 20
        end
        item
          Caption = 'NA'
          Width = 30
        end
        item
          Caption = 'FileName'
          Width = 180
        end
        item
          Caption = 'CSize'
        end
        item
          Caption = 'USize'
        end
        item
          Caption = 'CM'
          Width = 30
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 491
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object ButtonEject: TButton
        Left = 4
        Top = 4
        Width = 75
        Height = 25
        Caption = 'Eject'
        TabOrder = 0
        OnClick = ButtonEjectClick
      end
      object ButtonInject: TButton
        Left = 84
        Top = 4
        Width = 75
        Height = 25
        Caption = 'Inject'
        TabOrder = 1
        OnClick = ButtonInjectClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 96
    Top = 212
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 144
    Top = 208
  end
end
