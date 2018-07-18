unit UnitMain;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ZLib,
  ComCtrls,
  ShellCtrls,
  ExtCtrls,
  IniFiles,
  Types,
  UnitPakFile,
  UnitIniWorkDir;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    ShellTreeView1: TShellTreeView;
    Splitter1: TSplitter;
    ShellListView1: TShellListView;
    Splitter2: TSplitter;
    ListView1: TListView;
    Memo1: TMemo;
    Panel1: TPanel;
    ButtonEject: TButton;
    ButtonInject: TButton;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    procedure ShellListView1AddFolder(Sender: TObject; AFolder: TShellFolder; var CanAdd: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ShellListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ButtonEjectClick(Sender: TObject);
    procedure ButtonInjectClick(Sender: TObject);
    procedure ShellTreeView1Enter(Sender: TObject);
  private
    { Private declarations }
    CurrentPathName: string;
    PakFile: TPakFile;
    IniWorkDir: TIniWorkDir;
  public
    { Public declarations }
    procedure AnalizeAndDisplayPakFile(FileName: string);
    procedure DisplayPakFile(PakFile: TPakFile; ItemIndex: Integer = -1);
    procedure EjectFromPak(PakFile: TPakFile; ItemIndex: Integer; DatFileName: string);
    procedure InjectToPak(PakFileName: string; ItemIndex: Integer; DatFileName: string);
  end;

const
  BUFFER_SIZE = 1024;
  ZLIB_HEADER: array[0..1] of Byte = ($78, $DA);

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  PakFile := TPakFile.Create;
  IniWorkDir := TIniWorkDir.Create(Self);
  IniWorkDir.Add(ShellTreeView1);
  IniWorkDir.Add(OpenDialog1);
  IniWorkDir.Add(SaveDialog1);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IniWorkDir.Save;
  PakFile.Free;
end;

procedure TForm1.ShellListView1AddFolder(Sender: TObject; AFolder: TShellFolder; var CanAdd: Boolean);
var
  FileExt: string;
begin
  CanAdd := not AFolder.IsFolder;
  if CanAdd then
  begin
    FileExt := ExtractFileExt(AFolder.PathName);
    CanAdd := CompareText(FileExt, '.pak') = 0;
  end;
end;

procedure TForm1.ShellListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if Item <> nil then
  begin
    if CurrentPathName <> ShellListView1.Folders[Item.Index].PathName then
    begin
      CurrentPathName := ShellListView1.Folders[Item.Index].PathName;
      StatusBar1.SimpleText := CurrentPathName;
      //Memo1.Lines.Add('CurrentPathName = ' + CurrentPathName);
      if not ShellListView1.Folders[Item.Index].IsFolder then
        AnalizeAndDisplayPakFile(CurrentPathName);
    end;
  end;
end;

procedure TForm1.AnalizeAndDisplayPakFile(FileName: string);
begin
  PakFile.LoadFromFile(FileName);
  DisplayPakFile(PakFile);
end;

procedure TForm1.DisplayPakFile(PakFile: TPakFile; ItemIndex: Integer);
var
  i: Integer;
  Item: TListItem;
begin
  ListView1.Clear;
  for i := 0 to High(PakFile.DatFiles) do
  begin
    Item := ListView1.Items.Add;
    Item.Caption := IntToHex(i, 4);
    Item.SubItems.Add(IntToHex(PakFile.DatFiles[i].Offset, 8)); //Offset
    Item.SubItems.Add('');                //I
    Item.SubItems.Add('');                //NA
    Item.SubItems.Add('');                //Name
    if PakFile.DatFiles[i].Header.NameLength > 0 then
    begin
      Item.SubItems[1] := PakFile.DatFiles[i].Name.LetterI;
      Item.SubItems[2] := IntToHex(PakFile.DatFiles[i].Name.NameAttr, 2);
      Item.SubItems[3] := PakFile.DatFiles[i].Name.Name;
    end;
    Item.SubItems.Add(IntToStr(PakFile.DatFiles[i].Header.CompSize));
    Item.SubItems.Add(IntToStr(PakFile.DatFiles[i].Header.UncompSize));
    Item.SubItems.Add(IntToStr(PakFile.DatFiles[i].Header.CompMethod));
  end;
  if ItemIndex >= 0 then
    ListView1.ItemIndex := ItemIndex;
end;

procedure TForm1.ButtonEjectClick(Sender: TObject);
begin
  if ListView1.Selected <> nil then
  begin
    SaveDialog1.FileName := IntToHex(ListView1.ItemIndex, 8) + '.dat';
    if SaveDialog1.Execute then
    begin
      EjectFromPak(PakFile, ListView1.ItemIndex, SaveDialog1.FileName);
    end;
  end;
end;

procedure TForm1.ButtonInjectClick(Sender: TObject);
begin
  if ListView1.Selected <> nil then
  begin
    if OpenDialog1.Execute then
    begin
      InjectToPak(CurrentPathName, ListView1.ItemIndex, OpenDialog1.FileName);
    end;
  end;
end;

procedure TForm1.EjectFromPak(PakFile: TPakFile; ItemIndex: Integer; DatFileName: string);
var
  RawMemoryStream: TMemoryStream;
  DatFileStream: TFileStream;
  Decompressor: TDecompressionStream;
begin
  if (ItemIndex < 0) or (ItemIndex > High(PakFile.DatFiles)) then
    Exit;
  DatFileStream := TFileStream.Create(DatFileName, fmCreate or fmShareDenyWrite);

  case PakFile.DatFiles[ItemIndex].Header.CompMethod of
    0:
      begin
        DatFileStream.WriteBuffer(Pointer(PakFile.DatFiles[ItemIndex].DataBuffer)^, PakFile.DatFiles[ItemIndex].Header.CompSize);
      end;
    4:
      begin
        RawMemoryStream := TMemoryStream.Create;
        RawMemoryStream.WriteBuffer(ZLIB_HEADER, 2);
        RawMemoryStream.WriteBuffer(Pointer(PakFile.DatFiles[ItemIndex].DataBuffer)^, PakFile.DatFiles[ItemIndex].Header.CompSize);
        RawMemoryStream.Position := 0;
        Decompressor := TDecompressionStream.Create(RawMemoryStream);
        DatFileStream.CopyFrom(Decompressor, PakFile.DatFiles[ItemIndex].Header.UncompSize);
        Decompressor.Free;
        RawMemoryStream.Free;
      end;
  end;
  DatFileStream.Free;
end;

procedure TForm1.InjectToPak(PakFileName: string; ItemIndex: Integer; DatFileName: string);
var
  RawMemoryStream: TMemoryStream;
  DatFileStream: TFileStream;
  Compressor: TCompressionStream;
  //FileOffset: Integer;
  //DatFileHeader: TDatFileHeader;
begin
  if (ItemIndex < 0) or (ItemIndex > High(PakFile.DatFiles)) then
    Exit;

  DatFileStream := TFileStream.Create(DatFileName, fmOpenRead or fmShareDenyNone);
  PakFile.DatFiles[ItemIndex].Header.UncompSize := DatFileStream.Size;
  PakFile.DatFiles[ItemIndex].Header.CompMethod := 4;
  RawMemoryStream := TMemoryStream.Create;
  Compressor := TCompressionStream.Create(clMax, RawMemoryStream);
  Compressor.CopyFrom(DatFileStream, 0);
  Compressor.Free;
  DatFileStream.Free;
  PakFile.DatFiles[ItemIndex].Header.CompSize := RawMemoryStream.Size - 2;
  SetLength(PakFile.DatFiles[ItemIndex].DataBuffer, PakFile.DatFiles[ItemIndex].Header.CompSize);
  RawMemoryStream.Position := 2;
  RawMemoryStream.ReadBuffer(Pointer(PakFile.DatFiles[ItemIndex].DataBuffer)^, PakFile.DatFiles[ItemIndex].Header.CompSize);
  RawMemoryStream.Free;

  PakFile.RebuildOffsets;

  PakFile.Save;

  DisplayPakFile(PakFile, ItemIndex);

end;

procedure TForm1.ShellTreeView1Enter(Sender: TObject);
begin
  ShellTreeView1.Refresh(ShellTreeView1.Selected);

end;

end.
