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
  ComCtrls,
  ShellCtrls,
  StdCtrls,
  ExtCtrls,
  Types,
  IniFiles,
  UnitIniWorkDir;

type
  TForm1 = class(TForm)
    ShellListView1: TShellListView;
    Memo1: TMemo;
    ShellTreeView1: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Image1: TImage;
    procedure ShellListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    CurrentPathName: string;
    IniWorkDir: TIniWorkDir;
  public
    { Public declarations }
    procedure AnalizeAndDisplayFile(FileName: string);
  end;

type
  TRGBQuad = packed record
    case Integer of
      0:
      (rgbBlue: Byte;
        rgbGreen: Byte;
        rgbRed: Byte;
        rgbReserved: Byte);
      1:
      (rgbValue: Cardinal);
  end;

  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGBQuad) - 1] of TRGBQuad;

  PRGB32Array = ^TRGB32Array;

  TScanline = TRGB32Array;

  PScanline = ^TScanline;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ShellListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if Item <> nil then
  begin
    if CurrentPathName <> ShellListView1.Folders[Item.Index].PathName then
    begin
      CurrentPathName := ShellListView1.Folders[Item.Index].PathName;
      //Memo1.Lines.Add('CurrentPathName = ' + CurrentPathName);
      if not ShellListView1.Folders[Item.Index].IsFolder then
        AnalizeAndDisplayFile(CurrentPathName);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IniWorkDir := TIniWorkDir.Create(Self);
  IniWorkDir.Add(ShellTreeView1);
end;

procedure TForm1.AnalizeAndDisplayFile(FileName: string);
var
  Bitmap: TBitmap;

  FileStream: TFileStream;
  FileBuffer: TByteDynArray;
  Width, Height: Integer;
  ImageSize: Integer;
  x, y, i: Integer;
  ColorIndex: Integer;
  ScanLine: PScanline;
  rgbValue: Cardinal;
begin
  FileBuffer := nil;
  Width := 0;
  Height := 0;
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  case FileStream.Size of
    64768:
      begin
        Width := 320;
        Height := 200;
      end;
    256768:
      begin
        Width := 640;
        Height := 400;
      end;
  end;
  if Width <> 0 then
  begin
    SetLength(FileBuffer, FileStream.Size);
    FileStream.Read(Pointer(FileBuffer)^, FileStream.Size);
  end;
  FileStream.Free;

  Image1.Picture.Assign(nil);

  if FileBuffer <> nil then
  begin
    ImageSize := Width * Height;

    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    //Memo1.Lines.Add(IntToStr(ImageSize));

    i := 0;
    for y := 0 to Height - 1 do
    begin
      ScanLine := Bitmap.ScanLine[y];
      for x := 0 to Width - 1 do
      begin
        ColorIndex := FileBuffer[i] * 3;
        //Image1.Canvas.Pixels[x, y] := RGB(FileBuffer[ImageSize + ColorIndex + 0], FileBuffer[ImageSize + ColorIndex + 1], FileBuffer[ImageSize + ColorIndex + 2]);
        rgbValue := RGB(FileBuffer[ImageSize + ColorIndex + 2], FileBuffer[ImageSize + ColorIndex + 1], FileBuffer[ImageSize + ColorIndex + 0]);
        ScanLine^[x].rgbValue := rgbValue;

        i := i + 1;
      end;
    end;

    Image1.Picture.Assign(Bitmap);
    //Image1.Canvas.CopyMode := cmSrcCopy;
    //Image1.Canvas.StretchDraw(Rect(0, 0, Image1.Width, Image1.Height), Bitmap);
    //Bitmap.Free;

  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IniWorkDir.Save;
end;

end.
