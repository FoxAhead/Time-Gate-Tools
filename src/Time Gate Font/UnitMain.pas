unit UnitMain;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Types,
  ExtDlgs,
  Math,
  UnitIniWorkDir,
  Grids,
  ValEdit;

type
  TFontHeader = packed record
    Flag1: Byte;
    None1: Byte;
    IndexLow: Byte;
    IndexHigh: Byte;
    None2: Word;
  end;

  PFontHeader = ^TFontHeader;

  TCharHeader = packed record
    Offset: Integer;
    Width: Byte;
    Height: Byte;
    Pad1: ShortInt;
    BaseHeight: Byte;
    BaseWidth: Byte;
    Pad2: Byte;
  end;

  PCharHeader = ^TCharHeader;

  TForm1 = class(TForm)
    ListView1: TListView;
    Button1: TButton;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Panel1: TPanel;
    OpenPictureDialog1: TOpenPictureDialog;
    Button2: TButton;
    SavePictureDialog1: TSavePictureDialog;
    Button3: TButton;
    Splitter1: TSplitter;
    ScrollBox1: TScrollBox;
    Image2: TImage;
    Button4: TButton;
    Button5: TButton;
    SaveDialog1: TSaveDialog;
    ValueListEditor1: TValueListEditor;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ValueListEditor1SetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    IniWorkDir: TIniWorkDir;
  public
    { Public declarations }
    FontBitmap: TBitmap;
    FontHeader: TFontHeader;
    procedure LoadFontFile(FileName: string);
    procedure SaveFontFile(FileName: string);
    procedure DrawCharRect(CharIndex: Integer);
    procedure DrawChar(CharIndex: Integer);
    procedure ShowCharParams(Item: TListItem);
    function CharIndexToRow(CharIndex: Integer): Integer;
    function CharIndexToCol(CharIndex: Integer): Integer;
    function CharIndexToCoor(CharIndex: Integer): TPoint;
    function CoorToCharIndex(X, Y: Integer): Integer;
  end;

const
  MAP_CELL_SIZE = 32;
  MAP_COLUMNS = 16;
  MAP_ROWS = 16;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    LoadFontFile(OpenDialog1.FileName);
  end;

end;

procedure TForm1.LoadFontFile(FileName: string);
var
  FileStream: TFileStream;
  FileBuffer: TByteDynArray;
  i, j, CharIndex: Integer;
  CharHeader: PCharHeader;
  Item: TListItem;
  X, Y: Integer;
  FontPixel: Integer;
  PixelColor: TColor;
begin
  FileBuffer := nil;
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  SetLength(FileBuffer, FileStream.Size);
  FileStream.Read(Pointer(FileBuffer)^, FileStream.Size);
  FileStream.Free;

  FontHeader := PFontHeader(@FileBuffer[0])^;

  ListView1.Clear;
  FontBitmap.PixelFormat := pf32bit;
  FontBitmap.Width := 16 * MAP_CELL_SIZE;
  FontBitmap.Height := 16 * MAP_CELL_SIZE;

  FontBitmap.Canvas.Brush.Color := clWhite;
  FontBitmap.Canvas.Brush.Style := bsSolid;
  FontBitmap.Canvas.FillRect(Bounds(0, 0, FontBitmap.Width, FontBitmap.Height));

  i := 8;
  for CharIndex := FontHeader.IndexLow to FontHeader.IndexHigh do
  begin
    if i > High(FileBuffer) then
      Break;
    CharHeader := @FileBuffer[i];

    Item := ListView1.Items.Add;
    Item.Caption := IntToHex(CharIndex, 2);
    Item.SubItems.Add(IntToStr(CharHeader^.Width));
    Item.SubItems.Add(IntToStr(CharHeader^.Height));
    Item.SubItems.Add(IntToStr(CharHeader^.Pad1));
    Item.SubItems.Add(IntToStr(CharHeader^.BaseHeight));
    Item.SubItems.Add(IntToStr(CharHeader^.BaseWidth));
    Item.SubItems.Add(IntToStr(CharHeader^.Pad2));
    Item.SubItems.Add(IntToHex(CharHeader^.Offset, 4));

    j := CharHeader^.Offset;
    if j > 0 then
    begin
      for Y := 0 to CharHeader^.Height - 1 do
      begin
        for X := 0 to CharHeader^.Width - 1 do
        begin
          if j > High(FileBuffer) then
            Break;
          FontPixel := FileBuffer[j];
          if FontPixel = 0 then
            PixelColor := clWhite
          else
            PixelColor := clBlack;
          FontBitmap.Canvas.Pixels[X + CharIndex mod 16 * MAP_CELL_SIZE, Y + CharIndex div 16 * MAP_CELL_SIZE] := PixelColor;
          j := j + 1;
        end;
      end;
    end;

    i := i + $0A;
  end;

  Image1.Picture.Assign(nil);

  //Image1.Picture.Bitmap.Width := Image1.Width;
  //Image1.Picture.Bitmap.Height := Image1.Height;
  Image1.Canvas.Brush.Color := clBlack;
  Image1.Canvas.Brush.Style := bsSolid;
  Image1.Canvas.FillRect(Rect(0, 0, Image1.Width, Image1.Height));

  Image1.Canvas.CopyMode := cmSrcCopy;
  Image1.Canvas.CopyRect(Bounds(0, 0, FontBitmap.Width, FontBitmap.Height), FontBitmap.Canvas, Bounds(0, 0, FontBitmap.Width, FontBitmap.Height));

end;

procedure TForm1.SaveFontFile(FileName: string);
var
  FileStream: TFileStream;
  FileBuffer: TByteDynArray;
  i, j, k, CharIndex: Integer;
  CharHeader: PCharHeader;
  Item: TListItem;
  X, Y: Integer;
  PixelColor: TColor;
  CharCoor: TPoint;
  CharIsNotNull: Boolean;
begin
  FileBuffer := nil;
  SetLength(FileBuffer, MAP_CELL_SIZE * MAP_CELL_SIZE * 256);
  PFontHeader(@FileBuffer[0])^ := FontHeader;

  i := 8;
  j := 8 + (FontHeader.IndexHigh - FontHeader.IndexLow + 1) * $0A;
  for CharIndex := FontHeader.IndexLow to FontHeader.IndexHigh do
  begin
    CharHeader := @FileBuffer[i];
    Item := ListView1.Items[CharIndex - FontHeader.IndexLow];
    CharHeader^.Width := StrToInt(Item.SubItems[0]);
    CharHeader^.Height := StrToInt(Item.SubItems[1]);
    CharHeader^.Pad1 := StrToInt(Item.SubItems[2]);
    CharHeader^.BaseHeight := StrToInt(Item.SubItems[3]);
    CharHeader^.BaseWidth := StrToInt(Item.SubItems[4]);
    CharHeader^.Pad2 := StrToInt(Item.SubItems[5]);
    CharHeader^.Offset := $00000000;

    CharCoor := CharIndexToCoor(CharIndex);
    CharIsNotNull := False;
    k := 0;
    for Y := 0 to CharHeader^.Height - 1 do
    begin
      for X := 0 to CharHeader^.Width - 1 do
      begin
        PixelColor := FontBitmap.Canvas.Pixels[CharCoor.X + X, CharCoor.Y + Y];
        if PixelColor <> clWhite then
        begin
          FileBuffer[j + k] := $01;
          CharIsNotNull := True;
        end
        else
        begin
          FileBuffer[j + k] := $00;
        end;
        k := k + 1;
      end;
    end;

    if CharIsNotNull then
    begin
      CharHeader^.Offset := j;
      j := j + k;
    end;
    i := i + $0A;
  end;

  SetLength(FileBuffer, j);

  FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  FileStream.Write(Pointer(FileBuffer)^, Length(FileBuffer));
  FileStream.Free;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    FontBitmap.LoadFromFile(OpenPictureDialog1.FileName);
    Image1.Canvas.CopyMode := cmSrcCopy;
    Image1.Canvas.CopyRect(Bounds(0, 0, FontBitmap.Width, FontBitmap.Height), FontBitmap.Canvas, Bounds(0, 0, FontBitmap.Width, FontBitmap.Height));
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
  begin
    FontBitmap.SaveToFile(SavePictureDialog1.FileName);
  end;
end;

procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (Item <> nil) and Selected then
  begin
    DrawCharRect(Item.Index + FontHeader.IndexLow);
    DrawChar(Item.Index + FontHeader.IndexLow);
    ShowCharParams(Item);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FontBitmap := TBitmap.Create;
  Image2.Picture.Bitmap.Width := MAP_CELL_SIZE;
  Image2.Picture.Bitmap.Height := MAP_CELL_SIZE;

  IniWorkDir := TIniWorkDir.Create(Self);
  IniWorkDir.Add(OpenDialog1);
  IniWorkDir.Add(SavePictureDialog1);
  IniWorkDir.Add(OpenPictureDialog1);
  IniWorkDir.Add(SaveDialog1);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IniWorkDir.Save;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FontBitmap.Free;
end;

procedure TForm1.DrawCharRect(CharIndex: Integer);
begin
  Image1.Canvas.CopyMode := cmSrcCopy;
  Image1.Canvas.CopyRect(Bounds(0, 0, FontBitmap.Width, FontBitmap.Height), FontBitmap.Canvas, Bounds(0, 0, FontBitmap.Width, FontBitmap.Height));

  Image1.Canvas.Pen.Style := psDot;
  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Rectangle(Bounds(CharIndex mod 16 * MAP_CELL_SIZE, CharIndex div 16 * MAP_CELL_SIZE, MAP_CELL_SIZE, MAP_CELL_SIZE));
end;

procedure TForm1.DrawChar(CharIndex: Integer);
begin
  Image2.Canvas.Brush.Color := clWhite;
  Image2.Canvas.Brush.Style := bsSolid;
  Image2.Canvas.FillRect(Bounds(0, 0, MAP_CELL_SIZE, MAP_CELL_SIZE));

  Image2.Canvas.CopyMode := cmSrcCopy;
  Image2.Canvas.CopyRect(Bounds(0, 0, MAP_CELL_SIZE, MAP_CELL_SIZE), FontBitmap.Canvas, Bounds(CharIndex mod 16 * MAP_CELL_SIZE, CharIndex div 16 * MAP_CELL_SIZE, MAP_CELL_SIZE, MAP_CELL_SIZE));
end;

procedure TForm1.ShowCharParams(Item: TListItem);
var
  i: Integer;
begin
  ValueListEditor1.Strings.Clear;
  for i := 1 to ListView1.Columns.Count - 2 do
  begin
    ValueListEditor1.Values[ListView1.Columns[i].Caption] := Item.SubItems[i - 1];
    ValueListEditor1.ItemProps[ListView1.Columns[i].Caption].EditMask := '99;1; ';
  end;
end;

procedure TForm1.ValueListEditor1SetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  NewValue: string;
begin
  try
    NewValue := Trim(Value);
    StrToInt(NewValue);
    ListView1.Selected.SubItems[ARow - 1] := NewValue;
  except
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  CharIndex: Integer;
  CharCoor: TPoint;
  CharRect: TRect;
  i, X, Y: Integer;
  Width, Height: Integer;
  LetterRect: TRect;
  DestRect: TRect;
  SrcRect: TRect;
  TmpBitmap: TBitmap;
  TmpBitmapRect: TRect;
  BaseLine: Integer;
begin
  TmpBitmap := TBitmap.Create;
  TmpBitmap.Width := MAP_CELL_SIZE;
  TmpBitmap.Height := MAP_CELL_SIZE;
  TmpBitmapRect := Bounds(0, 0, TmpBitmap.Width, TmpBitmap.Height);
  TmpBitmap.Canvas.CopyMode := cmSrcCopy;
  TmpBitmap.Canvas.Brush.Color := clWhite;
  FontBitmap.Canvas.CopyMode := cmSrcCopy;

  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if i + FontHeader.IndexLow >= $80 then
    begin
      CharIndex := i + FontHeader.IndexLow;
      CharCoor := CharIndexToCoor(CharIndex);
      CharRect := Bounds(CharCoor.X, CharCoor.Y, MAP_CELL_SIZE, MAP_CELL_SIZE);
      LetterRect := Rect(MAP_CELL_SIZE, MAP_CELL_SIZE, -1, -1);
      BaseLine := -1;
      for Y := 0 to MAP_CELL_SIZE - 1 do
      begin
        for X := 0 to MAP_CELL_SIZE - 1 do
        begin
          case FontBitmap.Canvas.Pixels[CharCoor.X + X, CharCoor.Y + Y] of
            clBlack:
              begin
                UnionRect(LetterRect, LetterRect, Bounds(X, Y, 1, 1));
              end;
            clFuchsia:
              begin
                BaseLine := Y;
              end;
          end;
        end;
      end;
      Width := LetterRect.Right - LetterRect.Left;
      Height := LetterRect.Bottom - LetterRect.Top;
      if IsRectEmpty(LetterRect) then
      begin
        Width := 8;
        Height := 8;
      end;
      BaseLine := BaseLine - LetterRect.Top;
      if BaseLine < 0 then
        BaseLine := Height;
      ListView1.Items[i].SubItems[0] := IntToStr(Width);
      ListView1.Items[i].SubItems[1] := IntToStr(Height);
      ListView1.Items[i].SubItems[3] := IntToStr(BaseLine);
      ListView1.Items[i].SubItems[4] := IntToStr(Width);

      if not IsRectEmpty(LetterRect) and not PtInRect(LetterRect, Point(0, 0)) then
      begin
        DestRect := LetterRect;
        OffsetRect(DestRect, -LetterRect.Left, -LetterRect.Top);
        SrcRect := LetterRect;
        OffsetRect(SrcRect, CharCoor.X, CharCoor.Y);

        TmpBitmap.Canvas.FillRect(TmpBitmapRect);
        TmpBitmap.Canvas.CopyRect(DestRect, FontBitmap.Canvas, SrcRect);
        FontBitmap.Canvas.Draw(CharCoor.X, CharCoor.Y, TmpBitmap);

      end;

    end;
  end;

  DrawCharRect(0);
  TmpBitmap.Free;
end;

function TForm1.CharIndexToCol(CharIndex: Integer): Integer;
begin
  Result := CharIndex mod MAP_COLUMNS;
end;

function TForm1.CharIndexToRow(CharIndex: Integer): Integer;
begin
  Result := CharIndex div MAP_COLUMNS;
end;

function TForm1.CharIndexToCoor(CharIndex: Integer): TPoint;
begin
  Result := Point(CharIndex mod MAP_COLUMNS * MAP_CELL_SIZE, CharIndex div MAP_COLUMNS * MAP_CELL_SIZE);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    SaveFontFile(SaveDialog1.FileName)
  end;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ItemRect: TRect;
  IntersectionRect: TRect;
  NewIndex: Integer;
begin
  ListView1.ClearSelection;
  NewIndex := CoorToCharIndex(X, Y);
  NewIndex := Math.Max(FontHeader.IndexLow, NewIndex);
  NewIndex := Math.Min(FontHeader.IndexHigh, NewIndex);
  NewIndex := NewIndex - FontHeader.IndexLow;
  ListView1.ItemIndex := NewIndex;
  ListView1.Selected.Focused := True;
  ItemRect := ListView1.Selected.DisplayRect(drBounds);
  if not IntersectRect(IntersectionRect, ItemRect, ListView1.ClientRect) then
    ListView1.Scroll(0, ItemRect.Top - ListView1.ClientHeight div 2);
  ListView1.SetFocus;
end;

function TForm1.CoorToCharIndex(X, Y: Integer): Integer;
begin
  Result := (Y div MAP_CELL_SIZE) * MAP_COLUMNS + X div MAP_CELL_SIZE;
end;

procedure TForm1.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  NewIndex: Integer;
begin
  NewIndex := ListView1.ItemIndex;
  case Key of
    VK_LEFT:
      NewIndex := NewIndex - 1;
    VK_RIGHT:
      NewIndex := NewIndex + 1;
    VK_DOWN:
      begin
        NewIndex := NewIndex + MAP_COLUMNS;
        Key := 0;
      end;
    VK_UP:
      begin
        NewIndex := NewIndex - MAP_COLUMNS;
        Key := 0;
      end
  else
    Exit;
  end;

  ListView1.ClearSelection;
  ListView1.ItemIndex := Min(Max(NewIndex, 0), ListView1.Items.Count - 1);
  ListView1.Selected.Focused := True;
end;

procedure TForm1.Image2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    if ListView1.Selected <> nil then
    begin
      ListView1.Selected.SubItems[3] := IntToStr(Y div (Image2.Width div MAP_CELL_SIZE));
      ShowCharParams(ListView1.Selected);
    end;
end;

end.
