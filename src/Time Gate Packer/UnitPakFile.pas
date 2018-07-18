unit UnitPakFile;

interface

uses
  Types;

type
  TDatFileHeader = packed record
    Zero1: Integer;
    CompSize: Integer;
    UncompSize: Integer;
    CompMethod: Byte;
    Zero2: Byte;
    NameLength: Word;
  end;

  PDatFileHeader = ^TDatFileHeader;

  TDatFileName = packed record
    LetterI: Char;
    NameAttr: Byte;
    Name: array[0..253] of Char;
  end;

  PDatFileName = ^TDatFileName;

  TDatFile = record
    Offset: Integer;
    Header: TDatFileHeader;
    Name: TDatFileName;
    DataBuffer: TByteDynArray;
  end;

  TDatFileDynArray = array of TDatFile;

  TPakFile = class
  private
    procedure Clear; overload;
    procedure Clear(Index: Integer); overload;
  protected
  public
    FileName: string;
    DatFiles: TDatFileDynArray;
    constructor Create;
    destructor Destroy; override;
    procedure RebuildOffsets;
    procedure LoadFromFile(FileName: string);
    procedure Save;
  published
  end;

implementation

uses
  Classes,
  SysUtils,
  Windows;

{ TPakFile }

procedure TPakFile.Clear;
var
  i: Integer;
begin
  FileName := '';
  for i := 0 to High(DatFiles) do
  begin
    DatFiles[i].DataBuffer := nil;
  end;
  DatFiles := nil;
end;

procedure TPakFile.Clear(Index: Integer);
begin
  DatFiles[Index].Offset := 0;
  ZeroMemory(@DatFiles[Index].Header, SizeOf(DatFiles[Index].Header));
  ZeroMemory(@DatFiles[Index].Name, SizeOf(DatFiles[Index].Name));
  DatFiles[Index].DataBuffer := nil;
end;

constructor TPakFile.Create;
begin
  inherited;

end;

destructor TPakFile.Destroy;
begin
  Clear;
  inherited;
end;

procedure TPakFile.LoadFromFile(FileName: string);
var
  FileStream: TFileStream;
  i: Integer;
  FileOffset: Integer;
begin
  Clear;
  Self.FileName := FileName;
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  FileStream.Position := 4;
  i := -1;
  repeat
    try
      FileStream.ReadBuffer(FileOffset, 4);
      if FileOffset = 0 then
        Break;
      i := i + 1;
      SetLength(DatFiles, i + 1);
      DatFiles[i].Offset := FileOffset;
    except
      Break;
    end;
  until False;
  for i := 0 to High(DatFiles) do
  begin
    FileStream.Position := DatFiles[i].Offset;
    FileStream.ReadBuffer(DatFiles[i].Header, SizeOf(DatFiles[i].Header));
    if DatFiles[i].Header.NameLength > 0 then
      FileStream.ReadBuffer(DatFiles[i].Name, DatFiles[i].Header.NameLength);
    SetLength(DatFiles[i].DataBuffer, DatFiles[i].Header.CompSize);
    FileStream.ReadBuffer(Pointer(DatFiles[i].DataBuffer)^, DatFiles[i].Header.CompSize);
  end;
  FileStream.Free;
end;

procedure TPakFile.RebuildOffsets;
var
  Offset: Integer;
  i: Integer;
begin
  Offset := 4 + Length(DatFiles) * 4 + 4;
  for i := 0 to High(DatFiles) do
  begin
    DatFiles[i].Offset := Offset;
    Offset := Offset + SizeOf(TDatFileHeader);
    Offset := Offset + DatFiles[i].Header.NameLength;
    Offset := Offset + Length(DatFiles[i].DataBuffer);
  end;
end;

procedure TPakFile.Save;
var
  FileStream: TFileStream;
  OutValue: Integer;
  i: Integer;
begin
  FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  OutValue := 0;
  FileStream.WriteBuffer(OutValue, 4);
  for i := 0 to High(DatFiles) do
  begin
    FileStream.WriteBuffer(DatFiles[i].Offset, 4);
  end;
  FileStream.WriteBuffer(OutValue, 4);
  for i := 0 to High(DatFiles) do
  begin
    FileStream.Position := DatFiles[i].Offset;
    FileStream.WriteBuffer(DatFiles[i].Header, SizeOf(DatFiles[i].Header));
    if DatFiles[i].Header.NameLength > 0 then
      FileStream.WriteBuffer(DatFiles[i].Name, DatFiles[i].Header.NameLength);
    FileStream.WriteBuffer(Pointer(DatFiles[i].DataBuffer)^, DatFiles[i].Header.CompSize);
  end;
  FileStream.Free;
end;

end.
