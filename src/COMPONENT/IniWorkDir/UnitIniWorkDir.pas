unit UnitIniWorkDir;

interface

uses
  Classes,
  IniFiles;

type
  TIniWorkDir = class(TComponent)
  private
    CompList: TList;
    IniFile: TMemIniFile;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(Component: TComponent);
    procedure Save;
    function GetExistingPath(Path: string): string;
  published
  end;

implementation

uses
  Dialogs,
  Forms,
  SysUtils,
  ShellCtrls;

{ TTTIniWorkDir }

procedure TIniWorkDir.Add(Component: TComponent);
var
  Path: string;
begin
  Path := GetExistingPath(IniFile.ReadString('WorkDirs', Component.Name, ''));
  if Path <> '' then
  begin
    if Component is TOpenDialog then
      TOpenDialog(Component).InitialDir := Path;
    if Component is TShellTreeView then
    begin
      try
        TShellTreeView(Component).Path := Path;
      except
      end;
    end;
  end;
  CompList.Add(Component);
end;

constructor TIniWorkDir.Create(AOwner: TComponent);
begin
  inherited;
  CompList := TList.Create;
  IniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
end;

destructor TIniWorkDir.Destroy;
begin
  IniFile.Free;
  CompList.Free;
  inherited;
end;

function TIniWorkDir.GetExistingPath(Path: string): string;
var
  TestPath: string;
  ParentPath: string;
begin
  TestPath := Path;
  while not DirectoryExists(TestPath) do
  begin
    ParentPath := ExtractFilePath(ExcludeTrailingPathDelimiter(TestPath));
    if ParentPath = TestPath then
    begin
      Result := '';
      Exit;
    end;
    TestPath := ParentPath;
  end;
  Result := TestPath;
end;

procedure TIniWorkDir.Save;
var
  i: Integer;
  Component: TComponent;
  Path: string;
begin
  for i := 0 to CompList.Count - 1 do
  begin
    try
      Component := TComponent(CompList[i]);
      Path := '';
      if Component is TOpenDialog then
        Path := ExtractFilePath(TOpenDialog(Component).FileName);
      if Component is TShellTreeView then
        Path := TShellTreeView(Component).Path;
      if Path <> '' then
        IniFile.WriteString('WorkDirs', Component.Name, Path);
    except
    end;
  end;
  IniFile.UpdateFile;
end;

end.
