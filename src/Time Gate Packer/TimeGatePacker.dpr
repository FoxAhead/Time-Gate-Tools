program TimeGatePacker;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  UnitPakFile in 'UnitPakFile.pas',
  UnitIniWorkDir in '..\COMPONENT\IniWorkDir\UnitIniWorkDir.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Packer - Time Gate';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
