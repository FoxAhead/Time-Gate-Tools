program TimeGateFont;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  UnitIniWorkDir in '..\COMPONENT\IniWorkDir\UnitIniWorkDir.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Font - Time Gate';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
