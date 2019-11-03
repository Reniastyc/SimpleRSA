program MainProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormUnit in 'FormUnit.pas' {Form1},
  RdeEM.Prime in 'RdeEM.Prime.pas',
  RdeEM.Long in 'RdeEM.Long.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
