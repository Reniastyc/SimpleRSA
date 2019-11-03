program RSAProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  RSAForm in 'RSAForm.pas' {RSATest},
  RdeEM.Long in 'RdeEM.Long.pas',
  RdeEM.Prime in 'RdeEM.Prime.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRSATest, RSATest);
  Application.Run;
end.
