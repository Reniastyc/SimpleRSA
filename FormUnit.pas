unit FormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.Edit,
  RdeEM.Long, RdeEM.Prime, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Edit4: TEdit;
    Button3: TButton;
    Edit5: TEdit;
    Button4: TButton;
    Edit7: TEdit;
    Edit8: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  PrimeL: TObjectList<TLongInteger>;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  LI1, LI2, LI3, LI4, LI5: THexInt;
  s: string;
  t1, t2: Integer;
begin
  Memo1.Lines.Clear;
  LI1.DecValue := Edit1.Text;
  LI2.DecValue := Edit2.Text;
  LI3.DecValue := Edit3.Text;
  LI4 := LI1 + LI2;
  s := Format('%s + %s = %s', [LI1.DecValue, LI2.DecValue, LI4.DecValue]);
  Memo1.Lines.Add(s);
  LI4 := LI1 - LI2;
  s := Format('%s - %s = %s', [LI1.DecValue, LI2.DecValue, LI4.DecValue]);
  Memo1.Lines.Add(s);
  t1 := TThread.GetTickCount;
  LI4 := LI1 * LI2;
  s := Format('%s * %s = %s', [LI1.DecValue, LI2.DecValue, LI4.DecValue]);
  Memo1.Lines.Add(s);
  t2 := TThread.GetTickCount;
  Memo1.Lines.Add(Format('FFT Time: %d', [t2 - t1]));
  try
    LI4 := LI1 div LI2;
    LI5 := LI1 mod LI2;
    s := Format('%s / %s = %s ...... %s', [LI1.DecValue, LI2.DecValue, LI4.DecValue, LI5.DecValue]);
    Memo1.Lines.Add(s);
  except
    on EZeroError do
    begin
      Memo1.Lines.Add('Error Message: Cannot divide ZERO!');
    end;
  end;
  try
    t1 := TThread.GetTickCount;
    LI4 := LI1.PowerMod(LI2, LI3);
    s := Format('%s ^ %s %% %s = %s', [LI1.DecValue, LI2.DecValue, LI3.DecValue, LI4.DecValue]);
    Memo1.Lines.Add(s);
    t2 := TThread.GetTickCount;
    Memo1.Lines.Add(Format('FFT Time: %d', [t2 - t1]));
  Except
    on EZeroError do
    begin
      Memo1.Lines.Add('Error Message: Cannot divide ZERO!');
    end;
    on EPowerError do
    begin
      Memo1.Lines.Add('Error Message: Power index must be greater than ZERO!');
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  SL: TStringList;
begin
  Memo1.Lines.Clear;
  SL := PrimeNumbersGenerate16(StrToIntDef(Edit4.Text, 0));
  try
    Memo1.Lines.Text := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  LI: TLongInteger;
begin
  Memo1.Lines.Clear;
  LI := TLongInteger.Create;
  try
    LI.FromString10(Edit5.Text);
    if PrimalityTest(LI, PrimeL) then
    begin
      Memo1.Lines.Text := Format('"%s" is a Prime Number.', [Edit5.Text]);
    end
    else
    begin
      Memo1.Lines.Text := Format('"%s" is not a Prime Number.', [Edit5.Text]);
    end;
  finally
    FreeAndNil(LI);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Count: Integer;
  LI: TLongInteger;
  t1, t2, i: Integer;
begin
  Memo1.Lines.Clear;
  t1 := TThread.GetTickCount;
  Count := StrToIntDef(Edit7.Text, 10);
  if Count <= 0 then
  begin
    Exit;
  end;
  for i := 0 to StrToIntDef(Edit8.Text, 1) - 1 do
  begin
    LI := GetRandomPrimeNumber(Count, PrimeL);
    try
      Memo1.Lines.Add(LI.ToString10);
    finally
      LI.Free;
    end;
  end;
  t2 := TThread.GetTickCount;
  Memo1.Lines.Add(Format('FFT Time: %d', [t2 - t1]));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(PrimeL);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PrimeL := PrimeNumbersGenerate(25);
end;

end.
