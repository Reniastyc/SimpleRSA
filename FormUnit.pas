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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  PrimeL: TArray<TLongInteger>;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
{
var
  LR1, LR2, LR3: TLongReal;
  s: string;
begin
  Memo1.Lines.Clear;
  LR1 := TLongReal.Create;
  LR2 := TLongReal.Create;
  LR3 := TLongReal.Create;
  try
    LR1.FromStringDot(Edit1.Text);
    LR2.FromStringDot(Edit2.Text);
    s := LR3.CopyVal(LR1).Add(LR2).ToStringDot;
    Memo1.Lines.Add(s);
    s := LR3.CopyVal(LR1).Subtract(LR2).ToStringDot;
    Memo1.Lines.Add(s);
    s := LR3.CopyVal(LR1).Multiply(LR2).ToStringDot;
    Memo1.Lines.Add(s);
    s := LR3.CopyVal(LR1).Divide(LR2).ToStringDot;
    Memo1.Lines.Add(s);
  finally
    FreeAndNil(LR1);
    FreeAndNil(LR2);
    FreeAndNil(LR3);
  end;
end;
}
var
  LI1, LI2, LI3, LI4, LI5: TLongInteger;
  s1, s2, s: string;
  t1, t2: Integer;
begin
  Memo1.Lines.Clear;
  Edit1.Text := Edit1.Text.Replace(' ', '');
  Edit2.Text := Edit2.Text.Replace(' ', '');
  Edit3.Text := Edit3.Text.Replace(' ', '');
  Li1 := TLongInteger.Create;
  Li2 := TLongInteger.Create;
  Li3 := TLongInteger.Create;
  Li4 := TLongInteger.Create;
  Li5 := TLongInteger.Create;
  try
    LI1.FromString10(Edit1.Text);
    LI2.FromString10(Edit2.Text);
    LI3.FromString10(Edit3.Text);
    s1 := LI4.CopyVal(LI1).Add(LI2).ToString10;
    s := Format('%s + %s = %s', [Edit1.Text, Edit2.Text, s1]);
    Memo1.Lines.Add(s);
    s1 := LI4.CopyVal(LI1).Subtract(LI2).ToString10;
    s := Format('%s - %s = %s', [Edit1.Text, Edit2.Text, s1]);
    Memo1.Lines.Add(s);
    t1 := TThread.GetTickCount;
    s1 := LI4.CopyVal(LI1).Multiply(LI2).ToString10;
    s := Format('%s * %s = %s', [Edit1.Text, Edit2.Text, s1]);
    Memo1.Lines.Add(s);
    t2 := TThread.GetTickCount;
    Memo1.Lines.Add(Format('Multiply Time: %d', [t2 - t1]));
    try
      t1 := TThread.GetTickCount;
      s1 := LI4.CopyVal(LI1).DivAndMod(LI2, LI5).ToString10;
      s2 := LI5.ToString10;
      s := Format('%s / %s = %s ...... %s', [Edit1.Text, Edit2.Text, s1, s2]);
      Memo1.Lines.Add(s);
      t2 := TThread.GetTickCount;
      Memo1.Lines.Add(Format('Divide Time: %d', [t2 - t1]));
    except
      on EZeroError do
      begin
        Memo1.Lines.Add('Error Message: Cannot divide ZERO!');
      end;
    end;
    try
      t1 := TThread.GetTickCount;
      s1 := LI4.CopyVal(LI1).PowAndMod(LI2, LI3).ToString10;
      s := Format('%s ^ %s %% %s = %s', [Edit1.Text, Edit2.Text, Edit3.Text, s1]);
      Memo1.Lines.Add(s);
      t2 := TThread.GetTickCount;
      Memo1.Lines.Add(Format('Power Time: %d', [t2 - t1]));
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
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
    FreeAndNil(LI3);
    FreeAndNil(LI4);
    FreeAndNil(LI5);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.AddStrings(PrimeNumbersGenerate16(StrToIntDef(Edit4.Text, 0)));
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
      Memo1.Lines.Add(LI.ToString16);
    finally
      LI.Free;
    end;
  end;
  t2 := TThread.GetTickCount;
  Memo1.Lines.Add(Format('FFT Time: %d', [t2 - t1]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PrimeL := PrimeNumbersGenerate(25);
end;

end.
