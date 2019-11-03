unit RSAForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.Memo, FMX.EditBox, FMX.NumberBox,
  RdeEM.Long, RdeEM.Prime, FMX.ScrollBox, FMX.Layouts;

type
  TRSATest = class(TForm)
    Button1: TButton;
    EditP1: TEdit;
    EditP2: TEdit;
    Button2: TButton;
    Button3: TButton;
    EditE0: TEdit;
    Button4: TButton;
    EditN: TEdit;
    EditE: TEdit;
    EditD: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Button5: TButton;
    Button6: TButton;
    NumberBox1: TNumberBox;
    NumberBox2: TNumberBox;
    NumberBox3: TNumberBox;
    Back: TScaledLayout;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RSATest: TRSATest;
  Prime1, Prime2, RSAE0, RSAE, RSAD, RSAN, RSATE, RSACO: TLongInteger;
  PrimeL: TObjectList<TLongInteger>;

implementation

{$R *.fmx}

procedure TRSATest.Button1Click(Sender: TObject);
begin
  FreeAndNil(Prime1);
  Prime1 := GetRandomPrimeNumber(Trunc(NumberBox1.Value), PrimeL);
  EditP1.Text := Prime1.ToString16;
end;

procedure TRSATest.Button2Click(Sender: TObject);
begin
  FreeAndNil(Prime2);
  Prime2 := GetRandomPrimeNumber(Trunc(NumberBox2.Value), PrimeL);
  EditP2.Text := Prime2.ToString16;
end;

procedure TRSATest.Button3Click(Sender: TObject);
begin
  RSAE0.FromRandomOdd(Trunc(NumberBox3.Value));
  EditE0.Text := RSAE0.ToString16;
end;

procedure TRSATest.Button4Click(Sender: TObject);
begin
  if EditP1.Text = '' then
  begin
    FreeAndNil(Prime1);
    Prime1 := GetRandomPrimeNumber(Trunc(NumberBox1.Value), PrimeL);
    EditP1.Text := Prime1.ToString16;
  end;
  if EditP2.Text = '' then
  begin
    FreeAndNil(Prime2);
    Prime2 := GetRandomPrimeNumber(Trunc(NumberBox2.Value), PrimeL);
    EditP2.Text := Prime2.ToString16;
  end;
  if EditE0.Text = '' then
  begin
    RSAE0.FromRandom(Trunc(NumberBox3.Value));
    EditE0.Text := RSAE0.ToString16;
  end;
  RSAParameter(Prime1, Prime2, RSAE0, RSAE, RSAD, RSAN);
  EditE.Text := RSAE.ToString16;
  EditD.Text := RSAD.ToString16;
  EditN.Text := RSAN.ToString16;
  Label4.Text := Format('Ä£Î»Êý£º¡¸%d¡¹', [RSAN.Digit]);
end;

procedure TRSATest.Button5Click(Sender: TObject);
var
  bs: TBytes;
begin
  if Memo1.Text = '' then
  begin
    Exit;
  end;
  bs := WideBytesof(Memo1.Text);
  RSATE.FromBytes(bs);
  if RSATE.LessThan(RSAN) then
  begin
    RSATE.PowerMod(RSAE, RSAN);
  end;
  Memo1.Text := RSATE.ToString16;
end;

procedure TRSATest.Button6Click(Sender: TObject);
var
  s: string;
begin
  RSATE.FromString16(Memo1.Text);
  if RSATE.LessThan(RSAN) then
  begin
    RSATE.PowerMod(RSAD, RSAN);
  end;
  s := WideStringOf(RSATE.ToBytes);
  Memo1.Text := s;
end;

procedure TRSATest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Prime1);
  FreeAndNil(Prime2);
  FreeAndNil(RSAE0);
  FreeAndNil(RSAE);
  FreeAndNil(RSAD);
  FreeAndNil(RSAN);
  FreeAndNil(RSATE);
  FreeAndNil(PrimeL);
end;

procedure TRSATest.FormCreate(Sender: TObject);
begin
  Prime1 := TLongInteger.Create;
  Prime2 := TLongInteger.Create;
  RSAE0 := TLongInteger.Create;
  RSAE := TLongInteger.Create;
  RSAD := TLongInteger.Create;
  RSAN := TLongInteger.Create;
  RSATE := TLongInteger.Create;
  PrimeL := PrimeNumbersGenerate(25);
end;

end.
