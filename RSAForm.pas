unit RSAForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.Memo, FMX.EditBox, FMX.NumberBox, FMX.ScrollBox, FMX.Layouts, FMX.Memo.Types, FMX.ListBox,
  Winapi.Windows,
  RdeEM.Long, RdeEM.Prime;

type
  TRSATest = class(TForm)
    EditP1: TEdit;
    EditP2: TEdit;
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
    AniIndicator: TAniIndicator;
    StyleBook: TStyleBook;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Label5: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    Panel7: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Lang: TLang;
    ComboBoxLang: TComboBox;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Button3: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Panel8: TPanel;
    Label10: TLabel;
    ComboBox1: TComboBox;
    Label11: TLabel;
    ComboBox2: TComboBox;
    Label12: TLabel;
    ComboBox3: TComboBox;
    Label13: TLabel;
    ComboBox4: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ThreadOnTerminate(Sender: TObject);
    procedure ComboBoxLangChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetBuildInfo: string;

var
  RSATest: TRSATest;
  Prime1, Prime2, RSAE0, RSAE, RSAD, RSAN, RSATE, RSACO: TLongInteger;
  PrimeL: TArray<TLongInteger>;
  GetPrime1, GetPrime2, GetRSAE0: Boolean;
  MyThread1, MyThread2, MyThread3: TThread;

implementation

function GetBuildInfo: string;
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
  V1, V2, V3, V4: Word;
begin
  Result := '';
  VerInfoSize:=GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if VerInfoSize = 0 then
  begin
    Exit;
  end;
  GetMem(VerInfo, VerInfoSize);
  if not GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
  begin
    Exit;
  end;
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    V1 := dwFileVersionMS shr 16;
    V2 := dwFileVersionMS and $FFFF;
    V3 := dwFileVersionLS shr 16;
    V4 := dwFileVersionLS and $FFFF;
    Result := inttostr(v1) + '.' + inttostr(v2) + '.' + inttostr(v3) + '.' + inttostr(v4);
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

{$R *.fmx}

procedure TRSATest.Button1Click(Sender: TObject);
var
  FileName: string;
  Texts: TStringList;
begin
  if OpenDialog.Execute then
  begin
    FileName := OpenDialog.FileName;
  end
  else
  begin
    Exit;
  end;
  Texts := TStringList.Create;
  try
    Texts.LoadFromFile(FileName, TEncoding.UTF8);
    if Texts[0] = 'R. de E. M. RSA Public Key' then
    begin
      RSAN.FromString16(Texts[1]);
      RSAE.FromString16(Texts[2]);
      EditE.Text := RSAE.ToString16;
      EditN.Text := RSAN.ToString16;
      Label8.Text := Format('¡¸%d¡¹', [RSAE.Digit]);
      Label4.Text := Format('¡¸%d¡¹', [RSAN.Digit]);
    end;
  finally
    FreeAndNil(Texts);
  end;
end;

procedure TRSATest.Button2Click(Sender: TObject);
var
  FileName: string;
  Texts: TStringList;
begin
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
  end
  else
  begin
    Exit;
  end;
  Texts := TStringList.Create;
  try
    Texts.Add('R. de E. M. RSA Public Key');
    Texts.Add(RSAN.ToString16);
    Texts.Add(RSAE.ToString16);
    Texts.SaveToFile(FileName, TEncoding.UTF8);
  finally
    FreeAndNil(Texts);
  end;
end;

procedure TRSATest.Button3Click(Sender: TObject);
var
  FileName: string;
  Texts: TStringList;
begin
  if OpenDialog.Execute then
  begin
    FileName := OpenDialog.FileName;
  end
  else
  begin
    Exit;
  end;
  Texts := TStringList.Create;
  try
    Texts.LoadFromFile(FileName, TEncoding.UTF8);
    if Texts[0] = 'R. de E. M. RSA Private Key' then
    begin
      RSAN.FromString16(Texts[1]);
      RSAD.FromString16(Texts[2]);
      EditD.Text := RSAD.ToString16;
      EditN.Text := RSAN.ToString16;
      Label9.Text := Format('¡¸%d¡¹', [RSAD.Digit]);
      Label4.Text := Format('¡¸%d¡¹', [RSAN.Digit]);
    end;
  finally
    FreeAndNil(Texts);
  end;
end;

procedure TRSATest.Button4Click(Sender: TObject);
begin
  Back.Enabled := False;
  GetPrime1 := False;
  GetPrime2 := False;
  GetRSAE0 := False;
  AniIndicator.Enabled := True;
  AniIndicator.Visible := True;
  MyThread1 := TThread.CreateAnonymousThread(
    procedure
    begin
      FreeAndNil(Prime1);
      Prime1 := GetRandomPrimeNumber(Trunc(NumberBox1.Value), PrimeL);
      GetPrime1 := True;
    end
  );
  MyThread1.OnTerminate := ThreadOnTerminate;
  MyThread2 := TThread.CreateAnonymousThread(
    procedure
    begin
      FreeAndNil(Prime2);
      Prime2 := GetRandomPrimeNumber(Trunc(NumberBox2.Value), PrimeL);
      GetPrime2 := True;
    end
  );
  MyThread2.OnTerminate := ThreadOnTerminate;
  MyThread3 := TThread.CreateAnonymousThread(
    procedure
    begin
      RSAE0.FromRandom(Trunc(NumberBox3.Value));
      GetRSAE0 := True;
    end
  );
  MyThread3.OnTerminate := ThreadOnTerminate;
  MyThread1.Start;
  MyThread2.Start;
  MyThread3.Start;
end;

procedure TRSATest.Button5Click(Sender: TObject);
begin
  if Memo1.Text = '' then
  begin
    Exit;
  end;
  case ComboBox1.ItemIndex of
    0: RSATE.FromString16(Memo1.Text.Replace(sLineBreak, ''));
    1: RSATE.FromString10(Memo1.Text.Replace(sLineBreak, ''));
    2: RSATE.FromBytes(WideBytesof(Memo1.Text));
  else
    Exit;
  end;
  if RSATE.LessThan(RSAN) and not RSAE.IsZero then
  begin
    RSATE.PowAndMod(RSAE, RSAN);
    case ComboBox2.ItemIndex of
      0: Memo1.Text := RSATE.ToString16;
      1: Memo1.Text := RSATE.ToString10;
      2: Memo1.Text := WideStringOf(RSATE.ToBytes);
    else
      Exit;
    end;
  end;
end;

procedure TRSATest.Button6Click(Sender: TObject);
begin
  if Memo1.Text = '' then
  begin
    Exit;
  end;
  case ComboBox3.ItemIndex of
    0: RSATE.FromString16(Memo1.Text.Replace(sLineBreak, ''));
    1: RSATE.FromString10(Memo1.Text.Replace(sLineBreak, ''));
    2: RSATE.FromBytes(WideBytesof(Memo1.Text));
  else
    Exit;
  end;
  if RSATE.LessThan(RSAN) and not RSAD.IsZero then
  begin
    RSATE.PowAndMod(RSAD, RSAN);
    case ComboBox4.ItemIndex of
      0: Memo1.Text := RSATE.ToString16;
      1: Memo1.Text := RSATE.ToString10;
      2: Memo1.Text := WideStringOf(RSATE.ToBytes);
    else
      Exit;
    end;
  end;
end;

procedure TRSATest.Button7Click(Sender: TObject);
var
  FileName: string;
  Texts: TStringList;
begin
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
  end
  else
  begin
    Exit;
  end;
  Texts := TStringList.Create;
  try
    Texts.Add('R. de E. M. RSA Private Key');
    Texts.Add(RSAN.ToString16);
    Texts.Add(RSAD.ToString16);
    Texts.SaveToFile(FileName, TEncoding.UTF8);
  finally
    FreeAndNil(Texts);
  end;
end;

procedure TRSATest.Button8Click(Sender: TObject);
var
  FileName: string;
  Texts: TStringList;
begin
  if OpenDialog.Execute then
  begin
    FileName := OpenDialog.FileName;
  end
  else
  begin
    Exit;
  end;
  Texts := TStringList.Create;
  try
    Texts.LoadFromFile(FileName, TEncoding.UTF8);
    if Texts[0] = 'R. de E. M. RSA Keys' then
    begin
      RSAN.FromString16(Texts[1]);
      RSAE.FromString16(Texts[2]);
      RSAD.FromString16(Texts[3]);
      EditE.Text := RSAE.ToString16;
      EditD.Text := RSAD.ToString16;
      EditN.Text := RSAN.ToString16;
      Label8.Text := Format('¡¸%d¡¹', [RSAE.Digit]);
      Label9.Text := Format('¡¸%d¡¹', [RSAD.Digit]);
      Label4.Text := Format('¡¸%d¡¹', [RSAN.Digit]);
    end;
  finally
    FreeAndNil(Texts);
  end;
end;

procedure TRSATest.Button9Click(Sender: TObject);
var
  FileName: string;
  Texts: TStringList;
begin
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
  end
  else
  begin
    Exit;
  end;
  Texts := TStringList.Create;
  try
    Texts.Add('R. de E. M. RSA Keys');
    Texts.Add(RSAN.ToString16);
    Texts.Add(RSAE.ToString16);
    Texts.Add(RSAD.ToString16);
    Texts.SaveToFile(FileName, TEncoding.UTF8);
  finally
    FreeAndNil(Texts);
  end;
end;

procedure TRSATest.ComboBoxLangChange(Sender: TObject);
begin
  if ComboBoxLang.ItemIndex = 0 then
  begin
    LoadLangFromStrings(Lang.LangStr['tc']);
  end
  else if ComboBoxLang.ItemIndex = 1 then
  begin
    LoadLangFromStrings(Lang.LangStr['sc']);
  end
  else if ComboBoxLang.ItemIndex = 2 then
  begin
    LoadLangFromStrings(Lang.LangStr['en']);
  end;
end;

procedure TRSATest.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  FreeAndNil(Prime1);
  FreeAndNil(Prime2);
  FreeAndNil(RSAE0);
  FreeAndNil(RSAE);
  FreeAndNil(RSAD);
  FreeAndNil(RSAN);
  FreeAndNil(RSATE);
  for i := 0 to Length(PrimeL) - 1 do
  begin
    FreeAndNil(PrimeL[i]);
  end;
end;

procedure TRSATest.FormCreate(Sender: TObject);
var
  ID: LangID;
begin
  RSATest.Caption := 'R. de E. M. RSA Tool v' + GetBuildInfo;
  Prime1 := TLongInteger.Create;
  Prime2 := TLongInteger.Create;
  RSAE0 := TLongInteger.Create;
  RSAE := TLongInteger.Create;
  RSAD := TLongInteger.Create;
  RSAN := TLongInteger.Create;
  RSATE := TLongInteger.Create;
  PrimeL := PrimeNumbersGenerate(32);
  ID := GetSystemDefaultLangID;
  case ID of
    $1404, $0404, $0C04: 
    begin
      LoadLangFromStrings(Lang.LangStr['tc']);
      ComboBoxLang.ItemIndex := 0;
    end;
    $1004, $0804:  
    begin    
      LoadLangFromStrings(Lang.LangStr['sc']); 
      ComboBoxLang.ItemIndex := 1;
    end
  else
    LoadLangFromStrings(Lang.LangStr['en']);
  end;
end;

procedure TRSATest.ThreadOnTerminate(Sender: TObject);
begin
  if GetPrime1 and GetPrime2 and GetRSAE0 then
  begin
    EditP1.Text := Prime1.ToString16;
    EditP2.Text := Prime2.ToString16;
    EditE0.Text := RSAE0.ToString16;
    RSAParameter(Prime1, Prime2, RSAE0, RSAE, RSAD, RSAN);
    EditE.Text := RSAE.ToString16;
    EditD.Text := RSAD.ToString16;
    EditN.Text := RSAN.ToString16;
    Label8.Text := Format('¡¸%d¡¹', [RSAE.Digit]);
    Label9.Text := Format('¡¸%d¡¹', [RSAD.Digit]);
    Label4.Text := Format('¡¸%d¡¹', [RSAN.Digit]);
    AniIndicator.Enabled := False;
    AniIndicator.Visible := False;
    Back.Enabled := True;
  end;
end;

end.
