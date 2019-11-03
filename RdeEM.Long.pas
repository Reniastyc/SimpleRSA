{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{                    RdeEM Long                         }
{                     大数单元                          }
{                     ver 1.10                          }
{                                                       }
{    Copyright(c) 2018-2019 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit RdeEM.Long;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.Generics.Collections, System.Math;

type
  EZeroError = class(Exception);
  EPowerError = class(Exception);

  TLongInteger = class;

  TLongInteger = class
  private
    FSymb: Boolean; // 符号位，True代表正数。
    FNumL: TList<Boolean>;
    function SameSignAdd(Par: TLongInteger): TLongInteger;
    function SameSignSub(Par: TLongInteger): TLongInteger;
    function MultiplyOri(Par: TLongInteger): TLongInteger;
  public
    constructor Create; overload;
    constructor Create(LI: TLongInteger); overload;
    constructor Create(N: Int64); overload;
    destructor Destroy; override;
    // 判斷
    function Equal(Par: TLongInteger): Boolean;
    function GreaterThan(Par: TLongInteger): Boolean;
    function LessThan(Par: TLongInteger): Boolean;
    function IsZero: Boolean;
    function IsEven: Boolean;
    function IsOdd: Boolean;
    // 位數
    function Digit: Integer;
    // 轉換
    function ToString2: string;
    function ToString10: string;
    function ToString16: string;
    function ToBytes: TBytes;
    function FromString2(Str: string): Boolean;
    function FromString10(Str: string): Boolean;
    function FromString16(Str: string): Boolean;
    function FromInteger(N: Byte): TLongInteger; overload;
    function FromInteger(N: Cardinal): TLongInteger; overload;
    function FromInteger(N: Integer): TLongInteger; overload;
    function FromInteger(N: Int64): TLongInteger; overload;
    function FromBytes(B: TBytes): TLongInteger;
    function FromRandom(Digits: Integer): TLongInteger;
    function FromRandomOdd(Digits: Integer): TLongInteger;
    function FromRandomEven(Digits: Integer): TLongInteger;
    // 複製
    function CopyVal(Par: TLongInteger): TLongInteger;
    // 歸零、絕對值、正負
    function Zero: TLongInteger;
    function AbsoluteVal: TLongInteger;
    function Normalize: TLongInteger;
    function Positive: TLongInteger;
    function Negative: TLongInteger;
    // 運算
    function Add(Par: TLongInteger): TLongInteger;
    function Subtract(Par: TLongInteger): TLongInteger;
    function Multiply(Par: TLongInteger): TLongInteger;
    function Divide(Par: TLongInteger): TLongInteger;
    function Modulus(Par: TLongInteger): TLongInteger;
    function Power(Par: TLongInteger): TLongInteger;
    function PowerMod(PowPar, ModPar: TLongInteger): TLongInteger;
    function DivAndMod(Par: TLongInteger; var ModVal: TLongInteger): TLongInteger;
    function DivAndMod10(var ModVal: TLongInteger): TLongInteger;
    function Increase(Step: Cardinal = 1): TLongInteger;
    function Decrease(Step: Cardinal = 1): TLongInteger;
    function LogicalXor(Par: TLongInteger): TLongInteger;
    function LogicalOr(Par: TLongInteger): TLongInteger;
    function LogicalAnd(Par: TLongInteger): TLongInteger;
    function LogicalNot: TLongInteger;
    function ShiftR(Shift: Integer): TLongInteger;
    function ShiftL(Shift: Integer): TLongInteger;
    // 值
    function GetNumber(Index: Integer): Boolean;
  end;

  THexInt = record
  private
    FHex: string;
    function GetHexValue: string;
    function GetBinValue: string;
    function GetDecValue: string;
    procedure SetHexValue(const Value: string);
    procedure SetBinValue(const Value: string);
    procedure SetDecValue(const Value: string);
  public
    class function Create(const Str: string): THexInt; static;
    // 正負運算
    class operator Positive(const Hex1: THexInt): THexInt;
    class operator Negative(const Hex1: THexInt): THexInt; 
    // 比較運算
    class operator Equal(const Hex1, Hex2: THexInt): Boolean;
    class operator LessThan(const Hex1, Hex2: THexInt): Boolean;
    class operator GreaterThan(const Hex1, Hex2: THexInt): Boolean;
    class operator NotEqual(const Hex1, Hex2: THexInt): Boolean;
    class operator LessThanOrEqual(const Hex1, Hex2: THexInt): Boolean;
    class operator GreaterThanOrEqual(const Hex1, Hex2: THexInt): Boolean;
    // 四則運算
    class operator Add(const Hex1, Hex2: THexInt): THexInt;
    class operator Subtract(const Hex1, Hex2: THexInt): THexInt;
    class operator Multiply(const Hex1, Hex2: THexInt): THexInt;
    class operator Divide(const Hex1, Hex2: THexInt): THexInt;
    class operator IntDivide(const Hex1, Hex2: THexInt): THexInt;
    class operator Modulus(const Hex1, Hex2: THexInt): THexInt;
    // 其他功能
    function Power(Hex1: THexInt): THexInt;
    function PowerMod(Hex1, Hex2: THexInt): THexInt;
    // 屬性
    property BinValue: string          read GetBinValue         write SetBinValue;
    property DecValue: string          read GetDecValue         write SetDecValue;
    property HexValue: string          read GetHexValue         write SetHexValue;
  end;

  function GetHexChr(N: Byte): Char;
  function GetHexVal(C: Char): Byte;
  function GetDecChr(N: Byte): Char;
  function GetDecVal(C: Char): Byte;

implementation

function GetHexChr(N: Byte): Char;
begin
  case N of
    0: Exit('0');
    1: Exit('1');
    2: Exit('2');
    3: Exit('3');
    4: Exit('4');
    5: Exit('5');
    6: Exit('6');
    7: Exit('7');
    8: Exit('8');
    9: Exit('9');
    10: Exit('A');
    11: Exit('B');
    12: Exit('C');
    13: Exit('D');
    14: Exit('E');
    15: Exit('F');
  else
    Exit(#0);
  end;
end;

function GetHexVal(C: Char): Byte;
begin
  case C of
    '0': Exit(0);
    '1': Exit(1);
    '2': Exit(2);
    '3': Exit(3);
    '4': Exit(4);
    '5': Exit(5);
    '6': Exit(6);
    '7': Exit(7);
    '8': Exit(8);
    '9': Exit(9);
    'A': Exit(10);
    'B': Exit(11);
    'C': Exit(12);
    'D': Exit(13);
    'E': Exit(14);
    'F': Exit(15);
  else
    Exit(255);
  end;
end;

function GetDecChr(N: Byte): Char;
begin
  case N of
    0: Exit('0');
    1: Exit('1');
    2: Exit('2');
    3: Exit('3');
    4: Exit('4');
    5: Exit('5');
    6: Exit('6');
    7: Exit('7');
    8: Exit('8');
    9: Exit('9');
  else
    Exit(#0);
  end;
end;

function GetDecVal(C: Char): Byte;
begin
  case C of
    '0': Exit(0);
    '1': Exit(1);
    '2': Exit(2);
    '3': Exit(3);
    '4': Exit(4);
    '5': Exit(5);
    '6': Exit(6);
    '7': Exit(7);
    '8': Exit(8);
    '9': Exit(9);
  else
    Exit(255);
  end;
end;

{ TLongInteger }

function TLongInteger.AbsoluteVal: TLongInteger;
begin
  FSymb := True;
  Exit(Self);
end;

function TLongInteger.Add(Par: TLongInteger): TLongInteger;
begin
  if IsZero then
  begin
    CopyVal(Par);
  end
  else if Par.IsZero then
  begin
    Exit(Self);
  end
  else if FSymb = Par.FSymb then
  begin
    SameSignAdd(Par);
  end
  else
  begin
    FSymb := not FSymb;
    SameSignSub(Par);
    FSymb := not FSymb;
  end;
  Exit(Self);
end;

function TLongInteger.CopyVal(Par: TLongInteger): TLongInteger;
var
  i: Integer;
begin
  FSymb := Par.FSymb;
  FNumL.Clear;
  for i := 0 to Par.FNumL.Count - 1 do
  begin
    FNumL.Add(Par.FNumL[i]);
  end;
  Exit(Self);
end;

constructor TLongInteger.Create;
begin
  FNumL := TList<Boolean>.Create;
  FNumL.Add(False);
  FSymb := True;
end;

constructor TLongInteger.Create(LI: TLongInteger);
begin
  FNumL := TList<Boolean>.Create;
  FSymb := LI.FSymb;
  FNumL.AddRange(LI.FNumL);
end;

constructor TLongInteger.Create(N: Int64);
begin
  FNumL := TList<Boolean>.Create;
  if N >= 0 then
  begin
    FSymb := True;
  end
  else
  begin
    FSymb := False;
  end;
  N := Abs(N);
  repeat
    if N mod 2 = 1 then
    begin
      FNumL.Add(True);
    end
    else
    begin
      FNumL.Add(False);
    end;
    N := N div 2;
  until N = 0;
end;

function TLongInteger.Decrease(Step: Cardinal): TLongInteger;
var
  LI: TLongInteger;
begin
  LI := TLongInteger.Create(Step);
  try
    Subtract(LI);
  finally
    FreeAndNil(LI);
  end;
  Exit(Self);
end;

destructor TLongInteger.Destroy;
begin
  FreeAndNil(FNumL);
  inherited;
end;

function TLongInteger.Digit: Integer;
begin
  Exit(FNumL.Count);
end;

function TLongInteger.DivAndMod(Par: TLongInteger; var ModVal: TLongInteger): TLongInteger;
var
  ALI, BLI: TLongInteger;
  n, i: Integer;
  S: Boolean;
begin
  Normalize;
  Par.Normalize;
  if IsZero then
  begin
    Zero;
    Exit(Self);
  end;
  if Par.IsZero then
  begin
    raise EZeroError.Create('Error Message: Cannot divide ZERO!!');
  end;
  ALI := TLongInteger.Create;
  BLI := TLongInteger.Create;
  try
    S := not (FSymb xor Par.FSymb);
    ALI.CopyVal(Par);
    ALI.FSymb := True;
    FSymb := True;
    if LessThan(ALI) then
    begin
      Zero;
      Exit(Self);
    end;
    n := FNumL.Count - Par.FNumL.Count;
    BLI.FNumL.Count := n + 1;
    ALI.ShiftL(n);
    for i := n downto 0 do
    begin
      if not LessThan(ALI) then
      begin
        SameSignSub(ALI);
        BLI.FNumL[i] := True;
      end
      else
      begin
        BLI.FNumL[i] := False;
      end;
      ALI.ShiftR(1);
    end;
    BLI.Normalize;
    ModVal.CopyVal(BLI);
    FSymb := S;
  finally
    FreeAndNil(ALI);
    FreeAndNil(BLI);
  end;
  Exit(Self);
end;

function TLongInteger.DivAndMod10(var ModVal: TLongInteger): TLongInteger;
var
  ALI, BLI, Par: TLongInteger;
  n, i: Integer;
  S: Boolean;
begin
  Normalize;
  if IsZero then
  begin
    Zero;
    Exit(Self);
  end;
  ALI := TLongInteger.Create;
  BLI := TLongInteger.Create;
  Par := TLongInteger.Create;
  try
    Par.FNumL.Add(True);
    Par.FNumL.Add(False);
    Par.FNumL.Add(True);
    S := not (FSymb xor Par.FSymb);
    ALI.CopyVal(Par);
    ALI.FSymb := True;
    FSymb := True;
    if LessThan(ALI) then
    begin
      ModVal.CopyVal(Self);
      Zero;
      Exit(Self);
    end;
    n := FNumL.Count - Par.FNumL.Count;
    BLI.FNumL.Count := n + 1;
    ALI.ShiftL(n);
    for i := n downto 0 do
    begin
      if not LessThan(ALI) then
      begin
        SameSignSub(ALI);
        BLI.FNumL[i] := True;
      end
      else
      begin
        BLI.FNumL[i] := False;
      end;
      ALI.ShiftR(1);
    end;
    ModVal.CopyVal(Self);
    BLI.Normalize;
    CopyVal(BLI);
    FSymb := S;
  finally
    FreeAndNil(ALI);
    FreeAndNil(BLI);
    FreeAndNil(Par);
  end;
  Exit(Self);
end;

function TLongInteger.Divide(Par: TLongInteger): TLongInteger;
var
  ALI, BLI: TLongInteger;
  n, i: Integer;
  S: Boolean;
begin
  Normalize;
  Par.Normalize;
  if IsZero then
  begin
    Zero;
    Exit(Self);
  end;
  if Par.IsZero then
  begin
    raise EZeroError.Create('Error Message: Cannot divide ZERO!!');
  end;
  ALI := TLongInteger.Create;
  BLI := TLongInteger.Create;
  try
    S := not (FSymb xor Par.FSymb);
    ALI.CopyVal(Par);
    ALI.FSymb := True;
    FSymb := True;
    if LessThan(ALI) then
    begin
      Zero;
      Exit(Self);
    end;
    n := FNumL.Count - Par.FNumL.Count;
    BLI.FNumL.Count := n + 1;
    ALI.ShiftL(n);
    for i := n downto 0 do
    begin
      if not LessThan(ALI) then
      begin
        SameSignSub(ALI);
        BLI.FNumL[i] := True;
      end
      else
      begin
        BLI.FNumL[i] := False;
      end;
      ALI.ShiftR(1);
    end;
    BLI.Normalize;
    CopyVal(BLI);
    FSymb := S;
  finally
    FreeAndNil(ALI);
    FreeAndNil(BLI);
  end;
  Exit(Self);
end;

function TLongInteger.Equal(Par: TLongInteger): Boolean;
var
  i: Integer;
begin
  Normalize;
  Par.Normalize;
  if FSymb <> Par.FSymb then
  begin
    Exit(False);
  end;
  if FNumL.Count <> Par.FNumL.Count then
  begin
    Exit(False);
  end;
  for i := 0 to FNumL.Count - 1 do
  begin
    if FNumL[i] <> Par.FNumL[i] then
    begin
      Exit(False);
    end;
  end;
  Exit(True);
end;

function TLongInteger.FromInteger(N: Byte): TLongInteger;
begin
  FSymb := True;
  FNumL.Clear;
  N := Abs(N);
  repeat
    if N mod 2 = 1 then
    begin
      FNumL.Add(True);
    end
    else
    begin
      FNumL.Add(False);
    end;
    N := N div 2;
  until N = 0;
  Exit(Self);
end;

function TLongInteger.FromInteger(N: Cardinal): TLongInteger;
begin
  FSymb := True;
  FNumL.Clear;
  N := Abs(N);
  repeat
    if N mod 2 = 1 then
    begin
      FNumL.Add(True);
    end
    else
    begin
      FNumL.Add(False);
    end;
    N := N div 2;
  until N = 0;
  Exit(Self);
end;

function TLongInteger.FromInteger(N: Integer): TLongInteger;
begin
  if N >= 0 then
  begin
    FSymb := True;
  end
  else
  begin
    FSymb := False;
  end;
  FNumL.Clear;
  N := Abs(N);
  repeat
    if N mod 2 = 1 then
    begin
      FNumL.Add(True);
    end
    else
    begin
      FNumL.Add(False);
    end;
    N := N div 2;
  until N = 0;
  Exit(Self);
end;

function TLongInteger.FromBytes(B: TBytes): TLongInteger;
var
  n: Byte;
  i, j, l: Integer;
  LI: TLongInteger;
begin
  LI := TLongInteger.Create;
  try
    FSymb := True;
    FNumL.Clear;
    l := Length(B);
    for i := 0 to l - 1 do
    begin
      n := B[i];
      LI.FromInteger(n);
      FNumL.AddRange(LI.FNumL);
      for j := LI.FNumL.Count to 7 do
      begin
        FNumL.Add(False);
      end;
    end;
    FNumL.Add(True); // 确保不会以零为首
  finally
    FreeAndNil(LI);
  end;
  Exit(Self);
end;

function TLongInteger.FromInteger(N: Int64): TLongInteger;
begin
  if N >= 0 then
  begin
    FSymb := True;
  end
  else
  begin
    FSymb := False;
  end;
  FNumL.Clear;
  N := Abs(N);
  repeat
    if N mod 2 = 1 then
    begin
      FNumL.Add(True);
    end
    else
    begin
      FNumL.Add(False);
    end;
    N := N div 2;
  until N = 0;
  Exit(Self);
end;

function TLongInteger.FromRandom(Digits: Integer): TLongInteger;
var
  i: Integer;
begin
  if Digits <= 0 then
  begin
    Zero;
    Exit(Self);
  end;
  FSymb := True;
  FNumL.Clear;
  for i := 0 to Digits - 2 do
  begin
    Randomize;
    if Random < 0.5 then
    begin
      FNumL.Add(True);
    end
    else
    begin
      FNumL.Add(False);
    end;
  end;
  FNumL.Add(True);
  Exit(Self);
end;

function TLongInteger.FromRandomEven(Digits: Integer): TLongInteger;
var
  i: Integer;
begin
  if Digits <= 0 then
  begin
    Zero;
    Exit(Self);
  end;
  FSymb := True;
  FNumL.Clear;
  FNumL.Add(False);
  for i := 1 to Digits - 2 do
  begin
    Randomize;
    if Random < 0.5 then
    begin
      FNumL.Add(True);
    end
    else
    begin
      FNumL.Add(False);
    end;
  end;
  FNumL.Add(True);
  Exit(Self);
end;

function TLongInteger.FromRandomOdd(Digits: Integer): TLongInteger;
var
  i: Integer;
begin
  if Digits <= 0 then
  begin
    Zero;
    Exit(Self);
  end;
  FSymb := True;
  FNumL.Clear;
  FNumL.Add(True);
  for i := 1 to Digits - 2 do
  begin
    Randomize;
    if Random < 0.5 then
    begin
      FNumL.Add(True);
    end
    else
    begin
      FNumL.Add(False);
    end;
  end;
  FNumL.Add(True);
  Exit(Self);
end;

function TLongInteger.FromString10(Str: string): Boolean;
var
  i, l: Integer;
  v: Byte;
  LI, Par: TLongInteger;
  S: Boolean;
begin
  if Str = '' then
  begin
    Exit(False);
  end;
  if Str.Chars[0] = '-' then
  begin
    S := False;
    Str := Str.Substring(1);
  end
  else
  begin
    S := True;
  end;
  l := Str.Length;
  if l <= 0 then
  begin
    Exit(False);
  end;
  for i := 0 to l - 1 do if GetDecVal(Str.Chars[i]) = 255 then
  begin
    Exit(False);
  end;
  Zero;
  LI := TLongInteger.Create;
  Par := TLongInteger.Create;
  try
    Par.FNumL.Add(True);
    Par.FNumL.Add(False);
    Par.FNumL.Add(True);
    for i := 0 to l - 1 do
    begin
      v := GetDecVal(Str.Chars[i]);
      LI.FromInteger(v);
      MultiplyOri(Par).Add(LI);
    end;
  finally
    FreeAndNil(LI);
    FreeAndNil(Par);
  end;
  FSymb := S;
  Normalize;
  Exit(True);
end;

function TLongInteger.FromString16(Str: string): Boolean;
var
  i, l: Integer;
  v: Byte;
begin
  if Str = '' then
  begin
    Exit(False);
  end;
  Str := UpperCase(Str);
  l := Str.Length;
  if Str.Chars[0] = '-' then
  begin
    if l <= 1 then
    begin
      Exit(False);
    end;
    for i := 1 to l - 1 do if GetHexVal(Str.Chars[i]) = 255 then
    begin
      Exit(False);
    end;
    FSymb := False;
    FNumL.Clear;
    for i := l - 1 downto 1 do
    begin
      v := GetHexVal(Str.Chars[i]);
      if v mod 2 = 1 then
      begin
        FNumL.Add(True);
      end
      else
      begin
        FNumL.Add(False);
      end;
      v := v div 2;
      if v mod 2 = 1 then
      begin
        FNumL.Add(True);
      end
      else
      begin
        FNumL.Add(False);
      end;
      v := v div 2;
      if v mod 2 = 1 then
      begin
        FNumL.Add(True);
      end
      else
      begin
        FNumL.Add(False);
      end;
      v := v div 2;
      if v mod 2 = 1 then
      begin
        FNumL.Add(True);
      end
      else
      begin
        FNumL.Add(False);
      end;
    end;
  end
  else
  begin
    if l <= 0 then
    begin
      Exit(False);
    end;
    for i := 0 to l - 1 do
    begin
      if GetHexVal(Str.Chars[i]) = 255 then
      begin
        Exit(False);
      end;
    end;
    FSymb := True;
    FNumL.Clear;
    for i := l - 1 downto 0 do
    begin
      v := GetHexVal(Str.Chars[i]);
      if v mod 2 = 1 then
      begin
        FNumL.Add(True);
      end
      else
      begin
        FNumL.Add(False);
      end;
      v := v div 2;
      if v mod 2 = 1 then
      begin
        FNumL.Add(True);
      end
      else
      begin
        FNumL.Add(False);
      end;
      v := v div 2;
      if v mod 2 = 1 then
      begin
        FNumL.Add(True);
      end
      else
      begin
        FNumL.Add(False);
      end;
      v := v div 2;
      if v mod 2 = 1 then
      begin
        FNumL.Add(True);
      end
      else
      begin
        FNumL.Add(False);
      end;
    end;
  end;
  Normalize;
  Exit(True);
end;

function TLongInteger.FromString2(Str: string): Boolean;
var
  i, l: Integer;
begin
  if Str = '' then
  begin
    Exit(False);
  end;
  l := Str.Length;
  if Str.Chars[0] = '-' then
  begin
    if l <= 1 then
    begin
      Exit(False);
    end;
    for i := 1 to l - 1 do
    begin
      if not ((Str.Chars[i] = '0') or (Str.Chars[i] = '1')) then
      begin
        Exit(False);
      end;
    end;
    FSymb := False;
    FNumL.Clear;
    for i := l - 1 downto 1 do
    begin
      if Str.Chars[i] = '0' then
      begin
        FNumL.Add(False);
      end
      else if Str.Chars[i] = '1' then
      begin
        FNumL.Add(True);
      end;
    end;
  end
  else
  begin
    if l <= 0 then
    begin
      Exit(False);
    end;
    for i := 0 to l - 1 do
    begin
      if not ((Str.Chars[i] = '0') or (Str.Chars[i] = '1')) then
      begin
        Exit(False);
      end;
    end;
    FSymb := True;
    FNumL.Clear;
    for i := l - 1 downto 0 do
    begin
      if Str.Chars[i] = '0' then
      begin
        FNumL.Add(False);
      end
      else if Str.Chars[i] = '1' then
      begin
        FNumL.Add(True);
      end;
    end;
  end;
  Normalize;
  Exit(True);
end;

function TLongInteger.GetNumber(Index: Integer): Boolean;
begin
  Exit(FNumL[Index]);
end;

function TLongInteger.GreaterThan(Par: TLongInteger): Boolean;
var
  i: Integer;
begin
  Normalize;
  Par.Normalize;
  if FSymb and not Par.FSymb then
  begin
    Exit(True);
  end;
  if not FSymb and Par.FSymb then
  begin
    Exit(False);
  end;
  if FNumL.Count > Par.FNumL.Count then
  begin
    Exit(True);
  end;
  if FNumL.Count < Par.FNumL.Count then
  begin
    Exit(False);
  end;
  for i := FNumL.Count - 1 downto 0 do
  begin
    if FNumL[i] and not Par.FNumL[i] then
    begin
      Exit(True);
    end;
    if not FNumL[i] and Par.FNumL[i] then
    begin
      Exit(False);
    end;
  end;
  Exit(False);
end;

function TLongInteger.Increase(Step: Cardinal): TLongInteger;
var
  LI: TLongInteger;
begin
  LI := TLongInteger.Create(Step);
  try
    Add(LI);
  finally
    FreeAndNil(LI);
  end;
  Exit(Self);
end;

function TLongInteger.IsEven: Boolean;
begin
  Exit((FNumL.Count > 0) and (FNumL[0] = False));
end;

function TLongInteger.IsOdd: Boolean;
begin
  Exit((FNumL.Count > 0) and (FNumL[0] = True));
end;

function TLongInteger.IsZero: Boolean;
begin
  Normalize;
  if (FSymb = True) and (FNumL.Count = 1) and (FNumL[0] = False) then
  begin
    Exit(True);
  end
  else
  begin
    Exit(False);
  end;
end;

function TLongInteger.LessThan(Par: TLongInteger): Boolean;
var
  i: Integer;
begin
  Normalize;
  Par.Normalize;
  if FSymb and not Par.FSymb then
  begin
    Exit(False);
  end;
  if not FSymb and Par.FSymb then
  begin
    Exit(True);
  end;
  if FNumL.Count > Par.FNumL.Count then
  begin
    Exit(False);
  end;
  if FNumL.Count < Par.FNumL.Count then
  begin
    Exit(True);
  end;
  for i := FNumL.Count - 1 downto 0 do
  begin
    if FNumL[i] and not Par.FNumL[i] then
    begin
      Exit(False);
    end;
    if not FNumL[i] and Par.FNumL[i] then
    begin
      Exit(True);
    end;
  end;
  Exit(False);
end;

function TLongInteger.LogicalAnd(Par: TLongInteger): TLongInteger;
var
  i, n: Integer;
begin
  Normalize;
  Par.Normalize;
  if FNumL.Count < Par.FNumL.Count then
  begin
    n := FNumL.Count;
  end
  else
  begin
    n := Par.FNumL.Count;
  end;
  for i := 0 to n - 1 do
  begin
    FNumL[i] := FNumL[i] and Par.FNumL[i];
  end;
  FNumL.Count := n;
  Normalize;
  Exit(Self);
end;

function TLongInteger.LogicalNot: TLongInteger;
var
  i: Integer;
begin
  for i := 0 to FNumL.Count - 1 do
  begin
    FNumL[i] := not FNumL[i];
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.LogicalOr(Par: TLongInteger): TLongInteger;
var
  i, n: Integer;
begin
  Normalize;
  Par.Normalize;
  if FNumL.Count > Par.FNumL.Count then
  begin
    n := FNumL.Count;
  end
  else
  begin
    n := Par.FNumL.Count;
  end;
  for i := 0 to n - 1 do if i < Par.FNumL.Count then
  begin
    if i < FNumL.Count then
    begin
      FNumL[i] := FNumL[i] or Par.FNumL[i];
    end
    else
    begin
      FNumL.Add(Par.FNumL[i]);
    end;
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.LogicalXor(Par: TLongInteger): TLongInteger;
var
  i, n: Integer;
begin
  Normalize;
  Par.Normalize;
  if FNumL.Count > Par.FNumL.Count then
  begin
    n := FNumL.Count;
  end
  else
  begin
    n := Par.FNumL.Count;
  end;
  for i := 0 to n - 1 do if i < Par.FNumL.Count then
  begin
    if i < FNumL.Count then
    begin
      FNumL[i] := FNumL[i] xor Par.FNumL[i];
    end
    else
    begin
      FNumL.Add(Par.FNumL[i]);
    end;
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.Modulus(Par: TLongInteger): TLongInteger;
var
  ALI: TLongInteger;
  n, i: Integer;
  S: Boolean;
begin
  Normalize;
  Par.Normalize;
  if IsZero then
  begin
    Zero;
    Exit(Self);
  end;
  if Par.IsZero then
  begin
    raise EZeroError.Create('Error Message: Cannot divide ZERO!!');
    Exit(nil);
  end;
  ALI := TLongInteger.Create;
  try
    S := FSymb;
    ALI.CopyVal(Par);
    ALI.FSymb := True;
    FSymb := True;
    if LessThan(ALI) then
    begin
      FSymb := S;
      Exit(Self);
    end;
    n := FNumL.Count - Par.FNumL.Count;
    ALI.ShiftL(n);
    for i := n downto 0 do
    begin
      if not LessThan(ALI) then
      begin
        SameSignSub(ALI);
      end;
      ALI.ShiftR(1);
    end;
    Normalize;
    FSymb := S;
  finally
    FreeAndNil(ALI);
  end;
  Exit(Self);
end;

function TLongInteger.Multiply(Par: TLongInteger): TLongInteger;
var
  ALI, BLI, CLI, DLI, XLI, YLI, ZLI: TLongInteger;
  i, n: Integer;
  S: Boolean;
begin
  Normalize;
  Par.Normalize;
  if IsZero or Par.IsZero then
  begin
    Zero;
    Exit(Self);
  end;
  ALI := TLongInteger.Create;
  BLI := TLongInteger.Create;
  CLI := TLongInteger.Create;
  DLI := TLongInteger.Create;
  XLI := TLongInteger.Create;
  YLI := TLongInteger.Create;
  ZLI := TLongInteger.Create;
  try
    S := not (FSymb xor Par.FSymb);
    ALI.FNumL.Clear;
    BLI.FNumL.Clear;
    CLI.FNumL.Clear;
    DLI.FNumL.Clear;
    if FNumL.Count < Par.FNumL.Count then
    begin
      n := (Par.FNumL.Count + 1) div 2;
    end
    else
    begin
      n := (FNumL.Count + 1) div 2;
    end;
    for i := 0 to n - 1 do
    begin
      if i < FNumL.Count then
      begin
        BLI.FNumL.Add(FNumL[i]);
      end
      else
      begin
        BLI.FNumL.Add(False);
      end;
      if i < Par.FNumL.Count then
      begin
        DLI.FNumL.Add(Par.FNumL[i]);
      end
      else
      begin
        DLI.FNumL.Add(False);
      end;
    end;
    for i := n to 2 * n do
    begin
      if i < FNumL.Count then
      begin
        ALI.FNumL.Add(FNumL[i]);
      end
      else
      begin
        ALI.FNumL.Add(False);
      end;
      if i < Par.FNumL.Count then
      begin
        CLI.FNumL.Add(Par.FNumL[i]);
      end
      else
      begin
        CLI.FNumL.Add(False);
      end;
    end;
    XLI.CopyVal(ALI).MultiplyOri(CLI);
    YLI.CopyVal(BLI).MultiplyOri(DLI);
    ZLI.CopyVal(ALI.Subtract(BLI)).MultiplyOri(DLI.Subtract(CLI));
    ZLI.Add(XLI).Add(YLI);
    CopyVal(YLI);
    Add(ZLI.ShiftL(n)).Add(XLI.ShiftL(2 * n));
  finally
    FreeAndNil(ALI);
    FreeAndNil(BLI);
    FreeAndNil(CLI);
    FreeAndNil(DLI);
    FreeAndNil(XLI);
    FreeAndNil(YLI);
    FreeAndNil(ZLI);
  end;
  FSymb := S;
  Normalize;
  Exit(Self);
end;

function TLongInteger.MultiplyOri(Par: TLongInteger): TLongInteger;
var
  ALI, BLI: TLongInteger;
  n, i: Integer;
  S: Boolean;
begin
  Normalize;
  Par.Normalize;
  if IsZero or Par.IsZero then
  begin
    Zero;
    Exit(Self);
  end;
  ALI := TLongInteger.Create;
  BLI := TLongInteger.Create;
  try
    S := not (FSymb xor Par.FSymb);
    ALI.CopyVal(Self);
    ALI.FSymb := True;
    BLI.CopyVal(Par);
    BLI.FSymb := True;
    Zero;
    n := 0;
    for i := 0 to BLI.FNumL.Count - 1 do
    begin  
      if Par.FNumL[i] then
      begin
        ALI.ShiftL(i - n);
        n := i;
        SameSignAdd(ALI);
      end;
    end;
  finally
    FreeAndNil(ALI);
    FreeAndNil(BLI);
  end;       
  FSymb := S;
  Normalize;
  Exit(Self);
end;

function TLongInteger.Negative: TLongInteger;
begin
  FSymb := not FSymb;
  Normalize;
  Exit(Self);
end;

function TLongInteger.Normalize: TLongInteger;
begin
  while (FNumL.Count > 1) and (FNumL.Last = False) do
  begin
    FNumL.Delete(FNumL.Count - 1);
  end;
  if (FNumL.Count = 1) and (FNumL[0] = False) then
  begin
    FSymb := True;
  end;
  Exit(Self);
end;

function TLongInteger.Positive: TLongInteger;
begin
  Exit(Self);
end;

function TLongInteger.Power(Par: TLongInteger): TLongInteger;
var
  LIList: TObjectList<TLongInteger>;
  S: Boolean;
  i: Integer;
begin
  if Par.IsZero or not Par.FSymb then
  begin
    raise EPowerError.Create('Error Message: Power index must be greater than ZERO!');
  end;
  if IsZero then
  begin
    Exit(Self);
  end;
  if not FSymb and Par.FNumL[0] then
  begin
    S := True;
  end
  else
  begin
    S := FSymb;
  end;
  FSymb := True;
  LIList := TObjectList<TLongInteger>.Create;
  try
    LIList.Add(TLongInteger.Create);
    LIList[0].CopyVal(Self);
    for i := 0 to Par.FNumL.Count - 2 do
    begin
      LIList.Add(TLongInteger.Create(LIList[i]).Multiply(LIList[i]));
    end;
    if not FSymb and Par.FNumL[0] then
    begin
      FSymb := True;
    end;
    FNumL.Clear;
    FNumL.Add(True);
    for i := 0 to Par.FNumL.Count - 1 do if Par.FNumL[i] then
    begin
      Multiply(LIList[i]);
    end;
  finally
    FreeAndNil(LIList);
  end;
  Normalize;
  FSymb := S;
  Exit(Self);
end;

function TLongInteger.PowerMod(PowPar, ModPar: TLongInteger): TLongInteger;
var
  LIList: TObjectList<TLongInteger>;
  i: Integer;
  S: Boolean;
begin
  if PowPar.IsZero or not PowPar.FSymb then
  begin
    raise EPowerError.Create('Error Message: Power index must be greater than ZERO!');
  end;
  if ModPar.IsZero then
  begin
    raise EZeroError.Create('Error Message: Cannot divide ZERO!');
  end;
  if IsZero then
  begin
    Exit(Self);
  end;
  if not FSymb and PowPar.FNumL[0] then
  begin
    S := True;
  end
  else
  begin
    S := FSymb;
  end;
  FSymb := True;
  LIList := TObjectList<TLongInteger>.Create;
  try
    LIList.Add(TLongInteger.Create(Self));
    LIList[0].Modulus(ModPar);
    for i := 0 to PowPar.FNumL.Count - 2 do
    begin
      LIList.Add(TLongInteger.Create(LIList[i]).Multiply(LIList[i]).Modulus(ModPar));
    end;
    FNumL.Clear;
    FNumL.Add(True);
    for i := 0 to PowPar.FNumL.Count - 1 do if PowPar.FNumL[i] then
    begin
      Multiply(LIList[i]).Modulus(ModPar);
    end;
  finally
    FreeAndNil(LIList);
  end;
  Normalize;
  FSymb := S;
  Exit(Self);
end;

function TLongInteger.SameSignAdd(Par: TLongInteger): TLongInteger;
var
  F, X, Y, P, Q: Boolean;
  a, b, n: Integer;
  i: Integer;
begin
  Normalize;
  Par.Normalize;
  if FSymb <> Par.FSymb then
  begin
    Exit(Self);
  end;
  F := False;
  a := FNumL.Count;
  b := Par.FNumL.Count;
  n := Max(a, b);
  for i := 0 to n - 1 do
  begin
    if i >= a then
    begin
      FNumL.Add(False);
    end;
    P := FNumL[i];
    if i >= b then
    begin
      Q := False;
    end
    else
    begin
      Q := Par.FNumL[i];
    end;
    X := P xor Q xor F;
    Y := (P and Q) or (Q and F) or (F and P);
    FNumL[i] := X;
    F := Y;
  end;
  if F then
  begin
    FNumL.Add(True);
  end;
  Normalize;
  Exit(Self);
end;


function TLongInteger.SameSignSub(Par: TLongInteger): TLongInteger;
var
  F, X, Y, P, Q: Boolean;
  a, b, n: Integer;
  i: Integer;
  G: Boolean;
begin
  Normalize;
  Par.Normalize;
  if FSymb <> Par.FSymb then
  begin
    Exit(Self);
  end;
  if Equal(Par) then
  begin
    Zero;
    Exit(Self);
  end;
  F := False;
  a := FNumL.Count;
  b := Par.FNumL.Count;
  n := Max(a, b);
  if GreaterThan(Par) then
  begin
    G := True;
  end
  else
  begin
    G := False;
    FSymb := not FSymb;
  end;
  for i := 0 to n - 1 do
  begin
    if not G then
    begin
      if i >= a then
      begin
        FNumL.Add(False);
      end;
      Q := FNumL[i];
      if i >= b then
      begin
        P := False;
      end
      else
      begin
        P := Par.FNumL[i];
      end;
    end
    else
    begin
      if i >= a then
      begin
        FNumL.Add(False);
      end;
      P := FNumL[i];
      if i >= b then
      begin
        Q := False;
      end
      else
      begin
        Q := Par.FNumL[i];
      end;
    end;
    X := P xor Q xor F;
    Y := (not P and Q) or (Q and F) or (F and not P);
    FNumL[i] := X;
    F := Y;
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.ShiftL(Shift: Integer): TLongInteger;
var
  AList: TList<Boolean>;
  i: Integer;
begin
  if Shift <= 0 then
  begin
    Exit(Self);
  end;
  AList := TList<Boolean>.Create;
  try
    AList.Count := Shift;
    for i := 0 to Shift - 1 do
    begin
      AList[i] := False;
    end;
    FNumL.InsertRange(0, AList);
  finally
    FreeAndNil(AList);
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.ShiftR(Shift: Integer): TLongInteger;
begin
  if Shift <= 0 then
  begin
    Exit(Self);
  end;
  if FNumL.Count <= Shift then
  begin
    Zero;
  end;
  FNumL.DeleteRange(0, Shift);
  Normalize;
  Exit(Self);
end;

function TLongInteger.Subtract(Par: TLongInteger): TLongInteger;
begin
  if IsZero then
  begin
    CopyVal(Par);
    FSymb := not FSymb;
  end
  else if Par.IsZero then
  begin
    Exit(Self);
  end
  else if FSymb = Par.FSymb then
  begin
    SameSignSub(Par);
  end
  else
  begin
    FSymb := not FSymb;
    SameSignAdd(Par);
    FSymb := not FSymb;
  end;
  Exit(Self);
end;

function TLongInteger.ToBytes: TBytes;
var
  bs: TBytes;
  i, l: Integer;
  n: Byte;
begin
  FNumL.Delete(FNumL.Count - 1); // 删除首位以确保算法一致
  if FNumL.Count mod 8 <> 0 then
  begin
    SetLength(bs, 0);
    Exit(bs);
  end;
  l := FNumL.Count div 8;
  SetLength(bs, l);
  for i := 0 to l - 1 do
  begin
    n := 0;
    if FNumL[8 * i] then
    begin
      n := n + 1;
    end;
    if FNumL[8 * i + 1] then
    begin
      n := n + 2;
    end;
    if FNumL[8 * i + 2] then
    begin
      n := n + 4;
    end;
    if FNumL[8 * i + 3] then
    begin
      n := n + 8;
    end;
    if FNumL[8 * i + 4] then
    begin
      n := n + 16;
    end;
    if FNumL[8 * i + 5] then
    begin
      n := n + 32;
    end;
    if FNumL[8 * i + 6] then
    begin
      n := n + 64;
    end;
    if FNumL[8 * i + 7] then
    begin
      n := n + 128;
    end;
    bs[i] := n;
  end;
  Exit(bs);
end;

function TLongInteger.ToString10: string;
var
  F: Boolean;
  s: string;
  Par, ModVal: TLongInteger;
begin
  if FNumL.Count = 0 then
  begin
    Exit('');
  end;
  s := '';
  F := FSymb;
  FSymb := True;
  ModVal := TLongInteger.Create;
  Par := TLongInteger.Create;
  try
    Par.CopyVal(Self);
    repeat
      Par.DivAndMod10(ModVal);
      s := ModVal.ToString16 + s;
    until Par.IsZero;
  finally
    FreeAndNil(ModVal);
    FreeAndNil(Par);
  end;
  FSymb := F;
  if not FSymb then
  begin
    s := '-' + s;
  end;
  Exit(s);
end;

function TLongInteger.ToString16: string;
var
  s: string;
  i: Integer;
  n: Byte;
begin
  if FNumL.Count = 0 then
  begin
    Exit('');
  end;
  s := '';
  for i := 0 to (FNumL.Count - 1) div 4 do
  begin
    n := 0;
    if (4 * i + 3 < FNumL.Count) and (FNumL[4 * i + 3]) then
    begin
      Inc(n, 8);
    end;
    if (4 * i + 2 < FNumL.Count) and (FNumL[4 * i + 2]) then
    begin
      Inc(n, 4);
    end;
    if (4 * i + 1 < FNumL.Count) and (FNumL[4 * i + 1]) then
    begin
      Inc(n, 2);
    end;
    if (4 * i < FNumL.Count) and (FNumL[4 * i]) then
    begin
      Inc(n, 1);
    end;
    s := GetHexChr(n) + s;
  end;
  if not FSymb then
  begin
    s := '-' + s;
  end;
  Exit(s);
end;

function TLongInteger.ToString2: string;
var
  s: string;
  i: Integer;
begin
  if FNumL.Count = 0 then
  begin
    Exit('');
  end;
  if not FSymb then
  begin
    s := '-';
  end;
  for i := FNumL.Count - 1 downto 0 do
  begin
    if FNumL[i] then
    begin
      s := s + '1';
    end
    else
    begin
      s := s + '0';
    end;
  end;
  Exit(s);
end;

function TLongInteger.Zero: TLongInteger;
begin
  FNumL.Clear;
  FNumL.Add(False);
  FSymb := True;
  Exit(Self);
end;

{ THexInt }

class operator THexInt.Add(const Hex1, Hex2: THexInt): THexInt;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);
    LI2.FromString16(Hex2.FHex);
    Result.FHex := LI1.Add(LI2).ToString16;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class function THexInt.Create(const Str: string): THexInt;
var
  LI1: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  try
    if LI1.FromString16(Str) then
    begin
      Result.FHex := LI1.ToString16;
    end
    else
    begin
      Result.FHex := '0';
    end;
  finally
    FreeAndNil(LI1);
  end;
end;

class operator THexInt.Divide(const Hex1, Hex2: THexInt): THexInt;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);
    LI2.FromString16(Hex2.FHex);
    Result.FHex := LI1.Divide(LI2).ToString16;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.Equal(const Hex1, Hex2: THexInt): Boolean;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);      
    LI2.FromString16(Hex2.FHex);
    Exit(LI1.Equal(LI2));
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

function THexInt.GetBinValue: string;
var
  LI1: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  try
    if LI1.FromString16(FHex) then
    begin
      Result := LI1.ToString2;
    end;
  finally
    FreeAndNil(LI1);
  end;
end;

function THexInt.GetDecValue: string;
var
  LI1: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  try
    if LI1.FromString16(FHex) then
    begin
      Result := LI1.ToString10;
    end;
  finally
    FreeAndNil(LI1);
  end;
end;

function THexInt.GetHexValue: string;
begin
  Result := FHex;
end;

class operator THexInt.GreaterThan(const Hex1, Hex2: THexInt): Boolean;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);      
    LI2.FromString16(Hex2.FHex);
    Exit(LI1.GreaterThan(LI2));
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.GreaterThanOrEqual(const Hex1, Hex2: THexInt): Boolean;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);      
    LI2.FromString16(Hex2.FHex);
    Exit(not LI1.LessThan(LI2));
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.IntDivide(const Hex1, Hex2: THexInt): THexInt;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);
    LI2.FromString16(Hex2.FHex);
    Result.FHex := LI1.Divide(LI2).ToString16;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.LessThan(const Hex1, Hex2: THexInt): Boolean;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);      
    LI2.FromString16(Hex2.FHex);
    Exit(LI1.LessThan(LI2));
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.LessThanOrEqual(const Hex1, Hex2: THexInt): Boolean;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);      
    LI2.FromString16(Hex2.FHex);
    Exit(not LI1.GreaterThan(LI2));
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.Modulus(const Hex1, Hex2: THexInt): THexInt;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);
    LI2.FromString16(Hex2.FHex);
    Result.FHex := LI1.Modulus(LI2).ToString16;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.Multiply(const Hex1, Hex2: THexInt): THexInt;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);
    LI2.FromString16(Hex2.FHex);
    Result.FHex := LI1.Multiply(LI2).ToString16;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.Negative(const Hex1: THexInt): THexInt;
var
  LI1: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);
    LI1.Negative;
    Result.FHex := LI1.ToString16;
  finally
    FreeAndNil(LI1);
  end;
end;

class operator THexInt.NotEqual(const Hex1, Hex2: THexInt): Boolean;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);      
    LI2.FromString16(Hex2.FHex);
    Exit(not LI1.Equal(LI2));
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

class operator THexInt.Positive(const Hex1: THexInt): THexInt;
begin
  Result.FHex := Hex1.FHex;
end;

function THexInt.Power(Hex1: THexInt): THexInt;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(FHex);
    LI2.FromString16(Hex1.FHex);
    Result.FHex := LI1.Power(LI2).ToString16;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

function THexInt.PowerMod(Hex1, Hex2: THexInt): THexInt;
var
  LI1, LI2, LI3: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  LI3 := TLongInteger.Create;
  try
    LI1.FromString16(FHex);
    LI2.FromString16(Hex1.FHex);
    LI3.FromString16(Hex2.FHex);
    Result.FHex := LI1.PowerMod(LI2, LI3).ToString16;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

procedure THexInt.SetBinValue(const Value: string);
var
  LI1: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  try
    if LI1.FromString2(Value) then
    begin
      FHex := LI1.ToString16;
    end;
  finally
    FreeAndNil(LI1);
  end;
end;

procedure THexInt.SetDecValue(const Value: string);
var
  LI1: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  try
    if LI1.FromString10(Value) then
    begin
      FHex := LI1.ToString16;
    end;
  finally
    FreeAndNil(LI1);
  end;
end;

procedure THexInt.SetHexValue(const Value: string);
var
  LI1: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  try
    if LI1.FromString16(Value) then
    begin
      FHex := LI1.ToString16;
    end;
  finally
    FreeAndNil(LI1);
  end;
end;

class operator THexInt.Subtract(const Hex1, Hex2: THexInt): THexInt;
var
  LI1, LI2: TLongInteger;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  try
    LI1.FromString16(Hex1.FHex);
    LI2.FromString16(Hex2.FHex);
    Result.FHex := LI1.Subtract(LI2).ToString16;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
  end;
end;

end.
