{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{                    RdeEM Long                         }
{                     大数单元                          }
{                     ver 1.21                          }
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
  System.SysUtils, System.Math;

type
  EZeroError = class(Exception);
  EPowerError = class(Exception);

  TLongInteger = class;
  TLongReal = class;

  TComplexMA = record
  private
    FM: Double; // 模數
    FA: Double; // 輻角
  public
    class function Create(const Modulus, Argument: Double): TComplexMA; static; inline;
    class operator Multiply(const C1, C2: TComplexMA): TComplexMA; overload;
    class operator Multiply(const C1: TComplexMA; const C2: Double): TComplexMA; overload;
    class operator Divide(const C1, C2: TComplexMA): TComplexMA; overload;
    class operator Divide(const C1: TComplexMA; const C2: Double): TComplexMA; overload;
    class operator Add(const C1, C2: TComplexMA): TComplexMA;
    class operator Subtract(const C1, C2: TComplexMA): TComplexMA;
    function Modulus: Double;
    function Argument: Double;
    function Real: Double;
    function Imaginary: Double;
    function Conjugate: TComplexMA;
  end;

  TComplexRI = record
  private
    FR: Double; // 實部
    FI: Double; // 虛部
  public
    class function Create(const Real, Imaginary: Double): TComplexRI; static; inline;
    class operator Multiply(const C1, C2: TComplexRI): TComplexRI; overload;
    class operator Multiply(const C1: TComplexRI; const C2: Double): TComplexRI; overload;
    class operator Divide(const C1, C2: TComplexRI): TComplexRI; overload;
    class operator Divide(const C1: TComplexRI; const C2: Double): TComplexRI; overload;
    class operator Add(const C1, C2: TComplexRI): TComplexRI;
    class operator Subtract(const C1, C2: TComplexRI): TComplexRI;
    function Modulus: Double;
    function Argument: Double;
    function Real: Double;
    function Imaginary: Double;
    function Conjugate: TComplexRI;
  end;

  TLongInteger = class
  private
    FSymb: Boolean; // 符号位，True代表正数。
    FNumL: TArray<Boolean>;
    function AbsAdd(const Par: TLongInteger): TLongInteger;
    function AbsSub(const Par: TLongInteger; const ChangeSymbol: Boolean = True): TLongInteger;
      // AbsSub計算原數之絕對值與參數之絕對值的差值
      // 當ChangeSymbol爲True時，會在原數之絕對值小於參數之絕對值的情況下改變原數的符號
      // 當ChangeSymbol爲False時，則不會改變符號，默認情況下會改變原數符號
    function Mut10PowN(N: TLongInteger): TLongInteger;
    function Div10PowN(N: TLongInteger): TLongInteger;
    function Normalize: TLongInteger;
    function AbsEqual(Par: TLongInteger): Boolean;
    function AbsLessThan(Par: TLongInteger): Boolean;
    function AbsGreaterThan(Par: TLongInteger): Boolean;
  public
    constructor Create; overload;
    constructor Create(LI: TLongInteger); overload;
    constructor Create(N: Int64); overload;
    constructor Create(N: Integer); overload;
    constructor Create(N: Cardinal); overload;
    constructor Create(N: Word); overload;
    constructor Create(N: Byte); overload;
    destructor Destroy; override;
    // 複製
    function CopyVal(const Par: TLongInteger): TLongInteger;
    // 判斷
    function Equal(const Par: TLongInteger): Boolean;
    function LessThan(const Par: TLongInteger): Boolean;
    function GreaterThan(const Par: TLongInteger): Boolean;
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
    function ToBytes16: TBytes;
    function FromString2(Str: string): TLongInteger;
    function FromString10(Str: string): TLongInteger;
    function FromString16(Str: string): TLongInteger;
    function FromInteger(N: Byte): TLongInteger; overload;
    function FromInteger(N: Word): TLongInteger; overload;
    function FromInteger(N: Cardinal): TLongInteger; overload;
    function FromInteger(N: Integer): TLongInteger; overload;
    function FromInteger(N: Int64): TLongInteger; overload;
    function FromBytes(B: TBytes): TLongInteger;
    function FromBytes16(B: TBytes): TLongInteger;
    function FromRandom(Digits: Integer): TLongInteger;
    function FromRandomOdd(Digits: Integer): TLongInteger;
    function FromRandomEven(Digits: Integer): TLongInteger;
    // 歸零、絕對值、正負
    function Zero: TLongInteger;
    function AbsoluteVal: TLongInteger;
    function Positive: TLongInteger;
    function Negative: TLongInteger;
    // 運算
    function Add(const Par: TLongInteger): TLongInteger;
    function Subtract(const Par: TLongInteger): TLongInteger;
    function Multiply(const Par: TLongInteger): TLongInteger;
    function Divide(const Par: TLongInteger): TLongInteger;
    function Modulus(const Par: TLongInteger): TLongInteger;
    function Power(const Par: TLongInteger): TLongInteger;
    function PowAndMod(PowPar, ModPar: TLongInteger): TLongInteger;
    function DivAndMod(const Par, ModVal: TLongInteger): TLongInteger;
    function LogicalXor(const Par: TLongInteger): TLongInteger;
    function LogicalOr(const Par: TLongInteger): TLongInteger;
    function LogicalAnd(const Par: TLongInteger): TLongInteger;
    function LogicalNot: TLongInteger;
    function ShiftR(Shift: Integer): TLongInteger;
    function ShiftL(Shift: Integer): TLongInteger;
      // 值
    function GetValue(Index: Integer): Boolean;
  end;

  TLongReal = class // 用科學記數法記錄實數，Val = FCoe * Power(10, FExp)
  private
    FCoe: TLongInteger; // 係數
    FExp: Integer; // 指數
    function Normalize: TLongReal;
  public
    constructor Create; overload;
    constructor Create(LR: TLongReal); overload;
    constructor Create(Coe: TLongInteger); overload;
    constructor Create(Coe: TLongInteger; Exp: Integer); overload;
    destructor Destroy; override;
    // 複製
    function CopyVal(const Par: TLongReal): TLongReal;
    // 判斷
    function Equal(const Par: TLongReal; Digit: Integer = -5): Boolean;
    function LessThan(const Par: TLongReal; Digit: Integer = -5): Boolean;
    function GreaterThan(const Par: TLongReal; Digit: Integer = -5): Boolean;
      // Digit代表判定精度，如果兩數之差在該量級範圍內，則判定爲相等。
      // 如Digit取-5時，則相差絕對值在Power(10, -5)內都視爲相等。
    // 歸零、絕對值、正負
    function Zero: TLongReal;
    function AbsoluteVal: TLongReal;
    function Positive: TLongReal;
    function Negative: TLongReal;
    function Trunc(LI: TLongInteger): TLongInteger;
    function Round(LI: TLongInteger): TLongInteger;
    // 四則運算
    function Add(const Par: TLongReal): TLongReal;
    function Subtract(const Par: TLongReal): TLongReal;
    function Multiply(const Par: TLongReal): TLongReal;
    function Divide(const Par: TLongReal; Digit: Integer = 5): TLongReal;
      // Digit代表額外的位數。取0則相當於對兩數的係數部份做整數除法，取x則相當於保留x位小數
    // 轉換（僅支持無小數點之科學記數法的轉換）
    function ToStringOri: string;
    function ToStringDot :string;
    function FromStringOri(Str: string): TLongReal;
    function FromStringDot(Str: string): TLongReal;
    function FromInteger(Coe, Exp: Integer): TLongReal; overload;
    function FromInteger(Coe, Exp: Int64): TLongReal; overload;
  end;

  // 字符與數值互換
  function GetHexChr(N: Byte): Char;
  function GetHexVal(C: Char): Byte;
  function GetDecChr(N: Byte): Char;
  function GetDecVal(C: Char): Byte;

  // 複數變換相關
  function ComplexMA(const Modulus, Argument: Double): TComplexMA; inline; overload;
  function ComplexMA(const ComplexRI: TComplexRI): TComplexMA; inline; overload;
  function ComplexRI(const Real, Imaginary: Double): TComplexRI; inline; overload;
  function ComplexRI(const ComplexMA: TComplexMA): TComplexRI; inline; overload;

  // 傅利葉變換相關
  function GetReverseList(var Reverse: TArray<Integer>; Bit: Integer): Boolean;
  function FFT(var Complex: TArray<TComplexMA>; var Reverse: TArray<Integer>; Count: Integer): Boolean;
  function IFFT(var Complex: TArray<TComplexMA>; var Reverse: TArray<Integer>; Count: Integer): Boolean;
  function FFTBytes(const B: TBytes; var Com: TArray<TComplexMA>; var Rev: TArray<Integer>; Count: Integer): Boolean;
  function IFFTBytes(var B: TBytes; var Com: TArray<TComplexMA>; var Rev: TArray<Integer>; Count: Integer): Boolean;

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

function ComplexMA(const Modulus, Argument: Double): TComplexMA;
begin
  Result.FM := Modulus;
  Result.FA := Argument;
end;

function ComplexMA(const ComplexRI: TComplexRI): TComplexMA;
begin
  Result.FM := ComplexRI.Modulus;
  Result.FA := ComplexRI.Argument;
end;

function ComplexRI(const Real, Imaginary: Double): TComplexRI;
begin
  Result.FR := Real;
  Result.FI := Imaginary;
end;

function ComplexRI(const ComplexMA: TComplexMA): TComplexRI;
begin
  Result.FR := ComplexMA.Real;
  Result.FI := ComplexMA.Imaginary;
end;

function GetReverseList(var Reverse: TArray<Integer>; Bit: Integer): Boolean;
var
  i: Integer;
begin
  if Bit > 31 then
  begin
    Exit(False);
  end;
  SetLength(Reverse, 1 shl Bit);
  for i := 0 to (1 shl Bit) - 1 do
  begin
    Reverse[i] := (Reverse[i shr 1] shr 1) or ((i and 1) shl (bit - 1));
  end;
  Exit(True);
end;

function FFT(var Complex: TArray<TComplexMA>; var Reverse: TArray<Integer>; Count: Integer): Boolean;
var
  i, j, k, step: Integer;
  v, wnk: TComplexMA;
  x, y: TComplexRI;
begin
  if (Complex = nil) or (Reverse = nil) or (Count < 1) then
  begin
    Exit(False);
  end;
  for i := 0 to Count - 1 do if i < Reverse[i] then
  begin
    v := Complex[i];
    Complex[i] := Complex[Reverse[i]];
    Complex[Reverse[i]] := v;
  end;
  step := 1;
  while step < Count do
  begin
    j := 0;
    while j < Count do
    begin
      for k := j to j + step - 1 do
      begin
        wnk := TComplexMA.Create(1, (k - j) * PI / step);
        x := ComplexRI(Complex[k]);
        y := ComplexRI(wnk * Complex[k + step]);
        Complex[k] := ComplexMA(x + y);
        Complex[k + step] := ComplexMA(x - y);
      end;
      j := j + step shl 1;
    end;
    step := step shl 1;
  end;
  Exit(True);
end;

function IFFT(var Complex: TArray<TComplexMA>; var Reverse: TArray<Integer>; Count: Integer): Boolean;
var
  i, j, k, step: Integer;
  v, wnk: TComplexMA;
  x, y: TComplexRI;
begin
  if (Complex = nil) or (Reverse = nil) or (Length(Complex) <> Count) then
  begin
    Exit(False);
  end;
  for i := 0 to Count - 1 do if i < Reverse[i] then
  begin
    v := Complex[i];
    Complex[i] := Complex[Reverse[i]];
    Complex[Reverse[i]] := v;
  end;
  step := 1;
  while step < Count do
  begin
    j := 0;
    while j < Count do
    begin
      for k := j to j + step - 1 do
      begin
        wnk := TComplexMA.Create(1, (j - k) * PI / step);
        x := ComplexRI(Complex[k]);
        y := ComplexRI(wnk * Complex[k + step]);
        Complex[k] := ComplexMA(x + y);
        Complex[k + step] := ComplexMA(x - y);
      end;
      j := j + step shl 1;
    end;
    step := step shl 1;
  end;
  for i := 0 to Count - 1 do
  begin
    Complex[i] := Complex[i] / Count;
  end;
  Exit(True);
end;

function FFTBytes(const B: TBytes; var Com: TArray<TComplexMA>; var Rev: TArray<Integer>; Count: Integer): Boolean;
var
  i: Integer;
begin
  if (Com = nil) or (Rev = nil) or (Count < 1) then
  begin
    Exit(False);
  end;
  SetLength(Com, Count);
  for i := 0 to Length(B) - 1 do
  begin
    Com[i] := TComplexMA.Create(B[i], 0);
  end;
  for i := Length(B) to Count - 1 do
  begin
    Com[i] := TComplexMA.Create(0, 0);
  end;
  FFT(Com, Rev, Count);
  Exit(True);
end;

function IFFTBytes(var B: TBytes; var Com: TArray<TComplexMA>; var Rev: TArray<Integer>; Count: Integer): Boolean;
var
  i, j, k: Integer;
begin
  IFFT(Com, Rev, Count);
  k := 0;
  SetLength(B, Count);
  for i := 0 to Count - 1 do
  begin
    j := Round(Com[i].Real) + k;
    B[i] := j and 255;
    k := j shr 8;
  end;
  i := Count;
  while k > 0 do
  begin
    SetLength(B, i + 1);
    B[i] := k and 255;
    i := i + 1;
    k := k shr 8;
  end;
  Exit(True);
end;

{ TLongInteger }

function TLongInteger.AbsAdd(const Par: TLongInteger): TLongInteger;
var
  C, X, Y: Boolean;
  i, l: Integer;
begin
  C := False;
  l := Max(Length(FNumL), Length(Par.FNumL));
  SetLength(FNumL, l + 1);
  for i := 0 to l - 1 do
  begin
    if i >= Length(FNumL) then
    begin
      FNumL[i] := False;
    end;
    X := FNumL[i];
    if i >= Length(Par.FNumL) then
    begin
      Y := False;
    end
    else
    begin
      Y := Par.FNumL[i];
    end;
    FNumL[i] := X xor Y xor C;
    C := (X and Y) or (C and (X or Y));
  end;
  FNumL[l] := C;
  Normalize;
  Exit(Self);
end;

function TLongInteger.AbsSub(const Par: TLongInteger; const ChangeSymbol: Boolean): TLongInteger;
var
  C, X, Y, G: Boolean;
  i, l: Integer;
begin
  C := False;
  if AbsGreaterThan(Par) then
  begin
    G := True;
  end
  else
  begin
    G := False;
    if ChangeSymbol then
    begin
      FSymb := not FSymb;
    end;
  end;
  l := Max(Length(FNumL), Length(Par.FNumL));
  SetLength(FNumL, l);
  for i := 0 to l - 1 do
  begin
    if not G then
    begin
      if i >= Length(FNumL) then
      begin
        FNumL[i] := False;
      end;
      Y := FNumL[i];
      if i >= Length(Par.FNumL) then
      begin
        X := False;
      end
      else
      begin
        X := Par.FNumL[i];
      end;
    end
    else
    begin
      if i >= Length(FNumL) then
      begin
        FNumL[i] := False;
      end;
      X := FNumL[i];
      if i >= Length(Par.FNumL) then
      begin
        Y := False;
      end
      else
      begin
        Y := Par.FNumL[i];
      end;
    end;
    FNumL[i] := X xor Y xor C;
    C := (Y and C) or ((not X) and (Y or C))
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.AbsEqual(Par: TLongInteger): Boolean;
var
  i: Integer;
begin
  Normalize;
  Par.Normalize;
  if Length(FNumL) <> Length(Par.FNumL) then
  begin
    Exit(False);
  end;
  for i := 0 to Length(FNumL) - 1 do if FNumL[i] <> Par.FNumL[i] then
  begin
    Exit(False);
  end;
  Exit(True);
end;

function TLongInteger.AbsGreaterThan(Par: TLongInteger): Boolean;
var
  i: Integer;
begin
  Normalize;
  Par.Normalize;
  if Length(FNumL) > Length(Par.FNumL) then
  begin
    Exit(True);
  end;
  if Length(FNumL) < Length(Par.FNumL) then
  begin
    Exit(False);
  end;
  for i := Length(FNumL) - 1 downto 0 do
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

function TLongInteger.AbsLessThan(Par: TLongInteger): Boolean;
var
  i: Integer;
begin
  Normalize;
  Par.Normalize;
  if Length(FNumL) > Length(Par.FNumL) then
  begin
    Exit(False);
  end;
  if Length(FNumL) < Length(Par.FNumL) then
  begin
    Exit(True);
  end;
  for i := Length(FNumL) - 1 downto 0 do
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

function TLongInteger.AbsoluteVal: TLongInteger;
begin
  FSymb := True;
  Exit(Self);
end;

function TLongInteger.Add(const Par: TLongInteger): TLongInteger;
begin
  Normalize;
  Par.Normalize;
  if IsZero then
  begin
    Exit(CopyVal(Par));
  end
  else if Par.IsZero then
  begin
    Exit(Self);
  end
  else if FSymb = Par.FSymb then
  begin
    Exit(AbsAdd(Par));
  end
  else
  begin
    Exit(AbsSub(Par));
  end;
end;

function TLongInteger.CopyVal(const Par: TLongInteger): TLongInteger;
var
  i: Integer;
begin
  FSymb := Par.FSymb;
  SetLength(FNumL, Length(Par.FNumL));
  for i := 0 to Length(Par.FNumL) - 1 do
  begin
    FNumL[i] := Par.FNumL[i];
  end;
  Exit(Self);
end;

constructor TLongInteger.Create;
begin
  SetLength(FNumL, 1);
  FNumL[0] := False;
  FSymb := True;
end;

constructor TLongInteger.Create(LI: TLongInteger);
var
  i: Integer;
begin
  FSymb := LI.FSymb;
  SetLength(FNumL, Length(LI.FNumL));
  for i := 0 to Length(LI.FNumL) - 1 do
  begin
    FNumL[i] := LI.FNumL[i];
  end;
end;

constructor TLongInteger.Create(N: Int64);
var
  i: Integer;
begin
  FSymb := (N >= 0);
  SetLength(FNumL, 64);
  i := 0;
  N := Abs(N);
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
end;

constructor TLongInteger.Create(N: Integer);
var
  i: Integer;
begin
  FSymb := (N >= 0);
  SetLength(FNumL, 32);
  i := 0;
  N := Abs(N);
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
end;

constructor TLongInteger.Create(N: Cardinal);
var
  i: Integer;
begin
  FSymb := True;
  SetLength(FNumL, 32);
  i := 0;
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
end;

constructor TLongInteger.Create(N: Word);
var
  i: Integer;
begin
  FSymb := True;
  SetLength(FNumL, 16);
  i := 0;
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
end;

constructor TLongInteger.Create(N: Byte);
var
  i: Integer;
begin
  FSymb := True;
  SetLength(FNumL, 8);
  i := 0;
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
end;

destructor TLongInteger.Destroy;
begin
  SetLength(FNumL, 0);
  inherited;
end;

function TLongInteger.Digit: Integer;
begin
  Exit(Length(FNumL));
end;

function TLongInteger.Div10PowN(N: TLongInteger): TLongInteger;
var
  M: TLongInteger;
begin
  M := TLongInteger.Create(10);
  try
    M.Power(N);
    Divide(M);
  finally
    FreeAndNil(M);
  end;
  Exit(Self);
end;

function TLongInteger.DivAndMod(const Par, ModVal: TLongInteger): TLongInteger;
var
  ALI, BLI: TLongInteger;
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
  try
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
    n := Length(FNumL) - Length(Par.FNumL);
    SetLength(BLI.FNumL, n + 1);
    ALI.ShiftL(n);
    for i := n downto 0 do
    begin
      if not LessThan(ALI) then
      begin
        AbsSub(ALI);
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
  end;
  Exit(Self);
end;

function TLongInteger.Divide(const Par: TLongInteger): TLongInteger;
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
    n := Length(FNumL) - Length(Par.FNumL);
    SetLength(BLI.FNumL, n + 1);
    ALI.ShiftL(n);
    for i := n downto 0 do
    begin
      if not LessThan(ALI) then
      begin
        AbsSub(ALI);
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

function TLongInteger.Equal(const Par: TLongInteger): Boolean;
begin
  if Length(FNumL) = Length(Par.FNumL) then
  begin
    Exit(AbsEqual(Par));
  end;
  Exit(False);
end;

function TLongInteger.FromBytes(B: TBytes): TLongInteger;
var
  n: Byte;
  i, l: Integer;
begin
  FSymb := True;
  l := Length(B);
  SetLength(FNumL, 8 * l);
  for i := 0 to l - 1 do
  begin
    n := B[i];
    FNumL[8 * i] := (n and 1 = 1);
    n := n shr 1;
    FNumL[8 * i + 1] := (n and 1 = 1);
    n := n shr 1;
    FNumL[8 * i + 2] := (n and 1 = 1);
    n := n shr 1;
    FNumL[8 * i + 3] := (n and 1 = 1);
    n := n shr 1;
    FNumL[8 * i + 4] := (n and 1 = 1);
    n := n shr 1;
    FNumL[8 * i + 5] := (n and 1 = 1);
    n := n shr 1;
    FNumL[8 * i + 6] := (n and 1 = 1);
    n := n shr 1;
    FNumL[8 * i + 7] := (n and 1 = 1);
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.FromBytes16(B: TBytes): TLongInteger;
var
  n: Byte;
  i, l: Integer;
begin
  FSymb := True;
  l := Length(B);
  SetLength(FNumL, 4 * l);
  for i := 0 to l - 1 do
  begin
    n := B[i];
    FNumL[4 * i] := (n and 1 = 1);
    n := n shr 1;
    FNumL[4 * i + 1] := (n and 1 = 1);
    n := n shr 1;
    FNumL[4 * i + 2] := (n and 1 = 1);
    n := n shr 1;
    FNumL[4 * i + 3] := (n and 1 = 1);
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.FromInteger(N: Byte): TLongInteger;
var
  i: Integer;
begin
  FSymb := True;
  SetLength(FNumL, 8);
  i := 0;
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
  Exit(Self);
end;

function TLongInteger.FromInteger(N: Cardinal): TLongInteger;
var
  i: Integer;
begin
  FSymb := True;
  SetLength(FNumL, 32);
  i := 0;
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
  Exit(Self);
end;

function TLongInteger.FromInteger(N: Integer): TLongInteger;
var
  i: Integer;
begin
  FSymb := (N >= 0);
  SetLength(FNumL, 32);
  i := 0;
  N := Abs(N);
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
  Exit(Self);
end;

function TLongInteger.FromInteger(N: Int64): TLongInteger;
var
  i: Integer;
begin
  FSymb := (N >= 0);
  SetLength(FNumL, 64);
  i := 0;
  N := Abs(N);
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
  Exit(Self);
end;

function TLongInteger.FromInteger(N: Word): TLongInteger;
var
  i: Integer;
begin
  FSymb := True;
  SetLength(FNumL, 16);
  i := 0;
  repeat
    FNumL[i] := (N and 1  = 1);
    N := N shr 1;
    Inc(i);
  until N = 0;
  SetLength(FNumL, i);
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
  SetLength(FNumL, Digits);
  for i := 0 to Digits - 2 do
  begin
    Randomize;
    FNumL[i] := (Random < 0.5);
  end;
  FNumL[Digits - 1] := True;;
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
  SetLength(FNumL, Digits);
  FNumL[0] := False;
  for i := 1 to Digits - 2 do
  begin
    Randomize;
    FNumL[i] := (Random < 0.5);
  end;
  FNumL[Digits - 1] := True;;
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
  SetLength(FNumL, Digits);
  FNumL[0] := True;
  for i := 1 to Digits - 2 do
  begin
    Randomize;
    FNumL[i] := (Random < 0.5);
  end;
  FNumL[Digits - 1] := True;;
  Exit(Self);
end;

function TLongInteger.FromString10(Str: string): TLongInteger;
var
  i, l: Integer;
  v: Byte;
  LI, Par: TLongInteger;
begin
  if Str = '' then
  begin
    Exit(Zero);
  end;
  Str := Str.Replace(' ', '');
  if Str.Chars[0] = '-' then
  begin
    FSymb := False;
    Str := Str.Substring(1);
  end
  else
  begin
    FSymb := True;
  end;
  l := Str.Length;
  if l <= 0 then
  begin
    Exit(Zero);
  end;
  for i := 0 to l - 1 do if GetDecVal(Str.Chars[i]) = 255 then
  begin
    Exit(Zero);
  end;
  Zero;
  LI := TLongInteger.Create;
  Par := TLongInteger.Create;
  try
    SetLength(Par.FNumL, 4);
    Par.FNumL[0] := False;
    Par.FNumL[1] := True;
    Par.FNumL[2] := False;
    Par.FNumL[3] := True;
    for i := 0 to l - 1 do
    begin
      v := GetDecVal(Str.Chars[i]);
      LI.FromInteger(v);
      Multiply(Par).Add(LI);
    end;
  finally
    FreeAndNil(LI);
    FreeAndNil(Par);
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.FromString16(Str: string): TLongInteger;
var
  i, l: Integer;
  v: Byte;
begin
  if Str = '' then
  begin
    Exit(Zero);
  end;
  Str := UpperCase(Str).Replace(' ', '');
  l := Str.Length;
  if Str.Chars[0] = '-' then
  begin
    FSymb := False;
    Str := Str.Substring(1);
  end
  else
  begin
    FSymb := True;
  end;
  if l <= 0 then
  begin
    Exit(Zero);
  end;
  for i := 0 to l - 1 do
  begin
    if GetHexVal(Str.Chars[i]) = 255 then
    begin
      Exit(Zero);
    end;
  end;
  FSymb := True;
  SetLength(FNumL, 4 * l);
  for i := 0 to l - 1 do
  begin
    v := GetHexVal(Str.Chars[l - 1 - i]);
    FNumL[4 * i] := (v and 1 = 1);
    v := v shr 1;
    FNumL[4 * i + 1] := (v and 1 = 1);
    v := v shr 1;
    FNumL[4 * i + 2] := (v and 1 = 1);
    v := v shr 1;
    FNumL[4 * i + 3] := (v and 1 = 1);
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.FromString2(Str: string): TLongInteger;
var
  i, l: Integer;
begin
  if Str = '' then
  begin
    Exit(Zero);
  end;
  Str := Str.Replace(' ', '');
  if Str.Chars[0] = '-' then
  begin
    FSymb := False;
    Str := Str.Substring(1);
  end
  else
  begin
    FSymb := True;
  end;
  l := Str.Length;
  if l <= 0 then
  begin
    Exit(Zero);
  end;
  for i := 0 to l - 1 do if (Str.Chars[i] <> '0') or (Str.Chars[i] <> '1') then
  begin
    Exit(Zero);
  end;
  FSymb := True;
  SetLength(FNumL, l);
  for i := 0 to l - 1 do
  begin
    FNumL[i] := (Str.Chars[l - 1 - i] = '1');
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.GetValue(Index: Integer): Boolean;
begin
  Exit(FNumL[Index]);
end;

function TLongInteger.GreaterThan(const Par: TLongInteger): Boolean;
begin
  if FSymb and not Par.FSymb then
  begin
    Exit(True);
  end;
  if not FSymb and Par.FSymb then
  begin
    Exit(False);
  end;
  if FSymb and Par.FSymb then
  begin
    Exit(AbsGreaterThan(Par));
  end
  else
  begin
    Exit(AbsLessThan(Par));
  end;
end;

function TLongInteger.IsEven: Boolean;
begin
  Exit((Length(FNumL) > 0) and (FNumL[0] = False));
end;

function TLongInteger.IsOdd: Boolean;
begin
  Exit((Length(FNumL) > 0) and (FNumL[0] = True));
end;

function TLongInteger.IsZero: Boolean;
begin
  Normalize;
  if (FSymb = True) and (Length(FNumL) = 1) and (FNumL[0] = False) then
  begin
    Exit(True);
  end
  else
  begin
    Exit(False);
  end;
end;

function TLongInteger.LessThan(const Par: TLongInteger): Boolean;
begin
  if (not FSymb) and Par.FSymb then
  begin
    Exit(True);
  end;
  if FSymb and (not Par.FSymb) then
  begin
    Exit(False);
  end;
  if FSymb and Par.FSymb then
  begin
    Exit(AbsLessThan(Par));
  end
  else
  begin
    Exit(AbsGreaterThan(Par));
  end;
end;

function TLongInteger.LogicalAnd(const Par: TLongInteger): TLongInteger;
var
  i, n: Integer;
begin
  Normalize;
  Par.Normalize;
  if Length(FNumL) < Length(Par.FNumL) then
  begin
    n := Length(FNumL);
  end
  else
  begin
    n := Length(Par.FNumL);
  end;
  for i := 0 to n - 1 do
  begin
    FNumL[i] := FNumL[i] and Par.FNumL[i];
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.LogicalNot: TLongInteger;
var
  i: Integer;
begin
  for i := 0 to Length(FNumL) - 1 do
  begin
    FNumL[i] := not FNumL[i];
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.LogicalOr(const Par: TLongInteger): TLongInteger;
var
  i, n: Integer;
begin
  Normalize;
  Par.Normalize;
  if Length(FNumL) > Length(Par.FNumL) then
  begin
    n := Length(FNumL);
  end
  else
  begin
    n := Length(Par.FNumL);
    SetLength(FNumL, n);
  end;
  for i := 0 to n - 1 do if i < Length(Par.FNumL) then
  begin
    if i < Length(FNumL) then
    begin
      FNumL[i] := FNumL[i] or Par.FNumL[i];
    end
    else
    begin
      FNumL[i] := Par.FNumL[i];
    end;
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.LogicalXor(const Par: TLongInteger): TLongInteger;
var
  i, n: Integer;
begin
  Normalize;
  Par.Normalize;
  if Length(FNumL) > Length(Par.FNumL) then
  begin
    n := Length(FNumL);
  end
  else
  begin
    n := Length(Par.FNumL);
    SetLength(FNumL, n);
  end;
  for i := 0 to n - 1 do if i < Length(Par.FNumL) then
  begin
    if i < Length(FNumL) then
    begin
      FNumL[i] := FNumL[i] xor Par.FNumL[i];
    end
    else
    begin
      FNumL[i] := Par.FNumL[i];
    end;
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.Modulus(const Par: TLongInteger): TLongInteger;
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
    ALI.CopyVal(Par).FSymb := True;
    FSymb := True;
    if LessThan(ALI) then
    begin
      FSymb := S;
      Exit(Self);
    end;
    n := Length(FNumL) - Length(Par.FNumL);
    ALI.ShiftL(n);
    for i := n downto 0 do
    begin
      if not LessThan(ALI) then
      begin
        AbsSub(ALI);
      end;
      ALI.ShiftR(1);
    end;
  finally
    FreeAndNil(ALI);
  end;
  Normalize;
  FSymb := S;
  Exit(Self);
end;

function TLongInteger.Multiply(const Par: TLongInteger): TLongInteger;
var
  Bit, NumLength, i: Integer;
  RevList: TArray<Integer>;
  ComplexA, ComplexB: TArray<TComplexMA>;
  S: Boolean;
  B1, B2: TBytes;
begin
  Normalize;
  Par.Normalize;
  if IsZero or Par.IsZero then
  begin
    Zero;
    Exit(Self);
  end;
  S := not (FSymb xor Par.FSymb);
  B1 := ToBytes;
  B2 := Par.ToBytes;
  Bit := 1;
  NumLength := 2;
  while (1 shl bit) < Length(B1) + Length(B2) - 1 do
  begin
    NumLength := NumLength shl 1;
    Bit := Bit + 1;
  end;
  SetLength(ComplexA, NumLength);
  SetLength(ComplexB, NumLength);
  GetReverseList(RevList, Bit);
  FFTBytes(B1, ComplexA, RevList, NumLength);
  FFTBytes(B2, ComplexB, RevList, NumLength);
  for i := 0 to NumLength - 1 do
  begin
    ComplexA[i] := ComplexA[i] * ComplexB[i];
  end;
  IFFTBytes(B1, ComplexA, RevList, NumLength);
  FromBytes(B1);
  FSymb := S;
  Normalize;
  Exit(Self);
end;

function TLongInteger.Mut10PowN(N: TLongInteger): TLongInteger;
var
  M: TLongInteger;
begin
  M := TLongInteger.Create(10);
  try
    M.Power(N);
    Multiply(M);
  finally
    FreeAndNil(M);
  end;
  Exit(Self);
end;

function TLongInteger.Negative: TLongInteger;
begin
  FSymb := not FSymb;
  Normalize;
  Exit(Self);
end;

function TLongInteger.Normalize: TLongInteger;
var
  i, l: Integer;
begin
  l := Length(FNumL);
  for i := l downto 0 do
  begin
    if (i > 0) and FNumL[i - 1] then
    begin
      SetLength(FNumL, i);
      Break;
    end
    else if i = 0 then
    begin
      Zero;
    end;
  end;
  Exit(Self);
end;

function TLongInteger.Positive: TLongInteger;
begin
  Exit(Self);
end;

function TLongInteger.Power(const Par: TLongInteger): TLongInteger;
var
  LIB, LIP: TLongInteger;
  S: Boolean;
begin
  if not Par.FSymb then
  begin
    raise EPowerError.Create('Error Message: Power index mustnot be Smaller than ZERO!');
  end;
  if IsZero then
  begin
    Exit(Self);
  end;
  if Par.IsZero then
  begin
    FSymb := True;
    SetLength(FNumL, 1);
    FNumL[0] := True;
    Exit(Self);
  end;
  if not Par.FNumL[0] then
  begin
    S := True;
  end
  else
  begin
    S := FSymb;
  end;
  FSymb := True;
  LIB := TLongInteger.Create(Self);
  LIP := TLongInteger.Create(Par);
  SetLength(FNumL, 1);
  FNumL[0] := True;
  try
    while LIP.FSymb and not LIP.IsZero do
    begin
      if LIP.FNumL[0] then
      begin
        Multiply(LIB);
      end;
      LIP.ShiftR(1);
      LIB.Multiply(LIB);
    end;
  finally
    LIB.DisposeOf;
    LIP.DisposeOf;
  end;
  FSymb := S;
  Exit(Self);
end;

function TLongInteger.PowAndMod(PowPar, ModPar: TLongInteger): TLongInteger;
var
  LIB, LIP: TLongInteger;
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
  if not PowPar.FNumL[0] then
  begin
    S := True;
  end
  else
  begin
    S := FSymb;
  end;
  FSymb := True;
  LIB := TLongInteger.Create(Self);
  LIP := TLongInteger.Create(PowPar);
  SetLength(FNumL, 1);
  FNumL[0] := True;
  try
    while LIP.FSymb and not LIP.IsZero do
    begin
      if LIP.FNumL[0] then
      begin
        Multiply(LIB).Modulus(ModPar);
      end;
      LIP.ShiftR(1);
      LIB.Multiply(LIB).Modulus(ModPar);
    end;
  finally
    LIB.DisposeOf;
    LIP.DisposeOf;
  end;
  FSymb := S;
  Exit(Self);
end;

function TLongInteger.ShiftL(Shift: Integer): TLongInteger;
var
  AList: TArray<Boolean>;
  i: Integer;
begin
  if Shift <= 0 then
  begin
    Exit(Self);
  end;
  SetLength(AList, Shift);
  for i := 0 to Shift - 1 do
  begin
    AList[i] := False;
  end;
  Insert(AList, FNumL, 0);
  Normalize;
  Exit(Self);
end;

function TLongInteger.ShiftR(Shift: Integer): TLongInteger;
begin
  if Shift <= 0 then
  begin
    Exit(Self);
  end;
  if Length(FNumL) <= Shift then
  begin
    Zero;
  end
  else
  begin
    Delete(FNumL, 0, Shift);
  end;
  Normalize;
  Exit(Self);
end;

function TLongInteger.Subtract(const Par: TLongInteger): TLongInteger;
begin
  Normalize;
  Par.Normalize;
  if IsZero then
  begin
    FNumL := Copy(Par.FNumL, 0, Length(Par.FNumL));
    FSymb := not Par.FSymb;
    Exit(Self);
  end
  else if Par.IsZero then
  begin
    Exit(Self);
  end
  else if FSymb = Par.FSymb then
  begin
    Exit(AbsSub(Par));
  end
  else
  begin
    Exit(AbsAdd(Par));
  end;
end;

function TLongInteger.ToBytes: TBytes;
var
  bs: TBytes;
  i, l: Integer;
  n: Byte;
begin
  l := (Length(FNumL) - 1) div 8 + 1;
  SetLength(bs, l);
  for i := 0 to l - 1 do
  begin
    n := 0;
    if (8 * i < Length(FNumL)) and FNumL[8 * i] then
    begin
      n := n + 1;
    end;
    if (8 * i + 1 < Length(FNumL)) and FNumL[8 * i + 1] then
    begin
      n := n + 2;
    end;
    if (8 * i + 2 < Length(FNumL)) and FNumL[8 * i + 2] then
    begin
      n := n + 4;
    end;
    if (8 * i + 3 < Length(FNumL)) and FNumL[8 * i + 3] then
    begin
      n := n + 8;
    end;
    if (8 * i + 4 < Length(FNumL)) and FNumL[8 * i + 4] then
    begin
      n := n + 16;
    end;
    if (8 * i + 5 < Length(FNumL)) and FNumL[8 * i + 5] then
    begin
      n := n + 32;
    end;
    if (8 * i + 6 < Length(FNumL)) and FNumL[8 * i + 6] then
    begin
      n := n + 64;
    end;
    if (8 * i + 7 < Length(FNumL)) and FNumL[8 * i + 7] then
    begin
      n := n + 128;
    end;
    bs[i] := n;
  end;
  Exit(bs);
end;

function TLongInteger.ToBytes16: TBytes;
var
  bs: TBytes;
  i, l: Integer;
  n: Byte;
begin
  l := (Length(FNumL) - 1) div 4 + 1;
  SetLength(bs, l);
  for i := 0 to l - 1 do
  begin
    n := 0;
    if (4 * i < Length(FNumL)) and FNumL[4 * i] then
    begin
      n := n + 1;
    end;
    if (4 * i + 1 < Length(FNumL)) and FNumL[4 * i + 1] then
    begin
      n := n + 2;
    end;
    if (4 * i + 2 < Length(FNumL)) and FNumL[4 * i + 2] then
    begin
      n := n + 4;
    end;
    if (4 * i + 3 < Length(FNumL)) and FNumL[4 * i + 3] then
    begin
      n := n + 8;
    end;
    bs[i] := n;
  end;
  Exit(bs);
end;

function TLongInteger.ToString10: string;
var
  F: Boolean;
  s: string;
  Par, ModVal, LITen: TLongInteger;
begin
  if Length(FNumL) = 0 then
  begin
    Exit('0');
  end;
  s := '';
  F := FSymb;
  FSymb := True;
  ModVal := TLongInteger.Create;
  Par := TLongInteger.Create;
  LITen := TLongInteger.Create(10);
  try
    Par.CopyVal(Self);
    repeat
      Par.DivAndMod(LITen, ModVal);
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
  if Length(FNumL) = 0 then
  begin
    Exit('0');
  end;
  s := '';
  for i := 0 to (Length(FNumL) - 1) div 4 do
  begin
    n := 0;
    if (4 * i + 3 < Length(FNumL)) and (FNumL[4 * i + 3]) then
    begin
      Inc(n, 8);
    end;
    if (4 * i + 2 < Length(FNumL)) and (FNumL[4 * i + 2]) then
    begin
      Inc(n, 4);
    end;
    if (4 * i + 1 < Length(FNumL)) and (FNumL[4 * i + 1]) then
    begin
      Inc(n, 2);
    end;
    if (4 * i < Length(FNumL)) and (FNumL[4 * i]) then
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
  if Length(FNumL) = 0 then
  begin
    Exit('0');
  end;
  if not FSymb then
  begin
    s := '-';
  end;
  for i := Length(FNumL) - 1 downto 0 do
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
  SetLength(FNumL, 1);
  FNumL[0] := False;
  FSymb := True;
  Exit(Self);
end;

{ TLongReal }

function TLongReal.AbsoluteVal: TLongReal;
begin
  FCoe.AbsoluteVal;
  Exit(Self);
end;

function TLongReal.Add(const Par: TLongReal): TLongReal;
var
  N: Integer;
  LI: TLongInteger;
  R: TLongReal;
begin
  R := TLongReal.Create;
  LI := TLongInteger.Create;
  try
    N := Abs(Par.FExp - FExp);
    LI.FromInteger(N);
    if Par.FExp > FExp then
    begin
      R.CopyVal(Par);
      FCoe.Add(R.FCoe.Mut10PowN(LI));
    end
    else
    begin
      FExp := Par.FExp;
      FCoe.Mut10PowN(LI).Add(Par.FCoe);
    end;
    Normalize;
  finally
    FreeAndNil(R);
    FreeAndNil(LI);
  end;
  Exit(Self);
end;

function TLongReal.CopyVal(const Par: TLongReal): TLongReal;
begin
  FCoe.CopyVal(Par.FCoe);
  FExp := Par.FExp;
  Exit(Self);
end;

constructor TLongReal.Create;
begin
  inherited;
  FCoe := TLongInteger.Create;
  FExp := 0;
end;

constructor TLongReal.Create(LR: TLongReal);
begin
  inherited Create;
  FCoe := TLongInteger.Create(LR.FCoe);
  FExp := LR.FExp;
end;

constructor TLongReal.Create(Coe: TLongInteger);
begin
  inherited Create;
  FCoe := TLongInteger.Create(Coe);
  FExp := 0;
end;

constructor TLongReal.Create(Coe: TLongInteger; Exp: Integer);
begin
  inherited Create;
  FCoe := TLongInteger.Create(Coe);
  FExp := Exp;
end;

destructor TLongReal.Destroy;
begin
  FreeAndNil(FCoe);
  inherited;
end;

function TLongReal.Divide(const Par: TLongReal; Digit: Integer): TLongReal;
var
  LI: TLongInteger;
begin
  if Digit < 0 then
  begin
    Digit := 0;
  end;
  LI := TLongInteger.Create(Digit);
  try
    FCoe.Mut10PowN(LI).Divide(Par.FCoe);
    FExp := FExp - Digit - Par.FExp;
  finally
    FreeAndNil(LI);
  end;
  Normalize;
  Exit(Self);
end;

function TLongReal.Equal(const Par: TLongReal; Digit: Integer): Boolean;
var
  R: TLongReal;
  N: TLongInteger;
begin
  R := TLongReal.Create(Self);
  N := TLongInteger.Create;
  try
    R.Subtract(Par).AbsoluteVal;
    if R.FCoe.IsZero then
    begin
      Exit(True);
    end;
    R.FExp := R.FExp - Digit;
    if R.FExp > 0 then
    begin
      Exit(False);
    end;
    N.FromInteger(- R.FExp);
    R.FCoe.Div10PowN(N);
    Exit(R.FCoe.IsZero);
  finally
    FreeAndNil(R);
    FreeAndNil(N);
  end;
  Exit(False);
end;

function TLongReal.FromInteger(Coe, Exp: Integer): TLongReal;
begin
  FCoe.FromInteger(Coe);
  FExp := Exp;
  Exit(Self);
end;

function TLongReal.FromInteger(Coe, Exp: Int64): TLongReal;
begin
  FCoe.FromInteger(Coe);
  FExp := Exp;
  Exit(Self);
end;

function TLongReal.FromStringDot(Str: string): TLongReal;
var
  c: Char;
  i, n, l: Integer;
begin
  n := 0;
  for i := 0 to Str.Length - 1 do
  begin
    c := Str.Chars[i];
    if (c <> '.') and (GetDecVal(c) = 255) then
    begin
      Exit(Zero);
    end;
  end;
  if Str.StartsWith('0.') then
  begin
    Str := Str.Replace('.', '');
    for i := 0 to Str.Length - 1 do if Str.Chars[i] <> '0' then
    begin
      Str := Str.Substring(i);
      n := - i - Str.Length + 1;
      Break;
    end;
  end
  else
  begin
    n := Str.IndexOf('.');
    if n = -1 then
    begin
      l := Str.Length;
      n := 0;
    end
    else
    begin
      Str := Str.Replace('.', '');
      l := Str.Length;
      n := n - l;
    end;
    for i := 0 to l - 1 do if Str.Chars[l - i - 1] <> '0' then
    begin
      Str := Str.Substring(0, l - i);
      n := n + i;
      Break;
    end;
  end;
  FCoe.FromString10(Str);
  FExp := n;
  Exit(Self);
end;

function TLongReal.FromStringOri(Str: string): TLongReal;
var
  SS: TArray<string>;
begin
  SS := Str.Split(['e', 'E']);
  FCoe.FromString10(SS[0]);
  if not FCoe.IsZero then
  begin
    FExp := StrToIntDef(SS[1], 0);
  end
  else
  begin
    FExp := 0;
  end;
  Exit(Self);
end;

function TLongReal.GreaterThan(const Par: TLongReal; Digit: Integer): Boolean;
var
  R: TLongReal;
  N: TLongInteger;
begin
  R := TLongReal.Create(Self);
  N := TLongInteger.Create;
  try
    R.Subtract(Par);
    if not R.FCoe.FSymb then
    begin
      Exit(False);
    end;
    if R.FCoe.IsZero then
    begin
      Exit(False);
    end;
    R.FExp := R.FExp - Digit;
    if R.FExp > 0 then
    begin
      Exit(True);
    end;
    N.FromInteger(- R.FExp);
    R.FCoe.Div10PowN(N);
    Exit(not R.FCoe.IsZero);
  finally
    FreeAndNil(R);
    FreeAndNil(N);
  end;
  Exit(False);
end;

function TLongReal.LessThan(const Par: TLongReal; Digit: Integer): Boolean;
var
  R: TLongReal;
  N: TLongInteger;
begin
  R := TLongReal.Create(Self);
  N := TLongInteger.Create;
  try
    R.Subtract(Par);
    if R.FCoe.FSymb then
    begin
      Exit(False);
    end;
    if R.FCoe.IsZero then
    begin
      Exit(False);
    end;
    R.FExp := R.FExp - Digit;
    R.FCoe.AbsoluteVal;
    if R.FExp > 0 then
    begin
      Exit(True);
    end;
    N.FromInteger(- R.FExp);
    R.FCoe.Div10PowN(N);
    Exit(not R.FCoe.IsZero);
  finally
    FreeAndNil(R);
    FreeAndNil(N);
  end;
  Exit(False);
end;

function TLongReal.Multiply(const Par: TLongReal): TLongReal;
begin
  FCoe.Multiply(Par.FCoe);
  FExp := FExp + Par.FExp;
  Normalize;
  Exit(Self);
end;

function TLongReal.Negative: TLongReal;
begin
  FCoe.Negative;
  Exit(Self);
end;

function TLongReal.Normalize: TLongReal;
var
  S: string;
  i, l: Integer;
  T: TLongInteger;
begin
  FCoe.Normalize;
  if FCoe.IsZero then
  begin
    FExp := 0;
    Exit(Self);
  end;
  S := FCoe.ToString10;
  l := S.Length;
  for i := 0 to l - 1 do if S.Chars[l - i - 1] <> '0' then
  begin
    T := TLongInteger.Create(i);
    try
      FCoe.FromString10(S.Substring(0, S.Length - i));
      FExp := FExp + i;
    finally
      FreeAndNil(T);
    end;
    Break;
  end;
  Exit(Self);
end;

function TLongReal.Positive: TLongReal;
begin
  Exit(Self);
end;

function TLongReal.Round(LI: TLongInteger): TLongInteger;
var
  LRA, LRB: TLongReal;
  LIOne: TLongInteger;
begin
  LRA := TLongReal.Create;
  LRB := TLongReal.Create;
  LIOne := TLongInteger.Create(1);
  try
    LRA.FCoe.CopyVal(Trunc(LI));
    LRA.FExp := 0;
    LRA.Subtract(Self).Positive;
    LI.Positive;
    LRB.FromInteger(5, -1);
    if (LRA.Equal(LRB) and LI.IsOdd) or LRA.GreaterThan(LRB) then
    begin
      LI.Add(LIOne);
    end;
    LI.FSymb := FCoe.FSymb;
  finally
    FreeAndNil(LRA);
    FreeAndNil(LRB);
    FreeAndNil(LIOne);
  end;
  Exit(LI);
end;

function TLongReal.Subtract(const Par: TLongReal): TLongReal;
var
  N: Integer;
  LI: TLongInteger;
  R: TLongReal;
begin
  R := TLongReal.Create;
  LI := TLongInteger.Create;
  try
    N := Abs(Par.FExp - FExp);
    LI.FromInteger(N);
    if Par.FExp > FExp then
    begin
      R.CopyVal(Par);
      FCoe.Subtract(R.FCoe.Mut10PowN(LI));
    end
    else
    begin
      FExp := Par.FExp;
      FCoe.Mut10PowN(LI).Subtract(Par.FCoe);
    end;
    Normalize;
  finally
    FreeAndNil(R);
    FreeAndNil(LI);
  end;
  Exit(Self);
end;

{$ZEROBASEDSTRINGS ON}
function TLongReal.ToStringDot: string;
var
  f: Boolean;
  s, t: string;
  i, n: Integer;
begin
  if FExp = 0 then
  begin
    Exit(FCoe.ToString10);
  end
  else if FExp > 0 then
  begin
    SetLength(s, FExp);
    for i := 0 to FExp - 1 do
    begin
      s[i] := '0';
    end;
    Exit(Format('%s%s', [FCoe.ToString10, s]));
  end
  else
  begin
    n := - FExp;
    s := FCoe.ToString10;
    if s.StartsWith('-') then
    begin
      f := False;
      s := s.Substring(1);
    end
    else
    begin
      f := True;
    end;
    if n = Length(s) then
    begin
      if f then
      begin
        Exit(Format('0.%s', [s]));
      end
      else
      begin
        Exit(Format('-0.%s', [s]));
      end;
    end
    else if n > Length(s) then
    begin
      n := n - Length(s);
      SetLength(t, n);
      for i := 0 to n - 1 do
      begin
        t[i] := '0';
      end;
      if f then
      begin
        Exit(Format('0.%s%s', [t, s]));
      end
      else
      begin
        Exit(Format('-0.%s%s', [t, s]));
      end;
    end
    else
    begin
      n := Length(s) - n;
      s := s.Insert(n, '.');if f then
      begin
        Exit(Format('%s', [s]));
      end
      else
      begin
        Exit(Format('-%s', [s]));
      end;
    end;
  end;
end;
{$ZEROBASEDSTRINGS OFF}

function TLongReal.ToStringOri: string;
begin
  Exit(Format('%se%s', [FCoe.ToString10, IntToStr(FExp)]));
end;

function TLongReal.Trunc(LI: TLongInteger): TLongInteger;
var
  LIA: TLongInteger;
begin
  LIA := TLongInteger.Create;
  try
    LI.CopyVal(FCoe);
    LIA.FromInteger(FExp).Positive;
    if FExp < 0 then
    begin
      LI.Div10PowN(LIA);
    end
    else
    begin
      LI.Mut10PowN(LIA);
    end;
  finally
    FreeAndNil(LIA);
  end;
  Exit(LI);
end;

function TLongReal.Zero: TLongReal;
begin
  FCoe.Zero;
  FExp := 0;
  Exit(Self);
end;

{ TComplexMA }

class operator TComplexMA.Add(const C1, C2: TComplexMA): TComplexMA;
var
  A, B: Double;
begin
  A := C1.FM * Cos(C1.FA) + C2.FM * Cos(C2.FA);
  B := C1.FM * Sin(C1.FA) + C2.FM * Sin(C2.FA);
  Result.FM := Sqrt(A * A + B * B);
  Result.FA := ArcTan2(B, A);
end;

function TComplexMA.Argument: Double;
begin
  Result := FA;
end;

function TComplexMA.Conjugate: TComplexMA;
begin
  Result.FM := FM;
  Result.FA := -FA;
end;

class function TComplexMA.Create(const Modulus, Argument: Double): TComplexMA;
begin
  Result.FM := Modulus;
  Result.FA := Argument;
end;

class operator TComplexMA.Divide(const C1: TComplexMA; const C2: Double): TComplexMA;
begin
  Result.FM := C1.FM / C2;
  Result.FA := C1.FA;
end;

class operator TComplexMA.Divide(const C1, C2: TComplexMA): TComplexMA;
begin
  Result.FM := C1.FM / C2.FM;
  Result.FA := C1.FA - C2.FA;
end;

function TComplexMA.Imaginary: Double;
begin
  Result := FM * Sin(FA);
end;

function TComplexMA.Modulus: Double;
begin
  Result := FM;
end;

class operator TComplexMA.Multiply(const C1: TComplexMA; const C2: Double): TComplexMA;
begin
  Result.FM := C1.FM * C2;
  Result.FA := C1.FA;
end;

class operator TComplexMA.Multiply(const C1, C2: TComplexMA): TComplexMA;
begin
  Result.FM := C1.FM * C2.FM;
  Result.FA := C1.FA + C2.FA;
end;

function TComplexMA.Real: Double;
begin
  Result := FM * Cos(FA);
end;

class operator TComplexMA.Subtract(const C1, C2: TComplexMA): TComplexMA;
var
  A, B: Double;
begin
  A := C1.FM * Cos(C1.FA) - C2.FM * Cos(C2.FA);
  B := C1.FM * Sin(C1.FA) - C2.FM * Sin(C2.FA);
  Result.FM := Sqrt(A * A + B * B);
  Result.FA := ArcTan2(B, A);
end;

{ TComplexRI }

class operator TComplexRI.Add(const C1, C2: TComplexRI): TComplexRI;
begin
  Result.FR := C1.FR + C2.FR;
  Result.FI := C1.FI + C2.FI;
end;

function TComplexRI.Argument: Double;
begin
  Exit(ArcTan2(FI, FR));
end;

function TComplexRI.Conjugate: TComplexRI;
begin
  Result.FR := FR;
  Result.FI := FI;
end;

class function TComplexRI.Create(const Real, Imaginary: Double): TComplexRI;
begin
  Result.FR := Real;
  Result.FI := Imaginary;
end;

class operator TComplexRI.Divide(const C1, C2: TComplexRI): TComplexRI;
var
  M: Double;
begin
  M := C2.FR * C2.FR + C2.FI * C2.FI;
  Result.FR := (C1.FR * C2.FR + C1.FI * C2.FI) / M;
  Result.FI := (C1.FI * C2.FR - C1.FR * C2.FI) / M;
end;

class operator TComplexRI.Divide(const C1: TComplexRI; const C2: Double): TComplexRI;
begin
  Result.FR := C1.FR / C2;
  Result.FI := C1.FI / C2;
end;

function TComplexRI.Imaginary: Double;
begin
  Result := FI;
end;

class operator TComplexRI.Multiply(const C1, C2: TComplexRI): TComplexRI;
begin
  Result.FR := C1.FR * C2.FR - C1.FI * C2.FI;
  Result.FI := C1.FR * C2.FI + C1.FI * C2.FR;
end;

function TComplexRI.Modulus: Double;
begin
  Result := Sqrt(FR * FR + FI * FI);
end;

class operator TComplexRI.Multiply(const C1: TComplexRI; const C2: Double): TComplexRI;
begin
  Result.FR := C1.FR * C2;
  Result.FI := C1.FI * C2;
end;

function TComplexRI.Real: Double;
begin
  Result := FR;
end;

class operator TComplexRI.Subtract(const C1, C2: TComplexRI): TComplexRI;
begin
  Result.FR := C1.FR - C2.FR;
  Result.FI := C1.FI - C2.FI;
end;

end.


