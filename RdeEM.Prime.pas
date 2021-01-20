{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{                    RdeEM Prime                        }
{                     素数单元                          }
{                     ver 1.20                          }
{                                                       }
{    Copyright(c) 2018-2019 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit RdeEM.Prime;

interface

uses
  System.SysUtils, RdeEM.Long;

  function GetRandomPrimeNumber(Digits: Integer; PrimeL: TArray<TLongInteger>): TLongInteger;

  function PrimeNumbersGenerate(Count: Integer): TArray<TLongInteger>;
  function PrimeNumbersGenerate10(Count: Integer): TArray<string>;
  function PrimeNumbersGenerate16(Count: Integer): TArray<string>;

  function MillerRabinTest(Base, Number: TLongInteger): Boolean;
  function PrimalityTest(Number: TLongInteger; Count: Integer): Boolean; overload;
  function PrimalityTest(Number: TLongInteger; List: TArray<TLongInteger>): Boolean; overload;

  function LIGCD(LI1, LI2: TLongInteger): TLongInteger;
  function LILCM(LI1, LI2: TLongInteger): TLongInteger;
  function LIEXGCD(LI1, LI2, LX, LY: TLongInteger): TLongInteger;
  function IsCoPrime(LI1, LI2: TLongInteger): Boolean;

  function EuclidAlgorithm(LA, LB: TLongInteger): TLongInteger;
  function EuclidAlgorithmEX(LA, LB, LX, LY: TLongInteger): TLongInteger;
  function IntegerInverse(LI, MO: TLongInteger): TLongInteger;

  procedure RSAParameter(P1, P2, E0: TLongInteger; var E, D, N: TLongInteger);

implementation

function GetRandomPrimeNumber(Digits: Integer; PrimeL: TArray<TLongInteger>): TLongInteger;
var
  Number, ST1, ST2, LI, LIOne: TLongInteger;
  i: Integer;
  IsPrime, S: Boolean;
begin
  Randomize;
  ST1 := TLongInteger.Create(2);
  ST2 := TLongInteger.Create(4);
  LI := TLongInteger.Create(6);
  Number := TLongInteger.Create;
  LIOne := TLongInteger.Create(1);
  try
    Number.FromRandomOdd(Digits - 3).Multiply(LI).Add(LIOne);
    S := False;
    repeat
      IsPrime := False;
      for i := 0 to Length(PrimeL) do
      begin
        if i = Length(PrimeL) then
        begin
          IsPrime := True;
          Break;
        end;
        if PrimeL[i].Equal(Number) then
        begin
          IsPrime := True;
          Break;
        end;
        if LI.CopyVal(Number).Modulus(PrimeL[i]).IsZero then
        begin
          IsPrime := False;
          Break;
        end;
        if not MillerRabinTest(PrimeL[i], Number) then
        begin
          IsPrime := False;
          Break;
        end;
      end;
      if not IsPrime then
      begin
        if S then
        begin
          Number.Add(ST1);
        end
        else
        begin
          Number.Add(ST2);
        end;
        S := not S;
        Continue;
      end;
    until IsPrime;
  finally
    FreeAndNil(ST1);
    FreeAndNil(ST2);
    FreeAndNil(LI);
    FreeAndNil(LIOne);
  end;
  Exit(Number);
end;

function PrimeNumbersGenerate(Count: Integer): TArray<TLongInteger>;
var
  LI1, LI2, LI3, LI4, LIOne: TLongInteger;
  SL: TArray<TLongInteger>;
  i, n: Integer;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  LI3 := TLongInteger.Create;
  LI4 := TLongInteger.Create;
  LIOne := TLongInteger.Create(1);
  SetLength(SL, Count);
  try
    if Count > 0 then
    begin
      SL[0] := TLongInteger.Create(2);
    end
    else
    begin
      Exit(SL);
    end;
    if Count > 1 then
    begin
      SL[1] := TLongInteger.Create(3);
    end
    else
    begin
      Exit(SL);
    end;
    if Count > 2 then
    begin
      SL[2] := TLongInteger.Create(5);
    end
    else
    begin
      Exit(SL);
    end;
    if Count > 3 then
    begin
      SL[3] := TLongInteger.Create(7);
    end
    else
    begin
      Exit(SL);
    end;
    n := 4;
    LI1.FromInteger(7);
    while n < Count do
    begin
      LI1.Add(LIOne);
      for i := 0 to n - 1 do
      begin
        LI2.CopyVal(LI1);
        LI3.CopyVal(SL[i]);
        LI4.CopyVal(LI3);
        if LI2.Modulus(LI3).IsZero then
        begin
          Break;
        end;
        if LI1.LessThan(LI4.Multiply(LI3)) then
        begin
          SL[n] := TLongInteger.Create(LI1);
          Inc(n);
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
    FreeAndNil(LI3);
    FreeAndNil(LI4);
    FreeAndNil(LIOne);
  end;
  Exit(SL);
end;

function PrimeNumbersGenerate10(Count: Integer): TArray<string>;
var
  LI1, LI2, LI3, LI4, LIOne: TLongInteger;
  SL: TArray<TLongInteger>;
  SA: TArray<string>;
  i, n: Integer;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  LI3 := TLongInteger.Create;
  LI4 := TLongInteger.Create;
  LIOne := TLongInteger.Create(1);
  SetLength(SL, Count);
  try
    if Count > 0 then
    begin
      SL[0] := TLongInteger.Create(2);
    end
    else
    begin
      SA[0] := '2';
      Exit(SA);
    end;
    if Count > 1 then
    begin
      SL[1] := TLongInteger.Create(3);
    end
    else
    begin
      SA[0] := '2';
      SA[1] := '3';
      Exit(SA);
    end;
    if Count > 2 then
    begin
      SL[2] := TLongInteger.Create(5);
    end
    else
    begin
      SA[0] := '2';
      SA[1] := '3';
      SA[2] := '5';
      Exit(SA);
    end;
    if Count > 3 then
    begin
      SL[3] := TLongInteger.Create(7);
    end
    else
    begin
      SA[0] := '2';
      SA[1] := '3';
      SA[2] := '5';
      SA[3] := '7';
      Exit(SA);
    end;
    n := 4;
    LI1.FromInteger(7);
    while n < Count do
    begin
      LI1.Add(LIOne);
      for i := 0 to n - 1 do
      begin
        LI2.CopyVal(LI1);
        LI3.CopyVal(SL[i]);
        LI4.CopyVal(LI3);
        if LI2.Modulus(LI3).IsZero then
        begin
          Break;
        end;
        if LI1.LessThan(LI4.Multiply(LI3)) then
        begin
          SL[n] := TLongInteger.Create(LI1);
          Inc(n);
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
    FreeAndNil(LI3);
    FreeAndNil(LI4);
    FreeAndNil(LIOne);
  end;
  for i := 0 to n - 1 do
  begin
    SA[i] := SL[i].ToString10;
  end;
  Exit(SA);
end;

function PrimeNumbersGenerate16(Count: Integer): TArray<string>;
var
  LI1, LI2, LI3, LI4, LIOne: TLongInteger;
  SL: TArray<TLongInteger>;
  SA: TArray<string>;
  i, n: Integer;
begin
  LI1 := TLongInteger.Create;
  LI2 := TLongInteger.Create;
  LI3 := TLongInteger.Create;
  LI4 := TLongInteger.Create;
  LIOne := TLongInteger.Create(1);
  SetLength(SL, Count);
  try
    if Count > 0 then
    begin
      SL[0] := TLongInteger.Create(2);
    end
    else
    begin
      SA[0] := '2';
      Exit(SA);
    end;
    if Count > 1 then
    begin
      SL[1] := TLongInteger.Create(3);
    end
    else
    begin
      SA[0] := '2';
      SA[1] := '3';
      Exit(SA);
    end;
    if Count > 2 then
    begin
      SL[2] := TLongInteger.Create(5);
    end
    else
    begin
      SA[0] := '2';
      SA[1] := '3';
      SA[2] := '5';
      Exit(SA);
    end;
    if Count > 3 then
    begin
      SL[3] := TLongInteger.Create(7);
    end
    else
    begin
      SA[0] := '2';
      SA[1] := '3';
      SA[2] := '5';
      SA[3] := '7';
      Exit(SA);
    end;
    n := 4;
    LI1.FromInteger(7);
    while n < Count do
    begin
      LI1.Add(LIOne);
      for i := 0 to n - 1 do
      begin
        LI2.CopyVal(LI1);
        LI3.CopyVal(SL[i]);
        LI4.CopyVal(LI3);
        if LI2.Modulus(LI3).IsZero then
        begin
          Break;
        end;
        if LI1.LessThan(LI4.Multiply(LI3)) then
        begin
          SL[n] := TLongInteger.Create(LI1);
          Inc(n);
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(LI1);
    FreeAndNil(LI2);
    FreeAndNil(LI3);
    FreeAndNil(LI4);
    FreeAndNil(LIOne);
  end;
  for i := 0 to n - 1 do
  begin
    SA[i] := SL[i].ToString16;
  end;
  Exit(SA);
end;

function MillerRabinTest(Base, Number: TLongInteger): Boolean;
var
  LID, LIB, LIT, LIPOne, LINOne: TLongInteger;
begin
  if Number.Digit = 2 then
  case Number.ToString16.Chars[0] of
    '0', '1': Exit(False);
    '2', '3': Exit(True);
  end;
  if (Base.Digit = 2) and (Base.IsEven) and (Number.IsOdd) then
  begin
    Exit(True);
  end;
  if Number.IsEven then
  begin
    Exit(False);
  end;
  LID := TLongInteger.Create(Number);
  LIB := TLongInteger.Create(Base);
  LIT := TLongInteger.Create;
  LIPOne := TLongInteger.Create(1);
  LINOne := TLongInteger.Create;
  try
    LID.Subtract(LIPOne);
    LINOne.CopyVal(LID);
    LID.ShiftR(1);
    LIT.CopyVal(LID);
    while LID.IsEven do
    begin
      LID.ShiftR(1);
    end;
    LIB.PowAndMod(LID, Number);
    if LIB.Equal(LIPOne) or LIB.Equal(LINOne) then
    begin
      Exit(True);
    end
    else while not LID.Equal(LIT) do
    begin
      LID.ShiftL(1);
      if LIB.CopyVal(Base).PowAndMod(LID, Number).Equal(LINOne) then
      begin
        Exit(True);
      end;
    end;
  finally
    FreeAndNil(LID);
    FreeAndNil(LIB);
    FreeAndNil(LIT);
    FreeAndNil(LIPOne);
    FreeAndNil(LINOne);
  end;
  Exit(False);
end;

function PrimalityTest(Number: TLongInteger; Count: Integer): Boolean;
var
  List: TArray<TLongInteger>;
  Base, LI: TLongInteger;
  i: Integer;
begin
  Base := TLongInteger.Create;
  LI := TLongInteger.Create;
  List := PrimeNumbersGenerate(Count);
  try
    for i := 0 to Count - 1 do
    begin
      Base.CopyVal(List[i]);
      if Base.Equal(Number) then
      begin
        Exit(True);
      end;
      if LI.CopyVal(Number).Modulus(Base).IsZero then
      begin
        Exit(False);
      end;
      if not MillerRabinTest(Base, Number) then
      begin
        Exit(False);
      end;
    end;
  finally
    FreeAndNil(LI);
    FreeAndNil(Base);
    for i := 0 to Count - 1 do
    begin
      FreeAndNil(List[i]);
    end;
  end;
  Exit(True);
end;

function PrimalityTest(Number: TLongInteger; List: TArray<TLongInteger>): Boolean;
var
  Base, LI: TLongInteger;
  i: Integer;
begin
  Base := TLongInteger.Create;
  LI := TLongInteger.Create;
  try
    for i := 0 to Length(List) - 1 do
    begin
      Base.CopyVal(List[i]);
      if Base.Equal(Number) then
      begin
        Exit(True);
      end;
      if LI.CopyVal(Number).Modulus(Base).IsZero then
      begin
        Exit(False);
      end;
      if not MillerRabinTest(Base, Number) then
      begin
        Exit(False);
      end;
    end;
  finally
    FreeAndNil(LI);
    FreeAndNil(Base);
  end;
  Exit(True);
end;

function LIGCD(LI1, LI2: TLongInteger): TLongInteger;
var
  LA, LB: TLongInteger;
begin
  LA := TLongInteger.Create;
  LB := TLongInteger.Create;
  try
    LA.CopyVal(LI1);
    LB.CopyVal(LI2);
    Exit(EuclidAlgorithm(LA, LB));
  finally
    FreeAndNil(LA);
    FreeAndNil(LB);
  end;
end;

function LILCM(LI1, LI2: TLongInteger): TLongInteger;
var
  LIX, LIY: TLongInteger;
begin
  LIY := LIGCD(LI1, LI2);
  LIX := TLongInteger.Create;
  try
    LIX.CopyVal(LI1).Multiply(LI2).Divide(LIY);
  except
    on EZeroError do
    begin
      FreeAndNil(LIY);
      Exit(LIX.Zero);
    end;
  end;
  FreeAndNil(LIY);
  Exit(LIX);
end;

function LIEXGCD(LI1, LI2, LX, LY: TLongInteger): TLongInteger;
var
  LA, LB: TLongInteger;
begin
  LA := TLongInteger.Create;
  LB := TLongInteger.Create;
  try
    LA.CopyVal(LI1);
    LB.CopyVal(LI2);
    Exit(EuclidAlgorithmEX(LA, LB, LX, LY));
  finally
    FreeAndNil(LA);
    FreeAndNil(LB);
  end;
end;

function IsCoPrime(LI1, LI2: TLongInteger): Boolean;
var
  LI, LA, LB, LIOne: TLongInteger;
begin
  LI := TLongInteger.Create;
  LA := TLongInteger.Create;
  LB := TLongInteger.Create;
  LIOne := TLongInteger.Create(1);
  try
    LA.CopyVal(LI1);
    LB.CopyVal(LI2); 
    repeat
      LI.CopyVal(LA).Modulus(LB);
      if not LI.IsZero then
      begin
        LA.CopyVal(LB);
        LB.CopyVal(LI);
      end;
    until LI.IsZero;
    LI.CopyVal(LB);
  except
    on EZeroError do
    begin
      FreeAndNil(LI);
      FreeAndNil(LA);
      FreeAndNil(LB);
      FreeAndNil(LIOne);
      Exit(False);
    end;
  end;       
  FreeAndNil(LA);
  FreeAndNil(LB);
  if LI.Equal(LIOne) then
  begin
    FreeAndNil(LI);
    FreeAndNil(LIOne);
    Exit(True);
  end
  else
  begin
    FreeAndNil(LI);
    FreeAndNil(LIOne);
    Exit(False);
  end;
end;

function EuclidAlgorithm(LA, LB: TLongInteger): TLongInteger;
begin
  if LB.IsZero then
  begin
    Exit(TLongInteger.Create(LA));
  end;
  Exit(EuclidAlgorithm(LB, LA.Modulus(LB)));
end;

function EuclidAlgorithmEX(LA, LB, LX, LY: TLongInteger): TLongInteger;
var
  RE, TE, TS: TLongInteger;
begin
  if LB.IsZero then
  begin
    LX.FromInteger(1);
    LY.Zero;
    Exit(TLongInteger.Create(LA));
  end;
  TS := TLongInteger.Create(LA);
  Re := EuclidAlgorithmEX(LB, TS.Modulus(LB), LY, LX);
  TE := TLongInteger.Create(LX);
  TS := TLongInteger.Create(LA);
  try
    LY.Subtract(TE.Multiply(TS.Divide(LB)));
  finally
    FreeAndNil(TE);
    FreeAndNil(TS);
  end;
  Exit(RE);
end;

function IntegerInverse(LI, MO: TLongInteger): TLongInteger;
var
  LX, LY, GCD: TLongInteger;
begin
  LX := TLongInteger.Create;
  LY := TLongInteger.Create;
  try
    GCD := LIEXGCD(LI, MO, LX, LY);
    if (GCD.Digit = 1) and (GCD.GetValue(0) = True) then
    begin
      Exit(TLongInteger.Create(LX).Modulus(MO).Add(MO).Modulus(MO));
    end
    else
    begin
      Exit(nil);
    end;
  finally
    FreeAndNil(LX);
    FreeAndNil(LY);
    FreeAndNil(GCD);
  end;
end;

procedure RSAParameter(P1, P2, E0: TLongInteger; var E, D, N: TLongInteger);
var
  L, T1, T2, LIOne, LITwo: TLongInteger;
begin
  T1 := TLongInteger.Create;
  T2 := TLongInteger.Create;
  LIOne := TLongInteger.Create(1);
  LITwo := TLongInteger.Create(2);
  try
    N.CopyVal(P1).Multiply(P2);
    T1.CopyVal(P1).Subtract(LIOne);
    T2.CopyVal(P2).Subtract(LIOne);
    L := LILCM(T1, T2);
    T1.CopyVal(L).Subtract(LITwo);
    E.CopyVal(E0);
    while not E.LessThan(L) do
    begin
      E.ShiftR(1);
    end;
    while not IsCoPrime(E, L) do
    begin
      E.Add(LIOne);
    end;
    D := IntegerInverse(E, L);
  finally
    FreeAndNil(T1);
    FreeAndNil(T2);
    FreeAndNil(LIOne);
    FreeAndNil(LITwo);
  end;
end;

end.
