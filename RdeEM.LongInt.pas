{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{                   RdeEM LongInt                       }
{                     �󔵆�Ԫ                          }
{                     ver 1.21                          }
{                                                       }
{    Copyright(c) 2018-2019 Reniasty de El Magnifico    }
{                   �����̓ ��Ʒ                       }
{                 All rights reserved                   }
{                   �������Й���                        }
{                                                       }
{*******************************************************}

unit RdeEM.LongInt;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.Generics.Collections, System.Math;

type
  EZeroError = class(Exception);
  EPowerError = class(Exception);

  TLongInt = record
  private
    FSymb: Boolean; // True-������False-ؓ����
    FNumL: TArray<Boolean>;
    //
    function IsZero: Boolean;
    function AbstractValue: TLongInt;
    // �����Ąt�\��
    class function OriAdd(LongIntA, LongIntB: TLongInt): TLongInt; static; // ��̖��A��
    class function OriSubtract(LongIntA, LongIntB: TLongInt): TLongInt; static; // �޽^��ֵA���B����̖��A��
    class function OriMultiply(LongIntA, LongIntB: TLongInt): TLongInt; static;
  public
    // ���^�\��
    class operator Equal(LongIntA, LongIntB: TLongInt): Boolean;
    class operator NotEqual(LongIntA, LongIntB: TLongInt): Boolean;
    class operator LessThan(LongIntA, LongIntB: TLongInt): Boolean;
    class operator GreaterThan(LongIntA, LongIntB: TLongInt): Boolean;
    class operator LessThanOrEqual(LongIntA, LongIntB: TLongInt): Boolean;
    class operator GreaterThanOrEqual(LongIntA, LongIntB: TLongInt): Boolean;
    // ��λ�\��
    class operator LeftShift(LongIntA: TLongInt; IntB: Integer): TLongInt;
    class operator RightShift(LongIntA: TLongInt; IntB: Integer): TLongInt;
    // ��λ�\�㣬�\��Y����ȡ��ֵ
    class operator LogicalNot(LongIntA: TLongInt): TLongInt;
    class operator LogicalAnd(LongIntA, LongIntB: TLongInt): TLongInt;
    class operator LogicalOr(LongIntA, LongIntB: TLongInt): TLongInt;
    class operator LogicalXor(LongIntA, LongIntB: TLongInt): TLongInt;
    // ��ؓ�\��
    class operator Negative(LongIntA: TLongInt): TLongInt;
    class operator Positive(LongIntA: TLongInt): TLongInt;
    // �Ąt�\��
    class operator Add(LongIntA, LongIntB: TLongInt): TLongInt;
    class operator Subtract(LongIntA, LongIntB: TLongInt): TLongInt;
    class operator Multiply(LongIntA, LongIntB: TLongInt): TLongInt;

    // ���N����
    class function Create: TLongInt; static;

  end;

implementation

{ TLongInt }

class operator TLongInt.LogicalAnd(LongIntA, LongIntB: TLongInt): TLongInt;
var
  l, m, n, i: Integer;
  a, b: Boolean;
begin
  m := Length(LongIntA.FNumL);
  n := Length(LongIntB.FNumL);
  if n < m then
  begin
    l := m;
  end
  else
  begin
    l := n;
  end;
  with Result do if l = 0 then
  begin
    FSymb := True;
    SetLength(FNumL, 1);
    FNumL[0] := False;
  end
  else
  begin
    FSymb := True;
    SetLength(FNumL, l);
    for i := 0 to l - 1 do
    begin
      if i >= m then
      begin
        a := False;
      end
      else
      begin
        a := LongIntA.FNumL[i];
      end;
      if i >= n then
      begin
        b := False;
      end
      else
      begin
        b := LongIntB.FNumL[i];
      end;
      FNumL[i] := a and b;
    end;
  end;
end;

class operator TLongInt.LogicalNot(LongIntA: TLongInt): TLongInt;
var
  i, l: Integer;
begin
  l := Length(LongIntA.FNumL);
  with Result do if l = 0 then
  begin
    FSymb := True;
    SetLength(FNumL, 1);
    FNumL[0] := False;
  end
  else
  begin
    FSymb := True;
    SetLength(FNumL, l);
    for i := 0 to l - 1 do
    begin
      FNumL[i] := not FNumL[i];
    end;
  end;
end;

class operator TLongInt.LogicalOr(LongIntA, LongIntB: TLongInt): TLongInt;
var
  l, m, n, i: Integer;
  a, b: Boolean;
begin
  m := Length(LongIntA.FNumL);
  n := Length(LongIntB.FNumL);
  if n < m then
  begin
    l := m;
  end
  else
  begin
    l := n;
  end;
  with Result do if l = 0 then
  begin
    FSymb := True;
    SetLength(FNumL, 1);
    FNumL[0] := False;
  end
  else
  begin
    FSymb := True;
    SetLength(FNumL, l);
    for i := 0 to l - 1 do
    begin
      if i >= m then
      begin
        a := False;
      end
      else
      begin
        a := LongIntA.FNumL[i];
      end;
      if i >= n then
      begin
        b := False;
      end
      else
      begin
        b := LongIntB.FNumL[i];
      end;
      FNumL[i] := a or b;
    end;
  end;
end;

class operator TLongInt.LogicalXor(LongIntA, LongIntB: TLongInt): TLongInt;
var
  l, m, n, i: Integer;
  a, b: Boolean;
begin
  m := Length(LongIntA.FNumL);
  n := Length(LongIntB.FNumL);
  if n < m then
  begin
    l := m;
  end
  else
  begin
    l := n;
  end;
  with Result do if l = 0 then
  begin
    FSymb := True;
    SetLength(FNumL, 1);
    FNumL[0] := False;
  end
  else
  begin
    FSymb := True;
    SetLength(FNumL, l);
    for i := 0 to l - 1 do
    begin
      if i >= m then
      begin
        a := False;
      end
      else
      begin
        a := LongIntA.FNumL[i];
      end;
      if i >= n then
      begin
        b := False;
      end
      else
      begin
        b := LongIntB.FNumL[i];
      end;
      FNumL[i] := a xor b;
    end;
  end;
end;

class operator TLongInt.Multiply(LongIntA, LongIntB: TLongInt): TLongInt;
var
  ALI, BLI, CLI, DLI, XLI, YLI, ZLI: TLongInt;
  i, n: Integer;
  S: Boolean;
begin
  if LongIntA.IsZero or LongIntB.IsZero then
  begin
    Exit(TLongInt.Create);
  end;
  S := not (LongIntA.FSymb xor LongIntB.FSymb);
  if Length(LongIntA.FNumL) > Length(LongIntB.FNumL) then
  begin
    n := (Length(LongIntA.FNumL) + 1) div 2;
  end
  else
  begin
    n := (Length(LongIntB.FNumL) + 1) div 2;
  end;
  SetLength(ALI.FNumL, n);
  SetLength(BLI.FNumL, n);
  SetLength(CLI.FNumL, n);
  SetLength(DLI.FNumL, n);
  for i := 0 to n - 1 do
  begin
    if i < Length(LongIntA.FNumL) then
    begin
      BLI.FNumL[i] := LongIntA.FNumL[i];
    end
    else
    begin
      BLI.FNumL[i] := False;
    end;
    if i < Length(LongIntB.FNumL) then
    begin
      DLI.FNumL[i] := LongIntB.FNumL[i];
    end
    else
    begin
      DLI.FNumL[i] := False;
    end;
  end;
  for i := n to 2 * n - 1 do
  begin
    if i < Length(LongIntA.FNumL) then
    begin
      ALI.FNumL[i] := LongIntA.FNumL[i];
    end
    else
    begin
      ALI.FNumL[i] := False;
    end;
    if i < Length(LongIntB.FNumL) then
    begin
      CLI.FNumL[i] := LongIntB.FNumL[i];
    end
    else
    begin
      CLI.FNumL[i] := False;
    end;
  end;
  XLI := OriMultiply(ALI, CLI);
  YLI := OriMultiply(BLI, DLI);
  ZLI := OriMultiply(ALI - BLI, DLI - CLI) + XLI + YLI;
  YLI := YLI + (XLI shl n) + (ZLI shl (2 * n));
  YLI.FSymb := S;
  Exit(YLI);
end;

class operator TLongInt.Negative(LongIntA: TLongInt): TLongInt;
begin
  with Result do
  begin
    FSymb := not LongIntA.FSymb;
    FNumL := Copy(LongIntA.FNumL, 0, Length(LongIntA.FNumL));
  end;
end;

class operator TLongInt.NotEqual(LongIntA, LongIntB: TLongInt): Boolean;
begin
  Exit(not (LongIntA = LongIntB));
end;

class function TLongInt.OriAdd(LongIntA, LongIntB: TLongInt): TLongInt;
var
  Car, X, Y: TLongInt; // �քeӛ�h�o�Mλ�ͼ��H�Mλ��
begin
  Result := LongIntA xor LongIntB;
  Car := (LongIntA and LongIntB) shl 1;
  while not Car.IsZero do
  begin
    X := Result;
    Y := Car;
    Result := X xor Y;
    Car := (X and Y) shl 1;
  end;
  Result.FSymb := LongIntA.FSymb;
end;

class function TLongInt.OriMultiply(LongIntA, LongIntB: TLongInt): TLongInt;
var
  i, n: Integer;
  A: TLongInt;
  S: Boolean;
begin
  S := not (LongIntA.FSymb xor LongIntB.FSymb);
  A := LongIntA.AbstractValue;
  n := 0;
  for i := 0 to Length(LongIntB.FNumL) - 1 do if LongIntB.FNumL[i] then
  begin
    Result := Result + A shl (i - n);
    n := i;
  end;
  Result.FSymb := S;
end;

class function TLongInt.OriSubtract(LongIntA, LongIntB: TLongInt): TLongInt;
var
  Car, X, Y: TLongInt; // �քeӛ�h�o�Mλ��H��λ��
begin
  if LongIntA <= LongIntB then
  begin
    Result := TLongInt.Create;
  end
  else
  begin
    Result := LongIntA xor LongIntB;
    Car := (not LongIntA and LongIntB) shl 1;
    while not Car.IsZero do
    begin
      X := Result;
      Y := Car;
      Result := X xor Y;
      Car := (not X and Y) shl 1;
    end;
    Result.FSymb := LongIntA.FSymb;
  end;
end;

class operator TLongInt.Positive(LongIntA: TLongInt): TLongInt;
begin
  with Result do
  begin
    FSymb := LongIntA.FSymb;
    FNumL := Copy(LongIntA.FNumL, 0, Length(LongIntA.FNumL));
  end;
end;

function TLongInt.AbstractValue: TLongInt;
begin
    Result.FSymb := True;
    Result.FNumL := Copy(FNumL, 0, Length(FNumL));
end;

class operator TLongInt.Add(LongIntA, LongIntB: TLongInt): TLongInt;
begin
  if LongIntA.IsZero then
  begin
    Exit(+ LongIntB);
  end;
  if LongIntB.IsZero then
  begin
    Exit(+ LongIntA);
  end;
  if LongIntA.FSymb xor LongIntB.FSymb then
  begin
    Exit(LongIntA - (- LongIntB));
  end
  else
  begin
    Exit(OriAdd(LongIntA, LongIntB));
  end;
end;

class function TLongInt.Create: TLongInt;
begin
  with Result do
  begin
    FSymb := True;
    SetLength(FNumL, 1);
    FNumL[0] := False;
  end;
end;

class operator TLongInt.Equal(LongIntA, LongIntB: TLongInt): Boolean;
var
  l, n, m, i: Integer;
  a, b: Boolean;
begin
  if LongIntA.IsZero and LongIntB.IsZero then
  begin
    Exit(True);
  end;
  if LongIntA.FSymb xor LongIntB.FSymb then
  begin
    Exit(False);
  end;
  m := Length(LongIntA.FNumL);
  n := Length(LongIntB.FNumL);
  if m < n then
  begin
    l := n;
  end
  else
  begin
    l := m;
  end;
  for i := 0 to l - 1 do
  begin
    if i >= m then
    begin
      a := False;
    end
    else
    begin
      a := LongIntA.FNumL[i];
    end;
    if i >= n then
    begin
      b := False;
    end
    else
    begin
      b := LongIntB.FNumL[i];
    end;
    if a xor b then
    begin
      Exit(False);
    end;
  end;
  Exit(True);
end;

class operator TLongInt.GreaterThan(LongIntA, LongIntB: TLongInt): Boolean;
var
  l, n, m, i: Integer;
  a, b: Boolean;
begin
  if LongIntA.IsZero and LongIntB.IsZero then
  begin
    Exit(False);
  end;
  if LongIntA.IsZero then
  begin
    Exit(not LongIntB.FSymb);
  end;
  if LongIntB.IsZero then
  begin
    Exit(LongIntA.FSymb);
  end;
  if LongIntA.FSymb xor LongIntB.FSymb then
  begin
    Exit(LongIntA.FSymb);
  end;
  m := Length(LongIntA.FNumL);
  n := Length(LongIntB.FNumL);
  if m < n then
  begin
    l := n;
  end
  else
  begin
    l := m;
  end;
  for i := l - 1 downto 0 do
  begin
    if i >= m then
    begin
      a := False;
    end
    else
    begin
      a := LongIntA.FNumL[i];
    end;
    if i >= n then
    begin
      b := False;
    end
    else
    begin
      b := LongIntB.FNumL[i];
    end;
    if a xor b then
    begin
      Exit(a);
    end;
  end;
  Exit(False);
end;

class operator TLongInt.GreaterThanOrEqual(LongIntA, LongIntB: TLongInt): Boolean;
begin
  Exit(not (LongIntA < LongIntB));
end;

function TLongInt.IsZero: Boolean;
var
  i: Integer;
begin
  if Length(FNumL) = 0 then
  begin
    Exit(True);
  end;
  for i := 0 to Length(FNumL) - 1 do if FNumL[i] = True then
  begin
    Exit(False);
  end;
  Exit(True);
end;

class operator TLongInt.LeftShift(LongIntA: TLongInt; IntB: Integer): TLongInt;
var
  i: Integer;
begin
  with Result do
  begin
    SetLength(FNumL, Length(LongIntA.FNumL) + IntB);
    FSymb := LongIntA.FSymb;
    for i := 0 to IntB - 1 do
    begin
      FNumL[i] := False;
    end;
    for i := 0 to Length(LongIntA.FNumL) - 1 do
    begin
      FNumL[i + IntB] := LongIntA.FNumL[i];
    end;
  end;
end;

class operator TLongInt.LessThan(LongIntA, LongIntB: TLongInt): Boolean;
var
  l, n, m, i: Integer;
  a, b: Boolean;
begin
  if LongIntA.IsZero and LongIntB.IsZero then
  begin
    Exit(False);
  end;
  if LongIntA.IsZero then
  begin
    Exit(LongIntB.FSymb);
  end;
  if LongIntB.IsZero then
  begin
    Exit(not LongIntA.FSymb);
  end;
  if LongIntA.FSymb xor LongIntB.FSymb then
  begin
    Exit(LongIntB.FSymb);
  end;
  m := Length(LongIntA.FNumL);
  n := Length(LongIntB.FNumL);
  if m < n then
  begin
    l := n;
  end
  else
  begin
    l := m;
  end;
  for i := l - 1 downto 0 do
  begin
    if i >= m then
    begin
      a := False;
    end
    else
    begin
      a := LongIntA.FNumL[i];
    end;
    if i >= n then
    begin
      b := False;
    end
    else
    begin
      b := LongIntB.FNumL[i];
    end;
    if a xor b then
    begin
      Exit(b);
    end;
  end;
  Exit(False);
end;

class operator TLongInt.LessThanOrEqual(LongIntA, LongIntB: TLongInt): Boolean;
begin
  Exit(not (LongIntA > LongIntB));
end;

class operator TLongInt.RightShift(LongIntA: TLongInt; IntB: Integer): TLongInt;
var
  i: Integer;
begin
  with Result do if Length(LongIntA.FNumL) <= IntB then
  begin
    FSymb := True;
    SetLength(FNumL, 1);
    FNumL[0] := False;
  end
  else
  begin
    SetLength(FNumL, Length(LongIntA.FNumL) - IntB);
    FSymb := LongIntA.FSymb;
    for i := IntB to Length(LongIntA.FNumL) - 1 do
    begin
      FNumL[i - IntB] := LongIntA.FNumL[i];
    end;
  end;
end;

class operator TLongInt.Subtract(LongIntA, LongIntB: TLongInt): TLongInt;
begin
  if LongIntA.IsZero then
  begin
    Exit(- LongIntB);
  end;
  if LongIntB.IsZero then
  begin
    Exit(+ LongIntA);
  end;
  if LongIntA = LongIntB then
  begin
    Exit(TLongInt.Create);
  end;
  if LongIntA.FSymb xor LongIntB.FSymb then
  begin
    Exit(LongIntA + (- LongIntB));
  end;
  if LongIntA.FSymb then
  begin
    if LongIntA > LongIntB then
    begin
      Exit(OriSubtract(LongIntA, LongIntB));
    end
    else
    begin
      Exit(- OriSubtract(LongIntB, LongIntA));
    end;
  end
  else
  begin
    if LongIntA > LongIntB then
    begin
      Exit(OriSubtract(- LongIntB, - LongIntA));
    end
    else
    begin
      Exit(- OriSubtract(LongIntA, LongIntB));
    end;
  end;
end;

end.
