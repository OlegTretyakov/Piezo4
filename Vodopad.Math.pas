unit Vodopad.Math;

interface
uses System.SysUtils;

function BytesToHexStr(const bytes: TBytes): string; overload;
function BytesToHexStr(const bytes: Array of byte): string; overload;
function BytesToHexStr(const Buffer; Count : Word): string; overload;
function FixedTrunc(AValue : Double):integer;

(*function GetBitState(Src: Word; bit: byte): Boolean;overload;{bit num is zero-based}
function SetBit(Src: Word; bit: byte): Word;overload;{bit num is zero-based}
function ResetBit(Src: Word; bit: byte): Word;overload;{bit num is zero-based}
function InvertBit(Src: Word; bit: byte): Word;overload;{bit num is zero-based} *)

(*function GetBitState(Src: LongWord; bit: byte): Boolean;overload;{bit num is zero-based}
function SetBit(Src: LongWord; bit: byte): LongWord;overload;{bit num is zero-based}
function ResetBit(Src: LongWord; bit: byte): LongWord;overload;{bit num is zero-based}
function InvertBit(Src: LongWord; bit: byte): LongWord;overload;{bit num is zero-based} *)

function GetBitState(Src: Integer; bit: byte): Boolean;//overload;{bit num is zero-based}
function SetBit(Src: Integer; bit: byte): Integer;//overload;{bit num is zero-based}
function ResetBit(Src: Integer; bit: byte): Integer;//overload;{bit num is zero-based}
function InvertBit(Src: Integer; bit: byte): Integer;//overload;{bit num is zero-based}

function DoubleArrToStr(const Arr: Array of double): string;
function ZeroStdDev(const Arr: Array of double): double;
function StdDevFixed(const Arr: Array of double): double;
function StdDevFixedPpm(const Arr: Array of double; ARefFreq : double): double;
function HexChar(c: Char): Byte;
function HexToByte(source: Pchar): TBytes;
procedure SecondToTime(ASeconds : Int64; out Hours : Word;  out Minutes : byte; out Seconds : byte);
procedure SecondToMinSec(ASeconds : Cardinal; out Minutes : Word; out Seconds : byte);
function Equals(const A, B, Prec : double): boolean; 
function ppm(AFreq, ARefFreq : double):double;
function ppb(AFreq, ARefFreq : double):double;
function Absppm(AFreq, ARefFreq : double):double;
function PpmToFreq(Ppm, ARefFreq : double):double;

implementation

function BytesToHexStr(const bytes: TBytes): string; overload;
var
 i : integer;
begin
  Result := '';
  for I := 0 to high(bytes) do
    Result :=  Result + Format('%x' + '%x ', [(bytes[i] and $F0) shr 4, bytes[i] and $0F]);
end;

function BytesToHexStr(const bytes: Array of byte): string; overload;
var
 i1 : integer;
begin
  Result := '';
  for i1 := 0 to high(bytes) do
    Result :=  Result + Format('%x' + '%x ', [(bytes[i1] and $F0) shr 4, bytes[i1] and $0F]);
end; 

function BytesToHexStr(const Buffer; Count : Word): string; overload;
var
 i : Word;
 pba : pByteArray;
 xs : string;
begin 
  pba := pByteArray(@Buffer);
  i := 0;
  SetLength(result, Count * 3);
  FillChar(result[1], Count * 3, 0);
  while i < Count do
  begin
    FmtStr(xs, '%x' + '%x ', [(pba^[i] and $F0) shr 4, pba^[i] and $0F]);
    move(xs[1], result[1 + i * 3], 3);
    inc(i);
  end;
end;

function FixedTrunc(AValue : Double):integer;
var vFrac : Double;
vRes : Int64;
begin
  vFrac := Abs(Frac(AValue));
  if vFrac > 0.4999 then
  begin
    if (AValue > 0) then
      vRes := Trunc(AValue)+1
    else
      vRes := Trunc(AValue)-1;
  end else
    vRes := Trunc(AValue);
  result := integer(vRes);
end;

procedure SecondToTime(ASeconds : Int64; out Hours : Word;  out Minutes : byte; out Seconds : byte);
begin
  Minutes := 0;
  Hours := 0;
  if (ASeconds > 59) then
  begin
    Hours := ASeconds div 3600;
    Minutes := (ASeconds - Hours * 3600) div 60;
    Seconds := ASeconds - Hours * 3600 - Minutes * 60;
  end else
    Seconds := ASeconds;
end;

procedure SecondToMinSec(ASeconds : Cardinal; out Minutes : Word; out Seconds : byte);
begin
  Minutes := 0;
  if (ASeconds > 59) then
  begin
    Minutes := ASeconds  div 60;
    Seconds := ASeconds - Minutes * 60;
  end else
    Seconds := ASeconds;
end;

(*function GetBitState(Src: Word; bit: byte): Boolean;overload;
begin
  result := (Src and (1 shl bit)) <> 0;
end;

function SetBit(Src: Word; bit: byte): Word;overload;
begin
  Result := Src or (1 shl Bit);
end;

function ResetBit(Src: Word; bit: byte): Word;overload;
begin
  Result := Src and not (1 shl Bit);
end;

function InvertBit(Src: Word; bit: byte): Word;overload;
begin
  Result := Src xor (1 shl Bit);
end;

function GetBitState(Src: LongWord; bit: byte): Boolean;overload;
begin
  result := (Src and (1 shl bit)) <> 0;
end;

function SetBit(Src: LongWord; bit: byte): LongWord;overload;
begin
  Result := Src or (1 shl Bit);
end;

function ResetBit(Src: LongWord; bit: byte): LongWord;overload;
begin
  Result := Src and not (1 shl Bit);
end;

function InvertBit(Src: LongWord; bit: byte): LongWord;overload;
begin
  Result := Src xor (1 shl Bit);
end;  *)

function GetBitState(Src: Integer; bit: byte): Boolean;//overload;
begin
  result := (Src and (1 shl bit)) <> 0;
end;

function SetBit(Src: Integer; bit: byte): Integer;//overload;
begin
  Result := Src or (1 shl Bit);
end;

function ResetBit(Src: Integer; bit: byte): Integer;//overload;
begin
  Result := Src and not (1 shl Bit);
end;

function InvertBit(Src: Integer; bit: byte): Integer;//overload;
begin
  Result := Src xor (1 shl Bit);
end;


function DoubleArrToStr(const Arr: Array of double): string;
var
 i : integer;
begin
  Result := '';
  for I := low(Arr) to high(Arr) do
    Result :=  Result + Format('%f ', [Arr[i]]);
end;

function StdDevFixed(const Arr: Array of double): double;
var vSumm, vDev : double;
  i : integer;
begin
  vSumm := 0;
  result := 0;
  if (Length(Arr) > 1) then
  begin
    for I := Low(Arr) to High(Arr) do
    vSumm := vSumm + Arr[i];
    vDev := vSumm / Length(Arr);
    vSumm := 0;
    for I := Low(Arr) to High(Arr) do
      vSumm := vSumm + (vDev - Arr[i]) * (vDev - Arr[i]);
    Result := Sqrt(vSumm / (Length(Arr)-1));
  end;
end;

function StdDevFixedPpm(const Arr: Array of double; ARefFreq : double): double;
var vSumm, vDev : double;
  i : integer;
begin
  vSumm := 0;
  result := 0;
  if (Length(Arr) > 1) then
  begin
    for I := Low(Arr) to High(Arr) do
    vSumm := vSumm + AbsPpm(Arr[i], ARefFreq);
    vDev := vSumm / Length(Arr);
    vSumm := 0;
    for I := Low(Arr) to High(Arr) do
      vSumm := vSumm + (vDev - AbsPpm(Arr[i], ARefFreq)) * (vDev - AbsPpm(Arr[i], ARefFreq));
    Result := Sqrt(vSumm / (Length(Arr)-1));
  end;
end;

function ZeroStdDev(const Arr: Array of double): double;
var vSumm: double;
  i : integer;
begin
  vSumm := 0;
  result := 0;
  if (Length(Arr) > 1) then
  begin
    for I := Low(Arr) to High(Arr) do
      vSumm := vSumm + (Arr[i]* Arr[i]);
    Result := Sqrt(vSumm / (Length(Arr)-1));
  end;
end;

function HexChar(c: Char): Byte;
begin
  case c of
    '0'..'9':  Result := Byte(c) - Byte('0');
    'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
    'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
  else
    Result := 0;
  end;
end;

function HexToByte(source: Pchar): TBytes;
var k:integer;
begin
 SetLength(result, Round(Length(source)/2));
 k := 0;
 while k < Length(source) do
  begin
    result[Round(k/2)] := HexChar(source[k]) shl 4 + HexChar(source[k+1]);// + ' ';
    Inc(k, 2);
  end;
end;

function Equals(const A, B, Prec : double): boolean;
begin
  result := (abs(A - B) < Prec)
end;

function ppm(AFreq, ARefFreq : double):double;
begin
  if (ARefFreq < 0.0000001) then
    ARefFreq := 0.0000001;
  result := (AFreq - ARefFreq)/ARefFreq * 1000000;
end;

function PpmToFreq(Ppm, ARefFreq : double):double;
begin
  result := Ppm / 1000000 * ARefFreq + ARefFreq
end;

function ppb(AFreq, ARefFreq : double):double;
begin
  if (ARefFreq < 0.0000001) then
    ARefFreq := 0.0000001;
  result := (AFreq - ARefFreq)/ARefFreq * 1000000000;
end;

function Absppm(AFreq, ARefFreq : double):double;
begin
  result := abs(ppm(AFreq, ARefFreq));
end;

end.