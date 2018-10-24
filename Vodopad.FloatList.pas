unit Vodopad.FloatList;

interface

uses System.Classes;

const
  (* Минимальное значение для типа double *)
  _FLOAT_MIN_ = -1.7e308;//-1.1E4932;

  (* Максимальное значение для типа double *)
  _FLOAT_MAX_ = 1.7e308;//1.1E4932;   }

  (* Точность в рабочих вычислениях *)
  _EPSILON_ = 0.00001;

  (* Константа возвращаемая при успешном завершении функции *)
  _OK_ = 1;

  (* Константа возвращаемая при неудачном завершении функции *)
  _ERROR_ = 0;

type

  (* Класс генерации exception при переполнении списка *)
  EOutOfRange = class(EListError);

  (* Класс обеспечивает создание, удаление, вставку и доступ к элементам динами-
     ческого списка вещественных чисел.
     Дополнительно поддерживается сортировка списка, поиск минимального и макси-
     мального значений в списке.
     Особенностью реализации списка является введение понятия несуществующего зна-
     чения "property Null". Данное свойство определяет значение, которое не участ-
     вует в операциях получения min и max списка.
     Второй особенностью списка является работа с определенной точностью, значение
     выведено в константу _EPSILON_.
     Поиск и сортировка осуществляются без использования свойства NULL и _EPSILON_
  *)

  TFloatListCompare = function (Item1, Item2, Prec: Double): Integer;
  TxFloatList = class(TPersistent)
  private
    FList: TList;
    FDuplicates: TDuplicates;
    FNULL: double;
    FMin: double;
    FMax: double;
    FSizeOfFloat: integer;
    FSorted: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetCount(): integer;
    function GetItem(Index: integer): double;
    procedure SetItem(Index: integer; Value: double); virtual;
    procedure SetMin(Value: double);
    procedure SetMax(Value: double);
    procedure Sort(); virtual;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure ReadMin(Reader: TReader);
    procedure WriteMin(Writer: TWriter);
    procedure ReadMax(Reader: TReader);
    procedure WriteMax(Writer: TWriter);
    procedure ReadFloats(Reader: TReader);
    procedure WriteFloats(Writer: TWriter);
    procedure SetSorted(Value: Boolean);
    procedure QuickSort(L, R: integer);
    function Find(N, Prec: double; var Index: integer): Boolean; virtual;
    function FindInUnsorted(N, Prec: double; var Index: integer): Boolean;
    function Add(Value: double): integer; virtual;
    procedure AddFloats(List: TxFloatList); virtual;
    function Sum : double;
    procedure Assign(Source: TPersistent); override;
    procedure Clear(); virtual;
    procedure Delete(Index: integer); virtual;
    function Equals(List: TxFloatList; Prec: double): Boolean;
    procedure Exchange(Index1, Index2: integer); virtual;
    function IndexOf(N, Prec: double): integer; virtual;
    procedure Insert(Index: integer; Value: double); virtual;
    (* Помещает пустые значения в список начиная с позиции iFirst в количестве iCount *)
    function InsertNulls(iFirst, iCount: integer; _null: single): integer;
    procedure Move(CurIndex, NewIndex: integer); virtual;
    // определение max среди хранимых данных
    function FindMax(): double;
    // определение min среди хранимых данных
    function FindMin(): double;
    (* Заменяет все отрицательные значения на нулевое *)
    function ReplaceNegativeToNULL(): integer;
    (* Заменяет все значения ThisValue на ToValue, с точностью Prec *)
    function ReplaceValToVal(ThisValue, ToValue, Prec: double): integer;
    function ReplaceGreateToVal(ThisValue, ToValue, Prec: double): integer;
    function ReplaceLessToVal(ThisValue, ToValue, Prec: double): integer;
    (* Инвертирует знак всех значений*)
    function InvertValues(): integer;
    (* Меняет, инвертирует порядок всех элементов в списке *)
    function Reverse(): integer;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Count: integer read GetCount;
    property Items[Index: integer]: double read GetItem write SetItem; default;
    property Min: double read FMin write SetMin;
    property Max: double read FMax write SetMax;
    property Null: double read FNULL write FNULL;
    property Sorted: Boolean read FSorted write SetSorted;
    property List: TList read FList;
  end;

function SortFloatListAsc(Item1, Item2, Prec: Double): Integer;
function SortFloatListDsc(Item1, Item2, Prec: Double): Integer;
procedure SortFloatList(List : TxFloatList; Prec: Double; Compare: TFloatListCompare);
procedure QuickSortFloatList(SortList: TxFloatList; L, R: Integer; Prec: Double;
  SCompare: TFloatListCompare);

function ZeroStdDev(Arg: TxFloatList): double;
function StdDevFixed(Arr: TxFloatList): double; 
function StdDevFixedPpm(Arr: TxFloatList; ARefFreq : double): double;
  (********************************************************************)
implementation

uses //WinTypes,
Math, Vodopad.Math;



procedure QuickSortFloatList(SortList: TxFloatList; L, R: Integer; Prec: Double;
  SCompare: TFloatListCompare);
var
  I, J: Integer;
  P, T: Double;
begin
  repeat
    I := L;
    J := R;
    P := SortList[(L + R) shr 1];
    repeat
      while SCompare(SortList[I], P, Prec) < 0 do
        Inc(I);
      while SCompare(SortList[J], P, Prec) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList[I];
        SortList[I] := SortList[J];
        SortList[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortFloatList(SortList, L, J, Prec, SCompare);
    L := I;
  until I >= R;
end;

procedure SortFloatList(List : TxFloatList; Prec: Double; Compare: TFloatListCompare);
begin
  if (List <> nil) and (List.Count > 0) then
    QuickSortFloatList(List, 0, List.Count - 1, Prec, Compare);
end;

function SortFloatListAsc(Item1, Item2, Prec: Double): Integer;
var
vTmp : double;
begin
  if (abs(Item1 - Item2) < Prec) then
  begin
    result := 0;
    exit;
  end;
  result := -1;
  vTmp := Item1 - Item2;
  if vTmp > 0 then
    result := 1;
end;

function SortFloatListDsc(Item1, Item2, Prec: Double): Integer;
var
vTmp : double;
begin
  if (abs(Item1 - Item2) < Prec) then
  begin
    result := 0;
    exit;
  end;
  result := 1;
  vTmp := Item1 - Item2;
  if vTmp > 0 then
    result := -1;
end;


{function ppm(AFreq, ARefFreq : double):double;
begin
  if (ARefFreq < 0.0000001) then
    ARefFreq := 0.0000001;
  result := (AFreq - ARefFreq)/ARefFreq * 1000000;
end;

function Absppm(AFreq, ARefFreq : double):double;
begin
  result := abs(ppm(AFreq, ARefFreq));
end; }

function StdDevFixed(Arr: TxFloatList): double;
var vSumm, vDev : double;
  i : integer;
begin
  vSumm := 0;
  result := 0;
  if ((not assigned(Arr)) or (Arr.Count < 1)) then
  exit;
  i := 0;
  while (i < Arr.Count) do
  begin
    vSumm := vSumm + Arr[i];
    inc(i);
  end;
  vDev := vSumm / Arr.Count;
  vSumm := 0;
  i := 0;
  while (i < Arr.Count) do
  begin
    vSumm := vSumm + (vDev - Arr[i]) * (vDev - Arr[i]);
    inc(i);
  end;
  if Arr.count <= 1 then
    result := 0
  else
    Result := Sqrt(vSumm / (Arr.count-1));
end;

function StdDevFixedPpm(Arr: TxFloatList; ARefFreq : double): double;
var vSumm, vDev : double;
  i : integer;
begin
  vSumm := 0;
  result := 0;
  if ((not assigned(Arr)) or (Arr.Count < 1)) then
    exit;
  i := 0;
  while (i < Arr.Count) do
  begin
    vSumm := vSumm + AbsPpm(Arr[i], ARefFreq);
    inc(i);
  end;
  vDev := vSumm / Arr.Count;
  vSumm := 0;
  i := 0;
  while (i < Arr.Count) do
  begin
    vSumm := vSumm + (vDev - AbsPpm(Arr[i], ARefFreq)) * (vDev - AbsPpm(Arr[i], ARefFreq));
    inc(i);
  end;
  if Arr.count <= 1 then
    result := 0
  else
    Result := Sqrt(vSumm / (Arr.count-1));
end;  

function ZeroStdDev(Arg: TxFloatList): double;
var vArr : array of double;
i : integer;
begin
  result := 0;
  if (Arg.Count > 1) then
  try
    SetLength(vArr, Arg.Count);
    for i := Low(vArr) to High(vArr) do
      vArr[i] := Arg[i];
    try
      result := Vodopad.Math.ZeroStdDev(vArr);
    finally
      SetLength(vArr, 0);
    end;
  except
  end;
end;

constructor TxFloatList.Create;
begin
  inherited Create;
  FDuplicates := dupAccept;
  FList := TList.Create;
  FSizeOfFloat := SizeOf(double);
end;

destructor TxFloatList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TxFloatList.Assign(Source: TPersistent);
begin
  if Source is TxFloatList then
  begin
    Clear;
    AddFloats(TxFloatList(Source));
  end
  else
    inherited Assign(Source);
end;

procedure TxFloatList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Min', ReadMin, WriteMin, min <> 0);
  Filer.DefineProperty('Max', ReadMax, WriteMax, FMax <> 0);
  Filer.DefineProperty('Floats', ReadFloats, WriteFloats, Count > 0);
end;

procedure TxFloatList.ReadMin(Reader: TReader);
begin
  FMin := Reader.ReadFloat;
end;

procedure TxFloatList.WriteMin(Writer: TWriter);
begin
  Writer.WriteFloat(FMin);
end;

procedure TxFloatList.ReadMax(Reader: TReader);
begin
  FMax := Reader.ReadFloat;
end;

procedure TxFloatList.WriteMax(Writer: TWriter);
begin
  Writer.WriteFloat(FMax);
end;

procedure TxFloatList.ReadFloats(Reader: TReader);
begin
  Reader.ReadListBegin(); (* Считывание маркера начала списка *)
  Clear; (* Очистка иекущего списка *)
  while not Reader.EndOfList do
    Add(Reader.ReadFloat()); (* Добавление к списку хранящихся чисед *)
  Reader.ReadListEnd(); (* Считывание маркера конца списка *)
end;

procedure TxFloatList.WriteFloats(Writer: TWriter);
var
  i: integer;
begin
  Writer.WriteListBegin(); (* Вписываем маркер начала списка *)
  for i := 0 to Count - 1 do
    Writer.WriteFloat(GetItem(I)); (* Запись всех чисел из списка в Writer *)
  Writer.WriteListEnd(); (* Вписываем маркер конца списка *)
end;

procedure TxFloatList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort();
    FSorted := Value;
  end;
end;

function TxFloatList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TxFloatList.GetItem(Index: integer): double;
begin
  Result := PDouble(FList.Items[Index])^;
end;

procedure TxFloatList.SetItem(Index: integer; Value: double);
begin
  { if ( FMin <> FMax ) and ( ( Value < Fmin ) or ( Value > FMax ) ) then
      raise EOutOfRange.CreateFmt( 'Value must be within %d..%d', [FMin, FMax]);}
  PDouble(FList.Items[Index])^ := Value;
end;

procedure TxFloatList.SetMin(Value: double);
var
  i: integer;
begin
  if Value <> FMin then
  begin
    for i := 0 to Count - 1 do
      if GetItem(i) < Value then
        raise EOutOfRange.CreateFmt('Unable to set new minimum value. ' + #13 +
          'List contains values below %d', [Value]);
    FMin := Value;
    if FMin > FMax then
      FMax := FMin;
  end;
end;

procedure TxFloatList.SetMax(Value: double);
var
  i: integer;
begin
  if Value <> FMax then
  begin
    for i := 0 to Count - 1 do
      if GetItem(i) > Value then
        raise EOutOfRange.CreateFmt('Unable to set new maximum value. '#13 +
          'List contains values above %d', [Value]);
    FMax := Value;
    if FMax < FMin then
      FMin := FMax;
  end;
end;

procedure TxFloatList.AddFloats(List: TxFloatList);
var
  i: integer;
begin
  if (List.Count = 0) then
    exit;
  for i := 0 to Pred(List.Count) do
    Add(List[i]);
end;

function TxFloatList.Add(Value: double): integer;
begin
  Insert(Count, Value);
  result := Count;
end;

procedure TxFloatList.Clear;
var
  i: integer;
begin
  i := 0;
  while i < FList.Count do
  begin
    Dispose(PDouble(FList.Items[i]));
    inc(i);
  end;
  FList.Clear;
 { FMin := 0;
  FMax := 0;}
end;

procedure TxFloatList.Delete(Index: integer);
begin
  Dispose(PDouble(FList.Items[Index]));
  FList.Delete(Index);
end;

function TxFloatList.Equals(List: TxFloatList; Prec: double): Boolean;
var
  i, Count: integer;
begin
  Count := GetCount;
  if Count <> List.GetCount then
    Result := False
  else
  begin
    i := 0;
    while (i < Count) and (Abs(GetItem(i) - List.GetItem(i)) < Prec) do
      INC(i);
    Result := i = Count;
  end;
end;

procedure TxFloatList.Exchange(Index1, Index2: integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TxFloatList.Find(N, Prec: double; var Index: integer): Boolean;
var
  l, h, i: integer;
begin
  Result := False;
  l := 0;
  h := Count - 1;
  while l <= h do
  begin
    i := (l + h) shr 1;
    if GetItem(i) < N then
      l := i + 1
    else
    begin
      h := i - 1;
      if abs(GetItem(i) - N) < Prec then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          l := i;
      end;
    end;
  end;
  Index := l;
end;

function TxFloatList.FindInUnsorted(N, Prec: double; var Index: integer): Boolean;
begin
  Index := 0;

  while (Index < Count) and (abs(GetItem(Index) - N) > Prec) do
  inc(Index);  
  result := Index < Count;
end;

function TxFloatList.IndexOf(N, Prec: double): integer;
var
  i: integer;
begin
  Result := -1;
  if not Sorted then
  begin
    for i := 0 to Pred(GetCount) do
      if abs(GetItem(i) - N) < Prec then
      begin
        Result := i;
        exit;
      end;
  end
  else if Find(N, Prec, i) then
    Result := i;
end;

procedure TxFloatList.Insert(Index: integer; Value: double);
var
  P: PDouble;
begin
  //comment ad 12.04.2001 softland
  // if (FMin <> FMax) and (( Value < FMin ) or (Value > FMax )) then
  // raise EOutOfRange.CreateFmt( 'Value must be within %f..%f', [FMin, FMax ]);
  case FDuplicates of
    dupIgnore:
    begin
      if (IndexOf(Value, _EPSILON_) = -1) then
      begin
        NEW(p);
        p^ := Value;
        FList.Insert(Index, P);
      end;
    end;
    dupAccept:
    begin
      NEW(p);
      p^ := Value;
      FList.Insert(Index, P);
    end;
    dupError:   
    begin
      if (IndexOf(Value, _EPSILON_) = -1) then
      begin
        NEW(p);
        p^ := Value;
        FList.Insert(Index, P);
      end else
        raise EOutOfRange.Create( 'Duplicate detected');
    end;
  end;
end;

procedure TxFloatList.Move(CurIndex, NewIndex: integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TxFloatList.QuickSort(L, R: integer);
var
  vL, vR: integer;
  p: PDouble;
begin
  vL := L;
  vR := R;
  P := PDouble(FList[R-(R - L) div 2]);
  while vL < vR do
  begin
    while PDouble(FList[vL])^ < P^ do
      INC(vL);
    while PDouble(FList[vR])^ > P^ do
      DEC(vR);
    if vL <= vR then
    begin
      FList.Exchange(vL, vR);
      INC(vL);
      DEC(vR);
    end;
  end;
  if L < vR then
    QuickSort(L, vR);
  if vL < R then
    Quicksort(vL, R);
end;

procedure TxFloatList.Sort();
begin
  if not Sorted and (FList.Count > 1) then
    QuickSort(0, FList.Count - 1);
end;

function TxFloatList.Sum: double;
var
  i: integer;
begin
  i := 0;
  result := 0;
  while i < self.Count do
  begin
    result := result + GetItem(i);
    inc(i);
  end;
end;

function TxFloatList.FindMax(): double; // определение max среди хранимых данных
var
  i: integer;
begin
  FMax := _FLOAT_MIN_;
  i := 0;
  while (i < Count) do
  begin
    FMax := Math.Max(GetItem(i), FMax);
    inc(i);
  end;
  result := FMax;
end;

function TxFloatList.FindMin: double; //определение min среди хранимых данных
var
  i: integer;
begin
  FMin := _FLOAT_MAX_;
  i := 0;
  while (i < Count) do
  begin
    FMin := Math.Min(GetItem(i), FMin);
    inc(i);
  end;
  result := FMin;
end;

(* Заменяет все отрицательные значения на нулевое *)

function TxFloatList.ReplaceNegativeToNULL: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i] < 0 then
    begin
      Items[i] := self.Null;
      inc(result);
    end;
  end;
end;

function TxFloatList.ReplaceValToVal(ThisValue, ToValue, Prec: double): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
  begin
    if abs(Items[i] - ThisValue) < Prec then
    begin
      Items[i] := ToValue;
      inc(result);
    end;
  end;
end;

function TxFloatList.ReplaceLessToVal(ThisValue, ToValue, Prec: double):
  integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i] < ThisValue then
    begin
      Items[i] := ToValue;
      inc(result);
    end;
  end;
end;

function TxFloatList.ReplaceGreateToVal(ThisValue, ToValue, Prec: double):
  integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i] > ThisValue then
    begin
      Items[i] := ToValue;
      inc(result);
    end;
  end;
end;

function TxFloatList.InvertValues(): integer;
var
  i: integer;
begin
  result := _OK_;
  for i := 0 to Count - 1 do
    items[i] := -items[i];
end;

function TxFloatList.Reverse(): integer;
var
  i, j: integer;
begin
  result := _OK_;
  i := 0;
  j := Count - 1;
  repeat
    self.Exchange(i, j);
    inc(i);
    dec(j);
  until i >= j;
end;

(* Заполнение в заданных пределах значениями NULL
   Подразумевается положительное и возрастающее поведение глубины, т.е.
   0<STRT<STOP
   Еи _strt > текущего min или _stop < текущего максимума содержащегося в
   списке, то функция возвращает _ERROR_
   Еи _null не совпадает со значением принятым за NULL в списке, то это игнорируется
   Заполнение ведется с текущим шагом списка *)

function TxFloatList.InsertNulls(iFirst, iCount: integer; _null: single):
  integer;
var
  k: integer;
begin
  for k := 1 to iCount do
  begin
    Insert(iFirst, _null);
    inc(iFirst);
  end;
  result := _OK_;
end;

end.
