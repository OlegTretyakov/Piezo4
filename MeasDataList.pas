unit MeasDataList;

interface
uses System.Classes, System.Generics.Collections, System.Contnrs, Spline;
  type

  {”наследуй свой класс от TMeasDataItem, добавь новые пол€.
  в проперти TMeasDataList.ObjectClass укажи класс своего объекта.
  TMeasDataList.NewItem получаешь экземпл€р своего объекта}

  TMeasDataList = class;

  TMeasDataItem = class(TObject)
   private
    fDataList : TMeasDataList;
    fXValue, fYValue : double;
    fError : smallint;
   protected   
    function GetDataList: TMeasDataList;
    function GetError : smallint;
    function GetX: double;
    function GetY: double;
    procedure SetX(const Value: double);
    procedure SetY(const Value: double);
    procedure SetError(const Value: smallint);
    class procedure BeginUpdate(ADataList : TMeasDataList); virtual;
    class procedure EndUpdate(ADataList : TMeasDataList); virtual;
    class function CalcErrors(ADataList : TMeasDataList) : word; virtual;  
    class function CalcTotalCount(ADataList : TMeasDataList) : word; virtual;
   public
    constructor Create(ADataList : TMeasDataList); virtual;
    property DataList : TMeasDataList read GetDataList;
    function YX(AX : double):double; stdcall;
    property XValue : double read GetX write SetX;
    property YValue : double read GetY write SetY;
    property Error : smallint read GetError write SetError;
  end;


  TSplineList = class(TObjectList<TSpline>)
   public
    procedure ClearSplines;
    function NewItem(ASize : Word; SType : TSplineType = st_cspline) : TSpline;
  end;

  TMeasDataItemClass = class of TMeasDataItem;
  TMeasDataList = class(TObjectList)
   private
    fSType : TSplineType;
    fItemClass : TMeasDataItemClass;
    fSplines : TSplineList;
    procedure SetSplineType(const Value: TSplineType);
   protected
    function GetErrorsCount : word;
    function GetTotalCount : word;
    function GetItemClass: TMeasDataItemClass;
    procedure SetItemClass(const Value: TMeasDataItemClass);
   public
    constructor Create(SType : TSplineType = st_cspline);
    destructor Destroy; override;
    property ItemClass: TMeasDataItemClass read  GetItemClass write SetItemClass;
    function ItemsCount : Integer;
    procedure DeleteItem(AIndex : Cardinal);
    procedure ClearData;
    procedure BeginUpdate;
    procedure EndUpdate;
    function NewItem : TMeasDataItem;
    function YX(AX: double; ASplineIdx : Word=0): double;
    property SType : TSplineType read fSType write SetSplineType;
    property Splines : TSplineList read fSplines;
    function XExists(AValue, APrecis : double; out Index : integer):boolean; overload;
    function XExists(AValue : double; out Index : integer):boolean; overload;
    property ErrorsCount : word read GetErrorsCount;
    property TotalCount : word read GetTotalCount;
    function QueryItem(Index : integer; AClass : TMeasDataItemClass; var Obj):boolean;
  end;
  function SortByXAsc(Item1, Item2: Pointer): Integer;


implementation
uses System.SysUtils, System.Math;

{ TMeasDataList }

procedure TMeasDataList.BeginUpdate;
begin
  fItemClass.BeginUpdate(self);
end;

procedure TMeasDataList.ClearData;
begin
  self.Clear;
  fSplines.ClearSplines;
  fSplines.Clear;
end;

constructor TMeasDataList.Create(SType : TSplineType = st_cspline);
begin
  inherited Create;
  fSType := SType;
  fSplines := TSplineList.Create; 
  fItemClass := TMeasDataItem;
end;

procedure TMeasDataList.DeleteItem(AIndex: Cardinal);
begin
  self.Delete(AIndex);
end;

destructor TMeasDataList.Destroy;
begin
  fSplines.ClearSplines;
  FreeAndNil(fSplines);
  inherited Destroy;
end;

function SortByXAsc(Item1, Item2: Pointer): Integer;
var
vTmp1, vTmp2 : TObject;
begin
  result := 0;
  if Item1 = Item2 then
     exit;
  if not assigned(Item1) then
  begin
    result := 1;
    exit;
  end else
  if not assigned(Item2) then
  begin
    result := -1;
    exit;
  end;
  vTmp1 := TObject(Item1);
  vTmp2 := TObject(Item2);
  if not  (vTmp1 is TMeasDataItem) then
  begin
    result := 1;
    exit;
  end else
  if not (vTmp2 is TMeasDataItem) then
  begin
    result := -1;
    exit;
  end;
  result := 0;
  if (TMeasDataItem(vTmp2).XValue < TMeasDataItem(vTmp1).XValue) then
    result := 1
  else if (TMeasDataItem(vTmp2).XValue > TMeasDataItem(vTmp1).XValue) then
    result := -1;
end;

procedure TMeasDataList.EndUpdate;
var
vIdx : integer;
begin
  fItemClass.EndUpdate(self);
  if (fSplines.Count < 1) then
    exit;
  for vIdx := 0 to fSplines.Count - 1 do
    fSplines[vIdx].InitSpline;
end;

function TMeasDataList.GetErrorsCount: word;
begin
  result := fItemClass.CalcErrors(self);
end;

function TMeasDataList.GetItemClass: TMeasDataItemClass;
begin
  result := fItemClass;
end;

function TMeasDataList.GetTotalCount: word;
begin
  result := fItemClass.CalcTotalCount(self);
end;

function TMeasDataList.ItemsCount: Integer;
begin
  result := Count;
end;

function TMeasDataList.NewItem: TMeasDataItem;
begin
  Result := fItemClass.Create(self);
  Add(Result);
end;

procedure TMeasDataList.SetItemClass(const Value: TMeasDataItemClass);
begin
  if (fItemClass <> Value) then
    self.Clear;
  fItemClass := TMeasDataItemClass(Value);
end;

procedure TMeasDataList.SetSplineType(const Value: TSplineType);
begin
  if fSType <> Value then
  begin
    self.BeginUpdate;
    try
      fSType := Value;
    finally
      self.EndUpdate;
    end;
  end;
end;

function TMeasDataList.QueryItem(Index: integer; AClass: TMeasDataItemClass; var Obj): boolean;
begin
  try
    result := (Index > -1)
    and (Index < Count)
    and (items[Index] is AClass);
    if Result then
      TMeasDataItem(Obj) := TMeasDataItem(items[Index]);
  except
    result := false;
  end;
end;

function TMeasDataList.YX(AX: double; ASplineIdx : Word): double;
begin
  result := 0;
  if (ASplineIdx >= fSplines.Count) then
    exit;
  Result := fSplines[ASplineIdx].Y(AX);
end;



function TMeasDataList.XExists(AValue: double; out Index: integer): boolean;
begin
  result := XExists(AValue, 0.2, Index);
end;

function TMeasDataList.XExists(AValue, APrecis: double;
  out Index: integer): boolean;
begin
  Index := 0;
  while (Index < Count) do
  begin
    Result := SameValue(TMeasDataItem(Items[Index]).XValue, AValue, APrecis);
    if Result then
      Break;
    inc(Index);
  end;
  result := Index < Count;
end;

{ TMeasDataObject }

class function TMeasDataItem.CalcErrors(ADataList: TMeasDataList): word;
var
vIdx : Integer;
vDO : TMeasDataItem;
begin
  result := ADataList.Count;
  if result < 1 then
    exit;
  for vIdx := 0 to ADataList.Count - 1 do
  begin
    if ADataList.QueryItem(vIdx, TMeasDataItem, vDO) then
    begin
      if (vDO.Error = 0) then
        dec(result);
    end;
  end;
end;

class function TMeasDataItem.CalcTotalCount(ADataList: TMeasDataList): word;
begin
  result := ADataList.Count;
end;

constructor TMeasDataItem.Create(ADataList: TMeasDataList);
begin
  inherited Create;
  fDataList := ADataList;
  fError := 0;
end; 

class procedure TMeasDataItem.BeginUpdate(ADataList: TMeasDataList);
begin
  ADataList.fSplines.Clear;
end;

class procedure TMeasDataItem.EndUpdate(ADataList: TMeasDataList);
var
vNewSpline : TSpline;
vIdx : Integer;
vDO1, vDO2 : TMeasDataItem;
vOkCount, vErrCount : word;
begin
  ADataList.Sort(SortByXAsc);
  vOkCount := ADataList.Count;
  if (vOkCount < 3) then
    exit;
  for vIdx := vOkCount downto 1 do
  begin
    if ADataList.QueryItem(vIdx, TMeasDataItem, vDO1)
    and ADataList.QueryItem(vIdx-1, TMeasDataItem, vDO2) then
    begin
      if SameValue(vDO1.XValue, vDO2.XValue, 0.0005) then
         ADataList.DeleteItem(vIdx);
    end;
  end;
  vOkCount := ADataList.Count;
  vErrCount := ADataList.ErrorsCount;
  if ((vOkCount - vErrCount) < 3) then
    exit;
  vNewSpline := ADataList.Splines.NewItem(vOkCount - vErrCount, ADataList.SType);

  for vIdx := 0 to ADataList.Count - 1 do
  begin
    if ADataList.QueryItem(vIdx, TMeasDataItem, vDO1)
    and (vDO1.Error = 0) then
      vNewSpline.AddXY(vDO1.XValue, vDO1.YValue);
  end;
end;

function TMeasDataItem.GetDataList: TMeasDataList;
begin
  result := fDataList;
end;

function TMeasDataItem.GetError: smallint;
begin
  result := fError;
end;

function TMeasDataItem.GetX: double;
begin
  result := fXValue;
end;

function TMeasDataItem.GetY: double;
begin
  result := fYValue;
end;

procedure TMeasDataItem.SetError(const Value: smallint);
begin
  fError := Value;
end;

procedure TMeasDataItem.SetX(const Value: double);
begin
  fXValue := Value;
end;

procedure TMeasDataItem.SetY(const Value: double);
begin
  fYValue := Value;
end;

function TMeasDataItem.YX(AX: double): double;
begin
  result := DataList.YX(AX);
end;

{ TSplineList }

procedure TSplineList.ClearSplines;
var
vIdx : Word;
vSpline : TSpline;
begin
  vIdx := 0;
  while vIdx < Count do
  begin
    self[vIdx].Clear;
    inc(vIdx);
  end;
end;


function TSplineList.NewItem(ASize: Word; SType : TSplineType = st_cspline): TSpline;
begin
  result := TSpline.Create(ASize, SType);
  add(result);
end;


end.
