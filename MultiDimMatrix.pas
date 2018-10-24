unit MultiDimMatrix;

interface
uses
  System.Classes, System.Contnrs, MatrixTypes;
  
  type
  TColumnList = class(TList)
    protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Get(Index: Integer): pColumnItem;
   public
    function NewItem(AHeader : double) : pColumnItem;
    function First : pColumnItem; reintroduce;
    function Last : pColumnItem; reintroduce;
    procedure Clear; override;
    property Items[Index: Integer]: pColumnItem read Get; default;
  end;
  
  TRowList = class(TList)
    protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Get(Index: Integer): pRowItem;
   public
    function NewItem(ATag : SmallInt):pRowItem;
    function First : pRowItem; reintroduce;
    function Last : pRowItem; reintroduce;
    procedure Clear; override;
    property Items[Index: Integer]: pRowItem read Get; default;
  end;
  
  { ласс реализует €чейку с координатами Col, Row - X  и Y соответственно
   ласс €вл€етс€ родителем €чеек-хранилищ,
  определ€емых дальнейшей необходимостью}
  TCell = class(TObject, IInterface, ICell)
   strict private
    fCol : pColumnItem;
    fRow : pRowItem;
   private
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
   public
    constructor Create(ACol : pColumnItem; ARow : pRowItem); virtual;
    function Col : pColumnItem; stdcall;
    function Row : pRowItem; stdcall;
  end;

  TCellClass = class of TCell;


  { ласс - хранилище €чеек TCell
  и выдачу ссылок на интерфейс ICell }
  TCellList = class(TObject)
   strict private 
    fList : TObjectList;
    fCellClass : TCellClass;
   public
    constructor Create(ACellClass : TCellClass);
    destructor Destroy; override;
    property CellClass : TCellClass read fCellClass;
    function Count : integer;
    procedure Delete(Index : integer);
    procedure Clear;
    function NewItem(ACol : pColumnItem; ARow : pRowItem) : ICell;
    function QueryItem(AIndex : integer; const IID: TGUID; out Obj):boolean;
  end;

  { ласс многомерной матрицы
  “ип внутреннего набора определ€етс€ передачей
  ссылки на класс в конструктор или путем перекрыти€ функции DefaultCellClass
  ќбеспечивает поиск по индексу колонки, индексу строки и по типу интерфейса}
  {—труктура таблицы TMultiDimMatrix
        Col->∞—   -60     |..|     85     | <-Header
       \/Row
      Tag 1  |TCellClass  |..| TCellClass |
      Tag 2  |TCellClass  |..| TCellClass |
      Tag 3  |TCellClass  |..| TCellClass |
      ............
      Tag 6  |TCellClass  |..| TCellClass |
      TCellClass - Ёкзепл€р объекта TCellClass
      Ёто может быть €чейка как с простыми данными
      так и содержать в себе объекты, включа€ списки
      }
  TMultiDimMatrix = class(TObject, IInterface, IMultiDimMatrix)
   private
    fCols : TColumnList;
    fRows : TRowList;
    fCells : TCellList;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
   protected
    function DefaultCellClass : TCellClass; virtual;
    property Cells : TCellList read fCells;
   public
    constructor Create overload; virtual;
    constructor Create(ACellClass: TCellClass); {reintroduce;} overload; virtual;
    destructor Destroy; override;  
    procedure Clear; stdcall;
    function ColCount : integer; stdcall;
    function RowCount : integer; stdcall;
    function Header(ColumnIndex: byte): double; stdcall;
    {‘ункци€ вернет значение заголовка по его индексу
    »ндекс должен быть <  ColCount}
    function Tag(RowIndex: byte): SmallInt; stdcall;
    {‘ункци€ вернет значение строки по его индексу
    »ндекс должен быть <  RowCount}
    function FindCol(AHeader : double; var ACol : byte):Boolean;stdcall;
    function AddCol(AHeader : double) : integer; stdcall;
    function AddRow(ATag : SmallInt) : integer; stdcall;
    {‘ункции AddCol и AddRow добавл€ют колонку и строку с параметрами соответственно}
    function FindRow(ATag: SmallInt; var ARow : byte): boolean; stdcall;
    {‘ункци€ поиска строки по параметру Tag. ¬ернет true при успешном поиске
    ¬ выходной переменной ARow будет значение индекса строки}
    function ByIndex(ACol, ARow : byte; const IID: TGUID; out Obj): boolean; stdcall;
    {‘ункци€ поиска по индексам ACol, ARow и GUID-у €чейки.
    ¬озвратит true при найденной €чейке. ¬ выходной переменной Obj будет ссылка на интерфейс €чейки}
    function ByParams(ACol : byte; ATag : SmallInt; const IID: TGUID; out Obj): boolean; stdcall;
    {‘ункци€ поиска по индексу ACol, параметру ARow и GUID-у €чейки.
    ¬озвратит true при найденной €чейке. ¬ выходной переменной Obj будет ссылка на интерфейс €чейки}
  end;

  TMultiDimMatrixClass = class of TMultiDimMatrix;

implementation

uses System.SysUtils, System.Math;

{ TColumnList } 

function TColumnList.NewItem(AHeader: double): pColumnItem;
begin
  New(result);
  result.Index := byte(self.Count);
  result.Header := AHeader;
  inherited Add(result);
end;

procedure TColumnList.Clear;
begin
  while count > 0 do
    delete(count -1);
  inherited;
end;

function TColumnList.First: pColumnItem;
begin
  result := Get(0);
end;

function TColumnList.Get(Index: Integer): pColumnItem;
begin
  Result := pColumnItem(inherited Get(Index));
end;

function TColumnList.Last: pColumnItem;
begin
  result := Get(Count - 1);
end;

procedure TColumnList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
      Dispose(Ptr);
  inherited Notify(Ptr, Action);
end;

{ TRowList } 

function TRowList.NewItem(ATag: SmallInt): pRowItem;
begin
  New(result);
  result.Index := byte(self.Count);
  result.M_Tag := ATag;
  inherited Add(result);
end;

procedure TRowList.Clear;
begin
  while count > 0 do
    delete(count -1);
  inherited;
end;

function TRowList.First: pRowItem;
begin
  result := Get(0);
end;

function TRowList.Get(Index: Integer): pRowItem;
begin
  Result := pRowItem(inherited Get(Index));
end;

function TRowList.Last: pRowItem;
begin
  result := Get(Count - 1);
end;

procedure TRowList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
      Dispose(Ptr);
  inherited Notify(Ptr, Action);
end;

{ T2DimCellList }

function TCellList.NewItem(ACol : pColumnItem; ARow : pRowItem): ICell;
var
vTmp : TCell;
begin
  vTmp := fCellClass.Create(ACol, ARow);
  fList.Add(vTmp);
  result := vTmp as ICell;
end;

function TCellList.QueryItem(AIndex: integer; const IID: TGUID;
  out Obj): boolean;
begin
  result:= (AIndex > -1)
  and (AIndex < fList.Count)
  and assigned(fList.Items[AIndex])
  and Supports(fList.Items[AIndex], IID, Obj);
end;

procedure TCellList.Clear;
begin
  fList.Clear;
end;

function TCellList.Count: integer;
begin
  result := fList.Count;
end;

constructor TCellList.Create(ACellClass: TCellClass);
begin
  fList := TObjectList.Create;
  fCellClass := ACellClass;
end;

procedure TCellList.Delete(Index: integer);
begin
  fList.Delete(Index);
end;

destructor TCellList.Destroy;
begin
  fList.Clear;
  FreeAndNil(fList);
  inherited Destroy;
end;

{ TMultiDimMatrix }

constructor TMultiDimMatrix.Create;
begin
  Create(DefaultCellClass);
end;

constructor TMultiDimMatrix.Create(ACellClass: TCellClass);
begin
  fCols := TColumnList.Create;
  fRows := TRowList.Create;
  fCells := TCellList.Create(ACellClass);
end;

destructor TMultiDimMatrix.Destroy;
begin
  Clear;
  FreeAndNil(fCells);
  FreeAndNil(fCols);
  FreeAndNil(fRows);
  inherited Destroy;
end;

procedure TMultiDimMatrix.Clear;
begin
  fCells.Clear;
  fRows.Clear;
  fCols.Clear;
end;  

function TMultiDimMatrix.FindCol(AHeader: double; var ACol : byte): Boolean;
begin
  ACol := 0;
  while ACol < fCols.Count do
  begin
    if SameValue(fCols[ACol].Header, AHeader, 0.5) then
      Break;
    inc(ACol);
  end;
  Result := ACol < fCols.Count;
end;

function TMultiDimMatrix.AddCol(AHeader: double): integer;
var
i : integer;
vRes : byte;
v : ICell;
begin
  if FindCol(AHeader, vRes) then
  begin
    result := vRes;
    exit;
  end; 
  fCols.NewItem(AHeader);
  i := 0;
  while i < fRows.Count do
  begin
    v := fCells.NewItem(fCols.Last, fRows[i]);
    v := nil;
    inc(i);
  end;
  result := fCols.Count - 1;
end;

function TMultiDimMatrix.AddRow(ATag: SmallInt): integer;
var
i : integer;
vRes : byte;
v : ICell;
begin
  if FindRow(ATag, vRes) then
  begin
    result := vRes;
    exit;
  end;
  fRows.NewItem(ATag);
  i := 0;
  while i < fCols.Count do
  begin
    v := fCells.NewItem(fCols[i], fRows.Last);
    v := nil;
    inc(i);
  end;
  result := fRows.Count - 1;
end;

function TMultiDimMatrix.ColCount: integer;
begin
  result := fCols.Count;
end;

function TMultiDimMatrix.RowCount: integer;
begin
  result := fRows.Count;
end;  

function TMultiDimMatrix.Header(ColumnIndex: byte): double;
begin
  result := fCols[ColumnIndex]^.Header;
end;

function TMultiDimMatrix.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TMultiDimMatrix.Tag(RowIndex: byte): SmallInt;
begin
  result := fRows[RowIndex]^.M_Tag;
end;

function TMultiDimMatrix._AddRef: Integer;
begin
  Result := -1;
end;

function TMultiDimMatrix._Release: Integer;
begin
  Result := -1;
end;

function TMultiDimMatrix.DefaultCellClass: TCellClass;
begin
  result := TCell;
end;

function TMultiDimMatrix.FindRow(ATag: SmallInt; var ARow : byte): boolean;
begin
  ARow := 0;
  while ARow < fRows.Count do
  begin
    if (fRows.Items[ARow]^.M_Tag = ATag) then
      break;
    inc(ARow);
  end;
  result := ARow < fRows.Count;
end;

function TMultiDimMatrix.ByIndex(ACol, ARow: byte; const IID: TGUID;
  out Obj): boolean;
var
i : integer;
v : ICell;
begin
  i := 0;
  while (i < fCells.Count) do
  begin
    v := nil;
    if (fCells.QueryItem(i, ICell, v))
    and (v.Row^.Index = ARow)
    and (v.Col^.Index = ACol) then
      break;
    inc(i);
  end;
  result := (i < fCells.Count) and Assigned(v) and (v.QueryInterface(IID, Obj) = S_OK);
  v := nil;
end;

function TMultiDimMatrix.ByParams(ACol : byte; ATag: SmallInt; const IID: TGUID;
  out Obj): boolean;
var
i : integer;
v : ICell;
begin
  i := 0;
  while (i < fCells.Count) do
  begin
    v := nil;
    if (fCells.QueryItem(i, ICell, v))
    and (v.Row^.M_Tag = ATag)
    and (v.Col^.Index = ACol) then
      break;
    inc(i);
  end;
  result := (i < fCells.Count) and Assigned(v) and (v.QueryInterface(IID, Obj) = S_OK);
  v := nil;
end;

{ TCell }

function TCell.Col: pColumnItem;
begin
  result := fCol;
end;

constructor TCell.Create(ACol : pColumnItem; ARow : pRowItem);
begin
  fCol := ACol;
  fRow := ARow;
end;

function TCell.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TCell.Row: pRowItem;
begin
  result := fRow;
end;

function TCell._AddRef: Integer;
begin
  Result := -1;
end;

function TCell._Release: Integer;
begin
  Result := -1;
end;

end.

