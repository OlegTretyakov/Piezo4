unit ThreeDimMatrix;

interface

  uses
  System.Classes, System.Contnrs,
  ThreeDimTypes, MatrixTypes, MultiDimMatrix;

type
  TDepthCell = class;
  T3DimCell = class(TObject, IInterface, I3DimCell)
   private
    fDimCell : TDepthCell;
    fIndex : Word;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
   protected 
    function GetIndex:Word; stdcall;
   public
    constructor Create(const ADimCell : TDepthCell);virtual;
    destructor Destroy; override;
    property DimCell : TDepthCell read fDimCell;
    property CellIndex : Word read GetIndex;
  end;
  
  T3DimCellClass = class of T3DimCell;
  T3DimCellList = class(TObject)
   strict private
    fList : TObjectList;
    fCellClass : T3DimCellClass;
   public
    constructor Create(ACellClass : T3DimCellClass); virtual;
    destructor Destroy; override;
    property CellClass : T3DimCellClass read fCellClass;
    function Count : Integer;
    procedure Delete(Index : integer);
    procedure Clear;
    function NewItem(const ADimCell : TDepthCell) : T3DimCell;
    function QueryItem(AIndex : integer; const IID: TGUID; out Obj):boolean;
  end;
  
  {класс реализует €чейку с внутренним набором €чеек.
  “ип внутреннего набора определ€етс€ передачей
  ссылки на класс в конструктор или путем перекрыти€ функции DefaultCellClass}
  TDepthCell = class(TCell, IDepthCell)
   strict private
    fCells : T3DimCellList;
    fErrorsCount : Word;
   protected
    function DefaultCellClass: T3DimCellClass; virtual; 
    function GetDepth: Word; stdcall;
    procedure SetDepth(const Value: Word);stdcall;  
    function GetErrorsCount: Word; stdcall;
    procedure SetErrorsCount(const Value: Word);stdcall;
   public
    constructor Create(ACol: pColumnItem; ARow: pRowItem); override;
    destructor Destroy;override;
    property Depth : Word read GetDepth write SetDepth;
    function QueryItem(Index: Integer; const IID: TGUID; out Obj):boolean; stdcall; 
    property ErrorsCount : Word read GetErrorsCount write SetErrorsCount;
  end;

  T3DimMatrix = class(TMultiDimMatrix)
   protected
    function DefaultCellClass : TCellClass; override;
  end;

implementation

uses System.SysUtils;

{ T3DimCell }

constructor T3DimCell.Create(const ADimCell : TDepthCell);
begin
  fDimCell := ADimCell;
  fIndex := fDimCell.Depth;
end;

destructor T3DimCell.Destroy;
begin
  fDimCell := nil;
  inherited Destroy;
end;

function T3DimCell.GetIndex: Word;
begin
  result := fIndex;
end;

function T3DimCell.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function T3DimCell._AddRef: Integer;
begin
  Result := -1;
end;

function T3DimCell._Release: Integer;
begin
  Result := -1;
end;

{ T3DimCellList }



function T3DimCellList.NewItem(const ADimCell : TDepthCell): T3DimCell;
begin
  result := fCellClass.Create(ADimCell);
  fList.Add(result);
end;

function T3DimCellList.QueryItem(AIndex: integer; const IID: TGUID;
  out Obj): boolean;
begin
  result:= (AIndex > -1)
  and (AIndex < fList.Count)
  and Supports(fList.Items[AIndex], IID, Obj);
end;

procedure T3DimCellList.Clear;
begin
  fList.Clear;
end;

function T3DimCellList.Count: Integer;
begin
  result := fList.Count;
end;

constructor T3DimCellList.Create(ACellClass: T3DimCellClass);
begin
  fList := TObjectList.Create;
  fCellClass := ACellClass;
end;

procedure T3DimCellList.Delete(Index: integer);
begin
 fList.Delete(Index);
end;

destructor T3DimCellList.Destroy;
begin
  fList.Clear;
  FreeAndNil(fList);
  inherited Destroy;
end;

{ TDepthCell }

function TDepthCell.DefaultCellClass: T3DimCellClass;
begin
  result := T3DimCell;
end;

constructor TDepthCell.Create( ACol: pColumnItem; ARow: pRowItem);
begin
  inherited Create(ACol, ARow);
  fCells := T3DimCellList.Create(DefaultCellClass);
  fErrorsCount := 0;
end;

destructor TDepthCell.Destroy;
begin
  fCells.Clear;
  FreeAndNil(fCells);
  inherited Destroy;
end;

function TDepthCell.GetDepth: Word;
begin
  result := fCells.Count;
end;

function TDepthCell.GetErrorsCount: Word;
begin
  Result := fErrorsCount;
end;

function TDepthCell.QueryItem(Index: Integer; const IID: TGUID;
  out Obj): boolean;
begin
  result := fCells.QueryItem(Index, IID, Obj);
end;

procedure TDepthCell.SetDepth(const Value: Word);
begin
  while (fCells.Count > Value) do
    fCells.Delete(fCells.count -1);
  while (fCells.Count < Value) do
    fCells.NewItem(self);
  if fCells.Count < 1 then
    fErrorsCount := 0;
end;

procedure TDepthCell.SetErrorsCount(const Value: Word);
begin
  fErrorsCount := Value;
end;

{ T3DimMatrix }

function T3DimMatrix.DefaultCellClass: TCellClass;
begin
  result := TDepthCell;
end;

end.
