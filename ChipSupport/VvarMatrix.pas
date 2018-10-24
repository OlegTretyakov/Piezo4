unit VvarMatrix;

interface

uses
  System.Classes, MultiDimMatrix,
  ThreeDimTypes, ThreeDimMatrix,
  VvarMatrixIF, MeasDataList;
type 

  TValueCell = class(TCell, IValueCell)
   private
    fValue : double;
   protected
    function GetValue: double; stdcall;
    procedure SetValue(AValue : double); stdcall;
   public
    property Value: double read GetValue write SetValue;
  end; 

  TVvarCell = class(T3DimCell, IVvarCell)
   private
    fErrCode : SmallInt;
    fIzmNum : LongWord;
    fVdd, fVC,
    fVoltage : double;
    fVar : byte;
    fMemory : TMemoryStream;
   protected
    function GetErrCode: SmallInt; stdcall;
    function GetIzmNum: LongWord; stdcall;
    function GetMemory: TMemoryStream; stdcall; 
    function GetVdd: double; stdcall;
    function GetVC: double; stdcall;
    function GetVoltage: double; stdcall;
    procedure SetErrCode(const Value: SmallInt);stdcall;
    procedure SetIzmNum(const Value: LongWord); stdcall;
    procedure SetVdd(const Value: double); stdcall;
    procedure SetVC(const Value: double); stdcall;
    procedure SetVoltage(const Value: double); stdcall;
    function GetVar: byte; stdcall;
    procedure SetVar(const Value: byte); stdcall;
   public
    constructor Create(const ADimCell : TDepthCell);override;
    destructor Destroy; override;
    property ErrCode : SmallInt read GetErrCode write SetErrCode;
    property IzmNum : LongWord read GetIzmNum write SetIzmNum; 
    property Vdd : double read GetVdd write SetVdd;
    property VC : double read GetVC write SetVC;
    property Voltage : double read GetVoltage write SetVoltage;
    property mVar : byte read GetVar write SetVar;
    property Memory : TMemoryStream read GetMemory;
  end;

  TVvarDepthCell = class(TDepthCell)
   protected
    function DefaultCellClass: T3DimCellClass; override;
  end;


  {Структура таблицы TVvarMatrix
    3DimCell = это ячейка третьего измерения с контейнером для произвольных данных
     Col->°С |  -60     |..|    85    | <-Header
       \/Row
      Tag 1  |3DimCell  |..| 3DimCell |
      Tag 2  |3DimCell  |..| 3DimCell |
      Tag 3  |3DimCell  |..| 3DimCell |
      ............
      Tag 6  |3DimCell  |..| 3DimCell |

      }

  TVvarMatrix = class(TMultiDimMatrix, IVvarMatrix)
   protected
    function DefaultCellClass : TCellClass; override;
   public
    function ByVar(ACol : byte; ATag : SmallInt; AVar: byte; const IID: TGUID; out Obj):boolean; stdcall; 
    function TotalCount : word; stdcall;
    function ErrorCount : word; stdcall;
  end;

  {Структура таблицы TVoltageTable
        Col->°С -60 |..|  85  | <-Header
       \/Row
      Tag 1  |Volt  |..| Volt |
      Tag 2  |Volt  |..| Volt |
      Tag 3  |Volt  |..| Volt |
      ............
      Tag 6  |Volt  |..| Volt |

      }
  TVvarMatrixClass = class of TVvarMatrix;

  TVoltageTable = class(TMultiDimMatrix, IVoltageTable)
   protected   
    function GetVoltage(ACol, ARow : byte): double; stdcall;
    procedure SetVoltage(ACol, ARow : byte; AValue : double); stdcall;
    function DefaultCellClass : TCellClass; override;
   public
    property Voltage[ACol, ARow : byte]: double read GetVoltage write SetVoltage;
  end;

  TVoltageTableClass = class of TVoltageTable;

implementation
uses System.SysUtils;

{ TVvarCell }

constructor TVvarCell.Create(const ADimCell: TDepthCell);
begin
  inherited Create(ADimCell);
  fMemory := TMemoryStream.Create;
end;

destructor TVvarCell.Destroy;
begin
  FreeAndNil(fMemory);
  inherited;
end;

function TVvarCell.GetErrCode: SmallInt;
begin
  result := fErrCode;
end;

function TVvarCell.GetIzmNum: LongWord;
begin
  result := fIzmNum;
end;

function TVvarCell.GetMemory: TMemoryStream;
begin
  fMemory.Position := 0;
  result := fMemory;
end;

function TVvarCell.GetVoltage: double;
begin
  result := fVoltage;
end;

function TVvarCell.GetVC: double;
begin
  result := fVC;
end;

function TVvarCell.GetVar: byte;
begin
  result := fVar;
end;

function TVvarCell.GetVdd: double;
begin
  result := fVdd;
end;

procedure TVvarCell.SetErrCode(const Value: SmallInt);
begin
  fErrCode := Value;
end;

procedure TVvarCell.SetIzmNum(const Value: LongWord);
begin
  fIzmNum := Value;
end;

procedure TVvarCell.SetVoltage(const Value: double);
begin
  fVoltage := Value;
end;

procedure TVvarCell.SetVC(const Value: double);
begin
  fVC := Value;
end;

procedure TVvarCell.SetVar(const Value: byte);
begin
  fVar := Value;
end;

procedure TVvarCell.SetVdd(const Value: double);
begin
  fVdd := Value;
end;

{ VvarDepthCell }

function TVvarDepthCell.DefaultCellClass: T3DimCellClass;
begin
  result := TVvarCell;
end;

{ TVvarMeasTable }

function TVvarMatrix.ByVar(ACol : byte; ATag : SmallInt; AVar: byte; const IID: TGUID; out Obj): boolean;
var
vDepthCell : IDepthCell;
begin
  result := ByParams(ACol, ATag, IDepthCell, vDepthCell)
    and (vDepthCell.QueryItem(AVar, IID, Obj));
  vDepthCell := nil;
end;

function TVvarMatrix.DefaultCellClass: TCellClass;
begin
  result := TVvarDepthCell;
end;

function TVvarMatrix.TotalCount: word;
var
vIdx : word;
vCell : IDepthCell;
begin
  result := 0;
  vIdx := 0;
  while vIdx < Cells.Count do
  begin
    if Cells.QueryItem(vIdx, IDepthCell, vCell) then
       inc(result, vCell.Depth);
    vCell := nil;
    inc(vIdx);
  end;
end;

function TVvarMatrix.ErrorCount: word;
var
vCellIdx, VvarIdx : word;
vCell : IDepthCell;
vVvarCell : IVvarCell;
begin
  result := 0;
  vCellIdx := 0;
  while vCellIdx < Cells.Count do
  begin
    if Cells.QueryItem(vCellIdx, IDepthCell, vCell) then
    begin
      VvarIdx := 0;
      while VvarIdx < vCell.Depth do
      begin
        if vCell.QueryItem(VvarIdx, IVvarCell, vVvarCell)
        and (vVvarCell.ErrCode <> 0)  then
            inc(result);
        vVvarCell := nil;
        inc(VvarIdx);
      end;
    end;
    vCell := nil;
    inc(vCellIdx);
  end;
end;


{ TVoltageCell }

function TValueCell.GetValue: double;
begin
  result := fValue;
end;

procedure TValueCell.SetValue(AValue: double);
begin
  fValue := AValue;
end;

{ TVoltageTable }

function TVoltageTable.DefaultCellClass: TCellClass;
begin
  result := TValueCell;
end;

function TVoltageTable.GetVoltage(ACol, ARow: byte): double;
var
vBoostCell : IValueCell;
begin
  if ByIndex(ACol, ARow, IValueCell, vBoostCell) then
    result := vBoostCell.Value
  else
    result := 0.0001;
  vBoostCell := nil;
end;

procedure TVoltageTable.SetVoltage(ACol, ARow: byte; AValue: double);
var
vBoostCell : IValueCell;
begin
  if ByIndex(ACol, ARow, IValueCell, vBoostCell) then
    vBoostCell.Value := AValue;
  vBoostCell := nil;
end;

end.
