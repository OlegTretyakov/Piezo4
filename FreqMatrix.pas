unit FreqMatrix;
{$DebugInfo OFF}
{$LocalSymbols OFF}
{$ReferenceInfo OFF}
{$RangeChecks OFF}
{$OverflowChecks OFF}
{$Optimization ON}  
interface
uses Classes, MultiDimMatrix,
ThreeDimTypes, ThreeDimMatrix, FreqMatrixIF;
type

  TFreqMeasCell = class(T3DimCell, IFreqMeasCell)
   private
    fErrCode : SmallInt;
    fIzmNum : LongWord;
    fFreq, fDisp : double;
    fVar : byte;
    fMemory : TMemoryStream;
   protected
    function GetErrCode: SmallInt; stdcall;
    function GetIzmNum: LongWord; stdcall;
    function GetMemory: TMemoryStream; stdcall;
    function GetFreq: double; stdcall;   
    function GetDisp: double; stdcall;
    procedure SetErrCode(const Value: SmallInt);stdcall;
    procedure SetIzmNum(const Value: LongWord); stdcall;
    procedure SetFreq(const Value: double); stdcall; 
    procedure SetDisp(const Value: double); stdcall;
    function GetVar: byte; stdcall;
    procedure SetVar(const Value: byte); stdcall;
   public
    constructor Create(AOwner: TComponent; const ADimCell : IDepthCell);override;
    destructor Destroy; override;
    property ErrCode : SmallInt read GetErrCode write SetErrCode;
    property IzmNum : LongWord read GetIzmNum write SetIzmNum; 
    property Freq : double read GetFreq write SetFreq;   
    property Disp : double read GetDisp write SetDisp;
    property mVar : byte read GetVar write SetVar;
    property Memory : TMemoryStream read GetMemory;
  end;

  TFreqDepthCell = class(TDepthCell)
   protected
    function DefaultCellClass: T3DimCellClass; override;
  end;


  {Структура таблицы TFreqMatrix
    3DimCell = это ячейка третьего измерения с контейнером для произвольных данных
     Col->°С |  -60     |..|    85    | <-Header
       \/Row
      Tag 1  |3DimCell  |..| 3DimCell |
      Tag 2  |3DimCell  |..| 3DimCell |
      Tag 3  |3DimCell  |..| 3DimCell |
      ............
      Tag 6  |3DimCell  |..| 3DimCell |

      }

  TFreqMatrix = class(TMultiDimMatrix, IFreqMatrix)
   protected
    function DefaultCellClass : TCellClass; override;
   public
    function ByVar(ACol : byte; ATag : SmallInt; AVar: byte; const IID: TGUID; out Obj):boolean; stdcall; 
    function TotalCount : word; stdcall;
    function ErrorCount : word; stdcall;
  end;   
  TFreqMatrixClass = class of TFreqMatrix;

  {Структура таблицы TFreqTable
        Col->°С -60 |..|  85  | <-Header
       \/Row
      Tag 1  |Freq  |..| Freq |
      Tag 2  |Freq  |..| Freq |
      Tag 3  |Freq  |..| Freq |
      ............
      Tag 6  |Freq  |..| Freq |

      }    

  TValueCell = class(TCell, IValueCell)
   private
    fValue : double;
   protected
    function GetValue: double; stdcall;
    procedure SetValue(AValue : double); stdcall;
   public
    property Value: double read GetValue write SetValue;
  end;

  TFreqTable = class(TMultiDimMatrix, IFreqTable)
   private
    fNominal : Double;
   protected   
    function GetFreq(ACol, ARow : byte): double; stdcall;
    procedure SetFreq(ACol, ARow : byte; AValue : double); stdcall; 
    function GetNominal: double;stdcall;
    procedure SetNominal(AValue : double); stdcall;  
    function GetDeviation(ACol, ARow : byte): double;stdcall;
    function DefaultCellClass : TCellClass; override;
   public
    property Freq[ACol, ARow : byte]: double read GetFreq write SetFreq;
    property Nominal: double read GetNominal write SetNominal;
    property Deviation[ACol, ARow : byte]: double read GetDeviation;
  end;

  TFreqTableClass = class of TFreqTable;

implementation
uses SysUtils, vdMath;

{ TFreqMeasCell }

constructor TFreqMeasCell.Create(AOwner: TComponent; const ADimCell: IDepthCell);
begin
  inherited Create(AOwner, ADimCell);
  fMemory := TMemoryStream.Create;
end;

destructor TFreqMeasCell.Destroy;
begin
  FreeAndNil(fMemory);
  inherited;
end;

function TFreqMeasCell.GetErrCode: SmallInt;
begin
  result := fErrCode;
end;

function TFreqMeasCell.GetIzmNum: LongWord;
begin
  result := fIzmNum;
end;

function TFreqMeasCell.GetMemory: TMemoryStream;
begin
  fMemory.Position := 0;
  result := fMemory;
end;

function TFreqMeasCell.GetDisp: double;
begin
  Result := fDisp;
end;

function TFreqMeasCell.GetFreq: double;
begin
  result := fFreq;
end;

function TFreqMeasCell.GetVar: byte;
begin
  result := fVar;
end;

procedure TFreqMeasCell.SetErrCode(const Value: SmallInt);
begin
  fErrCode := Value;
end;

procedure TFreqMeasCell.SetIzmNum(const Value: LongWord);
begin
  fIzmNum := Value;
end;

procedure TFreqMeasCell.SetDisp(const Value: double);
begin
  fDisp := Value;
end;

procedure TFreqMeasCell.SetFreq(const Value: double);
begin
  fFreq := Value;
end;

procedure TFreqMeasCell.SetVar(const Value: byte);
begin
  fVar := Value;
end;

{ VvarDepthCell }

function TFreqDepthCell.DefaultCellClass: T3DimCellClass;
begin
  result := TFreqMeasCell;
end;

{ TVvarMeasTable }

function TFreqMatrix.ByVar(ACol : byte; ATag : SmallInt; AVar: byte; const IID: TGUID; out Obj): boolean;
var
vDepthCell : IDepthCell;
begin
  result := ByParams(ACol, ATag, IDepthCell, vDepthCell)
    and (vDepthCell.QueryItem(AVar, IID, Obj));
  vDepthCell := nil;
end;

function TFreqMatrix.DefaultCellClass: TCellClass;
begin
  result := TFreqDepthCell;
end;

function TFreqMatrix.TotalCount: word;
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

function TFreqMatrix.ErrorCount: word;
var
vCellIdx, VvarIdx : word;
vCell : IDepthCell;
vVvarCell : IFreqMeasCell;
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
        if vCell.QueryItem(VvarIdx, IFreqMeasCell, vVvarCell)
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

function TFreqTable.DefaultCellClass: TCellClass;
begin
  result := TValueCell;
end;

function TFreqTable.GetFreq(ACol, ARow: byte): double;
var vValueCell : IValueCell;
begin
  if ByIndex(ACol, ARow, IValueCell, vValueCell) then
    result := vValueCell.Value;
end;

procedure TFreqTable.SetFreq(ACol, ARow: byte; AValue: double);
var vBoostCell : IValueCell;
begin
  if ByIndex(ACol, ARow, IValueCell, vBoostCell) then
    vBoostCell.Value := AValue; 
end;   

function TFreqTable.GetNominal: double;
begin
  Result := fNominal;
end;

procedure TFreqTable.SetNominal(AValue: double);
begin
  fNominal := AValue;
end; 

function TFreqTable.GetDeviation(ACol, ARow: byte): double;
var vValueCell : IValueCell;
begin
  if ByIndex(ACol, ARow, IValueCell, vValueCell) then
    result := ppm(fNominal, vValueCell.Value);
end;  

end.
