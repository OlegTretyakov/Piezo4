unit MilandrRev8.Matrix;

interface
  uses System.Classes, ChipAbstract, ChipAbstractInterface,
  MilandrRev8IMS, MeasDataList,
  VvarMatrix, VvarMatrixIF;

  type

  TCTMilRev8 = class(TVoltageTable)
   private
    fTemprRegInfCoeff, //°C/1Inf
    fRegTemprInfCoeff, // 1Inf/°C
    fVoltsRegInfCoeff, //mvVolts/1Inf
    fRegVoltsInfCoeff // 1Inf/mvVolts
    : double;
    fCubTable : TMeasDataList;
    function GetTemprRegInfCoeff : double; stdcall;
    function GetRegTemprInfCoeff : double; stdcall;
    procedure CalcInfCoeffs(AInfTable : TVoltageTable);
   public
    constructor Create; override;
    destructor Destroy; override;
    procedure Fill(const AVvarMatrix: TVvarMatrix);
    procedure CalcVvar(const ABase, ACalc :TChipAbstract; const ASource, ADest : TMeasDataList);
    property TemprRegInfCoeff: double read GetTemprRegInfCoeff;
    property RegTemprInfCoeff: double read GetRegTemprInfCoeff;
    property CubTable : TMeasDataList read fCubTable;
  end;

implementation
uses
System.SysUtils,
System.IniFiles,
System.Math,
ThreeDimTypes,
gsl22,
Spline,
Vodopad.CustomIniHelpers,
Vodopad.FloatList,
MilandrRev8.Consts;

type

TVVarCubPoint= class(TMeasDataItem)
 strict private
  fTemprPont : double;
  fVvarCurve : TMeasDataList;
 protected
  class procedure EndUpdate(ADataList : TMeasDataList); override;
  function GetFreqCurve : TMeasDataList;
  function GetTemprPont: double;
  procedure SetTemprPont(const Value: double);
 public
  constructor Create(ADataList : TMeasDataList); override;
  destructor Destroy; override;
  property VvarCurve : TMeasDataList read GetFreqCurve;
  property TemprPont : double read GetTemprPont write SetTemprPont;
end;


constructor TVVarCubPoint.Create(ADataList: TMeasDataList);
begin
  inherited Create(ADataList);
  fVvarCurve := TMeasDataList.Create;
end;

destructor TVVarCubPoint.Destroy;
begin
  FreeAndNil(fVvarCurve);
  inherited Destroy;
end;

class procedure TVVarCubPoint.EndUpdate(ADataList: TMeasDataList);
begin
end;

function TVVarCubPoint.GetFreqCurve: TMeasDataList;
begin
  result := fVvarCurve;
end;

function TVVarCubPoint.GetTemprPont: double;
begin
  result := fTemprPont;
end;

procedure TVVarCubPoint.SetTemprPont(const Value: double);
begin
  fTemprPont := Value;
end;


{ TVvarCharactTable }

type
TVvarBitItem = record
  BitValue : Smallint;
  Voltage : double;
end;
pVvarBitItem = ^TVvarBitItem;
TSortedBitList = class(TList)
 protected
  procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  function Get(Index: Integer): TVvarBitItem;
 public
  function Add(ABitValue: Smallint; AVoltage : double):integer; reintroduce;
  procedure Clear; override;
  property Items[Index: Integer]: TVvarBitItem read Get; default;
end;

TSortedCalcBitList = class(TSortedBitList)
  function CalcBoost: double;
end;

procedure TSortedBitList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
      Dispose(Ptr);
    inherited Notify(Ptr, Action);
end;

function TSortedBitList.Get(Index: Integer): TVvarBitItem;
begin
  Result := TVvarBitItem(inherited Get(Index)^);
end;

function TSortedBitList.Add(ABitValue: Smallint; AVoltage : double):integer;
var
vItem : pVvarBitItem;
begin
  New(vItem);
  vItem.BitValue := ABitValue;
  vItem.Voltage := AVoltage;
  result := inherited Add(vItem);
end;

procedure TSortedBitList.Clear;
begin
  while count > 0 do
    delete(count -1);
    inherited;
end;


function SortByBitValue(Item1, Item2: Pointer): Integer;
begin
  result := pVvarBitItem(Item1)^.BitValue - pVvarBitItem(Item2)^.BitValue;
end;

function TSortedCalcBitList.CalcBoost: double;
var
vIdx : integer;
vCount : Word;
vVoltRange : double;
vBitRange : SmallInt;
begin
  result := 0;
  vCount := 0;
  if Count < 2 then
    exit;
  Sort(SortByBitValue);
  vIdx := 1;
  result := 0;
  while vIdx < Count do
  begin
    vVoltRange := Items[vIdx].Voltage - Items[vIdx-1].Voltage;
    vBitRange := Items[vIdx].BitValue - Items[vIdx-1].BitValue;
    if vBitRange > 0 then
    begin
      result := result + vVoltRange / vBitRange * 1000;
      inc(vCount);
    end;
    inc(vIdx);
  end;
  if (vCount > 0) then
    result := result / (vCount);
end;

constructor TCTMilRev8.Create;
begin
  inherited;
  fTemprRegInfCoeff := 0;
  fRegTemprInfCoeff := 0;
  fVoltsRegInfCoeff := 0;
  fRegVoltsInfCoeff := 0;
  fCubTable := TMeasDataList.Create();
  CubTable.ItemClass := TVVarCubPoint;
end;

destructor TCTMilRev8.Destroy;
begin
  FreeAndNil(fCubTable);
  inherited;
end;

procedure TCTMilRev8.Fill(const AVvarMatrix : TVvarMatrix);
var
vInfTable : TVoltageTable;
vMatrixCol, vBoostCol, vRow, vInfTableCol, vInfTableRow, vMeasRow, vDepthIndex, vTag : byte;
vTempr: double;
vRegisters : TMilandrRev8Registers;
vVvarBitList : TSortedCalcBitList;
vMeasRowCount, vMeasColCount : integer;
vBoostCell : IValueCell;
vDepthCell : IDepthCell;
vVvarCell : IVvarCell;
vCubTemprPont : TVVarCubPoint;
vCubItem : TMeasDataItem;
begin  
  fTemprRegInfCoeff := 0;
  self.Clear;

  vMeasRowCount := AVvarMatrix.RowCount;
  vMeasColCount := AVvarMatrix.ColCount;
  if (vMeasRowCount < 1)
  or (vMeasColCount < 1) then
    exit;
  try
    vMeasRow := 0;
    vRegisters := TMilandrRev8Registers.Create(nil);
    vVvarBitList := TSortedCalcBitList.Create;
    vInfTable := TVoltageTable.Create;
    try
      while vMeasRow < vMeasRowCount do
      begin
        if (not AVvarMatrix.ByIndex(0, vMeasRow, IDepthCell, vDepthCell))
        or (vDepthCell.ErrorsCount > 0) then
        begin
          inc(vMeasRow);
          vDepthCell := nil;
          Continue;
        end;
        vTag := vDepthCell.Row.M_Tag; 
        vMatrixCol := 0;
        if vTag = C_INF_Index then  /// укладываем INF
        begin
          while vMatrixCol < vMeasColCount do
          begin
            if (not AVvarMatrix.ByIndex(vMatrixCol, vMeasRow, IDepthCell, vDepthCell))
            or (vDepthCell.Depth < 1)
            or (vDepthCell.ErrorsCount > 0) then
            begin
              inc(vMatrixCol);
              vDepthCell := nil;
              Continue;
            end;
            vTempr := vDepthCell.Col.Header;
            vInfTableCol := vInfTable.AddCol(vTempr);
            vDepthIndex := 0;
            while vDepthIndex < vDepthCell.Depth do
            begin
              if vDepthCell.QueryItem(vDepthIndex, IVvarCell, vVvarCell)
              and (vVvarCell.ErrCode = 0) then
              begin
                vRegisters.LoadFromStream(vVvarCell.Memory);
                vInfTableRow := vInfTable.AddRow(vRegisters.BitValue[vTag]);
                if vInfTable.ByIndex(vInfTableCol, vInfTableRow, IValueCell, vBoostCell) then
                  vBoostCell.Value := vVvarCell.Voltage;
                vBoostCell := nil;
              end;
              vVvarCell := nil;
              inc(vDepthIndex);
            end;
            vDepthCell := nil;
            inc(vMatrixCol);
          end;
        end else// if (vTag in [C_LIN_Index, C_FOUR_Index, C_FIFTH_Index, C_OFS_Index]) then
        // укладываем остальные
        begin
          vRow := AddRow(vTag);
          while vMatrixCol < vMeasColCount do
          begin
            if (not AVvarMatrix.ByIndex(vMatrixCol, vMeasRow, IDepthCell, vDepthCell)) then
            begin
              inc(vMatrixCol);
              vDepthCell := nil;
              Continue;
            end;
            vTempr := vDepthCell.Col.Header;
            if (vDepthCell.ErrorsCount > 0) then 
            begin
              inc(vMatrixCol);
              vDepthCell := nil;
              Continue;
            end;
            if vRow = 0 then
             AddCol(vTempr);
            vDepthIndex := 0;
            vVvarBitList.Clear;
            while (vDepthIndex < vDepthCell.Depth) do
            begin
              if vDepthCell.QueryItem(vDepthIndex, IVvarCell, vVvarCell)
              and (vVvarCell.ErrCode = 0) then
              begin
                vRegisters.LoadFromStream(vVvarCell.Memory);
                vVvarBitList.Add(vRegisters.BitValue[vTag], vVvarCell.Voltage);
              end;
              vVvarCell := nil;
              inc(vDepthIndex);
            end;
            vBoostCell := nil;
            if FindCol(vTempr, vBoostCol)
            and ByIndex(vBoostCol, vRow, IValueCell, vBoostCell) then
               vBoostCell.Value := vVvarBitList.CalcBoost;
            vBoostCell := nil;
            vDepthCell := nil;
            inc(vMatrixCol);
          end;
        end;
        inc(vMeasRow);
      end;
      CubTable.ClearData;
      vMatrixCol := 0;
      vDepthCell := nil;
      while vMatrixCol < AVvarMatrix.ColCount do
      begin
        vVvarBitList.Clear;
        if AVvarMatrix.ByParams(vMatrixCol, C_CUB_Index, IDepthCell, vDepthCell)
        and (vDepthCell.ErrorsCount = 0) then
        begin
          vDepthIndex := 0;
          while (vDepthIndex < vDepthCell.Depth) do
          begin
            if vDepthCell.QueryItem(vDepthIndex, IVvarCell, vVvarCell) then
            begin
              vRegisters.LoadFromStream(vVvarCell.Memory);
              vVvarBitList.Add(vRegisters.BitValue[C_CUB_Index], vVvarCell.Voltage);
            end;
            vVvarCell := nil;
            inc(vDepthIndex);
          end;
          vVvarBitList.Sort(SortByBitValue);
          if vVvarBitList.Count >= 3 then
          begin
            vCubTemprPont := CubTable.NewItem as TVVarCubPoint;
            vCubTemprPont.TemprPont := vDepthCell.Col.Header;
            vCubTemprPont.XValue := vDepthCell.Col.Header;
            vDepthIndex := 0;
            vCubTemprPont.VvarCurve.BeginUpdate;
            try
              while (vDepthIndex < vVvarBitList.Count) do
              begin
                vCubItem := vCubTemprPont.VvarCurve.NewItem;
                vCubItem.XValue := vVvarBitList[vDepthIndex].BitValue;
                vCubItem.YValue := vVvarBitList[vDepthIndex].Voltage;
                inc(vDepthIndex);
              end;
            finally
              vCubTemprPont.VvarCurve.EndUpdate;
            end;
          end;
        end;
        inc(vMatrixCol);
        vDepthCell := nil;
      end;
      CalcInfCoeffs(vInfTable);
    finally
      FreeAndNil(vRegisters);
      FreeAndNil(vVvarBitList);
      FreeAndNil(vInfTable);
    end;
  except
    fTemprRegInfCoeff := 0;
    self.Clear;
  end;
end;

function TCTMilRev8.GetRegTemprInfCoeff: double;
begin
  result := fRegTemprInfCoeff;
end;

function TCTMilRev8.GetTemprRegInfCoeff: double;
begin
  result := fTemprRegInfCoeff;
end;


type
TLinFittParams = packed record
  CTable : TCTMilRev8;
  InfTable: TVoltageTable;
  InfSpline1, InfSpline2 : TMeasDataList;
end;
pLinFittParams = ^TLinFittParams;


function CalcError (const v :pGSL_vector; params : pvoid) : double ; cdecl;
var
vParams : pLinFittParams;
vItem: TMeasDataItem;
vdTempr, vdVolts, vMinTempr, vMaxTempr, vError, vSseDev : double;
vIdx, vSseCount : Word;
begin
  vParams := pLinFittParams(params);
  result  := 0;
  if (vParams.InfTable.RowCount < 2)
  or (vParams.InfTable.ColCount < 3) then
    exit;
  vMinTempr := vParams.InfTable.Header(0);
  vMaxTempr := vParams.InfTable.Header(vParams.InfTable.ColCount-1);
  vdTempr := gsl_vector_get(v, 0);
  vdVolts := gsl_vector_get(v, 1);
  if ((vMinTempr + vdTempr) > vMinTempr) then
    vMinTempr := vMinTempr + vdTempr;
  if ((vMaxTempr + vdTempr) < vMaxTempr) then
    vMaxTempr := vMaxTempr + vdTempr;
  vIdx := 0;
  vParams.InfSpline1.ClearData;
  vParams.InfSpline2.ClearData;
  vParams.InfSpline1.BeginUpdate;
  vParams.InfSpline2.BeginUpdate;
  try
    while vIdx < vParams.CTable.ColCount do
    begin
      if (((vParams.InfTable.Header(vIdx) > vMinTempr)
      or (SameValue(vParams.InfTable.Header(vIdx), vMinTempr, 0.01)))
      and ((vParams.InfTable.Header(vIdx) < vMaxTempr)
      or (SameValue(vParams.InfTable.Header(vIdx), vMaxTempr, 0.01)))) then
      begin
        vItem := vParams.InfSpline1.NewItem;
        vItem.XValue := vParams.InfTable.Header(vIdx);
        vItem.YValue := vParams.InfTable.Voltage[vIdx, 0];
        vItem := vParams.InfSpline2.NewItem;
        vItem.XValue := vParams.InfTable.Header(vIdx)+vdTempr;
        vItem.YValue := vParams.InfTable.Voltage[vIdx, 1]+vdVolts;
      end;
      inc(vIdx);
    end;
  finally
    vParams.InfSpline1.EndUpdate;
    vParams.InfSpline2.EndUpdate;
  end;
  if (vParams.InfSpline1.Splines.Count < 1)
  or (vParams.InfSpline1.Splines[0].Count < 3)
  or (vParams.InfSpline2.Splines.Count < 1)
  or (vParams.InfSpline2.Splines[0].Count < 3) then
    exit;

  vIdx := 0;
  vSseCount := 0;
  vSseDev := 0;
  while vIdx < vParams.InfTable.ColCount do
  begin
    if (((vParams.InfTable.Header(vIdx) > vMinTempr)
    or (SameValue(vParams.InfTable.Header(vIdx), vMinTempr, 0.01)))
    and ((vParams.CTable.Header(vIdx) < vMaxTempr)
    or (SameValue(vParams.InfTable.Header(vIdx), vMaxTempr, 0.01)))) then
    begin
      vError := Abs(vParams.InfSpline1.YX(vParams.InfTable.Header(vIdx)) - vParams.InfSpline2.YX(vParams.InfTable.Header(vIdx)));
      vSseDev := vSseDev + Power(vError, 2);
      inc(vSseCount);
    end;
    inc(vIdx);
  end;
  result := sqrt(vSseDev) / vSseCount;
end;

type
TIniInfCalcParams = record
  StartStepSize,
  EndStepSize,
  StartDeltaTempr,
  StartDeltaVolts : double;
  MinimizeIters :LongWord;
  procedure LoadIni;
end;

procedure TIniInfCalcParams.LoadIni;
var
f:TIniFile;
vFormat : TFormatSettings;
begin
  f := TIniFile.Create(ExtractFilePath(ParamStr(0))+C_ChipName+'.ini');
  vFormat := TFormatSettings.Create(1033);
  try
    StartStepSize := ReadFloat(f, 'CalcInfParams', 'StartStepSize', 1, vFormat);
    EndStepSize := ReadFloat(f, 'CalcInfParams', 'EndStepSize', 1e-2, vFormat);;
    StartDeltaTempr := ReadFloat(f, 'CalcInfParams', 'StartDeltaTempr', -11, vFormat);
    StartDeltaVolts := ReadFloat(f, 'CalcInfParams', 'StartDeltaVolts', 0.06, vFormat);
    MinimizeIters:= EnsureRange(f.ReadInteger('CalcInfParams', 'MinimizeIters', 100), 1, 10000);
  finally
    FreeAndNil(f);
  end;
end;

var
IniInfCalcParams : TIniInfCalcParams;

procedure TCTMilRev8.CalcInfCoeffs(AInfTable: TVoltageTable);
var
vInfSpline1 : TMeasDataList;
vInfSpline2 : TMeasDataList;
vDimSize :Size_t;
//vRegs : Smallint;
vSSVect, vMinimizeVect: pGSL_vector;
minex_func : gsl_multimin_function_struct;

MinimizerType : pGSL_multimin_fminimizer_type;
vFminimizer : pGSL_multimin_fminimizer;
vMinimizerIter : size_t;
vFminimizer_size,
vCalcInfDeltaTempr,
vCalcInfDeltaVolts: double;
vSizeStatus : integer;
vParams : TLinFittParams;
begin
  if (AInfTable.RowCount < 2)
  or (AInfTable.ColCount < 3) then
    exit;
  vDimSize := 2;
  vSSVect := gsl_vector_alloc(vDimSize);
  vMinimizeVect := gsl_vector_alloc (vDimSize);
  vInfSpline1 := TMeasDataList.Create();
  vInfSpline2 := TMeasDataList.Create();
  vParams.InfSpline1 := vInfSpline1;
  vParams.InfSpline2 := vInfSpline2;
  vParams.InfTable := AInfTable;
  vParams.CTable := self;
  try
    vCalcInfDeltaTempr := IniInfCalcParams.StartDeltaTempr;
    vCalcInfDeltaVolts := IniInfCalcParams.StartDeltaVolts;
    minex_func.f := @CalcError;
    minex_func.n := vDimSize;
    minex_func.params := pvoid(@vParams);
    gsl_vector_set_all(vSSVect, IniInfCalcParams.StartStepSize);
    gsl_vector_set(vMinimizeVect, 0, vCalcInfDeltaTempr);
    gsl_vector_set(vMinimizeVect, 1, vCalcInfDeltaVolts);
    MinimizerType := gsl_multimin_fminimizer_nmsimplex;
    vFminimizer := gsl_multimin_fminimizer_alloc (MinimizerType, vDimSize);
    try
      { Initialize method and iterate }
      gsl_multimin_fminimizer_set (vFminimizer, @minex_func, vMinimizeVect, vSSVect);
      vMinimizerIter := 0;
      repeat
        inc(vMinimizerIter);
        vSizeStatus := gsl_multimin_fminimizer_iterate(vFminimizer);
        if (vSizeStatus > 0) then
          break;
        vFminimizer_size := gsl_multimin_fminimizer_size(vFminimizer);
        vSizeStatus := gsl_multimin_test_size (vFminimizer_size, IniInfCalcParams.EndStepSize);
      until (vSizeStatus <> GSL_CONTINUE) or (vMinimizerIter >= IniInfCalcParams.MinimizeIters);
      if  (vSizeStatus = GSL_SUCCESS) then
      begin
        vCalcInfDeltaTempr := gsl_vector_get(vFminimizer.x, 0);
        vCalcInfDeltaVolts := gsl_vector_get(vFminimizer.x, 1);
        if AInfTable.Tag(0) <> AInfTable.Tag(1) then
        begin
          fTemprRegInfCoeff := vCalcInfDeltaTempr / (AInfTable.Tag(1) - AInfTable.Tag(0));
          fRegTemprInfCoeff := 1/fTemprRegInfCoeff;
          fVoltsRegInfCoeff := vCalcInfDeltaVolts / (AInfTable.Tag(1) - AInfTable.Tag(0));
          fRegVoltsInfCoeff := 1/fVoltsRegInfCoeff;
        end;
      end;
    finally
      gsl_multimin_fminimizer_free (vFminimizer);
    end;
  finally
    vParams.InfSpline1 := nil;
    vParams.InfSpline2 := nil;
    FreeAndNil(vInfSpline1);
    FreeAndNil(vInfSpline2);
    gsl_vector_free(vMinimizeVect);
    gsl_vector_free(vSSVect);
  end;
end;

procedure TCTMilRev8.CalcVvar(const ABase, ACalc: TChipAbstract; const ASource, ADest : TMeasDataList);
const
C_CalcBitsIdx : array[0..3] of byte = (C_LIN_Index, C_FOUR_Index, C_FIFTH_Index, C_OFS_Index);
var
vArrIdx, vCol : byte;
vDInfTempr, vDInfVolts, vVVarBase, vVvarCub, vCubBoost, vCurrTempr: double;
vCalcItem : TMeasDataItem;
vBoostCell : IValueCell;
vVvarCubPont : TVVarCubPoint;
vColCount, vCubPontIdx : integer;
begin  
  ADest.ItemClass := TMeasDataItem;
  ADest.ClearData;
  vColCount := self.ColCount;
  try
    if (not IsEqualGUID(ABase.ChipGUID, MilandrRev8.Consts.C_GUID))
    or (not IsEqualGUID(ACalc.ChipGUID, MilandrRev8.Consts.C_GUID))  then
      Exit;

    if (ASource.Splines.Count < 1)
      or (ASource.Splines[0].Count < 3) then
      Exit;

    vDInfTempr := (ACalc.BitValue[C_INF_Index] - ABase.BitValue[C_INF_Index]) * fTemprRegInfCoeff;
    vDInfVolts := (ACalc.BitValue[C_INF_Index] - ABase.BitValue[C_INF_Index]) * fVoltsRegInfCoeff;

    vCol := 0;
    ADest.BeginUpdate;
    try
      while vCol < vColCount do
      begin
        vCubBoost := 0;
        vCurrTempr := Header(vCol);
        vVVarBase := ASource.YX(vCurrTempr);
        if  CubTable.XExists(vCurrTempr, 0.5, vCubPontIdx)
        and CubTable.QueryItem(vCubPontIdx, TVVarCubPoint, vVvarCubPont)
        and (vVvarCubPont.VvarCurve.Splines.Count > 0)
        and (vVvarCubPont.VvarCurve.Splines[0].Count >= 3) then
        begin
          vVvarCub := vVvarCubPont.VvarCurve.YX(ACalc.BitValue[C_CUB_Index]);
          vCubBoost := (vVvarCub - vVvarCubPont.VvarCurve.YX(ABase.BitValue[C_CUB_Index]));
        end;
        vCalcItem  := ADest.NewItem;
        vCalcItem.XValue := vCurrTempr-vDInfTempr;
        vCalcItem.YValue := vVVarBase + vCubBoost;
        for vArrIdx := 0 to High(C_CalcBitsIdx) do
        begin
          if ByParams(vCol, C_CalcBitsIdx[vArrIdx], IValueCell, vBoostCell) then
                vCalcItem.YValue := vCalcItem.YValue +
                (ACalc.BitValue[C_CalcBitsIdx[vArrIdx]] - ABase.BitValue[C_CalcBitsIdx[vArrIdx]]) * vBoostCell.Value / 1000;
          vBoostCell := nil;
        end;
        vCalcItem.YValue := vCalcItem.YValue - vDInfVolts;
        inc(vCol);
      end;
    finally
      ADest.EndUpdate;
    end;
  except
    ADest.ClearData;
  end;
end;

initialization
 IniInfCalcParams.LoadIni;

finalization

end.
