unit MAS6279D8.Matrix;

interface
  uses
  System.Classes, ChipAbstract, ChipAbstractInterface,
  mas6279d8.DboBase, MAS6279D8IMS, MeasDataList,
  VvarMatrix, VvarMatrixIF;

  type

  TCTMasD8 = class(TVoltageTable)
   private
    fTemprRegInfCoeff, //°C/1Inf
    fRegTemprInfCoeff // 1Inf/°C
    : double;
    function GetTemprRegInfCoeff : double; stdcall;
    function GetRegTemprInfCoeff : double; stdcall;
   public
    procedure Fill(const AVvarMatrix: TVvarMatrix);
    procedure CalcVvar(const ABase, ACalc : TChipAbstract; const ASource, ADest : TMeasDataList);
    property TemprRegInfCoeff: double read GetTemprRegInfCoeff;
    property RegTemprInfCoeff: double read GetRegTemprInfCoeff;
  end;

implementation
uses
System.SysUtils,
ThreeDimTypes,
Vodopad.FloatList,
MAS6279D8.Consts;


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

procedure TCTMasD8.Fill(const AVvarMatrix : TVvarMatrix);
var
vInfTable : TVoltageTable;
vMatrixCol, vBoostCol, vRow, vInfTableCol, vInfTableRow, vMeasRow, vInfBaseCount, vDepthIndex, vTag : byte;
vTempr, vInfBaseVvar: double;
vCRegCoef : double;
vRegisters : TMas6279D8Registers;
vVvarBitList : TSortedCalcBitList;
vInfList : TSortedBitList;
vMidPoints, vCRegCoefs, vRegCCoefs : TxFloatList;
vMeasRowCount, vMeasColCount : integer;
vBoostCell : IValueCell;
vDepthCell : IDepthCell;
vVvarCell : IVvarCell;
begin  
  fTemprRegInfCoeff := 0;
  vInfBaseVvar := 0;
  vInfBaseCount := 0;
  self.Clear;

  vMeasRowCount := AVvarMatrix.RowCount;
  vMeasColCount := AVvarMatrix.ColCount;
  if (vMeasRowCount < 1)
  or (vMeasColCount < 1) then
    exit;
  try
    vMeasRow := 0;
    vRegisters := TMas6279D8Registers.Create(nil);
    vVvarBitList := TSortedCalcBitList.Create;
    vInfList := TSortedBitList.Create; 
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
        vDepthCell := nil; 
        if vTag = 3 then  /// укладываем INF
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
            vInfList.Clear;
            while (vDepthIndex < vDepthCell.Depth) do
            begin
              if vDepthCell.QueryItem(vDepthIndex, IVvarCell, vVvarCell)
              and (vVvarCell.ErrCode = 0) then
              begin
                vRegisters.LoadFromStream(vVvarCell.Memory);
                if (vDepthIndex = 0) then
                begin
                  vInfBaseVvar := vInfBaseVvar + vVvarCell.Voltage;
                  inc(vInfBaseCount);
                end else
                  vInfList.Add(vRegisters.BitValue[vTag], vVvarCell.Voltage);
              end;
              vVvarCell := nil;
              inc(vDepthIndex);
            end;
            vDepthIndex := 0;
            vInfList.Sort(SortByBitValue);
            while vDepthIndex < vInfList.Count do
            begin
              if not vInfTable.FindRow(vInfList[vDepthIndex].BitValue, vInfTableRow) then
                 vInfTableRow := vInfTable.AddRow(vInfList[vDepthIndex].BitValue);
              if vInfTable.ByIndex(vInfTableCol, vInfTableRow, IValueCell, vBoostCell) then
                vBoostCell.Value := vInfList[vDepthIndex].Voltage;
              vBoostCell := nil;
              inc(vDepthIndex);
            end;
            vDepthCell := nil;
            inc(vMatrixCol);
          end;
        end else if (vTag in [4..9]) then // укладываем остальные
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
      if (vInfBaseCount = 0) then
        exit;
      vInfBaseVvar := vInfBaseVvar / vInfBaseCount; // усреднили Vvar при начальном базовом INF
      {Расчет fInfCoeff из fInfTable}
      if (vInfTable.ColCount < 2)
      or (vInfTable.RowCount < 2) then {Если температурных или регистровых точек менее 2 - ничего не считаем}
        exit;
      {MidPointInf[i]=(Средний VVar-Vvar[i]TemprLow)/(Vvar[i]TemprHi-Vvar[i]TemprLow)*(TemprHi-TemprLow)}
      {°C/reg[m] = (MidPointInf[i+1]-MidPointInf[i])/(InfValue[i+1]-InfValue[i])
      fInfCoeff = Среднее(°C/reg[m..]) }
      vMidPoints := TxFloatList.Create;
      vCRegCoefs := TxFloatList.Create;
      vRegCCoefs := TxFloatList.Create;
      try
        vMeasRow := 0;
        {Структура полученой таблицы fInfTable
          Col->°С 0  |  35  |
         \/Row
        INF 0  |Volt   Volt
        INF 64 |Volt   Volt
        INF 128|Volt   Volt
        INF 192|Volt   Volt
        INF 256|Volt   Volt
        }
        {Двигаемся по измерениям, начиная от нуля,
        формируем список MidPoints}
        while vMeasRow < vInfTable.RowCount do
        begin
          vMidPoints.Add(
          (vInfBaseVvar - vInfTable.Voltage[0, vMeasRow]) /
          (vInfTable.Voltage[vInfTable.ColCount-1, vMeasRow]- vInfTable.Voltage[0, vMeasRow])*
          {Берем только крайние температурные точки измерения INF-а}
          (vInfTable.Header(byte(vInfTable.ColCount-1)) - vInfTable.Header(0)){+5});
          inc(vMeasRow);
        end;
        vMeasRow := 1;
        {Двигаемся по измерениям, начиная с единицы,
        формируем списки vCRegCoefs и vRegCCoefs}
        while vMeasRow < vInfTable.RowCount do
        begin
          if (vInfTable.Tag(vMeasRow) <> vInfTable.Tag(vMeasRow-1)) then
          begin
            vCRegCoef :=
              (vMidPoints[vMeasRow] - vMidPoints[vMeasRow-1]) /
              (vInfTable.Tag(vMeasRow) - vInfTable.Tag(vMeasRow-1));
            vCRegCoefs.Add(vCRegCoef);
            if (vCRegCoef <> 0) then
              vRegCCoefs.Add(1/vCRegCoef);
          end;
          inc(vMeasRow);
        end;
        {Считаем средние значения TemprRegInfCoeff и RegTemprInfCoeff}
        if vCRegCoefs.Count > 0 then
          fTemprRegInfCoeff := vCRegCoefs.Sum / vCRegCoefs.Count;
        if vRegCCoefs.Count > 0 then
          fRegTemprInfCoeff := vRegCCoefs.Sum / vRegCCoefs.Count;
      finally
        FreeAndNil(vMidPoints);
        FreeAndNil(vCRegCoefs);
        FreeAndNil(vRegCCoefs);
      end;
    finally
      FreeAndNil(vRegisters);
      FreeAndNil(vVvarBitList);
      FreeAndNil(vInfList);
      FreeAndNil(vInfTable);
    end;
  except
    fTemprRegInfCoeff := 0;
    self.Clear;
  end;
end;

function TCTMasD8.GetRegTemprInfCoeff: double;
begin
  result := fRegTemprInfCoeff;
end;

function TCTMasD8.GetTemprRegInfCoeff: double;
begin
  result := fTemprRegInfCoeff;
end;

procedure TCTMasD8.CalcVvar(const ABase, ACalc: TChipAbstract; const ASource, ADest : TMeasDataList);
var
vBitIndex, vCol : byte;
vDInf: double;
vCalcItem : TMeasDataItem;
vBoostCell : IValueCell;
vVarCurr : TMeasDataItem;
vColCount : integer;
begin  
  ADest.ItemClass := TMeasDataItem;
  ADest.ClearData;
  vColCount := self.ColCount;
  try
    if (not IsEqualGUID(ABase.ChipGUID, MAS6279D8.Consts.C_GUID))
    or (not IsEqualGUID(ACalc.ChipGUID, MAS6279D8.Consts.C_GUID))  then
      Exit;

    if (ASource.Splines.Count < 1)
      or (ASource.Splines[0].Count < 3)
      or (not ASource.QueryItem(0, TMeasDataItem, vVarCurr)) then
      Exit;

    vDInf := (ACalc.BitValue[3] - ABase.BitValue[3]) * fTemprRegInfCoeff;

    vCol := 0;
    ADest.BeginUpdate;
    try
      while vCol < vColCount do
      begin
        vCalcItem  := ADest.NewItem;
        vCalcItem.XValue := Header(vCol)+vDInf;
        vCalcItem.YValue := vVarCurr.YX(Header(vCol));
        for vBitIndex := 4 to 9 do
        begin
          if ByParams(vCol, vBitIndex, IValueCell, vBoostCell) then
                vCalcItem.YValue := vCalcItem.YValue +
                (ACalc.BitValue[vBitIndex] - ABase.BitValue[vBitIndex]) * vBoostCell.Value / 1000;
          vBoostCell := nil;
        end;
        inc(vCol);
      end;
    finally
      ADest.EndUpdate;
    end;
  except
    ADest.ClearData;
  end;
end;

end.
