//{/$/define TEST_TRACE}

unit MAS6279D8.ReconstructorThread;

interface



implementation

uses
System.Classes,
System.SysUtils,
System.IniFiles,
System.Math,
System.Generics.Defaults,
System.Generics.Collections,
Winapi.Windows,
Vodopad.CustomIniHelpers,
Vodopad.Math,
VdpDMAccess,
ToleranceObj,
ChipAbstractInterface,
MAS6279D8IMS,
MAS6279D8.Consts,
LoggerInterface,
StpProcessTypes,
AbstractReconstructorThread,
Stp8PositionDBExtention,
mas6279dStp8dboExtention,
mas6279d8.DboBase,
mas6279d8.DboMain,
gsl22;



type
  TIniParams = record
    StepSize : double;
    MinimizeIters,
    MajorIters,
    MinorIters :LongWord;
    MVBC : Byte;
    procedure LoadIni;
  end;

procedure TIniParams.LoadIni;
var
f:TIniFile;
vFormat : TFormatSettings;
begin
  f := TIniFile.Create(ExtractFilePath(ParamStr(0))+C_ChipName+'.ini');
  vFormat := TFormatSettings.Create(1033);
  try
    StepSize := ReadFloat(f, 'CalcParams', 'StepSize', 0.7, vFormat);
    MinimizeIters:= EnsureRange(f.ReadInteger('CalcParams', 'MinimizeIters', 100), 1, 10000);
    MajorIters:= EnsureRange(f.ReadInteger('CalcParams', 'MajorIters', 60), 1, 10000);
    MinorIters:= EnsureRange(f.ReadInteger('CalcParams', 'MinorIters', 10), 1, 10000);
    MVBC := EnsureRange(f.ReadInteger('CalcParams', 'MVBC', 13), 1, 254);
  finally
    FreeAndNil(f);
  end;
end;

type
TToleranceGroupEx = class(TToleranceGroup)
  CardID : Integer;
end;

TRegistersWeightsGroup = class(TObject)
 strict private
 fIters : TObjectList<TRegistersDouble>;
 public
  ID : Integer;
  constructor Create(AID : Integer);
  destructor Destroy; override;
  property Iters : TObjectList<TRegistersDouble> read fIters;
end;

procedure RegistersWeightsGroupLoad(const ADest : TRegistersWeightsGroup; const AConnection : TvdConnection; const ALoader : IPatternLoader);
var
i : Byte;
vSt : TStringList;
vFile : TMemIniFile;
fFs : TFormatSettings;
begin
  vSt := TStringList.Create;
  vFile := TMemIniFile.Create('');
  fFs := TFormatSettings.Create(1033);
  try
    ADest.Iters.Clear;
    ALoader.Load(AConnection, ADest.ID, vFile);
    vFile.ReadSections(vST);
    if vST.Count < 1 then
      Exit;
    for I := 0 to vST.Count -1 do
    begin
      ADest.Iters.Add(TRegistersDouble.Create);
      ADest.Iters.Last.Read(vFile, vST[i], fFs);
    end;
  finally
    FreeAndNil(vFile);
    FreeAndNil(vSt);
  end;
end;

constructor TRegistersWeightsGroup.Create(AID : Integer);
begin
  fIters := TObjectList<TRegistersDouble>.Create;
  Self.ID := AID;
end;

destructor TRegistersWeightsGroup.Destroy;
begin
  FreeAndNil(fIters);
  inherited;
end;

type
TMasD8Reconstructor = class(TReconstructorAbstractThread)
 private
  fConnection : TvdConnection;
  fDMAccess : IDMAccess;
  fLoader : IPatternLoader;
  fMinimizationRegisters : TChipBytes;
  fIniParams : TIniParams;
  fTolerances : TObjectList<TToleranceGroupEx>;
  fProcessOptions : TObjectList<TStp8masProcessDBOptions>;
  fRegistersWeights : TObjectList<TRegistersWeightsGroup>;
  function GetProcessOptions(ACardID : Integer; var oOptions : TStp8masProcessDBOptions): boolean;
  function GetTolerance(ACardID: Integer): TToleranceGroupEx;
  function GetRegistersWeightsGroup(AID: Integer): TRegistersWeightsGroup;
 protected
  procedure AfterCreate; override;
  procedure Execute; override;
end;

TRootMeasureObj = TMeasureSolutionCurves<TCalcCurves>;

TRecostructionObjects = packed record
  Reconstructor : TMasD8Reconstructor;
  MeasObj : TStp8masDbExtention;
  DbObj : TMasResultsDBObj;
  Iter : TMasIterationResults;
  RangeOptions : TInRangeProcOptionSet;
  CurrMeasure : TRootMeasureObj;
  DestRegisters : TMas6279D8Registers;
  ToleranceGroup : TToleranceGroupEx;
  ProcessOptions : TStp8masProcessDBOptions;
  AddProgQR : TvdQuery;
end;
pRecostructionObjects = ^TRecostructionObjects;


function RecostructorClass: TReconstructorThreadClass;stdcall;
begin
  Result := TMasD8Reconstructor;
end; exports RecostructorClass;

procedure ProcessingStates(AObj : pRecostructionObjects);
function AddToProg : Boolean;
const
C_ToProgInsertText = 'INSERT INTO PROG_QUE (POSITION_ID) VALUES (:POSITION_ID)';
C_ToProgUpdateText = 'UPDATE PROG_QUE SET PROG_QUE.RESULT = 0 WHERE POSITION_ID = :POSITION_ID';
C_SelInProgText = 'SELECT PROG_QUE.Q_ID, PROG_QUE.POSITION_ID, '+
    'PROG_QUE.RESULT FROM PROG_QUE WHERE POSITION_ID = :POSITION_ID';
begin
  Result := False;
  if AObj.AddProgQR.Active then
    AObj.AddProgQR.Close;
  AObj.AddProgQR.Unprepare;
  AObj.AddProgQR.Params.Clear;
  if not AObj.MeasObj.InProgQueueState.pqState then
    AObj.AddProgQR.SQL.Text := C_ToProgInsertText
  else
    AObj.AddProgQR.SQL.Text := C_ToProgUpdateText;
  try
    AObj.AddProgQR.Prepare;
    AObj.AddProgQR.ParamByName('POSITION_ID').AsInteger := AObj.MeasObj.BaseID;
    AObj.AddProgQR.ExecSQL;
    AObj.Reconstructor.fDMAccess.Commit(AObj.AddProgQR);
    Result := True;
  except on e : Exception do
    AObj.Reconstructor.fDMAccess.OnException(AObj.AddProgQR, e);
  end;
  if Result then
  begin
    if AObj.AddProgQR.Active then
      AObj.AddProgQR.Close;
    AObj.AddProgQR.Unprepare;
    AObj.AddProgQR.Params.Clear;
    AObj.AddProgQR.SQL.Text := C_SelInProgText;
    try
      AObj.AddProgQR.Prepare;
      AObj.AddProgQR.ParamByName('POSITION_ID').AsInteger := AObj.MeasObj.BaseID;
      AObj.AddProgQR.Open;
      AObj.MeasObj.InProgQueueState.pqState := (AObj.AddProgQR.RecordCount > 0);
      if AObj.MeasObj.InProgQueueState.pqState then
      begin
        AObj.MeasObj.InProgQueueState.pqID := AObj.AddProgQR.FieldByName('Q_ID').AsInteger;
        AObj.MeasObj.InProgQueueState.pqResult := byte(AObj.AddProgQR.FieldByName('RESULT').AsInteger);
      end;
    finally
      Result := AObj.MeasObj.InProgQueueState.pqState and (AObj.MeasObj.InProgQueueState.pqResult = 0);
      AObj.Reconstructor.fDMAccess.Commit(AObj.AddProgQR);
      if AObj.AddProgQR.Active then
        AObj.AddProgQR.Close;
    end;
  end;
end;
var
vLogItem : TPosLogItem;
vMaxTolerance_dF, vMaxTolerance_dF25, vMaxTolerance_Iter_dF, vMaxTolerance_Iter_dF25 : double;
vCommentCount, i : byte;
vTolerance : pToleranceItem;
vErrorItem : pMedItem;
vMeasDB : TEmbSQLDataBase;
vCompensResults : TCompensationResult;
vHaveTolerance : Boolean;
begin
  if ((not assigned(AObj))
  or (not assigned(AObj.DbObj))
  or (AObj.DbObj.ItersCount < 1)
  or (not assigned(AObj.Iter))
  or (not assigned(AObj.ProcessOptions))) then
     exit;
  vHaveTolerance := Assigned(AObj.ToleranceGroup) and (AObj.ToleranceGroup.Count > 0);
  vMaxTolerance_dF := 1E-8;
  vMaxTolerance_dF25 := 1E-8;
  vMaxTolerance_Iter_dF := 1E-8;
  vMaxTolerance_Iter_dF25  := 1E-8;
  if vHaveTolerance then
  begin
    vMaxTolerance_dF := AObj.ToleranceGroup.Last.dF;
    vMaxTolerance_dF25 := AObj.ToleranceGroup.Last.dF25;
    vMaxTolerance_Iter_dF := AObj.ToleranceGroup.Last.Iter_dF;
    vMaxTolerance_Iter_dF25  := AObj.ToleranceGroup.Last.Iter_dF25;
  end;
  vMeasDB := AObj.DbObj.CreateConnection;
  try
    vCompensResults.ClassNum := 0;
    vCompensResults.ClassName := '';
    AObj.Iter.State := AObj.Iter.State + [isCalcOk];
    AObj.DbObj.UpdateCompensationResult(vMeasDB, vCompensResults);
    if (AObj.RangeOptions = [rpFreqFinal])
    or (AObj.RangeOptions = [rpFreqZero, rpFreqFinal]) then
    begin
      {$REGION ' Если это финальный прогон '}
      if (AObj.Iter.Results.dF < vMaxTolerance_dF)
      and (Abs(AObj.Iter.Results.dF25) < vMaxTolerance_dF25)
      and (AObj.Iter.Results.Iter_dF < vMaxTolerance_Iter_dF)
      and (Abs(AObj.Iter.Results.Iter_dF25) < vMaxTolerance_Iter_dF25) then
      begin
        vLogItem.DT := Now;
        vLogItem.Step := 'Прогон';
        vLogItem.Result := 'OK';
        vLogItem.Comment := '';
        if vHaveTolerance then
        begin
          {$REGION ' Допуски определены '}
          {Ищем допуск}
          i := 0;
          while i < AObj.ToleranceGroup.Count do
          begin
            vTolerance := AObj.ToleranceGroup[i];
            if (AObj.Iter.Results.dF < vTolerance^.dF)
            and (Abs(AObj.Iter.Results.dF25) < vTolerance^.dF25)
            and (AObj.Iter.Results.Iter_dF < vTolerance^.Iter_dF)
            and (Abs(AObj.Iter.Results.Iter_dF25) < vTolerance^.Iter_dF25) then
              break;
            inc(i);
          end;
          vCompensResults.ClassNum := vTolerance^.Index;
          vCompensResults.ClassName := vTolerance^.Name;
          AObj.DbObj.UpdateCompensationResult(vMeasDB, vCompensResults);
          if (AObj.Iter.Errors.Find(medFreqFinal, vErrorItem))
          and (vErrorItem.ErrCount > 1) then
          begin
            vLogItem.Comment := Format(
            'Готовое изделие измерено с ошибками. Ошибок %f%. В допуске по классу %d(%s). '+
            'Результаты измерения: отклонение номинала \ ТЧХ, %f\ %f, '+
            'нестабильность номинала \ ТЧХ, ppm %f\ %f. '+
            'Допуск: отклонение номинала \ ТЧХ, %f\ %f, '+
            'нестабильность номинала  \ ТЧХ, ppm %f\ %f',
            [vErrorItem.Percents, vTolerance^.Index, vTolerance^.Name,
            AObj.Iter.Results.dF25, AObj.Iter.Results.dF,
            AObj.Iter.Results.Iter_dF25, AObj.Iter.Results.Iter_dF,
            vTolerance^.dF25, vTolerance^.dF,
            vTolerance^.Iter_dF25, vTolerance^.Iter_dF]);
          end else
          begin
            vLogItem.Comment := Format(
            'Готовое изделие. В допуске по классу %d(%s). '+
            'Результаты измерения: отклонение номинала \ ТЧХ, %f\ %f, '+
            'нестабильность номинала \ ТЧХ, ppm %f\ %f. '+
            'Допуск: отклонение номинала \ ТЧХ, %f\ %f, '+
            'нестабильность номинала \ ТЧХ, ppm %f\ %f',
            [vTolerance^.Index, vTolerance^.Name,
            AObj.Iter.Results.dF25, AObj.Iter.Results.dF,
            AObj.Iter.Results.Iter_dF25, AObj.Iter.Results.Iter_dF,
            vTolerance^.dF25, vTolerance^.dF,
            vTolerance^.Iter_dF25, vTolerance^.Iter_dF]);
          end;
          {$ENDREGION}
        end else
        begin
          if (AObj.Iter.Errors.Find(medFreqFinal, vErrorItem))
          and (vErrorItem.ErrCount > 1) then
          begin
            vLogItem.Comment := Format('Готовое изделие без класса допуска. Измерено с ошибками %f%',[vErrorItem.Percents]);
          end else
            vLogItem.Comment := 'Готовое изделие без класса допуска';
        end;
        vLogItem.Save(vMeasDB);
      end else
      begin
        vLogItem.DT := Now;
        vLogItem.Step := 'Прогон';
        if vHaveTolerance then
        begin
          vLogItem.Result := 'Ошибка';
          vLogItem.Comment := Format(
              'Результаты измерения: отклонение номинала \ ТЧХ, %f\ %f, '+
              'нестабильность номинала \ ТЧХ, ppm %f\ %f. '+
              'Допуск: отклонение номинала \ ТЧХ, %f\ %f, '+
              'нестабильность номинала \ ТЧХ, ppm %f\ %f.',
              [AObj.Iter.Results.dF25, AObj.Iter.Results.dF,
              AObj.Iter.Results.Iter_dF25, AObj.Iter.Results.Iter_dF,
              vMaxTolerance_dF25, vMaxTolerance_dF,
              vMaxTolerance_Iter_dF25, vMaxTolerance_Iter_dF]);

          if (AObj.Iter.Errors.Find(medFreqFinal, vErrorItem))
            and (vErrorItem.ErrCount > 1) then
            vLogItem.Comment := vLogItem.Comment + Format(' Измерено с ошибками %f%. Не в допуске',[vErrorItem.Percents])
          else
            vLogItem.Comment := vLogItem.Comment + ' Не в допуске';
          vCommentCount := 0;
          if (AObj.Iter.Results.dF >= vMaxTolerance_dF) then
          begin
            vLogItem.Comment := vLogItem.Comment + ' по отклонению ТЧХ в диапазоне температур';
            inc(vCommentCount);
          end;
          if (Abs(AObj.Iter.Results.dF25) >= vMaxTolerance_dF25) then
          begin
            if vCommentCount > 0 then
              vLogItem.Comment := vLogItem.Comment + ',';
            vLogItem.Comment := vLogItem.Comment + ' по отклонению номинала';
            inc(vCommentCount);
          end;
          if (AObj.Iter.Results.Iter_dF >= vMaxTolerance_Iter_dF) then
          begin
            if vCommentCount > 0 then
              vLogItem.Comment := vLogItem.Comment + ',';
            vLogItem.Comment := vLogItem.Comment + ' по стабильности ТЧХ между итерациями';
            inc(vCommentCount);
          end;
          if (Abs(AObj.Iter.Results.Iter_dF25) >= vMaxTolerance_Iter_dF25) then
          begin
            if vCommentCount > 0 then
              vLogItem.Comment := vLogItem.Comment + ',';
            vLogItem.Comment := vLogItem.Comment + ' по стабильности номинала между итерациями';
            inc(vCommentCount);
          end;
        end else
        begin
          vLogItem.Result := 'Ошибка';
          vLogItem.Comment := Format(
              'Результаты измерения: отклонение номинала \ ТЧХ, %f\ %f, '+
              'нестабильность номинала \ ТЧХ, ppm %f\ %f.',
              [AObj.Iter.Results.dF25, AObj.Iter.Results.dF,
              AObj.Iter.Results.Iter_dF25, AObj.Iter.Results.Iter_dF]);

          if (AObj.Iter.Errors.Find(medFreqFinal, vErrorItem))
            and (vErrorItem.ErrCount > 1) then
            vLogItem.Comment := vLogItem.Comment + Format(' Измерено с ошибками %f%. Допусков нет',[vErrorItem.Percents])
          else
            vLogItem.Comment := vLogItem.Comment + ' Допусков нет';
        end;
        vLogItem.Save(vMeasDB);
      end;
      {$ENDREGION}
    end else
    if (AObj.Iter.CalcResults.Count > 0)  then
    begin
      {$REGION ' иначе не финальный прогон и результатов больше нуля '}
      //vDBObj.ProcOptions.ProcResults.WorkResult := prOK;
      //vDBObj.ProcOptions.ProcResults.FatalError := false;
      if (AObj.Iter.Num >= AObj.ProcessOptions.CalcOptions.ItersBeforeProgCount) then
      begin
        {$REGION ' Если предпрошивочная итерация '}
        if (arProg in AObj.Iter.Options.ControlOptions.AfterCompensOptions.s)
        and vHaveTolerance then
        begin
          {$REGION ' Если прошивка разрешена и группы допуска определены '}
          if (AObj.Iter.Results.Calc_dF < vMaxTolerance_dF)
          and (Abs(AObj.Iter.Results.Calc_dF25) < vMaxTolerance_dF25)
          and (AObj.Iter.Results.Iter_dF < vMaxTolerance_Iter_dF)
          and (Abs(AObj.Iter.Results.Iter_dF25) < vMaxTolerance_Iter_dF25) then
          begin
            {$REGION ' Если в допуске '}
            i := 0;
            while i < AObj.ToleranceGroup.Count do
            begin
              vTolerance := AObj.ToleranceGroup[i];
              if (AObj.Iter.Results.Calc_dF < vTolerance^.dF)
              and (Abs(AObj.Iter.Results.Calc_dF25) < vTolerance^.dF25)
              and (AObj.Iter.Results.Iter_dF < vTolerance^.Iter_dF)
              and (Abs(AObj.Iter.Results.Iter_dF25) < vTolerance^.Iter_dF25) then
                break;
              inc(i);
            end;
            vLogItem.DT := Now;
            vLogItem.Step := 'Обработка результатов';
            vLogItem.Comment := Format(
            'В допуске по классу %d(%s). '+
            'Результаты расчета: отклонение номинала \ ТЧХ, %f\ %f, '+
            'нестабильность номинала \ ТЧХ, ppm %f\ %f.  '+
            'Допуск: отклонение номинала \ ТЧХ, %f\ %f, '+
            'нестабильность номинала \ ТЧХ, ppm %f\ %f',
            [vTolerance^.Index, vTolerance^.Name,
            AObj.Iter.Results.Calc_dF25, AObj.Iter.Results.Calc_dF,
            AObj.Iter.Results.Iter_dF25, AObj.Iter.Results.Iter_dF,
            vTolerance^.dF25, vTolerance^.dF,
            vTolerance^.Iter_dF25, vTolerance^.Iter_dF]);
            if AddToProg then
            begin
              while (AObj.Iter.CalcResults.Count > 1) do
               AObj.Iter.CalcResults.Delete(0);
              vLogItem.Result := 'OK';
              vLogItem.Comment := vLogItem.Comment +' Готово к прошивке.';
              AObj.DbObj.InitRegisters.Assign(AObj.Iter.CalcResults.Last.Registers);
              AObj.DbObj.UpdatePosData(vMeasDB, [udInitRegisters]);
            end
            else
            begin
              vLogItem.Result := 'Предупреждение';
              vLogItem.Comment := vLogItem.Comment +' Не удалось добавить в очередь на прошивку.';
            end;
            vLogItem.Save(vMeasDB);
            {$ENDREGION}
          end else
          begin
            {$REGION ' Не в допуске '}
            vLogItem.DT := Now;
            vLogItem.Step := 'Обработка результатов';
            vLogItem.Result := 'Ошибка';
            vLogItem.Comment := Format(
            'Результаты расчета: отклонение номинала \ ТЧХ, %f\ %f, '+
            'нестабильность номинала \ ТЧХ, ppm %f\ %f. '+
            'Допуск: отклонение номинала \ ТЧХ, %f\ %f, '+
            'нестабильность номинала \ ТЧХ, ppm %f\ %f.',
            [AObj.Iter.Results.Calc_dF25, AObj.Iter.Results.Calc_dF,
            AObj.Iter.Results.Iter_dF25,  AObj.Iter.Results.Iter_dF,
            vMaxTolerance_dF25, vMaxTolerance_dF,
            vMaxTolerance_Iter_dF25, vMaxTolerance_Iter_dF]);
            vLogItem.Comment := vLogItem.Comment + ' Не в допуске';
            vCommentCount := 0;
            if (AObj.Iter.Results.Calc_dF >= vMaxTolerance_dF) then
            begin
              vLogItem.Comment := vLogItem.Comment + ' по отклонению ТЧХ в диапазоне температур';
              inc(vCommentCount);
            end;
            if (Abs(AObj.Iter.Results.Calc_dF25) >= vMaxTolerance_dF25) then
            begin
              if vCommentCount > 0 then
                vLogItem.Comment := vLogItem.Comment + ',';
              vLogItem.Comment := vLogItem.Comment + ' по отклонению номинала';
              inc(vCommentCount);
            end;
            if (AObj.Iter.Results.Iter_dF >= vMaxTolerance_Iter_dF) then
            begin
              if vCommentCount > 0 then
                vLogItem.Comment := vLogItem.Comment + ',';
              vLogItem.Comment := vLogItem.Comment + ' по стабильности ТЧХ между итерациями';
              inc(vCommentCount);
            end;
            if (Abs(AObj.Iter.Results.Iter_dF25) >= vMaxTolerance_Iter_dF25) then
            begin
              if vCommentCount > 0 then
                vLogItem.Comment := vLogItem.Comment + ',';
              vLogItem.Comment := vLogItem.Comment + ' по стабильности номинала между итерациями';
              inc(vCommentCount);
            end;
            AObj.Iter.CalcResults.Clear;
            vLogItem.Save(vMeasDB);
            {$ENDREGION}
          end;
          {$ENDREGION}
        end else
        begin
          {$REGION ' Иначе - прошивка запрещена или допусков нет '}
          vLogItem.DT := Now;
          vLogItem.Step := 'Обработка результатов';
          if vHaveTolerance then
          begin
            if (AObj.Iter.Results.Calc_dF < vMaxTolerance_dF)
            and (Abs(AObj.Iter.Results.Calc_dF25) < vMaxTolerance_dF25)
            and (AObj.Iter.Results.Iter_dF < vMaxTolerance_Iter_dF)
            and (Abs(AObj.Iter.Results.Iter_dF25) < vMaxTolerance_Iter_dF25) then
            begin
              i := 0;
              while i < AObj.ToleranceGroup.Count do
              begin
                vTolerance := AObj.ToleranceGroup[i];
                if (AObj.Iter.Results.Calc_dF < vTolerance^.dF)
                and (Abs(AObj.Iter.Results.Calc_dF25) < vTolerance^.dF25)
                and (AObj.Iter.Results.Iter_dF < vTolerance^.Iter_dF)
                and (Abs(AObj.Iter.Results.Iter_dF25) < vTolerance^.Iter_dF25) then
                  break;
                inc(i);
              end;
              vLogItem.Result := 'OK';
              vLogItem.Comment := Format(
              'В допуске по классу %d(%s). '+
              'Результаты расчета: отклонение номинала \ ТЧХ, %f\ %f, '+
              'нестабильность номинала \ ТЧХ, ppm %f\ %f.  '+
              'Допуск: отклонение номинала \ ТЧХ, %f\ %f, '+
              'нестабильность номинала \ ТЧХ, ppm %f\ %f. Прошивка не разрешена оператором',
              [vTolerance^.Index, vTolerance^.Name,
              AObj.Iter.Results.Calc_dF25, AObj.Iter.Results.Calc_dF,
              AObj.Iter.Results.Iter_dF25,AObj.Iter.Results.Iter_dF,
              vTolerance^.dF25,  vTolerance^.dF,
              vTolerance^.Iter_dF25, vTolerance^.Iter_dF]);
            end else
            begin
              vLogItem.Result := 'Ошибка!';
              vLogItem.Comment := Format(
                'Результаты расчета: отклонение номинала \ ТЧХ, %f\ %f, '+
                'нестабильность номинала \ ТЧХ, ppm %f\ %f. '+
                'Максимальный допуск на: отклонение номинала \ ТЧХ, %f\ %f, '+
                'нестабильности номинала \ ТЧХ, ppm %f\ %f. Прошивка не разрешена оператором. Не в допуске: ',
                [AObj.Iter.Results.Calc_dF25, AObj.Iter.Results.Calc_dF,
                 AObj.Iter.Results.Iter_dF25, AObj.Iter.Results.Iter_dF,
                vMaxTolerance_dF25, vMaxTolerance_dF,
                vMaxTolerance_Iter_dF25, vMaxTolerance_Iter_dF]);
              vCommentCount := 0;
              if (AObj.Iter.Results.Calc_dF >= vMaxTolerance_dF) then
              begin
                vLogItem.Comment := vLogItem.Comment + ' по отклонению ТЧХ в диапазоне температур';
                inc(vCommentCount);
              end;
              if (Abs(AObj.Iter.Results.Calc_dF25) >= vMaxTolerance_dF25) then
              begin
                if vCommentCount > 0 then
                  vLogItem.Comment := vLogItem.Comment + ',';
                vLogItem.Comment := vLogItem.Comment + ' по отклонению номинала';
                inc(vCommentCount);
              end;
              if (AObj.Iter.Results.Iter_dF >= vMaxTolerance_Iter_dF) then
              begin
                if vCommentCount > 0 then
                  vLogItem.Comment := vLogItem.Comment + ',';
                vLogItem.Comment := vLogItem.Comment + ' по стабильности ТЧХ между итерациями';
                inc(vCommentCount);
              end;
              if (Abs(AObj.Iter.Results.Iter_dF25) >= vMaxTolerance_Iter_dF25) then
              begin
                if vCommentCount > 0 then
                  vLogItem.Comment := vLogItem.Comment + ',';
                vLogItem.Comment := vLogItem.Comment + ' по стабильности номинала между итерациями';
                inc(vCommentCount);
              end;
              AObj.Iter.CalcResults.Clear;
            end;
          end else
          begin
            vLogItem.Result := 'OK';
            vLogItem.Comment := 'Допуски не определены';
          end;
          vLogItem.Save(vMeasDB);
          {$ENDREGION}
        end;
        {$ENDREGION}
      end else
      begin
        vLogItem.DT := Now;
        vLogItem.Step := 'Обработка результатов';
        vLogItem.Result := 'OK';
        vLogItem.Comment := 'Готовность к следующей итерации';
        vLogItem.Save(vMeasDB);
      end;
      {$ENDREGION}
    end else
    begin
      AObj.Iter.State := AObj.Iter.State - [isCalcOk];
      AObj.Iter.State := AObj.Iter.State + [isCalcFail];
      vLogItem.DT := Now;
      vLogItem.Step := 'Обработка результатов';
      vLogItem.Result := 'Ошибка';
      vLogItem.Comment := 'Не найдено решений для продолжения компенсации';
      vLogItem.Save(vMeasDB);
    end;

    AObj.Iter.NextIterOptions.Assign(AObj.Iter.Options);
    AObj.Iter.Results.FinalizeTime := Now;
    if (isCalcFail in AObj.Iter.State) then
       AObj.Iter.Write(vMeasDB, True, [foMeasCalc, foResults, foOptions])
    else if (AObj.Iter.MeasSolutions.Count > 0) then
      AObj.Iter.Write(vMeasDB, True, [foMeasCalc, foIterCalc, foResults, foOptions, foNextOptions])
    else
      AObj.Iter.Write(vMeasDB, True, [foResults, foOptions, foNextOptions]);
    AObj.MeasObj.ReadPosData(vMeasDB);
  finally
    vMeasDB.DBClose;
    FreeAndNil(vMeasDB);
  end;
end;

function CalcError(const AVector :pGSL_vector; AParams : pvoid) : double ; cdecl;
var b : byte;
vParams : pRecostructionObjects;
TmpBit, vFreq25 : double;
vVecIdx :Size_t;
vErrItem : TCalcErrorObj;
vCurrCalc : TCalcCurves;
{$IFDEF TEST_TRACE}
sLog : string;
vBitIdx : Byte;
{$ENDIF}
begin
  vParams := pRecostructionObjects(AParams);
  result  := 0;
  vVecIdx := 0;
  for b := Low(vParams^.Reconstructor.fMinimizationRegisters) to High(vParams^.Reconstructor.fMinimizationRegisters) do
  begin
    TmpBit := gsl_vector_get(AVector, vVecIdx);
    if (TmpBit > C_ValsMax[vParams^.Reconstructor.fMinimizationRegisters[b]])
    or  (TmpBit < C_ValsMin[vParams^.Reconstructor.fMinimizationRegisters[b]]) then
      result := 100000;
    vParams^.DestRegisters.BitValue[vParams^.Reconstructor.fMinimizationRegisters[b]] := FixedTrunc(TmpBit);
    inc(vVecIdx);
  end;

  if (vParams^.CurrMeasure.CalcSolutions.Count < 1) then
     vParams^.CurrMeasure.NewCalc;
  vCurrCalc := vParams^.CurrMeasure.CalcSolutions.Last;
  vCurrCalc.CalcCurves(vParams^.DestRegisters);
  case vParams^.Iter.Options.CalcOptions.CalcMethod of
    cmGeneral:
     result :=  result + vCurrCalc.Results.dF + vCurrCalc.dReg;
    cm25:
    begin
      if vCurrCalc.Freq.QueryItem(0, TCalcErrorObj, vErrItem) then
      begin
        vFreq25 := vErrItem.FreqX(25);
        result :=  result + vCurrCalc.Results.dF + vCurrCalc.dReg +
        vParams^.Iter.Options.CalcOptions.cm25Coeff * Absppm(vFreq25, vParams^.Iter.DBExt.Nominal) ;
      end else
       result :=  result + vCurrCalc.Results.dF + vCurrCalc.dReg;
      vErrItem := nil;
    end;
  end;
  {$IFDEF TEST_TRACE}
  if Assigned(vParams^.Reconstructor.CalcLogger) and vParams^.Reconstructor.CalcLogger.LevelEnabled(lTrace) then
  begin
    sLog := '';
    for vBitIdx := Low(vParams^.Reconstructor.fMinimizationRegisters) to High(vParams^.Reconstructor.fMinimizationRegisters) do
    begin
      sLog := sLog + Format(', %s = %d',
        [C_RegNames[vParams^.Reconstructor.fMinimizationRegisters[vBitIdx]],
        vParams^.DestRegisters.BitValue[vParams^.Reconstructor.fMinimizationRegisters[vBitIdx]] ]);
    end;
    vParams^.Reconstructor.CalcLogger.Log(lTrace,
                Format('Processor Idx:%d. Board:%d, Pos:%d. '+
                'Test trace. Registers values: %s; Error:%8.5f; dReg:%8.5f; dF:%8.5f ppm; dF(25):%8.5f ppm',
    [vParams^.Reconstructor.Params.ProcessorBitIdx, vParams^.DbObj.Board_SN,
    vParams^.DbObj.Board_Pos,
    sLog, result,
    vCurrCalc.dReg,
    vCurrCalc.Results.dF,
    vCurrCalc.Results.dF25]));
  end;
  {$ENDIF}
end;

{$REGION ' Vector container '}

type


TMinimizeSolution = class(TObject)
  RootObj : TRootMeasureObj;
  Vector : pGSL_vector;
  Error, dF25, dF, dReg, Prior : double;
  constructor Create(DimSize :Size_t); reintroduce;
  destructor Destroy;override;
  procedure Calc(AParams : pRecostructionObjects);
  procedure Assign(const ASource : TMinimizeSolution);
end;

constructor TMinimizeSolution.Create(DimSize :Size_t);
begin
  inherited Create;
  Vector := gsl_vector_alloc(DimSize);
end;

destructor TMinimizeSolution.Destroy;
begin
  gsl_vector_free(Vector);
  inherited Destroy;
end;

procedure TMinimizeSolution.Assign(const ASource : TMinimizeSolution);
begin
  gsl_vector_memcpy (Vector, ASource.Vector);
  RootObj := ASource.RootObj;
  Error   := ASource.Error;
  dF      := ASource.dF;
  dF25    := ASource.dF25;
  dReg    := ASource.dReg;
  Prior   := ASource.Prior;
end;

procedure TMinimizeSolution.Calc(AParams : pRecostructionObjects);
begin
  RootObj := AParams^.CurrMeasure;
  Error := CalcError(Vector, pvoid(AParams));
  dF25 := RootObj.CalcSolutions.Last.Results.dF25;
  dF := RootObj.CalcSolutions.Last.Results.dF;
  dReg := RootObj.CalcSolutions.Last.dReg;
  Prior := RootObj.CalcSolutions.Last.Prior;
end;

{$ENDREGION}


{ TMasD8Reconstructor }

procedure TMasD8Reconstructor.AfterCreate;
begin
  fIniParams.LoadIni;
end;

function TMasD8Reconstructor.GetTolerance(ACardID : Integer): TToleranceGroupEx;
var
vPoIdx : Word;
vPoFindSucc : Boolean;
begin
  vPoIdx := 0;
  vPoFindSucc := false;
  while (vPoIdx < fTolerances.Count) do
  begin
    vPoFindSucc := fTolerances[vPoIdx].CardID = ACardID;
    if vPoFindSucc then
      break ;
    Inc(vPoIdx);
  end;
  if vPoFindSucc then
    Result := fTolerances[vPoIdx]
  else
  try
    Result := TToleranceGroupEx.Create;
    fTolerances.Add(Result);
    Result.CardID := ACardID;
    if Assigned(fDMAccess) then
        Result.LoadByCardID(fDMAccess, fConnection, ACardID);
  except
    on E : Exception do
    begin
      if Assigned(CalcLogger) then
        CalcLogger.Log(lWarning, Format('Thread ID:%d Error %s on load tolerance by card id=%d',[ThreadID, e.Message, ACardID]));
    end;
  end;
end;

function TMasD8Reconstructor.GetProcessOptions(ACardID : Integer; var oOptions: TStp8masProcessDBOptions): boolean;
var
vPoIdx : Word;
vOptions : TStp8masProcessDBOptions;
begin
  vPoIdx := 0;
  Result := false;
  while (vPoIdx < fProcessOptions.Count) do
  begin
    Result := fProcessOptions[vPoIdx].CardID = ACardID;
    if Result then
      break ;
    Inc(vPoIdx);
  end;
  if Result then
    oOptions := fProcessOptions[vPoIdx]
  else
  try
    vOptions := TStp8masProcessDBOptions.Create;
    result := fConnection.Connected and Assigned(fDMAccess);
    if result then
    begin
      try
        Result := vOptions.Stp8_Load(fDMAccess, fConnection, ACardID);
      except
        FreeAndNil(vOptions);
        raise;
      end;
      if Result then
      begin
        fProcessOptions.Add(vOptions);
        oOptions := vOptions;
      end  else
      begin
        FreeAndNil(vOptions);
        if Assigned(CalcLogger) then
          CalcLogger.Log(lWarning, Format('Thread ID:%d Error on load process options id %d',[ThreadID, ACardID]));
      end;
    end;
  except
    on E : Exception do
    begin
      Result := false;
      if Assigned(CalcLogger) then
        CalcLogger.Log(lException, E.Message);
    end;
  end;
end;

function TMasD8Reconstructor.GetRegistersWeightsGroup(AID : Integer):TRegistersWeightsGroup;
var
vPoIdx : Word;
vPoFindSucc : Boolean;
begin
  vPoIdx := 0;
  vPoFindSucc := false;
  while (vPoIdx < fRegistersWeights.Count) do
  begin
    vPoFindSucc := fRegistersWeights[vPoIdx].ID = AID;
    if vPoFindSucc then
      break ;
    Inc(vPoIdx);
  end;
  if vPoFindSucc then
    Result := fRegistersWeights[vPoIdx]
  else
  try
    result := TRegistersWeightsGroup.Create(AID);
    fRegistersWeights.Add(result);
    if fConnection.Connected then
      RegistersWeightsGroupLoad(result, fConnection, fLoader);
  except
    on E : Exception do
    begin
      if Assigned(CalcLogger) then
        CalcLogger.Log(lWarning, Format('Thread ID:%d Error %s on load registers weights id=%d',[ThreadID, e.Message, AID]));
    end;
  end;
end;


function Minimize(AMinimizeVector, ASSVect : pGSL_vector;
                          ADimSize :Size_t;
                          AMinex_func : pgsl_multimin_function_struct;
                          MinimizeThread : TMasD8Reconstructor) : boolean;
var
MinimizerType : pGSL_multimin_fminimizer_type;
vFminimizer : pGSL_multimin_fminimizer;
vMinimizerIter : size_t;
vFminimizer_size : double;
vSizeStatus : integer;
sLog : string;
vBitIdx,
vVecIdx : byte;
vError : Double;
vParams : pRecostructionObjects;
function GslStatusName(AStatus : integer):string;
begin
  case AStatus of
    -2: Result := 'GSL_CONTINUE';
    -1: Result := 'GSL_FAILURE';
    0: Result := 'GSL_SUCCESS'
    else
      Result := 'GSL_EDOM..GSL_EOF';
  end;
end;
begin
  try
    vParams := pRecostructionObjects(AMinex_func.params);
    if assigned(MinimizeThread)
      and Assigned(MinimizeThread.CalcLogger)
      and MinimizeThread.CalcLogger.LevelEnabled(lTrace) then
    begin
      sLog := 'Minimize vector:';
      vVecIdx := 0;
      for vBitIdx := Low(MinimizeThread.fMinimizationRegisters) to High(MinimizeThread.fMinimizationRegisters) do
      begin
        if vBitIdx = Low(MinimizeThread.fMinimizationRegisters)  then
          sLog := sLog + Format('%s = %8.5f',
          [C_RegNames[MinimizeThread.fMinimizationRegisters[vBitIdx]],
          gsl_vector_get(AMinimizeVector, vVecIdx) ])
        else
          sLog := sLog + Format(', %s = %8.5f',
          [C_RegNames[MinimizeThread.fMinimizationRegisters[vBitIdx]],
          gsl_vector_get(AMinimizeVector, vVecIdx) ]);
        inc(vVecIdx);
      end;
      sLog := sLog +' Step size vector:';
      vVecIdx := 0;
      for vBitIdx := Low(MinimizeThread.fMinimizationRegisters) to High(MinimizeThread.fMinimizationRegisters) do
      begin
        if vBitIdx = Low(MinimizeThread.fMinimizationRegisters)  then
          sLog := sLog + Format('%s = %8.5f',
          [C_RegNames[MinimizeThread.fMinimizationRegisters[vBitIdx]],
          gsl_vector_get(ASSVect, vVecIdx) ])
        else
          sLog := sLog + Format(', %s = %8.5f',
          [C_RegNames[MinimizeThread.fMinimizationRegisters[vBitIdx]],
          gsl_vector_get(ASSVect, vVecIdx) ]);
        inc(vVecIdx);
      end;
      vError := CalcError(AMinimizeVector, AMinex_func.params);
      MinimizeThread.CalcLogger.Log(lTrace,
                  Format('Processor Idx:%d. '+
                  'start minimize %s. Error:%8.5f; dReg:%8.5f; dF:%8.5f ppm; dF(25):%8.5f ppm',
      [MinimizeThread.fProcessorBitIdx, sLog,
      vError,
      vParams.CurrMeasure.CalcSolutions.Last.dReg,
      vParams.CurrMeasure.CalcSolutions.Last.Results.dF,
      vParams.CurrMeasure.CalcSolutions.Last.Results.dF25]));
    end;
    MinimizerType := gsl_multimin_fminimizer_nmsimplex2;
    vFminimizer := gsl_multimin_fminimizer_alloc(MinimizerType, ADimSize);
    try
      { Initialize method and iterate }
      gsl_multimin_fminimizer_set(vFminimizer, @AMinex_func.f, AMinimizeVector, ASSVect);
      vMinimizerIter := 0;
      repeat
        if ((not assigned(MinimizeThread)) or (MinimizeThread.Terminated)) then
          Break;
        inc(vMinimizerIter);
        vSizeStatus := gsl_multimin_fminimizer_iterate(vFminimizer);
        if (vSizeStatus = GSL_EDOM) then
        begin
          {$IFDEF TEST_TRACE}
          if assigned(MinimizeThread)
            and Assigned(MinimizeThread.CalcLogger)
            and MinimizeThread.CalcLogger.LevelEnabled(lTrace) then
          begin
            MinimizeThread.CalcLogger.Log(lTrace,
                        Format('Processor Idx:%d. '+
                        'Test trace: Size status = GSL_EDOM. Break',
            [MinimizeThread.ProcessorBitIdx]));
            end;
          {$ENDIF}
          break;
        end;
        vFminimizer_size := gsl_multimin_fminimizer_size(vFminimizer);
        vSizeStatus := gsl_multimin_test_size(vFminimizer_size, MinimizeThread.fIniParams.StepSize);
        {$IFDEF TEST_TRACE}
        if assigned(MinimizeThread)
          and Assigned(MinimizeThread.CalcLogger)
          and MinimizeThread.CalcLogger.LevelEnabled(lTrace) then
        begin
          MinimizeThread.CalcLogger.Log(lTrace,
                      Format('Processor Idx:%d. '+
                      'test trace iterate num:%d; minimizer size:%8.5f; Size status:%s ',
          [MinimizeThread.ProcessorBitIdx, vMinimizerIter,
          vFminimizer_size, GslStatusName(vSizeStatus)]));
        end;
        {$ENDIF}
      until (vSizeStatus <> GSL_CONTINUE) or (vMinimizerIter >= MinimizeThread.fIniParams.MinimizeIters);
      if assigned(MinimizeThread)
      and Assigned(MinimizeThread.CalcLogger) then
        MinimizeThread.CalcLogger.Log(lDebug,
                      Format('Thread ID:%d Processor Idx:%d Minimizer finished. Iters count=%d; max iters=%d; test size=%f',
          [MinimizeThread.ThreadID, MinimizeThread.fProcessorBitIdx, vMinimizerIter,
          MinimizeThread.fIniParams.MinimizeIters, vFminimizer_size]));

      result := assigned(MinimizeThread) and (not MinimizeThread.Terminated) and (((vSizeStatus = GSL_SUCCESS) or (vSizeStatus = GSL_CONTINUE)));
      if result then
        gsl_vector_memcpy(AMinimizeVector, vFminimizer.x);
    finally
      gsl_multimin_fminimizer_free(vFminimizer);
      if assigned(MinimizeThread)
        and Assigned(MinimizeThread.CalcLogger)
        and MinimizeThread.CalcLogger.LevelEnabled(lTrace) then
      begin
        sLog := 'Minimize vector:';
        vVecIdx := 0;
        for vBitIdx := Low(MinimizeThread.fMinimizationRegisters) to High(MinimizeThread.fMinimizationRegisters) do
        begin
          if vBitIdx = Low(MinimizeThread.fMinimizationRegisters)  then
            sLog := sLog + Format('%s = %8.5f',
            [C_RegNames[MinimizeThread.fMinimizationRegisters[vBitIdx]],
            gsl_vector_get(AMinimizeVector, vVecIdx) ])
          else
            sLog := sLog + Format(', %s = %8.5f',
            [C_RegNames[MinimizeThread.fMinimizationRegisters[vBitIdx]],
            gsl_vector_get(AMinimizeVector, vVecIdx) ]);
          inc(vVecIdx);
        end;
        vError := CalcError(AMinimizeVector, AMinex_func.params);
        MinimizeThread.CalcLogger.Log(lTrace,
                      Format('Processor Idx:%d. '+
                      'Minimize finish. %s; minimizer size:%8.5f; Size status:%s Error:%8.5f; dReg:%8.5f; dF:%8.5f ppm; dF(25):%8.5f ppm',
          [MinimizeThread.ProcessorBitIdx, sLog, vFminimizer_size, GslStatusName(vSizeStatus),
          vError,
          vParams.CurrMeasure.CalcSolutions.Last.dReg,
          vParams.CurrMeasure.CalcSolutions.Last.Results.dF,
          vParams.CurrMeasure.CalcSolutions.Last.Results.dF25]));
      end;
    end;
  except
    result := False;
    if assigned(MinimizeThread)
      and Assigned(MinimizeThread.CalcLogger) then
      MinimizeThread.CalcLogger.Log(lWarning,
                  Format('Thread ID:%d Processor Idx:%d Exception in gsl minimizer',
        [MinimizeThread.ThreadID, MinimizeThread.fProcessorBitIdx]));
  end;
end;


type
TRecostructionSolutionsInMeas = TObjectList<TMinimizeSolution>;
TRecostructionSolutionsByMeas = TObjectList<TRecostructionSolutionsInMeas>;


procedure TMasD8Reconstructor.Execute;
var
vParams : pRecostructionObjects;
vCanWhile : Boolean;
vMajorBest, vMinorBest, vOutBest : TMinimizeSolution;
vMajorIter, //j,
vMinorIter, vVecIdx, vDimSize :Size_t;
vBitIdx, vSolIndex, vCalcIdx
//vMVBC/// Coeff Vicinity of Bits in Major
: byte;
vItem : TReconsructionItem;
vSSVect, vMinimizeVect : pGSL_vector;
minex_func : gsl_multimin_function_struct;
vBV: integer;
sLog : string;
vTmp, vdF_FinalCurr, vCurrError : double;
vErrorItem : pMedItem;
vTotalMeas, vWorksCount,
vErrorCount, vProgress, vProgerssValue : word;
vRegistersWeightsGroup : TRegistersWeightsGroup;
vSolutionsInMeas : TRecostructionSolutionsInMeas;
vSolutionsList : TRecostructionSolutionsByMeas;
vByPriorCalcSolutionsInReconsComparer : IComparer<TMinimizeSolution>;
vByPriorIterSolutionsComparer : IComparer<TCalcCurves>;
begin
  {$IFDEF DEBUG}
    TThread.NameThreadForDebugging('MAS 6279D8 reconstructor thread');
  {$ENDIF}
  Priority := tpHigher;
  SetThreadAffinityMask(Handle, 1 shl fProcessorBitIdx);
  Sleep(20+((fProcessorBitIdx+1)*9));
  if (not Supports(fMainProc, IDMAccess, fDMAccess))
  or (not Supports(fMainProc, IPatternLoader, fLoader)) then
  begin
    if Assigned(CalcLogger) then
      CalcLogger.Log(lWarning, Format('Processor Idx:%d can not started. '+
                        'Main proc interfaces error',
                        [fProcessorBitIdx]));
    fDMAccess := nil;
    fLoader := nil;
    Exit;
  end;
  TMas6279D8Registers.ReadControlledBitsIndex('Stp8ChractRegisters', False, fMinimizationRegisters);
  if (Length(fMinimizationRegisters) < 1) then
  begin
    if Assigned(CalcLogger) then
      CalcLogger.Log(lWarning, Format('Processor Idx:%d can not started. '+
              'Section "Stp8ChractRegisters" of ini-file not exists or empty',
              [fProcessorBitIdx]));
    fDMAccess := nil;
    fLoader := nil;
    Exit;
  end;
  if Assigned(CalcLogger) then
    CalcLogger.Log(lInfo, Format('Thread ID:%d Processor Idx:%d - started',[ThreadID, fProcessorBitIdx]));
  New(vParams);
  vParams.Reconstructor := Self;
  vDimSize := Length(fMinimizationRegisters);
  vMajorBest := TMinimizeSolution.Create(vDimSize);
  vMinorBest := TMinimizeSolution.Create(vDimSize);
  vOutBest   := TMinimizeSolution.Create(vDimSize);
  vSSVect := gsl_vector_alloc(vDimSize);
  vMinimizeVect := gsl_vector_alloc (vDimSize);
  vParams.DbObj := TMasResultsDBObj.Create(nil);
  fTolerances := TObjectList<TToleranceGroupEx>.Create;
  fProcessOptions := TObjectList<TStp8masProcessDBOptions>.Create;
  fRegistersWeights := TObjectList<TRegistersWeightsGroup>.Create;
  vParams.DestRegisters := TMas6279D8Registers.Create(nil);
  vSolutionsList := TRecostructionSolutionsByMeas.Create;
  vByPriorCalcSolutionsInReconsComparer := TComparer<TMinimizeSolution>.Construct(
          function(const Left, Right: TMinimizeSolution): Integer
          begin
            {сортировка по оценке от большего к меньшему}
            result := TComparer<double>.Default.Compare(Right.Prior, Left.Prior);
            {if (Right.Prior > Left.Prior) then
              result := 1
            else
              result := -1;}
          end
          );
  vByPriorIterSolutionsComparer := TComparer<TCalcCurves>.Construct(
          function(const Left, Right: TCalcCurves): Integer
          begin
            {сортировка по оценке от большего к меньшему}
            result := TComparer<double>.Default.Compare(Right.Prior, Left.Prior);
            {if (Right.Prior > Left.Prior) then
              result := 1
            else
              result := -1;  }
          end
          );
  vCanWhile := true;
  fConnection := fDMAccess.CreateConnection(nil);
  try
    fConnection.Connected := True;
    if not fConnection.Connected then
    begin
      if Assigned(CalcLogger) then
              CalcLogger.Log(lWarning,
                              Format('Processor idx:%d. Error during main database connecting. Stoped!',
                              [fProcessorBitIdx]));
      Exit;
    end;
    vParams.AddProgQR := fDMAccess.CreateWriteQuery(nil, fConnection);
    try
      minex_func.f := @CalcError;
      minex_func.params := pvoid(vParams);
      //vMVBC := 13;
      Randomize;
      try
        while vCanWhile
        and (not Terminated) do
        begin
          vSolutionsList.Clear;
          vCanWhile  := Self.GetNext(vItem);
          if not vCanWhile then
             Continue;
          if (not Assigned(vItem.MeasuredObj))
          or (not (vItem.MeasuredObj is TPositionDBExtention)) then
          begin
            vItem.State := isFinish;
            Continue;
          end;

          if (not (vItem.MeasuredObj is TStp8masDbExtention)) then
          begin
            vItem.State := isSuspended;
            Continue;
          end;

          vParams.MeasObj := vItem.MeasuredObj as TStp8masDbExtention;
          try
            vParams.DbObj.Load(vParams.MeasObj.MeasFileName);
          except
            inc(vItem.ExceptCount);
            if (vItem.ExceptCount <= 3) then
              vItem.State := isWait
            else
              vItem.State := isFinish;
            if assigned(vItem.MeasuredObj)
            and Assigned(CalcLogger) then
            begin
              if vItem.State = isWait then
                CalcLogger.Log(lWarning,
                              Format('Processor Idx:%d. Board:%d, Pos:%d. - Exceptions on load. Exceptions count: %d. State wait on exception! ',
                  [fProcessorBitIdx, vItem.MeasuredObj.Board_SN, vItem.MeasuredObj.Board_Pos, vItem.ExceptCount]))
              else
                CalcLogger.Log(lWarning,
                              Format('Processor Idx:%d. Board:%d, Pos:%d. - Exceptions on load. Exceptions count: %d. State finish on exception! ',
                  [fProcessorBitIdx, vItem.MeasuredObj.Board_SN, vItem.MeasuredObj.Board_Pos, vItem.ExceptCount]));
            end;
            Continue;
          end;

          if not IsEqualGUID(vParams.DbObj.ChipGUID, MAS6279D8.Consts.C_GUID) then
          begin
            vItem.State := isSuspended;
            if Assigned(CalcLogger) then
              CalcLogger.Log(lWarning,
                              Format('Processor idx:%d. Board:%d, Pos:%d. not is MAS6279D8 - Suspended',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
            Continue;
          end;

          if (vParams.DbObj.ItersCount < 1) then
          begin
            vItem.State := isFinish;
            if Assigned(CalcLogger) then
              CalcLogger.Log(lWarning,
                              Format('Processor idx:%d. Board:%d, Pos:%d. iters count < 1 - Finish',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
            Continue;
          end;

          if not GetProcessOptions(vParams.MeasObj.RCardID, vParams.ProcessOptions) then
          begin
            vItem.State := isFinish;
            if Assigned(CalcLogger) then
              CalcLogger.Log(lWarning,
                              Format('Processor idx:%d. Board:%d, Pos:%d. not found process options for card ID=%d - Finish',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos, vParams.MeasObj.RCardID]));
            Continue;
          end;

          vParams.Iter := vParams.DbObj.Iterations.Last;
          if (not (arCalc in vParams.Iter.Options.ControlOptions.AfterCompensOptions.s)) then
          begin
            vItem.State := isFinish;
            if Assigned(CalcLogger) then
              CalcLogger.Log(lInfo,
                              Format('Processor idx:%d. Board:%d, Pos:%d. reconstruction not enabled - Finish',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
            Continue;
          end;

          vItem.State := isWork;
          vItem.TimeStart := Now;
          vItem.Progress := 0;
          if Assigned(CalcLogger) then
              CalcLogger.Log(lInfo,
                              Format('Processor idx:%d. Board:%d, Pos:%d. - Starting!',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
          vCanWhile := UpdateProgress;
          if not vCanWhile then
          begin
            Terminate;
            Break;
          end;

          vParams.ToleranceGroup := GetTolerance(vParams.MeasObj.RCardID);
          vRegistersWeightsGroup := GetRegistersWeightsGroup(vParams.ProcessOptions.CalcOptions.RegWeighPatternID);
          if Assigned(vRegistersWeightsGroup)
          and (vRegistersWeightsGroup.Iters.Count > 0) then
          begin
            if vParams.Iter.Num <= vRegistersWeightsGroup.Iters.Count then
              vParams.Iter.Options.CalcOptions.RegistersWeighs.Assign(vRegistersWeightsGroup.Iters[vParams.Iter.Num-1])
            else
             vParams.Iter.Options.CalcOptions.RegistersWeighs.Assign(vRegistersWeightsGroup.Iters.Last);
          end;
          if (vParams.Iter.Options.CalcOptions.RegistersWeighs.Count > 0) then
          begin
            sLog := '';
            for vBitIdx := 0 to vParams.Iter.Options.CalcOptions.RegistersWeighs.Count -1 do
            begin
              if vBitIdx = 0 then
                sLog := sLog + Format('%s weight:%8.5f',
                [C_RegNames[vParams.Iter.Options.CalcOptions.RegistersWeighs[vBitIdx].BitIndex],
                 vParams.Iter.Options.CalcOptions.RegistersWeighs[vBitIdx].Value])
              else
                sLog := sLog + Format('; %s weight:%8.5f',
                [C_RegNames[vParams.Iter.Options.CalcOptions.RegistersWeighs[vBitIdx].BitIndex],
                 vParams.Iter.Options.CalcOptions.RegistersWeighs[vBitIdx].Value]);
            end;
            CalcLogger.Log(lEvent,
                        Format('Processor Idx:%d. Board:%d, Pos:%d.  '+
                        'Registers weights:%s',
            [fProcessorBitIdx, vParams^.DbObj.Board_SN,
            vParams^.DbObj.Board_Pos, sLog]));
          end else
          begin
            CalcLogger.Log(lWarning,
                        Format('Processor Idx:%d. Board:%d, Pos:%d. Registers weights is empty',
            [fProcessorBitIdx, vParams^.DbObj.Board_SN, vParams^.DbObj.Board_Pos]));
          end;
          vParams.Iter.Options.CalcOptions.CalcMethod := vParams.ProcessOptions.CalcOptions.CalcMethod;
          vParams.Iter.Options.CalcOptions.cm25Coeff := vParams.ProcessOptions.CalcOptions.cm25Coeff;

          vParams.Iter.Errors.Clear;
          vParams.Iter.CalcResults.Clear;
          vParams.RangeOptions := vParams.Iter.Options.ControlOptions.InRangeOptions[vParams.Iter.Options.ControlOptions.MeasureMode].s;

          if (rpFreqZero in vParams.RangeOptions) then
          begin
            Sleep(10);

            vTotalMeas := vParams.Iter.TC_OFF.TotalCount;
            vWorksCount := vParams.Iter.MeasPoints.Count;
            vErrorCount := vParams.Iter.TC_OFF.ErrorsCount;
            if (vTotalMeas < vWorksCount)  then
            begin
              Inc(vErrorCount, vWorksCount - vTotalMeas);
              vTotalMeas := vWorksCount;
            end;
            if (vTotalMeas > 0) and (vErrorCount > 0) then
            begin
              if not vParams.Iter.Errors.Find(medFreqQuarz, vErrorItem) then
                vErrorItem := vParams.Iter.Errors.NewItem(medFreqQuarz);
              vErrorItem^.TotalCount := vTotalMeas;
              vErrorItem^.ErrCount := vErrorCount;
            end;
            if (vParams.RangeOptions = [rpFreqZero]) then
              vParams.Iter.RecalcMeasureResults;
          end;

          {$REGION ' Final Calc '}
          if (rpFreqFinal in vParams.RangeOptions) then
          begin
            Sleep(10);

            vTotalMeas := vParams.Iter.FreqFinal.TotalCount;
            vWorksCount := vParams.Iter.MeasPoints.WorksCount;
            vErrorCount := vParams.Iter.FreqFinal.ErrorsCount;
            if (vTotalMeas < vWorksCount)  then
            begin
              Inc(vErrorCount, vWorksCount - vTotalMeas);
              vTotalMeas := vWorksCount;
            end;
            if (vTotalMeas > 0) and (vErrorCount > 0) then
            begin
              if not vParams.Iter.Errors.Find(medFreqFinal, vErrorItem) then
                vErrorItem := vParams.Iter.Errors.NewItem(medFreqFinal);
              vErrorItem^.TotalCount := vTotalMeas;
              vErrorItem^.ErrCount := vErrorCount;
            end;
            vParams.Iter.RecalcMeasureResults;

            if ([rpFreqFinal] = vParams.RangeOptions) then
            begin
              Sleep(50+((fProcessorBitIdx+1)*9));

              if Assigned(CalcLogger) then
              CalcLogger.Log(lDebug,
                              Format('Processor idx:%d. Board:%d, Pos:%d. - Analyze final test curve finished. Apply tolreances and solve position state',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
              ProcessingStates(vParams);
              if Assigned(CalcLogger) then
                  CalcLogger.Log(lInfo,
                                  Format('Processor idx:%d. Board:%d, Pos:%d. - Final test reconsruction complete.',
                                  [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));

              vItem.State := isFinish;
              vItem.Progress := 100;
              vCanWhile := UpdateProgress;
              if not vCanWhile then
              begin
                Terminate;
                Break;
              end;

              Sleep(50+((fProcessorBitIdx+1)*9));
              {и запрос следующей позиции}
              Continue;
            end;
          end;
          {$endregion}

          if (not ([rpVvarCurrMeas, rpVvarCharactMeas, rpVvarFC, rpFreqCurr] <= vParams.RangeOptions)) then
          begin
            vItem.State := isFinish;
            if Assigned(CalcLogger) then
                CalcLogger.Log(lWarning,
                            Format('Processor Idx:%d. Board:%d, Pos:%d. No all data for reconsruction and minimization - rejected',
                  [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
            Continue;
          end;
          vProgress := 0;
          vSolIndex := 0;
          while Assigned(vItem)
            and (not Terminated)
            and vCanWhile
            and (vSolIndex < vParams.Iter.MeasSolutions.Count) do
          begin
            vParams.CurrMeasure := vParams.Iter.MeasSolutions[vSolIndex];

            vParams.CurrMeasure.Errors.Clear;

            vTotalMeas := vParams.CurrMeasure.Matrix.TotalCount;
            vErrorCount := vParams.CurrMeasure.Matrix.ErrorCount;
            if (vTotalMeas > 0) and (vErrorCount > 0) then
            begin
              if not vParams.CurrMeasure.Errors.Find(medCharact, vErrorItem) then
                vErrorItem := vParams.CurrMeasure.Errors.NewItem(medCharact);
              vErrorItem^.TotalCount := vTotalMeas;
              vErrorItem^.ErrCount := vErrorCount;
            end;

            vTotalMeas := vParams.CurrMeasure.Freq.TotalCount;
            vErrorCount := vParams.CurrMeasure.Freq.ErrorsCount;
            if (vTotalMeas < vParams^.Iter.MeasPoints.Count)  then
            begin
              Inc(vErrorCount, vParams^.Iter.MeasPoints.Count - vTotalMeas);
              vTotalMeas := vParams^.Iter.MeasPoints.Count;
            end;
            if (vTotalMeas > 0) and (vErrorCount > 0) then
            begin
              if not vParams.CurrMeasure.Errors.Find(medFreqCurr, vErrorItem) then
                vErrorItem := vParams.CurrMeasure.Errors.NewItem(medFreqCurr);
              vErrorItem^.TotalCount := vTotalMeas;
              vErrorItem^.ErrCount := vErrorCount;
            end;

            vTotalMeas := vParams.CurrMeasure.Vvar.TotalCount;
            vErrorCount := vParams.CurrMeasure.Vvar.ErrorsCount;
            if (vTotalMeas < vParams^.Iter.MeasPoints.Count)  then
            begin
              Inc(vErrorCount, vParams^.Iter.MeasPoints.Count - vTotalMeas);
              vTotalMeas := vParams^.Iter.MeasPoints.Count;
            end;
            if (vTotalMeas > 0) and (vErrorCount > 0) then
            begin
              if not vParams.CurrMeasure.Errors.Find(medVvarCurr, vErrorItem) then
                vErrorItem := vParams.CurrMeasure.Errors.NewItem(medVvarCurr);
              vErrorItem^.TotalCount := vTotalMeas;
              vErrorItem^.ErrCount := vErrorCount;
            end;

            vParams.CurrMeasure.CalcSolutions.Clear;

            vParams^.DestRegisters.Assign(vParams.CurrMeasure.Registers);
            vVecIdx := 0;
            for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
            begin
              //скопируем в стартовые значения битов в вектор OutBest
              gsl_vector_set (vOutBest.Vector, vVecIdx, vParams^.DestRegisters.BitValue[fMinimizationRegisters[vBitIdx]]);
              inc(vVecIdx);
            end;
            //обсчитаем отклонения для вектора OutBest
            try
              vOutBest.Calc(vParams);
            except
              if Assigned(CalcLogger) then
                CalcLogger.Log(lWarning,
                            Format('Processor Idx:%d. Board:%d, Pos:%d. exception on outbest calc. Measure:%d',
                  [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos, vSolIndex]));
              inc(vSolIndex);
              Continue;
            end;
            //добавим решение с измеренными значениями на случай ненахождения решения

            vSolutionsInMeas := TRecostructionSolutionsInMeas.Create(vByPriorCalcSolutionsInReconsComparer);
            vSolutionsList.Add(vSolutionsInMeas);
            vSolutionsInMeas.Add(TMinimizeSolution.Create(vDimSize));
            vSolutionsInMeas.Last.Assign(vOutBest);


            // копируем значения битов и расчитанные отклонения
            // в vMajorBest и vMinorBest
            vMajorBest.Assign(vOutBest);
            vMinorBest.Assign(vOutBest);
            // копируем значения битов из OutBest
            // в MinimizeVect - его будет крутить минизатор
            gsl_vector_memcpy(vMinimizeVect, vOutBest.Vector);
            minex_func.n := vDimSize;
            if Assigned(CalcLogger) and CalcLogger.LevelEnabled(lEvent) then
            begin
              sLog := '';
              vVecIdx := 0;
              for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
              begin
                if vBitIdx = Low(fMinimizationRegisters) then
                  sLog := sLog + Format('%s = %d',
                  [C_RegNames[fMinimizationRegisters[vBitIdx]],
                  FixedTrunc(gsl_vector_get (vOutBest.Vector, vVecIdx)) ])
                else
                  sLog := sLog + Format(', %s = %d',
                  [C_RegNames[fMinimizationRegisters[vBitIdx]],
                  FixedTrunc(gsl_vector_get (vOutBest.Vector, vVecIdx)) ]);
                inc(vVecIdx);
              end;
              CalcLogger.Log(lEvent,
                          Format('Processor Idx:%d. Board:%d, Pos:%d. Sol:%d '+
                          'Start minimize whith registers values: %s; Error:%8.5f; dReg:%8.5f; dF(25):%8.5f ppm; dF:%8.5f ppm',
              [fProcessorBitIdx, vParams^.DbObj.Board_SN,
              vParams^.DbObj.Board_Pos, vSolIndex + 1,
              sLog, vOutBest.Error,
              vOutBest.dReg,
              vOutBest.dF25,
              vOutBest.dF]));
            end;
            for vMajorIter := 0 to fIniParams.MajorIters -1 do
            begin
              if (not Assigned(vItem))
                or Terminated then
                    Break;
              vVecIdx := 0;
              for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
              begin
                gsl_vector_set(vSSVect, vVecIdx,
                        RandomRange(
                                    C_ValsMin[fMinimizationRegisters[vBitIdx]],
                                    C_ValsMax[fMinimizationRegisters[vBitIdx]]
                                    )
                              );
                inc(vVecIdx);
              end;
              if vMajorIter > 0 then
              begin
                vVecIdx := 0;
                for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
                begin
                  gsl_vector_set(vMinimizeVect, vVecIdx,
                      RandomRange(
                                  C_ValsMin[fMinimizationRegisters[vBitIdx]],
                                  C_ValsMax[fMinimizationRegisters[vBitIdx]]
                                  )
                                );
                  inc(vVecIdx);
                end;
              end;
              if Assigned(CalcLogger) and CalcLogger.LevelEnabled(lTrace) then
              begin
                sLog := '';
                vVecIdx := 0;
                for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
                begin
                  if vBitIdx = Low(fMinimizationRegisters) then
                    sLog := sLog + Format('%s = %d',
                            [C_RegNames[fMinimizationRegisters[vBitIdx]],
                            FixedTrunc(gsl_vector_get (vMinimizeVect, vVecIdx)) ])
                    else
                    sLog := sLog + Format(', %s = %d',
                            [C_RegNames[fMinimizationRegisters[vBitIdx]],
                            FixedTrunc(gsl_vector_get (vMinimizeVect, vVecIdx)) ]);
                  inc(vVecIdx);
                end;
                CalcLogger.Log(lTrace,
                            Format('Processor Idx:%d; Board:%d, Pos:%d. '+
                            'Major:%d process surface whith registers values: %s',
                [fProcessorBitIdx, vParams^.DbObj.Board_SN, vParams^.DbObj.Board_Pos, vMajorIter, sLog]));
              end;
              // минимизируем в большом круге
              try
                vCurrError := CalcError(vMinimizeVect, pvoid(vParams));
              except
                if Assigned(CalcLogger) then
                  CalcLogger.Log(lWarning,
                              Format('Processor Idx:%d. Board:%d, Pos:%d. Exception on calc error. Measure:%d major iter:%d',
                    [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos, vSolIndex, vMajorIter]));
                Continue;
              end;

              gsl_vector_memcpy (vMajorBest.Vector, vMinimizeVect);

              try
                vMajorBest.Calc(vParams);
              except
                if Assigned(CalcLogger) then
                  CalcLogger.Log(lWarning,
                              Format('Processor Idx:%d. Board:%d, Pos:%d. Exception on major best calc. Measure:%d major iter:%d',
                    [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos, vSolIndex, vMajorIter]));
                Continue;
              end;
              vMinorBest.Assign(vMajorBest);
              for vMinorIter := 0 to fIniParams.MinorIters -1 do
              begin
                if (not Assigned(vItem))
                  or Terminated then
                    Break;
                if vMinorIter > 0 then
                begin
                  vVecIdx := 0;
                  for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
                  begin
                    vBV := (C_ValsMax[fMinimizationRegisters[vBitIdx]] -
                            C_ValsMin[fMinimizationRegisters[vBitIdx]]) div fIniParams.MVBC;
                    gsl_vector_set(vMinimizeVect, vVecIdx,
                        RandomRange(
                          EnsureRange(FixedTrunc(gsl_vector_get(vMinorBest.Vector, vVecIdx)) - vBV,
                                      C_ValsMin[fMinimizationRegisters[vBitIdx]],
                                      C_ValsMax[fMinimizationRegisters[vBitIdx]]),
                          EnsureRange(FixedTrunc(gsl_vector_get(vMinorBest.Vector, vVecIdx)) + vBV,
                                      C_ValsMin[fMinimizationRegisters[vBitIdx]],
                                      C_ValsMax[fMinimizationRegisters[vBitIdx]])
                                    )
                                  );
                    gsl_vector_set(vSSVect, vVecIdx,
                        {RandomRange(
                          EnsureRange(FixedTrunc(gsl_vector_get(vMinorBest.Vector, vVecIdx)) - vBV,
                                      C_ValsMin[fMinimizationRegisters[vBitIdx]],
                                      C_ValsMax[fMinimizationRegisters[vBitIdx]]),
                          EnsureRange(FixedTrunc(gsl_vector_get(vMinorBest.Vector, vVecIdx)) + vBV,
                                      C_ValsMin[fMinimizationRegisters[vBitIdx]],
                                      C_ValsMax[fMinimizationRegisters[vBitIdx]])
                                    ) }
                                    2 * vBV
                                  );
                    Inc(vVecIdx);
                  end;
                end;
                if Assigned(CalcLogger) and CalcLogger.LevelEnabled(lTrace) then
                begin
                  sLog := '';
                  vVecIdx := 0;
                  for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
                  begin
                    if vBitIdx = Low(fMinimizationRegisters) then
                      sLog := sLog + Format('%s = %d',
                      [C_RegNames[fMinimizationRegisters[vBitIdx]],
                      FixedTrunc(gsl_vector_get (vMinimizeVect, vVecIdx)) ])
                    else
                      sLog := sLog + Format(', %s = %d',
                      [C_RegNames[fMinimizationRegisters[vBitIdx]],
                      FixedTrunc(gsl_vector_get (vMinimizeVect, vVecIdx)) ]);
                    inc(vVecIdx);
                  end;
                  CalcLogger.Log(lTrace,
                              Format('Processor Idx:%d. Board:%d, Pos:%d. '+
                              'Start minor:%d whith registers values: %s',
                  [fProcessorBitIdx, vParams^.DbObj.Board_SN, vParams^.DbObj.Board_Pos, vMinorIter, sLog]));
                end;
                //минимизируем в малом круге
                if Minimize(vMinimizeVect, vSSVect, vDimSize, @minex_func, self) then
                begin
                  try
                    vCurrError := CalcError(vMinimizeVect, pvoid(vParams));
                  except
                    if Assigned(CalcLogger) then
                      CalcLogger.Log(lWarning,
                                  Format('Processor Idx:%d. Board:%d, Pos:%d. Exception on calc error. Measure:%d major iter:%d minor iter:%d',
                        [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos, vSolIndex, vMajorIter, vMinorIter]));
                    Continue;
                  end;

                  if vCurrError < vMinorBest.Error then
                  begin
                    gsl_vector_memcpy (vMinorBest.Vector, vMinimizeVect);
                    try
                      vMinorBest.Calc(vParams);
                    except
                      if Assigned(CalcLogger) then
                        CalcLogger.Log(lWarning,
                                    Format('Processor Idx:%d. Board:%d, Pos:%d. Exception on minor best calc. Measure:%d major iter:%d minor iter:%d',
                          [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos, vSolIndex, vMajorIter, vMinorIter]));
                      Continue;
                    end;

                    if (vMinorBest.Error < vOutBest.Error) then
                    begin
                      vOutBest.Assign(vMinorBest);
                      if Assigned(CalcLogger) and CalcLogger.LevelEnabled(lEvent) then
                      begin
                        sLog := '';
                        vVecIdx := 0;
                        for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
                        begin
                          vParams^.DestRegisters.BitValue[fMinimizationRegisters[vBitIdx]] := FixedTrunc(gsl_vector_get (vMinorBest.Vector, vVecIdx));
                          if vBitIdx = Low(fMinimizationRegisters) then
                            sLog := sLog + Format('%s = %d',
                           [C_RegNames[fMinimizationRegisters[vBitIdx]] ,
                           vParams^.DestRegisters.BitValue[fMinimizationRegisters[vBitIdx]]])
                          else
                            sLog := sLog + Format(', %s = %d',
                           [C_RegNames[fMinimizationRegisters[vBitIdx]] ,
                           vParams^.DestRegisters.BitValue[fMinimizationRegisters[vBitIdx]]]);
                          inc(vVecIdx);
                        end;
                        CalcLogger.Log(lEvent,
                                    Format('Processor Idx:%d. Board:%d, Position:%d Best min added! '+
                                    'Major iter:%d Minor iter:%d. Sol:%d, '+
                                    'Registers Values: %s. Error:%8.5f; dReg:%8.5f; dF(25):%8.5f ppm; dF:%8.5f ppm',
                        [fProcessorBitIdx, vParams^.DbObj.Board_SN, vParams^.DbObj.Board_Pos,
                        vMajorIter, vMinorIter, vSolIndex + 1,
                        sLog, vMinorBest.Error, vMinorBest.dReg, vMinorBest.dF25, vMinorBest.dF]));
                      end;
                      vSolutionsInMeas.Add(TMinimizeSolution.Create(vDimSize));
                      vSolutionsInMeas.Last.Assign(vMinorBest);
                    end;
                  end;
                end;
                if not self.Terminated then
                begin
                  inc(vProgress);
                  vProgerssValue := vProgress * 100 div
                  (fIniParams.MinorIters * fIniParams.MajorIters * vParams^.Iter.MeasSolutions.Count);
                  if vItem.Progress <> vProgerssValue then
                  begin
                    vItem.Progress := vProgerssValue;
                    vCanWhile := UpdateProgress;
                    if not vCanWhile then
                    begin
                      Terminate;
                      Break;
                    end;
                  end;
                end;
                Sleep(1);
              end;/// end minor loop

            end; // end major loop
            vParams.CurrMeasure.CalcSolutions.Clear;
            vSolutionsInMeas.Sort;
            while vSolutionsInMeas.Count > vParams^.ProcessOptions.CalcOptions.MaxMeasuresCalcCount do
              vSolutionsInMeas.Delete(0);
            if vSolutionsInMeas.Count > 0 then
            begin
              vVecIdx := 0;
              while vVecIdx < vSolutionsInMeas.Count do
              begin
                vParams.CurrMeasure.CalcSolutions.NewItem(vSolIndex);
                vParams.CurrMeasure.CalcSolutions.Last.Registers.Assign(vParams.CurrMeasure.Registers);
                for vBitIdx := Low(fMinimizationRegisters) to High(fMinimizationRegisters) do
                begin
                  vParams.CurrMeasure.CalcSolutions.Last.Registers.BitValue[fMinimizationRegisters[vBitIdx]] :=
                  FixedTrunc(gsl_vector_get(vSolutionsInMeas[vVecIdx].Vector, vBitIdx));
                end;
                vParams.CurrMeasure.CalcSolutions.Last.CalcCurves;
                inc(vVecIdx);
              end;
            end else
            begin
              vParams.CurrMeasure.CalcSolutions.NewItem(vSolIndex);
              vParams.CurrMeasure.CalcSolutions.Last.Registers.Assign(vParams.CurrMeasure.Registers);
              vParams.CurrMeasure.CalcSolutions.Last.CalcCurves;
            end;
            inc(vSolIndex);
          end; // end meas solutions loop
          vParams^.Iter.Results.Calc_dF := 1E6;
          vParams^.Iter.Results.Calc_dF25 := 1E6;
          if Assigned(CalcLogger) then
              CalcLogger.Log(lDebug,
                              Format('Processor idx:%d. Board:%d, Pos:%d. - Minimize finished. Calculate results curves',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
          vParams.Iter.RecalcMeasureResults;
          if Assigned(CalcLogger) then
              CalcLogger.Log(lDebug,
                              Format('Processor idx:%d. Board:%d, Pos:%d. - Calculate results curves finished. Analyze results curves',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
          if (vParams^.Iter.Num >= vParams^.ProcessOptions.CalcOptions.ItersBeforeProgCount)
            and Assigned(vParams^.ToleranceGroup)
            and (vParams^.ToleranceGroup.Count > 0) then
          begin
            vSolIndex := 0;
            while vSolIndex < vParams^.Iter.MeasSolutions.Count do
            begin
              vCalcIdx := 0;
              while vCalcIdx < vParams^.Iter.MeasSolutions[vSolIndex].CalcSolutions.Count do
              begin
                if (vParams^.Iter.MeasSolutions[vSolIndex].CalcSolutions[vCalcIdx].Results.dF < vParams^.ToleranceGroup.Last.dF)
                    and (vParams^.Iter.MeasSolutions[vSolIndex].CalcSolutions[vCalcIdx].Results.dF25 < vParams^.ToleranceGroup.Last.dF25) then
                begin
                  vParams^.Iter.CalcResults.NewItem(vSolIndex);
                  vParams^.Iter.CalcResults.Last.Registers.Assign(vParams^.Iter.MeasSolutions[vSolIndex].CalcSolutions[vCalcIdx].Registers);
                  vParams^.Iter.CalcResults.Last.CalcCurves;
                end;
                Inc(vCalcIdx);
              end;
              inc(vSolIndex);
            end;
          end else
          begin
            vSolIndex := 0;
            while vSolIndex < vParams^.Iter.MeasSolutions.Count do
            begin
              vCalcIdx := 0;
              while vCalcIdx < vParams^.Iter.MeasSolutions[vSolIndex].CalcSolutions.Count do
              begin
                vParams^.Iter.CalcResults.NewItem(vSolIndex);
                vParams^.Iter.CalcResults.Last.Registers.Assign(vParams^.Iter.MeasSolutions[vSolIndex].CalcSolutions[vCalcIdx].Registers);
                vParams^.Iter.CalcResults.Last.CalcCurves;
                Inc(vCalcIdx);
              end;
              inc(vSolIndex);
            end;
          end;
          if vParams^.Iter.CalcResults.Count > 0 then
          begin
            vParams^.Iter.CalcResults.Sort(vByPriorIterSolutionsComparer);
            while vParams^.Iter.CalcResults.Count > vParams^.ProcessOptions.CalcOptions.MaxNextIterCalcCount do
              vParams^.Iter.CalcResults.Delete(0);
            vParams^.Iter.Results.Calc_dF := vParams^.Iter.CalcResults.Last.Results.dF;
            vParams^.Iter.Results.Calc_dF25 := vParams^.Iter.CalcResults.Last.Results.dF25;
          end;
          if Assigned(CalcLogger) then
              CalcLogger.Log(lDebug,
                              Format('Processor idx:%d. Board:%d, Pos:%d. - Analyze results curves finished. Apply tolreances and solve position state',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
          ProcessingStates(vParams);
          if Assigned(CalcLogger) then
              CalcLogger.Log(lInfo,
                              Format('Processor idx:%d. Board:%d, Pos:%d. - Reconsruction complete.',
                              [fProcessorBitIdx, vParams.DbObj.Board_SN, vParams.DbObj.Board_Pos]));
          vItem.State := isFinish;
          vCanWhile := UpdateProgress;
          if not vCanWhile then
          begin
            Terminate;
            Break;
          end;
        end;

      except on e : exception do
        begin
          if assigned(vItem) then
          begin
            inc(vItem.ExceptCount);
            if (vItem.ExceptCount <= 3) then
              vItem.State := isWait
            else
              vItem.State := isFinish;
            if assigned(vItem.MeasuredObj)
            and Assigned(CalcLogger) then
            begin
              if vItem.State = isWait then
                CalcLogger.Log(lWarning,
                              Format('Processor Idx:%d. Board:%d, Pos:%d. - Exceptions count: %d. State wait on exception! ',
                  [fProcessorBitIdx, vItem.MeasuredObj.Board_SN, vItem.MeasuredObj.Board_Pos, vItem.ExceptCount]))
              else
                CalcLogger.Log(lWarning,
                              Format('Processor Idx:%d. Board:%d, Pos:%d. - Exceptions count: %d. State finish on exception! ',
                  [fProcessorBitIdx, vItem.MeasuredObj.Board_SN, vItem.MeasuredObj.Board_Pos, vItem.ExceptCount]));
            end;
          end;
          if Assigned(CalcLogger) then
            CalcLogger.Log(lException, e);
        end;
      end;
    finally
      FreeAndNil(vParams.AddProgQR);
      fConnection.Close;
    end;
  finally
    fDMAccess := nil;
    fLoader := nil;
    SetLength(fMinimizationRegisters, 0);
    FreeAndNil(fConnection);
    FreeAndNil(vMajorBest);
    FreeAndNil(vMinorBest);
    FreeAndNil(vOutBest);
    gsl_vector_free(vMinimizeVect);
    gsl_vector_free(vSSVect);
    FreeAndNil(vSolutionsList);
    FreeAndNil(fRegistersWeights);
    FreeAndNil(fProcessOptions);
    FreeAndNil(fTolerances);
    FreeAndNil(vParams.DbObj);
    FreeAndNil(vParams.DestRegisters);
    Dispose(vParams);
    vByPriorCalcSolutionsInReconsComparer := nil;
    vByPriorIterSolutionsComparer := nil;
    if Assigned(CalcLogger) then
      CalcLogger.Log(lInfo, Format('Thread ID:%d Processor Idx:%d - Finished',
      [ThreadID, fProcessorBitIdx]));
  end;
end;

end.
