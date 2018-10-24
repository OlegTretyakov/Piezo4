unit MAS6279D8.MMPreparePoint;

interface
  uses AbstractStpMethod, MAS6279D8.Stp8TestThread, mas6279d8.DboBase, ToleranceObj,
  System.Generics.Collections;

  type
  Tmasd8PreparePoint = class(TAbstractStpMethod)
   protected
    function ThreadClass : TStpThreadClass;override;
   public
    function ModuleName : string; override;
    function ReadyToStart:Boolean; override;
  end;
  TToleranceGroupEx = class(TToleranceGroup)
    CardID : Integer;
  end;
  Tmasd8PreparePointTestThread = class(Tmasd8AbstractTestThread)
   private
    fTolerances : TObjectList<TToleranceGroupEx>;
    fTestSeqence : TPreparePointProcOption;
   protected
    function PositionClass : TProcessedPositionClass;override;
    function PreparePosition(const APosition : TProcessedPosition):Boolean; override;
    procedure StartMain; override;
    function InfTrim : Boolean;
    function CDACTrim : Boolean;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  Vodopad.Math,
  AbstractBoardInterface,
  dmLoggerInterface,
  PositionListInterface,
  ExtentionsListInterface,
  ChipAbstract,
  ChipAbstractInterface,
  MAS6279D8IMS,
  MAS6279D8.Consts,
  mas6279dStp8dboExtention,
  VdpDMAccess,
  Stp8PositionDBExtention,
  LoggerInterface;


{ Tmasd8StartTest }


function Tmasd8PreparePoint.ModuleName: string;
begin
  result := 'Mas 6279D8 preparer';
end;

function Tmasd8PreparePoint.ReadyToStart: Boolean;
var
vIdx : word;
vPpo : TPreparePointProcOptionSet;
vTmp : IDMAccess;
vBoard : IAbstractBoard;
vPositions : IPositionsList;
vPositionExtentions : IExtentions;
vDbExt : TStp8masDbExtention;
vBoardLog : IdmLogger;
begin
  try
    if not (rsChecked in fReady) then
    begin
      if ((not supports(fMainProc, IDMAccess, vTmp))
      or (not supports(fBoard, IAbstractBoard, vBoard))
      or (not vBoard.QueryPositionListInterface(IPositionsList, vPositions))
      or (vPositions.Count < 1)) then
      begin
        vPositions := nil;
        vBoard := nil;
        vTmp := nil;
        Exit;
      end;
      if not Supports(vBoard, IdmLogger, vBoardLog) then
        vBoardLog := nil;
      for vIdx := 0 to vPositions.Count-1 do
      begin
        vPositionExtentions := nil;
        vDbExt := nil;
        if ((not supports(vPositions[vIdx], IExtentions, vPositionExtentions))
        or (not vPositionExtentions.Find(TStp8masDbExtention, vDbExt))) then
          Continue;
        if vDbExt.ItersCount > 0 then
          vPpo := vDbExt.LastIter.Options.ControlOptions.PreparePointOptions.s
        else
          vPpo := vDbExt.StartOptions.ControlOptions.PreparePointOptions.s;
        if (vPpo <> []) then
        begin
          include(fReady, rsOk);
          if Assigned(vBoardLog) then
            vBoardLog.Log(lDebug, Format('position ¹%d ready for prepare point',[vDbExt.Board_Pos]));
        end else
        begin
          if Assigned(vBoardLog) then
            vBoardLog.Log(lDebug, Format('position ¹%d not ready for prepare point',[vDbExt.Board_Pos]));
        end;
      end;
      include(fReady, rsChecked);
      vBoardLog := nil;
    end;
  finally
    result := (rsOk in fReady);
    vPositionExtentions := nil;
    vBoardLog := nil;
    vPositions := nil;
    vBoard := nil;
    vTmp := nil;
  end;
end;

function Tmasd8PreparePoint.ThreadClass: TStpThreadClass;
begin
  result := Tmasd8PreparePointTestThread;
end;

type
TInfResult = record
  Inf, Test, DA, CLK : SmallInt;
  vTrim : Double;
end;
TInfResults = array [0..2] of TInfResult;
TInfTrimPosition = class(TProcessedPosition)
  InfResults : TInfResults;
  CalculatedInf : SmallInt;
  procedure CalcInf(ACurrTempr : double = 25);
end;

procedure TInfTrimPosition.CalcInf(ACurrTempr : double = 25);
var
vA, vB,
v_InfRoom,
vInfPoint : Double;
vBoardLogger : IdmLogger;
begin
  if (SameValue(InfResults[1].vTrim, 0, 0.01)
  or SameValue(InfResults[2].vTrim, 0, 0.01)) then
  begin
    CalculatedInf := -100;
    exit;
  end;
  vInfPoint := Self.CardProcessOptions.ControlOptions.InfPoint;
  vA := (InfResults[2].vTrim-InfResults[1].vTrim)/(InfResults[2].Inf-InfResults[1].Inf);
  vB := InfResults[1].vTrim-vA*InfResults[1].Inf;
  v_InfRoom := (InfResults[0].vTrim - vB)/vA;

  if not SameValue(ACurrTempr, vInfPoint, 0.1) then
  begin
    CalculatedInf := FixedTrunc(v_InfRoom -(vInfPoint - ACurrTempr)*7.69);
    if Supports(TestThread.Board, IdmLogger, vBoardLogger) then
      vBoardLogger.Log(lDebug, Format
        ('Position %d inf trim: '+
        'A=(VTrim2(%8.5f) - VTrim1(%8.5f))/(Inf2(%d)-Inf1(%d)) = %8.5f. '+
        'B=VTrim1(%8.5f)-A(%8.5f)*Inf1(%d)= %8.5f. '+
        'InfRoom=(VTInf(%8.5f) - B(%8.5f))/A(%8.5f) = %8.5f '+
        'INF=InfRoom(%8.5f) -(ATInf(%8.5f) - ATRoom(%8.5f))*7.69 = %d',
        [self.Position.BoardPos,
        InfResults[2].vTrim, InfResults[1].vTrim, InfResults[2].Inf, InfResults[1].Inf, vA,
        InfResults[1].vTrim, vA, InfResults[1].Inf, vB,
        InfResults[0].vTrim, vB, vA, v_InfRoom,
        v_InfRoom, vInfPoint, ACurrTempr, CalculatedInf]));
  end
  else
  begin
    CalculatedInf := FixedTrunc(v_InfRoom);
    if Supports(TestThread.Board, IdmLogger, vBoardLogger) then
      vBoardLogger.Log(lDebug, Format
        ('Position %d inf trim: '+
        'A=(VTrim2(%8.5f) - VTrim1(%8.5f))/(Inf2(%d)-Inf1(%d)) = %8.5f. '+
        'B=VTrim1(%8.5f)-A(%8.5f)*Inf1(%d) = %8.5f. '+
        'INF=(VTInf(%8.5f) - B(%8.5f))/A(%8.5f) = %d',
        [self.Position.BoardPos,
        InfResults[2].vTrim, InfResults[1].vTrim, InfResults[2].Inf, InfResults[1].Inf, vA,
        InfResults[1].vTrim, vA, InfResults[1].Inf, vB,
        InfResults[0].vTrim, vB, vA, CalculatedInf]));
  end;
  vBoardLogger := nil;
end;
type
TCDACTrimPosition = class(TProcessedPosition)
  CurrentBit : Byte;
  ToleranceGroup : TToleranceGroupEx;
  procedure CheckCDAC(MeasFreq: double);
  procedure IncCDAC;
  procedure DecCDAC;
  function EndCDAC: boolean;
  function CDACIsLimited: boolean;
end;

procedure TCDACTrimPosition.CheckCDAC(MeasFreq: double);
begin
  if (MeasFreq < self.DbExt.Nominal) then
    DecCDAC;
  Inc(CurrentBit);
  if not EndCDAC  then
    IncCDAC;
end;

procedure TCDACTrimPosition.IncCDAC;
begin
  if (CurrentBit < FixedTrunc(Log2(C_CDACC_Max + 1))) then
    Chip.Registers.CDACC := Chip.Registers.CDACC or ((C_CDACC_Mean+1) shr CurrentBit)
  else
    Chip.Registers.CDACF := Chip.Registers.CDACF
      or ((C_CDACF_Mean+1) shr (CurrentBit - FixedTrunc(Log2(C_CDACC_Max + 1))));
end;

procedure TCDACTrimPosition.DecCDAC;
begin
 if (CurrentBit < FixedTrunc(Log2(C_CDACC_Max + 1))) then
    Chip.Registers.CDACC := Chip.Registers.CDACC and not ((C_CDACC_Mean+1) shr CurrentBit)
  else
    Chip.Registers.CDACF := Chip.Registers.CDACF
      and not ((C_CDACF_Mean+1) shr (CurrentBit - FixedTrunc(Log2(C_CDACC_Max + 1))))
end;

function TCDACTrimPosition.EndCDAC: boolean;
begin
  result := (CurrentBit > C_CDACMaxBit);
end;

function TCDACTrimPosition.CDACIsLimited: boolean;
begin
  result := (((Chip.Registers.CDACC = C_CDACC_Max) and (Chip.Registers.CDACF = C_CDACF_Max))
    or ((Chip.Registers.CDACC = C_CDACC_Min) and (Chip.Registers.CDACF = C_CDACF_Min)));
end;

{ Tmasd8StartTestThread }

function Tmasd8PreparePointTestThread.CDACTrim: Boolean;
var
vIdx : Integer;
vSuccCount, vUnSuccCount : word;
vProcessPosition : TCDACTrimPosition;
vPpmError : Double;
vUpdatedFields : TUpdatedFields;
vPosLog : TPosLogItem;
begin
  Result := False;
  if not SendPositionsVoltagesAndResume(5000) then
    Exit;
  fBoardLogger.Log(lDebug, 'activate positions for CDAC trim');
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.Freq) then
    Exit;
  if not CheckPowerVoltages(vSuccCount) then
    Exit;
  if vSuccCount < ProcessPositions.Count then
  begin
    fBoardLogger.Log(lEvent, 'Drop positions which are out of tolerance after enables sending');
    DropUnSuccPositions;
    if ProcessPositions.Count < 1 then
    begin
      fBoardLogger.Log(lWarning, 'No positions for processing. Halt');
      Exit;
    end;
    if not SendEnables(3000) then
      Exit;
    if not SleepAndResume(fTimings.Freq) then
      Exit;
  end;
  while ProcessPositions.Count > 0 do
  begin
    LogPositonsVoltagesStates;
    {start write}
    if not ProgrammerWriteWorkAndResume(5000) then
      Break;
    if not SleepAndResume(fTimings.Freq) then
      Break;
    {start freq meas}
    if not FreqStartAndResume then
      Break;
    vIdx := ProcessPositions.Count-1;
    vSuccCount := 0;
    vUnSuccCount := 0;
    while vIdx > -1 do
    begin
      vProcessPosition := ProcessPositions[vIdx] as TCDACTrimPosition;
      vProcessPosition.Succ :=
            vProcessPosition.Freq.FreqResult(fFreqResult);
      if vProcessPosition.Succ then
      begin
        vProcessPosition.CheckCDAC(fFreqResult.Freq);
        if vProcessPosition.EndCDAC then
        begin
          vPpmError := Absppm(fFreqResult.Freq, vProcessPosition.DbExt.Nominal);
          if (vProcessPosition.ToleranceGroup.Count > 0) then
            vProcessPosition.Succ := (vPpmError < vProcessPosition.ToleranceGroup.Last.dF25)
          else
            vProcessPosition.Succ := True;
          vPosLog.DT := Now;
          vPosLog.Step := 'CDAC triming';
          if vProcessPosition.Succ then
          begin
            if vProcessPosition.ToleranceGroup.Count < 1 then
              vPosLog.Result := 'OK(Tolerance group empty)'
            else
              vPosLog.Result := 'OK';
            vPosLog.Comment := Format('dF=%f ppm < max dF(%f ppm). CDAC last freq:%f Hz. dF:%f ppm '+
            'CDACC=%d, CDACF=%d',
            [vPpmError, vProcessPosition.ToleranceGroup.Last.dF25, fFreqResult.Freq, vPpmError,

            vProcessPosition.Chip.Registers.CDACC,
            vProcessPosition.Chip.Registers.CDACF]);

          end else
          begin
            vPosLog.Result := 'Fail';
            vPosLog.Comment := Format('CDAC last freq:%f Hz. dF:%f ppm '+
            'CDACC=%d, CDACF=%d',
            [fFreqResult.Freq, vPpmError,
            vProcessPosition.Chip.Registers.CDACC,
            vProcessPosition.Chip.Registers.CDACF]);
          end;
          vProcessPosition.DbExt.AddProcessLog(vPosLog);
          if vProcessPosition.Succ then
          begin
            vUpdatedFields := [udStartOptions, udInitRegisters, udStepsResult];
            if vProcessPosition.CDACIsLimited
              and vProcessPosition.CardProcessOptions.ControlOptions.AllowChangeCDACNominal then
            begin
              vProcessPosition.DbExt.Nominal := fFreqResult.Freq;
              Include(vUpdatedFields, udNominal);
            end;
            Exclude(vProcessPosition.DbExt.StartOptions.ControlOptions.PreparePointOptions.s, ncCDAC);
            vProcessPosition.DbExt.InitRegisters.Registers.CDACC := vProcessPosition.Chip.Registers.CDACC;
            vProcessPosition.DbExt.InitRegisters.Registers.CDACF := vProcessPosition.Chip.Registers.CDACF;

            vProcessPosition.DbExt.StepResultExclude(msrCDACTrimFail);
            vProcessPosition.DbExt.StepResultInclude(msrCDACTrimOk);
            vProcessPosition.Succ := vProcessPosition.DbExt.UpdatePosData(vUpdatedFields);
            if vProcessPosition.Succ then
            begin
              ProcessPositions.Delete(vIdx);
              Inc(vSuccCount);
            end else
              Inc(vUnSuccCount);
          end else
            Inc(vUnSuccCount);
        end;
      end else
      begin
        fBoardLogger.Log(lWarning, Format('position %d freq results extract error',[vProcessPosition.Position.BoardPos]));
        Inc(vUnSuccCount);
      end;
      Dec(vIdx);
    end;
    if ((vSuccCount > 0) or (vUnSuccCount > 0)) then
    begin
      if vUnSuccCount > 0 then
      begin
        fBoardLogger.Log(lWarning, 'Drop positions whith errors during CDAC triming');
        DropUnSuccPositions;
      end;
      SetActiveProcessedPositions;
      if not SendEnables(3000) then
      begin
        Break;
        Terminate;
      end;
    end;
  end;
  Result := ProcessPositions.Count = 0;
end;

function Tmasd8PreparePointTestThread.InfTrim: Boolean;
var
vTestIdx : Byte;
vIdx,
vSuccCount : word;
vProcessPosition : TInfTrimPosition;
vCurrTempr : Double;
vPosLog : TPosLogItem;
begin
  Result := False;
  fBoardLogger.Log(lDebug, 'activate positions for Inf Trim');
  if not SendPositionsVoltagesAndResume(5000) then
    Exit;
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.AfterEnable) then
    Exit;
  if not CheckPowerVoltages(vSuccCount) then
    Exit;
  try
    if vSuccCount < ProcessPositions.Count then
    begin
      fBoardLogger.Log(lEvent, 'Drop positions which are out of tolerance after enables sending');
      DropUnSuccPositions;
      if ProcessPositions.Count < 1 then
      begin
        fBoardLogger.Log(lWarning, 'No positions for processing. Halt');
        Exit;
      end;
      if not SendEnables(3000) then
        Exit;
      if not SleepAndResume(fTimings.AfterEnable) then
        Exit;
    end;
    for vTestIdx := Low(TInfResults) to High(TInfResults) do
    begin
      if not SleepAndResume(fTimings.Voltage) then
        break;
      for vIdx := 0 to ProcessPositions.Count-1 do
      begin
        vProcessPosition := ProcessPositions[vIdx] as TInfTrimPosition;
        vProcessPosition.Chip.Registers.TEST := vProcessPosition.InfResults[vTestIdx].Test;
        vProcessPosition.Chip.Registers.INF := vProcessPosition.InfResults[vTestIdx].Inf;
        vProcessPosition.Chip.Registers.DA := vProcessPosition.InfResults[vTestIdx].DA;
        vProcessPosition.Chip.Registers.CLK := vProcessPosition.InfResults[vTestIdx].CLK;
        if (vTestIdx = Low(TInfResults)) then
          vProcessPosition.Succ := false;
      end;
      LogPositonsVoltagesStates;
      {start write}
      if not ProgrammerWriteWorkAndResume(5000) then
        break;
      if not SleepAndResume(fTimings.Voltage) then
        break;
      for vIdx := 0 to ProcessPositions.Count-1 do
      begin
        vProcessPosition := ProcessPositions[vIdx] as TInfTrimPosition;
        vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);
        vProcessPosition.InfResults[vTestIdx].vTrim := fPositionVoltages.Analog;
        if (vTestIdx = High(TInfResults)) then
          vProcessPosition.Succ := True;
      end;
      if (vTestIdx = High(TInfResults)) then
        Result := True;
    end;
    if not Result then
      Exit;
    vCurrTempr := fChamberProcess.CurrTempr;
    for vIdx := 0 to ProcessPositions.Count-1 do
    begin
      vProcessPosition := ProcessPositions[vIdx] as TInfTrimPosition;
      vProcessPosition.CalcInf(vCurrTempr);
      vProcessPosition.Succ := ((vProcessPosition.CalculatedInf <> -100)
        and (vProcessPosition.CalculatedInf >= C_INF_Min) and (vProcessPosition.CalculatedInf <= C_INF_Max));
      if vProcessPosition.Succ then
      begin
        vPosLog.DT := Now;
        vPosLog.Step := 'INF Trim';
        vPosLog.Result := 'OK';
        vPosLog.Comment := Format('Calculated Inf: %d',[vProcessPosition.CalculatedInf]);
        vProcessPosition.DbExt.AddProcessLog(vPosLog);
        vProcessPosition.DbExt.InitRegisters.Registers.INF := vProcessPosition.CalculatedInf;
        vProcessPosition.Chip.Registers.INF := vProcessPosition.CalculatedInf;
        if (ncInfTrim1 in vProcessPosition.DbExt.StartOptions.ControlOptions.PreparePointOptions.s) then
          Exclude(vProcessPosition.DbExt.StartOptions.ControlOptions.PreparePointOptions.s, ncInfTrim1)
        else
          Exclude(vProcessPosition.DbExt.StartOptions.ControlOptions.PreparePointOptions.s, ncInfTrim2);
        vProcessPosition.DbExt.StepResultExclude(msrInfTrimFail);
        vProcessPosition.DbExt.StepResultInclude(msrInfTrimOk);
        vProcessPosition.Succ := vProcessPosition.DbExt.UpdatePosData([udStartOptions, udInitRegisters, udStepsResult]);
        if not vProcessPosition.Succ then
          fBoardLogger.Log(lWarning, Format('position %d update start options error',
                    [vProcessPosition.Position.BoardPos]));
      end else
      begin
        fBoardLogger.Log(lWarning, Format('position %d INF Trim calculation fail',
                    [vProcessPosition.Position.BoardPos]));
        vPosLog.DT := Now;
        vPosLog.Step := 'INF Trim';
        vPosLog.Result := 'fail';
        vPosLog.Comment := Format('calculated inf:%d not applied',[vProcessPosition.CalculatedInf]);
        vProcessPosition.DbExt.AddProcessLog(vPosLog);
        vProcessPosition.Succ := True;
        vProcessPosition.Chip.Registers.INF := vProcessPosition.DbExt.InitRegisters.Registers.INF;
        if (ncInfTrim1 in vProcessPosition.DbExt.StartOptions.ControlOptions.PreparePointOptions.s) then
          Exclude(vProcessPosition.DbExt.StartOptions.ControlOptions.PreparePointOptions.s, ncInfTrim1)
        else
          Exclude(vProcessPosition.DbExt.StartOptions.ControlOptions.PreparePointOptions.s, ncInfTrim2);
        vProcessPosition.DbExt.StepResultExclude(msrInfTrimFail);
        vProcessPosition.DbExt.StepResultInclude(msrInfTrimOk);
        vProcessPosition.Succ := vProcessPosition.DbExt.UpdatePosData([udStartOptions, udInitRegisters, udStepsResult]);
        if not vProcessPosition.Succ then
          fBoardLogger.Log(lWarning, Format('position %d update start options error',
                    [vProcessPosition.Position.BoardPos]));
      end;
    end;
  finally
    vSuccCount := GetSuccCount;
    if (vSuccCount <> ProcessPositions.Count) then
      DropUnSuccPositions;
  end;
end;

function Tmasd8PreparePointTestThread.PositionClass: TProcessedPositionClass;
begin
  if ProcessState = psPrepare then
    Result := inherited PositionClass
  else
  case fTestSeqence of
    ncInfTrim1, ncInfTrim2:
      Result := TInfTrimPosition;
    ncCDAC:
      Result := TCDACTrimPosition;
  end;
end;

function Tmasd8PreparePointTestThread.PreparePosition(
  const APosition : TProcessedPosition): Boolean;
var
vPpo : TPreparePointProcOptionSet;
function GetTolerance(ACardID : Integer; var oTolerance : TToleranceGroupEx):boolean;
var
vPoIdx : Word;
begin
  vPoIdx := 0;
  result := false;
  while (vPoIdx < fTolerances.Count) do
  begin
    Result := fTolerances[vPoIdx].CardID = ACardID;
    if Result then
      break ;
    Inc(vPoIdx);
  end;
  if Result then
    oTolerance := fTolerances[vPoIdx]
  else
  try
    oTolerance := TToleranceGroupEx.Create;
    fTolerances.Add(oTolerance);
    oTolerance.CardID := ACardID;
    oTolerance.LoadByCardID(fDMAccess, fConnection, ACardID);
    Result := True;
  except
    on E : Exception do
      fBoardLogger.Log(lException, E.Message);
  end;
end;
begin

  if APosition.DbExt.ItersCount > 0 then
    vPpo := APosition.DbExt.LastIter.Options.ControlOptions.PreparePointOptions.s
  else
    vPpo := APosition.DbExt.StartOptions.ControlOptions.PreparePointOptions.s;
  if ProcessState = psPrepare then
    result := (vPpo <> [])
  else
  begin
    result := (fTestSeqence in vPpo);
    if result then
    begin
      case fTestSeqence of
        ncInfTrim1, ncInfTrim2:
        begin
          Result := (APosition is TInfTrimPosition);
          if not Result then
            Exit;
          APosition.Chip.Assign(APosition.DbExt.InitRegisters);//, vBitsIdx);}
          APosition.Chip.Registers.OFS := 8;
          APosition.Chip.Registers.LIN := 0;
          APosition.Chip.Registers.SQ := 0;
          APosition.Chip.Registers.CUB := 0;
          APosition.Chip.Registers.FOUR := 0;
          APosition.Chip.Registers.FIFTH := 0;
          APosition.Chip.Registers.XOPD := 1;
          TInfTrimPosition(APosition).InfResults[0].Inf := 134;
          TInfTrimPosition(APosition).InfResults[1].Inf := 80;
          TInfTrimPosition(APosition).InfResults[2].Inf := 160;
          TInfTrimPosition(APosition).InfResults[0].Test := 3;
          TInfTrimPosition(APosition).InfResults[1].Test := 2;
          TInfTrimPosition(APosition).InfResults[2].Test := 2;
          TInfTrimPosition(APosition).InfResults[0].DA := 0;
          TInfTrimPosition(APosition).InfResults[1].DA := 0;
          TInfTrimPosition(APosition).InfResults[2].DA := 0;
          TInfTrimPosition(APosition).InfResults[0].CLK := 1;
          TInfTrimPosition(APosition).InfResults[1].CLK := 1;
          TInfTrimPosition(APosition).InfResults[2].CLK := 1;
        end;
        ncCDAC:
        begin
          Result := (APosition is TCDACTrimPosition);
          if not Result then
            Exit;
          APosition.Chip.Assign(APosition.DbExt.InitRegisters);
          APosition.Chip.Registers.TEST := 0;
          APosition.Chip.Registers.XOPD := 1;
          APosition.Chip.Registers.CDACC := C_CDACC_Mean+1;
          APosition.Chip.Registers.CDACF := C_CDACF_Min;
          APosition.Chip.Registers.DA := 1;
          APosition.Chip.Registers.CLK := 1;
          TCDACTrimPosition(APosition).CurrentBit := 0;
          Result := GetTolerance(APosition.DbExt.RCardID, TCDACTrimPosition(APosition).ToleranceGroup);
        end;
      end;
    end;
  end;
end;

procedure Tmasd8PreparePointTestThread.StartMain;
var
vProcessPosition : TProcessedPosition;
vIdx : word;
//vProcBits : TChipBytes;
begin
  if Terminated then
    exit;
  fTolerances := TObjectList<TToleranceGroupEx>.Create;
  try
    fTestSeqence := ncInfTrim1;
    //ReadControlledBitsIndex(MAS6279D8IMS.TMas6279D8Registers, 'stp8InitRegisters', False, vProcBits);
    {vIdx := 0;
    while vIdx < ProcessPositions.Count do
    begin
      vProcessPosition := ProcessPositions[vIdx];
      vProcessPosition.DbExt.ProcessState := wsPreparePoint;
      vProcessPosition.DbExt.ProcessResult := prNone;
      vProcessPosition.DbExt.UpdatePosData([udProcessState, udProcessResult]);
      inc(vIdx);
    end;  }
    {ncInfTrim1}
    fBoardLogger.Log(lDebug, 'prepare positions for Inf Trim');
    FillProcessPositions;
    if Terminated then
      exit;
    SetActiveProcessedPositions;
    if ProcessPositions.Count > 0 then
    begin
      fExecResult := 1;
      vIdx := 0;
      while (vIdx < ProcessPositions.Count)  and (not Terminated) do
      begin
        vProcessPosition := ProcessPositions[vIdx];
        vProcessPosition.DbExt.StepResultInclude(msrInfTrimFail);
        vProcessPosition.DbExt.UpdatePosData([udStepsResult]);
        inc(vIdx);
      end;
      if Terminated then
        exit;
      if not InfTrim then
        Exit;
      fExecResult := 0;
    end;
    if Terminated then
      Exit;
    fTestSeqence := ncCDAC;
    {CDAC TRIM}
    fBoardLogger.Log(lDebug, 'prepare positions for CDAC trim');
    FillProcessPositions;
    if Terminated then
      exit;
    SetActiveProcessedPositions;
    if ProcessPositions.Count > 0 then
    begin
      fExecResult := 1;
      vIdx := 0;
      while (vIdx < ProcessPositions.Count) and (not Terminated) do
      begin
        vProcessPosition := ProcessPositions[vIdx];
        vProcessPosition.DbExt.StepResultInclude(msrCDACTrimFail);
        vProcessPosition.DbExt.UpdatePosData([udStepsResult]);
        inc(vIdx);
      end;
      if Terminated then
        exit;
      if not CDACTrim then
        Exit;
      fExecResult := 0;
    end;
    if Terminated then
      Exit;
    if ProcessPositions.Count < 1 then
    begin
      fBoardLogger.Log(lWarning, 'No positions for processing. Halt');
      Exit;
    end;
    fTestSeqence := ncInfTrim2;
    {ncInfTrim2}
    fBoardLogger.Log(lDebug, 'prepare positions for Inf Trim');
    FillProcessPositions;
    if Terminated then
      exit;
    SetActiveProcessedPositions;
    if ProcessPositions.Count > 0 then
    begin
      fExecResult := 1;
      vIdx := 0;
      while (vIdx < ProcessPositions.Count) and (not Terminated) do
      begin
        vProcessPosition := ProcessPositions[vIdx];
        vProcessPosition.DbExt.StepResultInclude(msrInfTrimFail);
        vProcessPosition.DbExt.UpdatePosData([udStepsResult]);
        inc(vIdx);
      end;
      if Terminated then
        exit;
      InfTrim;
      fExecResult := 0;
    end;
  finally
    FreeAndNil(fTolerances);
  end;
end;

end.

