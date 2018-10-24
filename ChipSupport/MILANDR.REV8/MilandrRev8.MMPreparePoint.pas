unit MilandrRev8.MMPreparePoint;

interface
  uses AbstractStpMethod, MilandrRev8.Stp8TestThread, MilandrRev8.DboBase, ToleranceObj,
  System.Generics.Collections;

  type
  TMilRev8PreparePoint = class(TAbstractStpMethod)
   protected
    function ThreadClass : TStpThreadClass;override;
   public
    function ModuleName : string; override;
    function ReadyToStart:Boolean; override;
  end;
  TToleranceGroupEx = class(TToleranceGroup)
    CardID : Integer;
  end;
  TMilRev8PreparePointTestThread = class(TMilRev8AbstractTestThread)
   private
    fTolerances : TObjectList<TToleranceGroupEx>;
    fTestSeqence : TPreparePointProcOption;
   protected
    function PositionClass : TProcessedPositionClass;override;
    function PreparePosition(const APosition : TProcessedPosition):Boolean; override;
    procedure StartMain; override;
    //function InfTrim : Boolean;
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
  MilandrRev8IMS,
  MilandrRev8.Consts,
  MilandrRev8Stp8dboExtention,
  VdpDMAccess,
  Stp8PositionDBExtention,
  LoggerInterface;


{ TMilRev8PreparePoint }


function TMilRev8PreparePoint.ModuleName: string;
begin
  result := 'Milnandr rev.8 preparer';
end;

function TMilRev8PreparePoint.ReadyToStart: Boolean;
var
vIdx : word;
vPpo : TPreparePointProcOptionSet;
vTmp : IDMAccess;
vBoard : IAbstractBoard;
vPositions : IPositionsList;
vPositionExtentions : IExtentions;
vDbExt : TStp8milDbExtention;
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
        or (not vPositionExtentions.Find(TStp8milDbExtention, vDbExt))) then
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

function TMilRev8PreparePoint.ThreadClass: TStpThreadClass;
begin
  result := TMilRev8PreparePointTestThread;
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

{ TMilRev8PreparePointTestThread }

function TMilRev8PreparePointTestThread.CDACTrim: Boolean;
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
  if (vSuccCount < ProcessPositions.Count) then
    DropInProtectPositions;
  if (ProcessPositions.Count < 1) then
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
            [vPpmError, vProcessPosition.ToleranceGroup.Last.dF25,
            fFreqResult.Freq, vPpmError,
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

function TMilRev8PreparePointTestThread.PositionClass: TProcessedPositionClass;
begin
  if ProcessState = psPrepare then
    Result := inherited PositionClass
  else
  if fTestSeqence = ncCDAC then
      Result := TCDACTrimPosition
  else
    Result := inherited PositionClass;
end;

function TMilRev8PreparePointTestThread.PreparePosition(
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
        (*ncInfTrim1, ncInfTrim2:
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
        end; *)
        ncCDAC:
        begin
          Result := (APosition is TCDACTrimPosition);
          if not Result then
            Exit;
          APosition.Chip.Assign(APosition.DbExt.InitRegisters);
          APosition.Chip.Registers.TEST := 0;
          //APosition.Chip.Registers.XOPD := 1;
          APosition.Chip.Registers.CDACC := C_CDACC_Mean+1;
          APosition.Chip.Registers.CDACF := C_CDACF_Min;
          {APosition.Chip.Registers.DA := 1;
          APosition.Chip.Registers.CLK := 1; }
          TCDACTrimPosition(APosition).CurrentBit := 0;
          Result := GetTolerance(APosition.DbExt.RCardID, TCDACTrimPosition(APosition).ToleranceGroup);
        end;
      end;
    end;
  end;
end;

procedure TMilRev8PreparePointTestThread.StartMain;
var
vProcessPosition : TProcessedPosition;
vIdx : word;
//vProcBits : TChipBytes;
begin
  if Terminated then
    exit;
  fTolerances := TObjectList<TToleranceGroupEx>.Create;
  try
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
  finally
    FreeAndNil(fTolerances);
  end;
end;

end.

