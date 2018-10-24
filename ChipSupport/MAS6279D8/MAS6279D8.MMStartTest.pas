unit MAS6279D8.MMStartTest;

interface
  uses AbstractStpMethod, MAS6279D8.Stp8TestThread, mas6279d8.DboBase;

  type
  Tmasd8StartTest = class(TAbstractStpMethod)
   protected
    function ThreadClass : TStpThreadClass;override;
   public
    function ModuleName : string; override;
    function ReadyToStart:Boolean; override;
  end;
  Tmasd8StartTestThread = class(Tmasd8AbstractTestThread)
   private
    fTestSeqence : TStartTestProcOption;
   protected
    function PreparePosition(const APosition : TProcessedPosition):Boolean; override;
    procedure StartMain; override;
  end;

implementation

uses
  System.SysUtils,
  Vodopad.Math,
  AbstractBoardInterface,
  PositionListInterface,
  ExtentionsListInterface,
  ChipAbstract,
  ChipAbstractInterface,
  MAS6279D8IMS,
  mas6279dStp8dboExtention,
  VdpDMAccess,
  Stp8PositionDBExtention,
  LoggerInterface;


{ Tmasd8StartTest }

function Tmasd8StartTest.ModuleName: string;
begin
  result := 'Mas 6279D8 start tester';
end;

function Tmasd8StartTest.ReadyToStart: Boolean;
var
vIdx : word;
vTmp : IDMAccess;
vBoard : IAbstractBoard;
vPositions : IPositionsList;
vPositionExtentions : IExtentions;
vDbExt : TStp8masDbExtention;
begin
  try
    if not (rsChecked in fReady) then
    begin
      if (((not supports(fMainProc, IDMAccess, vTmp))
      or (not supports(fBoard, IAbstractBoard, vBoard))
      or (not vBoard.QueryPositionListInterface(IPositionsList, vPositions)))
      or (vPositions.Count < 1)) then
      begin
        vPositions := nil;
        vBoard := nil;
        vTmp := nil;
        Exit;
      end;
      for vIdx := 0 to vPositions.Count-1 do
      begin
        vPositionExtentions := nil;
        vDbExt := nil;
        if ((not supports(vPositions[vIdx], IExtentions, vPositionExtentions))
        or (not vPositionExtentions.Find(TStp8masDbExtention, vDbExt))) then
          Continue;
        include(fReady, rsOk);
      end;
      include(fReady, rsChecked);
    end;
  finally
    result := (rsOk in fReady);
    vPositionExtentions := nil;
    vPositions := nil;
    vBoard := nil;
    vTmp := nil;
  end;
end;

function Tmasd8StartTest.ThreadClass: TStpThreadClass;
begin
  result := Tmasd8StartTestThread;
end;

{ Tmasd8StartTestThread }

function Tmasd8StartTestThread.PreparePosition(
  const APosition : TProcessedPosition): Boolean;
var
vSto : TStartTestOptionsSet;
begin
  if ProcessState = psPrepare then
    result := True
  else
  begin
    vSto := APosition.DbExt.StartOptions.ControlOptions.StartTestOptions.s;
    result := (vSto <> []) and (fTestSeqence in vSto);
    if result then
    begin
      case fTestSeqence of
        cpoWRTest:
        begin
          APosition.Chip.Assign(APosition.DbExt.InitRegisters);
        end;
        cpoFreqTest:
        begin
          APosition.Chip.Assign(APosition.DbExt.InitRegisters);
          APosition.Chip.Registers.TEST := 0;
          APosition.Chip.Registers.DA := 1;
          APosition.Chip.Registers.CLK := 1;
        end;
      end;
    end;
  end;
end;

procedure Tmasd8StartTestThread.StartMain;
var
vProcessPosition : TProcessedPosition;
vPpmError : Double;
vIdx, vSuccCount : word;
vPosLog : TPosLogItem;
vProcBits : TChipBytes;
begin
  SetActiveProcessedPositions;
  if Terminated then
    Exit;
  if not SendPositionsVoltagesAndResume(5000) then
    Exit;
  if not ProgrammerZState(5000) then
    Exit;
  if not SendEnables(3000) then
    Exit;
 
  LogPositonsVoltagesStates;

  fTestSeqence := cpoWRTest;
  {$Region ' WR Test '}
  FillProcessPositions;
  if Terminated then
    exit;
  if GetActiveCount > 0 then
  try
    ReadControlledBitsIndex(MAS6279D8IMS.TMas6279D8Registers, 'stp8InitRegisters', False, vProcBits);
    fBoardLogger.Log(lDebug, 'prepare positions for W/R test');
    vIdx := 0;
    while (vIdx < ProcessPositions.Count) and (not Terminated) do
    begin
      vProcessPosition := ProcessPositions[vIdx];
      if vProcessPosition.Position.Active then
      begin
        vProcessPosition.DbExt.StepResultInclude(msrWRTestFail);
        vProcessPosition.DbExt.UpdatePosData([udStepsResult]);
      end;
      inc(vIdx);
    end;
    if Terminated then
      exit;
    fBoardLogger.Log(lDebug, 'activate positions for W/R test');
    if not SendEnables(3000) then
      Exit;
    if not SleepAndResume(fTimings.AfterEnable) then
      Exit;
    if not CheckPowerVoltages(vSuccCount) then
      Exit;

    if vSuccCount < ProcessPositions.Count then
    begin
      fBoardLogger.Log(lEvent, 'Drop positions which are out of tolerance after enables sending');
      DropUnSuccPositions;
      if Terminated then
        exit;
      if ProcessPositions.Count < 1 then
      begin
        fBoardLogger.Log(lWarning, 'No positions for processing. Halt');
        Exit;
      end;
      if not SendEnables(3000) then
        Exit;
    end;

    if not SleepAndResume(fTimings.AfterEnable) then
      Exit;

    LogPositonsVoltagesStates;
    {start write}
    if not ProgrammerWriteWorkAndResume(5000) then
      Exit;
    
    if not SleepAndResume(fTimings.AfterEnable) then
      Exit;

    {start read}
    if not ProgrammerReadWorkAndResume(5000) then
      Exit;

    LogPositonsVoltagesStates;
    for vIdx := 0 to ProcessPositions.Count-1 do
    begin
      if Terminated then
        break;
      vProcessPosition := ProcessPositions[vIdx];
      if vProcessPosition.Position.Active then
      begin
        vProcessPosition.Succ :=
            vProcessPosition.DbExt.InitRegisters.IsIdenty(
                      vProcessPosition.Chip, vProcBits);
        if not vProcessPosition.Succ then
        begin
          fBoardLogger.Log(lWarning, Format('position %d write and read registers not identy',
                  [vProcessPosition.Position.BoardPos]));
          vPosLog.DT := Now;
          vPosLog.Step := 'Write/read test';
          vPosLog.Result := 'fail';
          vPosLog.Comment := '-';
          vProcessPosition.DbExt.AddProcessLog(vPosLog);
        end else
        begin
          vPosLog.DT := Now;
          vPosLog.Step := 'Write/read test';
          vPosLog.Result := 'OK';
          vPosLog.Comment := '-';
          vProcessPosition.DbExt.AddProcessLog(vPosLog);
            vProcessPosition.DbExt.StartOptions.ControlOptions.StartTestOptions.s :=
            vProcessPosition.DbExt.StartOptions.ControlOptions.StartTestOptions.s - [cpoWRTest];
          vProcessPosition.DbExt.StepResultExclude(msrWRTestFail);
          vProcessPosition.DbExt.StepResultInclude(msrWRTestOk);
          vProcessPosition.Succ := vProcessPosition.DbExt.UpdatePosData([udStartOptions, udStepsResult]);
          if not vProcessPosition.Succ then
            fBoardLogger.Log(lWarning, Format('position %d update start options error',
                  [vProcessPosition.Position.BoardPos]));
        end;
      end;
    end;
  finally
    SetLength(vProcBits, 0);
    vSuccCount := GetSuccCount;
    if vSuccCount < ProcessPositions.Count then
    begin
      fBoardLogger.Log(lEvent, 'Drop positions which are not passed write-read test');
      DropUnSuccPositions;
    end;
  end;
  {$ENDREGION}
  if Terminated then
    Exit;
  fTestSeqence := cpoFreqTest;
  {$Region ' Freq test '}
  fBoardLogger.Log(lDebug, 'prepare positions for freq test');
  FillProcessPositions;
  if Terminated then
    Exit;
  if GetActiveCount > 0 then
  try
    vIdx := 0;
    while (vIdx < ProcessPositions.Count) and not Terminated do
    begin
      vProcessPosition := ProcessPositions[vIdx];
      if vProcessPosition.Position.Active then
      begin
        vProcessPosition.DbExt.StepResultInclude(msrFreqTestFail);
        vProcessPosition.DbExt.UpdatePosData([udStepsResult]);
      end;
      inc(vIdx);
    end;
    fBoardLogger.Log(lDebug, 'activate positions for freq test');
    if not SendEnables(3000) then
      Exit;

    if not SleepAndResume(fTimings.AfterEnable) then
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
      if not SleepAndResume(fTimings.AfterEnable) then
        Exit;
    end;

    LogPositonsVoltagesStates;
    vIdx := 0;
    while (vIdx < ProcessPositions.Count) and not Terminated do
    begin
      vProcessPosition := ProcessPositions[vIdx];
      vProcessPosition.Chip.Registers.DA := 1;
      vProcessPosition.Chip.Registers.CLK := 1;
      inc(vIdx);
    end;
    if not SleepAndResume(fTimings.AfterEnable) then
      Exit;

    {start write}
    if not ProgrammerWriteWorkAndResume(5000) then
      Exit;
    if not SleepAndResume(fTimings.Freq) then
      Exit;

    LogPositonsVoltagesStates;
    {start freq meas}
    if not FreqStartAndResume then
      Exit;

    for vIdx := 0 to ProcessPositions.Count-1 do
    begin
      if Terminated then
        Break;
      vProcessPosition := ProcessPositions[vIdx];
      if vProcessPosition.Position.Active then
      begin
        vProcessPosition.Succ :=
            vProcessPosition.Freq.FreqResult(fFreqResult);
        if vProcessPosition.Succ then
        begin
          vPpmError := Absppm(fFreqResult.Freq, vProcessPosition.DbExt.Nominal);
          fBoardLogger.Log(lTrace, Format('position %d actual freq %f Hz,'+
          'actual dispersion %f ppm, '+
          'nominal %f Hz, '+
          'actual deviation %f ppm, '+
          'max deviation %f ppm, '+
          'max dispersion %f ppm',
          [vProcessPosition.Position.BoardPos,
          fFreqResult.Freq,
          fFreqResult.DispersionPpm,
          vProcessPosition.DbExt.Nominal,
          vPpmError,
          vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDeviation,
          vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDispersion]));
          vProcessPosition.Succ :=
              (vPpmError < vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDeviation)
              and (fFreqResult.DispersionPpm < vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDispersion);

          if not vProcessPosition.Succ then
          begin
            fBoardLogger.Log(lWarning, Format('position %d freq test error',
                    [vProcessPosition.Position.BoardPos]));
            vPosLog.DT := Now;
            vPosLog.Step := 'Freq test';
            vPosLog.Result := 'fail';
            vPosLog.Comment := Format('actual freq %f Hz,'+
                            'actual dispersion %f ppm, '+
                            'nominal %f Hz, '+
                            'actual deviation %f ppm, '+
                            'max deviation %f ppm, '+
                            'max dispersion %f ppm',
                            [fFreqResult.Freq,
                            fFreqResult.DispersionPpm,
                            vProcessPosition.DbExt.Nominal,
                            vPpmError,
                            vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDeviation,
                            vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDispersion]);
            vProcessPosition.DbExt.AddProcessLog(vPosLog);
          end else
          begin
            vPosLog.DT := Now;
            vPosLog.Step := 'Freq test';
            vPosLog.Result := 'OK';
            vPosLog.Comment := Format('actual freq %f Hz,'+
                            'actual dispersion %f ppm, '+
                            'nominal %f Hz',
                            [fFreqResult.Freq,
                            fFreqResult.DispersionPpm,
                            vProcessPosition.DbExt.Nominal]);
            vProcessPosition.DbExt.AddProcessLog(vPosLog);
            vProcessPosition.DbExt.StepResultExclude(msrFreqTestFail);
            vProcessPosition.DbExt.StepResultInclude(msrFreqTestOk);
            vProcessPosition.DbExt.StartOptions.ControlOptions.StartTestOptions.s :=
            vProcessPosition.DbExt.StartOptions.ControlOptions.StartTestOptions.s - [cpoFreqTest];
            vProcessPosition.Succ := vProcessPosition.DbExt.UpdatePosData([udStartOptions, udStepsResult]);
            if not vProcessPosition.Succ then
              fBoardLogger.Log(lWarning, Format('position %d update start options error',
                    [vProcessPosition.Position.BoardPos]));
          end;
        end;
      end;
    end;
    if Terminated then
      Exit;
    fExecResult := 0;
  finally
    vSuccCount := GetSuccCount;
    if vSuccCount < ProcessPositions.Count then
    begin
      fBoardLogger.Log(lEvent, 'Drop positions which are not passed freq test');
      DropUnSuccPositions;
    end;
  end;
  {$ENDREGION}


end;

end.
