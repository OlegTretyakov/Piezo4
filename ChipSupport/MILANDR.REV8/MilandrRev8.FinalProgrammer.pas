unit MilandrRev8.FinalProgrammer;

interface
  uses AbstractStpMethod, MilandrRev8.Stp8TestThread;
  type

  TMilRev8FinalProgrammerThread = class(TMilRev8AbstractTestThread)
  protected
    function PreparePosition(const APosition : TProcessedPosition):Boolean; override;
    procedure StartMain; override;
  end;

  TMilRev8FinalProgrammer = class(TFinalProgrammerStpMethod)
   protected
    function ThreadClass : TStpThreadClass;override;
   public
    function ModuleName : string; override;
    function ReadyToStart:Boolean; override;
  end;

implementation

uses
  ChipAbstract,
  AbstractBoardInterface,
  PositionListInterface,
  ExtentionsListInterface,
  MilandrRev8Stp8dboExtention,
  System.SysUtils,
  VdpDMAccess,
  MilandrRev8.DboBase,
  dmPositionControllerInterface,
  ChipAbstractInterface,
  MilandrRev8IMS, Stp8PositionDBExtention, LoggerInterface;

{ TMilRev8FinalProgrammer }

function TMilRev8FinalProgrammer.ModuleName: string;
begin
  result := 'Milnandr rev.8 final programmer';
end;

function TMilRev8FinalProgrammer.ReadyToStart: Boolean;
var
vIdx : word;
vBoard : IAbstractBoard;
vTmp : IDMAccess;
vPositions : IPositionsList;
vPositionExtentions : IExtentions;
vDbExt : TStp8milDbExtention;
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
      for vIdx := 0 to vPositions.Count-1 do
      begin
        vPositionExtentions := nil;
        vDbExt := nil;
        if (not supports(vPositions[vIdx], IExtentions, vPositionExtentions))
        or (not vPositionExtentions.Find(TStp8milDbExtention, vDbExt)) then
          Continue;
        if (arProg in vDbExt.StartOptions.ControlOptions.AfterCompensOptions.s)
        and (vDbExt.InProgQueueState.pqState) then
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

function TMilRev8FinalProgrammer.ThreadClass: TStpThreadClass;
begin
  result := TMilRev8FinalProgrammerThread;
end;

{ TMilRev8FinalProgrammerThread }

function TMilRev8FinalProgrammerThread.PreparePosition(
  const APosition : TProcessedPosition): Boolean;
begin
  Result := False;
  if ProcessState = psPrepare then
  begin
      result := (APosition.DbExt.ItersCount > 0)
        and (arProg in APosition.DbExt.LastIter.NextIterOptions.ControlOptions.AfterCompensOptions.s)
        and (APosition.DbExt.InProgQueueState.pqState);
  end;
  if Result then
  begin
    APosition.Chip.Assign(APosition.DbExt.InitRegisters);
    APosition.Chip.Registers.TEST := 0;
  end;
end;

procedure TMilRev8FinalProgrammerThread.StartMain;
var
vSendVoltages : pPositionsVoltages;
vSuccCount, vIdx : Word;
vProcBits : TChipBytes;
vProcessPosition : TProcessedPosition;
vPosLog : TPosLogItem;
vDB : TEmbSQLDataBase;
vErrStateQR,
vDeleteQR : TvdQuery;
begin
  fExecResult := 1;
  ReadControlledBitsIndex(MilandrRev8IMS.TMilandrRev8Registers, 'stp8InitRegisters', False, vProcBits);
  New(vSendVoltages);
  vErrStateQR := fDMAccess.CreateWriteQuery(nil, 'UPDATE PROG_QUE SET RESULT = 1 WHERE Q_ID = :Q_ID', fConnection);
  vDeleteQR := fDMAccess.CreateWriteQuery(nil, 'DELETE FROM PROG_QUE WHERE Q_ID = :Q_ID', fConnection);
  try
    fBoardLogger.Log(lDebug, 'start Milandr Rev.8 programmer');
    vSendVoltages.VDD := ProcessPositions[0].CardProcessOptions.VDD;
    vSendVoltages.VC := ProcessPositions[0].CardProcessOptions.VC;
    vSendVoltages.VAnalog := 0;
    vSendVoltages.VProg := 8;
    fPositionsController.SetVoltageValues(vSendVoltages);
    fBoardLogger.Log(lTrace, Format('Send voltages. VDD:%5.3fv., VC:%8.5fv. VA:%8.5fv. VPrg:%8.5fv.',
        [vSendVoltages.VDD,
        vSendVoltages.VC,
        vSendVoltages.VAnalog,
        vSendVoltages.VProg]));
    if not SleepAndResume(fTimings.Voltage) then
      Exit;
    if not SendEnables(3000) then
      Exit;
    if not SleepAndResume(fTimings.AfterEnable) then
      Exit;
    if not CheckPowerVoltages(vSuccCount) then
      Exit;
    if (vSuccCount < ProcessPositions.Count) then
      DropInProtectPositions;
    if (ProcessPositions.Count < 1) then
      Exit;
    if not SleepAndResume(3000) then
      Exit;
    if Terminated then
      Exit;
    {start write}
    if not ProgrammerWriteRomAndResume(15000) then
      Exit;

    if not SleepAndResume(2000) then
      Exit;
    {start read}
    if not ProgrammerReadRomAndResume(5000) then
      Exit;
    vErrStateQR.Prepare;
    vDeleteQR.Prepare;
    for vIdx := 0 to ProcessPositions.Count-1 do
    begin
      vProcessPosition := ProcessPositions[vIdx];
      vProcessPosition.Succ :=
          vProcessPosition.DbExt.InitRegisters.IsIdenty(
                    vProcessPosition.Chip, vProcBits);
      vDB := vProcessPosition.DbExt.CreateConnection;
      try
        if not vProcessPosition.Succ then
        begin
          fBoardLogger.Log(lWarning, Format('position %d final programming fail. Write and read registers not identy',
                  [vProcessPosition.Position.BoardPos]));
          Exclude(vProcessPosition.DbExt.LastIter.NextIterOptions.ControlOptions.AfterCompensOptions.s, arProg);
          vProcessPosition.DbExt.InProgQueueState.pqResult := 1;
          if vProcessPosition.DbExt.LastIter.Write(vDB, False, [foNextOptions]) then
          begin
            vErrStateQR.ParamByName('q_id').AsInteger := vProcessPosition.DbExt.InProgQueueState.pqID;
            vErrStateQR.ExecSQL;
            vProcessPosition.DbExt.InProgQueueState.pqState := false;;
            vPosLog.DT := Now;
            vPosLog.Step := 'Прошивка';
            vPosLog.Result := 'Ошибка';
            vPosLog.Comment := 'Записанные и считанные значения регистров не совпали';
            vPosLog.Save(vDB);
          end else
            fBoardLogger.Log(lWarning, Format('position %d final programming. Error on data write on measure DB',
                  [vProcessPosition.Position.BoardPos]));
        end else
        begin
          Exclude(vProcessPosition.DbExt.LastIter.NextIterOptions.ControlOptions.AfterCompensOptions.s, arProg);
          vProcessPosition.DbExt.LastIter.NextIterOptions.ControlOptions.MeasureMode := pmFinalTest;
          if vProcessPosition.DbExt.LastIter.Write(vDB, False, [foNextOptions]) then
          begin
            vDeleteQR.ParamByName('q_id').AsInteger := vProcessPosition.DbExt.InProgQueueState.pqID;
            vDeleteQR.ExecSQL;
            vProcessPosition.DbExt.InProgQueueState.pqState := false;
            vPosLog.DT := Now;
            vPosLog.Step := 'Прошивка';
            vPosLog.Result := 'OK';
            vPosLog.Comment := '-';
            vPosLog.Save(vDB);
          end else
            fBoardLogger.Log(lWarning, Format('position %d final programming. Error on data write on measure DB',
                  [vProcessPosition.Position.BoardPos]));
        end;
      finally
        FreeAndNil(vDB);
      end;
    end;
    if vErrStateQR.Transaction.Active then
      fDMAccess.Commit(vErrStateQR);
    if vDeleteQR.Transaction.Active then
      fDMAccess.Commit(vDeleteQR);
    DropUnSuccPositions;
    if ProcessPositions.Count > 0 then
    begin
      for vIdx := 0 to ProcessPositions.Count-1 do
      begin
        vProcessPosition := ProcessPositions[vIdx];
        vProcessPosition.Position.Active := vProcessPosition.Succ;
      end;
      if not ProgrammerZState(5000) then
        exit;
      if not SendDisablesActive(5000) then
        Exit;
      if not SleepAndResume(3000) then
        Exit;
      if not SendEnables(3000) then
        Exit;
    end;
    fExecResult := 0;
  finally
    Dispose(vSendVoltages);
    FreeAndNil(vDeleteQR);
    FreeAndNil(vErrStateQR);
    SetLength(vProcBits, 0);
  end;
end;

end.
