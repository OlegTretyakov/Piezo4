unit MilandrRev8.Stp8TestThread;

interface
   uses
    System.Classes,
    AbstractStpMethod, System.Generics.Collections, Winapi.Windows, System.Math,
    System.IniFiles, System.SysUtils, dmLoggerInterface, LoggerInterface,
    ChamberInterfaces, VdpDMAccess, MilandrRev8IMS, MilandrRev8.Consts,
    Stp8PositionDBExtention, MilandrRev8.DboBase, MilandrRev8Stp8dboExtention, AbstractBoardInterface,
    ExtentionsListInterface, PositionListInterface, PositionInterface, EventBusInterface,
    dmReadInterface, dmFreqMeasurerInterface, dmPositionControllerInterface, dmVoltageConsts,
    dmPositionVoltmeterInterface, dmProgrammerInterface;

  type


  TProcessState = (psPrepare, psProcessing, psSleep, psWaitSendVoltage,
                  psWaitSendState, psWaitBitsWorkWrite, psWaitBitsWorkRead,
                  psWaitBitsRomWrite, psWaitBitsRomRead, psWaitProgrammerZstate, psWaitFreq);

  TMilRev8AbstractTestThread = class;
  TProcessedPosition = class(TObject)
   public
    TestThread : TMilRev8AbstractTestThread;
    Succ : Boolean;
    Position : IPosition;
    Extentions : IExtentions;
    Controller : IdmPositionController;
    Freq : IdmPositionFreqResult;
    Voltages : IPositionVoltages;
    Chip : TMilandrRev8Registers;
    CardProcessOptions : TStp8milProcessDBOptions;
    DbExt : TStp8milDbExtention;
    constructor Create(const ATestThread : TMilRev8AbstractTestThread); virtual;
    destructor Destroy;override;
  end;
  TProcessedPositionClass = class of TProcessedPosition;

  TEvents = record
    OprerationSucc,
    Stop : THandle;
  end;
  TProcessorTimings = record
    Voltage,
    Freq,
    AfterEnable : Word; {mSec}
    procedure Read;
  end;
  TMilRev8AbstractTestThread = class(TStpThread)
  private
    fThreadFormatSettings : TFormatSettings;
    fEvents : TEvents;
    fProcessPositions : TObjectList<TProcessedPosition>;
    fProcessOptions : TObjectList<TStp8milProcessDBOptions>;
    fProcessState : TProcessState;
    procedure FreqMeterEvent(Sender: TObject; Event: TGUID; Params: Pointer);
    procedure OnProgrammerEvent(Sender: TObject; Event: TGUID; Params: Pointer);
    procedure PosControllerEvent(Sender: TObject; Event: TGUID;
      Params: Pointer);
   protected
    fTimings : TProcessorTimings;
    fPositionVoltages : pPositionVoltages;
    fFreqResult : pFreqResult;
    fConnection : TvdConnection;
    fDMAccess : IDMAccess;
    fChamberProcess : IChamberProcess;
    fAbstractBoard : IAbstractBoard;
    fPositionsList : IPositionsList;
    fPosListExtentions : IExtentions;
    fFreqStarter : IdmFreqStarter;
    fFreqController : IdmFreqController;
    fPositionsController : IdmPositionsController;
    fPositionsProgrammer : IdmProgrammer;
    fBoardLogger : IdmLogger;
    function PositionClass : TProcessedPositionClass;virtual;
    procedure FillProcessPositions;
    function CheckPowerVoltages(var SuccCount: Word): boolean;
    function PreparePosition(const APosition : TProcessedPosition):Boolean; virtual;abstract;
    function RemoveInactive: Boolean;
    function IndexOfProcessedPosition(ANum: Byte): integer;
    procedure UpdatePositionsState;
    procedure DropInProtectPositions;
    procedure DropUnSuccPositions;
    procedure SetActiveProcessedPositions;
    function GetSuccCount: Word;
    function SleepAndResume(ASleepTime: Cardinal): Boolean;
    procedure LogPositonsVoltagesStates;
    procedure LogPositonsRegisters;
    procedure LogPositonsFreqs;
    procedure ReInitPositions;
    function SendPositionsVoltagesAndResume(ATimeOut: Cardinal): Boolean;
    function ProgrammerWriteWorkAndResume(ATimeOut: Cardinal): Boolean;
    function ProgrammerReadWorkAndResume(ATimeOut: Cardinal): Boolean;
    function ProgrammerWriteRomAndResume(ATimeOut: Cardinal): Boolean;
    function ProgrammerReadRomAndResume(ATimeOut: Cardinal): Boolean;
    function ProgrammerZState(ATimeOut: Cardinal): Boolean;
    function FreqStartAndResume: Boolean;
    function GetActiveCount: Word;
    function SendEnables(ATimeOut: Cardinal): boolean;
    function SendDisablesActive(ATimeOut: Cardinal): boolean;
    procedure Execute; override;
    procedure StartMain; virtual; abstract;
    property ProcessPositions : TObjectList<TProcessedPosition> read fProcessPositions;
    property ProcessOptions : TObjectList<TStp8milProcessDBOptions> read fProcessOptions;
   public
    procedure Stop; override;
    property ProcessState : TProcessState read fProcessState;
  end;

const
C_ProcWaitStateStr : array[TProcessState] of string = (
'psPrepare', 'psProcessing', 'psSleep', 'psWaitSendVoltage',
                  'psWaitSendState', 'psWaitBitsWorkWrite', 'psWaitBitsWorkRead',
                  'psWaitBitsRomWrite', 'psWaitBitsRomRead', 'psWaitProgrammerZstate', 'psWaitFreq');

implementation
uses
System.DateUtils;




procedure TMilRev8AbstractTestThread.FreqMeterEvent(Sender: TObject; Event: TGUID;
  Params: Pointer);
begin
  if IsEqualGUID(Event, C_ReadDone) then
  begin
    if fProcessState = psWaitFreq then
    begin
      LogPositonsFreqs;
      SetEvent(fEvents.OprerationSucc);
    end;
  end else if IsEqualGUID(Event, C_ReadError) then
  begin
    fBoardLogger.Log(lWarning, 'Freq read error answer');
    if fProcessState = psWaitFreq then
    begin
      LogPositonsFreqs;
      SetEvent(fEvents.OprerationSucc);
    end;
  end;
end;

function TMilRev8AbstractTestThread.FreqStartAndResume: Boolean;
var
vTimeOut : Cardinal;
vStarted : Boolean;
vAttemptCount : Byte;
begin
  Result := False;
  vStarted := false;
  vAttemptCount := 1;
  while not vStarted  do
  begin
    if Terminated then
      break;
    vStarted := fFreqStarter.StartSeriesMeasure;
    if vStarted then
      vTimeOut := fFreqStarter.CalculatedTimeOut*2;
    if not vStarted then
    begin
      if (vAttemptCount < 3) then
      begin
        fBoardLogger.Log(lWarning, Format('Freq series measure not started from %d attempt, wait 1 sec',[vAttemptCount]));
        if not SleepAndResume(1000) then
          break;
      end else
      begin
        fBoardLogger.Log(lWarning, Format('Freq series measure not started from %d attempt - halt',[vAttemptCount]));
        Break;
      end;
    end else
      Break;
    inc(vAttemptCount);
  end;
  if Terminated then
    exit;
  if not vStarted then
  begin
    fBoardLogger.Log(lWarning, 'Error on start frequency measurer');
    exit;
  end else
  begin
    if (vAttemptCount > 1) then
      fBoardLogger.Log(lWarning, Format('Freq series measure started from %d attempt',[vAttemptCount]))
    else
      fBoardLogger.Log(lEvent, 'Freq series measure started');
  end;

  fProcessState := psWaitFreq;
  case WaitForMultipleObjects(2, @fEvents, False, vTimeOut) of
    WAIT_OBJECT_0:
    begin
      Result := True;
      fBoardLogger.Log(lEvent, 'Freq finished signal recieved');
    end;
    WAIT_OBJECT_0+1:
      fBoardLogger.Log(lTrace, 'Stop signal recieved');
    WAIT_TIMEOUT:
      fBoardLogger.Log(lWarning, 'frequency measurer not answer');
  end;
  fProcessState := psProcessing;
end;

procedure TMilRev8AbstractTestThread.PosControllerEvent(Sender: TObject; Event: TGUID;
  Params: Pointer);
begin
  if IsEqualGUID(Event, C_OnVoltageSended) then
  begin
    if fProcessState = psWaitSendVoltage then
      SetEvent(fEvents.OprerationSucc);
  end else
  if IsEqualGUID(Event, C_OnEnabledSended) then
  begin
    if fProcessState = psWaitSendState then
      SetEvent(fEvents.OprerationSucc);
  end;
end;

function TMilRev8AbstractTestThread.PositionClass: TProcessedPositionClass;
begin
  Result := TProcessedPosition;
end;

procedure TMilRev8AbstractTestThread.Stop;
begin
  inherited Stop;
  if (fProcessState in [psSleep, psWaitSendVoltage, psWaitSendState,
                  psWaitBitsWorkWrite, psWaitBitsWorkRead,
                  psWaitBitsRomWrite, psWaitBitsRomRead, psWaitProgrammerZstate, psWaitFreq]) then
    SetEvent(fEvents.Stop);
end;

procedure TMilRev8AbstractTestThread.OnProgrammerEvent(Sender: TObject; Event: TGUID;
  Params: Pointer);
var
vLogger : IdmLogger;
begin
  if IsEqualGUID(Event, C_OnProgrammerOperationSucc) then
  begin
    if (fProcessState in [psWaitBitsWorkWrite, psWaitBitsWorkRead,
                        psWaitBitsRomWrite, psWaitBitsRomRead, psWaitProgrammerZstate]) then
      SetEvent(fEvents.OprerationSucc);
  end else
  if IsEqualGUID(Event, C_OnProgrammerOperationTimeOut) then
  begin
    if (fProcessState in [psWaitBitsWorkWrite, psWaitBitsWorkRead,
                    psWaitBitsRomWrite, psWaitBitsRomRead, psWaitProgrammerZstate])  then
    begin
      if supports(fBoard, IdmLogger, vLogger) then
        vLogger.Log(lError, 'programmer timeout');
      vLogger := nil;
      SetEvent(fEvents.Stop);
    end;
  end;
end;

function TMilRev8AbstractTestThread.GetActiveCount:Word;
var
vIdx : Word;
begin
  Result := 0;
  vIdx := 0;
  while vIdx < fProcessPositions.Count do
  begin
    if fProcessPositions[vIdx].Position.Active then
      Inc(Result);
    Inc(vIdx);
  end;
end;

function TMilRev8AbstractTestThread.GetSuccCount:Word;
var
vIdx : Word;
begin
  Result := 0;
  vIdx := 0;
  while  vIdx < fProcessPositions.Count do
  begin
    if fProcessPositions[vIdx].Succ then
      Inc(Result);
    Inc(vIdx);
  end;
end;

function TMilRev8AbstractTestThread.RemoveInactive:Boolean;
var
vIdx : integer;
begin
  result := False;
  vIdx := fProcessPositions.Count-1;
  while vIdx > -1 do
  begin
    if not fProcessPositions[vIdx].Position.Active then
    begin
      fProcessPositions.Delete(vIdx);
      result := True;
    end;
    Dec(vIdx);
  end;
end;

procedure TMilRev8AbstractTestThread.DropUnSuccPositions;
begin
  if Terminated then
    exit;
  if fProcessPositions.Count < 1 then
    Exit;
  fBoardLogger.Log(lTrace, 'start "drop unsucc positions"');
  Synchronize(
  procedure
  var
  vIdx : word;
  vDuInListIdx : Integer;
  begin
    fPositionsList.BeginUpdate;
    try
      for vIdx := fProcessPositions.Count-1 downto 0 do
      begin
        if (fProcessPositions[vIdx].Position.Active)
        and  (not fProcessPositions[vIdx].Succ) then
        begin
          fBoardLogger.Log(lWarning,
                Format('Position %d droped',
                [fProcessPositions[vIdx].Position.BoardPos]));
          vDuInListIdx := fPositionsList.IndexOf(fProcessPositions[vIdx].Position.BoardPos);
          fProcessPositions.Delete(vIdx);
          if vDuInListIdx <> -1 then
            fPositionsList.Delete(vDuInListIdx);
        end;
      end;
    finally
      fPositionsList.EndUpdate;
    end;
  end);
  fBoardLogger.Log(lTrace, 'finish "drop unsucc positions"');
end;

procedure TMilRev8AbstractTestThread.DropInProtectPositions;
begin
  if Terminated then
    exit;
  if fProcessPositions.Count < 1 then
    Exit;
  fBoardLogger.Log(lTrace, 'start "drop in protect positions"');
  Synchronize(
  procedure
  var
  vIdx : word;
  vDuInListIdx : Integer;
  vLogItem : TPosLogItem;
  vDB : TEmbSQLDataBase;
  begin
    fPositionsList.BeginUpdate;
    try
      for vIdx := fProcessPositions.Count-1 downto 0 do
      begin
        if (fProcessPositions[vIdx].Controller.ProtectState) then
        begin
          fBoardLogger.Log(lWarning,
                Format('Position %d in protect. Droped',
                [fProcessPositions[vIdx].Position.BoardPos]));
          vLogItem.DT := now;
          vLogItem.Step := 'Позиция исключена из процесса';
          vLogItem.Result := '-';
          vLogItem.Comment := 'Срабатывание защиты';
          fProcessPositions[vIdx].DbExt.FatalError := true;
          vDB := fProcessPositions[vIdx].DbExt.CreateConnection;
          try
            vLogItem.Save(vDB);
            fProcessPositions[vIdx].DbExt.UpdatePosData(vDB, []);
          finally
            if vDB.TransactionActive then
              vDB.RollBack;
            vDB.DBClose;
            FreeAndNil(vDB);
            vDuInListIdx := fPositionsList.IndexOf(fProcessPositions[vIdx].Position.BoardPos);
            fProcessPositions.Delete(vIdx);
            if vDuInListIdx <> -1 then
              fPositionsList.Delete(vDuInListIdx);
          end;
        end;
      end;
    finally
      fPositionsList.EndUpdate;
    end;
  end);
  fBoardLogger.Log(lTrace, 'finish "drop in protect positions"');
end;

function TMilRev8AbstractTestThread.SendPositionsVoltagesAndResume(
  ATimeOut: Cardinal): Boolean;
var
vPositionsVoltages : pPositionsVoltages;
vFirstPosition : TProcessedPosition;
begin
  Result := False;
  if Terminated then
    exit;
  if (fProcessPositions.Count < 1) then
    Exit;
  vFirstPosition := fProcessPositions[0];
  New(vPositionsVoltages);
  try
    fPositionsController.GetVoltageValues(vPositionsVoltages);
    if (SecondsBetween(Now, vPositionsVoltages.TimeStamp) < 2)
    and (SameValue(vPositionsVoltages.VDD,
                vFirstPosition.CardProcessOptions.VDD,
                vFirstPosition.CardProcessOptions.ErrorLimits.VDDTolerance))
    and (SameValue(vPositionsVoltages.VC,
                vFirstPosition.CardProcessOptions.VC,
                vFirstPosition.CardProcessOptions.ErrorLimits.VCTolerance))
    and (SameValue(vPositionsVoltages.VAnalog, 0, vFirstPosition.CardProcessOptions.ErrorLimits.VCTolerance))
    and (SameValue(vPositionsVoltages.VProg, 0, vFirstPosition.CardProcessOptions.ErrorLimits.VDDTolerance)) then
    begin
      Result := True;
      fBoardLogger.Log(lTrace, Format('Actual voltages: VDD:%8.5fv., VC:%8.5fv. no needed send',
        [vPositionsVoltages.VDD,
        vPositionsVoltages.VC]));
    end else
    begin
      vPositionsVoltages.VDD := vFirstPosition.CardProcessOptions.VDD;
      vPositionsVoltages.VC := vFirstPosition.CardProcessOptions.VC;
      vPositionsVoltages.VAnalog := 0;
      vPositionsVoltages.VProg := 0;
      fBoardLogger.Log(lTrace, Format('Send voltages. VDD:%8.5fv., VC:%8.5fv.',
        [vPositionsVoltages.VDD,
        vPositionsVoltages.VC]));
      fProcessState := psWaitSendVoltage;
      fPositionsController.SetVoltageValues(vPositionsVoltages);
      case WaitForMultipleObjects(2, @fEvents, False, ATimeOut) of
        WAIT_OBJECT_0:
        begin
          Result := True;
          fBoardLogger.Log(lEvent, 'Send voltages OK signal recieved');
        end;
        WAIT_OBJECT_0+1:
          fBoardLogger.Log(lTrace, 'Stop signal recieved');
        WAIT_TIMEOUT:
          fBoardLogger.Log(lWarning, 'Timeout during sending voltages');
      end;
    end;
  finally
    Dispose(vPositionsVoltages);
    fProcessState := psProcessing;
    if Terminated then
      result := false;
  end;
end;

function TMilRev8AbstractTestThread.SleepAndResume(ASleepTime : Cardinal):Boolean;
begin
  if Terminated then
  begin
    result := false;
    exit;
  end;
  fBoardLogger.Log(lEvent, Format('Delay %d mSec',[ASleepTime]));
  fProcessState := psSleep;
  Result := (WaitForSingleObject(fEvents.Stop, ASleepTime) = WAIT_TIMEOUT);
  if not result then
    fBoardLogger.Log(lTrace, 'Stop signal recieved');
  fProcessState := psProcessing;
  if Terminated then
    result := false;
end;

function TMilRev8AbstractTestThread.SendDisablesActive(ATimeOut: Cardinal): boolean;
var
vApIdx,
vAffectedCount : Word;
vPosIdx : integer;
vEnabled, vLogging : Boolean;
vEnablePosition : IPosition;
vLogStr : string;
begin
  Result := false;
  if Terminated then
    exit;
  fProcessState := psWaitSendState;
  try
    vLogging := fBoardLogger.LevelEnabled(lTrace);
    fBoardLogger.Log(lTrace, 'start send disables active positions');

    if vLogging then
      vLogStr := '';
    vAffectedCount := 0;
    for vApIdx := 1 to fPositionsList.MaxCount do
    begin
      vEnabled := fPositionsController.Enabled[vApIdx-1];
      vPosIdx := fPositionsList.IndexOf(vApIdx);
      if (vPosIdx <> -1) then
      begin
        if (fPositionsList.QueryItem(vPosIdx, IPosition, vEnablePosition)) then
        begin
          if vEnablePosition.Active and vEnabled then
          begin
            vEnabled := false;
            fPositionsController.Enabled[vApIdx-1] := vEnabled;
            Inc(vAffectedCount);
            if vLogging then
              vLogStr := Format('%s (pos.%d:%s) ',[vLogStr, vApIdx, BoolToStr(vEnabled, true)]);
          end;
        end;
        vEnablePosition := nil;
      end;
    end;
    if vLogging then
    begin
      if vAffectedCount > 0 then
        fBoardLogger.Log(lTrace, Format('Send disables %s',[vLogStr]))
      else
        fBoardLogger.Log(lTrace, 'No positions for send disable flags');
    end;
    if (vAffectedCount < 1) and (not Terminated) then
    begin
      Result := True;
      Exit;
    end else if Terminated then
      exit;
    fBoardLogger.Log(lTrace, 'Wait send disables signal');
    case WaitForMultipleObjects(2, @fEvents, False, ATimeOut) of
      WAIT_OBJECT_0:
      begin
        Result := True;
        fBoardLogger.Log(lEvent, 'Send disables OK signal recieved');
      end;
      WAIT_OBJECT_0+1:
        fBoardLogger.Log(lTrace, 'Stop signal recieved');
      WAIT_TIMEOUT:
        fBoardLogger.Log(lWarning, 'Timeout during sending disables states');
    end;
  finally
    fProcessState := psProcessing;
    if Terminated then
      result := false;
  end;
end;

function TMilRev8AbstractTestThread.SendEnables(ATimeOut: Cardinal) : boolean;
var
vApIdx,
vAffectedCount : Word;
vPosIdx : integer;
vEnabled, vLogging : Boolean;
vEnablePosition : IPosition;
vLogStr : string;
begin
  Result := false;
  if Terminated then
    exit;
  fProcessState := psWaitSendState;
  try
    vLogging := fBoardLogger.LevelEnabled(lTrace);
    fBoardLogger.Log(lTrace, 'start "send enables"');

    if vLogging then
      vLogStr := '';
    vAffectedCount := 0;
    for vApIdx := 1 to fPositionsList.MaxCount do
    begin
      vEnabled := fPositionsController.Enabled[vApIdx-1];
      vPosIdx := fPositionsList.IndexOf(vApIdx);
      if (vPosIdx <> -1) then
      begin
        if (fPositionsList.QueryItem(vPosIdx, IPosition, vEnablePosition)) then
        begin
          if vEnablePosition.Active and (not vEnabled) then
            vEnabled := True;
          vEnablePosition := nil;
        end else if vEnabled then
          vEnabled := False;
      end else if vEnabled then
        vEnabled := False;
      if vLogging and (fPositionsController.Enabled[vApIdx-1] <> vEnabled) then
        vLogStr := Format('%s (pos.%d:%s) ',[vLogStr, vApIdx, BoolToStr(vEnabled, true)]);
      if (fPositionsController.Enabled[vApIdx-1] <> vEnabled) then
      begin
        fPositionsController.Enabled[vApIdx-1] := vEnabled;
        Inc(vAffectedCount);
      end;
    end;
    if vLogging then
    begin
      if vAffectedCount > 0 then
        fBoardLogger.Log(lTrace, Format('Send enables %s',[vLogStr]))
      else
        fBoardLogger.Log(lTrace, 'No positions for send enables');
    end;
    if (vAffectedCount < 1) and (not Terminated) then
    begin
      Result := True;
      Exit;
    end else if Terminated then
      exit;
    fBoardLogger.Log(lTrace, 'Wait send enables signal');
    case WaitForMultipleObjects(2, @fEvents, False, ATimeOut) of
      WAIT_OBJECT_0:
      begin
        Result := True;
        fBoardLogger.Log(lEvent, 'Send enables OK signal recieved');
      end;
      WAIT_OBJECT_0+1:
        fBoardLogger.Log(lTrace, 'Stop signal recieved');
      WAIT_TIMEOUT:
        fBoardLogger.Log(lWarning, 'Timeout during sending enable states');
    end;
  finally
    fProcessState := psProcessing;
    if Terminated then
      result := false;
  end;
end;

function TMilRev8AbstractTestThread.CheckPowerVoltages(var SuccCount : Word):boolean;
var
vApIdx, vApTryCount : Word;
vProcessPosition : TProcessedPosition;
vIdx : word;
vLogging : Boolean;
vLogStr : string;
begin
  Result := False;
  if fProcessPositions.Count < 1 then
    Exit;
  vLogging := fBoardLogger.LevelEnabled(lTrace);
  for vApTryCount := 0 to 5 do
  begin
    vApIdx := 0;
    if Terminated then
      break;
    while vApIdx < fProcessPositions.Count do
    begin
      vProcessPosition := fProcessPositions[vApIdx];
      if vProcessPosition.Position.Active then
      begin
        vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);
        if vLogging then
          fBoardLogger.Log(lTrace, Format('try №%d check position %d. VDD:%8.5fv., '+
          'VC:%8.5fv., VA:%8.5fv., enabled state:%s, protect state:%s, '+
          'VDD timestamp:%s, VC timestamp:%s, VA timestamp:%s, '+
          'enabled state timestamp:%s, protect state timestamp:%s',
          [vApTryCount,
          vProcessPosition.Position.BoardPos,
          fPositionVoltages.VDD,
          fPositionVoltages.VC,
          fPositionVoltages.Analog,
          BoolToStr(vProcessPosition.Controller.Enabled, true),
          BoolToStr(vProcessPosition.Controller.ProtectState, true),
          FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',fPositionVoltages.VDDTimeStamp, fThreadFormatSettings),
          FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',fPositionVoltages.VCTimeStamp, fThreadFormatSettings),
          FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',fPositionVoltages.AnalogTimeStamp, fThreadFormatSettings),
          FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',vProcessPosition.Controller.EnabledTimeStamp, fThreadFormatSettings),
          FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',vProcessPosition.Controller.ProtectTimeStamp, fThreadFormatSettings)
          ]));
        vProcessPosition.Succ :=
          SameValue(fPositionVoltages.VDD,
                  vProcessPosition.CardProcessOptions.VDD,
                  vProcessPosition.CardProcessOptions.ErrorLimits.VDDTolerance)
          and SameValue(fPositionVoltages.VC,
                  vProcessPosition.CardProcessOptions.VC,
                  vProcessPosition.CardProcessOptions.ErrorLimits.VCTolerance);
        if not vProcessPosition.Succ  then
          fBoardLogger.Log(lWarning, Format('check position %d error. card vdd:%8.5fv, vc:%8.5fv., '+
          'VDD Tolerance:%8.5fv., VCTolerance:%8.5fv',
          [vProcessPosition.Position.BoardPos,
          vProcessPosition.CardProcessOptions.VDD,
          vProcessPosition.CardProcessOptions.VC,
          vProcessPosition.CardProcessOptions.ErrorLimits.VDDTolerance,
          vProcessPosition.CardProcessOptions.ErrorLimits.VCTolerance]));
      end;
      Inc(vApIdx);
    end;
    SuccCount := GetSuccCount;
    if SuccCount < fProcessPositions.Count then
    begin
      if vLogging then
        vLogStr := 'try reactivate position';
      vIdx := 0;
      while vIdx < fProcessPositions.Count do
      begin
        vProcessPosition := fProcessPositions[vIdx];
        if (vProcessPosition.Position.Active)
          and (not vProcessPosition.Controller.Enabled) then
        begin
          vProcessPosition.Controller.Enabled := True;
          if vLogging then
            vLogStr := Format('%s (pos.%d send enabled:true) ',[vLogStr, vProcessPosition.Position.BoardPos]);
        end;
        inc(vIdx);
      end;
      if vLogging then
        fBoardLogger.Log(lTrace, vLogStr);
      if not SleepAndResume(3000) then
        Break;
    end else
    begin
      Result := True;
      break;
    end;
  end;
  if not Result then
    fBoardLogger.Log(lWarning, Format('check vortage error. succ count:%d, total count %d',[SuccCount, fProcessPositions.Count]))
  else
   fBoardLogger.Log(lEvent, 'check voltages ok');
  if Terminated then
    result := false;
end;

procedure TMilRev8AbstractTestThread.FillProcessPositions;
function GetProcessOptions(AID : Integer; var oOptions : TStp8milProcessDBOptions):boolean;
var
vPoIdx : Word;
begin
  vPoIdx := 0;
  result := false;
  while (vPoIdx < fProcessOptions.Count) do
  begin
    result := fProcessOptions[vPoIdx].CardID = AID;
    if result then
      break ;
    Inc(vPoIdx);
  end;
  if result then
  begin
    oOptions := fProcessOptions[vPoIdx];
    fBoardLogger.Log(lTrace, (Format('Process options id %d from cache',[AID])));
  end else
  try
    oOptions := TStp8milProcessDBOptions.Create;
    if oOptions.Stp8_Load(fDMAccess, fConnection, AID) then
    begin
      fProcessOptions.Add(oOptions);
      Result := True;
      fBoardLogger.Log(lTrace, (Format('Process options id %d loaded',[AID])));
    end else
    begin
      FreeAndNil(oOptions);
      result := false;
      fBoardLogger.Log(lWarning, (Format('Error on load process options id %d',[AID])));
    end;
  except
    on E : Exception do
      fBoardLogger.Log(lException, E.Message);
  end;
end;
var
vIdx : Word;
vProcessPosition : TProcessedPosition;
vInListPosition : IPosition;
begin
  fProcessPositions.Clear;
  fBoardLogger.Log(lEvent, 'Prepare positions');
  for vIdx := 0 to fPositionsList.Count-1 do
  begin
    if Supports(fPositionsList[vIdx], IPosition, vInListPosition) then
    begin
      vInListPosition.Active := False;
      vInListPosition := nil;
      vProcessPosition := PositionClass.Create(self);
      if ((not supports(fPositionsList[vIdx], IPosition, vProcessPosition.Position))
      or (not supports(fPositionsList[vIdx], IExtentions, vProcessPosition.Extentions))
      or (not vProcessPosition.Extentions.Find(TMilandrRev8Registers, vProcessPosition.Chip))
      or (not vProcessPosition.Extentions.Find(IPositionVoltages, vProcessPosition.Voltages))
      or (not vProcessPosition.Extentions.Find(IdmPositionController, vProcessPosition.Controller))
      or (not vProcessPosition.Extentions.Find(IdmPositionFreqResult, vProcessPosition.Freq))
      or (not vProcessPosition.Extentions.Find(TStp8milDbExtention, vProcessPosition.DbExt))
      or (not GetProcessOptions(vProcessPosition.DbExt.RCardID, vProcessPosition.CardProcessOptions))
      or (not PreparePosition(vProcessPosition))) then
      begin
        FreeAndNil(vProcessPosition);
        Continue;
      end;
      vProcessPosition.Succ := False;
      vProcessPosition.Position.Active := True;
      fProcessPositions.Add(vProcessPosition);
      if Terminated then
        break;
      fBoardLogger.Log(lEvent, Format('Prepared position %d',[vProcessPosition.Position.BoardPos]));
    end;
  end;
  if not Terminated then
    Synchronize(UpdatePositionsState);
end;

procedure TMilRev8AbstractTestThread.UpdatePositionsState;
begin
  fPositionsList.BeginUpdate;
  fPositionsList.EndUpdate;
end;

procedure TMilRev8AbstractTestThread.LogPositonsFreqs;
var
vIdx, vErrors : word;
vLogStr : string;
vProcessPosition : TProcessedPosition;
begin
  if (GetActiveCount < 1) then
    Exit;
  vLogStr := Format('Freq: series total\succ count:%d\%d.  Results:',
    [fFreqController.GetSeriesCount, fFreqController.GetSeriesSuccesCount]);
  vIdx := 0;
  vErrors := 0;
  if ((fFreqController.GetSeriesSuccesCount < 1)
  or (fFreqController.GetSeriesCount <> fFreqController.GetSeriesSuccesCount)) then
     inc(vErrors);
  while vIdx < fProcessPositions.Count do
  begin
    vProcessPosition := fProcessPositions[vIdx];
    if (vProcessPosition.Position.Active) then
    begin
      if vProcessPosition.Freq.FreqResult(fFreqResult) then
        vLogStr := Format('%s (pos.%d Freq:%n Hz; Disp %f Hz; Disp %f ppm; timestamp:%s) ',
        [vLogStr, vProcessPosition.Position.BoardPos,
        fFreqResult.Freq,
        fFreqResult.DispersionHz,
        fFreqResult.DispersionPpm,
        FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',fFreqResult.TimeStamp, fThreadFormatSettings)])
      else
      begin
        vLogStr := Format('%s (pos.%d read error)',[vLogStr, vProcessPosition.Position.BoardPos]);
        inc(vErrors);
      end;
    end;
    inc(vIdx);
  end;
  if vErrors > 0 then
    fBoardLogger.Log(lWarning, vLogStr)
  else
    fBoardLogger.Log(lInfo, vLogStr);
end;

procedure TMilRev8AbstractTestThread.LogPositonsVoltagesStates;
var
vIdx : word;
vLogging : Boolean;
vLogStr : string;
vProcessPosition : TProcessedPosition;
begin
  if (GetActiveCount < 1) then
    Exit;
  vLogging := fBoardLogger.LevelEnabled(lInfo);
  if vLogging then
    vLogStr := 'Positions voltages:'
  else
    Exit;
  vIdx := 0;
  while vIdx < fProcessPositions.Count do
  begin
    vProcessPosition := fProcessPositions[vIdx];
    if (vProcessPosition.Position.Active) then
    begin
      vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);
      vLogStr := Format('%s (pos %d. VDD:%8.5fv., '+
        'VC:%8.5fv., VA:%8.5fv., enabled state:%s, protect state:%s, '+
        'VDD timestamp:%s, VC timestamp:%s, VA timestamp:%s, '+
        'enabled state timestamp:%s, protect state timestamp:%s) ',
        [vLogStr,
        vProcessPosition.Position.BoardPos,
        fPositionVoltages.VDD,
        fPositionVoltages.VC,
        fPositionVoltages.Analog,
        BoolToStr(vProcessPosition.Controller.Enabled, true),
        BoolToStr(vProcessPosition.Controller.ProtectState, true),
        FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',fPositionVoltages.VDDTimeStamp, fThreadFormatSettings),
        FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',fPositionVoltages.VCTimeStamp, fThreadFormatSettings),
        FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',fPositionVoltages.AnalogTimeStamp, fThreadFormatSettings),
        FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',vProcessPosition.Controller.EnabledTimeStamp, fThreadFormatSettings),
        FormatDateTime('dd.mm.yyyy h:nn:ss:zzz',vProcessPosition.Controller.ProtectTimeStamp, fThreadFormatSettings)
        ]);
    end;
    inc(vIdx);
  end;
  fBoardLogger.Log(lInfo, vLogStr);
end;

procedure TMilRev8AbstractTestThread.LogPositonsRegisters;
var
vIdx : word;
vBitIdx : Byte;
vLogging : Boolean;
vLogStr, vRegStr : string;
vProcessPosition : TProcessedPosition;
begin
  if (GetActiveCount < 1) then
    Exit;
  vLogging := fBoardLogger.LevelEnabled(lInfo);
  if vLogging then
    vLogStr := 'Registers contains:'
  else
    Exit;
  vIdx := 0;
  while vIdx < fProcessPositions.Count do
  begin
    vProcessPosition := fProcessPositions[vIdx];
    if (vProcessPosition.Position.Active) then
    begin
      for vBitIdx := vProcessPosition.Chip.BitMinIndex to vProcessPosition.Chip.BitMaxIndex do
      begin
        if vBitIdx = vProcessPosition.Chip.BitMinIndex then
          vRegStr := Format('%s=%d',[vProcessPosition.Chip.BitName(vBitIdx), vProcessPosition.Chip.BitValue[vBitIdx]])
        else
         vRegStr := Format('%s, %s=%d',[vRegStr, vProcessPosition.Chip.BitName(vBitIdx), vProcessPosition.Chip.BitValue[vBitIdx]])
      end;
      vLogStr := Format('%s (pos.%d: %s) ',[vLogStr, vProcessPosition.Position.BoardPos, vRegStr]);
    end;
    inc(vIdx);
  end;
  fBoardLogger.Log(lInfo, vLogStr);
end;

function TMilRev8AbstractTestThread.ProgrammerReadRomAndResume(
  ATimeOut: Cardinal): Boolean;
var
  vProgrammerError : TProgrammerError;
  vAttemptCount : Byte;
begin
  result := False;
  fProcessState := psWaitBitsRomRead;
  vAttemptCount := 1;
  vProgrammerError := peAnyError;
  try
    while vProgrammerError <> peNoErrors do
    begin
      if Terminated then
        break;
      vProgrammerError := fPositionsProgrammer.StartOperation(4, false);
      if vProgrammerError <> peNoErrors then
      begin
        if vProgrammerError = peErrorOnStart then
        begin
          if (vAttemptCount < 3) then
          begin
            fBoardLogger.Log(lWarning, Format('Read rom not started whith error "%s" from %d attempt, wait 1 sec',
                          [C_programmerErrors[vProgrammerError], vAttemptCount]));
            if not SleepAndResume(1000) then
              break;
          end else
          begin
            fBoardLogger.Log(lWarning, Format('Read rom not started whith error "%s" from %d attempt - halt',
            [C_programmerErrors[vProgrammerError], vAttemptCount]));
            Break;
          end;
        end else
          Break;
      end else
        Break;
      inc(vAttemptCount);
    end;
    if Terminated then
      exit;
    if vProgrammerError <> peNoErrors then
      fBoardLogger.Log(lError, Format('Programmer error "%s" during read rom - halt',[C_programmerErrors[vProgrammerError]]))
    else
      fBoardLogger.Log(lEvent, 'Read rom started');
    Result := vProgrammerError = peNoErrors;
    if not Result then
      Exit;
    fBoardLogger.Log(lTrace, 'Wait read rom signal');
    case WaitForMultipleObjects(2, @fEvents, False, ATimeOut) of
      WAIT_OBJECT_0:
      begin
        Result := True;
        fBoardLogger.Log(lEvent, 'Read rom finished signal recieved');
        LogPositonsRegisters;
      end;
      WAIT_OBJECT_0+1:
        fBoardLogger.Log(lTrace, 'Stop signal recieved');
      WAIT_TIMEOUT:
        fBoardLogger.Log(lWarning, 'Programmer not answer during bits read rom');
    end;
  finally
    fProcessState := psProcessing;
  end;
end;

function TMilRev8AbstractTestThread.ProgrammerReadWorkAndResume(
  ATimeOut: Cardinal): Boolean;
var
  vProgrammerError : TProgrammerError;
  vAttemptCount : Byte;
begin
  result := False;
  fProcessState := psWaitBitsWorkRead;
  vAttemptCount := 1;
  vProgrammerError := peAnyError;
  try
    while vProgrammerError <> peNoErrors do
    begin
      if Terminated then
        break;
      vProgrammerError := fPositionsProgrammer.StartOperation(3, false);
      if vProgrammerError <> peNoErrors then
      begin
        if vProgrammerError = peErrorOnStart then
        begin
          if (vAttemptCount < 3) then
          begin
            fBoardLogger.Log(lWarning, Format('Read work not started whith error "%s" from %d attempt, wait 1 sec',
                          [C_programmerErrors[vProgrammerError], vAttemptCount]));
            if not SleepAndResume(1000) then
              break;
          end else
          begin
            fBoardLogger.Log(lWarning, Format('Read work not started whith error "%s" from %d attempt - halt',
            [C_programmerErrors[vProgrammerError], vAttemptCount]));
            Break;
          end;
        end else
          Break;
      end else
        Break;
      inc(vAttemptCount);
    end;

    if Terminated then
      exit;
    if vProgrammerError <> peNoErrors then
      fBoardLogger.Log(lError, Format('Programmer error "%s" during read work - halt',[C_programmerErrors[vProgrammerError]]))
    else
      fBoardLogger.Log(lEvent, 'Read work started');
    Result := vProgrammerError = peNoErrors;
    if not Result then
      Exit;
    fBoardLogger.Log(lTrace, 'Wait read work signal');

    case WaitForMultipleObjects(2, @fEvents, False, ATimeOut) of
      WAIT_OBJECT_0:
      begin
        Result := True;
        fBoardLogger.Log(lEvent, 'Read work finished signal recieved');
        LogPositonsRegisters;
      end;
      WAIT_OBJECT_0+1:
        fBoardLogger.Log(lTrace, 'Stop signal recieved');
      WAIT_TIMEOUT:
        fBoardLogger.Log(lWarning, 'Programmer not answer during bits read works');
    end;
  finally
    fProcessState := psProcessing;
  end;
end;

function TMilRev8AbstractTestThread.ProgrammerWriteRomAndResume(
  ATimeOut: Cardinal): Boolean;
var
  vProgrammerError : TProgrammerError;
  vAttemptCount : Byte;
begin
  result := False;
  LogPositonsRegisters;
  fProcessState := psWaitBitsRomWrite;
    vAttemptCount := 1;
    vProgrammerError := peAnyError;
  try
    while vProgrammerError <> peNoErrors do
    begin
      if Terminated then
        break;
      vProgrammerError := fPositionsProgrammer.StartOperation(2, true);
      if vProgrammerError <> peNoErrors then
      begin
        if vProgrammerError = peErrorOnStart then
        begin
          if (vAttemptCount < 3) then
          begin
            fBoardLogger.Log(lWarning, Format('Write rom not started whith error "%s" from %d attempt, wait 1 sec',
                          [C_programmerErrors[vProgrammerError], vAttemptCount]));
            if not SleepAndResume(1000) then
              break;
          end else
          begin
            fBoardLogger.Log(lWarning, Format('Write rom not started whith error "%s" from %d attempt - halt',
            [C_programmerErrors[vProgrammerError], vAttemptCount]));
            Break;
          end;
        end else
          Break;
      end else
        Break;
      inc(vAttemptCount);
    end;

    if Terminated then
      exit;
    if vProgrammerError <> peNoErrors then
      fBoardLogger.Log(lError, Format('Programmer error "%s" during write rom - halt',[C_programmerErrors[vProgrammerError]]))
    else
      fBoardLogger.Log(lEvent, 'Write rom started');
    Result := vProgrammerError = peNoErrors;
    if not Result then
      Exit;
    fBoardLogger.Log(lTrace, 'Wait write rom signal');
    case WaitForMultipleObjects(2, @fEvents, False, ATimeOut) of
      WAIT_OBJECT_0:
      begin
        Result := True;
        fBoardLogger.Log(lEvent, 'write rom finished signal recieved');
      end;
      WAIT_OBJECT_0+1:
        fBoardLogger.Log(lTrace, 'Stop signal recieved');
      WAIT_TIMEOUT:
        fBoardLogger.Log(lWarning, 'Programmer not answer during bits read');
    end;
  finally
    fProcessState := psProcessing;
  end;
end;

function TMilRev8AbstractTestThread.ProgrammerWriteWorkAndResume(
  ATimeOut: Cardinal): Boolean;
var
  vProgrammerError : TProgrammerError;
  vAttemptCount : Byte;
begin
  result := False;
  LogPositonsRegisters;
  fProcessState := psWaitBitsWorkWrite;
  vAttemptCount := 1;
  vProgrammerError := peAnyError;
  try
    while vProgrammerError <> peNoErrors do
    begin
      if Terminated then
        break;
      vProgrammerError := fPositionsProgrammer.StartOperation(1, true);
      if vProgrammerError <> peNoErrors then
      begin
        if vProgrammerError = peErrorOnStart then
        begin
          if (vAttemptCount < 3) then
          begin
            fBoardLogger.Log(lWarning, Format('Write work not started whith error "%s" from %d attempt, wait 1 sec',
                          [C_programmerErrors[vProgrammerError], vAttemptCount]));
            if not SleepAndResume(1000) then
              break;
          end else
          begin
            fBoardLogger.Log(lWarning, Format('Write work not started whith error "%s" from %d attempt - halt',
                  [C_programmerErrors[vProgrammerError], vAttemptCount]));
            Break;
          end;
        end else
          Break;
      end else
        Break;
      inc(vAttemptCount);
    end;
    if Terminated then
      exit;
    if vProgrammerError <> peNoErrors then
      fBoardLogger.Log(lError, Format('Programmer error "%s" during write work - halt',[C_programmerErrors[vProgrammerError]]))
    else
      fBoardLogger.Log(lEvent, 'Write work started');
    Result := vProgrammerError = peNoErrors;
    if not Result then
      Exit;
    fBoardLogger.Log(lTrace, 'Wait write work signal');
    case WaitForMultipleObjects(2, @fEvents, False, ATimeOut) of
      WAIT_OBJECT_0:
      begin
        Result := True;
        fBoardLogger.Log(lEvent, 'Write work finished signal recieved');
      end;
      WAIT_OBJECT_0+1:
        fBoardLogger.Log(lTrace, 'Stop signal recieved');
      WAIT_TIMEOUT:
        fBoardLogger.Log(lWarning, 'Programmer not answer during bits write');
    end;
  finally
    fProcessState := psProcessing;
  end;
end;

function TMilRev8AbstractTestThread.ProgrammerZState(ATimeOut: Cardinal): Boolean;
var
  vProgrammerError : TProgrammerError;
  vAttemptCount : Byte;
begin
  result := False;
  LogPositonsRegisters;
  fProcessState := psWaitProgrammerZstate;
  vAttemptCount := 1;
  vProgrammerError := peAnyError;
  try
    while (vProgrammerError <> peNoErrors) do
    begin
      if Terminated then
        break;
      vProgrammerError := fPositionsProgrammer.StartOperation(0, false);
      if vProgrammerError <> peNoErrors then
      begin
        if vProgrammerError = peErrorOnStart then
        begin
          if (vAttemptCount < 3) then
          begin
            fBoardLogger.Log(lWarning, Format('Set Z-State not started whith error "%s" from %d attempt, wait 1 sec',
                          [C_programmerErrors[vProgrammerError], vAttemptCount]));
            if not SleepAndResume(1000) then
              break;
          end else
          begin
            fBoardLogger.Log(lWarning, Format('Set Z-State not started whith error "%s" from %d attempt - halt',
                  [C_programmerErrors[vProgrammerError], vAttemptCount]));
            Break;
          end;
        end else
          Break;
      end else
        Break;
      inc(vAttemptCount);
    end;

    if Terminated then
      exit;
    if vProgrammerError <> peNoErrors then
      fBoardLogger.Log(lError, Format('Programmer error "%s" during setting Z-State - halt',[C_programmerErrors[vProgrammerError]]))
    else
      fBoardLogger.Log(lEvent, 'Set Z-State started');
    Result := vProgrammerError = peNoErrors;
    if not Result then
      Exit;
    fBoardLogger.Log(lTrace, 'Wait Set Z-State signal');

    case WaitForMultipleObjects(2, @fEvents, False, ATimeOut) of
      WAIT_OBJECT_0:
      begin
        Result := True;
        fBoardLogger.Log(lEvent, 'Setting Z-State finished signal recieved');
      end;
      WAIT_OBJECT_0+1:
        fBoardLogger.Log(lTrace, 'Stop signal recieved');
      WAIT_TIMEOUT:
        fBoardLogger.Log(lWarning, 'Programmer not answer during setting Z-State');
    end;
  finally
    fProcessState := psProcessing;
  end;
end;

procedure TMilRev8AbstractTestThread.ReInitPositions;
var
vIdx, vAffectedCount : word;
vProcessPosition : TProcessedPosition;
vInRahgeOptions: TInRangeProcOptionSet;
begin
  fProcessState := psPrepare;
  FillProcessPositions;
  if Terminated then
    exit;
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.Voltage) then
    Exit;
  vIdx := 0;
  vAffectedCount := 0;
  while (vIdx < ProcessPositions.Count) and (not Terminated) do
  begin
    vProcessPosition := ProcessPositions[vIdx];
    if vProcessPosition.DbExt.ItersCount > 0 then
      vInRahgeOptions := vProcessPosition.DbExt.LastIter.NextIterOptions.ControlOptions.InRangeOptions[
                                              vProcessPosition.DbExt.LastIter.NextIterOptions.ControlOptions.MeasureMode].s
    else
      vInRahgeOptions := vProcessPosition.DbExt.StartOptions.ControlOptions.InRangeOptions[
                                              vProcessPosition.DbExt.StartOptions.ControlOptions.MeasureMode].s;
    vProcessPosition.Position.Active := (rpFreqFinal in vInRahgeOptions);
    if vProcessPosition.Position.Active then
      Inc(vAffectedCount);
    inc(vIdx);
  end;
  if Terminated then
    Exit;
  if vAffectedCount > 0 then
  begin
    Synchronize(UpdatePositionsState);
    if not ProgrammerZState(5000) then
      Exit;
    if not SendDisablesActive(5000) then
      Exit;
    if not SleepAndResume(fTimings.AfterEnable) then
      Exit;
    if not SendEnables(3000) then
      Exit;
  end;
  vIdx := 0;
  vAffectedCount := 0;
  while (vIdx < ProcessPositions.Count) and (not Terminated) do
  begin
    vProcessPosition := ProcessPositions[vIdx];
    if vProcessPosition.DbExt.ItersCount > 0 then
      vInRahgeOptions := vProcessPosition.DbExt.LastIter.NextIterOptions.ControlOptions.InRangeOptions[
                                              vProcessPosition.DbExt.LastIter.NextIterOptions.ControlOptions.MeasureMode].s
    else
      vInRahgeOptions := vProcessPosition.DbExt.StartOptions.ControlOptions.InRangeOptions[
                                              vProcessPosition.DbExt.StartOptions.ControlOptions.MeasureMode].s;
    vProcessPosition.Position.Active := not (rpFreqFinal in vInRahgeOptions);
    if vProcessPosition.Position.Active then
    begin
      vProcessPosition.Chip.Assign(vProcessPosition.DbExt.InitRegisters);
      vProcessPosition.Chip.Registers.TEST := 0;
      Inc(vAffectedCount);
    end;
    inc(vIdx);
  end;
  if Terminated then
    Exit;
  if vAffectedCount > 0 then
  begin
    Synchronize(UpdatePositionsState);
    if not ProgrammerWriteWorkAndResume(5000) then
      Exit;
  end;
  vIdx := 0;
  while (vIdx < ProcessPositions.Count) and (not Terminated) do
  begin
    vProcessPosition := ProcessPositions[vIdx];
    vProcessPosition.Position.Active := False;
    inc(vIdx);
  end;
  if Terminated then
    Exit;
  Synchronize(UpdatePositionsState);
end;

function TMilRev8AbstractTestThread.IndexOfProcessedPosition(ANum : Byte):integer;
begin
  result := 0;
  while result < fProcessPositions.Count do
  begin
    if (fProcessPositions[result].Position.BoardPos = ANum) then
      Break;
    inc(result);
  end;
  if (result >= fProcessPositions.Count) then
   Result := -1;
end;

procedure TMilRev8AbstractTestThread.SetActiveProcessedPositions;
var
vmPIdx, vAffectedCount : Word;
vProcPosIdx : Integer;
vInListPosition : IPosition;
vLogging : Boolean;
vLogStr : string;
begin
  if fPositionsList.Count < 1 then
    Exit;
  fBoardLogger.Log(lTrace, 'Start SetActiveProcessedPositions');

  vLogging := fBoardLogger.LevelEnabled(lTrace);
  if vLogging then
    vLogStr := 'Positions set active: ';
  vmPIdx := 0;
  vAffectedCount := 0;
  while vmPIdx < fPositionsList.Count do
  begin
    if (fPositionsList.QueryItem(vmPIdx, IPosition, vInListPosition)) then
    begin
      vProcPosIdx := IndexOfProcessedPosition(vInListPosition.BoardPos);
      if vProcPosIdx = -1 then
      begin
        if vInListPosition.Active then
        begin
          vInListPosition.Active := False;
          Inc(vAffectedCount);
          if vLogging then
             vLogStr := Format('%s(%d:%s) ',
          [vLogStr, vInListPosition.BoardPos, BoolToStr(vInListPosition.Active, true)]);
        end;
      end else
      begin
        if not vInListPosition.Active then
        begin
          vInListPosition.Active := true;
          Inc(vAffectedCount);
          if vLogging then
             vLogStr := Format('%s(%d:%s) ',
          [vLogStr, vInListPosition.BoardPos, BoolToStr(vInListPosition.Active, true)]);
        end;
      end;
    end;
    vInListPosition := nil;
    if Terminated then
      break;
    inc(vmPIdx);
  end;
  if Terminated then
    exit;
  if vAffectedCount > 0 then
    Synchronize(UpdatePositionsState);
  if vLogging then
    fBoardLogger.Log(lTrace, vLogStr);
  fBoardLogger.Log(lTrace, 'Finish SetActiveProcessedPositions');
end;


type
TLinkedEvent = (lePosController, leFreqStarter, leProgrammer);
TLinkedEvents = set of TLinkedEvent;

procedure TMilRev8AbstractTestThread.Execute;
var
vEB : IEventBus;
vLinkedEvents : TLinkedEvents;
begin
  {$IFDEF DEBUG}
    TThread.NameThreadForDebugging('Milandr rev.8 measure thread');
  {$ENDIF}
  if ((not supports(fMainProc, IDMAccess, fDMAccess))
  or (not Supports(fMainProc, IChamberProcess, fChamberProcess))
  or (not supports(fBoard, IAbstractBoard, fAbstractBoard))
  or (not supports(fBoard, IdmLogger, fBoardLogger))
  or (not fAbstractBoard.QueryPositionListInterface(IPositionsList, fPositionsList))
  or (not fAbstractBoard.QueryPositionListInterface(IExtentions, fPosListExtentions))
  or (not fPosListExtentions.Find(IdmPositionsController, fPositionsController))
  or (not fPosListExtentions.Find(IdmFreqStarter, fFreqStarter))
  or (not fPosListExtentions.Find(IdmFreqController, fFreqController))
  or (not fPosListExtentions.Find(IdmProgrammer, fPositionsProgrammer))
  or (not fPositionsProgrammer.ProgrammerExists(MilandrRev8.Consts.C_GUID))) then
  begin
    fDMAccess := nil;
    fChamberProcess := nil;
    fPositionsController := nil;
    fFreqStarter := nil;
    fFreqController := nil;
    fPositionsProgrammer := nil;
    fPosListExtentions := nil;
    fPositionsList := nil;
    fBoardLogger := nil;
    fAbstractBoard := nil;
    Exit;
  end;
  fConnection := fDMAccess.CreateConnection(nil);
  fTimings.Read;
  fThreadFormatSettings := TFormatSettings.Create(GetThreadLocale);
  fEvents.OprerationSucc := CreateEvent(nil, False, False, nil);
  fEvents.Stop := CreateEvent(nil, False, False, nil);
  fProcessPositions := TObjectList<TProcessedPosition>.Create;
  fProcessOptions := TObjectList<TStp8milProcessDBOptions>.Create;
  fBoardLogger.Log(lInfo, Format('Timings: '+
                  'Before voltage measure:%d mSec; '+
                  'Before freq measure:%d mSec; '+
                  'After send enables:%d mSec; ',
                  [fTimings.Voltage, fTimings.Freq, fTimings.AfterEnable]));
  fBoardLogger.Log(lEvent, Format('Starting "%s" stp method', [self.Method.ModuleName]));
  New(fPositionVoltages);
  New(fFreqResult);
  fProcessState := psPrepare;
  try
    fConnection.Connected := True;
    vLinkedEvents := [];
    try
      if not Supports(fPositionsController, IEventBus, vEB) then
      begin
        fBoardLogger.Log(lWarning, 'Error during board positions controller events linking');
        Exit;
      end;
      vEB.Add(PosControllerEvent);
      Include(vLinkedEvents, lePosController);
      vEB := nil;
      if not Supports(fFreqStarter, IEventBus, vEB) then
      begin
        fBoardLogger.Log(lWarning, 'Error during board freq controller events linking');
        Exit;
      end;
      vEB.Add(FreqMeterEvent);
      Include(vLinkedEvents, leFreqStarter);
      vEB := nil;
      if not Supports(fPositionsProgrammer, IEventBus, vEB) then
      begin
        fBoardLogger.Log(lWarning, 'Error during board programmer events linking');
        Exit;
      end;
      vEB.Add(OnProgrammerEvent);
      Include(vLinkedEvents, leProgrammer);
      vEB := nil;

      FillProcessPositions;
      if Terminated then
        exit;
      if fProcessPositions.Count < 1 then
      begin
        fBoardLogger.Log(lWarning, 'No positions for processing. Halt');
        Exit;
      end;
      if not IsEqualGUID(fPositionsProgrammer.ActiveProgrammer, MilandrRev8.Consts.C_GUID) then
      begin
        fPositionsProgrammer.ActiveProgrammer := MilandrRev8.Consts.C_GUID;
        if not SleepAndResume(1500) then
          Exit;
        if not IsEqualGUID(fPositionsProgrammer.ActiveProgrammer, MilandrRev8.Consts.C_GUID) then
        begin
          fBoardLogger.Log(lWarning, Format('"%s" can not started. '+
                              'Programmer for MIlandr rev.8 not supported or not switched properly',
                              [self.Method.ModuleName]));
          Exit;
        end;
      end;
      try
        if not SendPositionsVoltagesAndResume(5000) then
          Exit;
        if not SendEnables(3000) then
          Exit;
        DropInProtectPositions;
        fProcessState := psProcessing;
        fBoardLogger.Log(lEvent, Format('"%s" started', [self.Method.ModuleName]));
        StartMain;
        if not Terminated then
          ReInitPositions;
      finally
        ProcessPositions.Clear;
      end;
    finally
      vEB := nil;
      if (lePosController in vLinkedEvents)
      and Supports(fPositionsController, IEventBus, vEB) then
        vEB.Remove(PosControllerEvent);
      vEB := nil;
      if (leFreqStarter in vLinkedEvents)
      and Supports(fFreqStarter, IEventBus, vEB) then
        vEB.Remove(FreqMeterEvent);
      vEB := nil;
      if (leProgrammer in vLinkedEvents)
      and Supports(fPositionsProgrammer, IEventBus, vEB) then
        vEB.Remove(OnProgrammerEvent);
      vEB := nil;
      fDMAccess.CloseConnection(fConnection);
    end;
  finally
    fBoardLogger.Log(lEvent, Format('"%s" finished. Exec result:%d',
                              [self.Method.ModuleName, fExecResult]));
    fProcessPositions.Clear;
    fProcessOptions.Clear;
    FreeAndNil(fConnection);
    fDMAccess := nil;
    fChamberProcess := nil;
    fPositionsController := nil;
    fFreqStarter := nil;
    fFreqController := nil;
    fPositionsProgrammer := nil;
    fPosListExtentions := nil;
    fPositionsList := nil;
    fBoardLogger := nil;
    fAbstractBoard := nil;
    FreeAndNil(fProcessPositions);
    FreeAndNil(fProcessOptions);
    Dispose(fPositionVoltages);
    Dispose(fFreqResult);
    CloseHandle(fEvents.OprerationSucc);
    CloseHandle(fEvents.Stop);
  end;
end;


{ TProcessedPosition }

constructor TProcessedPosition.Create(const ATestThread : TMilRev8AbstractTestThread);
begin
  inherited Create;
  TestThread := ATestThread;
  Position := nil;
  Extentions := nil;
  Chip := nil;
  DbExt := nil;
  Freq := nil;
  Succ := false;
end;

destructor TProcessedPosition.Destroy;
begin
  TestThread := nil;
  Voltages := nil;
  Controller := nil;
  Chip := nil;
  DbExt := nil;
  Freq := nil;
  Extentions := nil;
  Position := nil;
  CardProcessOptions := nil;
  inherited Destroy;
end;

{ TProcessorDelays }

procedure TProcessorTimings.Read;
var
vVoltage,
vFreq,
vAfterEnable : Integer;
begin
  if ((not TMilandrRev8Registers.ReadConstant('MeasProcessorSettings','DelayBeforeVoltageMeasure',vVoltage))
  or (vVoltage < 1)) then
    Voltage := 5000
  else
    Voltage := vVoltage * 1000;
  if ((not TMilandrRev8Registers.ReadConstant('MeasProcessorSettings','DelayBeforeFreqMeasure',vFreq))
  or (vFreq < 1)) then
    Freq := 5000
  else
    Freq := vFreq * 1000;
  if ((not TMilandrRev8Registers.ReadConstant('MeasProcessorSettings','DelayAfterEnable',vAfterEnable))
  or (vAfterEnable < 1)) then
    AfterEnable := 5000
  else
    AfterEnable := vAfterEnable * 1000;
end;

end.
