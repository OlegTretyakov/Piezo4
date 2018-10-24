unit AbstractMainProcess;
interface
  uses
    System.Classes, Winapi.Windows, Winapi.Messages, System.SysUtils, System.IniFiles,
    VdpDataModule, Vodopad.EventList, AbstractMainProcessInterface, Vodopad.Timer,
    EventBusInterface, CustomChildProcess, LoggerInterface;
  type
   TgpHwInitTestState = (HwtStart,
                          HwtChamberSearch, HwtChamberTestFinish,
                          HwtFinish);

  TConfigController = class(TComponent,
                            IConfigController)
   public
    procedure Load(const AType: string; ADest: TMemoryStream);overload; stdcall;
    procedure Load(const AType: string; ADest: TStrings);overload; stdcall;
    procedure Load(const AType: string; ADest: TMemIniFile);overload; stdcall;
    function Save(const AType: string; ASource: TStream): boolean; overload; stdcall;
    function Save(const AType: string; ASource: TStrings): boolean; overload; stdcall;
    function Save(const AType: string; ASource: TMemIniFile): boolean; overload; stdcall;
    function Delete(const AType: string):boolean; stdcall;
  end;
   TAbstractMainProcess = class (TComponent,
                                IMainProcess,
                                IChamberPortGetter,
                                IEventBus,
                                IMainProcessState,
                                IAbstractProcessLogger
                                {$IFDEF DEBUG}
                                ,IMainProcessDebug
                                {$ENDIF})
    strict private
     fLoggerFunct : pLoggerFunct;
     gpLogger : ILogger;
     fConfigController : TConfigController;
     fChamberLibHandle,
     fBoardProcessLibHandle : HMODULE;
     fDelayTimer : TvdTimer;
     fDataModule : TVdpDM;
     fChamberProcess,
     fBoardProcess : TCustomChildProcess;
     fHWState : TgpHwInitTestState;
     fHwTestResult : THwTestResults;
     FWindowHandle: HWND;
     fEventSubscribers : TCustomObjEventList;
     procedure DoHardwareTest;
    private
     procedure ConnectToDatabase;
     procedure MainExceptionHandler(Sender: TObject; E: Exception);
    protected
     fProcessWorked: Boolean;
     property DataModule : TVdpDM read fDataModule;
     procedure CallOnDBConnected; virtual;
     procedure DoProcAfter(ASeconds : cardinal; ACallBack : TNotifyEvent);
     {IAbstractProcessLoggerController}  
     procedure AchLogs(AOwner : TComponent);stdcall;
     function ShowLogFrm(AOwner : TComponent):TComponent; stdcall;
     procedure ShowModalLogFrm(AOwner : TComponent);stdcall;
     {IChamberPortGetter}
     function IChamberPortGetter.Port = GetChamberPort; 
     function GetChamberPort: Byte; stdcall;
     {IMainProcessState}
     function GetWorked : Boolean;stdcall;
     { IInterface }
     function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
     {IMainProcess}
     function GetHwTestResult: THwTestResults; stdcall;
     {IEventBus}
     procedure IEventBus.Add = EventMethodAdd;
     procedure IEventBus.Remove = EventMethodRemove;
     procedure EventMethodAdd(const AMethod : TCustomObjEvent); stdcall;
     procedure EventMethodRemove(const AMethod: TCustomObjEvent); stdcall;
     procedure BoardProcessEvent(ASender: TObject;
                                  Event : TGUID;
                                  Params : Pointer); virtual;
     procedure ChamberProcessEvent(Sender : TObject;
                                  Event : TGUID;
                                  Params : Pointer);virtual;
     procedure MessageHandler(var message : TMessage); virtual;
     procedure FullStop; virtual; abstract;
     procedure SendResumeHardwareTest;
    public
     constructor Create(AOwner : TComponent; ALoggerFunct : pLoggerFunct=nil); reintroduce; virtual;
     constructor CreateConfigMode(AOwner : TComponent);
     destructor Destroy; override;
     {IMainProcess}
     procedure PostMessage(Msg : Cardinal; WParam, LParam : Integer); stdcall;
     procedure Log(ALogLevel : TLogInfo; const AMessage : string);stdcall;
     procedure DoInfoMessage(const AMessage : string; ALevel: byte=0);stdcall;
     procedure SendStartHardwareTest;stdcall;
     property ProcessWorked: Boolean read fProcessWorked;
     property HwInitTestState : TgpHwInitTestState read fHWState;
     property HwTestResult : THwTestResults read GetHwTestResult;
     property EventSubscribers : TCustomObjEventList read fEventSubscribers;
   end;

TMainProcessClass = class of TAbstractMainProcess;

const
  Do_DB_Connect = 1024 + 220;
  Hardware_Test = Do_DB_Connect + 1;
  C_StartProcess = Hardware_Test+1;
  C_RestartProcess = C_StartProcess+1;
  C_UserStopProcess = C_RestartProcess+1;
  Env_Test = C_UserStopProcess +1;
  HW_TestOK = Env_Test + 1;
  HW_TestFail = HW_TestOK + 1;
  Env_TestOK  =  HW_TestFail + 1;
  Env_TestFail =  Env_TestOK + 1;
  gProcess =  Env_TestFail + 1;
  Pos_Update_Msg = gProcess + 1;

implementation
uses
Data.DB,
vcl.Forms,
VdpDMAccess,
ChamberInterfaces,
BoardProcessInterface,
Vodopad.Math;

type
  TPackageLoad = procedure;
  TPackageUnload = procedure;
  TChildProcessClassFunct = function: TChildProcessClass; stdcall;

function CreateMainProcess(AOwner : TComponent; AProcClass : Pointer; LoggerFunct : pLoggerFunct):TObject;stdcall;
begin
  result := TMainProcessClass(AProcClass).Create(AOwner, LoggerFunct);
end; exports CreateMainProcess;

function InitLogger(LoggerFunct : pLoggerFunct; out Obj):boolean;
var
vConfig : pLogClassConfig;
begin
  result := False;
  if not assigned(LoggerFunct) then
    Exit;
  New(vConfig);
  try
    vConfig.ConfigGUID := StringToGUID('{84D8838A-FE26-4310-88B8-5D3FEFC56BAC}');
    vConfig.EnabledLevels := [lInfo, lEvent, lDebug, lWarning, lError,
                             lException, lExceptionOS];//, lSQL];
    vConfig.DefaultLevelsView := [lInfo, lEvent, lWarning];
    vConfig.FileExtension := 'gplog';
    vConfig.ArhiveExtension := 'gplog7z';
    vConfig.ModuleName := 'Главный процесс';
    try
      Result := LoggerFunct.AddLogger(vConfig, Obj);
    except
      Result := false;
    end;
  finally
    Dispose(vConfig);
  end;
end;

function GetChamberClass(AMainProcess : TObject; out AClass:TChildProcessClass):THandle;
var
  vPackageLoad: TPackageLoad;
  vCC : IConfigController;
  vBuildFile : TMemIniFile;
  vLibFileName : TFileName;
  func:  function: TChildProcessClass; stdcall;
begin
  Result := 0;
  if not Supports(AMainProcess, IConfigController, vCC) then
  begin
    vCC := nil;
    Exit;
  end;
  vBuildFile := TMemIniFile.Create(''{, TEncoding.UTF8});
  try
    vCC.Load('Hardware', vBuildFile);
    try
      if vBuildFile.ValueExists('Chamber','ClassPlugin') then
      begin
        vLibFileName := ExtractFilePath(ParamStr(0))+vBuildFile.ReadString('Chamber','ClassPlugin','');
        if not FileExists(vLibFileName) then
          Exit;
        Result := SafeLoadLibrary(vLibFileName,
          SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
        if Result = 0 then
          Exit;
        @func := GetProcAddress(Result, 'ChamberProcessClass');
        if not Assigned(func) then
        begin
          FreeLibrary(Result);
          Result := 0;
          Exit;
        end;
        @vPackageLoad := GetProcAddress(Result, 'Initialize'); //Do not localize
        if Assigned(vPackageLoad) then
          vPackageLoad;
        AClass := func;
      end;
    except
      if Result<>0 then
      begin
        FreeLibrary(Result);
        Result := 0;
      end;
    end;
  finally
    FreeAndNil(vBuildFile);
    vCC := nil;
  end;
end;

function GetBoardProcessClass(AMainProcess : TObject; out AClass:TChildProcessClass):THandle;
var
vPackageLoad: TPackageLoad;
vCC : IConfigController;
vBuildFile : TMemIniFile;
vLibFileName : TFileName;
func:  TChildProcessClassFunct;
begin
  Result := 0;
  if not Supports(AMainProcess, IConfigController, vCC) then
  begin
    vCC := nil;
    Exit;
  end;
  vBuildFile := TMemIniFile.Create(''{, TEncoding.UTF8});
  try
    vCC.Load('Software', vBuildFile);
    try
      if vBuildFile.ValueExists('BuildConfiguration','BoardProcessClassPlugin') then
      begin
        vLibFileName := ExtractFilePath(ParamStr(0))+vBuildFile.ReadString('BuildConfiguration','BoardProcessClassPlugin','');
        if not FileExists(vLibFileName) then
          Exit;
        Result := SafeLoadLibrary(vLibFileName,
          SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
        if Result = 0 then
          Exit;
        @func := GetProcAddress(Result, 'BoardProcessClass');
        if not Assigned(func) then
        begin
          FreeLibrary(Result);
          Result := 0;
          Exit;
        end;
        @vPackageLoad := GetProcAddress(Result, 'Initialize'); //Do not localize
        if Assigned(vPackageLoad) then
          vPackageLoad;
        AClass := func;
      end;
    except
      if Result<>0 then
      begin
        FreeLibrary(Result);
        Result := 0;
      end;
    end;
  finally
    FreeAndNil(vBuildFile);
    vCC := nil;
  end;
end;

procedure SetThreadExecutionState(ESFlags: DWORD);
  stdcall; external kernel32 name 'SetThreadExecutionState';

const
  ES_SYSTEM_REQUIRED = $00000001;
  ES_CONTINUOUS = $80000000;

constructor TAbstractMainProcess.Create(AOwner : TComponent; ALoggerFunct : pLoggerFunct);
begin
  gpLogger := nil;
  fLoggerFunct := ALoggerFunct;
  inherited Create(AOwner);
  fChamberProcess := nil;
  fBoardProcess := nil;
  fDataModule := TVdpDM.Create(self);
  fDelayTimer := TvdTimer.Create(self);
  fDelayTimer.OnTimer := nil;
  fDelayTimer.Enabled := False;
  if not InitLogger(fLoggerFunct, gpLogger) then
     gpLogger := nil
  else
  begin
    fDataModule.SetLogger(gpLogger);
    Application.OnException := MainExceptionHandler;
  end;
  fConfigController := TConfigController.Create(self);
  fProcessWorked := false;
  fEventSubscribers := TCustomObjEventList.Create;

  fHWState := HwtStart;

  FWindowHandle := System.Classes.AllocateHWND(MessageHandler);
  PostMessage(Do_DB_Connect, 0, 0);
  SetThreadExecutionState(ES_SYSTEM_REQUIRED or ES_CONTINUOUS); 
  Log(lEvent, Format('Main process started. main thread: %d',[MainThreadId]));
end;

constructor TAbstractMainProcess.CreateConfigMode(AOwner: TComponent);
begin
  SetThreadExecutionState(ES_SYSTEM_REQUIRED or ES_CONTINUOUS);
  inherited Create(AOwner);
  fDelayTimer := TvdTimer.Create(self);
  fDelayTimer.OnTimer := nil;
  fDelayTimer.Enabled := False;
  FWindowHandle := 0;
  fChamberProcess := nil;
  fBoardProcess := nil;
  fChamberLibHandle := 0;
  fBoardProcessLibHandle := 0;
  fConfigController := TConfigController.Create(self);
  fDataModule := TVdpDM.Create(self);
  fProcessWorked := false;
  fHWState := HwtStart;
end;

destructor TAbstractMainProcess.Destroy;
procedure FreeLoadedLibrary(AModule: HMODULE);
var
vFinalizeProc: TPackageUnload;
begin
  @vFinalizeProc := GetProcAddress(AModule, 'Finalize'); //Do not localize
  if Assigned(vFinalizeProc) then
    vFinalizeProc;
  FreeLibrary(AModule);
end;
var
vEventBus : IEventBus;
begin  
  SetThreadExecutionState(ES_CONTINUOUS);
  if Supports(fBoardProcess, IEventBus, vEventBus) then
    vEventBus.Remove(BoardProcessEvent);
  vEventBus := nil;
  if Supports(fChamberProcess, IEventBus, vEventBus) then
     vEventBus.Remove(ChamberProcessEvent);
  vEventBus := nil;
  if Assigned(fChamberProcess) then
    FreeAndNil(fChamberProcess);
  if Assigned(fBoardProcess) then
    FreeAndNil(fBoardProcess);
  if Assigned(fEventSubscribers) then
    FreeAndNil(fEventSubscribers);
  if Assigned(fLoggerFunct) then
    fLoggerFunct.SetStoreInterface(nil);
  if Assigned(fConfigController) then
    FreeAndNil(fConfigController);
  if Assigned(fDataModule) then
    FreeAndNil(fDataModule);
  if FWindowHandle > 0 then
    System.Classes.DeallocateHWND(FWindowHandle);
  if fBoardProcessLibHandle<>0 then
  begin
    FreeLoadedLibrary(fBoardProcessLibHandle);
    fBoardProcessLibHandle := 0;
  end;
  if fChamberLibHandle<>0 then
  begin
    FreeLoadedLibrary(fChamberLibHandle);
    fChamberLibHandle := 0;
  end;
  Application.OnException := nil;
  gpLogger := nil;
  inherited Destroy;
end;

procedure TAbstractMainProcess.MessageHandler(var message: TMessage);
begin     
  case message.Msg of
    Do_DB_Connect :
    begin
      DoInfoMessage('Попытка подключения к базе данных.');
      ConnectToDatabase;
    end;
    Hardware_Test:DoHardwareTest;
    HW_TestOK:fEventSubscribers.Execute(Self, C_OnHWTestOK, nil);
    HW_TestFail:
    begin
      if (fProcessWorked) then
      begin
        DoInfoMessage('Остановка по ошибке теста узлов системы');
        FullStop;
      end;
      fEventSubscribers.Execute(Self, C_OnHWTestFail, nil);
    end;
  end;
end;

procedure TAbstractMainProcess.ConnectToDatabase;
begin
  if fDataModule.Connect then
  begin
    DoInfoMessage('Подключение к базе данных выполнено');
    CallOnDBConnected;
  end else
    DoInfoMessage('Подключение к базе данных не выполнено',1);
end;

procedure TAbstractMainProcess.CallOnDBConnected;
var
vEventBus : IEventBus;
vChamberClass,
vBoardProcessClass : TChildProcessClass;

procedure CreateChamberProcess;
begin
  try
    Log(lDebug, 'Load chamber process');
    fChamberLibHandle := GetChamberClass(Self, vChamberClass);
    if fChamberLibHandle <> 0 then
    begin
      Log(lDebug, 'Chamber process loaded');
      fChamberProcess := vChamberClass.Create(self, fLoggerFunct);
      Log(lDebug, 'Chamber process started');
    end;
  except on e : exception do
    begin
      Log(lWarning, 'Chamber process starting fail');
      if Assigned(gpLogger) then
        gpLogger.Log(lException, e);
      if Assigned(fLoggerFunct) then
        fLoggerFunct.FlushAll;
    end;
  end;
end;

procedure CreateBoardProcess;
begin
  try
    Log(lDebug, 'Load board process');
    fBoardProcessLibHandle := GetBoardProcessClass(Self, vBoardProcessClass);
    if fBoardProcessLibHandle <> 0 then
    begin
      Log(lDebug, 'Board process loaded');
      fBoardProcess := vBoardProcessClass.Create(self, fLoggerFunct);
      Log(lDebug, 'Board process started');
    end;
  except on e : exception do
    begin
      Log(lWarning, 'Board process starting fail');
      if Assigned(gpLogger) then
        gpLogger.Log(lException, e);
      if Assigned(fLoggerFunct) then
        fLoggerFunct.FlushAll;
    end;
  end;
end;
var
vSti : ILoggingConfStoreAccess;
begin
  if Assigned(fLoggerFunct)
  and Supports(fDataModule, ILoggingConfStoreAccess, vSti) then
    fLoggerFunct.SetStoreInterface(vSti);
  vSti := nil;
  CreateChamberProcess;
  if Supports(fChamberProcess, IEventBus, vEventBus) then
    vEventBus.Add(ChamberProcessEvent);
  vEventBus := nil;

  CreateBoardProcess;
  if Supports(fBoardProcess, IEventBus, vEventBus) then
    vEventBus.Add(BoardProcessEvent);
  vEventBus := nil;
  fEventSubscribers.Execute(Self, C_OnDBConnected, nil);
end;

procedure TAbstractMainProcess.DoProcAfter(ASeconds: cardinal; ACallBack: TNotifyEvent);
begin
  fDelayTimer.enabled := false;
  fDelayTimer.OnTimer := ACallBack;
  fDelayTimer.Interval := ASeconds * 1000;
  fDelayTimer.enabled := true;
end;

procedure TAbstractMainProcess.MainExceptionHandler(Sender: TObject; E: Exception);
begin
  Log(lException, Format('%s %s',[E.ClassName, E.Message]));
end;

procedure TAbstractMainProcess.SendResumeHardwareTest;
begin
  PostMessage(Hardware_Test, 0, 0);
end;

procedure TAbstractMainProcess.SendStartHardwareTest;
begin
  fHWState := HwtStart;
  SendResumeHardwareTest;
end;

procedure TAbstractMainProcess.DoHardwareTest;
var
vChamberProcess : IChamberProcess;
vChamberConnectionState : IChamberConnectionState;
begin
 case fHWState of
    HwtStart:
    begin
      fHwTestResult := [];
      fHWState := HwtChamberSearch;
      DoInfoMessage('Тестирование аппаратного окружения');
      SendResumeHardwareTest;
    end;
    HwtChamberSearch:
    begin
      DoInfoMessage('Термокамера');
      if Supports(self, IChamberProcess, vChamberProcess)
      and Supports(self, IChamberConnectionState, vChamberConnectionState) then
      begin
         if not vChamberConnectionState.Connected then
         begin
           fHwTestResult := [];
           vChamberProcess.StartChamberModule(GetChamberPort);
         end
         else
         begin
           Include(fHwTestResult, trChamberOK);
           fHWState:= HwtChamberTestFinish;
           SendResumeHardwareTest;
         end;
      end else
      begin
        fHWState:= HwtChamberTestFinish;
        SendResumeHardwareTest;
      end;
      vChamberConnectionState := nil;
      vChamberProcess := nil;
    end;
    HwtChamberTestFinish:
     begin
      if (trChamberOK in fHwTestResult) then
        DoInfoMessage('ОК')
      else
        DoInfoMessage('Ошибка',1);
      fHWState := HwtFinish;
      SendResumeHardwareTest;
    end;
    HwtFinish:
    begin
      if (trChamberOK in fHwTestResult) then
      begin
        PostMessage(HW_TestOK, 0, 0);
        DoInfoMessage('Аппаратное окружение - ОК');
        {$IFDEF LAB_DEBUG}
        DoInfoMessage('Лабораторная версия');
        {$ENDIF}
      end else
      begin
        PostMessage(HW_TestFail, 0, 0);
        DoInfoMessage('Тестирование аппаратного окружения неуспешно', 1);
        DoInfoMessage('Попробуйте выполнить конфигурирование');
      end;
    end;
  end;
end;

procedure TAbstractMainProcess.DoInfoMessage(const AMessage: string; ALevel: byte=0);
var
vMsg : TInfoMessage;
vLogInfo : TLogInfo;
begin
  if Assigned(gpLogger) then
  begin
    if ALevel = 0 then
      vLogInfo := lInfo
    else
      vLogInfo := lWarning;
    gpLogger.Log(vLogInfo, AMessage);
  end;
  
  vMsg.Level := ALevel;
  vMsg.Msg := AMessage;
  fEventSubscribers.Execute(Self, C_OnInfoMessage, @vMsg);
end;

procedure TAbstractMainProcess.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TAbstractMainProcess.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
   fEventSubscribers.Remove(AMethod);
end;

function TAbstractMainProcess.GetChamberPort: Byte;
var
vIni : TMemIniFile;
vCC : IConfigController;
begin
  result := 0;
  if not Supports(Self, IConfigController, vCC) then
  begin
    vCC := nil;
    Exit;
  end;
  vIni := TMemIniFile.Create(''{, TEncoding.UTF8});
  try
    vCC.Load('Hardware', vIni);
    Result := Byte(vIni.ReadInteger('Chamber','Port',0));
  finally
    FreeAndNil(vIni);
    vCC := nil;
  end;
end;

function TAbstractMainProcess.GetHwTestResult: THwTestResults;
begin
  Result := fHwTestResult;
end;

function TAbstractMainProcess.GetWorked: Boolean;
begin
  Result := fProcessWorked;
end;

procedure TAbstractMainProcess.PostMessage(Msg: Cardinal; WParam, LParam: Integer);
begin
  Winapi.Windows.PostMessage(FWindowHandle, Msg, WParam, LParam);
end;

function TAbstractMainProcess.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IsEqualGUID(IID, IEventBus) then
  begin
    if Assigned(fEventSubscribers) then
      Result := inherited QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end else
  if IsEqualGUID(IID, IConfigController) then
  begin
    if Assigned(fConfigController) and (fDataModule.Connected) then
      Result := fConfigController.QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end else
  if IsEqualGUID(IID, IAbstractProcessLogger) then
  begin
    if  Assigned(fLoggerFunct) then
      Result := inherited QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end;   
  Result := inherited QueryInterface(IID, Obj);
  if Result <> S_OK then
  begin
    if Supports(fDataModule, IID, Obj)
    or Supports(fChamberProcess, IID, Obj)
    or Supports(fBoardProcess, IID, Obj)
    or Supports(gpLogger, IID, Obj) then
      result := S_OK;
  end;
end;

procedure TAbstractMainProcess.AchLogs(AOwner: TComponent);
begin
  if Assigned(fLoggerFunct) then
    fLoggerFunct.AchLogs(AOwner);
end;

function TAbstractMainProcess.ShowLogFrm(AOwner : TComponent):TComponent; stdcall;
begin
  if Assigned(fLoggerFunct) then
    Result := fLoggerFunct.ShowLogFrm(AOwner);
end;

procedure TAbstractMainProcess.ShowModalLogFrm(AOwner: TComponent);
begin
  if Assigned(fLoggerFunct) then
    fLoggerFunct.ShowModalLogFrm(AOwner);
end;

procedure TAbstractMainProcess.BoardProcessEvent(ASender: TObject;
                                  Event : TGUID;
                                  Params : Pointer);
var
vBoardProcess : IBoardProcess;
begin 
  if IsEqualGUID(Event, C_SearchStart) then
  begin
    DoInfoMessage('Поиск плат...');
  end else
  if IsEqualGUID(Event, C_SearchComplete) then
  begin
    if Supports(ASender, IBoardProcess, vBoardProcess) then
    begin
      if vBoardProcess.BoardsCount > 0 then
        DoInfoMessage('Платы найдены')
      else
       DoInfoMessage('Платы не найдены',1);
    end;
  end;
  vBoardProcess := nil;
  fEventSubscribers.Execute(ASender, Event, Params);
end;

procedure TAbstractMainProcess.ChamberProcessEvent(Sender: TObject;
  Event: TGUID; Params: Pointer);
var
vChamberConnector : IChamberConnectionState;
vMinutes : Word;
vSeconds : byte;
vChamber : IChamber;
begin
  if IsEqualGUID(Event, C_OnChamber_Detected) then
  begin
    if not Supports(Sender, IChamberConnectionState, vChamberConnector)  then
    begin
      vChamberConnector := nil;
      exit;
    end;
    Log(lEvent, Format('Обнаружена камера. порт COM%d',
    [vChamberConnector.Port]));
    if (fHWState = HwtChamberSearch) then
    begin
      Include(fHwTestResult, trChamberOK);
      fHWState := HwtChamberTestFinish;
      SendResumeHardwareTest;
    end;
    vChamberConnector := nil;
  end else
  if IsEqualGUID(Event, C_OnChamber_TryConnectionFail) then
  begin
    Log(lEvent, 'Камера не найдена');
    if (fHWState = HwtChamberSearch) then
    begin
      fHWState := HwtChamberTestFinish;
      SendResumeHardwareTest;
    end;
  end else
  if IsEqualGUID(Event, C_OnChamber_ConnectionLost) then
  begin
    if not Supports(Sender, IChamberConnectionState, vChamberConnector) then
    begin
      vChamberConnector := nil;
      exit;
    end;
    Log(lWarning, Format('Соединение с камерой на порту COM%d потеряно',
    [vChamberConnector.Port]));
    fHWState := HwtChamberTestFinish;
    Exclude(fHwTestResult, trChamberOK);
    SendResumeHardwareTest;
    vChamberConnector := nil;
  end else
  if IsEqualGUID(Event, C_OnChamber_Disconnected) then
  begin
    if not Supports(Sender, IChamberConnectionState, vChamberConnector) then
    begin
      vChamberConnector := nil;
      exit;
    end;
    Log(lEvent, Format('Камера на порту COM%d отключена',
    [vChamberConnector.Port]));
    vChamberConnector := nil;
  end else
  if IsEqualGUID(Event, C_OnChamber_StabilizedTimeOut) then
  begin
    if not Supports(Sender, IChamber, vChamber) then
    begin
      vChamber := nil;
      exit;
    end;
    SecondToMinSec(vChamber.SecondsTargetTimeOut, vMinutes, vSeconds);
    vChamber := nil;
    DoInfoMessage(Format('Камера не вышла на точку за %d минут', [vMinutes]), 1);
    FullStop;
  end;
end;

procedure TAbstractMainProcess.Log(ALogLevel: TLogInfo; const AMessage: string);
begin
  if (ALogLevel = lInfo) then
    DoInfoMessage(AMessage)
  else if Assigned(gpLogger) then        
    gpLogger.Log(ALogLevel, AMessage);
end;

{ TConfigController }

procedure TConfigController.Load(const AType: string; ADest: TMemoryStream);
var
vDM : IDMAccess;
vReadQR : TvdQuery;
vBlob : TStream;
vMainProc : IMainProcess;
begin
  ADest.Clear;
  if not Supports(Self.owner, IDMAccess, vDM) then
  begin
    vDM := nil;
    Exit;
  end;
  vReadQR := vDM.CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE UPPER(TYPE) = :TYPE');
  try
    vReadQR.Prepare;
    vReadQR.ParamByName('TYPE').AsString := UpperCase(AType);
    vReadQR.Open;
    if vReadQR.RecordCount > 0 then
    begin
      vBlob := vReadQR.CreateBlobStream(vReadQR.FieldByName('FILE'), bmRead);
      try
        ADest.LoadFromStream(vBlob);
      finally
        FreeAndNil(vBlob);
      end;
    end else if Supports(Self.owner, IMainProcess, vMainProc) then
    begin
      vMainProc.Log(lWarning, Format('Requested config "%s" is not contains in database',[AType]));
      vMainProc := nil;
    end;
    vReadQR.Close;
  finally
    FreeAndNil(vReadQR);
    vDM := nil;
  end;
end;

procedure TConfigController.Load(const AType: string;
  ADest: TStrings);
var
vDM : IDMAccess;
vReadQR : TvdQuery;
vMainProc : IMainProcess;
begin
  ADest.Clear;
  if not Supports(Self.owner, IDMAccess, vDM) then
  begin
    vDM := nil;
    Exit;
  end;
  vReadQR := vDM.CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE UPPER(TYPE) = :TYPE');
  try
    vReadQR.Prepare;
    vReadQR.ParamByName('TYPE').AsString := UpperCase(AType);
    vReadQR.Open;
    if vReadQR.RecordCount > 0 then
      ADest.Text := vReadQR.FieldByName('FILE').AsWideString
    else if Supports(Self.owner, IMainProcess, vMainProc) then
    begin
      vMainProc.Log(lWarning, Format('Requested config "%s" is not contains in database',[AType]));
      vMainProc := nil;
    end;
    vReadQR.Close;
  finally
    FreeAndNil(vReadQR);
    vDM := nil;
  end;
end;

procedure TConfigController.Load(const AType : string; ADest : TMemIniFile);
var
vStr : TStringList;
begin
  ADest.Clear;
  vStr := TStringList.Create;
  try
     Load(AType, vStr);
     ADest.SetStrings(vStr);
  finally
    FreeAndNil(vStr);
  end;
end;

function TConfigController.Save(const AType: string; ASource: TStream): boolean;
var
vDM : IDMAccess;
vReadQR,
vWriteQR : TvdQuery;
vUpdate : Boolean;
begin
  result := False;
  if not Supports(Self.owner, IDMAccess, vDM) then
  begin
    vDM := nil;
    Exit;
  end;
  vReadQR := vDM.CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE UPPER(TYPE) = :TYPE');
  try
    vReadQR.Prepare;
    vReadQR.ParamByName('TYPE').AsString := UpperCase(AType);
    vReadQR.Open;
    vUpdate := vReadQR.RecordCount > 0;
    vReadQR.Close;
  finally
    FreeAndNil(vReadQR);
  end;

  ASource.Position := 0;

  if vUpdate then
    vWriteQR := vDM.CreateWriteQuery(nil, 'UPDATE CONFIG SET FILE = :FILE WHERE TYPE = :TYPE')
  else
    vWriteQR := vDM.CreateWriteQuery(nil, 'INSERT INTO CONFIG (TYPE, FILE) VALUES (:TYPE, :FILE)');
  try
    vWriteQR.Prepare;
    try
      vWriteQR.ParamByName('TYPE').AsString := AType;
      vWriteQR.ParamByName('FILE').LoadFromStream(ASource, ftWideMemo);
      vWriteQR.ExecSQL;
      vDM.Commit(vWriteQR);
      Result := True;
    except
      on e : exception do
        vDM.OnException(vWriteQR, e);
    end;
  finally
    FreeAndNil(vWriteQR);
    vDM := nil;
  end;
end;

function TConfigController.Save(const AType: string;
  ASource: TStrings): boolean;
var
vDM : IDMAccess;
vReadQR,
vWriteQR : TvdQuery;
vUpdate : Boolean;
begin
  result := False;
  if not Supports(Self.owner, IDMAccess, vDM) then
  begin
    vDM := nil;
    Exit;
  end;
  vReadQR := vDM.CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE UPPER(TYPE) = :TYPE');
  try
    vReadQR.Prepare;
    vReadQR.ParamByName('TYPE').AsString := UpperCase(AType);
    vReadQR.Open;
    vUpdate := vReadQR.RecordCount > 0;
    vReadQR.Close;
  finally
    FreeAndNil(vReadQR);
  end;

  if vUpdate then
    vWriteQR := vDM.CreateWriteQuery(nil, 'UPDATE CONFIG SET FILE = :FILE WHERE TYPE = :TYPE')
  else
    vWriteQR := vDM.CreateWriteQuery(nil, 'INSERT INTO CONFIG (TYPE, FILE) VALUES (:TYPE, :FILE)');
  try
    vWriteQR.Prepare;
    try
      vWriteQR.ParamByName('TYPE').AsString := AType;
      vWriteQR.ParamByName('FILE').AsWideMemo := ASource.Text;
      vWriteQR.ExecSQL;
      vDM.Commit(vWriteQR);
      Result := True;
    except
      on e : exception do
        vDM.OnException(vWriteQR, e);
    end;
  finally
    FreeAndNil(vWriteQR);
    vDM := nil;
  end;
end;

function TConfigController.Save(const AType : string; ASource : TMemIniFile):boolean;
var
vStr : TStringList;
begin
  vStr := TStringList.Create;
  try
    ASource.GetStrings(vStr);
    result := Save(AType, vStr);
  finally
    FreeAndNil(vStr);
  end;
end;

function TConfigController.Delete(const AType: string):Boolean;
var
vDM : IDMAccess;
vWriteQR : TvdQuery;
begin
  Result := False;
  if not Supports(Self.owner, IDMAccess, vDM) then
  begin
    vDM := nil;
    Exit;
  end;
  vWriteQR := vDM.CreateWriteQuery(nil, 'DELETE FROM CONFIG WHERE UPPER(TYPE) = :TYPE');
  try
    vWriteQR.Prepare;
    try
      vWriteQR.ParamByName('TYPE').AsString := UpperCase(AType);
      vWriteQR.ExecSQL;
      vDM.Commit(vWriteQR);
      Result := True;
    except
      on e : exception do
        vDM.OnException(vWriteQR, e);
    end;
  finally
    FreeAndNil(vWriteQR);
    vDM := nil;
  end;
end;

end.
