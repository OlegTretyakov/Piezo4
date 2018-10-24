unit Compensation_V8;



interface
  uses
  System.Classes, Winapi.Messages, Winapi.Windows,
  System.Contnrs, System.SysUtils, System.IniFiles,
  AbstractMainProcess, AbstractMainProcessInterface,
   StpProcessTypes, LoggerInterface,
  MainProcFormsSupportsInterface,
  ReconstructorFormInterface, CalcLogger;

type

  TgpEnvInitTestState = (EnvStart,
                        EnvStartBoardSearch, EnvBoardTestOk, EnvBoardFail,
                        EnvExecPrepareCalc,
                        EnvProcessStartTests,
                        EnvWaitStartTests,
                        EnvTestOk, EnvTestFail);

  TAutoProcState = (apStart, apStartCheckPrepare, apFindNextPoint, apWaitPreparePoint,
  apWaitPrepareProcess, apCheckProgOnPrepare, apProgStart, apInRangeMeasStarted,
  apWaitInRangePoint, apWaitInRangeProcess, apMeasFinished, apWaitNormalAfterFinish,
  apCheckCalc, apStopOrLoop, apPrepareStop, apStoped);

  
  TProcessState = (wsStartTest, wsPreparePoint, wsInRange, wsMeasureFinished);
  TFormLibDescriptor = class
  strict private
    FWindowHandle: HWND;
    fLibHandle : THandle;
    fFormInstance : TComponent;
    fLibName : string;
    procedure MessageHandler(var message : TMessage);
    function CreateProcessForm(AOwner : TComponent):boolean;
    procedure OnFormDestroy(Sender: TObject);
    procedure FreeFormLibrary;
  public
    constructor Create(const aFile : string; var AFileValid : Boolean);
    destructor Destroy;override;
    function GetFormInstance(AOwner : TComponent; var alreadyExists : boolean):TComponent;
  end;
  TUiLibDestcriptor = class
   strict private
    FWindowHandle: HWND;
    fLibHandle : THandle;
    fInstances : TObjectList;
    fLibFile : string;
    procedure MessageHandler(var message : TMessage);
    procedure OnComponentDestroy(Sender: TObject);
    procedure FreeUiLibrary;
   public
    constructor Create(const aFile : string);
    destructor Destroy;override;
    property LibFile:string  read fLibFile;
    function CreateComponent(const AClassGetterFunctName : Widestring; AOwner : TComponent):TComponent;
  end;
  TUiLibsRegistry = class(TObjectList)
  public
    function GetDestriptor(const aFile : string):TUiLibDestcriptor;
  end;
  TPatternLoader = class(TComponent,
                      IPatternLoader)
  public
    function Load(const AConnection : TObject; const AID : integer; AField : TPatternFiledType; const ADest: TMemoryStream):boolean;overload; stdcall;
    function Load(const AConnection : TObject; const AID : integer; const ADest: TStrings):boolean;overload; stdcall;
    function Load(const AConnection : TObject; const AID : integer; const ADest: TMemIniFile):boolean;overload; stdcall;
  end;
  TReconstructorResult = (crSucc, crUserCanceled, crEmpty);
  TV8CompensProcess = class(TAbstractMainProcess,
                            IMainProcessAppTitle,
                            ILoopedControl,
                            IDBBoards,
                            IStpStatesDescriptor,
                            IMainProcessStartStopController,
                            IMainProcessRestartController,
                            ICreateAboutForm,
                            ICreateCardForm,
                            ICreateCardListForm,
                            ICreatePositionsArrangeForm,
                            ICreateReconstructorForm,
                            ICreateProgammingQueueForm,
                            ICreateUiLibComponent,
                            ICalcLoggerGetter)
   private
    fPatternLoader : TPatternLoader;
    fEnvInitTestState : TgpEnvInitTestState;
    fRestartMode : boolean;
    fStopAfterCalc : boolean;
    fLooped : Boolean;
    fAutoProcState : TAutoProcState;
    fCreateCardForm,
    fCardListForm,
    fPositionsArrangeForm,
    fReconstructorForm,
    fProgammingQueueForm :TFormLibDescriptor;
    fUiLisbsRegistry : TUiLibsRegistry;
    fCalcLogger : ILogger;
    procedure DoEnvTestOK;
    procedure DoEnvTestFail;
    procedure DoEnvTest;
    procedure DoMainProcess;
    procedure ResumeEnvironmentTest; overload;
    procedure ResumeEnvironmentTest(ASecondsDelay : Cardinal); overload;
    procedure ResumeMainProcess; overload;
    procedure ResumeMainProcess(ASecondsDelay : Cardinal); overload;
    procedure CallEnvProcByTimer(Sender : TObject);
    procedure CallMainProcByTimer(Sender : TObject);
    procedure SetProcessState(AState: TProcessState);
    procedure StartProcess;
    procedure RestartProcess;
    procedure FullStopByUser;
    procedure CreateTermoprofile;
    function Reconstruction : TReconstructorResult;
    {ICalcLoggerGetter}
    function ICalcLoggerGetter.Get = CalcLoggerGet;
    function CalcLoggerGet(out obj):Boolean; stdcall;
    {IMainProcessAppTitle}
    function AppTitle : String;stdcall;
    {IDBBoards}
    procedure FillBoardsList(AList : TDBBoards); stdcall;
    function DeleteBoardRecord(ABoardSerial: Word): Boolean; stdcall;
   protected
    procedure CallOnDBConnected; override;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    procedure MessageHandler(var message: TMessage);override;
    procedure BoardProcessEvent(Sender : TObject;
                                Event : TGUID;
                                Params : Pointer); override;
    procedure ChamberProcessEvent(Sender : TObject;
                                  Event : TGUID;
                                  Params : Pointer);override; 
    {IStpStatesDescriptor}
    function StatesCount : Byte;stdcall;
    function StateProgrammName(AState : byte):String;stdcall;
    function StateHumanName(AState : byte):String;stdcall;
    {IShowAboutForm}
    function ICreateAboutForm.Create = AboutForm_Create;
    function AboutForm_Create(var alreadyExists : boolean): TComponent;stdcall;
    {IShowCreateCardForm}
    function ICreateCardForm.Create = CreateCardForm_Create;
    function CreateCardForm_Create(var alreadyExists : boolean): TComponent;stdcall;
    {IShowCardListForm}
    function ICreateCardListForm.Create = CardListForm_Create;
    function CardListForm_Create(var alreadyExists : boolean): TComponent;stdcall;
    {IShowPositionsArrangeForm}
    function ICreatePositionsArrangeForm.Create = PositionsArrangeForm_Create;
    function PositionsArrangeForm_Create(var alreadyExists : boolean): TComponent;stdcall;
    {ICreateReconstructorForm}
    function ICreateReconstructorForm.Create = ReconstructorForm_Create;
    function ReconstructorForm_Create(var alreadyExists : boolean): TComponent;stdcall;
    {IShowProgammingQueueForm}
    function ICreateProgammingQueueForm.Create = ProgammingQueueForm_Create;
    function ProgammingQueueForm_Create(var alreadyExists : boolean): TComponent;stdcall;
    {ICreateUiLibComponent}
    function ICreateUiLibComponent.Create = UiLibComponent_Create;
    function UiLibComponent_Create(const aLibFile : string;
                          const AClassGetterFunctName : Widestring; AOwner : TComponent):TComponent;stdcall;
    {ILoopedControl}
    procedure SetLooped(const Value: boolean); stdcall;
    function GetLooped : Boolean; stdcall;
    {IMainProcessStartStopController}
    procedure IMainProcessStartStopController.Start = UserController_Start;
    procedure IMainProcessStartStopController.Stop = UserController_Stop;
    procedure UserController_Start;stdcall;
    procedure UserController_Stop;stdcall;
    {IMainProcessRestartController}
    procedure IMainProcessRestartController.Restart = UserController_Restart;
    procedure UserController_Restart;stdcall;
    procedure FullStop; override;
   public
    constructor Create(AOwner : TComponent; ALoggerFunct : pLoggerFunct=nil);override;
    destructor Destroy;override;
  end;


implementation

uses
Data.DB,
Vcl.Controls,
System.Math,
System.StrUtils,
Vodopad.CustomIniHelpers,
System.Generics.Collections,
System.Generics.Defaults,
ChamberInterfaces,
AbstractProtocol,
BoardProcessInterface,
AbstractBoardInterface,
PositionListInterface,
ExtentionsListInterface,
Stp8PositionDBExtention,
Vodopad.FloatList,
Vodopad.Math,
VdpDMAccess,
FormsControllerInterface,
Compens_v8About,
Vodopad.Timer;

const
C_StpMethodsProgrammName : array [0..2] of String = (
                          'StartTest',
                          'PreparePoint',
                          'InRange');
C_StpMethodsHumanName: array [0..2] of String = (
                          'Start Test',
                          'Prepare Point',
                          'In Range');




function MainProcessClass : pointer; stdcall;
begin
  Result := Pointer(TV8CompensProcess);
end; exports MainProcessClass;

function InitCalcLogger(LoggerFunct : pLoggerFunct; out Obj):boolean;
var
vConfig : pLogClassConfig;
begin
  result := False;
  if not assigned(LoggerFunct) then
    Exit;
  New(vConfig);
  try
    vConfig.ConfigGUID := StringToGUID('{6111A319-786B-41BA-97CD-FCBC9778239E}');
    vConfig.EnabledLevels := [lInfo, lEvent, lDebug, lTrace, lWarning, lError,
                            {$IFDEF DEBUG}lEnter, lLeave, {$ENDIF}
                            lException, lExceptionOS];
    vConfig.DefaultLevelsView := [lInfo, lEvent, lWarning];
    vConfig.FileExtension := 'cllog';
    vConfig.ArhiveExtension := 'cllog7z';
    vConfig.ModuleName := 'Расчеты';
    try
      Result := LoggerFunct.AddLogger(vConfig, Obj);
    except
      Result := false;
    end;
  finally
    Dispose(vConfig);
  end;
end;

constructor TV8CompensProcess.Create(AOwner : TComponent; ALoggerFunct : pLoggerFunct);
begin
  inherited;
  if not InitCalcLogger(ALoggerFunct, fCalcLogger) then
    fCalcLogger := nil;
  fPatternLoader := TPatternLoader.Create(self);
  fRestartMode := false;
  fStopAfterCalc := false;
  fUiLisbsRegistry := TUiLibsRegistry.Create;
  fEnvInitTestState := EnvStart;
  fAutoProcState := apStoped;
end;

destructor TV8CompensProcess.Destroy;
begin
  if Assigned(fCreateCardForm) then
    FreeAndNil(fCreateCardForm);
  if Assigned(fCardListForm) then
    FreeAndNil(fCardListForm);
  if assigned(fPositionsArrangeForm) then
    FreeAndNil(fPositionsArrangeForm);
  if Assigned(fReconstructorForm) then
    FreeAndNil(fReconstructorForm);
  if Assigned(fProgammingQueueForm) then
    FreeAndNil(fProgammingQueueForm);
  FreeAndNil(fUiLisbsRegistry);
  inherited;
end;

procedure TV8CompensProcess.SetLooped(const Value: boolean);
var
vDoMessage : Boolean;
begin
  vDoMessage := fLooped <> Value;
  fLooped := Value;
  if vDoMessage then
  begin
    if fLooped then       
      DoInfoMessage('Режим цикла включен')
    else
      DoInfoMessage('Режим цикла выключен');
  end;
end;  

function TV8CompensProcess.GetLooped: Boolean;
begin
  Result := fLooped;
end;

procedure TV8CompensProcess.DoEnvTest;
var
vBoardProc : IBoardProcess; 
vStpController : IStpMeasureController;
begin
  Log(lDebug, Format('HwInitTestState = %d',[ord(HwInitTestState)]));
  if (trChamberOK in HwTestResult) then
  begin
    case fEnvInitTestState of
      EnvStart:
      begin
        fEnvInitTestState := EnvStartBoardSearch;
        DoInfoMessage('Старт функционального теста');
        fProcessWorked := true;
        fStopAfterCalc := False;
        EventSubscribers.Execute(self, C_OnProcessStarted, nil);
        ResumeEnvironmentTest;
      end;
      EnvStartBoardSearch:
      begin
        if Supports(Self, IBoardProcess, vBoardProc) then
        begin
          if vBoardProc.BoardsCount > 0 then
            vBoardProc.Clear;
          vBoardProc.StartSearch;
          DoInfoMessage('Поиск измерительных плат...');
        end else
        begin
          DoInfoMessage('Нет доступа к процессу управления платами', 1);
          fEnvInitTestState := EnvTestFail;
          ResumeEnvironmentTest;
        end;
        vBoardProc := nil;
      end;
      EnvBoardTestOk:
      begin
        DoInfoMessage('OK');
        if Supports(Self, IBoardProcess, vBoardProc) then
        begin
          if Supports(vBoardProc, IStpMeasureController, vStpController) then
          begin
            vStpController.InitStpProcess;
            if vBoardProc.PositionsCount > 0 then
            begin
              vBoardProc.SetAllPositionsToState(true);
              if fRestartMode then
              begin
                vStpController.DeleteUnSuccIter;
                fRestartMode := False;
              end;
              fEnvInitTestState := EnvExecPrepareCalc;
              DoInfoMessage('Проверка позиций, подготовленных к расчету');
            end else
            begin
              DoInfoMessage('Список позиций пуст', 1);
              fEnvInitTestState := EnvTestFail;
            end;
          end;
        end else
        begin
          DoInfoMessage('Нет доступа к процессу step-by-step модуля управления платами', 1);
          fEnvInitTestState := EnvTestFail;
        end;
        vStpController := nil;
        vBoardProc := nil;
        ResumeEnvironmentTest;
      end;
      EnvBoardFail:
      begin
        DoInfoMessage('Ошибка поиска измерительных плат!', 1);
        fEnvInitTestState := EnvTestFail;
        ResumeEnvironmentTest;
      end;
      EnvExecPrepareCalc:
      begin
        case Reconstruction of
          crSucc,
          crUserCanceled:
          begin
            fStopAfterCalc := true;
            fEnvInitTestState := EnvTestOk;
            ResumeEnvironmentTest;
          end;
          crEmpty:
          begin
            fEnvInitTestState := EnvProcessStartTests;
            ResumeEnvironmentTest;
          end;
        end;
      end;
      EnvProcessStartTests:
      begin
        if Supports(Self, IBoardProcess, vBoardProc)
        and Supports(Self, IStpMeasureController, vStpController) then
        begin
          vBoardProc.SetAllPositionsToState(true);
          SetProcessState(wsStartTest);
          if vStpController.StpReadyToStart then
          begin 
            fEnvInitTestState := EnvWaitStartTests;
            vStpController.StpStart;
          end else
          begin
            fEnvInitTestState := EnvTestOk;
            ResumeEnvironmentTest;
          end;
        end else
        begin
          fEnvInitTestState := EnvTestFail;
          ResumeEnvironmentTest;
        end;
        vStpController := nil;
        vBoardProc := nil;
      end;
      EnvTestOk:
      begin
        PostMessage(Env_TestOK, 0, 0);
        DoInfoMessage('Функциональный тест пройден');
        try
          EventSubscribers.Execute(self, C_PosUpdate, nil);
        except
        end;
      end;
      EnvTestFail:
      begin
        if Supports(Self, IBoardProcess, vBoardProc) then
        begin
          vBoardProc.Clear;
        end;
        vBoardProc := nil;
        PostMessage(Env_TestFail, 0, 0);
        DoInfoMessage('Функциональный тест не пройден', 1);
        fProcessWorked := false;
        EventSubscribers.Execute(self, C_OnProcessStoped, nil);
        try
          EventSubscribers.Execute(self, C_PosUpdate, nil);
        except
        end;
      end;
    end;
  end; 
end;

procedure TV8CompensProcess.DoEnvTestFail;
var
vStpController : IStpMeasureController;
begin
  EventSubscribers.Execute(self, C_OnEnvTestFail, nil);
  if ProcessWorked then
  begin
    if (fAutoProcState <> apStoped) then
    begin
      if Supports(Self, IStpMeasureController, vStpController) then
        vStpController.StpStop;
      vStpController := nil;
      fAutoProcState := apPrepareStop;
      ResumeMainProcess;
      DoInfoMessage(
      'Процесс термокомпенсации прерван по срабатыванию защиты узлов системы', 1);
    end;
  end;
end;

procedure TV8CompensProcess.DoEnvTestOK;
begin
  EventSubscribers.Execute(self, C_OnEnvTestOK, nil);
  if fStopAfterCalc then
  begin
    fAutoProcState := apPrepareStop;
    DoInfoMessage('Остановка после расчета', 1);
    ResumeMainProcess;
    fStopAfterCalc := false;
    exit;
  end;
  if (fAutoProcState = apStoped) then
    fAutoProcState := apStart;
  ResumeMainProcess;
end;

procedure TV8CompensProcess.DoMainProcess;
var
vChamberProc : IChamberProcess;
vBoardProc: IBoardProcess;
vStpController : IStpMeasureController;
vReconstructorResult : TReconstructorResult;
begin
  case fAutoProcState of
    apStart:
    begin
      if Supports(self, IChamberProcess, vChamberProc)
      and Supports(Self, IStpMeasureController, vStpController) then
      begin
        vChamberProc.ProcessWorked := true;
        fProcessWorked := true;
        {$IFNDEF LAB_DEBUG}
        fAutoProcState := apStartCheckPrepare;
        {$ELSE}
        fAutoProcState := apWaitPrepareProcess;
        {$ENDIF}
        DoInfoMessage('Старт процесса');
      end else
        fAutoProcState := apPrepareStop;
      vChamberProc := nil;
      vStpController := nil;
      ResumeMainProcess;
    end;
    apStartCheckPrepare:
    begin
      SetProcessState(wsPreparePoint);
      if Supports(Self, IStpMeasureController, vStpController) then
      begin
        vStpController.PrepareProgrammer;
        if vStpController.StpReadyToStart or vStpController.ReadyToProg then
        begin
          try
            EventSubscribers.Execute(self, C_PosUpdate, nil);
          except
          end;
          {$IFNDEF LAB_DEBUG}
          if Supports(self, IChamberProcess, vChamberProc) then
          begin
            if (vChamberProc.StabMode in [mInRange, mInRangeTime])
            and SameValue(vChamberProc.TargetTempr, 25, vChamberProc.MaxDeltaTempr)
            and (vChamberProc.IsExposureDone) then
            begin
              fAutoProcState := apWaitPrepareProcess;
              ResumeMainProcess(20);
            end else
            begin
              vChamberProc.ManualChargeTemprBySpeed(25, 0);
              DoInfoMessage('Выход в нормальные условия...');
              vChamberProc.StopExposure;
              fAutoProcState := apWaitPreparePoint;
            end;
            vChamberProc := nil;
          end else
          begin
            fAutoProcState := apPrepareStop;
            ResumeMainProcess;
          end;
          {$ELSE}
          fAutoProcState := apWaitPrepareProcess;
          ResumeMainProcess;
          {$ENDIF}
          vStpController.DoneProgrammer;
        end else
        begin
          fAutoProcState := apInRangeMeasStarted;
          try
            EventSubscribers.Execute(self, C_PosUpdate, nil);
          except
          end;
          ResumeMainProcess;
        end;
      end else
      begin
        fAutoProcState := apPrepareStop;
        ResumeMainProcess;
      end;
      vStpController := nil;
    end;
    apWaitPrepareProcess:
    begin
      {$REGION ' WaitPrepareProcess '}
      if Supports(Self, IStpMeasureController, vStpController) then
      begin
        if vStpController.StpReadyToStart then
          vStpController.StpStart
        else
        begin
          fAutoProcState := apCheckProgOnPrepare;
          ResumeMainProcess;
        end;
      end else
      begin
        fAutoProcState := apPrepareStop;
        ResumeMainProcess;
      end;
      vStpController := nil;
    {$ENDREGION}
    end;
    apCheckProgOnPrepare:
    begin
      DoInfoMessage('Проверка очереди на программирование');
      if Supports(Self, IStpMeasureController, vStpController) then
      begin
        vStpController.PrepareProgrammer;
        if vStpController.ReadyToProg then
        begin
          fAutoProcState := apProgStart;
          DoInfoMessage('Программирование изделий');
        end else
        begin
          fAutoProcState := apInRangeMeasStarted;
          vStpController.DoneProgrammer;
        end;
      end else
        fAutoProcState := apPrepareStop;
      ResumeMainProcess;
      vStpController := nil;
    end;
    apProgStart:
    begin
      if Supports(Self, IStpMeasureController, vStpController) then
      begin
        vStpController.StartFinalProgramming;
      end else
      begin
        fAutoProcState := apPrepareStop;
        ResumeMainProcess;
      end;
      vStpController := nil;
    end;
    apInRangeMeasStarted:
    begin
      {$REGION ' MeasStarted '}
      if Supports(Self, IBoardProcess, vBoardProc) then
      begin
        if vBoardProc.PositionsCount > 0 then
        begin
          SetProcessState(wsInRange);
          CreateTermoprofile;
          if Supports(self, IChamberProcess, vChamberProc) then
          begin
            if (vChamberProc.PointsCount > 0) then
            begin
              DoInfoMessage('Работа в диапазоне');
              vChamberProc.FirstPoint;
              if Supports(Self, IStpMeasureController, vStpController) then
              begin
                if vStpController.StpReadyToStart then
                begin
                  DoInfoMessage(Format('Выход на температурную точку %f°C.',
                  [vChamberProc.CurrPointTempr]));
                  {$IFNDEF  LAB_DEBUG}
                  fAutoProcState := apWaitInRangePoint;
                  vChamberProc.ProcessCurrent;
                  {$ELSE}
                  fAutoProcState := apWaitInRangeProcess;
                  ResumeMainProcess;
                  {$ENDIF}
                end else
                begin
                  DoInfoMessage(Format('Пропуск точки %f°C.',
                  [vChamberProc.CurrPointTempr]));
                  fAutoProcState := apFindNextPoint;
                  ResumeMainProcess;
                end;
              end else
              begin
                fAutoProcState := apPrepareStop;
                ResumeMainProcess;
              end;
              vStpController := nil;
            end else
            begin
              DoInfoMessage('Пустой термопрофиль', 1);
              fAutoProcState := apPrepareStop;
              ResumeMainProcess;
            end;
          end else
          begin
            DoInfoMessage('Нет позиций для измерения', 1);
            fAutoProcState := apPrepareStop;
            ResumeMainProcess;
          end;
        end else
        begin
          fAutoProcState := apPrepareStop;
          ResumeMainProcess;
        end;
      end else
      begin
        fAutoProcState := apPrepareStop;
        ResumeMainProcess;
      end;
      vChamberProc := nil;
      vBoardProc := nil;
      {$ENDREGION}
    end;
    apFindNextPoint:
    begin
      {$REGION ' FindNextPoint '}
      if Supports(self, IChamberProcess, vChamberProc) then
      begin
        if vChamberProc.NextPoint then
        begin
          if Supports(Self, IStpMeasureController, vStpController) then
          begin
            vStpController.StpReinit;
            if vStpController.StpReadyToStart then
            begin
              DoInfoMessage(Format('Выход на температурную точку %f°C.',
              [vChamberProc.CurrPointTempr]));
              {$IFNDEF  LAB_DEBUG}
              fAutoProcState := apWaitInRangePoint;
              vChamberProc.ProcessCurrent;
              {$ELSE}
              fAutoProcState := apWaitInRangeProcess;
              ResumeMainProcess;
              {$ENDIF}
            end else
            begin
              DoInfoMessage(Format('Пропуск точки %f°C.',
              [vChamberProc.CurrPointTempr]));
              fAutoProcState := apFindNextPoint;
              ResumeMainProcess;
            end;
          end else
          begin
            fAutoProcState := apPrepareStop;
            ResumeMainProcess;
          end;
          vStpController := nil;
        end else
        begin
          fAutoProcState := apMeasFinished;
          ResumeMainProcess;
        end;
      end else
      begin
        fAutoProcState := apPrepareStop;
        ResumeMainProcess;
      end;
      vChamberProc := nil;
      {$ENDREGION}
    end;
    apWaitInRangeProcess:
    begin
      {$REGION ' WaitInRangeProcess '}
      if Supports(self, IChamberProcess, vChamberProc) then
      begin
        {$IFNDEF  LAB_DEBUG}
        if not SameValue(vChamberProc.CurrTempr, vChamberProc.CurrPointTempr, vChamberProc.MaxDeltaTempr) then
        begin
          fAutoProcState := apWaitInRangePoint;
          Log(lWarning,
          Format('В камере %f°C. ожидается %f°C. Некорректная температура при старте измерения. Процесс переведен в режим ожидания',
          [vChamberProc.CurrTempr, vChamberProc.CurrPointTempr]));
          vChamberProc := nil;
          exit;
        end;
        {$ENDIF}

        if Supports(Self, IStpMeasureController, vStpController) then
        begin
          vStpController.StpStart;
        end else
        begin
          fAutoProcState := apPrepareStop;
          ResumeMainProcess;
        end;
        vStpController := nil;
      end else
      begin
        fAutoProcState := apPrepareStop;
        ResumeMainProcess;
      end;
      vChamberProc := nil;
      {$ENDREGION}
    end;
    apMeasFinished:
    begin
      DoInfoMessage('Процесс завершен');
      SetProcessState(wsMeasureFinished);
      {$IFNDEF LAB_DEBUG}
      if Supports(self, IChamberProcess, vChamberProc) then
      begin
        vChamberProc.ManualChargeTemprBySpeed(25, 0);
        DoInfoMessage('Выход в нормальные условия...');
        vChamberProc := nil;
      end;
      DoInfoMessage('Проверка позиций, подготовленных к расчету');
      fAutoProcState := apCheckCalc;
      {$ELSE}
      fAutoProcState := apCheckCalc;//apPrepareStop;
      {$ENDIF}
      ResumeMainProcess;
    end;
    apCheckCalc:
    begin
      vReconstructorResult := Reconstruction;
      try
        EventSubscribers.Execute(self, C_PosUpdate, nil);
      except
      end;
      case vReconstructorResult of
        crSucc:
        begin
          {$IFNDEF LAB_DEBUG}
          if Supports(self, IChamberProcess, vChamberProc) then
          begin
            if SameValue(vChamberProc.CurrTempr, 25, vChamberProc.MaxDeltaTempr)
            and vChamberProc.IsExposureDone then
            begin
              fAutoProcState := apStopOrLoop;
              ResumeMainProcess;
            end else
              fAutoProcState := apWaitNormalAfterFinish;
          end else
          begin
            fAutoProcState := apPrepareStop;
            ResumeMainProcess;
          end;
          {$ELSE}
            fAutoProcState := apStopOrLoop;
            ResumeMainProcess;
          {$ENDIF}
          vChamberProc := nil;
          DoInfoMessage('Расчет завершен');
        end;
        crUserCanceled:
        begin
          DoInfoMessage('Расчет отменен пользователем. Остановка');
          fAutoProcState := apPrepareStop;
          ResumeMainProcess;
        end;
        crEmpty:
        begin
          DoInfoMessage('Расчет завершен с ошибкой. Остановка');
          fAutoProcState := apPrepareStop;
          ResumeMainProcess;
        end;
      end;
    end;
    apStopOrLoop:
    begin
      if fLooped then
      begin
        fAutoProcState := apStartCheckPrepare;
        ResumeMainProcess(20);
      end else
      begin
        fAutoProcState := apPrepareStop;
        ResumeMainProcess;
      end;
    end;
    apPrepareStop:
    begin
      if Supports(self, IChamberProcess, vChamberProc) then
      begin
        DoInfoMessage('Выключение камеры');
        vChamberProc.ManualTemperatureOff;
        vChamberProc.StopExposure;
      end;
      vChamberProc := nil;
      if Supports(Self, IBoardProcess, vBoardProc) then
      begin
        vBoardProc.Clear;
      end;
      vBoardProc := nil;
      fAutoProcState := apStoped;
      ResumeMainProcess;
    end;
    apStoped:
    begin
      if Supports(self, IChamberProcess, vChamberProc)
      and vChamberProc.ProcessWorked then
        vChamberProc.ProcessWorked := false;
      vChamberProc := nil;
      if fProcessWorked then
      begin
        fProcessWorked := false;
        EventSubscribers.Execute(self, C_OnProcessStoped, nil);
        DoInfoMessage('Процесс завершен');
      end;
      try
        EventSubscribers.Execute(self, C_PosUpdate, nil);
      except
      end;
    end;
  end;
end;

procedure TV8CompensProcess.SetProcessState(AState : TProcessState);
var
vStpController : IStpMeasureController;
begin
  Log(lDebug, Format('New Process State= %d',[ord(AState)]));
  if Supports(Self, IStpMeasureController, vStpController) then
    vStpController.SetStpState(Ord(AState));
  vStpController := nil;
  try
    EventSubscribers.Execute(self, C_PosUpdate, nil);
  except
  end;
end;

procedure TV8CompensProcess.MessageHandler(var message: TMessage);
begin
  case message.Msg of
    C_StartProcess: StartProcess;
    C_RestartProcess: RestartProcess;
    C_UserStopProcess: FullStopByUser;
    Env_Test: DoEnvTest;
    Env_TestOK: DoEnvTestOK;
    Env_TestFail: DoEnvTestFail;
    gProcess : DoMainProcess;
    Pos_Update_Msg:
      EventSubscribers.Execute(self, C_PosUpdate, nil);
    else
      inherited;
  end;
end;

procedure TV8CompensProcess.FullStop;
var
vStpController : IStpMeasureController;
begin
  if (fEnvInitTestState <> EnvTestOk) and (fEnvInitTestState <> EnvStart) then
  begin
    fEnvInitTestState := EnvTestFail;
    ResumeEnvironmentTest;
    DoInfoMessage('Ошибка функционального теста');
  end;
  if (fAutoProcState <> apStoped) then
  begin
    fAutoProcState := apPrepareStop;
    if Supports(Self, IStpMeasureController, vStpController) then
      vStpController.StpStop;
    vStpController := nil;
    ResumeMainProcess;
    DoInfoMessage('Процесс термокомпенсации остановлен', 1);
  end;
end;

procedure TV8CompensProcess.FullStopByUser;
begin
  if (fEnvInitTestState <> EnvTestOk) or (fAutoProcState <> apStoped) then
  begin
    DoInfoMessage('Остановка по команде пользователя');
    FullStop;
  end;
end;

procedure TV8CompensProcess.CallEnvProcByTimer(Sender: TObject);
begin
  if (Sender is TvdTimer) then
    TvdTimer(Sender).enabled := False;
  ResumeEnvironmentTest;
end;

procedure TV8CompensProcess.CallMainProcByTimer(Sender: TObject);
begin
  if (Sender is TvdTimer) then
    TvdTimer(Sender).enabled := False;
  ResumeMainProcess;
end;

procedure TV8CompensProcess.ResumeEnvironmentTest;
begin
  PostMessage(Env_Test, 0, 0);
end;

procedure TV8CompensProcess.ResumeEnvironmentTest(ASecondsDelay: Cardinal);
begin
  DoProcAfter(ASecondsDelay, CallEnvProcByTimer);
end;

procedure TV8CompensProcess.ResumeMainProcess;
begin
  PostMessage(gProcess, 0, 0);
end;

procedure TV8CompensProcess.ResumeMainProcess(ASecondsDelay: Cardinal);
begin
  DoProcAfter(ASecondsDelay, CallMainProcByTimer);
end;

procedure TV8CompensProcess.StartProcess;
begin
  fEnvInitTestState := EnvStart;
  ResumeEnvironmentTest;
end;

function TV8CompensProcess.StateHumanName(AState: byte): String;
begin
  if AState <= High(C_StpMethodsHumanName) then
    Result := C_StpMethodsHumanName[AState]
  else
    Result := '';
end;

function TV8CompensProcess.StateProgrammName(AState: byte): String;
begin
  if AState <= High(C_StpMethodsProgrammName) then
    Result := C_StpMethodsProgrammName[AState]
  else
    Result := '';
end;

function TV8CompensProcess.StatesCount: Byte;
begin
  Result := High(C_StpMethodsProgrammName)+1;
end;

procedure TV8CompensProcess.RestartProcess;
begin
  if fRestartMode then
    exit;
  fRestartMode := true;
  StartProcess;
end;

type
TStpTermoprofileItem = record
  Tempr, Speed : Double;
  Expos : byte;
end;

TStpTermoprofile = class(TList<TStpTermoprofileItem>)
public
 ID : Integer;
 procedure Load(const ADMAccess : IDMAccess; const AID : Integer);
end;

procedure TStpTermoprofile.Load(const ADMAccess : IDMAccess; const AID : Integer);
const
C_SelPointsSql = 'SELECT PFPOINTS.POINTVALUE, PFPOINTS.INI_OPTIONS '+
'FROM PFPOINTS WHERE RANGE_ID = :RANGE_ID ORDER BY PFPOINTS.P_INDEX';
var
vSelectQR : TvdQuery;
vIniFile : TMemIniFile;
vStr : TStringList;
vItem : TStpTermoprofileItem;
vFS : TFormatSettings;
begin
  ID := AID;
  Self.Clear;
  vFS := TFormatSettings.Create(1033);
  vIniFile := TMemIniFile.Create('');
  vStr := TStringList.Create;
  vSelectQR := ADMAccess.CreateReadQuery(nil, C_SelPointsSql);
  try
    vSelectQR.Prepare;
    vSelectQR.ParamByName('RANGE_ID').AsInteger := ID;
    vSelectQR.Open;
    while not vSelectQR.Eof do
    begin
      vItem.Tempr := vSelectQR.FieldByName('POINTVALUE').AsFloat;
      vItem.Speed := 0;
      vItem.Expos := 0;
      vStr.Clear;
      vStr.Text := vSelectQR.FieldByName('INI_OPTIONS').AsWideString;
      if (vStr.Count > 0) then
      begin
        vIniFile.Clear;
        vIniFile.SetStrings(vStr);
        vItem.Speed := Vodopad.CustomIniHelpers.ReadFloat(
                        vIniFile, 'stpoptions', 'speed', 0, vFS);
        vItem.Expos := Byte(vIniFile.ReadInteger('stpoptions', 'expos', 0));
      end;
      Add(vItem);
      vSelectQR.Next;
    end;
    vSelectQR.Close;
  finally
    FreeAndNil(vSelectQR);
    FreeAndNil(vStr);
    FreeAndNil(vIniFile);
  end;
end;

procedure TV8CompensProcess.CreateTermoprofile;
const
C_SelRangeIDSql = 'select rcard.workrange '+
'from rcard where rcard.id = :rcard_id';
var i, vPosIdx, j, k : word;
vSelectQR : TvdQuery;
vCardsIDList : TList<Integer>;
vStpTermoprofiles : TObjectList<TStpTermoprofile>;
vTermoprofile : TStpTermoprofile;
vStpTermoprofileItem : TStpTermoprofileItem;
vTPID : integer;
vDMAccess : IDMAccess;
vChamberTermoprofile : ITermoProfile;
vChamberPoint : pTermoPoint;
vBoardProcess : IBoardProcess;
vChamberProcess : IChamberProcess;
vBoard : IAbstractBoard;
vPositions : IPositionsList;
vPositionExtentions : IExtentions;
vStp8DboPosition : TPositionDBExtention;
function FindTermoprofile(AID : Integer):Boolean;
var
vTPIdx : Word;
begin
  vTPIdx := 0;
  while (vTPIdx < vStpTermoprofiles.Count) do
  begin
    if (vStpTermoprofiles[vTPIdx].ID = AID) then
      break;
    Inc(vTPIdx);
  end;
  Result := vTPIdx < vStpTermoprofiles.Count;
end;
begin
  i := 0;
  if ((not Supports(self, IChamberProcess, vChamberProcess))
  or (not Supports(vChamberProcess, ITermoProfile, vChamberTermoprofile))
  or (not Supports(self, IBoardProcess, vBoardProcess))
  or (not Supports(self, IDMAccess, vDMAccess))) then
  begin
    vBoardProcess := nil;
    vChamberProcess := nil;
    vChamberTermoprofile := nil;
    vDMAccess := nil;
     Exit;
  end;
  vChamberTermoprofile.Clear;
  if (vBoardProcess.BoardsCount < 1) then
  begin
    vBoardProcess := nil;
    vChamberProcess := nil;
    vChamberTermoprofile := nil;
    vDMAccess := nil;
    Exit;
  end;

  vCardsIDList := TList<Integer>.Create;
  vStpTermoprofiles := TObjectList<TStpTermoprofile>.Create;
  vSelectQR := vDMAccess.CreateReadQuery(nil, C_SelRangeIDSql);
  try
    while (i < vBoardProcess.BoardsCount)  do
    begin
      if Supports(vBoardProcess.Board[i], IAbstractBoard, vBoard)
      and vBoard.QueryPositionListInterface(IPositionsList, vPositions) then
      begin
        vPosIdx := 0;
        while vPosIdx < vPositions.Count do
        begin
          if Supports(vPositions.Items[vPosIdx], IExtentions, vPositionExtentions)
          and vPositionExtentions.Find(TPositionDBExtention, vStp8DboPosition)
          and  (vCardsIDList.IndexOf(vStp8DboPosition.RCardID) = -1) then
            vCardsIDList.Add(vStp8DboPosition.RCardID);
          vPositionExtentions := nil;
          inc(vPosIdx);
        end;
      end;
      vBoard := nil;
      vPositions := nil;
      inc(i);
    end;
    if (vCardsIDList.Count < 1) then
      Exit;
    vSelectQR.Prepare;
    for j := 0 to vCardsIDList.Count - 1 do
    begin
      vSelectQR.ParamByName('rcard_id').AsInteger := vCardsIDList[j];
      vSelectQR.Open;
      vTPID := vSelectQR.FieldByName('workrange').AsInteger;
      if not FindTermoprofile(vTPID) then
      begin
        vStpTermoprofiles.Add(TStpTermoprofile.Create);
        vStpTermoprofiles.Last.Load(vDMAccess, vTPID);
      end;
      vSelectQR.Close;
    end;
    if (vStpTermoprofiles.Count < 1) then
      Exit;
    for j := 0 to vStpTermoprofiles.Count -1 do
    begin
      vTermoprofile := vStpTermoprofiles[j];
      if (vTermoprofile.Count < 1) then
        Continue;
      for k :=0 to vTermoprofile.Count -1 do
      begin
        vStpTermoprofileItem := vTermoprofile[k];
        vTPID := vChamberTermoprofile.IndexOf(vStpTermoprofileItem.Tempr);
        if vTPID <> -1 then
        begin
          vChamberPoint := vChamberTermoprofile[vTPID];
          {скорость по минимуму}
          if vStpTermoprofileItem.Speed < vChamberPoint.Speed then
            vChamberPoint.Speed := vStpTermoprofileItem.Speed;
          if vChamberPoint.Speed < 0.1 then
            vChamberPoint.Speed := vChamberProcess.DefaultSpeed;
          {выдержка по максимуму}
          if vStpTermoprofileItem.Expos > vChamberPoint.Expos then
             vChamberPoint.Expos := vStpTermoprofileItem.Expos;
        end else
        begin
          vChamberPoint := vChamberTermoprofile.NewItem;
          vChamberPoint.Tempr := vStpTermoprofileItem.Tempr;
          if vStpTermoprofileItem.Speed < 0.1 then
            vChamberPoint.Speed := vChamberProcess.DefaultSpeed
          else
            vChamberPoint.Speed := vStpTermoprofileItem.Speed;
          vChamberPoint.Expos := vStpTermoprofileItem.Expos;
        end;
      end;
    end;
    vChamberTermoprofile.Sort;
  finally
    FreeAndNil(vCardsIDList);
    FreeAndNil(vStpTermoprofiles);
    FreeAndNil(vSelectQR);
    vStp8DboPosition := nil;
    vBoardProcess := nil;
    vChamberProcess := nil;
    vChamberTermoprofile := nil;
    vDMAccess := nil;
  end;
end;

function TV8CompensProcess.Reconstruction : TReconstructorResult;
var
i, vPosIdx : word;
vShowModal : IShowModalForm;
vAlreadyExists : boolean;
vFrm : TComponent;
vReconsFrm : IReconstructorForm;
vBoardProcess : IBoardProcess;
vBoard : IAbstractBoard;
vPositions : IPositionsList;
vPositionExtentions : IExtentions;
vStp8DboPosition : TPositionDBExtention;
vCalculatedObjs : TList<TPositionDBExtention>;
begin
  Result := crEmpty;
  if not Assigned(fReconstructorForm) then
    Exit;
  vFrm := fReconstructorForm.GetFormInstance(self, vAlreadyExists);
  if vAlreadyExists then
    Exit;
  try
    if (not Supports(vFrm, IReconstructorForm, vReconsFrm))
    or (not Supports(vFrm, IShowModalForm, vShowModal))
    or (not Supports(self, IBoardProcess, vBoardProcess)) then
    begin
      vShowModal := nil;
      vReconsFrm := nil;
      vBoardProcess := nil;
      Exit;
    end;
    vCalculatedObjs := TList<TPositionDBExtention>.Create;
    try
      i := 0;
      while (i < vBoardProcess.BoardsCount)  do
      begin
        if Supports(vBoardProcess.Board[i], IAbstractBoard, vBoard)
        and vBoard.QueryPositionListInterface(IPositionsList, vPositions) then
        begin
          vPosIdx := 0;
          while vPosIdx < vPositions.Count do
          begin
            if Supports(vPositions.Items[vPosIdx], IExtentions, vPositionExtentions)
            and vPositionExtentions.Find(TPositionDBExtention, vStp8DboPosition)
            and vStp8DboPosition.RecostructionRequest then
              vCalculatedObjs.Add(vStp8DboPosition);
            vPositionExtentions := nil;
            inc(vPosIdx);
          end;
        end;
        vPositions := nil;
        vBoard := nil;
        inc(i);
      end;
      if (vCalculatedObjs.Count > 0) then
        vReconsFrm.Init(vCalculatedObjs, Self)
      else
        Exit;
    finally
      vBoardProcess := nil;
      vReconsFrm := nil;
      FreeAndNil(vCalculatedObjs);
    end;
    case vShowModal.ModalResult of
      mrOk: Result := crSucc;
      mrCancel: Result := crUserCanceled;
      mrAbort: Result := crEmpty;
    end;
  finally
    vShowModal := nil;
    FreeAndNil(vFrm);
  end;
end;

procedure TV8CompensProcess.ChamberProcessEvent(Sender: TObject; Event: TGUID;
  Params: Pointer);
var
vDt : double;
vExposMin : byte;
vExposSec : Cardinal;
vChamberProc : IChamberProcess;
vTP : ITermoProfile;
begin
  if IsEqualGUID(Event, C_OnChamber_TemprStabilized) then
  begin
    if (not Supports(Sender, IChamberProcess, vChamberProc))
    or (not Supports(Sender, ITermoProfile, vTP)) then
    begin
      vTP := nil;
      vChamberProc := nil;
      exit;
    end;
    if not (fAutoProcState in [apWaitPreparePoint, apWaitInRangePoint, apCheckCalc, apWaitNormalAfterFinish]) then
    begin
      vTP := nil;
      vChamberProc := nil;
      exit;
    end;

    vExposMin := 1;
    vDt := abs(vChamberProc.CurrTempr - vTP.PrevTempr);
    {$IFNDEF LAB_DEBUG}
    case fAutoProcState of
      apWaitPreparePoint:
        vExposMin := 10+FixedTrunc(vDt * 0.11);
      apWaitInRangePoint:
      begin
        if vTP[vChamberProc.CurrentPoint].Expos > 0 then
          vExposMin := vTP[vChamberProc.CurrentPoint].Expos
        else
          vExposMin := 15+FixedTrunc(vDt * 0.11);
      end;
      apCheckCalc, apWaitNormalAfterFinish:
        vExposMin := 15+FixedTrunc(vDt * 0.11);
    end;
    {$ENDIF}
    vExposSec := vExposMin*60;
    vChamberProc.StartExposure(vExposSec);
    {$IFNDEF LAB_DEBUG}
    DoInfoMessage(Format('Dt = %3.2g. Старт выдержки %d мин.', [vDt, vExposMin]));
    {$ELSE}
    DoInfoMessage(Format('Режим лабораторной отладки. Старт выдержки %d мин.', [vExposMin]));
    {$ENDIF}
    vTP := nil;
    vChamberProc := nil;
  end else
  if IsEqualGUID(Event, C_OnTemprStabilizedAndExposure) then
  begin
    case fAutoProcState of
      apWaitPreparePoint:
      begin
        fAutoProcState := apWaitPrepareProcess;
        ResumeMainProcess;
      end;
      apWaitInRangePoint:
      begin
        fAutoProcState := apWaitInRangeProcess;
        ResumeMainProcess;
      end;  
      apWaitNormalAfterFinish:
      begin
        fAutoProcState := apStopOrLoop;
        ResumeMainProcess;
      end;
    end;
  end else
    inherited;
end;

function TV8CompensProcess.AppTitle: String;
const
C_Application_Title = 'Термокомпенсация';
//C_Application_Title = 'MAS TCXO Trimming';
begin
  Result := C_Application_Title;
end;

procedure TV8CompensProcess.BoardProcessEvent(Sender : TObject; Event : TGUID; Params : Pointer);
var  
vParams : pCBPEventParams;
vBoard : IAbstractBoard;
vBoardProcess : IBoardProcess;
vStpController : IStpMeasureController;
vChamberProc : IChamberProcess;
begin
  try
    if not Supports(Sender, IBoardProcess, vBoardProcess) then
    begin
      inherited;
      Exit;
    end;
    if IsEqualGUID(Event, C_SearchComplete) then
    begin
      if vBoardProcess.BoardsCount > 0 then
      begin
        DoInfoMessage('Платы найдены');
        if (fEnvInitTestState =  EnvStartBoardSearch) then
        begin
          fEnvInitTestState :=  EnvBoardTestOk;
          ResumeEnvironmentTest;
        end;
      end else
      begin
        DoInfoMessage('Платы не найдены', 1);
        if (fEnvInitTestState =  EnvStartBoardSearch) then
        begin
          fEnvInitTestState :=  EnvBoardFail;
          ResumeEnvironmentTest;
        end;
      end;
    end else
    if IsEqualGUID(Event, C_STPMeasureTestStarted) then
    begin
      if Assigned(Params) then
      begin
        vParams := pCBPEventParams(Params);
        if Supports(vParams.Board, IAbstractBoard, vBoard) then
        begin
          if fEnvInitTestState = EnvWaitStartTests then
          begin
            DoInfoMessage(Format('Плата %d старт функционального теста изделий',
                      [vBoard.SerialNum]));
          end else
          if fAutoProcState = apWaitPrepareProcess then
          begin
            DoInfoMessage(Format('Плата %d старт подготовки изделий',
                      [vBoard.SerialNum]));
          end else
          if fAutoProcState = apWaitInRangeProcess then
          begin
            if Supports(self, IChamberProcess, vChamberProc) then
              DoInfoMessage(Format('Плата %d старт измерений на точке %f°C.',
                      [vBoard.SerialNum, vChamberProc.CurrPointTempr]));
          end else
            DoInfoMessage(Format('Плата %d старт измерения изделий',
                      [vBoard.SerialNum]));
        end;
      end;
    end else
    if IsEqualGUID(Event, C_STPMeasureTestEnded) then
    begin
      if not Supports(Sender, IStpMeasureController, vStpController) then
        Exit;
      if Assigned(Params) then
      begin
        vParams := pCBPEventParams(Params);
        if Supports(vParams.Board, IAbstractBoard, vBoard) then
        begin
          if fEnvInitTestState = EnvWaitStartTests then
          begin
            DoInfoMessage(Format('Плата %d функциональный тест изделий выполнен',
                      [vBoard.SerialNum]));
          end else
          if fAutoProcState = apWaitPrepareProcess then
          begin
            DoInfoMessage(Format('Плата %d подготовка изделий выполнена',
                      [vBoard.SerialNum]));
          end else
          if fAutoProcState = apWaitInRangeProcess then
          begin
            if Supports(self, IChamberProcess, vChamberProc) then
            DoInfoMessage(Format('Плата %d измерения на точке %f°C завершены',
                    [vBoard.SerialNum, vChamberProc.CurrPointTempr]));
          end else
            DoInfoMessage(Format('Плата %d измерение изделий выполнено',
                      [vBoard.SerialNum]));

        end;
      end;
      if (vStpController.WorkedCount > 0) then
        Exit;
      if (fEnvInitTestState = EnvWaitStartTests) then
      begin
        fEnvInitTestState := EnvTestOk;
        ResumeEnvironmentTest;
      end else
      begin
        case fAutoProcState of
          apWaitPrepareProcess:
          begin
            if (vBoardProcess.PositionsCount > 0) then
              fAutoProcState := apCheckProgOnPrepare
            else
              fAutoProcState := apPrepareStop;
            ResumeMainProcess;
          end;
          apProgStart:
          begin
            if (vBoardProcess.PositionsCount > 0) then
              fAutoProcState := apInRangeMeasStarted
            else
              fAutoProcState := apPrepareStop;
            ResumeMainProcess;
          end;
          apWaitInRangeProcess:
          begin
            if (vBoardProcess.PositionsCount > 0) then
              fAutoProcState := apFindNextPoint
            else
              fAutoProcState := apPrepareStop;
            ResumeMainProcess;
          end;
        end;
      end;
    end else
    if IsEqualGUID(Event, C_ProgrammerMethodStarted) then
    begin
      if Assigned(Params) then
      begin
        vParams := pCBPEventParams(Params);
        if Supports(vParams.Board, IAbstractBoard, vBoard) then
          DoInfoMessage(Format('Плата %d старт программирования...',
                [vBoard.SerialNum]));
      end;
    end else
    if IsEqualGUID(Event, C_ProgrammerMethodEnded) then
    begin
      if not Supports(Sender, IStpMeasureController, vStpController) then
        Exit;
      if Assigned(Params) then
      begin
        vParams := pCBPEventParams(Params);
        if Supports(vParams.Board, IAbstractBoard, vBoard) then
          DoInfoMessage(Format('Плата %d программирование - OK',
                [vBoard.SerialNum]));

      end;
      if (vStpController.WorkedCount > 0) then
        Exit;
      if (fAutoProcState = apProgStart) then
      begin
        if (vBoardProcess.PositionsCount > 0) then
          fAutoProcState := apInRangeMeasStarted
        else
          fAutoProcState := apPrepareStop;
        ResumeMainProcess;
      end;
    end else
      inherited;
  finally
    vBoard := nil;
    vStpController := nil;
    vBoardProcess := nil;
    vChamberProc := nil;
  end;
end;  

procedure TV8CompensProcess.FillBoardsList(AList : TDBBoards);
const
C_SelPosCountText = 'select count (*) from rpos'+
        ' where ((rpos.status <= 1) and (rpos.board = :board));';
var
vDMAccess : IDMAccess;
vSelCountQR : TvdQuery;
vCC : IConfigController;
vStr : TStringList;
vIni : TMemIniFile;
i, vSerial : Integer;
vNewItem : TBoardDescript;
begin
  AList.Clear;
  if ((not Supports(Self, IConfigController, vCC))
  or (not Supports(Self, IDMAccess, vDMAccess))) then
  begin
    vCC := nil;
    vDMAccess := nil;
    Exit;
  end;
  vStr := TStringList.Create;
  vIni := TMemIniFile.Create(''{, TEncoding.UTF8});
  vSelCountQR := vDMAccess.CreateReadQuery(nil, C_SelPosCountText);
  try
    vCC.Load('BoardsList', vIni);
    vIni.ReadSections(vStr);
    if vStr.Count < 1 then
    begin
      Log(lWarning, 'Чтение списка плат: ini-файл пуст');
      Exit;
    end;
    vSelCountQR.Prepare;
    try
      for I := 0 to vStr.Count - 1 do
      begin
        if TryStrToInt(vStr[i], vSerial)  then
        begin
          vNewItem := AList.NewItem;
          vNewItem.Serial := Word(vSerial);
          vSelCountQR.ParamByName('board').AsInteger := vSerial;
          vSelCountQR.Open;
          vNewItem.CurrCount := Word(vSelCountQR.FieldByName('count').AsInteger);
          vSelCountQR.Close;
          vNewItem.PositionsCount := Word(vIni.ReadInteger(vStr[i], 'PositionsCount', 0));
          vCC.Load(Format('Board%d',[vNewItem.Serial]), vNewItem.IniFile);
        end else
          Log(lWarning, Format('Чтение списка плат: ошибка конвертации серийного номера %s в тип "Integer"',[vStr[i]]));
      end;
      AList.Sort(TComparer<TBoardDescript>.Construct(
      function(const Left, Right: TBoardDescript): Integer
      begin
        result := Left.Serial - Right.Serial;
      end));
    except on e : Exception do
      begin
        Log(lWarning, Format('Exception %s %s during load boards list',[e.ClassName, e.Message]));
      end;
    end;
  finally
    FreeAndNil(vSelCountQR);
    FreeAndNil(vStr);
    FreeAndNil(vIni);
    vCC := nil;
    vDMAccess := nil;
  end; 
end;

function TV8CompensProcess.DeleteBoardRecord(ABoardSerial : Word) : Boolean;
var
vIni : TMemIniFile;
vCC : IConfigController;
begin
  result := False;
  if not Supports(Self, IConfigController, vCC) then
  begin
    vCC := nil;
    Exit;
  end;
  vIni := TMemIniFile.Create(''{, TEncoding.UTF8});
  try
    vCC.Load('BoardsList', vIni);
    result := vIni.SectionExists(IntToStr(ABoardSerial));
    if not Result then
      Exit;
    vIni.EraseSection(IntToStr(ABoardSerial));
    vCC.Save('BoardsList', vIni);
    vCC.Delete(Format('Board%d',[ABoardSerial]));
  finally
    FreeAndNil(vIni);
    vCC := nil;
  end;
end;

function TV8CompensProcess.CalcLoggerGet(out obj): Boolean;
begin
  Result := Assigned(fCalcLogger);
  if Result then
    ILogger(obj) := fCalcLogger;
end;

procedure TV8CompensProcess.CallOnDBConnected;
var
vBuildFile : TMemIniFile;
vFileValid : boolean;
vCC : IConfigController;
begin
  inherited;
  if not Supports(self, IConfigController, vCC) then
  begin
    vCC := nil;
    Exit;
  end;
  fCreateCardForm := nil;
  fCardListForm := nil;
  fPositionsArrangeForm := nil;
  fReconstructorForm := nil;
  fProgammingQueueForm := nil;
  vBuildFile := TMemIniFile.Create(''{, TEncoding.UTF8});
  try
    vCC.Load('Software', vBuildFile);
    if vBuildFile.ValueExists('BuildConfiguration','CreateCardForm') then
    begin
      fCreateCardForm := TFormLibDescriptor.Create(
      ExtractFilePath(ParamStr(0))+vBuildFile.ReadString('BuildConfiguration','CreateCardForm',''),
      vFileValid);
      if not vFileValid then
        FreeAndNil(fCreateCardForm);
    end;
    if vBuildFile.ValueExists('BuildConfiguration','CardListForm') then
    begin
      fCardListForm := TFormLibDescriptor.Create(
      ExtractFilePath(ParamStr(0))+vBuildFile.ReadString('BuildConfiguration','CardListForm',''),
      vFileValid);
      if not vFileValid then
        FreeAndNil(fCardListForm);
    end;
    if vBuildFile.ValueExists('BuildConfiguration','PositionsArrangeForm') then
    begin
      fPositionsArrangeForm := TFormLibDescriptor.Create(
      ExtractFilePath(ParamStr(0))+vBuildFile.ReadString('BuildConfiguration','PositionsArrangeForm',''),
      vFileValid);
      if not vFileValid then
        FreeAndNil(fPositionsArrangeForm);
    end;
    if vBuildFile.ValueExists('BuildConfiguration','ReconstructorForm') then
    begin
      fReconstructorForm := TFormLibDescriptor.Create(
      ExtractFilePath(ParamStr(0))+vBuildFile.ReadString('BuildConfiguration','ReconstructorForm',''),
      vFileValid);
      if not vFileValid then
        FreeAndNil(fReconstructorForm);
    end;
    if vBuildFile.ValueExists('BuildConfiguration','ProgammingQueueForm') then
    begin
      fProgammingQueueForm := TFormLibDescriptor.Create(
      ExtractFilePath(ParamStr(0))+vBuildFile.ReadString('BuildConfiguration','ProgammingQueueForm',''),
      vFileValid);
      if not vFileValid then
        FreeAndNil(fProgammingQueueForm);
    end;
  finally
    FreeAndNil(vBuildFile);
    vCC := nil;
  end;
end;

procedure TV8CompensProcess.UserController_Restart;
begin
  PostMessage(C_RestartProcess, 0, 0);
end;

procedure TV8CompensProcess.UserController_Start;
begin
  PostMessage(C_StartProcess, 0, 0);
end;

procedure TV8CompensProcess.UserController_Stop;
begin
  PostMessage(C_UserStopProcess, 0, 0);
end;  

function TV8CompensProcess.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, IPatternLoader) then
  begin
    if Assigned(fPatternLoader) then
      Result := fPatternLoader.QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end else
  if IsEqualGUID(IID, ICreateCardForm) then
  begin
    if Assigned(fCreateCardForm) then
      Result := inherited QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end else
  if IsEqualGUID(IID, ICreateCardListForm) then
  begin
    if  Assigned(fCardListForm) then
      Result := inherited QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end else
  if IsEqualGUID(IID, ICreatePositionsArrangeForm) then
  begin
    if  Assigned(fPositionsArrangeForm) then
      Result := inherited QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end else
  if IsEqualGUID(IID, ICreateReconstructorForm) then
  begin
    if  Assigned(fReconstructorForm) then
      Result := inherited QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end else
  if IsEqualGUID(IID, ICreateProgammingQueueForm) then
  begin
    if  Assigned(fProgammingQueueForm) then
      Result := inherited QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
    Exit;
  end else
    Result := inherited QueryInterface(IID, Obj);
end;

function TV8CompensProcess.CardListForm_Create(var alreadyExists : boolean): TComponent;
begin
  Result := fCardListForm.GetFormInstance(Self, alreadyExists);
end;

function TV8CompensProcess.CreateCardForm_Create(var alreadyExists : boolean): TComponent;
begin
  Result := fCreateCardForm.GetFormInstance(Self, alreadyExists);
end;

function TV8CompensProcess.UiLibComponent_Create(const aLibFile: string;
  const AClassGetterFunctName: Widestring; AOwner : TComponent): TComponent;
begin
  Result := fUiLisbsRegistry.GetDestriptor(aLibFile).CreateComponent(AClassGetterFunctName, AOwner);
end;

function TV8CompensProcess.PositionsArrangeForm_Create(var alreadyExists : boolean): TComponent;
begin
  Result := fPositionsArrangeForm.GetFormInstance(Self, alreadyExists);
end;

function TV8CompensProcess.ReconstructorForm_Create(var alreadyExists: boolean): TComponent;
begin
  Result := fReconstructorForm.GetFormInstance(Self, alreadyExists);
end;

function TV8CompensProcess.ProgammingQueueForm_Create(var alreadyExists : boolean): TComponent;
begin
  Result := fProgammingQueueForm.GetFormInstance(Self, alreadyExists);
end;

function TV8CompensProcess.AboutForm_Create(var alreadyExists : boolean): TComponent;
begin
  alreadyExists := False;
  result := TCompensAboutBox.Create(Self);
end;

{ TFormLibDescriptor }


constructor TFormLibDescriptor.Create(const aFile : string; var AFileValid : Boolean);
var
vLibHandle : THandle;
vProcAddr : Pointer;
begin
  FWindowHandle := System.Classes.AllocateHWND(MessageHandler);
  AFileValid := False;
  fLibName := aFile;
  fLibHandle := 0;
  fFormInstance:= nil;
  if not FileExists(fLibName) then
    Exit;
  try
    vLibHandle := SafeLoadLibrary(fLibName,
          SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
    if (vLibHandle > 0) then
    try
      vProcAddr := GetProcAddress(vLibHandle, 'GetProcFormClass');
      AFileValid := Assigned(vProcAddr);
    finally
      FreeLibrary(vLibHandle);
    end;
  except
    AFileValid := False;
  end;
end;


type
  TPackageLoad = procedure;
  TPackageUnload = procedure;

procedure TFormLibDescriptor.FreeFormLibrary;
var
vUnloadProc :TPackageUnload;
begin
  if (fLibHandle > 0) then
  begin
    try
      @vUnloadProc := GetProcAddress(fLibHandle, 'Finalize');
      if Assigned(vUnloadProc) then
        vUnloadProc;
    finally
      FreeLibrary(fLibHandle);
      fLibHandle := 0;
    end;
  end;
end;

procedure TFormLibDescriptor.OnFormDestroy(Sender: TObject);
begin
  fFormInstance := nil;
  Winapi.Windows.PostMessage(FWindowHandle, WM_USER+5, 0, 0);
end;

function TFormLibDescriptor.CreateProcessForm(AOwner: TComponent):boolean;
var
vFormClassGetter : function :TComponentClass;stdcall;
vInitLibProc :TPackageLoad;
vFormOnDestroyEvent : IFormOnDestroyEvent;
begin
  Result := False;
  fLibHandle := SafeLoadLibrary(fLibName,
      SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
    if (fLibHandle = 0) then
      Exit;
    @vFormClassGetter := GetProcAddress(fLibHandle, 'GetProcFormClass');
    if not Assigned(vFormClassGetter) then
    begin
      FreeLibrary(fLibHandle);
      fLibHandle := 0;
      Exit;
    end;
    @vInitLibProc := GetProcAddress(fLibHandle, 'Initialize');
    if Assigned(vInitLibProc) then
    try
       vInitLibProc;
    except on e : Exception do
      begin
        ApplicationShowException(e);
        raise e;
      end;
    end;
    fFormInstance := vFormClassGetter.Create(AOwner);
    if Supports(fFormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
       vFormOnDestroyEvent.OnDestroy := OnFormDestroy;
    vFormOnDestroyEvent := nil;
    result := True;
  except
    if (fLibHandle > 0) then
    begin
      FreeLibrary(fLibHandle);
      fLibHandle := 0;
    end;
  end;
end;

destructor TFormLibDescriptor.Destroy;
var
vFormOnDestroyEvent : IFormOnDestroyEvent;
begin
  if Supports(fFormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
    vFormOnDestroyEvent.OnDestroy := nil;
  vFormOnDestroyEvent := nil;
  fFormInstance := nil;
  FreeFormLibrary;
  System.Classes.DeallocateHWND(FWindowHandle);
  inherited Destroy;
end;

function TFormLibDescriptor.GetFormInstance(AOwner: TComponent;
  var alreadyExists: boolean): TComponent;
begin
  Result := nil;
  alreadyExists := assigned(fFormInstance);
  if alreadyExists then
  begin
    Result := fFormInstance;
    exit;
  end;
  if CreateProcessForm(AOwner) then
    result := fFormInstance;
end;

procedure TFormLibDescriptor.MessageHandler(var message: TMessage);
begin
  if message.Msg = WM_USER+5 then
  begin
    fFormInstance := nil;
    FreeFormLibrary;
  end;
end;

{ TMdiFormsDestcriptor }

constructor TUiLibDestcriptor.Create(const aFile : string);
begin
  FWindowHandle := System.Classes.AllocateHWND(MessageHandler);
  fLibFile := aFile;
  fLibHandle := 0;
  fInstances := TObjectList.Create(False);
end;

function TUiLibDestcriptor.CreateComponent(const AClassGetterFunctName : Widestring; AOwner: TComponent): TComponent;
var
vFormClassGetter : function :TComponentClass;stdcall;
vInitLibProc :TPackageLoad;
vOnDestroyEvent : IFormOnDestroyEvent;
begin
  Result := nil;
  if (fLibHandle = 0) then
  begin
    fLibHandle := SafeLoadLibrary(fLibFile,
      SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
    try
      if (fLibHandle = 0) then
        Exit;
      @vFormClassGetter := GetProcAddress(fLibHandle, LPCWSTR(AClassGetterFunctName));
      if not Assigned(vFormClassGetter) then
      begin
        FreeLibrary(fLibHandle);
        fLibHandle := 0;
        Exit;
      end;
      @vInitLibProc := GetProcAddress(fLibHandle, 'Initialize');
      if Assigned(vInitLibProc) then
         vInitLibProc;
    except
      if (fLibHandle > 0) then
      begin
        FreeLibrary(fLibHandle);
        fLibHandle := 0;
      end;
    end;
  end else
    @vFormClassGetter := GetProcAddress(fLibHandle, LPCWSTR(AClassGetterFunctName));
  if Assigned(vFormClassGetter) then
    result := vFormClassGetter.Create(AOwner);

  if Supports(result, IFormOnDestroyEvent, vOnDestroyEvent) then
  begin
    vOnDestroyEvent.OnDestroy := OnComponentDestroy;
    fInstances.Add(result);
  end else
  begin
    if Assigned(Result) then
      FreeAndNil(Result);
    if (fInstances.Count < 1) then
      Winapi.Windows.PostMessage(FWindowHandle, WM_USER+6, 0, 0);
  end;
  vOnDestroyEvent := nil;
end;

destructor TUiLibDestcriptor.Destroy;
var
vOnDestroyEvent : IFormOnDestroyEvent;
vIdx : Byte;
begin
  //vIdx := 0;
  if fInstances.Count > 0 then
  begin
    for vIdx := fInstances.Count-1 downto 0 do
    begin
      if Supports(fInstances[vIdx], IFormOnDestroyEvent, vOnDestroyEvent) then
      begin
        vOnDestroyEvent.OnDestroy := nil;
        vOnDestroyEvent := nil;
      end;
      fInstances.Delete(vIdx);
    end;
  end;
  FreeUiLibrary;
  System.Classes.DeallocateHWND(FWindowHandle);
  FreeAndNil(fInstances);
  inherited Destroy;
end;

procedure TUiLibDestcriptor.FreeUiLibrary;
var
vUnloadProc :TPackageUnload;
begin
  if (fLibHandle > 0) then
  begin
    @vUnloadProc := GetProcAddress(fLibHandle, 'Finalize');
    if Assigned(vUnloadProc) then
      vUnloadProc;
    FreeLibrary(fLibHandle);
    fLibHandle := 0;
  end;
end;

procedure TUiLibDestcriptor.MessageHandler(var message: TMessage);
begin
  if message.Msg = WM_USER+6 then
    FreeUiLibrary;
end;

procedure TUiLibDestcriptor.OnComponentDestroy(Sender: TObject);
var
vIdx : Integer;
begin
  vIdx := fInstances.IndexOf(Sender);
  if vIdx <> -1 then
    fInstances.Delete(vIdx);
  if (fInstances.Count < 1) then
    Winapi.Windows.PostMessage(FWindowHandle, WM_USER+6, 0, 0);
end;

{ TMdiFormsRegistry }

function TUiLibsRegistry.GetDestriptor(const aFile: string): TUiLibDestcriptor;
function FindDestriptor:integer;
begin
  result := 0;
  while (Result < Count)  do
  begin
    if (CompareStr(TUiLibDestcriptor(Items[result]).LibFile, aFile) = 0) then
      Break;
    Inc(result);
  end;
  if (Result = Count) then
    Result := -1;
end;
var
vIdx : Integer;
begin
  vIdx := FindDestriptor;
  if vIdx = -1 then
  begin
    Result := TUiLibDestcriptor.Create(aFile);
    Add(Result);
  end else
    Result := TUiLibDestcriptor(Items[vIdx]);
end;

{ TPatternLoader }

function TPatternLoader.Load(const AConnection : TObject; const AID: integer; AField: TPatternFiledType;
  const ADest: TMemoryStream):boolean;
var
vConnection : TvdConnection;
vDM : IDMAccess;
vReadQR : TvdQuery;
vBlob : TStream;
begin
  Result := False;
  ADest.Clear;
  vConnection := nil;
  if not Supports(Self.owner, IDMAccess, vDM) then
    Exit;
  if (AConnection is TvdConnection) then
     vConnection := AConnection as TvdConnection;
  case AField of
    pfBinary: vReadQR := vDM.CreateReadQuery(nil, 'SELECT PROC_PATTERNS.DATA_BINARY AS DATA '+
          'FROM PROC_PATTERNS WHERE PROC_PATTERNS.PATTERN_ID = :PATTERN_ID ', vConnection);
    pfText:  vReadQR := vDM.CreateReadQuery(nil, 'SELECT PROC_PATTERNS.DATA_TEXT AS DATA '+
          'FROM PROC_PATTERNS WHERE PROC_PATTERNS.PATTERN_ID = :PATTERN_ID ', vConnection);
  end;

  try
    vReadQR.Prepare;
    vReadQR.ParamByName('PATTERN_ID').AsInteger := AID;
    vReadQR.Open;
    Result := vReadQR.RecordCount > 0;
    if result then
    begin
      vBlob := vReadQR.CreateBlobStream(vReadQR.FieldByName('DATA'), bmRead);
      try
        ADest.LoadFromStream(vBlob);
      finally
        FreeAndNil(vBlob);
        Result := (ADest.Size > 0);
      end;
    end;
    vReadQR.Close;
  finally
    FreeAndNil(vReadQR);
    vDM := nil;
  end;
end;

function TPatternLoader.Load(const AConnection : TObject; const AID: integer; const ADest: TStrings):boolean;
var
vConnection : TvdConnection;
vDM : IDMAccess;
vReadQR : TvdQuery;
begin
  Result := False;
  ADest.Clear;
  vConnection := nil;
  if not Supports(Self.owner, IDMAccess, vDM) then
    Exit;
  if (AConnection is TvdConnection) then
     vConnection := AConnection as TvdConnection;
  vReadQR := vDM.CreateReadQuery(nil, 'SELECT PROC_PATTERNS.DATA_TEXT AS DATA '+
          'FROM PROC_PATTERNS WHERE PROC_PATTERNS.PATTERN_ID = :PATTERN_ID ', vConnection);

  try
    vReadQR.Prepare;
    vReadQR.ParamByName('PATTERN_ID').AsInteger := AID;
    vReadQR.Open;
    Result := vReadQR.RecordCount > 0;
    if result then
      ADest.Text := vReadQR.FieldByName('DATA').AsWideString;
    vReadQR.Close;
  finally
    FreeAndNil(vReadQR);
    vDM := nil;
  end;
end;

function TPatternLoader.Load(const AConnection : TObject; const AID: integer; const ADest: TMemIniFile):boolean;
var
vStr : TStringList;
begin
  ADest.Clear;
  vStr := TStringList.Create;
  try
    Result := Load(AConnection, AID, vStr);
    if result then
       ADest.SetStrings(vStr);
  finally
    FreeAndNil(vStr);
  end;
end;

end.
