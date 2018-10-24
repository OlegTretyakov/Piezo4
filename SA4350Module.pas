unit SA4350Module;
interface
uses
   WinApi.Windows, System.Classes, Vodopad.EventList,
   Vodopad.ObjectList, System.SysUtils, System.DateUtils, Vodopad.Timer,
   ComConnector, EventBusInterface,
   System.Contnrs, WinApi.Messages, LoggerInterface, ChamberInterfaces,
   CustomChildProcess;

  type
  TChamberCommands = (T, FT, LT, ST, KT, O, SY, SX, RS, GTL, RT);
  TMovVariance = class(TObject)
   private
    fItems : array of Double;
    fVariance,
    fAverageTempr : double;
   public
    constructor Create(Capacity : LongWord);
    destructor Destroy; override;
    procedure SetValue(value : double);
    property Variance : double read fVariance;
    property AverageTempr : double read fAverageTempr;
  end;


  TWatcher = record
   private
    fWatchTime : LongWord; //in seconds
    fStartingTime : TDateTime;
    fTemprRangeValue : double;
    procedure SetWatchTime(ASecons : LongWord);
    procedure SetRange(Range : double);
   public
    property RangeValue : double read fTemprRangeValue write SetRange;
    procedure StartExposure;
    property RangeTime : LongWord read fWatchTime write SetWatchTime;
    property StartingTime : TDateTime read fStartingTime;
    function TemprIsInRange(const aDifferent : double) : boolean;
    function SecondElapsed: LongWord;
    function isTimeExpired : boolean;
  end;

  pChamberCommandItem = ^TChamberCommandItem;
  TChamberCommandItem = record
  strict private
    fCommand : TChamberCommands;
    fTargetTempr : double;
    fSecondsTargetTime, fSecondsTimeOut : LongWord; //in seconds
    procedure SetTargetTempr(const Value: double);
    procedure SetTargetTime(const Value: LongWord); 
    procedure SetTimeOut(const Value: LongWord);
  public
    property Command : TChamberCommands read fCommand write fCommand;
    property TargetTempr : double read fTargetTempr write SetTargetTempr;
    property SecondsChargeTime : LongWord read fSecondsTargetTime write SetTargetTime;
    property SecondsTimeOut : LongWord read fSecondsTimeOut write SetTimeOut;
    function MemAlloc : pChamberCommandItem;
  end;

  TChamberCommandQueue = class(TList)
   protected
     procedure Notify(Ptr: Pointer; Action: TListNotification); override;
     function Get(Index: Integer): pChamberCommandItem;
   public
    function Add(ACommand : TChamberCommandItem) : integer;
    function First : pChamberCommandItem;
    function Last : pChamberCommandItem;
    procedure Clear; override;
    property Items[Index: Integer]: pChamberCommandItem read Get; default;
  end;

  TChamber = class(TComConnector, IChamber, IChamberConnectionState)
   private
    fCheckTimer : TvdTimer;
    fChamberIniParams :TChamberIni;
    fPMLocalFormatSettings : TFormatSettings;
    fMovVariance : TMovVariance;
    fGetTemprSended,
    fTryConnect, fConnected, fLostConnect,
    fLostConnectSignaled,
    fChargeTimeOutSignaled : boolean;
    fLostConnectTime, fReconectAfterLostTime, fChargeTimeStart : TDateTime;
    fCommandsQueue : TChamberCommandQueue;
    fChallengeInterval,
    fChallengeTimeOutInterval,
    fSecondsTargetTime,
    fSecondsTargetTimeOut,
    fLagCount, fTimeOutCoeff : LongWord;
    fCurrentTempr,// : Extended;
    fTargetTempr : double;
    fStabMode : TChamberStabMode;
    fTemprWatcher : TWatcher;
    FEncoding : TEncoding;
    fRxBuffer : TBytes;
    fTxBuffer : array [0..254] of byte;
    fTargetTimeOutCount,
    fTxCount : byte;
    fOnTempr,
    fOnTemprCharge,
    fOnTemprInTarget,
    fOnStartChallenge,
    fOnConnect,  
    fonTryConnectiontFail,
    fOnConnectionLost,
    fOnDisconnect,
    fOnChargeTimeOut,
    fOnTemprOutOfTarget : TNotifyEvent;
    procedure SetTargetTempr(const AValue : double); stdcall;
    function GetTargetTempr : double; stdcall;
    function GetCurrTempr : double; stdcall;
    //procedure SetCurrTempr(const AValue : double); stdcall;
    procedure SetStateConnected(const value : boolean);
    procedure CheckState(Sender : TObject);
    function getAverageTempr: double;
    function GetSpeed: Double; stdcall;
    procedure SendTempr(const Tempr : double; const SecondsChargeTime : LongWord = 0);
    procedure SendTemperatureOff;
    procedure SendChamberOff;
    procedure LoadParamsFromFile;
    procedure SendData;
    function SendSetTempr(const ATempr: double; const ASeconds: Cardinal): boolean;
    function GetSecondsTargetTime: LongWord; stdcall;
    function GetSecondsTargetTimeOut: LongWord;stdcall;
    function GetStabMode: TChamberStabMode; stdcall;
    property OnConnectionLost : TNotifyEvent read fOnConnectionLost write fOnConnectionLost;
    property OnTempr : TNotifyEvent read fOnTempr write fOnTempr;
    property OnTemprCharge : TNotifyEvent read fOnTemprCharge write fOnTemprCharge;
    property OnTemprInTarget: TNotifyEvent read fOnTemprInTarget write fOnTemprInTarget;
    property OnChargeTimeOut: TNotifyEvent read fOnChargeTimeOut write fOnChargeTimeOut;
    property OnTemprOutOfTarget : TNotifyEvent read fOnTemprOutOfTarget write fOnTemprOutOfTarget;
    {IChamberConnectionState}  
    function GetConnected:Boolean;stdcall;  
    function GetPort:Byte;stdcall;
   protected
    function TryConnect(APort : byte)  : boolean;
    procedure onResultDataPacket(const Buffer; Count : Word); override;
    procedure DoChallenge(Sender: TObject); override;
    procedure OnTimeOut(Sender: TObject); override;
    procedure SendDisconnect; virtual;
    property OnStartChallenge : TNotifyEvent read fOnStartChallenge write fOnStartChallenge;
    property OnConnect : TNotifyEvent read fOnConnect write fOnConnect;  
    property onTryConnectiontFail : TNotifyEvent read fonTryConnectiontFail write fonTryConnectiontFail;
    property OnDisconnect: TNotifyEvent read fOnDisconnect write fOnDisconnect;
   public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Speed : double read getSpeed;
    property AverageTempr : double read getAverageTempr;
    property Connected : boolean read GetConnected;
    property CurrTempr : double read GetCurrTempr;
    property TargetTempr : double read GetTargetTempr;
    property SecondsTargetTime : LongWord read GetSecondsTargetTime;
    property SecondsTargetTimeOut : LongWord read GetSecondsTargetTimeOut;
    property StabMode : TChamberStabMode read GetStabMode;
    property TemprWatcher : TWatcher read fTemprWatcher write fTemprWatcher;
  end;


  TChamberProcess = class;
  TTermoProfile = class(TList, IInterface, ITermoProfile)
  strict private
    fProcess : TChamberProcess;
    fPrevTempr : double;
    function Get(Index: Integer): pTermoPoint;
  private
    function GetPrevTempr: double;stdcall;
    procedure SetPrevTempr(const ATempr : double); stdcall;
    procedure ITermoProfile.Clear = ClearTP;
    procedure ClearTP; stdcall;
    procedure ITermoProfile.Sort = SortTP;
    procedure SortTP; stdcall;
    function ITermoProfile.Count = TermoProfile_Count;
    function TermoProfile_Count : Word; stdcall;
    function ITermoProfile.Get = TermoProfile_Get;
    function TermoProfile_Get(Index: Integer): pTermoPoint; stdcall;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    {IInterface}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(const AProcess : TChamberProcess);
    procedure Clear; override;
    function First : pTermoPoint;
    function Last : pTermoPoint;
    function NewItem : pTermoPoint; stdcall;
    property Items[Index: Integer]: pTermoPoint read Get; default;
    property PrevTempr : double read GetPrevTempr write SetPrevTempr;
    function PointExists(const ATempr : double):boolean;stdcall;
    function IndexOf(const ATempr : double):integer;stdcall;
  end;

  TChamberProcessIniParams = record
    Speed : double;
    procedure LoadParamsFromFile;
  end;

  TChamberProcess = class(TCustomChildProcess, IChamberProcess, IEventBus)
  strict private
    fIniParams : TChamberProcessIniParams;
    fUiHandle : THandle;
    fFormInstance : TComponent;
    fTermoProfile : TTermoProfile;
    fCurrentPoint : integer;
    fDoDisconnect, fProcessWorked : boolean;
    fChamber : TChamber;
    FWindowHandle: HWND;
    fExposureWatcher : TWatcher;
    fIsInExposure : boolean;

    fEventSubscribers :TCustomObjEventList;
    procedure MessageHandler(var message : TMessage);
    procedure FreeFormLibrary;
    procedure OnFormDestroy(Sender: TObject);
    procedure SetCurrentPoint(index : integer);
    function GetCurrentPointIdx: integer; stdcall;
    function PointExists(index : integer):boolean;
    procedure GotoPoint(AIndex : integer);
    procedure ChamberConnected(Sender: TObject);  
    procedure ChamberTryConnectFail(Sender: TObject);
    procedure ChamberConnectionLost(Sender: TObject);
    procedure ChamberDisconnected(Sender: TObject);
    procedure ChamberTempr(Sender: TObject);
    procedure ChamberTemprStabilized(Sender: TObject);  
    procedure ChamberStabilizedTimeOut(Sender: TObject);
    procedure ChamberTemprOutOfTarget(Sender: TObject);
    procedure TemprCharge(Sender: TObject);
  private
    procedure ChamberOffBtnClick(Sender: TObject);
    procedure SetTemprBtnClick(Sender: TObject);
    procedure TemperatOffBtnClick(Sender: TObject);
    function GetTargetTempr : double; stdcall;
    function GetCurrTempr : double; stdcall;
    function GetCurrPointTempr : double; stdcall;
    function MaxDeltaTempr : double; stdcall;
    procedure SetProcessWorked(const Value: boolean); stdcall;
    function GetProcessWorked: boolean;stdcall;
    function GetPointsCount : Word; stdcall;  
    function GetStabMode: TChamberStabMode; stdcall;
    function GetIsInExposure: boolean; stdcall;
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  public
    constructor Create(AOwner : TComponent; AFunct : pLoggerFunct); override;
    destructor Destroy; override;
    function StartChamberModule(sPort : byte) : boolean;stdcall;
    procedure ClearTermoProfile;stdcall;
    function DefaultSpeed : double; stdcall;
    procedure SendDisconnectAndWait;stdcall;
    property Chamber : TChamber read fChamber;
    property TermoProfile : TTermoProfile read fTermoProfile;
    property CurrentPoint : integer read fCurrentPoint;
    property ProcessWorked : boolean read GetProcessWorked write SetProcessWorked;
    procedure ShowForm(AOwner : TComponent); stdcall;
    procedure BlockForm(ADoBlock : boolean); stdcall;
    procedure CloseForm; stdcall;
    property PointsCount : Word read GetPointsCount;
    function FirstPoint : boolean; stdcall;
    function BOF: boolean;stdcall;
    function EOF : boolean; stdcall;
    function NextPoint : boolean;stdcall;
    function PrevPoint : boolean;stdcall;
    procedure ProcessCurrent;stdcall;
    procedure StartExposure(ASeconds : LongWord);stdcall;
    procedure StopExposure; stdcall;
    function ExposureSecondElapsed: LongWord; stdcall;
    function GetStartingTime: TDateTime;stdcall;
    property isExposure : boolean read GetIsInExposure;
    function IsExposureDone : boolean; stdcall;
    procedure ManualChargeTempr(const Tempr : double; const Minutes : LongWord = 0); stdcall;
    procedure ManualChargeTemprBySpeed(const Target, SpeedDegMin: double);stdcall;
    procedure ManualTemperatureOff; stdcall;
    procedure ManualChamberOff; stdcall;
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : EventBusInterface.TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: EventBusInterface.TCustomObjEvent); stdcall;
  end;

implementation
uses
System.Math,
System.AnsiStrings,
Vodopad.Math,
System.IniFiles,
ComConnectorInterface,
Vodopad.CustomIniHelpers,
AbstractSearchObject,
FormsControllerInterface,
SA4350MCInterface;


const
Max_Seconds_Lost_Connect : integer = 5;
C_StopMarker : array [0..1] of byte = ($0D, $0A);
Tempr_Min_Range : double = 0.2;
Tempr_Max_Range : double = 0.5;
Tempr_Range_Time : integer = 120;

OnFormClose = WM_USER + 220;
OnChamber_Detected = OnFormClose + 1;
OnChamber_TryConnectionFail = OnChamber_Detected + 1;
OnChamber_ConnectionLost = OnChamber_TryConnectionFail + 1;
OnChamber_Disconnected = OnChamber_ConnectionLost + 1;
OnChamber_Tempr = OnChamber_Disconnected + 1;
OnChamber_TemprStabilized = OnChamber_Tempr + 1;
OnChamber_StabilizedTimeOut = OnChamber_TemprStabilized + 1;
OnChamber_TemprOutOfTarget = OnChamber_StabilizedTimeOut + 1;
OnTempr_Charge = OnChamber_TemprOutOfTarget + 1;


function ChamberProcessClass: TChildProcessClass; stdcall;
begin
  Result := TChamberProcess;
end; exports ChamberProcessClass;

{$REGION ' Logger '}

function InitLogger(AFunct : pLoggerFunct; out Obj):boolean;
var
vConfig : pLogClassConfig;
begin
  Result := False;
  if not Assigned(AFunct) then
    Exit;
  New(vConfig);
  try
    vConfig.ConfigGUID := StringToGUID('{5426BE05-3103-4F44-ACC1-D25041D05DC6}');
    vConfig.EnabledLevels := [lInfo, lEvent, lDebug, lPortWrite, lPortRead, lTrace, lWarning, lError,
                             lException, lExceptionOS];
    vConfig.DefaultLevelsView := [lInfo, lEvent, lWarning];
    vConfig.FileExtension := 'chmblog';
    vConfig.ArhiveExtension := 'chmblog7z';
    vConfig.ModuleName := 'Процесс ТК(S&A-4350)';
    try
      Result := AFunct.AddLogger(vConfig, Obj);
    except
      Result := false;
    end;
  finally
    Dispose(vConfig);
  end;
end;

var ChmbLogger : ILogger;


{$ENDREGION}

{$REGION ' Chamber '}

type
TCSO = class(TAbstractSearchObject)
  fcmb : TChamber; 
  procedure OnStartChallenge(Sender : TObject);
  procedure OnConnectFail(Sender : TObject);
  procedure OnConnected(Sender : TObject);
  //procedure OnDisconnect(Sender: TObject);
  constructor Create(APort : byte;
                AFormHandle:HWND);override;
  destructor Destroy;override;
end;

constructor TCSO.Create(APort: byte;
                AFormHandle:HWND);
begin
  inherited;
  fcmb := TChamber.Create(nil);
  fcmb.OnConnect := OnConnected;
  fcmb.onTryConnectiontFail := OnConnectFail;
  //fcmb.OnDisconnect := OnDisconnect;
  if not fcmb.TryConnect(APort) then
  begin
    PostMessage(Self.FormHandle, C_ChamberSearchState, 4, Integer(Self));
    fcmb.StopTimers;
  end else
    fcmb.OnStartChallenge := OnStartChallenge;
end;

destructor TCSO.Destroy;
begin
  if fcmb.Connected then
  begin
    fcmb.SendDisconnect;
    fcmb.DoChallenge(nil);
  end;
  FreeAndNil(fcmb);
  inherited Destroy;
end;

procedure TCSO.OnStartChallenge(Sender: TObject);
begin
  PostMessage(Self.FormHandle, C_ChamberSearchState, 2, Integer(Self));
end;

procedure TCSO.OnConnected(Sender: TObject);
begin
  fcmb.StopTimers;
  //fcmb.PortClose;
  PostMessage(Self.FormHandle, C_ChamberSearchState, 3, Integer(Self));
end;

{procedure TCSO.OnDisconnect(Sender: TObject);
begin
  PostMessage(Self.FormHandle, C_ChamberSearchState, 5, Integer(Self));
end; }

procedure TCSO.OnConnectFail(Sender: TObject);
begin
  fcmb.StopTimers;
  PostMessage(Self.FormHandle, C_ChamberSearchState, 4, Integer(Self));
end;

function ChamberSearchObjectClass: TSearchObjectClass; stdcall;
begin
  Result := TCSO;
end; exports ChamberSearchObjectClass;

constructor TChamber.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FEncoding := TEncoding.GetEncoding(CP_ACP);
  SetLength(fRxBuffer, 20);
  fChargeTimeOutSignaled := false;
  fCheckTimer := TvdTimer.Create(nil);
  fCheckTimer.OnTimer := CheckState;
  fCheckTimer.Interval := 500;
  fCheckTimer.Enabled := true;
  fLagCount := 0;
  fTimeOutCoeff := 0;
  fGetTemprSended := false;
  fPMLocalFormatSettings := TFormatSettings.Create(1033);
  fConnected := false;
  fMovVariance := TMovVariance.create(20);
  fCommandsQueue := TChamberCommandQueue.Create;
  fChallengeInterval := 500;
  fChallengeTimeOutInterval := 1000;
  fSecondsTargetTimeOut := 600;
  fTemprWatcher.SetRange(Tempr_Max_Range);
  fTemprWatcher.RangeTime := Tempr_Range_Time;
  fStabMode := mChallenge;
  ConfigParser(C_StopMarker, 0, C_StopMarker, Length(C_StopMarker), false);
  fLostConnect := false;
  LoadParamsFromFile;
  fSecondsTargetTimeOut := 100;
end;

destructor TChamber.Destroy;
begin
  StopTimers;
  fCommandsQueue.Clear;
  FreeAndNil(fCommandsQueue);
  FreeAndNil(fMovVariance);
  FreeAndNil(fCheckTimer);
  FreeAndNil(FEncoding);
  SetLength(fRxBuffer, 0);
  inherited Destroy;
end; 

procedure TChamber.LoadParamsFromFile;
begin
  fChamberIniParams.LoadIni;
  fTemprWatcher.SetRange(fChamberIniParams.DeltaTempr);
  fTemprWatcher.RangeTime := fChamberIniParams.DeltaTime;
  fTimeOutCoeff := fChamberIniParams.TimeOutCoeff;
  if Assigned(ChmbLogger) then
    ChmbLogger.Log(
                lInfo,
                Format('DeltaTempr=%f °C, DeltaTime=%d сек',
                [fTemprWatcher.RangeValue, fTemprWatcher.RangeTime]));
end;

function TChamber.TryConnect(APort : byte) : boolean;
begin
  fCommandsQueue.Clear;
  PortClose;
  fLagCount := 0;
  fTargetTimeOutCount := 0;
  try
    ConfigPort(APort, 9600, false, false, false);
    result := PortOpenOk;
    if result then
    begin
      fTryConnect := true;
      result := CallChallenge(15);
    end;
  except
     Result := False;
  end;
end;

procedure TChamber.OnTimeOut(Sender: TObject);
begin
  StopTimers;
  if Assigned(ChmbLogger) then
  begin
    if Sender is TvdTimer then
    ChmbLogger.Log(
                  lWarning,
                  Format('Порт COM%d, Таймаут %d мсек',
                  [self.Port, TvdTimer(Sender).Interval])) else
    ChmbLogger.Log(
                  lWarning,
                  Format('Порт COM%d, Таймаут',
                  [self.Port]));
  end;
  fCommandsQueue.Clear;
  if ((fConnected) and (not fLostConnect) and (not fTryConnect)) then
  begin
    fLostConnect := true;
    fLostConnectTime := now;
  end;
  if ((not fLostConnect) and (not fConnected)) or (fTryConnect) then
  begin
    if Assigned(ChmbLogger) then
      ChmbLogger.Log(lDebug, Format('Нет устройства на порту COM%d' ,
         [Port]));
    PortClose;
    if Assigned(fonTryConnectiontFail) then
      fonTryConnectiontFail(Self);
    exit;
  end;

  if fLostConnect then
  begin
    if (SecondsBetween(now, fLostConnectTime) > Max_Seconds_Lost_Connect) then
    begin
      if fConnected then
        fReconectAfterLostTime := IncMilliSecond(now, Self.fChallengeTimeOutInterval * 3);
      fConnected := false;
      if not fLostConnectSignaled then
      begin
        if assigned(fOnConnectionLost) then
          fOnConnectionLost(self);
        fLostConnectSignaled := true;
      end;
      if (SecondsBetween(now, fReconectAfterLostTime) < 1) then
      begin
        PortClose;
        TryConnect(Port);
        fTryConnect := false;
        fReconectAfterLostTime := IncMilliSecond(now, Self.fChallengeTimeOutInterval * 3);
        exit;
      end;
    end;
    CallChallenge(15);
  end;
end;

function TChamber.getAverageTempr: double;
begin
  result := fMovVariance.AverageTempr;
end;

function TChamber.GetConnected: Boolean;
begin
  Result := fConnected;
end;

function TChamber.GetCurrTempr: double;
begin
  result := fCurrentTempr;
end;  

function TChamber.GetPort: Byte;
begin
  Result := Self.Port;
end;

function TChamber.GetSecondsTargetTime: LongWord;
begin
  result := fSecondsTargetTime;
end;

function TChamber.GetSecondsTargetTimeOut: LongWord;
begin
  result := fSecondsTargetTimeOut;
end;

function TChamber.GetStabMode: TChamberStabMode;
begin
  result := fStabMode;
end;

function TChamber.GetTargetTempr: double;
begin
  result := fTargetTempr;
end;

function TChamber.getSpeed: double;
begin
  result := fMovVariance.Variance;
end;

procedure TChamber.onResultDataPacket(const Buffer; Count : Word);
var
vCurrTempr : Double;
//vBuff : PByteArray;
vStr : String;
//i : byte;
begin
  {if Assigned(ChmbLogger) and (ChmbLogger.LevelEnabled(lPortRead)) then
    ChmbLogger.Log(lPortRead, Format('COM%d %s', [self.Port, BytesToHexStr(Buffer, Count)] )); }
  try
    //vBuff := PByteArray(Buffer);
    {SetLength(vStr, Count);
    Move(Buffer, vStr[1], Count); }
    if (fCommandsQueue.Count > 0) then
    begin
      if fCommandsQueue.First^.Command in [FT..ST, SY..RS, RT] then
      begin
        // ввести обработку этих команд
        fCommandsQueue.Delete(0);
      end;
    end else
    if fGetTemprSended then
    begin
      if Length(fRxBuffer) < Count then
        exit;
      FillChar(fRxBuffer[0], SizeOf(fRxBuffer), 0);
      Move(Buffer, fRxBuffer[0], Count);
      vStr := FEncoding.GetString(fRxBuffer, 0, Count);
      fGetTemprSended := false;
      if not fConnected then
      begin
        SetStateConnected(TryStrToFloat(vStr, fCurrentTempr, fPMLocalFormatSettings));
        if (not fTryConnect) and (not fConnected) then
        begin
          inc(fLagCount);
          fLostConnect := fLagCount > 10;
        end else if fConnected then
          fLagCount := 0;
        exit;
      end else
      if ((not TryStrToFloat(vStr, vCurrTempr, fPMLocalFormatSettings)
      or (not SameValue(fCurrentTempr, vCurrTempr, 20.0)))) then
      begin
        inc(fLagCount);
        fLostConnect := fLagCount > 10;
        SetStateConnected(fLostConnect);
         if Assigned(ChmbLogger) then
          ChmbLogger.Log(lWarning,
          'Лаг при расшифровке значения температуры');

        exit;
      end;

      fLagCount := 0;
      SetStateConnected(true);
      if (fStabMode = mChallenge) or (SameValue(fCurrentTempr, vCurrTempr, 5.0)) then
      begin
        fCurrentTempr := vCurrTempr;
        fMovVariance.SetValue(fCurrentTempr);
        if (fStabMode = mChallenge) then
          fTargetTempr := fCurrentTempr;
        if assigned(fOnTempr) then
          fOnTempr(self);
      end;
    end;
  finally
    StopTimeOut;
    if (not fLostConnect) then
      CallChallenge(fChallengeInterval);
  end;
end;

procedure TChamber.SetStateConnected(const value: boolean);
begin
  if fConnected = value then
    exit;
  fConnected := value;
  if fConnected then
  begin
    fLostConnectSignaled := false;
    StopTimeOut;
    if fTryConnect then
      fTryConnect := false;
    if fLostConnect then
      fLostConnect := false;
    fChallengeTimeOutInterval := 3000 ;
    if (assigned(fOnConnect)) then
      fOnConnect(self);
  end
  else
  begin
    if (assigned(fOnDisconnect)) then
      fOnDisconnect(self);
  end;
end;

procedure TChamber.SendData;
begin
  try
    PortWrite(FTxBuffer, FTxCount);
    {if Assigned(ChmbLogger) and (fCommandsQueue.First^.Command <> T) and ChmbLogger.LevelEnabled(lPortWrite) then
      ChmbLogger.Log(lPortWrite, Format('COM%d %s',[self.Port, BytesToHexStr(fTxBuffer, FTxCount)])); }
    FTxCount := 0;
  except
    on E : exception do
    if Assigned(ChmbLogger) then
      ChmbLogger.Log(lException, E);
  end;
end;

function TChamber.SendSetTempr(const ATempr : double; const ASeconds :Cardinal) : boolean;
var
vMin : Word;
vSec : byte;
vStr : AnsiString;
begin
  result := false;
  SecondToMinSec(ASeconds, vMin, vSec);
  //vTemprStr := Format('T%5.2n, %d.%d'+#13#10, ATempr, vMin, vSec, fPMLocalFormatSettings);
  System.AnsiStrings.FmtStr(vStr, 'T%5.2n, %d.%d'+#13#10,
     [ATempr, vMin, vSec], fPMLocalFormatSettings);
  FTxCount := 0;
  if Length(vStr) <= Length(fTxBuffer) then
  begin
    if Assigned(ChmbLogger) then
    if ChmbLogger.LevelEnabled(lPortWrite)then
      ChmbLogger.Log(lPortWrite, Format('COM%d %s',[self.Port, vStr]));
    move(vStr[1], fTxBuffer[0], Length(vStr));
    FTxCount := Length(vStr);
    SendData;
    result := true;
  end;
end;

const
cChamberStrCommands : array[TChamberCommands] of AnsiString =
('T%s, %d.%d'+#13#10, 'FT'+#13#10, 'LT'+#13#10,
'ST'+#13#10, 'KT'+#13#10, 'O'+#13#10, 'SY'+#13#10,
'SX'+#13#10, 'RS'+#13#10, 'GTL'+#13#10, 'RT'+#13#10);

C_GetTempr_Str: array [0..3] of byte = ($41, $54, $0D, $0A);
//C_GetTempr_Str: AnsiString = 'AT'+#13#10;

procedure TChamber.DoChallenge(Sender: TObject);
  procedure SendGetTempr;
  begin
    PortWrite(C_GetTempr_Str, Length(C_GetTempr_Str));
    {if Assigned(ChmbLogger) and ChmbLogger.LevelEnabled(lPortWrite) then
      ChmbLogger.Log(lPortWrite, Format('COM%d %s',[self.Port, BytesToHexStr(C_GetTempr_Str, Length(C_GetTempr_Str))])); }
  end;
  procedure SendCommand(ACommand: pChamberCommandItem);
  begin
    FTxCount := 0;
    if (ACommand^.Command = T) then
      exit;
    move(cChamberStrCommands[ACommand^.Command][1], fTxBuffer[0], Length(cChamberStrCommands[ACommand^.Command]));
    FTxCount := Length(cChamberStrCommands[ACommand^.Command]);
    SendData;
  end;
begin
  if ((not fConnected) and (not fLostConnect) and (assigned(fOnStartChallenge))) then
    fOnStartChallenge(self);
  if fConnected and (fCommandsQueue.Count > 0) then
  begin
    case fCommandsQueue.First^.Command of
      FT..ST, SY..RS, RT :
      begin
        SendCommand(fCommandsQueue.First);
        SetTimeOutMode(fChallengeTimeOutInterval);
      end;
      T:
      begin
        case fStabMode of 
          mChallenge,
          mCharge,
          mWatch,
          mChargeTimeOut :
          begin
            if SendSetTempr(fCommandsQueue.First^.TargetTempr, fCommandsQueue.First^.SecondsChargeTime) then
            begin
              if (fStabMode <> mCharge) then
                fChargeTimeStart := Now;
              if (fStabMode in [mChallenge, mWatch]) then 
                fStabMode := mCharge;
              fTargetTempr := fCommandsQueue.First^.TargetTempr;
              fSecondsTargetTime := fCommandsQueue.First^.SecondsChargeTime;
              fSecondsTargetTimeOut := fCommandsQueue.First^.SecondsTimeOut;
              if assigned(fOnTemprCharge) then
                fOnTemprCharge(self);
            end else if Assigned(ChmbLogger) then

                ChmbLogger.Log(lWarning,
                'Не удалось отправить команду установки температуры');
            {Подумать, что делать дальше:
            не удаляя из очереди, отправлять повторно до посинения
            или же что-то еще}
          end;
          mInRange,
          mInRangeTime:
          begin
            if not SameValue(fTargetTempr, fCommandsQueue.First^.TargetTempr, 0.05) then
            begin
              if SendSetTempr(fCommandsQueue.First^.TargetTempr, fCommandsQueue.First^.SecondsChargeTime) then
              begin
                fStabMode := mCharge;
                fChargeTimeStart := Now;
                fTargetTempr := fCommandsQueue.First^.TargetTempr;
                fSecondsTargetTime := fCommandsQueue.First^.SecondsChargeTime;
                fSecondsTargetTimeOut := fCommandsQueue.First^.SecondsTimeOut;
                if assigned(fOnTemprCharge) then fOnTemprCharge(self);
              end else if Assigned(ChmbLogger) then
              begin
                ChmbLogger.Log(lWarning,
                'Не удалось отправить команду установки температуры');
              end;
            end else if Assigned(ChmbLogger) then
            begin
                ChmbLogger.Log(lDebug,
                'Новая температура соотвествует заданной. Нет нужды писать в порт ТК');
            end;
          end;
        end;           
        fCommandsQueue.Delete(0);
        CallChallenge(fChallengeInterval);
      end;
      KT, O:
      begin
        SendCommand(fCommandsQueue.First);
        fStabMode := mChallenge;
        fCommandsQueue.Delete(0);
        CallChallenge(fChallengeInterval);
      end;
      GTL:
      begin
        SendCommand(fCommandsQueue.First);
        Sleep(50);
        fCommandsQueue.Clear;
        PortClose;
        //SetStateConnected(false);
      end;
    end;
  end
  else
  begin
    SendGetTempr;
    SetTimeOutMode(fChallengeTimeOutInterval);
    fGetTemprSended := true;
  end;
end;
  
procedure TChamber.SendTempr(const Tempr: double; const SecondsChargeTime: LongWord = 0);
var
Command : TChamberCommandItem;
vSecondsChargeTime: LongWord;
begin
  vSecondsChargeTime := SecondsChargeTime;
  if vSecondsChargeTime = 0 then
    vSecondsChargeTime := 60;
  Command.Command := T;
  Command.TargetTempr := Tempr;
  Command.SecondsChargeTime := vSecondsChargeTime;
  Command.SecondsTimeOut := vSecondsChargeTime + fTimeOutCoeff;

  fCommandsQueue.Add(Command);
  if Assigned(ChmbLogger) then
    ChmbLogger.Log(lDebug,
    Format('Подготовка команды T %f°C, время %d сек, таймаут %d сек' ,
         [Command.TargetTempr, Command.SecondsChargeTime, Command.SecondsTimeOut]));
end;

procedure TChamber.SendTemperatureOff;
var
Command : TChamberCommandItem;
begin
  Command.Command := KT;
  fCommandsQueue.Add(Command);
end;

procedure TChamber.SendChamberOff;
var
Command : TChamberCommandItem;
begin
  Command.Command := O;
  fCommandsQueue.Add(Command);
end;

procedure TChamber.SendDisconnect;
var
Command : TChamberCommandItem;
begin
  fCommandsQueue.Clear;
  Command.Command := GTL;
  fCommandsQueue.Add(Command);
  {move(cChamberStrCommands[GTL], fTxBuffer[0], Length(cChamberStrCommands[GTL]));
  FTxCount := Length(cChamberStrCommands[GTL]);
  SendData; }
end;

procedure TChamber.CheckState(Sender : TObject);
var CurrDiff : double;
begin
  case fStabMode of
    mCharge..mWatch:
    begin
      CurrDiff := abs(fCurrentTempr - fTargetTempr);  
      if fChargeTimeOutSignaled then
        fChargeTimeOutSignaled := false;
    end;
  end;
  case fStabMode of
    mChallenge:
    begin
      if fChargeTimeOutSignaled then
        fChargeTimeOutSignaled := false;
    end;
    mCharge:
    begin
      if (fTemprWatcher.TemprIsInRange(CurrDiff)) then
        fStabMode := mInRange
      else
      begin
        if (SecondsBetween(Now, fChargeTimeStart)  > fSecondsTargetTimeOut) then
          fStabMode := mChargeTimeOut;
      end;
    end;
    mInRange:
    begin
      fTemprWatcher.StartExposure;
      fStabMode := mInRangeTime;
    end;
    mInRangeTime:
    begin
      if (fTemprWatcher.TemprIsInRange(CurrDiff)) then
      begin
        if (fTemprWatcher.isTimeExpired) then
        begin
          fStabMode := mWatch;
          fTargetTimeOutCount := 0;
          if assigned(fOnTemprInTarget) then
            fOnTemprInTarget(self);
        end;
      end else
      begin
        if (SecondsBetween(Now, fChargeTimeStart)  > fSecondsTargetTimeOut) then
          fStabMode := mChargeTimeOut;
      end;
    end;
    mWatch:
    begin
      if not (fTemprWatcher.TemprIsInRange(CurrDiff)) then
      begin
        fStabMode := mCharge;
        inc(fTargetTimeOutCount);
        SendTempr(fTargetTempr, fSecondsTargetTime);
        if assigned(fOnTemprOutOfTarget) then
            fOnTemprOutOfTarget(self);
      end;
    end;
    mChargeTimeOut:
    begin
      if assigned(fOnChargeTimeOut) then
      begin
        if fTargetTimeOutCount > 1 then
        begin
          if not fChargeTimeOutSignaled then
          begin
            fChargeTimeOutSignaled := true;
            fOnChargeTimeOut(self);
          end;
        end else
        begin
          inc(fTargetTimeOutCount);
          fStabMode := mCharge; 
          SendTempr(fTargetTempr, fSecondsTargetTime);
        end;
      end
      else
      begin
        fStabMode := mChallenge;
        SendTempr(fTargetTempr, fSecondsTargetTime);
      end;
    end;
  end;
end;

procedure TChamber.SetTargetTempr(const AValue: double);
begin
  fTargetTempr := AValue;
end;

{procedure TChamber.SetCurrTempr(const AValue: double);
begin
  fCurrentTempr := AValue;
end; }

{$ENDREGION}

{ TChamberCommandQueue }

function TChamberCommandQueue.Add(ACommand: TChamberCommandItem): integer;
begin
  result := inherited Add(ACommand.MemAlloc);
end;

procedure TChamberCommandQueue.Clear;
begin
  while count > 0 do
      delete(count -1);
    inherited;
end;

function TChamberCommandQueue.First: pChamberCommandItem;
begin
  result := Get(0);
end;

function TChamberCommandQueue.Get(Index: Integer): pChamberCommandItem;
begin
  Result := pChamberCommandItem(inherited Get(Index));
end;

function TChamberCommandQueue.Last: pChamberCommandItem;
begin
   result := Get(Count - 1);
end;

procedure TChamberCommandQueue.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
      Dispose(Ptr);
    inherited Notify(Ptr, Action);
end;


{$REGION ' MovAverage '}
  
constructor TMovVariance.Create(Capacity: LongWord);
var i : integer;
begin
  inherited Create;
  Setlength(fItems, Capacity);
  for I := Low(fItems) to High(fItems) do
    fItems[i] := -500;
end;

destructor TMovVariance.Destroy;
begin
  Setlength(fItems, 0);
  fItems := nil;
  inherited Destroy;
end;

procedure TMovVariance.SetValue(value: double);
var i : integer;
    val1 : double;
begin
  for i := Low(fItems) to High(fItems) do
    if (abs(fItems[i] -500) < 0.01) then fItems[i] := value;

  val1 := Mean(fItems);
  for I := Low(fItems) to High(fItems) - 1 do
      fItems[i] := fItems[i+1];
  fItems[High(fItems)] := value;
  fAverageTempr := Mean(fItems);
  fVariance := abs(val1 - fAverageTempr);
end;

  
{$ENDREGION}


{ TRangeData }

function TWatcher.TemprIsInRange(const aDifferent: double): boolean;
begin
  result := (aDifferent < fTemprRangeValue);
end;

function TWatcher.SecondElapsed : LongWord;
begin
  result := SecondsBetween(now, fStartingTime);
end;

function TWatcher.isTimeExpired: boolean;
begin
  result := (SecondElapsed >= fWatchTime);
end;

procedure TWatcher.SetRange(Range: double);
begin
  fTemprRangeValue := EnsureRange(Range, 0.1, 100);
end;

procedure TWatcher.SetWatchTime(ASecons: LongWord);
begin
  fWatchTime := EnsureRange(ASecons, 1, 3600);
end;

procedure TWatcher.StartExposure;
begin
 fStartingTime := now;
end;

{ TTermoProfile }

function TTermoProfile.NewItem : pTermoPoint;
begin
  New(Result);
  inherited Add(Result);
end;

procedure TTermoProfile.Clear;
begin
  while count > 0 do
    delete(count -1);
  inherited Clear;
end;

procedure TTermoProfile.ClearTP;
begin
  Clear;
end;

constructor TTermoProfile.Create(const AProcess : TChamberProcess);
begin
  inherited Create;
  fProcess := AProcess;
end;

function TTermoProfile.First: pTermoPoint;
begin
  Result := Get(0);
end;

function TTermoProfile.Get(Index: Integer): pTermoPoint;
begin
  Result := pTermoPoint(inherited Get(Index));
end;

function TTermoProfile.GetPrevTempr: double;
begin
  result := fPrevTempr;
end;

function TTermoProfile.Last: pTermoPoint;
begin
  Result := Get(Count - 1);
end;

procedure TTermoProfile.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(Ptr);
  inherited Notify(Ptr, Action);
end;

function TTermoProfile.IndexOf(const ATempr: double): integer;
begin
  Result := 0;
  while (Result < Count) do
  begin
    if SameValue(Items[Result].Tempr, ATempr, fProcess.Chamber.fChamberIniParams.DeltaTempr) then
      Break;
    inc(Result);
  end;
  if Result = Count then
    Result := -1;
end;

function TTermoProfile.PointExists(const ATempr: double): boolean;
begin
  result := IndexOf(ATempr) <> -1;
end;

function TTermoProfile.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TTermoProfile.SetPrevTempr(const ATempr: double);
begin
  fPrevTempr := ATempr;
end;

function SortTpByAsc(Item1, Item2: Pointer): Integer;
var
vLeft, vRigth : pTermoPoint;
begin
  Result := 0;
  if ((not Assigned(Item1))
  or (not Assigned(Item2))) then
    Exit;
  vLeft := pTermoPoint(Item1);
  vRigth := pTermoPoint(Item2);
  if SameValue(vRigth.Tempr, vLeft.Tempr, 0.05) then
    result := 0
  else if vRigth.Tempr < vLeft.Tempr then
    Result := 1
  else
    Result := -1;
end;

procedure TTermoProfile.SortTP;
var
vChamberPoint : pTermoPoint;
vStr : string;
begin
  Sort(SortTpByAsc);
  if Assigned(ChmbLogger) then
  begin
    vStr := 'Termoprofile points: ';
    for vChamberPoint in self do
      vStr := Format('%s (value:%f°C, speed:%f°C/min, expos:%d)',
        [vStr, vChamberPoint.Tempr, vChamberPoint.Speed, vChamberPoint.Expos]);
    if Assigned(ChmbLogger) then
      ChmbLogger.Log(lInfo, vStr);
  end;
end;

function TTermoProfile.TermoProfile_Count: Word;
begin
  Result := Count;
end;

function TTermoProfile.TermoProfile_Get(Index: Integer): pTermoPoint;
begin
  Result := Get(Index);
end;

function TTermoProfile._AddRef: Integer;
begin
  result := -1;
end;

function TTermoProfile._Release: Integer;
begin
  result := -1;
end;

procedure TChamberProcessIniParams.LoadParamsFromFile;
var  
f:TIniFile;
vPMLocalFormatSettings : TFormatSettings;
begin
  vPMLocalFormatSettings := TFormatSettings.Create(1033);
  f := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance), '.ini'));
  try
    Speed := Vodopad.CustomIniHelpers.ReadFloat(f,'ChamberProcess', 'Speed', 1, vPMLocalFormatSettings);
    if Speed < 0.1 then
      Speed := 0.1;
  finally
    FreeAndNil(f);
  end;
end;


{ TChamberProcess }

constructor TChamberProcess.Create(AOwner : TComponent; AFunct : pLoggerFunct);
begin
  InitLogger(AFunct, ChmbLogger);
  inherited;
  fUiHandle := 0;
  fFormInstance := nil;
  fIniParams.LoadParamsFromFile;
  fProcessWorked := false;
  fEventSubscribers := TCustomObjEventList.Create;
  fChamber := TChamber.Create(nil);
  fChamber.OnConnect := ChamberConnected; 
  fChamber.onTryConnectiontFail := ChamberTryConnectFail;
  fChamber.OnDisconnect := ChamberDisconnected;
  fChamber.OnTempr := ChamberTempr;
  fChamber.OnTemprInTarget := ChamberTemprStabilized; 
  fChamber.OnChargeTimeOut := ChamberStabilizedTimeOut;
  fChamber.OnTemprOutOfTarget := ChamberTemprOutOfTarget;
  fChamber.OnTemprCharge := TemprCharge;
  fChamber.OnConnectionLost := ChamberConnectionLost;
  fTermoProfile := TTermoProfile.Create(Self);
  fCurrentPoint := 0;
  StopExposure;

  FWindowHandle := System.Classes.AllocateHWND(MessageHandler);
  fExposureWatcher.StartExposure;
end;

destructor TChamberProcess.Destroy;
var
vFormOnDestroyEvent : IFormOnDestroyEvent;
begin
  if Supports(fFormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
  begin
    vFormOnDestroyEvent.OnDestroy := nil;
    vFormOnDestroyEvent := nil;
  end;
  fFormInstance := nil;
  FreeFormLibrary;
  System.Classes.DeallocateHWND(FWindowHandle);
  fTermoProfile.Clear;
  FreeAndNil(fTermoProfile);
  SendDisconnectAndWait;
  FreeAndNil(fChamber);
  FreeAndNil(fEventSubscribers);
  inherited Destroy;
  ChmbLogger := nil;
end;

procedure TChamberProcess.FreeFormLibrary;
var
vUnloadProc :procedure;
begin
  if (fUiHandle > 0) then
  try
    @vUnloadProc := GetProcAddress(fUiHandle, 'Finalize');
    if Assigned(vUnloadProc) then
      vUnloadProc;
  finally
    FreeLibrary(fUiHandle);
    fUiHandle := 0;
  end;
end;

procedure TChamberProcess.OnFormDestroy(Sender: TObject);
begin
  fFormInstance := nil;
  Winapi.Windows.PostMessage(FWindowHandle, OnFormClose, 0, 0);
end;

procedure TChamberProcess.EventMethodAdd(const AMethod: EventBusInterface.TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TChamberProcess.EventMethodRemove(const AMethod: EventBusInterface.TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

function TChamberProcess.StartChamberModule(sPort : byte) : boolean;
begin
  result := fChamber.TryConnect(sPort);
end;

procedure TChamberProcess.SendDisconnectAndWait;
begin
  fDoDisconnect := true;
  if Assigned(fChamber) then
  try
    if (fChamber.Connected) then
    begin
      fChamber.SendDisconnect;
      fChamber.DoChallenge(nil);
    end;
  finally
    FreeAndNil(fChamber);
  end;
end;

procedure TChamberProcess.ChamberConnected(Sender : TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_Detected, 0, 0);
end;

procedure TChamberProcess.ChamberConnectionLost(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_ConnectionLost, 0, 0);
end;

procedure TChamberProcess.ChamberDisconnected(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_Disconnected, 0, 0);
end;

procedure TChamberProcess.TemprCharge(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnTempr_Charge, 0, 0);
end;

function TChamberProcess.EOF: boolean;
begin
  result := (fTermoProfile.Count = 0)
   or (fCurrentPoint = fTermoProfile.Count -1);
end;

function TChamberProcess.BOF: boolean;
begin
  result := (fCurrentPoint = 0);
end;

function TChamberProcess.PointExists(index: integer): boolean;
begin
  result := (fTermoProfile.Count > 0) and (index < fTermoProfile.Count);
end;

procedure TChamberProcess.GotoPoint(AIndex: integer);
var
vSecondsChargeTime : LongWord;
begin
  if PointExists(AIndex)  then
  begin
    fIniParams.LoadParamsFromFile;
    vSecondsChargeTime := trunc(abs(fTermoProfile[AIndex].Tempr - fChamber.CurrTempr) / fIniParams.Speed) * 60;
    fTermoProfile.PrevTempr := fChamber.TargetTempr;
    fChamber.fStabMode := mChallenge;
    fChamber.SendTempr(fTermoProfile[AIndex].Tempr, vSecondsChargeTime);
    fExposureWatcher.StartExposure;
  end else if Assigned(ChmbLogger) then
      ChmbLogger.Log(lError,
      Format('В термопрофиле нет точки с индексом %d ',[AIndex]));

end;

function TChamberProcess.IsExposureDone: boolean;
begin
  result := (fChamber.StabMode = mWatch)
  and (fExposureWatcher.isTimeExpired);// and (not fIsInExposure);
end;

function TChamberProcess.ExposureSecondElapsed : LongWord;
begin
  result := fExposureWatcher.SecondElapsed;
end;

function TChamberProcess.GetCurrentPointIdx: integer;
begin
  Result := fCurrentPoint;
end;

function TChamberProcess.GetCurrPointTempr: double;
begin
  result := -1000;
  if PointExists(fCurrentPoint)
  {and (fChamber.StabMode in [mCharge, mInRange, mInRangeTime, mWatch])}  then
    result := fTermoProfile[fCurrentPoint].Tempr;
end;

function TChamberProcess.GetCurrTempr: double;
begin
  result := fChamber.CurrTempr;
end;

function TChamberProcess.GetIsInExposure: boolean;
begin
  Result := fIsInExposure;
end;

function TChamberProcess.GetPointsCount: Word;
begin
  Result := fTermoProfile.Count;
end;

function TChamberProcess.GetProcessWorked: boolean;
begin
  Result := fProcessWorked;
end;

function TChamberProcess.GetTargetTempr: double;
begin
  result := fChamber.TargetTempr;
end;

function TChamberProcess.GetStabMode: TChamberStabMode;
begin
  if Assigned(fChamber) then
    Result := fChamber.StabMode;
end;

function TChamberProcess.GetStartingTime : TDateTime;
begin
  result := fExposureWatcher.StartingTime;
end;

procedure TChamberProcess.ManualChamberOff;
begin
  fChamber.SendChamberOff; 
  SetCurrentPoint(0);
end;

procedure TChamberProcess.ManualChargeTempr(const Tempr: double; const Minutes: LongWord);
var
vMinutesChargeTime : LongWord;
begin
  fIniParams.LoadParamsFromFile;
  fTermoProfile.PrevTempr := fChamber.CurrTempr;
  vMinutesChargeTime := Minutes;
  if vMinutesChargeTime = 0 then
    vMinutesChargeTime := trunc(abs(Tempr - fChamber.CurrTempr) / fIniParams.Speed);
  if vMinutesChargeTime < 1 then
    vMinutesChargeTime := 1;
  fChamber.fStabMode := mChallenge;
  fChamber.SendTempr(Tempr, vMinutesChargeTime * 60);
end;

procedure TChamberProcess.ManualChargeTemprBySpeed(const Target, SpeedDegMin: double);
var
vMinutesChargeTime : LongWord;
begin
  fIniParams.LoadParamsFromFile;
  fTermoProfile.PrevTempr := fChamber.CurrTempr;
  if SpeedDegMin < 0.1 then
    vMinutesChargeTime := trunc(abs(Target - fChamber.CurrTempr) / fIniParams.Speed)
  else
    vMinutesChargeTime := trunc(abs(Target - fChamber.CurrTempr) / SpeedDegMin);
  if vMinutesChargeTime < 1 then
    vMinutesChargeTime := 1;
  fChamber.fStabMode := mChallenge;
  fChamber.SendTempr(Target, vMinutesChargeTime * 60);
end;

procedure TChamberProcess.ManualTemperatureOff;
begin
  fChamber.SendTemperatureOff;
  SetCurrentPoint(0);
end;

function TChamberProcess.MaxDeltaTempr: double;
begin
  result := fChamber.TemprWatcher.RangeValue;
end;

procedure TChamberProcess.MessageHandler(var message: TMessage);
var
vFrm : ISA4350MC;
begin
  case message.Msg of
    OnFormClose:
    begin
      FreeFormLibrary;
    end;
    OnChamber_Detected:
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lDebug,'OnChamber_Detected');
      BlockForm(fProcessWorked or (not Chamber.Connected));
      fEventSubscribers.Execute(Self, C_OnChamber_Detected, nil);
      if Supports(fFormInstance, ISA4350MC, vFrm) then
        vFrm.SetMessageText(1, 'ОК');
    end;
    OnChamber_TryConnectionFail:
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lDebug,'OnChamber_TryConnectionFail');
      BlockForm(fProcessWorked or (not Chamber.Connected));
      fEventSubscribers.Execute(Self, C_OnChamber_TryConnectionFail, nil);
    end;
    OnChamber_ConnectionLost:
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lDebug,'OnChamber_ConnectionLost');
      BlockForm(fProcessWorked or (not Chamber.Connected));
      fEventSubscribers.Execute(Self, C_OnChamber_ConnectionLost, nil);
      if Supports(fFormInstance, ISA4350MC, vFrm) then
      begin
        vFrm.SetSpeed(0);
        vFrm.SetCurrentTempr(0);
        vFrm.SetTargetTempr(0);
        vFrm.SetMessageText(1, 'Соединение с камерой потеряно');
      end;
    end;
    OnChamber_Disconnected:
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lDebug,'OnChamber_Disconnected');
      BlockForm(fProcessWorked or (not Chamber.Connected));
      fEventSubscribers.Execute(Self, C_OnChamber_Disconnected, nil);
      if Supports(fFormInstance, ISA4350MC, vFrm) then
      begin
        vFrm.SetLigthState(lOff);
        vFrm.SetSpeed(0);
        vFrm.SetCurrentTempr(0);
        vFrm.SetTargetTempr(0);
        vFrm.SetMessageText(1, 'Сессия работы с камерой завершена');
      end;
    end;
    OnChamber_Tempr:
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lTrace,
        Format('Текущая температура %f°C', [fChamber.CurrTempr]));
      fEventSubscribers.Execute(Self, C_OnChamber_Tempr, nil);
      if fIsInExposure and IsExposureDone then
      begin
        StopExposure;
        fEventSubscribers.Execute(Self, C_OnTemprStabilizedAndExposure, nil);
      end;
      if Supports(fFormInstance, ISA4350MC, vFrm) then
      begin
        if fChamber.Connected then
        begin
          vFrm.SetSpeed(Chamber.Speed);
          vFrm.SetCurrentTempr(Chamber.CurrTempr);
          vFrm.SetTargetTempr(Chamber.TargetTempr);
          case Chamber.StabMode of
            mChallenge:
            begin
              vFrm.SetMessageText(2, 'Обмен');
              vFrm.SetLigthState(lOff);
            end;
            mCharge:
            begin
              vFrm.SetMessageText(2, 'Набор температуры');
              vFrm.SetLigthState(lOff);
            end;
            mInRange:
            begin
              vFrm.SetMessageText(2, 'В диапазоне');
              vFrm.SetLigthState(lOff);
            end;
            mInRangeTime:
            begin
              vFrm.SetMessageText(2,'Отсчет времени в диапазоне');
              vFrm.SetLigthState(lOff);
            end;
            mWatch :
            begin
              vFrm.SetMessageText(2,'Контроль диапазона');
              vFrm.UpdateClock(GetStartingTime);
              if IsExposureDone then
                vFrm.SetLigthState(lExposure)
              else
                vFrm.SetLigthState(lWatch);
            end;
          end;
        end else
        begin
          vFrm.SetLigthState(lOff);
          vFrm.SetSpeed(0);
          vFrm.SetCurrentTempr(0);
          vFrm.SetTargetTempr(0);
          vFrm.SetMessageText(2,'');
        end;
        vFrm.SetMessageText(0, Format('cp:%d', [CurrentPoint]));
      end;
    end;
    OnChamber_TemprStabilized:
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lDebug,
        Format('Температура %f°C стабилизировалась', [fChamber.CurrTempr]));
      fExposureWatcher.StartExposure;
      fEventSubscribers.Execute(Self, C_OnChamber_TemprStabilized, nil);
      if Supports(fFormInstance, ISA4350MC, vFrm) then
        vFrm.SetLigthState(lWatch);
    end; 
    OnChamber_StabilizedTimeOut:
    begin
      {if (fOnStabilizationTimeOut.Count = 0) and (PointExists(fCurrentPoint)) then
        fChamber.SendTempr(fTermoProfile[fCurrentPoint].Tempr, 60)
      else }
        fEventSubscribers.Execute(Self, C_OnChamber_StabilizedTimeOut, nil);
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lWarning, 'Таймаут стабилизации');
    end;
    OnChamber_TemprOutOfTarget:
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lWarning,
        'Выход за пределы диапазона стабилизации');
      {if (fOnTemprOutOfTarget.Count = 0) and PointExists(fCurrentPoint) then
        fChamber.SendTempr(fTermoProfile[fCurrentPoint].Tempr, 60)
      else  }
      fEventSubscribers.Execute(Self, C_OnChamber_TemprOutOfTarget, nil);
    end;
    OnTempr_Charge:
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lDebug,
        Format('Камера в режиме набора температуры %f°C. Заданное время %d сек',
        [fChamber.TargetTempr, fChamber.SecondsTargetTime]));
      fEventSubscribers.Execute(Self, C_OnTempr_Charge, nil);
    end;
  end;
  vFrm := nil;
end;

function TChamberProcess.FirstPoint : boolean;
begin
  result := PointExists(0);
  if result then
     SetCurrentPoint(0);
end;

function TChamberProcess.NextPoint : boolean;
begin
  result := PointExists(fCurrentPoint + 1);
  if result then
    SetCurrentPoint(fCurrentPoint + 1);
end;

function TChamberProcess.PrevPoint : boolean;
begin
  result := PointExists(fCurrentPoint - 1);
  if result then
    SetCurrentPoint(fCurrentPoint - 1);
end;

procedure TChamberProcess.ProcessCurrent;
begin
  if PointExists(fCurrentPoint) then
    GotoPoint(fCurrentPoint)
  else
   fEventSubscribers.Execute(Self, C_OnProfileEnded, nil);
end;

function TChamberProcess.QueryInterface(const IID: TGUID; out Obj): HResult;
var
vOut : IInterface;
begin
  result := inherited QueryInterface(IID, Obj);

  if Result = S_OK then
    exit;
  vOut := nil;
  if assigned(fChamber) then
  begin
    vOut := fChamber;
    Result := vOut.QueryInterface(IID, Obj);
  end;     
  if Result = S_OK then
    exit;
  vOut := nil;
  if assigned(fTermoProfile) then
  begin
    vOut := fTermoProfile;
    Result := vOut.QueryInterface(IID, Obj);
  end;
end;

procedure TChamberProcess.ChamberTempr(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_Tempr, 0, 0);
end;

procedure TChamberProcess.ChamberTemprOutOfTarget(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_TemprOutOfTarget, 0, 0);
end;

procedure TChamberProcess.ChamberTemprStabilized(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_TemprStabilized, 0, 0);
end;

procedure TChamberProcess.ChamberStabilizedTimeOut(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, onChamber_StabilizedTimeOut, 0, 0);
end;

procedure TChamberProcess.ChamberTryConnectFail(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_TryConnectionFail, 0, 0);
end;

procedure TChamberProcess.SetCurrentPoint(index: integer);
begin
  if PointExists(index) then
  begin
    fCurrentPoint := index;
  end
  else
    fCurrentPoint := 0;
end;

procedure TChamberProcess.SetProcessWorked(const Value: boolean);
begin
  if (fProcessWorked = Value) then
    exit;
  fProcessWorked := Value;
  BlockForm(fProcessWorked or (not Chamber.Connected));
end;

procedure TChamberProcess.ShowForm(AOwner: TComponent);
var
vCreateFormInstance : TFormCreateFunct;
vInitLibProc, vUnloadProc :procedure;
vShowMdiForm : IShowMdiForm;
vFormToFront : IFormToFront;
vFormOnDestroyEvent : IFormOnDestroyEvent;
vFrm : ISA4350MC;
begin
  if Assigned(fFormInstance)
  and Supports(fFormInstance, IFormToFront, vFormToFront) then
  begin
    vFormToFront.BringToFront;
    Exit;
  end;
  fUiHandle := SafeLoadLibrary(ExtractFilePath(ParamStr(0)) + 'SA4350ui.bpl',
        SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
    if (fUiHandle = 0) then
      Exit;
    @vCreateFormInstance := GetProcAddress(fUiHandle, 'CreateForm');
    if not Assigned(vCreateFormInstance) then
    begin
      FreeLibrary(fUiHandle);
      fUiHandle := 0;
      Exit;
    end;
    @vInitLibProc := GetProcAddress(fUiHandle, 'Initialize');
    if Assigned(vInitLibProc) then
       vInitLibProc;
    fFormInstance := vCreateFormInstance(AOwner, SetTemprBtnClick, TemperatOffBtnClick, ChamberOffBtnClick);
    if Supports(fFormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
       vFormOnDestroyEvent.OnDestroy := OnFormDestroy
    else
      raise Exception.Create('sa4350 form not supports destroy event interface');
    if Supports(fFormInstance, IShowMdiForm, vShowMdiForm) then
    begin
      vShowMdiForm.Show;
      if Supports(fFormInstance, ISA4350MC, vFrm) then
      begin
        vFrm.BlockForm(fProcessWorked or (not Chamber.Connected));
        if (not Chamber.Connected) then
          vFrm.SetMessageText(1, 'Камера не подключена');
        vFrm := nil;
      end;
    end
    else
      raise Exception.Create('sa4350 form not supports show interface');
  except
    if (fUiHandle > 0) then
    try
      if Assigned(fFormInstance) then
        FreeAndNil(fFormInstance);
      @vUnloadProc := GetProcAddress(fUiHandle, 'Finalize');
      if Assigned(vUnloadProc) then
          vUnloadProc;
    finally
      FreeLibrary(fUiHandle);
      fUiHandle := 0;
      fFormInstance := nil;
    end;
  end;
end;

procedure TChamberProcess.SetTemprBtnClick(Sender: TObject);
var
vFrm : ISA4350MC;
begin
  if not Supports(fFormInstance, ISA4350MC, vFrm) then
    Exit;
  if Chamber.Connected then
     ManualChargeTempr(vFrm.GetTargetTempr, vFrm.GetTargetTime)
  else
    vFrm.SetMessageText(2, 'Камера не доступна для ручного управления');
  vFrm := nil;
end;

procedure TChamberProcess.TemperatOffBtnClick(Sender: TObject);
var
vFrm : ISA4350MC;
begin
  if not Supports(fFormInstance, ISA4350MC, vFrm) then
    Exit;
  if Chamber.Connected then
     ManualTemperatureOff
  else
    vFrm.SetMessageText(2, 'Камера не доступна для ручного управления');
  vFrm := nil;
end;

procedure TChamberProcess.ChamberOffBtnClick(Sender: TObject);
var
vFrm : ISA4350MC;
begin
  if not Supports(fFormInstance, ISA4350MC, vFrm) then
    Exit;
  if Chamber.Connected then
     ManualChamberOff
  else
    vFrm.SetMessageText(2, 'Камера не доступна для ручного управления');
  vFrm := nil;
end;

function TChamberProcess.DefaultSpeed: double;
begin
  Result := fIniParams.Speed;
end;

procedure TChamberProcess.BlockForm(ADoBlock: boolean);
var
vFrm : ISA4350MC;
begin
  if not Supports(fFormInstance, ISA4350MC, vFrm) then
    Exit;
  vFrm.BlockForm(ADoBlock);
  vFrm := nil;
end; 

procedure TChamberProcess.ClearTermoProfile;
begin
  if Self.ProcessWorked then
    Exit;
  self.TermoProfile.Clear;
  SetCurrentPoint(0);
end;

procedure TChamberProcess.CloseForm;
var
vFrm : ISA4350MC;
begin
  if not Supports(fFormInstance, ISA4350MC, vFrm) then
    Exit;
  vFrm.Close;
  vFrm := nil;
end;

procedure TChamberProcess.StartExposure(ASeconds: LongWord);
begin
  fExposureWatcher.SetWatchTime(ASeconds);
  fExposureWatcher.SetRange(Tempr_Min_Range);
  fExposureWatcher.StartExposure;
  fIsInExposure := true;
end;

procedure TChamberProcess.StopExposure;
begin
  fIsInExposure := false;
end;

{ TChamberCommandItem }

function TChamberCommandItem.MemAlloc: pChamberCommandItem;
begin
  New(result);
  result^.Command := self.Command;
  result^.TargetTempr := self.TargetTempr;
  result^.SecondsChargeTime := self.SecondsChargeTime;
  result^.SecondsTimeOut := self.SecondsTimeOut;
end;

procedure TChamberCommandItem.SetTargetTempr(const Value: double);
begin
  fTargetTempr := EnsureRange(Value, -65, 150);
end;

procedure TChamberCommandItem.SetTargetTime(const Value: LongWord);
begin
  fSecondsTargetTime := EnsureRange(Value, 0, 86400);
end;

procedure TChamberCommandItem.SetTimeOut(const Value: LongWord);
begin
  fSecondsTimeOut := EnsureRange(Value, 0, 86400);
end;

end.
