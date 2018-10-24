unit ST221Module;

interface
uses
   WinApi.Windows, System.Classes, Vodopad.EventList,
   System.SysUtils, System.DateUtils, Vodopad.Timer,  CommPort, SerialPort, EventBusInterface,
   ProtocolDriver, ModBusDriver, ModBusSerial, commtypes, AbstractTag, DiscreteBlock, AnalogBLock,
   WinApi.Messages, ChamberInterfaces, LoggerInterface, CustomChildProcess, System.Threading;

  type
  TMovVariance = class(TObject)
   private
    fInited : Boolean;
    fItems : array of Double;
    fVariance,
    fAverageTempr : double;
   public
    constructor Create(Capacity : Cardinal);
    destructor Destroy; override;
    procedure SetValue(value : double);
    property Variance : double read fVariance;
    property AverageTempr : double read fAverageTempr;
  end;


  TWatcher = record
   private
    fWatchTime : Cardinal; //in seconds
    fStartingTime : TDateTime;
    fTemprRangeValue : double;
    procedure SetWatchTime(ASecons : Cardinal);
    procedure SetRange(Range : double);
   public
    property RangeValue : double read fTemprRangeValue write SetRange;
    procedure StartExposure;
    property RangeTime : Cardinal read fWatchTime write SetWatchTime;
    property StartingTime : TDateTime read fStartingTime;
    function TemprIsInRange(aDifferent : double) : boolean;
    function SecondElapsed: Cardinal;
    function isTimeExpired : boolean;
  end;

  TChamber = class(TComponent,
                    IChamber,
                    IChamberConnectionState,
                    IHMITagInterface)
   private
    fPort : Byte;
    fChamberIniParams :TChamberIni;
    fSerialPortDriver: TSerialPortDriver;
    fModBusRTUDriver : TModBusRTUDriver;
    fCurrTemprBlock,
    fOperationBlock,
    fTargetTemprBlock,
    fTargetSpeedBlock,
    fRemoteModeBlock,
    fRemotePortBlock : TAnalogBlock;
    fRemoteEnabledBlock : TDiscreteBlock;
    fCheckTimer : TvdTimer;
    fMovVariance : TMovVariance;
    fConnected,
    fErrorSignaled,
    fFirstRead,
    fConnectLost,
    fChargeTimeOutSignaled : boolean;
    fChargeTimeStart : TDateTime;
    fSecondsTargetTime,
    fSecondsTargetTimeOut : Cardinal;
    fStabMode : TChamberStabMode;
    fTemprWatcher : TWatcher;
    fTargetTimeOutCount : word;
    fOnTempr,
    fOnTemprCharge,
    fOnTemprInTarget,
    FOnChamberTryConnectFail,
    fOnConnect,
    fOnConnectLost,
    fOnChargeTimeOut,
    fOnTemprOutOfTarget : TNotifyEvent;
    fSearchTask : ITask;
    procedure DriverErrorEvent(Result:TProtocolIOResult; APacket : pIOPacket);
    {IHMITagInterface}
    procedure NotifyReadOk(Sender:TObject); stdcall;
    procedure NotifyReadFault(Sender:TObject); stdcall;
    procedure NotifyWriteOk(Sender:TObject); stdcall;
    procedure NotifyWriteFault(Sender:TObject);stdcall;
    procedure NotifyTagChange(Sender:TObject; AChangedIn : TTagChangedIn);stdcall;
    procedure RemoveTag(Sender:TObject); stdcall;
    procedure SetTargetTempr(const Value : double); stdcall;
    function GetTargetTempr : double; stdcall;
    function GetCurrTempr : double; stdcall;
    procedure SetStateConnected(const value : boolean);
    procedure CheckState(Sender : TObject);
    function getAverageTempr: double;
    function getVariance: double;
    procedure SendTemperatureOff;
    procedure LoadParamsFromFile;
    function GetSecondsTargetTime: Cardinal; stdcall;
    function GetSecondsTargetTimeOut: Cardinal;stdcall;
    function GetStabMode: TChamberStabMode; stdcall;
    function GetSpeed: Double; stdcall;
    function GetMaxSpeed: Double;
    procedure SetMaxSpeed(const Value: Double);
    property OnTempr : TNotifyEvent read fOnTempr write fOnTempr;
    property OnTemprCharge : TNotifyEvent read fOnTemprCharge write fOnTemprCharge;
    property OnTemprInTarget: TNotifyEvent read fOnTemprInTarget write fOnTemprInTarget;
    property OnChargeTimeOut: TNotifyEvent read fOnChargeTimeOut write fOnChargeTimeOut;
    property OnTemprOutOfTarget : TNotifyEvent read fOnTemprOutOfTarget write fOnTemprOutOfTarget;
   protected
    function TryConnect(APort : byte)  : boolean;
    property OnChamberTryConnectFail: TNotifyEvent read FOnChamberTryConnectFail write fOnChamberTryConnectFail;
    property OnConnect : TNotifyEvent read fOnConnect write fOnConnect;
    property OnConnectLost: TNotifyEvent read fOnConnectLost write fOnConnectLost; 
    {IChamberConnectionState}
    function GetConnected:Boolean;stdcall;
    function GetPort:Byte;stdcall;
   public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Variance : double read getVariance;
    property Speed : Double read GetSpeed;
    property AverageTempr : double read getAverageTempr;
    property Connected : boolean read GetConnected;
    property CurrTempr : double read GetCurrTempr;
    property TargetTempr : double read GetTargetTempr write SetTargetTempr;
    property MaxSpeed : Double read GetMaxSpeed write SetMaxSpeed;
    property SecondsTargetTime : Cardinal read GetSecondsTargetTime;
    property SecondsTargetTimeOut : Cardinal read GetSecondsTargetTimeOut;
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
    fProcessWorked : boolean;
    fChamber : TChamber;
    FWindowHandle: HWND;
    fExposureWatcher : TWatcher;
    fIsInExposure : boolean;

    fEventSubscribers :TCustomObjEventList;
    procedure MessageHandler(var message : TMessage);
    procedure FreeFormLibrary;
    procedure OnFormDestroy(Sender: TObject);
    procedure SetCurrentPoint(index : integer);
    function PointExists(index : integer):boolean;
    procedure GotoPoint(AIndex : integer);
    procedure ChamberConnected(Sender: TObject);  
    procedure ChamberTryConnectFail(Sender: TObject);
    procedure ChamberConnectionLost(Sender: TObject);
    procedure ChamberTempr(Sender: TObject);
    procedure ChamberTemprStabilized(Sender: TObject);  
    procedure ChamberStabilizedTimeOut(Sender: TObject);
    procedure ChamberTemprOutOfTarget(Sender: TObject);
    procedure TemprCharge(Sender: TObject);
  private
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
    function GetCurrentPointIdx: integer; stdcall;
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
    property CurrentPoint : integer read GetCurrentPointIdx;
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
    procedure StartExposure(ASeconds : Cardinal);stdcall;
    procedure StopExposure; stdcall;
    function ExposureSecondElapsed: Cardinal; stdcall;
    function GetStartingTime: TDateTime;stdcall;
    property isExposure : boolean read GetIsInExposure;
    function IsExposureDone : boolean; stdcall;
    procedure ManualChargeTempr(const Tempr : double; const Minutes : Cardinal = 0); stdcall;
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
Vodopad.Math,
System.IniFiles,
Vodopad.CustomIniHelpers,
AbstractSearchObject,
FormsControllerInterface,
ST221MCInterface;


const
Max_Seconds_Lost_Connect : integer = 5;
Tempr_Min_Range : double = 0.2;
Tempr_Max_Range : double = 0.5;
Tempr_Range_Time : integer = 120;
OnFormClose = WM_USER + 220;
OnChamber_Detected = OnFormClose + 1;
OnChamber_TryConnectionFail = OnChamber_Detected + 1;
OnChamber_ConnectionLost = OnChamber_TryConnectionFail + 1;
OnChamber_Tempr = OnChamber_ConnectionLost + 1;
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
    vConfig.EnabledLevels := [lInfo, lEvent, lDebug, lTrace, lWarning, lError,
                             lException, lExceptionOS];
    vConfig.DefaultLevelsView := [lInfo, lEvent, lWarning, lError];
    vConfig.FileExtension := 'chmblog';
    vConfig.ArhiveExtension := 'chmblog7z';
    vConfig.ModuleName := 'Процесс ТК(ST-221)';
    try
      Result := AFunct^.AddLogger(vConfig, Obj);
    except
      Result := false;
    end;
  finally
    Dispose(vConfig);
  end;
end;
var ChmbLogger : ILogger=nil;



{$ENDREGION}

{$REGION ' Chamber '}

type
TCSO = class(TAbstractSearchObject)
  fcmb : TChamber;
  procedure OnConnectFail(Sender : TObject);
  procedure OnConnected(Sender : TObject);
  constructor Create(APort : byte;
                AFormHandle:HWND);override;
  destructor Destroy;override;
end;

constructor TCSO.Create(APort : byte;
                AFormHandle : HWND);
begin
  inherited;
  fcmb := TChamber.Create(nil);
  fcmb.OnConnect := OnConnected;
  fcmb.OnChamberTryConnectFail := OnConnectFail;
  if not fcmb.TryConnect(APort) then
    PostMessage(AFormHandle, C_ChamberSearchState, 4, Integer(Self));
end;

procedure TCSO.OnConnected(Sender: TObject);
begin
  PostMessage(Self.FormHandle, C_ChamberSearchState, 3, Integer(Self));
end;

procedure TCSO.OnConnectFail(Sender: TObject);
begin
  PostMessage(Self.FormHandle, C_ChamberSearchState, 4, Integer(Self));
end;

destructor TCSO.Destroy;
begin
  FreeAndNil(fcmb);
  inherited Destroy;
end;

function ChamberSearchObjectClass: TSearchObjectClass; stdcall;
begin
  Result := TCSO;
end; exports ChamberSearchObjectClass;


const ChamberCommPort : array[0..2] of string = ('RS-232 Terminal','RS-232 External','RS-485 External');
constructor TChamber.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fSearchTask := nil;
  fModBusRTUDriver := TModBusRTUDriver.Create(self);
  fSerialPortDriver := TSerialPortDriver.Create(self);
  fSerialPortDriver.Timeout := 700;
  fModBusRTUDriver.DelayBetweenCmds := 5;
  fModBusRTUDriver.CommunicationPort := fSerialPortDriver;
  fModBusRTUDriver.OnErrorEvent := DriverErrorEvent;

  fCurrTemprBlock := fModBusRTUDriver.CreateAnalog(self, 1, 205, 2, true);
  fCurrTemprBlock.RefreshInterval := 200;
  fCurrTemprBlock.AddCallBacks(self);

  fOperationBlock := fModBusRTUDriver.CreateAnalog(self, 1, 200, 1, false);
  fOperationBlock.RefreshInterval := 200;
  fOperationBlock.AddCallBacks(self);

  fTargetTemprBlock := fModBusRTUDriver.CreateAnalog(self, 1, 201, 2, false);
  fTargetTemprBlock.RefreshInterval := 200;
  fTargetTemprBlock.AddCallBacks(self);

  fTargetSpeedBlock := fModBusRTUDriver.CreateAnalog(self, 1, 203, 2, false);
  fTargetTemprBlock.RefreshInterval := 200;
  fTargetTemprBlock.AddCallBacks(self);

  fRemoteModeBlock := fModBusRTUDriver.CreateAnalog(self, 1, 210, 1, false);
  fRemoteModeBlock.RefreshInterval := 200;
  fRemoteModeBlock.AddCallBacks(self);

  fRemoteEnabledBlock := fModBusRTUDriver.CreateDiscrete(self, 1, 500, 1, false);
  fRemoteEnabledBlock.RefreshInterval := 200;
  fRemoteEnabledBlock.AddCallBacks(self);
  
  fRemotePortBlock  := fModBusRTUDriver.CreateAnalog(self, 1, 608, 1, True);
  fRemotePortBlock.RefreshInterval := 200;
  fRemotePortBlock.AddCallBacks(self);

  fConnected := false;
  fMovVariance := TMovVariance.create(20);
  fSecondsTargetTimeOut := 1000;
  fTemprWatcher.SetRange(Tempr_Max_Range);
  fTemprWatcher.RangeTime := Tempr_Range_Time;
  fStabMode := mChallenge;
  fCheckTimer := TvdTimer.Create(nil);
  fCheckTimer.OnTimer := CheckState;
  fCheckTimer.Interval := 1000;
  fCheckTimer.Enabled := false;
end;

destructor TChamber.Destroy;
begin
  try
    if Assigned(fSearchTask) then
      TTask.WaitForAll([fSearchTask], 10000);
    fModBusRTUDriver.StopScan;
    fSerialPortDriver.Active := false;
  finally
    fCurrTemprBlock.AutoRead := false;
    fOperationBlock.AutoRead := false;
    fTargetTemprBlock.AutoRead := false;
    fTargetSpeedBlock.AutoRead := false;
    fRemoteModeBlock.AutoRead := false;
    fRemoteEnabledBlock.AutoRead := false;
    fRemotePortBlock.AutoRead := false;
    fRemotePortBlock.RemoveCallBacks(self);
    fRemoteEnabledBlock.RemoveCallBacks(self);
    fRemoteModeBlock.RemoveCallBacks(self);
    fOperationBlock.RemoveCallBacks(self);
    fTargetTemprBlock.RemoveCallBacks(self);
    fTargetSpeedBlock.RemoveCallBacks(self);
    fCurrTemprBlock.RemoveCallBacks(self);
    fSerialPortDriver.Active := false;
    FreeAndNil(fMovVariance);
    FreeAndNil(fCheckTimer);
    inherited Destroy;
  end;
end; 

procedure TChamber.DriverErrorEvent(Result: TProtocolIOResult; APacket: pIOPacket);
function bufferToHex(const ABuffer:array of Byte; AMax : word):String;
var
  i,
  vHigh : integer;
begin
  Result:='';
  vHigh := System.Math.Min(AMax-1, High(ABuffer));
  for i:=0 to vHigh do
    Result:=Result+IntToHex(ABuffer[i],2)+' ';
end;
var
vStr : string;
begin
  if Assigned(ChmbLogger) then
  begin
    vStr := Format('IOResult: %s; WriteIOResult: %s; ReadIOResult: %s; '+
    'Write time:%d ms; WR delay:%d ms; Read time:%d ms; '+
    'ToWrite count:%d; Written count:%d; Write retries:%d; '+
    'Delay between command:%d; ToRead count:%d; '+
    'Received count:%d; Read retries:%d; Write buffer:%s; Read buffer:%s',
          [C_ProtocolIOResultStr[Result],
          C_PortIOResultStr[APacket.WriteIOResult],
          C_PortIOResultStr[APacket.ReadIOResult],
          MilliSecondsBetween(APacket.WriteStart, APacket.WriteFinish),
          MilliSecondsBetween(APacket.WriteFinish, APacket.ReadStart),
          MilliSecondsBetween(APacket.ReadStart, APacket.ReadFinish),
          APacket.ToWriteCount,
          APacket.WrittenCount,
          APacket.WriteRetries,
          APacket.DelayBetweenCommand,
          APacket.ToReadCount,
          APacket.ReceivedCount,
          APacket.ReadRetries,
          bufferToHex(APacket.BufferToWrite, APacket.WrittenCount),
          bufferToHex(APacket.BufferToRead, APacket.ReceivedCount)
          ]);
    ChmbLogger.Log(lError, vStr);
  end;
end;

procedure TChamber.LoadParamsFromFile;
begin
  fChamberIniParams.LoadIni;
  fTemprWatcher.SetRange(fChamberIniParams.DeltaTempr);
  fTemprWatcher.RangeTime := fChamberIniParams.DeltaTime;
  if Assigned(ChmbLogger) then
    ChmbLogger.Log(
                lInfo,
                Format('DeltaTempr=%f °C, DeltaTime=%d сек',
                [fTemprWatcher.RangeValue, fTemprWatcher.RangeTime]));
end;

procedure TChamber.NotifyReadFault(Sender: TObject);
begin

end;

procedure TChamber.NotifyReadOk(Sender: TObject);
begin
  if fConnectLost then
    SetStateConnected(true);
end;

procedure TChamber.NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn);
begin
  if not Assigned(ChmbLogger) then
    exit;
  if fConnectLost then
    exit;
  if (Sender = fCurrTemprBlock) then
  begin
    ChmbLogger.Log(lTrace, Format('Curr tempr changed to %f°C',
     [GetCurrTempr]));
  end else
  if (Sender = fOperationBlock) then
  begin
    ChmbLogger.Log(lTrace, Format('Operation block changed. chamber mode:%d. target tempr:%f°C. max speed:%f°C/min',
     [fOperationBlock.Values[0], GetTargetTempr, GetMaxSpeed]));
  end else
  if (Sender = fTargetTemprBlock) then
  begin
    ChmbLogger.Log(lTrace, Format('Target tempr block changed. target tempr:%f°C',
     [GetTargetTempr]));
  end else
  if (Sender = fTargetSpeedBlock) then
  begin
    ChmbLogger.Log(lTrace, Format('Target speed block changed. max speed:%f°C/min',
     [GetMaxSpeed]));
  end else
  if (Sender = fRemoteModeBlock) then
  begin
    ChmbLogger.Log(lTrace, Format('Remote mode changed to %d',
     [fRemoteModeBlock.Values[0]]));
  end else
  if (Sender = fRemotePortBlock) then
  begin
    ChmbLogger.Log(lTrace, Format('state remote port changed to %s',
     [ChamberCommPort[fRemotePortBlock.values[0]]]));
  end else
  if (Sender = fRemoteEnabledBlock) then
  begin
    ChmbLogger.Log(lTrace, Format('state remote port changed to %s',
     [BoolToStr(fRemoteEnabledBlock.Values[0], true)]));
  end;
end;

procedure TChamber.NotifyWriteFault(Sender: TObject);
begin

end;

procedure TChamber.NotifyWriteOk(Sender: TObject);
begin
  
end;

procedure TChamber.RemoveTag(Sender: TObject);
begin

end;

function TChamber.TryConnect(APort : byte) : boolean;
var
vTestHandle : THandle;
begin
  if fSerialPortDriver.Active then
    fSerialPortDriver.Active := false;
  if Assigned(ChmbLogger) then
    ChmbLogger.Log(lInfo, Format('Проверка доступности порта COM%d',[APort]));
  try
    vTestHandle := CreateFile(PWideChar(Format('\\.\COM%d',[APort])),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
    result := vTestHandle <> INVALID_HANDLE_VALUE;
    if not Result then
    begin
      if Assigned(ChmbLogger) then
        ChmbLogger.Log(lWarning, Format('Порт COM%d недоступен',[APort]));
      Exit;
    end;
  finally
    CloseHandle(vTestHandle);
  end;
  if Assigned(ChmbLogger) then
    ChmbLogger.Log(lInfo, Format('Порт COM%d доступен',[APort]));

  fFirstRead := True;
  fConnectLost := false;
  LoadParamsFromFile;
  fTargetTimeOutCount := 0;
  fErrorSignaled := False;
  fSerialPortDriver.COMPort := Format('COM%d',[APort]);
  fSerialPortDriver.BaudRate := br115200;
  fSerialPortDriver.Active := true;
  fSearchTask := TTask.Run(
    procedure
    var
      vReadOk : Boolean;
    begin
      Sleep(100);
      vReadOk := fCurrTemprBlock.ReadSyn
              and fOperationBlock.ReadSyn
              and fTargetTemprBlock.ReadSyn
              and fTargetSpeedBlock.ReadSyn
              and fRemoteModeBlock.ReadSyn
              and fRemotePortBlock.ReadSyn
              and fRemoteEnabledBlock.ReadSyn;
      Sleep(10);
      TThread.Synchronize(TThread.Current,
        procedure
        begin
            if vReadOk then
            begin
              fPort := APort;
              fCheckTimer.Enabled := true;
            end else
            begin
              fSerialPortDriver.Active := false;
              if assigned(FOnChamberTryConnectFail) then
                FOnChamberTryConnectFail(Self);
            end;
          fSearchTask := nil;
        end);

    end);
end;

function TChamber.getAverageTempr: double;
begin
  result := fMovVariance.AverageTempr;
end;

function TChamber.GetConnected: Boolean;
begin
  Result := fConnected;
end;

function TChamber.GetSecondsTargetTime: Cardinal;
begin
  result := fSecondsTargetTime;
end;

function TChamber.GetSecondsTargetTimeOut: Cardinal;
begin
  result := fSecondsTargetTimeOut;
end;

function TChamber.GetSpeed: Double;
begin
  result := fMovVariance.Variance * (60000 / fCheckTimer.Interval);
end;

function TChamber.GetStabMode: TChamberStabMode;
begin
  result := fStabMode;
end;

function TChamber.getVariance: double;
begin
  result := fMovVariance.Variance;
end;


procedure TChamber.SetStateConnected(const value: boolean);
var
vPortValue : word;
begin
  if fConnected = value then
    exit;
  fConnected := value;
  if fConnected then
  begin
    fFirstRead := False;
    fConnectLost := false;
    if Assigned(ChmbLogger) then
    begin
      vPortValue := fRemotePortBlock.Values[0];
      if vPortValue in [0..2] then
      begin
        if Assigned(ChmbLogger) then
          ChmbLogger.Log(lInfo, Format(
            'Подключение к камере через порт %s',
            [ChamberCommPort[vPortValue]]));
      end
      else if Assigned(ChmbLogger) then
        ChmbLogger.Log(lInfo, Format(
            'Подключение к камере через порт %d',
            [vPortValue]))
    end;
    if (assigned(fOnConnect)) then
      fOnConnect(self);
  end else if not fConnectLost then
  begin
    fConnectLost := true;
    if Assigned(ChmbLogger) then
      ChmbLogger.Log(lWarning, 'Соединение с камерой потеряно');
    fCheckTimer.Interval := 3000;
    if (assigned(fOnConnectLost)) then
      fOnConnectLost(self);
  end;
  fCurrTemprBlock.AutoRead := value;
  fOperationBlock.AutoRead := value;
  fTargetTemprBlock.AutoRead := value;
  fTargetSpeedBlock.AutoRead := value;
  fRemoteModeBlock.AutoRead := value;
  fRemoteEnabledBlock.AutoRead := value;
  fRemotePortBlock.AutoRead := value;
end;

procedure TChamber.SendTemperatureOff;
begin
  fStabMode := mChallenge;
  fOperationBlock.Values[0] := 1;
  fOperationBlock.WriteAsyn;
end;

type
TSingleAdapter = record
  case integer of
    0: (f: Single);
    1: (w : array [0..1] of Word);
  end;

function TChamber.GetCurrTempr: double;
var
vAd : TSingleAdapter;
begin
  vAd.w[0] := fCurrTemprBlock.Values[0];
  vAd.w[1] := fCurrTemprBlock.Values[1];
  result := vAd.f;
end;

function TChamber.GetTargetTempr: double;
var
vAd : TSingleAdapter;
begin
  vAd.w[0] := fTargetTemprBlock.Values[0];
  vAd.w[1] := fTargetTemprBlock.Values[1];
  result := vAd.f;
end;

function TChamber.GetMaxSpeed: Double;
var
vAd : TSingleAdapter;
begin
  vAd.w[0] := fTargetSpeedBlock.Values[0];
  vAd.w[1] := fTargetSpeedBlock.Values[1];
  if (vAd.f < 0.1) then
    vAd.f := 0.1;
  result := vAd.f;
end;

procedure TChamber.SetMaxSpeed(const Value: Double);
var
vAd : TSingleAdapter;
begin
  if ((not fConnected)
  or (not (fOperationBlock.Values[0] in [1, 2]))
  or (not fRemoteEnabledBlock.Values[0])) then
  begin
    if Assigned(ChmbLogger) then
    begin
      if (not fConnected) then
        ChmbLogger.Log(lWarning, 'Нет соединения с камерой. Изменение скорости недоступно')
      else if (not(fOperationBlock.Values[0] in [1, 2])) then
        ChmbLogger.Log(lWarning, Format('Камера в режиме %d. Изменение скорости недоступно',
      [fOperationBlock.Values[0]]))
      else
        ChmbLogger.Log(lWarning, 'Удаленное управление запрещено. Изменение скорости недоступно');
    end;
    exit;
  end;
  vAd.f := Round((Value *  1000000)) / 1000000;
  if (vAd.f < 0.1) then
    vAd.f := 0.1;
  fTargetSpeedBlock.Values[0] := vAd.w[0];
  fTargetSpeedBlock.Values[1] := vAd.w[1];
  fTargetSpeedBlock.WriteAsyn;
end;

procedure TChamber.SetTargetTempr(const Value: double);
var
vAd : TSingleAdapter;
begin
  if ((not fConnected)
  or (not (fOperationBlock.Values[0] in [1, 2]))
  or (not fRemoteEnabledBlock.Values[0])) then
  begin
    if Assigned(ChmbLogger) then
    begin
      if (not fConnected) then
        ChmbLogger.Log(lWarning, 'Нет соединения с камерой. Изменение уставки температуры или режима недоступно')
      else if (not(fOperationBlock.Values[0] in [1, 2])) then
        ChmbLogger.Log(lWarning, Format('Камера в режиме %d. Изменение уставки температуры или режима недоступно',
      [fOperationBlock.Values[0]]))
      else
        ChmbLogger.Log(lWarning, 'Удаленное управление запрещено. Изменение уставки температуры или режима недоступно');
    end;
    exit;
  end;
  if  not (fStabMode in [mChargeTimeOut, mCharge]) then
  begin
    fChargeTimeStart := now;
    fSecondsTargetTime := Round((Abs(GetCurrTempr - Value) / GetMaxSpeed) * 60);
    fSecondsTargetTimeOut := fSecondsTargetTime*3 + fChamberIniParams.TimeOutCoeff;
    if Assigned(ChmbLogger) then
      ChmbLogger.Log(lDebug, Format(
      'Целевая температура:%5.3f°C. Расчетное время набора: %d сек. Таймаут: %d сек.',
      [Value, fSecondsTargetTime, fSecondsTargetTimeOut]));
  end;

  vAd.f := Round((Value *  1000000)) / 1000000;
  fTargetTemprBlock.Values[0] := vAd.w[0];
  fTargetTemprBlock.Values[1] := vAd.w[1];
  fTargetTemprBlock.WriteAsyn;
  if (fOperationBlock.Values[0] = 1) then
  begin
    fOperationBlock.Values[0] := 2;
    fOperationBlock.WriteAsyn;
  end;
  fStabMode := mCharge;
  if Assigned(ChmbLogger) then
      ChmbLogger.Log(lEvent, Format('Запись блока. Температура:%f°C. Макс.скорость:%f°C\мин',
      [vAd.f, GetMaxSpeed]));
end;

procedure TChamber.CheckState(Sender : TObject);

var CurrDiff : double;
begin
  if fFirstRead then
  begin
    SetStateConnected(True);
    Exit;
  end else if fConnectLost then
  begin
    fCurrTemprBlock.ReadAsyn;
    exit;
  end;
  SetStateConnected((MilliSecondsBetween(now, fCurrTemprBlock.ValueTimestamp) <= (fCheckTimer.Interval * 3)));

  if not fConnected then
    Exit;

  
  fMovVariance.SetValue(CurrTempr);
  if assigned(fOnTempr) then
    fOnTempr(self);
  case fStabMode of
    mCharge..mWatch:
      CurrDiff := abs(CurrTempr - TargetTempr);
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
        begin
          fStabMode := mChargeTimeOut;
          if fChargeTimeOutSignaled then
            fChargeTimeOutSignaled := false;
        end;
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
        begin
          fStabMode := mChargeTimeOut;
          if fChargeTimeOutSignaled then
            fChargeTimeOutSignaled := false;
        end;
      end;
    end;
    mWatch:
    begin
      if not (fTemprWatcher.TemprIsInRange(CurrDiff)) then
      begin
        inc(fTargetTimeOutCount);
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
          inc(fTargetTimeOutCount);
      end else
        fStabMode := mChallenge;
    end;
  end;
end;


function TChamber.GetPort: Byte;
begin
  Result := fPort;
end;

{$ENDREGION}

{$REGION ' MovAverage '}
  
constructor TMovVariance.Create(Capacity: Cardinal);
begin
  inherited Create;
  fInited := False;
  Setlength(fItems, Capacity);
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
  if not fInited then
  begin
    for i := Low(fItems) to High(fItems) do
        fItems[i] := value;
    fInited := True;
  end;

  val1 := Mean(fItems);
  for I := Low(fItems) to High(fItems) - 1 do
      fItems[i] := fItems[i+1];
  fItems[High(fItems)] := value;
  fAverageTempr := Mean(fItems);
  fVariance := abs(val1 - fAverageTempr);
end;

  
{$ENDREGION}


{ TRangeData }

function TWatcher.TemprIsInRange(aDifferent: double): boolean;
begin
  result := (aDifferent < fTemprRangeValue);
end;

function TWatcher.SecondElapsed : Cardinal;
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

procedure TWatcher.SetWatchTime(ASecons: Cardinal);
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
  if not Assigned(Item1)
  or not Assigned(Item2) then
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
vFS : TFormatSettings;
begin
  f := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance), '.ini'));
  vFS := TFormatSettings.Create(1033);
  try
    Speed := Vodopad.CustomIniHelpers.ReadFloat(f, 'ChamberProcess', 'Speed', 1, vFS);
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
  fChamber.OnChamberTryConnectFail := ChamberTryConnectFail;
  fChamber.OnConnect := ChamberConnected;
  fChamber.OnTempr := ChamberTempr;
  fChamber.OnTemprInTarget := ChamberTemprStabilized; 
  fChamber.OnChargeTimeOut := ChamberStabilizedTimeOut;
  fChamber.OnTemprOutOfTarget := ChamberTemprOutOfTarget;
  fChamber.OnTemprCharge := TemprCharge;
  fChamber.OnConnectLost := ChamberConnectionLost;
  fTermoProfile := TTermoProfile.Create(Self);
  fCurrentPoint := 0;
  StopExposure;

  FWindowHandle := System.Classes.AllocateHWND(MessageHandler);
  fExposureWatcher.StartExposure;
end;

function TChamberProcess.DefaultSpeed: double;
begin
  Result := fIniParams.Speed;
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
  if not Result then
    ChamberTryConnectFail(fChamber);
end;

procedure TChamberProcess.SendDisconnectAndWait;
begin
end;

procedure TChamberProcess.ChamberConnected(Sender : TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_Detected, 0, 0);
end;

procedure TChamberProcess.ChamberConnectionLost(Sender: TObject);
begin
  WinApi.Windows.PostMessage(FWindowHandle, OnChamber_ConnectionLost, 0, 0);
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
begin
  if PointExists(AIndex)  then
  begin
    fTermoProfile.PrevTempr := fChamber.TargetTempr;
    if (fTermoProfile[AIndex].Speed < 0.1) then
    begin
      fIniParams.LoadParamsFromFile;
      fChamber.MaxSpeed := fIniParams.Speed;
    end else
      fChamber.MaxSpeed := fTermoProfile[AIndex].Speed;
    fChamber.TargetTempr := fTermoProfile[AIndex].Tempr;
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

function TChamberProcess.ExposureSecondElapsed : Cardinal;
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
  fChamber.SendTemperatureOff;
  SetCurrentPoint(0);
end;

procedure TChamberProcess.ManualChargeTempr(const Tempr: double; const Minutes: Cardinal);
var
vMinutesChargeTime : Cardinal;
begin
  fTermoProfile.PrevTempr := fChamber.CurrTempr;
  vMinutesChargeTime := Minutes;
  if vMinutesChargeTime = 0 then
    vMinutesChargeTime := trunc(abs(Tempr - fChamber.CurrTempr) / fIniParams.Speed);  
  if vMinutesChargeTime < 1 then
    vMinutesChargeTime := 1;
  fChamber.fStabMode := mChallenge;
  fChamber.MaxSpeed := abs(Tempr - fChamber.CurrTempr) / vMinutesChargeTime;
  fChamber.TargetTempr:= Tempr;
end;

procedure TChamberProcess.ManualChargeTemprBySpeed(const Target, SpeedDegMin: double);
begin
  fChamber.fStabMode := mChallenge;
  fTermoProfile.PrevTempr := fChamber.CurrTempr;
  if SpeedDegMin < 0.1 then
   fChamber.MaxSpeed := fIniParams.Speed
  else
    fChamber.MaxSpeed := SpeedDegMin;
  fChamber.TargetTempr:= Target;
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
vFrm : IST221MC;
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
      if Supports(fFormInstance, IST221MC, vFrm) then
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
      if Supports(fFormInstance, IST221MC, vFrm) then
      begin
        vFrm.SetSpeed(0);
        vFrm.SetCurrentTempr(0);
        vFrm.SetTargetTempr(0);
        vFrm.SetMessageText(1, 'Соединение с камерой потеряно');
      end;
    end;
    OnChamber_Tempr:
    begin
      if Assigned(ChmbLogger) and (fChamber.Connected) then
        ChmbLogger.Log(lTrace,
        Format('Текущая температура %f°C', [fChamber.CurrTempr]));
      fEventSubscribers.Execute(Self, C_OnChamber_Tempr, nil);
      if fIsInExposure and IsExposureDone then
      begin
        StopExposure;
        fEventSubscribers.Execute(Self, C_OnTemprStabilizedAndExposure, nil);
      end;
      if Supports(fFormInstance, IST221MC, vFrm) then
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
      if Supports(fFormInstance, IST221MC, vFrm) then
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
        ChmbLogger.Log(lWarning, 'Выход за пределы диапазона стабилизации');
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
vFrm : IST221MC;
begin
  if Assigned(fFormInstance)
  and Supports(fFormInstance, IFormToFront, vFormToFront) then
  begin
    vFormToFront.BringToFront;
    Exit;
  end;
  fUiHandle := SafeLoadLibrary(ExtractFilePath(ParamStr(0)) + 'ST221ui.bpl',
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
    fFormInstance := vCreateFormInstance(AOwner, SetTemprBtnClick, TemperatOffBtnClick);
    if Supports(fFormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
       vFormOnDestroyEvent.OnDestroy := OnFormDestroy
    else
      raise Exception.Create('st221 form not supports destroy event interface');
    if Supports(fFormInstance, IShowMdiForm, vShowMdiForm) then
    begin
      vShowMdiForm.Show;
      if Supports(fFormInstance, IST221MC, vFrm) then
      begin
        vFrm.BlockForm(fProcessWorked or (not Chamber.Connected));
        if (not Chamber.Connected) then
          vFrm.SetMessageText(1, 'Камера не подключена');
        vFrm := nil;
      end;
    end
    else
      raise Exception.Create('st221 form not supports show interface');
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
vFrm : IST221MC;
begin
  if not Supports(fFormInstance, IST221MC, vFrm) then
    Exit;
  if Chamber.Connected then
     ManualChargeTempr(vFrm.GetTargetTempr, vFrm.GetTargetTime)
  else
    vFrm.SetMessageText(2, 'Камера не доступна для ручного управления');
  vFrm := nil;
end;

procedure TChamberProcess.TemperatOffBtnClick(Sender: TObject);
var
vFrm : IST221MC;
begin
  if not Supports(fFormInstance, IST221MC, vFrm) then
    Exit;
  if Chamber.Connected then
     ManualTemperatureOff
  else
    vFrm.SetMessageText(2, 'Камера не доступна для ручного управления');
  vFrm := nil;
end;

procedure TChamberProcess.BlockForm(ADoBlock: boolean);
var
vFrm : IST221MC;
begin
  if not Supports(fFormInstance, IST221MC, vFrm) then
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
vFrm : IST221MC;
begin
  if not Supports(fFormInstance, IST221MC, vFrm) then
    Exit;
  vFrm.Close;
  vFrm := nil;
end;

procedure TChamberProcess.StartExposure(ASeconds: Cardinal);
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


end.
