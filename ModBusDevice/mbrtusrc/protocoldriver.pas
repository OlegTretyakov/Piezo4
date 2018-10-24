unit ProtocolDriver;

interface

uses
  SysUtils, Classes, CommPort, CommTypes, ProtocolTypes, DriverMessageThread,
  TagsScanerThread, AbstractTag, CircleQueue, Windows,
  System.Generics.Collections;

type
  TScanableStation = class(TObject)
   private
    FStation : Word;
    FCoils : TObjectList<TAbstractTag>;
    FDiscretes : TObjectList<TAbstractTag>;
    FHoldings : TObjectList<TAbstractTag>;
    FInputs : TObjectList<TAbstractTag>;
    function GetTotalCount: Word;
   protected
    procedure AddTag(const ATag : TAbstractTag);
    procedure RemoveTag(const ATag : TAbstractTag);
   public
    constructor Create(AStation : Word);
    destructor Destroy; override;
    property TotalCount : Word read GetTotalCount;
    property Station : Word read FStation;
    property Coils : TObjectList<TAbstractTag> read FCoils;
    property Discretes : TObjectList<TAbstractTag> read FDiscretes;
    property Holdings : TObjectList<TAbstractTag> read FHoldings;
    property Inputs : TObjectList<TAbstractTag> read FInputs;
  end;
 
  TProtocolDriver = class(TComponent, IPortDriverEventNotification)
  private
    FDestroying : Boolean;
    FDriverTags : TObjectList<TAbstractTag>;
    FStations : TObjectList<TScanableStation>;

    //Scan write/read thread object
    FScanThread : TScanThread;

    //thread that fire events synchronized with main thread
    FMessageThread : TMessageThread;

    FOnErrorEvent : TProtocolErrorEvent;
    procedure CreateThreads;
    procedure StopThreads;
    procedure ScanRead(var oRescanAfter : Cardinal);
    procedure ThreadTerminated(ASender : TObject);
    {IPortDriverEventNotification}
    procedure DoPortOpened(ASender: TObject);
    procedure DoPortClosed(ASender: TObject);
    procedure DoPortRemoved(ASender: TObject);
  protected
    //: Tells if the protocol driver is ready
    FProtocolReady : Boolean;
    //: Stores the communication port driver used by protocol driver.
    FCommPort : TCommPortDriver;
    //: Mutex that protect the protocol driver.
   // FPDCritical : TCriticalSection;
    procedure PostProtocolError(AError : TProtocolIOResult; AErrorPacket : TCircleQueueItem<TIOPacket>);
    procedure SetCommPort(const CommPort : TCommPortDriver);
    procedure ProtocolErrorCallBack(Result : TProtocolIOResult; APacket : pIOPacket); virtual;
    function DoWrite(const ATagObj : TAbstractTag): TProtocolIOResult; virtual; abstract;
    function DoRead(const ATagObj : TAbstractTag): TProtocolIOResult; virtual; abstract;
    {IPortDriverEventNotification}
    function GetPortOpenedEvent: TNotifyEvent;
    function GetPortClosedEvent: TNotifyEvent;
    function NotifyThisEvents: TNotifyThisEvents; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure AddTag(const ATagObj : TAbstractTag);
    procedure RemoveTag(const ATagObj : TAbstractTag);
    procedure AddTagToScan(const ATagObj : TAbstractTag);
    procedure RemoveTagFromScan(const ATagObj : TAbstractTag);
    procedure ReadAsyn(const ATagObj : TAbstractTag);
    procedure WriteAsyn(const ATagObj : TAbstractTag);
    function ReadSyn(const ATagObj : TAbstractTag):TProtocolIOResult;
    function WriteSyn(const ATagObj : TAbstractTag):TProtocolIOResult;
    procedure StopScan;
    property CommunicationPort: TCommPortDriver read FCommPort write SetCommPort;
    property OnErrorEvent : TProtocolErrorEvent read FOnErrorEvent write FOnErrorEvent;
  end;


implementation

uses
System.Math;


constructor TProtocolDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommPort := nil;
  FDriverTags := TObjectList<TAbstractTag>.Create(False);
  FStations := TObjectList<TScanableStation>.Create;
  FProtocolReady := true;
  FMessageThread := nil;
  FScanThread := nil;
  FDestroying := False;
end;

procedure TProtocolDriver.CreateThreads;
begin
  if FDestroying then
    Exit;
  if (not assigned(FCommPort))
  or (not FCommPort.ReallyActive) then
    exit;
  if not Assigned(FMessageThread) then
    FMessageThread := TMessageThread.Create(ProtocolErrorCallBack,
                                                ThreadTerminated);
  if not Assigned(FScanThread) then
    FScanThread := TScanThread.Create(ScanRead,
                                    DoWrite,
                                    DoRead,
                                    FMessageThread,
                                    ThreadTerminated);
end;

procedure TProtocolDriver.StopScan;
begin
  if Assigned(FScanThread) then
  begin
    FScanThread.DisableScan;
    FScanThread.Stop;
    FreeAndNil(FScanThread);
  end;
end;

procedure TProtocolDriver.StopThreads;
begin
  if Assigned(FScanThread) then
  begin
    FScanThread.Stop;
    FreeAndNil(FScanThread);
  end;
  if Assigned(FMessageThread) then
  begin
    FMessageThread.Stop;
    FreeAndNil(FMessageThread);
  end;
end;

destructor TProtocolDriver.Destroy;
var
  c:Integer;
begin
  FDestroying := true;
  c:=FDriverTags.Count-1;
  while c > -1 do
  begin
    FDriverTags[c].RemoveDriver;
    dec(c);
  end;
  SetCommPort(nil);
  StopThreads;
  FreeAndNil(FStations);
  FreeAndNil(FDriverTags);
  inherited Destroy;
end;

procedure TProtocolDriver.SetCommPort(const CommPort : TCommPortDriver);
begin
  //if is the same communication port, exit.
  if (CommPort = FCommPort) then
    exit;

  if FCommPort<>nil then
    FCommPort.DelProtocol(Self);

  if CommPort<>nil then
    CommPort.AddProtocol(Self);

  FCommPort := CommPort;
end;

procedure TProtocolDriver.ThreadTerminated(ASender : TObject);
begin
  if (ASender = FScanThread) then
    FScanThread := nil;
  if (ASender = FMessageThread) then
    FMessageThread := nil;
  CreateThreads;
end;

procedure TProtocolDriver.AddTag(const ATagObj : TAbstractTag);
var
  vIdx : Integer;
begin
  if not assigned(ATagObj) then
    exit;
  vIdx := FDriverTags.IndexOf(ATagObj);
  if (vIdx = -1) then
     FDriverTags.Add(ATagObj);
  if ATagObj.AutoRead then
     AddTagToScan(ATagObj);
end;

procedure TProtocolDriver.RemoveTag(const ATagObj : TAbstractTag);
var
  vIdx : Integer;
begin
  RemoveTagFromScan(ATagObj);
  vIdx := FDriverTags.IndexOf(ATagObj);
  if (vIdx <> -1) then
     FDriverTags.Delete(vIdx);
end;

procedure TProtocolDriver.AddTagToScan(const ATagObj : TAbstractTag);
var
  vIdx : Word;
  vStation : TScanableStation;
  vNeedUpdate : Boolean;
begin
  if not assigned(ATagObj) then
    exit;
  if FDestroying then
  begin
    SwitchToThread;
    exit;
  end;

  vIdx := 0;
  vNeedUpdate := FStations.Count = 0;
  while vIdx < FStations.Count do
  begin
    if (FStations[vIdx].Station = ATagObj.PLCStation) then
      Break;
    Inc(vIdx);
  end;
  if vIdx < FStations.Count then
  begin
    vStation := FStations[vIdx];
    vStation.AddTag(ATagObj);
  end else
  begin
    vStation := TScanableStation.Create(ATagObj.PLCStation);
    vIdx := 0;
    while vIdx < FStations.Count do
    begin
      if (FStations[vIdx].Station >= ATagObj.PLCStation) then
        Break;
      Inc(vIdx);
    end;
    vStation.AddTag(ATagObj);
    TMonitor.Enter(FStations);
    try
      FStations.Insert(vIdx, vStation);
    finally
      TMonitor.Exit(FStations);
    end;
  end;

  if (not Assigned(FScanThread))
  or (not Assigned(FMessageThread)) then
    CreateThreads;
  if vNeedUpdate or (not FScanThread.ScanEnabled) then
    FScanThread.EnableScan;
end;

procedure TProtocolDriver.RemoveTagFromScan(const ATagObj : TAbstractTag);
var
  vIdx : Word;
begin
  if Assigned(FMessageThread) then
    FMessageThread.RemoveTag(ATagObj);
  if Assigned(FScanThread) then
    FScanThread.RemoveTag(ATagObj);
  vIdx := 0;
  while (vIdx < FStations.Count) do
  begin
    if (FStations[vIdx].Station = ATagObj.PLCStation) then
      Break;
    Inc(vIdx);
  end;
  if vIdx < FStations.Count then
  begin
    FStations[vIdx].RemoveTag(ATagObj);
    if (FStations[vIdx].TotalCount < 1) then
    begin
      TMonitor.Enter(FStations);
      try
        FStations.Delete(vIdx);
      finally
        TMonitor.Exit(FStations);
      end;
    end;
  end;
  if Assigned(FScanThread) and (FStations.Count < 1) then
      FScanThread.DisableScan;
end;

procedure TProtocolDriver.ReadAsyn(const ATagObj : TAbstractTag);
begin
  if FDestroying then
    exit;
  if (not Assigned(ATagObj))
  or (ATagObj.Size < 1) then
    exit;
  if (not Assigned(FScanThread))
  or (not Assigned(FMessageThread)) then
    CreateThreads;
  //post a message requesting a scanread
  if Assigned(FScanThread) then
    FScanThread.AsynRead(ATagObj);
end;

procedure TProtocolDriver.WriteAsyn(const ATagObj : TAbstractTag);
begin
  if FDestroying then
    exit;

  if (not Assigned(ATagObj))
  or (ATagObj.Size < 1) then
    exit;
  if not (ATagObj.TagType in [ttCoil, ttHolding]) then
    exit;
  if (not Assigned(FScanThread))
  or (not Assigned(FMessageThread)) then
    CreateThreads;
  if Assigned(FScanThread) then
    FScanThread.AsynWrite(ATagObj);
end;

function TProtocolDriver.ReadSyn(const ATagObj:TAbstractTag): TProtocolIOResult;
var
  vNotifications : IProtocolNotifications;
begin
  result := ioDriverError;
  try
    result := DoRead(ATagObj);
  finally
    if supports(ATagObj, IProtocolNotifications, vNotifications) then
    try
      if (result = ioOk) and (ATagObj.LastError = ioOk) then
        vNotifications.NotifyReadOk
      else
        vNotifications.NotifyReadFault;
    finally
      vNotifications := nil;
    end;
  end;
end;

function TProtocolDriver.WriteSyn(const ATagObj : TAbstractTag): TProtocolIOResult;
var
  vNotifications : IProtocolNotifications;
begin
  result := ioDriverError;
  if not (ATagObj.TagType in [ttCoil, ttHolding]) then
  begin
    result := ioIllegalFunction;
    exit;
  end;
  try
    result := DoWrite(ATagObj);
  finally
    if supports(ATagObj, IProtocolNotifications, vNotifications) then
    try
      if (result = ioOk) and (ATagObj.LastError = ioOk) then
        vNotifications.NotifyWriteOk
      else
        vNotifications.NotifyWriteFault;
    finally
      vNotifications := nil;
    end;
  end;
end;

procedure TProtocolDriver.ScanRead(var oRescanAfter: Cardinal);
procedure InternalScanBlock(const ABlock : TObjectList<TAbstractTag>);
var
  vBlkIdx : word;
  vBlkNextScan : Cardinal;
begin
  vBlkIdx := 0;
  TMonitor.Enter(ABlock);
  try
    while vBlkIdx < ABlock.Count do
    begin
      if (FDestroying
      or (not assigned(CommunicationPort))
      or (not CommunicationPort.ReallyActive)
      or TThread.CheckTerminated) then
        break;
      if ABlock[vBlkIdx].NeedRescan then
      begin
        DoRead(ABlock[vBlkIdx]);
        if Assigned(FMessageThread) then
          FMessageThread.ScanRead(ABlock[vBlkIdx]);
        Sleep(5);
      end;
      if ABlock[vBlkIdx].AutoRead then
      begin
        vBlkNextScan := ABlock[vBlkIdx].RemainingMilisecondsForNextScan;
        if vBlkNextScan < oRescanAfter then
          oRescanAfter := vBlkNextScan;
      end;
      inc(vBlkIdx);
    end;
  finally
    TMonitor.Exit(ABlock);
  end;
end;
var
  vTypeIdx : byte;
  vStIdx : word;
begin
  if FDestroying then
  begin
    SwitchToThread;
    exit;
  end;
  TMonitor.Enter(FStations);
  try
    vStIdx := 0;
    oRescanAfter := High(oRescanAfter)-1;
    while vStIdx < FStations.Count do
    begin
      if (FDestroying
      or (not assigned(CommunicationPort))
      or (not CommunicationPort.ReallyActive)
      or TThread.CheckTerminated) then
        break;

      for vTypeIdx := 0 to 3 do
      begin
        if (FDestroying
        or (not assigned(CommunicationPort))
        or (not CommunicationPort.ReallyActive)
        or TThread.CheckTerminated) then
          break;
        case vTypeIdx of
          0:InternalScanBlock(FStations[vStIdx].FCoils);
          1:InternalScanBlock(FStations[vStIdx].FDiscretes);
          2:InternalScanBlock(FStations[vStIdx].FHoldings);
          3:InternalScanBlock(FStations[vStIdx].FInputs);
        end;
      end;
      inc(vStIdx);
    end;
  finally
    TMonitor.Exit(FStations);
  end;
end;

procedure TProtocolDriver.ProtocolErrorCallBack(Result : TProtocolIOResult; APacket : pIOPacket);
begin
  if Assigned(FOnErrorEvent) then
    FOnErrorEvent(Result, APacket);
end;

function  TProtocolDriver.GetPortOpenedEvent: TNotifyEvent;
begin
  Result := DoPortOpened;
end;

function  TProtocolDriver.GetPortClosedEvent: TNotifyEvent;
begin
  Result := DoPortClosed;
end;

function  TProtocolDriver.NotifyThisEvents: TNotifyThisEvents;
begin
  Result := [ntePortOpen, ntePortClosed];
end;

procedure TProtocolDriver.DoPortOpened(ASender: TObject);
begin
  if FDestroying then
    exit;
  CreateThreads;
end;

procedure TProtocolDriver.DoPortClosed(ASender: TObject);
begin
  if FDestroying then
    exit;
  StopThreads;
end;

procedure TProtocolDriver.DoPortRemoved(ASender : TObject);
begin
  if CommunicationPort = ASender then
    CommunicationPort := nil;
end;

procedure TProtocolDriver.PostProtocolError(AError: TProtocolIOResult; AErrorPacket: TCircleQueueItem<TIOPacket>);
begin
  if (not Assigned(FMessageThread)) or (not FMessageThread.PostProtocolError(AError, AErrorPacket)) then
  begin
    if GetCurrentThreadId = MainThreadID then
    try
      ProtocolErrorCallBack(AError, Addr(AErrorPacket.Item));
    finally
      AErrorPacket.Release;
    end else
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        try
          ProtocolErrorCallBack(AError, Addr(AErrorPacket.Item))
        finally
          AErrorPacket.Release;
        end;
      end
      );
    end;
  end;
end;



{ TScanableStation }
constructor TScanableStation.Create(AStation: Word);begin
  inherited Create;
  FStation := AStation;
  FCoils := TObjectList<TAbstractTag>.Create(false);
  FDiscretes := TObjectList<TAbstractTag>.Create(false);
  FHoldings := TObjectList<TAbstractTag>.Create(false);
  FInputs := TObjectList<TAbstractTag>.Create(false);
end;

destructor TScanableStation.Destroy;begin
  FreeAndNil(FCoils);
  FreeAndNil(FDiscretes);
  FreeAndNil(FHoldings);
  FreeAndNil(FInputs);
  inherited Destroy;
end;

function TScanableStation.GetTotalCount: Word;begin
  Result := FCoils.Count
          + FDiscretes.Count
          + FHoldings.Count
          + FInputs.Count;
end;

procedure TScanableStation.AddTag(const ATag : TAbstractTag);var  vIdx : Word;begin
  case ATag.TagType of
    ttDiscrete:
    begin
      vIdx := 0;
      while (vIdx < FDiscretes.Count) do
      begin
        if (FDiscretes[vIdx].MemAddress >= ATag.MemAddress) then
          break;
        inc(vIdx);
      end;
      TMonitor.Enter(FDiscretes);
      try
        FDiscretes.Insert(vIdx, ATag);
      finally
        TMonitor.Exit(FDiscretes);
      end;
    end;
    ttCoil:
    begin
      vIdx := 0;
      while (vIdx < FCoils.Count) do
      begin
        if (FCoils[vIdx].MemAddress >= ATag.MemAddress) then
          break;
        inc(vIdx);
      end;
      TMonitor.Enter(FCoils);
      try
        FCoils.Insert(vIdx, ATag);
      finally
        TMonitor.Exit(FCoils);
      end;
    end;
    ttInput:
    begin
      vIdx := 0;
      while (vIdx < FInputs.Count) do
      begin
        if (FInputs[vIdx].MemAddress >= ATag.MemAddress) then
          break;
        inc(vIdx);
      end;
      TMonitor.Enter(FInputs);
      try
        FInputs.Insert(vIdx, ATag);
      finally
        TMonitor.Exit(FInputs);
      end;
    end;
    ttHolding:
    begin
      vIdx := 0;
      while (vIdx < FHoldings.Count) do
      begin
        if (FHoldings[vIdx].MemAddress >= ATag.MemAddress) then
          break;
        inc(vIdx);
      end;
      TMonitor.Enter(FHoldings);
      try
        FHoldings.Insert(vIdx, ATag);
      finally
        TMonitor.Exit(FHoldings);
      end;
    end;
  end;
end;

procedure TScanableStation.RemoveTag(const ATag : TAbstractTag);procedure InternalDeleteFrom(const ABlock : TObjectList<TAbstractTag>);varvTagIdx : Integer;begin  vTagIdx := ABlock.IndexOf(ATag);
  if vTagIdx <> -1 then
  begin
    TMonitor.Enter(ABlock);
    try
      ABlock.Delete(vTagIdx);
    finally
      TMonitor.Exit(ABlock);
    end;
  end;
end;varvTypeIdx : Byte;begin
  for vTypeIdx := 0 to 3 do
  begin
    case vTypeIdx of
      0:InternalDeleteFrom(FCoils);
      1:InternalDeleteFrom(FDiscretes);
      2:InternalDeleteFrom(FHoldings);
      3:InternalDeleteFrom(FInputs);
    end;
  end;
end;


end.