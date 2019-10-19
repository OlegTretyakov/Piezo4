unit PLCTag;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  AbstractTag,
  ProtocolDriver,
  ProtocolTypes;

type
  TPLCBlockElement = class;
  TPLCTag = class(TAbstractTag, IProtocolPLCTag, IProtocolNotifications)
   private
    FElements : TList<TPLCBlockElement>;
    //: Stores the protocol driver used by tag.
    FProtocolDriver : TProtocolDriver;
    procedure SetMemAddress(const Value: Word);
   protected
    //: Date/time of the last read request of tag.
    FLastReadTimeStamp : TDateTime;
    //: Date/time of the last update of the tag value.
    FValueTimeStamp : TDateTime;
    procedure CallAfterRead; virtual;
    procedure WriteOK; virtual;
    procedure SetDataChanged(ADataChanged : Boolean); override;
    procedure SetLastError(AError : TProtocolIOResult); override;
    {:Enable/disables the automatic tag read.
    @param(Value Boolean: @true enables, @false disables (manual).)
    }
    procedure SetAutoRead(Value : Boolean); virtual;
    {:Sets the scan rate of the tag in milliseconds.
    @param(Value TRefreshTime. Scan rate in milliseconds.)
    }
    procedure SetRefreshInterval(Value : TRefreshTime); virtual;
    function FindElement(DataIndex : word; var oObj : TPLCBlockElement) : Boolean;
    {IProtocolNotifications}
    function GetFireAsync : Boolean;
    procedure NotifyReadOk; override;
    //: Notifies when a read fault occurs.
    procedure NotifyReadFault; override;
    //: Notifies when a successful write occurs.
    procedure NotifyWriteOk; override;
    //: Notifies when a write fault occurs.
    procedure NotifyWriteFault; override;
   public
    //: @exclude
    constructor Create(AOwner: TComponent;
                        const ADriver : TProtocolDriver;
                        AStation,
                        AAddress, ASize : word;
                        AType : TTagType); reintroduce;
    destructor Destroy; override;
    property MemAddress: Word write SetMemAddress;
    function RemainingMilisecondsForNextScan: Cardinal; override;
    function NeedRescan : boolean; override;
    function ReadSyn : boolean; override;
    procedure ReadAsyn; override;
    function WriteSyn : boolean; override;
    procedure WriteAsyn; override;
    //: @seealso(TTag.AutoRead)
    property AutoRead write SetAutoRead;
    //: @seealso(TTag.RefreshTime)
    property RefreshInterval write SetRefreshInterval;
    //: Date/time of the last update of the tag value.
    property ValueTimestamp:TDateTime read FValueTimeStamp;
    //: Called when the protocol driver is being destroyed.
    procedure RemoveDriver; override;
  end;

  TPLCBlockElement = class(TComponent, IProtocolTag, IProtocolNotifications)
   private
    FIndex : word;
    FPLCTag : TPLCTag;
    FDataChanged : Boolean;
    FNotificationInterfaces : array of pointer;
    //: Stores the event to be called when a read has success.
    FOnReadOk : TNotifyEvent;
    //: Stores the event to be called when a read fail occurs.
    FOnReadFail : TNotifyEvent;
    //: Stores the event to be called when a value is written successfully on device.
    FOnWriteOk : TNotifyEvent;
    //: Stores the event called when a write of tag value has a failed.
    FOnWriteFail : TNotifyEvent;
    //: Stores the event called when the tag value changes, BEFORE notify the dependents of the tag.
    FOnValueChange : TNotifyEvent;
    function GetFireAsync: Boolean;
   protected
    procedure CallAfterRead; virtual;
    procedure SetDataChanged(ADataChanged : Boolean);virtual;
    procedure NotifyChange(AChangedIn : TTagChangedIn); virtual;
    procedure NotifyReadFault; virtual;
    procedure NotifyReadOk; virtual;
    procedure NotifyWriteFault; virtual;
    procedure NotifyWriteOk; virtual;
   public
    constructor Create(AOwner : TComponent; const APLCTag : TPLCTag; AIndex : word); reintroduce;
    destructor Destroy; override;
    property PLCTag : TPLCTag read FPLCTag;
    property Index : Word read FIndex;
    //: Event called to notify when a successful read occurs.
    property OnReadOK : TNotifyEvent read FOnReadOk write FOnReadOk;
    //: Event called when a read fault occurs.
    property OnReadFail : TNotifyEvent read FOnReadFail write FOnReadFail;
    //: Event called to notify when a write of the tag value has success.
    property OnWriteOk : TNotifyEvent read FOnWriteOk write FOnWriteOk;
    //: Event called when a write fault occurs.
    property OnWriteFail : TNotifyEvent read FOnWriteFail write FOnWriteFail;
    //: Event called when the tag value changes, AFTER notify all tag dependents.
    property OnValueChange : TNotifyEvent read FOnValueChange  write FOnValueChange;
    property FireAsync : Boolean read GetFireAsync;
    procedure AddCallBacks(const AObj : TObject);
    procedure RemoveCallBacks(const AObj : TObject);
  end;

implementation

uses
  System.DateUtils,
  hsstrings;

constructor TPLCTag.Create(AOwner: TComponent;
                            const ADriver : TProtocolDriver;
                            AStation, AAddress, ASize : word;
                            AType : TTagType);
begin
  inherited Create(AOwner, AStation, AAddress, ASize, AType);
  FValueTimeStamp := System.DateUtils.IncMinute(Now, -10);
  FLastReadTimeStamp := FValueTimeStamp;
  FRefreshInterval:=1000;
  FProtocolDriver:=ADriver;
  FElements := TList<TPLCBlockElement>.Create;
end;

destructor TPLCTag.Destroy;
begin
  RemoveDriver;
  while FElements.Count > 0 do
    FElements.Delete(FElements.Count-1);
  FreeAndNil(FElements);
  inherited Destroy;
end;

function TPLCTag.FindElement(DataIndex: word; var oObj: TPLCBlockElement): Boolean;
var
  i : word;
begin
  i := 0;
  while i < FElements.Count do
  begin
    if FElements[i].Index = DataIndex then
      break;
    inc(i);
  end;
  result := i < FElements.Count;
  if result then
    oObj := FElements[i];
end;

function TPLCTag.GetFireAsync: Boolean;
begin
  result := FFireAsync;
end;

function TPLCTag.NeedRescan: boolean;
begin
  result := FAutoRead and (MilliSecondsBetween(Now, FValueTimeStamp) >= FRefreshInterval);
end;

procedure TPLCTag.NotifyReadFault;
var
  i : word;
begin
  if (not assigned(self))
  or (not Assigned(FProtocolDriver))
  or (not Assigned(FElements))  then
    exit;
  inherited NotifyReadFault;
  i := 0;
  while i < FElements.Count do
  begin
    FElements[i].NotifyReadFault;
    inc(i);
  end;
end;

procedure TPLCTag.NotifyReadOk;
var
  i : word;
begin
  if (not assigned(self))
  or (not Assigned(FProtocolDriver))
  or (not Assigned(FElements))  then
    exit;
  inherited NotifyReadOk;
  i := 0;
  while i < FElements.Count do
  begin
    FElements[i].NotifyReadOk;
    inc(i);
  end;
end;

procedure TPLCTag.NotifyWriteFault;
var
  i : word;
begin
  if (not assigned(self))
  or (not Assigned(FProtocolDriver))
  or (not Assigned(FElements))  then
    exit;
  inherited NotifyWriteFault;
  i := 0;
  while i < FElements.Count do
  begin
    FElements[i].NotifyWriteFault;
    inc(i);
  end;
end;

procedure TPLCTag.NotifyWriteOk;
var
  i : word;
begin
  if (not assigned(self))
  or (not Assigned(FProtocolDriver))
  or (not Assigned(FElements))  then
    exit;
  inherited NotifyWriteOk;
  i := 0;
  while i < FElements.Count do
  begin
    FElements[i].NotifyWriteOk;
    inc(i);
  end;
end;

procedure TPLCTag.RemoveDriver;
begin
  if (FProtocolDriver<>nil) then
    FProtocolDriver.RemoveTag(self);
  FProtocolDriver := nil;
end;

procedure TPLCTag.SetAutoRead(Value : Boolean);
begin
  if FAutoRead=Value then
    exit;

  FAutoRead := Value;

  if (FProtocolDriver<>nil) then
  begin
    if Value then
    begin
      FProtocolDriver.AddTagToScan(self)
    end else
      FProtocolDriver.RemoveTagFromScan(self);
  end;
end;

procedure TPLCTag.SetDataChanged(ADataChanged : Boolean);
var
  i : Word;
begin
  inherited;
  if not ADataChanged then
  begin
    i := 0;
    while i < FElements.Count do
    begin
      FElements[i].SetDataChanged(ADataChanged);
      inc(i);
    end;
  end;
end;

procedure TPLCTag.SetLastError(AError : TProtocolIOResult);
begin
  inherited SetLastError(AError);
  if (AError = ioOK) then
    FValueTimeStamp := now;
end;

procedure TPLCTag.SetMemAddress(const Value : Word);
begin
  if FAddress=Value then
    exit;

  if (FProtocolDriver<>nil) and FAutoRead then
    FProtocolDriver.RemoveTagFromScan(Self);

  FAddress := Value;

  if (FProtocolDriver<>nil) and FAutoRead then
    FProtocolDriver.AddTagToScan(Self);
end;

procedure TPLCTag.SetRefreshInterval(Value : TRefreshTime);
begin
  if FRefreshInterval=Value then
    exit;

  if (FProtocolDriver<>nil) and FAutoRead then
    FProtocolDriver.RemoveTagFromScan(Self);

  FRefreshInterval := Value;

  if (FProtocolDriver<>nil) and FAutoRead then
    FProtocolDriver.AddTagToScan(Self);
end;

procedure TPLCTag.CallAfterRead;
begin
  FLastReadTimeStamp := now;
end;

procedure TPLCTag.WriteAsyn;
begin
  if assigned(FProtocolDriver)
  and (TagType in [ttCoil, ttHolding]) then
    FProtocolDriver.WriteAsyn(self);
end;

procedure TPLCTag.WriteOK;
begin
end;

function TPLCTag.WriteSyn : boolean;
begin
  result := false;
  if assigned(FProtocolDriver)
  and (TagType in [ttCoil, ttHolding])  then
    result := FProtocolDriver.WriteSyn(self) = ioOK;
end;

procedure TPLCTag.ReadAsyn;
begin
  if assigned(FProtocolDriver) then
    FProtocolDriver.ReadAsyn(self);
end;

function TPLCTag.ReadSyn : boolean;
begin
  result := false;
  if assigned(FProtocolDriver) then
    result := FProtocolDriver.ReadSyn(self) = ioOK;
end;

function TPLCTag.RemainingMilisecondsForNextScan: Cardinal;
var
  vMs : Int64;
begin
  vMS := MilliSecondsBetween(Now, FLastReadTimeStamp);
  if FRefreshInterval > vMS then
    Result := FRefreshInterval - vMS
  else
    result := 0;
end;

{TPLCBlockElement}

constructor TPLCBlockElement.Create(AOwner : TComponent; const APLCTag : TPLCTag; AIndex : word);
begin
  inherited Create(AOwner);
  FPLCTag := APLCTag;
  FIndex := AIndex;
end;

destructor TPLCBlockElement.Destroy;
var
  i : integer;
begin
  if Assigned(FPLCTag) then
  begin
    i := FPLCTag.FElements.IndexOf(Self);
    if i <> -1 then
      FPLCTag.FElements.Delete(i);
  end;
  for i := 0 to High(FNotificationInterfaces) do
    IHMITagInterface(FNotificationInterfaces[i]).RemoveTag(Self);
  SetLength(FNotificationInterfaces, 0);
  FPLCTag := nil;
  inherited Destroy;
end;

function TPLCBlockElement.GetFireAsync: Boolean;
begin
  result := Assigned(FPLCTag) and FPLCTag.FireAsync;
end;

procedure TPLCBlockElement.AddCallBacks(const AObj:TObject);
var
  vObj : IHMITagInterface;
  i : integer;
  vpTag : Pointer;
begin
  if not Supports(AObj, IHMITagInterface, vObj) then
    raise Exception.Create(SinvalidInterface);
  vpTag := Pointer(vObj);
  vObj := nil;
  i := 0;
  while (i < Length(FNotificationInterfaces)) do
  begin
    if (vpTag = FNotificationInterfaces[i]) then
      break;
    Inc(i);
  end;

  if i < Length(FNotificationInterfaces) then  //already exists
    Exit;

  SetLength(FNotificationInterfaces, Length(FNotificationInterfaces)+1);
  FNotificationInterfaces[High(FNotificationInterfaces)]:= vpTag;
end;

procedure TPLCBlockElement.RemoveCallBacks(const AObj:TObject);
var
  i,h : integer;
  found : Boolean;
  vObj : IHMITagInterface;
begin
  if not Supports(AObj, IHMITagInterface, vObj) then
    Exit;
  found:=false;
  h := High(FNotificationInterfaces);
  for i := 0 to h do
  begin
    if (Pointer(vObj)=FNotificationInterfaces[i]) then
    begin
      found := true;
      break;
    end;
  end;
  vObj := nil;
  if found then
  begin
    FNotificationInterfaces[i] := FNotificationInterfaces[h];
    FNotificationInterfaces[h] := nil;
    SetLength(FNotificationInterfaces, Length(FNotificationInterfaces)-1);
  end;
end;


procedure TPLCBlockElement.SetDataChanged(ADataChanged: Boolean);
begin
  FDataChanged := ADataChanged;
end;

procedure TPLCBlockElement.CallAfterRead;
begin

end;

procedure TPLCBlockElement.NotifyChange(AChangedIn : TTagChangedIn);
var
  i : integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  //Notify the dependent objects.
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyTagChange(self, AChangedIn);
    except
    end;
  end;
  try
    if Assigned(FOnValueChange) then
    begin
      n := FOnValueChange;
      n(Self);
    end;
  except
  end;
end;

procedure TPLCBlockElement.NotifyReadOk;
var
  i : integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyReadOk(self);
    except
    end;
  end;

  if Assigned(FOnReadOk) then
  begin
    n := FOnReadOk;
    n(self);
  end;
  if FDataChanged then
    NotifyChange(chInRead);
end;

procedure TPLCBlockElement.NotifyReadFault;
var
  i : integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyReadFault(self);
    except
    end;
  end;

  if Assigned(FOnReadFail) then
  begin
    n := FOnReadFail;
    n(self);
  end;
end;

procedure TPLCBlockElement.NotifyWriteOk;
var
  i : integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyWriteOk(self);
    except
    end;
  end;

  if Assigned(FOnWriteOk) then
  begin
    n := FOnWriteOk;
    n(self);
  end;
  if FDataChanged then
    NotifyChange(chInWrite);
end;

procedure TPLCBlockElement.NotifyWriteFault;
var
  i : integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyWriteFault(self);
    except
    end;
  end;

  if Assigned(FOnWriteFail) then
  begin
    n := FOnWriteFail;
    n(self);
  end;
end;

end.
