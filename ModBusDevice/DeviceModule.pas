unit DeviceModule;

interface
  uses
  System.Classes,
  Vodopad.EventList,  ModBusDeviceInterface,
  DeviceModuleInterface, EventBusInterface;
type

  TDeviceModule = class (TComponent, IDeviceModule, IEventBus)
  protected 
    fID,
    fVersion : Word;
    fBaseAddres : Word;
    fModbusDevice : IModBusDevice;
    fEventSubscribers : TCustomObjEventList;
    {IDeviceModuleInfo}
    function GetID : Word; stdcall;
    function GetVersion : Word; stdcall;
    function GetBaseAddress: Word; stdcall;
    procedure AfterCreate; virtual;
    procedure BeforeDestroy; virtual;
    function GetLibHandle : HModule; stdcall;
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: TCustomObjEvent); stdcall;
  public
    constructor Create(AOwner: TComponent; const AModbusDevice : IModBusDevice; ABaseAddres : Word); reintroduce;
    destructor Destroy; override;
    class procedure CMDStartConditions(const AConditions : TStrings); virtual;
    property ID : Word read GetID;
    property Version: Word read GetVersion;
    property BaseAddress: Word read GetBaseAddress;
  end;

  TDeviceModuleClass = class of TDeviceModule;
implementation
uses System.SysUtils;

{ TDeviceModule }

procedure TDeviceModule.AfterCreate;
begin
end;

procedure TDeviceModule.BeforeDestroy;
begin
end;

class procedure TDeviceModule.CMDStartConditions(const AConditions: TStrings);
begin
  AConditions.Clear;
end;

constructor TDeviceModule.Create(AOwner: TComponent; const AModbusDevice : IModBusDevice; ABaseAddres: Word);
begin
  inherited Create(AOwner);
  fModbusDevice := AModbusDevice;
  fBaseAddres := ABaseAddres;
  fEventSubscribers := TCustomObjEventList.Create;
  AfterCreate;
end;

destructor TDeviceModule.Destroy;
begin
  try
    BeforeDestroy;
  finally
    FreeAndNil(fEventSubscribers);
    fModbusDevice := nil;
    inherited Destroy;
  end;
end;

procedure TDeviceModule.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TDeviceModule.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

function TDeviceModule.GetBaseAddress: Word;
begin
  Result := fBaseAddres;
end;

function TDeviceModule.GetID: Word;
begin
  Result := fID;
end;

function TDeviceModule.GetLibHandle: HModule;
begin
  Result := HInstance;
end;

function TDeviceModule.GetVersion: Word;
begin
  Result := fVersion;
end;

end.
