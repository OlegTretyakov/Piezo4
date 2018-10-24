unit BoardVoltmeter;

interface
uses

  AbstractExtention,
  Vodopad.EventList,
  EventBusInterface,
  mbvg20402Interface,
  dmBoardVoltmeterInterface;
  type

  TBoardVoltmeterExtention = class(TAbstractExtention,
                                  IBoardVoltMeter,
                                  IEventBus)
   private
    fBoardVolmeterObj : TObject;
    fAnalogMeter : Imbvg20402Module;
    fEventSubscribers : TCustomObjEventList;
    procedure OnEvent(Sender : TObject; Event : TGUID; Params : Pointer);
   protected
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: TCustomObjEvent); stdcall;
    {Imbvg20402Module}
    procedure GetVoltageValues(ADest : pBoardPowerValues); stdcall;
   public
  end;

implementation
uses
System.SysUtils, AbstractDeviceInterface,
PositionInterface, ExtentionsListInterface;

{ TBoardVoltmeterExtention }

procedure TBoardVoltmeterExtention.AfterCreate;
var
vDM : IDeviceModules;
vEB : IEventBus;
vIdx : Byte;
begin
  inherited;
  fEventSubscribers := TCustomObjEventList.Create;
  fBoardVolmeterObj := nil;
  fAnalogMeter := nil;
  if Assigned(Owner)
  and Assigned(Owner.Owner)
  and Supports(Owner.Owner, IDeviceModules, vDM) then
  begin
    vIdx := 0;
    if vDM.FindBaseAddr(300, vIdx, fBoardVolmeterObj)
    and Supports(fBoardVolmeterObj, Imbvg20402Module, fAnalogMeter)
    and Supports(fBoardVolmeterObj, IEventBus, vEB) then
      vEB.Add(OnEvent)
    else
      fAnalogMeter := nil;
    vEB := nil;
  end;
  vDM := nil;
end;

procedure TBoardVoltmeterExtention.BeforeDestroy;
var
vEB : IEventBus;
begin
  if Supports(fBoardVolmeterObj, IEventBus, vEB) then
    vEB.Remove(OnEvent);
  vEB := nil;
  fBoardVolmeterObj := nil;
  fAnalogMeter := nil;
  FreeAndNil(fEventSubscribers);
  inherited;
end;

procedure TBoardVoltmeterExtention.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TBoardVoltmeterExtention.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod)
end;

procedure TBoardVoltmeterExtention.GetVoltageValues(ADest: pBoardPowerValues);
begin
  if Assigned(fAnalogMeter) then
    fAnalogMeter.GetVoltageValues(ADest);
end;

procedure TBoardVoltmeterExtention.OnEvent(Sender: TObject; Event: TGUID; Params: Pointer);
begin
  fEventSubscribers.Execute(Self, Event, Pointer(Sender));
end;

end.
