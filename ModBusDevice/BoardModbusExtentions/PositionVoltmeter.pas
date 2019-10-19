unit PositionVoltmeter;

interface
  uses
    AbstractExtention,
    ByListPositionInstallerInterface,
    Vodopad.EventList,
    EventBusInterface,
    dmPositionVoltmeterInterface,
    mbvg20401Interface;

  type

  TPosListPositionVoltmeterExtention = class;
  TPositionVoltmeterExtention = class(TAbstractExtention, IPositionVoltages)
   private
    fBoardPos : Byte;
    fVoltmeterExt : TPosListPositionVoltmeterExtention;
   protected
    procedure AfterCreate; override; 
    procedure BeforeDestroy; override;
   public
    {IPositionVoltages}
    procedure GetPositionVoltage(ADest : pPositionVoltages); stdcall;
  end;

  TPosListPositionVoltmeterExtention = class(TAbstractExtention,
                                  IByListPositionInstaller,
                                  IPositionsVoltages,
                                  IEventBus)
   private  
    fVDDmeterObj,
    fVCMeterObj,
    fAnalogMeterObj :TObject;
    fVDDmeter : Imbvg20401Module;
    fVCMeter : Imbvg20401Module;
    fAnalogMeter : Imbvg20401Module;
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
    {IByListPositionInstaller}
    function InstalledPositionExtClass : TAbstractExtentionClass;stdcall;
    {IPositionsVoltages}
    procedure GetPositionVoltage(AIndex : Word; ADest : pPositionVoltages); stdcall;
   public
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  AbstractDeviceInterface,
  PositionInterface,
  ExtentionsListInterface,
  dmChallengeControllerInterface;

{ TPosListPositionVoltmeterExtention }

procedure TPosListPositionVoltmeterExtention.AfterCreate;
var  
  vDM : IDeviceModules;
  vEB : IEventBus;
  vIdx : Byte;
begin
  inherited;
  fEventSubscribers := TCustomObjEventList.Create;
  fVDDmeter := nil;
  fVCMeter := nil;
  fAnalogMeter := nil;
  if Assigned(Owner)
  and Assigned(Owner.Owner)
  and Assigned(Owner.Owner.Owner)
  and Supports(Owner.Owner.Owner, IDeviceModules, vDM) then
  begin
    vIdx := 0;
    if vDM.FindBaseAddr(3000, vIdx, fVDDmeterObj)
    and Supports(fVDDmeterObj, Imbvg20401Module, fVDDmeter)
    and Supports(fVDDmeterObj, IEventBus, vEB) then
    begin
      vEB.Add(OnEvent);
    end;
    vEB := nil;

    vIdx := 0;
    if vDM.FindBaseAddr(4000, vIdx, fVCMeterObj)
    and Supports(fVCMeterObj, Imbvg20401Module, fVCMeter)
    and Supports(fVCMeterObj, IEventBus, vEB)then
    begin
      vEB.Add(OnEvent);
    end;
    vEB := nil;

    vIdx := 0;
    if vDM.FindBaseAddr(5000, vIdx, fAnalogMeterObj)
    and Supports(fAnalogMeterObj, Imbvg20401Module, fAnalogMeter)
    and Supports(fAnalogMeterObj, IEventBus, vEB)then
    begin
      vEB.Add(OnEvent);
    end;
    vEB := nil;
  end;
  vDM := nil;  
end;

procedure TPosListPositionVoltmeterExtention.BeforeDestroy;
var
  vEB : IEventBus;
begin
  if Supports(fAnalogMeterObj, IEventBus, vEB) then
    vEB.Remove(OnEvent);
  vEB := nil; 
  if Supports(fVCMeterObj, IEventBus, vEB) then
    vEB.Remove(OnEvent);
  vEB := nil;
  if Supports(fVDDmeterObj, IEventBus, vEB) then
    vEB.Remove(OnEvent);
  vEB := nil;
  fVDDmeter := nil;
  fVCMeter := nil;
  fAnalogMeter := nil;
  fAnalogMeterObj := nil;
  fVCMeterObj := nil;
  fVDDmeterObj := nil;
  FreeAndNil(fEventSubscribers);
  inherited;
end;

procedure TPosListPositionVoltmeterExtention.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TPosListPositionVoltmeterExtention.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod)
end;

procedure TPosListPositionVoltmeterExtention.GetPositionVoltage(AIndex: Word; ADest: pPositionVoltages);
begin
  try
    ADest.VDDTimeStamp := IncMinute(Now ,-100);
    ADest.VCTimeStamp := IncMinute(Now ,-100);
    ADest.AnalogTimeStamp := IncMinute(Now ,-100);
    if Assigned(fVDDmeter) then
    begin
      ADest.VDD := fVDDmeter.Voltage[AIndex];
      ADest.VDDTimeStamp := fVDDmeter.TimeStamp[AIndex];
    end;
    if Assigned(fVCMeter) then
    begin
      ADest.VC := fVCMeter.Voltage[AIndex];
      ADest.VCTimeStamp := fVCMeter.TimeStamp[AIndex];
    end;
    if Assigned(fAnalogMeter) then
    begin
      ADest.Analog := fAnalogMeter.Voltage[AIndex];
      ADest.AnalogTimeStamp := fAnalogMeter.TimeStamp[AIndex];
    end;
  except
  end;
end;

function TPosListPositionVoltmeterExtention.InstalledPositionExtClass: TAbstractExtentionClass;
begin
  Result := TPositionVoltmeterExtention;
end;

procedure TPosListPositionVoltmeterExtention.OnEvent(Sender: TObject; Event: TGUID; Params: Pointer);
begin
  fEventSubscribers.Execute(Sender, Event, Params);
end;

{ TPositionVoltmeterExtention }

procedure TPositionVoltmeterExtention.AfterCreate;
var
  vPosition : IPosition;
  vExtList : IExtentions;
begin
  inherited;
  fVoltmeterExt := nil;
  fBoardPos := 0;
  if Assigned(Owner) and Assigned(Owner.Owner) then
  begin
    if Supports(Owner.Owner, IPosition, vPosition) then
      fBoardPos := vPosition.BoardPos;
    if Assigned(Owner.Owner.Owner)
      and Supports(Owner.Owner.Owner, IExtentions, vExtList) then
      if not vExtList.Find(TPosListPositionVoltmeterExtention, fVoltmeterExt) then
        fVoltmeterExt := nil;
  end;
  vPosition := nil;
  vExtList := nil; 
end;

procedure TPositionVoltmeterExtention.BeforeDestroy;
begin
  fVoltmeterExt := nil;
  inherited;
end;

procedure TPositionVoltmeterExtention.GetPositionVoltage(ADest: pPositionVoltages);
begin
  if Assigned(fVoltmeterExt) and (fBoardPos > 0) then
    fVoltmeterExt.GetPositionVoltage(fBoardPos-1, ADest);
end;

end.
