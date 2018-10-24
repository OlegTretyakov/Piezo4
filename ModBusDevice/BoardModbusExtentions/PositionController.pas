unit PositionController;

interface
  uses AbstractExtention, ByListPositionInstallerInterface,
  Vodopad.EventList, EventBusInterface, dmPositionControllerInterface;
  type
  TPosListPositionControllerExtention = class;
  TPositionControllerExtention = class(TAbstractExtention, IdmPositionController)
   private
    fBoardPos : Byte;
    fControllerExt : TPosListPositionControllerExtention;
   protected
    procedure AfterCreate; override; 
    procedure BeforeDestroy; override;
    {IdmPositionController}
    function GetProtectState:Boolean;stdcall;
    function GetEnabled:Boolean; stdcall;
    procedure SetEnabled(Value : boolean); stdcall;
    function GetProtectTimeStamp:TDateTime;stdcall;
    function GetEnabledTimeStamp:TDateTime; stdcall;
   public
    property Enabled:Boolean read GetEnabled write SetEnabled;
    property ProtectState:Boolean read GetProtectState;    
    property EnabledTimeStamp:TDateTime read GetEnabledTimeStamp;
    property ProtectTimeStamp:TDateTime read GetProtectTimeStamp;
  end;

  TPosListPositionControllerExtention = class(TAbstractExtention,
                                  IByListPositionInstaller,
                                  IdmPositionsController,
                                  IEventBus)
   private
    fControllerObj :TObject;
    fController : IdmPositionsController;
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
    {IdmPositionsController}
    procedure GetVoltageValues(ADest : pPositionsVoltages); stdcall;
    procedure SetVoltageValues(ASource : pPositionsVoltages);stdcall;
    function GetProtectState(AIndex : word):Boolean;stdcall;
    function GetEnabled(AIndex : word):Boolean;stdcall;
    procedure SetEnabled(AIndex : word; Value : boolean);stdcall;
    function GetProtectTimeStamp:TDateTime;stdcall;
    function GetEnabledTimeStamp:TDateTime; stdcall;
    property EnabledTimeStamp:TDateTime read GetEnabledTimeStamp;
    property ProtectTimeStamp:TDateTime read GetProtectTimeStamp;
   public
  end;
implementation
uses System.SysUtils, AbstractDeviceInterface,
PositionListInterface, PositionInterface, ExtentionsListInterface,
dmVoltageConsts, System.DateUtils;

{ TPosListPositionControllerExtention }

procedure TPosListPositionControllerExtention.AfterCreate;
var  
vDM : IDeviceModules;
vEB : IEventBus;
vIdx : Byte;
begin
  inherited;
  fEventSubscribers := TCustomObjEventList.Create;
  fControllerObj := nil;
  fController := nil;
  if Assigned(Owner)
  and Assigned(Owner.Owner)
  and Assigned(Owner.Owner.Owner)
  and Supports(Owner.Owner.Owner, IDeviceModules, vDM) then
  begin
    vIdx := 0;
    if vDM.FindBaseAddr(1000, vIdx, fControllerObj) then
    begin
      if Supports(fControllerObj, IEventBus, vEB) then
        vEB.Add(OnEvent);
      if not Supports(fControllerObj, IdmPositionsController, fController)then
        fController := nil;
    end;
  end;
  vDM := nil;
  vEB := nil;
end;

procedure TPosListPositionControllerExtention.BeforeDestroy;
var
vEB : IEventBus;
begin
  if Supports(fControllerObj, IEventBus, vEB) then
    vEB.Remove(OnEvent);
  vEB := nil;
  fController := nil;
  fControllerObj := nil; 
  FreeAndNil(fEventSubscribers);
  inherited;
end;

procedure TPosListPositionControllerExtention.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TPosListPositionControllerExtention.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

function TPosListPositionControllerExtention.GetEnabled(AIndex: word): Boolean;
begin
  Result := assigned(fController) and fController.Enabled[AIndex];
end;

function TPosListPositionControllerExtention.GetEnabledTimeStamp: TDateTime;
begin
  if assigned(fController) then
    Result := fController.EnabledTimeStamp
  else
    Result := IncMinute(Now, -10);
end;

function TPosListPositionControllerExtention.GetProtectState(AIndex: word): Boolean;
begin
  Result := assigned(fController) and fController.ProtectState[AIndex];
end;

function TPosListPositionControllerExtention.GetProtectTimeStamp: TDateTime;
begin
  if assigned(fController) then
    Result := fController.ProtectTimeStamp
  else
    Result := IncMinute(Now, -10);
end;

procedure TPosListPositionControllerExtention.GetVoltageValues(ADest: pPositionsVoltages);
begin
  ADest.TimeStamp := IncMinute(Now, -10);
  if assigned(fController) then
     fController.GetVoltageValues(ADest);
end;

function TPosListPositionControllerExtention.InstalledPositionExtClass: TAbstractExtentionClass;
begin
  Result := TPositionControllerExtention;
end;

procedure TPosListPositionControllerExtention.OnEvent(Sender: TObject; Event: TGUID; Params: Pointer);
{var
i : Word;
vList : IPositionsList;
vPosition : IPosition; }
begin
  { не нужно!!! Active должно работать только в одну сторону: позиция -> плата
  if IsEqualGUID(Event, C_OnEnabledChanged) then
  begin
    if Assigned(Owner)
    and supports(Owner.Owner, IPositionsList, vList)
    and assigned(fController) then
    begin
      i := 0;
      while i < vList.Count do
      begin
        if vList.QueryItem(i, IPosition, vPosition) then
          vPosition.Active := IdmPositionsController(fController).Enabled[vPosition.BoardPos-1];
        vPosition := nil;
        Inc(i);
      end;
    end;
  end; }
  fEventSubscribers.Execute(Sender, Event, Params);
end;

procedure TPosListPositionControllerExtention.SetEnabled(AIndex: word; Value: boolean);
begin
  if assigned(fController) then
     fController.Enabled[AIndex] := Value;
end;

procedure TPosListPositionControllerExtention.SetVoltageValues(ASource: pPositionsVoltages);
begin
  if assigned(fController) then
     fController.SetVoltageValues(ASource);
end;

{ TPositionControllerExtention }

procedure TPositionControllerExtention.AfterCreate;
var
vPosition : IPosition;
vExtList : IExtentions;
begin
  inherited;
  fControllerExt := nil;
  vPosition := nil;
  fBoardPos := 0;
  if Assigned(Owner) and Assigned(Owner.Owner) then
  begin
    if Supports(Owner.Owner, IPosition, vPosition) then
      fBoardPos := vPosition.BoardPos;
    if Assigned(Owner.Owner.Owner)
      and Supports(Owner.Owner.Owner, IExtentions, vExtList)
      and vExtList.Find(TPosListPositionControllerExtention, fControllerExt)
      and Assigned(vPosition) then
        vPosition.Active := GetEnabled;
  end;
  vPosition := nil;
  vExtList := nil;
end;

procedure TPositionControllerExtention.BeforeDestroy;
begin
  fControllerExt := nil;
  inherited;  
end;

function TPositionControllerExtention.GetEnabled: Boolean;
begin
  Result := Assigned(fControllerExt)
    and (fBoardPos > 0)
    and fControllerExt.GetEnabled(fBoardPos-1);
end;

function TPositionControllerExtention.GetEnabledTimeStamp: TDateTime;
begin
  if Assigned(fControllerExt)
    and (fBoardPos > 0) then
    Result := fControllerExt.GetEnabledTimeStamp
  else
   Result := IncMinute(Now, -10);
end;

function TPositionControllerExtention.GetProtectState: Boolean;
begin
  Result := Assigned(fControllerExt)
    and (fBoardPos > 0)
    and fControllerExt.GetProtectState(fBoardPos-1);
end;

function TPositionControllerExtention.GetProtectTimeStamp: TDateTime;
begin
  if Assigned(fControllerExt)
    and (fBoardPos > 0) then
    Result := fControllerExt.GetProtectTimeStamp
  else
   Result := IncMinute(Now, -10);
end;

procedure TPositionControllerExtention.SetEnabled(Value: boolean);
begin
  if Assigned(fControllerExt) and (fBoardPos > 0) then
     fControllerExt.SetEnabled(fBoardPos-1, Value);
end;

end.
