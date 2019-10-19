unit FreqMeasureExtention;

interface
uses
  AbstractExtention,
  dmFreqMeasurerInterface,
  ByListPositionInstallerInterface,
  Vodopad.EventList,
  EventBusInterface;

  type
  TPosListFreqMeasureExtention = class;
  TPositionFreqMeasureExtention = class(TAbstractExtention, IdmPositionFreqResult)
   private
    fBoardPos : Byte;
    fFreqExt : TPosListFreqMeasureExtention;
   protected
    procedure AfterCreate; override;  
    procedure BeforeDestroy; override;
   public
    function FreqResult(const AResult : pFreqResult):Boolean; stdcall;
  end;

  TPosListFreqMeasureExtention = class(TAbstractExtention,
                                  IByListPositionInstaller,
                                  IdmFreqStarter,
                                  IdmFreqController,
                                  IdmFreqResults,
                                  IEventBus)
   private
    fFreqMeterObj :TObject;
    fFreqStarter : IdmFreqStarter;
    fFreqController : IdmFreqController;
    fFreqResults :IdmFreqResults;
    fEventSubscribers : TCustomObjEventList;
    procedure OnEvent(Sender : TObject; Event : TGUID; Params : Pointer);
   protected  
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    {IByListPositionInstaller}
    function InstalledPositionExtClass : TAbstractExtentionClass;stdcall; 
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: TCustomObjEvent); stdcall;
   public
    {IdmFreqStarter}
    function StartOnceMeasure: boolean; overload; stdcall;
    function StartOnceMeasure(ATime{mSec*100}:Word): boolean; overload; stdcall;
    function StartSeriesMeasure: boolean; overload; stdcall;
    function StartSeriesMeasure(ATime{mSec*100}:Word; AQuant : Byte): boolean; overload; stdcall;
    function StartPermanentlyMeasure: boolean; overload; stdcall;
    function StartPermanentlyMeasure(ATime{mSec*100}:Word): boolean; overload; stdcall;
    procedure StopPermanentlyMeasure; stdcall;
    function CalculatedTimeOut : Cardinal;stdcall;
    {IdmFreqController}
    function GetSeriesCount: Byte; stdcall;
    function GetSeriesSuccesCount: Byte; stdcall;
    {IdmFreqResults}
    function FreqResult(AIndex : Word; const AResult : pFreqResult): boolean; stdcall;
  end;


implementation
uses
  System.SysUtils,
  System.DateUtils,
  AbstractDeviceInterface,
  PositionInterface,
  ExtentionsListInterface;



{ TPositionFreqMeasureExtention }

procedure TPositionFreqMeasureExtention.AfterCreate;
var
  vPosition : IPosition;
  vExtList : IExtentions;
begin 
  inherited;
  fFreqExt := nil;
  fBoardPos := 0;
  if Assigned(Owner) and Assigned(Owner.Owner) then
  begin
    if Supports(Owner.Owner, IPosition, vPosition) then
      fBoardPos := vPosition.BoardPos;
    if Assigned(Owner.Owner.Owner)
      and Supports(Owner.Owner.Owner, IExtentions, vExtList) then
    begin
      if not vExtList.Find(TPosListFreqMeasureExtention, fFreqExt) then
        fFreqExt := nil; 
      vExtList := nil;
    end;
  end;
  vPosition := nil;
end; 

procedure TPositionFreqMeasureExtention.BeforeDestroy;
begin
  fFreqExt := nil;
  inherited; 
end;

function TPositionFreqMeasureExtention.FreqResult(const AResult: pFreqResult): Boolean;
begin
  Result := Assigned(fFreqExt) and (fBoardPos > 0);
  if Result then
    Result := fFreqExt.FreqResult(fBoardPos-1, AResult);
end;

{ TPosListMeasureExtention } 
procedure TPosListFreqMeasureExtention.AfterCreate;
var  
  vDM : IDeviceModules;
  vEB : IEventBus;
  vIdx : Byte;
begin
  inherited;
  fEventSubscribers := TCustomObjEventList.Create;
  fFreqMeterObj := nil;
  fFreqStarter := nil;
  fFreqController := nil;
  fFreqResults := nil;
  vIdx := 0;
  if Assigned(Owner)
  and Assigned(Owner.Owner)
  and Assigned(Owner.Owner.Owner)
  and Supports(Owner.Owner.Owner, IDeviceModules, vDM) then
  begin
    if vDM.FindBaseAddr(2000, vIdx, fFreqMeterObj) then
    begin
      if Supports(fFreqMeterObj, IEventBus, vEB) then
        vEB.Add(OnEvent);
      vEB := nil;
      if not Supports(fFreqMeterObj, IdmFreqStarter, fFreqStarter) then
        fFreqStarter := nil;
      if not Supports(fFreqMeterObj, IdmFreqController, fFreqController) then
        fFreqController := nil;
      if not Supports(fFreqMeterObj, IdmFreqResults, fFreqResults) then
        fFreqResults := nil;
    end;
  end;
  vDM := nil;
end;

procedure TPosListFreqMeasureExtention.BeforeDestroy;
var
  vEB : IEventBus;
begin
  if Supports(fFreqMeterObj, IEventBus, vEB) then
    vEB.Remove(OnEvent);
  vEB := nil;
  fFreqStarter := nil;
  fFreqController := nil;
  fFreqResults := nil;
  fFreqMeterObj := nil;
  FreeAndNil(fEventSubscribers);
  inherited; 
end;

function TPosListFreqMeasureExtention.CalculatedTimeOut: Cardinal;
begin
  Result := IdmFreqStarter(fFreqStarter).CalculatedTimeOut;
end;

procedure TPosListFreqMeasureExtention.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TPosListFreqMeasureExtention.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

function TPosListFreqMeasureExtention.FreqResult(AIndex: Word; const AResult: pFreqResult): Boolean;
begin
  AResult.TimeStamp := IncMinute(Now, -100);
  try
    Result := assigned(fFreqResults)
    and fFreqResults.FreqResult(AIndex, AResult);
  except
    Result := false;
  end;
end;

function TPosListFreqMeasureExtention.GetSeriesCount: Byte;
begin
  result := 0;
  if assigned(fFreqController) then
    Result := fFreqController.GetSeriesCount;
end;

function TPosListFreqMeasureExtention.GetSeriesSuccesCount: Byte;
begin
  result := 0;
  if assigned(fFreqController) then
    Result := fFreqController.GetSeriesSuccesCount;
end;

function TPosListFreqMeasureExtention.InstalledPositionExtClass : TAbstractExtentionClass;
begin
  result := TPositionFreqMeasureExtention;
end;

procedure TPosListFreqMeasureExtention.OnEvent(Sender: TObject; Event: TGUID; Params: Pointer);
begin
  fEventSubscribers.Execute(Sender, Event, Params);
end;

function TPosListFreqMeasureExtention.StartOnceMeasure: Boolean;
begin
  Result := assigned(fFreqStarter)
  and fFreqStarter.StartOnceMeasure;
end;

function TPosListFreqMeasureExtention.StartOnceMeasure(ATime: Word): Boolean;
begin
  Result := assigned(fFreqStarter)
  and fFreqStarter.StartOnceMeasure(ATime);
end;

function TPosListFreqMeasureExtention.StartPermanentlyMeasure: Boolean;
begin
  Result := assigned(fFreqStarter)
  and fFreqStarter.StartPermanentlyMeasure;
end;

function TPosListFreqMeasureExtention.StartPermanentlyMeasure(ATime: Word): Boolean;
begin
  Result := assigned(fFreqStarter)
  and fFreqStarter.StartPermanentlyMeasure(ATime);
end;

function TPosListFreqMeasureExtention.StartSeriesMeasure: Boolean;
begin
  Result := assigned(fFreqStarter)
  and fFreqStarter.StartSeriesMeasure;
end;

function TPosListFreqMeasureExtention.StartSeriesMeasure(ATime: Word; AQuant: Byte): Boolean;
begin
  Result := assigned(fFreqStarter)
  and fFreqStarter.StartSeriesMeasure(ATime, AQuant);
end;

procedure TPosListFreqMeasureExtention.StopPermanentlyMeasure;
begin
  if assigned(fFreqStarter)then
    fFreqStarter.StopPermanentlyMeasure;
end;

end.
