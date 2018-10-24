unit AbstractBoard;

interface
 uses
 System.Classes, Vodopad.EventList,
 EventBusInterface, AbstractProtocol, ExtentionsList, dmSerialSetterInterface,
 AbstractExtention, AbstractBoardInterface, PositionList, LoggerInterface, dmLoggerInterface;

 type  


  TAbstractBoard = class (TComponent,
                          IEventBus,
                          IAbstractBoard,
                          ISerialSetter,
                          IBoardEventBusExec,
                          IdmLogger)
   strict private  
    fSerialNum : Word;
    fPositions : TPositionsList;
    fProtocol : TAbstractProtocol;
    fExtentions : TExtentions;
    fEventSubscribers : TCustomObjEventList;
   private
    procedure ProtocolMessage(Sender : TAbstractProtocol; const AStr : string);
    procedure ProtocolError(Sender : TAbstractProtocol; const AStr : string);
    procedure ProtocolReadEvent(Sender: TAbstractProtocol; const AStr: string);
   protected
    {ISerialSetter}
    procedure SetSerial(ASerial : Word);stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall; 
    function QueryPositionListInterface(const IID: TGUID; out Obj): boolean; stdcall;
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: TCustomObjEvent); stdcall;
    {IBoardEventBusExec}
    procedure Execute(Sender : TObject; Event : TGUID; Params : Pointer); stdcall;
    {IAbstractBoard}
    function GetConnected : Boolean; stdcall;
    function GetSerialNum: Word; stdcall;
    {IdmLogger}
    procedure Log(ALevel : TLogInfo; const AStr : string); stdcall;
    function LevelEnabled(ALevel : TLogInfo):boolean; stdcall;
    procedure ProtocolEvents(Sender : TObject; Event : TGUID; Params : Pointer);
   public
    constructor Create(AOwner : TComponent; AProtocol : TAbstractProtocolClass); reintroduce;
    destructor Destroy; override;
    procedure ForEachPosition(AMethod : TForEachPositionMethod; Params : Pointer=nil); stdcall;
    property SerialNum : Word read GetSerialNum;
    property Positions : TPositionsList read fPositions;
    property Extentions : TExtentions read fExtentions;
    property Protocol : TAbstractProtocol read fProtocol;
  end;


implementation
uses
System.SysUtils, BoardProcessInterface;


constructor TAbstractBoard.Create(AOwner : TComponent; AProtocol : TAbstractProtocolClass);
var
vPEB : IEventBus;
begin
  inherited Create(AOwner);
  //fSelfIndex := -1;
  fSerialNum := 0; 
  fEventSubscribers := TCustomObjEventList.Create;
  fPositions := TPositionsList.Create(self);
  fExtentions := TExtentions.Create(self);
  fProtocol := nil;
  if Assigned(AProtocol) then
  begin
    fProtocol := AProtocol.Create(self);
    fProtocol.OnProtocolMessage := ProtocolMessage;
    fProtocol.OnProtocolError := ProtocolError;
    if LevelEnabled(lPortRead) then
      fProtocol.ProtocolPacket := ProtocolReadEvent;
    if Supports(fProtocol, IEventBus, vPEB) then
      vPEB.Add(ProtocolEvents);
    vPEB := nil;
  end;
end;

destructor TAbstractBoard.Destroy;
var
vPEB : IEventBus;
begin
  fPositions.Clear;
  fPositions.Extentions.Clear;
  fExtentions.Clear;
  if Assigned(fProtocol) then
    fProtocol.Close;
  if Supports(fProtocol, IEventBus, vPEB) then
    vPEB.Remove(ProtocolEvents);
  vPEB := nil;
  FreeAndNil(fExtentions);
  FreeAndNil(fPositions);
  FreeAndNil(fProtocol);
  FreeAndNil(fEventSubscribers);
  inherited Destroy;
end;

function TAbstractBoard.GetConnected: Boolean;
begin
  result := fProtocol.Connected;
end;

function TAbstractBoard.GetSerialNum: Word;
begin
  result := fSerialNum;
end;

function TAbstractBoard.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj); 
  if (Result <> S_OK) and Supports(fProtocol, IID, Obj) then
    Result := S_OK;
  if (Result <> S_OK) and Supports(fExtentions, IID, Obj) then
    Result := S_OK;
end;

function TAbstractBoard.QueryPositionListInterface(const IID: TGUID; out Obj): boolean;
begin
  Result := Supports(fPositions, IID, Obj);
end;

procedure TAbstractBoard.SetSerial(ASerial: Word);
begin
  fSerialNum := ASerial;
end;

function TAbstractBoard.LevelEnabled(ALevel: TLogInfo): boolean;
var
vLog : IBoardProcessLog;
begin
  Result := Supports(Owner, IBoardProcessLog, vLog) and (vLog.LevelEnabled(ALevel));
  vLog := nil;
end;

procedure TAbstractBoard.Log(ALevel : TLogInfo; const AStr: string);
var
vLog : IBoardProcessLog;
begin
  if Supports(Owner, IBoardProcessLog, vLog) then
  begin
      vLog.Log(aLevel, Format('Board SN:%d %s',[fSerialNum, AStr]));
    vLog := nil;
  end;
end;

procedure TAbstractBoard.ProtocolMessage(Sender: TAbstractProtocol; const AStr: string);
var
vLog : IBoardProcessLog;
begin
  if (fSerialNum = 0) then
  begin
    if Supports(Owner, IBoardProcessLog, vLog) then
      vLog.Log(lPortRead, Format('Board on:%s %s',[Sender.CommChannel, AStr]));
    vLog := nil;
  end
  else
    Log(lInfo, AStr);
end;

procedure TAbstractBoard.ProtocolReadEvent(Sender: TAbstractProtocol; const AStr: string);
var
vLog : IBoardProcessLog;
begin
  if (fSerialNum = 0) then
  begin
    if Supports(Owner, IBoardProcessLog, vLog) then
      vLog.Log(lPortRead, Format('Board on:%s %s',[Sender.CommChannel, AStr]));
    vLog := nil;
  end
  else
    Log(lPortRead, AStr);
end;

procedure TAbstractBoard.ProtocolError(Sender: TAbstractProtocol; const AStr: string);
var
vLog : IBoardProcessLog;
begin
  if (fSerialNum = 0) then
  begin
    if Supports(Owner, IBoardProcessLog, vLog) then
      vLog.Log(lError, Format('Board on:%s %s',[Sender.CommChannel, AStr]));
    vLog := nil;
  end
  else
    Log(lError, 'Protocol error: ' + AStr);
end;

procedure TAbstractBoard.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TAbstractBoard.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

procedure TAbstractBoard.Execute(Sender: TObject; Event: TGUID;
  Params: Pointer);
begin
  fEventSubscribers.Execute(self, Event, Params);
end;

procedure TAbstractBoard.ProtocolEvents(Sender: TObject; Event: TGUID; Params: Pointer);
begin
  fEventSubscribers.Execute(self, Event, Params);
end;

procedure TAbstractBoard.ForEachPosition(AMethod: TForEachPositionMethod; Params : Pointer);
var vIdx : Word;
begin
  if (fPositions.Count < 1) or (not Assigned(AMethod)) then
    Exit;
  for vIdx := fPositions.Count-1 downto  0 do
     AMethod(fPositions[vIdx], vIdx, Params);
end;



end.
