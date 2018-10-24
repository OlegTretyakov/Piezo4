unit PositionList;

interface
uses
  WinApi.Windows, System.SysUtils, System.Generics.Collections, System.Classes, ExtentionsList,
  PositionListInterface, Position, Vodopad.EventList,
  EventBusInterface, ExtentionsListInterface;

  type
  EPositionList = class(Exception);
  TPositionsList = class (TComponent,
                      IPositionsList,
                      IPositionsListExt,
                      IEventBus,
                      IExtentionsEventSink)
   strict private   
    fMaxPositionsCount,
    fActiveCount : Word;
    CS : TRtlCriticalSection;
    fList : TList<TPosition>;
    fExtentions : TExtentions;
    fEventSubscribers : TCustomObjEventList;
   private
    {IPositionsList}
    procedure SetMaxCount(const Value: Word);stdcall;
    function GetMaxCount: Word; stdcall;
    procedure IncActive; stdcall;
    procedure DecActive; stdcall;
    function GetActiveCount: Word; stdcall;
    function GetCount: Word; stdcall;
    function GetItem(AIndex : Word): TObject; stdcall;
    function GetPositionItem(AIndex : Word): TPosition;
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: TCustomObjEvent); stdcall;
    {IExtentionsEventSink}
    procedure IExtentionsEventSink.OnBeforeDelete = IExtentionsEventSink_OnBeforeDelete;
    procedure IExtentionsEventSink.OnAfterDelete = IExtentionsEventSink_OnAfterDelete;
    procedure IExtentionsEventSink.OnAfterAdd = IExtentionsEventSink_OnAfterAdd;
    procedure IExtentionsEventSink_OnBeforeDelete(Sender: TObject; AIndex : Integer); stdcall;
    procedure IExtentionsEventSink_OnAfterDelete(Sender: TObject; AIndex : Integer); stdcall;
    procedure IExtentionsEventSink_OnAfterAdd(Sender: TObject; AIndex : Integer); stdcall;
   protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
   public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    {IPositionsList}
    property ActiveCount : Word read GetActiveCount;
    property Count: Word read GetCount;
    property MaxCount : Word read GetMaxCount write SetMaxCount;
    function AdditionalExtentionClassCount(AClass : Pointer):Word; stdcall;
    function NewItem(BoardPos : Byte; AdditionalExtentions : TAdditionalExtentions=nil):TObject; stdcall;
    function TryBeginUpdate : boolean; stdcall;
    procedure BeginUpdate; stdcall;
    procedure EndUpdate; stdcall;
    function IndexOf(ABoardPos : byte) : integer; stdcall;
    function Exists(ABoardPos : byte) : boolean; stdcall;
    property Items[index : Word] : TObject read GetItem; default;
    function QueryItem(AIndex : Word; const IID : TGUID; out oPosition):Boolean;stdcall;
    procedure Delete(AIndex: Word); stdcall;
    procedure Clear; stdcall;
    function First : TObject;
    function Last: TObject; 
    property Extentions : TExtentions read fExtentions;
  end;

implementation

uses
PositionInterface,
ByListPositionInstallerInterface,
AbstractExtention;



constructor TPositionsList.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fList := TList<TPosition>.Create;
  InitializeCriticalSection(CS);
  fExtentions := TExtentions.Create(Self);
  fEventSubscribers := TCustomObjEventList.Create;
  fActiveCount := 0;
  fMaxPositionsCount := 0; 
end;
  
destructor TPositionsList.Destroy;
begin
  self.Clear;
  fExtentions.Clear;
  FreeAndNil(fEventSubscribers);
  FreeAndNil(fExtentions);
  DeleteCriticalSection(CS);
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure TPositionsList.Clear;
begin
  BeginUpdate;
  try
    while (self.Count > 0) do
      Delete(self.Count - 1);
  finally
    EndUpdate;
  end;
end;

procedure TPositionsList.Delete(AIndex: Word);
var
vPosition : TPosition;
begin
  vPosition := GetPositionItem(AIndex);
  try
    //fEventSubscribers.Execute(Self, C_ListBeforePositionDelete, Pointer(AIndex));
    if assigned(vPosition) and (vPosition.Active) then
      DecActive;
    FreeAndNil(vPosition);
  finally
    fList.Delete(AIndex);
    //fEventSubscribers.Execute(Self, C_ListAfterPositionDelete, Pointer(AIndex));
  end;
end;

function TPositionsList.IndexOf(ABoardPos : byte): integer;
begin
  result := 0;
  while (result < fList.Count) do
  begin
    if (fList.Items[result].BoardPos = ABoardPos) then
      break;
    inc(result);
  end;
  if (result >= Count) then
    result := -1;
end;

function TPositionsList.Exists(ABoardPos: byte): boolean;
begin 
  result := (IndexOf(ABoardPos) <> -1);
end;

function TPositionsList.TryBeginUpdate: boolean;
begin
  result := TryEnterCriticalSection(CS);
  if Result then
    fEventSubscribers.Execute(Self, C_ListBeginUpdate, nil);
end;

procedure TPositionsList.BeginUpdate;
begin
  EnterCriticalSection(CS);
  fEventSubscribers.Execute(Self, C_ListBeginUpdate, nil);
end;  

procedure TPositionsList.EndUpdate;
begin
  LeaveCriticalSection(CS);
  fEventSubscribers.Execute(Self, C_ListEndUpdate, nil);
end;
  
procedure TPositionsList.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TPositionsList.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

procedure TPositionsList.IncActive;
begin
  Inc(fActiveCount);
  fEventSubscribers.Execute(Self, C_ListActivePostionsCountChanged, nil);
end;
  
procedure TPositionsList.DecActive;
begin
  Dec(fActiveCount); 
  fEventSubscribers.Execute(Self, C_ListActivePostionsCountChanged, nil);
end;
  
function TPositionsList.First: TObject;
begin
  result := GetItem(0);
end;
  
function TPositionsList.Last: TObject;
begin
  result := GetItem(fList.count - 1);
end;

function TPositionsList.NewItem(BoardPos : Byte; AdditionalExtentions : TAdditionalExtentions):TObject;
var
vIdx : Integer;
vPosition : TPosition;
i : Word;
vPosListPosExt : IByListPositionInstaller;
vClass : TAbstractExtentionClass;
vPosExtentions : IExtentions;
begin
  vIdx := IndexOf(BoardPos);
  if vIdx <> -1 then
  begin
    result := fList.items[vIdx];
    Exit;
  end;

  vPosition := TPosition.Create(self, BoardPos);
  if Supports(vPosition, IExtentions, vPosExtentions) then
  begin
    if Assigned(AdditionalExtentions)
    and (Length(AdditionalExtentions) > 0) then
      for I := Low(AdditionalExtentions) to High(AdditionalExtentions) do
         vPosExtentions.Install(TAbstractExtentionClass(AdditionalExtentions[i]));
    i := 0;
    while fExtentions.Find(i, IByListPositionInstaller, vPosListPosExt) do
    begin
      vClass := vPosListPosExt.InstalledPositionExtClass;
      if assigned(vClass) then
        vPosExtentions.Install(vClass);
      vPosListPosExt := nil;
      inc(i);
    end;
  end;
  vPosExtentions := nil;
  if vPosition.Active then
    IncActive;
  i := Word(fList.Add(vPosition));
  Result := vPosition;
  fEventSubscribers.Execute(Self, C_ListAfterPositionAdd, Pointer(i));
end; 

function TPositionsList.AdditionalExtentionClassCount(AClass: Pointer): Word;
var
i, vExtIdx : Word;
vClass : TAbstractExtentionClass;
vPosExtentions : IExtentions;
begin
  i := 0;
  Result := 0;
  try
    if not Assigned(AClass) then
      Exit;
    vClass := TAbstractExtentionClass(AClass);
    while (i < fList.Count) do
    begin
      vExtIdx := 0;
      if Supports(fList.Items[i], IExtentions, vPosExtentions)
      and vPosExtentions.Find(vExtIdx, vClass) then
        Inc(result);
      vPosExtentions := nil;
      inc(i);
    end;
  except
    Result := 0;
  end;
end;

procedure TPositionsList.IExtentionsEventSink_OnAfterAdd(Sender: TObject; AIndex: integer);
var
vPosIndex : integer;
vAdded : IByListPositionInstaller;
vInsClass : TAbstractExtentionClass;
vPosExtions : IExtentions;
vIndex : Word;
begin
  vIndex := Word(AIndex);
  if fExtentions.Find(vIndex, IByListPositionInstaller, vAdded) then
  begin
    vInsClass := vAdded.InstalledPositionExtClass;
    if Assigned(vInsClass) then
    begin
      vPosIndex := 0;
      while (vPosIndex < fList.Count) do
      begin
        if Supports(fList.Items[vPosIndex], IExtentions, vPosExtions) then
          vPosExtions.Install(vInsClass);
        vPosExtions := nil;
        inc(vPosIndex);
      end;
    end;
  end;
  vAdded := nil;
end;

procedure TPositionsList.IExtentionsEventSink_OnAfterDelete(Sender: TObject; AIndex: Integer);
begin
end;

procedure TPositionsList.IExtentionsEventSink_OnBeforeDelete(Sender: TObject; AIndex: Integer);
var
vPosIndex : integer;
vRemoved : IByListPositionInstaller;
vRemoveClass : TAbstractExtentionClass;
vPosExtions : IExtentions;   
vIndex : Word;
begin  
  vIndex := Word(AIndex);
  if fExtentions.Find(vIndex, IByListPositionInstaller, vRemoved) then
  begin
    vRemoveClass := vRemoved.InstalledPositionExtClass;
    if Assigned(vRemoveClass) then
    begin
      vPosIndex := 0;
      while (vPosIndex < fList.Count) do
      begin
        if Supports(fList.Items[vPosIndex], IExtentions, vPosExtions) then
          vPosExtions.Delete(vRemoveClass);
        vPosExtions := nil;
        inc(vPosIndex);
      end;
    end;
  end;
  vRemoved := nil;
end;

function TPositionsList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> S_OK) and Supports(fExtentions, IID, Obj) then
    Result := S_OK;
end;

function TPositionsList.QueryItem(AIndex: Word; const IID: TGUID;
  out oPosition): Boolean;
begin
  Result := (Aindex < Count)
      and Supports(fList.items[Aindex], IID, oPosition);
end;

function TPositionsList.GetActiveCount: Word;
begin
  Result := fActiveCount;
end;

function TPositionsList.GetCount: Word;
begin
  Result := Word(fList.Count);
end;

function TPositionsList.GetPositionItem(AIndex: Word): TPosition;
begin
  if AIndex > FList.Count then
    Raise EPositionList.Create(Format('Requested index (%d) out of range. Current positions count = %d',[AIndex, FList.Count]));
  result := fList[AIndex];
end;

function TPositionsList.GetItem(AIndex: Word): TObject;
begin
  if AIndex > FList.Count then
    Raise EPositionList.Create(Format('Requested index (%d) out of range. Current positions count = %d',[AIndex, FList.Count]));
  result := fList[AIndex];
end;

function TPositionsList.GetMaxCount: Word;
begin
  Result := fMaxPositionsCount;
end;

procedure TPositionsList.SetMaxCount(const Value: Word);
begin
  fMaxPositionsCount := Value;
  while (self.Count > fMaxPositionsCount) do
     self.Delete(self.Count-1);
end; 

end.
