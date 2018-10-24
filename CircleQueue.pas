unit CircleQueue;

interface
  uses
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs;

  type
   TCircleQueue<T> = class;
   TCircleQueueItem<T> = class(TObject)
   private
    FOwner : TCircleQueue<T>;
   public
    Busy : boolean;
    Item : T;
   constructor Create(const AOwner : TCircleQueue<T>);
   procedure Release;
  end;
  TCircleQueue<T> = class(TObjectList<TCircleQueueItem<T>>)
  protected
    FCS : TCriticalSection;
    fAcquiredCount : Word;
    procedure Lock;
    procedure UnLock;
    procedure InternalRelease(const AItem : TCircleQueueItem<T>);
  public
   constructor Create; reintroduce;
   destructor Destroy; override;
   function Acquire : TCircleQueueItem<T>;
   procedure Release(const AItem : TCircleQueueItem<T>);
   property AcquiredCount : Word read fAcquiredCount;
 end;
implementation

uses System.SysUtils;



{ TItem<T> }

constructor TCircleQueueItem<T>.Create(const AOwner : TCircleQueue<T>);
begin
  FOwner := AOwner;
  Busy := true;
end;

procedure TCircleQueueItem<T>.Release;
begin
  if assigned(FOwner) then
    FOwner.Release(Self);
end;

{ TCircleQueue<T> }

constructor TCircleQueue<T>.Create;
begin
  inherited Create(true);
  FCS := TCriticalSection.Create;
  fAcquiredCount := 0;
end;

destructor TCircleQueue<T>.Destroy;
begin
  Lock;
  try
    Clear;
  finally
    UnLock;
    FreeAndNil(FCS);
    inherited Destroy;
  end;
end;

procedure TCircleQueue<T>.Lock;
begin
  FCS.Enter;
end;

procedure TCircleQueue<T>.UnLock;
begin
  FCS.Release;
end;

function TCircleQueue<T>.Acquire: TCircleQueueItem<T>;
var
vIdx : Word;
begin
  Lock;
  try
    if (Count = 0) then
    begin
      Add(TCircleQueueItem<T>.Create(self));
      result := Last;
      Inc(fAcquiredCount);
      exit;
    end;
    vIdx := 0;
    while vIdx < Count do
    begin
      if not items[vIdx].Busy then
        break;
      inc(vIdx);
    end;
    if vIdx < Count then
    begin
      items[vIdx].Busy := true;
      result := items[vIdx];
      Inc(fAcquiredCount);
    end else
    begin
      Add(TCircleQueueItem<T>.Create(self));
      result := Last;
      Inc(fAcquiredCount);
    end;
  finally
    UnLock;
  end;
end;

procedure TCircleQueue<T>.InternalRelease(const AItem: TCircleQueueItem<T>);
var
vIdx : Word;
begin
  vIdx := 0;
  while vIdx < Count do
  begin
    if (items[vIdx] = AItem) then
    begin
      items[vIdx].Busy := false;
      Dec(fAcquiredCount);
      break;
    end;
    inc(vIdx);
  end;
end;

procedure TCircleQueue<T>.Release(const AItem: TCircleQueueItem<T>);
begin
  Lock;
  try
    InternalRelease(AItem);
  finally
    UnLock;
  end;
end;

end.
