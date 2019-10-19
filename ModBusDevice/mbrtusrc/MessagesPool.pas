unit MessagesPool;

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  CircleQueue;

type

  TMSMsg = record
    MsgID : Cardinal;
    lParam : Pointer;
    wParam : Pointer;
  end;
  MessagesPoolException = class(Exception);

  TMessagesPool = class(TCircleQueue<TMSMsg>)
   private
    function AcquireHigh: TCircleQueueItem<TMSMsg>;
    function AcquireLower: TCircleQueueItem<TMSMsg>;
   public
    function PeekMessage(var oMsg : TMSMsg; AFilterMinMsg, AFilterMaxMsg : Cardinal): Boolean;
    function PostMessage(MsgID : Cardinal; wParam, lParam : Pointer; Priority : Boolean) : boolean;
    procedure RemoveMessage(wParam, lParam:Pointer);
  end;

implementation

uses
  hsstrings;

function TMessagesPool.AcquireHigh: TCircleQueueItem<TMSMsg>;
var
  vIdx : Word;
begin
  Lock;
  try
    if (Count = 0) then
    begin
      Add(TCircleQueueItem<TMSMsg>.Create(self));
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
      Insert(0, TCircleQueueItem<TMSMsg>.Create(self));
      result := items[0];
      Inc(fAcquiredCount);
    end;
  finally
    UnLock;
  end;
end;

function TMessagesPool.AcquireLower: TCircleQueueItem<TMSMsg>;
var
  vIdx : integer;
begin
  Lock;
  try
    if (Count = 0) then
    begin
      Add(TCircleQueueItem<TMSMsg>.Create(self));
      result := Last;
      Inc(fAcquiredCount);
      exit;
    end;
    vIdx := Count - 1;
    while vIdx > -1 do
    begin
      if not items[vIdx].Busy then
        break;
      dec(vIdx);
    end;
    if (vIdx > -1) then
    begin
      items[vIdx].Busy := true;
      result := items[vIdx];
      Inc(fAcquiredCount);
    end else
    begin
      Add(TCircleQueueItem<TMSMsg>.Create(self));
      result := Last;
      Inc(fAcquiredCount);
    end;
  finally
    UnLock;
  end;
end;

function TMessagesPool.PeekMessage(var oMsg : TMSMsg; AFilterMinMsg, AFilterMaxMsg : Cardinal): Boolean;
var
  vIdx : Word;
  vItem : TCircleQueueItem<TMSMsg>;
begin
  Result := false;
  Lock;
  try
    if (AcquiredCount < 1) then
      exit;
    vIdx := 0;
    vItem := nil;
    while vIdx < Count do
    begin
      vItem := items[vIdx];
      if (vItem.Busy) then
      begin
        if (AFilterMinMsg > 0) and (AFilterMaxMsg >= AFilterMinMsg) then
        begin
          if (vItem.Item.MsgID >= AFilterMinMsg) and (vItem.Item.MsgID <= AFilterMaxMsg) then
            break;
        end else
          break;
      end;
      inc(vIdx);
    end;
    result := (vIdx < Count) and assigned(vItem);
    if result then
    begin
      oMsg.MsgID := vItem.Item.MsgID;
      oMsg.lParam := vItem.Item.lParam;
      oMsg.wParam := vItem.Item.wParam;
      InternalRelease(vItem);
    end;
  finally
    UnLock;
  end;
end;

function TMessagesPool.PostMessage(MsgID : Cardinal; wParam, lParam : Pointer; Priority : Boolean) : boolean;
var
  vIdx : Word;
  vItem : TCircleQueueItem<TMSMsg>;
begin
  Lock;
  try
    result := self.Count < 5000;
    if not result then
       raise MessagesPoolException.Create(SoutOfMemory);

    vIdx := 0;
    while vIdx < Count do
    begin
      if (items[vIdx].Busy)
      and (items[vIdx].Item.MsgID = MsgID)
      and (items[vIdx].Item.wParam = wParam)
      and (items[vIdx].Item.lParam = lParam) then
      begin
        result := False;
        break;
      end;
      inc(vIdx);
    end;
    if not result then //message exists
      Exit;
    if not Priority then
      vItem := AcquireLower
    else
      vItem := AcquireHigh;
    vItem.Item.MsgID := MsgID;
    vItem.Item.wParam := wParam;
    vItem.Item.lParam := lParam;
  finally
    UnLock;
  end;
end;

procedure TMessagesPool.RemoveMessage(wParam, lParam: Pointer);
var
  vIdx : Word;
begin
  if not Assigned(Self) then
    Exit;
  Lock;
  try
    vIdx := 0;
    while vIdx < Count do
    begin
      if (items[vIdx].Busy) then
      begin
        if ((wParam <> nil) and (items[vIdx].Item.wParam = wParam)) then
          items[vIdx].Busy := false
        else if ((lParam <> nil) and (items[vIdx].Item.lParam = lParam)) then
        begin
          items[vIdx].Busy := false
        end;
      end;
      inc(vIdx);
    end;
  finally
    UnLock;
  end;
end;

end.

