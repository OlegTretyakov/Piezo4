unit DriverMessageThread;


interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  ProtocolTypes,
  MessageSpool,
  commtypes,
  CircleQueue,
  AbstractTag;

type
  TSynMessageThreadEvents = record
    MessageEvent,
    StopEvent : THandle;
  end;
  TMessageThread = class(TThread)
  private
    FProtocolErrorEvent : TProtocolErrorEvent;
    FEvents : TSynMessageThreadEvents;
    FPool:TMessageSpool;
  protected
    procedure Execute; override;
  public
    constructor Create(AProtocolErrorCalBack : TProtocolErrorEvent; AOnTerminateNotify : TNotifyEvent);
    destructor Destroy; override;
    procedure Stop;
    function PostProtocolError(AError:TProtocolIOResult; AErrorPacket : TCircleQueueItem<TIOPacket>):boolean;
    procedure ScanRead(const TagObj : TAbstractTag);
    procedure ScanWrite(const TagObj : TAbstractTag);
    procedure RemoveTag(const TagObj : TAbstractTag);
  end;

implementation

uses
  WinApi.Windows;

constructor TMessageThread.Create(AProtocolErrorCalBack : TProtocolErrorEvent; AOnTerminateNotify : TNotifyEvent);
begin
  FProtocolErrorEvent := AProtocolErrorCalBack;
  OnTerminate := AOnTerminateNotify;
  FPool := TMessageSpool.Create;
  //Priority := tpTimeCritical;
  FreeOnTerminate := True;
  inherited Create(false);
end;

destructor TMessageThread.Destroy;
begin
  FreeAndNil(FPool);
  inherited Destroy;
end;

procedure TMessageThread.Stop;
begin 
  OnTerminate := nil;
  FreeOnTerminate := false;
  Terminate;
  SetEvent(FEvents.StopEvent);
  WaitFor;
end;

procedure TMessageThread.Execute;
var
  vMsg : TMSMsg;
  vTagObj : TAbstractTag;
  vErrorPacket : TCircleQueueItem<TIOPacket>;
  vNotifications : IProtocolNotifications;
begin
  fEvents.MessageEvent := CreateEvent(nil, False, False, nil);
  fEvents.StopEvent := CreateEvent(nil, False, False, nil);
  {$IFDEF DEBUG}
    TThread.NameThreadForDebugging('Modbus driver message thread');
  {$ENDIF}
  try
    while not Terminated do
    begin
      case WaitForMultipleObjects(2, @fEvents, False, INFINITE) of
        WAIT_OBJECT_0:
        begin
          while (not Terminated) and
                FPool.PeekMessage(vMsg, PSM_TAGSCANREAD, PSM_PROTOCOL_ERROR) do
          begin
            case vMsg.MsgID of
              PSM_TAGSCANWRITE:
              begin
                vTagObj := TAbstractTag(vMsg.wParam);
                if supports(vTagObj, IProtocolNotifications, vNotifications) then
                try
                  if vNotifications.FireAsync then
                  begin
                    try  
                      if vTagObj.LastError = ioOk then
                        vNotifications.NotifyWriteOk
                      else
                        vNotifications.NotifyWriteFault;
                    except
                    end;
                  end else
                  begin
                    Synchronize(
                    procedure
                    begin
                      try 
                        if vTagObj.LastError = ioOk then
                          vNotifications.NotifyWriteOk
                        else
                          vNotifications.NotifyWriteFault;
                      except
                      end;
                    end
                    );
                  end;
                finally
                  vNotifications := nil;
                end;
              end;
              PSM_TAGSCANREAD:
              begin
                vTagObj := TAbstractTag(vMsg.wParam);
                if supports(vTagObj, IProtocolNotifications, vNotifications) then
                try
                  if vNotifications.FireAsync then
                  begin
                    try
                      if vTagObj.LastError = ioOk then
                        vNotifications.NotifyReadOk
                      else
                        vNotifications.NotifyReadFault;
                    except
                    end;
                  end else
                  begin
                    Synchronize(
                    procedure
                    begin
                      try
                        if vTagObj.LastError = ioOk then
                          vNotifications.NotifyReadOk
                        else
                          vNotifications.NotifyReadFault;
                      except
                      end;
                    end
                    );
                  end;
                finally
                  vNotifications := nil;
                end;
              end;
              PSM_PROTOCOL_ERROR:
              begin
                vErrorPacket:= TCircleQueueItem<TIOPacket>(vMsg.wParam);
                try
                  if assigned(FProtocolErrorEvent) then
                  begin
                    Synchronize(
                    procedure
                    begin
                      try
                        FProtocolErrorEvent(TProtocolIOResult(vMsg.lParam), Addr(vErrorPacket.Item));
                      except                        
                      end;
                    end);
                  end;
                finally
                  vErrorPacket.Release;
                end;
              end;
            end;
          end;
        end;
        WAIT_OBJECT_0+1:
          break;
      end;
    end;
  finally
    CloseHandle(fEvents.MessageEvent);
    CloseHandle(fEvents.StopEvent);
  end;
end;

function TMessageThread.PostProtocolError(AError:TProtocolIOResult; AErrorPacket: TCircleQueueItem<TIOPacket>):boolean;
begin
  try
    result := FPool.PostMessage(PSM_PROTOCOL_ERROR, Pointer(AErrorPacket), Pointer(AError), false);
    if result then
      SetEvent(FEvents.MessageEvent);
  except
    result := False;
  end;
end;

procedure TMessageThread.RemoveTag(const TagObj: TAbstractTag);
begin
  FPool.RemoveMessage(Pointer(TagObj), nil);
end;

procedure TMessageThread.ScanRead(const TagObj : TAbstractTag);
begin
  if FPool.PostMessage(PSM_TAGSCANREAD, Pointer(TagObj), nil, false) then
    SetEvent(FEvents.MessageEvent);
end;

procedure TMessageThread.ScanWrite(const TagObj : TAbstractTag);
begin
   if FPool.PostMessage(PSM_TAGSCANWRITE, Pointer(TagObj), nil, false) then
    SetEvent(FEvents.MessageEvent);
end;

end.
