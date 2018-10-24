unit TagsScanerThread;


interface

uses
  Classes, SysUtils, AbstractTag, DriverMessageThread, MessageSpool, syncobjs,
  ProtocolTypes, CircleQueue, Windows;

type
  TScanThreadEvents = record
    MessageEvent,
    StopEvent : THandle;
  end;
  TScanThread = class(TThread)
  private
    FScanEnabled : Boolean;
    FEvents : TSynMessageThreadEvents;
    FDoScanRead : TScanReadProc;
    FDoSafeWrite,
    fSafeRead : TSafeIOFunct;
    FPool : TMessageSpool;
    FMessageThread:TMessageThread;
    procedure CheckPool;
  protected
    procedure Execute; override;
  public
    constructor Create(AScanRead:TScanReadProc;
                      ASafeWrite,
                      ASafeRead:TSafeIOFunct;
                      const AMessageThread:TMessageThread;
                      AOnTerminateNotify : TNotifyEvent);
    destructor Destroy; override;
    procedure Stop;
    procedure AsynWrite(const TagObj:TAbstractTag);
    procedure AsynRead(const TagObj:TAbstractTag);
    procedure RemoveTag(const TagObj:TAbstractTag);
    procedure EnableScan;
    procedure DisableScan;
    property ScanEnabled : Boolean read FScanEnabled;
  end;

implementation





constructor TScanThread.Create(AScanRead:TScanReadProc;
                      ASafeWrite,
                      ASafeRead : TSafeIOFunct;
                      const AMessageThread:TMessageThread;
                      AOnTerminateNotify : TNotifyEvent);
begin
  FScanEnabled := False;
  FPool := TMessageSpool.Create;
  FDoScanRead := AScanRead;
  fSafeRead := ASafeRead;
  FDoSafeWrite := ASafeWrite;
  FMessageThread := AMessageThread;
  OnTerminate:= AOnTerminateNotify;
  //Priority := tpTimeCritical;
  FreeOnTerminate := True;
  inherited Create(false);
end;

destructor TScanThread.Destroy;
begin
  FreeAndNil(FPool);
  inherited Destroy;
end;

procedure TScanThread.DisableScan;
begin
  FScanEnabled := False;
end;

procedure TScanThread.Stop;
begin
  OnTerminate := nil;
  FreeOnTerminate := false;
  Terminate;
  SetEvent(FEvents.StopEvent);
  WaitFor;
end;

procedure TScanThread.EnableScan;
begin
  FScanEnabled := true;
  SetEvent(FEvents.MessageEvent);
end;

procedure TScanThread.Execute;
var
vWaitTimeOut : Cardinal;
vScanEnabled : Boolean;
begin
  {$IFDEF DEBUG}
    TThread.NameThreadForDebugging('Modbus driver scanner thread');
  {$ENDIF}
  fEvents.MessageEvent := CreateEvent(nil, False, False, nil);
  fEvents.StopEvent := CreateEvent(nil, False, False, nil);
  try
    while not Terminated do
    begin
      vScanEnabled := FScanEnabled and assigned(FDoScanRead);
      if vScanEnabled then
        vWaitTimeOut := 200
      else
        vWaitTimeOut := INFINITE;
      case WaitForMultipleObjects(2, @fEvents, False, vWaitTimeOut) of
        WAIT_OBJECT_0:
        begin
          CheckPool;
        end;
        WAIT_OBJECT_0+1:
          break;
        WAIT_TIMEOUT:
        begin
          if Terminated then
            break;
          vScanEnabled := FScanEnabled and assigned(FDoScanRead);
          if vScanEnabled then
            FDoScanRead(vWaitTimeOut)
          else
            vWaitTimeOut := INFINITE;
          CheckPool;
        end;
      end;
    end;
  finally
    CloseHandle(fEvents.MessageEvent);
    CloseHandle(fEvents.StopEvent);
  end;
end;

procedure TScanThread.RemoveTag(const TagObj: TAbstractTag);
begin
  FPool.RemoveMessage(Pointer(TagObj), nil);
end;

procedure TScanThread.CheckPool;
var
vMsg : TMSMsg;
vTagObj : TAbstractTag;
begin
  while (not Terminated) and
        FPool.PeekMessage(vMsg, PSM_TAGSCANREAD, PSM_TAGSCANWRITE) do
  begin
    case vMsg.MsgID of
      PSM_TAGSCANWRITE:
      begin
        vTagObj := TAbstractTag(vMsg.wParam);
        if assigned(FDoSafeWrite) then
        begin
          try
            try
              FDoSafeWrite(vTagObj);
            except
            end;
          finally
            if Assigned(FMessageThread) then
              FMessageThread.ScanWrite(vTagObj);
          end;
        end;
      end;
      PSM_TAGSCANREAD:
      begin
        vTagObj := TAbstractTag(vMsg.wParam);
        if assigned(fSafeRead) then
        begin
          try
            try
              fSafeRead(vTagObj);
            except
            end;
          finally
            if Assigned(FMessageThread) then
              FMessageThread.ScanRead(vTagObj);
          end;
        end;
      end;
    end;
  end;
end;

procedure TScanThread.AsynRead(const TagObj: TAbstractTag);
begin
  if FPool.PostMessage(PSM_TAGSCANREAD, Pointer(TagObj), nil, false) then
    SetEvent(FEvents.MessageEvent);
end;

procedure TScanThread.AsynWrite(const TagObj:TAbstractTag);
begin
  {Write message have high priority and put on top of pool}
  if FPool.PostMessage(PSM_TAGSCANWRITE, Pointer(TagObj), nil, true) then
    SetEvent(FEvents.MessageEvent);
end;

end.
