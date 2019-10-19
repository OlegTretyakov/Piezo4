unit CommPort;

interface

uses
  System.Classes,
  CommTypes,
  CircleQueue,
  MessagesPool,
  WinAPI.Windows;

type
  TErrorEventRec = record
    Event : TCommPortErrorEvent;
    Port : TObject;
    Error:TIOResult;
  end;

  TNotifyEventRec = record
    Port : TObject;
    Event : TNotifyEvent;
  end;

  TEventNotificationEvents = record
    MessageEvent,
    StopEvent : THandle;
  end;

  TEventNotificationThread = class(TThread)
  private
    FEvents : TEventNotificationEvents;
    FEventQueue : TCircleQueue<TNotifyEventRec>;
    FErrorQueue : TCircleQueue<TErrorEventRec>;
    FPool : TMessageSpool;
  protected
    procedure Execute; override;
  public
    constructor Create(AOnTerminateNotify : TNotifyEvent);
    destructor Destroy; override;
    procedure Stop;
    //: Sends a communication error message to application;
    function PostCommErrorEvent(const APort : TObject; const Event:TCommPortErrorEvent; Error:TIOResult) : boolean;
    //: Sends a port event message (port open, closed or diconnected) to application;
    function PostCommPortEvent(const APort : TObject; const AEvent : TNotifyEvent):Boolean;
  end;



  TCommPortDriverLogEvent = procedure (const Action : TIOAction; APacket : pIOPacket) of object;

  TCommPortDriver = class(TComponent)
  private
    FDestroying : Boolean;
    FLogEvent : TCommPortDriverLogEvent;
    FNotificationThread : TEventNotificationThread;
    FLastOSErrorNumber : Integer;
    FLastOSErrorMessage : String;
    FOnCommErrorReading : TCommPortErrorEvent;
    FOnCommErrorWriting : TCommPortErrorEvent;
    FOnCommPortOpened,
    FOnCommPortOpenError : TNotifyEvent;
    FOnCommPortClosed,
    FOnCommPortCloseError : TNotifyEvent;
    FOnCommPortDisconnected : TNotifyEvent;
    procedure CreateThreads;
    procedure ThreadTerminated(Sender : TObject);
    procedure SetActive(Value : Boolean);
    function InternalIOCommand(ACmd : TIOCommand; APacket : pIOPacket):Boolean;
    //: Opens the communication port (thread-safe).
    procedure InternalPortStart(var Ok : Boolean);
    //: Closes the communication port (thread-safe).
    procedure InternalPortStop(var Ok : Boolean);
  protected
    FExclusiveDevice:Boolean;
    procedure CommError(AWriteCmd:Boolean; AError : TIOResult);
    procedure CommPortOpened;
    procedure CommPortOpenError;
    procedure CommPortClose;
    procedure CommPortCloseError;
    procedure DoReadError(const APort : TObject; AError : TIOResult); virtual;
    procedure DoWriteError(const APort : TObject; AError : TIOResult); virtual;
    procedure DoPortOpened(ASender : TObject); virtual;
    procedure DoPortOpenError(ASender : TObject); virtual;
    procedure DoPortClose(ASender : TObject); virtual;
    procedure DoPortCloseError(ASender : TObject); virtual;
    procedure DoPortDisconnected(ASender : TObject); virtual;
  protected
    FActive : Boolean;
    FClearBufOnErr : Boolean;
    FProtocols : array of TComponent;
    FEventInterfaces:IPortDriverEventNotificationArray;
    procedure Read(APacket : pIOPacket); virtual; abstract;
    procedure Write(APacket : pIOPacket); virtual; abstract;
    procedure NeedSleepBetweenRW(ADelay : Cardinal=0);
    procedure PortStart(var Ok : Boolean); virtual; abstract;
    procedure PortStop(var Ok : Boolean); virtual; abstract;
    function  ComSettingsOK : Boolean; virtual;
    procedure ClearALLBuffers; virtual; abstract;
    procedure DoExceptionInActive;
    procedure RefreshLastOSError;
    property OnCommErrorReading:TCommPortErrorEvent read FOnCommErrorReading write FOnCommErrorReading;
    property OnCommErrorWriting:TCommPortErrorEvent read FOnCommErrorWriting write FOnCommErrorWriting;
    property OnCommPortOpened:TNotifyEvent read FOnCommPortOpened write FOnCommPortOpened;
    property OnCommPortOpenError:TNotifyEvent read FOnCommPortOpenError write FOnCommPortOpenError;
    property OnCommPortClosed:TNotifyEvent read FOnCommPortClosed write FOnCommPortClosed;
    property OnCommPortCloseError:TNotifyEvent read FOnCommPortCloseError write FOnCommPortCloseError;
    property OnCommPortDisconnected:TNotifyEvent read FOnCommPortDisconnected write FOnCommPortDisconnected;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AddProtocol(AProt : TComponent);
    procedure DelProtocol(AProt : TComponent);

    function IOCommandSync(ACmd : TIOCommand; APacket : pIOPacket):boolean;
    {:
    Return true if the communication port is open really.
    }
    function ReallyActive: Boolean;
    //: Opens (@true) or close (@false) the communication port.
    property Active: Boolean read FActive write SetActive;
    //:If @true, clears the input/output buffers of communication port if an I/O error has been found.
    property ClearBuffersOnCommErrors: Boolean read FClearBufOnErr write FClearBufOnErr default true;
    //: The last error code registered by the OS.
    property LastOSErrorNumber: Integer read FLastOSErrorNumber;
    //: The last error message registered by the OS.
    property LastOSErrorMessage: String read FLastOSErrorMessage;
    property LogEvent : TCommPortDriverLogEvent read FLogEvent write FLogEvent;
  end;


implementation

uses
  System.SysUtils,
  ProtocolDriver,
  hsstrings,
  System.Math;


constructor TEventNotificationThread.Create(AOnTerminateNotify : TNotifyEvent);
begin
  OnTerminate := AOnTerminateNotify;
  FPool := TMessagesPool.Create;
  FEventQueue := TCircleQueue<TNotifyEventRec>.Create;
  FErrorQueue := TCircleQueue<TErrorEventRec>.Create;
  FreeOnTerminate := True;
  inherited Create(false);
end;

destructor TEventNotificationThread.Destroy;
begin
  FreeAndNil(FPool);
  FreeAndNil(FEventQueue);
  FreeAndNil(FErrorQueue);
  inherited Destroy;
end;

procedure TEventNotificationThread.Stop;
begin
  OnTerminate := nil;
  FreeOnTerminate := false;
  Terminate;
  SetEvent(FEvents.StopEvent);
  WaitFor;
end;

procedure TEventNotificationThread.Execute;
var
  vMessage : TMSMsg;
  vEventItem : TCircleQueueItem<TNotifyEventRec>;
  vErrorItem : TCircleQueueItem<TErrorEventRec>;
begin
  {$IFDEF DEBUG}
    TThread.NameThreadForDebugging('Modbus serial port notification thread');
  {$ENDIF}
  fEvents.MessageEvent := CreateEvent(nil, False, False, nil);
  fEvents.StopEvent := CreateEvent(nil, False, False, nil);
  try
    while not Terminated do
    begin
      try
        case WaitForMultipleObjects(2, @fEvents, False, INFINITE) of
          WAIT_OBJECT_0:
          begin
            while (not Terminated) and
                  FPool.PeekMessage(vMessage, PSM_COMMERROR, PSM_PORT_EVENT) do
            begin
              case vMessage.MsgID of
                PSM_COMMERROR:
                begin
                  vErrorItem := TCircleQueueItem<TErrorEventRec>(vMessage.wParam);
                  try
                    Synchronize(
                    procedure
                    begin
                      try
                        vErrorItem.Item.Event(vErrorItem.Item.Port, vErrorItem.Item.Error);
                      except
                      end;
                    end
                    );
                  finally
                    vErrorItem.Release;
                  end;
                end;
                PSM_PORT_EVENT:
                begin
                  vEventItem := TCircleQueueItem<TNotifyEventRec>(vMessage.wParam);
                  try
                    Synchronize(
                    procedure
                    begin
                      try
                        vEventItem.Item.Event(vEventItem.Item.Port);
                      except
                      end;
                    end
                    );
                  finally
                    vEventItem.Release;
                  end;
                end;
              end;
            end;
          end;
          WAIT_OBJECT_0+1:
          begin
            break;
          end;
        end;
      except
        break;
      end;
    end;
  finally
    CloseHandle(fEvents.MessageEvent);
    CloseHandle(fEvents.StopEvent);
  end;
end;

function TEventNotificationThread.PostCommErrorEvent(const APort : TObject; const Event:TCommPortErrorEvent; Error:TIOResult):boolean;
var
  vErrorItem : TCircleQueueItem<TErrorEventRec>;
begin
  try
    vErrorItem := FErrorQueue.Acquire;
    vErrorItem.Item.Port := APort;
    vErrorItem.Item.Event := Event;
    vErrorItem.Item.Error := Error;
    try
      if FPool.PostMessage(PSM_COMMERROR, Pointer(vErrorItem), nil, false) then
        SetEvent(FEvents.MessageEvent);
      result := true;
    except
      vErrorItem.Release;
      raise;
    end;
  except
    result := false;
  end;
end;

function TEventNotificationThread.PostCommPortEvent(const APort : TObject; const AEvent:TNotifyEvent):Boolean;
var
  vEventItem : TCircleQueueItem<TNotifyEventRec>;
begin
  try
    vEventItem := FEventQueue.Acquire;
    vEventItem.Item.Port := APort;
    vEventItem.Item.Event := AEvent;
    try
      if FPool.PostMessage(PSM_PORT_EVENT, Pointer(vEventItem), nil, false) then
        SetEvent(FEvents.MessageEvent);
      result := true;
    except
      vEventItem.Release;
      raise;
    end;
  except
    result := false;
  end;
end;


constructor TCommPortDriver.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FActive := false;
  FExclusiveDevice := false;
  FLastOSErrorMessage := '';
  FLastOSErrorNumber := 0;
  FClearBufOnErr := true;
  FLogEvent := nil;
  FDestroying := false;
  FNotificationThread := nil;
end;

procedure TCommPortDriver.CreateThreads;
begin
  if FDestroying then
    Exit;
  if not Assigned(FNotificationThread) then
    FNotificationThread := TEventNotificationThread.Create(ThreadTerminated);
end;

destructor TCommPortDriver.Destroy;
var
  i : integer;
begin
  FDestroying := true;
  for i := 0 to High(FProtocols) do
    TProtocolDriver(FProtocols[i]).CommunicationPort := nil;
  for i := 0 to High(FEventInterfaces) do
    FEventInterfaces[i].DoPortRemoved(self);
  Active := false;
  if assigned(FNotificationThread) then
  begin
    FNotificationThread.Stop;
    FreeAndNil(FNotificationThread);
  end;
  SetLength(FProtocols, 0);
  inherited Destroy;
end;

procedure TCommPortDriver.AddProtocol(AProt : TComponent);
var
  i : integer;
  found, interfaced : Boolean;
  vTmp : IPortDriverEventNotification;
begin
  interfaced := Supports(AProt, IPortDriverEventNotification, vTmp);
  if not interfaced then
    if not (AProt is TProtocolDriver) then
      raise Exception.Create(SCompIsntADriver);

  found := false;
  if interfaced then
  begin
    for i := 0 to High(FEventInterfaces) do
    begin
      if (FEventInterfaces[i]= vTmp) then
      begin
        found := true;
        break;
      end;
    end;
  end else
  begin
    for i := 0 to High(FProtocols) do
    begin
      if (FProtocols[i] = AProt) then
      begin
        found := true;
        break;
      end;
    end;
  end;

  if not found then
  begin
    if interfaced then
    begin
      i := length(FEventInterfaces);
      SetLength(FEventInterfaces, i+1);
      FEventInterfaces[i] := vTmp;
    end else
    begin
      i := length(FProtocols);
      SetLength(FProtocols, i+1);
      FProtocols[i] := AProt;
    end;
  end;
  vTmp := nil;
  CreateThreads;
end;

procedure TCommPortDriver.DelProtocol(AProt : TComponent);
var
  vFound, vInterfaced : Boolean;
  i : Integer;
  vTmp : IPortDriverEventNotification;
begin
  vInterfaced := Supports(AProt, IPortDriverEventNotification, vTmp);
  vFound := false;
  if vInterfaced then
  begin
    for i := 0 to High(FEventInterfaces) do
    begin
      if (FEventInterfaces[i] = vTmp) then
      begin
        vFound := true;
        break;
      end;
    end;
  end else
  begin
    for i := 0 to High(FProtocols) do
    begin
      if (FProtocols[i] = AProt) then
      begin
        vFound := true;
        break;
      end;
    end;
  end;

  if vFound then
  begin
    if vInterfaced then
    begin
      FEventInterfaces[i] := FEventInterfaces[High(FEventInterfaces)];
      FEventInterfaces[High(FEventInterfaces)] := nil;
      SetLength(FEventInterfaces, High(FEventInterfaces));
    end else
    begin
      FProtocols[i] := FProtocols[High(FProtocols)];
      SetLength(FProtocols, High(FProtocols));
    end;
  end;
  vTmp := nil;
  if (Length(FEventInterfaces) < 1) and assigned(FNotificationThread) then
  begin
    FNotificationThread.Stop;
    FreeAndNil(FNotificationThread);
  end;
end;

function  TCommPortDriver.ComSettingsOK: Boolean;
begin
  Result := false;
end;

procedure TCommPortDriver.CommError(AWriteCmd : Boolean; AError : TIOResult);
var
  vEvent : TCommPortErrorEvent;
  vPostFail : boolean;
begin
  if FDestroying then
    exit;
  vPostFail := false;
  if GetCurrentThreadId = MainThreadID then
  begin
    try
      if AWriteCmd then
        DoWriteError(self, AError)
      else
        DoReadError(self, AError);
    except
    end;
  end else if assigned(FNotificationThread) then
  begin
    if AWriteCmd then
      vEvent := DoWriteError
    else
      vEvent := DoReadError;
    vPostFail := not FNotificationThread.PostCommErrorEvent(self, vEvent, AError);
  end;
  if vPostFail then
  begin
    TThread.Synchronize(nil,
    procedure
    begin
      try
        if AWriteCmd then
          DoWriteError(self, AError)
        else
          DoReadError(self, AError);
      except
      end;
    end
    );
  end;
end;

procedure TCommPortDriver.CommPortOpened;
var
  i : Integer;
begin
  if FDestroying then
    exit;

  if GetCurrentThreadId = MainThreadID then
  begin
    try
      DoPortOpened(Self);
    except
    end;
    for i := 0 to High(FEventInterfaces) do
    begin
      if ntePortOpen in FEventInterfaces[i].NotifyThisEvents then
        FEventInterfaces[i].DoPortOpened(Self);
    end;
  end else
  if assigned(FNotificationThread) and FNotificationThread.PostCommPortEvent(self, DoPortOpened) then
  begin
    for i := 0 to High(FEventInterfaces) do
    begin
      if (ntePortOpen in FEventInterfaces[i].NotifyThisEvents) then
        FNotificationThread.PostCommPortEvent(self, FEventInterfaces[i].GetPortOpenedEvent);
    end;
  end else
  begin
    TThread.Synchronize(nil,
    procedure
    var
      vIdx : integer;
    begin
      try
        DoPortOpened(Self);
      except
      end;
      for vIdx := 0 to High(FEventInterfaces) do
      begin
        if ntePortOpen in FEventInterfaces[vIdx].NotifyThisEvents then
          FEventInterfaces[vIdx].DoPortOpened(Self);
      end;
    end
    );
  end;
end;

procedure TCommPortDriver.CommPortOpenError;
begin
  if FDestroying then
    exit;
  if GetCurrentThreadId = MainThreadID then
  begin
    try
      DoPortOpenError(Self);
    except
    end
  end else if (not assigned(FNotificationThread)) or (not FNotificationThread.PostCommPortEvent(self, DoPortOpenError)) then
  begin
    TThread.Synchronize(nil,
    procedure
    begin
      try
        DoPortOpenError(Self);
      except
      end
    end
    );
  end;
end;

procedure TCommPortDriver.CommPortClose;
var
  i : Integer;
begin
  if FDestroying then
    exit;

  if GetCurrentThreadId = MainThreadID then
  begin
    try
      DoPortClose(Self);
    except
    end;
    for i := 0 to High(FEventInterfaces) do
    begin
      if ntePortClosed in FEventInterfaces[i].NotifyThisEvents then
        FEventInterfaces[i].DoPortClosed(Self);
    end;
  end else
  if assigned(FNotificationThread) and FNotificationThread.PostCommPortEvent(self, DoPortClose) then
  begin
    for i := 0 to High(FEventInterfaces) do
    begin
      if ntePortClosed in FEventInterfaces[i].NotifyThisEvents then
        FNotificationThread.PostCommPortEvent(self, FEventInterfaces[i].GetPortClosedEvent);
    end;
  end else
  begin
    TThread.Synchronize(nil,
    procedure
    var
      vIdx : integer;
    begin
      try
        DoPortClose(Self);
      except
      end;
      for vIdx := 0 to High(FEventInterfaces) do
      begin
        if ntePortClosed in FEventInterfaces[vIdx].NotifyThisEvents then
          FEventInterfaces[vIdx].DoPortClosed(Self);
      end;
    end
    );
  end;
end;

procedure TCommPortDriver.CommPortCloseError;
begin
  if FDestroying then
    exit;

  if GetCurrentThreadId = MainThreadID then
  begin
    try
      DoPortCloseError(Self);
    except
    end
  end else
  if (not assigned(FNotificationThread)) or (not FNotificationThread.PostCommPortEvent(self, DoPortCloseError)) then
  begin
    TThread.Synchronize(nil,
    procedure
    begin
      try
        DoPortCloseError(Self);
      except
      end
    end
    );
  end;
end;

procedure TCommPortDriver.DoReadError(const APort : TObject; AError : TIOResult);
begin
  if Assigned(FOnCommErrorReading) then
    FOnCommErrorReading(APort, AError);
end;

procedure TCommPortDriver.DoWriteError(const APort : TObject; AError : TIOResult);
begin
  if Assigned(FOnCommErrorWriting) then
    FOnCommErrorWriting(APort, AError);
end;

procedure TCommPortDriver.DoPortOpened(ASender : TObject);
begin
  if Assigned(FOnCommPortOpened) then
    FOnCommPortOpened(ASender);
end;

procedure TCommPortDriver.DoPortOpenError(ASender : TObject);
begin
  if Assigned(FOnCommPortOpenError) then
    FOnCommPortOpenError(ASender);
end;

procedure TCommPortDriver.DoPortClose(ASender : TObject);
begin
  if Assigned(FOnCommPortClosed) then
    FOnCommPortClosed(ASender);
end;

procedure TCommPortDriver.DoPortCloseError(ASender : TObject);
begin
  if Assigned(FOnCommPortCloseError) then
    FOnCommPortCloseError(ASender);
end;

procedure TCommPortDriver.DoPortDisconnected(ASender : TObject);
begin
  if Assigned(FOnCommPortDisconnected) then
    FOnCommPortDisconnected(ASender);
end;

function TCommPortDriver.ReallyActive: Boolean;
begin
  if (csDesigning in ComponentState) then
  begin
    if FExclusiveDevice then
      Result := false
    else
      Result := FActive and (not FDestroying);
  end else
    Result := FActive and (not FDestroying);
end;

procedure TCommPortDriver.SetActive(Value:  Boolean);
var
  vOk : boolean;
begin
  if Value = FActive then
    exit;

  vOk := Value;
  if Value and (not FActive) then
  begin
    InternalPortStart(vOk);
  end;
  
  if (not vOk) and FActive then
  begin
    InternalPortStop(vOk);
  end;
end;

procedure TCommPortDriver.ThreadTerminated(Sender : TObject);
begin
  FNotificationThread := nil;
  CreateThreads;
end;

function TCommPortDriver.IOCommandSync(ACmd : TIOCommand; APacket : pIOPacket):boolean;
begin
  Result := false;
  TMonitor.Enter(self);
  try
    if FActive then
    begin
      //prepare the command packet.
      APacket.WriteIOResult := iorNone;
      APacket.WrittenCount := 0;
      APacket.WriteRetries := 3;

      APacket.ReadIOResult := iorNone;
      APacket.ReceivedCount := 0;
      APacket.ReadRetries := 0;

      //executes the I/O command.
      Result := InternalIOCommand(ACmd, APacket);
    end else
    begin
      if ACmd in [iocRead, iocReadWrite, iocWriteRead] then
        APacket^.ReadIOResult := iorNotReady;
      if ACmd in [iocWrite, iocReadWrite, iocWriteRead] then
        APacket^.WriteIOResult := iorNotReady;
    end;
  finally
    TMonitor.Exit(self);
  end;
end;

function TCommPortDriver.InternalIOCommand(ACmd : TIOCommand; APacket : pIOPacket):Boolean;
begin
  Result := False;
  try
    //executes the I/O command.
    case ACmd of
      iocRead:
      begin
        Read(APacket);
        if Assigned(FLogEvent) then
          FLogEvent(ioaRead, APacket);
        Result := true;
      end;
      iocReadWrite:
      begin
        Read(APacket);
        if Assigned(FLogEvent) then
          FLogEvent(ioaRead, APacket);
        NeedSleepBetweenRW(APacket.DelayBetweenCommand);
        Write(APacket);
        if Assigned(FLogEvent) then
          FLogEvent(ioaWrite, APacket);
        Result := true;
      end;
      iocWrite:
      begin
        Write(APacket);
        if Assigned(FLogEvent) then
          FLogEvent(ioaWrite, APacket);
        Result := true;
      end;
      iocWriteRead:
      begin
        Write(APacket);
        if Assigned(FLogEvent) then
          FLogEvent(ioaWrite, APacket);
        NeedSleepBetweenRW(APacket.DelayBetweenCommand);
        Read(APacket);
        if Assigned(FLogEvent) then
          FLogEvent(ioaRead, APacket);
        Result := true;
      end;
    end;
  except
    Result := False;
    if ACmd in [iocRead, iocReadWrite, iocWriteRead] then
      APacket^.ReadIOResult := iorPortError;
    if ACmd in [iocWrite, iocReadWrite, iocWriteRead] then
      APacket^.WriteIOResult := iorPortError;
  end;
end;

procedure TCommPortDriver.InternalPortStart(var Ok : Boolean);
begin
  PortStart(ok);
  RefreshLastOSError;
  if Ok then
    CommPortOpened
  else
    CommPortOpenError;
end;

procedure TCommPortDriver.InternalPortStop(var Ok : Boolean);
begin
  PortStop(ok);
  RefreshLastOSError;
  if Ok then
    CommPortClose
  else
    CommPortCloseError;
end;

procedure TCommPortDriver.DoExceptionInActive;
begin
  if FActive then
  begin
    if (not (csDesigning in ComponentState))
    and (not FExclusiveDevice) then
      raise Exception.Create(SimpossibleToChangeWhenActive);
  end;
end;

procedure TCommPortDriver.RefreshLastOSError;
{$IFNDEF FPC}
{$IF defined(WIN32) or defined(WIN64)}
var
  vBuffer : PAnsiChar;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF FPC}
  FLastOSErrorNumber:=GetLastOSError;
  FLastOSErrorMessage:=SysErrorMessage(FLastOSErrorNumber);
{$ELSE}
{$IF defined(WIN32) or defined(WIN64)}
  FLastOSErrorNumber:=GetLastError;
  GetMem(vBuffer, 512);
  if FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, nil, FLastOSErrorNumber, LANG_NEUTRAL, vBuffer, 512, nil)<>0 then
  begin
    FLastOSErrorMessage:=vBuffer;
    FreeMem(vBuffer);
  end else
    FLastOSErrorMessage:=SFaultGettingLastOSError;
{$ENDIF}
{$ENDIF}
end;

procedure TCommPortDriver.NeedSleepBetweenRW(ADelay : Cardinal);
begin
  if (ADelay > 0) then
    Sleep(ADelay);
end;

end.
