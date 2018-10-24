unit ComConnector;

interface
  uses Windows, CPort, PacketParcer, Vodopad.Timer, SysUtils, Classes, ComConnectorInterface;

  type
  TComConnector = class abstract(TComponent, IComConnector)
   private
    FAsyncPtr: PAsync;
    fPort : byte;
    fComPort : TComport;
    fPacketParser : TPacketParcer;
    fChallengeTimer, fTimeOutTimer : TvdTimer;
    fDSR : TDSR;
    fFlowControl : boolean;
   protected
    function PortWrite(const Buffer; Count : LongWord) : integer; stdcall;
    function GetDSR: pDSR; stdcall;
    procedure onResultDataPacket(const Buffer; Count : Word); virtual;
    procedure DoChallenge(Sender: TObject); virtual;
    procedure OnTimeOut(Sender : TObject); virtual;
    procedure OnDSRChange(Sender: TObject; OnOff: Boolean); virtual;
    function GetSyncMethod: TPortSyncMethod; stdcall;
    procedure SetSyncMethod(const Value: TPortSyncMethod);stdcall;
   public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property ComPort : TComport read fComPort;
    property SyncMethod: TPortSyncMethod read GetSyncMethod write SetSyncMethod;
    function FlowControl: boolean;stdcall;
    function Port: Byte; stdcall;
    property DSR : pDSR read GetDSR;
    procedure ConfigPort(APort : byte; ABaud : LongWord;
              AdtrEnable, ArtsEnable, AFlowControl : boolean); stdcall;
    procedure ConfigParser(const StartMarker; StartSize : byte;
                          const StopMarker; StopSize : byte;
                          AIncludeMarkers : boolean = false); stdcall;
    procedure SetBufferSize(AInput, AOutput : LongWord); stdcall;
    function PortOpenOk : boolean; stdcall;
    procedure PortClose; virtual; stdcall;
    function IsConnected : boolean; stdcall;
    function CallChallenge(const After : LongWord) : boolean; virtual; stdcall;
    procedure SetTimeOutMode(const ATimeOut : LongWord); stdcall;
    procedure StopTimeOut; stdcall;
    procedure StopTimers; stdcall;
  end;
  
procedure EnumComPorts(Ports: TStringList);
function ComNameToByte(const ComName :String):byte;
function StringListComparePorts(List: TStringList; Index1, Index2: Integer): Integer;
  
implementation

function ComNameToByte(const ComName :String):byte;
var
tmpName : string;
R : integer;
begin
  result := 0;
  R := 0;
  tmpName := trim(ComName);
  if (pos('COM', tmpName) = 1) and (Length(tmpName) > 3)then
  begin
    Delete(tmpName, 1, 3);
    if (Length(tmpName) > 0) and (Length(tmpName) < 3) then
    try
      R := StrToInt(tmpName);
    except
    end;
  end;
  if (R > 0) and (R < 255) then
     result := byte(R);
end;

function StringListComparePorts(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result :=
  ComNameToByte(List.Strings[index1])-ComNameToByte(List.Strings[Index2]);
end;

procedure EnumComPorts(Ports: TStringList);
begin
  CPort.EnumComPorts(Ports);
  Ports.CustomSort(StringListComparePorts);
end;

{ TComConnector }


constructor TComConnector.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  VCLComObject := nil;
  fChallengeTimer := TvdTimer.Create(self);
  fChallengeTimer.OnTimer := DoChallenge;
  fChallengeTimer.Enabled := false;

  fTimeOutTimer := TvdTimer.Create(self);
  fTimeOutTimer.OnTimer := OnTimeOut;
  fTimeOutTimer.Enabled := false;

  fComPort := TComport.Create(self);
  fDSR.State := true;
  fFlowControl := false;
  SyncMethod := ComConnectorInterface.smThreadSync;
  fComPort.BaudRate := br38400;

  fPacketParser := TPacketParcer.Create(ComPort);
  fPacketParser.OnPacket := onResultDataPacket;
  FComPort.OnDSRChange := OnDSRChange;
  New(FAsyncPtr);
  FillChar(FAsyncPtr^.Overlapped, SizeOf(TOverlapped), 0);
  FAsyncPtr^.Overlapped.hEvent := CreateEvent(nil, True, True, nil);
end;

destructor TComConnector.Destroy;
begin
  StopTimers;
  CloseHandle(FAsyncPtr^.Overlapped.hEvent);
  Dispose(FAsyncPtr);
  FAsyncPtr := nil;
  FreeAndNil(fPacketParser);
  inherited Destroy;
end;

procedure TComConnector.DoChallenge(Sender: TObject);
begin
end;

function TComConnector.FlowControl: Boolean;
begin
  result := fFlowControl;
end;

function TComConnector.GetDSR: pDSR;
begin
  Result := @fDSR;
end;

function TComConnector.GetSyncMethod: TPortSyncMethod;
begin
  Result := TPortSyncMethod(Self.fComPort.SyncMethod);
end;

function TComConnector.Port: Byte;
begin
  result := fPort;
end;

function TComConnector.CallChallenge(const After : LongWord) : boolean;
begin
  result := false;
  if fTimeOutTimer.Enabled or fChallengeTimer.Enabled then
    exit;
  fChallengeTimer.Enabled := false;
  fChallengeTimer.Interval := After;
  fChallengeTimer.Enabled := IsConnected;
  result := fChallengeTimer.Enabled;
end;

procedure TComConnector.ConfigParser(const StartMarker; StartSize : Byte;
                                    const StopMarker; StopSize : Byte;
                                    AIncludeMarkers : Boolean = false);
begin
  fPacketParser.SetStartMarker(StartMarker, StartSize);
  fPacketParser.SetStopMarker(StopMarker, StopSize);
  fPacketParser.IncludeMarkers := AIncludeMarkers;
end;

procedure TComConnector.ConfigPort(APort: byte; ABaud: LongWord;
AdtrEnable, ArtsEnable, AFlowControl : Boolean);
var vTestHandle : THandle;
begin
  if IsConnected then
    exit;
  fPort := APort;
  try
    vTestHandle := CreateFile(PWideChar(Format('\\.\COM%d',[fPort])),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
    if vTestHandle = INVALID_HANDLE_VALUE then
      Exit;
  finally
    CloseHandle(vTestHandle);
  end;
  FComPort.Port := Format('COM%u', [fPort]);
  case ABaud of
    110: fComPort.BaudRate := br110;
    300: fComPort.BaudRate := br300;
    600: fComPort.BaudRate := br600;
    1200: fComPort.BaudRate := br1200;
    2400: fComPort.BaudRate := br2400;
    4800: fComPort.BaudRate := br4800;
    9600: fComPort.BaudRate := br9600;
    14400: fComPort.BaudRate := br14400;
    19200: fComPort.BaudRate := br19200;
    34800: fComPort.BaudRate := br38400;
    56000: fComPort.BaudRate := br56000;
    57600: fComPort.BaudRate := br57600;
    115200: fComPort.BaudRate := br115200;
    128000: fComPort.BaudRate := br128000;
    256000: fComPort.BaudRate := br256000;
    else
    begin
      fComPort.BaudRate := brCustom;
      fComPort.CustomBaudRate := ABaud;
    end;
  end;
  if AdtrEnable then
    FComport.FlowControl.ControlDTR := dtrEnable
  else
    FComport.FlowControl.ControlDTR := dtrDisable;
  if ArtsEnable then
    fComPort.FlowControl.ControlRTS := rtsEnable
  else
    fComPort.FlowControl.ControlRTS := rtsDisable;
  fFlowControl := AFlowControl;
  if fFlowControl then
    fDSR.State := True;
end;

function TComConnector.IsConnected: Boolean;
begin
  result := FComPort.Connected;
end;

procedure TComConnector.OnDSRChange(Sender: TObject; OnOff: Boolean);
begin
  fDSR.State := OnOff;
  if fFlowControl and not fDSR.State then
    fDSR.OffTime := now;
end;

procedure TComConnector.onResultDataPacket(const Buffer; Count: Word);
begin
end;

procedure TComConnector.OnTimeOut(Sender: TObject);
begin
end;

procedure TComConnector.StopTimeOut;
begin
  fTimeOutTimer.Enabled := false;
end;

procedure TComConnector.StopTimers;
begin
  fChallengeTimer.Enabled := false;
  fTimeOutTimer.Enabled := false;
end;

procedure TComConnector.PortClose;
begin 
  StopTimers;
  FComPort.Close;
end;

function TComConnector.PortOpenOk: Boolean;
var
vTestHandle : THandle;
begin
  try
    try
      vTestHandle := CreateFile(PWideChar('\\.\' + FComPort.Port),
        GENERIC_READ or GENERIC_WRITE,
        0,
        nil,
        OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
      result := vTestHandle <> INVALID_HANDLE_VALUE;
      if not Result then
        Exit;
    finally
      CloseHandle(vTestHandle);
    end;
    FComPort.Open;
    result := true;
  except
    result := false;
  end;
end;

function TComConnector.PortWrite(const Buffer; Count: LongWord) : integer;
begin
  try
    FAsyncPtr^.Data := nil;
    FAsyncPtr^.Size := 0;
    FComPort.WriteAsync(Buffer, Integer(Count), FAsyncPtr);
    Result := FComPort.WaitForAsync(FAsyncPtr);
  finally
    if FAsyncPtr^.Data <> nil then
      FreeMem(FAsyncPtr^.Data);
    FAsyncPtr^.Data := nil;
    FAsyncPtr^.Size := 0;
  end;
end;

procedure TComConnector.SetBufferSize(AInput, AOutput: LongWord);
begin
  fComPort.Buffer.InputSize := AInput;
  fComPort.Buffer.OutputSize := AOutput;
end;

procedure TComConnector.SetSyncMethod(const Value: TPortSyncMethod);
begin
  Self.fComPort.SyncMethod := TSyncMethod(Value);
end;

procedure TComConnector.SetTimeOutMode(const ATimeOut: LongWord);
begin
  fPacketParser.ResetBuffer;
  fTimeOutTimer.Enabled := false;
  fTimeOutTimer.Interval := ATimeOut;
  fTimeOutTimer.Enabled := true;
  fChallengeTimer.Enabled := false;
end;


end.
