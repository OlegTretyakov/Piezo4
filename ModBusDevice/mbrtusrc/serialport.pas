unit SerialPort;


interface

uses

  CommTypes, CommPort, SysUtils, Classes,
  {$IF defined(WIN32) or defined(WIN64) OR defined(WINCE)} Windows,{$ENDIF}
  {$IFDEF UNIX} Serial, Unix, BaseUnix, TermIO, {$ENDIF}
  DateUtils;


type


  {:
  @name enumerates all baud rates.
  This baud rates are supported on all OSes.

  @value br110    = 110 bps
  @value br300    = 300 bps
  @value br600    = 600 bps
  @value br1200   = 1200 bps
  @value br2400   = 2400 bps
  @value br4800   = 4800 bps
  @value br9600   = 9600 bps
  @value br19200  = 19200 bps
  @value br38400  = 38400 bps
  @value br57600  = 57600 bps
  @value br115200 = 115200 bps
  }
  TSerialBaundRate = (br110, br300, br600, br1200, br2400, br4800, br9600,
                      br19200, br38400, br57600, br115200);


  {:
  @name enumarates all stop bits.
  This values are supported on all OSes

  @value sb1 = 1 stop bit
  @value sb2 = 2 stop bit
  }
  TSerialStopBits = (sb1, sb2);


  {:
  @name enumerates all parity modes.
  This values are supported on all OSes.

  @value spNone Don't check the parity.
  @value spOdd  Check errors using the odd parity.
  @value spEven Check errors using the even parity.
  }
  TSerialParity = (spNone, spOdd, spEven);


  {:
  @name enumerates all data byte sizes.
  This values are supported on all OSes.

  @value db5 The data byte will have 5 bits of size.
  @value db6 The data byte will have 6 bits of size.
  @value db7 The data byte will have 7 bits of size.
  @value db8 The data byte will have 8 bits of size.
  }
  TSerialDataBits= (db5, db6, db7, db8);


  {:
  @abstract(Serial port driver. Working on  Windows, Linux and FreeBSD.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TCommPortDriver)
  }
  TSerialPortDriver = class(TCommPortDriver)
  private
    FPortName : String;
    FTimeout : Cardinal;
    FBaundRate : TSerialBaundRate;
    FStopBits : TSerialStopBits;
    FParity : TSerialParity;
    FDataBits : TSerialDataBits;
    {$IF defined(WIN32) or defined(WIN64) OR defined(WINCE)}
    FPortEventName : String;
    FSavedDCB : DCB;
    FDCB : DCB;
    FComTimeouts : COMMTIMEOUTS;
    FOverlapped : TOverlapped;
    FPortHandle : THandle;
    {$ELSE}
    LockOpen : Boolean;
    PPortHandle : TSerialHandle;
    PSavedState : TSerialState;
    {$IFEND}
    FBackupPortSettings : Boolean;
    procedure SetTimeOut(v : Cardinal);
    procedure SetBaundRate(v : TSerialBaundRate);
    procedure SetStopBits(v : TSerialStopBits);
    procedure SetParity(v : TSerialParity);
    procedure SetDataBits(v : TSerialDataBits);
    procedure SetCOMPort(const v : string);
    {$IF defined(WIN32) or defined(WIN64)}
    function MakeDCBString:String;
    {$ENDIF}
    function COMExist(const v : string):Boolean;
  protected
    procedure Read(APacket : pIOPacket); override;
    procedure Write(APacket : pIOPacket); override;
    {: @exclude }
    procedure PortStart(var Ok : Boolean); override;
    {: @exclude }
    procedure PortStop(var Ok : Boolean); override;
    {: @exclude }
    function  ComSettingsOK: Boolean; override;
    {: @exclude }
    procedure ClearALLBuffers; override;
  public
    {:
    Creates a new serial port driver with the following settings: baud rate 19200bps,
    8 data bits, 1 stop bits, without parity check and 100ms of timeout.
    @seealso(TCommPortDriver)
    }
    constructor Create(AOwner : TComponent); override;
    {:
    Serial port driver to be used. This names depends of operating system.
    On Windows the name is COMx, ob Linux is ttySx and on FreeBSD is cuadx.
    }
    property COMPort: string read FPortName write SetCOMPort;

    {: How many milliseconds a read or write operation can take. }
    property Timeout: Cardinal read FTimeout write SetTimeOut;
    {:
    Serial port baud rate.
    @seealso(TSerialBaundRate)
    }
    property BaudRate: TSerialBaundRate read FBaundRate write SetBaundRate stored true default br19200;
    {:
    Data byte size.
    @seealso(TSerialDataBits)
    }
    property DataBits: TSerialDataBits read FDataBits write SetDataBits stored true default db8;
    {:
    Parity check.
    @seealso(TSerialParity)
    }
    property Parity: TSerialParity read FParity write SetParity stored true default spNone;
    {:
    Stop bits.
    @seealso(TSerialStopBits)
    }
    property StopBits: TSerialStopBits read FStopBits write SetStopBits stored true default sb1;
    {:
    If @true the driver will do a of the older settings of the serial port before
    open it, and restore when it's closed.
    }
    property BackupPortSettings: Boolean read FBackupPortSettings write FBackupPortSettings stored true default false;

  end;


{$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
function CTL_CODE( DeviceType, Func, Method, Access:Cardinal):Cardinal;

const METHOD_BUFFERED         = 0;
const METHOD_IN_DIRECT        = 1;
const METHOD_OUT_DIRECT       = 2;
const METHOD_NEITHER          = 3;
const FILE_DEVICE_SERIAL_PORT = $0000001b;
const FILE_ANY_ACCESS         = 0;
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF LINUX}
var PortPrefix : array[0..1] of string = ('ttyS','ttyUSB');
{$ENDIF}
{$IFDEF FREEBSD}
var PortPrefix : array[0..0] of string = ('cuad');
{$ENDIF}
{$IFDEF NETBSD}
var PortPrefix : array[0..0] of string = ('cuad');
{$ENDIF}
{$IFDEF OPENBSD}
var PortPrefix : array[0..0] of string = ('cuad');
{$ENDIF}
{$ENDIF}


implementation

uses
System.Math,
hsstrings;

{$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
function CTL_CODE( DeviceType, Func, Method, Access:Cardinal):Cardinal;
begin
  result := (((DeviceType) shl 16) or ((Access) shl 14) or ((Func) shl 2) or (Method));
end;
{$ENDIF}


constructor TSerialPortDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FExclusiveDevice := true;
  FBaundRate := br19200;
  FDataBits  := db8;
  FStopBits  := sb1;
  FParity    := spNone;
  FTimeout   := 1000;
  {$IFDEF UNIX}
  LockOpen:=false;
  {$ENDIF}
end;

procedure TSerialPortDriver.Read(APacket : pIOPacket);
{$IF defined(WIN32) or defined(WIN64)}
var
  vReaded,
  vRetries : Cardinal;
begin
  APacket^.ReadStart := Now;
  vRetries := 0;
  APacket^.ReceivedCount := 0;
  APacket^.ToReadCount := Cardinal(Min(APacket^.ToReadCount, Length(APacket^.BufferToRead)));
  while (APacket^.ReceivedCount < APacket^.ToReadCount) do
  begin
    if (MilliSecondsBetween(Now, APacket^.ReadStart) >= FTimeout) then
      break;
    ResetEvent(FOverlapped.hEvent);
    FOverlapped.Offset := 0;
    FOverlapped.OffsetHigh := 0;

    ReadFile(FPortHandle, APacket^.BufferToRead[APacket^.ReceivedCount], APacket^.ToReadCount-APacket^.ReceivedCount, vReaded, @FOverlapped);

    WaitForSingleObject(FOverlapped.hEvent, FTimeout);
    GetOverlappedResult(FPortHandle, FOverlapped, vReaded, true);
    APacket^.ReceivedCount := APacket^.ReceivedCount + vReaded;
    Inc(vRetries);
    if (APacket^.ReceivedCount < APacket^.ToReadCount) then
      Sleep(20);
  end;
  APacket^.ReadFinish := Now;
{$ENDIF}

{$IFDEF UNIX}
var
  vReaded : Cardinal;
  vRetries : Word;
  start : TDateTime;
  Req, Rem : TimeSpec;
begin
  vRetries := 0;
  start := Now;

  Packet^.Received := 0;
  Packet^.ReadIOResult:=iorNone;
  while (Packet^.Received<Packet^.ToRead) and (vRetries<Packet^.ReadRetries) do
  begin
     vReaded := SerRead(PPortHandle,Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received);
     Packet^.Received := Packet^.Received + vReaded;
     if (MilliSecondsBetween(Now,start)>PTimeout) then
     begin
        inc(vRetries);
        start:=Now;
     end;
     //waits 0,1ms
     Req.tv_sec:=0;
     Req.tv_nsec:=100000;
     FpNanoSleep(@Req,@Rem);
  end;
{$ENDIF}

{$IF defined(WINCE)}
var
  vRetries:Word;
begin
{$IFEND}

  APacket^.ReadRetries := vRetries;
  if (APacket^.ReceivedCount < APacket^.ToReadCount) then
  begin
    APacket^.ReadIOResult := iorTimeOut;
    if FClearBufOnErr then
      ClearALLBuffers;
  end else
    APacket^.ReadIOResult := iorOK;

  if APacket^.ReadIOResult<>iorOK then
    CommError(false, APacket^.ReadIOResult);
end;

procedure TSerialPortDriver.Write(APacket : pIOPacket);
{$IF defined(WIN32) or defined(WIN64)}
var
  vWritten : Cardinal;
begin
  ResetEvent(FOverlapped.hEvent);
  FOverlapped.Offset := 0;
  FOverlapped.OffsetHigh := 0;
  APacket^.WriteRetries := 0;
  APacket^.WriteStart := now;
  if not WriteFile(FPortHandle, APacket^.BufferToWrite[0], APacket^.ToWriteCount, APacket^.WrittenCount, @FOverlapped) then
  begin
    if (WaitForSingleObject(FOverlapped.hEvent, FTimeout) = WAIT_OBJECT_0) then
    begin
      APacket^.WriteIOResult := iorOK;
      GetOverlappedResult(FPortHandle, FOverlapped, vWritten, true);
      APacket^.WrittenCount := vWritten;
    end else
    begin
      APacket^.WriteIOResult := iorTimeOut;
      if FClearBufOnErr then
         ClearALLBuffers;
    end;
  end else
  begin
    APacket^.WriteIOResult := iorPortError;
    if FClearBufOnErr then
       ClearALLBuffers;
  end;
  APacket^.WriteRetries := 1;
  APacket^.WriteFinish := now;
{$ENDIF}
{$IFDEF UNIX}
var
  vWritten,
  vRetries : Word;
begin
  vRetries := 0;

  Packet^.Written := 0;
  while (Packet^.Written<Packet^.ToWrite) and (vRetries<Packet^.WriteRetries) do
  begin
    vWritten := SerWrite (PPortHandle,Packet^.BufferToWrite[Packet^.Written], Packet^.ToWrite-Packet^.Written);
    Packet^.Written := Packet^.Written + vWritten;
    Inc(vRetries);
  end;

  Packet^.WriteRetries := vRetries;
  if Packet^.ToWrite>Packet^.Written then
  begin
    Packet^.WriteIOResult := iorTimeOut;
    if PClearBufOnErr then
       InternalClearALLBuffers;
  end else
    Packet^.WriteIOResult := iorOK;
{$ENDIF}
{$IF defined(WINCE)}
var
  vRetries : Integer;
begin
{$IFEND}

  if APacket^.WriteIOResult<>iorOK then
    CommError(true, APacket^.WriteIOResult);
end;

procedure TSerialPortDriver.PortStart(var Ok : Boolean);
{$IF defined(WIN32) or defined(WIN64)}
var
  strdcb : String;
label erro1, erro2, erro3;
begin
  if FActive then
  begin
    Ok := true;
    exit;
  end;
  FPortEventName := FPortName+'_TimeOutEvent';

  FOverlapped.Offset := 0;
  FOverlapped.OffsetHigh := 0;
  FOverlapped.Internal := 0;
  FOverlapped.InternalHigh := 0;
  FOverlapped.hEvent :=  CreateEvent(nil, TRUE, FALSE, PChar(FPortEventName)) ;

  FComTimeouts.ReadIntervalTimeout := 2;
  FComTimeouts.ReadTotalTimeoutMultiplier := 2;
  FComTimeouts.ReadTotalTimeoutConstant := FTimeout;
  FComTimeouts.WriteTotalTimeoutMultiplier := 2;
  FComTimeouts.WriteTotalTimeoutConstant := (FTimeout div 4);

  if not COMExist(FPortName) then
    goto erro1;

  FPortHandle := CreateFile(PWideChar('\\.\' + FPortName),
                  GENERIC_READ or GENERIC_WRITE,
                  0,
                  nil,
                  OPEN_EXISTING,
                  FILE_FLAG_WRITE_THROUGH or FILE_FLAG_OVERLAPPED,
                  0);
  if FPortHandle=INVALID_HANDLE_VALUE then
  begin
    RefreshLastOSError;
    goto erro1;
  end;

  //seta o tamanho dos buffer se leitura e escrita
  //sets the length of the buffers of read and write
  if not SetupComm(FPortHandle, 8192, 8192) then
  begin
    RefreshLastOSError;
    goto erro1;
  end;

  //monta string DCB
  //makes a DCB string
  strdcb := MakeDCBString;
  //zera a estrutura DCB (um bug conhecido, parametro incorreto!);
  //Fill with zeros the structure.
  FillMemory(@FDCB,sizeof(DCB), 0);
  FDCB.DCBlength := sizeof(DCB);
  if not BuildCommDCB(PChar(strdcb), FDCB) then
  begin
    RefreshLastOSError;
    goto erro2;
  end;

  //faz backup da DCB que estava setada na porta
  //backup the old settings.
  if FBackupPortSettings then
    GetCommState(FPortHandle, FSavedDCB);

  //seta a nova estrutura DCB na porta de comunicação
  //sets the new DCB struture.
  if not SetCommState(FPortHandle, FDCB) then
  begin
    RefreshLastOSError;
    goto erro3;
  end;

  //Seta os timeouts
  //sets the timeouts.
  if not SetCommTimeouts(FPortHandle, FComTimeouts) then
  begin
    RefreshLastOSError;
    goto erro3;
  end;

  ClearALLBuffers;

  ok := true;
  FActive := true;
  exit;

erro3:
  if FBackupPortSettings then
    SetCommState(FPortHandle, FSavedDCB);
erro2:
  CloseHandle(FPortHandle);
erro1:
  FPortEventName := '';
  CloseHandle(FOverlapped.hEvent);
  FOverlapped.hEvent := 0;
  ok := false;
  FActive := false;
{$ENDIF}
{$IF defined(WINCE)}
begin
  //ToDO
{$IFEND}
{$IFDEF UNIX}

var
   r : Integer;
   tios : termios;
begin
  //abre a porta
  //open the serial port
  PPortHandle := fpopen('/dev/'+PPortName, O_RDWR or O_NOCTTY or O_NONBLOCK);
  if PPortHandle<0 then
  begin
     RefreshLastOSError;
     Ok := false;
     PActive := false;
     exit;
  end;
  
  //se e para salvar as configs da porta...
  //backup the serial port settings.
  if PBackupPortSettings then
    PSavedState := SerSaveState(PPortHandle);

  r := 0;
  fillchar(tios, sizeof(tios), #0);

  tios.c_oflag := 0;
  tios.c_lflag := 0;

  //velocidade
  //sets the baudrate
  case PBaundRate of
     br110:
     begin
       tios.c_ispeed := B110;
       tios.c_ospeed := B110;
     end;
     br300:
     begin
       tios.c_ispeed := B300;
       tios.c_ospeed := B300;
     end;
     br600:
     begin
       tios.c_ispeed := B600;
       tios.c_ospeed := B600;
     end;
     br1200:
     begin
       tios.c_ispeed := B1200;
       tios.c_ospeed := B1200;
     end;
     br2400:
     begin
       tios.c_ispeed := B2400;
       tios.c_ospeed := B2400;
     end;
     br4800:
     begin
       tios.c_ispeed := B4800;
       tios.c_ospeed := B4800;
     end;
     br9600:
     begin
       tios.c_ispeed := B9600;
       tios.c_ospeed := B9600;
     end;
     br38400:
     begin
       tios.c_ispeed := B38400;
       tios.c_ospeed := B38400;
     end;
     br57600:
     begin
       tios.c_ispeed := B57600;
       tios.c_ospeed := B57600;
     end;
     br115200:
     begin
       tios.c_ispeed := B115200;
       tios.c_ospeed := B115200;
     end;
     else begin
       tios.c_ispeed := B19200;
       tios.c_ospeed := B19200;
     end;
  end;
  
  tios.c_cflag := tios.c_ispeed or CREAD or CLOCAL;
  
  //databits
  case PDataBits of
     db5:
        tios.c_cflag := tios.c_cflag or CS5;
     db6:
        tios.c_cflag := tios.c_cflag or CS6;
     db7:
        tios.c_cflag := tios.c_cflag or CS7;
     else
        tios.c_cflag := tios.c_cflag or CS8;
  end;

  //seta paridade, tamanho do byte, stop bits...
  //data byte size, parity and stop bits
  case PParity of
    spOdd:
      tios.c_cflag := tios.c_cflag or PARENB or PARODD;
    spEven:
      tios.c_cflag := tios.c_cflag or PARENB;
  end;
  

  if PStopBits=sb2 then
     tios.c_cflag := tios.c_cflag or CSTOPB;

  tcflush(PPortHandle, TCIFLUSH);

  r := tcsetattr(PPortHandle, TCSANOW, tios);
  if (r = -1) then
  begin
     RefreshLastOSError;
     Ok := false;
     PActive := false;
     exit;
  end;
  
  tcflush(PPortHandle, TCIOFLUSH);
  
  //seta o uso exclusivo da porta.
  //makes the serial port for exclusive access
  fpioctl(integer(PPortHandle), TIOCEXCL, nil);

  InternalClearALLBuffers;

  PActive := true;
  ok := true;
{$ENDIF}
end;

procedure TSerialPortDriver.PortStop(var Ok : Boolean);
begin
{$IF defined(WIN32) or defined(WIN64)}
  //close serial port
  if FActive then
  begin
    {$IFNDEF FPC}
    CancelIo(FPortHandle);
    {$ENDIF}

    FPortEventName := '';
    EscapeCommFunction(FPortHandle, CLRRTS);
    EscapeCommFunction(FPortHandle, CLRDTR);
    if FBackupPortSettings then
      SetCommState(FPortHandle,FSavedDCB);
    CloseHandle(FPortHandle);
    CloseHandle(FOverlapped.hEvent);
    FActive := false;
    ok := true;
  end else
    ok := true;
{$ENDIF}
{$IFDEF UNIX}
  if PActive then
  begin
    InternalClearALLBuffers;
    if PBackupPortSettings then
      SerRestoreState(PPortHandle,PSavedState);
    SerClose(PPortHandle);
  end;
  ok := true;
{$ENDIF}
end;

function TSerialPortDriver.ComSettingsOK: Boolean;
{$IF defined(WIN32) or defined(WIN64)}
var
  strdcb : String;
  vardcb : DCB;
begin
  strdcb := MakeDCBString;
  Result := COMExist(FPortName) and BuildCommDCB(PChar(strdcb), vardcb);
{$ENDIF}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  Result := COMExist(PPortName);
{$ENDIF}
end;

procedure TSerialPortDriver.SetCOMPort(const v:string);
begin
  DoExceptionInActive;
  if COMExist(v) then
    FPortName := v
  else
    if (v='(none)')  then
       FPortName:=''
    else
       raise Exception.Create(v+': '+SserialPortNotExist);
end;

function TSerialPortDriver.COMExist(const v : string): Boolean;
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring : String;
  d : DCB;
begin
  dcbstring := v+': baud=1200 parity=N data=8 stop=1';
  Result := BuildCommDCB(PChar(dcbstring),d)
{$ENDIF}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
var
   i : Integer;
begin
  Result := false;
  for i := 0 to high(PortPrefix) do
  begin
    if (LeftStr(v, Length(PortPrefix[i]))=PortPrefix[i]) and FileExists('/dev/'+v) then
    begin
      Result := true;
      exit;
    end;
  end;
{$ENDIF}
end;

procedure TSerialPortDriver.SetTimeOut(v : Cardinal);
begin
  DoExceptionInActive;
  FTimeout := v;
end;

procedure TSerialPortDriver.SetBaundRate(v : TSerialBaundRate);
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring : String;
  d : DCB;
  Old : TSerialBaundRate;
begin
  DoExceptionInActive;

  old := FBaundRate;
  FBaundRate := v;
  dcbstring := MakeDCBString;

  if not BuildCommDCB(PChar(dcbstring),d) then
  begin
    RefreshLastOSError;
    FBaundRate := old;
    raise Exception.Create(SinvalidMode);
  end;
{$ENDIF}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  PBaundRate := v;
{$ENDIF}
end;

procedure TSerialPortDriver.SetStopBits(v : TSerialStopBits);
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring : String;
  d : DCB;
  Old : TSerialStopBits;
begin
  DoExceptionInActive;

  old := FStopBits;
  FStopBits := v;
  dcbstring := MakeDCBString;

  if not BuildCommDCB(PChar(dcbstring),d) then
  begin
    RefreshLastOSError;
    FStopBits := old;
    raise Exception.Create(SinvalidMode);
  end;
{$ENDIF}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  PStopBits := v;
{$ENDIF}
end;

procedure TSerialPortDriver.SetParity(v : TSerialParity);
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring : String;
  d : DCB;
  Old : TSerialParity;
begin
  DoExceptionInActive;

  old := FParity;
  FParity := v;
  dcbstring := MakeDCBString;

  if not BuildCommDCB(PChar(dcbstring),d) then
  begin
    RefreshLastOSError;
    FParity := old;
    raise Exception.Create(SinvalidMode);
  end;
{$ENDIF}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  PParity := v;
{$ENDIF}
end;

procedure TSerialPortDriver.SetDataBits(v : TSerialDataBits);
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring : String;
  d : DCB;
  Old : TSerialDataBits;
begin
  DoExceptionInActive;

  old := FDataBits;
  FDataBits := v;
  dcbstring := MakeDCBString;

  if not BuildCommDCB(PChar(dcbstring), d) then
  begin
    RefreshLastOSError;
    FDataBits := old;
    raise Exception.Create(SinvalidMode);
  end;
{$ENDIF}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  PDataBits := v;
{$ENDIF}
end;

{$IF defined(WIN32) or defined(WIN64)}
function TSerialPortDriver.MakeDCBString: String;
begin
  Result := '';
  case FBaundRate of
    br110:
      Result := 'baud=110 ';
    br300:
      Result := 'baud=300 ';
    br600:
      Result := 'baud=600 ';
    br1200:
      Result := 'baud=1200 ';
    br2400:
      Result := 'baud=2400 ';
    br4800:
      Result := 'baud=4800 ';
    br9600:
      Result := 'baud=9600 ';
    br19200:
      Result := 'baud=19200 ';
    br38400:
      Result := 'baud=38400 ';
    br57600:
      Result := 'baud=57600 ';
    br115200:
      Result := 'baud=115200 ';
    else
      Result := 'baud=19200 ';
  end;

  case FParity of
    spNone:
      Result := Result + 'parity=N ';
    spOdd:
      Result := Result + 'parity=O ';
    spEven:
      Result := Result + 'parity=E ';
    else
      Result := Result + 'parity=N ';
  end;

  case FStopBits of
    sb1:
      Result := Result + 'stop=1 ';
    sb2:
      Result := Result + 'stop=2 ';
    else
      Result := Result + 'stop=1 ';
  end;

  case FDataBits of
    db5:
      Result := Result + 'data=5';
    db6:
      Result := Result + 'data=6';
    db7:
      Result := Result + 'data=7';
    db8:
      Result := Result + 'data=8';
    else
      Result := Result + 'data=8';
  end;
end;
{$ENDIF}

procedure TSerialPortDriver.ClearALLBuffers;
{$IF defined(WIN32) or defined(WIN64)}
var
  IOCTL_SERIAL_CONFIG_SIZE : Cardinal;
  dwFlags, vBytesRet : Cardinal;
  buf2 : array[0..8192] of byte;
begin
  IOCTL_SERIAL_CONFIG_SIZE := CTL_CODE (FILE_DEVICE_SERIAL_PORT, 32, METHOD_BUFFERED, FILE_ANY_ACCESS);
  dwFlags := PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR;
  DeviceIoControl(FPortHandle, IOCTL_SERIAL_CONFIG_SIZE, @buf2, Length(buf2), @buf2, Length(buf2), vBytesRet, nil);
  FlushFileBuffers(FPortHandle);
  PurgeComm(FPortHandle, dwFlags);
{$ENDIF}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  //flush buffers...
  tcflush(PPortHandle, TCIFLUSH);
  tcflush(PPortHandle, TCIOFLUSH);
  //purge comm...
  {$IFDEF LINUX}
  fpioctl(integer(PPortHandle), TCIOFLUSH, nil);
  {$ELSE}
  fpioctl(integer(PPortHandle), TIOCFLUSH, nil);
  {$ENDIF}
{$ENDIF}
end;

end.