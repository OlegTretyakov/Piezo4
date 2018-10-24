unit AbstractTag;

interface

uses
  System.Classes, System.SysUtils;

type

  {:
  Defines the the update rate range of tags.
  }
  TRefreshTime = 1..$7FFFFFFF;

  TArrayOfWord = array of word;
  //TArrayOfBool = array of boolean;
  TDiscreteSize = 1..2000;
  TAnalogSize = 1..125;
  TDiscreteIndex = 0..1999;
  TAnalogIndex = 0..124;
  TTagType = (ttDiscrete, //RO one bit
              ttCoil,     //RW one bit
              ttInput,    //RO one word
              ttHolding); //RW one word
  {:
  Enumerates all tag data types.
  @value(pttDefault  Word length and data type variable.)
  @value(pttShortInt Signed Integer, 8 bits sized.)
  @value(pttByte     Unsigned Integer, 8 bits sized.)
  @value(pttSmallInt Signed Integer, 16 bits sized.)
  @value(pttWord,    Unsigned Integer, 16 bits sized.)
  @value(pttInteger  Signed Integer, 32 bits sized.)
  @value(pttDWord,   Unsigned Integer, 32 bits sized.)
  @value(pttFloat    Float, 32 bits sized.)
  }
  (*TTagDataType = (pttDefault,                    //size variable
              pttShortInt, pttByte,          //8 bits
              pttSmallInt, pttWord,          //16 bits
              pttInteger, pttDWord, pttFloat //32 bits
             );   *)


  {:
  Enumerates all commands accept by the tag.

  @value(tcScanRead        Values are read using the driver scan.)
  @value(tcScanWrite       Values are write using the driver scan.)
  @value(tcRead            Values are read synchronous (without driver scan).)
  @value(tcWrite           Values are write synchronous (without driver scan).)
  @value(tcInternalUpdate  Internal tag update command.)
  }

  //TTagCommand = (tcScanRead, tcScanWrite, tcRead, tcWrite, tcInternalUpdate);


  {:
  Enumerates all results that can be returned by the protocol driver to a
  read/write request of a tag.

  @value(ioDriverError            Internal driver error.)
  @value(ioCommError              Communication error.)
  @value(ioOk                     Sucessfull request.)
  @value(ioTimeout                Communication timeout.)
  @value(ioIllegalFunction        Invalid IO function.)
  @value(ioIllegalRegAddress      Invalid memory address.)
  @value(ioIllegalValue           Invalid value.)
  @value(ioPLCError               Device error.)
  @value(ioTagError               Internal tag error.)
  @value(ioNullDriver             Tag without a driver.)
  @value(ioIllegalStationAddress  Invalid device address.)
  @value(ioIllegalRequest         The request is invalid or not supported.)
  @value(ioObjectNotExists        The requested object doesn't exists.)
  @value(ioIllegalMemoryAddress   The request is out of bound of the memory space of device.)
  @value(ioUnknownError           A invalid error code was returned.)
  @value(ioEmptyPacket            A empty packet was returned.)
  @value(ioPartialOk              An action was partially successful.)
  }

  TProtocolIOResult = (ioNone, ioDriverError, ioCommError, ioOk, ioTimeOut,
                       ioIllegalFunction, ioIllegalRegAddress,ioIllegalValue,
                       ioPLCError, ioTagError, ioNullDriver, ioIllegalRequest,
                       ioIllegalStationAddress, ioObjectNotExists,
                       ioIllegalMemoryAddress, ioUnknownError, ioEmptyPacket,
                       ioPartialOk);


  {:
  Tag events notification interface. Used to notifies the linked controls about
  any event that occurs with the tag.
  }
  TTagChangedIn = (chInRead, chInWrite);
  IHMITagInterface = interface
    ['{4301B240-79D9-41F9-A814-68CFEFD032B8}']
    //: Called when the tag has a successful read.
    procedure NotifyReadOk(Sender : TObject); stdcall;
    //: Called when the tag has a fault read.
    procedure NotifyReadFault(Sender : TObject); stdcall;
    //: Called when the tag has a successful write.
    procedure NotifyWriteOk(Sender : TObject); stdcall;
    //: Called when the tag has a fault write.
    procedure NotifyWriteFault(Sender : TObject);stdcall;
    //: Called when the tag value changes.
    procedure NotifyTagChange(Sender : TObject; AChangedIn : TTagChangedIn);stdcall;
    //: Notifies the control about the destruction of the tag.
    procedure RemoveTag(Sender : TObject);stdcall;
  end;

  //: Base class for all tags.
  TAbstractTag = class(TComponent)
   private
    FTagType : TTagType;
   protected
    FFireAsync : Boolean;
    FDataChanged : Boolean;
    //: Stores if the tag will be updated automatically.
    FAutoRead : Boolean;
    //: Stores the device Station address of the tag.
    FStation : Word;
    //: Stores the device memory address of the tag.
    FAddress : Word;
    //: Stores how many memories are being mapped.
    FSize : Word;
    //: Stores the function code to read the values of the tag.
    FReadFunction : byte;
    //: Stores the function code to write the values of the tag.
    FWriteFunction : byte;
    //: Stores the update time of the tag.
    FRefreshInterval : TRefreshTime;
    FLastError : TProtocolIOResult;
    //: Stores the event to be called when a read has success.
    FOnReadOk : TNotifyEvent;
    //: Stores the event to be called when a read fail occurs.
    FOnReadFail : TNotifyEvent;
    //: Stores the event to be called when a value is written successfully on device.
    FOnWriteOk : TNotifyEvent;
    //: Stores the event called when a write of tag value has a failed.
    FOnWriteFail : TNotifyEvent;
    //: Stores the event called when the tag value changes, BEFORE notify the dependents of the tag.
    FOnValuesChange : TNotifyEvent;
    //: Stores the notification event interface of all dependent objects.
    FNotificationInterfaces : array of pointer;

    procedure SetLastError(AError : TProtocolIOResult); virtual;
    procedure SetDataChanged(ADataChanged : Boolean); virtual;
    //: Notifies when a successful read occurs.
    procedure NotifyReadOk; virtual;
    //: Notifies when a read fault occurs.
    procedure NotifyReadFault; virtual;
    //: Notifies when a successful write occurs.
    procedure NotifyWriteOk; virtual;
    //: Notifies when a write fault occurs.
    procedure NotifyWriteFault; virtual;
    //: Notifies when the tag value changes.
    procedure NotifyChange(AChangedIn : TTagChangedIn); virtual;
   public
    //: @exclude
    constructor Create(AOwner : TComponent;
                        AStation,
                        AAddress,
                        ASize : word;
                        AType : TTagType); reintroduce;
    //: @exclude
    destructor Destroy; override;
    function RemainingMilisecondsForNextScan: Cardinal; virtual; abstract;
    property TagType : TTagType read fTagType;
    function NeedRescan : boolean; virtual;
    //: If @true, the tag will be updated automaticaly.
    property AutoRead: Boolean read FAutoRead;
    property LastError : TProtocolIOResult read FLastError;
    //: Device Station that contains the memory being mapped, if applicable.
    property PLCStation: Word read FStation;
    //: The address of the memory being mapped.
    property MemAddress: Word read FAddress;
    //: Protocol driver function that will read the memory being mapped.
    property MemReadFunction: byte read FReadFunction;
    //: Protocol driver function that will write values in the memory being mapped.
    property MemWriteFunction: byte read FWriteFunction;
    //: Update time of the tag, in milliseconds.
    property RefreshInterval: TRefreshTime read FRefreshInterval;
    //: Number of memories being mapped, if applicable.
    property Size : Word read FSize;
    //: Event called to notify when a successful read occurs.
    property OnReadOK: TNotifyEvent read FOnReadOk write FOnReadOk;
    //: Event called when a read fault occurs.
    property OnReadFail: TNotifyEvent read FOnReadFail write FOnReadFail;
    //: Event called to notify when a write of the tag value has success.
    property OnWriteOk: TNotifyEvent read FOnWriteOk write FOnWriteOk;
    //: Event called when a write fault occurs.
    property OnWriteFail: TNotifyEvent read FOnWriteFail write FOnWriteFail;
    //: Event called when the tag value changes, AFTER notify all tag dependents.
    property OnValuesChange: TNotifyEvent read FOnValuesChange  write FOnValuesChange;
    //: Remove a interface from the tag notification list.
    function ReadSyn : boolean; virtual; abstract;
    procedure ReadAsyn; virtual; abstract;
    function WriteSyn : boolean; virtual; abstract;
    procedure WriteAsyn; virtual; abstract;
    property FireAsync : Boolean read FFireAsync write FFireAsync;
    //: Adds a new notification interface to the tag notification list.
    procedure AddCallBacks(const AObj : TObject);
    procedure RemoveCallBacks(const AObj : TObject);
    procedure RemoveDriver; virtual; abstract;
  end;

const
C_ProtocolIOResultStr : array[TProtocolIOResult] of String =(
                       'None', 'DriverError', 'CommError', 'Ok', 'TimeOut',
                       'IllegalFunction', 'IllegalRegAddress','IllegalValue',
                       'PLCError', 'TagError', 'NullDriver', 'IllegalRequest',
                       'IllegalStationAddress', 'ObjectNotExists',
                       'IllegalMemoryAddress', 'UnknownError', 'EmptyPacket',
                       'PartialOk');

implementation

uses
System.Math,
hsstrings;


constructor TAbstractTag.Create(AOwner : TComponent;
                                AStation,
                                AAddress,
                                ASize : word;
                                AType : TTagType);
begin
  inherited Create(AOwner);
  FFireAsync := false;
  fTagType := AType;
  FAutoRead := false;
  FStation := AStation;
  FAddress := AAddress;
  FSize := ASize;
  FReadFunction := 0;
  FWriteFunction := 0;
  FRefreshInterval := 1000;
  SetLength(FNotificationInterfaces, 0);
  if (ASize < 1) then
    Exit;
  case fTagType of
    ttDiscrete:
    begin
      FReadFunction := $02;
    end;
    ttCoil:
    begin
      FReadFunction := $01;
      if (ASize < 2) then
        FWriteFunction:=$05
      else
        FWriteFunction:=$0F
    end;
    ttInput:
    begin
      FReadFunction := $04;
    end;
    ttHolding:
    begin
      FReadFunction := $03;
      if (ASize < 2) then
        FWriteFunction:=$06
      else
        FWriteFunction:=$10
    end;
  end;
end;

destructor TAbstractTag.Destroy;
var
  i : Integer;
begin
  for i := 0 to High(FNotificationInterfaces) do
    IHMITagInterface(FNotificationInterfaces[i]).RemoveTag(Self);
  SetLength(FNotificationInterfaces, 0);
  inherited Destroy;
end;

procedure TAbstractTag.AddCallBacks(const AObj : TObject);
var
  vObj : IHMITagInterface;
  i : Integer;
  vpTag : Pointer;
begin
  if not Supports(AObj, IHMITagInterface, vObj) then
    raise Exception.Create(SinvalidInterface);
  vpTag := Pointer(vObj);
  vObj := nil;
  i := 0;
  while (i < Length(FNotificationInterfaces)) do
  begin
    if (vpTag = FNotificationInterfaces[i]) then
      break;
    Inc(i);
  end;

  if i < Length(FNotificationInterfaces) then //already exists
    Exit;

  SetLength(FNotificationInterfaces, Length(FNotificationInterfaces)+1);
  FNotificationInterfaces[High(FNotificationInterfaces)]:= vpTag;
end;

procedure TAbstractTag.RemoveCallBacks(const AObj : TObject);
var
  i, h : Integer;
  found : Boolean;
  vObj : IHMITagInterface;
begin
  if not Supports(AObj, IHMITagInterface, vObj) then
    Exit;
  found := false;
  h := High(FNotificationInterfaces);
  for i := 0 to h do
  begin
    if (Pointer(vObj)=FNotificationInterfaces[i]) then
    begin
      found := true;
      break;
    end;
  end;
  vObj := nil;
  if found then
  begin
    FNotificationInterfaces[i] := FNotificationInterfaces[h];
    FNotificationInterfaces[h] := nil;
    SetLength(FNotificationInterfaces, Length(FNotificationInterfaces)-1);
  end;
end;

procedure TAbstractTag.SetDataChanged(ADataChanged: Boolean);
begin
  FDataChanged := ADataChanged;
end;

procedure TAbstractTag.SetLastError(AError : TProtocolIOResult);
begin
  FLastError := AError;
end;

function TAbstractTag.NeedRescan: boolean;
begin
  result := false;
end;

procedure TAbstractTag.NotifyChange(AChangedIn : TTagChangedIn);
var
  i : Integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  //Notify the dependent objects.
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyTagChange(self, AChangedIn);
    except
    end;
  end;
  try
    if Assigned(FOnValuesChange) then
    begin
      n := FOnValuesChange;
      n(Self);
    end;
  except
  end;
end;

procedure TAbstractTag.NotifyReadOk;
var
  i : Integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyReadOk(self);
    except
    end;
  end;

  if Assigned(FOnReadOk) then
  begin
    n := FOnReadOk;
    n(self);
  end;
  if FDataChanged then
    NotifyChange(chInRead);
end;

procedure TAbstractTag.NotifyReadFault;
var
  i:Integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyReadFault(self);
    except
    end;
  end;

  if Assigned(FOnReadFail) then
  begin
    n := FOnReadFail;
    n(self);
  end;
end;

procedure TAbstractTag.NotifyWriteOk;
var
  i : Integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyWriteOk(self);
    except
    end;
  end;

  if Assigned(FOnWriteOk) then
  begin
    n := FOnWriteOk;
    n(self);
  end;
  if FDataChanged then
    NotifyChange(chInWrite);
end;

procedure TAbstractTag.NotifyWriteFault;
var
  i : Integer;
  n : TNotifyEvent;
  ni : Pointer;
begin
  for i := 0 to High(FNotificationInterfaces) do
  begin
    try
      ni := FNotificationInterfaces[i];
      IHMITagInterface(ni).NotifyWriteFault(self);
    except
    end;
  end;

  if Assigned(FOnWriteFail) then
  begin
    n := FOnWriteFail;
    n(self);
  end;
end;

end.