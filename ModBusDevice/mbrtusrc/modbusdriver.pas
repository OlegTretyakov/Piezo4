unit ModBusDriver;

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver,
  ProtocolTypes, AbstractTag, CircleQueue, Windows, DiscreteBlock, AnalogBLock;

type
  TModBusDriver = class(TProtocolDriver)
  protected
    FIOPacketsQueue : TCircleQueue<TIOPacket>;
    FInternalDelayBetweenCmds : Cardinal;
    procedure BuildReadPacket(const ATagObj : TAbstractTag; APacket: pIOPacket); virtual;
    procedure BuildWritePacket(const ATagObj : TAbstractTag; APacket: pIOPacket); virtual;
    function ParcePacket(APacket:pIOPacket; const ATagObj : TAbstractTag): TProtocolIOResult; virtual;
    function DoWrite(const ATagObj : TAbstractTag): TProtocolIOResult; override;
    function DoRead(const ATagObj : TAbstractTag): TProtocolIOResult; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function CreateDiscrete(AClass : TDiscreteBlockClass; AOwner : TComponent;
                        AStation,
                        AAddress : Word;
                        ASize : TDiscreteSize;
                        AReadOnly : boolean): TDiscreteBlock; overload;
    function CreateDiscrete(AOwner : TComponent;
                        AStation,
                        AAddress : Word;
                        ASize : TDiscreteSize;
                        AReadOnly : boolean): TDiscreteBlock; overload;
    function CreateAnalog(AClass : TAnalogBlockClass; AOwner : TComponent;
                        AStation,
                        AAddress : Word;
                        ASize : TAnalogSize;
                        AReadOnly : boolean): TAnalogBlock; overload;
    function CreateAnalog(AOwner : TComponent;
                        AStation,
                        AAddress : Word;
                        ASize : TAnalogSize;
                        AReadOnly : boolean): TAnalogBlock; overload;
    property DelayBetweenCmds: Cardinal read FInternalDelayBetweenCmds write FInternalDelayBetweenCmds;
  end;

implementation


constructor TModBusDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FIOPacketsQueue := TCircleQueue<TIOPacket>.Create;
  FProtocolReady := false;
  FInternalDelayBetweenCmds := 20;
end;

function TModBusDriver.CreateAnalog(AOwner: TComponent; AStation, AAddress: Word; ASize: TAnalogSize; AReadOnly: boolean): TAnalogBlock;
begin
  result := CreateAnalog(TAnalogBlock, AOwner, AStation, AAddress, ASize, AReadOnly);
end;

function TModBusDriver.CreateAnalog(AClass: TAnalogBlockClass; AOwner: TComponent; AStation, AAddress: Word; ASize: TAnalogSize;
  AReadOnly: boolean): TAnalogBlock;
begin
  result := AClass.Create(AOwner, Self, AStation, AAddress, ASize, AReadOnly);
end;

function TModBusDriver.CreateDiscrete(AClass: TDiscreteBlockClass; AOwner: TComponent; AStation, AAddress: Word; ASize: TDiscreteSize;
  AReadOnly: boolean): TDiscreteBlock;
begin
  result := AClass.Create(AOwner, Self, AStation, AAddress, ASize, AReadOnly);
end;

function TModBusDriver.CreateDiscrete(AOwner: TComponent; AStation, AAddress: Word; ASize: TDiscreteSize; AReadOnly: boolean): TDiscreteBlock;
begin
  result := CreateDiscrete(TDiscreteBlock, AOwner, AStation, AAddress, ASize, AReadOnly);
end;

destructor TModBusDriver.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FIOPacketsQueue);
end;

procedure TModBusDriver.BuildWritePacket(const ATagObj : TAbstractTag; APacket: pIOPacket);
var
  vTag : IProtocolPLCTag;
begin
  APacket.ToWriteCount := 0;
  APacket.ToReadCount := 0;
  if Supports(ATagObj, IProtocolPLCTag, vTag) then
    vTag.SetLastError(ioCommError);
  vTag := nil;
end;

procedure TModBusDriver.BuildReadPacket(const ATagObj: TAbstractTag; APacket: pIOPacket);
var
  vTag : IProtocolPLCTag;
begin
  APacket.ToWriteCount := 0;
  APacket.ToReadCount := 0;
  if Supports(ATagObj, IProtocolPLCTag, vTag) then
    vTag.SetLastError(ioCommError);
  vTag := nil;
end;

function TModBusDriver.ParcePacket(APacket: pIOPacket; const ATagObj : TAbstractTag): TProtocolIOResult;
begin
  Result:=ioDriverError;
end;

function TModBusDriver.DoWrite(const ATagObj : TAbstractTag): TProtocolIOResult;
var
  vIOPacket : pIOPacket;
  vPacketItem : TCircleQueueItem<TIOPacket>;
  vReleasePacket : boolean;
begin
    vReleasePacket := true;
    vPacketItem := FIOPacketsQueue.Acquire;
    try
      vIOPacket := Addr(vPacketItem.Item);
      vIOPacket.DelayBetweenCommand := FInternalDelayBetweenCmds;
      BuildWritePacket(ATagObj,
                        vIOPacket);
      if (vIOPacket.ToWriteCount < 1) then
      begin
        Result := ioCommError;
        exit;
      end;
      if Assigned(FCommPort)
      and FCommPort.ReallyActive then
      begin
        if FCommPort.IOCommandSync(iocWriteRead,
                                        vIOPacket) then
          Result := ParcePacket(vIOPacket, ATagObj)
        else
          Result := ioCommError;
      end else
        Result := ioNullDriver;
    finally
      if (result <> ioOK) then
        vReleasePacket := false;
      if vReleasePacket then
        vPacketItem.Release
      else
        PostProtocolError(result, vPacketItem);
    end;
end;

function TModBusDriver.DoRead(const ATagObj : TAbstractTag): TProtocolIOResult;
var
  vIOPacket : pIOPacket;
  vPacketItem : TCircleQueueItem<TIOPacket>;
  vReleasePacket : boolean;
  vTag : IProtocolPLCTag;
begin

  vReleasePacket := true;
  vPacketItem := FIOPacketsQueue.Acquire;
  try
    vIOPacket := Addr(vPacketItem.Item);
    BuildReadPacket(ATagObj, vIOPacket);
    vIOPacket.DelayBetweenCommand := FInternalDelayBetweenCmds;
    if (vIOPacket.ToWriteCount < 1)
    or (vIOPacket.ToReadCount < 1) then
    begin
      Result := ioCommError;
      exit;
    end;
    if Assigned(FCommPort)
    and FCommPort.ReallyActive then
    begin
      if FCommPort.IOCommandSync(iocWriteRead,
                                    vIOPacket) then
      begin
        Result := ParcePacket(vIOPacket, ATagObj);
        if Supports(ATagObj, IProtocolPLCTag, vTag) then
          vTag.CallAfterRead;
        vTag := nil;
      end else
        Result := ioCommError;
    end else
      Result := ioNullDriver;
  finally
    if (result <> ioOK) then
      vReleasePacket := false;
    if vReleasePacket then
      vPacketItem.Release
    else
      PostProtocolError(result, vPacketItem);
  end;
end;

end.
