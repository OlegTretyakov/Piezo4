unit mbpg21201Module;

interface

uses
  System.Classes, DeviceModule, ModBusDeviceInterface,
  dmChannelsInterface, mbProgrammerInterface,
  AbstractTag, AnalogBLock, DiscreteBlock,
  ChipAbstractInterface, Vodopad.Timer;
type

  TVirtualPosition = class(TComponent)
   public
    fRegistersBlock : TAnalogBlock;
    fModbusDevice : IModBusDevice;
    fBaseAddress : Word;
    constructor Create(AOwner: TComponent; const ADevice : IModBusDevice; ABaseAddress : Word); reintroduce;
    destructor Destroy; override;
    function GetRegisters(ABoardPosition : Byte; const ADest : IChipAbstract) : boolean;
    function SetRegisters(ABoardPosition : Byte; const ASource : IChipAbstract) : boolean;
  end;
  TProgrammerDiscreteBlock = class(TDiscreteBlock)
   public
    property ValuesReaded;
    property ValuesToWrite;
  end;
  Tmbpg21201Module = class(TDeviceModule,
                      IdmChannels, ImbProgrammer,
                      IHMITagInterface)
   private
    fPositionsCount : Word;
    fStartBlock,
    fReadyBlock : TProgrammerDiscreteBlock;
    fOperationBlock : TAnalogBlock;
    fMasksBlock : TDiscreteBlock;
    fVirtualPosition : TVirtualPosition;
    fCurrOperation : Integer;
    fTimeOutTimer : TvdTimer;
    procedure OnOperationTimeOut(Sender : TObject);
    {IBlockNotifyInterface}
    procedure NotifyReadOk(Sender:TObject); stdcall;
    procedure NotifyReadFault(Sender:TObject);stdcall;
    procedure NotifyWriteOk(Sender:TObject); stdcall;
    procedure NotifyWriteFault(Sender:TObject); stdcall;
    procedure NotifyTagChange(Sender:TObject; AChangedIn : TTagChangedIn); stdcall;
    procedure RemoveTag(Sender:TObject);stdcall;
    {IdmChannels}
    function GetChannelsCount : word; stdcall;
    {ImbProgrammer}  
    function ChipSupported(GUID : TGUID):boolean; stdcall;
    function SupportedChip :TGUID; stdcall;
    procedure FillSupportedOperations(AOperations : TStrings);stdcall;
    function GetEnabled(AIndex : Byte):Boolean; stdcall;
    procedure SetEnabled(AIndex : Byte; AValue :Boolean); stdcall;
    function Ready:Boolean;stdcall;
    function StartOperation(AOperation : Cardinal):Boolean;stdcall;
    function GetRegisters(ABoardPosition : Byte; const ADest : IChipAbstract) : boolean; stdcall;
    function SetRegisters(ABoardPosition : Byte; const ASource : IChipAbstract) : boolean; stdcall;
   protected
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
  end;


implementation   
uses
Winapi.Windows,
System.SysUtils,
AbstractDeviceInterface,
MAS6279D8.Consts;

const
C_ID : Word = 212;
C_Ver : Word = 01;
//This is s MasD8

function GetMbModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := Tmbpg21201Module;
end; exports GetMbModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;



procedure Tmbpg21201Module.AfterCreate;
var
vModuleInfoBlock : TAnalogBlock;
begin
  fPositionsCount := 0;
  fCurrOperation := -1;
  fTimeOutTimer := TvdTimer.Create(self);
  fTimeOutTimer.OnTimer := OnOperationTimeOut;
  fTimeOutTimer.Interval := 30000;
  fTimeOutTimer.Enabled := false;
  vModuleInfoBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 3, true);
  try
    if vModuleInfoBlock.ReadSyn then
    begin
      fID := vModuleInfoBlock.Values[0];
      fVersion := vModuleInfoBlock.Values[1];
      fPositionsCount := vModuleInfoBlock.Values[2];
    end else
      Exit;
  finally
    FreeAndNil(vModuleInfoBlock);
  end;
  if (fPositionsCount < 1) then
    Exit;
  fStartBlock := fModbusDevice.Driver.CreateDiscrete(TProgrammerDiscreteBlock,
                                      self, fModbusDevice.Station, BaseAddress, 1, false) as TProgrammerDiscreteBlock;
  fReadyBlock := fModbusDevice.Driver.CreateDiscrete(TProgrammerDiscreteBlock,
                                                    self,
                                                    fModbusDevice.Station,
                                                    BaseAddress, 1, true) as TProgrammerDiscreteBlock;


  fReadyBlock.AddCallBacks(self);
  fOperationBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 1, false);
  fMasksBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress+10, fPositionsCount, false);
  fVirtualPosition := TVirtualPosition.Create(Self, fModbusDevice, BaseAddress);
end;

procedure Tmbpg21201Module.BeforeDestroy;
begin
  fTimeOutTimer.Enabled := false;
  fReadyBlock.RemoveCallBacks(self);
end;

function Tmbpg21201Module.GetChannelsCount: word;
begin
  Result := fPositionsCount;
end;

function Tmbpg21201Module.GetEnabled(AIndex: Byte): boolean;
begin
  Result := (fPositionsCount > 0)
        and (AIndex < fMasksBlock.Size)
        and fMasksBlock.Values[AIndex];
end;

procedure Tmbpg21201Module.SetEnabled(AIndex : Byte; AValue :Boolean);
begin
  if (fPositionsCount > 0) and (AIndex < fMasksBlock.Size) then
    fMasksBlock.Values[AIndex] := AValue;
end;

function Tmbpg21201Module.GetRegisters(ABoardPosition: byte; const ADest: IChipAbstract): boolean;
begin
  Result := (fPositionsCount > 0) and fVirtualPosition.GetRegisters(ABoardPosition, ADest);
end;

procedure Tmbpg21201Module.NotifyReadFault(Sender: TObject);
begin
end;

procedure Tmbpg21201Module.NotifyReadOk(Sender: TObject);
begin

end;

procedure Tmbpg21201Module.NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn);
begin
  if (AChangedIn <> chInRead) then
    exit;
  if ((Sender = fReadyBlock)
  and (fCurrOperation in [0..4])) then
  begin
    if fReadyBlock.Values[0] then
    begin
      fTimeOutTimer.Enabled := False;
      fReadyBlock.AutoRead := False;
      case fCurrOperation of
        1,2:// write
          fEventSubscribers.Execute(Self, C_RegistersWriteDone, nil);
        3,4://read
          fEventSubscribers.Execute(Self, C_RegistersReadDone, nil);
      end;
      fCurrOperation := -1;
      fEventSubscribers.Execute(Self, C_ProgrammerOperationFinished, nil);
    end;
  end;
end;

procedure Tmbpg21201Module.NotifyWriteFault(Sender: TObject);
begin
end;

procedure Tmbpg21201Module.NotifyWriteOk(Sender: TObject);
begin
end;

function Tmbpg21201Module.Ready: Boolean;
begin
  Result := (fPositionsCount > 0)
        and fReadyBlock.Values[0];
end;

procedure Tmbpg21201Module.RemoveTag(Sender: TObject);
begin

end;

procedure Tmbpg21201Module.OnOperationTimeOut(Sender : TObject);
begin
  fTimeOutTimer.Enabled := False;
  fReadyBlock.AutoRead := False;
  fEventSubscribers.Execute(Self, C_ProgrammerOperationTimeOut, nil);
end;

function Tmbpg21201Module.SetRegisters(ABoardPosition: Byte; const ASource: IChipAbstract): boolean;
begin
  Result := (fPositionsCount > 0) and fVirtualPosition.SetRegisters(ABoardPosition, ASource);
end;

function Tmbpg21201Module.StartOperation(AOperation : Cardinal): Boolean;
var
vOperation : Word;
begin
  vOperation := Word(AOperation);
  Result := (fPositionsCount > 0) and (vOperation in [0..4])
        and fReadyBlock.ReadSyn
        and Ready
        and fMasksBlock.WriteSyn;
  if Result then
  begin
    fOperationBlock.Values[0] := vOperation;
    Result := fOperationBlock.WriteSyn;
  end;
  if Result then
  begin
    fStartBlock.Values[0] := True;
    Result := fStartBlock.WriteSyn;
    if Result then
    begin
      fStartBlock.ValuesReaded[0] := true;
      fReadyBlock.ValuesReaded[0] := false;
      fCurrOperation := vOperation;
      fReadyBlock.RefreshInterval := 10;
      fReadyBlock.AutoRead := True;
      if GetCurrentThreadId <> MainThreadID then
      begin
        TThread.Synchronize(nil,
        procedure
        begin
          fTimeOutTimer.Enabled := true;
        end
        );
      end else
      begin
        fTimeOutTimer.Enabled := true;
      end;
    end;
  end;
end;

procedure Tmbpg21201Module.FillSupportedOperations(AOperations : TStrings);
begin
  AOperations.Clear;
  if (fPositionsCount < 1) then
    Exit;
  AOperations.AddObject('Write Work',TObject(Cardinal(131072+1)));
  AOperations.AddObject('Read Work',TObject(Cardinal(3)));
  AOperations.AddObject('Read PROM',TObject(Cardinal(4)));
  AOperations.AddObject('Write PROM',TObject(Cardinal(196608+2)));
  AOperations.AddObject('Z-State',TObject(Cardinal(0)));
end;

function Tmbpg21201Module.SupportedChip: TGUID;
begin
  Result := MAS6279D8.Consts.C_GUID;
end;

function Tmbpg21201Module.ChipSupported(GUID: TGUID): boolean;
begin
  Result := IsEqualGUID(GUID, MAS6279D8.Consts.C_GUID);
end;

{ TVirtualPosition }
const
C_BitIdxToElement : array [MAS6279D8.Consts.C_BitMinIndex..MAS6279D8.Consts.C_BitMaxIndex] of Byte = (15,16,17,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14);

constructor TVirtualPosition.Create(AOwner: TComponent; const ADevice: IModBusDevice; ABaseAddress : Word);
begin
  inherited Create(AOwner);
  fModbusDevice := ADevice;
  fBaseAddress := ABaseAddress;
  fRegistersBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, ABaseAddress+101, High(C_BitIdxToElement)+1, false);
end;

destructor TVirtualPosition.Destroy;
begin
  fModbusDevice := nil;
  inherited Destroy;
end;

type
TSmallIntToWord = record
  case integer of
    0: (s: SmallInt);
    1: (w : Word);
end;

function TVirtualPosition.GetRegisters(ABoardPosition: Byte; const ADest: IChipAbstract): boolean;
var
vBitIdx : Byte;
vAdapter : TSmallIntToWord;
begin 
  Result := (ABoardPosition > 0)
            and (ABoardPosition <=Tmbpg21201Module(Owner).fPositionsCount)
            and IsEqualGUID(ADest.ChipGUID, MAS6279D8.Consts.C_GUID);
  if not Result then
    Exit;
  fRegistersBlock.MemAddress := fBaseAddress + (ABoardPosition-1) * 20 + 101;
  Result := fRegistersBlock.ReadSyn;
  if not Result then
    Exit;
  for  vBitIdx:= Low(C_BitIdxToElement) to High(C_BitIdxToElement) do
  begin
    if C_BitIdxToElement[vBitIdx] in [5, 7..8] then
    begin
      vAdapter.w := fRegistersBlock.Values[C_BitIdxToElement[vBitIdx]];
      ADest.BitValue[vBitIdx] := vAdapter.s;
    end else
    begin
      ADest.BitValue[vBitIdx] := fRegistersBlock.Values[C_BitIdxToElement[vBitIdx]];
    end;
  end;
end;

function TVirtualPosition.SetRegisters(ABoardPosition: Byte; const ASource: IChipAbstract): boolean;
var
vBitIdx : Byte;
vAdapter : TSmallIntToWord;
begin
  Result := (ABoardPosition > 0)
            and (ABoardPosition <=Tmbpg21201Module(Owner).fPositionsCount)
            and IsEqualGUID(ASource.ChipGUID, MAS6279D8.Consts.C_GUID);
  if not Result then
    Exit;
  fRegistersBlock.MemAddress := fBaseAddress + (ABoardPosition-1) * 20 + 101;
  for  vBitIdx:= Low(C_BitIdxToElement) to High(C_BitIdxToElement) do
  begin
    if C_BitIdxToElement[vBitIdx] in [5, 7..8] then
    begin
      vAdapter.s := ASource.BitValue[vBitIdx];
      fRegistersBlock.Values[C_BitIdxToElement[vBitIdx]] := vAdapter.w;
    end else
    begin
      fRegistersBlock.Values[C_BitIdxToElement[vBitIdx]] := ASource.BitValue[vBitIdx];
    end;
  end;
  Result := fRegistersBlock.WriteSyn;
end;

end.
