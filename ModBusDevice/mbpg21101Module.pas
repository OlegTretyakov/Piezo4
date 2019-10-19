unit mbpg21101Module;

interface

uses
  System.Classes,
  DeviceModule,
  ModBusDeviceInterface,
  dmChannelsInterface,
  mbProgrammerInterface,
  AbstractTag,
  AnalogBLock,
  DiscreteBlock,
  ChipAbstractInterface,
  Vodopad.Timer;
type

  TVirtualPosition = class(TComponent)
   public
    fRegistersBlock: TAnalogBlock;
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

  Tmbpg21101Module = class(TDeviceModule,
                      IdmChannels,
                      ImbProgrammer,
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
    procedure NotifyReadOk(Sender: TObject); stdcall;
    procedure NotifyReadFault(Sender: TObject);stdcall;
    procedure NotifyWriteOk(Sender: TObject); stdcall;
    procedure NotifyWriteFault(Sender: TObject); stdcall;
    procedure NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn); stdcall;
    procedure RemoveTag(Sender: TObject); stdcall;
    {IdmChannels}
    function GetChannelsCount : word; stdcall;
    {ImbProgrammer}
    function ChipSupported(GUID : TGUID): boolean; stdcall;
    function SupportedChip :TGUID; stdcall;
    procedure FillSupportedOperations(AOperations : TStrings);stdcall;
    function GetEnabled(AIndex : Byte): boolean; stdcall;
    procedure SetEnabled(AIndex : Byte; AValue: boolean); stdcall;
    function Ready: boolean; stdcall;
    function StartOperation(AOperation : Cardinal): boolean;stdcall;
    function GetRegisters(ABoardPosition : byte; const ADest : IChipAbstract) : boolean; stdcall;
    function SetRegisters(ABoardPosition : byte; const ASource : IChipAbstract) : boolean; stdcall;
   protected
    procedure AfterCreate;override;
    procedure BeforeDestroy; override;
  end;


implementation   
uses
  Winapi.Windows,
  System.SysUtils,
  AbstractDeviceInterface,
  MilandrRev8.Consts;

const
C_ID : Word = 211;
C_Ver : Word = 01;

function GetMbModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := Tmbpg21101Module;
end; exports GetMbModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;



procedure Tmbpg21101Module.AfterCreate;
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
                                                    Self, fModbusDevice.Station,
                                                    BaseAddress, 1, false) as TProgrammerDiscreteBlock;
  fReadyBlock := fModbusDevice.Driver.CreateDiscrete(TProgrammerDiscreteBlock,
                                                    self,
                                                    fModbusDevice.Station,
                                                    BaseAddress, 1, true) as TProgrammerDiscreteBlock;
  fReadyBlock.AddCallBacks(Self);
  fOperationBlock := fModbusDevice.Driver.CreateAnalog(Self, fModbusDevice.Station, BaseAddress, 1, false);
  fMasksBlock := fModbusDevice.Driver.CreateDiscrete(Self, fModbusDevice.Station, BaseAddress+10, fPositionsCount, false);
  fVirtualPosition := TVirtualPosition.Create(Self, fModbusDevice, BaseAddress);
end;

procedure Tmbpg21101Module.BeforeDestroy;
begin
  fTimeOutTimer.Enabled := false;
  fReadyBlock.RemoveCallBacks(Self);
end;

function Tmbpg21101Module.GetChannelsCount: word;
begin
  Result := fPositionsCount;
end;

function Tmbpg21101Module.GetEnabled(AIndex: Byte): boolean;
begin
  Result := (fPositionsCount > 0)
        and (AIndex < fMasksBlock.Size)
        and fMasksBlock.Values[AIndex];
end;

procedure Tmbpg21101Module.SetEnabled(AIndex : Byte; AValue :Boolean);
begin
  if (fPositionsCount > 0) and (AIndex < fMasksBlock.Size) then
    fMasksBlock.Values[AIndex] := AValue;
end;

function Tmbpg21101Module.GetRegisters(ABoardPosition: Byte; const ADest: IChipAbstract): boolean;
begin
  Result := (fPositionsCount > 0) and fVirtualPosition.GetRegisters(ABoardPosition, ADest);
end;

procedure Tmbpg21101Module.NotifyReadFault(Sender: TObject);
begin
end;

procedure Tmbpg21101Module.NotifyReadOk(Sender: TObject);
begin

end;

procedure Tmbpg21101Module.NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn);
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

procedure Tmbpg21101Module.NotifyWriteFault(Sender: TObject);
begin
end;

procedure Tmbpg21101Module.NotifyWriteOk(Sender: TObject);
begin
end;

procedure Tmbpg21101Module.OnOperationTimeOut(Sender : TObject);
begin
  fTimeOutTimer.Enabled := False;
  fReadyBlock.AutoRead := False;
  fEventSubscribers.Execute(Self, C_ProgrammerOperationTimeOut, nil);
end;

function Tmbpg21101Module.Ready: Boolean;
begin
  Result := (fPositionsCount > 0)
        and fReadyBlock.Values[0];
end;

procedure Tmbpg21101Module.RemoveTag(Sender: TObject);
begin

end;

function Tmbpg21101Module.SetRegisters(ABoardPosition: Byte; const ASource: IChipAbstract): boolean;
begin
  Result := (fPositionsCount > 0) and fVirtualPosition.SetRegisters(ABoardPosition, ASource);
end;

function Tmbpg21101Module.StartOperation(AOperation : Cardinal): Boolean;
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

procedure Tmbpg21101Module.FillSupportedOperations(AOperations : TStrings);
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

function Tmbpg21101Module.SupportedChip: TGUID;
begin
  Result := MilandrRev8.Consts.C_GUID;
end;

function Tmbpg21101Module.ChipSupported(GUID: TGUID): boolean;
begin
  Result := IsEqualGUID(GUID, MilandrRev8.Consts.C_GUID);
end;

{ TVirtualPosition }

const                                                                                //14
C_BitIdxToElement : array [MilandrRev8.Consts.C_BitMinIndex..MilandrRev8.Consts.C_BitMaxIndex] of Byte = (14,//test
                                                                                                          0, //inf
                                                                                                          1, //lin
                                                                                                          2, //scale
                                                                                                          3, //cub
                                                                                                          4, //four
                                                                                                          5, //fifth
                                                                                                          6, //ofs
                                                                                                          7, //cdacc
                                                                                                          8,//cdacf
                                                                                                          9,//div
                                                                                                          12,//sc
                                                                                                          13,//vtune
                                                                                                          11,//ampl
                                                                                                          10//fch
                                                                                                          );

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

function TVirtualPosition.GetRegisters(ABoardPosition: Byte; const ADest: IChipAbstract): boolean;
var
  vBitIdx : Byte;
//vAdapter : TSmallIntToWord;
begin 
  Result := (ABoardPosition > 0)
            and (ABoardPosition <=Tmbpg21101Module(Owner).fPositionsCount)
            and IsEqualGUID(ADest.ChipGUID, MilandrRev8.Consts.C_GUID);
  if not Result then
    Exit;
  fRegistersBlock.MemAddress := fBaseAddress + (ABoardPosition-1) * 20 + 101;
  Result := fRegistersBlock.ReadSyn;
  if not Result then
    Exit;
  for  vBitIdx:= Low(C_BitIdxToElement) to High(C_BitIdxToElement) do
  begin
    {vAdapter.w := fRegistersBlock.Values[C_BitIdxToElement[vBitIdx]];
    ADest.BitValue[vBitIdx] := vAdapter.s; }
    ADest.BitValue[vBitIdx] := fRegistersBlock.Values[C_BitIdxToElement[vBitIdx]];
  end;
end;

function TVirtualPosition.SetRegisters(ABoardPosition: Byte; const ASource: IChipAbstract): boolean;
var
  vBitIdx : Byte;
//vAdapter : TSmallIntToWord;
begin
  Result := (ABoardPosition > 0)
            and (ABoardPosition <=Tmbpg21101Module(Owner).fPositionsCount)
            and IsEqualGUID(ASource.ChipGUID, C_GUID);
  if not Result then
    Exit;
  fRegistersBlock.MemAddress := fBaseAddress + (ABoardPosition-1) * 20 + 101;
  for  vBitIdx:= Low(C_BitIdxToElement) to High(C_BitIdxToElement) do
  begin
    {vAdapter.s := ASource.BitValue[vBitIdx];
    fRegistersBlock.Values[C_BitIdxToElement[vBitIdx]] := vAdapter.w; }
    fRegistersBlock.Values[C_BitIdxToElement[vBitIdx]] := ASource.BitValue[vBitIdx];
  end;
  Result := fRegistersBlock.WriteSyn;
end;

end.
