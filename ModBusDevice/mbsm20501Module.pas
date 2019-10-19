unit mbsm20501Module;

interface
  uses
    System.Classes,
    System.Threading,
    System.SyncObjs,
    DeviceModule,
    ModBusDeviceInterface,
    mbsm20501Interface,
    dmChannelsInterface,
    AbstractTag,
    DiscreteBlock,
    AnalogBLock;

  type

  Tmbsm20501Module = class(TDeviceModule,
                      Imbsm20501,
                      IdmChannels,
                      IHMITagInterface)
   private
    fPositionsCount : word;
    fCLKModeCoilsBlock,
    fDAModeCoilsBlock,
    fMOUTModeCoilsBlock,
    fCLKCoilsBlock,
    fDACoilsBlock,
    fMOUTCoilsBlock,
    fCLKDiscretesBlock,
    fDADiscretesBlock,
    fMOUTDiscretesBlock : TDiscreteBlock;
    fMOUTSwitchesBlock : TAnalogBlock;
    fWriteTask : ITask;
    fWriteTasks : TWriteTasks;
    fCS : TCriticalSection;
    procedure WriteAsyn(Sender : TObject);
    {IdmChannels}
    function GetChannelsCount : word; stdcall;
    {Imbsm20501}
    function GetAutoRead : boolean; stdcall;
    procedure SetAutoRead(const Value : boolean); stdcall;
    procedure SetCLKMode(AIndex : Byte; const Value : boolean); stdcall;
    function GetCLKMode(AIndex : Byte) : boolean; stdcall;
    procedure SetDAMode(AIndex : Byte; const Value : Boolean); stdcall;
    function GetDAMode(AIndex : Byte): boolean; stdcall;
    procedure SetMOUTMode(AIndex : Byte; const Value : boolean);stdcall;
    function GetMOUTMode(AIndex : Byte): boolean;stdcall;
    procedure SetCLK(AIndex : Byte; const Value : boolean);stdcall;
    function GetCLK(AIndex : Byte) : boolean;stdcall;
    procedure SetDA(AIndex : Byte; const Value : boolean);stdcall;
    function GetDA(AIndex : Byte) : boolean;stdcall;
    procedure SetMOUT(AIndex : Byte; const Value : boolean);stdcall;
    function GetMOUT(AIndex : Byte) : boolean;stdcall;
    procedure SetMOUTSwitch(AIndex : Byte; const Value : Byte);stdcall;
    function GetMOUTSwitch(AIndex: Byte): Byte;stdcall;
    function GetCLKDR(AIndex : Byte): boolean; stdcall;
    function GetDADR(AIndex : Byte): boolean; stdcall;
    function GetMOUTDR(AIndex :Byte): boolean; stdcall;
   {IBlockNotifyInterface}
    procedure NotifyReadOk(Sender:TObject); stdcall;
    procedure NotifyReadFault(Sender:TObject);stdcall;
    procedure NotifyWriteOk(Sender:TObject); stdcall;
    procedure NotifyWriteFault(Sender:TObject); stdcall;
    procedure NotifyTagChange(Sender:TObject; AChangedIn : TTagChangedIn); stdcall;
    procedure RemoveTag(Sender:TObject);stdcall;
   protected
    procedure AfterCreate;override;
    procedure BeforeDestroy;override;
   public
    class procedure CMDStartConditions(const AConditions : TStrings);override;
  end;


implementation
uses
  System.SysUtils;

{ Tmbsm20501Module }

const
C_ID : Word = 205;
C_Ver : Word = 01;

function GetMbModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := Tmbsm20501Module;
end; exports GetMbModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;

procedure Tmbsm20501Module.AfterCreate;
var
vModuleInfoBlock : TAnalogBlock;
begin
  inherited;
  fPositionsCount := 0;
  fWriteTask := nil;
  fWriteTasks := [];
  fCS := TCriticalSection.Create;
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
  if (fPositionsCount < 1) or (fPositionsCount > 125) then
    Exit;
  fCLKModeCoilsBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress, TDiscreteSize(fPositionsCount), false);
  fDAModeCoilsBlock:= fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress+100, TDiscreteSize(fPositionsCount), false);
  fMOUTModeCoilsBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress+200, TDiscreteSize(fPositionsCount), false);
  fCLKCoilsBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress+300, TDiscreteSize(fPositionsCount), false);
  fDACoilsBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress+400, TDiscreteSize(fPositionsCount), false);
  fMOUTCoilsBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress+500, TDiscreteSize(fPositionsCount), false);
  fCLKDiscretesBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress, TDiscreteSize(fPositionsCount), true);
  fDADiscretesBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress+100, TDiscreteSize(fPositionsCount), true);
  fMOUTDiscretesBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress+200, TDiscreteSize(fPositionsCount), true);
  fMOUTSwitchesBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, TAnalogSize(fPositionsCount), false);
  fCLKModeCoilsBlock.AddCallBacks(self);
  fDAModeCoilsBlock.AddCallBacks(self);
  fMOUTModeCoilsBlock.AddCallBacks(self);
  fCLKCoilsBlock.AddCallBacks(self);
  fDACoilsBlock.AddCallBacks(self);
  fMOUTCoilsBlock.AddCallBacks(self);
  fCLKDiscretesBlock.AddCallBacks(self);
  fDADiscretesBlock.AddCallBacks(self);
  fMOUTDiscretesBlock.AddCallBacks(self);
  fMOUTSwitchesBlock.AddCallBacks(self);
end;

procedure Tmbsm20501Module.BeforeDestroy;
begin
  if Assigned(fWriteTask) then
    fWriteTask.Wait(50000);
  SetAutoRead(false);
  fMOUTSwitchesBlock.RemoveCallBacks(self);
  fMOUTDiscretesBlock.RemoveCallBacks(self);
  fDADiscretesBlock.RemoveCallBacks(self);
  fCLKDiscretesBlock.RemoveCallBacks(self);
  fMOUTCoilsBlock.RemoveCallBacks(self);
  fDACoilsBlock.RemoveCallBacks(self);
  fCLKCoilsBlock.RemoveCallBacks(self);
  fMOUTModeCoilsBlock.RemoveCallBacks(self);
  fDAModeCoilsBlock.RemoveCallBacks(self);
  fCLKModeCoilsBlock.RemoveCallBacks(self);
  FreeAndNil(fCS);
  inherited;
end;


class procedure Tmbsm20501Module.CMDStartConditions(const AConditions: TStrings);
begin
  inherited;
  AConditions.Add('sm');
end;

function Tmbsm20501Module.GetAutoRead: boolean;
begin
  Result := fCLKModeCoilsBlock.AutoRead
        or fDAModeCoilsBlock.AutoRead
        or fMOUTModeCoilsBlock.AutoRead
        or fCLKCoilsBlock.AutoRead
        or fDACoilsBlock.AutoRead
        or fMOUTCoilsBlock.AutoRead
        or fMOUTSwitchesBlock.AutoRead;
end;

procedure Tmbsm20501Module.SetAutoRead(const Value: boolean);
begin
  fCLKModeCoilsBlock.AutoRead := Value;
  fDAModeCoilsBlock.AutoRead := Value;
  fMOUTModeCoilsBlock.AutoRead := Value;
  fCLKCoilsBlock.AutoRead := Value;
  fDACoilsBlock.AutoRead := Value;
  fMOUTCoilsBlock.AutoRead := Value;
  fMOUTSwitchesBlock.AutoRead := Value;
  fMOUTDiscretesBlock.AutoRead := Value;
  fDADiscretesBlock.AutoRead := Value;
  fCLKDiscretesBlock.AutoRead := Value;
end;

function Tmbsm20501Module.GetChannelsCount: word;
begin
  result := fPositionsCount;
end;

function Tmbsm20501Module.GetCLK(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fCLKCoilsBlock.Size then
    result := fCLKCoilsBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetCLKDR(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fCLKDiscretesBlock.Size then
    result := fCLKDiscretesBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetCLKMode(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fCLKModeCoilsBlock.Size then
    result := fCLKModeCoilsBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetDA(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fDACoilsBlock.Size then
    result := fDACoilsBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetDADR(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fDADiscretesBlock.Size then
    result := fDADiscretesBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetDAMode(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fDAModeCoilsBlock.Size then
    result := fDAModeCoilsBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetMOUT(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fMOUTCoilsBlock.Size then
    result := fMOUTCoilsBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetMOUTDR(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fMOUTDiscretesBlock.Size then
    result := fMOUTDiscretesBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetMOUTMode(AIndex: Byte): boolean;
begin
  result := false;
  if AIndex < fMOUTModeCoilsBlock.Size then
    result := fMOUTModeCoilsBlock.Values[AIndex];
end;

function Tmbsm20501Module.GetMOUTSwitch(AIndex: Byte): Byte;
begin
  result := 0;
  if AIndex < fMOUTSwitchesBlock.Size then
    result := Byte(fMOUTSwitchesBlock.Values[AIndex]);
end;

procedure Tmbsm20501Module.NotifyReadFault(Sender: TObject);

var
  vTask : TsmReadTask;
  vMessage : pReadMessage;
begin
  if (Sender = fMOUTSwitchesBlock) then
     vTask := smrdMOUTSwitch
  else if (Sender = fMOUTDiscretesBlock) then
     vTask := smrdDrMOUT
  else if (Sender = fDADiscretesBlock) then
     vTask := smrdDrDA
  else if (Sender = fCLKDiscretesBlock) then
     vTask := smrdDrCLK
  else if (Sender = fMOUTCoilsBlock) then
     vTask := smrdMOUT
  else if (Sender = fDACoilsBlock) then
     vTask := smrdDA
  else if (Sender = fCLKCoilsBlock) then
     vTask := smrdCLK
  else if (Sender = fMOUTModeCoilsBlock) then
     vTask := smrdMOUTMode
  else if (Sender = fDAModeCoilsBlock) then
     vTask := smrdDAMode
  else if (Sender = fCLKModeCoilsBlock) then
     vTask := smrdCLKMode
  else
    Exit;

  New(vMessage);
  try
    vMessage.IOTask := vTask;
    fEventSubscribers.Execute(Self, C_ServiceModuleOnReadFail, vMessage);
  finally
    Dispose(vMessage);
  end;
end;

procedure Tmbsm20501Module.NotifyReadOk(Sender: TObject);
var
  vTask : TsmReadTask;
  vMessage : pReadMessage;
begin
  if (Sender = fMOUTSwitchesBlock) then
     vTask := smrdMOUTSwitch
  else if (Sender = fMOUTDiscretesBlock) then
     vTask := smrdDrMOUT
  else if (Sender = fDADiscretesBlock) then
     vTask := smrdDrDA
  else if (Sender = fCLKDiscretesBlock) then
     vTask := smrdDrCLK
  else if (Sender = fMOUTCoilsBlock) then
     vTask := smrdMOUT
  else if (Sender = fDACoilsBlock) then
     vTask := smrdDA
  else if (Sender = fCLKCoilsBlock) then
     vTask := smrdCLK
  else if (Sender = fMOUTModeCoilsBlock) then
     vTask := smrdMOUTMode
  else if (Sender = fDAModeCoilsBlock) then
     vTask := smrdDAMode
  else if (Sender = fCLKModeCoilsBlock) then
     vTask := smrdCLKMode
  else
    Exit;

  New(vMessage);
  try
    vMessage.IOTask := vTask;
    fEventSubscribers.Execute(Self, C_ServiceModuleOnRead, vMessage);
  finally
    Dispose(vMessage);
  end;
end;

procedure Tmbsm20501Module.NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn);
begin
end;

procedure Tmbsm20501Module.NotifyWriteFault(Sender: TObject);
var
  vTask : TsmWriteTask;
  vMessage : pWriteMessage;
begin
  if (Sender = fMOUTSwitchesBlock) then
     vTask := smioMOUTSwitch
  else if (Sender = fMOUTCoilsBlock) then
     vTask := smioMOUT
  else if (Sender = fDACoilsBlock) then
     vTask := smioDA
  else if (Sender = fCLKCoilsBlock) then
     vTask := smioCLK
  else if (Sender = fMOUTModeCoilsBlock) then
     vTask := smioMOUTMode
  else if (Sender = fDAModeCoilsBlock) then
     vTask := smioDAMode
  else if (Sender = fCLKModeCoilsBlock) then
     vTask := smioCLKMode
  else
    Exit;
  New(vMessage);
  try
    vMessage.IOTask := vTask;
    fEventSubscribers.Execute(Self, C_ServiceModuleOnWriteFail, vMessage);
  finally
    Dispose(vMessage);
  end;
end;

procedure Tmbsm20501Module.NotifyWriteOk(Sender: TObject);
var
  vTask : TsmWriteTask;
  vMessage : pWriteMessage;
begin
  if (Sender = fMOUTSwitchesBlock) then
     vTask := smioMOUTSwitch
  else if (Sender = fMOUTCoilsBlock) then
     vTask := smioMOUT
  else if (Sender = fDACoilsBlock) then
     vTask := smioDA
  else if (Sender = fCLKCoilsBlock) then
     vTask := smioCLK
  else if (Sender = fMOUTModeCoilsBlock) then
     vTask := smioMOUTMode
  else if (Sender = fDAModeCoilsBlock) then
     vTask := smioDAMode
  else if (Sender = fCLKModeCoilsBlock) then
     vTask := smioCLKMode
  else
    Exit;
  New(vMessage);
  try
    vMessage.IOTask := vTask;
    fEventSubscribers.Execute(Self, C_ServiceModuleOnWrite, vMessage);
  finally
    Dispose(vMessage);
  end;
end;

procedure Tmbsm20501Module.RemoveTag(Sender: TObject);
begin
end;

procedure Tmbsm20501Module.SetCLK(AIndex: Byte; const Value: boolean);
begin
  if Assigned(fCLKCoilsBlock) and (AIndex < fCLKCoilsBlock.Size) then
  begin
    fCS.Enter;
    try
      fCLKCoilsBlock.Values[AIndex] := Value;
      Include(fWriteTasks, smioCLK);
      if not assigned(fWriteTask) then
        fWriteTask := TTask.Run(nil, WriteAsyn);
    finally
      fCS.Leave;
    end;
  end;
end;

procedure Tmbsm20501Module.SetCLKMode(AIndex: Byte; const Value: boolean);
begin
  if Assigned(fCLKModeCoilsBlock) and (AIndex < fCLKModeCoilsBlock.Size) then
  begin
    fCS.Enter;
    try
      fCLKModeCoilsBlock.Values[AIndex] := Value;
      Include(fWriteTasks, smioCLKMode);
      if not assigned(fWriteTask) then
        fWriteTask := TTask.Run(nil, WriteAsyn);
    finally
      fCS.Leave;
    end;
  end;
end;

procedure Tmbsm20501Module.SetDA(AIndex: Byte; const Value: boolean);
begin
  if Assigned(fDACoilsBlock) and (AIndex < fDACoilsBlock.Size) then
  begin
    fCS.Enter;
    try
      fDACoilsBlock.Values[AIndex] := Value;
      Include(fWriteTasks, smioDA);
      if not assigned(fWriteTask) then
        fWriteTask := TTask.Run(nil, WriteAsyn);
    finally
      fCS.Leave;
    end;
  end;
end;

procedure Tmbsm20501Module.SetDAMode(AIndex: Byte; const Value: boolean);
begin
  if Assigned(fDAModeCoilsBlock) and (AIndex < fDAModeCoilsBlock.Size) then
  begin
    fCS.Enter;
    try
      fDAModeCoilsBlock.Values[AIndex] := Value;
      include(fWriteTasks, smioDAMode);
      if not assigned(fWriteTask) then
        fWriteTask := TTask.Run(nil, WriteAsyn);
    finally
      fCS.Leave;
    end;
  end;
end;

procedure Tmbsm20501Module.SetMOUT(AIndex: Byte; const Value: boolean);
begin
  if Assigned(fMOUTCoilsBlock) and (AIndex < fMOUTCoilsBlock.Size) then
  begin
    fCS.Enter;
    try
      fMOUTCoilsBlock.Values[AIndex] := Value;
      Include(fWriteTasks, smioMOUT);
      if not assigned(fWriteTask) then
        fWriteTask := TTask.Run(nil, WriteAsyn);
    finally
      fCS.Leave;
    end;
  end;
end;

procedure Tmbsm20501Module.SetMOUTMode(AIndex: Byte; const Value: boolean);
begin
  if Assigned(fMOUTModeCoilsBlock) and (AIndex < fMOUTModeCoilsBlock.Size) then
  begin
    fCS.Enter;
    try
      fMOUTModeCoilsBlock.Values[AIndex] := Value;
      Include(fWriteTasks, smioMOUTMode);
      if not assigned(fWriteTask) then
        fWriteTask := TTask.Run(nil, WriteAsyn);
    finally
      fCS.Leave;
    end;
  end;
end;

procedure Tmbsm20501Module.SetMOUTSwitch(AIndex: Byte; const Value: Byte);
begin
  if Assigned(fMOUTSwitchesBlock) and (AIndex < fMOUTSwitchesBlock.Size) then
  begin
    fCS.Enter;
    try
      fMOUTSwitchesBlock.Values[AIndex] := Value;
      Include(fWriteTasks, smioMOUTSwitch);
      if not assigned(fWriteTask) then
        fWriteTask := TTask.Run(nil, WriteAsyn);
    finally
      fCS.Leave;
    end;
  end;
end;

procedure Tmbsm20501Module.WriteAsyn(Sender : TObject);
var
  vTask : TsmWriteTask;
begin
  if (fWriteTasks = []) then
    Exit;

  sleep(50);
  fCS.Enter;
  try
    for vTask := Low(vTask) to High(vTask) do
    begin
      if (vTask in fWriteTasks) then
      begin
        case vTask of
          smioCLKMode:
          begin
            fCLKModeCoilsBlock.WriteAsyn;
            fCLKModeCoilsBlock.ReadAsyn;
          end;
          smioDAMode:
          begin
            fDAModeCoilsBlock.WriteAsyn;
            fDAModeCoilsBlock.ReadAsyn;
          end;
          smioMOUTMode:
          begin
            fMOUTModeCoilsBlock.WriteAsyn;
            fMOUTModeCoilsBlock.ReadAsyn;
          end;
          smioCLK:
          begin
            fCLKCoilsBlock.WriteAsyn;
            fCLKCoilsBlock.ReadAsyn;
          end;
          smioDA:
          begin
            fDACoilsBlock.WriteAsyn;
            fDACoilsBlock.ReadAsyn;
          end;
          smioMOUT:
          begin
            fMOUTCoilsBlock.WriteAsyn;
            fMOUTCoilsBlock.ReadAsyn;
          end;
          smioMOUTSwitch:
          begin
            fMOUTSwitchesBlock.WriteAsyn;
            fMOUTSwitchesBlock.ReadAsyn;
          end;
        end;
      end;
    end;
    fWriteTask := nil;
    fWriteTasks := [];
  finally
    fCS.Leave;
  end;
end;

end.
