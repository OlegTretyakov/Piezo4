unit mbvs20201Module;

interface
  uses
    System.Classes,
    System.Threading,
    DeviceModule,
    dmPositionControllerInterface,
    ModBusDeviceInterface,
    dmChannelsInterface,
    dmChallengeControllerInterface,
    AbstractTag,
    AnalogBLock,
    DiscreteBlock;

  type


  Tmbvs20201Module = class(TDeviceModule, IdmPositionsController,
                      IdmChannels,
                      IdmChallengeController,
                      IHMITagInterface)
   private
    fPositionsCount : word;
    fWriteTask : ITask;
    fDestroying,
    fAuto : Boolean;
    fVoltageBlock: TAnalogBlock;
    fEnableBlock,
    fProtectBlock : TDiscreteBlock;
    {IBlockNotifyInterface}
    procedure NotifyReadOk(Sender:TObject); stdcall;
    procedure NotifyReadFault(Sender:TObject); stdcall;
    procedure NotifyWriteOk(Sender:TObject); stdcall;
    procedure NotifyWriteFault(Sender:TObject); stdcall;
    procedure NotifyTagChange(Sender:TObject; AChangedIn : TTagChangedIn); stdcall;
    procedure RemoveTag(Sender:TObject); stdcall;
    {IdmChannels}
    function GetChannelsCount : word; stdcall;
    {IdmChallengeController}  
    function GetAuto : Boolean; stdcall;
    procedure SetAuto(const Value : Boolean); stdcall;
    procedure CallChallenge; stdcall;
    {IdmPositionsController}
    procedure GetVoltageValues(ADest : pPositionsVoltages); stdcall;
    procedure SetVoltageValues(ASource : pPositionsVoltages); stdcall;
    function GetProtectState(AIndex : word):Boolean; stdcall;
    function GetEnabled(AIndex : word):Boolean; stdcall;
    procedure SetEnabled(AIndex : word; Value : boolean); stdcall;
    function GetProtectTimeStamp : TDateTime; stdcall;
    function GetEnabledTimeStamp : TDateTime; stdcall;
   protected
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
  end;


implementation 
uses
  System.SysUtils,
  System.DateUtils,
  System.Math,
  dmVoltageConsts,
  AbstractDeviceInterface;

const
C_ID : Word = 202;
C_Ver : Word = 01;

function GetMbModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := Tmbvs20201Module;
end; exports GetMbModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;

procedure Tmbvs20201Module.AfterCreate;
var
  vModuleInfoBlock : TAnalogBlock;
begin
  fPositionsCount := 0;
  fAuto := False;
  fWriteTask := nil;
  fVoltageBlock := nil;
  fEnableBlock := nil;
  fProtectBlock := nil;
  fDestroying := false;
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

  if (fPositionsCount < 1) or (fPositionsCount > High(TDiscreteSize)) then
    exit;

  fVoltageBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 8, false);
  fEnableBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress, TDiscreteSize(fPositionsCount), false);
  fProtectBlock := fModbusDevice.Driver.CreateDiscrete(self, fModbusDevice.Station, BaseAddress, TDiscreteSize(fPositionsCount), True);
end;

procedure Tmbvs20201Module.BeforeDestroy;
begin
  fDestroying := true;
  if Assigned(fWriteTask) then
    fWriteTask.Wait(5000);
  if fAuto then
    SetAuto(false);
end;  

procedure Tmbvs20201Module.CallChallenge;
begin
  if Assigned(fProtectBlock) then
    fProtectBlock.ReadAsyn;
  if Assigned(fEnableBlock) then
    fEnableBlock.ReadAsyn;
  if Assigned(fVoltageBlock) then
    fVoltageBlock.ReadAsyn;
end;

function Tmbvs20201Module.GetEnabled(AIndex: word): Boolean;
begin
  Result := (AIndex < fPositionsCount)
        and assigned(fEnableBlock)
        and (fEnableBlock.Values[AIndex]);
end;

function Tmbvs20201Module.GetAuto: Boolean;
begin
  if (not Assigned(fEnableBlock))
  or (not Assigned(fProtectBlock))
  or (not Assigned(fVoltageBlock)) then
  begin
    result := false;
    exit;
  end;
  result := fVoltageBlock.AutoRead
        or fEnableBlock.AutoRead
        or fProtectBlock.AutoRead;
end;

function Tmbvs20201Module.GetChannelsCount: word;
begin
  Result := fPositionsCount;
end;

function Tmbvs20201Module.GetProtectState(AIndex: word): Boolean;
begin
  Result := (AIndex < fPositionsCount)
        and Assigned(fProtectBlock)
        and (fProtectBlock.Values[AIndex]);
end;

function Tmbvs20201Module.GetProtectTimeStamp: TDateTime;
begin
  if not Assigned(fProtectBlock) then
  begin
    result := IncMinute(now, -10);
    exit;
  end;
  result := fProtectBlock.ValueTimestamp;
end;  

function Tmbvs20201Module.GetEnabledTimeStamp: TDateTime;
begin
  if not Assigned(fEnableBlock) then
  begin
    result := IncMinute(now, -10);
    exit;
  end;
  result := fEnableBlock.ValueTimestamp;
end;

procedure Tmbvs20201Module.GetVoltageValues(ADest: pPositionsVoltages);
var
  vVoltage : record
    case integer of
      0: (f: Single);
      1: (w : array [0..1] of Word);
    end;
begin
  if not Assigned(fEnableBlock) then
  begin
    ADest.TimeStamp := IncMinute(now, -10);
    exit;
  end;
  vVoltage.w[0] := fVoltageBlock.Values[0];
  vVoltage.w[1] := fVoltageBlock.Values[1];
  ADest.VDD := Round((vVoltage.f * 1000000)) / 1000000;
  vVoltage.w[0] := fVoltageBlock.Values[2];
  vVoltage.w[1] := fVoltageBlock.Values[3];
  ADest.VC := Round((vVoltage.f * 1000000)) / 1000000;
  vVoltage.w[0] := fVoltageBlock.Values[4];
  vVoltage.w[1] := fVoltageBlock.Values[5];
  ADest.VAnalog := Round((vVoltage.f * 1000000)) / 1000000;
  vVoltage.w[0] := fVoltageBlock.Values[6];
  vVoltage.w[1] := fVoltageBlock.Values[7];
  ADest.VProg := Round((vVoltage.f * 1000000)) / 1000000;
  ADest.TimeStamp := fVoltageBlock.ValueTimestamp;
end;

procedure Tmbvs20201Module.SetAuto(const Value: Boolean);
begin
  if Value = fAuto  then
    Exit;
  if (not Assigned(fEnableBlock))
  or (not Assigned(fProtectBlock))
  or (not Assigned(fVoltageBlock)) then
    exit;
  if Value then
  begin
    fEnableBlock.RefreshInterval := 200;
    fProtectBlock.RefreshInterval := 200;
    fVoltageBlock.RefreshInterval := 200;
    fProtectBlock.AddCallBacks(self);
    fEnableBlock.AddCallBacks(self);
    fVoltageBlock.AddCallBacks(self);
  end else
  begin
    fProtectBlock.RemoveCallBacks(self);
    fEnableBlock.RemoveCallBacks(self);
    fVoltageBlock.RemoveCallBacks(self);
  end;
  fProtectBlock.AutoRead := Value;
  fEnableBlock.AutoRead := Value;
  fVoltageBlock.AutoRead := Value;
  fAuto := Value;
end;

procedure Tmbvs20201Module.SetEnabled(AIndex: word; Value: boolean);
begin
  if (AIndex >= fPositionsCount)
  or (not Assigned(fEnableBlock)) then
    exit;
  fEnableBlock.Values[AIndex] := Value;
  if not Assigned(fWriteTask) then
  begin
    fWriteTask := TTask.Run(
    procedure
    begin
      Sleep(100);
      if (not fDestroying) and Assigned(fEnableBlock) then
        fEnableBlock.WriteAsyn;
      fWriteTask := nil;
    end);
  end;
end;

procedure Tmbvs20201Module.SetVoltageValues(ASource: pPositionsVoltages);
var
  vVoltage : record
    case integer of
      0: (f: Single);
      1: (w : array [0..1] of Word);
    end;
begin
  if (not Assigned(fVoltageBlock)) then
    exit;
  vVoltage.f := Round((ASource.VDD * 1000000)) / 1000000;
  fVoltageBlock.Values[0]:= vVoltage.w[0];
  fVoltageBlock.Values[1]:= vVoltage.w[1];

  vVoltage.f := Round((ASource.VC * 1000000)) / 1000000;
  fVoltageBlock.Values[2] := vVoltage.w[0];
  fVoltageBlock.Values[3] := vVoltage.w[1];

  vVoltage.f := Round((ASource.VAnalog * 1000000)) / 1000000;
  fVoltageBlock.Values[4] := vVoltage.w[0];
  fVoltageBlock.Values[5] := vVoltage.w[1];

  vVoltage.f := Round((ASource.VProg * 1000000)) / 1000000;
  fVoltageBlock.Values[6] := vVoltage.w[0];
  fVoltageBlock.Values[7] := vVoltage.w[1];
  fVoltageBlock.WriteAsyn;
end;

procedure Tmbvs20201Module.NotifyReadOk(Sender: TObject);
begin
end;

procedure Tmbvs20201Module.NotifyReadFault(Sender: TObject);
begin
end;

procedure Tmbvs20201Module.NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn);
begin
  if (Sender = fVoltageBlock) then
    fEventSubscribers.Execute(Self, C_OnVoltageChanged, nil)
  else if (Sender = fEnableBlock) then
    fEventSubscribers.Execute(Self, C_OnEnabledChanged, nil)
  else if (Sender = fProtectBlock) then
    fEventSubscribers.Execute(Self, C_OnProtectChanged, nil);
end;

procedure Tmbvs20201Module.NotifyWriteFault(Sender: TObject);
begin
end;

procedure Tmbvs20201Module.NotifyWriteOk(Sender: TObject);
begin
  if (sender = fVoltageBlock) then
    fEventSubscribers.Execute(Self, C_OnVoltageSended, nil)
  else if (sender = fEnableBlock) then
    fEventSubscribers.Execute(Self, C_OnEnabledSended, nil);
end;  

procedure Tmbvs20201Module.RemoveTag(Sender: TObject);
begin
end;

end.
