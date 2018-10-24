unit mbvg20401Module;

{Измеритель напряжений}

interface
uses
  System.Classes,
  DeviceModule, mbvg20401Interface,
  ModBusDeviceInterface, AbstractTag, AnalogBLock,
  dmChannelsInterface, dmChallengeControllerInterface,
  System.Generics.Collections;
type

  TVoltageValue = record
    Value : Double;
    TimeStamp : TDateTime;
  end;

  Tmbvg20401Module = class(TDeviceModule, Imbvg20401Module,
                      IdmChannels,
                      IdmChallengeController,
                      IHMITagInterface)
   private
    fAuto : Boolean;
    fVoltageBlocks : TList<TAnalogBlock>;
    fPositionsCount : Word;
    fValues : array of TVoltageValue;
    {IdmChannels}
    function GetChannelsCount : word; stdcall;
    {IdmChallengeController} 
    function GetAuto : Boolean;stdcall;
    procedure SetAuto(const Value : Boolean);stdcall;
    procedure CallChallenge;stdcall;
    {Imbvg20401Module}
    function GetVoltage(AIndex : Word):Double; stdcall;
    function GetTimeStamp(AIndex : Word):TDateTime; stdcall;
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
  end;


implementation   
uses System.SysUtils, System.Math,
AbstractDeviceInterface, dmVoltageConsts, System.DateUtils;

const
C_ID : Word = 204;
//C_ID : Word = 04;
C_Ver : Word = 01;

function GetMbModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := Tmbvg20401Module;
end; exports GetMbModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;



procedure Tmbvg20401Module.AfterCreate;
var
vModuleInfoBlock : TAnalogBlock;
vPositionIdx, vMemOffSet, vWordsTotalSize, vElementIdx: Word;
begin
  fAuto := False;
  fPositionsCount := 0;
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

  if fPositionsCount < 1 then
    Exit;
  SetLength(fValues, fPositionsCount);
  fVoltageBlocks := TList<TAnalogBlock>.Create;
  vMemOffSet := BaseAddress +100;
  vWordsTotalSize := fPositionsCount*4;
  vPositionIdx := 0;
  vElementIdx := 0;
  while vPositionIdx < fPositionsCount do
  begin
    if (fVoltageBlocks.Count = 0)
    or (vElementIdx = 120) then
    begin
      if vWordsTotalSize > 120 then
      begin
        vElementIdx := 0;
        fVoltageBlocks.Add(fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, vMemOffSet, 120, true));
        dec(vWordsTotalSize, 120);
      end else
      begin
        vElementIdx := 0;
        fVoltageBlocks.Add(fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, vMemOffSet, vWordsTotalSize, true));
      end;
    end;
    inc(vMemOffSet, 4);
    Inc(vElementIdx, 4);
    Inc(vPositionIdx);
  end;
end;

procedure Tmbvg20401Module.BeforeDestroy;
begin
  if fAuto then
    SetAuto(false);
  if Assigned(fVoltageBlocks) then
  begin
    fVoltageBlocks.Clear;
    FreeAndNil(fVoltageBlocks);
  end;
  SetLength(fValues, 0);
end; 

procedure Tmbvg20401Module.CallChallenge;
var
vIdx : Byte;
begin
  vIdx := 0;
  while vIdx < fVoltageBlocks.Count do
  begin
    fVoltageBlocks[vIdx].ReadAsyn;
    Inc(vIdx);
  end;
end;

function Tmbvg20401Module.GetTimeStamp(AIndex: Word): TDateTime;
begin
  Result := IncMinute(now, -10);
  if (AIndex <= High(fValues)) then
    Result := fValues[AIndex].TimeStamp;
end;

function Tmbvg20401Module.GetAuto: Boolean;
var
vIdx : Byte;
begin
  Result := False;
  vIdx := 0;
  while vIdx < fVoltageBlocks.Count do
  begin
    if vIdx = 0 then
      result := fVoltageBlocks[vIdx].AutoRead
    else
      result := result or fVoltageBlocks[vIdx].AutoRead;
    Inc(vIdx);
  end;
end;

function Tmbvg20401Module.GetChannelsCount: word;
begin
  Result := fPositionsCount;
end;

function Tmbvg20401Module.GetVoltage(AIndex: Word): Double;
begin
  Result := 0;
  if (AIndex <= High(fValues)) then
    Result := fValues[AIndex].Value;
end;  

procedure Tmbvg20401Module.RemoveTag(Sender: TObject);
begin
end;

procedure Tmbvg20401Module.NotifyReadFault(Sender: TObject);
begin
end;

procedure Tmbvg20401Module.NotifyReadOk(Sender: TObject);
var
vBlockIdx, vElemIdx,  vArrIdx, vArrHi, vChangedCount : Word;
vVoltage : record
  case integer of
    0: (d: Double);
    1: (a : array [0..3] of Word);
  end;
begin
  vBlockIdx := 0;
  while vBlockIdx < fVoltageBlocks.Count do
  begin
    if (TObject(fVoltageBlocks[vBlockIdx]) = Sender) then
      Break;
    inc(vBlockIdx);
  end;
  if vBlockIdx >= fVoltageBlocks.Count then
    Exit;

  vChangedCount := 0;
  vArrIdx := vBlockIdx*30;
  vArrHi := High(fValues);
  vElemIdx := 0;
  while vElemIdx+3 < fVoltageBlocks[vBlockIdx].Size do
  begin
    vVoltage.a[0] := fVoltageBlocks[vBlockIdx].Values[vElemIdx];
    vVoltage.a[1] := fVoltageBlocks[vBlockIdx].Values[vElemIdx+1];
    vVoltage.a[2] := fVoltageBlocks[vBlockIdx].Values[vElemIdx+2];
    vVoltage.a[3] := fVoltageBlocks[vBlockIdx].Values[vElemIdx+3];
    if (vArrIdx <= vArrHi) then
    begin
      fValues[vArrIdx].TimeStamp := Now;
      if (not SameValue(fValues[vArrIdx].Value, vVoltage.d, 0.0005)) then
        Inc(vChangedCount);
      fValues[vArrIdx].Value := vVoltage.d;
    end;
    Inc(vArrIdx);
    Inc(vElemIdx, 4);
  end;
  if vChangedCount > 0 then
    fEventSubscribers.Execute(Self, C_OnVoltageChanged, nil);
end;

procedure Tmbvg20401Module.NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn);
begin
end;

procedure Tmbvg20401Module.NotifyWriteFault(Sender: TObject);
begin
end;

procedure Tmbvg20401Module.NotifyWriteOk(Sender: TObject);
begin
end;

procedure Tmbvg20401Module.SetAuto(const Value: Boolean);
var vIdx : Byte;
begin
  if Value = fAuto  then
    Exit;

  if Value and (fVoltageBlocks.Count < 1) then
    Exit;

  vIdx := 0;
  while vIdx < fVoltageBlocks.Count do
  begin
    if Value then
    begin
      fVoltageBlocks[vIdx].RefreshInterval := 450;
      fVoltageBlocks[vIdx].AddCallBacks(self);
    end else
      fVoltageBlocks[vIdx].RemoveCallBacks(self);
    fVoltageBlocks[vIdx].AutoRead := Value;
    Inc(vIdx);
  end;
  if (vIdx > 0) then
    fAuto := Value;
end;


end.
