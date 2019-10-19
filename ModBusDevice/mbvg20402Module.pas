unit mbvg20402Module;

{Измеритель напряжений}

interface
uses
  System.Classes,
  DeviceModule,
  mbvg20402Interface,
  ModBusDeviceInterface,
  AbstractTag,
  AnalogBLock,
  dmBoardVoltmeterInterface,
  dmChallengeControllerInterface;

type

   Tmbvg20402Module = class(TDeviceModule,
                      Imbvg20402Module,
                      IdmChallengeController,
                      IHMITagInterface)
   private
    fAuto : boolean;
    fVoltageBlock: TAnalogBlock;
    {IdmChallengeController}  
    function GetAuto : Boolean;stdcall;
    procedure SetAuto(const Value : Boolean);stdcall;
    procedure CallChallenge;stdcall;
    {Imbvg20402Module}
    procedure GetVoltageValues(ADest : pBoardPowerValues); stdcall;
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
uses
  System.SysUtils,
  System.DateUtils,
  System.Math,
  dmVoltageConsts,
  AbstractDeviceInterface;

const
C_ID : Word = 204;
//C_ID : Word = 04;
C_Ver : Word = 02;

function GetMbModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := Tmbvg20402Module;
end; exports GetMbModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;

procedure Tmbvg20402Module.AfterCreate;
var
vModuleInfoBlock : TAnalogBlock;
vChannelsCount: Word;
begin
  fAuto := False;
  vChannelsCount := 0;
  fVoltageBlock := nil;
  vModuleInfoBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 3, true);
  try
    if vModuleInfoBlock.ReadSyn then
    begin
      fID := vModuleInfoBlock.Values[0];
      fVersion := vModuleInfoBlock.Values[1];
      vChannelsCount := vModuleInfoBlock.Values[2];
    end else
      Exit;
  finally
    FreeAndNil(vModuleInfoBlock);
  end;

  if (vChannelsCount <> 4) then
    exit;
  fVoltageBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress+10, vChannelsCount*4, true);

end;

procedure Tmbvg20402Module.BeforeDestroy;
begin
  if fAuto then
    SetAuto(false);
end; 

procedure Tmbvg20402Module.CallChallenge;
begin
  if Assigned(fVoltageBlock) then
    fVoltageBlock.ReadAsyn;
end;

function Tmbvg20402Module.GetAuto: Boolean;
begin
  Result := Assigned(fVoltageBlock) and fVoltageBlock.AutoRead;
end; 

procedure Tmbvg20402Module.SetAuto(const Value: Boolean);
begin
  if Value = fAuto  then
    Exit;
  if (not Assigned(fVoltageBlock)) then
    exit;
  if Value then
  begin
    fVoltageBlock.RefreshInterval := 300;
    fVoltageBlock.AddCallBacks(self);
  end else
    fVoltageBlock.RemoveCallBacks(self);
  fVoltageBlock.AutoRead := Value;
  fAuto := Value;
end;

procedure Tmbvg20402Module.GetVoltageValues(ADest: pBoardPowerValues);
var
vVoltage : record
  case integer of
    0: (d: Double);
    1: (a : array [0..3] of Word);
  end;
begin
  if (not Assigned(fVoltageBlock))
  or (fVoltageBlock.Size < 16) then
  begin
    ADest.TimeStamp := IncMinute(Now, -10);
    exit;
  end;
  vVoltage.a[0] := fVoltageBlock.Values[0];
  vVoltage.a[1] := fVoltageBlock.Values[1];
  vVoltage.a[2] := fVoltageBlock.Values[2];
  vVoltage.a[3] := fVoltageBlock.Values[3];
  ADest.VCORE := vVoltage.d;

  vVoltage.a[0] := fVoltageBlock.Values[4];
  vVoltage.a[1] := fVoltageBlock.Values[5];
  vVoltage.a[2] := fVoltageBlock.Values[6];
  vVoltage.a[3] := fVoltageBlock.Values[7];
  ADest.VDD := vVoltage.d;

  vVoltage.a[0] := fVoltageBlock.Values[8];
  vVoltage.a[1] := fVoltageBlock.Values[9];
  vVoltage.a[2] := fVoltageBlock.Values[10];
  vVoltage.a[3] := fVoltageBlock.Values[11];
  ADest.VAnalog := vVoltage.d;

  vVoltage.a[0] := fVoltageBlock.Values[12];
  vVoltage.a[1] := fVoltageBlock.Values[13];
  vVoltage.a[2] := fVoltageBlock.Values[14];
  vVoltage.a[3] := fVoltageBlock.Values[15];
  ADest.VProg := vVoltage.d;
  ADest.TimeStamp := fVoltageBlock.ValueTimestamp;
end;

procedure Tmbvg20402Module.NotifyReadFault(Sender: TObject);
begin
end;

procedure Tmbvg20402Module.NotifyReadOk(Sender: TObject);
begin
end;

procedure Tmbvg20402Module.NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn);
begin
  if (AChangedIn <> chInRead) then
    exit;
  if (Sender = fVoltageBlock) then
    fEventSubscribers.Execute(Self, C_OnVoltageChanged, nil);
end;

procedure Tmbvg20402Module.NotifyWriteFault(Sender: TObject);
begin
end;

procedure Tmbvg20402Module.NotifyWriteOk(Sender: TObject);
begin
end;  

procedure Tmbvg20402Module.RemoveTag(Sender: TObject);
begin 
end;


end.
