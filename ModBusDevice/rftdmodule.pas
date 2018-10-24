unit rftdmodule;

{Терморегулятор}

interface
uses FastMM4,
    Classes, DeviceModule,
    rftdmoduleinterface, ExtCtrls;
type
  
  TWatcher = record
   private
    fWatchTime : LongWord; //В минутах
    fStartingTime : TDateTime;
    fTemprRangeValue : double;
    procedure SetWatchTime(AMinutes : LongWord);
    procedure SetRange(Range : double);
   public
    property RangeValue : double read fTemprRangeValue write SetRange;
    procedure StartExposure;
    property RangeTime : LongWord read fWatchTime write SetWatchTime;
    property StartingTime : TDateTime read fStartingTime;
    function TemprIsInRange(aDifferent : double) : boolean;
    function MinutesElapsed: LongWord;
    function isTimeExpired : boolean;
  end;
  TRftdModule = class(TDeviceModule, IRftdModule)
   private
    fCheckTimer : TTimer;
    fRegulatorStateBlock,
    fRegulatorStateElement,
    fCurrTemprBlock,
    fCurrTemprElement,
    fTemprParamsBlock,
    fTargetTemprElement,
    fTemprSpeedElement: Pointer;
    fChargeTimeStart : TDateTime;
    fChallengeTimeOutInterval,
    fMinutesTargetTimeOut : LongWord;
    fStabMode : TStabMode;
    fTemprWatcher : TWatcher;
    procedure ValueChanged(Sender: TObject);
    procedure RegulatorStateChanged(Sender: TObject);
    procedure CheckState;
    procedure LoadParamsFromFile;
    function GetTargetCurrent: double; stdcall;
    function GetTargetTarget: double; stdcall;
    procedure SetTemprTarget(const Value: double); stdcall;
    function GetTemprSpeed : double; stdcall;
    procedure SetTemprSpeed(const Value: double); stdcall;
    function GetRegulatorActive: boolean;  stdcall;
    procedure SetRegulatorActive(const Value: boolean); stdcall;
    function GetStabMode: TStabMode; stdcall;
    function GetStabilizedTime : TDateTime; stdcall;
   protected
    procedure AfterCreate;override;
   public
    property RegulatorActive : boolean read GetRegulatorActive write SetRegulatorActive;
    property TemprCurrent: double read GetTargetCurrent;
    property TemprTarget : double read GetTargetTarget write SetTemprTarget;
    property TemprSpeed : double read GetTemprSpeed write SetTemprSpeed;
    property MinutesTargetTimeOut : LongWord read fMinutesTargetTimeOut;
    property StabMode : TStabMode read GetStabMode;
    property ChargeTimeStart : TDateTime read fChargeTimeStart;
    property TemprWatcher : TWatcher read fTemprWatcher write fTemprWatcher;
  end;

function GetModuleClassType : TDeviceModuleClass; stdcall;
function GetIDFunc : Word; stdcall;
function GetVerFunc : Word; stdcall;

implementation
uses SysUtils, Math, vdMath,
IniFiles, DateUtils,
ModBusDeviceInterface;



function GetModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := TRftdModule;
end; exports GetModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;

{ TRftdModule }

procedure TRftdModule.AfterCreate;
var
vDevice : IModBusDevice;
var
vPLCBlockSettings : pPLCBlockSettings;
vPLCBlockElementSettings : pPLCBlockElementSettings;
begin
  if not Supports(self.Owner.Owner, IModBusDevice, vDevice) then
    raise Exception.Create('RftdModule.Create. Не найден интерфейс IModBusDevice');
  New(vPLCBlockSettings);
  New(vPLCBlockElementSettings);
  try
    fID := C_ID;
    fVersion := C_Ver;
    fCheckTimer := vDevice.CreateTimer(Self);
    fCheckTimer.OnTimer := ValueChanged;
    
    vPLCBlockSettings.OnChanged := nil;
    vPLCBlockElementSettings.OnChanged := nil;
    
    vPLCBlockSettings.AutoRead := True;
    vPLCBlockSettings.AutoWrite := True;
    vPLCBlockSettings.TagType := pttDefault;
    vPLCBlockSettings.Size := 1;
    vPLCBlockSettings.MemAddress := BaseAddress;
    vPLCBlockSettings.MemReadFunction :=  1;
    vPLCBlockSettings.MemWriteFunction := 5;
    vPLCBlockSettings.UpdateInterval := 100;
    vPLCBlockSettings.OnChanged := nil;
    fRegulatorStateBlock := vDevice.CreatePLCBlock(self, vPLCBlockSettings);

    //vDevice.PLCBlockAction(fRegulatorStateBlock, ARead);
    
    vPLCBlockElementSettings.PLCBlock :=  fRegulatorStateBlock;
    vPLCBlockElementSettings.Index := 0;
    vPLCBlockElementSettings.OnChanged := RegulatorStateChanged;
    fRegulatorStateElement := vDevice.CreatePLCElement(self, vPLCBlockElementSettings);

    vPLCBlockSettings.AutoWrite := false;
    vPLCBlockSettings.TagType := pttFloat;
    vPLCBlockSettings.MemWriteFunction := 0;
    vPLCBlockSettings.MemReadFunction := 4;
    vPLCBlockElementSettings.OnChanged := ValueChanged;

    fCurrTemprBlock := vDevice.CreatePLCBlock(self, vPLCBlockSettings);

    vPLCBlockElementSettings.PLCBlock := fCurrTemprBlock;
    fCurrTemprElement := vDevice.CreatePLCElement(self, vPLCBlockElementSettings);

    vPLCBlockSettings.AutoWrite := True;
    vPLCBlockSettings.Size := 2;
    vPLCBlockSettings.MemReadFunction :=  3;
    vPLCBlockSettings.MemWriteFunction := 16;

    fTemprParamsBlock := vDevice.CreatePLCBlock(self, vPLCBlockSettings);


    vPLCBlockElementSettings.PLCBlock := fTemprParamsBlock;
    fTargetTemprElement := vDevice.CreatePLCElement(self, vPLCBlockElementSettings);
    vPLCBlockElementSettings.Index := 1;
    fTemprSpeedElement := vDevice.CreatePLCElement(self, vPLCBlockElementSettings);
    fChallengeTimeOutInterval := 10000;
    fMinutesTargetTimeOut := 180;
    fTemprWatcher.SetRange(0.5);
    fTemprWatcher.RangeTime := 10;
    fStabMode := mChallenge;
    LoadParamsFromFile;
  finally
    Dispose(vPLCBlockElementSettings);
    Dispose(vPLCBlockSettings);
    vDevice := nil;
  end;  
end;

function TRftdModule.GetTargetCurrent: double;
begin
  result := ElementAsDouble[fCurrTemprElement];
end;

function TRftdModule.GetRegulatorActive: boolean;
begin
  result := ElementAsBoolean[fRegulatorStateElement];
end;

function TRftdModule.GetStabilizedTime: TDateTime;
begin
  Result := Self.fTemprWatcher.fStartingTime;
end;

function TRftdModule.GetStabMode: TStabMode;
begin
  Result := fStabMode;
end;

function TRftdModule.GetTargetTarget: double;
begin
  result := ElementAsDouble[fTargetTemprElement];
end;

function TRftdModule.GetTemprSpeed: double;
begin
  result := ElementAsDouble[fTemprSpeedElement];
end;

procedure TRftdModule.LoadParamsFromFile;
var
vIniName  : string;
f : TIniFile;  
vDS : Char;
vCreate : boolean;
begin
  vIniName := ExtractFilePath(ParamStr(0))+ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'); 
  vCreate := not FileExists(vIniName);  
  vDS := DecimalSeparator;
  DecimalSeparator := '.';
  f := TIniFile.Create(vIniName);
  try
    if not vCreate then
    begin
      if f.ValueExists('Process params', 'DeltaTempr') then
      fTemprWatcher.SetRange(
        f.ReadFloat('Process params', 'DeltaTempr', 0.03))
      else
      begin
        fTemprWatcher.SetRange(0.03); 
        f.WriteFloat('Process params', 'DeltaTempr', 0.03);
        f.UpdateFile;
      end;

      if f.ValueExists('Process params', 'TemprCheckTimeOut') then
        fMinutesTargetTimeOut := f.ReadInteger('Process params', 'TemprCheckTimeOut', 180)
      else
      begin
        f.WriteInteger('Process params', 'TemprCheckTimeOut', 180);
        f.UpdateFile;
      end;

      if f.ValueExists('Process params', 'DeltaTime') then
        fTemprWatcher.RangeTime :=
      f.ReadInteger('Process params', 'DeltaTime', 10)
      else
      begin
        fTemprWatcher.RangeTime := 10;
        f.WriteInteger('Process params', 'DeltaTime', 10);
        f.UpdateFile;
      end;
    end else
    begin
      f.WriteFloat('Process params', 'DeltaTempr', 0.03);
      f.WriteInteger('Process params', 'TemprCheckTimeOut', 180);
      f.WriteInteger('Process params', 'DeltaTime', 10);  
      f.UpdateFile;
    end;
  finally
    FreeAndNil(f);   
    DecimalSeparator := vDS;
  end;
end;

procedure TRftdModule.RegulatorStateChanged(Sender: TObject);
begin
  if ElementAsBoolean[fRegulatorStateElement] then
  begin
    if fStabMode = mChallenge then
    begin
      fStabMode := mCharge;
      fChargeTimeStart := now;
      FireEvent(self.Owner.Owner, C_OnTemprCharge, nil);
    end;
  end else
    fStabMode := mChallenge;
  FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
end;

procedure TRftdModule.SetRegulatorActive(const Value: boolean);
begin
  if ElementAsBoolean[fRegulatorStateElement] = Value then
    Exit;
  ElementAsBoolean[fRegulatorStateElement] := Value; 
end;

procedure TRftdModule.SetTemprTarget(const Value: double);
var
vValue : Double;
begin
  vValue := ElementAsDouble[fTargetTemprElement];
  if Equals(vValue, Value, fTemprWatcher.fTemprRangeValue) then
    exit;
  ElementAsDouble[fTargetTemprElement] := Value;
  if ElementAsBoolean[fRegulatorStateElement] then
  begin
    fStabMode := mCharge;
    fChargeTimeStart := now;
    FireEvent(self.Owner.Owner, C_OnTemprCharge, nil);
  end;
end;

procedure TRftdModule.SetTemprSpeed(const Value: double);
begin
  ElementAsDouble[fTemprSpeedElement] := Value;
end;

procedure TRftdModule.ValueChanged(Sender: TObject);
begin
  if Pointer(Sender) = fCurrTemprElement then
  begin
    FireEvent(self.Owner.Owner, C_OnTempr, nil);
  end{ else if Sender = fTargetTemprElement then
  begin
    CheckState;
  end else if Sender = fTemprSpeedElement then
  begin
    CheckState;
  end};
  CheckState;
end;

procedure TRftdModule.CheckState;
var CurrDiff : double;
begin
    CurrDiff := abs(TemprCurrent - TemprTarget);
  case fStabMode of
    mChallenge:;
    mCharge:
    begin
      if (fTemprWatcher.TemprIsInRange(CurrDiff)) then
      fStabMode := mInRange else
      begin
        if (MinutesBetween(Now, fChargeTimeStart)  > fMinutesTargetTimeOut) then
        begin
          fStabMode := mChargeTimeOut;
          FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
          {RtpLogger.Log(sllEvent, Format('Порт COM%d Таймаут набора %d минут',
          [self.Port, MinutesBetween(Now, fChargeTimeStart)] )); }
        end;
      end;
    end;
    mInRange:
    begin
      fTemprWatcher.StartExposure;
      fStabMode := mInRangeTime;
      FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
      {RtpLogger.Log(sllEvent, Format('Порт COM%d старт наблюдения %d минут',
          [self.Port, fTemprWatcher.RangeTime] ));  }
    end;
    mInRangeTime:
    begin
      if (fTemprWatcher.TemprIsInRange(CurrDiff)) then
      begin
        if (fTemprWatcher.isTimeExpired) then
        begin
          fStabMode := mWatch;
          FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
          FireEvent(self.Owner.Owner, C_OnTemprInTarget, nil);
        end;
      end else
      begin
        fStabMode := mCharge;
        FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
        FireEvent(self.Owner.Owner, C_OnTemprCharge, nil);
      end;
    end;
    mWatch:
    begin
      if not (fTemprWatcher.TemprIsInRange(CurrDiff)) then
      begin
        fStabMode := mOutTarget;  
        FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
        FireEvent(self.Owner.Owner, C_OnTemprOutOfTarget, nil);
      end;
    end;
    mOutTarget:
    begin
      if (fTemprWatcher.TemprIsInRange(CurrDiff)) then
      begin
        fTemprWatcher.StartExposure;
        fStabMode := mOutTargetInRangeTime;
        FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
        {RtpLogger.Log(sllEvent, Format('Порт COM%d старт наблюдения %d минут',
          [self.Port, fTemprWatcher.RangeTime] )); }
      end;
    end;
    mOutTargetInRangeTime:
    begin
      if (fTemprWatcher.TemprIsInRange(CurrDiff)) then
      begin
        if (fTemprWatcher.isTimeExpired) then
        begin
          fStabMode := mWatch; 
          FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
          FireEvent(self.Owner.Owner, C_OnTemprTargetRet, nil);
        end;
      end else
      begin
        fStabMode := mOutTarget; 
        FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
        FireEvent(self.Owner.Owner, C_OnTemprOutOfTarget, nil);
      end;
    end;
    mChargeTimeOut:
    begin   
      FireEvent(self.Owner.Owner, C_OnChargeTimeOut, nil);
      fChargeTimeStart := now;
      fStabMode := mCharge;  
      FireEvent(self.Owner.Owner, C_OnRegulatorStateChange, nil);
    end;
  end;
end;


{ TWatcher }

function TWatcher.TemprIsInRange(aDifferent: double): boolean;
begin
  result := (aDifferent < fTemprRangeValue);
end;

function TWatcher.MinutesElapsed : LongWord;
begin
  result := minutesBetween(now, fStartingTime);
end;

function TWatcher.isTimeExpired: boolean;
begin
  result := (MinutesElapsed >= fWatchTime);
end;

procedure TWatcher.SetRange(Range: double);
begin
  fTemprRangeValue := EnsureRange(Range, 0, 120);
end;

procedure TWatcher.SetWatchTime(AMinutes: LongWord);
begin
  fWatchTime := EnsureRange(AMinutes, 1, 3600);
end;

procedure TWatcher.StartExposure;
begin
  fStartingTime := now;
end;

end.
