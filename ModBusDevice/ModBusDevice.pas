unit ModBusDevice;

interface
  uses
  WinApi.Windows, System.Classes, System.SysUtils, Vodopad.ObjectList,
  Vodopad.EventList,
  AbstractDeviceInterface, Vodopad.Timer,
  ModBusDriver, DiscreteBlock, AnalogBLock, DeviceModule, ModBusDeviceInterface, EventBusInterface;

type
  IModuleInstaller = interface(IDeviceModules)
    ['{FA35B3C3-9F41-4399-AD9E-4236AD14E7E9}']
    procedure Install(const AModuleClass : TDeviceModuleClass; BaseAddres : Word);stdcall;
  end;
  TDeviceModules = class(TComponent, IDeviceModules, IModuleInstaller)
   strict private
    fModules : TExObjectList;
   private
    function GetItem(AIdx: Word): TDeviceModule;
   protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    {IModuleInstaller}
    procedure Install(const AModuleClass : TDeviceModuleClass; BaseAddres : Word);stdcall;
   public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Item[AIdx : Word]:TDeviceModule read GetItem;
    {IDeviceModules}
    function QueryItem(AIdx : Word; IID : TGUID; var Obj):Boolean; stdcall;
    function FindBaseAddr(Addr : Word; out oIndex : Byte; out Obj : TObject):Boolean; stdcall;
    function Find(ModuleIID : TGUID):Boolean; overload; stdcall;
    function Find(IID : TGUID; var Obj):boolean; overload; stdcall;
    function Find(var AIdx : Word; IID : TGUID; var Obj):boolean; overload; stdcall;
    function Find(var AIdx : Word; IID : TGUID):boolean; overload; stdcall;
    function Count : Word; stdcall;
    procedure Delete(IID: TGUID); stdcall;
    procedure Clear; stdcall;
  end;
  TModBusDevice = class;
  TModBusDevice = class(TComponent, IModBusDevice, IEventBus)
   strict private
    fPingTimer : TvdTimer;
    fConnected : Boolean;
    fDriver : TModBusDriver;
    fIdVerBlock : TAnalogBlock;
    fSysInfoModule : TDeviceModule;
    fModules : TDeviceModules;
    fStation, fBaseAdress: Word;
    fOnMessage : TModBusDeviceMessage;
    fEventSubscribers : TCustomObjEventList;
    procedure OnDeviceInit(Sender : TObject);
    procedure OnDevicePing(Sender : TObject);
    procedure SetConnected(Value : Boolean);
   private
    function GetDriver : TModBusDriver; stdcall;
    function GetStation : Word;stdcall;
    function CreateSysInfoInstance : boolean;
    {IModBusDevice}
    function GetConnected: Boolean; stdcall;
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: TCustomObjEvent); stdcall;
    function GetOnMessage: TModBusDeviceMessage; stdcall;
    procedure SetOnMessage(const Value: TModBusDeviceMessage); stdcall;
   protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
   public
    constructor Create(AOwner : TComponent; const ADriver : TModBusDriver; AStation, ABaseAdress : Word); reintroduce;
    destructor Destroy; override;
    function Start : boolean;
    property Driver : TModBusDriver read GetDriver;
    property Station : Word read GetStation;
    property Connected : Boolean read GetConnected;
    property SysInfoModule : TDeviceModule read fSysInfoModule;
    property Modules : TDeviceModules read fModules;
    property OnMessage : TModBusDeviceMessage read GetOnMessage write SetOnMessage;
  end;

implementation
uses
System.DateUtils,
System.Math,
RS232Phisycal,
Vodopad.Math,
System.IniFiles,
DeviceModuleInterface,
dmSerialSetterInterface,
dmSysInfoInterface,
mbsiControlInterface,
mbPluginManager;
type
TModulesManager = class(TMBPM)
  procedure FindPluginFiles; override;
end;

TSysInfoManager = class(TMBPM)
  procedure FindPluginFiles; override;
end;

var
V_InstanceCount : Word=0;
V_ModulesManager : TModulesManager=nil;
V_SysInfoManager : TSysInfoManager=nil;



{ TDeviceModules }

procedure TDeviceModules.Clear;
begin
  fModules.Clear;
end;

constructor TDeviceModules.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fModules := TExObjectList.Create;
end;

procedure TDeviceModules.Delete(IID: TGUID);
var vIdx : word;
begin
  vIdx := 0;
  while Find(vIdx, IID) do
    fModules.Delete(vIdx);
end;

destructor TDeviceModules.Destroy;
begin
  Clear;
  FreeAndNil(fModules);
  inherited Destroy;
end;

function TDeviceModules.Count: Word;
begin
  result := fModules.Count;
end;

function TDeviceModules.QueryItem(AIdx: Word; IID: TGUID;
  var Obj): Boolean;
begin
  Result := Supports(fModules.Items[AIdx], IID, Obj);
end;

function TDeviceModules.Find(ModuleIID: TGUID): Boolean;
var vIdx : word;
begin
  vIdx := 0;
  Result := Find(vIdx, ModuleIID);
end;

function TDeviceModules.Find(var AIdx: Word; IID: TGUID; var Obj): boolean;
begin
  result := Find(AIdx, IID);
  result := result and QueryItem(AIdx, IID, Obj);
end;

function TDeviceModules.Find(var AIdx: Word; IID: TGUID): boolean;
var
vIdx : word;
vTmp : IInterface;
begin
  vIdx := AIdx;
  while (vIdx < fModules.Count) do
  begin
    vTmp := nil;
    if Supports(fModules.Items[vIdx], IID, vTmp) then
      Break;
    inc(vIdx);
  end;
  vTmp := nil;
  result := (vIdx < fModules.Count);
  if result then
    AIdx := vIdx;
end;

function TDeviceModules.FindBaseAddr(Addr: Word; out oIndex : Byte; out Obj : TObject): Boolean;
begin
  oIndex := 0;
  while (oIndex < fModules.Count)
  and (TDeviceModule(fModules[oIndex]).BaseAddress <> Addr) do
    inc(oIndex);
  Result := oIndex < fModules.Count;
  if Result then
    Obj := fModules[oIndex];
end;

function TDeviceModules.GetItem(AIdx: Word): TDeviceModule;
begin
  Result := TDeviceModule(fModules[AIdx]);
end;

function TDeviceModules.Find(IID: TGUID; var Obj): boolean;
var vIdx : Word;
begin
  vIdx := 0;
  result := Find(vIdx, IID, Obj);
end;

procedure TDeviceModules.Install(const AModuleClass: TDeviceModuleClass; BaseAddres : Word);
var
vDevice : IModBusDevice;
vModule : IDeviceModule;
begin
  if Supports(Self.Owner, IModBusDevice, vDevice) then
  try
    fModules.Add(AModuleClass.Create(self, vDevice, BaseAddres));
    if Supports(fModules.Last, IDeviceModule, vModule)
    and Assigned(Owner)
    and (Owner is TModBusDevice)
    and Assigned(TModBusDevice(Owner).OnMessage) then
      TModBusDevice(Owner).OnMessage(TModBusDevice(Owner), Format('BaseAddress:%d ID:%d Version:%d installed in thread:%d',
      [vModule.BaseAddress, vModule.ID, vModule.Version, GetCurrentThreadID]));
  finally
    vModule := nil;
    vDevice := nil;
  end;
end;

function TDeviceModules.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> S_OK) and Find(IID, Obj) then
    Result := S_OK;
end;


{ TModBusDevice }

constructor TModBusDevice.Create(AOwner: TComponent;
                                 const ADriver: TModBusDriver;
                                 AStation, ABaseAdress: Word);


begin
  Inc(V_InstanceCount);
  inherited Create(AOwner);
  fSysInfoModule := nil;
  fPingTimer := TvdTimer.Create(self);
  fDriver := ADriver;
  fStation := AStation;
  fBaseAdress := ABaseAdress;
  fConnected := False;
  fEventSubscribers := TCustomObjEventList.Create;
  fModules := TDeviceModules.Create(self);
  fIdVerBlock := fDriver.CreateAnalog(self, fStation, fBaseAdress, 2, true);

  if not Assigned(V_ModulesManager) then
  begin
    V_ModulesManager := TModulesManager.Create;
    V_ModulesManager.FindPluginFiles;
  end;
  if not Assigned(V_SysInfoManager) then
  begin
    V_SysInfoManager := TSysInfoManager.Create;
    V_SysInfoManager.FindPluginFiles;
  end;
end;  

destructor TModBusDevice.Destroy;
begin
  fPingTimer.Enabled := false;
  SetConnected(false);
  fModules.Clear;
  fEventSubscribers.Clear;
  FreeAndNil(fEventSubscribers);
  inherited Destroy;
  Dec(V_InstanceCount);
  if V_InstanceCount=0 then
  begin
    if Assigned(V_ModulesManager) then
    begin
      FreeAndNil(V_ModulesManager);
      V_ModulesManager := nil;
    end;
    if Assigned(V_SysInfoManager) then
    begin
      FreeAndNil(V_SysInfoManager);
      V_SysInfoManager := nil;
    end;
  end;
end;

procedure TModBusDevice.OnDeviceInit(Sender: TObject);
var
vmbSi : ImbsiControl;
begin
  if Supports(fSysInfoModule, ImbsiControl, vmbSi)
  and vmbSi.Init then
  begin
    fPingTimer.Interval := 1000;
    fPingTimer.OnTimer := OnDevicePing;
    SetConnected(true);
  end  else
  begin
    fPingTimer.Enabled := False;
    fPingTimer.OnTimer := nil;
    fEventSubscribers.Execute(Self, C_OnConnectFail, nil);
  end;
  vmbSi := nil;
end;

procedure TModBusDevice.OnDevicePing(Sender: TObject);
var
vmbSi : ImbsiControl;
begin
  if Supports(fSysInfoModule, ImbsiControl, vmbSi) then
  begin
    if (MilliSecondsBetween(Now, vmbSi.LastSuccChallenge) < 4000) then
    begin
      if vmbSi.SystemChanged then
      begin
        {fPingTimer.OnTimer := OnDeviceInit;
        fPingTimer.Interval := 10; }
        fPingTimer.Enabled := false;
        fEventSubscribers.Execute(Self, C_OnSystemChanged, nil);
      end else if not fConnected then
      begin
        fConnected := true;
        fEventSubscribers.Execute(Self, C_OnConnectionRestore, nil);
      end;
    end else
    begin
      if fConnected then
      begin
        fConnected := False;
        fPingTimer.Interval := 3000;
        fEventSubscribers.Execute(Self, C_OnConnectionLost, nil);
      end;
    end;
    vmbSi := nil;
  end else
  begin
    fPingTimer.Enabled := False;
    fModules.Clear;
     if Assigned(fSysInfoModule) then
       FreeAndNil(fSysInfoModule);
     SetConnected(False);
  end;
end;

function TModBusDevice.Start : boolean;
begin
  fPingTimer.Interval := 10;
  Result := fIdVerBlock.ReadSyn and CreateSysInfoInstance;
  fPingTimer.Enabled := result;
  if not Result then
    Exit;
  fPingTimer.OnTimer := OnDeviceInit;
end;

function TModBusDevice.CreateSysInfoInstance : boolean;
var
vModuleClass : TDeviceModuleClass;
vModuleId, vModuleVer : Word;
begin
  fModules.Clear;
  if Assigned(fSysInfoModule) then
    FreeAndNil(fSysInfoModule);
  vModuleId := fIdVerBlock.Values[0];
  vModuleVer := fIdVerBlock.Values[1];
  Result := V_SysInfoManager.FindModuleClass(vModuleId, vModuleVer, vModuleClass);
  if Result then
    fSysInfoModule := vModuleClass.Create(Self, self, fBaseAdress);
end;

procedure TModBusDevice.SetConnected(Value: Boolean);
var
vSysInfo : IdmSysInfo;
vss : ISerialSetter;
begin
  if fConnected = Value then
    Exit;
  fConnected := Value;
  if fConnected then
  begin 
    if Supports(fSysInfoModule, IdmSysInfo, vSysInfo)
    and Supports(Owner.Owner, ISerialSetter, vss) then
      vss.SetSerial(vSysInfo.Serial);
    vss := nil;
    vSysInfo := nil;
    fEventSubscribers.Execute(Self, C_OnConnected, nil);
  end else
    fEventSubscribers.Execute(Self, C_OnDisconnected, nil);
end;

procedure TModBusDevice.SetOnMessage(const Value: TModBusDeviceMessage);
begin
  fOnMessage := Value;
end;

function TModBusDevice.GetConnected: Boolean;
begin
  Result := fConnected;
end;

function TModBusDevice.GetDriver: TModBusDriver;
begin
  Result := fDriver;
end;

function TModBusDevice.GetOnMessage: TModBusDeviceMessage;
begin
  result := fOnMessage;
end;

function TModBusDevice.GetStation: Word;
begin
  Result := fStation;
end;

function TModBusDevice.QueryInterface(const IID: TGUID; out Obj): HResult;
begin           
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> S_OK) and Supports(fModules, IID, Obj) then
    Result := S_OK;
  if (Result <> S_OK) and Supports(fSysInfoModule, IID, Obj) then
    Result := S_OK;
  if (Result <> S_OK) and Supports(V_ModulesManager, IID, Obj) then
    Result := S_OK;
end; 

procedure TModBusDevice.EventMethodAdd(const AMethod : TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TModBusDevice.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

{ TModulesManager }

procedure TModulesManager.FindPluginFiles;
var
vEnabled : Boolean;
searchResult : TSearchRec;
vPluginInfo : TMBPluginInfo;
GetClassFunc : function : TDeviceModuleClass; stdcall;
GetIDFunc,
GetVerFunc : function : Word; stdcall;
vPluginsPath, vModBusPluginsExt,
vIniName  : string;
vIniFile : TIniFile;
vUpdateIni : Boolean;
begin
  vModBusPluginsExt := 'bpl';
  vIniName := ChangeFileExt(GetModuleName(HInstance),'.ini');
  vPluginsPath := ExtractFilePath(vIniName);
  vIniFile := TIniFile.Create(vIniName);
  try
    if vIniFile.ValueExists('Plugins Options', 'Path') and
    DirectoryExists(vIniFile.ReadString('Plugins Options', 'Path', vPluginsPath) ) then
        vPluginsPath := vIniFile.ReadString('Plugins Options', 'Path', vPluginsPath);
    if vIniFile.ValueExists('Plugins Options', 'PluginsExtention') then
      vModBusPluginsExt := vIniFile.ReadString('Plugins Options', 'PluginsExtention', vModBusPluginsExt);

    vUpdateIni := False;
    if FindFirst(vPluginsPath+'mb*.'+vModBusPluginsExt, faAnyFile, searchResult) = 0 then
    begin
      repeat
        try
          vPluginInfo.iModule := SafeLoadLibrary(vPluginsPath + searchResult.Name,
          SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
          if (vPluginInfo.iModule > 0) then
          begin
            @GetIDFunc := GetProcAddress(vPluginInfo.iModule, 'GetIDFunc');
            @GetVerFunc := GetProcAddress(vPluginInfo.iModule, 'GetVerFunc');
            @GetClassFunc := GetProcAddress(vPluginInfo.iModule, 'GetMbModuleClassType');
            if Assigned(GetClassFunc)
            and Assigned(GetIDFunc)
            and Assigned(GetVerFunc) then
            begin
              vPluginInfo.ModuleClass := GetClassFunc;
              vPluginInfo.ID := GetIDFunc;
              vPluginInfo.Version := GetVerFunc;
              vEnabled := vIniFile.ReadBool('Plugins Options',
                 Format('ID%dVer%d',[vPluginInfo.ID, vPluginInfo.Version]), true);
              if not vIniFile.ValueExists('Plugins Options',
                 Format('ID%dVer%d',[vPluginInfo.ID, vPluginInfo.Version])) then
              begin
                vIniFile.WriteBool('Plugins Options',
                 Format('ID%dVer%d',[vPluginInfo.ID, vPluginInfo.Version]), true);
                vUpdateIni := True;
              end;
              if vEnabled and (IndexByIDVer(vPluginInfo.ID, vPluginInfo.Version) = -1) then
                Add(vPluginInfo)
              else
                FreeLibrary(vPluginInfo.iModule);
            end else
              FreeLibrary(vPluginInfo.iModule);
          end;
        except
          if (vPluginInfo.iModule > 0) then
            FreeLibrary(vPluginInfo.iModule);
          Continue;
        end;
      until FindNext(searchResult) <> 0;
      FindClose(searchResult);
    end;
    if vUpdateIni then
      vIniFile.UpdateFile;
  finally
    FreeAndNil(vIniFile);
  end;
end;

{ TSysInfoManager }

procedure TSysInfoManager.FindPluginFiles; 
var
searchResult : TSearchRec;
vPluginInfo : TMBPluginInfo;
GetClassFunc : function : TDeviceModuleClass; stdcall;
GetIDFunc,
GetVerFunc : function : Word; stdcall;
vCurrentPath : string;
begin
  vCurrentPath := ExtractFilePath(GetModuleName(HInstance));
  if FindFirst(vCurrentPath+'*.bpl', faAnyFile, searchResult) = 0 then
  begin
    repeat
      try
        vPluginInfo.iModule := SafeLoadLibrary(vCurrentPath + searchResult.Name,
          SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
        if (vPluginInfo.iModule > 0) then
        begin
          @GetIDFunc := GetProcAddress(vPluginInfo.iModule, 'GetIDFunc');
          @GetVerFunc := GetProcAddress(vPluginInfo.iModule, 'GetVerFunc');
          @GetClassFunc := GetProcAddress(vPluginInfo.iModule, 'GetSiModuleClassType');
          if Assigned(GetClassFunc)
          and Assigned(GetIDFunc)
          and Assigned(GetVerFunc) then
          begin
            vPluginInfo.ModuleClass := GetClassFunc;
            vPluginInfo.ID := GetIDFunc;
            vPluginInfo.Version := GetVerFunc;
            if (IndexByIDVer(vPluginInfo.ID, vPluginInfo.Version) = -1) then
              Add(vPluginInfo)
            else
              FreeLibrary(vPluginInfo.iModule);
          end else
            FreeLibrary(vPluginInfo.iModule);
        end;
      except
        if (vPluginInfo.iModule > 0) then
          FreeLibrary(vPluginInfo.iModule);
        Continue;
      end;
    until FindNext(searchResult) <> 0;
    FindClose(searchResult);
  end;

end;

initialization

finalization
if Assigned(V_ModulesManager) then
  FreeAndNil(V_ModulesManager);
if Assigned(V_SysInfoManager) then
  FreeAndNil(V_SysInfoManager);

  
end.
