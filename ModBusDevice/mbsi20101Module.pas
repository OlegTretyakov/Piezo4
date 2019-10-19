unit mbsi20101Module;

interface
  uses
    AbstractTag,
    DeviceModule,
    AnalogBLock,
    ModBusDeviceInterface,
    mbsiControlInterface,
    dmSysInfoInterface;

  type
  TSysInfoModule = class(TDeviceModule,
                      ImbsiControl,
                      IdmSysInfo,
                      IHMITagInterface)
   private
    fModel,
    fProtocolVer,
    fHwVer,
    fFwVer,
    fSerial,
    fModulesCount : Word;
    fSystemChanged : boolean;
    fLastSuccChallenge : TDateTime;
    fSysInfoBlock : TAnalogBlock;
    {IBlockNotifyInterface}
    procedure NotifyReadOk(Sender:TObject); stdcall;
    procedure NotifyReadFault(Sender:TObject);stdcall;
    procedure NotifyWriteOk(Sender:TObject); stdcall;
    procedure NotifyWriteFault(Sender:TObject); stdcall;
    procedure NotifyTagChange(Sender:TObject; AChangedIn : TTagChangedIn); stdcall;
    procedure RemoveTag(Sender:TObject);stdcall;
    {ImbsiControl}
    function Init: Boolean; stdcall;
    function GetLastSuccChallenge : TDateTime;stdcall;
    function GetSystemChanged:Boolean; stdcall;
    {IdmSysInfo}
    function GetModel : Word;stdcall;
    function GetPVer : Word;stdcall;
    function GetHwVer : Word;stdcall;
    function GetFwVer : Word;stdcall;
    function GetSerial : Word;stdcall;
   protected
    procedure AfterCreate;override;
  end;

implementation
uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Math,
  mbPluginManagerInterface,
  AbstractDeviceInterface,
  dmChallengeControllerInterface;

const
C_ID : Word = 201;
//C_ID : Word = 01;
C_Ver : Word = 01;

function GetSiModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := TSysInfoModule;
end; exports GetSiModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;
{ TSysInfoModule }

procedure TSysInfoModule.AfterCreate;
begin
  fSysInfoBlock := nil;
  fSystemChanged := false;
  fSysInfoBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 8, true);
end;

type
IModuleInstaller = interface(IDeviceModules)
  ['{FA35B3C3-9F41-4399-AD9E-4236AD14E7E9}']
  procedure Install(const AModuleClass : TDeviceModuleClass; BaseAddres : Word);stdcall;
end;

function TSysInfoModule.Init: Boolean;
var
  vInstaller : IModuleInstaller;
  vPluginManager : IMBPM;
  vModuleBaseAddrBlock,
  vModuleIdVerBlock  : TAnalogBlock;
  vModuleClass : TDeviceModuleClass;
  vSystemModulesIdx,
  vIdVerBlockMemAddr,
  vModuleIDValue, vModuleVerValue : Word;
  vChallengeCtrl : IdmChallengeController;
  vIniFile : TIniFile;
  vUpdateIni : Boolean;
  vCmdIdx : Byte;
  vCmdConditions : TStringList;
begin
  fLastSuccChallenge := Now;
  fSystemChanged := false;
  result := Assigned(fSysInfoBlock)
  and Supports(fModbusDevice, IModuleInstaller, vInstaller)
  and Supports(fModbusDevice, IMBPM, vPluginManager);
  if not result then
  begin
    vInstaller := nil;
    vPluginManager := nil;
    Exit;
  end;
  vInstaller.Clear;
  Result := False;
  vCmdConditions := TStringList.Create;
  try
    if not fSysInfoBlock.ReadSyn then
    begin
      if Assigned(fModbusDevice.OnMessage) then
        fModbusDevice.OnMessage(fModbusDevice, 'SysInfo block read error');
      Exit;
    end;
    fId := fSysInfoBlock.Values[0];
    fVersion := fSysInfoBlock.Values[1];
    fModel := fSysInfoBlock.Values[2];
    fProtocolVer := fSysInfoBlock.Values[3];
    fHwVer := fSysInfoBlock.Values[4];
    fFwVer := fSysInfoBlock.Values[5];
    fSerial := fSysInfoBlock.Values[6];
    fModulesCount := fSysInfoBlock.Values[7];
    result := (fId = C_ID)
              and (fVersion = C_Ver)
              and (fModulesCount > 0);
    if not result then
      Exit;
    Result := False;

    vUpdateIni := false;
    vIniFile := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance), '.ini'));
    vModuleBaseAddrBlock:= fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress+8, fModulesCount, true);
    try
      if not vModuleBaseAddrBlock.ReadSyn then
          Exit;

      vModuleIdVerBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress+8, 2, true);
      try
        vSystemModulesIdx := 0;
        while vSystemModulesIdx < fModulesCount do
        begin
          vIdVerBlockMemAddr := vModuleBaseAddrBlock.Values[vSystemModulesIdx];
          vModuleIdVerBlock.MemAddress := vIdVerBlockMemAddr;
          if vModuleIdVerBlock.ReadSyn then
          begin
            vModuleIDValue:= vModuleIdVerBlock.Values[0];
            if (not vIniFile.ValueExists(Format('%d',[vIdVerBlockMemAddr]), 'ID'))
            or (vIniFile.ReadInteger(Format('%d',[vIdVerBlockMemAddr]), 'ID', vModuleIDValue) <> vModuleIDValue) then
            begin
              vIniFile.WriteInteger(Format('%d',[vIdVerBlockMemAddr]), 'ID', vModuleIDValue);
              vUpdateIni := True;
            end;
            vModuleVerValue := vModuleIdVerBlock.Values[1];
            if (not vIniFile.ValueExists(Format('%d',[vIdVerBlockMemAddr]), 'Version'))
            or (vIniFile.ReadInteger(Format('%d',[vIdVerBlockMemAddr]), 'Version', vModuleVerValue) <> vModuleVerValue) then
            begin
              vIniFile.WriteInteger(Format('%d',[vIdVerBlockMemAddr]), 'Version', vModuleVerValue);
              vUpdateIni := True;
            end;
            if vPluginManager.FindModuleClass(vModuleIDValue, vModuleVerValue, vModuleClass) then
            begin
              if vIniFile.ReadBool(Format('%d',[vIdVerBlockMemAddr]), 'Enabled', true) then
              begin
                if (not vIniFile.ValueExists(Format('%d',[vIdVerBlockMemAddr]), 'Enabled')) then
                begin
                  vIniFile.WriteBool(Format('%d',[vIdVerBlockMemAddr]), 'Enabled', true);
                  vUpdateIni := True;
                end;
                vModuleClass.CMDStartConditions(vCmdConditions);
                if vCmdConditions.Count < 1 then
                begin
                  vInstaller.Install(vModuleClass, vIdVerBlockMemAddr);
                end else
                begin
                  if Assigned(fModbusDevice.OnMessage) then
                  fModbusDevice.OnMessage(fModbusDevice,
                        Format('SysInfo reader: module ID:%d ver:%d neded start conditions "%s" in command line',
                        [vModuleIDValue, vModuleVerValue, vCmdConditions.Text]));
                  for vCmdIdx := 0 to vCmdConditions.Count - 1 do
                  begin
                    if FindCmdLineSwitch(vCmdConditions[vCmdIdx]) then
                    begin
                      vInstaller.Install(vModuleClass, vIdVerBlockMemAddr);
                      break;
                    end;
                  end;
                end;
              end else
              begin
                if Assigned(fModbusDevice.OnMessage) then
                  fModbusDevice.OnMessage(fModbusDevice,
                        Format('SysInfo reader: module ID:%d ver:%d disabled by ini-file:%s',
                        [vModuleIDValue, vModuleVerValue, vIniFile.FileName]));
              end;
            end else
            begin
              if Assigned(fModbusDevice.OnMessage) then
                  fModbusDevice.OnMessage(fModbusDevice,
                        Format('SysInfo reader: No plugin file for module ID:%d ver:%d ',
                        [vModuleIDValue, vModuleVerValue]));
              if (not vIniFile.ValueExists(Format('%d',[vIdVerBlockMemAddr]), 'Enabled')) then
              begin
                vIniFile.WriteBool(Format('%d',[vIdVerBlockMemAddr]), 'Enabled', false);
                vUpdateIni := True;
              end;
            end;
          end else
          begin
            if Assigned(fModbusDevice.OnMessage) then
              fModbusDevice.OnMessage(fModbusDevice,
                    Format('SysInfo reader: ID and version block by address:%d read error',
                    [vIdVerBlockMemAddr]));
          end;
          Inc(vSystemModulesIdx);
        end;
      finally
        FreeAndNil(vModuleIdVerBlock);
      end;
      if vUpdateIni then
        vIniFile.UpdateFile;
    finally
      FreeAndNil(vModuleBaseAddrBlock);
      FreeAndNil(vIniFile);
    end;
    Randomize;
    vSystemModulesIdx := 0;
    while vSystemModulesIdx < vInstaller.Count do
    begin
      if vInstaller.QueryItem(vSystemModulesIdx, IdmChallengeController, vChallengeCtrl) then
      begin
        vChallengeCtrl.Auto := True;
        vChallengeCtrl := nil;
      end;
      Inc(vSystemModulesIdx);
    end;
    Result := vInstaller.Count > 0;
    if Result then
    begin
      fSysInfoBlock.RefreshInterval := TRefreshTime(RandomRange(700, 1200));
      fSysInfoBlock.AutoRead := True;
      fSysInfoBlock.AddCallBacks(self);
    end;
  finally
    FreeAndNil(vCmdConditions);
    vInstaller := nil;
    vPluginManager := nil;
  end;
end; 

procedure TSysInfoModule.NotifyReadFault(Sender: TObject);
begin

end;

procedure TSysInfoModule.NotifyReadOk(Sender: TObject);
begin
  fLastSuccChallenge := Now;
  if not fSystemChanged then
  begin
    fSystemChanged :=
       (fId <> fSysInfoBlock.Values[0])
    or (fVersion <> fSysInfoBlock.Values[1])
    or (fModel <> fSysInfoBlock.Values[2])
    or (fProtocolVer <> fSysInfoBlock.Values[3])
    or (fHwVer <> fSysInfoBlock.Values[4])
    or (fFwVer <> fSysInfoBlock.Values[5])
    or (fSerial <> fSysInfoBlock.Values[6])
    or (fModulesCount <> fSysInfoBlock.Values[7]);
  end;
end;

procedure TSysInfoModule.NotifyTagChange(Sender: TObject; AChangedIn : TTagChangedIn);
begin
end;

procedure TSysInfoModule.NotifyWriteFault(Sender: TObject);
begin
end;

procedure TSysInfoModule.NotifyWriteOk(Sender: TObject);
begin
end;

procedure TSysInfoModule.RemoveTag(Sender: TObject);
begin
end;

function TSysInfoModule.GetFwVer: Word;
begin
  Result := fFwVer;
end;

function TSysInfoModule.GetHwVer: Word;
begin
  Result := fHwVer;
end;

function TSysInfoModule.GetLastSuccChallenge: TDateTime;
begin
  Result := fLastSuccChallenge;
end;

function TSysInfoModule.GetModel: Word;
begin
  Result := fModel;
end;

function TSysInfoModule.GetPVer: Word;
begin
  Result := fProtocolVer;
end;

function TSysInfoModule.GetSerial: Word;
begin
  Result := fSerial;
end;

function TSysInfoModule.GetSystemChanged: Boolean;
begin
  Result := fSystemChanged;
end;

end.
