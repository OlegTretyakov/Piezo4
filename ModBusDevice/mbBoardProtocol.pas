unit mbBoardProtocol;

interface

uses
  System.Classes,
  RS232Phisycal,
  AbstractExtention,
  AbstractProtocol,
  ModBusDevice,
  ModBusDeviceInterface,
  ProtocolTypes,
  commtypes,
  SerialPort,
  ModBusSerial,
  AbstractTag;
      
  type
  TModBusBoardProtocol = class(TAbstractProtocol, IRS232Phisycal)
   private
    fPort : Byte;
    fSerialPortDriver: TSerialPortDriver;
    fModBusRTUDriver : TModBusRTUDriver;
    fDevice : TModBusDevice;
    procedure DeviceMessage(const Sender: IModBusDevice; const AStr : string);
    procedure DriverEvent(const Action: TIOAction; APacket : pIOPacket);
    procedure DriverErrorEvent(Result: TProtocolIOResult; APacket : pIOPacket);
    {IRS232Phisycal}
    function IRS232Phisycal.GetPort = IRS232Phisycal_GetPort;
    function IRS232Phisycal.GetConnected = IRS232Phisycal_GetConnected;
    function IRS232Phisycal.Connect = IRS232Phisycal_Connect;
    procedure IRS232Phisycal.Close = IRS232Phisycal_Close;
    function IRS232Phisycal_GetPort : Byte; stdcall;
    function IRS232Phisycal_GetConnected: Boolean; stdcall;
    function IRS232Phisycal_Connect(APort : Byte):boolean; stdcall;
    procedure IRS232Phisycal_Close; stdcall;
   protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    procedure AfterCreate; override;  
    procedure BeforeDestroy; override;
   public
    class function ProtocolName : string; override;
    function CommChannel : string; override;
    function Connected : Boolean; override;
    procedure FillSysInfo(SysInfoStruct : pSysInfoStruct); override;
    procedure SaveDBConfig(const ADest : TStrings); override;
    procedure FillBoardExtentionsClasses(var oClasses :TExtentionsClasses); override;
    procedure FillPositionsListExtentionsClasses(var oClasses :TExtentionsClasses); override;
    procedure Close; override;
  end;


implementation
uses
  WinApi.Windows,
  System.Math,
  System.SysUtils,
  System.IniFiles,
  System.DateUtils,
  dmSysInfoInterface,
  dmChannelsInterface,
  mbvg20402Interface,
  mbProgrammerInterface,
  BoardVoltmeter,
  dmFreqMeasurerInterface,
  FreqMeasureExtention,
  dmPositionControllerInterface,
  PositionController,
  mbvg20401Interface,
  PositionVoltmeter,
  mbpc21001Interface,
  ProgrammerExtention;


function GetBoardProtocolClass : TAbstractProtocolClass; stdcall;
begin
  result := TModBusBoardProtocol;
end; exports GetBoardProtocolClass;

{ Tmbbp30Ext }

procedure TModBusBoardProtocol.AfterCreate;
begin
  fModBusRTUDriver := TModBusRTUDriver.Create(self);
  fSerialPortDriver := TSerialPortDriver.Create(self);
  fSerialPortDriver.Timeout := 1000;
  fModBusRTUDriver.CommunicationPort := fSerialPortDriver;
  fModBusRTUDriver.DelayBetweenCmds := 3;
  fModBusRTUDriver.OnErrorEvent := DriverErrorEvent;
  fDevice := TModBusDevice.Create(self, fModBusRTUDriver, 1, 100);
  fDevice.OnMessage := DeviceMessage;
end;

procedure TModBusBoardProtocol.BeforeDestroy;
begin
  if Assigned(OnProtocolMessage) then
    OnProtocolMessage(self, Format('ModBus board protocol destroying in thread:%d',[GetCurrentThreadID]));
  inherited; 
end;

procedure TModBusBoardProtocol.Close;
begin
  if Assigned(OnProtocolMessage) then
    OnProtocolMessage(self, Format('ModBus board protocol close in thread:%d',[GetCurrentThreadID]));
  fModBusRTUDriver.StopScan;
  fDevice.Modules.Clear;
  fSerialPortDriver.Active := False;
  fModBusRTUDriver.CommunicationPort := nil;
end;

function TModBusBoardProtocol.CommChannel: string;
begin
  Result := fSerialPortDriver.COMPort;
end;

function TModBusBoardProtocol.Connected: Boolean;
begin
  Result := IRS232Phisycal_GetConnected;
end;

function TModBusBoardProtocol.IRS232Phisycal_Connect(APort: Byte): boolean;
var
  vTestHandle : THandle;
begin
  result := False;
  fPort := 0;
  try
    if fSerialPortDriver.Active then
      fSerialPortDriver.Active := false;
    vTestHandle := CreateFile(PWideChar(Format('\\.\COM%d',[APort])),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
    result := vTestHandle <> INVALID_HANDLE_VALUE;
    if not Result then
      Exit;
  finally
    CloseHandle(vTestHandle);
  end;
  try
    fSerialPortDriver.COMPort := Format('COM%d',[APort]);
    fSerialPortDriver.BaudRate := br115200;
    if Assigned(ProtocolPacket) then
      fSerialPortDriver.LogEvent := DriverEvent;
    fSerialPortDriver.Active := true;
    result := fSerialPortDriver.Active;
    if Result then
    begin
      if Assigned(OnProtocolMessage) then
        OnProtocolMessage(self, Format('ModBus board protocol starting in thread:%d',[GetCurrentThreadID]));
      fPort := APort;
      result := fDevice.Start;
    end;
  except
    result := False;
  end;
end;

procedure InternalClassAdd(AClass : TAbstractExtentionClass; var oClasses: TExtentionsClasses);
begin
  SetLength(oClasses, Length(oClasses)+1);
  oClasses[High(oClasses)] := AClass;
end;

procedure TModBusBoardProtocol.FillBoardExtentionsClasses(var oClasses: TExtentionsClasses);
begin  
  inherited;
  if fDevice.Modules.Find(Imbvg20402Module) then
     InternalClassAdd(TBoardVoltmeterExtention, oClasses);
end;

procedure TModBusBoardProtocol.FillPositionsListExtentionsClasses(var oClasses: TExtentionsClasses);
begin
  inherited;
  if fDevice.Modules.Find(IdmFreqStarter)
  and fDevice.Modules.Find(IdmFreqController)
  and fDevice.Modules.Find(IdmFreqResults) then
     InternalClassAdd(TPosListFreqMeasureExtention, oClasses);
  if fDevice.Modules.Find(IdmPositionsController) then
     InternalClassAdd(TPosListPositionControllerExtention, oClasses);  
  if fDevice.Modules.Find(Imbvg20401Module) then
     InternalClassAdd(TPosListPositionVoltmeterExtention, oClasses);  
  if fDevice.Modules.Find(Imbpc21001Module) then
     InternalClassAdd(TPosListProgrammerExtention, oClasses);
end;

procedure TModBusBoardProtocol.FillSysInfo(SysInfoStruct: pSysInfoStruct);
var
  vChannels : IdmChannels;
  vSysInfo : IdmSysInfo;
  vModuleIdx : Byte;
begin
  inherited;
  if fDevice.Modules.Find(IdmChannels, vChannels) then
    SysInfoStruct.MaxPositionsCount := vChannels.Count;
  vChannels := nil;
  if Supports(fDevice, IdmSysInfo, vSysInfo) then
  begin
    SysInfoStruct.ProtocolDestription  := Format('Modbus protocol ver %d',[vSysInfo.PVer]);
    SysInfoStruct.ProtocolVersion  := vSysInfo.PVer;
    SysInfoStruct.ModelCode  := vSysInfo.Model;
    SysInfoStruct.HwVer  := vSysInfo.HwVer;
    SysInfoStruct.FwVer  := vSysInfo.FwVer;
    SysInfoStruct.Serial  := vSysInfo.Serial;
    SysInfoStruct.LoadedModulesCount := fDevice.Modules.Count;
    vModuleIdx := 0;
    while vModuleIdx < SysInfoStruct.LoadedModulesCount  do
    begin
      if vModuleIdx = 0 then
        SysInfoStruct.LoadedModules := Format('Total modules:%d. Module base address:%d, ID:%d, Version:%d;',
        [SysInfoStruct.LoadedModulesCount,
        fDevice.Modules.Item[vModuleIdx].BaseAddress,
        fDevice.Modules.Item[vModuleIdx].ID,
        fDevice.Modules.Item[vModuleIdx].Version])
      else
        SysInfoStruct.LoadedModules := Format('%s Module base address:%d, ID:%d, Version:%d;',
        [SysInfoStruct.LoadedModules,
        fDevice.Modules.Item[vModuleIdx].BaseAddress,
        fDevice.Modules.Item[vModuleIdx].ID,
        fDevice.Modules.Item[vModuleIdx].Version]);
      vChannels := nil;
      if Supports(fDevice.Modules.Item[vModuleIdx], IdmChannels, vChannels)
      and (SysInfoStruct.MaxPositionsCount < vChannels.Count) then
         SysInfoStruct.MaxPositionsCount := vChannels.Count;

      vChannels := nil;
      Inc(vModuleIdx);
    end;
    vSysInfo := nil;
  end;
end;

function TModBusBoardProtocol.IRS232Phisycal_GetConnected: Boolean;
begin
  Result := fDevice.Connected;
end;

function TModBusBoardProtocol.IRS232Phisycal_GetPort: Byte;
begin
  Result := fPort;
end;

procedure TModBusBoardProtocol.IRS232Phisycal_Close;
begin
  self.Close;
end;

function bufferToHex(const ABuffer : array of Byte; AMax : word):String;
var
  i,
  vHigh : integer;
begin
  Result:='';
  vHigh := System.Math.Min(AMax-1, High(ABuffer));
  for i:=0 to vHigh do
    Result:=Result+IntToHex(ABuffer[i],2)+' ';
end;

procedure TModBusBoardProtocol.DeviceMessage(const Sender : IModBusDevice; const AStr: string);
begin
  if Assigned(OnProtocolMessage) then
    OnProtocolMessage(self, AStr);
end;

procedure TModBusBoardProtocol.DriverErrorEvent(Result: TProtocolIOResult; APacket: pIOPacket);
var
  vStr : string;
begin
  if Assigned(OnProtocolError) then
  begin
    vStr := Format('IOResult: %s; WriteIOResult: %s; ReadIOResult: %s; '+
    'Write time:%d ms; WR delay:%d ms; Read time:%d ms; '+
    'ToWrite count:%d; Written count:%d; Write retries:%d; '+
    'Delay between command:%d; ToRead count:%d; '+
    'Received count:%d; Read retries:%d; Write buffer:%s; Read buffer:%s',
          [C_ProtocolIOResultStr[Result],
          C_PortIOResultStr[APacket.WriteIOResult],
          C_PortIOResultStr[APacket.ReadIOResult],
          MilliSecondsBetween(APacket.WriteStart, APacket.WriteFinish),
          MilliSecondsBetween(APacket.WriteFinish, APacket.ReadStart),
          MilliSecondsBetween(APacket.ReadStart, APacket.ReadFinish),
          APacket.ToWriteCount,
          APacket.WrittenCount,
          APacket.WriteRetries,
          APacket.DelayBetweenCommand,
          APacket.ToReadCount,
          APacket.ReceivedCount,
          APacket.ReadRetries,
          bufferToHex(APacket.BufferToWrite, APacket.WrittenCount),
          bufferToHex(APacket.BufferToRead, APacket.ReceivedCount)
          ]);
    OnProtocolError(self, vStr);
  end;
end;

procedure TModBusBoardProtocol.DriverEvent(const Action: TIOAction; APacket: pIOPacket);
var
  vStr : string;
begin
  if Assigned(ProtocolPacket) and (Action = ioaRead) then
  begin
    vStr := Format('Write:%s; Read:%s',
          [
          bufferToHex(APacket.BufferToWrite, APacket.WrittenCount),
          bufferToHex(APacket.BufferToRead, APacket.ReceivedCount)
          ]);
    ProtocolPacket(self, vStr);
  end;
end;

class function TModBusBoardProtocol.ProtocolName: string;
begin
  Result := 'Modbus board protocol';
end;

function TModBusBoardProtocol.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj); 
  if (Result <> S_OK) and Supports(fDevice, IID, Obj) then
    Result := S_OK;
end;

procedure TModBusBoardProtocol.SaveDBConfig(const ADest: TStrings);
var
  vIni: TMemIniFile;
  vSysInfo : IdmSysInfo;
  vPG : ImbProgrammer;
  vModulesCount,
  vModuleIdx : Byte;
begin
  inherited;
  if not Supports(fDevice, IdmSysInfo, vSysInfo) then
    Exit;
  vIni:= TMemIniFile.Create('');
  try
    vModulesCount := Byte(fDevice.Modules.Count);
    vIni.WriteInteger('Sysinfo', 'ModelCode', vSysInfo.Model);
    vIni.WriteInteger('Sysinfo', 'ProtocolVer', vSysInfo.PVer);
    vIni.WriteInteger('Sysinfo', 'HardwareVer', vSysInfo.HwVer);
    vIni.WriteInteger('Sysinfo', 'FirmwareVer', vSysInfo.FwVer);
    vIni.WriteInteger('Sysinfo', 'LoadedModules', vModulesCount);
    vModuleIdx := 0;
    while vModuleIdx < vModulesCount  do
    begin
       vIni.WriteInteger(Format('Module%d',[vModuleIdx]),
              'BaseAddr', fDevice.Modules.Item[vModuleIdx].BaseAddress);
       vIni.WriteInteger(Format('Module%d',[vModuleIdx]),
              'ID', fDevice.Modules.Item[vModuleIdx].ID);
       vIni.WriteInteger(Format('Module%d',[vModuleIdx]),
              'Version', fDevice.Modules.Item[vModuleIdx].Version);

      if Supports(fDevice.Modules.Item[vModuleIdx], ImbProgrammer, vPG) then
      begin
        vIni.WriteInteger('Programmers', vPG.SupportedChip.ToString, 1);
        vPG := nil;
      end;
      Inc(vModuleIdx);
    end;
    vIni.GetStrings(ADest)
  finally
    FreeAndNil(vIni);
    vSysInfo := nil;
  end;
end;

end.
