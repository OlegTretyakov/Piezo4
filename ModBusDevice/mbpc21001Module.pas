unit mbpc21001Module;

interface
uses
  System.Classes, DeviceModule, mbpc21001Interface,
  ModBusDeviceInterface, AbstractTag, AnalogBLock;

type

  Tmbpc21001Module = class(TDeviceModule, Imbpc21001Module)
   private
    fActiveIDBlock,
    fActiveIDBlockGetter : TAnalogBlock;
    {Imbpc21001Module}
    function GetActiveProgrammer : Word; stdcall;
    procedure SetActiveProgrammer(ABaseAddr : Word); stdcall;
   protected
    procedure AfterCreate;override;
  end;


implementation   
uses System.SysUtils;

const
C_ID : Word = 210;
C_Ver : Word = 01;

function GetMbModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := Tmbpc21001Module;
end; exports GetMbModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;



procedure Tmbpc21001Module.AfterCreate;
var
vModuleInfoBlock : TAnalogBlock;
begin
  vModuleInfoBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 2, true);
  try
    if vModuleInfoBlock.ReadSyn then
    begin
      fID := vModuleInfoBlock.Values[0];
      fVersion := vModuleInfoBlock.Values[1];
    end else
      Exit;
  finally
    FreeAndNil(vModuleInfoBlock);
  end;
  fActiveIDBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 1, false);
  fActiveIDBlockGetter := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress+10, 1, true);

end;

function Tmbpc21001Module.GetActiveProgrammer: Word;
begin
  if Assigned(fActiveIDBlockGetter) and fActiveIDBlockGetter.ReadSyn then
    Result := fActiveIDBlockGetter.Values[0]
  else
    Result := 0;
end;

procedure Tmbpc21001Module.SetActiveProgrammer(ABaseAddr: Word);
begin
  if Assigned(fActiveIDBlock) then
  begin
    fActiveIDBlock.Values[0] := ABaseAddr;
    fActiveIDBlock.WriteSyn;
  end;
end;

end.
