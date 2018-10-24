unit mbpc21001Interface;

interface
uses DeviceModuleInterface;
  type
  Imbpc21001Module = interface(IDeviceModule)
    ['{237D03A1-7373-4ECD-90F6-99A571526377}']  
    function GetActiveProgrammer : Word; stdcall;
    procedure SetActiveProgrammer(ABaseAddr : Word); stdcall;
    property ActiveProgrammerBaseAddr:Word read GetActiveProgrammer write SetActiveProgrammer;
  end;

implementation

end.
