unit DeviceModuleInterface;

interface
type  
  IDeviceModule = interface(IInterface)
   ['{10A48D71-EDA4-4423-9C74-6E38946499DE}']
    function GetID : Word; stdcall;
    function GetVersion : Word; stdcall;  
    function GetBaseAddress: Word; stdcall;
    property ID : Word read GetID;
    property Version: Word read GetVersion; 
    property BaseAddress: Word read GetBaseAddress;
  end;

implementation

end.
