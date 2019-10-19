unit dmSysInfoInterface;

interface
   type
   IdmSysInfo = interface(IInterface)
     ['{C68824AA-062F-4C63-99A3-0AD2D6767B07}']
    function GetModel : Word; stdcall;
    function GetPVer : Word; stdcall;
    function GetHwVer : Word; stdcall;
    function GetFwVer : Word; stdcall;
    function GetSerial : Word; stdcall;
    property Model: Word read GetModel;
    property PVer : Word read GetPVer;
    property HwVer : Word read GetHwVer;
    property FwVer : Word read GetFwVer;
    property Serial : Word read GetSerial;
   end;


implementation

end.
