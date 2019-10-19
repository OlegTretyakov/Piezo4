unit ModBusDeviceInterface;

interface
uses
  System.Classes,
  ModBusDriver;

  type
  IModBusDevice = interface;
  TModBusDeviceMessage = procedure (const Sender : IModBusDevice; const AStr : string) of object;
  TCustomObjEvent = procedure (Sender : TObject; Event : TGUID; Params : Pointer) of object;

  IModBusDevice = interface(IInterface)
    ['{1040779E-39A8-492D-9FA3-B654C13E866A}']
    function GetOnMessage: TModBusDeviceMessage; stdcall;
    procedure SetOnMessage(const Value: TModBusDeviceMessage); stdcall;
    function GetConnected: Boolean; stdcall;
    property Connected : Boolean read GetConnected;
    function GetDriver : TModBusDriver; stdcall;
    function GetStation : Word;stdcall;
    property Driver : TModBusDriver read GetDriver;
    property Station : Word read GetStation;
    property OnMessage : TModBusDeviceMessage read GetOnMessage write SetOnMessage;
  end;


implementation

end.
