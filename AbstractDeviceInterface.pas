unit AbstractDeviceInterface;

interface

type


  IDeviceModules = interface(IInterface)
    ['{9142303F-6472-4F4A-8884-5CEEB7673BD6}']
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


implementation


end.
