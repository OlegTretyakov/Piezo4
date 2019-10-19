unit EventBusInterface;

interface

  type
  TCustomObjEvent = procedure (Sender : TObject; Event : TGUID; Params : Pointer) of object;

  IEventBus = interface(IInterface)
    ['{7E26165B-97FB-4467-AEAF-37D230F083DB}']
    procedure Add(const AMethod : TCustomObjEvent); stdcall;
    procedure Remove(const AMethod: TCustomObjEvent); stdcall;
  end;


implementation

end.
