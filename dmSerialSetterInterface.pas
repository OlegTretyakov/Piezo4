unit dmSerialSetterInterface;

interface
  type
  ISerialSetter = interface(IInterface)
  ['{EAC9AE24-6C80-4A51-A954-BBE617A97C0B}']
   procedure SetSerial(ASerial : Word);stdcall; 
  end;

implementation

end.
