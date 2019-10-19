unit dmPositionVoltmeterInterface;

interface
  type

  TPositionVoltages = record
    VDD,
    VC,
    Analog : Double;
    VDDTimeStamp,
    VCTimeStamp,
    AnalogTimeStamp : TDateTime;
  end;

  pPositionVoltages = ^TPositionVoltages;
  IPositionVoltages = interface(IInterface)
    ['{48790F4A-BF8C-4F21-9C0B-D2A41C65F964}']
    procedure GetPositionVoltage(ADest : pPositionVoltages); stdcall;
  end;

  IPositionsVoltages = interface(IInterface)
    ['{0F6A53DF-63D3-4568-8BA6-A846F1E2727A}'] 
    procedure GetPositionVoltage(AIndex : Word; ADest : pPositionVoltages); stdcall;
  end;

implementation

end.
