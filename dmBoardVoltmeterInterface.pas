unit dmBoardVoltmeterInterface;

interface
  type
  pBoardPowerValues = ^TBoardPowerValues;
  TBoardPowerValues = record
    VDD,
    VCORE,
    VAnalog,
    VProg : Double;
    TimeStamp : TDateTime;
  end;
  IBoardVoltMeter = interface(Iinterface)
   ['{CBCB34C2-22A6-42AC-9343-1EFAD5421AC6}']
    procedure GetVoltageValues(ADest : pBoardPowerValues); stdcall;
  end;
implementation

end.
