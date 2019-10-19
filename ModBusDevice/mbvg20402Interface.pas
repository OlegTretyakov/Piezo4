unit mbvg20402Interface;

interface
  uses
    dmBoardVoltmeterInterface;
  type

   Imbvg20402Module = interface(IInterface)
     ['{AD1D67F1-F615-4D7D-B733-59F26E2A1714}']
    procedure GetVoltageValues(ADest : pBoardPowerValues); stdcall;
   end;
implementation



end.
