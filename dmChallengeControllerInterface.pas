unit dmChallengeControllerInterface;

interface
  type
  IdmChallengeController = interface(IInterface)
    ['{38EB6238-C312-4086-890C-CE40340FBCA1}']
    function GetAuto : Boolean; stdcall;
    procedure SetAuto(const Value : Boolean); stdcall;
    procedure CallChallenge; stdcall;
    property Auto: Boolean read GetAuto write SetAuto;
  end;
implementation

end.
