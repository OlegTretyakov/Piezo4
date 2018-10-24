unit mbsiControlInterface;

interface
   type
    ImbsiControl = interface(IInterface)
      ['{DF61AE59-41B3-4EA2-983E-884053A2A6B6}']
      function Init : Boolean; stdcall;
      function GetLastSuccChallenge : TDateTime;stdcall;
      function GetSystemChanged : Boolean; stdcall;
      property LastSuccChallenge : TDateTime read GetLastSuccChallenge;
      property SystemChanged : Boolean read GetSystemChanged;
    end;
implementation

end.
