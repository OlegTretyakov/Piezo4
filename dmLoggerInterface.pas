unit dmLoggerInterface;

interface

uses
  LoggerInterface;

type
  IdmLogger = interface(IInterface)
    ['{F5889C71-945F-4041-805D-06D3D136AEF1}']
    procedure Log(ALevel : TLogInfo; const AStr : string); stdcall;
    function LevelEnabled(ALevel : TLogInfo):boolean; stdcall;
  end;


implementation

end.
