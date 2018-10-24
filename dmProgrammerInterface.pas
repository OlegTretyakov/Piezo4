unit dmProgrammerInterface;

interface
  uses
  System.Classes, ChipAbstractInterface;
  type
  IdmPositionProgrammer = interface(IInterface)
  ['{03D33165-30C1-4985-AF1B-A5C0DB735492}']
    function GetRegisters : boolean; overload; stdcall;
    function GetRegisters(const ADest : IChipAbstract) : boolean; overload;  stdcall;
    function SetRegisters : boolean;overload; stdcall;
    function SetRegisters(const ASource : IChipAbstract) : boolean;overload; stdcall;
  end;
  TProgrammerError =(peNoErrors, {No Errors, All programmers was operated correct}
                  peAnyError,{unhandled error}
                  peException, {Error raised by exception }
                  peNoSelectedProgrammer, {Active programmer not selected. Do execute select before starting}
                  peNoModuleFinded, {No finded plugin module whith selected ID on programming starting}
                  peErrorOnStart, {all prepare stage is complete, error on start}
                  peNoActivePositions, {No active positons for any programmer}
                  peOperationTimeOut); {Programmer started but finished signal not responded}
  IdmProgrammer = interface(IInterface)
   ['{86C57CEE-F7A9-4694-AAFB-494B29B8C54E}']
    function ActiveProgrammerSelected: Boolean; stdcall;
    function ProgrammerExists(const AProgrammer : TGUID):Boolean; stdcall;
    procedure FillSupportedOperations(const AProgrammer : TGUID; AOperations : TStrings);stdcall;
    function GetActiveProgrammer : TGUID;stdcall;
    procedure SetActiveProgrammer(const AProgrammer : TGUID);stdcall;
    procedure StartOperationAsyn(AOperation : Cardinal; ACopyFromPositions : Boolean);stdcall;
    function StartOperation(AOperation : Cardinal; ACopyFromPositions : Boolean):TProgrammerError;stdcall;
    function GetLastError : TProgrammerError; stdcall;
    function GetRegisters(ABoardPosition : Byte; const ADest : IChipAbstract) : boolean; stdcall;
    function SetRegisters(ABoardPosition : Byte; const ASource : IChipAbstract) : boolean; stdcall;
    property ActiveProgrammer:TGUID  read GetActiveProgrammer write SetActiveProgrammer;
    property LastError : TProgrammerError read GetLastError;
  end;
  IServiceMode = interface(IInterface)
    ['{2985FFB1-AB80-4804-A5C3-36483A0016FA}']
    function GetActiveProgrammer : Word; stdcall;
    procedure SetActiveProgrammer(ABaseAddr : Word); stdcall;
    property ActiveProgrammerBaseAddr:Word read GetActiveProgrammer write SetActiveProgrammer;
  end;
const
C_programmerErrors : array [TProgrammerError] of string = ('No Errors',
                                                    'Unhandled error',
                                                    'Error raised by exception',
                                                    'Active programmer not selected',
                                                    'No finded plugin module whith selected base address on programming starting',
                                                    'All prepare stage is complete, error on start',
                                                    'No active positons for any programmer',
                                                    'Operation timeout');

C_ProgrammerStartError : TGUID = '{4D676A1F-3E52-49DB-B36F-C15FFC141F0D}';
C_ProgrammerStarted : TGUID = '{D1E210B1-63DC-49A8-8CB1-A1FC8FBDB376}';
C_OnProgrammerOperationSucc : TGUID = '{C9234118-3374-48FB-86B2-3099278AA757}';
C_OnProgrammerOperationTimeOut : TGUID = '{04D830FE-A295-4DB8-A1C4-C40302D54CF4}';
implementation

end.
