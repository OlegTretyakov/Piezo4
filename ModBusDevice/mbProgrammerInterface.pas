unit mbProgrammerInterface;

interface
uses System.Classes, ChipAbstractInterface, DeviceModuleInterface;
  type
  ImbProgrammer = interface(IDeviceModule)
    ['{FADD8103-BF3F-45CB-BAE8-5E6A23676392}']
    function ChipSupported(GUID : TGUID):boolean; stdcall;
    function SupportedChip :TGUID; stdcall;
    procedure FillSupportedOperations(AOperations : TStrings);stdcall;
    function GetEnabled(AIndex : Byte):Boolean; stdcall;
    procedure SetEnabled(AIndex : Byte; AValue :Boolean); stdcall;
    property Enabled[AIndex : Byte]:Boolean read GetEnabled write SetEnabled;
    function Ready:Boolean;stdcall;
    function StartOperation(AOperation : Cardinal) :Boolean;stdcall;
    function GetRegisters(ABoardPosition : byte; const ADest : IChipAbstract) : boolean; stdcall;
    function SetRegisters(ABoardPosition : byte; const ASource : IChipAbstract) : boolean; stdcall;
  end;
const
C_RegistersReadDone : TGUID = '{7B03796E-30BA-4F57-9748-E5CD2629EB6F}';
C_RegistersWriteDone : TGUID = '{C14DAD9A-165D-4641-8C68-DFDBA2F0BD30}';
C_ProgrammerOperationFinished : TGUID = '{B9EBAFCB-EEFE-420F-BB74-ADB8B90496BC}';
C_ProgrammerOperationTimeOut : TGUID = '{ED05B2B5-A26C-44EF-A5C6-2F9151616975}';
implementation

end.
