unit ChipManagerInterface;

interface
  uses System.Classes, ChipAbstract;
  type
  IChipPluginManager = interface(IInterface)
   ['{457A2F94-C3EB-441B-A0B2-5DF9FDC852A0}']
    function Count : Byte;stdcall;
    function GetClass(const Index: Byte): TChipClass; stdcall;
    function GetModuleName(const Index: Integer): String; stdcall;
    function IndexOf(const AGUID : TGUID) : integer;overload;stdcall;
    function IndexOf(AClass : TChipClass) : integer;overload;stdcall;
    property ItemClass[const Index: Byte]: TChipClass read GetClass; default;
    property ModuleName[const Index: Integer]: String read GetModuleName;
    procedure AsStrings(AList : TStrings); stdcall;
    function ChipPligunsCount(AChipIndex : Integer):byte; overload; stdcall;
    function ChipPligunsCount(const AGUID : TGUID):byte; overload; stdcall;
    function ChipPligunsCount(AClass : TChipClass):byte; overload; stdcall;
    function FindChipPligunFunction(AChipIndex : Integer; var AIdx : Byte; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(AChipIndex : Integer; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(const AGUID : TGUID; var AIdx : Byte; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(const AGUID : TGUID; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(AClass : TChipClass; var AIdx : Byte; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(AClass : TChipClass; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
  end;

implementation

end.
