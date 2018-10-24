unit ChipAbstractInterface;

interface
  type
  TChipBytes = array of Byte;
  IChipAbstract = interface(IInterface)
    ['{DC7FB1F6-88C1-459A-BC6F-ECCDF28F3165}']
    function GetInstance : TObject; stdcall;
    function GetBitValue(AIndex : byte):smallint; stdcall;
    procedure SetBitValue(AIndex : byte; AValue: smallInt); stdcall;    
    procedure Assign(const ASource : IChipAbstract; const ABitsIndex : TChipBytes=nil); stdcall;
    function IsIdenty(const ASource : IChipAbstract; const ABitsIndex : TChipBytes=nil): boolean; stdcall;
    procedure Clear; stdcall;
    function BitMinIndex : byte; stdcall;
    function BitMaxIndex : byte; stdcall;
    function ChipGUID : TGUID; stdcall;
    function ChipName: String; stdcall;
    function BitMinValue(AIndex : byte):smallint; stdcall;
    function BitMeanValue(AIndex : byte):smallint; stdcall;
    function BitMaxValue(AIndex : byte):smallint; stdcall;
    function BitDefValue(AIndex : byte):smallint; stdcall;
    procedure ReadControlledBitsIndex(const ASection : string; ADefault : Boolean; out oBitsIndex : TChipBytes); stdcall;
    function ReadIntConstant(const ASection, AIdent : string; var oConst : Integer):Boolean;stdcall;
    function ReadStrConstant(const ASection, AIdent : string; var oConst : String):Boolean;stdcall;
    property BitValue[AIndex : byte]:smallint read GetBitValue write SetBitValue;
  end;
implementation

end.
