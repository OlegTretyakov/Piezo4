unit PositionListInterface;

interface
  type
  TAdditionalExtentions = array of  pointer;
  IPositionsList = interface(IInterface)
    ['{15C41D7F-A3C3-483F-A7B6-62FFBE639B4F}'] 
    function GetItem(Index : Word): TObject; stdcall;
    procedure SetMaxCount(const Value: Word); stdcall;
    function GetMaxCount: Word; stdcall;
    function GetCount: Word; stdcall;
    function GetActiveCount: Word; stdcall;
    function IndexOf(ABoardPos : byte) : integer; stdcall;
    function Exists(ABoardPos : byte) : boolean; stdcall;
    property ActiveCount : Word read GetActiveCount;
    property Count: Word read GetCount;
    property MaxCount : Word read GetMaxCount write SetMaxCount; 
    function AdditionalExtentionClassCount(AClass : Pointer):Word; stdcall;
    function NewItem(BoardPos : Byte; AdditionalExtentions : TAdditionalExtentions=nil):TObject; stdcall;
    function TryBeginUpdate : boolean; stdcall;
    procedure BeginUpdate; stdcall;
    procedure EndUpdate; stdcall; 
    function QueryItem(AIndex : Word; const IID : TGUID; out oPosition):Boolean;stdcall;
    procedure Delete(AIndex: Word); stdcall;
    procedure Clear; stdcall;
    property Items[index : Word] : TObject read GetItem; default;
  end;
  IPositionsListExt = interface(IPositionsList)
    ['{6DC83B4A-D6F0-4510-846F-72C07A16C382}']   
    procedure IncActive; stdcall;
    procedure DecActive; stdcall;
  end;  
const
C_ListBeginUpdate : TGUID = '{A924FDE1-68FD-4F6E-8C80-63578B975DAF}';
C_ListEndUpdate : TGUID = '{B8F97CB8-D5EF-4DFF-8B7D-AF5363FD4BAE}';
C_ListReaded : TGUID = '{AE05BC82-70E5-403A-9805-58C6038FB821}';
C_ListAfterPositionAdd : TGUID = '{580A4961-1B5E-404A-B6D1-2434394328F9}';
(*C_ListBeforePositionDelete : TGUID = '{CA74FA41-AFBA-4594-A3CF-33C5CB9E0C7B}';
C_ListAfterPositionDelete  : TGUID = '{4536F409-07F3-4008-B08C-8D2DCF683125}'; *)
C_ListActivePostionsCountChanged : TGUID = '{B347D2E4-B186-4453-9944-9C238914DADD}';
implementation

end.
