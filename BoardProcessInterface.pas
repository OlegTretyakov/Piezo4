unit BoardProcessInterface;

interface
  uses
    LoggerInterface;

  type  
  TCBPEventParams = record
    Board : TObject;
    Params : Pointer;
  end;
  pCBPEventParams = ^TCBPEventParams;
  TForEachBoardMethod = procedure (const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
  IBoardProcess = interface(IInterface)
    ['{D38C8E3B-7CDB-4CB0-BB3F-B81CC0B6A420}']
    function GetItems(AIndex : byte): TObject; stdcall;
    procedure ForEachBoard(AMethod : TForEachBoardMethod; Params : Pointer=nil); stdcall;
    procedure StartSearch; stdcall;
    function BoardsCount : byte; stdcall;
    function PositionsCount: cardinal; stdcall; 
    function ActivePositionsCount: cardinal; stdcall;
    procedure DeleteBoard(AIndex : byte); stdcall;
    procedure Clear; stdcall;
    property Board[index : byte] : TObject read GetItems; default;
    function IndexOf(const ABoard : TObject) : Integer; overload;
    function IndexOf(const ABoardSN : word) : Integer; overload;
    procedure DeleteInactivePositions;stdcall;
    procedure SetAllPositionsToState(AActive : boolean);stdcall;
  end;
  IBoardProcessLog = interface(IBoardProcess)
  ['{644C06D7-2048-4A5E-8E7C-08E016C653FF}']  
    procedure Log(ALevel: TLogInfo; const Text: WideString;  Instance: TObject=nil);stdcall;
    function LevelEnabled(ALevel : TLogInfo):boolean; stdcall;
  end;

const
C_SearchStart : TGUID = '{A8FCF405-D71D-4253-892F-75220845C06A}';
C_BoardAdded : TGUID = '{5A7155F7-6DFC-417D-AA12-5BDA5715D0BB}';
C_SearchComplete : TGUID = '{3CD41B2E-4E9A-4E89-83FD-872F15443330}';
C_BeforeBoardDelete : TGUID = '{D7DBE311-2C68-4ED6-A847-C26B543A49E5}';
implementation

end.
