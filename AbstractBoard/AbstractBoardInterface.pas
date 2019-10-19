unit AbstractBoardInterface;


interface

  type
  TForEachPositionMethod = procedure (const APosition:TObject; PositionIndex : Word; Params : Pointer); stdcall;

  IAbstractBoard = interface(IInterface)
    ['{4EE4A203-4F34-4FEC-B789-4F7A4798BC1E}']
    function GetConnected : Boolean; stdcall;
    function GetSerialNum: Word; stdcall;
    function QueryPositionListInterface(const IID: TGUID; out Obj): boolean; stdcall;
    property Connected : Boolean read GetConnected;
    property SerialNum : Word read GetSerialNum;
    procedure ForEachPosition(AMethod : TForEachPositionMethod; Params : Pointer=nil); stdcall;
  end;

  IBoardEventBusExec = interface(IInterface)
    ['{9475BB43-04B9-4743-A516-5A826441A000}']
    procedure Execute(Sender : TObject; Event : TGUID; Params : Pointer); stdcall;
  end;

implementation



end.