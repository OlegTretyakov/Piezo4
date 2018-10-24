unit ThreeDimTypes;

interface
  uses MatrixTypes;

  type
  IDepthCell = interface(ICell)
    ['{97E1F1EB-0E6E-464C-862A-B277EC3A89BF}']
    function GetDepth: Word; stdcall;
    procedure SetDepth(const Value: Word);stdcall;  
    function GetErrorsCount: Word; stdcall;
    procedure SetErrorsCount(const Value: Word);stdcall;
    property Depth : Word read GetDepth write SetDepth;
    function QueryItem(Index: Integer; const IID: TGUID; out Obj):boolean; stdcall; 
    property ErrorsCount : Word read GetErrorsCount write SetErrorsCount;
  end;

  I3DimCell = interface (IInterface)
    ['{17E908C2-3C45-41C7-833C-F088E7D0EEB2}']
    function GetIndex:Word; stdcall;
    property CellIndex : Word read GetIndex;
  end;

implementation


end.
