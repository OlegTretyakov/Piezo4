unit PositionInterface;

interface
  
  type
  IPosition = interface(IInterface)
    ['{7286C364-E959-41CD-93CA-12BA2D1D7746}']
    function GetActive: boolean; stdcall;
    function GetBoardPos: byte; stdcall;
    procedure SetActive(const Value: boolean); stdcall;
    property BoardPos : byte read GetBoardPos;
    property Active : boolean read GetActive write SetActive; 
  end;


implementation

end.
