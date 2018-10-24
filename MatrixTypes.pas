unit MatrixTypes;

interface
 type
  pColumnItem = ^TColumnItem;
  TColumnItem = record
    Index : byte;
    Header : double;
  end;

  pRowItem = ^TRowItem;
  TRowItem = record
    Index : byte;
    M_Tag : SmallInt;
  end;

  ICell = interface (IInterface)
    ['{287BE155-362D-453C-A020-022148D7C614}']
    function Col : pColumnItem; stdcall;
    function Row : pRowItem; stdcall;
  end; 

  IMultiDimMatrix = interface(IInterface)
    ['{C283D3B5-66B0-415D-98B7-048D1C17AE73}']
    procedure Clear; stdcall;
    function ColCount : integer; stdcall;
    function RowCount : integer; stdcall;
    function Header(Index: byte): double; stdcall;
    function Tag(Index: byte): SmallInt; stdcall; 
    function FindCol(AHeader : double; var ACol : byte):Boolean; stdcall;
    function AddCol(AHeader : double) : integer; stdcall;
    function AddRow(ATag : SmallInt) : integer; stdcall;
    function FindRow(ATag: SmallInt; var ARow : byte): boolean; stdcall;
    function ByIndex(ACol, ARow : byte; const IID: TGUID; out Obj): boolean; stdcall;
    function ByParams(ACol: byte; ATag : SmallInt; const IID: TGUID; out Obj): boolean; stdcall;
  end;
implementation

end.
