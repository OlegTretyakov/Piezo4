unit FreqMatrixIF;

interface
uses Classes, ThreeDimTypes, MatrixTypes, MCPInterface;
  type
  IFreqMeasCell = interface(I3DimCell)
  ['{542E1628-8BAC-43DA-8FDC-1F579CA80BFB}']
    function GetErrCode: SmallInt; stdcall;
    function GetIzmNum: LongWord; stdcall;
    function GetMemory: TMemoryStream; stdcall;
    function GetFreq: double; stdcall;   
    function GetDisp: double; stdcall;
    procedure SetErrCode(const Value: SmallInt);stdcall;
    procedure SetIzmNum(const Value: LongWord); stdcall;
    procedure SetFreq(const Value: double); stdcall; 
    procedure SetDisp(const Value: double); stdcall;
    function GetVar: byte; stdcall;
    procedure SetVar(const Value: byte); stdcall;
    property ErrCode : SmallInt read GetErrCode write SetErrCode;
    property IzmNum : LongWord read GetIzmNum write SetIzmNum; 
    property Freq : double read GetFreq write SetFreq; 
    property Disp : double read GetDisp write SetDisp;
    property mVar : byte read GetVar write SetVar;
    property Memory : TMemoryStream read GetMemory;
  end;

  IFreqMatrix = interface(IMultiDimMatrix)
    ['{10F65339-CA1F-4D0E-A967-606C27B53655}']
    function ByVar(ACol : byte; ATag : SmallInt; AVar: byte; const IID: TGUID; out Obj):boolean; stdcall;
    function TotalCount : word; stdcall;
    function ErrorCount : word; stdcall;
  end;

  IValueCell = interface (ICell)
    ['{0CFB0835-176E-4B4D-A049-BB30C6859897}']
    function GetValue: double; stdcall;
    procedure SetValue(AValue : double); stdcall;
    property Value: double read GetValue write SetValue;
  end;

  IFreqTable = interface(IMultiDimMatrix)
    ['{DF1E1D89-6CCF-4493-A07E-85B3D4876E0E}']
    function GetFreq(ACol, ARow : byte): double;stdcall;
    procedure SetFreq(ACol, ARow : byte; AValue : double); stdcall;
    property Freq[ACol, ARow : byte]: double read GetFreq write SetFreq;  
    function GetNominal: double;stdcall;
    procedure SetNominal(AValue : double); stdcall;
    property Nominal: double read GetNominal write SetNominal;
    function GetDeviation(ACol, ARow : byte): double;stdcall;
    property Deviation[ACol, ARow : byte]: double read GetDeviation;  
  end;


  IFreqCharactTable = interface(IFreqTable)
    ['{7EF927A6-1221-4A9F-B766-F7177852E522}']
    procedure Fill(const AMeasTable: IInterface; ARegistersClass: TRegistersClass); stdcall;
    procedure CalcFreq(ABase, ACalc :TRegisters; const ASource, ADest, AFreqLow, AFreqHi : IInterface); stdcall;
  end;
implementation

end.
