unit VvarMatrixIF;

interface
uses
  System.Classes, ThreeDimTypes, MatrixTypes;

  type
  IVvarCell = interface(I3DimCell)
  ['{FF237668-F8C1-4D83-B89C-7E80061D5F22}']
    function GetErrCode: SmallInt; stdcall;
    function GetIzmNum: LongWord; stdcall;
    function GetMemory: TMemoryStream; stdcall;
    function GetVdd: double; stdcall;
    function GetVC: double; stdcall;
    function GetVoltage: double; stdcall;
    procedure SetErrCode(const Value: SmallInt);stdcall;
    procedure SetIzmNum(const Value: LongWord); stdcall;  
    procedure SetVdd(const Value: double); stdcall;
    procedure SetVC(const Value: double); stdcall;
    procedure SetVoltage(const Value: double); stdcall;
    function GetVar: byte; stdcall;
    procedure SetVar(const Value: byte); stdcall;
    property ErrCode : SmallInt read GetErrCode write SetErrCode;
    property IzmNum : LongWord read GetIzmNum write SetIzmNum; 
    property Vdd : double read GetVdd write SetVdd;
    property VC : double read GetVC write SetVC;
    property Voltage : double read GetVoltage write SetVoltage;
    property mVar : byte read GetVar write SetVar;
    property Memory : TMemoryStream read GetMemory;
  end;

  IVvarMatrix = interface(IMultiDimMatrix)
    ['{08700970-6FEB-4CB9-A4DD-570C052D1282}']
    function ByVar(ACol : byte; ATag : SmallInt; AVar: byte; const IID: TGUID; out Obj):boolean; stdcall;
    function TotalCount : word; stdcall;
    function ErrorCount : word; stdcall;
  end;

  IValueCell = interface (ICell)
    ['{0C51DC4C-27A6-47EA-B837-F00025E11CE0}']
    function GetValue: double; stdcall;
    procedure SetValue(AValue : double); stdcall;
    property Value: double read GetValue write SetValue;
  end;

  IVoltageTable = interface(IMultiDimMatrix)
    ['{5E4BC30F-B89D-4358-A695-40DE9E5FD217}']
    function GetVoltage(ACol, ARow : byte): double;stdcall;
    procedure SetVoltage(ACol, ARow : byte; AValue : double); stdcall;
    property Voltage[ACol, ARow : byte]: double read GetVoltage write SetVoltage;
  end;

implementation

end.
