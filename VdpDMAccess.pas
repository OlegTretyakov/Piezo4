unit VdpDMAccess;

interface
  uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client;
  type
  TvdConnection = TFDCustomConnection;
  TvdTransaction = TFDTransaction;
  TvdDataSet = TFDRdbmsDataSet;
  TvdQuery = TFDQuery;
  TvdStoredProc = TFDStoredProc;
  IDMAccess = interface (IInterface)
    ['{DCA07C15-8F34-4DE2-9841-4E17DBF81E5A}']
    function DBHomePath : string; stdcall;
    function MeasuresHomePath : string; stdcall;
    function Connected: Boolean; stdcall;
    function CreateConnection(const AOwner : TComponent) : TvdConnection;stdcall;
    procedure CloseConnection(const AConnection : TvdConnection);stdcall;
    function CreateReadTransaction(const AOwner : TComponent; const AConnection : TvdConnection):TvdTransaction; stdcall;
    function CreateWriteTransaction(const AOwner : TComponent; const AConnection : TvdConnection):TvdTransaction; stdcall;
    function CreateQuery(const AOwner : TComponent; const AConnection : TvdConnection = nil):TvdQuery; stdcall;
    function CreateStoredProc(const AOwner : TComponent; const AConnection : TvdConnection = nil):TvdStoredProc; stdcall;
    function CreateReadQuery(const AOwner : TComponent; const AConnection : TvdConnection = nil) : TvdQuery; overload; stdcall;
    function CreateReadStoredProc(const AOwner : TComponent; const AConnection : TvdConnection = nil) : TvdStoredProc; overload;stdcall;
    function CreateReadQuery(const AOwner : TComponent; const SQLText : string; const AConnection : TvdConnection = nil) : TvdQuery; overload; stdcall;
    function CreateReadStoredProc(const AOwner : TComponent; const AStoredProcName : string; const AConnection : TvdConnection = nil) : TvdStoredProc; overload; stdcall;
    function CreateWriteQuery(const AOwner : TComponent; const AConnection : TvdConnection = nil) : TvdQuery; overload; stdcall;
    function CreateWriteStoredProc(const AOwner : TComponent; const AConnection : TvdConnection = nil) : TvdStoredProc; overload; stdcall;
    function CreateWriteQuery(const AOwner : TComponent; const SQLText : string; const AConnection : TvdConnection = nil) : TvdQuery; overload; stdcall;
    function CreateWriteStoredProc(const AOwner : TComponent; const AStoredProcName : string; const AConnection : TvdConnection = nil) : TvdStoredProc; overload; stdcall;
    procedure Commit(const ADataSet : TvdDataSet); stdcall;
    procedure OnException(const ADataSet : TvdDataSet; E : Exception); stdcall;
  end;

implementation

end.
