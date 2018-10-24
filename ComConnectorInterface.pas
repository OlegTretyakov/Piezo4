unit ComConnectorInterface;

interface
  uses Windows;
  type   
  TDSR = record
    State: boolean;
    OffTime : TDateTime;
  end;
  pDSR = ^TDSR;
  TPortSyncMethod = (smThreadSync, smWindowSync, smNone);
  IComConnector = interface (IInterface)
  ['{097E253A-5DCA-4A58-BB4A-DBF3F241B820}']      
    function PortWrite(const Buffer; Count : LongWord) : integer; stdcall; 
    function GetSyncMethod: TPortSyncMethod; stdcall;
    procedure SetSyncMethod(const Value: TPortSyncMethod);stdcall;
    property SyncMethod: TPortSyncMethod read GetSyncMethod write SetSyncMethod;
    function GetDSR: pDSR; stdcall;
    function FlowControl: boolean;stdcall;
    function Port: Byte; stdcall;
    property DSR : pDSR read GetDSR;
    procedure ConfigPort(APort : byte; ABaud : LongWord;
              AdtrEnable, ArtsEnable, AFlowControl : boolean); stdcall;
    procedure ConfigParser(const StartMarker; StartSize : byte;
                          const StopMarker; StopSize : byte;
                          AIncludeMarkers : boolean = false); stdcall;
    procedure SetBufferSize(AInput, AOutput : LongWord); stdcall;
    function PortOpenOk : boolean; stdcall;
    procedure PortClose; stdcall;
    function IsConnected : boolean; stdcall;
    function CallChallenge(const After : LongWord) : boolean; stdcall;
    procedure SetTimeOutMode(const ATimeOut : LongWord); stdcall;
    procedure StopTimeOut; stdcall;
    procedure StopTimers; stdcall;
  end;  

implementation

end.
