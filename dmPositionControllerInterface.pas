unit dmPositionControllerInterface;

interface
  type 
  pPositionsVoltages = ^TPositionsVoltages;
  TPositionsVoltages = record
    VDD,
    VC,
    VAnalog,
    VProg : Double;
    TimeStamp : TDateTime;
    procedure Assign(ASource : TPositionsVoltages); overload;
    procedure Assign(ASource : pPositionsVoltages); overload;
  end;
  IdmPositionController = interface(IInterface)
    ['{F391175B-36E1-4B8C-9865-AD0CC6E8E4DC}'] 
    function GetProtectState:Boolean;stdcall;
    function GetEnabled:Boolean; stdcall;
    procedure SetEnabled(Value : boolean); stdcall;
    function GetProtectTimeStamp:TDateTime;stdcall;
    function GetEnabledTimeStamp:TDateTime; stdcall;
    property Enabled:Boolean read GetEnabled write SetEnabled;
    property ProtectState:Boolean read GetProtectState;
    property EnabledTimeStamp:TDateTime read GetEnabledTimeStamp;
    property ProtectTimeStamp:TDateTime read GetProtectTimeStamp;
  end;
  IdmPositionsController = interface(IInterface)
    ['{3F14AF70-9BB5-406B-AB4D-D7AAAEE874A2}']
    procedure GetVoltageValues(ADest : pPositionsVoltages); stdcall;
    procedure SetVoltageValues(ASource : pPositionsVoltages);stdcall;
    function GetProtectState(AIndex : word):Boolean;stdcall;
    function GetEnabled(AIndex : word):Boolean;stdcall;
    function GetProtectTimeStamp:TDateTime;stdcall;
    function GetEnabledTimeStamp:TDateTime; stdcall;
    procedure SetEnabled(AIndex : word; Value : boolean);stdcall;
    property Enabled[AIndex : word]:Boolean read GetEnabled write SetEnabled;
    property ProtectState[AIndex : word]:Boolean read GetProtectState;  
    property EnabledTimeStamp:TDateTime read GetEnabledTimeStamp;
    property ProtectTimeStamp:TDateTime read GetProtectTimeStamp;
  end; 

implementation

{ Tmbvs21PowerValues }

procedure TPositionsVoltages.Assign(ASource: pPositionsVoltages);
begin
  VDD := ASource^.VDD;
  VC := ASource^.VC;
  VAnalog := ASource^.VAnalog;
  VProg := ASource^.VProg;
  TimeStamp := ASource^.TimeStamp;
end;

procedure TPositionsVoltages.Assign(ASource: TPositionsVoltages);
begin
  VDD := ASource.VDD;
  VC := ASource.VC;
  VAnalog := ASource.VAnalog;
  VProg := ASource.VProg;
  TimeStamp := ASource.TimeStamp;
end;

end.
