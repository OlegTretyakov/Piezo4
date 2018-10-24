unit AbstractReconstructorThread;

interface
  uses
  System.Classes, LoggerInterface, Stp8PositionDBExtention, ChipManagerInterface;
  type
  TReconstructorAbstractThread = class;
  TReconstructorThreadClass = class of TReconstructorAbstractThread;
  TItemState = (isSuspended, isWait, isStart, isWork, isFinish);
  TItemStates = set of TItemState;
  TReconsructionItem = class(TObject)
   private
    fMeasuredObj : TPositionDBExtention;
    fThreadClass : TReconstructorThreadClass;
   public
    TimeStart : TDateTime;
    ItemIndex,
    Progress : Word;
    State : TItemState;
    ExceptCount : Cardinal;
    constructor Create(const AMeasuredObj :TPositionDBExtention);
    destructor Destroy; override;
    function Init(const APluginManager :IChipPluginManager):boolean;
    property MeasuredObj: TPositionDBExtention read fMeasuredObj;
    property ThreadClass : TReconstructorThreadClass read fThreadClass;
  end;

  IFormMessageInterface = interface(IInterface)
    ['{5BD51912-9A84-4521-B949-BEA5C1E06D7F}']
    function GetNext(var AItem : TReconsructionItem): Boolean; stdcall;
    procedure UpdateProgress; stdcall;
  end;



  TReconstructorAbstractThread = class(TThread)
   protected
    fProcessorBitIdx : Byte;
    fMainProc : TObject;
    fMessageInterface : TComponent;
    CalcLogger : ILogger;
    procedure AfterCreate; virtual;
    function GetNext(var AItem : TReconsructionItem):Boolean;
    function UpdateProgress: Boolean;
   public
    constructor Create(ProcessorBitIdx : Byte;
                      MainProc : TObject;
                      AOnTerminate : TNotifyEvent;
                      MessageInterface : TComponent); reintroduce; virtual;
    destructor Destroy; override;
    property ProcessorBitIdx : byte read fProcessorBitIdx;
    property Terminated;
  end;

const
cWaitTimeOut = 1000;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  CalcLogger;



{ ReconstructorAbstractThread }

constructor TReconstructorAbstractThread.Create(ProcessorBitIdx : Byte;
                      MainProc : TObject;
                      AOnTerminate : TNotifyEvent;
                      MessageInterface : TComponent);
var
vCalcLoggerGetter : ICalcLoggerGetter;
begin
  inherited Create(false);
  fMainProc := MainProc;
  fProcessorBitIdx := ProcessorBitIdx;
  fMessageInterface := MessageInterface;
  if (not Supports(fMainProc, ICalcLoggerGetter, vCalcLoggerGetter))
  or (not vCalcLoggerGetter.Get(CalcLogger)) then
    CalcLogger := nil;
  vCalcLoggerGetter := nil;
  OnTerminate := AOnTerminate;
  FreeOnTerminate := True;
  AfterCreate;
end;


destructor TReconstructorAbstractThread.Destroy;
begin
  if Assigned(CalcLogger) then
      CalcLogger.Log(lInfo, Format('Thread ID:%d Processor Idx:%d - destroyed',
      [ThreadID, fProcessorBitIdx]));
  CalcLogger := nil;
  fMessageInterface := nil;
  inherited Destroy;
end;

{ TReconsructionItem }

constructor TReconsructionItem.Create(const AMeasuredObj: TPositionDBExtention);
begin
  fMeasuredObj := AMeasuredObj;
  fThreadClass := nil;
  State := isSuspended;
  ExceptCount := 0;
  Progress := 0;
end;

destructor TReconsructionItem.Destroy;
begin
  fMeasuredObj := nil;
  fThreadClass := nil;
  inherited;
end;

type
TRecostructorClassGetter = function : TReconstructorThreadClass;stdcall;

function TReconsructionItem.Init(const APluginManager: IChipPluginManager): boolean;
var
vRecostructorClassGetter : TRecostructorClassGetter;
vRecostructorClassGetterPtr : Pointer;
begin
  Result := Assigned(APluginManager)
            and APluginManager.FindChipPligunFunction(fMeasuredObj.ChipGUID, 'RecostructorClass', vRecostructorClassGetterPtr);
  if Result then
  try
    @vRecostructorClassGetter := vRecostructorClassGetterPtr;
    fThreadClass := vRecostructorClassGetter;
    Result := Assigned(fThreadClass);
  except
    Result := False;
    fThreadClass := nil;
  end;
end;

procedure TReconstructorAbstractThread.AfterCreate;
begin
end;

function TReconstructorAbstractThread.GetNext(var AItem : TReconsructionItem): Boolean;
var
vResult : boolean;
vItem : TReconsructionItem;
vBitIdx : Byte;
begin
  if Terminated then
  begin
    Result := False;
    Exit;
  end;
  vBitIdx := fProcessorBitIdx;
  vResult := False;
  try
    Self.Synchronize(
    procedure
    var
    vIntf : IFormMessageInterface;
    begin
      try
        try
          vResult := Supports(fMessageInterface, IFormMessageInterface, vIntf)
            and vIntf.GetNext(vItem);
        finally
          vIntf := nil;
        end;
      except on e : Exception do
        if Assigned(CalcLogger) then
        begin
          CalcLogger.Log(lWarning, Format('Processor Idx:%d Exception %s during get next item',
          [vBitIdx, e.ClassName]));
          CalcLogger.Flush(true);
        end;
      end;
    end);
  finally
    result := vResult and Assigned(vItem);
    if result then
      AItem := vItem
    else
      AItem := nil;
  end;
end;

function TReconstructorAbstractThread.UpdateProgress: Boolean;
var
vResult : boolean;
vBitIdx : Byte;
begin
  if Terminated then
  begin
    Result := False;
    Exit;
  end;
  vResult := False;
  try
    Self.Synchronize(
    procedure
    var
    vIntf : IFormMessageInterface;
    begin
      try
        try
          vResult := Supports(fMessageInterface, IFormMessageInterface, vIntf);
          if vResult then
            vIntf.UpdateProgress;
        finally
          vIntf := nil;
        end;
      except on e : Exception do
        if Assigned(CalcLogger) then
        begin
          CalcLogger.Log(lWarning, Format('Processor Idx:%d Exception %s during update progress',
          [vBitIdx, e.ClassName]));
          CalcLogger.Flush(true);
        end;
      end;
    end);
  finally
    result := vResult;
  end;
end;

end.
