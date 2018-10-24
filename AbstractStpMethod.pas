unit AbstractStpMethod;

interface

uses
  WinApi.Windows, System.Classes;
  type
  TAbstractStpMethod = class;
  TStpThread = class(TThread)
   protected
    fMethod : TAbstractStpMethod;
    fBoard,
    fBoardProc,
    fMainProc : TObject;
    fExecResult : Byte;
   public
    constructor Create(const AMethod : TAbstractStpMethod;
                       const AMainProcess, ABoardProc, ABoard : TObject;
                       const  AOnTerminateNotify : TNotifyEvent); reintroduce; virtual;
    destructor Destroy; override;
    procedure Stop; virtual;
    property Method : TAbstractStpMethod read fMethod;
    property Board : TObject read fBoard;
    property BoardProc : TObject read fBoardProc;
    property MainProc : TObject read fMainProc;
  end;
  TStpThreadClass = class of TStpThread;

  TReadyState = (rsChecked, rsOk);
  TReadyStates = set of TReadyState;
  TAbstractStpMethod = class(TObject)
   private
    fThread : TStpThread;
    procedure OnThreadTerminated(Sender : TObject);
   protected
    fReady : TReadyStates;
    fBoard,
    fBoardProc,
    fMainProc : TObject;
    fParentMessageHandle: HWND;
    function ThreadClass : TStpThreadClass;virtual;
    procedure AfterCreate;virtual;
   public
    constructor Create(const AMainProcess, ABoardProc, ABoard : TObject; AMessageHandle: HWND);
    destructor Destroy; override;
    function ModuleName : string; virtual; abstract;
    function ReadyToStart:Boolean; virtual; abstract;   
    function ThreadExists: boolean;
    procedure Start; virtual;
    procedure Stop; virtual;
    property Board : TObject read fBoard;
    property BoardProc : TObject read fBoardProc;
    property MainProc : TObject read fMainProc;
  end;
  TStpMethodClass = class of TAbstractStpMethod;
  TStpMethodClasses = array of TStpMethodClass;

  TFinalProgrammerStpMethod = class(TAbstractStpMethod)
    function ExternalPowerNeded : Boolean; virtual;
    procedure Prepare; virtual;
  end;
  TFinalProgrammerClass = class of TFinalProgrammerStpMethod;
  
  const
  C_StpMethodStarted = 1024+1;
  C_StpMethodEnded = C_StpMethodStarted + 1;
  C_ProgrammerStarted = C_StpMethodEnded + 1;
  C_ProgrammerEnded =  C_ProgrammerStarted + 1;
implementation

uses
System.SysUtils;


{ TAbstractStpMethod }

constructor TAbstractStpMethod.Create(const AMainProcess, ABoardProc, ABoard : TObject; AMessageHandle: HWND);
begin
  fReady := [];
  fThread := nil;
  fMainProc := AMainProcess; 
  fBoardProc := ABoardProc;
  fBoard := ABoard;
  fParentMessageHandle := AMessageHandle;
  AfterCreate;
end;

procedure TAbstractStpMethod.AfterCreate;
begin
end;

destructor TAbstractStpMethod.Destroy;
begin
  if Assigned(fThread) then
  begin
    fThread.OnTerminate := nil;
    fThread.Stop;
  end;
  fBoard := nil;
  fBoardProc := nil;
  fMainProc := nil;
  inherited Destroy;
end;

procedure TAbstractStpMethod.Start; 
var
vThreadClass : TStpThreadClass;
begin
  vThreadClass := ThreadClass;
  if (not Assigned(fThread))
  and (Assigned(vThreadClass)) then
  begin
    fThread := vThreadClass.Create(Self, fMainProc, fBoardProc, fBoard, OnThreadTerminated);
    PostMessage(fParentMessageHandle, C_StpMethodStarted, 0, 0);
  end;
end;

procedure TAbstractStpMethod.Stop;
begin
  fReady := [];
  if Assigned(fThread) then
  begin
    fThread.Stop;
    fThread.WaitFor;
    FreeAndNil(fThread);
  end;
end;

function TAbstractStpMethod.ThreadClass: TStpThreadClass;
begin
  Result := nil;
end;

function TAbstractStpMethod.ThreadExists: boolean;
begin
  Result := Assigned(fThread);
end;

procedure TAbstractStpMethod.OnThreadTerminated(Sender: TObject);
begin
  fThread := nil;
  fReady := [];
  PostMessage(fParentMessageHandle, C_StpMethodEnded, 0, TStpThread(Sender).fExecResult);
end;

{ TStpThread }

constructor TStpThread.Create(const AMethod : TAbstractStpMethod;
                              const AMainProcess, ABoardProc, ABoard: TObject;
                              const AOnTerminateNotify : TNotifyEvent);
begin
  fExecResult := 1;
  fMethod := AMethod;
  fMainProc := AMainProcess;
  fBoardProc := ABoardProc;
  fBoard := ABoard;
  FreeOnTerminate := True;
  OnTerminate := AOnTerminateNotify;
  inherited Create(false);
end;

destructor TStpThread.Destroy;
begin
  fMethod := nil;
  fBoard := nil;
  fBoardProc := nil;
  fMainProc := nil;
  inherited Destroy;
end;

procedure TStpThread.Stop;
begin
  FreeOnTerminate := false;
  OnTerminate := nil;
  Terminate;
end;

{ TFinalProgrammerStpMethod }

function TFinalProgrammerStpMethod.ExternalPowerNeded: Boolean;
begin
  Result := False;
end;

procedure TFinalProgrammerStpMethod.Prepare;
begin
  fReady := [];
end;

end.
