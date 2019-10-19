unit CustomChildProcess;

interface

  uses
    System.Classes,
    LoggerInterface;

  type

  TCustomChildProcess = class(TComponent)
  private
  protected
    fFunct : pLoggerFunct;
    procedure AfterCreate;virtual;
    procedure BeforeDestroy;virtual;
  public
    constructor Create(AOwner : TComponent; AFunct : pLoggerFunct); reintroduce; virtual;
    destructor Destroy; override;
  end;
  TChildProcessClass = class of TCustomChildProcess;
  
implementation




{ TCustomChildProcess }

procedure TCustomChildProcess.AfterCreate;
begin
end;

procedure TCustomChildProcess.BeforeDestroy;
begin 
end;

constructor TCustomChildProcess.Create(AOwner: TComponent; AFunct: pLoggerFunct);
begin 
  fFunct := AFunct;
  inherited Create(AOwner);
  AfterCreate;
end;

destructor TCustomChildProcess.Destroy;
begin
  BeforeDestroy;
  inherited Destroy;
end;

end.
