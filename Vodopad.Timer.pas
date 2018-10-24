unit Vodopad.Timer;

interface
  uses WinApi.Windows, Winapi.Messages, System.Classes;
  type
  TvdTimer = class(TComponent)
  private
    FInterval: Cardinal;
    FWindowHandle: HWND;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure WndProc(var Msg: TMessage);
  protected
    procedure Timer; dynamic;
{$IF DEFINED(CLR)}
  strict protected
    procedure Finalize; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

implementation

{ TvdTimer }

constructor TvdTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := false;
  FInterval := 10;
  FWindowHandle := AllocateHWnd(WndProc);
end;

destructor TvdTimer.Destroy;
begin
  FEnabled := False;
  if FWindowHandle <> 0 then
  begin
    UpdateTimer;
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
  end;
{$IF DEFINED(CLR)}
  System.GC.SuppressFinalize(self);
{$ENDIF}
  inherited Destroy;
end;

{$IF DEFINED(CLR)}
procedure TvdTimer.Finalize;
begin
  FEnabled := False;
  if FWindowHandle <> 0 then
  begin
    KillTimer(FWindowHandle, 1);
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
  end;
  inherited;
end;
{$ENDIF}

procedure TvdTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
      try
        Timer;
      except

      end
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TvdTimer.UpdateTimer;
begin
  KillTimer(FWindowHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create('No dmTimer');
end;

procedure TvdTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TvdTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TvdTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TvdTimer.Timer;
var
vOnTimer : TNotifyEvent;
begin
  vOnTimer := FOnTimer;
  if Assigned(vOnTimer) then
    vOnTimer(Self);
end;

end.
