unit Position;

interface
uses System.Classes, ExtentionsList, AbstractExtention, PositionInterface;
type

  TPosition = class(TComponent, IPosition)
   strict private
    fBoardPos : byte;
    fActive : boolean;
    fExtentions : TExtentions;
   private
    function GetActive: boolean; stdcall;
    function GetBoardPos: byte; stdcall;
    procedure SetActive(const Value: boolean); stdcall; 
   protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
   public
    constructor Create(AOwner : TComponent; ABoardPos : byte); reintroduce; virtual;
    destructor Destroy;override;
    property BoardPos : byte read GetBoardPos;
    property Active : boolean read GetActive write SetActive;
    property Extentions : TExtentions read fExtentions;
  end;

implementation

uses System.SysUtils, PositionListInterface;

constructor TPosition.Create(AOwner : TComponent; ABoardPos : byte);
begin
  inherited Create(AOwner);  
  fExtentions := TExtentions.Create(self);
  fBoardPos := ABoardPos;
  fActive := true;
end;

destructor TPosition.Destroy;
begin
  fExtentions.Clear;
  inherited;
end;

function TPosition.GetActive: boolean;
begin
  result := fActive;
end;

function TPosition.GetBoardPos: byte;
begin
  result := fBoardPos;
end;

procedure TPosition.SetActive(const Value: boolean);
var
vPosList : IPositionsListExt;
begin
  if (fActive <> Value) then
  begin
    fActive := Value;
    if Supports(Owner, IPositionsListExt, vPosList) then
    begin
      if fActive then
        vPosList.IncActive
      else
        vPosList.DecActive;
    end;
    vPosList := nil;
  end;
end;  

function TPosition.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> S_OK) and Supports(fExtentions, IID, Obj) then
    Result := S_OK;
end;


end.
