unit DiscreteBlock;

interface
  uses System.Classes, AbstractTag, PLCTag, ProtocolDriver, ProtocolTypes;

  type

  TDiscreteBlock = class(TPLCTag, IProtocolDiscreteTag)
   private
    FReadedValues,
    FValuesToWrite : array of Boolean;
   protected
    procedure WriteOK; override;
    function GetReadedValue(AIndex: Word): boolean;
    procedure SetReadedValue(AIndex: Word; const Value: boolean);
    function GetToWriteValue(AIndex: Word): boolean;
    procedure SetToWriteValue(AIndex: Word; const Value: boolean);
    property ValuesReaded[AIndex : Word]: boolean read GetReadedValue write SetReadedValue;
    property ValuesToWrite[AIndex : Word]: boolean read GetToWriteValue write SetToWriteValue;
   public
    constructor Create(AOwner: TComponent;
                        const ADriver : TProtocolDriver;
                        AStation,
                        AAddress : Word;
                        ASize : TDiscreteSize;
                        AReadOnly : boolean); reintroduce;
    destructor Destroy; override;
    property Values[AIndex : Word]: Boolean read GetReadedValue write SetToWriteValue;
  end;
  TDiscreteBlockClass = class of TDiscreteBlock;
  TDiscreteElement = class(TPLCBlockElement)
  private
    FBlock : TDiscreteBlock;
    function GetValue: Boolean;
    procedure SetValue(const Value: Boolean);
   public
    constructor Create(AOwner : TComponent; const ABlock : TDiscreteBlock; AIndex : TDiscreteIndex); reintroduce;
    property Value: Boolean read GetValue write SetValue;
  end;

implementation

uses System.SysUtils, hsstrings;

{ TDiscreteBlock }

constructor TDiscreteBlock.Create(AOwner: TComponent;
                                  const ADriver: TProtocolDriver;
                                  AStation, AAddress: Word;
                                  ASize: TDiscreteSize;
                                  AReadOnly: boolean);
var
vTagType : TTagType;
begin
  if AReadOnly then
    vTagType := ttDiscrete
  else
    vTagType := ttCoil;
  inherited Create(AOwner, ADriver, AStation, AAddress, Word(ASize), vTagType);
  SetLength(FReadedValues, Word(ASize));
  SetLength(FValuesToWrite, Word(ASize));
end;

destructor TDiscreteBlock.Destroy;
begin
  inherited Destroy;
  SetLength(FValuesToWrite, 0);
  SetLength(FReadedValues, 0);
end;

function TDiscreteBlock.GetReadedValue(AIndex: Word): boolean;
begin
  Result := FReadedValues[AIndex];
end;

function TDiscreteBlock.GetToWriteValue(AIndex: Word): boolean;
begin
  result := FValuesToWrite[AIndex];
end;

procedure TDiscreteBlock.SetReadedValue(AIndex: Word; const Value: boolean);
var
vObj : TPLCBlockElement;
vObjI : IProtocolTag;
begin
  if (FReadedValues[AIndex] <> Value) then
  begin
    if self.FindElement(AIndex, vObj)
    and Supports(vObj, IProtocolTag, vObjI) then
      vObjI.SetDataChanged(true);
    vObjI := nil;
    if (not FDataChanged) then
      FDataChanged := true;
  end;
  FReadedValues[AIndex] := Value;
end;

procedure TDiscreteBlock.SetToWriteValue(AIndex: Word; const Value: boolean);
begin
  FValuesToWrite[AIndex] := Value;
end;

procedure TDiscreteBlock.WriteOK;
var
vIdx : word;
begin
  inherited;
  vIdx := 0;
  while vIdx <= High(FValuesToWrite) do
  begin
    SetReadedValue(vIdx, FValuesToWrite[vIdx]);
    inc(vIdx);
  end;
end;

{ TDiscreteElement }

constructor TDiscreteElement.Create(AOwner: TComponent; const ABlock: TDiscreteBlock; AIndex: TDiscreteIndex);
begin
  inherited Create(AOwner, ABlock, Word(AIndex));
  FBlock := ABlock;
end;

function TDiscreteElement.GetValue: Boolean;
begin
  if not Assigned(FBlock) then
    raise Exception.Create(SinvalidTag);
  result := FBlock.Values[self.Index];
end;

procedure TDiscreteElement.SetValue(const Value: Boolean);
begin
  if not Assigned(FBlock) then
    raise Exception.Create(SinvalidTag);
  FBlock.Values[self.Index] := Value;
end;

end.
