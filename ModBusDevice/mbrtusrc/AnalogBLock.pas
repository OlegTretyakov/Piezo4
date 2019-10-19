unit AnalogBLock;

interface
  uses
    System.Classes,
    AbstractTag,
    PLCTag,
    ProtocolDriver,
    ProtocolTypes;

  type
  TAnalogBlock = class(TPLCTag, IProtocolAnalogTag)
   private
    FReadedValues,
    FValuesToWrite : array of word;
   protected
    procedure WriteOK; override;
    function GetReadedValue(AIndex: Word): Word;
    procedure SetReadedValue(AIndex: Word; const Value: Word);
    function GetToWriteValue(AIndex: Word): Word;
    procedure SetToWriteValue(AIndex: Word; const Value: Word);
    property ValuesReaded[AIndex : Word]: Word read GetReadedValue write SetReadedValue;
    property ValuesToWrite[AIndex : Word]: Word read GetToWriteValue write SetToWriteValue;
   public
    constructor Create(AOwner: TComponent;
                        const ADriver : TProtocolDriver;
                        AStation,
                        AAddress : Word;
                        ASize : TAnalogSize;
                        AReadOnly : boolean); reintroduce;
    destructor Destroy; override;
    property Values[AIndex : Word]: Word read GetReadedValue write SetToWriteValue;
  end;

  TAnalogBlockClass = class of TAnalogBlock;
  TAnalogElement = class(TPLCBlockElement)
  private
    FBlock : TAnalogBlock;
    function GetValue: Word;
    procedure SetValue(const Value: Word);
   public
    constructor Create(AOwner : TComponent; const ABlock : TAnalogBlock; AIndex : TDiscreteIndex); reintroduce;
    property Value: Word read GetValue write SetValue;
  end;

implementation

uses
  System.SysUtils,
  hsstrings;

{ TAnalogBlock }

constructor TAnalogBlock.Create(AOwner: TComponent;
                                const ADriver: TProtocolDriver;
                                AStation, AAddress: Word;
                                ASize: TAnalogSize;
                                AReadOnly: boolean);
var
  vTagType : TTagType;
begin
  if AReadOnly then
    vTagType := ttInput
  else
    vTagType := ttHolding;
  inherited Create(AOwner, ADriver, AStation, AAddress, Word(ASize), vTagType);
  SetLength(FReadedValues, Word(ASize));
  SetLength(FValuesToWrite, Word(ASize));
end;

destructor TAnalogBlock.Destroy;
begin
  inherited Destroy;
  SetLength(FValuesToWrite, 0);
  SetLength(FReadedValues, 0);
end;

function TAnalogBlock.GetReadedValue(AIndex: Word): Word;
begin
  Result := FReadedValues[AIndex];
end;

function TAnalogBlock.GetToWriteValue(AIndex: Word): Word;
begin
  result := FValuesToWrite[AIndex];
end;

procedure TAnalogBlock.SetReadedValue(AIndex: Word; const Value: Word);
var
  vObj : TPLCBlockElement;
  vObjI : IProtocolTag;
begin
  if (FReadedValues[AIndex] <> Value) then
  begin
    if FindElement(AIndex, vObj)
    and Supports(vObj, IProtocolTag, vObjI) then
      vObjI.SetDataChanged(true);
    vObjI := nil;
    if (not FDataChanged) then
      FDataChanged := true;
  end;
  FReadedValues[AIndex] := Value;
end;

procedure TAnalogBlock.SetToWriteValue(AIndex: Word; const Value: Word);
begin
  FValuesToWrite[AIndex] := Value;
end;

procedure TAnalogBlock.WriteOK;
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

{ TAnalogElement }

constructor TAnalogElement.Create(AOwner: TComponent; const ABlock: TAnalogBlock; AIndex: TDiscreteIndex);
begin
  inherited Create(AOwner, ABlock, Word(AIndex));
  FBlock := ABlock;
end;

function TAnalogElement.GetValue: Word;
begin
  if not Assigned(FBlock) then
    raise Exception.Create(SinvalidTag);
  result := FBlock.Values[self.Index];
end;

procedure TAnalogElement.SetValue(const Value: Word);
begin
  if not Assigned(FBlock) then
    raise Exception.Create(SinvalidTag);
  FBlock.Values[self.Index] := Value;
end;

end.
