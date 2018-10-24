unit Vodopad.EventList;

interface

uses System.Classes, System.Generics.Collections;
type
  {PMethodList = ^TMethodList;
  TMethodList = Array[0..$00FFFFFF] of TMethod;
  TCustomMethodList = class abstract (TObject)
   private
    FList: PMethodList;
    fCount : integer;
    FCapacity : integer;
    function GetItem(Index: integer): TMethod;
    procedure Grow;
    function IndexOf(const AMethod: TMethod): Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
   protected
    procedure Add(const AMethod: TMethod);
    procedure Remove(const AMethod: TMethod); overload;
   public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Count : integer read fCount;
    property Capacity: Integer read FCapacity;
    property Item [index : integer] : TMethod read GetItem; default;
    procedure Remove(Index : integer);overload;
  end; }

  TEventList = class(TList<TNotifyEvent>)
   public
    procedure Subscribe(const AMethod : TNotifyEvent);
    procedure Execute(Sender : TObject);
    procedure UnSubscribe(const AMethod: TNotifyEvent);
  end;
  TCustomObjEvent = procedure (Sender : TObject; Event : TGUID; Params : Pointer) of object;
  TCustomObjEventList = class(TList<TCustomObjEvent>)
   public
    procedure Subscribe(const AMethod : TCustomObjEvent);
    procedure Execute(Sender : TObject; Event : TGUID; Params : Pointer);
    procedure UnSubscribe(const AMethod: TCustomObjEvent);
  end;

  TStrEvent = procedure(Sender: TObject; const Str: string) of object;
  TStrEventList = class(TList<TStrEvent>)
   public
    procedure Subscribe(const AMethod : TStrEvent);
    procedure Execute(Sender : TObject; const Str : string);
    procedure UnSubscribe(const AMethod: TStrEvent);
  end;

implementation

{$REGION ' TCustomEventList '}
(*
  {TCustomEventList}
  
  constructor TCustomMethodList.Create;
  begin
    inherited;
  end;
  
  destructor TCustomMethodList.Destroy;
  begin
    SetCapacity(0);
    inherited;
  end;

  procedure TCustomMethodList.Grow;
  var Delta: Integer;
  begin
    if FCapacity > 64 then
      Delta := FCapacity div 4
    else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
    SetCapacity(Count + Delta);
  end;
  
  procedure TCustomMethodList.SetCapacity(const Value: Integer);
  begin
    if FCapacity <> Value then
    begin
      FCapacity := Value;
      ReallocMem(FList, FCapacity * SizeOf(TMethod));
    end;
  end;
  
  procedure TCustomMethodList.SetCount(const Value: Integer);
  begin
    FCount := Value;
    if(FCount > FCapacity) then
      Grow;
  end;
  
  procedure TCustomMethodList.Clear;
  begin
    FCount:= 0;
    SetCapacity(0);
  end;

  procedure TCustomMethodList.Add(const AMethod: TMethod);
  begin
    if (IndexOf(AMethod) <> -1) then
      exit;
    SetCount(Count + 1);
    FList^[Count - 1].Data:= AMethod.Data;
    FList^[Count - 1].Code:= AMethod.Code;
  end;

  procedure TCustomMethodList.Remove(Index : integer);
  begin
    if (Index < 0) or (index >= fCount) then
      Exit;
    Dec(FCount);
    if (Index < Count) then
      System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(TMethod));
  end;
  
  procedure TCustomMethodList.Remove(const AMethod: TMethod);
  begin
    Remove(IndexOf(AMethod));
  end;

  function TCustomMethodList.IndexOf(const AMethod: TMethod): Integer;
  begin
    result := 0;
    while (result < fCount) do
    begin
      if ((FList^[result].Code = AMethod.Code)
      and (FList^[result].Data = AMethod.Data)) then
        Break;
      inc(result);
    end;
    if (result = Count) then
    result := -1;
  end;

  function TCustomMethodList.GetItem(Index: integer): TMethod;
  begin
    Result:= FList^[Index];
  end;
*)
{$ENDREGION}

{TCustomObjEventList}

procedure TCustomObjEventList.Subscribe(const AMethod: TCustomObjEvent);
begin
  Add(AMethod);
end;

procedure TCustomObjEventList.Execute(Sender: TObject; Event: TGUID;
  Params: Pointer);
var
i : integer;
begin
  i := 0;
  while i < Count do
  begin
    try
      if assigned(TMethod(Items[i]).Code) then
        self.Items[i](Sender, Event, Params);
    finally
      inc(i);
    end;
  end;
end;

procedure TCustomObjEventList.UnSubscribe(const AMethod: TCustomObjEvent);
begin
  Remove(AMethod);
end;

{ TEventList }

procedure TEventList.Subscribe(const AMethod: TNotifyEvent);
begin
  Add(AMethod);
end;

procedure TEventList.Execute(Sender: TObject);
var
i : integer;
begin
  i := 0;
  while i < Count do
  begin
    try
      if assigned(TMethod(Items[i]).Code) then
        Items[i](Sender);
    finally
      inc(i);
    end;
  end;
end;

procedure TEventList.UnSubscribe(const AMethod: TNotifyEvent);
begin
  Remove(AMethod);
end;

{ TStrEventList }

procedure TStrEventList.Subscribe(const AMethod: TStrEvent);
begin
  Add(AMethod);
end;
  
procedure TStrEventList.Execute(Sender: TObject; const Str: string);
var
i : integer;
begin
  i := 0;
  while i < Count do
  begin
    try
      if assigned(TMethod(Items[i]).Code) then
        Items[i](Sender, Str);
    finally
      inc(i);
    end;
  end;
end;
  
procedure TStrEventList.UnSubscribe(const AMethod: TStrEvent);
begin
  Remove(AMethod);
end;


end.
