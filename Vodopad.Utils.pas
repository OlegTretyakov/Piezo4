unit Vodopad.Utils;


interface
uses System.Classes;

{function SupportsIF(const Instance: IInterface; const IID: TGUID; out Intf): Boolean; overload;
function SupportsIF(const Instance: TObject; const IID: TGUID; out Intf): Boolean; overload;
function SupportsIF(const Instance: IInterface; const IID: TGUID): Boolean; overload;
function SupportsIF(const Instance: TObject; const IID: TGUID): Boolean; overload; // Unsafe
function SupportsIF(const AClass: TClass; const IID: TGUID): Boolean; overload; }

procedure ClearStringsObjects(AStrings : TStrings);
procedure ClearStringsPointers(AStrings : TStrings);
function ArrayEof(idx : word; const Arr : TBoundArray): boolean; overload;
function ArrayEof(idx : word; const Arr : array of integer): boolean; overload;
function ArrayEof(idx : word; const Arr : array of Word): boolean; overload;

implementation

procedure ClearStringsObjects(AStrings : TStrings);
var i : integer;
begin
  i := 0;
  while i < AStrings.Count do
  begin
    if assigned(AStrings.Objects[i])
    and (AStrings.Objects[i] is TObject) then
      AStrings.Objects[i].Free;
    inc(i);
  end;
end;

procedure ClearStringsPointers(AStrings : TStrings);
var i : integer;
vObj : Pointer;
begin
  i := 0;
  while i < AStrings.Count do
  begin
    if assigned(AStrings.Objects[i]) then
    begin
      vObj := Pointer(AStrings.Objects[i]);
      Dispose(vObj);
    end;
    inc(i);
  end;
end;

{$B-} 

function ArrayEof(idx : word; const Arr : TBoundArray): boolean; overload;
begin
  result := (Length(Arr) = 0) or (idx > high(Arr));
end;

function ArrayEof(idx : word; const Arr : array of integer): boolean; overload;
begin
  result := (Length(Arr) = 0) or (idx > high(Arr));
end;

function ArrayEof(idx : word; const Arr : array of Word): boolean; overload;
begin
  result := (Length(Arr) = 0) or (idx > high(Arr));
end;

{ Interface support routines }

{function SupportsIF(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

function SupportsIF(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
var
  LUnknown: IUnknown;
begin
  Result := (Instance <> nil) and
            ((Instance.GetInterface(IUnknown, LUnknown) and SupportsIF(LUnknown, IID, Intf)) or
             Instance.GetInterface(IID, Intf));
  LUnknown := nil;
end;

function SupportsIF(const Instance: IInterface; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result := SupportsIF(Instance, IID, Temp);
  Temp := nil;
end;

function SupportsIF(const Instance: TObject; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  // NOTE: Calling this overload on a ref-counted object that has REFCOUNT=0
  // will result in it being freed upon exit from this routine.
  Result := SupportsIF(Instance, IID, Temp);
  Temp := nil;
end;

function SupportsIF(const AClass: TClass; const IID: TGUID): Boolean;
begin
  Result := AClass.GetInterfaceEntry(IID) <> nil;
end; }

end.