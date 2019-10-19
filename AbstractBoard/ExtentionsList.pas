unit ExtentionsList;

interface
  uses
  System.Classes,
  Vodopad.ObjectList,
  AbstractExtention,
  ExtentionsListInterface;

  type

   TExtentions = class(TComponent, IExtentions)
   private  
    fExtList : TExObjectList;
    procedure OnBeforeDelete(Sender: TObject; AIndex : integer);
    procedure OnAfterDelete(Sender: TObject; AIndex : integer);
    procedure OnAfterAdd(Sender: TObject; AIndex : integer);
    function GetItem(AIndex : Word):TAbstractExtention;stdcall;
   public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Install(const AExtentionClass : TAbstractExtentionClass):Boolean; overload; stdcall;
    function Install(const AExtentionClass : TAbstractExtentionClass; var AExtention):Boolean;  overload; stdcall;
    function Find(var AExtIdx : Word; IID : TGUID):boolean; overload; stdcall;
    function Find(var AExtIdx : Word; IID : TGUID; out Obj):boolean; overload; stdcall;
    function Find(IID : TGUID; out Obj):boolean; overload;stdcall;
    function Find(const AExtentionClass : TAbstractExtentionClass;
                            var AExtention):boolean;overload; stdcall;
    function Find(var AExtIdx : Word; const AExtentionClass : TAbstractExtentionClass;
                            var AExtention):boolean; overload; stdcall;
    function Find(var AExtIdx : Word;
                    const AExtentionClass : TAbstractExtentionClass):boolean; overload;stdcall;
    function Count : Word; stdcall;
    property Item[AIndex : Word]:TAbstractExtention read GetItem; default;
    procedure Delete(const AExtentionClass : TAbstractExtentionClass); stdcall;
    procedure Clear; stdcall;
  end;


implementation

uses
  System.SysUtils;

{ TExtentions }  

constructor TExtentions.Create(AOwner : TComponent);
var
  vTmp : IExtentionsEventSink;
begin
  inherited Create(AOwner);
  fExtList := TExObjectList.Create;
  if Supports(AOwner, IExtentionsEventSink, vTmp) then
  begin
    fExtList.OnBeforeDeleteObject := OnBeforeDelete;
    fExtList.OnAfterDeleteObject := OnAfterDelete;
    fExtList.OnAfterAddObject := OnAfterAdd;
  end;
  vTmp := nil;
end;  

destructor TExtentions.Destroy;
begin
  Clear;
  FreeAndNil(fExtList);
  inherited Destroy;
end;  

function TExtentions.Find(var AExtIdx: Word; IID: TGUID; out Obj): boolean;
var
  vExtIdx : word;
begin
  vExtIdx := AExtIdx;
  while (vExtIdx < fExtList.Count) do
  begin  
    Pointer(Obj) := nil;
    if Supports(fExtList.Items[vExtIdx], IID, Obj) then
      Break;
    inc(vExtIdx);
  end;
  result := (vExtIdx < fExtList.Count);
  if result then
    AExtIdx := vExtIdx;
end;

function TExtentions.Count: Word;
begin
  result := fExtList.Count;
end;

function TExtentions.Install(const AExtentionClass: TAbstractExtentionClass;
  var AExtention): Boolean;
var
  vExtIdx : word;
begin
  vExtIdx := 0;
  result := not Find(vExtIdx, AExtentionClass, AExtention);
  if result then
  begin
    TAbstractExtention(AExtention) := AExtentionClass.Create(Self);
    fExtList.Add(TObject(AExtention));
   (* {$IFDEF VD7LOG}
      fBoard.DoLog(lDebug,
      format('BoardExtentions Installed Extention %s',
      [TAbstractExtention(fExtList.Last).ClassName]));
    {$ENDIF}  *)
  end;
end;
 
function TExtentions.Install(const AExtentionClass: TAbstractExtentionClass):boolean;
var
  vExtention: TAbstractExtention;
begin
  result := Install(AExtentionClass, vExtention);
end;

procedure TExtentions.Delete(const AExtentionClass: TAbstractExtentionClass);
var
  vExtIdx : word;
begin
  vExtIdx := 0;
  while Find(vExtIdx, AExtentionClass) do
  begin
    fExtList.Delete(vExtIdx);
    vExtIdx := 0;
  end;
end;

procedure TExtentions.Clear;
begin
  fExtList.Clear;
end;

function TExtentions.Find(var AExtIdx: Word;
  const AExtentionClass: TAbstractExtentionClass; var AExtention): boolean;
begin
  result := Find(AExtIdx, AExtentionClass);
  if result then
    TAbstractExtention(AExtention) := TAbstractExtention(fExtList.Items[AExtIdx]);
end;

function TExtentions.Find(var AExtIdx: Word;
  const AExtentionClass: TAbstractExtentionClass): boolean;
var
  vExtIdx : word;
begin
  vExtIdx := AExtIdx;
  while (vExtIdx < fExtList.Count) do
  begin
    if (fExtList.Items[vExtIdx] is AExtentionClass) then
      Break;
    inc(vExtIdx);
  end;
  result := (vExtIdx < fExtList.Count);
  if result then
    AExtIdx := vExtIdx;
end;

function TExtentions.GetItem(AIndex: Word): TAbstractExtention;
begin
  result := TAbstractExtention(fExtList.Items[AIndex]);
end;

function TExtentions.Find(var AExtIdx: Word; IID: TGUID): boolean;
var
  vTmp : IInterface;
begin
  Result := Find(AExtIdx, IID, vTmp);
  vTmp := nil;
end;

function TExtentions.Find(IID: TGUID; out Obj): boolean;
var
  vExtIdx : word;
begin
  vExtIdx := 0;
  Result := Find(vExtIdx, IID, Obj);
end;

function TExtentions.Find(const AExtentionClass: TAbstractExtentionClass;
                            var AExtention): boolean;
var
  vExtIdx : Word;
begin
  vExtIdx := 0;
  result := Find(vExtIdx, AExtentionClass, AExtention);
end;

procedure TExtentions.OnAfterAdd(Sender: TObject; AIndex: integer);
var
  vEventSink : IExtentionsEventSink;
begin
  if Supports(Owner, IExtentionsEventSink, vEventSink) then
    vEventSink.OnAfterAdd(Self, AIndex);
  vEventSink := nil;
end;

procedure TExtentions.OnAfterDelete(Sender: TObject; AIndex: integer);
var
  vEventSink : IExtentionsEventSink;
begin
  if Supports(Owner, IExtentionsEventSink, vEventSink) then
    vEventSink.OnAfterDelete(Self, AIndex);
  vEventSink := nil;
end;

procedure TExtentions.OnBeforeDelete(Sender: TObject; AIndex: integer);
var
  vEventSink : IExtentionsEventSink;
begin
  if Supports(Owner, IExtentionsEventSink, vEventSink) then
    vEventSink.OnBeforeDelete(Self, AIndex);
  vEventSink := nil;
end;

end.
