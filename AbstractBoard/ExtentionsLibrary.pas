unit ExtentionsLibrary;

interface
uses
  WinApi.Windows, System.Classes, AbstractExtention;
type
  TExtentionLibInfo = record
    iModule: HMODULE;
    ExtentionClass : TAbstractExtentionClass;
  end;
  pExtentionLibInfo = ^TExtentionLibInfo;
  TExtentionLibList = class(TList)
  protected
    function GetModuleName(const Index: Integer): String; 
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Get(const Index: Integer): TAbstractExtentionClass;
   public
    function Exists(ClassName : TAbstractExtentionClass):Boolean;
    function Add(APluginInfo : TExtentionLibInfo) : integer;
    property Items[const Index: Integer]: TAbstractExtentionClass read Get; default;
    property ModuleName[const Index: Integer]: String read GetModuleName;
    procedure FindPluginFiles; virtual; abstract;
  end;

implementation
uses System.SysUtils;

function TExtentionLibList.Add(APluginInfo : TExtentionLibInfo): integer;
var
vInfo : pExtentionLibInfo;
begin
  New(vInfo);
  vInfo^.iModule := APluginInfo.iModule;
  vInfo^.ExtentionClass := APluginInfo.ExtentionClass;
  result := inherited Add(vInfo);
end;

function TExtentionLibList.Exists(ClassName: TAbstractExtentionClass): Boolean;
var
i : Word;
begin
  Result := false;
  i := 0;
  while i < Count do
  begin
    Result := Items[i] = ClassName;
    if Result then
      Break;
    Inc(i);
  end;
end;

function TExtentionLibList.Get(const Index: Integer): TAbstractExtentionClass;
var
vInfo : pExtentionLibInfo;
begin
  result := nil;
  if (Index < Count) then
  begin 
    vInfo := pExtentionLibInfo(inherited Get(Index));
    result := vInfo.ExtentionClass;
  end else
    raise Exception.Create(Format('No extention module found whith index %d',[Index]));
end;

function TExtentionLibList.GetModuleName(const Index: Integer): String;
var
vInfo : pExtentionLibInfo;
Buffer: array[0..MAX_PATH] of Char;
begin
  if (Index > -1) and (Index < Count) then
  begin
    vInfo := pExtentionLibInfo(inherited Get(Index));
    SetString(Result, Buffer, GetModuleFileName(vInfo.iModule, Buffer, SizeOf(Buffer)));
  end;
end;


procedure TExtentionLibList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    if (pExtentionLibInfo(Ptr).iModule > 0) then
      FreeLibrary(pExtentionLibInfo(Ptr).iModule);
    Dispose(Ptr);
  end;
  inherited Notify(Ptr, Action);
end;

end.
