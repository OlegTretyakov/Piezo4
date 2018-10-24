unit mbPluginManager;

interface
  uses WinApi.Windows, System.Classes, System.SysUtils, DeviceModule, mbPluginManagerInterface;
  type
  TMBPluginInfo =  packed record
    iModule: HMODULE;
    ModuleClass : TDeviceModuleClass;
    ID,
    Version : Word;
  end;
  pMBPluginInfo = ^TMBPluginInfo;
  TMBPM = class abstract (TList, IInterface, IMBPM)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Get(const Index: Integer): pMBPluginInfo;  
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall; 
    function IndexByIDVer(ID, Version : Word) : integer;
    function Add(APluginInfo : TMBPluginInfo) : integer;
   public
    property Items[const Index: Integer]: pMBPluginInfo read Get; default;
    procedure FindPluginFiles; virtual; abstract;
    function FindModuleClass(ID, Version : Word; var oModuleClass : TDeviceModuleClass):Boolean;stdcall;
  end;

implementation

{ TMBPM }



function TMBPM.Add(APluginInfo : TMBPluginInfo): integer;
var vInfo : pMBPluginInfo;
begin
  New(vInfo);
  vInfo^.iModule := APluginInfo.iModule;
  vInfo^.ModuleClass := APluginInfo.ModuleClass;
  vInfo^.ID := APluginInfo.ID;
  vInfo^.Version :=  APluginInfo.Version;
  result := inherited Add(vInfo);
end;

function TMBPM.Get(const Index: Integer): pMBPluginInfo;
begin
  result := nil;
  if (Index < Count) then
    result := pMBPluginInfo(inherited Get(Index));
end;

{function TMBPM.GetModuleName(const Index: Integer): Shortstring;
var
Buffer: array[0..MAX_PATH] of Char;
begin
  if (Index > -1) and (Index < Count) then
    SetString(Result, Buffer, GetModuleFileName(Items[Index].iModule, Buffer, SizeOf(Buffer)));
end;  }

function TMBPM.IndexByIDVer(ID, Version : Word): integer;
var
vItem : pMBPluginInfo;
begin
  result := 0;
  while result < self.Count do
  begin
    vItem := Items[result];
    if (vItem.ID = ID)
    and (vItem.Version = Version) then
      Break;
    inc(result);
  end;
  if (result = self.Count) then
    result := -1;
end;

function TMBPM.FindModuleClass(ID, Version: Word; var oModuleClass: TDeviceModuleClass): Boolean;
var
vIdx : Integer;
begin
  vIdx := IndexByIDVer(ID, Version);
  Result := (vIdx <> -1);
  if Result then
  begin
    oModuleClass := Get(vIdx).ModuleClass;
    Result := Assigned(oModuleClass);
  end;
end;

procedure TMBPM.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    if (pMBPluginInfo(Ptr).iModule > 0) then
      FreeLibrary(pMBPluginInfo(Ptr).iModule);
    Dispose(Ptr);
  end;
  inherited Notify(Ptr, Action);
end;


function TMBPM.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TMBPM._AddRef: Integer;
begin
  Result := -1;
end;

function TMBPM._Release: Integer;
begin
  Result := -1;
end;

end.
