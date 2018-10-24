unit ChipPluginManager;

interface
uses
  WinApi.Windows, System.Classes, ChipManagerInterface, ChipAbstract, System.SysUtils;
  type
  TChipPluginInfo = record
    iModule: HMODULE;
  end;
  pChipPluginInfo = ^TChipPluginInfo;
  TChipPlugins = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
   public
    function Add(AModule: HMODULE) : integer; reintroduce;
    function Find(var AIdx : Byte; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload;
    function Find(const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload;
  end;
  TChipClassPluginInfo = class (TObject)
    iModule: HMODULE;
    ChipClass : TChipClass;
    ChipPlugins : TChipPlugins;
    constructor Create(AModule: HMODULE; AChipClass : TChipClass);
    destructor Destroy; override;
  end;
  TChipPluginManager = class(TList, IChipPluginManager)
  protected
    function IChipPluginManagerCount : Byte;stdcall;
    function IChipPluginManager.Count = IChipPluginManagerCount;
    function GetModuleName(const Index: Integer): String; stdcall;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetClass(const Index: Byte): TChipClass; stdcall;
    {IInterface}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall; 
    function ModuleAdd(AModule: HMODULE; AChipClass : TChipClass) : integer;
   public
    function IndexOf(const AGUID : TGUID) : integer;overload;stdcall;
    function IndexOf(AClass : TChipClass) : integer;overload;stdcall;
    property ItemClass[const Index: Byte]: TChipClass read GetClass; default;
    property ModuleName[const Index: Integer]: String read GetModuleName;
    procedure AsStrings(AList : TStrings); stdcall;
    procedure FindPluginFiles;
    function ChipPligunsCount(AChipIndex : Integer):byte; overload; stdcall;
    function ChipPligunsCount(const AGUID : TGUID):byte; overload; stdcall;
    function ChipPligunsCount(AClass : TChipClass):byte; overload; stdcall;
    function FindChipPligunFunction(AChipIndex : Integer; var AIdx : Byte; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(AChipIndex : Integer; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(const AGUID : TGUID; var AIdx : Byte; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(const AGUID : TGUID; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(AClass : TChipClass; var AIdx : Byte; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
    function FindChipPligunFunction(AClass : TChipClass; const AFunctionName : string; out oProcAddr : Pointer):Boolean;overload; stdcall;
  end;


implementation

uses
  System.IniFiles;

type
TChipClassFunct = function : TChipClass; stdcall;
TPackageLoad = procedure;
TPackageUnload = procedure;

{ TChipPluginManager }  
function TChipPluginManager.ChipPligunsCount(AChipIndex: Integer): byte;
var
vInfo : TChipClassPluginInfo;
begin
  Result :=0;
  if (AChipIndex > -1) and (AChipIndex < Count) then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(AChipIndex));
    if Assigned(vInfo.ChipPlugins) then
      Result := Byte(vInfo.ChipPlugins.Count);
  end;
end;

function TChipPluginManager.ChipPligunsCount(const AGUID: TGUID): byte;
var
vChipIdx : integer;
vInfo : TChipClassPluginInfo;
begin
  Result :=0;
  vChipIdx := IndexOf(AGUID);
  if vChipIdx <> -1 then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(vChipIdx));
    if Assigned(vInfo.ChipPlugins) then
      Result := Byte(vInfo.ChipPlugins.Count);
  end;
end;

function TChipPluginManager.ChipPligunsCount(AClass: TChipClass): byte;
var
vChipIdx : integer;
vInfo : TChipClassPluginInfo;
begin
  Result :=0;
  vChipIdx := IndexOf(AClass);
  if vChipIdx <> -1 then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(vChipIdx));
    if Assigned(vInfo.ChipPlugins) then
      Result := Byte(vInfo.ChipPlugins.Count);
  end;
end;

function TChipPluginManager.FindChipPligunFunction(const AGUID: TGUID; var AIdx: Byte; const AFunctionName: string; out oProcAddr: Pointer): Boolean;
var
vChipIdx : integer;
vInfo : TChipClassPluginInfo;
begin
  vChipIdx := IndexOf(AGUID);
  Result := vChipIdx <> -1;
  if Result then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(vChipIdx));
    Result := Assigned(vInfo.ChipPlugins) and vInfo.ChipPlugins.Find(AIdx, AFunctionName, oProcAddr);
  end;
end;

function TChipPluginManager.FindChipPligunFunction(AChipIndex: Integer; const AFunctionName: string; out oProcAddr: Pointer): Boolean;
var
vInfo : TChipClassPluginInfo;
begin
  Result := (AChipIndex > -1) and (AChipIndex < Count);
  if Result then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(AChipIndex));
    Result := Assigned(vInfo.ChipPlugins) and vInfo.ChipPlugins.Find(AFunctionName, oProcAddr);
  end;
end;

function TChipPluginManager.FindChipPligunFunction(AChipIndex: Integer; var AIdx: Byte; const AFunctionName: string; out oProcAddr: Pointer): Boolean;
var
vInfo : TChipClassPluginInfo;
begin
  Result := (AChipIndex > -1) and (AChipIndex < Count);
  if Result then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(AChipIndex));
    Result := Assigned(vInfo.ChipPlugins) and vInfo.ChipPlugins.Find(AIdx, AFunctionName, oProcAddr);
  end;
end;

function TChipPluginManager.FindChipPligunFunction(const AGUID: TGUID; const AFunctionName: string; out oProcAddr: Pointer): Boolean;
var
vChipIdx : integer;
vInfo : TChipClassPluginInfo;
begin
  vChipIdx := IndexOf(AGUID);
  Result := vChipIdx <> -1;
  if Result then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(vChipIdx));
    Result := Assigned(vInfo.ChipPlugins) and vInfo.ChipPlugins.Find(AFunctionName, oProcAddr);
  end;
end;

function TChipPluginManager.FindChipPligunFunction(AClass: TChipClass; const AFunctionName: string; out oProcAddr: Pointer): Boolean;
var
vChipIdx : integer;
vInfo : TChipClassPluginInfo;
begin
  vChipIdx := IndexOf(AClass);
  Result := vChipIdx <> -1;
  if Result then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(vChipIdx));
    Result := Assigned(vInfo.ChipPlugins) and vInfo.ChipPlugins.Find(AFunctionName, oProcAddr);
  end;
end;

function TChipPluginManager.FindChipPligunFunction(AClass: TChipClass; var AIdx: Byte; const AFunctionName: string; out oProcAddr: Pointer): Boolean;
var
vChipIdx : integer;
vInfo : TChipClassPluginInfo;
begin
  vChipIdx := IndexOf(AClass);
  Result := vChipIdx <> -1;
  if Result then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(vChipIdx));
    Result := Assigned(vInfo.ChipPlugins) and vInfo.ChipPlugins.Find(AIdx, AFunctionName, oProcAddr);
  end;
end;


procedure TChipPluginManager.FindPluginFiles;
var
  searchResult : TSearchRec;
  vModule: HMODULE;
  vChipClass : TChipClass;
  GetClassFunc : TChipClassFunct;
  vCurrPath : string;
begin
  vCurrPath := ExtractFilePath(ParamStr(0));
  if FindFirst(vCurrPath+'*.bpl', faAnyFile, searchResult) = 0 then
  begin
    repeat
      try
        vModule := SafeLoadLibrary(vCurrPath + searchResult.Name,
        SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
        if (vModule > 0) then
        begin
          @GetClassFunc := GetProcAddress(vModule, 'GetChipClass');
          if Assigned(GetClassFunc) then
          begin
            vChipClass := GetClassFunc;
            if (IndexOf(vChipClass.ChipGUID) = -1) then
              ModuleAdd(vModule, vChipClass)
            else
             FreeLibrary(vModule);
          end else
            FreeLibrary(vModule);
        end;
      except
        if (vModule > 0) then
          FreeLibrary(vModule);
        Continue;
      end;
    until FindNext(searchResult) <> 0;
    FindClose(searchResult);
  end;
end;

function TChipPluginManager.ModuleAdd(AModule: HMODULE; AChipClass : TChipClass): integer;
begin
  result := inherited Add(Pointer(TChipClassPluginInfo.Create(AModule, AChipClass)));
end;

procedure TChipPluginManager.AsStrings(AList: TStrings);
var i : integer;
begin
  i := 0;
  AList.BeginUpdate;
  try
    AList.Clear;
    while (i < self.Count) do
    begin
      AList.Add(ItemClass[i].ChipName);
      inc(i);
    end;
  finally
    AList.EndUpdate;
  end;
end;

function TChipPluginManager.GetClass(const Index: Byte): TChipClass;
var vInfo : TChipClassPluginInfo;
begin
  result := nil;
  if (Index < Count) then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(Index));
    result := vInfo.ChipClass;
  end else
    raise Exception.Create(Format('No chip module found whith index %d',[Index]));
end;

function TChipPluginManager.GetModuleName(const Index: Integer): String;
var vInfo : TChipClassPluginInfo;
Buffer: array[0..MAX_PATH] of Char;
begin
  if (Index > -1) and (Index < Count) then
  begin
    vInfo := TChipClassPluginInfo(inherited Get(Index));
    SetString(Result, Buffer, GetModuleFileName(vInfo.iModule, Buffer, SizeOf(Buffer)));
  end;
end;

function TChipPluginManager.IChipPluginManagerCount: Byte;
begin
  Result := Byte(Count);
end;

function TChipPluginManager.IndexOf(AClass: TChipClass): integer;
begin
  result := IndexOf(AClass.ChipGUID);
end;

function TChipPluginManager.IndexOf(const AGUID: TGUID): integer;
begin
  result := 0;
  while (result < self.Count) do
  begin
    if IsEqualGUID(ItemClass[Byte(result)].ChipGUID, AGUID) then
      Break;
    inc(result);
  end;
  if (result = self.Count) then
    result := -1;
end;

procedure TChipPluginManager.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TChipClassPluginInfo(Ptr).Free;
  inherited Notify(Ptr, Action);
end;


function TChipPluginManager.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TChipPluginManager._AddRef: Integer;
begin
  Result := -1;
end;

function TChipPluginManager._Release: Integer;
begin
  Result := -1;
end;

{ TChipPluginInfo }

constructor TChipClassPluginInfo.Create(AModule: HMODULE; AChipClass : TChipClass);
var
vPluginsCount, vPluginsIdx : Integer;
vFileName : string;
vPluginModule : HMODULE;
vCurrPath : string; 
vLoadProc :TPackageLoad;
f : TIniFile;
vSt : TStringList;
begin
  inherited Create;
  iModule := AModule;
  ChipClass := AChipClass;
  ChipPlugins := nil;
  f := TIniFile.Create(ExtractFilePath(ParamStr(0))+AChipClass.ChipName+'.ini');
  vSt := TStringList.Create;
  try
    f.ReadSection('PluginsInfo', vSt);
    vPluginsCount := vSt.Count;
    if (vPluginsCount > 0) then
    begin
      ChipPlugins := TChipPlugins.Create;
      vPluginsIdx := 0;
      vCurrPath := ExtractFilePath(ParamStr(0));
      while vPluginsIdx < vPluginsCount do
      begin
        try
          vFileName := f.ReadString('PluginsInfo',vSt[vPluginsIdx],'');
          if FileExists(vCurrPath + vFileName) then
          begin
            vPluginModule := SafeLoadLibrary(vCurrPath + vFileName,
            SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
            try
              if vPluginModule > 0 then
              begin
                @vLoadProc := GetProcAddress(vPluginModule, 'Initialize');
                if Assigned(vLoadProc) then
                  vLoadProc;
                ChipPlugins.Add(vPluginModule);
              end;
            except
              if vPluginModule > 0 then
              begin
                FreeLibrary(vPluginModule);
                vPluginModule := 0;
              end;
            end;
          end;
        finally
          Inc(vPluginsIdx);
        end;
      end;
      if ChipPlugins.Count < 1 then
         FreeAndNil(ChipPlugins);
    end;
  finally
    FreeAndNil(f);
    FreeAndNil(vSt);
  end;
end;

destructor TChipClassPluginInfo.Destroy;
begin
  if Assigned(ChipPlugins) then
  begin
    ChipPlugins.Clear;
    FreeAndNil(ChipPlugins);
  end;
  if (iModule > 0) then
    FreeLibrary(iModule);
  inherited Destroy;
end;


{ TChipPlugins }


function TChipPlugins.Add(AModule: HMODULE): integer;
var
vPl : pChipPluginInfo;
begin
  New(vPl);
  vPl.iModule := AModule;
  Result := inherited Add(vPl);
end;

function TChipPlugins.Find(var AIdx : Byte; const AFunctionName: string; out oProcAddr : Pointer): Boolean;
var
vPl : pChipPluginInfo;
vIdx : Byte;
begin
  Result := (Count > 0) and (AIdx < Count);
  if not Result then
    Exit;
  vIdx := AIdx;
  while vIdx < Count do
  begin
    vPl := pChipPluginInfo(Get(vIdx));
    if Assigned(vPl) then
    begin
      oProcAddr := GetProcAddress(vPl.iModule, LPCWSTR(AFunctionName){PWideChar(AFunctionName)});
      Result := Assigned(oProcAddr);
      if Result then
      begin
        AIdx := vIdx;
        Break;
      end;
    end;
    Inc(vIdx);
  end;
  if not Result then
    oProcAddr := nil;
end;

function TChipPlugins.Find(const AFunctionName: string; out oProcAddr : Pointer): Boolean;
var
vIdx : Byte;
begin
  vIdx := 0;
  Result := Find(vIdx, AFunctionName, oProcAddr);
end;

procedure TChipPlugins.Notify(Ptr: Pointer; Action: TListNotification);
var
vPl : pChipPluginInfo;
vUnloadProc :TPackageUnload;
begin
  if Action = lnDeleted then
  begin
    vPl := pChipPluginInfo(Ptr);
    if (vPl.iModule > 0) then
    begin
      @vUnloadProc := GetProcAddress(vPl.iModule, 'Finalize');
      if Assigned(vUnloadProc) then
        vUnloadProc;
      FreeLibrary(vPl.iModule);
    end;
    Dispose(Ptr);
  end;
  inherited Notify(Ptr, Action);
end;

end.
