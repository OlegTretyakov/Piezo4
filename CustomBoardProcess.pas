unit CustomBoardProcess;

interface

  uses WinApi.Windows, Winapi.Messages, System.Classes, System.SysUtils, System.Threading,
  ExtentionsList, BoardProcessInterface, AbstractBoard, AbstractProtocol, AbstractExtention, ExtentionsLibrary,
  EventBusInterface, Vodopad.EventList, Vodopad.ObjectList, CustomChildProcess,
  ManualControlFormAccessInterface, LoggerInterface, Vodopad.Timer;

  type
  TMCLib = class(TObject)
    strict private
    FWindowHandle: HWND;
    procedure FreeFormLibrary;
    procedure MessageHandler(var message : TMessage);
  public
    FormLibrary : HMODULE;
    FormInstance : TComponent;
    procedure OnControlFormDestroy(Sender: TObject);
    constructor Create();
    destructor Destroy;override;
  end;
  TCustomBoardProcess = class(TCustomChildProcess,
                              IBoardProcess,
                              IEventBus,
                              ICreateManualControlForm,
                              IBoardProcessLog)
  private
    fProtocol : TAbstractProtocolClass;
    fProticolLib : HMODULE;
    fSearchTimer, fExtractTmpTimer : TvdTimer;
    fFormLib : TMCLib;
    fExtentions : TExtentions;
    fBoards : TExObjectList;
    fTmpBoardsList : TExObjectList;
    fEventSubscribers : TCustomObjEventList;
    procedure StartSearchInternal(Sender : TObject);
    procedure OnExtreactTmp(Sender : TObject);
    procedure SearchTimeOut(Sender : TObject);
    procedure InitProtocolLib;
    procedure DoneProtocolLib;
    procedure UpdateBoardConfig(ABoard: TAbstractBoard);
    procedure CallOnSearchComplete;
  protected   
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    procedure BoardEvent(Sender : TObject; Event : TGUID; Params : Pointer); virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    {IBoardProcess}
    function GetItems(AIndex : byte): TObject; stdcall;
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : EventBusInterface.TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: EventBusInterface.TCustomObjEvent); stdcall;
    function GetManualControlFormClass(out oModule:HMODULE; out oFormClass : TComponentClass):Boolean;virtual;
    {ICreateManualControlForm}
    function ICreateManualControlForm.Create = ICreateManualControlForm_Create;
    function ICreateManualControlForm_Create(var alreadyExists : boolean) : TComponent;stdcall;
    property EventSubscribers : TCustomObjEventList read fEventSubscribers;
  public
    procedure Log(ALevel: TLogInfo; const Text: WideString;  Instance: TObject=nil);stdcall;
    function LevelEnabled(ALevel : TLogInfo): boolean; stdcall;
    {IBoardProcess}
    procedure ForEachBoard(AMethod : TForEachBoardMethod; Params : Pointer=nil); stdcall;
    procedure StartSearch; stdcall;
    function BoardsCount : byte; stdcall;
    function PositionsCount: cardinal; stdcall;
    function ActivePositionsCount: cardinal; stdcall;
    procedure DeleteBoard(AIndex : byte); virtual; stdcall;
    procedure Clear; virtual; stdcall;
    property Board[index : byte] : TObject read GetItems; default;
    function IndexOf(const ABoard : TObject) : integer; overload;
    function IndexOf(const ABoardSN : word) : integer; overload;
    procedure DeleteInactivePositions;stdcall;
    procedure SetAllPositionsToState(AActive : boolean);stdcall;
  end;

implementation

uses
System.IniFiles,
AbstractMainProcessInterface,
AbstractBoardInterface,
RS232Phisycal, RS232Utils,
PositionListInterface,
PositionInterface,
FormsControllerInterface;


type
  TPackageUnload = procedure;
  TPackageLoad = procedure;

var
vgBoardLogger : ILogger = nil;

function InitLogger(AFunct : pLoggerFunct; out Obj):boolean;
var
vConfig : pLogClassConfig;
begin
  Result := False;
  if not Assigned(AFunct) then
    Exit;
  New(vConfig);
  try
    vConfig.ConfigGUID := StringToGUID('{50581E3A-4D39-4EB9-B3C5-45766BB22644}');
    vConfig.EnabledLevels := [lInfo, lEvent, lDebug, {lPortWrite,} lPortRead, lTrace, lWarning, lError,
                              lException, lExceptionOS];
    vConfig.DefaultLevelsView := [lInfo, lEvent, lDebug, lTrace, lWarning, lError, lException, lExceptionOS];
    vConfig.FileExtension := 'brdlog';
    vConfig.ArhiveExtension := 'brdlog7z';
    vConfig.ModuleName := 'Платы';
    try
      Result := AFunct.AddLogger(vConfig, Obj);
    except
      Result := false;
    end;
  finally
    Dispose(vConfig);
  end;
end;


function GetProtocolClass(AMainProcess : TObject; out AClass:TAbstractProtocolClass):HMODULE;
var
vCC : IConfigController;
vBuildFile : TMemIniFile;
vLibFileName : TFileName;
func:  function: TAbstractProtocolClass; stdcall;
begin
  Result := 0;
  if not Supports(AMainProcess, IConfigController, vCC) then
    Exit;
  vBuildFile := TMemIniFile.Create(''{, TEncoding.UTF8});
  try
    vCC.Load('Software', vBuildFile);
    try
      if vBuildFile.ValueExists('BuildConfiguration','BoardProtocolPlugin') then
      begin
        vLibFileName := ExtractFilePath(ParamStr(0))+vBuildFile.ReadString('BuildConfiguration','BoardProtocolPlugin','');
        if not FileExists(vLibFileName) then
          Exit;
        Result := SafeLoadLibrary(vLibFileName,
          SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
        if Result = 0 then
          Exit;
        @func := GetProcAddress(Result, 'GetBoardProtocolClass');
        if not Assigned(func) then
        begin
          FreeLibrary(Result);
          Result := 0;
          Exit;
        end;
        AClass := func;
      end;
    except
      if Result<>0 then
      begin
        FreeLibrary(Result);
        Result := 0;
      end;
    end;
  finally
    FreeAndNil(vBuildFile);
    vCC := nil;
  end;
end;

{TCustomBoardProcess}

{$REGION ' internal types '}
type
TSetAllStateParams = record
  Active : Boolean;
end;
pSetAllStateParams = ^TSetAllStateParams;
{$ENDREGION}

{$REGION ' internal functions '}
procedure InternalDeleteInactivePositions(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vBoard : IAbstractBoard;
vPosList : IPositionsList;
vPosition : IPosition;
i : Word;
begin
  if Supports(ABoard, IAbstractBoard, vBoard)
   and vBoard.QueryPositionListInterface(IPositionsList, vPosList)
   and (vPosList.Count > 0) then
  begin
    for I := vPosList.Count - 1 downto 0 do
    begin
      if vPosList.QueryItem(i, IPosition, vPosition) then
      begin
        if not vPosition.Active then
         vPosList.Delete(i);
        vPosition := nil;
      end else
        vPosList.Delete(i);
    end;
  end;
  vPosList := nil;
  vBoard := nil;
end;

procedure InternalSetAllPositionsToState(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vBoard : IAbstractBoard;
vPosList : IPositionsList;
vPosition : IPosition;
i : Word;
vParams : pSetAllStateParams;
begin
  if not Assigned(Params) then
    Exit;
  vParams := pSetAllStateParams(Params);
  if Supports(ABoard, IAbstractBoard, vBoard)
   and vBoard.QueryPositionListInterface(IPositionsList, vPosList)
   and (vPosList.Count > 0) then
  begin
    for I := 0 to vPosList.Count - 1 do
    begin
      if vPosList.QueryItem(i, IPosition, vPosition) then
        vPosition.Active := vParams.Active;
      vPosition := nil;
    end;
  end;
  vPosList := nil;
  vBoard := nil;
end;

{$ENDREGION}



constructor TMCLib.Create;
begin
  FWindowHandle := System.Classes.AllocateHWND(MessageHandler);
  FormInstance:= nil;
end;

destructor TMCLib.Destroy;
var
vFormOnDestroyEvent : IFormOnDestroyEvent;
begin
  if Supports(FormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
  begin
    vFormOnDestroyEvent.OnDestroy := nil;
    vFormOnDestroyEvent := nil;
  end;

  FreeFormLibrary;
  FormInstance := nil;
  System.Classes.DeallocateHWND(FWindowHandle);
  inherited Destroy;
end;

procedure TMCLib.FreeFormLibrary;
var
vUnloadProc :TPackageUnload;
begin
  if (FormLibrary > 0) then
  begin
    @vUnloadProc := GetProcAddress(FormLibrary, 'Finalize');
    if Assigned(vUnloadProc) then
       vUnloadProc;
    FreeLibrary(FormLibrary);
  end;
  FormLibrary := 0;
end;

procedure TMCLib.MessageHandler(var message: TMessage);
begin
  if message.Msg = WM_USER+8 then
   FreeFormLibrary;
end;

procedure TMCLib.OnControlFormDestroy(Sender: TObject);
begin
  FormInstance := nil;
  Winapi.Windows.PostMessage(FWindowHandle, WM_USER+8, 0, 0);
end;

procedure TCustomBoardProcess.AfterCreate;
begin
  fSearchTimer := TvdTimer.Create(self);
  fExtractTmpTimer := TvdTimer.Create(self);
  fFormLib := TMCLib.Create;
  InitLogger(fFunct, vgBoardLogger);
  fBoards := TExObjectList.Create;
  fTmpBoardsList := TExObjectList.Create;
  fEventSubscribers := TCustomObjEventList.Create;
  fExtentions := TExtentions.Create(self);
  fProticolLib := 0;
  fProtocol := nil;
end;

procedure TCustomBoardProcess.BeforeDestroy;
begin
  fExtentions.Clear;
  self.clear;
  FreeAndNil(fBoards);
  FreeAndNil(fTmpBoardsList);
  FreeAndNil(fEventSubscribers);
  DoneProtocolLib;
  vgBoardLogger := nil;
  FreeAndNil(fFormLib);
end;

procedure TCustomBoardProcess.EventMethodAdd(const AMethod: EventBusInterface.TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TCustomBoardProcess.EventMethodRemove(const AMethod: EventBusInterface.TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

procedure TCustomBoardProcess.ForEachBoard(AMethod: TForEachBoardMethod; Params : Pointer);
var
vIdx : Byte;
begin
  if (fBoards.Count < 1) or (not Assigned(AMethod)) then
    Exit;
  for vIdx := fBoards.Count-1 downto  0 do
     AMethod(Board[vIdx], vIdx, Params);
end;

procedure TCustomBoardProcess.Clear;
begin
  try
    while (BoardsCount > 0) do
      DeleteBoard(BoardsCount - 1);
  except
  end;
end;

procedure TCustomBoardProcess.DeleteBoard(AIndex: byte);
var
vParams : pCBPEventParams;
vIndex : pByte;
vSN : Word;
begin
  if AIndex >=fBoards.Count then
    Exit;
  New(vParams);
  New(vIndex);
  try
    vParams.Board := Board[AIndex];
    vSN := TAbstractBoard(vParams.Board).SerialNum;
    if Assigned(vgBoardLogger) then
      vgBoardLogger.Log(lDebug, Format('Delete board index:%d, SN:%d',[AIndex, vSN]));
    vIndex^ := AIndex;
    vParams.Params := vIndex;
    fEventSubscribers.Execute(Self, C_BeforeBoardDelete, vParams);
    fBoards.Delete(AIndex);
    vParams.Board := nil;
    if Assigned(vgBoardLogger) then
      vgBoardLogger.Log(lDebug, Format('Board index:%d, SN:%d - deleted',[AIndex, vSN]));
  finally
    Dispose(vParams);  
    Dispose(vIndex);
    if (fBoards.Count < 1) then
    begin
      {fPosListMandatoryExtentions.Clear;
      fBoardMandatoryExtentions.Clear; }
      DoneProtocolLib;
    end;
  end;
end;

procedure TCustomBoardProcess.DeleteInactivePositions;
begin
  ForEachBoard(InternalDeleteInactivePositions, nil);
end;

procedure TCustomBoardProcess.SetAllPositionsToState(AActive: boolean);
var
vParams : pSetAllStateParams;
begin
  New(vParams);
  try
    vParams.Active := AActive;
    ForEachBoard(InternalSetAllPositionsToState, vParams);
  finally
    Dispose(vParams);
  end;
end;

procedure TCustomBoardProcess.CallOnSearchComplete;
begin
  fEventSubscribers.Execute(Self, C_SearchComplete, nil);
end;

procedure TCustomBoardProcess.BoardEvent(Sender : TObject; Event : TGUID; Params : Pointer);
var
vIndex : integer;
vPlgIdx : Word;
vParams : pCBPEventParams;
vExtClasses : TExtentionsClasses;
vSysInfo : pSysInfoStruct;
vPortIf : IRS232Phisycal;
begin
  New(vParams);
  try
    vParams.Board := Sender;
    vParams.Params := Params;
    if IsEqualGUID(Event, C_OnConnected) then
    begin
      vIndex := IndexOf(vParams.Board);
      if (vIndex = -1) then
      begin
        if (fTmpBoardsList.IndexOf(vParams.Board) <> -1) then
          fBoards.Add(fTmpBoardsList.Extract(vParams.Board))
        else
          fBoards.Add(vParams.Board);
        New(vSysInfo);
        try
          TAbstractBoard(vParams.Board).Protocol.FillSysInfo(vSysInfo);
          TAbstractBoard(vParams.Board).Positions.MaxCount := vSysInfo.MaxPositionsCount;
           Log(
              lEvent, Format('Board connected. '+
              'Model Code:%d; Protocol description:%s; '+
              'Hardware version:%d; Firmware version:%d; SN:%d',
              [vSysInfo.ModelCode, vSysInfo.ProtocolDestription,
              vSysInfo.HwVer, vSysInfo.FwVer, vSysInfo.Serial]));
          if vSysInfo.LoadedModulesCount > 0 then
          begin
            Log(
              lEvent, Format('Board SN:%d Loaded modules:%s',
              [vSysInfo.Serial, vSysInfo.LoadedModules]));
          end;
        finally
          Dispose(vSysInfo);
        end;

       { while vPlgIdx < fBoardMandatoryExtentions.Count do
        begin
          TAbstractBoard(vParams.Board).Extentions.Install(fBoardMandatoryExtentions[vPlgIdx]);
          Inc(vPlgIdx);
        end;
        vPlgIdx := 0;
        while vPlgIdx < fPosListMandatoryExtentions.Count do
        begin
          TAbstractBoard(vParams.Board).Positions.Extentions.Install(fPosListMandatoryExtentions[vPlgIdx]);
          Inc(vPlgIdx);
        end;  }
        
        TAbstractBoard(vParams.Board).Protocol.FillBoardExtentionsClasses(vExtClasses);
        if Length(vExtClasses) > 0 then
          for vPlgIdx := Low(vExtClasses) to High(vExtClasses) do
            TAbstractBoard(vParams.Board).Extentions.Install(vExtClasses[vPlgIdx]);

        TAbstractBoard(vParams.Board).Protocol.FillPositionsListExtentionsClasses(vExtClasses);
        if Length(vExtClasses) > 0 then
          for vPlgIdx := Low(vExtClasses) to High(vExtClasses) do
            TAbstractBoard(vParams.Board).Positions.Extentions.Install(vExtClasses[vPlgIdx]);
            
        fEventSubscribers.Execute(Self, C_BoardAdded, vParams);
        if fTmpBoardsList.Count < 1 then
        begin
          fSearchTimer.Enabled := false;
          Log(lEvent, 'All boards found');
          CallOnSearchComplete;
        end;
      end;
      UpdateBoardConfig(TAbstractBoard(vParams.Board));
    end else
    if IsEqualGUID(Event, C_OnDisconnected) then
    begin
      Log(
          lWarning, Format('Board SN:%d disconnected.',
          [TAbstractBoard(vParams.Board).SerialNum]));
    end else
    if IsEqualGUID(Event, C_OnConnectionLost) then
    begin
      Log(
          lWarning, Format('Board SN:%d connection lost.',
          [TAbstractBoard(vParams.Board).SerialNum]));
    end else  
    if IsEqualGUID(Event, C_OnConnectionRestore) then
    begin
      Log(
          lEvent, Format('Board SN:%d connection restored.',
          [TAbstractBoard(vParams.Board).SerialNum]));
    end else
    if IsEqualGUID(Event, C_OnSystemChanged) then
    begin
      if Supports(vParams.Board, IRS232Phisycal, vPortIf) then
      begin
        Log(lWarning,
            Format('Плата на порту COM%d заменена. Опрос порта остановлен',[vPortIf.Port]));
        vPortIf := nil;
      end;
      vIndex := self.IndexOf(vParams.Board);
      if vIndex <> -1 then
      begin
        TTask.Run(
        procedure
        begin
          TThread.Queue(nil,
          procedure
          begin
            try
              self.DeleteBoard(vIndex);
            except
            end;
          end
          );
        end
        );
      end;
    end else
    if IsEqualGUID(Event, C_OnConnectFail) then
    begin
      if Supports(vParams.Board, IRS232Phisycal, vPortIf) then
      begin
        Log(lWarning,
            Format('Плата из списка поиска на порту COM%d не найдена',[vPortIf.Port]));
        vPortIf := nil;
      end;
      vIndex := fTmpBoardsList.IndexOf(vParams.Board);
      if vIndex <> -1 then
      begin
        fExtractTmpTimer.Tag := vIndex;
        fExtractTmpTimer.Enabled := True;
      end;
    end else
    if IsEqualGUID(Event, C_ListReaded) then
    begin
     Log(lEvent, Format('Board SN:%d readed from database %d positions.',
          [TAbstractBoard(vParams.Board).SerialNum, TAbstractBoard(vParams.Board).Positions.Count]));
    end;
    fEventSubscribers.Execute(Self, Event, vParams);
  finally
    Dispose(vParams);
    SetLength(vExtClasses, 0);
  end;
end;

procedure TCustomBoardProcess.UpdateBoardConfig(ABoard: TAbstractBoard);
var
vCC : IConfigController;
vIni : TMemIniFile;
vSt : TStringList;
begin
  if not Supports(self.Owner, IConfigController, vCC) then
  begin
    vCC := nil;
    exit;
  end;
  vIni := TMemIniFile.Create('');
  vSt := TStringList.Create;
  try
    vCC.Load('BoardsList', vIni);
    vIni.WriteInteger(IntToStr(ABoard.SerialNum), 'PositionsCount', ABoard.Positions.MaxCount);
    vCC.Save('BoardsList', vIni);
    ABoard.Protocol.SaveDBConfig(vSt);
    vCC.Save(Format('Board%d',[ABoard.SerialNum]), vSt);
  finally
    FreeAndNil(vSt);
    FreeAndNil(vIni);
    vCC := nil;
  end;
end;

function TCustomBoardProcess.BoardsCount: byte;
begin
  result := byte(fBoards.Count);
end;

function TCustomBoardProcess.PositionsCount : cardinal;
var
i : integer;
vAbstractBoard : IAbstractBoard;
vPositions : IPositionsList;
begin
  result := 0;
  i := 0;
  while (i < BoardsCount) do
  begin
    if Supports(Board[i], IAbstractBoard, vAbstractBoard)
    and vAbstractBoard.QueryPositionListInterface(IPositionsList, vPositions) then
      Inc(result, vPositions.Count);
    vPositions := nil;
    vAbstractBoard := nil;
    Inc(i);
  end;
end; 

function TCustomBoardProcess.ActivePositionsCount: cardinal;
var
i : integer;
vAbstractBoard : IAbstractBoard;
vPositions : IPositionsList;
begin
  result := 0;
  i := 0;
  while (i < BoardsCount) do
  begin
    if Supports(Board[i], IAbstractBoard, vAbstractBoard)
    and vAbstractBoard.QueryPositionListInterface(IPositionsList, vPositions) then
      Inc(result, vPositions.ActiveCount);
    vPositions := nil;
    vAbstractBoard := nil;
    Inc(i);
  end;
end;

function TCustomBoardProcess.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> S_OK) and Supports(fExtentions, IID, Obj) then
    Result := S_OK;
end;

function TCustomBoardProcess.GetItems(AIndex: byte): TObject;
begin
  Result := fBoards.Items[AIndex];
end;

function TCustomBoardProcess.IndexOf(const ABoard: TObject): integer;
begin
  result := 0;
  while  (result < fBoards.Count) and (Board[result] <> ABoard) do
    inc(result);
  if (result = fBoards.Count) then
    result := -1;
end;

function TCustomBoardProcess.IndexOf(const ABoardSN: word): integer;
begin
  result := 0;
  while  (result < fBoards.Count) and (TAbstractBoard(Board[result]).SerialNum <> ABoardSN) do
    inc(result);
  if (result = fBoards.Count) then
    result := -1;
end;

function TCustomBoardProcess.GetManualControlFormClass(out oModule: HMODULE; out oFormClass: TComponentClass): Boolean;
var
  searchResult : TSearchRec;
  GetClassFunc : function : TComponentClass; stdcall;
  vCurrPath : string;
begin
  vCurrPath := ExtractFilePath(ParamStr(0));
  if FindFirst(vCurrPath+'*.bpl', faAnyFile, searchResult) = 0 then
  try
    repeat
      try
        oModule := SafeLoadLibrary(vCurrPath + searchResult.Name,
        SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
        if (oModule > 0) then
        begin
          @GetClassFunc := GetProcAddress(oModule, 'GetGetSimpleManualControlFormClass');
          if Assigned(GetClassFunc) then
          begin
            oFormClass := GetClassFunc;
            Result := Assigned(oFormClass);
            if Result then
              Break
            else
             FreeLibrary(oModule);
          end else
            FreeLibrary(oModule);
        end;
      except
        if (oModule > 0) then
          FreeLibrary(oModule);
        Continue;
      end;
    until FindNext(searchResult) <> 0;
  finally
    FindClose(searchResult);
    if not Result then
    begin
      oModule := 0;
      oFormClass := nil;
    end;
  end;
end;

function TCustomBoardProcess.ICreateManualControlForm_Create(var alreadyExists : boolean):TComponent;
var
vFormClass: TComponentClass;
vLoadProc : TPackageLoad;
vFormOnDestroyEvent : IFormOnDestroyEvent;
begin
  alreadyExists := assigned(fFormLib.FormInstance);
  if alreadyExists then
  begin
    Result := fFormLib.FormInstance;
    exit;
  end;
  if GetManualControlFormClass(fFormLib.FormLibrary, vFormClass) then
  begin
    @vLoadProc := GetProcAddress(fFormLib.FormLibrary, 'Initialize');
    if Assigned(vLoadProc) then
    try
       vLoadProc;
    except on e : Exception do
      begin
        Self.Log(lWarning, Format('Excetion "%s" message:"%s"',[e.ClassName, e.Message]));
        raise e;
      end;
    end;
    fFormLib.FormInstance := vFormClass.Create(self);
    if Supports(fFormLib.FormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
    begin
      vFormOnDestroyEvent.OnDestroy := fFormLib.OnControlFormDestroy;
      vFormOnDestroyEvent := nil;
    end;
    Result := fFormLib.FormInstance;
  end;
end;

procedure TCustomBoardProcess.InitProtocolLib;
var
vLoadProc: TPackageLoad;
begin
  if fProticolLib = 0 then
  begin
    fProticolLib := GetProtocolClass(Owner, fProtocol);
    if fProticolLib = 0 then
      exit;
    @vLoadProc := GetProcAddress(fProticolLib, 'Initialize');
    if Assigned(vLoadProc) then
      vLoadProc;
  end;
end;

function TCustomBoardProcess.LevelEnabled(ALevel: TLogInfo): boolean;
begin
  Result := Assigned(vgBoardLogger) and vgBoardLogger.LevelEnabled(ALevel);
end;

procedure TCustomBoardProcess.Log(ALevel: TLogInfo; const Text: WideString; Instance: TObject);
var
vBoard : IAbstractBoard;
begin
  if not Assigned(vgBoardLogger) then
    Exit;
  if not vgBoardLogger.LevelEnabled(ALevel) then
    Exit;
  if Supports(Instance, IAbstractBoard, vBoard) then
    vgBoardLogger.Log(ALevel, Format('Board SN:%d Message: %s',[vBoard.SerialNum, Text]))
  else if Assigned(Instance) then
    vgBoardLogger.Log(ALevel, Instance.ClassName +' '+ Text)
  else
    vgBoardLogger.Log(ALevel, Text);
  vBoard := nil;
end;

procedure TCustomBoardProcess.DoneProtocolLib;
var
vUnloadProc: TPackageUnload;
begin
  if fProticolLib <> 0 then
  begin
    @vUnloadProc := GetProcAddress(fProticolLib, 'Finalize');
    try
      if Assigned(vUnloadProc) then
        vUnloadProc;
    finally 
      FreeLibrary(fProticolLib);
      fProticolLib := 0;
      fProtocol := nil;
    end;
  end;
end;

procedure TCustomBoardProcess.StartSearch;
begin
  fSearchTimer.Interval := 10;
  fSearchTimer.OnTimer := StartSearchInternal;
  fSearchTimer.Enabled := True;

  fExtractTmpTimer.Interval := 10;
  fExtractTmpTimer.OnTimer := OnExtreactTmp;
  fExtractTmpTimer.Enabled := False;
  fExtractTmpTimer.Tag := -1;
end;

procedure TCustomBoardProcess.StartSearchInternal(Sender: TObject);
var
vPortList : TStringList;
i : integer;
FHandl: THandle;
vPort : Byte;
vBoard: TAbstractBoard;
vBoardEB : IEventBus;
vPortIf : IRS232Phisycal;
begin
  fSearchTimer.Interval := 20000;
  fSearchTimer.OnTimer := SearchTimeOut;
  fSearchTimer.Enabled := True;
  fEventSubscribers.Execute(Self, C_SearchStart, nil);

  vPortList := TStringList.Create();
  try
    EnumComPorts(vPortList);
    fTmpBoardsList.Clear;
    if vPortList.Count < 1 then
      Exit;

    if fProticolLib = 0 then
        InitProtocolLib;
    if fProticolLib = 0 then
    begin
      Log(lWarning, 'No protocol library found');
      CallOnSearchComplete;
      fSearchTimer.Enabled := false;
      Exit;
    end;
    i := 0;
    while (i < vPortList.Count) do
    begin
      FHandl := CreateFile(PWideChar('\\.\' + vPortList[i]),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
      if FHandl <> INVALID_HANDLE_VALUE then
      begin
        CloseHandle(FHandl);
        vBoard := TAbstractBoard.Create(Self, fProtocol);
        if Supports(vBoard, IEventBus, vBoardEB) then
        begin
          vBoardEB.Add(BoardEvent);
          if Supports(vBoard, IRS232Phisycal, vPortIf) then
          begin
            vPort := ComNameToByte(vPortList[i]);
            Log(lEvent, Format('Test protorcol "%s" on port COM%d',[fProtocol.ProtocolName, vPort]));
            if vPortIf.Connect(vPort) then
            begin
              vPortIf := nil;
              vBoardEB := nil;
              fTmpBoardsList.Add(vBoard);
            end else
            begin
              vPortIf := nil;
              vBoardEB := nil;
              FreeAndNil(vBoard);
            end;
          end else
          begin
            vBoardEB := nil;
            FreeAndNil(vBoard);
          end;
        end else
        begin
          vBoardEB := nil;
          FreeAndNil(vBoard);
        end;
      end;
      inc(i);
    end;
    if fTmpBoardsList.Count < 1 then
    begin
      if vPortList.Count < 1 then
        Log(lWarning, 'No vacant ports')
      else
        Log(lWarning, 'No boards whith try connect');
      CallOnSearchComplete;
      fSearchTimer.Enabled := false;
    end;
  finally
    FreeAndNil(vPortList);
  end;
end;

procedure TCustomBoardProcess.OnExtreactTmp(Sender: TObject);
begin
  fExtractTmpTimer.Enabled := False;
  if (fExtractTmpTimer.Tag <> -1)
    and (fExtractTmpTimer.Tag < fTmpBoardsList.Count) then
    fTmpBoardsList.Delete(fExtractTmpTimer.Tag);
  fExtractTmpTimer.Tag := -1;
  if fTmpBoardsList.Count < 1 then
  begin
    CallOnSearchComplete;
    fSearchTimer.Enabled := false;
  end;
end;

procedure TCustomBoardProcess.SearchTimeOut(Sender: TObject);
begin
  fSearchTimer.Enabled := false;
  fTmpBoardsList.Clear;
  if (fBoards.Count < 1) then
  begin
    DoneProtocolLib;
    Log(lEvent, 'All ports challenged. No boards found');
  end;
  CallOnSearchComplete;
end;

end.
