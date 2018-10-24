unit cbp30Module;

interface
  uses WinApi.Windows, WinApi.Messages, System.Classes, CustomBoardProcess,
  LoggerInterface, ChipPluginManager, 
  StpProcessTypes, AbstractExtention,
  AbstractStpMethod;

  type
  
  TBoardStpExtentionManager = class(TAbstractExtention)
   private 
    FWindowHandle: HWND;
    fBoard,
    fBoardProc,
    fMainProc : TObject;
    fCurrStpState : Byte;
    MeasureMethods : array of TStpMethodClass;
    CurrMethodInstance : TAbstractStpMethod;
    FinalProgClass : TFinalProgrammerClass;
    FinalProgInstance : TFinalProgrammerStpMethod;
    procedure MessageHandler(var message : TMessage);
    procedure FillMethods(ProcAddr : Pointer; AStpState : byte); 
    procedure Init(const AMainProc, ABoardProc, ABoard : TObject; AStatesCount : byte);
    procedure SetState(AState : Byte);
    function ReadyToStart:Boolean; 
    function Worked : boolean;
    procedure Reinit;
    procedure Start;
    procedure Stop;
    procedure PrepareProgrammer;
    function ExternalProgPowerNeded : Boolean;
    function ReadyToProg : Boolean;
    procedure StartFinalProgramming;  
    procedure DoneProgrammer;
   protected   
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
  end;


  Tcbp30 = class(TCustomBoardProcess,
                  IStpMeasureController)
   private
    fChipLibrary : TChipPluginManager;
    procedure InitializeStpMethods;
   protected  
    procedure AfterCreate;override;
    procedure BeforeDestroy;override;  
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function GetManualControlFormClass(out oModule:HMODULE; out oFormClass : TComponentClass):Boolean;override;
    {IStpMeasureController}
    procedure ReadPositions;stdcall;
    procedure ReadBoardPositions(const ABoard: TObject);stdcall;
    function PositionExists(const AID : integer):boolean; stdcall;
    procedure InitStpProcess;stdcall;
    procedure SetStpState(AState : byte);stdcall;
    procedure StpReinit; stdcall;
    function StpReadyToStart:Boolean; stdcall;
    procedure StpStart; stdcall;
    function WorkedCount : Byte;stdcall;
    procedure StpStop; stdcall;
    procedure DeleteLastIter(const ABoard: TObject=nil); stdcall;
    procedure DeleteUnSuccIter(const ABoard: TObject=nil); stdcall;
    procedure PrepareProgrammer;stdcall;
    procedure DoneProgrammer;stdcall;
    function ExternalProgPowerNeded : Boolean;stdcall;
    function ReadyToProg : Boolean;stdcall;
    procedure StartFinalProgramming;stdcall;
    property ChipLibrary : TChipPluginManager read fChipLibrary;
   public
    procedure Clear; override; stdcall;
    procedure DeleteBoard(AIndex : byte); override;
  end;

implementation
uses System.SysUtils,
System.Contnrs,
CustomChildProcess,
AbstractBoardInterface,
ExtentionsListInterface,
PositionListInterface,
PositionInterface,
ChipAbstract,
VdpDMAccess,
ChipAbstractInterface,
EventBusInterface;

function BoardProcessClass: TChildProcessClass; stdcall;
begin
  Result := Tcbp30;
end; exports BoardProcessClass;

function BoardProcessVersion: Word; stdcall;
begin
  Result := 30;
end; exports BoardProcessVersion;

{ TCompensBoardProcess }

type
{Import}
TPositionsListDBInitProc = procedure (const ADMAccess : IDMAccess; ABoardSerial : Word; const APositionList : IPositionsList);stdcall;
TPositionsListDBOperationsProc = procedure (const ADMAccess : IDMAccess; const APositionList : IPositionsList; ACheckActive : boolean);stdcall;
TPreparePositionsProc = procedure (const ADMAccess : IDMAccess; const APositionList : IPositionsList; AState : byte);stdcall;
TPositionsDBCheckIDFunc = function(const APositionList : IPositionsList; const AID : integer):boolean;stdcall;
TStpMethodClassGetter = function : TStpMethodClass;stdcall;
TFinalProgrammerClassGetter = function : TFinalProgrammerClass;stdcall;

{$REGION ' Internal types '}
TCreatePositionsParams = record
  PosListQR : TvdQuery;
  DMAccess : IDMAccess;
  cbp : Tcbp30;
end;
pCreatePositionsParams = ^TCreatePositionsParams;

TInitStpParams = record
  cbp : Tcbp30;
end;
pInitStpParams = ^TInitStpParams;

TStpSetParams = record
  State : Byte; 
  DMAccess : IDMAccess;
  cbp : Tcbp30;
end;
pStpSetParams = ^TStpSetParams;

TCheckStpActiveCountParams = record
  Count : Byte;
end;
pCheckStpActiveCountParams = ^TCheckStpActiveCountParams;


TCheckPositionIDParams = record
  cbp : Tcbp30;
  ID : Integer;
  Result : boolean;
end;
pCheckPositionIDParams = ^TCheckPositionIDParams;

TIterOperationsParams = record
  cbp : Tcbp30;
  DMAccess : IDMAccess;
  CheckActive : Boolean;
end;
pIterOperationsParams = ^TIterOperationsParams;
{$ENDREGION}

{$REGION ' Internal functions '}
procedure InternalCheckStpWorkedCount(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vParams : pCheckStpActiveCountParams;
vBoardExtentions : IExtentions;
vBoardStpManager : TBoardStpExtentionManager;
begin
  if not Assigned(Params) then
    Exit;
  vParams := pCheckStpActiveCountParams(Params);
  if Supports(ABoard, IExtentions, vBoardExtentions)
  and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager)
  and vBoardStpManager.Worked then
    Inc(vParams.Count);
  vBoardExtentions := nil;
end;

procedure InternalCreateBoardPositions(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vParams : pCreatePositionsParams;
vBoard : IAbstractBoard;
vPosList : IPositionsList;
vChipIndex : Integer;
vChipClass : TChipClass;
vAddExtentions : TAdditionalExtentions;
vFreeQuery : Boolean;
vPositionsInitDBProcPtr : Pointer;
vPositionsDBInitProc : TPositionsListDBInitProc;
vEBE : IBoardEventBusExec;
begin
  if not Assigned(Params) then
    Exit;
  vParams := pCreatePositionsParams(Params);
  if (not Assigned(vParams.DMAccess))
  or (not Assigned(vParams.cbp)) then
     Exit;
  vFreeQuery := not Assigned(vParams.PosListQR);
  try
    if Supports(ABoard, IAbstractBoard, vBoard)
    and vBoard.QueryPositionListInterface(IPositionsList, vPosList) then
    begin
      if vFreeQuery then
         vParams.PosListQR := vParams.DMAccess.CreateReadQuery(nil,
        'select rpos.id, rpos.rnum, rpos.rcard, '+
          'rpos.board, rpos.boardpos as board_pos, rpos.status, rcard.chip_guid '+
          'from rpos inner join rcard on (rpos.rcard = rcard.id) '+
          'where (rpos.board = :board) and (rpos.status <= 1) '+
          'order by rpos.boardpos');
      vPosList.BeginUpdate;
      try
        vParams.PosListQR.Prepare;
        try
          vParams.PosListQR.ParamByName('board').AsInteger := vBoard.SerialNum;
          vParams.PosListQR.Open;
          vPosList.Clear;
          vChipIndex := vParams.cbp.ChipLibrary.IndexOf(StringToGUID(vParams.PosListQR.FieldByName('chip_guid').AsString));
          if (vChipIndex <> -1) then
          begin
            vChipClass := vParams.cbp.ChipLibrary.ItemClass[Byte(vChipIndex)];
            SetLength(vAddExtentions, Length(vAddExtentions)+1);
            vAddExtentions[High(vAddExtentions)] := Pointer(vChipClass);
            while not vParams.PosListQR.Eof do
            begin
              if IsEqualGUID(vChipClass.ChipGUID, StringToGUID(vParams.PosListQR.FieldByName('chip_guid').AsString) ) then
                vPosList.NewItem(byte(vParams.PosListQR.FieldByName('board_pos').AsInteger), vAddExtentions);
              vParams.PosListQR.Next;
            end;
            if (vPosList.Count > 0)
            and vParams.cbp.ChipLibrary.FindChipPligunFunction(vChipIndex, 'PositionsDBInitProc', vPositionsInitDBProcPtr) then
            begin
              @vPositionsDBInitProc := vPositionsInitDBProcPtr;
              vPositionsDBInitProc(vParams.DMAccess, vBoard.SerialNum, vPosList);
            end;
          end;
          vParams.DMAccess.Commit(vParams.PosListQR);
        except
          on e : Exception do
            vParams.DMAccess.OnException(vParams.PosListQR, E);
        end;
      finally
        vPosList.EndUpdate;
        SetLength(vAddExtentions, 0);
        if vFreeQuery then
          FreeAndNil(vParams.PosListQR);
        if Supports(ABoard, IBoardEventBusExec, vEBE) then
          vEBE.Execute(ABoard, C_ListReaded, nil);
        vEBE := nil;
      end;
    end;
  finally
    vPosList := nil;
    vBoard := nil;
  end;
end;

procedure InternalInitStpMethods(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vParams : pInitStpParams;
vBoard : IAbstractBoard;
vBoardExtentions : IExtentions;
vPosExtentions : IExtentions;
vBoardStpManager : TBoardStpExtentionManager;
vPosList : IPositionsList;
vChip : IChipAbstract;
vStatesDescriptor : IStpStatesDescriptor;
vStpMethod : byte;
vFinalProgrammerClassGetter : TFinalProgrammerClassGetter;
vChipIdx : Integer;
vProcAddr : Pointer;
vStatesCount : Byte;
begin
  vStatesCount := 0;
  if not Assigned(Params) then
    Exit;
  vParams := pInitStpParams(Params);
  if (not Assigned(vParams.cbp))
  or (not Assigned(vParams.cbp.ChipLibrary)) then
     Exit;
  if Supports(vParams.cbp.Owner, IStpStatesDescriptor, vStatesDescriptor) then
    vStatesCount :=  vStatesDescriptor.StatesCount;

  if vStatesCount < 1 then
  begin
    vStatesDescriptor := nil;
    Exit;
  end;
  try
    if Supports(ABoard, IAbstractBoard, vBoard)
    and vBoard.QueryPositionListInterface(IPositionsList, vPosList)
    and Supports(ABoard, IExtentions, vBoardExtentions)
    and vPosList.QueryItem(0, IExtentions, vPosExtentions)
    and vPosExtentions.Find(IChipAbstract, vChip) then
    begin
      vChipIdx := vParams.cbp.ChipLibrary.IndexOf(vChip.ChipGUID);
      if (vChipIdx <> -1)
      and (vParams.cbp.ChipLibrary.ChipPligunsCount(vChipIdx) > 0)
      and vBoardExtentions.Install(TBoardStpExtentionManager)
      and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager) then
      begin
        vBoardStpManager.Init(vParams.cbp.Owner, vParams.cbp, ABoard, vStatesCount);

        for vStpMethod := 0 to vStatesCount-1 do
        begin
          if vParams.cbp.ChipLibrary.FindChipPligunFunction(vChipIdx,
                                vStatesDescriptor.StateProgrammName(vStpMethod)+'StpClass',
                                vProcAddr) then
          begin
            vBoardStpManager.FillMethods(vProcAddr, vStpMethod);
          end;
        end;
        if vParams.cbp.ChipLibrary.FindChipPligunFunction(vChipIdx,
                                                        'FinalProgrammerClass',
                                                        vProcAddr) then
        begin
          @vFinalProgrammerClassGetter := vProcAddr;
          vBoardStpManager.FinalProgClass := vFinalProgrammerClassGetter;
        end;
      end;
    end;
  finally
    vChip := nil;
    vPosExtentions := nil;
    vPosList := nil;
    vBoardExtentions := nil;
    vBoard := nil;
    vStatesDescriptor := nil;
  end;
end; 

procedure InternalChangeStpState(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vParams : pStpSetParams;
vBoard : IAbstractBoard;
vBoardExtentions : IExtentions;
vPosExtentions : IExtentions;
vPosList : IPositionsList; 
vChip : IChipAbstract;
vBoardStpManager : TBoardStpExtentionManager;
vPrepareProc : TPreparePositionsProc;
vChipIdx : Integer;
vProcAddr : Pointer;
begin
  if not Assigned(Params) then
    Exit;
  vParams := pStpSetParams(Params);
  try
    if Supports(ABoard, IExtentions, vBoardExtentions)
    and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager)
    and Supports(ABoard, IAbstractBoard, vBoard)
    and vBoard.QueryPositionListInterface(IPositionsList, vPosList)
    and vPosList.QueryItem(0, IExtentions, vPosExtentions)
    and vPosExtentions.Find(IChipAbstract, vChip) then
    begin
      vChipIdx := vParams.cbp.ChipLibrary.IndexOf(vChip.ChipGUID);
      if (vChipIdx <> -1)
      and vParams.cbp.ChipLibrary.FindChipPligunFunction(vChipIdx,'PrepareStatePositionsProc',
                                vProcAddr) then
      begin
        @vPrepareProc := vProcAddr;
        vPrepareProc(vParams.DMAccess, vPosList, vParams.State);
        vBoardStpManager.SetState(vParams.State);
      end;
    end;
  finally
    vChip := nil;
    vPosExtentions := nil;
    vPosList := nil;
    vBoard := nil;
    vBoardExtentions := nil;
  end;
end;

procedure InternalStpReadyToStart(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vBoard : IAbstractBoard;
vParams : pCheckStpActiveCountParams;
vBoardExtentions : IExtentions;
vBoardStpManager : TBoardStpExtentionManager;
begin
  if not Assigned(Params) then
    Exit;
  vParams := pCheckStpActiveCountParams(Params);
  if  Supports(ABoard, IAbstractBoard, vBoard)
  and vBoard.Connected
  and Supports(ABoard, IExtentions, vBoardExtentions)
  and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager)
  and vBoardStpManager.ReadyToStart then
    Inc(vParams.Count);
  vBoardExtentions := nil;
  vBoard := nil;
end;

procedure InternalStpReinit(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vBoardExtentions : IExtentions;
vBoardStpManager : TBoardStpExtentionManager;
begin
  if Supports(ABoard, IExtentions, vBoardExtentions)
  and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager) then
    vBoardStpManager.Reinit;
  vBoardExtentions := nil;
end;

procedure InternalStpStart(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vBoard : IAbstractBoard;
vBoardExtentions : IExtentions;
vBoardStpManager : TBoardStpExtentionManager;
begin
  if Supports(ABoard, IAbstractBoard, vBoard)
  and vBoard.Connected
  and Supports(ABoard, IExtentions, vBoardExtentions)
  and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager)
  and vBoardStpManager.ReadyToStart then
    vBoardStpManager.Start;
  vBoardExtentions := nil;
  vBoard := nil;
end;

procedure InternalStpStop(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vBoardExtentions : IExtentions;
vBoardStpManager : TBoardStpExtentionManager;
begin
  if Supports(ABoard, IExtentions, vBoardExtentions)
  and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager) then
    vBoardStpManager.Stop;
  vBoardExtentions := nil;
end;

procedure InternalCheckPositionID(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vParams : pCheckPositionIDParams;
vBoard : IAbstractBoard;
vPosList : IPositionsList;
vPositionExtentions : IExtentions;
vChipIndex : Integer;
vChip : IChipAbstract;
vPositionsDBCheckIDFuncPtr : Pointer;
vPositionsDBCheckIDFunc : TPositionsDBCheckIDFunc;
begin
  if not Assigned(Params) then
    Exit;
  vParams := pCheckPositionIDParams(Params);
  if vParams.Result then
    Exit;
  if (not Assigned(vParams.cbp)) then
     Exit;
  vChipIndex := -1;
  try
    if Supports(ABoard, IAbstractBoard, vBoard)
    and vBoard.QueryPositionListInterface(IPositionsList, vPosList) then
    begin
      if vPosList.QueryItem(0, IExtentions, vPositionExtentions)
      and vPositionExtentions.Find(IChipAbstract, vChip)  then
        vChipIndex := vParams.cbp.ChipLibrary.IndexOf(vChip.ChipGUID);
      vChip := nil;
      vPositionExtentions := nil;
      if vParams.cbp.ChipLibrary.FindChipPligunFunction(vChipIndex,
                                                'PositionsDBCheckID',
                                                vPositionsDBCheckIDFuncPtr) then
      begin
        @vPositionsDBCheckIDFunc := vPositionsDBCheckIDFuncPtr;
        vParams.Result := vPositionsDBCheckIDFunc(vPosList, vParams.ID);
      end;
    end;
  finally
    vPosList := nil;
    vBoard := nil;
  end;
end;

procedure InternalDeleteLastIter(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vParams : pIterOperationsParams;
vBoard : IAbstractBoard;
vPosList : IPositionsList;
vPositionExtentions : IExtentions;
vChipIndex : Integer;
vChip : IChipAbstract;
vDBProcPtr : Pointer;
vDBProc : TPositionsListDBOperationsProc;
begin
   if not Assigned(Params) then
    Exit;
  vParams := pIterOperationsParams(Params);
  if (not Assigned(vParams.cbp))
  or (not Assigned(vParams.DMAccess)) then
     Exit;
  vChipIndex := -1;
  try
    if Supports(ABoard, IAbstractBoard, vBoard)
    and vBoard.QueryPositionListInterface(IPositionsList, vPosList) then
    begin
      vPosList.BeginUpdate;
      try
        if vPosList.QueryItem(0, IExtentions, vPositionExtentions)
        and vPositionExtentions.Find(IChipAbstract, vChip)  then
          vChipIndex := vParams.cbp.ChipLibrary.IndexOf(vChip.ChipGUID);
      finally
        vPosList.EndUpdate;
        vChip := nil;
        vPositionExtentions := nil;
      end;
      if vParams.cbp.ChipLibrary.FindChipPligunFunction(vChipIndex,
                                                'PositionsDBDeleteLastIter',
                                                vDBProcPtr) then
      begin
        @vDBProc := vDBProcPtr;
        vDBProc(vParams.DMAccess, vPosList, vParams.CheckActive);
      end;
    end;
  finally
    vPosList := nil;
    vBoard := nil;
  end;
end;

procedure InternalDeleteUnSuccIter(const ABoard: TObject; BoardIndex : byte; Params : Pointer); stdcall;
var
vParams : pIterOperationsParams;
vBoard : IAbstractBoard;
vPosList : IPositionsList;
vPositionExtentions : IExtentions;
vChipIndex : Integer;
vChip : IChipAbstract;
vDBProcPtr : Pointer;
vDBProc : TPositionsListDBOperationsProc;
begin
   if not Assigned(Params) then
    Exit;
  vParams := pIterOperationsParams(Params);
  if (not Assigned(vParams.cbp))
  or (not Assigned(vParams.DMAccess)) then
     Exit;
  vChipIndex := -1;
  try
    if Supports(ABoard, IAbstractBoard, vBoard)
    and vBoard.QueryPositionListInterface(IPositionsList, vPosList) then
    begin
      vPosList.BeginUpdate;
      try
        if vPosList.QueryItem(0, IExtentions, vPositionExtentions)
        and vPositionExtentions.Find(IChipAbstract, vChip)  then
          vChipIndex := vParams.cbp.ChipLibrary.IndexOf(vChip.ChipGUID);
      finally
        vPosList.EndUpdate;
        vChip := nil;
        vPositionExtentions := nil;
      end;
      if vParams.cbp.ChipLibrary.FindChipPligunFunction(vChipIndex,
                                                'PositionsDBDeleteUnSuccIter',
                                                vDBProcPtr) then
      begin
        @vDBProc := vDBProcPtr;
        vDBProc(vParams.DMAccess, vPosList, vParams.CheckActive);
      end;
    end;
  finally
    vPosList := nil;
    vBoard := nil;
  end;
end;

{$ENDREGION}


procedure Tcbp30.AfterCreate;
begin
  inherited;
  fChipLibrary := TChipPluginManager.Create;
  fChipLibrary.FindPluginFiles;
end;

procedure Tcbp30.BeforeDestroy;
begin
  inherited;
  FreeAndNil(fChipLibrary);
end;

procedure Tcbp30.InitStpProcess;
begin
  ReadPositions;
  InitializeStpMethods;
end;

function Tcbp30.PositionExists(const AID : integer): boolean;
var
vParams : pCheckPositionIDParams;
begin
  New(vParams);
  try
    vParams.Result := False;
    vParams.cbp := Self;
    vParams.ID := AID;
    ForEachBoard(InternalCheckPositionID, vParams);
  finally
    vParams.cbp := nil;
    Result := vParams.Result;
    Dispose(vParams);
  end;
end;

procedure Tcbp30.Clear;
begin
  StpStop;
  inherited Clear;
end;

procedure Tcbp30.ReadBoardPositions(const ABoard: TObject);
var
vParams : pCreatePositionsParams;
begin
  New(vParams);
  vParams.PosListQR := nil;
  vParams.cbp := Self;
  try
    if not Supports(self.Owner, IDMAccess, vParams.DMAccess) then
      Exit;
    InternalCreateBoardPositions(ABoard, 0, vParams);
  finally
    vParams.cbp := nil;
    vParams.DMAccess := nil;
    Dispose(vParams);
  end;
end;

procedure Tcbp30.ReadPositions;
var
vParams : pCreatePositionsParams;
begin
  New(vParams);
  try
    vParams.cbp := Self;
    if not Supports(self.Owner, IDMAccess, vParams.DMAccess) then
      Exit;
    vParams.PosListQR := vParams.DMAccess.CreateReadQuery(nil,
        'SELECT RPOS.ID, RPOS.RNUM, RPOS.RCARD, '+
          'RPOS.BOARD, RPOS.BOARDPOS as BOARD_POS, RPOS.STATUS, RCARD.CHIP_GUID '+
          'FROM RPOS INNER JOIN RCARD ON (RPOS.RCARD = RCARD.ID) '+
          'WHERE (RPOS.BOARD = :BOARD) AND (RPOS.STATUS <= 1) '+
          'ORDER BY RPOS.BOARDPOS');
    try
      try
        ForEachBoard(InternalCreateBoardPositions, vParams);
      except
        on e : Exception do
          vParams.DMAccess.OnException(vParams.PosListQR, E);
      end;
    finally
      FreeAndNil(vParams.PosListQR);
    end;
  finally
    vParams.DMAccess := nil;
    vParams.cbp := nil;
    Dispose(vParams);
  end;
end;

procedure Tcbp30.DeleteBoard(AIndex: byte);
begin
  InternalStpStop(Board[AIndex], AIndex, nil);
  inherited DeleteBoard(AIndex);
end;

procedure Tcbp30.DeleteLastIter(const ABoard: TObject);
var
vParams : pIterOperationsParams;
begin
  New(vParams);
  try
    if not Supports(self.Owner, IDMAccess, vParams.DMAccess) then
      Exit;
    vParams.cbp := Self;
    vParams.CheckActive := Assigned(ABoard);
    if vParams.CheckActive then
      InternalDeleteLastIter(ABoard, 0, vParams)
    else
      ForEachBoard(InternalDeleteLastIter, vParams);
  finally
    vParams.cbp := nil;
    vParams.DMAccess := nil;
    Dispose(vParams);
  end;
end;

procedure Tcbp30.DeleteUnSuccIter(const ABoard: TObject);
var
vParams : pIterOperationsParams;
begin
  New(vParams);
  try
    if not Supports(self.Owner, IDMAccess, vParams.DMAccess) then
      Exit;
    vParams.cbp := Self;
    vParams.CheckActive := Assigned(ABoard);
    if vParams.CheckActive then
      InternalDeleteUnSuccIter(ABoard, 0, vParams)
    else
      ForEachBoard(InternalDeleteUnSuccIter, vParams);
  finally
    vParams.cbp := nil;
    vParams.DMAccess := nil;
    Dispose(vParams);
  end;
end;

procedure Tcbp30.InitializeStpMethods;
var
vParams : pInitStpParams;
begin
  New(vParams);
  vParams.cbp := self;
  try
    ForEachBoard(InternalInitStpMethods, vParams);
  finally
    vParams.cbp := nil;
    Dispose(vParams);
  end;
end;

procedure Tcbp30.SetStpState(AState: byte);
var
vParams : pStpSetParams;
begin
  New(vParams);
  vParams.State := AState;
  vParams.cbp := Self;
  try  
    if not Supports(self.Owner, IDMAccess, vParams.DMAccess) then
      Exit;
    ForEachBoard(InternalChangeStpState, vParams);
  finally
    vParams.cbp := nil;
    vParams.DMAccess := nil;
    Dispose(vParams);
  end;
end;

function Tcbp30.StpReadyToStart: Boolean;
var
vParams : pCheckStpActiveCountParams;
begin
  New(vParams);
  vParams.Count := 0;
  try
    ForEachBoard(InternalStpReadyToStart, vParams);
  finally
    result := (vParams.Count > 0);
    Dispose(vParams);
  end;
end;

procedure Tcbp30.StpReinit;
begin
  ForEachBoard(InternalStpReinit, nil);
end;

procedure Tcbp30.StpStart;
begin
  ForEachBoard(InternalStpStart, nil);
end;

procedure Tcbp30.StpStop;
begin
  ForEachBoard(InternalStpStop, nil);
end;  

function Tcbp30.WorkedCount: Byte;
var
vCheckStpCountParams : pCheckStpActiveCountParams;
begin
  New(vCheckStpCountParams);
  vCheckStpCountParams.Count := 0;
  try
    ForEachBoard(InternalCheckStpWorkedCount, vCheckStpCountParams);     
  finally
    result := vCheckStpCountParams.Count;
    Dispose(vCheckStpCountParams);
  end;
end;

procedure Tcbp30.PrepareProgrammer;
var
vIdx, vCount : Word;
vBoardExtentions :IExtentions;
vBoardStpManager : TBoardStpExtentionManager; 
begin
  vCount := BoardsCount;
  vIdx := 0;
  while vIdx < vCount  do
  begin
    if Supports(Board[vIdx], IExtentions, vBoardExtentions)
    and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager) then
      vBoardStpManager.PrepareProgrammer;
    vBoardExtentions := nil;
    Inc(vIdx);
  end;
end;

function Tcbp30.ReadyToProg: Boolean; 
var
vIdx, vCount, vResCount : Word;
vBoardExtentions :IExtentions;
vBoardStpManager : TBoardStpExtentionManager; 
begin
  Result := False; 
  vCount := BoardsCount;
  vIdx := 0;
  vResCount := 0;
  while vIdx < vCount  do
  begin
    if Supports(Board[vIdx], IExtentions, vBoardExtentions)
    and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager)
    and vBoardStpManager.ReadyToProg then
     Inc(vResCount);
    vBoardExtentions := nil;
    Inc(vIdx);
  end;
  result := vResCount > 0;
end;

function Tcbp30.ExternalProgPowerNeded: Boolean;
var
vIdx, vCount, vResCount : Word;
vBoardExtentions :IExtentions;
vBoardStpManager : TBoardStpExtentionManager; 
begin
  Result := False; 
  vCount := BoardsCount;
  vIdx := 0;
  vResCount := 0;
  while vIdx < vCount  do
  begin
    if Supports(Board[vIdx], IExtentions, vBoardExtentions)
    and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager) 
    and vBoardStpManager.ExternalProgPowerNeded then
     Inc(vResCount);
    vBoardExtentions := nil;
    Inc(vIdx);
  end;
  result := vResCount > 0;
end;

procedure Tcbp30.StartFinalProgramming;
var
vIdx, vCount : Word;
vBoardExtentions :IExtentions;
vBoardStpManager : TBoardStpExtentionManager; 
begin
  vCount := BoardsCount;
  vIdx := 0;
  while vIdx < vCount  do
  begin
    if Supports(Board[vIdx], IExtentions, vBoardExtentions)
    and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager) then
      vBoardStpManager.StartFinalProgramming;
    vBoardExtentions := nil;
    Inc(vIdx);
  end;
end;  

procedure Tcbp30.DoneProgrammer;
var
vIdx, vCount : Word;
vBoardExtentions :IExtentions;
vBoardStpManager : TBoardStpExtentionManager; 
begin
  vCount := BoardsCount;
  vIdx := 0;
  while vIdx < vCount  do
  begin
    if Supports(Board[vIdx], IExtentions, vBoardExtentions)
    and vBoardExtentions.Find(TBoardStpExtentionManager, vBoardStpManager) then
      vBoardStpManager.DoneProgrammer;
    vBoardExtentions := nil;
    Inc(vIdx);
  end;
end;

function Tcbp30.GetManualControlFormClass(out oModule: HMODULE; out oFormClass: TComponentClass): Boolean;
var
  searchResult : TSearchRec;
  GetClassFunc : function : TComponentClass; stdcall;
  vCurrPath : string;
  vServiceMode : Boolean;
begin
  Result := False;
  vServiceMode := FindCmdLineSwitch('sm');
  if fChipLibrary.Count < 1 then
    Result := inherited GetManualControlFormClass(oModule, oFormClass)
  else
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
            if not vServiceMode then
              @GetClassFunc := GetProcAddress(oModule, 'GetGetChipSupporttedManualControlFormClass')
            else
              @GetClassFunc := GetProcAddress(oModule, 'GetGetServiceModeManualControlFormClass');
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
    end;
    if not Result then
       Result := inherited GetManualControlFormClass(oModule, oFormClass);
  end;
end;

function Tcbp30.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> S_OK) and Supports(fChipLibrary, IID, Obj) then
    Result := S_OK;
end;

{TBoardStpExtentionManager}

procedure TBoardStpExtentionManager.AfterCreate;
begin
  inherited;
  FWindowHandle := System.Classes.AllocateHWND(MessageHandler);
  CurrMethodInstance := nil;
  FinalProgClass := nil;
  FinalProgInstance := nil;
end;

procedure TBoardStpExtentionManager.BeforeDestroy;
begin    
  System.Classes.DeallocateHWND(FWindowHandle);
  if Assigned(CurrMethodInstance) then
  begin  
    if CurrMethodInstance.ThreadExists then
      CurrMethodInstance.Stop;
    FreeAndNil(CurrMethodInstance);
  end;
  SetLength(MeasureMethods, 0);
  CurrMethodInstance := nil;
  FinalProgInstance := nil;
  fBoard := nil;
  fBoardProc := nil;
  fMainProc := nil;
  inherited;
end;

procedure TBoardStpExtentionManager.Init(const AMainProc, ABoardProc, ABoard: TObject; AStatesCount: byte);
begin  
  fMainProc := AMainProc;
  fBoardProc := ABoardProc;
  fBoard := ABoard;
  fCurrStpState := 0;
  SetLength(MeasureMethods, AStatesCount);
end;

procedure TBoardStpExtentionManager.MessageHandler(var message: TMessage);
var
vEB : IBoardEventBusExec;
begin
  case message.Msg of
    C_StpMethodStarted:
    begin
      if Supports(fBoard, IBoardEventBusExec, vEB) then
        vEB.Execute(fBoard, C_STPMeasureTestStarted, nil);
      vEB := nil;
    end;
    C_StpMethodEnded:
    begin
      if Assigned(CurrMethodInstance) then
        FreeAndNil(CurrMethodInstance);
      if Supports(fBoard, IBoardEventBusExec, vEB) then
        vEB.Execute(fBoard, C_STPMeasureTestEnded, nil);
      vEB := nil;
    end; 
    C_ProgrammerStarted:
    begin
      if Supports(fBoard, IBoardEventBusExec, vEB) then
        vEB.Execute(fBoard, C_ProgrammerMethodStarted, nil);
      vEB := nil;
    end;
    C_ProgrammerEnded:
    begin
      if Assigned(FinalProgInstance) then
        FreeAndNil(FinalProgInstance);
      if Supports(fBoard, IBoardEventBusExec, vEB) then
        vEB.Execute(fBoard, C_ProgrammerMethodEnded, nil);
      vEB := nil;
    end;
  end;
end;

procedure TBoardStpExtentionManager.SetState(AState: Byte);
begin
  if Assigned(CurrMethodInstance) then
  begin  
    if CurrMethodInstance.ThreadExists then
      CurrMethodInstance.Stop;
    if (AState >  High(MeasureMethods))
    or (not Assigned(MeasureMethods[AState]))
    or (not (CurrMethodInstance is MeasureMethods[AState])) then
      FreeAndNil(CurrMethodInstance);
  end;
  fCurrStpState := AState;
  if (not Assigned(CurrMethodInstance))
  and (AState <=  High(MeasureMethods))
  and (Assigned(MeasureMethods[fCurrStpState])) then
    CurrMethodInstance := MeasureMethods[fCurrStpState].Create(fMainProc, fBoardProc, fBoard, FWindowHandle);
end;

procedure TBoardStpExtentionManager.Start;
begin
  if Assigned(CurrMethodInstance) then
  begin
    if CurrMethodInstance.ThreadExists then
      CurrMethodInstance.Stop;
    if (fCurrStpState >  High(MeasureMethods))
    or (not Assigned(MeasureMethods[fCurrStpState]))
    or (not (CurrMethodInstance is MeasureMethods[fCurrStpState])) then
    begin
      FreeAndNil(CurrMethodInstance);
      if (fCurrStpState <=  High(MeasureMethods))
      and (Assigned(MeasureMethods[fCurrStpState])) then
        CurrMethodInstance := MeasureMethods[fCurrStpState].Create(fMainProc, fBoardProc, fBoard, FWindowHandle);
    end;
  end else if (fCurrStpState <=  High(MeasureMethods))
      and (Assigned(MeasureMethods[fCurrStpState])) then
    CurrMethodInstance := MeasureMethods[fCurrStpState].Create(fMainProc, fBoardProc, fBoard, FWindowHandle);
  if Assigned(CurrMethodInstance) then
    CurrMethodInstance.Start;
end; 

procedure TBoardStpExtentionManager.PrepareProgrammer;
begin
  if Assigned(FinalProgClass) then
  begin
    if not Assigned(FinalProgInstance) then
      FinalProgInstance := FinalProgClass.Create(fMainProc, fBoardProc, fBoard, FWindowHandle);
    FinalProgInstance.Prepare;
  end;
end; 

function TBoardStpExtentionManager.ReadyToProg: Boolean;
begin
  Result := Assigned(FinalProgInstance) and FinalProgInstance.ReadyToStart;
end;

procedure TBoardStpExtentionManager.StartFinalProgramming;
begin
  if Assigned(FinalProgInstance)
  and FinalProgInstance.ReadyToStart then
    FinalProgInstance.Start;
end;  

function TBoardStpExtentionManager.ExternalProgPowerNeded: Boolean;
begin
  Result := Assigned(FinalProgInstance) and FinalProgInstance.ReadyToStart and FinalProgInstance.ExternalPowerNeded;
end;  

procedure TBoardStpExtentionManager.DoneProgrammer;
begin
  if Assigned(FinalProgInstance) then
  begin
    if FinalProgInstance.ThreadExists then     
      FinalProgInstance.Stop;
    FreeAndNil(FinalProgInstance);
  end;
end;

procedure TBoardStpExtentionManager.Stop;
begin
  if Assigned(CurrMethodInstance) then
    CurrMethodInstance.Stop;
  if Assigned(FinalProgInstance) then
    FinalProgInstance.Stop;
end; 

function TBoardStpExtentionManager.ReadyToStart: Boolean;
begin
  if (not Assigned(CurrMethodInstance))
      and (fCurrStpState <=  High(MeasureMethods))
      and (Assigned(MeasureMethods[fCurrStpState])) then
    CurrMethodInstance := MeasureMethods[fCurrStpState].Create(fMainProc, fBoardProc, fBoard, FWindowHandle);
  Result := Assigned(CurrMethodInstance) and CurrMethodInstance.ReadyToStart;
end;

procedure TBoardStpExtentionManager.Reinit;
begin
  if Assigned(CurrMethodInstance) then
  begin
    if CurrMethodInstance.ThreadExists then
      Exit;
    FreeAndNil(CurrMethodInstance);
    if (fCurrStpState <=  High(MeasureMethods))
    and (Assigned(MeasureMethods[fCurrStpState])) then
      CurrMethodInstance := MeasureMethods[fCurrStpState].Create(fMainProc, fBoardProc, fBoard, FWindowHandle);
  end else if (fCurrStpState <=  High(MeasureMethods))
      and (Assigned(MeasureMethods[fCurrStpState])) then
    CurrMethodInstance := MeasureMethods[fCurrStpState].Create(fMainProc, fBoardProc, fBoard, FWindowHandle);
end;

function TBoardStpExtentionManager.Worked: boolean;
begin
  Result := (Assigned(CurrMethodInstance) and CurrMethodInstance.ThreadExists)
         or (Assigned(FinalProgInstance) and FinalProgInstance.ThreadExists);
end;

procedure TBoardStpExtentionManager.FillMethods(ProcAddr: Pointer; AStpState: byte);
var
vStpMethodClassGetter : TStpMethodClassGetter;
begin
  @vStpMethodClassGetter := ProcAddr;
  MeasureMethods[AStpState] := vStpMethodClassGetter;
end;

end.
