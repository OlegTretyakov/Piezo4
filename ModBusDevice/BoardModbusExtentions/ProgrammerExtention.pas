unit ProgrammerExtention;

interface

uses
  System.Classes,
  System.Threading,
  AbstractExtention,
  ByListPositionInstallerInterface,
  dmProgrammerInterface,
  PositionInterface,
  ChipAbstractInterface,
  Vodopad.EventList,
  EventBusInterface,
  mbpc21001Interface;

type
  TPosListProgrammerExtention = class;

  TPositionProgrammerExtention = class(TAbstractExtention, IdmPositionProgrammer)
   private
    fBoardPos : Byte;
    fProgrammerExt : TPosListProgrammerExtention;
    fPosition : IPosition;
    fPositionChip : IChipAbstract;
   protected
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    {IdmPositionProgrammer}
    function GetRegisters : boolean; overload; stdcall;
    function GetRegisters(const ADest : IChipAbstract) : boolean; overload;  stdcall;
    function SetRegisters : boolean;overload; stdcall;
    function SetRegisters(const ASource : IChipAbstract) : boolean; overload; stdcall;
  end;
  
  TPosListProgrammerExtention = class(TAbstractExtention,
                                      IByListPositionInstaller,
                                      IdmProgrammer,
                                      IEventBus)
   private
    fLastError : TProgrammerError;
    fStartTask : ITask;
    fAutoFrezedModules : TList;
    fFrezedProcessed : Boolean;
    fProgrammerController : Imbpc21001Module;
    fProgrammers : TList;
    fEventSubscribers : TCustomObjEventList;
    procedure FreezeModules;
    procedure UnFreezeModules;
    function FindProgrammerAddr(const AProgrammer : TGUID; out oAddr:Word):boolean;
    function FindProgrammerIdx(const AProgrammer : TGUID; out oIdx:byte):boolean;overload;
    function FindProgrammerIdx(const AAddr:Word; out oIdx:byte):boolean;overload;
    function ActiveCountByGuid(const AProgrammer : TGUID):Word;
    function PositionProcessed(BoardPos: Byte; const AProgrammer: TGUID): Boolean;
    procedure OnProgrammerEvent(Sender : TObject; Event : TGUID; Params : Pointer);
    function SendFromPositionToBoard(APosition: Byte):boolean;
   protected
   { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    {IByListPositionInstaller}
    function InstalledPositionExtClass : TAbstractExtentionClass;stdcall;  
    {IEventBus}
    procedure IEventBus.Add = EventMethodAdd;
    procedure IEventBus.Remove = EventMethodRemove;
    procedure EventMethodAdd(const AMethod : TCustomObjEvent); stdcall;
    procedure EventMethodRemove(const AMethod: TCustomObjEvent); stdcall;
    {IdmProgrammer}
    function ActiveProgrammerSelected: Boolean; stdcall;
    procedure FillSupportedOperations(const AProgrammer : TGUID; AOperations : TStrings);stdcall;
    function ProgrammerExists(const AProgrammer : TGUID):Boolean; stdcall;
    function GetActiveProgrammer : TGUID;stdcall;
    procedure SetActiveProgrammer(const AProgrammer : TGUID);stdcall;
    procedure StartOperationAsyn(AOperation : Cardinal; ACopyFromPositions : Boolean);stdcall;
    function StartOperation(AOperation : Cardinal; ACopyFromPositions : Boolean):TProgrammerError;stdcall;
    function GetLastError : TProgrammerError; stdcall;
    function GetRegisters(ABoardPosition : Byte; const ADest : IChipAbstract) : boolean; stdcall;
    function SetRegisters(ABoardPosition : Byte; const ASource : IChipAbstract) : boolean; stdcall;
   public
    property ActiveProgrammer:TGUID  read GetActiveProgrammer write SetActiveProgrammer;
    property LastError : TProgrammerError read fLastError;
  end;


implementation
uses
  System.SysUtils,
  AbstractDeviceInterface,
  PositionListInterface,
  ExtentionsListInterface,
  dmChallengeControllerInterface,
  dmChannelsInterface,
  dmReadInterface,
  dmWriteInterface,
  mbProgrammerInterface;

const
C_NULL_PROG : TGUID = '{00000000-0000-0000-0000-000000000000}';

{ TPositionProgrammerExtention }

procedure TPositionProgrammerExtention.AfterCreate;
var
  vExtList : IExtentions;
begin
  inherited;
  fPositionChip := nil;
  fBoardPos := 0;
  fProgrammerExt := nil;
  fPosition := nil;
  if Assigned(Owner) and Assigned(Owner.Owner) then
  begin
    if Supports(Owner.Owner, IPosition, fPosition) then
    begin
      fBoardPos := fPosition.BoardPos;
      if Supports(fPosition, IExtentions, vExtList) then
      begin  
        if not vExtList.Find(IChipAbstract, fPositionChip) then
        begin
          fPositionChip := nil;
          fPosition := nil;
        end;
      end else
        fPosition := nil;
      vExtList := nil;
    end;
    if Assigned(Owner.Owner.Owner)
      and Supports(Owner.Owner.Owner, IExtentions, vExtList) then
    begin
      if not vExtList.Find(TPosListProgrammerExtention, fProgrammerExt) then
      begin
        fProgrammerExt := nil;
        fPositionChip := nil;
        fBoardPos := 0;
        fPosition := nil;
      end;
    end;
    vExtList := nil;
  end;
end;

procedure TPositionProgrammerExtention.BeforeDestroy;
begin
  fPositionChip := nil;
  fBoardPos := 0;
  fProgrammerExt := nil;
  fPosition := nil;
  inherited;
end;

function TPositionProgrammerExtention.GetRegisters: boolean;
begin
  Result := Assigned(fPositionChip) and GetRegisters(fPositionChip);
end;

function TPositionProgrammerExtention.GetRegisters(const ADest: IChipAbstract): boolean;
begin
  Result := (fBoardPos > 0)
      and Assigned(fProgrammerExt)
      and Assigned(ADest);
  if Result then
    Result := fProgrammerExt.GetRegisters(fBoardPos, ADest);
end;

function TPositionProgrammerExtention.SetRegisters: boolean;
begin
  Result := Assigned(fPositionChip) and SetRegisters(fPositionChip);
end;

function TPositionProgrammerExtention.SetRegisters(const ASource: IChipAbstract): boolean;
begin
  Result := (fBoardPos > 0)
      and Assigned(fProgrammerExt)
      and Assigned(ASource);
  if Result then
    Result := fProgrammerExt.SetRegisters(fBoardPos, ASource);
end;


{ TPosListProgrammerExtention }

procedure TPosListProgrammerExtention.AfterCreate;
var
  vDM : IDeviceModules;
  vPCObj : TObject;
  vPG : ImbProgrammer;
  vEB : IEventBus;
  vIdx : Byte;
  vIdx1 : Word;
begin
  inherited;
  fStartTask := nil;
  fAutoFrezedModules := TList.Create;
  fFrezedProcessed := false;
  fProgrammerController := nil;
  fEventSubscribers := TCustomObjEventList.Create;
  fProgrammers := TList.Create;
  vIdx := 0;
  if Assigned(Owner)
  and Assigned(Owner.Owner)
  and Supports(Owner.Owner.Owner, IDeviceModules, vDM) then
  begin
    if vDM.FindBaseAddr(6000, vIdx, vPCObj)
    and Supports(vPCObj, Imbpc21001Module, fProgrammerController) then
    begin
      vIdx1 := 0;
      while vDM.Find(vIdx1, ImbProgrammer, vPG) do
      begin
        if Supports(vPG, IEventBus, vEB) then
          vEB.Add(OnProgrammerEvent);
        fProgrammers.Add(Pointer(vPG));
        vEB := nil;
        vPG := nil;
        Inc(vIdx1);
      end;
    end else
      fProgrammerController := nil;
  end;
  vDM := nil;
end;

procedure TPosListProgrammerExtention.BeforeDestroy;
var  
  vEB : IEventBus;
  vIdx : Byte;
begin
  vIdx := 0;
  while vIdx < fProgrammers.Count do
  begin
    if Supports(ImbProgrammer(fProgrammers[vIdx]), IEventBus, vEB) then
      vEB.Remove(OnProgrammerEvent);
    vEB := nil;
    fProgrammers[vIdx] := nil;
    inc(vIdx);
  end;
  vIdx := 0;
  while vIdx < fAutoFrezedModules.Count do
  begin
    fAutoFrezedModules[vIdx] := nil;
    inc(vIdx);
  end;
  fProgrammers.Clear;
  FreeAndNil(fProgrammers);
  FreeAndNil(fEventSubscribers);
  fAutoFrezedModules.Clear;
  FreeAndNil(fAutoFrezedModules);
  inherited;
end;

procedure TPosListProgrammerExtention.FreezeModules;
var
  vModules : IDeviceModules;
  vController : IdmChallengeController;
  vIdx : Word;
begin
  if not fFrezedProcessed then
  begin
    if Supports(Owner.Owner.Owner, IDeviceModules, vModules) then
    begin
      vIdx := 0;
      while vModules.Find(vIdx, IdmChallengeController, vController) do
      begin
        if vController.Auto then
          fAutoFrezedModules.Add(Pointer(vController));
        vController := nil;
        Inc(vIdx);
      end;
      vModules := nil;
    end;
    fFrezedProcessed := True;
  end;
  vIdx := 0;
  while vIdx < fAutoFrezedModules.Count do
  begin
    IdmChallengeController(fAutoFrezedModules[vIdx]).Auto := False;
    Inc(vIdx);
  end;
end;

procedure TPosListProgrammerExtention.UnFreezeModules;
var
  vIdx : Word;
begin
  vIdx := 0;
  while vIdx < fAutoFrezedModules.Count do
  begin
    IdmChallengeController(fAutoFrezedModules[vIdx]).Auto := true;
    Inc(vIdx);
  end;
end;

procedure TPosListProgrammerExtention.OnProgrammerEvent(Sender: TObject; Event: TGUID; Params: Pointer);
var
  vPosList : IPositionsList;
  vPosition : IPosition;
  vExtList : IExtentions;
  vPosProgrammer : TPositionProgrammerExtention;
  vIdx : Word;
begin
  if IsEqualGUID(Event, C_RegistersReadDone) then
  begin
    if Assigned(Owner)
    and Supports(Owner.Owner, IPositionsList, vPosList) then
    begin
      vIdx := 0;
      while vIdx < vPosList.Count do
      begin
        if Supports(vPosList[vIdx], IPosition, vPosition)
        and vPosition.Active
        and Supports(vPosList[vIdx], IExtentions, vExtList)
        and vExtList.Find(TPositionProgrammerExtention, vPosProgrammer) then
          vPosProgrammer.GetRegisters;
        vExtList := nil;
        vPosition := nil;
        Inc(vIdx);
      end;
      vPosList := nil;
    end;
  end else
  begin
    if IsEqualGUID(Event, C_ProgrammerOperationFinished) then
    begin
      fLastError := peNoErrors;
      fEventSubscribers.Execute(Self, C_OnProgrammerOperationSucc, nil);
    end
    else if IsEqualGUID(Event, C_ProgrammerOperationTimeOut) then
    begin
      fLastError := peOperationTimeOut;
      fEventSubscribers.Execute(Self, C_OnProgrammerOperationTimeOut, nil);
    end;
    UnFreezeModules;
  end;
end;

procedure TPosListProgrammerExtention.EventMethodAdd(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Add(AMethod);
end;

procedure TPosListProgrammerExtention.EventMethodRemove(const AMethod: TCustomObjEvent);
begin
  fEventSubscribers.Remove(AMethod);
end;

function TPosListProgrammerExtention.InstalledPositionExtClass: TAbstractExtentionClass;
begin
  result := TPositionProgrammerExtention;
end;

function TPosListProgrammerExtention.PositionProcessed(BoardPos : Byte; const AProgrammer: TGUID):Boolean;
var
  vPosList : IPositionsList;
  vPositionObj : TObject;
  vPosition : IPosition;
  vExtList : IExtentions;
  vIdx : Integer;
  vChip : IChipAbstract;
begin
  Result := False;
  if Assigned(Owner)
  and Supports(Owner.Owner, IPositionsList, vPosList) then
  begin
    vIdx := vPosList.IndexOf(BoardPos);
    if vIdx = -1 then
    begin
      vPosList := nil;
      Exit;
    end;
    vPositionObj := vPosList.Items[Word(vIdx)];
    Result := Supports(vPositionObj, IPosition, vPosition)
      and vPosition.Active
      and Supports(vPositionObj, IExtentions, vExtList)
      and vExtList.Find(IChipAbstract, vChip)
      and IsEqualGUID(vChip.ChipGUID, AProgrammer);
    vChip := nil;
    vExtList := nil;
    vPosition := nil;
    vPosList := nil;
  end;
end;

function TPosListProgrammerExtention.SendFromPositionToBoard(APosition : Byte):boolean;
var
  vPosList : IPositionsList;
  vExtList : IExtentions;
  vProgrammer : IdmPositionProgrammer;
  vIdx : integer;
begin
  Result := Assigned(Owner)
  and Supports(Owner.Owner, IPositionsList, vPosList);
  if result then
  begin
    vIdx := vPosList.IndexOf(APosition);
    result := vIdx <> -1;
  end;
  if result then
    result := vPosList.QueryItem(vIdx, IExtentions, vExtList)
      and vExtList.Find(IdmPositionProgrammer, vProgrammer)
      and vProgrammer.SetRegisters;
  vProgrammer := nil;
  vExtList := nil;
  vPosList := nil;
end;

function TPosListProgrammerExtention.ProgrammerExists(const AProgrammer: TGUID): Boolean;
var
  vID : Word;
begin
  Result := FindProgrammerAddr(AProgrammer, vID);
end;

function TPosListProgrammerExtention.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IsEqualGUID(IID, IByListPositionInstaller) then
  begin
    if fProgrammers.Count > 0 then
      result := inherited QueryInterface(IID, Obj)
    else
      Result := E_NOINTERFACE;
  end else
    result := inherited QueryInterface(IID, Obj);
end;

function TPosListProgrammerExtention.ActiveCountByGuid(const AProgrammer: TGUID): Word;
var
  vPosList : IPositionsList;
  vPositionObj : TObject;
  vPosition : IPosition;
  vExtList : IExtentions;
  vIdx : Word;
  vChip : IChipAbstract;
begin
  Result := 0;
  if Assigned(Owner)
  and Supports(Owner.Owner, IPositionsList, vPosList) then
  begin
    vIdx := 0;
    while vIdx < vPosList.Count do
    begin
      vChip := nil;
      vExtList := nil;
      vPosition := nil;
      vPositionObj := vPosList.Items[vIdx];
      if Supports(vPositionObj, IPosition, vPosition)
      and vPosition.Active
      and Supports(vPositionObj, IExtentions, vExtList)
      and vExtList.Find(IChipAbstract, vChip)
      and IsEqualGUID(vChip.ChipGUID, AProgrammer) then
        inc(Result);
      vChip := nil;
      vExtList := nil;
      vPosition := nil;
      Inc(vIdx);
    end;
    vPosList := nil;
  end;
end;

function TPosListProgrammerExtention.ActiveProgrammerSelected: Boolean;
var
  vAddr : Word;
  vIdx : Byte;
begin
  Result := Assigned(fProgrammerController);
  if not Result then
    Exit;
  vAddr := fProgrammerController.ActiveProgrammerBaseAddr;
  result := FindProgrammerIdx(vAddr, vIdx);
end;

procedure TPosListProgrammerExtention.FillSupportedOperations(
  const AProgrammer: TGUID; AOperations: TStrings);
var
  vIdx: byte;
begin
  AOperations.BeginUpdate;
  try
    AOperations.Clear;
    if FindProgrammerIdx(AProgrammer, vIdx) then
       ImbProgrammer(fProgrammers[vIdx]).FillSupportedOperations(AOperations);
  finally
    AOperations.EndUpdate;
  end;
end;

function TPosListProgrammerExtention.FindProgrammerAddr(const AProgrammer: TGUID; out oAddr: Word): boolean;
var
  vIdx : Byte;
begin
  vIdx := 0;
  while (vIdx < fProgrammers.Count) do
  begin
    if ImbProgrammer(fProgrammers[vIdx]).ChipSupported(AProgrammer) then
    begin
      oAddr := ImbProgrammer(fProgrammers[vIdx]).BaseAddress;
      Break;
    end;
    Inc(vIdx);
  end;
  Result := (vIdx < fProgrammers.Count);
end;

function TPosListProgrammerExtention.FindProgrammerIdx(const AAddr: Word; out oIdx: byte): boolean;
begin
  oIdx := 0;
  while (oIdx < fProgrammers.Count) do
  begin
    if ImbProgrammer(fProgrammers[oIdx]).BaseAddress = AAddr then
      Break;
    Inc(oIdx);
  end;
  Result := (oIdx < fProgrammers.Count);
end;

function TPosListProgrammerExtention.FindProgrammerIdx(const AProgrammer: TGUID; out oIdx: byte): boolean;
begin
  oIdx := 0;
  while (oIdx < fProgrammers.Count) do
  begin
    if ImbProgrammer(fProgrammers[oIdx]).ChipSupported(AProgrammer) then
      Break;
    Inc(oIdx);
  end;
  Result := (oIdx < fProgrammers.Count);
end; 

function TPosListProgrammerExtention.GetActiveProgrammer: TGUID;
var
  vAddr : Word;
  vIdx : Byte;
begin
  Result := C_NULL_PROG;
  if not Assigned(fProgrammerController) then
     Exit;
  vAddr := fProgrammerController.ActiveProgrammerBaseAddr;
  if FindProgrammerIdx(vAddr, vIdx) then
    result := ImbProgrammer(fProgrammers[vIdx]).SupportedChip;
end;

procedure TPosListProgrammerExtention.SetActiveProgrammer(const AProgrammer: TGUID);
var
  vActive : TGUID;
  vAddr : Word;
begin
  if ActiveProgrammerSelected then
  begin
    vActive := GetActiveProgrammer;
    if (not IsEqualGUID(vActive, AProgrammer))
    and FindProgrammerAddr(AProgrammer, vAddr) then
     fProgrammerController.ActiveProgrammerBaseAddr := vAddr;
  end else
  if Assigned(fProgrammerController)
    and FindProgrammerAddr(AProgrammer, vAddr) then
    fProgrammerController.ActiveProgrammerBaseAddr := vAddr;
end;

function TPosListProgrammerExtention.SetRegisters(ABoardPosition: Byte; const ASource: IChipAbstract): boolean;
var
  vIdx : Byte;
begin
  Result := Assigned(ASource)
          and FindProgrammerIdx(ASource.ChipGUID, vIdx)
          and ImbProgrammer(fProgrammers[vIdx]).SetRegisters(ABoardPosition, ASource);
end;

procedure TPosListProgrammerExtention.StartOperationAsyn(AOperation : Cardinal; ACopyFromPositions : Boolean);
begin
  if Assigned(fStartTask) then
    fStartTask.Wait(30000);
  fStartTask := TTask.Run(
    procedure
    var
      vEnabled : Boolean;
      vIdx, vChIndex : Byte;
      vAddr : Word;
      vActive : TGUID;
      vChannels : IdmChannels;
      vResult : TProgrammerError;
    begin
      vResult := peAnyError;
      FreezeModules;
      try
        Sleep(50);
        try
          if ActiveProgrammerSelected then
          begin
            vAddr := fProgrammerController.ActiveProgrammerBaseAddr;
            if FindProgrammerIdx(vAddr, vIdx) then
            begin
              vActive := ImbProgrammer(fProgrammers[vIdx]).SupportedChip;
              if (ActiveCountByGuid(vActive) > 0)
              and Supports(ImbProgrammer(fProgrammers[vIdx]), IdmChannels, vChannels) then
              begin
                TThread.Synchronize(nil,
                procedure
                begin
                  fEventSubscribers.Execute(Self, C_ProgrammerStarted, nil);
                end
                );
                vChIndex := 0;
                while vChIndex < vChannels.Count do
                begin
                  vEnabled := PositionProcessed(vChIndex+1, vActive);
                  //ImbProgrammer(fProgrammers[vIdx]).Enabled[vChIndex] := vEnabled;
                  if vEnabled and ACopyFromPositions then
                    vEnabled := SendFromPositionToBoard(vChIndex+1);
                  ImbProgrammer(fProgrammers[vIdx]).Enabled[vChIndex] := vEnabled;
                  Inc(vChIndex);
                end;
                if ImbProgrammer(fProgrammers[vIdx]).StartOperation(AOperation) then
                  vResult := peNoErrors
                else
                  vResult := peErrorOnStart;
                vChannels := nil;
              end else
                vResult:= peNoActivePositions;
            end else
              vResult:= peNoModuleFinded;
          end else
            vResult:= peNoSelectedProgrammer;
        except
          vResult := peException;
        end;
      finally
        fLastError := vResult;
        if vResult <> peNoErrors then
        begin
          UnFreezeModules;
          TThread.Synchronize(nil,
          procedure
          begin
            fEventSubscribers.Execute(Self, C_ProgrammerStartError, Pointer(vResult));
          end
          );
        end;
        fStartTask := nil;
      end;
    end
  );
end;

function TPosListProgrammerExtention.StartOperation(AOperation: Cardinal; ACopyFromPositions: Boolean): TProgrammerError;
var
  vEnabled : Boolean;
  vIdx, vChIndex : Byte;
  vAddr : Word;
  vActive : TGUID;
  vChannels : IdmChannels;
begin
  Result := peAnyError;
  FreezeModules;
  try
    Sleep(50);
    try
      if ActiveProgrammerSelected then
      begin
        vAddr := fProgrammerController.ActiveProgrammerBaseAddr;
        if FindProgrammerIdx(vAddr, vIdx) then
        begin
          vActive := ImbProgrammer(fProgrammers[vIdx]).SupportedChip;
          if (ActiveCountByGuid(vActive) > 0)
          and Supports(ImbProgrammer(fProgrammers[vIdx]), IdmChannels, vChannels) then
          begin
            TThread.Queue(nil,
            procedure
            begin
              fEventSubscribers.Execute(Self, C_ProgrammerStarted, nil);
            end
            );
            vChIndex := 0;
            while vChIndex < vChannels.Count do
            begin
              vEnabled := PositionProcessed(vChIndex+1, vActive);
              //ImbProgrammer(fProgrammers[vIdx]).Enabled[vChIndex] := vEnabled;
              if vEnabled and ACopyFromPositions then
                vEnabled := SendFromPositionToBoard(vChIndex+1);
              ImbProgrammer(fProgrammers[vIdx]).Enabled[vChIndex] := vEnabled;
              Inc(vChIndex);
            end;
            if ImbProgrammer(fProgrammers[vIdx]).StartOperation(AOperation) then
              Result := peNoErrors
            else
              Result := peErrorOnStart;
            vChannels := nil;
          end else
            Result:= peNoActivePositions;
        end else
          Result:= peNoModuleFinded;
      end else
        Result:= peNoSelectedProgrammer;
    except
      Result := peException;
    end;
  finally
    if Result <> peNoErrors then
      UnFreezeModules;
    fLastError := Result;
  end;
end;

function TPosListProgrammerExtention.GetRegisters(ABoardPosition: Byte; const ADest: IChipAbstract): boolean;
var
  vIdx : Byte;
begin
  Result := Assigned(ADest)
          and FindProgrammerIdx(ADest.ChipGUID, vIdx)
          and ImbProgrammer(fProgrammers[vIdx]).GetRegisters(ABoardPosition, ADest);
end;

function TPosListProgrammerExtention.GetLastError: TProgrammerError;
begin
  result := fLastError;
end;

end.
