unit mbfm20301Module;

{Измеритель частоты}

interface
uses
  System.Classes, DeviceModule, dmFreqMeasurerInterface,
  ModBusDeviceInterface, dmReadInterface, dmChannelsInterface,
  Vodopad.FloatList, Vodopad.ObjectList, Vodopad.Timer,
  ModBusDriver, AnalogBLock, DiscreteBlock, System.Generics.Collections;
type

  TIniConfigMeasParams = record
    OnceTime, MultiplicityTime {mSec*100}: Word;
    Multiplicity,
    StaticFilterValue : Byte;
    procedure Read;
  end;

  pIniConfigMeasParams = ^TIniConfigMeasParams;

  TPosition = class(TComponent)
   private
    fResult : TFreqResult;
    fFreqList : TxFloatList;
    procedure FreqClear;
    function MeanFreq: double;
    procedure SetFreq(const AValue : double; DoAdd : Boolean = true);
   public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ApplyStaticFilter(AFilterValue : byte);
  end;

  TQIState = (QiAdd, QiStart, QiDone, QiSucc);
  TMeasQueueItem = record
    MeasTime : Word;
    State : TQIState;
  end;
  
  pMeasQueueItem = ^TMeasQueueItem;
  TMeasQueue = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Get(const Index: Integer): pMeasQueueItem;
   public
    function Add(MeasTime : Word) : integer;
    function StateCount(State : TQIState) : Byte;
    property Items[const Index: Integer]: pMeasQueueItem read Get; default;
    function First(State : TQIState; out Item : pMeasQueueItem):Boolean;
  end;

  TPermanentlyMeasureMode = record
    Time : Word;{mSec*100}
    Enabled : Boolean;
  end;
  TFreqModuleDiscreteBlock = class(TDiscreteBlock)
  public
    property ValuesReaded;
  end;
  Tmbfm20301Module = class(TDeviceModule,
                        IdmChannels,
                        IdmRead,
                        IdmFreqStarter,
                        IdmFreqController,
                        IdmFreqResults)
   private
    fCalculatedTimeOut : Cardinal;
    fTimeOutTimer : TvdTimer;
    fPermanentlyMeasureMode : TPermanentlyMeasureMode;
    fFreqBlocks : TList<TAnalogBlock>;
    fMeasureStartBlock :TFreqModuleDiscreteBlock; //coil offset=0
    fMeasRdyBlock : TFreqModuleDiscreteBlock;//discrete offset=0
    fMeasStateBlock : TAnalogBlock;//input offset=3
    fMeasTimeBlock : TAnalogBlock;//holding offset=0
    fMeasureQueue : TMeasQueue;
    FMeasQueueState : record
      TotalCount,
      SuccCont : byte;
    end;
    fIniMeasParams : TIniConfigMeasParams;
    fPositionList : TExObjectList;
    fAutoFrezedModules : TList;
    fFrezedProcessed : Boolean;
    procedure FreezeModules;  
    procedure ChallengeFreezed;
    procedure UnFreezeModules;
    procedure OnRdyBlockChanged(Sender : TObject);
    procedure OnFreqTimeOut(Sender : TObject);
    procedure FreqRead;
    function FreqStart(AMeasTime{mSec*100}: Word): boolean;
    procedure ClearBuffers;
    procedure FreqDone(Event: TGUID);
    {IdmChannels}
    function GetChannelsCount : word; stdcall;
    {IdmFreqStarter}
    function StartOnceMeasure: Boolean; overload; stdcall;
    function StartOnceMeasure(ATime{mSec*100}:Word): Boolean; overload; stdcall;
    function StartSeriesMeasure: Boolean; overload; stdcall;
    function StartSeriesMeasure(ATime{mSec*100}:Word; AQuant : Byte): Boolean; overload; stdcall;
    function StartPermanentlyMeasure: Boolean; overload; stdcall;
    function StartPermanentlyMeasure(ATime{mSec*100}:Word): Boolean; overload; stdcall;
    procedure StopPermanentlyMeasure; stdcall;
    function CalculatedTimeOut : Cardinal;stdcall;
    {IdmFreqController}
    function GetSeriesCount: Byte; stdcall;
    function GetSeriesSuccesCount: Byte; stdcall;
    {IdmRead}
    function Read(Params : Pointer=nil): Boolean; stdcall;
    {IdmFreqResults}
    function FreqResult(AIndex : Word; const AResult : pFreqResult):Boolean; stdcall;
   protected
    procedure AfterCreate;override;
    procedure BeforeDestroy;override;
  end;


implementation   
uses System.SysUtils, Vodopad.Math, System.IniFiles, Vodopad.CustomIniHelpers,
System.DateUtils, AbstractTag, AbstractDeviceInterface, dmChallengeControllerInterface,
  Winapi.Windows;

const
C_ID : Word = 203;
//C_ID : Word = 03;
C_Ver : Word = 01;

function GetMbModuleClassType : TDeviceModuleClass; stdcall;
begin
  result := Tmbfm20301Module;
end; exports GetMbModuleClassType;

function GetIDFunc : Word; stdcall;
begin
  result := C_ID;
end; exports GetIDFunc;

function GetVerFunc : Word; stdcall;
begin
  result := C_Ver;
end; exports GetVerFunc;



{ Tmbfm20301Module }
procedure TIniConfigMeasParams.Read;
var
f : TIniFile;
vIniName : TFileName;
vCreate : boolean;
begin
  vIniName := ChangeFileExt(GetModuleName(HInstance), '.ini');
  vCreate := not FileExists(vIniName);
  f := TIniFile.Create(vIniName);
  try
    if not vCreate then
    begin
      if not f.ValueExists('Once measure params', 'MeasureTime') then
      begin
        f.WriteInteger('Once measure params', 'MeasureTime', 5);
        f.UpdateFile;
      end
      else
        OnceTime :=
        Word(f.ReadInteger('Once measure params', 'MeasureTime', 5));

      if not f.ValueExists('Series measure params', 'Multiplicity') then
      begin
        f.WriteInteger('Series measure params', 'Multiplicity', 5); 
        f.UpdateFile;
      end
      else
        Multiplicity :=
        byte(f.ReadInteger('Series measure params', 'Multiplicity', 5));

      if not f.ValueExists('Series measure params', 'MeasureTime') then
      begin
        f.WriteInteger('Series measure params', 'MeasureTime', 5); 
        f.UpdateFile;
      end
      else
        MultiplicityTime :=
        Word(f.ReadInteger('Series measure params', 'MeasureTime', 4));
      
      if not f.ValueExists('Series measure params', 'StaticFilterValue') then
      begin
        f.WriteInteger('Series measure params', 'StaticFilterValue', 10);
        f.UpdateFile;
      end
      else
        StaticFilterValue :=
        byte(f.ReadInteger('Series measure params', 'StaticFilterValue', 10));
    end else
    begin
      f.WriteInteger('Once measure params', 'MeasureTime', 5);
      f.WriteInteger('Series measure params', 'Multiplicity', 5);
      f.WriteInteger('Series measure params', 'MeasureTime', 4);
      f.WriteInteger('Series measure params', 'StaticFilterValue', 10); 
      f.UpdateFile;
    end;
  finally
    FreeAndNil(f);
  end;
end;

procedure Tmbfm20301Module.AfterCreate;
var
vModuleInfoBlock : TAnalogBlock;
vPositionsCount, vMemOffSet, vWordsTotalSize, vBlockSize, vElementIdx: Word;
begin
  vPositionsCount := 0;
  fTimeOutTimer := TvdTimer.Create(self);
  fTimeOutTimer.OnTimer := OnFreqTimeOut;
  fTimeOutTimer.Enabled := false;
  fAutoFrezedModules := TList.Create;
  fFrezedProcessed := false;
  fPermanentlyMeasureMode.Enabled := False;
  fPositionList := TExObjectList.Create(False);
  vModuleInfoBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 3, true);
  try
    if vModuleInfoBlock.ReadSyn then
    begin
      fID := vModuleInfoBlock.Values[0];
      fVersion := vModuleInfoBlock.Values[1];
      vPositionsCount := vModuleInfoBlock.Values[2];
    end else
      Exit;
  finally
    FreeAndNil(vModuleInfoBlock);
  end;
  fMeasureStartBlock := fModbusDevice.Driver.CreateDiscrete(TFreqModuleDiscreteBlock,
                        self,
                        fModbusDevice.Station,
                        BaseAddress,
                        1,
                        false) as TFreqModuleDiscreteBlock;
  fMeasRdyBlock := fModbusDevice.Driver.CreateDiscrete(TFreqModuleDiscreteBlock,
                        self,
                        fModbusDevice.Station,
                        BaseAddress,
                        1,
                        true) as TFreqModuleDiscreteBlock;
  fMeasRdyBlock.FireAsync := True;
  fMeasStateBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress+3, 1, true);
  fMeasTimeBlock := fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, BaseAddress, 1, false);


  fFreqBlocks := TList<TAnalogBlock>.Create;
  vMemOffSet := BaseAddress +100;
  vWordsTotalSize := vPositionsCount*4;
  vElementIdx := 0;
  while fPositionList.Count < vPositionsCount do
  begin
    if (fFreqBlocks.Count = 0)
    or (vElementIdx = 120) then
    begin
      if vWordsTotalSize > 120 then
      begin
        vBlockSize := 120;
        dec(vWordsTotalSize, vBlockSize);
      end else
        vBlockSize := vWordsTotalSize;
      vElementIdx := 0;
      fFreqBlocks.Add(fModbusDevice.Driver.CreateAnalog(self, fModbusDevice.Station, vMemOffSet, TAnalogSize(vBlockSize), true));
    end;
    fPositionList.Add(TPosition.Create(self));
    inc(vElementIdx, 4);
    inc(vMemOffSet, 4);
  end;
  fMeasureQueue := TMeasQueue.Create;
end;

procedure Tmbfm20301Module.BeforeDestroy;
var
vIdx : Byte;
begin
  fTimeOutTimer.Enabled := false;
  if Assigned(fFreqBlocks) then
  begin
    fFreqBlocks.Clear;
    FreeAndNil(fFreqBlocks);
  end;
  if assigned(fPositionList) then
  begin
    fPositionList.Clear;
    FreeAndNil(fPositionList);
  end;
  if Assigned(fMeasureQueue) then
  begin
    fMeasureQueue.Clear;
    FreeAndNil(fMeasureQueue);
  end;
  vIdx := 0;
  while vIdx < fAutoFrezedModules.Count do
  begin
    fAutoFrezedModules[vIdx] := nil;
    Inc(vIdx);
  end;
  fAutoFrezedModules.Clear;
  FreeAndNil(fAutoFrezedModules);
end;

function Tmbfm20301Module.Read(Params: Pointer): Boolean;
var
vParams : pFreqMeasParams;
begin
  if not Assigned(Params) then
    Result := StartSeriesMeasure
  else
  begin
    vParams := pFreqMeasParams(Params);
    Result := StartSeriesMeasure(vParams.ATime, vParams.AQuant);
  end;
end;

function Tmbfm20301Module.StartSeriesMeasure(ATime{mSec*100}: Word; AQuant : Byte): Boolean;
begin
  Result :=(ATime > 100)
        and (AQuant > 0)
        and (fMeasureQueue.Count = 0);
  if result then
    Exit;
  if not fMeasRdyBlock.AutoRead then
    result := fMeasRdyBlock.ReadSyn;
  if result then
    Result := fMeasRdyBlock.Values[0]
  else
    exit;
  if Result then
  begin
    ClearBuffers;
    while (fMeasureQueue.Count < AQuant) do
      fMeasureQueue.Add(ATime);
    if FreqStart(ATime) then
    begin
      FreezeModules;
      fMeasRdyBlock.ValuesReaded[0] := false;
      fMeasRdyBlock.OnValuesChange := OnRdyBlockChanged;
      fMeasRdyBlock.RefreshInterval := 5;
      fMeasRdyBlock.AutoRead := True;
      fCalculatedTimeOut := (ATime * 100 + 1000) * AQuant + 3000;
      if GetCurrentThreadId <> MainThreadID then
      begin
        TThread.Synchronize(nil,
        procedure
        begin
          fTimeOutTimer.Interval := fCalculatedTimeOut;
          fTimeOutTimer.Enabled := true;
        end
        );
      end else
      begin
        fTimeOutTimer.Interval := fCalculatedTimeOut;
        fTimeOutTimer.Enabled := true;
      end;
    end else
      result := false;
  end;
end;

function Tmbfm20301Module.StartPermanentlyMeasure(ATime: Word): Boolean;
begin
  Result := (ATime > 100);
  if result then
    Exit;
  if fPermanentlyMeasureMode.Enabled then
  begin
    fPermanentlyMeasureMode.Time := ATime;
    Exit;
  end;
  Result := (fMeasureQueue.Count = 0);
  if not result then //measurer in busy. stop and try again
    Exit;
  if not fMeasRdyBlock.AutoRead then
    result := fMeasRdyBlock.ReadSyn;
  if result then
    Result := fMeasRdyBlock.Values[0]
  else
    exit;
  if Result then
  begin
    fPermanentlyMeasureMode.Time := ATime;
    ClearBuffers;
    fMeasureQueue.Add(ATime);
    fPermanentlyMeasureMode.Enabled := true;
    if FreqStart(ATime) then
    begin
      FreezeModules;
      fMeasRdyBlock.ValuesReaded[0] := false;
      fMeasRdyBlock.OnValuesChange := OnRdyBlockChanged;
      fMeasRdyBlock.RefreshInterval := 5;
      fMeasRdyBlock.AutoRead := True;
      fCalculatedTimeOut := (ATime*100+1000) + 3000;
      if GetCurrentThreadId <> MainThreadID then
      begin
        TThread.Synchronize(nil,
        procedure
        begin
          fTimeOutTimer.Interval := fCalculatedTimeOut;
          fTimeOutTimer.Enabled := true;
        end
        );
      end else
      begin
        fTimeOutTimer.Interval := fCalculatedTimeOut;
        fTimeOutTimer.Enabled := true;
      end;
    end else
    begin
      result := false;
      fPermanentlyMeasureMode.Enabled := true;
    end;
  end;
end;

procedure Tmbfm20301Module.OnRdyBlockChanged(Sender : TObject);
var
vMeasQueueItem: pMeasQueueItem;
begin
  if fMeasRdyBlock.Values[0] then
  begin
    if fMeasureQueue.First(QiStart, vMeasQueueItem) then
      FreqRead;
    if fMeasureQueue.First(QiAdd, vMeasQueueItem) then
      FreqStart(vMeasQueueItem.MeasTime);
  end;
end;

function Tmbfm20301Module.FreqStart(AMeasTime{mSec*100}: Word) : boolean;
var
vMeasQueueItem: pMeasQueueItem;
begin
  result := false;
  if not fMeasTimeBlock.ReadSyn then
    exit;
  if fMeasTimeBlock.Values[0] <> AMeasTime then
  begin
    fMeasTimeBlock.Values[0] := AMeasTime;
    result := fMeasTimeBlock.WriteSyn;
  end else
    result := true;
  if not result then
    exit;
  fMeasureStartBlock.Values[0]:= true;
  result := fMeasureStartBlock.WriteSyn;
  if not result then
    exit;
  if fPermanentlyMeasureMode.Enabled then
    ChallengeFreezed;
  if fMeasureQueue.First(QiAdd, vMeasQueueItem) then
  begin
    if (fMeasureQueue.Count > 0)
    and (fMeasureQueue.StateCount(QiAdd)=fMeasureQueue.Count) then
    begin
      if (GetCurrentThreadID <> MainThreadID) then
      begin
        TThread.Synchronize(nil,
        procedure
        begin
          fEventSubscribers.Execute(Self, C_ReadStart, nil);
        end);
      end else
        fEventSubscribers.Execute(Self, C_ReadStart, nil);
    end;
    vMeasQueueItem.State := QiStart;
  end;
end;

procedure Tmbfm20301Module.FreqRead;
function ReadFreqs : Boolean;
var vIdx : Byte;
begin
  Result := false;
  vIdx := 0;
  while vIdx < fFreqBlocks.Count do
  begin
    Result := fFreqBlocks[vIdx].ReadSyn;
    if not Result then
      Break;
    Inc(vIdx);
  end;
end;
var
vBlockIdx, vElemIdx, vPosIdx : Word;
vPosition : TPosition;
vMeasQueueCurrItem: pMeasQueueItem;
vSuccCount, vQueueCount : Byte;
vLogging : Boolean;
vFreqLogStr : string;
vFreq : record
  case integer of
    0: (d: Double);
    1: (a : array [0..3] of Word);
  end;
begin
  if fMeasureQueue.First(QiStart, vMeasQueueCurrItem) then
  try
    vMeasQueueCurrItem.State := QiDone;
    if fMeasStateBlock.ReadSyn then
    begin
      vLogging := Assigned(fModbusDevice.OnMessage);
      try
        if (fMeasStateBlock.Values[0] = 0) then
        begin
          if ReadFreqs then
          begin
            vMeasQueueCurrItem.State := QiSucc;
            vSuccCount := GetSeriesSuccesCount;
            vQueueCount := fMeasureQueue.Count;
            vBlockIdx := 0;
            vPosIdx := 0;
            if vLogging then
              vFreqLogStr := 'Recieved freq data: ';
            while vBlockIdx < fFreqBlocks.Count do
            begin
              vElemIdx := 0;
              while vElemIdx+3 < fFreqBlocks[vBlockIdx].Size do
              begin
                vFreq.a[0] := fFreqBlocks[vBlockIdx].Values[vElemIdx];
                vFreq.a[1] := fFreqBlocks[vBlockIdx].Values[vElemIdx+1];
                vFreq.a[2] := fFreqBlocks[vBlockIdx].Values[vElemIdx+2];
                vFreq.a[3] := fFreqBlocks[vBlockIdx].Values[vElemIdx+3];
                if vPosIdx < fPositionList.Count then
                begin
                  vPosition := TPosition(fPositionList[vPosIdx]);
                  vPosition.SetFreq(vFreq.d, vQueueCount > 1);
                  if vLogging then
                    vFreqLogStr := Format('%s (Pos.%d %n Hz)',[vFreqLogStr, vPosIdx+1, vFreq.d]);
                  if (fIniMeasParams.StaticFilterValue>0)
                  and (vQueueCount > 1)
                  and (vSuccCount >= vQueueCount) then
                    vPosition.ApplyStaticFilter(fIniMeasParams.StaticFilterValue);
                end;
                Inc(vPosIdx);
                Inc(vElemIdx, 4);
              end;
              inc(vBlockIdx);
            end;
            if vLogging then
              fModbusDevice.OnMessage(fModbusDevice, vFreqLogStr);
          end else
          begin
            if vLogging then
              fModbusDevice.OnMessage(fModbusDevice, 'Freq measure error: read freq function returned false');
            if (not fPermanentlyMeasureMode.Enabled)
            and (fMeasureQueue.StateCount(QiDone) * 3 < fMeasureQueue.Count)  then
              fMeasureQueue.Add(vMeasQueueCurrItem.MeasTime);
          end;
        end else
        begin
          if vLogging then
              fModbusDevice.OnMessage(fModbusDevice, 'Freq measure error: measurer state non equals zero');
          if (not fPermanentlyMeasureMode.Enabled)
           and (fMeasureQueue.StateCount(QiDone) * 3 < fMeasureQueue.Count)  then
            fMeasureQueue.Add(vMeasQueueCurrItem.MeasTime);
        end;
      finally
        if (fMeasureQueue.StateCount(QiAdd)=0) then
        begin
          if (fMeasureQueue.StateCount(QiSucc)>0) then
            FreqDone(C_ReadDone)
          else
            FreqDone(C_ReadError);
          if fPermanentlyMeasureMode.Enabled then
          begin
            vMeasQueueCurrItem.State := QiAdd;
            vMeasQueueCurrItem.MeasTime := fPermanentlyMeasureMode.Time;
          end;
        end;
      end;
    end else if (fMeasureQueue.StateCount(QiAdd) = 0) then
      FreqDone(C_ReadError);
  except
    FreqDone(C_ReadError);
  end else
    FreqDone(C_ReadError);
end;

procedure Tmbfm20301Module.FreqDone(Event : TGUID);
begin
  try
    if not fPermanentlyMeasureMode.Enabled then
    begin
      fMeasRdyBlock.OnValuesChange := nil;
      fMeasRdyBlock.AutoRead := false;
    end;
    FMeasQueueState.TotalCount := Byte(fMeasureQueue.Count);
    FMeasQueueState.SuccCont := fMeasureQueue.StateCount(QiSucc);
  finally
    if not fPermanentlyMeasureMode.Enabled then
    begin
      if (GetCurrentThreadID <> MainThreadID) then
      begin
        TThread.Synchronize(nil,
        procedure
        begin
          fTimeOutTimer.Enabled := false;
        end
        );
      end else
         fTimeOutTimer.Enabled := false;
      UnFreezeModules;
    end;
    if (GetCurrentThreadID <> MainThreadID) then
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        fEventSubscribers.Execute(Self, Event, nil);
      end);
    end else
      fEventSubscribers.Execute(Self, Event, nil);
    if not fPermanentlyMeasureMode.Enabled then
      fMeasureQueue.Clear;
  end;
end;

procedure Tmbfm20301Module.OnFreqTimeOut(Sender : TObject);
begin
  FreqDone(C_ReadError);
  if fPermanentlyMeasureMode.Enabled then
  begin
    fMeasureQueue.Clear;
    fMeasureQueue.Add(fPermanentlyMeasureMode.Time);
    FreqStart(fPermanentlyMeasureMode.Time);
  end;
end;

function Tmbfm20301Module.GetChannelsCount: word;
begin
  Result := Word(fPositionList.Count);
end;  

function Tmbfm20301Module.FreqResult(AIndex: Word; const AResult: pFreqResult): Boolean;
begin
  AResult.SeriesCount := FMeasQueueState.TotalCount;
  AResult.SeriesSuccesCount := FMeasQueueState.SuccCont;
  AResult.TimeStamp := IncMinute(Now, -10);
  Result := AIndex < fPositionList.Count;
  if Result then
    AResult.Assign(TPosition(fPositionList[AIndex]).fResult);
end;

function Tmbfm20301Module.GetSeriesCount: Byte;
begin
  result := FMeasQueueState.TotalCount;
end;

function Tmbfm20301Module.GetSeriesSuccesCount: Byte;
begin
  result := FMeasQueueState.SuccCont;
end;

procedure Tmbfm20301Module.ClearBuffers;
var i : integer;
begin
  for I := 0 to fPositionList.Count - 1 do
    TPosition(fPositionList[i]).FreqClear;
end;  

function Tmbfm20301Module.StartOnceMeasure: Boolean;
begin
  fIniMeasParams.Read;
  Result := StartSeriesMeasure(fIniMeasParams.OnceTime, 1);
end;

function Tmbfm20301Module.StartSeriesMeasure: Boolean;
begin
  fIniMeasParams.Read;
  Result := StartSeriesMeasure(fIniMeasParams.MultiplicityTime, fIniMeasParams.Multiplicity);
end;

function Tmbfm20301Module.StartOnceMeasure(ATime{mSec*100}: Word): Boolean;
begin
  Result := StartSeriesMeasure(ATime, 1);
end;

function Tmbfm20301Module.StartPermanentlyMeasure: Boolean;
begin
  fIniMeasParams.Read;
  Result := StartPermanentlyMeasure(fIniMeasParams.OnceTime);
end;

procedure Tmbfm20301Module.FreezeModules;
var
vModules : IDeviceModules;
vController : IdmChallengeController;
vIdx : Word;
begin
  if not fFrezedProcessed then
  begin
    if Supports(owner, IDeviceModules, vModules) then
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

function Tmbfm20301Module.CalculatedTimeOut: Cardinal;
begin
  Result := fCalculatedTimeOut;
end;

procedure Tmbfm20301Module.ChallengeFreezed;
var
vIdx : Word;
begin
  vIdx := 0;
  while vIdx < fAutoFrezedModules.Count do
  begin
    IdmChallengeController(fAutoFrezedModules[vIdx]).CallChallenge;
    Inc(vIdx);
  end;
end;

procedure Tmbfm20301Module.UnFreezeModules;
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

procedure Tmbfm20301Module.StopPermanentlyMeasure;
begin
  fPermanentlyMeasureMode.Enabled := False;
end;

{TFreqPosition}


procedure TPosition.ApplyStaticFilter(AFilterValue : byte);
var i, j, vRejIdx : integer;
vRejCount : word;
vMean, vDelta : double;
begin
  if (AFilterValue = 0) or (fFreqList.Count < 1) then
    exit;
    
  vRejCount := (fFreqList.Count * AFilterValue) div 100;
  if (fFreqList.Count <= vRejCount) then
    exit;
  vRejIdx := 0;
  while vRejIdx < vRejCount do
  begin
    vMean := fFreqList.Sum / fFreqList.Count;
    i := 0;
    vDelta := 0;
    j := -1;
    while i < fFreqList.Count do
    begin
      if (abs(fFreqList[i] - vMean) > vDelta) then
      begin
        vDelta := abs(fFreqList[i] - vMean);
        j := i;
      end;
      inc(i);
    end;
    if (j <> -1) then
      fFreqList.Delete(j);
    inc(vRejIdx);
  end;
  fResult.Freq := MeanFreq;
end;


constructor TPosition.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fFreqList := TxFloatList.Create;
end;

destructor TPosition.Destroy;
begin
  fFreqList.Clear;
  FreeAndNil(fFreqList);
  inherited Destroy;
end;

procedure TPosition.SetFreq(const AValue: double; DoAdd : Boolean = true);
begin
  if DoAdd then
  begin
    fFreqList.Add(AValue);
    fResult.Freq := MeanFreq;
    fResult.DispersionHz := Vodopad.FloatList.StdDevFixed(fFreqList);
    fResult.DispersionPpm := Vodopad.FloatList.StdDevFixedPpm(fFreqList, fResult.Freq);
    if (fFreqList.Count >= 2) then
      fResult.TimeStamp := Now;      
  end else
  begin
    fResult.Freq := AValue;
    fResult.TimeStamp := Now;
  end;
end;

procedure TPosition.FreqClear;
begin
  fFreqList.Clear;
  fResult.Freq := 0;
  fResult.DispersionHz := 0;
  fResult.DispersionPpm := 0;
  fResult.TimeStamp := IncMinute(Now, -10);
end;

function TPosition.MeanFreq: double;
begin
  if (fFreqList.Count < 2) then
  begin
    if (fFreqList.Count > 0) then
      result := fFreqList[0]
    else
      result := fResult.Freq;
  end
  else
    result := fFreqList.Sum / fFreqList.Count;
end;  


{ TMeasQueue }

function TMeasQueue.Add(MeasTime: Word): integer;
var
vItem : pMeasQueueItem;
begin
  New(vItem);
  vItem.State := QiAdd;
  vItem.MeasTime := MeasTime;
  Result := inherited add(vItem);
end;

function TMeasQueue.First(State : TQIState; out Item: pMeasQueueItem): Boolean;
var
vIdx : Byte;
begin
  Result := false;
  vIdx := 0;
  while vIdx < Self.Count do
  begin
    Item := Self[vIdx];
    Result := (Item.State = State);
    if Result then
      break;
    Inc(vIdx);
  end;
end;

function TMeasQueue.Get(const Index: Integer): pMeasQueueItem;
begin
  result := pMeasQueueItem(inherited Get(Index));
end;

procedure TMeasQueue.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(Ptr);
  inherited Notify(Ptr, Action);
end;

function TMeasQueue.StateCount(State : TQIState): Byte;
var
vIdx : Byte;
begin
  Result := 0;
  vIdx := 0;
  while vIdx < Self.Count do
  begin
    if (Self[vIdx].State = State) then
      Inc(Result);
    Inc(vIdx);
  end;
end;

end.
